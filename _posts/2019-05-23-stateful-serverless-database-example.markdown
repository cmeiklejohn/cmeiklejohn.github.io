---
layout: post
title:  "Introduction to Stateful Serverless: key-value database with Azure Durable Entities"
date:   2019-05-23 00:00:00 -0000
categories: serverless
group: Serverless
---

## Introduction

_You can find the code for this example on [GitHub](https://github.com/cmeiklejohn/DurableFunctionDatabase)_

As part of my internship at Microsoft Research during the Summer of 2018, Sebastian Burckhardt and I did a lot of research investigating what types of applications were not possible, or extremely difficult to build with serverless infrastructures, how serverless could be extended to make use of state and how to achieve coordination when building upon serverless -- coordination being required to solve some types of problems.  We spent an entire summer building a prototype implementation of what types of applications could be built with these extensions called the [Reactive Machine](http://www.reactive-machine.org), with Sebastian building the front-end language and myself writing the backend runtime system.  After the summer came to a close, we open-sourced our implementation and wrote up some documentation on how it works.

At [Microsoft Build 2019](https://www.microsoft.com/en-us/build) this year, Durable Entities -- the product of serveral months of work with Sebastian embedded in the Azure Functions team, was announced.  Durable Entities, an extension that's in alpha release to the Durable Functions programming model, allows functions to retain state across function invocations where only a single instance is guaranteed to exist at a single point in time.  Now that I'm back at Microsoft for the summer, I've decided to write a few small applications using the framework to try it out.

To introduce you to durable entities, I'm going to walk through building a small key-value store that allows users to read and write objects over the web, implemented with durable functions and durable entities.

## Durable Entities

Durable Entities allow us to model stateful serverless functions.  Durable entities are represented using a unique identifier, called the entity id, persist their state automatically across function executions, ensure that only a single invocation can execute at a given time, and have execution that is indivisible.  If an entity, given it's entity identifier doesn't exist, it's created on demand.  If you are familiar with [Microsoft Orleans](https://dotnet.github.io/orleans/), durable entities look a lot like virtual actors.

To model our database, we will create a durable entity called ```Register```.  ```Register``` will support two operations: get, to return the current value of the register, and set to set the contents of the register.  With set, once we update the register, we will return the value back to the user.

```c#
[FunctionName("Register")]
public static void Register(
    [EntityTrigger] IDurableEntityContext ctx)
{
    string currentValue = ctx.GetState<string>();

    switch (ctx.OperationName)
    {
        case "set":
            string operand = ctx.GetInput<string>();
            currentValue = operand;
            ctx.SetState(currentValue);
            ctx.Return(currentValue);
            break;
        case "get":
            ctx.Return(currentValue);
            break;
    }
}
```

When an entity is invoked, it is supplied with a context object, ```DurableEntityContext``` that can be used to retrieve state, mutate state, and return a value to the caller of the function.  In our example, when the Register is invoked, we use the ```ctx.GetState<T>()``` call to return the current state for the entity.  If the operation is a get, we can immediately return this value to the user with ```ctx.Return()```.

With the set operation, we first use the ```ctx.GetInput<T>()``` function to retrieve the input to the ```Register``` entity -- the functions arguments.  Once we have this value, we use ```ctx.SetState()``` to set the current value and finally ```ctx.Return()``` to return the value to the caller.

## Handling the Request

We first define a function ```Database_HttpStart```, that accepts either GET or PUT requests at a URL ```Database/KEY```.  This function will respond to HTTP GET and PUT events to the URL.  GET requests are used to retrieve the current value for a key; PUT operations, with a given payload, will store a value for a key in the database.

```c#
[FunctionName("Database_HttpStart")]
public static async Task<HttpResponseMessage> HttpStart(
    [HttpTrigger(AuthorizationLevel.Anonymous, "get", "put", Route = "Database/{key}")] HttpRequestMessage req,
    string key,
    [OrchestrationClient] IDurableOrchestrationClient starter,
    ILogger log)
```

If the request is a GET, we start a new orchestration using the ```DurableOrchestrationClient``` that will run the ```Database_GET_Orchestrator``` function.  We supply the key for the register we want to update, taking the key from the URL.  Durable orchestrations are guaranteed to complete -- their execution will be durably logged and replayed until successful.  Under failure, re-execution will ensure that any intermediate results are persisted and used during the replay.

```c#
// GET request
if (req.Method == HttpMethod.Get)
{
    instanceId = await starter.StartNewAsync(nameof(Database_GET_Orchestrator), key);
    log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
    return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
}
```

If the request is a PUT, we start a new orchestration using the ```DurableOrchestrationClient``` that will run the ```Database_PUT_Orchestrator``` function.  We supply both the key for the register we want to update, taking the key from the URL, and also the value we want to write, taken from the PUT data.  

```c#
// PUT request
else if(req.Method == HttpMethod.Put)
{
    var content = req.Content;
    string value = content.ReadAsStringAsync().Result;
    instanceId = await starter.StartNewAsync(nameof("Database_PUT_Orchestrator"), new WriteOperation(key, value));
    log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
    return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
}
```

We wrap both the key and value into a ```WriteOperation``` object to pass them to the orchestration as input data -- only a single value may be supplied as input to the orchestration.  We include the implementation below.

```c#
public class WriteOperation
{
    public string Key { get; set; }

    public string Value { get; set; }

    public WriteOperation(string key, string value)
    {
        Key = key;
        Value = value;
    }
}
```

With each of these requests, the ```WaitForCompletionOrCreateCheckStatusResponseAsync``` call will wait the maximum amount of time for the request to finish, and if the request hasn't finished, will return a URL containing the location to poll waiting for completion.  If you prefer to poll, ```await starter.CreateCheckStatusResponseAsync``` will return a response to the user containing the poll URL.  Again, since durable functions are guaranteed to execute, clients can poll the URL continuously until the operation completes.

## Orchestrations

Now, we have to implement our two orchestrations: the GET orchestration, ```Database_GET_Orchestrator```, and the PUT orchestration, ```Database_PUT_Orchestrator```.  Implementation of these orchestrations is fairly straightforward.

The GET orchestrator simply retrieves the current value of the key.  To access the argument passed to the GET orchestration, we use the ```context.GetInput<T>()``` method on the ```DurableOrchestrationContext``` object passed into the orchestration.  Using this key, we can construct an identifier that references the unique entity for the key.  Finally, the orchestration will perform a call to the GET orchestration with the entity id and the operation to perform represented as text: ```get```.  

```CallEntityAsync<T>``` is a blocking call where the caller waits for a response from the invoked function.  To make a function invocation without waiting for the response, ```SignalEntityAsync``` can be used.

```c#
[FunctionName("Database_GET_Orchestrator")]
public static async Task<string> DatabaseGetOrchestratorAsync(
    [OrchestrationTrigger] IDurableOrchestrationContext context)
{
    var key = context.GetInput<string>();

    EntityId id = new EntityId(nameof(Register), key);

    return await context.CallEntityAsync<string>(id, "get");
}
```

Similarly, the PUT orchestration is also straightforward.  Using ```context.GetInput<T>```, we retrieve the ```WriteOperation``` object that was provided by the caller of the orchestration, construct the entity id, and perform the async call to the set operation -- here, we provide the value that should be stored by the entity.

```c#
[FunctionName("Database_PUT_Orchestrator")]
public static async Task<string> DatabasePutOrchestratorAsync(
    [OrchestrationTrigger] IDurableOrchestrationContext context)
{
    var operation = context.GetInput<WriteOperation>();

    EntityId id = new EntityId(nameof(Register), operation.Key);

    return await context.CallEntityAsync<string>(id, "set", operation.Value);
}
```

With that, our orchestration is complete and ready to be run.

We can build the application with ```dotnet build``` and then run the local functions evnrionment using ```func host start```.


```
The simulator kicks up and we're ready to go:

> Executing task: func host start <


                  %%%%%%
                 %%%%%%
            @   %%%%%%    @
          @@   %%%%%%      @@
       @@@    %%%%%%%%%%%    @@@
     @@      %%%%%%%%%%        @@
       @@         %%%%       @@
         @@      %%%       @@
           @@    %%      @@
                %%
                %

Azure Functions Core Tools (2.7.1158 Commit hash: f2d2a2816e038165826c7409c6d10c0527e8955b)
Function Runtime Version: 2.0.12438.0
SKipping 'FUNCTIONS_CORETOOLS_ENVIRONMENT' because value is null
[5/23/2019 10:30:21 PM] Starting Rpc Initialization Service.
[5/23/2019 10:30:21 PM] Initializing RpcServer
[5/23/2019 10:30:21 PM] Building host: startup suppressed:False, configuration suppressed: False
[5/23/2019 10:30:25 PM] Initializing extension with the following settings: Initializing extension with the following settings:
[5/23/2019 10:30:25 PM] HubName:SampleHubVS, StorageProvider: { AzureStorage: { ConnectionStringName: , PartitionCount: 4, ControlQueueBatchSize: 32, ControlQueueVisibilityTimeout: 00:05:00, WorkItemQueueVisibilityTimeout: 00:05:00, TrackingStoreConnectionStringName: , MaxQueuePollingInterval: 00:00:30,  },  }, MaxConcurrentActivityFunctions: 40, MaxConcurrentOrchestratorFunctions: 40, ExtendedSessionsEnabled: False, EventGridTopicEndpoint: , NotificationUrl: http://localhost:7071/runtime/webhooks/durabletask, LogReplayEvents: False. InstanceId: . Function: . HubName: SampleHubVS. AppName: . SlotName: . ExtensionVersion: 2.0.0. SequenceNumber: 0.
[5/23/2019 10:30:25 PM] Initializing Host.
[5/23/2019 10:30:25 PM] Host initialization: ConsecutiveErrors=0, StartupCount=1
[5/23/2019 10:30:25 PM] LoggerFilterOptions
[5/23/2019 10:30:25 PM] {
[5/23/2019 10:30:25 PM]   "MinLevel": "None",
[5/23/2019 10:30:25 PM]   "Rules": [
[5/23/2019 10:30:25 PM]     {
[5/23/2019 10:30:25 PM]       "ProviderName": null,
[5/23/2019 10:30:25 PM]       "CategoryName": null,
[5/23/2019 10:30:25 PM]       "LogLevel": null,
[5/23/2019 10:30:25 PM]       "Filter": "<AddFilter>b__0"
[5/23/2019 10:30:25 PM]     },
[5/23/2019 10:30:25 PM]     {
[5/23/2019 10:30:25 PM]       "ProviderName": "Microsoft.Azure.WebJobs.Script.WebHost.Diagnostics.SystemLoggerProvider",
[5/23/2019 10:30:25 PM]       "CategoryName": null,
[5/23/2019 10:30:25 PM]       "LogLevel": "None",
[5/23/2019 10:30:25 PM]       "Filter": null
[5/23/2019 10:30:25 PM]     },
[5/23/2019 10:30:25 PM]     {
[5/23/2019 10:30:25 PM]       "ProviderName": "Microsoft.Azure.WebJobs.Script.WebHost.Diagnostics.SystemLoggerProvider",
[5/23/2019 10:30:25 PM]       "CategoryName": null,
[5/23/2019 10:30:25 PM]       "LogLevel": null,
[5/23/2019 10:30:25 PM]       "Filter": "<AddFilter>b__0"
[5/23/2019 10:30:25 PM]     }
[5/23/2019 10:30:25 PM]   ]
[5/23/2019 10:30:25 PM] }
[5/23/2019 10:30:25 PM] FunctionResultAggregatorOptions
[5/23/2019 10:30:25 PM] {
[5/23/2019 10:30:25 PM]   "BatchSize": 1000,
[5/23/2019 10:30:25 PM]   "FlushTimeout": "00:00:30",
[5/23/2019 10:30:25 PM]   "IsEnabled": true
[5/23/2019 10:30:25 PM] }
[5/23/2019 10:30:25 PM] SingletonOptions
[5/23/2019 10:30:25 PM] {
[5/23/2019 10:30:25 PM]   "LockPeriod": "00:00:15",
[5/23/2019 10:30:25 PM]   "ListenerLockPeriod": "00:00:15",
[5/23/2019 10:30:25 PM]   "LockAcquisitionTimeout": "10675199.02:48:05.4775807",
[5/23/2019 10:30:25 PM]   "LockAcquisitionPollingInterval": "00:00:05",
[5/23/2019 10:30:25 PM]   "ListenerLockRecoveryPollingInterval": "00:01:00"
[5/23/2019 10:30:25 PM] }
[5/23/2019 10:30:25 PM] Starting JobHost
[5/23/2019 10:30:25 PM] Starting Host (HostId=minintovaolh3-255252354, InstanceId=6ac82daa-5bef-41ee-94ef-fcfd19e46539, Version=2.0.12438.0, ProcessId=17940, AppDomainId=1, InDebugMode=False, InDiagnosticMode=False, FunctionsExtensionVersion=)
[5/23/2019 10:30:25 PM] Loading functions metadata
[5/23/2019 10:30:26 PM] 4 functions loaded
[5/23/2019 10:30:26 PM] WorkerRuntime: dotnet. Will shutdown other standby channels
[5/23/2019 10:30:27 PM] Generating 4 job function(s)
[5/23/2019 10:30:27 PM] Found the following functions:
[5/23/2019 10:30:27 PM] DurableFunctionDatabase.Database.DatabaseGetOrchestratorAsync
[5/23/2019 10:30:27 PM] DurableFunctionDatabase.Database.HttpStart
[5/23/2019 10:30:27 PM] DurableFunctionDatabase.Database.DatabasePutOrchestratorAsync
[5/23/2019 10:30:27 PM] DurableFunctionDatabase.Database.Register
[5/23/2019 10:30:27 PM]
[5/23/2019 10:30:27 PM] Host initialized (1589ms)
[5/23/2019 10:30:27 PM] Starting task hub worker. InstanceId: . Function: . HubName: SampleHubVS. AppName: . SlotName: . ExtensionVersion: 2.0.0. SequenceNumber: 1.
[5/23/2019 10:30:29 PM] Host started (3850ms)
[5/23/2019 10:30:30 PM] Job host started
Hosting environment: Production
Content root path: C:\Users\t-chme\source\repos\DurableFunctionDatabase\DurableFunctionDatabase\bin\Debug\netcoreapp2.1
Now listening on: http://0.0.0.0:7071
Application started. Press Ctrl+C to shut down.

Http Functions:

        Database_HttpStart: [GET,PUT] http://localhost:7071/Database/{key}

[5/23/2019 10:30:35 PM] Host lock lease acquired by instance ID '000000000000000000000000F4A07733'.
```

Let's write some data!

```
$ curl -X PUT -d "my-value" http://localhost:7071/Database/1
"my-value"
$ curl -X GET http://localhost:7071/Database/1
"my-value"
```

Yeah!