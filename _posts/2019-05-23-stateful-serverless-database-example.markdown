---
layout: post
title:  "Building a key-value database with Azure Durable Entities"
date:   2019-05-23 00:00:00 -0000
categories: serverlesss
group: Serverless
---

## Introduction

...

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

## Durable Entities

...

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

...