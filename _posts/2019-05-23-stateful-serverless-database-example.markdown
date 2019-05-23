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

We first define a function ```Database_HttpStart```, that accepts either GET or PUT requests at a URL ```Database/KEY```.  This function will respond to HTTP GET and PUT events to the URL.

```c#
[FunctionName("Database_HttpStart")]
public static async Task<HttpResponseMessage> HttpStart(
    [HttpTrigger(AuthorizationLevel.Anonymous, "get", "put", Route = "Database/{key}")] HttpRequestMessage req,
    string key,
    [OrchestrationClient] IDurableOrchestrationClient starter,
    ILogger log)
```

If the request is a GET, we start a new orchestration using the ```DurableOrchestrationClient``` that will run the ```Database_GET_Orchestrator``` function.  We supply the key for the register we want to update, taking the key from the URL.

```c#
// GET request
if (req.Method == HttpMethod.Get)
{
    instanceId = await starter.StartNewAsync("Database_GET_Orchestrator", key);
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
    instanceId = await starter.StartNewAsync("Database_PUT_Orchestrator", new WriteOperation(key, value));
    log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
    return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
}
```

With each of these requests, the ```WaitForCompletionOrCreateCheckStatusResponseAsync``` call will wait the maximum amount of time for the request to finish, and if the request hasn't finished, will return a URL containing the location to poll waiting for completion.

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

We will also create a class ```WriteOperation``` to represent a write in the system and to encapsulate the value we want to write along with the key.  Here is the implementation of that class, which is straightforward, and used as part of the PUT operation.

## Orchestrations

...

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

...

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

...

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