---
layout: post
title:  ""
date:   2019-05-23 00:00:00 -0000
categories: serverlesss
group: Serverless
---

```c#
[FunctionName("Database_HttpStart")]
public static async Task<HttpResponseMessage> HttpStart(
    [HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "Database/{key}")] HttpRequestMessage req,
    string key,
    [OrchestrationClient] IDurableOrchestrationClient starter,
    ILogger log)
```

```c#
{
    string instanceId;

    // GET request
    if (req.Method == HttpMethod.Get)
    {
        instanceId = await starter.StartNewAsync("Database_GET_Orchestrator", key);
        log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
        return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
    }

    // POST request
    else if(req.Method == HttpMethod.Post)
    {
        var content = req.Content;
        string value = content.ReadAsStringAsync().Result;
        instanceId = await starter.StartNewAsync("Database_POST_Orchestrator", new WriteOperation(key, value));
        log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
        return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
    }

    // Otherwise.
    else
    {
        return req.CreateResponse(System.Net.HttpStatusCode.BadRequest);
    }
}
```