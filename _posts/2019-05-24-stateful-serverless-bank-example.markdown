---
layout: post
title:  "Stateful Serverless: Critical Sections with Azure Durable Functions"
date:   2019-05-25 00:00:00 -0000
categories: serverless
group: Serverless
---

_This is part two of a series of blog posts on building stateful serverless applications.  If you haven't read the first part, you should go [read it first](http://christophermeiklejohn.com/serverless/2019/05/23/stateful-serverless-database-example.html).  The example used in this post is available on [GitHub](https://github.com/cmeiklejohn/DurableFunctionBank)._

## Introduction

In our previous post, we talked about how Durable Entities allow users to build applications using functions whose state is persisted across multiple function invocations.  This is an extremely powerful primitive, because it simplifies the development of stateful applications while avoiding the complexities of reasoning about concurrent invocations of the same function operating with cloud storage. 

In this post, we are going to look at how we can coordinate between multiple function invocations.  To do that, we're going to use a motivating example: the ever unrealstic, but perfect example for explaining invariants preservation under concurrency, the atomic bank transer.

Let's start by building a small little bank application with Durable Functions.

## Modeling an account

We use a durable entity to model each account called ```Account```.  Accounts will, when initialized on demand, set their balance at zero, and allow callers to a.) check the balance, b.) perform deposits, and c.) perform withdrawals.  The implementation of this is fairly straightforward, and regardless of the type of operation invoked, we will always return the current balance of the account to the user.

```c#
[FunctionName("Account")]
public static void Account(
    [EntityTrigger] IDurableEntityContext ctx)
{
    int currentValue = ctx.GetState<int>();
    int operand;

    if (ctx.IsNewlyConstructed)
    {
        currentValue = 0;
        ctx.SetState(currentValue);
    }

    switch (ctx.OperationName)
    {
        case "balance":              
            break;
        case "deposit":
            operand = ctx.GetInput<int>();
            currentValue += operand;
            ctx.SetState(currentValue);
            break;
        case "withdraw":
            operand = ctx.GetInput<int>();
            currentValue -= operand;
            ctx.SetState(currentValue);
            break;
    }

    ctx.Return(currentValue);
}
```

Second, we'll create a class that reepresents an operation on an account, called ```AccountOperation```.  Each account operation will contain an account id, for the account being operated on, a string representing the type information that will be used to match against operations inside of the ```Account``` entity, and an integer representing the account balance.

```c#
public class AccountOperation
{
    public string AccountId { get; set; }

    public string Type { get; set; }

    public int Amount { get; set; }

    public AccountOperation(string accountId, string type, int amount)
    {
        AccountId = accountId;
        Type = type;
        Amount = amount;
    }
}
```

Now, let's start implementing some functions over accounts.

## Account functions

All of our account functions are going to share the same structure -- they wil be invoked through a REST endpoint and call a durable task orchestration to perform the actual work of the function.  An orchestration is used because it's guaranteed to execute to completion as it checkpoints it state before, during, and after execution.  If the call doesn't return in a particular amount of time, the caller will be returned a URL that can be used to check on the response using polling, as the orchestration is guaranteed to complete in the future.

Our endpoint for the balance call is straightforward -- it parses the URL and invokes an orchestration to get the current account balance.

```c#
[FunctionName("BalanceInquiry")]
public static async Task<HttpResponseMessage> BalanceInquiry(
    [HttpTrigger(AuthorizationLevel.Anonymous, "get", Route = "Account/{accountId}")] HttpRequestMessage req,
    string accountId,
    [OrchestrationClient] IDurableOrchestrationClient starter,
    ILogger log)
{
    string instanceId; 

    // GET request
    if (req.Method == HttpMethod.Get)
    {
        instanceId = await starter.StartNewAsync("GetBalance", accountId);
        log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
        return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
    }
    else
    {
        return req.CreateResponse(System.Net.HttpStatusCode.BadRequest);
    }
}
```

The orchestration is straightforward as well -- we take the input to the call using ```context.GetInput<T>```, create a reference to the unique durable ```Entity``` using the account identifier, and invoke the balance call.

```c#
[FunctionName("GetBalance")]
public static async Task<int> GetBalance(
    [OrchestrationTrigger] IDurableOrchestrationContext context)
{
    var accountId = context.GetInput<string>();

    EntityId id = new EntityId(nameof(Account), accountId);

    return await context.CallEntityAsync<int>(id, "balance");
}
```

We can define similar functions and endpoints for performing both deposits and withdrawls.  Here's what a deposit would look like:

```c#
[FunctionName("Deposit")]
public static async Task<string> Deposit(
    [OrchestrationTrigger] IDurableOrchestrationContext context)
{
    var accountOperation = context.GetInput<AccountOperation>();

    EntityId id = new EntityId(nameof(Account), accountOperation.AccountId);

    return await context.CallEntityAsync<string>(id, "deposit", accountOperation.Amount);
}

[FunctionName("PerformDeposit")]
public static async Task<HttpResponseMessage> PerformDeposit(
    [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = "Account/{accountId}/Deposit/{amount}")] HttpRequestMessage req,
    string accountId,
    string amount,
    [OrchestrationClient] IDurableOrchestrationClient starter,
    ILogger log)
{
    string instanceId;

    // POST request
    if (req.Method == HttpMethod.Post)
    {
        instanceId = await starter.StartNewAsync("Deposit", new AccountOperation(accountId, "deposit", Int32.Parse(amount)));
        log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
        return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
    }
    else
    {
        return req.CreateResponse(System.Net.HttpStatusCode.BadRequest);
    }
}
```

Here's what the withdraw looks like:

```c#
[FunctionName("Withdraw")]
public static async Task<string> Withdraw(
    [OrchestrationTrigger] IDurableOrchestrationContext context)
{
    var accountOperation = context.GetInput<AccountOperation>();

    EntityId id = new EntityId(nameof(Account), accountOperation.AccountId);

    return await context.CallEntityAsync<string>(id, "withdraw", accountOperation.Amount);
}

[FunctionName("PerformWithdraw")]
public static async Task<HttpResponseMessage> PerformWithdraw(
    [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = "Account/{accountId}/Withdraw/{amount}")] HttpRequestMessage req,
    string accountId,
    string amount,
    [OrchestrationClient] IDurableOrchestrationClient starter,
    ILogger log)
{
    string instanceId;

    // POST request
    if (req.Method == HttpMethod.Post)
    {
        instanceId = await starter.StartNewAsync("Withdraw", new AccountOperation(accountId, "withdraw", Int32.Parse(amount)));
        log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
        return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
    }
    else
    {
        return req.CreateResponse(System.Net.HttpStatusCode.BadRequest);
    }
}
```

In each case, we create an ```AccountOperation``` object that represents the operation we want to perform -- either a withdraw or a deposit on an account -- and then invoke the associated orchestration which performs the operation on the entity.  Entities execute in serial, guarantee durability and operations are guaranteed to complete.

## Transfers

Let's get to the more interesting part of the applicaation: bank transfers.  Bank transfers are interesting for the invariants they propose, even though they do not really reflect the way these transfers actually occur in real banks.  However, we use bank transfers as a motivating example to demonstrate how it is quite easy to violate these invariants under typical distributed systems issues around partial failure.

Transfers are interesting because they bring up two challenging requirements: a.) in order to not lose money, we need to make sure that the system finishes the transfer atomically: we withdraw the money from one account and then subsequently deposit the money into another account; and  b.) in order to make sure we maintain a non-negative balance in our accounts, we must ensure that prior to withdrawing the money from one account to deposit into the other, that we check the balance: furthermore, this balance cannot change during execution of the transfer, because it may lead to two withdraw operations occurring in parallel.

Entities already help us with problem of ensuring that no other execution can interfere when performing the individual withdraw and deposit operations on each account.  Orchestrations help us with the problem of ensuring atomicity by guaranteeing that if the deposit and withdraw are composed together as an orchestration that both will be guaranteed to complete.  __But, what about the issue of ensuring that the balance doesn't change between when we check the balance and perform the withdraw?__

Let's first get some boilerplate out of the way.  

First, we create a class to represent transfer operations.

```c#
public class TransferOperation
{
    public string FromAccountId { get; set; }

    public string ToAccountId { get; set; }

    public int Amount { get; set; }

    public TransferOperation(string fromAccountId, string toAccountId, int amount)
    {
        FromAccountId = fromAccountId;
        ToAccountId = toAccountId;
        Amount = amount;
    }
}
```

Then, we build the REST endpoint for transfer operation.

```c#
[FunctionName("PerformTransfer")]
public static async Task<HttpResponseMessage> PerformTransfer(
    [HttpTrigger(AuthorizationLevel.Anonymous, "post", Route = "Transfer/{amount}/From/{fromAccountId}/To/{toAccountId}")] HttpRequestMessage req,
    string amount,
    string fromAccountId,
    string toAccountId,
    [OrchestrationClient] IDurableOrchestrationClient starter,
    ILogger log)
{
    string instanceId;

    // POST request
    if (req.Method == HttpMethod.Post)
    {
        instanceId = await starter.StartNewAsync("Transfer", new TransferOperation(fromAccountId, toAccountId, Int32.Parse(amount)));
        log.LogInformation($"Started orchestration with ID = '{instanceId}'.");
        return await starter.WaitForCompletionOrCreateCheckStatusResponseAsync(req, instanceId, System.TimeSpan.MaxValue);
    }
    else
    {
        return req.CreateResponse(System.Net.HttpStatusCode.BadRequest);
    }
}
```

## Critical Sections

Now, let's build the ```Transfer``` orchestration.  In our orchestration, we use the ```context.LockAsync()``` operation within a ```using``` block to take out locks on the objects we are about to perform the operations on.  We start by waiting for the lock to be acquired, checking the balance from the account we wish to transfer the amount from making sure we will not violate the non-negative balance invariant, and then if there are available funds in the correct amount, proceed with performing the transfer.

The transfer itself can take advantage of task parallelism and perform both the deposit and withdrawl at the same time.  Using the ```Task.WhenAll``` operation, we can block execution until both operations complete and we exit the critical section.  Using locks on each of the entities, the ```using``` block desginates a critical section where we are guaranteed that no other operations will affect all of the locked entities.

```c#
[FunctionName("Transfer")]
public static async Task<bool> Transfer(
    [OrchestrationTrigger] IDurableOrchestrationContext context)
{
    var transferOperation = context.GetInput<TransferOperation>();

    bool transferSuccessful = false;

    EntityId fromAccountId = new EntityId(nameof(Account), transferOperation.FromAccountId);
    EntityId toAccountId = new EntityId(nameof(Account), transferOperation.ToAccountId);

    using (await context.LockAsync(fromAccountId, toAccountId))
    {
        var fromAccountBalance = await context.CallEntityAsync<int>(fromAccountId, "balance");

        if(fromAccountBalance >= transferOperation.Amount)
        {
            var taskList = new List<Task>();
            taskList.Add(context.CallEntityAsync<int>(fromAccountId, "withdraw", transferOperation.Amount));
            taskList.Add(context.CallEntityAsync<int>(toAccountId, "deposit", transferOperation.Amount));
            await Task.WhenAll(taskList.ToArray());
            transferSuccessful = true;
        }
    }

    return transferSuccessful;
}
```

The locks and critical section allow us to coordinate between multiple invocations of functions: something, that is not possible in existing functions frameworks without using storage explicitly in your function code as a coordination mechanism.  

Critical sections are possible because orchestrations are guaranteed to execute to completion and because Entities act in serial.  To ensure global progress, critical sections impose the following restrictions:

* No nesting of critical sections to prevent deadlocks arising from lock acquisition order.
* Critical sections cannot invoke orchestrations, only operations on Durable Entities.
* Critical sections can only invoke operations on locked entities -- calls to unlocked entities will throw.
* Parallel calls can only be invoked on different entities -- calls to the same entity in parallel are prevented to ensure a sequential order of call and response.
* One way messages, signals, can only be made to unlocked entities.

## Conclusion

All in all, critical sections bring incredible power to serverless applications that enable building rich, stateful applications not previous possible.  Check out the 2.0.0-alpha release of Durable Functions and feel free to reach out to me if you have any questions!