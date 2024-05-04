---
layout: post
title:  "Filibuster 2.0: Redis Fault Injection"
date:   2023-09-01 10:00:00 -0000
categories: filibuster
group: filibuster
---

Ever wanted to test your microservice application against Redis failures? Filibuster 2.0 supports injecting faults against Redis, PostgreSQL, CockroachDB, and DynamoDB.

Here, using our IntelliJ visualizer for tests, we see that in this test we injected a failure on a synchronous GET command to Redis and the test still passed. That's fault tolerant code!

Not using synchronous operations? No problem! Filibuster will inject execeptions for each asynchronous GET or SET operation and defer the fault injection until someone calls get or set on the future. 

Wanna find out if someone is using thenAccept but forgetting to catch the exception? Filibuster can fail the test if the developer never gets the value of the returned future too!!

<img src="/img/1691309222915.jpeg" style="width: 100%">