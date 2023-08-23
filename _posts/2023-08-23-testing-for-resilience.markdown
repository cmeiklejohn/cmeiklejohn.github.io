---
layout: post
title:  "Testing Applications for Resilience Bugs vs Testing Applications for Resilience"
date:   2023-08-23 00:00:00 -0000
categories: filibuster
group: short
---

Testing microservice applications that are designed to be resilient, in order to find application bugs related to resilience, is difficult. This is different from testing microservice applications for resilience.

Consider a microservice application using a custom RPC client that contains circuit breakers, and retry logic, and other resilience measures. Now, think about how we might want to test our service to see what it does if one of the RPCs that the service issues fails.

Naively, I could inject a single fault for that RPC, perhaps in the network layer or maybe in the application layer. What will happen? Probably nothing. Why? Because the request is immediately retried.  OK, so I’m going to now inject a bunch of failures by taking the service it’s calling offline, OK, now I’ve triggered the circuit breaker and none of the RPCs are even being issued. Now, my application is probably broken.

This is what I think of as an unprincipled fault injection test because my test targeted different things, all at once, and I didn’t learn much from running the test. First, I tested my service’s RPC library’s retry mechanism. Then, I tested my circuit breakers opening. Then, I tested what my services does when the circuit breaker is open.

In developing Filibuster, I’ve been thinking hard about how to approach these different testing scenarios. We address this challenge in stages.

* First, we disable the resilience behavior in the RPC framework when testing, so we can identify application bugs in isolation without fighting automatic retries and risking that circuit breakers open during testing. This is key when running a lot of different failure cases, locally, in rapid succession (over 100+ failure cases a minute, depending on the service). Here, we synthetically inject the open circuit breaker and the timeout exception in a way that allows us to find bugs in the ways errors are handled at the service level, without relying on injecting enough faults to trigger timeouts or wait for timeouts to actually fire.  Then, we proceed to inducing timeouts via waiting, after we ensure the timeout exceptions are handled correctly.

* Second, we test the RPC framework independently to test it’s built in resilience mechanisms.

* Third, we test the application, using the RPC framework, to identify if the resilience mechanisms are properly implemented, integrated, and configured in the application — and, that the application does what it should when all of the retries fail, timeouts fire, etc. By the time you get here, compositionally, you should know precisely what is going to happen and you’re looking to reject the hypothesis.

Lots of times we talk about resilience in isolation — staying online when faults happen. 

What we often do not talk about is the effects of RPCs failing occasionally and the impact they have on our application(s).