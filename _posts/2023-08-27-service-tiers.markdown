---
layout: post
title:  "Testing Applications Resilience in the Presence of Service Tiers"
date:   2023-08-27 00:00:00 -0000
categories: filibuster
group: short
---

Many microservice applications classify their dependencies into different [Service Tiers](https://thenewstack.io/how-service-tiers-can-help-to-avoid-microservices-disasters/).  Perhaps your developers are working on a Tier 1 service and want to make sure that they are tolerant to any failure of Tier 2+ failures.  How can they do that?

Well, if they've written a functional test that can be run with Filibuster, you can use a new feature of Filibuster called [_fault injection filters_](https://github.com/filibuster-testing/filibuster-java-instrumentation/blob/main/src/main/java/cloud/filibuster/junit/filters/FilibusterFaultInjectionFilter.java).  Fault injection filters allow you to arbitrarily implement filter functions that are provided with the invoked RPC service, method, and arguments to conditionally prevent fault injection.  For example, by implementing a filter that allows faults for any downstream services with a tier greater than your service.

Here, we have a simple purchase method executed by an Order Service.  It issues 7 downstream RPCs: 

* one to look up the user, which when failed, causes the service to return error.
* one to validate the user session, which when failed, causes the service to return error.
* one to look up the user's cart, which when failed, causes the service to return error.
* three to look up the possible discounts for the user's cart.
* one to send an email to the user using a Tier 2 service.

Now, if I use Filibuster normally, I'll execute this test over and over and fail every RPC and every combination of RPCs.  Any test that asserts the purchase is successful will fail: for example, by failing the downstream RPC to look up the user, the test will fail because I'll return error.  In this case, I'm supposed to return error, and Filibuster provides an API that allows you to say this precisely: when that downstream RPC fails, I won't be able to complete the purchase and will get this error.  

But, perhaps I want to verify *if all of my Tier 2 dependencies fail*, the system will always return success.  

Since sending the user an email is done using a Tier 2 service, I can easily implement a filter that states that I do not want to inject faults on any Tier 2 services, and that the email service is a Tier 2 service.  Then, when I run Filibuster, I will only inject faults on Tier 2 services and any failure of my test indicates that it is not tolerant to a Tier 2 service failure: it returned failure, when the system should have been tolerant to that failure.

<img src="/img/filibuster-service-tiers.png" style="width: 100%">
