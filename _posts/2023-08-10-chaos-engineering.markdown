---
layout: post
title:  "Chaos Engineering or Software Testing?"
date:   2023-08-10 00:00:00 -0000
categories: filibuster
group: short
---

Recently, I've seen a lot of posts under the #chaosengineering hash tag on LinkedIn --- Netflix style --- and as someone who has been researching resilience in software for 5 years as part of my Ph.D., I thought I would provide my thoughts.

First, we need to keep things in context. When Netflix was first moving to the cloud and "inventing" chaos engineering, EC2 was unreliable and instances would be reclaimed, fail, or otherwise disappear. Therefore, they wanted to be able to tolerate this. This is key behind their "inject realistic faults" philosophy.

But, that was 2007 and this is 2023. Then, Kubernetes didn't exist. Heck, Mesos didn't even exist. Now, we live in a world where we have more reliable cloud computing services, cluster orchestrators, advanced RPC clients, and all sort of infrastructure innovations. Randomly crashing nodes now shows us nothing, outside of ensuring that we have a reasonable crash/restart policies, liveness/readiness probes, and more than one node.

When it comes to building resilience in our applications, we need to first think about what we want to test, in relation to our software and supporting infrastructure.

For example, we might want to test if devs respond correctly to a failure. We also might want to test parts of our infrastructure. We also might want to test our application. A holistic approach addresses all of these, but as good software engineers know, strong testing is compositional, where we build guarantees over time through the testing of individual components and repeat those tests regularly (think: regression tests.)

For example, if I want to know if my auto-scaling policies work, I target a specific test for this. I don't run a test that may test my entire organization's response to a failure, before knowing anything about how the system will handle the failure.

If I'm looking for application bugs that happen when services are unavailable in a microservice architecture, I don't randomly crash a server and see what happens. Why? First, it's very hard to reproduce because code is complex, concurrent, and most importantly nondeterministic. In short, no way to easily attach a debugger, reproduce, and solve the problem. Second, it's just testing too much! Test the service to failures of its dependencies by realistically simulating the failures and fixing them.

Then, and only then, crash the service in a subsequent test to invalidate the hypothesis that your service is can handle any failure. Then, expand out further.

Compositional reasoning: it's how we reason about programs that we write.

We test something to determine its behavior, and then we test components that use those components under the behavior we identified.

Only when we know how everything should work do we run the end to end test of the system where we test all of the components working together. We have an assumption about what will happen: our test is used to possibly invalidate that assumption.