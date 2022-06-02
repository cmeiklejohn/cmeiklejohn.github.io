---
layout: post
title:  "Extending Fiibuster to Test Redis"
date:   2022-06-02 00:00:00 -0000
categories: filibuster
group: filibuster
---

TODO

# Summary

The automated resilience testing tool, Filibuster, lets developers test microservice applications for resilience against remote service unavailability. 
Filibuster achieves this by instrumenting remote procedure calls (RPC), commonly made using libraries like gRPC and Python’s requests, to perform a systematic exploration of all possible RPC faults for a given microservice application. 
However, the current version of Filibuster does not support fault injection when client libraries for databases are used to issue the remote procedure calls. 
In this article, I discuss work that I performed as part of a student research project in the Composable Systems Lab, part of the Institute for Software Research at CMU, to explore the viability of using Filibuster to test external services that use 3rd party client libraries such as Redis.

# Introduction

As microservice applications become increasingly commonplace, it is critical to test the behavior of these applications under _partial failure_: when one or more of the services that the application depends on fails. 
If the application lacks the necessary error handling, it may completely fail rather than gracefully degrade when one or more of its dependent services fails. 
To address this issue, Filibuster systematically enumerates the possible RPC failures that an application might observe and then subjects the application to these failures, allowing developers to ensure correct operation under these failure scenarios during testing and prior to deployment.

Since microservice architectures are often used for web application backends, it is critical that Filibuster not only test services that communicate with other services, but also test services that communicate with databases, as databases play an important role in data persistence in backend services. 
Expanding Filibuster to support this style of fault injection on databases, and in the specific case Redis, will allow us to create a prototype and inform future research and engineering efforts towards this goal.

We started this work with two interesting questions:

1. Could Filibuster be expanded to support database calls?
2. How would this new support for database calls change the way the Filibuster tool interacts with microservice applications?
 
# Example: Cinema Microservice Application

To explore what a design for database fault injection might look like, we decided to start by expanding one of the examples in the Filibuster application corpus with Redis, an in-memory data store often used as a cache. 
The Filibuster application corpus contains several example microservice applications, some of which are reproduced from industry to demonstrate different request patterns. 
Request patterns that are exemplified by the corpus range from retries on failure, to fallback requests, to using default responses on failure, as well as the different types of ways requests can be structured and nested.

The example we chose to expand was one of the Filibuster corpus’ cinema examples. 
It consists of four services: users, bookings, showtimes, and movies. 
These services allow the user to retrieve user information, book movies, retrieve showtime information, and perform other actions one might expect from a cinema site.

While this application exposes several different REST APIs, one important API allows users to retrieve their movie bookings. 
Starting with a request from the client to the users service, the application makes several GET requests to retrieve their bookings, as shown by the diagram below:

<img src="/img/eunice-cinema-example.png" width="800">

