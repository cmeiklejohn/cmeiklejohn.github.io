---
layout: post
title:  "Extending Filibuster to Test Redis"
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

Let's work with an example.
 
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

## Retrieve Bookings

First, the users service makes a call to Redis to check if the username exists in the database. 
If the user does not exist, then the users service will return a 404 Not Found. 
If the Redis call returns a successful response, then the users service will contact the bookings service and request the data associated with the given username.

Once the bookings service receives this GET request, it takes the following steps:

1. Check that the username exists in the bookings database via Redis call.  If the username doesn’t exist, then the bookings service will return a 404.  
   1. When the users service receives this response, it returns a 404 back to the user.
2. If this call is successful, retrieve the dates on which the user has booked a movie from Redis. 
3. For each booking date for that particular user, retrieve the movie identifiers. The movie identifiers are unique to each movie.
4. Return a JSON object containing the movie identifiers associated with each booking date for the user.

After the users service has received a response from the bookings service, it contacts the movies service and retrieves the movie data associated with each movie identifier. Then, the movies service retrieves the movie data from Redis and returns the data to the users service, which then returns the data to the user.

## Testing The Bookings API

To test this endpoint, we make a request to the users service that will retrieve a particular user's bookings, which corresponds to the endpoint `users/{username}/bookings`. 
In our test, we look for the bookings associated with `chris_rivers`, so we make a call to the endpoint `users/chris_rivers/bookings`.

We start with the basic functional test behavior that would exist prior to our adaptation for HTTP RPC fault injection.  
Our test asserts that if the response code from this call is 200 OK, which indicates success, the data returned matches the data associated with the `chris_rivers` user. 

To handle the cases where Filibuster has injected faults for the HTTP RPCs, we add conditional code to account for behavior under failure.  
In the case where an RPC fails, our test asks Filibuster to verify whether it injected a fault, _i.e.,_ that the fault was intentional — this is done using the `was_fault_injected()` conditional. Our test asserts that if the RPC failed, Filibuster did actually inject a fault and the HTTP status matches the expected status when the call fails. 
In this case, since the only errors we expect are a 404 Not Found or a 503 Service Unavailable, we assert that the status code must be 404 or 503.  

We excerpt the test below:

```python
bookings_endpoint = "{}/users/{}/bookings".format(helper.get_service_url('users'), 'chris_rivers')
users_bookings = requests.get(bookings_endpoint, timeout=helper.get_timeout('bookings'))
if users_bookings.status_code == 200:
    assert users_bookings.json() == {'20151201': [{'rating': '8.8','title': 'Creed','uri': '/movies/267eedb8-0f5d-42d5-8f43-72426b9fb3e6'}]}
else:
    assert was_fault_injected() and users_bookings.status_code in [404, 503]
```



TODO