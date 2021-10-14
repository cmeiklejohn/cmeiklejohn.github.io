---
layout: post
title:  "Dynamic Reduction: Optimizing Service-level Fault Injection Testing With Service Encapsulation"
date:   2021-10-14 00:00:00 -0000
categories: filibuster
group: filibuster
---

_This is the fourth post in a series of posts on our upcoming ACM SoCC '21 paper, 
"Service-level Fault Injection Testing" leading up to the open-source release of our prototype implementation, Filibuster.  In our [first post](http://christophermeiklejohn.com/filibuster/2021/10/02/filibuster-1.html), we talked about the challenges of doing resilience engineering research on microservice architectures due to the lack of an application corpus; in our [second post](http://christophermeiklejohn.com/filibuster/2021/10/06/filibuster-2.html), we detailed our approach called Service-level Fault Injection Testing, or SFIT._

In this post, we're going to look at how Filibuster can take advantage of certain properties of microservice architectures to reduce testing overhead: key to running resilience tests in development or CI before code ships to production.

We *highly recommend* you read our [second post](http://christophermeiklejohn.com/filibuster/2021/10/06/filibuster-2.html) that describes how precisely Service-level Fault Injection works: it's critical to understand the methodology before understanding how we apply optimizations for test case reduction.

## Example: Ride Sharing

Let's consider an example microservice application that might back a ride sharing service.

<img src="/img/filibuster-dr-1.png" width="600">

In this example, a few things are happening:

* First, the API gateway is contacted by the mobile app to display to a driver a page that estimates how much money they might make driving tonight.
* The API gateway contacts the *payments* service, which in turn pre-authorizes a payment using a 3rd party payment processor that has 99.9% uptime.
* After that, a *workload* service is contacted that returns the estimated workload, perhaps using a ML model.
* Finally, a *assets* service is contacted to retrieve required stylesheets and other image assets needed to display the page.

This application has some failure handling built in already:

1. If the *payments* service fails for any reason -- or if it's dependency is unavailable -- the error is ignored and payments will be authorized at a later date.  We *ignore* failures here and perhaps log some information for offline reconciliation.
2. If either the *workload* or *assets* service is unavailable, the request returns a 503 Service Unavailable to the mobile client, the mobile client handles this error and prompts the user to try again.

We start with a functional test that looks like the following:

```python
def test_functional_load_drivers_page():
    username = "cmeiklejohn”
    uri = "http://api-server/drivers/{}".format(username))
    result = requests.get(uri)
    assert result.status_code == 200
```

### Applying Filibuster

We test this application using Filibuster, which causes us to make the first modification when Service D fails from fault injection.

```python
def test_functional_load_drivers_page():
    username = "cmeiklejohn”
    uri = "http://api-server/drivers/{}".format(username))
    result = requests.get(uri)
    if filibuster.assertions.was_fault_injected():
        assert result.status_code == 503
    else:
        assert result.status_code == 200
```

Then, we make another modification when Filibuster executes the test where Service B fails.

```python
def test_functional_load_drivers_page():
    username = "cmeiklejohn”
    uri = "http://api-server/drivers/{}".format(username))
    result = requests.get(uri)
    if filibuster.assertions.was_fault_injected():
        if filibuster.assertions.was_fault_injected_on("payments"):
            assert result.status_code == 200
        else:
            assert result.status_code == 503
    else:
        assert result.status_code == 200
```

Great.

## Failure Permutations

Now, we're left asking *what tests does Filibuster actually need to run?*

Let's think through the problem.

### First Principle: Tolerance to Failure of Single Service

We know that we need to, at a minimum, test tolerance to a single failure of one of each service's dependencies.  Let's use the ride sharing example to understand what we mean.

First, we need to test how Service B reacts to it's only dependency, Service E failing.  We need to do this for *each of the ways* that Service E can fail.  For instance, any possible error responses that E might return to relay some sort of context-specific error (e.g., 404 Not Found) as well as any possible exceptions that Service B's RPC client might throw (e.g., Connection Error, Timeout.) . 

<img src="/img/filibuster-dr-2.png" width="600">

Similarly, we will also need to repeat this process for each of Service A's dependencies as well.  Starting with Service B, we repeat this process for Service B, C, and then D.

<img src="/img/filibuster-dr-3.png" width="600">

### Second Principle: Permutations of Service's Direct Dependencies

We also need to test the possible permutations of failures for each services' direct dependencies.  This is done
for a very specific reason: the service's implementation may contain a conditional based on *a certain combination of failures*.  

In this example, it is possible that Service A contains code that performs a very specific action when *Service B and Service D fail together for a particular exception type.* 

<img src="/img/filibuster-dr-4.png" width="600">

Therefore, we also iterate these combinations as well: assuming a single failure, this would result in the combinations (B, C), (B, D), (C, D), and (B, C, D).

But, we're left with the question: *do we need to test the combination where Service E and Service D fail together?*

<img src="/img/filibuster-dr-5.png" width="600">

The answer is: no.

## Dynamic Reduction

This is the basis of our algorithmic optimization for service-level fault injection testing (SFIT) that we call dynamic reduction.

Here's the intution behind dynamic reduction:

1. We have already tested the effect on Service A when Service D fails.
2. We have already tested the effect on Service B when Service E fails.
3. We have already tested all of the possible failures (and success) outcomes from Service B, as directly observable by Service A.
4. From these known outcomes, we *already know* what the outcome of this test will be from outcomes we have already observed.

*Now, it is important to note that we know this information based on the following assumptions we place on the functional test being executed: a.) service responses, as part of this single functional test execution, are fixed; b.) service outcomes are not dependendent on previous failures; and c.) the functional test is free from observable nondeterminism, either scheduling or data nondeterminism.  Therefore, dynamic reduction is not sound in general.*

<img src="/img/filibuster-dr-6.png" width="600">

The property that we exploit here, we refer to as __service encapsulation__: the idea that when Service E fails, it's failure is not directly observable by Service A, but rather only visible through Service B, by the response that Service B returns to Service A.  Therefore, if we can identify the possible responses that can be returned by Service B and test A's resilence to these failures, we do not need to redundantly test A's direct dependencies along with failures deeper in the call chain.

However, this optimization relies on the fact that service implementations return error codes that are conditional on the behavior of their dependencies and do not encode the failure into a response that is considered successful.  As an example, if Service B returned a successful response to Service A that contained a boolean indicating whether or not Service E responded to it's request successfully, then this optimization would not be sound (as stated above, it is *not* generally sound.)  We believe that this type of design where errors are encoded through error responses in failures is very important: *it enables compositional reasoning that can directly reduce testing overhead.*

<img src="/img/filibuster-dr-7.png" width="600">

You can see this benefit in the recreation of Audible's infrastructure for our corpus.  In this example, to generate all of the possible tests required for full coverage of two possible call site exceptions and a number of service-specific error codes that are returned, we had to generate *69* tests, but only needed to execute *31* of these tests: service encapsulation can be exploited as the Audible Download Service and Content Delivery Servies hide the failures of their components from the rest of the application.  

__Deeper, microservice graphs, enables compositional reasoning through service encapsulation.__

Graphs that grow wider, rather than deeper, do not benefit from these types of optimization.  Here's a recreation of the Netflix homepage loading process in our corpus.  Here, there's a large fanout from a single service where a number of fallbacks are specified.  As we demonstrated above, we need to test *all of the combinations to ensure that there doesn't exist application code conditional on some set of failures.*  Therefore, we are left with a combinatorial explosion: we have to generate 1,606 tests and can only remove 3 of those tests through dynamic reduction (resulting in 1,603 executed tests.)

If you're interested in learning more about dynamic reduction, you should check out our [ACM SoCC '21 presentation](https://www.youtube.com/watch?v=pyYh-vNspAI) and [preprint](http://christophermeiklejohn.com/publications/filibuster-socc-2021.pdf) where we talk in more detail about it.

## Conclusion

This was a short introduction to an optimization for service-level fault injection testing with Filibuster, called dynamic reduction, that reduces the overhead of resilience testing in the local development environment.  With our upcoming release of Filibuster, we will release full documentation on our tool, an example corpus and this tutorial.

Stay locked in by following us [@FilibusterFault](http://www.twitter.com/FilibusterFault) on Twitter to know when our next post will be available.