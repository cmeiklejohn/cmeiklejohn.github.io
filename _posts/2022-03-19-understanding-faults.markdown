---
layout: post
title:  "Understanding why Resilience Faults Occur"
date:   2022-03-19 00:00:00 -0000
categories: filibuster
group: filibuster
---

# Fault Taxonomy

Before my automated resilience testing tool, **[Filibuster](http://filibuster.cloud)**, was targeted at the resilience testing of microservice applications, it was targeted at the testing of distributed protocols, while in the development phase.

The first version of Filibuster grew out of my previous Ph.D. work, Partisan.  Partisan was a high-performance distributed runtime that was designed for actor-languages. (e.g., Erlang)  Based on  the observation that once you controlled the distributed runtime you could inject faults before, during, and after the processing of messages — as well as control the order of message transmission to determinize one aspect of the distributed execution — well, fault injection to detect problems follows naturally.  At the time, this seemed a novel observation; it wasn’t, in fact the authors of ORCHESTRA made a similar observation about distributed applications in [1996](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.6485&rep=rep1&type=pdf).  *(The embarrassing fact regarding this is that the second author on this paper is the current present of Carnegie Mellon University, where I am a current Ph.D. student.).* 

While our technique identified known faults in 2PC, CTP, and 3PC faster than the state-of-the-art, it required cumbersome annotations on message handlers: while these annotations could be automatically generated using static analysis with academic actor languages due to the high-level message handlers they use; these annotations had to be manually written commonplace actor languages, like Akka and Erlang.  Since Partisan was implemented in Erlang, this posed some problems.  It made it almost impossible to compete with existing fault injection approaches taken by distributed model checkers (e.g., CMC, SAMC, FlyMC) that relied on more general space-reduction techniques. (e.g., DPOR, symmetry reduction.).  Furthermore, the lack of an actual application corpus due to the limited use of Erlang posed real problems when performing an academic evaluation.

_We decided to take a step back: we’d focus on microservice applications instead._

We decided to refocus Filibuster on microservice applications with a brand new implementation, implemented in Python based on the observation that a potential Pittsburgh-area company used Python, with hopes of evaluation on their application.  That didn’t pan out as we expected, so we resorted to an evaluation on a different corpus of student applications, which ultimately posed a number of different problems: they didn’t represent the types of industrial microservice applications that are really being built, of which our tool was designed for.

_We decided to take a further, larger step back._

Our first approach was to identify existing open-source microservice applications, use their source code revision system and issue tracker to identify any previous resolved resilience bugs, reintroduce them, and then try to automatically find them using the new version of Filibuster.  However, these applications simply do not exist in the open-source ecosystem.  All that do exist, are tutorial applications showing how to write microservice applications.  These applications do not contain bugs.  

Next, we decided to take the tutorial applications and introduce resilience bugs: this approach has been taken in several papers, [as recent as SoCC ‘21](https://dl.acm.org/doi/abs/10.1145/3472883.3486986), where our first paper on Filibuster appeared.  However, most postmortems that discuss resilience issues that resulted in real outages do not contain information about the root cause.   This made reproduction of realistic bugs difficult.

We also tried to use academic bug corpora.  Bug corpora used for automated program repair, harvested from GitHub, do not contain microservice applications.  On the other hand, a recent academic paper who’s primary contribution is a corpora of microservice applications containing bugs contains *no bugs related to the microservice architecture itself* but only bugs that would exist in any monolithic web application.

If you’re interested, I’ve written at length about how it is difficult to do research in this area in a [previous blog post](https://christophermeiklejohn.com/filibuster/2021/10/02/filibuster-1.html), a topic I believe is a ripe research area but limited by access to real applications.

## Methods

If you have read my [previous blog post](https://christophermeiklejohn.com/filibuster/2021/10/02/filibuster-1.html), you will know that in order to solve this problem I watched 77 presentations from industrial conferences and blog posts on the use of chaos engineering to identify *the types of resilience issues that companies experience and how they go about identifying them.*  I performed this research from August 2020 to December of 2020, the Fall 2020 semester at Carnegie Mellon University.

For this research, I aggregated all of these results and constructed a taxonomy of what I found.  This was performed using what could be seen as a combination of the techniques from multiple case study analysis and grounded theory: note taking was performed during observation of the videos, open coding performed using these notes, and axial coding performed to aggregate these codes into categories using constant comparison.  From these categories, we then identified the core categories: the main causes of resilience issues and the main techniques used to identify them.  From there, we identified a theory about the existing testing techniques that could be used in order to address each category.   

This process was rather ad-hoc: I had not taken a qualitative nor quantitative methods course at the time I performed this research, so there are many places where we veered from these approaches based on what “felt” right.  We do not intend to publish this research for this very reason; we also do not plan to publish due to the limited number of observations we were able to make.

*(Note to future Ph.D. students: take a research methods course earlier, it will help you later on!  If you are at Carnegie Mellon University, I highly recommend Laura Dabbish’s class on Advanced Research Methods in HCII.)*

For the following discussions, we assume that readers are familiar with [chaos engineering:](https://christophermeiklejohn.com/filibuster/2022/03/17/what-is-chaos-engineering.html)

### Coding

For this research, I watched all 77 presentations on my sofa using Chromecast.  For each presentation, I took notes related to application structure, testing procedures, resilience bugs, the processes used to identify resilience bugs, discussion of bug resolution techniques, and future directions.  I also noted several quotes during this observation, if I felt they were related to resilience.  

From here, we performed an *extremely ad-hoc version of axial coding*, a technique that I did not know existed when we performed this analysis.  It took the following form.  First, I disregarded any discussions of organizational structure, method, processes, or certain technologies that were not related to resilience.  

Based on my review of notes through an ad-hoc derivation of the constant comparative method, I identified that the following categories as the *core categories*, and present them here as questions used when analyzing my notes: 

1. **Experiments.**
Did the presenter talk about an actual chaos experiment they ran to identify a bug?
2. **Resilience Bugs.**
Did the presenter talk about a resilience bug they observed or discovered (not bugs related to normal logic errors that could occur in a non-distributed application)?
3. **Resilience Patterns.**
Did the presenter talk about a pattern (*e.g.,* circuit breaker, etc.) that they used to improve resilience of their application?
4. **Application Structure.**
Did the presenter talk about an architectural pattern in a microservice application? 

From here, I further refined sub-category of *resilience bugs* using the constant comparative method:

1. Bug in application code.
2. Bug in cloud service misconfiguration.
3. Bug could have been identified using a mock, if written.
4. Bug occurred in infrastructure, but was triggered by bug in the application.

From here, I created another sub-category of resilience bugs using the constant comparative method, keeping in mind that individual items can belong to multiple categories:

1. Bugs that could have been identified using traditional testing techniques involving mocks.
2. Bugs that could *not* have been identified using traditional testing techniques involving mocks.

Then, I created a sub-category of experiments using the constant comparative method, keeping in mind that individual items can belong to multiple categories:

1. Experiments that revealed bugs that could have been identified using traditional techniques involving mocks.
2. Experiments that revealed bugs that could *not* have been identified using traditional techniques involving mocks.

From here, selective coding was used to understand the relationship between, and develop a theory of, the classes of resilience bugs that companies experience and the techniques that are required to identify them.

## Theory

In general, many bugs that are identified using chaos engineering — a technique that was popularized by Netflix to test their service where faults are introduced in the live, production environment on real customers and the application’s behavior under fault is observed — could be identified earlier using more traditional unit, integration, and functional tests in a local, development environment.  

As a specific example, Expedia tested a simple fallback pattern where, when one dependent service is unavailable and returns an error, another service is contacted instead afterwards.  There is no need to run this experiment in production by terminating servers in production: a simple test that mocks the response of the dependent service and returns a failure is sufficient.  However, more complicated, and more interesting, examples exist.

### Audible

One example that was particularly interesting was Audible.  The Audible example is quite complicated and involves a number of services in delivering an audiobook to an end user.

<img src="/img/audible.png" width="800">

In this example, when a user requests an audiobook using the Audible app, it first issues a request to the Content Delivery Engine to find the URL of the Content Delivery Service that contains the audiobook assets; we can think of this as a primitive sharding layer that is used to support scalability.  Once the Audible app retrieves this URL, it issues a request to the specified Content Delivery Service.

The Content Delivery Service first makes a request to the Audible Download Service.  The Audible Download Service is responsible for first verifying that the user owns the audiobook.  Next, the Audible Download Service verifies that a DRM license can be acquired for that audiobook.  Finally, it updates statistics at the Stats service before returning a response to the Content Delivery Service.  If either ownership of the book cannot be verified or a DRM license cannot be activated, an error response is returned to the Audible Download Service, which propagates back to the user through an error in the client.

Once the Audible Download Service completes it work, the Content Delivery Service contacts the Audio Assets service to retrieve the audiobook assets.  Then, the Content Delivery Service contacts the Asset Metadata Service to retrieve the chapter descriptions.  In this design, there is an implicit assumption that if an audiobook exists in the system and the assets are available, the metadata containing the chapter descriptions will also be available.

In this example — a real outage reported by Audible, the asset metadata is unavailable either because of developer error or a race condition.  As a result of this, a latent bug in the Content Delivery Service that doesn’t expect the content to be unavailable if the assets are available, causes an error to be propagated back to the end user.  The mobile application, not expecting this error code to ever occurs then presents a generic error to the user after retrying the request a number of times, and, after presenting a generic error to the user, causes them to hit the retry button.  This influx of retries causes all of Audible to fail: the system incurs an outage.  In this case, a number of software bugs — all detectable through traditional unit, integration, and functional testing, causes the system to overload and exhaust the available compute capacity in the cloud.  Thus, a resulting outage.  

### Taxonomy

When we look at this example, and the other examples that we surveyed, we can identify there are two major concerns for the developers of microservice applications.  First, *anticipation of application errors*: ensuring the application contains code error handling for all possible errors.  Second, *ensuring proper behavior and scalability of resilience countermeasures: the methods that we take to contain unexpected, untested failures when they occur.*  

We diagram this relationship below:

![Types in Taxonomy](/img/types-of-taxonomy.png)

We represent this as four quadrants, bifurcated by abstraction layer and whether or not resilience countermeasures are present.  

1. Failures might occur because our system fails to handle expected load.  For example, we may be missing a security policy or auto-scaling rule that is required for operation inducing external faults to our application.  These external faults may trigger latent, internal faults in our application.
2. Failures might occur because our infrastructure configurations are wrong.  For example, we might fail to configure auto-scaling rules or concurrent execution limits for serverless functions; this may cause our application to experience an external fault (e.g., concurrent execution limit exceeded.) These external faults may trigger latent, internal faults in our application.
3. Our application may be missing error handling code for possible errors: error codes we do not expect from external services, error codes we fail to handle from our own services through malfunctioning endpoints, by way of software defects, or services that are unavailable.
4. Our application may contain incorrect or unscalable error handling code.  In this case, an error causes the application or service to take a code path designed for error handling, but that error handling path may introduce additional load on the system causing the system, or some number of the application’s services, to fail.

Ok, so what is the theory.

### Theory of Resilience Testing

Why does the taxonomy matter?  It matters, because each quadrant needs to be addressed by a separate testing methodology, with separate tools, and separate techniques.

We can represent this as follows, by reimagining the same graph with different axes:

![Taxonomy](/img/taxonomy.png)

In this example, we have modified the abscissa to represent where the analysis needs to be performed; for the ordinate, we have modified it to represent whether or not we can identify the problem in a local environment or in the cloud.

When it comes to missing configurations, these can be detected locally, in the development environment.  In fact, missing configurations for cloud services can easily be detected through static analysis; missing application error handling code can *sometimes* be detected statically, but most definitely detected dynamically, *especially and most notably using fault-injection approaches like Filibuster.* 

However, when it comes to the problems of cloud configuration, things change.  Incorrect or unscalable cloud configurations, where autoscaling rules or concurrent execution limits are incorrect, must be detected in the cloud environment.  Since there is no way to run these services locally using those configurations, the only way to identify these issues are to run those configurations in the cloud under either actual or synthetic load.  We note that Azure has made some movement in this area by allowing them to run their serverless runtime environment locally.

When it comes to incorrect, or unscalable, cloud configurations that only cause faults under a latent application fault, these failures must be tested in the cloud environment *when we can induce faults.*  While chaos engineering can identify a number of these faults when they occur in the infrastructure, through failed instances or blackholed DNS, all of these failures surface themselves as errors or exceptions *in the application,* which indicates that we should be injecting these failures in a cloud environment to identify them, in the application where they can be explicitly controlled and exercised.

Our core observation: complexity increases as we move towards the upper right quadrant: we need to not only run our application in a cloud environment but also inject faults in that environment.  This is where chaos engineering is typically targeted — injection of faults in the production environment — but, based on our survey, the majority of bugs they discover fall into the quadrants below the x-axis: faults that should be identified well before applications are deployed into production.  When latent software defects are not identified in the local environment and result in outages, they *naturally appear* to fall into the upper-right quadrant, as application failures that result in capacity overloads, when they *actually originate* from the lower-right quadrant, simple application errors that could have been detected through more rigorous software testing before deployment.

## Takeaways

What are the takeaways?

1. Failures can occur because of both infrastructure and application-level problems.  Therefore, a multi-level testing approach is necessary to address both.
2. Application-level problems, such as the problems in the Audible example, can result in outages if left unhandled and then propagate to the infrastructure-level.  Therefore, try to eliminate as many problems as possible through local testing and then use testing in the cloud, under load with principled fault injection to determine if the system reacts to failures under load correctly.
3. Infrastructure-level misconfigurations should be detected statically, if possible, using configuration validation tools.  Verifying these configurations should be done using load testing tools, based on expected load.

If you're interested in this work, consider applying to Carnegie Mellon University or reach out to me for more information!