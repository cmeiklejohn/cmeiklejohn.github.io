---
layout: post
title:  "What is Chaos Engineering?"
date:   2022-03-17 00:00:00 -0000
categories: filibuster
group: filibuster
---

_I originally wrote this post on February 19th, 2021, when I was doing the
initial research that would turn into my automated resilience verification tool
[Filibuster](http://filibuster.cloud).  You can find my entire series of blog posts 
on Filibuster starting [here](http://christophermeiklejohn.com/filibuster/2021/10/02/filibuster-1.html)
and you can find our paper [here](http://christophermeiklejohn.com/publications/filibuster-socc-2021.pdf)._

Over the past few months, I've spent quite a bit of my free time both reading
and watching everything I can get my hands on that 
 is somehow related to Chaos Engineering to try to figure out what it actually is.  

However, the more I watch and the more I design my own experiments for small
microservice applications that I create, the less I feel I understand what Chaos
Engineering is.

Let's break it down.

## Techniques

To start, I'm going to breakdown precisely what I mean when I say I'm looking at Chaos Engineering.  

After hours upon hours of watching talks, reading articles, and reading the
excellent [Chaos Engineering book](https://www.oreilly.com/library/view/chaos-engineering/9781492043850/)
from O'Reilly Media, I've identified at least three distinct techniques that
seem to fall under the umbrella term Chaos Engineering or Resilience Engineering
(which is sometimes used quite loosely, as it has origins in a real science.)

For reference, you can find all three of these techniques mentioned in the Chaos Engineering book, under that umbrella term, but I'll do my best to highlight the differences.

* *Game Days*: a technique that was originally pioneered by Jesse Robbins at Amazon for verifying the technological and organizational robustness of large-scale distributed applications.  Jesse was a volunteer firefighter and based this technique on the idea that firefighters practice fighting fires before fighting real fires in controlled burns.  This was later practiced by many companies (e.g., Slack, Microsoft, Google) but there are two notable examples: Google, identifying that all of their monitoring was centrally located in the SF region when simulating an earthquake in the SF region; Facebook, beginning this practice to simulate DC outages due to floods after witnessing the impact of Hurricane Katrina.  What's important here is that Game Days simulate a large-scale incident and evaluate a organizational reponse to that incident.  [If you want more details, you can learn all of this here.](https://queue.acm.org/detail.cfm?id=2371297)

* *Chaos Engineering*: the Netflix-style Chaos Engineering technique where a KPI metric is monitored, a hypothesis made, a fault-injected in production on some subset of traffic, and a conclusion drawn from the metric.  We'll dive deeper into this for the remainder of the article.

* *Application-level Fault Injection*: a technique where Chaos Engineering style experimentation is performed, but targeted faults are injected in the application level in the form of latency via sleep calls, exceptions thrown, and other application-specific failures.  Notable examples are Gremlin's [ALFI](https://www.gremlin.com/community/tutorials/getting-started-with-application-level-failure-injection-alfi-hello-world/) product, LinkedIn's [LinkedOut](https://engineering.linkedin.com/blog/2018/05/linkedout--a-request-level-failure-injection-framework) Google Chrome extension-based fault-injection mechanism, and Netflix's newer systems deployed in production and discussed at ICSE, specifically [ChAP and Monocle.](https://arxiv.org/abs/1905.04648) _At this point, I would be remiss if I failed to mention that our tool, Filibuster, is a very specific version  of application-level fault injection that advances the state-of-the-art in academic microservice testing.  You can find our SoCC '21 paper [here](http://christophermeiklejohn.com/publications/filibuster-socc-2021.pdf)._

These techniques are evolutions of one another: Game Days originated in the early 2000s (2004), Chaos Engineering came into the public eye around the late 2000s (2007) and application-level fault injection is the newest kid on the block, with most of these systems being invented in the past few years.  

For this article, I want to investigate Chaos Engineering, specifically.

## Chaos Engineering

So, what does Netflix say Chaos Engineering is?

Chaos Engineering, as described by the Principles of Chaos Engineering website and the co-authored O'Reilly Media book from Netflix, follows a process that is roughy equivalent to what you probably remember from high-school science.  

It breaks down like the following, taken directly from the [Principles of Chaos Engineering website](https://principlesofchaos.org/):

1. Start by defining 'steady state' as some measurable output of a system that indicates normal behavior.
2. Hypothesize that this steady state will continue in both the control group and the experimental group.
3. Introduce variables that reflect real world events like servers that crash, hard drives that malfunction, network connections that are severed, etc.
4. Try to disprove the hypothesis by looking for a difference in steady state between the control group and the experimental group.

For Netflix, the 'steady state' metric is successful stream starts per second, or known by it's shorthand, [SPS](https://netflixtechblog.com/sps-the-pulse-of-netflix-streaming-ae4db0e05f8a).  SPS is a metric that very rarely deviates much week to week, has peaks and valleys throughout the day, and can be used as a quick sanity check to make sure Netflix is operating normally.

For the hypothesis, we need to write this in terms of the 'steady state' metric.  So, we might write something like, "when service X is unavailable, there is no variation in the SPS metric".

For the real world events, well, we will just crash some servers that are involved in loading the Netflix homepage.  From there, we attempt to disprove our hypothesis.  

There are a few things important in this type of experimental design:

1. The 'steady state' metric is an actual metric.  It shouldn't be a boolean condition, it should be a metric that has some variability where we can observe some notable variation when things are going wrong.  
2. The hypothesis should consider this variation and not be a trivial negation of the 'steady state' metric (easy, if you incorrectly specify the metric as a boolean).  We have to have some idea of what the impact may or may not be on the actual outcome of the system.
3. We must know the types of real-world events to introduce.  Many chaos engineering talks from folks in industry discuss that we can discover the ['unknown-unknowns'](https://en.wikipedia.org/wiki/There_are_known_knowns) by fault-injection: but the entire premise of this style of fault-injection means that we know the types of events that can occur and therefore evaluate their impact which is unknown (this, would actually be a known-unknown: we know the faults that might occur, but do not know their relevance or impact just yet.)
4. Finally, and probably the most important, we don't run experiments on things we know will affect the metric negatively.  In Netflix's case, we belive an outage of the bookmarks service (a non-critical service) shouldn't affect viewing of Netflix streams; but, we know that if all of our authentication services are down or video assets are unavailable due to a global outage, well, we don't need to run those experiments in production.

## Example: Netflix

In many of the talks from Netflix, we see the [same motivating example](https://www.infoq.com/presentations/rethinking-chaos-engineering/) presented many times.  I'll reproduce it here.

Netflix's AppBoot process is the process of loading the Netflix homepage on a device that has a Netflix app.  This process involves your device communicating with a Netflix API server, that then communicates with different microservices to load each row of content in your homepage.  This consists of loading content from My List, your Bookmarks --- which are your last played location within in progress movies or TV shows --- your personal Recommendations, what's trending, and so forth.  

As a chaos engineer, we might wonder what happens when a non-critical service such as the Bookmarks service goes down.  Let's run an experiment to find out!

So, we run an experiment where we monitor the SPS to determine that if the Bookmarks service is unavailable, does this affect the successful stream starts per second?  We might find a few things while running this experiment in production.  To do this, we split off 1% of our traffic to a control cluster, 1% to an experimental cluster and observe the SPS.  Here are some potential outcomes:

* If we don't have any error handling code for the Bookmarks service, well, then we probably run into trouble because our Netflix homepage fails to load.  We would definitely notice this through a variation in the SPS in the experimental group.
*  If we do have this error handling code -- and perhaps, we load other content instead and replace the Bookmarks list on our homepage with something else -- maybe this has *little to no effect* on SPS and we determine that, when the Bookmarks service is down, it has a negligible effect on the SPS metric and everything is fine.  

## Analysis

What I find curious about this chaos experimentation process is that I believe multiple concerns are being evaluated at the same time and we're using chaos engineering as a tool to evaluate both of them simultaneously.

### Robustness

First, let's consider faults around applications robustness.

If our API gateway is contacting multiple services to construct the Netflix homepage (e.g., My List, Bookmarks, etc.) one must assume that any number of these services might be down.  

This isn't a new realization -- in fact, literature on testing Service-Oriented Architectures (the forgotten grandparent of microservice architectures) for robustness discusses this in detail: at any point, the service you might be contacting could be unavailable (amongst a plethora of other errors related to incompatibilities in versioning, parameters, etc.) and your application must be able to handle it.  In fact, the idea of contacting an alternative service when the desired service isn't available or meet a QoS deadline has been discussed in detail a number of times.  We refer to these nowadays as *fallbacks* but they were referred to using different terminology around 14 years ago.  Most of the solutions for dealing with this problem in service-oriented architectures are quite heavyweight: they involve code generation, XML, WSDL, and SOAP; however, in modern-day microservice architectures that rely primarily on HTTP or gRPC, this could easily be tested using stubs (or, more advanced codegen techniques.)  

In the majority of microservice application code, this fallback behavior is written using either exception handlers or conditional branches, so we have to step back and ask _why wait until the code is in production to test this when it could easily be identified during development or testing?_

### Metrics

Next, let's think about using the SPS metric to evaluate the outcome of the experiment.

In the case of the Bookmarks example, let's consider that we have fallback code to load the Trending Now list instead of the Bookmarks when that service is down.  Here, we're looking to see whether the choice of displaying different content has an effect on the choice of whether people will continue to watch movies on Netflix using an aggregate metric.  This is distinctly different from evaluating whether or not our error handling code for the Bookmarks service being down is operating correctly -- this is effectly a form of split testing the Netflix homepage to determine whether or not certain content causes people to choose not to stream movies.  Given the evaluation is done on the granularity of a rather large metric, minor variations might not even be observed by those running the experiments -- it could have an effect, but not an effect that we care about from a business point of view.

## What Exactly is Chaos Engineering?

This idea of testing fallbacks using this style of Chaos Engineering at Netflix while observing the SPS became so pervasive, that Netflix set out to automate the process using a system called Monocle.  For Netflix, it was an easy process: since all of their remote procedure calls were made using Hystrix, the Hystrix configuration could then be used to construct a list of experiments that could be automatically run.  The ChAP (Chaos Automation Platform) that Netflix built allowed them to automatically spin up clusters, reroute the appropriate traffic, run the experiments, and report the results as a Jira ticket in the backlog.  

Unfortunately (for who, I wonder as I write this), this system was [disabled 2 months after launch](https://www.infoq.com/presentations/rethinking-chaos-engineering/) because it was creating large backlogs that weren't being addressed.  I found one thing that Nora Jones at Netflix observed particularly interesting: the Monocle system was created to automatically run these experiments because developers, who had the ability to run these experiments manually weren't.  Why?  When prompted with selecting the percentage of production traffic to route to these experiments, developers ran away scared at answering the question.

I'm left scratching my head at this point.

We all know that the majority of errors in software come from [error handling code that's untested](http://petertsehsun.github.io/soen691/current/papers/osdi14-paper-yuan.pdf).  So, testing this error handling code should be a good thing, right?  If we have enough foresight to anticipate that the Bookmarks service for Netflix is non-critical, and might be down, we would want to test that our error handling branch handles this case correctly, no?  Surely.  But, presumably we should be testing this in development, when we're writing the code to contact that service.  The Service-Oriented Architecture people knew this; and we all write sequential, single machine programs that sometimes make a call to a library that might return an error (e.g., LFI, etc.), and we test that too, right?  

So, why aren't we testing this behavior in development, long before we get to our testing, staging, and production enviornments?

Second, there is value in running experiments in production to determine the impact of our choices in handling errors.  But, that should be long after we have decided to handle the error, when we are trying to evaluate our different choices in handling errors.  It feels that these things should be decoupled; we shouldn't be using chaos engineering as the only outlet for testing both the way we recover from errors -- along with whether or not our choices in handling those errors are correct.

So, I leave you with this question: what exactly _is_ chaos engineering?