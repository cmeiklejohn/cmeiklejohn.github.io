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
