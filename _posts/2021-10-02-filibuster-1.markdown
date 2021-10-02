---
layout: post
title:  "Announcing Filibuster: Part 1, Corpus"
date:   2021-10-02 00:00:00 -0000
categories: filibuster
group: filibuster
---

In a few days, we will be releasing the prototype implementation of Service-level Fault Injection Testing (accepted to [ACM SoCC '21](https://acmsocc.org/2021/)), called Filibuster, as open-source on GitHub.  Service-level Fault Injection Testing is a new technique for the resilience testing of microservice applications, during development or in continuous integration environments.  Filibuster was developed by the Composable Systems Lab, part of the Institute for Software Research, at Carnegie Mellon University, by me, Andrea Estrada, Yiwen Song, Heather Miller, and Rohan Padhye.

Over the next week or so, leading up to the open-source release, I'm going to write a series of blog posts that describe what exactly Filibuster is, how it can be used to identify bugs, and then walk through identifying an example bug in an application using Filibuster.  But, before we get into that, I want to take a little time to talk about the challenges of doing research in the area of microservice resilience testing.

First, a bit of history.

## History

In 2018, our group released an open-source alternative distributed runtime for Erlang called [Partisan](https://github.com/lasp-lang/partisan), that was published at USENIX ATC '18.  The main contributions of this work were improving both the performance of distribution, through parallelism, and the scale of Distributed Erlang, by supporting different clustering topologies for applications (peer-to-peer, etc.).  Partisan itself wasn't developed in isolation, Partisan was in fact extracted (and subsequently improved and independently evaluated) from my previous work on a CRDT-based database and programming model, [Lasp](https://github.com/lasp-lang/lasp), which was published at PPDP '15 and then evaluated at large-scale at PPDP '17 on an example application taken from industry.

To build Partisan, we needed to implement our own networking layer that all communication for Distributed Erlang would run through.   We wanted to build this in a modular way, because one of the goals of Partisan was to allow, through configuration, the ability to guarantee certain message orderings: for example, causal or total order.  To achieve this, we build an interposition system into Partisan that allowed any other component in the system to register callbacks on either message transmission or message reception.  This enabled a causal order framework we developed, originally called Ishikawa and later integrated into Partisan, to plug in, attach clocks to messages and then enforce that those messages were delivered in the correct order.   While our paper was under submission, we realized that with control of all of the messages in the system and the ability to interpose on those messages, we could easily use these mechanisms to perform fault-injection on any application that was ported to run on Partisan.  At the time, we thought this was rather novel, but much, much later we realized that a system in 1991, Orchestra, did this using TCL scripts over TCP packet payloads for fault-injection testing.

Since we primarily used the initial version of this new fault-injection behavior in Partisan to test commit protocols by injecting omission faults causing the protocols to stop making progress, we named it Filibuster.

## Partisan's Filibuster

The original version of Filibuster, which was part of Partisan, was used to test a number of commit and broadcast protocols that we implemented in Erlang to demonstrate that we could identify known bugs or deficiencies: in fact, I wrote about it [in a blog post on this blog in 2019](http://christophermeiklejohn.com/erlang/partisan/2019/04/20/fault-injection-reliable-broadcast.html) and [gave a talk about it at Code BEAM '19 in San Francisco.](https://codesync.global/media/partisan-testable-high-performance-large-scale-distributed-erlang/)  In addition to investigating reliable broadcast, we also targeted Two-Phase Commit, Collaborative Termination Protocol, and Three-Phase Commit with our original version.  Our original approach was two-pronged and targeted at protocol development: as you were developing the protocol you performed exhaustive omission failure testing of messages (up to a bound) and then switched to a stateful QuickCheck-style of testing where a model of your application behavior that was already passing with QuickCheck, was given to Filibuster, which would then interpose enabling and disabling faults in between commands that were issued to your system.  We used this model of testing to test the aforementioned commit protocols and a reliable broadcast protocol.

From there, we ported a number of applications to run on Filibuster and Partisan to perform further testing: paxoid, a Paxos implementation written in Erlang that was being used in production; Lashup, a CRDT-based database that was previously used inside of Mesosphere's DC/OS (of which, I had also worked on as an employee of Mesosphere, prior to starting my Ph.D.); the distributed database, Riak (of which, I had worked on for 5 years as an employee of Basho, prior to starting my Ph.D.) and erlang-hbbft, an implementation of HoneyBadgerBFT, implemented and used in production by Helium.  For each of these applications, I had to then write a QuickCheck stateful model that described the application behavior. For some of these it was easy: a strongly consistent key-value store or atomic broadcast protocol; for others, it was much harder: an eventually consistent key-value store.  In fact, some of the results were surprising: some of the systems had bizarre safety properties that were hard to express.  For example, paxoid, a system primarily used for handing out primary keys, under network partition, would potentially renumber the keys given out under conflicts to maintain a total order, forcing clients to get new keys, which was desired behavior of the system and difficult to express without reimplementing the arbitration logic into the test itself.

We submitted this work to a conference in 2019 and it was rejected for several reasons.  First, we didn't have a good bug corpus to work from, as many of these systems had bugs in earlier iterations that were not documented fully enough to reproduce.  Second, we found several new bugs; however, these bugs were explained away as behavior of the system.  Third, our two-pronged approach was a bit too ad hoc and problematic and difficult to use.  Fourth, and finally, we relied on a somewhat problematic static analysis for reducing redundant test cases during exhaustive search that needed more precision.

As we have completely abandoned this line of work, I'll omit the details.

## Filibuster 2.0

Around this time, I made the switch from working on testing protocol development — of which there was significant research using stateless model checkers to identify faults from message reordering and message omission with systems like FlyMC, SAMC on systems like Apache Cassandra and Zookeeper — to microservice applications.  

We reimplemented Filibuster from scratch in Python, targeting requests issued between different services using HTTP, and found an open-source microservice application that we performed testing on.  Using this application, we ran a comparison between running traditional-style chaos experiments with random fault-injection (designed to mimic using Gremlin, but not actually using Gremlin itself) and using Filibuster to identify a bug, demonstrating Filibuster could identify the bug faster.  In lieu of identifying any other open-source microservice applications, we resorted to testing a number of student implementations of a causally consistent database, implemented in Python as Flask services, and identified a number of bugs in the applications that were not caught by an autograder used by the classes TAs.  

We submitted this work in 2020 to two different conferences, after the first round of feedback, and again were rejected both times.  

Again, one of the key problems with the research: the weak evaluation.  

## Filibuster 3.0

In the Fall of 2020, after completing an internship at Amazon in the Automated Research Group in software testing, I was back, ready to try again.  With a pandemic, stay-at-home order in place, we decided to take a major step back and reassess a new approach for the project.  The major problem with the current iteration of the work: the evaluation.  We needed to find a way to improve the evaluation.

In the Software Engineering community, researchers working on bug finding, flaky tests, or automated program repair typically have several corpora of applications, that contain known bugs and the revisions where they were fixed, to work from.  It's their benchmark, similar to a TPC-C benchmark in the transaction processing community.  However, most of these applications are monolithic in design, and command line applications, libraries, or other tools.  There has been recent work this year on building a corpus for microservices to fulfill a similar goal, but after investigation, most of the bugs contained in this corpus are just normal application logic bugs — incorrect display in a UI, incorrect values returned from a service, etc. — they aren't bugs *specific* to microservice architectures.

In the Systems community, most researchers working on improving identifying bugs in distributed applications focus on *infrastructure:* Cassandra, Zookeeper, etc.  However, these applications, while distributed, differ in their design as compared to microservices: nodes are typically deployed as replicas of one another and are optimized to send as few messages as possible to reduce latency, overhead, etc.  Many of the stateless model checker approaches to identifying bugs (e.g., FlyMC, SAMC, etc.) optimize and prioritize their search based on these traits: for example, symmetry reduction, a technique used with dynamic partial order reduction, avoids testing executions where a single fault is explored on each replica of a node with the same functionality, under the assumption that the behavior will be the same in all executions.   Because these infrastructure systems are commonly used as evaluation for fault-injection in distributed systems, a common thread of reviewers from all of our previous submissions: why aren't we testing using the corpus that these systems are using?

Microservice architectures are completely different: they optimize for modularity for rapid software development with small teams, in the same way a monolithic piece of software would be modularized by functionality.  As an example, Uber in 2020 was composed of 2,200 microservices: this doesn't look nor operate like a Zookeeper cluster.  

We knew we needed to build a corpus, and from that corpus we would be able to identify search optimizations specific to microservices, in the same way that techniques like symmetry reduction had optimized for distributed infrastructure.  

### Building a Corpus

Microservice applications just aren't open source; that's the core issue.  

In most cases, the microservice code that is being written at companies is core, intellectual property and remains private.  When something does get open sourced, it's typically one service, in isolation, with any specific logic stripped about what services consume it or what services it might consume when deployed in a real system.  When bugs exist, they may not even be disclosed — we talked to one industry developer working on microservices who said that legally, as a publicly traded company, they cannot disclose this information.  We confirmed this by performing searches of GitHub for applications that used HTTP and GRPC libraries to make requests and manually went through the results to identify microservices: even if we found one, it would be in isolation and there wasn't a full application.  

*(Full disclosure: there are two known microservice applications, Hipster Shop and Sock Shop, that are fully open source and used to demonstrate how to build microservices. However, these applications do not contain bugs, and are implemented in multiple languages, which makes it difficult to do research with due to the overhead of implementing the prototype in X different languages.)*

We knew we needed to start somewhere different.  As one of our primary goals was to demonstrate that many types of chaos engineering-style experiments that developers were running could be done locally, in development, before code ships, we decided to start there.  For our survey, we decided to go to the chaos engineering literature.

For our survey of this literature, I did two things.  First, I looked at every single conference over the past few years (e.g., Chaos Conf, re:Invent) looking for talks about chaos engineering and aggregated them.  Second, I went to companies that provided chaos engineering services and used their client list to then search for those companies, identify any talks that they gave on chaos engineering, and archived them.  I spent multiple weeks sitting on my sofa, Chromecast'ing every single talk and writing up notes on every single talk I watched.  I did the same for blog posts when I couldn't find a conference talk available on YouTube or Vimeo.

When watching the talks, I looked for a few different things:

- First, did the presenter talk about an actual chaos experiment they ran in enough detail that I could reproduce it myself.
- Second, did the presenter talk about a bug they ran into in enough detail that I could reproduce it?
- Third, did the presenter talk about a pattern (e.g., circuit breaker, etc.) that they used to improve resilience of their application?
- Fourth, did the presenter talk about an architectural pattern in enough detail that I could reimplement it?

After a survey of around 77 presentations, I aggregated the results.  I then built a taxonomy of the bugs that we identified through this survey and categorized them.  For example, did the bug occur because of an error in application code?  Did the bug occur because of misconfiguration of cloud service infrastructure?  Could I have found the bug using a mock, if I had written it?  Did the bug in the infrastructure only occur because of a latent bug in the application?  

After building this taxonomy, we then looked at the local testing techniques that could target each area taxonomy and identified the first two areas that we wanted to target: 

- bugs, that either were or were not found using chaos engineering, that could have been identified using traditional mocking techniques;
- chaos experiments that were run in staging or production, that could have been performed locally using traditional mocking techniques.

Finally, I collected an interesting list of microservice request patterns that we identified during the survey.

### Implementing and Evaluating the Corpus

In the Spring of 2021, I had two awesome undergraduate research (Andrea Estrada and Yiwen Song) students at Carnegie Mellon University, join my project to help out for the semester.  We split efforts up with the goal of having a completed project by April, leaving a month to write a paper for SoCC '21.  

Since we now knew precisely the class of applications we were going to target, we would, in parallel, completely rewrite Filibuster from scratch with this class of applications in mind, while building out a testing harness for the applications and implement the entire corpus we identified.  

In total, we implemented a corpus containing 4 applications reproduced from industry: Audible, Expedia, Netflix, and Mailchimp and 8 example applications to demonstrate different request patterns: from retries to fallback requests, to default responses under unavailability, and different types of ways requests can be structured and nested.

For our SoCC '21 submission, we did a full evaluation of this corpus and were able to exhaustively test all applications while identifying all of the bugs contained in the corpus.  We demonstrate that this can be done rather quickly: we integrated Filibuster both in the local development environment as part of the development experience and integrated it with Amazon's CodeBuild CI/CD environment as well.

## Conclusion

Our corpus is one of the three main contributions of our ACM SoCC '21 paper on Service-level Fault Injection Testing.  We believe that the lack of work on identifying resilience bugs in microservice architectures in both the Systems and Software Engineering communities is a direct result of a lack of such a corpus and we're hoping that our work helps bridge that gap moving forward.  As part of the open source release of Filibuster, we provide not only the corpus, but tooling to automatically Dockerize each application, and deploy using Compose, Minikube, or EKS.

We believe that resilience engineering and this type of testing of microservice applications is an important aspect of modern day application development and it's an area that is not discussed enough in academia.  This is evidenced by the amount of material on chaos engineering and the sheer amount of companies of all sizes — large tech firms (e.g., Microsoft, Amazon, Google), big box retailers (e.g., Walmart, Target), financial institutions (e.g., JPMC, HSBC), and media and telecommunications companies (e.g., Conde Nast, media dpg, Netflix.) — who have adopted such approaches and discussed them publically.

*In our next post, we will talk about our second contribution, the technique of Service-level Fault Injection, and how it can be used to identify resilience issues.  In our final post, we will discuss our third contribution, dynamic reduction, a technique that leverages the structure of microservice applications to reduce testing overhead.*