---
layout: post
title:  "Building Tech-Transferrable Research Software"
date:   2023-08-12 00:00:00 -0000
categories: filibuster
group: short
---

The distance between research software and software that can be used by real software engineers is --- quite honestly --- enormous. It just doesn't get talked about enough, and it should.

### Finding bugs in Zookeeper

There's a long lineage of work (short: papers) on finding concurrency bugs in systems like Zookeeper, with a lot of it done by the University of Chicago.

Why is that?

Well, there are multiple factors, but perhaps the most interesting to me is that some time in the past, someone found a way to instrument Zookeeper in such a way that they could control thread scheduling and each RPC sent. Therefore, when new Ph.D. students come along, they can build on this existing work and focus only on the *algorithmic* improvements.

For example, SAMC (OSDI '14) had semantics-aware dynamic reduction policies like local/crash-message independence; FlyMC (EuroSys '19) had parallel flips and state symmetry. Same system, more bugs found each paper. The bugs just keep getting more complicated too, as it's a battle of attrition. From FlyMC: "The second bug was reported to appear once every 500 unit test cycles [...] successfully discovered 5 new bugs [...] bug depths ranged from 9 to 30 events."

In short, Zookeeper bugs get more and more complicated, we come up with better tricks to find them; and Zookeeper keeps getting better every day. One might even ask: when will Zookeeper run out of bugs to be found, if ever?

### Microservices

In my own Ph.D. work, I've been building software for testing microservices applications where one of the key tenets is that services are developed by different teams. This means that each service can be written in a different style, using different versions of libraries, with different dependencies, using different testing frameworks, using different DI frameworks, etc., etc. This makes mechanization of a "one size fits all" approach to testing... very hard.

I think there are two implications when it comes to software testing:

* From the industrial perspective, I feel like this is why developers often resort to infrastructure-level testing techniques, because utilizing an application-level testing technique across multiple services is hard, often less-valued, work.

* From the research perspective, it makes developing software that can run on real services very difficult. In short, don't be too generic in that you sacrifice performance, but be generic enough, having just the right hooks, that you can wire yourself into any framework.

### Open Questions

This is a skill that Ph.D. students just do not have. In fact, I often fail at it myself, and I was a professional engineer for 15 years in senior roles before starting my Ph.D.

Tech transfer isn't a goal for academics and doesn't seem to be really valued. As a community, we often rely on someone in industry to create a derivitive work that works on real things.

Perhaps this is how research is supposed to be in the field of Computer Science.

Is that how it should be in the field of Software Engineering?