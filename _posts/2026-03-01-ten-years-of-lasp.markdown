---
layout: post
title:  "Ten Years of Lasp"
date:   2026-03-01 08:00:00 -0000
group: lasp
categories: lasp distributed
---

Peter Van Roy and I just published a retrospective at [PPDP '25](https://dl.acm.org/doi/10.1145/3756907.3756910) — the same venue where the original Lasp paper appeared a decade ago — looking back at ten years of influence from Lasp, the coordination-free programming model Peter and I built together in 2015. I wanted to write a bit about what that paper covers, and what it actually felt like to watch an idea travel from a prototype to something that showed up in production systems I never expected.

### What Lasp Was

Lasp was a programming model built on top of Conflict-Free Replicated Data Types (CRDTs). The core idea was simple: if you structure your distributed computation around data types that are guaranteed to converge — regardless of the order in which updates arrive, regardless of network partitions — you can eliminate most of the coordination overhead that makes distributed systems so hard to build and reason about.

The CAP theorem had been the backdrop for a lot of the distributed systems conversation in that era. CRDTs offered a path toward availability and partition-tolerance without sacrificing convergence, but their use was still mostly ad hoc. Lasp tried to give that a principled, declarative home: a functional programming model where eventual consistency wasn't something you bolted on, but the default assumption.

I was working at Basho Technologies at the time, deep in the operational reality of building eventually consistent databases in Erlang. That context shaped everything about how Lasp was designed. The pain points weren't hypothetical — I had watched coordinaton-heavy systems fall over under real load, and I was motivated to find something better.

The motivating use case came from Rovio Entertainment, the company behind Angry Birds. Their game required tracking player state across a globally distributed, occasionally connected player base — exactly the kind of scenario where coordination is prohibitively expensive and convergence is what you actually need. That use case grounded Lasp's design in something real.

### The Scale Experiments

Lasp was part of the EU-funded SyncFree research project, which brought together researchers across Europe to advance the foundations of CRDT-based distributed systems. As part of our deliverables to the European Commission, we ran large-scale experiments on AWS — over 1,000 nodes — to demonstrate that these techniques worked at scale. Between 2015 and 2017, that kind of deployment was genuinely difficult: few frameworks supported it, and the operational challenges around service discovery, dissemination, and convergence were not trivial.

We published those results at PPDP in 2017. What I remember most clearly is how much of the work was just fighting infrastructure that wasn't ready for what we were trying to do. To get Lasp to run on top of Mesos's Marathon framework — because I was contracting at Mesosphere at the time — I had to essentially rebuild the network layer of Lasp from scratch. That work became Partisan.

### The Things That Grew Out of It

Partisan, the open-source distribution layer I built to run those experiments, ended up being more widely adopted than Lasp itself. It became a high-performance alternative to Erlang's built-in distribution, influenced improvements to Erlang's distributed networking internals, and eventually found its way into open-source and proprietary projects I had no involvement in. Alejandro Ramallo, an Erlang developer who adopted both Lasp and Partisan, deployed them in systems powering LoJack's stolen car recovery service across several South American countries. I did not see that coming.

Lasp was also adopted as the storage backend for Erleans, an open-source implementation of Microsoft Orleans on Erlang — which is a satisfying full-circle moment given that I spent two summers at Microsoft Research working on Orleans' transactional semantics.

The fault-injection mechanisms in Partisan's early network layer eventually became the seed of Filibuster, the fault injection testing framework that was the subject of my Ph.D. dissertation. The path from Lasp to Partisan to Filibuster to DoorDash is not a straight line, but there is a line. The only way to verify that a coordination-free system converges correctly is to partition the network and check what happens when it heals. That thinking, originally motivated by Lasp's correctness requirements, eventually became a general-purpose approach to testing microservice resilience.

### What the Academic Community Did With It

The retrospective paper covers quite a bit of follow-on academic work that I found genuinely gratifying to trace. Systems like Katara built on Lasp's foundational principles to synthesize CRDTs with verified lifting. LoRe and Varda extended Lasp's declarative, coordination-free semantics toward verifiably safe compositional distributed software. Several PhD dissertations — from researchers in Belgium, Portugal, the UK, the US, and elsewhere — adopted Lasp as either a technical or theoretical foundation.

The work Matthew Weidner and Heather Miller and I published together on composing op-based CRDTs with semidirect products also grew directly out of the Lasp model. That paper, published at ICFP in 2020, is something I'm quietly proud of — it's one of the more mathematically interesting things I've worked on.

The PPDP program committee awarded the original Lasp paper their 2025 retrospective award, which is what prompted the retrospective paper in the first place. It is a bit surreal to receive a recognition like that for work that started, essentially, as a research project I threw myself into while working at Basho with no particular expectation that anyone outside the SyncFree project would care.

### Why It Still Matters

The retrospective paper makes the case — and I believe it — that Lasp's core insight is increasingly relevant rather than less. Multi-region active-active deployments are becoming standard architecture for companies at scale. A single round-trip operation at the speed of light takes 133 milliseconds. Coordination protocols like Two-Phase Commit or Paxos require multiple round trips just to agree on a single value. The math doesn't work for globally distributed state if coordination is your default.

Edge computing, local-first software, offline-capable mobile applications, and federated systems all face the same constraint: central coordination is sometimes simply unavailable. Coordination-free convergence isn't a research curiosity in those environments — it's a requirement.

I'll be curious to see what the next ten years look like for CRDTs specifically. The theoretical foundations are solid. The open question is whether the programming model abstractions — the things that Lasp was trying to work out — will find their way into mainstream languages and frameworks in a form that developers can actually use without thinking too hard about it. I think they will.

The full paper is available on [ACM](https://dl.acm.org/doi/10.1145/3756907.3756910).
