---
layout: post
title:  "Filibuster 2.0: Developer Tooling as a Childhood Dream"
date:   2023-08-30 11:00:00 -0000
group:  filibuster-2
categories: filibuster
---

What a long strange trip (the Ph.D.) it's been:

In 2018, when restarting my Ph.D. journey at Carnegie Mellon University, I wrote up a proposal for a Microsoft Research Fellowship that was sadly, rejected. In it, I said I wanted to make microservice programming more like monolithic programming with an integrated development environment. It even had fake screenshots we hacked with a fake LSP server. It was a large vision, where I wanted distributed type checking, fault injection, a linter, etc.

Fast forward to late 2023, and I'm on the precipice of defending my Ph.D. While I had to rework my vision because it was much too large --- and along the way completely and utterly lost the thread a few times diverging off into distributed runtimes, stateful serverless, model checking, and the like --- I finally settled on fault injection testing as the primary topic of my Ph.D. thesis. (No more CRDT!). You can blame a certain Program Analysis course at Carnegie Mellon University that completely changed my mind about what is interesting about Software Engineering.

...and, my vision is slowly becoming true!

## Filibuster 2.0

Here's Filibuster today: a vertically integrated fault injection, exhaustive, resilience testing technique.  It's integrated directly into IntelliJ.  You can inject faults, lint your code for bad microservice programming patterns, visualize your execution... and wait for it, it even helps create "negative" tests through integrated programming recommendations that exercise your application code in "bad" situations, verifying that, indeed, you react to the bad properly. It reports coverage and even detects several bad test writing patterns.

<img src="/img/1689988322009.jpeg" style="width: 100%">

## "Coaching" UX

By far my favorite part of my Ph.D. thesis work has to be the UX/UI tool (and methodology) that I wrote for helping write and debug microservice resilience tests.  It's just so cool --- I'm realizing a childhood dream of building developer tooling ever since my high school days, when I was so excited to unwrap my first copy of Visual Studio.

Not only does it help visualize the RPCs that your application is making when you execute a test, if your test happens to fail because a fault was injected, it will prompt you with a question: was this failure expected or not?  If so, it helps you write code into your test to specify what the behavior of your application is under failure.

What's neat here is the integrated Javadoc on how to write the tests, a mechanism for replaying the failure so you can attach a debugger, and if you provide enough hints to the system, it can even automatically figure out what your system will do under certain failures.

<img src="/img/1691033390525.jpeg" style="width: 100%">