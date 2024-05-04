---
layout: post
title:  "Filibuster 2.0: Developer Tooling as a Childhood Dream"
date:   2023-08-30 10:00:00 -0000
group:  filibuster-2
categories: filibuster
---

My favorite part of my Ph.D. thesis work has to be the UX/UI tool that I wrote for helping write and debug microservice resilience tests.  It's just so cool --- I'm realizing a childhood dream of building developer tooling ever since my high school days, when I was so excited to unwrap my first copy of Visual Studio.

Not only does it help visualize the RPCs that your application is making when you execute a test, if your test happens to fail because a fault was injected, it will prompt you with a question: was this failure expected or not? If so, it helps you write code into your test to specify what the behavior of your application is under failure.

What's neat here is the integrated Javadoc on how to write the tests, a mechanism for replaying the failure so you can attach a debugger, and if you provide enough hints to the system, it can even automatically figure out what your system will do under certain failures.

<img src="/img/1691033390525.jpeg" style="width: 100%">