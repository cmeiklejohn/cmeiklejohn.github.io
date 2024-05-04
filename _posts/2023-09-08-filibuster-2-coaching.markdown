---
layout: post
title:  "Filibuster 2.0: Coaching UI"
date:   2023-09-08 08:00:00 -0000
categories: filibuster
group: filibuster
---

Your developers are writing functional tests for their microservice and, when RPCs fail, they throw exceptions. Do they test the cases where it throws? Do they have tests for it?

Using Filibuster, you can automatically identify these scenarios and prompt the developer to answer these questions.

Here's a case where a developer threw an exception when a downstream RPC failed. We ask them: "did you mean to throw?" and, if so, they are prompted to tell the system that this exception is on purpose: this allows us to determine resilience failures automatically.

<img src="/img/1691309630814.jpeg" style="width: 100%">