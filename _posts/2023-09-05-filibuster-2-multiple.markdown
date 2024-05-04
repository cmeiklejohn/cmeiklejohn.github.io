---
layout: post
title:  "Filibuster 2.0: Microservice Linter, Multiple Invocations to the Same RPC Method"
date:   2023-09-05 08:00:00 -0000
categories: filibuster
---

You can also use Filibuster's dynamic analysis linter to find microservice smells. 

Here's one: invoking multiple RPCs to the same service because you can't send them all together! This leaves you at risk for partial side-effects being applied: refactor your API to let developers supply all inputs and write data to your database transactionally!!!

<img src="/img/1691311807950.jpeg" style="width: 100%">




















~