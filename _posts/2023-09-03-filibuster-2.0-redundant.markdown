---
layout: post
title:  "Filibuster 2.0: Microservice Linter, Redundant RPCs"
date:   2023-09-03 08:00:00 -0000
categories: filibuster
---

You can also use Filibuster's dynamic analysis linter to find microservice smells. 

Here's one: executing the same RPC multiple times, since you've already got the response and shouldn't issue a failure-possible, expensive RPC again.

<img src="/img/1691311592348.jpeg" style="width: 100%">