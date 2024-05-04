---
layout: post
title:  "Filibuster 2.0: Microservice Linter, Requests become part of a Response"
date:   2023-09-04 08:00:00 -0000
group:  filibuster-2
categories: filibuster
---

You can also use Filibuster's dynamic analysis linter to find microservice smells. 

Here's one: using the arguments from one RPC to Service A as the inputs to a different RPC on the same Service A. You should refactor your API so I don't have to make multiple RPCs!!

<img src="/img/1691311683887.jpeg" style="width: 100%">