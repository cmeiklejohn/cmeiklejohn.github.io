---
layout: post
title:  "Filibuster 2.0: Byzantine Fault Injection with Hardcoded Fault Values"
date:   2023-09-09 08:00:00 -0000
group:  filibuster-2
categories: filibuster
---

Ever want to test your system against Redis returning wrong values, like instead of returning an error, it returns an empty string? What about an empty byte array? What about a database field being null? You can do it with Filibuster 2.0!

Let's return a null from a Redis get, easy! Simply done with Filibuster.

<img src="/img/1691310592563.jpeg" style="width: 100%">