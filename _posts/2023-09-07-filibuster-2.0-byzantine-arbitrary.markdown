---
layout: post
title:  "Filibuster 2.0: Byzantine Fault Injection with Arbitrary Faults"
date:   2023-09-07 08:00:00 -0000
categories: filibuster
group:  filibuster-2
---

Ever wanted to just throw all sort of values at your database and see what happens to your application? Filibuster's byzantine fault injector can take an arbitrary "value transformer" that looks like a functional fold, that allows you to come up with new fault injection scenarios as you inject faults!

Let's test our application against flipping characters in a string response from Redis! 

Here, we *observe* the response from the test that passes with no faults and then flip a character in the response in the test where no faults were injected. We actually built this to flip *every* character in the string. 

What was once "example" in the Redis response becomes " xample"! And then becomes "e ample"!

<img src="/img/1691310856004.jpeg" style="width: 100%">