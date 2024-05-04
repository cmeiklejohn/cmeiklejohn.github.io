---
layout: post
title:  "Filibuster 2.0: Healthcheck your Functional Test Suite with API Coverage"
date:   2023-09-02 08:00:00 -0000
categories: filibuster
group: filibuster
---

Pushed out a new prototype Filibuster feature tonight: 🎉 use Filibuster's IntelliJ plugin to get a "health check" of your microservice's functional test suite. 🎉 

Here, I can quickly open the plugin after running my test suite and see what RPC methods my service is exposing, how many unique functional tests I have covering those methods, how many specific Filibuster tests I have for those, and how many tests were automatically generated using Filibuster to exercise unique fault injection scenarios: where I injected HTTP and GRPC faults.

<img src="/img/1691649263651.jpeg" style="width: 100%">