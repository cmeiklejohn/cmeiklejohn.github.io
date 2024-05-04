---
layout: post
title:  "Filibuster 2.0: Developer Tooling as a Childhood Dream"
date:   2023-08-31 10:00:00 -0000
categories: filibuster
group: filibuster
---

🎉 Happy to announce the 2.0 release of Filibuster (for JVM languages!) 🎉

This release includes:

- Lots of improvements for HTTP fault injection for Armeria's HTTP clients and visualization of HTTP faults into the Filibuster IntelliJ plugin.
- Introduction of fault injection support for Redis, CockroachDB, DynamoDB, and PostgreSQL.
- Byzantine fault injection (value-based fault injection) support for all supported databases, GRPC, and HTTP. BFI is supported through two modes: specification of explicit-values for BFI or a fold-based value transformer for dynamically creating new BFI scenarios at runtime.
- A new testing library for writing microservice resilience tests with a "coaching" UI that helps you identify bugs or specify behavior under fault, integrated directly into IntelliJ through the use of plugin.
- Support for integrated code coverage reporting with CodeCov and reporting on API coverage through the UX.

Shout out to Michael Issac Assad for his incredible hustle in getting all of this working. 🍻

Look forward to a series of (blog) posts in the coming weeks on how to use all of these new features.