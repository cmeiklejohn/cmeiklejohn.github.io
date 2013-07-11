---
layout: post
title:  "LambdaJam 2013: Webmachine Tutorial"
date:   2013-07-11 12:10:50 -0400
categories: erlang webmachine
---

Yesterday, as part of the [LambdaJam 2013][lambdajam] conference, [Sean
Cribbs][seancribbs] and I presented a workshop on building functional
web applications with [Webmachine][webmachine], a "toolkit" for building
correct well-behaved HTTP applications.

For those of you who weren't able to make it, both the [slides][slides]
and the [code][code] are available for you to follow along at home.

The tutorial starts with a basic hello world application, and walks you
though building upon that to create a Twitter clone with features such
as: CSRF token protection, streaming responses, conditional
requests, templating, and the visual debugger.

Try it out, and let me know what you think.

[webmachine]: http://github.com/basho/webmachine
[seancribbs]: http://twitter.com/seancribbs
[slides]: https://speakerdeck.com/cmeiklejohn/functional-web-apps-with-webmachine
[code]: https://github.com/cmeiklejohn/webmachine-tutorial
[lambdajam]: http://lambdajam.com/
