---
layout: post
title:  "Revisiting: Chaos Engineering or Software Testing?"
date:   2023-08-13 00:00:00 -0000
categories: filibuster
group: short
---

__This addresses comments from LinkedIn on my previous blog post called "Chaos Engineering or Software Testing?"__

Last week, I took a bit of heat for my post on chaos engineering for couple reasons. I wanted to comment on them here.

---

1.) I said chaos engineering was random. First off, in my post, the context I was referring to was the initial incarnation of chaos engineering a la "chaos monkey" with my textual reference to 2007.  The initial chaos monkey was random in its instance termination. In retrospect, I should have been more precise. I was referring to this specifically because I had recently saw many posts about using chaos monkey on LinkedIn.

However, when it comes to random exploration, I was using it to draw a distinction from systematic exhaustive exploration: what I've (personally) found is that talking to people about what could fail and designing experiments around it isn't nearly as useful as a systematic exploration of all RPC that a service (or services) execute. Often, the weak point is an RPC or dependency that the developer doesn't remember is being called as part of some process. Hence, I prefer using the computers to figure out what to do and have them do it automatically.

And, let me tell you, microservices issue a *lot* of RPCs and developers are consistently surprised: "Oh, I forgot we were calling that service."

2.) I'm aware of the history of resilience engineering. I think it's useful for making sure that on-call works properly, fail-over of AZ/region/DCs work, people get paged, whatever. I don't disagree with any of that; in fact, I've cited Jesse's papers, Google's papers and talks, etc. all with high recommendations in my own papers and lectures that I've given on the topic at CMU.

---

What I *do* disagree with is using chaos engineering in place of isolated functional software testing. More specifically, I shouldn't be turning a server or availability zone off to make sure that the RPCs I am issuing from a different service in a different availability zone handle a Connection Error Exception: I can easily do that in the code. In the way I see it, you are issuing RPCs (library call) and it's your duty to test your application for what happens when those RPCs fail (exceptions), just like any other library call you'd test. If developer's use RPCs like ordinary library calls, they should be tested like library calls.

To be clear: I'm not saying this is the only testing that you do, it's just one step along a path. Learn what things return when they are failing and return errors; then, figure out what the larger system impact is: compositional testing and reasoning. No use injecting a fault in production when your code doesn't (correctly) handle errors without first finding out your code doesn't have a (correct) error handler.

(Waldo is going to want to murder me, but this the reality. Thankfully, there are tools for testing RPCs for failures, timeouts, high latency, etc.)

You don't use chaos engineering to randomly delete files to see if your application code handles a File Not Found exception, do you?