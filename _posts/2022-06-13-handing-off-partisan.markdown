---
layout: post
title:  "Handing off Partisan"
date:   2022-06-13 00:00:00 -0000
categories: filibuster
group: filibuster
---



I’m handing off maintenance of Partisan, the distributed runtime system I built as part of my Ph.D., to @aramallo.  

Alejandro has done tremendous work: from fixing bugs, feature enhancement to performance improvements.

Alejandro has brought Partisan to places that, as a PhD student who works mostly in a lab, never imagined:  
_LO/JACK LATAM, the lost vehicle recovery service, since 2019, has been using Partisan as part of its Magenta Platform, tracking 300k vehicles, 10k devices, receiving over 30M GPS transmissions each day!_

You can learn more about how it's used here.

<iframe class="youtube-player" width="480" height="360" src="//www.youtube.com/embed/XxJ1IS8mo84" frameborder="1"> </iframe>

Leapsight has used Partisan as the backbone of two of their products: 

* [Bondy](https://github.com/Leapsight/bondy), an open source, always-on and scalable application networking platform for modern architectures. Bondy is an all-in-one event and service mesh that offers both Publish-Subscribe (PubSub) and routed Remote Procedure Calls (RPC). Bondy implements the open Web Application Messaging Protocol (WAMP) and is written in Erlang.
* [PlumDB](https://gitlab.com/leapsight/plum_db), a database globally replicated via Epidemic Broadcast Trees and lasp-lang’s Partisan.  An offspring of Plumtree and Partisan, a descendant of Riak Core Metadata Store.

