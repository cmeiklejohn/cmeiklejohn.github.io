---
layout: post
title:  "Handing off Partisan"
date:   2022-06-13 00:00:00 -0000
categories: erlang partisan 
group: partisan
---

I’m happy to announce that I am handing off maintenance of [Partisan](https://github.com/lasp-lang/partisan), the distributed runtime system I built as part of my Ph.D., to [Alejandro Ramallo](https://github.com/aramallo/partisan) at Leapsight.

Alejandro has done tremendous work on Partisan over the last two years: from fixing bugs, feature enhancement to performance improvements.

Alejandro has also brought Partisan to places that, as a PhD student who works mostly in a lab, never imagined:  
_LO/JACK LATAM, the lost vehicle recovery service, since 2019, has been using Partisan as part of its Magenta Platform, tracking 300k vehicles, 10k devices, receiving over 30M GPS transmissions each day!_

You can learn more about how it's used here:

<iframe class="youtube-player" width="480" height="360" src="//www.youtube.com/embed/XxJ1IS8mo84" frameborder="1"> </iframe>

## Industrial Usage

Leapsight has used Partisan as the backbone of two of their products: 

* [Bondy](https://github.com/Leapsight/bondy), an open source, always-on and scalable application networking platform for modern architectures. Bondy is an all-in-one event and service mesh that offers both Publish-Subscribe (PubSub) and routed Remote Procedure Calls (RPC). Bondy implements the open Web Application Messaging Protocol (WAMP) and is written in Erlang.
* [PlumDB](https://gitlab.com/leapsight/plum_db), a database globally replicated via Epidemic Broadcast Trees and lasp-lang’s Partisan.  An offspring of Plumtree and Partisan, a descendant of Riak Core Metadata Store.

## Future Plans

Alejandro has a lot in store for Partisan:

- API usage
  - Add remote monitoring support of the local node using partisan.
  - Allowing the parallelism setting to apply in a per-channel, not global, setting.
  - Normalize all APIs to use the URI encoded remote pids, references, and node names.
- Feature: Casual delivery
    - Disk-based storage for failover.
    - Implement reliabile causal broadcast
        - Ishikawa? [https://github.com/lasp-lang/ishikawa/blob/master/src/ishikawa.erl](https://github.com/lasp-lang/ishikawa/blob/master/src/ishikawa.erl)
        - LoCaMu?  [https://www.gsd.inesc-id.pt/~ler/reports/valtersantosea.pdf](https://www.gsd.inesc-id.pt/~ler/reports/valtersantosea.pdf)
- Maintenance: Overlay Tree Construction
    - One `partisan_plumtree_broadcast` server per channel to increase throughput
    - Explore allowing concurrent access to broadcast members by making `partisan_plumtree_broadcast` server use `ets`
    - Implement [Thicket](https://asc.di.fct.unl.pt/~jleitao/pdf/srds10-mario.pdf)
- General improvements
  - OTP 25 support
  - QUIC Transport