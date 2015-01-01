---
layout: post
title:  "Readings in conflict-free replicated data types"
date:   2014-07-22 12:29:30 -0500
categories: crdt
group: readings
---

_This is a work in progress post outlining research topics related to
conflict-free replicated data types, or CRDTs._

Yesterday, Basho announced the release of [Riak 2.0.0 RC1][RC1], which
contains a comprehensive set of "data types" that can be used for
building more robust distributed applications.  For an overview of how
to use these data types in Riak to avoid custom, and error prone, merge
functions, see the [Basho documentation site][docs].

You're probably more familiar with another name for these data types:
conflict-free replicated data types (CRDTs).  Simply put, CRDTs are data
structures which capture some aspect of causality, along with providing
interfaces for safely operating over the value and correctly merging
state with diverged and concurrently edited structures.

This provides a very useful property when combined with an eventual
consistency, or AP-focused, data store: Strong Eventual Consistency
(SEC).  Strong Eventual Consistency is an even stronger convergence
property than eventual consistency: given that all updates are delivered
to all replicas, there is no need for conflict resolution, given the
conflict-free merge properties of the data structure.  Simply put,
correct replicas which have received all updates have the same state.

Here's a [great overview][overview] by one of the inventors of CRDTs,
Marc Shapiro, where he discusses conflict-free replicated data types and
their relation to strong eventual consistency.

In this [Hacker News thread][hn], there was an interesting discussion
about why one might want to implement these on the server, why
implementing them is non-trivial, and what the most recent research
related to them consists of.

This post serves as a reading guide on the the various areas of
conflict-free replicated data types.  Papers are broken down into various
areas and sorted in reverse chronologically.

# Basics

Overviews and history of the various conflict-free replicated data
types, implementation details, problem statements, etc.

* [A comprehensive study of Convergent and Commutative Replicated Data Types](http://hal.inria.fr/inria-00555588)
  <span class="author">Marc Shapiro, Nuno Preguiça, Carlos Baquero, Marek Zawirski</span>
  <span class="date">2011</span>
* [Conflict-free replicated data types](http://dl.acm.org/citation.cfm?id=2050642)
  <span class="author">Marc Shapiro, Nuno Preguiça, Carlos Baquero, Marek Zawirski</span>
  <span class="date">2011</span>
* [CRDTs: Consistency without concurrency control](http://arxiv.org/abs/0907.0929)
  <span class="author">Mihai Letia, Nuno Preguiça, Marc Shapiro</span>
  <span class="date">2009</span>
* [A Commutative Replicated Data Type for Cooperative Editing](http://dl.acm.org/citation.cfm?id=1584339.1584604)
  <span class="author">Nuno Preguica, Joan Manuel Marques, Marc Shapiro, Mihai Letia</span>
  <span class="date">2009</span>
* [Designing a commutative replicated data type](http://arxiv.org/abs/0710.1784)
  <span class="author">Marc Shapiro, Nuno Preguiça</span>
  <span class="date">2007</span>

# Causality

Advanced causality tracking mechanisms, each focusing on a particular
specific problem.  Important in the implementation of some CRDTs and for
historical purposes.

* [Scalable and Accurate Causality Tracking for Eventually Consistent Stores](http://haslab.uminho.pt/tome/files/dvvset-dais.pdf)
  <span class="author">Paulo Sérgio Almeida, Carlos Baquero, Ricardo Gonçalves, Nuno Preguiça, and Victor Fonte</span>
  <span class="date">2014</span>
* [Dotted Version Vectors: Logical Clocks for Optimistic Replication](http://arxiv.org/abs/1011.5808)
  <span class="author">Nuno Preguiça, Carlos Baquero, Paulo Sérgio Almeida, Victor Fonte, Ricardo Gonçalves</span>
  <span class="date">2010</span>
* [Interval Tree Clocks](http://dl.acm.org/citation.cfm?id=1496330)
  <span class="author">Paulo Sérgio Almeida, Carlos Baquero, Victor Fonte</span>
  <span class="date">2008</span>
* [Bounded Version Vectors](http://gsd.di.uminho.pt/publications/gsd-2004-03)
  <span class="author">José Bacelar Almeida, Paulo Sérgio Almeida, Carlos Baquero.</span>
  <span class="date">2004</span>

# Uses

Various insights into developing on, and using, CRDTs in systems.

* [On the composability of the Riak DT map: expanding from embedded to multi-key structures](http://dl.acm.org/citation.cfm?doid=2596631.2596635)
  <span class="author">Christopher Meiklejohn</span>
  <span class="date">2014</span>
* [Riak DT map: a composable, convergent replicated dictionary](http://dl.acm.org/citation.cfm?id=2596633)
  <span class="author">Russell Brown, Sean Cribbs, Sam Elliott, Christopher Meiklejohn</span>
  <span class="date">2014</span>
* [Riak PG: distributed process groups on dynamo-style distributed storage](http://dl.acm.org/citation.cfm?id=2505305.2505309&coll=DL&dl=GUIDE)
  <span class="author">Christopher Meiklejohn</span>
  <span class="date">2013</span>
* [Incremental stream processing using computational conflict-free replicated data types](http://dl.acm.org/citation.cfm?id=2460762)
  <span class="author">David Navalho, Sérgio Duarte, Nuno Preguiça, Marc Shapiro</span>
  <span class="date">2013</span>
* [SwiftCloud: Fault-Tolerant Geo-Replication Integrated all the Way to the Client Machine](http://arxiv.org/abs/1310.3107)
  <span class="author">Marek Zawirski, Annette Bieniusa, Valter Balegas, Sérgio Duarte, Carlos Baquero, Marc Shapiro, Nuno Preguiça</span>
  <span class="date">2013</span>
* [Evaluating Dotted Version Vectors in Riak](http://asc.di.fct.unl.pt/~nmp/pubs/inforum-2011-2.pdf)
  <span class="author">Ricardo Goncalves, Paulo Sergio Almeida, Carlos Baquero, Victor Fonte, and Nuno Preguica</span>
  <span class="date">2011</span>

# Optimizations

Optimizations, critical to making some CRDTs practical.

* [Merging OT and CRDT algorithms](http://dl.acm.org/citation.cfm?id=2596636&CFID=513905613&CFTOKEN=18423880)
  <span class="author">Ahmed-Nacer Mehdi, Pascal Urso, Valter Balegas, Nuno Perguiça</span>
  <span class="date">2014</span>
* [Making operation-based CRDTs operation-based](http://dl.acm.org/citation.cfm?id=2596632&CFID=513905613&CFTOKEN=18423880)
  <span class="author">Carlos Baquero, Paulo Sérgio Almeida, Ali Shoker</span>
  <span class="date">2014</span>
* [Efficient state-based CRDTs by decomposition](http://dl.acm.org/citation.cfm?id=2596634)
  <span class="author">Paulo Sérgio Almeida, Ali Shoker, Carlos Baquero</span>
  <span class="date">2014</span>
* [Scalable Eventually Consistent Counters over Unreliable Networks](http://arxiv.org/abs/1307.3207)
  <span class="author">Paulo Sérgio Almeida, Carlos Baquero</span>
  <span class="date">2013</span>
* [An optimized conflict-free replicated set](http://arxiv.org/abs/1210.3368)
  <span class="author">Annette Bieniusa, Marek Zawirski, Nuno Preguiça, Marc Shapiro, Carlos Baquero, Valter Balegas, Sérgio Duarte</span>
  <span class="date">2012</span>

# Verification

Verification of CRDTs and their use in eventually consistent
applications.

* [Towards verifying eventually consistent applications](http://dl.acm.org/citation.cfm?id=2596638&CFID=513905613&CFTOKEN=18423880)
  <span class="author">Burcu Kulahcioglu Ozkan, Erdal Mutlu, Serdar Tasiran</span>
  <span class="date">2014</span>
* [Vector Clocks in Coq: An Experience Report](http://arxiv.org/abs/1406.4291)
  <span class="author">Christopher Meiklejohn</span>
  <span class="date">2014</span>

# Related

Related works; specifically programming languages which leverage
techniques related to lattices and order theory to provide determinism
when distributed.

* [Joining Forces: Toward a Unified Account of LVars and Convergent Replicated Data Types](https://www.cs.indiana.edu/~lkuper/papers/joining-wodet14.pdf)
  <span class="author">Lindsey Kuper, Ryan R. Newton</span>
  <span class="date">2014</span>
* [Freeze After Writing: Quasi-Deterministic Parallel Programming with LVars](http://www.cs.indiana.edu/~lkuper/papers/lvish-popl14.pdf)
  <span class="author">Lindsey Kuper, Aaron Turon, Neelakantan R. Krishnaswami, Ryan R. Newton</span>
  <span class="date">2014</span>
* [LVars: Lattice-based Data Structures for Deterministic Parallelism](http://www.cs.indiana.edu/~lkuper/papers/lvars-fhpc13.pdf)
  <span class="author">Lindsey Kuper Ryan R. Newton</span>
  <span class="date">2013</span>
* [Logic and Lattices for Distributed Programming](http://www.eecs.berkeley.edu/Pubs/TechRpts/2012/EECS-2012-167.pdf)
  <span class="author">Neil Conway, William Marczak, Peter Alvaro, Joseph M. Hellerstein, David Maier</span>
  <span class="date">2012</span>

# Talks

Finally, for those of you who learn more by watching, here's some talks.

* [Eventually Consistent Computation with CRDTs](https://www.youtube.com/watch?v=8_z9-iRiSZw)
  <span class="author">Christopher S. Meiklejohn</span>
  <span class="date">2014</span>
* [SyncFree: Large-Scale Computation Without Synchronization](https://www.youtube.com/watch?v=1KP_pxFhlVU)
  <span class="author">Annette Bieniusa and Christopher S. Meiklejohn</span>
  <span class="date">2014</span>
* [Berlin Buzzwords 2014 - Consistency without Consensus](https://www.youtube.com/watch?v=U6xLcIf1Qlw)
  <span class="author">Peter Bourgon</span>
  <span class="date">2014</span>
* [EmberConf 2014 - Convergent/Divergent](https://www.youtube.com/watch?v=qyVNG7fnubQ)
  <span class="author">Christopher Meiklejohn</span>
  <span class="date">2014</span>
* [Wicked Good Ruby 2013 - Bloom: A Language for Disorderly Distributed Programming](https://www.youtube.com/watch?v=66bU45vVF00)
  <span class="author">Christopher Meiklejohn</span>
  <span class="date">2013</span>
* [Strange Loop 2012 - Eventually-Consistent Data Structures](http://www.infoq.com/presentations/CRDT)
  <span class="author">Sean Cribbs</span>
  <span class="date">2012</span>
* [Strong Eventual Consistency and Conflict-free Replicated Data Types](http://research.microsoft.com/apps/video/default.aspx?id=153540&r=1)
  <span class="author">Marc Shapiro</span>
  <span class="date">2011</span>

I'm hoping to make this into a living document, so please submit [pull
requests][pull] or leave comments!

[pull]: https://github.com/cmeiklejohn/cmeiklejohn.github.io
[hn]: https://news.ycombinator.com/item?id=8066168
[RC1]: http://lists.basho.com/pipermail/riak-users_lists.basho.com/2014-July/015556.html
[overview]: http://research.microsoft.com/apps/video/default.aspx?id=153540&r=1
[docs]: http://docs.basho.com/riak/2.0.0/theory/concepts/crdts/
