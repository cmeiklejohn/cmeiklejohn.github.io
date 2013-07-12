---
layout: post
title:  "Readings in distributed systems"
date:   2013-07-12 12:00:35 -0400
categories: distributed systems
---

_This post is a work in progress._

Inspired by a recent purchase of the [Red Book][redbook], which provides
a curated list of important papers around database systems, I've decided
to begin assembling a list of important papers in distributed systems.
Similar to the Red Book, I've broken each group of papers out into a
series of categories.

## Consensus

The problems of establishing consensus in a distributed system.

* [In Search of an Understandable Consensus Algorithm][raft] -
    Diego Ongaro, John Ousterhout - 2013
* [A Simple Totally Ordered Broadcast Protocol][zab] - Benjamin Reed,
  Flavio P. Junqueira - 2008
* [Paxos Made Live - An Engineering Perspective][paxoslive] -
    Tushar Deepak Chandra, Robert Griesemer, Joshua Redstone - 2007
* [Paxos Made Simple][paxossimple] - Leslie Lamport - 2001
* [Impossibility of Distributed Consensus with One Faulty Process][flp]
  - Michael Fischer, Nancy Lynch, Michael Patterson - 1985
* [The Byzantine Generals Problem][generals] - Leslie Lamport - 1982

#

## Consistency

Types of consistency, and practical solutions to solving ensuring atomic
operations across a set of replicas.

* [HAT, not CAP: Highly Available Transactions][hat] - Peter Bailis,
  Alan Fekete, Ali Ghodsi, Joseph M. Hellerstein, Ion Stoica - 2013
* [Brewer's Conjecture and the Feasibility of Consistent, Available,
  Partition-Tolerant Web Services][cap] - Seth Gilbert, Nancy Lynch -
  2002
* [Harvest, Yield, and Scalable Tolerant Systems][harvest] - Armando
  Fox, Eric A. Brewer - 1999
* [Linearizability: A Correctness Condition for Concurrent
  Objects][linearizability] - Maurice P. Herlihy, Jeannette M. Wing -
  1990
* [Time, Clocks, and the Ordering of Events in a Distributed
  System][clocks] - Leslie Lamport - 1978

#

## Conflict-free data structures

Studies on data structures which do not require coordination to ensure
convergence to the correct value.

* [A Comprehensive Study of Convergent and Commutative Replicated Data
    Types][crdt1] - Mark Shapiro, Nuno Preguiça, Carlos Baquero, Marek
    Zawirski - 2011
* [A Commutative Replicated Data Type For Cooperative Editing][treedoc]
  - Nuno Preguica, Joan Manuel Marques, Marc Shapiro, Mihai Letia, 2009
* [CRDTs: Consistency Without Concurrency Control][crdt2] - Mihai Letia,
  Nuno Preguiça, Marc Shapiro - 2009

#

## Distributed programming

Languages aimed towards disorderly distributed programming as well as
case studies on problems in distributed programming.

* [Logic and Lattices for Distributed Programming][blooml] - Neil
  Conway, William Marczak, Peter Alvaro, Joseph M. Hellerstein, David
  Maier - 2012
* [Dedalus: Datalog in Time and Space][dedalus] - Peter Alvaro, William
  R. Marczak, Neil Conway, Joseph M. Hellerstein, David Maier, Russell
  Sears - 2011
* [MapReduce: Simplified Data Processing on Large Clusters][mapreduce] -
  Jeffrey Dean, Sanjay Ghemawat - 2004
* [A Note On Distributed Computing][computing] - Samuel C. Kendall, Jim
  Waldo, Ann Wollrath, Geoff Wyant - 1994

#

## Systems

Implemented and theoretical distributed systems.

* [A History Of The Virtual Synchrony Replication Model][synchrony] -
  Ken Birman - 2010
* [Cassandra — A Decentralized Structured Storage System][cassandra] -
    Avinash Lakshman, Prashant Malik - 2009
* [Dynamo: Amazon’s Highly Available Key-Value Store][dynamo] -
    Giuseppe DeCandia, Deniz Hastorun, Madan Jampani, Gunavardhan
    Kakulapati, Avinash Lakshman, Alex Pilchin, Swaminathan
    Sivasubramanian, Peter Vosshall and Werner Vogels - 2007
* [Stasis: Flexible Transactional Storage][stasis] - Russell Sears, Eric
  Brewer - 2006
* [The Process Group Approach to Reliable Distributed Computing][isis] -
  Ken Birman - 1993

#

I'm hoping to make this into a living document, so please submit [pull
requests][pull] or leave comments!

[pull]: https://github.com/cmeiklejohn/cmeiklejohn.github.io
[redbook]: http://www.amazon.com/Readings-Database-Systems-Joseph-Hellerstein/dp/0262693143
[raft]: https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf
[paxoslive]: http://research.google.com/pubs/pub33002.html
[dynamo]: http://www.read.seas.harvard.edu/~kohler/class/cs239-w08/decandia07dynamo.pdf
[crdt1]: http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf
[hat]: http://arxiv.org/pdf/1302.0309.pdf
[linearizability]: http://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf
[paxossimple]: http://www.cs.utexas.edu/users/lorenzo/corsi/cs380d/past/03F/notes/paxos-simple.pdf
[generals]: http://www.cs.cornell.edu/courses/cs614/2004sp/papers/lsp82.pdf
[flp]: http://macs.citadel.edu/rudolphg/csci604/ImpossibilityofConsensus.pdf
[treedoc]: http://hal.inria.fr/docs/00/44/59/75/PDF/icdcs09-treedoc.pdf
[zab]: http://www.research.yahoo.com/pub/3274
[computing]: http://dl.acm.org/citation.cfm?id=974938
[blooml]: http://db.cs.berkeley.edu/papers/UCB-lattice-tr.pdf
[dedalus]: http://db.cs.berkeley.edu/papers/datalog2011-dedalus.pdf
[clocks]: http://www.stanford.edu/class/cs240/readings/lamport.pdf
[harvest]: http://lab.mscs.mu.edu/Dist2012/lectures/HarvestYield.pdf
[crdt2]: http://hal.archives-ouvertes.fr/docs/00/39/79/81/PDF/RR-6956.pdf
[mapreduce]: http://research.google.com/archive/mapreduce.html
[cassandra]: http://www.cs.cornell.edu/projects/ladis2009/papers/lakshman-ladis2009.pdf
[synchrony]: http://www.cs.cornell.edu/ken/History.pdf
[stasis]: http://www.cs.berkeley.edu/~sears/publications/Stasis-OSDI.pdf
[isis]: http://www.cs.cornell.edu/projects/spinglass/public_pdfs/Process%20Group%20Approach.pdf
[cap]: http://dl.acm.org/citation.cfm?id=564601
