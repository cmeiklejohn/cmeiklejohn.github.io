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
series of categories, each highlighting a progression of related ideas
over time focused in a specific area of research within the field.

Keeping the tradition of the Red Book, I've included both papers which
resulted in very successful systems and/or techniques, as well as papers
which introduced a concept which was either immediately dismissed or
proven incorrect.  This emphasizes the progression of ideas which lead
to the development of these systems.

## Consensus

The problems of establishing consensus in a distributed system.

* [In Search of an Understandable Consensus Algorithm][raft]
  <span class="author">Diego Ongaro, John Ousterhout</span>
  <span class="date">2013</span>
* [A Simple Totally Ordered Broadcast Protocol][zab]
  <span class="author">Benjamin Reed, Flavio P. Junqueira</span>
  <span class="date">2008</span>
* [Paxos Made Live - An Engineering Perspective][paxoslive]
  <span class="author">Tushar Deepak Chandra, Robert Griesemer, Joshua Redstone</span>
  <span class="date">2007</span>
* [The Chubby Lock Service for Loosely-Coupled Distributed Systems][chubby]
  <span class="author">Mike Burrows</span>
  <span class="date">2006</span>
* [Paxos Made Simple][paxossimple]
  <span class="author">Leslie Lamport</span>
  <span class="date">2001</span>
* [Impossibility of Distributed Consensus with One Faulty Process][flp]
  <span class="author">Michael Fischer, Nancy Lynch, Michael Patterson</span>
  <span class="date">1985</span>
* [The Byzantine Generals Problem][generals]
  <span class="author">Leslie Lamport</span>
  <span class="date">1982</span>

## Consistency

Types of consistency, and practical solutions to solving ensuring atomic
operations across a set of replicas.

* [Brewer's Conjecture and the Feasibility of Consistent, Available, Partition-Tolerant Web Services][cap]
  <span class="author">Seth Gilbert, Nancy Lynch</span>
  <span class="date">2002</span>
* [Harvest, Yield, and Scalable Tolerant Systems][harvest]
  <span class="author">Armando Fox, Eric A. Brewer</span>
  <span class="date">1999</span>
* [Linearizability: A Correctness Condition for Concurrent Objects][linearizability]
  <span class="author">Maurice P. Herlihy, Jeannette M. Wing</span>
  <span class="date">1990</span>
* [Time, Clocks, and the Ordering of Events in a Distributed System][clocks]
  <span class="author">Leslie Lamport</span>
  <span class="date">1978</span>
* [Highly Available Transactions: Virtues and Limitations][hat]
  <span class="author">Peter Bailis, Aaron Davidson, Alan Fekete, Ali Ghodsi, Joseph M. Hellerstein, Ion Stoica</span>
  <span class="date">2013</span>

## Conflict-free data structures

Studies on data structures which do not require coordination to ensure
convergence to the correct value.

* [A Comprehensive Study of Convergent and Commutative Replicated Data Types][crdt1]
  <span class="author">Mark Shapiro, Nuno Preguiça, Carlos Baquero, Marek Zawirski</span>
  <span class="date">2011</span>
* [A Commutative Replicated Data Type For Cooperative Editing][treedoc]
  <span class="author">Nuno Preguica, Joan Manuel Marques, Marc Shapiro,
  Mihai Letia</span>
  <span class="date">2009</span>
* [CRDTs: Consistency Without Concurrency Control][crdt2]
  <span class="author">Mihai Letia, Nuno Preguiça, Marc Shapiro</span>
  <span class="date">2009</span>

## Distributed programming

Languages aimed towards disorderly distributed programming as well as
case studies on problems in distributed programming.

* [Logic and Lattices for Distributed Programming][blooml]
  <span class="author">Neil Conway, William Marczak, Peter Alvaro,
  Joseph M. Hellerstein, David Maier</span>
  <span class="date">2012</span>
* [Dedalus: Datalog in Time and Space][dedalus]
  <span class="author">Peter Alvaro, William R. Marczak, Neil Conway,
  Joseph M. Hellerstein, David Maier, Russell Sears</span>
  <span class="date">2011</span>
* [MapReduce: Simplified Data Processing on Large Clusters][mapreduce]
  <span class="author">Jeffrey Dean, Sanjay Ghemawat</span>
  <span class="date">2004</span>
* [A Note On Distributed Computing][computing]
  <span class="author">Samuel C. Kendall, Jim Waldo, Ann Wollrath, Geoff
  Wyant</span>
  <span class="date">1994</span>

## Systems

Implemented and theoretical distributed systems.

* [A History Of The Virtual Synchrony Replication Model][synchrony]
  <span class="author">Ken Birman</span>
  <span class="date">2010</span>
* [Cassandra — A Decentralized Structured Storage System][cassandra]
  <span class="author">Avinash Lakshman, Prashant Malik</span>
  <span class="date">2009</span>
* [Dynamo: Amazon’s Highly Available Key-Value Store][dynamo]
  <span class="author">Giuseppe DeCandia, Deniz Hastorun, Madan Jampani,
  Gunavardhan Kakulapati, Avinash Lakshman, Alex Pilchin, Swaminathan
  Sivasubramanian, Peter Vosshall and Werner Vogels</span>
  <span class="date">2007</span>
* [Stasis: Flexible Transactional Storage][stasis]
  <span class="author">Russell Sears, Eric Brewer</span>
  <span class="date">2006</span>
* [Bigtable: A Distributed Storage System for Structured Data][bigtable]
  <span class="author">Fay Chang, Jeffrey Dean, Sanjay Ghemawat, Wilson
  C. Hsieh, Deborah A. Wallach, Mike Burrows, Tushar Chandra, Andrew
  Fikes, and Robert E. Gruber</span>
  <span class="date">2006</span>
* [The Google File System][gfs]
  <span class="author">Sanjay Ghemawat, Howard Gobioff, and Shun-Tak Leung</span>
  <span class="date">2003</span>
* [The Process Group Approach to Reliable Distributed Computing][isis]
  <span class="author">Ken Birman</span>
  <span class="date">1993</span>

## Books

Overviews and details covering many of the above papers and concepts compiled into single resources.

* [Distributed Systems: for fun and profit][distsys_fun-profit]
  <span class="author">Mikito Takada</span>
  <span class="date">2013</span>

---

I'm hoping to make this into a living document, so please submit [pull
requests][pull] or leave comments!

[pull]: https://github.com/cmeiklejohn/cmeiklejohn.github.io
[redbook]: http://www.amazon.com/Readings-Database-Systems-Joseph-Hellerstein/dp/0262693143
[raft]: https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf
[paxoslive]: http://research.google.com/pubs/pub33002.html
[dynamo]: http://www.read.seas.harvard.edu/~kohler/class/cs239-w08/decandia07dynamo.pdf
[crdt1]: http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf
[hat]: http://www.bailis.org/papers/hat-vldb2014.pdf
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
[stasis]: http://dl.acm.org/citation.cfm?id=1298459
[isis]: http://www.cs.cornell.edu/projects/spinglass/public_pdfs/Process%20Group%20Approach.pdf
[cap]: http://dl.acm.org/citation.cfm?id=564601
[bigtable]: http://research.google.com/archive/bigtable-osdi06.pdf
[chubby]: http://research.google.com/archive/chubby-osdi06.pdf
[gfs]: http://research.google.com/archive/gfs.html
[distsys_fun-profit]: http://book.mixu.net/distsys/
