---
layout: post
title:  "Distinguishing Lasp from Bloom<sup>L</sup>: Programming System vs. Programming Model"
date:   2018-03-02 09:00:00 -0000
categories: lasp
group: syncfree
---

This week, I participated in Dagstuhl Seminar 18091, ["Data Consistency in Distributed Systems: Algorithms, Programs, and Databases"](https://www.dagstuhl.de/en/program/calendar/semhp/?semnr=18091) where I presented some in development work on making it easier to program with both the Lasp and Bloom<sup>L</sup> distributed programming models.  During my presentation, I drew comparisons between Lasp and Bloom<sup>L</sup> systems, focusing mainly on the data types, operational semantics, and how the systems share similar designs.  However, while trying to convince the audience during a background section that both systems were similar, and therefore our techniques and formalisms applied to both systems, I ended up underselling my own system, mainly because I focused on the programming model alone, and not the system we've built around it.

Given that, I set out to describe both systems holistically, to understand the full set of differences between the two.

At the highest level, both systems share very similar designs.  Both Lasp and Bloom<sup>L</sup> are systems for writing coordination-free distributed programs.  Coordination-free, in this sense, means that for operations expressed in a coordination-free subset of their respective programming models, these applications can avoid the need for consensus -- no etcd, Zookeeper, etc. -- because these programs are designed in a manner where computations will not be affected by network reordering or message duplication.  Through the use of lattice programming, instantiated as ever-growing sets, or more useful data types like CRDTs, combined with monotone application logic, applications will be correct-by-construction, and anomaly free.

# Data Types and Combinators

Bloom<sup>L</sup> is a generalization of Bloom from sets to lattices.  Bloom<sup>L</sup> allows users to "bring their own" lattices, mutators, query methods, and combinators.  Data types must have values that form a lattice, and have a merge function that correctly computes the least-upper-bound.  Combinators in Bloom<sup>L</sup>, between different lattice types, must either be monotone functions or a special case of monotone functions, homomorphisms.   These functions must be labeled accordingly, and these labels are unchecked by the system and assumed to be correct.  When functions are homomorphic, Bloom<sup>L</sup> can incrementally maintain the program as the system evolves.  Bloom<sup>L</sup> is implemented in Ruby, and therefore can be used with existing Ruby applications.  Bloom<sup>L</sup> allows users to write programs where coordination is required, and uses an analysis technique to enforce these points of order with consensus. (However, I don't believe the implementation supports this automatically?)

Lasp provides non-monotonic query (value right now), monotonic query (give me the last value you read, and I'll give you the same or a newer value: session guarantees), and update over any user-defined lattice: users can bring their own lattices by implementing a data type interface in the Erlang programming language.  This interface requires that the user supply the values of the lattice, the ordering relationship, a merge function, and update methods that inflate the state.  Lasp comes with over 20 CRDT implementations, but combinators are only provided for one CRDT: the Observed-Remove Set.  Lasp's combinators include: map, filter, product, union, intersection, and fold.  Lasp's fold requires that the accumulator function be both associative, commutative, idempotent, and invertible, and these requirements are not checked by the system.  Lasp is implemented in Erlang, and therefore can be used with existing Erlang / Elixir applications.  Lasp only allows expression of coordination-free computations, and is meant to be embedded in applications where they can benefit from exploiting weak ordering for performance.

# The System

The system, however, is where Lasp is architecturally quite different from Bloom<sup>L</sup>.  

Bloom<sup>L</sup>'s programming model for communication is channel based -- if you know the name of a remote node, you can send that node a message by monotonically inserting something into the channel.  

In Lasp, every variable in the system, whether derived through a combinator or a value that is updated directly, points to a key in an underlying highly-available, fault-tolerant key-value store.  By default, this key-value store is fully replicated, but nodes can have object filters that allow them to omit certain groups of objects for replication.  Therefore, whenever a value changes in the system through use of the update command, every node that is replicating that object will observe that change.  Changes can be propagated in two ways: full state-based replication, where the entire lattice is shipped to all replicas; or, delta-based, where only change representations are sent. 

Lasp's communication layer is powered by Partisan, our system for supporting node-to-node communication in Erlang using a variety of network topologies.  Partisan provides point-to-point messaging using different network topologies: full mesh, client-server (where, clients communicate via a server instance), publish-subscribe (via RabbitMQ) or a partially connected overlay with spanning tree optimization to reduce redundancy in the network.  We've [demonstrated both scaling Lasp to over 1,000 nodes fully replicated](https://arxiv.org/abs/1708.06423), and shown [over and order of magnitude increase in performance](https://arxiv.org/abs/1802.02652) for the Riak Core distributed system, using Partisan.  We're hoping to hit 4,000 nodes this year.

Lasp also comes with deployment support, via a system called Sprinter, that supports cluster deployments on Apache Mesos, Kubernetes-based systems like Google Cloud Platform, and Docker Compose, making it easy to deploy and manage at scale.

Lasp isn't just a programming model, it's what I like to call a programming system for distributed applications, like systems like Argus were in the 80s.  We're actively growing the performance of the system, along with features, and language expressivity.  Stay tuned for the coming months, where we plan to release a number of new features.
