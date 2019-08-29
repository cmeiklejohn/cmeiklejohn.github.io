---
layout: post
title:  "Mixed-Cluster Versioning: A Story of Capabilities, Erlang and Riak"
date:   2019-08-29 00:00:00 -0000
categories: riak erlang
---

Dealing with multiple versions of the same software deployed in a distributed system is challenging: nowadays, we think of it as schema evolution, versioned APIs, and IDLs.  Let's talk about the challenges from the Erlang perspective.

## Erlang

Erlang is a dynamically typed, concurrent functional programming language.  Programs in Erlang are written using actors: lightweight processes which do not share memory and communicate only using asynchronous message passing.  Erlang supports *hot code loading*, where the code run by actors can be altered without bringing the system down.  Erlang allows two versions of any given module in a library to be active at the same time.  This feature was critical for Ericsson, the creator of the programming language, due to the high-availability requirements of telephony applications.

Let's explain hot code loading in terms of Ericsson's use case: a phone switch.  In the phone switch, each phone call is an actor: in Erlang's terms, this is a process.  Processes are extremely lightweight and a single machine can support hundreds of thousands of actors on a single machine.  Processes communicate with one another by sending and receiving messages.  Processes execute code: in Erlang, this code is typically a loop that waits to receive a message, processes that message and potentially sends more, and repeats this process.  When the application developer wants to deploy a new version of the software, the developer can create a new version of a module and install that new version into the system.  As two different versions of a module can be loaded at the same time, each process can be running one of the possible two versions of the code.  With both versions loaded, new processes will run the new version of the code and existing processes will upgrade at their next loop iteration, if written recursively.  Finally, when no more processes are running the old code, the old version will be automatically uninstalled.  Therefore, hot code loading assumes that all callers to the module being replaced have to use an API (e.g., exported functions) that does not change between versions and only send messages that each version can support.

Nowadays, Erlang is also distributed out-of-the-box: different instances of the Erlang virtual machine, running on either the same node or different nodes, can be clustered together.  When nodes are clustered together, Erlang provides transparent messaging between actors located on different nodes.  Not only can each node use hot code loading itself, but different nodes can be running different versions of the application.  This is both a blessing and a curse for real systems.

*An aside: since messages in Erlang can contain closures, this is even more dangerous.  Closures are sent by sending a reference to the module that the closure is defined in, along with bound variables.  Closures are named and referred to by a monotonically increasing integer that represents the count of closures in the actual file.  Therefore, references to closures might refer to code that isn't available on the remote node.*

Therefore, when writing distributed Erlang with hot code loading, the developer needs to think and reason about multiple versions of the same software project and all of their possible interactions. 

## Capabilities

When working on Riak at Basho, we needed a way to handle clusters of Riak that ran mixed versions.   Riak, in particular, was designed to run in mixed version clusters for up to two releases back.  This was very important for many of our enterprise customers: we had one customer in particular that would only upgrade a single node a day, for safety, across a 50 node Riak cluster.   Due to the speed of Riak releases on our 1.4 bugfix branch, some customers also had three versions deployed in a single cluster at one time: during a rolling upgrade they were already starting the next upgrade before finishing the previous upgrade.

To achieve this, we set out a policy on how upgrades in the code itself should be performed.  First, we made sure that modules would always send versioned messages (e.g., a tagged tuple containing the module name, version number, and message.)  This ensured that we would only be *increasing the version number monotonically* and *only adding new types of messages* and never removing code for handling existing messages.  This required quite a bit of dilligence on the programmers behalf to remember this procedure.  This was important because the system has to be able to handle the following scenarios; two of these scenarios are the case only when the cluster is running a single version, those are presented in italics.

* *v1 nodes sending v1 messages to v1 nodes.*
* v1 nodes sending v1 messages to v2 nodes.
* *v2 nodes sending v2 messages to v2 nodes.*
* v2 nodes sending v2 messages to v1 nodes.
* v2 nodes sending v1 messages to v1 nodes.

Now, this worked for introducing new message types into an existing system.  But, it's not all that simple.  What happens if a v2 message is sent to a v1 node?  Well, if the v1 has a receive clause that matches a wildcard message, it will drop it on the floor; but, what if the code is not written to discard messages that it isn't waiting for?  Ideally, we need to prevent v2 nodes from sending v2 messages to v1 nodes.

Similarly, what if we try to send a message to a process that doesn't exist yet?  If we introduce new functionality in v2 that doesn't exist in v1 we can't expect that functionality to be globally available yet.  In Riak, this came up in a number of ways --- adding new metrics, new storage backends, new anti-entropy mechanisms. 

To achieve this, we used a cluster capability system in Riak.  Each node would broadcast a set of capabilities that it knew about: these were represented as atom's in Erlang, which are similar to Ruby's symbols.  These capabilities at each node would be broadcast using Riak's gossip mechanism and each node would receive capabilities from all of the other nodes.  With the capabilities forming a lattice (e.g., set lattice with union as the least-upper-bound), each node could determine the capabilities that *all nodes knew about*, which is effectively computing the greatest lower bound of the node's individual capabilities.  Therefore, each node would only send messages and communicate using features that all nodes knew about.  For evolving an individual modules from v1 to v2, we would use individual capabilities that the module would register with the system and we would let the capability system do the rest.  Once all of the nodes knew about v2, the position in the lattice would advance and the system would move from talking using the v1 messages to the v2 messages (but, really would be using the v2 superset of v1 features.)

This feature was used for powering all of Riak's mixed cluster upgrade mechanisms and enabled us to introduce new features that would only be available once all nodes were upgraded, such as: bucket types, security, metrics and observability.

## Open Challenges

Is this stuff easy to use?  Well, sure, if you've got the entire story in your head and you're aware of precisely how the upgrade has to happen. 

In 2013, when I was starting my graduate student career as a full-time Basho employee and part-time student, I shipped some code that introduced new message types for our administritive control panel inside of Riak.  Unaware of the proper way to do this, I didn't use a new message type, I didn't version the messages correctly and instead altered their payload in place.  I shipped this code as part of a release of Riak and left for the Oregon Programming Languages Summer School.  

While at the summer school at the end of the first week, during a lecture from Frank Pfenning that my soon-to-be advisor Heather Miller was also attending, I received a phone call: I had crashed a 30 node production cluster at a customer in California.  I need to spend the entire second week of the summer school writing a fix to revert my changes, introduce the new messages as new message types, write a hotfix for all Riak versions affected, and get it shipped immediately.  In fact, this problem bothered me so much that on a business trip to Stockholm, I visited Uppsala University to talk with the authors of Dialyzer, a message passing and type analysis tool for Erlang, on how we could extend the tool to consider code running across different versions.  This stuff isn't easy.  It's tricky, and we do not have proper tooling for developing, testing, and simulating these scenarios.  

Now, what about APIs?  Well, APIs get us a lot of the way there.  In reality, very few people are using distributed programming languages to build their applications and are instead building self-contained microservices in more traditional programming languages.  These microservices are then composed together using other microservies through the use of APIs that communicate by either publish-subscribe or remote procedure call.  Developers typically use versioned APIs to facilitate this.  But, APIs are only starting to appear now that advertise their capabilities, and typically an external service discovery (e.g., service mesh, etc.) or configuration service (e.g., Zookeeper) to coordinate changes around which capabilities should be active at a given time.  Most of these applications are typically developed in isolation as individual components, which helps reinforce the importance of proper API design and evolution.

I think there's a lot of open challenges and interesting work in this area.  Here's just a small list of some of the things I've been thinking about:

* What sort of developer tooling (e.g., editor, compiler) can assist the developer in ensuring that they properly version messages and introduce new capabilities accordingly?
* How can we design development environments that allow developers to test and experiment with operating their code in mixed version environments?