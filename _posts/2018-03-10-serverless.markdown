---
layout: post
title:  "Necessity is the Mother of Invention: stateful experiments on AWS Lambda"
date:   2018-03-10 09:00:00 -0000
categories: lasp
group: syncfree
---

In January 2017, we presented results to the European Commission on a FP7
project called SyncFree, where we were tasked with a large-scale evaluation
of the Lasp programming system. In this evaluation, we spent about 9,000+ EUR
running a large-scale experiment on Amazon demonstrating that our runtime
system and programming environment could scale to a 1,024 node, fully
replicated, CRDT data store, using an application scenario from Rovio
Entertainment. We've written up the results in a paper at PPDP '17, that is
available [online](https://arxiv.org/abs/1708.06423).

Running this experiment was clearly expensive: multiple runs over clusters of
100+ machines in Amazon's Cloud Computing environment, using Apache Mesos to
further subdivide the machine, where we ran a container per simulated "node"
running an instance of the Lasp database. To achieve this scale, we wrote a
new distribution layer for the Erlang runtime system, called Partisan that's
already had some adoption in industry and has [demonstrated higher
scalability and order-of-magnitude improvements in
performance](https://arxiv.org/abs/1802.02652).

However, I want to get to 4,000 nodes this year. Next year, I want 10,000
nodes. So, I set out to figure out how to run academic experiments much
cheaper, because I'm a graduate student with very limited funding.

My idea was to try to figure out how I could leverage cheaper computing
services, like Amazon Lambda and the serverless movement, for running
short-lived geo-distributed application scenarios.

# Erlang on Lambda: Exlam

To start with, I forked Jesse Schoch's AWS elixir-on-lambda project
[Exlam](https://github.com/jschoch) to build upon.

The way exlam works is the following:

An AWS Lambda compatible Node.JS 4.3 project wraps an installation of the
Erlang Virtual Machine and a relx-generated release of an Erlang / Elixir
project. This is bundled into a zip file and deployed to AWS Lambda using the
Lambda command line interface.

On deployment, or initial launch of the Lambda container, the file is
unzipped into a temporary directory, and the the Node.JS application invoked.
This Node.JS project then uses child_process to launch the Erlang virtual
machine and issue a request to the container through a HTTP interface exposed
by the Elixir application, once launched.

# Exlam Modifications

To get exlam working for a basic deployment of an Erlang application, I had
to make a few modifications.

First, if the BEAM is launched with Distributed Erlang enabled, the BEAM
tries to launch an instance of the Erlang Port Mapper Daemon, which fails to
bind the socket with EACCESS, and crashes the BEAM. To work around this, I
had to create a custom inialization script that didn't enable Distributed
Erlang, by avoiding use of the `-name` or `-sname` parameters.

The next problem I ran into was that the AWS containers that are used for
Lambda are running an older version of Amazon Linux with a version of OpenSSL
that is no longer available via the Amazon RPM repository. This prevented the
crypto library from loading, so I attempted to build a version of the BEAM
with a statically linked OpenSSL. I ran into problems with this as well,
because the BEAM's static linking option for OpenSSL requires that you
install files into particular paths -- not just any path will work -- which I
discovered after two days of recompiling and redeployment.

Finally, I found a more elegant option: I wrote a Dockerfile for exlam that
uses the AWS Amazon Linux image version used by Lambda, and build and bundle
my application there before deployment into the Amazon environment.

# Distributed Erlang

Now, without Distributed Erlang, how can my nodes communicate with one
another?

As part of our work on scaling Distributed Erlang, I've built a replacement
system named Partisan that can support different cluster topologies and
different network transports for allowing actor messaging between Erlang
nodes. Out of the box, Partisan supports a variety of different transport
mechanisms (libp2p, gen_tcp) and different network topologies (client-server,
unstructured overlays with partial views, spanning trees, etc.) I decided
that I could use Partisan to provide my transport for message passing between
nodes.

However, when running in the AWS Lambda environment, processes do not have
the ability to bind ports, and so I needed another transport for message
passing between the various instances that were running to enable them to
communicate with one another. To faciliate this, I wrote a AMQP backend for
Partisan that allows me to have each Lambda instance connect to a message
broker and use thse outgoing TCP connections for bidirectional communication
between each of the nodes in the system. As each node comes online, it
registers a locally generated name into a membership data structure, stored
as a CRDT, that is gossiped between all peers in the system.

Now, as my Lambda instances scale out, nodes will come online, register
themselves into the membership, gossip the membership around to other nodes
in the system, providing cluster membership visibility to all other nodes in
the system.

# Lasp

Now, Lasp is a peer-to-peer, fully replicated CRDT database.

First, how can I get updates into the system? To get updates into the system,
I can use the AWS Lambda invocation API to submit a HTTP event into exlam,
which then is proxied by Node.JS into the Erlang application that's exposing
a HTTP interface for get/put. That's easy.

But, how do I deal with anti-entropy? For those of you unfamiliar with how
modern eventually consistency distributed databses work, anti-entropy is a
background process that runs to ensure that new, or stale, replicas are kept
up to date. If my instances are stopped after each invocation, and my
containers eventually killed, how can my system ensure that stopped replicas
are kept up to date.

To do this was slightly complicated and problematic. At each Lambda
invocation I have the system take all of the local updates, dispatch them to
the message broker for the other nodes in the system, as determined by the
membership, then sleep the difference of 30 seconds minus the propagation
time, to ensure the container stays up for 30 seconds. On receipt of these
messages, nodes merge those changes into their local state, keeping the state
of the database alive through transient function invocations.

The final piece of the puzzle: ensuring that I invoke Lambdas at a rate that
keeps at least one instance per region and availability zone in Lambda to
keep my database alive. When I want more nodes, I can scale up the Lambda
invocations, which launch more containers that join the membership, and scale
my geo-replicated database.

# Problems

But, many problems still remain.  I'll point out a couple of the obvious:

* My state is only alive as long as an instance with the state is.  With a large enough working data set, the anti-entropy session between nodes won't be able to complete within the time that the container is alive.
* Failure detection is interesting here: how do you know if a node is alive or not -- given that all nodes will be paused at a particular point, and eventually terminated due to inactivity.  Therefore, it's important to have a system that can adapt to high-churn.

# Why?

When building experiments for highly-scalable geo-distributed systems, things
get expensive quickly. Our PPDP '17 paper argues that this is the reason that
many of these systems are not evaluated at scale -- it's both difficult and
expensive to run these experiments.

Could I just have used DynamoDB and EC2 instances?

Sure, but things get expensive quickly. Managing individual servers is
difficult, and when you start using spot instances, the transient nature of
these instances can make it difficult to draw conclusions. Systems like
Kubernetes make these experiments easier to control, but as the cost
increases -- in fact, most of our latest experiments have been done in Google
Cloud Platform using Container Engine to ease the difficulty of running
experiments.

One notable thing that came out of this Rube Goldberg machine is that you can
get extremely far running infrastructure on the free tier alone, and that if
you're clever enough, you can bend Lambda to do some very interesting things.