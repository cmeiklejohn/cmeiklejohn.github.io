---
layout: post
title:  "Weekend Report: Erlang's Process Identifiers and My Weekend Bug Hunting"
date:   2021-01-14 00:00:00 -0000
categories: erlang partisan lasp
group: Partisan
---

Here's a fun bug I dealt with this weekend.  

When building [Partisan](http://github.com/lasp-lang/partisan), our high-performance replacement for Distributed Erlang, we knew that we would need a way to easily convert applications written using Distributed Erlang to Partisan, which had a slightly different API as it was implemented in user space instead of as a VM feature that is supported by custom syntax.  

## Processes

One of the first challenges we had was dealing with process identifiers contained in messages sent between different nodes.

Process identifiers, when using Distributed Erlang, are tricky business.  

* Process identifiers are relative.  For example, a process identifier `0.25.0` is a process on the local machine; we know this because the first position in the process id is a 0.  This refers to process 25 on the local node.  

* If I send this local process identifier `0.25.0` in a message to another machine, it will be received at the other machine as `X.25.0`; in this case, the X will be the position in the connection list of the node that the identifier came from.  This means that `0.25.0` on machine A and machine B refers to two different processes.  

* It gets even more complicated: if A sends a local process identifier to B and B then forwards to C and C isn't connected to A yet?  A will be connected to C in order to properly name the process identifier.  If this can't happen, the process will crash.

This "rewriting" of process identifiers happens during serialization/deserialization.  In fact, you can write a process identifier out into a binary file on disk, copy that file to another machine, and then deserialize it there, and as long as this process all works, you won't get a `badarg` when attempting to deserialize.  

This can get even more crazy: could node A write out `0.25.0` to disk and node B read from disk and have the process identifier reference the wrong process?  Yes, you can definitely do this.  

(You also can do this with the `pid(0,25,0)` operation, as well.)

## Partisan and Process Identifiers

Partisan supports alternative transport layers that are not based on Distributed Erlang.  For example, it allows you to run over AMQP, or any of our custom, highly-scalable, TCP transports.  This is a problem, because you cannot deserialize a process identifier for a node that isn't connected with Distributed Erlang. 

To solve this problem, we integrated a pid rewriting mechanism in Partisan that would automatically rewrite process identifiers into a format that conainted information about where the process was running and it's local identifier name there.  We then added an additional API into the system that would support messaging with these identifiers regardless of if the nodes were directly connected or whether or not the message needed to use a proxy node to deliver it.

## The Bug

My first Ph.D. work is a programming model for distributed programming, [Lasp](http://github.com/lasp-lang/lasp).  

Lasp contains a database inside that uses CRDTs for object storage and provides automatic synchronization.  Partisan is the transport layer that Lasp uses for synchronizing this database.   Each object in Lasp is identified by a key that is placed in local storage and it's associated value is a CRDT payload along with a vector clock that's used for fast object comparison to avoid running the expensive CRDT merge when receiving an object during syncronization that is known to be old.  

In Lasp, the API for interacting with CRDTs allows users to specify an operation to perform on the CRDT as well as the "actor"'s name, which is the unique identifier used by the node performing the update that is used in order to detect concurrent operations.  The vector clock is an Erlang dictionary, which is represented as an Erlang term containing some metadata and an embedded list of key-value pairs.

Now, the example application in the Lasp README instructs users to setup a two node cluster, perform two concurrent updates on each of the nodes, and then synchronize them, and read the resulting object to demonstrate how data synchronization works and CRDTs resolve conflicting updates.  You can run this program in a loop to see concurrent, conflicting, updates being applied and see them automatically resolve.

But, this example program instructs the developer to specify the actor as the local process identifier!  That's not going to be good.

## Example

Here's the problematic execution.  The data structure at the end of each line is list data structure that is close to, not exactly, the internal representation of the vector clock, minus the metadata.

First iteration.

* Node A, process `0.1.0` updates the CRDT which updates the payload and the vector clock. `[(0.1.0, 1)]`
* Node B, process `0.2.0` updates the CRDT which updates the payload and the vector clock. `[(0.2.0, 1)]`
* They synchronize, which rewrites the vector clock to remove the local process identifiers and the merges the CRDTs and the vector clocks.  This produces a vector clock of `[((A, 0.1.0), 1), ((B, 0.1.0), 1)]`

Second iteration.

* Node A updates again, updates the clock using it's local identifier.  `[((A, 0.1.0), 1), ((B, 0.1.0), 1),  (0.1.0, 1)]`
* B does the same. `[((A, 0.1.0), 1), ((B, 0.1.0), 1),  (0.2.0, 1)]`
* They synchronize and perform the rewrite, the vector clocks are merged. `[((A, 0.1.0), 1), ((B, 0.1.0), 1),  ((A, 0.1.0), 1), ((B, 0.2.0), 1)]`

Xth iteration.

* This repeats until the system runs out of memory because the vector clocks get too large and the Erlang node crashes.

Now, the user doesn't notice this memory problem, only that the clock appears to be the same.  But, the user never looks at the clock really, because it's an internal implementation used to speed up synchronization.  This is because, if you have a dictionary that has multiple entries for the same key, when you read the dictionary -- rather than look at it's internal representation -- you'll just see two values, `(A, 0.1.0)` points to `1` and `(B, 0.1.0)` points to `1`.

## The Fix

Therefore, the bugfix that was pushed in the latest version of Lasp prohibits the use of process identifiers or references in the vector clock or CRDT payload itself.  This is because you can actually break convergence if you happened to include a process identifier as part of the payload of a CRDT.  We didn't put this restriction on the actual data type itself, because that behavior is fine if you're using a CRDT in a normal Distributed Erlang cluster, it's only the combination with Partisan and Lasp where it becomes an issue.