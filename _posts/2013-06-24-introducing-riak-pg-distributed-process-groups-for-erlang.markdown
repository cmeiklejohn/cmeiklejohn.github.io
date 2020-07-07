---
layout: post
title:  "Introducing Riak PG: Distributed process groups for Erlang"
date:   2013-06-24 14:14:53 -0700
categories: erlang riak crdt
---

_This paper has been accepted and will be presented at [ACM SIGPLAN
Erlang Workshop 13][sigplan]._

[Riak PG][riak_pg] is prototype implementation of distributed process
groups built on top of [Riak Core][riak_core].  Its main goals are to
provide high availability and deterministic conflict resolution in the
event of network partitions.  In providing these guarantees, [Riak
PG][riak_pg] provides an "eventually consistent" view of process groups
for applications which can tolerate this behaviour.

[Riak PG][riak_pg] is not meant for production use, but more as an
experiment in trying to model high availability process groups in Erlang
without the use of coordinated global ETS transactions or consensus.

_This work was mainly motivated by observed failures in both
[gproc][gproc] and [pg2][pg2].  For more detailed information, see these
previous posts on [gproc][gprocblog] and [pg2][pg2blog] failure
semantics._

# Design

Similar to the [Riak][riak_kv] data store, data is replicated a number
of times throughout a cluster of nodes using the familiar concepts of
consistent hashing and virtual nodes.  Both retrieval and modification
of process groups are handled by finite state machines, which write
updates to a series of virtual nodes responsible for handling each
group.  We leverage conflict-free replicated data types to ensure that
updates to the process group always commute, which guarantees
convergence amongst all replicas in the event of partial writes or
network partitions.

# Interface

For compatibility, [Riak PG][riak_pg] provides the same interface as the
built-in Erlang [pg2][pg2] library.  In addition to providing calls to
return members of the process group, [Riak PG][riak_pg] extends this
interface to provide calls to return only processes from the process
group which are located on the calling node, or any connected node.  To
illustrate:

To return all members:

{% highlight erlang %}
%% @doc Return a listing of members of a particular group.
-spec members(term()) -> {ok, list(pid())} | {error, timeout}.
members(Group) ->
    {ok, ReqId} = riak_pg_members_fsm:members(Group),
    wait_for_reqid(ReqId, ?TIMEOUT).
{% endhighlight %}

To return only local members:

{% highlight erlang %}
%% @doc Return a listing of local members of a particular group.
-spec local_members(term()) -> {ok, list(pid())} | {error, timeout}.
local_members(Group) ->
    {ok, ReqId} = riak_pg_members_fsm:members(Group),
    case wait_for_reqid(ReqId, ?TIMEOUT) of
        {ok, Members} ->
            LocalMembers = lists:filter(fun(Pid) ->
                            node(Pid) =:= node() end, Members),
            {ok, LocalMembers};
        {error, Error} ->
            {error, Error}
    end.
{% endhighlight %}

To return only connected members:

{% highlight erlang %}
%% @doc Return a listing of connected members of a particular group.
-spec connected_members(term()) -> {ok, list(pid())} | {error, timeout}.
connected_members(Group) ->
    {ok, ReqId} = riak_pg_members_fsm:members(Group),
    case wait_for_reqid(ReqId, ?TIMEOUT) of
        {ok, Members} ->
            ConnectedMembers = lists:filter(fun(Pid) ->
                            lists:member(node(Pid), nodes()) end, Members),
            {ok, ConnectedMembers};
        {error, Error} ->
            {error, Error}
    end.
{% endhighlight %}

# Conflict resolution

Conflict resolution is handled by using a conflict-free replicated data
type, specifically an observed-removed set as described by Baquero and
Shapiro in [A comprehensive study of Convergent and Commutative
Replicated Data Types][crdt].  A dictionary is used to track a series of
these sets, one representing each process group.  When partitions occur,
additions and removals of processes to groups are guaranteed to converge
to the correct value.

Here's an example of read repair, resolving values which have diverged.

{% highlight erlang %}
%% @doc Perform merge of replicas.
merge(Replies) ->
    lists:foldl(fun({_, Pids}, Acc) ->
        riak_dt_orset:merge(Pids, Acc) end, riak_dt_orset:new(), Replies).
{% endhighlight %}

[Riak PG][riak_pg] leverages an Erlang implementation of these data
types, called [Riak DT][riak_dt] that's being developed by [Sean
Cribbs][seancribbs], [Russell Brown][russelldb], and [Sam
Elliott][lenary] of Basho.

# Garbage collection of the set

Garbage collection of the observed-removed set is definitely
problematic, as its growth is unbounded.  One such approach for dealing
with garbage collection from accumulating tombstones or repeated
additions and deletions of the same value is outlined in Baquero and
Shapiro's work in [An optimized conflict-free replicated
set][optimized].  Riak PG currently does not implement a garbage
collection strategy for the process groups.

## Process monitors

Using process monitors to handle the removal of process identifiers from
groups as they are terminated is also an area that's problematic in the
event of network partitions.  In the case of processes that have joined
to groups which don't contain replicas for that group, and are
potentially located across a partition, how do we effectively monitor
and ensure we remove the process from the group when it terminates?
Riak PG currently does not provide a solution for this, and defers to
application code for handling of the retrieval of terminated processes.

# Conclusion

While [Riak PG][riak_pg] provides a higher availability process group
registry, lack of garbage collection of the data structures is very
problematic when dealing with a large number of processes.  This is
especially problematic in Erlang because of its use of lightweight
processes for modeling computations and handling concurrency.

The code for Riak PG is available on [GitHub][riak_pg].

_View this story on [Hacker News][hn]._

[hn]: https://news.ycombinator.com/item?id=5942370
[riak_kv]: https://github.com/basho/riak_kv
[gproc]: https://github.com/uwiger/gproc
[pg2]: http://erlang.org/doc/man/pg2.html
[riak_core]: https://github.com/basho/riak_core
[riak_pg]: http://github.com/cmeiklejohn/riak_pg
[seancribbs]: https://twitter.com/seancribbs
[russelldb]: https://twitter.com/russelldb
[riak_dt]: https://github.com/basho/riak_dt
[lenary]: https://twitter.com/lenary
[gprocblog]: http://christophermeiklejohn.com/erlang/2013/06/05/erlang-gproc-failure-semantics.html
[pg2blog]: http://christophermeiklejohn.com/erlang/2013/06/03/erlang-pg2-failure-semantics.html
[optimized]: http://arxiv.org/abs/1210.3368
[crdt]: http://hal.inria.fr/inria-00555588/
[sigplan]: http://www.erlang.org/workshop/2013/
