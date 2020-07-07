---
layout: post
title:  "Riak PG: Pruning terminated processes"
date:   2013-06-27 19:39:45 -0700
categories: riak erlang crdt
---

_In my previous [post][blog], I discussed the implementation of a distributed
process group registry, called [Riak PG][riakpg].  This post continues
that thread and discusses the problem of process monitors across
partitions._

As previously discussed, Riak PG stores copies of the process group on a
number of replicas within a cluster of Erlang nodes.  However, this
increases the complexity of pruning that process list when those
processes are terminated.  More specifically, processes running on one
side of a partition have all of their replicas located on the opposite
side of a partition, so process monitors are useless in determining when
that process should be removed from the process list.

One strategy for resolving this is to involve an extra node in the
coordination of requests, an additional replica running where that
process is running.  This vnode can then monitor the local process for
changes in that particular processes state, and update it's local
replica accordingly.  However, this solution is problematic if this node
completely fails.  If the node completely fails, not only does that
replica become unavailable, but due to it's replica being unavailable,
that update never propogates.

One alternative strategy is to prune the process listing during the read
repair process based only on known information about terminated
processes.

First, we modify the coordinating finite state machine to, once replicas
have been merged, prune _known terminated processes on available nodes_.

For instance, the modification to the read-repair process:

{% highlight erlang %}
%% @doc Perform read repair.
finalize(timeout, #state{replies=Replies}=State) ->
    Merged = merge(Replies),
    Pruned = prune(Merged),
    ok = repair(Replies, State#state{pids=Pruned}),
    {stop, normal, State}.
{% endhighlight %}

First, we determine if a process identifier should be pruned or not, and
call to the observed-removed set to remove it.  We use an arbitrary
atom, `none`, because the value isn't used in the implementation of the
set.

{% highlight erlang %}
%% @doc Based on connected nodes, prune out processes that no longer
%%      exist.
prune(Set) ->
    Pids0 = riak_dt_orset:value(Set),
    lists:foldl(fun(Pid, Pids) ->
                case prune_pid(Pid) of
                    true ->
                        riak_dt_orset:update({remove, none, Pid}, Pids);
                    false ->
                        Pids
                end
        end, Set, Pids0).
{% endhighlight %}

To determine if processes should be pruned, we only prune when the node
hosting the process id is available, but the process is no longer alive.

{% highlight erlang %}
%% @doc If the node is connected, and the process is not alive, prune
%%      it.
prune_pid(Pid) when is_pid(Pid) ->
    lists:member(node(Pid), nodes()) andalso
      (is_process_alive(Pid) =:= false).
{% endhighlight %}

Qed.

[blog]: http://christophermeiklejohn.com/erlang/riak/crdt/2013/06/24/introducing-riak-pg-distributed-process-groups-for-erlang.html
[riakpg]: https://github.com/cmeiklejohn/riak_pg
