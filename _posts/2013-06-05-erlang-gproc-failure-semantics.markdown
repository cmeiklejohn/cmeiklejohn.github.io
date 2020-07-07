---
layout: post
title:  "Erlang gproc Failure Semantics"
date:   2013-06-05 17:12:16
categories: erlang
---

In my previous post, we discussed [the failure semantics of
`pg2`][previous] during partitions and when dealing with dynamic cluster
management.  In this post, we're going to look at a common alternative
to `pg2`, the extended process registry, [`gproc`][gproc].

# So, what is `gproc`?

`gproc`, as described by Ulf Wiger in his [2007 paper][epr], outlines an
extended process registry for Erlang which provides a number of benefits
over the default process registry.  It also provides a global mode,
implemented by leveraging `gen_leader`, a leader election algorithm
based roughly on a [2005 paper][leader] and original implementation by
Thomas Arts and Hans Svensson.

Briefly, some of the notable improvements include:

* Any term can be a process alias
* Processes can have multiple aliases (also non-unique aliases supported)
* Counters, aggregated counters
* Global distribution of the process registry
* Thoroughly tested locally with Erlang QuickCheck

We're specifically going to look at emulating a `pg2`-like interface
with `gproc`, by tagging groups of processes with a name, and querying
the `gproc` to get the list of processes.

Let's explore the API using just the local distribution model of
`gproc`.

First, register the current process and ask for the registered processes
under that name.

{% highlight erlang %}
(riak_pg1@127.0.0.1)1> gproc:reg({p, l, group}).
true
(riak_pg1@127.0.0.1)2> gproc:lookup_pids({p, l, group}).
[<0.501.0>]
{% endhighlight %}

To explain the registration key: `group` is a term alias for the
process, `p` represents a non-unique property, so we can tag multiple
processes, and `l` means register locally.

# Some application changes to support `gproc`.

We start off by adding a dependency on `gproc` to the test application
we used last time to test pg.  We also need to add `gen_leader` as a
dependency to support the global distribution of the registry.

We'll use the current recommended version of `gen_leader` by `gproc`,
which is known to have [multiple][multiple] [problems][problems] during
[netsplits][netsplits].

{% highlight erlang %}
{gproc, ".*", 
 {git, "git://github.com/uwiger/gproc.git", "master"}},
{gen_leader, ".*",
 {git, "https://github.com/garret-smith/gen_leader_revival.git", "master"}}
{% endhighlight %}

We'll also modify the `vm.args` file to enable global distribution.

{% highlight erlang %}
-gproc gproc_dist all
{% endhighlight %}

Done.

# First things first...

It's important to note that nodes _must_ be connected prior to
starting the `gproc` application (which is ultimately responsible for
starting `gen_leader` with a candidates list.)

Let's see this in action.  Starting with our same `riak_core` cluster of
three nodes, we'll start the first two nodes and inspect the `gproc`
state.

Again, to explain the registration key: `group` is a term alias for the
process, `p` represents a non-unique property, so we can tag multiple
processes, and `g` means register globally.

{% highlight erlang %}
(riak_pg1@127.0.0.1)1> gproc:reg({p, g, group}).
true
(riak_pg1@127.0.0.1)2> gproc_dist:get_leader().
'riak_pg1@127.0.0.1'
(riak_pg1@127.0.0.1)4> nodes().
['riak_pg2@127.0.0.1']
(riak_pg1@127.0.0.1)5> gproc:lookup_pids({p, g, group}).
[<0.489.0>]
{% endhighlight %}

{% highlight erlang %}
(riak_pg2@127.0.0.1)3> gproc:reg({p, g, group}).
true
(riak_pg2@127.0.0.1)4> nodes().
['riak_pg1@127.0.0.1']
(riak_pg2@127.0.0.1)5> gproc_dist:get_leader().
'riak_pg2@127.0.0.1'
(riak_pg2@127.0.0.1)6> gproc:lookup_pids({p, g, group}).
[<0.996.0>]
{% endhighlight %}

As the `gproc` application was started prior to the nodes being
connected, they each are their own leader with their own state, which is
not replicated.

Now, let's restart the `gproc` application once we know for sure the
nodes are connected.

{% highlight erlang %}
(riak_pg1@127.0.0.1)7> nodes().
['riak_pg2@127.0.0.1']
(riak_pg1@127.0.0.1)8> application:start(gproc).
ok
22:59:20.357 [info] Application gproc started on node 'riak_pg1@127.0.0.1'
(riak_pg1@127.0.0.1)9> gproc:reg({p, g, group}).
true
(riak_pg1@127.0.0.1)10> self().
<0.489.0>
(riak_pg1@127.0.0.1)11>
{% endhighlight %}

{% highlight erlang %}
(riak_pg2@127.0.0.1)8> application:start(gproc).
ok
22:59:33.919 [info] Application gproc started on node 'riak_pg2@127.0.0.1'
(riak_pg2@127.0.0.1)9> gproc:lookup_pids({p, g, group}).
[<12417.489.0>]
(riak_pg2@127.0.0.1)10>
{% endhighlight %}

# So, what happens when a node becomes unreachable?

First, let's make the first node, which has the registered process,
unreachable.

{% highlight erlang %}
(riak_pg2@127.0.0.1)10> gproc:lookup_pids({p, g, group}).
[<12417.489.0>]
(riak_pg2@127.0.0.1)11> 
23:08:28.289 [error] ** Node 'riak_pg1@127.0.0.1' not responding **
** Removing (timedout) connection **
(riak_pg2@127.0.0.1)11> gproc:lookup_pids({p, g, group}).
[<12417.489.0>]
(riak_pg2@127.0.0.1)12> gproc:lookup_pids({p, g, group}).
[]
{% endhighlight %}

The process is available for a short period, but gets removed shortly
after the net tick time period times out the connection.

When the partition heals, we see the process return.

{% highlight erlang %}
(riak_pg2@127.0.0.1)26> gproc:lookup_pids({p, g, group}).
[<12417.489.0>]
{% endhighlight %}

# What happens when an addition happens during a partition?

{% highlight erlang %}
(riak_pg1@127.0.0.1)20> gproc:lookup_pids({p, g, group}).
[<0.10439.0>]
{% endhighlight %}

{% highlight erlang %}
23:23:19.325 [error] ** Node 'riak_pg1@127.0.0.1' not responding **
** Removing (timedout) connection **
(riak_pg2@127.0.0.1)38> gproc:lookup_pids({p, g, group}).
[]
(riak_pg2@127.0.0.1)39> gproc:reg({p, g, group}).
true
(riak_pg2@127.0.0.1)40> gproc:lookup_pids({p, g, group}).
[<0.996.0>]
{% endhighlight %}

And, when we heal that partition.

{% highlight erlang %}
(riak_pg2@127.0.0.1)41> gproc:lookup_pids({p, g, group}).
[<0.996.0>,<12417.10439.0>]
{% endhighlight %}

{% highlight erlang %}
(riak_pg1@127.0.0.1)21> gproc:lookup_pids({p, g, group}).
[<12417.996.0>,<0.10439.0>]
{% endhighlight %}

# So, what happens when we add a node with an unresponsive leader?

First, identify the leader.

{% highlight erlang %}
(riak_pg1@127.0.0.1)33> gproc:lookup_pids({p, g, group}).
[<0.13370.0>,<12417.14190.0>]
(riak_pg1@127.0.0.1)34> gproc_dist:get_leader().
'riak_pg1@127.0.0.1'
{% endhighlight %}

Now, take the leader down.

{% highlight erlang %}
(riak_pg2@127.0.0.1)49> gproc:lookup_pids({p, g, group}).
[<12417.13370.0>,<0.14190.0>]
(riak_pg2@127.0.0.1)50> gproc_dist:get_leader().
'riak_pg1@127.0.0.1'
(riak_pg2@127.0.0.1)51> 
23:52:04.443 [error] ** Node 'riak_pg1@127.0.0.1' not responding **
** Removing (timedout) connection **
(riak_pg2@127.0.0.1)51> gproc_dist:get_leader().
'riak_pg2@127.0.0.1'
(riak_pg2@127.0.0.1)52> gproc:lookup_pids({p, g, group}).
[<0.14190.0>]
(riak_pg2@127.0.0.1)53>
{% endhighlight %}

Then, join the third node.

{% highlight erlang %}
(riak_pg3@127.0.0.1)3> gproc_dist:get_leader().
** exception exit:
{timeout,{gen_leader,local_call,[gproc_dist,get_leader]}}
in function  gen_leader:call/2 (src/gen_leader.erl, line 326)
(riak_pg3@127.0.0.1)4> gproc:lookup_pids({p, g, group}).
[]
{% endhighlight %}

Finally, restore the original leader.

{% highlight erlang %}
(riak_pg2@127.0.0.1)53> gproc:lookup_pids({p, g, group}).
[<12417.13370.0>,<0.14190.0>]
(riak_pg2@127.0.0.1)54> gproc_dist:get_leader().
'riak_pg1@127.0.0.1'
(riak_pg2@127.0.0.1)55>
{% endhighlight %}

Now, let's check in on the third node. 

{% highlight erlang %}
riak_pg3@127.0.0.1)6> nodes().
['riak_pg2@127.0.0.1','riak_pg1@127.0.0.1']
(riak_pg3@127.0.0.1)7> gproc_dist:get_leader().
** exception exit:
{timeout,{gen_leader,local_call,[gproc_dist,get_leader]}}
in function  gen_leader:call/2 (src/gen_leader.erl, line 326)
(riak_pg3@127.0.0.1)8>
{% endhighlight %}

We see that it's fully connected, however the `get_leader` call
continues to fail with timeouts until we restart `gproc` on the first
two nodes.  In fact, if we restart `gproc` on all three nodes _too
quickly_, simulating small partitions during the restart period, we run
into the same situation again, with nodes being unable to determine who
the leader is due to [deadlock].  Some of these issues are documented in
Ulf's [2007 paper][epr] in the future work section as well.

# Things get worse...

Let's register a unique name on the primary node instead of the
non-unique property types.

{% highlight erlang %}
(riak_pg1@127.0.0.1)5> gproc:reg({n, g, riak_pg1}).
true
(riak_pg1@127.0.0.1)6> gproc:lookup_pids({n, g, riak_pg1}).
[<0.513.0>]
(riak_pg1@127.0.0.1)7> gproc:lookup_pids({n, g, riak_pg1}).
[<0.513.0>]
{% endhighlight %}

{% highlight erlang %}
(riak_pg2@127.0.0.1)4> gproc:lookup_pids({n, g, riak_pg1}).
[<12457.513.0>]
(riak_pg2@127.0.0.1)5>
{% endhighlight %}

Now, let's partition the network.

{% highlight erlang %}
(riak_pg1@127.0.0.1)8> 
14:46:19.963 [error] ** Node 'riak_pg2@127.0.0.1' not responding **
** Removing (timedout) connection **
14:46:19.963 [error] ** Node 'riak_pg3@127.0.0.1' not responding **
** Removing (timedout) connection **
{% endhighlight %}

And, as soon as we heal the partition:

{% highlight erlang %}
(riak_pg1@127.0.0.1)8> gproc:lookup_pids({n, g, riak_pg1}).
[<0.513.0>]
(riak_pg1@127.0.0.1)9> gproc:lookup_pids({n, g, riak_pg1}).
[]
{% endhighlight %}

What?  What happened to the registered name?  Did the process die?

{% highlight erlang %}
(riak_pg1@127.0.0.1)10> self().
<0.513.0>
(riak_pg1@127.0.0.1)11>
{% endhighlight %}

Nope.  

{% highlight erlang %}
(riak_pg1@127.0.0.1)12> gproc:lookup_pids({n, g, riak_pg1}).
[<0.513.0>]
{% endhighlight %}

But, it eventually returns.  This is the behaviour when the leader
becomes unavailable and there is a period of time where there is no
leader elected.

Permanent data loss is also considered a possibility with [some of the
data structures][loss], which is also mentioned in the [paper][epr].
It's important to note that I couldn't trigger this type of behaviour
with the non-unique property types.

# Counters

I didn't review counters, single, shared, or aggregated for data loss,
as I'm saving that topic for a future blog post.

# Conclusion

While `gproc` has been tested very thoroughly using utilities like
Erlang QuickCheck, its reliance on `gen_leader` is problematic.  Given
numerous forks and implementations of `gen_leader` trying to address the
data loss and netsplit issues, none have been proven to operate
correctly and guarantee termination.  Above, using a few small simulated
tests across a maximum cluster size of three nodes, we've observed data
loss, failure to handle dynamic membership, and timeout situtations due
to deadlock and failed leader election.

In conclusion, `gproc` seems to operate extremely well when used in a
local environment, but any use of it in a distributed system, where you
have dynamic cluster management and network partitions, appears to be
non-deterministic due to its reliance on `gen_leader`.

Feedback encouraged!

_Thanks to OJ Reeves and Heinz N. Gies for providing valuable feedback
on this blog post._

_View this story on [Hacker News][hn]._

[epr]:       http://dl.acm.org/citation.cfm?id=1292520.1292522&coll=DL&dl=GUIDE&CFID=205201430&CFTOKEN=69071722
[gproc]:     https://github.com/uwiger/gproc
[previous]:  http://christophermeiklejohn.com/erlang/2013/06/03/erlang-pg2-failure-semantics.html
[dynamic]:   http://erlang.org/pipermail/erlang-questions/2011-August/060370.html
[global]:    http://erlang.org/pipermail/erlang-questions/2011-December/063132.html
[netsplits]: https://github.com/uwiger/gproc/issues/19
[multiple]:  https://github.com/uwiger/gproc/issues/38
[problems]:  https://github.com/uwiger/gproc/issues/30
[leader]:    http://dl.acm.org/citation.cfm?id=1088368
[deadlock]:  http://erlang.org/pipermail/erlang-questions/2012-July/067761.html
[loss]:      http://erlang.org/pipermail/erlang-questions/2012-July/067749.html
[hn]:        https://news.ycombinator.com/item?id=5876188
