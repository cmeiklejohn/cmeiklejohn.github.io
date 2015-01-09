---
layout: post
title:  "Programming Models, Part 1: Try Derflow!"
date:   2014-09-28 00:26:21 -0000
categories: derflow erlang
group:  SyncFree
---

_Updated January 9th, 2015: Derflow has since been renamed to Lasp,
which is open source on [GitHub][lasp].  For more information
surrounding the name change, see this [post][name]._

One of the main goals of the [SyncFree][SyncFree] project is to build a
programming model for working with eventual consistency and
conflict-free replicated data types.  We've been well on our way to that
goal, influenced heavily by Peter Van Roy's [work on dataflow
programming][CTMCP], by the [Bloom][Bloom] work at the University of
California, Berkeley, as well as the [LVars][LVars] work at Indiana University.

In this post, we aim to give a high level overview of our inspirations
for the work, the general progress, links to videos, publications, and
then provide instructions about how to build the language if you wish to
experiment with it.  Future blog posts will dig into the implementation
details of the programming model.

# Derflow, A New Hope

Our initial prototype, which was put together in May, focused on a
distributed deterministic dataflow model in Erlang -- think of it as a
distributed [IVars] build on top of Riak Core (or equally, something
similar to [Ozma][Ozma]).

The core of this model relies on a distributed single-assignment
variable store, which is replicated using Dynamo-style quorum-based
replication.  Each of the variables of the program are hashed and
distributed to a set of nodes, ensuring fault-tolerance.

This version of our prototype was documented in our [Erlang Workshop '14
paper][EW14] and presented this year in Goteborg, Sweden.

There are also a couple talks available as well, which discuss this
implementation as well: one from the [Erlang User Conference
'14][EUC14Video] and one from [Erlang Workshop '14][EW14Video].

# Derflow, The Lattice Strikes Back

Our second work, which is due to be presented at [LADIS Workshop
'14][LADIS14] in October, expands the model to operate over bounded
join-semilattices, while providing a threshold read operation on these
variables to ensure determinism.  If you know the [LVars][LVars] work,
this should sound very familiar.

In addition, we explore an alternative approach to executing programs --
if the programs are guaranteed to be deterministic, why not replicate
the entire programs themselves?  In this model, we explore providing an
API where entire programs are registered with the cluster, executed in a
fault-tolerant manner with the results returned to the user.  More to
come on this in a future post.

# Join us!

So, how can you play along at home?

First, we will assume you've got Erlang installed.  Unfortunately, given
some dependencies of `riak_core`, the latest supported version of Erlang
is currently R16B02.

First, clone and build `derflow`:

{% highlight sh %}
$ git clone git@github.com:cmeiklejohn/derflow.git
$ cd derflow
$ make
{% endhighlight %}

Now, you can build a packaged release or development release, in the
same way you would for Riak.  If you're looking to develop with the
language, I'd recommend building a `stagedevrel`, which builds 6
independent releases, all sharing the same compiled application code to
ease in cluster deployment and testing.

{% highlight sh %}
$ make stagedevrel
{% endhighlight %}

For building sample applications, we've built a test harness using
`riak_test` which executes applications and verifies they have the
correct result.  This harness compiles a local derflow program, creates
a cluster of nodes to execute it on, and remotely deploys the program to
the cluster and returns the result.

To configure `riak_test`, do the following:

{% highlight sh %}
$ cd ..
$ git clone git@github.com:basho/riak_test.git
$ cd riak_test
$ make
{% endhighlight %}

Once you have done that, make sure you copy the included
`riak_test.config` file to `~/.riak_test.config` and update the paths
referenced in the file.

Running the tests is pretty simple:

{% highlight sh %}
$ cd ../derflow
$ make riak-test
{% endhighlight %}

In theory, all of the tests should pass.  If not, well, research is
research.

# In Conclusion

This article serves to provide a general overview of the work, and
pointers to all of the relevant resources if you're interested in
contributing, assisting, or playing around with our prototype language.  

Next in the series, we'll begin to dig into how each of the different
models in Derflow is implemented as well as the various tradeoffs
between them.

Please let me know if you run into any problems playing around with our
prototype.  I'd love to hear your feedback.

[EUC14Video]: https://www.youtube.com/watch?v=vk0-48qacqw
[EW14Video]: https://www.youtube.com/watch?v=camJBbIw6m8
[SyncFree]: https://syncfree.lip6.fr
[EW14]: http://dl.acm.org/citation.cfm?id=2633451
[LADIS14]: http://ladisworkshop.org/node/10
[Bloom]: http://www.bloom-lang.net
[LVars]: http://dl.acm.org/citation.cfm?id=2502326
[Ozma]: http://dl.acm.org/citation.cfm?id=2489841
[IVars]: http://hackage.haskell.org/package/monad-par-0.3.4.4/docs/Control-Monad-Par.html
[CTMCP]: http://www.info.ucl.ac.be/~pvr/book.html
[name]: /erlang/lasp/2014/12/21/lasp.html
[lasp]: https://github.com/cmeiklejohn/lasp
