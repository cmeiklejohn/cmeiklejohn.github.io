---
layout: post
title:  "Programming Models, Part 5: Ad Counter, Part 2"
date:   2015-01-10 15:39:10 -0500
categories: lasp erlang
group:  SyncFree
---

_As we discussed in our first [post][part1], [Lasp][lasp] is the name of
our distributed deterministic programming model that is the basis of our
research into providing a more expressive way of working with CRDTs and
eventual consistency._

# Intro

This post is a continuation of our work on building an eventually
consistent advertisement counter using Lasp.  To get the most benefit
from this article, you should read the first [post][counter] in this
series.

In this post, we look at an alternative method for tracking the list of
active advertisements: a replicated data structure which supports an
arbitrary number of concurrent addition and removal operations to
elements in a set.  This data structure is called the Observed-Remove
set (OR-set), and was originally formalized by Shapiro et al. in ["A
Comprehensive Study of Convergent and Commutative Replicated Data
Types"][shapiro].

# Motivation

Using the observed-remove set for tracking the active advertisements is
beneficial for several reasons:

* Removals can be handled correctly without coordination, given the
  propagation delay is acceptable.
* Clients no longer need to maintain a list of advertisements
  themselves; clients can track just a reference to the set of active
  advertisements which is maintained by the variable store.
* Clients can periodically cache the active advertisements list based on
  a divergence control strategy of their chosing.
* When clients tracked their own list of active advertisements, they
  would have to wait for messages from the server about when to disable
  an advertisement.  Inevitably, these "disable" messages can (and
  demonstrated during evaluation of the previous example) be delayed by
  requests to view advertisements.  This leads to advertisements being
  displayed significantly more than they should.

# The Advertisement Counter Example

As before, we've broken our example application into four components:

* Create a counter for each advertisement
* Initialize a process for each client who will be viewing ads
* Initialize a process for each server who will be tracking ads
* Simulate a bunch of advertisements being viewed

## Creating advertisement counters

We do not alter our original approach of modeling each advertisement
counter as a grow-only counter (G-Counter).  However, instead of
tracking the advertisement counters in a normal Erlang list, we use a
observed-remove set, as shown below.

{% highlight erlang %}
%% Generate an OR-set for tracking advertisement counters.
{ok, Ads} = lasp:declare(riak_dt_orset),

%% Build an advertisement counter, and add it to the set.
lists:map(fun(_) ->
          {ok, Id} = lasp:declare(riak_dt_gcounter),
          {ok, _} = lasp:update(Ads, {add, Id}, undefined)
          end, lists:seq(1,5)),
{% endhighlight %}

We begin by declaring a new variable, of type `riak_dt_orset`, and then
for each advertisement we want to count impressions of, update the
observed-remove to include it.

## Initialize client processes

Again, we spawn "client" processes, which respond to requests to view
advertisements.

{% highlight erlang %}
%% Generate a OR-set for tracking clients.
{ok, Clients} = lasp:declare(riak_dt_orset),

%% Each client takes the full list of ads when it starts, and reads
%% from the variable store.
lists:map(fun(Id) ->
          ClientPid = spawn_link(?MODULE, client, [Id, Ads, undefined]),
          {ok, _} = lasp:update(Clients, {add, ClientPid}, undefined),
          end, lists:seq(1,5)),
{% endhighlight %}

Each of these clients only needs to track the identifier of the active
advertisement set, instead of the list of advertisements themselves.

{% highlight erlang %}
%% @doc Client process; standard recurisve looping server.
client(Id, Ads, PreviousValue) ->
    receive
        view_ad ->
            %% Get current ad list.
            {ok, {_, AdList0, _}} = lasp:read(Ads, PreviousValue),
            AdList = riak_dt_orset:value(AdList0),

            case length(AdList) of
                0 ->
                    %% No advertisements left to display; ignore
                    %% message.
                    client(Id, Ads, AdList0);
                _ ->
                    %% Select a random advertisement from the list of
                    %% active advertisements.
                    Ad = lists:nth(random:uniform(length(AdList)),
                                   AdList),

                    %% Increment it.
                    {ok, _} = lasp:update(Ad, increment, Id),
                    lager:info("Incremented ad counter: ~p", [Ad]),

                    client(Id, Ads, AdList0)
            end
    end.
{% endhighlight %}

Now, clients are no longer responsible for removing advertisements from
their list to display when requested by the "server" processes.  When a
request to "view" an advertisement arrives, each client process either
uses a locally cached copy of advertisements that are displayable, or
request from the variable store the current list of active
advertisements.

### Monotonic Reads

The read operation used here is what we are referring to as a _monotonic
read_.  A monotonic read operation takes an previously observed value
in the provided data type's lattice and blocks until the variable's
current value is an _inflation_ of the previous.

_For simplicity, think the greater-than-or-equal-to relationship over
natural numbers; we want to ensure we never view the value 1 if we have
already observed the value 2._

This behavior is __extremely important__: if our variable store is
replicated using an optimisic replication strategy, during failure
conditions we may read from a replica which contains an earlier value,
which would render our program incorrect.

In the case of our observed-remove set, the monotonic read operation
allows us ensure we always read values in causal order; we will never
read the empty set after reading a set with a value, unless that value
had been specifically removed (compared to the alternative case in
coordination-free cases, where you would observe and earlier value where
the value had not been added yet.)

## Initialize server processes

Just as with our previous example, we initialize one "server" process
per advertisement.

{% highlight erlang %}
%% Launch a server process for each advertisement, which will block
%% until the advertisement should be disabled.

%% Create a OR-set for the server list.
{ok, Servers} = lasp:declare(riak_dt_orset),

%% Get the current advertisement list.
{ok, {_, AdList0, _}} = lasp:read(Ads),
AdList = riak_dt_orset:value(AdList0),

%% For each advertisement, launch one server for tracking it's
%% impressions and wait to disable.
lists:foldl(fun(Ad, _Servers) ->
            ServerPid = spawn_link(?MODULE, server, [Ad, Ads]),
            {ok, _} = lasp:update(Servers, {add, ServerPid},
                                  undefined),
            Servers
            end, Servers, AdList),
{% endhighlight %}

However, we take a slightly different approach.  First, we iterate the
current list of advertisements spawning a process for each one.  When
spawning the process we provide the identifier to the list of
advertisements, and not the actual list of advertisements.

{% highlight erlang %}
%% @doc Server functions for the advertisement counter.  After 5 views,
%%      disable the advertisement.
%%
server(Ad, Ads) ->
    %% Blocking threshold read for 5 advertisement impressions.
    {ok, _} = lasp:read(Ad, 5),

    %% Remove the advertisement.
    {ok, _} = lasp:update(Ads, {remove, Ad}, Ad),

    lager:info("Removing ad: ~p", [Ad]).
{% endhighlight %}

Like before, we do a blocking _monotonic read_, which will not unblock
until the counter for the given advertisement reaches at least five.
Once the read unblocks, instead of sending a message to each client
notifying them to remove the ad, we modify the set directly by issuing
an update.

## Simulating the requests.

Finally, some code to run the advertisement counter simulation.

{% highlight erlang %}
%% Start the client simulation.

%% Get client list.
{ok, {_, ClientList0, _}} = lasp:read(Clients),
ClientList = riak_dt_orset:value(ClientList0),

Viewer = fun(_) ->
        Pid = lists:nth(random:uniform(5), ClientList),
        Pid ! view_ad
end,
lists:map(Viewer, lists:seq(1,100)),
{% endhighlight %}

In this example, we launch 100 requests to view a random sequence of
advertisements to exercise our code and verify the behavior is correct.

# Evaluation

Let's compare divergence with both approaches:

{% highlight erlang %}
Gathering totals...
<<110,53,13,91,199,31,77,58,182,249,60,178,202,211,89,243>> impressions: 21
<<168,68,91,50,208,71,65,25,150,112,253,136,180,119,19,155>> impressions: 20
<<204,96,55,64,234,28,72,69,135,52,240,26,111,45,53,188>> impressions: 20
<<45,233,202,246,10,43,69,227,143,26,191,102,16,172,96,69>> impressions: 20
<<212,97,51,81,130,124,78,25,166,119,199,202,168,187,115,66>> impressions: 19
{% endhighlight %}

With our original ad counter, because the advertisement removal messages
are interleaved with requests to view advertisements, we suffer a high
amount of divergence: most counters stop around 20, when they should
stop at 5.  This only gets worse when higher levels of concurrency are
introduced.

{% highlight erlang %}
Gathering totals...
<<208,159,75,41,9,151,70,107,182,210,180,66,247,132,211,14>> impressions: 5
<<195,43,172,108,73,61,67,190,140,7,109,243,209,246,5,162>> impressions: 5
<<50,181,254,178,117,171,66,174,153,199,160,145,159,98,225,150>> impressions: 5
<<29,35,74,153,33,176,66,193,189,68,180,224,73,78,95,53>> impressions: 10
<<152,192,71,228,61,171,77,132,137,223,207,108,112,142,160,168>> impressions: 5
{% endhighlight %}

Obviously, reading directly from the variable store each time cuts down
on divergence.  However, when dealing with offline applications, this
approach is not viable.

What's valuable in this approach compared to the original, is that
clients can cache the advertisements locally and choose when to
synchronize.  This alters the model to shift divergence control to the
client -- clients can update as connectivity is available, and diverge
during offline periods, instead of relying on the delivery of messages
from the server.

# Conclusion

In this post, we introduced a few new concepts:

* The _monotonic read_ operation, which ensures that reads are always
  advancing in a lattice defined by the data type of the object being
  read.  This ensures that we never read an earlier value and our
  programs evolve monotonically.
* Using the observed-remove set in Lasp to build richer applications.

Additionally, we alleviated the following problems in our previous
example:

* Clients no longer need to track all possible state they need to act
  on.
* Clients now control their own divergence: they choose how long to
  cache the active list of advertisements and when to refresh it,
  instead of relying on messages from the server.  We've shown in the
  evaluation that this method is much better in terms of divergence.

Thanks for reading.

# Resources

_If you're interested in this research and would like to discuss further
or assist, feel free to contact me using the details in the footer._

_For more information on [SyncFree][syncfree] and the use cases we have
focused our research on, I recommend [this talk][ricon] given by Annette
Bieniusa and myself at RICON 2014 in Las Vegas._

[part1]: http://christophermeiklejohn.com/derflow/erlang/2014/09/28/try-derflow.html
[derflow]: https://github.com/cmeiklejohn/derflow
[ricon]: https://www.youtube.com/watch?v=1KP_pxFhlVU
[syncfree]: http://syncfree.lip6.fr
[name]: /erlang/lasp/2014/12/21/lasp.html
[lasp]: https://github.com/cmeiklejohn/lasp
[counter]: /derflow/erlang/2014/11/16/ad-counter-derflow.html
[shapiro]: https://hal.inria.fr/inria-00555588/document
