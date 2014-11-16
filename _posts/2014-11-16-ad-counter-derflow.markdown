---
layout: post
title:  "SyncFree / Programming Models, Part 3: Ad Counter, Part 1"
date:   2014-11-16 15:20:10 -0500
categories: derflow erlang
---

_As we discussed in our first [post][part1], [Derflow][derflow] is the
name of our distributed deterministic programming model that is the
basis of our research into providing a more expressive way of working
with CRDTs and eventual consistency._

# Intro

One of the use cases we've focused our research on is around an
eventually consistent advertisement counter.  For example, consider you
are a rather large game company with millions of units deployed in the
field -- these devices can be online and offline at any point during the
day.

Each of these devices have a number of advertisements they can display
during the game -- each advertisement needs to be displayed a minimum
number of times. (read: advertisement impressions)

In our case, it's acceptable to display an advertisement more times than
the number of impressions that have been paid for, which allows us to
loosely coordinate the tracking of these counters.  This is important,
as we can not guarantee that clients will be online at particular times
to use coordination to make a change to the configuration -- such as
removing an advertisement once it's maximum display limit has been
reached.

In the design we will look at below, we define correct operation of this
application as never losing an advertisement impression, and eventually
converging to the correct number of impressions depending on our
divergence control strategy.

# The Advertisement Counter Example

Let's start by examining our application code.

To initialize our application, we begin by performing four main tasks:

* Create a counter for each advertisement
* Initialize a process for each client who will be viewing ads
* Initialize a process for each server who will be tracking ads
* Simulate a bunch of advertisements being viewed

## Createing advertisement counters

We model each advertisement counter as a grow-only counter (G-Counter).
We use the grow-only counter provided by the `riak_dt` library, the
`riak_dt_gcounter`.

Here's example code which creates five advertisement counters.

{% highlight erlang %}
lager:info("Initialize advertisement counters..."),
Generator = fun(_) ->
        lager:info("Generating advertisement!"),
        {ok, Id} = derflow:declare(riak_dt_gcounter),
        Id
end,
Ads = lists:map(Generator, lists:seq(1,5)),
{% endhighlight %}

This returns a list of five unique advertisement counters, which we will
use for tracking the number of impressions for each advertisement.

## Initializing clients

Following that, we initialize a series of clients, each of which is
given the list of advertisements they are responsible for displaying to
the user.  These clients represent `derflow` clients, running at the
client, near the end user.

{% highlight erlang %}
lager:info("Launching clients..."),
Launcher = fun(Id) ->
        ClientPid = spawn(?MODULE, client, [Id, Ads]),
        lager:info("Launched client; id: ~p pid: ~p~n", [Id, ClientPid]),
        ClientPid
end,
Clients = lists:map(Launcher, lists:seq(1,5)),
{% endhighlight %}

Each client process handles three things: returning the list of
active advertisements, viewing advertisements, and removing
advertisements.  We use a simple recurisve process which blocks on
receiving messages to perform each of these operations.

{% highlight erlang %}
%% @doc Client process; standard recurisve looping server.
client(Id, Ads) ->
    lager:info("Client ~p running; ads: ~p~n", [Id, Ads]),
    receive
        {active_ads, Pid} ->
            Pid ! Ads,
            client(Id, Ads);
        view_ad ->
            %% Choose an advertisement to display.
            %% Use hd() for simplicity of example.
            Ad = hd(Ads),
            lager:info("Displaying ad: ~p from client: ~p~n", [Ad, Id]),

            %% Update ad by incrementing value.
            {ok, Value, _} = derflow:read(Ad),
            {ok, Updated} = riak_dt_gcounter:update(increment, Id, Value),
            {ok, _} = derflow:bind(Ad, Updated),

            client(Id, Ads);
        {remove_ad, Ad} ->
            %% Remove ad.
            lager:info("Removing ad: ~p from client: ~p~n", [Ad, Id]),
            client(Id, Ads -- [Ad])
    end.
{% endhighlight %}

When a request to view an advertisement arrives, we choose an
advertisement to display (here, we are just choosing the first, but it
could be random -- we've also omitted the code to actually display the
advertisement on the screen, if you haden't noticed) and then we
increment the counter for this particular advertisement.

This `bind` operation succeeds because in this case, the value we are
pushing back to the constraint store is an inflation of the lattice; the
G-Counter is only ever going to grow.

## Initializing servers

Next, we initialize one server process per advertisement.  Here's what
that code looks like:

{% highlight erlang %}
lager:info("Launch a server for each advertisement..."),
Server = fun(Ad) ->
        ServerPid = spawn(?MODULE, server, [Ad, Clients]),
        lager:info("Launched server; ad: ~p~n", [Ad]),
        ServerPid
end,
_Servers = lists:map(Server, Ads),
{% endhighlight %}

Each of these server processes performs a threshold `read` against the
counter for the advertisement it's tracking; this threshold `read`
operation will block, thereby suspending execution of the server process
until the counter has reached at least 5.

{% highlight erlang %}
%% @doc Server functions for the advertisement counter.  After 5 views,
%       disable the advertisement.
server(Ad, Clients) ->
    lager:info("Server launched for ad: ~p", [Ad]),
    {ok, _, _} = derflow:read(Ad, 5),
    lager:info("Threshold reached; disable ad ~p for all clients!",
               [Ad]),
    lists:map(fun(Client) ->
                %% Tell clients to remove the advertisement.
                Client ! {remove_ad, Ad}
        end, Clients),
    io:format("Advertisement ~p reached display limit!", [Ad]).
{% endhighlight %}

Once the threshold has been reached, the server process will unblock and
notify all clients to stop displaying the advertisement.

## Simulating the requests.

Finally, some code to run the advertisement counter simulation.

{% highlight erlang %}
lager:info("Running advertisements..."),
Viewer = fun(_) ->
        Pid = lists:nth(random:uniform(5), Clients),
        io:format("Running advertisement for pid: ~p~n", [Pid]),
        Pid ! view_ad
end,
_ = lists:map(Viewer, lists:seq(1,50)),
{% endhighlight %}

In this example, we launch 50 requests to view a random sequence of
advertisements to exercise our code and verify the behavior is correct.

# Where do we go from here?

So far, we've assumed that clients are online, and that when we go to
update state in the constraint store, we will be able to contact it.
However, in a large-scale distributed system, especially when dealing
with a large amount of mobile clients, it is understood that that
mechanism will not be true.  Given this is active research, we don't
have all of the answers yet, but we're slowly working towards them.

One idea is to be able to automatically decompose these programs at the
point where we use shared conflict-free replicated data types between
clients and the server supporting greater divergence without sacrificing
correctness.

Let's look at an example below:

## Greater divergence, support offline operation

If we look closely at our client code, we see that each time an
advertisement is viewed, we update a shared counter.

{% highlight erlang %}
%% Update ad by incrementing value.
{ok, Value, _} = derflow:read(Ad),
{ok, Updated} = riak_dt_gcounter:update(increment, Id, Value),
{ok, _} = derflow:bind(Ad, Updated),
{% endhighlight %}

Each view triggers a counter to be incremented at the shared,
replicated, fault-tolerant, constraint store at the `derflow` cluster.
However, this isn't required if we're willing to allow for greater
divergence, which can lead to greater over-counting.

Once approach we can take, which still allows for correct operation with
greater divergence, is to use a second counter locally, and merge state
into the counter stored by the server periodically.

For instance, we increment a local counter stored at the client:

{% highlight erlang %}
{ok, Local} = riak_dt_gcounter:update(increment, Id, Local0),
{% endhighlight %}

Then, we periodically update upstream:

{% highlight erlang %}
{ok, Value, _} = derflow:read(Ad),
Merged = riak_dt_gcounter:merge(Local, Value),
{ok, _} = derflow:bind(Ad, Merged),
{% endhighlight %}

Of course, these mechanisms are easily exploited if coded explicitly
using CRDTs.  In our example above, the transformation is trivial given
we are operating over grow-only counters.  However, our goals remains to
integrate this at the programming model layer -- given that
transformations and composition of the more complex data types are not
as trivial and previous ad-hoc approaches have proven error prone.

Thanks for reading.

# Resources

The code for this example can be found on [GitHub][counter].

_If you're interested in this research and would like to discuss further
or assist, feel free to contact me using the details in the footer._

_For more information on [SyncFree][syncfree] and the use cases we have
focused our research on, I recommend [this talk][ricon] given by Annette
Bieniusa and myself at RICON 2014 in Las Vegas._

[part1]: http://christophermeiklejohn.com/derflow/erlang/2014/09/28/try-derflow.html
[derflow]: https://github.com/cmeiklejohn/derflow
[counter]: https://github.com/cmeiklejohn/derflow/blob/e6449b1d3b410bab284b95e478ed534dd8d204a3/riak_test/derflow_adcounter_test.erl
[ricon]: https://www.youtube.com/watch?v=1KP_pxFhlVU
[syncfree]: http://syncfree.lip6.fr
