---
layout: post
title:  "Programming Models, Part 6: Leaderboard"
date:   2015-10-17 20:00:00 -0300
categories: lasp erlang
group:  SyncFree
---

_As discussed previously, [Lasp][lasp] is the name of our declarative
programming model for eventually consistent computations._

## Introduction

Previously, we've discussed the design of an [eventually consistent,
advertisement counter](/lasp/erlang/2015/01/10/ad-counter-orset.html)
using our declarative, eventually consistent programming model for edge
computation called [Lasp][lasp].  Over the past few weeks, I've been
working towards building out additional use cases based on our
[partnership with Rovio Entertainment](https://syncfree.lip6.fr/) as
part of the SyncFree research project.  Today, I'm going to look at the
design of a eventually consistent top-K leaderboard application.

If you don't know much about Lasp, I suggest watching this
[video](https://www.youtube.com/watch?v=lsKaNDj4TrE) before proceeding
with this article.

## System Model

For our system model, we're going to assume that as a provider of mobile
games, we have a large number of clients that will be playing games
offline.  Each client can uniquely identify itself, and will locally
maintain a periodically synchronized leaderboard of top scores for each
game.  Clients will be able to locally modify their leaderboard while
they are offline, and synchronization will occur when they have
connectivity (or as specified by the client.)

We're going to look into how we can build this application with Lasp.
(To follow along, the full code is available on
[GitHub](https://github.com/lasp-lang/lasp/blob/master/riak_test/lasp_leaderboard_test.erl).)

## Top-K CRDT

We introduce the `lasp_top_k_var` data type, that's heaviy inspired by
the work from Navalho et al. in ["A Study of CRDTs that do
Computations"](http://dl.acm.org/citation.cfm?id=2745948).

{% highlight erlang %}
%% Create a leaderboard datatype.
{ok, LeaderboardId} = lasp:declare(lasp_top_k_var),
{% endhighlight %}

The [design of this
CRDT](https://github.com/lasp-lang/lasp/blob/master/src/lasp_top_k_var.erl)
ensures that update operations and merge operations preserve the top-K
entries by value, arbitrating on lexicographical order of keys.  For
this application, we assume 1 for the value of K.  (Keep in mind, this
implemention is just a prototype to explore the design space, and should
not be used in production!)

## Initialize client processes

We begin by initializing a series of client processes.  Each client is
initialized with four pieces of data:

* The process identifier of the simulator, so the clients can report to
  the simulator about what actions they are performing.
* A globally unique identifier for each client, as some CRDTs require
  that clients can be uniquely identified.  We assume the top-K
  leaderboard uses this identifier as the name of the client.
* An identifier for the Lasp runtime to know how to talk to the
  canonical version of the leaderboard.
* An initial copy of the leaderboard.

{% highlight erlang %}
%% @doc Launch a series of client processes.
clients(Runner, LeaderboardId, Leaderboard) ->
    SpawnFun = fun(Id) -> spawn_link(?MODULE, client, [Runner, Id, LeaderboardId, Leaderboard]) end,
    lists:map(SpawnFun, lists:seq(1, ?NUM_CLIENTS)).
{% endhighlight %}

Each client is modeled as an Erlang process that recursively processes
incoming messages until it receives a terminate message and subsequently
shuts down.  This process is responsible for handling two types of
messages and periodically synchronizing their state back to the server.

In a practical setting, you would probably want to synchronize state as
long as connectivity was available, and only perform periodic
synchronization when connectivity or battery power was limited.

The simulator will be responsible for periodically sending messages to
the client saying that as game has been completed; when the simulation
completes, a terminate message will be sent to the client, which will
cause the client to perform a final synchronization of state with the
server.

Periodically, as represented with the after clause, the client will
synchronize state back to the server, even if state has not changed.
This could be performed to only synchronize if state has changed; but
we've kept it simple for the example.

{% highlight erlang %}
%% @doc Client process.
client(Runner, Id, LeaderboardId, Leaderboard0) ->
    receive
        {complete_game, Score} ->
            io:format("Client ~p completed game with score ~p.", [Id, Score]),

            %% Update local leaderboard.
            {ok, Leaderboard} = lasp_top_k_var:update({set, Id, Score}, Id, Leaderboard0),

            %% Notify the harness that an event has been processed.
            Runner ! {event, Score},

            client(Runner, Id, LeaderboardId, Leaderboard);
        terminate ->
            io:format("Client ~p shutting down, issuing final synchronization.", [Id]),

            %% Synchronize copy of leaderboard.
            {ok, {_, _, _, Leaderboard}} = lasp:bind(LeaderboardId, Leaderboard0),

            ok
    after
        10 ->
            io:format("Client ~p synchronizing leaderboard.", [Id]),

            %% Synchronize copy of leaderboard.
            {ok, {_, _, _, Leaderboard}} = lasp:bind(LeaderboardId, Leaderboard0),

            client(Runner, Id, LeaderboardId, Leaderboard)
    end.
{% endhighlight %}

Each client locally updates its copy of the leaderboard using the API
provided by the top-K CRDT: the clients identifer is used as their name
in the leaderboard and the leaderboard is updated with the score from
the completed game.

## Simulator

Our simulator is pretty straightforward.  Given a list of clients, the
simulator picks a random client, generates a random score for a game
that's been simulated, and sends this score to the client and sleeps.
We sleep between each game simulation to allow interleaving of the
periodic state synchronization with the server with recording the
results of completed games.

{% highlight erlang %}
%% @doc Simulate clients.
simulate(_Runner, ClientList) ->
    %% Start the simulation.
    ViewerFun = fun(_) ->
            %% Pick a random client.
            Random = random:uniform(length(ClientList)),
            Pid = lists:nth(Random, ClientList),

            %% Sleep to simulate game run time.
            timer:sleep(5),

            %% This simulates a game being completed on a clients device.
            Pid ! {complete_game, random:uniform(100000000)}
    end,
    lists:foreach(ViewerFun, lists:seq(1, ?NUM_EVENTS)).
{% endhighlight %}

When clients synchronize their state back, this `bind` operation ensures
that the value stored at the data center is "merged" with the incoming
value, and the result of the merge is returned to the user.  This serves
to get the latest version of the leaderboard, and disseminate data
through the server to other clients, as they periodically perform their
merge operations.

{% highlight erlang %}
%% Synchronize copy of leaderboard.
{ok, {_, _, _, Leaderboard}} = lasp:bind(LeaderboardId, Leaderboard0),
{% endhighlight %}

Additionally, we want our simulation to block until all events have been
processed.

{% highlight erlang %}
%% @doc Wait for all events to be delivered in the system.
wait_for_events(Count, NumEvents, MaxValue0) ->
    receive
        {event, Score} ->
            MaxValue = max(Score, MaxValue0),
            case Count >= NumEvents of
                true ->
                    io:format("~p events served, max is: ~p!", [Count, MaxValue]),
                    MaxValue;
                false ->
                    wait_for_events(Count + 1, NumEvents, MaxValue)
            end
    end.
{% endhighlight %}

To execute our simulation, we do the following.

{% highlight erlang %}
test() ->
    Self = self(),

    %% Create a leaderboard datatype.
    {ok, LeaderboardId} = lasp:declare(lasp_top_k_var),

    %% Read the leaderboard's current value.
    {ok, {_, _, _, Leaderboard}} = lasp:read(LeaderboardId, undefined),

    %% Launch client processes.
    ClientList = clients(Self, LeaderboardId, Leaderboard),

    %% Initialize simulation.
    simulate(Self, ClientList),

    %% Wait until we receive num events.
    FinalResult = wait_for_events(1, ?NUM_EVENTS, 0),

    %% Terminate all clients.
    terminate(ClientList),

    %% Read the result and print it.
    {ok, {_, _, _, FinalLeaderboard}} = lasp:read(LeaderboardId, undefined),
    Final = orddict:to_list(lasp_top_k_var:value(FinalLeaderboard)),

    %% Assert we got the right score.
    [{_, FinalResult}] = Final,

    io:format("Final Leaderboard: ~p", [Final]),

    ok.
{% endhighlight %}

We launch our clients, initialize the simulation, ensure we wait for
all events to be processed and finally assert that the value, when
the system reaches quiescence, is correct!

## Code Mesh 2015

If you enjoyed this article, I'll be speaking on coordination-free
designs for mobile gaming at [Code Mesh on November 3,
2015!](http://www.codemesh.io/)

[lasp]: https://lasp-lang.org/
