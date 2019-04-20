---
layout: post
title:  "Fault-Injection with Demers' Direct Mail Protocol"
date:   2019-04-20 00:00:00 -0000
categories: erlang lasp
group: Partisan
---

*This is the first article in a series about building reliable fault-tolerant protocols with [Partisan](http://partisan.cloud), our high-performance, distributed runtime for the Erlang programming language.*

## Testing Asynchrononous Protocols: Reliable Broadcast
To demonstrate constructing a protocol based on identifying counterexamples and refining the implementation based on these counterexamples, we are going to consider the case of implementing a protocol that should achieve reliable broadcast.  

The protocols we are going to consider are well documented and in their original presentation by Demers *et al.* are presented as refinements for both (a.) efficiency and (b.) reliability. The first protocol, *direct mail* is the simplest protocol that tries to achieve reliable broadcast but fails to achieve this under both membership changes, general message omissions, and crash failures.  The second protocol we consider is *anti-entropy*, a protocol that is extremely resilient to failure, but relies on pairwise exchange of all messages that are received to ensure reliable broadcast.  The final protocol we consider is *rumor-mongering*, a protocol that attempts to achieve reliable broadcast by using techniques from both direct mail and anti-entropy -- direct mail for its efficiency and anti-entropy for its resilience.

### Attempt #1: Demers _et al._'s Direct Mail

We start by creating a process for the direct mail protocol implementation.  This implementation will support two calls ```broadcast```, for sending a message, and ```update```, for updating the membership received from the Partisan system for when view changes occur.  For state at each node, we will track the currently known membership, so we don't have to look it up every time we want to make a broadcast.

{% highlight erlang %}
-export([broadcast/2,
         update/1]).

-record(state, {membership}).

%% @doc Broadcast.
broadcast(ServerRef, Message) ->
    gen_server:cast(?MODULE, {broadcast, ServerRef, Message}).

%% @doc Membership update.
update(LocalState0) ->
    LocalState = partisan_peer_service:decode(LocalState0),
    gen_server:cast(?MODULE, {update, LocalState}).
{% endhighlight %}

We now need to implement the behavior for each of these callbacks.  First, we define the behavior that occurs when the membership is updated.  For this, we will update our local cache of the membership.

{% highlight erlang %}
%% @doc Perform membership update when the membership changes.
handle_cast({update, Membership0}, State) ->
    Membership = membership(Membership0),
    {noreply, State#state{membership=Membership}};
{% endhighlight %}

We will use a helper called ```membership``` that will be used to ensure that all of the nodes in the system sort the membership the same way: this ensures that when we want to begin testing, we remain deterministic.

{% highlight erlang %}
%% @private -- sort to remove nondeterminism in node selection.
membership(Membership) ->
    lists:usort(Membership).
{% endhighlight %}

The direct mail protocol states that for each messages we want to broadcast, we first deliver the message to ourselves and then to all of the other members in the cluster, as determined by the known membership.  We can do this by implementing a callback for ```broadcast``` that uses the cluster's local view of the membership and then forwards a message to every node in that view.

Our broadcast function is implemented as follows: 

* We take a named process identifier at the destination that the message will be forwarded to.
* We derive a unique, deterministic, identifier and payload for the message.
* For each known member in the cluster, we forward the message to that node.
* We use local storage to keep track of the transmitted messages.

{% highlight erlang %}
%% @doc Handle an outgoing broadcast message.
handle_cast({broadcast, ServerRef, Message},
            #state{membership=Membership}=State) ->
    Manager = manager(),

    %% Generate message id.
    MyNode = partisan_peer_service_manager:mynode(),
    Id = {MyNode, erlang:unique_integer([monotonic, positive])},

    %% Forward to process.
    partisan_util:process_forward(ServerRef, Message),

    %% Store outgoing message.
    true = ets:insert(?MODULE, {Id, Message}),

    %% Forward messages to every node except our own node identifier.
    lists:foreach(fun(N) ->
        Manager:forward_message(N, 
                                ?GOSSIP_CHANNEL, 
                                ?MODULE, 
                                {broadcast, Id, ServerRef, Message})
    end, membership(Membership) -- [MyNode]),

    {noreply, State};
{% endhighlight %}

Now, we need to define how messages should be handled upon receipt by the other nodes in the cluster.  We implement a handler for incoming messages, that pattern matches on the body of the message and takes action accordingly.  In this example, when a message is received by a node, we forward to the destination process using the supplied process identifier, if we have not seen the message yet; otherwise, we drop the message without further processing.

{% highlight erlang %}
%% @doc Incoming messages.
handle_info({broadcast, Id, ServerRef, Message}, State) ->
    case ets:lookup(?MODULE, Id) of
        [] ->
            %% Forward to process.
            partisan_util:process_forward(ServerRef, Message),

            %% Store.
            true = ets:insert(?MODULE, {Id, Message}),

            ok;
        _ ->
            ok
    end,

    {noreply, State};
{% endhighlight %}

With that, the direct mail protocol implementation is finished.  However, by now, the reader should be suspicious of this protocol: there is no built in redundancy for message omissions, membership changes or crash failures.

To test these bugs, we use Partisan's built-in testing infrastructure.  We start by using the provided *reliable broadcast* model, which takes an input model and states that every transmitted message should be received by all non-crashed nodes in the cluster.

The reliable broadcast model provides two possible commands for test generation of a application developer supplied broadcast implementation.

* **Broadcast:** A ```broadcast``` function that, given a node identifier, a message to broadcast, and a destination process identifier, transmits the message.  Application developers who implement their own broadcast protocol  are expected to provide a ```broadcast``` function of their own.  The destination process and its identifier will be provided automatically by the test harness and passed to this call.
* **Assertion:** A ```check_mailbox``` function that, given a node identifier and a set of expected messages, performs and assertion to verify that all of the expected received messages are there.  This function uses the spawned destination process to accumulate and perform assertions on messages.

Now specified, Partisan's testing infrastructure will automatically generate random schedules of commands and at each command, insert that the postconditions from each command return true.  Partisan's commands are selected from the following types of commands:

* **Membership Commands:**  Maintaining a minimum number of nodes in the cluster, Partisan will perform random join and leave operations for a number of additional nodes.  This ensures that application behavior remains correct under cluster transitions.
    
* **Fault Commands:**  Given a fault model, introduce a number of faults, including, but not limited to, *stop* failures, *crash* failures, *send-omission* failures, *receive-omission* failures, *general omission* failures, and *arbitrary* failures.  Failures will only be introduced given a failure tolerance level, specified by the application developer.

* **Model Commands:** These commands are the model specific commands that drive application behavior.  In the case we are discussing, these are the commands from the reliable broadcast model.

### Counterexample #1: Omission Faults
Let us start by testing the model under omissions faults -- typically observed during network partitions.  We start by running Partisan's fault injector to find a counterexample with a failure tolerance of 1 simultaneous failure.  

We can do this simply by running the following command:

{% highlight sh %}
$ FAULT_INJECTION=true bin/counterexample-find.sh
{% endhighlight %}

We find our first counterexample!  After several schedule tests and commands, our counterexample looks as follows.  Partisan produces the full execution trace in the output, but we retain only the most important parts for our explanation here.  We see that *node_5* is missing two messages in the mailbox assertion.  

{% highlight erlang %}
verifying mailbox at node node_5:
=> sent: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{346,node_3,-6},{350,node_1,-38},{354,node_3,-19}], 
=> received: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{350,node_1,-38}]

verification of mailbox at node_5 failed!
=> missing: [{346,node_3,-6},{354,node_3,-19}], 
=> received: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{350,node_1,-38}]

postcondition result: false; command: prop_partisan_reliable_broadcast:check_mailbox([node_5])
{% endhighlight %}

If we look at an excerpt of the message trace of the execution, we see the following:

{% highlight sh %}
node_3@GS18227 <- node_1@GS18227: {forward_message,demers_direct_mail,{broadcast,{node_1@GS18227,2},receiver,{350,node_1,-38}}}
node_3@GS18227 => node_5@GS18227: DROPPED {broadcast,{node_3@GS18227,3},receiver,{346,node_3,-6}}
node_3@GS18227 => node_1@GS18227: {forward_message,demers_direct_mail,{broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}}
node_1@GS18227 <- node_3@GS18227: {forward_message,demers_direct_mail,{broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}}
node_3@GS18227 => node_2@GS18227: {forward_message,demers_direct_mail,{broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}}
node_2@GS18227 <- node_3@GS18227: {forward_message,demers_direct_mail,{broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}}
node_3@GS18227 => node_4@GS18227: {forward_message,demers_direct_mail,{broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}}
node_4@GS18227 <- node_3@GS18227: {forward_message,demers_direct_mail,{broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}}
node_3@GS18227 => node_5@GS18227: DROPPED {broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}
{% endhighlight %}

We see from the trace that Partisan has introduced several failures randomly throughout the execution.  Here we see the send omission from *node_3* to *node_5*.

### Identifying and Replaying the Fault
We can see from the assertion that *node_5* is missing two messages from *node_3*.  Examining the message trace, it is clear that the send omission failure that prohibited *node_3* from sending to *node_5* caused the two message omissions resulting in the failure; therefore, reliable broadcast cannot be satisfied under this fault model.

We can replay our fault using Partisan's deterministic testing replay behavior.  This will use the previous trace and command schedule to run the same set of commands and enforce the message delivery order using barriers to ensure deterministic replay of messages on the network.

We can run the following command for replay:

{% highlight sh %}
$ FAULT_INJECTION=true bin/counterexample-replay.sh
Staging counterexample...
Replaying counterexample...
{% endhighlight %}

Replaying the fault will produce the same output as the previous example did. We see here that under replay we have the same failure.

{% highlight erlang %}
verifying mailbox at node_5: 
=> sent: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{346,node_3,-6},{350,node_1,-38},{354,node_3,-19}]
=> received: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{350,node_1,-38}]

verification of mailbox at node_5 failed!
=> missing: [{346,node_3,-6},{354,node_3,-19}], 
=> received: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{350,node_1,-38}]

postcondition result: false; command: prop_partisan_reliable_broadcast:check_mailbox([node_5])

...

Counterexample held and replayed...
{% endhighlight %}

Cool, it replayed the example.

### Resolution: Acknowledgements defeat Finite Omission Faults
Omission faults are problematic when using protocols that only transmit messages once.  In this case, a network partition causes messages to be omitted between two participants, resulting in two messages that never get delivered to one of the nodes in the cluster.

One solution to the problem is to use message acknowledgements.  We can use a modified Partisan call when transmitting messages to ensure that we continue to redeliver messages until the remote node acknowledges them.  That ensure that when a partition is resolved, these messages will eventually be delivered and we will be able to fulfill reliable broadcast.

Here, we provide an example where we modify our call for message forwarding to ask for message acknowledgements.  This call specifies that this message needs to be acknowledged by using the ```{ack, true}``` tuple as an optional argument.

{% highlight erlang %}
%% Forward messages.
lists:foreach(fun(N) ->
     Manager:forward_message(N, 
                             ?GOSSIP_CHANNEL, 
                             ?MODULE, 
                             {broadcast, Id, ServerRef, Message}, 
                             [{ack, true}])
end, membership(Membership) -- [MyNode]),
{% endhighlight %}

By adding message acknowledgements to the direct mail protocol, which performs only a single message transmission for each peer, we can ensure that we are able to recover from message omission failures if and when the network partition heals and omission faults stop occurring.  This is provided by the reliable unicast mechanism in Partisan that will retransmit and de-duplicate messages based on message identifier, as shown above using the ```[{ack, true}]``` option.