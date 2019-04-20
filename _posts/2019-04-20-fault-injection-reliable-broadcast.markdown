---
layout: post
title:  "Using Fault-Injection to Evolve a Reliable Broadcast Protocol"
date:   2019-04-20 00:00:00 -0000
categories: erlang partisan
group: Partisan
---

*This is the first article in a series about building reliable fault-tolerant applications with [Partisan](http://partisan.cloud), our high-performance, distributed runtime for the Erlang programming language.  As part of this project, we will start with some pretty simple protocols and show how our system will guide you in adjusting the protocol for fault-tolerance issues.*

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

### Counterexample #2: Unresolved Omission Faults
However, we quickly discover through random testing that this may not be enough if the network partition does not heal.  Consider the following execution where the network partition does not recover -- the continuous retransmission results in more omitted messages.

Here's another generated test that fails, due to unresolved message omissions.

{% highlight erlang %}
verifying mailbox at node_2:
=> sent: [{19,node_1,-3},{22,node_4,6},{27,node_1,-3}]
=> received: [{19,node_1,-3},{27,node_1,-3}]

verification of mailbox at node_2 failed!
=> missing: [{22,node_4,6}]
=> received: [{19,node_1,-3},{27,node_1,-3}]

postcondition result: false; command: prop_partisan_reliable_broadcast:check_mailbox([node_2])
{% endhighlight %}

Here's the resulting trace, showing that even retranmission of dropped messages is only sufficient if the partition heals.

{% highlight erlang %}
node_4@parrhesia => node_2@parrhesia: DROPPED {broadcast,{node_4@parrhesia,2},receiver,{22,node_4,6}}
node_1@parrhesia => node_2@parrhesia: {forward_message,node_1@parrhesia,{undefined,[{node_1@parrhesia,4}]},demers_direct_mail,{broadcast,{node_1@parrhesia,3},receiver,{27,node_1,-3}}}
node_4@parrhesia => node_3@parrhesia: {forward_message,node_4@parrhesia,{undefined,[{node_4@parrhesia,3}]},demers_direct_mail,{broadcast,{node_4@parrhesia,2},receiver,{22,node_4,6}}}
node_1@parrhesia => node_3@parrhesia: {forward_message,node_1@parrhesia,{undefined,[{node_1@parrhesia,5}]},demers_direct_mail,{broadcast,{node_1@parrhesia,3},receiver,{27,node_1,-3}}}
node_1@parrhesia => node_4@parrhesia: {forward_message,node_1@parrhesia,{undefined,[{node_1@parrhesia,6}]},demers_direct_mail,{broadcast,{node_1@parrhesia,3},receiver,{27,node_1,-3}}}
node_4@parrhesia => node_2@parrhesia: DROPPED {broadcast,{node_4@parrhesia,2},receiver,{22,node_4,6}}
node_4@parrhesia => node_2@parrhesia: DROPPED {broadcast,{node_4@parrhesia,2},receiver,{22,node_4,6}}
node_4@parrhesia => node_2@parrhesia: DROPPED {broadcast,{node_4@parrhesia,2},receiver,{22,node_4,6}}
node_4@parrhesia => node_2@parrhesia: DROPPED {broadcast,{node_4@parrhesia,2},receiver,{22,node_4,6}}
node_4@parrhesia => node_2@parrhesia: DROPPED {broadcast,{node_4@parrhesia,2},receiver,{22,node_4,6}}
node_2 leaving command: [check_mailbox,node_2]
{% endhighlight %}

While a valid counterexample, it is unrealistic as this test demonstrates that an assertion fails only due to the fact that the partition is not resolved.

To resolve this, we will consider an alternative scheduler for Partisan that assumes that partitions heal in finite time.  We will not discuss the details of this scheduler here, but we will demonstrate its usage in testing.

### Counterexample #3: Crash Failures
Assuming that the system will only suffer from network partitions of finite length, we soon discover that the acknowledgement mechanism is not enough when the system has to also consider the possibility of crash failures.  If we consider the case where a node has buffered a message and is retransmitting that message, a crash of the transmitting node will prevent the message from ever being delivered to the destination.

In short *acknowledgements* are sufficient for being resilient against *general-omission* failures with the direct mail protocol.  However, acknowledgements are not sufficient for being robust against crash failures of the node with the omission fault.  

To demonstrate this, we will use the *finite fault* scheduler for Partisan, which ensures that faults only exist for a finite time during the execution, resolving the faults by either crashing the faulted nodes or healing them, before performing a final assertion.

We demonstrate the usage of the finite fault scheduler here:

{% highlight sh %}
$ SCHEDULER=finite_fault FAULT_INJECTION=true bin/counterexample-find.sh
{% endhighlight %}

Running this, we quickly identify another counterexample.  This counterexample demonstrates that with finite faults enabled, it is sufficient to have a node partitioned during a send, those sends are kept outstanding and retransmitted for the duration of the experiment, but the node is crashed prior to receiving an acknowledgement of that send.  This relates to the case of a faulty node that transmits a message and does not receive an acknowledgement before ultimately crashing.

### Attempt #2: Demers *et al.*'s Anti-Entropy
Demers *et al.*'s *anti-entropy* performs pairwise exchanges of messages to synchronize two replicas.  It was originally designed for replica repair in the Clearinghouse system.  In *anti-entropy*, nodes can operate in a possible three modes: (a.) *pull*, where one node requests the contents of another; (b.) *push*, where one node pushes its data store contents to another, or (c.) *push-pull*, where a two-way exchange is performed.

In a modern implementation of this algorithm, nodes would exchange either checksums or message identifiers that would be used to identify difference between data objects -- this would result in transmission of only these data objects.  In our implementation, for simplicity and to demonstrate the protocol, we will transmit the entire contents of the database at each exchange.  For our implementation, we use the *push-pull* algorithm, which is the most widely used implementation of *rumor-mongering* today.

Since our implementation of anti-entropy is quite different from our implementation of direct mail, let us spend time to walk through it.  We will assume the same top-level API containing the ```broadcast``` and ```check_mailbox``` functions.

Let us look at the initializer.  We are sure to initiate a callback that will schedule the periodic exchanges.

{% highlight erlang %}
%% Schedule anti-entropy.
schedule_anti_entropy(),
{% endhighlight %}

Here is our periodic function.

{% highlight erlang %}
%% @private
schedule_anti_entropy() ->
    Interval = 1000,
    erlang:send_after(Interval, ?MODULE, antientropy).
{% endhighlight %}

When our timer fires, we perform the following actions: (a.) transmit our messages as *push* messages and (b.) reschedule the timer.

{% highlight erlang %}
handle_info(antientropy, #state{membership=Membership}=State) ->
    Manager = manager(),
    MyNode = partisan_peer_service_manager:mynode(),

    %% Get all of our messages.
    OurMessages = ets:foldl(fun({Id, Message}, Acc) ->
        Acc ++ [{Id, Message}] 
    end, [], ?MODULE),

    %% Forward to random subset of peers.
    AntiEntropyMembers = select_random_sublist(membership(Membership), ?GOSSIP_FANOUT),

    lists:foreach(fun(N) ->
        Manager:forward_message(N, ?GOSSIP_CHANNEL, ?MODULE, {push, MyNode, OurMessages}, [])
    end, AntiEntropyMembers -- [MyNode]),

    %% Reschedule.
    schedule_anti_entropy(),

    {noreply, State};
{% endhighlight %}

When another node receives a *push*, we respond with a *pull* message containing our messages after we have incorporated the incoming messages into our state.

{% highlight erlang %}
handle_info({push, FromNode, TheirMessages}, State) ->
    Manager = manager(),
    MyNode = partisan_peer_service_manager:mynode(),

    %% Encorporate their messages and process them if we did not see them.
    lists:foreach(fun({Id, {ServerRef, Message}}) ->
        case ets:lookup(?MODULE, Id) of
            [] ->
                %% Forward to process.
                partisan_util:process_forward(ServerRef, Message),

                %% Store.
                true = ets:insert(?MODULE, {Id, {ServerRef, Message}}),

                ok;
            _ ->
                ok
        end
    end, TheirMessages),

    %% Get all of our messages.
    OurMessages = ets:foldl(fun({Id, {ServerRef, Message}}, Acc) ->
        Acc ++ [{Id, {ServerRef, Message}}] 
    end, [], ?MODULE),

    %% Forward message back to sender.
    lager:info("~p: sending messages to node ~p", [node(), FromNode]),
    Manager:forward_message(FromNode, ?GOSSIP_CHANNEL, ?MODULE, {pull, MyNode, OurMessages}, []),

    {noreply, State}
{% endhighlight %}

Now, to handle the *pull* message, we process the incoming messages and store them.

{% highlight erlang %}
handle_info({pull, _FromNode, Messages}, State) ->
    %% Process all incoming.
    lists:foreach(fun({Id, {ServerRef, Message}}) ->
        case ets:lookup(?MODULE, Id) of
            [] ->
                %% Forward to process.
                partisan_util:process_forward(ServerRef, Message),

                %% Store.
                true = ets:insert(?MODULE, {Id, {ServerRef, Message}}),

                ok;
            _ ->
                ok
        end
    end, Messages),

    {noreply, State};   
{% endhighlight %}

Now, our implementation is complete.  We can use same failure model as before to identify a further counterexample.

{% highlight sh %}
$ SCHEDULER=finite_fault FAULT_INJECTION=true bin/counterexample-find.sh
{% endhighlight %}

It is clear that *anti-entropy* is more resilient to message omission and crash failures.  A given message has a higher probability of reaching a node by transmitting that message to a peer prior to its crash; therefore, ensuring that a message will ultimately be received at its final recipient through the transitivity of a anti-entropy process.  However, our original model fails because of the time it takes for this to happen.  

How can we reduce the latency of a broadcast?

### Attempt #3: Demers *et al.*'s Rumor-Mongering
Demers *et al.*'s *rumor-mongering* protocol is designed around the theory of epidemics.  In their protocol design, messages are sent to their recipients immediately, but continuously re-forwarded until each node has seen the message ``enough'' times: this being a factor of the level of resilience the system desires based on the theory.  For our implementation of the protocol, we re-f   orward messages until each node has seen the message at least once.

We build upon the *direct mail* implementation from earlier, using parts from our *anti-entropy* implementation.  We alter our original broadcast to include the origin node in the message.  We also choose to only send this message to a random subset of our peers.

{% highlight erlang %}
%% Forward to random subset of peers.
AntiEntropyMembers = select_random_sublist(membership(Membership), ?GOSSIP_FANOUT),

lists:foreach(fun(N) ->
    Manager:forward_message(N, 
                            ?GOSSIP_CHANNEL, 
                            ?MODULE, 
                            {broadcast, Id, ServerRef, Message, MyNode}, 
                            [])
end, AntiEntropyMembers -- [MyNode]),
{% endhighlight %}

Next, we modify the behavior when we receive a message.  If we receive a message that we have not seen before, we forward it to a random subset of our peers, excluding the peer that we received the message from.

{% highlight erlang %}
handle_info({broadcast, Id, ServerRef, Message, FromNode}, 
            #state{membership=Membership}=State) ->
    case ets:lookup(?MODULE, Id) of
        [] ->
            %% Forward to process.
            partisan_util:process_forward(ServerRef, Message),

            %% Store.
            true = ets:insert(?MODULE, {Id, Message}),

            %% Forward to our peers.
            Manager = manager(),
            MyNode = partisan_peer_service_manager:mynode(),

            %% Forward to random subset of peers: except ourselves and where we got it from.
            AntiEntropyMembers = select_random_sublist(membership(Membership), ?GOSSIP_FANOUT),

            lists:foreach(fun(N) ->
                Manager:forward_message(N, 
                                        ?GOSSIP_CHANNEL, 
                                        ?MODULE, 
                                        {broadcast, Id, ServerRef, Message, MyNode}, 
                                        [])
            end, AntiEntropyMembers -- [MyNode, FromNode]),

            ok;
        _ ->
            ok
    end,

    {noreply, State};
{% endhighlight %}

Now, our implementation is complete.  We can run this new implementation under the same failure model.

{% highlight sh %}
$ SCHEDULER=finite_fault FAULT_INJECTION=true bin/counterexample-find.sh
{% endhighlight %}

However, we still run into issues.  With a purely asynchronous send from the source with a network partition, the message may ultimately be delayed to its final destination because of message omission failures.  

This implementation, while still yielding a counterexample, is the most robust implementation of reliable broadcast we have seen yet, as it minimizes the latency for each send while optimizing *fanout* in an attempt to achieve reliable broadcast.  The only way to achieve a stronger guarantee is to rely on some form of *consensus*.