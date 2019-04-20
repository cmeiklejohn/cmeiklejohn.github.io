---
layout: post
title:  ""
date:   2019-04-20 00:00:00 -0000
categories: erlang lasp
group: Partisan
---

## Testing Asynchrononous Protocols: Reliable Broadcast
To demonstrate constructing a protocol based on identifying counterexamples and refining the implementation based on these counterexamples, we are going to consider the case of implementing a protocol that should achieve reliable broadcast.  

The protocols we are going to consider are well documented and in their original presentation~\footnote{These protocols were formally presented in~\cite{Demers:1987:EAR:41840.41841} but originally used by the Clearinghouse~\cite{Oppen:1983:CDA:357436.357439} system.} by Demers \textit{et al.}~\cite{Demers:1987:EAR:41840.41841} are presented as refinements for both (a.) efficiency and (b.) reliability. The first protocol, \textit{direct mail} is the simplest protocol that tries to achieve reliable broadcast but fails to achieve this under both membership changes, general message omissions, and crash failures.  The second protocol we consider is \textit{anti-entropy}, a protocol that is extremely resilient to failure, but relies on pairwise exchange of all messages that are received to ensure reliable broadcast.  The final protocol we consider is \textit{rumor-mongering}, a protocol that attempts to achieve reliable broadcast by using techniques from both direct mail and anti-entropy -- direct mail for its efficiency and anti-entropy for its resilience.

### Attempt #1: Demers _et al._'s Direct Mail

We start by creating a process for the direct mail protocol implementation.  This implementation will support two calls \mintinline{erlang}{broadcast}, for sending a message, and \mintinline{erlang}{update}, for updating the membership received from the \Name\ system for when view changes occur.  For state at each node, we will track the currently known membership, so we don't have to look it up every time we want to make a broadcast.

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

The direct mail protocol states that for each messages we want to broadcast, we first deliver the message to ourselves and then to all of the other members in the cluster, as determined by the known membership.  We can do this by implementing a callback for \mintinline{erlang}{broadcast} that uses the cluster's local view of the membership and then forwards a message to every node in that view.

Our broadcast function is implemented as follows: 

\begin{enumerate}
    \item We take a named process identifier at the destination that the message will be forwarded to.
    \item We derive a unique, deterministic, identifier and payload for the message.
    \item For each known member in the cluster, we forward the message to that node.
    \item We use local storage to keep track of the transmitted messages.
\end{enumerate}

\begin{minted}{erlang}
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
\end{minted}

Now, we need to define how messages should be handled upon receipt by the other nodes in the cluster.  We implement a handler for incoming messages, that pattern matches on the body of the message and takes action accordingly.  In this example, when a message is received by a node, we forward to the destination process using the supplied process identifier, if we have not seen the message yet; otherwise, we drop the message without further processing.

\begin{minted}{erlang}
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
\end{minted}

With that, the direct mail protocol implementation is finished.  However, by now, the reader should be suspicious of this protocol: there is no built in redundancy for message omissions, membership changes or crash failures.

To test these bugs, we use \Name's built-in testing infrastructure.  We start by using the provided \textit{reliable broadcast} model, which takes an input model and states that every transmitted message should be received by all non-crashed nodes in the cluster.

The reliable broadcast model provides two possible commands for test generation of a application developer supplied broadcast implementation.

\begin{enumerate}
    \item \textbf{Broadcast.} A \mintinline{erlang}{broadcast} function that, given a node identifier, a message to broadcast, and a destination process identifier, transmits the message.  Application developers who implement their own broadcast protocol  are expected to provide a \mintinline{erlang}{broadcast} function of their own.  The destination process and its identifier will be provided automatically by the test harness and passed to this call.
    \item \textbf{Assertion.} A \mintinline{erlang}{check_mailbox} function that, given a node identifier and a set of expected messages, performs and assertion to verify that all of the expected received messages are there.  This function uses the spawned destination process to accumulate and perform assertions on messages.
\end{enumerate}

Now specified, \Name's testing infrastructure will automatically generate random schedules of commands and at each command, insert that the postconditions from each command return true.  \Name's commands are selected from the following types of commands:

\begin{itemize}
    \item \textbf{Membership Commands.}  Maintaining a minimum number of nodes in the cluster, \Name\ will perform random join and leave operations for a number of additional nodes.  This ensures that application behavior remains correct under cluster transitions.
    
    \item \textbf{Fault Commands.}  Given a fault model, introduce a number of faults, including, but not limited to, \textit{stop} failures, \textit{crash} failures, \textit{send-omission} failures, \textit{receive-omission} failures, \textit{general omission} failures, and \textit{arbitrary}\footnote{Arbitrary is the more general term for Byzantine faults.} failures.  Failures will only be introduced given a failure tolerance level, specified by the application developer.

    \item \textbf{Model Commands.} These commands are the model specific commands that drive application behavior.  In the case we are discussing, these are the commands from the reliable broadcast model.
\end{itemize}

\subsection{Counterexample \#1: Omission Faults}
Let us start by testing the model under omissions faults -- typically observed during network partitions.  We start by running \Name's fault injector to find a counterexample with a failure tolerance of $1$\footnote{1 is the default value for fault tolerance, more commonly referred t as $f$.} simultaneous failure.  

We can do this simply by running the following command:

\begin{minted}{bash}
$ FAULT_INJECTION=true bin/counterexample-find.sh
\end{minted}

We find our first counterexample!  After several schedule tests and commands, our counterexample looks as follows.  \Name\ produces the full execution trace in the output, but we retain only the most important parts for our explanation here.  We see that \textit{node\textunderscore5} is missing two messages in the mailbox assertion.  

\begin{minted}[escapeinside=||]{erlang}
verifying mailbox at node node_5:
=> sent: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{346,node_3,-6},{350,node_1,-38},{354,node_3,-19}], 
=> received: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{350,node_1,-38}]

verification of mailbox at node_5 failed!
=> missing: [{346,node_3,-6},{354,node_3,-19}], 
=> received: [{298,node_3,-8},{320,node_4,-27},{334,node_4,8},{343,node_2,25},{350,node_1,-38}]

postcondition result: false; command: prop_partisan_reliable_broadcast:check_mailbox([node_5])
\end{minted}

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
node_3@GS18227 => node_5@GS18227: |\colorbox{yellow}{DROPPED}| {broadcast,{node_3@GS18227,4},receiver,{354,node_3,-19}}
{% endhighlight %}

We see from the trace that Partisan has introduced several failures randomly throughout the execution.  Here we see the send omission from __node_3__ to __node_5__.

\subsubsection{Identifying and Replaying the Fault}
We can see from the assertion that __node_5__ is missing two messages from __node_3__.  Examining the message trace, it is clear that the send omission failure that prohibited __node_3__ from sending to __node_5__ caused the two message omissions resulting in the failure; therefore, reliable broadcast cannot be satisfied under this fault model.

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