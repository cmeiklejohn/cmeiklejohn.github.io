---
layout: post
title:  "Verified Vector Clocks: An Experience Report, Part 2"
date:   2013-11-18 22:01:09 -0500
categories: coq erlang
---

_This post outlines a bunch of experimental work I've completed to model
data structures in Coq, leveraging Tim Carstens' [verlang][verlang]
project to extract the data structures into executable Erlang._

_Here's a link to the [first][first] post in this series._

_Updated March 8th, 2014: A full talk about this work was presented at Erlang
Factory, San Francisco 2014.  Both the
[slides](https://speakerdeck.com/cmeiklejohn/vector-clocks-in-coq-an-experience-report)
and [video](https://www.youtube.com/watch?v=IINmkv4izVQ) are available._

# Extending with timestamps

Let us extend our existing model to carry timestamps, as the
[`riak_core`][riak_core] model does.  In core, we model the actual item
in the vector clock as nested tuples structured similar to the
following:

{% highlight erlang %}
-type vc_entry() :: {vclock_node(), {counter(), timestamp()}}.
{% endhighlight %}

These timestamps are generated with Erlang code, which is similar to the
following, but optimized:

{% highlight erlang %}
2> calendar:datetime_to_gregorian_seconds(erlang:universaltime()).
63551049296
{% endhighlight %}

This produces a monotonically advancing integer representing the current
time in seconds.  Note, this time is system dependent, succeptable to
clock skew and all of the other wonderful things that come with using
wall clock time.  That aside, we are aiming to model something that could
be used as a replacement for the existing Riak Core vclock model, so
we will go with this model.

We will start by modifying our types to model the timestamp as a natural
number, and abstract the functions used to initialize and increment it
so we can replace these with the actual calls to the timestamp
generating function in Erlang.

We will begin by modeling the clock itself as product of products,
essentially a triple as a product is formed like the following:

{% highlight coq %}
Inductive prod (A B:Type) : Type :=
  pair : A -> B -> prod A B.

  Add Printing Let prod.

  Notation "x * y" := (prod x y) : type_scope.
  Notation "( x , y , .. , z )" := (pair .. (pair x y) .. z) : core_scope.
{% endhighlight %}

See the implementation below:

{% highlight coq %}
(** Type definitions for actors, counts and timestamps. *)
Definition actor := nat.
Definition count := nat.
Definition timestamp := nat.

(** Model clocks as triples. *)
Definition clock := prod actor (prod count timestamp).

(** Model vector clocks at a list of clock triples. *)
Definition vclock := (list clock)%type.
{% endhighlight %}

Let us now add some functions for handling the incrementing and
initializing of values:

{% highlight coq %}
(** Function to initialize a timestamp with the default value. *)
Definition init_timestamp := O.

(** Function to increment a timestamp from one value to the next. *)
Definition incr_timestamp (timestamp : timestamp) := S timestamp.

(** Function to initialize a counter with the default values. *)
Definition init_count := O.

(** Function to increment the counter from one value to the next. *)
Definition incr_count (count : count) := S count.
{% endhighlight %}

Let us now update our functions to increment the timestamp accordingly:

{% highlight coq %}
(** Increment a vector clock. *)
Definition increment (actor : actor) (vclock : vclock) :=
  match find (find' actor) vclock with
  | None => 
    cons (pair actor (pair init_count init_timestamp)) vclock
  | Some (pair x (pair count timestamp)) => 
    cons (pair x (pair (incr_count count) (incr_timestamp timestamp)))
                       (filter (find' actor) vclock)
  end.

(** Helper fold function for equality comparison betwen two vector clocks. *)
Definition equal' status_and_vclock (clock : clock) :=
  match clock, status_and_vclock with
    | pair actor (pair count timestamp), 
      pair status vclock => match find (find' actor) vclock with
                              | None => 
                                pair false vclock
                              | Some (pair _ (pair y z)) => 
                                pair (andb 
                                        status
                                        (andb
                                           (beq_nat count y)
                                           (beq_nat timestamp z)))
                                        vclock
                            end
  end.

(** Compare equality between two vector clocks. *)
Definition equal (vc1 vc2 : vclock) := 
  match fold_left equal' vc1 (pair true vc2) with
    | pair false _ => 
      false
    | pair true _ => 
      match fold_left equal' vc2 (pair true vc1) with
        | pair false _ => 
          false
        | pair true _ => 
          true
      end
  end.

(** Less than or equal to comparson for natural numbers. *)
Fixpoint ble_nat (n m : nat) {struct n} : bool :=
  match n with
  | O => true
  | S n' =>
      match m with
      | O => false
      | S m' => ble_nat n' m'
      end
  end.

(** Decends fold helper for determining ordering. *)
Definition descends' status_and_vclock (clock : clock) :=
  match clock, status_and_vclock with
    | pair actor (pair count timestamp),
      pair status vclock => match find (find' actor) vclock with
                              | None => 
                                pair false vclock
                              | Some (pair _ (pair y z)) => 
                                pair (andb
                                        status
                                        (andb
                                           (ble_nat count y)
                                           (ble_nat timestamp z))) vclock
                            end
  end.

(** Determine if one vector clock is a descendent of another. *)
Definition descends (vc1 vc2 : vclock) := 
  match fold_left descends' vc2 (pair true vc1) with
    | pair false _ =>
      false
    | pair true _ => 
      true
  end.

(** Fold helper for the merge function which computes max. *)
Definition max' (vclock : vclock) (clock : clock) :=
  match clock with
    | pair actor (pair count timestamp) => 
      match find (find' actor) vclock with
        | None => 
          cons (pair actor (pair count timestamp)) vclock
        | Some (pair _ (pair y z)) => 
          cons (pair actor (pair (max count y) (max timestamp z)))
               (filter (find'' actor) vclock)
      end
  end.

(** Merge two vector clocks. *)
Definition merge (vc1 vc2 : vclock) := fold_left max' vc1 vc2.

(** Return current count of an actor in a vector clock. *)
Definition get_counter (actor : actor) (vclock : vclock) :=
  match find (find' actor) vclock with
      | None => 
        None
      | Some (pair a (pair count timetsamp)) =>
        Some count
  end.
{% endhighlight %}

Finally, let us add a function to return the timestamps:

{% highlight coq %}
(** Return current timestamp of an actor in a vector clock. *)
Definition get_timestamp (actor : actor) (vclock : vclock) :=
  match find (find' actor) vclock with
      | None => 
        None
      | Some (pair a (pair count timetsamp)) =>
        Some timestamp
  end.
{% endhighlight %}

Now, to figure out how to implement the prune functionality, starting by
examining the implementation of `prune` in Riak Core.

For more information on why we have to prune, check out the classic
["Why Vector Clocks Are Hard"][hard] article from the Basho blog.

{% highlight erlang %}
% @doc Possibly shrink the size of a vclock, depending on current age and size.
-spec prune(V::vclock(), Now::integer(), BucketProps::term()) -> vclock().
prune(V,Now,BucketProps) ->
    %% This sort need to be deterministic, to avoid spurious merge conflicts later.
    %% We achieve this by using the node ID as secondary key.
    SortV = lists:sort(fun({N1,{_,T1}},{N2,{_,T2}}) -> {T1,N1} < {T2,N2} end, V),
    prune_vclock1(SortV,Now,BucketProps).
% @private
prune_vclock1(V,Now,BProps) ->
    case length(V) =< get_property(small_vclock, BProps) of
        true ->
            V;
        false ->
            {_,{_,HeadTime}} = hd(V),
            case (Now - HeadTime) < get_property(young_vclock,BProps) of
                true ->
                    V;
                false ->
                    prune_vclock1(V,Now,BProps,HeadTime)
            end
    end.
% @private
prune_vclock1(V,Now,BProps,HeadTime) ->
    % has a precondition that V is longer than small and older than young
    case (length(V) > get_property(big_vclock,BProps)) orelse
         ((Now - HeadTime) > get_property(old_vclock,BProps)) of
        true ->
            prune_vclock1(tl(V),Now,BProps);
        false ->
            V
    end.
{% endhighlight %}

The first thing we see is that we do not prune the vector clock if it is
still considered small.

{% highlight erlang %}
prune_vclock1(V,Now,BProps) ->
    case length(V) =< get_property(small_vclock, BProps) of
        true ->
            V;
{% endhighlight %}

In the case where the vector clock is no longer considered small, we
take the earliest clock, from the lexographically earliest actor, and
attempt to determine if it is still considered young.  If it is, we do
nothing.  If not, our vector clock has candidates for pruning.

{% highlight erlang %}
prune_vclock1(V,Now,BProps) ->
    case length(V) =< get_property(small_vclock, BProps) of
        false ->
            {_,{_,HeadTime}} = hd(V),
            case (Now - HeadTime) < get_property(young_vclock,BProps) of
                true ->
                    V;
                false ->
                    prune_vclock1(V,Now,BProps,HeadTime)
            end
    end.
{% endhighlight %}

Let's examine the next function call.  In the false case, unless the
vector clock is considered large, or the timestamp is considered old, we
do not perform the prune.

{% highlight erlang %}
prune_vclock1(V,Now,BProps,HeadTime) ->
    % has a precondition that V is longer than small and older than young
    case (length(V) > get_property(big_vclock,BProps)) orelse
         ((Now - HeadTime) > get_property(old_vclock,BProps)) of
        true ->
            prune_vclock1(tl(V),Now,BProps);
        false ->
            V
    end.
{% endhighlight %}

Then we repeat the entire process with the tail of the list, given that
the head was prime for pruning.

Now, we've run into a couple problems:

* We have no way to access properties stored in buckets, or the
  application environment in Riak.
* We have no easy way to work with the pure Erlang structures in Coq.

So, the approach we will take is implementing a prune function in Coq,
which will take all of the environmental arguments explicitly, and then
we will write a function in our Erlang module to bridge the gap between
the data structures in Coq and in Erlang.

Here's what our prune function looks like in Coq.

{% highlight erlang %}
Fixpoint prune'
         (vclock : vclock)
         (small large : nat)
         (young old : timestamp) :=
  match vclock with
    | nil =>
      vclock
    | pair actor (pair count timestamp) :: clocks =>
      match (ble_nat (length vclock) small) with 
        | true => 
          vclock
        | false => 
          match (ble_nat timestamp young) with
            | true => 
              vclock
            | false => 
              match (bgt_nat timestamp old) with
                  | false => 
                    vclock
                  | true => 
                    match (bgt_nat (length vclock) large) with
                        | false =>
                          vclock
                        | true => 
                          prune' clocks small large young old
                    end
              end
          end
      end
  end.
{% endhighlight %}

In the next post, we'll look at how we begin integrating this with the
`vclock` module in Riak Core.

[verlang]: https://github.com/tcarstens/verlang
[riak_core]: https://github.com/basho/riak_core
[hard]: http://basho.com/why-vector-clocks-are-hard/
[first]: http://christophermeiklejohn.com/coq/erlang/2013/11/04/verified-vector-clocks-an-experience-report-part-1.html
