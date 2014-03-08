---
layout: post
title:  "Verified Vector Clocks: An Experience Report, Part 1"
date:   2013-11-04 18:39:31 -0500
categories: coq erlang
---

_This post outlines a bunch of experimental work I've completed to model
data structures in Coq, leveraging Tim Carstens' [verlang][verlang]
project to extract the data structures into executable Erlang._

_Updated March 8th, 2014: A full talk about this work was presented at Erlang
Factory, San Francisco 2014.  Both the
[slides](https://speakerdeck.com/cmeiklejohn/vector-clocks-in-coq-an-experience-report)
and [video](https://www.youtube.com/watch?v=IINmkv4izVQ) are available._

# Modeling vector clocks

Let us start by modeling vector clocks in Coq.  We will start by
focusing on the API exposed by [`riak_core`'s][riak_core] vector clock
[implementation][vclock].

For this first implementation, we're going to implement the API without
the timestamps that vclock uses for pruning for a couple reasons: they
are not core to an implementation of vector clocks or version vectors,
and there does not appear to be a good way to model UNIX timestamps in
Coq.

First, some boilerplate and type definitions.  We'll be modeling the
`vclock` as a list of pairs, each pair representing a particular actor
and the count of events observed.  In the actual Riak implementation,
this is modeled as the product of atoms and integers.

_Keep in mind these are not the most efficient implementations of these
methods, we are mainly focusing on naive implementations where we can
easily debug the translation to Core Erlang incase it fails._

{% highlight coq %}
Require Import Coq.Lists.List.
Require Import Coq.Arith.EqNat.
Require Import Coq.Bool.Bool.

Module VVClock.

Definition actor := nat.
Definition count := nat.
Definition clock := (actor * count)%type.
Definition vclock := list clock%type.
{% endhighlight %}

Now, let's add our constructor functions:

{% highlight coq %}
Definition fresh : vclock := nil.
{% endhighlight %}

Now, our functions to increment an actor.

{% highlight coq %}
(** Return a single arity function which searches for a particular
    actor. *)
Definition find' (actor : actor) :=
  fun clock : clock => match clock with
                           | pair x _ => beq_nat actor x
                       end.

(** Return a single arity function which will be filter predicate for
    actor. *)
Definition find'' (actor : actor) :=
  fun clock : clock => match clock with
                           | pair x _ => negb (beq_nat actor x)
                       end.

(** Increment actor in the vclock *)
Definition increment (actor : actor) (vclock : vclock) :=
  match find (find' actor) vclock with
  | None => 
    cons (pair actor 1) vclock
  | Some (pair x y) => 
    cons (pair x (S y)) (filter (find' actor) vclock)
  end.
*)
{% endhighlight %}

Some functions to compare equality amongst vector clocks:

{% highlight coq %}
Definition equal' status_and_vclock (clock : clock) :=
  match clock, status_and_vclock with
    | pair actor count, 
      pair status vclock => match find (find' actor) vclock with
                              | None => 
                                pair false vclock
                              | Some (pair _ y) => 
                                pair (andb
                                        status
                                        (beq_nat count y)) vclock
                            end
  end.

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
{% endhighlight %}

Some functions to compare vector clocks and ensure a proper partial
ordering:

{% highlight coq %}
Fixpoint ble_nat (n m : nat) {struct n} : bool :=
  match n with
  | O => true
  | S n' =>
      match m with
      | O => false
      | S m' => ble_nat n' m'
      end
  end.

Definition descends' status_and_vclock (clock : clock) :=
  match clock, status_and_vclock with
    | pair actor count,
      pair status vclock => match find (find' actor) vclock with
                              | None => 
                                pair false vclock
                              | Some (pair _ y) => 
                                pair (andb
                                        status
                                        (ble_nat count y)) vclock
                                                               end
  end.

Definition descends (vc1 vc2 : vclock) := 
  match fold_left descends' vc2 (pair true vc1) with
    | pair false _ =>
      false
    | pair true _ => 
      true
  end.
{% endhighlight %}

And, finally some functions to perform the merge betwen two vector
clocks.

{% highlight coq %}
Definition max' (vclock : vclock) (clock : clock) :=
  match clock with
    | pair actor count =>  match find (find' actor) vclock with
                             | None => 
                               cons (pair actor count) vclock
                             | Some (pair _ y) => 
                               cons (pair actor (max count y))
                                    (filter (find'' actor) vclock)
                           end
  end.

Definition merge (vc1 vc2 : vclock) := fold_left max' vc1 vc2.
{% endhighlight %}

Qed, one might say.

# Examining the extracted code

{% highlight coq %}
Extraction Language CoreErlang.
Recursive Extraction VVClock.
{% endhighlight %}

Let us now examine the generated code.

One of the first problems that we encounter is that the function used to
construct the vector clock is not extracted.  Let's manually create this
function in the extracted Core Erlang.

{% highlight erlang %}
'fresh'/0 = fun () ->
  []
{% endhighlight %}

We also modify the module exports at the top to reflect this as well.

Next, we'll notice that there are a bunch of function calls which are
incorrectly qualified as calls to `vvclock.VVClock`, which we'll
manually modify to contain the correct call.  These are calls to
functions within the `vvclock` module.

{% highlight erlang %}
call 'vvclock':'ble_nat'
     ( _actor
     , _a
     )
{% endhighlight %}

Another problem we run into are function calls which have not been fully
qualified with their arity.  For example, the call to `descends@` missing
its arity, `descends@/2`.

{% highlight coq %}
'descends'/2 = fun (_vc1, _vc2) ->
  case call 'Coq.Lists.List':'fold_left'
            ( 'descends@'
            , _vc2
            , { 'Pair'
              , 'True'
              , _vc1
              }
            ) of
{% endhighlight %}

Finally, we run into a problem with currying, where function calls like
the following...

{% highlight coq %}
Definition find'' (actor : actor) :=
  fun clock : clock => match clock with
                           | pair x _ => negb (beq_nat actor x)
                       end.
{% endhighlight %}

...get exported to something like the following:

{% highlight erlang %}
'find@'/2 = fun (_actor, _clock) -> 
  case _clock of
    { 'Pair'
    , _c
    , _x
    } when 'true' ->
        call 'Coq.Arith.EqNat':'beq_nat'
             ( _actor
             , _c
             )
   end
{% endhighlight %}

...which causes code like the following, to fail:

{% highlight erlang %}
case call 'Coq.Lists.List':'find'
          ( call 'vvclock':'find@' (_actor)
          , _vclock
          ) of
{% endhighlight %}

In cases like this, we have solved the problem by manually inlining the
call to the find function.

Now, given that we have modeled these clocks as products of Peano
numbers, we can take the existing `riak_core` test suite, and execute
it with only a slight modification and watch the tests pass.

{% highlight erlang %}
riak_core_test() ->
    A  = vvclock:fresh(),
    B  = vvclock:fresh(),
    A1 = vvclock:increment('O', A),
    B1 = vvclock:increment({'S', 'O'}, B),
    B2 = vvclock:increment({'S', 'O'}, B1),

    io:format("A:  ~p~n", [A]),
    io:format("B:  ~p~n", [B]),
    io:format("A1: ~p~n", [A1]),
    io:format("B1: ~p~n", [B1]),
    io:format("B2: ~p~n", [B2]),

    'True'  = vvclock:descends(A1, A),
    'True'  = vvclock:descends(B1, B),
    'False' = vvclock:descends(A1, B1),

    A2 = vvclock:increment('O', A1),
    C  = vvclock:merge(A2, B1),
    C1 = vvclock:increment({'S', {'S', 'O'}}, C),

    io:format("A2: ~p~n", [A2]),
    io:format("C: ~p~n",  [C]),
    io:format("C1: ~p~n", [C1]),

    'True'  = vvclock:descends(C1, A2),
    'True'  = vvclock:descends(C1, B1),
    'False' = vvclock:descends(B1, C1),
    'False' = vvclock:descends(B1, A1),

    ok.
{% endhighlight %}

And, for those of you interested in the output:

{% highlight erlang %}
A:  []
B:  []
A1: [{'Pair','O',{'S','O'}}]
B1: [{'Pair',{'S','O'},{'S','O'}}]
B2: [{'Pair',{'S','O'},{'S',{'S','O'}}}]
A2: [{'Pair','O',{'S',{'S','O'}}}]
C:  [{'Pair','O',{'S',{'S','O'}}},{'Pair',{'S','O'},{'S','O'}}]
C1: [{'Pair',{'S',{'S','O'}},{'S','O'}},
     {'Pair','O',{'S',{'S','O'}}},
          {'Pair',{'S','O'},{'S','O'}}]
{% endhighlight %}

Cool, that's not a bad start.  We can't directly apply this to Riak yet,
as the true and false atoms are represented as constructors for the
Boolean type in Coq, and we're stuck using Peano numbers.  We also don't
have any proofs yet of these functions.  

We will be addressing both of these issues, as well as how we adapted
this library for use in the Riak data store in subsequent posts.

_This code is available on [GitHub][vvclocks] as an OTP application and
[video][video] and [slides][slides] are available of a lightning talk I
did on this work at [RICON|West 2013][ricon]._

[verlang]: https://github.com/tcarstens/verlang
[riak_core]: https://github.com/basho/riak_core
[vclock]: https://github.com/basho/riak_core/blob/develop/src/vclock.erl
[slides]: https://speakerdeck.com/cmeiklejohn/verified-vector-clocks
[video]: http://ricon.io/west2013.html
[vvclocks]: https://github.com/cmeiklejohn/vvclocks
[ricon]: http://ricon.io
[slides]: https://speakerdeck.com/cmeiklejohn/vector-clocks-in-coq-an-experience-report
[presentation]: https://www.youtube.com/watch?v=IINmkv4izVQ
