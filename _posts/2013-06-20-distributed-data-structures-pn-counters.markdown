---
layout: post
title:  "Distributed data structures with Coq: PN-Counters"
date:   2013-06-20 10:17:10
categories: coq
---

Continuing my [previous][previous] post on proving properties about
CRDTs, specifically `G-Counter`s, we'll now look at the `PN-Counter`, a
counter which allows the value to both increment and decrement while
guaranteeing convergence.  Since this builds off of the lemmas proven in
the previous [post][previous], I highly recommend you read that post
first.

First, let's define a `PN-Counter` as a product of two `G-Counter`s.
We'll use one set for counting the increments, and one for counting the
decrements.

{% highlight coq %}
(* Initialize an empty PN_Counter. *)
Definition PN_Counter := (G_Counter * G_Counter)%type.
Definition PN_Counter_init : PN_Counter := (G_Counter_init, G_Counter_init).
{% endhighlight %}

Then, let's define increment and decrement functions for a particular
actor, which call the `G-Counter` increment function on each set.  In
this example, we use `fst` and `snd` to destructure the `PN-Counter`
into two seperate counters, and then after we perform the increment we
use `pair`, the product constructor, to rebuild the `PN-Counter`.

{% highlight coq %}
(* Increment a PN_Counter for a particular actor. *)
Definition PN_Counter_incr actor (clocks : PN_Counter) :=
  pair (G_Counter_incr actor (fst clocks)) (snd clocks).

(* Decrement a PN_Counter for a particular actor. *)
Definition PN_Counter_decr actor (clocks : PN_Counter) :=
  pair (fst clocks) (G_Counter_incr actor (snd clocks)).
{% endhighlight %}

Let's define a function to reveal the current value of the `PN-Counter`,
by subtracting the values of each counter.

{% highlight coq %}
(* Reveal the current value of a PN_Counter. *)
Definition PN_Counter_reveal clocks :=
  minus (G_Counter_reveal (fst clocks)) (G_Counter_reveal (snd clocks)).
{% endhighlight %}

Then, let's define merge, comparsion, and equality functions, which
leverage the functions we've defined over `G-Counter`s.

{% highlight coq %}
(* Merge two PN_Counters. *)
Definition PN_Counter_merge c1 c2 :=
  pair (G_Counter_merge (fst c1) (fst c2)) 
       (G_Counter_merge (snd c1) (snd c2)).

(* Compare two G_Counters. *)
Definition PN_Counter_compare (c1 c2 : PN_Counter) :=
  and (G_Counter_compare (fst c1) (fst c2))
      (G_Counter_compare (snd c1) (snd c2)).

(* Verify that two PN_Counters are equal. *)
Definition PN_Counter_equal (c1 c2 : PN_Counter) :=
  ClockMap.Equal (fst c1) (fst c2) /\ ClockMap.Equal (snd c1) (snd c2).
{% endhighlight %}

Cool.  Now, let's start proving things.  

First, let's prove that the merge operation commutes.  Similar to our
`G-Counter` proof, we can simply perform the necessary destructuring of
the `PN-Counter` type, and then apply the lemma we've previously defined
which shows that the merge operation on clocks commute.

{% highlight coq %}
Theorem PN_Counter_merge_comm : forall c1 c2,
  PN_Counter_equal (PN_Counter_merge c1 c2) (PN_Counter_merge c2 c1).
Proof.
  intros.
  unfold PN_Counter_equal.
  split; unfold ClockMap.Equal; intros;
    unfold PN_Counter_merge; unfold G_Counter_merge; simpl;
    repeat rewrite ClockMapFacts.map2_1bis; auto.
    apply Clock_merge_comm.
    apply Clock_merge_comm.
Qed.
{% endhighlight %}

Same thing, we can use the existing lemmas we've proven about
`G-Counter`'s and their merge operation, and apply it to prove that, in
addition to being commutative, they are also associative and idempotent.

{% highlight coq %}
Theorem PN_Counter_merge_idempotent : forall c1,
  PN_Counter_equal (PN_Counter_merge c1 c1) c1.
Proof.
  intros.
  unfold PN_Counter_equal.
  split; unfold ClockMap.Equal; intros;
    unfold PN_Counter_merge; unfold G_Counter_merge; simpl;
    repeat rewrite ClockMapFacts.map2_1bis; auto.
    apply Clock_merge_idempotent.
    apply Clock_merge_idempotent.
Qed.

Theorem PN_Counter_merge_assoc : forall c1 c2 c3,
  PN_Counter_equal
    (PN_Counter_merge c1 (PN_Counter_merge c2 c3))
    (PN_Counter_merge (PN_Counter_merge c1 c2) c3).
Proof.
  intros.
  unfold PN_Counter_equal.
  split; unfold ClockMap.Equal; intros;
    unfold PN_Counter_merge; unfold G_Counter_merge; simpl;
    repeat rewrite ClockMapFacts.map2_1bis; auto;
    repeat rewrite <- Clock_merge_assoc; reflexivity.
Qed.
{% endhighlight %}

Now, let's prove that both the increment and decrement operations
monotonically advance the structure.  This is extremely trivial, as
we've already proven that incrementing an individual `G-Counter`
monotonically advances the structure, given that the increment and
decrement are both only increment operations operating on either side of
a product.

{% highlight coq %}
Theorem PN_Counter_incr_mono : forall clocks actor,
  PN_Counter_compare clocks (PN_Counter_incr actor clocks).
Proof.
  intros; unfold PN_Counter_compare; unfold PN_Counter_incr; simpl.
  split. apply G_Counter_incr_mono.
  apply G_Counter_compare_idempotent.
Qed.

Theorem PN_Counter_decr_mono : forall clocks actor,
  PN_Counter_compare clocks (PN_Counter_decr actor clocks).
Proof.
  intros; unfold PN_Counter_compare; unfold PN_Counter_decr; simpl.
  split. apply G_Counter_compare_idempotent.
  apply G_Counter_incr_mono.
Qed.
{% endhighlight %}

Now, let's confirm that the merge operation of the `PN-Counter` cause
the data structure to monotonically advance.  This proof is trivial,
given we've already proven that this property holds true for the
`G-Counter`.  We can simply apply the theorem we've already defined.

{% highlight coq %}
Theorem PN_Counter_merge_mono : forall c1 c2,
  PN_Counter_compare c1 (PN_Counter_merge c1 c2).
Proof.
  intros; unfold PN_Counter_compare; unfold PN_Counter_merge; split; simpl;
  apply G_Counter_merge_mono.
Qed.
{% endhighlight %}

Qed.

[repo]: https://github.com/cmeiklejohn/distributed-data-structures
[previous]: http://christophermeiklejohn.com/coq/2013/06/11/distributed-data-structures.html
