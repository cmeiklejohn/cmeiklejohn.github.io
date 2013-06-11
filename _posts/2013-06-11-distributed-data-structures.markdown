---
layout: post
title:  "Distributed data structures with Coq"
date:   2013-06-11 12:17:10
categories: coq
---

A few weeks ago, I gave a lightning talk at [Basho's][basho] distributed
systems conference in New York, [RICON|East][ricon] focused around the
modeling and provability of two types of CRDT's as outlined in the
Shapiro et. al. work in [A Comprehensive Study of Convergent and
Commutative Replicated Data Types][shapiro].  The [video][talk] is
finally available online and the source code is available on
[GitHub][repo].  The repo also contains some basic modeling of some of
Conway's work outlined in [Logic and Lattices for Distributed
Programming][conway].

We'll briefly look at the grow-only counter, which is very similar to a
vector clock.  Since understanding the clock data structure is required
to begin proving facts about vectors of them, let's dig into clocks
first.

# Clocks

Let's first define a map of clocks as a finite map of natural numbers.

{% highlight coq %}
Module ClockMap := FMapWeakList.Make (Nat_as_Legacy_OT).
Module ClockMapFacts := FMapFacts.Facts (ClockMap).
{% endhighlight %}

Let's define two functions for working with clocks.  Remember, that
functions need to be totally ordered to ensure termination.  The find
function for working with maps is going to return an option nat, so we
author our pattern matches accordingly.

{% highlight coq %}
Definition Clock_merge (n1 n2 : option nat) :=
  match n1, n2 with
    | None, None => None
    | Some n, None => Some n
    | None, Some n => Some n
    | Some n1', Some n2' => Some (max n1' n2')
  end.

Definition Clock_compare (n1 n2 : option nat) :=
  match n1, n2 with
    | None, None => None
    | Some n, None => Some false
    | None, Some n => Some true
    | Some n1', Some n2' => Some (leb n1' n2')
  end.
{% endhighlight %}

Now, we can begin proving certain properties about clocks.  First, we
can prove that merge is commutative because the `max` operation is
commutative over natural numbers.  We simply destructure the clock
functions, simplify the terms and apply the existing lemma.

{% highlight coq %}
Lemma Clock_merge_comm : forall n1 n2, 
  Clock_merge n1 n2 = Clock_merge n2 n1.
Proof.
  intros. destruct n1; destruct n2; auto.
  simpl. f_equal. apply Max.max_comm.
Qed.
{% endhighlight %}

In the same way, we can prove that the merging of two clocks are
idempotent and associative for similar reasons; both hold true beacuse
the max operation is both associative and idempotent.

{% highlight coq %}
Lemma Clock_merge_idempotent : forall n1, 
  Clock_merge n1 n1 = n1.
Proof.
  intros. destruct n1; auto; simpl.
  f_equal. apply Max.max_idempotent.
Qed.

Lemma Clock_merge_assoc : forall n1 n2 n3,
  Clock_merge n1 (Clock_merge n2 n3) = Clock_merge (Clock_merge n1 n2) n3.
Proof.
  intros. destruct n1; destruct n2; destruct n3; auto.
  unfold Clock_merge. f_equal. apply Max.max_assoc.
Qed.
{% endhighlight %}

Finally, we can define a proof which will help us later on that shows
that comparing a clock to itself always returns true (specifically
because less than or equal to is a partial ordering over natural
numbers).  This will help us later when attempting to prove certain
properties about G-Counters monotonically advancing.

{% highlight coq %}
Definition Clock_true (n1 n2 : option nat) :=
  match n1, n2 with
    | None, None => None
    | Some n, None => Some true
    | None, Some n => Some true
    | Some n1', Some n2' => Some true
  end.

Lemma Clock_compare_refl : forall x,
  Clock_compare x x = Clock_true x x.
Proof.
  intros; destruct x; auto. unfold Clock_compare. unfold Clock_true.
  destruct n; auto. f_equal. simpl. rewrite leb_correct; auto.
Qed.
{% endhighlight %}

Boom.

# G-Counters

A `G-Counter` represents a grow-only counter, and is modeled similarly
to a vector clock.  A series of actor/increment pairs are stored in a
vector, and the sum of the counter is derived by a `fold` across the
counters.

First, let's define a `G-Counter` as a `ClockMap`, as defind above, with
the values of the map also as natural numbers.  Then, we define a
function to initialize an empty `G-Counter`

{% highlight coq %}
Definition G_Counter := ClockMap.t nat.
Definition G_Counter_init : G_Counter := ClockMap.empty nat.
{% endhighlight %}

Then, let's define a function for incrementing a particular actor in a
map of clocks.  This is simply done by pattern matching the find, and
calling the successor function on the existing actor's value if it's
present.

{% highlight coq %}
Definition G_Counter_incr actor clocks :=
  match ClockMap.find actor clocks with
    | None => ClockMap.add actor 1 clocks
    | Some count => (ClockMap.add actor (S count) clocks)
  end.
{% endhighlight %}

Then, we define a function to reveal the current value of the counter by
performing a `fold` over the values and perfoming a sum.

{% highlight coq %}
Definition G_Counter_reveal clocks :=
  ClockMap.fold (fun key elt acc => (plus acc elt)) clocks 0.
{% endhighlight %}

To merge, we can just use `map2` with our existing merge function.

{% highlight coq %}
Definition G_Counter_merge c1 c2 :=
  ClockMap.map2 Clock_merge c1 c2.
{% endhighlight %}

To test equality, we can use the built-in equality test for maps.

{% highlight coq %}
Definition G_Counter_equal (c1 c2 : G_Counter) :=
  ClockMap.Equal c1 c2.
{% endhighlight %}

Finally, to compare, we can apply the comparsion function to the two
maps of clocks, and then test equality against a map of true values of
the same length, which will allow us to test of one map is a partial
ordering of another map.

{% highlight coq %}
Definition G_Counter_compare (c1 c2 : G_Counter) :=
  ClockMap.Equal
    (ClockMap.map2 Clock_compare c1 c2) (ClockMap.map2 Clock_true c1 c2).
{% endhighlight %}

So, now we can begin proving aspects about `G-Counters`.

First, let's prove that merge is commutative.

Given that we've already proven that merging two clocks is commutative,
we simply need to destructure the `G-Counter` to a point where we can
apply the previously defined lemma.

{% highlight coq %}
Theorem G_Counter_merge_comm : forall c1 c2,
  G_Counter_equal (G_Counter_merge c1 c2) (G_Counter_merge c2 c1).
Proof.
  intros; unfold G_Counter_merge.
  unfold ClockMap.Equal; intro.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  apply Clock_merge_comm.
Qed.
{% endhighlight %}

Similarly, we can also prove that the merge operation is idempotent and
associative, as we've already proven properties about these operations
over clocks. 

{% highlight coq %}
Theorem G_Counter_merge_idempotent : forall clocks,
  G_Counter_equal (G_Counter_merge clocks clocks) clocks.
Proof.
  intros; unfold G_Counter_merge.
  unfold ClockMap.Equal; intro.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  apply Clock_merge_idempotent.
Qed.

Theorem G_Counter_merge_assoc : forall c1 c2 c3,
  G_Counter_equal
    (G_Counter_merge c1 (G_Counter_merge c2 c3))
    (G_Counter_merge (G_Counter_merge c1 c2) c3).
Proof.
  intros; unfold G_Counter_merge.
  unfold ClockMap.Equal; intro.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  repeat rewrite <- Clock_merge_assoc; reflexivity.
Qed.
{% endhighlight %}

Next, we can prove that the increment function monotonically advances
the data structure by using our comparsion function between an
pre- and post-incremented data structure.  

This proof gets a little bit more complex, having to prove facts about
incrementing a value when its present vs. not present in an existing map
of clocks.  However, again, using the facts we've already proven over
clocks, we can simply apply our existing lemmas.

{% highlight coq %}
Theorem G_Counter_incr_mono : forall clocks actor,
  G_Counter_compare clocks (G_Counter_incr actor clocks).
Proof.
  intros; unfold G_Counter_compare; unfold ClockMap.Equal; intro.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  elim (eq_nat_dec actor y); intro. 
    subst. unfold Clock_compare, Clock_true. unfold G_Counter_incr. simpl.
    destruct (ClockMap.find y clocks).
      rewrite ClockMapFacts.add_eq_o. f_equal.
        induction n; auto. reflexivity. reflexivity.
      unfold G_Counter_incr.
      destruct (ClockMap.find actor clocks) eqn:factor.
        rewrite ClockMapFacts.add_neq_o; auto. apply Clock_compare_refl.
        rewrite ClockMapFacts.add_neq_o; auto. apply Clock_compare_refl.
Qed.
{% endhighlight %}

Finally, we can prove that the merge operation monotonically advances
the data structure, by again comparing the pre- and post-merged data
structures.  Since our comparsion function operates over natual numbers,
using the less than or equal to operator, we can simply destructure the
merge operation, and apply a lemma showing that max and less than or
equal to, preserve partial ordering.

{% highlight coq %}
Lemma leb_max_mono : forall n m,
  leb n (max n m) = true.
Proof.
  intros.
  generalize dependent m.
  induction n; induction m; auto with arith; simpl.
  rewrite leb_correct; auto. rewrite IHn; reflexivity.
Qed.

Theorem G_Counter_merge_mono : forall c1 c2,
  G_Counter_compare c1 (G_Counter_merge c1 c2).
Proof.
  intros; unfold G_Counter_compare.
  unfold ClockMap.Equal; intro.
  unfold Clock_compare, Clock_true, G_Counter_merge.
  repeat rewrite ClockMapFacts.map2_1bis; auto.
  destruct (ClockMap.find y c1);
    destruct (ClockMap.find y c2); simpl; f_equal.
      apply leb_max_mono.
      rewrite leb_correct; auto.
Qed.
{% endhighlight %}

Boom.

# Conclusion

I haven't been able to finish a proper proof of partial ordering
preservation for `G-Counters` yet, but I'm currently actively working on
it.  I hope to address both that, and `PN-Counters` in a subsequent blog
post.

Feedback is encouraged!

# Qed.

[basho]: http://basho.com
[ricon]: http://ricon.io/archive/2013/east.html
[repo]: https://github.com/cmeiklejohn/distributed-data-structures
[talk]: http://www.youtube.com/watch?v=3RJ24YSiKTI&t=36m15s
[shapiro]: http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf
[conway]: http://db.cs.berkeley.edu/papers/UCB-lattice-tr.pdf
