---
layout: post
title:  "Verified Vector Clocks: An Experience Report, Part 3"
date:   2013-12-15 16:00:00 -0500
categories: coq erlang
group: vvclocks
---

_This post outlines a bunch of experimental work I've completed to model
data structures in Coq, leveraging Tim Carstens' [verlang][verlang]
project to extract the data structures into executable Erlang._

_Here's a link to the [first][first] and [second][second] posts in this
series._

_Updated March 8th, 2014: A full talk about this work was presented at Erlang
Factory, San Francisco 2014.  Both the
[slides](https://speakerdeck.com/cmeiklejohn/vector-clocks-in-coq-an-experience-report)
and [video](https://www.youtube.com/watch?v=IINmkv4izVQ) are available._

# Writing the wrapper

Let us now look at writing the wrapper module that we will use to
integrate our extracted code with the existing [`riak_core`][riak_core]
OTP application.

# Timestamps

We first need to make some adjustments to the generated Core Erlang
code, beacuse it's using the literal atoms `init_timestamp` and
`init_count` instead of calling to those functions, which is generating
data structures we can not operate over in Erlang.

We will make the following adjustments to the Core Erlang, as well as
add these new functions to the module exports.

{% highlight erlang %}
'init_timestamp'/0 = fun () ->
  'O'
'init_count'/0 = fun () ->
  'O'
{% endhighlight %}

We also modify the default case to call these functions, instead of just use the
atoms as values.

{% highlight erlang %}
'None' when 'true' ->
    [{ 'Pair'
    , _actor
    , { 'Pair'
    , call 'vvclock':'init_count' ()
    , call 'vvclock':'init_timestamp' ()
    }
    }|_vclock]
{% endhighlight %}

_You can see these changes [here](https://github.com/cmeiklejohn/vvclocks/commit/4d0b91cc3bec83bd59f4f69509b426878a9f1665)._

# Adding the original example test

Now, let's add back the original test from `riak_core`.

{% highlight erlang %}
%% @doc Riak Core example test.
riak_core_example_test() ->
    A = vclock:fresh(),
    B = vclock:fresh(),
    A1 = vclock:increment(a, A),
    B1 = vclock:increment(b, B),
    true = vclock:descends(A1,A),
    true = vclock:descends(B1,B),
    false = vclock:descends(A1,B1),
    A2 = vclock:increment(a, A1),
    C = vclock:merge([A2, B1]),
    C1 = vclock:increment(c, C),
    true = vclock:descends(C1, A2),
    true = vclock:descends(C1, B1),
    false = vclock:descends(B1, C1),
    false = vclock:descends(B1, A1),
    ok.
{% endhighlight %}

_I have added this back in the following [commit](https://github.com/cmeiklejohn/vvclocks/commit/db7117613eceed148cd61f5bbbda963c0dbcb20f)._

# The wrapper module

Now, let's take a look at the wrapper module.

Let's start by providing the same API as the existing `vclock` module.

{% highlight erlang %}
-module(vclock).
-export([fresh/0,
         descends/2,
         merge/1,
         get_counter/2,
         get_timestamp/2,
         increment/2,
         increment/3,
         all_nodes/1,
         equal/2,
         prune/3,
         timestamp/0]).
{% endhighlight %}

First, a function to return a timestamp as a natural number.

{% highlight erlang %}
%% @doc Return natural timestamp.
timestamp() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).
{% endhighlight %}

When generating a fresh vector clock, we can just call directly to our
exported code.

{% highlight erlang %}
%% @doc Generate a fresh vector clock.
fresh() ->
    vvclock:fresh().
{% endhighlight %}

When incrementing, since actors can be any Erlang term, we first need to
convert that term into a Peano number, and then call the generated
increment function.

{% highlight erlang %}
%% @doc Increment a vector clock for a particular actor.
increment(Actor, VClock) ->
    vvclock:increment(term_to_peano(Actor), VClock).
{% endhighlight %}

We're going to skip the `increment/3` call which takes a timestamp.

{% highlight erlang %}
%% @doc Increment a vector clock with a particular timestamp.
increment(_Actor, _Timestamp, _VClock) ->
    %% Not implementing this one for now.
    erlang:error(deprecated).
{% endhighlight %}

When using the accessor functions, we need to convert the actor to a
Peano number, and then convert the count back to an Erlang term.

{% highlight erlang %}
%% @doc Return counter for a particular actor.
get_counter(Actor, VClock) ->
    Counter = vvclock:get_counter(term_to_peano(Actor), VClock),
    peano_to_term(Counter).
{% endhighlight %}

Same thing applies for the timestamps.

{% highlight erlang %}
%% @doc Return timestamp for a particular actor.
get_timestamp(Actor, VClock) ->
    Timestamp = vvclock:get_timestamp(term_to_peano(Actor), VClock),
    peano_to_term(Timestamp).
{% endhighlight %}

When handle descends or equality, we need to match against the extracted
constructors for the True and False types.

{% highlight erlang %}
%% @doc Determine if one vector clock is an ancestor of another.
descends(VClock1, VClock2) ->
    case vvclock:descends(VClock1, VClock2) of
        'True' ->
            true;
        'False' ->
            false
    end.
{% endhighlight %}

{% highlight erlang %}
%% @doc Compare equality of two vclocks.
equal(VClock1, VClock2) ->
    case vvclock:equal(VClock1, VClock2) of
        'True' ->
            true;
        'False' ->
            false
    end.
{% endhighlight %}

Again, when dealing with the actors, which are stored as Peano numbers,
we convert back and forth between Erlang terms.

{% highlight erlang %}
%% @doc Return list of all actors that have ever touched the vclock.
all_nodes(VClock) ->
    [peano_to_term(Node) || Node <- vvclock:all_nodes(VClock)].
{% endhighlight %}

When pruning, we need to extract items out of the application
environment, and pass them directly into the Core Erlang code that's
been extracted so it can operate over them.

{% highlight erlang %}
%% @doc Prune vclocks.
prune(VClock, _Timestamp, BProps) ->
    Old =   term_to_peano(get_property(old_vclock, BProps)),
    Young = term_to_peano(get_property(young_vclock, BProps)),
    Large = term_to_peano(get_property(large_vclock, BProps)),
    Small = term_to_peano(get_property(small_vclock, BProps)),
    vvclock:prune(VClock, Small, Large, Young, Old).
{% endhighlight %}

With the merge function, we simply adapt the single arity list call to
recurse through the list and perform the merges using the extracted
merge.

{% highlight erlang %}
%% @doc Merge function which operates on a list of vector clocks.
merge([VClock1,VClock2|VClocks]) ->
    merge([vvclock:merge(VClock1, VClock2)|VClocks]);
merge([VClock]) ->
    VClock;
merge([]) ->
    [].
{% endhighlight %}

Cool.

# The timestamp callback

Finally, we need to modify our Core Erlang code to call back into this
wrapper module to generate proper Erlang timestamps needed when
incrementing vector clocks.  We support that by exporting a function
only used by the Core Erlang, which returns a current timestamp, in
Peano format.

{% highlight erlang %}
%% @doc Peanoized timestamp.
peano_timestamp() ->
  term_to_peano(timestamp()).
{% endhighlight %}

Then, we modify our callers in the Core Erlang to use that function in
our support wrapper.

{% highlight erlang %}
'init_timestamp'/0 = fun () ->
  call 'vclock':'peano_timestamp' ()
'incr_timestamp'/1 = fun (_timestamp) -> 
  call 'vclock':'peano_timestamp' ()
'init_count'/0 = fun () ->
  call 'vclock':'peano_timestamp' ()
'incr_count'/1 = fun (_count) -> 
  call 'vclock':'peano_timestamp' ()
{% endhighlight %}

Seems straightforward.

# Support functions

In supporting data type conversion, it gets a bit more interesting.

For example, writing the functions to perform the natural to Peano
conversion is trivial, but extremely slow in the way that data
constructors are modeled in the extracted Erlang code as nested tupes.

Here's the function to convert to a Peano.

{% highlight erlang %}
%% @doc Convert a natural number into a peano.
natural_to_peano(0) ->
    'O';
natural_to_peano(Natural) ->
    {'S', natural_to_peano(Natural - 1)}.
{% endhighlight %}

...and here's the function to convert from a Peano.

{% highlight erlang %}
%% @doc Convert a peano number into a natural.
peano_to_natural('O') ->
    0;
peano_to_natural({'S', Peano}) ->
    1 + Peano.
{% endhighlight %}

Granted, these could be converted to tail calls for efficiency, but most
the problems I ran into was the sheer amount of execution time required
to convert a large integer to a Peano number.

Now, we look at how we can handle conversion of arbitrary terms to
integers.

In the version used to evaluate the test suite, I've stubbed these
functions out the following way to verfiy that the rest of the code
executed correctly.

For example, converting a natural to a term...

{% highlight erlang %}
%% @doc Convert a natural number to an erlang term.
natural_to_term(Natural) ->
    case Natural of
        1 ->
            a;
        2 ->
            b;
        3 ->
            c;
        4 ->
            d;
        _ ->
            erlang:error(not_supported)
    end.
{% endhighlight %}

...and converting a term to a natural.

{% highlight erlang %}
%% @doc Convert an erlang term to a natural number.
term_to_natural(Term) ->
    case Term of
        a ->
            1;
        b ->
            2;
        c ->
            3;
        d ->
            4;
        _ ->
            erlang:error(not_supported)
    end.
{% endhighlight %}

I'm not sure what the best way is to convert a term in Erlang to an
integer representation without hashing the binary representation if it.
However, this yields extremely large integers, which when converted
to Peano numbers take _extremely_ long to complete.

For example, in one run it took multiple minutes just to perform the
Peano number conversion for an extremely small integer.

I believe the next step in moving forward is to rework the extraction
code to use some sort of String to model the actors, to eliminate the
need for Peano numbers there.  However, we still run into a similar
problem when modeling timestamps as Peano numbers, so an optimized
representation or conversion mechanism is probably desired.

_The wrapper has been added in the following [commit](https://github.com/cmeiklejohn/vvclocks/commit/1d2dd68077e977c463b2344c56ad724f73e6a222)._

# Conclusion

At this point, we have gotten a bit further, and now have a version of
the library that can work directly as a replacement for `riak_core`'s
`vclock` module.  However, the performance is abysmal due to the data
structure modeling, which will be the topic of the next post in this
series.

Thanks!

[verlang]: https://github.com/tcarstens/verlang
[riak_core]: https://github.com/basho/riak_core
[hard]: http://basho.com/why-vector-clocks-are-hard/
[first]: http://christophermeiklejohn.com/coq/erlang/2013/11/04/verified-vector-clocks-an-experience-report-part-1.html
[second]: http://christophermeiklejohn.com/coq/erlang/2013/11/19/verified-vector-clocks-an-experience-report-part-2.html
