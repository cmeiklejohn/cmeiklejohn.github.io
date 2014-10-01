---
layout: post
title:  "SyncFree Consortium / Programming Models, Part 2: QuickChecking Derflow"
date:   2014-10-01 10:20:10 -0500
categories: derflow erlang
---

_As we discussed in our first [post][part1], [Derflow][derflow] is the
name of our distributed deterministic programming model that is the
basis of our research into providing a more expressive way of working
with CRDTs and eventual consistency._

Today, we look at how we can go about building the first version of a
Erlang QuickCheck model for testing our distributed variable store works
as expected.

# Modularization

So, how do we start building a model for our language to QuickCheck
with?

First, if you recall, our language uses a distributed single assignment
store in which distribution is supported by `riak_core`.  Unfortunately,
testing applications with dependencies on `riak_core` is extremely
difficult, due to the amount of setup required.  We've attempted it
before at Basho and it requires a [lot][ec_eqc] of [setup][put_fsm_eqc].

To make this easier, we first separate out the logic in our virtual node
which controls the distributed store with the backend that actually
writes values to disk.  For this purpose, we've created a
[derflow_ets][derflow_ets] backend, which provides an API to the
distributed store, which we can run our tests against.

# Building the Model

Building the model is straightforward: how can you model a variable
store that is backed by `ets`?  We can use a `dict`!

So, what are the invarients of our langauge?  Let's enumerate them.

If you recall, our language provides operations over both
single-assignment variables and lattices.  When operating with a
lattice, we want to ensure that we allow variables to rebind if the
given value is an inflation of that lattice.

So, we want to test the following operations:

## Single-assignment store

What are the properties of our single-assignment store?

* We can declare variables as single-assignment variables.
* We can bind these variables to values.
* We can re-bind these variables to the same value, but not a different value.
* We can read anything that's been bound and get the correct value.

## Lattice store

What are the properties of our lattice store?

* We can declare lattice variables.
* We can bind these variables to values.
* We can rebind these variables to a new value, if it is an inflation of that lattice.
* We can perform a threshold read which returns the threshold value supplied.

# Stateful testing

For testing, we use the `eqc_statem` behavior, which is a state-machine
based approach for testing functions with side-effects.  For more
information on testing using this facility, check out this
[talk][statem] from Laura M. Castro.

## Generating commands

Let's take a look at our code that generates commands:

{% highlight erlang %}
    Variables = dict:fetch_keys(Store),
    oneof(
        [{call, ?MODULE, declare,
          [oneof([elements(Types), undefined]), Ets]}] ++
        [?LET({Variable, GeneratedValue}, {elements(Variables), nat()},
             begin
                    Value = value(Variable, Store, GeneratedValue),
                    Threshold = threshold(Variable, Store),
                    oneof([{call, ?MODULE, bind, [Variable, Value, Ets]},
                           {call, ?MODULE, read, [Variable, Threshold, Ets]}])
                end) || length(Variables) > 0]).
{% endhighlight %}

When generating commands, we need to model our three basic types of
operations: `read`, `bind`, and `declare`.  We use the `oneof` generator
to select one of the following commands -- `declare` operations will be
generated without constraints composed of one of the lattice types, or
the `undefined` type to specify that we want a single-assignment
variable.

For `read` and `bind` operations: we need to ensure we only generate
these for variables which have been declared; this is done by the
`length(Variables) > 0` predicate on the list comprehension.

Additionally, these commands also need to be called with the correct
types of values.  For example, a read threshold over a lattice needs to
be called with a valid lattice value; similarly a bind needs to be
triggered with the correct type as well.  In our example here, we use
the `?LET` macro to materialize the symbolic into an actual value, which
we can inspect the type of and generate the appropriate threshold and
value.

Here's our threshold and value generators, which currently, are the same:

{% highlight erlang %}
%% @doc Generate values for threshold, based on operating type.
threshold(Variable, Store) ->
    case dict:find(Variable, Store) of
        {ok, #variable{type=undefined}} ->
            undefined;
        {ok, #variable{type=Type}} ->
            ?LET({Object, Update},
                 {Type:new(), Type:gen_op()},
                  begin
                    {ok, X} = Type:update(Update, undefined, Object),
                    X
                  end)
    end.

%% @doc Generate values for binds, based on operating type.
value(Variable, Store, DefaultValue) ->
    case dict:find(Variable, Store) of
        {ok, #variable{type=undefined}} ->
            DefaultValue;
        {ok, #variable{type=Type}} ->
            ?LET({Object, Update},
                 {Type:new(), Type:gen_op()},
                  begin
                    {ok, X} = Type:update(Update, undefined, Object),
                    X
                  end)
    end.
{% endhighlight %}

In this case, the type of one of the callers will be a CRDT provided by
the [riak_dt][riak_dt] library.  Each of the data types provided, such
as `riak_dt_gset`, for the grow-only set, exports a function `gen_op()`,
which generates a random operation, allowing us to generate random types
of CRDTs.

Here's an example of the operations generator for the `riak_dt_gset`:

{% highlight erlang %}
gen_op() ->
    oneof([{add, int()},
           {add_all, non_empty(list(int()))}]).
{% endhighlight %}

Now, let's examine the preconditions on command generation.

# Preconditions

What preconditions do we want to check when running our model?

Well, given that derflow `read` operations are blocking -- blocking on
unbound single-assignment variables and threshold read operations where
the threshold is not yet met, we want to prevent us from executing the
tests with those values.

We do that with the following precondition on read:

{% highlight erlang %}
precondition(#state{store=Store},
             {call, ?MODULE, read, [Id, Threshold, _Store]}) ->
    case dict:find(Id, Store) of
        error ->
            %% Not declared.
            false;
        {ok, #variable{value=undefined}} ->
            %% Not bound.
            false;
        {ok, #variable{type=undefined}} ->
            true;
        {ok, #variable{value=Value, type=Type}} ->
            case derflow_ets:threshold_met(Type, Value, Threshold) of
                true ->
                    true;
                false ->
                    false
            end
    end;
{% endhighlight %}

Now, let's examine the postconditions on command generation.

# Postconditions

There are two important postconditions of our model we must verify:
`bind` operations which fail should only fail for a valid reason;
`read` operations which succeed must succeed with the correct value.

When performing a `read`, we assert that we only ever read the value
that we observed being bound to that variable when executing the model.
If it is a threshold read, we assert that the value returned is the
threshold -- if the value wasn't at least the threshold, we would have
failed the precondition shown above.

{% highlight erlang %}
%% Ensure we always read values that we are expecting.
postcondition(#state{store=Store},
              {call, ?MODULE, read, [Id, Threshold, _]}, {ok, V, _}) ->
    case dict:find(Id, Store) of
        {ok, #variable{value=Value}} ->
            case Threshold of
                undefined ->
                    Value == V;
                _ ->
                    Threshold == V
            end;
        _ ->
            false
    end;
{% endhighlight %}

When asserting that `bind` fails only when it is supposed to, we verify
that the only time it is allowed to fail is if we are attempting to
re-assign a already bound variable to a new value.

{% highlight erlang %}
%% If a bind failed, that's only allowed if the variable is already
%% bound or undefined.
%%
postcondition(#state{store=Store},
              {call, ?MODULE, bind, [Id, V, _]}, error) ->
    case dict:find(Id, Store) of
        {ok, #variable{type=Type, value=undefined}} ->
            false;
        {ok, #variable{type=Type, value=Value}} ->
            case derflow_ets:is_inflation(Type, Value, V) of
                true ->
                    false;
                false ->
                    true
            end
    end;
{% endhighlight %}

True.

# Conclusion

This was just a very brief overview of how we began testing derflow
using Erlang QuickCheck.  For a deeper view, please checkout our [test]
[derflow_ets_eqc] on GitHub.

Thanks!

[derflow_ets_eqc]: https://github.com/cmeiklejohn/derflow/blob/master/test/derflow_ets_eqc.erl
[riak_dt]: https://github.com/basho/riak_dt
[statem]: http://www.slideshare.net/lauramcastro/testing-data-consistency-of-dataintensive-applications-using-quickcheck
[part1]: http://christophermeiklejohn.com/derflow/erlang/2014/09/28/try-derflow.html
[derflow]: https://github.com/cmeiklejohn/derflow
[ec_eqc]: https://github.com/basho/riak_kv/blob/develop/test/ec_eqc.erl
[put_fsm_eqc]: https://github.com/basho/riak_kv/blob/develop/test/put_fsm_eqc.erl
[derflow_ets]: https://github.com/cmeiklejohn/derflow/blob/master/src/derflow_ets.erl
