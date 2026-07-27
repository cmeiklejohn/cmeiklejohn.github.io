---
layout: post
title:  "One Writer"
subtitle: "Our tools assume one writer, and assume that writer is a human.  Nothing computes what a change reads and writes at runtime, so the only known fix is brute force priced for organizations."
date:   2026-07-26 22:00:00 -0400
group: ai
categories: ai zabriskie agents reliability testing ci distributed
---

_In this blog post, I discuss three days in July 2026 when a single agent session ran away from me, and what those three days revealed about the concurrency assumptions buried in our development tooling._

Six weeks ago I wrote [The Test Suite Was the Incident]({% post_url 2026-06-10-the-test-suite-was-the-incident %}): my test suite had grown a pile of shared data nobody owned, every pull request paid to rebuild it, and the resulting failures had nothing to do with the changes under review.  That cost me about $180 in one night.

I got a worse one.  It lasted three days, and in one twenty-four-hour stretch of it I burned through an entire Codex 20x max plan.

This post is not really about that, though.  It is about a property of our tooling that the three days made impossible to ignore.

**Nearly every layer of this assumes one writer, and assumes that writer is a human.**  Git hands you a conflict and waits.  Code review assumes somebody reads.  A migration sequence assumes somebody is assigning the order.

Each of those protocols terminates in a person.  That is fine while there is exactly one, and while they are, in fact, a person.

The agent runtime turns out to be on that list too, which I did not expect.  It spawned eighty-four workers into a single checkout without being able to say what any one of them would read or write, and that is the same question git can't answer about a diff.

That assumption was invisible for forty years because nothing ever bound it at my scale.  Agents break both halves at once: there are many of them, and not one of them is the person the protocol was waiting for.  Git is the partial exception, and I will come to why the exception does not help.

Nothing in the stack detects the violation when it happens.  It gets caught later, somewhere else, attributed to the wrong change, and paid for at full price.

Some context for readers arriving fresh.  [Zabriskie](https://zabriskie.app) is a social app for live-music fans, and it's also a deliberate experiment: I'm building a real, deployed, actually-used application almost entirely with AI agents (agents that wrote the features, agents that wrote the tests guarding those features, and agents that now open most of the pull requests), in order to find out what that's like and, more usefully, where it breaks.  I've written almost none of the code.

That framing matters, because several things below look like obvious mistakes and are.  I let agents design a migration scheme with only another agent reviewing it, I stopped reading most diffs, and I let sixty-four pull requests go up in a single day, all of which a careful engineer would tell you not to do and would be right about.  But the point of running an experiment at the extreme is to find the walls.  That week I found several at once.

A _migration_, throughout, is a versioned SQL file that changes the database schema.  _CI_ is the automated checking that runs on every proposed change: build the app, spin up a fresh database, run the tests.

Here is the shape of the three days.  Treat these numbers as texture, not as evidence, for a reason I will get to.

| | 24 Jul | 25 Jul | 26 Jul |
| --- | ---: | ---: | ---: |
| pull requests opened | 19 | 64 | 28 |
| pull requests merged | 20 | 50 | 30 |
| incidents logged | 0 | 6 | 301 |

Days in that table are UTC; clock times in the narrative below are Eastern (where I was).  The burst ran past midnight UTC: 301 incidents on the 26th plus 60 more before 1 AM on the 27th, so 361 for the burst.  Every incident count below is scoped to that window, and 253 rows is the total before 24 July.

I offer that comparison as a sense of scale and not as a baseline, for the following reason.  Those incidents exist because a standing instruction tells agents to log their own mistakes, and ten minutes into the worst night I tightened that instruction.  The log therefore measures _reported_ failures.

Look at the daily series and it gets worse: there are days that week with ten and twenty merged pull requests and zero logged incidents, which at any real failure rate means nobody was logging rather than nothing broke.  In short, I can't give you a trustworthy baseline.  What follows rests on mechanism and on a few dated, checkable events, not on 361.

## The Session

Late Saturday night the queue jammed.  Sixty-four pull requests had gone up that day, `main` was red, and nineteen open pull requests were stuck behind a suite that could not tell me which of them was broken.  At 12:31 AM I opened a session with [Codex](https://platform.openai.com/docs/codex) and complained that CI was wasting too much money.

Codex read that and hired a workforce.

Over seventeen hours that session made 81 spawn calls, producing 74 direct children; those children spawned 10 more, for 84 threads.  Then 358 calls waiting on them, 96 listing them, 85 sending follow-up work, and 13 interrupting them, which comes to six hundred and thirty-three tool calls of pure management overhead (spawn, wait, list, follow up, interrupt) against 54 messages from me over the same seventeen hours.  All 84 ran in the same checked-out copy of the repository.

Trivial parallelization, at scale, with a coordinator that only managed agents, on a toolchain built for one writer.  The rest of this post is about why that combination is so much worse than it sounds.

I asked afterward why it had spawned anything, since I never requested it.  A language model asked why it did something produces plausible narrative, not introspection, and I apply that same skepticism to the self-reported incidents above.

So the quotes below are not evidence, and I'm not offering them as any.  I had already reached the same conclusion from the tool-call counts before I asked.  I print them because they state that conclusion more plainly than I did, and because there is something worth looking at in a system that can describe the failure this precisely and could not avoid it.

> I chose to spawn multiple agents even though you did not ask me to. The environment permitted proactive delegation, but that was permission, not a requirement. I treated the availability of agents as a reason to use them instead of first asking whether they would reduce time, cost, or risk.

It then diagnosed why the task was a bad candidate:

> A PR queue is largely serial because every merge changes `main`, which changes the integration state of every remaining PR. The work needed one authoritative coordinator moving through the queue in order. Parallel agents could not independently merge overlapping PRs without continuously invalidating one another's assumptions.

On what those 358 waits were doing:

> The agents' outputs created additional coordination work for me. I had to read their findings, compare conflicting recommendations, inspect supporting evidence, and decide how to combine them. The agents therefore generated work for the coordinator instead of reliably removing work from the critical path.

A system with no notion of its own concurrency was handed a concurrency primitive and used it, reasonably, on a task the substrate could not support.  What that substrate is, and why nothing in it objected, is the rest of this post.

One number deserves care.  I do not know how many of those 84 threads _wrote_ to the checkout instead of reading it; from Codex's own account many were investigating.  A reader is harmless and a writer is not, and nothing in the toolchain drew the distinction or could.  That's the same missing primitive one level up: not even the agent runtime knew which of its children were writers.

## The Substrate

Start with the layer that did anticipate this, because it is the one people reach for.  Branches are optimistic concurrency control; worktrees go further, giving each writer a physically separate checkout so that two agents can hold two versions of the tree at once.  That works.

However, git's isolation stops at the edge of the source tree.  A worktree gives an agent its own files.  It doesn't give it its own database, port range, mock server, or position in the migration sequence.  Everything below the filesystem is shared and singular.

That assumption was invisible to _me_ because it never bound me.  It has bound large organizations for a very long time, however, and over the last two decades they built the response to it: merge queues, hermetic builds, database-per-test, trunk-based development, automated culprit-finding, and whole infrastructure teams whose only job is to keep the thing moving.

So what is new here isn't the concurrency.  **It's that a solo developer now operates in the regime that used to require an infrastructure organization, with none of the infrastructure and no headcount to build it.  Agents removed the cap on my arrival rate.  They didn't hand me Google's build system.**

Partial isolation is then its own trap.  A worktree gives you the _feeling_ of a private workspace (clean tree, own branch, no file collisions), and it reads as properly parallel right up until two private workspaces write the same database row.  Nothing errors.  Nothing warns.  The contention surfaces later, somewhere else, as a red X on an unrelated pull request.

Worse, the isolation _below_ git is advisory, and agents have to choose it.  Mine routinely don't.  One incident reads _"PR 1868 isolated E2E attempt fell back to shared ports"_: the agent tried to isolate its test environment, isolation failed, and nothing stopped the run.

Worse still, a worktree is cut from a commit and stays there, so it's isolated _in the past_ and decays with every merge.  When I changed one of the agent gates in July (they're tracked scripts in the repository, not `.git/hooks`, so every worktree carries its own copy pinned to the commit it was cut from) the fix merged at 1:48 AM, and four hours later 73 of 74 worktrees were still running the old one.  A stale worktree can't detect its own staleness, so it goes green about a world that no longer exists, and the error is deferred to the only actor holding current `main`, which is CI.

### Convergence and Invariant Preservation

One framing before the specifics, because it unifies them.  Every merge mechanism here is built to _terminate_, and none of them is built to _preserve invariants_.

Git is honest about the cases it cannot decide: on a textual conflict it halts and asks you.  The trouble is the far more common case, where it doesn't halt, produces a merge confidently, and the invariant it never knew about is now false.

That distinction is the oldest lesson in the replicated-data literature, and it has a canonical counterexample.  Take a replicated map where each field merges independently under its own perfectly reasonable rule.  One field holds a person's name; another holds the length of that name.  Two replicas concurrently write different names.

Each field converges exactly as specified, and the result is a record whose `name` came from one replica and whose `length` came from the other, with the invariant tying them now false.  Nothing merged incorrectly.  The composition of correct local merges is simply not a correct global merge.

Closing that gap is the point of work like Balegas and colleagues' [Indigo](https://www.dpss.inesc-id.pt/~rodrigo/indigo_eurosys15.pdf), which enforces application invariants over eventually consistent stores, and which I have [written about before]({% post_url 2018-11-14-ipa %}).

Some of what follows has that shape.  The first case does not, and I will not dress it up.

### The Sealed Prefix

This project has 1,388 migrations, and rebuilding a test database from all of them on each of eight parallel test machines is as slow as it sounds.  So, in response to my complaining for weeks that CI was too expensive, an agent froze a database snapshot into the repository for CI to restore, applying only what came after.

It's a good optimization: it saves 13 to 18 minutes of machine time per run.  It merged at 5:01 AM Eastern on the third day.

Fifty-four minutes later the first pull request failed, because its migration no longer sorted after the newly frozen prefix.  Then another.  By late morning a single incident covers four at once: _"Four queued PRs carried migrations older than the sealed CI baseline suffix."_

The diagnosis here is ordinary, and a critic will say so.  Other agents had already opened pull requests that appended migrations; one agent then sealed a new prefix underneath them; those two streams of work conflicted; and nothing anywhere in the system noticed until CI rejected the queued pull requests several hours later.  That's parallel work colliding and finding out late, not a subtle invariant bug.

Caching a prefix of an ordered log made the collision loud and retroactive, but it did not invent the collision.  So the loop closes: I complained CI was expensive, an agent made CI cheaper, and the mechanism became a new source of CI failures against the whole queue.

### Merge Functions and Runtime State

Git detects conflicts over lines of text; my conflicts live in shared runtime state.  From that week: _"Song-call E2E suites deleted each other's shared pending call."_

Two test files, no overlapping lines, both green alone, and the row one of them depends on is the row the other deletes.  The collision is in the database at runtime, not in the diff.

Those two files are the `name` and the `length`.  They share no lines, each merges cleanly, each is individually green, and the invariant binding them is false the moment they land together.  No improvement to git's merge algorithm catches that, because the property is not a property of any file: it is a property of the composition, and git has no representation of the composition to check.

### Verification Under Composition

Which gives the sharpest version of the problem: **green(A on base) and green(B on base) does not imply green(merge of A and B).**

The test suite is the only thing in my pipeline that checks the invariant at all, since git checks text and timestamps check ordering.  So the tests are my invariant checker.

I do run them on the composition.  Pull requests rebase onto current `main` and merge in sequence, each one retested before it lands, and `main` itself is checked after land.  That catches the break.  It just catches it late, and only by paying the serial tax: rebase, retest, merge, next.  That tax is exactly the queue-wide cost the sealed prefix made visible on the morning when one seal invalidated everything queued behind it.

Serialization is the honest answer, and I already pay it.

Speculative merge queues try to buy the same guarantee back with parallelism: build the candidate futures, `main+A`, `main+A+B`, `main+A+B+C`, test them concurrently, and discard a failure from the middle.  OpenStack's [Zuul](https://zuul-ci.org/) has gated that way since 2012; [GitHub's merge queue](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/configuring-pull-request-merges/managing-a-merge-queue) ships a version; and [bors](https://bors.tech/), which lands Rust, does the cheaper batch-and-bisect variant.  It works, and it works by brute force, which I will come back to.

### Shared Environments

One port range, one development database, one mock server.

I do have a script assigning each worktree its own ports by hashing the directory name (the right idea), and it hashes into 99 slots.  With dozens of worktrees on disk the birthday math makes a collision effectively certain, and two agents get handed the same port whenever the colliding pair happens to be running at once.

### Undivided Work

Trivial parallelism doesn't work on a problem that was never split into non-conflicting units.  The agents were pointed at one jammed queue and found their own boundaries, which were mostly the same boundaries.

What looked like a coordinator was a process manager: spawn, wait, list, follow up, six hundred and thirty-three tool calls of overhead, all of it without any deep context about which pieces of work actually interacted.  A coordinator that only manages agents can't divide work it does not understand, so the children rediscover the same failures, edit the same new scripts, and invalidate one another's assumptions.  Codex said as much afterward: the agents generated work for the coordinator instead of removing work from the critical path.

Classified by root cause, 245 of the 361 entries collapse into ten systemic problems, 73 are one-off product bugs, and 43 I could not classify.  Here are the ten, because a taxonomy whose majority is invisible isn't a taxonomy:

| systemic root cause | incidents |
| --- | ---: |
| tooling the session was writing that same day | 87 |
| no per-agent workspace isolation | 33 |
| errors skipped, swallowed, or reported as success | 32 |
| affected-spec selection far too broad | 20 |
| migration order versus the sealed baseline | 16 |
| tests leaning on shared fixture state | 16 |
| recovery work that would not converge | 12 |
| CI cost structure | 12 |
| evidence produced against a base that moved | 10 |
| verification run without its dependencies | 7 |

I would defend the shape of that table rather than the ratio: workers under a shallow coordinator rediscover the same things, often enough to dominate a log.

Look down the column and the majority is exactly that failure.  The top row alone (tooling the session was writing that same day, 87 incidents) is agents redoing and colliding on the same work, because nothing holding the session had enough context to keep them off each other's toes.  Add isolation failures and swallowed errors and the top three account for 152.

Those are not ordinary defects that concurrency merely made frequent.  They are the artifact of coordination without context: a parent counting agents instead of understanding the work, so that the children rediscover and rewrite the same things, file the same findings, and hand the parent the job of reconciling them.  The seal and the port hash still have boring mechanical fixes, and recovery-that-would-not-converge and CI cost structure are process problems.  The rediscovery is neither.

The one that is irreducible in a different sense is the sixth row: tests leaning on shared fixture state, or green(A) and green(B) failing to imply green(A+B) in ordinary clothes.

It accounts for sixteen of the 361, tied with the seal for the smallest systemic bucket, and I want to put that number in front of you instead of letting you find it by subtraction.  Sixteen out of 361 is four percent.

So that substrate problem is the _rarest_ thing that happened that week.  It is also the only one where catching it means building the composition and running the entire suite against it, which I do, serially, late, and which is precisely the expensive thing.  It is therefore the only one on the list where I can't tell you what an _affordable_ fix would even look like.  The common problem looks much simpler: parallelism without a division of the work, supervised by something that was counting agents instead of understanding them.

Except that the two are the same problem, and it took me most of a week to see it.  What a coordinator needs in order to divide work is exactly a read/write set per unit, which is the primitive this whole post is about, one layer up.  Mine had no way to compute one, so it couldn't divide anything, so eighty-four agents went out and found the same boundaries and did the same work on top of each other.  That's the top two rows of the table, a hundred and twenty entries, a third of everything I logged.  I'm counting two rows and not the three above, because swallowed errors are ordinary bad error handling and belong to a different complaint.

So the four percent isn't a rare problem I've chosen to write about instead of the common one.  It's the identical question, "what does this thing read, and what does it write," asked about a code change instead of about an agent.  At the code layer the question is open, which is the research problem.  At the agent layer it's answerable in principle, and my runtime simply didn't ask, which is the more embarrassing half and the one my week actually went to.

## The Missing Primitive

So what would it take for some layer to detect any of this?

Start with why detection is hard.  One pull request writes a row to `show_attendance`; another reads a count over it.  Neither diff says so.  The write goes through an HTTP handler, an ORM, and some SQL; the read goes through the app.  **The conflict is a property of what the code does when it runs, and it's not present in the text.**

That leaves two ways to find it:

1. Execute the composition and observe what breaks.
2. Compute the effects of each change and compare them without running anything.

Nobody can do the second precisely enough at the granularity where my conflicts live, so everybody does the first.  If you can't compute effects, you can't tell which pairs need checking, and so you pay for a full verification of every candidate future you are willing to buy.

Uber's [SubmitQueue](https://www.uber.com/ci/en/blog/slashing-ci-costs-at-uber/) needed a probabilistic model of which changes would land in order to prune the speculation tree, and that is less an optimization than a confession: you only guess your way through the tree when nothing can tell you which branches matter.

So the state of the art converts an unsolved program-analysis problem into a compute purchase.  That is rational at Google's scale and precisely the wrong trade at mine, because speculation costs queue depth times full suite times parallel environments, and agents raise queue depth.  The sanctioned fix for the correctness problem is more CI spend, scaling with the exact variable agents just multiplied.

My two complaints turn out to be one complaint.  "CI is too expensive" and "composition failures show up only when the serial queue retests them" are the same missing primitive seen from two sides.

Here is the size of it in my own logs:

* Over three days, 510 CI runs were spent against 111 opened and 100 merged pull requests: roughly five full verifications per change that landed.
* The runs attach to opened pull requests rather than merged ones, so read that as an order of magnitude and not a rate.
* Of the 510, 112 failed or were cancelled outright, and cancellations are usually a new push superseding an old one rather than an escape.
* Deleting every one of those 112 takes the multiplier from 5.1 to 4.0, not to 1.
* The rest is ordinary iteration, some fraction of it rebasing onto a main that moved underneath, which is the same problem in ordinary clothes.
* The primitive is therefore worth about a quarter of the excess, and only if every one of those failures turns out to be a composition escape, which I cannot show.  A serial queue stays slow either way.

Databases avoid all of this for one reason.  **A transaction declares its read/write set.**  The system knows what each transaction touched, so it can detect conflicts, order what must be ordered, and let everything else proceed.

That question (which operations may run without coordination) has an exact answer in the literature.  [Bailis and colleagues](https://www.bailis.org/papers/ca-vldb2015.pdf) named it _invariant confluence_: a set of operations is I-confluent with respect to an invariant precisely when merging any two invariant-preserving reachable states that share a common ancestor yields another invariant-preserving state.

This is a necessary and sufficient condition for running those operations coordination-free while maintaining the invariant, not a heuristic.  If your workload is I-confluent you can run coordination-free and stay correct; if it is not, coordination is a requirement, and **no merge function will save you.**

The result does not transfer to code cleanly, and it's worth saying where it breaks.  I-confluence is defined over a merge operator on states, with reachability and a fixed invariant, whereas for code changes there is no defined merge over program states, and the invariant here is the test suite, which every merge rewrites.  What I'm borrowing is the shape of the question, not the theorem.

Source code carries a partial declaration at best.  Build systems get closest: [Bazel](https://bazel.build/) and [Nix](https://nixos.org/) require every target to declare its inputs and outputs (a real read/write set at file granularity), and that is how affected-target computation works across enormous monorepos.

However, file granularity is the wrong granularity here.  My two colliding pull requests touch entirely disjoint files.  A build graph would call them independent, and a build graph would be right, and they would still destroy each other, because the thing they share is a row in a database that neither one names.

At the granularity where my conflicts live, nothing declares those effects.  So there is no conflict detection there, and without it you can't tell which regime a pair of changes is in, and if you can't tell, the only safe policy is to assume the worst and coordinate everything.

> **What is the runtime read/write set of a code change, and can it be computed precisely enough, and cheaply enough, to decide which changes are safe to integrate concurrently?**

I've already built a bad version of this without recognizing it.  My affected-spec selector maps a change to the tests it could affect, so that CI only runs what is relevant.  That's a read-set estimator, and its failure mode is the research problem in miniature: it traced a one-screen change through a shared UI component, concluded the change could affect anything, and scheduled all 115 test files.

An over-approximation collapsing to "conflicts with everything" is the same degenerate case as a lock covering the whole table, or as serializing every update to a dictionary even when the changes are to disjoint keys.  Sound, and useless.

One objection here is that I am demanding a soundness nobody needs.  Large organizations run test selection that is deliberately imprecise and absorb the escapes: changes that should have been tested, were not, and break trunk after they land.

It's tempting to say I can't absorb escapes because my blast radius is smaller, but that has it backwards: an escape into Google's trunk blocks far more people than an escape into mine.  What they have, and I don't, is containment.  Post-submit continuous builds, automated culprit-finding, and rollback tooling all bound what an escape costs.

I have none of that.  What I have instead is a queue of agents that respond to a red trunk by independently rediscovering the failure, each one paying for its own full CI run, none of them asked to, and none of them aware that eleven others are doing exactly the same thing at exactly the same moment.  Escapes are cheaper for Google not because fewer people are blocked, since more are, but because they built the machine that catches and bounds the damage.  Without that machine, imprecise selection is a cost I can't absorb.

Prior art exists, and none of it quite lands:

* [Regression test selection](https://www.cs.purdue.edu/homes/xyzhang/fall07/Papers/p173-rothermel.pdf) has estimated read sets for years, at file granularity, the granularity that doesn't help, though [dynamic variants](https://mir.cs.illinois.edu/marinov/publications/GligoricETAL15PracticalRTS.pdf) get closer than my static one does.
* Speculative merge automation (e.g., [Zuul](https://zuul-ci.org/), [SubmitQueue](https://www.uber.com/ci/en/blog/slashing-ci-costs-at-uber/)) tests the composition, which is the expensive thing.
* [Proactive conflict detection](https://people.cs.umass.edu/~brun/pubs/pubs/Brun11fse.pdf) went after semantic conflicts directly more than a decade ago and never became something anybody runs.

## Requirements for Coordination-Free Development

Coordination-free multi-agent development, if it's possible at all, needs solving at two layers, in order.  Getting the second right while the first is broken buys nothing.

_The environment has to permit it first._  Separate databases, working directories, port ranges, and instances of every service, per agent, enforced rather than attempted.  Writers in one checkout contend regardless of how elegant your merge semantics are, and I still do not know how many of those 84 were writers.

_Then the artifacts have to stop requiring a coordinator._  A total order needs somebody to assign it, and in the single-writer world the assigner was free: migration numbers went up because one person added one at a time.  Put N agents on it and the assigner is gone.  You can reintroduce one as a central coordinator handing out positions, but a central coordinator _is_ coordination, and it caps throughput at one.

Two mainstream designs exist and always have: a total order by timestamp, as [Rails](https://guides.rubyonrails.org/active_record_migrations.html) has done since 2008, and a dependency graph, as [Django](https://docs.djangoproject.com/en/stable/topics/migrations/) and [Alembic](https://alembic.sqlalchemy.org/) do.  Under one writer the choice barely matters.  Under many it matters enormously, because a total order needs an assigner and a graph doesn't.  My agent picked the total order.

Nobody ever weighed the two, and that's the part worth sitting with, because it isn't really about migrations.  I ratified the timestamp scheme myself, back in March, and what I ratified was a fix for filename _collisions_: that was the problem in front of me, the fix solved it, and so I shipped it and [wrote a post about it]({% post_url 2026-03-30-multi-agent-systems-have-a-distributed-systems-problem %}) without once asking what the scheme would do under load.

An agent proposed it, an agent reviewed it, and I signed off on the part I understood.  Review is one of those protocols that terminates in a person, and what the person is supposed to supply is an independent prior.  Neither reviewer had one: not the agent, which reached for the most common pattern, and not me, because I was evaluating a collision fix and never saw an ordering decision.  The better option was documented, shipped in two major frameworks, and one search away.

This is what vibe coding actually costs, and it's not bad code.  The code was ordinary and defensible.  What is missing is the one question nobody asked, because asking it requires somebody who has been hurt by the answer before, and I had removed all of those people from the loop on purpose.

Django and Alembic don't solve it either, and the reason is the granularity problem again.  A Django dependency is an edge to another _migration_: this one runs after `0042_add_venue`, which says nothing about what either one reads or writes.  Two migrations with no declared edge are unordered, so if one adds a column and the other's `UPDATE` reads it, that pair conflicts, no edge exists, and nothing detects it.  The DAG removed the global sequence and left the effects undeclared.

And when two branches produce divergent heads, the resolution is a merge revision: a node with two parents that doesn't verify the branches are compatible.  It records a human's assertion that they are, the same protocol terminating in the same person, one layer down.

So the proposal isn't "invent a DAG."  The DAG exists.  It is to make the edges _effect-based_ instead of identity-based: know that this migration writes `show_attendance` and reads `shows.date`, and derive the ordering constraints and the genuine independence from that.

The objection is that this relocates the labor, and "somebody declares it" across 84 agents is not a plan.  For general code that lands.  For migrations it doesn't, because the effects are already in the text: SQL names its objects out loud, so parsing `ALTER TABLE show_attendance ADD COLUMN` or `UPDATE shows SET ... WHERE date > ...` yields the read set and the write set with no human, no annotation, and no agent remembering.  All 1,388 of mine are plain `.sql`, and the parsing is commodity, since [sqlglot](https://github.com/tobymao/sqlglot) does column-level lineage today.

Two caveats.  Disjointness only rules out conflicts through the objects a migration names, so an invariant spanning two tables can still break when two migrations touch one table each: the name-and-length record, one level down.  And my sixteen seal incidents have a two-line fix with no research content in it.  The case rests on the class rather than on those sixteen, because any ordering constraint several writers must satisfy independently has this shape.

Notably, [dbt](https://docs.getdbt.com/reference/dbt-jinja-functions/ref) builds its graph from `ref()` and `source()` macros a human writes by hand, in the one ecosystem where the effects are most extractable.  The hard case is the handler-through-ORM-through-SQL path, where the effects genuinely aren't in the text.  The easy case has been sitting in my repository the whole time, unbuilt.

This isn't the CRDT move.  CRDTs design a data type so that concurrent operations commute by construction; this is optimistic concurrency control, which extracts read/write sets and coordinates only the pairs that overlap.  Designing for commutativity would mean defining the data type, and here the data type is "a software system."

The same argument applies to source code and lands somewhere less comfortable.  Git's answer to two agents editing one file is to linearize, rebase the second onto the first, and resolve the conflicts: a coordination protocol with a human standing at the conflict point.  It works because textual conflicts are rare enough and humans read them well.  Both halves are under pressure now: agents collide constantly, and the human who was supposed to adjudicate is increasingly not reading the diff.

Which raises the question I keep arriving at and don't love.  **Text files, line-based diffs, and sequential human review are a human interface.**  A structural representation, where merge is defined over the program's structure instead of over lines, where a change declares what it reads and writes, and where invariants are attached to the artifact and checked mechanically, is a far better substrate for many concurrent writers.

People have proposed exactly that for decades and it mostly didn't take, not merely because developers like text, though they do, but because text is the interchange format that every other tool in the ecosystem already agrees on (diff, blame, bisect, grep, review, CI logs, and every static analyzer ever written), so that replacing it means replacing all of them, per language, before you get any benefit at all.

The exceptions are the argument rather than an exception to it.  [Darcs](http://darcs.net/) and then [Pijul](https://pijul.org/) built patch algebras where independent changes provably commute (the CRDT move applied directly to source control), and [Unison](https://www.unison-lang.org/) stores a content-addressed syntax tree with no text merge at all.  They exist, they work, and almost nobody uses them.

The tempting move is to say that agents change that calculus, because the reader has left the room and the ergonomics objection leaves with them.  I do not think that is safe yet.

Language models are the most text-dependent writers we have ever built: trained on text diffs and line-oriented source, and currently worse at manipulating syntax trees than at manipulating the text I would be proposing to abandon.  The party being served may not be human, but it learned to code by reading us.  Whether that is a durable property of these systems or an artifact of what we happened to train them on is the load-bearing question, and it's not one I can answer from a migration log.

## Takeaways

So here is what I don't know, gathered in one place rather than dropped along the way.

Whether a runtime read/write set can be computed precisely enough to be worth having.  Whether text is a contingent fact about today's models or a durable one.  And, most uncomfortably, what the specification is once humans stop reading code: if the test suite becomes the only surviving statement of what the system should do, and the suite is also written by agents, then attaching invariants to the artifact and checking them mechanically relocates the trust problem rather than solving it.

That last one is a different complaint from the rest of this post.  I've been arguing that the suite gets run against the wrong artifact.  That's the worry that it _is_ the wrong artifact.

The short-term fixes are unglamorous and I'm doing them anyway: a merge queue with a concurrency limit of one, a worktree with its own port range and database per agent, a cap on how many subagents a session may spawn, and one incident per root cause instead of one per symptom.  That is admission control and resource isolation, plus better bookkeeping.  None of it is research, and none of it touches the verification multiplier, the part that needs something nobody has.

Codex could describe all of this afterward, lucidly, and could answer none of it beforehand.  It spawned 84 workers anyway, which is less a mistake than a preview.

My setup is deliberately extreme, and a team that reviews its diffs and never let an agent design its schema layer will see a fraction of what I saw.  They will still see some of it.  The substrate underneath is the same one, and it still assumes there is one of you.

If you are working on conflict detection, effect extraction, or merge queues that don't cost a fortune, or if you have hit this same wall from the other direction inside a large organization, [please reach out](https://bsky.app/profile/christophermeiklejohn.com).  I would very much like to be told that some part of this is already solved.

Three open questions is a worse ending than one answer.  It's the honest count.

---

*This is part of an ongoing series about building [Zabriskie](https://zabriskie.app) with AI agents. Previously: [The Test Suite Was the Incident]({% post_url 2026-06-10-the-test-suite-was-the-incident %}), [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}), and [Multi-Agent Systems Have a Distributed Systems Problem]({% post_url 2026-03-30-multi-agent-systems-have-a-distributed-systems-problem %}).*
