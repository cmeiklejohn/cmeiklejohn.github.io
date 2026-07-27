---
layout: post
title:  "One Writer"
subtitle: "Our tools assume one writer, and assume that writer is a human. Nothing computes what a change reads and writes at runtime, so the only known fix is brute force priced for organizations."
date:   2026-07-26 22:00:00 -0400
group: ai
categories: ai zabriskie agents reliability testing ci distributed
---

Six weeks ago I wrote [The Test Suite Was the Incident]({% post_url 2026-06-10-the-test-suite-was-the-incident %}): my test suite had grown a pile of shared data nobody owned, every pull request paid to rebuild it, and the resulting failures had nothing to do with the changes under review. That cost me about $180 in one night.

I got a worse one, and it lasted three days. This post is not really about that, though. It's about a property of our tooling that the three days made impossible to ignore.

**Nearly every layer of this assumes one writer, and assumes that writer is a human.** Git hands you a conflict and waits. Code review assumes somebody reads. A migration sequence assumes somebody is assigning the order. Each of those protocols terminates in a person, which is fine while there is exactly one and they are one.

That was invisible for forty years because nothing ever bound it at my scale. Agents break both halves at once: there are many of them, and not one of them is the person the protocol was waiting for. Git is the partial exception, and I'll come to why the exception doesn't help. Nothing in the stack detects the violation when it happens. It gets caught later, somewhere else, attributed to the wrong change, and paid for at full price.

Context for readers arriving fresh. [Zabriskie](https://zabriskie.app) is a social app for live-music fans, and it is also a deliberate experiment: I'm building a real, deployed, used application almost entirely with AI agents, to find out what that's like and where it breaks. I've written almost none of the code. Agents wrote the features and agents wrote the tests that guard them.

That framing matters, because several things below look like obvious mistakes and are. I let agents design a migration scheme with only another agent reviewing it. I stopped reading most diffs. I let sixty-four pull requests go up in a day. The point of running an experiment at the extreme is to find the walls, and that week I found several at once.

A **migration**, throughout, is a versioned SQL file that changes the database schema. **CI** is the automated checking that runs on every proposed change: build the app, spin up a fresh database, run the tests.

Here is the shape of the three days. Treat these as texture rather than evidence, for a reason I'll get to.

| | 24 Jul | 25 Jul | 26 Jul |
| --- | ---: | ---: | ---: |
| pull requests opened | 19 | 64 | 28 |
| pull requests merged | 20 | 50 | 30 |
| incidents logged | 0 | 6 | 301 |

Days in that table are UTC; clock times in the narrative below are Eastern, which is where I was. The burst ran past midnight UTC: 301 incidents on the 26th plus 60 more before 1 AM on the 27th, so 361 for the burst. Every incident count below is scoped to that window, and 253 rows is the total before 24 July.

I offer that comparison as a sense of scale and not as a baseline, for the following reason. Those incidents exist because a standing instruction tells agents to log their own mistakes, and ten minutes into the worst night I tightened that instruction. The log measures *reported* failures. Look at the daily series and it gets worse: there are days that week with ten and twenty merged pull requests and zero logged incidents, which at any real failure rate means nobody was logging rather than nothing broke. I can't give you a trustworthy baseline. What follows rests on mechanism and on a few dated, checkable events, not on 361.

## The Experiment I Didn't Mean to Run

Late Saturday night the queue jammed. Sixty-four pull requests had gone up that day, `main` was red, and nineteen open pull requests were stuck behind a suite that couldn't tell me which of them was broken. At 12:31 AM I opened a session with [Codex](https://platform.openai.com/docs/codex) and complained that CI was wasting too much money.

Codex read that and hired a workforce. Over seventeen hours that session made 81 spawn calls, producing 74 direct children; those children spawned 10 more, for 84 threads. Then 358 calls waiting on them, 96 listing them, 85 sending follow-up work, 13 interrupting. Six hundred and thirty-three tool calls of pure management overhead against 54 messages from me. All 84 ran in the same checked-out copy of the repository.

Trivial parallelization, at scale, with a coordinator that only managed agents, on a toolchain built for one writer. The rest of this post is about why that combination is so much worse than it sounds.

I asked afterward why it had spawned anything, since I never requested it. Take what follows as a fluent account rather than an explanation: a language model asked why it did something produces plausible narrative, not introspection, and I apply that skepticism to self-reported incidents earlier so I should apply it here. What makes the quotes worth printing is that the account is *correct*, whatever its provenance.

> I chose to spawn multiple agents even though you did not ask me to. The environment permitted proactive delegation, but that was permission, not a requirement. I treated the availability of agents as a reason to use them instead of first asking whether they would reduce time, cost, or risk.

It then diagnosed why the task was a bad candidate:

> A PR queue is largely serial because every merge changes `main`, which changes the integration state of every remaining PR. The work needed one authoritative coordinator moving through the queue in order. Parallel agents could not independently merge overlapping PRs without continuously invalidating one another's assumptions.

On what those 358 waits were doing:

> The agents' outputs created additional coordination work for me. I had to read their findings, compare conflicting recommendations, inspect supporting evidence, and decide how to combine them. The agents therefore generated work for the coordinator instead of reliably removing work from the critical path.

A system with no notion of its own concurrency was handed a concurrency primitive and used it, reasonably, on a task the substrate couldn't support. What that substrate is, and why nothing in it objected, is the rest of the post. I should be careful about one number, though: I don't know how many of those 84 threads *wrote* to the checkout rather than reading it. From Codex's own account many were investigating. A reader is harmless and a writer isn't, and nothing in the toolchain drew the distinction or could. Which is the same missing primitive one level up. Not even the agent runtime knew which of its children were writers.

## The Substrate Assumes One Writer

Start with the layer that did anticipate this, because it's the one people reach for. Branches are optimistic concurrency control; worktrees go further, giving each writer a physically separate checkout so two agents can hold two versions of the tree at once. That works.

The trouble is that git's isolation stops at the edge of the source tree. A worktree gives an agent its own files. It does not give it its own database, port range, mock server, or position in the migration sequence. Everything below the filesystem is shared and singular.

That assumption was invisible to *me* because it never bound me. It has bound large organizations for a long time, and they built the response: merge queues, hermetic builds, database-per-test, trunk-based development, whole infrastructure teams. What's new isn't the concurrency. **It's that a solo developer now operates in the regime that used to require an infrastructure organization, with none of the infrastructure and no headcount to build it. Agents removed the cap on my arrival rate. They did not hand me Google's build system.**

Partial isolation is then its own trap. A worktree gives you the *feeling* of a private workspace: clean tree, own branch, no file collisions, reading as properly parallel right until two private workspaces write the same database row. Nothing errors. Nothing warns. The contention surfaces later, somewhere else, as a red X on an unrelated pull request.

Worse, the isolation *below* git is advisory, and agents have to choose it. Mine routinely don't. One incident reads *"PR 1868 isolated E2E attempt fell back to shared ports"*: the agent tried to isolate its test environment, isolation failed, and nothing stopped the run.

Worse, a worktree is cut from a commit and stays there, so it's isolated *in the past* and decays with every merge. When I changed a git hook in July, the fix merged at 1:48 AM and four hours later **73 of 74 worktrees were still running the old copy**. A stale worktree can't detect its own staleness, so it goes green about a world that no longer exists, and the error is deferred to the only actor holding current `main`, which is CI.

### Convergence is not invariant preservation

One framing before the specifics, because it unifies them. Every merge mechanism here is built to **terminate** and none is built to **preserve invariants**. Git is honest about the cases it can't decide: on a textual conflict it halts and asks you. The trouble is the far more common case, where it doesn't halt, produces a merge confidently, and the invariant it never knew about is now false.

That distinction is the oldest lesson in the replicated-data literature and it has a canonical counterexample. Take a replicated map where each field merges independently under its own perfectly reasonable rule. One field holds a person's name, another the length of that name. Two replicas concurrently write different names. Each field converges exactly as specified, and the result is a record whose `name` came from one replica and whose `length` came from the other, with the invariant tying them now false. Nothing merged incorrectly. The composition of correct local merges is not a correct global merge. Closing that gap is what work like Balegas and colleagues' [Indigo](https://www.dpss.inesc-id.pt/~rodrigo/indigo_eurosys15.pdf), which enforces application invariants over eventually consistent stores, is [for]({% post_url 2018-11-14-ipa %}).

Some of what follows has that shape. The first case is plainer, and I should not dress it up.

### Sealing a prefix under an in-flight queue

This project has 1,388 migrations, and rebuilding a test database from all of them on each of eight parallel test machines is as slow as it sounds. So, in response to my complaining for weeks that CI was too expensive, an agent froze a database snapshot into the repository for CI to restore, applying only what came after. Good optimization: it saves 13 to 18 minutes of machine time per run. It merged at 5:01 AM Eastern on the third day.

Fifty-four minutes later the first pull request failed, because its migration no longer sorted after the newly frozen prefix. Then another. By late morning a single incident covers four at once: *"Four queued PRs carried migrations older than the sealed CI baseline suffix."*

The diagnosis is ordinary, and a critic will say so. Other agents had already opened pull requests that appended migrations. One agent sealed a new prefix underneath them. Those streams of work conflicted, and nothing noticed until CI rejected the queued pull requests much later. That is parallel work colliding and finding out late — not a subtle invariant bug. Caching a prefix of an ordered log made the collision loud and retroactive, but it did not invent the collision. So the loop closes: I complained CI was expensive, an agent made CI cheaper, and the mechanism became a new source of CI failures against the whole queue.

### A merge function looking in the wrong place

Git detects conflicts over lines of text; my conflicts live in shared runtime state. From that week: *"Song-call E2E suites deleted each other's shared pending call."* Two test files, no overlapping lines, both green alone, and the row one of them depends on is the row the other deletes. The collision is in the database at runtime, not in the diff. Those are the `name` and the `length`. They share no lines, each merges cleanly, each is individually green, and the invariant binding them is false the moment they land together. No improvement to git's merge algorithm catches that, because the property isn't a property of any file. It's a property of the composition, and git has no representation of the composition to check.

### Verification that doesn't compose

Which gives the sharpest version: **green(A on base) and green(B on base) does not imply green(merge of A and B).** The test suite is the only thing in my pipeline that checks the invariant at all, since git checks text and timestamps check ordering. So the tests are my invariant checker. I do run them on the composition: pull requests rebase onto current `main` and merge in sequence, each one retested before it lands, and `main` itself is checked after land. That catches the break. It just catches it late, and only by paying the serial tax — rebase, retest, merge, next — which is exactly the queue-wide cost the sealed prefix made visible when one seal invalidated everything behind it.

Serialization is the honest answer, and I already pay it. Speculative merge queues try to buy the same guarantee back with parallelism: build the candidate futures, `main+A`, `main+A+B`, `main+A+B+C`, test them concurrently, and discard a failure from the middle. OpenStack's [Zuul](https://zuul-ci.org/) has gated that way since 2012; [GitHub's merge queue](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/configuring-pull-request-merges/managing-a-merge-queue) ships a version; [bors](https://bors.tech/), which lands Rust, does the cheaper batch-and-bisect variant. It works, and it works by brute force, which I'll come back to.

### An environment with one of everything

One port range, one development database, one mock server. I do have a script assigning each worktree its own ports by hashing the directory name, which is the right idea, and it hashes into 99 slots. With dozens of worktrees on disk the birthday math makes a collision effectively certain, and two agents get handed the same port whenever the colliding pair happens to be running at once.

### Work that was never actually divided

Trivial parallelism does not work on a problem that was never split into non-conflicting units. The agents were pointed at one jammed queue and found their own boundaries, which were mostly the same boundaries. What looked like a coordinator was a process manager: spawn, wait, list, follow up — six hundred and thirty-three tool calls of overhead — without deep context about which pieces of work actually interacted. A coordinator that only manages agents cannot divide work it does not understand, so the children rediscover the same failures, edit the same new scripts, and invalidate one another's assumptions. Codex said as much afterward: the agents generated work for the coordinator instead of removing work from the critical path.

Classified by root cause: 245 of the 361 entries collapse into ten systemic problems, 73 are one-off product bugs, and 43 I couldn't classify. Here are the ten, because a taxonomy whose majority is invisible isn't a taxonomy:

| systemic root cause | incidents |
| --- | ---: |
| tooling the session was writing that same day | 87 |
| no per-agent workspace isolation | 33 |
| errors skipped, swallowed, or reported as success | 32 |
| affected-spec selection far too broad | 20 |
| migration order versus the sealed baseline | 16 |
| tests leaning on shared fixture state | 16 |
| recovery work that wouldn't converge | 12 |
| CI cost structure | 12 |
| evidence produced against a base that moved | 10 |
| verification run without its dependencies | 7 |

I'd defend the shape rather than the ratio: workers under a shallow coordinator rediscover the same things, often enough to dominate a log.

Look down that column and the majority is exactly that failure. The top row alone — tooling the session was writing that same day, 87 incidents — is agents redoing and colliding on the same work because nothing holding the session had enough context to keep them off each other's toes. Add isolation failures and swallowed errors and the top three are 152. Those are not ordinary defects that concurrency merely made frequent. They are the artifact of coordination without context: a parent counting agents instead of understanding the work, so the children rediscover and rewrite the same things. The seal and the port hash still have boring mechanical fixes. Recovery-that-wouldn't-converge and CI cost structure are process. The rediscovery is not.

The one that is irreducible in a different sense is the sixth row: tests leaning on shared fixture state, which is green(A) and green(B) failing to imply green(A+B) wearing work clothes.

It accounts for sixteen of the 361, tied with the seal for the smallest systemic bucket, and I want to put that number in front of you rather than let you find it by subtraction. Sixteen out of 361 is four percent. That substrate problem is the *rarest* thing that happened that week, and the only one where catching it means building the composition and running the entire suite against it — which I do, serially, late — which is precisely the expensive thing, and the only one where I can't tell you what an *affordable* fix would look like. The common problem was simpler: parallelism without a division of the work, supervised by something that was counting agents rather than understanding them.

## The Missing Primitive

So what would it take for some layer to detect any of this?

Start with why detection is hard. One pull request writes a row to `show_attendance`; another reads a count over it. Neither diff says so. The write goes through an HTTP handler, an ORM, and some SQL; the read goes through the app. **The conflict is a property of what the code does when it runs, and it is not present in the text.**

That leaves two ways to find it:

1. Execute the composition and observe what breaks.
2. Compute the effects of each change and compare them without running anything.

Nobody can do the second precisely enough at the granularity where my conflicts live, so everybody does the first. If you can't compute effects you can't tell which pairs need checking, so you pay for a full verification of every candidate future you're willing to buy. Uber's [SubmitQueue](https://www.uber.com/ci/en/blog/slashing-ci-costs-at-uber/) needed a probabilistic model of which changes would land in order to prune the speculation tree, which is less an optimization than a confession: you only guess your way through the tree when nothing can tell you which branches matter.

So the state of the art converts an unsolved program-analysis problem into a compute purchase. Rational at Google's scale, and precisely the wrong trade at mine, because speculation costs queue depth times full suite times parallel environments, and agents raise queue depth. The sanctioned fix for the correctness problem is more CI spend, scaling with the exact variable agents just multiplied.

My two complaints turn out to be one complaint. "CI is too expensive" and "composition failures show up only when the serial queue retests them" are the same missing primitive seen from two sides.

Here's the size of it in my own logs:

- Over three days, 510 CI runs were spent against 111 opened and 100 merged pull requests: roughly five full verifications per change that landed.
- The runs attach to opened pull requests rather than merged ones, so read that as an order of magnitude and not a rate.
- Of the 510, 112 failed or were cancelled outright, and cancellations are usually a new push superseding an old one rather than an escape.
- I shouldn't overstate what fixing that buys. Deleting every one of those 112 takes the multiplier from 5.1 to 4.0, not to 1.
- The rest is ordinary iteration, some fraction of which is rebasing onto a main that moved underneath, which is the same problem in work clothes.
- The primitive is worth about a quarter of the excess, and only if every one of those failures turns out to be a composition escape, which I can't show. A serial queue stays slow either way.

Databases avoid all of this for one reason. **A transaction declares its read/write set.** The system knows what each transaction touched, so it can detect conflicts, order what must be ordered, and let everything else proceed.

That question, which operations may run without coordination, has an exact answer in the literature. [Bailis and colleagues](https://www.bailis.org/papers/ca-vldb2015.pdf) named it **invariant confluence**: a set of operations is I-confluent with respect to an invariant precisely when merging any two invariant-preserving reachable states that share a common ancestor yields another invariant-preserving state. It's a necessary and sufficient condition for running those operations coordination-free while maintaining the invariant, not a heuristic. If your workload is I-confluent you can run coordination-free and stay correct; if it isn't, coordination is a requirement and **no merge function will save you.**

Source code carries a partial declaration at best. Build systems get closest: [Bazel](https://bazel.build/) and [Nix](https://nixos.org/) require every target to declare its inputs and outputs, which is a real read/write set at file granularity, and it's how affected-target computation works across enormous monorepos. But file granularity is the wrong granularity here. My two colliding pull requests touch entirely disjoint files. A build graph would call them independent, and a build graph would be right, and they would still destroy each other, because the thing they share is a row in a database that neither one names.

At the granularity where my conflicts live, nothing declares those effects. So there's no conflict detection there, and without it you can't tell which regime a pair of changes is in, and if you can't tell, the only safe policy is to assume the worst and coordinate everything.

> **What is the runtime read/write set of a code change, and can it be computed precisely enough, and cheaply enough, to decide which changes are safe to integrate concurrently?**

I've already built a bad version of this without recognizing it. My affected-spec selector maps a change to the tests it could affect, so CI only runs what's relevant. That's a read-set estimator, and its failure mode is the research problem in miniature: it traced a one-screen change through a shared UI component, concluded the change could affect anything, and scheduled all 115 test files. An over-approximation collapsing to "conflicts with everything" is the same degenerate case as a lock covering the whole table, or serializing every update to a dictionary even when the changes are to disjoint keys. Sound, and useless.

The obvious objection is that I'm demanding a soundness nobody needs. Large organizations run test selection that is deliberately imprecise and absorb the escapes — changes that should have been tested, weren't, and break trunk after they land.

It is tempting to say I can't absorb escapes because my blast radius is smaller, but that has it backwards: an escape into Google's trunk blocks far more people than an escape into mine. What they have, and I don't, is containment. Post-submit continuous builds, automated culprit-finding, and rollback tooling bound what an escape costs. I have none of that. What I have instead is a queue of agents that respond to a red trunk by independently rediscovering the failure, each paying for its own full CI run, without being asked. Escapes are cheaper for them not because fewer people are blocked — more are — but because they built the machine that catches and bounds the damage. Without that machine, imprecise selection is a cost I can't absorb.

Prior art exists and none of it quite lands:

- [Regression test selection](https://www.cs.purdue.edu/homes/xyzhang/fall07/Papers/p173-rothermel.pdf) has estimated read sets for years, at file granularity, which is the granularity that doesn't help, though [dynamic variants](https://mir.cs.illinois.edu/marinov/publications/GligoricETAL15PracticalRTS.pdf) get closer than my static one does.
- Speculative merge automation — [Zuul](https://zuul-ci.org/), [SubmitQueue](https://www.uber.com/ci/en/blog/slashing-ci-costs-at-uber/), and the rest — tests the composition, which is the expensive thing.
- [Proactive conflict detection](https://people.cs.umass.edu/~brun/pubs/pubs/Brun11fse.pdf) went after semantic conflicts directly more than a decade ago and never became something anybody runs.

## What Coordination-Free Would Actually Require

Coordination-free multi-agent development, if it's possible, needs solving at two layers, in order. Getting the second right while the first is broken buys nothing.

**The environment has to permit it first.** Separate databases, working directories, port ranges, and instances of every service, per agent, enforced rather than attempted. Writers in one checkout contend regardless of how elegant your merge semantics are, and I still don't know how many of those 84 were writers.

**Then the artifacts have to stop requiring a coordinator.** A total order needs somebody to assign it, and in the single-writer world the assigner was free: migration numbers went up because one person added one at a time. Put N agents on it and the assigner is gone. You can reintroduce one as a central coordinator handing out positions, but a central coordinator *is* coordination, and it caps throughput at one.

Two mainstream designs exist and always have: a total order by timestamp, which is what [Rails](https://guides.rubyonrails.org/active_record_migrations.html) has done since 2008, and a dependency graph, which is what [Django](https://docs.djangoproject.com/en/stable/topics/migrations/) and [Alembic](https://alembic.sqlalchemy.org/) do. Under one writer the choice barely matters. Under many it matters enormously, because a total order needs an assigner and a graph doesn't. The move, which is not a new idea, is to prefer the graph — to stop requiring a total order. My agent picked the total order.

That's the part worth sitting with, and it isn't really about migrations. Nobody ever weighed a sequence against a graph. I ratified the timestamp scheme myself, back in March, and I want to be exact about what I ratified: a fix for filename *collisions*. That was the problem in front of me, the fix solved it, and I shipped it and [wrote a post about it]({% post_url 2026-03-30-multi-agent-systems-have-a-distributed-systems-problem %}). What nobody did, me included, was ask the next question, which is whether a total order is the right shape when several writers are appending at once. The workload was never a consideration.

An agent proposed it and an agent reviewed it, and I signed off on the part I understood. This is the human assumption failing in its own particular way. Review is one of those protocols that terminates in a person, and what the person is supposed to supply is an independent prior. Neither reviewer here had one: not the agent, which reached for the most common pattern, and not me, because I was evaluating a collision fix and never saw an ordering decision. The better option was documented, shipped in two major frameworks, and one search away.

This is what vibe coding actually costs, and it isn't bad code. The code was ordinary and defensible. What's missing is the one question nobody asked, because asking it requires somebody who has been hurt by the answer before, and I had removed all of those people from the loop on purpose.

Django and Alembic also don't solve it, and the reason is the granularity problem again. A Django dependency is an edge to another *migration*: this one runs after `0042_add_venue`. It says nothing about what the migration reads or writes. Two migrations with no declared edge are unordered, and the framework runs them in whichever direction the traversal produces. If one adds a column and the other's `UPDATE` reads it, that pair conflicts, no edge exists, and nothing detects it. The DAG removed the global sequence and left the effects undeclared. When two branches produce divergent heads, the resolution is a merge revision: a node with two parents that doesn't verify the branches are compatible. It records a human's assertion that they are, which is the same protocol terminating in the same person, one layer down.

So the proposal isn't "invent a DAG." The DAG exists. It's to make the edges *effect-based* rather than identity-based: know that this migration writes `show_attendance` and reads `shows.date`, and derive the ordering constraints and the genuine independence from that.

The obvious objection is that this relocates the labor, and "somebody declares it" across 84 agents is not a plan. For general code that objection lands. For migrations it doesn't, because the effects are already in the text. A migration is SQL, and SQL names its objects out loud: `ALTER TABLE show_attendance ADD COLUMN`, `UPDATE shows SET ... WHERE date > ...`. The read set and write set are extractable by parsing the file. No human, no annotation, no agent remembering. Two migrations cannot conflict *through the objects they name* when those sets are disjoint, and that much falls out of the extraction instead of being asserted by anyone. It is not full commutativity, and I should not pretend otherwise: an invariant spanning two tables can still break when two migrations touch one table each, which is the name-and-length record one level down. Syntactic disjointness buys you the conflicts you can see.

Which makes migrations the tractable corner of the general problem. All 1,388 of mine are plain `.sql` files, and the parsing is commodity: [sqlglot](https://github.com/tobymao/sqlglot) does column-level lineage today. Worth noting what the SQL ecosystem's most famous DAG builder does instead. [dbt](https://docs.getdbt.com/reference/dbt-jinja-functions/ref)'s graph comes from `ref()` and `source()` macros that a human writes by hand, which is an identity-based, hand-declared edge set: exactly the thing I've spent this section arguing against, in the one ecosystem where the effects are most extractable. The hard case is the handler-through-ORM-through-SQL path, where the effects genuinely aren't in the text. The easy case has been sitting in my repository the whole time, unbuilt.

Though I should be straight about what that buys. My sixteen seal incidents have a two-line fix with no research content: key the cached snapshot on the *set* of applied migrations rather than filename sort order, and the ordering constraint disappears. The case for effect-based edges doesn't rest on those sixteen. It rests on the class. Any ordering constraint several writers must satisfy independently has this shape, and the next one I build will have it too unless something can tell me which migrations actually interact.

That isn't the CRDT move, and I should call it what it is, because the distinction is the whole of the previous section. CRDTs design a data type so concurrent operations commute by construction. This is optimistic concurrency control: extract read/write sets, test disjointness, coordinate the pairs that overlap. It's the database move, which I already set up when I said a transaction declares its read/write set. Designing for commutativity would require defining the data type, and here the data type is "a software system," except in the corner where it's a SQL file.

The same argument applies to source code and lands somewhere less comfortable. Git's answer to two agents editing one file is to linearize, rebase the second onto the first, resolve the conflicts: a coordination protocol with a human standing at the conflict point. It works because textual conflicts are rare enough and humans read them well. Both halves are under pressure now: agents collide constantly, and the human who was supposed to adjudicate is increasingly not reading the diff.

Which raises the question I keep arriving at and don't love. **Text files, line-based diffs, and sequential human review are a human interface.** A structural representation, where merge is defined over the program's structure rather than over lines, where a change declares what it reads and writes, and where invariants are attached to the artifact and checked mechanically, is a far better substrate for many concurrent writers.

People have proposed exactly that for decades and it mostly didn't take, not merely because developers like text, though they do, but because text is the interchange format every other tool already agrees on: diff, blame, bisect, grep, review, CI logs, every static analyzer. Replacing it means replacing all of them, per language. The exceptions are the argument rather than an exception to it. [Darcs](http://darcs.net/) and then [Pijul](https://pijul.org/) built patch algebras where independent changes provably commute, which is the CRDT move applied directly to source control, and [Unison](https://www.unison-lang.org/) stores a content-addressed syntax tree with no text merge at all. They exist, they work, and almost nobody uses them.

The tempting move is to say agents change that calculus, because the reader has left the room and the ergonomics objection leaves with them. I don't think that's safe yet. Language models are the most text-dependent writers we have ever built: trained on text diffs and line-oriented source, and currently worse at manipulating syntax trees than at manipulating the text I'd be proposing to abandon. The party being served may not be human, but it learned to code by reading us. Whether that's a durable property of these systems or an artifact of what we happened to train them on is the load-bearing question, and it isn't one I can answer from a migration log.

So here is what I don't know, gathered in one place rather than dropped along the way. Whether a runtime read/write set can be computed precisely enough to be worth having. Whether text is a contingent fact about today's models or a durable one. And, most uncomfortably, what the specification is once humans stop reading code: if the test suite becomes the only surviving statement of what the system should do, and the suite is also written by agents, then attaching invariants to the artifact and checking them mechanically relocates the trust problem rather than solving it. That last one is a different complaint from the rest of this post. I've been arguing the suite gets run against the wrong artifact. That is the worry that it *is* the wrong artifact.

## Where That Leaves Me

The short-term fixes are unglamorous and I'm doing them anyway: a merge queue with a concurrency limit of one, a worktree with its own port range and database per agent, a cap on how many subagents a session may spawn, and one incident per root cause instead of one per symptom. That's admission control and resource isolation plus better bookkeeping. None of it is research, and none of it touches the verification multiplier, which is the part that needs something nobody has.

Codex could describe all of this afterward, lucidly, and could answer none of it beforehand. It spawned 84 workers anyway, which is less a mistake than a preview. My setup is deliberately extreme, and a team that reviews its diffs and never let an agent design its schema layer will see a fraction of what I saw. They will see some of it. The substrate underneath is the same one, and it still assumes there is one of you.

Three open questions is a worse ending than one answer. It's the honest count.

---

*This is part of an ongoing series about building [Zabriskie](https://zabriskie.app) with AI agents. Previously: [The Test Suite Was the Incident]({% post_url 2026-06-10-the-test-suite-was-the-incident %}), [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}), and [Multi-Agent Systems Have a Distributed Systems Problem]({% post_url 2026-03-30-multi-agent-systems-have-a-distributed-systems-problem %}).*
