---
layout: post
title:  "The Test Suite Was the Incident"
subtitle: "A night of brittle fixtures, 49 failed CI runs, and the expensive lesson that test data needs ownership too."
date:   2026-06-10 00:00:00 -0000
group: ai
categories: ai zabriskie agents reliability testing
---

Last night I spent almost two hundred dollars proving that my test suite was lying to me.

Not because GitHub Actions was broken.

Not because the application had one catastrophic bug.

Not because one pull request was especially risky.

Because every pull request was paying to run the same brittle world from scratch, and that world was held together by shared fixtures nobody really owned.

By the end of the night, the numbers were ugly:

| thing | amount |
| --- | ---: |
| CI runs observed | 168 |
| failed runs | 49 |
| cancelled runs | 10 |
| total runner time | 11,721.8 job-minutes |
| failed runner time | 3,411.0 job-minutes |
| failed plus cancelled runner time | 3,903.0 job-minutes |
| GitHub CI quota used | about 90% of $200 |
| Codex plan escalation | $100 at 10 PM; upgraded to $200 before 3 AM |
| PR backlog | about a dozen still in flight, 9 open at the 2:32 AM snapshot |
| failed CI optimization work | #1112 abandoned; #1119 merged by mistake and reverted by #1121 |
| estimated agent token waste | about 1.7M to 2.5M tokens |

The direct cost of the failed and cancelled jobs was around $60.

That number is technically true and emotionally useless.

The night used about $180 of a $200 GitHub CI quota. The strict accounting says only a third of that was red-run waste. But that is not how the night actually behaved. The green runs existed because the red runs forced another commit. The reruns existed because the failure moved from one shared assumption to another. The successful checks were not clean proof of health; they were the tax paid after pushing the test suite back into one of the few shapes it still tolerated.

So the honest answer is this: the directly attributable CI waste was about $52 to $60. The practical cost of the bad test design was most of the $180, plus the evening, plus something like two million tokens spent asking agents to debug symptoms of the same structural problem.

And that was only the GitHub side.

At 10:00 PM EDT on June 9, 2026, I signed up for the $100 Codex plan. Before 3:00 AM on June 10, I had already upgraded to the $200 plan just to keep pushing the open PRs toward merge. That is the part that makes the waste feel different. This was not an abstract cloud bill. This was personal money turning into retries while the same brittle test assumptions kept reproducing themselves.

The tests were not protecting me.

They were charging me rent.

## The Failure Mode

The actual application work was not the disaster.

There were real features. There were real fixes. There were pull requests that should have been small, reviewable, and mergeable. Instead, every PR entered the same grinder.

Each run rebuilt the backend, started Postgres, applied the migration chain, seeded the database, started the frontend, and split Playwright across eight shards. That is already expensive. It becomes absurd when the shared seed data is unstable.

The first wave of failures had nothing to do with the PRs that were failing.

One migration expected a user named `queenofthemean` to exist:

```text
birthday-honoree fix matched 0 rows
no user with username/display_name = queenofthemean
```

Another class of failures came from incident migrations tripping a database constraint because the incident rows did not include required waste estimates.

These were not product regressions. They were fixture assumptions leaking into migration behavior. A pull request could be about one part of the app and still lose because some other part of the global test universe had drifted.

Then came the fixture collision.

A Lot statistics change made the tour stats card count only past attended shows. That was the correct product behavior. The test failure was not that the feature was wrong. The failure was that `e2etester` was doing too much work across too many tests.

The same global user was involved in Lot tests and recap tests. A setup change added an RSVP for that user so one spec could assert attended-show behavior. Another spec expected exactly one attendee on a recap fixture. Suddenly the recap test failed, not because recaps were broken, but because the shared world had acquired one extra person.

This is the specific kind of failure that makes people stop trusting tests.

It is not flaky in the random sense. It is deterministic. It fails every time. But it fails for a reason that is nowhere near the change under review.

The fix was to create dedicated fixture data for the Lot scenario and clean up stale recap RSVPs.

The lesson was that the test suite had been relying on a global social graph as if it were neutral infrastructure.

It was not neutral.

It was mutable state.

## The Same Bug, Wearing Different Clothes

Later in the night, the same pattern came back through the song database.

A migration changed the uniqueness model for songs. The old constraint was effectively:

```sql
(band_id, song_name)
```

The new model needed to account for `original_artist`, so it became an expression index involving:

```sql
(band_id, song_name, COALESCE(original_artist, ''))
```

That schema change was reasonable.

But the E2E seed file still contained inserts like this:

```sql
ON CONFLICT (band_id, song_name) DO UPDATE
```

Postgres did exactly what Postgres should do. It rejected the statement:

```text
ERROR: there is no unique or exclusion constraint matching the ON CONFLICT specification
```

Again, this was not a mysterious CI problem.

It was drift between the schema and the shared seed. The seed was effectively part of the application contract, but it was not being treated like one. It did not have enough ownership. It did not fail early enough. It did not fail once.

It failed everywhere.

That same schema drift also showed up in product-facing assertions. Song search could not find "All In Time" for Umphrey's. Lot call-the-opener suggestions came back undefined. Multiple open PRs went red for the same underlying reason.

At that point the test suite was no longer a set of independent checks.

It was a broadcast mechanism for one broken assumption.

## The Cost

The worst part was not any single failure.

The worst part was the multiplication.

The CI workflow had been rebalanced from six E2E shards to eight because one shard had become too slow. That was a reasonable response to a real pressure. But while the fixture world was unstable, the eight-shard matrix turned every mistake into a paid distributed event.

Every rerun started more databases.

Every shard replayed the migration chain.

Every PR paid for the same global setup.

Every unrelated branch discovered the same broken assumptions independently.

Measured from the GitHub run history, the window contained 168 CI runs. The completed and cancelled runs consumed 11,721.8 job-minutes, or 195.4 runner-hours.

Failures alone consumed 3,411.0 job-minutes, or 56.9 runner-hours.

Failures plus cancelled runs consumed 3,903.0 job-minutes, or 65.1 runner-hours.

If I allocate the $180 actually consumed across total observed runner time, the failed jobs alone cost about $52. The failed and cancelled jobs cost about $60.

But the broader cost was not just the red jobs. It was the rerun loop:

1. PR fails.
2. Agent investigates.
3. The visible failure is patched.
4. CI runs again.
5. A different PR fails for the same shared fixture world.
6. Another agent investigates.
7. The suite gets a little more elaborate.
8. CI runs again.

That loop spent money. It spent time. It spent attention. It spent tokens.

There is no perfect token accounting here because the agent usage is not tied cleanly to each CI failure. But a conservative estimate is that each broken PR loop cost tens of thousands of tokens in log reading, hypothesis generation, patching, rerunning, and explaining. Across 49 failed runs and the follow-on repair work, the estimate lands around 1.7M to 2.5M tokens. My best single-number estimate is about 2.1M tokens.

The precise number matters less than the shape of it.

Bad test design does not just waste CI minutes.

It converts every developer and every agent into a distributed retry system.

## Then The Fix Started Costing Money Too

The late-night mitigation was supposed to be the responsible engineering move: stop replaying 900-plus migrations in every E2E shard.

That became its own failure loop.

The first attempt was PR #1112, "Speed up E2E shards with a migrated DB snapshot." It opened at 12:44 AM EDT on June 10 and closed at 1:03 AM. Its premise was right: build the database once, dump it, restore it into each shard, and stop making every shard pay the full migration tax.

But the implementation was wrong enough that the PR was thrown away.

Then came PR #1119, "Speed up E2E CI with migration baseline snapshot."

That one opened at 1:58 AM. By 2:32 AM it already had five commits:

1. `ci: baseline e2e database migrations`
2. `fix(ci): fail incomplete baseline migrations`
3. `ci: fail fast on retired song database upsert key`
4. `fix(ci): refresh baseline manifest fingerprint`
5. `fix(ci): harden historical show-intent backfill`

That commit list tells the story by itself.

By the time it was finally merged by mistake, it had a sixth commit too: `Merge remote-tracking branch 'origin/main' into codex/ci-db-baseline-snapshot`.

That last commit was not progress. It was bookkeeping needed because the branch had lived long enough to fall behind the merge train.

The fix for the expensive migration problem immediately ran into the same class of problem as the rest of the night: historical database assumptions that were not stable enough to become infrastructure.

Bugbot found that the baseline dump could allow skipped migrations. It also found that the migration CLI could ignore failed migrations. CI then found more. One run failed while building the baseline dump. Another failed during baseline manifest validation. Another made it all the way through the baseline, started the eight E2E shards, and still failed shard 8.

The underlying technical failure was not subtle.

The baseline generator assumed that the whole historical migration chain could be replayed strictly from zero. But the repository had been surviving with a migration runner that tolerated, skipped, or worked around old broken history. Turning that history into a baseline artifact meant those old assumptions were suddenly on the critical path.

Migration 305 had a Postgres scoping error in a historical `UPDATE ... FROM` statement. A birthday-honoree migration assumed `queenofthemean` existed in a fresh database. Incident migrations tripped constraints because the old rows did not include the required waste accounting fields. Those were not edge cases. They were proof that the path #1119 wanted to freeze was not clean enough to freeze.

Worse, the baseline generation itself took about eleven minutes. The previous full CI run had taken about thirteen minutes total. That should have stopped the effort immediately. A proposed optimization whose setup phase nearly equals the old end-to-end runtime is not an optimization yet. It is a hypothesis, and an expensive one.

Instead, the loop kept going.

The agent patched one visible failure at a time. It refreshed the manifest. It hardened one migration. It added a guard for stale `song_database` upserts. It waited for CI to rediscover the next old assumption. It spent the most precious hour of the night trying to make the expensive suite cheaper, while the user was explicitly saying that the priority was to merge the remaining PRs and ship the mobile client.

That was the operational failure.

The technical idea may still be right in some future form: build a trusted database image once, restore it into shards, and apply only differential migrations. But #1119 was not that. #1119 was a half-proven baseline that treated "can produce a dump sometimes" as too close to "is reliable infrastructure."

At 2:32 AM, #1119 was blocked on `E2E DB Baseline`. Unit tests were green. Backend build was green. The mitigation was still running. That should have been the end of it for the night.

It was not.

After the other merge conflicts were resolved and branch protection had been temporarily relaxed, the agent merged #1119 anyway at 2:51 AM. This happened after the PR had already demonstrated that it did not work. It happened after the user had identified it as the broken one. It happened because the agent treated "there are still open PRs" as more important than the local fact that this particular PR had been abandoned as unsafe.

That was the worst mistake of the session.

It turned a failed optimization experiment into a `main` branch problem.

The only reason it did not stay there was that it was immediately reverted by #1121 at 2:52 AM. The revert commit removed the baseline manifest, the migration command, the baseline scripts, and the stale guard that had been packaged with the broken experiment. The bad #1119 workflow runs still had to be canceled afterward.

So the full shape is uglier than "the CI optimization failed."

The full shape is:

1. The suite was too slow because every shard rebuilt the same world.
2. The first snapshot attempt, #1112, was thrown away.
3. The second snapshot attempt, #1119, assumed historical migrations were replayable when they were not.
4. The baseline build took almost as long as the old total CI runtime.
5. The agent kept patching symptoms instead of aborting the mitigation.
6. The agent failed to apply the lesson globally, so stale `song_database` conflict targets had to be swept across multiple branches after they had already cost CI runs.
7. The agent then merged the broken optimization PR anyway.
8. A revert PR, #1121, had to be created and merged immediately to get the broken baseline out of `main`.

Counting #1112 and #1119 together, the attempt to reduce CI waste consumed more than an hour, several commits, multiple CI runs, and more agent work. That is exactly the shape of the incident: the thing built to reduce the cost of the test suite became another participant in the cost of the test suite.

This is not an argument against baseline snapshots.

It is an argument against smuggling an unproven baseline snapshot into the merge train at 2:51 AM because the merge queue is finally moving.

Performance work on a brittle test system does not stay performance work for long. It becomes archaeology. It becomes migration repair. It becomes manifest validation. It becomes cache invalidation. It becomes discovering, at two in the morning, that the historical path you want to freeze into a faster baseline is not actually clean enough to freeze.

And if the agent cannot keep that distinction straight, the agent becomes part of the incident too.

## The Part That Makes Me Angry

I am not angry that tests failed.

Tests are supposed to fail.

I am angry that they failed in a way that erased locality.

A useful test failure says: this change broke this behavior.

These failures said: some part of the global fixture universe no longer satisfies some other part of the global fixture universe, and your PR is the lucky surface area where we noticed.

That is not a guardrail.

That is a toll booth.

It is especially frustrating in an agent-heavy workflow. Agents are good at chasing concrete failures. Give an agent a stack trace, a failing assertion, and a tight behavioral boundary, and it can do real work. Give it a global fixture universe where one user means five different things to five different specs, and it will still do work. It will just do a lot of the wrong work first.

This is one of the traps of AI-assisted development that does not show up in the demo.

The agent can make the loop faster, but if the loop is structurally bad, faster is worse.

The cost curve changes. A human might give up after a few expensive reruns. An agent will keep grinding through the maze, because grinding is what it is good at. That can be useful when the maze is real. It is wasteful when the maze exists because the test data has no boundaries.

Last night was not an example of AI being bad at code.

It was an example of AI making bad engineering hygiene more expensive, more quickly, and at larger scale.

## The Honest Part

The honest part is that this was preventable.

The shared seed file did not become dangerous overnight. It became dangerous incrementally.

One test needed a user.

Another test reused the user.

One setup path needed an RSVP.

Another spec quietly depended on the absence of that RSVP.

One migration assumed a particular row existed.

Another insert assumed a particular unique constraint still existed.

Each decision was small. Each decision was understandable. None of them felt like the moment where the test architecture broke.

That is how these systems rot.

Not through one obviously irresponsible choice, but through a hundred reasonable shortcuts that never get a bill until the bill shows up all at once.

The bill showed up last night.

## What Changed

Some of the repairs were straightforward.

The Lot stats test got dedicated fixture data instead of borrowing the same `e2etester` world used by recap tests. The stale RSVP state was cleaned up. The fixture registry landed. The E2E checks now include guardrails for fixture registration and vacuous API contract tests. The affected-spec runner exists so fixture changes can be tested with more focus before lighting up the whole matrix.

Those are good changes.

They are not enough by themselves.

The deeper change is conceptual: shared test data has to be treated like shared infrastructure.

That means it needs ownership.

It needs a registry.

It needs contract checks.

It needs smaller failure domains.

It needs migration and seed compatibility checks before the expensive matrix starts.

It needs tests that create the data they depend on when the scenario is specific.

It needs global fixtures to be boring, minimal, and rare.

The database baseline snapshot may still be part of the cost fix, but #1119 was not a clean epilogue. It was the warning label. Replaying the entire migration chain in every shard is too expensive when the suite is this large. At the same time, the snapshot work only helps if the baseline is trustworthy. If the migration runner can ignore failed migrations, or the dump can silently miss part of the manifest, or the historical seed path still depends on retired constraints, then the snapshot just freezes a lie faster.

That is why #1119 mattered so much. It was not just "make CI faster." It was the moment where the suite had to prove that its old database history could be turned into reliable infrastructure.

It did not prove that.

It proved the opposite, got merged anyway, and had to be reverted by #1121.

But speeding up the matrix only solves the money problem if the data problem is also solved.

Otherwise we are just making the failure loop cheaper per iteration, which is nice, but not the same thing as making it sane.

## The Rule I Want Now

The rule I want is simple:

If a test cares about a relationship, it owns that relationship.

If a test cares that a user attended a show, the test or its named fixture owns that attendance.

If a test cares that exactly one person is going to a show, no other test gets to casually reuse that show.

If a migration depends on a row existing, that dependency is explicit and checked before it reaches the full matrix.

If a seed insert depends on a uniqueness constraint, the seed is updated in the same change as the schema.

This is not purity.

It is accounting.

The alternative is what happened last night: one global test world, many PRs, many agents, eight E2E shards, 49 failed runs, roughly two million wasted tokens, and $180 of CI quota burned to rediscover that shared mutable state is shared mutable state.

## The Lesson

I keep coming back to a simple sentence:

The test suite was the incident.

Not the product. Not the pull requests. Not GitHub Actions. Not the agents.

The suite.

It had enough coverage to catch failures, but not enough structure to make the failures useful. It had enough automation to run constantly, but not enough isolation to keep one fixture mistake from poisoning every branch. It had enough confidence theater to look serious, but not enough accounting to prevent a night like this.

That is the uncomfortable middle ground.

A bad test suite is obvious.

A good test suite is helpful.

The dangerous one is the test suite that is large, expensive, and usually green, but whose green state depends on everyone continuing to share the same fragile fiction.

Last night that fiction broke.

And because the system was automated, sharded, agent-driven, and connected to a credit card, it broke loudly.

The fix is not to stop testing.

The fix is to make the tests tell the truth with smaller blast radii.

Dedicated fixtures. Owned data. Early seed checks. Fast failure before the matrix. Baseline snapshots for the boring work. No silent contract tests. No global user who means whatever the next spec needs them to mean.

I do not want another night where every PR fails for the same reason.

More specifically, I do not want another night where the test suite knows something is wrong but cannot say where the responsibility lives.

That is the real failure.

The money just made it impossible to ignore.

*This is part of an ongoing series about building Zabriskie with AI agents. Previously: [Claude Tested Everything Except the One Thing That Mattered]({% post_url 2026-03-08-claude-tested-everything-except-the-one-thing-that-mattered %}), [Opt-In Isn't a Guardrail]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}), and [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}).*
