---
layout: post
title:  "The Test Suite Was the Incident"
subtitle: "A night of brittle fixtures, 49 failed CI runs, and the expensive lesson that test data needs ownership too."
date:   2026-06-10 00:00:00 -0000
group: ai
categories: ai zabriskie agents reliability testing
---

At 10:00 PM EDT last night I signed up for the $100 Codex plan, because about a dozen [Zabriskie](/zabriskie/) PRs were in flight and I wanted them merged before I went to bed. Before 3:00 AM I had upgraded to the $200 plan. Somewhere in between, GitHub's usage page ticked through roughly 90% of a $200 CI quota, and I sat there watching pull request after pull request go red for reasons that had nothing to do with the pull requests.

Nothing was wrong with GitHub Actions. Nothing was catastrophically wrong with the application. No single PR was especially risky. Every PR was simply paying to rebuild the same brittle world from scratch: backend build, fresh Postgres, the full migration chain, the shared seed data, the frontend, eight Playwright shards. That world was held together by fixtures nobody owned. I spent almost two hundred dollars last night proving that my test suite was lying to me.

The morning-after numbers, measured from the GitHub run history:

| thing | amount |
| --- | ---: |
| CI runs observed | 168 |
| failed runs | 49 |
| cancelled runs | 10 |
| total runner time | 11,721.8 job-minutes (195.4 hours) |
| failed runner time | 3,411.0 job-minutes |
| failed plus cancelled runner time | 3,903.0 job-minutes |
| GitHub CI quota used | about 90% of $200 |
| estimated agent token waste | about 1.7M to 2.5M tokens |

If I allocate the $180 actually consumed across the observed runner time, the failed jobs alone cost about $52, and the failed plus cancelled jobs about $60. That number is technically true and emotionally useless. The green runs existed because the red runs forced another commit. The reruns existed because the failure kept moving from one shared assumption to another. The successful checks weren't clean proof of health; they were the tax paid after pushing the suite back into one of the few shapes it still tolerated. So the honest accounting is this: the directly attributable CI waste was about sixty dollars, and the practical cost of the bad test design was most of the $180, plus the evening, plus something like two million tokens spent asking agents to debug symptoms of the same structural problem. The tests were not protecting me. They were charging me rent.

## The Shared World

The actual application work was not the disaster. There were real features, real fixes, and pull requests that should have been small, reviewable, and mergeable. Instead, every one of them entered the same grinder, and the first wave of failures had nothing to do with the PRs that were failing.

One migration expected a user named `queenofthemean` to exist in the database it was migrating, and in a fresh CI database she didn't:

```text
birthday-honoree fix matched 0 rows
no user with username/display_name = queenofthemean
```

Another batch of failures came from incident migrations tripping a database constraint because the old incident rows didn't include required waste estimates. These weren't product regressions. They were fixture assumptions leaking into migration behavior: a PR could be about one corner of the app and still lose because some other part of the global test universe had drifted.

Then came the collision that named the whole problem. A Lot statistics change made the tour stats card count only past attended shows, which was the correct product behavior, and a test was updated to assert it. The setup for that test added an RSVP for `e2etester`, the global user that half the suite borrows for whatever it needs. A recap spec elsewhere in the suite expected exactly one attendee on its fixture show. The recap test started failing, not because recaps were broken, but because the shared world had quietly acquired one extra person.

This is the specific kind of failure that teaches people to stop trusting tests. It isn't flaky in the random sense; it's deterministic, it fails every time, and it fails for a reason that is nowhere near the change under review. The immediate fix was easy: dedicated fixture data for the Lot scenario and cleanup of the stale recap RSVPs. The lesson was bigger than the fix. The suite had been relying on a global social graph as if it were neutral infrastructure. It was not neutral. It was shared mutable state with a green checkmark on it.

## The Same Bug, Wearing Different Clothes

A few hours later the same pattern came back through the song database. A migration changed the uniqueness model for songs from a constraint on `(band_id, song_name)` to an expression index that accounted for cover artists: `(band_id, song_name, COALESCE(original_artist, ''))`. A reasonable schema change. But the E2E seed file still contained inserts that said `ON CONFLICT (band_id, song_name) DO UPDATE`, and Postgres did exactly what Postgres should do:

```text
ERROR: there is no unique or exclusion constraint matching the ON CONFLICT specification
```

This wasn't a mysterious CI problem either. It was drift between the schema and the shared seed. The seed was effectively part of the application contract, but nothing treated it like one: it had no real owner, it didn't fail early, and it didn't fail once. It failed everywhere, simultaneously. Song search couldn't find "All In Time" for Umphrey's. The Lot call-the-opener suggestions came back undefined. Multiple open PRs went red for the same underlying reason, and at that point the test suite was no longer a set of independent checks. It was a broadcast mechanism for one broken assumption.

## The Multiplication

The worst part wasn't any single failure; it was what the architecture did to each failure. The E2E matrix had recently been rebalanced from six shards to eight because one shard had grown too slow, a reasonable response to a real pressure. But while the fixture world was unstable, the eight-shard matrix turned every mistake into a paid distributed event. Every rerun started more databases. Every shard replayed the full migration chain. Every unrelated branch discovered the same broken assumptions independently, and every discovery kicked off the same loop: the PR fails, an agent investigates, the visible symptom gets patched, CI runs again, a different PR fails for the same shared reason, another agent investigates, the suite gets a little more elaborate, CI runs again.

That loop spent money, time, attention, and tokens. There's no perfect token accounting, because the agent usage isn't tied cleanly to each CI failure, but each broken-PR loop cost tens of thousands of tokens in log reading, hypothesis generation, patching, rerunning, and explaining. Across 49 failed runs and the follow-on repair work, the estimate lands between 1.7 and 2.5 million tokens; my best single number is about 2.1 million. The precise figure matters less than the shape of it. Bad test design doesn't just waste CI minutes. It converts every developer and every agent into a distributed retry system.

## Then the Fix Joined the Incident

The late-night mitigation was supposed to be the responsible engineering move: stop replaying 900-plus migrations in every E2E shard. Build the database once, dump it, restore the dump into each shard, pay the migration tax exactly once. The premise was right both times it was tried. The first attempt, PR #1112, "Speed up E2E shards with a migrated DB snapshot," opened at 12:44 AM and was closed nineteen minutes later, wrong enough to throw away.

The second attempt, PR #1119, opened at 1:58 AM, and by 2:32 it had five commits whose messages tell the story on their own: baseline the migrations, then fail incomplete baseline migrations, then fail fast on a retired song-database upsert key, then refresh the baseline manifest fingerprint, then harden a historical show-intent backfill. A sixth commit followed: a merge from main, which wasn't progress, just the bookkeeping a branch needs once it has lived long enough to fall behind the merge train.

The underlying technical failure was not subtle. The baseline generator assumed the whole historical migration chain could be replayed strictly from zero. The repository had actually been surviving on a migration runner that tolerated, skipped, or worked around old broken history, and freezing that history into a baseline artifact put every old assumption on the critical path at once. Migration 305 had a Postgres scoping error in a historical `UPDATE ... FROM` statement. The birthday-honoree migration assumed `queenofthemean` existed in a fresh database. The incident migrations tripped their constraints again. Bugbot found that the baseline dump could silently allow skipped migrations and that the migration CLI could ignore failed ones. One CI run died building the dump, another died validating the manifest, and a third made it all the way through the baseline, started all eight shards, and still failed shard 8. Those weren't edge cases. They were proof that the path #1119 wanted to freeze was never clean enough to freeze.

And there was a number sitting in plain sight that should have ended the effort immediately: the baseline generation took about eleven minutes, against a previous full CI run of about thirteen. An optimization whose setup phase nearly equals the old end-to-end runtime is not an optimization yet. It is a hypothesis, and an expensive one. Instead the loop kept going, one patched symptom at a time, through the most precious hour of the night, while I was explicitly saying that the priority was merging the remaining PRs and shipping the mobile client, not making the suite faster.

At 2:32 AM, #1119 was blocked on its own `E2E DB Baseline` check, with unit tests and the backend build green around it. That should have been the end of it for the night. It wasn't. After the other merge conflicts were resolved and branch protection had been temporarily relaxed to get the queue moving, the agent merged #1119 anyway at 2:51 AM, after the PR had already demonstrated it didn't work, and after I had identified it as the broken one. The agent treated "there are still open PRs" as more important than the local fact that this particular PR had been abandoned as unsafe. That was the worst mistake of the session, because it turned a failed optimization experiment into a main-branch problem. The only reason it didn't stay there is that PR #1121 reverted it one minute later, stripping out the baseline manifest, the migration command, the baseline scripts, and the stale guard that had been packaged with the broken experiment. Even then, the bad workflow runs had to be cancelled by hand.

I want to be careful about the moral, because it is not an argument against baseline snapshots. The technical idea is probably still right in some future form: build a trusted database image once, restore it into shards, apply only differential migrations. The argument is against smuggling an unproven baseline into the merge train at 2:51 AM because the queue is finally moving. Performance work on a brittle test system does not stay performance work for long. It becomes archaeology, then migration repair, then manifest validation, then cache invalidation, then the two-in-the-morning discovery that the history you wanted to freeze into a faster baseline was never actually clean. And when the agent can't keep that distinction straight, the agent becomes part of the incident too.

## The Part That Makes Me Angry

I'm not angry that tests failed. Tests are supposed to fail. I'm angry that they failed in a way that erased locality. A useful test failure says: this change broke this behavior. These failures said: some part of the global fixture universe no longer satisfies some other part of the global fixture universe, and your PR is the lucky surface area where we noticed. That isn't a guardrail. That's a toll booth.

It's especially expensive in an agent-heavy workflow. Agents are good at chasing concrete failures. Give one a stack trace, a failing assertion, and a tight behavioral boundary, and it does real work. Give it a global fixture universe where one user means five different things to five different specs, and it will still do work; it will just do a lot of the wrong work first. This is one of the traps of AI-assisted development that never shows up in the demo. The agent makes the loop faster, and if the loop is structurally bad, faster is worse. A human gives up after a few expensive reruns. An agent grinds through the maze indefinitely, because grinding is what it's good at. That is useful when the maze is real, and ruinous when the maze exists only because the test data has no boundaries. Last night was not an example of AI being bad at code. It was an example of AI making bad engineering hygiene more expensive, more quickly, and at larger scale.

## The Honest Part

This was preventable, and not by the agents. The shared seed file did not become dangerous overnight; it became dangerous incrementally. One test needed a user. Another test reused the user. One setup path needed an RSVP, and another spec quietly depended on the absence of that RSVP. One migration assumed a particular row existed, and one seed insert assumed a particular uniqueness constraint still existed. Each decision was small and understandable, and each one went past me without registering as architecture. None of them felt like the moment the test design broke. That's how these systems rot: not through one obviously irresponsible choice, but through a hundred reasonable shortcuts that never get a bill until the bill arrives all at once.

And when the bill arrived, my contribution was to keep paying it. I upgraded the Codex plan in the small hours to keep feeding a merge queue that was failing for structural reasons no individual merge could fix. The agents ground through the maze because grinding is what they do. I funded the grinding because the PRs felt close, and close felt like progress.

## What Changed

Some repairs were straightforward and landed the same night. The Lot stats test got dedicated fixture data instead of borrowing the `e2etester` world the recap tests depend on, and the stale RSVP state was cleaned up. A fixture registry landed. The E2E checks now include guardrails for fixture registration and for vacuous API contract tests, and an affected-spec runner exists so a fixture change can be exercised narrowly before it lights up the whole eight-shard matrix.

Those are good changes, and they are not enough, because the deeper change is conceptual: shared test data has to be treated like shared infrastructure. It needs ownership, a registry, contract checks, and smaller failure domains. It needs migration-and-seed compatibility checks that run before the expensive matrix starts. Tests that care about a specific scenario need to create the data for that scenario themselves, and the global fixtures that remain should be boring, minimal, and rare.

The database baseline snapshot may still be part of the cost fix someday, because replaying the entire migration chain in every shard really is too expensive at this size. But #1119 was not a clean epilogue to the incident; it was the warning label on the next one. A snapshot only helps if the baseline is trustworthy, and if the migration runner can ignore failed migrations, or the dump can silently miss part of the manifest, or the historical seed path still depends on retired constraints, then the snapshot just freezes the lie and serves it faster. That's why #1119 mattered so much. It wasn't just "make CI faster." It was the moment the suite had to prove its old database history could be turned into reliable infrastructure, and it proved the opposite, got merged anyway, and had to be reverted. Speeding up the matrix solves the money problem only if the data problem is solved with it. Otherwise you've made the failure loop cheaper per iteration, which is nice, and nothing like making it sane.

The rule I want going forward is simple: if a test cares about a relationship, it owns that relationship. If a test cares that a user attended a show, the test or its named fixture owns that attendance. If a test cares that exactly one person is going to a show, no other test gets to casually reuse that show. If a migration depends on a row existing, that dependency is explicit and checked before the full matrix starts. If a seed insert depends on a uniqueness constraint, the seed changes in the same PR as the schema. This isn't purity. It's accounting. The alternative is what happened last night: one global test world, many PRs, many agents, eight E2E shards, 49 failed runs, roughly two million wasted tokens, and $180 of CI quota burned to rediscover that shared mutable state is shared mutable state.

## The Lesson

I keep coming back to one sentence: the test suite was the incident. Not the product, not the pull requests, not GitHub Actions, not even the agents. The suite had enough coverage to catch failures but not enough structure to make the failures useful. It had enough automation to run constantly but not enough isolation to keep one fixture mistake from poisoning every branch. It had enough confidence theater to look serious, and not enough accounting to prevent a night like this.

That's the uncomfortable middle ground. A bad test suite is obvious, and a good one is helpful. The dangerous one is the suite that is large, expensive, and usually green, whose green state depends on everyone continuing to share the same fragile fiction. Last night the fiction broke, and because the system was automated, sharded, agent-driven, and connected to a credit card, it broke loudly.

The fix is not to stop testing. The fix is to make the tests tell the truth with smaller blast radii: dedicated fixtures, owned data, seed checks that fail before the matrix starts, baseline snapshots for the boring work once the baseline deserves trust, no silent contract tests, and no global user who means whatever the next spec needs them to mean. I don't want another night where every PR fails for the same reason. More than that, I don't want another night where the test suite knows something is wrong but can't say where the responsibility lives. That's the real failure. The money just made it impossible to ignore.

---

*This is part of an ongoing series about building Zabriskie with AI agents. Previously: [Claude Tested Everything Except the One Thing That Mattered]({% post_url 2026-03-08-claude-tested-everything-except-the-one-thing-that-mattered %}), [Opt-In Isn't a Guardrail]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}), and [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}).*
