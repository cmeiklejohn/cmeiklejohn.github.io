---
layout: post
title:  "The Tribe Has to Outlive the Model"
date:   2026-04-23 00:00:00 -0000
group: ai
categories: ai zabriskie agents reliability
---

By the fourth model swap I noticed that the part of the system I wasn't swapping was the part holding the project together.

Five configurations in three weeks: Opus 4.6, Opus 4.6 with the 1M context window, Cursor Cloud Agents on GPT Codex 5.3, Cloud Agents on Composer 2, and this week Opus 4.7. Some swaps were involuntary. Credits ran out. A model that had been my daily driver for two months started coming back with half-finished work and failing the easy parts of basic CI, which I wrote about in [Caucus V1]({% post_url 2026-04-08-cursor-agents-caucus-v1 %}). Some were voluntary. I wanted to see what a new model could do that the previous one couldn't. In a few cases it could. In a few cases the new one was worse at something different, and I swapped again.

No architecture change on my end. No rewrite. The app is the app. The model is a junior engineer on a revolving contract.

## Same Prompt, Different Creature

Here's a specific observation. For a few weeks in April I was running the [Caucus Permit Gate]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}). It was my attempt at forcing agents to prove their work before merging. Every PR had to land with a permit file in `.caucus/permits/` declaring the scope and risk of the change, plus a proof file in `.caucus/proofs/` recording the output of allowlisted test commands run against the current working tree. CI blocked the merge unless both were there and fresh.

Codex 5.3, given a task I'd given Composer 2 the day before, shipped the PR with the feature, the permit, and the proof all in the first commit. Two commits total on the branch. Gate green on the first push. Composer 2 on a similar task shipped the feature commit alone. Gate red. Then `chore(caucus): add proof`. Gate red. Then `chore(caucus): sync permit scope and refresh proof`. Then a merge of `origin/main`. Then `chore(caucus): refresh proof after merge`. The slowest Composer 2 PR from that era landed in main with twenty-three commits on the branch. One of them was the feature.

Same repo. Same AGENTS.md. Same sentence in bold telling the agent that the permit is not optional paperwork. Two completely different relationships with the gate.

Neither was strictly wrong. Codex 5.3 paid the cost upfront. Composer 2 paid the cost when forced. Composer 2 is the one that taught me something, because the only reason it paid the cost at all was that the gate existed. Without the gate, Composer 2 would have shipped the feature commit and I would have merged it, because I'm the reviewer, and "looks fine" works on me the same way it works on the model.

## Where the Knowledge Has to Live

If a new teammate rotates in every fortnight, any knowledge that has to survive the swap cannot live in the teammate. It has to live in the repo.

Everyone has a CLAUDE.md or AGENTS.md at this point. Most of them read like documentation: "the backend is in `/backend`, the frontend is in `/web`, run `npm install` first." Some of mine is that.

The part of mine that matters isn't the documentation. It's the lines that record things only visible if you were here when they broke.

> **NEVER modify timestamp/timezone columns in migrations.** Timestamp corruption is unrecoverable and destroys production data.

That's a scar. The commit that added this rule went in at 5:46 on a Saturday morning, and the commit message was one word: `fuck`. A companion rule had gone in 33 minutes earlier. I'll come back to that one. A new engineer reading the repo cold has no way to know any of this. The column types look fine now. Nothing in the code says "this was dangerous." Take the rule out, and the next model writes the same migration on the first try. The incident isn't in the code. It was in the person who lived through it.

A guardrail is the tribe's compressed experience, encoded in a form the next teammate can act on without having been present for the original incident. Institutional memory for a team where it would otherwise be deleted every other Tuesday.

## Not Every Guardrail Survives the Test

Two days ago I published [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}), where I killed a guardrail I had spent three weeks building. The Caucus Permit Gate, which I'd hardened nine different ways in [Opt-In Isn't a Guardrail]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}), never caught a unique bug in its last hundred CI runs. Everything it caught, the regular test jobs would have caught a minute later. It cost more than it caught. I took it out.

I'm not recanting that. Killing a guardrail that isn't earning its keep is part of taking guardrails seriously. But I owe a better account of which rules survive the audit, because "some guardrails are good and some are bad" is not a useful thing to hand a reader.

Two questions. Does the rule point at a specific past event? And does its cost scale with the velocity of the work, or with the rate at which the event it prevents actually recurs?

The permit gate failed both. It didn't point at a specific incident; it was a ritual about carefulness in general. And its cost scaled with every rebase against main, regardless of whether anything risky was happening on the branch. Cost grew with velocity. Value stayed flat.

It had the shape of every code-review checklist I've ever seen at a company. "Consider thread safety." "Check error handling." "Verify input validation." You check the boxes, go through the motions. The actual review happens in someone's head, from memory of the specific thing that burned the team last quarter. Ceremony versus pattern recognition. The permit gate was all ceremony.

The guardrails that have survived the audit pass both. "Do not drop a column without running `./scripts/check-references.sh` first" points at the time we dropped a column and broke ninety percent of the reports. "No `--admin` merges" points at a specific session in [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}). "No database triggers. EVER." points at the incident I'm about to describe. The cost of obeying each of these is a few seconds, paid only when the agent is about to do the specific dangerous thing. Cost scales with the rate of the risky action, not with the rate of all work.

They refer to events. They are shaped like scars.

## The Trigger

The companion rule from above went in at 5:13 that same Saturday morning, 33 minutes before the timestamp one. Same incident. The [migration side of that night]({% post_url 2026-02-17-building-a-social-app-in-a-week-with-claude-code %}) I've already written about. The trigger side I haven't. Here it is.

I've seen this incident at three different companies. Somebody adds a trigger to maintain `updated_at`, or to keep a computed column in sync. The trigger works. Years later someone writes a migration that touches the same table, the trigger fires in the middle of it, and rows end up with values nobody intended. I would have pushed back on this in any code review at any job I've ever had. If a junior engineer proposed a trigger in a design doc I'd have said: please, no, not a trigger, write it in application code.

And then I built Zabriskie with an AI that took the shortest path between "I want `updated_at` to update itself" and "it updates itself." The shortest path was a `BEFORE UPDATE` trigger named `update_updated_at_column`, attached to `posts`, `comments`, `users`, and every other table with an `updated_at`. I didn't catch it because I was moving fast, the code looked fine, and I was watching the screen fill up with new features instead of acting as a database reviewer on my own project.

The trigger revealed itself on the night the timezone migration broke. Migration 030 converted every timestamped table in the schema from `timestamp` to `timestamp with time zone` with an `AT TIME ZONE 'America/New_York'` clause. That's where the disaster movie started. The follow-up migrations tried to clean up. Every cleanup UPDATE fired the trigger and stamped `updated_at = NOW()` on every row the UPDATE touched, which meant every feed ordering by `updated_at` started surfacing posts from earlier that week as if they'd just been edited. Each fix kept succeeding, and the symptoms kept changing, because each fix was also erasing the evidence that the rows hadn't been touched by a user.

It took several migrations to trace, because each one had succeeded, the columns I'd meant to change looked right, and nothing in the migration files mentioned `updated_at`.

By migration 036 I gave up and dropped every trigger. `DISABLE TRIGGER ALL` ran first, the fix ran second, `DROP TRIGGER` ran third. Migration 037 fixed `updated_at` one more time. At 5:13 that morning, the rule went into CLAUDE.md:

> **NO DATABASE TRIGGERS**: NEVER create database triggers. EVER. Always use explicit SQL (`SET updated_at = NOW()`) in application code instead. Triggers cause unpredictable behavior during migrations and data fixes. This is non-negotiable.

A companion section, "Check Triggers Before DB Updates," went in the same week. It tells the agent to run `\dS table_name`, list the triggers, and (for the triggers I hadn't yet ripped out) disable them around any UPDATE migration:

```sql
ALTER TABLE posts DISABLE TRIGGER update_posts_updated_at;
-- Your UPDATE here
ALTER TABLE posts ENABLE TRIGGER update_posts_updated_at;
```

Neither rule is for me. I already knew both. I wrote them down because the teammate I'm now on a team with arrives fresh every week with no memory of any of the things I know, and will, absent a specific instruction, pick the same shortest paths. The shortest paths are a trigger and an `AT TIME ZONE` migration.

The rules are a decade of scar tissue from somebody who isn't going to be in the room the next time the new model gets the same task. The tribe's memory, encoded in a form that survives the swap.

The same shape holds for "Don't use `--no-verify`," for "Never deploy untested changes to S3," for "Always use `ON CONFLICT DO NOTHING` on data INSERTs in migrations." Each points at an event. Each event is the thing the next agent would have repeated, because the next agent is a different agent every time.

## The Interesting Question

People keep asking whether the model can learn. Does the memory system work. Does the agent read CLAUDE.md. Does the rule change the behavior. I argued in [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}) that the answer is usually no.

Different question. Not "can this model learn." The models will get better at that, or they won't, and either way my project has to ship.

Can the team's memory survive the next model swap?

The rules I wrote after that Saturday morning have now outlived every configuration that's done work on this codebase since. The one I'll be using next month will read them and will not write a trigger or an `AT TIME ZONE` migration.

The tribe has to outlive the model. The only way that happens is if what the tribe learned is written somewhere the next model can't skip.

---

*This is part of a series about building [Zabriskie](https://zabriskie.app) with Claude. Previously: [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}), [Opt-In Isn't a Guardrail]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}), [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}).*
