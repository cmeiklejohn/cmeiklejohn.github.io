---
layout: post
title:  "Local Reasoning"
subtitle: "The app died at Red Rocks while its health checks stayed green. Every line of code was fine. The sum was nobody's job."
date:   2026-08-31 02:00:00 -0400
group: ai
categories: ai zabriskie agents reliability performance distributed
---

[Zabriskie](https://zabriskie.app/) is a social app I'm building around music, films, books, art, and the people who care about them. I build it entirely through vibe coding: I describe what I want and evaluate the result in the browser, but I don't read the implementation code. Coding agents write everything, the implementation, the tests, the audits. Claude Code, Cursor, and Codex have all merged work here. My job is deciding what to build and looking at what comes out.

On August 28 I was at Red Rocks for the second of two nights of the band Goose, and more people were on Zabriskie around me than had ever been on it at once. I spent the evening debugging instead of watching the show, because people kept walking up to tell me the app wasn't working. They were right. The home surface, a page called [the Lot](https://zabriskie.app/v2/lot), mostly wouldn't load. Requests that normally finish in well under a second were taking tens of seconds or timing out entirely. For an app whose whole point is telling you what's being played right now, a page that answers in forty seconds isn't degraded, it just has no answer.

Debugging from a seat at a show, with nothing but a phone, meant asking an agent what was wrong with the site. It couldn't tell me. So I started pasting everything I could find in the Railway console into a cloud agent, screen by screen. The screen that mattered showed a queue of identical writes all blocked on the same lock, and the first diagnosis of the night came out of it: lock contention, a database deadlocking under load.

Half right. The queue was real, a client bug was re-registering push tokens over and over, and the first fix merged that night made it stop. But Postgres never reported a deadlock, the reads that reached it ran fast, and the health endpoint stayed green from the first stalled page to the last. The actual problem was a layer up, in my own app. Requests were stuck waiting to borrow a database connection from the app's pool, the fixed set of twenty-five open lines it keeps to its database, before their queries ever reached Postgres at all. Nothing measured that wait and nothing logged it. The agent reading my screenshots wasn't hallucinating. It got handed a true fact from the wrong layer, because the right layer had no facts to give.

I want to be careful about the lesson here, because "the AI wrote bad code" isn't it. Taken one change at a time, almost all of the code is reasonable. The problem is that an agent reasons locally. It sees the function it's editing, the file around it, the change it's about to make, and within that boundary its judgment is mostly fine. The things that took the app down aren't visible at that scale. How many connections a request holds, how many queries a page runs, those are properties of the whole program at once, and nobody, me included, had ever looked at them.

## What was actually wrong

Shipping a fix that night was its own adventure. The merge gate wanted a CI check that wouldn't pass, the automation didn't have permission to re-run it, and Railway was having an unrelated incident of its own and couldn't start a deploy. I ended up disabling branch protection from my phone, in my seat, at 02:19 UTC.

What was the fix actually fixing? Every request has to borrow one of those twenty-five connections before it can do anything at all. The code the agents had written would borrow a connection, hold it while walking through a list of results, and borrow a second one for each item on the list. One request doing that is rude but harmless. Twenty-five at once is gridlock: every request holding a connection while waiting for another, and nobody able to move. All of that waiting happened inside my own app, before anything reached the database, which is why the database looked innocent all night. The health check stayed green for an even better reason. It doesn't touch the database at all, it returns a string formatted once at startup. Cheap, fast, and useless.

The gridlock code had been in production since February. What arrived that night was the crowd. The pool didn't help either: nobody chose twenty-five connections for this workload. An agent picked the number at scaffold time, before the app had users, and nobody ever went back. Go's pool will even tell you when it's starving, two counters, `WaitCount` and `WaitDuration`, would have named this failure immediately, and we had never logged them. Almost no query carried a timeout. The facts that mattered most were the ones nobody had asked the system to emit.

The fix rewrote the worst read paths to stop holding a connection while borrowing another, resized the pool, and gave the app the first pool telemetry it has ever had. The honest end of the war story is that I don't know the moment the app came back. The fix was merged while the band was still playing, and it couldn't reach production until Railway recovered.

## The remediation was excellent, which is the problem

Here's the part I find most interesting, because the response was close to exemplary and it still missed the point.

The agents got the mechanism exactly right, wrote a regression test that shrinks the pool to a single connection and reproduces the hang deterministically, and swept the whole backend for the pattern. The first scan claimed 188 sites. The next claimed 217. The real number, once the analysis was rebuilt to check what actually holds a connection, was about forty, and all forty were fixed, with a static check behind them so the class can't come back. The shows list alone went from 151 database round trips per page to 4. By morning the defect class was extinct and guarded. If the story ended there, you'd conclude the loop works.

The next day, an agent looked at one more endpoint and found it executing 51 to 71 SQL statements per request, of which 23 were distinct.

This was the endpoint every home-screen widget and both watch apps poll all day. The new guardrail had nothing to say about it, because it hunts the gridlock pattern, holding one connection while borrowing another, and this endpoint didn't do that. This was a different disease. The part that stays with me is that the costs were documented. This comment sits in the code, verbatim:

```go
// limit 1 — NOT watchOnThisDayLimit. Every card costs two more queries
// (highlight + setlist), and this is called from watchChipText and
// watchChipShowID as well as the handler. Fanning out to 5 at all three call
// sites would take one request from ~9 queries to ~33 to render a chip that
// shows exactly one stat. The rotation pays for rotation; nothing else does.
```

Whoever wrote that, and in this codebase that means an agent, clearly understood the cost model. It prices the request at about nine queries. The measured number was 51 to 71. Nothing enforced the nine, nothing would have noticed when it stopped being nine, and the other contributors to the total lived outside every context window that ever edited this file.

My first instinct was to call this a regression, the next disease showing up right after the first was cured. That's not what happened. Nothing new appeared. These queries had been there all along, and someone finally pointed an instrument at them. Which raises the obvious question: if this endpoint sat at 51 queries per request until somebody happened to look, what is everything else sitting at?

## A census

So I measured. I turned on a setting that writes down every question the app asks the database, loaded each page once as a normal signed-in user, and counted. In the table, statements are how many questions it took to draw the page once, and shapes are how many different questions were in the mix. One request per page, one user, so treat these as shape rather than precision.

| surface | statements per request | distinct shapes |
|---|---|---|
| Shows page | 340 | 14 |
| The Lot (home) | 292 | 164 |
| Band page | 186 | 28 |
| Watch idle-context (the endpoint above) | 72 | 43 |
| Flow feed | 40 | 30 |
| Profile feed | 14 | 8 |
| Notifications | 3 | 3 |

Six more rows sit in between, from tour stats at 60 down to the profile page at 9. The endpoint from the last section, the one that just got its own perf fix, is the fourth worst page in the app. These numbers aren't just latency, either. Every statement stretches how long a request holds its borrowed connection, so a 292-statement page is 292 slices of hold time on the same pool that gridlocked in August. Same arithmetic, different magnitude.

The ratio of statements to shapes splits the table into two diseases. The Shows page is the classic loop: 340 statements but only 14 shapes, a handful of queries repeated once per card for sixty-five cards. The Lot is the more instructive one: 292 statements across 164 distinct shapes. There's no villain loop to point at. It's an accretion. Every feature that ever shipped onto the home screen brought three or four queries with it, each addition individually negligible, and the page now runs about three hundred statements, one after another, every time someone opens the app.

The Shows page bothers me the most, because of where its queries live. They sit inside the function that draws one show card, so every page that draws cards inherits them, and the Shows page draws sixty-five. Neither piece looks expensive on its own. Each query takes a fraction of a millisecond, and a loop over cards is how every list ever gets built. The cost only exists in the multiplication, and no edit ever held both ends of it. When the outage-night fix reported the shows list going from 151 round trips to 4, that count was true, for the loop being edited. The card renderer it calls kept its queries, and the same page runs 340 statements today. A reviewer handed that change would have approved it. My review loop did approve it. Nobody was stupid and nothing was broken, and the app was still guaranteed to meet its first real crowd unmeasured.

The same week's digging also turned up a single unindexed function quietly eating 79.5 percent of all production database time. Aggregate costs, invisible because aggregate was nobody's view.

## Why this keeps happening

I've written twice before about this project failing in the same direction. In June the test suite grew a pile of shared fixture data that no test owned ([The Test Suite Was the Incident]({% post_url 2026-06-10-the-test-suite-was-the-incident %})). In July the development tooling turned out to assume a single, human writer ([One Writer]({% post_url 2026-07-26-one-writer %})). Now the application itself: a shared pool and a shared database, exhausted by code that was correct from every vantage point anyone ever occupied. The thing that fails is always shared, and the reasoning that fails is always local. There's an irony in that for me, because I spent my academic life around formal verification, where local reasoning is exactly what you want, and it's safe there because the math guarantees the rest of the world stays untouched while you reason. Nothing here guaranteed anything of the sort. Every handler shares the pool, every query shares the database's time, and every change was checked against purely local facts, the build, the tests, the review of the diff, while the whole was never anyone's to prove.

The obvious objection is that human codebases rot exactly this way too, and they do. Nothing about an LLM invents the overly chatty page. What's different is that human organizations smuggle global reasoning in through side channels. An engineer who ships a page and then watches it crawl in production carries that burn into every page they build afterward. On-call turns production behavior into instinct. Ownership keeps one person attached to a surface long enough to notice its sum drifting. Every one of those channels runs through a person staying bound to consequences over time, and an agent loop severs all of them by construction. This codebase absorbed a couple thousand pull requests in a matter of months, each written by a session that had never seen the app before, will never see it again, and never feels a page load. The only memory the system has is what gets written down where the next session will read it, and nothing anyone wrote down said what a request should cost. It never occurred to me either.

To be fair to the agents, the same loop that never noticed 292 statements on the home page diagnosed the gridlock while the site was down, corrected its own faulty scan before acting on it, and reverted six of its own eight optimizations when the benchmarks disagreed with the plan. At diagnosis, with the evidence in front of them, these agents matched any engineer I've worked with, at several times the speed. Diagnosis is a local task, the evidence arrives pre-gathered and the mechanism is right there in the fragment. Prevention is the global task, and nothing in the loop was ever charged with a global. That includes me. The nine-queries comment crossed my desk inside a diff and I approved it. I've since come to think that careful cost prose in a diff actually lowers scrutiny rather than raising it. It reads as diligence, nothing fires when the price is exceeded, and the diligence is exactly what makes you stop checking.

## Make every global a local number

What I'm changing isn't "stop letting agents write the backend." It's simpler than that: every global property this app depends on has to become a local number that some check can see. Statements per request is measurable now, so the next step is a budget in CI, a test that fails the day a page crosses its ceiling. The card renderer that runs sixty-five times a page should say so in the file, where the next session will trip over it, welded to that budget so it can't rot into a lie the way "about nine queries" did. The looking itself has to be somebody's job too. Every repair in this story was trivial once the defect was seen, one keyword, two indexes, a rewritten loop. When fixing is nearly free, discovery is the entire cost, which is why there's now a daily job that walks production's query statistics and just looks.

I'll be honest about the limits. A statement budget wouldn't have caught the gridlock, whose counts were modest. The pool's wait counters catch that one now. Catching it before a crowd instead of during one takes a load test, which I never asked for, and the standard piece of plumbing that manages database connections for you, which still isn't there. The census doesn't replace either.

The Shows page runs 340 statements per request today, after the sweep, behind the guardrails. Nobody can say whether it survives a crowd, because nobody has pointed one at it. What I have that I didn't have in August is the number. What I still don't have is whatever names the next category, the way connection hold time was August's and statements per request was this week's. In a human organization that was somebody's job, the engineer who wonders, unprompted, what happens when this meets a crowd. Whether an agent loop can be made to do that wondering is the question this summer leaves me with, and it isn't a question about smarter models. Every model in this story was smart enough. The sum always fit in a context window. It was never anyone's assignment.
