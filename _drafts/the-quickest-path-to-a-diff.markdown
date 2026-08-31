---
layout: post
title:  "The Quickest Path to a Diff"
subtitle: "The app died at Red Rocks while its health checks stayed green. Every line of code was fine. The sum was nobody's job."
date:   2026-08-31 02:00:00 -0400
group: ai
categories: ai zabriskie agents reliability performance distributed
---

[Zabriskie](https://zabriskie.app/) is a social app I'm building around music, films, books, art, and the people who care about them. Its center of gravity is live music, specifically the jam band world I'm part of. In that world no two shows get the same setlist, so the crowd spends the night asking the same questions: what song is this, what have they played so far, who else is here. Zabriskie answers them while the show is happening. The setlist updates song by song as the band plays, the people in the crowd and the people watching the stream from home share one chat, and the current song sits on your lock screen and your watch so checking it doesn't mean opening the app. The home page is called [the Lot](https://zabriskie.app/v2/lot), after the parking-lot scene at these shows, and when one of your bands is on stage somewhere, it opens on that show.

I build it entirely through vibe coding: I describe what I want and evaluate the result in the browser, but I don't read the implementation code. Coding agents write everything, the implementation, the tests, the audits. Claude Code, Cursor, and Codex have all merged work here. My job is deciding what to build and looking at what comes out.

On August 28 I was at Red Rocks for the second of two nights of the band Goose, and more people were on Zabriskie around me than had ever been on it at once. I spent the evening debugging instead of watching the show, because people kept walking up to tell me the app wasn't working. They were right. The Lot mostly wouldn't load. Requests that normally finish in well under a second were taking tens of seconds or timing out entirely. For an app whose whole point is telling you what's being played right now, a page that answers in forty seconds isn't degraded, it just has no answer.

Debugging from a seat at a show, with nothing but a phone, meant asking an agent what was wrong with the site. It couldn't tell me. So I started pasting everything I could find in the Railway console into a cloud agent, screen by screen. The screen that mattered showed a queue of identical writes all blocked on the same lock, and the first diagnosis of the night came out of it: lock contention, a database deadlocking under load.

Half right. The queue was real, a client bug was re-registering push tokens over and over, and the first fix merged that night made it stop. But Postgres never reported a deadlock, the reads that reached it ran fast, and the health endpoint stayed green from the first stalled page to the last. The actual problem was a layer up, in my own app. Requests were stuck waiting to borrow a database connection from the app's pool, the twenty-five connections it keeps open to its database, before their queries ever reached Postgres at all. Nothing measured that wait and nothing logged it. The agent reading my screenshots wasn't hallucinating. It got handed a true fact from the wrong layer, because the right layer had no facts to give.

I want to be careful about the lesson here, because "the AI wrote bad code" isn't it. Taken one change at a time, almost all of the code is reasonable. The problem is that an agent reasons locally. It sees the function it's editing, the file around it, the change it's about to make, and within that boundary its judgment is mostly fine. The things that took the app down aren't visible at that scale. How many connections a request holds, how many queries a page runs, those are properties of the whole program at once, and nobody, me included, had ever looked at them. That gap is what this post is about: all of the reasoning was local, and everything that broke was global.

## What was actually wrong

Every request has to borrow one of those twenty-five connections before it can touch the database, and it gives the connection back when its queries finish. The code the agents had written borrowed a connection to loop over a list of results, and inside that loop borrowed a second connection for each item, while still holding the first. Condensed, the pattern was:

```go
rows, _ := db.QueryContext(ctx, listShowsSQL) // borrows connection #1
defer rows.Close()                            // #1 stays borrowed until the loop ends

for rows.Next() {
    var show Show
    rows.Scan(&show.ID, &show.Date)

    // loadSetlist runs its own query: it borrows connection #2
    // while #1 is still held.
    show.Setlist = loadSetlist(ctx, db, show.ID)
}
```

One request running this holds two connections at once. That's fine while the pool has spare connections. That night it didn't: twenty-five requests were in loops like this at the same time, each holding one connection and waiting for a second one, and a connection only comes back when one of those requests finishes. None of them could finish. That's the gridlock. Requests queued behind it until they timed out.

The waiting happened inside my app, before any query reached the database. That's why Postgres looked fine all night: the queries that did get a connection ran fast. The health check stayed green because it doesn't run a query. It returns a string formatted once at startup.

The code had been in production since February. August 28 was the first night with enough traffic to fill the pool. The pool size didn't help: nobody chose twenty-five connections for this workload. An agent picked the number at scaffold time, before the app had users, and nobody went back to it. Go's pool tracks exactly this failure, `WaitCount` and `WaitDuration`, how many times a query waited for a connection and for how long. We had never logged them. Almost no query had a timeout. None of the numbers that would have explained the outage were being recorded.

The fix rewrote the worst read paths to stop holding one connection while borrowing another, retuned the pool, and started logging the wait counters. Deploying it was its own problem. The merge gate wanted a CI check that wouldn't pass, the automation didn't have permission to re-run it, and Railway, where the app runs, was having an unrelated incident and couldn't start a deploy. I disabled branch protection from my phone, in my seat, at 02:19 UTC. I don't know the exact moment the app came back. The fix merged while the band was still playing, and it couldn't reach production until Railway recovered.

## The remediation was excellent, which is the problem

The agents got the mechanism exactly right, wrote a regression test that shrinks the pool to a single connection and reproduces the hang deterministically, and swept the whole backend for the pattern. The first scan claimed 188 sites. The next claimed 217. The real number, once the analysis was rebuilt to check what actually holds a connection, was about forty, and all forty were fixed, with a static check behind them so the class can't come back. The shows list alone went from 151 database round trips per page to 4. By morning the defect class was extinct and guarded. If the story ended there, you'd conclude the loop works.

The next day, an agent looked at one more endpoint and found it executing 51 to 71 SQL statements per request, of which 23 were distinct.

This was the endpoint every home-screen widget and both watch apps poll all day. The new guardrail had nothing to say about it, because it hunts the gridlock pattern, holding one connection while borrowing another, and this endpoint didn't do that. This was a different disease. The part that stays with me is that the costs were documented. This comment sat in the code, verbatim:

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

Six more rows sit in between, from tour stats at 60 down to the profile page at 9. The endpoint from the last section, the one that just got its own perf fix, is the fourth worst page in the app; the census caught it before that fix landed. These numbers aren't just latency, either. Every statement stretches how long a request holds its borrowed connection, so a 292-statement page is 292 slices of hold time on the same pool that gridlocked in August. Same arithmetic, different magnitude.

The ratio of statements to shapes splits the table into two diseases. The Shows page is the classic loop: 340 statements but only 14 shapes, a handful of queries repeated once per card for sixty-five cards. The Lot is the more instructive one: 292 statements across 164 distinct shapes. There's no villain loop to point at. It's an accretion. Every feature that ever shipped onto the home screen brought three or four queries with it, each addition individually negligible, and the page now runs about three hundred statements, one after another, every time someone opens the app.

The Shows page bothers me the most, because of where its queries live. They sit inside the function that draws one show card, so every page that draws cards inherits them. And the Shows page has no page size: it draws every show happening in the next seven days, and the day I measured, that was sixty-five cards. Neither piece looks expensive on its own. Each query takes a fraction of a millisecond, and a loop over cards is how every list ever gets built. The cost only exists in the multiplication, and no edit ever held both ends of it. When the outage-night fix reported the shows list going from 151 round trips to 4, that count was true, for the loop being edited. The card renderer it calls kept its queries, and the same page runs 340 statements today. A reviewer handed that change would have approved it. My review loop did approve it. Nobody was stupid and nothing was broken, and the app was still guaranteed to meet its first real crowd unmeasured.

The same week's digging also turned up a single unindexed function quietly eating 79.5 percent of all production database time. Aggregate costs, invisible because aggregate was nobody's view.

## Why this keeps happening

I've written twice before about this project failing in the same direction. In June the test suite grew a pile of shared fixture data that no test owned ([The Test Suite Was the Incident]({% post_url 2026-06-10-the-test-suite-was-the-incident %})). In July the development tooling turned out to assume a single, human writer ([One Writer]({% post_url 2026-07-26-one-writer %})). Now it's the application: a shared pool and a shared database, exhausted by code that passed every check anyone ever ran against it. All three failures have the same shape. The broken thing was shared, and every check that cleared it, the build, the tests, the review of one diff at a time, was local.

There's a personal sting in this one. My academic work was distributed systems and fault injection: systems whose parts are individually fine and fail in combination, and tools that find those failures by attacking a running system on purpose, because reading the parts won't surface them. I spent years doing that to other people's systems. I never did it to mine. No load test, no drills, nothing that ever tested the whole running program. In someone else's system, this failure wouldn't have surprised me for a second.

Watch how something new gets added, because this is where the difference lives. A developer adding a query to a page they know starts from what's already there: there's a query two functions up that fetches almost this, so they extend it, or join against it, or pull the shared part into a helper. Nobody calls that global reasoning. It's just what editing a program you remember looks like. A session doesn't do any of that unless you ask for it. It takes the quickest path to a working diff, and the quickest path is a fresh query, a fresh component, a fresh style, written right where it stands. The backend queries the attendance table, who's going to this show, from 264 places in 69 files. The little uppercase label that sits above a card title has been implemented hundreds of times, an inline style here, a one-off helper there, and no session ever reached for a shared one, because finding it costs a search and writing a new one costs nothing. Refactoring doesn't happen unless you ask. Modularization doesn't happen unless you ask. And review doesn't catch it: every change here is reviewed, a review bot plus an adversarial reviewer we built, but both read the diff, and duplication is invisible in a diff. The copy looks clean on its own, the original is in some other file, and no human opens the file.

Human codebases rot this way too. The other difference is what happens to the person who wrote the code, after the code ships. You ship a slow page, you watch it crawl, and you build the next one differently. You get paged at 2am and the lesson sticks. Nobody in this loop gets paged. This codebase took a couple thousand pull requests in a few months, each one from a session that had never seen the app before and was gone before the code ran. A session knows only what's written down for it, and nothing written down said what a request should cost. I never thought to write it down either.

To be fair to the agents, the problem isn't ability. The same loop that missed 292 statements on the home page diagnosed the gridlock while the site was down, and reverted six of its own eight optimizations when the benchmarks said they were wrong. Handed the evidence, agents are as good as any engineer I've worked with, and faster. But diagnosis is local: the evidence is sitting right there. Prevention is global, and nobody in the loop was assigned it. Including me. The nine-queries comment crossed my desk in a diff and I approved it, because it looked careful. A cost argued in prose looks handled. Nobody ever checks it again.

## Where does global reasoning come from?

The thing that broke, every time, was a property of the whole program: connections held across a request, statements accumulated across a page, one function's share of the database's time. The reasoning that touched the code, every time, was local: the agent sees one function, the tests check one behavior, a review reads one diff, I approve one change at a time. None of that reasoning was wrong, which is the uncomfortable part. The Shows page runs 340 statements per request and every line involved is fine.

So where is reasoning about the whole supposed to come from? Not from the sessions: each one is born into a diff and gone by the merge. Not from the checks: every check anyone has written looks at one change. Not from me: I decide what to build and judge what comes out, and I have never read this program. Nobody in this loop stands anywhere the whole program is visible.

The human version of global reasoning was never a formal method. It was one person editing a program they remembered. Integration happened by default, because the developer knew what was already there; the whole lived in somebody's head, and writing the code was what kept it current. This loop has no such head, and that leaves me with questions I can't answer yet. Will models ever do this unprompted, or is reading the neighborhood before every change simply never the quickest path to a diff? Are context windows even the constraint? This program still fits inside one, and no session has ever chosen to load it. Maybe the answer isn't cognition at all: static analysis, dynamic analysis, test suites strong enough to pin down the whole program, machinery doing for the loop what memory used to do for a developer. Or maybe a loop like this only works with a human in it who actually reads the code. I don't know which of these it is. What I know is what happens with none of them, because I watched it happen from a seat at Red Rocks.
