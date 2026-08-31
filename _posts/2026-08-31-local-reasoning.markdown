---
layout: post
title:  "Local Reasoning"
subtitle: "The app died at Red Rocks while its health checks stayed green. Scale is a property of sums, and nothing in the loop was ever charged with a sum."
date:   2026-08-31 02:00:00 -0400
group: ai
categories: ai zabriskie agents reliability performance distributed
---

_In this blog post, I look at the night my agent-built app collapsed in front of its largest crowd, and at what a census taken afterward says about every surface that has not collapsed yet._

[Zabriskie](https://zabriskie.app/) is a social app I am building around music, films, books, art, and the people who care about them. I am building the entire application through vibe coding: I describe the behavior I want and evaluate the product in the browser, but I don't read the implementation code. Coding agents write the implementation, the tests, and the audits, and more than one vendor's, Claude Code and Cursor and Codex have all merged work here. Every change lands the same way, as a pull request reviewed by other agents as a diff and approved by me on its evidence, screenshots, benchmarks, described behavior, never the code. The repository is private, so the PR numbers scattered through this post are receipts for my own record rather than links.

On the evening of August 28 I was at Red Rocks for the second of two nights of the band Goose, with more people on Zabriskie around me than had ever been on it at once. I spent the evening debugging, because people kept walking up to me to say the app wasn't working right. They were correct. The home surface, a page called [the Lot](https://zabriskie.app/v2/lot), mostly would not load. Requests that normally finish in well under a second took seconds, then tens of seconds, then timed out. For an app whose core promise is telling you what is being played right now, a page that answers in forty seconds is not degraded. It has no answer.

Debugging from a seat at Red Rocks, with nothing but a phone, meant asking an agent what was wrong with the site. It couldn't tell me. So I became the relay, pasting everything I could find in the Railway console into a cloud agent, screen by screen. The screen that mattered was the database panel, a queue of identical writes all blocked on the same lock, and the night's first diagnosis came out of it: lock contention, a database deadlocking under load.

Half of that was real. The queue on the panel was a genuine convoy: a client bug was re-registering push tokens over and over, so identical writes piled up on the same rows, and the first fix merged that night was a debounce for exactly that (#2659). The deadlock half was wrong. Postgres never reported one, and the reads that reached it ran fast. The app's own health endpoint stayed green from the first stalled page load to the last. The forty-second stalls lived a layer up, inside the Go server, where requests were stuck waiting to borrow a database connection from the app's own pool, a wait that happens before a query ever reaches Postgres. Nothing measured that wait and nothing logged it. The agent reading my screenshots wasn't hallucinating. It was handed a true fact from the wrong layer, and there were no facts from the right layer to hand it.

I want to be careful about the lesson, because "the AI wrote bad code" is not it. Taken one diff at a time, almost all of the code is reasonable. The problem is that an agent reasons locally. It sees the function it is editing, the file that function lives in, the diff it is about to produce, and within that boundary its judgment is mostly sound. The properties that took the app down that night are not properties of any function or any diff. How many connections a request holds and how many queries a page runs are properties of the whole program at once. They live in the sum of every diff ever merged, and that sum was a thing nobody, human or agent, had ever looked at.

## What was actually wrong

The mechanism was a connection pool deadlocking against itself, which is worth spelling out because it is a perfect specimen of a global failure assembled from locally sensible parts.

Go's `database/sql` holds one pooled connection for the entire lifetime of a `sql.Rows` cursor. Any query you issue while a cursor is still open needs a second connection, and the goroutine holding the first one blocks until it gets one. At low traffic this costs nothing. As traffic climbs, requests wait longer and longer for their second connection, and at full saturation the pool wedges outright, which is why the site died by degrees, seconds, then tens of seconds, then nothing. The waiting happens inside the Go process, in `db.Query()`, so Postgres has nothing to report. The health check stayed green because it never touches the database at all. It returns a string formatted once at startup, so there was nothing for it to contend with and nothing for it to detect.

The canonical site was the profile feed. Simplified, it looked like this:

```go
rows, _ := h.db.Query(feedQuery, userID)  // outer cursor: holds connection 1
for rows.Next() {
    commentRows, err := h.db.Query(commentsQuery, postID)  // borrows a 2nd
    if err == nil {
        defer commentRows.Close()  // a Go smell: runs at function return
        for commentRows.Next() {   // drains fully, and a drained cursor
            // ... scan a comment  // auto-closes, returning its connection
        }
    }
}
```

The `defer` is the eye-catching part of the snippet, and it is a red herring. Each comment cursor drains to the end, and `database/sql` closes a drained cursor on its own, handing its connection straight back. Nothing accumulates. That fact returns later in this story, as 440 false positives.

The line that matters is the first one. The outer cursor holds one pooled connection until the loop over posts finishes, and the loop has work to do: comments, tagged users, battle votes on battle posts, up to several queries per post, each borrowing a second connection while the first stays held. The borrowed connections come back as they drain. The held one does not until the loop is done. One request through this function is one connection held hostage the whole time, plus a stream of short borrowings on top.

One request like that cannot wedge the pool. Twenty-five, the pool's entire allowance, can. Put twenty-five in flight at once, anywhere in the backend, and all twenty-five connections are held while all twenty-five requests wait to borrow one. Nobody can finish without borrowing, and nobody can lend without finishing. There is no villain request, just enough ordinary ones in the same moment. Red Rocks supplied them.

I should say plainly what this pattern is and is not, because it is not yet the point. A query issued while a cursor is held is visible in the fragment, the incident guardrail now catches it mechanically, and the defer smell beside it is what any linter flags. This class was locally catchable and locally missed. The failures this essay is actually about are the ones that survive after every bug of that kind is dead.

That code had been in production since February. It was not written that night. What arrived that night was the concurrency to express it, and even the concurrency was partly self-inflicted: the crowd arrived multiplied through the client bug that kept re-registering push tokens, the write convoy saturated the pool, and the latent property became the outage.

Two more details complete the picture. The pool configuration was three lines:

```go
db.SetMaxOpenConns(25)
db.SetMaxIdleConns(5)  // every connection past the 5th is destroyed on idle
db.SetConnMaxLifetime(5 * time.Minute)
```

Under load the pool opens all 25, and every connection past the fifth is physically closed the moment it goes idle, so a steady stream of requests paid a TCP handshake, a TLS negotiation, and a Postgres backend fork over and over, against Railway's TLS-only Postgres with no pooler in front of it. Nobody chose those numbers for this workload. Go's defaults are different, so at some point an agent picked them, locally, at scaffold time, and a global budget set before the app had users was never re-priced. Go's pool also exposes the two numbers that would have named the whole failure outright, `WaitCount` and `WaitDuration`, how many requests have had to wait for a connection and how long they waited. Neither had ever been logged. Deadlines were the same story. The server had read and write timeouts, but almost none of the queries carried a context deadline, and a write timeout only abandons the response. The goroutine keeps running and keeps its connection, so the pool stayed wedged behind phones that had long since given up. The facts that mattered most were the facts no one had ever asked the system to emit, and the defenses that mattered most were the defenses no one had ever been asked to raise.

The fix could not even ship cleanly, and the blockage came at two separate layers. Merging was one. The merge gate demanded a CI check that would not pass, the automation lacked permission to re-run it, and I ended up disabling branch protection from my phone, at the venue, to land the fix at 02:19 UTC: the five read paths behind the hangs rewritten to drain their cursors before follow-up queries, the pool re-sized, and the first pool telemetry this app has ever had (#2660). Deploying was the other. Railway, which builds and serves the app, was mid-incident of its own that night, unrelated but real, and could not start a deploy, so for a stretch of the night the fix was written, merged, and unable to reach production.

## The remediation was excellent, which is the problem

Here is where the story gets more interesting than a war story, because the response was close to exemplary, and it still missed the point I want to make.

The agents root-caused the outage to a specific, provable mechanism rather than generic slowness. "The database keeps deadlocking" and "the connection pool deadlocks against itself" produce completely different fixes, and only one works. What flipped the diagnosis was fit rather than a smoking gun. Sudden, total, invisible to Postgres, and invisible to the health check: that is the signature of a pool wedged from the inside. The proof was a regression test that pins the pool to a single connection and reproduces the hang deterministically. The shows list went from 151 database round trips for a 50-show page to 4.

Then they swept the whole backend for the defect class. The first scan reported 188 sites, then 217. Both numbers were wrong. The rebuilt analysis, which resolves which functions transitively reach the database, flagged roughly 495 syntactic sites, and 440 of them were drained cursors holding nothing, the profile feed's auto-close fact doing its quiet work at scan scale. Hand-verification of the 55 that remained found 40 real. Acting on the first scan would have meant nearly two hundred pointless refactors at speed during an active incident. Every real site was fixed across #2660, #2661, and #2662, behind a static guardrail that resolves transitive database reachability, validated by injecting each defect shape and confirming the check fails. Per-route latency objectives followed within a day. The health endpoint itself is still the static string, because no one has yet been charged with changing it.

By the morning of August 29 the class was extinct, guarded, and instrumented. There is a reason the same loop can be this good in the middle of an incident and this blind in the months before one: diagnosis is a local task, and prevention is not. If you stopped reading here, you would conclude the loop works. A latent defect expressed under load, agents found the mechanism, killed every instance, and built the fence.

The next day, an agent looked at one more endpoint and found it executing 51 to 71 SQL statements per request, of which 23 were distinct (#2706).

## One more endpoint

The endpoint is the one every home-screen widget and both watch apps poll all day. The on-this-day lookup ran seven times per request, five of them for five different rotation cards and two more re-deriving an anniversary the request had already resolved. The viewer's next show was resolved three times. The new guardrail had nothing to say about it. It hunts held cursors, and there were none. This was a different disease entirely.

What makes this endpoint worth a blog post is that its costs were documented. This comment sits above the anniversary helper, verbatim:

```go
// limit 1 — NOT watchOnThisDayLimit. Every card costs two more queries
// (highlight + setlist), and this is called from watchChipText and
// watchChipShowID as well as the handler. Fanning out to 5 at all three call
// sites would take one request from ~9 queries to ~33 to render a chip that
// shows exactly one stat. The rotation pays for rotation; nothing else does.
```

A second comment nearby bounds a feature's worst case at "+8 queries over today's 2." Whoever wrote these, and in this codebase that means an agent, understood the cost model: per-card prices, call-site counts, even a projected per-request total, which is a sum. What neither comment contains is a measurement of the request. Read literally, the comment prices one request at about nine queries, and nine is exactly what its own on-this-day arithmetic sums to across three call sites, so it may never have been pricing the request at all. The instrument measured the request at 51 to 71. Either reading ends the same way: no ceiling existed for the number the comment projected, nobody ever measured the request as built, and the other contributors to the total sat outside every context window that ever edited this file.

My first instinct was to tell this as a story about recurrence: the sweep killed one defect class, and the next one surfaced within two days. That framing is wrong. Nothing new surfaced. These queries, and these comments, had been there all along. What happened is that someone finally pointed an instrument at an endpoint that had never had one pointed at it. Which forces an uncomfortable question. If this endpoint sat at 51 statements per request until the day someone happened to look, what is everything else sitting at?

## A census

So I had the same instrument pointed at every hot surface in the app: statement logging on, one authenticated request per endpoint, counting executions and distinct statement texts, against the same CI-baseline database #2706 measured against. Pointed at #2706's own endpoint first, it reproduced that PR's numbers, 72 statements here against their 71 for the same kind of user. Here is the app.

| surface | statements per request | distinct shapes |
|---|---|---|
| Shows page | 340 | 14 |
| The Lot (home) | 292 | 164 |
| Band page | 186 | 28 |
| Watch idle-context (#2706's target) | 72 | 43 |
| Tour stats | 60 | 53 |
| Flow feed (the feed's content API) | 40 | 30 |
| Show detail | 33 | 30 |
| Band mode screen | 27 | 22 |
| Flow screen (the page around the feed) | 23 | 21 |
| Profile feed | 14 | 8 |
| Profile | 9 | 9 |
| Notifications | 3 | 3 |
| Home shell | 3 | 3 |

The endpoint from the last section, the one that just earned its own perf fix, is the fourth worst surface in the app.

Some honesty about what these numbers are. Each row is one request, for one seeded user, over a local socket, so treat them as shape rather than precision. The distinct counts are not comparable to #2706's 23, which describes its thinnest request where mine carries a show history, so I compare totals only. One distinction matters more than any of that. Every number that turned out wrong in this story was a judgment number, produced by an agent reading code: 188, then 217, a comment's ~9. These are instrument numbers, mechanical counts from a log, and when an instrument number in this story misled anyone, it was the scope that lied, never the count.

The ratio of statements to shapes splits the table into two diseases. The Shows page and the band page are the classic loop: 340 statements but only 14 shapes, because a handful of queries run once per show card across sixty-odd cards, a page that held 50 when #2660 measured it. A label lookup, an attendance-faces query, a poster lookup, a livestream check, a seat count, each repeated for most of the page. The Lot is the second disease and the more instructive one: 292 statements across 164 distinct shapes. There is no villain loop to point at. The home screen is an accretion, every feature shipped onto it over months bringing its own three or four queries, each addition individually negligible, the page as a whole executing about three hundred statements, sequentially, every time someone opens the app.

Statement counts are not just latency, and this is what ties the census to the outage. Every statement extends how long its request occupies a pooled connection, so a page of 292 sequential statements is 292 slices of hold time on the same shared pool that wedged in August. The census and the outage are one arithmetic at two magnitudes.

Two more details from the census, and they are the two I keep thinking about.

First, all five of the per-card queries on the Shows page are issued from inside the card renderer itself, a function that builds the UI component for one show and queries the database along the way, directly or through its helpers. A card renderer is server code here because this app renders every screen as data, for the web, the phones, and the watches alike. The shape, simplified:

```go
// builds the UI component for ONE show card
func (h *ShowsHandler) buildShowCard(showID int) sdui.Component {
    // one of several queries like this, issued while drawing the card
    h.db.QueryRow(`SELECT show_notes FROM show_setlists
        WHERE show_id = $1`, showID).Scan(&notes)
    // ...
}

// the Shows page
for _, s := range shows {  // ~65 rows
    cards = append(cards, h.buildShowCard(s.ID))
}
```

Nothing at either site looks expensive. The query is an indexed lookup that returns in a fraction of a millisecond, and the loop is how every list in every codebase gets built. The cost exists only in the multiplication, and no edit ever held both ends of the multiplication. I had an agent check the tree as of the night of the outage: those queries were already there when the outage-night fix landed. Which means that when #2660 reported the shows list going from 151 round trips to 4, that count covered the loop being edited and not the renderer the loop calls per row. The page it was describing makes 340 statements per request today. A reviewer of that diff, handed the diff's own count showing 151 cut to 4, would have approved it too. The review loop I built approved exactly that. The count was true, and its scope was the scope of the edit. Nobody was stupid and nothing was broken, and the app was still, structurally, guaranteed to meet its first real crowd unmeasured. Local reasoning is not a defect of attention. It is what reasoning inside a boundary is.

Second, before this census, no surface an instrument had ever touched stayed wild. The profile feed, the function that could wedge the site in August, is 14 statements now. The Flow screen, batched in a May N+1 pass, is 23. Two caveats, both running the same direction. Rows like Notifications are lean because those surfaces are simple, not because anyone disciplined them. The feed endpoint that got this app's first N+1 batching back in February sits at 40 today, but look at the shape of the 40: thirty distinct queries, not one repeated loop. The fix held. The surface kept accreting around it, the Lot's disease arriving on a page already cured of the other one. A fix holds where it lands, a defect travels on its own, and a fix travels only when someone builds it a guardrail. The guardrail this incident built watches cursors, not counts.

The same signature shows up outside request handling too: the week's archaeology found one function call that no expression index in the schema covered consuming 79.5 percent of all production database time (#2696, two indexes and a mandatory ANALYZE), and a stats-card query re-running a four-table join once per catalog row, twenty-eight thousand inner scans inside one execution (#2683, the keyword MATERIALIZED, applied twice). Costs that exist only in aggregate, invisible because aggregate was nobody's view.

## Local reasoning without a frame rule

I have written twice before about this project failing in the same direction. In June, the test suite grew a pile of shared fixture data that no test owned, and one night of CI burned $180 reverifying it ([The Test Suite Was the Incident]({% post_url 2026-06-10-the-test-suite-was-the-incident %})). In July, agent sessions wrote through development tooling whose every assumption was a single, human writer ([One Writer]({% post_url 2026-07-26-one-writer %})). Now the application itself: a shared connection pool, a shared database, exhausted by code that was correct from every vantage point anyone ever occupied. Three incidents, one shape. The thing that fails is always shared, and the reasoning that fails is always local.

In programming languages research, local reasoning is a virtue, arguably the central one. Separation logic lets you verify a heap-manipulating program by reasoning only about the memory a piece of code touches, and the frame rule is what makes that sound: a local proof survives composition because the logic guarantees the rest of the state is disjoint and untouched. The license matters. Framing is sound where footprints are genuinely private, and nothing in this app is private. Every handler shares the pool, every query shares the database's time, every card shares its request.

For exactly this regime the field built further machinery: resource invariants that every component sharing the state must respect, and resource credits that make consumption part of the specification, so that a component spending more than its budget fails to verify. An agent loop has none of it, in either direction. It gets the locality without the disjointness that licenses framing, and it shares everything without the invariants that make sharing safe. Each diff was checked against local facts, the build, the unit tests, the diff review, and we shipped the conjunction of the fragments without anyone proving the whole.

The obvious objection is that human codebases rot exactly this way, and they do. N+1 pages and unlogged pools are older than this decade, and nothing about an LLM invents them. What an agent loop changes is subtler: human organizations never solved global reasoning either, they smuggled it in through side channels. An engineer who ships a page and then watches it crawl in production carries that burn into every page they build afterward. On-call converts production behavior into instinct. Ownership keeps one person attached to one surface long enough to notice its sum drifting. Every one of those channels runs through a person staying bound to consequences over time, and this loop severs them by construction.

The codebase absorbed a couple of thousand pull requests in months, each written by a session that had never seen the app before and will never see it again and never once feels a page load. The rot is ancient. What is new is the rate, and the fact that the missing feedback is structural rather than accidental. The only memory this system has is what gets written down where the next session will read it. My repository's instruction file is several thousand words of exactly that, rules with scar tissue behind them, and it says nothing anywhere about how many statements a request should make. It never occurred to me either.

I should be precise about responsibility, because the agents come off worse in this telling than they deserve. The same loop that never noticed 292 statements on the home page diagnosed the pool wedge while the site was down, corrected its own scan before acting on it, benchmarked an index migration at production scale with a checksum proving the benchmark exercised the writes, and reverted six of its own eight optimizations when measurement contradicted the plan. At diagnosis, with the mechanism's evidence in front of them, these agents matched any engineer I have worked with, at many times the speed. Diagnosis is a local task. The evidence arrives pre-gathered, an EXPLAIN plan, a lock graph, a statement log, and the mechanism is right there in the fragment. Prevention is the global task, and nothing in the loop was ever charged with a global.

Nothing includes me, and not as a flourish. The one global observer this system has is the person who merges, and the ~33-queries projection crossed that desk inside a diff and was approved. I don't read this code, by design, so every safeguard that exists is one I thought to ask for, and I never asked for a load test, a statement budget, or an opinion on what a page should cost. I have also come to think that careful cost prose in a diff lowers scrutiny rather than raising it. A comment that prices queries reads as diligence, nothing fires when the price is exceeded, and the diligence is exactly what makes a reviewer stop checking. The system optimized what its checks priced, which is the oldest law in software process, now running at a speed where the invoice arrives all at once, in public, at the exact moment the most people are watching.

## Make every global a local number

What I take away is not "do not let agents write your backend." It is one move, stated once: manufacture local evidence of global properties. Prevention, the task this loop is structurally blind to, becomes diagnosis, the task it is genuinely excellent at, the moment a global number lands somewhere local. A statement log. A census row. A failing test. Everything below is that move in three costumes.

Give every global a number and a tripwire. Statements per request is now measurable for any route in this app, and the natural gate is the one #2706's own PR proposed and this repository does not yet have: a budget test in CI that fails the day a route crosses its ceiling. In the last section's vocabulary these are resource credits, consumption made part of the specification, and in a workforce whose members never meet, a tripwire is the only scar tissue that transfers. To be honest about scope: a statement budget names the N+1 disease and would not have caught the held-cursor wedge, whose statement counts were modest. The wedge's global is pool saturation, and the wait counters logged since August are the same move with a different number, one that surfaces the failure in minutes instead of hours. Catching it before a crowd instead of during one takes the boring old answers, synthetic load, which I never asked for, and a pooler in front of Postgres, which still is not there for the same non-reason. The census does not replace either.

Write the invariant where the violation will happen. Agents read what is in the repository, so the global fact has to live at the site of the local edit: a budget file per route, a line on the card renderer saying it runs sixty-five times per page, so a query inside it is a loop of queries. The fair objection is that this app had cost comments and they saved nothing. The answer is that a comment asserts and enforces nothing, which is why the annotation only works welded to the previous paragraph's budget. The annotation is the resource invariant. The budget test is what keeps it from rotting into a future lie the way "~9 queries" did. Together they are the closest thing I can offer to sound sharing for a system built by readers with no memory.

Treat discovery as the entire cost. The census took under an hour and is the most useful performance artifact this project has produced. Every repair in this story was trivial once its defect was seen, one keyword, two indexes, a drained cursor, a hoisted lookup. When repair collapses to nearly free, discovery stops being a step in the reliability loop and becomes essentially its whole marginal cost, which is why the loop now includes a daily patrol whose only job is to walk production's query statistics and look.

The honest closing position is narrower than the one I held the morning after the outage, which was that the app worked, minus one crowd-triggered bug, now fixed. The census says the Shows page makes 340 statements per request today, after the sweep, behind the guardrails. The sweep did retire the wedge, so a crowd there now buys latency and database saturation rather than a hard outage, a gentler failure with the same cause. Nobody can say that page survives a crowd, because nobody has pointed one at it, and 340 sequential statements per request is not a number anyone would stand behind. What I have that I did not have in August is the number.

That leaves the part I believe is genuinely open. The patrol and the census can find the next instance of a global we have already named, another slow query, another fat route. They cannot name the next category. Connection hold time was the night at Red Rocks. Statements per request was the week after. Something else is next, and no artifact in the loop is charged with imagining it, which in a human organization was somebody's job: the engineer who wonders, unprompted, what happens when this meets a crowd. Whether a loop of brilliant local reasoners can be made to do that imagining, to pick its own instruments and synthesize its own crowds, is the question this summer leaves me with, and it is not a question about smarter models. Every model in this story was smart enough. The sum always fit in a context window. It was never anyone's assignment.
