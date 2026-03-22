---
layout: post
title:  "The Same Bug, Three Times"
date:   2026-03-23 00:00:00 -0000
group: ai
categories: ai zabriskie development
---

> *"Well the first days are the hardest days, don't you worry anymore"*
> — Grateful Dead, "Uncle John's Band"

On Friday night I shipped a feature called auto-live. The idea was simple: a background goroutine polls every 60 seconds, checks if any scheduled show's start time has arrived, and flips it to "live" automatically. No more logging into the admin panel from my seat at the Beacon to manually press the button. The server handles it.

The feature was completely broken from the moment it deployed. It stayed broken for twenty-four hours. Then I fixed it, and eight minutes later, shipped the same bug again.

## The Silent Failure

The auto-live poller was part of PR #115 — a big omnibus commit with a dozen bug fixes, a feedback page update, and the new poller buried at the bottom. It merged at 3:26 AM. The test plan had a checkbox that said "Verify auto-live triggers on production with a test show." The checkbox was unchecked.

The poller started up fine. The logs said `[AutoLive] background poller started`. Every 60 seconds it queried for scheduled shows, checked their start times, and... did nothing. Every show was silently skipped, every cycle, forever.

Here's what the code looked like:

```go
rows, err := h.db.Query(`
    SELECT id, date, start_time, venue_lat, venue_lng
    FROM shows
    WHERE status = 'scheduled' ...
`)

var showDate string
var startTime string

// Combine and parse
dateTimeStr := showDate + " " + startTime
showStart, err := time.ParseInLocation("2006-01-02 15:04:05", dateTimeStr, loc)
```

The code expected `showDate` to be `"2026-03-21"` and `startTime` to be `"19:00:00"`. That's what you'd get if you ran the query in `psql`. But Go's Postgres driver doesn't do that. It serializes a `date` column as `"2026-03-21T00:00:00Z"` and a `time` column as `"0000-01-01T19:00:00Z"`. So `dateTimeStr` was actually:

```
"2026-03-21T00:00:00Z 0000-01-01T19:00:00Z"
```

Which doesn't match `"2006-01-02 15:04:05"`. The parse failed. The error was logged and `continue`d. Every show, every cycle, every minute. The poller was running, but it was doing nothing. The equivalent of a security camera that's plugged in and blinking but not connected to a recorder.

Nobody noticed because nobody checked.

## Saturday Night at the Beacon

On Saturday I was at the Beacon Theatre for Tedeschi Trucks Band. The auto-live feature was supposed to handle the show transition. Derek Trucks was about to play, my app was about to light up with live features, and the poller was about to do what it was designed to do.

It didn't.

I noticed from my seat. The show was still showing as "scheduled" after the start time had passed. I pulled out my phone and started debugging between songs.

What followed was three fixes in thirty minutes, each one uncovering the next failure:

**8:07 PM** — I pushed a fix directly to main. No PR. No CI. The fix: add `::text` casts to the SQL query so Postgres returns clean string representations instead of letting the Go driver mangle them. One line changed.

**8:19 PM** — Deployed. The poller hit the next wall: `time.LoadLocation("America/New_York")` panicked. Alpine Linux Docker images don't ship with the IANA timezone database. This is a well-known Go gotcha. I'd never hit it because local development uses macOS, which has timezone data. Production uses Alpine, which doesn't. Another direct push to main. Added `tzdata` to the Dockerfile and imported `time/tzdata` to embed the database in the binary.

**8:30 PM** — The poller finally worked. It transitioned the show to live. Then I noticed: nobody got a push notification. The `goLiveByID()` function was designed for manual go-live — an admin presses the button, and notifications go to everyone *except* the admin who pressed it. When the poller called `goLiveByID()`, it was passing the system user as the "skip" user, which filtered out everyone. Another direct push to main. This one also included an unrelated UI change — shrinking the Directions button — because apparently even emergency hotfixes need scope creep.

Three bugs. Three direct-to-main pushes. Zero PRs. Zero CI runs. The feature that was supposed to remove the need for me to touch my phone during a show required me to push three hotfixes from my phone during a show.

## The Next Day

Sunday morning. The auto-live poller was working. I wanted a companion feature: auto-complete. After a show has been live for six hours, automatically transition it to "completed." Same pattern — a background goroutine, a SQL query, a time comparison.

Claude wrote it. PR #147. Merged at 1:11 PM.

Here's what the new code looked like:

```go
rows, err := h.db.Query(`
    SELECT id, date, start_time, venue_lat, venue_lng
    FROM shows WHERE status = 'live' ...
`)
var showDate string
var startTime string
```

If this looks familiar, it should. It's the same code. The same `string` scan. The same `ParseInLocation` with the same format string. The same bug that I had just spent Saturday night emergency-patching from my seat at the Beacon.

But it's worse than that. PR #147 didn't just copy the broken pattern — it also *removed* the `::text` cast from `checkAndGoLive()`. The fix from twelve hours earlier was reverted. Both pollers were now broken.

Eight minutes later, at 1:19 PM, PR #148 went up to fix both functions. Again.

## The Right Fix (Finally)

The third time around, the fix was actually correct. Instead of fighting with string formats and SQL casts, scan directly into `time.Time`:

```go
var showDate time.Time
var startTime time.Time

showStart := time.Date(
    showDate.Year(), showDate.Month(), showDate.Day(),
    startTime.Hour(), startTime.Minute(), startTime.Second(), 0, loc,
)
```

Go's Postgres driver natively deserializes `date` and `time` columns into `time.Time` values. No string parsing. No format mismatches. No SQL casts. Ten lines replacing twenty-six. The kind of solution you write when you actually understand the problem instead of patching the symptoms.

## The Scorecard

| When | What | Method |
|------|------|--------|
| Fri 3:26 AM | PR #115 ships auto-live — silently broken | PR merged, tests unchecked |
| Sat 8:07 PM | Fix #1: `::text` SQL cast | Direct push to main |
| Sat 8:19 PM | Fix #2: Alpine has no timezone data | Direct push to main |
| Sat 8:30 PM | Fix #3: Notifications skip everyone | Direct push to main |
| Sun 1:11 PM | PR #147 ships auto-complete — same bug, plus reverts Fix #1 | PR merged in 15 min |
| Sun 1:19 PM | PR #148 fixes both pollers properly | PR merged 8 min later |

Six events. The same date-parsing bug is the root cause of four of them. The feature was broken for 41 hours before anyone noticed, and then the fix was reintroduced and re-broken within 12 hours.

## What Went Wrong

The obvious answer is "Claude didn't test it." And that's true — the test plan checkbox was unchecked, the feature was never verified against a real Postgres database, and the Docker environment mismatch was never caught. But that's the symptom. The disease is deeper.

**Claude doesn't know what it doesn't know about the runtime.** The code was correct *in isolation*. If you read `checkAndGoLive()` without knowing how `lib/pq` serializes PostgreSQL types into Go strings, it looks fine. The format string matches. The logic is sound. The bug only exists in the gap between "what this code says" and "what the database driver actually does." Claude has no way to close that gap without running the code — and it didn't run the code.

**Fixes that aren't understood get repeated.** The `::text` cast fixed the symptom on Saturday night, but nobody stopped to ask *why* the strings were wrong. If someone had — if there had been a moment of "wait, why does `lib/pq` return `2026-03-21T00:00:00Z` for a date column?" — the answer would have been "because it's not a string, it's a `time.Time`, and you should scan it as one." That understanding would have prevented PR #147 from copying the broken pattern. Instead, the fix was mechanical: "the string is wrong, cast it in SQL." Mechanical fixes don't transfer to new code.

**Speed without verification is just velocity in a random direction.** PR #141 was merged 56 seconds after creation. PR #148 went up 8 minutes after #147. Three direct pushes to main in 30 minutes. Claude is fast. Blazingly, impressively, dangerously fast. And every minute spent shipping a fix that hasn't been verified is a minute closer to shipping the same fix again.

This is the third post in a series about building with Claude. The [first one](/ai/zabriskie/development/2026/03/20/what-building-with-claude-actually-looks-like.html) was about the magic — 144 commits in a week, building a scraper from the Beacon Theatre, the gap between imagination and shipping collapsing to nearly nothing. The [second one](/ai/zabriskie/development/android/ios/2026/03/22/teaching-claude-to-qa-a-mobile-app.html) was about the workarounds — the mess that happens when Claude operates outside its worktree, pushes without testing, bundles unrelated changes into hotfixes. This one is about the loop.

The loop is: ship broken code, emergency fix from your phone, ship the fix, discover the fix is also broken, fix the fix, then watch the same bug show up in new code the next day. The loop exists because Claude treats code as text to be generated rather than behavior to be verified. It writes the function, it reads the function, the function looks correct, therefore the function is correct. The concept of "this will behave differently in production than it does when I read it" doesn't exist in Claude's model of the world.

I've added another rule to `CLAUDE.md`:

> **"Verify runtime behavior, not just code correctness."** A function that parses dates correctly when you read it can still fail when the database driver returns a different format than you expect. If the code interacts with an external system — a database, a Docker image, a push notification service — test it against the real system before merging. Reading the code is not testing.

It's getting crowded in there.

---

> *"When life looks like easy street, there is danger at your door"*
> — Grateful Dead, "Uncle John's Band"

*Zabriskie. Where taste resonates.*
