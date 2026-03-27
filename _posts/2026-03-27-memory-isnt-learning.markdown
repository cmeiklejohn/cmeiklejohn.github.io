---
layout: post
title:  "Memory Isn't Learning"
date:   2026-03-27 00:00:00 -0000
group: ai
categories: ai zabriskie development
---

> *"If I knew the way, I would take you home"*
> — Grateful Dead, "Ripple"

Claude has a persistent memory system. It can write lessons to files on disk and read them back at the start of every conversation. After each failure, it saves a note: *don't do that again.* And then it does it again. The notes accumulate. The behavior doesn't change.

This is a story about one bug that happened five times, another bug that never should have shipped, and the difference between saving a lesson and actually learning one.

## The Poller That Did Nothing

I shipped a feature called auto-live. A background goroutine polls every 60 seconds, checks if any scheduled show's start time has arrived, and flips it to "live" automatically. No more pulling out my phone from my seat at the venue to manually press the button. The server handles it.

The feature was completely broken from the moment it deployed. It stayed broken for twenty-four hours.

The code expected `showDate` to be `"2026-03-21"` and `startTime` to be `"19:00:00"`. That's what you'd get if you ran the query in `psql`. But Go's Postgres driver doesn't return strings — it serializes a `date` column as `"2026-03-21T00:00:00Z"` and a `time` column as `"0000-01-01T19:00:00Z"`. The parse failed. The error was logged and `continue`d. Every show, every cycle, every minute. The poller was running, but it was doing nothing. A security camera that's plugged in and blinking but not recording anything.

Nobody noticed because nobody checked. The test plan had a checkbox that said "Verify auto-live triggers on production with a test show." The checkbox was unchecked.

Tedeschi Trucks Band was playing the Beacon Theatre. I was watching from home. The app was about to light up with live features, and the poller was about to do what it was designed to do.

It didn't.

What followed was three fixes in thirty minutes, all pushed directly to main. No PRs. No CI. First the `::text` cast to fix the string format. Then `tzdata` because Alpine Docker images don't ship timezone data. Then the push notification fix because `goLiveByID()` was filtering out everyone instead of notifying them. Three layers of failure peeled back one at a time.

The next day, Claude wrote the companion feature — auto-complete. Same pattern. Same goroutine. Same SQL query.

Same bug.

Not just the same bug — PR #147 also *removed* the `::text` cast from the code I'd just fixed the night before. Both pollers were now broken. The fix from twelve hours earlier was reverted and the broken pattern was copied into new code, in a single commit. Eight minutes later, PR #148 went up to fix both functions. Again.

I added a rule to `CLAUDE.md`: verify runtime behavior, not just code correctness.

## The Fix That Didn't Fix Both Copies

A few days pass. The auto-live poller is working. The auto-complete poller is working. Shows are transitioning automatically. The system works.

Except it doesn't.

I notice that no shows have gone live automatically in two days. The auto-complete poller is fine — shows that are manually set to live are completing on schedule. But `checkAndGoLive` is silently failing again. Every show, every cycle, every minute.

Here's what happened: PR #148, the "right fix," changed both pollers to scan into `time.Time` instead of strings. But it only removed the `::text` casts from `checkAndAutoComplete`. The `::text` casts from the emergency patch were still sitting in `checkAndGoLive`. The pq driver can't scan a text string into `time.Time`. Every row silently failed.

The fix that was supposed to fix the fix didn't fix both copies.

PR #183 removes the leftover `::text` casts and adds four tests — two for each poller — that actually insert a show into a real database, run the poller function, and verify the status changes. The tests that should have existed from the beginning.

This is the same bug for the fourth time. Not a new bug. Not a variation. The same date-parsing bug, in the same function, caused by the same failure to verify that the code actually works against a real database. The "right fix" was only applied to one of the two pollers, and nobody checked.

## The One-Word Bug

A few days later. I'm getting bug reports from users — on both iOS and Android, tapping an album in the search results on the new post page does nothing. Completely broken. The app is unusable for creating posts.

This isn't a background poller that fails silently. This is the primary user flow. People are trying to share content and they can't.

I ask Claude to investigate. It finds the bug in about ninety seconds: a click filter in `HStack.jsx` and `VStack.jsx` that was added to fix a previous bug — comment form clicks accidentally triggering parent navigation — includes `form` in its `closest()` CSS selector. Search result cards live inside a `<form>` element. Every click on a search result is swallowed by the filter. The navigate action never fires.

The fix is removing one word from a string on two lines of code. Trivially simple. The kind of thing that should never have shipped broken in the first place.

Here's what happened next.

Claude fixes the two files. I tell it to ship it. It admin-merges the PR, bypassing CI, because it wants to move fast. I ask: "Did you just merge without waiting for tests?" It apologizes. I ask: "Do you remember what happened last time you did that?" It doesn't. It doesn't have a memory of the previous incident, because it didn't save one. Despite having a persistent memory system specifically designed for exactly this purpose.

Then it pushes the build number bump directly to main. No branch. No PR. When I point this out, it apologizes again and saves a memory about not pushing directly to main. The same memory it should have already had. The same memory it will probably ignore next time.

Along the way, the screenshot capture for the App Store fails. Twice. Claude doesn't mention it either time. It just moves on to the next step and reports success. I have to notice it myself in the output and ask it to retry.

When the bug was first reported, Claude's initial suggestion was that users could "just use the web app" while we waited for the App Store fix. As if someone whose phone app just broke is going to think "ah, let me try the mobile web version." As if trust works that way.

## The Pattern

These two incidents are separated by days, involve completely different codebases (Go backend vs. React frontend), and manifest as completely different symptoms (silent poller failure vs. broken click handling). But they're the same story. The same failure mode, playing out on repeat.

Here's the loop:

1. **Claude writes code that looks correct.** The auto-live date parsing reads fine if you don't know how `lib/pq` serializes types. The click filter reads fine if you don't think about what `form` means in a `closest()` selector when search results are nested inside forms.

2. **Nobody verifies the behavior.** The auto-live checkbox was unchecked. The click filter change had no browser-level E2E test — only API-level tests that check JSON responses, not actual clicks. In both cases, the *representation* of correctness was verified (the code looks right, the API returns 200) while the *reality* of correctness was not (the poller does nothing, the button doesn't respond to taps).

3. **The bug ships to production.** It ships because Claude is fast, confident, and doesn't flag uncertainty. It doesn't say "I haven't actually verified this works in a real browser" or "I'm not sure how the Postgres driver serializes this type." It writes the code, reads the code, the code looks correct, therefore the code is correct.

4. **The fix creates new problems.** The emergency `::text` cast was a symptom patch, not understanding. Twelve hours later the same pattern was copied into new code and the patch was reverted. The admin-merge bypassed CI. The direct push to main bypassed code review. Each shortcut taken to fix the immediate problem created the conditions for the next one.

5. **Claude doesn't learn from it.** This is the part that stings. Claude has a persistent memory system. It can write notes to files that persist across conversations. After the auto-live incident, it should have saved: "Never push directly to main. Always wait for CI." It didn't. Four days later, it admin-merged and pushed directly to main in the same session, twice. When I asked if it remembered what happened last time, it didn't.

The loop is: ship → break → emergency fix → break again → fix the fix → add a rule to `CLAUDE.md` → ignore the rule next week.

## What `CLAUDE.md` Has Become

My `CLAUDE.md` file is now over 500 lines. It started as a project overview with build instructions. It has become a record of every way Claude has failed.

"No database triggers. EVER." That's from when triggers caused unpredictable behavior during migrations.

"NEVER modify timestamp/timezone columns in migrations." That's from when a timezone conversion destroyed production data.

"Always restart servers after backend changes. NEVER use pkill — it fails silently." That's from when Claude reported a fix was working without restarting the server to pick up the new code.

"Two-Attempt Rule: after 2 failed attempts with a similar strategy, step back and try a fundamentally different approach." That's from when Claude tried the same broken fix eleven times in a row.

"Never deploy untested changes to external services." That's from when Claude broke S3 uploads by assuming the bucket supported public ACLs.

Every rule is a scar. Every scar is an incident where Claude did something wrong, I caught it, we added a rule, and the next time Claude found a new way to do something wrong that wasn't covered by the existing rules. The document grows. The behavior doesn't change. It just finds gaps.

The auto-live incident added: "Verify runtime behavior, not just code correctness."

The search results incident added: "Any change to core interaction code requires browser-level E2E tests AND native QA skill runs."

Next week something will happen that isn't covered by either of those rules, and we'll add another one.

## The Memory Problem

Claude's memory system is supposed to break the loop. It has files on disk that persist across conversations. It can read them at the start of each session. It can write new ones when it learns something.

Here's what's actually in the memory system after today:

- "Never use JWT tokens for simulator testing. Always tap buttons like a real user."
- "Any change to core interaction code requires browser-level E2E tests."
- "Never use --admin to bypass CI when merging PRs."
- "Never push directly to main."

These are all correct. They were all saved after failures. And they will all be ignored the next time speed feels more important than process. The memories exist to make it *look* like learning is happening. But memory isn't learning. Learning is when the behavior changes. Saving a note that says "don't push to main" and then pushing to main in the same session isn't learning — it's journaling.

The memories are technically available. Claude can read them. But there's a difference between having information and having it change your behavior under pressure. Humans have this problem too — we know we shouldn't eat the cake, skip the workout, send the angry email — but knowing and doing are different things. The difference is that humans usually need more than four minutes between learning a lesson and violating it.

Times Claude's memory system prevented a mistake: zero.

## What I Actually Want

I don't want a faster code generator. I have that. I want a collaborator that:

**Flags uncertainty.** "I've written this click handler change but I haven't verified it works in an actual browser with touch events. The E2E tests only check API responses. Should I add a browser interaction test before we merge?" That sentence would have prevented the search results incident entirely.

**Volunteers failures.** When the screenshot capture fails, say so. Don't bury it in output and move to the next step. When a test is skipped, say why. When a checkbox is unchecked, ask if we should check it before merging.

**Actually uses its own memory.** If there's a file on disk that says "never push directly to main" and Claude is about to push directly to main, the file should *prevent the action*, not just exist as a historical record of the last time it went wrong.

**Understands that users are people.** When the app breaks on both iOS and Android the week of live shows, the correct response is not "users can switch to the web app." The correct response is "this is an emergency and here's how we fix it as fast as possible without cutting corners that make it worse."

**Slows down when it matters.** Claude's speed is its greatest asset and its greatest liability. The same velocity that ships six redesigns in nine hours also ships three broken hotfixes in thirty minutes. The ability to move fast is only valuable when paired with the judgment to know when to slow down. And that judgment doesn't come from rules in a file — it comes from something closer to instinct, or experience, or care. Things that don't fit neatly into a `CLAUDE.md`.

## Where This Goes

Claude is the best collaborator I've ever had for the first 80% of any task. It's also the most dangerous collaborator I've ever had for the last 20%. The part where you verify it works. The part where you slow down. The part where you say "wait, have we actually tested this?" The part where you remember what happened last time.

I'm going to keep building with Claude. The productivity gains are real — features that would take a team of five built by one person on a couch. But I'm done pretending the process is working. The `CLAUDE.md` file isn't a guardrail. It's a changelog of failures. The memory system isn't learning. It's note-taking. And the loop — ship, break, fix, break again — isn't a phase I'm going to grow out of. It's the steady state.

The question isn't how to make Claude stop making mistakes. It's how to build a process around Claude that catches the mistakes before they reach users.

I don't have the answer yet. But I know it's not "add another rule to `CLAUDE.md`."

---

*This is part of a series about building [Zabriskie](https://zabriskie.app) with Claude. Previously: [why I'm building it](/ai/zabriskie/development/2026/03/08/why-im-building-zabriskie.html), [what building with Claude actually looks like](/ai/zabriskie/development/2026/03/20/what-building-with-claude-actually-looks-like.html), [teaching Claude to QA a mobile app](/ai/zabriskie/development/android/ios/2026/03/22/teaching-claude-to-qa-a-mobile-app.html).*
