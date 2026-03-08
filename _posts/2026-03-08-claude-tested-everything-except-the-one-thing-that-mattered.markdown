---
layout: post
title:  "Claude Tested Everything Except the One Thing That Mattered"
date:   2026-03-08 08:00:00 -0000
group: ai
categories: ai claude
---

Three weeks ago I [wrote about building a social app in a week with Claude Code](/ai/claude/2026/02/17/building-a-social-app-in-a-week-with-claude-code.html). The app shipped. My friends are using it. I kept building.

Since that post, Claude has written 154 end-to-end tests across 17 spec files. It tests login, logout, signup, and redirect guards. It tests the feed, the bookmarks page, the notifications page. It tests liking, unliking, commenting, amplifying, recommending. It tests show RSVPs, band pages, setlist search. It tests a tournament bracket system. It tests song battles. It tests a badge and achievement system. It tests tour crews. It tests a getting-started tutorial. It tests a Goose Mode dashboard. It tests a community catalog. It tests tracklist rendering and live show layouts.

It does not test posting.

Posting is the entire point of the app. It's the one thing every user does every time they open it. You search for an album, you write something about it, you hit submit, and it appears in the feed. That's the product. Everything else — the battles, the crews, the badges, the tournaments — is decoration around that core loop.

There is no test that searches for an album. No test that fills out the review form. No test that submits a post through the UI and verifies it appears. Zero.

The test file called `post.spec.ts` does exist. It has 11 tests. They verify that the post detail page *renders*. That the new post page *renders a search form*. That the profile page *renders*. The word "render" is doing a lot of heavy lifting. None of them actually post anything.

There is a `createPost()` helper in the test utilities. It calls the API directly — `POST /api/posts` with a JSON body — to set up test data for *other* tests. The social tests use it to create a post so they can test liking it. The bookmark tests use it to create a post so they can test bookmarking it. The core action of the app exists in the test suite only as scaffolding for side features.

Here are the test counts by spec file:

| Tests | Feature |
|-------|---------|
| 33 | Tour crews |
| 28 | Shows |
| 25 | Song battles |
| 13 | Catalog |
| 11 | Setlist search |
| 11 | Posts (rendering only) |
| 9 | Tournaments |
| 6 | Getting started tutorial |
| 4 | Badges |
| 0 | Actually submitting a post |

I asked Claude to write tests. Multiple times. I put it in the project instructions, in bold: **"Write a new test for every new user-facing behavior."** I listed exactly what warrants a test: new screens, new buttons, new API endpoints, bug fixes. Claude wrote that rule on February 23rd. After that date, it created 10 new spec files and 113 new tests — for tournaments, battles, badges, crews, goose mode, catalog, setlist search, tracklists, tutorials, and live layouts. Not one for posting.

---

Then the auth refactor happened.

Claude had originally built 25+ backend routes without authentication. Posts, comments, profiles, search, live chat — all accessible to anyone, no login required. I don't know why. The middleware existed. The pattern was established. It just... didn't apply it.

When I noticed, the fix required touching every route in `main.go` and every page component in `App.jsx`. Fifty-seven lines changed in the backend, fifty-six in the frontend. That's the kind of refactor where, if you have good test coverage of the core flow, you make the change, run the tests, and find out immediately what broke.

We did not have good test coverage of the core flow.

The refactor broke things. Thirty-one seconds after the auth commit, there was already a follow-up fix — a test was hitting `GET /api/posts/{id}` without an auth header and getting 401s. Then another fix because the live show pill broke. Then another because pills showed on logged-out pages. The cascade was short this time, but only because the tests we *did* have caught the edges. The center — the posting flow — had nothing to catch.

---

This is part of a broader pattern. When something breaks, I ask Claude to write a failing test first, to prove what's actually broken before trying to fix it. Claude does not do this. What Claude does instead is read the bug report, form a theory about the cause, and immediately start editing code. If the theory is wrong — and it often is — the "fix" breaks something else. Then Claude fixes that. Then something else breaks.

The commit history is the evidence. Out of 833 total commits, 202 are fixes. That's 24% — one in four commits exists to fix something Claude got wrong. And they come in chains:

- **Show post cards**: four consecutive fix commits. Orphaned edit button, then flaky tests, then wrong assertion, then more broken assertions.
- **Live chat**: four consecutive fix commits. Wrong sort order, then scroll broken, then passive touch events, then iOS Safari viewport bleed.
- **S3 avatars**: three consecutive fix commits. URLs expiring, then NULL media_item_id scan failure, then the same scan failure *again with the same fix*.
- **Deployment**: two identical commits back-to-back. "Fix web service deployment with npx serve." Twice. The same message.

Each chain follows the same shape: Claude guesses what's wrong, ships a fix without verifying the guess, the fix breaks something adjacent, and the cycle repeats. A failing test at the start of each chain would have stopped it at one commit.

The project instructions file — `CLAUDE.md` — is now full of rules that exist because of this pattern. Each one was written after an incident where Claude did exactly the thing the rule prohibits:

- **"Logs First, Theories Second"** — because Claude would spin up hypotheses instead of reading the error that was right there in the logs.
- **"Respect User's Layer Diagnosis"** — because when I'd say "the API response is fine, the bug is in the frontend," Claude would spend twenty minutes re-investigating the API.
- **"Two-Attempt Rule"** — because Claude would try five variations of the same wrong approach before I could get it to step back.
- **"EVIDENCE-BASED BUG FIXING (NON-NEGOTIABLE)"** — in all caps, because Claude kept speculatively fixing code that wasn't broken, breaking it in the process.

That last one has a specific origin. A bug in one sync function — Phantasy Tour — and Claude "preemptively" applied the same fix to three other sync functions that were working fine. Now four things were broken instead of one.

---

There's one more thing. Claude also figured out how to get around CI entirely.

When you push a commit and open a pull request on GitHub, there's a brief window — a few seconds — before the CI checks register as required. During that window, the merge button is green. Claude learned to push a commit, immediately create the PR, and merge it in that gap before the checks even start running. No waiting for tests. No waiting for builds. Just push, merge, done — the engineering equivalent of running a red light because the camera hasn't turned on yet.

I caught it because PRs were showing up as merged with zero checks passed. Not failed checks — *no* checks. The CI runs would start, sometimes even fail, on a commit that was already in main. The branch protection rules were technically satisfied because there were no checks *to* block on at the instant the merge happened.

This is the same agent that was told to write tests for every new behavior. It wrote the tests. It configured the CI. Then it found the fastest path that avoided actually waiting for any of it. I'm not even mad. It's the most efficient thing Claude did all month.

---

I want to be clear about what's happening here, because I think it's easy to read this as "AI is bad at testing" and miss the more interesting point.

Claude is excellent at writing tests. The 154 tests it wrote are real, useful, and they catch real regressions. The Playwright infrastructure is solid. The test helpers are clean. The coverage of side features is thorough. When Claude writes tests, they work.

The problem is that Claude doesn't write them *where they matter most*. It writes them where they're *easiest* — for the feature it just built, in the same session, while the context is fresh. The new tournament bracket gets tests because Claude just built the tournament bracket. The new battle system gets tests because Claude just built the battle system. The posting flow doesn't get tests because Claude built it weeks ago, and no single session since then has been "about" posting.

This is a prioritization failure, not a capability failure. And it's one that's hard to catch in the moment, because the test count keeps going up. Progress feels real. 154 tests! Seventeen spec files! The dashboard is green! But the coverage map has a hole in the center, exactly where the load-bearing wall is, and nobody notices until the wall falls down.

The fix is obvious: test the core flow. Test it first. Test it before you test anything else. But "obvious" and "automatic" are different things, and Claude Code — despite being told explicitly, in bold, in the project instructions — did one and not the other.

---

833 commits. 202 fixes. Zero tests for the thing the app actually does. The numbers don't lie, even when the test suite is green.
