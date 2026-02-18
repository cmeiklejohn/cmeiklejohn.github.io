---
layout: post
title:  "I Built a Social App in Five Days with Claude Code"
date:   2026-02-17 08:00:00 -0000
categories: ai claude
---

Most of my messages went out between midnight and 6am.

I know this because Anthropic generates a weekly usage report for Claude Code, and mine arrived looking like a police blotter: 384 messages, 27 sessions, 168 files touched, six days. I was responding at a median of 29 seconds — barely reading the output before firing back. The report described my style as "reactive and corrective rather than spec-driven," which is a polite way of saying I was moving too fast and fixing things as they broke.

What I was building: a private social app for my close-knit group of friends. We share live recordings of bands — Phish, Grateful Dead, that world — favorite films, books, and which upcoming shows we plan to catch this summer. Think a tiny, invite-only corner of the internet for people who care deeply about live music and want somewhere to talk about it with people they actually know. Live chat between people couch-touring and people in the pit is coming next.

The app is live. My friends are using it. It runs on Go with a server-driven UI architecture, and it ships as native iOS and Android apps. I built all of it in five days with Claude Code, and I want to tell you what that actually felt like.

<img src="/img/zabriskie-screenshot-1.png" style="width: 100%">

---

Launch day, a database migration corrupted production timestamp data.

The migration modified column types in a running database. Timestamps encode timezone assumptions at the type level — change them mid-flight and the data doesn't migrate, it breaks. The feed went down. I asked Claude to fix it. Each fix made things worse: wrong column names, broken SQL syntax, incorrect timezone arithmetic, until finally an overwrite ran that couldn't be undone. The Anthropic report summarized it as "Claude's database migrations went full disaster movie — each fix spawned a new production incident, permanently destroying timestamp data." That's accurate. Some of that data is simply gone.

What I learned from it is now the first rule in my project file: never touch timestamp or timezone columns in migrations, ever. But learning it cost real data on a real launch day, which is a lousy way to learn anything.

The timestamp disaster was the worst single incident, but the pattern underneath it showed up constantly: Claude moving confidently in the wrong direction, and me not stopping it soon enough. Push notification debugging is another example. Missing APNs tokens sent Claude deep into provisioning profiles, certificates, entitlements — a long, plausible-looking path that turned out to be completely wrong. The actual problem was missing `AppDelegate` methods in code Claude itself had written earlier. The report counted 30 instances of wrong-approach debugging across the project. That's a lot of time watching a very capable thing solve the wrong problem.

The server restart issue was more mundane but somehow more maddening for it. After any change to Go code, you have to restart the backend for the changes to take effect — compiled language, nothing exotic. Claude kept forgetting. I kept seeing no changes, assuming something was broken, investigating, finding nothing, eventually realizing the old process was still running. This happened across four or more sessions before I wrote a rule explicit enough that it actually stuck. `pkill` was the culprit — it fails silently, leaving the old process alive. The fix was `lsof` to find the PID, `kill -9` on that specific process, wait, restart, verify. A procedure that takes thirty seconds and has to be written down or it doesn't happen.

---

What got built, despite all of this, is genuinely surprising to me.

A full server-driven UI migration — the backend sends the entire interface as JSON, the React frontend just renders it, no hardcoded pages. iOS and Android apps, both submitted to their respective stores within the same week. End-to-end push notifications wired to every interaction. An AI-powered feature that surfaces what your friends are collectively into right now. An invite management system, automated build scripts, 107 database migrations, user engagement charts, a changelog that notifies users when something new ships.

Claude's ability to hold a large multi-file change in mind — a database migration, a new API handler, a frontend component, and a mobile layout fix, all in one coherent session — is where it earns everything. When the scope is clear and the pattern is known, it moves at a speed that doesn't feel real.

Only 2 of my 27 sessions fully achieved what I set out to do. That number sounds damning until you consider that 27 sessions in six days shipped something real that people are using. The sessions that failed were almost always the same shape: open-ended, no clear stopping point, debugging something visual or stateful where Claude had no feedback loop and I had too much patience for wrong approaches. The sessions that worked were tight — one known thing, done correctly, then stop.

---

The report called my style "ambitious" and noted I "tolerate high friction from repeated wrong approaches." I'd put it differently: I was building something I actually cared about, for people I actually know, and the deadline was summer tour. That changes your relationship to the friction.

You're not writing code with Claude Code. You're steering. The gap between those two things is where all the frustration lives, and also where all the speed comes from. When you accept that you're the judgment layer — deciding when to redirect, when to stop, when a fix is making things worse — the tool becomes something genuinely extraordinary.

Summer tour is coming. The app is ready.
