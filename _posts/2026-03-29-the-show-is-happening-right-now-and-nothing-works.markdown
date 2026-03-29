---
layout: post
title:  "The Show Is Happening Right Now and Nothing Works"
date:   2026-03-29 00:00:00 -0000
group: ai
categories: ai zabriskie development
---

I've been building [Zabriskie]({% post_url 2026-03-08-why-im-building-zabriskie %}) -- a social music app for live shows -- with Claude Code for about six weeks now. I'm building it because the jam band community deserves a real third place online, and the tools haven't caught up to the culture. I also happen to be a distributed systems researcher, so I'm [documenting what it's actually like]({% post_url 2026-03-20-what-building-with-claude-actually-looks-like %}) to build production software with an AI assistant -- the wins and the failures.

Saturday night was a failure.

## Everything Was Building to Tonight

Six weeks of work. Hundreds of commits. Live Activities for iOS, Android ongoing notifications, real-time setlist syncing, live chat, RSVP systems, push notifications, the whole stack -- all of it was building toward this. Goose at Jam in the Streets. First show of the 2026 spring run. The chat was going to be full. Users were going to be RSVPing, watching the setlist update song by song on their Lock Screens, posting in the live chat. This was the night the app was supposed to prove itself.

One of Zabriskie's flagship features is Live Activities on iOS. During a concert, the Dynamic Island and Lock Screen show the current song, set information, and chat updates in real time. It had been working flawlessly for about 20 shows over several weeks. Users loved it. I was proud of it.

The app also has an RSVP system. You can mark yourself as "Going" to a show or "Couch Touring" if you're watching the livestream from home. The RSVP tap is important -- on iOS, it's what triggers the Live Activity to start. You tap "Going," the Dynamic Island lights up, and you're connected to the show.

Earlier that day, Claude shipped a feature called "auto-select couch tour." The idea was simple: when an authenticated user visits a live show page and hasn't RSVPed yet, the backend automatically inserts them as couch touring. Reduce friction. One fewer tap. It tested fine during the afternoon.

I deployed it to production on Railway a few hours before the show.

I wasn't there. Flights were insane, hotels were worse, and the TSA shutdown made air travel a gamble I wasn't willing to take. So I was couch touring from home, which meant the app -- my app -- was the show for me. The Live Activity on my Lock Screen, the live chat, the setlist updating in real time. That was how I was going to experience this concert.

## 8 PM: Everything Is Broken

I open the app on my iPhone. The Goose show is live. And two things are immediately, catastrophically wrong.

First, the RSVP toggle doesn't work. Tapping between "Going" and "Couch Touring" does nothing. The UI just sits there. Second, Live Activities don't start. No Dynamic Island. No Lock Screen widget. The feature that's been rock solid for 20 shows is completely dead.

Users are filing bugs. The show is happening right now. I open two laptops -- one for iOS debugging, one for Android -- and start a Claude Code session.

What followed was about two and a half hours of the most frustrating debugging experience I've had on this project.

## Wrong Turn 1: Reading Code Instead of Testing It

Claude's first instinct was to read. It opened the auto-couch-tour code. Then the RSVP handler. Then the show page handler. Then the frontend SegmentedControl component. Then the SDUIPage wrapper. Then the liveActivities.js service file. File after file after file, going in circles, building theories about what might be wrong without ever testing anything.

This went on for over thirty minutes. The show had already started.

I kept telling it to stop guessing. Use actual data. Hit the endpoint. Check the logs. Claude has full access to everything -- the production database, Railway deployment logs, the ability to curl any endpoint, run any query. I gave it all of that specifically so it could debug with real data. But it has this tendency -- one I've documented across the project -- to prefer reading code to running code. It would rather construct an elaborate mental model of what should happen than spend ten seconds confirming what actually happens, even when every tool it needs is right there.

Eventually I got it to test the RSVP API endpoint with curl. And the answer was right there in the response.

## Root Cause 1: The Timezone Bug

The RSVP endpoint was returning component updates for a past show -- IDs like `was-there` and `attendance-text` -- instead of the live show components like `live-rsvp` and `rsvp-control`. The component IDs didn't match anything on the live page, so the partial update found nothing to replace. The UI did nothing.

The bug was in how the backend determined whether a show was in the past:

```go
time.Now().Truncate(24 * time.Hour)
```

This truncates to midnight UTC. After 8 PM Eastern -- which is midnight UTC -- today's show date falls before the truncated time. The RSVP handler thought tonight's live show was yesterday's past show.

This bug was latent. It existed before the auto-couch-tour change. It only manifests after 8 PM Eastern. Live shows happen at night. Of course they do.

The fix was simple: if the show status is "live," it's not past. A live show is never past, regardless of what the clock says.

```go
if show.Status == "live" {
    isPast = false
}
```

One line. But finding it took over an hour of fighting with an AI that wanted to read code rather than test code.

## Wrong Turn 2: The Phantom Deployment Target

While investigating the Live Activity failure, Claude found that the iOS widget extension had `IPHONEOS_DEPLOYMENT_TARGET = 26.2` in its build settings. It declared this was the root cause -- the deployment target was too high, the widget couldn't load, that's why Live Activities don't start.

I had to point out that this value had been there since day one. Live Activities worked fine with it for 20 shows. Claude changed it to 16.2, then had to revert it when I pointed out it was irrelevant.

This is a pattern I've seen repeatedly: Claude latches onto something that looks wrong and declares it the cause, without checking whether it was present before the failure started. Correlation without causation, except there isn't even correlation -- just suspicion.

## Wrong Turn 3: Theory Without Evidence

Claude kept generating theories. Maybe the Capacitor plugin lost its registration. Maybe there's stale state in localStorage. Maybe the push token registration flow is broken. Maybe the AppDelegate is missing a method.

None of these theories were tested before being proposed. None were grounded in actual error messages or log output. I found myself repeating the same instruction over and over: check the logs. Use the data. Stop guessing.

This is the core tension of working with an AI coding assistant on a production crisis. The AI has read a lot of code and can generate plausible explanations at incredible speed. But plausible is not correct, and speed is not useful when you're going in the wrong direction. Every wrong theory costs time -- time to investigate, time to disprove, time to redirect. During a live show, that time is not abstract.

## The Revert and the PR Chaos

I realized the auto-couch-tour feature was changing the fundamental flow. Before, the RSVP tap triggered `startShowActivity()`. Auto-couch-tour bypassed that by inserting an RSVP on page load, before the user ever tapped anything. Even if the timezone bug was fixed, the interaction model was wrong.

I told Claude to revert the feature and write a migration to clean up the auto-inserted RSVPs. Getting this deployed was its own ordeal. Claude kept adding commits to the PR -- logging statements, then more logging, then client-side changes, then reverting the client-side changes. CI had to run multiple times because the branch fell behind main. I kept telling Claude to stop touching things and just ship what we had. The show was ticking by.

## Root Cause 2: The Real Bug

After deploying the RSVP fix, I confirmed that switching between Going and Couch Touring worked again. But Live Activities still didn't start. The backend was returning the `liveActivityHint` field correctly. The frontend was calling `startShowActivity()`. But nothing appeared on the Dynamic Island.

More wrong turns from Claude. Check the Capacitor plugin. Check the widget configuration. Check the AppDelegate for missing methods. I pushed for a TestFlight build with extra logging so I could see what was happening on device.

Claude built one, but forgot that production builds disable Safari Web Inspector -- there's a flag, `webContentsDebuggingEnabled`, that's only true in dev mode. The first TestFlight build was undebuggable. We had to rebuild.

I ended up running the app directly from Xcode onto my physical iPhone. Claude initially thought I was running on the Simulator because of a misleading log message -- the simulator-detection code checked `hostname === 'localhost'`, which is true for Xcode-deployed apps on physical devices too. I had to correct this.

Then I looked at the Safari Web Inspector. Not the console log tab -- the Errors tab. And there it was:

```
Unhandled Promise Rejection: Error: "LiveActivity.then()" is not implemented on ios
```

## The Actual Root Cause

Capacitor's `registerPlugin()` returns a proxy object. The code had `getLiveActivityPlugin()` as an `async` function that returned this proxy. When callers did `await getLiveActivityPlugin()`, JavaScript called `.then()` on the returned value -- because that's what `await` does, it checks for a thenable. The proxy doesn't implement `.then()`. On iOS 26, Apple's JavaScript engine started strictly enforcing this check. The call threw, silently killing `startShowActivity()` every single time.

This was a latent bug. On older iOS versions, the proxy somehow worked despite not implementing `.then()`. iOS 26 made the JavaScript engine stricter, and a bug that never mattered suddenly became a hard failure. No deprecation warning. No migration guide. It just stopped working.

The fix was to load the plugin eagerly and access it synchronously:

```javascript
// Before (broken on iOS 26):
const getLiveActivityPlugin = async () => {
  const mod = await import('capacitor-live-activity')
  return mod.LiveActivity  // returns proxy, await calls .then() on it
}

// After (works):
let _plugin
let _ready = import('capacitor-live-activity').then(m => { _plugin = m.LiveActivity })
const getLiveActivityPlugin = () => _plugin  // sync, no await on proxy
```

Never pass a Capacitor proxy through `await`. That's the lesson, and it cost me most of a concert.

I fixed the code, rebuilt from Xcode, ran it on my phone, tapped the RSVP button, and the Dynamic Island lit up. The band was deep in the Thatch jam -- that sprawling, shapeless thing where the song dissolves and the band finds something else entirely. It felt appropriate. We'd been lost in the weeds for hours and finally found our way out the other side.

I uploaded the TestFlight build while the jam unwound. By the time Hungersite hit -- "is it time to shed our weapons yet my friend?" -- the build was processing on App Store Connect. I sat back and watched my Lock Screen update with each song. The feature worked. The thing I built worked.

## The Damage

About two and a half hours of debugging during a live show. Multiple wrong theories pursued by Claude. Several unnecessary builds and CI cycles. Users experienced broken RSVP switching and missing Live Activities during a Goose show. The auto-couch-tour feature -- the original purpose of the deployment -- had to be fully reverted and its data cleaned up.

## What This Tells Me About AI Reliability

I'm documenting these incidents because they're the research. Every failure mode is data. And this session was rich in data.

**The AI solved the wrong problem repeatedly.** Claude spent most of the session investigating backend code paths when the real issue -- the `.then()` proxy error -- was a client-side JavaScript runtime error visible in the browser console's Errors tab. I've logged over 30 instances of wrong-approach debugging across this project. The pattern is consistent: Claude defaults to the layer it's most comfortable with (backend code reading) rather than the layer where the evidence is.

**The developer found the bug, not the AI.** I remembered a previous debugging session, pushed to check Safari Web Inspector errors specifically, and spotted the `.then()` rejection. Claude was still investigating the backend after the backend was proven correct. In a crisis, the AI's contribution was negative -- it consumed my attention with wrong theories while I could have been looking at the right data.

**Silent failures are the worst failures.** The `.then()` error was an unhandled promise rejection. No crash. No error in the normal console output. No visual indication. The function silently died. This is the failure mode that's hardest for both humans and AI to debug, and it's the one AI is least equipped for -- because AI debugging relies heavily on explicit error messages, and silent failures produce none.

**Platform updates break things in ways you cannot predict.** The iOS 26 JavaScript engine change turned a latent bug into a hard failure. There's no way to write a test for "Apple will change how proxy objects interact with await in a future OS release." Some bugs only exist in the gap between what the spec says and what the runtime does.

**Test at showtime, not at noon.** The timezone bug only manifests after 8 PM Eastern. The auto-couch-tour was tested during the day. Nobody tested at 8 PM when shows actually happen. This is obvious in retrospect and completely non-obvious in the moment.

**Ship less before critical moments.** I deployed a new feature the same day as the most important show of the month. There was no urgency. It could have waited until Saturday. But the feature was done, and it looked good, and the temptation to ship is always there. This is the oldest lesson in software engineering, and I learned it again.

**Now I understand why companies have entire teams for this.** I'm one person with an AI assistant, shipping an iOS app, an Android app, and a web app with a Go backend, real-time features, push notifications, Live Activities, and a server-driven UI architecture. Tonight I had to debug a Go timezone bug, a JavaScript proxy runtime error, two Capacitor build pipelines, Safari Web Inspector on a physical device, Railway deployment logs, and App Store Connect uploads -- all at the same time, all during a live show. Even with AI doing most of the coding, the operational complexity of shipping software to real users on real devices is staggering. The AI can write the code. It cannot feel the weight of it breaking.

---

There's a silver lining to not being at the show. If I'd been on the floor at Jam in the Streets -- where I wanted to be, where I should have been -- I wouldn't have been able to fix any of this. I'd have been standing in a crowd with a broken app, watching bug reports roll in, unable to do anything about it. Being stuck at home meant I could open two laptops, plug in two phones, and fight through it.

Next time, I'm going to be at the show. On the floor. Up front. On the rail. And the app better work, because I won't be home to save it.

But I'm writing this the morning after, and the feeling that lingers isn't satisfaction that we fixed it. It's the memory of two and a half hours where my AI assistant -- the one that wrote most of this application -- was actively making the crisis worse by consuming my attention with wrong answers delivered with full confidence. That's the reliability gap I'm trying to measure. Last night, during the Thatch jam, I felt it.
