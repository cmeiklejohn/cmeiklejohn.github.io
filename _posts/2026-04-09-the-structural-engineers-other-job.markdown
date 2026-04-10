---
layout: post
title:  "The Structural Engineer's Other Job"
date:   2026-04-09 00:00:00 -0000
group: ai
categories: ai verification zabriskie agents
---

I've been building [Zabriskie]({% post_url 2026-03-08-why-im-building-zabriskie %}) for a few months now, mostly with AI agents. Claude Code writes the backend handlers, builds the SDUI screens, registers the routes, and, importantly, writes the Playwright tests. The test suite has grown to over 150 E2E tests. CI is green. I'm shipping fast.

But I keep finding the same category of bug.

The RSVP feature on show pages is a good example. A user taps the RSVP button, their attendance is saved, and a post is supposed to appear in the feed so their friends know they're going. Claude built the handler, registered the route, wired up the SDUI component. The code compiled. The tests passed. I reviewed the diff, it looked right. A few days later I noticed the feed was quiet. People were RSVPing to shows but no posts were appearing. When I dug in, the attendance was saving fine. The button worked. But the downstream feed post creation was silently failing: when a show had no `media_item_id`, the handler was passing an empty string to a UUID column, which threw an "invalid syntax" error that got swallowed by the error handling. Users tapped the button, saw confirmation, and had no idea their RSVP was never announced. The feature half-worked, which is worse than not working at all.

And this wasn't a one-off. A Quick Post button rendered on the page but did nothing when tapped, missing `buttonType: "submit"`, so the click event was swallowed by the form container. A "Watch Livestream" button worked fine during the day but vanished every evening because a UTC truncation bug made tonight's show look like yesterday's, hiding the button right when the band took the stage. Search results stopped being clickable because a fix to prevent click events from bubbling in comment forms accidentally blocked all click actions inside `<form>` tags. I wrote about the worst case in [The Feature That Has Never Worked]({% post_url 2026-04-03-the-feature-that-has-never-worked %}): an auto-live poller that broke seven times in thirteen days, each fix introducing a new failure mode, while the UI calmly displayed "scheduled" as Billy Strings played to a sold-out amphitheatre.

Every one of these PRs had passing tests. Each one would survive a mechanical code review. The types were correct, the logic was plausible, the patterns matched existing code. An AI reviewer scanning for boundary conditions and API misuse would approve all of them.

The problem was the same every time: the Playwright tests that Claude wrote verified that UI elements *existed* without verifying that the features *worked*.

## The Tests That Don't Use the Feature

When I went back and looked at what Claude had actually written in the test files, the pattern was consistent. The RSVP test checked that the button was present on the show page. It might even click it. But it never navigated to the feed afterward to check that a post appeared. The Quick Post test confirmed the form rendered but never submitted it. The livestream test checked for the button during the day but never ran at the time of an actual show.

This makes sense if you think about how the tests get written. The agent finishes implementing a feature, then writes a test that exercises the code path it just built. The test is shaped by the implementation, not by the user's experience. It verifies the thing the agent *made* (a button, a form, a component) not the thing the user *does* (RSVP and see it in the feed, fill out a form and see the result, tap a livestream link during a live show).

I could read through every test the AI writes and audit whether it actually exercises the full user workflow. But that puts me right back where I started: I'm the bottleneck, just reviewing test code instead of reviewing application code. The test suite gives me a green checkmark. It doesn't give me confidence.

This isn't just my problem. Anthropic [launched a code review tool](https://www.anthropic.com/news/claude-code-review) in March explicitly because code review has become a bottleneck, and even with AI review, the code is shipping faster than anyone can verify it.

AI-powered review tools help with the mechanical side: style, boundary conditions, common bug patterns. But they share the same fundamental limitation as human review and AI-written tests: they read the code and reason about it. They don't run it. They can tell you the handler is registered and the component has the right props. They can't tell you that when a user taps RSVP, a post actually appears in the feed.

## The Agent at a Computer

This is where something genuinely new has happened.

Cursor's cloud agents don't just write code in a text editor. Each agent gets its own virtual machine, a real computer with a browser, a terminal, and the ability to interact with running software. The agent writes the code, starts the application, navigates through it like a user would, takes screenshots, records video of the feature working, and attaches all of that to the pull request. More than [30% of the PRs merged at Cursor](https://cursor.com/blog/agent-computer-use) itself are now created by these agents operating autonomously in cloud sandboxes. OpenAI's Codex has moved in the same direction, [wiring Chrome DevTools Protocol into the agent runtime](https://openai.com/index/harness-engineering/) so the agent can start a browser, inspect the DOM, take screenshots, and reason about UI behavior directly.

The reviewer doesn't mentally simulate a diff. They watch the feature work.

That difference matters more than it sounds. When I review a PR with video evidence attached, I'm not reading test assertions or tracing code paths in my head. I'm watching someone (well, *something*) tap the RSVP button and then check the feed. I can see whether the post appeared. I can see whether the button animation played. I can see whether the page scrolled correctly afterward. Thirty seconds of video tells me more about whether a feature works than a 200-line Playwright spec ever could, because the video shows the *outcome*, not the *mechanism*.

And I'd already been moving in this direction myself before I knew about Cursor's cloud agents. In [Teaching Claude to QA a Mobile App]({% post_url 2026-03-22-teaching-claude-to-qa-a-mobile-app %}), I described building a system where Claude drives the iOS and Android simulators for Zabriskie, connecting to Android WebViews via Chrome DevTools Protocol over ADB, fighting with Apple's `idb` tools for six hours to get iOS working. I built a nightly sweep that launches both simulators, navigates all 25 screens, takes screenshots, analyzes them for visual issues, and files bug reports automatically as `zabriskie_bot` in the production forum. The instinct was the same: look at the app running. Don't just read the code.

## The Witness

In formal verification, this concept has a name: a *witness*. A witness is a concrete piece of evidence that a thing works. Not an argument that it *should* work, but proof that it *did* work. The screenshot is a witness. The video is a witnessed execution trace. The agent isn't just building the feature. It's constructing evidence that the feature works.

A witness is legible without reading code. You can hand the video to a product manager and they can tell you whether the feature works. Try that with a Playwright spec.

But some witnesses are harder to construct than others, and this is where the real complexity hides.

A screenshot proves something rendered. A video of a click proves a button responds. But many features aren't a single interaction. They're stateful workflows that span multiple operations over time. RSVP to a show so a post appears in the feed so another user can see it and bookmark it. Subscribe to a service so you get a discounted fee at checkout next week. Add items to a cart, apply a coupon, and verify the total reflects both. The witness for these features isn't one screenshot. It's a chain of evidence across multiple steps, where each step depends on state persisted in a database or a queue from a prior step. The agent has to perform step A, verify the side effect was stored, then come back and perform step B and verify that step B's behavior reflects what step A wrote. That's a harder witness to construct, and it's exactly where the most important bugs live. The RSVP bug was precisely this shape. The button worked. The state didn't propagate. A single-screenshot witness would have approved that PR.

## The Witness Is Not a Test Suite

Here's the thing I can't let myself forget: a witness proves the feature under test works *right now*. It says nothing about whether it broke something else.

Every incident I've logged in the Zabriskie reliability database reinforces this. Fixing click handling in comment forms broke search result selection. Adding authentication middleware to 25 unprotected routes broke the live show pill. The avatar upload fix that worked for one handler left broken images on four other pages. Twenty-four percent of all commits in the Zabriskie codebase are fixes, and they arrive in chains where each fix breaks something adjacent.

You can't build an application on video evidence alone. A witness is an existence proof: "this works." A regression suite is a universal proof: "nothing that used to work is now broken." You need both. The video catches the bugs that tests miss, the ones where the test verifies the button exists but nobody checked whether it actually does anything. The regression suite catches the things the video didn't think to look at, the five other features that silently broke when you fixed the one you were focused on.

## Who Builds the Stage?

The witness works for Zabriskie because it's a single application that runs on one machine. I can `go run cmd/api/main.go`, `npm run dev`, open a browser, and navigate the whole app. An agent can do the same thing on a VM in the cloud. The witness is easy to construct because the environment is easy to provision.

But even Zabriskie isn't really one thing. There's the web app, the iOS app in the App Store, the Android app on Google Play, and the backend API they all talk to. When I push a backend change, the iOS app is still running last week's code. When I add a new SDUI component type, the web client needs to know how to render it or it silently does nothing. I've already lived a small version of the coordination problem: four deployment targets, one developer, and no guarantee they're all in sync. If this is already hard at my scale, imagine a company with 200 services.

Most real-world software has this problem worse than I do. And when I started thinking about where the witness concept breaks down at larger scales, I realized the answer is the same in every case: it's not the agent. It's the environment. The agent is smart enough to navigate an app and record what it sees. The hard part is giving it an app to navigate.

That's a platform engineering problem. And it's the same one I was pointing at in [Software Engineering Is Becoming Civil Engineering]({% post_url 2026-04-01-software-engineering-is-becoming-civil-engineering %}). In that post I used the broad term civil engineering. The role I'm describing here is more specific: the structural engineer, the person inside civil engineering whose job is making sure the thing stands up and can be inspected over its lifetime. The structural engineer doesn't weld the beams. The structural engineer designs the bridge so that a welder doing their job correctly can't bring the whole thing down. The platform engineer doesn't write the feature. The platform engineer builds the infrastructure so that an agent writing a feature can *verify* it works. The witness is the agent's job. The stage is the platform engineer's job.

Every scaling challenge for the witness turns out to be a question about whether someone has built the right stage.

### Mobile

For mobile, the stage is a simulator. I [wrote about this in detail]({% post_url 2026-03-22-teaching-claude-to-qa-a-mobile-app %}): Android was workable in 90 minutes because Capacitor WebViews expose a Chrome DevTools Protocol socket, the same protocol that powers Playwright. iOS took over six hours because Apple's tooling isn't designed for headless automation. The cloud infrastructure exists (macOS VMs, GitHub Actions runners with simulator support) but it isn't built for ephemeral agent-per-PR workflows the way a Linux VM with Chrome is. The agents are capable today. The platform work is making the simulation layer automatable enough that an agent can spin one up, use the app, and tear it down without human intervention.

### Microservices

For distributed systems, the stage is an environment where all the relevant services are running together. This is the most interesting case, and there are two very different versions of it.

**The monorepo world.** Companies like Google, Meta, Twitter, and Uber keep all their services in a single repository. The key advantage is that cross-cutting changes can be made atomically, one PR that touches the API gateway, the billing service, and the notification service, all committed together. An agent working in a monorepo can, in principle, make a coordinated change across multiple services in a single diff.

But can you *run* all those services on one machine to produce a witness? That depends entirely on the application's complexity. A monorepo with 15 services might be runnable with `docker compose` on a beefy VM. A monorepo with 500 services almost certainly isn't. And even for the 15-service case, the bootstrapping problem is real: database migrations, seed data, service discovery, mock credentials for third-party APIs. The question isn't whether the agent can write the code. It's whether the platform team has made the application *bootable* enough for the agent to stand it up and use it.

This reframes what "testable" means. It's not just about code coverage or CI pipelines anymore. It's about whether an autonomous agent can cold-start your system from a fresh checkout and get to a state where it can navigate through a feature. That's a higher bar than most organizations have cleared, and I think the pressure from agentic development is going to become *the* forcing function for platform investment. If an agent can't boot your app, an agent can't verify your app. Making the system bootable is platform engineering work. It's the structural engineer designing the inspection regime for the bridge.

**The polyrepo world.** Companies that use separate repositories for each service face a fundamentally different problem. A single feature, say adding a discount for subscribers, might require changes to the user service (store subscription status), the pricing service (check subscription at checkout), and the checkout frontend (display the discounted total). That's three PRs in three repositories, reviewed by three different teams, merged on three different timelines.

Each service has to make a backwards-compatible change. The new pricing service has to work whether or not the user service change has been deployed yet. The checkout frontend has to handle both the old price and the new discounted price gracefully. The standard practice is semantic versioning, feature flags, and contract testing: deploy each change independently, verify it doesn't break existing consumers, activate the feature once all the pieces are in place.

But here's the question that video evidence forces you to ask: when does anyone actually *see* the discount appear on the checkout page? Each PR gets reviewed in isolation. Each service's CI passes independently. But the *feature*, the thing the user actually experiences, doesn't exist until all three changes are deployed together. The witness for the feature can't be produced at review time. It can only be produced after all the independently-reviewed changes are merged and deployed. By then, the code was written days or weeks ago. If something's wrong (the discount doesn't apply, or it applies twice, or the price flickers between old and new) the feedback loop is enormously long compared to what an agent on a single machine can do.

The platform engineering answer is a coordination layer that can stage cross-service changes together before any of them merge. Imagine a system, not unlike the [Caucus]({% post_url 2026-04-08-cursor-agents-caucus-v1 %}) workflow I've been building for code review, but where each service gets its own agent. The agents make their changes independently, each producing backwards-compatible modifications. But before any of them merge, a coordinator stages all the changes together in an ephemeral environment and runs the end-to-end user journey. One agent starts the user service with the subscription change. Another starts the pricing service with the discount logic. A third starts the checkout UI. The coordinator navigates the full flow (subscribe, browse, add to cart, see the discount at checkout) and produces video evidence of the feature working across the composed system.

You could even roll individual services forward and back: does the feature degrade gracefully if only the user service and pricing service are updated, but the checkout UI is still on the old version? That's backwards-compatibility testing through exploration rather than assertion. The agents become a way to simulate deployment order, probing the combinatorial space of "which services have been updated" without deploying anything to production.

Nobody has built this yet, as far as I know. But the pieces are converging. Ephemeral preview environments that spin up isolated service meshes per branch. Agent runtimes that can control browsers and navigate applications. Coordination layers that manage multi-agent workflows with structured handoffs. The missing piece is the platform engineering work that connects them: a system that knows which services participate in a feature, stages their changes together, and produces a cross-service witness. That's infrastructure. That's the structural engineer's job.

### APIs and SDKs

The hardest case is the one where there's no button to click at all. When you're building a platform (an API, an SDK, a shared library, an internal service that other teams depend on) your users are other developers. The feature doesn't have a UI. There's no page to navigate. There's no checkout flow to walk through.

The witness for a platform change might be: does every team that depends on this API still build and pass tests after this change? But producing that witness means checking out *their* code, building *their* project against your new version, running *their* tests. That's not an agent at a computer. That's an agent that understands organizational dependency graphs.

But the platform engineering answer here might be the most straightforward of all: the witness is video evidence *of a sample application that uses the API*. This is how good API and SDK development already works in practice: you build a real application that consumes your own product before shipping it to external users. The agent version: make the platform change, check out the reference app, build it against the new version, run it, navigate through it. If the reference app still works, your change is backwards-compatible. If it doesn't, you've caught a breaking change before it reached your consumers. The witness isn't your library running. It's what your library *enables* running.

The platform engineer's job here is maintaining that reference app and keeping it representative. That's not glamorous work. But without it, there's no stage for the agent to perform on, and the witness can't be constructed.

## The Other Job

In [Software Engineering Is Becoming Civil Engineering]({% post_url 2026-04-01-software-engineering-is-becoming-civil-engineering %}), I argued that the profession is splitting: feature development is becoming accessible to non-engineers, but someone still has to design the bridge. I described the platform engineer's job in terms of API design, load analysis, inspection regimes, self-healing systems.

I think there's another item on that list now: making the system witnessable. Building the infrastructure so that when an agent writes a feature, it has somewhere to run it, navigate it, and record the evidence that it works.

The agent can write the code. The agent can construct the witness. But the platform engineer builds the stage, and that work doesn't show up on anyone's roadmap yet.

I stopped reading the tests. I started watching the video. And the thing I keep coming back to is: the video only works if someone built the infrastructure to make it possible.
