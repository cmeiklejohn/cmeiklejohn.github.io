---
layout: post
title:  "Software Engineering Is Becoming Civil Engineering"
date:   2026-04-01 00:00:00 -0000
group: ai
categories: ai engineering
---

After my guest lecture on AI in Michael Hilton's Foundations of Software Engineering course (CMU 17-313) today, a conversation started that I haven't been able to stop thinking about: we're watching software engineering split into two disciplines, the same way civil engineering split from construction a long time ago.

The welders who join steel beams on a bridge are skilled tradespeople. They're not involved in the structural design. They don't decide where the load-bearing members go. They don't reason about wind shear or seismic tolerance. But the bridge is *designed* so that a welder doing their job correctly can't bring the whole thing down. The structural engineer's job isn't to weld — it's to create a system where welding happens safely within well-defined constraints.

I think this is what's happening to software engineering right now. Not in five years. This year.

## The Split

Product managers are writing code. Not hypothetically — I'm watching it happen. With Claude Code and tools like it, people who understand the product but never wrote a line of code are adding features, building new behaviors, shipping changes. They describe what they want, the AI writes it, they iterate. The feedback loop is tight and the results are surprisingly good.

This is the welding. And there's nothing wrong with it.

But someone has to design the bridge. Someone has to decide how the database schema handles multi-tenancy. Someone has to design the deployment pipeline so a bad change rolls back automatically. Someone has to build the abstraction layer that lets a product manager add a new notification type without accidentally breaking the payment flow. That's the platform engineer — the structural engineer of software.

The profession is splitting, and the split is healthy. The mistake would be pretending it isn't happening, or pretending that both sides require the same skills.

## What the Platform Has to Guarantee

Here's where civil engineering has something important to teach us. A bridge isn't just designed to work — it's designed to be *safe to build on*. The structural engineer doesn't just hand off blueprints and hope for the best. The design itself encodes constraints that make certain categories of failure impossible.

Software platforms need the same properties. If product managers and AI agents are going to be writing and shipping code, the platform has to guarantee:

- **Isolation.** A change to the notification system can't corrupt the payments database. This isn't just good architecture — it's a safety requirement. Feature boundaries need to be enforced, not suggested. Blast radius has to be bounded by design, not by convention.

- **Accountability.** Every change has to be traceable to a person, an agent, and a reason. When something breaks at 2am, you need to know what changed, who initiated it, and what it was supposed to do. This isn't about blame — it's about diagnosis. Civil engineers have inspection records for every weld on a bridge. We need the equivalent for every deploy.

- **Observability.** You can't manage what you can't see. The platform needs to surface what's happening in real time — not just logs and metrics, but semantic observability. Not "HTTP 500 on endpoint /api/notify" but "the notification feature that was deployed 20 minutes ago by the growth PM is failing for 12% of users." The gap between raw telemetry and human-meaningful context is where most incidents rot.

- **Self-healing.** This is the one most platforms get wrong. When you detect a problem — elevated error rates, latency spikes, failed health checks — the system should automatically mitigate. Roll back the deploy. Disable the feature flag. Shed load. A bridge has expansion joints that absorb thermal stress without human intervention. Software needs the equivalent: automatic responses to predictable failure modes, so that a bad change from a product manager at 3pm doesn't become a production incident at 3am.

## The Uncomfortable Implication

The uncomfortable part of this analogy is what it says about coding itself. If software engineering becomes civil engineering, then writing code becomes more like welding — a skilled trade, but not the core intellectual discipline. The discipline becomes the design of systems that are safe to build on, safe to extend, and safe to operate.

This doesn't mean coding stops mattering. Welding matters enormously — a bad weld can kill people. But the structural engineer's job is to design a system where a single bad weld doesn't bring down the bridge. That's the shift. The value moves from writing correct code to designing systems that remain correct even when individual changes are imperfect.

And with AI agents doing more of the coding, "individual changes are imperfect" isn't a hypothetical — it's the baseline assumption. Agents hallucinate. They introduce subtle bugs. They make confident changes based on incomplete context. The platform has to absorb this. Not by making agents perfect, but by making the system tolerant of imperfection.

## What This Means in Practice

I've been thinking about this in the context of Zabriskie, the app I'm building with Claude Code. The architecture decisions that matter most aren't the ones about which framework to use or how to structure the code — they're the ones about what happens when something goes wrong.

Server-driven UI means I can push changes without App Store reviews — but it also means a bad server response can break every client simultaneously. So the platform needs client-side fallbacks, version negotiation, graceful degradation. The feature flag system isn't just a convenience — it's a safety mechanism that lets me (or a PM, or an agent) ship something and have it automatically disabled if error rates spike.

The database migration system needs to be idempotent and reversible, because agents will create conflicting migrations (I've seen this happen). The test suite needs to run automatically on every change, because the person making the change might not know what to test. The monitoring needs to be semantic enough that "the thing the growth PM shipped is broken" is a sentence the system can construct and act on, not just a pattern a human has to recognize in a wall of logs.

This is platform engineering. It's the structural design of the bridge. And it's becoming the actual discipline of software engineering, while the coding itself — increasingly done by AI — becomes the construction.

## The Conversation We Need to Have

At CMU, we talked about what this means for how we train software engineers. If the core discipline is shifting from "write correct code" to "design systems that are safe to build on," then the curriculum needs to shift too. More systems design, more fault tolerance, more observability. Less syntax, less framework-of-the-year. More thinking about failure modes and blast radius. More civil engineering.

The welders aren't going away. The work is real and important and skilled. But the bridge designers — the people who make it safe for everyone else to build — that's where the profession is heading. And I think we're going to get there faster than anyone expects.

