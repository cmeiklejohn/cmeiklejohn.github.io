---
layout: post
title:  "Software Engineering Is Becoming Civil Engineering"
date:   2026-04-01 00:00:00 -0000
group: ai
categories: ai engineering
---

After my guest lecture on AI in [Michael Hilton's](https://www.cs.cmu.edu/~mhilton/) Foundations of Software Engineering course ([CMU 17-313](https://cmu-313.github.io)) today, a conversation started that I haven't been able to stop thinking about: software engineering is going through the same transition that building went through in the 18th century, when structural design separated from craft construction and became its own discipline. What we now call civil engineering.

The welders who join steel beams on a bridge are skilled tradespeople. They're not involved in the structural design. They don't decide where the load-bearing members go. They don't reason about wind shear or seismic tolerance. But the bridge is *designed* so that a welder doing their job correctly can't bring the whole thing down. The structural engineer's job isn't to weld. It's to create a system where welding happens safely within well-defined constraints.

I think this is what's happening to software engineering right now. Not in five years. This year.

## The Split

Product managers are writing code. This is how I operate with [Zabriskie](/ai/zabriskie/community/2026/03/08/why-im-building-zabriskie.html), the app I'm building. I act as a product manager more than a programmer. I have non-technical collaborators who file bugs and feature requests, and Claude Code implements them directly. People who've never written a line of code are describing what they want, the AI writes it, they verify, it ships. The feedback loop is tight and the results are surprisingly good.

This is the welding. And there's nothing wrong with it.

But someone has to design the bridge. Someone has to decide how the database schema handles multi-tenancy. Someone has to design the deployment pipeline so a bad change rolls back automatically. Someone has to build the abstraction layer that lets a product manager add a new notification type without accidentally breaking the payment flow. That's the platform engineer. The structural engineer of software.

The profession is splitting, and the split is healthy. The mistake would be pretending it isn't happening, or pretending that both sides require the same skills.

## What the Platform Has to Guarantee

Here's where civil engineering has something important to teach us. The analogy goes deeper than I expected.

A civil engineer doesn't just design a bridge. They decide *where* the bridge goes based on geology, water flow, soil load. They specify the materials. They calculate the forces. They design the inspection regime. They assess the environmental impact. They ensure compliance with building codes. Then, and only then, does construction begin.

Every one of these has a software analog, and together they paint a picture of what platform engineering actually is:

- **Site selection and domain isolation.** A civil engineer picks the bridge site based on the terrain. In software, this is the API design between components, the domain boundaries, the isolation between services. Where does one system end and another begin? Get this wrong and every change becomes a potential cascading failure. The platform engineer decides where the "bridges" go between domains, and makes sure a change on one side can't bring down the other.

- **Material specification.** The engineer specifies what grade of steel, what concrete mix. In software, this is choosing the languages, databases, queues, and frameworks that the rest of the team builds on. These choices constrain what's possible. That's the point. You pick materials that are well-understood, well-tested, and appropriate for the load they'll bear.

- **Load analysis.** Civil engineers calculate expected traffic, weight, wind, seismic forces, and then design for margins of safety, typically 2-4x the expected load. Software needs the same discipline. Capacity planning, rate limiting, designing for 10x your expected traffic. When a PM ships a feature that goes viral, the platform can't buckle.

- **Inspection regimes.** A civil engineer doesn't just design how the bridge is built. They design how it will be *inspected over its lifetime*. In software, this is observability and code review. Not just logs and metrics, but semantic observability. Not "HTTP 500 on endpoint /api/notify" but "the notification feature that was deployed 20 minutes ago by the growth PM is failing for 12% of users." The gap between raw telemetry and human-meaningful context is where most incidents rot. Code review serves the same function: a structured inspection process that catches problems before they reach production.

- **Environmental impact.** Before a bridge is built, engineers assess how it affects the surrounding area. Traffic patterns, water flow, ecosystems. In software, the analog is cost. What does this feature cost to run? What does it cost to maintain? What does it cost when it breaks? Platform engineers need to make these costs visible and bounded, because the people shipping features often have no idea what their changes cost downstream.

- **Codes and standards compliance.** Building codes aren't optional. They encode decades of hard-won lessons from failures. In software, this is security standards, accessibility requirements, regulatory compliance. The platform has to enforce these as constraints, not suggestions. Code review is the inspection mechanism, but the standards themselves need to be baked into the platform so that violations are caught automatically, not by a human reviewer who might miss them.

And one more that civil engineers take for granted and most software platforms still get wrong:

- **Self-healing.** A bridge has expansion joints that absorb thermal stress without human intervention. Foundations shift and the structure adapts. Software needs the equivalent: automatic responses to predictable failure modes. When you detect elevated error rates, latency spikes, failed health checks, the system should automatically mitigate. Roll back the deploy. Disable the feature flag. Shed load. A bad change from a product manager at 3pm can't become a production incident at 3am.

## What Actually Changes About the Job

This isn't about coding becoming less important. It's about what you spend your time on.

Today, most software engineers spend the majority of their day writing features. Tomorrow, I think the best ones will spend their day designing the systems that make it safe for *anyone* to ship features. This is what I've been spending most of my time on with Zabriskie. I'm still deeply technical. I'm still writing code. But the code I write is the platform, the constraints, the safety nets. Not the feature itself.

The day-to-day shifts. Instead of "implement the notification preference screen," it's "design the notification system so that a PM can add a new notification type and the worst thing that happens if they get it wrong is that one notification doesn't send." Instead of writing the migration, it's designing the migration system so that conflicting migrations are detected and blocked automatically. Instead of fixing the bug, it's building the observability that surfaces the bug before a user reports it.

It's not a demotion. It's a different kind of engineering. And honestly, it's harder. Writing a feature is a bounded problem. Designing a platform that stays safe as dozens of people and agents ship changes to it every day, that's an open-ended one. With AI agents doing more of the feature work, the assumption has to be that individual changes will sometimes be imperfect. Agents hallucinate. They introduce subtle bugs. They make confident changes based on incomplete context. The platform has to absorb this. Not by making agents perfect, but by making the system tolerant of imperfection.

## What This Means in Practice

I've been thinking about this in the context of Zabriskie. The architecture decisions that matter most aren't the ones about which framework to use or how to structure the code. They're the ones about what happens when something goes wrong.

Server-driven UI means I can push changes without App Store reviews, but it also means a bad server response can break every client simultaneously. So the platform needs client-side fallbacks, version negotiation, graceful degradation. The feature flag system isn't just a convenience. It's a safety mechanism that lets me, or a PM, or an agent, ship something and have it automatically disabled if error rates spike.

The database migration system needs to be idempotent and reversible, because agents will create conflicting migrations. I've seen this happen. The test suite needs to run automatically on every change, because the person making the change might not know what to test. The monitoring needs to be semantic enough that "the thing the growth PM shipped is broken" is a sentence the system can construct and act on, not just a pattern a human has to recognize in a wall of logs.

This is platform engineering. It's the structural design of the bridge. And it's becoming the actual discipline of software engineering, while the coding itself, increasingly done by AI, becomes the construction.

## The Conversation We Need to Have

At CMU, we talked about what this means for how we train software engineers. If the core discipline is shifting from "write correct code" to "design systems that are safe to build on," then the curriculum needs to shift too. More systems design, more fault tolerance, more observability. Less syntax, less framework-of-the-year. More thinking about failure modes and blast radius. More civil engineering.

The welders aren't going away. The work is real and important and skilled. But the bridge designers, the people who make it safe for everyone else to build, that's where the profession is heading. And I think we're going to get there faster than anyone expects.

