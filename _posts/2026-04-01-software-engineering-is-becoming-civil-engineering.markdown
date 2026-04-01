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

## The Hard Questions

I keep hearing the same anxiety from different directions. Engineers wondering what their job looks like in two years. Students wondering if they're learning the right things. At CMU, two questions came up that crystallized it for me.

The first: students early in their software engineering careers don't know how to tell when the AI is doing something wrong. They don't have the spidey-sense yet. The AI generates code that looks plausible, passes a surface-level review, and the student ships it. They can't smell the bad decision because they've never seen what a bad decision leads to. How do you develop judgment about something you've never experienced failing?

The second is even harder: where do our senior engineers come from? The ability to design good platforms, to make the right architectural calls, that comes from experience. You learn what breaks by building things that broke. You learn where to put the domain boundaries by having drawn them in the wrong place. You learn what to monitor by having been the person staring at useless dashboards during an incident at 2am. If AI is writing most of the code, and junior engineers aren't getting the reps of building and breaking things themselves, how do they develop the judgment to become the platform engineers we need?

These are connected, and they form a kind of paradox. The spidey-sense and the architectural intuition come from the same place: years of building things, watching them fail, and understanding why. But we're taking the coding away from students at the exact moment they need it most. The judgment to tell an AI it's wrong comes from having written enough code to know what right looks like. And we're handing them AI before they've had the chance to develop that judgment. So we're stuck: we need them to code to build intuition, but the industry is moving toward a world where they don't code.

I don't think the SE curriculum is fundamentally wrong. Courses like 17-313 teach the right things. But software engineering education has been underserviced for a long time, and now it's more important than ever. If anything, I'd add platform engineering as a first-class topic. Not as an elective, but as a core part of how we teach students to think about building software. How do you design systems that other people (and agents) can safely build on top of?

But here's the tension I keep coming back to, and I'm genuinely struggling with it: you need to understand code to design good platforms. You can't design a migration system that handles conflicts if you've never written a migration. You can't design isolation boundaries if you don't understand how a database connection pool works. You can't build semantic observability if you've never been the person debugging a production incident from raw logs. The understanding comes from doing the work.

So if AI is writing more and more of the code, and junior engineers are getting fewer reps of building and breaking things themselves, how do they develop the deep understanding that platform engineering requires? Civil engineering solved this with structured apprenticeship. You don't go from coursework to designing bridges. There are years of supervised practice, increasing responsibility, professional licensing exams. The judgment develops through guided experience, not just classroom instruction.

I don't think software engineering needs PE exams. But we need to take this seriously.

Here's a concrete example. For years, I gave one guest lecture per semester in a software engineering course at CMU on reliably releasing software. Feature flags, metrics, observability, safe deployments, self-healing, rollback strategies. One lecture. A nice-to-know topic in a course full of other things. That content is now the whole game. It's not a single lecture anymore. It's the core of what platform engineers need to understand, and it deserves its own course, its own projects, its own curriculum.

CMU and programs like it need to build curriculum around platform engineering. Not as a footnote in a software engineering survey course, but as a first-class discipline. Teach students to design systems that are safe to build on. Teach them to think about blast radius, isolation, observability, self-healing. Give them broken systems and ask them to figure out why. Give them a platform and ask them to make it safe for a non-technical PM, or an AI agent, to ship changes without bringing everything down.

The profession is changing. The people who make it safe for everyone else to build, that's where it's heading. And the question of how we train those people is one we can't afford to put off.

