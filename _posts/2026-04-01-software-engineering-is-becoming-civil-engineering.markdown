---
layout: post
title:  "Software Engineering Is Becoming Civil Engineering"
date:   2026-04-01 00:00:00 -0000
group: ai
categories: ai engineering
---

I gave a guest lecture on AI in [Michael Hilton's](https://www.cs.cmu.edu/~mhilton/) Foundations of Software Engineering course ([CMU 17-313](https://cmu-313.github.io)) today. One of my favorite things about lecturing is the conversations that happen afterward, the ones that go in directions nobody planned. This one hasn't left my head: software engineering is going through the same transition that building went through in the 18th century, when structural design separated from craft construction and became its own discipline. What we now call civil engineering.

The welders who join steel beams on a bridge are skilled tradespeople. They're not involved in the structural design. They don't decide where the load-bearing members go. They don't reason about wind shear or seismic tolerance. But the bridge is *designed* so that a welder doing their job correctly can't bring the whole thing down. The structural engineer's job isn't to weld. It's to create a system where welding happens safely within well-defined constraints.

I think this is what's happening to software engineering right now. Not in five years. This year.

## The Split

Product managers are writing code. This is how I operate with [Zabriskie](/ai/zabriskie/community/2026/03/08/why-im-building-zabriskie.html), the app I'm building. I act as a product manager more than a programmer. I have non-technical collaborators who file bugs and feature requests, and Claude Code implements them directly. People who've never written a line of code are describing what they want, the AI writes it, they verify, it ships. The feedback loop is tight and the results are surprisingly good.

But someone has to design the bridge. Someone has to decide how the database schema handles multi-tenancy. Someone has to design the deployment pipeline so a bad change rolls back automatically. Someone has to build the abstraction layer that lets a product manager add a new notification type without accidentally breaking the payment flow. That's the platform engineer. The structural engineer of software.

The PMs writing features? That's the welding. And there's nothing wrong with it. But it only works if the bridge is designed right.

The profession is splitting. The mistake would be pretending it isn't happening.

## What the Platform Has to Guarantee

Here's where civil engineering has something important to teach us. The analogy goes deeper than I expected.

A civil engineer doesn't just design a bridge. They decide *where* the bridge goes based on geology, water flow, soil load. They specify the materials. They calculate the forces. They design the inspection regime. They assess the environmental impact. They ensure compliance with building codes. Then, and only then, does construction begin.

Every one of these has a software analog, and together they paint a picture of what platform engineering actually is:

- **Site selection and domain isolation.** A civil engineer picks the bridge site based on geology and terrain. In software, this is API design, domain boundaries, isolation between services. Get this wrong and every change becomes a potential cascading failure.

- **Material specification.** The engineer specifies what grade of steel, what concrete mix. In software, this is choosing the languages, databases, queues, and frameworks. These choices constrain what's possible. That's the point.

- **Load analysis.** Civil engineers design for 2-4x the expected load. Software needs the same discipline. Capacity planning, rate limiting, designing for 10x your expected traffic. When a PM ships a feature that goes viral, the platform can't buckle.

- **Inspection regimes.** A civil engineer designs how the bridge will be *inspected over its lifetime*. In software, this is observability and code review. Not "HTTP 500 on endpoint /api/notify" but "the notification feature deployed 20 minutes ago by the growth PM is failing for 12% of users." Semantic observability, not raw telemetry.

- **Codes and standards compliance.** Building codes encode decades of hard-won lessons from failures. In software, this is security standards, accessibility requirements, regulatory compliance. The platform enforces these as constraints, not suggestions. Violations get caught automatically, not by a human reviewer who might miss them.

- **Self-healing.** A bridge has expansion joints that absorb thermal stress without human intervention. Software needs the equivalent. When you detect elevated error rates or failed health checks, the system should automatically mitigate. Roll back the deploy. Disable the feature flag. A bad change at 3pm can't become a production incident at 3am.

## What Actually Changes About the Job

This isn't about coding becoming less important. It's about what you spend your time on.

Today, most software engineers spend the majority of their day writing features. Tomorrow, I think the best ones will spend their day designing the systems that make it safe for *anyone* to ship features. This is what I've been spending most of my time on with Zabriskie. I'm still deeply technical. I'm still writing code. But the code I write is the platform, the constraints, the safety nets. Not the feature itself.

The day-to-day shifts. Instead of "implement the notification preference screen," it's "design the notification system so that a PM can add a new notification type and the worst thing that happens if they get it wrong is that one notification doesn't send." Instead of writing the migration, it's designing the migration system so that conflicting migrations are detected and blocked automatically. Instead of fixing the bug, it's building the observability that surfaces the bug before a user reports it.

It's not a demotion. It's a different kind of engineering. And honestly, it's harder. Writing a feature is a bounded problem. Designing a platform that stays safe as dozens of people and agents ship changes to it every day, that's an open-ended one. With AI agents doing more of the feature work, the assumption has to be that individual changes will sometimes be imperfect. Agents hallucinate. They introduce subtle bugs. They make confident changes based on incomplete context. The platform has to absorb this. Not by making agents perfect, but by making the system tolerant of imperfection.

## The Hard Questions

I keep hearing the same anxiety from different directions. Engineers wondering what their job looks like in two years. Students wondering if they're learning the right things. At CMU, two questions came up that crystallized it for me.

The first: students early in their software engineering careers don't know how to tell when the AI is doing something wrong. They don't have the spidey-sense yet. The AI generates code that looks plausible, passes a surface-level review, and the student ships it. They can't smell the bad decision because they've never seen what a bad decision leads to. How do you develop judgment about something you've never experienced failing?

The second is even harder: where do our senior engineers come from? The ability to design good platforms, to make the right architectural calls, that comes from experience. You learn what breaks by building things that broke. You learn where to put the domain boundaries by having drawn them in the wrong place. You learn what to monitor by having been the person staring at useless dashboards during an incident at 2am. If AI is writing most of the code, and junior engineers aren't getting the reps of building and breaking things themselves, how do they develop the judgment to become the platform engineers we need?

These are connected, and they form a kind of paradox. The spidey-sense and the architectural intuition come from the same place: years of building things, watching them fail, and understanding why. You can't design a migration system that handles conflicts if you've never written a migration. You can't design isolation boundaries if you don't understand how a database connection pool works. You can't build semantic observability if you've never been the person debugging a production incident from raw logs. The understanding comes from doing the work.

But we're taking the coding away from students at the exact moment they need it most. The judgment to tell an AI it's wrong comes from having written enough code to know what right looks like. And we're handing them AI before they've had the chance to develop that judgment. So we're stuck: we need them to code to build intuition, but the industry is moving toward a world where they don't code.

I want to be clear: the SE curriculum isn't fundamentally wrong. Courses like 17-313 teach the right things. But it's been underserviced for a long time, and what was adequate before isn't adequate now. This is a sea change. The entire relationship between humans and code is shifting, and the curriculum needs to shift with it. Platform engineering can't be an afterthought or a single lecture in a survey course. It needs to be a first-class part of how we teach students to think about building software.

Civil engineering solved the experience problem with structured apprenticeship. You don't go from coursework to designing bridges. There are years of supervised practice, increasing responsibility, professional licensing exams. The judgment develops through guided experience, not just classroom instruction.

I don't think software engineering needs PE exams. But we need to take this seriously.

Here's a concrete example. For years, I gave one guest lecture per semester in a software engineering course at CMU on reliably releasing software. Feature flags, metrics, observability, safe deployments, self-healing, rollback strategies. One lecture. A nice-to-know topic in a course full of other things. That content is now the whole game. It's not a single lecture anymore. It's the core of what platform engineers need to understand, and it deserves its own course, its own projects, its own curriculum.

The profession is changing. The question of how we train the people who make it safe for everyone else to build is one we can't afford to put off.

