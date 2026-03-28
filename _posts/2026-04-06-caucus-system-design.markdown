---
layout: post
title:  "Caucus: The System Design — Agents, CRDTs, and Fault Injection"
date:   2026-04-06 00:00:00 -0000
group: ai
categories: ai agents distributed zabriskie
---

[Last week](/ai/agents/distributed/zabriskie/2026/03/30/caucus.html) I talked about watching two Claude Code instances silently overwrite each other's database migrations — the same lost update problem that distributed databases solved decades ago. I traced the thread from that incident through ChatDev, MetaGPT, and AutoGen, and argued that nobody in the multi-agent space is treating these systems as what they actually are: distributed systems with unreliable components, partial information, and no shared clock.

I called the project Caucus — independent parties coming together to coordinate. This week: the system design.

## The Thesis

**Multi-agent LLM systems face the same coordination problems as distributed systems — consensus, state synchronization, fault tolerance — and techniques from the distributed systems literature can measurably improve agent team performance.**

This isn't a metaphor. It's a structural mapping.

## The Agents

Caucus coordinates a team of specialized agents working on a shared codebase:

- A focus group of **UserAgents**, each with a distinct personality and use case — a Phish fan who lives in the live show features, a film obsessive who cares about reviews and ratings, a vinyl collector who mostly posts about albums. They use the app the way real people do, and they find different bugs because they care about different things.
- A **TriageAgent** that classifies incoming reports and assigns them by priority.
- **QA Agents** for each platform — one for iOS, one for Android, one for web — that run visual smoke tests, tap through screens, and catch regressions before users do.
- One or more **DeveloperAgents** that write code, run tests, and submit patches.
- A **PMAgent** that holds the product roadmap and resolves conflicts when agents' priorities collide.

I've already built independent skills for each of these roles that I use in a single-agent setting — a bug triage skill, a visual QA skill that runs against the iOS Simulator and Android emulator, a release skill that builds and ships to TestFlight. They work well in isolation. The research question is what happens when they need to coordinate.

## Where Coordination Breaks Down

Every coordination problem in multi-agent LLM systems has a well-studied analog in the distributed systems literature. In most cases, the solutions have existed for decades. They're just not being used.

**Conflicts and stale reads.** In a distributed database, two replicas can accept writes concurrently — you need a strategy or data gets lost. In a multi-agent system, two DeveloperAgents can modify the same file concurrently. The file is the record. The agents are the replicas. The subtler version is the stale read: an agent reads the issue tracker, picks a bug, starts coding a fix — but another agent resolved that bug ten minutes ago. Redundant work based on stale state. Same anomaly, different substrate.

**Failure and recovery.** Any node can crash at any time. In a multi-agent system, an agent can hit a context window limit, hallucinate a fix, or just stop responding. The other agents have to detect this, recover in-progress work, and continue. This is the crash-recovery model from distributed systems, applied to LLM processes.

**Ordering without a clock.** Lamport's 1978 paper <span class="citation">(Lamport 1978)</span> established that you can reason about event ordering using happened-before relations even without a shared clock. Consider: a UserAgent files a bug report, a TriageAgent assigns it to DeveloperAgent-1, but DeveloperAgent-2 sees the original report before the assignment arrives. Two agents independently fixing the same issue because the system can't express causal ordering. Vector clocks <span class="citation">(Fidge 1988; Mattern 1989)</span> solve this. Nobody in the agent space is using them.

**Partition tolerance.** Communication failures — API timeouts, rate limits, one agent buried in a long task — split agents into groups that can't communicate. They diverge. When they reconnect, their states need to merge. CRDTs <span class="citation">(Shapiro et al. 2011)</span> were literally designed for this.

## A Friday Night at the Gorge

Let me make this concrete.

It's a Friday night. Goose is playing two nights at Madison Square Garden. Zabriskie has live show features — a setlist tracker that polls sources in real time, a live chat (we call it "Live Chomping"), and a banner that lights up when a show is happening. Users are watching the stream from their couches and posting in the app.

At 9:47pm, a user reports that the live setlist isn't updating. At the same time, another user reports that the chat is duplicating messages.

Without multi-agent coordination, this is my Friday night. I stop watching the show, open my laptop, fix both bugs sequentially, deploy twice. An hour gone.

Here's what the Caucus system should do: The **UserAgent** has already noticed both problems — it's been browsing the live show page all night. The **TriageAgent** classifies the setlist issue as critical and the chat duplication as high, assigns them to separate DeveloperAgents. **DeveloperAgent-1** fixes the polling query, **DeveloperAgent-2** fixes the WebSocket reconnection handler. Both patches ship. I'm still watching Goose.

But here's the thing that makes this a research project: what happens when both fixes touch the same handler? When the assignment message is delayed? When a "fix" passes its own tests but breaks something else? When an agent's context window fills up mid-fix?

These are coordination failures. And whether you're coordinating microservices or LLM agents, the failure modes are structurally identical.

## The Coordination Layer

The agents themselves aren't the contribution. The coordination layer is. I'm studying three dimensions, each with a baseline and an improved variant.

### Shared State

The baseline is what most multi-agent systems do today: a shared JSON file with last-write-wins semantics. Two concurrent updates? Last one wins, the other is silently lost.

The improved variant uses CRDT-inspired state management. Each agent maintains a local replica. Updates are tagged with vector clocks for causal ordering. Merge functions are conflict-free by construction — a grow-only set for known bugs, an append-only log for activity history, a last-writer-wins register with proper causal ordering for mutable fields. Both updates preserved and merged deterministically. No locks. No leader.

### Message Topology

The baseline is a star topology: all communication goes through the TriageAgent as a central hub. Simple, but a bottleneck and single point of failure.

The improved variant is a graph topology: agents communicate directly. I expect reduced latency and improved resilience, but increased message overhead — exactly the tradeoffs distributed systems research has been quantifying for decades.

### Fault Injection

This is where Filibuster comes back. For multi-agent systems, "things going wrong" means:

- **Agent dropout**: a DeveloperAgent crashes mid-fix
- **Hallucinated fix**: a "fix" that passes its own tests but introduces a regression
- **Contradictory context**: two agents have different beliefs about the codebase
- **Context overflow**: an agent loses track of earlier decisions
- **Delayed communication**: an agent works with outdated information
- **Byzantine agent**: an agent produces confidently wrong output

I'm building a fault injection layer that introduces each of these systematically, the same way Filibuster injects network failures between microservices.

## How I'll Know If It Works

Ten evaluation scenarios: seven real bugs from Zabriskie's history, three synthetic feature requests. Each has a known-good solution and a test suite. The experiment matrix crosses topology (star vs. graph), state synchronization (naive vs. CRDT), and failure mode (clean run, plus each of the six failure types).

The metrics are concrete: did the agents fix the bug? How many interactions? Did the fix break something else? How much communication overhead? After a failure, how long until the system was productive again?

Every experiment is reproducible. Every result is logged.

More to come. Follow along.

<div id="refs" class="references">
<div id="ref-fidge1988timestamps">
<p>Fidge, Colin J. 1988. "Timestamps in Message-Passing Systems That Preserve the Partial Ordering." <em>Proceedings of the 11th Australian Computer Science Conference</em> 10 (1): 56–66.</p>
</div>
<div id="ref-lamport1978time">
<p>Lamport, Leslie. 1978. "Time, Clocks, and the Ordering of Events in a Distributed System." <em>Communications of the ACM</em> 21 (7): 558–65.</p>
</div>
<div id="ref-mattern1989virtual">
<p>Mattern, Friedemann. 1989. "Virtual Time and Global States of Distributed Systems." <em>Parallel and Distributed Algorithms</em> 1 (23): 215–26.</p>
</div>
<div id="ref-shapiro2011comprehensive">
<p>Shapiro, Marc, Nuno Preguiça, Carlos Baquero, and Marek Zawirski. 2011. "A Comprehensive Study of Convergent and Commutative Replicated Data Types." <em>INRIA Technical Report</em> 7506. <a href="https://hal.inria.fr/inria-00555588" class="uri">https://hal.inria.fr/inria-00555588</a>.</p>
</div>
</div>
