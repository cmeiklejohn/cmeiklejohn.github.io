---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 1: The Landscape"
date:   2026-04-24 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

I've been reading multi-agent systems papers for weeks trying to figure out where the field actually is, and the honest answer is that it moves fast enough that any single paper is a snapshot, not a map. So this is the map I wish I'd had when I started. It's a short series of posts meant to get someone up to speed on multi-agent LLM systems without having to read thirty papers first.

Before I start, a note on the frame I'm going to use. I'm going to talk about two "waves" of multi-agent research, and one outside disruption that happened between them. I want to be upfront that the waves are a reader aid, not a historical claim. Nobody in 2023 was writing "wave 1" papers. The field did not hold a meeting and decide what wave it was in. The waves aren't like French New Wave cinema or second-wave feminism, where participants knew they were part of a movement and defined their work against prior generations. What I'm doing here is retrospective grouping, the kind you do to keep thirty papers straight in your head.

What the grouping captures is that certain clusters of papers share assumptions, benchmarks, and failure modes. What it misses is that parallel threads exist (debate, simulation, distributed-systems-adjacent work) that don't fit the wave structure at all, and that plenty of individual papers sit awkwardly between waves. If the framing helps you navigate the literature, keep it. If it gets in the way, drop it. The papers are what matter; the waves are scaffolding.

With that caveat in mind: two rough clusters, one outside disruption that reshaped both, and two rough questions the MAS field has been trying to answer.

<div class="mas-series-nav">
  <div class="mas-series-label">Getting Up to Speed on MAS</div>
  <ol>
    <li class="mas-current"><strong>Part 1. The Landscape (you are here)</strong></li>
    <li><a href="{% post_url 2026-04-25-mas-series-02-the-vocabulary %}">Part 2. The Vocabulary</a></li>
    <li><a href="{% post_url 2026-04-26-mas-series-03-wave-one %}">Part 3. Wave 1: Can Agents Coordinate At All?</a></li>
    <li><a href="{% post_url 2026-04-27-mas-series-04-wave-two %}">Part 4. Wave 2: Why It Breaks</a></li>
    <li><a href="{% post_url 2026-04-28-mas-series-05-debate-state-coordination %}">Part 5. Debate, State, and Coordination</a></li>
    <li><a href="{% post_url 2026-04-29-mas-series-06-verification-patterns %}">Part 6. Verification Patterns</a></li>
    <li><a href="{% post_url 2026-04-30-mas-series-07-benchmarks %}">Part 7. Benchmarks and What They Miss</a></li>
    <li><a href="{% post_url 2026-05-01-mas-series-08-open-questions %}">Part 8. Open Questions</a></li>
  </ol>
</div>

## Wave 1: Can Multiple LLMs Coordinate At All? (2023)

The first wave is the one most people have heard of. A cluster of papers came out in roughly a six-month window in 2023, all answering some version of the same question: if you put multiple LLMs together, can they do something one LLM cannot?

<div class="mas-timeline-wave">
  <div class="mas-wave-header">Wave 1 · Theory and Architecture</div>
  <div class="mas-wave-subhead">Can multiple LLMs coordinate at all? What's the right shape?</div>
  <div class="mas-timeline">
    <div class="mas-timeline-node mas-c-camel">
      <div class="mas-node-date">Mar 2023</div>
      <div class="mas-node-name">CAMEL</div>
      <div class="mas-node-desc">Two agents role-play</div>
    </div>
    <div class="mas-timeline-node mas-c-genagents">
      <div class="mas-node-date">Apr 2023</div>
      <div class="mas-node-name">Gen. Agents</div>
      <div class="mas-node-desc">Memory and reflection</div>
    </div>
    <div class="mas-timeline-node mas-c-debate">
      <div class="mas-node-date">May 2023</div>
      <div class="mas-node-name">Debate (Du)</div>
      <div class="mas-node-desc">Competition as coordination</div>
    </div>
    <div class="mas-timeline-node mas-c-chatdev">
      <div class="mas-node-date">Jul 2023</div>
      <div class="mas-node-name">ChatDev</div>
      <div class="mas-node-desc">Pairwise chat pipeline</div>
    </div>
    <div class="mas-timeline-node mas-c-metagpt">
      <div class="mas-node-date">Aug 2023</div>
      <div class="mas-node-name">MetaGPT</div>
      <div class="mas-node-desc">Artifacts and test execution</div>
    </div>
    <div class="mas-timeline-node mas-c-autogen">
      <div class="mas-node-date">Aug 2023</div>
      <div class="mas-node-name">AutoGen</div>
      <div class="mas-node-desc">Configurable framework</div>
    </div>
    <div class="mas-timeline-node mas-c-green">
      <div class="mas-node-date">Aug 2023</div>
      <div class="mas-node-name">AgentVerse</div>
      <div class="mas-node-desc">Dynamic group composition</div>
    </div>
  </div>
</div>

These papers are all proofs of concept. They show that multi-agent coordination is viable for some task, demonstrate the idea works on a benchmark, and argue that their particular coordination structure beats simpler baselines. [CAMEL](https://arxiv.org/abs/2303.17760) uses two agents role-playing. [Generative Agents](https://arxiv.org/abs/2304.03442) uses memory streams and reflection in a social simulation. [Du et al.](https://arxiv.org/abs/2305.14325) uses multi-round debate between identical model instances. [ChatDev](https://arxiv.org/abs/2307.07924) chains pairwise dialogues into a software development pipeline. [MetaGPT](https://arxiv.org/abs/2308.00352) replaces dialogue with structured artifacts and real test execution. [AutoGen](https://arxiv.org/abs/2308.08155) is a framework for building whatever topology you want.

The wave-1 papers share three assumptions that look very different in hindsight. First, they assume the benchmarks they run on are the task. Second, they treat failure as a termination condition: when the system stops converging, it just stops. Third, they trust agents to coordinate without formal concurrency control, shared memory protocols, or recovery paths. These assumptions are where wave 2 would later start to push back.

## What Happened Next Door (2024)

Before I get to wave 2, I have to account for a thing that happened in parallel that isn't really MAS research but reshaped what MAS has to answer for.

Through 2024, a wave of agentic coding systems shipped: [Devin](https://devin.ai), [SWE-agent](https://arxiv.org/abs/2405.15793), [OpenHands](https://arxiv.org/abs/2407.16741), [AutoDev](https://arxiv.org/abs/2403.08299), and Microsoft's [Magentic-One](https://arxiv.org/abs/2411.04468). These are not multi-agent systems in the wave-1 sense. Most of them are single agents with well-designed tool interfaces. The SWE-agent paper in particular showed that interface quality matters more than adding agents. They got a 10.7 percentage point improvement on SWE-bench from interface design alone, without changing the model.

I bring this up because you cannot read the MAS papers from 2025 onward without this context. Wave 1 had implicitly assumed that multi-agent coordination was the way to solve complex agentic tasks. By late 2024, the agentic coding community had empirically falsified that assumption for at least one large class of tasks (focused software engineering). Anthropic's [research system post](https://www.anthropic.com/engineering/multi-agent-research-system) from June 2025 states the conclusion plainly: multi-agent earns its overhead on "breadth-first queries with independent parallel subtasks" and underperforms on "tasks needing shared context, including most coding tasks."

<div class="mas-callout">
  <div class="mas-callout-label">Why this matters for MAS readers</div>
  The agentic coding papers aren't MAS research. But they narrowed the MAS claim. After 2024, "multi-agent for coding" became harder to defend without evidence, and the MAS field's next wave is partly a response to that. If you're reading a post-2024 MAS paper, it's almost certainly arguing implicitly against the single-agent-with-tools baseline that Devin and SWE-agent established. I won't spend a whole post on these systems because they're not MAS, but they belong on your mental map of the landscape.
</div>

Magentic-One is the interesting exception. It's a real multi-agent system with an orchestrator coordinating four specialized workers. It earns its overhead on hard multi-step reasoning (38 percent on GAIA) but not on focused coding. The stuck-counter mechanism it introduces (if an agent loops more than twice, reflect and replan) is one of the few genuine MAS design patterns to come out of this period. I'll come back to it in later posts.

## Wave 2: Why Does It Break? (2025 and Beyond)

The second MAS wave is where the field is now. With wave-1 systems running in production and the agentic coding turn having clarified when MAS is and isn't the right tool, people started asking: when MAS does fail, why? And how do we even test that?

<div class="mas-timeline-wave">
  <div class="mas-wave-header">Wave 2 · Why Does It Break?</div>
  <div class="mas-wave-subhead">MAS works sometimes. Now: why does it fail? How do you test reliability?</div>
  <div class="mas-timeline">
    <div class="mas-timeline-node mas-c-red">
      <div class="mas-node-date">Mar 2025</div>
      <div class="mas-node-name">MAST (Cemri)</div>
      <div class="mas-node-desc">14 failure modes, 1,600 traces</div>
    </div>
    <div class="mas-timeline-node mas-c-purple">
      <div class="mas-node-date">Jun 2025</div>
      <div class="mas-node-name">Anthropic Research</div>
      <div class="mas-node-desc">Production orchestrator-worker</div>
    </div>
    <div class="mas-timeline-node mas-c-green">
      <div class="mas-node-date">Aug 2025</div>
      <div class="mas-node-name">Info Sharing in Planning</div>
      <div class="mas-node-desc">Shared notebook on travel planning</div>
    </div>
    <div class="mas-timeline-node mas-c-red">
      <div class="mas-node-date">Feb 2026</div>
      <div class="mas-node-name">MAS-FIRE</div>
      <div class="mas-node-desc">Systematic fault injection for MAS</div>
    </div>
    <div class="mas-timeline-node mas-c-blue">
      <div class="mas-node-date">Mar 2026</div>
      <div class="mas-node-name">Silo-Bench</div>
      <div class="mas-node-desc">Communication-reasoning gap</div>
    </div>
  </div>
</div>

The [MAST paper](https://arxiv.org/abs/2503.13657) from Cemri and collaborators is the one I keep coming back to. They annotated 1,600 traces across seven popular multi-agent frameworks and built a taxonomy of 14 failure modes. Every framework they tested had failure rates between 41 and 87 percent. The top three failures are step repetition, reasoning-action mismatch, and being unaware of termination conditions. These are not model capability problems. They are system design problems.

[MAS-FIRE](https://arxiv.org/abs/2602.19843) goes the other direction. Instead of observing failures in the wild, they inject them on purpose. Fifteen fault types across intra-agent and inter-agent categories, three injection mechanisms, and a dual-level reliability metric. The most interesting result is what they call the capability paradox: GPT-5's strict instruction compliance becomes a liability under "Blind Trust" faults, where DeepSeek-V3's less compliant behavior holds up better.

[Silo-Bench](https://arxiv.org/abs/2603.01045) adds the third leg. 1,620 experiments showing that agents successfully form coordination topologies and actively exchange information, yet systematically fail to synthesize distributed state into correct answers. The bottleneck is not communication. The bottleneck is reasoning over distributed state.

## What Each Wave Trusts

If you want a one-sentence read on each wave, this is it.

<div class="mas-taxonomy">
  <div class="mas-tax-row">
    <span class="mas-tax-label">Wave 1</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Trusts that role structure and dialogue are enough</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Outside</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Agentic coding: trusts that good tools beat agent count</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Wave 2</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Trusts nothing and measures what breaks</span>
    </div>
  </div>
</div>

Read as a progression, the rough arc is: tell agents to coordinate, then notice that sometimes you don't need them to, then measure what happens when you do. Each step trusts the agents less and verifies more than the one before.

## What the Waves Don't Capture

The two-wave framing is tidy, which should make you suspicious. Some things it flattens or misses:

Parallel threads run outside the waves entirely. The [debate papers](https://arxiv.org/abs/2305.14325) are a cluster of their own, focused on multiple instances of the same model arguing with each other. They barely cite the coordination-theory papers and mostly don't get cited back. Generative Agents is a social simulation paper that sits uncomfortably in wave 1 because it came out in 2023, but its descendants (game agents, persona simulations) are their own research community. Distributed-systems-adjacent work on shared state and coordination avoidance is a separate thread that's only now starting to touch the MAS literature.

Individual papers don't fit cleanly. AutoGen is a 2023 paper that kept evolving through 2024. The Anthropic research post is engineering content, not a research paper. Magentic-One sits between the agentic coding turn and the reliability wave. Calling any of these "wave 1" or "wave 2" is a judgment call, not a fact.

Keep all of that in mind as you read the rest of the series. The waves are a way to group papers for comprehension, not a claim about how the field evolved.

## What's Coming

The next seven posts build on this landscape.

Part 2 covers the vocabulary the field uses for itself. Three surveys have done the work of consolidating the shared terms, and once you know the vocabulary you can read any paper in the field at a glance.

Parts 3 and 4 go deep on the two waves. Part 3 is the canonical coordination-theory papers: CAMEL, ChatDev, MetaGPT, AutoGen, what each one actually builds and where each one quietly disagrees with the others. Part 4 is the reliability wave: MAST, MAS-FIRE, Silo-Bench, and what happens when you try to measure a multi-agent system honestly.

Part 5 covers the parallel threads. Multi-agent debate (Du, Liang), shared state as coordination (Ou et al.), and the CALM theorem as a bridge between distributed systems and multi-agent AI.

Parts 6 and 7 are cross-cutting. Part 6 is verification patterns, including Cursor's visual feedback loop, which is the most interesting production-scale verification pattern I've seen and isn't in any of the papers. Part 7 is benchmarks, what they measure, what they miss, and why ChatDev and MetaGPT can report contradictory results on each other without either being obviously wrong.

Part 8 is what I think is still missing, what's worth stealing from adjacent fields, and what I'd read if I had to start over.

Next post: the vocabulary.
