---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 2: The Vocabulary"
date:   2026-04-25 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

If you try to read multi-agent systems papers without the vocabulary, you will get nowhere. The field has settled on a shared set of words for the pieces of a system, and every paper now slots into those categories even when it pretends to be doing something novel. This post is about those words. Once you know them, you can read any paper in the field and know what it is and isn't claiming.

<div class="mas-series-nav">
  <div class="mas-series-label">Getting Up to Speed on MAS</div>
  <ol>
    <li><a href="{% post_url 2026-04-24-mas-series-01-the-landscape %}">Part 1. The Landscape</a></li>
    <li class="mas-current"><strong>Part 2. The Vocabulary (you are here)</strong></li>
    <li><a href="{% post_url 2026-04-26-mas-series-03-wave-one %}">Part 3. Wave 1: Can Agents Coordinate At All?</a></li>
    <li><a href="{% post_url 2026-04-27-mas-series-04-wave-two %}">Part 4. Wave 2: Why It Breaks</a></li>
    <li><a href="{% post_url 2026-04-28-mas-series-05-debate-state-coordination %}">Part 5. Debate, State, and Coordination</a></li>
    <li><a href="{% post_url 2026-04-29-mas-series-06-verification-patterns %}">Part 6. Verification Patterns</a></li>
    <li><a href="{% post_url 2026-04-30-mas-series-07-benchmarks %}">Part 7. Benchmarks and What They Miss</a></li>
    <li><a href="{% post_url 2026-05-01-mas-series-08-open-questions %}">Part 8. Open Questions</a></li>
  </ol>
</div>

Three surveys have done the work of consolidating the vocabulary. Each one cuts the space slightly differently, but together they give you the conceptual toolkit.

## Tran et al.: Actors, Types, Structures, Strategies

The most useful single survey is [Tran et al. (2025)](https://arxiv.org/abs/2501.06322). It defines a multi-agent system formally as a tuple of agents, collaboration channels, collective goals, and an environment. Then it taxonomizes the space along four axes.

<div class="mas-taxonomy">
  <h4>Tran's Four Axes</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Types</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Cooperation (aligned goals)</span>
      <span class="mas-tax-chip">Competition (conflicting goals)</span>
      <span class="mas-tax-chip">Coopetition (mixed)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Structures</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Centralized (hub)</span>
      <span class="mas-tax-chip">Decentralized (P2P)</span>
      <span class="mas-tax-chip">Hierarchical (layered)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Strategies</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Rule-based (voting, consensus)</span>
      <span class="mas-tax-chip">Role-based (SOP, expertise)</span>
      <span class="mas-tax-chip">Model-based (Theory of Mind)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Architecture</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Static (pre-defined)</span>
      <span class="mas-tax-chip">Dynamic (runtime adjustment)</span>
    </div>
  </div>
</div>

Most of the famous wave-1 papers are in one box: cooperative, hierarchical, role-based, static. Everyone is doing roughly the same thing, with small variations in how agents pass messages and what they produce at each step. The survey's most useful claim is that the optimal structure varies with the task. There is no universal topology.

## Zhou et al.: The Five-Component Agent

[Zhou et al. (2024)](https://link.springer.com/article/10.1007/s44336-024-00009-2) takes a different cut. Instead of asking how agents coordinate, they ask what each agent actually has inside it. They propose a five-component model that applies to any LLM-based agent.

<div class="mas-taxonomy">
  <h4>Zhou's Five Components</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">01 Profile</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">How the agent is created with role and expertise</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">02 Perception</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">How the agent observes its environment</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">03 Self-Action</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Memory, reasoning, and planning</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">04 Mutual Interaction</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Communication paradigm, structure, content</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">05 Evolution</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Self-reflection, progressive enhancement</span>
    </div>
  </div>
</div>

Reading this as a distributed systems person, the labels sound like things you'd recognize from any actor system. Profile is identity. Perception is input. Self-Action is local state plus computation. Mutual Interaction is message passing. Evolution is the weakest piece, because nobody has really figured out what "agent learning from its own history" looks like in production.

## Chen et al.: Applications and Unsolved Challenges

The third survey, [Chen et al. (2024)](https://arxiv.org/abs/2412.17481), is the one I'd skim rather than read in full. The applications chapter is useful, but what you actually want is the challenges section.

<div class="mas-taxonomy">
  <h4>Chen's Challenge Levels</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Agent-level</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Alignment for simulation</span>
      <span class="mas-tax-chip">Hallucination propagation</span>
      <span class="mas-tax-chip">Long-context limits</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Interaction-level</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Efficiency explosion</span>
      <span class="mas-tax-chip">Accumulative error</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Evaluation-level</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">No standardized benchmarks</span>
      <span class="mas-tax-chip">No objective metrics</span>
      <span class="mas-tax-chip">No individual vs aggregate frameworks</span>
    </div>
  </div>
</div>

The interaction-level challenges are the ones that most concern me. Efficiency explosion is the observation that multi-agent systems scale worse than linearly because each agent's autoregressive generation multiplies the token cost. Accumulative error is what it sounds like: errors made in round one propagate and amplify in rounds two, three, four.

## Mapping Papers Into These Taxonomies

The payoff of the vocabulary is that you can now categorize any paper in the field at a glance.

<div class="mas-compare-wrap">
<table class="mas-compare">
  <thead>
    <tr><th>System</th><th>Type</th><th>Structure</th><th>Strategy</th><th>Architecture</th></tr>
  </thead>
  <tbody>
    <tr><td>CAMEL</td><td>Cooperation</td><td>Decentralized pair</td><td>Role-based</td><td>Static</td></tr>
    <tr><td>ChatDev</td><td>Cooperation</td><td>Hierarchical pipeline</td><td>Role-based</td><td>Static</td></tr>
    <tr><td>MetaGPT</td><td>Cooperation</td><td>Centralized pool</td><td>Role + Rule-based</td><td>Static</td></tr>
    <tr><td>Debate (Du)</td><td>Competition</td><td>Decentralized all-to-all</td><td>Rule-based rounds</td><td>Static</td></tr>
    <tr><td>Generative Agents</td><td>Coopetition</td><td>Decentralized open env</td><td>Model-based retrieval</td><td>Dynamic</td></tr>
    <tr><td>Anthropic Research</td><td>Cooperation</td><td>Centralized orchestrator</td><td>Role-based</td><td>Dynamic</td></tr>
    <tr><td>AutoGen</td><td>Configurable</td><td>Configurable</td><td>Configurable</td><td>Static or Dynamic</td></tr>
  </tbody>
</table>
</div>

Most of the canonical papers sit in the cooperative, role-based, static quadrant. The interesting ones are the exceptions. Du et al. is the rare competitive debate paper. Generative Agents is the rare fully dynamic system. AutoGen tries to be everything at once, which is its whole thesis.

<div class="mas-callout">
  <div class="mas-callout-label">Why vocabulary matters</div>
  When two papers claim they "disagree," the vocabulary lets you ask: are they actually addressing the same problem? ChatDev and MetaGPT both call themselves "multi-agent software engineering frameworks" but they have different structures, different strategies, and different failure modes. You need the words to see that they are solving slightly different versions of the same problem.
</div>

## The Gap the Vocabulary Exposes

The taxonomies do something else besides categorize papers. They make gaps visible.

Zhou's "Evolution" component is the weakest across every system. Nobody has a real story for how agents learn from their own history in production. MetaGPT's "test-driven retry" is the closest wave-1 paper to Evolution, and it's still just a bounded retry loop with no memory of past attempts.

Tran's "dynamic architecture" category is almost empty. The wave-1 papers all fix their topology at design time. AutoGen makes topology configurable, but it's configured by the developer, not adjusted at runtime. The only system that truly adjusts at runtime is Generative Agents, and that's a simulation, not a production framework.

Chen's "evaluation-level" challenges are unsolved in a way that's embarrassing for the field. When ChatDev claims 88 percent executability and MetaGPT claims 41 percent on a comparable benchmark, you're not looking at a performance difference. You're looking at two papers measuring different things with different tools and calling them the same.

Next post: the wave-1 theory papers in detail. CAMEL, Generative Agents, ChatDev, MetaGPT, AutoGen. What each one actually builds, what each one trusts, and where each one breaks.
