---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 8: Open Questions"
date:   2026-05-01 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

I started this series because I'd been reading multi-agent papers for weeks and wanted a map I wished I'd had on day one. This is the last post. I want to close it by laying out what the field still hasn't figured out, what I think is worth stealing from adjacent fields, and what I'd read if I had to start over.

<div class="mas-series-nav">
  <div class="mas-series-label">Getting Up to Speed on MAS</div>
  <ol>
    <li><a href="{% post_url 2026-04-24-mas-series-01-the-landscape %}">Part 1. The Landscape</a></li>
    <li><a href="{% post_url 2026-04-25-mas-series-02-the-vocabulary %}">Part 2. The Vocabulary</a></li>
    <li><a href="{% post_url 2026-04-26-mas-series-03-wave-one %}">Part 3. Wave 1: Can Agents Coordinate At All?</a></li>
    <li><a href="{% post_url 2026-04-27-mas-series-04-wave-two %}">Part 4. Wave 2: Why It Breaks</a></li>
    <li><a href="{% post_url 2026-04-28-mas-series-05-debate-state-coordination %}">Part 5. Debate, State, and Coordination</a></li>
    <li><a href="{% post_url 2026-04-29-mas-series-06-verification-patterns %}">Part 6. Verification Patterns</a></li>
    <li><a href="{% post_url 2026-04-30-mas-series-07-benchmarks %}">Part 7. Benchmarks and What They Miss</a></li>
    <li class="mas-current"><strong>Part 8. Open Questions (you are here)</strong></li>
  </ol>
</div>

## Stealable Ideas

Some ideas are not yet general patterns in the field, but they're battle-tested in individual papers, and any new multi-agent system should probably adopt them. These are the things I'd take from one paper and apply in a different context.

<div class="mas-taxonomy">
  <h4>Things Any New Multi-Agent System Should Adopt</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Artifacts</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Structured documents between stages (MetaGPT)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Clarification</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Agents can ask before they act (ChatDev dehallucination)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Reflection</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Importance-triggered synthesis (Generative Agents)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Memory retrieval</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Recency x relevance x importance (Generative Agents)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Shared state</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Append-only notebook for structured info (Ou et al.)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Tool interface</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">ACI-quality commands with guardrails (SWE-agent)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Stuck detection</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Count loops, trigger replanning (Magentic-One)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Sandboxing</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Docker plus permission configs (AutoDev, OpenHands)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Verification</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Modality shift: code to visual, code to tests (Cursor, MetaGPT)</span>
    </div>
  </div>
</div>

None of these are hard to implement. None of them require a research breakthrough. They just haven't been brought together in a single system yet.

## Open Research Questions

The gaps are bigger. These are the questions I don't see anyone answering yet.

<div class="mas-paper-card" style="border-left-color: var(--mas-pink);">
  <div class="mas-card-title">
    <strong>1. Topology-to-reliability mapping</strong>
  </div>
  <p>CAMEL, ChatDev, MetaGPT all fix their topology at design time. AutoGen makes it configurable but doesn't study the effects. Nobody has varied topology systematically and measured reliability outcomes on the same task set. Hub-and-spoke vs mesh vs layered control: do they have different error rates, recovery times, incident severities? We don't know. Magentic-One's architecture lessons and MAS-FIRE's fault taxonomy are both one step away from this kind of study.</p>
</div>

<div class="mas-paper-card" style="border-left-color: var(--mas-pink);">
  <div class="mas-card-title">
    <strong>2. CRDTs for multi-agent shared state</strong>
  </div>
  <p>MetaGPT's shared pool grows monotonically with no conflict resolution. ChatDev discards dialogue at phase boundaries. Generative Agents' memories are per-agent with no sharing. Nobody has applied CRDT merge semantics to multi-agent shared state. The CALM theorem predicts when coordination-free works and when it doesn't. The engineering work of building CRDT-backed agent state hasn't been done.</p>
</div>

<div class="mas-paper-card" style="border-left-color: var(--mas-pink);">
  <div class="mas-card-title">
    <strong>3. Failure recovery, not just failure detection</strong>
  </div>
  <p>Every wave-1 system stops on failure. ChatDev stops after 10 rounds. MetaGPT stops after 3 test failures. AutoGen stops at max_round. None of them model recovery. Can a multi-agent system degrade gracefully, reassign work, escalate, fall back to a simpler approach? MAS-FIRE's fault injection framework is the closest thing to a way to test this, but the recovery strategies it would test don't exist in print yet.</p>
</div>

<div class="mas-paper-card" style="border-left-color: var(--mas-pink);">
  <div class="mas-card-title">
    <strong>4. Reflection for software engineering agents</strong>
  </div>
  <p>Generative Agents proved that periodic reflection produces better long-term behavior in simulation. No software engineering paper has tried this. After a Dev-to-QA loop cycles three times, can the system synthesize "this is an architectural issue, not a code issue" and change strategy? That's a reflection primitive adapted to the SE domain. The MAST data on step repetition (15.7 percent) suggests this would help directly.</p>
</div>

<div class="mas-paper-card" style="border-left-color: var(--mas-pink);">
  <div class="mas-card-title">
    <strong>5. Benchmark reliability</strong>
  </div>
  <p>ChatDev and MetaGPT report contradictory results on each other. Different benchmarks, different metrics, no reproducibility. Incident-level logging against real codebases might provide more trustworthy reliability measurement than self-reported aggregate benchmarks. This is infrastructure work. It's expensive. But the alternative is a field that can't actually tell you which system is better.</p>
</div>

<div class="mas-paper-card" style="border-left-color: var(--mas-pink);">
  <div class="mas-card-title">
    <strong>6. Backpressure and escalation protocols</strong>
  </div>
  <p>MetaGPT's Architect can hallucinate an impossible interface; the Engineer just tries to implement it. ChatDev's dehallucination is the closest thing to backpressure, but it's prompt-level. Can agents formally reject or request revision of upstream artifacts? What's the protocol? Does it improve outcomes, or does it just add latency? This is the place where distributed systems vocabulary (flow control, rejection, retry) maps most directly into multi-agent AI, and it's been barely explored.</p>
</div>

## The Distributed Systems Bridge

The research gap I find most interesting is the one I've been flagging throughout this series. The multi-agent AI field has reinvented several problems that distributed systems solved twenty or thirty years ago.

<div class="mas-compare-wrap">
<table class="mas-compare">
  <thead>
    <tr><th>Distributed systems problem</th><th>Multi-agent equivalent</th><th>Status in MAS literature</th></tr>
  </thead>
  <tbody>
    <tr><td>Lost updates</td><td>Two agents overwriting each other's work</td><td>Not addressed</td></tr>
    <tr><td>Causal consistency</td><td>Ordering agent actions across a pipeline</td><td>Not addressed</td></tr>
    <tr><td>Coordination avoidance (CALM)</td><td>When agents can work without synchronization</td><td>Not applied</td></tr>
    <tr><td>CRDTs</td><td>Merging divergent agent views of shared state</td><td>Not applied</td></tr>
    <tr><td>Fault injection (Jepsen)</td><td>MAS-FIRE (starting to emerge)</td><td>Early work</td></tr>
    <tr><td>Back pressure</td><td>Rejecting upstream inputs</td><td>Not formalized</td></tr>
    <tr><td>Escalation / circuit breaking</td><td>What happens when an agent fails</td><td>Not addressed</td></tr>
  </tbody>
</table>
</div>

These aren't one-to-one mappings. LLM agents have features that distributed systems nodes don't (they hallucinate, their behavior is probabilistic, their errors are semantic rather than syntactic). But the underlying coordination problems are the same. The right move is to take what worked in distributed systems, adapt it to the semantic messiness of LLMs, and build from there.

<div class="mas-callout">
  <div class="mas-callout-label">Where I think the field is going</div>
  Wave 1 asked whether agents could coordinate at all. The agentic coding turn showed that for a lot of tasks you don't need them to. Wave 2 is about why MAS breaks when you do need it. What comes next, I think, is the wave where multi-agent AI stops pretending it isn't a distributed systems problem and starts applying the full toolkit: CRDTs for shared state, causal ordering for handoffs, fault injection for reliability testing, coordination-avoidance theorems for knowing when to bother synchronizing at all. The groundwork is there. The application hasn't happened.
</div>

## What I'd Read If I Were Starting Over

If you have limited time and want to get the core of the field fast, here's the reading list I'd give my past self.

1. [Tran et al. survey (2025)](https://arxiv.org/abs/2501.06322) for vocabulary.
2. [CAMEL](https://arxiv.org/abs/2303.17760) for the simplest wave-1 mental model.
3. [MetaGPT](https://arxiv.org/abs/2308.00352) for the ambitious wave-1 mental model.
4. [SWE-agent](https://arxiv.org/abs/2405.15793) for the interface-design lesson from the agentic coding turn.
5. [Magentic-One](https://arxiv.org/abs/2411.04468) for a real multi-agent system from the same period.
6. [MAST](https://arxiv.org/abs/2503.13657) for what actually goes wrong in the wild.
7. [Anthropic's research system post](https://www.anthropic.com/engineering/multi-agent-research-system) for production lessons.
8. [Ou et al. on information sharing](https://arxiv.org/abs/2508.12981) for what state sharing actually buys you.
9. [CALM theorem](https://arxiv.org/abs/1901.01930) for the theoretical bridge to distributed systems.

Nine papers. If you read those in that order, you have a working model of the field. You won't have read everything, but you'll have read enough to understand where new papers fit when you encounter them.

## Closing

When I started reading this literature, I thought I was looking at a niche subfield of LLM research. What I found was the multi-agent AI community quietly rediscovering distributed systems, usually without the vocabulary to name what they were rediscovering. Every paper has pieces of the answer. None of them have the full picture. And the full picture, I think, will come from someone who knows both fields well enough to actually bridge them.

That's the work I'm doing in [Caucus]({% post_url 2026-04-08-cursor-agents-caucus-v1 %}). It's also the work I think the field needs more of, and the reason I wrote this series in the first place. If I've saved you a few weeks of reading, that was the whole point.

Thanks for reading.
