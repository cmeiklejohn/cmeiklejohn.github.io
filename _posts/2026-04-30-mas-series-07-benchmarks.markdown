---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 7: Benchmarks and What They Miss"
date:   2026-04-30 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

If you've read this far, you've noticed that every paper I've discussed has a number next to it. 85.9 percent on HumanEval. 12.5 percent on SWE-bench. 25 percent on TravelPlanner. These numbers do a lot of work in the multi-agent literature, and they also do a surprising amount of harm. This post is about the benchmarks themselves. What they measure. What they don't. And why ChatDev and MetaGPT can report contradictory results on each other without either one being obviously wrong.

<div class="mas-series-nav">
  <div class="mas-series-label">Getting Up to Speed on MAS</div>
  <ol>
    <li><a href="/ai/agents/mas-series/2026/04/24/mas-series-01-the-landscape.html">Part 1. The Landscape</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/25/mas-series-02-the-vocabulary.html">Part 2. The Vocabulary</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/26/mas-series-03-wave-one.html">Part 3. Wave 1: Can Agents Coordinate At All?</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/27/mas-series-04-wave-two.html">Part 4. Wave 2: Why It Breaks</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/28/mas-series-05-debate-state-coordination.html">Part 5. Debate, State, and Coordination</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/29/mas-series-06-verification-patterns.html">Part 6. Verification Patterns</a></li>
    <li class="mas-current"><strong>Part 7. Benchmarks and What They Miss (you are here)</strong></li>
    <li><a href="/ai/agents/mas-series/2026/05/01/mas-series-08-open-questions.html">Part 8. Open Questions</a></li>
  </ol>
</div>

## The Landscape

Here's every benchmark that's come up in the series so far, plus a few that haven't.

<div class="mas-compare-wrap">
<table class="mas-compare">
  <thead>
    <tr><th>Benchmark</th><th>Domain</th><th>What It Tests</th><th>Scale</th><th>Multi-Agent?</th><th>Notable Results</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>HumanEval</td>
      <td>Code generation</td>
      <td>Write a correct Python function from a docstring</td>
      <td>164 tasks</td>
      <td>No, single function</td>
      <td>MetaGPT 85.9%, AutoDev 91.5%</td>
    </tr>
    <tr>
      <td>MBPP</td>
      <td>Code generation</td>
      <td>Entry-level Python from description</td>
      <td>974 tasks</td>
      <td>No, single function</td>
      <td>MetaGPT 87.7%</td>
    </tr>
    <tr>
      <td>SWE-bench</td>
      <td>Software engineering</td>
      <td>Resolve real GitHub issues in real repos</td>
      <td>2,294 (Verified: 500)</td>
      <td>Designed for single agent</td>
      <td>SWE-agent 12.5%, Devin 13.9%</td>
    </tr>
    <tr>
      <td>GAIA</td>
      <td>General assistant</td>
      <td>Multi-step reasoning with tools, web, files</td>
      <td>466 tasks</td>
      <td>Yes, benefits from parallel tools</td>
      <td>AutoGen #1, Magentic-One 38%</td>
    </tr>
    <tr>
      <td>WebArena</td>
      <td>Web tasks</td>
      <td>Real websites: shopping, forums, CMS</td>
      <td>812 tasks</td>
      <td>Designed for single agent</td>
      <td>Magentic-One 32.8%</td>
    </tr>
    <tr>
      <td>AssistantBench</td>
      <td>Assistant tasks</td>
      <td>Open-ended web browsing plus reasoning</td>
      <td>214 tasks</td>
      <td>Designed for single agent</td>
      <td>Magentic-One 13.3%</td>
    </tr>
    <tr>
      <td>BrowseComp</td>
      <td>Web retrieval</td>
      <td>Hard information retrieval via deep browsing</td>
      <td>~1,500 tasks</td>
      <td>Benefits from parallel search</td>
      <td>Anthropic +90% multi vs single</td>
    </tr>
    <tr class="mas-row-highlight">
      <td>TravelPlanner</td>
      <td>Constrained planning</td>
      <td>Multi-constraint travel planning</td>
      <td>1,225 tasks</td>
      <td>Explicitly tests coordination</td>
      <td>Ou et al. 25% with notebook + orchestrator</td>
    </tr>
    <tr class="mas-row-highlight">
      <td>Silo-Bench</td>
      <td>Distributed coordination</td>
      <td>Algorithmic tasks requiring cross-agent synthesis</td>
      <td>30 tasks, 54 configs</td>
      <td>Designed for MAS evaluation</td>
      <td>Agents fail at synthesis</td>
    </tr>
  </tbody>
</table>
</div>

Two of these (highlighted) were designed with multi-agent evaluation in mind. The other seven were designed for single agents. Multi-agent systems get evaluated on them anyway.

## Why This Matters

When you run a multi-agent system on a single-agent benchmark, you're measuring the wrong thing. HumanEval gives you a pass-at-1 score. It doesn't tell you how many tokens you burned to get there. It doesn't tell you how many agent turns were redundant. It doesn't tell you what happened when one of your agents got stuck. If you care about coordination quality, none of this information is in the score.

This is why ChatDev and MetaGPT can report contradictory numbers on similar tasks. ChatDev's paper claims 88 percent executability. MetaGPT's paper claims 41 percent executability. Different benchmarks, different metrics, different evaluation criteria. Neither paper is obviously lying. Neither paper is obviously right. And the field has no standard way to resolve the contradiction.

<div class="mas-callout">
  <div class="mas-callout-label">What single-agent benchmarks can't measure</div>
  Coordination quality. Communication overhead. Redundant work between agents. Recovery behavior when one agent fails. The token cost of the coordination itself. How performance degrades with scale. These are the things that distinguish multi-agent systems from single agents. And they're invisible to HumanEval, SWE-bench, and every other benchmark designed around "does the output match the expected answer."
</div>

## When Multi-Agent Actually Helps

If you look across all the benchmark results, a pattern emerges about when multi-agent systems earn their coordination overhead.

<div class="mas-taxonomy">
  <h4>Where the Multi-Agent Premium Pays Off</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Helps</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-ok">Breadth-first search (BrowseComp: +90%)</span>
      <span class="mas-tax-chip mas-chip-ok">Hard multi-step reasoning (GAIA Level 3: 2x)</span>
      <span class="mas-tax-chip mas-chip-ok">Constrained planning with state sharing (TravelPlanner: 3.3x)</span>
      <span class="mas-tax-chip mas-chip-ok">Independent parallel subtasks</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Doesn't help</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-bad">Focused coding (SWE-bench: single agents win)</span>
      <span class="mas-tax-chip mas-chip-bad">Tasks needing shared context (most coding)</span>
      <span class="mas-tax-chip mas-chip-bad">Simple function generation (HumanEval: overhead not worth it)</span>
      <span class="mas-tax-chip mas-chip-bad">Distributed reasoning / synthesis (Silo-Bench)</span>
    </div>
  </div>
</div>

This is the benchmark-level version of the conclusion I've been building toward across the whole series. Multi-agent earns its overhead on specific task shapes: breadth-first, parallel-decomposable, state-sharing-friendly. On other task shapes, it costs more than it delivers. The benchmarks, taken together, are unambiguous about this. It's just that most individual benchmarks can't show it.

## The Benchmark Problem, Stated Plainly

<div class="mas-callout">
  <div class="mas-callout-label">The benchmark gap</div>
  Most widely-used benchmarks (HumanEval, MBPP, SWE-bench, WebArena) were designed for single agents. Multi-agent systems get shoehorned into them, but the benchmarks can't measure coordination quality, communication overhead, or failure recovery, which are the things that distinguish MAS from single agents. TravelPlanner and Silo-Bench are rare exceptions that explicitly test multi-agent dynamics. ChatDev and MetaGPT reporting contradictory results on each other is a direct consequence of this gap.
</div>

Chen et al.'s survey names three evaluation-level challenges: no standardized benchmarks, no objective metrics, no common framework for individual vs aggregate evaluation. All three are symptoms of the same underlying issue. The field hasn't agreed on what it's measuring.

There are a few ways this gets resolved. One is that better MAS-specific benchmarks emerge (TravelPlanner and Silo-Bench are early signs). Another is that production telemetry replaces synthetic benchmarks (Anthropic's internal research eval). A third is that the field matures enough to distinguish "this benchmark tests single-agent capability" from "this benchmark tests multi-agent capability," and stops reporting contradictory single-agent numbers as if they were MAS comparisons.

None of these are fully here yet. If you're reading a paper and the headline number is HumanEval Pass@1, you're probably looking at a single-agent capability test dressed up as a MAS evaluation. Calibrate accordingly.

## What I Look For

When I read a paper with benchmark numbers now, here's what I check:

1. Is this benchmark designed for single agents or multi-agent systems?
2. If it's single-agent, are they comparing against single-agent baselines, or are they using it to claim MAS superiority?
3. What's the token cost of the system? If that number isn't reported, I assume it's high.
4. Do they report failure rates or just success rates? MAST data tells us this matters.
5. Is this the first number they cite, or are they hiding it behind a friendlier number?

Most wave-1 papers fail several of these checks. The agentic coding papers pass them, and wave-2 papers are starting to. This is partly why the post-2024 literature is more trustworthy than the 2023 literature.

Next post, the last in the series: open questions. What's missing. What's next. What I'd read if I were doing this again. And the research gap that I keep tripping over: the absence of a rigorous distributed systems foundation underneath the multi-agent AI work.
