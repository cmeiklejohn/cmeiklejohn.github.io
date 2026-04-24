---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 5: Debate, State, and Coordination"
date:   2026-04-28 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

If wave 1 was about role-playing and the agentic coding turn was about interface quality, there's a parallel thread running through the field asking a more fundamental question: what should multiple agents actually <em>do</em> with each other? Debate? Share state? Coordinate? And are any of these interchangeable? This post is about four papers that sit at that intersection, including one that isn't really an LLM paper at all but is the clearest theoretical bridge from distributed systems into multi-agent AI.

{% include mas-series-nav.html current="mas-series-05-debate-state-coordination" %}

## Du et al.: Convergent Debate

<div class="mas-paper-card mas-debate">
  <div class="mas-card-title">
    <strong>Improving Factuality and Reasoning through Multiagent Debate</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2305.14325">arXiv 2305.14325</a> · ICML 2024</span>
  </div>
  <p class="mas-card-oneliner">Multiple LLM instances debate until they converge on an answer.</p>
  <div class="mas-card-bet">Core bet: Showing agents each other's answers changes their reasoning</div>
  <ul>
    <li>Mechanism: N agents independently answer, then see each other's responses and revise over multiple rounds</li>
    <li>Standard setup: 3 agents, 2 rounds (conservative; scales better with more)</li>
    <li>Works on black-box models with identical prompts across all tasks</li>
    <li>Key insight: debate is not voting; agents actually change their reasoning when shown alternatives</li>
    <li>Performance improves monotonically with more agents and more rounds</li>
    <li>"Society of minds" framing; collective intelligence from identical model instances</li>
  </ul>
  <div class="mas-card-source">Du, Li, Torralba, Tenenbaum, Mordatch</div>
</div>

Du et al. is the canonical multi-agent debate paper. The setup is simple. You run the same LLM multiple times on the same question. Each instance generates its own answer independently. Then you show each instance what the others said and ask it to revise. Do this for two or three rounds. The answers converge.

What makes this different from self-consistency or ensembling is that the agents see each other's reasoning, not just their answers. If instance A argued that the square root of 144 is 12 for reason X, and instance B argued it's 12 for reason Y, instance A's second attempt might incorporate reason Y. This is why "more rounds" helps. It's not just more samples. It's refinement.

## Liang et al.: Adversarial Debate

<div class="mas-paper-card mas-debate">
  <div class="mas-card-title">
    <strong>Encouraging Divergent Thinking through Multi-Agent Debate (MAD)</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2305.19118">arXiv 2305.19118</a> · EMNLP 2024</span>
  </div>
  <p class="mas-card-oneliner">Two debaters plus a judge, explicitly prompted to disagree.</p>
  <div class="mas-card-bet">Core bet: Once an LLM is confident, only external pressure unsticks it</div>
  <ul>
    <li>Identifies the Degeneration-of-Thought (DoT) problem: a confident LLM can't self-reflect its way out of wrong answers</li>
    <li>Two debaters plus a judge in a tit-for-tat format; judge has adaptive break</li>
    <li>Debaters explicitly prompted to disagree: "it's not necessary to fully agree"</li>
    <li>GPT-3.5 plus MAD beat GPT-4 baseline on commonsense translation</li>
    <li>Counter-intuitive arithmetic: 37 percent (MAD) vs 26 percent (single GPT-3.5) vs 51 percent (GPT-4)</li>
    <li>Failure mode: increasing debater count degrades performance (context length limits)</li>
    <li>Judge shows bias toward outputs matching its own architecture</li>
  </ul>
</div>

Liang et al. is a contrast to Du. Instead of converging agents, you have divergent ones. The debaters are prompted to disagree. The judge picks a winner or calls for another round. The paper's theoretical contribution is the Degeneration-of-Thought problem, which is: once an LLM commits to an answer with confidence, it can't self-reflect its way back. You have to push it.

The striking result is that GPT-3.5 with MAD beats GPT-4 alone on commonsense translation. You can get a stronger system by forcing a weaker model to argue with itself than by upgrading the model. This is a paper I'd hand to anyone who thinks "just use a better model" is always the right answer.

The failure mode is worth noting. MAD doesn't scale well beyond two debaters, because the context window fills up with arguments. And the judge develops a bias when different LLMs are used as debaters: it favors outputs that look like its own model family. Both of these are signs that the architecture is more fragile than the benchmark numbers suggest.

## Ou et al.: Shared State as Coordination

<div class="mas-paper-card" style="border-left-color: var(--mas-green);">
  <div class="mas-card-title">
    <strong>Analyzing Information Sharing and Coordination in Multi-Agent Planning</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2508.12981">arXiv 2508.12981</a> · August 2025</span>
  </div>
  <p class="mas-card-oneliner">A shared notebook plus a reflective orchestrator on travel planning.</p>
  <div class="mas-card-bet">Core bet: Explicit information tracking beats unstructured conversation</div>
  <ul>
    <li>Task: TravelPlanner benchmark, long-horizon, multi-constraint planning</li>
    <li>Shared notebook: reduces hallucination errors by 18 percent by forcing explicit information tracking</li>
    <li>Reflective orchestrator: directs conversation focus, reduces errors by additional 13.5 percent in targeted areas</li>
    <li>Combined: 25 percent pass rate (vs 7.5 percent single-agent baseline), 3.3x improvement</li>
    <li>Notebook alone helps more than orchestrator alone; state sharing greater than coordination for this task</li>
    <li>Directly answers: structured information sharing prevents agents from inventing unsupported details</li>
  </ul>
  <div class="mas-card-source">Ou, Vaduguru, Fried</div>
</div>

Ou et al. is the cleanest empirical study I've seen on what state sharing actually buys you. The task is constrained travel planning. The authors compare three setups: single agent, multi-agent with shared notebook, multi-agent with shared notebook and a reflective orchestrator. The notebook is the key mechanism. It's an append-only log where agents record what they've learned. Agents read from it before they propose anything new.

The headline finding is that the notebook reduces hallucination errors by 18 percent. The orchestrator adds another 13.5 percent. But read carefully: the notebook does more work than the orchestrator. Most of the benefit comes from forcing agents to write down what they know and read from a shared record. The coordination mechanism (the orchestrator) is secondary to the state sharing mechanism (the notebook).

This is a clear result for how to build multi-agent systems for constrained planning. Give them a shared structured state. Make them write to it. Make them read from it. Then worry about coordination.

## The CALM Theorem: When Coordination Is Avoidable

<div class="mas-paper-card" style="border-left-color: var(--mas-purple);">
  <div class="mas-card-title">
    <strong>Keeping CALM: When Distributed Consistency Is Easy</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/1901.01930">arXiv 1901.01930</a> · Hellerstein &amp; Alvaro · CACM 2020</span>
  </div>
  <p class="mas-card-oneliner">Which computations need coordination, and which don't.</p>
  <div class="mas-card-bet">Theoretical result: Monotonic programs are coordination-free; non-monotonic programs aren't</div>
  <ul>
    <li>CALM = Consistency As Logical Monotonicity</li>
    <li>Theorem: programs with consistent, coordination-free distributed implementations are exactly the monotonic programs</li>
    <li>If a computation only adds information (monotonic), it can run coordination-free and still get the right answer</li>
    <li>If it retracts information (non-monotonic), coordination is required for consistency</li>
    <li>Not yet applied to LLM agents in print, but the bridge is direct</li>
  </ul>
</div>

This one isn't a multi-agent LLM paper. It's a distributed systems paper from 2019 that states a theorem about when coordination is and isn't necessary. I include it here because it's the most direct theoretical bridge between classical distributed systems work and multi-agent AI, and no one in the LLM literature has formally made the connection yet.

Here's the CALM claim, translated for multi-agent AI. If your multi-agent system is only ever adding information to shared state (writing to a notebook, appending to a log, producing artifacts), the agents can run without coordinating with each other and still converge to a consistent answer. If the agents ever need to retract or update existing information, then coordination is required to avoid inconsistency.

<div class="mas-callout">
  <div class="mas-callout-label">Why this connects to Ou et al.</div>
  Ou et al.'s shared notebook is monotonic. Agents only append new information to it. Nothing gets retracted. That's why it works without heavy coordination machinery. The CALM theorem predicts this result. If Ou's notebook allowed agents to edit each other's entries, they would have needed coordination protocols (locks, version vectors, CRDTs) to keep the notebook consistent. They didn't, because they didn't need it.
</div>

## Putting It Together

The four papers in this post are doing different things, but they're converging on the same observation. The question "how should multiple agents work together" has more than one answer, and the answer depends on the structure of the task.

<div class="mas-compare-wrap">
<table class="mas-compare">
  <thead>
    <tr><th>Pattern</th><th>What Agents Do</th><th>When It Works</th><th>When It Doesn't</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>Convergent debate (Du)</td>
      <td>Show each other reasoning, converge</td>
      <td>Reasoning tasks with a right answer</td>
      <td>Context fills up fast; limited scale</td>
    </tr>
    <tr>
      <td>Adversarial debate (Liang)</td>
      <td>Argue opposite sides, judge decides</td>
      <td>Unstuck models with the DoT problem</td>
      <td>Judge bias; doesn't scale beyond 2</td>
    </tr>
    <tr>
      <td>Shared notebook (Ou)</td>
      <td>Append information to a log everyone reads</td>
      <td>Constrained planning, long-horizon tasks</td>
      <td>Tasks requiring real-time coordination</td>
    </tr>
    <tr>
      <td>Coordination-free (CALM)</td>
      <td>Monotonic writes, no coordination</td>
      <td>Aggregation, counting, set-building</td>
      <td>Anything that retracts or updates</td>
    </tr>
  </tbody>
</table>
</div>

Wave-1 multi-agent papers didn't distinguish between these. They all called themselves "multi-agent collaboration" and treated the coordination structure as interchangeable. It isn't. The structure has to match the task.

<div class="mas-callout">
  <div class="mas-callout-label">The distributed systems bridge</div>
  This is the point where my PhD work starts to feel directly relevant to the multi-agent AI literature. CALM, CRDTs, version vectors, causal consistency: these are all formalisms for when agents need to coordinate and when they don't. None of them have been rigorously applied to LLM-based multi-agent systems yet. That's an opportunity. It's also a caution. If the field doesn't pick up this vocabulary, it will keep reinventing solutions that distributed systems solved decades ago.
</div>

Next post: verification patterns. How do these systems know when they've done the right thing? Test execution, dialogue consensus, structural gates, and Cursor's visual feedback loop, which is the most interesting production-scale verification pattern I've seen and isn't in any of the papers yet.
