---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 4: Wave 2 (Why It Breaks)"
date:   2026-04-27 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

By 2025, two things had happened. Wave-1 architectures were running in production (Anthropic had shipped its research system; the open-source ecosystem around orchestrator-worker patterns was maturing). The agentic coding turn had made clear that multi-agent was not the right tool for focused coding, and narrowed the interesting MAS question to "when we do use it, why does it break?"

This wave is where I find the literature most useful, because it's where empirical work finally catches up with the claims of wave 1.

<div class="mas-series-nav">
  <div class="mas-series-label">Getting Up to Speed on MAS</div>
  <ol>
    <li><a href="/ai/agents/mas-series/2026/04/24/mas-series-01-the-landscape.html">Part 1. The Landscape</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/25/mas-series-02-the-vocabulary.html">Part 2. The Vocabulary</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/26/mas-series-03-wave-one.html">Part 3. Wave 1: Can Agents Coordinate At All?</a></li>
    <li class="mas-current"><strong>Part 4. Wave 2: Why It Breaks (you are here)</strong></li>
    <li><a href="/ai/agents/mas-series/2026/04/28/mas-series-05-debate-state-coordination.html">Part 5. Debate, State, and Coordination</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/29/mas-series-06-verification-patterns.html">Part 6. Verification Patterns</a></li>
    <li><a href="/ai/agents/mas-series/2026/04/30/mas-series-07-benchmarks.html">Part 7. Benchmarks and What They Miss</a></li>
    <li><a href="/ai/agents/mas-series/2026/05/01/mas-series-08-open-questions.html">Part 8. Open Questions</a></li>
  </ol>
</div>

## MAST: 14 Failure Modes from 1,600 Traces

<div class="mas-paper-card mas-mast">
  <div class="mas-card-title">
    <strong>Why Do Multi-Agent LLM Systems Fail? (MAST)</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2503.13657">arXiv 2503.13657</a> · NeurIPS 2025 D&amp;B</span>
  </div>
  <p class="mas-card-oneliner">1,600 annotated traces across 7 frameworks. First empirical taxonomy of why MAS break.</p>
  <div class="mas-card-bet">Core contribution: MAST taxonomy of 14 failure modes in 3 categories</div>
  <ul>
    <li>1,600+ traces across MetaGPT, ChatDev, HyperAgent, AppWorld, AG2, Magentic-One, OpenManus</li>
    <li>41 to 87 percent failure rates across all frameworks; systemic, not isolated</li>
    <li>Top 3 failures: step repetition (15.7 percent), reasoning-action mismatch (13.2 percent), unaware of termination (12.4 percent)</li>
    <li>Systems with explicit verifiers (MetaGPT, ChatDev) had fewer failures</li>
    <li>Inter-agent failures require "theory of mind"; agents can't model each other's information needs</li>
    <li>Adding high-level objective verification gave +15.6 percent improvement</li>
    <li>LLM-as-Judge pipeline: 94 percent accuracy against human experts</li>
  </ul>
  <div class="mas-card-source">
    Cemri, Pan, Yang, Agrawal, Chopra, Tiwari, Keutzer, Parameswaran, Klein, Ramchandran, Zaharia, Gonzalez, Stoica
  </div>
</div>

The MAST paper is the one I keep coming back to. It's the first rigorous empirical study of multi-agent failures. The authors took 1,600 execution traces from seven popular multi-agent frameworks, annotated each one with human experts, and built a 14-mode failure taxonomy.

The headline number is that every framework they tested had failure rates between 41 and 87 percent. Every single one. These are the production frameworks. These are the systems people cite in their papers. And they fail almost as often as they succeed.

<div class="mas-taxonomy">
  <h4>MAST's 14 Failure Modes</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">FC1 System Design</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Disobey task spec (11.8%)</span>
      <span class="mas-tax-chip">Disobey role spec (1.5%)</span>
      <span class="mas-tax-chip">Step repetition (15.7%)</span>
      <span class="mas-tax-chip">Loss of conversation history (2.8%)</span>
      <span class="mas-tax-chip">Unaware of termination (12.4%)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">FC2 Inter-Agent</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Conversation reset (2.2%)</span>
      <span class="mas-tax-chip">Fail to ask for clarification (6.8%)</span>
      <span class="mas-tax-chip">Task derailment (7.4%)</span>
      <span class="mas-tax-chip">Information withholding (0.85%)</span>
      <span class="mas-tax-chip">Ignored other agent's input (1.9%)</span>
      <span class="mas-tax-chip">Reasoning-action mismatch (13.2%)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">FC3 Verification</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Premature termination (6.2%)</span>
      <span class="mas-tax-chip">No/incomplete verification (8.2%)</span>
      <span class="mas-tax-chip">Incorrect verification (9.1%)</span>
    </div>
  </div>
</div>

The taxonomy matters because it lets you diagnose specific failures. When someone says "my agent got stuck in a loop," you can now ask whether that's step repetition (FM-1.3) or conversation reset (FM-2.1). Those have different causes and different fixes.

The finding that the paper downplays but I think is most important: the inter-agent failures are the hardest to fix. FC1 issues are prompt engineering problems. You can get meaningful improvements by rewriting role specifications. FC2 issues require what the paper calls "theory of mind," meaning the agents don't accurately model each other's information needs. Prompt fixes don't help there. The solutions are structural.

## MAS-FIRE: Fault Injection for Multi-Agent Systems

<div class="mas-paper-card mas-mast">
  <div class="mas-card-title">
    <strong>MAS-FIRE: Fault Injection and Reliability Evaluation for LLM-Based Multi-Agent Systems</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2602.19843">arXiv 2602.19843</a> · February 2026</span>
  </div>
  <p class="mas-card-oneliner">The first systematic fault injection framework for LLM-based MAS.</p>
  <div class="mas-card-bet">Core contribution: Active probing via fault injection, not just passive observation</div>
  <ul>
    <li>15 fault types: 8 intra-agent (planning, memory, reasoning, action) + 7 inter-agent (config, instruction, communication)</li>
    <li>Three injection mechanisms: prompt modification, response rewriting, message routing manipulation</li>
    <li>Tested on MetaGPT, Table-Critic, CAMEL with GPT-5 and DeepSeek-V3</li>
    <li>Key finding: config and instruction faults are catastrophic (Robustness Score = 0 percent for Blind Trust on MetaGPT)</li>
    <li>Capability paradox: GPT-5's strict compliance hurts under Blind Trust (6.3 percent) vs DeepSeek-V3 (70.6 percent)</li>
    <li>Linear pipelines extremely vulnerable; iterative architectures resilient (79-91 percent)</li>
    <li>Shared message pools neutralize memory faults (+25 percent advantage)</li>
  </ul>
</div>

MAST is observational. You watch real failures and categorize them. MAS-FIRE is the complement: you inject failures on purpose and measure how the system handles them. This is standard practice in distributed systems (Chaos Engineering, Jepsen) but it's new for LLM agents.

The taxonomy is worth reading carefully.

<div class="mas-taxonomy">
  <h4>MAS-FIRE's 15 Fault Types</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Intra-agent</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Inexecutable Plan</span>
      <span class="mas-tax-chip">Critical Info Loss</span>
      <span class="mas-tax-chip">Memory Loss</span>
      <span class="mas-tax-chip">Context Length Violation</span>
      <span class="mas-tax-chip">Hallucination</span>
      <span class="mas-tax-chip">Tool Selection Error</span>
      <span class="mas-tax-chip">Param Filling Error</span>
      <span class="mas-tax-chip">Param Format Error</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Inter-agent</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Role Ambiguity</span>
      <span class="mas-tax-chip">Blind Trust</span>
      <span class="mas-tax-chip">Instruction Logic Conflict</span>
      <span class="mas-tax-chip">Instruction Ambiguity</span>
      <span class="mas-tax-chip">Message Cycle</span>
      <span class="mas-tax-chip">Message Storm</span>
      <span class="mas-tax-chip">Broadcast Amplification</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Injection via</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Prompt Modification</span>
      <span class="mas-tax-chip">Response Rewriting</span>
      <span class="mas-tax-chip">Message Routing Manipulation</span>
    </div>
  </div>
</div>

The capability paradox is the finding I find most provocative. GPT-5 is a stronger model than DeepSeek-V3 by most benchmarks. But under the "Blind Trust" fault (where one agent is told to unconditionally accept instructions from another), GPT-5 fails almost completely (6.3 percent robustness) while DeepSeek-V3 holds up (70.6 percent). Why? Because GPT-5 is better at following instructions. It's also better at following bad instructions. Strict compliance is a liability when you can't trust the source.

The implication for system design is that you want agents that can question upstream inputs. Not agents that just obey.

## Silo-Bench: Communication Isn't Reasoning

<div class="mas-paper-card mas-mast">
  <div class="mas-card-title">
    <strong>Silo-Bench: Evaluating Distributed Coordination in Multi-Agent LLM Systems</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2603.01045">arXiv 2603.01045</a> · March 2026</span>
  </div>
  <p class="mas-card-oneliner">1,620 experiments showing agents can communicate but can't reason about distributed state.</p>
  <div class="mas-card-bet">Core finding: The bottleneck is synthesis, not acquisition</div>
  <ul>
    <li>30 algorithmic tasks across 3 communication complexity tiers, 54 configs, 1,620 experiments</li>
    <li>Central finding: agents form correct coordination topologies and actively exchange information</li>
    <li>But they systematically fail to synthesize distributed state into correct answers</li>
    <li>Bottleneck is information integration, not information acquisition</li>
    <li>Coordination overhead increases with agent scale, eventually eliminating parallelization benefits</li>
    <li>Merely increasing agent count cannot circumvent context limitations</li>
  </ul>
</div>

Silo-Bench is the paper I wish more people would read. The finding is simple and surprising. When you give multiple LLM agents a problem that requires distributed reasoning, they do all the communication correctly. They build the right topology. They exchange the right information. Then they fail to synthesize what they've gathered into a correct answer.

The bottleneck isn't the network. The bottleneck is the integration. Each agent has received the necessary pieces, and each agent individually fails to combine those pieces into the answer. This is not a coordination problem in the distributed systems sense. It's a reasoning problem that the coordination can't compensate for.

For wave-1 architectures, this result is devastating. The whole argument for agents-debating-each-other was that two agents looking at the same problem from different angles could synthesize a better answer than one agent alone. Silo-Bench says: maybe sometimes, but not for information-integration tasks, which is most tasks.

## What Wave 3 Adds That Wave 1 and 2 Missed

<div class="mas-taxonomy">
  <h4>Wave 3's New Contributions</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Observation</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-ok">Real failure data across multiple frameworks (MAST)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Injection</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-ok">Active fault testing (MAS-FIRE)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Limits</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-ok">What coordination can and can't fix (Silo-Bench)</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Production lessons</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-ok">Anthropic blog: 15x token cost, shared-context failures</span>
    </div>
  </div>
</div>

<div class="mas-callout">
  <div class="mas-callout-label">The wave-2 framing</div>
  MAST observes what breaks. MAS-FIRE tests what breaks by injecting it. Silo-Bench identifies the limits of what coordination can fix. Together they provide a reliability research stack that wave 1 didn't have. What's still missing: gates, recovery protocols, longitudinal failure datasets. The field is still working on these.
</div>

The critical thing wave 2 does is separate "this is a coordination problem" from "this is a reasoning problem." Wave 1 assumed everything was a coordination problem. Wave 2 says: if your agents can't synthesize distributed state individually, no amount of better message passing will save you. Fix the reasoning first. Coordinate second.

Next post: debate, state, and the CALM theorem. Three papers on whether agents should agree, disagree, or just share a notebook. And a theoretical result from distributed systems that explains which choice makes sense when.
