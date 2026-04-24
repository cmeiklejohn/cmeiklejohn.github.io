---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 3: Wave 1 (Can Agents Coordinate At All?)"
date:   2026-04-26 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

Wave 1 is the cluster of papers from 2023 that people actually cite. When someone says "I read the multi-agent papers," they usually mean these. In this post I'm going to walk through the canonical five, explain what each one actually builds, and show where they agree and where they quietly disagree with each other.

{% include mas-series-nav.html current="mas-series-03-wave-one" %}

## CAMEL: Two Agents Role-Playing

<div class="mas-paper-card mas-camel">
  <div class="mas-card-title">
    <strong>CAMEL: Communicative Agents for "Mind" Exploration</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2303.17760">arXiv 2303.17760</a> · NeurIPS 2023</span>
  </div>
  <p class="mas-card-oneliner">Two LLMs role-play until the task is done.</p>
  <div class="mas-card-bet">Core bet: Prompt constraints keep agents on task</div>
  <div>
    <span class="mas-tag">2 agents</span>
    <span class="mas-tag">role-play</span>
    <span class="mas-tag">inception prompting</span>
  </div>
  <ul>
    <li>AI User (instructor) and AI Assistant in a structured dialogue loop</li>
    <li>Inception prompting: symmetric system prompts with explicit constraints like "Never flip roles"</li>
    <li>Task specifier agent elaborates vague human input into concrete tasks</li>
    <li>Documented failure modes: role flipping, instruction repetition, vague responses, conversational loops</li>
    <li>All mitigations are prompt-level, no structural enforcement</li>
  </ul>
</div>

CAMEL is the simplest of the wave-1 papers and also the most honest. It's two LLMs. One plays a user, one plays an assistant. They talk. The paper's main contribution is inception prompting, which is a way of writing system prompts that keep agents from breaking character. The failure modes the paper documents are the failure modes you'd expect: agents flip roles, agents repeat themselves, agents give vague answers.

What's missing from CAMEL is any structural enforcement. If the agent flips roles, nothing stops it except a prompt instruction that says "don't flip roles." There's no protocol-level guarantee. This is a pattern you'll see repeated across wave-1: trust the prompt, hope for the best.

## Generative Agents: Memory, Reflection, and Planning

<div class="mas-paper-card mas-genagents">
  <div class="mas-card-title">
    <strong>Generative Agents: Interactive Simulacra of Human Behavior</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2304.03442">arXiv 2304.03442</a> · UIST 2023</span>
  </div>
  <p class="mas-card-oneliner">Give agents memory, reflection, and planning so they behave believably over time.</p>
  <div class="mas-card-bet">Core bet: Retrieval scoring produces believable behavior</div>
  <div>
    <span class="mas-tag">25 agents</span>
    <span class="mas-tag">memory stream</span>
    <span class="mas-tag">reflection</span>
    <span class="mas-tag">planning</span>
  </div>
  <ul>
    <li>Memory stream: every observation stored with timestamp and importance score (LLM-rated 1 to 10)</li>
    <li>Retrieval: weighted sum of recency (exponential decay), relevance (cosine similarity), and importance</li>
    <li>Reflection: triggered when accumulated importance crosses a threshold (roughly 2-3 times per day)</li>
    <li>Planning: top-down recursive (day, hour, 5-15 minute blocks); replans on unexpected events</li>
    <li>Emergent behaviors: a Valentine's Day party self-organized from one suggestion; info diffusion from 4 percent to 32 percent awareness in two game days</li>
  </ul>
</div>

This paper is the outlier in wave-1, because it isn't trying to build software. It's a social simulation. 25 agents living in a Sims-style town. What makes it interesting for multi-agent systems is that it's the only wave-1 paper that takes memory seriously. Every observation gets stored with a timestamp and an importance score. When an agent needs to act, it retrieves memories using a weighted combination of recency, relevance, and importance. Reflections are higher-level thoughts synthesized from clusters of observations.

None of the software engineering papers in wave-1 do anything like this. They don't need to, because their tasks have clear start and end conditions. But when you look at what production multi-agent systems are starting to need, the Generative Agents architecture has more of the right pieces than MetaGPT does.

## ChatDev: Pairwise Chat as a Software Pipeline

<div class="mas-paper-card mas-chatdev">
  <div class="mas-card-title">
    <strong>ChatDev: Communicative Agents for Software Development</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2307.07924">arXiv 2307.07924</a></span>
  </div>
  <p class="mas-card-oneliner">Chain pairwise dialogues into a software development pipeline.</p>
  <div class="mas-card-bet">Core bet: Dialogue convergence equals correct output</div>
  <div>
    <span class="mas-tag">pairwise chat</span>
    <span class="mas-tag">phase pipeline</span>
    <span class="mas-tag">dehallucination</span>
  </div>
  <ul>
    <li>Fixed pipeline: Design, then Coding, then Testing; each phase is an instructor-assistant dialogue</li>
    <li>Communicative dehallucination: the assistant flips role and asks clarifying questions before committing to an answer</li>
    <li>Short-term memory (full dialogue within a phase) and long-term memory (extracted solutions across phases)</li>
    <li>Termination: 10 rounds max, or two consecutive rounds without changes</li>
    <li>No escalation path when convergence fails, it just stops</li>
  </ul>
</div>

ChatDev is the paper that got me into this literature in the first place. It's a pipeline of pairwise dialogues. Pairs of agents talk about design, then pairs of agents talk about coding, then pairs of agents talk about testing. The most interesting mechanism is communicative dehallucination, which is a prompt pattern where the assistant asks clarifying questions before answering. This is the closest any wave-1 paper gets to backpressure.

The structural problem with ChatDev is that when agents can't converge in 10 rounds, the system just stops. There's no fallback. No mechanism for the system to notice that it's stuck and escalate. No concurrency control on the shared artifacts. I wrote about [how this breaks down in practice]({% post_url 2026-03-30-multi-agent-systems-have-a-distributed-systems-problem %}) a few weeks ago.

## MetaGPT: Structured Artifacts and Test Execution

<div class="mas-paper-card mas-metagpt">
  <div class="mas-card-title">
    <strong>MetaGPT: Meta Programming for Multi-Agent Collaborative Framework</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2308.00352">arXiv 2308.00352</a> · ICLR 2024 Oral</span>
  </div>
  <p class="mas-card-oneliner">Replace dialogue with structured documents and real test execution.</p>
  <div class="mas-card-bet">Core bet: Schemas plus passing tests equal correct output</div>
  <div>
    <span class="mas-tag">5 roles</span>
    <span class="mas-tag">artifact pub-sub</span>
    <span class="mas-tag">executable feedback</span>
  </div>
  <ul>
    <li>Roles: PM, Architect, Project Manager, Engineer, QA (waterfall)</li>
    <li>No dialogue; agents produce structured documents (PRD, system design, task list, code, tests)</li>
    <li>Pub-sub message pool: agents publish artifacts, subscribe by role to relevant messages</li>
    <li>Executable feedback: unit tests actually run; failures trigger up to 3 retries referencing PRD and design docs</li>
    <li>Results: 85.9 percent Pass@1 on HumanEval, 100 percent task completion, 0.83 human revisions vs ChatDev's 2.5</li>
  </ul>
</div>

MetaGPT is the most ambitious wave-1 paper. Instead of dialogue, the agents produce structured documents. Instead of relying on agents to agree, the framework executes tests. The paper's strongest claim is the structural one: if agents produce artifacts with defined schemas, and those artifacts are validated by execution, you get better coordination than you get from unconstrained dialogue.

I think that claim holds up. But the coordination model is still based on a shared mutable pool, which is the same thing [Riak](https://docs.riak.com/riak/kv/latest/learn/concepts/causal-context/index.html) solved twenty years ago with version vectors. MetaGPT doesn't have that. Agents publish to the pool. Agents subscribe. Nobody tracks causality.

## AutoGen: A Framework, Not a System

<div class="mas-paper-card mas-autogen">
  <div class="mas-card-title">
    <strong>AutoGen: Next-Gen LLM Applications via Multi-Agent Conversation</strong>
    <span class="mas-card-meta"><a href="https://arxiv.org/abs/2308.08155">arXiv 2308.08155</a></span>
  </div>
  <p class="mas-card-oneliner">A configurable framework. Build whatever multi-agent system you want.</p>
  <div class="mas-card-bet">Core bet: Developers will build the right topology</div>
  <div>
    <span class="mas-tag">framework</span>
    <span class="mas-tag">ConversableAgent</span>
    <span class="mas-tag">GroupChat</span>
    <span class="mas-tag">human-in-loop</span>
  </div>
  <ul>
    <li>ConversableAgent base class: any entity that sends and receives messages (LLM, human, tool, code executor)</li>
    <li>Pluggable reply functions via register_reply(); agent behavior is what it does when it gets a message</li>
    <li>GroupChatManager: selects next speaker via LLM role-play prompting or an FSM</li>
    <li>Human-in-the-loop as a dial: per-agent config of ALWAYS, SOMETIMES, or NEVER</li>
    <li>Number one on GAIA at time of publication, roughly 2x performance on the hardest level</li>
  </ul>
</div>

AutoGen is the odd paper in wave-1 because it's not a system, it's a framework. The contribution is that every agent, whether it's an LLM, a human, a tool, or a code executor, speaks the same message protocol. You compose them however you want. The GroupChatManager can pick the next speaker via an LLM or via a finite state machine you define.

AutoGen is more honest than the others about the fact that there's no "right" multi-agent architecture. It doesn't try to tell you what your agents should be. It gives you the plumbing and assumes you know what you're doing. For that reason it's probably aged better than CAMEL or ChatDev.

## What Wave 1 Got Right

Every one of these papers took LLMs out of single-user chat and put them into multi-step coordination tasks. That's a real contribution. Role specialization, structured dialogue, tool use patterns, task decomposition, memory and reflection as first-class primitives. These ideas came out of wave-1 and the field is still using them.

## What Wave 1 Got Wrong

<div class="mas-taxonomy">
  <h4>Shared Assumptions That Didn't Survive</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Failure model</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-bad">Treated as termination, not a system state</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Concurrency control</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-bad">Shared state with no causality tracking</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Evaluation</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-bad">Benchmarks designed for single agents</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Escalation</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-bad">No path when convergence fails</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Topology</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-bad">Fixed at design time</span>
    </div>
  </div>
</div>

Every wave-1 paper treats failure as a termination condition. When ChatDev can't converge, it stops. When MetaGPT's tests fail three times, it stops. When AutoGen hits max_round, it stops. None of these systems have a model for what happens next. This is the gap wave-2 papers would later start trying to fill.

None of the wave-1 papers have concurrency control on their shared state. MetaGPT's message pool grows monotonically and nobody tracks causality. ChatDev discards dialogue at phase boundaries. Generative Agents' memory is per-agent with no sharing. If you had two agents in MetaGPT trying to edit the same file, nothing in the framework would stop them from overwriting each other's work.

And all of them evaluate against benchmarks that were designed for single agents. HumanEval, MBPP, SWE-bench. These benchmarks measure whether the output is correct. They don't measure coordination quality, communication overhead, or recovery behavior. Which are the things that distinguish a multi-agent system from a single agent.

Next post: wave-2 papers, which measure what actually breaks in these systems. With wave-1 architectures running in production, and the agentic coding turn having clarified when MAS isn't the right tool at all, the field started asking why MAS fails when you do use it and how to test that honestly.
