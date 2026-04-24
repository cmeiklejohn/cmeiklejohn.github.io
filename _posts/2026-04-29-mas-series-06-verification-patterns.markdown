---
layout: post
title:  "Getting Up to Speed on Multi-Agent Systems, Part 6: Verification Patterns"
date:   2026-04-29 12:00:00 -0000
group: ai
series: mas
categories: ai agents mas-series
---

Every agent system has to answer the same question eventually: how does it know it did the right thing? Wave-1 papers mostly don't. Wave-2 papers get serious about it. Wave-3 papers measure what happens when they don't. And the most interesting verification pattern in the field right now is one that isn't in any paper at all. It's in a commercial product.

{% include mas-series-nav.html current="mas-series-06-verification-patterns" %}

## Three Architectures

Every verification pattern in the field fits into one of three categories. The difference is who checks the work and how.

<div class="mas-taxonomy">
  <h4>Three Verification Architectures</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Self-Verify</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Same agent checks its own work</span>
      <span class="mas-tax-chip">Fast, no coordination overhead</span>
      <span class="mas-tax-chip mas-chip-bad">Blind to its own mistakes</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Separate Verifier</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Different agent or system checks the work</span>
      <span class="mas-tax-chip mas-chip-ok">Catches blind spots the author can't see</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Structural Gate</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Work cannot proceed without passing a gate</span>
      <span class="mas-tax-chip mas-chip-ok">Strongest: not advisory, blocking</span>
    </div>
  </div>
</div>

Wave-1 papers are mostly in the first category. MetaGPT introduces the second with its QA agent. Wave-2 papers and production systems are moving toward the third.

## What Each System Actually Does

<div class="mas-compare-wrap">
<table class="mas-compare">
  <thead>
    <tr><th>System</th><th>Pattern</th><th>Feedback Signal</th><th>Verifier</th><th>Modality Shift</th></tr>
  </thead>
  <tbody>
    <tr><td>CAMEL</td><td>Dialogue consensus</td><td>Partner agrees</td><td>Peer (same capability)</td><td>No (text → text)</td></tr>
    <tr><td>ChatDev</td><td>Code review plus compiler</td><td>Reviewer approval + compile</td><td>Reviewer agent + compiler</td><td>Partial</td></tr>
    <tr><td>MetaGPT</td><td>Unit test execution</td><td>Tests pass or fail</td><td>Test runtime (external)</td><td>Yes (code → result)</td></tr>
    <tr><td>Gen. Agents</td><td>None at runtime</td><td>N/A</td><td>Post-hoc human eval</td><td>N/A</td></tr>
    <tr><td>SWE-agent</td><td>ACI feedback + tests</td><td>Command output + test results</td><td>Environment</td><td>Yes</td></tr>
    <tr><td>Magentic-One</td><td>Orchestrator inner loop</td><td>Progress assessment</td><td>Orchestrator (separate)</td><td>Partial</td></tr>
    <tr class="mas-row-highlight"><td>Cursor Agent</td><td>Visual feedback loop</td><td>Screenshot of rendered UI</td><td>Same agent (self-verify)</td><td>Yes (code → visual)</td></tr>
  </tbody>
</table>
</div>

The last row is the interesting one.

## Cursor's Visual Feedback Loop

Cursor's agent mode has a pattern that isn't in any of the papers. When it's implementing a UI change, it does this:

1. Writes code.
2. Starts the app or preview.
3. Takes a screenshot.
4. Looks at the screenshot.
5. Decides whether the output matches the intent.
6. Fixes or ships.

This is self-verification, which from the table above should be the weakest category. The agent is checking its own work. And MAST data tells us self-verification has a 13.2 percent failure rate in the form of reasoning-action mismatch: the agent thinks it did the right thing but didn't.

What saves Cursor's approach is the modality shift. The agent wrote code (text). The verification happens on a screenshot (pixels). Re-reading your own code in the same modality you wrote it is a weak check. Looking at the rendered output of your code is a much stronger one. You can't make the same mistake twice because you're looking at a different representation of the work.

<div class="mas-callout">
  <div class="mas-callout-label">The modality shift principle</div>
  The stronger the modality shift between the work and the verification, the more bugs you catch. Code to test execution (MetaGPT) is a modality shift. Code to screenshot (Cursor) is a modality shift. Code to executable proof (structural gates) is a modality shift. Re-reading your own code is not. This is why wave-1 papers that rely on dialogue consensus score so poorly on reasoning-action-mismatch failures: text to text is not a real check.
</div>

## The Design Space

Once you've internalized the three architectures and the modality-shift principle, you can think about verification design more clearly.

<div class="mas-taxonomy">
  <h4>Verification Choices Across the Field</h4>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Strongest</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-ok">Structural gate with modality shift</span>
      <span class="mas-tax-chip mas-chip-ok">Separate verifier with modality shift</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Useful</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip">Self-verify with modality shift</span>
      <span class="mas-tax-chip">Separate verifier without modality shift</span>
    </div>
  </div>
  <div class="mas-tax-row">
    <span class="mas-tax-label">Weakest</span>
    <div class="mas-tax-items">
      <span class="mas-tax-chip mas-chip-bad">Self-verify without modality shift (dialogue consensus)</span>
      <span class="mas-tax-chip mas-chip-bad">No verification at runtime</span>
    </div>
  </div>
</div>

Most wave-1 systems are in the bottom category. The agentic coding turn added some modality shift (tests, screenshots). Wave-2 systems and production systems are starting to combine patterns: Cursor uses self-verify with modality shift, and the hybrid opportunity is to layer a separate verifier or structural gate on top of that fast inner loop.

<div class="mas-callout">
  <div class="mas-callout-label">Hybrid opportunity</div>
  Use visual feedback as the dev agent's inner loop for fast iteration, but gate the output with a separate QA agent or executable proof to catch self-verification blind spots. This is a pattern you can build today, and it's more robust than either approach alone.
</div>

## Why Verification Is Undertheorized

The multi-agent papers spend a lot of time on coordination and almost no time on verification. This is backwards. The MAST data from wave 2 shows that verification failures (FC3: premature termination, incomplete verification, incorrect verification) account for 23.5 percent of all observed failures across seven frameworks. That's more than any other single failure category if you group them.

If verification were a first-class concern, you'd expect to see papers titled "How Agents Should Check Their Work" or "Verification Protocols for Multi-Agent Systems." Those papers don't really exist. What we have is a lot of papers that casually mention their verification mechanism in a subsection and move on.

The papers that take verification most seriously are the ones from the agentic coding turn, because they had to. You can't fake SWE-bench. If your tests don't pass, you don't get credit. Interface design, guardrails, and structured feedback all exist in those systems because the benchmark forces them to. When the benchmark is a transcript of agents talking to each other, you don't need real verification. When the benchmark is a working piece of software, you do.

Next post: benchmarks. What the standard evaluations measure, what they miss, and why ChatDev and MetaGPT can report contradictory results on each other without either being obviously wrong.
