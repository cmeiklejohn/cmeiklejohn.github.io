---
layout: post
title: "Science at LLM Speed: Three-Pass Editorial Audit"
date: 2026-08-16 01:15:00 -0400
published: false
---

# Audit Scope

This record covers the revision of
`_posts/2026-08-15-science-at-llm-speed.markdown` completed on August 16, 2026.
The structural, evidence, and readability reviews were run separately and in
that order. This is an editorial record, not a public series installment.

# Pass 1: Structural Reader

## Paragraph And Section Map

| Section | Job | Decision |
| --- | --- | --- |
| ICLR opening | Begin with a documented institutional response to nonexistent references and show the human work required to verify them. | Keep. It supplies the concrete event before the thesis. |
| Plausible bibliography | Establish that fabricated and incorrect citations predate the ICLR incident, then show why retrieval helps without making synthesis automatically true. | Keep. The section earns the article's distinction between fluent support and contradicting evidence. |
| More research-like work | Define SetScope, define the autonomous research program used to build it, establish the historical cost of research engineering through Filibuster, connect the project directly to Discovery Loop, and name error propagation inside an autonomous loop. | Reorder. SetScope and the autonomous program must appear before Filibuster so the reader knows what the comparison explains. Split the Discovery Loop paragraph so the company fact and the relevance to SetScope do not compete. |
| Polished analysis | Extend the problem beyond academic papers to ordinary data products and science-shaped public analysis. | Split into its own section. It is adjacent to autonomous research but performs a different argumentative job. |
| Reviewer | Show that adding a model reviewer does not necessarily add independent information, while preserving a constructive example of bounded review assistance. | Keep. It answers the obvious proposed remedy. |
| Machine can find something real | Reject the anti-AI reading through FunSearch and AI Scientist-v2, then define what a useful external check contributes. | Keep. It supplies the positive standard used to judge the author's project. |
| Research loop failed twice | Apply that standard to the two invalid notebooks, restore the original project chronology, and report the bounded August 13 field-test observation. | Rename and keep. The literal heading makes the section's job visible before the chronology begins. |
| What comes next | Give each remaining installment an independent question and promise. | Keep. Seven short previews are justified because this is both the first article and the series entry point. |

## Structural Result

The article now moves from a public failure, to the standard of contradicting
evidence, to the author's attempt to automate research, to two failures of that
loop, and finally to the constraints the rest of the series will build. No
example now depends on SetScope context that has not yet been introduced.

# Pass 2: Evidence Reviewer

## Claim Ledger

| Claim family | Evidence and boundary | Result |
| --- | --- | --- |
| ICLR 2026 nonexistent references | ICLR program-chair retrospective documents the automated screen, false positives, area-chair and program-chair review, at least three human checks per flagged paper, and desk rejection of confirmed cases. | Supported. The article does not infer how the references were produced. |
| Walters and Wilder citation study | Scientific Reports paper reports 42 topics, 636 citations, and unverifiable-work rates of 55 percent for GPT-3.5 and 18 percent for GPT-4. | Supported. The article explicitly limits the result to those models, prompts, and topics. |
| OpenScholar | Nature paper reports retrieval over 45 million open-access papers, citation checking, and improved correctness and citation accuracy in the authors' evaluation. | Supported. The prose says retrieval makes contradiction possible, not that it guarantees truth. |
| SetScope purpose and autonomous workflow | First-person project account plus the preserved project architecture and experiment records. | Supported as project description. The prose states plainly that the product guesses live Goose songs and that the LLM program was meant to choose and run research iterations. |
| Filibuster engineering duration | Author recollection: three months for the OpenTelemetry prototype and another six months adding tests to one production application. | Retain as first-person chronology. The days-or-one-day comparison is explicitly labeled a counterfactual rather than a measured speedup. |
| Discovery Loop | Contemporary reporting identifies Jeff Dean, Sanjay Ghemawat, Oriol Vinyals, and Quoc Le and describes the company's goal as automating experimental loops in science and engineering. | Supported as a reported company goal. The article makes the SetScope comparison in the author's voice and does not claim equal scale or domain. |
| Drosos et al. | CHIWORK paper reports a participatory study with 15 people, uses in information gathering and sensemaking, and verification that participants described as effortful and time-consuming. | Corrected. Removed the unsupported stronger claim that verification required as much work as unaided analysis. |
| LLM-modified peer review | ICML paper estimates 6.5 to 16.9 percent of review text at four conferences was substantially modified or produced by an LLM. | Supported. The article preserves the corpus-level boundary and does not classify individual reviews. |
| Review feedback intervention | Nature Machine Intelligence paper reports a randomized intervention over more than 20,000 ICLR 2025 reviews and more informative revised reviews under blinded evaluation. | Supported. The article treats it as assistance to a defined human process, not replacement of peer review. |
| FunSearch | Nature paper reports executable program search that produced new cap-set constructions and bin-packing heuristics. | Supported. The prose limits the evaluator's force to its specified property. |
| AI Scientist-v2 | The paper reports three workshop submissions, human selection of initial ideas and completed runs, one accepted workshop paper, missing citations, possible train-test overlap, imprecise descriptions, incorrect figure interpretations, and implemented but unused temperature scaling. | Corrected for precision. "Citation inaccuracies" became "missing citations," and the figure-interpretation defect is now named. |
| August 13 SetScope field test | Internal series architecture and run records preserve correct identities emitted at least once for 10 of 12 performances, misses, false switches, capture limitations, and the absence of a viewer-visible delivery receipt. | Supported with boundaries. The prose does not call this a whole-show accuracy estimate or evidence of viewer-visible delivery. |

## Evidence Result

No unsupported external quantitative claim remains in the current revision.
The Drosos sentence and AI Scientist-v2 defect list were corrected. Filibuster
timing remains a first-person recollection, and its proposed LLM speedup remains
explicitly counterfactual. The August 13 observation remains a bounded product
field-test fact rather than a scientific result or an accuracy rate.

# Pass 3: Readability Editor

## Continuous Rendered Read

- The old order interrupted the SetScope explanation with six paragraphs about
  Filibuster before the article had established why the comparison mattered.
  The revision introduces the product and the autonomous program first.
- The Filibuster history now acts as the cost baseline for the autonomous plan.
  Its counterfactual qualification is separated from the concrete chronology so
  the caveat does not swallow the example.
- Discovery Loop now follows the cost comparison and directly states the shared
  goal in plain English. The company description and the SetScope comparison
  are separate paragraphs.
- Extra-academic analysis previously arrived as another turn inside an already
  overloaded section. The new heading gives that argument its own entry point.
- The audio-project heading now states what happens instead of announcing that
  a small lab is entering an abstract historical moment.
- The ending previously compressed permissions, evidence, state, review, and
  claim promotion into one long rhetorical question. It now states the three
  concrete design problems and the failure that motivates the series.
- The seven-part preview remains intentionally explicit. On Part 1 and the
  series landing page, it helps a reader decide whether any later installment
  stands on its own.

## Readability Result

The rendered article can now be read straight through without requiring the
editorial outline to explain why Filibuster, Discovery Loop, fan analysis, or
the two failed notebooks appear where they do. No remaining paragraph was
flagged as a misplaced tangent. The next readability pass should begin from a
fresh rendered read, not from this record.
