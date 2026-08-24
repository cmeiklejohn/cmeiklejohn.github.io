---
layout: post
title: "The Safeguards Have to Run: Three-Pass Editorial Audit"
published: false
---

# Audit Scope

This record covers `_posts/2026-08-18-two-notebooks-lost.markdown`. It replaces the earlier audit, whose passing verdict was invalidated when an author read found undefined terminology, a buried takeaway, and chronology that did not match the retained repository history.

Four adversarial readers examined the post independently for standalone clarity, structural throughline, factual support, and voice. Findings were checked against the retained history in `zabriskie-audio-research`, including the Notebook 1 deletion commit, the Notebook 2 methodology and implementation, and the later cache-contamination errata.

# Pass 1: Find the Story

The previous post treated the pair-level training leak, the ninety-second detector, the reserved test data, and the incomplete purge as separate notebook anecdotes. Its central sentence said that a missing rule could propagate through later work, but the notebook sections did not build that argument.

The first rewrite made enforcement the throughline: a rule written in a plan or data file is not a safeguard unless it controls execution or stops the process when violated. The pivot from song identification to improvisation remained only to explain how promising intermediate results changed the question the autonomous program pursued.

The pass failed. It repeated the thesis too often, placed two unrelated failures under one section heading, and claimed that leftover Notebook 1 cache data caused Notebook 2 to reuse the ninety-second segmentation.

**Result:** Fail, followed by structural rewrite.

# Pass 2: Reconstruct the Record

The factual reader found that the compressed chronology was wrong. Notebook 2 began on April 30 while Notebook 1 remained available for reference. Notebook 1 was removed from the main project folder on May 4, and Notebook 2 was stopped on May 6. The later 1,120-file cache purge occurred on May 17, after both notebooks had been removed, and documented contamination entering a subsequent notebook rather than causing Notebook 2's first implementation.

The revision separated the documented failures:

- Notebook 1 used pair-random cross-validation, allowing one performance to appear in both training and testing through different pairs.
- A 44-case detector holdout contained 38 analyzable performances and rejected the fixed-region detector.
- Notebook 2's written method rejected blanket ninety-second boundaries, but its implementation independently inherited that default and propagated measurements cut at those boundaries.
- Four of five reserved song groups, accounting for 22 of 25 performances at lock time, were accessed during development.
- After both notebooks were removed from the project folder, a subsequent notebook loaded 131 generated files from the mixed cache. Purging them changed one numerical cutoff slightly without materially changing the affected results.

The post no longer treats the cache as the cause of Notebook 2's recurrence or claims that every failure had the same immediate mechanism. Their shared lesson is at the control layer: each named boundary could be crossed without stopping the run.

**Result:** Fail, followed by factual and chronological revision.

# Pass 3: Standalone Read

The final pass removed remaining research shorthand, defined a notebook and the autonomous division of labor, explained the A/B–A/C pair leak, defined the three data roles in plain language, and replaced technical cache terminology with a description of generated measurements stored outside the project folder.

The structural reader confirmed one continuous sequence: concrete leak; autonomous-process definition; SetScope origin; detour into improvisation; Notebook 1 failures; Notebook 2 recurrence; later purge failure; executable safeguards; return to SetScope.

The author read then removed an unnecessary explanation of how the source data changed. The post now keeps only the causal point that matters to its argument: apparent progress made by the autonomous program kept the improvisation detour running.

The evidence reader required two last corrections: the detector treated changes within composed passages as evidence rather than directly labeling passages, and later analyses reused generated measurements rather than generated audio clips. Both were corrected.

The voice reader removed inflated stakes, duplicated aphorisms, and any implication that the solution is for the author to reproduce every autonomous step manually. The post now assigns the author responsibility for defining and approving constraints while assigning enforcement, interruption, and provenance tracking to the system.

**Result:** Pass.

# Final Claim

The contaminated split, inherited ninety-second default, opened final-test data, and incomplete purge are different mistakes. Together they show that natural-language instructions do not constrain an autonomous research process. The safeguards must execute as part of the system and block or surface violations before later work inherits them.

# Voice-Model Pass

After the three editorial gates passed, the post was checked against `scripts/voice/voice-guide.md`, the canonical model derived from Christopher's pre-2026 writing. Three incident-led technical posts were used as mode-matched references: the 2021 Partisan process-identifier bug, the 2023 distinction between testing for resilience bugs and testing resilience, and the 2023 essay on building research software that transfers into practice.

The pass found that the post's reasoning and structure matched the model, but its surface cadence was too formal. It relied heavily on expanded constructions such as "did not" and moved between sections through declarations rather than questions. The revision added contractions, made the A/B-A/C contradiction an explicit question, and turned the final synthesis and Part 3 handoff into questions that arise from the mechanics. It did not alter the evidence, chronology, or claim boundaries.

The final prose contains 1,274 words with a mean sentence length of 16.24 words, a median of 15, and a mean paragraph length of 47.19 words. The paragraph cadence closely matches the pre-2026 corpus; the sentences remain somewhat shorter because this post explains several data-boundary failures to a general reader. An anti-imitation comparison found no shared eight-word passages with the three exemplars.

The structural sequence remained unchanged after the voice pass. The evidence claims were rechecked because contractions and question transitions did not alter their scope. A continuous reread found no new undefined terms, duplicated conclusions, or abrupt transitions. The ending now creates forward pressure by asking when a live show stops being independent evidence after the program uses it to choose its next action.

**Result:** Pass.
