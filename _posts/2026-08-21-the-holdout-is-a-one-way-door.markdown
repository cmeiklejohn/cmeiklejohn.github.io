---
layout: post
title: "The Holdout Is a One-Way Door"
subtitle: "Evidence cannot independently confirm the claim it helped shape."
date: 2026-08-21 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/the-holdout-is-a-one-way-door/
categories: ai research zabriskie agents
---

The first cross-validation scheme in the audio project looked clean at the row level and failed inside each row.

Each row contained a pair of performances, and the pair rows did not overlap. But an individual performance could appear in more than one pair. Performance A could be part of training pair `(A, B)` and then return inside held-out pair `(A, C)`.

This is the useful way to think about a holdout: it is not a folder, dataframe column, or call to `train_test_split`. It is a promise about unavailable information.

The promise says that the evidence used for final evaluation did not help choose the model, features, thresholds, exclusions, aliases, or story being evaluated. If the evidence shaped those decisions, moving its files to a new directory does not make it independent again.

## The unit that has to stay apart

Live audio makes the obvious version of this problem unusually easy to create.

One Goose show may exist as a Bandcamp FLAC, an Archive.org recording, a stream capture, an MP3 conversion, and a set of tracks cut at slightly different boundaries. Two recordings may come from the same source with different metadata. One may include thirty seconds of crowd noise that another omits. Hashes can differ even when much of the underlying performance is the same.

If I split at the file level, the model can train on one copy of a performance and test on another. If I split at the track level, songs from the same show can still cross the boundary and carry venue, mix, crowd, tuning, and recording-source signatures with them. A classifier that appears to recognize a song may partly be recognizing a particular night.

The relevant unit therefore depends on the claim. For the early classifier, at minimum, all representations of a performance had to stay together. For stronger generalization claims, whole shows or chronological blocks had to stay together. Duplicate detection had to operate on more than filenames and exact bytes.

None of this is exotic machine learning. The problem is that an agent can build a formally valid split at the wrong unit, then generate enough downstream work that the unit disappears from attention.

## The cache remembers too

For Notebook 1, the rule is straightforward: derived artifacts inherit the data role of their sources.

If test audio contributes to a normalized feature, the normalization artifact has seen the test set. If a threshold is chosen from a sweep containing test outcomes, the threshold has seen the test set. If a candidate list is ranked using held-out performance, the ordering has seen the test set. A cache does not become neutral because it stores numbers instead of audio.

After the two notebook failures, the project removed 1,120 derived artifacts: 51 jam-only chroma arrays, 153 jam-feature variants, 320 sliding-window variants, 325 per-track JSON summaries, and 271 other feature files.

That number is real, but it needs care. The [surviving audit](https://github.com/cmeiklejohn/zabriskie-audio-research/blob/bfd8b0aa360b7898d7f50fa4aa6f119a9783d4d5/docs/logs/r7-errata-2026-05-17-cache-contamination.html) describes a mixed purge of descendants from both notebooks. It does not cleanly assign every one of the 1,120 files to a single failure lineage.

Notebook 1 descendants carried information contamination where their values were derived from the leaked split. Notebook 2 descendants embodied a measurement premise that had become invalid. They were removed together because both sets were unfit for the work that was about to reuse them, not because both failures were the same kind of leakage.

This became uncomfortable when we examined integrity checks that had passed. One check compared a pinned value in a manifest with a value re-derived from a cached baseline. The values matched, which looked like independent confirmation. They matched because both descended from the same contaminated computation.

The check proved consistency. It did not prove independence.

This distinction is easy to lose in agent-generated work because the system can produce a hash, a manifest, a recomputation script, and a green report around one lineage of evidence. The artifact chain becomes more traceable without becoming less circular.

## The human crosses the boundary

The harder leak is not in a file.

Suppose I evaluate a model on a holdout and learn that So Ready is often confused with Arrow. I inspect both classes, add a rhythmic feature, change a threshold, and run the holdout again. Perhaps the second result improves. The holdout labels never entered gradient descent. They still changed the system.

The same thing happens when I:

- add or remove a feature family after seeing which songs fail;
- alter aliases after inspecting errors;
- exclude unusually short tracks after they reduce a metric;
- change the music gate after seeing preshow failures;
- tune a confidence threshold against the desired headline;
- choose the next hypothesis because a result made it seem promising; or
- stop iterating when the number finally looks acceptable.

The researcher is part of the information path. So is the agent, if it reads the evaluation report and proposes the next candidate.

This is why leakage and HARKing, hypothesizing after the results are known, meet in an iterative system. A hypothesis developed after seeing an outcome may be interesting and worth testing. It is exploration. The same outcome cannot then be presented as independent confirmation of the hypothesis it caused us to formulate.

The problem is not that people learn. Research depends on learning from results. The problem is representing adapted evidence as if the adaptation did not happen.

## A one-way door for a particular claim

Opening a holdout does not poison the audio forever.

This point matters because the strong version of the rule quickly becomes absurd. If I listen to a 2024 Goose show while debugging a song recognizer, the recording can still be useful for studying an unrelated question about crowd-noise removal. It can remain training data, engineering data, a regression fixture, or an example in a user interface. Data does not acquire a universal contamination mark because a human saw it.

The permanent status is scoped to the claim, candidate, and adaptive lineage the outcome influenced.

If I inspect the 2024 show to tune recognizer thresholds, it cannot later serve as sealed confirmation of that recognizer or a descendant that inherited the tuning decision. A new Git branch does not change this. Neither does deleting the result, forgetting the exact score, or asking a different agent to rerun the script. The information has entered the development history.

A genuinely unrelated question can reuse the raw recording when its measurement, candidate, thresholds, exclusions, and analysis policy do not inherit choices from that opened result. Ambiguous cases should be treated as opened engineering work and recorded that way. That policy gives up some rhetorical convenience in exchange for an honest account of what the evidence can support.

## What the current boundary looks like

The rebuilt recognizer uses several overlapping controls because no single file split can carry the promise:

1. Audio is grouped by performance and show before assignment.
2. Exact and near-duplicate controls look across providers, encodings, and track cuts.
3. Chronological evaluations reconstruct only information that would have existed at the prediction date.
4. Data populations have explicit roles: training, opened engineering, prospective field test, or sealed evaluation.
5. Plans, candidate hashes, catalog snapshots, and scoring policies are frozen before a sealed run.
6. Labels remain opaque during prediction and are joined only during scoring.
7. Opening an evaluation creates a permanent record of which candidate and policy saw it.

These rules are not proof that a result is clean. They make the promise inspectable. A future reader can determine which shows were available, how the grouping worked, what version ran, when labels became visible, and whether the result was used to change the next candidate.

This also explains why small personal projects find clean evaluation expensive. Every show held back is a show unavailable for diagnosis. Every opened result reduces the supply of evidence that can independently answer the same question. A researcher with a finite corpus cannot keep creating fresh test sets after each disappointing result.

The practical answer is role separation, not amnesia. Use opened shows aggressively for debugging. Preserve a smaller sealed population for one frozen candidate. Accumulate prospective evidence by running future shows, while recognizing that each show becomes opened for every later version once its outcome has influenced development.

## What a clean holdout does not establish

It is possible to do all of this correctly and still be wrong.

A whole-show split can prevent the model from hearing the same performance twice. Frozen hashes can prove that the candidate did not change. Opaque labels can show that the scorer did not inform prediction. None of those checks establishes that the feature, label, or detector means what the claim says it means.

Notebook 2 had this different failure. Its ninety-second segmentation rule could have been applied to a perfectly isolated holdout and would still have called several minutes of composition a jam. Independence would make the estimate cleaner. It would not make the measurement valid.

A clean holdout still leaves a different question. What if every artifact matches the plan, but the measurement does not correspond to the event named in the claim?
