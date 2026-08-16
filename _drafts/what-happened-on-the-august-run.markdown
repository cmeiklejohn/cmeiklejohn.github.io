---
layout: post
title: "What Happened on the August Run"
subtitle: "A live product is a history of versions, misses, and repairs, not one score."
published: false
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/what-happened-on-tour/
categories: ai research zabriskie agents
---

> **Draft status, August 15:** As of the frozen ledger snapshot at 2026-08-15 17:41:22 UTC, only the August 13 pilot has a reconciled result. Every remaining result is marked below rather than inferred or invented. This note will be removed after the August 28 cutoff and final reconciliation.

SetScope listens to a live Goose stream and displays its best guess about the current song without receiving the show date or setlist. On August 13, it produced correct identities from a genuinely new show while Goose was still onstage. Only part of the capture remained valid, so the run did not produce a metric-eligible whole-show result.

## What the tool promised

The practical goal is to let someone watching the stream see what is playing without consulting an external setlist.

The product has at least four observable layers. The acoustic models emit candidates. A controller enters or changes a stable song lock. The interface renders that state. An optional adapter can publish the guess into chat or another outward surface. An event at one layer does not establish that the next one occurred.

For this run, the question is therefore not "how accurate was the model?" It is what each instrumented version actually heard, emitted, held, displayed, or published under the conditions of each show.

## The pre-policy baseline

The August 13 San Diego pilot establishes the baseline from which later versions changed. The runtime emitted correct identities at least once for ten of twelve performances, made three false switches, and missed two performances. The post-show acoustic diagnostic found eleven of twelve, showing that controller state was materially worse than candidate discovery.

The two misses also exposed different product limits. Capsized was a first-time-played cover outside the closed catalog. Correct 726 evidence appeared during the encore, but the controller retained an already incorrect state. Set 1 then failed its capture-timebase check, preventing exact latency measurement for that portion of the show.

These observations motivated unknown handling, controller work, and stronger capture accounting. They remain pre-policy history and do not enter the August 15-28 aggregate.

The frozen ledger covers eleven Goose performances from August 13 through August 28:

- **August 13, Cal Coast Credit Union Amphitheater:** pre-policy pilot; partially valid shadow run; descriptive only.
- **August 14, Greek Theatre:** pre-policy ledger row; reconciliation pending; excluded from the post-policy aggregate.
- **August 15, Frost Amphitheater:** post-policy; pending.
- **August 16, Grand Theatre at Grand Sierra Resort:** post-policy; pending.
- **August 18, Commodore Ballroom:** post-policy; pending.
- **August 19, WAMU Theater:** post-policy; pending.
- **August 21, Hayden Homes Amphitheater:** post-policy; pending.
- **August 22, Hayden Homes Amphitheater:** post-policy; pending.
- **August 24, Kettlehouse Amphitheater:** post-policy; pending.
- **August 27, Red Rocks Amphitheatre:** post-policy; pending.
- **August 28, Red Rocks Amphitheatre:** post-policy; pending.

As of the 2026-08-15 17:41:22 UTC snapshot, only August 13 has a reconciled source record. It used model version 0521 in blind shadow mode, contains one invalid and one valid capture session, lacks a preserved UI-observation receipt, and did not exercise publication. The August 14 row remains deliberately incomplete rather than reconstructed from memory. No reconciled result appears in that snapshot for the remaining rows.

The source system also labels thirteen earlier June and July performances as Summer Tour 2026. They are outside this retrospective. SetScope live testing began in August, and expanding the denominator backward would create a tour claim about shows the product never attempted to hear.

## The record we kept

Each scheduled show receives a row even when no usable result exists. The ledger records whether a stream was available, whether an attempt began, which candidate and policy ran, whether capture remained valid, and which artifacts survive.

For a session to be metric-eligible, the frozen policy requires preserved music-gate changes, candidate emissions, stable lock entries and clears, UI observations, and any publication attempt, acknowledgment, visible receipt, correction, failure, or duplicate. It also requires source frames, finalized audio, and recognizer-decoded samples to reconcile before the session contributes to time-based metrics. A future run still has to demonstrate that the runtime and surrounding observers produced those artifacts.

Truth is created after the show from the final setlist and captured audio. For the post-policy period, the first truth artifact is assembled without access to SetScope predictions. It records plausible onset and transition intervals rather than pretending a segue has one naturally exact breakpoint. If the reconciler sees the predictions first, that truth remains useful for diagnosis but does not enter the primary post-policy metrics.

This produces more missing values than reconstructing everything from memory would. That is deliberate. A lock without a UI receipt remains an internal lock. An acknowledgment without an independent fetch remains an acknowledged attempt. A show with invalid capture remains an operational failure even when some guesses look correct.

## Two pilots, then a frozen collection policy

August 13 and 14 occurred before the retrospective policy was frozen. The documented August 13 pilot remains in the narrative because it changed the product. August 14 remains an unreconciled ledger row and contributes no result. Neither is pooled into the August 15-28 metrics.

Policy v2 was frozen at 2026-08-15 17:41:22 UTC, followed by the v3 addendum at 17:54:34 UTC, before the Stanford show. Together they fix the population, event meanings, capture tolerances, truth process, denominators, scoring derivations, and rules for version stratification. SetScope can change during the run, but a later version does not rewrite an earlier show's history.

This matters because the system will almost certainly improve in response to specific failures. A controller revised after San Diego is confounded with every later song, venue, stream, and operating condition. Comparing its percentage with the earlier version is useful product history, not a controlled estimate of the code change's causal effect.

The deployed-version record currently contains one confirmed assignment: version 0521, the adaptive gate-window controller, ran in blind shadow mode on August 13.

Later engineering candidates added a [short-budget boundary path](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0523-boundary-short-budget-fast-path-result.md), an [unknown guard for initial locks](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0530-unknown-guard-initial-lock-result.md), and [continuous recovery from unknown](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0532-continuous-unknown-recovery-result.md). Those artifacts establish engineering succession, not deployment. Until a show row names its installed artifact and policy, the retrospective cannot assign that show to the latest available candidate.

The August 14 Los Angeles pilot still requires reconciliation. Until the record establishes whether a run occurred, which version ran, whether capture was valid, and what artifacts survive, it contributes no recognition result.

## Availability before accuracy

The first August-run report describes operation, not recognition:

- **Pre-policy pilots, two scheduled:** one stream and one attempt confirmed; the other remains pending. No fully valid run is confirmed. One partially valid run is confirmed. Publication was not exercised in the confirmed run.
- **Post-policy collection, nine scheduled:** stream, attempt, validity, and publication states remain pending in the frozen snapshot.

After August 28, every pending cell will be replaced from the ledger. A stream that is unavailable is not a classifier miss; it is an availability limit outside SetScope's control that still prevents the viewer-facing service from operating. An operator who cannot start the tool records a failure of this deployment workflow, not automatically a defect in the classifier or application code. Capture failures and incomplete logs have their own states. None may be converted into a recognition result.

A partially valid show remains partially valid even if one set supplies usable song opportunities. The valid opportunities may contribute to version-specific identity results when their entire interval falls inside a session declared valid before truth is inspected. They do not convert the show into a successful run.

## How often and how quickly it was correct

The identity results will report counts before percentages:

**[RESULTS PENDING: For August 15-28, insert metric-eligible in-catalog song opportunities; first stable locks emitted; correct first locks; correct locks by 60 and 90 admitted music seconds; opportunities with no correct lock; and out-of-vocabulary episodes assigned a catalog identity. Stratify every result by exact product version.]**

Latency begins at an interval, not an exact point, when a song segues from the previous performance. The primary clock is admitted music time, with captured time reported separately. A song that never locks remains censored in the denominator rather than receiving the slowest observed latency.

**[RESULTS PENDING: Report median and distribution of time to first correct candidate and first correct stable lock, including onset uncertainty. Compare with external setlist observations only for songs where both observer receipts exist and host clocks are healthy.]**

An external comparison measures when Zabriskie's observer first received a matching external setlist value. It does not reveal when an external editor recognized the song or when another fan first saw it. The event definitions have to match before a timing difference means anything.

## The failures the aggregate would hide

Every miss receives a mechanism category before examples become a story:

- capture or timebase invalidity;
- music-gate admission during nonmusic or rejection during music;
- no correct acoustic candidate;
- correct candidate blocked by the controller;
- false switch inside a continuing performance;
- delayed or ambiguous segue handling;
- out-of-vocabulary or debut material;
- missing catalog or alias coverage;
- UI state without an observation receipt; or
- attempted publication without confirmed delivery.

**[RESULTS PENDING: Insert failure counts by category and version. Retain unresolved cases as unresolved rather than assigning the most flattering subsystem.]**

This is also where the songs that seemed as though they should be easy return to the story. Thatch and Big Modern have distinctive openings to a listener familiar with Goose. In San Diego, their opening evidence arrived near the earliest point the architecture could produce it, but the later state behavior was poor. That suggests at least two questions: whether a short-window opening model reduces first-lock latency, and whether the continuous controller can avoid treating later jam sections as new songs.

**[RESULTS PENDING: After complete denominators are visible, compare Thatch, Big Modern, and other repeated opportunities across versions. Use them to explain mechanisms, not to replace the aggregate.]**

## What reached the screen and chat

The live console makes the internal process visible, but a persisted lock does not prove that a viewer saw it. The policy counts a UI-observed event only when the rendered state is polled or observed with the same identity and timestamp. Whether each post-policy run supplies that receipt remains part of the result.

Publication has still more required states. A conforming record distinguishes an attempted post, its acknowledgment and durable identifier, and an independent fetch establishing visibility. Corrections and duplicates are separate events. Shadow-mode guesses never count as delivered posts.

**[RESULTS PENDING: Report UI-observed locks, publication attempts, acknowledgments, independently visible posts, failures, corrections, and duplicates by run. If publication is never exercised, say so plainly.]**

Whether the display helped anyone is a different question. I can report my own experience and attributed comments from people who used it. The telemetry can show that a correct title appeared before an external observation. It cannot show that a viewer noticed, trusted, or benefited from the title without direct evidence from the viewer.

## What the August run changed

**[RESULTS PENDING: Write the product verdict after the August 28 cutoff, manifest reconciliation, blind truth freeze, and deterministic scoring report. State which version-specific changes remain, which regressions appeared, and which open questions move into the next run.]**

There are several honest endings available.

The product may become accurate when it operates but remain unreliable to start and capture. It may recognize openings quickly and still switch incorrectly during jams. It may become conservative enough to preserve precision while abstaining too often to be useful. It may fail to improve. The run may also be operationally compromised enough that no broad recognition summary is eligible.

All of those outcomes are publishable because the purpose of the ledger is to preserve what happened, not to manufacture the launch story we hoped to tell.

The project began with a simple wish: let a computer listen to a Goose show and tell me the song. The difficult part turned out not to be producing a plausible title. It was building a loop that could keep a plausible title, a correct title, a stable displayed title, and a defensible claim about the product from collapsing into the same thing.

On August 28, the record will decide how far we got.
