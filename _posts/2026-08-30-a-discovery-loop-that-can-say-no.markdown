---
layout: post
title: "A Discovery Loop That Can Say No"
subtitle: "Bound the agent, preserve the failures, and let the evidence limit the claim."
date: 2026-08-30 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/what-we-built-instead/
categories: ai research zabriskie agents
---

SetScope began as a viewer-facing song guesser: listen to a live Goose stream and identify the current song without receiving the date or setlist. The audio corpus pulled the project into a harder detour about measuring improvisation. Two notebooks from that detour were deleted after hidden setup decisions invalidated them.

The first research-discipline document was our response. It had ten rules, named roles, explicit approvals, and an advisor agent with authority to block work. We used it for Dispatch 002, an experiment that compared listener-marked returns to composition with an acoustic changepoint detector. Every required gate passed. The general interpretation was still unsupported.

The failure was useful because it changed what I thought the system needed to control. A long prompt could tell an agent to be careful. A checklist could require it to produce more artifacts. Neither represented the state of the research strongly enough to determine which action was allowed next.

The project needed a discovery loop that could say no.

## The loop is the system

This project used an autonomous research program to improve SetScope. The program could propose an experiment, implement it, run it, evaluate the output, and choose what to try next with limited human intervention. That is the same basic ambition behind the emerging work on automated discovery loops in science and engineering. The important unit is no longer one prompt or model response. It is the loop that carries state from one iteration into the next.

That state includes more than code and results. It includes which labels have been seen, which thresholds were adjusted, which failures changed the design, what the evaluator actually measured, which runtime path was exercised, and what claim the project is now tempted to make.

The lost notebooks showed that information roles and methodological decisions had to survive across iterations. Dispatch 002 showed that an internally consistent evaluator could still measure the wrong thing. The browser incident showed that a correct offline experiment did not exercise the deployed audio path. In every case, an agent could produce a locally correct next step from a globally invalid state.

The correction was to move the critical state outside the model's conversational memory and make it durable.

## From instructions to state

An instruction such as "do not use the test set" sounds clear until the project has five copies of a performance, three feature caches, a threshold selected after an opened result, and an agent that did not participate in the original decision. The instruction has to become data the system can inspect.

The current process represents at least these things explicitly:

- a question and the exact claim it is capable of supporting;
- the role of every input population;
- a candidate model, feature set, catalog, and policy identified by hashes;
- an analysis and scoring specification frozen before outcomes are visible;
- the boundary between prediction, scoring, diagnosis, and promotion;
- immutable result artifacts, including invalid and negative outcomes;
- the evidence scope of a run, from component crop to live runtime to observed UI or publication; and
- the actions the system is permitted to take automatically.

This is less elegant than telling an agent to "follow scientific best practices." It is also inspectable. A candidate cannot quietly change after the run starts if the installed artifact has a recorded digest. A shadow event cannot become a delivered chat post if publication is disabled and receipts are required. A show used for diagnosis cannot later be described as sealed confirmation without contradicting its recorded role.

The rules still depend on people honoring them. State does not eliminate discretion. It makes the exercise of discretion visible.

## A bounded experiment

The useful unit of autonomy became a bounded experiment rather than an open-ended request to improve the model.

Before execution, the experiment records:

1. the question;
2. the allowed inputs;
3. the candidate and every configurable policy;
4. the metric and selection rule;
5. the compute or iteration budget;
6. the outcome that would falsify or stop the treatment; and
7. the downstream action the result may authorize.

Planning, execution, and scoring are separate states. The planner can inspect opened engineering evidence. The executor receives frozen inputs and does not change the candidate. The scorer joins outcomes only after immutable predictions exist. Promotion requires evidence appropriate to the claim being promoted.

This last point is where many agent workflows become vague. A result may authorize one action without supporting the next. A replay on 75 opened shows can justify another browser rehearsal while remaining useless as independent confirmation. A prospective run may demonstrate correct identities on future audio without supplying the capture record needed for a whole-show latency metric. Controller, interface, and publication events require their own evidence. Each promotion crosses another boundary.

## Five coordinates for evidence

We eventually began classifying evidence on separate axes instead of assigning it one adjective such as "clean" or "live."

**Information access.** Was the outcome unavailable when the candidate was frozen, sealed until scoring, or already opened and available for adaptation?

**Temporal relation.** Was the evaluation retrospective on existing audio or prospective on a future event?

**System scope.** Did it exercise an isolated model, an integrated offline replay, the live runtime through controller state, the rendered interface, or an outward publication path?

**Publication action.** Was the system operating in shadow mode, attempting a post, receiving an acknowledgment, or independently observed as visible?

**Metric eligibility.** Did capture, truth, timing, and protocol requirements pass for the metric being reported?

These coordinates prevent one strong property from standing in for the others. The August 13 run was prospective and its audio was unavailable when the candidate was built. It exercised the live path through saved controller outputs. It did not preserve complete proof of every frozen pre-music condition, its first set failed the timebase rule, its truth boundaries were reconstructed after the show, and no separate UI-observation or outward-publication receipt survives.

So it establishes a valuable fact: the system emitted correct blind song identities during a genuinely new show. It does not establish a formal whole-show accuracy rate or successful automatic publication.

That sentence is more cumbersome than "the live test passed." It is also much harder for the next agent to misunderstand.

## Preserving failure as an output

The project originally treated failed runs as interruptions on the way to the result. That made it easy to rerun something slightly differently and report only the version that completed.

Now invalidation is a terminal result. A capture with missing samples retains its events but cannot contribute to metrics requiring continuous time. A candidate that emits no lock remains in the coverage denominator. An unknown song is not silently removed because the closed catalog could not name it. A planned comparison that lacks synchronized clocks is reported as missing rather than assigned a tie.

Negative experiments remain useful too. One rebuilt question searched 263 curated jamchart entries for a frozen set of descriptive terms, found none, and halted before loading audio. Preserving the halt prevented the agent from silently broadening the phrase list until something matched.

A protocol that produces only impeccable refusals is not a research program. The halt matters because it keeps an unobserved change from turning a failed question into an apparently successful one.

## The larger opened engineering result

After the August 13 show, we used opened shows to improve the temporal controller and its handling of abstention. The [corpus protocol](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0494-corpus-scale-evaluation-preregistered.md) had identified 113 eligible, manifest-verified shows from 2021 through 2025 whose dates appeared in neither fitting nor prior evaluation. A frozen within-year hash split assigned 75 to engineering and 38 to sealed confirmation without reading song titles or outcomes.

The selected controller candidate, version 0532, was designed after reviewing earlier failures, including the August 13 show and version 0531. The 75 shows were opened development material. The result explicitly classified itself as engineering evidence and disabled setlist writes. The replay did not exercise Chrome, CoreAudio, the interface, or public posting, and it was not independent confirmation.

Under the frozen replay rule, the selected treatment produced 65 correct initial locks by ninety admitted music seconds and abstained on the remaining ten shows. It emitted no wrong initial locks, which yields 100 percent selective initial precision and 86.67 percent correct coverage across all 75 shows. Median admitted music time to a correct initial lock was sixty seconds. It produced no premusic locks and no initial or recovery locks on out-of-vocabulary material in that replay.

The [complete replay result](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0532-continuous-unknown-recovery-result.md) also retained the observation-level accounting:

| Observation class | Count |
| --- | ---: |
| All ten-second observations | 65,085 |
| Scoreable in-catalog observations | 61,446 |
| Correct state | 57,079 |
| Incorrect state | 3,415 |
| Unlocked | 952 |
| Outside the scoreable population | 3,639 |

That is 92.89 percent correct state across scoreable observation time. Across 733 eligible in-vocabulary archive boundaries, the system detected 632 by ninety admitted music seconds, or 86.22 percent. Different slices of the replay produce different transition rates, which is precisely why the artifact retains the raw counts and scoring scope rather than one summary percentage.

The larger sample does not replace the prospective field test. It answers a different question: under a frozen replay rule on these opened shows, did this treatment meet the engineering gates required to advance? It did, and the authorized next action is another complete-product test, not a universal reliability claim.

## The boundary the agent cannot cross

The other 38 shows from the same eligible population remain sealed under a [one-time final-candidate protocol](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0512-sealed-confirmation-execution-protocol.md). The within-year hash assignment prevents outcome- or song-based selection and preserves the year distribution; it does not make the 38 shows representative of every venue, source, year, or song in the wider Goose archive. The candidate, model artifacts, catalog, policy, inputs, scorer, and hashes must be frozen before that population is opened. Once scored, the set becomes opened forever for that candidate lineage.

This is intentionally expensive. The point of a sealed evaluation is not to create a renewable source of encouraging numbers. It is to answer one question once without allowing the answer to influence the system that produced it.

Even a good sealed result would remain one level in the product record. It could establish independent performance on the frozen catalog evaluation without showing that the browser path, interface, or automatic-publication path worked. Evidence at one level may authorize the next test without establishing the levels above it. Keeping those steps separate is the central job of the control plane.

## What we built instead

I do not think the resulting process is a universal method for AI-assisted research. It grew around specific failures in one self-directed project, and it has costs we have not measured.

The gates may consume enough attention to erase the speedup. They can become paperwork. Approvals can become ceremonial. A protocol can preserve a perfect record of a bad measurement. We do not yet have an opportunity ledger showing how often each control catches a problem versus merely delaying work.

What the system can do is narrower:

- make data roles and openings durable;
- freeze a bounded experiment before outcomes arrive;
- require an evaluator with access to evidence outside the generated artifact chain;
- exercise the path the product will actually run;
- preserve invalid and negative outcomes;
- attach every result to the claims and actions it may support; and
- block automatic promotion when the required evidence does not exist.

That is enough to change the behavior of the loop. The agent can still propose the next experiment and implement most of it. It can no longer convert a promising chart into a sealed result, an internal state into a delivered product event, or a successful engineering replay into authorization to post publicly without leaving a contradiction in the record.

The method will be tested by inconvenience, not prose. Goose is playing eleven shows from August 13 through August 28. SetScope will change during the run. Streams will fail, songs will segue, and new covers may fall outside the catalog. The record has to retain which version heard each show, including the nights when no valid result exists. That is where the protocol becomes real.
