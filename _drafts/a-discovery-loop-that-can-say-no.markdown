---
layout: post
title: "A Discovery Loop That Can Say No"
subtitle: "A strong result should authorize only the next step its evidence can support."
date: 2026-08-30 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
published: false
permalink: /series/the-machine-in-the-lab/what-we-built-instead/
categories: ai research zabriskie agents
---

The best SetScope result so far arrived on 75 historical Goose shows we had set aside for engineering.

By ninety seconds of music, the new controller had identified the opening song correctly on 65 shows and made no wrong opening call. It initially stayed quiet on the other ten, kept listening, and eventually recovered the correct current song on all ten.

I could have summarized that as 100 percent across 75 shows. That would have hidden almost everything important about the result.

The first number was 65 correct decisions and ten abstentions, or 86.67 percent coverage at ninety seconds with no wrong calls. The later recoveries happened under a different rule and at different times. More importantly, the controller had been designed by inspecting earlier failures on these same 75 shows. The result was strong evidence that the engineering change worked on the material that shaped it. It was not independent evidence about what would happen next.

The project had another 38 shows that remained unopened. We did not run them. Setlist posting remained off. The result authorized one action: install the controller and take it back through the browser.

That decision is what I mean by a discovery loop that can say no. The name deliberately echoes the company described in Part 1: the ambition is the same kind of automated experimental cycle, applied here to a much smaller problem. There is no affiliation between the projects.

## Why the loop needed a boundary

SetScope listens to a live Goose stream and guesses which song is playing. I used an LLM agent to improve it: propose an experiment, write the code, run it, inspect the output, and use the result to choose what to try next.

That loop made a solo project possible at a scale I could not have reached by hand. It also produced the failures in this series.

The first research notebook expanded into too many questions without preserving evidence capable of independently evaluating the path it took. The second wrote down a rule against fixed ninety-second jam boundaries, then reintroduced the same boundaries in its implementation. We responded with written plans and additional reviewers. The next Phish experiment passed its publication gates, but no prepublication review compared what was audible at both the listener and detector timestamps.

Each failure followed the same acceleration. The agent made a plausible choice, generated a result from it, and carried the result into the next experiment before the choice returned to view.

More instructions inside the conversation did not solve that problem. The important state had to survive across agents and sessions: which shows had been opened, which failures had influenced the controller, which version actually ran, and what the resulting evidence was still allowed to support.

## What we built

There is no single research-control application enforcing all of this. The current system is a workflow made from written plans, hashes, separate execution and scoring scripts, saved results, human approvals, and promotion rules.

Three changes matter most.

Every show has a recorded job. Some fit the acoustic models. Opened shows are available for debugging and comparison. A smaller group remains hidden for one final run. Once a result from a show changes the system, a new filename or branch cannot make that show independent again.

Each experiment freezes the version, inputs, controller policy, and scoring rule before execution. Predictions are saved before labels are joined. Failed capture, an abstention, and a negative result remain in the record instead of disappearing behind the next successful run.

Finally, the plan says what a result may change. A classifier score can justify integration work. An offline whole-show replay can justify a browser rehearsal. A browser rehearsal can justify a shadow run with a live stream. None of those steps automatically enables public setlist posting.

People and agents still have to honor the process. Its value is that the next action can be checked against a durable record instead of reconstructed from chat history.

## The live show that became engineering data

The August 13 Goose show in San Diego supplied the first genuinely prospective evidence. Its audio did not exist while SetScope was being built. During the show, the runtime emitted the correct current song at least once for 10 of 12 performances. A correct title appeared in the internal acoustic evidence for 11 of 12.

It also emitted three false switches and missed two songs. Set 1 failed its capture-timebase check, which invalidated exact latency from that portion. The truth boundaries available after the show were approximate. The surviving audit does not establish what was rendered in the UI or delivered through the publication path.

The show answered the smallest product question: SetScope could produce useful blind song identifications from new live audio. It also showed exactly where the product was weak. The controller could mistake a long jam for another song, wait minutes before accepting a correct transition, and remain stuck on the wrong title after the acoustic models recovered.

Once we inspected those failures and used them to design later versions, August 13 became engineering data. It could teach the agent what to fix. A later controller needed different evidence to show that the fix generalized.

## What happened on the 75 shows

The larger controller experiment began with 113 eligible, verified shows from 2021 through 2025. A frozen hash split assigned 75 to engineering and 38 to final confirmation before song titles or outcomes were examined.

The agent used the 75 opened shows to develop how SetScope handled uncertainty. The selected version, 0532, began conservatively: 65 correct opening locks by ninety music seconds and ten abstentions. It made no premusic opening or recovery locks and no opening or recovery calls on material outside its song vocabulary.

While the displayed state remained unknown, the controller continued to evaluate two independent model families. It recovered six ordinary Goose openers after collecting more evidence. Four other shows began with soundcheck jams or songs outside the catalog; the controller waited until it had sustained evidence for a known song actually being played. All ten recoveries matched the current in-catalog song.

The same replay produced 99.33 percent correct current-song choices among the switches whose truth could be scored. That number is useful for comparing engineering versions. It does not include Chrome, the macOS audio route, the interface, or public posting.

The [saved result](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0532-continuous-unknown-recovery-result.md) names the only promotion it earned: package that exact controller into SetScope with posting disabled, then run a controlled browser rehearsal.

We packaged the controller with posting disabled. The browser rehearsal remained the next test, and the 38 final shows stayed closed.

## The evidence we have not spent

The unopened shows are useful because the controller has not learned from their outcomes. Before they can be scored, the local model artifacts, catalog, controller, inputs, and scoring script have to be frozen. Once the results are visible, those shows become ordinary engineering material for every later version influenced by them.

A good result there would answer one offline whole-show question for one fixed version. The browser would still need to prove that it received continuous audio. The interface and publication path would still need their own receipts.

This is expensive in a small project. Every hidden show is one less show available for diagnosis. The alternative is a test set that quietly becomes a development set while its headline stays unchanged.

The workflow does not guarantee good science. A frozen experiment can still measure the wrong thing, and approvals can decay into ceremony. What it gives the autonomous program is a memory it cannot rewrite without leaving evidence: which inputs were available, what failed, which version ran, and where the result had to stop.

The next browser rehearsal is the immediate test. It has to preserve the abstention and recovery behavior while adding Chrome, CoreAudio, the controller, and the rendered state back into the path. If that works, the 38 shows will still be waiting for one final offline question. If it does not, they stay closed while the failure becomes the next engineering case.
