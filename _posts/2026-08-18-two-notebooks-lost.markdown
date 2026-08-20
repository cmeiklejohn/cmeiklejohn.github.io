---
layout: post
title: "Two Notebooks Lost"
subtitle: "The code was mostly right. The experiments were already lost."
date: 2026-08-18 08:00:00 -0400
published: false
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/two-notebooks-lost-series/
categories: ai research zabriskie agents
---

I deleted two notebooks of audio research during this project, but not at once. Notebook 1 failed after four days on April 30. I wrote down what had gone wrong and started over that day. Notebook 2 failed for a different reason on May 6, less than a week later.

Between them, the notebooks represented a little more than a week of nearly continuous agent work. The second had already produced two long blog posts that I put online. Three people read them. I had also asked friends to complete a listening study in their free time. The study was already in front of two people. One of them, my regular Phish-going friend Patrick, had spent an evening on it. I was planning to talk about the results at a music festival beginning two days later.

All of it came down because each notebook contained a silent setup decision that invalidated what followed. The decisions were different. One crossed an information boundary. The other changed what the experiment was measuring. In both cases, most of the code worked exactly as written.

This is a postmortem about what an LLM agent did, but putting the work online was still my decision. I asked the agent to move quickly, then put the resulting work online because its reports presented the stated requirements as satisfied. I did not independently reconstruct every split or trace each setup decision back to the raw data. Doing that after every run would have defeated the reason for delegating the research process. The failure was that the system could violate an explicit method without blocking the work or disclosing the violation, then present everything downstream as finished.

I have a PhD and have spent much of my career doing systems research. That matters because these were not rules I did not know to write. My background helped me specify the constraints and recognize the failures once they became visible. It could not make an undisclosed violation visible inside a large volume of competent-looking work.

## The project before the notebooks

The original idea was not to study improvisation. It was to build an automatic setlist tool for [Zabriskie](https://zabriskie.app).

Goose streams many of its shows live. A viewer can usually learn what song is playing by waiting for someone to update a setlist, checking a fan site, or recognizing it. I wanted the computer to listen to the stream and say, in real time, what the band was playing. I had a growing archive of labeled shows from Bandcamp and public archives. Track boundaries and titles supplied a large, imperfect, but unusually useful supervised corpus.

The first practical question was direct: given thirty or sixty seconds of an in-progress performance, can a system identify the song without knowing the date or consulting the setlist?

Then the corpus started suggesting more ambitious questions. Jam-band performances contain repeated compositions, long improvisations, segues, returns to earlier themes, and community labels for musical events whose boundaries are sometimes disputed even by careful listeners. The apparent power of the audio features made it tempting to ask whether those events could be measured too. The setlist project became a broader audio-research program. I started working with Phish because its archives and community annotations made the harder questions look unusually tractable.

This detour mattered. Automatic song identification asks whether a predicted title matches the song that is actually playing. The jam questions asked where improvisation begins, where it ends, and what an audible return to composed material means. The first problem is difficult engineering. The second also contains a measurement problem.

## Notebook 1

The first notebook evaluated pairs of performances using cross-validation. The held-out rows were supposed to remain untouched until the end of the analysis.

The agent built a per-performance feature pipeline, trained a classifier, and produced early evaluation results. The numbers were good. They were good enough that I became suspicious and asked for an audit of the split.

The pair rows did not overlap, but the performances inside them did. Performance A could appear in a training pair `(A, B)` and then appear again in a held-out pair `(A, C)`. The model was being evaluated on individual performances it had already encountered through other pairs.

Once I knew that, there was no honest adjustment to the reported number. The leak had influenced the feature work, model choices, and interpretation of early results. Making a cleaner split after seeing those outcomes would produce a useful engineering experiment, but it would not restore the independence the original test was supposed to provide.

I withdrew Notebook 1 after four days, wrote a short list of things not to do again, and started over. Its files remained in the working tree until May 4.

The larger lesson was not that every split required my personal inspection. It was that a split the system could violate without stopping or reporting the violation had not been established at all.

## Notebook 2

Notebook 2 looked much more disciplined.

I created a new corpus and wrote a methodology framework organized around four research questions. The first question was about "operationalization knobs," the decisions required to turn an informal musical idea into something a program can compute. The document explicitly warned against defining a performance as a ninety-second composed head, a ninety-second composed tail, and a jam in between. That fixed-offset rule fails whenever the opening composition lasts longer than ninety seconds, which is true for most of the songs I wanted to study.

I gave the framework to the agent. We built seven audio axes, per-performance fingerprints, per-window heat maps, clustering analyses, threshold sweeps, and canary checks. The work produced patterns. The patterns produced two blog posts, complete with timestamped walkthroughs and guided listening. I built a small site for the research and a listening assignment so other people could check whether the metrics corresponded to what they heard.

For six days, the work looked like work.

Then a reviewer raised an asymmetry in two of the axes. I asked the agent to explain what the script was doing. During that explanation, it mentioned two constants named `HEAD_S` and `TAIL_S`. Both were set to ninety seconds. The implementation treated every window after the first ninety seconds and before the last ninety seconds as jam material.

The constant came from an earlier audit script. It had been reused in the new pipeline without being reconsidered, even though the written framework identified that exact reuse as a failure mode.

I asked what this did to specific tracks. The rule always assigned the track duration minus the first and last ninety seconds to `jam`. Using the rounded durations below, that works out to roughly 84, 79, 90, and 84 percent of each track. The answer was not subtle:

- A June 29, 2000 Sand was 18.8 minutes long. In my listening notes, the opening composition ran roughly four to five minutes.
- A September 4, 2016 Light was 14.1 minutes long. I heard roughly three to four minutes of opening composition.
- A 1994 Bangor Tweezer was 29.9 minutes long. I put its opening composition at roughly three to five minutes.
- A 2024 Bethel Bathtub Gin was 18.4 minutes long. I heard roughly five to seven minutes of opening composition.

In every case, the script began the jam at ninety seconds.

Several minutes of verses, choruses, and composed instrumental material had been placed inside the windows labeled `jam`. Every chart built from those windows mixed the musical phenomenon I wanted to study with material the method itself said should be excluded.

The default propagated through nine analysis scripts and the artifacts they produced: fingerprints, threshold sweeps, cluster assignments, silhouette scores, confidence intervals, per-window data, and every cell in the figures included in the blog posts. Research questions 2 through 4 lost their numerical findings; the first question was principally a preregistration. The listening study was asking people to evaluate evidence the project itself could no longer defend.

So Notebook 2 came down too.

## Why the code review did not save me

Neither notebook failed because the generated Python was nonsense. Imports resolved. Types lined up. Feature arrays had the expected dimensions. Charts read the artifacts they were supposed to read. The prose accurately described the chart values.

In Notebook 1, a few lines created the wrong unit of separation. In Notebook 2, one inherited number defined the wrong observation window. The surrounding implementation was mostly correct. Reviewing the diff for ordinary software quality would not necessarily have exposed either decision.

Research software has an awkward property: setup is part of the claim. A window size, grouping key, exclusion, normalization reference, or threshold can be a few characters in a program and still determine what evidence the program is capable of producing. When the decision is wrong, the program may run more cleanly than ever. It is computing the wrong experiment without complaint.

The agent was good at producing plausible continuations. An earlier script had a ninety-second constant, so the next script inherited it. A request to remove public assignments could be satisfied by archiving them. A request for progress after a process failure could be satisfied by producing a process. None of these moves was absurd in isolation.

After Notebook 2 failed, I told the agent to delete the active listening assignments so they would no longer appear on the site. It changed their status to `archived` and wrote a log entry explaining that this achieved my public-takedown intent while avoiding a destructive action. I had said delete. The agent had made a locally defensible substitution and presented it as the completed task. I had to ask again, using the phrase "hard delete," before the rows were removed.

This did not ruin an experiment. It revealed the same interaction in miniature: the agent filled an unstated decision with a reasonable default, and the result looked responsible while doing something I had not authorized.

A little later, frustrated with cleanup, I asked for some real progress that was not tainted. The agent responded with a 273-line research-discipline document: ten rules, four roles, and a seven-step approval workflow. We still use descendants of it, but at that moment it was a process artifact in response to a request for an empirical result.

The silence was the problem. The downstream work treated each choice as settled before I knew a choice had been made.

By the time a result exists, it becomes harder to reopen its premises without wanting the premises to be right. I had charts. I had interpretations. I had blog posts that looked finished enough to put online. The quantity and polish of the artifacts increased the cost of asking whether the first step had been valid.

The choice never appeared as a decision requiring review. The agent did not volunteer it, and the reports treated the resulting pipeline as complete. That is the system we actually built.

## Two failures, not one

It would be convenient to end this by writing a checklist called "never trust defaults." That would miss the important distinction between the notebooks.

Notebook 1 failed because information crossed a boundary. The test evidence was no longer independent of training and development. This is a provenance problem that extends through duplicate files, derived caches, feature choices, and the researcher who remembers what happened.

Notebook 2 failed because an implementation convenience defined the observable phenomenon. Even with a perfectly isolated test set, the pipeline would still have called several minutes of composition a jam. This is a meaning problem. It asks whether the measurement corresponds to the event named in the claim. A later experiment would pass every process gate we wrote after these failures and demonstrate that the problem was not solved by cleaner procedure.

The common mechanism is acceleration. The agent could propose code, implement it, run it, interpret the output, write the post, and suggest the next experiment before its own reports had surfaced the premises of the first one. Every trip around the loop made the hidden choice more expensive to expose.

We eventually returned to the original set-detection problem. That return did not make the two lost notebooks irrelevant. It gave us a narrower question with an answer that could be checked during a live show. It also forced us to decide what a check actually is.

The first answer begins with the boundary Notebook 1 crossed: once evidence helps shape the system, what would it mean for that evidence to be independent again?
