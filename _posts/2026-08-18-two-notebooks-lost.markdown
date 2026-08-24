---
layout: post
title: "Two Notebooks Lost"
subtitle: "The first collapsed under validation. The second repeated a mistake it had already named."
date: 2026-08-18 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/two-notebooks-lost-series/
categories: ai research zabriskie agents
---

The audio project began with a fairly direct product question: could a computer listen to a live Goose show and tell people which song was playing? That product eventually became SetScope.

The first research notebook did not stay on that question. Once I had a labeled archive of live recordings and a growing collection of audio features, the project expanded into a much broader audio-intelligence program. It compared performances of the same song, tested how much audio was needed to recognize one, searched for similar jams, and tried to measure the difference between composed music and open improvisation. The corpus mixed Goose and Phish recordings. The improvisation work leaned heavily on Phish because Phish has a deep archive and decades of community annotation.

An LLM agent was running much of this loop. I supplied the recordings, the questions, and the constraints. The agent proposed analyses, wrote the code, ran them, interpreted the results, and used one result to choose the next experiment.

The speed was exhilarating. It was also the problem. In eight days the project produced more code, figures, findings, public pages, and listening exercises than I could keep in my head at once. When the notebook failed, it did not fail for one tidy reason. An audit exposed several different problems at the same time.

By April 30, three Notebook 1 research posts had appeared on the public site. One had already been pulled as its taxonomy shifted. A replacement had briefly appeared and then returned to draft after review. That evening I withdrew the notebook's central published Type II result. I abandoned Notebook 2 on May 6.

## Notebook 1: the published claim fails its held-out test

One line of work in Notebook 1 tried to recognize songs from short excerpts. Another asked whether two recordings were performances of the same song. A third tried to identify different kinds of improvisation from audio features.

The same-song work produced one of the first warning signs. One analysis classified pairs of performances as "same song" or "different song." Different pair rows could still reuse an individual performance, making the reported split less independent than it appeared.

A related retrieval experiment had a different problem. Even when it held out one performance, other performances of the same song could remain in training. When the agent reran retrieval with entire songs held out, the audio-only system put the right song first 43 percent of the time and in its first five choices 76 percent of the time.

Those were corrections to song-recognition experiments. They were not the event that killed Notebook 1.

The published claim that collapsed was about improvisation. The notebook had proposed three acoustic signatures for what jam-band listeners call Type II playing, where a performance leaves the song's ordinary composed structure. A public research post used the November 9, 1998 Bathtub Gin from UIC as its central example.

The signatures looked convincing on the performances used to develop them. Then the agent ran them on 44 held-out cases: 11 community-annotated Type II performances and 33 composed or Type I controls. One of the three signatures never fired. All three never fired together, even though the public research post was built around that combination. The other two produced more false positives than the argument could survive, including on Reba and several Goose songs with contrasting composed sections.

The detector was reading changes inside the composition as departures from the composition. Its first-ninety-seconds, middle, and last-ninety-seconds windowing assumed a song form that many of the songs did not have.

That result was decisive. The public post's central claim did not generalize beyond the examples that had shaped it, so I withdrew it on April 30.

The same audit also forced a harder conclusion about the notebook as a whole. It had accumulated many questions, datasets, thresholds, and evaluations without keeping one final test set out of sight from beginning to end while the program chose its next steps. Some individual reruns could produce more honest estimates. The notebook itself could no longer produce an independent final verdict on the research path it had already taken.

The problem was not simply "data leakage," and it was not simply a bad ninety-second constant. The system had generated an expanding chain of claims faster than it had maintained the evidence needed to support them.

## Notebook 2: the written method and the running code diverged

Notebook 2 was the reset. It began with a locked split of 72 recordings across 13 songs: 33 for training, 14 for validation, and 25 for testing. Many recordings had also appeared in Notebook 1, so this was not a magically untouched corpus. Their earlier roles were documented, and the new notebook was supposed to answer a narrower question under a more explicit method.

The question was how to turn the informal idea of Type II improvisation into something a program could measure. The methodology listed the choices that had to be made: what reference to compare a performance against, which audio features to use, how to aggregate them, where the jam began, and how any threshold would be selected.

It also named the earlier windowing rule as a known failure:

> Fixed offset: first 90 seconds equals the head, the middle equals the jam, and the last 90 seconds equals the tail. Simple, song-form-naive, and wrong for Reba or any song whose composed section runs longer than 90 seconds.

The document chose per-song boundaries as the default. The fixed offset was supposed to be a fallback that could not proceed until it passed a specific negative control.

Then the first Notebook 2 analysis set `HEAD_S = 90` and `TAIL_S = 90`.

The source code did not hide the constants. Its comments even called the calculation the "N1 default." What the research loop failed to do was notice that the implementation contradicted the notebook's own declared method and stop the experiment there.

Instead, the work continued. The notebook expanded the corpus, compared several audio measurements, generated charts, and produced two listening assignments and two long research posts. The posts remained drafts, although they were accessible at their direct URLs while I worked on them.

By the time the mismatch was identified, the fixed boundary had propagated through the analyses, figures, draft posts, and listening exercises.

The mistake was especially difficult to excuse because Notebook 1 had already demonstrated it and Notebook 2 had already written down the rule that should have prevented it. This was not an unknown edge case. It was a named failure mode reintroduced by the first implementation.

On May 6, I abandoned Notebook 2. Its active research files were removed from the working tree, the listening assignments and submissions were deleted, and Notebook 3 began under a new research discipline.

## Why the work still looked finished

Most of the generated Python was not nonsense. It loaded the expected files. The arrays had the expected dimensions. The charts accurately displayed the artifacts they read. The prose accurately described the charts.

That is what made the failures expensive.

Notebook 1 built a public interpretation on detectors that failed held-out predictive validation. Notebook 2 built a new methodological framework, then allowed its implementation to violate one of the framework's explicit rules. In both cases the code continued to run, the outputs continued to look coherent, and each output gave the agent another plausible reason to continue.

I was responsible for putting the work in front of other people. But the answer cannot be that an autonomous research program is useful only if I independently reconstruct every split, constant, and intermediate artifact after each run. At that point the system has handed the research process back to me and added an audit job on top.

The program needed to make its setup decisions explicit, verify them against the written method, and stop when they disagreed. It did none of those things reliably.

Notebook 3 continued with narrower Phish questions. On May 29, Notebook 4 returned the project to its original job: improve the live Goose song guesser. That gave us a much more direct output to check. Either the song title was right or it was not. The next post follows that return to SetScope and the problem it exposed: once an agent has used an evaluation to choose what to build next, what evidence is still independent enough to evaluate the resulting system?
