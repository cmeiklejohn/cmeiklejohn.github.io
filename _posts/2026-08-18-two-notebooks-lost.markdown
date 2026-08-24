---
layout: post
title: "The Safeguards Have to Run"
subtitle: "Two abandoned notebooks showed me the difference between a research rule and a working control."
date: 2026-08-18 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/two-notebooks-lost-series/
categories: ai research zabriskie agents
---

*This is Part 2 of [The Machine in the Lab](/series/the-machine-in-the-lab/), a series about using an autonomous research program built from large language models (LLMs) to build SetScope, a live Goose song guesser.*

In the first research notebook, I told an autonomous program to keep training and test data separate. Its report showed a clean split. It wasn't.

How could both be true?

The program was learning whether two recordings were performances of the same song. It created pairs of recordings, then randomly divided those pairs between training and testing. A performance called A could appear with B during training and appear again with C during testing. The rows were different, but the program had already encountered A. That meant the test could reward it for recognizing a performance it had seen rather than learning what made two performances instances of the same song.

This is the kind of problem a working safeguard should prevent. Instead, the rule existed in the research plan while the code quietly violated it.

The LLM wasn't just writing code that I reviewed step by step. I supplied the objective, the audio collection, and the rules. The program proposed experiments, wrote and ran code, interpreted results, and chose what to investigate next without asking me to approve every decision. I called each sustained run a notebook: the accumulated data, code, experiments, reports, and decisions for one research direction.

The original objective was to build [SetScope]({% post_url 2026-08-15-science-at-llm-speed %}#the-research-loop-gets-faster), a system that could listen to a live [Goose](https://www.goosetheband.com) show and name the song while the band was still playing it. A live version can change in speed, length, arrangement, and improvisation while remaining recognizably the same song.

The first song-identification experiments produced a positive result: measurements from the composed sections appeared to distinguish one song from another across live performances. That suggested the composed parts preserved much of a song's identity while the improvised parts accounted for much of the variation between performances. So the program started asking what those same measurements could tell us about the improvisation. Its reports arrived quickly and appeared to form a coherent account of the music, so I let it continue. That detour became Notebook 1.

## Notebook 1

Correcting the pair split reduced the reported performance of the classifier that used measurements taken only from the audio. There was still useful signal in it, but the original result hadn't tested whether it could generalize to performances it had never encountered. The program had acknowledged the rule about separate evidence without implementing a split that enforced it at the level of a complete performance.

The notebook had already expanded beyond that classifier. One later experiment tried to identify open-ended improvisation by comparing an opening section, a middle section, and a closing section of each track. When actual musical boundaries were unavailable, the implementation treated the first and last ninety seconds as composed material and the middle as the jam.

I asked the program to evaluate the detector against a separate group of 44 performances that had not been used to choose its numerical cutoffs. Six were too short for the method, leaving 38 analyzable cases. The fixed windows placed composed material that occurred after the opening ninety seconds inside the region treated as the jam. The detector repeatedly treated changes within those composed passages as evidence of open improvisation.

That audit rejected this detector. It did not provide a clean test of the entire path that led to it, because earlier results had already influenced which features, thresholds, and questions the program pursued. I stopped Notebook 1.

## Notebook 2

Notebook 2 continued the improvisation work under a written methodology intended to prevent the first notebook's failures. It began on April 30 and assigned each song permanently to one of three uses: trying methods, choosing among them, or one final test that was supposed to remain unopened until the method was finished. It also rejected the blanket ninety-second division in favor of boundaries chosen for each song from the music itself.

The code didn't follow those rules. The first implementation again set the opening and closing regions to ninety seconds. Later analyses reused measurements cut at those boundaries. The method had warned against the assumption, but nothing compared the running code with the method and stopped the work.

The final test wasn't protected either. The locked collection assigned every performance of five songs, 25 recordings at the time, to remain unopened until the method was finished. Development analyses accessed four of those five groups, accounting for 22 of the 25 recordings, while the program was still choosing its method. A label in a data file said the groups were reserved. Nothing prevented the program from opening them.

I had started Notebook 2 while Notebook 1 was still present for reference. On May 4, I told the program to remove every Notebook 1 file so it couldn't contaminate the ongoing work. The cleanup removed the visible scripts, reports, figures, and data from the main project folder. But it couldn't make Notebook 2 a clean restart: the new work had already inherited the old ninety-second default.

By the time I stopped Notebook 2 on May 6, its fixed segmentation had propagated into analyses, figures, two draft blog posts available by direct link, and listening exercises. I had also put accounts of the earlier work on the project's public site and sent listening exercises to friends. I don't know how many people saw the pages, but the autonomous process had produced two coherent bodies of work that I couldn't trust.

A later inventory on May 17 exposed another failure. After both notebooks had been removed from the project folder, 1,120 generated feature and measurement files from the abandoned work remained in reusable storage outside that folder. A subsequent notebook had loaded 131 of those files while calculating one of its numerical cutoffs. Removing them changed the number only slightly and didn't materially change the affected results. But the supposed clean slate hadn't been clean, and the program had no record showing where every generated file had come from.

## The safeguards have to run

What had the written safeguards actually protected? In practice, very little: every named boundary could be crossed without stopping the run.

A performance-level split should have rejected any training and test pair that shared a performance. Reserved data should have been inaccessible until a recorded release step. Generated files should have carried their source history, and a purge should have verified that no descendant remained in shared storage. A run whose code contradicted its written method should have stopped before producing dependent results.

Those checks belong inside the autonomous process. My job is to define and approve the constraints, not to reconstruct every data assignment, generated file, and numerical constant after the program finishes. If the system requires that reconstruction, it has not automated the research process. It has automated the production of work for a human auditor.

A natural-language instruction can describe a safeguard. It cannot enforce one.

After Notebook 2, I set the improvisation work aside and returned the program to SetScope's original Goose song-identification problem. A new live show offered evidence that no earlier notebook could have seen. But each show could be new only once. Once the program used that show to decide what to do next, was it still independent evidence or had it become part of development? That's the question in Part 3. Part 4 returns to the separate problem exposed by the ninety-second detector: whether a consistent measurement actually represents the music named in the claim.
