---
layout: post
title: "Two Notebooks Lost"
subtitle: "Notebook 1 had no final test. Notebook 2 wrote safeguards but did not enforce them."
date: 2026-08-18 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/two-notebooks-lost-series/
categories: ai research zabriskie agents
---

*This is Part 2 of [The Machine in the Lab](/series/the-machine-in-the-lab/), a seven-part series about building SetScope, a live Goose song guesser, with an autonomous research program built from large language models (LLMs).*

[SetScope]({% post_url 2026-08-15-science-at-llm-speed %}#the-research-loop-gets-faster) began with a fairly direct question: could a computer listen to a live [Goose](https://www.goosetheband.com) show and tell people which song was playing while the band was still onstage?

Building a live recognizer first required a more basic answer: could audio tell that two performances were the same song even when the tempo, key, length, and improvisation changed? The first experiments compared same-song and different-song pairs. Their early results appeared to show that the composed sections carried most of the stable song identity while the jams, the improvised passages inside the performances, varied from night to night.

That apparent result opened a broader and more interesting question. If improvisation was where performances diverged, could numerical measurements of rhythm, harmony, and texture describe how they diverged? Answering it required labeled data for two kinds of improvisation that jam-band listeners call Type I and Type II. In Type I, the band solos or varies the music while remaining inside the song's composed structure. In Type II, it leaves that structure for open-ended improvisation. My Goose archive had recordings and song titles, but it did not have a comparable collection of those annotations. [Phish](https://phish.com/) did: decades of performances accompanied by community-maintained lists of notable jams, listener-written notes, and Type II labels.

That is how the project changed bands. SetScope was still a Goose product, but the research notebook, a running workspace of code, data, experiments, and reports, was now driven by Phish data and Phish questions. It began searching for similar jams and trying to distinguish composed music from open improvisation instead of asking only how quickly SetScope could name a Goose song.

The LLM agent appeared to be making remarkable progress. I supplied the recordings, questions, and constraints. The agent proposed analyses, wrote the code, ran it, interpreted the results, and used each result to choose the next experiment. Each experiment produced another plausible finding and another promising direction, so I let it keep going instead of stopping to ask whether the new work was still improving SetScope.

The speed was exhilarating. In a short span, the project produced more code, figures, findings, webpages, and listening exercises than I could keep in my head at once. This was the [same pitfall described in Part 1]({% post_url 2026-08-15-science-at-llm-speed %}#a-polished-analysis-is-not-necessarily-a-scientific-result): the volume and consistency of the output made forward motion look like evidence that the work was correct. A missing rule in one experiment could quietly carry into later code, figures, posts, and listening exercises before I noticed.

That happened twice. Notebook 1 had no final test that could challenge the research path after the agent had used earlier results to choose what to do next. I stopped it and began Notebook 2 with explicit safeguards. Notebook 2 then proceeded without enforcing them. The failures were different, but both were able to spread because the research loop kept producing plausible work on top of an unchecked decision.

## Notebook 1: the missing final test

Notebook 1 had no rule requiring one final set of recordings to remain outside the entire sequence of experiments. The agent could use a result to choose the next question, use that answer to choose a measurement, and use that measurement to set a numerical cutoff. Each step produced another plausible result, but there was no untouched evidence left to ask whether the sequence had gone in the right direction.

The consequence became visible in the improvisation work. The notebook proposed three measurable patterns for Type II playing, where a performance leaves the song's ordinary composed structure. I made a research webpage explaining the result and showed it to two friends.

The agent later applied the detector to 44 recordings that had not been used to set its numerical cutoffs. One pattern did not appear on any of them. The complete three-pattern combination never appeared either. The other two repeatedly marked composed passages as open improvisation.

The detector had divided every recording into its first ninety seconds, its middle, and its last ninety seconds. Many songs do not have that structure, so it mistook ordinary changes between composed sections for departures into open improvisation. I took the webpage down.

That test showed that the detector on the webpage was wrong. It could not tell me which of the notebook's other results would survive a comparable challenge. Those results had already guided what the agent tried next, and Notebook 1 had reserved no final evidence for evaluating the path as a whole. The missing rule had allowed one result to become a webpage and guide later experiments before anything required the research loop to challenge it. That is why I stopped Notebook 1.

## Notebook 2: the rules existed only on paper

Notebook 2 tried to prevent the same kind of propagation. It assigned recordings to fixed roles and designated 25 for a final test, while documenting that some had already appeared in Notebook 1. Its written method also replaced the fixed ninety-second division with a default boundary tailored to each song. The old rule could be used only as a fallback and had to be checked against Reba, a Phish song whose composed sections had already fooled the detector.

The rules did not control what the research loop actually ran. While the agent was still deciding what to investigate next, one analysis examined 22 of the 25 recordings assigned to the final test. They still had the label "test," but their results had already influenced what the agent did next.

The same thing happened with the audio boundaries. The newer analyses continued to use ninety seconds instead of the song-specific default, and the required Reba check was not run. Nothing compared the running code with the written method and stopped the work.

The ninety-second decision then appeared in later analyses, figures, two draft research posts, and two listening exercises I sent to friends. The code, charts, and prose agreed with one another because they all inherited the same setup. On May 6, I stopped Notebook 2, removed its active code and research files, and deleted the listening exercises and the responses people had submitted.

## Why the work still looked finished

Notebook 1 lacked a rule protecting a final evaluation. Notebook 2 wrote down safeguards, but nothing enforced them. In both cases, unchecked setup choices traveled through a growing body of internally consistent work before I noticed them.

Most of the generated analysis code was not nonsense. It loaded the intended files. The charts accurately displayed the saved results they read. The prose accurately described the charts. That internal consistency is why I trusted the work long enough to build on it.

The same trap is not specific to this project. A fan analysis, an academic paper, or an autonomous research system can accumulate code, charts, and apparently consistent results until forward motion itself looks like scientific progress. The work can take the form of a research program before its conclusions survive a test that did not already influence the program.

I was responsible for showing the webpage to my friends and asking them to complete the listening exercises. But the answer cannot be that an autonomous research program is useful only if I independently reconstruct every data assignment, numerical constant, and intermediate output after each run. At that point the system has handed the research process back to me and added an audit job on top.

The program needed to expose its data assignments and numerical rules, compare the running code with the written method, and stop when they disagreed. It did none of those things reliably.

I made one controlled attempt to continue the Phish work under stricter experimental rules. Then, on May 29, I returned the project to its original job: improve the live Goose song guesser.

That return gave us a more direct output to challenge: a current-song guess produced while the show was playing. But evaluation was not limited to whether the guessed title was right or wrong. It also had to ask how long the guess took, when the system should withhold a guess because the evidence was weak, whether the title changed while the same song continued, and whether the result actually reached the viewer.

The next post follows the problem that return to SetScope exposed. Once test results have guided the next version of the system, can those same results still provide an honest test?
