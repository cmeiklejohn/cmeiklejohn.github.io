---
layout: post
title: "When Every Check Passes"
subtitle: "All the artifacts agreed. The interpretation had never been tested."
date: 2026-08-24 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
published: false
permalink: /series/the-machine-in-the-lab/when-every-check-passes/
categories: ai research zabriskie agents
---

On May 23, I published an audio-research result after every gate used to approve publication returned green. Then I read the post, pressed play on its first example, and heard the interpretation fall apart.

This was not an experiment on SetScope, the Goose song guesser. It came from the separate Phish detour described earlier in this series. The question was whether audio features could locate the end of an improvised jam, where the band returned to the composed song.

The process around the experiment was elaborate because two earlier notebooks had already failed. The plan was reviewed. Advisor conditions were added. The implementation, saved numbers, figures, prose, and final page were audited against one another. A second review approved the complete research post. I approved a soft launch and then a public one. The [publication log](https://github.com/cmeiklejohn/zabriskie-audio-research/blob/bfd8b0aa360b7898d7f50fa4aa6f119a9783d4d5/docs/logs/dispatch-002-publication.html) recorded every step.

Every gate used to authorize publication was green.

The first listener mark sat two seconds before the audible end of the music.

## The result we thought we had

The experiment compared human marks for the end of a jam with a detector based on changes in onset density, a rough measure of how frequently new musical events begin. It found large changes over time, removed candidates too close to the track edges or one another, and required at least two candidates to survive. When they did, it selected the last one. Otherwise it abstained.

That last part matters. The detector did not identify a return to composed music directly. It selected a late acoustic change and compared its time with a listener's mark.

The full evaluation contained 16 tracks: ten listener-marked positives and six negatives. Eight of the positives belonged to the two multi-performance song groups, four Sands and four Bathtub Gins; Reba and Tweezer appeared once each. The headline result came from those two larger groups. The detector landed within plus or minus 60 seconds of the listener mark on three performances of each song, six of eight in total. We interpreted those agreements as evidence that the return from improvisation to composition created a detectable acoustic change.

The first example was Sand from July 3, 2000. The listener mark was 11:56. The detector predicted 11:42. The music ended around 11:58, although the archive file continued through applause and tail audio until 13:23.

When I listened, the mark did not sound like an abstract structural boundary. It sounded inseparable from the ending of the song. The scorer was right that the two timestamps were close. I no longer knew whether their proximity supported the sentence we had published.

## The first correction was wrong too

The first explanation seemed obvious. Perhaps the listeners had marked the ends of performances rather than returns to composition.

The labeling instructions allowed two meanings for `jam_end`: mark the return to composed material, or mark the end of the track when the improvisation continued through the ending. The analysis treated those cases as if they all meant the first thing.

Several successful cases occurred near the end, which made the label problem look systematic. We withdrew the result and wrote the initial correction around that diagnosis.

Then I listened to all ten positive cases in the 16-track evaluation.

Eight listener marks were genuine returns to composed music. One Bathtub Gin used the permitted track-end interpretation because there was no composed return. One Reba mark did not survive review at all. The old withdrawal record says nine of ten were composed returns, but its own case table records eight; I am using the case-level record here.

The labels were not systematically anchored to track endings. In this selected set, the band often returned to the song shortly before the music ended. What looked like evidence of bad labeling was mostly a property of the performances.

That did not restore the published result.

The detector was designed to choose the last large acoustic change in a track. Many valid returns also happened late. Agreement between the two timestamps could therefore mean that the detector recognized the return, or that both the return and the detector's preferred change happened near the ending. The original experiment did not distinguish those explanations.

The listening review could invalidate our first interpretation and our first correction. It could not validate a replacement result. I was one outcome-aware listener reviewing selected positive cases after publication. The exercise could not estimate false negatives, specificity, or behavior over the full corpus.

The honest conclusion was narrower: the original score did not establish that the detector recognized a return to composed music.

## What all the checks had checked

Nothing in the approval chain was fake. It verified that the implementation followed the plan, the figure matched the saved result, the prose matched the figure, and the required reviews occurred.

Every check followed the same chain. The label fed the scorer. The scorer produced an artifact. The artifact produced a figure. The figure supported the prose. Reviewers traced the claim backward through that chain and found it internally consistent.

No prepublication gate required a claim-focused comparison of what was audible at both the listener and detector timestamps. The labels came from listening, but the interpretation built on their agreement had not been checked that way.

The ambiguity was visible in the data definition. The instructions said that `jam_end` could mean either a composed return or the end of the performance. The hypothesis assumed the first meaning. A plan-to-data review should have stopped there, but our process treated the field as settled ground truth.

Traceability established where the claim came from. It did not establish that the measurement represented the musical event named in the claim.

## Returning to the phenomenon

After the withdrawal, the project added an acoustic reality check for claims about audio. Before an analysis proceeds, and again before publication, someone has to listen at the relevant labels and predictions and record what is audible.

Human hearing is not an infallible instrument. The first review was especially weak as a new experiment because I knew the result and the suspected failure. Later listening assignments used neutral metadata, randomized order, no preloaded explanation, and structured responses so that disagreement would survive the review.

The value of listening was not that a human replaced the model. It was that the check encountered the underlying phenomenon through a path that did not descend from the same artifacts. The labels, scorer, chart, and prose could all share one mistaken interpretation. Audio could contradict all of them.

This is the limitation of adding more agents to one review chain. They can inspect a plan, implementation, result, and post while sharing the same blind spot. More agreement increases confidence only when a reviewer brings information capable of producing a different answer.

For this experiment, that information arrived when someone pressed play.

SetScope would expose the same lesson in a product setting. An offline classifier could score well while the browser received broken audio, the controller changed songs at the wrong time, or the interface displayed the wrong state. The next useful check had to run through the product itself.
