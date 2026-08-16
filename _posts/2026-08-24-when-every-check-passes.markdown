---
layout: post
title: "When Every Check Passes"
subtitle: "All the artifacts agreed. The interpretation had never been tested."
date: 2026-08-24 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
permalink: /series/the-machine-in-the-lab/when-every-check-passes/
categories: ai research zabriskie agents
---

On May 23, I published an audio-research result after it passed every check the project required.

Those checks existed because the two earlier notebooks had failed. We had added explicit plans, reviews, artifact audits, and publication approvals to prevent another plausible result from outrunning its evidence.

The plan had been reviewed. Five advisor conditions had been applied. The figure-generation script passed three byte-faithfulness audits. The numbers in the prose traced to saved artifacts. A content audit found no unsupported numerical claims. A second advisor review approved the complete dispatch. I approved a soft launch and then a hard launch. The page went live with a homepage card and a final [publication log](https://github.com/cmeiklejohn/zabriskie-audio-research/blob/bfd8b0aa360b7898d7f50fa4aa6f119a9783d4d5/docs/logs/dispatch-002-publication.html).

Every recorded check was green.

Then I read the article and listened to the first example.

The listener mark sat two seconds before the audible end of the song.

## The result we thought we had

The experiment concerned the end of a jam. In the project's labeling system, a listener marked the point where improvisation ended and the band returned to composed material. We compared those marks against an onset-density changepoint detector, a signal-processing method intended to find a large change in musical activity.

The reported result was that the detector recovered the listener-marked end on three of four Sands and three of four Bathtub Gins within a song-specific tolerance. The article interpreted this as evidence that the end of a jam was acoustically recoverable as a band-level return to composed structure, even though the beginning remained difficult to detect.

That interpretation was the reason the result mattered. Agreement between timestamps by itself is not very interesting. If the detector and listener were identifying the same musical event through different paths, we had evidence that a community concept could be grounded in an acoustic change.

The first example was Sand from July 3, 2000. The listener mark was 11:56, the detector prediction was 11:42, and the music ended around 11:58. The archived track continued through audience and tail audio until 13:23. When I played the clip, the marked event did not initially sound like an abstract structural boundary. It sounded inseparable from the song ending.

The pipeline had answered the scoring question correctly: the predicted time was near the labeled time. I no longer knew whether either timestamp supported the sentence we had published.

## The first diagnosis

The published six-of-eight verdict covered four Sands and four Bathtub Gins. The broader ten-case positive set also contained a Reba and a Tweezer. Five of the six reported hits had a listener mark within two minutes of the archive track boundary. Two were within a minute.

The first explanation seemed obvious. Perhaps the labels were systematically anchored to the end of the musical performance rather than the return to composition. The labeling instructions allowed two cases: mark the return to composed material, or mark track end when the jam continued through the end of the song. The analysis had silently assumed every value had the first meaning.

Under that explanation, the detector might have been doing useful structural work while the labels were wrong. One failed Bathtub Gin appeared to support the inversion: the detector found a shift around 14:50, while the label sat at 16:11, almost exactly at track end.

We wrote the withdrawal record around that diagnosis. The central claim was unsupported because label agreement could not establish recovery of composed structure if the labels represented a different event.

Listening to all ten cases made the diagnosis mostly wrong.

## The labels were better than the explanation

I used a one-off page that presented each listener mark and detector prediction with audio controls. Phish often returns to the song shortly before the music ends. The proximity that had looked like evidence of bad labels was, in most cases, a property of the performance structure.

The [preserved review record](https://github.com/cmeiklejohn/zabriskie-audio-research/blob/bfd8b0aa360b7898d7f50fa4aa6f119a9783d4d5/docs/logs/dispatch-002-withdrawal.html#L144-L170) contains a contradiction I missed at the time. Its ten-row case table records eight listener marks I heard as composed returns, one Bathtub Gin mark as track end without a composed return, and one Reba for which I wrote that both the listener and detector marks were wrong. The summary immediately below the table says nine of ten listener marks were composed returns. Those statements cannot both be true.

The table is the case-level record, so I am using eight here and treating the old nine-of-ten summary as an error. The cases were:

- **Sand, July 3, 2000:** composed return; original score: hit; 1:27 to the archive boundary.
- **Sand, December 31, 2010:** composed return; original score: hit; 0:36 to the archive boundary.
- **Sand, July 28, 2017:** composed return; original score: hit; 1:26 to the archive boundary.
- **Sand, October 26, 2018:** composed return; original score: miss; 2:11 to the archive boundary.
- **Bathtub Gin, July 10, 1999:** track end; original score: miss; 0:05 to the archive boundary.
- **Bathtub Gin, July 20, 1998:** composed return; original score: hit; 0:58 to the archive boundary.
- **Bathtub Gin, February 14, 2003:** composed return; original score: hit; 2:06 to the archive boundary.
- **Bathtub Gin, February 28, 2003:** composed return; original score: hit; 1:45 to the archive boundary.
- **Reba, August 16, 1993:** listener and detector marks both rejected; original score: miss; 5:59 to the archive boundary.
- **Tweezer, November 2, 1994:** composed return; original score: hit; 2:07 to the archive boundary.

Eight of ten is enough to reject the first diagnosis that the labels were systematically anchored to track end. One label used the second permitted interpretation. One label did not survive the review at all. Most of the suspicious proximity came from the way these performances returned to the song and then ended.

## What the corrected case record could say

My ten-case listening review has important limitations. I was one listener. I knew the experiment had failed, knew the suspicious pattern, and saw the selected positive cases. This was an outcome-aware, post-publication review, not blinded annotation or an inter-rater agreement study. Because I reviewed selected positives, the exercise cannot estimate false negatives, specificity, or the detector's behavior over the full corpus.

The review could reject our first explanation, but it did not establish the strong replacement explanation written into the withdrawal summary. All seven hits in the broader table were composed-return cases whose marks occurred 36 to 127 seconds before the archive boundary. The valid Sand miss occurred at 2 minutes 11 seconds, only four seconds beyond the largest hit gap. The Gin miss used the track-end interpretation, and the Reba mark at 5 minutes 59 seconds had itself been rejected.

The algorithm selected the final large changepoint in time order, so it was structurally biased toward late events. The selected ten-case review does not show how often that bias caused a hit or miss. It showed that the original interpretation had not been independently established, and that the first confident explanation of the failure had outrun the evidence too.

That is not a new detector result. It is a limit on what the old result can claim.

## What the gates actually checked

Nothing in the approval chain was fake. Each check answered a legitimate question:

- Did the implementation follow the plan?
- Did the generated figure match the saved numerical artifact?
- Did the prose accurately report the figure and result?
- Were the planned unit, thresholds, and qualifiers preserved?
- Were the required reviews and approvals recorded?

These are necessary checks. Without them, a claim can drift between plan, code, result, and prose. But every gate followed the same derivative chain. The label fed the scorer. The scorer fed the artifact. The artifact fed the figure. The figure fed the prose. Reviewers traced the claim backward through that chain and found it internally consistent.

No gate asked what was audible at the labeled and predicted timestamps.

Traceability established where the claim came from. It did not establish that the source measurement represented the musical event the claim named.

Notebook 2 had left this debt behind. Its ninety-second default made a convenient segment define what counted as a jam. The new discipline caught inherited constants and forced plans to state their assumptions, but we treated the labels in Dispatch 002 as settled ground truth. The interpretive choice moved from a constant into the meaning of a field, where the existing rules did not know to look.

The most embarrassing part was that the ambiguity was documented. The labeling instructions explicitly said `jam_end` could mean return to composition or track end if no return occurred. A plan-to-data audit could have noticed that the hypothesis assumed only the first case. It did not.

## A different evidence path

The project added a rule requiring an acoustic reality check for claims about audio. Before an analysis proceeds, and again before publication, someone must listen at the relevant labels and predictions and record what is audible.

The value of this check is not that human hearing is infallible. It is that the evidence does not descend from the same artifact chain. A waveform feature, a label, and a chart can agree because they share an assumption. Listening can contradict the interpretation through another modality.

The original reality check was not independent in every sense. I was the author, knew the outcomes, and had already proposed an explanation.

Later assignments were redesigned to present neutral metadata, randomize order, omit preloaded notes, and collect structured responses. That reduces some forms of priming and makes disagreement easier to inspect. It does not turn listeners into objective instruments, and it does not retroactively improve the first review.

This matters because "add a human" is not a methodology. Which human? What did they know? Which cases did they hear? What categories were available? Were disagreements preserved? A domain check has its own measurement design.

The first check still did something the elaborate agent process had failed to do: it returned to the underlying phenomenon. It made the published interpretation suspect, and then it made the first attempted correction suspect too.

## The erratum needs evidence

An erratum can repeat the original failure if it is allowed to close around the first plausible explanation. The correction needed an evidence path outside the artifacts and diagnosis that produced it.

This is the limit of process-heavy review in a discovery loop. Multiple agents can inspect a plan, implementation, result, and article while sharing one blind spot. Adding another reviewer to the same chain increases scrutiny without necessarily creating contradiction. The evaluator has to be capable of telling the entire chain that its interpretation is wrong.

For Dispatch 002, that evaluator was a person pressing play.

The project eventually left the jam-classification detour and returned to the original song-identification tool. That made the output easier to name: either the screen said the song that was playing or it did not. It did not make evaluation simple. An offline score still could not say what happened between the browser and the model.
