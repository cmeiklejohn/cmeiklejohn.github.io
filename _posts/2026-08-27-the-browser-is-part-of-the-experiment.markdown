---
layout: post
title: "The Browser Is Part of the Experiment"
subtitle: "A model score cannot tell you what the live system actually heard."
date: 2026-08-27 08:00:00 -0400
group: ai
series: lab
editorial_review: three-pass
published: false
permalink: /series/the-machine-in-the-lab/the-browser-is-part-of-the-experiment/
categories: ai research zabriskie agents
---

SetScope once identified Animal before the music started.

On August 12, I opened a historical Goose show in the browser and started the live song guesser from the beginning. The screen locked to Animal after 40 captured seconds of crowd and intro audio. By the time I took a screenshot at one minute, the interface displayed Animal with what it called 99 percent confidence.

The number was not a calibrated probability. It was a rank score, and Animal led the next candidate by only 0.0022.

The [incident record](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0477-june30-training-overlap-incident.md) established more than a suspicious screenshot. The exact June 30, 2026 performance had been used in local model fitting. At least two ten-second training crops contained mostly crowd or intro audio while carrying the Animal label. During the rehearsal, the music gate falsely declared music after 3.25 seconds and allowed a closed-set classifier to choose among songs even though no song had begun.

The captured stream and training file were not identical enough to claim literal waveform memorization. The correct title did not rescue the event anyway. SetScope had made a song decision on nonmusic from a performance it had already seen.

That failure involved training overlap, bad labels, a bad gate, a closed catalog with no safe unknown state, and a UI that called an ordinal score confidence. It did not explain every strange browser result that followed.

## What the offline number measured

At the time, SetScope also had an apparently strong component result. It correctly named 46 of 54 accepted, in-vocabulary, single-song tracks from five shows absent from its local fitting data, roughly 85 percent. The tracks were already cut, decoded, and known to contain one labeled song. They represented 45 of the model's 254 labels.

That result answered a useful question: could the classifier recognize many pre-cut tracks under that protocol?

It did not test whether Chrome delivered valid audio, whether the system stayed quiet during crowd noise, whether a title remained stable through a jam, whether it switched across a segue, or whether an internal decision reached the interface. Calling the number live accuracy made a component test answer for a product that had never run.

SetScope's actual path was much longer. Chrome decoded the stream. macOS routed it through a virtual audio device. A capture process segmented and resampled it. A music gate decided whether the song models were allowed to run. Several model families proposed candidates. A controller decided when one candidate could replace another. Only then did the application render a title.

The offline evaluation began near the model. The product began in the browser.

## A separate run heard no music at all

Later that day, a different browser session ran for more than six minutes without admitting a single second of music. The song selector never ran.

This was the opposite of the first Animal incident. It could not explain the pre-music lock, because in this session no model was allowed to make any song decision. It exposed a separate failure earlier in the audio path.

The saved waveform contained exact-zero gaps about 0.105 seconds long at roughly 1.1-second intervals. The signal looked loud and the segment timestamps advanced, but the music gate saw a discontinuous input. Its median score was about 0.08. The same Animal performance decoded from the read-only archive scored almost 1.00.

Removing an asynchronous resampler eliminated the periodic zeros but not the missing audio. Nominal ten-second segments contained only 7.3 to 8.9 seconds of decoded samples. A direct tone test localized the remaining loss to FFmpeg's AVFoundation capture path.

The dashboard had been measuring files declared, not audio successfully transported.

## Repairing the path before judging the model

We replaced FFmpeg capture with a native CoreAudio helper and made the runtime account separately for source frames received, samples finalized into captured audio, and samples decoded by the recognizer. Segments also recorded signal level, exact-zero fraction, and internal dropouts. A session with missing samples became an invalid run instead of a mysterious model miss.

The first fresh browser run after the capture repair used the same historical Animal performance. It received 246.0 source seconds and finalized and decoded 245.93 seconds. No song was proposed during the opening crowd and intro. All four acoustic views eventually ranked Animal first, and the controller locked it 70 seconds after the estimated music onset.

That valid input exposed a policy problem. One uncertain gate observation during continuous music immediately erased the accumulated window. We copied the exact captured bytes and changed only that rule: an uncertain observation waited for the next one instead of resetting state. Animal locked 40 seconds earlier.

That was an exact-byte policy comparison. It showed that one controller change helped on one opened diagnostic input. It did not prove that the repaired browser path behaved the same way.

So we ran the browser again. The next session reconciled 215.9 source seconds with 215.89 decoded seconds. Nothing was proposed during the intro. Animal became the first acoustic hypothesis after 30 admitted music seconds, all four views agreed by 60, and the displayed state locked around 70 seconds after music onset and remained stable.

Now the transport repair, policy change, and browser path had been tested separately.

## One song was not enough

Before the transport repair, a Dr. Darkness-to-Drive rehearsal had missed Drive, switched incorrectly to Madhuvan, and ended with more than two minutes of unaccounted capture time. We repeated the same two-song sequence after repairing the input path.

In the new run, Dr. Darkness locked after 70 seconds of admitted music. Individual model views drifted briefly during the song, but the held state did not switch. The player advanced naturally into Drive around 357 captured seconds. Drive led the selector at 400 seconds, but the controller held Dr. Darkness until all four acoustic sources and the transition model agreed at 410 seconds.

That was the behavior I wanted: frequently refreshed evidence, a conservative displayed state, and a real transition without requiring a silent boundary.

The run still failed its final input accounting. Capture wrote 48 segments while the recognizer exited after 47, leaving a 1.284-second gap. That defect led to a fixed half-second reconciliation rule and a shutdown fix. A second exact-byte replay, this time testing final-segment draining rather than gate policy, processed all 48 segments and preserved the Dr. Darkness and Drive decisions.

Two replays had answered two different questions. The Animal gate-policy change received a fresh browser confirmation. The final-segment repair had only its exact-byte replay; another complete browser run was still required.

## The next evening's live show

On August 13, Goose played in San Diego. SetScope listened while the show was happening, using audio that had not existed during development.

The [post-show audit](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0521-2026-08-13-live-show-audit.md) was useful and mixed. A correct song appeared in the acoustic evidence for 11 of 12 performances. The runtime emitted the correct current song at least once for 10 of 12. It emitted 13 locks in total, including three false switches, and missed two songs.

Set 1 had a 66-second capture-timebase gap, so its exact latencies were invalid. Set 2 passed input accounting, but its song boundaries were reconstructed from published whole-minute durations. The surviving audit does not establish what was rendered in the UI or delivered through the publication path.

This was genuine prospective evidence that SetScope could recognize songs from a new show. It was not a formal whole-show reliability result. It showed the model could find most of the songs, and also that the controller could mistake a section of a long jam for another song, delay a correct transition for minutes, or retain the wrong state and never emit an acoustically recognized encore.

The browser tests changed what counted as progress. Offline evaluation could test recognition under controlled inputs. Browser and live runs tested whether the product received valid audio, remained quiet when it should, held the right state, changed at the right time, and delivered the result. SetScope needed both. A percentage from either one could not stand in for the other.
