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

The song recognizer once identified Animal before the music started.

I had opened a Goose show in the browser, started SetScope, and watched the interface warm up over crowd noise. At sixty seconds, I watched it display Animal with 99 percent confidence. The music had not begun. That observation survives in my screenshot, not in the later transport-incident record.

This was especially alarming because the system was supposed to contain a music gate. Preshow chatter and crowd noise were not merely difficult examples. They were inputs on which the song classifier should not have been allowed to make a decision at all.

At the time, we also had an apparently strong offline result: 46 correct predictions on 54 segmented tracks from five held-out shows, or roughly 85 percent top one. I had been using that number as evidence that we were approaching a usable live recognizer.

The browser was telling me the number described something else.

That Animal rehearsal established a symptom, not its cause. We had not preserved enough of that session to prove why the premusic lock occurred. A later six-minute run, in which the system failed to admit any music at all, gave us the first captured input we could inspect from browser to recognizer.

## A song guesser, not a scientific instrument

SetScope has a deliberately ordinary job. It listens to a Goose stream and shows a viewer its best guess about the current song. The viewer should not need the show date, a setlist, or a second fan site.

The title on the screen is a product output. It is not a scientific finding about Goose. A browser rehearsal is a product test. Its logs can support a methodological claim about how we evaluated the system, but correctly naming Animal does not turn the application into a research instrument.

That plain description helps clarify what the old 85 percent result actually measured. We had cut labeled audio into segments and asked the classifier to name each segment. The 54 rows were leakage-disjoint at the file, performance, and show-date levels under the frozen result. They covered only 45 of the model's 254 catalog labels, and the test began with an already decoded piece of a known music track. It was a valid but narrow component evaluation.

It did not measure:

- whether the browser audio reached the recognizer intact;
- whether the system stayed quiet during preshow music, crowd noise, and set break;
- how quickly a title appeared after music began;
- whether the displayed title remained stable through a long jam;
- whether the system detected a segue with no silent boundary;
- what happened when the band played a song outside the catalog; or
- whether a correct internal state reached the screen or a public chat.

Calling 46 of 54 "live accuracy" collapsed all of those untested behaviors into one convenient percentage.

## The demonstrations got stranger

Animal was not an isolated miss. Browser tests produced rapidly changing song identities, long delays after all acoustic models appeared to agree, and locks authorized by a contextual rule even when the audio models favored another song. One run continued for six minutes without admitting a single second of music. The selector never ran.

It was tempting to treat each outcome as a classifier error. Animal is musically distinctive. Royal is musically distinctive. Thatch and Big Modern begin with recognizable riffs. Perhaps the features were not good enough, the candidate generator was too weak, or the temporal model was holding the wrong state.

Those were real issues, but they assumed that the model was hearing what I heard through the speakers.

The deployed path was longer:

1. Chrome decoded the Nugs stream.
2. macOS routed the output through BlackHole while maintaining a monitor path to the speakers.
3. A capture process read the virtual audio device.
4. The runtime segmented the stream and resampled it.
5. A music gate decided whether enough continuous music existed.
6. Several acoustic model families generated candidates.
7. A temporal controller decided whether a candidate could replace the current song.
8. The application persisted state and rendered the interface.

The offline experiment began around step six. The product began at step one.

## What the browser audio contained

We saved the captured audio from the six-minute failure and inspected the waveform. The [incident record](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0506-browser-capture-input-integrity-incident.md) reports exact-zero gaps about 0.105 seconds long at roughly 1.1-second intervals.

The signal was loud. Segment timestamps advanced normally. The dashboard therefore reported a healthy session. But the music gate saw a discontinuous input unlike the archive recording on which it had been developed.

The difference was large enough to measure directly. The incident summary reported a median music-gate output of about 0.08 across the browser run's scored windows and about 1.00 when the same Animal performance was decoded from the read-only archive file.

An early suspect was the asynchronous resampler. Removing it eliminated the periodic digital-zero pattern, but nominal ten-second capture segments still contained only 7.3 to 8.9 seconds of decoded audio. A direct tone test localized the remaining loss to FFmpeg's AVFoundation input path.

The timestamps had been measuring files being declared, not samples successfully transported.

This is why the browser belonged in the experiment. An archive decoder could not expose a macOS capture failure. A model benchmark could not tell us that the input contained holes. The product was producing apparent model behavior from invalid audio.

## Making input validity visible

We replaced the FFmpeg capture path with a native CoreAudio helper. It selected the named device directly, downmixed to mono, and wrote one contiguous sample stream into the segmenter.

The more important change was accounting. The session began recording three quantities independently:

- source frames received from CoreAudio;
- samples finalized into the captured audio; and
- samples decoded by the recognizer.

A session passed transport integrity only when those clocks reconciled under the fixed 0.5-second tolerance. Each segment also reported RMS, peak, exact-zero fraction, and internal dropout counts. Startup failed if the selected device produced no callbacks. A completed capture with missing recognizer samples became an invalid run rather than a mysterious model miss.

On a valid replay of Animal, 246.000 source seconds became 245.930667 finalized and decoded seconds. The timebase passed. The opening crowd and intro produced no song proposal. All four acoustic views eventually ranked Animal first, and the system locked it after seventy admitted music seconds.

Seventy seconds was slower than I wanted, but it was a real latency measured from audio the model had actually received.

## A valid input exposed a policy failure

That valid capture exposed the next problem. One music-gate observation fell below threshold during continuous Animal. The runtime immediately cleared its accumulated audio window, delaying the lock. We copied the exact captured bytes and replayed them with one policy change: an uncertain gate window waited for the next observation instead of immediately resetting state. Nothing about the input or model changed. Animal locked forty seconds earlier.

Exact-byte replay let us ask whether a policy change improved behavior without accidentally changing the browser input at the same time.

## One song was not enough

The next diagnostic started at the beginning of Dr. Darkness and allowed the Nugs player to advance naturally into Drive.

The interface refreshed candidate telemetry throughout Dr. Darkness, including brief drift toward other songs, while the publication state held the correct title. When Drive began, the acoustic sources changed before the temporal controller authorized a transition. At 410 seconds, all four sources ranked Drive first and the transition model switched the held state. This was the behavior we wanted: frequently changing evidence, conservative displayed state.

The run also revealed that capture wrote 48 segments while the recognizer exited after 47. The missing tail was only 1.284 seconds, and it did not change the two song decisions when replayed. It still made the original session invalid under the fixed transport rule. Shutdown was part of the input path too.

Longer tests exposed errors that no isolated track could express. A system can name every segment correctly and still perform badly if it changes songs in the middle of a jam, refuses to switch across a segue, carries state through a set break, or assigns a debut cover to the nearest known title.

The evaluation surface had to expand from top one to a product record:

- precision and coverage of the first stable lock;
- time from admitted music onset to first correct evidence and stable state;
- correct, incorrect, and abstained held-state time;
- false stable switches and their dwell;
- behavior during nonmusic and unknown material;
- transition behavior across pauses and segues;
- capture validity; and
- separately observed UI and publication delivery.

These metrics do not replace controlled corpus evaluation. They answer questions the corpus experiment never asked.

## The night the narrow question worked

On August 13, Goose played in San Diego. SetScope listened while the show was happening and emitted correct blind song identities from audio that had not existed when the system was built.

This was the smallest prospective test we had wanted from the beginning: can the runtime emit correct blind song identities during a genuinely new show?

It could.

The [saved run and operator record](https://github.com/cmeiklejohn/zabriskie/blob/f52b45f47e0884d1504474d276e4230e2e0f2acd/tools/audio_detection/cloud/v0521-2026-08-13-live-show-audit.md) preserve the live trace. The later run ledger records that source date and setlist context were not used. We did not preserve one canonical pre-music receipt containing every candidate hash, audio route, mode, publication state, and start field later required by the full protocol.

The runtime also missed performances and changed songs incorrectly. One set failed the capture-timebase check, and we lacked separate receipts for what reached the interface or public chat. The run did not supply a formal whole-show accuracy estimate or proof of viewer-visible delivery.

The next question was how to let an autonomous research program keep improving those pieces without allowing one useful field test, one large engineering replay, or one polished percentage to become a claim it could not support.
