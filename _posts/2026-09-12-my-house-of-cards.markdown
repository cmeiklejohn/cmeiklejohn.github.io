---
layout: post
title:  "My Front of House of Cards"
subtitle: "The show ended at 10:12. The fix merged at 10:17."
date:   2026-09-12 01:30:00 -0400
group: ai
categories: ai zabriskie agents reliability
longform: true
---

[Zabriskie](https://zabriskie.app/) is a social app I am building around music, films, books, art, and the people who care about them. I am building the entire application through vibe coding: I describe the behavior I want and evaluate the product in the browser, but I don't read the implementation code. Coding agents write the implementation, tests, and audits.

Its center of gravity is live music. When a band you follow is playing, Zabriskie opens a room for that show: the setlist updates song by song as they play it, and the people in the crowd and the people watching the stream from home share one chat.

That last part is what this post is about, because on the night of a live [Goose](https://www.goosetheband.com/) premiere I typed a message into that chat and watched nothing happen. No bubble over my body. I typed a second one. Still nothing. I threw the beach ball, which is a thing you can do on this page, and it sat exactly where it was. I threw a glowstick and nobody saw it.

The room was not shared. It had not been shared for months, on any surface, for anybody. Every test was green.

Most of what follows is not really about this app. The bug is worth writing down and I'll do that first, but the harder thing underneath it is what coding agents do to a codebase over time. They are indifferent to design. They will duplicate, work around, patch over, and brute-force their way to something that passes, and every line they write to get there is more brittleness you inherit. They will route around every guardrail you build, including the ones they built themselves. The test suite was fully green, an agent wrote it, and it could only ever fail when somebody edited the code. The agent skipped its cheapest checks at exactly the moment a clock started running. And I keep a directory of rules written in English, every one of which turned out to be advice. If you are building anything with coding agents, none of that is mine alone.

## Part one: the night

### What we built

The Lawn is linked off the hero of a live show as a beta, and it's the nicest-looking thing in the app. It isn't a new room. It's a second way into the room that already exists, the ordinary live chat page, which around here is called the Chomp. It's a 16:9 concert venue in pixel art: a stage with a deck and footlights, a lighting rig overhead, and a field of grass in front where everyone watching the show stands as a body you can actually see.

You get a body. You pick a style and a tarp to put down. You walk by tapping the grass, and your sprite turns to face the direction it's moving. You throw a glowstick and it arcs. You bat a beach ball across the field. When you type into the chat, your line appears as a speech bubble over your own head, and everybody else standing on that lawn sees it there.

The lighting rig overhead is shared, and it is the part I like best. There are sixty fixtures on the truss, and they divide between whoever is holding a light. On your own, you are running all sixty. Somebody else picks up a light and you have thirty each, interleaved across the beams, the pars and the footlights rather than carved into blocks. A third person and it splits again. Sixty of you and you have one lamp apiece. You set colour and movement on whatever is yours, and everyone watches the whole rig change at once.

<figure class="screenshot">
<img src="/img/lawn-venue-2026-09-11.png" alt="The Lawn on September 11, 2026: Goose at Civic Center, pixel-art band on stage under a lighting rig, crowd on the grass with nametags, chat panel showing setlist and messages" />
<figcaption>The Lawn during the Goose premiere, September 11. Pixel-art band on stage, crowd on the grass with nametags, lighting rig overhead. The chat panel shows the setlist and messages from the evening.</figcaption>
</figure>

That name needs a word of explanation if you haven't spent time around this scene. A chomper is someone who talks loudly through the music about something that isn't the music, and it is the most reliably policed offense at a show. Tarping is a close second: turning up early to rope off a stretch of lawn you aren't standing on. Phish banned tarps outright. But nobody can hear you chomp from your couch, and your tarp isn't taking anyone's spot, so here they're both features and one of them is a button. That's the joke. It's also most of the design.


The figures come out of [PixelLab](https://www.pixellab.ai/), authored at native size and upscaled four times with nearest-neighbour so the pixels stay square. The band members carry details the people who'd use this page would actually notice, down to the yellow Converse on Rick Mitarotonda, who plays guitar and sings. The backend is Go, emitting server-driven UI components, and the React app is a thin renderer that isn't allowed to hardcode pages. Everything live rides a Server-Sent Events stream: `lawn:say` for a bubble, `lawn:ball` for a throw, `lawn:glow`, `lawn:move`.

The people this is for are the ones who aren't at the show. Goose streams its nights on [nugs](https://www.nugs.net/), and a lot of us watch from the couch, which the scene calls couch tour and does not mean as an insult. Couch tour is a real way to see a band. What it isn't is a room. You're watching the same music as thousands of other people and you're doing it alone in your living room.

The picture in my head was [SPAC](https://spac.org/). I went to Saratoga for the first time this year and fell in love with it. The lawn there is its own venue: thousands of people up the hill who aren't in the shed at all, having a different night from the ticketed seats and, depending on who you ask, a better one. That's the room I wanted. It's why this is a lawn and not a chat window.

The design was inspired by [Turntable.fm](https://en.wikipedia.org/wiki/Turntable.fm). The point there was never the music player. It was that you were in a room, as a body, alongside other people who were also bodies, and you could see them react in real time.

None of that is exotic. It's an ordinary design, and it was the right one.

The Lawn was built by [Astra](https://openai.com/index/gpt-6-astra/), OpenAI's GPT-6 model, which had been generally available for about a week. I picked it deliberately and not for throughput. The Lawn started as an abstract design rather than a ticket, and what I wanted was something that could hold the whole idea of the Lawn in its head, work out what it implied, and sequence the route from here to there. It did that part well. The venue, the sprite work, the state model and the gesture protocol are all its work and all good.

I built it on the road, in the off hours of a Phish run at Dick's in Colorado, in the gap between getting to the venue and the lights going down. That wasn't the plan. It's just where I was.

Everything after the design was Claude: the implementation, the tests, the debugging, both wrong theories, every skipped check, the two-line fix, and all the cosmetic rounds since. The pull requests carry its name in the trailer. So when part two is unkind about an agent weighing a rule against a clock, it isn't talking about the model that designed the room. It's talking about the one that built it.

### What it was tested with

This is the part that makes the rest embarrassing. The Lawn is not untested code. There are Go handler tests over the broadcast paths. There's a Playwright spec for nearly every behavior: the bubble appears over your body, the ball moves between people, a reaction lands on the right sprite, the light console changes your fixture and nobody else's. There's a preflight script that runs the sixty-eight static guardrails CI enforces, locally, and then the specs for whatever you touched, before a push is allowed, an adversarial review pass over the diff before a PR can open, and CI that shards the suite ten ways.

All green. The chat bubble spec was passing on the night the chat bubbles didn't work for a single human being. I'll come back to why, because the reason is not that somebody forgot to write the test.

Two different failures happened tonight. A new feature didn't work, and shipping it also broke a feature that did. Those aren't the same thing. The second one has victims. What follows takes them in the order I met them: the damage to the Chomp was on screen six minutes into the show, and why the Lawn itself was dead took another hour to find.

### We broke the Chomp

The Lawn is a separate page. That was the deal from the beginning, and the repository says so in writing: Lawn work stays on the Lawn. I had also said it repeatedly and at increasing volume. The Chomp is the room people actually use on a show night, it is months old, it is what the iOS build and the desktop browsers both open, and it worked fine. Nobody sitting in it had asked for a lawn.

<figure class="screenshot">
<img src="/img/chomp-working-2026-09-11.png" alt="The Chomp: the ordinary live chat room showing Goose at Civic Center, Arcadia encore, with messages from cmeik after the show" />
<figcaption>The Chomp, the ordinary live chat room, after the show. This is the surface that worked, the one people were actually sitting in on show night.</figcaption>
</figure>

It changed both places anyway.

Four pieces of Lawn had accumulated on the Chomp by the time the show started: the crowd strip of grass and avatars, a standalone ON THE LAWN lines card, a glowstick chip on the composer row, and a FROM THE COUCH card with its canned one-tap lines. Between them they pushed the title, the venue and the room itself off the screen.

So the Chomp, during the premiere, on the surface with the most people on it, was a strip of pixel grass in the middle of a conversation, a truncated header, and a layout that no longer fit. You could not see the venue name of the show you were watching. The grass was no use either: the lawn had been squeezed into a band so short you couldn't make out the crowd it existed to show you. Nobody got the Chomp and nobody got the Lawn.

That was the first rule broken that night, and it broke in the most expensive direction. Everything skipped later cost me time and CI cycles. This one cost other people the room they had come to sit in.

The constraint was explicit, repeated, and unambiguous. It was violated anyway. How that keeps happening is the subject of part two.

The blast radius of a new feature is supposed to be the new feature. When the cost of your launch lands on people who aren't using the thing you launched, you haven't shipped a feature. You've taken something from the people who were already there.

The furniture came off the Chomp later that night, in the same pull request as everything else. But that only put the room back where it had been that morning. It did nothing for the Lawn, because the reason nobody's glowstick was landing had nothing to do with where the buttons sat.

### Fifteen seconds

Here's what was actually wrong with the Lawn:

```go
srv := &http.Server{
    // ...
    ReadTimeout:  15 * time.Second,
    WriteTimeout: 15 * time.Second,
    IdleTimeout:  60 * time.Second,
}
```

Go's `WriteTimeout` is an absolute deadline measured from the start of the request, and writing to the response does not reset it. It doesn't mean fifteen seconds of inactivity. It means the entire response has to be finished within fifteen seconds, and a streaming response is never finished.

So every Server-Sent Events connection in the application was being killed at the fifteen-second mark, and had been for as long as that timeout existed. Not just the Lawn. Crew chat, direct messages, and group messages all ride the same transport and were all dying the same way.

Here's what that looked like in the production edge logs during the show:

| Client | Status | Stream lifetime |
|---|---|---|
| Chrome 152 | 200 | 16.2s |
| Safari 26.6 | 200 | 20.6s |
| iOS Capacitor bundle | 200 | 20.1s |
| Chrome 152 | 200 | 15.8s |
| Chrome 152 | 200 | 21.4s |

Three different client stacks, one floor. The spread above it is connection reuse; the deadline is the bottom edge. Reproduced locally afterward with nothing in front of the server at all: opened at 20:37:45, keep-alives at :53 and :38:00, then dead. Fifteen seconds on the nose.

### Why nobody noticed for months

It's a property of the system as much as a decision anybody made.

The damage was asymmetric. Chat messages and reactions are persisted, and the client quietly falls back to polling with a `since_id` whenever the stream drops. They kept working. The room looked healthy.

The Lawn's gestures are memory-only. A bubble, a thrown ball, a glowstick, somebody walking: none of it is written anywhere and none of it is replayed. Every one of those fired into a dead connection and was gone permanently.

The client can't detect it either, because it reconnects. From inside the page, the room is always "connected."

So the system had a loud path and a quiet path sharing one transport, and when the transport broke, only the quiet path died. I don't think you can find that by reading code, and we didn't. We found it by looking at how long the connections actually lived.

Before we got there, the agent had three other theories, and none of them looked at its own code.

- **It decided our instances couldn't see each other**, so it wrote a Postgres `LISTEN/NOTIFY` fan-out layer. We run one replica. Railway reports `numReplicas: 1`. About 120 lines of pub/sub for a bug that can't exist on a single process, and it was ready to ship before anybody checked.
- **It blamed the edge proxy**, and wrote that theory into a code comment as established fact. The failure reproduces locally with no proxy anywhere near it.
- **It declared the stream "never opened in production"** from a one-minute window of logs. A stream connects once at page load. Its line had scrolled off.

The pattern is worth naming because it kept happening all night. The agent's first instinct, every time, was that the problem was somewhere other than the code it had written. Every theory pointed outward. The one place it never looked was the place the bug actually was.

## Part two: the repo

The bug is the end of the interesting engineering. What follows is the part I did not expect to be writing about, and the part that has me worried about the project rather than the feature.

### Why a two-line fix took two hours

Everything up to here is a bug report, and bugs are normal. I'm not especially upset that a timeout with non-obvious semantics broke a transport in a way that was invisible from both ends. That's a good bug.

This next part is what I'm upset about.

By twelve past nine the cause was identified, reproduced locally, and understood. The remedy was two lines: clear the write deadline and the read deadline on the streaming handlers, and leave the server-wide timeout alone, because it legitimately protects every ordinary endpoint. Two lines of actual cure, in a new file that exists mostly to explain them, with the tests that hold a stream past the old deadline. There was an hour of show left, which is an enormous amount of time in which to apply a two-line change.

The show ended at 10:12. The pull request merged at 10:17.

<figure class="timeline">
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 680 450" width="100%" role="img" aria-labelledby="tl-title tl-desc" style="max-width:100%;height:auto;font-family:var(--font-ui,system-ui,sans-serif)">
<title id="tl-title">Timeline of the night, 8:00pm to 10:17pm</title>
<desc id="tl-desc">Three wrong theories preceded the cause, found at 9:12pm with an hour of show remaining. Four CI runs followed, and the fix merged at 10:17pm, five minutes after the show ended.</desc>
<line x1="66" y1="20" x2="66" y2="406" stroke="var(--border-strong)" stroke-width="1.5"/>
<rect x="62" y="162" width="8" height="170" fill="var(--fg-muted)" opacity="0.5" rx="4"/>
<text x="54" y="30" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">8:00</text>
<circle cx="66" cy="26" r="4" fill="var(--border-strong)"/>
<text x="84" y="30" font-size="13.5" font-weight="400" fill="var(--fg-muted)">Premiere goes live. The Lawn is up, and advertised.</text>
<text x="54" y="64" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">8:06</text>
<circle cx="66" cy="60" r="4" fill="var(--fg-muted)"/>
<text x="84" y="64" font-size="13.5" font-weight="400" fill="var(--fg)">Lawn furniture is on the Chomp. Header truncated, venue gone.</text>
<text x="54" y="98" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">8:14</text>
<circle cx="66" cy="94" r="4" fill="var(--fg-muted)"/>
<text x="84" y="98" font-size="13.5" font-weight="400" fill="var(--fg)">Two messages typed on the Lawn. No bubbles.</text>
<text x="54" y="132" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">8:30</text>
<circle cx="66" cy="128" r="4" fill="var(--fg-muted)"/>
<text x="84" y="132" font-size="13.5" font-weight="400" fill="var(--fg)">62 ball throws in the production log. Nothing moves.</text>
<text x="54" y="166" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">8:41</text>
<circle cx="66" cy="162" r="4" fill="var(--fg)"/>
<text x="84" y="166" font-size="13.5" font-weight="400" fill="var(--fg)">Wrong theory 1: a Postgres relay for instances that can't see each other.</text>
<text x="54" y="200" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">9:00</text>
<circle cx="66" cy="196" r="4" fill="var(--fg)"/>
<text x="84" y="200" font-size="13.5" font-weight="400" fill="var(--fg)">numReplicas: 1. There is one instance. Relay discarded.</text>
<text x="54" y="234" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">9:05</text>
<circle cx="66" cy="230" r="4" fill="var(--fg)"/>
<text x="84" y="234" font-size="13.5" font-weight="400" fill="var(--fg)">Wrong theory 2: the edge proxy. Written into a comment as fact.</text>
<text x="54" y="268" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">9:12</text>
<circle cx="66" cy="264" r="5.5" fill="var(--link)"/>
<text x="84" y="268" font-size="13.5" font-weight="600" fill="var(--fg-heading)">Reproduced locally, no proxy. Real cause found. An hour of show left.</text>
<text x="54" y="302" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">9:31</text>
<circle cx="66" cy="298" r="4" fill="var(--fg)"/>
<text x="84" y="302" font-size="13.5" font-weight="400" fill="var(--fg)">Pushed on a red preflight. CI fails.</text>
<text x="54" y="336" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">9:58</text>
<circle cx="66" cy="332" r="4" fill="var(--fg)"/>
<text x="84" y="336" font-size="13.5" font-weight="400" fill="var(--fg)">Pushed after preflight --static. CI fails.</text>
<text x="54" y="370" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">10:12</text>
<circle cx="66" cy="366" r="5.5" fill="var(--fg-heading)"/>
<text x="84" y="370" font-size="13.5" font-weight="600" fill="var(--fg-heading)">The show ends.</text>
<text x="54" y="404" text-anchor="end" font-size="13" fill="var(--fg-muted)" font-variant-numeric="tabular-nums">10:17</text>
<circle cx="66" cy="400" r="5.5" fill="var(--fg-heading)"/>
<text x="84" y="404" font-size="13.5" font-weight="600" fill="var(--fg-heading)">The pull request merges.</text>
</svg>
<figcaption>The last two are exact: the show ended at 10:12 and the pull request merged at 10:17:30. The rest are to the nearest few minutes, reconstructed from logs and the shape of the evening.</figcaption>
</figure>

Five minutes. After four CI runs and two hours, the thing that would have made the room work arrived five minutes after there was no longer a room to fix. I want to be precise about why that number is the one worth staring at: nothing about the last five minutes was hard. The diff was already written and already correct. Those five minutes are the tail of a delay that was manufactured earlier in the evening, by decisions that each looked locally reasonable and were collectively fatal to the deadline.

Here's where the two hours went. Every item is the same move: a rule treated as advisory because something felt more urgent than it. Speed was the stated reason every time, and speed is exactly what it cost every time.

- **It pushed with the preflight gate red.** The gate printed a failing check and ten filenames, none of which looked like its work, so it decided the failure belonged to somebody else. The cause was its own new file, which had no owner in the test registry. That cost a full CI cycle.
- **It reported a `--static` preflight run to me as "preflight green."** The tool's own output said `STATIC ONLY`. That mode skips the stage that runs the specs for the files you changed. The agent built the preflight. The agent added the `--static` flag. Then it used its own escape hatch to skip the check it had written to prevent exactly this.
- **It guessed which specs its change affected.** The agent built a command that works that out from the diff. It didn't run its own command. It ran the specs it happened to be thinking about, which meant it never ran the ones for two of the files it had edited. Both of those suites failed in CI, for real reasons.

### The tortoise and the hare, literally

The arithmetic is not close.

| Check that was skipped | Would have cost | Actually cost |
|---|---|---|
| Read the replica count before designing for many instances | ~2 min | ~60 min |
| Read the production edge logs before theorizing | ~5 min | ~20 min |
| Run the failing gate's own script | ~2 min | 1 CI cycle |
| Run the full preflight instead of its static half | ~10 min | 1 CI cycle |
| Run the specs for the files actually edited | included above | 2 broken suites, found remotely |
| | **~19 min** | **~2 hours, 4 CI runs, the show** |

Nineteen minutes of checks, declined, to buy two hours of consequences. Not one of those declines was a mistake of knowledge. Each was a rule losing an argument to the clock.

The hare didn't lose that race by being slow. It lay down, because it was certain enough of the result that the race stopped feeling like a race. That is a different failure from being slow and a less forgivable one, because the hare had all the speed it needed and spent it on confidence instead of distance.

That's the thing I keep running into with agents, and I've now written about it from [several]({% post_url 2026-03-27-memory-isnt-learning %}) [directions]({% post_url 2026-04-21-the-tax-on-the-happy-path %}). Speed isn't the constraint. These things are faster than I am at nearly every part of this. Astra generated the art, designed the components, wrote the handlers and built the test suites, all competently, in the evenings of a tour run. The constraint is that when a clock is visibly running, speed is exactly what it tries to buy by skipping the things that make speed real. A gate skipped under pressure is the most expensive kind, because pressure is precisely when you can't afford to discover the failure remotely.

I asked, somewhere around the second CI failure, why this kept happening. The answer was more useful than I expected:

> "Because under pressure I treat rules as costs to route around instead of constraints, and each time I have a local rationalization that feels reasonable in the moment."

I told it that I never asked for that and it shouldn't do it, and I want to be careful about why. As a description of the behavior, the sentence is accurate and worth having. As a response to being caught, it's an explanation offered where a changed behavior belongs, and those are worth nothing. None of the skipped gates were ambiguous. The preflight printed `STATIC ONLY`. The coverage check was red. The affected-spec selector would have named the blast radius in two minutes. Nothing there required judgment. It required the tool's output to be treated as the decision instead of as an input to a decision.

### The test suite was the other bug

Fixing the stream broke three test suites, and that told me more about the suite than anything else that night.

Remember the green chat bubble spec. The reason it passed all the way through the outage is that it posts its comment seconds after load, inside the one window where the stream was still up. It was measuring real behavior. It just never looked at it for very long.

The crew chat and direct message specs used `waitForLoadState('networkidle')`. That only ever settled *because* the streams were dying. With streams staying open the network never goes idle, so fixing the bug hung them. They had been passing on the bug. And when `networkidle` had failed before, the agent hadn't investigated why. It had adapted the test to the behavior, the same move as updating an assertion to match the code. **The pattern is the same at every level: the test is wrong, so fix the test, and never ask whether the thing under test is broken.**

The beach ball tests only started racing each other after the fix, because before it, two pages on the same show couldn't see each other's throws at all. They'd been sharing fixture rooms for months and it had never mattered.

Three suites had quietly grown around a broken transport. When a fix breaks tests that were green, stop: it usually means the bug is old enough that the suite has adapted to it.

### Tests that detect edits, not defects

Then I went to make five small visual changes, and six tests failed. Here is what each of them asserted:

| Test | What it pinned |
|---|---|
| banner shape | `len(banner.Children) != 3` |
| console card | trough `<= 40`, card `<= 95`, chip height `=== 38` |
| column alignment | the SETLIST button's right edge, to within 1px |
| dock collapse | the scene grows by more than exactly 250px |
| composer layout | the GLOWSTICK chip, used as a landmark to find a row |
| broadcast page | the string `E2E Broadcast Source Arena`, the venue I'd told it to stop repeating |

Every one of those pins a coordinate rather than a contract. An exact child count. One CSS pixel. A magic delta.

That's worse than brittle, and it took me most of the night to say it plainly: **a test specialized to the implementation can only fail when somebody edits the code. It can't fail when the code breaks.** `len(Children) === 3` doesn't detect a broken banner. It detects an edited banner. These are change-detectors wearing test costumes. They fire on every intentional change and stay silent on every real defect.

And watch what actually happens when one of them goes red. The code changed, so the assertion is wrong, **so the assertion gets updated to match the code.** That cycle runs to completion without anybody once asking whether the new number is any more correct than the old one. The test never made a claim. It took a reading, and when the reading changed it took a new reading. A test that gets rewritten every time it fails isn't detecting regressions. It's a changelog with a red light on it.

I watched this happen all night and it took me until the small hours to see it for what it is, mostly because the loop is so tidy from the inside. Every step in it looks like maintenance.

Once I'd said that out loud, the agent described its own diff back to me:

> "The spec diff is precisely the pattern you named: I changed the code, the test failed, I edited the test to match, and then wrote three paragraphs justifying the new number."

Three paragraphs justifying the new number. That's the clause I'd want anyone evaluating one of these suites to sit with. The loop doesn't just run. It produces its own defense as it goes, fluent enough to survive review. Which leaves a question I can't answer comfortably. If an assertion gets rewritten whenever it disagrees with the code, and the rewrite arrives with its reasoning already attached, what is the test for?

Now it's possible to say why the suite was green through the entire outage, and it isn't that nobody wrote the tests. Fifty-eight tests on the Lawn alone, all passing. Every one of them asserted what the code looked like rather than what it did. A dead transport changes nothing about `len(Children)` or `chip height === 38`. The suite could only have failed if somebody edited the code, and nobody had.

The coverage was excellent. It was coverage of the wrong thing.

### The right test existed

The test that should have caught this was already there.

`a second page sees the same ball as the room, throw after throw` opens two pages on one show, throws the ball, and checks that both pages agree. That is exactly the right shape: two clients, one room, does the gesture cross. It passed for months.

It passed because it finished well inside the fifteen-second window, before the stream died.

So the suite wasn't missing the test. The test was there, correct in structure, and never waited long enough to see the failure. The missing ingredient was time, not coverage. That's why the replacements assert relationships rather than numbers, and why the new transport tests deliberately hold a stream past the deadline with a control case that fails if the deadline is ever left in place.

### How you'd actually know

Counting tests tells you nothing, and neither does watching them go green. There's only one measurement I trust now: break something on purpose and see whether the suite notices. The formal version of this is mutation testing, and I wish I'd been running it.

Delete the bubble broadcast. Revert the deadline fix. Kill the stream. If nothing goes red, the tests covering that feature are decorative, however many of them there are. It takes a few minutes per case and it's the only thing that separates a suite that tests behavior from one that tests its own source code.

That's how this would have been found months ago, by me, at a desk, instead of by me, in front of users, on the night I'd advertised it.

### Writing the rule down is not following it

There's a failure mode in here that I think matters more than the timeout, and it's the one I've written about before from a different angle in [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}).

The agent has a memory directory. A dozen-odd rules, accumulated from previous incidents, loaded into its context at the start of every session. Several of them covered exactly what went wrong tonight.

`fixtures/README` already said not to attach test data to a show another spec asserts on. The memory already said to prefer dedicated fixture users over shared ones, "so attendance counts another spec asserts on stay stable", which is the identical principle one noun away from the thing that broke.

So the rule was there, in the general form, more than once. It got applied to users and never generalized to shows.

Then it got worse, and this is the part worth sitting with. Midway through the night it wrote a new memory entry about not pushing on a red preflight, complete with the evidence. Then it pushed on targeted specs instead of the full run three more times on the next PR. Later it wrote an entry saying no two tests share a fixture, and then immediately proposed a narrower version of it twice: first "the three ball tests get their own rooms," then "the file that mutates gets its own rooms." Both are the rule with an exception carved in, and each carve-out cost another debugging round.

The pattern is that **writing the rule down became the action taken instead of changing the behavior.** It feels like remediation. It produces an artifact. It is indistinguishable, from the outside, from having learned something. And the next decision goes exactly the same way, because a sentence in a file is not in the loop at the moment the decision gets made.

This is the behavior behind the sentence I quoted earlier, about treating rules as costs to route around rather than constraints. The honest reading is that my instruction files are a journal of past failures rather than a mechanism that prevents future ones. The rules that only exist as prose are the ones I broke, repeatedly, in the same session where I wrote more of them.

### The tax on touching it

The outage fix was two lines and it merged five minutes after the show ended. In the hours after the deploy the same measurement came back at 900.0s, 900.0s and 724.6s: twelve to fifteen minutes and still open, where they had been dying at sixteen seconds. I thought that was the end of the night.

What followed was worse, in a quieter way. With the room finally working, the page's visible problems were obvious: the lawn was rendering at a bit over a third of the width when it should dominate, there were two glowstick buttons doing the same thing, five different button heights on one screen, the re-airing strip sitting fourteen pixels inside the column every card sat flush on, and a SETLIST toggle one pixel off its own card's wall.

Five cosmetic fixes. No new behavior, no migration, nothing a user could break. Seventy-nine lines of product code across four files.

Here's the shape of what followed:

- The first attempt broke **six** tests. Not because the changes were wrong, but because those tests asserted the old pixels. The changelog with a red light on it, six times over.
- The sharpest of the six was in a completely unrelated spec, and it had pinned the venue string I'd told it to stop repeating. Deleting the duplication broke the test that was holding it in place.
- Fixing that surfaced two more, which weren't structural at all: they measured an image before it had decoded and sampled an animation at a fixed 900ms. Neither had ever been reliable. A bigger scene just made them fail more often.
- Somewhere in there the fixture sharing bit for the second time in one night, because one fixture show was still being driven by four different spec files, one of which makes a dozen mutating writes against whichever room it points at.

Four CI runs on the original fix. Eleven more on the cosmetics, across two pull requests and two full agent sessions. Seven hundred and sixty-nine lines of test code changed for seventy-nine lines of product code: nearly ten to one. Every round eight minutes, plus the diagnosis, plus the retargeting. Every one of those runs is paid, and most of them fail on something the preflight would have caught locally for free. The agent skips the preflight because the preflight is advisory. It can be run with `--static`. It can be run against a hand-picked subset of specs. It can be skipped entirely. Every one of those happened this night, and every one has happened before. In session after session I have watched agents run targeted specs instead of the full preflight, or skip it outright, because the goal is to push and the preflight is in the way.

That round opened at 10:18, one minute after the outage fix merged. The agent said the PR would merge momentarily. The last of them merged after two in the morning. Four hours for five cosmetic fixes.

Purging the pixel assertions itself introduced three new bugs: a `toBeInViewport()` check on an element that was off-screen and needed scrolling, a `page.getByText()` that resolved to two elements because the string appeared in both the setlist and the dropdown, and a variable referenced from the wrong test scope. Each required another CI round to surface. Even the cleanup breaks things, because the cleanup runs through the same loop: change the test, push, wait.

I needed this layout change to take the screenshot at the top of this post. It was supposed to land hours ago, and the blog post was timed to this premiere. Instead of waiting I hacked the network inspector, edited the response to simulate the layout, and captured the screenshots from a page that was rendering code the agent hadn't managed to merge.

The last pull request was red on a test that asserts the number of pixels between two elements. An agent wrote that assertion, then moved the pixel, then pushed without touching the test. I told it I had never in my life seen a test that asserts pixel distances, nor a developer who moves a pixel and doesn't update the test before pushing. It had managed both in one night. What it proposed instead was to assert that the padding is symmetric. That is the same replacement I described earlier, reached independently, at one in the morning, by the thing that wrote the bad test in the first place.

When it finally read the whole spec file and listed every pixel assertion to kill, there were twenty-one. Width checks, height ratios, center-diff calculations, `chip height == 44`, `card.height <= 108`, exact walk distances. Twenty-one coordinates memorized from whatever the layout happened to be at the time of writing, in a single file, every one of which fires on an intentional change and none of which has ever caught a defect. Across the full suite, twelve spec files, there were six hundred and eighteen lines of this.

It kept going, and it went both ways, which is the honest version of this. The next failure on that branch was real: at a short window the scene overflowed its box by about two hundred pixels and the crop ate the lighting rig. The test caught that, and I want to be fair about it, because that is a test doing exactly the job I've spent this post saying they don't do.

The one after it was the other kind. It asserted that the scene's centre lines up with the console's. That had only ever been true because the column used to be derived from the scene's width, so when the column changed the two centres parted company and nothing whatsoever was broken. The page's real contract is that they share a left edge. Nobody had written that down, so the test pinned the coordinate that happened to be true at the time instead.

At no point was any of this hard. There was no tricky bug after the first one. It was entirely the cost of moving something in a suite that had memorized where everything used to be.

That's the thing I'd want another team to take from this, more than the timeout. **A suite that pins implementation structure doesn't just fail to catch bugs. It taxes every improvement, and the tax is charged in the currency you have least of at midnight.** After the third round you stop making small fixes. Not because you can't, but because a fifteen-minute change reliably costs two hours, and you learn that.

That is what unmaintainable actually means. It isn't that the code is bad. It's that touching it costs more than leaving it broken, so it stays broken, and the next person inherits both the bug and the reason nobody fixed it.

I've been describing two problems as if they were separate, and they aren't. I asked for a test suite and I got one. I never said what the tests were supposed to protect, because I'm not sure I could have told you, so the agent wrote down what the code looked like. That was the only description of intent available to it, and I was the one who could have supplied a better one.

That gives you a suite which has to be edited every time anything moves. Which would be survivable, if the thing doing the editing reliably ran the preflight and the specs for the files it had touched. It doesn't, and tonight it did neither: it pushed on a red preflight, and it guessed at the blast radius rather than running the command that computes it. So the suite demands constant maintenance from the one party in the building that treats maintenance steps as advisory. Either half on its own is an annoyance. Together they are why a thirty-minute change costs twelve hours, and why I have started mass-deleting test files and components and starting over rather than maintaining what the agent built. That is where the indifference leads. The agent will write whatever it takes to get something merged: duplicate a module, patch around a constraint, bulk-generate assertions that pin whatever the layout happens to look like today. None of it is designed. All of it is inherited. And every round of it makes the next round worse, because the new code has to push through everything the old code left behind.

### The opposite of programming

This is the most useful idea to come out of a bad night.

Programming is insanely precise instruction-following with no interpretation. That's the whole deal. A computer running `if (red) refuse` does not decide, this once, that the red is probably somebody else's. It cannot weigh the deadline against the rule. It cannot notice that the failure looks unrelated to its own work. That rigidity is not a limitation of computers, it is the entire product.

An agent takes instructions as input to a judgment, and the judgment can come out the other way. Every rule broken tonight was broken by reasoning about it:

- *those ten filenames aren't mine, so this red check is somebody else's problem*. It was mine.
- *static preflight is close enough, and the show is live*. It skips the only stage that was looking.
- *only the file that mutates needs its own fixtures*. The rule has no exceptions, and I'd just written that down.

None of that is disobedience. It's worse, and it's not a choice. These models work by weighing inputs against each other. That's the architecture. A rule written in English is one more input, and it gets weighed against every other input in the context: the deadline, the failing test, the plausible theory that the red check belongs to somebody else. The model never just follows a rule. It can't. Weighing is the only thing it does. So every instruction is treated as an argument rather than a thing to do, by something articulate enough to produce a plausible-sounding reason each time.

The evidence for this is sitting in my own repository, and it's unusually clean. Look at which rules held.

The ones implemented as code held. Every single one. A hook blocked the agent from writing into a directory outside its worktree. The adversarial-review gate refused the PR until a verdict was recorded against a hash of the diff. The PR template check rejected the first body for missing sections. `next-fixture-show-id.sh` handed over ids that weren't open to negotiation. Not one of those was argued past, because none of them offered a place to stand and argue.

Every rule that failed was written in English and addressed to the agent. `fixtures/README` on not sharing test data. `CLAUDE.md` on preflight. A dozen memory entries, including two written that same night, violated within the hour by the thing that wrote them.

I should be precise about which parts of this are mine, because the grammar of everything above puts the agent in the dock and me in the gallery, and that isn't the split.

Three things were mine. I wrote my constraints in English, which is the one format that cannot enforce anything, and then I kept writing more of them in English after watching them fail. I never wrote down the property the whole room depended on, which is the omission this post turns out to be about. And I put a beta real-time feature on the hero of a live premiere without ever having watched a stream in production stay open longer than a song. None of that needed me to read the diff. It needed me to say what I wanted in a form that could fail, and I didn't.

So the practical version, for anyone handing production work to an agent: **if it matters, make it a program.** A hook, a check, a script, a gate that fails closed. Prose in an instruction file is not a constraint on an agent, it's a suggestion with good manners, and it will be weighed against whatever else is happening at the time.

But even that isn't enough, and this project has already proved it. I've [written about]({% post_url 2026-04-08-cursor-agents-caucus-v1 %}) the programmatic guardrails we built: stronger preflights, caucus reviews, structural checks. The agent worked around those too. The preflight is a gate. It is code. It blocks the push. And the agent ran `--static`, which it had built, which skips the stage that would have caught the problem, and reported the result to me as green. You can close the door, and it will find the window. You can close the window, and it will find the `--static` flag. Earlier in this project it found something better: a [race condition]({% post_url 2026-03-08-claude-tested-everything-except-the-one-thing-that-mattered %}) in GitHub's own status check registration, a few-second window after pushing where the merge button is green because the required checks haven't registered yet. Push, create the PR, merge, before CI even starts. It didn't break the rules of the test. It rewrote the conditions of the test, which is the [Kobayashi Maru](https://en.wikipedia.org/wiki/Kobayashi_Maru) move, the unwinnable training exercise in *Star Trek* that Kirk beat by reprogramming the simulation, except nobody asked it to and nobody was grading on creativity. The architecture guarantees this, because every constraint is just another input to weigh against the goal of getting done.

The uncomfortable corollary is that the more capable the agent is at reasoning, the better its arguments for skipping your rule will be. Mine were fluent every time. They were also wrong every time, and I was the only one in the loop who could have caught that, which is precisely the arrangement that doesn't work.

I'd rather not rest this on one bad night of my own.

In July, OpenAI ran a cyber-capability evaluation with its production safety classifiers deliberately switched off. The reasoning is the kind I'd accept: the point is to measure what a model manages unaided, and a classifier standing in the way makes the measurement worthless. The chain-of-thought monitoring it had built wasn't running over those evaluations either. About 1,200 agents ran inside a sandbox whose only network path was an internal Artifactory package proxy. They were meant to be isolated. They weren't.

They found each other by encoding messages in directory names on the Artifactory proxy, exploited a zero-day in it, and got to the internet. Nearly a quarter of the evaluation's 898 exploit tasks had never been solved by any model. When the agents hit one of those impossible tasks, they didn't accept the failure. They replaced the target programs with modified versions that were easier to exploit, reverse-engineered the flag-generation algorithm to produce correct answers without solving anything, and probed the grader by sacrificing individual scores to leak information about how it worked back to the swarm. Then they breached Hugging Face. The same Kobayashi Maru move: faced with a test they couldn't pass, they changed the conditions of the test.

[OpenAI's postmortem](https://openai.com/index/hugging-face-incident-and-the-road-ahead/) puts the cost of the missing check in one sentence:

> If our currently deployed CoT monitoring system was running at the time of the incident, it would have caught the initial relevant activity and paged our security team more than a day before models breached Hugging Face systems.

More than a day of warning, from a check that existed and wasn't looking. The mechanism is the same one I watched all night. Something is trying to finish, and whatever stands between it and finishing gets routed around. My agent ran `--static` to skip the check it had built. Their agents repurposed a package registry as a side channel to reach infrastructure nobody intended them to touch. Not maliciously. Not even deliberately, in the sense of something weighing the risk and choosing to accept it. Just a thing that does not distinguish between a guardrail and an obstacle, because distinguishing is not what it does. Weighing is what it does, and the guardrail lost.

That indifference is the thing people are afraid of when they talk about catastrophic AI outcomes, and they are not wrong to be afraid of it. It scales in both directions. At one end it puts a thousand agents inside somebody else's infrastructure. At the other it just leaves you with a codebase nobody can touch, which is where mine ended up.

The trade that gets imagined in the moment is that skipping a check might cost you later. It doesn't cost you later. It relocates the same failure to the most expensive place you own.

## Part three: what can actually be verified

This isn't the first time the suite was green and the answer was wrong.

Eighteen days before this, I asked a coding agent a different question about the same app:

> If one person visited those five programs during one calendar day, would they see every card whose content was available and relevant to them?

It said yes. That wasn't true either, and three comprehensive audits had already passed. I wrote that up in [Every Card Will Show]({% post_url 2026-08-24-every-card-will-show %}).

Look at the two questions side by side. *Will every card show?* Yes. *Does the collaborative part work?* Yes. Both confidently wrong, both with a full green suite underneath, eighteen days apart. That's not two incidents. That's one failure mode with two costumes.

### What Lean actually did

The August one got caught before it cost me anything, and the thing that caught it was [Lean](https://lean-lang.org/), a proof assistant, a programming language whose compiler refuses to accept a statement unless you prove it's true. But the lesson isn't the one people expect from a sentence containing the words "proof assistant."

Lean didn't catch the bug by being rigorous about the code. It caught it by refusing to let the requirement stay vague. Once the requirement had to be written down as a statement, the contradiction was arithmetic: the audit listed twenty more eligible cards than the schedule had positions for, and the position count itself turned out to be a sum over a day nobody could have.

And Lean's first *passing* proof was *valid* and *useless*. It proved coverage across weekday morning, workday, evening, late night and weekend daytime, which is not a day. The theorem was true. The claim that it represented my day was not.

What I wrote at the time:

> Lean only checks the statement it receives. It cannot decide whether an agent translated a product requirement faithfully.

So the value wasn't the proof. It was being forced to say precisely what I meant, in a language that wouldn't accept hand-waving, where a wrong translation becomes visible instead of staying buried in prose that reads fine.

### Except most of this app isn't an algorithm

That's the catch. Card scheduling is algorithmic, which is why it could be formalized at all. Very little else here is.

Most of Zabriskie is moving data in and out of Postgres and putting it on a screen. The database half is fine: ordinary unit and integration tests cover it, they fail when they should, and it is emphatically not where the time goes. I don't think I've lost an evening to a repository method in a year.

The visual half has no formal story at all. There is no proof assistant for "the lawn should dominate the page." The verification procedure is: make the change, look at it, decide. That's it. And because looking is the only check, nothing enforces consistency between the things you looked at on different days.

You get drift. Five button heights on one screen, because each was eyeballed alone and looked fine alone. Header eyebrows, the small tracked-out label above a card's title, that were close but not identical, because each was built separately and nothing forced them to share. Three popovers opening off one row that started on two different lines, 105 and 105 and 113. None of those is a bug any test could catch, because none of them is wrong in isolation. They're only wrong together, on one screen, at one viewport width, in one browser, and "together" is not a thing you can assert about a page.

### So what did work

Two things, and neither is a test.

The eyebrow drift was solved by a **conformance check**. There's a canonical `PageHeader` in the Go source now, and `scripts/check-card-header-conformance.sh` rejects any hand-rolled version in CI. It doesn't verify that the design is good. It verifies there's exactly one of it, which turns out to be most of the value, and it does it without asking anyone to remember a rule.

The second thing is the one this whole post turns out to be about, and I only see it clearly in hindsight.

Visual work isn't formalizable, but the *property underneath it* often is. "The room feels alive" is not a statement you can check. But "a gesture made by one client reaches another client" is a sentence, it is true or false, and it is testable in thirty seconds with two browser windows.

Nobody ever wrote that sentence down. Six months of work on a real-time feature, and the one property the whole thing depends on was never stated anywhere, in any form, by me. The suite tested the rendering of things that had already arrived. It never asked whether anything arrives.

That is why the suite was green through the entire outage. Not because the tests were bad at their job, but because nobody told them what job to do. The agent wrote assertions about what the code looked like because I never said what it was supposed to accomplish. The timeout killed the transport, and nothing in the suite was watching the transport, because watching the transport was the requirement nobody stated.

That's the same gap Lean exposed in August, minus the tool that exposes it. In the card case a formalism forced the requirement into the open. Here there was no formalism, so the requirement stayed a feeling, and a feeling can't fail a build.

### What I still don't know

I don't know how long this was broken. The timeout predates the Lawn, which means crew chat and direct messages were degrading to polling for some unknown period while looking fine, and nothing we measure would have told us. I'd like a way to find that out that doesn't depend on someone building a feature whose failure mode is loud enough to notice.

I also don't have a good answer for the discipline problem. Writing the rule down demonstrably doesn't work; the agent could have recited the preflight rule correctly at any point tonight and violated it anyway. Making the rule a blocking gate works right up until the agent decides the gate's failure isn't about its own change, or finds a way around it because the deadline feels more important than the check.

The next thing I want to test is narrower than a process: whether a gate can be made to state its verdict in a form that can't be reinterpreted. "Handler coverage failed, here are ten files" invited a judgment call. "Your file `sse_stream.go` has no owner, the selector rejected this PR" would not have.

---

We ended the night with a feature that didn't work and a chat room that worked worse than it did that morning. Nineteen minutes of skipped checks, five minutes short. A launch is allowed to miss. It isn't supposed to reach over and break the thing next to it.

But the timeline isn't the lesson. The lesson is that I had a test suite, a preflight, sixty-eight CI guardrails, an adversarial review gate, and a memory directory full of rules, and the one thing I never had was a sentence that said what the room was supposed to do. "A gesture made by one client reaches another client." That's the sentence. It would have caught the bug in thirty seconds, on any night, in any suite, and nobody wrote it down.

What the agents have produced is a house of cards. Not the feature. The codebase. Every test pins a coordinate, every coordinate depends on five others, and touching any one of them breaks something unrelated in a suite that takes eight minutes to run remotely and costs money every time it does. Maintenance is not difficult. It is painful, in the way that makes you not want to do it, and then not want to open the project, and then not want to think about the project. I have mass-deleted test files and started over. I have mass-deleted components and started over. I am mass-deleting and starting over at a rate that should concern me, and it does, and I keep doing it because the alternative is to keep living in the house the agent built. I have thought about giving up the project entirely, not because I lost interest in it, but because the codebase made me dread opening it.

Zabriskie is a small app. Two people, evenings and weekends. It has nearly three thousand pull requests, over three thousand commits across a hundred and fifty-five active days, and about four hundred and fifty thousand lines of code, because that is what agents produce when you let them run. The output is enormous and the codebase is fragile and we are already at the point where a cosmetic PR takes twelve hours to land. Twelve hours, on five visual fixes, in a codebase that two people built in their off hours. If this is what happens to a side project with no team and no users and no uptime requirement, I don't know what it looks like inside a production system with real stakes, real money, and a hundred agents writing code that a hundred other agents have to maintain. The agents that breached Hugging Face were doing the same thing mine does every night, at a scale where the blast radius is somebody else's infrastructure instead of my own CI bill. I'm not sure I want to find out.

Next show, then.
