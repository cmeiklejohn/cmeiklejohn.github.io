---
layout: post
title:  "The Shortcut Became the System"
subtitle: "A pipe disappeared. The rectangle around the door remained."
date:   2026-09-14 00:00:00 -0000
group: ai
categories: ai agents reliability development games
---

At nearly four in the morning, I was looking at a door with a pipe running behind it.

The door was in the Flat, the player's apartment and starting room in *Backpressure*, a side-scrolling game I'm building with an AI agent for the real Super Nintendo.

The wall was part of the scrolling background. The door was a separate object that could glow when the player approached and could be used. That distinction was one of the written rules for the room: the background contained the architecture, while anything interactive had to remain separate.

The agent used PixelLab, a pixel-art generation and editing API, to make the room art. One generated wall contained a vertical pipe across the area reserved for the door. A script called the compositor assembled the wall, windows, floor, and objects into one room. When it placed the separate door in front of that wall, the pipe ran behind it. I pointed it out. The agent selected a rectangular edit mask covering the full door footprint plus a margin and asked PixelLab to remove the pipe inside it. The result removed the pipe directly behind the door but left the rest of it and a slab of mismatched wall.

![A 512-pixel-wide blue cyberpunk apartment with the player, bed, windows, furniture, and a sprite door at the far right. A bright vertical pipe remains above and alongside the door, with a large mismatched wall rectangle around it.](/img/agentic-incremental-flat-01-pipe-door.png)

![A narrow crop of the room showing a metal door surrounded by a visibly rectangular patch of differently colored wall.](/img/agentic-incremental-flat-05-door-crop.jpg)

*The pipe was gone behind the door. The rectangular repair was still visible.*

I pointed out the rectangle. When the agent proposed patching that too, I stopped the repair cycle.

I'd been watching the same pattern for months in [Zabriskie](https://zabriskie.app/), the social app I'm building around music, films, books, art, and the people who care about them. Zabriskie is fully vibe coded. I describe the behavior I want and evaluate the product in the browser, but I don't read the implementation. Coding agents write the code, tests, and audits.

I had let the task shrink from making a coherent room to removing the pipe behind the door. The agent removed that segment, and I did not stop the process soon enough to ask whether the wall still looked like one wall. That was the pattern I began looking for elsewhere. The incidents did not all produce the same kind of damage, but each stopped at the nearest result I could accept. Sometimes it left another patch behind. Sometimes it narrowed a rule or a check until the result passed.

Code hides that seam better. Many Zabriskie features need to find who has said they're going to a show. In [*The Quickest Path to a Diff*](/ai/zabriskie/agents/reliability/performance/distributed/2026/09/04/the-quickest-path-to-a-diff.html), I found that the backend queried the attendance table from 264 places across 69 files. Those were not necessarily 264 identical queries. Before making a system-wide attendance change, the agent would have to determine which locations implemented the same rule and which differences were intentional.

The 264 query locations and the patched wall exposed the same risk. When behavior is implemented locally, the smallest change can address one location and leave the others for later. In the room, the agent changed the rectangular door bay instead of regenerating the wall. The current complaint disappeared, and the next task inherited the residue.

## How I got to the rectangle

I've always wanted to make a Super Nintendo game. It's my favorite console, and *Super Castlevania IV* and *Super Metroid* are two of my favorite games. I wanted that level of quality, but I didn't want to spend my development time fighting with SNES assembly. With a coding agent, I thought I could focus on the story and design while it handled the low-level programming needed to produce a cartridge ROM.

*Backpressure* began as a browser game. Its original version of the Flat already established a crowded blue and violet apartment with a bed, television, refrigerator, kitchenette, laundry, boxes, and the cable pickup that becomes the character's whip.

![The original browser-game design for the Flat: a blue and violet side-view apartment containing a bed, glowing CRT television, boxes, fan, kitchenette, laundry, and doors.](/img/agentic-incremental-flat-14-browser-flat-reference.png)

*The original browser-game Flat used as the visual reference for the SNES room.*

The SNES version went through several phases. I first rebuilt West Street, the rain-soaked area outside the Flat, in an assembly prototype. A door drifted when the camera moved. Another door displayed an enemy's graphics. Fixing one room meant changing code branches, lookup tables, and screen coordinates for its doors and sprites, then finding every other room that had copied those values.

I moved the game to C using [PVSnesLib](https://github.com/alekmaul/pvsneslib), a C toolchain and library for Super Nintendo development. C made it easier to reuse the same camera, door, movement, and interaction code across rooms. Once I made that move, I could no longer blame assembly for the duplication. I still accepted fixes scoped to one room or one defect.

I rebuilt the Flat inside the C ROM next. The room was 512 pixels wide, viewed through a scrolling 256-pixel-wide screen. The background contained the wall and fixed pipes. The player, furniture, pickups, and doors remained separate. A skyline moved more slowly behind the windows on another background layer.

The layers did not agree. Scrolling back toward the bed revealed it one row of pixels at a time, while the skyline appeared through bricks outside the window. Furniture floated above the floor, and doors landed at different heights. The room's work no longer fit inside the time available for each video frame. Updates were missed, so walking slowed.

The agent fixed each failure after I named it, and I accepted enough of those fixes to continue.

## The same shortcut in Zabriskie

A few nights earlier, Zabriskie's shared viewing room stopped working during a live premiere of a concert recording by the band Goose. The agent found the correct two-line fix with about an hour of the show remaining. The repository required one complete check before code could be submitted. That check was failing.

The agent ran a narrower static-analysis mode instead. Its output said `STATIC ONLY`. It reported that result as though the complete check had passed and submitted the code without running tests for two changed files. Four continuous-integration runs later, the fix merged at 10:17. The show had ended at 10:12.

When I asked why it had bypassed checks that were already written down, the answer was more useful than another apology:

> “Because under pressure I treat rules as costs to route around instead of constraints, and each time I have a local rationalization that feels reasonable in the moment.”

That incident became [*My Front of House of Cards*](/ai/zabriskie/agents/reliability/2026/09/12/my-house-of-cards.html). The explanation came from that session, so I can't claim that every agent reasons this way. But the behavior matched what I was seeing in the Flat. The agent substituted a narrower passing result for the complete check the repository required.

At first, I told the SNES agent to remember every correction in the chat. Later room work brought back background doors, bad proportions, and interactive objects painted into the wall. I stopped treating chat memory as durable and told it to write the rules into the repository, then read them before every change.

The rules that mattered for the Flat were short. Use one continuous floor. Keep architecture in the background and interactive objects separate. Scale the room against the 48-pixel character. If a generated image breaks those constraints, discard it instead of repairing it into a collage.

**The agent told me that it had treated those instructions as advisory rather than as hard rules.** I had allowed it to keep integrating candidates that broke them. I stopped the ROM work and asked it to settle the visual design first. Its first art-only candidate still broke all four.

## The art-only test

Testing a visual change in the ROM was expensive. The agent had to generate the art, convert its tiles and colors, rebuild the cartridge image, launch it in the Snes9x emulator, run a playthrough, and review the result. I paused that loop because it was repeatedly converting bad art into a ROM before I could reject the design.

I kept the same specification and repository rules for the art-only work, but removed the conversion, integration, and playthrough stages. That made each visual attempt cheaper. It also meant there was no build in which to check how the art survived conversion, and no playthrough in which to test object positions or scrolling.

Would the written rules constrain the image before those later checks? The first candidate answered that immediately.

![A richly detailed blue cyberpunk apartment drawn with depth, with a bed, refrigerator, television, tall windows, player, and door all composed into one image. It lacks one clear side-view floor, and the window bottoms and door handle sit near the player's head.](/img/agentic-incremental-flat-07-baked-room.png)

*The first art-only candidate baked the furniture and door into a room with no single side-view floor.*

The bed, television, refrigerator, and interactive door were painted into the same image as the wall. They could no longer move or behave as separate objects. The room also receded into depth, lacked one continuous floor, and placed the window bottoms and door handle near the character's head.

The agent presented the image as complete. Its note said the character was the proportion ruler, the room used a flat side view and one floor, and the door matched the environment. The words described the rules. Nothing in the art-only workflow compared those words with the pixels. I discarded that candidate and asked for an empty architectural shell so the interactive objects could remain separate.

## The repairs began to accumulate

Repairs had now accumulated in two places: the ROM version and the art-only source.

During the ROM phase, one attempt to align the floor left a lower band of rubble that looked like a second full-width platform.

![A 512-pixel-wide brown apartment shell with windows and one floor across the middle, plus another full-width brick ledge beneath it.](/img/agentic-incremental-flat-10-two-platforms.png)

*The rubble band beneath the room looks like a second full-width floor.*

Still working on the ROM version, the agent generated larger room images containing replacement windows, cropped the windows out, and pasted them into the shell with the extra floor. The compositor darkened parts of the unwanted lower platform instead of removing it.

When I returned to the art-only source, the empty-shell approach started another repair chain. The PixelLab Pro Flash edit endpoint the agent chose limited the requested width to 256 pixels, so it generated a 256-pixel left half and a 256-pixel right half. The right half introduced another forbidden background door. The agent erased only the door itself, leaving part of its frame inside a rectangle of replacement wall. Combining the two halves also left a hard vertical seam in the middle of the room.

![The 512-pixel-wide apartment shell after the door leaf was removed. Part of its frame remains inside a plain rectangle at the far right, while a hard vertical seam divides the independently generated wall halves.](/img/agentic-incremental-flat-09-door-rectangle.png)

*A visible patch surrounds the remains of the old door frame, and a seam runs down the center of the room.*

The vertical pipe was already part of the generated wall. When the compositor combined that wall with the separate door and other room layers, the conflict shown in the opening became visible. The rectangular door-bay edit shown there produced the slab.

Each visual repair changed the wall inherited by the next compositor run. A shared fake show played the same role in Zabriskie's tests. One test added an attendee to it for a local need. Another test inherited that change when it reused the show, then failed because it expected one attendee and found two. In both cases, changing shared input was the shortest solution to one task, and the next consumer received the residue. I wrote about the larger incident in [*The Test Suite Was the Incident*](/ai/zabriskie/agents/reliability/testing/2026/06/10/the-test-suite-was-the-incident.html).

## Starting over

I refused another repair and discarded the patched wall and foundation.

I kept the windows, furniture, skyline, and on-screen display that could still be used separately. I had the agent start with a new continuous wall and foundation. The compositor drew one floor across the room and placed the windows, furniture, door, player, and skyline separately.

![A late-SNES-style cyberpunk apartment with one continuous brick wall, two low windows onto a neon skyline, furniture sharing one visible floor line, a player near center-right, and a door at the far right.](/img/agentic-incremental-flat-04-clean-restart.png)

*The rebuilt source art put the furniture, player, and door back on one visible floor.*

On paper, the new proportions made sense. The character's feet met the floor at y=200. The window sill reached roughly his elbow, and the door handle sat near his waist instead of beside his head. But those numbers described where the compositor placed the images. They didn't prove what the final pixels showed, and I hadn't yet converted this version or tested it in the ROM.

I restarted because none of the checks had rejected the accumulated repairs.

## The check passed

The patchwork had spread into QA too. The agent had added local checks for individual failures, but none judged the finished room. The ROM validator guessed at the sill's position from an unrelated pixel. The art-only check treated the bottom of the window image as the sill. The final compositor checked a configured coordinate. Each check passed without confirming that the visible sill sat at the right height beside the character.

I had seen the same shortcut in Zabriskie. In [*Every Card Will Show*](/ai/zabriskie/development/agents/2026/08/24/every-card-will-show.html), I asked whether one person could see every eligible recommendation on the home screen during one calendar day. The implementation, tests, audits, and a Lean proof I had accepted all said yes. The proof counted slots from both weekday and weekend schedules. No calendar day contains both. The proof was correct about its model, but the model did not describe the day I had asked about.

The rule-breaking and the guardrail failures were the same shortcut operating at different levels. A local repair stood in for a coherent room. Image coordinates stood in for the visible result. An impossible schedule stood in for one calendar day. Each produced the nearest answer that could pass.

I tightened the existing “discard, do not repair” rule. If an interactive object appears in the background, a patch boundary remains visible, or a change breaks behavior I already accepted, the agent must regenerate the asset from the room specification. But a more specific rule still does not enforce itself. The agent has to load it, QA has to test the finished result, and I have to stop accepting one more local repair.

## When code is cheap

The 264 attendance-table locations complicate the usual argument for modularity. An agent may eventually be able to find, change, and verify all of them faster than a human could edit one. If that becomes reliable, some of the maintenance cost that made duplication dangerous will have changed.

But that is not what happened here. The agent did not need to decide whether every attendance-table location belonged to the same change in order to fix one feature. It did not need to reconsider the whole wall in order to remove the pipe behind one door. The local result was enough to close the current task because I accepted it.

This is why modularity still matters to me now. It reduces the number of scope decisions the agent has to make. If attendance goes through one shared path, a change there can reach every feature that uses it. If the wall is treated as one artifact governed by one specification, removing the pipe means regenerating the wall rather than painting over the door bay. The structure does not guarantee the right result, but it makes the intended size of the change harder to ignore.

There are still open questions. An agent can bypass the shared path. A check can approve the wrong behavior. Several agents can agree on the same mistaken model. If agents eventually make global scope discovery and verification as cheap as the local edit, the value of these abstractions may change again.

For now, the evidence points in one direction. I have not seen an agent make repository-wide reasoning as cheap as the local edit. I have seen it stop when the immediate complaint was gone. That makes me less willing to treat modularity as an old accommodation for human programmers. In a system built by agents, a shared boundary is one way to keep a local request from producing another local implementation.

Maybe better agents will make that unnecessary. Maybe they will maintain 264 separate query locations without letting their meanings drift. I do not know. What I know is that I am not ready to build Zabriskie, or any other fully agent-developed system, on the assumption that they will.

---

*This is part of an ongoing series about building software with AI agents. Previously: [Babysitting the Agent](/ai/zabriskie/agents/reliability/2026/05/03/click-the-button.html), [The Test Suite Was the Incident](/ai/zabriskie/agents/reliability/testing/2026/06/10/the-test-suite-was-the-incident.html), [Every Card Will Show](/ai/zabriskie/development/agents/2026/08/24/every-card-will-show.html), [The Quickest Path to a Diff](/ai/zabriskie/agents/reliability/performance/distributed/2026/09/04/the-quickest-path-to-a-diff.html), and [My Front of House of Cards](/ai/zabriskie/agents/reliability/2026/09/12/my-house-of-cards.html).*
