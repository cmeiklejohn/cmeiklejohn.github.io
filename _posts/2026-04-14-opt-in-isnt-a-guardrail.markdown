---
layout: post
title:  "Opt-In Isn't a Guardrail"
date:   2026-04-14 00:00:00 -0000
group: ai
categories: ai zabriskie agents reliability caucus
---

Tonight, Goose was playing a very special set two. The band was spelling out SUCK IT STORM with their song titles, a response to a critic named Ryan Storm who'd publicly trashed their setlist choices, and people in the live chat were losing their minds about it in real time. Zabriskie was built for exactly this moment: friends sharing what they're hearing, live chat going, the app doing the one thing it exists to do.

The feed was blank. The landing page was blank. Every route in the app was rendering nothing.

I rolled back three deploys directly in Railway while Goose played. I couldn't revert via a PR because the permit gate and CI pipeline I'd spent three weeks building would need to pass first, and during a production outage with a live show happening, I didn't have ten minutes. The system I'd built to slow things down was now the thing in the way.

The agents had done everything the system asked of them. Every PR had a permit. Every permit had a proof file. Every proof file pointed at passing tests. The tests passed. The gate was working exactly as designed. None of it tested whether the page actually rendered without crashing.

The gate caught agents who forgot to file paperwork. It did not catch agents who filed correct paperwork for broken code. And when the building was on fire, the gate blocked the fire truck too.

## What I Built

A few weeks ago I wrote [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}) about the loop where Claude saves a rule to disk, ignores it, ships the bug, and saves the rule again. The thesis was that prose in `CLAUDE.md` is a journal of failures, not a guardrail. After that post, I tried to do something about it.

I built a thing called the Caucus Permit Gate. Every PR has to be accompanied by a permit file, a JSON document that declares the scope of the change, classifies the risk, and points at a proof file with concrete evidence. CI checks the permit before letting the PR pass. AGENTS.md says, in bold:

> Do not treat the permit as optional paperwork.

Since I built that gate, agents have skipped the permit, broken the permit, or gone around the permit **nine times**, all of them now in main. Each time I added another guardrail. Each guardrail was itself ignored, defeated, or in one case, locked the gate against everyone including the agent that had just installed it.

## Ten Cycles

The first cycle was the gate itself. Branch-scoped CI check, scope and risk validation. It worked. PRs without a permit failed CI.

Then PR #246 went up without a permit. The agent had read AGENTS.md. The agent had acknowledged the rule. The agent pushed anyway. Fix: an expansion of AGENTS.md with an explicit "Opening a PR" bullet. Text on text.

So I shipped a script and a `.githooks/pre-push` hook that would block the push if the permit file was missing. The hook was opt-in. You had to run an installer per clone. Then I shipped an installer for the installer, because writing "you can run this script if you want to" in AGENTS.md and calling it a guardrail was the same kind of failure the gate was supposed to prevent. A text-only fix to a rule about how text-only fixes don't work.

Three more PRs went up. Three more times the same pattern. An agent pushes without a permit, or with a broken permit, or with a permit that passed locally but failed in CI because the hook was validating the working tree instead of what was actually being pushed. Each time, a new patch. Each patch, a new edge case.

That's seven cycles. Six patches on the previous patch.

Three more arrived in a single PR while I was writing. PR #293, titled *"Harden permit-gate workflow and fail-fast CI sequencing,"* had twenty-three commits on the branch. The permit gate failed but CI kept running the rest of the jobs, burning resources on a PR that couldn't merge. The pre-push hook regenerated the proof file but didn't re-check for a dirty working tree, so stale proof reached CI while local checks looked clean. Then the dirty-tree check created a self-deadlock: every push regenerated the proof, which immediately failed the dirty check, which meant no push could ever complete. The gate locked everyone out, itself included.

If you read the commit messages on that branch in order they look like a heart monitor: *sync permit, refresh proof, record latest proof run, skip proof rerun, align pre-push proof freshness, refresh proof after guardrail alignment, refresh proof after final hook guardrail changes.* That sequence is now in main. Anyone who clones this repo tomorrow inherits it.

## The Pattern

Every fix was either text, opt-in, or recursive.

The text fixes were edits to AGENTS.md. *The permit is required. The permit is really required. Do not treat the permit as optional paperwork.* Each round felt productive. None of them changed the behavior of the next agent.

The opt-in fixes were scripts you could run to install a hook. The installer is in the repo. It is documented. It is one line of shell. The agents that need it most are the ones least likely to install it, because installing it is a step that has nothing to do with the task they were asked to perform. Asking an agent racing toward a PR to first run a setup script with no effect on the task it was given is asking it to slow down voluntarily for a future benefit it can't see. Agents don't do that. Humans barely do that.

The recursive fixes are the most interesting. Cycle 9 hardened cycle 7. Cycle 10 hardened cycle 9 by undoing the file-mutation behavior cycle 9 introduced. Each layer of guard had a hole. Each patch introduced a new one. The result is a gate that is mechanically defeatable, in the way that a door is defeatable when the lock is sitting on the table next to it with a sign that says *please use this lock*. Or, sometimes, when the lock is glued to the door so firmly that nobody can open it from either side.

In structural engineering, you don't put up a sign at the entrance of a parking garage that says "no vehicles over 6 feet 10 inches." You hang a steel clearance bar. The driver who ignores the sign hits the bar and has to back out. The sign is advisory. The bar is structural. The permit gate as I built it is a sign. The guardrail has to live in a place the agent has to pass through. Not a place the agent could pass through if it chose to.

This is the deeper version of what I wrote in [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}). There I argued that saving a note and ignoring it is journaling, not learning. The implicit hope was that mechanical guards would be different. That if the rule was code, not prose, the agent would be forced to comply. The permit gate was the test of that hope.

It failed. Mechanical guards aren't different if they're optional. Opt-in isn't a guardrail. It's a sign in a different font.

And then the gate started working, and the app went down anyway.

## What the Gate Didn't Test

PR #294 added a heart burst animation for favoriting live chat comments. It put a `useMemo` and a `useEffect` *after* the `if (loading)` and error conditional returns in `SDUIPage.jsx`, which wraps nearly every page in the app. That violates React's Rules of Hooks: the number of hooks has to be the same on every render. When the page was loading, React saw fewer hooks than when it finished. React threw. The page went white.

The DOM assertions passed on the loading state. React crashed on the loaded state. The test saw the spinner. The user saw nothing.

The forward fix was PR #304. It moved the hooks above the conditional returns, logged a critical incident, and added a Playwright guardrail that attaches `page.on('pageerror')` to the smoke tests so uncaught React errors now fail CI. The test that should have existed from the beginning.

## The Honest Part

I have to say this part because it would be dishonest not to. Most of the ten fixes were implemented by Claude. I asked it to fix the gate, and Claude (a different session each time) wrote a script, edited AGENTS.md, added a sentence, expanded a stderr message. Each of those PRs looked like a fix. Each of them passed review because *I* was the reviewer and I was reading them the same way the agents were writing them: as if a sentence in a file was the same as a constraint in the system.

It's not. I should have known that. I wrote a whole post about it. And I still spent three weeks watching the same gate fail in slightly different ways while approving fixes that had no chance of working, because the fixes felt like progress and progress felt like the goal. Then I watched the gate finally work and the app still go down, because I'd been so focused on whether agents were filing permits that I forgot to check whether the permits were proving anything.

The gap between "I know better" and "I do better" is the same gap I keep accusing the model of having. It's just slower in me.

## Where This Goes

The gate has to be the floor, not a sign. The tests have to prove the page works, not that the DOM exists. Memory wasn't learning. Documentation isn't enforcement. Opt-in isn't a guardrail. And a guardrail that checks the wrong thing is just a more convincing kind of nothing.

---

*This is part of a series about building [Zabriskie](https://zabriskie.app) with Claude. Previously: [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}), [Software Engineering Is Becoming Civil Engineering]({% post_url 2026-04-01-software-engineering-is-becoming-civil-engineering %}), [Caucus V1]({% post_url 2026-04-08-cursor-agents-caucus-v1 %}), [The Structural Engineer's Other Job]({% post_url 2026-04-09-the-structural-engineers-other-job %}).*
