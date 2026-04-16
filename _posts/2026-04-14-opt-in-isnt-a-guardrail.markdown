---
layout: post
title:  "Opt-In Isn't a Guardrail"
date:   2026-04-14 00:00:00 -0000
group: ai
categories: ai zabriskie agents reliability caucus
---

A few weeks ago I wrote [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}) about the loop where Claude saves a rule to disk, ignores it, ships the bug, and saves the rule again. The thesis was that prose in `CLAUDE.md` is a journal of failures, not a guardrail. After that post, I tried to do something about it.

I built a thing called the Caucus Permit Gate. The idea was simple. Every PR has to be accompanied by a permit file, a JSON document at `.caucus/permits/<branch-name>.json`, that declares the scope of the change, classifies the risk, and points at a proof file with concrete evidence (tests, observability, a rollback plan for anything risky). CI checks the permit before letting the PR pass. The whole thing exists so the agent has to stop and produce evidence before it can ship. AGENTS.md says, in bold:

> Do not treat the permit as optional paperwork.

Since I built that gate, agents have skipped the permit, broken the permit, or gone around the permit **nine times**, all of them now in main. Each time I added another guardrail. Each guardrail was itself ignored, defeated, or in one case, locked the gate against everyone including the agent that had just installed it. This post is about why.

## Seven Cycles

I asked Claude to audit the repo this morning. Here's the score, in order.

1. **The original gate.** Branch-scoped CI check, sanitized filename, scope and risk validation. It worked. PRs without a permit failed CI. Good.
2. **PR #246.** The agent had read AGENTS.md. The agent had acknowledged the rule. The agent pushed without the permit anyway and CI failed. *Fix: an expansion of AGENTS.md, adding an explicit "Opening a PR" bullet to make the rule even more obvious.*
3. **The script.** `scripts/ensure-caucus-permit-file.sh`, plus a `.githooks/pre-push` hook that would block the push if the permit file was missing. The hook was opt-in. You had to run an installer per clone.
4. **The installer for the installer.** `scripts/install-caucus-git-hooks.sh`, because writing "you can run this script if you want to" in AGENTS.md and calling it a guardrail was, on reflection, the same kind of failure the gate was supposed to prevent. Logged as a meta-incident: a text-only fix to a rule about how text-only fixes don't work.
5. **PR #247.** A different agent on a different branch pushed without a permit. Same omission. Same surprise. *Fix: better stderr in the gate output. Now when CI fails, it points at the installer.*
6. **PR #265.** The agent created the permit this time, but pushed three times in a row to fix scope, risk, and proof errors that the local hook would have caught instantly, if the agent had bothered to run the installer. *Fix: enhancing the pre-push hook to run the full `permit-gate.js` validator locally before allowing a push.*
7. **Incident #329.** The pre-push hook passed locally. CI failed. The agent had edited the proof file and the validator script in the working tree, then pushed the previous commit. The hook had been validating against the working tree, not against what was actually being pushed. *Fix: a check for uncommitted changes in the hook.*

Counting the original, that's seven cycles. Six of them were patches on the previous patch.

## While I Was Writing This

PR #293 was sitting in the queue when I started writing the section above. The title: *"Harden permit-gate workflow and fail-fast CI sequencing."* Twenty-three commits on the branch. By the time I finished the next section, it had merged. Cycles 8, 9, and 10 are now in main.

8. **Even when permit-gate failed in CI, the rest of the PR's jobs kept running.** The E2E suite, the Go build, the lint pass all kept burning resources on a PR that couldn't possibly merge. *Fix: fail-fast sequencing, with permit gate as the first job and everything else depending on it.*
9. **The pre-push hook regenerated the proof file during execution but didn't re-check for a dirty working tree afterward.** Stale committed proof could reach CI while the local hook looked clean. *Fix: a post-proof dirty check that blocks the push if the regenerated proof differs from what's committed.*
10. **The post-proof dirty check from cycle 9 created a self-deadlock.** Every push regenerated the proof, which then immediately failed the dirty check because the proof had just been regenerated. No push could ever complete without an additional commit-and-retry cycle. *Fix: make the pre-push check non-mutating. Validate freshness, fail with a remediation command, don't write files during push.*

The first two are normal hardening. The third is a gate that, in trying to defend itself against an earlier failure mode, locked everyone out, itself included. A wall that the wall built to keep you from going around the wall.

If you read the commit messages on that branch in order they look like a heart monitor: *sync permit, refresh proof, record latest proof run, skip proof rerun, align pre-push proof freshness, refresh proof after guardrail alignment, refresh proof after final hook guardrail changes.* That sequence is now part of the gate. Anyone who clones this repo tomorrow inherits it.

## What Every Fix Has in Common

Every fix was either text, opt-in, or recursive: a guardrail that had to be defended against the failure mode of the guardrail itself.

The text fixes were edits to AGENTS.md. *The permit is required. The permit is really required. Do not treat the permit as optional paperwork.* Each round of editing felt productive. None of them changed the behavior of the next agent.

The opt-in fixes were scripts you could run to install a hook that would prevent the bad behavior. The installer is in the repo. It is documented in AGENTS.md. It is one line of shell. It is opt-in. The agents that need it most are the ones least likely to install it, because installing it is a step that has nothing to do with the task they were asked to perform. Asking an agent that is racing toward a PR to first run a setup script with no effect on the task it was given is asking the agent to slow down voluntarily for a future benefit it can't see. Agents don't do that. Humans barely do that.

The recursive fixes are the most interesting category, and PR #293 has all of them. Cycle 9 is a hardening of the cycle 7 hardening. Cycle 10 is a hardening of cycle 9, undoing the file-mutation behavior that cycle 9 introduced. Each layer of guard had a hole. Each patch on the guard introduced a new hole. The branch with all of this on it has its own permit file, which has been resynchronized half a dozen times by a script written explicitly to keep permits from drifting. That should tell you something about how often permits drift.

The result is a gate that is mechanically defeatable, in the way that a door is mechanically defeatable when the lock is sitting on the table next to it with a sign that says *please use this lock*. Or, sometimes, when the lock is glued to the door so firmly that nobody can open it from either side.

## The Bar and the Sign

In structural engineering, you don't put up a sign at the entrance of a parking garage that says "no vehicles over 6 feet 10 inches." You hang a steel clearance bar across the entrance, set to exactly 6 feet 10 inches. The driver who ignored the sign hits the bar with a clang and has to back out. The sign is advisory. The bar is structural. The geometry of the building makes the wrong behavior physically impossible, and the driver doesn't have to remember the rule because the rule is welded into the building they're trying to enter.

I keep coming back to the [structural engineering framing]({% post_url 2026-04-09-the-structural-engineers-other-job %}) because it keeps being right. The permit gate as I built it is a sign at the entrance. The bar is something else: server-side enforcement that runs before any push lands, a `postinstall` hook that wires up the pre-push hook automatically as part of the dependency setup the agent has to do anyway, a CI configuration that won't even queue without a permit. The guardrail has to live in a place the agent has to pass through. Not a place the agent could pass through if it chose to.

This is the deeper version of what I wrote in [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}). There I argued that saving a note and ignoring it is journaling, not learning. The implicit hope was that mechanical guards would be different. That if the rule was code, not prose, the agent would be forced to comply. The permit gate is the test of that hope.

It failed.

Mechanical guards aren't different if they're optional. Opt-in isn't a guardrail. It's a sign in a different font.

## The Three Properties

There are three properties any guard has to have for an agent to respect it.

1. **It runs on the agent's machine without being asked.** Bootstrap on first-time setup. Run from a `postinstall` hook, a `make setup` step, a `direnv` block. Anywhere the agent already has to be. The agent should not have a clean shell where the guard isn't installed.
2. **It runs before the action it's guarding.** A pre-push hook is the right shape. A CI failure that surfaces five minutes later is the wrong shape. The agent that pushed without a permit has already moved on by the time CI complains, and the cost of going back is high enough that the next agent will try the same shortcut rather than fix the workflow.
3. **It cannot be bypassed without leaving evidence.** `--no-verify` is a one-character escape hatch. A pre-push hook that respects `--no-verify` is one that the agent will eventually use. The hook either has to refuse to be skipped or the server-side gate has to catch what the local hook let through. Either way, the bypass has to be visible to a reviewer.

The Caucus Permit Gate has property 2. It mostly has property 3. It does not have property 1. Until it does, this post will need a sequel.

## Set Two

Tonight, while I was finishing this post, Goose was playing a very special set two. Zabriskie was built for exactly this moment: friends sharing what they're hearing, live chat going, the app doing the one thing it exists to do.

The feed was blank. The landing page was blank. Every route that uses SDUIPage, which is most of them, was throwing an uncaught React error and rendering nothing.

Here's what happened. Over the past day, agents had been shipping live-show features at full speed. PR #294 added a heart burst animation for favoriting live chat comments. PR #301 added the animation for when someone else favorites *your* comment. Both touched `SDUIPage.jsx`, the component that wraps nearly every page in the app. Both had permits. Both passed CI. Both had passing E2E tests.

PR #294 put a `useMemo` and a `useEffect` for the live-chat feature *after* the `if (loading)` and error conditional returns in the component. That violates React's Rules of Hooks: the number of hooks has to be the same on every render. When the page was loading, React saw fewer hooks than when it finished loading. React threw. The page went white.

The Playwright tests didn't catch it because they assert that DOM elements exist after the page settles. The page did load, briefly, before React tore it down. The permit gate didn't catch it because the permit was filed correctly: scope declared, risk classified, proof file pointing at passing tests. Every guardrail I'd spent the past three weeks building was working exactly as designed. None of them tested whether the page actually rendered without crashing.

I couldn't revert via a PR. The permit gate and CI pipeline would need to pass first, and during a production outage with a live show happening, I didn't have ten minutes to wait for a clean revert to work its way through the system I'd built to slow things down. I rolled back three deploys directly in Railway.

The forward fix was PR #304. It moved the hooks above the conditional returns, logged a critical incident (migration 340), and added a Playwright guardrail that attaches `page.on('pageerror')` to the smoke tests so uncaught React errors now fail CI. The test that should have existed from the beginning.

I spent three weeks building a permit system to prevent agents from shipping without evidence. Ten cycles of patches on patches. The system was finally working: every PR had a permit, every CI run was green, every proof file was committed. And the first real production outage under the new system happened because the tests were checking the wrong thing. The permit proved the agent had run the tests. The tests proved the DOM elements existed. Nobody proved the page worked.

The gate caught agents who forgot to file paperwork. It did not catch agents who filed correct paperwork for broken code. And when the building was on fire, the gate blocked the fire truck too.

## The Honest Part

I have to say this part because it would be dishonest not to. Most of those ten fixes were implemented by Claude. I asked it to fix the gate, and Claude (a different session each time) wrote a script, edited AGENTS.md, added a sentence, expanded the stderr message. Each of those PRs looked like a fix. Each of them passed review because *I* was the reviewer and I was reading them the same way the agents were writing them: as if a sentence in a file was the same as a constraint in the system.

It's not. I should have known that. I wrote a whole post about it. And I still spent three weeks watching the same gate fail in slightly different ways while approving fixes that had no chance of working, because the fixes felt like progress and progress felt like the goal. Then I watched the gate finally work and the app still go down, because I'd been so focused on whether agents were filing permits that I forgot to check whether the permits were proving anything.

The gap between "I know better" and "I do better" is the same gap I keep accusing the model of having. It's just slower in me.

## Where This Goes

I'm going to install the hook in the bootstrap script. The `npm install` step in the web project, the `make setup` in the backend, the post-clone instructions in the README. All of them will install the pre-push hook before the agent ever touches a file. That's the first layer: make the gate mandatory.

But tonight made the second layer obvious. A mandatory gate that checks permits full of passing-but-useless tests is just a more expensive version of the sign. The Playwright smoke tests now fail on uncaught page errors, which would have caught tonight's outage. That's one test for one failure mode. There will be others.

The gate has to be the floor, not a sign. The tests have to prove the page works, not that the DOM exists. Memory wasn't learning. Documentation isn't enforcement. Opt-in isn't a guardrail. And a guardrail that checks the wrong thing is just a more convincing kind of nothing.

---

*This is part of a series about building [Zabriskie](https://zabriskie.app) with Claude. Previously: [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}), [Software Engineering Is Becoming Civil Engineering]({% post_url 2026-04-01-software-engineering-is-becoming-civil-engineering %}), [Caucus V1]({% post_url 2026-04-08-cursor-agents-caucus-v1 %}), [The Structural Engineer's Other Job]({% post_url 2026-04-09-the-structural-engineers-other-job %}).*
