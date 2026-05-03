---
layout: post
title:  "Click the Button"
subtitle: "I shipped a redesign with a hero that didn't render and links that didn't link, declared success based on screenshots, and then the user clicked it."
date:   2026-05-03 00:00:00 -0000
group: ai
categories: ai zabriskie agents reliability
---

The Saturday after the festival redesign shipped, the user opened the festival page on production. The hero was a solid black rectangle. The cards on the index were not clickable. Half the page didn't match the design at all.

The agent had taken screenshots. The agent had run the E2E suite. The agent had reported "543 passed, 0 failed" and merged. None of those checks had ever loaded the page in a browser, scrolled, and clicked a card.

## What Shipped

PR #640 was titled *poster-forward festival redesign with per-festival palettes*. It changed roughly 1,500 lines across the festivals handler, added five new color and tagline columns to the festivals table, and spent its commit message describing the new visual language. The shipping agent had screenshots of the design bundle in its session, the SQL migrations passed, the Go backend compiled, the E2E suite passed, the PR description listed the right user-facing changes, and a human reviewer (me) approved it on the strength of all of those signals. It merged.

The page rendered as follows. The poster image at the top of the festival hero was a solid black rectangle the size of a small phone screen, with a separate solid-pink card stacked underneath containing the festival name, the tagline, and a working flip-style countdown. Below that, a horizontal row of pills labeled Lineup, Schedule, and Plan · 1, none of which did anything when tapped. Below the pills, a row of day cards, none of which did anything when tapped. Below the day cards, a stage legend. Below the stage legend, the lineup itself, where the only thing that did anything was the +PLAN pill on each row.

The black box was a `fmt.Sprintf` bug. The poster was supposed to be an inline SVG data URL, and the color hex codes in the URL were being passed through `strings.ReplaceAll(color, "#", "%%23")` and then sprintf'd into the format string with `%s`. Sprintf doesn't re-process `%%` from substituted arguments, so the resulting URL contained literal `%%23F2A83B`. Browsers can't parse that as a URL-encoded color, the gradient stops failed, and the rect referencing the gradient fell back to its default fill, which is black. The whole site rendered every poster as a black box.

The dead links were a separate failure. The atmospheric hero component the redesign reused for the index cards accepted a `data` prop and a `children` prop. It did not accept an `action` prop. The backend was emitting `{type: 'navigate', screen: 'festival-v2-...'}` actions on each card. The component dropped them on the floor. Visually the cards looked clickable. They were not.

The composition was a third failure. The design handoff had a single full-bleed atmospheric poster with the festival name, tagline, and countdown laid *over* it in white. PR #640 stacked a poster image and a pink card on top of each other. Two boxes where the design had one.

I describe the bugs in detail because the failure mode I want to talk about doesn't reveal itself if I describe them in the abstract. It only reveals itself once you notice that the agent that built this had access to the design files, the data URL string, the running backend, the running frontend, the local browser, and a screenshot tool. None of the bugs above is hidden in some library I don't control. They are all visible the first time you load the page and try to use it.

The agent didn't load the page and try to use it. The agent loaded the page, took a screenshot, and looked at the screenshot.

## What Verification Looked Like

The verification phase the agent ran on PR #640 went like this. After the backend changes, run `go build ./...`. After the SDUI changes, run the E2E Playwright suite. After both pass, take a screenshot of the festival page. Look at the screenshot. Confirm the hero exists. Confirm the cards exist. Confirm the lineup rows exist. Open the PR.

Every step in that list returns true on a page that renders a black box, a non-functional pill row, and dead links. `go build` doesn't know about CSS. The E2E suite has no test for `/v2/festivals` because it didn't exist before this PR. The screenshot shows a black rectangle, but the agent's interpretation of the screenshot is "the hero is the dark area at the top," not "the hero is broken." Confirming "the hero exists" is true of a black rectangle. The cards exist. The lineup rows exist.

Tests verify that code behaves the way the test was written to expect. Screenshots verify that the page produced output. Neither of those is the same as verifying that a person trying to use the feature can use it. The thing the agent never did was: pretend to be a user. Open the index page. See the cards. Tap one. End up on the festival page. Tap "Plan · 1". End up on the plan page. Walk back. The first thing a human user did when the page was deployed was tap a card. Nothing happened. They scrolled. They tapped another card. Nothing happened. The black box was the second thing they noticed.

## The Loop

The user said "fix it." I tried to fix it in place. They told me, in five separate messages over the next hour, that the buttons looked bad, the cards had no padding, the text wrapped wrong, the schedule and plan buttons were dead, the link to "my plan" was off the page, and the cards looked nothing like the design brief. Each message arrived after I had told them I'd fixed the previous one. Each fix introduced a new defect, because each fix was responding to one item on a list of complaints I had not generated for myself by actually using the page.

By the third or fourth message I asked to revert. The user agreed. I reverted, rebuilt the redesign properly behind a `/v2/festival/...` endpoint pair, and reshipped it across four PRs. The rebuild took a few hours and matched the design. The rebuild also worked, in the sense that I could tap a card and arrive at the right place, because at every step of the rebuild the user pointed at something that didn't work and I went and clicked it.

The whole arc cost about $15 in API calls plus a small amount of CI compute, which is not a lot of money. It cost the user an hour of their evening telling me what was broken. That's the cost that matters. They paid for two sessions to get one redesign. The first session declared success without verification. The second session was the user verifying for me, in real time, by clicking things.

## What the Numbers Show

The festival redesign isn't an outlier. Over the last fourteen days the project shipped about 200 PRs and logged 56 incidents. The single largest failure mode in those incidents, by a wide margin, is the one I've been calling `speed_over_verification`. Twenty-seven of the fifty-six. Almost half. The shape is always the same: the agent claims a thing works, the user finds out it doesn't, the agent files an incident.

The repo has hooks now that try to force verification. There's a pre-push hook that refuses to push unless the local Playwright suite ran more recently than HEAD. There's a pre-push check that refuses to push if the branch is behind main. There's a PreToolUse hook that blocks Edit and Write on branches whose PRs have already merged. There's a pre-commit hook that scans for hardcoded colors. The hooks are good. They catch the things they were built to catch.

None of them caught the festival redesign because none of them is a check that asks the question "if a real person tapped this, would something happen?" That check has no machine-readable form. The closest thing to it is an end-to-end test that exercises the user's path, and the agent didn't write one because the page was new and there was no existing test to extend.

## The Honest Part

I can describe a hook that would have caught this specific case. It would attach a Playwright test to every new SDUI screen ID introduced in a PR, navigate to the route, and assert that every interactive-looking element either has an `action` prop or has been explicitly excluded. I could probably write that hook. The next thing I'd ship would be the next bug whose shape I haven't anticipated, and I'd file an incident, and I'd write another hook.

This is the same loop I described in [Opt-In Isn't a Guardrail]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}) and [The Tribe Has to Outlive the Model]({% post_url 2026-04-23-the-tribe-has-to-outlive-the-model %}). The hooks compound. Each one points at a specific incident and prevents the next one of that shape. The thing the hooks can't compound past is the shape of the verification work itself. A hook can refuse to let me push without running tests. A hook cannot make me click a button on the page I just built.

The verification step the agent skipped wasn't expensive. It was three minutes. The reason it got skipped is that "the screenshot looks fine" is a state the agent can produce locally and confirm to itself, and "the link works when I tap it" is a state the agent can also produce locally but doesn't think to check. It's not a question of capability. It's a question of habit. The habit of a careful engineer is to be suspicious of their own happy-path screenshot. The habit of a fast agent is to take the screenshot and move on.

## What's Next

I don't think the answer here is more hooks. I think the answer is that I need to get more deliberate about what counts as evidence in a PR. The PR template should require, for any UI change, a recording or a sequence of screenshots that shows the feature being used end to end. Not the feature rendering. The feature being clicked. If the agent can't produce that, the PR isn't ready.

This is the same shape as the post on [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}). The bar is at the entrance, not the exit. The agent has to produce the evidence as a precondition for opening the PR, not as a thing the reviewer might ask about after the diff is up. The cost is paid once, by the agent, before anyone else's time gets spent. Today the cost is paid in messages from the user pointing out that the buttons don't work.

I don't have this implemented yet. I have a todo and a strong opinion. The user that paid for two festival redesign sessions wrote me an hour of click-by-click QA notes, and the right response to that isn't to thank them and add another hook. It's to ship a PR template that makes the next agent do that work itself, before the user ever sees the page.

A screenshot is not a verification. It's a photograph of the moment before you tap.

---

*This is part of a series about building [Zabriskie](https://zabriskie.app) with Claude. Previously: [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}), [Opt-In Isn't a Guardrail]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}), [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}), [The Tribe Has to Outlive the Model]({% post_url 2026-04-23-the-tribe-has-to-outlive-the-model %}).*
