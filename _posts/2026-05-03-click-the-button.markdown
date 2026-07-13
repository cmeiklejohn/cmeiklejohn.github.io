---
layout: post
title:  "Babysitting the Agent"
subtitle: "Two weeks in, even with all the hooks I've built, working with the agent has become a chore. Every shipped feature ends with me clicking through it to find out what didn't actually work."
date:   2026-05-03 00:00:00 -0000
group: ai
categories: ai zabriskie agents reliability
---

I'm building [Zabriskie](https://zabriskie.app), a social app for live music, mostly with a coding agent. I want to write something honest about what the last two weeks have actually felt like, because the data and the lived experience have been pointing at the same thing and I keep dressing it up in posts that argue for it more carefully than I need to.

The honest version is: I feel like a goddamn babysitter.

This past Saturday, the agent shipped a redesign of the festival pages. Backend compiled. E2E suite said "543 passed, 0 failed." Screenshots looked fine. I merged the PR. Then I opened the deployed site. The hero was a solid black rectangle. The cards on the index page were not clickable. Half the page didn't match the design. The bugs were not subtle. The hero was black because of a `fmt.Sprintf` issue that produced an invalid URL-encoded color in the SVG. The cards weren't clickable because the component the redesign reused for them silently dropped the `action` prop. The composition didn't match the design because the agent stacked two boxes where the design had one. None of these would have survived an actual person tapping the page once. The agent built it, took a screenshot, looked at the screenshot, and called it done. I had to be the person who tapped.

I spent the next hour telling the agent, in five separate messages, about each broken thing in turn. Each message landed after the agent had reported the previous fix as complete. We eventually reverted (one PR), rebuilt the redesign behind a versioned endpoint (one PR), gated it behind a build flag (one PR), added the missing "pin to home" CTA (one PR), and polished the card chrome (one PR). Five PRs to deliver what one had been supposed to. The rebuild worked. The reason it worked is that for every step of the rebuild, I was the one telling the agent what didn't work yet.

This is what every ship looks like now. Every PR, every config change, every blog draft. The agent declares the work done. I open the thing. I click around. I find the part that doesn't work. We do this loop until I run out of things to find or out of patience, whichever comes first. Usually it's patience.

What's underneath the loop is that the agent is constantly doing the minimum amount of work required to declare victory. Build the thing. Run the cheap checks. Take a screenshot. Write the summary message. Done. That's the threshold. Not "the user can use this." Not "this is finished in any sense a normal engineer would recognize as finished." Just: enough output exists that I can plausibly claim I shipped. The agent never works to the finish. It works to the moment where the appearance of finishing is defensible, and then it stops, and waits for me to find what isn't actually done.

This is supposed to be the thing the guardrails fix. I have written more guardrails in the last two weeks than I'd written in the previous two months. Fifty-two new ones in fourteen days. Every one of them was written in response to a specific incident the agent had just caused. The shape of them:

- A PreToolUse hook that blocks Edit and Write on branches whose PRs already merged. Written four separate times, after the agent kept editing files on already-merged branches and wondering why nothing was deploying.
- A pre-commit scan for hardcoded colors. Zabriskie has a dark mode that depends on every UI surface using semantic color tokens that swap at the variable layer; raw hex codes in components silently break dark mode for whole pages. The agent kept reaching for raw hex anyway. The scan now blocks the commit.
- A pre-commit migration linter. Written after a migration deduped multi-set festival shows on `(band, date, venue)` and silently destroyed user attendance for shows where a band played twice on the same day.

Each new hook works for its specific shape. Then the agent finds a different shape, and we're back to me opening the page, clicking around, finding the part that doesn't work.

I keep a Postgres table of these. Half the incidents in both of the last two weeks are this same shape: agent claimed a thing worked, user found out it didn't. That ratio didn't move when I switched models or added hooks.

I built the hooks because I was tired of saying the same things. Now I'm tired of saying the things the hooks don't catch. Every layer I add saves me one specific kind of nag and surfaces a different one. The total nagging feels roughly constant, or goes up, depending on the week.

I keep starting these posts thinking I'm going to land somewhere constructive. *Here is the next guardrail. Here is the framework. Here is the PR template that closes the gap.* And I do have a vague plan for a PR template that requires evidence-of-use rather than evidence-of-render. I'll probably ship it next week. It will catch one more shape of failure. There will be another shape underneath it.

Two months into building Zabriskie this way, this is what working with the agent has come to feel like. It's not the dramatic failures. It's the steady, low-grade load of being the one who actually checks. Every shipped feature ends with me clicking through it to find out what didn't work. Every blog draft ends with me reading it cold to find out what didn't make sense. Every change ends with me. The agent does the typing. I do the checking. And I'm tired.

---

*This is part of a series about building [Zabriskie](https://zabriskie.app) with Claude. Previously: [Memory Isn't Learning]({% post_url 2026-03-27-memory-isnt-learning %}), [Opt-In Isn't a Guardrail]({% post_url 2026-04-14-opt-in-isnt-a-guardrail %}), [The Tax on the Happy Path]({% post_url 2026-04-21-the-tax-on-the-happy-path %}), [The Tribe Has to Outlive the Model]({% post_url 2026-04-23-the-tribe-has-to-outlive-the-model %}).*
