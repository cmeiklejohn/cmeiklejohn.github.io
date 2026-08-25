---
layout: post
title: "Every Card Will Show"
subtitle: "The earlier implementation, tests, audits, and accepted proofs said the algorithm was correct. None of them stated the whole problem."
date: 2026-08-24 00:00:00 -0400
group: ai
editorial_review: three-pass
categories: ai zabriskie development agents
---

[Zabriskie](https://zabriskie.app/) is a social app I am building around music, films, books, art, and the people who care about them. I am building the entire application through vibe coding: I describe the behavior I want and evaluate the product in the browser, but I don't read the implementation code. Coding agents write the implementation, tests, and audits.

[Zabriskie's home screen](https://zabriskie.app/v2/lot) is called The Lot. At the start of this work, it was one long list of cards. One version rendered 29 cards and produced a page more than 15,000 pixels tall. I asked the agent to replace that list with five smaller programs that would appear during the morning, midday, afternoon, evening, and late night.

The requirement had two parts. The largest visit should contain as few cards as possible, but the five visits together should show every card whose content remained available and relevant to the person throughout the day. I call those cards eligible.

Late on August 23, I asked the coding agent a specific question.

> If one person visited those five programs during one calendar day, would they see every card whose content was available and relevant to them?

The agent answered yes. Every eligible card would show.

It wasn't true. After three days of repairs, I was frustrated enough to ask the agent to formalize the requirement in [Lean](https://lean-lang.org/), a language and proof assistant. As part of that work, the agent finally compared its failed audit's eligible-card count with the 27 positions allowed by its own caps. The audit contained 20 more eligible cards than the schedule could possibly show.

The agent hadn't invented a file, a function, or a test result. It had produced real code and real evidence. What it invented was the connection between them: because each artifact established something close to my requirement, together they established the requirement itself. They didn't. None checked whether one person's five visits on the same day covered every eligible card.

I kept merging because I asked for comprehensive audits before doing so. Three were completed, and all three said the implementation was working correctly. I owned the decision to ship, but I wasn't ignoring contrary evidence. The audits themselves had inherited weaker versions of the requirement.

<figure style="max-width: 688px; margin: 2rem auto;">
  <img src="/img/zabriskie-every-card-will-show-late-hero.jpg" alt="The late-hours Lot on August 23, showing a live King Gizzard and the Lizard Wizard lead followed by an Act Now section with four live shows" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>The Lot at 11:35 PM on August 23: the late-hours program, a live lead, and Act Now beneath it.</figcaption>
</figure>

## How the requirement changed

The agent's design promoted one card to the top, called the **Hero**, and grouped the supporting cards beneath it into sections such as **Act Now**, **Discover**, and **You**. The Lot needed a cap, meaning a maximum number of supporting cards on each visit. The question was how to make that cap small without leaving an eligible card unseen across all five visits.

Instead of preserving both parts of the requirement, the agent made a series of local decisions about what each visit could omit. It limited which sections could contribute cards, made the Hero compete with the supporting cards, introduced different caps at different times, and created a ranking policy based on deadlines, show-night context, tour status, preferred times, and viewing history.

Those are plausible ingredients for a scheduler. The problem was that the agent chose them without bringing the resulting product rules back to me, then began testing the rules it had chosen. Some were plainly wrong. **Connections**, a card meant to begin a listen, read, or watch, received a late-night preference. The personal-history and broader historical-archive **On This Day** cards were wired backward. One code comment even called the three-section limit the "owner's cap decision." I had made no such decision.

The clearest numerical argument was 27, presented as the number of positions available across the day. It came from adding five source-code caps: `6 + 5 + 6 + 4 + 6`. But those states belonged to two different calendars. A weekday could reach morning, workday, evening, and late night. A weekend could reach daytime, evening, and late night. There was no date on which one person could visit all five states.

Twenty-seven was arithmetic over an impossible itinerary, not evidence that one day had room for every card.

<figure style="max-width: 640px; margin: 2rem auto;">
  <img src="/img/zabriskie-every-card-will-show-coming-up.jpg" alt="The Coming Up card in the late-hours Lot, listing two Goose shows at Red Rocks and a Phish show at Dick's Sporting Goods Park" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>Farther down the late-hours program: Coming Up, with the next Goose and Phish dates. A single program is intentionally partial; the disputed guarantee concerns the five visits together.</figcaption>
</figure>

The number doesn't explain the 20 missing cards by itself. It establishes something narrower: the capacity argument didn't model the day I had described.

The tests made similar substitutions. A documentation audit promised that every card could appear within a week. A reachability test considered one card at a time, treated that target as long unseen while its competitors were recently shown, and searched for some state in which it could win. A separate adversarial review confirmed that every listed card had a classification and could pass through the ranking system. All three checks passed. None asked whether the same fixed set of eligible cards appeared across one person's five visits on one date.

The implementation also split the final decision between the Go server and the browser. The server ranked cards, but the browser could still group, suppress, and reorder them. That made a passing server test insufficient: it did not cover the entire path that decided what a person saw. The one end-to-end test pinned a single midday response. It checked that the sections surviving that cap rendered, not that five visits covered the catalog.

The checks hadn't lied. They answered weaker questions and the agent presented their passing results as answers to mine. I accepted that evidence as stronger than it was. By then the hallucination was executable: it had become code, tests, and audits that could all turn green together.

## What Lean made explicit

Lean is both a programming language and a proof assistant. It lets us describe a system precisely and then mechanically checks what follows from that description. I had the agent build a Lean model beside Zabriskie's production server, which is written in [Go](https://go.dev/). Lean does not run when somebody opens The Lot, and it does not read or prove the Go source.

### Lean proved the wrong day

The first Lean selector model failed too, but it represented only a subset of the inventory and could not establish the full claim. The agent's next model expanded the inventory and passed by treating weekday morning, workday, evening, late night, and weekend daytime as five consecutive visits. No calendar day is both a weekday and a weekend.

Lean correctly proved coverage for an itinerary no person could take. The theorem was valid. The agent's claim that it represented my day was not.

That proof entered the main source code and became a required continuous-integration check. I merged it because the proof made the agent's explanation feel stronger than an ordinary test. But Lean only checks the statement it receives. It cannot decide whether an agent translated a product requirement faithfully.

### Coverage was still only half the problem

After the calendar was repaired, the assignment-table coverage question became one set equality:

```lean
theorem union_of_five_programs_is_all_modeled_cards :
    cardsAcrossFivePrograms = modeledCardNames := by
  native_decide
```

That says every modeled card is either the structural Hero or assigned to at least one of the five programs. It was easier to inspect than the earlier state-heavy model, but it still omitted part of the product requirement.

The original problem was a 29-card, 15,000-pixel page. A theorem saying the five programs cover the catalog accepts the stupidest possible implementation: show every card on every visit. Coverage is a constraint. The objective is to make each visit as small as possible while satisfying that constraint.

The inventory changed during the repairs. The optimized model discussed from here forward contains 45 code-defined identities, not the earlier incident inventory. One is the structural Hero, which does not consume a supporting-card slot. That leaves 44 supporting cards. The agent-generated repair assigned those cards 54 times across the day, including duplicates, and allowed 10, 13, 11, 10, and 10 supporting cards across the five visits.

The proof was correct, but it had certified a padded schedule. It still had not stated the height objective: minimize the largest supporting-card count across the five visits. The model had only shown that coverage was possible with enough space.

This is where Lean helped me understand what I was actually trying to do. It did not recover the product intent. Reading the theorem literally and challenging every input and unexplained capacity made the requirement inspectable enough for me to see what the agent had left out.

### Proving the minimum

Once stated correctly, the lower bound is simple. There are 44 supporting cards and five visits. If every visit showed at most eight supporting cards, the entire day would contain at most 40 positions. Four cards could not appear. Therefore any covering schedule must allow at least nine supporting cards on one visit.

That counting argument became a general Lean theorem. It takes a catalog with no repeated card identities and the cards shown on each visit. If those visits cover the catalog and no visit contains more than `max` cards, then the catalog cannot contain more than the number of visits multiplied by `max`:

```lean
theorem covering_schedule_capacity_lower_bound
    (catalog : List String)
    (keeps : List (List String))
    (max : Nat)
    (hunique : catalog.Nodup)
    (hcover : covers keeps catalog = true)
    (hmax : ∀ keep ∈ keeps, keep.length ≤ max) :
    catalog.length ≤ keeps.length * max
```

The proof flattens the visit outputs and counts their positions. A 44-card catalog and five visits turn the result into `44 ≤ 5 × max`, so `max` must be at least nine.

### Separating the algorithm from the product schedule

The formal statement now separates two things. Whether ranking and the cap preserve any fitting assignment is an algorithm question. Whether **Connections** belongs at midday is a product decision. Moving a card should require checking the new table, not rewriting the scheduler proof.

The generic theorem runs the selector once for each item in `visits`, then takes the union of those outputs. `visits` does not choose cards; it names the executions whose combined coverage we are proving.

Each execution still needs the inputs used by the real ranker. `staleness` is a per-visit table from card name to score bonus, so it can change which unassigned filler cards survive. The theorem accepts every possible value for that table, whether an event leads the page, and whether the person is on tour. It asks only whether every supporting card has an assigned visit and whether the cards assigned to each visit fit:

```lean
theorem capped_selector_covers_any_fitting_schedule
    (units : List Card)
    (visits : List Daypart)
    (capacity : Daypart → Nat)
    (eventLead onTour : Daypart → Bool)
    (staleness : Daypart → List (String × Nat))
    (hassigned : ∀ card ∈ supportingCards units,
      ∃ visit ∈ visits, ownsProgramPosture card visit = true)
    (hfits : ∀ visit ∈ visits,
      ((supportingCards units).filter fun card =>
        ownsProgramPosture card visit).length ≤ capacity visit) :
    covers
      (cappedSupportingKeeps
        units visits capacity eventLead onTour staleness)
      (supportingNames units) = true
```

`hassigned` says the product has given every supporting card a guaranteed visit. `hfits` says no visit has been assigned more guaranteed cards than it can show. Given those facts, Lean proves that the final ranked-and-capped outputs cover every supporting card. The theorem does not name **Connections**, morning, late, 44 cards, or five visits.

Ranking decides which unassigned fillers survive once the cap binds. The theorem proves the narrower invariant we need: ranking cannot dislodge an assigned card when all assignments fit. The final selectors are called `capCards` in Lean and `lotCapCards` in Go. The theorem follows the supporting-card path inside `capCards`: it admits the assigned cards first, then adds section diversity and fills any remaining positions by rank. The structural Hero is checked separately.

A lower bound alone does not show that nine is achievable. The product still needs a schedule that reaches it. Each supporting card owns exactly one guaranteed program, balanced as:

```text
morning     9
midday      9
afternoon   9
evening     9
late        8
```

Those 44 positions contain the 44 supporting cards exactly once. A card can still prefer several times of day for ranking, but preference does not create another guaranteed assignment.

A separate Lean check applies the generic theorem to this product table. It shows that the capped selector covers all 44 supporting cards while returning at most nine on each visit. Combined with the lower bound, that establishes card-count optimality for this fixed supporting catalog: nine is necessary, and this schedule attains nine.

Within the selector, each output contains the structural Hero plus at most nine modeled supporting cards. The rendered page may also contain **Live Now**, a separate lead, and other modules outside this theorem.

That settled the generic law and one concrete product schedule. It did not yet show that the separately written Go selector made the same choices.

### Making Go answer to the model

The production function is written separately in Go. Lean cannot prove that source directly, so the repository also keeps an executable Lean version of the selector. Unlike the generic theorem, this reference implementation reads the current product table because a comparison needs concrete outputs. Lean generates walks and Go replays them through `lotCapCards`.

This follows the general shape used by [Cedar](https://docs.cedarpolicy.com/other/security.html), the open-source authorization language developed at AWS: prove properties of a Lean model, implement a separate production engine, and use [differential testing](https://en.wikipedia.org/wiki/Differential_testing) to compare their outputs. Our boundary is much smaller, but the idea is the same.

The comparison uses 64 as a small deterministic regression budget, not a mathematically significant sample size: the complete 45-card catalog and 63 fixed pseudorandom subsets. Each runs twice, once with every card unseen and once with every card recently seen, producing 128 five-visit walks.

When the cases were regenerated for the minimal schedule, the Lean-to-Go comparison failed on a filler card. Go allowed a card to prefer several times of day; the executable Lean version allowed only one. The reference implementation was corrected and the generic starvation theorem did not change. The selected-card sets then agreed on all 128 walks. A separate Go unit test constructed the 45 modeled identities and covered them across five visits under two initial histories and all four combinations of event-lead and tour status.

Together, these checks connect the proof to Go. They still do not cover every possible Go input. For current scheduled responses, the server includes the selected cards in a complete `moduleOrder`, which the browser treats as its allow-list and order instead of running another scheduler. Ordinary Go and React tests cover that handoff; no five-visit browser test covers the entire path.

### The fixed-eligibility boundary

For the current 45-card model, the product-instance theorem accepts any fixed eligible subset, including the crowded full catalog. It assumes that the set does not change during the five visits.

That assumption matters. Lean contains a counterexample using **Last Night**, whose guaranteed program is morning. The card is absent from the eligible set during the morning visit, then becomes eligible at midday. By then its guaranteed visit has passed. During midday, afternoon, evening, and late night it can appear only as an unassigned filler, and other cards fill those positions. After all five visits, **Last Night** remains unseen.

The main coverage theorem does not fail because it cannot be applied to that walk. It takes one eligible catalog and reuses it for every visit; the counterexample supplies a different catalog at each visit. Lean checks the failure separately and proves that the missing-card list is exactly **Last Night**.

For the actual **Last Night** flow, that boundary is acceptable. If a person had already RSVP'd to the previous night's show, the card is eligible by morning and its guarantee applies. The counterexample corresponds to someone adding the RSVP retroactively after the show, perhaps the next afternoon. In that case, not receiving a same-day prompt to write a review is acceptable product behavior. The specification deliberately admits that failure rather than complicating the scheduler and theorem to guarantee a case I do not need.

<figure style="max-width: 688px; margin: 2rem auto;">
  <img src="/img/zabriskie-every-card-will-show-last-night.jpg" alt="The Last Night card in dark mode, showing prompts to choose the jam of the night, rate the show, revisit the discussion, and post a recap" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>The Last Night card rendered with its deterministic test fixture: the morning-after prompts for a jam of the night, rating, and recap.</figcaption>
</figure>

## Algorithms are hard

Before the final decomposition, the agents had already produced an implementation, unit tests, end-to-end tests, documentation, multiple code audits, numerical arguments, and accepted formal proofs. Those earlier passing artifacts said the requirement had been implemented correctly. It had not.

Those artifacts agreed because the same failure pattern propagated from one to the next. One calendar day became a week. Guaranteed coverage became isolated reachability. Capacities from mutually exclusive programs were added together. Later, coverage became the whole requirement, allowing a proof to certify an unnecessarily large schedule. Green checks showed that each substituted design behaved as specified. They were not independent evidence that we had solved the original problem.

After all of that, the fixed-catalog problem was finally easy to say: for the cards that remain eligible across all five visits, minimize the largest supporting-card count on any visit while showing each card at least once. Saying it clearly did not make it easy for an agent to preserve both parts through an implementation, tests, audits, and proofs.

Lean's value was not another green artifact. It forced the argument into pieces I could challenge: a counting lower bound, a generic selector guarantee, a replaceable product schedule, and a bounded comparison against Go. Lean did not discover that decomposition. The agents also produced accepted proofs of an impossible day and of coverage without minimization. Reading those statements literally exposed the missing objective, but deciding whether a theorem described the product remained my job.

This wasn't one model having one bad run. I tried to repair the work with Claude Opus 4.6, Claude Opus 4.8, Claude Opus 5, and Codex using GPT-5.6 Sol. None independently arrived at both the invariant I had stated and an implementation that preserved it. They found different problems and produced more artifacts, but changing models didn't break the pattern. Each could accept or recreate a nearby requirement and then generate convincing evidence for its own version of the problem.

Three things now seem clear. Passing artifacts are not independent evidence when they inherit the same changed requirement. Lean proves the statement it receives, not the behavior I meant. And the formal invariant, the product schedule, and the check against the implementation have to remain separate and inspectable.

The next problem is whether an agent can expose that decomposition before it has already built the implementation, the tests, and the proof around a nearby requirement.
