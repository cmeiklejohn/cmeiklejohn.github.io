---
layout: post
title: "Every Card Will Show"
subtitle: "The implementation, tests, audits, and proofs all said the algorithm was correct. They were not checking the same thing."
date: 2026-08-24 00:00:00 -0400
group: ai
editorial_review: three-pass
categories: ai zabriskie development agents
---

[Zabriskie](https://zabriskie.app/) is a social app I am building around music, films, books, art, and the people who care about them. It isn't organized around followers. Instead, it uses shared shows and overlapping taste to help people find culture and one another. People can post about albums, films, books, and concerts, say whether they are going to a show or watching from home, talk while a show is live, and keep a history of what they have seen. I wrote about that live-show experience in [The Whole Night](/ai/zabriskie/development/2026/08/17/the-whole-night.html).

Zabriskie is also an experiment in building an entire application through vibe coding. I describe the behavior I want and evaluate what the product does in the browser, but I don't read the implementation code. Coding agents write the implementation, tests, and audits.

Zabriskie's home screen is called [The Lot](https://zabriskie.app/v2/lot). It isn't one permanent feed. The Lot is assembled from cards, individual blocks that might contain a live show, a recommendation, a deadline, or something from a person's history. Today it changes between five scheduled versions over the course of a day: morning, midday, afternoon, evening, and late night. I call each of those versions a program in this post.

Late on August 23, after spending most of two days trying to repair The Lot, I asked the AI coding agent implementing it a specific question.

> If one person visited those five programs during one calendar day, would they see every card whose content was available and relevant to them?

The answer was yes. Every card would show.

That wasn't true. After the earlier attempts kept failing, I got frustrated and asked the agent to formalize the requirement in Lean. Only then did we discover the scale of the failure: of the 47 applicable cards the agent modeled, 20 didn't appear.

I think this is a useful kind of hallucination to call out. The agent hadn't invented a file, a function, or a test result. It had produced real code and real evidence. What it invented was the connective claim: because each piece of evidence established something close to my requirement, it was treated as establishing the requirement itself.

This is a story about an AI producing an implementation, tests, audits, and numerical arguments that all said the algorithm was correct. Each artifact embodied or checked a weaker claim near my requirement: coverage within a week, isolated reachability, catalog completeness, or eventually coverage across an impossible itinerary. None checked whether one person's five visits on the same day exposed every relevant card.

The work behind those artifacts began with a concrete problem. The earlier Lot was one long list: one version rendered 29 cards and produced a page more than 15,000 pixels tall. Showing everything at once wasn't a good answer. The scheduling algorithm was supposed to solve both problems at once: make each visit shorter and appropriate to that moment of the day while distributing the full set across five visits instead of silently dropping cards.

Each program is divided into named sections. A deadline belongs in **Act Now**. Music recommendations belong in **Discover**. Personal history belongs in **You**. The large lead card at the top, which the code calls the **Hero**, should remain first. No single program had to show everything. The promise was that the five programs together would not hide a relevant card from the person for the entire day.

<figure style="max-width: 688px; margin: 2rem auto;">
  <img src="/img/zabriskie-every-card-will-show-late-hero.jpg" alt="The late-hours Lot on August 23, showing a live King Gizzard and the Lizard Wizard Hero followed by an Act Now section with four live shows" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>The Lot at 11:35 PM on August 23: the late-hours program, a live Hero, and Act Now beneath it.</figcaption>
</figure>

I spent roughly ten hours on this on August 22 and another twelve on August 23. [Lean](https://lean-lang.org/), a tool for writing and checking precise claims about software, was a small part near the end. Most of the time went into discovering how many different versions of my requirement had been implemented and certified without satisfying it.

I kept merging because I asked for comprehensive audits before doing so. Three were completed, and all three said the implementation was working correctly. I wasn't ignoring evidence that contradicted the claim. The audits themselves had inherited those weaker versions of the requirement.

That responsibility doesn't make the intermediate decisions mine. I asked for one person to see every relevant card across five visits in one day. The agent introduced limits, scores, classifications, a weekly window, tests that considered each card separately, and a design that let two different parts of the app decide what appeared. It didn't ask me about any of them. These weren't clarifications of my requirement. They were new product rules, a new definition of success, and a new software design.

## What the agent invented instead

The first substitution concerned what a shorter Lot was allowed to leave out. I had asked for the cards to be distributed across the day. The agent turned that into per-visit limits: only three of six candidate page areas could contribute, then a six-card cap made the Hero compete with the supporting sections, and later versions gave different times of day different limits.

The agent also wrote a ranking policy and a catalog of card identities. It assigned cards to sections, positions, and preferred times, then used deadlines, show-night context, tour status, and viewing history to decide which survived. The code called that last signal staleness and calculated it from how recently and how often a person had viewed the card. My requirement was that every relevant card have an opportunity to appear across the day without being starved. The agent chose these rules as its way of implementing that requirement. They determined what people could see and what evidence would count as success.

Some choices were plainly wrong. **Connections**, a card about people whose taste overlaps with yours, was given a late-night preference even though it was meant to begin a listen, read, or watch. Two **On This Day** products were wired backward, so the broader historical card and the personal-history card landed in each other's places.

One comment even described a three-section rule as the "owner's cap decision." I had made no such decision. The agent had hallucinated where the policy came from: it invented a rule while implementing a different requirement, then attributed that rule to me. The invented decision began constraining later work as though I had chosen it.

The clearest numerical argument was 27, presented as the number of card positions available across the day. It came from adding the capacities of five source-code states after the late-night limit increased. But those states described two different schedules, not five stops in one day:

- A weekday could reach weekday morning, workday, evening, and late night.
- A weekend could reach weekend daytime, evening, and late night.

Evening and late night appeared on both schedules, but the other programs were mutually exclusive. Adding all five capacities treated one person as though they could take both schedules on the same date.

Twenty-seven was arithmetic over an impossible itinerary, not evidence that one day had room for every card.

<figure style="max-width: 640px; margin: 2rem auto;">
  <img src="/img/zabriskie-every-card-will-show-coming-up.jpg" alt="The Coming Up card in the late-hours Lot, listing two Goose shows at Red Rocks and a Phish show at Dick's Sporting Goods Park" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>Farther down the late-hours program: Coming Up, with the next Goose and Phish dates. A single program is intentionally partial; the disputed guarantee concerns the five visits together.</figcaption>
</figure>

The number doesn't explain those 20 missing cards by itself. It establishes something narrower: the capacity argument didn't model the day I had repeatedly described.

## What the checks actually established

Before the first passing Lean model, three different pieces of assurance said the system was complete. None was the test I had requested.

The first was a documentation audit. It reported 42 "cappable" cards and concluded that no card was permanently prevented from appearing. But the audit replaced my one-day requirement with a promise that a person could see every card within a week. I had not asked for a week. The agent hallucinated that time window, then audited its own version of the requirement. It made no code change and did not test five visits in one day.

The second was a reachability test, a test that ran the selection code rather than inspecting documentation. The agent replaced my requirement with a different one: for each card considered separately, there should be at least one favorable situation in which it could appear. My requirement asked whether all the cards appeared across the same person's five visits on one day. The test took each card separately, gave that card the maximum staleness bonus, treated comparable cards in its tier as recently shown, and searched the five time-of-day states for somewhere it could win. **Act Now** cards were divided into deadline and non-deadline groups, and test-only stand-ins represented the other tiers.

The test established exactly that individual reachability claim: every card could win in some favorable scenario. It did not establish my shared-day claim because each target card received its own specially prepared world, and those worlds could contradict one another.

The third assurance was an adversarial review, a separate agent trying to find mistakes in the catalog the first agent had invented. It confirmed that every listed card type had an explicit classification and could pass through the ranking system. That was another check of the substituted requirement, not a test of the one-person journey I had asked for.

There was another unrequested implementation decision. The agent split the scheduling policy between the server, which chose the card data, and the browser, which turned that data into the visible page. I had not asked for both layers to decide what appeared. Before the reachability work, the browser contained its own fixed ordering for cards within each section. The next version added an order from the server, but the browser still grouped cards, suppressed some of them, constructed sections, ordered those sections, and treated the Hero as an ordinary scored position. Every extra decision point created another place for a card to disappear, making the original guarantee harder to implement and verify.

A passing server test was therefore not yet a passing product test. The thing being certified was not the entire path that decided what the person saw. One end-to-end test crossed that boundary, but it pinned a single midday response and checked that every section surviving that one cap rendered. It did not ask whether the union of one person's five visits covered the catalog.

The checks hadn't lied. The agent had encoded weaker questions, then treated their passing results as answers to mine. I accepted that evidence as stronger than it was. At that point the hallucination had become executable, encoded in checks that could pass. A documentation audit, a test, and a review could all succeed, making an untested guarantee feel like a fact.

## What Lean made explicit

What would it take to test the question I had actually asked?

Lean is both a programming language and a proof assistant, a tool for describing a system in logic and checking claims about that description. We used it to build a small, independent model of The Lot beside the server used by the live app, which is written in the [Go programming language](https://go.dev/). Lean does not run when somebody opens The Lot, and it does not read the Go source. We rewrote only the pieces needed for this claim, then made the two implementations compare results.

The shape of this setup came from [Cedar](https://docs.cedarpolicy.com/other/security.html), the open-source language developed at AWS for deciding who is allowed to do what in an application. Cedar has a model written in Lean and a separate engine, the Rust program that makes real authorization decisions. It connects them with [differential testing](https://en.wikipedia.org/wiki/Differential_testing): generate an input, run it through both implementations, and check that they produce the same result. The proofs establish properties of the model; the differential tests check that the live engine continues to behave like it. We followed this model for the bounded part of The Lot that chooses the lead and supporting cards.

### Lean proved the wrong day

The agent wrote the first Lean coverage model too. I am still not sure whether asking an agent to formalize a requirement agents had repeatedly misunderstood was a good idea. But the model failed, which was useful: it was where we discovered that 20 of the 47 cards were starved. The dangerous version was the agent's next one. It passed, but it modeled the wrong day. It treated weekday morning, workday, evening, late night, and weekend daytime as five consecutive visits. A single calendar day cannot be both a weekday and a weekend.

Lean proved that every card appeared in that impossible sequence. The theorem was valid. The agent's claim that it represented my one-day requirement was not.

That passing version was added to the main source code, and its Lean check became required in continuous integration (CI), the automated test gate for every proposed code change. I merged it. The presence of a proof made the result feel stronger than it was, and I accepted the agent's description of the modeled day.

This is where formal verification, using mathematical proof to check a software claim, meets the hallucination problem. Lean checked the statement it was given. It could not check whether the agent had translated my product requirement faithfully.

### Fixing the five-visit schedule

The repaired Lean model uses one fixed walk: morning, midday, afternoon, evening, and late night. That removes the impossible mixture of weekday and weekend programs from the coverage calculation.

Lean does not model a calendar date or determine its day of the week. It assumes that all five programs are available during one day, then asks what the selector shows across that walk. Separate Go tests send 8 AM, noon, 3 PM, 8 PM, and 11 PM on both a Wednesday and a Sunday through the production clock resolver. Those tests, not the Lean model, check that the five-program walk is possible on a real date.

The coverage model concerns the ranked cards that rotate through The Lot's sections. **Live Now** and other uncapped modules bypass that selector, so they are outside this claim. When an independent Hero exists, the model keeps it without charging it against the supporting-card limit. A separate Lean model checks which already-built candidate, if any, is chosen to lead the page.

Each identity becomes a typed record containing its section, deadline status, position, and preferred time. Each non-Hero identity also receives at least one program where it is guaranteed room. The code calls a section a `Tier`, a preferred time an `affinity`, and a guaranteed program a `posture`:

```lean
structure Card where
  name : String
  tier : Tier
  deadline : Bool := false
  renderOrder : Nat := 0
  affinity : Affinity := .none
  posture : Daypart := .none
  additionalPostures : List Daypart := []
```

`additionalPostures` means additional guaranteed programs. Most cards are guaranteed room in one program. **Connections** is guaranteed room at both midday and afternoon because either window leaves time to follow the card into a listen, read, or watch. It remains one card, and the theorem doesn't require it to appear twice. Lean didn't choose this policy; the model records the choice and checks what follows from it.

### Reserving room before ranking

The important rule is simpler than the ranking system. For each program, the selector first keeps every applicable card assigned to that program. Only then does ranking fill any positions left over. Ranking can change the order and the filler, but it is not supposed to decide whether an assigned card receives its guaranteed opportunity.

Lean expresses the reservation pass as `takePhase0`. It walks the ranked list and keeps every card that owns the current program until the program reaches its capacity:

```lean
def takePhase0 (sorted : List Card) (d : Daypart)
    (max : Nat) : List String :=
  sorted.foldl (init := []) fun keep u =>
    if keep.length >= max then keep
    else if ownsProgramPosture u d then keep ++ [u.name]
    else keep
```

The capacities are sized to the assignments in the catalog. Lean checks the number of cards assigned to morning, midday, afternoon, evening, and late night:

```lean
theorem programmed_capacities_match_catalog :
    (fivePostures.map fun posture =>
      (fullCatalog.filter fun c => ownsProgramPosture c posture).length) =
    [10, 13, 11, 10, 10] := by
  native_decide
```

Those are also the five program limits. In other words, the guarantee comes from assigning every ranked card at least one program, reserving those cards before ordinary ranking, and providing enough room for all the assignments. Ranking determines order and fills holes left by cards that are not applicable. It is not the source of the no-starvation guarantee.

The Lean implementation also mirrors the rest of production's ranking so the two selectors can be compared. That machinery is part of the implementation model, not the reason the coverage result holds.

Lean runs the complete catalog through the five modeled programs. Two small helpers turn those outputs into the coverage question:

```lean
def covered (keeps : List (List String)) (name : String) : Bool :=
  keeps.any (fun visit => visit.contains name)

def missing (keeps : List (List String)) (catalog : List String) :
    List String :=
  (catalog.filter (fun name => !(covered keeps name))).mergeSort
    (fun a b => a ≤ b)
```

`covered` asks whether one card name appears in at least one of the five lists. `missing` applies that question to the complete modeled catalog. The coverage checks require the result to be empty under each fixed event-lead and tour configuration they evaluate.

This is a finite check of the current catalog and modeled selector. It does not prove that arbitrary eligibility or mode changes during the day preserve coverage. The structural protection is the reservation pass and the matching capacities above: ordinary ranking happens after the cards promised room in that program have been kept.

### Connecting Lean back to Go

But did the Go code make the same decisions as the model?

The product makes two different top-of-page decisions. One chooses a primary lead from ten already-built candidates. The ranked catalog separately includes `hero:hero`, a structural identity that lets the coverage model account for an independent Hero without charging it against supporting capacity. The primary-lead chooser has its own smaller check: ten yes-or-no values representing whether each lead candidate is present, plus one of six program values. That produces 2<sup>10</sup> × 6, or 6,144, possible inputs. Lean writes the expected lead choice for every one, and Go must match all 6,144 rows. Candidate construction and rendering are outside that exhaustive comparison.

The supporting-card selector has far too many possible input catalogs to compare exhaustively. Instead, Lean writes a fixed sample of 128 selector walks, including the complete modeled catalog and reproducible subsets across the four event-lead and tour configurations. That number is a chosen test budget, not a total derived from the model.

Go replays those walks through `lotCapCards`, the production function that selects supporting cards, and must return the same lists Lean produced. A separate Go unit test constructs a synthetic all-applicable inventory with the same modeled names and runs the real selector across the four configurations. These checks exercise the production selection function, not the full request-to-browser path.

That is a useful bridge, but it isn't proof that Lean and Go agree for every possible input. Lean proves the coverage theorem inside its model. The primary-lead comparison is exhaustive for its narrow input. The supporting-card comparison is sampled, with a full-catalog case and a separate full-catalog Go unit test. These are stronger and more explicit checks than we had before, but they establish different things.

What happened after the selector returned? A checked result was still useless if the browser could discard or reorder it. We therefore centralized scheduling-time eligibility, deduplication, caps, sections, and ordering on the server. The checked selector owns the cap and reservation decision. The surrounding server code resolves candidates and produces `moduleOrder`, the final allow-list and order, plus `moduleSections` for placement. The browser follows those instructions instead of running its old second scheduling policy.

The browser still honors explicit user actions such as local dismissals, and conventional tests cover the handoff from server identities to rendered components. This doesn't turn the theorem into proof of pixels. It closes the known hole where a later scheduling layer could silently weaken the selector's result.

These checks now run on every proposed code change. A failed Lean theorem, stale generated data that no longer matches Lean, a disagreement on one of the differential cases, or a missing card in the synthetic Go walk fails the check. The old independent caps were replaced with capacities sized to the assigned cards, currently 10, 13, 11, 10, and 10, and Lean checks that those assignments still fit.

This was the first version I would accept as implementing the coverage design. Lean didn't certify the whole product or prove every behavior of the ranking algorithm. It checked the complete catalog against explicit program assignments and capacities, while the additional tests made it much harder for the production selector to drift away without being noticed. That is different from producing another artifact that merely says the implementation looks correct.

## Algorithms are hard

By then the agent had produced an implementation, unit tests, end-to-end tests, documentation, multiple code audits, and several numerical arguments. Each artifact said, in its own way, that the requirement had been implemented correctly. It had not.

Those artifacts agreed because the same failure pattern propagated from one artifact and agent to the next. One calendar day became a week. Guaranteed coverage became isolated reachability. Capacities from mutually exclusive programs were added together. Scheduling was divided between the server and browser. Then tests and audits treated those invented decisions as requirements. Green tests were not independent evidence about the invariant I had asked for. They were evidence that the substituted designs behaved as specified.

The invariant was easy to state: across one person's five visits on one calendar day, every available and relevant card should appear at least once. That did not make the algorithm easy to implement correctly. The agent could read the specification, write the implementation, write unit and end-to-end tests, and audit its own work, but it repeatedly failed to keep the invariant intact. Each time it encountered an ambiguity or an implementation obstacle, it invented a nearby requirement instead of asking me.

The power we eventually got from Lean wasn't another claim that the code looked right. Once the five daily programs, the card assignments, the reservation pass, and the capacities were explicit, Lean could check that the modeled catalog left no card behind. Go then had to match every primary-lead case and the sampled supporting-card comparisons, while a separate unit test walked the real Go selector through the full modeled catalog. This didn't prove every Go input, changing eligibility, changing modes, or the whole product. It gave us a checked property with an explicit boundary and strong evidence that the production selector agreed on the cases we compared.

Lean itself was not immune to the problem. The first passing Lean model contained the wrong itinerary, and I let it merge without catching that mismatch. A theorem can make a misunderstanding more convincing if nobody checks that the formal statement still matches the request. The important step was not adding a proof to the artifact pile. It was making a precise, bounded version of my original invariant explicit, checking it, connecting it to the production implementation, and refusing to let a later layer weaken it.

This wasn't one model having one bad run. I tried to repair the work with Claude Opus 4.6, Claude Opus 4.8, Claude Opus 5, and Codex using GPT-5.6 Sol. None independently arrived at both the invariant I had stated and an implementation that preserved it. They found different problems and produced more artifacts, but changing models didn't break the pattern. Each could accept or recreate a nearby requirement and then generate convincing evidence for its own version of the problem.

That is the takeaway for me. Algorithms are hard, even when their invariants are easy to say. AI can now read a specification, write an implementation, generate unit tests, generate end-to-end tests, and produce several audits explaining why everything is correct. It cannot guarantee an invariant merely by generating all the artifacts that say the invariant holds. If they all inherit an unexamined change to the requirement, their agreement means very little.

For an algorithm whose correctness depends on an invariant, the invariant needs an executable form and an explicit bridge to the implementation. That bridge may check every input, sample some inputs, or rely on ordinary tests, and those offer different levels of confidence. Even then, the guarantee is only about the formal statement, so a person still has to be shown, in terms they can evaluate, that the statement describes the requirement they actually meant.

The final test should have been the first. The hard part is stating it before an AI has built the implementation, the tests, and the argument for trusting a nearby one.
