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

Lean is both a programming language and a proof assistant, a tool for describing a system in logic and checking claims about that description. We used it to build an independent model of The Lot beside the server used by the live app, which is written in the [Go programming language](https://go.dev/). Lean does not run when somebody opens The Lot, and it does not read the Go source.

The Lean code ended up doing two jobs. It checked whether the catalog was covered across five programs, and it reproduced enough of the production selector to compare Lean's output with Go's. Those jobs require different levels of detail. The coverage property needs program assignments, reservations, and capacities. The production comparison also needs the scoring and ordering rules. Those extra rules are part of the implementation twin, not the starvation argument.

We followed the verification shape used by [Cedar](https://docs.cedarpolicy.com/other/security.html), the open-source authorization language developed at AWS. Cedar has a model in Lean and a separate Rust engine, then uses [differential testing](https://en.wikipedia.org/wiki/Differential_testing) to compare their results. The proofs establish properties of the model; the comparisons check that the production implementation still behaves like it.

### Lean proved the wrong day

The agent wrote the first Lean coverage model too. I am still not sure whether asking an agent to formalize a requirement agents had repeatedly misunderstood was a good idea. But the model failed, which was useful: it was where we discovered that 20 of the 47 cards were starved. The dangerous version was the agent's next one. It passed, but it modeled the wrong day. It treated weekday morning, workday, evening, late night, and weekend daytime as five consecutive visits. A single calendar day cannot be both a weekday and a weekend.

Lean proved that every card appeared in that impossible sequence. The theorem was valid. The agent's claim that it represented my one-day requirement was not.

That passing version was added to the main source code, and its Lean check became required in continuous integration (CI), the automated test gate for every proposed code change. I merged it. The presence of a proof made the result feel stronger than it was, and I accepted the agent's description of the modeled day.

This is where formal verification, using mathematical proof to check a software claim, meets the hallucination problem. Lean checked the statement it was given. It could not check whether the agent had translated my product requirement faithfully.

### The proof was still too complicated

Fixing the impossible day did not fix the shape of the proof. The next version still ran a production-like selector inside the coverage theorem. It carried a `localDay` value that did not influence which program Lean chose, checked two viewing histories even though history could change order and filler but not which cards had reserved room, and ran four fixed event and tour combinations. All of that was real implementation behavior. None of it was the starvation invariant.

We eventually found that none of those inputs belonged in the coverage theorem. The theorem was valid for the executions it modeled, but the agent had reproduced the machinery around my question instead of stating the invariant directly.

Eventually I asked:

> Is the union of the cards across the five daily programs equal to the set of all available cards?

That is the theorem we should have written first. I had the agent replace the old coverage theorem with that statement and rerun the complete Lean and Go verification loop. It passed.

### The theorem we actually needed

The repaired product defines the same five programs every day: morning, midday, afternoon, evening, and late night. A separate Go test sends five local times through the production hour-to-program resolver on a representative weekday and weekend date. On each date, those five visits must produce the full sequence. The Lean coverage theorem does not need a date or an hour.

`modeledCardNames` is simply the 45 names in Lean's fixed catalog. Lean does not decide which cards are eligible in a live request. We use the full catalog as the most crowded case by treating every ranked card identity as if it has content to render. If the full catalog's reservations fit, a fixed subset can only require fewer slots. This does not cover eligibility changing between visits.

The model collects the cards reserved in each program, adds the structural Hero that is always present, and compares that union with the catalog. `nameSet` sorts the names and removes duplicates so the two lists represent sets:

```lean
def modeledCardNames : List String :=
  nameSet fullNames

def cardsAcrossFivePrograms : List String :=
  nameSet (structuralNames ++ programs.flatMap reservedNames)

theorem union_of_five_programs_is_all_modeled_cards :
    cardsAcrossFivePrograms = modeledCardNames := by
  native_decide
```

`native_decide` evaluates this finite equality and turns the result into a checked proof. **Connections** is reserved in two programs, but set equality removes the duplicate and only requires the card to appear once. **Live Now** and other uncapped modules are outside this selector.

This is the assignment invariant, not yet a proof of what reached the browser. A separate theorem checks that each program has enough room for its reservations. The more detailed model handles history, scoring, ordering, and modes so it can be compared with Go. Those checks connect the small theorem to the implementation without putting the implementation back inside the theorem.

### Connecting Lean back to Go

But did the Go code make the same decisions as the model?

This is where the additional model detail matters. The production selector still contains scoring and ordering rules, even though starvation should not depend on them. The Lean twin reproduces those rules so the compared cases fail if Go moves the reservation pass or lets an assigned card lose to filler.

The supporting-card selector has far too many possible input catalogs to compare exhaustively. Instead, Lean writes a fixed sample of 128 selector walks, including the complete modeled catalog and reproducible subsets across the four event-lead and tour configurations. That number is a chosen test budget, not a total derived from the model.

Go replays those walks through `lotCapCards`, the production function that selects supporting cards, and must return the same lists Lean produced. A separate Go unit test constructs a synthetic all-applicable inventory with the same modeled names and runs the real selector across the four configurations. These checks exercise the production selection function, not the full request-to-browser path.

That is a useful bridge, but it isn't proof that Lean and Go agree for every possible input. Lean checks coverage inside its model. The supporting-card comparison is sampled, with a full-catalog case and a separate full-catalog Go unit test. These are stronger and more explicit checks than we had before, but they establish different things.

What happened after the selector returned? A checked result was still useless if the browser could discard or reorder it. We therefore centralized scheduling-time eligibility, deduplication, caps, sections, and ordering on the server. The checked selector owns the cap and reservation decision. The surrounding server code resolves candidates and produces `moduleOrder`, the final allow-list and order, plus `moduleSections` for placement. The browser follows those instructions instead of running its old second scheduling policy.

The browser still honors explicit user actions such as local dismissals, and conventional tests cover the handoff from server identities to rendered components. This doesn't turn the theorem into proof of pixels. It closes the known hole where a later scheduling layer could silently weaken the selector's result.

These checks now run on every proposed code change. A failed Lean theorem, stale generated data that no longer matches Lean, a disagreement on one of the differential cases, or a missing card in the synthetic Go walk fails the check. The old independent caps were replaced with capacities sized to each program's assigned cards, and Lean fails if an assignment count exceeds its corresponding capacity.

This was the first version I would accept as implementing the coverage design. Lean didn't certify the whole product or prove every behavior of the ranking algorithm. It checked the complete catalog against explicit program assignments and capacities, while the additional tests made it much harder for the production selector to drift away without being noticed. That is different from producing another artifact that merely says the implementation looks correct.

## Algorithms are hard

By then the agent had produced an implementation, unit tests, end-to-end tests, documentation, multiple code audits, and several numerical arguments. Each artifact said, in its own way, that the requirement had been implemented correctly. It had not.

Those artifacts agreed because the same failure pattern propagated from one artifact and agent to the next. One calendar day became a week. Guaranteed coverage became isolated reachability. Capacities from mutually exclusive programs were added together. Scheduling was divided between the server and browser. Then tests and audits treated those invented decisions as requirements. Green tests were not independent evidence about the invariant I had asked for. They were evidence that the substituted designs behaved as specified.

The invariant was easy to state: across one person's five visits on one calendar day, every available and relevant card should appear at least once. That did not make the algorithm easy to implement correctly. The agent could read the specification, write the implementation, write unit and end-to-end tests, and audit its own work, but it repeatedly failed to keep the invariant intact. Each time it encountered an ambiguity or an implementation obstacle, it invented a nearby requirement instead of asking me.

The power we eventually got from Lean wasn't another claim that the code looked right. Once the five daily programs, the card assignments, the reservation pass, and the capacities were explicit, Lean could check that the modeled catalog left no card behind. Go then had to match the sampled supporting-card comparisons, while a separate unit test walked the real Go selector through the full modeled catalog. This didn't prove every Go input, changing eligibility, changing modes, or the whole product. It gave us a checked property with an explicit boundary and strong evidence that the production selector agreed on the cases we compared.

Lean itself was not immune to the problem. The first passing model contained the wrong itinerary. The repair was valid but still buried the invariant under dates, history, modes, and scoring until I questioned each input while editing this post. A theorem can make a misunderstanding more convincing, or make a simple claim needlessly difficult to inspect. The important step was not adding a proof to the artifact pile. It was reducing the theorem to the invariant I could evaluate, checking it, and then connecting that small claim to the production implementation.

This wasn't one model having one bad run. I tried to repair the work with Claude Opus 4.6, Claude Opus 4.8, Claude Opus 5, and Codex using GPT-5.6 Sol. None independently arrived at both the invariant I had stated and an implementation that preserved it. They found different problems and produced more artifacts, but changing models didn't break the pattern. Each could accept or recreate a nearby requirement and then generate convincing evidence for its own version of the problem.

That is the takeaway for me. Algorithms are hard, even when their invariants are easy to say. AI can now read a specification, write an implementation, generate unit tests, generate end-to-end tests, and produce several audits explaining why everything is correct. It cannot guarantee an invariant merely by generating all the artifacts that say the invariant holds. If they all inherit an unexamined change to the requirement, their agreement means very little.

For an algorithm whose correctness depends on an invariant, the invariant needs an executable form and an explicit bridge to the implementation. That bridge may check every input, sample some inputs, or rely on ordinary tests, and those offer different levels of confidence. Even then, the guarantee is only about the formal statement, so a person still has to be shown, in terms they can evaluate, that the statement describes the requirement they actually meant.

The final test should have been the first. The hard part is stating it before an AI has built the implementation, the tests, and the argument for trusting a nearby one.
