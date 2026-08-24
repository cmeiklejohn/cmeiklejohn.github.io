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

This is a story about an AI producing an implementation, tests, audits, and numerical arguments that all said the algorithm was correct. Each artifact embodied or checked a weaker claim near my requirement: coverage within a week, isolated reachability, catalog completeness, or eventually coverage across an impossible itinerary. None checked whether one person's five visits, carrying one viewing history through the day, exposed every relevant card.

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

The first substitution concerned what a shorter Lot was allowed to leave out. I had asked for the cards to be distributed across the day. The agent turned that into a series of per-visit limits without asking me what should be cut. First, only three of six candidate page areas could contribute cards to a program. Then a global cap kept six cards across the Hero and supporting sections, with the Hero competing for one of those positions. Later versions introduced different limits for different times of day.

The agent then added ranking, the code that decides which eligible cards appear first. Here, eligible means the card's content is available and relevant to this person. Deadlines received a 1,000-point advantage. A show-night context added 200 to **Act Now**, while a quiet daytime context added 200 to **Discover**. Attending a run of shows added 60 to **Your Couch Tour** and **Your Circle**. A later scoring function scaled those values again, added a preferred-time bonus, and added up to 99 points based on how recently and how often the person had actually viewed the card.

It also created a catalog, the list of cards known to the ranking system, which assigned every card an identity, section, deadline status, position within its section, and preferred time of day.

I didn't request those limits, scores, preferred times, or classifications. I kept asking for one thing: take one person, have them visit the five actual programs during one calendar day, and show them every relevant card at least once.

The agent never brought these choices back to me as product questions. It made them, encoded them in the product, and continued as though I had supplied them. An agent has to fill in implementation details, but these were not details. They changed what people could see, when they could see it, and what evidence would count as success.

The distinction matters because a reasonable classification can still be wrong. **Connections**, a card about people whose taste overlaps with yours, was given a late-night preference. Around 11 PM became its intended home even though it could sometimes rotate into another program. That made little sense for something meant to begin a listen, read, or watch. Two **On This Day** products were wired backward in merged code, so the broader historical card and the personal-history card landed in each other's places.

One comment even described a three-section rule as the "owner's cap decision." I had made no such decision. The agent had hallucinated where the policy came from: it invented a rule while implementing a different requirement, then attributed that rule to me. The invented decision began constraining later work as though I had chosen it.

The most revealing number was 27. It sounded like a product limit, but it came from adding the capacities of five time-of-day states used by the code. Initially those limits totaled 26. When late night increased from three cards to four, they totaled 27.

The problem was that those five states couldn't occur during one day.

Before the repair, The Lot did not yet use the five daily programs described above. Its clock chose a program partly from the day of the week. A weekday could reach weekday morning, workday, evening, and late night. A weekend day could reach weekend daytime, evening, and late night. The five names in the source code were therefore possible programs across a week, not five stops available on one date.

On a weekday, one visit to each available program exposed 20 capped card positions before the late-night increase and 21 after it. On a weekend, the equivalent totals were 15 and then 16. Those totals included the Hero as an ordinary capped position. Repeated visits could rotate cards within a program, but there was no calendar day containing all five states that had been added together.

The 27-position limit was therefore not evidence about a real person's day. It was arithmetic over an impossible itinerary.

<figure style="max-width: 640px; margin: 2rem auto;">
  <img src="/img/zabriskie-every-card-will-show-coming-up.jpg" alt="The Coming Up card in the late-hours Lot, listing two Goose shows at Red Rocks and a Phish show at Dick's Sporting Goods Park" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>Farther down the late-hours program: Coming Up, with the next Goose and Phish dates. A single program is intentionally partial; the disputed guarantee concerns the five visits together.</figcaption>
</figure>

The capacity argument doesn't explain those 20 missing cards by itself. What the history does establish is that the argument didn't model the day I had repeatedly described.

## What the checks actually established

Before the first passing Lean model, three different pieces of assurance said the system was complete. None was the test I had requested.

The first was a documentation audit. It reported 42 "cappable" cards and concluded that no card was permanently prevented from appearing. But the audit replaced my one-day requirement with a promise that a person could see every card within a week. I had not asked for a week. The agent hallucinated that time window, then audited its own version of the requirement. It made no code change and did not test five visits in one day.

The second was a reachability test, a test that ran the selection code rather than inspecting documentation. I had not asked whether each card could appear somewhere in isolation. The agent hallucinated that substitute requirement, then built a test for it. The test took each card separately, gave that card the maximum staleness bonus, treated comparable cards in its tier as recently shown, and searched the five time-of-day states for somewhere it could win. **Act Now** cards were divided into deadline and non-deadline groups, and test-only stand-ins represented the other tiers.

The test established the claim it was built to establish: every card can win in some favorable scenario.

It did not establish mine: that one person, carrying one consistent history through five chronological visits, sees every relevant card across those visits. Each target card received its own specially prepared world. Those worlds could contradict one another.

The third assurance was an adversarial review, a separate agent trying to find mistakes in the catalog the first agent had invented. It confirmed that every listed card type had an explicit classification and could pass through the ranking system. That was another check of the substituted requirement, not a test of the one-person journey I had asked for.

There was another unrequested implementation decision. The agent split the scheduling policy between the server, which chose the card data, and the browser, which turned that data into the visible page. I had not asked for both layers to decide what appeared. Before the reachability work, the browser contained its own fixed ordering for cards within each section. The next version added an order from the server, but the browser still grouped cards, suppressed some of them, constructed sections, ordered those sections, and treated the Hero as an ordinary scored position. Every extra decision point created another place for a card to disappear, making the original guarantee harder to implement and verify.

A passing server test was therefore not yet a passing product test. The thing being certified was not the entire path that decided what the person saw. One end-to-end test crossed that boundary, but it pinned a single midday response and checked that every section surviving that one cap rendered. It did not carry one history through five visits or ask whether their union covered the catalog.

The checks hadn't lied. The agent had encoded weaker questions, then treated their passing results as answers to mine. I accepted that evidence as stronger than it was. At that point the hallucination had become executable, encoded in checks that could pass. A documentation audit, a test, and a review could all succeed, making an untested guarantee feel like a fact.

## What Lean made explicit

What would it take to test the question I had actually asked?

Lean is both a programming language and a proof assistant, a tool for describing a system in logic and checking claims about that description. We used it to build a small, independent model of The Lot beside the server used by the live app, which is written in the [Go programming language](https://go.dev/). Lean does not run when somebody opens The Lot, and it does not read the Go source. We rewrote only the pieces needed for this claim, then made the two implementations compare results.

The shape of this setup came from [Cedar](https://docs.cedarpolicy.com/other/security.html), the open-source language developed at AWS for deciding who is allowed to do what in an application. Cedar has a model written in Lean and a separate engine, the Rust program that makes real authorization decisions. It connects them with [differential testing](https://en.wikipedia.org/wiki/Differential_testing): generate an input, run it through both implementations, and check that they produce the same result. The proofs establish properties of the model; the differential tests check that the live engine continues to behave like it. We followed this model for the bounded part of The Lot that chooses the lead and supporting cards.

### Lean proved the wrong day

The agent wrote the first Lean coverage model too. I am still not sure whether asking an agent to formalize a requirement agents had repeatedly misunderstood was a good idea. But the model failed, which was useful: it was where we discovered that 20 of the 47 cards were starved. The dangerous version was the agent's next one, the first passing model presented as proving my requirement. It carried one history through five programs, but it treated weekday morning, workday, evening, late night, and weekend daytime as consecutive visits.

Lean correctly proved coverage for an itinerary no person could take. The theorem was valid. The claim that it described one person's day was not.

That passing version was added to the main source code, and its Lean check became required in continuous integration (CI), the automated test gate for every proposed code change. I merged it. The presence of a proof made the result feel stronger than it was, and I accepted the agent's description of the modeled day.

This is where formal verification, using mathematical proof to check a software claim, meets the hallucination problem. Lean checked the statement it was given. It could not check whether the agent had translated my product requirement faithfully.

### Putting one day into the model

The repair began by representing a visit with a day identifier and an hour. The five visits are written directly in the model:

```lean
structure LocalVisit where
  localDay : Nat
  hour : Nat

def fiveVisitsOn (localDay : Nat) : List LocalVisit :=
  [ { localDay, hour := 8 }
  , { localDay, hour := 12 }
  , { localDay, hour := 15 }
  , { localDay, hour := 20 }
  , { localDay, hour := 23 }
  ]
```

All five records carry the same `localDay`, and Lean checks that none differs. Its own hour-to-program function uses the five fixed hours rather than interpreting the date. Separate Go tests therefore send the same hours through the production clock resolver on both a Wednesday and a Sunday. Those cases guard against a return of the old weekday-versus-weekend split.

Lean models only the part of The Lot controlled by the ranked selector. It includes the structural Hero identity so the coverage question accounts for the top card, but the Hero appears independently and does not consume a supporting-card position. The model excludes **Live Now** and other modules inserted elsewhere on the page. The theorem below establishes coverage only for cards that pass through this selector.

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

`additionalPostures` records one current product choice. **Connections** is one catalog identity, but it is reserved at both midday and afternoon because either window leaves time to follow the card into a listen, read, or watch. Lean didn't derive that decision, and the theorem doesn't require the card to appear twice. The model makes the choice visible and checks what follows from it.

### Carrying one viewing history

At each visit, the model runs its version of the selector. It first preserves the cards guaranteed room in that program, then uses ranking to fill any space left over. Every retained card is treated as viewed and receives an impression. Before the next visit, the model advances the age of those impressions by the actual gap between the fixed hours. It then recomputes each card's staleness bonus from its impression count and age using the same four-hour cooldown and frequency formula as the Go code. The noon visit therefore receives the history produced by the morning visit, the afternoon receives the history from both, and so on. That is the crucial difference from testing every card in its own favorable world.

Initial viewing history became another explicit boundary of the theorem. The first corrected same-day model began with every modeled card unseen, which gives each one the maximum staleness bonus. That is a favorable starting point. We added a second uniform case by giving every card exactly one fresh impression immediately before the 8 AM visit. Each begins with a zero bonus, but that number is not frozen. A card shown again at 8 AM has two impressions when the four-hour cooldown ends at noon, while a card not shown still has one. The production formula gives them different bonuses, which can change their order.

The first attempt to add that second case got it wrong. Lean and Go both carried a fixed map of bonus numbers through the five visits. They agreed with each other, and both history cases passed, but neither side modeled how production recomputed the bonus as time passed. The required adversarial review caught the shared mistake. We replaced the bonus map with impression count and age, then made both models advance the clock and recalculate before every visit.

Lean now checks all four fixed combinations of the event-lead and tour conditions from both selected starting histories, and all eight walks pass. These theorems do not quantify over arbitrary prior impression counts or ages, mixed histories, cards becoming available or unavailable during the day, or those two conditions changing between visits. They also assume that every retained card is actually viewed and records an impression.

The next definitions assemble that walk. `ScheduledVisit` keeps the program and hour together, while `ImpressionHistory` stores the two values the production staleness function needs:

```lean
structure ImpressionHistory where
  count : Nat
  ageHours : Nat

structure ScheduledVisit where
  daypart : Daypart
  hour : Nat

def scheduledVisitsOn (localDay : Nat) : List ScheduledVisit :=
  (fiveVisitsOn localDay).map fun visit =>
    { daypart := postureAtHour visit.hour, hour := visit.hour }

def freshHistoryFor (cards : List Card) :
    List (String × ImpressionHistory) :=
  cards.map fun c => (c.name, { count := 1, ageHours := 0 })

def oneDayKeepsFrom (localDay : Nat) (eventLead onTour : Bool)
    (initialHistory : List (String × ImpressionHistory)) :
    List (List String) :=
  simulateDayFrom fullCatalog (scheduledVisitsOn localDay)
    eventLead onTour initialHistory

def oneDayKeepsUnseen (localDay : Nat) (eventLead onTour : Bool) :
    List (List String) :=
  oneDayKeepsFrom localDay eventLead onTour []
```

`scheduledVisitsOn` maps the five fixed hours in `fiveVisitsOn` to morning, midday, afternoon, evening, and late night without throwing the hours away. `freshHistoryFor` constructs the one-impression starting case. `oneDayKeepsFrom` gives those visits and an explicit initial history to `simulateDayFrom`, which runs the selector five times and returns five lists of retained card names. An empty initial list represents cards the person has never seen. `oneDayKeepsUnseen` is the explicitly named empty-history case used by the displayed theorem. `eventLead` and `onTour` tell the simulator which two product conditions remain fixed during the walk.

Inside `simulateDayFrom`, the transition happens in four steps:

```lean
let staleness := stalenessFromHistory history
let keep := capCards units visit.daypart isEventLead onTour
  (capForDaypart visit.daypart) staleness
let recorded := recordImpressions history keep
let nextHistory :=
  match rest with
  | [] => recorded
  | next :: _ => advanceHistory recorded (next.hour - visit.hour)
```

`stalenessFromHistory` applies the production formula to the history at the time of the current request. `capCards` makes the selection. `recordImpressions` increments the count and resets the age of every retained card. `advanceHistory` then adds the local-hour gap before the recursive call handles the next visit.

Two more helpers turn those five outputs into the coverage question:

```lean
def covered (keeps : List (List String)) (name : String) : Bool :=
  keeps.any (fun visit => visit.contains name)

def missing (keeps : List (List String)) (catalog : List String) :
    List String :=
  (catalog.filter (fun name => !(covered keeps name))).mergeSort
    (fun a b => a ≤ b)
```

`covered` asks whether one card name appears in at least one of the five lists. `missing` applies that question to the complete modeled catalog, returns every name for which the answer is no, and sorts the result so failures are reproducible. With those definitions in place, the theorem can state the invariant directly.

The central theorem contains both the claim and the proof Lean checks. Read it in two halves. Everything before `:= by` is the statement we are asking Lean to establish. Everything after it is the proof:

```lean
theorem five_visits_on_one_day_starve_no_applicable_card_when_all_unseen
    (localDay : Nat) :
    missing (oneDayKeepsUnseen localDay false false) fullNames = [] := by
  unfold oneDayKeepsUnseen oneDayKeepsFrom
  have h : scheduledVisitsOn localDay = fiveScheduledVisits :=
    every_local_day_has_the_same_five_scheduled_visits localDay
  rw [h]
  native_decide
```

Start with the statement. `(localDay : Nat)` means the theorem must hold for any local calendar day, represented here as a non-negative whole number. It does not need an hour parameter because `fiveVisitsOn` already supplies all five hours. The theorem's name now says `when_all_unseen` instead of hiding its starting-history assumption. The two `false` values turn off the event-lead and tour modes for this displayed case. `missing ... fullNames = []` says that after those five visits, the list of modeled card names that never appeared must be empty.

A neighboring theorem passes `freshHistoryFor fullCatalog` into `oneDayAllModesCoverFrom` and checks all four combinations of the event-lead and tour modes. The all-unseen case also has an all-modes theorem. Together they prove that the full catalog is covered from those two uniform starting histories while the mode conditions remain fixed throughout each walk.

The lines after `by` reduce that statement to a finite computation:

- `unfold oneDayKeepsUnseen oneDayKeepsFrom` replaces the helper names with the actual five-visit simulation and its empty initial history.
- `have h` proves that any `localDay` produces the same five scheduled visits, including their hours and programs.
- `rw [h]` replaces the arbitrary day's visit list with that fixed sequence.
- `native_decide` runs the resulting simulation and comparison. Lean can finish the theorem only if the computed missing list is empty.

This does not certify every behavior of the ranking algorithm. It certifies one property of the modeled selector: from either selected starting history, with these visits, fixed eligibility and mode conditions, catalog assignments, capacities, and reservations, no modeled card is left unseen. Ranking still decides how unused room is filled and how cards are ordered.

### Connecting Lean back to Go

But did the Go code make the same decisions as the model?

The product makes two different top-of-page decisions. One chooses a primary lead from ten already-built candidates. The ranked catalog separately includes `hero:hero`, a structural identity that lets the coverage model account for an independent Hero without charging it against supporting capacity. The primary-lead chooser has its own smaller check: ten yes-or-no values representing whether each lead candidate is present, plus one of six program values. That produces 2<sup>10</sup> × 6, or 6,144, possible inputs. Lean writes the expected lead choice for every one, and Go must match all 6,144 rows. Candidate construction and rendering are outside that exhaustive comparison.

The supporting-card selector has far too many possible subsets to compare exhaustively. Instead, the Lean executable writes 64 fixed catalog trials. Trial zero uses the complete modeled catalog. The other 63 use a fixed starting value to choose subsets and exercise all four fixed event-lead and tour combinations. Sixty-four is simply the test budget someone chose, not a total derived from the model. Each catalog is now run from both selected starting histories, producing 128 comparisons. The Go test also rejects the sample if it contains fewer than 32 distinct subsets, misses one of the four combinations, or omits either starting case.

Go replays those 128 rows through `lotCapCards`, the production function that selects supporting cards, and `lotStalenessBonusForHistory`, the production function that derives a bonus from impression history. It records each retained card, advances the age between visits, and must return the same lists Lean produced for those inputs. A regression test pins the four-hour transition that exposed the frozen-history bug: a card viewed again has two impressions and a bonus of 49, while its unserved peer has one impression and a bonus of 74. A separate Go unit test constructs a synthetic all-applicable inventory containing the same modeled names and carries both selected starting histories through all four event-lead and tour modes. It tests the real selection and staleness functions, not the full request-to-browser path.

That is a useful bridge, but it isn't proof that Lean and Go agree for every possible input. Lean proves the coverage theorem inside its model. The primary-lead comparison is exhaustive for its narrow input. The supporting-card comparison is sampled, with a full-catalog case and a separate full-catalog Go unit test. These are stronger and more explicit checks than we had before, but they establish different things.

What happened after the selector returned? A checked result was still useless if the browser could discard or reorder it. We therefore centralized scheduling-time eligibility, deduplication, caps, sections, and ordering on the server. The checked selector owns the cap and reservation decision. The surrounding server code resolves candidates and produces `moduleOrder`, the final allow-list and order, plus `moduleSections` for placement. The browser follows those instructions instead of running its old second scheduling policy.

The browser still honors explicit user actions such as local dismissals, and conventional tests cover the handoff from server identities to rendered components. This doesn't turn the theorem into proof of pixels. It closes the known hole where a later scheduling layer could silently weaken the selector's result.

These checks now run on every proposed code change. A failed Lean theorem, stale generated data that no longer matches Lean, a disagreement on one of the differential cases, or a missing card in the synthetic Go walk fails the check. The old independent caps were replaced with capacities sized to the assigned cards, currently 10, 13, 11, 10, and 10, and Lean checks that those assignments still fit.

This was the first version I would accept as implementing the coverage design. Lean didn't certify the whole product or prove every behavior of the ranking algorithm. It established coverage inside the model for two explicit starting histories under fixed eligibility and mode conditions, and the additional tests made it much harder for the production selector to drift away without being noticed. That is different from producing another artifact that merely says the implementation looks correct.

## Algorithms are hard

By then the agent had produced an implementation, unit tests, end-to-end tests, documentation, multiple code audits, and several numerical arguments. Each artifact said, in its own way, that the requirement had been implemented correctly. It had not.

Those artifacts agreed because the same failure pattern propagated from one artifact and agent to the next. One calendar day became a week. Guaranteed coverage became isolated reachability. Capacities from mutually exclusive programs were added together. Scheduling was divided between the server and browser. Then tests and audits treated those invented decisions as requirements. Green tests were not independent evidence about the invariant I had asked for. They were evidence that the substituted designs behaved as specified.

The invariant was easy to state: carry one person's viewing history through five visits on one calendar day, then ensure that every available and relevant card appeared at least once. That did not make the algorithm easy to implement correctly. The agent could read the specification, write the implementation, write unit and end-to-end tests, and audit its own work, but it repeatedly failed to keep the invariant intact. Each time it encountered an ambiguity or an implementation obstacle, it invented a nearby requirement instead of asking me.

The power we eventually got from Lean wasn't another claim that the code looked right. Once the correct day, the impression-history transition, and the two explicit starting histories were part of the model, Lean established coverage for all four fixed mode combinations from each start. Go then had to match every primary-lead case and 128 supporting-card comparisons, while a separate unit test walked the real Go selection and staleness functions through the full modeled catalog from both starts. This didn't prove every Go input, every possible history, changing eligibility, changing modes, or the whole product. It gave us a checked property with an explicit boundary and strong evidence that the production selector agreed on the cases we compared.

Lean itself was not immune to the problem. The first passing Lean model contained the wrong itinerary, and I let it merge without catching that mismatch. A theorem can make a misunderstanding more convincing if nobody checks that the formal statement still matches the request. The important step was not adding a proof to the artifact pile. It was making a precise, bounded version of my original invariant explicit, checking it, connecting it to the production implementation, and refusing to let a later layer weaken it.

This wasn't one model having one bad run. I tried to repair the work with Claude Opus 4.6, Claude Opus 4.8, Claude Opus 5, and Codex using GPT-5.6 Sol. None independently arrived at both the invariant I had stated and an implementation that preserved it. They found different problems and produced more artifacts, but changing models didn't break the pattern. Each could accept or recreate a nearby requirement and then generate convincing evidence for its own version of the problem.

That is the takeaway for me. Algorithms are hard, even when their invariants are easy to say. AI can now read a specification, write an implementation, generate unit tests, generate end-to-end tests, and produce several audits explaining why everything is correct. It cannot guarantee an invariant merely by generating all the artifacts that say the invariant holds. If they all inherit an unexamined change to the requirement, their agreement means very little.

For an algorithm whose correctness depends on an invariant, the invariant needs an executable form and an explicit bridge to the implementation. That bridge may check every input, sample some inputs, or rely on ordinary tests, and those offer different levels of confidence. Even then, the guarantee is only about the formal statement, so a person still has to be shown, in terms they can evaluate, that the statement describes the requirement they actually meant.

The final test should have been the first. The hard part is stating it before an AI has built the implementation, the tests, and the argument for trusting a nearby one.
