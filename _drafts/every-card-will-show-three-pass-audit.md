# Every Card Will Show: three-pass editorial audit

Post: `_posts/2026-08-24-every-card-will-show.markdown`

Evidence base:

- Zabriskie source excerpts rechecked against `origin/main` at `51995c9433e522d9e27735aa30118b1aa7e02098`; the successful bounded proof loop remains recorded below at its August 24 merge commit.
- Blog `origin/master` at `5da3f3ebd727b25d3b59182f583fd6b7f1f96382`.
- The author's first-person account for the conversation, elapsed time, the agent's 47-item product enumeration with 20 missing, agency, repeated requirement, production timing, and Zabriskie's role as a fully vibe-coded application whose implementation code he does not read.

## Structural reader

1. Opening incident: keep. It establishes the exact promise and observed contradiction before introducing the system.
2. Product and experiment background: keep before the incident. It defines Zabriskie, states that the application is built entirely through agents without manual implementation review, and then defines The Lot, its five programs, and the intended product behavior.
3. Responsibility boundary: keep at the end of the introduction. It locates the author's responsibility without assigning the invented policy to him.
4. Agent-chosen policy: keep as the first main section. The author supplied the no-starvation requirement; the sequence from caps through ranking and classification to the impossible 27-position argument shows how the chosen implementation drifted away from it.
5. Pre-Lean assurances: keep together as the second main section. The documentation audit, per-card reachability test, catalog review, and browser split all support the same distinction between local reachability and one-person coverage.
6. Lean primer and chronology: keep as the final technical section, split under `Lean proved the wrong day`, `Fixing the five-visit schedule`, `Reserving room before ranking`, and `Connecting Lean back to Go`. Introduce the bounded Cedar-style architecture, then show the failed first model and the first passing model's impossible itinerary before explaining the reservation-and-capacity repair. This keeps the proof failure inside the incident chronology rather than revealing it after the corrected implementation.
7. Internal audit note and publication checklist: remove from the public body. Preserve them here instead. The evidence limits that matter to the argument remain explicit in the post.

The new subheads keep the model failure, fixed schedule, structural coverage mechanism, and implementation bridge from reading as one uninterrupted proof tutorial. No section is left doing multiple unrelated jobs after this pass.

## Evidence reviewer

| Claim | Boundary or source | Disposition |
| --- | --- | --- |
| The agent said every card would show | Author recollection | Kept in first-person incident framing. |
| Zabriskie is built entirely through vibe coding and the author does not read its implementation code | Author account of the experiment's method | Added near the beginning so reliance on agent-written code, tests, and audits is a premise of the incident rather than a detail inferred later. |
| The agent enumerated 47 applicable product cards and found 20 did not appear | Author-supplied account of the agent's work | Corrected the agency: the author requested the enumeration; the agent performed it. The 47 was not a committed source-code catalog. |
| Roughly ten hours on August 22 and twelve on August 23 | Author recollection | Kept with `roughly` and changed relative dates before moving the post to August 24. |
| 29 cards and more than 15,000 pixels | PR #2514 evidence records 29 cards and 15,554 pixels | Rounded pixel count in prose. |
| Three of six candidate areas, then a six-card global cap | PRs #2505 and #2514 | Corrected to say that the Hero competed inside the six positions rather than sitting above six supporting positions. |
| Ranking values 1,000, 200, 60, and up to 99 staleness points | Ranking implementations in the audited history | Removed the exact constants from the public post because they do not support a later claim. Retained only the staleness description needed to explain the earlier isolated reachability test. |
| Connections given a late preference and On This Day products reversed | Audited merged implementations and their corrections | Avoids presenting late affinity as an absolute gate; product judgment about its intended 11 PM home remains first-person interpretation. |
| The capacities totaled 26, then 27 | PRs #2599 and #2610 | Removed the intermediate total of 26. Kept 27 because the article uses it as the example of arithmetic over incompatible schedules. |
| Weekday positions 20 then 21; weekend positions 15 then 16 | Derived from one visit to each distinct old program | Removed from the public post because the intermediate totals obscured the only distinction the argument needs: weekday and weekend followed mutually exclusive schedules. |
| Audit and repository counts of 42, 46, and 45 | PRs #2602, #2621/#2622, and merged final catalog | The 42-card documentation audit was internally inconsistent. The first executable inventory contained 46 ranked identities. The later server-owned layout collapsed Local Scene from separate Act Now and Discover identities into one canonical Discover identity, producing the current 45. The repository does not preserve the remaining 47-to-46 explanation. |
| The documentation audit promised coverage within a week | PR #2602 for the weekly claim; author account for the fact that no weekly window was requested | Identified as a hallucinated replacement of the stated one-day requirement, not merely a weaker interpretation. |
| Per-card reachability used target staleness, fresh comparable peers, separate Act Now cohorts, and synthetic tier representatives | PR #2621 for the test mechanics; author account for the fact that isolated reachability was not requested | Identified as another hallucinated replacement requirement: the agent built a passing test for isolated favorable worlds instead of the requested one-person day. |
| Browser retained grouping, suppression, and section policy after server order was introduced | PR #2621-era browser path; author account for the fact that split scheduling ownership was not requested | Kept; identifies the split as an agent-created implementation decision that added places for cards to disappear and made the requested guarantee harder to implement and verify. |
| First passing Lean model combined weekday and weekend states | Commit `0956318a9` and PR #2543 history | Corrected the chronology: earlier Lean work reported failure; this was the first passing model presented as proving the requirement. |
| Corrected visits are 8 AM, noon, 3 PM, 8 PM, and 11 PM on one local day | PR #2633 | Kept, including Wednesday and Sunday clock-resolver cases. |
| Current derived capacities are 10, 13, 11, 10, and 10 | `LotLead.Cap.capForDaypart`, `programmed_capacities_match_catalog`, and the successful current proof report | Kept as capacities sized to current program assignments, not independent evidence of coverage. |
| Formal catalog contains 45 identities including Hero; Live Now is outside the proof | Audited `origin/main` catalog | Kept and explicitly not used to reconcile the observed 47. |
| Connections owns both midday and afternoon | `lotCardAdditionalProgramPostures`, the `discover:connection` rationale in `lot_program_catalog.go`, `docs/lot-posture-audit.md`, and the matching Lean catalog entry | Explained that an applicable Connections card is intentionally repeated because either daytime window leaves runway to follow its listen/read/watch path. Identified this as current product policy encoded by Lean, not a mathematical consequence of the proof, and clarified that it remains one catalog identity. |
| Late-hours Hero, Act Now, and Coming Up appeared in production | Read-only signed-in screenshots captured from `zabriskie.app/v2/lot` at 11:35 PM on August 23 and just after midnight on August 24 | Used only in image captions describing the visible program; not treated as five-visit coverage evidence. |
| Lean models card metadata, five same-day visits, ranking, selection, and evolving staleness | `lean/LotLead/Cap.lean` and `lean/LotLead/Cover.lean` on `origin/main` | Added as the core of the primer; explicitly described as independent of the request path and Go source. |
| Public code excerpts match the current Lean source | `takePhase0` from `lean/LotLead/Cap.lean`; `programmed_capacities_match_catalog`, `covered`, and `missing` from `lean/LotLead/Cover.lean` at `51995c943` | Kept the examples adjacent to their plain-language translations and centered them on the reservation-and-capacity mechanism. |
| Lean checks coverage from two selected uniform starting histories | The all-unseen and all-fresh theorems in `lean/LotLead/Cover.lean` | Removed the history tutorial from the public post. Those finite cases are not a proof of arbitrary-history independence, and the structural coverage argument comes from reservations plus matching capacity. |
| Go is compared through 64 deterministic trials from two histories plus a synthetic full-catalog selector test | `lot_cover_lean_diff_test.go`, `lot_card_day_coverage_test.go`, and `scripts/lean-lot-lead.sh` | Explicitly says the 128 supporting-card comparisons are sampled and that the full-catalog unit test calls the real selector without exercising the full request-to-browser path. |
| Current browser no longer applies the old second scheduling policy | `lotApplyCardCap` and `lotRankCards` in `backend/internal/handlers/lot_posture.go`; `serverOwnsLayout` and `applyServerModuleOrder` in `web/src/components/sdui/cinematic/CinematicLot.jsx`; `docs/lot-posture-audit.md` | Separates server-owned candidate construction and layout from the checked selector. Preserves explicit client dismissals and does not claim proof of rendered pixels. |
| The current bounded coverage loop passes | Merged PR #2646 and `origin/main` at `fffec3548` on August 24 | Verified the two starting-history cases, all four fixed event-lead and tour combinations, 128 sampled Lean/Go supporting-card comparisons, the 6,144 exhaustive lead comparisons, and the production history transition. Kept separate from a claim about every possible history or rendered browser output. |
| The approach follows Cedar's verification shape rather than integrating Cedar | [Cedar security documentation](https://docs.cedarpolicy.com/other/security.html), [cedar-spec](https://github.com/cedar-policy/cedar-spec), and Zabriskie `lean/README.md` | Kept the analogy to a formal Lean model, separate production engine, and differential testing bounded to the Lot selector. |
| The Lot island covers two finite decisions | `lean/README.md`, `LotLead/Pick.lean`, `LotLead/Enum.lean`, and `LotLead/CoverCheck.lean` | Distinguishes the primary-lead chooser from the structural Hero identity. Explains the exhaustive arithmetic as ten Boolean candidate flags times six program values, `2^10 × 6 = 6,144`, and the separate 64-trial sampled supporting-card comparison. |
| The failure is described as a hallucinated guarantee rather than fabricated code or test output | Interpretive framing grounded in the documented sequence of real implementations, passing weaker checks, the author's repeated requirement, and the first Lean model's impossible day | Woven through the opening, invented owner attribution, assurance section, Lean correction, and conclusion; wording preserves responsibility and distinguishes Lean's valid proof from the false product claim made around it. |

Corrections preserved from the evidence pass:

- Removed the `simulateDayFrom` history tutorial after confirming that history feeds ranking but is not the source of the no-starvation guarantee.
- Recentered the Lean explanation on `takePhase0` reservations and the checked per-program assignment counts.
- Removed `localDay` from the public explanation after confirming that it is never consulted by the Lean clock or selector and does not contribute to the coverage result.
- Defined `additionalPostures` in product language as additional guaranteed programs.
- Replaced any implication that 27 caused the observed 20 missing cards with two independently bounded claims.
- Replaced "daily maximum" language with "one visit to each distinct program."
- Corrected the old six-card cap and 20/21 and 15/16 totals to include the Hero.
- Corrected the scoring contexts for the 200- and 60-point bonuses.
- Changed "first Lean model" to "first passing Lean model" after verifying that the earlier Lean model reported failure.
- Scoped the public coverage discussion to the finite catalog and fixed event-lead/tour modes without presenting the selected history cases as a general theorem about history.
- Scoped the working Lean result to the formal model and server-side ranking function rather than the complete production UI.
- Distinguished the browser chronology before and after server-provided card order.
- Identified the 47-item count as the agent's product-level enumeration, not the author's manual work or a committed catalog. Explained the repository-backed 46-to-45 Local Scene deduplication and left the 47-to-46 difference explicitly unresolved.
- Removed the 46/45 inventory reconciliation from the public post because it distracted from the incident. The public narrative retains 47 as the agent's product-level count and refers to the later Lean input only as the modeled catalog; the detailed source history remains in this internal audit.
- Corrected the afternoon capacity from 10 to 11 after rerunning the current proof report.
- Corrected the later late-night capacity from 1 to 10 after PR #2646 merged.
- Replaced `same weaker claim` with the specific nearby claims the artifacts embodied or checked.
- Corrected the Hero explanation: the structural identity is included in coverage but does not consume supporting-card capacity.
- Recorded the two actual uniform starting histories in this audit while removing them from the public primer because they do not establish arbitrary-history independence.
- Corrected staleness to describe impression recency and frequency rather than time since server selection alone.
- Replaced the remaining ambiguous `shipped code` with `merged code`.
- Added the vibe-coding method near the beginning and replaced the implication that the author should have inspected the Lean source with the actual failure: he accepted the agent's inaccurate description of the modeled day.

## Continuous-read readability editor

- Rendered successfully with Jekyll and read in generated page order.
- Added public links at first mention for Zabriskie, The Lot, Lean, Go, and Cedar. All five destinations returned HTTP 200 on August 24. Removed a link to the private Zabriskie source repository because it returned 404 for a reader without a signed-in GitHub session.
- Reworked the opening for a reader who has never used Zabriskie: separate paragraphs now explain the app's cultural scope and non-follower organization, common activities, The Lot, cards, and the five daily programs before the incident begins. Added a link to the deeper live-show account in "The Whole Night."
- Defined ranking, eligibility, staleness, the catalog, supporting positions, reachability testing, adversarial review, server and browser responsibilities, proof assistant, differential testing, and continuous integration at first use.
- Kept the short paragraphs around the opening contradiction and the first incorrect Lean model because both are genuine turns.
- Removed scoring constants that never support a later claim. Kept only the staleness explanation needed for the earlier reachability substitution.
- Defined staleness and Lean on first use.
- Reordered the Lean section so the first passing but wrong model appears before the repair.
- Kept the Lean examples for typed card and visit data, the reservation pass, assignment capacities, and coverage helpers, while explaining each in prose.
- Kept each Lean excerpt adjacent to a plain-language translation and defined unfamiliar type names before relying on them.
- Preserved the distinction between a theorem about the finite Lean model, sampled differential agreement, the independent Go test, and unverified production behavior.
- Introduced Cedar only to explain the verification architecture, then immediately bounded the analogy so readers do not infer that Cedar schedules The Lot.
- Threaded hallucination through the argument as a false connection among real artifacts, not as a claim that the source code, test results, or Lean proof were fabricated.
- Made agency explicit at the section and article level: the author requested the no-starvation outcome; the agent chose the specific limits, rankings, and classifications, then introduced the weekly window, isolated reachability test, and split scheduling ownership without surfacing those substitutions.
- Removed the public-facing audit appendix, which interrupted the ending after the argument had already reached its production boundary.
- Added two current production screenshots at the points where the reader first encounters the Lot structure and the distinction between one partial program and the five-visit guarantee. The second image uses the Coming Up card; the What's New panel was dismissed before capture.
- Checked for em dashes and accidental Markdown H1 headings in the body; none remain.
- Applied `scripts/voice/voice-guide.md` using the 2021 Partisan bug post and 2023 resilience-testing post as mode-matched cadence references.
- Added contractions and question-driven transitions, removed institutional phrasing, and ended with the unresolved difficulty of stating the correct invariant before an AI builds evidence for a nearby one.
- Split the expanded Lean material into four named transitions so a reader can follow the chronology without treating the entire section as one proof listing.
- Read the revised page continuously after rendering. Removed the repeated Hero explanation, narrowed every conclusion that had called the two selected histories `extremes`, and retained the short sentence `Lean correctly proved coverage for an itinerary no person could take` as the central turn.
- Rechecked the code-block spacing and the new third-level headings in the rendered page; both have clear separation from adjacent prose.
- Added the vibe-coding paragraph between the product introduction and The Lot so the reader understands why the author evaluated browser behavior and requested audits instead of reading the implementation. The later conclusion now asks for the formal statement to be presented in terms a person can evaluate, rather than requiring manual source inspection.
- Added a transition into the first implementation section: the request to distribute cards across the day became an agent-chosen policy about what each individual visit could omit.
- Corrected the ranking paragraph so it no longer claims that the author had not asked for product rules. It now distinguishes the requested no-starvation outcome from the agent's chosen implementation.
- Made the reachability substitution explicit: one person's five-visit union became a separate favorable situation for each card. Combined the following explanation so the section states the contrast once.
- Replaced the dense weekday/weekend capacity accounting with the two actual schedules and the direct explanation that 27 added mutually exclusive programs.
- Condensed `What the agent invented instead` around four necessary points: caps, ranking and catalog policy, concrete classification failures and false owner attribution, and the impossible 27-position argument.
- Replaced the abstract `impossible itinerary` explanation in the Lean chronology with the missing concrete fact: the passing model placed weekday and weekend programs inside one supposed calendar day.
- Removed the history tutorial after a continuous reread showed that it made ranking state sound like the source of the coverage property. The revised section moves directly from program assignments to the reservation pass, matching capacities, and the empty missing-card check.
- Replaced the inert `localDay` code example with the actual boundary: Lean assumes the fixed five-program walk, while Wednesday/Sunday Go tests establish that production can realize it on a calendar date.
- Rendered the shortened section with Jekyll, rechecked every displayed Lean excerpt against current Zabriskie `origin/main`, and confirmed that the revision introduces no em dashes or stale history code examples.

## Remaining publication checks

- The public post consistently uses 47 only for the agent's product-level incident enumeration and does not introduce the later repository catalog size. The unresolved historical reconciliation remains documented here rather than interrupting the article.
- The model and provider version names remain author-supplied provenance. Verify their public naming before publication if exact version attribution is important.

No high-severity factual, technical, structural, or readability objection remained after the August 24 revision and rendered reread. The first-person claims about the interaction, elapsed time, the agent's 47-item enumeration, and model participation remain author-supplied provenance rather than repository-derived facts.
