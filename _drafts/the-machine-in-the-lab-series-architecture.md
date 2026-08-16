---
layout: page
title: "The Machine in the Lab: Series Architecture"
description: "The complete argument map, post contracts, transitions, and adversarial review protocol for The Machine in the Lab."
permalink: /series/the-machine-in-the-lab/series-architecture/
---

This is not a prose outline. It is the logical architecture of the series. Its purpose is to make the progression inspectable before the articles acquire narrative momentum, polished transitions, or conclusions that feel inevitable because they have been written well.

An adversarial reviewer should be able to use this document to answer five questions:

1. Does every post earn a distinct claim?
2. Does each claim follow from the evidence and the claims before it?
3. Can every post be understood and valued by a reader who has not read the others?
4. Does the complete series answer the question it opens without pretending the final evidence is stronger than it is?
5. Could any post be removed or merged without breaking the argument?

The ordinary [editorial plan](/series/the-machine-in-the-lab/editorial-plan/) remains the source for proposed sections, evidence inventories, lengths, and exclusions. This document governs the argument across those posts.

## 1. The Series Contract

### Controlling question

What has to change when an LLM agent can implement empirical research while silently violating constraints it was explicitly given, then carry those violations into the next experiment?

### Series answer

The answer is not simply more careful prompting, more agent reviewers, more reproducible code, or a larger holdout. This case exposed three distinct boundaries that an LLM-mediated empirical workflow must represent. They are not offered as a complete taxonomy of scientific validity:

1. **The information boundary:** what data, outcomes, labels, and derived artifacts may influence design or fitting.
2. **The meaning boundary:** whether the operational measurement corresponds to the phenomenon named in the claim.
3. **The execution boundary:** whether the complete deployed path actually receives and processes the phenomenon the evaluation assumes it receives.

Those boundaries must be represented in durable state outside the model, enforced before downstream work multiplies, and evaluated with evidence appropriate to the claim. A better process does not guarantee a successful result, prevent every invalid promotion, or make human approvals non-ceremonial. It can create an inspectable record, restrict what an agent is permitted to do automatically, and make a stronger claim require an explicit, auditable violation of the stated evidence policy.

### Author and project frame

The author came to this project with a PhD and years of experience doing systems research. That background is context, not evidence that any result is correct. The series therefore cannot explain the failures away as the predictable result of an inexperienced person asking an LLM to perform a technical task they did not understand.

The actual experiment was more interesting: could an experienced researcher use an LLM agent to make a self-directed audio-research program possible at a scale that would otherwise require a team or much more time? The project originally began as an automatic set-detection project: could Zabriskie listen to an in-progress Goose show and identify the song without consulting an external setlist? The available live-audio corpus and the apparent power of audio features then pulled the work into a more ambitious research detour about how jam-band listeners describe improvisation, structure, and transitions. Those questions were interesting, but they contained disputed constructs, subjective boundaries, inherited labels, and many plausible operational definitions. They were much harder to answer cleanly than they first appeared.

The research program eventually returned to its original song-recognition target with a directly observable product question: can a dateless, setlist-free system listen to a continuous live Goose stream and emit a correct current-song identity within a stated time window?

SetScope has a deliberately ordinary product purpose: show a viewer its best current-song guess so the viewer does not have to consult an external setlist. The displayed song title is a product output, not a scientific finding. A live browser run is a product field test, not a study of Goose's music. Its logs can document what one version did under recorded conditions, and clean corpus evaluations can support bounded reliability claims, but neither turns the guesser itself into a scientific instrument. The research object in this series is the human-agent development and evaluation process: which conclusions that process may draw from a component score, a field test, a sealed evaluation, or a deployment record.

The August 13, 2026 San Diego show supplied a genuine live field test: the audio did not exist during training or development, and SetScope produced blind song guesses while the show was happening without receiving the date or setlist. The preserved runtime record emitted correct identities at least once for 10 of the 12 song performances. A later internal acoustic-family diagnosis found 11 of 12, but that is not an additional product output. These are descriptive facts about live runtime behavior, not a scientific result or a formal whole-show accuracy estimate. Set 1 failed the planned capture-integrity check, and the 61.5 percent Set 2 held-state calculation used an approximate truth timeline reconstructed after the show rather than the required frozen boundary artifact. No preserved UI-observation or outward-publication receipt was located, so the evidence reaches the controller but not confirmed user-visible delivery. The field test establishes the practical fact that the runtime can emit correct identities on never-before-existing live audio; it does not establish a formal accuracy rate or reliable continuous operation. The remaining product questions concern false transitions inside jams, abstention and unknown material, continuous held-state accuracy, latency, capture integrity, display and publication delivery, catalog coverage, and independent generalization.

That return is the constructive arc of the series. The project did not solve the broad improvisation questions it encountered during the detour. It returned to its original product ambition with a target whose inputs, outputs, errors, and remaining uncertainty could be inspected. SetScope is therefore neither a convenient late success nor a tighter operationalization of improvisation. It is the original fan-facing problem, re-entered after the failed detour changed how the project would evaluate its own work.

### The causal mechanism

The series is not built on the claim that LLMs invented research error. Humans leak data, choose bad proxies, overinterpret exploratory findings, and test the wrong system without any machine assistance.

The narrower mechanism is:

1. An agent makes implementation dramatically cheaper and faster.
2. Empirical validity depends on setup choices that ordinary software review can treat as minor implementation details.
3. The agent is rewarded for plausible forward progress, so reasonable defaults and inherited assumptions often remain implicit.
4. Plausible code quickly produces tables, charts, reports, and prose.
5. Every downstream artifact increases the financial, social, and psychological cost of reopening the original decision.
6. Internal checks tend to compare artifacts within the same chain, allowing every check to agree with the same mistaken premise.
7. The speedup is therefore real, but it applies equally to valid work and invalid work.

The series must demonstrate this mechanism rather than treating "AI makes mistakes" as its thesis.

### Reader promise

The reader should leave with:

- a precise explanation of why plausible implementation is not enough for empirical work;
- three separate validity boundaries they can apply to their own projects;
- a concrete account of how those boundaries failed in one research program;
- an account of how an automatic set-detection project detoured into broad improvisation research and returned to its original target as a directly testable live-system question;
- a reusable protocol for assigning data roles, freezing decisions, preserving provenance, checking the underlying phenomenon, and validating deployed paths; and
- an honest conclusion whose wording follows the evidence coordinates, including abstentions and failures, rather than a success story chosen in advance.

### Scope

The series concerns computational empirical research in which an LLM agent helps design experiments, write analysis code, operate infrastructure, interpret results, or draft claims. The audio project is the running case because its inputs can be inspected directly and because it crossed from exploratory analysis into a live product.

The series does not attempt to define all science, evaluate every form of AI-assisted scholarship, prove that agent-mediated work is generally less reliable, or claim that the proposed operating protocol is sufficient for every field.

## 2. The Complete Progression

The series has a six-post methodological arc and a seventh-post product payoff:

| Part | Reader's question | New claim earned | Remaining problem |
| --- | --- | --- | --- |
| 1. Science at LLM Speed | Why is this a research problem now? | LLMs make complete-looking research artifacts cheap; a valid claim still requires evidence outside the generated artifact chain. | What does the failure look like inside an actual project? |
| 2. Two Notebooks Lost | How can plausible work become invalid? | In empirical work, setup decisions are part of the experiment. One silent choice can invalidate every correct artifact downstream. | Which distinct information and meaning boundaries did the two failures expose? |
| 3. The Holdout Is a One-Way Door | What counts as leakage and independence? | Independence belongs to the complete information history of the research process, including derived artifacts and human adaptation. | Can a clean, preregistered, internally consistent experiment still make the wrong claim? |
| 4. When Every Check Passes | What can process and provenance fail to detect? | Internal consistency cannot establish construct or measurement validity. The named interpretation must be checked against the underlying phenomenon through a different evidence path. | What new validity problem appears when the project leaves the jam-research detour, returns to automatic song identification, and evaluates a live deployed system? |
| 5. The Browser Is Part of the Experiment | Why can an offline metric fail in use? | Product behavior belongs to the complete deployed path. The August 13 field test produced correct blind controller outputs on never-before-existing audio while exposing capture, transition, held-state, unknown-song, and missing display/publication receipts that offline metrics did not measure. | What operating system can represent and coordinate all three case-derived boundaries without stopping useful work? |
| 6. A Discovery Loop That Can Say No | What does a defensible agent-mediated workflow look like? | Bounded experiments, durable data roles, immutable artifacts, independent reality checks, complete-product rehearsals, and claim-specific evidence can make the process auditable. The deployed prototype and any broad numerical reliability or viewer-benefit claim are separate outcomes. | What happens when that operating discipline meets an eleven-show August run of changing versions, imperfect transport, real songs, and publication paths? |
| 7. What Happened on the August Run | What did the product actually emit from August 13-28? | A live product must be reported as a versioned operational history: recognition, latency, stability, availability, and publication behavior cannot be collapsed into one model score. | Future product work continues, but the series has reported the defined August-run record without inventing a complete-tour or viewer-effect study. |

### The case-derived three-boundary spine

Parts 3 through 5 must not collapse into a generic sequence of mistakes. These are the three boundary failures this project exposed, not an exhaustive theory. Sampling, statistical inference, uncertainty, population validity, incentives, and other scientific failure classes remain outside this spine unless a post addresses them explicitly.

| Boundary | Governing question | Characteristic failure | Evidence that can answer it | Primary post |
| --- | --- | --- | --- | --- |
| Information | Did outcomes or related information influence design, fitting, or selection? | Performance leakage, contaminated caches, opened holdouts, adaptive thresholds, human HARKing | Provenance, grouping, point-in-time reconstruction, opaque labels, frozen roles and hashes | Part 3 |
| Meaning | Does the operational measurement correspond to the construct named in the claim? | A detector agrees with labels for the wrong acoustic event; one person's labels are treated as a shared construct; a proxy receives a stronger name than it earned | Independent domain inspection, blinded annotation, agreement studies, alternative operationalizations, construct validation | Part 4 |
| Execution | Did the evaluated system receive and process valid input through the same path used in deployment? | Archive metrics look good while browser capture drops samples; a component vote is overridden incorrectly; premusic becomes a song lock | Source-frame accounting, exact-byte replay, complete-product telemetry, whole-show scoring, prospective shadow use | Part 5 |

Part 6 is not a fourth boundary. It is the operating protocol that coordinates all three and keeps one evidence dimension from substituting for another.

### The discovery-loop reading

The series is also a field report about automated experimental loops. This connection should be visible to readers following the new generation of systems that propose, execute, evaluate, and revise experiments, including the contemporary company Discovery Loop, without turning the series into commentary about a company whose implementation is not public.

1. Part 1 establishes why automating the loop has become a live research and systems objective.
2. Part 2 shows an uncontrolled loop producing plausible experimental state while failing to surface or enforce the premises carried into the next iteration.
3. Part 3 identifies the information problem: iteration changes what the agent and operator know, so independence must be represented as lineage rather than file placement.
4. Part 4 identifies the evaluator problem: the loop can optimize a reproducible proxy that does not measure the named phenomenon.
5. Part 5 identifies the execution problem: the loop must exercise the actual input, transport, controller, and display path rather than only its strongest component.
6. Part 6 supplies the control plane: typed state, evidence roles, invalid outcomes, stop conditions, and promotion permissions outside the generating agent.
7. Part 7 reports the loop as history, preserving which version produced each success and failure rather than attributing all outcomes to the final system.

The recruiting signal should come from the technical fit. Parts 1 and 6 may name Discovery Loop. The other posts should establish the mechanics without repeatedly invoking Jeff Dean or speculating about the company's architecture.

## 3. The Claim Ladder

Each rung depends on the one before it. A draft that jumps past a rung has broken the series even if every sentence in the draft is individually true.

### S1. Cheap research-like production changes what must be enforced and disclosed

LLMs participate in literature search, writing, review, code, analysis, and hypothesis generation. They can contribute to verifiable work, and they make complete-looking scholarly artifacts cheap to produce. That observation does not establish a general rate comparison between production and verification costs. It creates the narrower question that controls the series: what evidence outside the generated artifact chain judges each output?

### S2. Plausible implementation can conceal different kinds of methodological choice

The two lost notebooks show the local mechanism, but they do not fail in the same way. Correct-looking code created a data split without protecting independence and inherited a segmentation constant without establishing that the resulting unit represented the phenomenon in the claim. The downstream artifacts were internally plausible because most of the implementation was correct. Notebook 1 creates the information-validity debt paid in Part 3; Notebook 2 creates the meaning-validity debt paid in Part 4.

### S3. Independence is historical, not spatial

A clean directory or a `train_test_split` call does not establish independence. Performances, shows, caches, labels, feature choices, thresholds, and human knowledge all carry information. Once an outcome changes design, the affected data may support engineering but cannot return to sealed confirmation status.

### S4. Independence does not establish meaning

Notebook 2 first exposed the danger of letting a convenient segmentation procedure define the phenomenon being studied. Even a later, cleanly executed plan can test correspondence to a label while being interpreted as correspondence to the world. The Dispatch 002 chain pays off that deferred problem because it passed the process built after the notebook failures and still reached the wrong acoustic interpretation.

### S5. A valid measurement does not establish deployed behavior

Offline audio decoded directly from archives does not exercise browser playback, system routing, capture, segmentation, gates, state transitions, or publication logic. The live path can therefore fail before or after the model while the model's offline result remains unchanged.

### S6. Auditability comes from constrained claims, not procedural confidence

The final protocol assigns each artifact a role and each claim a required position on the evidence axes. It can show that a process is more auditable and that some specific failure shapes are now caught earlier. The August 13 field test documents correct blind guesses on genuinely future live audio, but its capture and truth-timeline failures prevent it from supplying a whole-show performance estimate. The existence of the prototype, correct emitted guesses, broad quantitative reliability, viewer benefit, and authorization for automatic public posting remain separate claims and decisions.

### S7. An August run is a product history, not one score

The final retrospective applies the distinctions rather than adding a fourth validity boundary. SetScope may change during the eleven-show August 13-28 western run, runs may fail before inference, songs may segue without clean boundaries, and external setlist timestamps may describe a different event from a SetScope lock. The honest product record therefore preserves versions, clocks, invalid operations, abstentions, and separately verified display or publication behavior before aggregating. Its conclusion describes how this evolving tool operated during that defined window, not across the 13 earlier summer shows carrying the same source tour label. First-person feedback may describe whether it felt helpful, but the telemetry alone does not estimate a viewer effect.

## 4. Post Contracts

Every post has two contracts. The **standalone contract** states what a reader receives without reading the series. The **series contract** states what logical work the post performs in the larger arc.

## Part 1. Science at LLM Speed

### Working title proposition

**Title:** Science at LLM Speed
**Subtitle:** Research-like output is cheap. Valid claims still need an outside judge.

The exact words can change. The title and subtitle must distinguish cheap production from independently judged evidence without asserting an unsupported comparison between the rates or costs of production and verification.

### Standalone contract

A reader unfamiliar with the audio project should understand the current range of LLM involvement in research, see both documented failure and credible acceleration, and leave with a test for distinguishing generated scholarly form from externally judged work.

### Series function

Part 1 establishes why the personal story is not only a software postmortem. It defines the general pressure that the rest of the series examines in one tractable case.

### Question answered

What is different about doing research when the same class of system can search the literature, draft the paper, write the analysis, review the result, and explain why the result is convincing?

### Argument chain

1. Nonexistent citations in real submissions demonstrate that fluent scholarly form can enter institutional processes without the scholarship behind it.
2. Large-scale studies suggest LLM assistance is affecting manuscript production and review, although attribution and causality must be qualified.
3. Explicitly assisted review and retrieval-grounded literature systems show that model involvement is not itself the problem.
4. Verifiable systems such as program search and experimentally evaluated hypothesis systems show that LLMs can participate in real discovery when something outside generated prose judges the output.
5. A qualitative CHIWORK study of end-user data analysis shows the relevant capability and friction directly: participants could use generative assistance for information gathering, hypothesis generation, and analysis strategy while struggling to specify context and verify results. Its sample does not establish population prevalence.
6. A 2026 bioinformatics perspective supplies the conceptual warning without pretending it is an experiment: access to functional analysis code does not supply the expertise required to judge the reference data, method, parameters, or scientific interpretation.
7. Science-shaped fan analytics predates LLMs; the adjacent project does not establish a new historical category or a causal increase. It is useful precisely because it is transparent, serious, and published as a fan data product rather than an academic paper. It makes the extra-academic publication category concrete without establishing anything about the creator's institutional affiliation, formal training, or use of an LLM.
8. Part 1 should not put that project's labels or particular musical claim on trial. Doing so would both identify an anonymized example and spend Part 4's central lesson before the author's own evidence earns it. The detailed methodological audit remains private source work, not the argument of this section.
9. Any claim about the project's public reception must be omitted unless separately documented; reception is not required for the series' logic.
10. Our project is not observing this shift from above. It produced the same scientific-looking completeness and later supplies the series' substantive evidence about what those forms can conceal.
11. Part 1 therefore raises the category problem beyond journals and conferences; Parts 2 through 5 show, through the author's own project, what independent adjudication actually requires.

### Claim earned

LLM assistance can make sophisticated analytical activity and research-like artifacts easier to produce, but polished form, reproducible calculation, and internal agreement are not substitutes for independently adjudicated evidence.

### Evidence base and status

Primary research papers, institutional policies and reports, end-user data-analysis research, a domain-expert perspective with its evidentiary status labeled, and public project methodology and analytical claims.

### Required counterarguments

- Humans fabricate citations and overclaim results too.
- A fan data product is allowed to be exploratory, subjective, and entertaining.
- Reproducibility and transparency remain valuable even when they do not establish validity.
- Some LLM-mediated systems produce externally verifiable contributions.
- An outside judge can still be correlated with the generator, selected to reward the candidate, or itself model-generated; externality is necessary for some claims but never sufficient by itself.

The answer is not that these objections are wrong. The series claims rapid propagation in this case and category confusion, not uniquely machine-created misconduct. The adjacent project illustrates that a serious science-shaped data product can be published outside a formal academic venue; it does not establish an LLM cause, creator affiliation, widespread reception, or Part 1's construct-validity argument.

### Standalone context requirement

Part 1 should need no prior series context. Its closing introduction to the audio lab must be short enough that the article remains a field-level argument rather than the first chapter of a memoir.

### Handoff to Part 2

If generated work must be judged outside its own artifact chain, what happens when a real project mistakes plausible implementation and clean outputs for those outside checks?

### Red-team failure conditions

- The post becomes a catalogue of alarming AI anecdotes.
- The unnamed fan project becomes an identifiable antagonist rather than a bounded example of science-shaped work outside formal academic publication.
- The post litigates label reliability, construct validity, or the named musical result and thereby performs Part 4 in miniature.
- The absence of peer review is treated as proof that the adjacent analysis is false.
- Positive acceleration cases receive less methodological scrutiny than negative cases.
- The post ends with "humans must verify AI" without defining what independent verification means.

### Removal test

Without Part 1, the series becomes a personal tool-failure story and loses the reason these incidents matter beyond one repository. It also loses the positive cases needed to explain why people accept the risk.

## Part 2. Two Notebooks Lost

### Working title proposition

**Title:** Two Notebooks Lost
**Subtitle:** In empirical work, setup is the experiment.

### Standalone contract

A reader should understand how two small, plausible implementation choices invalidated a large body of work, why the autonomous system neither blocked nor reported them, and why responsibility for publication is different from manually reconstructing every experiment the system claims to have completed.

The author should be introduced as an experienced, PhD-trained systems researcher attempting an ambitious self-directed research program with LLM assistance. The credential is not an appeal to authority. It establishes that the constraints were understood and written down. The failure was not ignorance of holdouts or implementation fidelity; it was that the system could violate explicit constraints without enforcing or disclosing them.

The existing *Two Notebooks Lost* file remains contemporaneous source material rather than a contract-compliant series draft. Its phrase "the question I started with" refers to the first jam-research notebook question, not the origin of the full Zabriskie audio project. Part 2 will be a separate series draft built from that source. It needs the full set-detection-to-jam-detour chronology and a selective compression of the later governance coda assigned to Parts 4 and 6. This architecture does not authorize changing the contemporaneous source file itself without author approval.

### Series function

Part 2 converts Part 1's general risk into a causal incident. It gives the series stakes, establishes the recurring failure shape, and creates the need for a system rather than advice.

### Question answered

How can mostly correct code produce research that must be discarded in full?

### Argument chain

1. The first notebook leaked performances across training and test while presenting a clean split at the surface.
2. The resulting metrics looked strong because the model could exploit information that should have remained unavailable.
3. Once feature and method decisions had been made against those outcomes, changing the split could not restore independent evidence.
4. The second notebook inherited a 90-second head-and-tail segmentation default despite a methodology document that warned against that exact strategy. Its immediate defect was plan-to-implementation fidelity: the code violated the written method.
5. The constant propagated through scripts, fingerprints, clusters, charts, published posts, and a listening study. The broader debt was measurement validity: the convenient unit of analysis silently defined which portion of the music could count as the phenomenon, a problem that merely making later code match a plan would not solve.
6. The failures share a propagation mechanism but create different methodological debts. Notebook 1 invalidated independence because information crossed a boundary. Notebook 2 first violated its plan and then exposed the separate need to justify that an operational measurement corresponds to the named phenomenon.
7. Part 3 follows the first debt through the holdout, caches, derivatives, and human adaptation. Part 4 returns to the second debt after a much stronger review process still confuses a consistent measurement chain with the claimed acoustic event.
8. The attempted deletion that became archival and the request for progress that produced paperwork show the same lower-stakes bias toward plausible forward action.
9. The reports presented the explicit constraints as satisfied, and the author used those reports to publish claims and involve other people's time.
10. The author remains responsible for publication, but the system failure is the absence of enforcement and disclosure. Requiring manual reconstruction of every completed run would negate the autonomy being evaluated.
11. The improvisation questions encountered during the detour were genuinely interesting but too dependent on disputed labels and operational definitions to yield the clean answer the early work implied.
12. A compact coda records the immediate response: the first written discipline retired Q01 before audio loaded and forced Q02 into a narrower, caveated claim. This belongs here as contemporaneous history, not as proof that the new process solved validity.
13. Part 4 owns the later demonstration that those internal gates could still preserve a meaning error. Part 6 owns the mature operating protocol. Part 5 does not inherit Q01 or Q02.

### Claim earned

Research setup is not preliminary plumbing. A hidden split, constant, label definition, or unit of analysis can be the experiment, and downstream correctness cannot repair it.

### Evidence base and status

Contemporaneous user-authored postmortem, retained repository history, invalidated artifacts, methodology documents, and the record of the takedown.

### Required counterarguments

- A competent researcher remains responsible for claims they publish.
- These failures could occur with a junior human assistant.
- A long checklist may remove the speed benefit that motivated agent use.

The post should concede all three without accepting the premise that autonomy requires the researcher to reperform every mechanical check. The author remains responsible for publication. The system being evaluated is responsible for enforcing or disclosing explicit constraints it claims to have followed. If every split and constant must be reconstructed manually before any output can be used, the system is a fast implementation assistant, not an autonomous researcher. The contribution is the speed, silent violation, and propagation documented inside this project, not a general causal estimate.

### Standalone context requirement

Part 2 must explain the full origin-detour-return chronology, audio question, corpus, and labels in plain language. A standalone reader must learn that automatic set detection was the original goal before the corpus pulled the project into Type I and Type II jam research. The post cannot assume the reader understands those categories or has read Part 1.

### Handoff to Part 3

The two notebooks leave two questions open. Part 3 follows the first: if setup is the experiment, what exactly must be protected, and when does a dataset or artifact stop being independent? The second question, whether the operational measurement represents the phenomenon named in the claim, remains explicitly deferred to Part 4.

### Red-team failure conditions

- The post blames Claude while minimizing the author's approval and publication decisions.
- The train-test leak and segmentation error become generic cautionary tales without their distinct mechanics.
- The rules written afterward are presented as a solution before they are tested.
- Emotional cost substitutes for evidence that the work was invalid.
- The post tries to explain the full later governance system and steals Part 6's work.

### Removal test

Without Part 2, later governance appears as abstract process enthusiasm. The reader never sees why one locally reasonable choice can require deleting an entire line of work.

## Part 3. The Holdout Is a One-Way Door

### Working title proposition

**Title:** The Holdout Is a One-Way Door
**Subtitle:** Evidence cannot independently confirm the claim that opening it helped shape.

### Standalone contract

A reader should gain a practical model of leakage as information flow and be able to classify raw files, performances, shows, caches, features, labels, and human adaptation as part of the same boundary.

### Series function

Part 3 provides the first systematic correction to Part 2. It defines the information boundary and explains why the first cleanup was incomplete.

### Question answered

What does it mean for evidence to be independent after an adaptive research process has already begun?

### Argument chain

1. A split is a promise about unavailable information, not a pair of folders.
2. Audio from the same performance or show can cross the boundary under different filenames, providers, encodings, or track cuts.
3. Derived features and caches inherit the role and contamination state of their source data.
4. The later purge removed 1,120 artifacts descended from the two invalid notebooks as a mixed set. The surviving audit does not allocate every file cleanly between the lineages, so the post must not imply lineage-specific counts. Where provenance is established, N1 descendants inherited information contamination and N2 descendants embodied an invalid measurement premise. Part 3 uses only documented N1 examples to explain leakage and leaves documented N2 examples for Part 4.
5. A check can be perfectly self-consistent and still tautological when the pinned and recomputed values descend from the same mistaken baseline.
6. The researcher is part of the information path. Outcomes influence feature families, thresholds, aliases, exclusions, stopping decisions, and the stories that seem worth testing.
7. This is the connection to HARKing: a hypothesis developed after outcomes are seen can remain valuable exploration, but it cannot be represented as independently confirmed by the same outcomes.
8. A clean boundary therefore requires whole-performance grouping, duplicate control, point-in-time reconstruction, explicit data roles, opaque labels during prediction, frozen hashes, and a record of every opening.
9. Opened data can support engineering, diagnosis, and genuinely unrelated future questions. It cannot be made sealed again for the affected claim, candidate, or adaptive research lineage by deleting the analyst's memory or starting a new branch.
10. An adaptive lineage follows causal influence, not repository ancestry. A new question is unrelated only when its claim, candidate, measurement, thresholds, exclusions, and analysis policy do not inherit outcome-specific choices from the opened result. Reusing a raw corpus does not automatically join the lineage; reusing an adaptation learned from its outcomes does. Ambiguous cases default to opened engineering status and record the disputed dependency.

### Claim earned

Independence is a property of the complete provenance and decision history. Once evidence influences design, it permanently loses sealed-confirmation status for the claim, candidate, and adaptive research lineage it shaped. That does not make the data unusable or contaminated for every unrelated future question.

### Evidence base and status

Leak record, cache-contamination erratum, duplicate and performance grouping audits, point-in-time feature rules, sealed execution protocol, and exploration-versus-confirmation literature.

### Required counterarguments

- Perfect isolation may be impossible in a long-running personal project.
- Small datasets make large sealed sets expensive.
- Researchers necessarily learn from outcomes.
- A mixed purge count does not establish how many artifacts belonged to either lineage without a file-level lineage inventory.

The answer is role separation, not amnesia. Opened data remains useful; the claim made from it changes.

### Standalone context requirement

Part 3 should briefly restate the Notebook 1 leak but must not retell both notebook stories. Its independent value is the information-flow model and the claim-scoped permanent data-role rule.

### Handoff to Part 4

Suppose every information boundary is clean, every constant is justified, and every artifact matches the plan. Does that establish that the measurement means what the paper says it means?

### Red-team failure conditions

- Leakage is reduced to duplicate files or incorrect random splitting.
- The post implies that preregistration eliminates adaptive judgment.
- Engineering data is described as worthless after opening.
- The 38-show sealed set is described as evidence before it is opened under the frozen protocol.
- The post implies that clean independence establishes acoustic truth.
- N2-derived artifacts are called information-contaminated merely because they were removed in the same purge as N1 descendants.

### Removal test

Without Part 3, the final protocol has no defensible account of why data roles remain permanent within an adaptive research lineage or why a new test split cannot repair development already informed by the affected outcomes.

## Part 4. When Every Check Passes

### Working title proposition

**Title:** When Every Check Passes
**Subtitle:** All the artifacts agreed. The interpretation had never been tested.

Here, "every check" means every check in the project's defined internal gate sequence. It does not mean every possible validity check.

### Standalone contract

A reader should understand the difference between internal consistency and construct or measurement validity through a case in which an elaborate research discipline approved a result whose interpretation direct listening made suspect.

### Series function

Part 4 pays the second debt opened by Notebook 2. Better data hygiene resolves neither a segmentation procedure that defines the phenomenon by convenience nor a detector that agrees with labels for the wrong acoustic reason. The Dispatch 002 case demonstrates that the correction built after Part 2 shared this blind spot because every reviewer inspected the same derivative chain.

### Question answered

What kind of error survives approved plans, code review, smoke tests, artifact checks, figure checks, prose checks, and an advisor review?

### Argument chain

1. Notebook 2 had already shown that a convenient segmentation procedure could determine what the analysis was capable of observing. The initial cleanup removed the result without fully generalizing the methodological lesson.
2. The project responded to the lost notebooks by writing R-1 through R-10, assigning agent roles, requiring plans, justifying constants, running smoke tests, logging actions, and obtaining explicit approvals.
3. Dispatch 002 passed the complete internal process.
4. The author listened after publication to the first case and made the interpretation suspect because the detected event occurred near track end rather than at the claimed composed return.
5. The first diagnosis blamed systematically bad labels.
6. The author then reviewed all ten positive Q23 cases through a one-off listening page. This was a single-rater, outcome-aware check performed after the suspicious case and initial hypothesis were known; it was neither blinded nor a multi-rater agreement study. The preserved case table records eight composed-return marks, one track-end mark, and one rejected mark, while the summary below it incorrectly says nine of ten. The series uses the table, names the contradiction, and concludes only that the systematic-bad-label diagnosis failed. Because only selected positives were reviewed, the exercise cannot estimate false negatives, specificity, or detector-wide construct validity.
7. The detector selected the final large onset-density change and was therefore biased toward late events. The broader table contains seven hits, all composed returns 36-127 seconds before the archive boundary; its valid Sand miss is only four seconds beyond that range, while the Gin miss uses the track-end label and the Reba mark was rejected. The review invalidates the strong first diagnosis but does not prove the withdrawal summary's track-end-confound explanation.
8. The labels also permitted more than one operational interpretation, while the analysis silently assumed only one.
9. Every preexisting gate checked consistency among plan, code, artifacts, numbers, figures, and prose. None checked whether the named event was audible at the relevant moment.
10. The first check was independent only in evidence modality: it returned to the audio rather than another derivative artifact. It was not independent in evaluator or information history.
11. The R-11 acoustic reality check was added as a required different evidence path. A later amendment replaced one-off pages with neutral metadata, randomized presentation, no preloaded listener notes, and structured submissions for future checks; that improvement does not retroactively blind the original ten-case review.
12. Even the erratum required additional evidence because the first explanation of the failure was itself wrong.

### Claim earned

Traceability can prove where a claim came from without proving construct or measurement validity. The interpretation requires direct contact with the underlying phenomenon through a different evidence path, with the evaluator, selection procedure, information exposure, ambiguity, and limits reported explicitly.

### Evidence base and status

Dispatch plan and withdrawal record, full gate history, original labeling instructions, the author's ten-case outcome-aware listening record, and the later neutralized R-11 amendment.

### Required counterarguments

- Listening is subjective and can be primed.
- Labels can legitimately operationalize a concept differently from a listener's intuition.
- One spot check is anecdotal.

The post must not imply that the original ten-case review removed priming or supplied inter-rater reliability. It should show that a single outcome-aware listener returned to all ten positive cases and corrected the first diagnosis, then distinguish that historical check from the later neutralized R-11 design for future assignments.

### Standalone context requirement

Part 4 must explain the claimed acoustic event and the review system without requiring knowledge of R-1 through R-10. The event chain matters; the names of every rule do not.

### Handoff to Part 5

Dispatch 002 does not become the recognizer in the next post. Once the jam-research detour exposes its unresolved meaning problem, the project returns to its original automatic song-identification goal. That creates a different question: what counts as valid evidence when the target is no longer an offline research artifact but the behavior of a live application receiving audio through a browser and operating system?

### Red-team failure conditions

- R-11 is presented as a universal solution rather than another fallible measurement process.
- More agents are presented as the answer even though the agents shared the artifact chain.
- The first incorrect erratum disappears from the story.
- Internal consistency is treated as worthless rather than necessary but insufficient.
- Subjective musical boundaries are forced into false precision.

### Removal test

Without Part 4, the series falsely implies that clean data roles, written plans, and reviewer agents are sufficient. Part 6's independent reality checks would have no demonstrated necessity.

## Part 5. The Browser Is Part of the Experiment

### Working title proposition

**Title:** The Browser Is Part of the Experiment
**Subtitle:** A model metric cannot validate the system that carries audio to the decision.

### Product/research boundary

SetScope is a viewer-facing song guesser. The song name on screen is a product output, not scientific evidence. The browser and live-show records are product-evaluation material used to diagnose behavior and make release decisions. The series may use that history as support for a narrower methodological claim about deployed-path evaluation, but it must never imply that guessing a setlist is itself a scientific contribution.

### Standalone contract

A reader should understand why a live product can contradict an offline model report, how to localize the failure with transport records, and why end-to-end rehearsals belong to product evaluation rather than ceremonial demonstration.

### Series function

Part 5 marks the return from the jam-research detour to the project's original automatic song-identification target, then moves from measurement validity to execution and transport validity in that deployed system. Dispatch 002 is historical instruction, not a recognizer component. The post also validates the user's repeated insistence that browser behavior mattered more than incremental offline percentage gains.

### Question answered

What was the recognizer actually hearing when the browser demonstration contradicted the model reports?

### Argument chain

1. A historical 46/54 result from five held-out segmented shows was described as roughly 85 percent top one.
2. That number did not measure continuous capture, premusic behavior, state stability, transitions, unknowns, or publication decisions.
3. Browser rehearsals produced impossible or contradictory behavior: locks before music, unstable identities, long delays after unanimous votes, and a six-minute run with no admitted music.
4. The initial temptation was to explain those outcomes as model errors.
5. The actual deployed path included Chrome, macOS routing, BlackHole, capture, segmentation, resampling, a music gate, several acoustic families, a temporal controller, held state, and the UI.
6. FFmpeg's AVFoundation capture lost samples, and an earlier resampler inserted periodic digital-zero gaps. Segment timestamps still made the session appear healthy.
7. Native CoreAudio capture plus source-frame, finalized-sample, and decoded-sample reconciliation exposed transport validity directly.
8. Exact-byte replay separated model and policy changes from changing audio input.
9. Once transport was valid, a gate-hysteresis experiment showed a separate state-policy problem without changing the captured bytes.
10. Two-song and whole-show tests exposed transition, shutdown-drain, catalog, and controller problems that track crops could not express.
11. A complete product evaluation must therefore include initial-lock precision and coverage, latency, false switches, transition behavior, nonmusic safety, abstention, capture validity, and publication decisions.
12. The preregistration required one installed candidate and runtime policy to remain unchanged after music began, and the audit records the exercised model and policy versions. No canonical pre-music receipt preserving every required hash, route, mode, publication-state, and start-time field has been located, so the post must distinguish intended frozen conditions from preserved compliance proof. The August 13 canary nevertheless ran prospectively on a genuinely unseen show: because the show had not yet happened, none of its audio or outcomes could have influenced training, model selection, or an earlier test result.
13. The runtime emitted correct identities for 10 of 12 song performances during the show. A post-show internal acoustic-family diagnosis found 11 of 12, but it is not a second product-capability count. The earned live product observation is the narrower 10-of-12 emitted-output fact.
14. The run did not yield the frozen whole-show metric it was intended to produce. Set 1 failed the preregistered timebase-integrity gate, and Set 2's later 61.5 percent held-state calculation used approximate post-show boundaries rather than the required frozen, hashed truth timeline.
15. The remaining post-show diagnostics were still specific and useful: three observed false switches, two observed misses, a debut cover outside the closed catalog, false identity changes inside long jams, correct acoustic evidence blocked by transition policy, and a Set 1 capture gap. They diagnose the product without becoming a preregistered estimate of its accuracy or latency.

### Claim earned

The behavior a viewer experiences belongs to the complete deployed path. A component result cannot predict transport stages, state policies, display rendering, or publication behavior that it never exercised. The live field test showed correct blind controller outputs on future live audio while also showing that the transition controller exercised on August 13 failed specific continuous-state requirements. The preserved record does not establish a UI-observed state or outward chat publication, and this historical live-runtime result does not determine the readiness of a later candidate.

### Evidence base and status

Browser session artifacts, source-frame telemetry, captured waveforms, exact-zero analysis, archive-versus-browser A/B, exact-byte replay, two-song diagnostics, whole-show replay, the August 13 prospective canary and post-show audit, and frozen browser protocols. The August 13 field test is prospective, live-runtime through controller state, shadow, and descriptive rather than metric-eligible for the intended whole-show estimate. No preserved receipt extends it to confirmed display or outward publication scope.

### Required counterarguments

- A few browser runs are not a population-level accuracy estimate.
- Fixing capture does not improve the classifier by itself.
- Offline evaluation remains necessary for controlled comparison and coverage.

The post should agree. End-to-end evidence and corpus evidence answer different questions and must be used together.

### Standalone context requirement

Part 5 should define SetScope, its job, and the disputed 85 percent in a compact opening. It should not require the reader to understand the earlier jam-segmentation research.

### Handoff to Part 6

The project has answered the smallest product-capability question by direct observation: a blind system can emit correct song identities during a live Goose show whose audio did not exist when the system was built. It has not produced a valid whole-show accuracy estimate, a viewer-benefit estimate, or an answer to whether the system can maintain reliable current-song state across a catalog and a tour. How can an agent continue improving the product while keeping those open measurements from being silently rewritten as solved?

### Red-team failure conditions

- The browser incident is treated as proof that model quality did not matter.
- A successful Animal or two-song replay is promoted into product accuracy.
- Product telemetry is described without explaining what decision it enabled.
- The original overbroad 85 percent is replaced by another selective metric without its raw denominator.
- Live demonstration is treated as a substitute for sealed corpus evaluation.
- A failure of the August 13 controller is written as a readiness verdict about a later candidate.

### Removal test

Without Part 5, the series ends at research artifacts and never shows that a strong offline component result can coexist with an invalid deployed path. The final multi-axis evidence model would be incomplete.

## Part 6. A Discovery Loop That Can Say No

### Working title proposition

**Title:** A Discovery Loop That Can Say No
**Subtitle:** Bound the agent, preserve the failures, and let claim-specific evidence decide.

### Standalone contract

A reader should receive a reusable operating protocol and a candid account of what that protocol did and did not establish in the recognizer program. The post can be complete before a sealed result exists because its claim is about the operating discipline, not proof that a song guesser is science. It must remain honest if later quantitative evaluations disappoint.

### Series function

Part 6 synthesizes the three case-derived boundaries, reports the strongest current engineering result without laundering it into confirmation, and closes by separating the deployed prototype from the stronger reliability, viewer-benefit, and publication claims still open.

### Question answered

What practical system lets an LLM agent perform substantial empirical work while making evidence roles, permissions, and violations durable and inspectable?

### Argument chain

1. A long instruction is not durable methodology. The model can summarize it, forget distinctions, substitute a shorter goal, or optimize around inconvenience.
2. The methodology must live in versioned artifacts, explicit data roles, hashes, execution permissions, immutable results, and state transitions.
3. The useful unit of autonomy is a bounded experiment with a frozen question, permitted inputs, metric, analysis policy, budget, falsifier, and stopping rule.
4. Planning, execution, and scoring must be separated so labels and outcomes are unavailable when predictions are created.
5. Negative and invalid results remain in the ledger. They are not overwritten by the next successful run.
6. Evidence is classified on separate axes: information access relative to the candidate, temporal relation, system scope, publication-action scope, and metric eligibility. Those dimensions cannot substitute for one another.
7. The August 13 live field test is the first direct runtime answer in chronology and execution: blind recognition emitted correct identities on genuinely future live audio through capture, gating, inference, and controller state. Its failed Set 1 capture gate, missing frozen truth timeline, and absent UI-observation receipt mean that the record remains live-runtime rather than confirmed complete-product evidence and that post-show counts and held-state calculations remain descriptive diagnostics rather than formal performance estimates.
8. The later v0532 result is a strong outcome-informed engineering result over all 75 opened shows assigned to the engineering side of the frozen v0494 split: 65 correct initial locks by 90 music seconds with ten abstentions, 100 percent selective initial precision, 86.67 percent raw correct coverage, and offline decision-pipeline transition metrics under its recorded policy. The 75 shows are a complete opened engineering population under that protocol, not a representative sample of all Goose performances.
9. Because the recovery rule was designed after v0531 outcomes were inspected, v0532 cannot establish independent generalization or retroactively repair the August 13 canary. Its larger sample and metric eligibility do not demote the earlier result's prospective status.
10. The 38-show sealed protocol allows one opening only after the final candidate and all engineering prerequisites are frozen. The 38 were assigned from the same 113 eligible shows by a frozen within-year hash split, without song or outcome inspection. That prevents outcome-based selection and preserves the eligible population's year distribution; it does not establish representativeness beyond the verified corpus. The opening can supply one confirmation result for the frozen candidate; afterward the shows are opened engineering evidence for any adaptive work, whether the candidate passed or failed.
11. Additional final-candidate complete-product evaluation with display receipts is required before making broad quantitative claims about future user-visible behavior and, if separately authorized, the outward publication path. Repeated future shows do not remain independent confirmation if their outcomes change the candidate or stopping policy.
12. The final conclusion can say that a viewer-facing prototype exists, emitted correct live identities, and left major measurements open. It cannot call the product useful to viewers without direct or explicitly first-person evidence. If it reports stronger quantitative or publication-readiness claims, it must report the result that occurs, not the ending the series would prefer.

### Claim earned from the current evidence

The project has built a more explicit and auditable process that catches several previously invisible failure classes, sharply limits what current engineering results are allowed to claim, and produced a live fan tool that made correct blind guesses on never-before-existing audio. It has not yet produced a metric-eligible whole-show estimate of reliability, and the product does not need to masquerade as a scientific result for that narrower accomplishment to matter.

### Claim that remains blocked

No present evaluation establishes that the recognizer maintains reliable continuous current-song state across the catalog, benefits viewers, or should be enabled for automatic public setlist publication. Those are stronger product claims and decisions than "the runtime emitted correct identities during one live show."

### Available record and status

Research-discipline history, preregistration/result pairs, immutable hashes, experiment ledger, August 13 live-canary preregistration and audit, v0532 opened-engineering report, and the frozen sealed-execution and browser-rehearsal protocols. No future sealed result or additional final-candidate live record is counted as available.

### The current ending and three possible future updates

**No stronger result yet:** close on the operating protocol and the narrow product fact already observed: SetScope emitted correct guesses during a genuinely new live show, while its broad reliability, viewer benefit, and automatic-publication safety remain open. This is a complete ending for Part 6, not a scientific validation claim.

**Required evidence passes:** report the frozen thresholds, raw denominators, uncertainty, misses, abstentions, slices, sealed result, and final-candidate prospective behavior. The conclusion is that this candidate passed these gates, not that the governance system guarantees truth.

**Mixed result:** report which gates and evidence coordinates passed and failed. A model may clear identity but fail nonmusic safety, clear sealed audio but fail live transport, or maintain precision only by abstaining too often. The series ends with a narrower usable claim.

**Required evidence fails:** report the failure in full and preserve the protocol. The process still has value if its permissions block automatic publication and its durable record makes any attempt to call an opened result sealed explicit and auditable. The final takeaway becomes that a useful research process can preserve an unwelcome result without rewriting its status.

### Required counterarguments

- The governance cost may consume much of the speedup.
- A single personal project cannot validate a universal methodology.
- Durable artifacts can become paperwork that is internally consistent but substantively empty.
- Human approvals can become ceremonial.
- The highlighted governance catches were selected because they were memorable; without a complete opportunity ledger they do not estimate how often the protocol catches errors or how many it misses.

The post must treat those as continuing risks. The protocol is an operating hypothesis tested by this project, not a finished theory of research automation.

### Standalone context requirement

Part 6 must introduce the three case-derived boundaries, their non-exhaustive scope, SetScope, and the distinctions among product output, evaluation record, and research claim without recapping every incident. A reader should be able to adapt the protocol without mistaking it for a complete validity taxonomy or a validated universal method.

### Ending requirement

The ending must return to the controlling question: which decisions require durable roles, explicit permissions, and evidence outside the agent's artifact chain? It should name the remaining unresolved evidence rather than closing with a generic claim that humans and AI work best together.

Part 6 may be drafted and published without the one-time sealed result because its conclusion is the operating protocol and the distinction among a deployed prototype, a reliability estimate, viewer benefit, and automatic publication. A broad quantitative reliability claim still waits for the one-time sealed result and an appropriately clean complete-product evaluation. A claim that automatic public posting worked additionally requires an actual outward-publication record.

### Red-team failure conditions

- The engineering result is called sealed, out of sample, prospective, or product grade.
- Selective precision appears without raw coverage and abstentions.
- Governance artifacts are presented as evidence that the model works.
- A failed sealed result is omitted, delayed, or reframed as merely another development iteration.
- The protocol is reduced to multiple agent roles rather than data and evidence boundaries.
- Part 6 claims the process solved the problem merely because it is elaborate.

### Removal test

Without Part 6, the series diagnoses three case-derived boundaries but never offers or tests a coherent operating response. It would end as a postmortem rather than a research program.

## Part 7. What Happened on the August Run

### Working title proposition

**Title:** What Happened on the August Run
**Subtitle:** A live product is a history of versions, failures, and emitted guesses, not one score.

### Standalone contract

A reader should understand what SetScope is, how it was used across Goose's eleven-show August 13-28 western run, what each instrumented version emitted or published, how the system changed, which runs were operationally invalid, and what the complete record supports saying about performance. Direct viewer experience remains qualitative unless separately recorded. No knowledge of the notebook failures should be required.

### Series function

Part 7 is the longitudinal product payoff. Parts 1-6 explain how the project learned to distinguish information, meaning, execution, product output, and research claim. The retrospective applies those distinctions to the defined August run without pretending it was one frozen experiment, represented the complete 24-show summer tour, or made the guesser a scientific instrument.

### Question answered

Across Goose's eleven-show August 13-28 western run, how often and how quickly did each SetScope version emit a correct, stable current-song identity, and where did the product path fail?

### Argument chain

1. SetScope's product promise is narrow: emit and display a current-song guess without waiting for an external setlist.
2. August 13 showed that the task was possible in a real live show, but one run could not describe operation across the full eleven-show August window.
3. The per-show ledger records the exact product-stratum tuple, run mode, session timebases, audio health, music admission, guesses, state entries and clears, corrections, abstentions, separately receipted UI and publication actions, and post-show truth reconciliation.
4. Because the product changes during the August run, results must be stratified by stable version and policy. Later repairs cannot be credited to earlier runs, and sparse strata may support only descriptive counts rather than meaningful comparison.
5. Product behavior has several dimensions: operational availability, identity correctness and coverage, time to first correct guess, time to stable lock, false-switch dwell, abstention, display observation, and public-post behavior.
6. Capture failures, premusic admission, unknown material, segue ambiguity, recognition errors, controller errors, and outward-publication failures must remain separate even when they all produce a wrong or missing title on screen.
7. External setlist timing is comparable only when both systems' clocks and events are defined. "First SetScope guess" and "external database ingestion" are not automatically equivalent timestamps.
8. Memorable song examples can explain mechanisms after the complete denominators are shown; they cannot select the verdict.
9. Post-show truth version 1 must be reconciled by a different person or clean agent process and hashed without access to SetScope outputs. Outcome-aware truth can support diagnosis but cannot enter the primary post-policy metrics.
10. Policy v2 freezes the base population and artifact schemas; the prospective v3 addendum resolves report scopes, normalized attempts and restarts, truth corrections and topology, delivery linkage, two-host clock evidence, uniform record envelopes, and additional adversarial vectors before the August 15 show. The invalid v1 pre-freeze draft remains documented rather than being silently relabeled.
11. The conclusion reports the frozen metrics and the versioned operating profile that occurred without assigning a post-hoc grade.

### Claim available now

As of the frozen ledger snapshot at 2026-08-15 17:41:22 UTC, no reconciled post-policy August-run outcome appears in the record. August 13 is a documented pre-policy pilot; August 14 remains an unreconciled ledger row. The post's prospective collection question, event schema, denominator policy, versioning rule, and possible endings were fixed before the remaining shows.

### Claim available after the tour

The complete, reconciled record may support bounded statements about how SetScope operated during the defined August 13-28 period, under the versions and conditions actually observed. It cannot represent the 13 earlier June-July shows sharing the source tour label. Direct user feedback may support explicitly first-person or qualitative statements about usefulness. Neither becomes a scientific claim about Goose's music or proof that the research protocol generalizes.

### Available record and status

August 13 supplies the first documented case, while August 14 remains an unreconciled pre-policy ledger row. Policy v2 and its prospective v3 addendum freeze the eleven-show August 13-28 western-run population and govern the August 15-28 collection window. The first draft's in-place freeze failure is preserved as an incident rather than treated as prospective evidence. Subsequent show logs, capture telemetry, version manifests, post-show setlists, external observation receipts, and viewer/operator notes are being collected. Results prose waits for completed run states, prediction-blind reconciled truth, and deterministic scoring under that combined policy.

### Four valid result shapes

The retrospective does not assign a post-hoc grade. These scenarios test whether the narrative remains honest:

**Favorable operating profile:** the frozen metrics show frequent correct early locks, little incorrect dwell, and high operational availability. Report the versions, denominators, misses, and remaining failure slices.

**Mixed operating profile:** metrics vary materially by song, show, condition, or version. Report the strata and do not average away the boundary of performance.

**Poor operating profile:** correct locks are late, unstable, sparse, or frequently wrong. Preserve the full record and say so without converting the discussion into a viewer-effect claim.

**Operationally compromised record:** capture, launch, clock, logging, or truth-reconciliation failures prevent a credible August-run estimate. Report product availability and the missing measurement rather than substituting successful anecdotes.

### Required counterarguments

- The software changed too often for one aggregate number.
- Post-show truth and song boundaries can be approximate, especially around segues.
- External setlist timestamps may not be semantically comparable.
- First-person or friend feedback can describe experience but does not estimate a general viewer effect.
- A product retrospective is not a scientific paper.
- Truth reconciliation can be biased if the reconciler has seen SetScope's outputs; primary post-policy scoring therefore requires a prediction-blind first truth hash.
- Frequent software changes may leave version strata too small for comparative claims even when every individual run is valid.
- The policy and metric window were frozen only after the August 13-14 pilots were observed; it is an outcome-informed product retrospective, not a preregistered eleven-show study.
- Version strata remain confounded with song, venue, capture condition, and chronology, so their differences are descriptive rather than causal.
- Capture and stream failures may be informative missingness and must remain in the operating record even when no song opportunity can be scored.

The post should agree with all ten and design its claim accordingly.

### Standalone context requirement

Part 7 must define SetScope, its viewer-facing purpose, the tour and cutoff, the versions included, the product metrics, and the limits of the truth and timing records. It should summarize the methodology history in no more than the context needed to explain why failed runs and changed versions remain visible.

### Red-team failure conditions

- Different versions are pooled into one headline accuracy number.
- Invalid captures or failed starts disappear from the product-availability denominator.
- Selective precision appears without coverage, abstention, or valid listening time.
- SetScope is compared with an external service using incompatible timestamp meanings.
- Distinctive-song anecdotes decide the aggregate verdict.
- A useful fan tool is described as scientific validation.
- Outcome-aware truth or sparse version strata are silently treated as primary comparative evidence.

### Removal test

Without Part 7, Parts 1-6 still form a complete methodological argument, but the series ends after one live field test and never pays off the reader's product question across the August run. The retrospective is the narrative and operational conclusion, not a premise retroactively required to make the earlier posts true.

## 5. Post Independence Matrix

Each post must contain enough local context to stand alone. Independence does not mean repeating the entire series.

| Part | Context it must restate | Concept it must define locally | Independent value | Context it may omit |
| --- | --- | --- | --- | --- |
| 1 | None | External judge, research-like form, publication context, exploration vs confirmation | Framework for evaluating LLM-mediated research claims across formal and informal publication contexts | Detailed construct-validity adjudication and audio history |
| 2 | Audio question, corpus, two notebook goals | Setup decision, leakage, segmentation | Concrete postmortem for agent-assisted empirical work | Field-wide literature survey |
| 3 | One-paragraph Notebook 1 recap | Information flow, data role, opened vs sealed | Practical model of leakage and adaptive analysis | Dispatch 002 and browser path |
| 4 | Brief origin of the governance rules | Internal consistency, construct validity, operationalization | Why review chains can agree on the wrong construct | Detailed split mechanics and live product architecture |
| 5 | SetScope purpose and disputed historical metric | Component metric, transport validity, complete-product evidence | End-to-end evaluation and observability case | Jam taxonomy and full rule history |
| 6 | SetScope, the case-derived three-boundary model, current evidence status | Bounded experiment, evidence coordinates, immutable result | Reusable protocol plus a prospective result whose evidentiary limits remain explicit | Full incident reconstructions |
| 7 | SetScope purpose, August-run cutoff, version history | Product availability, versioned field record, comparable clocks | Honest longitudinal product retrospective | Detailed notebook, governance history, and earlier summer shows |

Every post must provide:

1. one explicit question;
2. one concrete incident or evidence base;
3. one distinction the reader can reuse;
4. one conclusion the evidence supports;
5. one limitation preventing a stronger conclusion; and
6. one sentence explaining what remains unresolved.

## 6. Handoff Integrity

The end of each post should create a real unresolved problem, not advertise the next installment.

| Handoff | Premise carried forward | New question | Invalid shortcut |
| --- | --- | --- | --- |
| 1 to 2 | Independent judgment separates acceleration from self-confirming form | What does self-confirmation look like in practice? | Repeating citation hallucinations as the personal failure |
| 2 to 3 | The two notebooks create different debts; Notebook 1 concerns information independence | What information must be kept outside the design loop? | Treating a better random split as the complete repair or pretending it also resolves Notebook 2 |
| 3 to 4 | Clean information boundaries can support independent evaluation | Does the measurement correspond to the named phenomenon? | Treating preregistration as construct validation |
| 4 to 5 | The jam-research detour has exposed a meaning failure; the project now returns to its original automatic song-identification goal | What evidence describes the input path and behavior of the deployed recognizer? | Pretending Dispatch 002 became a validated recognizer component or treating one listening check as product evaluation |
| 5 to 6 | Complete-product behavior must be described on multiple evidence axes | How are all boundaries enforced during autonomous work? | Treating observability or browser testing as the entire protocol |
| 6 to 7 | A deployed prototype, a reliability estimate, viewer benefit, and automatic publication are separate outcomes | What did the instrumented product path actually emit across the defined eleven-show August run? | Treating one live show, one frozen model, or one aggregate score as the August-run story |

## 7. Terminology That Must Not Drift

### Output, artifact, evidence, and claim

- **Output:** anything produced by a model, script, person, or instrument.
- **Artifact:** a persisted output with provenance.
- **Evidence:** an artifact or observation that is valid for a specified question under a specified design.
- **Claim:** the interpretation the author asks the evidence to support.

The series should never use these as synonyms. A large artifact set is not automatically a large evidence base.

### Reproducibility and replicability

- **Computational reproducibility:** the same data, code, and method produce a consistent result.
- **Replicability:** a new study aimed at the same scientific question produces a consistent result.

The series uses reproducibility as necessary provenance, not proof of scientific validity.

### Exploration and confirmation

- **Exploration:** outcomes may influence what is tried, retained, or interpreted.
- **Confirmation:** the question, candidate, metric, and analysis policy are frozen before the confirming outcomes are exposed.

Exploration is not a lesser moral category. It supports a different claim.

### Internal consistency, construct validity, and execution validity

- **Internal consistency:** plan, implementation, artifacts, numbers, figures, and prose agree.
- **Construct or measurement validity:** the operational measurement and its interpretation correspond to the phenomenon named in the claim.
- **Execution or transport validity:** the deployed system receives and processes the phenomenon through the path the evaluation assumes.

The series should not redefine the broader statistical term *external validity* to cover either of these questions. It should name construct or measurement validity in Part 4 and execution or transport validity in Part 5.

### Evidence coordinates, not evidence buckets

The project previously treated *engineering*, *sealed*, *prospective*, *browser*, and *product* as though they were alternatives. They are not. They describe different axes, and one evidence item occupies a position on every relevant axis:

1. **Information access relative to the candidate:** *opened/adaptive* outcomes are available for diagnosis or have influenced development; *unavailable-at-freeze* outcomes could not influence the frozen candidate. Historical data can be unavailable through a sealed policy; genuinely future data can be unavailable because the event does not yet exist. Once either outcome is inspected, it is opened for later candidates even though the original frozen run retains its historical status.
2. **Temporal relation:** *retrospective* evidence predates or was available before the candidate and policy were frozen; *prospective* evidence arrives afterward. A future event can become opened engineering evidence for later candidates once it is inspected.
3. **System scope:** *component* evidence exercises one model or isolated stage; *integrated-offline* evidence exercises the decision pipeline over recorded inputs while bypassing live transport and display; *live-runtime* evidence exercises actual capture, gates, inference, and controller state; *complete-product* evidence additionally preserves a receipt for the user-visible state. A browser session reaches only the last stage for which a receipt survives.
4. **Publication-action scope:** *shadow* evidence may exercise the publication decision logic but disables the outward write; *publication-path* evidence includes the actual user-visible or automatic outward action.
5. **Metric eligibility:** *descriptive/diagnostic* evidence can establish that an event occurred and expose failure modes; *metric-eligible* evidence satisfies the frozen capture, truth, denominator, and scoring requirements for a stated performance estimate.

For the frozen canary candidate, August 13 was unavailable-at-freeze, prospective, live-runtime evidence through controller state. Its saved observations are descriptive rather than metric-eligible for the intended whole-show estimate. Once the post-show setlist and traces were reviewed, the show became opened/adaptive evidence for every later candidate; that does not erase what the original prospective run demonstrated. The preserved audit calls the sessions shadow traces, the preregistration disabled authoritative setlist and marker writes, and no UI-observation or chat-delivery receipt appears in the reviewed artifacts. The available record therefore does not support confirmed display or outward-publication scope. No percentage or claim should appear without the coordinates that justify its use.

### Model, agent, and system

- **Model:** fitted or prompted component producing an output.
- **Agent:** a model operating with tools, context, and authority over actions.
- **System:** people, agents, code, data, infrastructure, interfaces, policies, and incentives together.

Responsibility and failure claims should identify which level they concern.

## 8. Evidence Architecture

The series should make the role of evidence visible, not merely cite a large quantity of it.

| Evidence | Design or coordinates | Supports | Does not support |
| --- | --- | --- | --- |
| Published citation and review studies | External literature; retain each source's study design and population | Existence and estimated prevalence of specified LLM-mediated behaviors | Attribution of every suspicious paper or review to AI |
| Private adjacent-project methodology audit | Private editorial fact-checking; not a public anonymous evidence object | Existence of a serious, transparent, science-shaped data product published outside a formal academic venue; material for a possible separately attributed critique | Part 1's construct-validity argument, hidden implementation details, proof that its labels are wrong, creator affiliation, public inspectability, or permission to identify the project |
| Two-notebook repository record | Retrospective, opened historical record; descriptive | What failed in this project and how artifacts propagated | Population claims about all LLM-assisted research |
| Provenance and split audits | Retrospective audits of information access and adaptation history | Information roles and contamination findings | Construct validity or product accuracy |
| Author's original ten-case audio review | Opened, retrospective, different modality, single outcome-aware evaluator, descriptive | Whether this listener heard the claimed event in the ten selected positive Q23 cases; evidence that corrected the first diagnosis | Blinded judgment, inter-rater reliability, evaluator independence, or population-level validity |
| Later neutralized R-11 protocol | Prospective design specification; no result by itself | A more reproducible design for future acoustic-reality checks using neutral metadata, randomized presentation, and structured submissions | Retroactive blinding of the original review or proof that one listener's construct is shared |
| Offline whole-show replay | Information access and metric eligibility vary by run; retrospective; integrated-offline scope without live transport | Behavior over opened or sealed recorded shows under a specified runtime | Live capture reliability or future-show performance unless the design supports it |
| Browser capture telemetry | Information and temporal status vary by run; live-runtime through controller state, extending to complete-product only with a display receipt; shadow or publication action stated separately | Integrity and decisions through the last receipted stage | Broad catalog accuracy from a few rehearsals or unreceipted display behavior |
| v0532 engineering result | Opened/adaptive, retrospective, integrated-offline, shadow, metric-eligible for its recorded engineering question | Performance of an outcome-informed candidate on 75 opened shows | Independent generalization, live transport, UI behavior, or outward publication |
| Future sealed result | Unavailable-at-freeze through the seal for one candidate; retrospective; integrated-offline; no outward publication action; metric-eligible if gates pass | That candidate's performance on the defined sealed population | Future-tour representativeness, live transport, unlimited future validity, or automatic publication safety |
| August 13 prospective live canary | Unavailable-at-freeze and prospective for the canary candidate; opened/adaptive for later candidates after post-show review; live-runtime through controller state; shadow; descriptive rather than metric-eligible for the intended whole-show estimate | The temporally independent fact that dateless, setlist-free recognition emitted correct identities during one future show; saved runtime failure modes | Confirmed UI delivery, a whole-show accuracy or latency estimate, catalog-wide reliability, exercised outward publication, or launch readiness |
| Additional final-candidate prospective operation | Unavailable-at-freeze and prospective relative to a frozen final candidate; complete-product if a display receipt survives, otherwise live-runtime through the last receipted stage; shadow or publication-path as stated; metric eligibility depends on frozen capture and truth gates | Real future behavior through the last independently receipted deployed stage and specified action policy | Correctness of unobserved decisions or later candidates after adaptation |
| August-run field ledger | Versioned longitudinal product record; information and metric status stated per run; all reviewed outcomes opened for subsequent versions | Operational behavior of the recorded versions during the August 13-28 window, plus display or publication behavior only when separately receipted | One frozen-candidate accuracy claim, the 13 earlier summer shows, unobserved shows, future-tour reliability, scientific claims about the music, or universal protocol validity |

### SetScope coordinate inventory

| Item | Information access | Temporal | System scope | Publication action | Metric status |
| --- | --- | --- | --- | --- | --- |
| Historical 46/54 segmented result | Leakage-disjoint for the exact 54 rows at file, performance, and show-date levels | Retrospective | Component crops | None | Eligible only for its narrow 45-label segmented sample; not live accuracy |
| Corrupt browser sessions | Opened/adaptive | Retrospective rehearsal | Complete-product attempted, invalid capture | Shadow | Invalid for recognition metrics; diagnostic incident |
| Repaired native-capture browser sessions | Opened/adaptive | Retrospective rehearsal | Live-runtime or complete-product according to preserved UI receipt | Shadow | Diagnostic unless the named run satisfies frozen truth and capture gates |
| Animal exact-byte gate A/B | Opened/adaptive | Retrospective | Integrated-offline replay | Shadow | Diagnostic comparison, not live-system evidence |
| Dr. Darkness to Drive browser run | Opened/adaptive | Retrospective rehearsal | Live-runtime or complete-product according to preserved UI receipt | Shadow | Diagnostic two-song incident |
| Dr. Darkness to Drive exact-byte replay | Opened/adaptive | Retrospective | Integrated-offline replay | Shadow | Diagnostic policy comparison |
| August 13 canary | Unavailable-at-freeze for canary; opened for later candidates | Prospective | Live-runtime through controller state; no preserved UI-observation receipt | Shadow | Descriptive, not eligible for intended whole-show metric |
| v0532 75-show result | Opened/adaptive | Retrospective | Integrated-offline | Shadow | Eligible for frozen engineering question only |
| v0532 browser-rehearsal protocol | Not an outcome | Future design | Intended complete-product | Shadow unless separately authorized | No behavioral result yet |
| 38-show sealed protocol | Unavailable-at-freeze if provenance remains clean | Retrospective audio | Intended integrated-offline | None | No result yet |
| August-run ledger | Varies by version before each show; opened for every later adaptive version after review | Prospective operation accumulated from August 13-28 | Live-runtime, display, or publication scope stated per receipted stage; invalid operation otherwise | Shadow or publication-path stated per run | Ongoing; eligibility determined per capture session, song opportunity, and aggregate policy |

### Editorial artifact index

The public prose will need durable, reader-accessible citations or archived excerpts where appropriate. During outline review, the canonical local artifacts are:

- Notebook source and immediate Q01/Q02 response: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_drafts/two-notebooks-lost.markdown`
- Earliest surviving live-song timing preregistration located during this audit: `/Volumes/Zabriskie Work/repos/zabriskie-audio-local-resume/tools/audio_detection/cloud/v0135-live-timebase-fusion-preregistered.md`, committed July 30, 2026. It corroborates the later return to live song recognition but does not independently establish that set detection preceded the jam-research detour; that ordering remains the author's first-person chronology.
- Cache-contamination erratum: `/Users/cmeiklejohn/GitHub/zabriskie-audio-research/docs/logs/r7-errata-2026-05-17-cache-contamination.html`
- Dispatch 002 withdrawal: `/Users/cmeiklejohn/GitHub/zabriskie-audio-research/docs/logs/dispatch-002-withdrawal.html`
- Research-discipline history: `/Users/cmeiklejohn/GitHub/zabriskie-audio-research/docs/research-discipline.html`
- Neutralized R-11 assignment protocol: `/Users/cmeiklejohn/GitHub/zabriskie-audio-research/supabase/migrations/20260524000000_seed_draw1_r11_decoupling_check_v1.sql`
- R-11 plan lock: `/Users/cmeiklejohn/GitHub/zabriskie-audio-research/artifacts/draw1/r11_plan_lock_submission.json`
- Browser-capture incident: `/Volumes/Zabriskie Work/repos/zabriskie-audio-local-resume/tools/audio_detection/cloud/v0506-browser-capture-input-integrity-incident.md`
- August 13 preregistration and audit: `/Volumes/Zabriskie Work/repos/zabriskie-audio-local-resume/tools/audio_detection/cloud/v0519-2026-08-13-prospective-live-canary-preregistered.md` and `v0521-2026-08-13-live-show-audit.md`
- v0532 preregistration and result: `/Volumes/Zabriskie Work/repos/zabriskie-audio-local-resume/tools/audio_detection/cloud/v0532-continuous-unknown-recovery-preregistered.md` and `v0532-continuous-unknown-recovery-result.md`
- Sealed execution protocol: `/Volumes/Zabriskie Work/repos/zabriskie-audio-local-resume/tools/audio_detection/cloud/v0512-sealed-confirmation-execution-protocol.md`
- August-run retrospective policy v2: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_drafts/setscope-summer-tour-2026-retrospective-policy.md`
- Prospective policy v3 addendum: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_drafts/setscope-summer-tour-2026-policy-v3-addendum.md`
- Policy v3 machine contract: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_data/setscope_summer_tour_2026_v3_addendum.yml`
- Invalid v1 freeze incident: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_drafts/setscope-summer-tour-2026-policy-v1-freeze-incident.md`
- Frozen August-run manifest: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_data/setscope_summer_tour_2026.yml`
- Ongoing version and run-validity ledger: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_data/setscope_summer_tour_2026_runs.yml`
- Frozen run-record schema: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_data/setscope_summer_tour_2026_run_schema.yml`
- Frozen event, truth, UI, external-observation, and scoring-report schemas: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_data/setscope_summer_tour_2026_artifact_schemas.yml`
- Frozen scoring derivation and adversarial vectors: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_data/setscope_summer_tour_2026_scoring_spec.yml`
- Policy, manifest, and schema digests: `/Users/cmeiklejohn/GitHub/cmeiklejohn.github.io/_drafts/setscope-summer-tour-2026-retrospective-policy.sha256`

## 9. Deliberate Repetition And Forbidden Repetition

Some ideas must recur because they are the throughline. They should gain meaning each time.

### Deliberate recurrence

- **The surface looks complete:** scholarly prose in Part 1, notebooks in Part 2, audit trails in Part 4, percentages and UI in Part 5.
- **The outside judge:** primary sources and evaluators in Part 1, split audits in Part 3, audio in Part 4, source frames and complete-product scoring in Part 5, sealed and prospective evidence in Part 6. Each judge still requires an independence and construct check of its own.
- **The human remains inside the system:** publication responsibility in Part 2, adaptation in Part 3, primed diagnosis in Part 4, insistence on browser tests in Part 5, approval and veto limits in Part 6.
- **Failure must change status:** invalid notebooks, contaminated caches, withdrawn dispatch, invalid browser sessions, opened engineering results, final sealed verdict.
- **The product remains concrete:** the original song-guesser ambition in Part 2, the browser path in Part 5, the narrow live result in Part 6, and the versioned tour outcome in Part 7.

### Forbidden repetition

- Part 3 must not retell Notebook 2 at length; it must identify that failure as a separate debt deferred to Part 4.
- Part 4 must briefly reconnect its meaning problem to Notebook 2 rather than presenting Dispatch 002 as an unrelated new lesson.
- Part 4 must not re-explain generic data leakage.
- Parts 4 and 5 must not use "external validity" as a vague label for construct, transport, capture, and integration questions that can be named more precisely.
- Part 6 must not repeat every incident before presenting the protocol.
- No post after Part 2 should rediscover that setup matters as if it were a new conclusion.
- No post may end with the same generic instruction to "keep a human in the loop."

## 10. Counterargument Map

An adversarial review should verify that the series encounters these objections at the point where they are strongest.

| Objection | Required location | Required answer |
| --- | --- | --- |
| Humans make all of these errors | Parts 1 and 2 | Yes. The general point is not uniquely machine-created misconduct; the case-specific claim concerns rapid propagation, plausible surface quality, and a system that neither enforced nor disclosed explicit constraints. |
| The adjacent example is a fan product, not a paper | Part 1 | Yes. That is the point: a serious science-shaped artifact can exist outside the institutions where research-quality debates are usually located. It is not presented as misconduct, proof of invalidity, or evidence that its creator used an LLM. |
| An anonymous example cannot be independently inspected by the reader | Part 1 | Correct. The public post may use it only as a nonessential illustration of a category already supported elsewhere. The private dossier preserves the source for editorial audit; the series' substantive validity argument comes from the author's named case. |
| There is no counterfactual for how much time the LLM saved | Parts 1 and 2 | Correct. Treat "weeks into hours" as the author's documented project estimate, not a causal productivity measurement or population claim. |
| One person's labels can still be excellent | Part 4 | Yes. Reliability is unmeasured, not disproven. Expertise and agreement answer different questions, and the series' own labels require the same scrutiny. |
| The author should have reviewed every split and constant personally | Part 2 | The author remains responsible for publication, but this is not an answer to the autonomy problem. The constraints were explicit and the reports presented them as satisfied. A system that requires manual reconstruction of every run is not autonomously running the research process. |
| Preregistration and clean splits solve the problem | Parts 3 and 4 | They protect confirmation and information boundaries but do not establish construct validity. |
| Listening is subjective | Part 4 | Report that the historical ten-case check used one outcome-aware author-listener and therefore did not estimate agreement. Preserve ambiguity, and describe neutral presentation or multiple raters as stronger future designs rather than properties of the original evidence. |
| A frozen plan can preserve a bad construct more efficiently | Parts 4 and 6 | Yes. Durability protects provenance and permissions; it does not establish meaning. A different evidence path must still challenge the construct. |
| Browser demonstrations are anecdotal | Part 5 | They diagnose product-path validity; they complement rather than replace corpus evaluation. |
| Correct identity at least once may have little product value | Part 5 | Yes. The August discovery counts demonstrate capability, while held state, latency, false switches, abstention, and publication behavior describe the rest of the product path. Viewer benefit remains a separate question. |
| An elaborate protocol can become performative paperwork | Parts 4 and 6 | It already did. The protocol matters only when it changes permissions, catches failures before propagation, and preserves sealed and final-candidate prospective outcomes without relabeling them. |
| Governance erases the speed advantage | Part 6 | Possibly. The series should measure and admit the tax rather than assert a free reliability gain. |
| A clean sealed set can still be unrepresentative | Part 6 | Correct. A sealed result supports only its defined sampling frame and population. It does not establish future-tour or catalog-wide performance without a defensible connection to that population. |
| Repeated future shows remain prospective confirmation | Part 6 | Not automatically. A show's temporal status is prospective relative to a frozen candidate, but once outcomes influence revisions or stopping, it becomes opened engineering evidence for later candidates. |
| One case study cannot prove a general method | Part 6 | Correct. The result is an operating protocol and set of failure mechanisms, not a universal causal estimate. |

## 11. Series-Level Claims And Falsifiers

### Claim A: In this project, LLM assistance rapidly propagated invalid empirical premises

**Support required:** the project's documented implementation speed, silent methodology decisions, rapid downstream propagation, and evidence that the invalidity was substantive rather than cosmetic.

**Falsifier or narrowing evidence:** the case does not establish a population-level causal effect of LLM use. If the incidents cannot be separated even locally from ordinary project haste, state only that this human-agent system produced and propagated invalid work quickly.

### Claim B: Durable boundaries improve auditability and constrain automatic action

**Support required:** concrete cases in which role restrictions, frozen plans, provenance, label blindness, or validation gates created an inspectable record, blocked an automatic action, or caught a problem before broader propagation. Existing examples include Q01 halting before audio loaded and the August canary keeping authoritative setlist and marker writes disabled; the post must distinguish those recorded constraints from broader claims about prevention.

**Falsifier or narrowing evidence:** if the rules only generate documentation after the same mistakes, the claim narrows to traceability. The architecture does not claim that these boundaries guarantee prevention or that people cannot ignore them.

### Claim C: SetScope emitted correct live song identities

**Support available for the narrow version:** the August 13 field test, in which the runtime emitted correct blind song guesses during a genuinely new live show, together with the saved trace and its documented failures.

**Current permitted version:** SetScope emitted the correct identity at least once for 10 of 12 recorded song performances during one live show. This is a product observation, not a whole-show accuracy, duration, display-delivery, viewer-benefit, or catalog-wide reliability claim.

**Support required for a stronger version:** clean complete-product and corpus evaluations across identity, latency, transitions, abstention, nonmusic, and capture integrity. Automatic-publication readiness also requires the relevant operational safety decision and outward-path record.

### Claim D: The protocol generalizes beyond this project

**Support required for a strong version:** use in other empirical projects or independent adoption and evaluation.

**Current permitted version:** the protocol exposes reusable questions and mechanisms that other practitioners can test. The series does not yet establish cross-project efficacy.

## 12. Title And Takeaway Alignment

The title/subtitle pair for each post should be testable against one sentence.

| Part | Proposition the title must carry | One-sentence takeaway |
| --- | --- | --- |
| 1 | Research-like output is cheap; validity still requires an outside judge | Ask what independently judges the output, not whether an AI touched it. |
| 2 | Setup decisions are experimental decisions | Correct downstream work cannot rescue an invalid premise. |
| 3 | Opened evidence cannot independently confirm the claim it helped shape | Data roles follow information flow within an adaptive research lineage, including human adaptation and derivatives. |
| 4 | Internal agreement can preserve a shared mistake | Check the named construct against the underlying phenomenon. |
| 5 | Product behavior belongs to the complete path | Measure what the deployed system heard and did, not only what a component can do offline. |
| 6 | Auditable autonomy is bounded and evidence-led | Freeze permissions, preserve failures, and let claim-specific evidence determine what may be said. |
| 7 | A live product is a versioned operational history | Report how the evolving tool operated using the complete August-run record, not one score or a few memorable songs. |

An adversarial agent should flag any title that promises a more dramatic result than the post earns, and any post whose actual conclusion cannot be stated in its title or subtitle without qualification.

## 13. Adversarial Reader Scenarios

The architecture should survive at least these readers:

1. **The AI optimist:** looks for selective treatment of positive and negative AI evidence.
2. **The AI skeptic:** looks for cases where ordinary human research error is rhetorically blamed on a model.
3. **The statistician:** looks for adaptive selection, denominator changes, unsupported generalization, and uncertainty omitted from claims.
4. **The measurement researcher:** looks for proxies receiving construct names without validation and for reliability being confused with validity.
5. **The music-domain expert:** looks for false precision around jam boundaries, segues, composition, and listener disagreement.
6. **The product engineer:** looks for offline metrics presented as end-to-end behavior and for observability without acceptance decisions.
7. **The source-project author:** looks for a fair, non-identifying account of stated purpose, acknowledged limitations, and which claims actually depend on hand labels.
8. **The reader who starts with Part 4:** needs enough project context to understand the failed gate system without Parts 1 through 3.
9. **The reader who only reads Part 6:** needs a usable protocol and an honest evidence status without the emotional force of the preceding narrative.
10. **The reader after a failed sealed evaluation:** should find that the series still makes sense and has not promised a successful recognizer from the beginning.
11. **The product reader:** should never be asked to mistake a viewer-facing song guesser, a field-test log, and a scientific result for the same thing.
12. **The tour reader:** should be able to tell which product version ran, whether the audio path was healthy, what viewers saw, and which denominator supports every aggregate.

## 14. Adversarial Audit Protocol

The adversarial reviewer is evaluating argument integrity, not style. It should not rewrite titles, reorder paragraphs, or reward rhetorical smoothness. It should assume that a polished draft can hide a broken dependency.

### Required inputs

Provide the reviewer with:

1. this architecture document;
2. the canonical editorial plan;
3. the Part 1 source dossier;
4. all available post drafts;
5. the evidence artifacts named by each post;
6. the current status of the sealed and prospective evaluation; and
7. for Part 7, the frozen tour manifest and scoring policy plus the current per-run version, validity, delivery, truth, and external-observation records.

Do not provide a prose summary in place of these files. The reviewer should inspect the source artifacts directly.

### Review stages

#### Stage 1: Claim extraction

Extract every substantive series-level and post-level claim. For each, identify:

- the exact wording;
- the evidence cited;
- the population and conditions;
- whether the claim is descriptive, causal, predictive, interpretive, or normative;
- whether the claim was known at the time of the event or learned later; and
- the strongest narrower claim the evidence certainly supports.

#### Stage 2: Dependency audit

For every conclusion, identify the premises it requires. Flag:

- a premise not established in the same or an earlier post;
- a premise established only by a later result;
- a post that repeats a prior conclusion without adding a new boundary;
- a transition that turns chronology into causality; and
- a conclusion that depends on the reader remembering undefined context from another post.

#### Stage 3: Evidence-role audit

Classify every project evidence item on each applicable axis: information access relative to the candidate, temporal relation, system scope, publication-action scope, and metric eligibility. External literature and domain reality checks should be named by source and design rather than forced onto those project-execution axes. Flag any unmarked change of coordinate or substitution between axes, including:

- opened engineering metrics described as generalization;
- browser anecdotes described as population accuracy;
- reproducibility described as validity;
- inter-rater absence described as label error;
- a proxy score described as the construct itself; and
- selective precision presented without coverage and abstentions.

#### Stage 4: Standalone audit

Read each post in isolation. Score whether it provides:

- the question;
- necessary project context;
- definition of its central distinction;
- concrete evidence;
- an earned takeaway;
- limitations; and
- value to a reader who never opens another post.

#### Stage 5: Throughline audit

Read the series in order and test whether the reader's model changes exactly once at each post:

1. cheap research-like production requires an outside judge;
2. setup is the experiment;
3. independence is an information history;
4. internal consistency is not construct or measurement validity;
5. component validity is not system validity;
6. bounded autonomy makes those evidence distinctions auditable and limits automatic action; and
7. a longitudinal product result must preserve versions, operating failures, clocks, and viewer-visible behavior.

Flag missing steps, collapsed boundaries, repeated revelations, and any post that does not make the next post necessary.

#### Stage 6: Ending audit

Run Part 6 against no sealed result yet, sealed pass, mixed result, and sealed fail. Then run Part 7 against a favorable, mixed, poor, and operationally compromised August-run record. Flag any earlier sentence that becomes misleading under one of those outcomes. The series passes this stage only if its methodological thesis survives every outcome and the product conclusion changes appropriately.

### Required output

The reviewer should return:

1. **Overall verdict:** `coherent`, `coherent with revisions`, or `broken`.
2. **Critical breaks:** unsupported dependencies that invalidate the progression.
3. **Post contract table:** pass or fail for standalone value, distinct claim, handoff, title alignment, and evidence role.
4. **Claim ledger:** each claim with evidence, permitted wording, and overclaim risk.
5. **Redundancy report:** material that belongs to another post or performs no new logical work.
6. **Missing counterarguments:** the strongest reasonable objection not answered where it arises.
7. **Boundary-confusion report:** places where information, meaning, and execution validity are conflated.
8. **Ending robustness:** pass, mixed, and fail scenarios.
9. **Minimum repairs:** the smallest architectural changes required before prose revision.
10. **Questions for the author:** unresolved factual or interpretive choices the reviewer must not decide silently.

### Copyable adversarial prompt

```text
You are an adversarial argument reviewer, not a writing coach. Review the attached
series architecture, editorial plan, drafts, source dossier, and named evidence
artifacts. Do not improve the prose. Do not propose prettier titles. Do not infer
that a claim is supported because it sounds reasonable or because several
artifacts agree with one another.

Your job is to determine whether the seven-post series forms one complete logical
progression while each post remains independently useful.

Treat these as distinct and non-substitutable:
1. information independence;
2. construct or meaning validity;
3. deployed execution validity;
4. the orthogonal evidence axes: information access relative to the candidate, temporal relation, system
scope, publication-action scope, and metric eligibility.
5. a user-facing song guess, a product field-test record, and a research claim about the development process.

SetScope is an ordinary fan-facing song guesser. Its displayed guesses are
product outputs, not scientific findings. Audit whether the series preserves
that distinction while still applying rigorous evaluation discipline to any
quantitative reliability or automatic-publication claim.

For every substantive claim, state the evidence, population, conditions, claim
type, and strongest narrower wording certainly supported. Flag chronology used
as causality, an opened result represented as confirmation, computational
reproducibility represented as scientific validity, lack of inter-rater evidence
represented as label error, a proxy represented as its construct, and selective
precision reported without coverage.

Read every post twice: once alone and once in series order. A standalone post
must define its question and distinction, supply enough local context, present
concrete evidence, earn one takeaway, and state the limit on that takeaway. In
series order, each post must add exactly one necessary rung:

P1: cheap research-like production requires an outside judge.
P2: setup is the experiment.
P3: independence is an information history.
P4: internal consistency is not construct or measurement validity.
P5: component validity is not complete-product validity.
P6: bounded autonomy makes those distinctions auditable, limits automatic
action, and submits claims to the evidence coordinates and gates they require.
P7: a tour retrospective preserves product versions, operating failures, clocks,
and viewer-visible behavior instead of collapsing them into one score.

Test Part 6 with no sealed result yet, a pass, a mixed result, and a fail. Test
Part 7 with a favorable, mixed, poor, and operationally compromised August-run record.
Flag any earlier promise that makes one of those honest endings impossible.

Return:
- overall verdict: coherent, coherent with revisions, or broken;
- critical logical breaks, ordered by severity;
- a per-post contract table;
- a claim and evidence ledger;
- repeated or misplaced material;
- missing counterarguments;
- conflated validity boundaries;
- standalone failures;
- ending robustness for every Part 6 and Part 7 outcome;
- the minimum architectural repairs required;
- factual or interpretive questions that must go back to the author.

Prefer a direct finding over a balanced-sounding paragraph. Do not silently fix
the series on the author's behalf.
```

## 15. Acceptance Criteria Before Full Drafting

The architecture is ready for prose work only when an adversarial review finds:

- no critical dependency break in S1 through S7;
- a distinct claim and removal justification for all seven posts;
- standalone context, takeaway, and limitation for every post;
- no unmarked change on or substitution among information, temporal, system-scope, publication-action, and metric-eligibility axes;
- no conflation of information, meaning, and execution boundaries;
- no title or subtitle that promises more than the post earns;
- no conflation of a SetScope guess, a product field-test record, and a scientific or methodological claim;
- a fair, source-grounded, and non-identifying adjacent-project example whose only argumentative job is showing a science-shaped artifact published outside a formal academic venue;
- a Part 6 ending that remains honest with no sealed result yet and under pass, mixed, or fail;
- a Part 7 architecture that remains honest under a favorable, mixed, poor, or operationally compromised August-run record; and
- explicit unresolved questions returned to the author rather than decided by an agent.

## 16. Current Open Decisions

The seven-post length is now justified as a six-post methodological arc plus a final longitudinal product retrospective. Parts 1-6 are architecturally ready for prose; Part 7 is architecturally ready but its results prose intentionally waits for the August-run record. The remaining factual or product decisions are:

1. How much of the existing user-authored *Two Notebooks Lost* draft should remain unchanged. No rewrite should occur without explicit approval.
2. What the final frozen SetScope candidate is, whether the 38-show sealed set remains clean against its complete provenance, and whether execution is authorized.
3. What target population and sampling frame the 38-show sealed result is allowed to represent; cleanliness does not by itself establish representativeness.
4. Whether Part 6 will include any broad quantitative reliability or automatic-publication claim. If not, the existing field-test record is sufficient for its narrower product story; if so, the corresponding clean evaluation or deployment record must exist first.
5. Completion of the per-run version, availability, capture, delivery, truth, and external-observation records required by the already-frozen August 13-28 western-run policy.

These open decisions do not block Parts 1-6. They bound Part 6's ending: the current version may claim that a live prototype emitted correct identities and that the development process became more auditable, but not a whole-show reliability estimate, viewer benefit, independent generalization, or successful automatic-publication path. Part 7 remains deliberately open on outcome until the August 28 cutoff and reconciliation of its complete eleven-show versioned record.

---

[Back to the editorial plan](/series/the-machine-in-the-lab/editorial-plan/)
