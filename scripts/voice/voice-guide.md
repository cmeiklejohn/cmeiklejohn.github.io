# Christopher Meiklejohn Voice Guide

> **Canonical reusable voice model for this repository.** Every agent drafting
> or revising public prose must read this file first. The model is derived from
> the pre-2026 authorial corpus described below; generated posts from 2026 onward
> are evaluation material, not training material. Update this guide only when
> repeated author feedback or a fresh corpus audit supports the change.

This is a working editorial model, not a claim that style can be reduced to a
list of mannerisms. It exists to keep AI-assisted writing attached to
Christopher's pre-AI body of work instead of allowing one generated draft to
become the style reference for the next.

## Corpus Authority

The primary corpus is every authorial blog post dated before 2026. The current
audit finds 92 pre-2026 posts and includes 91; the June 2, 2022 Filibuster post
is excluded from voice inference because the post identifies Eunice Chen as
its author.

The secondary corpus is the complete publications inventory at
`publications.html`, together with the PDFs available in the site repository
and open-access author or publisher copies. Papers have a different role:

- Sole-authored and first-authored papers are strong evidence for how
  Christopher defines a problem, qualifies a claim, develops an example, and
  separates a result from its implications.
- Papers with Christopher in the middle of a long author list are evidence of
  technical interests and scholarly conventions, not reliable evidence of his
  individual surface voice.
- The dissertation is especially useful for research structure and claim
  discipline, but it is not the target cadence for a blog post.

Within the blog corpus, recent pre-2026 long-form writing receives the most
weight. Early tutorials, conference announcements, living reading lists, and
short 2023 product demonstrations are still analyzed, but they do not dictate
the form of a contemporary essay.

The machine-readable inventory and aggregate measurements are in
`corpus-report.json`. Public PDFs are enumerated in
`open-paper-sources.json`; extracted paper text is not retained in Git.

## The Voice In One Paragraph

Start with the thing that happened or the distinction that matters. Establish
why it is a problem in plain language, then earn abstraction through a concrete
system, bug, experiment, or personal experience. Move by asking the next real
question and answering it with mechanics. Use first person when it establishes
what was observed, built, misunderstood, or changed. Be willing to say that a
result is confusing, that an explanation is incomplete, or that a choice did
not work. Let one short sentence carry a turn when it deserves the space. End
with the unresolved implication, the next experiment, or a question whose
answer the body has made newly difficult.

## Structural Habits

### Open On Evidence

Characteristic openings enter through an event, a claim, or a sharp
distinction:

- a bug encountered over the weekend;
- the worst professional moment;
- a concrete gap between research software and usable software;
- two kinds of resilience testing that sound similar but are not;
- a system that behaves counterintuitively under failure.

Do not begin with a generic history of a field when a real incident can carry
the opening. Background arrives after the reader knows why it matters.

### Make The Distinction Early

Many posts turn on a precise distinction: testing for resilience versus
testing resilience behavior, deterministic programming versus reliable
execution, an implementation that passes checks versus a result that matches
reality. State the distinction directly, then test it against an example.

The device is powerful because it clarifies the argument. Do not repeat a
synthetic "not X but Y" construction in every section.

### Move Through Questions And Mechanics

Questions are working joints in the prose, not decoration. They usually appear
where a reader would genuinely ask what happens next: What fails? Why does the
obvious test teach us nothing? What happens under a partition? What would make
the result trustworthy?

Answer with system behavior, an execution trace, numbered stages, or a
specific observation. Avoid answering a technical question with another layer
of rhetoric.

### Build From Example To General Claim

The recurring argument shape is:

1. establish the concrete situation;
2. expose the surprising behavior or contradiction;
3. explain the mechanism;
4. state the narrower general lesson;
5. identify what remains unresolved.

This matters for the research series. The audio incidents should produce the
methodological claim. The methodological claim should not be announced first
and followed by incidents that merely decorate it.

### End With Forward Pressure

Endings commonly invite the next question, name the remaining work, or leave
the reader with the consequence of the argument. A brief summary is fine when
the post is a tutorial, but the essays should not finish with a generic recap
or an inspirational slogan.

## Sentence And Paragraph Rhythm

The aggregate blog corpus is conversational but not terse. After generated
HTML and code samples are removed, the current audit contains 82,266 words,
with a median sentence of 17 words, a mean sentence of 19.33 words, and a
median prose paragraph of 29 words. Those are diagnostics, not quotas.

- Mix compact turns with longer explanatory sentences.
- Use one-sentence paragraphs for actual pivots, not as a constant dramatic
  effect.
- Contractions are normal and frequent. A draft that expands every "it's,"
  "I've," and "don't" will sound falsely formal.
- Parenthetical qualifications and asides are part of the voice. They should
  reveal uncertainty, history, or a useful side observation, not perform
  cleverness.
- Lists appear when the system genuinely has stages, cases, or consequences.
  Do not force a list simply to make prose scan.
- Technical nouns are allowed to carry precision. Do not replace an exact term
  with a loose metaphor merely to make the passage sound literary.

## Stance And Personality

### First Person Is Evidence

"I" and "we" are used often, but usually to locate responsibility or
observation: what I built, what we assumed, what I did not notice, what the
system returned. First person should make provenance clearer. It should not
turn the post into personal branding.

### Confidence Is Earned Locally

The writing can be blunt about a failure and tentative about its explanation
in the same paragraph. Preserve that asymmetry. Say what happened plainly;
qualify why it happened until the evidence is sufficient.

Useful moves include:

- "I do not know" when the detail is not necessary to the claim;
- "perhaps" when proposing an interpretation;
- "the result establishes X, but not Y" when narrowing a conclusion;
- correcting the first explanation when later evidence contradicts it.

### Humor Comes From The Situation

The humor is dry, occasional, and usually embedded in a technical aside or an
admission. It works because the surrounding explanation is serious. Do not add
jokes on a schedule, manufacture self-deprecation, or turn failures into cute
characters.

### Criticism Includes The Author

Strong criticism does not float above the events. The author states his own
role in defining the constraints, deciding what to publish, or building the
system. That accountability is central to the research series and should
remain visible even when the agent made the immediate error.

Responsibility for publishing a claim is not the same as a requirement to
manually reconstruct every operation an autonomous system reports as complete.
When Christopher supplied an explicit constraint and the system presented the
work as satisfying it, do not rewrite the incident as a lesson that he should
have personally rechecked every split, constant, file, or generated artifact.
Name the actual system failure: the constraint was neither enforced nor its
violation disclosed. If routine manual reperformance is required before any
result can be used, the system is not autonomously running the research process.

## Academic Habits Worth Carrying Into Essays

The papers repeatedly begin from an operational problem, define the relevant
model, state what the proposed system does, and distinguish guarantees from
evaluation. Carry these habits into public writing:

- define a loaded term before measuring it;
- separate a model, implementation, evaluation, and interpretation;
- name the conditions under which a claim holds;
- state contributions or stages explicitly when there are several;
- use an industrial or system example to test whether an abstraction matters;
- preserve negative results and limitations;
- distinguish current evidence from future work.

Do not import the paper register wholesale. Blog prose should not accumulate
abstract nouns, passive constructions, citation clusters, or a formal
"contributions" section unless the material genuinely needs them.

## Modes

### Research Essay

Use an incident-led opening, a clearly stated methodological distinction,
mechanistic reconstruction, and an ending that exposes the next research
problem. This is the mode for *The Machine in the Lab*.

### Technical Walkthrough

State the task, show the system or code, explain surprising behavior, and
finish with the practical implication or next step. Detailed examples may
dominate; personality stays in the transitions and asides.

### Personal Essay

Anchor chronology in specific moments. Be candid without smoothing the event
into a lesson too early. Reflection comes after the reader has seen what
happened.

### Short Announcement

Lead with the release or result, show the concrete capability, and stop. Do not
use the compressed announcement cadence as the template for a long essay.

## Anti-Model

A draft is drifting away from the corpus when it does several of these:

- opens with "In an era" or a broad survey paragraph that could introduce any
  topic;
- states the thesis repeatedly before showing the event that produced it;
- uses polished parallelism in every paragraph;
- stacks slogans, metaphors, or three-item lists where mechanics are needed;
- treats "however," "therefore," and "crucially" as a substitute for logical
  movement;
- removes contractions and uncertainty until the prose sounds institutional;
- adds fake intimacy, manufactured jokes, or sentimental closure;
- overuses one-sentence paragraphs to simulate urgency;
- turns every nuance into a symmetrical "not this, but that" declaration;
- concludes that a process "offers a roadmap" without naming the actual next
  decision or experiment;
- sounds more certain after the voice pass than the evidence permits.

Do not preserve historical typos, punctuation accidents, or formatting quirks
as style. The target is the author's reasoning and cadence, not a forensic
reproduction of copy-editing errors.

## Drafting And Revision Protocol

1. **Evidence draft.** Write from the source dossier, experiment record, and
   claim boundaries. Do not optimize for voice yet.
2. **Structural reader.** Read for order before editing sentences. Write down
   the job of every paragraph, the context it requires, and the transition that
   earns its place. Flag material introduced before the reader can understand
   why it matters, sections doing several unrelated jobs, and paragraphs that
   could move or disappear without changing the argument.
3. **Voice pass.** Use this guide and a small, mode-matched set of pre-2026
   exemplars. Adjust cadence, questions, first-person provenance, and asides.
4. **Anti-imitation pass.** Search for distinctive phrases copied from source
   posts and rewrite them. The new post should sound authored by the same
   person without echoing an old paragraph.
5. **Evidence reviewer.** Build a claim ledger for every number, date,
   chronology statement, causal statement, contemporary fact, and description
   of a result. Classify each as supported, explicitly bounded, first-person
   recollection, or unsupported. Recheck the cited source or experiment record
   after every material prose revision; a source that is real but does not
   support the sentence is an evidence failure.
6. **Readability editor.** Read the rendered article continuously from the
   beginning, without using the outline as a substitute for the reading
   experience. Flag repetition, tangents, delayed context, abrupt transitions,
   qualification overload, jargon, overlong paragraphs, and sentences that are
   too balanced or polished to say naturally. Preserve technical density where
   the argument requires it.
7. **Author review.** Treat Christopher's edits as new evidence about the
   model. Update this guide only from repeated preferences, not from one local
   wording change.

### Required Three-Pass Editorial Gate

The structural reader, evidence reviewer, and readability editor are three
independent gates. A general "hostile review," prose audit, or source check does
not stand in for all three.

For each pass, preserve a concrete result:

- **Structure:** a paragraph map and an explicit move, merge, split, or keep
  decision for every flagged section.
- **Evidence:** a claim ledger with the source and boundary for every flagged
  claim, plus a list of corrections made.
- **Readability:** notes from a continuous rendered read and the edits made for
  flow, cadence, repetition, and transitions.

Run the passes in that order on the current draft. If one pass causes a material
revision, rerun every later pass it can affect. A post is not prose-ready until
all three gates have passed after the final material revision. Apply this gate
to every post in *The Machine in the Lab*, including revisions to already
published parts and the eventual tour retrospective.

## Series-Specific Check

Before a *Machine in the Lab* post is considered ready for author review:

- the required structural, evidence, and readability gates have all passed on
  the current revision;
- the opening is a documented event, finding, or institutional action;
- the article reaches the audio-research story through evidence rather than a
  generic AI transition;
- responsibility for publication remains visible without turning autonomy into
  a requirement that the author manually reperform every reported check;
- every number is attached to its population and methodological boundary;
- later knowledge is not placed into the mouth of the earlier narrator;
- the ending creates a real reason to read the next part;
- a reader can remove the AI references and still recognize the prose as
  Christopher's way of explaining a technical failure.
