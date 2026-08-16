# Claude Writing Guidance

For any public-facing prose in this repository, read
`scripts/voice/voice-guide.md` before drafting or revising. It is the canonical
reusable model of Christopher Meiklejohn's voice and is shared with Codex.

- Treat Christopher's authorial pre-2026 blog posts as the primary voice corpus.
- Use sole-authored and first-authored papers for technical precision,
  qualification, and argument structure. Do not infer individual surface voice
  from papers with several coauthors.
- Do not use posts from 2026 onward to train or redefine the voice model. They
  may contain AI-assisted prose and are evaluation material only.
- Weight recent pre-2026 long-form posts more heavily than announcements,
  reading notes, or short product updates.
- Preserve the distinction between voice and truth. Verify citations, claim
  boundaries, provenance, numbers, and uncertainty independently.
- Before any public post is called prose-ready, run the three independent
  editorial gates in `scripts/voice/voice-guide.md`: structural reader,
  evidence reviewer, and continuous-read readability editor. A generic hostile
  review or prose audit does not substitute for these three passes.
- Do not copy distinctive passages from earlier posts. Reproduce habits of
  reasoning and cadence, not wording.
- Do not rewrite an existing user-authored draft without explicit approval.
- Treat repeated author feedback as evidence for improving the guide; do not
  change it based on one isolated wording preference.

The supporting corpus inventory and reproducible analysis live in
`scripts/voice/`. `AGENTS.md` contains the equivalent repository instruction
for Codex; both tools must use the same canonical guide.
