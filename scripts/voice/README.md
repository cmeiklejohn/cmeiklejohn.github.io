# Voice Corpus

This directory makes the editorial voice model inspectable and reproducible.
It is deliberately excluded from the generated Jekyll site.

## Policy

- Blog cutoff: December 31, 2025.
- Primary voice corpus: every authorial pre-2026 post.
- Guest-authored material remains on the site but is excluded from voice
  inference.
- Papers inform technical structure and claim discipline. Authorship position
  controls how much evidence a paper provides about individual surface voice.
- No extracted full paper text is stored in Git.
- 2026 AI-assisted posts can be evaluated against the model but cannot train
  it. This prevents generated prose from becoming its own authority.

## Files

- `voice-guide.md`: the canonical reusable editorial model and revision protocol.
- `corpus-report.json`: the generated corpus inventory and measurements.
- `open-paper-sources.json`: public author, publisher, and arXiv PDF sources.
- `analyze_corpus.py`: the audit and report generator.

Repository-level `AGENTS.md` and `CLAUDE.md` require Codex and Claude to read
`voice-guide.md` before drafting or revising public prose. This keeps reuse
automatic instead of depending on the author remembering to attach the guide
to every task.

## Rebuild

Use a Python environment containing `pypdf`:

```bash
python3 scripts/voice/analyze_corpus.py --download-open-papers
```

The downloads are cached under `scripts/voice/.cache/`, which is ignored by
Git. The command inventories the 39 publication-page entries, analyzes every
available full text, and recomputes the post metrics.

Four short publication records do not currently have an exact downloadable
manuscript in the source list. The checked-in report names them explicitly;
their official abstracts, related author manuscripts, or author talk
transcripts were reviewed manually. They must not be represented as exact
full-text coverage.

## Use While Drafting

The guide is a retrieval-and-revision aid, not a fine-tuned language model.
For a new article:

1. select a small set of pre-2026 examples in the same mode;
2. draft from the article's evidence and claim boundaries;
3. run the structural reader and preserve its paragraph map;
4. run the evidence reviewer and preserve its claim ledger;
5. run the readability editor against the rendered article;
6. apply the voice and anti-imitation passes in `voice-guide.md`;
7. rerun any editorial gate affected by a material revision;
8. treat author edits as evaluation evidence, not automatic new training data.

This arrangement is intentional. A fine-tune would make provenance and
corrections harder to inspect while offering little advantage over a strong
corpus profile plus mode-matched retrieval for a body of this size.
