#!/usr/bin/env python3
"""Audit the pre-AI writing corpus used for Christopher's voice guide.

This script records provenance and aggregate tendencies. It does not train a
generative model and does not retain extracted paper text in the repository.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import statistics
import urllib.request
from collections import Counter, defaultdict
from datetime import date
from html.parser import HTMLParser
from pathlib import Path

try:
    from pypdf import PdfReader
except ImportError as exc:  # pragma: no cover - environment-specific guidance
    raise SystemExit("pypdf is required to analyze papers") from exc


ROOT = Path(__file__).resolve().parents[2]
VOICE_DIR = Path(__file__).resolve().parent
DEFAULT_CACHE = VOICE_DIR / ".cache" / "papers"
WORD_RE = re.compile(r"[A-Za-z][A-Za-z'’-]*")
GUEST_POSTS = {
    "2022-06-02-extending-filibuster-to-redis.markdown": (
        "Guest-authored by Eunice Chen; retained in the site but excluded "
        "from authorial voice inference."
    )
}


def strip_front_matter(text: str) -> str:
    return re.sub(r"\A---\s*\n.*?\n---\s*\n", "", text, count=1, flags=re.S)


def prose_from_markdown(text: str) -> str:
    text = strip_front_matter(text)
    text = re.sub(r"{%\s*highlight.*?%}.*?{%\s*endhighlight\s*%}", " ", text, flags=re.S)
    text = re.sub(r"```.*?```", " ", text, flags=re.S)
    text = re.sub(r"^\s{4}.*$", " ", text, flags=re.M)
    text = re.sub(r"^\s*\[[^]]+\]:\s*\S+.*$", " ", text, flags=re.M)
    text = re.sub(r"!\[([^]]*)\]\([^)]*\)", r"\1", text)
    text = re.sub(r"\[([^]]+)\]\([^)]*\)", r"\1", text)
    text = re.sub(r"\[([^]]+)\]\[[^]]+\]", r"\1", text)
    text = re.sub(r"<[^>]+>", " ", text)
    text = re.sub(r"{%.*?%}|{{.*?}}", " ", text, flags=re.S)
    text = re.sub(r"^[#>*+-]+\s*", "", text, flags=re.M)
    text = re.sub(r"[`*_~]", "", text)
    text = re.sub(r"[ \t]+", " ", text)
    return re.sub(r"\n{3,}", "\n\n", text).strip()


def metric_block(text: str) -> dict:
    words = WORD_RE.findall(text)
    sentences = [
        len(WORD_RE.findall(part))
        for part in re.findall(r"[^.!?]+[.!?]+", text)
        if WORD_RE.search(part)
    ]
    paragraphs = [
        len(WORD_RE.findall(part))
        for part in re.split(r"\n\s*\n", text)
        if WORD_RE.search(part)
    ]
    lower_words = [word.lower() for word in words]
    contractions = [word for word in lower_words if "'" in word or "’" in word]
    return {
        "words": len(words),
        "sentences": len(sentences),
        "mean_sentence_words": round(statistics.mean(sentences), 2) if sentences else 0,
        "median_sentence_words": statistics.median(sentences) if sentences else 0,
        "paragraphs": len(paragraphs),
        "mean_paragraph_words": round(statistics.mean(paragraphs), 2) if paragraphs else 0,
        "median_paragraph_words": statistics.median(paragraphs) if paragraphs else 0,
        "question_marks": text.count("?"),
        "exclamation_marks": text.count("!"),
        "parenthetical_groups": len(re.findall(r"\([^)]{3,}\)", text)),
        "contractions": len(contractions),
        "first_person_singular": sum(word in {"i", "me", "my", "mine"} for word in lower_words),
        "first_person_plural": sum(word in {"we", "us", "our", "ours"} for word in lower_words),
    }


class PublicationParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.section = ""
        self.in_h2 = False
        self.h2_parts: list[str] = []
        self.item: dict | None = None
        self.field = "other"
        self.link: dict | None = None
        self.entries: list[dict] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        values = dict(attrs)
        classes = set((values.get("class") or "").split())
        if tag == "h2":
            self.in_h2 = True
            self.h2_parts = []
        elif tag == "li":
            self.item = {
                "section": self.section,
                "title_parts": [],
                "author_parts": [],
                "date_parts": [],
                "links": [],
            }
            self.field = "title"
        elif self.item is not None and tag == "span":
            if "author" in classes:
                self.field = "author"
            elif "date" in classes:
                self.field = "date"
            elif "pub-links" in classes:
                self.field = "links"
        if self.item is not None and tag == "a":
            self.link = {"label_parts": [], "url": values.get("href", "")}

    def handle_data(self, data: str) -> None:
        if self.in_h2:
            self.h2_parts.append(data)
        if self.item is not None and self.field in {"title", "author", "date"}:
            self.item[f"{self.field}_parts"].append(data)
        if self.link is not None:
            self.link["label_parts"].append(data)

    def handle_endtag(self, tag: str) -> None:
        if tag == "h2" and self.in_h2:
            self.section = " ".join("".join(self.h2_parts).split())
            self.in_h2 = False
        elif tag == "a" and self.link is not None and self.item is not None:
            self.item["links"].append(
                {
                    "label": " ".join("".join(self.link["label_parts"]).split()),
                    "url": self.link["url"],
                }
            )
            self.link = None
        elif tag == "span" and self.item is not None:
            self.field = "other"
        elif tag == "li" and self.item is not None:
            title = " ".join("".join(self.item["title_parts"]).split())
            authors = " ".join("".join(self.item["author_parts"]).split())
            publication = " ".join("".join(self.item["date_parts"]).split())
            if title and authors and publication:
                if authors == "Christopher S. Meiklejohn" or authors == "Christopher Meiklejohn":
                    role = "sole"
                elif authors.startswith("Christopher"):
                    role = "first"
                else:
                    role = "coauthor"
                self.entries.append(
                    {
                        "section": self.item["section"],
                        "title": title,
                        "authors": authors,
                        "publication": publication,
                        "authorship_role": role,
                        "links": self.item["links"],
                    }
                )
            self.item = None
            self.field = "other"


def parse_publications(path: Path) -> list[dict]:
    parser = PublicationParser()
    parser.feed(path.read_text())
    return parser.entries


def download_open_papers(cache: Path, sources_path: Path) -> None:
    cache.mkdir(parents=True, exist_ok=True)
    for source in json.loads(sources_path.read_text()):
        destination = cache / source["filename"]
        if destination.exists() and destination.stat().st_size > 1000:
            continue
        request = urllib.request.Request(
            source["url"], headers={"User-Agent": "Mozilla/5.0 voice-corpus-audit"}
        )
        with urllib.request.urlopen(request, timeout=90) as response:
            destination.write_bytes(response.read())
        if not destination.read_bytes().startswith(b"%PDF"):
            raise RuntimeError(f"Downloaded source is not a PDF: {source['url']}")


def analyze_pdfs(paths: list[Path]) -> dict:
    seen: dict[str, str] = {}
    documents: list[dict] = []
    for path in sorted(paths):
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        if digest in seen:
            continue
        seen[digest] = path.name
        reader = PdfReader(str(path))
        word_count = 0
        for page in reader.pages:
            word_count += len(WORD_RE.findall(page.extract_text() or ""))
        documents.append(
            {
                "filename": path.name,
                "pages": len(reader.pages),
                "words": word_count,
                "sha256": digest,
                "source": "site" if path.parent == ROOT / "publications" else "open-access-cache",
            }
        )
    return {
        "unique_full_text_pdfs": len(documents),
        "total_extracted_words": sum(item["words"] for item in documents),
        "documents": documents,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--paper-cache", type=Path, default=DEFAULT_CACHE)
    parser.add_argument("--download-open-papers", action="store_true")
    parser.add_argument("--output", type=Path, default=VOICE_DIR / "corpus-report.json")
    args = parser.parse_args()

    sources_path = VOICE_DIR / "open-paper-sources.json"
    if args.download_open_papers:
        download_open_papers(args.paper_cache, sources_path)

    post_paths = sorted(
        path
        for path in (ROOT / "_posts").iterdir()
        if re.match(r"\d{4}-", path.name) and int(path.name[:4]) < 2026
    )
    included_paths = [path for path in post_paths if path.name not in GUEST_POSTS]
    post_texts = {path.name: prose_from_markdown(path.read_text(errors="replace")) for path in included_paths}

    period_texts: dict[str, list[str]] = defaultdict(list)
    for name, text in post_texts.items():
        year = int(name[:4])
        if year <= 2015:
            period = "2013-2015"
        elif year <= 2019:
            period = "2016-2019"
        else:
            period = "2020-2023"
        period_texts[period].append(text)

    publications = parse_publications(ROOT / "publications.html")
    role_counts = Counter(entry["authorship_role"] for entry in publications)
    pdf_paths = list((ROOT / "publications").glob("*.pdf"))
    if args.paper_cache.exists():
        pdf_paths.extend(args.paper_cache.glob("*.pdf"))

    combined_posts = "\n\n".join(post_texts.values())
    report = {
        "generated_on": date.today().isoformat(),
        "policy": {
            "cutoff": "Only posts dated before 2026 are eligible for voice inference.",
            "blog_role": "Primary evidence for narrative voice.",
            "paper_role": "Evidence for technical precision, qualification, and argument structure.",
            "coauthor_rule": "Many-author papers are not treated as direct evidence of surface voice.",
        },
        "blog": {
            "discovered_posts": len(post_paths),
            "included_authorial_posts": len(included_paths),
            "excluded_posts": GUEST_POSTS,
            "aggregate": metric_block(combined_posts),
            "periods": {
                period: {"posts": len(texts), **metric_block("\n\n".join(texts))}
                for period, texts in sorted(period_texts.items())
            },
            "most_common_contractions": Counter(
                word.lower()
                for word in WORD_RE.findall(combined_posts)
                if "'" in word or "’" in word
            ).most_common(20),
        },
        "publications": {
            "publication_page_entries": len(publications),
            "authorship_roles": dict(sorted(role_counts.items())),
            "entries": publications,
            "coverage_note": (
                "Every publication-page entry was inventoried. Full text was analyzed when a site or "
                "open-access PDF was available; four short items are represented by official abstracts, "
                "related author manuscripts, or author talk transcripts instead of an exact downloadable manuscript."
            ),
            "partial_text_entries": [
                "Partisan: Enabling Real-World Protocol Evaluation",
                "On The Composability of the Riak DT Map: Expanding From Embedded To Multi-Key Structures",
                "Riak PG: distributed process groups on dynamo-style distributed storage",
                "Augmented Inverted Indexes to Track Causality in Eventually Consistent Data Stores",
            ],
            "pdf_corpus": analyze_pdfs(pdf_paths),
        },
    }

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2, ensure_ascii=True) + "\n")


if __name__ == "__main__":
    main()
