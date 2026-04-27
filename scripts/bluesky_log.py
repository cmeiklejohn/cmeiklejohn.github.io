#!/usr/bin/env python3
"""Bluesky reshare log — track which blog posts have been shared.

Usage:
    python scripts/bluesky_log.py status [--group <group>]
    python scripts/bluesky_log.py next   [--group <group>] [--limit N]
    python scripts/bluesky_log.py mark <post-url> [--bluesky-url <bsky-url>]

The log lives in _data/bluesky_log.yml. After running `mark`, commit
the file so the source of truth stays in version control.

URL drift caveat: a few posts have a frontmatter `date:` time that
crosses midnight in the local timezone, which makes Jekyll generate a
URL one day off from the filename (e.g. mas-series-01 has filename
2026-04-24 but URL /04/23/...). When in doubt, paste the actual URL
from the live site rather than constructing it from the filename.
"""
import argparse
import re
import sys
from datetime import date
from pathlib import Path

try:
    import yaml
except ImportError:
    sys.exit("This script requires PyYAML. Install with: pip install pyyaml")

ROOT = Path(__file__).resolve().parents[1]
LOG_PATH = ROOT / "_data" / "bluesky_log.yml"
POSTS_DIR = ROOT / "_posts"


def load_log():
    if not LOG_PATH.exists():
        return {}
    with open(LOG_PATH) as f:
        data = yaml.safe_load(f) or {}
    return data.get("shared") or {}


def save_log(shared):
    LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
    header = (
        "# Tracks when each blog post was last shared on Bluesky.\n"
        "#\n"
        "# Updated by:\n"
        "#   python scripts/bluesky_log.py mark <post-url> [--bluesky-url <bsky-url>]\n"
        "#\n"
        "# Queried by:\n"
        "#   python scripts/bluesky_log.py status [--group <group>]\n"
        "#   python scripts/bluesky_log.py next   [--group <group>]\n"
        "#\n"
        "# Commit this file alongside the share — it's the source of truth for\n"
        "# which evergreen posts are due for another rotation.\n\n"
    )
    body = yaml.safe_dump({"shared": shared}, default_flow_style=False, sort_keys=True)
    LOG_PATH.write_text(header + body)


def parse_frontmatter(text):
    m = re.match(r"---\n(.*?)\n---\n", text, re.DOTALL)
    if not m:
        return {}
    return yaml.safe_load(m.group(1)) or {}


def post_url_from_path(path):
    """Reproduce Jekyll's default permalink: /:categories/:year/:month/:day/:title.html

    Note: this uses the date from the filename, NOT from `date:` frontmatter,
    so timezone-drift posts (rare) may end up off by one day. Paste the
    real URL from the live site if so.
    """
    fm = parse_frontmatter(path.read_text())
    m = re.match(r"^(\d{4})-(\d{2})-(\d{2})-(.+)$", path.stem)
    if not m:
        return None
    year, month, day, slug = m.groups()
    cats = fm.get("categories", "")
    if isinstance(cats, str):
        cats = cats.split()
    cat_path = "/".join(c for c in cats if c)
    if cat_path:
        return f"/{cat_path}/{year}/{month}/{day}/{slug}.html"
    return f"/{year}/{month}/{day}/{slug}.html"


def list_posts():
    posts = []
    for p in sorted(POSTS_DIR.glob("*.markdown")):
        fm = parse_frontmatter(p.read_text())
        url = post_url_from_path(p)
        if not url:
            continue
        title = (fm.get("title") or "").strip()
        subtitle = (fm.get("subtitle") or "").strip()
        group = fm.get("group") or ""
        posts.append({
            "url": url,
            "title": title,
            "subtitle": subtitle,
            "group": group,
            "filename": p.name,
        })
    return posts


def cmd_status(args):
    shared = load_log()
    posts = list_posts()
    if args.group:
        posts = [p for p in posts if p["group"] == args.group]

    print(f"{'TITLE':<58} {'GROUP':<14} {'LAST SHARED':<14} TIMES")
    print("-" * 100)
    for p in sorted(posts, key=lambda p: p["filename"], reverse=True):
        log = shared.get(p["url"]) or {}
        last = log.get("last_shared", "—")
        times = log.get("times", 0)
        title = (p["title"] or "")[:56]
        print(f"{title:<58} {p['group']:<14} {str(last):<14} {times}")
    print()
    total = len(posts)
    shared_count = sum(1 for p in posts if shared.get(p["url"]))
    print(f"{shared_count}/{total} posts have been shared on Bluesky at least once.")


def cmd_next(args):
    shared = load_log()
    posts = list_posts()
    if args.group:
        posts = [p for p in posts if p["group"] == args.group]

    def sort_key(p):
        log = shared.get(p["url"]) or {}
        last = log.get("last_shared")
        return (1 if last else 0, str(last or ""))

    posts.sort(key=sort_key)
    limit = args.limit or 5
    label = f"in `{args.group}`" if args.group else "across all groups"
    print(f"Suggested next {limit} posts to share on Bluesky ({label}):\n")
    for i, p in enumerate(posts[:limit]):
        log = shared.get(p["url"]) or {}
        last = log.get("last_shared", "never shared")
        print(f"{i+1}. {p['title']}")
        if p["subtitle"]:
            print(f"   {p['subtitle']}")
        print(f"   url:   {p['url']}")
        print(f"   group: {p['group']}")
        print(f"   last:  {last}")
        print()


def cmd_mark(args):
    shared = load_log()
    url = args.url
    # Strip host if user pasted a full URL
    m = re.search(r"https?://[^/]+(/.+)", url)
    if m:
        url = m.group(1)
    if not url.startswith("/"):
        url = "/" + url

    today = date.today().isoformat()
    log = shared.get(url) or {}
    log["last_shared"] = today
    if "first_shared" not in log:
        log["first_shared"] = today
    log["times"] = log.get("times", 0) + 1
    if args.bluesky_url:
        log["bluesky_url"] = args.bluesky_url

    shared[url] = log
    save_log(shared)
    print(f"Marked {url} as shared on {today} (times: {log['times']})")
    if args.bluesky_url:
        print(f"Bluesky post: {args.bluesky_url}")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("status", help="Show share status for each post")
    s.add_argument("--group", help="Filter by `group:` frontmatter")
    s.set_defaults(func=cmd_status)

    n = sub.add_parser("next", help="Suggest the next post to share")
    n.add_argument("--group", help="Filter by `group:` frontmatter")
    n.add_argument("--limit", type=int, default=5)
    n.set_defaults(func=cmd_next)

    m = sub.add_parser("mark", help="Mark a post as shared today")
    m.add_argument("url", help="Post URL (relative or absolute)")
    m.add_argument("--bluesky-url", help="The Bluesky post URL, optional")
    m.set_defaults(func=cmd_mark)

    args = parser.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
