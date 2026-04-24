#!/usr/bin/env python3
"""
Simulates what .github/workflows/daily-rebuild.yml would do on each day
of the MAS series publishing window. Run locally before pushing to
catch any date-parsing or front-matter issues.

Usage: python3 .github/workflows/test-daily-rebuild.py

Expected output: one "would trigger rebuild" line per day from 2026-04-24
through 2026-05-01, each listing the single post whose date just crossed
into the past.
"""
import os
import re
import datetime as dt
from pathlib import Path

POSTS_DIR = Path(__file__).resolve().parents[2] / "_posts"
CRON_HOUR_UTC = 12
CRON_MINUTE_UTC = 5
WINDOW_HOURS = 6

# Simulate the cron firing each day from 04-24 through 05-02 (one day past
# the last scheduled post, to confirm no extra triggers).
sim_dates = [
    dt.datetime(2026, 4, 24, CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 4, 25, CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 4, 26, CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 4, 27, CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 4, 28, CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 4, 29, CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 4, 30, CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 5, 1,  CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
    dt.datetime(2026, 5, 2,  CRON_HOUR_UTC, CRON_MINUTE_UTC, tzinfo=dt.timezone.utc),
]


def parse_post_date(path):
    """Return tz-aware datetime parsed from Jekyll front matter `date:`.

    Handles both timezone-qualified and naive date formats. Naive dates
    are assumed to be UTC (Jekyll's default).
    """
    with open(path) as f:
        head = []
        for line in f:
            head.append(line)
            if len(head) > 1 and line.strip() == "---":
                break
    fm = "".join(head)
    m = re.search(r'^date:\s+(.+)$', fm, re.MULTILINE)
    if not m:
        return None
    raw = m.group(1).strip()
    for fmt in ("%Y-%m-%d %H:%M:%S %z", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d"):
        try:
            ts = dt.datetime.strptime(raw, fmt)
            if ts.tzinfo is None:
                ts = ts.replace(tzinfo=dt.timezone.utc)
            return ts
        except ValueError:
            continue
    return None


def crossed_into_past(post_ts, now, window_hours):
    cutoff = now - dt.timedelta(hours=window_hours)
    return cutoff < post_ts <= now


def main():
    # Only simulate against the MAS series posts so output stays readable.
    # The actual workflow scans every post in _posts/, same logic applies.
    posts = sorted(POSTS_DIR.glob("*mas-series*.markdown"))
    parsed = []
    for p in posts:
        ts = parse_post_date(p)
        if ts is None:
            print(f"WARN: could not parse date in {p.name}")
            continue
        parsed.append((ts, p.name))

    print(f"Scanned {len(parsed)} MAS series posts in {POSTS_DIR}\n")

    any_trigger_seen = False
    for now in sim_dates:
        newly_past = [(ts, name) for ts, name in parsed
                      if crossed_into_past(ts, now, WINDOW_HOURS)]
        if newly_past:
            any_trigger_seen = True
            print(f"[{now.strftime('%Y-%m-%d %H:%M UTC')}] WOULD TRIGGER REBUILD:")
            for ts, name in newly_past:
                print(f"    - {name}  ({ts.strftime('%Y-%m-%d %H:%M %z')})")
        else:
            print(f"[{now.strftime('%Y-%m-%d %H:%M UTC')}] nothing to publish, skip")

    print()
    if not any_trigger_seen:
        print("WARNING: no rebuilds would ever trigger. Check post dates.")
    else:
        print("If each cron fire detects exactly one post (except the first"
              " and last days), the workflow is correctly wired.")


if __name__ == "__main__":
    main()
