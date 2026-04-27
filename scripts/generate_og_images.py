#!/usr/bin/env python3
"""Generate 1200×630 Open Graph cards for every Jekyll post.

Output: /og/{slug}.png — referenced from _includes/seo-meta.html.

Usage:
    python scripts/generate_og_images.py        # only generate missing
    python scripts/generate_og_images.py --force  # regenerate all

Run this whenever you add a post (or change a title/subtitle). Commit
the resulting /og/*.png files alongside the post.

Fonts are vendored under scripts/fonts/. Pull them with:
    bash scripts/fetch_fonts.sh
"""
import re
import sys
from datetime import date as date_cls
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont
import yaml

ROOT = Path(__file__).resolve().parents[1]
POSTS = ROOT / "_posts"
OUT = ROOT / "og"
FONTS_DIR = Path(__file__).resolve().parent / "fonts"

OUT.mkdir(exist_ok=True)

# --- Brand palette (light mode) ---------------------------------------------
BG = (235, 232, 226)      # --bg-elevated
FG = (18, 17, 20)         # --fg-heading
MUTED = (110, 108, 104)   # --fg-muted
BRAND = (61, 47, 115)     # --brand

W, H = 1200, 630
PAD_X = 90
PAD_TOP = 70
PAD_BOTTOM = 90


def parse_frontmatter(text):
    m = re.match(r"---\n(.*?)\n---\n", text, re.DOTALL)
    if not m:
        return {}
    return yaml.safe_load(m.group(1)) or {}


def slug_from_path(p):
    return re.sub(r"^\d{4}-\d{2}-\d{2}-", "", p.stem)


def date_from_path(p):
    m = re.match(r"^(\d{4})-(\d{2})-(\d{2})", p.stem)
    if m:
        return date_cls(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    return None


def load_fonts():
    return {
        "title": ImageFont.truetype(str(FONTS_DIR / "Literata-Bold.ttf"), 64),
        "subtitle": ImageFont.truetype(str(FONTS_DIR / "Literata-Regular.ttf"), 30),
        "small": ImageFont.truetype(str(FONTS_DIR / "Outfit-Regular.ttf"), 22),
        "small_bold": ImageFont.truetype(str(FONTS_DIR / "Outfit-Bold.ttf"), 22),
    }


def text_w(draw, text, font):
    bbox = draw.textbbox((0, 0), text, font=font)
    return bbox[2] - bbox[0]


def wrap(draw, text, font, max_w):
    words = (text or "").split()
    lines, line = [], []
    for word in words:
        candidate = " ".join(line + [word])
        if text_w(draw, candidate, font) <= max_w:
            line.append(word)
        else:
            if line:
                lines.append(" ".join(line))
            line = [word]
    if line:
        lines.append(" ".join(line))
    return lines


def render(title, subtitle, post_date, fonts):
    img = Image.new("RGB", (W, H), BG)
    draw = ImageDraw.Draw(img)

    # Brand vertical bar on the left edge.
    draw.rectangle((0, 0, 14, H), fill=BRAND)

    # Top-left site label.
    draw.text(
        (PAD_X, PAD_TOP),
        "CHRISTOPHERMEIKLEJOHN.COM",
        font=fonts["small_bold"],
        fill=BRAND,
    )

    # Title (large serif, dark).
    max_text_w = W - PAD_X - 60
    title_lines = wrap(draw, title, fonts["title"], max_text_w)
    title_line_h = 80

    # Subtitle (medium serif, muted) — limit to 4 lines.
    sub_lines = wrap(draw, subtitle, fonts["subtitle"], max_text_w) if subtitle else []
    sub_lines = sub_lines[:4]
    sub_line_h = 44

    title_h = len(title_lines) * title_line_h
    sub_h = (len(sub_lines) * sub_line_h + 30) if sub_lines else 0
    block_h = title_h + sub_h

    available_top = PAD_TOP + 60
    available_bottom = H - PAD_BOTTOM - 30
    available_h = available_bottom - available_top
    y = available_top + max(0, (available_h - block_h) // 2)

    for line in title_lines:
        draw.text((PAD_X, y), line, font=fonts["title"], fill=FG)
        y += title_line_h

    if sub_lines:
        y += 30
        for line in sub_lines:
            draw.text((PAD_X, y), line, font=fonts["subtitle"], fill=MUTED)
            y += sub_line_h

    # Bottom row: byline left, date right.
    byline_y = H - PAD_BOTTOM
    draw.text((PAD_X, byline_y), "Christopher Meiklejohn", font=fonts["small"], fill=MUTED)

    if post_date:
        try:
            date_text = post_date.strftime("%b %-d, %Y").upper()
        except ValueError:
            date_text = post_date.strftime("%b %d, %Y").upper()
        date_w = text_w(draw, date_text, fonts["small"])
        draw.text((W - PAD_X - date_w, byline_y), date_text, font=fonts["small"], fill=MUTED)

    return img


def main():
    force = "--force" in sys.argv
    fonts = load_fonts()
    posts = sorted(POSTS.glob("*.markdown"))

    print(f"Found {len(posts)} posts; output dir: {OUT.relative_to(ROOT)}")
    generated, skipped = 0, 0

    for p in posts:
        slug = slug_from_path(p)
        out_path = OUT / f"{slug}.png"
        if out_path.exists() and not force:
            skipped += 1
            continue

        fm = parse_frontmatter(p.read_text())
        title = (fm.get("title") or "Untitled").strip()
        subtitle = (fm.get("subtitle") or "").strip()
        post_date = date_from_path(p)

        img = render(title, subtitle, post_date, fonts)
        img.save(out_path, optimize=True)
        generated += 1
        print(f"  {slug}")

    print(f"\n{generated} generated, {skipped} skipped")
    if skipped and not force:
        print("(run with --force to regenerate existing)")


if __name__ == "__main__":
    main()
