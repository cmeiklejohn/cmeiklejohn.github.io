#!/usr/bin/env python3
"""Generate the custom Open Graph card for The Machine in the Lab."""

from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "img" / "the-machine-in-the-lab-source.jpg"
OUT = ROOT / "og" / "the-machine-in-the-lab-editorial.png"
FONTS = ROOT / "fonts"

WIDTH = 1200
HEIGHT = 630

PAPER = (238, 232, 216)
PAPER_MUTED = (194, 188, 174)
RED = (194, 70, 52)


def font(name: str, size: int) -> ImageFont.FreeTypeFont:
    return ImageFont.truetype(str(FONTS / name), size)


TITLE = font("Literata-SemiBold.ttf", 61)
SUBTITLE = font("Literata-Regular.ttf", 25)
LABEL = font("Outfit-SemiBold.ttf", 19)
SMALL = font("Outfit-Medium.ttf", 17)


def cover(source: Image.Image) -> Image.Image:
    source_ratio = source.width / source.height
    target_ratio = WIDTH / HEIGHT

    if source_ratio > target_ratio:
        crop_width = round(source.height * target_ratio)
        left = (source.width - crop_width) // 2
        crop_box = (left, 0, left + crop_width, source.height)
    else:
        crop_height = round(source.width / target_ratio)
        top = (source.height - crop_height) // 2
        crop_box = (0, top, source.width, top + crop_height)

    return source.crop(crop_box).resize((WIDTH, HEIGHT), Image.Resampling.LANCZOS)


def darken_copy_space(image: Image.Image) -> Image.Image:
    overlay = Image.new("RGBA", image.size, (0, 0, 0, 0))
    pixels = overlay.load()
    fade_start = 430
    fade_end = 720

    for x in range(fade_end):
        if x <= fade_start:
            alpha = 82
        else:
            alpha = round(82 * (fade_end - x) / (fade_end - fade_start))
        for y in range(HEIGHT):
            pixels[x, y] = (0, 0, 0, alpha)

    return Image.alpha_composite(image.convert("RGBA"), overlay).convert("RGB")


def render() -> Image.Image:
    image = cover(Image.open(SOURCE).convert("RGB"))
    image = darken_copy_space(image)
    draw = ImageDraw.Draw(image)

    draw.text((70, 55), "A SEVEN-PART FIELD REPORT", font=LABEL, fill=PAPER_MUTED)

    draw.text((68, 128), "The Machine", font=TITLE, fill=PAPER)
    draw.text((68, 205), "in the Lab", font=TITLE, fill=PAPER)

    draw.rectangle((70, 319, 145, 324), fill=RED)
    draw.multiline_text(
        (70, 355),
        "An autonomous LLM ran the experiments.\nTwice, it presented invalid research as finished.",
        font=SUBTITLE,
        fill=PAPER,
        spacing=12,
    )

    draw.text((70, 558), "CHRISTOPHERMEIKLEJOHN.COM", font=SMALL, fill=PAPER_MUTED)

    return image


def main() -> None:
    OUT.parent.mkdir(exist_ok=True)
    render().save(OUT, optimize=True)
    print(f"Wrote {OUT.relative_to(ROOT)} ({WIDTH}x{HEIGHT})")


if __name__ == "__main__":
    main()
