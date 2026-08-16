#!/usr/bin/env python3
"""Generate the custom Open Graph card for The Machine in the Lab."""

from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "og" / "the-machine-in-the-lab.png"
FONTS = ROOT / "fonts"

WIDTH = 1200
HEIGHT = 630

BACKGROUND = (235, 232, 226)
PANEL = (246, 244, 239)
INK = (24, 22, 27)
MUTED = (105, 101, 101)
BRAND = (61, 47, 115)
TEAL = (42, 113, 108)
RED = (169, 67, 57)
LINE = (201, 197, 190)


def font(name: str, size: int) -> ImageFont.FreeTypeFont:
    return ImageFont.truetype(str(FONTS / name), size)


TITLE = font("Literata-SemiBold.ttf", 62)
SUBTITLE = font("Literata-Regular.ttf", 28)
LABEL = font("Outfit-SemiBold.ttf", 20)
NODE_TITLE = font("Outfit-SemiBold.ttf", 21)
NODE_DETAIL = font("Outfit-Regular.ttf", 17)
SMALL = font("Outfit-Regular.ttf", 18)
SMALL_BOLD = font("Outfit-SemiBold.ttf", 18)


def text_width(draw: ImageDraw.ImageDraw, value: str, face: ImageFont.FreeTypeFont) -> int:
    box = draw.textbbox((0, 0), value, font=face)
    return box[2] - box[0]


def centered_text(
    draw: ImageDraw.ImageDraw,
    box: tuple[int, int, int, int],
    value: str,
    face: ImageFont.FreeTypeFont,
    fill: tuple[int, int, int],
    y_offset: int = 0,
) -> None:
    x1, y1, x2, y2 = box
    bounds = draw.textbbox((0, 0), value, font=face)
    width = bounds[2] - bounds[0]
    height = bounds[3] - bounds[1]
    x = x1 + ((x2 - x1) - width) / 2
    y = y1 + ((y2 - y1) - height) / 2 - bounds[1] + y_offset
    draw.text((x, y), value, font=face, fill=fill)


def node(
    draw: ImageDraw.ImageDraw,
    box: tuple[int, int, int, int],
    title: str,
    detail: str,
) -> None:
    x1, y1, x2, y2 = box
    draw.rounded_rectangle(box, radius=8, fill=PANEL, outline=LINE, width=2)
    draw.rectangle((x1, y1, x1 + 7, y2), fill=TEAL)
    draw.text((x1 + 23, y1 + 15), title, font=NODE_TITLE, fill=INK)
    draw.text((x1 + 23, y1 + 43), detail, font=NODE_DETAIL, fill=MUTED)


def arrow(
    draw: ImageDraw.ImageDraw,
    start: tuple[int, int],
    end: tuple[int, int],
    color: tuple[int, int, int] = BRAND,
    width: int = 3,
) -> None:
    draw.line((start, end), fill=color, width=width)
    ex, ey = end
    sx, sy = start
    if abs(ex - sx) >= abs(ey - sy):
        direction = 1 if ex > sx else -1
        points = [(ex, ey), (ex - 10 * direction, ey - 6), (ex - 10 * direction, ey + 6)]
    else:
        direction = 1 if ey > sy else -1
        points = [(ex, ey), (ex - 6, ey - 10 * direction), (ex + 6, ey - 10 * direction)]
    draw.polygon(points, fill=color)


def render() -> Image.Image:
    image = Image.new("RGB", (WIDTH, HEIGHT), BACKGROUND)
    draw = ImageDraw.Draw(image)

    draw.rectangle((0, 0, 14, HEIGHT), fill=BRAND)
    draw.text((74, 58), "THE MACHINE IN THE LAB", font=LABEL, fill=BRAND)

    draw.text((74, 137), "The Machine", font=TITLE, fill=INK)
    draw.text((74, 217), "in the Lab", font=TITLE, fill=INK)

    draw.text(
        (76, 330),
        "What happens when an LLM runs\nthe next experiment?",
        font=SUBTITLE,
        fill=MUTED,
        spacing=13,
    )

    draw.line((74, 498, 621, 498), fill=LINE, width=2)
    draw.text((74, 526), "A SEVEN-PART FIELD REPORT", font=SMALL_BOLD, fill=INK)
    draw.text((74, 557), "Christopher Meiklejohn", font=SMALL, fill=MUTED)

    draw.line((668, 52, 668, 578), fill=LINE, width=2)
    draw.text((716, 58), "AUTONOMOUS RESEARCH", font=LABEL, fill=BRAND)

    propose = (716, 124, 902, 205)
    run = (944, 124, 1130, 205)
    evaluate = (944, 388, 1130, 469)
    revise = (716, 388, 902, 469)

    node(draw, propose, "PROPOSE", "choose experiment")
    node(draw, run, "RUN", "write + execute")
    node(draw, evaluate, "EVALUATE", "interpret result")
    node(draw, revise, "REVISE", "choose what is next")

    arrow(draw, (905, 164), (937, 164))
    arrow(draw, (1037, 211), (1037, 379))
    arrow(draw, (937, 428), (909, 428))

    draw.line((809, 379, 809, 236, 704, 236, 704, 164, 709, 164), fill=BRAND, width=3)
    draw.polygon([(716, 164), (706, 158), (706, 170)], fill=BRAND)

    failure = (786, 258, 1060, 331)
    draw.rounded_rectangle(failure, radius=8, fill=(246, 232, 228), outline=RED, width=2)
    centered_text(draw, (786, 265, 1060, 293), "FAILURE NOT REPORTED", SMALL_BOLD, RED)
    centered_text(draw, (786, 294, 1060, 323), "enters the next experiment", SMALL, INK)
    arrow(draw, (923, 334), (923, 378), color=RED, width=3)

    footer = "CHRISTOPHERMEIKLEJOHN.COM"
    footer_width = text_width(draw, footer, SMALL_BOLD)
    draw.text((1130 - footer_width, 548), footer, font=SMALL_BOLD, fill=BRAND)

    return image


def main() -> None:
    OUT.parent.mkdir(exist_ok=True)
    render().save(OUT, optimize=True)
    print(f"Wrote {OUT.relative_to(ROOT)} ({WIDTH}x{HEIGHT})")


if __name__ == "__main__":
    main()
