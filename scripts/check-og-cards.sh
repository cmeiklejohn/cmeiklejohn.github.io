#!/usr/bin/env bash
# Every post must have the Open Graph card its own meta tags point at.
#
# _includes/seo-meta.html sets og:image to /og/{slug}.png for any post without
# an explicit `image:` in front matter. When that PNG is missing the link
# preview renders bare everywhere, and LinkedIn caches the failure against the
# post's og:url in a way Post Inspector cannot clear afterwards. Renaming a
# post is the usual way this happens: the slug moves, the card does not.
#
# Regenerate cards with: python3 scripts/generate_og_images.py
set -uo pipefail
cd "$(dirname "$0")/.."

missing=0
for post in _posts/*; do
  [ -f "$post" ] || continue

  # front matter is everything between the first two --- lines
  fm=$(awk 'NR==1 && /^---/ {f=1; next} f && /^---/ {exit} f' "$post")

  # an explicit image: wins, so no generated card is required
  if grep -qE '^image:[[:space:]]*[^[:space:]]' <<<"$fm"; then
    continue
  fi

  # Jekyll's page.slug: front-matter slug: if set, else filename minus date
  slug=$(grep -E '^slug:[[:space:]]*[^[:space:]]' <<<"$fm" | head -1 |
         sed -E 's/^slug:[[:space:]]*//; s/^"(.*)"$/\1/; s/^'"'"'(.*)'"'"'$/\1/')
  if [ -z "$slug" ]; then
    stem=$(basename "$post"); stem="${stem%.*}"
    slug=$(sed -E 's/^[0-9]{4}-[0-9]{2}-[0-9]{2}-//' <<<"$stem")
  fi

  if [ ! -s "og/${slug}.png" ]; then
    echo "MISSING og/${slug}.png   <- $post"
    missing=$((missing + 1))
  fi
done

if [ "$missing" -gt 0 ]; then
  echo
  echo "$missing post(s) have no Open Graph card; their link previews will render bare."
  echo "Fix: python3 scripts/generate_og_images.py && git add og/"
  exit 1
fi

echo "OK: every post has an Open Graph card."
