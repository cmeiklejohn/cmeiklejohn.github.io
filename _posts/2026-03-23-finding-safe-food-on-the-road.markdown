---
layout: post
title:  "Finding Safe Food on the Road"
date:   2026-03-23 00:00:00 -0000
group: ai
categories: ai development claude
---

> *"What a long strange trip it's been"*
> — Grateful Dead, "Truckin'"

## The Problem Nobody Talks About

When I was doing my PhD in Europe — splitting time between Belgium, Portugal, and Paris — I got diagnosed with celiac disease. I'd gone to Europe partly because of the food. Paris. I wanted to teach there, live there, eat there. Then I found out I couldn't eat bread, and in France that's not a dietary restriction — it's an existential one.

It got bad enough that food became one of the reasons I left. Not the only reason, but a real one. I couldn't navigate restaurants in languages I spoke fluently because the cross-contamination risks were invisible and the cultural understanding of celiac was years behind the US. I remember thinking: my traveling days are over. I have this disease, and it means I stay home, I cook for myself, and I stop pretending I can live the life I wanted.

That was wrong. But it took years — and two specific tools — to prove it.

Find Me Gluten Free gave me a community of people who'd already eaten at every restaurant I was considering and reported whether it was safe. DoorDash gave me delivery to wherever I was staying, so I didn't have to walk into a restaurant and try to explain celiac disease to a kitchen that had never heard of it. Between the two of them, I've eaten safely in hundreds of cities. Great food, not just survival food. The infrastructure existed. It just wasn't connected.

When you have celiac disease and you're on tour — following bands from city to city, crashing in hotels — food isn't an adventure. It's a minefield. But it's a minefield I've learned to navigate.

Here's what it actually looks like: you fly into a new city the night before a show. You check into the hotel, drop your bags, and immediately start thinking about food — not just tonight, but tomorrow before the venue, and maybe the day after if you're staying for a second night. You need to figure out what's safe in a city you've never eaten in before.

You open DoorDash and start scrolling. There are 400 restaurants. Some of them say "gluten-free options available," which means nothing. A pizza place with a GF crust that gets made on the same counter as regular pizza isn't safe. A Thai restaurant that says "we can make it without soy sauce" doesn't know that their oyster sauce has wheat in it. You can't tell from a DoorDash listing whether a restaurant is actually safe for someone with celiac or just checking a marketing box.

So you switch to Find Me Gluten Free — a community site where people with celiac actually review restaurants and report whether they got sick. Great data. Real safety information from people who understand cross-contamination, dedicated fryers, and separate prep areas. But FMGF doesn't do delivery. It doesn't know whether that restaurant with the 4.8 safety rating is available on DoorDash at your hotel right now.

You end up with two tabs open, manually cross-referencing. Copy a restaurant name from FMGF, paste it into DoorDash, see if it shows up, check the delivery area, go back, try the next one. You're doing this the night you land, trying to line up safe options for the next two days so you're not scrambling between soundcheck and doors. It's 11pm. You're exhausted. You give up and eat a protein bar from your bag.

I got tired of the protein bar.

## What We Built

The Itinerant Glutard is a tool that connects the two systems nobody connected before. You enter a city, a state, and your delivery address. It scrapes Find Me Gluten Free for every reviewed restaurant in that city, then checks each one against DoorDash to see if it can deliver to where you are right now. The results come back sorted by a safety score — a 0-to-100 composite that weights the restaurant's GF level, its FMGF star rating, review count, and specific safety signals like dedicated fryers and separate kitchens.

The name is what it sounds like. Itinerant: traveling from place to place. Glutard: affectionate self-deprecation from the celiac community — the kind of word you earn after your third accidental glutening at a restaurant that swore they understood. An itinerant glutard is someone with celiac disease who's on the road and trying to eat.

There are two modes. Full Search takes your address and does the whole pipeline — FMGF scrape, DoorDash availability check for each restaurant, merged results with direct ordering links and delivery time estimates. It takes a minute or two because it's driving a headless browser through DoorDash for every restaurant. Quick Browse skips the DoorDash check entirely and just shows you the FMGF safety data for a city. That's the one you use before you even book the hotel — scope out what's safe in Portland or Denver or Philly so you know what you're walking into.

## The Safety Score

This is the part I care about most. The safety score is computed entirely from Find Me Gluten Free data — it's a composite of the information that FMGF's celiac community has already gathered through years of reviews, safety ratings, and incident reports. We're not inventing safety judgments; we're synthesizing what the community already knows into a single number you can act on quickly.

The score runs from 0 to 100, broken into five tiers:

- **80–100**: Celiac Safe. Dedicated gluten-free facility or overwhelming positive evidence from the celiac community.
- **60–79**: Likely Safe. Strong GF menu with good community feedback.
- **40–59**: Use Caution. Has GF options but limited safety data.
- **20–39**: Higher Risk. Minimal celiac-specific information.
- **0–19**: Unknown. No data. You're on your own.

The calculation is weighted, and every input comes from FMGF. GF level counts for 40% — a dedicated gluten-free restaurant scores higher than one that just has "gluten-free options." FMGF's star rating is 30%. Review count is 15%, because a restaurant with 200 reviews and a 4.5 rating is a more reliable signal than one with 3 reviews and a 5.0. The remaining 15% comes from safety signals extracted from the FMGF listings: separate fryer, dedicated kitchen, celiac-safe rating, knowledgeable staff. A negative report — someone in the community reported getting sick — drops the score.

When you're in a new city and you don't know anything, the score gives you a starting point. Green means order with confidence. Yellow means read the reviews first. Red means maybe stick with the protein bar.

## The Architecture

The backend is Express and Puppeteer. Both scrapers need a headless browser because FMGF and DoorDash render their content with JavaScript — you can't just fetch the HTML and parse it. The FMGF scraper navigates to the city page, scrolls to trigger lazy loading, then uses Cheerio to extract restaurant data from the rendered DOM. The DoorDash scraper is more involved: it has to set a delivery address (which means finding the input, typing the address, waiting for the autocomplete dropdown, selecting the first suggestion), then search for each restaurant by name with fuzzy matching.

The frontend is React and Vite. Simple by design — a search form, a list of restaurant cards with safety badges, filter buttons for "all," "on DoorDash," and "safe (60+)." Each card shows the restaurant name, cuisine, FMGF rating, GF level, safety signals, and a DoorDash order button with delivery time if it's available.

The whole thing is held together with web scraping, which means it's inherently fragile. If DoorDash changes their address input selector or FMGF redesigns their restaurant cards, the scrapers break. The code has fallback selectors and multiple strategies for finding elements, but this isn't an API integration — it's a browser pretending to be a person. That's the tradeoff. Neither service offers a public API for this data, so scraping is what you get.

## Building It With Claude

I was already doing all of this manually. Every trip, the same ritual: open FMGF, find the restaurants in the city, open DoorDash, search for each one by name, check if it delivers to the hotel, keep a mental list of the ones that work. It took 30–45 minutes on a good night. On a bad night — a city with a lot of FMGF listings or a DoorDash that kept showing me sponsored results instead of the restaurant I searched for — I'd give up halfway through.

The process worked. It was just slow, tedious, and manual. I knew exactly what I was doing at every step. I just couldn't build the tool to automate it because the work involved — scraping two JavaScript-heavy sites with a headless browser, navigating DoorDash's address input flow, fuzzy-matching restaurant names across platforms — was the kind of grinding infrastructure code that would have taken me weeks of evenings to get right.

Claude made it possible to build the thing I was already doing by hand. I described the manual workflow — go to FMGF, get the restaurants, check each one on DoorDash at this address — and Claude wrote the Puppeteer automation, the DOM navigation, the fallback selectors for when DoorDash's UI didn't behave as expected, the fuzzy name matching, the React frontend, all of it. The domain knowledge was mine: which safety signals matter, how to weight them, why review count is a confidence measure, why FMGF data comes first. But the scraping infrastructure that turned a 45-minute manual process into a two-minute automated one — that's what Claude made feasible for a solo developer building something on evenings and weekends.

## The Name

Itinerant Glutard. "Glutard" is celiac community slang — the kind of self-deprecating shorthand people use when they've spent enough years explaining cross-contamination to waiters and reading ingredient labels on soy sauce. It's an in-group term, affectionate in the way that only people who share the condition tend to use it. Itinerant because I'm on the road.

There was a version of me in a tiny apartment in Belgium who believed this disease meant staying put. That the world had shrunk to the places I could cook for myself. That touring — the thing I wanted most — was something other people got to do.

I was wrong. The community data existed. The delivery infrastructure existed. I just needed to connect them. I've eaten safely at hundreds of places in dozens of cities since then, and the food has been genuinely good — not sad compromises, not protein bars, not going hungry. The Itinerant Glutard is the tool that makes that process faster, but the real thing that makes it possible is that the celiac community built FMGF and DoorDash built delivery, and between the two of them, the road opened back up.

I have celiac disease. I travel to see live music. I got tired of going hungry.

Now I don't.
