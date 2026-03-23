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

When you have celiac disease and you're on tour — following bands from city to city, crashing in hotels, eating whatever you can find — food isn't an adventure. It's a minefield.

Here's what it actually looks like: you land in a new city at 10pm after a show. You're starving. You open DoorDash and start scrolling. There are 400 restaurants. Some of them say "gluten-free options available," which means nothing. A pizza place with a GF crust that gets made on the same counter as regular pizza isn't safe. A Thai restaurant that says "we can make it without soy sauce" doesn't know that their oyster sauce has wheat in it. You can't tell from a DoorDash listing whether a restaurant is actually safe for someone with celiac or just checking a marketing box.

So you switch to Find Me Gluten Free — a community site where people with celiac actually review restaurants and report whether they got sick. Great data. Real safety information from people who understand cross-contamination, dedicated fryers, and separate prep areas. But FMGF doesn't do delivery. It doesn't know whether that restaurant with the 4.8 safety rating is available on DoorDash at your hotel right now.

You end up with two tabs open, manually cross-referencing. Copy a restaurant name from FMGF, paste it into DoorDash, see if it shows up, check the delivery area, go back, try the next one. It's 11pm. You're exhausted. You give up and eat a protein bar from your bag.

I got tired of the protein bar.

## What We Built

The Itinerant Glutard is a tool that connects the two systems nobody connected before. You enter a city, a state, and your delivery address. It scrapes Find Me Gluten Free for every reviewed restaurant in that city, then checks each one against DoorDash to see if it can deliver to where you are right now. The results come back sorted by a safety score — a 0-to-100 composite that weights the restaurant's GF level, its FMGF star rating, review count, and specific safety signals like dedicated fryers and separate kitchens.

The name is what it sounds like. Itinerant: traveling from place to place. Glutard: what celiacs sometimes call ourselves. An itinerant glutard is someone with celiac disease who's on the road and trying to eat.

There are two modes. Full Search takes your address and does the whole pipeline — FMGF scrape, DoorDash availability check for each restaurant, merged results with direct ordering links and delivery time estimates. It takes a minute or two because it's driving a headless browser through DoorDash for every restaurant. Quick Browse skips the DoorDash check entirely and just shows you the FMGF safety data for a city. Useful when you're planning ahead and want to see what's out there before you commit to an address.

## The Safety Score

This is the part I care about most. Not every gluten-free restaurant is equally safe, and the difference matters when you're the one who gets sick.

The score runs from 0 to 100, broken into five tiers:

- **80–100**: Celiac Safe. Dedicated gluten-free facility or overwhelming positive evidence from the celiac community.
- **60–79**: Likely Safe. Strong GF menu with good community feedback.
- **40–59**: Use Caution. Has GF options but limited safety data.
- **20–39**: Higher Risk. Minimal celiac-specific information.
- **0–19**: Unknown. No data. You're on your own.

The calculation is weighted. GF level counts for 40% — a dedicated gluten-free restaurant scores higher than one that just has "gluten-free options." FMGF's star rating is 30%. Review count is 15%, because a restaurant with 200 reviews and a 4.5 rating is a more reliable signal than one with 3 reviews and a 5.0. The remaining 15% comes from safety signals extracted from the listings: separate fryer, dedicated kitchen, celiac-safe rating, knowledgeable staff. A negative report — someone reported getting sick — drops the score.

When you're in a new city and you don't know anything, the score gives you a starting point. Green means order with confidence. Yellow means read the reviews first. Red means maybe stick with the protein bar.

## The Architecture

The backend is Express and Puppeteer. Both scrapers need a headless browser because FMGF and DoorDash render their content with JavaScript — you can't just fetch the HTML and parse it. The FMGF scraper navigates to the city page, scrolls to trigger lazy loading, then uses Cheerio to extract restaurant data from the rendered DOM. The DoorDash scraper is more involved: it has to set a delivery address (which means finding the input, typing the address, waiting for the autocomplete dropdown, selecting the first suggestion), then search for each restaurant by name with fuzzy matching.

The frontend is React and Vite. Simple by design — a search form, a list of restaurant cards with safety badges, filter buttons for "all," "on DoorDash," and "safe (60+)." Each card shows the restaurant name, cuisine, FMGF rating, GF level, safety signals, and a DoorDash order button with delivery time if it's available.

The whole thing is held together with web scraping, which means it's inherently fragile. If DoorDash changes their address input selector or FMGF redesigns their restaurant cards, the scrapers break. The code has fallback selectors and multiple strategies for finding elements, but this isn't an API integration — it's a browser pretending to be a person. That's the tradeoff. Neither service offers a public API for this data, so scraping is what you get.

## Building It With Claude

I built this with Claude Code, and it's a good example of the pattern I keep coming back to: I know the problem intimately, and Claude knows how to build the solution.

I've been the person in the hotel room cross-referencing tabs. I know which safety signals matter and which ones are noise. I know that "gluten-free options" on a DoorDash listing is almost meaningless but "dedicated gluten-free kitchen" on FMGF is a strong signal. I know that review count matters because a single enthusiastic reviewer doesn't tell you whether the restaurant is consistently safe. That domain knowledge — the lived experience of eating with celiac disease — is what shaped the scoring formula, the tier thresholds, the safety signals we extract, and the decision to put FMGF data first and DoorDash second.

Claude handled the scraping logic, the DOM navigation, the fallback selectors, the fuzzy name matching, the React components, the scoring math. The Puppeteer automation for DoorDash alone — finding the address input across different page states, handling the autocomplete dropdown, searching for restaurants and matching them by name — is the kind of tedious, selector-heavy code that would have taken me days of trial and error. Claude wrote it, I tested it, we iterated when DoorDash's UI didn't behave the way we expected.

## The Name

Itinerant Glutard. It's self-deprecating and specific. If you know, you know. If you don't, it's probably not for you, and that's fine. The best tools are the ones built by people who have the problem they're solving. I have celiac disease. I travel to see live music. I got tired of going hungry.

Now I don't.
