---
layout: app
title: Divided Rye
permalink: /apps/divided-rye/
app_page: true
app_icon: divided-rye
description: A personal iPhone app for finding gluten-free restaurant evidence and matching delivery locations while traveling.
---

I built Divided Rye because I want to travel to Phish shows and find places I can eat when I get back to the hotel. It brings together Find Me Gluten Free reviews and the corresponding restaurant locations on DoorDash or Uber Eats.

Open it at your current location, choose a delivery service, and see the matching places. Overall ratings, safety ratings, distance, and opening status stay visible on the cards. Star a branch when you've had good luck there; explicitly closed places are hidden by default.

An optional Zabriskie login syncs your stars and preferences across devices. You can also use it without an account. If Find Me Gluten Free requires a login for the nearby search, Divided Rye can fall back to public US city listings, leaving unavailable ratings and distances blank.

## A look around

### Start where you are

The location card shows the search area. Update it when you move to another hotel or neighborhood, then choose DoorDash, Uber Eats, or both. The app matches restaurant branches, including their addresses, rather than assuming every location of a chain is interchangeable.

{% include app-shot.html src="/images/divided-rye/nearby.png" alt="Divided Rye nearby results with location, delivery selector, ratings, and distance" caption="A full screen from the running app, using a simulated location in San Francisco." %}

### Read the card

Stars show the overall Find Me Gluten Free rating; hearts show the reported safety rating when one is available. The review count gives those ratings context. A missing rating stays missing. Dedicated gluten-free places and places with gluten-free menu options have different badges.

Distance is relative to the search location. “Hours unknown” means the app could not establish opening status. Hide Closed removes places explicitly identified as closed; it doesn't turn an unknown result into an open one.

### Open the matching delivery branch

A delivery button appears when the app has a matching branch link. While it checks a place, a spinner shows that work is still in progress. Links are cached so returning to the same area doesn't repeatedly search for the same stores. Confirm your delivery address with the provider before ordering.

{% include app-shot.html src="/images/divided-rye/delivery.png" alt="Divided Rye with Uber Eats selected and a matched restaurant delivery button" caption="Uber Eats uses its own matches and cache. You can switch providers or check both." %}

### Remember what worked

Star a particular branch when you've had good luck there, then use the Starred filter to find it again. Stars and preferences save on the device without a Zabriskie account. Signing in adds backend sync; existing device settings can be imported explicitly.

You can also use the app without signing into Find Me Gluten Free. It uses the accessible listings and ratings, with a public US city-listing fallback when nearby search requires a login. That fallback can have less detail, and it leaves unavailable distances and ratings blank.

This is currently a personal iPhone build. The name is a *Divided Sky* pun.

The ratings come from community reports, and a delivery link identifies a restaurant branch. Neither guarantees that a meal is safe for someone with celiac disease. Check the restaurant's practices and confirm your delivery address before ordering.

Powered by [Zabriskie](/apps/zabriskie/). Divided Rye is independent of Find Me Gluten Free, DoorDash, and Uber Eats.
