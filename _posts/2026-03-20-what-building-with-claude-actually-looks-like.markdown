---
layout: post
title:  "What Building With Claude Actually Looks Like"
date:   2026-03-20 00:00:00 -0000
group: zabriskie
categories: zabriskie development
---

> *"Sometimes a notion gets a-hold of you, carries you away"*
> — Grateful Dead, "Althea"

## The Saturday

On March 7th, I sat down at 11am and started building. By 2am I had shipped a Relisten integration with inline audio players for archival recordings, a Quick Post feature for sharing past shows, Phantasy Tour as a live setlist source, Sign in with Apple, a badges and achievements system, security hardening across every API route, a complete CI pipeline with GitHub Actions, and something called Goose Mode — a tour companion dashboard with a wandering desktop goose that follows your cursor around the screen.

That was one day. 124 commits.

I kept going. By the end of the week, the total was 144 commits across 7 days. Relisten integration. Live show experience. Six new bands on the platform. A bug forum. Backend test coverage from 43.9% to 70.4%. An admin analytics dashboard. Apple App Review fixes. Scrapers for four different setlist sources. Archive.org as a recording provider. Mobile layout fixes for three different iOS edge cases.

I'm building Zabriskie by myself, and I'm building it with Claude. This is what that actually looks like.

## The Cost of Trying Something

Here's the thing nobody tells you about building with an AI collaborator: the most important change isn't speed. It's what happens to your relationship with bad ideas.

Goose Mode started at 9pm on a Saturday night as a tour-focused homepage with a literal wandering goose — a transparent animated sprite that wanders around the page while you browse upcoming shows. It was fun. It was also wrong. The layout didn't work. The information hierarchy was off. The goose was distracting in a way that stopped being charming after about thirty seconds.

Old me would have agonized. I'd spend an hour in Figma trying to figure out the right layout before writing a line of code. I'd poll people. I'd sit with it for a few days. The cost of being wrong was high enough that I'd optimize for not being wrong.

Instead, at 11:25pm, I told Claude to redesign the whole thing as a tour companion dashboard. By 3:37am it had been redesigned again — left-aligned with grouped tour timelines. By 4:08am, another redesign — card-per-tour visual timelines. By 4:42am, another — live countdown, attendee avatars, couch tour cards. By 5:41am I'd added a flip-clock countdown. By 6:17am there were interactive tour maps embedded in each card.

Six versions in nine hours. Each one a real, working implementation I could tap through on my phone. Not mockups. Not wireframes. Running code. The version that shipped was the fifth attempt, and I only knew it was right because I'd seen the four that weren't.

When the cost of trying something drops to near zero, you stop designing in your head and start designing in reality. That changes everything.

## A Pigeons Show, a Broken Chat, and a Deploy at 11pm

On the night of March 7th, Pigeons Playing Ping Pong was playing a show. People were using Zabriskie's live show features — Live Chomping, the setlist tracker, the "tonight" banner. I was watching the show on the couch and also watching my app.

The chat input was getting cut off on Chrome mobile. I could see it happening in real time because I was using it. At 10:33pm I fixed the layout. At 10:39pm I realized live show posts weren't appearing in the main feed while a show was active — a bug nobody would have found in testing because it only manifested when a real show was actually live. Fixed it. Deployed. At 10:45pm I added Phantasy Tour as a live setlist source because the existing sources weren't picking up the setlist fast enough. At 11:20pm I fixed the song order — songs were appearing out of sequence, and duplicate comments were showing up in the chat.

At 11:34pm I discovered that on newer iPhones, the live page content was rendering underneath the Dynamic Island. That's not something you find in a simulator. That's something you find when you're holding the phone in your hand, trying to see who's chomping, and you can't read the first line of text.

This is what dogfooding actually means. Not "I used my own app once and it seemed fine." It means you're sitting on your couch during a Pigeons show, fixing a CSS bug that's blocking your own experience, deploying it, and immediately seeing whether it worked — all while the show is still going.

## The Breadth Problem

Solo developers are supposed to specialize. Pick a lane. You can't do backend and frontend and mobile and DevOps and design. That's a team.

In one week I worked on:

- **Audio infrastructure** — Relisten API integration, playlist players, collapsible audio widgets, caching, recording quality filtering
- **Real-time features** — Live chat (sorry, Live Chomping), WebSocket setlist polling, presence indicators
- **Data ingestion** — Scrapers for Phantasy Tour, TTBase, setlist.fm, Archive.org/etree
- **Security** — Rate limiting, CORS hardening, JWT invalidation on password change, auth on every route
- **Testing** — Playwright E2E coverage, mock HTTP servers, backend coverage push, CI pipeline
- **Mobile** — iOS Dynamic Island fix, Chrome mobile layout, safe area insets, App Review compliance
- **Social features** — Quick Post, @mentions, clickable URLs, clickable avatars, bug forum with upvotes
- **Design** — Goose Mode (x5), Spotify now-listening card, admin analytics dashboard

This isn't sustainable without Claude. I want to be honest about that. I'm not some 10x developer who figured out the productivity secret. I'm a normal developer who has a collaborator that doesn't sleep, doesn't get bored of writing tests, and can context-switch from a Go backend handler to a Swift layout constraint to a Playwright test assertion without missing a beat.

The thing Claude is genuinely good at — the thing that makes the breadth possible — is carrying the context. I can say "the Relisten player should be collapsible, like we did on the show listing page" and it knows what I mean because it wrote that code an hour ago. I don't have to re-explain the component architecture every time I switch contexts. It already knows.

## The Things I Still Do

I want to be clear about what Claude doesn't do, because the discourse around AI and coding has gotten absurd in both directions. People either think it writes your entire app for you, or they think it's useless. Neither is true.

Claude doesn't know what to build. It doesn't know that a Pigeons show is happening tonight and that the setlist tracker needs Phantasy Tour as a source. It doesn't know that the Goose Mode countdown should use a flip-clock style because that's what feels right for the aesthetic. It doesn't know that the live chat should be called "Live Chomping" because that's what the community actually calls it. It doesn't know that the Quick Post feature exists because I watched someone try to share a recording and give up because it was too many steps.

Every feature started with me noticing something — a pain point, an opportunity, an idea at 3am that I couldn't let go of. Claude is the best collaborator I've ever had for turning those observations into running software. But the observations are mine. The taste is mine. The understanding of what this community needs is mine.

And the bugs. The bugs are mine too. The production crash from the code coverage import that should never have been in main.go — that was a human mistake. Claude wrote the instrumentation; I'm the one who forgot to check the build before deploying. The authentication forwarding bug that broke internal SDUI calls — that emerged from the interaction between two features Claude had built separately, each correct in isolation, broken in combination. Integration bugs are still human problems. They require understanding the whole system, not just the code.

## 43.9% to 70.4%

On March 8th at 8:45am — after the Pigeons show, after the Goose Mode all-nighter, after 124 commits in a single day — I asked Claude to push the backend test coverage as high as it could go.

One commit. 43.9% to 70.4%.

This is the thing that makes me genuinely optimistic about building alone. The testing tax — the thing that slows down every solo developer, the thing you skip because you're tired and the feature works and you'll write tests later (you won't) — that tax is effectively gone. Claude writes comprehensive tests. Not just happy-path assertions. Edge cases. Error conditions. Auth boundary tests. The kind of tests you'd write if you had infinite patience and no ship date.

I still write tests for the things that matter to me — the tricky integration points, the things where the test itself is the specification. But the coverage floor, the boring-but-necessary tests that catch regressions? That's not my job anymore. And that means I actually have test coverage, which means I can refactor with confidence, which means the codebase stays healthy even at this pace.

## Three Nights at the Beacon

The Pigeons show was from my couch. The Tedeschi Trucks Band run at the Beacon Theatre was from my seat.

TTB was playing three nights at the Beacon in March. I had tickets. I also had a platform that didn't know Tedeschi Trucks Band existed yet. On Tuesday afternoon I added them — 58 shows for the 2026 Future Soul Tour. But TTB's setlists don't come from setlist.fm. They come from TTBase, which has a completely different HTML structure. So I needed a new scraper.

That night, from my seat at the Beacon, I built it. I told Claude what TTBase looked like, what data I needed, and how it should integrate with the live setlist poller. Claude wrote the scraper. I deployed it. It didn't work — the HTML structure didn't match what we'd expected. So I told Claude what was wrong, it fixed the scraper to match the actual Songfish HTML structure, I deployed again, and watched the setlist populate in real time while the band was playing.

I used the app across all three shows. I'd be sitting there, listening to Derek Trucks play, and I'd notice something — a layout bug, a feature that didn't work right, something that could be better. I'd pull out my phone, tell Claude what I needed, watch it write the fix, push it to production, and then check it on my phone. All from my seat. All while the show was happening.

This is a different thing from the Pigeons show, where I was on the couch and had a laptop open. At the Beacon I was in the audience with nothing but my phone. The workflow was: notice a problem, describe it to Claude in plain English, Claude fixes it and pushes to prod, I pull up the app and verify. No laptop. No IDE. No terminal. Just me, my phone, and a collaborator who could do the rest.

By the end of the three-night run, the TTB experience on Zabriskie was solid. Live setlists from TTBase. Show pages with all the metadata. The whole thing built and refined from inside the venue where the band was playing. That's not a development workflow I ever imagined having.

## The Week Keeps Going

After the Saturday marathon and the Beacon run, I kept building. Sunday: clickable URLs in posts and comments. A small thing. The kind of thing you'd never prioritize on a roadmap but that users notice immediately.

Thursday: Grahame Lesh & Friends with 25 shows. Apple App Review fixes — a camera crash and an Android banner that needed updating. And then the bug forum. A full in-app bug reporting system with upvotes, comments, categorization, bot notifications, and admin tools. Built in an afternoon.

The bug forum is maybe the most meta thing I've built. A bug reporting system, built by one person with an AI collaborator, for reporting bugs in an app built by one person with an AI collaborator. It has six commits spanning two hours. It works. Users are filing bugs in it right now.

## What This Means

I wrote the manifesto for Zabriskie on March 8th, in between the all-night Goose Mode session and deploying fixes for the Pigeons show. The manifesto is about reclaiming the internet as a third place. About building community infrastructure that serves people instead of extracting from them. About doing it as a non-profit, solo, self-funded, because that's the only way it gets done honestly.

The week that followed is what makes that possible. Not because AI is magic, but because it changes the economics of ambition. A single person can build something that previously required a team. Not because the single person became superhuman, but because the gap between "what I can imagine" and "what I can ship" got dramatically smaller.

I have eighty beta users now. They're filing bugs. They're posting about shows. They're using Live Chomping during actual shows. The thing works. Not in a demo sense — in a "people are using this to connect with each other around music" sense.

That's all I ever wanted.

The gap between what you can imagine and what you can ship is the space where ideas go to die. For years I had this idea — the third place, the taste-based community, all of it — and I couldn't build it because I'm one person with a day job. Now I can. Not perfectly. Not without bugs. Not without 3am sessions that leave me wrecked the next day. But I can build it, and I can build it fast enough that the community doesn't outgrow the infrastructure.

144 commits in a week. Six versions of Goose Mode. A scraper built from a seat at the Beacon Theatre. A live show debugged in real time. A test suite that actually exists. A bug forum built in two hours.

This is what building with Claude actually looks like. It's not a press release. It's a Saturday that starts at 11am and ends sometime around dawn. It's three nights at the Beacon with nothing but your phone, shipping fixes between sets. And when you look up, the thing you imagined is running on your phone, and people are using it.

---

> *"Sometimes a notion gets a-hold of you, ties you to the tracks"*
> — Grateful Dead, "Althea"

*Zabriskie. Where taste resonates.*
