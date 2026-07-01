---
layout: post
title:  "The App That Lives Between Shows"
date:   2026-06-30 12:00:00 -0700
group: ai
categories: ai zabriskie development
---

> *"Thank god this app rocks and I can live through the chompers."*
> (a chomper, in the chat, on a night they could not make the show)

Two months ago I [wrote up Goose Spring '26]({% post_url 2026-04-26-spring-tour-recap %}). The through-line of that post was that the live show finally worked. Live Activities stayed up, the setlist was seconds behind the room instead of minutes, song calls landed, and forty people spent a tour sending each other 3,737 messages from their couches and their seats. The app was good for the two and a half hours a night that a band was on stage.

That was the easy half. The hard half is the other twenty-one and a half hours, and the days between shows, and the weeks between tours.

Here is the thing nobody tells you about building a live-event app: the event is not the problem. The gap is the problem. The tour ends, the chat empties out, the lock-screen Live Activity goes dark, and everyone drifts back to their normal feeds until the next run gets announced. I said this in the Spring post, in the section about the jam bracket, and then I spent the next two months building against it on purpose. The whole point of a third place is that you can go there when nothing is happening. A bar that only opens during the game is not a bar. It is a stadium.

So this is a post about what we shipped to make Zabriskie worth opening on a Tuesday in the middle of June with no show anywhere. But I want to say the real thing first, because it is the reason the rest of it exists. I am proud of this one. I love this app in a way I have not loved something I built in a long time, and I love it because it is not really mine. It is a small and genuinely growing community of people who love the same bands I do, and we are growing it the slow way, by hand, one person at a time, with no ads, no growth team, and no playbook. Two of us build it. One of us has spent the last few months handing out stickers on the lot and talking strangers into it between sets. That is the entire marketing department, and I will get to him, because he deserves most of this post.

It is also a post about whether the between-shows bet worked, which is a question you answer with numbers, and I am going to be honest about the numbers, including the ones that did not go the way I wanted.

I counted the PRs again, the same way I did in April. Between the last show of Spring tour and today, **787 pull requests** merged into [Zabriskie](https://github.com/cmeiklejohn/zabriskie). That is more than two and a half times the entire Spring tour, in about the same span of calendar. The app went from build 25-ish to **iOS 53 and Android 51**, shipped **v1.5.0 to the App Store and the Play Store**, and grew an entire second client: it runs on your **watch** now, which Patrick built end to end. And nearly half of those 787 pull requests are his, which is the single most important fact in this post and the one I want to sit on for a minute before anything else.

But start with the front door, because that is the biggest change.

## The Lot

When you open Zabriskie now, you do not land on a feed. You land on **The Lot**.

The Lot is a personalized home. It is the leftmost tab and the default route, and it is built to answer one question the feed never could: what is worth my attention right now, for me. If a band you follow is on stage somewhere, the hero card is that show with a live setlist preview ticking underneath it. If nothing is live, it reaches for the next best thing: tonight's shows, last night's recap, the jam bracket that is still taking votes, an "On This Day" card that falls back to a band anniversary when you personally have no show on the date, a bookmark worth revisiting, a nudge to post to the Flow if you have been quiet for a week.

<style>@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Instrument+Serif:ital@0;1&family=Jost:wght@400;500;600;700&display=swap');</style>

<div style="display:flex; justify-content:center; margin:18px auto; max-width:420px;">
  <div style="background:#0e0e12; border-radius:44px; padding:11px; box-shadow:0 22px 60px -18px rgba(20,18,30,0.65); width:352px;">
    <div style="background:#F0EDE4; border-radius:34px; overflow:hidden; color:#2A2A3A; position:relative;">

      <!-- status bar -->
      <div style="display:flex; align-items:center; justify-content:space-between; padding:11px 22px 4px; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:13px; font-weight:600; color:#2A2A3A;">
        <span>9:41</span>
        <span style="letter-spacing:0.06em; font-size:11px; color:#6B6B7B;">📶 &nbsp; 5G &nbsp; 🔋</span>
      </div>

      <!-- greeting header -->
      <div style="padding:8px 18px 4px; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
        <div style="display:flex; align-items:center; gap:7px; flex-wrap:wrap;">
          <span style="color:#E83A73; font-size:12px; line-height:1;">✦</span>
          <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:10px; font-weight:700; letter-spacing:0.22em; text-transform:uppercase; color:#9A9AAA;">The Lot</span>
          <span style="font-size:11px; color:#9A9AAA;">· Tuesday · Jun 30</span>
        </div>
        <div style="display:flex; align-items:flex-start; justify-content:space-between; gap:12px; margin-top:8px;">
          <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:24px; font-weight:500; letter-spacing:0.01em; line-height:1.1; color:#2A2A3A;">Good morning, cmeik</div>
          <div style="width:38px; height:38px; border-radius:50%; background:linear-gradient(135deg,#E83A73,#F2A83B); flex-shrink:0; box-shadow:0 2px 8px rgba(20,18,30,0.14);"></div>
        </div>
        <div style="display:flex; align-items:center; gap:11px; margin-top:8px; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12.5px; color:#6B6B7B;">
          <span>🔥 12-day streak</span><span style="width:3px; height:3px; border-radius:50%; background:#9A9AAA;"></span><span>✍️ 3 logged this week</span>
        </div>
      </div>

      <!-- Tier-0 cinematic hero: UP NEXT (the gap, made visible) -->
      <div style="margin:12px 14px 4px; border-radius:18px; overflow:hidden; box-shadow:0 16px 40px -14px rgba(20,18,30,0.5);">
        <div style="position:relative; min-height:206px; background:radial-gradient(circle at 30% 24%, #F8C8A8 0%, #F2A83B 20%, #E83A73 52%, #2A5FAA 100%);">
          <div style="position:absolute; inset:0; opacity:0.12; mix-blend-mode:overlay; background-image:url("data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E"); background-size:150px 150px;"></div>
          <div style="position:absolute; left:0; right:0; bottom:0; height:100%; background:linear-gradient(to top, rgba(20,18,30,0.90) 4%, rgba(20,18,30,0.5) 46%, transparent 100%);"></div>
          <div style="position:relative; z-index:1; display:flex; flex-direction:column; gap:12px; padding:15px 17px 17px; min-height:206px;">
            <div style="display:flex; align-items:flex-start; justify-content:space-between; gap:10px;">
              <span style="display:inline-flex; align-items:center; padding:6px 12px; border-radius:999px; background:rgba(20,18,30,0.5); font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:10px; font-weight:700; letter-spacing:0.18em; color:#fff;">UP NEXT</span>
              <div style="text-align:right;">
                <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-variant-numeric:tabular-nums; font-size:20px; font-weight:600; color:#fff; letter-spacing:0.04em;">9d 04h</div>
                <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:600; letter-spacing:0.16em; color:rgba(255,255,255,0.7); text-transform:uppercase;">until doors</div>
              </div>
            </div>
            <div style="margin-top:auto;">
              <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:28px; font-weight:600; letter-spacing:0.03em; line-height:1.08; color:#fff;">Goose</div>
              <div style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:20px; color:rgba(255,255,255,0.95); margin-top:1px; line-height:1.2;">The Capitol Theatre<span style="color:rgba(255,255,255,0.58);"> · Port Chester, NY</span></div>
              <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:600; letter-spacing:0.09em; text-transform:uppercase; color:rgba(255,255,255,0.72); margin-top:7px;">Fri Jul 10 · 8:00 PM</div>
              <div style="display:flex; align-items:center; gap:9px; margin-top:11px;">
                <div style="display:flex;">
                  <div style="width:26px; height:26px; border-radius:50%; background:linear-gradient(135deg,#2A5FAA,#3AC4E8); box-shadow:0 0 0 2px #14121e; margin-right:-8px;"></div>
                  <div style="width:26px; height:26px; border-radius:50%; background:linear-gradient(135deg,#E83A73,#F2A83B); box-shadow:0 0 0 2px #14121e; margin-right:-8px;"></div>
                  <div style="width:26px; height:26px; border-radius:50%; background:linear-gradient(135deg,#F2A83B,#FACC15); box-shadow:0 0 0 2px #14121e;"></div>
                </div>
                <span style="font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:13px; color:rgba(255,255,255,0.85);">patrick, gmart, +10 in your crew going</span>
              </div>
              <button style="margin-top:14px; padding:12px 16px; width:100%; border-radius:999px; border:none; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-weight:700; font-size:12px; letter-spacing:0.12em; text-transform:uppercase; background:#E83A73; color:#fff; box-shadow:0 10px 28px -6px rgba(232,58,115,0.6);">View show →</button>
            </div>
          </div>
        </div>
      </div>

      <!-- tier label -->
      <div style="display:flex; align-items:center; gap:12px; margin:18px 16px 10px;">
        <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:700; letter-spacing:0.20em; text-transform:uppercase; color:#9A9AAA;">Act now</span>
        <span style="flex:1; height:1px; background:rgba(42,42,58,0.10);"></span>
        <span style="font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; color:#9A9AAA;">nothing live tonight</span>
      </div>

      <!-- Jam Bracket card -->
      <div style="margin:0 14px 12px; background:#FEFDFB; border-radius:16px; box-shadow:0 6px 18px rgba(20,18,30,0.06); padding:16px; position:relative; overflow:hidden;">
        <span style="position:absolute; left:0; top:0; bottom:0; width:3px; background:#2A5FAA;"></span>
        <div style="display:flex; align-items:center; gap:8px; margin-bottom:9px;">
          <span style="font-size:15px;">🏆</span>
          <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:700; letter-spacing:0.16em; text-transform:uppercase; color:#2A5FAA;">Jam Bracket · Semifinals</span>
        </div>
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:21px; font-weight:600; letter-spacing:0.02em; color:#2A2A3A;">Madhuvan <span style="color:#9A9AAA; font-size:15px;">vs</span> Into the Myst</div>
        <div style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:16px; color:#2A5FAA; margin-top:2px;">voting closes in 18h · you haven't voted</div>
        <button style="margin-top:14px; padding:9px 16px; border-radius:999px; border:none; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-weight:600; font-size:11px; letter-spacing:0.10em; text-transform:uppercase; background:#E83A73; color:#fff; box-shadow:0 6px 16px -4px rgba(232,58,115,0.3);">Listen to both · vote</button>
      </div>

      <!-- On This Day card -->
      <div style="margin:0 14px 14px; background:#FEFDFB; border-radius:16px; box-shadow:0 6px 18px rgba(20,18,30,0.06); padding:16px; position:relative; overflow:hidden;">
        <span style="position:absolute; left:0; top:0; bottom:0; width:3px; background:#F2A83B;"></span>
        <div style="display:flex; align-items:center; gap:8px; margin-bottom:7px;">
          <span style="font-size:15px;">📅</span>
          <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:700; letter-spacing:0.16em; text-transform:uppercase; color:#F2A83B;">On this day</span>
        </div>
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:600; letter-spacing:0.02em; color:#2A2A3A;">2 years ago at the Cap</div>
        <div style="font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:13px; color:#6B6B7B; margin-top:5px; line-height:1.5;">You caught Hungersite → Arrow. 34-show gap on the Arrow that night.</div>
      </div>

      <!-- bottom nav -->
      <div style="display:flex; align-items:center; justify-content:space-around; padding:9px 8px 12px; border-top:1px solid rgba(42,42,58,0.08); background:rgba(255,255,255,0.4); font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
        <div style="display:flex; flex-direction:column; align-items:center; gap:2px;"><span style="font-size:17px;">🪧</span><span style="font-size:8px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#E83A73;">Lot</span></div>
        <div style="display:flex; flex-direction:column; align-items:center; gap:2px; opacity:0.45;"><span style="font-size:17px;">🌀</span><span style="font-size:8px; font-weight:600; letter-spacing:0.06em; text-transform:uppercase; color:#6B6B7B;">Flow</span></div>
        <div style="display:flex; flex-direction:column; align-items:center; gap:2px; opacity:0.45;"><span style="font-size:17px;">📺</span><span style="font-size:8px; font-weight:600; letter-spacing:0.06em; text-transform:uppercase; color:#6B6B7B;">Live</span></div>
        <div style="display:flex; flex-direction:column; align-items:center; gap:2px; opacity:0.45;"><span style="font-size:17px;">🎟️</span><span style="font-size:8px; font-weight:600; letter-spacing:0.06em; text-transform:uppercase; color:#6B6B7B;">Shows</span></div>
        <div style="display:flex; flex-direction:column; align-items:center; gap:2px; opacity:0.45;"><span style="font-size:17px;">🔔</span><span style="font-size:8px; font-weight:600; letter-spacing:0.06em; text-transform:uppercase; color:#6B6B7B;">You</span></div>
      </div>

    </div>
  </div>
</div>

The design principle underneath The Lot is the same one that governs the whole app: it never shows you an empty room. If there is nothing live, it does not say "nothing is live." It finds the thing about your history, or your friends, or the band's history, that is worth a tap. An app that opens onto a dead feed teaches you to stop opening it. The Lot is the counter to that reflex, and it is the surface that most of the rest of this post feeds into.

## Hangs and Crews: The Social Bets That Have Not Landed Yet

The most on-the-nose thing we built for the between-shows problem is **Hangs**. A hang is a real-world meetup: tacos before the show, a lot rendezvous, a hotel-bar session after the encore. You make one, you drop a place on it, and people RSVP.

Every hang has a cinematic detail page at `/hang/:id` with a hero, the RSVP list, an activity timeline, and a **map**. The place picker autocompletes against OpenStreetMap, so you are pinning a real venue, not typing an address into a box. There is a `/hangs` hub that consolidates every hang you are part of and a `/hangs/past` archive so the meetups you have already done do not clutter the live list.

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="height:120px; background:linear-gradient(135deg,#fb923c 0%,#ea580c 60%,#8B5CF6 100%); position:relative;">
      <div style="position:absolute; bottom:10px; left:14px; color:#fff;">
        <div style="font-size:10px; font-weight:800; letter-spacing:0.06em; text-transform:uppercase; opacity:0.9;">Pre-show hang</div>
        <div style="font-size:19px; font-weight:800;">Tacos before Red Rocks 🌮</div>
      </div>
      <div style="position:absolute; top:10px; right:12px; background:rgba(0,0,0,0.35); color:#fff; font-size:10px; font-weight:700; padding:4px 8px; border-radius:8px;">Fri · 6:00pm</div>
    </div>
    <div style="padding:14px;">
      <div style="display:flex; gap:10px; align-items:center; padding:10px 12px; background:#fff; border-radius:12px;">
        <span style="font-size:20px;">📍</span>
        <div style="flex:1;"><div style="font-size:13px; font-weight:700;">Torchy's Tacos · Morrison</div><div style="font-size:11px; color:#6B7280;">1.2 mi from the venue · pinned on the map</div></div>
      </div>
      <div style="margin-top:10px; padding:10px 12px; background:#fff; border-radius:12px;">
        <div style="font-size:10px; font-weight:800; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase; margin-bottom:8px;">Going · 4</div>
        <div style="display:flex; align-items:center; gap:8px;">
          <div style="display:flex;">
            <div style="width:26px; height:26px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:10px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-7px;">P</div>
            <div style="width:26px; height:26px; border-radius:50%; background:linear-gradient(135deg,#b0eaff,#3ba8e0); border:2px solid #fff; font-size:10px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-7px;">C</div>
            <div style="width:26px; height:26px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; font-size:10px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-7px;">B</div>
            <div style="width:26px; height:26px; border-radius:50%; background:linear-gradient(135deg,#fde68a,#f59e0b); border:2px solid #fff; font-size:10px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">G</div>
          </div>
          <span style="flex:1; font-size:11px; color:#6B7280;">patrick, chomper1, +2</span>
          <span style="font-size:12px; font-weight:800; padding:6px 14px; border-radius:12px; background:#FB923C; color:#fff;">I'm in</span>
        </div>
      </div>
    </div>
  </div>
</div>

Here is the honest part, and it covers the bigger swing too. Hangs has a dozen entries so far, not a movement. And Crews, which got the most social engineering of anything this stretch, landed even quieter. We turned Crews from a roster into a real-time room this tour: a live channel with messages, reactions, and typing indicators, live-show song markers mirrored straight in, and song calls you can throw from inside the crew feed that share the same store as the live room. It is genuinely nice engineering, and almost nobody used it. Ten crews, about two dozen messages total, across the entire window. I am putting that number in the post because the premise of writing these is that the receipts are real, and "we shipped real-time crew chat" and "people use real-time crew chat" are two different claims and only the first one is true yet. My read is that both Hangs and Crews are tour-shaped features. They need a big shared run pointing everyone at the same cities on the same nights to fill, and there was not one in this window. The plumbing is done. Whether people move in is a Fall problem, and I will report back either way.

## A Reference Library You Browse for Fun

The other way to earn a Tuesday open is to be worth reading when nothing is happening. So a big chunk of the 787 went into turning Zabriskie into a browsable reference for the bands themselves, and this is the part where the usage numbers actually show up.

**The Songbook.** We rewrote the old song-search box into a full **catalog directory**: every band's entire songbook, browsable, with a band-switcher rail so you can jump from Goose's book to Phish's to Billy's. It is styled as a "Gold Ledger," with letter headers and per-song play counts, and it marks the songs you personally have caught live so your own book fills in as you tour. There are **17,326 songs** in it across every band we track. It is the thing you open in a hotel room at 1am to settle a bet about how many times they have played a song.

**The Poster Archive.** Every show can carry its posters now, including multiple variants per show (the foil, the rainbow, the artist edition) with proper variant labels and a carousel. Admins get a variant picker and an "add another" flow, and there is an in-app button to **sync posters straight from the Goose store**, backed by a cron job that refreshes the archive daily. This is the poster-archive goal I have had for a while: not "the one official poster," but the whole wall of variants a fan might recognize from a show they were at.

**Sit-ins and Guests.** This one is Patrick's, top to bottom. When a guest sits in with the band, that is now first-class data: a `/guests` directory, guest credits on the song page and the setlist with per-instrument icons, band-emoji guest reactions, and a backfill of the guest history across the whole catalog. Phish alone came in at 151 guests over 1,006 appearances, plus JRAD, Greensky, Goose, and more. If you ever wanted to know every time a specific person walked on stage with a band, that list exists now because Patrick built it, and it is the kind of thing only a real fan thinks to build.

<div style="display:flex; gap:14px; flex-wrap:wrap; justify-content:center; margin:18px auto;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); padding:0; flex:0 0 auto; max-width:300px; overflow:hidden; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
    <div style="background:linear-gradient(135deg,#b8860b,#8B5CF6); color:#fff; padding:10px 14px; font-size:11px; font-weight:800; letter-spacing:0.04em; text-transform:uppercase;">📖 Songbook · Goose</div>
    <div style="padding:10px 14px;">
      <div style="font-size:10px; font-weight:800; color:#b8860b; margin:4px 0;">· I ·</div>
      <div style="display:flex; justify-content:space-between; padding:5px 0; font-size:13px;"><span style="font-weight:600;">Indian River <span style="color:#10b981;">✓</span></span><span style="color:#6B7280; font-size:11px;">×117</span></div>
      <div style="display:flex; justify-content:space-between; padding:5px 0; font-size:13px;"><span style="font-weight:600;">Into the Myst <span style="color:#10b981;">✓</span></span><span style="color:#6B7280; font-size:11px;">×62</span></div>
      <div style="display:flex; justify-content:space-between; padding:5px 0; font-size:13px;"><span>Il Duderino</span><span style="color:#6B7280; font-size:11px;">×9</span></div>
    </div>
  </div>

  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); padding:0; flex:0 0 auto; max-width:300px; overflow:hidden; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
    <div style="background:#262626; color:#fff; padding:10px 14px; font-size:11px; font-weight:800; letter-spacing:0.04em; text-transform:uppercase;">🎤 Guests · Factory Fiction</div>
    <div style="padding:12px 14px;">
      <div style="display:flex; gap:10px; align-items:center; padding:6px 0;"><span style="font-size:16px;">🎻</span><div style="flex:1;"><div style="font-size:13px; font-weight:700;">Guest fiddle</div><div style="font-size:10px; color:#6B7280;">3 appearances · last 12/13/25</div></div></div>
      <div style="display:flex; gap:10px; align-items:center; padding:6px 0;"><span style="font-size:16px;">🎺</span><div style="flex:1;"><div style="font-size:13px; font-weight:700;">Guest horns</div><div style="font-size:10px; color:#6B7280;">1 appearance · Goosemas '24</div></div></div>
      <div style="margin-top:6px; font-size:11px; font-weight:700; color:#8B5CF6;">See full guest directory →</div>
    </div>
  </div>
</div>

None of these three are things you open during a show. They are things you open because you are a fan and it is a slow afternoon and you want to fall into your band's history for twenty minutes. That is the whole idea, and unlike the crew rooms, this is the part people actually did all summer. More on that below.

## It Runs on Your Watch Now

We shipped an entire new client this window: **Wear OS and Apple Watch**, and Patrick built essentially all of it, from the SwiftUI screens to the WidgetKit complications to the connectivity layer.

The headline feature is an **always-on live show on your wrist**. When a band you follow is playing, an OngoingActivity and a watch-face complication put the current song, set, and elapsed time on your watch, persistent, glanceable, updating as the setlist moves. There is a countdown complication for the gap between shows, so even when nothing is live your watch face can tell you how long until the next one. We mirrored the live reaction kit to the watch, so you can react to a song without pulling your phone out of your pocket at all.

<div style="display:flex; gap:20px; flex-wrap:wrap; justify-content:center; margin:18px auto; align-items:center;">
  <div style="background:#000; border-radius:44px; padding:10px; box-shadow:0 8px 24px rgba(0,0,0,0.25);">
    <div style="width:150px; height:186px; border-radius:36px; background:radial-gradient(circle at 50% 30%, #2a1d3a, #0a0510); display:flex; flex-direction:column; align-items:center; justify-content:center; color:#fff; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; padding:14px;">
      <div style="font-size:26px;">🪿</div>
      <div style="font-size:9px; font-weight:800; letter-spacing:0.08em; color:#a855f7; margin-top:4px;">GOOSE · LIVE</div>
      <div style="font-size:16px; font-weight:800; margin-top:6px; text-align:center; line-height:1.1;">Tumble</div>
      <div style="font-size:10px; opacity:0.7; margin-top:2px;">SET 2 · 6:41</div>
      <div style="display:flex; gap:8px; margin-top:10px; font-size:15px;"><span>🔥</span><span>🤯</span><span>🕺</span></div>
    </div>
  </div>
  <div style="max-width:220px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626; font-size:13px; line-height:1.5;">
    <div style="font-weight:800; margin-bottom:4px;">Glance, don't grab.</div>
    The complication keeps the live setlist on your watch face the whole show. The reaction ring means you can throw a 🔥 at a jam without ever taking your phone out.
  </div>
</div>

The watch is the purest expression of the "be present at the show" argument I keep having with people, turned inside out. The complaint is that the app pulls you into your phone. The watch is the app admitting the phone is too much during the show, and shrinking down to a glance and a tap on your wrist so you can put the phone away and still be in the conversation. It is the app taking the criticism seriously and building the answer.

## The Whole App Got Redesigned Underneath All of This

Quietly, in parallel with every feature above, we rebuilt the look of the app. The Flow, the live chat, profiles, notifications, crews, and festivals all got "v2" cinematic redesigns: warmer, larger, more editorial, less like a database with a skin on it. This is a big deal operationally because of how it ships. Every redesign runs behind a version flag, keyed to the client build, so an old phone that has not updated keeps getting the old design and a fresh install gets the new one, and neither breaks. That is the machinery that lets us redesign a live surface without stranding the person watching a show on a two-month-old binary. It is unglamorous and it is most of why we can move this fast without breaking the people already here.

## New Rooms: Cabo, the Festivals, More Bands

The band list kept growing. We backfilled full histories for **Widespread Panic (3,114 shows), Eggy, and Dizgo**, on top of the dozen bands we added during Spring, each with the full treatment: a source for the setlists, a band page, a band mode, a spot in the directory. There are **102 bands and 38,894 shows** in the catalog now.

We built out **festivals** as their own surface: a multi-stage schedule grid, a plan phase, and a full **festival recap** that reads the whole weekend rather than one show, with a program block, hero moments, a route map, and per-day bookends. **Viva El Gonzo** in Cabo got the destination-festival treatment (down to canonicalizing every set to San José del Cabo), and we seeded lineups for All Good Now and Northlands. Goose Summer is on the calendar and already wired in.

The point of all of it is the same: the more bands and the more festivals live in the app, the more nights of the year there is something happening for somebody, and the fewer truly dead Tuesdays there are.

## Half of This Is Patrick

I need to stop and do this properly, because if you have read this far you have been reading "we" the whole time and the "we" is doing a lot of quiet work.

Patrick McCaughey (he is `@blimpalot` in the app) wrote **nearly half of the 787 pull requests in this window.** Not helper commits. Whole features, end to end, that are among the best things in the product. The Apple Watch app is his. The entire sit-in and guest-credit system is his. Group DMs and the Messages hub are his. Birthdays, big chunks of the poster archive, huge swaths of Tour Stats, the band backfills, the reaction kits for new bands. When I said the reference library is the part people actually used all summer, I was mostly describing Patrick's work.

He put it best himself, in the chat one night, and I am quoting it because it is both generous and exactly backwards about who deserves credit:

> *"I might have wrote the app but all the cool shit in the app isn't me it's Patrick lol"*

That was me, about him. He would tell you the opposite. We are both a little bit right and he is more right than I am.

And here is the part that does not show up in a commit log at all. **Patrick made stickers.** Actual, physical, die-cut vinyl stickers, and he has spent this whole stretch handing them out at shows, on the lot, in the lot line, to strangers between sets, and quietly talking people into trying the app one human conversation at a time. Every growth chart later in this post that ticks up, ticks up partly because a guy stood in a parking lot in the heat and gave someone a sticker and said you should try this thing my buddy and I made.

<div style="display:flex; gap:18px; flex-wrap:wrap; justify-content:center; margin:20px auto; max-width:520px;">
  <div style="filter:drop-shadow(0 8px 14px rgba(20,18,30,0.28)); transform:rotate(-5deg);">
    <div style="width:180px; height:180px; border-radius:26px; background:#FEFDFB; padding:9px; box-sizing:border-box;">
      <div style="width:100%; height:100%; border-radius:20px; background:radial-gradient(circle at 32% 26%, #F8C8A8 0%, #F2A83B 22%, #E83A73 55%, #2A5FAA 100%); display:flex; flex-direction:column; align-items:center; justify-content:center; color:#fff; text-align:center; position:relative; overflow:hidden;">
        <div style="position:absolute; inset:0; opacity:0.12; mix-blend-mode:overlay; background-image:url("data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E"); background-size:120px 120px;"></div>
        <div style="font-size:40px; position:relative;">🪿</div>
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:20px; font-weight:700; letter-spacing:0.16em; position:relative; margin-top:4px;">ZABRISKIE</div>
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:600; letter-spacing:0.22em; text-transform:uppercase; opacity:0.9; position:relative; margin-top:3px;">couch tour '26</div>
      </div>
    </div>
  </div>
  <div style="filter:drop-shadow(0 8px 14px rgba(20,18,30,0.28)); transform:rotate(4deg);">
    <div style="border-radius:999px; background:#FEFDFB; padding:9px; box-sizing:border-box;">
      <div style="width:162px; height:162px; border-radius:999px; background:#141220; display:flex; flex-direction:column; align-items:center; justify-content:center; color:#fff; text-align:center; border:2px solid #E83A73; position:relative;">
        <div style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:22px; color:#F2A83B; line-height:1.05;">are you<br>chomping?</div>
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:700; letter-spacing:0.2em; text-transform:uppercase; color:rgba(255,255,255,0.75); margin-top:10px;">✦ get on the lot</div>
      </div>
    </div>
  </div>
</div>
<div style="text-align:center; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; color:#9A9AAA; margin:-4px auto 6px; max-width:420px;">Stickers in the app's colors. A mockup, not a photo of Patrick's real ones, which are better.</div>

This is what organic actually means. Not a referral loop or a viral coefficient. A person who loves the thing, standing in front of another person, in the place they both love to be. You cannot buy that and you cannot fake it, and it is the entire reason a two-person app has a real community around it at all.

## Did It Work? The Honest Numbers

I promised numbers, not vibes, so here they are, including the ones I would have quietly dropped if I were selling something.

**Users grew from 251 to 426.** That is 70% growth in two months, which sounds great until you look at *when* it happened.

<div style="background:#e5e2d9; padding:20px 18px; border-radius:14px; margin:16px auto; max-width:560px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="font-size:11px; font-weight:800; letter-spacing:0.05em; text-transform:uppercase; color:#6B7280; margin-bottom:14px;">New signups by week · Mar 23 → Jun 29</div>
  <div style="display:flex; align-items:flex-end; gap:5px; height:130px;">
    <div style="flex:1; background:#8B5CF6; height:44%; border-radius:3px 3px 0 0;" title="30"></div>
    <div style="flex:1; background:#8B5CF6; height:65%; border-radius:3px 3px 0 0;" title="44"></div>
    <div style="flex:1; background:#8B5CF6; height:19%; border-radius:3px 3px 0 0;" title="13"></div>
    <div style="flex:1; background:#8B5CF6; height:76%; border-radius:3px 3px 0 0;" title="52"></div>
    <div style="flex:1; background:#8B5CF6; height:69%; border-radius:3px 3px 0 0;" title="47"></div>
    <div style="flex:1; background:#7c3aed; height:62%; border-radius:3px 3px 0 0;" title="42 · tour ends"></div>
    <div style="flex:1; background:#7c3aed; height:100%; border-radius:3px 3px 0 0;" title="68"></div>
    <div style="flex:1; background:#c4b5a0; height:9%; border-radius:3px 3px 0 0;" title="6"></div>
    <div style="flex:1; background:#c4b5a0; height:12%; border-radius:3px 3px 0 0;" title="8"></div>
    <div style="flex:1; background:#c4b5a0; height:10%; border-radius:3px 3px 0 0;" title="7"></div>
    <div style="flex:1; background:#c4b5a0; height:9%; border-radius:3px 3px 0 0;" title="6"></div>
    <div style="flex:1; background:#c4b5a0; height:10%; border-radius:3px 3px 0 0;" title="7"></div>
    <div style="flex:1; background:#c4b5a0; height:21%; border-radius:3px 3px 0 0;" title="14"></div>
    <div style="flex:1; background:#c4b5a0; height:21%; border-radius:3px 3px 0 0;" title="14"></div>
    <div style="flex:1; background:#c4b5a0; height:4%; border-radius:3px 3px 0 0;" title="3"></div>
  </div>
  <div style="display:flex; justify-content:space-between; font-size:9px; color:#9CA3AF; margin-top:6px;">
    <span>◀ Spring tour</span><span>tour ends</span><span>summer ▶</span>
  </div>
  <div style="font-size:10px; color:#6B7280; margin-top:8px; text-align:center;">Purple = tour weeks. The two tallest bars are the tour and the week right after it. Then it falls off a cliff.</div>
</div>

Signups are event-driven. They spiked during the tour and the week after (the wrap-up avalanche, word of mouth, and Patrick's stickers), then dropped to single digits a week through June. This is the thing I got wrong going in: I half-expected the between-shows features to keep pulling new people in on their own. They did not. **You acquire people at shows, from other people.** Nothing I shipped in May moved that needle, and pretending otherwise would be exactly the kind of thing this blog exists to call out.

But I want to be clear that I am proud of that chart, spike and cliff and all. Those 426 people are not a paid-acquisition cohort. Every one of them is someone who was at a show, or in a chat, or handed a sticker, and decided this was worth their time. That is the only kind of growth I actually want. I would rather have 426 people who love it than 40,000 who installed it and forgot, and the shape of that chart is what growing something real and by hand actually looks like.

So if new signups are a tour phenomenon, the real job of everything in this post is not acquisition. It is **retention**: keeping the people who showed up in Spring around and warm until the next run, so the community is intact when it matters. And on that, the numbers are better and more honest:

- **The live chat never went silent.** 2,547 messages from 42 people across this window, versus 3,737 from 40 during the concentrated Spring run. Fewer messages, because there were far fewer big Goose nights, but the same size of core showing up, between tours, with no fourteen-show schedule forcing them to.
- **People spent the quiet months logging their history.** Attendance records went from 1,783 to **4,598**, and **2,570 of those were logged in this window**, spread across 70 different bands. Cataloging the shows you have been to is the most between-shows activity there is. Nobody logs a 2016 club show during a live set. They do it on a slow Sunday, which is precisely the Tuesday-open I was trying to earn.
- **The reference library got used.** The Songbook, the poster archive, and the guest directory are the surfaces with real traffic on no-show days, which tracks: they are built to be read when nothing is live.

And the numbers I am **not** going to give you: a clean daily-active-users chart. We record a row when a user opens the app, but that table has multi-week holes in it from May and early June where the logging was broken, so any DAU or "percent active on non-show days" figure I quoted would be built on gaps. I would rather tell you the table is broken than draw a confident line through missing data. Fixing that instrumentation is now on the list, because the between-shows thesis deserves a real measurement and right now I cannot give it one.

The one-line version: **we are not going viral, and I do not want to. We are growing a thing I love slowly, by hand, and keeping the room warm between tours. Mostly we did.**

## What This Stretch Taught Me

- **The gap is the product, and it is a retention problem, not a growth problem.** Spring proved the show works. This stretch proved that new users arrive on the tour calendar and not otherwise, so the between-shows job is to hold the community you already have. Almost every feature in this post is a bet on retention, and the honest scoreboard is "the core stayed" and "crews and hangs have not proven themselves yet," not a signup hockey stick.
- **Measure the thesis or you are guessing.** I built a whole argument about between-shows engagement and then discovered my own session-tracking had holes in it. Shipping the features without fixing the instrument to grade them is a mistake I am writing down so I actually fix it.
- **Shipping a redesign is a distributed-systems problem.** The version-flag machinery that lets an old build keep its old UI while a new build gets v2 is the least visible work of the whole two months and close to the most important. Without it, moving this fast would mean breaking the exact people who already showed up.
- **787 PRs is still not normal, and most of it is still Claude.** Same as April: a lot of this was written by Claude, a lot of it had to be rewritten by Claude after I caught it going sideways, and every sharp edge got logged into the [agent reliability dataset]({% post_url 2026-06-10-the-test-suite-was-the-incident %}) I keep publishing from. The failures are the point of the dataset, not something to hide.
- **Being on the App Store changes what you build.** Report-and-block moderation on chat messages was not a feature anyone in the community asked for. It was the price of admission to the store, and it shipped in the same window as everything fun. Growing up means building the boring safety surface too.

And I should say the obvious thing, the thing under all of it, the reason two people spend nights and tour weekends building a couch-touring app instead of doing literally anything else: **Goose fucks.** That is the whole premise. The band is that good, the jams are worth talking about at 1am, the bustouts are worth calling from your couch, and the community around them is worth building a home for. Everything in this post is downstream of a band being good enough that people want to be in the same room about it, even when the room is a chat.

Goose Summer is on the calendar. The festivals are seeded. The watch is on my wrist, and Patrick built it. The app is not done, and the real test of everything here is not this recap. It is whether the rooms fill when the next tour points everyone at the same cities again.

If you toured with us this Spring, thanks for chomping. If you have been in the chat on the dead Tuesdays, you are the 42, and you are the entire experiment working. If Patrick handed you a sticker in a parking lot and you scanned it, welcome, you are the growth chart. And if you have never tried it and an app that is good company between shows sounds like something you want, come find us. There is probably something worth reading tonight, even if nothing is live.

---

*One housekeeping note, same as last time: the screens in this post are mockups, not real screenshots, drawn to make each feature legible in one frame. The real app looks slightly different on iOS, Android, and the watch (Live Activities vs. ongoing notifications, complication styling, system fonts). The features themselves all shipped, and the stickers are real (Patrick's are cooler than the mockup here). The growth figures are real production numbers as of June 30, including the unflattering ones. Patrick's share of the work is real too, and if anything I undersold it.*
