---
layout: post
title:  "Spring Tour Recap: A Month of Shipping on Zabriskie"
date:   2026-04-26 12:00:00 -0700
group: ai
categories: ai zabriskie development
---

> *"this app is the bees. I am very grateful for it."*
> (a chomper, end of show, Irving, 4/25)

Goose Spring '26 just wrapped. The run started March 28 in Athens and closed out Saturday night in Irving. Fourteen shows. One festival lead-in. Fourteen live chats running in parallel with each show, with 40 people sending **3,737 messages** to each other across the tour. Most of them sent from couches.

For the second half of tour I had a weird but perfect setup. Goose was on Eastern time. I was couch-touring most of those shows from a hotel room in Vegas. Then Phish was at Sphere (six nights across two runs, 4/16-18 and 4/23-25), and I was at all of them. Pacific time. The two shows overlapped by about an hour every night: the back end of the Goose set ran into the front end of Phish at Sphere. Which meant at any given moment in that hour I had two live chats open and two iOS Live Activities going on the lock screen. One for the show I was actually at, one for the show I was following along with from inside a different venue. The Dynamic Island had to share.

That overlap hour is where the whole pitch of this app clicked for me, so it's worth pulling out as its own point. Sports fans have lived inside multi-game nights forever. Your team's game on the TV, the other playoff game on a tablet, score alerts buzzing on your phone, the group chat scrolling beside it, fantasy stats updating in another tab. Nobody thinks twice about it. That kind of parallel, shared, real-time consumption is the default for sports.

Music has never had any of that. A concert has always been one show, in one room, ending when the lights come up, with whatever conversation you happened to have with the person next to you. If two of your favorite bands are playing the same night in different cities, that has historically just been a thing you mourn. There's no second-screen experience for live music. There's no group chat thread for the show you're not at. There's no "score alert" telling you the band you can't see just opened with something rare. That whole layer doesn't exist.

We've gotten pushback on this specifically. People have told us, in the chat and in person, that they don't think we should be encouraging anyone to open Zabriskie at a show. The argument is the same one phones have always heard at live events: be present, put it away, watch the band. I take it seriously. I also remember being on the other side of the same argument seventeen years ago.

Back in 2008 when the iPhone first came out, I worked at a baseball startup in Boston. We built live play-prediction inside the app: in your seat, during the game, what's the next pitch, did the runner just steal. The reaction we got from people who had never tried it was word-for-word identical to what we get now about Zabriskie. "No one is going to be on their phone at a baseball game." Today every Major League ballpark has a stadium app open across the section, every wrist has the score on it, and a fan in their car or their living room is part of the same conversation as the fan in section 304. The game didn't get worse. The community got bigger. People who couldn't physically be there became part of being there.

Live music gets there too. The phone isn't the enemy of the show. The phone, used well, is what lets the show have a community around it that outlasts the show.

That is a lot of what we're building. Anything that broke at the Goose show, Patrick and I would fix between Goose and Phish, and I'd run it live at Phish two hours later. Every feature got tested twice a night, against two different bands, in two different time zones, by a person who was actively living the multi-show pattern the app is supposed to enable.

I started writing this because I wanted to remember what we built during the tour. I looked at the PR list and counted. **Three hundred and nine pull requests** merged into [Zabriskie](https://github.com/cmeiklejohn/zabriskie) between the first show and the last. About fifty of those merged today, with the tour wrap-up package shipping in real time as I'm writing this. That number doesn't feel real. It is real. Most of it shipped to the web immediately so we could test it ourselves the moment it merged, then went out to our TestFlight and Play Store testers within hours, and will be live in the App Store and Play Store this week for everyone. Some of it shipped during the show.

This is what stuck.

## The Live Show Got Real

The biggest change is that "couch touring with the app" is now a thing people actually do, not a thing I keep telling people they should try. Here is what the chat looked like in Houston on 4/23, around 11pm, from Patrick, my collaborator on the project:

> Whew just got home. Was updating the setlist while walking home from the bar with pip and Zabriskie open lmao
>
> Ohhhhh snapp how bout that new feature I built today fam?!
>
> Got a couple folks in the chomp who witnessed the FTP!

He's talking about the FTP-witness pill, which he had shipped earlier that day. FTP is "first time played." Every song in a setlist has a debut show. When a song plays during a live chat, the app now scans everyone watching, and if any of them were RSVP'd to that song's original debut show, an inline 👀 pill appears under the song name calling them out. It looks like this:

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:#262626; color:#fff; padding:10px 16px; font-size:11px; letter-spacing:0.04em; text-transform:uppercase; display:flex; justify-content:space-between;">
      <span>Live Chat · MSG '26</span>
      <span style="opacity:0.65; font-weight:400; text-transform:none; letter-spacing:0;">1 witness</span>
    </div>
    <div style="padding:8px 0 14px;">
      <div style="padding:6px 14px;"><span style="font-size:13px; font-weight:600; color:#8B5CF6; background:#EDE9FE; padding:4px 12px; border-radius:12px;">🎸 Set 2 begins</span></div>
      <div style="padding:6px 14px;"><span style="font-size:13px; font-weight:600; color:#8B5CF6; background:#EDE9FE; padding:4px 12px; border-radius:12px;">🎵 All I Need</span></div>
      <div style="padding:2px 14px 6px;"><span style="font-size:13px; font-weight:700; color:#065F46; background:#D1FAE5; padding:4px 12px; border-radius:12px;">👀 1 person in chomp was at the FTP</span></div>
      <div style="padding:6px 14px; display:flex; gap:10px; align-items:flex-start;">
        <div style="width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#d9c6ff 0%,#a88fe6 100%); display:flex; align-items:center; justify-content:center; font-size:14px; font-weight:700; color:#fff; flex-shrink:0;">P</div>
        <div style="display:flex; flex-direction:column; gap:2px;">
          <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:600; font-size:14px;">patrick</span><span style="font-size:10px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#EC4899;">🎸 Show</span></div>
          <div style="font-size:14px; line-height:1.3;">omg I forgot I was there for the og</div>
        </div>
      </div>
    </div>
  </div>
</div>

Tap the pill and it pops open the original debut show's setlist sheet, with a Bookmark button so you can save the show for later. If you yourself were at that debut, the per-song stat sheet gets an extra "You were at this song's FTP" strip with the date and venue. So Patrick wrote and shipped the FTP-witness feature that day, then came home from a different show and used it himself in the chat for a third show. That is the loop now.

The Live Activity on iOS got a dedicated set break UI, so when the band walks off your Lock Screen tells you instead of just freezing on the last song. It buzzes when the setlist updates, so you don't have to keep waking your phone to check. Android got a redesign that matches the iOS Live Activity layout, with rich notifications that persist for the entire show instead of falling off the lock screen after a few minutes.

<div style="display:flex; gap:14px; flex-wrap:wrap; justify-content:center; margin:18px auto;">
  <div style="background:#0a0a0a; border-radius:36px; padding:14px; box-shadow:0 8px 24px rgba(0,0,0,0.18); flex:0 0 auto;">
    <div style="background:linear-gradient(180deg, #2a1d3a 0%, #1a0f2a 100%); border-radius:24px; width:280px; padding:14px 16px; color:#fff; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
      <div style="display:flex; justify-content:space-between; align-items:center; font-size:11px; opacity:0.7; margin-bottom:8px;"><span>9:41</span><span>📶 5G ⌁ 78%</span></div>
      <div style="background:rgba(139,92,246,0.18); border:1px solid rgba(139,92,246,0.35); border-radius:18px; padding:12px 14px; backdrop-filter:blur(12px);">
        <div style="display:flex; gap:10px; align-items:center; margin-bottom:6px;">
          <div style="width:32px; height:32px; border-radius:8px; background:linear-gradient(135deg,#a855f7,#7c3aed); display:flex; align-items:center; justify-content:center; font-size:14px;">🪿</div>
          <div style="display:flex; flex-direction:column; flex:1;">
            <span style="font-size:11px; font-weight:600; opacity:0.7;">GOOSE · LIVE</span>
            <span style="font-size:10px; opacity:0.55;">Saenger Theatre, NOLA</span>
          </div>
          <span style="font-size:11px; opacity:0.6;">●</span>
        </div>
        <div style="font-size:18px; font-weight:700; line-height:1.15;">🎵 Tumble</div>
        <div style="font-size:11px; opacity:0.65; margin-top:2px;">Set 2 · song 4</div>
      </div>
      <div style="text-align:center; font-size:9px; opacity:0.4; margin-top:6px;">BEFORE · last song frozen on screen</div>
    </div>
  </div>

  <div style="background:#0a0a0a; border-radius:36px; padding:14px; box-shadow:0 8px 24px rgba(0,0,0,0.18); flex:0 0 auto;">
    <div style="background:linear-gradient(180deg, #2a1d3a 0%, #1a0f2a 100%); border-radius:24px; width:280px; padding:14px 16px; color:#fff; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
      <div style="display:flex; justify-content:space-between; align-items:center; font-size:11px; opacity:0.7; margin-bottom:8px;"><span>10:23</span><span>📶 5G ⌁ 71%</span></div>
      <div style="background:rgba(251,146,60,0.18); border:1px solid rgba(251,146,60,0.45); border-radius:18px; padding:12px 14px; backdrop-filter:blur(12px);">
        <div style="display:flex; gap:10px; align-items:center; margin-bottom:6px;">
          <div style="width:32px; height:32px; border-radius:8px; background:linear-gradient(135deg,#fb923c,#ea580c); display:flex; align-items:center; justify-content:center; font-size:14px;">🪿</div>
          <div style="display:flex; flex-direction:column; flex:1;">
            <span style="font-size:11px; font-weight:600; color:#fb923c;">GOOSE · SET BREAK</span>
            <span style="font-size:10px; opacity:0.55;">Saenger Theatre, NOLA</span>
          </div>
          <span style="font-size:18px; color:#fb923c;">⏸</span>
        </div>
        <div style="font-size:18px; font-weight:700; line-height:1.15; color:#fb923c;">⏸️ Set break</div>
        <div style="font-size:11px; opacity:0.7; margin-top:2px;">After Set 2 · 9 songs</div>
      </div>
      <div style="text-align:center; font-size:9px; color:#fb923c; opacity:0.7; margin-top:6px;">AFTER · dedicated set break UI</div>
    </div>
  </div>
</div>

Couch tour viewers get a 30-second spoiler hide on fresh song bubbles, because the people in the building are ahead of the stream.

The bubble doesn't blur. It renders as three pulsing dots, the same way an iMessage typing indicator does. There's a reason it's that specific shape and not a frosted blur or a "?" placeholder.

The moment a song starts in the room, people start talking about it in the chat. You need a marker in the timeline so a couch viewer can see "okay, the in-venue chompers are reacting to whatever this is right now," follow the conversation in context, and not get hit with the song name as a spoiler before they've heard a note of it. The dots are that marker. When the 30 seconds is up, they flip to the song name and the chat above lines up with what the couch viewer is now hearing.

We landed on 30 by testing it live during real shows. A 4K livestream encodes in roughly that window before it reaches a couch viewer, so 30 seconds is close to the actual gap between the room and the screen. We extended this to admin-typed setlist entries too, because the setlist is now mostly admin-typed.

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:#262626; color:#fff; padding:10px 16px; font-size:11px; letter-spacing:0.04em; text-transform:uppercase; display:flex; justify-content:space-between; align-items:center;">
      <span>🪿 Goose · Live</span>
      <span style="opacity:0.65; font-weight:400; text-transform:none; letter-spacing:0;">couch view · stream behind</span>
    </div>

    <div style="padding:8px 0 14px;">
      <div style="padding:6px 14px;"><span style="font-size:13px; font-weight:600; color:#8B5CF6; background:#EDE9FE; padding:4px 12px; border-radius:12px;">🎵 Atlas Dogs</span></div>

      <div style="padding:6px 14px; display:flex; gap:10px; align-items:flex-start;">
        <div style="width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); display:flex; align-items:center; justify-content:center; font-size:14px; font-weight:700; color:#fff; flex-shrink:0;">P</div>
        <div style="display:flex; flex-direction:column; gap:2px; flex:1;">
          <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:600; font-size:14px;">patrick</span><span style="font-size:10px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#EC4899;">🎸 Show</span></div>
          <div style="font-size:14px; line-height:1.3;">opener slaps already</div>
        </div>
      </div>

      <div style="padding:6px 14px;"><span style="font-size:13px; font-weight:600; color:#8B5CF6; background:#EDE9FE; padding:4px 12px; border-radius:12px;">🎵 Tumble</span></div>

      <div style="padding:6px 14px; display:flex; gap:10px; align-items:flex-start;">
        <div style="width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#d9c6ff,#a88fe6); display:flex; align-items:center; justify-content:center; font-size:14px; font-weight:700; color:#fff; flex-shrink:0;">M</div>
        <div style="display:flex; flex-direction:column; gap:2px; flex:1;">
          <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:600; font-size:14px;">chomper2</span><span style="font-size:10px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#EC4899;">🎸 Show</span></div>
          <div style="font-size:14px; line-height:1.3;">TUMBLE!!! called it 📣</div>
        </div>
      </div>

      <div style="padding:6px 14px; display:flex; gap:10px; align-items:flex-start;">
        <div style="width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); display:flex; align-items:center; justify-content:center; font-size:14px; font-weight:700; color:#fff; flex-shrink:0;">P</div>
        <div style="display:flex; flex-direction:column; gap:2px; flex:1;">
          <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:600; font-size:14px;">patrick</span><span style="font-size:10px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#EC4899;">🎸 Show</span></div>
          <div style="font-size:14px; line-height:1.3;">31 show gap, hot 🔥</div>
        </div>
      </div>

      <div style="padding:6px 14px; display:flex; align-items:center; gap:8px;">
        <span style="font-size:13px; font-weight:600; color:#8B5CF6; background:#EDE9FE; padding:6px 14px; border-radius:14px; display:inline-flex; align-items:center; gap:4px; min-width:64px; justify-content:center;">
          <span style="width:6px; height:6px; border-radius:50%; background:#8B5CF6; opacity:0.4;"></span>
          <span style="width:6px; height:6px; border-radius:50%; background:#8B5CF6; opacity:0.7;"></span>
          <span style="width:6px; height:6px; border-radius:50%; background:#8B5CF6; opacity:1;"></span>
        </span>
        <span style="font-size:10px; color:#9CA3AF;">unblurs in 17s · in-venue is reacting</span>
      </div>

      <div style="padding:6px 14px; display:flex; gap:10px; align-items:flex-start;">
        <div style="width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); display:flex; align-items:center; justify-content:center; font-size:14px; font-weight:700; color:#fff; flex-shrink:0;">P</div>
        <div style="display:flex; flex-direction:column; gap:2px; flex:1;">
          <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:600; font-size:14px;">patrick</span><span style="font-size:10px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#EC4899;">🎸 Show</span></div>
          <div style="font-size:14px; line-height:1.3;">OH NO WAY 🤯🤯🤯</div>
        </div>
      </div>
    </div>
  </div>
</div>

Here's what happened. We started this tour pulling setlists from a couple of upstream sources that publish their own version of the setlist a few minutes after each song lands. Even when those sources are healthy, an entry usually shows up two to ten minutes after the song actually starts. For a couch viewer, that's the difference between catching the opening of a jam and finding out twenty minutes later you missed it.

So mid-tour we just stopped waiting. If you're at the show, type the song into the manage-setlist screen the moment you hear it. Patrick was doing this from the floor most nights, both at his Goose shows and as a couch viewer when I was the one at Phish. The upstream sources still run as a backstop, but the live in-app setlist is now driven by whoever is fastest in the room. Latency dropped from minutes to seconds.

## Song Calls

People watching from home love to try to guess the next song from the opening notes. You hear a couple of bars on the stream, you blurt out "MADHUVAN!", you're either a hero or you wait six seconds and pretend you didn't say anything. Until this tour the only place to do that was a group text or whoever happened to be in the room with you.

This was the feature I was most nervous to ship and most happy we did. During a live show, you can now call the next song inside the app. There's a 📣 chip on the current-song strip; tap it, type the song you think comes next, and the app fuzzy-matches against the band's catalog (so "atlas" picks up "Atlas Dogs"). Submit, and a pending pill drops below the strip. If the song you called actually plays, you get a green ✓ YOU CALLED IT pill with a confetti burst. Misses fade quietly. There's a per-show leaderboard so you can see who's hot tonight, but deliberately no global all-time ranking.

This is one of the load-bearing design principles of the whole app, so it's worth stating plainly: **we are not building Fantasy Music.** The minute you ship a season-long leaderboard, the gravitational pull of the product changes. The goal becomes winning. People start optimizing their calls, gaming the window, refreshing for stats, treating the show as input to a meta-game played somewhere outside of it. The community shrinks into a competition. The leaderboard exists, because the celebration of a correct call is part of the fun, but it lives inside the show and ends with the show. Community first, scoreboard second. Every feature in this app gets evaluated against that line:

<div style="display:flex; gap:14px; flex-wrap:wrap; justify-content:center; margin:18px auto;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); padding:14px 16px; flex:0 0 auto; max-width:340px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
    <div style="font-size:9px; font-weight:700; letter-spacing:0.08em; color:#6B7280; text-transform:uppercase; margin-bottom:8px;">Now Playing · Tap 📣 to call next</div>
    <div style="display:flex; align-items:center; gap:10px; padding:10px 12px; background:#fff; border-radius:14px;">
      <div style="font-size:22px;">🎵</div>
      <div style="flex:1; display:flex; flex-direction:column;">
        <span style="font-size:15px; font-weight:700;">All I Need</span>
        <span style="font-size:11px; color:#6B7280;">Set 2 · song 3</span>
      </div>
      <span style="font-size:13px; font-weight:700; color:#8B5CF6; background:#EDE9FE; padding:6px 12px; border-radius:14px; cursor:pointer;">📣 Call</span>
    </div>
    <div style="margin-top:10px; padding:8px 12px; background:rgba(139,92,246,0.10); border-radius:12px; font-size:12px; color:#5b21b6; display:flex; align-items:center; gap:8px;">
      <span style="font-size:14px;">⏳</span><span>Pending: <strong>Empress of Organos</strong></span>
    </div>
  </div>

  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); padding:14px 16px; flex:0 0 auto; max-width:340px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
    <div style="font-size:9px; font-weight:700; letter-spacing:0.08em; color:#6B7280; text-transform:uppercase; margin-bottom:8px;">Now Playing</div>
    <div style="display:flex; align-items:center; gap:10px; padding:10px 12px; background:#fff; border-radius:14px;">
      <div style="font-size:22px;">🎵</div>
      <div style="flex:1; display:flex; flex-direction:column;">
        <span style="font-size:15px; font-weight:700;">Empress of Organos</span>
        <span style="font-size:11px; color:#6B7280;">Set 2 · song 4</span>
      </div>
    </div>
    <div style="margin-top:10px; padding:10px 12px; background:linear-gradient(135deg,#10b981 0%,#059669 100%); border-radius:12px; color:#fff; font-size:13px; font-weight:700; display:flex; align-items:center; gap:8px; position:relative; overflow:hidden;">
      <span style="font-size:16px;">✓</span><span>YOU CALLED IT</span>
      <span style="position:absolute; right:8px; top:6px; font-size:14px;">🎉</span>
      <span style="position:absolute; right:24px; top:14px; font-size:10px;">✨</span>
      <span style="position:absolute; right:36px; top:4px; font-size:8px;">★</span>
    </div>
    <div style="margin-top:6px; font-size:11px; color:#6B7280; padding:0 4px;">📣 1 win this show · tap your badge to see history</div>
  </div>
</div>

Across the tour, **105 song calls** went out from 9 different callers. The most-called song was Factory Fiction, called eight separate times by different people across the run. It never landed. The most-correct call was Into the Myst, which hit three times.

## Chomp (Live Chat) Grew Up

The live chat is called Chomp because of course it is. It got a lot of love this tour:

- **@mention autocomplete** in the composer, with the dropdown flipping above the input on Android when there's no room below.
- **Tap a song pill** in the chat to open a stat sheet showing last played, gap, FTP info, and your personal history with the song.
- **FTP-witness pill** so when someone in the chat is seeing a song for the first time, everyone knows.
- **Heart burst animation** when someone favorites your message. This is small. It also matters.
- **Tap the purple here-now bar** to expand the full chomper roster. When more than eight people are watching, the roster scrolls instead of pushing the rest of the page off the screen.
- **Optimistic bubble insertion** so your message appears the instant you hit send, not after the round trip.
- **@mention notifications** that route to chat instead of the generic notification feed.

Plus a long tail of avatar rendering speedups (thumbnails over full-size, async decoding, parallel batch enrichment) that make the chat feel like it's keeping up with the show instead of catching up to it.

Here is roughly what the chomp looks like now during a live show, with most of those features in one frame: the purple here-now bar at the top (tap to expand the roster), a tappable song pill, an @mention rendered with its purple highlight, and a chat message that's been hearted.

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:#262626; color:#fff; padding:10px 16px; font-size:11px; letter-spacing:0.04em; text-transform:uppercase; display:flex; justify-content:space-between; align-items:center;">
      <span>🪿 Goose · Live</span>
      <span style="opacity:0.65; font-weight:400; text-transform:none; letter-spacing:0;">Saenger Theatre</span>
    </div>

    <div style="margin:10px 12px; padding:8px 12px; background:linear-gradient(135deg,#8B5CF6 0%,#7c3aed 100%); border-radius:14px; display:flex; align-items:center; gap:8px; cursor:pointer;">
      <div style="display:flex;">
        <div style="width:24px; height:24px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; display:flex; align-items:center; justify-content:center; font-size:10px; font-weight:700; color:#fff; margin-right:-8px;">P</div>
        <div style="width:24px; height:24px; border-radius:50%; background:linear-gradient(135deg,#b0eaff,#3ba8e0); border:2px solid #fff; display:flex; align-items:center; justify-content:center; font-size:10px; font-weight:700; color:#fff; margin-right:-8px;">C</div>
        <div style="width:24px; height:24px; border-radius:50%; background:linear-gradient(135deg,#d9c6ff,#a88fe6); border:2px solid #fff; display:flex; align-items:center; justify-content:center; font-size:10px; font-weight:700; color:#fff; margin-right:-8px;">M</div>
        <div style="width:24px; height:24px; border-radius:50%; background:linear-gradient(135deg,#ffc8d8,#ec4899); border:2px solid #fff; display:flex; align-items:center; justify-content:center; font-size:10px; font-weight:700; color:#fff; margin-right:-8px;">Q</div>
        <div style="width:24px; height:24px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; display:flex; align-items:center; justify-content:center; font-size:10px; font-weight:700; color:#fff;">B</div>
      </div>
      <span style="flex:1; color:#fff; font-size:12px; font-weight:600;">12 chomping right now</span>
      <span style="color:#fff; opacity:0.8; font-size:14px;">›</span>
    </div>

    <div style="padding:6px 14px;"><span style="font-size:13px; font-weight:600; color:#8B5CF6; background:#EDE9FE; padding:4px 12px; border-radius:12px; cursor:pointer;">🎵 Atlas Dogs</span></div>

    <div style="padding:6px 14px; display:flex; gap:10px; align-items:flex-start;">
      <div style="width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); display:flex; align-items:center; justify-content:center; font-size:14px; font-weight:700; color:#fff; flex-shrink:0;">P</div>
      <div style="display:flex; flex-direction:column; gap:2px; flex:1;">
        <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:600; font-size:14px;">patrick</span><span style="font-size:10px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#EC4899;">🎸 Show</span></div>
        <div style="font-size:14px; line-height:1.3;">this jam is <span style="color:#8B5CF6; font-weight:600;">unreal</span> 🔥🔥🔥</div>
        <div style="display:flex; gap:8px; margin-top:4px; align-items:center;">
          <span style="font-size:12px; padding:2px 10px; border-radius:10px; background:rgba(236,72,153,0.12); color:#EC4899; font-weight:600; cursor:pointer;">❤ 4</span>
        </div>
      </div>
    </div>

    <div style="padding:6px 14px; display:flex; gap:10px; align-items:flex-start;">
      <div style="width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#b0eaff,#3ba8e0); display:flex; align-items:center; justify-content:center; font-size:14px; font-weight:700; color:#fff; flex-shrink:0;">C</div>
      <div style="display:flex; flex-direction:column; gap:2px; flex:1;">
        <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:600; font-size:14px;">chomper1</span><span style="font-size:10px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#3B82F6;">🛋 Couch</span></div>
        <div style="font-size:14px; line-height:1.3;"><span style="color:#8B5CF6; font-weight:600; background:#EDE9FE; padding:1px 4px; border-radius:4px;">@patrick</span> agreed, this is the version we'll talk about later</div>
      </div>
    </div>

    <div style="padding:8px 14px 14px;">
      <div style="display:flex; gap:8px; align-items:center; padding:8px 12px; background:#fff; border-radius:18px; border:1px solid rgba(0,0,0,0.08);">
        <span style="font-size:14px; color:#9CA3AF; flex:1;">Say something to the chomp…</span>
        <span style="font-size:12px; font-weight:700; color:#fff; background:#8B5CF6; padding:4px 12px; border-radius:12px;">Send</span>
      </div>
    </div>
  </div>
</div>

Tap any of those purple `🎵` song pills and a stat sheet slides up. Last time the band played it, gap since, debut date and venue, and a personal strip showing what you specifically have done with the song. The one card answers the four questions every chomper asks the moment a song starts ("when did they last play this," "is this rare," "first time?", and "have I caught it"):

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:#262626; color:#fff; padding:10px 16px; font-size:11px; letter-spacing:0.04em; text-transform:uppercase; display:flex; justify-content:space-between; align-items:center;">
      <span>🎵 Song stats</span>
      <span style="opacity:0.65; font-weight:400; text-transform:none; letter-spacing:0;">tap-up sheet</span>
    </div>

    <div style="padding:18px 16px 6px;">
      <div style="display:inline-block; font-size:9px; font-weight:800; letter-spacing:0.08em; color:#fff; background:#059669; padding:3px 8px; border-radius:999px; text-transform:uppercase;">Rare</div>
      <div style="font-size:24px; font-weight:800; margin-top:8px; line-height:1.1;">Factory Fiction</div>
      <div style="font-size:11px; color:#6B7280; margin-top:2px;">Goose · 27 lifetime plays</div>
    </div>

    <div style="padding:8px 16px 8px;">
      <div style="padding:10px 12px; background:linear-gradient(135deg,#EC4899 0%,#db2777 100%); color:#fff; border-radius:12px; display:flex; align-items:center; gap:8px;">
        <span style="font-size:18px;">🎯</span>
        <div style="flex:1;"><div style="font-size:13px; font-weight:700;">You've never caught Factory Fiction live</div><div style="font-size:10px; opacity:0.85;">on your wishlist · 8 fans called it this tour, none landed</div></div>
      </div>
    </div>

    <div style="padding:0 16px 14px; display:grid; grid-template-columns:1fr 1fr; gap:8px;">
      <div style="background:#fff; padding:10px 12px; border-radius:10px;">
        <div style="font-size:9px; font-weight:700; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase;">Last played</div>
        <div style="font-size:15px; font-weight:800; margin-top:2px;">12/13/25</div>
        <div style="font-size:10px; color:#6B7280;">Goosemas · Hampton</div>
      </div>
      <div style="background:#fff; padding:10px 12px; border-radius:10px;">
        <div style="font-size:9px; font-weight:700; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase;">Show gap</div>
        <div style="font-size:15px; font-weight:800; margin-top:2px;">14 shows</div>
        <div style="font-size:10px; color:#6B7280;">since last play</div>
      </div>
      <div style="background:#fff; padding:10px 12px; border-radius:10px;">
        <div style="font-size:9px; font-weight:700; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase;">FTP</div>
        <div style="font-size:15px; font-weight:800; margin-top:2px;">Oct 9, 2016</div>
        <div style="font-size:10px; color:#6B7280;">The Hartford, CT</div>
      </div>
      <div style="background:#fff; padding:10px 12px; border-radius:10px;">
        <div style="font-size:9px; font-weight:700; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase;">All-time plays</div>
        <div style="font-size:15px; font-weight:800; margin-top:2px;">27</div>
        <div style="font-size:10px; color:#6B7280;">across all tours</div>
      </div>
    </div>

    <div style="padding:0 16px 16px;">
      <div style="padding:8px 12px; background:rgba(139,92,246,0.10); border-radius:10px; font-size:11px; color:#5b21b6; display:flex; align-items:center; gap:8px;">
        <span style="font-size:14px;">👀</span>
        <span><strong>0 people in chomp</strong> were at this song's FTP. (It was a 2016 small-club show, before most of us found the band.)</span>
      </div>
    </div>
  </div>
</div>

## Tour Stats Got Serious

Tour Stats started as a personal-only "here are some numbers about your shows" page. It is now a real product surface.

- **Cross-user view with a public/private toggle**, so you can compare your tour to your friends' tours if they've opted in.
- **Band filter on every drill-down**, so you can see your Phish stats separately from your Goose stats separately from your Max Creek stats.
- **New stat cards**: Setlist Staples (most-played songs you haven't caught yet), FTP count in the overview, Tour Completion with band names on each row, GEOGRAPHIC REACH normalized so the dup-shows-table users don't double-count states.
- **Interactive drill-downs** that take you straight to the show or song.
- **Per-band bustout threshold** so Phish rotation staples stop misflagging as 🔥 fire bustouts.
- **Past-show logging flow** with tour, festival, and event pills, plus month sub-chips. Logging shows you went to before joining is now a real path, not a chore.

We also surfaced Tour Stats in the compass (More) drawer, so people can find it.

Here's what most of the screen looks like: band pill at the top, public/private toggle, the OVERVIEW row with FTP count and Tour Completion, then the Setlist Staples card surfacing "common picks you've somehow missed" (going RSVPs only, since couch tour doesn't count against you).

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:#262626; color:#fff; padding:10px 16px; font-size:11px; letter-spacing:0.04em; text-transform:uppercase; display:flex; justify-content:space-between; align-items:center;">
      <span>🧭 Tour Stats</span>
      <span style="opacity:0.65; font-weight:400; text-transform:none; letter-spacing:0;">@cmeik</span>
    </div>

    <div style="padding:12px 14px 4px; display:flex; gap:8px; align-items:center;">
      <span style="font-size:12px; font-weight:700; padding:6px 12px; border-radius:14px; background:#8B5CF6; color:#fff;">🪿 Goose ›</span>
      <span style="flex:1;"></span>
      <span style="font-size:11px; font-weight:700; padding:5px 10px; border-radius:12px; background:#fff; border:1px solid #d1d5db; color:#6B7280;">🌐 Public</span>
    </div>

    <div style="padding:8px 14px 6px; font-size:10px; font-weight:800; letter-spacing:0.06em; color:#6B7280; text-transform:uppercase;">Overview</div>
    <div style="padding:0 14px 12px; display:grid; grid-template-columns:1fr 1fr; gap:8px;">
      <div style="background:#fff; padding:10px 12px; border-radius:12px;">
        <div style="font-size:9px; font-weight:700; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase;">🥚 FTPs</div>
        <div style="font-size:22px; font-weight:800; margin-top:2px;">37</div>
        <div style="font-size:10px; color:#6B7280;">debuts you caught live</div>
      </div>
      <div style="background:#fff; padding:10px 12px; border-radius:12px;">
        <div style="font-size:9px; font-weight:700; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase;">Shows</div>
        <div style="font-size:22px; font-weight:800; margin-top:2px;">52</div>
        <div style="font-size:10px; color:#6B7280;">across 18 venues</div>
      </div>
    </div>

    <div style="padding:0 14px 12px;">
      <div style="background:#fff; padding:10px 12px; border-radius:12px;">
        <div style="display:flex; justify-content:space-between; align-items:baseline;">
          <div style="font-size:10px; font-weight:800; letter-spacing:0.05em; color:#6B7280; text-transform:uppercase;">Tour Completion</div>
          <div style="font-size:11px; color:#6B7280;">Spring '26</div>
        </div>
        <div style="font-size:18px; font-weight:800; margin-top:4px;">9 / 14 nights</div>
        <div style="height:6px; background:#EDE9FE; border-radius:3px; margin-top:6px; overflow:hidden;">
          <div style="height:100%; width:64%; background:linear-gradient(90deg,#8B5CF6,#7c3aed);"></div>
        </div>
      </div>
    </div>

    <div style="padding:0 14px 14px;">
      <div style="background:#fff; padding:14px 14px 12px; border-radius:14px;">
        <div style="display:flex; align-items:baseline; gap:8px;">
          <span style="font-size:14px;">📌</span><span style="font-size:12px; font-weight:800; letter-spacing:0.04em; color:#8B5CF6; text-transform:uppercase;">Setlist Staples</span>
        </div>
        <div style="font-size:11px; color:#6B7280; margin-bottom:10px;">Common picks · still on your list</div>
        <div style="display:flex; flex-direction:column; gap:6px;">
          <div style="display:flex; align-items:center; gap:10px; padding:6px 10px; background:#F5F2EB; border-radius:8px;"><span style="font-size:11px; font-weight:800; color:#8B5CF6; min-width:16px;">1</span><span style="flex:1; font-size:13px; font-weight:600;">Indian River</span><span style="font-size:10px; color:#6B7280;">×117</span></div>
          <div style="display:flex; align-items:center; gap:10px; padding:6px 10px; background:#F5F2EB; border-radius:8px;"><span style="font-size:11px; font-weight:800; color:#8B5CF6; min-width:16px;">2</span><span style="flex:1; font-size:13px; font-weight:600;">Butter Rum</span><span style="font-size:10px; color:#6B7280;">×113</span></div>
          <div style="display:flex; align-items:center; gap:10px; padding:6px 10px; background:#F5F2EB; border-radius:8px;"><span style="font-size:11px; font-weight:800; color:#8B5CF6; min-width:16px;">3</span><span style="flex:1; font-size:13px; font-weight:600;">Lead the Way</span><span style="font-size:10px; color:#6B7280;">×86</span></div>
          <div style="display:flex; align-items:center; gap:10px; padding:6px 10px; background:#F5F2EB; border-radius:8px;"><span style="font-size:11px; font-weight:800; color:#8B5CF6; min-width:16px;">4</span><span style="flex:1; font-size:13px; font-weight:600;">White Lights</span><span style="font-size:10px; color:#6B7280;">×68</span></div>
          <div style="display:flex; align-items:center; gap:10px; padding:6px 10px; background:#F5F2EB; border-radius:8px;"><span style="font-size:11px; font-weight:800; color:#8B5CF6; min-width:16px;">5</span><span style="flex:1; font-size:13px; font-weight:600;">Crosseyed & Painless</span><span style="font-size:10px; color:#6B7280;">×41</span></div>
        </div>
      </div>
    </div>
  </div>
</div>

My version of this card looks nothing like the example. I've been to 85 Goose shows, so the common picks are all caught and my staples list is now entirely rare bustouts. Crosseyed & Painless is the one that still won't come.

## Today: The Tour Recap Avalanche

Today was insane. Sunday is wrap-up day, the morning after the run closed out, and we shipped the entire end-of-tour package in the span of about eighteen hours. Every show now gets a recap blurb generated from the live chat sentiment, weighted by which songs got the most love. Recaps use Opus when there's actual chat content and skip cleanly when there isn't, so we're not paying tokens to summarize empty rooms. We backfilled recaps for older shows via a CLI, which means every Goose show on the platform now has a top-level recap blurb you can read.

The Flow got a "For you" section that consolidates LIVE NOW, tonight's shows, and tour recap into one place, with sparkles. Cards for archival recordings collapse into a trending card so the feed doesn't get spammy when 40 people post the same Relisten link.

### How the bracket gets built

The end-of-tour jam tournament is downstream of the per-show recap pipeline, so the seeding isn't editorial. It's data.

For every show, we run sentiment analysis over the live chat and weight by song. A song that triggers a flurry of fire emoji and "ARE YOU KIDDING ME"s reads as a heater. A song that gets polite acknowledgment doesn't. That's how we identify the jams that actually moved the room.

Then we cross-reference each jam against historical setlist data. A song with a 31-show gap or a sub-10 lifetime play count is automatically a bustout candidate. A regularly-played rotation song needs the chat heat to carry it. The two signals combine into a per-show "jam score."

When tour wraps, we forward-link those jam scores into a tournament. The top sixteen become the bracket. Highest jam score gets the 1 seed, lowest gets the 16, and we run a March Madness style bracket: Round of 16, Quarters, Semis, Final. Voting opens for each round in sequence. The community decides the winner.

Each matchup card has an inline audio player for each side, since you obviously need to hear both jams to vote between them. No second tab, no link to chase. Two sources are wired in: a soundboard recording if you have a paid streaming subscription, or the taper recording for free if you don't. You always get audio for both. The card looks like this:

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:800px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:760px; margin:0 auto;">
    <div style="background:#262626; color:#fff; padding:10px 16px; font-size:11px; letter-spacing:0.04em; text-transform:uppercase; display:flex; justify-content:space-between; align-items:center;">
      <span>🏆 Goose Spring '26 Bracket</span>
      <span style="opacity:0.65; font-weight:400; text-transform:none; letter-spacing:0;">Round of 16 · 4 of 8 voted</span>
    </div>
    <div style="padding:10px 14px 4px; font-size:10px; font-weight:800; letter-spacing:0.06em; color:#6B7280; text-transform:uppercase;">Vote in this matchup</div>

    <div style="padding:8px 14px 14px;">
      <div style="background:#fff; border-radius:14px; padding:14px 14px 12px; box-shadow:0 1px 4px rgba(0,0,0,0.04);">
        <div style="display:grid; grid-template-columns:1fr auto 1fr; gap:8px; align-items:center;">
          <div style="display:flex; flex-direction:column; gap:2px; padding:8px 10px; background:#EDE9FE; border-radius:10px; cursor:pointer;">
            <div style="font-size:9px; font-weight:800; letter-spacing:0.05em; color:#8B5CF6;">SEED 2</div>
            <div style="font-size:14px; font-weight:700; line-height:1.2;">Tumble</div>
            <div style="font-size:10px; color:#6B7280; margin-top:2px;">4/22 · Saenger, NOLA</div>
          </div>
          <div style="font-size:11px; font-weight:800; color:#6B7280;">VS</div>
          <div style="display:flex; flex-direction:column; gap:2px; padding:8px 10px; background:#F5F2EB; border:1px dashed #d1d5db; border-radius:10px; cursor:pointer;">
            <div style="font-size:9px; font-weight:800; letter-spacing:0.05em; color:#6B7280;">SEED 7</div>
            <div style="font-size:14px; font-weight:700; line-height:1.2;">Hungersite</div>
            <div style="font-size:10px; color:#6B7280; margin-top:2px;">3/28 · Athens</div>
          </div>
        </div>

        <div style="margin-top:10px; padding:8px 10px; background:#F5F2EB; border-radius:10px; display:flex; align-items:center; gap:10px;">
          <span style="width:30px; height:30px; border-radius:50%; background:#262626; color:#fff; display:flex; align-items:center; justify-content:center; font-size:13px; flex-shrink:0;">▶</span>
          <div style="display:flex; flex-direction:column; gap:2px; flex:1; min-width:0;">
            <span style="font-size:11px; font-weight:700;">🎵 Tumble · 4/22 NOLA · soundboard</span>
            <div style="display:flex; align-items:center; gap:6px;">
              <span style="font-size:9px; color:#6B7280;">3:14</span>
              <div style="height:3px; background:rgba(0,0,0,0.08); border-radius:2px; flex:1; overflow:hidden;"><div style="height:100%; width:18%; background:#8B5CF6;"></div></div>
              <span style="font-size:9px; color:#6B7280;">17:42</span>
            </div>
          </div>
        </div>

        <div style="margin-top:6px; padding:8px 10px; background:#F5F2EB; border-radius:10px; display:flex; align-items:center; gap:10px;">
          <span style="width:30px; height:30px; border-radius:50%; background:#fff; border:2px solid #262626; color:#262626; display:flex; align-items:center; justify-content:center; font-size:13px; flex-shrink:0;">▶</span>
          <div style="display:flex; flex-direction:column; gap:2px; flex:1; min-width:0;">
            <span style="font-size:11px; font-weight:700;">🎵 Hungersite · 3/28 Athens · taper</span>
            <div style="display:flex; align-items:center; gap:6px;">
              <span style="font-size:9px; color:#6B7280;">0:00</span>
              <div style="height:3px; background:rgba(0,0,0,0.08); border-radius:2px; flex:1; overflow:hidden;"><div style="height:100%; width:0%; background:#8B5CF6;"></div></div>
              <span style="font-size:9px; color:#6B7280;">12:08</span>
            </div>
          </div>
        </div>

        <div style="margin-top:10px; display:flex; align-items:center; gap:8px;">
          <div style="display:flex;">
            <div style="width:20px; height:20px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-6px;">P</div>
            <div style="width:20px; height:20px; border-radius:50%; background:linear-gradient(135deg,#b0eaff,#3ba8e0); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-6px;">C</div>
            <div style="width:20px; height:20px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">B</div>
          </div>
          <span style="font-size:10px; color:#6B7280;">3 voted for Tumble · 1 for Hungersite</span>
        </div>
      </div>
    </div>

    <div style="padding:6px 14px 4px; font-size:10px; font-weight:800; letter-spacing:0.06em; color:#6B7280; text-transform:uppercase;">Full bracket</div>
    <div style="padding:6px 14px 18px; display:grid; grid-template-columns:1.4fr 1fr 1fr 0.8fr; gap:8px; overflow-x:auto;">

      <!-- Round of 16 (8 matchups) -->
      <div style="display:flex; flex-direction:column; gap:6px;">
        <div style="font-size:8px; font-weight:800; letter-spacing:0.06em; color:#9CA3AF; text-transform:uppercase;">Round of 16</div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">(1) Madhuvan ✓</div><div style="color:#6B7280;">vs Pancakes</div></div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px; border:2px solid #8B5CF6;"><div style="font-weight:700;">(2) Tumble · 4/22</div><div style="color:#8B5CF6; font-weight:600;">vs (7) Hungersite — voting</div></div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">(3) Atlas Dogs ✓</div><div style="color:#6B7280;">vs (6) Empress</div></div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">(4) Into the Myst ✓</div><div style="color:#6B7280;">vs Doobie</div></div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">(5) All I Need</div><div style="color:#6B7280;">vs (12) Borne — voting</div></div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">(6) Arrow ✓</div><div style="color:#6B7280;">vs Travelers</div></div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">(7) Echo of a Rose</div><div style="color:#6B7280;">vs Yeti — voting</div></div>
        <div style="background:#fff; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">(8) Indian River ✓</div><div style="color:#6B7280;">vs Creatures</div></div>
      </div>

      <!-- Quarters -->
      <div style="display:flex; flex-direction:column; gap:6px; justify-content:space-around;">
        <div style="font-size:8px; font-weight:800; letter-spacing:0.06em; color:#9CA3AF; text-transform:uppercase;">Quarters</div>
        <div style="background:#EDE9FE; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">Madhuvan</div><div style="color:#6B7280;">vs winner</div></div>
        <div style="background:#EDE9FE; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">Atlas Dogs</div><div style="color:#6B7280;">vs Into the Myst</div></div>
        <div style="background:#EDE9FE; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">TBD</div><div style="color:#6B7280;">vs Arrow</div></div>
        <div style="background:#EDE9FE; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="font-weight:700;">TBD</div><div style="color:#6B7280;">vs Indian River</div></div>
      </div>

      <!-- Semis -->
      <div style="display:flex; flex-direction:column; gap:6px; justify-content:space-around;">
        <div style="font-size:8px; font-weight:800; letter-spacing:0.06em; color:#9CA3AF; text-transform:uppercase;">Semis</div>
        <div style="background:#FCE7F3; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="color:#6B7280;">TBD</div><div style="font-weight:700;">vs TBD</div></div>
        <div style="background:#FCE7F3; padding:6px 8px; border-radius:6px; font-size:10px;"><div style="color:#6B7280;">TBD</div><div style="font-weight:700;">vs TBD</div></div>
      </div>

      <!-- Final -->
      <div style="display:flex; flex-direction:column; gap:6px; justify-content:center;">
        <div style="font-size:8px; font-weight:800; letter-spacing:0.06em; color:#9CA3AF; text-transform:uppercase;">Final</div>
        <div style="background:#FEF3C7; padding:10px 8px; border-radius:6px; font-size:10px; text-align:center;"><div style="font-size:18px; margin-bottom:2px;">🏆</div><div style="color:#6B7280;">Jam of the Tour</div><div style="font-weight:700; font-size:11px; margin-top:2px;">?</div></div>
      </div>

    </div>
  </div>
</div>

The community is voting now.

The reason the bracket matters more than it might first appear is that the gap between tours is where most music apps die. The tour ends, the chat empties out, the lock-screen Live Activity goes dark, and people drift back to their normal feeds until the next run is announced. The jam tournament is a deliberate counter to that. Voting runs across multiple weeks, the rounds release on a schedule, and every matchup card pulls a real audio clip from a real show people went to, so listening to the bracket is also re-listening to the tour. The conversation in the chat doesn't end when the lights come up in Irving. It keeps going through Round of 16, Quarters, Semis, Final, and by the time we crown a Jam of the Tour, the next run is already on the calendar and the muscle memory of opening the app every day is intact. The bracket is the bridge.

The "all bands" UX got a final shape today too: the band dropdown collapsed into a BANDS card on Tour Stats, the All Bands directory got tuned-in count parity with band pages, Dead & Company switched to a "📜 Setlist archive" tour-status because they're done touring, Billy Strings got a full historical import and a live setlist source, and a bunch of polish on Tour Completion, soundcheck-row dedup, encore-vs-Set-3 labeling, miracle ticket cards, and the rest of the long tail.

The honest version of "today" is that we picked Sunday for the avalanche because the tour had just wrapped, the chat was still active, and any new bug would surface immediately. It worked. We shipped a thing, watched the chat react, and either fixed it or moved on inside an hour. Cycle repeated forty-ish times.

## So Many Bands

This was the tour where Zabriskie stopped being a Goose-and-Phish app.

We added: **Trey Anastasio Band 🎺, Mike Gordon Band 🌵, Umphrey's McGee 🧢, Dead & Company 🌹, Billy Strings 🪕, Max Creek 🐥, King Gizzard & the Lizard Wizard 🧙, Radiohead 🐻, My Morning Jacket, Spafford, Dogs in a Pile, and Daniel Donato's Cosmic Country.** Phish got rebranded to ⭕ and JRAD to ⚡ along the way.

For each band we did the full thing: found a source for the historical setlists and backfilled them, wired up the band page with My Recent and Recent Shows with inline-expand setlists, and added it to the All Bands directory so people could actually find it. Dead & Company got the archival treatment because the band is done touring, and "Tune in" doesn't make sense for a band you can't tune into.

We also built a "Request a band" escape hatch in onboarding for everyone whose band still isn't here.

## Onboarding Stopped Being a Wall

Onboarding got rebuilt around a simple idea: get to value in the first session. New users now see a first-run, show-aware prompt on `/start` that adapts to what's happening that night. There's a "Log past shows" step with a dedicated band picker, a tune-in tap with an explainer for what tuning in actually does, and a value-prop card that explains what the app is in one screen:

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:linear-gradient(135deg,#8B5CF6 0%,#7c3aed 100%); color:#fff; padding:16px 18px;">
      <div style="font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; opacity:0.85;">Step 2 of 4</div>
      <div style="font-size:20px; font-weight:800; margin-top:6px; line-height:1.15;">Which bands do you follow?</div>
      <div style="font-size:12px; opacity:0.85; margin-top:4px;">Tune in to get setlists, live chat, and recap on every show.</div>
    </div>

    <div style="padding:14px;">
      <div style="display:flex; flex-direction:column; gap:8px;">
        <div style="display:flex; align-items:center; gap:12px; padding:10px 12px; background:#fff; border-radius:12px; border:2px solid #8B5CF6;">
          <div style="width:34px; height:34px; border-radius:10px; background:linear-gradient(135deg,#a855f7,#7c3aed); display:flex; align-items:center; justify-content:center; font-size:18px;">🪿</div>
          <div style="flex:1;"><div style="font-size:14px; font-weight:700;">Goose</div><div style="font-size:11px; color:#6B7280;">14 shows this tour · 12 chomping tonight</div></div>
          <span style="font-size:11px; font-weight:800; padding:5px 10px; border-radius:10px; background:#8B5CF6; color:#fff;">✓ Tuned in</span>
        </div>
        <div style="display:flex; align-items:center; gap:12px; padding:10px 12px; background:#fff; border-radius:12px;">
          <div style="width:34px; height:34px; border-radius:10px; background:linear-gradient(135deg,#fb7185,#e11d48); display:flex; align-items:center; justify-content:center; font-size:18px;">⭕</div>
          <div style="flex:1;"><div style="font-size:14px; font-weight:700;">Phish</div><div style="font-size:11px; color:#6B7280;">At Sphere tonight · couch tour live</div></div>
          <span style="font-size:11px; font-weight:700; padding:5px 10px; border-radius:10px; background:#EDE9FE; color:#8B5CF6;">+ Tune in</span>
        </div>
        <div style="display:flex; align-items:center; gap:12px; padding:10px 12px; background:#fff; border-radius:12px;">
          <div style="width:34px; height:34px; border-radius:10px; background:linear-gradient(135deg,#fde68a,#f59e0b); display:flex; align-items:center; justify-content:center; font-size:18px;">🌹</div>
          <div style="flex:1;"><div style="font-size:14px; font-weight:700;">Dead & Company</div><div style="font-size:11px; color:#6B7280;">📜 Setlist archive · no upcoming shows</div></div>
          <span style="font-size:11px; font-weight:700; padding:5px 10px; border-radius:10px; background:#EDE9FE; color:#8B5CF6;">+ Tune in</span>
        </div>
        <div style="display:flex; align-items:center; gap:12px; padding:10px 12px; background:#fff; border-radius:12px;">
          <div style="width:34px; height:34px; border-radius:10px; background:linear-gradient(135deg,#a7f3d0,#10b981); display:flex; align-items:center; justify-content:center; font-size:18px;">🪕</div>
          <div style="flex:1;"><div style="font-size:14px; font-weight:700;">Billy Strings</div><div style="font-size:11px; color:#6B7280;">On tour · next show 4/29</div></div>
          <span style="font-size:11px; font-weight:700; padding:5px 10px; border-radius:10px; background:#EDE9FE; color:#8B5CF6;">+ Tune in</span>
        </div>
      </div>

      <div style="margin-top:14px; padding:10px 12px; background:rgba(139,92,246,0.08); border-radius:10px; font-size:11px; color:#5b21b6; line-height:1.4;">
        <strong>Don't see your band?</strong> Tap below to request one and we'll get the full show history wired up.
      </div>
      <div style="margin-top:8px; font-size:11px; font-weight:700; color:#8B5CF6; text-align:center; padding:6px;">Request a band →</div>

      <div style="margin-top:8px; display:flex; gap:8px;">
        <div style="flex:1; padding:12px; background:#fff; border:1px solid #d1d5db; border-radius:12px; text-align:center; font-size:13px; font-weight:700; color:#6B7280;">Skip</div>
        <div style="flex:2; padding:12px; background:#8B5CF6; border-radius:12px; text-align:center; font-size:13px; font-weight:700; color:#fff;">Continue →</div>
      </div>
    </div>
  </div>
</div>

We also widened the gate for who can sign up, because the Spring tour brought a lot of new people in.

A big part of why onboarding actually got better instead of just getting a redesign is that Patrick was on the ground at every Goose show, putting the app in front of people he met at the bar, on the lot, in the seats next to him. He'd watch a brand-new user open the app cold, see exactly where they got stuck or confused, take notes on the friction in real time, and then file the fixes from the passenger seat on the drive to the next city. The "Log past shows" step, the explainer on what tuning in actually does, the value-prop card, the "Request a band" escape hatch, all of those came out of that loop. Not speculative redesigns. Each one came directly from watching a real person fail at the previous version, with the new version landing before the next show.

## Goose Mode

We started building dedicated per-band "modes" this tour, basically a tour companion dashboard tailored to one band at a time. Both Goose Mode and Phish Mode shipped during the run. Each one knows its band's calendar, color palette, and ritual vocabulary, and surfaces what matters for that specific community.

The centerpiece is the Tour Timeline with a live countdown to the next show. Past shows you went to are checked off. Upcoming shows show date, weather, and which of your friends are going. The whole timeline is annotated with your crew dripping in and out of the run, so you can see at a glance who joined for which leg, who left after the southeast swing, who flew in for the closer. The countdown ticks every second:

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:linear-gradient(135deg,#a855f7 0%,#7c3aed 100%); color:#fff; padding:14px 16px;">
      <div style="display:flex; align-items:center; gap:10px;">
        <div style="width:36px; height:36px; border-radius:10px; background:rgba(255,255,255,0.2); display:flex; align-items:center; justify-content:center; font-size:20px;">🪿</div>
        <div style="flex:1;"><div style="font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; opacity:0.85;">Goose Mode</div><div style="font-size:16px; font-weight:800;">Spring '26 · Texas Run</div></div>
      </div>
    </div>

    <div style="padding:14px 14px 6px; text-align:center; background:#fff;">
      <div style="font-size:9px; font-weight:800; letter-spacing:0.08em; color:#9CA3AF; text-transform:uppercase;">Tonight · doors in</div>
      <div style="display:flex; justify-content:center; gap:8px; margin-top:6px;">
        <div style="background:#F5F2EB; padding:6px 10px; border-radius:8px; min-width:46px;"><div style="font-size:22px; font-weight:800; color:#7c3aed;">01</div><div style="font-size:9px; color:#6B7280; font-weight:700; letter-spacing:0.05em;">HRS</div></div>
        <div style="background:#F5F2EB; padding:6px 10px; border-radius:8px; min-width:46px;"><div style="font-size:22px; font-weight:800; color:#7c3aed;">23</div><div style="font-size:9px; color:#6B7280; font-weight:700; letter-spacing:0.05em;">MIN</div></div>
        <div style="background:#F5F2EB; padding:6px 10px; border-radius:8px; min-width:46px;"><div style="font-size:22px; font-weight:800; color:#7c3aed;">04</div><div style="font-size:9px; color:#6B7280; font-weight:700; letter-spacing:0.05em;">SEC</div></div>
      </div>
      <div style="font-size:11px; color:#6B7280; margin-top:8px;">Bayou Music Center · Houston · 5 in your crew going</div>
    </div>

    <div style="padding:14px 14px 4px; font-size:10px; font-weight:800; letter-spacing:0.06em; color:#6B7280; text-transform:uppercase;">Tour Timeline · Crew</div>

    <div style="padding:0 14px 14px; display:flex; flex-direction:column; gap:6px;">

      <div style="display:flex; gap:10px; align-items:flex-start; padding:10px 12px; background:#fff; border-radius:10px; opacity:0.55;">
        <div style="display:flex; flex-direction:column; align-items:center; min-width:36px; padding-top:2px;"><span style="font-size:16px;">✓</span><span style="font-size:9px; color:#6B7280;">4/19</span></div>
        <div style="flex:1;">
          <div style="font-size:13px; font-weight:600;">St. Augustine Amphitheatre</div>
          <div style="font-size:10px; color:#6B7280;">St. Augustine · couch toured</div>
          <div style="display:flex; align-items:center; gap:6px; margin-top:6px;">
            <div style="display:flex;">
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">P</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">B</div>
            </div>
            <span style="font-size:9px; color:#6B7280;">2 in crew · Patrick joined the run here</span>
          </div>
        </div>
      </div>

      <div style="display:flex; gap:10px; align-items:flex-start; padding:10px 12px; background:#fff; border-radius:10px; opacity:0.55;">
        <div style="display:flex; flex-direction:column; align-items:center; min-width:36px; padding-top:2px;"><span style="font-size:16px;">✓</span><span style="font-size:9px; color:#6B7280;">4/21</span></div>
        <div style="flex:1;">
          <div style="font-size:13px; font-weight:600;">Saenger Theatre</div>
          <div style="font-size:10px; color:#6B7280;">New Orleans · couch toured</div>
          <div style="display:flex; align-items:center; gap:6px; margin-top:6px;">
            <div style="display:flex;">
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">P</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#b0eaff,#3ba8e0); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">C</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">B</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffc8d8,#ec4899); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">M</div>
            </div>
            <span style="font-size:9px; color:#6B7280;">4 in crew · Mwat & C joined</span>
          </div>
        </div>
      </div>

      <div style="display:flex; gap:10px; align-items:flex-start; padding:10px 12px; background:#fff; border-radius:10px; opacity:0.55;">
        <div style="display:flex; flex-direction:column; align-items:center; min-width:36px; padding-top:2px;"><span style="font-size:16px;">✓</span><span style="font-size:9px; color:#6B7280;">4/22</span></div>
        <div style="flex:1;">
          <div style="font-size:13px; font-weight:600;">Saenger Theatre</div>
          <div style="font-size:10px; color:#6B7280;">New Orleans · couch toured · Tumble jam ranked #2</div>
          <div style="display:flex; align-items:center; gap:6px; margin-top:6px;">
            <div style="display:flex;">
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">P</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#b0eaff,#3ba8e0); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">C</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">B</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffc8d8,#ec4899); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">M</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#fde68a,#f59e0b); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">G</div>
            </div>
            <span style="font-size:9px; color:#6B7280;">5 in crew · Gmart joined for Texas leg</span>
          </div>
        </div>
      </div>

      <div style="display:flex; gap:10px; align-items:flex-start; padding:12px 12px; background:#fff; border-radius:10px; border:2px solid #7c3aed;">
        <div style="display:flex; flex-direction:column; align-items:center; min-width:36px; padding-top:2px;"><span style="font-size:18px;">●</span><span style="font-size:9px; color:#7c3aed; font-weight:800;">4/23</span></div>
        <div style="flex:1;">
          <div style="font-size:13px; font-weight:700;">Bayou Music Center · <span style="color:#7c3aed;">tonight</span></div>
          <div style="font-size:10px; color:#6B7280;">Houston · 79°F clear · you're couch touring</div>
          <div style="display:flex; align-items:center; gap:6px; margin-top:6px;">
            <div style="display:flex;">
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">P</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">B</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#fde68a,#f59e0b); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">G</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#d9c6ff,#a88fe6); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">L</div>
            </div>
            <span style="font-size:9px; color:#6B7280;">4 in crew · C left after NOLA · Lubby joined</span>
          </div>
        </div>
        <span style="font-size:11px; font-weight:800; padding:5px 10px; border-radius:10px; background:#7c3aed; color:#fff;">RSVP'd</span>
      </div>

      <div style="display:flex; gap:10px; align-items:flex-start; padding:10px 12px; background:#fff; border-radius:10px;">
        <div style="display:flex; flex-direction:column; align-items:center; min-width:36px; padding-top:2px;"><span style="font-size:14px; color:#9CA3AF;">○</span><span style="font-size:9px; color:#6B7280;">4/24</span></div>
        <div style="flex:1;">
          <div style="font-size:13px; font-weight:600;">Moody Center</div>
          <div style="font-size:10px; color:#6B7280;">Austin · in 1 day</div>
          <div style="display:flex; align-items:center; gap:6px; margin-top:6px;">
            <div style="display:flex;">
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">P</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#fde68a,#f59e0b); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">G</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#d9c6ff,#a88fe6); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">L</div>
            </div>
            <span style="font-size:9px; color:#6B7280;">3 in crew going · B sitting it out</span>
          </div>
        </div>
      </div>

      <div style="display:flex; gap:10px; align-items:flex-start; padding:10px 12px; background:#fff; border-radius:10px;">
        <div style="display:flex; flex-direction:column; align-items:center; min-width:36px; padding-top:2px;"><span style="font-size:14px; color:#9CA3AF;">○</span><span style="font-size:9px; color:#6B7280;">4/25</span></div>
        <div style="flex:1;">
          <div style="font-size:13px; font-weight:600;">Toyota Music Factory</div>
          <div style="font-size:10px; color:#6B7280;">Irving · in 2 days · last night of Spring run</div>
          <div style="display:flex; align-items:center; gap:6px; margin-top:6px;">
            <div style="display:flex;">
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">P</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#a7f3d0,#10b981); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">B</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#fde68a,#f59e0b); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">G</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#d9c6ff,#a88fe6); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; margin-right:-5px;">L</div>
              <div style="width:18px; height:18px; border-radius:50%; background:linear-gradient(135deg,#ffc8d8,#ec4899); border:2px solid #fff; font-size:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700;">Q</div>
            </div>
            <span style="font-size:9px; color:#6B7280;">5 in crew · Q flew in for the closer</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</div>

## Miracle Tickets

A long jam band tradition: someone has an extra ticket and gives it away, no questions, sometimes for nothing, sometimes for a smile. We built that into the app as a first-class feature this tour. If you have an extra, you post it as a Miracle Ticket attached to the show. People can request it. The owner draws a winner based on engagement (so the lurker who never participates doesn't beat the regular who's been in the chat all run). Any admin can also award on the owner's behalf if they're not online. Cards are collapsible and grouped by show, so on a busy night the feed doesn't drown:

<div style="background:#e5e2d9; padding:18px; border-radius:14px; margin:16px auto; max-width:520px; font-family:-apple-system,BlinkMacSystemFont,system-ui,sans-serif; color:#262626;">
  <div style="background:#F5F2EB; border-radius:18px; box-shadow:0 6px 20px rgba(0,0,0,0.08); overflow:hidden; max-width:380px; margin:0 auto;">
    <div style="background:#262626; color:#fff; padding:10px 16px; font-size:11px; letter-spacing:0.04em; text-transform:uppercase; display:flex; justify-content:space-between; align-items:center;">
      <span>☝️ 🎫 Miracle Tickets</span>
      <span style="opacity:0.65; font-weight:400; text-transform:none; letter-spacing:0;">2 open</span>
    </div>

    <div style="padding:12px 14px;">
      <div style="background:#fff; border-radius:14px; padding:14px 14px 12px; box-shadow:0 1px 4px rgba(0,0,0,0.04);">
        <div style="display:flex; gap:10px; align-items:center; margin-bottom:10px;">
          <div style="width:38px; height:38px; border-radius:10px; background:linear-gradient(135deg,#a855f7,#7c3aed); display:flex; align-items:center; justify-content:center; font-size:18px;">🪿</div>
          <div style="flex:1;"><div style="font-size:13px; font-weight:700;">Goose · 4/22</div><div style="font-size:10px; color:#6B7280;">Saenger Theatre · New Orleans</div></div>
          <span style="font-size:10px; padding:3px 8px; border-radius:8px; background:#FEF3C7; color:#92400E; font-weight:700;">OPEN</span>
        </div>
        <div style="font-size:13px; line-height:1.4; color:#262626; padding:10px 12px; background:#F5F2EB; border-radius:10px;">"Have an extra GA, can meet at the box office at 7. Just want it to go to someone who'll love it. 🌹"</div>
        <div style="display:flex; align-items:center; gap:8px; margin-top:10px;">
          <div style="width:24px; height:24px; border-radius:50%; background:linear-gradient(135deg,#ffd6b0,#f59e47); display:flex; align-items:center; justify-content:center; font-size:11px; font-weight:700; color:#fff;">P</div>
          <span style="font-size:11px; color:#6B7280;">posted by patrick · 2h ago · 7 entries</span>
        </div>
        <div style="display:flex; gap:6px; margin-top:10px;">
          <div style="flex:1; padding:8px; background:#fff; border:1px solid #d1d5db; border-radius:10px; text-align:center; font-size:12px; font-weight:700; color:#6B7280;">View entries</div>
          <div style="flex:1; padding:8px; background:#EC4899; border-radius:10px; text-align:center; font-size:12px; font-weight:700; color:#fff;">🙏 Enter to win</div>
        </div>
      </div>
    </div>

    <div style="padding:0 14px 14px;">
      <div style="background:#fff; border-radius:14px; padding:14px 14px 12px; box-shadow:0 1px 4px rgba(0,0,0,0.04); opacity:0.7;">
        <div style="display:flex; gap:10px; align-items:center; margin-bottom:8px;">
          <div style="width:38px; height:38px; border-radius:10px; background:linear-gradient(135deg,#fb7185,#e11d48); display:flex; align-items:center; justify-content:center; font-size:18px;">⭕</div>
          <div style="flex:1;"><div style="font-size:13px; font-weight:700;">Phish · 4/23 · Sphere</div><div style="font-size:10px; color:#6B7280;">Las Vegas · awarded</div></div>
          <span style="font-size:10px; padding:3px 8px; border-radius:8px; background:#D1FAE5; color:#065F46; font-weight:700;">CLOSED</span>
        </div>
        <div style="display:flex; align-items:center; gap:8px; font-size:11px; color:#6B7280;">
          <span style="font-size:14px;">🎉</span>
          <span>went to <strong style="color:#262626;">@chomper4</strong> · drawn by engagement</span>
        </div>
      </div>
    </div>
  </div>
</div>

## Everything Else Worth Mentioning

- **Seat sharing.** A discovery screen, show-card badges, avatar-x to unshare, per-show seat sharing with @mentions in the crew chat, and seat-share notifications that show the seat inline instead of dumping into live chat.
- **Live Recording posts** with a smart provider cascade and an autocomplete dropdown for the artist field.
- **Show notes** on every expanded setlist, not just Chomp. Plus a Show Notes editor on manage-setlist for admins.
- **The Live tab.** Promoted out of the compass into its own dedicated nav slot, with an always-open compass replacing it.

## What the Chat Said at the End

There was a moment in Irving last night, near the encore, that I want to keep. I'm pulling these straight from the chat, anonymized:

> *(Patrick)* Man. end of tour is always so bittersweet. Great run of shows though. Damn, Goose, ya got me good
>
> *(a chomper)* this app is the bees. I am very grateful for it.
>
> *(another chomper)* Thank you for the invite. It's a super cool concept and appreciate all your efforts!
>
> *(another)* Awesome show, thanks for the hangs. We will hopefully see some of you in Toronto!

That is the whole point. That is the third place. People who started the tour as usernames in a beta group, sending each other Eminence-chase updates and bustout calls and gummy timing notes, ending the tour planning to meet in Toronto and trading European tour stops. One of them listed his run: Brixton, Brussels, Amsterdam, Paris. Patrick offered to add him to a crew. Real people, real plans, made through an app we started building eight weeks ago.

## What This Tour Taught Me

- **The app holds up under live load now.** Since I [wrote about the iOS 26 .then() proxy disaster]({% post_url 2026-03-29-the-show-is-happening-right-now-and-nothing-works %}), Live Activities have stayed up through every single show. Nobody filed a "the lock screen is dead" bug for the rest of the tour. That's the most boring victory of the month, and it's the one I'm proudest of.
- **Two shows a night is a cheat code.** Goose on Eastern from the hotel couch, Phish at Sphere on Pacific from my seat. Patrick on the ground at Goose, me at Phish, both of us in the chomp on whichever show wasn't ours. Anything that broke at 9pm Eastern got fixed before the next song started at Phish. You cannot manufacture that kind of feedback loop on purpose. We got lucky with the calendar and we used it.
- **The community runs faster than the app.** Song calls is the clearest example I can point to: on 4/19 a chomper said in chat, "Would be cool if you could program it to where when one of us guesses the song in shows everyone fun Lil game. Idk how hard that would be to do tho hahs." Five days later that feature shipped end to end. The whole tour was full of small versions of this loop, where someone says "we need X" in the chat and a few days later a version of X is in the app. Building this with the people who use it is dramatically faster than building it from a spec.
- **Three hundred PRs in a month is not normal and I should not pretend it is.** Most of the code was written by Claude. A lot of it had to be re-written by Claude after I caught it doing something wrong. Every sharp edge from the tour got logged into the agent reliability dataset I've been building, and I'll keep writing about it separately. The point of building in public with an AI assistant is that the failures are part of the dataset, not embarrassments to hide.

Goose Cabo is in a few weeks. Goose Summer is right after. I have a list. The app is not done.

In the meantime, if you were on tour, thanks for chomping. If you were at home, thanks for being in the chat. If you've never tried the app and the third place I keep talking about sounds like something you'd want, come find us. The next show is already on the calendar.

---

*One housekeeping note: the screens in this post are mockups, not actual app screenshots, drawn to make each feature legible in context. The real app looks slightly different on iOS vs. Android (Live Activities vs. ongoing notifications, system fonts, badge styling, the typing indicator's exact pulse), and the mockups smooth those over for readability. The features themselves all shipped.*
