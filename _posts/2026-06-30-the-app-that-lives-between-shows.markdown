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

There was also a real deadline this time. Goose put out a new album, **Big Modern!**, and took it on the road this summer. The album drop and the tour behind it are the biggest moment a band's community gets all year, and I wanted every surface of the app ready for it: the live chat, the show page, the home screen, the band's whole history one tap away. So a lot of what follows is us getting the room ready for the party. And when the Big Modern! tour actually started in June, the room filled. I will show you the chat numbers, because they are the ones I am proudest of.

Start with the room everyone actually lives in during a show: the chat.

## The Live Chat Got Rebuilt, and It Is Where Everything Happens

During a show the chat is called the Chomp, and it is the beating heart of the whole app. This window we tore it down to the studs and rebuilt it as **Live Chat V2**: warmer, bigger, more cinematic, and organized around one new idea, the **reaction heatbar**.

The heatbar sits above the chat and shows the room temperature. It counts the reactions flying right now, so you can feel a jam landing before you have read a single word. It turns out a lot of people want to be in the room without composing a sentence, and a tap of 🔥 is a much lower bar than typing. Around it we shipped set markers ("🎼 Set 1 begins," a "Set break" vote chip), duration-sized song progress bars, role badges, a spoiler-safe delay for couch viewers so the people in the building do not ruin the song for the people on the stream, and, because we are on the App Store now, report-and-block moderation on every message.

<div style="color-scheme:light; background:#E7E2D6; padding:16px; border-radius:20px; margin:16px auto; max-width:420px;">
  <div style="background:#F0EDE4; border-radius:20px; box-shadow:0 12px 34px rgba(20,18,30,0.14); overflow:hidden; max-width:390px; margin:0 auto; color:#2A2A3A; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
    <div style="position:relative; padding:12px 16px; background:radial-gradient(circle at 20% 20%, #E83A73, #2A5FAA 120%); color:#fff;">
      <div style="position:absolute; inset:0; opacity:0.12; mix-blend-mode:overlay; background-image:url(&quot;data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E&quot;); background-size:130px 130px;"></div>
      <div style="position:relative; display:flex; align-items:center; justify-content:space-between;">
        <div style="display:flex; align-items:center; gap:8px;"><span style="width:8px; height:8px; border-radius:50%; background:#EF4444;"></span><span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px; font-weight:700; letter-spacing:0.14em;">GOOSE · LIVE</span></div>
        <span style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:15px; opacity:0.92;">The Cap · Set 2</span>
      </div>
    </div>

    <!-- reaction heatbar (room temperature) -->
    <div style="display:flex; align-items:center; gap:8px; padding:9px 14px; background:rgba(232,58,115,0.06); border-bottom:1px solid rgba(42,42,58,0.06);">
      <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:700; letter-spacing:0.12em; text-transform:uppercase; color:#9A9AAA;">Room temp</span>
      <span style="font-size:13px;">🔥 <b style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px;">41</b></span>
      <span style="font-size:13px;">🤯 <b style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px;">18</b></span>
      <span style="font-size:13px;">🕺 <b style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px;">12</b></span>
      <span style="font-size:13px;">🎷 <b style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px;">7</b></span>
      <span style="flex:1;"></span>
      <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:10px; color:#E83A73; font-weight:700;">🌡 boiling</span>
    </div>

    <div style="padding:8px 0 8px;">
      <div style="padding:6px 14px;"><span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px; font-weight:600; letter-spacing:0.04em; color:#2A5FAA; background:rgba(42,95,170,0.10); padding:4px 12px; border-radius:12px;">🎼 Set 2 begins</span></div>
      <div style="padding:4px 14px 2px;"><span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:13px; font-weight:600; color:#E83A73; background:rgba(232,58,115,0.10); padding:4px 12px; border-radius:12px;">🎵 Madhuvan</span> <span style="font-size:11px; color:#9A9AAA;">↩ 6-show gap</span></div>

      <div style="padding:8px 14px; display:flex; gap:10px; align-items:flex-start;">
        <div style="width:32px; height:32px; border-radius:50%; background:linear-gradient(135deg,#F2A83B,#E83A73); display:flex; align-items:center; justify-content:center; font-size:13px; font-weight:700; color:#fff; flex-shrink:0;">P</div>
        <div>
          <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:700; font-size:14px;">patrick</span><span style="font-size:9px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#E83A73;">🎸 Show</span></div>
          <div style="font-size:14px; line-height:1.35;">HERE IT IS. this is the one 🔥🔥🔥</div>
          <div style="margin-top:4px;"><span style="font-size:11px; padding:2px 9px; border-radius:10px; background:rgba(232,58,115,0.10); color:#E83A73; font-weight:700;">🔥 9</span></div>
        </div>
      </div>

      <div style="padding:4px 14px; display:flex; gap:10px; align-items:flex-start;">
        <div style="width:32px; height:32px; border-radius:50%; background:linear-gradient(135deg,#3AC4E8,#2A5FAA); display:flex; align-items:center; justify-content:center; font-size:13px; font-weight:700; color:#fff; flex-shrink:0;">C</div>
        <div>
          <div style="display:flex; gap:6px; align-items:center;"><span style="font-weight:700; font-size:14px;">chomper1</span><span style="font-size:9px; padding:2px 6px; border-radius:8px; color:#fff; font-weight:700; background:#3AC4E8;">🛋 Couch</span></div>
          <div style="font-size:14px; line-height:1.35;">YOOO CHOMPERS 🎸🤝🛋️ nugs is 40s behind, no spoilers</div>
        </div>
      </div>
    </div>

    <div style="padding:8px 14px 14px;">
      <div style="display:flex; gap:8px; align-items:center; padding:9px 12px; background:#FEFDFB; border-radius:16px; border:1px solid rgba(42,42,58,0.08);">
        <span style="font-size:14px; color:#9A9AAA; flex:1; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">Say something to the chomp…</span>
        <span style="font-size:12px; font-weight:700; color:#fff; background:#E83A73; padding:5px 14px; border-radius:12px; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">Send</span>
      </div>
    </div>
  </div>
</div>

One detail I love more than I expected to: the reactions are **band-specific.** The palette changes depending on whose show you are in. Walk into a Goose chat and your reactions are the band, Rick and Peter and Trevor and Cotter, plus the instruments. Walk into a Phish chat and it is Page, Trey, Mike, and Fishman. Every band we add gets its own kit, drawn from its own members and its own instrument vocabulary, all server-driven so we can hand a new band its reactions without shipping an app update. There are even conditional sit-in reactions, so 🎷 Stuart Bogie shows up in the palette on the nights he actually sits in with Goose. It is a small thing that tells the regulars we know exactly whose room they are standing in.

<div style="color-scheme:light; background:#E7E2D6; padding:16px; border-radius:20px; margin:16px auto; max-width:420px;">
  <div style="background:#F0EDE4; border-radius:18px; box-shadow:0 10px 28px rgba(20,18,30,0.12); overflow:hidden; max-width:390px; margin:0 auto; color:#2A2A3A; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; padding:14px 16px;">
    <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:10px; font-weight:700; letter-spacing:0.16em; text-transform:uppercase; color:#9A9AAA; margin-bottom:10px;">React · 🪿 Goose kit</div>
    <div style="display:flex; gap:8px; flex-wrap:wrap;">
      <span style="display:inline-flex; align-items:center; gap:6px; padding:7px 11px; border-radius:999px; background:#FEFDFB; box-shadow:0 2px 6px rgba(20,18,30,0.06); font-size:13px; font-weight:600;"><span style="width:18px;height:18px;border-radius:50%;background:linear-gradient(135deg,#E83A73,#F2A83B);"></span> Rick</span>
      <span style="display:inline-flex; align-items:center; gap:6px; padding:7px 11px; border-radius:999px; background:#FEFDFB; box-shadow:0 2px 6px rgba(20,18,30,0.06); font-size:13px; font-weight:600;"><span style="width:18px;height:18px;border-radius:50%;background:linear-gradient(135deg,#2A5FAA,#3AC4E8);"></span> Peter</span>
      <span style="display:inline-flex; align-items:center; gap:6px; padding:7px 11px; border-radius:999px; background:#FEFDFB; box-shadow:0 2px 6px rgba(20,18,30,0.06); font-size:13px; font-weight:600;"><span style="width:18px;height:18px;border-radius:50%;background:linear-gradient(135deg,#F2A83B,#FACC15);"></span> Trevor</span>
      <span style="display:inline-flex; align-items:center; gap:6px; padding:7px 11px; border-radius:999px; background:#FEFDFB; box-shadow:0 2px 6px rgba(20,18,30,0.06); font-size:13px; font-weight:600;"><span style="width:18px;height:18px;border-radius:50%;background:linear-gradient(135deg,#8B5CF6,#E83A73);"></span> Cotter</span>
      <span style="display:inline-flex; align-items:center; gap:5px; padding:7px 11px; border-radius:999px; background:#FEFDFB; box-shadow:0 2px 6px rgba(20,18,30,0.06); font-size:15px;">🎸</span>
      <span style="display:inline-flex; align-items:center; gap:5px; padding:7px 11px; border-radius:999px; background:#FEFDFB; box-shadow:0 2px 6px rgba(20,18,30,0.06); font-size:15px;">🥁</span>
      <span style="display:inline-flex; align-items:center; gap:5px; padding:7px 11px; border-radius:999px; background:#FEFDFB; box-shadow:0 2px 6px rgba(20,18,30,0.06); font-size:15px;">🎹</span>
      <span style="display:inline-flex; align-items:center; gap:5px; padding:7px 11px; border-radius:999px; background:rgba(232,58,115,0.10); font-size:15px;">🔥</span>
    </div>
  </div>
</div>

The chat noticed the second we shipped it: *"New reactions are super cute. Rick, Peter, Trevor, Cotter?"*

Here is the honest engagement picture, and it is the number I am happiest about in the whole post. Reactions were basically dormant through May, a handful a week. Then the **Big Modern!** tour started and the chat went vertical: **312, then 1,067, then 897 reactions in three consecutive weeks.** In just the last three weeks of tour, the Goose live chats carried roughly **1,500 messages and 2,300 reactions**, about 3,800 interactions total, which rivals the entire fourteen-show Spring tour in a fraction of the nights. When the band finally gave everyone something to be loud about, the redesigned room was ready to be loud in.

The people in it will tell you what it feels like better than I can:

> *"you only get the lockscreen setlist if you mark 🛋️ or 🎸. but it's kind of the killer feature"*
>
> *"New reactions are super cute."*
>
> *"I love the time counter for the song."*
>
> *"I've now seen 27 of 35 Big Modern! ever played 🤣"*

## The Lot

When you open Zabriskie now, you do not land on a feed. You land on **The Lot**.

The Lot is a personalized home. It is the leftmost tab and the default route, and it is built to answer one question the feed never could: what is worth my attention right now, for me. If a band you follow is on stage somewhere, the hero card is that show with a live setlist preview ticking underneath it. If nothing is live, it reaches for the next best thing, and it has a lot of next-best things to reach for: tonight's shows and who is going to them, last night's auto-generated recap, the jam bracket that is still taking votes, a new album to go listen to (it pushed the **Big Modern!** listening party the week it dropped), an "On This Day" card that surfaces a show from your own history or a band anniversary, a historical show worth revisiting, a bookmark to return to, a one-tap RSVP, even a nudge to post to the Flow if you have been quiet for a week. Every card is a real destination, not a placeholder. The Lot's whole job is to always have one more good reason to stay.

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
          <div style="position:absolute; inset:0; opacity:0.12; mix-blend-mode:overlay; background-image:url(&quot;data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E&quot;); background-size:150px 150px;"></div>
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
                <span style="font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:13px; color:rgba(255,255,255,0.85);">patrick, gmart, +10 you follow going</span>
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

## The Show Page Became the Whole Show

Every show now opens into a redesigned, cinematic **show detail page**, and it is doing a lot of jobs at once. Before the show it is a countdown and a planning surface. During the show it hands off to the live chat. After the show it is the recap and the setlist for good.

The hero is a sunset marquee with a live flip-clock counting down to doors. Under it, three avatar rows show your people sorted into **GOING**, **COUCH** (couch touring), and **WANT TIX**, so you can see the shape of the night at a glance and who to find. You can **call the opener right from the show page**, days before doors, the same one-tap prediction game that lives on the Lot and in the live room (they all share one store, so your pick follows you everywhere). You can share **where you are sitting** and see where everyone else is, section and row, so your friends can actually find each other in a shed. There is trip planning for lodging, a "the stage is dark" empty state that flips to a live setlist when the first song lands, and, once it is over, the full setlist with gaps, sit-ins, and a recap blurb built from the room's own reactions.

<div style="color-scheme:light; background:#E7E2D6; padding:16px; border-radius:20px; margin:16px auto; max-width:420px;">
  <div style="background:#F0EDE4; border-radius:20px; box-shadow:0 12px 34px rgba(20,18,30,0.14); overflow:hidden; max-width:390px; margin:0 auto; color:#2A2A3A; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
    <div style="position:relative; min-height:150px; background:radial-gradient(circle at 50% 18%, #F8C8A8 0%, #F2A83B 22%, #E83A73 60%, #2A2A3A 108%);">
      <div style="position:absolute; inset:0; opacity:0.12; mix-blend-mode:overlay; background-image:url(&quot;data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E&quot;); background-size:130px 130px;"></div>
      <div style="position:absolute; left:0; right:0; bottom:0; height:100%; background:linear-gradient(to top, rgba(20,18,30,0.9) 6%, rgba(20,18,30,0.4) 55%, transparent 100%);"></div>
      <div style="position:relative; z-index:1; padding:14px 16px 16px; display:flex; flex-direction:column; height:100%; justify-content:flex-end; min-height:150px;">
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:24px; font-weight:600; letter-spacing:0.03em; color:#fff;">Goose</div>
        <div style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:17px; color:rgba(255,255,255,0.94);">The Capitol Theatre · Port Chester</div>
        <div style="display:flex; gap:7px; margin-top:10px;">
          <div style="background:rgba(20,18,30,0.5); border-radius:10px; padding:5px 9px; text-align:center;"><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:700; color:#fff; font-variant-numeric:tabular-nums;">02</div><div style="font-size:7px; letter-spacing:0.12em; color:rgba(255,255,255,0.7);">DAYS</div></div>
          <div style="background:rgba(20,18,30,0.5); border-radius:10px; padding:5px 9px; text-align:center;"><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:700; color:#fff; font-variant-numeric:tabular-nums;">14</div><div style="font-size:7px; letter-spacing:0.12em; color:rgba(255,255,255,0.7);">HRS</div></div>
          <div style="background:rgba(20,18,30,0.5); border-radius:10px; padding:5px 9px; text-align:center;"><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:700; color:#fff; font-variant-numeric:tabular-nums;">31</div><div style="font-size:7px; letter-spacing:0.12em; color:rgba(255,255,255,0.7);">MIN</div></div>
          <div style="background:rgba(20,18,30,0.5); border-radius:10px; padding:5px 9px; text-align:center;"><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:700; color:#F2A83B; font-variant-numeric:tabular-nums;">08</div><div style="font-size:7px; letter-spacing:0.12em; color:rgba(255,255,255,0.7);">SEC</div></div>
        </div>
      </div>
    </div>

    <div style="padding:12px 14px;">
      <!-- who's going -->
      <div style="display:flex; gap:8px; margin-bottom:12px;">
        <div style="flex:1; background:#FEFDFB; border-radius:12px; padding:8px 10px; text-align:center;"><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:700; letter-spacing:0.08em; color:#2A5FAA;">GOING</div><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:700;">14</div></div>
        <div style="flex:1; background:#FEFDFB; border-radius:12px; padding:8px 10px; text-align:center;"><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:700; letter-spacing:0.08em; color:#3AC4E8;">COUCH</div><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:700;">31</div></div>
        <div style="flex:1; background:#FEFDFB; border-radius:12px; padding:8px 10px; text-align:center;"><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:700; letter-spacing:0.08em; color:#F2A83B;">WANT TIX</div><div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:18px; font-weight:700;">6</div></div>
      </div>

      <!-- call the opener -->
      <div style="background:#FEFDFB; border-radius:14px; padding:12px; margin-bottom:10px;">
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:10px; font-weight:700; letter-spacing:0.14em; text-transform:uppercase; color:#2A5FAA; margin-bottom:8px;">🔮 Call the opener</div>
        <div style="display:flex; gap:6px; flex-wrap:wrap;">
          <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px; font-weight:600; padding:6px 12px; border-radius:999px; background:rgba(42,95,170,0.10); color:#2A5FAA;">Hungersite</span>
          <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px; font-weight:600; padding:6px 12px; border-radius:999px; border:1px solid rgba(42,42,58,0.14); color:#6B6B7B;">Arrow</span>
          <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px; font-weight:600; padding:6px 12px; border-radius:999px; border:1px solid rgba(42,42,58,0.14); color:#6B6B7B;">Madhuvan</span>
        </div>
      </div>

      <!-- who's sitting where -->
      <div style="background:#FEFDFB; border-radius:14px; padding:12px;">
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:10px; font-weight:700; letter-spacing:0.14em; text-transform:uppercase; color:#E83A73; margin-bottom:8px;">🪑 Who's sitting where</div>
        <div style="display:flex; align-items:center; gap:8px; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12.5px; color:#2A2A3A;"><span style="width:22px;height:22px;border-radius:50%;background:linear-gradient(135deg,#F2A83B,#E83A73);"></span> patrick · <b>Orch L, Row G</b></div>
        <div style="display:flex; align-items:center; gap:8px; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12.5px; color:#2A2A3A; margin-top:6px;"><span style="width:22px;height:22px;border-radius:50%;background:linear-gradient(135deg,#F2A83B,#FACC15);"></span> gmart · <b>Balcony, Row B</b></div>
      </div>
    </div>
  </div>
</div>

That "call the opener from anywhere" detail is a small example of a rule we hold hard: a feature should be one thing with one source of truth, reachable from wherever you happen to be. Your opener call is the same object whether you make it on the Lot four days out, on the show page the morning of, or in the live room as the lights drop. People noticed. One night in the chat: *"we should build a closer call feature for pre show lol."* That is the app working, when the users start designing it with you.

## The Bracket Is the Bridge Between Tours

This is the feature I care about most for the actual between-shows problem, because it is the one that reaches into the dead weeks and keeps people listening.

When a tour ends, the app seeds a **March Madness style bracket** of that tour's best jams. The seeding is not editorial: we run sentiment over every show's live chat, weight it by song, cross-reference the historical setlist data for bustouts and rarities, and the top sixteen jams become the bracket. Then the rounds release on a schedule, Round of 16, Quarters, Semis, Final, and the community votes each one. Ties break by seed. Followers get a push when a new round opens.

The part that makes it work is the audio. Every matchup card has an **inline player for both jams**, streaming from **Relisten** where the recording exists, with a nugs.net "listen on" link as the fallback when it does not. You cannot vote between two twenty-minute jams without hearing them, so voting in the bracket means re-listening to the tour you just watched, one jam at a time, for weeks after the lights came up. By the time we crown a Jam of the Tour, the next run is already on the calendar and nobody ever fully left.

<div style="color-scheme:light; background:#E7E2D6; padding:16px; border-radius:20px; margin:16px auto; max-width:460px;">
  <div style="background:#F0EDE4; border-radius:18px; box-shadow:0 10px 28px rgba(20,18,30,0.12); overflow:hidden; max-width:420px; margin:0 auto; color:#2A2A3A; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
    <div style="padding:11px 15px; background:#141220; color:#fff; display:flex; align-items:center; justify-content:space-between;">
      <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:700; letter-spacing:0.14em;">🏆 JAM BRACKET · SEMIFINAL</span>
      <span style="font-size:11px; opacity:0.6;">Big Modern! Tour</span>
    </div>
    <div style="padding:14px 15px;">
      <!-- side 1 -->
      <div style="background:#FEFDFB; border-radius:14px; padding:12px 13px; box-shadow:0 2px 8px rgba(20,18,30,0.05);">
        <div style="display:flex; align-items:baseline; justify-content:space-between;">
          <div><span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:800; color:#E83A73;">SEED 1</span> <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:16px; font-weight:700;">Madhuvan</span></div>
          <span style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:13px; color:#9A9AAA;">6/22 · SPAC</span>
        </div>
        <div style="display:flex; align-items:center; gap:10px; margin-top:9px;">
          <span style="width:30px; height:30px; border-radius:50%; background:#141220; color:#fff; display:flex; align-items:center; justify-content:center; font-size:12px;">▶</span>
          <div style="flex:1;">
            <div style="display:flex; align-items:center; gap:6px; font-size:10px; color:#9A9AAA;"><span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-weight:700; color:#2A5FAA;">RELISTEN</span> · 22:14</div>
            <div style="height:4px; background:rgba(42,42,58,0.08); border-radius:2px; margin-top:4px; overflow:hidden;"><div style="height:100%; width:34%; background:#E83A73;"></div></div>
          </div>
        </div>
      </div>
      <div style="text-align:center; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:800; color:#9A9AAA; padding:8px 0;">VS</div>
      <!-- side 2 -->
      <div style="background:#FEFDFB; border-radius:14px; padding:12px 13px; box-shadow:0 2px 8px rgba(20,18,30,0.05);">
        <div style="display:flex; align-items:baseline; justify-content:space-between;">
          <div><span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:800; color:#9A9AAA;">SEED 4</span> <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:16px; font-weight:700;">Into the Myst</span></div>
          <span style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:13px; color:#9A9AAA;">6/14 · Raleigh</span>
        </div>
        <div style="display:flex; align-items:center; gap:10px; margin-top:9px;">
          <span style="width:30px; height:30px; border-radius:50%; background:#fff; border:2px solid #141220; color:#141220; display:flex; align-items:center; justify-content:center; font-size:12px;">▶</span>
          <div style="flex:1;">
            <div style="display:flex; align-items:center; gap:6px; font-size:10px; color:#9A9AAA;"><span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-weight:700; color:#6B6B7B;">nugs.net</span> · 18:03</div>
            <div style="height:4px; background:rgba(42,42,58,0.08); border-radius:2px; margin-top:4px; overflow:hidden;"><div style="height:100%; width:0%; background:#E83A73;"></div></div>
          </div>
        </div>
      </div>
      <div style="display:flex; align-items:center; gap:8px; margin-top:12px;">
        <button style="flex:1; padding:10px; border-radius:999px; border:none; background:#E83A73; color:#fff; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-weight:700; font-size:11px; letter-spacing:0.10em; text-transform:uppercase;">Vote Madhuvan</button>
        <button style="flex:1; padding:10px; border-radius:999px; border:1px solid rgba(42,42,58,0.16); background:#FEFDFB; color:#2A2A3A; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-weight:700; font-size:11px; letter-spacing:0.10em; text-transform:uppercase;">Vote Into the Myst</button>
      </div>
    </div>
  </div>
</div>

The gap between tours is where most music apps quietly die. The bracket is the deliberate answer: it turns the dead weeks into a reason to open the app every few days, re-listen to a jam, and argue about it in the chat, right up until the next tour makes the argument moot.

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

None of these three are things you open during a show. They are things you open because you are a fan and it is a slow afternoon and you want to fall into your band's history for twenty minutes. That is the whole idea, and this is the part people actually did all summer. More on that below.

## Goose Mode and the Band Pages

Each band gets a **band mode**, a tour companion tailored to one band at a time, and Goose Mode got most of the love because Goose is home base. It knows the band's calendar, colors, and vocabulary. The centerpiece is a tour timeline with a live countdown to the next show, past shows checked off, upcoming shows carrying weather and which of your friends are going, and your friends annotated in and out of the run leg by leg. Every band also has a full band page: recent shows with inline-expand setlists, your personal history with that band, and a jump straight into the Songbook. Adding a band is a whole project (find a setlist source, backfill the history, wire the page and the mode), and we did it more than a dozen times, because the more homes there are, the more nights someone has a reason to open the app.

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

None of this happened in isolation. The Lot, the live chat, the show page, the Flow, profiles, notifications, festivals, and the band modes all got "v2" cinematic redesigns in the same language: warm parchment and sunset gradients, film grain, an editorial serif for the venues, less like a database with a skin on it and more like a thing made by people who love the thing. The whole app changed clothes this window. This is a big deal operationally because of how it ships. Every redesign runs behind a version flag, keyed to the client build, so an old phone that has not updated keeps getting the old design and a fresh install gets the new one, and neither breaks. That is the machinery that lets us redesign a live surface without stranding the person watching a show on a two-month-old binary. It is unglamorous and it is most of why we can move this fast without breaking the people already here.

## New Rooms: Cabo, the Festivals, More Bands

The band list kept growing. We backfilled full histories for **Widespread Panic (3,114 shows), Eggy, and Dizgo**, on top of the dozen bands we added during Spring, each with the full treatment: a source for the setlists, a band page, a band mode, a spot in the directory. There are **102 bands and 38,894 shows** in the catalog now.

We built out **festivals** as their own surface: a multi-stage schedule grid, a plan phase, and a full **festival recap** that reads the whole weekend rather than one show, with a program block, hero moments, a route map, and per-day bookends. **Viva El Gonzo** in Cabo got the destination-festival treatment (down to canonicalizing every set to San José del Cabo), and we seeded lineups for All Good Now and Northlands. Goose Summer is on the calendar and already wired in.

The point of all of it is the same: the more bands and the more festivals live in the app, the more nights of the year there is something happening for somebody, and the fewer truly dead Tuesdays there are.

## Festival Mode: Plan It Like a Conference

A festival is a different animal from a tour stop, so it gets its own mode. The centerpiece is a **multi-stage schedule grid**, and the closest honest comparison is a work conference program: stages down one axis, set times across the other, every slot a block you can tap. Except the sessions are sets and the tracks are stages, and the scheduling conflict you are agonizing over is which of two bands you love you have to miss.

You build a plan for each day, the grid flags your overlaps, every day carries its own weather, and there is a live peek that lights up the set happening right now. When the weekend is over, Festival Mode writes a **recap of the whole event**, not one show but the full arc: a program block, the standout moments, a route map of where the weekend went, and per-day bookends. **Viva El Gonzo** in Cabo got the full treatment this year, down to canonicalizing every set to San José del Cabo.

<div style="color-scheme:light; background:#E7E2D6; padding:16px; border-radius:20px; margin:16px auto; max-width:480px;">
  <div style="background:#F0EDE4; border-radius:18px; box-shadow:0 10px 28px rgba(20,18,30,0.12); overflow:hidden; max-width:440px; margin:0 auto; color:#2A2A3A; font-family:'Inter',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;">
    <div style="padding:12px 15px; background:radial-gradient(circle at 20% 20%, #F2A83B, #E83A73 120%); color:#fff; display:flex; align-items:center; justify-content:space-between;">
      <span style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:12px; font-weight:700; letter-spacing:0.10em;">⛺ VIVA EL GONZO · SAT</span>
      <span style="font-family:'Instrument Serif','Cormorant Garamond',Georgia,serif; font-style:italic; font-size:14px; opacity:0.92;">San José del Cabo · 82°</span>
    </div>
    <div style="padding:12px 13px;">
      <div style="display:grid; grid-template-columns:58px 1fr 1fr; gap:6px; font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:9px; font-weight:700; letter-spacing:0.06em; color:#9A9AAA; text-transform:uppercase; margin-bottom:6px;">
        <div></div><div style="text-align:center;">Beach Stage</div><div style="text-align:center;">Jungle Stage</div>
      </div>
      <div style="display:grid; grid-template-columns:58px 1fr 1fr; gap:6px; align-items:stretch;">
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:700; color:#6B6B7B; display:flex; align-items:center;">7:00</div>
        <div style="background:#FEFDFB; border-radius:10px; padding:9px; border-left:3px solid #2A5FAA;"><div style="font-size:12px; font-weight:700;">Goose</div><div style="font-size:10px; color:#9A9AAA;">7:00 · Set I</div></div>
        <div style="background:#FEFDFB; border-radius:10px; padding:9px; opacity:0.55;"><div style="font-size:12px; font-weight:600;">Dogs in a Pile</div><div style="font-size:10px; color:#9A9AAA;">7:30</div></div>
      </div>
      <div style="display:grid; grid-template-columns:58px 1fr 1fr; gap:6px; align-items:stretch; margin-top:6px;">
        <div style="font-family:'Jost',-apple-system,BlinkMacSystemFont,system-ui,sans-serif; font-size:11px; font-weight:700; color:#6B6B7B; display:flex; align-items:center;">9:00</div>
        <div style="background:rgba(232,58,115,0.10); border-radius:10px; padding:9px; border-left:3px solid #E83A73;"><div style="font-size:12px; font-weight:700;">Goose</div><div style="font-size:10px; color:#E83A73; font-weight:600;">9:15 · Set II</div></div>
        <div style="background:rgba(232,58,115,0.10); border-radius:10px; padding:9px; border-left:3px solid #E83A73;"><div style="font-size:12px; font-weight:700;">Khruangbin</div><div style="font-size:10px; color:#E83A73; font-weight:600;">9:30 · conflict</div></div>
      </div>
      <div style="margin-top:10px; display:flex; align-items:center; gap:8px; padding:8px 11px; background:rgba(242,168,59,0.12); border-radius:10px; font-size:11.5px; color:#8a5a10;">
        <span>⚠️</span><span>You planned both 9:00 sets. Goose Set II overlaps Khruangbin by 45 min.</span>
      </div>
    </div>
  </div>
</div>

## Half of This Is Patrick

I need to stop and do this properly, because if you have read this far you have been reading "we" the whole time and the "we" is doing a lot of quiet work.

Patrick McCaughey (he is `@blimpalot` in the app) wrote **nearly half of the 787 pull requests in this window.** Not helper commits. Whole features, end to end, that are among the best things in the product. The Apple Watch app is his. The entire sit-in and guest-credit system is his. Group DMs and the Messages hub are his. Birthdays, big chunks of the poster archive, huge swaths of Tour Stats, the band backfills, the reaction kits for new bands. When I said the reference library is the part people actually used all summer, I was mostly describing Patrick's work.

He put it best himself, in the chat one night, and I am quoting it because it is both generous and exactly backwards about who deserves credit:

> *"I might have wrote the app but all the cool shit in the app isn't me it's Patrick lol"*

That was me, about him. He would tell you the opposite. We are both a little bit right and he is more right than I am.

And here is the part that does not show up in a commit log at all. **Patrick made stickers.** Actual, physical, die-cut vinyl slaps, and he brings them to **every single show** and hands them out: on the lot, in the lot line, in the pit, to the person next to him, to the bartender, to total strangers between sets. He is not just leaving them on a table. He is walking up to people, showing them the app on his own phone, and talking them into it one human conversation at a time, night after night, city after city. Every growth chart later in this post that ticks up, ticks up because Patrick stood in a parking lot in the heat and sold someone on a thing he helped build. The man is a mensch. I do not know a better word for it, and I do not think there is one. Half the code and all of the street team, cheerfully, for the love of it.

<div style="display:flex; gap:18px; flex-wrap:wrap; justify-content:center; margin:20px auto; max-width:520px;">
  <div style="filter:drop-shadow(0 8px 14px rgba(20,18,30,0.28)); transform:rotate(-5deg);">
    <div style="width:180px; height:180px; border-radius:26px; background:#FEFDFB; padding:9px; box-sizing:border-box;">
      <div style="width:100%; height:100%; border-radius:20px; background:radial-gradient(circle at 32% 26%, #F8C8A8 0%, #F2A83B 22%, #E83A73 55%, #2A5FAA 100%); display:flex; flex-direction:column; align-items:center; justify-content:center; color:#fff; text-align:center; position:relative; overflow:hidden;">
        <div style="position:absolute; inset:0; opacity:0.12; mix-blend-mode:overlay; background-image:url(&quot;data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E&quot;); background-size:120px 120px;"></div>
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

My favorite version of this showed up in the chat one night, and it is the whole thesis in one message:

> *"the owner of this bar is super cool. huge deadhead and big goose fan. he is renting a bus and taking his whole staff to the SPAC show! just got him on the app hahha"*

That is the growth engine. Not a referral loop or a viral coefficient. A person who loves the thing, standing in front of another person, in the place they both love to be, and now a bar owner is busing his staff to a Goose show and they are all in the chat. You cannot buy that and you cannot fake it, and it is the entire reason a two-person app has a real community around it at all.

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
- **And when there was a show, engagement went vertical.** As covered up in the live-chat section: reactions went from a handful a week to 1,067 in a single week once the Big Modern! tour started, and the last three weeks of tour drove roughly 3,800 chat interactions, rivaling the whole Spring run. The between-tours job is retention, but the tour itself proved the redesigned room is more alive than it has ever been.

And the numbers I am **not** going to give you: a clean daily-active-users chart. We record a row when a user opens the app, but that table has multi-week holes in it from May and early June where the logging was broken, so any DAU or "percent active on non-show days" figure I quoted would be built on gaps. I would rather tell you the table is broken than draw a confident line through missing data. Fixing that instrumentation is now on the list, because the between-shows thesis deserves a real measurement and right now I cannot give it one.

The one-line version: **we are not going viral, and I do not want to. We are growing a thing I love slowly, by hand, and keeping the room warm between tours. Mostly we did.**

## What This Stretch Taught Me

- **The gap is the product, and it is a retention problem, not a growth problem.** Spring proved the show works. This stretch proved that new users arrive on the tour calendar and not otherwise, so the between-shows job is to hold the community you already have. Almost every feature in this post is a bet on retention, and the honest scoreboard is "the core stayed," not a signup hockey stick.
- **Measure the thesis or you are guessing.** I built a whole argument about between-shows engagement and then discovered my own session-tracking had holes in it. Shipping the features without fixing the instrument to grade them is a mistake I am writing down so I actually fix it.
- **Shipping a redesign is a distributed-systems problem.** The version-flag machinery that lets an old build keep its old UI while a new build gets v2 is the least visible work of the whole two months and close to the most important. Without it, moving this fast would mean breaking the exact people who already showed up.
- **787 PRs is still not normal, and most of it is still Claude.** Same as April: a lot of this was written by Claude, a lot of it had to be rewritten by Claude after I caught it going sideways, and every sharp edge got logged into the [agent reliability dataset]({% post_url 2026-06-10-the-test-suite-was-the-incident %}) I keep publishing from. The failures are the point of the dataset, not something to hide.
- **Being on the App Store changes what you build.** Report-and-block moderation on chat messages was not a feature anyone in the community asked for. It was the price of admission to the store, and it shipped in the same window as everything fun. Growing up means building the boring safety surface too.

And I should say the obvious thing, the thing under all of it, the reason two people spend nights and tour weekends building a couch-touring app instead of doing literally anything else: **Goose fucks.** That is the whole premise. The band is that good, the jams are worth talking about at 1am, the bustouts are worth calling from your couch, and the community around them is worth building a home for. Everything in this post is downstream of a band being good enough that people want to be in the same room about it, even when the room is a chat.

Goose Summer is on the calendar. The festivals are seeded. The watch is on my wrist, and Patrick built it. The app is not done, and the real test of everything here is not this recap. It is whether the room stays this alive after the tour ends, when the next run is still just an announcement away.

If you toured with us this Spring, thanks for chomping. If you have been in the chat on the dead Tuesdays, you are the 42, and you are the entire experiment working. If Patrick handed you a sticker in a parking lot and you scanned it, welcome, you are the growth chart. And if you have never tried it and an app that is good company between shows sounds like something you want, come find us. There is probably something worth reading tonight, even if nothing is live.

---

*One housekeeping note, same as last time: the screens in this post are mockups, not real screenshots, drawn to make each feature legible in one frame. The real app looks slightly different on iOS, Android, and the watch (Live Activities vs. ongoing notifications, complication styling, system fonts). The features themselves all shipped, and the stickers are real (Patrick's are cooler than the mockup here). The growth figures are real production numbers as of June 30, including the unflattering ones. Patrick's share of the work is real too, and if anything I undersold it.*
