---
layout: post
title:  "The Whole Night"
date:   2026-08-17 19:00:00 -0400
group: ai
categories: ai zabriskie development
---

On Thursday, August 13, Goose opened the West Coast run in San Diego with "Animal." Sixteen people in Zabriskie had said they were going. Seventeen had said they were on the couch. By the time the band finished the "726" encore, fourteen people had written 160 messages in the Chomp and five had sent 48 reactions across a twelve-song show.

It was a regular Thursday show, which made it useful for understanding what had changed. The RSVPs split almost perfectly between **GOING** and **COUCH**, and the app had to do a different job before the show, in the room, at home, and after the encore.

Seven weeks earlier, I published [The App That Lives Between Shows]({% post_url 2026-06-30-the-app-that-lives-between-shows %}). The live room worked, The Lot had finally given the app a home screen, and the watch clients were running in our field builds. I wrote that the hard product problem was the gap between one show and the next. The Lot existed, but it did not yet know what time it was.

I was not exactly wrong, but I had drawn the boundary in the wrong place. A show does not begin when the first note lands or end when a live flag changes in the database. The plan starts days earlier. During the show, a person standing at the rail and a person watching from a couch need almost opposite things from the same room. The setlist, conversation, and people remain after the band leaves the stage. Over the next seven weeks, Zabriskie started to follow that longer edge.

## The Lot Learned What Time It Was

In June, I wrote as though The Lot was mostly finished because it had become the home screen. It was not. It was a beautiful pile.

Every useful thing we built earned a card, and every card wanted a permanent place. A live show, an upcoming RSVP, a Crate recommendation, an anniversary, an invitation, a recap, a bracket, and the latest thing somebody did could all be individually correct and collectively exhausting. Opening the app increasingly answered the question, "What does Zabriskie have?" That is a bad question for a home screen.

The better question is: **what should we do right now?**

### One Lead, Not Every Feature

The Lot now has two clocks: the show's and the person's. It knows whether somebody is going, couch touring, or still deciding; whether a room is live; whether a show just ended; what is waiting for a response; and which cards have been sitting unseen. From those candidates it chooses one lead move, a short schedule, and a few supporting cards.

A live RSVP wins. A show later tonight comes next. The morning after a show, the unfinished rating and Chomp can take the space. An anniversary can win a quiet day. If none of those has a claim, the time of day breaks the tie: something to spin in the morning, something to return to during the workday, the local scene or a stream in the evening, a deeper archive dig on the weekend. The page says which moment it was programmed for because otherwise the cards simply appear to move around by accident.

<figure style="max-width: 420px; margin: 2rem auto;">
  <img src="/img/zabriskie-whole-night-ios-dark-lot.png" alt="The Lot in Zabriskie for iOS in dark mode on a Monday evening, showing my show and posting activity, a morning-after Goose recap, and recent Flow activity" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>My actual Monday evening Lot: the unfinished Goose night wins the lead; the Flow sits beneath it.</figcaption>
</figure>

This required the page to do something software is generally bad at: decline to show correct information. We capped both the number of sections and the number of cards inside them. One busy section cannot consume the page. A card that has been buried for several visits gets one chance to move up. When the final card in a section is dismissed, its empty heading leaves too. The schedule stays short. The rest of the app still exists; the home screen is no longer required to prove it.

### Tonight Is a Room, Not a Date

The earlier Lot split a live show, the other shows tonight, and a person's couch plans into separate pieces. The new **Tonight** card puts the open rooms first and the later rooms after them. A live row carries the current song and set, the stream when one exists, and one compact door into the Chomp. The same row can still change a plan between **GOING** and **COUCH**. Several couch RSVPs across a run collapse into **Your Couch Tour**, an itinerary with the nights left, the correct local tune-in times, and the other people staying home.

A pending **Bring Your People** invitation now lands on The Lot instead of depending on somebody finding the original notification again. **What's New** moved out of a floating toast and into a card that can be read and dismissed. Received reactions say who reacted and which post they found. A locked opener call becomes a status instead of pretending to be another urgent action. These are small decisions, but they are the difference between a page that understands the state of something and a page that merely knows the feature exists.

<figure style="max-width: 420px; margin: 2rem auto;">
  <img src="/img/zabriskie-whole-night-ios-dark-lot-tonight.png" alt="The Lot in Zabriskie for iOS in dark mode showing This Weekend recommendations and my circle around Goose's West Coast Tour 2026" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>One screen lower: weekend options and the West Coast run, weighted by who is already in the plan.</figcaption>
</figure>

### The Archive Has to Return Somewhere

The Lot also became the place where a completed show can come back with a reason. The next morning it can ask how the night was, collect the jam of the show, and reopen the original Chomp. Much later, **On This Day** can put a personal anniversary beside a notable show from the archive. A bookmarked show can return with the explanation for why it was saved. A new release can arrive with its listening party already attached. A scheduled Tape Night can reopen an old room at an actual time instead of leaving another permanent card on the page.

The quieter parts matter too. The Crate contributes one pick, not fifteen. The Flow contributes a glimpse, not a second feed. Weekend shows, incoming releases, saved shows, Local Scene, recent love, tour statistics, recommendations, and the year so far all have a way onto the page, but none is entitled to remain there. There is no follow graph underneath this. The inputs are attendance, plans, taste, the person's own history, and what the community is doing now.

<div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 1.5rem; max-width: 920px; margin: 2rem auto; align-items: start;">
  <figure style="margin: 0;">
    <img src="/img/zabriskie-whole-night-ios-dark-lot-history.png" alt="The Lot in Zabriskie for iOS in dark mode showing who is going to Goose at Red Rocks, a Field of Vision festival recap, and upcoming Goose couch dates" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
    <figcaption>The social edge of a future show beside a recap that can finally return after the festival.</figcaption>
  </figure>
  <figure style="margin: 0;">
    <img src="/img/zabriskie-whole-night-ios-dark-lot-discovery.png" alt="The Lot in Zabriskie for iOS in dark mode showing my populated Goose West Coast Couch Tour itinerary with local times and stream links" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
    <figcaption>Nine nights on the couch become one tour itinerary, with local times and stream links attached.</figcaption>
  </figure>
</div>

### The Work Was the Editing

Then we kept sanding it. Every card received art and a place inside a real section. Buttons stopped changing shape from one card to the next. We fixed shadows, contrast, touch targets, haptics, sharing, loading, and dismissal, then added guardrails because each one had already drifted at least once. Cards can share their own image instead of an authenticated URL nobody else can open. The page survives the old mobile bundles that do not know about its new priorities. Its tests now fail when a card escapes its section, a call to action changes shape, or one kind of card crowds out everything else.

I count 187 non-merge commits in this window touching Lot-specific handlers, rendering, tests, or those guardrails. That number does not prove that we chose correctly. It does explain why the four screens above are not the home screen I published in June.

The Lot is where the rest of this story first appears. It notices that a night is coming, hands the person into the right room, and then decides what from the night is worth bringing back.

## Before Doors

The Lot can decide which night matters. The show page still has to help finish the plan. The June page already brought RSVPs, seating, trip planning, opener calls, and the live setlist into one place. The August work focused on the awkward part: a plan exists, but it is not finished.

<figure style="max-width: 420px; margin: 2rem auto;">
  <img src="/img/zabriskie-whole-night-ios-dark-before-doors.png" alt="Zabriskie for iOS in dark mode showing the countdown and Going, Couch, and Want Tix groups for an upcoming Goose show" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>The plan before doors: one show, three kinds of intent.</figcaption>
</figure>

**Bring Your People** lets someone pull another person into the plan. **Ticket Check** asks the embarrassingly necessary follow-up: you said you were going, but do you actually have a ticket? The rest of the run now sits beside the current night. A **Song Wish List** collects what people hope to hear, while **Played Here Before** brings back the band's earlier visits to the room. For someone staying home, the same run becomes a tune-in schedule in the correct timezone.

The home-screen widgets follow the same clock. On an ordinary day they can surface an anniversary, a recent show, or the year so far. As a show approaches, they become a countdown and then a live glance. Gap Watch can pull an overdue song into the days before the next RSVP. Call the Opener already existed in June; what was missing was the answer. Now the result can find its way back to the person who made the call instead of disappearing into the setlist.

This is still small-community software. Since July 1, people have sent 11 show invitations and added 28 songs to wish lists. That establishes that somebody used them. It does not establish a habit, and it certainly does not allow me to turn two small counters into a victory lap.

## At the Rail

Once the first note lands, the problem reverses. Before doors, the app asks people to make decisions. At the show, it should ask for as little attention as possible.

We call that posture the **rail**: someone is in the room, service is bad, both hands are occupied, and looking at a phone is already one thing too many. The Apple Watch and Wear OS faces can carry the current song, the current set, the setlist so far, and the latest Chomp messages. From there, one tap can send a reaction or like a message; a canned reply or dictation can answer the room without pretending that a watch is a very small phone.

Most of the work was in the cases that are easy to omit from a demo. The watches recover from stale state and keep long song titles inside a round screen. They distinguish set break from encore. If two shows overlap, a long press opens a switcher so the watch follows the room the person intended to follow. The encore produces one deliberate haptic. After the show, the face holds the final setlist in an afterglow instead of immediately dropping back to an idle screen.

Patrick led the Wear OS client, while I built most of the Apple Watch client. We made parity a project rule: when a live-show capability belongs on the wrist, both clients get it. Parity does not mean copying the phone. If the watch requires the same attention as the phone, we have missed the reason to build it.

The phones moved in the same direction. The Live Activity on iOS and the ongoing notification on Android carry the current song without requiring the app to remain open. Haptics mark actions and actual show transitions rather than buzzing indiscriminately. The full setlist and conversation are still there, but the default at the venue should be a glance followed by putting the device away.

## On the Couch

At home, getting out of the way is the wrong goal. The stream is on, a keyboard is available, and the conversation may be the reason someone opened Zabriskie at all.

We call that posture the **salon**. The composer remains in reach, the thread gets the screen space, and a show switcher appears in the header when several bands are live. Opening the keyboard expands the conversation rather than crushing it. The setlist and the full live page remain one move away.

The RSVP supplies gravity, never permission. **GOING** can prioritize a show on the wrist and change the countdown and copy on The Lot. **COUCH** turns that copy toward tuning in. Neither choice locks or hides anything. The larger RSVP-weighted phone layout is still unfinished, and that is intentional for now: someone at the venue can spend set break reading every message, while someone at home can send a watch reaction while cooking dinner.

This is why the San Diego split mattered. Sixteen people said they were at the show and seventeen said they were on the couch. Both groups reached the same setlist and the same Chomp, but the interfaces around that shared room could begin to pull in different directions. When Goose reached "Big Modern!" in the second set, the wrist could carry the song title for someone in the amphitheater while the phone could give the thread room for someone at home. The production counts do not tell me which RSVP group wrote each message, and I do not need them to. The near-even split shows that both postures were present.

Across all shows since July 1, 33 people have sent 1,303 live messages and 27 have sent 4,962 reactions. The reactions come from a smaller group and arrive quickly. That is the point: sometimes a sentence is too much, but a reaction is still enough to say that somebody else heard the same thing.

## After the Lights

In San Diego the encore was "726." The older app mostly treated that as the end of its useful work. The setlist completed, the live state shut down, and the room receded into the archive.

Now the night has an exit. The watch and phone hold the final state in an afterglow. The next morning, The Lot can ask for a one-tap rating and return the completed show with its setlist and recap. The rating also lives on the show's page instead of existing only on a temporary home-screen card. The recap reopens the original Chomp, because a database flag changing from live to completed is not a good reason to break the conversation.

<figure style="max-width: 420px; margin: 2rem auto;">
  <img src="/img/zabriskie-whole-night-ios-dark-after-lights.png" alt="Zabriskie Band Mode for Goose in dark mode showing the August 13 encore 726 as a recent bust-out" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>The August 13 encore, already returning as part of the archive.</figcaption>
</figure>

Much later, the same show can return through **On This Day**. What used to be one card is now a reel that can place a personal anniversary beside a notable archival show and the other concerts that happened on that date. The watch and widgets can surface those anniversaries too. We do not need to manufacture a new commemorative post every year. The record already exists, with the people, setlist, poster, recording links, and conversation attached.

This changed how I think about the archive. A completed show is reference data, but it is also the durable object that the live room, the next morning, and the anniversary can all return to.

## The Rest of the App Changed With It

The show lifecycle became the easiest way for me to understand the work, but not everything we shipped fits inside one Goose show.

My favorite counterexample came from Phish's five-night run at Madison Square Garden. The first night revealed the rule: each show would rebuild one year of the band's history, moving from 1992 through 1996. What should the app do with that information before the second night?

For the next four nights, Zabriskie turned the pattern into **Setlist Prophecy**. The show page read the real rotation from that night's year, removed anything already played earlier in the run, and surfaced likely songs, era-specific opener choices, long-gap candidates, and the gags that belonged to that version of Phish. It even made a call on Trey's shirt.

<div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 1.5rem; max-width: 920px; margin: 2rem auto; align-items: start;">
  <figure style="margin: 0;">
    <img src="/img/zabriskie-setlist-prophecy-phish-1993-dark.png" alt="Dark-mode Zabriskie Setlist Prophecy board for Phish's 1993-themed night, showing likely songs and era-specific gags" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
    <figcaption>Night two: the 1993 board, with likely songs and era gags still on the table.</figcaption>
  </figure>
  <figure style="margin: 0;">
    <img src="/img/zabriskie-setlist-prophecy-phish-1994-dark.jpg" alt="Dark-mode Zabriskie Setlist Prophecy board for Phish's 1994-themed night, showing Trey's shirt prediction, likely songs, and setlist-note signals" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
    <figcaption>Night three: the 1994 board after we added the shirt call and signals from the archive's setlist notes.</figcaption>
  </figure>
</div>

After the encore, the prophecy graded itself. The wardrobe call landed four nights out of five, ending with Trey's gold velour Clifford Ball shirt on the 1996 finale. That night also delivered two of the four stranger calls: **Harpua** closed the first set, and **Crosseyed and Painless** supplied the promised wink toward the *Remain in Light* Halloween show later in 1996.

<figure style="max-width: 420px; margin: 2rem auto;">
  <img src="/img/zabriskie-whole-night-ios-dark-phish-1996.png" alt="Zabriskie for iOS in dark mode showing Phish's July 29 finale at Madison Square Garden" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>The fifth night of the time machine: 1996, back in the completed-show archive.</figcaption>
</figure>

People played along. Twelve people made 39 RSVPs across the five shows. Seven made 22 Call the Opener guesses, while the live rooms produced 221 messages from six people and 614 reactions from three. None of the opener guesses landed.

Setlist Prophecy was not a machine-learning oracle. It was an editorial game built from the archive, and its misses were part of the result. It gave people something specific to argue about before doors and something funny to score after the encore without asking them to populate a separate, empty room.

The same archive problem appeared in the rest of the app. The **Composer** became one screen for sharing an album, film, book, or show. Search begins while someone types; recent listening and attendance supply suggestions; choosing an item opens the form in place. If I have written about the same album before, the old post can return as context for the new one instead of vanishing down the Flow.

<figure style="max-width: 420px; margin: 2rem auto;">
  <img src="/img/zabriskie-whole-night-ios-dark-flow.png" alt="The Zabriskie Flow in dark mode showing a richly formatted post with video, text, reactions, and navigation" loading="lazy" style="width: 100%; height: auto; border-radius: 24px;">
  <figcaption>The Flow in the same dark-mode iOS build.</figcaption>
</figure>

Albums now open into an **Album Hub**: the record is divided by side, tracks connect to their live histories, and the page carries its discussion and scheduled listening parties. The **Library** became one search across recordings, albums, films, and books, with Relisten audio playing inside the app. We also opened a **Local Scene** pilot around Pittsburgh. A show should not need decades of setlist data before it is allowed to exist in Zabriskie.

The catalog grew from 102 band records and 38,894 shows in the June post to 340 band records and 47,669 shows. Those numbers require an immediate qualification: this is not 238 new bands with equally complete histories. The increase includes Local Scene and broader roster imports. The deeper additions include the Slip, Dave Matthews Band, Dave & Tim, Medeski Martin & Wood, and more of the Kimock archive.

The mobile shell also started behaving like one. Browser confirmation boxes became house sheets and toasts. Sheets drag and spring; buttons have haptics; swipe-back works from the edge. Photos can be pinched, panned, and dismissed. A universal link can survive login and return to the page that opened it. The app can distinguish being offline from merely waiting on a request. No single item belongs in a headline. Together, they remove the recurring feeling that a website has been placed inside an app-shaped frame.

## The Receipt

From July 1 through August 17, **1,025 pull requests** merged into Zabriskie: 717 from me and 308 from Patrick. The iOS build number moved from 53 to 112. Android moved from 51 to 157, with Wear at 158.

That is an absurd amount of software for less than seven weeks. It is also an easy number to misread. Pull-request throughput does not prove that the product improved. It does explain how a regular show night came to touch so many surfaces that either did not exist, or barely existed, when I wrote the June post.

The community grew much more slowly than the software. Registered users went from 426 to **508**, an increase of 82 people. There were **50 weekly active users** and 98 monthly active users when I pulled the production numbers for this post. The attendance ledger grew from 4,598 to **6,020 records**. Since July 1, 24 people have written 84 album, book, film, photo, listening, show-review, or video posts, alongside the 1,303 live messages and 4,962 live reactions.

It is a real room, and it is still a small one. Most weeks after the first half of July brought single-digit signups. Live activity remains concentrated in a core group. I like the direction of those numbers. None of them means that we have solved growth or broad participation.

Some of the new ideas did almost nothing. During its full fourteen-day test, a pre-show conversation box drew eight comments from two people across seven shows, with no replies. The new show-photo wall received zero photos. Venue Intel received zero tips. Watch-party membership remained at zero.

It would be easy to blame the size of the community, except 24 people made 84 regular content posts during the same stretch. The narrower explanation is more useful: adding an empty box to a show page does not give somebody a reason to be the first person inside it.

That is the useful correction underneath all of this work. Features that fit a motion people were already making---checking who is going, glancing at the current song, reacting in a live room, rating the night the next morning---have a chance to become part of the show. Features that ask somebody to populate a new room alone have a much harder job.

Seven weeks ago I wrote that the gap between shows was the hard half of the product. I still believe that. What changed is that I no longer see the show as a two-and-a-half-hour block with empty time on either side. It has a long edge: the plan before doors, the split between the rail and the couch, the ride home, the next morning, and the date years later when the whole thing returns.

On August 13, that edge ran from 33 people making a plan, through "Animal," 160 messages, twelve songs, and one final "726." The app stayed with the night after the music stopped.

We can carry the whole night now. I do not yet know how often people will want us to, whether enough of them will, or whether the pieces that worked during a Goose or Phish run will become habits between them. That is the next problem, and it is much harder to solve than merging another thousand pull requests.
