---
layout: post
title: "Software for One"
date: 2026-09-06 12:00:00 -0600
permalink: /ai/personal/programming/2026/09/06/software-for-one.html
description: "Building apps for the things I want to do, with AI helping make the tools. On personal software, maintenance, and getting on with life."
categories: ai personal programming
app_page: true
---

When I was growing up, my parents wrote programs on a [Commodore Plus/4](https://americanhistory.si.edu/collections/object/nmah_608218), and later a [Commodore 64](https://archive.computerhistory.org/resources/text/Commodore/Commodore.Commodore64.1982.102646264.pdf). The 64 was my first computer. Neither of them worked in computing. They had other jobs, and they used [Commodore BASIC](https://www.valoroso.it/file-share/documenti-manuali/Commodore-Plus4-user-manual.pdf) to write little programs that automated repetitive parts of their work.

The programs didn't need a market. They didn't need other users. My parents had something they wanted the computer to do, and they wrote a program to do it. That always seemed like a reasonable future for programming: everybody should be able to make the computer do the things that would make their own life easier.

I've been building tools for myself with AI, and that old idea feels practical again. I can make an app around something I need, then change it as I use it. But keeping that app on my phone still means dealing with tools and processes organized around development, testing, and distribution. I want making software for myself to become an ordinary way to use the computer. Having a use for a program should be enough reason to make it.

## I thought the language would do it

When I was working on distributed systems, I spent a lot of time studying programming languages and models. I was interested in how much of the work a person could express directly, and how much machinery they still had to build around it.

[APL](https://mastering.dyalog.com/Introduction.html) let a programmer work on whole arrays of data with compact mathematical expressions, instead of writing a loop for every operation. [COBOL](https://www.ibm.com/think/topics/cobol) brought English-like statements and descriptions of business records into programming. They offered different ways to bring the program closer to the calculation or business process someone had in mind.

[MUMPS](https://en.wikipedia.org/wiki/MUMPS), the Massachusetts General Hospital Utility MultiProgramming System, brought the database into the programming environment. Programs could read and write persistent, shared data through the language itself. That was the connection that interested me: someone building an information system could work directly with stored records, with the database handling persistence.

Excel is an everyday example of this idea. You can build a budget by writing formulas that relate income, expenses, and a remaining balance. You specify the relationships, and [Excel tracks the dependencies and recalculates](https://learn.microsoft.com/en-us/office/vba/excel/concepts/excel-performance/excel-improving-calculation-performance) as the inputs change. You don't have to write the sequence of updates yourself. That's a form of declarative programming, even if the person making the spreadsheet never calls it programming.

These aren't all declarative systems. What connects them for me is the attempt to let people express more of their own work while the computer handles more of the mechanics. A spreadsheet also shows that making useful software for yourself is something people already do.

Later, I wrote about [Hermes](/pl/2016/03/05/hermes.html), a system developed at Digital Equipment Corporation for invoking objects without requiring the caller to keep track of their location. One of its motivating applications involved expense forms moving through an organization. I also wrote about [Argus](/pl/2016/08/08/argus.html), [Barbara Liskov and her colleagues' work](https://css.csail.mit.edu/6.824/2014/papers/argus88.pdf) on language support for distributed programs, including failures and atomic actions.

My own work on [Lasp](/erlang/lasp/2014/12/21/lasp.html) was part of this interest. Within its model, programmers could combine computations over replicated data without implementing convergence themselves. I wanted the system to take care of recurring distributed-systems work that otherwise fell to each application.

I expected progress to look like better abstractions: a better language, a better runtime, a better way to express the program.

Now I'm building apps with AI, including GPT-6 Astra, and I'm starting to see that future arrive through a route I didn't expect. I can describe the tool I want, try it, and describe what needs to change. The language and runtime still matter, but much more of my interaction with the programming process happens through the thing I'm trying to accomplish.

## An app for the hotel

I wanted an iPhone app I could open at a hotel while traveling to [Phish](https://phish.com/) shows and use to find somewhere to eat. With [celiac disease](https://www.niddk.nih.gov/health-information/digestive-diseases/celiac-disease), that takes more work than opening a delivery app and choosing something that looks good. There was a point when I wasn't sure I'd be able to keep doing these trips.

I already use [Find Me Gluten Free](https://info.findmeglutenfree.com/). I also use food delivery. What I wanted was the intersection: reviews of nearby restaurants, including their [safety ratings](https://info.findmeglutenfree.com/star-vs-safety-ratings), matched to the same restaurant location on [DoorDash](https://www.doordash.com/) or [Uber Eats](https://www.ubereats.com/).

The point is to get a meal I feel safe eating delivered to the hotel or wherever I'm staying, so I can eat before the show and then head out to Shakedown.

So I built it with AI. It's called [Divided Rye](/apps/divided-rye/). (A [*Divided Sky*](https://phish.com/song/divided-sky/) pun, which I'm very happy with.) In five hours, I had a version I could use on my phone. I've kept adding to it since.

The app uses my location, finds nearby restaurant listings, and looks for the corresponding delivery locations. I can see the distance and ratings, choose a delivery service, and star a branch I've had good luck with. It hides places it can explicitly identify as closed. If it can't determine the hours, it says so.

{% include app-shot.html src="/images/divided-rye/nearby.png" alt="Divided Rye showing nearby restaurant ratings, distance, and a delivery link" caption="The question I wanted to answer from the hotel: which nearby places have gluten-free reviews and a matching delivery location?" %}

This also turned out to be a useful example of what building with AI actually involves. Early versions sometimes showed no restaurants, and they missed chains I already knew worked for me. We changed how it found delivery links, added caches so it wouldn't search for the same branch every time I opened the app, and checked the results at a fixed Denver location before adding GPS back into the test.

The work included debugging the thing I wanted to use. A restaurant review doesn't establish that a particular meal is safe, and matching a branch doesn't establish that it delivers to my hotel. Those distinctions have to survive the interface. Generating more code doesn't remove them.

The result is now on my phone. I brought my own experience to the debugging, and we used infrastructure I already had through Zabriskie, the social app I've been building. What surprised me was how quickly I could get to a tool worth using and keep changing it around what I needed.

## The same thing, for a film

[Criterion 24/7](/criterion-247/) took another five hours to get to a usable version, and I've kept building on that too. I rarely remember to check the [Criterion Channel's continuous stream](https://www.criterionchannel.com/events/criterion-24-7) when I'm just sitting around. By the time I look, something I wanted to watch might already be over.

I wanted a history of what I'd missed, a way to mark films to revisit on the Channel later, and notifications when a new film was starting so I could tune in. Now I have widgets on my iPhone, iPad, and Mac desktop showing what's playing. The app can notify me about films starting and when something I saved to watch comes on.

{% include app-shot.html src="/images/criterion-247/iphone-current.png" alt="Criterion 24/7 on iPhone showing the current film and time remaining" caption="Criterion 24/7 lets me catch what's starting and save what I missed for later." %}

I also wanted to go straight from watching a film to posting a review on [Zabriskie](https://zabriskie.app/), my social network, to share it with my friends. Criterion can open Zabriskie for me to write that post. Zabriskie can use those posts, along with people's interests and cultural connections, to help them find other media they might like.

That lets the small app do the part I wanted for myself, while Zabriskie handles the shared conversation and discovery.

<figure class="app-screenshot" style="max-width: 640px;">
  <a href="/images/zabriskie/lot-connection-2026-09-06.png"><img src="/images/zabriskie/lot-connection-2026-09-06.png" alt="A Connections card in Zabriskie's Lot linking my post about the Grateful Dead album American Beauty to another person's post about the band's debut album." loading="lazy" /></a>
  <figcaption>The same discovery works across music, too. Here, The Lot connects my post about American Beauty to someone else's post about the Dead's first album.</figcaption>
</figure>

I've put more screenshots and details on the [pages for these apps](/apps/).

## Software getting out of the way

I wrote in [*Rift*]({% post_url 2026-05-03-rift %}) about what I'd lost as programming became managing agents. Programming had been something I loved doing, with Phish on, for thirty years. Supervising a queue of agents didn't give me the same thing. So building these apps faster isn't enough on its own. If I [spend the show managing the software that was supposed to help me enjoy it]({% post_url 2026-09-04-the-quickest-path-to-a-diff %}), I haven't accomplished what I wanted.

With Divided Rye, I want to find dinner and head out. With Criterion, I want to catch a film.

With Zabriskie, I can find a historical show played on this date and stream it. For tonight's show, I can see which friends are going, when it starts, and where they're sitting, then chat with them while the band plays. Those are the things I wanted the software for: listening to music, finding my friends, and sharing the show with them.

The agent can help me build the tool and change it when I need to. Then I want to use it and get on with the evening. That's what I mean by software getting out of the way. Making and managing it should take up less of my life, so I can spend more of it doing the thing I wanted the software for.

## Keeping it on my phone

Once I've built one of these tools, I want to put it on my phone, change it when I need to, and keep using it.

There are ways to do this without publishing to the App Store. Apple supports [direct installation on registered devices](https://developer.apple.com/help/account/devices/devices-overview/) through its developer program. With a free Personal Team, the provisioning profiles expire after [seven days](https://developer.apple.com/help/account/basics/about-your-developer-account). A TestFlight build can be tested for [up to ninety days](https://developer.apple.com/help/app-store-connect/test-a-beta-version/testflight-overview/). I've been using development installs and TestFlight builds. They work. They also leave me managing a development or testing lifecycle for an app I simply want to use.

I'm still waiting on App Store approvals for tools I was able to build in hours. Getting them into the store has taken days, with an automated agent walking through the [submission boilerplate](https://developer.apple.com/app-store/submitting/) along the way.

Having an agent handle the process helps. It also makes the mismatch more apparent: we can automate much of the work of preparing a product for distribution, while the simpler intention of keeping a personal program still lives inside that developer workflow.

I want the phone to treat a personal app as something I can keep. Give it the permissions it needs, preserve its data when I change it, and let me keep using it without treating it as a test build. Sharing it with other people could be a separate decision I make later. Using it myself should be an ordinary place to start.

## Building for myself, sharing with others

When we first built [Zabriskie](https://zabriskie.app/), I wanted [a social place for people who went to shows]({% post_url 2026-03-08-why-im-building-zabriskie %}), people who couch toured, and people who did some of each. That depends on a shared place. Other people being there is part of what makes it useful.

<figure class="app-screenshot" style="max-width: 640px;">
  <a href="/images/zabriskie/cmeik-lot-2026-09-06.png"><img src="/images/zabriskie/cmeik-lot-2026-09-06.png" alt="My Lot as cmeik, showing tonight's Phish show at Dick's, the countdown to doors, my AC/DC Bag opener prediction, and people attending or watching from home." loading="lazy" /></a>
  <figcaption>My Lot before Phish at Dick's: a countdown to doors, my opener call, and people going to the show or joining from the couch.</figcaption>
</figure>

We also built smaller things into it, including a way to see the current setlist and put it on a watch. That display can be useful to me even if nobody else uses the same interface. We still need a source for the setlist, but we don't all need to look at it the same way.

{% include app-shot.html src="/images/zabriskie/watch-now-playing.png" alt="Zabriskie on Apple Watch showing the current song and reaction buttons during a simulated show" caption="The current song and reactions on Apple Watch, shown here during a demo show." %}

I've also noticed other setlist apps, including [OnStage](https://www.reddit.com/r/phish/comments/1vccvle/onstage_live_phish_setlist_ios_app/) and [Flodown](https://www.reddit.com/r/GoosetheBand/comments/1qrens5/new_goose_app_focused_on_live_shows/), being posted and promoted. OnStage focuses on live setlist updates; Flodown also has community features. They make different choices about what a fan might want. I can see reasons to use either, or to want a version that works a little differently.

And the work needed to make another version is changing. Take [Foul Domain](https://fouldomain.com/), a Phish statistics site. Its creator [describes doing very little coding by hand](https://www.reddit.com/r/phish/comments/1s1mume/comment/ocb4iz7/), spending their time defining requirements, making architecture decisions, and testing instead. That's much closer to how I've been working too.

With to-do lists and finance apps, we've spent years choosing among other people's versions of what we need. Each comes with someone's idea of the right way to work. You pick the one that fits your workflow best, live with the parts that don't, and hope the maintainer keeps moving in a direction you like. People debate which app is better when what they often mean is which app fits the way they work.

What happens when making the version that fits you becomes practical?

This is the part of programming I see becoming commoditized: turning a small, specific requirement into working software. As that gets cheaper, I have another option when an existing app almost fits. I can make the version I wanted, and its usefulness to me can be enough to justify the work.

That changes what success looks like. Divided Rye has a reason to exist if it helps me eat before a show. It doesn't need a growth strategy to justify the time I spent on it. There must be so many small things people would like their computers to do that will never support a business. I want those things to be worth building too.

To some extent, Zabriskie is the same thing. It's what I want from a social app and a media diary. Other people seem to like it too, but they don't all use it the way I do. That makes sense. I built it around my intentions, and they bring their own. Something made for me can turn out to be useful to someone else without fitting them exactly.

Facebook reflects the intentions of the people building it too. So does a to-do app whose idea of the right workflow doesn't match mine. Zabriskie isn't exempt from that just because I built it. What interests me is more people getting to make those decisions for themselves, instead of always choosing among the decisions already made for them.

I'd still pay for useful services or a well-maintained app. Divided Rye depends on restaurant reviews and delivery services; Criterion depends on a stream and a backend that keeps its history. A personal interface can sit on top of shared infrastructure, even when the people using that infrastructure want different things from it.

## How long does it need to last?

Maintenance is still a huge part of this. I've written about [a feature that was quick to build and kept breaking]({% post_url 2026-04-03-the-feature-that-has-never-worked %}), and about [changes that looked reasonable individually but brought down the application together]({% post_url 2026-09-04-the-quickest-path-to-a-diff %}). I've seen agents produce software that is much easier to add to than to keep working. Getting an app onto my phone in five hours doesn't tell me what it will cost to live with it for five years.

But some personal software might only need to last a weekend. I could make a little tool to organize one trip, use it, and be done. If building it becomes cheap enough, that could be worthwhile even if I never maintain it again. Disposable software, in that sense, could be a useful outcome: it served its purpose.

Divided Rye might become something I use for years. A tool for one person can still matter a great deal to that person. What changes is that I can decide when its job is done. I may want to keep the restaurant notes or the films I saved even if I replace the app. A temporary tool still needs to work while I depend on it, and replacing it shouldn't mean losing what I put into it.

Zabriskie and the services these apps rely on have a different obligation. Other people depend on them continuing to work, on their data surviving, and on changes not breaking yesterday's behavior. Cheap code doesn't remove that work. The possibility of a disposable personal tool doesn't make a shared service disposable.

This is another thing I want the platform to support: keeping a useful program, replacing it while preserving my data, or retiring it when I'm finished. I don't yet know whether maintaining my own apps will be as manageable as building them. Making that manageable is part of the work ahead.

## Start with what I need

Sam Altman has described agents as [virtual coworkers](https://blog.samaltman.com/three-observations), including agents that do software engineering. There's overlap with what I'm describing, but the part that interests me is what I have when the conversation ends. For a recurring need, I want the agent to leave me with an app whose behavior I can try, adjust, and return to. With Criterion, that means a widget I can glance at and saved films I can come back to. When my needs change, I can bring the agent back to change the software.

The agent helps me build something that does the job. I don't need every interaction with my computer to become another task to delegate and supervise.

Imagine opening the computer and saying that you need a way to manage your finances. It asks where the information is, you choose what it can access, and you work together on how you want to see it. You tell it that you get paid irregularly, that you put money aside for travel, or that the usual monthly budget doesn't make sense for you. It builds around that. A month later, when you understand what you need a little better, you change it.

I want that to be practical for someone who has never worked as a programmer. My own experience doesn't establish that we're there yet. It does make the goal much more concrete for me. A person should be able to bring their understanding of their own life to the computer and use that to shape how it works. They should be able to try something, live with it, and change their mind without having to convince a software company that enough other people want the same thing.

The social case is further out, but I want to push the idea there too. If people could keep their own data and make it available on their own terms, I could ask for the films my friends were talking about, the shows we were going to together, and the books I meant to come back to, all in a view that made sense to me. Someone else could make a different view of the things shared with them. Shared services would still have work to do, but we could have more say in how we encountered one another's lives through them.

Watching my parents write BASIC made me think that making a program could be an ordinary part of using a computer. Now I have a restaurant app on my phone because I wanted to keep going to shows. I want more people to be able to make the things that let them do what matters to them, use them for as long as they help, and get on with their lives. We should build our tools and our platforms around that possibility. A program for one person is enough.
