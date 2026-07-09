---
layout: post
title:  "Something Went Wrong. Please Try Again."
subtitle: "A cancelled flight, three confident machines, and the woman on the Hilton Diamond line who refused to stop."
date:   2026-07-08 12:00:00 -0700
group: ai
categories: ai personal agents reliability testing
---

Toronto to Detroit to Norfolk. Two nights of Goose at the Dome. I have made this trip, or one shaped like it, more times than I can count.

On Sunday my connection to Norfolk slipped an hour, then another, then another. At 11:30 PM Delta cancelled it along with hundreds of other flights, said the next seat out was Tuesday, and told us to sleep in the airport. The run was Sunday and Monday. Whatever happened next, night one was already gone.

Everything near the terminal was full. So I called outward.

## The hotel that knew the date

Sunday night, 12:41 AM. The hotel's AI knew the date better than I did. That is how it tried to book me the wrong night.

I did not know it was an AI. It sounded like a person. It paused before answering, the way you pause when you are reading something off a screen. I had been in the Detroit airport for nine hours, and for eight minutes I believed I was talking to someone having a hard night too.

"Sunday night through Tuesday."

"Great, checking in Monday at 3:00 PM."

"No. Tonight. I need a room tonight."

"Tonight is Monday. Checking in Monday, departing Wednesday."

"Two nights, starting Sunday."

"Okay, Sunday. But tonight is just after Sunday midnight, so it's Monday. Two nights, starting Monday, to Wednesday. Shall I book that for you?"

Read that last one again. It was right. It knew it was past midnight, it knew that made the date Monday, and it offered me a room for a night I would spend in Virginia. The night that started on Sunday is still going. Every person alive knows this and no one has ever had to say it out loud.

I raised my voice at it. When I finally understood what I was talking to, my first feeling was not anger. It was embarrassment. Someone chose those pauses. They spent real effort making it sound like a person who could help me, and none at all on the hour when a person would have to.

Nothing was broken. Nothing failed, nothing crashed, nothing was even misunderstood. It did exactly what it was built to do, on an input nobody thought to try, confidently and fluently and kindly the whole way down. And 12:41 AM is the only hour anyone calls a hotel they never planned to stay in. So either nobody tested that call, or somebody did and shipped it anyway.

## The phone number

I gave up on calling hotels one at a time and called the Hilton Diamond line instead. A woman answered and understood in one sentence. Flight cancelled, airport hotels full, need a room tonight, near the Detroit airport, tonight meaning the night that started on Sunday. No confusion about the date. She started pulling up my account.

Before she could book anything, she needed a phone number on the account. There wasn't one. Fifteen years of stays and apparently I had never given Hilton a phone number, and the booking could not proceed without it, and she could not add it for me. I had to add it myself, from my phone, in the terminal, at one in the morning.

Something went wrong. Please try again.

I tried again. Something went wrong. Please try again.

I spent my PhD on that sentence. Not the words, the thing underneath them. On why distributed systems fail in ways nobody tests for. On what happens when a request crosses four services and one of them times out and the caller has no idea which one, or why, or whether retrying is safe. On building tools that force a system into that state on purpose, before a customer finds it at 1:00 AM in Detroit.

And there it was. Four words. No error code. No correlation ID. No indication of whether the write had partially landed, so no way to know whether trying again would help or make it worse. I tried again anyway. What else is there.

This is a different failure from the hotel bot, and it is worth being precise about the difference. The bot was a testing failure: a system that understood everything I said and still could not help me, because nobody enumerated the one hour when its only real customers would call. The error screen is an observability failure: a system that knew something had gone wrong, knew where, knew what, and told me nothing. Somewhere in Hilton's infrastructure there is a log line with the truth in it. The one person who needed that truth, standing in a terminal trying to give a company his phone number so it would let him pay for a room, got four words and a suggestion to try again.

## The security questions

The phone number never saved. So I got back in the queue for another human, to add a phone number by hand. That took a long time. When I reached her, she had to verify my identity first.

Her: What was the last hotel you stayed at?

Me: The one in Toronto. I checked out of it yesterday morning.

Her: What is its street address?

Me: I don't know. It was a hotel. I slept there and I left.

Her: I need the address to verify your identity.

Me: I am going to google it, and then I am going to read it back to you, and we are both going to pretend that proved something.

She accepted the address. I found it in nine seconds, on the open internet, on the same phone that could not update my own account. A stranger with my name and a search bar could have done the same thing. The system was not verifying that I was me. It was verifying that I was willing to perform the ritual, at 1:00 AM, with a dead phone battery and no room, and I was.

## Three machines, one pattern

That is the whole night, and it is one pattern three times. A voice bot that parsed every word I said and booked the wrong night. An error screen that knew exactly what failed and said nothing. A security check that verified my identity with a fact anyone on earth could look up in nine seconds. Three systems, each of which had exactly enough information to be confident and not enough to be right.

None of them were broken. That is the part I keep coming back to. Every one of them did precisely what it was built to do. The failures were not in the systems. They were in the space between what got built and what got tested, and that space is invisible until a customer is standing in it. My research career was about finding those states on purpose, injecting the fault before the customer trips over it, and I could not tell you a cleaner set of examples than the ones I collected involuntarily in a single hour: the input nobody enumerated, the error nobody instrumented, the check nobody thought about from the attacker's side. You do not find these by testing the happy path harder. You find them by asking, specifically and adversarially, what the worst hour of your customer's night looks like, and then dialing in from inside it.

## The woman on the Diamond line

Identity verified, phone number added by hand, back to the original problem: it is now well past one in the morning and I have nowhere to sleep.

She started calling hotels herself. One at a time, with me on hold. The first two were full. On the fourth she found a room in a suburb of Detroit.

She had no clever solution. There was no system on her side doing anything smart, no availability dashboard collapsing the search into one query. She just refused to stop until the problem was gone, which is the one behavior nobody has figured out how to ship.

Think about what she actually did, against the three machines that came before her. The bot understood my words and not my situation; she understood the situation in one sentence. The error screen knew the truth and would not say it; she narrated everything she was doing, hold music and all, so I always knew where things stood. The security questions performed verification without verifying; she performed none of the ritual and simply took responsibility for the outcome. Every quality the automation was imitating, she had the real version of, and the real version is not a feature. It is a person who has decided your problem is now her problem.

## Service is the product

Service is not a cost center waiting to be automated. Service is the product. It exists precisely for the night the plan falls apart. Every company putting a bot on that line has decided the only calls worth answering are the easy ones. Nobody calls at 12:41 AM with an easy one. The hard call is the whole job.

I build this stuff. I research LLM agents. I am not against any of it. The bot that answered that hotel phone will get better, and the error screen could be fixed by one engineer in an afternoon, and I will keep working on exactly these problems, because they are solvable. I am against shipping it untested into the exact moment a customer needs it most, and then calling the person who cleans up afterward the cost center.

And perhaps that is why we still pay for service. Not for the room. For the person who picks up when the room is gone.

I did not sleep in the airport. She stayed on the phone until I had somewhere to go.
