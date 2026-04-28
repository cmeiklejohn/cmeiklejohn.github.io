---
layout: page
title: "AI Agents"
description: "Notes on building production software with AI agents — Claude Code, Cursor, multi-agent systems. The view from inside the work: what breaks, what surprises, what the research literature gets right and wrong."
permalink: /agents/
---

<p class="topic-landing-lead">
  Notes on building real software with AI agents — Claude Code, Cursor, multi-agent systems. Half practitioner ("here's what broke this week"), half research lens ("here's what the literature gets right and wrong"). I'm shipping <a href="/zabriskie/">Zabriskie</a> almost entirely through agents and writing about it as I go.
</p>

<p class="topic-landing-meta">
  <strong>For the research landscape:</strong> the eight-part series <a href="/series/multi-agent-systems/">Getting Up to Speed on Multi-Agent Systems</a>.
</p>

<p class="topic-landing-meta">
  <strong>For the practitioner view:</strong> start with <a href="/ai/zabriskie/community/2026/03/08/why-im-building-zabriskie.html">Why I'm Building Zabriskie</a>, then <a href="/ai/zabriskie/development/2026/04/26/spring-tour-recap.html">Spring Tour Recap</a>, then <a href="/ai/agents/distributed/zabriskie/2026/03/30/multi-agent-systems-have-a-distributed-systems-problem.html">Multi-Agent Systems Have a Distributed Systems Problem</a>.
</p>

<aside class="related-posts" aria-label="Posts about AI agents">
  <p class="related-posts-label">All posts about AI agents</p>
  <ul class="related-posts-list">
    {%- assign agent_posts = site.posts | where_exp: "p", "p.categories contains 'agents'" | sort: "date" | reverse -%}
    {%- for post in agent_posts -%}
      <li>
        <a class="related-posts-link" href="{{ post.url }}">
          <span class="related-posts-date">{{ post.date | date: "%b %-d, %Y" }}</span>
          <span class="related-posts-title">{{ post.title }}</span>
          {%- if post.subtitle -%}
            <span class="related-posts-subtitle">{{ post.subtitle }}</span>
          {%- endif -%}
        </a>
      </li>
    {%- endfor -%}
  </ul>
</aside>

{% include subscribe.html %}

<p class="series-landing-back">
  <a href="/">← Home</a> · <a href="/archive.html">Full archive →</a>
</p>
