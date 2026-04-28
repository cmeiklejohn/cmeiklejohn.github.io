---
layout: page
title: "Zabriskie"
description: "A non-profit social platform for live music — couch-touring, in-show chat, multi-show parallel viewing. Built almost entirely with AI agents. Notes on what works, what breaks, and what shipping production code through agents actually looks like."
permalink: /zabriskie/
---

<p class="topic-landing-lead">
  Zabriskie is a non-profit social platform for live music — couch-touring, in-show chat, multi-show parallel viewing. Built almost entirely with AI agents. The blog posts below are about both the product and what I learn shipping with agents, in roughly equal measure.
</p>

<p class="topic-landing-meta">
  <strong>Live:</strong> <a href="https://zabriskie.app">zabriskie.app</a> &nbsp;·&nbsp;
  <strong>Code:</strong> <a href="https://github.com/cmeiklejohn/zabriskie">github.com/cmeiklejohn/zabriskie</a>
</p>

<p class="topic-landing-meta">
  If you're a jam-band fan who tours, this is for you. If you're an engineer curious about what shipping a real product through Claude Code and Cursor actually looks like, also for you. The two audiences overlap more than you'd think.
</p>

<aside class="related-posts" aria-label="Zabriskie posts">
  <p class="related-posts-label">Posts about Zabriskie</p>
  <ul class="related-posts-list">
    {%- assign zabriskie_posts = site.posts | where_exp: "p", "p.categories contains 'zabriskie'" | sort: "date" | reverse -%}
    {%- for post in zabriskie_posts -%}
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
