---
layout: page
title: "The Machine in the Lab"
description: "How I used an autonomous LLM research program to build a live Goose song guesser, discarded two invalid notebooks, and began testing it during a real tour."
permalink: /series/the-machine-in-the-lab/
series: lab
image: /og/the-machine-in-the-lab-editorial.png
image_card_type: summary_large_image
---

<p class="series-landing-lead">I wanted to build a tool that could tell Goose fans what song they were hearing while a show was still happening. I also wanted an autonomous LLM system to do the research needed to build it. The system would propose experiments, write and run code, evaluate the results, and decide what to try next. The tool became SetScope.</p>

<p class="series-landing-meta">
  By my estimate, the agent compressed weeks of implementation into hours. It also helped produce two polished research notebooks that I later discarded. One leaked recordings between training and test. The other analyzed the wrong portions of the songs because it inherited a 90-second segmentation rule that contradicted the method I had written. Both came with clean code, persuasive charts, and conclusions that sounded finished.
</p>

<p class="series-landing-meta">
  I have a PhD and years of systems-research experience. That background did not prevent me from accepting either result. The project began with automatic song and set detection. The recordings pulled it into harder questions about improvisation. After both notebooks failed, I returned to the original goal: build the live song guesser.
</p>

<p class="series-landing-meta">
  On August 13, we ran SetScope during a Goose show whose audio had not existed during development. The saved record shows that SetScope produced the right song at least once for 10 of the 12 performances. It also missed two performances and switched songs three times when it should not have. The record is incomplete, so those numbers do not amount to a whole-show accuracy score.
</p>

<p class="series-landing-meta">
  This series will follow that story from beginning to end. It starts with what the autonomous researcher made possible and how it convinced me twice that invalid work was finished. It then explains how a review system in which every check passed still accepted the wrong interpretation of the audio. After the eleven-show August 13-28 run, the final post will report which versions actually ran, how quickly correct guesses appeared, where the system failed, and what the surviving records can establish about the viewer-facing system.
</p>

<p class="series-landing-meta">
  This is not a prompting guide. It is for people using LLMs to run experiments or build the systems around them. The question throughout is simple: when an agent can produce the next result by itself, what evidence gets to say that the result is wrong?
</p>

{% if jekyll.environment == "development" %}
<p class="series-landing-meta">
  <strong>Editorial review:</strong> <a href="/series/the-machine-in-the-lab/editorial-plan/">Read the seven-part outline</a> and the <a href="/series/the-machine-in-the-lab/series-architecture/">full argument map</a> before reviewing the individual drafts.
</p>
{% endif %}

<section class="series-landing-list" aria-label="Series posts">
  {%- assign published_urls = site.posts | where: "series", "lab" | map: "url" -%}
  <ol>
    {%- for entry in site.data.lab_series -%}
      {%- capture part_prefix -%}Part {{ forloop.index }}. {% endcapture -%}
      {%- assign is_published = false -%}
      {%- for u in published_urls -%}
        {%- if u == entry.url -%}{%- assign is_published = true -%}{%- break -%}{%- endif -%}
      {%- endfor -%}
      <li class="series-landing-item{% unless is_published %} series-landing-pending{% endunless %}">
        {%- if is_published -%}
          <a href="{{ entry.url }}" class="series-landing-link">
            <span class="series-landing-copy">
              <span class="series-landing-title">{{ entry.title | remove_first: part_prefix }}</span>
              <span class="series-landing-subtitle">{{ entry.subtitle }}</span>
            </span>
            {%- if entry.date -%}<span class="series-landing-date">{{ entry.date | date: "%B %-d, %Y" }}</span>{%- endif -%}
          </a>
        {%- else -%}
          <span class="series-landing-link series-landing-link-pending">
            <span class="series-landing-copy">
              <span class="series-landing-title">{{ entry.title | remove_first: part_prefix }}</span>
              <span class="series-landing-subtitle">{{ entry.subtitle }}</span>
            </span>
            <span class="series-landing-date">{% if entry.status == "collecting-results" %}collecting tour results{% elsif entry.status == "blocked-on-result" %}blocked on evidence{% elsif entry.date %}publishes {{ entry.date | date: "%B %-d" }}{% elsif entry.status == "source-draft" %}source draft{% elsif entry.status == "draft" %}draft in progress{% else %}planned{% endif %}</span>
          </span>
        {%- endif -%}
      </li>
    {%- endfor -%}
  </ol>
</section>

{% include subscribe.html %}

<p class="series-landing-back">
  <a href="/">&larr; Home</a> &middot; <a href="/archive.html">Full archive &rarr;</a>
</p>
