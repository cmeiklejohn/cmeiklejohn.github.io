---
layout: page
title: "Getting Up to Speed on Multi-Agent Systems"
description: "An eight-part series mapping the multi-agent LLM literature for readers coming in fresh — two waves of research, one outside disruption, and the questions the field is still trying to answer."
permalink: /series/multi-agent-systems/
series: mas
---

<p class="series-landing-lead">An eight-part series mapping the multi-agent LLM literature for readers coming in fresh. Two waves of research, one outside disruption that reshaped both, and the questions the field is still trying to answer.</p>

<p class="series-landing-meta">
  <strong>Who this is for:</strong> you already ship or evaluate LLM agents (tools, long context, basic eval loops) and want the <em>research</em> landscape in view. This is not an on-ramp to transformers or prompting fundamentals.
</p>

<p class="series-landing-meta">
  Read straight through, or jump in wherever the title pulls you. The first two parts set up vocabulary; parts 3–6 walk through specific clusters of papers; parts 7–8 zoom out to benchmarks and what's still open.
</p>

<section class="series-landing-list" aria-label="Series posts">
  {%- assign published_urls = site.posts | where: "series", "mas" | map: "url" -%}
  <ol>
    {%- for entry in site.data.mas_series -%}
      {%- assign is_published = false -%}
      {%- for u in published_urls -%}
        {%- if u == entry.url -%}{%- assign is_published = true -%}{%- break -%}{%- endif -%}
      {%- endfor -%}
      <li class="series-landing-item{% unless is_published %} series-landing-pending{% endunless %}">
        {%- if is_published -%}
          <a href="{{ entry.url }}" class="series-landing-link">
            <span class="series-landing-title">{{ entry.title }}</span>
            <span class="series-landing-date">{{ entry.date | date: "%B %-d, %Y" }}</span>
          </a>
        {%- else -%}
          <span class="series-landing-link series-landing-link-pending">
            <span class="series-landing-title">{{ entry.title }}</span>
            <span class="series-landing-date">publishes {{ entry.date | date: "%B %-d, %Y" }}</span>
          </span>
        {%- endif -%}
      </li>
    {%- endfor -%}
  </ol>
</section>

{% include subscribe.html %}

<p class="series-landing-back">
  <a href="/">← Home</a> · <a href="/archive.html">Full archive →</a>
</p>
