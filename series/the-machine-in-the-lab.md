---
layout: page
title: "The Machine in the Lab"
description: "A PhD researcher's field report on using LLM agents for an ambitious audio project, losing two notebooks, returning to a live song guesser for fans, and following it through a Goose tour."
permalink: /series/the-machine-in-the-lab/
series: lab
---

<p class="series-landing-lead">By my estimate, in this project an LLM agent compressed weeks of implementation into hours. It also helped compress weeks of invalid research into hours, then handed it back as clean code, persuasive charts, and a conclusion that looked finished.</p>

<p class="series-landing-meta">
  This is a field report from an audio-research project that lost two notebooks to data leakage and an inherited segmentation default, built an elaborate review system, watched that system miss a different class of error, and eventually developed a workflow that could preserve failures instead of explaining them away.
</p>

<p class="series-landing-meta">
  I came to the project with a PhD and years of systems-research experience. I was trying to find out whether an LLM agent could make a serious independent research program possible at a scale I could not manage alone. The project started as automatic set detection, detoured into questions about improvisation that were much harder to answer cleanly than they appeared, and eventually returned to its original target: a tool that listens to a live Goose show and tells viewers which song is probably playing. On August 13, SetScope emitted correct song identities while the show was happening, using audio that had not existed when the system was built. Those guesses were the output of a fan-facing product, not scientific findings. The saved record later found correct identities emitted at least once for 10 of 12 song performances. A post-show diagnostic also found two misses and three false switches, but capture and scoring problems prevent those observations from becoming a whole-show accuracy, stability, or viewer-benefit estimate.
</p>

<p class="series-landing-meta">
  The final post will return after the eleven-show August 13-28 western run with the longer product record: which versions ran, how quickly correct guesses appeared, where the system switched or stayed wrong, when capture failed, and what the screen and publication paths actually produced. That retrospective will report the defined August run rather than the complete summer tour, and it will not compress evolving software into one flattering score. Any claim that it made shows easier to follow will remain first-person or qualitative unless we collect direct viewer evidence.
</p>

<p class="series-landing-meta">
  <strong>Who this is for:</strong> people using LLMs to design experiments, write analysis code, operate research infrastructure, or interpret results. This is not a prompting guide. It is about information boundaries, measurement validity, execution validity, hidden state, and which decisions require durable permissions and evidence outside the agent's own artifact chain.
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
      {%- assign is_published = false -%}
      {%- for u in published_urls -%}
        {%- if u == entry.url -%}{%- assign is_published = true -%}{%- break -%}{%- endif -%}
      {%- endfor -%}
      <li class="series-landing-item{% unless is_published %} series-landing-pending{% endunless %}">
        {%- if is_published -%}
          <a href="{{ entry.url }}" class="series-landing-link">
            <span class="series-landing-title">{{ entry.title }}</span>
            {%- if entry.date -%}<span class="series-landing-date">{{ entry.date | date: "%B %-d, %Y" }}</span>{%- endif -%}
          </a>
        {%- else -%}
          <span class="series-landing-link series-landing-link-pending">
            <span class="series-landing-title">{{ entry.title }}</span>
            <span class="series-landing-date">{% if entry.status == "source-draft" %}source draft{% elsif entry.status == "draft" %}draft in progress{% elsif entry.status == "collecting-results" %}collecting tour results{% elsif entry.status == "blocked-on-result" %}blocked on evidence{% else %}planned{% endif %}</span>
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
