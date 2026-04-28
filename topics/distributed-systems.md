---
layout: page
title: "Distributed Systems"
description: "Notes on coordination, consistency, fault injection, and CRDTs — drawn from a Ph.D. on coordination-free programming and ten years of work on Lasp, Partisan, SyncFree, Filibuster, and verified vector clocks. The throughline of the blog."
permalink: /distributed-systems/
---

<p class="topic-landing-lead">
  This is the throughline of the blog. A decade of work on coordination-free programming, conflict-free replicated data types, fault injection, and the engineering practice of building distributed systems that don't lie about their state. The recent AI-agents writing keeps re-discovering problems I spent that decade staring at.
</p>

<p class="topic-landing-meta">
  <strong>The projects, in rough chronological order:</strong>
</p>

<ul class="topic-landing-projects">
  <li><strong>Verified Vector Clocks</strong> — a Coq-verified vector clock library, exported to Erlang for Riak. Posts walk through the experience of formalizing a distributed primitive in a proof assistant.</li>
  <li><strong>SyncFree / Lasp</strong> — a coordination-free distributed programming language. Five years of work, including the EU-funded SyncFree research project. Lasp lets you build distributed applications without coordination, using CRDTs as the underlying data model.</li>
  <li><strong>Partisan</strong> — a high-performance, fault-tolerant distributed runtime for Erlang. Replaces the default Erlang distribution layer with one designed for modern cloud environments.</li>
  <li><strong>Filibuster</strong> — service-level fault injection testing. Companion to my Ph.D. thesis. Production version at <a href="https://filibuster.cloud">filibuster.cloud</a>.</li>
  <li><strong>Programming Models for Distributed Computing</strong> — the seminar curriculum I taught, walking through the foundational papers in the field (Argus, Emerald, Hermes, Promises, etc.).</li>
</ul>

<p class="topic-landing-meta">
  <strong>The current connection to AI agents:</strong> see <a href="/ai/agents/distributed/zabriskie/2026/03/30/multi-agent-systems-have-a-distributed-systems-problem.html">Multi-Agent Systems Have a Distributed Systems Problem</a> — multi-agent LLM systems are rediscovering coordination problems the distributed-systems field solved in the 1970s, just with worse vocabulary.
</p>

<aside class="related-posts" aria-label="Distributed systems posts">
  <p class="related-posts-label">All distributed-systems posts</p>
  <ul class="related-posts-list">
    {%- assign distsys_groups = "SyncFree,Partisan,vvclocks,filibuster,filibuster-2,pl,Serverless,Macrometa" | split: "," -%}
    {%- assign distsys_posts = site.posts | where_exp: "p", "distsys_groups contains p.group" | sort: "date" | reverse -%}
    {%- for post in distsys_posts -%}
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
  <a href="/">← Home</a> · <a href="/archive.html">Full archive →</a> · <a href="/research.html">Research →</a>
</p>
