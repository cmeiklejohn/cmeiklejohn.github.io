/**
 * Bluesky comments — fetches a post's reply thread from the public Bluesky
 * AppView and renders it under the matching blog post.
 *
 * Activated by the presence of a `<section class="bsky-comments">` element
 * with `data-bsky-uri="at://..."` (provided by _includes/bluesky-comments.html).
 *
 * No auth required; the AppView serves public threads anonymously. If the
 * fetch fails (offline, rate limit, post deleted), we fall back to a
 * "View on Bluesky" link.
 */
(function () {
  "use strict";

  var APPVIEW = "https://public.api.bsky.app/xrpc/app.bsky.feed.getPostThread";

  function $(tag, attrs, children) {
    var el = document.createElement(tag);
    if (attrs) {
      Object.keys(attrs).forEach(function (k) {
        if (k === "class") el.className = attrs[k];
        else if (k === "text") el.textContent = attrs[k];
        else el.setAttribute(k, attrs[k]);
      });
    }
    (children || []).forEach(function (c) {
      if (c) el.appendChild(c);
    });
    return el;
  }

  function escapeHtml(s) {
    return (s || "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function linkifyText(text, facets) {
    // Bluesky text uses byte offsets in facets. Sort by byteStart, walk the
    // string, emit links for facet ranges and plain text for the rest.
    if (!facets || !facets.length) return escapeHtml(text);
    var encoder = new TextEncoder();
    var bytes = encoder.encode(text);
    var sorted = facets.slice().sort(function (a, b) {
      return a.index.byteStart - b.index.byteStart;
    });
    var pieces = [];
    var cursor = 0;
    sorted.forEach(function (facet) {
      if (facet.index.byteStart > cursor) {
        pieces.push(escapeHtml(new TextDecoder().decode(bytes.slice(cursor, facet.index.byteStart))));
      }
      var slice = new TextDecoder().decode(bytes.slice(facet.index.byteStart, facet.index.byteEnd));
      var feature = (facet.features || [])[0] || {};
      if (feature.$type === "app.bsky.richtext.facet#link") {
        pieces.push(
          '<a href="' + escapeHtml(feature.uri) + '" rel="external nofollow">' + escapeHtml(slice) + "</a>"
        );
      } else if (feature.$type === "app.bsky.richtext.facet#mention") {
        pieces.push(
          '<a href="https://bsky.app/profile/' + escapeHtml(feature.did) + '" rel="external">' + escapeHtml(slice) + "</a>"
        );
      } else if (feature.$type === "app.bsky.richtext.facet#tag") {
        pieces.push(escapeHtml(slice));
      } else {
        pieces.push(escapeHtml(slice));
      }
      cursor = facet.index.byteEnd;
    });
    if (cursor < bytes.length) {
      pieces.push(escapeHtml(new TextDecoder().decode(bytes.slice(cursor))));
    }
    return pieces.join("").replace(/\n/g, "<br>");
  }

  function relativeTime(iso) {
    var t = new Date(iso).getTime();
    if (isNaN(t)) return "";
    var diff = (Date.now() - t) / 1000;
    if (diff < 60) return "just now";
    if (diff < 3600) return Math.floor(diff / 60) + "m ago";
    if (diff < 86400) return Math.floor(diff / 3600) + "h ago";
    if (diff < 86400 * 30) return Math.floor(diff / 86400) + "d ago";
    return new Date(iso).toLocaleDateString(undefined, { month: "short", day: "numeric", year: "numeric" });
  }

  function postUrl(uri, handle) {
    var rkey = uri.split("/").pop();
    return "https://bsky.app/profile/" + handle + "/post/" + rkey;
  }

  function renderReply(reply, depth) {
    if (!reply || reply.$type !== "app.bsky.feed.defs#threadViewPost") return null;
    var post = reply.post;
    if (!post || !post.author) return null;

    var author = post.author;
    var record = post.record || {};
    var handle = author.handle || "unknown";
    var displayName = author.displayName || handle;
    var avatar = author.avatar;

    var avatarEl = avatar
      ? $("img", { class: "bsky-comment-avatar", src: avatar, alt: "", loading: "lazy", width: "36", height: "36" })
      : $("span", { class: "bsky-comment-avatar bsky-comment-avatar-fallback", "aria-hidden": "true" });

    var nameEl = $("a", {
      class: "bsky-comment-name",
      href: "https://bsky.app/profile/" + handle,
      rel: "external",
      text: displayName,
    });

    var handleEl = $("span", { class: "bsky-comment-handle", text: "@" + handle });

    var timeEl = $("a", {
      class: "bsky-comment-time",
      href: postUrl(post.uri, handle),
      rel: "external",
      title: record.createdAt || "",
      text: relativeTime(record.createdAt),
    });

    var headerEl = $("div", { class: "bsky-comment-header" }, [nameEl, handleEl, timeEl]);

    var bodyEl = $("div", { class: "bsky-comment-body" });
    bodyEl.innerHTML = linkifyText(record.text || "", record.facets);

    var metaPieces = [];
    if (post.likeCount) metaPieces.push((post.likeCount === 1 ? "1 like" : post.likeCount + " likes"));
    if (post.repostCount) metaPieces.push((post.repostCount === 1 ? "1 repost" : post.repostCount + " reposts"));
    if (post.replyCount) metaPieces.push((post.replyCount === 1 ? "1 reply" : post.replyCount + " replies"));
    var metaEl = metaPieces.length
      ? $("div", { class: "bsky-comment-meta", text: metaPieces.join(" · ") })
      : null;

    var children = [headerEl, bodyEl];
    if (metaEl) children.push(metaEl);

    var commentEl = $("article", { class: "bsky-comment bsky-comment-depth-" + depth }, [
      $("div", { class: "bsky-comment-avatar-wrap" }, [avatarEl]),
      $("div", { class: "bsky-comment-content" }, children),
    ]);

    var nestedReplies = (reply.replies || [])
      .map(function (r) { return renderReply(r, depth + 1); })
      .filter(Boolean);

    if (nestedReplies.length) {
      var nestedEl = $("div", { class: "bsky-comment-replies" }, nestedReplies);
      commentEl.appendChild(nestedEl);
    }

    return commentEl;
  }

  function renderThread(section, thread, originalUrl) {
    var body = section.querySelector(".bsky-comments-body");
    body.innerHTML = "";

    var post = thread && thread.post;
    var likeCount = post ? post.likeCount || 0 : 0;
    var repostCount = post ? post.repostCount || 0 : 0;
    var replies = (thread && thread.replies) || [];

    if (likeCount || repostCount) {
      var summaryParts = [];
      if (likeCount) summaryParts.push(likeCount === 1 ? "1 like" : likeCount + " likes");
      if (repostCount) summaryParts.push(repostCount === 1 ? "1 repost" : repostCount + " reposts");
      var summary = $("p", { class: "bsky-comments-summary", text: summaryParts.join(" · ") });
      body.appendChild(summary);
    }

    if (!replies.length) {
      var empty = $("p", { class: "bsky-comments-empty" }, [
        document.createTextNode("No replies yet — "),
        $("a", { href: originalUrl, rel: "external", text: "be the first to reply on Bluesky" }),
        document.createTextNode("."),
      ]);
      body.appendChild(empty);
      return;
    }

    var list = $("div", { class: "bsky-comments-list" },
      replies.map(function (r) { return renderReply(r, 0); }).filter(Boolean));
    body.appendChild(list);

    var footer = $("p", { class: "bsky-comments-footer" }, [
      $("a", { href: originalUrl, rel: "external", text: "View full thread on Bluesky →" }),
    ]);
    body.appendChild(footer);
  }

  function renderError(section, originalUrl) {
    var body = section.querySelector(".bsky-comments-body");
    body.innerHTML = "";
    var p = $("p", { class: "bsky-comments-error" }, [
      document.createTextNode("Couldn't load the thread. "),
      $("a", { href: originalUrl, rel: "external", text: "View on Bluesky →" }),
    ]);
    body.appendChild(p);
  }

  function loadOne(section) {
    var uri = section.getAttribute("data-bsky-uri");
    var url = section.getAttribute("data-bsky-url");
    if (!uri) return;

    var endpoint = APPVIEW + "?uri=" + encodeURIComponent(uri) + "&depth=6";
    fetch(endpoint, { headers: { Accept: "application/json" } })
      .then(function (r) {
        if (!r.ok) throw new Error("HTTP " + r.status);
        return r.json();
      })
      .then(function (data) {
        if (!data || !data.thread) throw new Error("malformed thread");
        renderThread(section, data.thread, url);
      })
      .catch(function () {
        renderError(section, url);
      });
  }

  function init() {
    var sections = document.querySelectorAll(".bsky-comments");
    Array.prototype.forEach.call(sections, loadOne);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
