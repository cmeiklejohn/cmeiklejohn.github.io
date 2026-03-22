---
layout: post
title:  "Teaching Claude to QA a Mobile App"
date:   2026-03-23 00:00:00 -0000
group: ai
categories: ai zabriskie development android ios
---

> *"I see a red door and I want it painted black"*
> — The Rolling Stones, "Paint It Black"

Zabriskie runs on three platforms: web, iOS, and Android. The web gets tested by Playwright — 150+ E2E tests that run on every push. But the mobile apps had nothing. No automated QA, no visual regression checks, no way to know if either client was rendering correctly without manually clicking through every screen. I decided to fix that by teaching Claude to drive both mobile platforms, take screenshots, analyze them for issues, and file its own bug reports.

Android took 90 minutes. iOS took over six hours. The difference says everything about the state of mobile automation tooling in 2026.

## Android: The Easy One

The first challenge was connectivity. Inside the Android emulator, `localhost` refers to the emulator itself, not the host Mac. When the Capacitor app tries to reach `localhost:3000` or `localhost:8080`, it gets nothing. The fix is `adb reverse`:

```bash
adb reverse tcp:3000 tcp:3000
adb reverse tcp:8080 tcp:8080
```

Simple, but you have to re-run it every time the emulator restarts.

The real breakthrough was realizing that Capacitor apps run inside an Android WebView, and WebViews expose a Chrome DevTools Protocol socket. You can find it, forward it to a local port, and suddenly you have full programmatic control:

```bash
# Find the WebView's DevTools socket
WV_SOCKET=$(adb shell "cat /proc/net/unix" | \
  grep webview_devtools_remote | \
  grep -oE 'webview_devtools_remote_[0-9]+' | head -1)

# Forward it to a local port
adb forward tcp:9223 localabstract:$WV_SOCKET

# Full CDP access
curl http://localhost:9223/json
```

With CDP, authentication is one WebSocket message — inject a JWT into localStorage and navigate to the feed. Navigation is another message — set `window.location.href`. No coordinate guessing, no UI interaction, no fighting with keyboards or dialogs. The same protocol that Playwright and Puppeteer use, just connected to an Android WebView instead of a desktop browser.

Combined with `adb shell screencap` for screenshots, I built a Python script that sweeps all 25 screens of the app in about 90 seconds. Landing, login, all four feeds, post detail, profile, shows hub, content creation forms, catalog, battles, bug forum, diary, badges, tour crews — everything. Each screenshot gets analyzed for visual issues: broken layouts, error messages, missing images, blank screens, status bar overlap.

When the sweep finds something wrong, it authenticates as `zabriskie_bot`, uploads the screenshot to S3, and files a properly formatted bug report to the production forum. The title format is `[Android QA] Shows Hub: RSVP button overlaps venue text` — immediately clear that it came from automation and which screen is affected. It knows about expected states too: the crew detail page returning "Forbidden" for non-members isn't a bug, empty avatar circles aren't bugs, and the "Preview" text in profile settings is a known cosmetic issue.

The whole thing runs as a scheduled task every morning at 8:47 AM. The first full run came back clean: 25 screens, 0 critical issues, 2 minor cosmetic notes. If someone's change breaks a screen overnight, there's a bug filed before anyone's had coffee.

Ninety minutes, start to finish.

## iOS: The Hard One

I figured iOS would be straightforward. Same app, same screens, the Simulator is right there on my Mac. What followed was one of the most absurd debugging sessions I've had — not because the problem was technically profound, but because the iOS Simulator is a fortress of tiny, compounding restrictions that each seem reasonable in isolation but together create a nightmare.

### You Can't Type an Email Address

The first idea was clean: add a deep link handler, generate a JWT, open the URL via `simctl openurl`, and skip the login form entirely. Four attempts, four different failure modes — the native bundle was stale, the config pointed at production, the JWT secret was wrong, the Vite dev server was listening on IPv6 while the Simulator tried IPv4. Zero logins.

So I fell back to typing credentials into the login form. AppleScript can send keystrokes to the Simulator. But the login form has `type="email"` on the input, and AppleScript's `keystroke "@"` sends `Shift+2`, which the Simulator interprets as a keyboard shortcut. Every attempt to type `@` either switched the form to Sign Up, navigated to Forgot Password, or opened a context menu.

Pasting didn't work either. `Cmd+V` gets intercepted by the Simulator. Setting the iOS pasteboard via `simctl pbcopy` produced garbled text. The macOS clipboard and the iOS pasteboard are separate systems.

The fix was a code change: update the backend login handler from `WHERE email = $1` to `WHERE email = $1 OR username = $1`, change the form input from `type="email"` to `type="text"`, and create a test user with a known password. Now I could type "qatest" instead of needing an `@` symbol. A backend modification to work around a keyboard limitation.

### You Can't Dismiss Native Dialogs

Upon login, iOS shows a "Would Like to Send You Notifications" dialog rendered by UIKit, not the WebView. Native iOS dialogs cannot be dismissed by any form of macOS-synthesized input.

I tried AppleScript `click at` coordinates across a grid of 100+ positions. `cliclick` at every possible coordinate. Python Quartz CGEvent mouse events. Pressing Return and Enter. Finding the button in the accessibility tree (not exposed). `simctl privacy grant` (not supported for notifications on iOS 26). `simctl ui alert accept` (doesn't exist).

The dialog sat there, immovable, blocking the app.

The fix was writing directly to the Simulator's TCC.db — the privacy permissions database — inserting a pre-approval for `kTCCServiceUserNotification`, then restarting SpringBoard. But the timing is critical: it has to happen before installing the app, or the permission state gets cached. And the app's JavaScript calls `PushNotifications.requestPermissions()` on login, which can retrigger it, so I had to add a guard that skips permission requests on localhost.

The correct sequence: uninstall app, write TCC permission, restart SpringBoard, reinstall app, launch, then login. Only in that exact order does the dialog not appear.

### You Can't Navigate by Coordinates (Until You Can)

The app has a floating nav bar with three bubble buttons in the top-right corner — a Z logo, an avatar, and a `+` — each opening a vertical dropdown. To test all 25 screens, I needed to tap specific dropdown items. I had coordinates from the CSS. The math checked out. But every approach had a different failure mode.

AppleScript `click at` uses macOS window coordinates. You need the window position, the device screen group offset, the Simulator's scaling mode (Point Accurate vs. Pixel Accurate vs. Fit Screen), and whether the toolbar is showing. First sweep: 42% accuracy.

Facebook's `idb` sends taps in device logical points (390x844), so no translation needed. Better for main nav buttons, but dropdown item coordinates were slightly off — taps would close the dropdown before hitting the item, or punch through the z-index to content behind it. Second sweep: 57% accuracy.

The breakthrough was the `ios-simulator-mcp` tool's `ui_describe_point` function. Point it at any coordinate and it returns the accessibility label, role, and frame:

```
ui_describe_point(365, 163)
→ AXLabel: "Currents", type: Link, frame: (342, 159, 40x40)
```

I mapped every dropdown item by probing in 48pt increments. My Y positions were right but my X was wrong — the `+` dropdown items are at x=258, not x=269. An 11-point error that routed every tap to the wrong column. With verified coordinates and 1.5-second waits for dropdown animations, the sweep hit 100% of screens.

The winning combination: `ui_describe_point` for discovery, `idb ui tap` for execution. Map the UI first, tap second. Don't guess coordinates — measure them.

### The Fundamental Gap

The contrast is stark. Android authentication:

```python
ws.send('{"method":"Runtime.evaluate","params":{"expression":"localStorage.setItem(\'token\',\'xxx\')"}}')
```

iOS authentication: uninstall app, write to TCC database, restart SpringBoard, reinstall app, launch, wait 5 seconds, tap Sign In at specific coordinates, wait, tap Email field, type "qatest" via AppleScript, press Tab, type "qatest123", press Return, wait, hope.

Apple's WKWebView doesn't expose Chrome DevTools Protocol. Safari Web Inspector uses a proprietary binary protocol that only Safari speaks. `ios-webkit-debug-proxy` only works with real USB devices. `safaridriver` connects to macOS Safari, not the Simulator's WebView.

Android gives you a WebSocket and says "here's the browser, do whatever you want." iOS gives you a locked door and a note that says "please use Xcode."

## The Mess in the Middle

Between getting Android working and finishing iOS, something happened that illustrates a different kind of failure — not a platform limitation, but an agent discipline problem.

Railway deployments started failing with a Go version mismatch. My local Go had auto-updated to 1.26, which silently bumped `go.mod` to require Go 1.25, while the Dockerfile still used `golang:1.24-alpine`. A two-file fix.

Claude was operating in a git worktree — a clean, isolated copy of the repo designed for exactly this kind of surgical change. Instead of making the fix there, it `cd`'d into the main repository where I had a dozen unrelated in-progress changes. It staged every dirty file, committed them all with the Go version fix, pushed, and opened a PR. The PR contained QA login endpoints, bug forum updates, iOS Simulator workarounds, E2E test config changes, push notification code, and three new skill files. None of which had anything to do with a Go version number.

Then it got auto-merged before I could close it.

The bad merge left duplicate variable declarations throughout the test suite — functions declared twice, variables declared twice. One of the accidentally included changes was a form placeholder rename from `"Email"` to `"Email or Username"`, which broke every auth E2E test that used `page.fill('input[placeholder="Email"]')`. A catalog test that asserted `itemCount > 50` only worked against my local database — CI has a handful of records.

To fix a two-file change, I ended up making four follow-up commits across three PRs. The first two I pushed without running tests locally. They failed. The third I actually ran tests first. It passed. Three rounds of "push and pray" before doing what should have been step one: run the tests, read the output, fix what's broken, verify, then push. The same debugging rule I enforce every session — check the logs first, theories second — and I ignored it for my own changes.

## What This All Adds Up To

Both platforms now have working QA skills. Every morning, the Android emulator and the iOS Simulator boot up, sweep 25 screens each, analyze the screenshots, and file bug reports for anything that looks wrong. Three platforms, all tested, all filing their own bugs.

The lessons keep reinforcing each other:

**CDP over taps.** Don't fight coordinate systems if you can use the browser's own debugging protocol. Android gives you this for free. iOS doesn't, and every workaround adds fragility.

**Measure, don't guess.** The accessibility API that finally made iOS navigation work is the same principle as checking logs before forming theories. Don't assume you know where a button is — ask the system.

**Stay in the worktree.** Isolation only works if you respect the boundaries. The moment you step outside "just for a quick look," you're one careless command away from committing a dozen unrelated files to production.

**Run the tests before you push.** Three rounds of push-and-pray before doing what should have been step one. The gap between knowing a rule and following it is measured in wasted commits.

Apple, if you're reading this: please expose CDP or WebDriver for Simulator WebViews. The developer tools are great when a human is using them. They're nearly useless when an AI is trying to.
