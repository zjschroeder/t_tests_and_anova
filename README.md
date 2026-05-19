# PSY 302 — t-tests & ANOVA, step by step (Shiny)

A five-tab interactive app that builds each test taught in PSY 302 from the
ground up, using the lab scenarios students already saw in class as the
running example.

## Tabs

| Tab | Test | Lab scenario |
|---|---|---|
| 1 | Single-sample t-test | Lab 4 — loneliness in college students |
| 2 | Paired-samples t-test | Lab 6 — anxiety / CBT |
| 3 | Independent-samples t-test | Lab 5 — Chips Ahoy vs Chips Deluxe |
| 4 | Between-subjects one-way ANOVA | Lab 7 — CBT / Mindfulness / Waitlist BDI |
| 5 | Factorial (two-way) ANOVA | Lab 8 — caffeine × personality |

The four t-test and one-way ANOVA tabs each follow a parallel 9-step
skeleton; factorial ANOVA is a streamlined 6-step orientation (per a midterm-
scope decision — factorial is post-midterm material).

## The 9-step skeleton (tabs 1–4)

| Step | Topic |
|---|---|
| 1 | The null world for the test statistic |
| 2 | Run one study: what statistic do we compute? |
| 3 | Run it again — sampling variability |
| 4 | Sampling distribution of the relevant statistic |
| 5 | Standard error of that statistic |
| 6 | The test statistic = signal ÷ noise |
| 7 | Setting alpha |
| 8 | Critical values + degrees of freedom |
| 9 | The decision + APA write-up |

The same `(signal) / (noise)` framing carries through every tab. Paired ⇒
"single-sample on the difference scores." Independent ⇒ "sampling
distribution of differences between means." ANOVA ⇒ F as signal/noise
generalized from t.

## Three parallel explanation cards per step

Every step has three side-by-side cards with the same content at three
levels of formality:

- **Academic** (blue) — the formal definition, full notation
- **In human words** (green) — plain English, no jargon
- **Case study** (amber) — the matching lab scenario with real numbers

Colors are from the Okabe-Ito palette (Wong 2011, *Nature Methods*) and are
colorblind-safe across protan, deutan, and tritan vision.

## Running locally

```r
install.packages(c("shiny", "bslib", "ggplot2", "plotly", "scales"))
shiny::runApp(".")
```

Tested on R ≥ 4.1. `thematic` is an optional dependency (loaded with `requireNamespace()`) — install it if you want plot fonts to match the bslib theme.

## File layout

```
t_test_viz/
├── app.R              # entry point — shared helpers, UI shell, server shell
├── R/
│   ├── tab_paired.R      # Tab 2 — paired-samples t-test walkthrough
│   ├── tab_indep.R       # Tab 3 — independent-samples t-test walkthrough
│   ├── tab_anova.R       # Tab 4 — one-way ANOVA walkthrough
│   └── tab_factorial.R   # Tab 5 — factorial ANOVA walkthrough
└── README.md
```

Each `R/tab_*.R` defines (a) a `LABn` constants object with the matching
lab's data and pre-computed statistics, (b) plot helpers specific to that
test, and (c) a single `source_*_server(input, output, session)` entry
point that registers the tab's `output$*_tab_ui` renderer plus any per-step
reactives. The shell `server()` in app.R calls these entry points once
per session.

Shiny auto-sources files in `R/` (since 1.5.0), but `app.R` also calls
`source()` on them explicitly so the app works on older versions and so the
load order is obvious. `app.R` publishes its shared helpers (palette,
plotting primitives, `step_container`, `explanation_triad`, `scenario_card`,
`draw_t_curve`, `draw_F_decision`, etc.) into `globalenv()` so the tab
modules can see them.

## Design notes

The app applies a few documented best practices for educational stats apps:

- **Three nested distributions, labelled on every screen**: population,
  sample, sampling. Conflating sample distribution with sampling
  distribution is the single biggest stumbling block in intro inference
  (delMas, Garfield & Chance; Sotos et al. 2007). Every step that
  introduces a sampling distribution names it explicitly and contrasts it
  with the sample distribution.
- **Signal ÷ noise as a single template** across all five tests. Paired
  collapses to one-sample-on-differences; independent uses a pooled SE; F
  in ANOVA is just MS_between / MS_within. Same recipe, different parts.
- **Active-learning prompts**. Each step includes "Try this" and "What to
  notice" callouts that ask students to predict before sliding, then check
  themselves.
- **APA-style write-up card** at the bottom of every tab. Translates the
  numerical output into the verbal form PSY 302 expects on the midterm.
- **Okabe-Ito palette** (Wong 2011, *Nature Methods*) for colorblind-safe
  categorical colors.
- **MathJax-rendered equations** so formulas look like real equations, not
  Unicode strings.
- **In-figure labels with white backgrounds** — every label that sits over
  plot data uses `annotate("label", ...)` with white fill, so values stay
  legible regardless of underlying line/curve.

## Deploying as a static website (free, no server)

This is the recommended path for sharing on Canvas. The whole R interpreter
plus the app's seven packages get compiled to WebAssembly and shipped as a
static `docs/` directory — meaning **no Shiny server is involved**, the
entire app runs inside the student's browser. GitHub Pages hosts the result
for free with no traffic cap, no sleep timeout, and no login.

### What's already in this repo for you

- `shinylive_export.R` — one-command local build script
- `.github/workflows/deploy.yml` — GitHub Action that rebuilds + publishes
  the static bundle on every push to `main`
- `.gitignore` — keeps the local `docs/` build out of the repo (GitHub
  Pages serves the artifact built by the Action, not the committed copy)

### One-time setup

1. Create a GitHub repo for the app (public, so Pages is free) and push
   the contents of `t_test_viz/` to its `main` branch:
   ```sh
   cd "/Users/zachschroeder/Desktop/PSY 302/t_test_viz"
   git init -b main
   git add . && git commit -m "initial commit"
   gh repo create psy302-stats-explainer --public --source=. --push
   ```
2. In the new repo, go to **Settings → Pages**. Under "Build and
   deployment → Source," choose **GitHub Actions** (not "Deploy from a
   branch"). That's it — the workflow we already shipped does the rest.

### Every deploy

Just push:
```sh
git add .
git commit -m "update content"
git push
```

The GitHub Action runs in ~3–5 minutes, downloads the seven WebAssembly
packages from `repo.r-wasm.org`, exports the static bundle, and publishes
to `https://<your-github-username>.github.io/psy302-stats-explainer/`.

The first deploy is longer (5–8 min) because it warms a cache; subsequent
deploys are faster.

### Previewing locally (optional)

If you want to test the static build on your laptop before pushing:

```sh
Rscript shinylive_export.R
Rscript -e 'httpuv::runStaticServer("docs", port = 8000)'
# Open http://localhost:8000 in a browser
```

### Sharing with students on Canvas

After deploy:

- **Easiest**: paste the GitHub Pages URL into a Canvas page or
  announcement. It renders as a clickable link.
- **Embedded**: in a Canvas page → click the HTML editor → paste:
  ```html
  <iframe src="https://YOUR-USERNAME.github.io/psy302-stats-explainer/"
          width="100%" height="900" frameborder="0"></iframe>
  ```
  The app then loads inline inside the Canvas page.

No student login or account is needed.

### Caveats

- **First-load size** is roughly 25–35 MB (the R interpreter + seven
  precompiled WebAssembly packages). Students see a loading bar for about
  15–30 seconds the first time they open the link. The browser caches
  everything, so the second visit is near-instant.
- **plotly, ggplot2, bslib, scales** are all available as WebAssembly
  binaries via the [r-wasm package repo](https://repo.r-wasm.org/) — I
  verified this against the current PACKAGES manifest before recommending
  this path. If you add a new dependency later, double-check it has a
  WebAssembly build before pushing.
- **`thematic` is optional** (the app loads it via `requireNamespace`), so
  if its WebAssembly build is ever missing the app degrades gracefully.

### Why this works on the free tier forever

- GitHub Pages free tier has **no monthly hour cap** (unlike shinyapps.io)
  and serves static files from a CDN. The "compute" happens in each
  student's browser, so there's nothing to bill.
- The repo can stay public with no risk — the app source is already
  educational material.

---

## Deploying to shinyapps.io

The app is already configured for shinyapps.io deployment — there is a
`deploy.R` script and an `.rscignore` that keeps editor / OS clutter and the
local Claude Preview launch config out of the bundle.

### One-time setup

1. Make a free account at <https://www.shinyapps.io>.
2. From the dashboard go to **Account → Tokens** and copy your token + secret.
3. Run once interactively in R:

   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name   = "<your-account>",
                             token  = "<token>",
                             secret = "<secret>")
   ```

   Credentials are persisted to `~/.config/R/rsconnect/`, so you only have
   to do this once per machine.

### Every deploy

From the project root (`t_test_viz/`):

```sh
Rscript deploy.R
```

The script (a) installs any missing CRAN deps, (b) sources `app.R` in
isolation to surface any errors before uploading, and (c) calls
`rsconnect::deployApp()` with `appName = "psy302-stats-explainer"`.
On success the live app opens at
`https://<your-account>.shinyapps.io/psy302-stats-explainer/`.

To pick a different URL slug, edit the `appName` argument inside
`deploy.R`. Keep it stable across deploys so links stay valid.

### What gets bundled

`rsconnect` uploads everything in the project directory except patterns
listed in `.rscignore`. Currently the bundle is exactly:

```
app.R
R/tab_paired.R
R/tab_indep.R
R/tab_anova.R
R/tab_factorial.R
README.md
```

`.claude/`, `deploy.R`, dot-files (`.DS_Store`, `.Rhistory`, etc.) and
IDE state directories are all excluded.

### CRAN dependencies

Detected automatically by `rsconnect::appDependencies()`:

| Package | Used for |
|---|---|
| shiny | the app framework |
| bslib | theming (`bs_theme()`, Google Fonts) |
| ggplot2 | static plots |
| plotly | interactive Step-1 population curve |
| scales | percent-axis labels in the ANOVA familywise-error plot |
| thematic | *optional* — only loaded if installed; harmonises plot fonts |

71 packages in the transitive closure. The free shinyapps.io tier (25
active hours/month, 5 apps) is more than enough for one class section.

### MathJax CDN

The app loads MathJax v2.7.9 from cdnjs (`cdnjs.cloudflare.com`). Shiny's
default `withMathJax()` points at `mathjax.rstudio.com`, which has been
offline since the RStudio→Posit rebrand; `app.R` overrides it. No further
action needed — the override travels with the deployed app.

## Customising for future semesters

- **Lab scenarios**: each tab's `LABn` constants object lives at the top
  of its `R/tab_*.R` file. Update the data vectors and pre-computed
  statistics there to swap in a different running example.
- **Slider defaults** (Tab 1): edit `sliderInput("pop_mu", ...)`,
  `sliderInput("pop_sigma", ...)`, and `sliderInput("pop_n", ...)`
  defaults near the Step 1 / Step 2 / Step 5 / Step 9 input blocks in
  `app.R`.
- **Theme**: the `bs_theme(bootswatch = "minty")` call picks the look.
  Try `"cosmo"`, `"sandstone"`, `"flatly"`, or `"yeti"` for variants. The
  Okabe-Ito plot palette is independent of the bootswatch theme.
- **Alpha choice on Tab 1**: chooseable per-session via radio buttons.
  Tabs 2-5 currently fix α = .05 (the PSY 302 default) — to vary it, add a
  `radioButtons("*_alpha", ...)` to the corresponding `R/tab_*.R` and
  pass it through to that tab's Step 7/8/9 plots.

## Pedagogical notes for the instructor

- **Sample vs sampling distribution**: every tab includes at least one
  callout that contrasts the two distributions side-by-side using that
  tab's specific statistic. This is the user-flagged hardest concept for
  the cohort, so the language is deliberately repetitive across tabs.
- **Paired collapses to single-sample**: Tab 2 Step 6 includes an explicit
  side-by-side that shows the paired t formula reducing to the Tab 1
  formula when applied to difference scores.
- **Why ANOVA exists**: Tab 4 Step 2 visualises family-wise error
  inflation as the motivating story, before introducing F. This frames the
  whole apparatus as a solution to a concrete problem rather than a magic
  trick.
- **Interaction first**: Tab 5 Step 6 includes the decision rule
  ("interaction first, then main effects"), with an explicit warning that
  significant main effects can be misleading when an interaction is present.
- **APA write-up template** in every tab matches the Lab key's exact
  phrasing so students can practice the format ahead of the midterm.
