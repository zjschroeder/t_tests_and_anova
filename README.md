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

