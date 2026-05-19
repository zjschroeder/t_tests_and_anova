---
editor_options: 
  markdown: 
    wrap: 72
---

# PSY 302 — t-tests & ANOVA, step by step (Shiny)

A five-tab interactive app that builds each test taught in PSY 302 from
the ground up, using the lab scenarios students already saw in class as
the running example.

## Tabs

| Tab | Test                           |
|-----|--------------------------------|
| 1   | Single-sample t-test           |
| 2   | Paired-samples t-test          |
| 3   | Independent-samples t-test     |
| 4   | Between-subjects one-way ANOVA |
| 5   | Factorial (two-way) ANOVA      |

The four t-test and one-way ANOVA tabs each follow a parallel 9-step
skeleton; factorial ANOVA is a streamlined 6-step orientation (per a
midterm- scope decision — factorial is post-midterm material).

## Three parallel explanation cards per step

Every step has three side-by-side cards with the same content at three
levels of formality:

-   **Academic** (blue) — the formal definition, full notation
-   **In human words** (green) — plain English, no jargon
-   **Case study** (amber) — the matching lab scenario with real numbers

Colors are from the Okabe-Ito palette (Wong 2011, *Nature Methods*) and
are colorblind-safe across protan, deutan, and tritan vision.

## Running locally

``` r
install.packages(c("shiny", "bslib", "ggplot2", "plotly", "scales"))
shiny::runApp(".")
```

Tested on R ≥ 4.1. `thematic` is an optional dependency (loaded with
`requireNamespace()`) — install it if you want plot fonts to match the
bslib theme.

### CRAN dependencies

| Package  | Used for                                                     |
|----------|--------------------------------------------------------------|
| shiny    | the app framework                                            |
| bslib    | theming (`bs_theme()`, Google Fonts)                         |
| ggplot2  | static plots                                                 |
| plotly   | interactive Step-1 population curve                          |
| scales   | percent-axis labels in the ANOVA familywise-error plot       |
| thematic | *optional* — only loaded if installed; harmonises plot fonts |
