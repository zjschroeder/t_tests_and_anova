# PSY 302, t-tests & ANOVA, step by step ------------------------------------
# A multi-tab Shiny app for the PSY 302 audience. Each tab is a parallel 9-step
# walkthrough that builds one test from the ground up, using the lab scenario
# students already saw in class as the case-study card.
#
#   1. Single-sample t-test       (peas in a baby's ear)
#   2. Paired-samples t-test      (staring down seagulls)
#   3. Independent-samples t-test (Daves know more Daves)
#   4. Between-subjects one-way ANOVA (climate concern across religious groups)
#   5. Factorial ANOVA            (parental medication dosing accuracy)
#
# Each tab ends with a quest section: a fresh dataset and nine reveal-toggle
# questions that mirror the same nine analytic steps. Case studies are drawn
# from Hartnett's "Not Awful and Boring" teaching collection (see
# notawfulandboring.blogspot.com).
#
# Each step has three side-by-side explanation cards (Academic / Plain English
# / Case study), an equation rendered via MathJax, an interactive piece, and a
# "Continue ↓" button to unlock the next step.
#
# Step blocks live in static UI gated by conditionalPanel (not renderUI),
# so widgets persist across step transitions and reset observers don't fire
# spuriously. Equations use real LaTeX via withMathJax. Color palette is
# Okabe-Ito (Wong 2011), colorblind-safe.
#
# Run locally: shiny::runApp("app.R")
# ----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(scales)   # used by tab_anova.R for percent-axis labels

# Optional dependency: thematic harmonizes ggplot fonts/colors with the bslib
# theme so plots look like the rest of the app. Falls back gracefully if it
# is not installed (e.g., on a slim shinyapps.io bundle).
if (requireNamespace("thematic", quietly = TRUE)) {
  # Inherit fonts only, keep the carefully tuned Okabe-Ito palette as the
  # source of truth for plot colours.
  thematic::thematic_shiny(bg = NA, fg = NA, accent = NA, font = "auto")
}

# Okabe-Ito palette + semantic aliases ----------------------------------------
OI <- list(
  black      = "#000000",
  orange     = "#E69F00",
  sky        = "#56B4E9",
  green      = "#009E73",
  yellow     = "#F0E442",
  blue       = "#0072B2",
  vermillion = "#D55E00",
  purple     = "#CC79A7"
)

PAL <- list(
  pop_dark   = OI$blue,
  pop_med    = OI$sky,
  pop_light  = "#A6DAF0",
  pop_xlight = "#D6EBF6",
  band1      = OI$blue,
  band2      = OI$sky,
  band3      = "#A6DAF0",
  band_far   = "#D6EBF6",
  samp_pt    = OI$green,
  samp_line  = "#006B4F",
  theory     = OI$vermillion,
  reject     = OI$orange,
  reject_dk  = "#7A5300",
  pval       = OI$purple,
  obs        = OI$black,
  ok_bg      = "#E1F5EE", ok_fg  = "#0F6E56",
  warn_bg    = "#FFF1D6", warn_fg = "#7A4F00",
  info_bg    = "#E1F1FB", info_fg = "#003D6B",
  step_bg    = "#F0F8FB", step_brd = OI$blue,
  card_def_bg = "#E1F1FB", card_def_fg = "#003D6B", card_def_acc = OI$blue,
  card_ex_bg  = "#E1F5EE", card_ex_fg  = "#0F6E56", card_ex_acc  = OI$green,
  card_tldr_bg = "#FFF1D6", card_tldr_fg = "#7A4F00", card_tldr_acc = OI$orange
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ggplot theme ----------------------------------------------------------------
base_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey90"),
      axis.title         = element_text(size = 12, colour = "#333"),
      axis.text          = element_text(colour = "#444"),
      plot.title         = element_text(size = 14, face = "bold", colour = "#222"),
      plot.subtitle      = element_text(size = 12, colour = "#555"),
      plot.margin        = margin(8, 14, 8, 14)
    )
}

# Adaptive x-axis range / breaks for t-distribution plots --------------------
# Make sure observed t-statistics, critical values, and the body of the
# sampling distribution all remain visible, even with extreme |t| or low df.
t_xrange <- function(..., min_extent = 6, pad = 1.2) {
  vals <- c(...)
  vals <- vals[is.finite(vals)]
  L    <- if (length(vals)) max(min_extent, max(abs(vals)) + pad) else min_extent
  L    <- min(L, 50)                  # cap so the curve doesn't disappear
  c(-L, L)
}
t_xbreaks <- function(xr) {
  L    <- xr[2]
  step <- if (L <= 7) 2 else if (L <= 12) 3 else if (L <= 20) 5 else 10
  seq(-floor(L / step) * step, floor(L / step) * step, by = step)
}

# Plot label helper: white background for in-figure labels (visibility) -------
geom_label_clean <- function(...) {
  geom_label(..., fill = "white", label.size = NA, label.r = unit(0.15, "lines"),
             alpha = 0.9)
}
annotate_clean <- function(label, x, y, colour, size = 4.2, fontface = "bold",
                           hjust = 0.5, vjust = 0.5) {
  list(
    annotate("label", x = x, y = y, label = label,
             colour = colour, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.92,
             size = size, fontface = fontface, hjust = hjust, vjust = vjust)
  )
}

# UI helpers ------------------------------------------------------------------
step_container <- function(step_num, title, ..., id_prefix = "step") {
  div(
    id = sprintf("%s-%d", id_prefix, step_num),  # anchor target for sidebar TOC
    class = "mt-4 mb-3 p-4 step-block",
    style = sprintf(
      "background:%s; border-left:6px solid %s; border-radius:10px; scroll-margin-top:80px;",
      PAL$step_bg, PAL$step_brd),
    div(style = sprintf(
          "color:%s; font-weight:600; letter-spacing:0.05em; font-size:12px; text-transform:uppercase;",
          PAL$step_brd),
        sprintf("Step %d", step_num)),
    h3(title, style = "margin:4px 0 14px 0;"),
    ...
  )
}

# Three-form colour-coded explanation block (Formal / In context / tl;dr) ----
explanation_triad <- function(formal, example, tldr) {
  card <- function(label, body, bg, fg, acc) {
    div(class = "col-md-4",
        div(class = "p-3 h-100",
            style = sprintf("background:%s; color:%s; border-radius:8px; border-left:5px solid %s;",
                            bg, fg, acc),
            div(style = sprintf(
                  "font-size:11px; font-weight:700; letter-spacing:0.06em; color:%s; opacity:0.85; text-transform:uppercase;",
                  fg),
                label),
            div(style = "margin-top:8px; line-height:1.55;", body)
        )
    )
  }
  div(class = "row g-3 my-3",
      card("Academic",        formal,  PAL$card_def_bg,  PAL$card_def_fg,  PAL$card_def_acc),
      card("In human words",  example, PAL$card_ex_bg,   PAL$card_ex_fg,   PAL$card_ex_acc),
      card("Case study",      tldr,    PAL$card_tldr_bg, PAL$card_tldr_fg, PAL$card_tldr_acc)
  )
}

# MathJax helpers -------------------------------------------------------------
# NB: Shiny's bundled `withMathJax()` points at mathjax.rstudio.com, which has
# been offline since the RStudio→Posit rebrand, the SSL handshake fails and
# MathJax never loads, so `\(...\)` strings render as raw text. We override
# `withMathJax` here with a version that loads MathJax v2.7.9 from cdnjs
# instead. The script is wrapped in `singleton()` so it's only injected once
# per page even if `withMathJax()` is called many times.
withMathJax <- function(...) {
  path <- "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  tagList(
    tags$head(singleton(tags$script(src = path, type = "text/javascript"))),
    ...,
    tags$script(HTML(
      "if (window.MathJax && window.MathJax.Hub) { ",
      "  MathJax.Hub.Queue(['Typeset', MathJax.Hub]); ",
      "}"))
  )
}
math_block <- function(latex_str) {
  withMathJax(
    div(class = "p-3 my-3",
        style = "background:#fff; border:1px dashed #b8b3d8; border-radius:8px; text-align:center; min-height:60px; display:flex; align-items:center; justify-content:center; font-size:18px;",
        HTML(paste0("$$", latex_str, "$$"))
    )
  )
}
math_inline <- function(latex_str) {
  withMathJax(HTML(paste0("\\(", latex_str, "\\)")))
}

callout_warm <- function(...) {
  div(class = "p-3 my-3",
      style = sprintf("background:%s; color:%s; border-radius:8px; line-height:1.55;",
                      PAL$warn_bg, PAL$warn_fg),
      ...)
}

continue_button <- function(id, label = "Continue ↓") {
  div(class = "mt-3", style = "text-align:right;",
      actionButton(id, label, class = "btn-primary"))
}

help_details <- function(summary_text, body_text) {
  tags$details(
    class = "mt-2",
    tags$summary(
      style = sprintf("cursor:pointer; font-weight:600; color:%s;", PAL$pop_dark),
      summary_text
    ),
    div(class = "p-2 mt-1",
        style = sprintf("background:#fff; border-left:3px solid %s; border-radius:4px; line-height:1.55;",
                        PAL$pop_dark),
        body_text)
  )
}

plot_legend <- function(items) {
  div(class = "d-flex justify-content-center mt-2 mb-3 flex-wrap",
      style = "gap:18px; font-size:12.5px; color:#444;",
      lapply(items, function(it) {
        marker <- if (identical(it$type, "line")) {
          tags$span(style = sprintf(
            "display:inline-block; width:26px; height:0; border-top:2.5px %s %s; vertical-align:middle; margin-right:7px;",
            it$dash %||% "solid", it$color))
        } else {
          tags$span(style = sprintf(
            "display:inline-block; width:14px; height:14px; background:%s; vertical-align:middle; margin-right:7px; border-radius:2px; opacity:0.85;",
            it$color))
        }
        tags$span(marker, it$label)
      })
  )
}

# Stat card pieces ------------------------------------------------------------
stat_card <- function(label, value, key = FALSE) {
  div(
    div(class = if (key) "stat-key" else "stat-label", label),
    div(class = if (key) "stat-kv"  else "stat-value", value)
  )
}

# Quest section helpers -------------------------------------------------------
# Each test tab ends with a "Now you try" section: a fresh dataset and a list
# of questions, one per analytic step, each with a `<details>`-toggleable
# worked solution. The structure mirrors the 9-step walkthrough above, so
# students rehearse the same procedural moves on new numbers.

quest_section <- function(title, scenario_html, data_block, questions,
                          id_prefix = "quest") {
  div(class = "mt-5 mb-3",
      style = sprintf("background:#f6efe2; border-radius:12px; border-top:6px solid %s; padding:24px 24px 12px 24px;",
                      OI$orange),
      div(style = sprintf("color:%s; font-weight:700; letter-spacing:0.05em; font-size:12px; text-transform:uppercase;",
                          OI$orange),
          "Now you try"),
      h3(title, style = "margin:6px 0 14px 0;"),
      scenario_html,
      div(class = "p-3 mt-2 mb-3",
          style = "background:#fff; border:1px solid #e9d8a8; border-radius:8px;",
          h5("The data for this scenario", style = "margin-top:0;"),
          data_block),
      p(style = "color:#7A4F00; font-size:13px; font-style:italic;",
        "Work through each question on paper or in your head, then click the ",
        tags$b("Show my work"),
        " toggle to compare against a full worked solution. The questions ",
        "mirror the nine steps from the walkthrough above, so you are ",
        "rehearsing the same procedure with new numbers."),
      lapply(seq_along(questions), function(i) {
        quest_question(i, questions[[i]]$prompt, questions[[i]]$solution,
                       id_prefix = id_prefix)
      })
  )
}

quest_question <- function(step_num, prompt, solution, id_prefix = "quest") {
  div(class = "mb-2",
      id = sprintf("%s-q-%d", id_prefix, step_num),
      style = "background:#fff; border:1px solid #e9d8a8; border-radius:8px; padding:14px 18px;",
      div(style = sprintf(
            "color:%s; font-weight:700; letter-spacing:0.05em; font-size:11px; text-transform:uppercase; margin-bottom:4px;",
            OI$orange),
          sprintf("Question %d", step_num)),
      div(style = "line-height:1.55; margin-bottom:8px;", prompt),
      tags$details(
        class = "quest-details",
        tags$summary(
          style = sprintf("cursor:pointer; font-weight:600; color:%s; font-size:13.5px;",
                          OI$orange),
          "Show my work"),
        div(class = "mt-2 p-3",
            style = "background:#fdf8ec; border-left:3px solid #d6c08a; border-radius:4px; line-height:1.6;",
            solution)
      )
  )
}

# Scenario-card UI helper (used by tabs 2-5) ----------------------------------
# A coloured header card that calls out the lab number plus running scenario.
scenario_card <- function(title, ..., lab_label = NULL) {
  div(class = "p-3 mb-3",
      style = sprintf("background:%s; color:%s; border-radius:10px; border-top:6px solid %s;",
                      PAL$info_bg, PAL$info_fg, PAL$pop_dark),
      h2(title, style = "margin-top:0;"),
      if (!is.null(lab_label))
        tags$div(style = sprintf(
                   "color:%s; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; font-size:12px; margin-bottom:6px;",
                   PAL$pop_dark),
                 lab_label),
      ...
  )
}

# t-distribution decision plot (used by Tab 1 Step 9 and all new tabs) -------
# Shaded rejection region under two-tailed t-distribution. Marks observed t
# and the symmetric critical values.
draw_t_curve <- function(tobs, df_v, alpha = 0.05) {
  cv_hi <- qt(1 - alpha / 2, df_v)
  xr <- t_xrange(tobs, cv_hi, -cv_hi)
  x  <- seq(xr[1], xr[2], length.out = 400)
  curve_df <- data.frame(x = x, y = dt(x, df_v))
  region_df <- function(a, b) {
    if (a >= b) return(data.frame(x = numeric(0), y = numeric(0)))
    xs <- seq(a, b, length.out = 200); data.frame(x = xs, y = dt(xs, df_v))
  }
  a_lo <- region_df(xr[1], -cv_hi); a_hi <- region_df(cv_hi, xr[2])
  p_lo <- region_df(xr[1], -abs(tobs)); p_hi <- region_df(abs(tobs), xr[2])

  label_offset <- diff(xr) * 0.02
  max_y <- max(curve_df$y)
  ggplot() +
    geom_area(data = a_lo, aes(x, y), fill = PAL$reject, alpha = 0.55) +
    geom_area(data = a_hi, aes(x, y), fill = PAL$reject, alpha = 0.55) +
    geom_area(data = p_lo, aes(x, y), fill = PAL$pval,   alpha = 0.7) +
    geom_area(data = p_hi, aes(x, y), fill = PAL$pval,   alpha = 0.7) +
    geom_line(data = curve_df, aes(x, y), colour = "black", linewidth = 0.6) +
    geom_vline(xintercept = tobs, colour = PAL$obs, linewidth = 1) +
    geom_vline(xintercept = c(-cv_hi, cv_hi), colour = PAL$reject_dk,
               linetype = "dashed", linewidth = 0.5) +
    annotate("label", x = tobs, y = max_y * 1.08,
             label = sprintf("Your t = %.2f\n(signal ÷ noise)", tobs),
             colour = PAL$obs, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 3.6, fontface = "bold", lineheight = 0.9) +
    annotate("label", x = cv_hi + label_offset, y = max_y * 0.85,
             label = sprintf("CV = %.2f", cv_hi),
             colour = PAL$reject_dk, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 3.4, fontface = "bold", hjust = 0) +
    annotate("label", x = -cv_hi - label_offset, y = max_y * 0.85,
             label = sprintf("CV = %.2f", -cv_hi),
             colour = PAL$reject_dk, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 3.4, fontface = "bold", hjust = 1) +
    scale_x_continuous(limits = xr, breaks = t_xbreaks(xr)) +
    scale_y_continuous(limits = c(0, max_y * 1.25)) +
    labs(x = "t-statistic = signal ÷ noise (on the null sampling distribution)",
         y = "Density") +
    base_theme()
}

# F-distribution decision plot (used by tab 4: one-way ANOVA) ----------------
# One-tailed rejection region under F(df1, df2). Marks observed F + F_crit.
draw_F_decision <- function(Fobs, df1, df2, alpha = 0.05) {
  Fcv  <- qf(1 - alpha, df1, df2)
  xmax <- max(Fobs, Fcv) * 1.4 + 1
  x    <- seq(0, xmax, length.out = 400)
  cd   <- data.frame(x = x, y = df(x, df1, df2))
  reg  <- subset(cd, x >= Fcv)
  max_y <- max(cd$y[is.finite(cd$y)])
  label_offset <- xmax * 0.02
  ggplot() +
    geom_area(data = reg, aes(x, y), fill = PAL$reject, alpha = 0.6) +
    geom_line(data = cd, aes(x, y), colour = "black", linewidth = 0.6) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_vline(xintercept = Fobs, colour = PAL$obs, linewidth = 1.1) +
    geom_vline(xintercept = Fcv,  colour = PAL$reject_dk,
               linetype = "dashed", linewidth = 0.5) +
    annotate("label", x = Fobs, y = max_y * 1.08,
             label = sprintf("Your F = %.2f", Fobs),
             colour = PAL$obs, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4.2, fontface = "bold") +
    annotate("label", x = Fcv + label_offset, y = max_y * 0.85,
             label = sprintf("F_crit = %.2f", Fcv),
             colour = PAL$reject_dk, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 3.6, fontface = "bold", hjust = 0) +
    scale_x_continuous(limits = c(0, xmax)) +
    scale_y_continuous(limits = c(0, max_y * 1.3)) +
    labs(x = sprintf("F-statistic (signal ÷ noise) on null sampling distribution, df = (%d, %d)", df1, df2),
         y = "Density") +
    base_theme()
}

# Publish shared helpers into globalenv -------------------------------------
# Shiny sources app.R into its own frame, but auto-sources R/*.R into globalenv.
# The per-tab modules in R/ therefore cannot see the helpers above unless we
# explicitly copy them across. (Functions retain their original closures, so
# `PAL`, `base_theme`, etc. that they internally reference still resolve.)
local({
  shared_helpers <- c(
    "PAL", "OI", "%||%",
    "base_theme", "t_xrange", "t_xbreaks",
    "geom_label_clean", "annotate_clean",
    "step_container", "explanation_triad",
    "math_block", "math_inline",
    "callout_warm", "help_details", "plot_legend",
    "stat_card", "scenario_card",
    "quest_section", "quest_question",
    "draw_t_curve", "draw_F_decision",
    "withMathJax"   # override Shiny's broken default (dead CDN)
  )
  for (.h in shared_helpers) {
    if (exists(.h, inherits = TRUE))
      assign(.h, get(.h), envir = globalenv())
  }
})

# Source the per-tab modules. Shiny 1.5.0+ auto-loads files in R/, but we
# source them explicitly so the app works under older versions as well.
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE)
}
rm(.f)

# UI ===========================================================================
ui <- page_navbar(
  title = "PSY 302: t-tests & ANOVA, step by step",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font    = font_google("Inter"),
    heading_font = font_google("Source Serif Pro"),
    code_font    = font_google("JetBrains Mono")
  ),
  fillable = FALSE,
  header = tags$head(
    withMathJax(),
    tags$style(HTML("
      /* Smooth scroll for sidebar anchor jumps */
      html { scroll-behavior: smooth; }

      h2, h3, h4, h5 { font-weight:600; color:#1a3a4a; }
      .stat-label { font-size:12px; color:#555; font-weight:600; }
      .stat-value { font-size:22px; font-weight:500; color:#222; }
      .stat-key   { font-size:13px; color:#0072B2; font-weight:700; }
      .stat-kv    { font-size:24px; font-weight:600; color:#0072B2; }
      .form-group .irs { width: 100% !important; }
      .step-block .irs-bar { background: linear-gradient(to bottom, #0072B2, #56B4E9) !important; }
      .pad-top { padding-top: 24px; }

      /* ---- Step unlock animation ---- */
      .step-block {
        animation: stepFadeIn 0.45s ease-out;
      }
      @keyframes stepFadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to   { opacity: 1; transform: translateY(0); }
      }

      /* ---- Sidebar TOC (Radiant-style) ---- */
      .sidebar-col {
        position: sticky;
        top: 80px;
        align-self: flex-start;
        max-height: calc(100vh - 100px);
        overflow-y: auto;
      }
      @media (max-width: 991.98px) {
        /* Stack the sidebar above the main column instead of sticking it. */
        .sidebar-col { position: static; max-height: none; overflow-y: visible; }
      }
      .sidebar-toc {
        background: #fff;
        border: 1px solid #e0e6ea;
        border-radius: 10px;
        padding: 12px 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .sidebar-toc-title {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: #6b7c85;
        padding: 4px 10px 8px 10px;
      }
      .toc-progress-wrap {
        height: 6px;
        background: #ecf2f4;
        border-radius: 3px;
        margin: 0 10px 12px 10px;
        overflow: hidden;
      }
      .toc-progress-bar {
        height: 100%;
        background: linear-gradient(90deg, #78c2ad, #56B4E9);
        transition: width 0.35s ease-out;
      }
      .toc-progress-label {
        font-size: 11px;
        color: #6b7c85;
        padding: 0 10px 6px 10px;
      }
      .toc-item {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 8px 10px;
        border-radius: 6px;
        margin: 2px 0;
        text-decoration: none;
        color: #1a3a4a;
        font-size: 13.5px;
        line-height: 1.3;
        transition: background 0.15s ease, transform 0.15s ease;
      }
      .toc-item:hover      { background: #f3f8fa; transform: translateX(2px); text-decoration: none; }
      .toc-item.completed  { color: #0f6e56; }
      .toc-item.completed:hover { background: #e8f5ef; }
      .toc-item.current    { background: #e1f1fb; font-weight: 700; color: #003d6b;
                             border-left: 3px solid #0072B2; padding-left: 7px; }
      .toc-item.locked     { color: #b0bcc2; pointer-events: none; cursor: default; }
      .toc-num {
        display: inline-flex; align-items: center; justify-content: center;
        width: 22px; height: 22px;
        border-radius: 50%;
        background: #ecf2f4; color: #6b7c85;
        font-size: 11px; font-weight: 700;
        flex-shrink: 0;
      }
      .toc-item.completed .toc-num { background: #78c2ad; color: #fff; }
      .toc-item.current   .toc-num { background: #0072B2; color: #fff; }
      .toc-item.locked    .toc-num { background: #f5f7f8; color: #c5cfd3; }
      .toc-check {
        margin-left: auto; color: #78c2ad; font-weight: 700; font-size: 13px;
      }

      /* ---- Decision-tree widget on Tab 2 ---- */
      .dt-recommend {
        background: linear-gradient(135deg, #e1f5ee 0%, #d3eee2 100%);
        border-left: 5px solid #78c2ad;
        border-radius: 8px;
        padding: 14px 18px;
        font-size: 15px;
        color: #0f6e56;
      }
      .dt-recommend b { color: #074a3a; }
    "))
  ),

  # ===========================================================================
  # TAB 1: Build a one-sample t-test
  # ===========================================================================
  nav_panel(
    title = "Single-sample t",
    div(class = "container-fluid",
        style = "max-width: 1380px; padding-top: 16px;",
        div(class = "row g-4",

            # ===== Sidebar TOC (sticky) =================================
            div(class = "col-lg-3 sidebar-col",
                uiOutput("step_toc")
            ),

            # ===== Main column ==========================================
            div(class = "col-lg-9",

        # ---- Header card ----
        div(class = "p-4 mb-3",
            style = sprintf("background:%s; color:%s; border-radius:10px; border-top:6px solid %s;",
                            PAL$info_bg, PAL$info_fg, PAL$pop_dark),
            h2("Build a single-sample t-test, from the ground up",
               style = "margin-top:0;"),
            p(style = "margin-bottom:6px;",
              "This walkthrough builds an entire ",
              tags$b("single-sample t-test"),
              " from the ground up. The single-sample t-test is what we ",
              "reach for when we have one group and a known benchmark value, ",
              "and we want to ask whether the group's average is ",
              "meaningfully different from that benchmark."),
            p(style = "margin-bottom:6px;",
              tags$b("Running example. "),
              "An ", tags$em("Onion"),
              " satire piece once reported (with a straight face) that ",
              "toddler scientists had finally determined how many peas a ",
              "baby can fit in their ear, settling the matter at four. ",
              "Suppose a follow-up study of ", math_inline("n = 11"),
              " babies finds that the average pea-fitting capacity is six, ",
              "not four. Is this enough to overturn the long-standing ",
              "four-pea claim? The single-sample t-test is the procedure ",
              "for answering exactly that question, and we will work the ",
              "whole thing out below."),
            p(style = "margin-bottom:0;",
              "Each step in the walkthrough adds one new symbol or one new ",
              "conceptual move, then gives you an opportunity to play with ",
              "it. By the end you will have constructed the test statistic ",
              math_inline("t = \\dfrac{M - \\mu_0}{s/\\sqrt{n}}"),
              ", chosen an alpha level, looked up a critical value, and ",
              "made a reject or fail-to-reject decision. ",
              tags$b("Click the Continue ↓ button at the end of each step to advance."))
        ),

        # =======================================================================
        # STEP 1, The null world (μ, σ)
        # =======================================================================
        step_container(
          1, "The null world: \\(\\mu\\) and \\(\\sigma\\)",
          explanation_triad(
            formal = tagList(
              tags$b("Academic. "),
              "A ", tags$b("population"),
              " is the full set of units (people, leaves, transistors) we ",
              "would ideally measure. Its ", tags$b("mean"),
              " is denoted ", math_inline("\\mu"),
              ", and its ", tags$b("standard deviation"),
              " is ", math_inline("\\sigma"),
              ". Together these are the ", tags$em("population parameters"),
              ". The population parameters are usually unknown in real ",
              "research, and we approximate them from sample statistics."
            ),
            example = tagList(
              tags$b("In human words. "),
              math_inline("\\mu"),
              " is the answer we would get if we could somehow measure ",
              tags$em("every"),
              " person in the population and average their scores. ",
              math_inline("\\sigma"),
              " is how spread out the scores typically are around that ",
              "average. Neither of these quantities is usually something a ",
              "researcher gets to know directly. The population describes ",
              "the whole world, and a sample is the small slice of it we ",
              "actually have access to."
            ),
            tldr = tagList(
              tags$b("Case study (peas). "),
              "The population of interest is ",
              tags$em("all babies' pea-fitting ear-canal capacity"),
              ". The long-standing benchmark for this population is ",
              math_inline("\\mu_0 = 4"),
              " peas, which is the value the test will evaluate against. ",
              "The true value of ", math_inline("\\sigma"),
              " is unknown to us. We will estimate it from the eleven-baby ",
              "sample in Step 5 (the data give ",
              math_inline("s \\approx 0.78"), ")."
            )
          ),
          math_block("\\text{Population parameters: } \\mu \\text{ (mean)},\\ \\sigma \\text{ (standard deviation)}"),

          # ---- Case-study numbers (displayed; the actual slider widgets
          # are hidden below so the population plot still has values to use).
          div(class = "p-3 my-3",
              style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
              h5("The case-study numbers for this step", style = "margin-top:0;"),
              p(style = "color:#666; font-size:13px;",
                "Step 1 introduces the two population parameters that the ",
                "test reasons about. The sliders below the plot let you see ",
                "what happens when you change them, but the numbers shown ",
                "here are the values from our peas case study. Step 2 will ",
                "draw a sample under these parameters."),
              fluidRow(
                column(4, stat_card("μ (population mean)",
                                    "unknown")),
                column(4, stat_card("σ (population SD)",
                                    "unknown; estimated from sample as 0.78")),
                column(4, stat_card("μ₀ (null-hypothesis benchmark)",
                                    "4 peas", key = TRUE)))),

          # ---- Hidden sliders (preserve the input bindings used downstream
          # but keep the widget out of the visible UI per the redesign).
          tags$div(style = "display:none;",
            sliderInput("pop_mu", "Population mean (μ)",
                        min = 1, max = 100, value = 6, step = 0.5),
            sliderInput("pop_sigma", "Population SD (σ)",
                        min = 1, max = 25, value = 1, step = 0.5)
          ),
          plotlyOutput("popPlot", height = "360px"),
          callout_warm(
            tags$b("Reading the plot:"),
            " each colored band hovers with two facts, what fraction of the ",
            "population sits inside that band, and the cumulative ",
            "\"% below this point\" running up the curve. (Cumulative percentages ",
            "are useful for percentile reasoning, e.g., \"a value at +1σ is at ",
            "the 84th percentile.\")"
          ),

          # Assumptions checklist (adapted from GraphPad's Ultimate Guide to t-tests)
          div(class = "p-3 my-3",
              style = sprintf("background:%s; color:%s; border-radius:8px; border-left:5px solid %s;",
                              PAL$info_bg, PAL$info_fg, PAL$pop_dark),
              h5("Before a t-test is appropriate, five things must be true",
                 style = "margin-top:0;"),
              tags$ol(style = "margin-bottom:0; padding-left:20px;",
                tags$li(tags$b("One variable of interest. "),
                        "You are studying one numeric outcome (e.g., heart rate). Studies of the relationship between two variables call for a different procedure."),
                tags$li(tags$b("Numeric data. "),
                        "Measurements you can average. Eye color or political party would need a different test."),
                tags$li(tags$b("Two groups or fewer. "),
                        "With three or more groups, use ANOVA instead."),
                tags$li(tags$b("Random sample. "),
                        "The subjects you measured were drawn randomly from the population you want to talk about."),
                tags$li(tags$b("Approximately normal, or large enough n. "),
                        "For small samples, the data should be roughly bell-shaped. The Central Limit Theorem rescues large samples even when the population isn't normal.")
              )
          ),

          continue_button("continue1")
        ),

        # =======================================================================
        # STEP 2, Run one study (x̄)
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 2",
          step_container(
            2, "Run one study: collect a sample, compute \\(M\\)",
            explanation_triad(
              formal = tagList(
                tags$b("Academic. "),
                "A ", tags$b("sample"),
                " is a finite collection of n units randomly drawn from the ",
                "population. The ", tags$b("sample mean"),
                ", written ", math_inline("M = \\frac{1}{n}\\sum_{i=1}^{n} x_i"),
                ", is our point estimate of ", math_inline("\\mu"), "."
              ),
              example = tagList(
                tags$b("In human words. "),
                "Pick ", math_inline("n"),
                " people at random, measure each one, and average their ",
                "scores. That single number is your best guess at ",
                math_inline("\\mu"),
                " from this one study. Different ", math_inline("n"),
                " people would give a slightly different answer."
              ),
              tldr = tagList(
                tags$b("Case study (peas). "),
                "The pea-study scientists assemble ", math_inline("n = 11"),
                " volunteer babies and record how many peas each one can ",
                "fit in a single ear. Their measurements are 5, 6, 7, 6, 5, ",
                "6, 6, 5, 7, 6, and 7. Averaging the eleven values gives ",
                math_inline("M = 6"),
                " peas, which is two peas above the four-pea benchmark. ",
                "The rest of the walkthrough decides whether that gap is ",
                "large enough to call the four-pea claim into question."
              )
            ),
            math_block("M = \\dfrac{x_1 + x_2 + \\cdots + x_n}{n}"),
            fluidRow(
              column(6, sliderInput("pop_n", "Sample size (n)",
                                    min = 2, max = 100, value = 20, step = 1)),
              column(6, div(class = "pad-top",
                            actionButton("run_one", "Run the study",
                                         class = "btn-primary")))
            ),
            plotOutput("step2_plot", height = "260px"),
            uiOutput("step2_stats"),
            callout_warm(
              tags$b("Try this:"),
              " run the study several times at n = 5, then again at n = 50. ",
              "Notice that ", math_inline("M"),
              " bounces around a lot at small n, but stays much closer to μ at ",
              "large n. That bounce is called ", tags$b("sampling variability"),
              ", and it is the entire reason we need a t-test. ",
              "Step 3 will turn this idea into a picture."
            ),
            continue_button("continue2")
          )
        ),

        # =======================================================================
        # STEP 3, Run it again (sampling variability), facet to keep pop fixed
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 3",
          step_container(
            3, "Run the study again. And again. (Sampling variability)",
            explanation_triad(
              formal = tagList(
                tags$b("Academic. "),
                "Repeating the experiment under identical conditions yields a ",
                "different ", math_inline("M"), " each time. ",
                "The set of all such ", math_inline("M"),
                " values forms an empirical distribution we can plot. The amount ",
                "by which sample means differ from each other is called ",
                tags$b("sampling variability"), "."
              ),
              example = tagList(
                tags$b("In human words. "),
                "If the same study were run by ten different labs, each lab ",
                "would draw a different ", math_inline("n"), " people and ",
                "get a slightly different average. Even in a world with no ",
                "real effect, the answers would not all line up. Random ",
                "sampling ", tags$em("jiggles"), " the result."
              ),
              tldr = tagList(
                tags$b("Case study (peas). "),
                "Imagine ten separate pea-study replications, each one ",
                "conducted by a different child-development lab. Each lab ",
                "would gather their own eleven babies, run the same ",
                "protocol, and report a slightly different sample mean. ",
                "The dot plot below stacks one green dot for each imagined ",
                "re-run, giving a visual record of how the result wiggles ",
                "from study to study."
              )
            ),
            div(style = "display:flex; gap:8px; flex-wrap:wrap;",
                actionButton("repeat_one",   "Run another study", class = "btn-primary"),
                actionButton("repeat_five",  "Run 5 more",        class = "btn-primary"),
                actionButton("repeat_reset", "Reset",             class = "btn-outline-secondary")
            ),
            plotOutput("step3_plot", height = "320px"),
            uiOutput("step3_counter"),
            callout_warm(
              tags$b("What to notice:"),
              " the dots cluster around μ but they do not all land on μ. ",
              "How wide is that cluster? Step 4 will let it grow ",
              "until it forms a recognizable bell curve, the ",
              tags$b("sampling distribution of the mean"), "."
            ),
            continue_button("continue3")
          )
        ),

        # =======================================================================
        # STEP 4, Sampling distribution (CLT)
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 4",
          step_container(
            4, "If we ran the study many times: the sampling distribution",
            explanation_triad(
              formal = tagList(
                tags$b("Academic. "),
                "The ", tags$b("sampling distribution of "),
                math_inline("M"),
                " is the probability distribution of all possible sample means ",
                "for a fixed n. Under repeated sampling from a population with ",
                "mean μ and SD σ, ",
                math_inline("M"), " has mean ", math_inline("\\mu"),
                " and SD ", math_inline("\\sigma/\\sqrt{n}"), ". ",
                "By the ", tags$b("Central Limit Theorem"),
                ", as n grows, the shape of this distribution approaches a ",
                "normal curve regardless of the population's original shape."
              ),
              example = tagList(
                tags$b("In human words. "),
                "Imagine running your study 500 times, instead of just 10. ",
                "The histogram of all 500 sample means ",
                "is a thought-experiment object, \"the distribution of how this ",
                "study could have turned out.\" You never actually collect this ",
                "in real life, but ", tags$em("you know"), " what shape it has, ",
                "because of the CLT."
              ),
              tldr = tagList(
                tags$b("Case study (peas). "),
                "If we imagine running the pea study with 500 different ",
                "samples of 11 babies, the 500 ", math_inline("M"),
                " values pile up into a bell curve centered on the true ",
                "population mean. That bell is the ",
                tags$b("sampling distribution of the mean"),
                ". The sampling distribution is a different object from the ",
                "sample distribution. The sample distribution shows the ",
                "eleven individual pea counts from one study. The sampling ",
                "distribution shows the cloud of possible sample means ",
                "across many imagined re-runs of the whole study."
              )
            ),
            help_details(
              "What is the Central Limit Theorem?",
              tagList(
                p(tags$b("Plain statement:"),
                  " when you average enough independent observations together, ",
                  "the distribution of those averages is approximately normal, ",
                  "even if the underlying population is not normal at all."),
                p(style = "margin-bottom:0;",
                  "Formally, as ", tags$b("n → ∞"),
                  ", the sampling distribution of ",
                  math_inline("\\bar{x}"),
                  " converges to ",
                  math_inline("\\mathcal{N}(\\mu,\\ \\sigma/\\sqrt{n})"),
                  ". This is the result that makes the t-distribution applicable ",
                  "to a wide range of populations.")
              )
            ),
            help_details(
              "Why is it called \"Student's\" t? A short history",
              tagList(
                p("The t-distribution was discovered in 1908 by ",
                  tags$b("William Sealy Gosset"),
                  ", a chemist and statistician working at the ",
                  tags$b("Guinness Brewery in Dublin"), "."),
                p("Gosset was trying to monitor barley and yeast quality with ",
                  "very small samples, often only 3 to 5 kegs, because brewing ",
                  "each one took weeks. The standard normal distribution assumed ",
                  "you knew the true population standard deviation, but Gosset ",
                  "had to estimate it from a handful of measurements, which made ",
                  "the normal approximation too confident at small n."),
                p(style = "margin-bottom:0;",
                  "He worked out the correct probability distribution for the ",
                  "ratio of sample mean minus benchmark, divided by ",
                  "estimated standard error, which is exactly the ",
                  "signal-to-noise ratio you will meet in Step 6. ",
                  "Guinness considered statistical methods a trade secret and ",
                  "wouldn't let employees publish under their own names, so Gosset ",
                  "published under the pen name ",
                  tags$em("\"Student.\""),
                  " The t-distribution has carried his pseudonym ever since.")
              )
            ),
            math_block("\\bar{x} \\;\\sim\\; \\mathcal{N}\\!\\left(\\mu,\\ \\dfrac{\\sigma}{\\sqrt{n}}\\right)"),
            div(style = "display:flex; gap:8px; flex-wrap:wrap;",
                actionButton("draw_50",    "Draw 50 more studies",  class = "btn-primary"),
                actionButton("draw_500",   "Draw 500 more studies", class = "btn-primary"),
                actionButton("draw_reset", "Reset", class = "btn-outline-secondary")
            ),
            plotOutput("step4_plot", height = "320px"),
            plot_legend(list(
              list(label = "Population distribution (one observation at a time)", type = "line",
                   color = PAL$pop_dark, dash = "dotted"),
              list(label = "Theoretical sampling distribution",  type = "line",
                   color = PAL$theory,   dash = "dashed"),
              list(label = "Simulated sample means (histogram)",  type = "fill",
                   color = PAL$samp_pt)
            )),
            uiOutput("step4_counter"),
            callout_warm(
              tags$b("Notice:"),
              " the histogram is centered on μ but it is ", tags$em("narrower"),
              " than the population curve. The width of this curve has a name. ",
              "That is the next step."
            ),
            continue_button("continue4")
          )
        ),

        # =======================================================================
        # STEP 5, Standard error
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 5",
          step_container(
            5, "How wide is that curve? Standard error",
            explanation_triad(
              formal = tagList(
                tags$b("Academic. "),
                "The ", tags$b("standard error of the mean"),
                ", written ", math_inline("s_M"),
                ", is the standard deviation of the sampling distribution of ",
                math_inline("M"),
                ". In real studies we estimate it from the sample data using ",
                "the formula ", math_inline("s_M = s/\\sqrt{n}"),
                ", where s is the sample standard deviation."
              ),
              example = tagList(
                tags$b("In human words. "),
                "Sample SD (s) describes how much the individual ", tags$em("scores"),
                " in one sample wiggle around the sample mean. Standard error ",
                math_inline("(s_M)"), " describes how much the ", tags$em("sample mean itself"),
                " wiggles from study to study. As ", math_inline("n"),
                " gets bigger, ", math_inline("s_M"), " shrinks, bigger ",
                "studies give more trustworthy averages."
              ),
              tldr = tagList(
                tags$b("Case study (peas). "),
                "For the pea data, the sample SD works out to ",
                math_inline("s = \\sqrt{6/10} \\approx 0.775"),
                ", computed from the deviations of the eleven counts (5, 6, ",
                "7, 6, 5, 6, 6, 5, 7, 6, 7) around their mean of 6. With ",
                math_inline("n = 11"),
                ", the estimated standard error is ",
                math_inline("s_M = 0.775/\\sqrt{11} \\approx 0.234"),
                " peas. The eleven scores in the sample scatter by about ",
                "0.78 peas (the sample SD). The sample mean across imagined ",
                "re-runs of the whole study scatters by about 0.23 peas (",
                "the standard error). Both quantities describe spread, ",
                "but they describe the spread of different things."
              )
            ),

            # Combined explanation (real-studies SD primary; σ as footnote)
            div(class = "p-3",
                style = "background:#fff; border:1px solid #eee; border-radius:8px;",
                h5("In real studies, SE comes from your sample data:"),
                math_block("\\text{SE} = \\dfrac{s}{\\sqrt{n}}"),
                p(tags$b("Why √n?"),
                  " When you average n independent measurements, random ups ",
                  "and downs partly cancel out. The math is precise: the ",
                  tags$em("variance"), " of an average of n independent ",
                  "measurements is exactly the original variance divided by n,"),
                math_block("\\text{Var}(\\bar{x}) = \\dfrac{s^2}{n}"),
                p("Standard deviation is the square root of variance, which gives ",
                  math_inline("\\text{SE} = s/\\sqrt{n}"),
                  ". The square-root relationship has a practical consequence: ",
                  tags$b("to cut SE in half, you need 4× the sample size; "),
                  "to cut SE by a factor of 10, you need 100× the sample. This ",
                  "is why studies with detectable but small effects often need ",
                  "thousands of participants. It is not arbitrary, it is the ",
                  "exact rate at which random noise averages out."),
                p(style = "margin-bottom:0;",
                  tags$b("Concrete numbers."),
                  " If s = 10 then n = 4 → SE = 5; n = 25 → SE = 2; ",
                  "n = 100 → SE = 1; n = 400 → SE = 0.5."),
                tags$details(
                  class = "mt-3",
                  tags$summary(
                    style = sprintf("cursor:pointer; font-weight:600; color:%s; font-size:13px;",
                                    PAL$pop_dark),
                    "Footnote: where σ comes in"),
                  div(class = "p-2 mt-1",
                      style = sprintf("background:#fff; border-left:3px solid %s; border-radius:4px; line-height:1.55; font-size:13px;",
                                      PAL$pop_dark),
                      "If you somehow knew the population SD ",
                      math_inline("\\sigma"),
                      " in advance, the textbook formula would be ",
                      math_inline("\\text{SE} = \\sigma/\\sqrt{n}"),
                      ". In real studies we never know σ, so we substitute s, ",
                      "and the t-distribution (instead of the normal) handles the ",
                      "extra uncertainty that introduces. From here on we use ",
                      math_inline("s/\\sqrt{n}"), ".")
                )
            ),

            # Stacked sliders + live equation panel
            div(class = "row g-3 mt-3",
                # Left: stacked sliders mirroring numerator (s) over denominator (n)
                div(class = "col-md-5",
                    div(class = "p-3 h-100",
                        style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                        h5("Inputs (numerator on top, denominator on bottom)",
                           style = "margin-top:0;"),
                        sliderInput("step5_sigma",
                                    "Population SD (σ), which sets the typical sample s",
                                    min = 1, max = 25, value = 1, step = 0.5,
                                    width = "100%"),
                        sliderInput("step5_n", "Sample size (n)",
                                    min = 2, max = 100, value = 11, step = 1,
                                    width = "100%"),
                        p(style = "color:#666; font-size:12px; margin-bottom:0;",
                          "These are the same inputs as Step 1 / Step 2, moving one ",
                          "moves the other.")
                    )
                ),
                # Right: live equation rendered with current values
                div(class = "col-md-7",
                    div(class = "p-3 h-100 d-flex align-items-center justify-content-center",
                        style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                        uiOutput("step5_live_eq")
                    )
                )
            ),

            # Figure underneath sliders
            div(class = "p-3 mt-3",
                style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                div(style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap;",
                    h5("Sampling distribution at every (σ, n) you've tried",
                       style = "margin:0;"),
                    actionButton("reset_se_history", "Reset history",
                                 class = "btn-sm btn-outline-secondary")
                ),
                p(style = "color:#666; font-size:13px; margin-top:6px;",
                  "Each curve is a normal sampling distribution for one ",
                  "(σ, n) combination. Most recent in solid black; older ",
                  "snapshots fade to lighter blue."),
                plotOutput("step5_history_plot", height = "260px")
            ),

            # Live calculation card BELOW the figure
            div(class = "p-3 mt-3",
                style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                h5("Numerical breakdown, current values", style = "margin-top:0;"),
                uiOutput("step5_live_calc")
            ),

            callout_warm(
              tags$b("Try this:"),
              " push n to 100 and watch the curve narrow. Push σ up and watch ",
              "it widen. Each move adds a snapshot to the history plot so ",
              "you can compare side by side."
            ),
            continue_button("continue5")
          )
        ),

        # =======================================================================
        # STEP 6, t-statistic
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 6",
          step_container(
            6, "Putting it together: the t-statistic",
            explanation_triad(
              formal = tagList(
                tags$b("Academic. "),
                "Let ", math_inline("\\mu_0"),
                " denote the value claimed by the null hypothesis ",
                math_inline("H_0:\\ \\mu = \\mu_0"),
                ". The ", tags$b("single-sample t-statistic"),
                " is the standardized distance between ",
                math_inline("M"), " and ", math_inline("\\mu_0"),
                ", measured in standard errors:"
              ),
              example = tagList(
                tags$b("In human words. "),
                "The numerator (", math_inline("M - \\mu_0"),
                ") is the ", tags$b("signal"),
                ", how far your sample average drifted from the benchmark. ",
                "The denominator (", math_inline("s_M = s/\\sqrt{n}"),
                ") is the ", tags$b("noise"),
                ", how much you'd ", tags$em("expect"),
                " the sample average to drift by chance alone. ",
                tags$em("t = signal ÷ noise."),
                " A big t means the drift is much larger than chance would produce."
              ),
              tldr = tagList(
                tags$b("Case study (peas). "),
                "For the pea study, the signal is ",
                math_inline("M - \\mu_0 = 6 - 4 = 2"),
                " peas. The noise is ",
                math_inline("s_M \\approx 0.234"),
                " peas. The resulting t-statistic is ",
                math_inline("t = 2 / 0.234 \\approx 8.55"),
                " with ", math_inline("df = n - 1 = 10"),
                ". A signal eight-and-a-half standard errors out from the ",
                "null is an enormous result. We are about to evaluate ",
                "whether it is large enough to reject the four-pea benchmark, ",
                "and the answer is going to be yes by a comfortable margin."
              )
            ),
            math_block("t \\;=\\; \\dfrac{\\bar{x} - \\mu_0}{s/\\sqrt{n}}"),

            # Three complementary readings
            div(class = "row g-3 my-2",
                div(class = "col-md-4",
                    div(class = "p-3 h-100",
                        style = sprintf("background:%s; color:%s; border-radius:8px;",
                                        PAL$info_bg, PAL$info_fg),
                        tags$b("Reading 1, Signal-to-noise ratio (primary)."),
                        p(style = "margin-bottom:0;",
                          tags$b("Signal"), " = x̄ − μ₀ (how far the sample is from the null). ",
                          tags$b("Noise"), " = s/√n (typical wiggle of x̄ across studies, ",
                          "i.e., the spread of the sampling distribution). ",
                          "A large |t| means the signal is large relative to the noise."))
                ),
                div(class = "col-md-4",
                    div(class = "p-3 h-100",
                        style = sprintf("background:%s; color:%s; border-radius:8px;",
                                        PAL$ok_bg, PAL$ok_fg),
                        tags$b("Reading 2, A position on the sampling distribution."),
                        p(style = "margin-bottom:0;",
                          "Because the denominator equals the SD of the null ",
                          "sampling distribution of x̄, t expresses where your ",
                          "sample lands on that distribution. t = 0 is the center; ",
                          "|t| ≫ 0 is far out in the tails."))
                ),
                div(class = "col-md-4",
                    div(class = "p-3 h-100",
                        style = sprintf("background:%s; color:%s; border-radius:8px;",
                                        PAL$warn_bg, PAL$warn_fg),
                        tags$b("Reading 3. Z-score for sample means."),
                        p(style = "margin-bottom:0;",
                          "A z-score uses σ. The t-statistic uses ",
                          math_inline("s/\\sqrt{n}"),
                          " because we are standardizing a sample mean on the ",
                          tags$em("sampling"),
                          " distribution. The single-observation version, ",
                          "which would standardize a raw value on the ",
                          "population distribution, uses σ in the ",
                          "denominator instead."))
                )
            ),

            div(class = "p-3 mt-3",
                style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                h5("Choose your null benchmark",
                   style = "margin-top:0;"),
                p(style = "color:#666; font-size:13px;",
                  math_inline("\\mu_0"),
                  " is the specific number your research question compares ",
                  "against. The pea study uses ", math_inline("\\mu_0 = 4"),
                  " (the four-pea conventional wisdom). Other examples: ",
                  "IQ test mean = 100; placement-test passing score = 70; ",
                  "body-temperature benchmark = 98.6°F."),
                fluidRow(
                  column(6, numericInput("mu0", "Null-hypothesis mean (μ₀)",
                                         value = 4, step = 0.5)),
                  column(6, "")
                )
            ),
            uiOutput("step6_calc"),
            plotOutput("step6_plot", height = "260px"),
            callout_warm(
              tags$b("Reading t on the sampling distribution:"),
              " t near 0 sits at the center of the null sampling distribution, ",
              "your signal is small relative to your noise. As |t| grows, ",
              "your sample slides into the tails. ",
              tags$em("How far"),
              " out it has to sit before we call it \"too extreme\" is what ",
              "the critical value will decide, and that requires first ",
              "choosing an ", tags$b("alpha level"), ", the next step."
            ),
            continue_button("continue6")
          )
        ),

        # =======================================================================
        # STEP 7, Setting alpha
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 7",
          step_container(
            7, "Setting alpha (\\(\\alpha\\)): how much risk are you willing to take?",
            explanation_triad(
              formal = tagList(
                tags$b("Alpha (α)"),
                " is the prespecified probability of rejecting the null ",
                "hypothesis when the null is in fact true, the ",
                tags$b("Type I error rate"),
                ". It is set ", tags$em("before"),
                " any data are collected. The orange area in the plot below ",
                "is an α-sized slice of the ", tags$em("null sampling distribution"),
                ", the most extreme signal-to-noise ratios that t can produce ",
                "when H₀ is true."
              ),
              example = tagList(
                "At α = 0.05, you are reserving the 5% of signal-to-noise ",
                "outcomes that sit furthest in the tails of the sampling ",
                "distribution. A t-statistic landing in that orange region ",
                "would be a rare result if H₀ were true, rare enough that ",
                "you'd rather reject H₀ than believe such a fluke. Stricter ",
                "fields (α = 0.01) shrink the tails. Pilot studies (α = 0.10) ",
                "widen them. The right value depends on what each kind of ",
                "mistake costs in context."
              ),
              tldr = tagList(
                "α is the slice of the ", tags$b("sampling distribution"),
                " you are willing to call \"too extreme to be H₀.\" The ",
                tags$em("edges"), " of that slice will become the critical ",
                "values in Step 8."
              )
            ),
            math_block("\\Pr(\\text{reject } H_0 \\mid H_0 \\text{ true}) \\;=\\; \\alpha"),

            div(class = "row g-3",
                column(width = 6,
                  div(class = "p-3",
                      style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                      h5("Alpha", style = "margin-top:0;"),
                      radioButtons(
                        "alpha_choice", NULL,
                        choiceNames = list(
                          HTML("<b>0.05  (5%, the standard default)</b>"),
                          "0.10  (10%, lenient, pilot studies)",
                          "0.01  (1%, strict, high-stakes research)"
                        ),
                        choiceValues = c("0.05", "0.10", "0.01"),
                        selected = "0.05"
                      )
                  )
                ),
                column(width = 6,
                  div(class = "p-3",
                      style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                      h5("Test type, one-tailed or two-tailed?",
                         style = "margin-top:0;"),
                      radioButtons(
                        "tail_choice", NULL,
                        choiceNames = list(
                          HTML("<b>Two-tailed</b> &mdash; testing whether \\(\\bar{x}\\) is <i>different</i> from \\(\\mu_0\\) in either direction"),
                          HTML("Right-tailed &mdash; testing whether \\(\\bar{x}\\) is <i>greater than</i> \\(\\mu_0\\)"),
                          HTML("Left-tailed &mdash; testing whether \\(\\bar{x}\\) is <i>less than</i> \\(\\mu_0\\)")
                        ),
                        choiceValues = c("two", "right", "left"),
                        selected = "two"
                      )
                  )
                )
            ),

            plotOutput("step7_plot", height = "260px"),

            # Detailed one-vs-two-tailed pedagogical section
            div(class = "p-3 mt-3",
                style = sprintf("background:%s; color:%s; border-radius:8px;",
                                PAL$info_bg, PAL$info_fg),
                h5("Where does the orange shaded region go?",
                   style = "margin-top:0;"),
                p(tags$b("Two-tailed (default)."),
                  " The α budget is split in half, α/2 in the left tail and ",
                  "α/2 in the right. You reject H₀ if t lands in either tail. ",
                  "Use this when you do not have a strong prior reason to expect ",
                  "the effect in only one direction."),
                p(tags$b("Right-tailed."),
                  " The full α sits in the right tail. You reject H₀ only if ",
                  math_inline("\\bar{x}"),
                  " is reliably greater than ", math_inline("\\mu_0"),
                  ". Use this when the alternative direction (",
                  math_inline("\\bar{x} < \\mu_0"),
                  ") is impossible or scientifically uninteresting."),
                p(tags$b("Left-tailed."),
                  " Mirror image, full α in the left tail."),
                p(style = "margin-bottom:0;",
                  tags$b("Practical guidance: "),
                  "use a two-tailed test by default. A one-tailed test gives ",
                  "you more statistical power in the predicted direction, but ",
                  "it makes effects in the opposite direction ",
                  tags$em("invisible"),
                  ". If a new drug might unexpectedly hurt patients, a ",
                  "right-tailed test will not catch that, and you would not ",
                  "want to miss it.")
            ),

            callout_warm(
              tags$b("Tradeoff:"),
              " smaller α reduces false alarms (Type I errors) but raises the ",
              "rate of missing real effects (Type II errors). Larger α does ",
              "the opposite. The right balance depends on what each kind of ",
              "mistake costs in context."
            ),
            continue_button("continue7")
          )
        ),

        # =======================================================================
        # STEP 8, Critical values + df history
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 8",
          step_container(
            8, "Critical values: where exactly does the orange end?",
            explanation_triad(
              formal = tagList(
                "The ", tags$b("critical value (CV)"),
                " is a specific point on the ",
                tags$em("null sampling distribution"),
                ", the signal-to-noise ratio that bounds the α-tail. For a ",
                "two-tailed test at level α with df degrees of freedom, ",
                math_inline("\\text{CV} = \\pm t_{\\alpha/2,\\,df}"),
                ". Any observed t with |t| ≥ |CV| has crossed that cutoff."
              ),
              example = tagList(
                "At α = 0.05, two-tailed: with n = 10 (df = 9) the CV is ±2.26. ",
                "That means a signal-to-noise ratio of ±2.26 marks the edge of ",
                "the 5% tail region on the df = 9 sampling distribution. ",
                "With n = 30 (df = 29) the cutoff tightens to ±2.05, the ",
                "sampling distribution is narrower, so a smaller signal-to-noise ",
                "is already \"extreme.\" With df = 1000 the CV is ≈ ±1.96."
              ),
              tldr = tagList(
                "CV = the ", tags$b("critical signal-to-noise ratio"),
                " on the null sampling distribution. Your t will be projected ",
                "against this line in the next step."
              )
            ),
            math_block("\\text{Two-tailed CV} \\;=\\; \\pm t_{\\alpha/2,\\,df},\\quad df = n - 1"),

            fluidRow(
              column(6, sliderInput("cv_df", "Degrees of freedom (df = n − 1)",
                                    min = 1, max = 100, value = 10, step = 1)),
              column(6,
                     selectInput("cv_alpha", "Alpha (α)",
                                 choices = c("0.10" = 0.10,
                                             "0.05" = 0.05,
                                             "0.01" = 0.01),
                                 selected = 0.05),
                     actionButton("reset_df_history", "Reset history",
                                  class = "btn-sm btn-outline-secondary"))
            ),
            plotOutput("step8_plot", height = "320px"),
            uiOutput("step8_cv_summary"),
            plot_legend(list(
              list(label = "Current t-distribution",        type = "line",
                   color = "black"),
              list(label = "Earlier df snapshots",          type = "line",
                   color = PAL$pop_med),
              list(label = "Rejection region (area = α)", type = "fill",
                   color = PAL$reject)
            )),

            callout_warm(
              tags$b("Try this:"),
              " set df = 1, then 3, then 10. At very low df the sampling ",
              "distribution has heavier tails, so the critical signal-to-noise ",
              "ratio gets pushed farther out (df = 1 → CV ≈ ±12.7). At higher ",
              "df the sampling distribution tightens and CV shrinks toward ",
              "±1.96. Either way, ", tags$em("CV is just a point on the sampling distribution"),
              ", its location depends on α and df, nothing else."
            ),

            div(class = "p-3 mt-3",
                style = sprintf("background:%s; color:%s; border-radius:8px;",
                                PAL$info_bg, PAL$info_fg),
                h5("The connection to z and the normal distribution",
                   style = "margin-top:0;"),
                p(style = "margin-bottom:6px;",
                  "When σ is known and n is large, the standardized statistic ",
                  math_inline("z = (\\bar{x} - \\mu_0)/(\\sigma/\\sqrt{n})"),
                  " follows a standard normal distribution. Its two-tailed ",
                  "α = 0.05 critical value is ", tags$b("±1.96"),
                  ", a number you have likely memorized."),
                p(style = "margin-bottom:0;",
                  "The t-distribution adjusts ±1.96 upward to account for the ",
                  "extra uncertainty from estimating σ with s. With df = 9 the ",
                  "two-tailed 0.05 CV is ±2.26 (a 15% widening). With df = 29 ",
                  "it is ±2.05. As df → ∞, t merges back into z and the CV ",
                  "approaches ±1.96 from above. ",
                  tags$b("The t-distribution is just z, plus an honesty correction for small samples."))
            ),

            continue_button("continue8")
          )
        ),

        # =======================================================================
        # STEP 9, The decision (synthesis page with all inputs duplicated)
        # =======================================================================
        conditionalPanel(
          condition = "output.walk_step >= 9",
          step_container(
            9, "The decision: reject \\(H_0\\), or fail to reject?",
            explanation_triad(
              formal = tagList(
                tags$b("Academic. "),
                "The decision rule projects your t-statistic, your ",
                tags$b("signal-to-noise ratio"), ", onto the null sampling ",
                "distribution and compares it with the critical value. ",
                "Reject ", math_inline("H_0"),
                " when ", math_inline("|t| \\geq \\text{CV}"),
                ". Equivalently, reject when the ", tags$b("p-value"),
                " (the tail area of the sampling distribution beyond your t) ",
                "is below α. Failing to reject is not the same as accepting ",
                "H₀; it means your signal-to-noise did not clear the cutoff."
              ),
              example = tagList(
                tags$b("In human words. "),
                "Compare your |t| with the critical value (the cutoff). If ",
                "your sample's signal-to-noise is bigger than the cutoff, your ",
                "result lands in the α-sized tail of the sampling ",
                "distribution, which is a rare event under H₀, and you ",
                "reject H₀. Otherwise, your ",
                "signal-to-noise sits inside the \"this is what H₀ regularly ",
                "produces\" zone, and you fail to reject."
              ),
              tldr = tagList(
                tags$b("Case study (peas). "),
                "For the pea study, signal = ",
                math_inline("M - \\mu_0 = 6 - 4 = 2"), " peas. Noise = ",
                math_inline("s_M \\approx 0.234"),
                ". The t-statistic works out to ",
                math_inline("t(10) \\approx 8.55"),
                ". The two-tailed critical value at α = .05 with df = 10 is ",
                "±2.228. Because 8.55 is far past 2.228, we ",
                tags$b("reject H₀"),
                ". The Onion's claim that babies max out at four peas does ",
                "not survive contact with the data. A reasonable APA-style ",
                "writeup would read: ",
                tags$em("\"Using a single-sample t-test, the pea-fitting "),
                tags$em("capacity of the eleven babies sampled (M = 6.00, "),
                tags$em("SD = 0.78) was significantly higher than the "),
                tags$em("conventional benchmark of four peas, "),
                tags$em("t(10) = 8.55, p < .001.\"")
              )
            ),
            math_block("\\text{Reject } H_0 \\;\\Longleftrightarrow\\; |t| \\geq \\text{CV} \\;\\Longleftrightarrow\\; p < \\alpha"),

            # ALL inputs collected here, synced with prior step inputs
            div(class = "p-3 mt-3",
                style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
                h5("Adjust any input, every prior step updates with you",
                   style = "margin-top:0;"),
                p(style = "color:#666; font-size:13px;",
                  "These controls are bidirectionally synced with the inputs in ",
                  "Steps 1, 2, 6, 7, and 8. Use them as the synthesis dashboard."),
                fluidRow(
                  column(4, sliderInput("step9_mu", "μ", min = 1, max = 100,
                                        value = 6, step = 0.5)),
                  column(4, sliderInput("step9_sigma", "σ", min = 1, max = 25,
                                        value = 1, step = 0.5)),
                  column(4, sliderInput("step9_n", "n", min = 2, max = 100,
                                        value = 11, step = 1))
                ),
                fluidRow(
                  column(4, numericInput("step9_mu0", "μ₀", value = 4, step = 0.5)),
                  column(4, selectInput("step9_alpha", "α",
                                        choices = c("0.10" = 0.10,
                                                    "0.05" = 0.05,
                                                    "0.01" = 0.01),
                                        selected = 0.05)),
                  column(4, selectInput("step9_tail", "Test type",
                                        choices = c("Two-tailed" = "two",
                                                    "Right-tailed" = "right",
                                                    "Left-tailed" = "left"),
                                        selected = "two"))
                ),
                div(style = "display:flex; gap:8px; flex-wrap:wrap;",
                    actionButton("step9_run", "Run a fresh study with these inputs",
                                 class = "btn-primary"),
                    actionButton("step9_reset_to_defaults",
                                 "Reset everything to defaults",
                                 class = "btn-outline-secondary"))
            ),

            uiOutput("step9_summary"),
            plotOutput("step9_plot", height = "360px"),
            plot_legend(list(
              list(label = "Rejection region (area = α)", type = "fill",
                   color = PAL$reject),
              list(label = "p-value (area beyond your t)",     type = "fill",
                   color = PAL$pval),
              list(label = "Critical value lines",             type = "line",
                   color = PAL$reject_dk, dash = "dashed"),
              list(label = "Your observed t",                  type = "line",
                   color = PAL$obs)
            )),
            callout_warm(
              tags$b("Plain-language summary:"),
              " every t-test reduces to the same projection, line your ",
              "observed ", tags$b("signal-to-noise ratio"),
              " up against the ", tags$b("critical signal-to-noise"),
              " on the null sampling distribution. ",
              tags$em("If the null were true, data this extreme would happen less than α of the time"),
              ", so when your t crosses the cutoff we discard H₀ as a working ",
              "assumption. \"Fail to reject\" means your signal-to-noise did not ",
              "clear the line, the data are consistent with H₀, but that is ",
              "not the same as proving H₀ is correct."
            )
          )
        ),

        # ===================================================================
        # Tutorial finale: slider-manipulation guide + practice-quest unlock
        # ===================================================================
        conditionalPanel(
          condition = "output.walk_step >= 9",
          div(class = "mt-5 mb-3 p-4",
              style = sprintf("background:#E1F1FB; border-left:6px solid %s; border-radius:10px;",
                              PAL$pop_dark),
              h3("Now play with the machinery",
                 style = "margin-top:0;"),
              p("The walkthrough above is built around one specific case ",
                "(eleven babies, a four-pea benchmark). That choice of ",
                "numbers is convenient for following along, but the test ",
                "itself behaves predictably as you change the inputs. The ",
                "best way to internalize how the parts fit together is to ",
                "go back through the steps and move the sliders around. ",
                "Here is a starter set of things to try, with what to look ",
                "for in each case."),
              tags$ul(style = "margin-bottom:0; padding-left:20px; line-height:1.7;",
                tags$li(tags$b("Sample size (Step 2 and Step 5). "),
                  "Push ", math_inline("n"), " from 10 up to 100. The ",
                  "sample mean ", math_inline("M"), " in Step 2 stops ",
                  "bouncing around as much, and the sampling distribution ",
                  "in Step 4 narrows. The standard error ",
                  math_inline("s_M = s/\\sqrt{n}"),
                  " in Step 5 shrinks by a factor of √n. Notice that ",
                  "quadrupling n only halves the standard error."),
                tags$li(tags$b("Population SD (Step 5). "),
                  "Slide ", math_inline("\\sigma"),
                  " from 1 up to 25 and watch the sampling-distribution ",
                  "curve widen. The signal-to-noise ratio gets harder to ",
                  "drive past the critical value as the population gets ",
                  "noisier. Bigger ", math_inline("\\sigma"),
                  " means we need a bigger ",
                  math_inline("M - \\mu_0"),
                  " gap (or a bigger n) to reject H₀."),
                tags$li(tags$b("Population mean (Step 9). "),
                  "Drag ", math_inline("\\mu"),
                  " above and below ", math_inline("\\mu_0"),
                  ". The t-statistic crosses zero exactly when the true ",
                  "population mean equals the benchmark, and grows in ",
                  "magnitude as the true mean moves further away. ",
                  "Symmetric: ", math_inline("\\mu = \\mu_0 + 1"),
                  " gives the same |t| as ",
                  math_inline("\\mu = \\mu_0 - 1"), "."),
                tags$li(tags$b("Alpha and df (Step 8). "),
                  "Pick α = .10 to see the rejection region widen. Pick ",
                  "α = .01 to see it shrink. Slide df from 1 to 50 and ",
                  "watch the t-distribution's tails get lighter as df ",
                  "grows. At df = 1 the critical value is ±12.7 (we need ",
                  "an extreme signal-to-noise to reject), but at df = 50 ",
                  "the critical value is back near ±2.0."),
                tags$li(tags$b("Step 9 dashboard. "),
                  "Step 9 collects every input on one screen. Drag the ",
                  "sliders here and watch the verdict flip between ",
                  tags$em("Reject H₀"), " and ", tags$em("Fail to reject"),
                  ". Try to find the smallest sample size that turns a ",
                  "marginally non-significant result into a significant ",
                  "one. Then try to find a noise level that does the ",
                  "opposite."))
          ),
          div(class = "text-center mb-4",
              p(style = "color:#444; font-size:14px;",
                "When you have spent a few minutes playing with the ",
                "sliders, unlock the practice quest below. The quest gives ",
                "you a fresh dataset and walks you through the same nine ",
                "steps with new numbers."),
              actionButton("tab1_show_quest",
                           "Show me the practice quest →",
                           class = "btn-primary btn-lg"))
        ),

        # ===================================================================
        # QUEST. body-temperature study (Cohen & Marill, 2018)
        # ===================================================================
        conditionalPanel(
          condition = "input.tab1_show_quest > 0",
          quest_section(
          title = "Is 98.6°F still the right benchmark for healthy body temperature?",
          id_prefix = "tab1quest",
          scenario_html = tagList(
            p("The figure 98.6°F was reported in 1868 by the German ",
              "physician Carl Wunderlich, who took roughly a million ",
              "axillary measurements with a mercury thermometer that ",
              "today's metrology would not consider especially accurate. ",
              "Cohen and Marill (2018) revisited the question using ",
              "modern data from the Feverprints project and reported that ",
              "the population mean for healthy adults appears to be lower ",
              "than Wunderlich's value. Your job in this quest is to use ",
              "a one-sample t-test to evaluate whether the data are ",
              "consistent with 98.6°F as the population mean."),
            p(style = "margin-bottom:0;",
              "Imagine you have a fresh dataset of ", math_inline("n = 30"),
              " healthy adult oral temperatures, with sample mean ",
              math_inline("M = 97.7"), "°F and sample standard deviation ",
              math_inline("s = 0.72"), "°F. The benchmark you want to ",
              "compare against is ", math_inline("\\mu_0 = 98.6"),
              "°F. Use α = .05, two-tailed.")
          ),
          data_block = tagList(
            tags$ul(style = "margin-bottom:0;",
              tags$li(tags$b("Sample size: "), math_inline("n = 30")),
              tags$li(tags$b("Sample mean: "), math_inline("M = 97.7"), "°F"),
              tags$li(tags$b("Sample SD: "), math_inline("s = 0.72"), "°F"),
              tags$li(tags$b("Benchmark: "),
                      math_inline("\\mu_0 = 98.6"), "°F"),
              tags$li(tags$b("Alpha: "), "α = .05, two-tailed"))
          ),
          questions = list(
            list(
              prompt = tagList(
                tags$b("Step 1. "), "State the null and alternative ",
                "hypotheses for the body-temperature study, and identify ",
                "the population value that H₀ specifies."),
              solution = tagList(
                p(math_inline("H_0: \\mu = 98.6"), "°F. The population mean ",
                  "of healthy adult body temperature equals Wunderlich's ",
                  "value of 98.6°F."),
                p(math_inline("H_1: \\mu \\neq 98.6"),
                  "°F. The population mean differs from 98.6°F in some ",
                  "unspecified direction. The two-tailed framing is the ",
                  "right choice here because Cohen and Marill's hypothesis ",
                  "did not specify a direction in advance, and a healthy-",
                  "adult population could in principle be either warmer or ",
                  "cooler than Wunderlich's value."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 2. "), "What are the sample statistics for ",
                "this study, and what symbols would the lab key use to ",
                "name them?"),
              solution = tagList(
                p("The sample mean is ", math_inline("M = 97.7"),
                  "°F (the average of the 30 individual oral temperatures). ",
                  "The sample standard deviation is ",
                  math_inline("s = 0.72"),
                  "°F (the spread of those 30 temperatures around the ",
                  "sample mean). The sample size is ", math_inline("n = 30"),
                  ". These three numbers are everything we need from the ",
                  "data to run the test."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 3. "), "Imagine the Feverprints team gathered ",
                "a different sample of 30 healthy adults next week. Would ",
                "you expect to see the same ", math_inline("M = 97.7"),
                "°F? Why or why not?"),
              solution = tagList(
                p("No. Next week's sample of 30 would draw 30 different ",
                  "people, and even from the same population, the new ",
                  "sample mean would land at a slightly different value. ",
                  "This run-to-run variation is sampling variability. It ",
                  "is a property of the procedure rather than of any single ",
                  "dataset. Over many imagined re-runs, the ",
                  math_inline("M"),
                  " values would cluster around the true population mean, ",
                  "with a width given by the standard error."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 4. "), "Under H₀, where would the sampling ",
                "distribution of ", math_inline("M"),
                " be centered? What shape would it take?"),
              solution = tagList(
                p("Under H₀, the sampling distribution of ", math_inline("M"),
                  " is centered at ", math_inline("\\mu_0 = 98.6"),
                  "°F. Its shape is a t-distribution with ",
                  math_inline("df = n - 1 = 29"),
                  ". With n that large, the distribution is close to ",
                  "normal in shape but with slightly heavier tails. The ",
                  "width of this sampling distribution is set by the ",
                  "standard error you will compute next."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 5. "), "Compute the standard error of ",
                math_inline("M"), " from ",
                math_inline("s"), " and ", math_inline("n"), "."),
              solution = tagList(
                p(math_inline("s_M = s/\\sqrt{n} = 0.72/\\sqrt{30}"),
                  ". The square root of 30 is approximately 5.477, so ",
                  math_inline("s_M \\approx 0.72 / 5.477 \\approx 0.131"),
                  "°F. Across imagined re-runs of this study, the sample ",
                  "mean would typically wiggle by about 0.13°F."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 6. "), "Compute the t-statistic and write a ",
                "one-sentence interpretation of what it represents."),
              solution = tagList(
                p(math_inline("t = (M - \\mu_0)/s_M = (97.7 - 98.6)/0.131"),
                  ". The numerator is the signal of −0.9°F. Dividing by ",
                  "the noise estimate of 0.131°F gives ",
                  math_inline("t \\approx -6.87"),
                  ". The observed sample mean sits about 6.87 standard ",
                  "errors below the benchmark value of 98.6°F."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 7. "), "What α value would you set for this ",
                "test, and what does that choice represent?"),
              solution = tagList(
                p("Use α = .05, two-tailed, following the PSY 302 default. ",
                  "Alpha represents the long-run rate at which we would ",
                  "reject H₀ in a world where H₀ is in fact true. The ",
                  ".05 value is split evenly across the two tails, so 2.5% ",
                  "of the sampling distribution sits in each tail of the ",
                  "rejection region."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 8. "), "What is the two-tailed critical value ",
                "at α = .05 with the appropriate degrees of freedom?"),
              solution = tagList(
                p("With ", math_inline("df = n - 1 = 29"),
                  ", the two-tailed .05 critical values are ",
                  math_inline("\\pm t_{.025,\\ 29} = \\pm 2.045"),
                  ". Any observed t-statistic with ",
                  math_inline("|t| \\geq 2.045"),
                  " lands in the rejection region of the t-distribution ",
                  "with 29 degrees of freedom."))
            ),
            list(
              prompt = tagList(
                tags$b("Step 9. "), "Compare your observed t to the ",
                "critical value. What is your decision? Write up the ",
                "result in APA-style prose."),
              solution = tagList(
                p("The observed value of ", math_inline("|t| = 6.87"),
                  " is much larger than the critical value of 2.045, so we ",
                  tags$b("reject H₀"), ". The corresponding p-value is ",
                  "less than .001."),
                p(style = "margin-bottom:0;",
                  "APA-style writeup: ",
                  tags$em("\"Using a single-sample t-test, the average "),
                  tags$em("body temperature in our sample of healthy adults "),
                  tags$em("(M = 97.7°F, SD = 0.72) was significantly lower "),
                  tags$em("than Wunderlich's historical benchmark of 98.6°F, "),
                  tags$em("t(29) = −6.87, p < .001.\""),
                  " Cohen and Marill's hypothesis is supported. The data ",
                  "are not consistent with 98.6°F as the population mean ",
                  "of healthy adult body temperature."))
            )
          )
        )
        )  # close conditionalPanel for tab1_show_quest

            )   # close col-lg-9 (main column)
        )       # close row
    )           # close container-fluid
  ),

  # ===========================================================================
  # TAB 2: Paired-samples t-test (staring down seagulls)
  # ===========================================================================
  nav_panel(
    title = "Paired t",
    uiOutput("paired_tab_ui")
  ),

  # ===========================================================================
  # TAB 3: Independent-samples t-test (Daves know more Daves)
  # ===========================================================================
  nav_panel(
    title = "Independent t",
    uiOutput("indep_tab_ui")
  ),

  # ===========================================================================
  # TAB 4: One-Way ANOVA (state-level neuroticism by US region)
  # ===========================================================================
  nav_panel(
    title = "One-Way ANOVA",
    uiOutput("anova_tab_ui")
  ),

  # ===========================================================================
  # TAB 5: Factorial ANOVA (parental medication dosing)
  # ===========================================================================
  nav_panel(
    title = "Factorial ANOVA",
    uiOutput("fact_tab_ui")
  ),

  nav_spacer(),
  nav_item(tags$a(href = "https://shiny.posit.co/", target = "_blank",
                  "Built with Shiny",
                  style = "font-size:12px; color:#888;"))
)

# Server =======================================================================
server <- function(input, output, session) {

  # ============================================================================
  # State
  # ============================================================================
  walk <- reactiveValues(
    step          = 1,
    last_sample   = numeric(0),
    repeat_means  = numeric(0),
    many_means    = numeric(0),
    se_history    = list(),
    df_history    = list()
  )

  # Expose walk$step to JS for conditionalPanel
  output$walk_step <- reactive({ walk$step })
  outputOptions(output, "walk_step", suspendWhenHidden = FALSE)

  # --- Sticky sidebar TOC (Radiant-style step navigator) -------------------
  step_titles <- c(
    "The null world",
    "Run one study",
    "Run it again",
    "Sampling distribution",
    "Standard error",
    "The t-statistic",
    "Setting alpha",
    "Critical values",
    "The decision"
  )
  output$step_toc <- renderUI({
    cur <- walk$step
    pct <- round((cur - 1) / (length(step_titles) - 1) * 100)
    items <- lapply(seq_along(step_titles), function(i) {
      state <- if (i < cur)  "completed"
               else if (i == cur) "current"
               else                "locked"
      check <- if (state == "completed") tags$span(class = "toc-check", HTML("&#10003;")) else NULL
      # Locked steps don't render a link target.
      href  <- if (state == "locked") NULL else sprintf("#step-%d", i)
      tags$a(
        href  = href,
        class = paste("toc-item", state),
        tags$span(class = "toc-num", i),
        tags$span(class = "toc-title", step_titles[i]),
        check
      )
    })
    div(class = "sidebar-toc",
        div(class = "sidebar-toc-title", "Walkthrough"),
        div(class = "toc-progress-wrap",
            div(class = "toc-progress-bar",
                style = sprintf("width:%d%%;", pct))),
        div(class = "toc-progress-label",
            sprintf("Step %d of %d  ·  %d%% complete", cur, length(step_titles), pct)),
        items
    )
  })

  # Sample data resets only on EXPLICIT parameter changes (we use direct value
  # comparison so spurious re-fires don't reset the user's hard-earned sample).
  prev_pop <- reactiveValues(mu = 6, sigma = 1, n = 11)
  observe({
    mu <- input$pop_mu; sigma <- input$pop_sigma; n <- input$pop_n
    if (is.null(mu) || is.null(sigma) || is.null(n)) return()
    if (mu != prev_pop$mu || sigma != prev_pop$sigma || n != prev_pop$n) {
      walk$last_sample  <- numeric(0)
      walk$repeat_means <- numeric(0)
      walk$many_means   <- numeric(0)
      prev_pop$mu <- mu; prev_pop$sigma <- sigma; prev_pop$n <- n
    }
  })

  # Continue handlers
  observeEvent(input$continue1, { walk$step <- max(walk$step, 2) })
  observeEvent(input$continue2, { walk$step <- max(walk$step, 3) })
  observeEvent(input$continue3, { walk$step <- max(walk$step, 4) })
  observeEvent(input$continue4, { walk$step <- max(walk$step, 5) })
  observeEvent(input$continue5, { walk$step <- max(walk$step, 6) })
  observeEvent(input$continue6, { walk$step <- max(walk$step, 7) })
  observeEvent(input$continue7, { walk$step <- max(walk$step, 8) })
  observeEvent(input$continue8, { walk$step <- max(walk$step, 9) })

  # --- Bidirectional sync helpers -------------------------------------------
  # Sliders: pop_sigma <-> step5_sigma <-> step9_sigma
  sync_pair_slider <- function(a, b) {
    observeEvent(input[[a]], {
      av <- input[[a]]; bv <- input[[b]]
      if (!is.null(bv) && !isTRUE(all.equal(bv, av))) {
        updateSliderInput(session, b, value = av)
      }
    }, ignoreInit = TRUE)
    observeEvent(input[[b]], {
      av <- input[[a]]; bv <- input[[b]]
      if (!is.null(av) && !isTRUE(all.equal(av, bv))) {
        updateSliderInput(session, a, value = bv)
      }
    }, ignoreInit = TRUE)
  }
  sync_pair_slider("pop_sigma",  "step5_sigma")
  sync_pair_slider("step5_sigma","step9_sigma")
  sync_pair_slider("pop_sigma",  "step9_sigma")
  sync_pair_slider("pop_n",      "step5_n")
  sync_pair_slider("step5_n",    "step9_n")
  sync_pair_slider("pop_n",      "step9_n")
  sync_pair_slider("pop_mu",     "step9_mu")

  # Numeric input sync: mu0 <-> step9_mu0
  observeEvent(input$mu0, {
    if (!is.null(input$step9_mu0) && !isTRUE(all.equal(input$step9_mu0, input$mu0))) {
      updateNumericInput(session, "step9_mu0", value = input$mu0)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$step9_mu0, {
    if (!is.null(input$mu0) && !isTRUE(all.equal(input$mu0, input$step9_mu0))) {
      updateNumericInput(session, "mu0", value = input$step9_mu0)
    }
  }, ignoreInit = TRUE)

  # Select / radio sync: alpha_choice <-> step9_alpha; tail_choice <-> step9_tail
  observeEvent(input$alpha_choice, {
    v <- input$alpha_choice
    if (!is.null(input$step9_alpha) && !isTRUE(all.equal(as.numeric(input$step9_alpha), as.numeric(v)))) {
      updateSelectInput(session, "step9_alpha", selected = v)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$step9_alpha, {
    v <- input$step9_alpha
    if (!is.null(input$alpha_choice) && !isTRUE(all.equal(as.numeric(input$alpha_choice), as.numeric(v)))) {
      updateRadioButtons(session, "alpha_choice", selected = as.character(v))
    }
  }, ignoreInit = TRUE)
  observeEvent(input$tail_choice, {
    v <- input$tail_choice
    if (!is.null(input$step9_tail) && input$step9_tail != v) {
      updateSelectInput(session, "step9_tail", selected = v)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$step9_tail, {
    v <- input$step9_tail
    if (!is.null(input$tail_choice) && input$tail_choice != v) {
      updateRadioButtons(session, "tail_choice", selected = v)
    }
  }, ignoreInit = TRUE)

  # SE history
  observeEvent(c(input$pop_sigma, input$pop_n), {
    sigma <- input$pop_sigma; n <- input$pop_n
    if (is.null(sigma) || is.null(n) || sigma <= 0 || n < 1) return()
    walk$se_history <- c(walk$se_history,
                         list(list(sigma = sigma, n = n, se = sigma / sqrt(n))))
    if (length(walk$se_history) > 8)
      walk$se_history <- tail(walk$se_history, 8)
  })
  observeEvent(input$reset_se_history, { walk$se_history <- list() })

  # df history
  observeEvent(c(input$cv_df, input$cv_alpha), {
    dfv <- input$cv_df; alpha <- as.numeric(input$cv_alpha)
    if (is.null(dfv) || is.null(alpha) || dfv < 1) return()
    walk$df_history <- c(walk$df_history,
                         list(list(df = dfv, alpha = alpha,
                                   cv = qt(1 - alpha / 2, dfv))))
    if (length(walk$df_history) > 6)
      walk$df_history <- tail(walk$df_history, 6)
  })
  observeEvent(input$reset_df_history, { walk$df_history <- list() })

  # Step 9 buttons
  observeEvent(input$step9_run, {
    walk$last_sample <- rnorm(input$pop_n, input$pop_mu, input$pop_sigma)
  })
  observeEvent(input$step9_reset_to_defaults, {
    updateSliderInput(session, "pop_mu", value = 6)
    updateSliderInput(session, "pop_sigma", value = 1)
    updateSliderInput(session, "pop_n", value = 11)
    updateNumericInput(session, "mu0", value = 4)
    updateRadioButtons(session, "alpha_choice", selected = "0.05")
    updateRadioButtons(session, "tail_choice", selected = "two")
    walk$last_sample  <- numeric(0)
    walk$repeat_means <- numeric(0)
    walk$many_means   <- numeric(0)
    walk$se_history   <- list()
    walk$df_history   <- list()
  })

  # ============================================================================
  # STEP 1, population plot
  # ============================================================================
  output$popPlot <- renderPlotly({
    mu    <- input$pop_mu    %||% 6
    sigma <- input$pop_sigma %||% 1

    make_band <- function(a, b, color, label) {
      xs <- seq(a, b, length.out = 80)
      ys <- dnorm(xs, mu, sigma)
      list(x = c(a, xs, b), y = c(0, ys, 0), color = color, label = label)
    }

    bands <- list(
      make_band(mu - 4 * sigma, mu - 3 * sigma, PAL$band_far,
                "Beyond −3σ<br>≈0.15% of population is here<br>≈0.15% of population is at or below −3σ"),
      make_band(mu - 3 * sigma, mu - 2 * sigma, PAL$band3,
                "Between −3σ and −2σ<br>≈2.35% of population is here<br>≈2.5% of population is at or below −2σ"),
      make_band(mu - 2 * sigma, mu - 1 * sigma, PAL$band2,
                "Between −2σ and −1σ<br>≈13.5% of population is here<br>≈16% of population is at or below −1σ"),
      make_band(mu - 1 * sigma, mu + 1 * sigma, PAL$band1,
                "Within ±1σ of μ<br>≈68% of population is here<br>≈84% of population is at or below +1σ"),
      make_band(mu + 1 * sigma, mu + 2 * sigma, PAL$band2,
                "Between +1σ and +2σ<br>≈13.5% of population is here<br>≈97.5% of population is at or below +2σ"),
      make_band(mu + 2 * sigma, mu + 3 * sigma, PAL$band3,
                "Between +2σ and +3σ<br>≈2.35% of population is here<br>≈99.85% of population is at or below +3σ"),
      make_band(mu + 3 * sigma, mu + 4 * sigma, PAL$band_far,
                "Beyond +3σ<br>≈0.15% of population is here<br>≈99.85% of population is at or below +3σ")
    )

    p <- plot_ly()
    for (b in bands) {
      p <- p %>% add_polygons(
        x = b$x, y = b$y,
        fillcolor = b$color, opacity = 0.78,
        line = list(width = 0),
        hoveron = "fills",
        hoverinfo = "text", text = b$label,
        showlegend = FALSE
      )
    }

    xc <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 400)
    p <- p %>% add_lines(
      x = xc, y = dnorm(xc, mu, sigma),
      line = list(color = "#222", width = 2),
      hoverinfo = "skip", showlegend = FALSE
    )

    y_max <- dnorm(mu, mu, sigma)
    p <- p %>% add_segments(
      x = mu, xend = mu, y = 0, yend = y_max,
      line = list(color = "#222", width = 1, dash = "dash"),
      hoverinfo = "skip", showlegend = FALSE
    )

    fmt <- function(v) formatC(round(v, 1), format = "g", drop0trailing = TRUE)
    tickvals <- c(mu - 3 * sigma, mu - 2 * sigma, mu - sigma,
                  mu, mu + sigma, mu + 2 * sigma, mu + 3 * sigma)
    ticktext <- c(
      paste0("μ−3σ<br><b>", fmt(mu - 3 * sigma), "</b>"),
      paste0("μ−2σ<br><b>", fmt(mu - 2 * sigma), "</b>"),
      paste0("μ−σ<br><b>",  fmt(mu - sigma),     "</b>"),
      paste0("μ<br><b>",    fmt(mu),             "</b>"),
      paste0("μ+σ<br><b>",  fmt(mu + sigma),     "</b>"),
      paste0("μ+2σ<br><b>", fmt(mu + 2 * sigma), "</b>"),
      paste0("μ+3σ<br><b>", fmt(mu + 3 * sigma), "</b>")
    )

    # Cumulative percentage annotations above the x-axis
    cum_labels <- c("0.15%", "2.5%", "16%", "50%", "84%", "97.5%", "99.85%")
    annots <- lapply(seq_along(tickvals), function(i) {
      list(x = tickvals[i], y = 1.05, xref = "x", yref = "paper",
           text = paste0(cum_labels[i], "<br>below"),
           showarrow = FALSE,
           font = list(size = 10, color = "#555"),
           align = "center")
    })

    p %>% layout(
      xaxis = list(title = "Population value",
                   tickvals = tickvals, ticktext = ticktext,
                   zeroline = FALSE),
      yaxis = list(title = "Density", showticklabels = FALSE,
                   zeroline = FALSE,
                   range = c(0, y_max * 1.15)),
      hoverlabel = list(bgcolor = "white", font = list(size = 12),
                        align = "left"),
      hovermode = "closest",
      annotations = annots,
      margin = list(t = 50, r = 20, b = 60, l = 50)
    ) %>% config(displayModeBar = FALSE)
  })

  # ============================================================================
  # STEP 2, one study, with population overlay
  # ============================================================================
  observeEvent(input$run_one, {
    walk$last_sample <- rnorm(input$pop_n, input$pop_mu, input$pop_sigma)
  })

  output$step2_plot <- renderPlot({
    mu    <- input$pop_mu    %||% 6
    sigma <- input$pop_sigma %||% 1
    s     <- walk$last_sample
    xlim  <- c(mu - 4 * sigma, mu + 4 * sigma)
    y_max <- dnorm(mu, mu, sigma)
    y_top <- y_max * 1.45

    g <- ggplot() +
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                    geom = "area", fill = PAL$pop_med, alpha = 0.22,
                    xlim = xlim, n = 400) +
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                    colour = PAL$pop_dark, linewidth = 0.8, alpha = 0.6,
                    xlim = xlim, n = 400) +
      geom_vline(xintercept = mu, colour = PAL$pop_dark,
                 linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
      annotate("label", x = mu, y = y_max * 1.15, label = sprintf("μ = %g", mu), colour = PAL$pop_dark, fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95, size = 4)

    if (length(s) == 0) {
      g <- g + annotate("label", x = mu, y = y_max * 0.5,
                        label = "Click \"Run the study\" to draw a sample",
                        colour = "#666", fill = "white", label.size = NA,
                        size = 4.2)
    } else {
      m <- mean(s)
      x_label <- if (abs(m - mu) < sigma * 0.4) m + sigma * 0.6 else m
      vjust_label <- if (abs(m - mu) < sigma * 0.4) 0.5 else 0.5
      g <- g +
        geom_jitter(data = data.frame(x = s, y = y_max * 0.05),
                    aes(x = x, y = y),
                    colour = PAL$samp_pt, size = 3.2, alpha = 0.8,
                    width = 0, height = y_max * 0.025) +
        geom_vline(xintercept = m, colour = PAL$samp_line, linewidth = 1) +
        annotate("label", x = x_label, y = y_top * 0.9,
                       label = sprintf("x̄ = %.2f", m), colour = PAL$samp_line, fill = "white", label.size = NA,
                   label.r = unit(0.15, "lines"), alpha = 0.95,
                   size = 4.5, fontface = "bold")
    }
    g + scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = c(0, y_top * 1.05)) +
      labs(x = "Measurement value", y = NULL) +
      base_theme() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }, res = 96)

  output$step2_stats <- renderUI({
    s <- walk$last_sample
    if (length(s) == 0) return(NULL)
    div(class = "p-3 my-2",
        style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
        fluidRow(
          column(4, stat_card("Sample mean (x̄)", sprintf("%.2f", mean(s)))),
          column(4, stat_card("Sample SD (s)",   sprintf("%.2f", sd(s)))),
          column(4, stat_card("Sample size (n)", length(s)))
        )
    )
  })

  # ============================================================================
  # STEP 3, facet to keep population fixed; auto-resize y for dot stack
  # ============================================================================
  observeEvent(input$repeat_one, {
    s <- rnorm(input$pop_n, input$pop_mu, input$pop_sigma)
    walk$repeat_means <- c(walk$repeat_means, mean(s))
    walk$last_sample  <- s
  })
  observeEvent(input$repeat_five, {
    new_means <- replicate(5, mean(rnorm(input$pop_n, input$pop_mu, input$pop_sigma)))
    walk$repeat_means <- c(walk$repeat_means, new_means)
  })
  observeEvent(input$repeat_reset, { walk$repeat_means <- numeric(0) })

  output$step3_plot <- renderPlot({
    mu    <- input$pop_mu    %||% 6
    sigma <- input$pop_sigma %||% 1
    means <- walk$repeat_means
    xlim  <- c(mu - 4 * sigma, mu + 4 * sigma)

    # Two-row facet: population panel on top (fixed), dot stack below (free)
    pop_x <- seq(xlim[1], xlim[2], length.out = 400)
    df_pop <- data.frame(x = pop_x, y = dnorm(pop_x, mu, sigma),
                         panel = factor("Population (\"one observation at a time\")",
                                        levels = c("Population (\"one observation at a time\")",
                                                   "Sample means (each dot = one study)")))
    df_dots <- if (length(means) > 0)
      data.frame(x = means,
                 panel = factor("Sample means (each dot = one study)",
                                levels = c("Population (\"one observation at a time\")",
                                           "Sample means (each dot = one study)")))
    else NULL

    df_mu_pop <- data.frame(panel = factor("Population (\"one observation at a time\")",
                                           levels = c("Population (\"one observation at a time\")",
                                                      "Sample means (each dot = one study)")),
                            x = mu)
    df_mu_dot <- data.frame(panel = factor("Sample means (each dot = one study)",
                                           levels = c("Population (\"one observation at a time\")",
                                                      "Sample means (each dot = one study)")),
                            x = mu)

    g <- ggplot() +
      geom_line(data = df_pop, aes(x, y),
                colour = PAL$pop_dark, linewidth = 0.8) +
      geom_area(data = df_pop, aes(x, y),
                fill = PAL$pop_med, alpha = 0.18) +
      geom_vline(data = df_mu_pop, aes(xintercept = x),
                 colour = PAL$pop_dark, linetype = "dashed", linewidth = 0.4)

    # μ label only on top facet
    g <- g + geom_label(
      data = data.frame(x = mu, y = max(df_pop$y) * 1.05,
                        label = sprintf("μ = %g", mu),
                        panel = factor("Population (\"one observation at a time\")",
                                       levels = c("Population (\"one observation at a time\")",
                                                  "Sample means (each dot = one study)"))),
      aes(x = x, y = y, label = label),
      colour = PAL$pop_dark, fill = "white", label.size = NA,
      label.r = unit(0.15, "lines"), alpha = 0.95, size = 4
    )

    if (!is.null(df_dots)) {
      g <- g + geom_dotplot(
        data = df_dots, aes(x = x),
        binwidth = (xlim[2] - xlim[1]) / 60,
        fill = PAL$samp_pt, colour = "white",
        stackdir = "up", dotsize = 0.85,
        method = "histodot", stackratio = 1.05
      ) +
        geom_vline(data = df_mu_dot, aes(xintercept = x),
                   colour = PAL$pop_dark, linetype = "dashed",
                   linewidth = 0.4, alpha = 0.6)
    }

    g + facet_grid(panel ~ ., scales = "free_y", switch = "y") +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(NULL, breaks = NULL) +
      labs(x = "Value", y = NULL) +
      base_theme() +
      theme(strip.placement = "outside",
            strip.text.y.left = element_text(angle = 0, face = "bold",
                                              size = 11, colour = "#333"),
            strip.background = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }, res = 96)

  output$step3_counter <- renderUI({
    n_studies <- length(walk$repeat_means)
    div(class = "p-2 my-2", style = sprintf("color:%s; font-size:14px;", PAL$pop_dark),
        sprintf("Studies you have run so far: %d", n_studies))
  })

  # ============================================================================
  # STEP 4, sampling distribution
  # ============================================================================
  observeEvent(input$draw_50, {
    new_means <- replicate(50, mean(rnorm(input$pop_n, input$pop_mu, input$pop_sigma)))
    walk$many_means <- c(walk$many_means, new_means)
  })
  observeEvent(input$draw_500, {
    new_means <- replicate(500, mean(rnorm(input$pop_n, input$pop_mu, input$pop_sigma)))
    walk$many_means <- c(walk$many_means, new_means)
  })
  observeEvent(input$draw_reset, { walk$many_means <- numeric(0) })

  output$step4_plot <- renderPlot({
    mu    <- input$pop_mu    %||% 6
    sigma <- input$pop_sigma %||% 1
    n     <- input$pop_n     %||% 11
    se    <- sigma / sqrt(n)
    means <- walk$many_means
    xlim  <- c(mu - 4 * sigma, mu + 4 * sigma)

    x  <- seq(xlim[1], xlim[2], length.out = 400)
    th <- data.frame(x = x, y = dnorm(x, mu, se))
    pop <- data.frame(x = x, y = dnorm(x, mu, sigma))

    g <- ggplot() +
      geom_line(data = pop, aes(x, y),
                colour = PAL$pop_dark, linewidth = 0.6,
                linetype = "dotted", alpha = 0.7)
    if (length(means) > 0) {
      g <- g + geom_histogram(
        data = data.frame(m = means),
        aes(x = m, y = after_stat(density)),
        binwidth = (xlim[2] - xlim[1]) / 50,
        fill = PAL$samp_pt, colour = "white",
        linewidth = 0.2, alpha = 0.7
      )
    }
    g + geom_line(data = th, aes(x, y),
                  colour = PAL$theory, linewidth = 1.1, linetype = "dashed") +
      geom_vline(xintercept = mu, colour = PAL$pop_dark,
                 linetype = "dashed", linewidth = 0.4) +
      annotate("label", x = mu, y = max(th$y) * 1.07,
                     label = sprintf("μ = %g", mu), colour = PAL$pop_dark, fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95, size = 4) +
      scale_x_continuous(limits = xlim) +
      labs(x = "Sample mean (x̄)", y = "Density",
           subtitle = sprintf(
             "Population SD = %.2f. Theoretical SE (σ/√n) = %.2f.",
             sigma, se)) +
      base_theme()
  }, res = 96)

  output$step4_counter <- renderUI({
    n_draws <- length(walk$many_means)
    div(class = "p-2 my-2", style = sprintf("color:%s; font-size:14px;", PAL$pop_dark),
        sprintf("Hypothetical studies drawn: %d", n_draws))
  })

  # ============================================================================
  # STEP 5, live equation, history plot, live calc
  # ============================================================================
  step5_se <- reactive({
    sigma <- input$pop_sigma %||% 1
    n     <- input$pop_n     %||% 11
    s     <- if (length(walk$last_sample) > 0) sd(walk$last_sample) else NA_real_
    list(sigma = sigma, n = n, s = s,
         se_pop = sigma / sqrt(n),
         se_samp = if (!is.na(s)) s / sqrt(n) else NA_real_)
  })

  output$step5_live_eq <- renderUI({
    r <- step5_se()
    s_for_eq <- if (!is.na(r$s)) r$s else r$sigma  # fall back to σ before sample
    se       <- s_for_eq / sqrt(r$n)
    label_s  <- if (!is.na(r$s)) "s" else "\\sigma"
    fmt <- function(v) formatC(round(v, 2), format = "f", digits = 2)
    fmt3 <- function(v) formatC(round(v, 3), format = "f", digits = 3)
    eq <- paste0(
      "\\text{SE} \\;=\\; \\frac{", label_s, "}{\\sqrt{n}} \\;=\\; ",
      "\\frac{", fmt(s_for_eq), "}{\\sqrt{", r$n, "}} \\;=\\; ",
      fmt3(se)
    )
    withMathJax(
      tagList(
        div(style = "font-size: 22px; text-align:center;",
            HTML(paste0("$$", eq, "$$"))),
        if (is.na(r$s))
          div(style = "color:#888; font-size:11px; text-align:center; margin-top:8px;",
              "Showing σ in the numerator until you've drawn a sample. Run a study in Step 2 to use s.")
        else
          div(style = "color:#0F6E56; font-size:11px; text-align:center; margin-top:8px;",
              "Using s from your most recent sample.")
      )
    )
  })

  output$step5_history_plot <- renderPlot({
    hist <- walk$se_history
    mu   <- input$pop_mu %||% 6
    if (length(hist) == 0) {
      return(
        ggplot() +
          annotate("label", x = 0, y = 0,
                   label = "Move a slider to start the history",
                   colour = "#666", fill = "white", label.size = NA, size = 4.5) +
          theme_void()
      )
    }
    sigma_max <- max(sapply(hist, function(z) z$sigma))
    xlim <- c(mu - 4 * sigma_max, mu + 4 * sigma_max)
    n_h  <- length(hist)

    rows <- do.call(rbind, lapply(seq_along(hist), function(i) {
      h    <- hist[[i]]
      age  <- n_h - i
      x    <- seq(xlim[1], xlim[2], length.out = 250)
      data.frame(
        x = x, y = dnorm(x, mu, h$se),
        idx = i, is_current = (age == 0),
        alpha_v = if (age == 0) 1 else max(0.20, 0.7 - age * 0.10),
        label_se = sprintf("σ=%g, n=%d ⇒ SE=%.2f", h$sigma, h$n, h$se)
      )
    }))
    current_label <- rows$label_se[rows$is_current][1]

    ggplot(rows, aes(x = x, y = y, group = idx)) +
      geom_line(data = subset(rows, !is_current),
                aes(alpha = alpha_v), colour = PAL$pop_med, linewidth = 0.9) +
      geom_line(data = subset(rows,  is_current),
                colour = "black", linewidth = 1.3) +
      scale_alpha_identity() +
      geom_vline(xintercept = mu, linetype = "dashed", colour = "#888",
                 linewidth = 0.4) +
      annotate("label", x = mu, y = max(rows$y) * 1.05, label = current_label, fill = "white", colour = "black", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95, size = 4) +
      scale_x_continuous(limits = xlim) +
      labs(x = "Possible sample mean (x̄)", y = "Density") +
      base_theme()
  }, res = 96)

  output$step5_live_calc <- renderUI({
    r <- step5_se()
    primary_card <- if (!is.na(r$s))
      fluidRow(
        column(3, stat_card("s (sample SD)", sprintf("%.2f", r$s))),
        column(3, stat_card("n",             r$n)),
        column(3, stat_card("√n",            sprintf("%.3f", sqrt(r$n)))),
        column(3, stat_card("SE = s/√n",     sprintf("%.3f", r$se_samp), key = TRUE))
      )
    else
      div(style = "color:#888;",
          "Using σ until you draw a sample. ",
          fluidRow(
            column(3, stat_card("σ (population SD)", sprintf("%.2f", r$sigma))),
            column(3, stat_card("n",                 r$n)),
            column(3, stat_card("√n",                sprintf("%.3f", sqrt(r$n)))),
            column(3, stat_card("SE = σ/√n",         sprintf("%.3f", r$se_pop), key = TRUE))
          )
      )
    primary_card
  })

  # ============================================================================
  # STEP 6, t-statistic (must show t marker on first render)
  # ============================================================================
  step6_results <- reactive({
    s   <- walk$last_sample
    mu0 <- input$mu0 %||% 4
    if (length(s) == 0) {
      # Fallback: pretend we have a sample at the current population to keep
      # the figure visible even before the first manual run.
      mu    <- input$pop_mu    %||% 6
      sigma <- input$pop_sigma %||% 1
      n     <- input$pop_n     %||% 11
      list(have_sample = FALSE,
           mu0 = mu0, n = n, xbar = mu, s = sigma, se = sigma / sqrt(n),
           t = (mu - mu0) / (sigma / sqrt(n)), df = n - 1)
    } else {
      n     <- length(s); xbar <- mean(s); s_val <- sd(s)
      se    <- s_val / sqrt(n)
      list(have_sample = TRUE,
           n = n, xbar = xbar, s = s_val, se = se, mu0 = mu0,
           t = (xbar - mu0) / se, df = n - 1)
    }
  })

  output$step6_calc <- renderUI({
    r <- step6_results()
    note <- if (r$have_sample) NULL
            else div(style = "color:#888; font-size:12px; margin-bottom:8px;",
                     "Showing population values as placeholders ",
                     "(x̄ = μ, s = σ). Run a study in Step 2 to compute t from real data.")
    div(class = "p-3 my-2",
        style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
        note,
        fluidRow(
          column(2, stat_card("x̄",         sprintf("%.2f", r$xbar))),
          column(2, stat_card("μ₀",        sprintf("%.2f", r$mu0))),
          column(2, stat_card("s",         sprintf("%.2f", r$s))),
          column(2, stat_card("n",         r$n)),
          column(2, stat_card("SE = s/√n", sprintf("%.3f", r$se))),
          column(2, stat_card("t = (x̄−μ₀)/SE", sprintf("%.3f", r$t), key = TRUE))
        )
    )
  })

  output$step6_plot <- renderPlot({
    r <- step6_results()
    df_v <- r$df
    xr   <- t_xrange(r$t)
    x    <- seq(xr[1], xr[2], length.out = 400)
    cd   <- data.frame(x = x, y = dt(x, df_v))

    g <- ggplot(cd, aes(x, y)) +
      geom_line(colour = "black", linewidth = 0.7) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
      # μ₀ corresponds to t = 0
      geom_vline(xintercept = 0, colour = PAL$pop_dark,
                 linetype = "dashed", linewidth = 0.5) +
      annotate("label", x = 0, y = max(cd$y) * 1.10,
                     label = "Null: t = 0\n(center of the sampling distribution)", colour = PAL$pop_dark, fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95,
                 size = 3.6, lineheight = 0.9) +
      # Observed t
      geom_vline(xintercept = r$t, colour = PAL$samp_line, linewidth = 1) +
      annotate("point", x = r$t, y = 0, colour = PAL$samp_line, size = 3) +
      annotate("label", x = r$t, y = max(cd$y) * 0.7,
                     label = sprintf("Your t = %.2f\n(signal ÷ noise)", r$t), colour = PAL$samp_line, fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95,
                 size = 4.3, fontface = "bold", lineheight = 0.9)

    # Distance arrow / annotation between 0 and t
    if (abs(r$t) > 0.05) {
      mid_x <- r$t / 2
      g <- g +
        annotate("segment", x = 0, xend = r$t,
                 y = max(cd$y) * 0.45, yend = max(cd$y) * 0.45,
                 colour = PAL$samp_line, linewidth = 0.6,
                 arrow = arrow(ends = "both", length = unit(0.12, "inches"))) +
        annotate("label", x = mid_x, y = max(cd$y) * 0.45,
                       label = sprintf("|t| = %.2f SE\n(signal-to-noise)", abs(r$t)), colour = PAL$samp_line, fill = "white", label.size = NA,
                   label.r = unit(0.15, "lines"), alpha = 0.95, size = 3.6, lineheight = 0.9)
    }

    g + scale_x_continuous(limits = xr, breaks = t_xbreaks(xr)) +
      labs(x = "t-statistic = signal (x̄ − μ₀) ÷ noise (s/√n), located on the null sampling distribution",
           y = "Density",
           subtitle = sprintf(
             "Sampling distribution of t under H₀: df = n − 1 = %d%s",
             df_v,
             if (!r$have_sample) "  (placeholder, run a study to use real data)"
             else "")) +
      base_theme()
  }, res = 96)

  # ============================================================================
  # STEP 7, alpha plot
  # ============================================================================
  output$step7_plot <- renderPlot({
    alpha <- as.numeric(input$alpha_choice %||% 0.05)
    tail  <- input$tail_choice %||% "two"
    df_v  <- if (length(walk$last_sample) > 0) length(walk$last_sample) - 1 else 24

    cv_lo <- NA_real_; cv_hi <- NA_real_
    if (tail == "two") { cv_hi <- qt(1 - alpha / 2, df_v); cv_lo <- -cv_hi
    } else if (tail == "right") { cv_hi <- qt(1 - alpha, df_v)
    } else { cv_lo <- qt(alpha, df_v) }

    xr <- t_xrange(cv_lo, cv_hi)
    x  <- seq(xr[1], xr[2], length.out = 600)
    cd <- data.frame(x = x, y = dt(x, df_v))

    region_df <- function(a, b) {
      if (is.na(a) || is.na(b) || a >= b) return(NULL)
      xs <- seq(a, b, length.out = 200)
      data.frame(x = xs, y = dt(xs, df_v))
    }
    regs <- switch(tail,
      two   = list(region_df(xr[1], cv_lo), region_df(cv_hi, xr[2])),
      right = list(region_df(cv_hi, xr[2])),
      left  = list(region_df(xr[1], cv_lo))
    )

    g <- ggplot()
    for (r in regs) if (!is.null(r) && nrow(r) > 1)
      g <- g + geom_area(data = r, aes(x = x, y = y),
                         fill = PAL$reject, alpha = 0.7)

    g + geom_line(data = cd, aes(x, y), colour = "black", linewidth = 0.7) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
      annotate("label", x = 0, y = max(cd$y) * 0.5,
                     label = sprintf("white area = 1 − α = %.2f\n(plausible signal-to-noise\nunder H₀)", 1 - alpha), colour = "#444", fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95, size = 3.6, lineheight = 0.95) +
      scale_x_continuous(limits = xr, breaks = t_xbreaks(xr)) +
      labs(x = "t-statistic (signal-to-noise ratio under H₀)", y = "Density",
           subtitle = sprintf(
             "Sampling distribution of t under H₀ (df = %d). Orange tail area = α = %.2f, the most extreme signal-to-noise outcomes.",
             df_v, alpha)) +
      base_theme()
  }, res = 96)

  # ============================================================================
  # STEP 8, df history plot
  # ============================================================================
  output$step8_plot <- renderPlot({
    hist <- walk$df_history
    if (length(hist) == 0) return(NULL)
    n_h  <- length(hist)

    # Adaptive x-range covering every CV across the df history
    all_cvs <- vapply(hist, function(h) qt(1 - h$alpha / 2, h$df), numeric(1))
    xr <- t_xrange(all_cvs)
    x  <- seq(xr[1], xr[2], length.out = 500)

    rows <- do.call(rbind, lapply(seq_along(hist), function(i) {
      h    <- hist[[i]]
      age  <- n_h - i
      data.frame(
        x = x, y = dt(x, h$df), idx = i,
        is_current = (age == 0),
        alpha_v = if (age == 0) 1 else max(0.20, 0.7 - age * 0.10),
        df_lab = h$df
      )
    }))

    current <- hist[[n_h]]
    cv_hi <- qt(1 - current$alpha / 2, current$df)
    cv_lo <- -cv_hi
    cd_curr <- subset(rows, is_current)
    reg_lo <- subset(cd_curr, x <= cv_lo)
    reg_hi <- subset(cd_curr, x >= cv_hi)

    g <- ggplot() +
      geom_line(data = subset(rows, !is_current),
                aes(x = x, y = y, group = idx, alpha = alpha_v),
                colour = PAL$pop_med, linewidth = 0.9) +
      scale_alpha_identity()

    if (nrow(reg_lo) > 1)
      g <- g + geom_area(data = reg_lo, aes(x, y),
                         fill = PAL$reject, alpha = 0.65)
    if (nrow(reg_hi) > 1)
      g <- g + geom_area(data = reg_hi, aes(x, y),
                         fill = PAL$reject, alpha = 0.65)

    label_offset <- diff(xr) * 0.02
    g + geom_line(data = cd_curr, aes(x, y),
                  colour = "black", linewidth = 1.2) +
      geom_vline(xintercept = c(cv_lo, cv_hi),
                 colour = PAL$reject_dk, linetype = "dashed", linewidth = 0.55) +
      annotate("label", x = cv_hi + label_offset, y = max(cd_curr$y) * 0.95,
                     label = sprintf("CV = %.2f\n(critical signal:noise)", cv_hi), colour = PAL$reject_dk, fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95,
                 size = 3.8, fontface = "bold", hjust = 0, lineheight = 0.9) +
      annotate("label", x = cv_lo - label_offset, y = max(cd_curr$y) * 0.95,
                     label = sprintf("CV = %.2f\n(critical signal:noise)", cv_lo), colour = PAL$reject_dk, fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95,
                 size = 3.8, fontface = "bold", hjust = 1, lineheight = 0.9) +
      scale_x_continuous(limits = xr, breaks = t_xbreaks(xr)) +
      labs(x = "t-statistic (signal-to-noise ratio) on the sampling distribution under H₀",
           y = "Density",
           subtitle = sprintf(
             "Current df = %d, α = %.2f, CVs mark the signal-to-noise cutoff on the sampling distribution.",
             current$df, current$alpha)) +
      base_theme()
  }, res = 96)

  output$step8_cv_summary <- renderUI({
    dfv   <- input$cv_df %||% 10
    alpha <- as.numeric(input$cv_alpha %||% 0.05)
    cv    <- qt(1 - alpha / 2, dfv)
    div(class = "p-3 my-2",
        style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
        fluidRow(
          column(4, stat_card("df", dfv)),
          column(4, stat_card("α",  sprintf("%.2f", alpha))),
          column(4, stat_card("Two-tailed CV", sprintf("±%.3f", cv), key = TRUE))
        )
    )
  })

  # ============================================================================
  # STEP 9, synthesis (uses sync'd inputs, displays full decision)
  # ============================================================================
  step9_calc <- reactive({
    r     <- step6_results()
    alpha <- as.numeric(input$alpha_choice %||% 0.05)
    tail  <- input$tail_choice %||% "two"
    df_v  <- r$df; t_obs <- r$t
    cv_lo <- NA_real_; cv_hi <- NA_real_
    if (tail == "two") { cv_hi <- qt(1 - alpha / 2, df_v); cv_lo <- -cv_hi
    } else if (tail == "right") { cv_hi <- qt(1 - alpha, df_v)
    } else { cv_lo <- qt(alpha, df_v) }

    p_val <- switch(tail,
      two   = 2 * (1 - pt(abs(t_obs), df_v)),
      right = 1 - pt(t_obs, df_v),
      left  = pt(t_obs, df_v)
    )
    p_val <- max(0, min(1, p_val))

    reject <- switch(tail,
      two   = abs(t_obs) >= cv_hi,
      right = t_obs >= cv_hi,
      left  = t_obs <= cv_lo
    )

    list(have = r$have_sample, t = t_obs, df = df_v, alpha = alpha, tail = tail,
         cv_lo = cv_lo, cv_hi = cv_hi, p = p_val, reject = reject,
         xbar = r$xbar, mu0 = r$mu0, s = r$s, n = r$n)
  })

  output$step9_summary <- renderUI({
    z <- step9_calc()
    cv_text <- if (z$tail == "two") sprintf("±%.3f", z$cv_hi)
               else if (z$tail == "right") sprintf("%.3f", z$cv_hi)
               else sprintf("%.3f", z$cv_lo)
    bg <- if (z$reject) PAL$ok_bg else PAL$warn_bg
    fg <- if (z$reject) PAL$ok_fg else PAL$warn_fg
    msg <- if (z$reject) "Reject H₀" else "Fail to reject H₀"
    msg_long <- if (z$reject)
        sprintf("Signal-to-noise |t| = %.2f ≥ critical signal-to-noise CV = %s, your sample sits past the cutoff in the tail of the null sampling distribution.",
                abs(z$t), cv_text)
      else
        sprintf("Signal-to-noise |t| = %.2f < critical signal-to-noise CV = %s, your sample sits inside the bulk of the null sampling distribution.",
                abs(z$t), cv_text)

    fallback <- if (!z$have)
      div(style = "color:#888; font-size:12px; margin-bottom:8px;",
          "Showing placeholder values (x̄ = μ, s = σ) until you click ",
          tags$b("\"Run a fresh study\""), " above.")
    else NULL

    div(class = "p-3 my-2",
        style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
        fallback,
        fluidRow(
          column(3, stat_card("Your t",         sprintf("%.3f", z$t))),
          column(3, stat_card("Critical value", cv_text)),
          column(3, stat_card("p-value",
                              if (z$p < 0.001) "< 0.001" else sprintf("%.3f", z$p))),
          column(3, stat_card("α",              sprintf("%.2f", z$alpha)))
        ),
        div(class = "p-3 mt-3", style = sprintf(
              "background:%s; color:%s; border-radius:8px; font-weight:600; font-size:18px;",
              bg, fg),
            msg,
            tags$br(),
            tags$span(style = "font-weight:400; font-size:14px;", msg_long))
    )
  })

  output$step9_plot <- renderPlot({
    z <- step9_calc()
    df_v <- z$df
    xr <- t_xrange(z$t, z$cv_lo, z$cv_hi)
    x  <- seq(xr[1], xr[2], length.out = 600)
    cd <- data.frame(x = x, y = dt(x, df_v))
    region_df <- function(a, b) {
      if (is.na(a) || is.na(b) || a >= b) return(NULL)
      xs <- seq(a, b, length.out = 200)
      data.frame(x = xs, y = dt(xs, df_v))
    }

    alpha_regs <- switch(z$tail,
      two   = list(region_df(xr[1], z$cv_lo), region_df(z$cv_hi, xr[2])),
      right = list(region_df(z$cv_hi, xr[2])),
      left  = list(region_df(xr[1], z$cv_lo))
    )
    p_regs <- switch(z$tail,
      two   = list(region_df(xr[1], -abs(z$t)), region_df(abs(z$t), xr[2])),
      right = list(region_df(z$t, xr[2])),
      left  = list(region_df(xr[1], z$t))
    )

    g <- ggplot()
    for (r in alpha_regs) if (!is.null(r) && nrow(r) > 1)
      g <- g + geom_area(data = r, aes(x, y), fill = PAL$reject, alpha = 0.55)
    for (r in p_regs) if (!is.null(r) && nrow(r) > 1)
      g <- g + geom_area(data = r, aes(x, y), fill = PAL$pval, alpha = 0.7)

    label_offset <- diff(xr) * 0.02
    g <- g +
      geom_line(data = cd, aes(x, y), colour = "black", linewidth = 0.7) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
      geom_vline(xintercept = z$t, colour = PAL$obs, linewidth = 1.1) +
      annotate("point", x = z$t, y = 0, colour = PAL$obs, size = 3) +
      annotate("label", x = z$t, y = max(cd$y) * 1.08,
                     label = sprintf("Your t = %.2f\n(signal ÷ noise)", z$t), colour = PAL$obs, fill = "white", label.size = NA,
                 label.r = unit(0.15, "lines"), alpha = 0.95,
                 size = 4.2, fontface = "bold", lineheight = 0.9)
    if (!is.na(z$cv_lo))
      g <- g +
        geom_vline(xintercept = z$cv_lo, colour = PAL$reject_dk,
                   linetype = "dashed", linewidth = 0.5) +
        annotate("label", x = z$cv_lo - label_offset, y = max(cd$y) * 0.95,
                       label = sprintf("CV = %.2f\n(critical signal:noise)", z$cv_lo), colour = PAL$reject_dk, fill = "white", label.size = NA,
                   label.r = unit(0.15, "lines"), alpha = 0.95,
                   size = 3.6, hjust = 1, lineheight = 0.9)
    if (!is.na(z$cv_hi))
      g <- g +
        geom_vline(xintercept = z$cv_hi, colour = PAL$reject_dk,
                   linetype = "dashed", linewidth = 0.5) +
        annotate("label", x = z$cv_hi + label_offset, y = max(cd$y) * 0.95,
                       label = sprintf("CV = %.2f\n(critical signal:noise)", z$cv_hi), colour = PAL$reject_dk, fill = "white", label.size = NA,
                   label.r = unit(0.15, "lines"), alpha = 0.95,
                   size = 3.6, hjust = 0, lineheight = 0.9)

    g + scale_x_continuous(limits = xr, breaks = t_xbreaks(xr)) +
      labs(x = "t-statistic = signal ÷ noise, projected onto the null sampling distribution",
           y = "Density",
           subtitle = sprintf(
             "df = %d  |  α = %.2f  |  p-value = %s  |  decision: %s",
             z$df, z$alpha,
             if (z$p < 0.001) "< 0.001" else sprintf("%.3f", z$p),
             if (z$reject) "reject H₀" else "fail to reject H₀")) +
      base_theme()
  }, res = 96)

  # ============================================================================
  # TAB 2, Paired-samples t-test walkthrough (seagulls)
  # ============================================================================
  source_paired_server(input, output, session)

  # ============================================================================
  # TAB 3, Independent-samples t-test walkthrough (Daves)
  # ============================================================================
  source_indep_server(input, output, session)

  # ============================================================================
  # TAB 4, One-Way ANOVA walkthrough (regional neuroticism)
  # ============================================================================
  source_anova_server(input, output, session)

  # ============================================================================
  # TAB 5, Factorial ANOVA walkthrough (medication dosing)
  # ============================================================================
  source_factorial_server(input, output, session)
}

shinyApp(ui, server)
