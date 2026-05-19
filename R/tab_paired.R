# =============================================================================
# tab_paired.R
# Paired-samples t-test walkthrough. Staring down seagulls.
# Goumas, Burns, Kelley & Boogert (2019), Biology Letters.
# https://notawfulandboring.blogspot.com/2019/08/seagull-thievery-deterrent-research.html
# =============================================================================

# ---- Case-study constants (a six-seagull subset, made-up but in spirit) -----
# The published paper observed n = 19 seagulls in a two-condition design and
# reported t(18) = 3.13, p = .006, d = 0.717. The numbers below are a
# pedagogically convenient subsample that preserves the direction and produces
# a similar reject-H0 conclusion. Approach times are in seconds: the time it
# takes a seagull to walk toward an open chip bag. Longer times in the "stare"
# condition mean the seagull is more wary.
SEAGULL <- local({
  before <- c(80, 68, 92, 75, 70, 88)   # look-away condition (seagull approaches faster)
  after  <- c(70, 63, 74, 63, 63, 73)   # stare condition (seagull stays away longer)
  D      <- before - after              # delay caused by being stared at
  n      <- length(D)
  M_D    <- mean(D)
  SS_D   <- sum((D - M_D)^2)
  s_D    <- sqrt(SS_D / (n - 1))
  se     <- s_D / sqrt(n)
  t_obs  <- M_D / se
  df_v   <- n - 1
  p_val  <- 2 * (1 - pt(abs(t_obs), df_v))
  list(participants = c("A", "B", "C", "D", "E", "F"),
       before = before, after = after, D = D, n = n,
       M_D = M_D, SS_D = SS_D, s_D = s_D, se = se,
       t = t_obs, df = df_v, p = p_val,
       M_before = mean(before), SD_before = sd(before),
       M_after  = mean(after),  SD_after  = sd(after))
})
# Backwards-compatibility alias so existing references in this file keep working.
LAB6 <- SEAGULL

# ---- Quest dataset: NICU naming convention (Adelman et al., 2015) ----------
# Ten NICUs, each contributing a before- and after-intervention error count.
# Real numbers from the case-study post.
NICU <- local({
  before <- c(47, 45, 52, 50, 46, 38, 63, 40, 37, 40)
  after  <- c(36, 26, 38, 32, 42, 20, 41, 27, 26, 29)
  D      <- before - after
  n      <- length(D)
  M_D    <- mean(D)
  SS_D   <- sum((D - M_D)^2)
  s_D    <- sqrt(SS_D / (n - 1))
  se     <- s_D / sqrt(n)
  t_obs  <- M_D / se
  df_v   <- n - 1
  p_val  <- 2 * (1 - pt(abs(t_obs), df_v))
  list(before = before, after = after, D = D, n = n,
       M_D = M_D, SS_D = SS_D, s_D = s_D, se = se,
       t = t_obs, df = df_v, p = p_val,
       M_before = mean(before), SD_before = sd(before),
       M_after = mean(after), SD_after = sd(after))
})

# ---- Plot helpers (paired-tab-specific) ------------------------------------

# Connected dot plot: each subject is a line from "Before" to "After"
plot_paired_pairs <- function(before, after, ids) {
  long <- data.frame(
    id    = factor(rep(ids, 2), levels = ids),
    cond  = factor(rep(c("Before", "After"), each = length(ids)),
                   levels = c("Before", "After")),
    score = c(before, after)
  )
  M_b <- mean(before); M_a <- mean(after)
  ggplot(long, aes(x = cond, y = score, group = id)) +
    geom_line(colour = "grey60", alpha = 0.7, linewidth = 0.6) +
    geom_point(aes(colour = cond), size = 3.2, alpha = 0.9) +
    scale_colour_manual(values = c(Before = PAL$pop_dark, After = PAL$samp_pt)) +
    annotate("segment", x = 0.78, xend = 1.22, y = M_b, yend = M_b,
             colour = PAL$pop_dark, linewidth = 1.4) +
    annotate("segment", x = 1.78, xend = 2.22, y = M_a, yend = M_a,
             colour = PAL$samp_pt, linewidth = 1.4) +
    annotate("label", x = 1, y = M_b,
             label = sprintf("M_before = %.2f", M_b),
             colour = PAL$pop_dark, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4, fontface = "bold", vjust = -0.6) +
    annotate("label", x = 2, y = M_a,
             label = sprintf("M_after = %.2f", M_a),
             colour = PAL$samp_pt, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4, fontface = "bold", vjust = -0.6) +
    labs(x = NULL, y = "Approach time (seconds)",
         subtitle = "Each grey line is one subject. Coloured horizontal bars are condition means.") +
    base_theme() +
    theme(legend.position = "none")
}

# Difference-scores dot plot with M_D and H₀ marked
plot_diff_scores <- function(D, M_D, ids) {
  df <- data.frame(id = ids, D = D)
  rng <- range(c(D, 0, M_D))
  pad <- diff(rng) * 0.2 + 1
  ggplot(df, aes(x = D, y = 0)) +
    geom_vline(xintercept = 0,  colour = PAL$pop_dark,
               linetype = "dashed", linewidth = 0.6) +
    geom_vline(xintercept = M_D, colour = PAL$samp_line,
               linewidth = 1.1) +
    geom_jitter(width = 0, height = 0.1, colour = PAL$samp_pt,
                size = 3.6, alpha = 0.85) +
    geom_text(aes(label = id), nudge_y = 0.18, size = 3.4,
              colour = PAL$samp_line) +
    annotate("label", x = 0, y = 0.5,
             label = "H₀ says μ_D = 0\n(no average change)",
             colour = PAL$pop_dark, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 3.6, lineheight = 0.9, vjust = 1) +
    annotate("label", x = M_D, y = -0.45,
             label = sprintf("M_D = %.2f\n(our sample's average change)", M_D),
             colour = PAL$samp_line, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4, fontface = "bold", lineheight = 0.9, vjust = 1) +
    scale_x_continuous(limits = c(rng[1] - pad, rng[2] + pad)) +
    scale_y_continuous(limits = c(-1, 1), breaks = NULL) +
    labs(x = "Difference score D = before − after", y = NULL,
         subtitle = "Each green dot is one subject's D-score. The black line is the sample mean of the D-scores.") +
    base_theme() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# Sampling distribution of M_D under H₀ vs observed M_D
plot_sampling_dist_MD <- function(M_D, se, df_v, mu_D_null = 0) {
  # plot the t-distribution rescaled to M_D-units centered at mu_D_null
  xr   <- c(mu_D_null - 4 * se, mu_D_null + 4 * se)
  if (M_D < xr[1] || M_D > xr[2]) {
    xr <- range(c(xr, M_D + c(-0.5, 0.5) * se))
  }
  x  <- seq(xr[1], xr[2], length.out = 400)
  y  <- dt((x - mu_D_null) / se, df_v) / se
  cd <- data.frame(x = x, y = y)
  max_y <- max(cd$y)
  ggplot(cd, aes(x, y)) +
    geom_area(fill = PAL$pop_med, alpha = 0.2) +
    geom_line(colour = PAL$pop_dark, linewidth = 0.7) +
    geom_vline(xintercept = mu_D_null, colour = PAL$pop_dark,
               linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = M_D, colour = PAL$samp_line, linewidth = 1.1) +
    annotate("label", x = mu_D_null, y = max_y * 1.1,
             label = "Center of sampling distribution\nunder H₀: μ_D = 0",
             colour = PAL$pop_dark, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 3.6, lineheight = 0.9) +
    annotate("label", x = M_D, y = max_y * 0.6,
             label = sprintf("Our M_D = %.2f", M_D),
             colour = PAL$samp_line, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4.2, fontface = "bold") +
    labs(x = "Possible sample mean of difference scores (M_D)",
         y = "Density",
         subtitle = sprintf("Sampling distribution under H₀: t with df = %d, scaled so SD = SE = %.3f.",
                            df_v, se)) +
    base_theme() +
    scale_y_continuous(limits = c(0, max_y * 1.25))
}

# ---- Server entry point -----------------------------------------------------
source_paired_server <- function(input, output, session) {

  output$paired_tab_ui <- renderUI({
    withMathJax(div(class = "container-fluid", style = "max-width: 1240px; padding-top: 16px;",

      scenario_card(
        "Build a paired-samples t-test, from the ground up",
        lab_label = "Running example: staring down seagulls",
        p(style = "margin-bottom:6px;",
          "The paired t-test is what we reach for when the ", tags$b("same subjects"),
          " are measured twice. The classic case is a before/after design, ",
          "but matched pairs (twins, dyads, left-vs-right hand) also qualify. ",
          "What unites them is that every measurement in the first condition ",
          "can be paired one-to-one with a measurement in the second, so ",
          "each subject acts as their own control."),
        p(style = "margin-bottom:6px;",
          tags$b("Running example. "),
          "Goumas and colleagues (2019), in a paper published in Biology ",
          "Letters, asked whether seagulls back off if a human stares at ",
          "them while they are angling for a chip bag. Each seagull was ",
          "observed twice: once with the human looking away, and once with ",
          "the human staring it down. The published paper reports ",
          math_inline("t(18) = 3.13"), ", p = .006, d = 0.717, with the ",
          "stare condition producing slower approach times. The numbers ",
          "throughout this tab use a six-seagull subset of the same data ",
          "to keep the arithmetic small enough to do by hand."),
        p(style = "margin-bottom:0;",
          tags$b("Worth holding onto across every step. "),
          "A paired-samples t-test ", tags$em("is"),
          " a single-sample t-test in disguise. Once we compute the ",
          "difference scores for each seagull, the rest of the procedure ",
          "is exactly the procedure from Tab 1, with ", math_inline("M_D"),
          " standing in for ", math_inline("M"), " and 0 standing in for ",
          math_inline("\\mu_0"),
          ". You already know how to do this. You are just calling the ",
          "ingredients by different names.")
      ),

      # ------ STEP 1 -------------------------------------------------------
      step_container(
        1, "The null world: \\(H_0: \\mu_D = 0\\)",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "Let ", math_inline("D_i = X_i - Y_i"),
            " denote the within-subject difference for subject ",
            math_inline("i"),
            ". The population of all such difference scores has mean ",
            math_inline("\\mu_D"),
            " and SD ", math_inline("\\sigma_D"),
            ". The null hypothesis specifies that, in this population, the ",
            "average within-subject change is exactly zero (",
            math_inline("H_0: \\mu_D = 0"),
            "). The corresponding two-tailed alternative is ",
            math_inline("H_1: \\mu_D \\neq 0"), "."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Picture a world where the treatment does nothing at all. ",
            "Different people would still change in different directions ",
            "between the before and after measurements (some up, some ",
            "down, some unchanged), so individual differences would not be ",
            "exactly zero. But the ", tags$em("average"),
            " change across the whole population would settle at zero. ",
            "That is the world H₀ describes. The job of the test is to ",
            "decide whether our sample's average change can comfortably fit ",
            "inside it, or whether it is too large to plausibly belong there."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "H₀ in the seagull study says that, in the seagull population, ",
            "being stared at does nothing on average to a seagull's ",
            "approach time. H₁ says the average within-bird change in the ",
            "population is non-zero. Notice that the test gives us only ",
            "two possible verdicts. We can reject H₀ or fail to reject it. ",
            "There is no third option that lets us declare H₀ to be true."
          )
        ),
        math_block("H_0:\\ \\mu_D = 0 \\qquad H_1:\\ \\mu_D \\neq 0"),
        div(class = "p-3 my-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            h5("The case-study numbers for this step", style = "margin-top:0;"),
            p(style = "color:#666; font-size:13px;",
              "Step 1 names the population the test reasons about. ",
              "In the paired design, that population is a population of ",
              "difference scores (look-away minus stare, in seconds). ",
              "Under H₀, the average difference is zero."),
            fluidRow(
              column(4, stat_card("μ_D (population mean change)",
                                  "0 under H₀")),
              column(4, stat_card("σ_D (population SD of changes)",
                                  "unknown; estimated as s_D ≈ 4.87")),
              column(4, stat_card("n (seagulls in subset)",
                                  LAB6$n, key = TRUE)))),
        callout_warm(
          tags$b("Worth pausing on. "),
          "The population the test cares about here is a population of ",
          tags$em("difference scores"), ", which is a different kind of object ",
          "from the population of raw before-treatment scores or the ",
          "population of raw after-treatment scores. By computing each ",
          "subject's within-person change, we have converted a two-condition ",
          "comparison into a one-sample comparison against the value zero. ",
          "That conversion is the conceptual move that makes the paired ",
          "t-test work, and it is also what lets us reuse everything we ",
          "already know about the single-sample t-test."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 2 -------------------------------------------------------
      step_container(
        2, "Run one study: collect pairs, compute \\(D_i\\) and \\(M_D\\)",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "For each of n participants, we record both measurements and ",
            "compute ", math_inline("D_i = X_i - Y_i"),
            ". From the n difference scores we then compute the sample mean ",
            math_inline("M_D = \\frac{1}{n} \\sum_{i=1}^{n} D_i"),
            " and the sum of squared deviations ",
            math_inline("\\text{SS}_D = \\sum (D_i - M_D)^2"),
            ", from which the sample standard deviation of the differences ",
            "follows as ",
            math_inline("s_D = \\sqrt{\\text{SS}_D/(n-1)}"), "."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Subtract each person's after-score from their before-score, ",
            "write that number next to their name, and then average those ",
            "n numbers. That single average is a one-number summary of how ",
            "much, on average, the people in your sample changed from one ",
            "measurement occasion to the next."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "Doing this for the six seagulls in our subset gives the ",
            "following differences (look-away minus stare, in seconds). ",
            "Seagull A: 80 − 70 = 10. Seagull B: 68 − 63 = 5. C: 92 − 74 = ",
            "18. D: 75 − 63 = 12. E: 70 − 63 = 7. F: 88 − 73 = 15. Averaging ",
            "the six values yields ",
            math_inline("M_D = (10+5+18+12+7+15)/6 \\approx 11.17"),
            " seconds. The sum of squared deviations from the mean is ",
            math_inline("\\text{SS}_D \\approx 118.83"),
            ", and the resulting sample SD of the differences is ",
            math_inline("s_D = \\sqrt{118.83/5} \\approx 4.87"),
            " seconds."
          )
        ),
        math_block("M_D = \\dfrac{D_1 + D_2 + \\cdots + D_n}{n}"),
        div(class = "row g-3 mt-2",
            column(7, plotOutput("paired_step2_pairs", height = "280px")),
            column(5, plotOutput("paired_step2_diffs", height = "280px"))),
        div(class = "p-3 mt-2",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("n", LAB6$n)),
              column(3, stat_card("M_before", sprintf("%.2f", LAB6$M_before))),
              column(3, stat_card("M_after",  sprintf("%.2f", LAB6$M_after))),
              column(3, stat_card("M_D",      sprintf("%.2f", LAB6$M_D), key = TRUE))
            ),
            fluidRow(
              column(3, stat_card("SS_D",     sprintf("%.0f", LAB6$SS_D))),
              column(3, stat_card("df = n−1", LAB6$df)),
              column(3, stat_card("s_D",      sprintf("%.3f", LAB6$s_D))),
              column(3, ""))
        ),
        callout_warm(
          tags$b("A common point of confusion worth heading off now. "),
          "The cluster of six green dots in the right-hand plot above shows ",
          "one specific kind of distribution: the ",
          tags$b("sample distribution"),
          " of D-scores in this study. Its spread is ",
          math_inline("s_D ≈ 4.87"),
          ". A different distribution will appear over the next few steps: ",
          "the ", tags$b("sampling distribution"), " of ",
          math_inline("M_D"),
          " under repeated sampling. These two distributions are easy to ",
          "conflate (especially because they share a name root), but they ",
          "describe different things, and most of the conceptual work of a ",
          "paired t-test lives in keeping them separate."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 3 -------------------------------------------------------
      step_container(
        3, "Run the study again. And again. (Sampling variability of \\(M_D\\))",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "Each independent replication of a study yields a different ",
            math_inline("M_D"),
            " because each replication draws a different sample of n ",
            "subjects. The spread of those ", math_inline("M_D"),
            " values across infinitely many imagined replications is ",
            "referred to as the ", tags$b("sampling variability"), " of ",
            math_inline("M_D"),
            ". This kind of variability is a property of the procedure ",
            "itself, in the sense that it describes how the procedure ",
            "would behave under repetition. An individual dataset does not ",
            "have a sampling variability on its own."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Picture six different research groups around the country, each ",
            "of them running the seagull study with their own six seagulls. ",
            "Each group would compute a slightly different ",
            math_inline("M_D"),
            ". They share the same protocol and the same target population, ",
            "and yet they end up with different numbers, because no two of ",
            "them happened to sample the same six people. That run-to-run ",
            "wiggle in ", math_inline("M_D"), " is sampling variability."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "We can build intuition about this by simulating what those ",
            "imagined re-runs would have looked like. The buttons below ",
            "draw fresh samples of six subjects from a population with the ",
            "parameters the seagull study estimated, compute each new ",
            math_inline("M_D"),
            ", and add it to the growing cloud of possible outcomes."
          )
        ),
        div(style = "display:flex; gap:8px; flex-wrap:wrap;",
            actionButton("paired_repeat_one",   "Run another study",  class = "btn-primary"),
            actionButton("paired_repeat_five",  "Run 5 more",         class = "btn-primary"),
            actionButton("paired_repeat_reset", "Reset",              class = "btn-outline-secondary")),
        plotOutput("paired_step3_plot", height = "280px"),
        uiOutput("paired_step3_counter"),
        callout_warm(
          tags$b("What to notice. "),
          "The dots cluster around the true value of ",
          math_inline("\\mu_D"),
          ", but no individual study lands exactly on it. The width of that ",
          "cluster has a name and a formula. Step 5 introduces both."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 4 -------------------------------------------------------
      step_container(
        4, "If we ran it many times: the sampling distribution of \\(M_D\\)",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "The ", tags$b("sampling distribution of mean difference scores"),
            " is the probability distribution of all possible values of ",
            math_inline("M_D"), " for a fixed sample size ", math_inline("n"),
            ". Under repeated sampling from a population with mean ",
            math_inline("\\mu_D"), " and SD ", math_inline("\\sigma_D"),
            ", the sampling distribution of ", math_inline("M_D"),
            " has mean ", math_inline("\\mu_D"),
            " and SD ", math_inline("\\sigma_D/\\sqrt{n}"),
            ". The Central Limit Theorem additionally guarantees that the ",
            "shape of the distribution approaches a normal curve as ",
            math_inline("n"),
            " grows, regardless of how the original difference scores ",
            "are distributed."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Imagine that, instead of six clinics replicating the seagull study, we had ",
            "500 of them. We could collect all 500 of their reported ",
            math_inline("M_D"),
            " values and pile them up in a histogram. The histogram we ",
            "would end up with is the sampling distribution of ",
            math_inline("M_D"),
            ". This is a different object from the histogram in Step 2. ",
            "The Step 2 histogram showed six dots: the six within-person ",
            "D-scores in one study. The histogram here would show 500 dots: ",
            "the 500 ", math_inline("M_D"),
            " values across imagined re-runs of the whole study."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "Under H₀, the sampling distribution of ",
            math_inline("M_D"),
            " is centered at zero (because H₀ says ",
            math_inline("\\mu_D = 0"),
            "), and its spread is set by ",
            math_inline("\\sigma_D/\\sqrt{n}"),
            ". We do not know the value of ", math_inline("\\sigma_D"),
            " exactly, but Step 2 gave us an estimate of it (",
            math_inline("s_D ≈ 4.87"),
            "). Step 5 turns that estimate into a standard error."
          )
        ),
        math_block("M_D \\;\\sim\\; \\mathcal{N}\\!\\left(\\mu_D,\\ \\dfrac{\\sigma_D}{\\sqrt{n}}\\right)\\ \\text{(by the CLT)}"),
        plotOutput("paired_step4_plot", height = "300px"),
        callout_warm(
          tags$b("The two distributions that get confused most. "),
          tags$br(),
          "• ", tags$b("Sample distribution: "),
          "the six D-scores in our one study. Its spread is ",
          math_inline("s_D"), ".", tags$br(),
          "• ", tags$b("Sampling distribution: "),
          "the (imagined) cloud of ", math_inline("M_D"),
          " values across many repeats of the study. Its spread is ",
          math_inline("s_D/\\sqrt{n}"),
          " (the standard error). ", tags$br(),
          "These are two genuinely different distributions, and most of the ",
          "intuition for hypothesis testing depends on keeping them apart. ",
          "Whenever a description in the rest of this tab refers to ",
          "\"the distribution,\" it is worth pausing for a moment and ",
          "asking yourself which of these two is being described."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 5 -------------------------------------------------------
      step_container(
        5, "How wide is the sampling distribution? Standard error of \\(M_D\\)",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "The standard error of ", math_inline("M_D"),
            ", typically written ", math_inline("s_{M_D}"),
            ", is the standard deviation of the sampling distribution of ",
            math_inline("M_D"),
            ". We estimate the standard error from our one sample's data ",
            "using the formula ",
            math_inline("s_{M_D} = s_D/\\sqrt{n}"), "."
          ),
          example = tagList(
            tags$b("In human words. "),
            "The two quantities ", math_inline("s_D"), " and ",
            math_inline("s_{M_D}"),
            " describe two different kinds of scatter, even though they ",
            "look similar on paper. ", math_inline("s_D"),
            " describes how the individual D-scores within one study spread ",
            "around their average, whereas ", math_inline("s_{M_D}"),
            " describes how the average D-score itself spreads across the ",
            "imagined re-runs of the whole study. The √n in the denominator ",
            "guarantees that ", math_inline("s_{M_D}"),
            " is always smaller than ", math_inline("s_D"),
            ", and that it shrinks as the sample size grows. Larger studies ",
            "produce more trustworthy estimates of the population average."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "For the seagull subset, ", math_inline("s_D \\approx 4.87"),
            " and ", math_inline("n = 6"), ", so ",
            math_inline("s_{M_D} = 4.87/\\sqrt{6} \\approx 1.99"),
            " seconds. The average within-bird change in approach time ",
            "across imagined re-runs of the study would typically wiggle ",
            "by about two seconds."
          )
        ),
        math_block("s_{M_D} \\;=\\; \\dfrac{s_D}{\\sqrt{n}}"),
        div(class = "p-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("s_D",  sprintf("%.3f", LAB6$s_D))),
              column(3, stat_card("n",    LAB6$n)),
              column(3, stat_card("√n",   sprintf("%.3f", sqrt(LAB6$n)))),
              column(3, stat_card("s_{M_D} = s_D/√n",
                                  sprintf("%.3f", LAB6$se), key = TRUE)))),
        callout_warm(
          tags$b("Why √n. "),
          "When n independent measurements are averaged, their random ",
          "ups and downs partially cancel each other out. The variance of ",
          "the resulting average works out to exactly ",
          math_inline("\\sigma_D^2/n"),
          ", and taking the square root of that variance gives the ",
          "standard error. One practical consequence: halving the standard ",
          "error requires quadrupling the sample size. Paired designs are ",
          "powerful relative to independent designs, but sample size still ",
          "carries weight."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 6 -------------------------------------------------------
      step_container(
        6, "Putting it together: the paired t-statistic",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "Under ", math_inline("H_0:\\ \\mu_D = 0"),
            ", the standardized distance between the observed ",
            math_inline("M_D"),
            " and the null value of zero, measured in units of estimated ",
            "standard error, follows a t-distribution with ",
            math_inline("df = n - 1"),
            ". The t-statistic for the paired-samples test is therefore the ",
            "observed ", math_inline("M_D"),
            " divided by its estimated standard error from Step 5."
          ),
          example = tagList(
            tags$b("In human words. "),
            "The t-statistic is signal divided by noise. The numerator (",
            math_inline("M_D"),
            ") is the signal portion, which is the average within-person ",
            "change in our sample. The denominator (",
            math_inline("s_D/\\sqrt{n}"),
            ") is the noise portion, which is the amount that ",
            math_inline("M_D"),
            " ordinarily wiggles just by chance from one study to the next. ",
            "A large t-statistic means that the signal in our sample is ",
            "large relative to the routine wiggle. A small t-statistic ",
            "means the signal is small enough that the routine wiggle could ",
            "plausibly have produced it on its own."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "Plugging the seagull study's numbers into the formula gives ",
            math_inline("t = M_D / s_{M_D} = 11.17 / 1.99 \\approx 5.61"),
            ", with ", math_inline("df = n - 1 = 5"),
            ". The observed average within-person change sits 5.61 ",
            "standard errors away from zero. We are about to evaluate ",
            "whether a value that far from zero is plausible under H₀."
          )
        ),
        math_block("t \\;=\\; \\dfrac{M_D - 0}{s_D/\\sqrt{n}} \\;=\\; \\dfrac{M_D}{s_{M_D}}, \\quad df = n - 1"),

        div(class = "row g-3 my-3",
            div(class = "col-md-6",
                div(class = "p-3 h-100",
                    style = sprintf("background:%s; color:%s; border-radius:8px;",
                                    PAL$info_bg, PAL$info_fg),
                    tags$b("The paired-collapses-to-one-sample insight."),
                    p(style = "margin-bottom:0;",
                      "Once the difference scores have been computed, the ",
                      "paired t-test ", tags$em("is"),
                      " a single-sample t-test applied to those difference ",
                      "scores, against a null value of zero. Taking the ",
                      "Tab 1 formula ",
                      math_inline("t = (M - \\mu_0)/(s/\\sqrt{n})"),
                      " and substituting ", math_inline("M = M_D"), ", ",
                      math_inline("\\mu_0 = 0"), ", and ",
                      math_inline("s = s_D"),
                      " yields exactly the formula in the math box above. ",
                      "This is one statistical procedure wearing two ",
                      "different costumes."))),
            div(class = "col-md-6",
                div(class = "p-3 h-100",
                    style = sprintf("background:%s; color:%s; border-radius:8px;",
                                    PAL$ok_bg, PAL$ok_fg),
                    tags$b("Why pairing is powerful."),
                    p(style = "margin-bottom:0;",
                      "Subtracting each subject's own baseline removes ",
                      "between-person variability from the denominator. ",
                      "Twin pairs, before/after, matched controls, ",
                      "within-subject manipulations all rely on the same ",
                      "logic. People differ from each other in lots of ",
                      "uninteresting ways, but within-person changes are ",
                      "comparable across people. As a result, ",
                      math_inline("s_D"),
                      " is typically much smaller than the standard ",
                      "deviation of either condition's raw scores, which is ",
                      "why paired tests usually have more statistical power ",
                      "than independent t-tests when subjects can be ",
                      "matched.")))),

        plotOutput("paired_step6_plot", height = "260px"),
        callout_warm(
          tags$b("Reading the picture. "),
          "The curve is the sampling distribution of ", math_inline("M_D"),
          " under H₀ (centered at zero, with SE = ",
          sprintf("%.3f", LAB6$se),
          "). The vertical black line marks the seagull study's observed value of ",
          math_inline("M_D"), " = ", sprintf("%.2f", LAB6$M_D),
          ". That value is ", sprintf("%.2f", LAB6$t),
          " standard errors above zero, which puts it well into the tail of ",
          "the distribution that H₀ is supposed to describe. The remaining ",
          "steps work out exactly how surprising that location is."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 7 -------------------------------------------------------
      step_container(
        7, "Setting alpha (\\(\\alpha\\))",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "Alpha (α) is the pre-specified probability of rejecting the ",
            "null hypothesis in a world where the null hypothesis is in ",
            "fact true. It controls the long-run rate of Type I errors. ",
            "The convention in PSY 302, and across most of psychology, is ",
            "α = .05 with a two-tailed alternative."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Alpha is how much false-alarm risk we are willing to accept ",
            "before running the test. At α = .05, we are deciding in ",
            "advance that we are okay with a 5% chance of rejecting H₀ in ",
            "a world where H₀ happens to be true. Shrinking α makes the ",
            "test more conservative (real effects get missed more often). ",
            "Widening α makes it more liberal (false alarms happen more ",
            "often). The conventional .05 represents an old compromise ",
            "between these two failure modes."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "the seagull study uses α = .05 with a two-tailed alternative, following ",
            "the PSY 302 default. The α-budget is split evenly between the ",
            "two tails of the sampling distribution, so 2.5% sits in the ",
            "upper tail and 2.5% sits in the lower tail."
          )
        ),
        math_block("\\Pr(\\text{reject } H_0 \\mid H_0 \\text{ true}) \\;=\\; \\alpha"),
        callout_warm(
          tags$b("A point of confusion. "),
          "Alpha is set ", tags$em("before"),
          " any data are collected, and the chosen value has nothing to do ",
          "with the t-statistic that eventually comes out of the analysis. ",
          "Alpha defines the cutoffs (which Step 8 will compute), and the ",
          "p-value (Step 9) measures how far past those cutoffs the ",
          "observed result happens to sit."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 8 -------------------------------------------------------
      step_container(
        8, "Critical values: where does the tail begin?",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "For ", math_inline("df = n - 1"),
            " degrees of freedom and a two-tailed test at level α, the ",
            "critical values bounding the rejection region are ",
            math_inline("\\pm t_{\\alpha/2,\\ df}"),
            ". These can be looked up in a t-table or computed in R with ",
            math_inline("\\texttt{qt}"),
            ". The critical value depends on both the chosen α and the df, ",
            "and on nothing else."
          ),
          example = tagList(
            tags$b("In human words. "),
            "The critical value is the signal-to-noise cutoff that separates ",
            "\"plausible under H₀\" from \"too extreme to credit H₀ with.\" ",
            "Larger samples produce tighter sampling distributions, which in ",
            "turn pull the critical value closer to zero, so smaller ",
            "studies have to clear a higher bar to claim a significant ",
            "result."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "With ", math_inline("n = 6"), " participants, ",
            math_inline("df = n - 1 = 5"),
            ". At α = .05 two-tailed, the critical values bounding the ",
            "rejection region work out to ",
            math_inline("\\pm t_{.025,\\ 5} = \\pm 2.571"),
            ". That is the cutoff the seagull study's t-statistic will need to clear."
          )
        ),
        math_block("\\text{Two-tailed CV} \\;=\\; \\pm t_{\\alpha/2,\\ df},\\quad df = n - 1"),
        plotOutput("paired_step8_plot", height = "260px"),
        callout_warm(
          tags$b("Where the small-n penalty comes from. "),
          "With only six subjects, ", math_inline("df = 5"),
          " and the t-distribution has noticeably heavier tails than the ",
          "normal distribution. As a result, the critical value (±2.571) ",
          "sits much further out than the z-equivalent (±1.96). As n grows, ",
          "the t-distribution tightens back toward the normal, and the ",
          "two-tailed .05 critical value drifts back toward ±1.96."
        ),
        id_prefix = "paired-step"
      ),

      # ------ STEP 9 -------------------------------------------------------
      step_container(
        9, "The decision: reject \\(H_0\\), or fail to reject?",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "We reject ", math_inline("H_0"), " when ",
            math_inline("|t| \\geq |\\text{CV}|"),
            ", which is equivalent to the condition ",
            math_inline("p < \\alpha"),
            ". Failing to reject H₀ is a different kind of conclusion ",
            "from concluding that H₀ is true. The test has two formally ",
            "available verdicts, and \"H₀ is true\" is not one of them."
          ),
          example = tagList(
            tags$b("In human words. "),
            "We project our observed t-statistic onto the null sampling ",
            "distribution and check where it lands. If it lands past the ",
            "critical value (somewhere in the α-sized tail of the ",
            "distribution), we reject H₀, because a result that extreme ",
            "would be a rare event in a world where the treatment does ",
            "nothing. If our t-statistic lands inside the critical values, ",
            "we fail to reject H₀, because the result we observed is well ",
            "within the range of things H₀ regularly produces."
          ),
          tldr = tagList(
            tags$b("Case study (seagulls). "),
            "The seagull subset yielded t(5) = 5.61, and |5.61| is ",
            "comfortably larger than the critical value of 2.571, so we ",
            tags$b("reject H₀"),
            " with p ≈ .003. A reasonable APA-style writeup for the subset ",
            "would read: ",
            tags$em("\"Using a paired-samples t-test, seagulls' approach "),
            tags$em("time in the stare condition (M = 67.67 s, SD = 5.28) "),
            tags$em("was significantly shorter than their approach time in "),
            tags$em("the look-away condition (M = 78.83 s, SD = 9.68), "),
            tags$em("t(5) = 5.61, p = .003.\""),
            " The full-sample paper reports the same direction of effect ",
            "with t(18) = 3.13, p = .006 across all 19 seagulls."
          )
        ),
        math_block("\\text{Reject } H_0 \\;\\Longleftrightarrow\\; |t| \\geq \\text{CV} \\;\\Longleftrightarrow\\; p < \\alpha"),

        div(class = "p-3 mt-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("Your t",         sprintf("%.3f", LAB6$t))),
              column(3, stat_card("Critical value", "±2.571")),
              column(3, stat_card("p-value",
                                  if (LAB6$p < 0.001) "< 0.001" else sprintf("%.3f", LAB6$p))),
              column(3, stat_card("α", "0.05"))),
            div(class = "p-3 mt-3",
                style = sprintf("background:%s; color:%s; border-radius:8px; font-weight:600; font-size:18px;",
                                PAL$ok_bg, PAL$ok_fg),
                "Reject H₀",
                tags$br(),
                tags$span(style = "font-weight:400; font-size:14px;",
                          sprintf("|t| = %.2f ≥ |CV| = 2.571. The seagull study's signal-to-noise ratio sits past the cutoff, far into the tail of the null sampling distribution. The hypothesis that staring at seagulls does nothing to their approach time is therefore rejected.",
                                  abs(LAB6$t))))),
        plotOutput("paired_step9_plot", height = "320px"),
        callout_warm(
          tags$b("Plain-language summary. "),
          "Every paired t-test reduces to the same final move. We line the ",
          "observed signal-to-noise ratio ",
          math_inline("M_D / s_{M_D}"),
          " up against the critical signal-to-noise value on the null ",
          "sampling distribution. Past the critical value, we reject H₀. ",
          "Inside the critical value, we fail to reject H₀. The phrase ",
          "\"fail to reject\" is sometimes misread as \"H₀ is true,\" but ",
          "the two are different conclusions, and only the first is ",
          "available from this procedure."
        ),
        id_prefix = "paired-step"
      ),

      # ------ APA write-up reference card ----------------------------------
      div(class = "p-3 mt-4",
          style = sprintf("background:%s; color:%s; border-radius:10px; border-left:5px solid %s;",
                          PAL$warn_bg, PAL$warn_fg, OI$orange),
          h5("Translating numbers into APA-style prose", style = "margin-top:0;"),
          p("PSY 302 expects a standard write-up format. Here is the APA paragraph for the seagull subset analysis:"),
          tags$blockquote(style = "font-style:italic; margin-left:8px; border-left:4px solid #d6c08a; padding-left:12px;",
            "Using a paired-samples t-test, seagulls' approach time when ",
            "the human stared at them (M = 67.67 s, SD = 5.28) was ",
            "significantly shorter than their approach time when the human ",
            "looked away (M = 78.83 s, SD = 9.68), t(5) = 5.61, p = .003."),
          p(style = "margin-bottom:0;",
            tags$b("Template: "),
            "Using a [test], [DV after-condition (M = , SD = )] was ",
            "[significantly / non-significantly] [direction] than [DV ",
            "before-condition (M = , SD = )], t(df) = , p = , 95% CIµDiff[ , ].")
      ),

      # ===================================================================
      # Tutorial finale: build intuition, then unlock practice quest
      # ===================================================================
      div(class = "mt-5 mb-3 p-4",
          style = sprintf("background:#E1F1FB; border-left:6px solid %s; border-radius:10px;",
                          PAL$pop_dark),
          h3("Now play with the machinery", style = "margin-top:0;"),
          p("The numbers in the seagull walkthrough are fixed, but the ",
            "test's behavior is not. Before you move on to the practice ",
            "quest, take a few minutes to picture how the result would ",
            "change if the inputs changed. The simulation buttons in ",
            "Step 3 are the most direct way to experience this: each ",
            "click draws a new imagined replication, so you can watch ",
            "the cloud of ", math_inline("M_D"),
            " values fill in across imagined re-runs of the study."),
          tags$ul(style = "margin-bottom:0; padding-left:20px; line-height:1.7;",
            tags$li(tags$b("Bigger sample size. "),
              "If we observed 19 seagulls (the original Goumas et al. n) ",
              "instead of six, ", math_inline("s_{M_D}"),
              " would shrink by √(19/6) ≈ 1.78. The same ",
              math_inline("M_D"), " of 11 seconds would yield a t-value ",
              "roughly 1.78× bigger, and a smaller p-value. Larger ",
              "samples make the same effect easier to detect."),
            tags$li(tags$b("Bigger or smaller sample SD. "),
              "Picture the seagull data noisier: each seagull's reaction ",
              "to the stare varied wildly. ", math_inline("s_D"),
              " would grow, ", math_inline("s_{M_D}"),
              " would grow with it, and the t-statistic would shrink ",
              "toward zero. A real effect can disappear under noise."),
            tags$li(tags$b("A different M_D. "),
              "Suppose stare-down delayed seagull approach by only 2 ",
              "seconds on average instead of 11. The t-statistic would ",
              "drop by roughly 5×, and the conclusion would likely flip ",
              "to fail-to-reject. Effect size matters as much as ",
              "sample size."),
            tags$li(tags$b("Run the Step 3 simulation many times. "),
              "Each click of ", tags$em("Run 5 more"),
              " draws fresh imagined replications. Watch how the cloud ",
              "of M_D values clusters around the true average change ",
              "and never lands exactly on any single value."))
      ),
      div(class = "text-center mb-4",
          p(style = "color:#444; font-size:14px;",
            "When the relationship between sample size, noise, and the ",
            "test result feels intuitive, unlock the practice quest ",
            "below."),
          actionButton("tab2_show_quest",
                       "Show me the practice quest →",
                       class = "btn-primary btn-lg")),

      # ===================================================================
      # QUEST. NICU naming-convention intervention (Adelman et al., 2015)
      # ===================================================================
      conditionalPanel(condition = "input.tab2_show_quest > 0",
      quest_section(
        title = "Does changing how unnamed NICU babies are labeled reduce medical errors?",
        id_prefix = "tab2quest",
        scenario_html = tagList(
          p("Before 2013, most American NICUs identified unnamed newborns ",
            "using extremely generic temporary labels such as ",
            tags$em("BabyBoy Smith"), " or ", tags$em("BabyGirl Jones"),
            ". Two infants in the same unit could end up with nearly ",
            "identical chart names, and Adelman and colleagues found that ",
            "this contributed to ", tags$b("retract-and-reorder"),
            " errors: situations where a clinician orders a treatment, ",
            "then later retracts the order because it was charted under ",
            "the wrong patient. The intervention swapped the generic ",
            "labels for ones that included the mother's first name (e.g., ",
            tags$em("WendysBoy Smith"),
            "), giving each unnamed infant a more distinctive chart name."),
          p(style = "margin-bottom:0;",
            "Ten NICUs were tracked for one year before the intervention ",
            "and one year after. The values below are monthly counts of ",
            "retract-and-reorder events at each NICU. Use a paired-samples ",
            "t-test at α = .05, two-tailed, to evaluate whether the ",
            "intervention reduced errors.")
        ),
        data_block = tagList(
          tags$table(class = "table table-sm mb-0",
            tags$thead(tags$tr(
              tags$th("NICU"), tags$th("Before"), tags$th("After"),
              tags$th("Difference (Before − After)"))),
            tags$tbody(
              lapply(seq_along(NICU$before), function(i) tags$tr(
                tags$td(LETTERS[i]),
                tags$td(NICU$before[i]),
                tags$td(NICU$after[i]),
                tags$td(NICU$D[i])
              ))
            )),
          tags$p(style = "margin-top:8px; margin-bottom:0;",
                 tags$b("Summary stats: "),
                 sprintf("n = %d, M_D = %.2f, SS_D = %.2f, s_D = %.3f.",
                         NICU$n, NICU$M_D, NICU$SS_D, NICU$s_D))
        ),
        questions = list(
          list(
            prompt = tagList(tags$b("Step 1. "),
              "State H₀ and H₁ for the NICU study."),
            solution = tagList(
              p(math_inline("H_0: \\mu_D = 0"),
                ". The new naming convention does nothing on average to ",
                "the monthly count of retract-and-reorder errors."),
              p(math_inline("H_1: \\mu_D \\neq 0"),
                ". The new naming convention changes the average monthly ",
                "count in some direction."))
          ),
          list(
            prompt = tagList(tags$b("Step 2. "),
              "Compute M_D and s_D for the ten NICUs. The differences are ",
              "given in the right column of the table above."),
            solution = tagList(
              p("The ten differences are 11, 19, 14, 18, 4, 18, 22, 13, ",
                "11, 11. Summing gives 141, so ",
                math_inline("M_D = 141/10 = 14.10"),
                " errors. The deviations from 14.10 squared and summed ",
                "give ", math_inline("\\text{SS}_D = 248.90"),
                ", and the sample SD of the differences is ",
                math_inline("s_D = \\sqrt{248.90/9} \\approx 5.26"), "."))
          ),
          list(
            prompt = tagList(tags$b("Step 3. "),
              "Would you expect another set of ten NICUs to produce the ",
              "exact same M_D? Why or why not?"),
            solution = tagList(
              p("No. A different set of ten NICUs would have different ",
                "patient mixes, different staffing patterns, and different ",
                "baseline error rates, so their ",
                math_inline("M_D"),
                " would land at a slightly different value even if the ",
                "intervention worked identically. This run-to-run wiggle ",
                "is sampling variability, and it is exactly what the ",
                "denominator of the t-statistic is designed to measure."))
          ),
          list(
            prompt = tagList(tags$b("Step 4. "),
              "Under H₀, where is the sampling distribution of M_D ",
              "centered? What is its shape?"),
            solution = tagList(
              p("Under H₀, the sampling distribution of ",
                math_inline("M_D"),
                " is centered at zero (because H₀ says ",
                math_inline("\\mu_D = 0"),
                "). Its shape is a t-distribution with ",
                math_inline("df = n - 1 = 9"),
                ", which has heavier tails than the normal distribution. ",
                "Its width is set by the standard error you will compute ",
                "in the next step."))
          ),
          list(
            prompt = tagList(tags$b("Step 5. "),
              "Compute the standard error of M_D."),
            solution = tagList(
              p(math_inline("s_{M_D} = s_D/\\sqrt{n} = 5.26/\\sqrt{10}"),
                ". The square root of 10 is roughly 3.162, so ",
                math_inline("s_{M_D} \\approx 5.26/3.162 \\approx 1.66"),
                " errors. Across imagined re-runs of the study, the ",
                "monthly-error-reduction estimate would typically wiggle ",
                "by about 1.66 errors per NICU."))
          ),
          list(
            prompt = tagList(tags$b("Step 6. "),
              "Compute the paired t-statistic."),
            solution = tagList(
              p(math_inline("t = M_D / s_{M_D} = 14.10 / 1.66 \\approx 8.49"),
                ". The observed average reduction sits 8.49 standard ",
                "errors above zero, which is an enormous distance on the ",
                "null sampling distribution."))
          ),
          list(
            prompt = tagList(tags$b("Step 7. "),
              "What α value should we use, and what does it represent?"),
            solution = tagList(
              p("Use α = .05, two-tailed, the PSY 302 default. Alpha is ",
                "the maximum acceptable rate of falsely rejecting a true ",
                "null hypothesis. The two-tailed framing splits .05 evenly ",
                "across the two tails of the sampling distribution."))
          ),
          list(
            prompt = tagList(tags$b("Step 8. "),
              "Find the two-tailed critical value at α = .05 for the ",
              "appropriate df."),
            solution = tagList(
              p("With ", math_inline("df = n - 1 = 9"),
                ", the two-tailed .05 critical values are ",
                math_inline("\\pm t_{.025,\\ 9} = \\pm 2.262"),
                ". Any observed |t| ≥ 2.262 lands in the rejection region."))
          ),
          list(
            prompt = tagList(tags$b("Step 9. "),
              "Compare your observed t to the critical value, state your ",
              "decision, and write up the result in APA-style prose."),
            solution = tagList(
              p("|t| = 8.49 is far larger than 2.262, so we ",
                tags$b("reject H₀"),
                ". The p-value is less than .001."),
              p(style = "margin-bottom:0;",
                "APA-style writeup: ",
                tags$em("\"Using a paired-samples t-test, monthly counts of "),
                tags$em("retract-and-reorder errors after the naming-"),
                tags$em("convention change (M = 31.70, SD = 7.21) were "),
                tags$em("significantly lower than before the change "),
                tags$em("(M = 45.80, SD = 7.84), t(9) = 8.49, p < .001.\""),
                " The intervention reduced errors. Adelman and colleagues ",
                "(2015) report the same direction of effect in the full ",
                "study, supporting the conclusion that more distinctive ",
                "chart names reduce medication mix-ups."))
          )
        )
      )
      )  # close conditionalPanel(tab2_show_quest)
    ))
  })

  # ---- Per-step reactive state and plot outputs -----------------------------

  # Step 2 plots are static (the seagull study numbers, baked into LAB6 above)
  output$paired_step2_pairs <- renderPlot({
    plot_paired_pairs(LAB6$before, LAB6$after, LAB6$participants)
  }, res = 96)
  output$paired_step2_diffs <- renderPlot({
    plot_diff_scores(LAB6$D, LAB6$M_D, LAB6$participants)
  }, res = 96)

  # Step 3 . simulated replications of the seagull study
  pw <- reactiveValues(repeat_MDs = numeric(0), many_MDs = numeric(0))
  observeEvent(input$paired_repeat_one, {
    # simulate one fresh n=6 study using the seagull study's estimated parameters
    new <- rnorm(LAB6$n, mean = LAB6$M_D, sd = LAB6$s_D)
    pw$repeat_MDs <- c(pw$repeat_MDs, mean(new))
  })
  observeEvent(input$paired_repeat_five, {
    new_MDs <- replicate(5, mean(rnorm(LAB6$n, mean = LAB6$M_D, sd = LAB6$s_D)))
    pw$repeat_MDs <- c(pw$repeat_MDs, new_MDs)
  })
  observeEvent(input$paired_repeat_reset, { pw$repeat_MDs <- numeric(0) })

  output$paired_step3_plot <- renderPlot({
    MDs <- pw$repeat_MDs
    xr  <- c(LAB6$M_D - 4 * LAB6$se, LAB6$M_D + 4 * LAB6$se)
    g <- ggplot() +
      geom_vline(xintercept = LAB6$M_D, colour = PAL$pop_dark,
                 linetype = "dashed", linewidth = 0.4) +
      annotate("label", x = LAB6$M_D, y = 1.05,
               label = sprintf("Centre = our M_D = %.2f", LAB6$M_D),
               colour = PAL$pop_dark, fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.95, size = 4)
    if (length(MDs) > 0) {
      g <- g + geom_dotplot(
        data = data.frame(x = MDs),
        aes(x = x),
        binwidth = (xr[2] - xr[1]) / 50,
        fill = PAL$samp_pt, colour = "white",
        stackdir = "up", dotsize = 0.85,
        method = "histodot", stackratio = 1.05
      )
    } else {
      g <- g + annotate("label", x = LAB6$M_D, y = 0.5,
                        label = "Click a button above to simulate replications",
                        colour = "#666", fill = "white", label.size = NA, size = 4.2)
    }
    g + scale_x_continuous(limits = xr) +
      scale_y_continuous(NULL, breaks = NULL, limits = c(0, 1.2)) +
      labs(x = "Possible M_D from an imagined replication", y = NULL,
           subtitle = "Each green dot = one imagined re-run of the seagull study. The dashed line is the seagull study's actual M_D.") +
      base_theme() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }, res = 96)
  output$paired_step3_counter <- renderUI({
    div(class = "p-2", style = sprintf("color:%s; font-size:14px;", PAL$pop_dark),
        sprintf("Studies you've simulated so far: %d", length(pw$repeat_MDs)))
  })

  # Step 4 . sampling distribution of M_D under H₀ (full curve)
  output$paired_step4_plot <- renderPlot({
    se   <- LAB6$se
    df_v <- LAB6$df
    xr   <- c(min(0, LAB6$M_D) - 4 * se, max(0, LAB6$M_D) + 4 * se)
    x    <- seq(xr[1], xr[2], length.out = 400)
    y    <- dt((x - 0) / se, df_v) / se
    cd   <- data.frame(x = x, y = y)
    max_y <- max(cd$y)
    ggplot(cd, aes(x, y)) +
      geom_area(fill = PAL$pop_med, alpha = 0.2) +
      geom_line(colour = PAL$pop_dark, linewidth = 0.7) +
      geom_vline(xintercept = 0, colour = PAL$pop_dark,
                 linetype = "dashed", linewidth = 0.5) +
      geom_vline(xintercept = LAB6$M_D, colour = PAL$samp_line, linewidth = 1.1) +
      annotate("label", x = 0, y = max_y * 1.08,
               label = "Under H₀, M_D should be near 0",
               colour = PAL$pop_dark, fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.95, size = 3.8) +
      annotate("label", x = LAB6$M_D, y = max_y * 0.6,
               label = sprintf("Our M_D = %.2f", LAB6$M_D),
               colour = PAL$samp_line, fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.95,
               size = 4.2, fontface = "bold") +
      scale_y_continuous(limits = c(0, max_y * 1.25)) +
      labs(x = "Possible M_D",
           y = "Density",
           subtitle = sprintf(
             "Sampling distribution of M_D under H₀: centered at 0, SE = %.3f, df = %d. Far from the curve's bulk = surprising under H₀.",
             se, df_v)) +
      base_theme()
  }, res = 96)

  # Step 6 . sampling distribution with observed M_D
  output$paired_step6_plot <- renderPlot({
    plot_sampling_dist_MD(LAB6$M_D, LAB6$se, LAB6$df, mu_D_null = 0)
  }, res = 96)

  # Step 8 . t-distribution with CV marked
  output$paired_step8_plot <- renderPlot({
    alpha <- 0.05; df_v <- LAB6$df
    cv    <- qt(1 - alpha / 2, df_v)
    xr    <- t_xrange(-cv, cv)
    x     <- seq(xr[1], xr[2], length.out = 400)
    cd    <- data.frame(x = x, y = dt(x, df_v))
    reg_lo <- subset(cd, x <= -cv)
    reg_hi <- subset(cd, x >=  cv)
    max_y  <- max(cd$y)
    label_offset <- diff(xr) * 0.02
    ggplot() +
      geom_area(data = reg_lo, aes(x, y), fill = PAL$reject, alpha = 0.65) +
      geom_area(data = reg_hi, aes(x, y), fill = PAL$reject, alpha = 0.65) +
      geom_line(data = cd, aes(x, y), colour = "black", linewidth = 0.7) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
      geom_vline(xintercept = c(-cv, cv), colour = PAL$reject_dk,
                 linetype = "dashed", linewidth = 0.5) +
      annotate("label", x =  cv + label_offset, y = max_y * 0.85,
               label = sprintf("CV = %+.3f", cv),
               colour = PAL$reject_dk, fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.95,
               size = 3.8, fontface = "bold", hjust = 0) +
      annotate("label", x = -cv - label_offset, y = max_y * 0.85,
               label = sprintf("CV = %+.3f", -cv),
               colour = PAL$reject_dk, fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.95,
               size = 3.8, fontface = "bold", hjust = 1) +
      scale_x_continuous(limits = xr, breaks = t_xbreaks(xr)) +
      labs(x = sprintf("t-statistic (df = %d)", df_v), y = "Density",
           subtitle = "Orange tails = the α = 0.05 rejection region (2.5% per side).") +
      base_theme()
  }, res = 96)

  # Step 9 . full decision picture
  output$paired_step9_plot <- renderPlot({
    draw_t_curve(LAB6$t, LAB6$df, alpha = 0.05) +
      labs(subtitle = sprintf(
        "df = %d  |  α = 0.05  |  p = %s  |  decision: reject H₀",
        LAB6$df,
        if (LAB6$p < 0.001) "< 0.001" else sprintf("%.3f", LAB6$p)))
  }, res = 96)
}
