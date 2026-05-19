# =============================================================================
# tab_anova.R
# Between-Subjects One-Way ANOVA walkthrough. Regional personality differences
# across the US. Based on state-level NEO neuroticism scores from Renfrew et
# al. (2013), aggregated into the four U.S. Census Bureau regions and
# popularized by Time Magazine's "America's Mood Map" interactive.
# https://notawfulandboring.blogspot.com/2020/04/online-day-6-one-way-anova-example.html
# https://time.com/7612/americas-mood-map-an-interactive-guide-to-the-united-states-of-attitude/
# =============================================================================

# ---- Case-study constants: state-level neuroticism by US Census region ------
# Each value is one state's average neuroticism score on the NEO. The data
# come from "ANOVA OCEAN by State.csv" (n_states = 49; Alaska, Hawaii and DC
# are excluded by some Census definitions). Region codes: NE, MW, S, W.
NEURO <- local({
  Northeast <- c(57.7, 59.6, 53.4, 60.0, 71.0, 60.7, 56.9, 64.9, 63.9)
  Midwest   <- c(56.0, 53.4, 50.6, 50.8, 36.1, 44.5, 49.2, 41.7, 48.5,
                 59.3, 47.6, 45.5)
  South     <- c(48.7, 56.2, 49.7, 41.8, 56.5, 38.0, 59.6, 51.0, 79.2,
                 41.7, 59.6, 41.4, 47.0, 51.0, 47.4, 50.0)
  West      <- c(38.1, 39.1, 49.7, 39.0, 30.4, 32.5, 43.6, 41.6, 38.4,
                 52.1, 51.0, 43.4)
  groups       <- list(Northeast = Northeast, Midwest = Midwest,
                       South = South, West = West)
  group_names <- names(groups)
  k          <- length(groups)
  N          <- sum(lengths(groups))
  M_grp      <- vapply(groups, mean, numeric(1))
  n_grp      <- lengths(groups)
  s_grp      <- vapply(groups, sd, numeric(1))
  SS_grp     <- vapply(groups, function(g) sum((g - mean(g))^2), numeric(1))
  M_grand    <- sum(M_grp * n_grp) / N
  SS_total   <- sum(vapply(groups, function(g) sum((g - M_grand)^2), numeric(1)))
  SS_between <- sum(n_grp * (M_grp - M_grand)^2)
  SS_within  <- sum(SS_grp)
  df_b       <- k - 1
  df_w       <- N - k
  MS_b       <- SS_between / df_b
  MS_w       <- SS_within  / df_w
  F_obs      <- MS_b / MS_w
  p_val      <- pf(F_obs, df_b, df_w, lower.tail = FALSE)
  F_cv       <- qf(0.95, df_b, df_w)
  list(groups = groups, group_names = group_names, k = k, N = N,
       n_grp = n_grp, M_grp = M_grp, s_grp = s_grp, SS_grp = SS_grp,
       M_grand = M_grand,
       SS_total = SS_total, SS_between = SS_between, SS_within = SS_within,
       df_b = df_b, df_w = df_w,
       MS_b = MS_b, MS_w = MS_w, F = F_obs, p = p_val, F_cv = F_cv)
})
LAB7 <- NEURO  # alias for backwards compatibility

# ---- Quest dataset: life satisfaction (Cantril Ladder) by world region -----
# Fabricated to reflect typical patterns in the World Happiness Report data.
# Each respondent rates their current life on a 0-10 ladder.
CANTRIL <- local({
  Europe        <- c(8, 7, 7, 8, 6, 8, 7, 7, 8, 6, 7, 9)
  N_America     <- c(7, 8, 7, 6, 7, 8, 6, 7)
  East_Asia     <- c(6, 5, 6, 7, 5, 6, 6, 5, 7, 6)
  Sub_Saharan   <- c(4, 5, 3, 4, 5, 5, 4, 6, 5, 4, 3, 4)
  groups        <- list(Europe = Europe, `North America` = N_America,
                        `East Asia` = East_Asia, `Sub-Saharan Africa` = Sub_Saharan)
  group_names <- names(groups)
  k          <- length(groups)
  N          <- sum(lengths(groups))
  M_grp      <- vapply(groups, mean, numeric(1))
  n_grp      <- lengths(groups)
  s_grp      <- vapply(groups, sd, numeric(1))
  SS_grp     <- vapply(groups, function(g) sum((g - mean(g))^2), numeric(1))
  M_grand    <- sum(M_grp * n_grp) / N
  SS_total   <- sum(vapply(groups, function(g) sum((g - M_grand)^2), numeric(1)))
  SS_between <- sum(n_grp * (M_grp - M_grand)^2)
  SS_within  <- sum(SS_grp)
  df_b       <- k - 1
  df_w       <- N - k
  MS_b       <- SS_between / df_b
  MS_w       <- SS_within  / df_w
  F_obs      <- MS_b / MS_w
  p_val      <- pf(F_obs, df_b, df_w, lower.tail = FALSE)
  F_cv       <- qf(0.95, df_b, df_w)
  list(groups = groups, group_names = group_names, k = k, N = N,
       n_grp = n_grp, M_grp = M_grp, s_grp = s_grp, SS_grp = SS_grp,
       M_grand = M_grand,
       SS_total = SS_total, SS_between = SS_between, SS_within = SS_within,
       df_b = df_b, df_w = df_w,
       MS_b = MS_b, MS_w = MS_w, F = F_obs, p = p_val, F_cv = F_cv)
})

# ---- Plot helpers (ANOVA-tab-specific) -------------------------------------
plot_k_groups <- function(groups, show_grand_mean = TRUE) {
  M_grp   <- vapply(groups, mean, numeric(1))
  M_grand <- weighted.mean(M_grp, lengths(groups))
  long <- do.call(rbind, lapply(seq_along(groups), function(i) {
    data.frame(group = factor(names(groups)[i], levels = names(groups)),
               score = groups[[i]])
  }))
  cols <- c(PAL$pop_dark, PAL$samp_pt, OI$vermillion, OI$purple, OI$orange)
  g <- ggplot(long, aes(x = group, y = score, colour = group)) +
    geom_jitter(width = 0.18, height = 0, size = 3, alpha = 0.85) +
    scale_colour_manual(values = setNames(cols[seq_along(groups)], names(groups))) +
    base_theme() +
    theme(legend.position = "none") +
    labs(x = NULL, y = "Score")
  for (i in seq_along(groups)) {
    g <- g + annotate("segment",
                      x = i - 0.3, xend = i + 0.3,
                      y = M_grp[i], yend = M_grp[i],
                      colour = cols[i], linewidth = 1.4) +
      annotate("label", x = i, y = M_grp[i],
               label = sprintf("M_%s = %.2f", names(groups)[i], M_grp[i]),
               colour = cols[i], fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.95,
               size = 3.6, fontface = "bold", vjust = -0.7)
  }
  if (show_grand_mean) {
    g <- g + geom_hline(yintercept = M_grand,
                        linetype = "dashed", colour = "grey45", linewidth = 0.5) +
      annotate("label", x = 0.6, y = M_grand,
               label = sprintf("Grand mean = %.2f", M_grand),
               colour = "grey25", fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.92,
               size = 3.4, hjust = 0, vjust = -0.3)
  }
  g
}

# Familywise-error curve: P(≥1 false positive) = 1 - (1-α)^c
plot_familywise <- function(alpha = 0.05) {
  c_vals <- 1:20
  p_at_least_one <- 1 - (1 - alpha) ^ c_vals
  df <- data.frame(c = c_vals, p = p_at_least_one)
  ggplot(df, aes(c, p)) +
    geom_hline(yintercept = alpha, linetype = "dashed", colour = PAL$pop_dark,
               linewidth = 0.5) +
    annotate("label", x = 18, y = alpha,
             label = "Original α = .05", colour = PAL$pop_dark,
             fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95, size = 3.4) +
    geom_line(colour = PAL$reject, linewidth = 1.2) +
    geom_point(colour = PAL$reject, size = 2.4) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous(breaks = c(1, 3, 6, 10, 15, 20)) +
    labs(x = "Number of pairwise t-tests run",
         y = "Probability of ≥ 1 false positive",
         subtitle = "Each extra t-test inflates the family-wise error rate. By 6 tests you're past 26%.") +
    base_theme()
}

# SS-decomposition vertical-arrows picture
plot_ss_decomposition <- function(groups) {
  M_grp   <- vapply(groups, mean, numeric(1))
  M_grand <- weighted.mean(M_grp, lengths(groups))
  long <- do.call(rbind, lapply(seq_along(groups), function(i) {
    data.frame(group = factor(names(groups)[i], levels = names(groups)),
               gid   = i,
               score = groups[[i]],
               M_grp = M_grp[i])
  }))
  cols <- c(PAL$pop_dark, PAL$samp_pt, OI$vermillion, OI$purple, OI$orange)
  ggplot(long, aes(x = gid, y = score, colour = group)) +
    # Within-group arrows (from each dot to its group mean)
    geom_segment(aes(xend = gid, yend = M_grp),
                 colour = PAL$samp_pt, alpha = 0.6, linewidth = 0.4) +
    # Between-group arrows (from each group mean to grand mean)
    geom_segment(data = data.frame(gid = seq_along(groups), M_grp = M_grp,
                                   M_grand = M_grand),
                 aes(x = gid, xend = gid,
                     y = M_grp, yend = M_grand),
                 colour = PAL$reject, alpha = 0.95, linewidth = 1.2,
                 inherit.aes = FALSE) +
    geom_jitter(width = 0.05, height = 0, size = 2.5, alpha = 0.85) +
    geom_hline(yintercept = M_grand, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_point(data = data.frame(gid = seq_along(groups), M_grp = M_grp),
               aes(x = gid, y = M_grp),
               colour = "black", size = 3.4, inherit.aes = FALSE) +
    scale_colour_manual(values = setNames(cols[seq_along(groups)], names(groups))) +
    scale_x_continuous(breaks = seq_along(groups),
                       labels = names(groups)) +
    annotate("label", x = 0.6, y = M_grand,
             label = sprintf("Grand mean = %.2f", M_grand),
             colour = "grey25", fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.92,
             size = 3.4, hjust = 0, vjust = -0.3) +
    labs(x = NULL, y = "Score",
         subtitle = "Orange = each group mean's distance to grand mean (between). Green = each observation's distance to its group mean (within).") +
    base_theme() +
    theme(legend.position = "none")
}

# ---- Server entry point -----------------------------------------------------
source_anova_server <- function(input, output, session) {

  output$anova_tab_ui <- renderUI({
    withMathJax(div(class = "container-fluid", style = "max-width: 1240px; padding-top: 16px;",

      scenario_card(
        "Build a between-subjects one-way ANOVA, from the ground up",
        lab_label = "Running example: America's mood map",
        p(style = "margin-bottom:6px;",
          "Analysis of variance (ANOVA) is what we reach for when there are ",
          tags$b("three or more groups"),
          " to compare. The question the test addresses is whether any of ",
          "the group means differ from one another, taken collectively, ",
          "rather than whether any one specific pair of group means differs."),
        p(style = "margin-bottom:6px;",
          tags$b("Running example. "),
          "In 2013, Time Magazine published an interactive feature called ",
          tags$a("America's Mood Map",
                 href = "https://time.com/7612/americas-mood-map-an-interactive-guide-to-the-united-states-of-attitude/",
                 target = "_blank"),
          ", drawing on a personality study by Renfrew and colleagues that ",
          "gave the NEO to roughly 1.5 million Americans. The piece argued ",
          "that the Northeast scores high on neuroticism (the ",
          tags$em("\"Temperamental and Uninhibited\""),
          " region in the article's framing), the West scores low, and the ",
          "Midwest and South sit somewhere in between. We can ask whether ",
          "those regional differences are real by treating each state's ",
          "neuroticism score as one observation and running a one-way ",
          "ANOVA with U.S. Census region as the grouping factor. The data ",
          "for the walkthrough cover 49 states, distributed unevenly across ",
          "four regions: Northeast (n = 9), Midwest (n = 12), South (n = ",
          "16), West (n = 12). The full F-test we work out yields F(3, 45) ",
          "= 10.38, p < .001."),
        p(style = "margin-bottom:0;",
          tags$b("How this compares to the t-tests. "),
          "The test statistic changes from t to F, but the underlying ",
          "signal-divided-by-noise idea remains intact. In ANOVA, the ",
          "signal portion of the ratio captures between-group variability ",
          "(how spread out the group means are around the grand mean), and ",
          "the noise portion captures within-group variability (how spread ",
          "out the individual scores are around their own group means). ",
          "When the signal is large relative to the noise, we conclude that ",
          "the groups likely come from populations with different means.")
      ),

      # ------ STEP 1 -------------------------------------------------------
      step_container(
        1, "The null world: \\(H_0:\\ \\mu_1 = \\mu_2 = \\cdots = \\mu_k\\)",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "Consider ", math_inline("k"),
            " independent populations with means ",
            math_inline("\\mu_1, \\mu_2, \\ldots, \\mu_k"),
            " and a common variance ", math_inline("\\sigma^2"),
            ". The null hypothesis for a one-way ANOVA specifies that all ",
            math_inline("k"),
            " population means are equal to one another. The alternative ",
            "hypothesis specifies that at least one pair of population ",
            "means differs. The phrasing of the alternative is worth ",
            "pausing on. It does not assert that all means differ, only ",
            "that some pair of them does."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Picture a world in which the three treatment groups all draw ",
            "from populations with the same average neuroticism score, so that the ",
            "regional label has no real effect on depression. Even in that ",
            "world, our three sample means will look a little different ",
            "from each other simply because each group's sample is random. ",
            "The test asks whether the variation in the group means is ",
            "large enough that sampling luck on its own is an unreasonable ",
            "explanation for it."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            "For the mood-map study, H₀ specifies that ",
            math_inline("\\mu_{\\text{NE}} = \\mu_{\\text{MW}} = \\mu_{\\text{W}}"),
            ". H₁ specifies that at least one of these three means differs ",
            "from another. Rejecting H₀ does not tell us that all three ",
            "differ. It tells us only that some pair does, and a separate ",
            "post-hoc procedure (Tukey HSD, in Step 9) is needed to ",
            "identify which pairs are driving the omnibus result."
          )
        ),
        math_block("H_0:\\ \\mu_1 = \\mu_2 = \\cdots = \\mu_k \\qquad H_1:\\ \\text{at least one pair of }\\mu\\text{'s differs}"),
        div(class = "p-3 my-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            h5("The case-study numbers for this step", style = "margin-top:0;"),
            p(style = "color:#666; font-size:13px;",
              "The one-way ANOVA reasons about k populations at once. ",
              "For the mood-map study we have four (one per U.S. Census ",
              "region). Under H₀, all four populations have the same ",
              "average neuroticism score."),
            fluidRow(
              column(3, stat_card("μ_NE (Northeast)",
                                  sprintf("estimated as %.2f", LAB7$M_grp["Northeast"]))),
              column(3, stat_card("μ_MW (Midwest)",
                                  sprintf("estimated as %.2f", LAB7$M_grp["Midwest"]))),
              column(3, stat_card("μ_S (South)",
                                  sprintf("estimated as %.2f", LAB7$M_grp["South"]))),
              column(3, stat_card("μ_W (West)",
                                  sprintf("estimated as %.2f", LAB7$M_grp["West"])))),
            fluidRow(
              column(12, p(style = "color:#666; font-size:13px; margin-top:8px; margin-bottom:0;",
                tags$b("H₀ in this study: "),
                "all four μ values are equal. The next steps will evaluate ",
                "whether the observed spread of regional means is too large ",
                "to credit a single population mean.")))),
        callout_warm(
          tags$b("Worth pausing on the alternative hypothesis. "),
          "H₁ asserts that at least one pair of means differs, which is a ",
          "weaker claim than the claim that all of them differ. When we ",
          "reject H₀ with k = 4 groups, the result tells us that some ",
          "pair-level difference exists somewhere in the data. It does ",
          "not tell us how many pairs differ, or which ones. That is what ",
          "the post-hoc tests are for."
        ),
        id_prefix = "anova-step"
      ),

      # ------ STEP 2 -------------------------------------------------------
      step_container(
        2, "Why not just run a bunch of t-tests? (Family-wise error)",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "With ", math_inline("k"), " groups there are ",
            math_inline("\\binom{k}{2} = k(k-1)/2"),
            " possible pairwise comparisons. If each comparison is tested ",
            "at level α, the family-wise Type I error rate (the probability ",
            "of producing at least one false positive across the whole ",
            "family of tests) is bounded above by ",
            math_inline("1 - (1-\\alpha)^c"),
            ", where c is the number of comparisons. This is the ",
            "Boole-Bonferroni bound, and the related Šidák correction is ",
            "tighter when the comparisons are independent. A single omnibus ",
            "ANOVA addresses the question with one test, at one α."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Each additional t-test creates another opportunity for a false ",
            "positive to slip through. With three t-tests at α = .05, the ",
            "chance of at least one of them coming up significant under ",
            "H₀ rises to over 26%. With ten, it is over 40%. One omnibus ",
            "ANOVA evaluates everything at the same α without that ",
            "inflation."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            "With k = 4 groups (Northeast, Midwest, South, West), there are ",
            "six possible pairwise t-tests. Running all three at α = .05 ",
            "would inflate the family-wise error rate to roughly 14%. The ",
            "omnibus F-test in this tab controls error at α = .05 across ",
            "all comparisons taken together. If the omnibus F is ",
            "significant, the next step (Tukey HSD) handles the pairwise ",
            "comparisons with an appropriate correction built in."
          )
        ),
        math_block("\\text{family-wise } \\alpha \\;\\leq\\; 1 - (1-\\alpha)^c"),
        plotOutput("anova_step2_plot", height = "300px"),
        callout_warm(
          tags$b("This is the motivating reason ANOVA exists. "),
          "Everything that follows in the rest of this tab (the F-statistic, ",
          "the sums of squares, the mean squares) is bookkeeping in the ",
          "service of one goal. We want a single test, at a single α, that ",
          "asks whether the group means are too spread out to attribute ",
          "to sampling luck."
        ),
        id_prefix = "anova-step"
      ),

      # ------ STEP 3 -------------------------------------------------------
      step_container(
        3, "Run one study: \\(k\\) group means and the grand mean",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "Within each group ", math_inline("j"),
            " we compute the group mean ", math_inline("M_j"),
            " and record the group sample size ", math_inline("n_j"),
            ". The ", tags$b("grand mean"),
            " is the average of every observation in the dataset, taken ",
            "across groups, and equals ",
            math_inline("M_{\\cdot} = \\frac{1}{N}\\sum_j n_j M_j"),
            ", a weighted average of the group means."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Every group has its own average. If we stack all of the ",
            "scores together and compute one big average across the whole ",
            "dataset, ignoring group membership, we get the grand mean. ",
            "The interesting question for ANOVA is how far the individual ",
            "group means sit from that grand mean. Larger displacements ",
            "are evidence of group-level effects. Smaller displacements are ",
            "consistent with H₀."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            "The four regional means for state-level neuroticism are ",
            math_inline("M_{\\text{NE}} = 60.91"),
            ", ", math_inline("M_{\\text{MW}} = 48.61"),
            ", ", math_inline("M_{\\text{S}} = 51.23"),
            ", and ", math_inline("M_{\\text{W}} = 41.61"),
            ". The group sample sizes are 9, 12, 16, and 12 respectively, ",
            "for a total of N = 49 states. The grand mean works out to ",
            sprintf("%.2f", LAB7$M_grand),
            ". The Northeast sits well above the grand mean, the West well ",
            "below it, and the Midwest and South land in between. The Time ",
            "Magazine framing of the Northeast as ",
            tags$em("\"temperamental and uninhibited\""),
            " maps onto the highest cell of the table."
          )
        ),
        math_block("M_{\\cdot} \\;=\\; \\dfrac{1}{N}\\sum_{j=1}^{k} n_j M_j"),
        plotOutput("anova_step3_plot", height = "320px"),
        div(class = "p-3 mt-2",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("k (groups)", LAB7$k)),
              column(3, stat_card("N (total states)",  LAB7$N)),
              column(3, stat_card("Grand mean",
                                  sprintf("%.2f", LAB7$M_grand), key = TRUE)),
              column(3, "")),
            fluidRow(
              column(3, stat_card("M_NE", sprintf("%.2f", LAB7$M_grp["Northeast"]))),
              column(3, stat_card("M_MW", sprintf("%.2f", LAB7$M_grp["Midwest"]))),
              column(3, stat_card("M_S",  sprintf("%.2f", LAB7$M_grp["South"]))),
              column(3, stat_card("M_W",  sprintf("%.2f", LAB7$M_grp["West"]))))),
        id_prefix = "anova-step"
      ),

      # ------ STEP 4 -------------------------------------------------------
      step_container(
        4, "The big idea: total variation splits into between + within",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "The total sum of squares can be partitioned exactly into two ",
            "additive pieces. The first is ",
            math_inline("\\text{SS}_{\\text{total}} = \\sum_{i,j}(x_{ij} - M_{\\cdot})^2"),
            ", which captures total variability of all observations around ",
            "the grand mean. The between-groups component is ",
            math_inline("\\text{SS}_{\\text{between}} = \\sum_{j} n_j (M_j - M_{\\cdot})^2"),
            ", which captures how far each group mean sits from the grand ",
            "mean, weighted by group sample size. The within-groups ",
            "component is ",
            math_inline("\\text{SS}_{\\text{within}} = \\sum_{i,j}(x_{ij} - M_j)^2"),
            ", which captures how scattered the observations are around ",
            "their own group means. The two components add to the total ",
            "exactly: ",
            math_inline("\\text{SS}_{\\text{total}} = \\text{SS}_{\\text{between}} + \\text{SS}_{\\text{within}}"),
            "."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Every observation in the dataset deviates from the grand mean ",
            "by some amount. That deviation can be split into two pieces. ",
            "Part of it is the gap between the observation's group mean and ",
            "the grand mean, which we attribute to whatever is different ",
            "about each treatment condition. The rest of it is the gap ",
            "between the observation and its own group mean, which we ",
            "attribute to noise within the condition. Total variability ",
            "in the dataset is the sum of these two pieces, and the test ",
            "statistic in Step 6 asks which piece is doing the heavy lifting."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            "For the mood-map study, the SS values are ",
            sprintf("SS_total = %.1f, SS_between = %.1f, and SS_within = %.1f.",
                    LAB7$SS_total, LAB7$SS_between, LAB7$SS_within),
            sprintf(" The partitioning identity holds (%.1f + %.1f = %.1f), as it always does.",
                    LAB7$SS_between, LAB7$SS_within, LAB7$SS_total),
            " Most of the total variability in the mood-map study's data lives in the ",
            "between-groups component, which is the first hint that the ",
            "regional label is doing real work."
          )
        ),
        math_block("\\text{SS}_{\\text{total}} \\;=\\; \\text{SS}_{\\text{between}} \\;+\\; \\text{SS}_{\\text{within}}"),
        plotOutput("anova_step4_plot", height = "320px"),
        callout_warm(
          tags$b("Why the partition matters. "),
          "If H₀ holds, then between-group variability is just more noise ",
          "of the same kind as within-group noise. After we normalize each ",
          "piece by its degrees of freedom (Step 5), the two normalized ",
          "values should sit at roughly the same magnitude. If H₀ does not ",
          "hold and the treatment is producing real differences in average ",
          "outcomes, the group means get pushed apart, the between-groups ",
          "SS balloons, and the normalized between value pulls away from ",
          "the normalized within value. The F-ratio is constructed to ",
          "detect exactly this kind of imbalance."
        ),
        id_prefix = "anova-step"
      ),

      # ------ STEP 5 -------------------------------------------------------
      step_container(
        5, "Mean squares: variance estimates from each piece",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "A mean square is a sum of squares divided by its degrees of ",
            "freedom. The between-groups mean square is ",
            math_inline("\\text{MS}_{\\text{between}} = \\text{SS}_{\\text{between}}/(k - 1)"),
            ", and the within-groups mean square is ",
            math_inline("\\text{MS}_{\\text{within}} = \\text{SS}_{\\text{within}}/(N - k)"),
            ". Each of these can be interpreted as a variance estimate. ",
            "Indeed, the formula ", math_inline("\\text{MS} = \\text{SS}/df"),
            " is structurally identical to the formula ",
            math_inline("s^2 = \\text{SS}/(n-1)"),
            " we have been using throughout the course."
          ),
          example = tagList(
            tags$b("In human words. "),
            "A mean square is a variance, with the spread computed per ",
            "degree of freedom available. MS_between tells us roughly how ",
            "much the group means scatter around the grand mean. MS_within ",
            "tells us roughly how much the individual scores scatter around ",
            "their own group means. Both are measured in the same squared ",
            "units, which is what allows us to take their ratio in the ",
            "next step."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            sprintf("For the mood-map study, df_between = k − 1 = %d and df_within = N − k = %d. ",
                    LAB7$df_b, LAB7$df_w),
            sprintf("That gives MS_between = %.1f/%d = %.2f and MS_within = %.1f/%d = %.2f.",
                    LAB7$SS_between, LAB7$df_b, LAB7$MS_b,
                    LAB7$SS_within, LAB7$df_w, LAB7$MS_w),
            " The between-groups mean square is much larger than the ",
            "within-groups mean square, which is the second hint (after ",
            "the SS partition) that the regional label is doing real work."
          )
        ),
        math_block("\\text{MS}_{\\text{between}} \\;=\\; \\dfrac{\\text{SS}_{\\text{between}}}{k - 1}, \\qquad \\text{MS}_{\\text{within}} \\;=\\; \\dfrac{\\text{SS}_{\\text{within}}}{N - k}"),
        div(class = "p-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            div(class = "table-responsive",
                tags$table(class = "table table-sm mb-0",
                  tags$thead(tags$tr(
                    tags$th("Source"), tags$th("SS"), tags$th("df"),
                    tags$th("MS"), tags$th("F"))),
                  tags$tbody(
                    tags$tr(
                      tags$td("Between groups"),
                      tags$td(sprintf("%.1f", LAB7$SS_between)),
                      tags$td(LAB7$df_b),
                      tags$td(sprintf("%.2f", LAB7$MS_b)),
                      tags$td(tags$b(sprintf("%.2f", LAB7$F)))),
                    tags$tr(
                      tags$td("Within groups (error)"),
                      tags$td(sprintf("%.1f", LAB7$SS_within)),
                      tags$td(LAB7$df_w),
                      tags$td(sprintf("%.2f", LAB7$MS_w)),
                      tags$td("")),
                    tags$tr(style = "border-top: 2px solid #aaa;",
                      tags$td(tags$b("Total")),
                      tags$td(tags$b(sprintf("%.1f", LAB7$SS_total))),
                      tags$td(tags$b(LAB7$N - 1)),
                      tags$td(""), tags$td(""))
                  )))),
        callout_warm(
          tags$b("Both mean squares estimate the same kind of thing. "),
          "Under H₀, both mean squares estimate the same population variance ",
          "(the within-group noise). Under H₁, MS_between picks up an ",
          "additional component on top of that shared noise, coming from ",
          "the real differences between treatment populations. The F-ratio ",
          "in the next step compares these two estimates so that any extra ",
          "component in MS_between shows up as a deviation from 1."
        ),
        id_prefix = "anova-step"
      ),

      # ------ STEP 6 -------------------------------------------------------
      step_container(
        6, "Putting it together: the F-statistic",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "The F-statistic is the ratio of between-group variance to ",
            "within-group variance: ",
            math_inline("F = \\dfrac{\\text{MS}_{\\text{between}}}{\\text{MS}_{\\text{within}}}"),
            ". Under H₀, F follows an F-distribution with degrees of ",
            "freedom ", math_inline("(k - 1,\\ N - k)"),
            ". The F-distribution is non-negative and right-skewed."
          ),
          example = tagList(
            tags$b("In human words. "),
            "F is between-group spread divided by within-group spread. ",
            "When the groups have the same average, both pieces estimate the same ",
            "underlying noise, so F sits near 1 (with some sampling wiggle ",
            "around that center). When the treatments push the group means ",
            "apart from one another, the numerator inflates while the ",
            "denominator stays put, so F grows. Larger F values are stronger ",
            "evidence against H₀."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            sprintf("Plugging the mood-map study's numbers in gives F = %.2f / %.2f \\approx %.2f. ",
                    LAB7$MS_b, LAB7$MS_w, LAB7$F),
            "That value is more than twenty times the no-effect baseline ",
            "of 1, which already signals a sizable treatment effect even ",
            "before we look up the critical value."
          )
        ),
        math_block("F \\;=\\; \\dfrac{\\text{MS}_{\\text{between}}}{\\text{MS}_{\\text{within}}} \\;=\\; \\dfrac{\\text{signal (treatment + noise)}}{\\text{noise alone}}, \\quad df = (k-1,\\ N-k)"),
        div(class = "row g-3 my-3",
            div(class = "col-md-6",
                div(class = "p-3 h-100",
                    style = sprintf("background:%s; color:%s; border-radius:8px;",
                                    PAL$info_bg, PAL$info_fg),
                    tags$b("Why F = 1 is the no-effect baseline."),
                    p(style = "margin-bottom:0;",
                      "When H₀ is true, MS_between and MS_within both ",
                      "estimate the same within-group variance, so their ",
                      "ratio is a noisy estimate of one. An F-statistic ",
                      "near 1 is therefore unremarkable. An F-statistic ",
                      "much greater than 1 is evidence against H₀, with ",
                      "larger values constituting stronger evidence."))),
            div(class = "col-md-6",
                div(class = "p-3 h-100",
                    style = sprintf("background:%s; color:%s; border-radius:8px;",
                                    PAL$ok_bg, PAL$ok_fg),
                    tags$b("Connection to the t-test."),
                    p(style = "margin-bottom:0;",
                      "When there are exactly two groups, the F-statistic ",
                      "from a one-way ANOVA equals the square of the ",
                      "t-statistic from an independent-samples t-test (",
                      math_inline("F = t^2"),
                      "), and the two procedures yield identical p-values. ",
                      "The ANOVA can be read as a natural generalization of ",
                      "the t-test to three or more groups, with the same ",
                      "underlying signal-divided-by-noise logic.")))),
        callout_warm(
          tags$b("One asymmetry worth flagging. "),
          "F is bounded below by zero (variance estimates can never be ",
          "negative) and is right-skewed, with most of its mass piled up ",
          "near 1 and a long tail extending out to the right. The ",
          "rejection region for an F-test therefore lives entirely in the ",
          "right tail of the distribution, even when the underlying ",
          "hypothesis is two-sided. Small values of F do not lead to ",
          "rejection."
        ),
        id_prefix = "anova-step"
      ),

      # ------ STEP 7 -------------------------------------------------------
      step_container(
        7, "Setting alpha",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "Alpha is set to .05 in PSY 302, following standard convention. ",
            "F-tests are right-tailed, so the entire α-budget is allocated ",
            "to the upper tail of the F-distribution."
          ),
          example = tagList(
            tags$b("In human words. "),
            "The F-test does not have a two-tailed version. Values of F ",
            "below 1 do not lead to rejection of H₀, since they correspond ",
            "to between-group spread being smaller than within-group ",
            "spread, which is the opposite of what evidence against H₀ ",
            "would look like. The 5% rejection region therefore sits ",
            "entirely in the upper tail of the distribution."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            "the mood-map study uses α = .05, right-tailed, the PSY 302 default."
          )
        ),
        math_block("\\Pr(F \\geq F_{\\text{crit}} \\mid H_0) \\;=\\; \\alpha"),
        id_prefix = "anova-step"
      ),

      # ------ STEP 8 -------------------------------------------------------
      step_container(
        8, "Critical F value, df = (\\(k-1,\\ N-k\\))",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "The critical F value bounding the rejection region is ",
            math_inline("F_{\\text{crit}} = F_{1-\\alpha,\\ df_b,\\ df_w}"),
            ", which can be read from a two-way F-table indexed by ",
            "numerator df ", math_inline("(df_b)"),
            " and denominator df ", math_inline("(df_w)"),
            ", or computed in R with ",
            math_inline("\\texttt{qf}"), "."
          ),
          example = tagList(
            tags$b("In human words. "),
            "F-crit is the right-tail cutoff. Observed F values past this ",
            "cutoff fall into the rejection region. Larger samples produce ",
            "smaller cutoffs, and larger numerator df also lowers the ",
            "cutoff, because additional groups provide more opportunities ",
            "for genuine differences in means to register."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            sprintf("With df = (%d, %d) and α = .05, F_crit ≈ %.2f.",
                    LAB7$df_b, LAB7$df_w, LAB7$F_cv)
          )
        ),
        math_block("F_{\\text{crit}} \\;=\\; F_{1-\\alpha,\\ df_b,\\ df_w}"),
        plotOutput("anova_step8_plot", height = "260px"),
        id_prefix = "anova-step"
      ),

      # ------ STEP 9 -------------------------------------------------------
      step_container(
        9, "Decision + post-hoc (Tukey HSD)",
        explanation_triad(
          formal = tagList(
            tags$b("Academic. "),
            "We reject ", math_inline("H_0"), " when ",
            math_inline("F \\geq F_{\\text{crit}}"),
            ". When H₀ is rejected, a post-hoc procedure is needed to ",
            "identify which group pairs differ. Tukey's HSD is the ",
            "standard choice in PSY 302. It uses the residual MS_within ",
            "as the noise estimate while controlling the family-wise error ",
            "rate across all pairs."
          ),
          example = tagList(
            tags$b("In human words. "),
            "Rejecting the omnibus H₀ tells us that some pair of group ",
            "means differs. It does not tell us which pair, or how many. ",
            "Tukey HSD runs the pairwise comparisons with the appropriate ",
            "correction built in, so that we can identify the significant ",
            "pairs without inflating the family-wise error rate back up to ",
            "the value the omnibus test was specifically designed to keep ",
            "in check."
          ),
          tldr = tagList(
            tags$b("Case study (mood map). "),
            sprintf("The mood-map study yielded F(%d, %d) = %.2f, which comfortably exceeds the critical value of F_crit ≈ %.2f. We therefore ",
                    LAB7$df_b, LAB7$df_w, LAB7$F, LAB7$F_cv),
            tags$b("reject H₀"),
            ", with p < .001. The omnibus result tells us some pair of ",
            "regional means differs, but a follow-up Tukey HSD test is ",
            "needed to identify which. The table below summarizes the six ",
            "pairwise comparisons. The Northeast differs significantly from ",
            "every other region. The Midwest and South do not differ from ",
            "one another reliably, but both differ from the Northeast and ",
            "the West. The West differs significantly from the Northeast, ",
            "the Midwest, and the South."
          )
        ),
        math_block("\\text{Reject } H_0 \\;\\Longleftrightarrow\\; F \\geq F_{\\text{crit}}"),
        div(class = "p-3 mt-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("Your F", sprintf("%.2f", LAB7$F))),
              column(3, stat_card("F_crit", sprintf("%.2f", LAB7$F_cv))),
              column(3, stat_card("df", sprintf("(%d, %d)", LAB7$df_b, LAB7$df_w))),
              column(3, stat_card("p-value",
                                  if (LAB7$p < 0.001) "< 0.001" else sprintf("%.3f", LAB7$p)))),
            div(class = "p-3 mt-3",
                style = sprintf("background:%s; color:%s; border-radius:8px; font-weight:600; font-size:18px;",
                                PAL$ok_bg, PAL$ok_fg),
                "Reject H₀",
                tags$br(),
                tags$span(style = "font-weight:400; font-size:14px;",
                          sprintf("F = %.2f ≥ F_crit = %.2f. The four regions differ on state-level neuroticism. Tukey HSD pinpoints which pairs are driving the omnibus result.",
                                  LAB7$F, LAB7$F_cv)))),
        plotOutput("anova_step9_plot", height = "300px"),

        # Tukey HSD-style pairwise summary table
        div(class = "p-3 mt-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            h5("Pairwise post-hoc comparisons", style = "margin-top:0;"),
            p(style = "color:#666; font-size:13px;",
              "Each row is one pair of regions. The t-statistic uses the ",
              "pooled MS_within from the omnibus ANOVA as the noise ",
              "estimate. The values below are simple pairwise t-tests, ",
              "not the studentized-range Tukey correction; the ranking of ",
              "pairs is essentially the same."),
            div(class = "table-responsive",
                tags$table(class = "table table-sm mb-0",
                  tags$thead(tags$tr(
                    tags$th("Pair"), tags$th("M_diff"),
                    tags$th("t(45)"), tags$th("p-value"),
                    tags$th("Decision (α = .05)"))),
                  tags$tbody(
                    tags$tr(tags$td("Northeast vs West"),
                            tags$td("19.30"), tags$td("5.51"),
                            tags$td("< .001"), tags$td("Reject")),
                    tags$tr(tags$td("Northeast vs Midwest"),
                            tags$td("12.30"), tags$td("3.51"),
                            tags$td(".001"), tags$td("Reject")),
                    tags$tr(tags$td("Northeast vs South"),
                            tags$td("9.68"), tags$td("2.92"),
                            tags$td(".005"), tags$td("Reject")),
                    tags$tr(tags$td("South vs West"),
                            tags$td("9.62"), tags$td("3.18"),
                            tags$td(".003"), tags$td("Reject")),
                    tags$tr(tags$td("Midwest vs West"),
                            tags$td("7.00"), tags$td("2.16"),
                            tags$td(".036"), tags$td("Reject")),
                    tags$tr(tags$td("Midwest vs South"),
                            tags$td("−2.62"), tags$td("−0.86"),
                            tags$td(".390"), tags$td("Fail to reject")))))),
        id_prefix = "anova-step"
      ),

      # ---- APA card -------------------------------------------------------
      div(class = "p-3 mt-4",
          style = sprintf("background:%s; color:%s; border-radius:10px; border-left:5px solid %s;",
                          PAL$warn_bg, PAL$warn_fg, OI$orange),
          h5("Translating numbers into APA-style prose", style = "margin-top:0;"),
          tags$blockquote(style = "font-style:italic; margin-left:8px; border-left:4px solid #d6c08a; padding-left:12px;",
            "Using a between-subjects one-way ANOVA on state-level NEO ",
            "neuroticism scores, we found that the overall effect of U.S. ",
            "Census region was significant, F(3, 45) = 10.38, p < .001. ",
            "Pairwise comparisons revealed that the Northeast (M = 60.91, ",
            "SD = 5.22) scored higher than the Midwest (M = 48.61, SD = ",
            "6.42), the South (M = 51.23, SD = 10.64), and the West (M = ",
            "41.61, SD = 6.53), all ps ≤ .005. The West scored significantly ",
            "lower than every other region (all ps < .05). The Midwest and ",
            "South did not differ from one another (p = .390)."),
          p(style = "margin-bottom:0;",
            tags$b("Template: "),
            "Using a [test], the overall effect of [IV] on [DV] was ",
            "[significant / non-significant], F(df_b, df_w) = , p = . ",
            "Tukey HSD: [pairwise comparisons with t, p].")
      ),

      # ===================================================================
      # Tutorial finale: build intuition, then unlock practice quest
      # ===================================================================
      div(class = "mt-5 mb-3 p-4",
          style = sprintf("background:#E1F1FB; border-left:6px solid %s; border-radius:10px;",
                          PAL$pop_dark),
          h3("Now play with the machinery", style = "margin-top:0;"),
          p("Before you move on to the practice quest, picture how this ",
            "mood-map result would shift if the inputs changed. ANOVA's ",
            "moving parts are the group means, the within-group SDs, the ",
            "group sample sizes, and the number of groups. Each one ",
            "leverages the F-statistic in its own way."),
          tags$ul(style = "margin-bottom:0; padding-left:20px; line-height:1.7;",
            tags$li(tags$b("Bigger n in every group. "),
              "Imagine each region had 100 states instead of 9–16. ",
              "MS_within would be estimated more precisely, df_w would ",
              "grow, the F-critical value would shrink, and the same ",
              "regional mean spread would produce a much larger F-ratio. ",
              "Larger samples make small differences easier to detect."),
            tags$li(tags$b("Bigger or smaller within-group SD. "),
              "Imagine each region's states scattered more wildly. ",
              "MS_within balloons, the same MS_between produces a ",
              "smaller F-ratio, and the real regional effect can hide ",
              "behind within-region noise."),
            tags$li(tags$b("Group means closer together. "),
              "Suppose the Northeast mean were 55 instead of 60.91, and ",
              "the West mean 45 instead of 41.61. MS_between would ",
              "shrink, F would shrink, and a clear effect could become ",
              "ambiguous."),
            tags$li(tags$b("More groups (larger k). "),
              "Imagine the same analysis with k = 9 instead of k = 4. ",
              "df_b grows, but each pairwise comparison becomes harder ",
              "to detect under the family-wise error correction. The ",
              "Step 2 curve makes this concrete: every added group adds ",
              "more pairwise comparisons, and the omnibus F has to ",
              "shoulder more of the work."),
            tags$li(tags$b("Read the source table left to right. "),
              "Each row in Step 5's source table is one ingredient. SS ",
              "is what you partition. df is what you divide by. MS is ",
              "the variance estimate. F is the ratio of MS_between to ",
              "MS_within. Trace the ingredients horizontally to make ",
              "sure each step's role is clear."))
      ),
      div(class = "text-center mb-4",
          p(style = "color:#444; font-size:14px;",
            "When the moving parts feel intuitive, unlock the practice ",
            "quest below."),
          actionButton("tab4_show_quest",
                       "Show me the practice quest →",
                       class = "btn-primary btn-lg")),

      # ===================================================================
      # QUEST. Life satisfaction across world regions (Cantril Ladder)
      # ===================================================================
      conditionalPanel(condition = "input.tab4_show_quest > 0",
      quest_section(
        title = "Does life satisfaction (the Cantril Ladder) differ across world regions?",
        id_prefix = "tab4quest",
        scenario_html = tagList(
          p("The Cantril Ladder is the question that drives the World ",
            "Happiness Report. Respondents are asked to picture a ladder ",
            "numbered 0 at the bottom (the worst possible life) and 10 at ",
            "the top (the best possible life) and to say where they ",
            "currently stand. The data below are fabricated ladder ",
            "responses from four world regions. Your job is to use a ",
            "one-way ANOVA at α = .05 to decide whether the regions ",
            "differ in average life satisfaction."),
          p(style = "margin-bottom:0;",
            "Use the summary statistics in the box and the source-table ",
            "formulas from the walkthrough above. Then, in Step 9, decide ",
            "what Tukey HSD adds to the picture.")
        ),
        data_block = tagList(
          tags$table(class = "table table-sm mb-0",
            tags$thead(tags$tr(
              tags$th("Region"), tags$th("n"), tags$th("M"),
              tags$th("SD"), tags$th("SS_within"))),
            tags$tbody(
              lapply(seq_along(CANTRIL$groups), function(i) tags$tr(
                tags$td(CANTRIL$group_names[i]),
                tags$td(CANTRIL$n_grp[i]),
                tags$td(sprintf("%.2f", CANTRIL$M_grp[i])),
                tags$td(sprintf("%.2f", CANTRIL$s_grp[i])),
                tags$td(sprintf("%.2f", CANTRIL$SS_grp[i])))))),
          tags$p(style = "margin-top:8px; margin-bottom:0;",
                 tags$b("Totals: "),
                 sprintf("k = %d, N = %d, grand mean = %.2f, SS_between = %.2f, SS_within = %.2f.",
                         CANTRIL$k, CANTRIL$N, CANTRIL$M_grand,
                         CANTRIL$SS_between, CANTRIL$SS_within))
        ),
        questions = list(
          list(
            prompt = tagList(tags$b("Step 1. "),
              "State the omnibus H₀ and H₁ for the Cantril ladder study."),
            solution = tagList(
              p(math_inline("H_0: \\mu_1 = \\mu_2 = \\mu_3 = \\mu_4"),
                ". The four regions have the same average ladder rating."),
              p(math_inline("H_1: \\text{at least one pair of }\\mu\\text{'s differs}"),
                ". Some pair of regional means differs from another. The ",
                "test does not specify which pair."))
          ),
          list(
            prompt = tagList(tags$b("Step 2. "),
              "Why would running three pairwise t-tests in place of one ",
              "ANOMA be a bad idea here?"),
            solution = tagList(
              p("With four groups there are six possible pairwise ",
                "comparisons. Running each at α = .05 inflates the ",
                "family-wise Type I error rate to roughly ",
                math_inline("1 - (1-.05)^6 \\approx 0.26"),
                ", which is far higher than the .05 nominal rate. The ",
                "omnibus F-test controls all six comparisons at the ",
                "single α."))
          ),
          list(
            prompt = tagList(tags$b("Step 3. "),
              "List the four group means and compute the grand mean. ",
              "Comment on which groups are noticeably above and below the ",
              "grand mean."),
            solution = tagList(
              p(sprintf("M_Europe = %.2f, M_NAmer = %.2f, M_EAsia = %.2f, M_SSA = %.2f.",
                        CANTRIL$M_grp[1], CANTRIL$M_grp[2],
                        CANTRIL$M_grp[3], CANTRIL$M_grp[4])),
              p(sprintf("Grand mean = %.2f. Europe and North America sit above the grand mean. East Asia is slightly below it. Sub-Saharan Africa sits well below.",
                        CANTRIL$M_grand)))
          ),
          list(
            prompt = tagList(tags$b("Step 4. "),
              "Confirm that SS_total = SS_between + SS_within for this ",
              "dataset, and report each component."),
            solution = tagList(
              p(sprintf("SS_between = %.2f, SS_within = %.2f, SS_total = %.2f. The identity %.2f + %.2f = %.2f holds.",
                        CANTRIL$SS_between, CANTRIL$SS_within,
                        CANTRIL$SS_total,
                        CANTRIL$SS_between, CANTRIL$SS_within,
                        CANTRIL$SS_total)),
              p("Most of the total variability lives in the between-groups ",
                "piece, which already hints at a real regional effect."))
          ),
          list(
            prompt = tagList(tags$b("Step 5. "),
              "Compute the two mean squares."),
            solution = tagList(
              p(sprintf("df_between = k - 1 = %d. df_within = N - k = %d.",
                        CANTRIL$df_b, CANTRIL$df_w)),
              p(sprintf("MS_between = SS_between / df_between = %.2f / %d = %.2f.",
                        CANTRIL$SS_between, CANTRIL$df_b, CANTRIL$MS_b)),
              p(sprintf("MS_within = SS_within / df_within = %.2f / %d = %.2f.",
                        CANTRIL$SS_within, CANTRIL$df_w, CANTRIL$MS_w)))
          ),
          list(
            prompt = tagList(tags$b("Step 6. "),
              "Compute the F-statistic."),
            solution = tagList(
              p(sprintf("F = MS_between / MS_within = %.2f / %.2f \\approx %.2f.",
                        CANTRIL$MS_b, CANTRIL$MS_w, CANTRIL$F)),
              p("That is more than thirty times the no-effect baseline of ",
                "one, which is a clear signal that the regions do not all ",
                "have the same average ladder rating."))
          ),
          list(
            prompt = tagList(tags$b("Step 7. "),
              "What α value should we use, and why is the F-test ",
              "right-tailed even though H₁ is two-sided?"),
            solution = tagList(
              p("Use α = .05. F is bounded below by zero and right-skewed. ",
                "The rejection region lives entirely in the upper tail ",
                "because only large values of F (where between-group ",
                "variance dwarfs within-group variance) constitute evidence ",
                "against H₀."))
          ),
          list(
            prompt = tagList(tags$b("Step 8. "),
              "Find the critical F value at α = .05 with the appropriate df."),
            solution = tagList(
              p(sprintf("F_crit = F_{1-α, df_b, df_w} = F_{.95, %d, %d} ≈ %.2f.",
                        CANTRIL$df_b, CANTRIL$df_w, CANTRIL$F_cv)))
          ),
          list(
            prompt = tagList(tags$b("Step 9. "),
              "What is your decision? Write up the result in APA-style. ",
              "What additional analysis would you want to run to identify ",
              "which regions differ?"),
            solution = tagList(
              p(sprintf("F = %.2f is much larger than the critical value of %.2f, so we ",
                        CANTRIL$F, CANTRIL$F_cv),
                tags$b("reject H₀"),
                ". The corresponding p-value is well below .001."),
              p(style = "margin-bottom:0;",
                "APA-style writeup: ",
                tags$em(sprintf("\"Using a between-subjects one-way ANOVA, the overall effect of world region on Cantril Ladder ratings was significant, F(%d, %d) = %.2f, p < .001. Tukey HSD post-hoc tests should be used to identify which regional pairs differ.\"",
                                CANTRIL$df_b, CANTRIL$df_w, CANTRIL$F))))
          )
        )
      )
      )  # close conditionalPanel(tab4_show_quest)
    ))
  })

  # ---- Step 2 . familywise-error curve ------------------------------------
  output$anova_step2_plot <- renderPlot({ plot_familywise(0.05) }, res = 96)

  # ---- Step 3 . k-group dot plot ------------------------------------------
  output$anova_step3_plot <- renderPlot({
    plot_k_groups(LAB7$groups, show_grand_mean = TRUE)
  }, res = 96)

  # ---- Step 4 . SS decomposition picture -----------------------------------
  output$anova_step4_plot <- renderPlot({
    plot_ss_decomposition(LAB7$groups)
  }, res = 96)

  # ---- Step 8 . F-distribution with CV ------------------------------------
  output$anova_step8_plot <- renderPlot({
    draw_F_decision(LAB7$F, LAB7$df_b, LAB7$df_w, alpha = 0.05) +
      labs(subtitle = sprintf("F-distribution under H₀, df = (%d, %d). Orange tail = α = 0.05.",
                              LAB7$df_b, LAB7$df_w))
  }, res = 96)

  # ---- Step 9 . F-decision picture ----------------------------------------
  output$anova_step9_plot <- renderPlot({
    draw_F_decision(LAB7$F, LAB7$df_b, LAB7$df_w, alpha = 0.05) +
      labs(subtitle = sprintf("F(%d, %d) = %.2f. Past F_crit = %.2f → reject H₀.",
                              LAB7$df_b, LAB7$df_w, LAB7$F, LAB7$F_cv))
  }, res = 96)
}
