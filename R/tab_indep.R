# =============================================================================
# tab_indep.R
# Independent-samples t-test walkthrough. Daves know more Daves.
# Data inspired by Reddit user u/quoctran98 (r/SampleSize, r/dataisbeautiful)
# https://notawfulandboring.blogspot.com/2020/08/daves-know-more-daves-independent-t.html
# =============================================================================

# ---- Case-study constants: how many Daves do you know? ----------------------
# Two independent groups of survey respondents (n = 12 each). Numbers below
# are fabricated to land near the spirit of Quoc Tran's reported result:
# people who are themselves named Dave know more Daves than people who are not.
DAVES <- local({
  group1 <- c(2, 4, 5, 6, 6, 7, 8, 8, 9, 10, 11, 8)   # respondents named Dave
  group2 <- c(1, 1, 2, 3, 3, 4, 4, 4, 5, 6, 6, 9)     # respondents not named Dave
  n1 <- length(group1); n2 <- length(group2)
  M1 <- mean(group1);   M2 <- mean(group2)
  s1 <- sd(group1);     s2 <- sd(group2)
  SS1 <- sum((group1 - M1)^2); SS2 <- sum((group2 - M2)^2)
  df_v <- n1 + n2 - 2
  sp2  <- (SS1 + SS2) / df_v
  se   <- sqrt(sp2 * (1 / n1 + 1 / n2))
  t_obs <- (M1 - M2) / se
  p_val <- 2 * (1 - pt(abs(t_obs), df_v))
  d_val <- (M1 - M2) / sqrt(sp2)
  list(b1 = group1, b2 = group2, n1 = n1, n2 = n2,
       M1 = M1, M2 = M2, s1 = s1, s2 = s2,
       SS1 = SS1, SS2 = SS2, sp2 = sp2, se = se,
       t = t_obs, df = df_v, p = p_val, d = d_val,
       diff = M1 - M2)
})
# Alias kept for backwards compatibility with existing references in this file.
LAB5 <- DAVES

# ---- Quest dataset: probiotics for antibiotic-induced GI problems -----------
# A two-group clinical-trial-style dataset. Each child takes a 14-day course
# of antibiotics with either a probiotic-laced yogurt or a placebo yogurt.
# DV = number of days during the course with reported GI symptoms.
PROBIOTIC <- local({
  probiotic <- c(0, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 0, 1, 3, 5)
  placebo   <- c(3, 4, 4, 5, 5, 6, 6, 6, 7, 4, 3, 2, 5, 8, 4)
  n1 <- length(probiotic); n2 <- length(placebo)
  M1 <- mean(probiotic);    M2 <- mean(placebo)
  s1 <- sd(probiotic);      s2 <- sd(placebo)
  SS1 <- sum((probiotic - M1)^2); SS2 <- sum((placebo - M2)^2)
  df_v <- n1 + n2 - 2
  sp2  <- (SS1 + SS2) / df_v
  se   <- sqrt(sp2 * (1 / n1 + 1 / n2))
  t_obs <- (M1 - M2) / se
  p_val <- 2 * (1 - pt(abs(t_obs), df_v))
  d_val <- (M1 - M2) / sqrt(sp2)
  list(prob = probiotic, plac = placebo, n1 = n1, n2 = n2,
       M1 = M1, M2 = M2, s1 = s1, s2 = s2,
       SS1 = SS1, SS2 = SS2, sp2 = sp2, se = se,
       t = t_obs, df = df_v, p = p_val, d = d_val,
       diff = M1 - M2)
})

# ---- Plot helpers (independent-tab-specific) -------------------------------
plot_two_groups <- function(g1, g2, name1, name2) {
  long <- data.frame(
    group = factor(c(rep(name1, length(g1)), rep(name2, length(g2))),
                   levels = c(name1, name2)),
    score = c(g1, g2)
  )
  M1 <- mean(g1); M2 <- mean(g2)
  ggplot(long, aes(x = group, y = score, colour = group)) +
    geom_jitter(width = 0.18, height = 0, size = 3, alpha = 0.85) +
    annotate("segment", x = 0.7, xend = 1.3, y = M1, yend = M1,
             colour = PAL$pop_dark, linewidth = 1.4) +
    annotate("segment", x = 1.7, xend = 2.3, y = M2, yend = M2,
             colour = PAL$samp_pt, linewidth = 1.4) +
    annotate("label", x = 1, y = M1,
             label = sprintf("M_1 = %.2f", M1),
             colour = PAL$pop_dark, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4, fontface = "bold", vjust = -0.7) +
    annotate("label", x = 2, y = M2,
             label = sprintf("M_2 = %.2f", M2),
             colour = PAL$samp_pt, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4, fontface = "bold", vjust = -0.7) +
    scale_colour_manual(values = setNames(c(PAL$pop_dark, PAL$samp_pt),
                                          c(name1, name2))) +
    labs(x = NULL, y = "Number of Daves known",
         subtitle = "Each dot is one respondent. Horizontal bars are group means.") +
    base_theme() +
    theme(legend.position = "none")
}

# Sampling distribution of (M1 - M2) under H₀
plot_diff_means_sampling <- function(diff_obs, se, df_v, mu_diff_null = 0) {
  xr <- c(min(mu_diff_null, diff_obs) - 4 * se,
          max(mu_diff_null, diff_obs) + 4 * se)
  x  <- seq(xr[1], xr[2], length.out = 400)
  y  <- dt((x - mu_diff_null) / se, df_v) / se
  cd <- data.frame(x = x, y = y)
  max_y <- max(cd$y)
  ggplot(cd, aes(x, y)) +
    geom_area(fill = PAL$pop_med, alpha = 0.2) +
    geom_line(colour = PAL$pop_dark, linewidth = 0.7) +
    geom_vline(xintercept = mu_diff_null, colour = PAL$pop_dark,
               linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = diff_obs, colour = PAL$samp_line, linewidth = 1.1) +
    annotate("label", x = mu_diff_null, y = max_y * 1.08,
             label = "Under H₀: μ_1 − μ_2 = 0\n(no real group difference)",
             colour = PAL$pop_dark, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 3.6, lineheight = 0.9) +
    annotate("label", x = diff_obs, y = max_y * 0.6,
             label = sprintf("Our (M_1 − M_2) = %.2f", diff_obs),
             colour = PAL$samp_line, fill = "white", label.size = NA,
             label.r = unit(0.15, "lines"), alpha = 0.95,
             size = 4.2, fontface = "bold") +
    scale_y_continuous(limits = c(0, max_y * 1.25)) +
    labs(x = "Possible (M_1 − M_2)",
         y = "Density",
         subtitle = sprintf(
           "Sampling distribution of the difference between means under H₀: centered at 0, SE = %.3f, df = %d.",
           se, df_v)) +
    base_theme()
}

# ---- Server entry point -----------------------------------------------------
source_indep_server <- function(input, output, session) {

  output$indep_tab_ui <- renderUI({
    withMathJax(div(class = "container-fluid", style = "max-width: 1240px; padding-top: 16px;",

      scenario_card(
        "Build an independent-samples t-test",
        lab_label = "Running example: do Daves know more Daves?",
        p(style = "margin-bottom:6px;",
          "The independent-samples t-test is what we reach for when we have ",
          tags$b("two separate groups"),
          " of different subjects (A versus B, treatment versus control, ",
          "extraverts versus introverts). The two groups are independent ",
          "of one another. Each subject contributes a measurement to one ",
          "group and only one group. The test is set up to evaluate whether ",
          "the two groups' means come from populations with the same average."),
        p(style = "margin-bottom:6px;",
          tags$b("Running example. "),
          "A Reddit user (u/quoctran98) noticed that the people he knew named ",
          "Dave seemed to be unusually well-connected to other Daves, and he ",
          "wondered whether this was true more generally. He posted a survey ",
          "on r/SampleSize asking respondents how many people they know who ",
          "are named Dave. Twelve respondents in each group (Daves and ",
          "non-Daves) provide the data for our walkthrough below. Sample ",
          "statistics: ",
          math_inline("M_1 = 7.00"), " for Daves, ",
          math_inline("M_2 = 4.00"), " for non-Daves, with ",
          math_inline("t(22) = 3.02"), ", p = .006, d = 1.23."),
        p(style = "margin-bottom:0;",
          tags$b("How this differs from Tab 2. "),
          "The paired test in Tab 2 pulled both measurements from the same ",
          "subject. The independent test in this tab pulls each measurement ",
          "from a different subject, so we now have ", tags$em("two"),
          " separate sources of sampling noise (one from each group). The ",
          "denominator of the test statistic combines both. The sampling ",
          "distribution that organizes the whole procedure is also ",
          "different. It describes the spread of the ",
          tags$em("difference between two sample means"),
          ", or ", math_inline("M_1 - M_2"),
          ", under repeated sampling.")
      ),

      # ------ STEP 1 -------------------------------------------------------
      step_container(
        1, "A world where the groups do not differ: \\(H_0:\\ \\mu_1 = \\mu_2\\)",
        explanation_triad(
          formal = tagList(
            "Consider two independent populations with means ",
            math_inline("\\mu_1"), " and ", math_inline("\\mu_2"),
            " and common variance ", math_inline("\\sigma^2"),
            ". The null hypothesis specifies that the two population means ",
            "are equal (", math_inline("H_0: \\mu_1 = \\mu_2"),
            "), which is equivalent to the statement that the difference ",
            "between them is zero (", math_inline("\\mu_1 - \\mu_2 = 0"),
            "). The corresponding two-tailed alternative is ",
            math_inline("\\mu_1 - \\mu_2 \\neq 0"), "."
          ),
          example = tagList(
            "Picture a world where the two groups secretly come from ",
            "populations with the same average. Even in that world, the ",
            "particular samples we happen to draw will produce two means ",
            "that differ a little, because every sample is random and ",
            "no two are alike. The test asks how much of a gap between ",
            "the two sample means is too large to attribute to that kind ",
            "of routine sample-to-sample variation."
          ),
          tldr = tagList(
            "For the Daves study, H₀ says that the population of Daves and ",
            "the population of non-Daves know the same average number of ",
            "people named Dave (whatever quirk of social networks would ",
            "make Dave-density vary by personal name does not exist). ",
            "H₁ says the two population averages differ. The data will ",
            "tell us which of these worlds is more plausible."
          )
        ),
        math_block("H_0:\\ \\mu_1 - \\mu_2 = 0 \\qquad H_1:\\ \\mu_1 - \\mu_2 \\neq 0"),
        div(class = "p-3 my-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            h5("The case-study numbers for this step", style = "margin-top:0;"),
            p(style = "color:#666; font-size:13px;",
              "The independent-samples test reasons about two populations ",
              "at once. Under H₀, the two populations have the same average."),
            fluidRow(
              column(4, stat_card("μ_1 (Daves' population mean)",
                                  "unknown; estimated as M_1 = 7.00")),
              column(4, stat_card("μ_2 (non-Daves' population mean)",
                                  "unknown; estimated as M_2 = 4.00")),
              column(4, stat_card("μ_1 − μ_2 (null gap)",
                                  "0 under H₀", key = TRUE)))),
        callout_warm(
          tags$b("Two sources of noise instead of one. "),
          "The single-sample t-test in Tab 1 had one source of sampling ",
          "noise to worry about. The independent-samples t-test has two. ",
          "Both group means wiggle from study to study, and the test ",
          "statistic has to take both wiggles into account in the ",
          "denominator. The pooled-variance formula in Step 5 is how the ",
          "machinery does that combining."
        ),
        id_prefix = "indep-step"
      ),

      # ------ STEP 2 -------------------------------------------------------
      step_container(
        2, "Run one study: two samples, two means, one gap",
        explanation_triad(
          formal = tagList(
            "We draw ", math_inline("n_1"),
            " observations from population 1 and ", math_inline("n_2"),
            " observations from population 2, independently of one another. ",
            "From those samples we compute ",
            math_inline("M_1, M_2, \\text{SD}_1"), ", and ", math_inline("\\text{SD}_2"),
            " using the same formulas we have been using all along. The ",
            tags$b("signal"), " of interest is the gap between the two ",
            "sample means, written ", math_inline("M_1 - M_2"), "."
          ),
          example = tagList(
            "Survey twelve respondents in each group, count the Daves each ",
            "person knows, and average within each group. We end up with ",
            "two group averages, and we subtract them to get a single ",
            "number summarizing how the two groups compare in our particular ",
            "sample. That number is what the test evaluates."
          ),
          tldr = tagList(
            "Doing the arithmetic on the Daves study's data: Daves gives ",
            math_inline("M_1 = 7.00"), " with ",
            math_inline("\\text{SD}_1 = 2.56"),
            ", and non-Daves gives ", math_inline("M_2 = 4.00"),
            " with ", math_inline("\\text{SD}_2 = 2.30"),
            ". The observed gap is ",
            math_inline("M_1 - M_2 = 3.00"),
            " Daves, with Daves knowing more Daves on average."
          )
        ),
        math_block("\\text{signal} \\;=\\; M_1 - M_2"),
        plotOutput("indep_step2_plot", height = "320px"),
        div(class = "p-3 mt-2",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("n_1", LAB5$n1)),
              column(3, stat_card("M_1", sprintf("%.2f", LAB5$M1))),
              column(3, stat_card("SD_1", sprintf("%.2f", LAB5$s1))),
              column(3, stat_card("SS_1", sprintf("%.2f", LAB5$SS1)))),
            fluidRow(
              column(3, stat_card("n_2", LAB5$n2)),
              column(3, stat_card("M_2", sprintf("%.2f", LAB5$M2))),
              column(3, stat_card("SD_2", sprintf("%.2f", LAB5$s2))),
              column(3, stat_card("SS_2", sprintf("%.2f", LAB5$SS2)))),
            fluidRow(
              column(3, stat_card("M_1 − M_2",
                                  sprintf("%.2f", LAB5$diff), key = TRUE)),
              column(9, ""))),
        callout_warm(
          tags$b("Three distributions to keep apart. "),
          "The cluster of blue dots in the plot above is one sample ",
          "distribution (the Dave counts for the Daves group). The cluster ",
          "of green dots is a second sample distribution (the Dave counts ",
          "for the non-Daves group). Each has its own spread, ",
          math_inline("\\text{SD}_1"), " and ", math_inline("\\text{SD}_2"),
          " respectively, which together feed into the noise estimate. A ",
          "third distribution shows up in Step 4: the sampling distribution ",
          "of the difference between two sample means. That third ",
          "distribution is a different object from either of the first two, ",
          "and it is the one the t-statistic is referenced against."
        ),
        id_prefix = "indep-step"
      ),

      # ------ STEP 3 -------------------------------------------------------
      step_container(
        3, "Run it again: sampling variability of \\(M_1 - M_2\\)",
        explanation_triad(
          formal = tagList(
            "Independent replications of the same study draw fresh samples ",
            "from each of the two populations, producing different pairs ",
            "of sample means (", math_inline("M_1, M_2"),
            ") and therefore different gaps ",
            math_inline("M_1 - M_2"),
            ". The dispersion of those gaps across infinitely many ",
            "replications is referred to as the sampling variability of ",
            "the difference between sample means."
          ),
          example = tagList(
            "Picture 100 separate research groups each ",
            "running the Daves study with twelve fresh respondents in each group. They would ",
            "produce 100 slightly different gaps between the two group ",
            "averages, centered on the true difference but scattered around ",
            "it. The rest of this tab works with the shape of that scatter."
          ),
          tldr = tagList(
            "We can build intuition about this by simulating replications. ",
            "The buttons below assume the groups really do differ by the Daves study's ",
            "observed gap on average, draw fresh samples from each ",
            "population, compute each new ", math_inline("M_1 - M_2"),
            ", and add it to the growing cloud of possible outcomes."
          )
        ),
        div(style = "display:flex; gap:8px; flex-wrap:wrap;",
            actionButton("indep_repeat_one",   "Run another study",  class = "btn-primary"),
            actionButton("indep_repeat_five",  "Run 5 more",         class = "btn-primary"),
            actionButton("indep_repeat_reset", "Reset",              class = "btn-outline-secondary")),
        plotOutput("indep_step3_plot", height = "280px"),
        uiOutput("indep_step3_counter"),
        callout_warm(
          tags$b("What changes from Tab 1. "),
          "In the single-sample tab we were building the sampling ",
          "distribution of ", math_inline("M"),
          ", a single mean. Here we are building the sampling distribution ",
          "of ", math_inline("M_1 - M_2"),
          ", the gap between two means. The underlying idea is the same ",
          "(variation across imagined replications of a study), but the ",
          "random quantity being tracked is now a difference rather than a ",
          "single mean. That changes the formula for the standard error, ",
          "which Step 5 works out."
        ),
        id_prefix = "indep-step"
      ),

      # ------ STEP 4 -------------------------------------------------------
      step_container(
        4, "If we ran it many times: the sampling distribution of \\(M_1 - M_2\\)",
        explanation_triad(
          formal = tagList(
            "Under H₀, the random quantity ",
            math_inline("M_1 - M_2"),
            " has mean zero and standard deviation ",
            math_inline("\\sqrt{\\sigma_1^2/n_1 + \\sigma_2^2/n_2}"),
            ". The Central Limit Theorem guarantees that, for sufficient ",
            "sample sizes, the sampling distribution of the difference ",
            "approaches a normal curve, and the procedure we use here is ",
            "an approximation to that limiting distribution using a ",
            "t-distribution that accounts for our needing to estimate the ",
            "two SDs from the data."
          ),
          example = tagList(
            "Picture the world in which the two groups have identical ",
            "average Dave counts. Even in that world, sample-to-sample luck ",
            "produces gaps between the two sample means that bounce around ",
            "zero. Most of those gaps are small. Larger gaps occur ",
            "occasionally. The shape of that bouncing-around is the ",
            "sampling distribution of the difference, and the rest of the ",
            "test machinery is dedicated to working out how rare our ",
            "particular gap would be in that world."
          ),
          tldr = tagList(
            "The null sampling distribution for the Daves study is centered at zero ",
            "(because H₀ says ", math_inline("\\mu_1 - \\mu_2 = 0"),
            "). Its spread is determined by the standard error of the ",
            "difference, which Step 5 will compute. The black vertical ",
            "line in the plot below marks where the Daves study's actual gap lands ",
            "on that sampling distribution."
          )
        ),
        math_block("(M_1 - M_2) \\;\\sim\\; N\\!\\left(0,\\ \\sqrt{\\dfrac{\\sigma_1^2}{n_1} + \\dfrac{\\sigma_2^2}{n_2}}\\right)"),
        plotOutput("indep_step4_plot", height = "300px"),
        callout_warm(
          tags$b("The distribution most often confused with this one. "),
          tags$br(),
          "• ", tags$b("Sample distribution within one group: "),
          "the twelve Dave counts inside Daves (or inside non-Daves). ",
          "Its spread is ", math_inline("\\text{SD}_1"), " or ",
          math_inline("\\text{SD}_2"), ", respectively.", tags$br(),
          "• ", tags$b("Sampling distribution of the difference: "),
          "the bell curve in the plot above. The random variable here is ",
          math_inline("M_1 - M_2"), ", a quantity that does not exist as a ",
          "number until both group averages have been computed and ",
          "subtracted. Each dot in that bell curve corresponds to one ",
          "entire imagined replication of the Daves study."
        ),
        id_prefix = "indep-step"
      ),

      # ------ STEP 5 -------------------------------------------------------
      step_container(
        5, "Pooled variance and standard error of \\(M_1 - M_2\\)",
        explanation_triad(
          formal = tagList(
            "The pooled-variance procedure assumes homogeneity of variance ",
            "across the two populations, i.e., ",
            math_inline("\\sigma_1 = \\sigma_2"),
            ". Under that assumption, the best single estimate of the ",
            "common population variance is the ", tags$b("pooled variance"),
            ", ",
            math_inline("\\text{SD}_p^2 = \\dfrac{\\text{SS}_1 + \\text{SS}_2}{(n_1 - 1) + (n_2 - 1)}"),
            ". The standard error of the difference between sample means ",
            "then works out to ",
            math_inline("\\text{SE} = \\sqrt{\\text{SD}_p^2 \\left(1/n_1 + 1/n_2\\right)}"), "."
          ),
          example = tagList(
            "Both groups give us information about how noisy the within-",
            "group measurements are. We pool that information together to ",
            "get one solid estimate of within-group noise, which is the ",
            "pooled variance. Dividing the pooled noise by the appropriate ",
            "combination of sample sizes (and taking the square root) gives ",
            "us the standard error of the gap between the two sample means."
          ),
          tldr = tagList(
            "Plugging the Daves study's numbers in: ",
            math_inline("\\text{SD}_p^2 = (\\text{SS}_1 + \\text{SS}_2) / df"),
            " = ", sprintf("(%.2f + %.2f)/22 = %.3f", LAB5$SS1, LAB5$SS2, LAB5$sp2),
            ". From there, the standard error of the gap is ",
            math_inline(sprintf("\\text{SE} = \\sqrt{\\text{SD}_p^2 (1/12 + 1/12)} \\approx %.3f",
                                LAB5$se)), "."
          )
        ),
        math_block("\\text{SD}_p^2 \\;=\\; \\dfrac{\\text{SS}_1 + \\text{SS}_2}{(n_1-1) + (n_2-1)}, \\qquad \\text{SE} \\;=\\; \\sqrt{\\text{SD}_p^2 \\left(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\right)}"),
        div(class = "p-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("SS_1 + SS_2",
                                  sprintf("%.2f", LAB5$SS1 + LAB5$SS2))),
              column(3, stat_card("df_within = (n_1-1)+(n_2-1)",
                                  LAB5$df)),
              column(3, stat_card("SD_p² (pooled variance)",
                                  sprintf("%.3f", LAB5$sp2))),
              column(3, stat_card("SE",
                                  sprintf("%.3f", LAB5$se), key = TRUE)))),
        callout_warm(
          tags$b("A frequent source of confusion: Cohen's d uses a ",
                 "different denominator from t. "),
          "Cohen's d for an independent-samples t-test is computed as ",
          math_inline("d = (M_1 - M_2)/\\sqrt{\\text{SD}_p^2}"),
          ", and for the Daves study it works out to ",
          sprintf("d = %.2f", LAB5$d),
          ", which counts as a large effect by Cohen's benchmarks ",
          "(d ≥ 0.8). The denominator of t uses ",
          math_inline("\\text{SE}"),
          " (the standard error of the difference between means), whereas ",
          "the denominator of d uses ", math_inline("\\sqrt{\\text{SD}_p^2}"),
          " (the pooled standard deviation). The two denominators serve ",
          "two different questions. The t-statistic answers whether the ",
          "effect is detectable given how much sampling noise we expect. ",
          "Cohen's d answers how large the effect is in standardized units ",
          "of within-group variation."
        ),
        id_prefix = "indep-step"
      ),

      # ------ STEP 6 -------------------------------------------------------
      step_container(
        6, "Putting it together: the independent-samples t-statistic",
        explanation_triad(
          formal = tagList(
            "Under ", math_inline("H_0:\\ \\mu_1 - \\mu_2 = 0"),
            " and the assumption of homogeneity of variance, the ",
            "standardized gap between the two sample means follows a ",
            "t-distribution with ",
            math_inline("df = (n_1-1)+(n_2-1) = n_1+n_2-2"),
            ". The corresponding t-statistic is the observed gap divided ",
            "by its standard error from Step 5."
          ),
          example = tagList(
            "Same recipe as every other t-test. The numerator is signal ",
            "(the gap between the two sample means, ",
            math_inline("M_1 - M_2"),
            "). The denominator is noise (the standard error of that gap ",
            "from Step 5). Dividing one by the other tells us how many ",
            "standard-error units the observed gap sits from zero on the ",
            "null sampling distribution."
          ),
          tldr = tagList(
            "Plugging the Daves study's numbers into the formula gives ",
            math_inline(sprintf("t = (M_1 - M_2) / SE = %.2f / %.3f \\approx %.2f",
                                LAB5$diff, LAB5$se, LAB5$t)),
            ", with df = 22. The observed gap is about 3 standard errors ",
            "away from zero, which puts it far out in the ",
            "tail of the null sampling distribution."
          )
        ),
        math_block("t \\;=\\; \\dfrac{M_1 - M_2}{\\sqrt{\\text{SD}_p^2\\left(\\frac{1}{n_1} + \\frac{1}{n_2}\\right)}}, \\quad df = n_1 + n_2 - 2"),
        plotOutput("indep_step6_plot", height = "260px"),
        callout_warm(
          tags$b("The Levene's-test side comment. "),
          "The pooling step in Step 5 assumes that the two groups have ",
          "similar variances (",
          math_inline("\\sigma_1 \\approx \\sigma_2"),
          "). Levene's test is the procedure conventionally used to ",
          "evaluate that assumption. For the Daves study's purposes, we are hoping ",
          "that Levene's test is non-significant, because a non-significant ",
          "result is consistent with the homogeneity assumption. If ",
          "Levene's test turns out significant instead, the recommended ",
          "adjustment is to use Welch's t-test, which does not pool the ",
          "two variances. This is a footnote for our purposes and will not ",
          "appear on the midterm, but it is the standard accommodation in ",
          "practice."
        ),
        id_prefix = "indep-step"
      ),

      # ------ STEP 7 -------------------------------------------------------
      step_container(
        7, "Setting alpha (\\(\\alpha\\))",
        explanation_triad(
          formal = tagList(
            "Alpha (α) is the pre-specified probability of rejecting the ",
            "null hypothesis in a world where the null is in fact true. ",
            "By convention in PSY 302, α = .05 with a two-tailed alternative."
          ),
          example = tagList(
            "Alpha is the amount of false-alarm risk we are willing to ",
            "accept. The conceptual story here is identical to the one in ",
            "Tab 1 and Tab 2. Alpha sets the cutoffs that determine when ",
            "we reject H₀, and the p-value (which appears in Step 9) ",
            "measures how extreme the observed result is on the null ",
            "sampling distribution."
          ),
          tldr = tagList(
            "The Daves study uses the PSY 302 default of α = .05 with a two-tailed ",
            "alternative."
          )
        ),
        math_block("\\Pr(\\text{reject } H_0 \\mid H_0 \\text{ true}) \\;=\\; \\alpha"),
        id_prefix = "indep-step"
      ),

      # ------ STEP 8 -------------------------------------------------------
      step_container(
        8, "Critical values, \\(df = n_1 + n_2 - 2\\)",
        explanation_triad(
          formal = tagList(
            "For a two-tailed test at level α, the critical values bounding ",
            "the rejection region are ",
            math_inline("\\pm t_{\\alpha/2,\\ df}"),
            ", with ", math_inline("df = n_1 + n_2 - 2"),
            ". The df expression follows from the fact that we have ",
            math_inline("n_1 + n_2"),
            " independent observations and we are estimating two group ",
            "means from them."
          ),
          example = tagList(
            "The df can be read as the total number of independent ",
            "observations minus the number of group means we had to ",
            "estimate from them. Larger samples produce larger df values, ",
            "which produce tighter sampling distributions, which in turn ",
            "produce smaller critical values."
          ),
          tldr = tagList(
            "For the Daves study, ", math_inline("df = 12 + 12 - 2 = 22"),
            ". At α = .05 two-tailed, the critical values are ",
            math_inline("\\pm t_{.025,\\ 22} = \\pm 2.074"), "."
          )
        ),
        math_block("\\text{Two-tailed CV} \\;=\\; \\pm t_{\\alpha/2,\\ df},\\quad df = n_1 + n_2 - 2"),
        plotOutput("indep_step8_plot", height = "260px"),
        id_prefix = "indep-step"
      ),

      # ------ STEP 9 -------------------------------------------------------
      step_container(
        9, "The decision: reject \\(H_0\\) or fail to reject?",
        explanation_triad(
          formal = tagList(
            "We reject ", math_inline("H_0"), " when ",
            math_inline("|t| \\geq |\\text{CV}|"),
            ", which is equivalent to the condition ",
            math_inline("p < \\alpha"),
            ". The interpretation of \"fail to reject\" mirrors what was ",
            "said in Tab 2's Step 9. Failing to reject H₀ is not the same ",
            "as concluding that H₀ is true."
          ),
          example = tagList(
            "We compare our observed t-statistic to the cutoff. If it ",
            "lands past the critical value, we reject H₀, because the gap ",
            "between the two sample means is too large to chalk up to ",
            "sampling luck. If it lands inside the critical value, we fail ",
            "to reject H₀, because the gap is small enough that sampling ",
            "luck plausibly could have produced it on its own."
          ),
          tldr = tagList(
            sprintf("The Daves study yielded t(22) = %.2f, and |%.2f| is well past the critical value of 2.074. We therefore ",
                    LAB5$t, LAB5$t),
            tags$b("reject H₀"),
            " with p = .006. The APA-style writeup ",
            "reads: ",
            tags$em("\"Using an independent samples t-test, the average number "),
            tags$em("of Daves known by respondents named Dave (M = 7.00, "),
            tags$em("SD = 2.56) was significantly higher than that of non-Daves "),
            tags$em("(M = 4.00, SD = 2.30), t(22) = 3.02, p = .006, "),
            tags$em("95% CIµDiff[0.94, 5.06]. This was a large effect, d = 1.23.\"")
          )
        ),
        math_block("\\text{Reject } H_0 \\;\\Longleftrightarrow\\; |t| \\geq \\text{CV} \\;\\Longleftrightarrow\\; p < \\alpha"),
        div(class = "p-3 mt-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            fluidRow(
              column(3, stat_card("Your t", sprintf("%.3f", LAB5$t))),
              column(3, stat_card("Critical value", "±2.074")),
              column(3, stat_card("p-value",
                                  if (LAB5$p < 0.001) "< 0.001" else sprintf("%.3f", LAB5$p))),
              column(3, stat_card("Cohen's d", sprintf("%.2f", LAB5$d)))),
            div(class = "p-3 mt-3",
                style = sprintf("background:%s; color:%s; border-radius:8px; font-weight:600; font-size:18px;",
                                PAL$ok_bg, PAL$ok_fg),
                "Reject H₀",
                tags$br(),
                tags$span(style = "font-weight:400; font-size:14px;",
                          sprintf("|t| = %.2f ≥ |CV| = 2.074. The observed gap of 3.00 Daves is too large to attribute to sampling luck under the assumption that the two groups have equal average Dave counts.",
                                  abs(LAB5$t))))),
        plotOutput("indep_step9_plot", height = "320px"),
        callout_warm(
          tags$b("A quick sanity check across all three t-tests. "),
          "The ", math_inline("M_1 - M_2"),
          " framing of the independent-samples t mirrors the ",
          math_inline("M_D"),
          " framing of the paired t, which in turn mirrors the ",
          math_inline("M - \\mu_0"),
          " framing of the single-sample t. All three statistics share the ",
          "same shape: (a quantity of interest) divided by (the standard ",
          "error of that quantity of interest). Only the quantity and ",
          "the standard-error formula change."
        ),
        id_prefix = "indep-step"
      ),

      # ---- APA card -------------------------------------------------------
      div(class = "p-3 mt-4",
          style = sprintf("background:%s; color:%s; border-radius:10px; border-left:5px solid %s;",
                          PAL$warn_bg, PAL$warn_fg, OI$orange),
          h5("Translating numbers into APA-style prose", style = "margin-top:0;"),
          tags$blockquote(style = "font-style:italic; margin-left:8px; border-left:4px solid #d6c08a; padding-left:12px;",
            "Using an independent-samples t-test, we found that the ",
            "average number of Daves known by respondents named Dave ",
            "(M = 7.00, SD = 2.56) was significantly higher than the ",
            "average number known by respondents with other names ",
            "(M = 4.00, SD = 2.30), t(22) = 3.02, p = .006, ",
            "95% CIµDiff[0.94, 5.06]. This was a large effect, d = 1.23."),
          p(style = "margin-bottom:0;",
            tags$b("Template: "),
            "Using a [test], the average [DV] of [group 1 (M=, SD=)] was ",
            "[significantly/non-significantly] [direction] than [group 2 ",
            "(M=, SD=)], t(df) = , p = , 95% CIµDiff[ , ]. This was a ",
            "[size] effect, d = .")
      ),

      # ===================================================================
      # Tutorial finale: build intuition, then unlock practice quest
      # ===================================================================
      div(class = "mt-5 mb-3 p-4",
          style = sprintf("background:#E1F1FB; border-left:6px solid %s; border-radius:10px;",
                          PAL$pop_dark),
          h3("Now play with the machinery", style = "margin-top:0;"),
          p("Before you tackle the practice quest, picture how the Daves ",
            "study's result would shift if any of its inputs changed. ",
            "The Step 3 simulation buttons are the most concrete tool you ",
            "have on this tab. Each click draws a fresh imagined ",
            "replication, letting you watch the sampling distribution of ",
            math_inline("M_1 - M_2"),
            " fill in. As you run more simulations, the cloud fills in ",
            "around the true gap."),
          tags$ul(style = "margin-bottom:0; padding-left:20px; line-height:1.7;",
            tags$li(tags$b("Bigger n in either group. "),
              "Imagine that 100 Daves and 100 non-Daves were surveyed ",
              "instead of 12 each. The pooled SE in Step 5 would ",
              "shrink by a factor of roughly √(100/12) ≈ 2.9, the t-statistic would ",
              "grow accordingly, and the p-value would drop. Bigger ",
              "studies make small effects easier to detect."),
            tags$li(tags$b("Equal vs unequal group sizes. "),
              math_inline("\\text{SE} = \\sqrt{\\text{SD}_p^2(1/n_1 + 1/n_2)}"),
              " is minimized when ", math_inline("n_1 = n_2"),
              ". Unequal groups lose statistical power. If the Daves ",
              "study had recruited 22 non-Daves and 2 Daves instead of ",
              "12 each, the test would have much less power, even with ",
              "the same total N."),
            tags$li(tags$b("Bigger pooled SD. "),
              "Picture the data noisier within each group. ",
              math_inline("\\text{SD}_p^2"), " grows, ",
              math_inline("\\text{SE}"),
              " grows with it, and the same observed gap of three Daves ",
              "becomes much harder to distinguish from sampling luck."),
            tags$li(tags$b("Run the Step 3 simulations many times. "),
              "Hit ", tags$em("Run 5 more"),
              " repeatedly. Watch the cloud of (M_1 − M_2) values take shape ",
              "as it grows. The standard error in Step 5 measures the ",
              "width of that cloud."))
      ),
      div(class = "text-center mb-4",
          p(style = "color:#444; font-size:14px;",
            "When the relationships among sample size, within-group ",
            "noise, and the observed gap feel intuitive, unlock the ",
            "practice quest below."),
          actionButton("tab3_show_quest",
                       "Show me the practice quest →",
                       class = "btn-primary btn-lg")),

      # ===================================================================
      # QUEST. Probiotics for antibiotic-induced GI problems
      # ===================================================================
      conditionalPanel(condition = "input.tab3_show_quest > 0",
      quest_section(
        title = "Do probiotics protect kids from the GI side effects of antibiotics?",
        id_prefix = "tab3quest",
        scenario_html = tagList(
          p("Antibiotics can wreak havoc on a child's gut microbiome and ",
            "lead to days of diarrhea, cramping, and general misery. A ",
            "clinical trial described by Merenstein and colleagues asks ",
            "whether a particular probiotic strain in yogurt reduces those ",
            "side effects. Thirty children prescribed a fourteen-day course ",
            "of antibiotics are randomly assigned either to receive a ",
            "yogurt containing the probiotic strain or to receive an ",
            "identical-looking placebo yogurt. The outcome is the number ",
            "of days during the course on which the child reports ",
            "gastrointestinal symptoms."),
          p(style = "margin-bottom:0;",
            "Use an independent-samples t-test at α = .05, two-tailed, to ",
            "evaluate whether the probiotic and placebo groups differ.")
        ),
        data_block = tagList(
          tags$ul(style = "margin-bottom:0;",
            tags$li(tags$b("Probiotic group: "),
                    sprintf("n = %d, M = %.2f days, SD = %.2f",
                            PROBIOTIC$n1, PROBIOTIC$M1, PROBIOTIC$s1)),
            tags$li(tags$b("Placebo group: "),
                    sprintf("n = %d, M = %.2f days, SD = %.2f",
                            PROBIOTIC$n2, PROBIOTIC$M2, PROBIOTIC$s2)),
            tags$li(tags$b("Sum of squared deviations: "),
                    sprintf("SS_1 = %.2f, SS_2 = %.2f",
                            PROBIOTIC$SS1, PROBIOTIC$SS2)))
        ),
        questions = list(
          list(
            prompt = tagList(tags$b("Step 1. "),
              "State H₀ and H₁ for the probiotic study."),
            solution = tagList(
              p(math_inline("H_0: \\mu_1 - \\mu_2 = 0"),
                ". The probiotic and placebo groups come from populations ",
                "with the same average number of GI-symptom days."),
              p(math_inline("H_1: \\mu_1 - \\mu_2 \\neq 0"),
                ". The two population averages differ in some direction."))
          ),
          list(
            prompt = tagList(tags$b("Step 2. "),
              "What is the observed gap (signal) between the two sample ",
              "means? Which group is higher?"),
            solution = tagList(
              p(sprintf("M_1 - M_2 = %.2f - %.2f = %.2f days. The placebo group reports about 2.5 more GI-symptom days, on average, than the probiotic group.",
                        PROBIOTIC$M1, PROBIOTIC$M2, PROBIOTIC$diff)))
          ),
          list(
            prompt = tagList(tags$b("Step 3. "),
              "If the trial were re-run with a fresh set of 30 children, ",
              "would you expect the same gap of 2.5 days? Explain."),
            solution = tagList(
              p("No. A new sample of 30 children would produce two slightly ",
                "different group means, even if the true population effect ",
                "were unchanged. The gap is a noisy estimate of the true ",
                "average effect, and the standard error in Step 5 will ",
                "quantify how noisy."))
          ),
          list(
            prompt = tagList(tags$b("Step 4. "),
              "Under H₀, where is the sampling distribution of (M₁ − M₂) ",
              "centered, and what shape does it take?"),
            solution = tagList(
              p("It is centered at zero (because H₀ says ",
                math_inline("\\mu_1 - \\mu_2 = 0"),
                ") and takes a t-distribution shape with ",
                math_inline("df = n_1 + n_2 - 2 = 28"),
                ". Its width is set by the standard error in Step 5."))
          ),
          list(
            prompt = tagList(tags$b("Step 5. "),
              "Compute the pooled variance and the standard error of the ",
              "difference between means."),
            solution = tagList(
              p(sprintf("Pooled variance: SD_p² = (SS_1 + SS_2) / df = (%.2f + %.2f) / 28 = %.3f.",
                        PROBIOTIC$SS1, PROBIOTIC$SS2, PROBIOTIC$sp2)),
              p(sprintf("Standard error: SE = sqrt(SD_p² · (1/n_1 + 1/n_2)) = sqrt(%.3f · 2/15) ≈ %.3f days.",
                        PROBIOTIC$sp2, PROBIOTIC$se)))
          ),
          list(
            prompt = tagList(tags$b("Step 6. "),
              "Compute the independent-samples t-statistic."),
            solution = tagList(
              p(sprintf("t = (M_1 - M_2) / SE = %.2f / %.3f ≈ %.2f.",
                        PROBIOTIC$diff, PROBIOTIC$se, PROBIOTIC$t),
                " The observed gap sits about ",
                sprintf("%.1f standard errors below zero", abs(PROBIOTIC$t)),
                "."))
          ),
          list(
            prompt = tagList(tags$b("Step 7. "),
              "What α value should we use, and what does it represent?"),
            solution = tagList(
              p("Use α = .05, two-tailed. Alpha is the maximum rate at ",
                "which we are willing to falsely reject H₀ in a world ",
                "where H₀ is true."))
          ),
          list(
            prompt = tagList(tags$b("Step 8. "),
              "Find the two-tailed critical value with the appropriate df."),
            solution = tagList(
              p("With df = 28, the two-tailed .05 critical values are ",
                math_inline("\\pm t_{.025,\\ 28} = \\pm 2.048"), "."))
          ),
          list(
            prompt = tagList(tags$b("Step 9. "),
              "What is your decision? Write up the result in APA style."),
            solution = tagList(
              p(sprintf("|t| = %.2f is much larger than 2.048, so we ", abs(PROBIOTIC$t)),
                tags$b("reject H₀"), ". The p-value is less than .001."),
              p(style = "margin-bottom:0;",
                "APA-style writeup: ",
                tags$em(sprintf("\"Using an independent-samples t-test, children receiving the probiotic yogurt reported fewer days of GI symptoms (M = %.2f, SD = %.2f) than children receiving the placebo (M = %.2f, SD = %.2f), t(28) = %.2f, p < .001, d = %.2f.\"",
                                PROBIOTIC$M1, PROBIOTIC$s1,
                                PROBIOTIC$M2, PROBIOTIC$s2,
                                PROBIOTIC$t, abs(PROBIOTIC$d)))))
          )
        )
      )
      )  # close conditionalPanel(tab3_show_quest)
    ))
  })

  # ---- Step 2 plot ---------------------------------------------------------
  output$indep_step2_plot <- renderPlot({
    plot_two_groups(LAB5$b1, LAB5$b2, "Daves", "non-Daves")
  }, res = 96)

  # ---- Step 3 simulations -------------------------------------------------
  iw <- reactiveValues(diffs = numeric(0))
  observeEvent(input$indep_repeat_one, {
    g1 <- rnorm(LAB5$n1, LAB5$M1, sqrt(LAB5$sp2))
    g2 <- rnorm(LAB5$n2, LAB5$M2, sqrt(LAB5$sp2))
    iw$diffs <- c(iw$diffs, mean(g1) - mean(g2))
  })
  observeEvent(input$indep_repeat_five, {
    new <- replicate(5, {
      g1 <- rnorm(LAB5$n1, LAB5$M1, sqrt(LAB5$sp2))
      g2 <- rnorm(LAB5$n2, LAB5$M2, sqrt(LAB5$sp2))
      mean(g1) - mean(g2)
    })
    iw$diffs <- c(iw$diffs, new)
  })
  observeEvent(input$indep_repeat_reset, { iw$diffs <- numeric(0) })

  output$indep_step3_plot <- renderPlot({
    ds <- iw$diffs
    xr <- c(LAB5$diff - 4 * LAB5$se, LAB5$diff + 4 * LAB5$se)
    g <- ggplot() +
      geom_vline(xintercept = LAB5$diff, colour = PAL$pop_dark,
                 linetype = "dashed", linewidth = 0.4) +
      annotate("label", x = LAB5$diff, y = 1.05,
               label = sprintf("Observed gap = %.2f", LAB5$diff),
               colour = PAL$pop_dark, fill = "white", label.size = NA,
               label.r = unit(0.15, "lines"), alpha = 0.95, size = 4)
    if (length(ds) > 0) {
      g <- g + geom_dotplot(
        data = data.frame(x = ds), aes(x = x),
        binwidth = (xr[2] - xr[1]) / 50,
        fill = PAL$samp_pt, colour = "white",
        stackdir = "up", dotsize = 0.85,
        method = "histodot", stackratio = 1.05)
    } else {
      g <- g + annotate("label", x = LAB5$diff, y = 0.5,
                        label = "Click a button to simulate replications",
                        colour = "#666666", fill = "white", label.size = NA, size = 4.2)
    }
    g + scale_x_continuous(limits = xr) +
      scale_y_continuous(NULL, breaks = NULL, limits = c(0, 1.2)) +
      labs(x = "Possible (M_1 − M_2) from an imagined replication", y = NULL,
           subtitle = "Each green dot = one imagined re-run of the Daves study.") +
      base_theme() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }, res = 96)
  output$indep_step3_counter <- renderUI({
    div(class = "p-2", style = sprintf("color:%s; font-size:14px;", PAL$pop_dark),
        sprintf("Studies you've simulated so far: %d", length(iw$diffs)))
  })

  # ---- Step 4 sampling-distribution-of-difference ------------------------
  output$indep_step4_plot <- renderPlot({
    plot_diff_means_sampling(LAB5$diff, LAB5$se, LAB5$df, mu_diff_null = 0)
  }, res = 96)

  # ---- Step 6 sampling distribution with observed gap -------------------
  output$indep_step6_plot <- renderPlot({
    plot_diff_means_sampling(LAB5$diff, LAB5$se, LAB5$df, mu_diff_null = 0)
  }, res = 96)

  # ---- Step 8 critical-value picture -------------------------------------
  output$indep_step8_plot <- renderPlot({
    alpha <- 0.05; df_v <- LAB5$df
    cv    <- qt(1 - alpha / 2, df_v)
    xr    <- t_xrange(-cv, cv)
    x     <- seq(xr[1], xr[2], length.out = 400)
    cd    <- data.frame(x = x, y = dt(x, df_v))
    reg_lo <- subset(cd, x <= -cv); reg_hi <- subset(cd, x >= cv)
    max_y <- max(cd$y); label_offset <- diff(xr) * 0.02
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
           subtitle = "Orange tails = the α = 0.05 rejection region.") +
      base_theme()
  }, res = 96)

  # ---- Step 9 decision picture -------------------------------------------
  output$indep_step9_plot <- renderPlot({
    draw_t_curve(LAB5$t, LAB5$df, alpha = 0.05) +
      labs(subtitle = sprintf(
        "df = %d  |  α = 0.05  |  p %s  |  decision: reject H₀",
        LAB5$df,
        if (LAB5$p < 0.001) "< 0.001" else sprintf("= %.3f", LAB5$p)))
  }, res = 96)
}
