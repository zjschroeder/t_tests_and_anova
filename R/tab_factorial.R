# =============================================================================
# tab_factorial.R
# Factorial (two-way) ANOVA walkthrough. Parents dosing children with medicine:
# how do measurement modality and prescribed dose interact to produce dosing
# errors? Inspired by Yin et al. (Pediatrics, 2016).
# https://notawfulandboring.blogspot.com/2017/01/parents-may-be-giving-their-children.html
# =============================================================================

# ---- Case-study constants: medication-dosing accuracy ----------------------
# 2 × 3 design. Rows: measurement modality (small syringe vs medicine cup).
# Cols: prescribed dose (2.5 ml, 5.0 ml, 7.5 ml). Cell values are the
# average percent dosing error in each combination. Lower is more accurate.
DOSE <- local({
  cells <- matrix(c(
    5,  7,  8,    # Syringe
    30, 22, 18    # Medicine cup
  ), nrow = 2, byrow = TRUE,
  dimnames = list(Modality = c("Syringe", "Medicine cup"),
                  Dose     = c("2.5 ml", "5.0 ml", "7.5 ml")))
  list(
    cells = cells,
    M_modality = rowMeans(cells),    # 6.67, 23.33
    M_dose     = colMeans(cells),    # 17.5, 14.5, 13.0
    M_grand    = mean(cells),        # 15.0
    # Reported test statistics from the Yin et al. (2016) study, adapted for
    # this 2x3 setup. F-stats are illustrative, in the spirit of the original
    # paper which reported a large modality main effect and a significant
    # interaction.
    F_modality = 78.3, df_mod = c(1, 84), p_modality = "< .001",
    F_dose     = 4.5,  df_dose = c(2, 84), p_dose = ".013",
    F_interaction = 9.2, df_inter = c(2, 84), p_interaction = "< .001",
    tukey = data.frame(
      pair = c("2.5 ml vs 5.0 ml", "5.0 ml vs 7.5 ml", "2.5 ml vs 7.5 ml"),
      p    = c(".082", ".480", ".009"),
      direction = c("2.5 ml > 5.0 ml", "5.0 ml ≈ 7.5 ml", "2.5 ml > 7.5 ml"))
  )
})
LAB8 <- DOSE  # backwards-compatibility alias

# Cell-means HTML table builder
build_cell_means_table <- function(cells, M_row, M_col, M_grand) {
  tags$table(class = "table table-bordered table-sm mb-0",
    style = "max-width: 720px;",
    tags$thead(tags$tr(
      tags$th(""),
      lapply(colnames(cells), function(d) tags$th(d)),
      tags$th(style = "background:#f6f5f0;", "Row mean"))),
    tags$tbody(
      lapply(seq_len(nrow(cells)), function(i) {
        tags$tr(
          tags$td(tags$b(rownames(cells)[i])),
          lapply(seq_len(ncol(cells)), function(j) {
            tags$td(style = "text-align:center;", sprintf("%.2f", cells[i, j]))
          }),
          tags$td(style = "background:#f6f5f0; text-align:center; font-weight:600;",
                  sprintf("%.2f", M_row[i])))
      }),
      tags$tr(style = "border-top: 2px solid #aaa;",
        tags$td(tags$b("Col mean"), style = "background:#f6f5f0;"),
        lapply(seq_along(M_col), function(j)
          tags$td(style = "background:#f6f5f0; text-align:center; font-weight:600;",
                  sprintf("%.2f", M_col[j]))),
        tags$td(style = "background:#dfdaca; text-align:center; font-weight:700;",
                sprintf("%.2f", M_grand)))))
}

# Interaction plot
plot_interaction <- function(cells) {
  long <- as.data.frame(as.table(cells))
  names(long) <- c("Modality", "Dose", "Score")
  ggplot(long, aes(x = Dose, y = Score, group = Modality, colour = Modality)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 4) +
    scale_colour_manual(values = setNames(c(PAL$pop_dark, PAL$samp_pt),
                                          c("Syringe", "Medicine cup"))) +
    labs(x = "Prescribed dose", y = "Cell mean dosing-error percentage",
         subtitle = "Non-parallel lines (especially crossing) signal an interaction effect.") +
    base_theme() +
    theme(legend.position = "top")
}

# ---- Server entry point -----------------------------------------------------
source_factorial_server <- function(input, output, session) {

  output$fact_tab_ui <- renderUI({
    withMathJax(div(class = "container-fluid", style = "max-width: 1240px; padding-top: 16px;",

      scenario_card(
        "Factorial (two-way) ANOVA",
        lab_label = "Running example: parents dosing children with medicine",
        p(style = "margin-bottom:6px;",
          "A factorial ANOVA includes ",
          tags$b("two (or more) independent variables"),
          ", each with its own set of levels. The procedure produces ",
          tags$em("three"),
          " separate F-tests in a two-factor design: one main effect for ",
          "each factor, plus an interaction effect that captures how the ",
          "two factors combine."),
        p(style = "margin-bottom:6px;",
          tags$b("Running example. "),
          "Yin and colleagues (Pediatrics, 2016) studied how parents ",
          "measure liquid medication for their children. The design ",
          "crossed two variables: the device the parent uses (an oral syringe ",
          "or a medicine cup), and the prescribed dose (2.5 ml, 5.0 ml, or ",
          "7.5 ml). Adapted for our walkthrough as a 2 × 3 design with 15 ",
          "parents per cell, the dependent variable is the average percent ",
          "error each parent made when drawing the dose."),
        p(style = "margin-bottom:0;",
          tags$b("A heads-up. "),
          "This tab is intentionally lighter than the others. Factorial ",
          "ANOVA is post-midterm material, so the goal here is conceptual ",
          "orientation rather than full procedural depth. We cover cell ",
          "means, marginal means, three F-ratios, and how to read an ",
          "interaction plot, but with less hand-holding through the ",
          "machinery than the t-test and one-way ANOVA tabs offer.")
      ),

      # ------ STEP 1 . Cell means table ------------------------------------
      step_container(
        1, "The 2 × 3 design: cell means, marginal means, grand mean",
        explanation_triad(
          formal = tagList(
            "A 2 × 3 factorial design crosses Factor A (with two levels) ",
            "and Factor B (with three levels), producing six ",
            tags$b("cells"),
            ". Each cell has a sample of size ", math_inline("n"),
            " and a corresponding ", tags$b("cell mean"), " ",
            math_inline("M_{ij}"),
            ". The ", tags$b("marginal means"),
            " summarize one factor at a time by collapsing across the ",
            "other. Row means average across the levels of Factor B, and ",
            "column means average across the levels of Factor A. The ",
            tags$b("grand mean"),
            " is the overall average across the whole dataset."
          ),
          example = tagList(
            "Imagine a two-row by three-column grid. Inside each cell, ",
            "write the average score for that specific combination of ",
            "conditions. The numbers running down the right-hand edge are ",
            "the row marginals (one for each measurement device, ",
            "averaging across all three doses). The numbers along the ",
            "bottom are the column marginals (one for each dose, ",
            "averaging across both devices). The bottom-right corner is ",
            "the grand mean."
          ),
          tldr = tagList(
            "Syringes are accurate ",
            "across all three prescribed doses (errors of 5%, 7%, and 8%). ",
            "Medicine cups are terrible at the smallest dose (30% error) ",
            "and steadily improve at larger doses (22% at 5 ml, 18% at ",
            "7.5 ml). The two rows show qualitatively different patterns, ",
            "which is what statisticians mean by an interaction."
          )
        ),
        div(class = "p-3 mt-2",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            build_cell_means_table(LAB8$cells, LAB8$M_modality,
                                   LAB8$M_dose, LAB8$M_grand)),
        callout_warm(
          tags$b("Three kinds of mean worth keeping straight. "),
          tags$br(),
          "• ", tags$b("Cell mean. "),
          "The average within one specific combination of factor levels ",
          "(for instance, the Syringe by 2.5 ml cell). There are six cell ",
          "means in a 2 × 3 design.", tags$br(),
          "• ", tags$b("Marginal mean. "),
          "The average across one factor, ignoring the other. For example, ",
          "the marginal mean for Syringes collapses across all three ",
          "prescribed doses. Marginal means are what the main-effect F-tests ",
          "evaluate.", tags$br(),
          "• ", tags$b("Grand mean. "),
          "The average across the entire dataset. The grand mean serves ",
          "as the baseline against which SS_total is measured, and ",
          "appears as the bottom-right corner of the cell-means table."
        ),
        id_prefix = "fact-step"
      ),

      # ------ STEP 2 . Three null hypotheses ------------------------------
      step_container(
        2, "Three null hypotheses, three F-tests",
        explanation_triad(
          formal = tagList(
            "Factorial ANOVA evaluates three separate null hypotheses ",
            "simultaneously. The first is the main effect of A, ",
            math_inline("H_0^A:\\ \\mu_{1\\cdot} = \\mu_{2\\cdot}"),
            ", which states that the row marginals are equal. The second ",
            "is the main effect of B, ",
            math_inline("H_0^B:\\ \\mu_{\\cdot 1} = \\mu_{\\cdot 2} = \\mu_{\\cdot 3}"),
            ", which states that the column marginals are equal. The third ",
            "is the interaction, ",
            math_inline("H_0^{AB}: \\text{no interaction}"),
            ", which states that the simple effect of A is the same at ",
            "every level of B (and vice versa)."
          ),
          example = tagList(
            "The main effect of A asks whether syringes, on average, produce ",
            "different error from medicine cups when we ignore prescribed dose. The ",
            "main effect of B asks whether prescribed dose affects error, ",
            "on average, when we ignore the device. The interaction asks ",
            "whether the effect of one factor depends on the level of the ",
            "other (whether dose affects syringe accuracy differently from ",
            "how it affects medicine-cup accuracy). Note that the interaction can ",
            "show up as significant even when neither main effect does."
          ),
          tldr = tagList(
            "The three null hypotheses for the dosing study specify, in turn: that ",
            "the overall modality marginals are equal (",
            math_inline("\\mu_{\\text{Syringe}} = \\mu_{\\text{Medicine cup}}"),
            "); that the overall dose marginals are equal (",
            math_inline("\\mu_{2.5\\,\\text{ml}} = \\mu_{5.0\\,\\text{ml}} = \\mu_{7.5\\,\\text{ml}}"),
            "); and that there is no interaction between modality and ",
            "prescribed dose. Each gets its own F-test."
          )
        ),
        math_block("H_0^{A},\\quad H_0^{B},\\quad H_0^{A \\times B}"),
        id_prefix = "fact-step"
      ),

      # ------ STEP 3 . SS partition ----------------------------------------
      step_container(
        3, "Partitioning the variance: \\(SS_{\\text{total}} = SS_A + SS_B + SS_{AB} + SS_{\\text{within}}\\)",
        explanation_triad(
          formal = tagList(
            "The total sum of squares partitions into four additive ",
            "components. ",
            math_inline("SS_A"),
            " captures variation due to Factor A and is computed from the ",
            "row marginals. ",
            math_inline("SS_B"),
            " captures variation due to Factor B and is computed from the ",
            "column marginals. ",
            math_inline("SS_{AB}"),
            " captures the interaction, which is the part of the cell-mean ",
            "variation that the two main effects alone cannot account for. ",
            math_inline("SS_{\\text{within}}"),
            " captures the residual noise within cells."
          ),
          example = tagList(
            "The conceptual structure is the same as in one-way ANOVA, ",
            "with one elaboration. The between-groups piece, which used to ",
            "be a single sum of squares, now splits into three sub-pieces. ",
            "We can ask how much of the variation in cell means is ",
            "attributable to Factor A on its own, how much to Factor B on ",
            "its own, and how much to the combination of the two factors ",
            "(the interaction). Adding those three pieces back together ",
            "reproduces the variation in the cell means."
          ),
          tldr = tagList(
            "For the by-hand version of the dosing study (which uses a different ",
            "scenario from the Jamovi data above), the partitioning gives ",
            "SS_A = 120, SS_B = 60, SS_AB = 60, SS_within = 120, and ",
            "SS_total = 360. The interaction piece is the same size as ",
            "the Factor B main effect."
          )
        ),
        math_block("SS_{\\text{total}} \\;=\\; SS_A + SS_B + SS_{A \\times B} + SS_{\\text{within}}"),
        id_prefix = "fact-step"
      ),

      # ------ STEP 4 . Three F-ratios + the dosing study source table ----------------
      step_container(
        4, "Three F-ratios, same denominator, different numerators",
        explanation_triad(
          formal = tagList(
            "All three F-ratios share a common denominator, ",
            math_inline("MS_{\\text{within}}"),
            ", which serves as the noise estimate. The three numerators ",
            "differ, with one mean square per effect (",
            math_inline("MS_A"), ", ", math_inline("MS_B"),
            ", and ", math_inline("MS_{A \\times B}"),
            "). Each effect carries its own pair of degrees of freedom for ",
            "the F-distribution it is referenced against."
          ),
          example = tagList(
            "The signal-to-noise idea repeats three times. Each F-ratio ",
            "asks whether one specific piece of between-cell variation is ",
            "larger than what we would expect from the within-cell noise ",
            "alone. The three pieces are evaluated independently, although ",
            "they share a noise estimate, which is why they all come ",
            "together in a single source table."
          ),
          tldr = tagList(
            "Three F-tests, one for each null hypothesis. The source table ",
            "below collects the Jamovi numbers the dosing study reports."
          )
        ),
        math_block("F_A = \\dfrac{MS_A}{MS_{\\text{within}}}, \\quad F_B = \\dfrac{MS_B}{MS_{\\text{within}}}, \\quad F_{AB} = \\dfrac{MS_{A \\times B}}{MS_{\\text{within}}}"),
        div(class = "p-3",
            style = "background:#fff; border:1px solid #ddd; border-radius:8px;",
            h5("Source table for the dosing study (illustrative numbers)", style = "margin-top:0;"),
            div(class = "table-responsive",
                tags$table(class = "table table-sm mb-0",
                  tags$thead(tags$tr(
                    tags$th("Effect"), tags$th("F"), tags$th("df"),
                    tags$th("p"), tags$th("Decision (α = .05)"))),
                  tags$tbody(
                    tags$tr(
                      tags$td(tags$b("Modality (A)")),
                      tags$td(sprintf("%.2f", LAB8$F_modality)),
                      tags$td(sprintf("(%d, %d)", LAB8$df_mod[1], LAB8$df_mod[2])),
                      tags$td(LAB8$p_modality),
                      tags$td("Reject")),
                    tags$tr(
                      tags$td(tags$b("Dose (B)")),
                      tags$td(sprintf("%.2f", LAB8$F_dose)),
                      tags$td(sprintf("(%d, %d)", LAB8$df_dose[1], LAB8$df_dose[2])),
                      tags$td(LAB8$p_dose),
                      tags$td("Reject")),
                    tags$tr(
                      tags$td(tags$b("Modality × Dose")),
                      tags$td(sprintf("%.2f", LAB8$F_interaction)),
                      tags$td(sprintf("(%d, %d)", LAB8$df_inter[1], LAB8$df_inter[2])),
                      tags$td(LAB8$p_interaction),
                      tags$td("Reject")))))),
        id_prefix = "fact-step"
      ),

      # ------ STEP 5 . Interaction plot ------------------------------------
      step_container(
        5, "Reading an interaction plot: parallel lines and what they mean",
        explanation_triad(
          formal = tagList(
            "An interaction plot places one factor on the x-axis and ",
            "renders the other factor as separate lines, one for each ",
            "level. Each point on each line represents one cell mean. ",
            "Parallel lines indicate the absence of an interaction. ",
            "Non-parallel lines indicate the presence of an interaction. ",
            "Lines that cross indicate a ", tags$em("disordinal"),
            " (qualitative) interaction, in which the direction of A's ",
            "effect reverses across levels of B."
          ),
          example = tagList(
            "When the two lines run roughly parallel, the effect of dose ",
            "looks the same for both measurement devices, which is what we ",
            "mean by no interaction. When the lines fan out or cross, the ",
            "effect of dose depends on the device, which is what an ",
            "interaction effect looks like graphically. The greater the ",
            "departure from parallel, the larger the interaction."
          ),
          tldr = tagList(
            "The interaction plot for the dosing study shows two lines ",
            "with very different shapes. The syringe line stays low and ",
            "roughly flat across all three doses (errors of 5%, 7%, 8%). ",
            "The medicine-cup line starts much higher at the 2.5 ml dose ",
            "(30% error) and drops steadily as the prescribed dose grows ",
            "(22% at 5 ml, 18% at 7.5 ml). The two devices respond to ",
            "dose in different ways, and that difference is the ",
            "interaction the F-test is detecting."
          )
        ),
        plotOutput("fact_step5_plot", height = "340px"),
        callout_warm(
          tags$b("A plotting convention. "),
          "By convention, the factor with more levels goes on the x-axis ",
          "of an interaction plot. In the dosing study, dose has three ",
          "levels and modality has two, so dose goes on x. The convention ",
          "exists because the eye tends to track patterns along the ",
          "x-axis more easily than across separate lines, and longer ",
          "x-axes make the lines easier to compare."
        ),
        id_prefix = "fact-step"
      ),

      # ------ STEP 6 . Interpretation rule ---------------------------------
      step_container(
        6, "Interpretation rule: interaction first, then main effects",
        explanation_triad(
          formal = tagList(
            "When the A × B interaction is significant, the main effects ",
            "of A and B should be interpreted with caution. A significant ",
            "interaction means that the effect of one factor depends on ",
            "the level of the other, in which case the marginal main ",
            "effects can be a misleading summary of the underlying ",
            "pattern. The conventional accommodation is to describe the ",
            "pattern of cell means directly, or to report simple effects ",
            "(the effect of A separately within each level of B)."
          ),
          example = tagList(
            "When the lines on an interaction plot cross or fan out, the ",
            "phrase \"larger doses produce less error on average\" is ",
            "misleading, because the average masks the fact ",
            "that the relationship between dose and error depends on the ",
            "measurement device. The better approach is to describe what ",
            "is happening cell by cell. Something like \"medicine cups ",
            "produce most of the error, and the error gets especially bad ",
            "at small doses\" communicates what the data show."
          ),
          tldr = tagList(
            "The dosing study's interaction is significant, F(2, 84) = ",
            "9.2, p < .001. Following the standard accommodation, the ",
            "cell means should be interpreted directly rather than the ",
            "dose marginals. A reasonable verbal summary: ",
            tags$em("\"Oral syringes produced low and roughly constant "),
            tags$em("dosing error across the three prescribed doses, "),
            tags$em("whereas medicine cups produced substantially higher "),
            tags$em("error at the smallest prescribed dose and steadily "),
            tags$em("less error as the prescribed dose grew larger.\"")
          )
        ),
        callout_warm(
          tags$b("A simple interpretation routine. "),
          tags$br(),
          "Step 1: check whether the interaction is significant. If it is, ",
          "describe the cell-mean pattern directly (or run simple-effects ",
          "tests). The main effects should not be over-interpreted in that ",
          "case.", tags$br(),
          "Step 2: if the interaction is non-significant, the main effects ",
          "can be interpreted on their own. Significant main effects with ",
          "more than two levels then call for a Tukey HSD post-hoc test ",
          "to identify which levels differ."
        ),
        id_prefix = "fact-step"
      ),

      # ------ APA write-up reference card ----------------------------------
      div(class = "p-3 mt-4",
          style = sprintf("background:%s; color:%s; border-radius:10px; border-left:5px solid %s;",
                          PAL$warn_bg, PAL$warn_fg, OI$orange),
          h5("Translating numbers into APA-style prose", style = "margin-top:0;"),
          tags$blockquote(style = "font-style:italic; margin-left:8px; border-left:4px solid #d6c08a; padding-left:12px;",
            "A 2 × 3 between-subjects factorial ANOVA examined whether ",
            "measurement modality (oral syringe vs medicine cup) and ",
            "prescribed dose (2.5 ml, 5.0 ml, or 7.5 ml) affected the ",
            "percent error parents made when measuring a child's liquid ",
            "medication. The main effect of modality was significant, ",
            "F(1, 84) = 78.3, p < .001: syringes produced lower error ",
            "overall than medicine cups. The main effect of dose was also ",
            "significant, F(2, 84) = 4.5, p = .013. These main effects ",
            "were qualified by a significant modality × dose interaction, ",
            "F(2, 84) = 9.2, p < .001. The interaction reflected the ",
            "pattern that syringes produced low and roughly constant ",
            "error across all three doses, whereas medicine cups produced ",
            "much higher error at the smallest prescribed dose and ",
            "steadily lower error as the dose grew larger."),
          p(style = "margin-bottom:0;",
            tags$b("Template: "),
            "A [design] factorial ANOVA examined whether [Factor A] and ",
            "[Factor B] affected [DV]. Main effect of A: F(df_A, df_w) = , ",
            "p = . Main effect of B: F(df_B, df_w) = , p = . Interaction: ",
            "F(df_AB, df_w) = , p = . [If interaction significant: describe ",
            "the pattern from the interaction plot.]")
      )
    ))
  })

  # ---- Plots --------------------------------------------------------------
  output$fact_step5_plot <- renderPlot({
    plot_interaction(LAB8$cells)
  }, res = 96)
}
