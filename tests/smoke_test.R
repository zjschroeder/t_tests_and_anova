# =============================================================================
# Smoke test: load the app the way shiny::runApp would, then force every
# tab's renderUI and the main reactive plots so hard errors surface in CI
# instead of in front of students.
#
# Run from the repo root:  Rscript tests/smoke_test.R
# =============================================================================

library(shiny)

app <- shinyAppDir(".")
cat("app.R sourced; static UI built OK\n")

testServer(app, {
  # Force each tab's full UI (this is where scenario text, tables, and
  # data-field references like LAB8$M_modality actually get evaluated).
  for (o in c("paired_tab_ui", "indep_tab_ui", "anova_tab_ui",
              "fact_tab_ui")) {
    v <- output[[o]]
    stopifnot(!is.null(v))
    cat(o, "renders OK\n")
  }
  # Force the tab-1 reactive outputs that build at default input values.
  for (o in c("popPlot", "step7_plot", "pexp_plot", "pexp_readout",
              "step_toc")) {
    v <- output[[o]]
    stopifnot(!is.null(v))
    cat(o, "renders OK\n")
  }
})

cat("SMOKE TEST PASSED\n")
