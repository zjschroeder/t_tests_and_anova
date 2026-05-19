# =============================================================================
# deploy.R — push the PSY 302 t-tests & ANOVA Shiny app to shinyapps.io
#
# Run this from the project root *after* you have set your shinyapps.io
# credentials (one-time setup; see SETUP below). The script:
#   1. Verifies all CRAN dependencies are installed.
#   2. Checks that the app boots locally without errors.
#   3. Calls rsconnect::deployApp() with a stable appName.
#
# Files matching patterns in .rscignore are excluded from the bundle.
# =============================================================================

# ---- SETUP (do once) --------------------------------------------------------
# 1. Create a free shinyapps.io account: https://www.shinyapps.io/admin/#/signup
# 2. From the dashboard, copy your Token + Secret (Account → Tokens).
# 3. Run once interactively, replacing placeholders:
#       rsconnect::setAccountInfo(name   = "<your-account>",
#                                 token  = "<token>",
#                                 secret = "<secret>")
#    This persists credentials to ~/.config/R/rsconnect/, so you only have to
#    do it once per machine.

# ---- DEPLOY -----------------------------------------------------------------
required_pkgs <- c("shiny", "bslib", "ggplot2", "plotly", "scales", "rsconnect")
missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

# Sanity-check: confirm the app sources cleanly before uploading.
message("Sourcing app.R to catch errors before deploy...")
src <- readLines("app.R")
end <- which(grepl("^shinyApp\\(", src))[1]
src <- src[seq_len(end - 1)]
tmp <- tempfile(fileext = ".R"); writeLines(src, tmp)
e <- try(sys.source(tmp, envir = new.env(parent = globalenv())), silent = TRUE)
unlink(tmp)
if (inherits(e, "try-error")) {
  stop("app.R failed to source:\n", attr(e, "condition")$message)
}
message("✓ app.R sources OK")

# Deploy. appName is the URL slug under <account>.shinyapps.io/<appName>.
# Change to taste, but keep it stable across deploys so links don't break.
rsconnect::deployApp(
  appDir  = ".",
  appName = "psy302-stats-explainer",
  appTitle = "PSY 302 — t-tests & ANOVA, step by step",
  forceUpdate = TRUE,    # overwrite the existing deployment with the same name
  launch.browser = TRUE  # open the live URL once upload completes
)
