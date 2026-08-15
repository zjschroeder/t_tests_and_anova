# =============================================================================
# Consistency check for the PSY 302 t-tests & ANOVA app.
#
# The five tabs share near-identical scaffolding, which makes it easy for
# text from one tab's scenario to leak into another during bulk edits. This
# script catches the leaks (and a few other recurring slip-ups) before they
# reach students.
#
# Run from the repo root:  Rscript dev/consistency_check.R
# Exits nonzero when a check fails, so CI can gate on it.
# =============================================================================

files <- c("app.R", "R/tab_paired.R", "R/tab_indep.R", "R/tab_anova.R",
           "R/tab_factorial.R")
problems <- character(0)
note <- function(...) problems <<- c(problems, sprintf(...))

# ---- 1. Every file parses ---------------------------------------------------
for (f in files) {
  r <- try(parse(f), silent = TRUE)
  if (inherits(r, "try-error"))
    note("%s: does not parse: %s", f, conditionMessage(attr(r, "condition")))
}

# ---- 2. Cross-scenario term leaks ------------------------------------------
# Each tab file may only talk about its own scenario (walkthrough + quest).
# app.R is excluded: it hosts the navbar and describes all five tabs.
leak_terms <- list(
  "R/tab_paired.R"    = c("peas", "\\bDaves\\b", "\\bchips\\b", "caffeine",
                          "extravert", "introvert", "neuroticism",
                          "medicine cup", "syringe", "probiotic",
                          "within-person", "participants"),
  "R/tab_indep.R"     = c("peas", "seagull", "\\bchips\\b", "\\bbags?\\b",
                          "caffeine", "neuroticism",
                          "medicine cup", "syringe", "consumer-protection",
                          "clinics"),
  "R/tab_anova.R"     = c("peas", "seagull", "\\bDaves\\b", "\\bchips\\b",
                          "caffeine", "extravert", "introvert",
                          "medicine cup", "syringe", "probiotic"),
  "R/tab_factorial.R" = c("peas", "seagull", "\\bDaves\\b", "\\bchips\\b",
                          "caffeine", "extravert", "introvert", "personality",
                          "neuroticism", "\\bmg\\b", "probiotic")
)
for (f in names(leak_terms)) {
  lines <- readLines(f, warn = FALSE)
  for (term in leak_terms[[f]]) {
    hits <- grep(term, lines, ignore.case = TRUE)
    for (h in hits)
      note("%s:%d: scenario leak '%s': %s", f, h, term,
           trimws(substr(lines[h], 1, 90)))
  }
}

# ---- 3. Data-field references resolve --------------------------------------
# Catches things like LAB8$M_personality when the list only defines
# M_modality (which silently renders a blank column). The tab files' top
# level only defines data blocks and functions, so sourcing them in an
# isolated env is safe.
lab_alias <- c(LAB5 = "R/tab_indep.R", LAB6 = "R/tab_paired.R",
               LAB7 = "R/tab_anova.R", LAB8 = "R/tab_factorial.R")
for (alias in names(lab_alias)) {
  f   <- lab_alias[[alias]]
  env <- new.env(parent = globalenv())
  r <- try(sys.source(f, envir = env), silent = TRUE)
  if (inherits(r, "try-error")) {
    note("%s: cannot source top level: %s", f,
         conditionMessage(attr(r, "condition")))
    next
  }
  if (!exists(alias, envir = env)) {
    note("%s: expected data alias %s not defined", f, alias)
    next
  }
  fields <- names(get(alias, envir = env))
  lines  <- readLines(f, warn = FALSE)
  for (i in seq_along(lines)) {
    for (m in regmatches(lines[i],
                         gregexpr(paste0(alias, "\\$[A-Za-z_.][A-Za-z0-9_.]*"),
                                  lines[i]))[[1]]) {
      fld <- sub(paste0(alias, "\\$"), "", m)
      if (!fld %in% fields)
        note("%s:%d: %s references missing field '%s'", f, i, alias, fld)
    }
  }
}

# ---- 4. Recurring text slip-ups --------------------------------------------
for (f in files) {
  lines <- readLines(f, warn = FALSE)
  # Raw LaTeX in plain strings: "\\approx" outside math_inline/math_block
  # renders as literal backslash text.
  hits <- grep('\\\\\\\\approx', lines)
  hits <- hits[!grepl("math_inline|math_block", lines[hits])]
  for (h in hits)
    note("%s:%d: raw \\approx outside a math helper", f, h)
  # Sentences that start lowercase with a scenario name (find-replace tell).
  hits <- grep('"the (Daves|seagull|mood-map|dosing|pea) study', lines)
  for (h in hits)
    note("%s:%d: sentence starts lowercase: %s", f, h,
         trimws(substr(lines[h], 1, 90)))
  # Three-digit hex colors break older ggplot2/grid.
  hits <- grep('"#[0-9a-fA-F]{3}"', lines)
  for (h in hits)
    note("%s:%d: 3-digit hex color (use 6 digits): %s", f, h,
         trimws(substr(lines[h], 1, 90)))
  # Notation convention: standard deviation is SD, standard error is SE.
  # Old s-notation (s_M, s_D, s_1, s_2, s_p, s_{...}) must not reappear in
  # display strings. Code variable names (s_D <- ..., LAB6$s_D) are fine.
  for (h in seq_along(lines)) {
    strs <- regmatches(lines[h], gregexpr('"[^"]*"', lines[h]))[[1]]
    if (any(grepl('\\bs_[MD12p{]', strs)))
      note("%s:%d: old s-notation in display text (use SD/SE): %s", f, h,
           trimws(substr(lines[h], 1, 90)))
  }
}

# ---- Report -----------------------------------------------------------------
if (length(problems)) {
  cat("CONSISTENCY CHECK FAILED:\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
  quit(status = 1)
} else {
  cat("Consistency check passed:", length(files), "files clean.\n")
}
