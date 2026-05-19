# =============================================================================
# shinylive_export.R — build the static-HTML version of the app
#
# Produces a `docs/` directory containing the entire R interpreter (webR),
# the seven CRAN packages we depend on, and the app's source — all compiled
# to WebAssembly. After running this, the contents of `docs/` are a complete,
# self-contained website that can be served by GitHub Pages, Netlify, or any
# static host.
#
# Run from the project root:
#     Rscript shinylive_export.R
# =============================================================================

required_pkgs <- c("shinylive")
missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

suppressMessages(library(shinylive))
message("shinylive version: ", as.character(packageVersion("shinylive")))

# Clean any previous build so we don't ship stale assets
if (dir.exists("docs")) {
  message("Removing previous docs/ build...")
  unlink("docs", recursive = TRUE, force = TRUE)
}

# Build the static site. shinylive picks up app.R and R/*.R automatically.
# The webR interpreter + binary packages are downloaded from repo.r-wasm.org
# at export time and bundled into docs/.
message("Building static Shinylive bundle into docs/ ...")
shinylive::export(appdir = ".", destdir = "docs", quiet = FALSE)

# Add a one-line note GitHub Pages reads to skip Jekyll processing — important
# because Jekyll silently ignores files/directories starting with an
# underscore, and Shinylive uses `_modules` etc.
file.create("docs/.nojekyll")

# Done.
message("\n[OK] Static bundle ready in docs/")
message("    Preview locally:  Rscript -e 'httpuv::runStaticServer(\"docs\")'")
message("    Then deploy:      git add docs .nojekyll && git commit -m 'rebuild' && git push")
