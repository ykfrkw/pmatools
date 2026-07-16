# deploy.R - Push the Shiny app to shinyapps.io
#
# Usage (from the pairwise_meta_analysis directory):
#   Rscript deploy.R
#
# Architecture: pmatools sources are vendored under R/_pmatools/ and inst
#   files under _pmatools_inst/. The app does not depend on the pmatools
#   package, so shinyapps.io's build server does NOT need to fetch from
#   ykfrkw/pmatools. This bypasses the longstanding HTTP 401 issue caused
#   by a stale GITHUB_PAT cached on the shinyapps.io account.
#
# Maintenance: when pmatools is updated upstream, refresh the vendored
#   copies with `update_vendor.R` (or `cp -R ../pmatools/R/*.R R/_pmatools/`
#   followed by `cp -R ../pmatools/inst/* _pmatools_inst/`).

suppressPackageStartupMessages({
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("rsconnect is required. Install with install.packages('rsconnect').")
  }
})

APP_NAME <- "pairwise_meta_analysis"
ACCOUNT  <- "yuki-furukawa"
APP_DIR  <- normalizePath(".")

cat("=== Step 1/3: prepare clean bundle (.rscignore) ===\n")
rsignore_path <- file.path(APP_DIR, ".rscignore")
writeLines(c(
  "app_*.R",        # historical backups (if any return)
  "prompt.Rmd",
  "SPEC.md",        # design doc - not needed at runtime
  "Core GRADE papers", # 4 MB of reference PDFs
  ".Rproj.user",
  ".DS_Store",
  "*.Rproj",
  "rsconnect",      # local deployment metadata
  ".deploy_excluded",
  "deploy.R",
  "update_vendor.R",
  "*.tar.gz"
), rsignore_path)
cat("   Wrote .rscignore (", length(readLines(rsignore_path)), " patterns)\n", sep = "")

# Files with non-UTF-8 (Latin-1) byte sequences in the filename break
# rsconnect's path scanner. Stash them out of the way during deploy.
hide_dir   <- file.path(APP_DIR, ".deploy_excluded")
hide_paths <- list.files(APP_DIR, pattern = "^app_.*\\.R$", full.names = TRUE)
if (length(hide_paths)) {
  dir.create(hide_dir, showWarnings = FALSE)
  cat("   Hiding", length(hide_paths), "extra files during deploy.\n")
  file.rename(hide_paths, file.path(hide_dir, basename(hide_paths)))
  on.exit({
    if (dir.exists(hide_dir)) {
      moved <- list.files(hide_dir, full.names = TRUE)
      file.rename(moved, file.path(APP_DIR, basename(moved)))
      try(unlink(hide_dir, recursive = TRUE), silent = TRUE)
    }
  }, add = TRUE)
}

cat("\n=== Step 2/3: deploy to shinyapps.io ===\n")
Sys.setenv(LANG = "en_US.UTF-8", LC_ALL = "en_US.UTF-8")
result <- tryCatch({
  rsconnect::deployApp(
    appDir         = APP_DIR,
    appName        = APP_NAME,
    account        = ACCOUNT,
    forceUpdate    = TRUE,
    launch.browser = FALSE,
    quarto         = FALSE
  )
  TRUE
}, error = function(e) {
  cat("\n!!! Deploy FAILED:\n   ", conditionMessage(e), "\n", sep = "")
  FALSE
})

cat("\n=== Step 3/3: cleanup ===\n")
if (file.exists(rsignore_path)) {
  unlink(rsignore_path)
  cat("   Removed .rscignore\n")
}

if (isTRUE(result)) {
  cat("\nSUCCESS. App live at https://", ACCOUNT, ".shinyapps.io/", APP_NAME, "/\n",
      sep = "")
} else {
  cat("\nFAIL. Inspect the log above and the shinyapps.io dashboard.\n")
  quit(status = 1)
}
