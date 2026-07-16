# deploy.R - Push the Shiny app to shinyapps.io
#
# =============================================================================
# !! DEPENDENCY LIFELINE - READ BEFORE TOUCHING DESCRIPTION !!
#
# The rare-event backends `metafor`, `mmeta`, and `BiasedUrn` are referenced
# ONLY as name strings in R/_pmatools/rare_events.R (e.g. package = "metafor",
# extra_packages = "BiasedUrn", engine = "mmeta"), so rsconnect's static code
# scan CANNOT detect them. The app DESCRIPTION's Imports field is the ONLY
# mechanism that gets them installed on the shinyapps.io server. They MUST
# stay listed in the app DESCRIPTION Imports, even if grep for `pkg::` finds
# nothing. Removing them deploys fine but breaks the rare-event method suite
# at runtime. Run `Rscript update_vendor.R --check-only` to audit.
# =============================================================================
#
# Usage (from the pairwise_meta_analysis directory):
#   Rscript deploy.R
#
# Bundle exclusions live in the permanent, committed .rscignore at the repo
# root (one glob pattern per line; rsconnect reads it at deploy time). Edit
# that file -- this script no longer generates or deletes it.
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

cat("=== Step 1/3: verify clean-bundle config (.rscignore) ===\n")
rsignore_path <- file.path(APP_DIR, ".rscignore")
if (!file.exists(rsignore_path)) {
  stop(".rscignore is missing from ", APP_DIR, ". It is a permanent, ",
       "committed file that keeps SPEC.md, reference PDFs, and local ",
       "metadata out of the deploy bundle. Restore it with ",
       "`git checkout -- .rscignore` before deploying.")
}
cat("   Found .rscignore (", length(readLines(rsignore_path)),
    " patterns)\n", sep = "")

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

cat("\n=== Step 3/3: report ===\n")

if (isTRUE(result)) {
  cat("\nSUCCESS. App live at https://", ACCOUNT, ".shinyapps.io/", APP_NAME, "/\n",
      sep = "")
} else {
  cat("\nFAIL. Inspect the log above and the shinyapps.io dashboard.\n")
  quit(status = 1)
}
