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
# at runtime. Run `Rscript shiny/stage_bundle.R --check-only` to audit.
# =============================================================================
#
# Usage (from anywhere in the repo):
#   Rscript shiny/deploy.R
#
# Bundle exclusions live in the permanent, committed .rscignore beside this
# script (one glob pattern per line; rsconnect reads it at deploy time). Edit
# that file -- this script no longer generates or deletes it.
#
# Architecture: the app SOURCES pmatools rather than installing it, because a
#   stale GITHUB_PAT cached on the shinyapps.io account makes
#   install_github(ykfrkw/pmatools) return HTTP 401 on the build server. So
#   the bundle carries the package sources itself, staged from ../R and
#   ../inst into R/_pmatools/ and _pmatools_inst/ by stage_bundle.R.
#
#   Those two directories are GENERATED AND GITIGNORED, so a fresh clone does
#   not have them. Step 2 below runs the staging itself rather than trusting
#   whoever is deploying to remember -- which keeps "clone, then one command"
#   true and makes a stale bundle impossible.

suppressPackageStartupMessages({
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("rsconnect is required. Install with install.packages('rsconnect').")
  }
})

APP_NAME <- "pmatools"
ACCOUNT  <- "yuki-furukawa"

# Self-locating: `Rscript shiny/deploy.R` from the repo root must deploy the
# app, not the package. normalizePath(".") would have made appDir the repo
# root -- silently, and the resulting bundle has no app.R at its top level.
.script_dir <- function() {
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f) == 0L) return(normalizePath("."))
  normalizePath(dirname(sub("^--file=", "", f[1L])))
}
APP_DIR  <- .script_dir()
PKG_ROOT <- normalizePath(file.path(APP_DIR, ".."))
setwd(APP_DIR)

cat("=== Step 1/5: refuse a dirty tree ===\n")
# deployApp() ships the working tree as it stands, so deploying from a dirty
# repo puts bytes in production that no commit describes -- and the bundle is
# then unreproducible exactly when someone needs to reproduce it.
.porcelain <- suppressWarnings(system2(
  "git", c("-C", shQuote(PKG_ROOT), "status", "--porcelain"),
  stdout = TRUE, stderr = FALSE))
.dirty <- .porcelain[nzchar(trimws(.porcelain))]
if (length(.dirty) > 0L) {
  stop("Repo is dirty; commit or stash before deploying:\n  ",
       paste(utils::head(.dirty, 10L), collapse = "\n  "),
       if (length(.dirty) > 10L)
         paste0("\n  ... and ", length(.dirty) - 10L, " more") else "",
       call. = FALSE)
}
.head <- suppressWarnings(system2(
  "git", c("-C", shQuote(PKG_ROOT), "rev-parse", "--short", "HEAD"),
  stdout = TRUE, stderr = FALSE))
cat("   Clean. Deploying ", PKG_ROOT, " @ ", .head[1L], "\n", sep = "")

cat("\n=== Step 2/5: stage the pmatools sources into the bundle ===\n")
# Warnings promoted to errors on purpose: stage_bundle.R warns rather than
# stops for a missing app-DESCRIPTION dependency and for a template lookup its
# rewrite missed. Both are invisible locally and fail only at runtime, in
# production -- exactly the class of problem a deploy gate exists to catch.
withCallingHandlers(
  source(file.path(APP_DIR, "stage_bundle.R"), local = new.env()),
  warning = function(w) {
    stop("stage_bundle.R reported a problem that would only surface in ",
         "production:\n  ", conditionMessage(w), call. = FALSE)
  }
)

cat("\n=== Step 3/5: verify clean-bundle config (.rscignore) ===\n")
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

cat("\n=== Step 4/5: deploy to shinyapps.io ===\n")
Sys.setenv(LANG = "en_US.UTF-8", LC_ALL = "en_US.UTF-8")
result <- tryCatch({
  rsconnect::deployApp(
    appDir         = APP_DIR,
    appName        = APP_NAME,
    account        = ACCOUNT,
    # Two deployment records coexist in rsconnect/ during the migration off
    # the old pairwise_meta_analysis app; naming the full triple leaves
    # rsconnect no room to resolve the wrong one.
    server         = "shinyapps.io",
    forceUpdate    = TRUE,
    launch.browser = FALSE,
    quarto         = FALSE
  )
  TRUE
}, error = function(e) {
  cat("\n!!! Deploy FAILED:\n   ", conditionMessage(e), "\n", sep = "")
  FALSE
})

cat("\n=== Step 5/5: report ===\n")

if (isTRUE(result)) {
  cat("\nSUCCESS. App live at https://", ACCOUNT, ".shinyapps.io/", APP_NAME, "/\n",
      sep = "")
} else {
  cat("\nFAIL. Inspect the log above and the shinyapps.io dashboard.\n")
  quit(status = 1)
}
