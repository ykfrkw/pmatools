# test-vendor-collisions.R - the app source()s pmatools instead of installing
# it, so the package's helpers and the app's own live in ONE environment and a
# shared name is a silent override rather than an error. The app's files are
# sourced last, so the app wins and the PACKAGE calls the app's function - a
# failure that surfaces far from its cause. It cost a bundle its analysis.R
# once (`.responder_block`, v0.5.1): the app's widget builder answered a call
# meant for the export helper, and the only symptom was a missing file.

library(testthat)

# Top-level `name <- value` assignments of one directory of R sources.
.top_level_names <- function(dir) {
  files <- list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)
  unique(unlist(lapply(files, function(f) {
    exprs <- parse(f)
    out <- character(0)
    for (e in exprs) {
      if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") &&
          is.name(e[[2]])) {
        out <- c(out, as.character(e[[2]]))
      }
    }
    out
  })))
}

# `%||%` is defined by both, identically, and has been since before the repos
# merged. Every other shared name is a bug.
ALLOWED_SHARED_NAMES <- "%||%"

test_that("no app helper shadows a pmatools helper of the same name", {
  pkg_dir <- file.path(dirname(PMA_APP_ROOT), "R")
  skip_if_not(dir.exists(pkg_dir), "package sources not next to the app")

  shared <- intersect(.top_level_names(pkg_dir),
                      .top_level_names(file.path(PMA_APP_ROOT, "R")))
  expect_equal(sort(setdiff(shared, ALLOWED_SHARED_NAMES)), character(0))
})
