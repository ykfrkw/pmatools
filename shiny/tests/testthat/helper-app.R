# helper-app.R - make the app's pure helpers available to the tests.
#
# There is no package here, so nothing can be library()'d. The files below are
# sourced straight out of R/, in the order app.R sources them, and every one of
# them only DEFINES things at source time - no Shiny session is created, no
# reactive is touched. That is why only these three are listed: R/step1_data.R,
# R/step2_ma.R, R/step3_grade.R and R/step4_export.R define server functions
# whose bodies need `input` / `output` / `session` / `state`, and testing those
# would need a Shiny test harness this app does not have.

# testthat::test_dir() sets the working directory to tests/testthat before
# sourcing helpers, so walking up from getwd() lands on the app root -- which
# is shiny/ now that the app is a subdirectory of the package repo, not the
# repo root. tests/testthat.R starts its own walk from the script path
# instead, because it runs before test_dir() has set anything.
PMA_APP_ROOT <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "app.R"))) {
    up <- dirname(d)
    if (identical(up, d)) stop("could not find app.R above ", getwd())
    d <- up
  }
  d
})

for (.f in c("R/ui_helpers.R", "R/educational_copy.R", "R/step3_threshold.R")) {
  source(file.path(PMA_APP_ROOT, .f))
}
rm(.f)
