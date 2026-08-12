# tests/testthat.R - entry point for `Rscript tests/testthat.R`
#
# This project is a Shiny app, not a package: there is nothing to
# library() or load_all(). tests/testthat/helper-app.R sources the app's own R
# files instead, so the tests below exercise exactly the code the app runs.
#
# Run either of (from the app root):
#   Rscript -e 'testthat::test_dir("tests/testthat")'
#   Rscript tests/testthat.R
local({
  # Walk up from the working directory to the app root, so the entry point
  # works whether it is invoked from the app root or from tests/.
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "app.R"))) {
    up <- dirname(d)
    if (identical(up, d)) stop("could not find app.R above ", getwd())
    d <- up
  }
  testthat::test_dir(file.path(d, "tests", "testthat"))
})
