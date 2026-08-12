# tests/testthat.R - entry point for `Rscript tests/testthat.R`
#
# This project is a Shiny app, not a package: there is nothing to
# library() or load_all(). tests/testthat/helper-app.R sources the app's own R
# files instead, so the tests below exercise exactly the code the app runs.
#
# Run any of (from anywhere in the repo):
#   Rscript shiny/tests/testthat.R
#   Rscript -e 'testthat::test_dir("shiny/tests/testthat")'
local({
  # Start the walk at this script, not at getwd(). The app is a subdirectory
  # of the pmatools package repo now, so walking up from the working directory
  # only finds app.R when you happen to have cd'd into shiny/ first -- and
  # from the repo root it walks past the app entirely and off the top.
  start <- {
    f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(f) > 0L) dirname(sub("^--file=", "", f[1L])) else getwd()
  }
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "app.R"))) {
    up <- dirname(d)
    if (identical(up, d)) stop("could not find app.R above ", start)
    d <- up
  }
  testthat::test_dir(file.path(d, "tests", "testthat"))
})
