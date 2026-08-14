# test-version-stamp.R — the pmatools version stamp written into export
# bundles (results.txt and the header of the generated analysis.R).
#
# A host that vendors the pmatools sources (source()s R/*.R rather than
# installing the package) is invisible to utils::packageVersion(), so it
# declares its version through options(pmatools.version_stamp = "x.y.z").
# .vendored_version_stamp() validates that option; .pmatools_version() puts
# the installed version first and falls back to it.
#
# The option is saved and restored with on.exit(). withr became a declared
# Suggests in 0.5.1, so withr::local_options() is available here too; the
# on.exit() form is kept because it is correct as it stands and rewriting it
# would change nothing a reader or a test can observe.

library(testthat)

test_that("an unset option yields the unknown-version string", {
  old <- options(pmatools.version_stamp = NULL)
  on.exit(options(old), add = TRUE)

  expect_identical(pmatools:::.vendored_version_stamp(),
                   "(vendored; version unknown)")
})

test_that("a valid stamp is suffixed with ' (vendored)'", {
  old <- options(pmatools.version_stamp = "0.5.1")
  on.exit(options(old), add = TRUE)

  expect_identical(pmatools:::.vendored_version_stamp(), "0.5.1 (vendored)")

  # Surrounding whitespace is trimmed rather than rejected.
  options(pmatools.version_stamp = "  0.5.1  ")
  expect_identical(pmatools:::.vendored_version_stamp(), "0.5.1 (vendored)")
})

test_that("unusable option values fall back to the unknown-version string", {
  old <- options(pmatools.version_stamp = NULL)
  on.exit(options(old), add = TRUE)

  bad <- list(
    NULL,
    numeric_version("0.5.1"),   # not a character vector
    0.51,                       # numeric
    TRUE,                       # logical
    c("0.5.1", "0.5.2"),        # length 2
    character(0),               # length 0
    "",                         # empty string
    "   ",                      # blank after trimws()
    NA_character_,              # NA character
    NA                          # logical NA
  )

  for (i in seq_along(bad)) {
    options(pmatools.version_stamp = bad[[i]])
    expect_identical(pmatools:::.vendored_version_stamp(),
                     "(vendored; version unknown)",
                     info = sprintf("bad value #%d", i))
  }
})

test_that("the argument form validates the same way as the option", {
  # .vendored_version_stamp() takes the option as a default argument, so the
  # validation can be exercised without touching the session options.
  expect_identical(pmatools:::.vendored_version_stamp("0.4.0"),
                   "0.4.0 (vendored)")
  expect_identical(pmatools:::.vendored_version_stamp(NULL),
                   "(vendored; version unknown)")
  expect_identical(pmatools:::.vendored_version_stamp(list("0.4.0")),
                   "(vendored; version unknown)")
})

test_that(".pmatools_version() prefers the installed version over the option", {
  installed <- tryCatch(as.character(utils::packageVersion("pmatools")),
                        error = function(e) NULL)
  skip_if(is.null(installed), "pmatools is not resolvable as an installed package")

  old <- options(pmatools.version_stamp = "9.9.9")
  on.exit(options(old), add = TRUE)

  expect_identical(pmatools:::.pmatools_version(), installed)
  expect_false(grepl("vendored", pmatools:::.pmatools_version(), fixed = TRUE))
})

test_that(".pmatools_version() falls back to the stamp when lookup fails", {
  # Stand-in for the vendored case, where packageVersion() errors.
  fallback <- function(stamp) {
    old <- options(pmatools.version_stamp = stamp)
    on.exit(options(old), add = TRUE)
    tryCatch(stop("there is no package called 'pmatools'"),
             error = function(e) pmatools:::.vendored_version_stamp())
  }

  expect_identical(fallback("0.5.1"), "0.5.1 (vendored)")
  expect_identical(fallback(NULL), "(vendored; version unknown)")
})
