library(testthat)

skip_if_not_installed("meta")

make_long_binary <- function() {
  data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
}

make_long_continuous <- function() {
  data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    mean    = c(10, 12, 11, 13, 9, 11),
    sd      = c(3, 3, 3, 3, 3, 3),
    stringsAsFactors = FALSE
  )
}

test_that("run_ma binary returns meta object", {
  data <- make_long_binary()
  ma <- run_ma(data, outcome_type = "binary", sm = "OR")
  expect_s3_class(ma, "meta")
  expect_equal(ma$k, 3)
})

test_that("run_ma binary RR + MH", {
  data <- make_long_binary()
  ma <- run_ma(data, outcome_type = "binary", sm = "RR", method = "MH")
  expect_s3_class(ma, "meta")
})

test_that("run_ma continuous SMD", {
  data <- make_long_continuous()
  ma <- run_ma(data, outcome_type = "continuous", sm = "SMD")
  expect_s3_class(ma, "meta")
  expect_true(!is.null(ma$TE.random))
})

test_that("run_ma continuous MD", {
  data <- make_long_continuous()
  ma <- run_ma(data, outcome_type = "continuous", sm = "MD")
  expect_s3_class(ma, "meta")
})

test_that("run_ma rejects invalid sm", {
  data <- make_long_binary()
  expect_error(run_ma(data, outcome_type = "binary", sm = "SMD"),
               regexp = "not valid")
})

make_long_binary_custom <- function() {
  # Non-standard treat labels; alphabetical order: cbt < placebo
  data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("cbt", "placebo"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
}

test_that("alphabetical arm fallback emits a warning stating the assignment", {
  data <- make_long_binary_custom()
  expect_warning(
    run_ma(data, outcome_type = "binary", sm = "OR"),
    regexp = "assigned alphabetically.*control = 'cbt'.*experimental = 'placebo'"
  )
})

test_that("no fallback warning when arm labels are supplied", {
  data <- make_long_binary_custom()
  expect_no_warning(
    run_ma(data, outcome_type = "binary", sm = "OR",
           experimental_label = "cbt", control_label = "placebo")
  )
})

test_that("effect direction respects explicit arm labels", {
  data <- make_long_binary_custom()
  ma_fallback <- suppressWarnings(
    run_ma(data, outcome_type = "binary", sm = "OR")
  )  # control = 'cbt', experimental = 'placebo'
  ma_explicit <- run_ma(data, outcome_type = "binary", sm = "OR",
                        experimental_label = "cbt",
                        control_label      = "placebo")

  # Swapping arms flips the log OR sign
  expect_equal(ma_explicit$TE.random, -ma_fallback$TE.random,
               tolerance = 1e-10)
  # cbt has fewer events -> OR(cbt vs placebo) < 1
  expect_lt(ma_explicit$TE.random, 0)
})

test_that("run_ma validates supplied arm labels", {
  data <- make_long_binary_custom()
  expect_error(
    run_ma(data, outcome_type = "binary", sm = "OR",
           experimental_label = "drug", control_label = "placebo"),
    regexp = "not found in treat values"
  )
  expect_error(
    run_ma(data, outcome_type = "binary", sm = "OR",
           experimental_label = "cbt", control_label = "cbt"),
    regexp = "must be distinct"
  )
})

test_that("run_ma infers the other arm when only one label is supplied", {
  data <- make_long_binary_custom()
  ma <- run_ma(data, outcome_type = "binary", sm = "OR",
               experimental_label = "cbt")
  ma_full <- run_ma(data, outcome_type = "binary", sm = "OR",
                    experimental_label = "cbt", control_label = "placebo")
  expect_equal(ma$TE.random, ma_full$TE.random)
})

test_that("ingest_data validates experimental/control labels", {
  data <- make_long_binary_custom()
  expect_error(
    ingest_data(data, format = "long",
                experimental_label = "drug", control_label = "placebo"),
    regexp = "not found in treat values"
  )
  expect_error(
    ingest_data(data, format = "long",
                experimental_label = "cbt", control_label = "cbt"),
    regexp = "must be distinct"
  )
  ok <- ingest_data(data, format = "long",
                    experimental_label = "cbt", control_label = "placebo")
  expect_setequal(unique(ok$treat), c("experimental", "control"))
})

test_that("run_ma rejects unfiltered multi-outcome data", {
  data <- rbind(
    transform(make_long_continuous(), outcome = "ISI"),
    transform(make_long_continuous(), outcome = "TST")
  )
  expect_error(run_ma(data, outcome_type = "continuous"),
               regexp = "multiple outcomes")
})
