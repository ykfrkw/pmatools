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

test_that("one scale per study pools every study under SMD", {
  # The reason SMD exists: three trials, three depression instruments. The
  # `outcome` column names the instrument, and nothing here is double-counted.
  data <- make_long_continuous()
  data$outcome <- rep(c("PHQ-9", "HAMD", "BDI"), each = 2)
  ma <- run_ma(data, outcome_type = "continuous", sm = "SMD")
  expect_equal(ma$k, length(unique(data$studlab)))
})

test_that("run_ma rejects a study that carries two outcomes", {
  data <- rbind(
    transform(make_long_continuous(), outcome = "ISI"),
    transform(make_long_continuous(), outcome = "TST")
  )
  expect_error(run_ma(data, outcome_type = "continuous"),
               regexp = "more than one outcome for the same study")
  # The message has to name the studies, or the reviewer cannot find them.
  expect_error(run_ma(data, outcome_type = "continuous"), regexp = "A, B, C")
})

test_that("every method.tau choice reaches the fit, and REML is the default", {
  data <- make_long_continuous()
  for (estimator in c("REML", "PM", "DL", "SJ", "ML", "EB")) {
    ma <- run_ma(data, outcome_type = "continuous", sm = "SMD",
                 method.tau = estimator)
    expect_equal(ma$method.tau, estimator, info = estimator)
  }
  expect_equal(run_ma(data, outcome_type = "continuous", sm = "SMD")$method.tau,
               "REML")
})

test_that("hakn decides the random-effects CI, automatically or on request", {
  data <- make_long_continuous()
  two_studies <- data[data$studlab %in% c("A", "B"), , drop = FALSE]

  auto_k3 <- run_ma(data, outcome_type = "continuous", sm = "SMD")
  expect_equal(auto_k3$method.random.ci, "HK")

  auto_k2 <- run_ma(two_studies, outcome_type = "continuous", sm = "SMD")
  expect_equal(auto_k2$method.random.ci, "classic")

  off <- run_ma(data, outcome_type = "continuous", sm = "SMD", hakn = FALSE)
  expect_equal(off$method.random.ci, "classic")

  on <- run_ma(data, outcome_type = "continuous", sm = "SMD", hakn = TRUE)
  expect_equal(on$method.random.ci, "HK")
})

test_that("forcing hakn on below k = 3 warns but is still applied", {
  data <- make_long_continuous()
  two_studies <- data[data$studlab %in% c("A", "B"), , drop = FALSE]
  expect_warning(
    forced <- run_ma(two_studies, outcome_type = "continuous", sm = "SMD",
                     hakn = TRUE),
    regexp = "very wide"
  )
  expect_equal(forced$method.random.ci, "HK")
})
