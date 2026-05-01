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
