library(testthat)

skip_if_not_installed("meta")

# Mock dominated meta object (cf. test-grade_meta.R)
make_mock_dominated <- function(te_all, te_low_only) {
  m <- list(
    k            = 3L,
    w.random     = c(80, 10, 10),
    TE           = c(te_all, te_low_only, te_low_only),
    seTE         = c(0.10, 0.45, 0.45),
    TE.random    = te_all,
    lower.random = te_all - 0.4,
    upper.random = te_all + 0.4,
    sm           = "RR",
    I2           = 0.10,
    tau2         = 0.01,
    pval.Q       = 0.30,
    event.e      = c(40, 5, 5),
    event.c      = c(10, 4, 4),
    n.e          = c(200, 20, 20),
    n.c          = c(200, 20, 20),
    studlab      = c("Large-A", "Small-B", "Small-C"),
    data         = NULL
  )
  class(m) <- "meta"
  m
}

test_that("inflation threshold 0.10 blocks rate-down for small inflation", {
  # Modest inflation: te_all=1.05, te_low=1.00 -> ratio = 5%
  m <- make_mock_dominated(te_all = 1.05, te_low_only = 1.00)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_true(grepl("inflation below threshold", rob_row$notes))
})

test_that("inflation threshold 0.10 allows rate-down for large inflation", {
  # Large inflation: te_all=1.4, te_low=0.3 -> ratio = 367%
  m <- make_mock_dominated(te_all = 1.4, te_low_only = 0.3)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
})

test_that("inflation threshold 0 (v0.1.0 compat) rates down for any inflation", {
  m <- make_mock_dominated(te_all = 1.05, te_low_only = 1.00)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
})

test_that("small_values = NULL uses |TE| comparison", {
  # |te_all|=1.4 > |te_low|=0.3 -> direction_ok TRUE; inflation 367% > 10% -> serious
  m <- make_mock_dominated(te_all = 1.4, te_low_only = 0.3)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
})

test_that("small_values = NULL: high-RoB toward null does NOT rate down", {
  # |te_all|=0.2 < |te_low|=1.1 -> direction_ok FALSE -> no
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
})
