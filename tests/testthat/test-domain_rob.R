library(testthat)

skip_if_not_installed("meta")

# Mock dominated meta object (cf. test-grade_meta.R)
# `seTE.random` is set so the new (v0.3.2+) CI-overlap and significance-change
# branches can be exercised. Tighten/loosen it across tests to control overlap.
make_mock_dominated <- function(te_all, te_low_only,
                                seTE.random = 0.10,
                                seTE        = c(0.10, 0.45, 0.45)) {
  m <- list(
    k            = 3L,
    w.random     = c(80, 10, 10),
    TE           = c(te_all, te_low_only, te_low_only),
    seTE         = seTE,
    TE.random    = te_all,
    seTE.random  = seTE.random,
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
  # CIs nearly identical at this scale -> overlap path or "below threshold"
  expect_true(grepl("overlap|below threshold", rob_row$notes))
})

test_that("large inflation rates down to some_concerns (v0.3+; sign-flip required for serious)", {
  # Large inflation: te_all=1.4, te_low=0.3 -> ratio = 367%, signs both positive (no flip)
  # Tight CIs to keep overlap < 0.8.
  m <- make_mock_dominated(te_all = 1.4, te_low_only = 0.3,
                           seTE.random = 0.05, seTE = c(0.05, 0.20, 0.20))
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
})

test_that("sign flip from removing high-RoB rates down to serious", {
  # te_all positive, te_low negative -> sign flip -> "serious"
  m <- make_mock_dominated(te_all = 1.0, te_low_only = -0.5)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
})

test_that("inflation threshold 0 (v0.1.0 compat) rates down for any inflation", {
  m <- make_mock_dominated(te_all = 1.05, te_low_only = 1.00,
                           seTE.random = 0.05, seTE = c(0.05, 0.20, 0.20))
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
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

# v0.3.2+ — nmatools-derived branches ----------------------------------------

test_that("CI-overlap >= 0.8 forces 'no' even with inflation > threshold", {
  # 40% inflation in point estimates, similar wide CIs -> overlap_ratio ~0.83.
  m <- make_mock_dominated(te_all = 1.4, te_low_only = 1.0,
                           seTE.random = 1.0, seTE = c(1.0, 1.0, 1.0))
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_true(grepl("overlap", rob_row$notes))
})

test_that("Significance change rates down to some_concerns even when inflation < threshold", {
  # Modest inflation (8%, < 10%), but excluding high-RoB shifts the CI to no
  # longer cross null. te_all=0.27, se=0.20 -> CI [-0.12, 0.66] (crosses null);
  # te_low=0.25 with very tight se=0.05 -> CI [0.15, 0.35] (does not cross null).
  m <- make_mock_dominated(te_all = 0.27, te_low_only = 0.25,
                           seTE.random = 0.20,
                           seTE = c(0.20, 0.05, 0.05))
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_true(grepl("significance changes", rob_row$notes))
})

test_that("weight_note reports both count % and weight %", {
  m <- make_mock_dominated(te_all = 1.05, te_low_only = 1.00)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob_row$notes, "by count")
  expect_match(rob_row$notes, "by weight")
})

test_that("diff_note no longer prints |TE_all| / |TE_low|", {
  m <- make_mock_dominated(te_all = 1.4, te_low_only = 0.3,
                           seTE.random = 0.05, seTE = c(0.05, 0.20, 0.20))
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_false(grepl("\\|TE_all\\|", rob_row$notes))
  expect_false(grepl("\\|TE_low\\|", rob_row$notes))
  expect_match(rob_row$notes, "relative inflation")
})
