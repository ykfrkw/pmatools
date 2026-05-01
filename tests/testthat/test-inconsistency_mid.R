library(testthat)

skip_if_not_installed("meta")

make_mock_meta <- function(te_vec, i2 = 0.5, tau2 = 0.05) {
  m <- list(
    k            = length(te_vec),
    TE           = te_vec,
    seTE         = rep(0.20, length(te_vec)),
    TE.random    = mean(te_vec),
    lower.random = mean(te_vec) - 0.3,
    upper.random = mean(te_vec) + 0.3,
    I2           = i2,
    tau2         = tau2,
    pval.Q       = 0.05,
    sm           = "OR",
    event.e      = rep(20, length(te_vec)),
    event.c      = rep(15, length(te_vec)),
    n.e          = rep(100, length(te_vec)),
    n.c          = rep(100, length(te_vec)),
    studlab      = paste0("S", seq_along(te_vec)),
    data         = NULL,
    w.random     = rep(1, length(te_vec))
  )
  class(m) <- "meta"
  m
}

# ---- Manual flowchart paths ----

test_that("ci_diff = 'no' -> judgment 'no'", {
  m <- make_mock_meta(c(0.2, 0.3, 0.4), i2 = 0.6)
  g <- grade_meta(m, inconsistency_ci_diff = "no")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
  expect_false(row$auto)
})

test_that("majority_one_side -> 'no'", {
  m <- make_mock_meta(c(0.2, 0.3, 0.4), i2 = 0.6)
  g <- grade_meta(m,
    inconsistency_ci_diff        = "yes",
    inconsistency_threshold_side = "majority_one_side"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
})

test_that("opposite_sides + subgroup explained -> 'no'", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m,
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "yes"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
})

test_that("opposite_sides + no subgroup -> 'serious'", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m,
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "no"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "serious")
})

# ---- Auto path: I^2 only (no Q-test) ----

test_that("auto Step 1: I^2 <= 25% -> 'no' regardless of Q p", {
  m <- make_mock_meta(c(0.1, 0.1, 0.1), i2 = 0.20, tau2 = 0)
  g <- grade_meta(m)
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
  expect_true(row$auto)
})

test_that("auto Step 1: I^2 > 25% triggers Step 2", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.60)
  g <- grade_meta(m)
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  # opposite-sided TEs -> 'serious'
  expect_equal(row$judgment, "serious")
})

# ---- Auto Step 2 with MID ----

test_that("auto Step 2 with MID: all studies above MID -> majority_one_side -> 'some'", {
  # All TE > +log(1.2) = 0.182  (above_mid)
  m <- make_mock_meta(c(0.30, 0.40, 0.50), i2 = 0.60)
  g <- grade_meta(m, mid = 1.20, mid_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "some")
  expect_true(grepl("vs +/-MID", row$notes, fixed = TRUE))
})

test_that("auto Step 2 with MID: zone tally distinguishes opposite from majority", {
  # TE values: 1 above, 1 below, 1 trivial -> opposite sides
  m <- make_mock_meta(c(0.30, -0.30, 0.0), i2 = 0.70)
  g <- grade_meta(m, mid = 1.20, mid_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "serious")
})

# ---- Auto Step 2 without MID (null=0 fallback) ----

test_that("auto Step 2 without MID: all TE > 0 -> majority_one_side -> 'some'", {
  m <- make_mock_meta(c(0.20, 0.30, 0.40), i2 = 0.60)
  g <- grade_meta(m)
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "some")
  expect_true(grepl("vs null", row$notes))
})
