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
  g <- grade_meta(m, inconsistency_ci_diff = "no", threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
  expect_false(row$auto)
})

test_that("majority_one_side -> 'no'", {
  m <- make_mock_meta(c(0.2, 0.3, 0.4), i2 = 0.6)
  g <- grade_meta(m,
    inconsistency_ci_diff        = "yes",
    inconsistency_threshold_side = "majority_one_side", threshold_type = "null"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
})

test_that("opposite_sides + subgroup explained -> 'no'", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m,
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "yes", threshold_type = "null"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
})

# Updated (v0.5.1): Core GRADE 3 declines to endorse a two-level inconsistency
# downgrade, so every automated / flowchart path caps at -1.
test_that("opposite_sides + no subgroup -> 'some_concerns' (capped at -1)", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m,
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "no", threshold_type = "null"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "some_concerns")
  expect_equal(row$downgrade, -1L)
  expect_match(row$notes, "capped at one level", fixed = TRUE)
})

test_that("the scalar override is the only route to -2 for inconsistency", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m, threshold_type = "null", inconsistency = "serious",
                  inconsistency_rationale = "Directions of effect irreconcilable")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -2L)
})

# ---- Auto path: I^2 only (no Q-test) ----

# Cut-off updated 25% -> 30% (v0.5.1). Reason: 30% is the only numeric value
# Core GRADE 3 puts on paper ("one will seldom see serious inconsistency with
# I2 values <30%"); 25% had no source. I^2 = 0.28 is the new pass case and
# would have rated down under the old cut-off.
test_that("auto Step 1: I^2 <= 30% -> 'no' regardless of Q p", {
  m <- make_mock_meta(c(0.1, 0.1, 0.1), i2 = 0.20, tau2 = 0)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
  expect_true(row$auto)
  expect_match(row$notes, "I2 <= 30%", fixed = TRUE)
})

test_that("I^2 between the old 25% and the new 30% cut-off no longer rates down", {
  # Opposite-sided TEs: under the pre-v0.5.1 cut-off this reached Step 2 and
  # returned 'some_concerns'. It now stops at Step 1.
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.28)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
})

test_that("the auto note names the I^2 gate as a surrogate for visual inspection", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.60)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_match(row$notes, "visual inspection of forest plots", fixed = TRUE)
  expect_match(row$notes, "CINeMA", fixed = TRUE)
  expect_match(row$notes, "ICEMAN", fixed = TRUE)
})

test_that("auto Step 1: I^2 > 30% triggers Step 2", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.60)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  # opposite-sided TEs -> rate down, capped at one level (v0.5.1)
  expect_equal(row$judgment, "some_concerns")
})

# ---- Auto Step 2 with Threshold ----

test_that("auto Step 2 with Threshold: all studies above Threshold -> majority_one_side -> 'no'", {
  # All TE > +log(1.2) = 0.182 -> all in 'above' zone -> single zone share = 100%
  # >= 80% one-side share -> consistent direction -> do not rate down (matches
  # manual flowchart's 'majority_one_side -> no' branch in BMJ Core GRADE 3).
  m <- make_mock_meta(c(0.30, 0.40, 0.50), i2 = 0.60)
  g <- grade_meta(m, threshold = 1.20, threshold_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
  expect_true(grepl("vs +/-Threshold", row$notes, fixed = TRUE))
})

test_that("auto Step 2 with Threshold: zone tally distinguishes opposite from majority", {
  # TE values: 1 above, 1 below, 1 trivial -> opposite sides (capped at -1)
  m <- make_mock_meta(c(0.30, -0.30, 0.0), i2 = 0.70)
  g <- grade_meta(m, threshold = 1.20, threshold_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "some_concerns")
  expect_match(row$notes, "clinically opposite", fixed = TRUE)
})

# ---- Chosen threshold shared with Imprecision (Core GRADE 3 Fig 2) ---------

test_that("inconsistency and imprecision use the SAME chosen threshold", {
  # threshold_type = "null" with a point estimate beyond the MID resolves the
  # rating target to non_null_effect, whose chosen threshold is the null.
  # Before v0.5.1 Inconsistency still received the raw MID here.
  m <- make_mock_meta(c(0.30, -0.30, 0.0), i2 = 0.70)
  m$TE.random <- 0.60   # |TE| > log(1.2) -> non_null_effect
  g <- grade_meta(m, threshold = 1.20, threshold_scale = "ratio",
                  threshold_type = "null")
  expect_equal(g$rating_target, "non_null_effect")

  incon <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  impre <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  # Inconsistency now says the chosen threshold is the null...
  expect_match(incon$notes, "vs null = 0 (chosen threshold is the null",
               fixed = TRUE)
  # ...and Imprecision rates against the null threshold too.
  expect_match(impre$notes, "the null threshold", fixed = TRUE)
})

test_that("a MID target makes both domains use +/-MID", {
  m <- make_mock_meta(c(0.30, -0.30, 0.0), i2 = 0.70)
  g <- grade_meta(m, threshold = 1.20, threshold_scale = "ratio")
  incon <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  impre <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_match(incon$notes, "vs +/-Threshold", fixed = TRUE)
  expect_match(incon$notes, "same as Imprecision", fixed = TRUE)
  expect_match(impre$notes, "the Threshold (+/-MID)", fixed = TRUE)
})

# ---- Auto Step 2 without Threshold (null=0 fallback) ----

test_that("auto Step 2 without Threshold: all TE > 0 -> majority_one_side -> 'no'", {
  # Without Threshold the trivial zone collapses to {0}: all 3 TE > 0 ->
  # n_above = 3, share 100% -> consistent direction -> do not rate down.
  m <- make_mock_meta(c(0.20, 0.30, 0.40), i2 = 0.60)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "no")
  expect_true(grepl("vs null", row$notes))
})
