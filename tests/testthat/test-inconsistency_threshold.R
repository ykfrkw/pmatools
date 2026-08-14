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
  expect_equal(row$judgment, "not_serious")
  expect_false(row$auto)
})

test_that("majority_one_side -> 'no'", {
  m <- make_mock_meta(c(0.2, 0.3, 0.4), i2 = 0.6)
  g <- grade_meta(m,
    inconsistency_ci_diff        = "yes",
    inconsistency_threshold_side = "majority_one_side", threshold_type = "null"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
})

test_that("opposite_sides + subgroup explained -> 'no'", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m,
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "yes", threshold_type = "null"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
})

# Updated (v0.5.1): the opposite-sided branch rates down TWO levels. It is the
# one place pmatools departs from Core GRADE 3 on the size of a downgrade, so
# the note is asserted alongside the judgment: a -2 that arrives without the
# reasoning is the failure mode this pins.
test_that("opposite_sides + no subgroup -> 'serious' (-2), and says why", {
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m,
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "no", threshold_type = "null"
  )
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2L)
  expect_match(row$notes, "This departs from Core GRADE 3", fixed = TRUE)
  expect_match(row$notes, "direction of the effect is unresolved", fixed = TRUE)
})

test_that("the scattered branch is untouched and still stops at -1", {
  # The neighbour of the -2 branch, and the reason removing the cap is not a
  # blanket change. Against +/-log(1.20): 3 estimates above, 1 below, 2 in the
  # trivial zone. No zone holds 80%, but the smaller side is 1 of 6 (17%) and
  # so misses the 20% gate, which is what "opposite sides" means here. The
  # estimates disagree in magnitude, not in direction: -1.
  m <- make_mock_meta(c(0.30, 0.40, 0.50, 0.05, 0.02, -0.30), i2 = 0.70)
  g <- grade_meta(m, threshold = 1.20, threshold_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -1L)
  expect_match(row$notes, "heterogeneous magnitude", fixed = TRUE)
  # No -2 means no departure to declare.
  expect_false(grepl("This departs from Core GRADE 3", row$notes, fixed = TRUE))
})

test_that("the scalar override sets inconsistency independently of the flowchart", {
  # Before v0.5.1 this was the ONLY route to -2 in this domain. The automated
  # opposite-sides branch now reaches it too, so what is left to pin is that
  # an explicit judgment replaces the flowchart in either direction.
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.7)
  g <- grade_meta(m, threshold_type = "null", inconsistency = "very_serious",
                  inconsistency_rationale = "Directions of effect irreconcilable")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2L)

  g_down <- grade_meta(m, threshold_type = "null",
                       inconsistency = "some_concerns",
                       inconsistency_rationale = "One outlier trial, pre-specified")
  row_down <- g_down$domain_assessments[
    g_down$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row_down$judgment, "serious")
  expect_equal(row_down$downgrade, -1L)
  expect_false(row_down$auto)
})

# ---- Auto path: I^2 only (no Q-test) ----

# Cut-off updated 25% -> 30% (v0.5). Reason: 30% is the only numeric value
# Core GRADE 3 puts on paper ("one will seldom see serious inconsistency with
# I2 values <30%"); 25% had no source. I^2 = 0.28 is the new pass case and
# would have rated down under the old cut-off.
test_that("auto Step 1: I^2 <= 30% -> 'no' regardless of Q p", {
  m <- make_mock_meta(c(0.1, 0.1, 0.1), i2 = 0.20, tau2 = 0)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
  expect_true(row$auto)
  expect_match(row$notes, "I2 <= 30%", fixed = TRUE)
})

test_that("I^2 between the old 25% and the new 30% cut-off no longer rates down", {
  # Opposite-sided TEs: under the pre-v0.5 cut-off this reached Step 2 and
  # returned 'some_concerns'. It now stops at Step 1.
  m <- make_mock_meta(c(-0.5, 0.5, -0.5), i2 = 0.28)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
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
  # opposite-sided TEs -> Step 2 reached, and its opposite-sides leaf is -2
  expect_equal(row$judgment, "very_serious")
})

# ---- Auto Step 2 with Threshold ----

test_that("auto Step 2 with Threshold: all studies above Threshold -> majority_one_side -> 'no'", {
  # All TE > +log(1.2) = 0.182 -> all in 'above' zone -> single zone share = 100%
  # >= 80% one-side share -> consistent direction -> do not rate down (matches
  # manual flowchart's 'majority_one_side -> no' branch in BMJ Core GRADE 3).
  m <- make_mock_meta(c(0.30, 0.40, 0.50), i2 = 0.60)
  g <- grade_meta(m, threshold = 1.20, threshold_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
  expect_true(grepl("vs +/-Threshold", row$notes, fixed = TRUE))
})

test_that("auto Step 2 with Threshold: zone tally distinguishes opposite from majority", {
  # TE values: 1 above, 1 below, 1 trivial -> opposite sides, which is -2
  m <- make_mock_meta(c(0.30, -0.30, 0.0), i2 = 0.70)
  g <- grade_meta(m, threshold = 1.20, threshold_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "very_serious")
  expect_match(row$notes, "rate down 2 (clinically opposite)", fixed = TRUE)
})

# ---- Auto Step 3: the subgroup answer now reaches the automated path ------
# Before 0.5.1 the automated opposite-sides note told the reviewer to supply
# inconsistency_subgroup_explained, and supplying it switched the whole domain
# onto the MANUAL path - which then aborted unless inconsistency_ci_diff and
# inconsistency_threshold_side were supplied too. The advice was a no-op.

.opposite_sides_meta <- function() make_mock_meta(c(0.30, -0.30, 0.0), i2 = 0.70)

test_that("auto opposite sides + subgroup explained -> 'no', on the auto path", {
  g <- grade_meta(.opposite_sides_meta(), threshold = 1.20,
                  threshold_scale = "ratio",
                  inconsistency_subgroup_explained = "yes")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
  # Still the AUTOMATED path: no rationale was required and none was given.
  expect_true(row$auto)
  expect_match(row$notes, "AUTO Step 3", fixed = TRUE)
  # The caveat that makes the answer interpretable travels with it.
  expect_match(row$notes, "Subgroup credibility is not auto-detectable",
               fixed = TRUE)
  expect_match(row$notes, "present subgroup results separately", fixed = TRUE)
})

test_that("auto opposite sides + subgroup NOT explained rates down two levels", {
  g <- grade_meta(.opposite_sides_meta(), threshold = 1.20,
                  threshold_scale = "ratio",
                  inconsistency_subgroup_explained = "no")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2L)
  expect_true(row$auto)
  expect_match(row$notes, "NOT explained by a credible subgroup", fixed = TRUE)
})

test_that("an unanswered subgroup question lands on the same leaf as 'no'", {
  # The default is the conservative one and has been since 0.5.1; what
  # changed in 0.5.1 is how conservative, so pin the level as well as the
  # pointer at the argument that would move it.
  g <- grade_meta(.opposite_sides_meta(), threshold = 1.20,
                  threshold_scale = "ratio")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2L)
  expect_match(row$notes, "Supply inconsistency_subgroup_explained",
               fixed = TRUE)
})

test_that("the subgroup answer does nothing on the other automated branches", {
  # Majority on one side: Step 3 is never reached, so answering it must not
  # change the verdict or invent a Step 3 note.
  g <- grade_meta(make_mock_meta(c(0.30, 0.40, 0.50), i2 = 0.60),
                  threshold = 1.20, threshold_scale = "ratio",
                  inconsistency_subgroup_explained = "yes")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
  expect_false(grepl("AUTO Step 3", row$notes, fixed = TRUE))
})

test_that("the auto path validates the subgroup answer", {
  expect_error(
    grade_meta(.opposite_sides_meta(), threshold = 1.20,
               threshold_scale = "ratio",
               inconsistency_subgroup_explained = "maybe"),
    "inconsistency_subgroup_explained"
  )
})

# ---- Chosen threshold shared with Imprecision (Core GRADE 3 Fig 2) ---------

test_that("inconsistency and imprecision use the SAME chosen threshold", {
  # threshold_type = "null" with a point estimate beyond the MID resolves the
  # rating target to non_null_effect, whose chosen threshold is the null.
  # Before v0.5 Inconsistency still received the raw MID here.
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
  expect_match(impre$notes, "the +/-Threshold band", fixed = TRUE)
})

# ---- Auto Step 2 without Threshold (null=0 fallback) ----

test_that("auto Step 2 without Threshold: all TE > 0 -> majority_one_side -> 'no'", {
  # Without Threshold the trivial zone collapses to {0}: all 3 TE > 0 ->
  # n_above = 3, share 100% -> consistent direction -> do not rate down.
  m <- make_mock_meta(c(0.20, 0.30, 0.40), i2 = 0.60)
  g <- grade_meta(m, threshold_type = "null")
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "not_serious")
  expect_true(grepl("vs null", row$notes))
})
