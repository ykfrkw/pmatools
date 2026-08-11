# test-rating_target.R — Core GRADE 2 (BMJ 2025;389:e081904)
#   Fig 2: choosing the target of the certainty rating
#   Fig 4: rating down (or not) for imprecision
#
# Covers the threshold-type entry gate, the automatic target derivation, the
# manual-override rationale gate, and every branch of the Fig 4 flowchart.

library(testthat)
library(meta)

skip_if_not_installed("meta")

impre <- function(g) {
  g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
}

# RR ~ 0.75, CI crosses the null (k = 3, few events).
meta_rr <- function() {
  metabin(
    event.e = c(10, 15, 20), n.e = c(50, 60, 70),
    event.c = c(15, 20, 25), n.c = c(50, 60, 70),
    studlab = c("A", "B", "C"), sm = "RR", method = "MH",
    random = TRUE, common = FALSE
  )
}

# RR ~ 0.99 with a narrow CI: point estimate essentially at the null.
meta_near_null <- function() {
  metabin(
    event.e = c(50, 55), n.e = c(500, 500),
    event.c = c(52, 54), n.c = c(500, 500),
    studlab = c("A", "B"), sm = "RR", method = "Inverse",
    random = TRUE, common = FALSE
  )
}

# RR ~ 0.12: implausibly large effect, CI far from the null, CI ratio ~ 6.
meta_large_rr <- function() {
  metabin(
    event.e = c(2, 3),   n.e = c(60, 60),
    event.c = c(20, 22), n.c = c(60, 60),
    studlab = c("A", "B"), sm = "RR", method = "Inverse",
    random = TRUE, common = FALSE
  )
}

# MD = 0.1 with SD 2: moderate (standardized 0.05), CI inside +/-1.0.
meta_md_moderate <- function() {
  metacont(
    n.e = c(40, 40), mean.e = c(10.1, 10.0), sd.e = c(2, 2),
    n.c = c(40, 40), mean.c = c(10.0, 9.9),  sd.c = c(2, 2),
    studlab = c("A", "B"), sm = "MD", random = TRUE, common = FALSE
  )
}

# MD = -2.9 with SD 2: large (standardized 1.45), CI well beyond +/-0.5.
meta_md_large <- function() {
  metacont(
    n.e = c(60, 60), mean.e = c(7.0, 7.1),  sd.e = c(2, 2),
    n.c = c(60, 60), mean.c = c(10.0, 9.9), sd.c = c(2, 2),
    studlab = c("A", "B"), sm = "MD", random = TRUE, common = FALSE
  )
}

# --------------------------------------------------------------------------
# 1-2. Entry gate: threshold_type = "mid" requires a MID
# --------------------------------------------------------------------------

test_that("threshold_type = 'mid' without a threshold aborts with a suggested value", {
  m <- meta_rr()
  err <- tryCatch(grade_meta(m, threshold_type = "mid"),
                  error = function(e) conditionMessage(e))
  expect_type(err, "character")
  expect_match(err, "threshold_type = 'mid' requires a threshold", fixed = TRUE)
  # The message must carry the actual suggest_threshold() recommendation, not
  # a generic placeholder.
  sugg <- suggest_threshold(m)
  expect_match(err, format(signif(sugg$threshold_user, 4)), fixed = TRUE)
  expect_match(err, sprintf("threshold_scale = '%s'", sugg$threshold_scale),
               fixed = TRUE)
})

test_that("threshold_type = 'mid' is the default, so a bare call aborts", {
  m <- meta_rr()
  expect_error(grade_meta(m), "requires a threshold")
})

test_that("require_threshold = FALSE proceeds without a MID", {
  m <- meta_rr()
  g <- suppressWarnings(grade_meta(m, threshold_type = "mid",
                                   require_threshold = FALSE))
  expect_s3_class(g, "pmatools")
  expect_equal(g$rating_target, "non_null_effect")
  expect_match(g$rating_target_note, "require_threshold = FALSE", fixed = TRUE)
})

test_that("supplying a threshold satisfies the gate", {
  m <- meta_rr()
  g <- suppressWarnings(grade_meta(m, threshold = 1.2,
                                   threshold_scale = "ratio"))
  expect_equal(g$threshold_type, "mid")
})

# --------------------------------------------------------------------------
# 3. MID threshold: target follows the side of the MID the point estimate is on
# --------------------------------------------------------------------------

test_that("MID threshold: |TE| > MID -> important_effect", {
  g <- suppressWarnings(grade_meta(meta_large_rr(), threshold = 1.2,
                                   threshold_scale = "ratio",
                                   ois_events = 40))
  expect_equal(g$rating_target, "important_effect")
  expect_true(g$rating_target_auto)
  expect_match(g$rating_target_note, "Important effect")
})

test_that("MID threshold: |TE| <= MID -> little_to_no_difference", {
  g <- suppressWarnings(grade_meta(meta_near_null(), threshold = 1.2,
                                   threshold_scale = "ratio"))
  expect_equal(g$rating_target, "little_to_no_difference")
  expect_match(g$rating_target_note, "Little or no difference")
})

# --------------------------------------------------------------------------
# 4-5. Null threshold
# --------------------------------------------------------------------------

test_that("null threshold + point estimate near null -> little_to_no_difference, judged against the MID", {
  g <- suppressWarnings(grade_meta(meta_near_null(), threshold_type = "null",
                                   threshold = 1.2, threshold_scale = "ratio"))
  expect_equal(g$rating_target, "little_to_no_difference")
  expect_match(g$rating_target_note, "very near the null")
  # Imprecision must now use +/-MID, not the null.
  expect_match(impre(g)$notes, "Threshold (+/-MID)", fixed = TRUE)
})

test_that("null threshold + point estimate not near null -> non_null_effect, judged against the null", {
  g <- suppressWarnings(grade_meta(meta_large_rr(), threshold_type = "null",
                                   threshold = 1.2, threshold_scale = "ratio",
                                   ois_events = 40))
  expect_equal(g$rating_target, "non_null_effect")
  expect_match(impre(g)$notes, "the null threshold", fixed = TRUE)
})

test_that("null threshold without a MID falls back to non_null_effect and says so", {
  g <- suppressWarnings(grade_meta(meta_near_null(), threshold_type = "null"))
  expect_equal(g$rating_target, "non_null_effect")
  expect_match(g$rating_target_note, "No MID was supplied")
})

test_that("a MID-based target without a MID aborts", {
  # Nearness to the null can only be judged against a MID, so the automatic
  # derivation never reaches this state; it is reachable by asking for the
  # target explicitly. Imprecision for this target is judged against +/-MID,
  # so the MID is mandatory.
  m <- meta_near_null()
  expect_error(
    suppressWarnings(grade_meta(
      m, threshold_type = "null",
      rating_target = "little_to_no_difference",
      rating_target_rationale = "Panel judged the estimate to be at the null"
    )),
    "requires a threshold (MID)", fixed = TRUE
  )
})

# --------------------------------------------------------------------------
# 6. Manual rating_target override
# --------------------------------------------------------------------------

test_that("manual rating_target without a rationale aborts", {
  m <- meta_rr()
  expect_error(
    suppressWarnings(grade_meta(m, threshold_type = "null",
                                rating_target = "non_null_effect")),
    "Overriding the rating target judgment requires rating_target_rationale"
  )
})

test_that("manual rating_target with a rationale succeeds and is recorded", {
  m <- meta_rr()
  g <- suppressWarnings(grade_meta(
    m, threshold_type = "null", rating_target = "non_null_effect",
    rating_target_rationale = "Panel rated certainty in any true effect"
  ))
  expect_equal(g$rating_target, "non_null_effect")
  expect_false(g$rating_target_auto)
  expect_match(g$rating_target_note,
               "Manual override \\(non_null_effect\\): Panel rated certainty")
  # The note propagates to the Imprecision domain notes (and thus downstream).
  expect_match(impre(g)$notes, "Manual override")
})

test_that("an unknown rating_target aborts", {
  m <- meta_rr()
  expect_error(
    suppressWarnings(grade_meta(m, threshold_type = "null",
                                rating_target = "bogus",
                                rating_target_rationale = "x")),
    "rating_target must be one of"
  )
})

# --------------------------------------------------------------------------
# 7. Core GRADE 2 Fig 4 paths
# --------------------------------------------------------------------------

test_that("(a) CI crosses the threshold -> rate down one level", {
  g <- suppressWarnings(grade_meta(meta_rr(), threshold_type = "null",
                                   ois_events = 1000))
  row <- impre(g)
  expect_equal(row$judgment, "some_concerns")
  expect_match(row$notes, "rate down one level (sample size not considered",
               fixed = TRUE)
})

test_that("(b) CI crosses both thresholds -> rate down two levels", {
  m <- metabin(
    event.e = c(2, 3, 1), n.e = c(20, 25, 18),
    event.c = c(3, 2, 4), n.c = c(20, 25, 18),
    studlab = c("S1", "S2", "S3"), sm = "RR", method = "Inverse",
    random = TRUE, common = FALSE, incr = 0.1
  )
  g <- suppressWarnings(grade_meta(m, threshold = 1.05,
                                   threshold_scale = "ratio",
                                   ois_events = 10))
  row <- impre(g)
  expect_equal(row$judgment, "serious")
  expect_match(row$notes, "CI crosses TWO thresholds", fixed = TRUE)
})

test_that("(c) CI does not cross and effect is moderate -> do not rate down", {
  g <- suppressWarnings(grade_meta(meta_md_moderate(),
                                   outcome_type = "absolute",
                                   threshold = 1.0,
                                   threshold_scale = "te_scale",
                                   ois_n = 5000))
  row <- impre(g)
  expect_equal(row$judgment, "no")
  expect_match(row$notes, "effect moderate", fixed = TRUE)
  expect_match(row$notes, "do not rate down (OIS not applied)", fixed = TRUE)
})

test_that("(d) continuous: CI does not cross, effect large, N < OIS -> rate down one", {
  g <- suppressWarnings(grade_meta(meta_md_large(),
                                   outcome_type = "absolute",
                                   threshold = 0.5,
                                   threshold_scale = "te_scale",
                                   ois_n = 500))
  row <- impre(g)
  expect_equal(row$judgment, "some_concerns")
  expect_match(row$notes, "OIS approach", fixed = TRUE)
  expect_match(row$notes, "N < OIS -> rate down one level", fixed = TRUE)
})

test_that("(e) binary: CI ratio >= 3 -> consider rating down two levels", {
  g <- suppressWarnings(grade_meta(meta_large_rr(), threshold = 1.2,
                                   threshold_scale = "ratio",
                                   ois_events = 40))
  row <- impre(g)
  expect_equal(row$judgment, "serious")
  expect_match(row$notes, "consider rating down two levels", fixed = TRUE)
  expect_match(row$notes, "CI ratio", fixed = TRUE)
})

test_that("continuous rule of thumb: total N >= 800 does not rate down", {
  # Large effect, CI clear of the threshold, unattainably large OIS, but the
  # 400-per-group (total 800) rule of thumb applies.
  m <- metacont(
    n.e = rep(300, 2), mean.e = c(7.0, 7.1),  sd.e = c(2, 2),
    n.c = rep(300, 2), mean.c = c(10.0, 9.9), sd.c = c(2, 2),
    studlab = c("A", "B"), sm = "MD", random = TRUE, common = FALSE
  )
  g <- suppressWarnings(grade_meta(m, outcome_type = "absolute",
                                   threshold = 0.5,
                                   threshold_scale = "te_scale",
                                   ois_n = 100000))
  row <- impre(g)
  expect_equal(row$judgment, "no")
  expect_match(row$notes, ">= 800 (rule of thumb)", fixed = TRUE)
})

# --------------------------------------------------------------------------
# 8. Regression: OIS is no longer applied unconditionally
# --------------------------------------------------------------------------

test_that("regression: moderate effect, CI inside the thresholds, N far below OIS -> no rate down", {
  # Old behaviour: total N / OIS <= 30% forced "serious" regardless of where
  # the CI sat. Core GRADE 2 Fig 4 only consults the OIS when the CI does not
  # cross the threshold AND the effect is implausibly large.
  g <- suppressWarnings(grade_meta(meta_md_moderate(),
                                   outcome_type = "absolute",
                                   threshold = 1.0,
                                   threshold_scale = "te_scale",
                                   ois_n = 5000))
  row <- impre(g)
  expect_equal(row$judgment, "no")
  expect_equal(row$downgrade, 0L)
  expect_match(row$notes, "OIS not applied on this Fig 4 path", fixed = TRUE)
})

# --------------------------------------------------------------------------
# Helper-level unit checks
# --------------------------------------------------------------------------

test_that(".is_implausibly_large follows the Core GRADE 2 binary wording", {
  # RRR > 40% ("certainly"), > 30% ("possibly"), otherwise moderate.
  expect_true(pmatools:::.is_implausibly_large(log(0.55), "RR")$large)
  expect_equal(pmatools:::.is_implausibly_large(log(0.55), "RR")$level, "certain")
  expect_true(pmatools:::.is_implausibly_large(log(0.65), "RR")$large)
  expect_equal(pmatools:::.is_implausibly_large(log(0.65), "RR")$level, "possible")
  expect_false(pmatools:::.is_implausibly_large(log(0.80), "RR")$large)
  # Symmetric for effects above 1.
  expect_true(pmatools:::.is_implausibly_large(-log(0.55), "RR")$large)
})

test_that(".ci_ratio and .ci_ratio_cut match the Fig 4 caption", {
  # upper bound / lower bound on the ratio scale.
  expect_equal(pmatools:::.ci_ratio(log(0.5), log(2.0), "RR"), 4)
  expect_true(is.na(pmatools:::.ci_ratio(-1, 1, "MD")))
  expect_equal(pmatools:::.ci_ratio_cut("RR"), 3)
  expect_equal(pmatools:::.ci_ratio_cut("OR"), 2.5)
  expect_true(is.na(pmatools:::.ci_ratio_cut("MD")))
})

test_that(".derive_rating_target covers the Fig 2 branch table", {
  d <- pmatools:::.derive_rating_target
  expect_equal(d(0.5,  0.2, "mid")$target,  "important_effect")
  expect_equal(d(0.1,  0.2, "mid")$target,  "little_to_no_difference")
  expect_equal(d(0.1,  0.2, "null")$target, "little_to_no_difference")
  expect_equal(d(0.5,  0.2, "null")$target, "non_null_effect")
  expect_equal(d(0.5, NULL, "null")$target, "non_null_effect")
  # Threshold handed to Imprecision: +/-MID except for a non-null target.
  expect_equal(d(0.1, 0.2, "null")$threshold_for_imprecision, 0.2)
  expect_equal(d(0.5, 0.2, "null")$threshold_for_imprecision, 0)
})
