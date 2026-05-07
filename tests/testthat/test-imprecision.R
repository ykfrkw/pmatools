library(testthat)
library(meta)

skip_if_not_installed("meta")

# Small meta with k=3 — used for -2 (OIS <= 30%) tests.
small_meta <- function() {
  metabin(
    event.e = c(10, 15, 20),
    n.e     = c(50, 60, 70),
    event.c = c(15, 20, 25),
    n.c     = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm      = "RR",
    method  = "MH",
    random  = TRUE,
    common  = FALSE
  )
}

# Wider meta with deliberately wide CIs to exercise the both-thresholds rule.
# Few events + large variance -> wide log(RR) CI.
wide_ci_meta <- function() {
  metabin(
    event.e = c(2, 3, 1),
    n.e     = c(20, 25, 18),
    event.c = c(3, 2, 4),
    n.c     = c(20, 25, 18),
    studlab = c("S1", "S2", "S3"),
    sm      = "RR",
    method  = "Inverse",
    random  = TRUE,
    common  = FALSE,
    incr    = 0.1
  )
}

# --------------------------------------------------------------------------
# Rule (b): N or events <= 30% of OIS -> serious
# --------------------------------------------------------------------------
test_that("Rule (b): events <= 30% of OIS triggers serious", {
  m <- small_meta()
  # Total events = 105, OIS = 1000 -> pct = 10.5% (well below 30%).
  g <- suppressWarnings(grade_meta(m, ois_events = 1000))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -2L)
  expect_match(row$notes, "<= 30%", fixed = TRUE)
})

test_that("Rule (b): events between 30% and 100% of OIS gives some_concerns", {
  m <- small_meta()
  # Total events = 105, OIS = 200 -> pct = 52.5%.
  g <- suppressWarnings(grade_meta(m, ois_events = 200))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "some_concerns")
  expect_equal(row$downgrade, -1L)
  # Notes should display observed / target counts alongside the percentage
  # so users can verify the OIS check at a glance.
  expect_match(row$notes, "observed 105 / target 200 events", fixed = TRUE)
})

test_that("Rule (b) for continuous: N <= 30% of OIS triggers serious", {
  m <- metacont(
    n.e = c(20, 25), mean.e = c(5, 6), sd.e = c(2, 2),
    n.c = c(20, 25), mean.c = c(7, 8), sd.c = c(2, 2),
    studlab = c("X", "Y"), sm = "MD", random = TRUE, common = FALSE
  )
  # Total N = 90, OIS = 1000 -> 9%.
  g <- suppressWarnings(grade_meta(m, outcome_type = "absolute", ois_n = 1000))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "serious")
  expect_match(row$notes, "<= 30%", fixed = TRUE)
})

# --------------------------------------------------------------------------
# Rule (a): CI contains both ±Thresholds -> serious
# --------------------------------------------------------------------------
test_that("Rule (a): CI containing both Thresholds triggers serious", {
  m <- wide_ci_meta()
  # On RR scale this CI typically spans [<<1, >>1]. With a tight Threshold,
  # log(RR) CI extends below -log(1.05) and above +log(1.05) -> contains both
  # thresholds.
  g <- suppressWarnings(grade_meta(m, threshold = 1.05, threshold_scale = "ratio",
                                    ois_events = 10))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "serious")
  expect_match(row$notes, "BOTH Thresholds", fixed = TRUE)
})

test_that("Rule (a): CI within Thresholds, OIS met -> no concern", {
  # Construct a precise meta where both upper and lower are well within
  # ±Threshold.
  m <- metacont(
    n.e = rep(2000, 4), mean.e = rep(10, 4), sd.e = rep(1, 4),
    n.c = rep(2000, 4), mean.c = rep(10, 4), sd.c = rep(1, 4),
    studlab = paste0("S", 1:4), sm = "MD", random = TRUE, common = FALSE
  )
  # Tight CI around 0; Threshold = 0.5 on TE scale; OIS_n = 100 (already met).
  g <- suppressWarnings(grade_meta(m,
    outcome_type = "absolute", threshold = 0.5, threshold_scale = "te_scale",
    ois_n = 100))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "no")
  expect_equal(row$downgrade, 0L)
  expect_match(row$notes, "within Threshold", fixed = TRUE)
})

# --------------------------------------------------------------------------
# Regression test: CI entirely beyond Threshold (definitive effect)
# was incorrectly flagged as "crosses one threshold" -> some_concerns.
# Correct GRADE Guidance 34 behavior: no rate down (definitive important effect).
# --------------------------------------------------------------------------
test_that("CI entirely beyond +Threshold -> no rate down (regression)", {
  # Construct a CI like [OR 1.62, 3.34] vs Threshold OR 1.25.
  # log(1.62) = 0.482, log(3.34) = 1.206, log(1.25) = 0.223 -> entirely above +T.
  m <- metabin(
    event.e = c(40, 50, 55), n.e = c(100, 100, 100),
    event.c = c(15, 18, 22), n.c = c(100, 100, 100),
    studlab = c("A", "B", "C"), sm = "OR",
    method = "Inverse", random = TRUE, common = FALSE
  )
  g <- suppressWarnings(grade_meta(
    m, threshold = 1.25, threshold_scale = "ratio",
    ois_p0 = 0.2, ois_p1 = 0.4
  ))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "no")
  expect_equal(row$downgrade, 0L)
  expect_match(row$notes, "beyond Threshold", fixed = TRUE)
  expect_false(grepl("crosses one Threshold", row$notes, fixed = TRUE))
})

# --------------------------------------------------------------------------
# Combined behaviour
# --------------------------------------------------------------------------
test_that("OR-scale Threshold derives ois_p1 via odds (not RR approximation)", {
  # When sm = "OR" and the user supplies a ratio-scale Threshold, the OIS
  # auto-calc must convert Threshold -> p1 via the odds formula
  #   p1 = (p0 * OR) / (1 - p0 + p0 * OR)
  # rather than the RR-style p1 = p0 * exp(log OR), which diverges from
  # the truth as p0 moves away from 0.
  m <- metabin(
    event.e = c(40, 45, 50),
    n.e     = c(100, 100, 100),
    event.c = c(50, 55, 60),
    n.c     = c(100, 100, 100),
    studlab = c("A", "B", "C"),
    sm = "OR", method = "Inverse", random = TRUE, common = FALSE
  )
  # Control event rate ~ 0.55 - high enough that OR != RR materially.
  # With Threshold = 0.75 (OR scale), odds-formula ois_p1 ~= 0.478;
  # RR approximation would give p0*0.75 ~= 0.413. Notes should reflect
  # the odds-formula value.
  g <- suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  m_ois_p1 <- regmatches(row$notes,
                          regexpr("ois_p1 = [0-9.]+", row$notes))
  expect_match(m_ois_p1, "^ois_p1 = ")
  ois_p1_val <- as.numeric(sub("ois_p1 = ", "", m_ois_p1))
  # Compute expected from the OR formula directly using whatever p0 the
  # function picked from the data (control-arm pooled rate).
  cer <- (50 + 55 + 60) / (3 * 100)
  or_val <- 0.75
  expected <- (cer * or_val) / (1 - cer + cer * or_val)
  expect_equal(ois_p1_val, round(expected, 4), tolerance = 5e-4)
  # Sanity: OR-derived ois_p1 must differ from naive RR approximation.
  rr_approx <- cer * or_val
  expect_gt(abs(ois_p1_val - rr_approx), 0.02)
})

test_that("Crosses null but not both Thresholds, OIS met (>=100%) -> some_concerns", {
  # Small effect, narrow-ish CI that crosses null but stays inside ±Threshold.
  m <- metabin(
    event.e = c(50, 60, 70),
    n.e     = c(500, 500, 500),
    event.c = c(48, 62, 72),
    n.c     = c(500, 500, 500),
    studlab = c("A", "B", "C"),
    sm = "RR", method = "MH", random = TRUE, common = FALSE
  )
  g <- suppressWarnings(grade_meta(m, threshold = 1.5, threshold_scale = "ratio",
                                    ois_events = 100))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(row$judgment %in% c("no", "some_concerns"))
})
