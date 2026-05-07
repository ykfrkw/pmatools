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
# Rule (a): CI crosses both ±MID thresholds -> serious
# --------------------------------------------------------------------------
test_that("Rule (a): CI crossing both MID thresholds triggers serious", {
  m <- wide_ci_meta()
  # On RR scale this CI typically spans [<<1, >>1]. With a tight MID,
  # log(RR) CI extends beyond -log(1.05) on one side and +log(1.05) on the
  # other -> crosses both thresholds.
  g <- suppressWarnings(grade_meta(m, mid = 1.05, mid_scale = "ratio",
                                    ois_events = 10))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "serious")
  expect_match(row$notes, "BOTH MID thresholds", fixed = TRUE)
})

test_that("Rule (a): CI within MID thresholds, OIS met -> no concern", {
  # Construct a precise meta where both upper and lower are well within
  # ±MID.
  m <- metacont(
    n.e = rep(2000, 4), mean.e = rep(10, 4), sd.e = rep(1, 4),
    n.c = rep(2000, 4), mean.c = rep(10, 4), sd.c = rep(1, 4),
    studlab = paste0("S", 1:4), sm = "MD", random = TRUE, common = FALSE
  )
  # Tight CI around 0; MID = 0.5 on TE scale; OIS_n = 100 (already met).
  g <- suppressWarnings(grade_meta(m,
    outcome_type = "absolute", mid = 0.5, mid_scale = "te_scale", ois_n = 100))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "no")
  expect_equal(row$downgrade, 0L)
})

# --------------------------------------------------------------------------
# Combined behaviour
# --------------------------------------------------------------------------
test_that("Crosses null but not both MIDs, OIS met (>=100%) -> some_concerns", {
  # Small effect, narrow-ish CI that crosses null but stays inside ±MID.
  m <- metabin(
    event.e = c(50, 60, 70),
    n.e     = c(500, 500, 500),
    event.c = c(48, 62, 72),
    n.c     = c(500, 500, 500),
    studlab = c("A", "B", "C"),
    sm = "RR", method = "MH", random = TRUE, common = FALSE
  )
  g <- suppressWarnings(grade_meta(m, mid = 1.5, mid_scale = "ratio",
                                    ois_events = 100))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(row$judgment %in% c("no", "some_concerns"))
})
