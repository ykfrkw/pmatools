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
# OIS rules. Core GRADE 2 Fig 4 only reaches the OIS approach when the CI does
# NOT cross the chosen threshold and the effect is implausibly large; when the
# CI does cross it, Fig 4 rates down without considering sample size.
# --------------------------------------------------------------------------
test_that("events <= 30% of OIS but CI crosses the threshold -> rate down one", {
  m <- small_meta()
  # Total events = 105, OIS = 1000 -> pct = 10.5% (well below 30%).
  # Expectation changed with the Fig 4 rewrite: the CI crosses the chosen
  # (null) threshold, so Fig 4 stops at "rate down one level" and never
  # consults the OIS; the previous implementation applied "<= 30% of OIS"
  # unconditionally and returned "serious".
  g <- suppressWarnings(grade_meta(m, ois_events = 1000, threshold_type = "null"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "some_concerns")
  expect_equal(row$downgrade, -1L)
  expect_match(row$notes, "OIS not applied on this Fig 4 path", fixed = TRUE)
})

test_that("events between 30% and 100% of OIS gives some_concerns", {
  m <- small_meta()
  # Total events = 105, OIS = 200 -> pct = 52.5%.
  # Still -1 after the Fig 4 rewrite, but now because the CI crosses the null
  # threshold rather than because the OIS was unmet.
  g <- suppressWarnings(grade_meta(m, ois_events = 200, threshold_type = "null"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "some_concerns")
  expect_equal(row$downgrade, -1L)
  # Notes should display observed / target counts alongside the percentage
  # so users can verify the OIS check at a glance.
  expect_match(row$notes, "observed 105 / target 200 events", fixed = TRUE)
})

test_that("continuous: large effect, CI clear of the threshold, N < 30% of OIS -> serious", {
  m <- metacont(
    n.e = c(20, 25), mean.e = c(5, 6), sd.e = c(2, 2),
    n.c = c(20, 25), mean.c = c(7, 8), sd.c = c(2, 2),
    studlab = c("X", "Y"), sm = "MD", random = TRUE, common = FALSE
  )
  # Total N = 90, OIS = 1000 -> 9%.
  g <- suppressWarnings(grade_meta(m, outcome_type = "absolute", ois_n = 1000, threshold_type = "null"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "serious")
  # Display fixed (v0.5): the note used to read "<= 30%" while the decision
  # used a strict "<". Fig 4's node is "N<30% of OIS".
  expect_match(row$notes, "< 30%", fixed = TRUE)
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
# Null threshold + CI spanning both MIDs -> rate down two levels
#
# Core GRADE 2 (p6), verbatim:
#   "The two considerations also apply to imprecision judgments when Core GRADE
#    users choose the null as the threshold of interest. For example, consider
#    a situation in which users rate their certainty in a benefit (threshold
#    the null) but the CI also includes clearly important harm. The finding
#    that the CI is consistent with both benefit and important harm motivates a
#    plain language summary stating that the intervention 'may' result in a
#    benefit, and rating down two levels for imprecision."
# --------------------------------------------------------------------------

# RR ~ 1.32 with a CI of roughly [0.76, 2.30]: the point estimate is beyond a
# MID of 1.20 (so the null-threshold target is a non-null effect) and the CI
# spans both -MID and +MID.
.make_spans_both_mids <- function() {
  meta::metagen(TE = rep(log(1.32), 2), seTE = rep(0.40, 2),
                studlab = c("A", "B"), sm = "RR", tau.preset = 0)
}

impre_row <- function(g) {
  g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
}

test_that("null threshold + CI spanning both MIDs -> serious (-2)", {
  g <- suppressWarnings(grade_meta(
    .make_spans_both_mids(),
    threshold_type = "null", threshold = 1.20, threshold_scale = "ratio"
  ))
  # The point estimate is beyond the MID, so the target is a non-null effect
  # and the -1/-0 decision is made against the null...
  expect_equal(g$rating_target, "non_null_effect")
  row <- impre_row(g)
  # ...but the CI also includes clearly important harm, so two levels.
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -2L)
  expect_match(row$notes, "crosses BOTH Thresholds", fixed = TRUE)
  expect_match(row$notes, "null-threshold path", fixed = TRUE)
})

test_that("null threshold without a MID cannot reach -2 (two-level check undecidable)", {
  g <- suppressWarnings(grade_meta(.make_spans_both_mids(),
                                   threshold_type = "null"))
  row <- impre_row(g)
  expect_equal(row$judgment, "some_concerns")
  expect_equal(row$downgrade, -1L)
  expect_match(row$notes, "the null threshold", fixed = TRUE)
})

test_that("the MID threshold reaches -2 on the same data (both routes agree)", {
  g <- suppressWarnings(grade_meta(.make_spans_both_mids(),
                                   threshold = 1.20, threshold_scale = "ratio"))
  row <- impre_row(g)
  expect_equal(row$judgment, "serious")
})

# --------------------------------------------------------------------------
# Combined behaviour
# --------------------------------------------------------------------------
# Rewritten (v0.5): the binary OIS no longer derives ois_p1 from the
# Threshold at all (the odds-vs-RR conversion this test used to exercise is
# gone). Core GRADE 2 p6: "For binary outcomes, these involve specifying ...
# the control group event rate (chosen from the context), and a modest relative
# risk reduction, typically 20% or 25%."
.ois_p1_from_notes <- function(notes) {
  m <- regmatches(notes, regexpr("ois_p1 = [0-9.]+", notes))
  as.numeric(sub("ois_p1 = ", "", m))
}

.make_binary_ois_meta <- function() {
  metabin(
    event.e = c(40, 45, 50),
    n.e     = c(100, 100, 100),
    event.c = c(50, 55, 60),
    n.c     = c(100, 100, 100),
    studlab = c("A", "B", "C"),
    sm = "OR", method = "Inverse", random = TRUE, common = FALSE
  )
}

test_that("binary ois_p1 comes from ois_rrr (default 20%), not the Threshold", {
  m   <- .make_binary_ois_meta()
  cer <- (50 + 55 + 60) / (3 * 100)   # control-arm pooled rate = 0.55
  g <- suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(.ois_p1_from_notes(row$notes), round(cer * 0.80, 4),
               tolerance = 5e-4)
  expect_match(row$notes, "modest relative risk reduction", fixed = TRUE)
  # It must NOT be the old Threshold-derived (odds-formula) value.
  odds_derived <- (cer * 0.75) / (1 - cer + cer * 0.75)
  expect_gt(abs(.ois_p1_from_notes(row$notes) - odds_derived), 0.02)
})

test_that("ois_rrr changes the binary OIS; ois_p1 takes precedence over it", {
  m   <- .make_binary_ois_meta()
  cer <- (50 + 55 + 60) / (3 * 100)

  g25 <- suppressWarnings(grade_meta(m, threshold = 0.75,
                                     threshold_scale = "ratio",
                                     ois_rrr = 0.25))
  row25 <- g25$domain_assessments[g25$domain_assessments$domain == "Imprecision", ]
  expect_equal(.ois_p1_from_notes(row25$notes), round(cer * 0.75, 4),
               tolerance = 5e-4)

  g20 <- suppressWarnings(grade_meta(m, threshold = 0.75,
                                     threshold_scale = "ratio"))
  row20 <- g20$domain_assessments[g20$domain_assessments$domain == "Imprecision", ]
  target_of <- function(notes) {
    as.numeric(sub("target N=", "",
                   regmatches(notes, regexpr("target N=[0-9]+", notes))))
  }
  # A larger RRR is easier to detect, so the OIS shrinks.
  expect_lt(target_of(row25$notes), target_of(row20$notes))

  # Explicit ois_p1 wins over ois_rrr.
  g_p1 <- suppressWarnings(grade_meta(m, threshold = 0.75,
                                      threshold_scale = "ratio",
                                      ois_p1 = 0.30, ois_rrr = 0.25))
  row_p1 <- g_p1$domain_assessments[g_p1$domain_assessments$domain == "Imprecision", ]
  expect_match(row_p1$notes, "p1=0.300", fixed = TRUE)
})

test_that("ois_rrr is validated", {
  m <- .make_binary_ois_meta()
  expect_error(
    suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio",
                                ois_rrr = 0)),
    regexp = "ois_rrr"
  )
  expect_error(
    suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio",
                                ois_rrr = 1)),
    regexp = "ois_rrr"
  )
})

test_that("binary OIS is compared in participants, not events", {
  # Core GRADE 2 Fig 4 caption: "N=number of participants; OIS=optimal
  # information size"; body: "If the total sample size of all the studies
  # included in a meta-analysis exceeds the OIS, one does not rate down".
  m <- .make_binary_ois_meta()
  g <- suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_match(row$notes, "target N=", fixed = TRUE)
  expect_match(row$notes, "compares participants", fixed = TRUE)
  # observed = sum(n.e) + sum(n.c) = 600 participants (not the 300 events).
  expect_match(row$notes, "observed 600", fixed = TRUE)
})

test_that("explicit ois_events still drives an event-based comparison", {
  m <- .make_binary_ois_meta()
  g <- suppressWarnings(grade_meta(m, threshold = 0.75,
                                   threshold_scale = "ratio",
                                   ois_events = 1000))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_match(row$notes, "events", fixed = TRUE)
  expect_match(row$notes, "target 1000 events", fixed = TRUE)
})

# --------------------------------------------------------------------------
# .total_n_strict() vs .total_n(): two same-named helpers used to collide, and
# sof_table.R's lenient display version silently won package-wide. They must
# now diverge exactly where it matters -- a single-arm meta, which has $n but
# no arm totals.
# --------------------------------------------------------------------------

# metamean(): a genuine single-arm object -- $n populated, n.e / n.c absent.
.make_single_arm_meta <- function() {
  metamean(n = c(300, 300, 300), mean = c(5, 5.2, 4.8), sd = c(2, 2, 2),
           studlab = c("A", "B", "C"), random = TRUE, common = FALSE)
}

test_that("on a single-arm meta the strict helper is NA while the display one uses $n", {
  m <- .make_single_arm_meta()
  expect_null(m$n.e)
  expect_null(m$n.c)
  expect_identical(pmatools:::.total_n_strict(m), NA_real_)
  expect_equal(pmatools:::.total_n(m), 900)
})

test_that("on a two-arm meta both helpers return sum(n.e) + sum(n.c)", {
  m <- small_meta()
  expected <- sum(m$n.e) + sum(m$n.c)
  expect_equal(pmatools:::.total_n_strict(m), expected)
  expect_equal(pmatools:::.total_n(m), expected)
})

test_that("the 800 rule of thumb no longer fires off a single-arm total", {
  # Fig 4's continuous rule of thumb is "400 patients per group (total sample
  # size 800)", so it needs a real two-arm total. Before the helpers were
  # separated, a $n-only object with N = 900 reached that branch and the notes
  # claimed "total N = 900 >= 800". SMD keeps the effect large without a pooled
  # SD, which is what carries this object into the OIS approach at all.
  m <- meta::metagen(TE = c(1.0, 1.1, 0.9), seTE = c(0.1, 0.1, 0.1),
                     studlab = c("A", "B", "C"), sm = "SMD",
                     random = TRUE, common = FALSE)
  m$n.e <- NULL
  m$n.c <- NULL
  m$n   <- c(300, 300, 300)
  expect_equal(pmatools:::.total_n(m), 900)   # the note would have fired on this

  res <- suppressWarnings(assess_imprecision(m, threshold_type = "null"))
  expect_false(grepl("rule of thumb", res$notes, fixed = TRUE))
  expect_false(grepl("total N =", res$notes, fixed = TRUE))
  # The judgment is unchanged: the object falls through to the "OIS could not
  # be computed" branch, which also does not rate down.
  expect_equal(res$judgment, "no")
  expect_match(res$notes, "OIS could not be computed", fixed = TRUE)
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
