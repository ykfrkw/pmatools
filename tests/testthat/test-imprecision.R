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
  # unconditionally and returned "very_serious".
  g <- suppressWarnings(grade_meta(m, ois_events = 1000, threshold_type = "null",
    small_values = "desirable"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -1L)
  expect_match(row$notes, "OIS not applied on this Fig 4 path", fixed = TRUE)
})

test_that("events between 30% and 100% of OIS gives some_concerns", {
  m <- small_meta()
  # Total events = 105, OIS = 200 -> pct = 52.5%.
  # Still -1 after the Fig 4 rewrite, but now because the CI crosses the null
  # threshold rather than because the OIS was unmet.
  g <- suppressWarnings(grade_meta(m, ois_events = 200, threshold_type = "null",
    small_values = "desirable"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "serious")
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
  g <- suppressWarnings(grade_meta(m, outcome_type = "absolute", ois_n = 1000, threshold_type = "null",
    small_values = "desirable"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "very_serious")
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
                                    small_values = "desirable",
                                    ois_events = 10))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "very_serious")
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
    small_values = "desirable",
    outcome_type = "absolute", threshold = 0.5, threshold_scale = "te_scale",
    ois_n = 100))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "not_serious")
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
    m,
    small_values = "desirable", threshold = 1.25, threshold_scale = "ratio",
    ois_p0 = 0.2, ois_p1 = 0.4
  ))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(row$judgment, "not_serious")
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
    small_values = "desirable",
    .make_spans_both_mids(),
    threshold_type = "null", threshold = 1.20, threshold_scale = "ratio"
  ))
  # The point estimate is beyond the MID, so the target is a non-null effect
  # and the -1/-0 decision is made against the null...
  expect_equal(g$rating_target, "non_null_effect")
  row <- impre_row(g)
  # ...but the CI also includes clearly important harm, so two levels.
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2L)
  expect_match(row$notes, "crosses BOTH Thresholds", fixed = TRUE)
  expect_match(row$notes, "null-threshold path", fixed = TRUE)
})

test_that("null threshold without a MID cannot reach -2 (two-level check undecidable)", {
  g <- suppressWarnings(grade_meta(.make_spans_both_mids(),
                                   small_values = "desirable",
                                   threshold_type = "null"))
  row <- impre_row(g)
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -1L)
  expect_match(row$notes, "the null threshold", fixed = TRUE)
})

test_that("the MID threshold reaches -2 on the same data (both routes agree)", {
  g <- suppressWarnings(grade_meta(.make_spans_both_mids(),
                                   small_values = "desirable",
                                   threshold = 1.20, threshold_scale = "ratio"))
  row <- impre_row(g)
  expect_equal(row$judgment, "very_serious")
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
  g <- suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio",
    small_values = "desirable"))
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
                                     small_values = "desirable",
                                     threshold_scale = "ratio",
                                     ois_rrr = 0.25))
  row25 <- g25$domain_assessments[g25$domain_assessments$domain == "Imprecision", ]
  expect_equal(.ois_p1_from_notes(row25$notes), round(cer * 0.75, 4),
               tolerance = 5e-4)

  g20 <- suppressWarnings(grade_meta(m, threshold = 0.75,
                                     small_values = "desirable",
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
                                      small_values = "desirable",
                                      threshold_scale = "ratio",
                                      ois_p1 = 0.30, ois_rrr = 0.25))
  row_p1 <- g_p1$domain_assessments[g_p1$domain_assessments$domain == "Imprecision", ]
  expect_match(row_p1$notes, "p1=0.300", fixed = TRUE)
})

test_that("ois_rrr is validated", {
  m <- .make_binary_ois_meta()
  expect_error(
    suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio",
                                small_values = "desirable",
                                ois_rrr = 0)),
    regexp = "ois_rrr"
  )
  expect_error(
    suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio",
                                small_values = "desirable",
                                ois_rrr = 1)),
    regexp = "ois_rrr"
  )
})

test_that("binary OIS is compared in participants, not events", {
  # Core GRADE 2 Fig 4 caption: "N=number of participants; OIS=optimal
  # information size"; body: "If the total sample size of all the studies
  # included in a meta-analysis exceeds the OIS, one does not rate down".
  m <- .make_binary_ois_meta()
  g <- suppressWarnings(grade_meta(m, threshold = 0.75, threshold_scale = "ratio",
    small_values = "desirable"))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_match(row$notes, "target N=", fixed = TRUE)
  expect_match(row$notes, "compares participants", fixed = TRUE)
  # observed = sum(n.e) + sum(n.c) = 600 participants (not the 300 events).
  expect_match(row$notes, "observed 600", fixed = TRUE)
})

test_that("explicit ois_events still drives an event-based comparison", {
  m <- .make_binary_ois_meta()
  g <- suppressWarnings(grade_meta(m, threshold = 0.75,
                                   small_values = "desirable",
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

  res <- suppressWarnings(assess_imprecision(m, threshold_type = "null",
    small_values = "desirable"))
  expect_false(grepl("rule of thumb", res$notes, fixed = TRUE))
  expect_false(grepl("total N =", res$notes, fixed = TRUE))
  # The judgment is unchanged: the object falls through to the "OIS could not
  # be computed" branch, which also does not rate down.
  expect_equal(res$judgment, "not_serious")
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
                                    small_values = "desirable",
                                    ois_events = 100))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(row$judgment %in% c("not_serious", "serious"))
})

# --------------------------------------------------------------------------
# Outcome direction and the binary OIS alternative rate (v0.5.1).
#
# Core GRADE 2 writes "a modest relative risk REDUCTION" because its worked
# example has an undesirable event. small_values = "undesirable" says the
# opposite -- a smaller outcome VALUE is worse, so the EVENTS are the good
# thing -- and the OIS then has to be powered against p0 * (1 + rrr).
# --------------------------------------------------------------------------

# One dataset for all three direction cases: a clear increase in the event
# rate, so the CI is well clear of the null and the direction is unambiguous.
direction_meta <- function() {
  metabin(
    event.e = c(40, 45, 50),
    n.e     = c(100, 110, 120),
    event.c = c(20, 22, 25),
    n.c     = c(100, 110, 120),
    studlab = c("A", "B", "C"),
    sm = "RR", method = "Inverse", random = TRUE, common = FALSE
  )
}

# The OIS target N assess_imprecision() computed, read back off the note.
ois_target_from_notes <- function(notes) {
  m <- regmatches(notes, regexpr("target N=[0-9]+", notes))
  if (!length(m)) return(NA_integer_)
  as.integer(sub("target N=", "", m))
}

test_that("small_values decides which side of ois_p0 the OIS target sits on", {
  m <- direction_meta()
  args <- list(outcome_type = "relative", threshold_internal = log(1.25),
               ois_p0 = 0.20, ois_rrr = 0.20)

  r_und  <- do.call(assess_imprecision,
                    c(list(m), args, list(small_values = "undesirable")))
  r_des  <- do.call(assess_imprecision,
                    c(list(m), args, list(small_values = "desirable")))

  # Undesirable outcome VALUE => desirable EVENT => a benefit is an increase.
  expect_match(r_und$notes, "ois_p1 = 0.2400", fixed = TRUE)
  expect_match(r_und$notes, "modest relative risk increase", fixed = TRUE)
  # Desirable outcome value => undesirable event => a benefit is a reduction.
  expect_match(r_des$notes, "ois_p1 = 0.1600", fixed = TRUE)
  expect_match(r_des$notes, "modest relative risk reduction", fixed = TRUE)

  # The two directions therefore power the OIS to different sample sizes,
  # which is why the argument cannot be optional: up to 0.5.0 an omitted
  # direction silently took the "desirable" arithmetic for both outcomes.
  n_und  <- ois_target_from_notes(r_und$notes)
  n_des  <- ois_target_from_notes(r_des$notes)
  expect_true(is.finite(n_und) && is.finite(n_des))
  expect_false(identical(n_und, n_des))
})

test_that("assess_imprecision() refuses to guess the direction", {
  m <- direction_meta()
  # The direction gate fires before the note is built, so the wording of that
  # note (ours says "threshold", not "MID", since 0.5.1) is asserted by the
  # tests that supply small_values rather than here.
  expect_error(
    assess_imprecision(m, outcome_type = "relative",
                       threshold_internal = log(1.25),
                       ois_p0 = 0.20, ois_rrr = 0.20),
    class = "pmatools_direction_gate"
  )
})

test_that("the direction reaches assess_imprecision() through grade_meta()", {
  m <- direction_meta()
  note_of <- function(g) {
    g$domain_assessments$notes[g$domain_assessments$domain == "Imprecision"]
  }
  g_und <- suppressWarnings(grade_meta(
    m, threshold = 1.25, threshold_scale = "ratio",
    small_values = "undesirable", ois_p0 = 0.20))
  g_des <- suppressWarnings(grade_meta(
    m, threshold = 1.25, threshold_scale = "ratio",
    small_values = "desirable", ois_p0 = 0.20))
  expect_match(note_of(g_und), "modest relative risk increase", fixed = TRUE)
  expect_match(note_of(g_des), "modest relative risk reduction", fixed = TRUE)
})

test_that("ois_p1 is clamped into (0, 1) and the note says so", {
  m <- direction_meta()
  r <- assess_imprecision(m, outcome_type = "relative",
                          threshold_internal = log(1.25),
                          ois_p0 = 0.95, ois_rrr = 0.25,
                          small_values = "undesirable")
  # 0.95 * 1.25 = 1.1875, which is not a probability.
  expect_match(r$notes, "clamped into (0, 1) from 1.1875", fixed = TRUE)
  expect_match(r$notes, "ois_p1 = 1.0000", fixed = TRUE)
})

test_that("an explicit ois_p1 is never re-signed by the direction", {
  m <- direction_meta()
  r <- assess_imprecision(m, outcome_type = "relative",
                          threshold_internal = log(1.25),
                          ois_p0 = 0.20, ois_p1 = 0.05,
                          small_values = "undesirable")
  expect_match(r$notes, "p1=0.050", fixed = TRUE)
  expect_false(grepl("modest relative risk", r$notes, fixed = TRUE))
})

test_that("the ois_target_rate fact records the rate and the direction", {
  m <- direction_meta()
  g <- suppressWarnings(grade_meta(
    m, threshold = 1.25, threshold_scale = "ratio",
    small_values = "undesirable", ois_p0 = 0.20))
  f <- domain_facts(g, "Imprecision")
  expect_true("ois_target_rate" %in% f$key)
  row <- f[f$key == "ois_target_rate", ]
  expect_equal(row$numeric[1], 0.24, tolerance = 1e-8)
  expect_match(row$value[1], "an increase", fixed = TRUE)
})

test_that("an effect above the null is not called a relative risk reduction", {
  # The magnitude 1 - exp(-|log RR|) is right either way; the word was not.
  m <- direction_meta()
  r <- assess_imprecision(m, outcome_type = "relative", threshold_type = "null",
    small_values = "desirable")
  expect_match(r$notes, "relative risk increase", fixed = TRUE)
  expect_false(grepl("effect implausibly large (relative risk reduction",
                     r$notes, fixed = TRUE))
})

test_that("an effect below the null still reads as a reduction", {
  m <- metabin(
    event.e = c(20, 22, 25),
    n.e     = c(100, 110, 120),
    event.c = c(40, 45, 50),
    n.c     = c(100, 110, 120),
    studlab = c("A", "B", "C"),
    sm = "RR", method = "Inverse", random = TRUE, common = FALSE
  )
  r <- assess_imprecision(m, outcome_type = "relative", threshold_type = "null",
    small_values = "desirable")
  expect_match(r$notes, "relative risk reduction", fixed = TRUE)
  expect_false(grepl("relative risk increase", r$notes, fixed = TRUE))
})

# --------------------------------------------------------------------------
# Continuous OIS: ois_sd is derived rather than demanded (v0.5.1).
# --------------------------------------------------------------------------

continuous_meta <- function() {
  metacont(
    n.e    = c(40, 45, 50),
    mean.e = c(10.0, 10.5, 9.8),
    sd.e   = c(4.0, 4.2, 3.9),
    n.c    = c(40, 45, 50),
    mean.c = c(14.0, 14.6, 13.7),
    sd.c   = c(4.1, 4.0, 4.2),
    studlab = c("A", "B", "C"),
    sm = "MD", random = TRUE, common = FALSE
  )
}

test_that("a continuous outcome derives ois_sd instead of skipping the OIS", {
  m <- continuous_meta()
  r <- assess_imprecision(m, outcome_type = "absolute",
                          small_values = "desirable",
                          threshold_internal = 2)
  expect_match(r$notes, "derived from the pooled within-study SD", fixed = TRUE)
  expect_match(r$notes, "OIS: delta=2.000", fixed = TRUE)
  expect_false(grepl("OIS could not be computed", r$notes, fixed = TRUE))
  # Recorded as derived, not as something the caller supplied.
  f <- attr(r, "facts")
  expect_true("ois_sd_source" %in% f$key)
  expect_equal(f$numeric[f$key == "ois_sd_source"],
               compute_pooled_sd(m), tolerance = 1e-8)
})

test_that("a supplied ois_sd still wins and is not reported as derived", {
  m <- continuous_meta()
  r <- assess_imprecision(m, outcome_type = "absolute",
                          small_values = "desirable",
                          threshold_internal = 2, ois_sd = 10)
  expect_match(r$notes, "sigma=10.000", fixed = TRUE)
  expect_false(grepl("derived from the pooled within-study SD", r$notes,
                     fixed = TRUE))
})

# An SMD analysis carrying real arm-level SDs is the case the metagen fixtures
# elsewhere in this file cannot reach: they have no sd.e/sd.c, so
# compute_pooled_sd() fails and the unit mismatch never surfaced. The SMD is
# already in SD units, so sigma is 1 and the OIS is the textbook
# 2(z_a+z_b)^2/delta^2 per arm -- with the raw pooled SD of about 4 it was
# inflated by roughly 17x, enough to flip Fig 4's large-effect path.
test_that("an SMD outcome uses sigma = 1 rather than the raw pooled SD", {
  m <- continuous_meta()
  m$sm <- "SMD"
  r <- assess_imprecision(m, outcome_type = "absolute",
                          small_values = "desirable",
                          threshold_internal = 0.20)
  expect_match(r$notes, "sigma=1.000", fixed = TRUE)
  expect_match(r$notes, "OIS: delta=0.200", fixed = TRUE)
  expect_match(r$notes, "the SMD is expressed in within-study SD units",
               fixed = TRUE)
  expect_false(grepl("derived from the pooled within-study SD", r$notes,
                     fixed = TRUE))

  n_arm   <- 2 * (stats::qnorm(0.975) + stats::qnorm(0.80))^2 / 0.20^2
  total_n <- ceiling(2 * n_arm)
  expect_equal(ceiling(n_arm), 393)
  expect_equal(total_n, 785)
  expect_match(r$notes, sprintf("target N=%d", total_n), fixed = TRUE)

  f <- attr(r, "facts")
  expect_equal(f$numeric[f$key == "ois_sd_source"], 1, tolerance = 1e-8)
})

test_that("an MD outcome still derives sigma from the pooled SD", {
  m <- continuous_meta()
  r <- assess_imprecision(m, outcome_type = "absolute",
                          small_values = "desirable",
                          threshold_internal = 0.20)
  sd_pooled <- compute_pooled_sd(m)
  expect_gt(sd_pooled, 1)
  expect_match(r$notes, sprintf("sigma=%.3f", sd_pooled), fixed = TRUE)
  expect_match(r$notes, "derived from the pooled within-study SD", fixed = TRUE)
})

test_that("a supplied ois_sd still wins over the SMD's sigma = 1", {
  m <- continuous_meta()
  m$sm <- "SMD"
  r <- assess_imprecision(m, outcome_type = "absolute",
                          small_values = "desirable",
                          threshold_internal = 0.20, ois_sd = 2)
  expect_match(r$notes, "sigma=2.000", fixed = TRUE)
  expect_false(grepl("the SMD is expressed in within-study SD units", r$notes,
                     fixed = TRUE))
})

test_that("'OIS could not be computed' names the input that was missing", {
  # No MID, so ois_delta cannot be derived and the pooled SD alone is useless.
  m <- meta::metagen(TE = c(1.0, 1.1, 0.9), seTE = c(0.1, 0.1, 0.1),
                     studlab = c("A", "B", "C"), sm = "SMD",
                     random = TRUE, common = FALSE)
  m$n.e <- c(100, 100, 100)
  m$n.c <- c(100, 100, 100)
  r <- suppressWarnings(assess_imprecision(m, outcome_type = "absolute",
                                           small_values = "desirable",
                                           threshold_type = "null"))
  expect_match(r$notes, "OIS could not be computed (missing ois_delta",
               fixed = TRUE)
})

# ==========================================================================
# threshold_sides: the one-sided threshold test (v0.5.1; SPEC.md 4.5.1b)
# ==========================================================================
#
# Order here is deliberate and the first two blocks are the load-bearing ones.
# Golden invariance comes first: nothing else in this section means anything if
# the default setting is not byte-for-byte what it was. Monotonicity comes
# second, because it is the test that would have caught the rejected one-sided
# `-2` rule, and it is written even though the implementation looks obviously
# right -- "obviously right" is what the rejected rule also looked.

# An interval built to order. assess_imprecision() reads lower.random /
# upper.random, so they are set directly rather than reverse-engineered from a
# pair of studies whose pooling would move them.
ts_meta <- function(lower, upper, sm = "RR") {
  te <- (lower + upper) / 2
  se <- (upper - lower) / (2 * stats::qnorm(0.975))
  m <- meta::metagen(TE = c(te, te), seTE = c(se, se),
                     studlab = c("A", "B"), sm = sm,
                     random = TRUE, common = FALSE)
  m$lower.random <- lower
  m$upper.random <- upper
  m$TE.random    <- te
  m$n.e <- c(100, 100)
  m$n.c <- c(100, 100)
  m
}

# The threshold every case below is tested against, on the TE scale.
TS_THRESHOLD <- 0.5

# Every zone on both sides of the band, plus the boundary cases where a limit
# sits exactly on a threshold (which must NOT count as a crossing).
TS_INTERVALS <- list(
  wholly_below      = c(-2.0, -1.0),
  crosses_lower     = c(-1.5, -0.2),
  inside_band       = c(-0.3,  0.3),
  crosses_upper     = c(-0.2,  1.5),
  wholly_above      = c( 1.0,  2.0),
  crosses_both      = c(-1.5,  1.5),
  on_both_limits    = c(-0.5,  0.5),
  lower_on_upper_t  = c( 0.5,  1.5),
  upper_on_lower_t  = c(-1.5, -0.5),
  crosses_upper_2   = c( 0.1,  0.9),
  crosses_lower_2   = c(-0.9, -0.1)
)

TS_MEASURES <- list(RR = "relative", MD = "absolute", SMD = "absolute")

# One row of the grid, at a given sidedness.
ts_assess <- function(sm, outcome_type, small_values, ci, ...) {
  suppressWarnings(assess_imprecision(
    ts_meta(ci[1], ci[2], sm = sm),
    outcome_type              = outcome_type,
    small_values              = small_values,
    threshold_internal        = TS_THRESHOLD,
    threshold_for_imprecision = TS_THRESHOLD,
    ...))
}

# Walk the grid, handing each cell to `f`. A loop, not one case: the rejected
# `-2` rule was wrong on exactly one interval shape out of eleven.
ts_walk <- function(f) {
  for (sm in names(TS_MEASURES)) {
    for (small_values in SMALL_VALUES_LEVELS) {
      for (nm in names(TS_INTERVALS)) {
        f(sm = sm, outcome_type = TS_MEASURES[[sm]],
          small_values = small_values, nm = nm, ci = TS_INTERVALS[[nm]])
      }
    }
  }
}

ts_fact <- function(row, key) {
  facts <- attr(row, "facts")
  if (is.null(facts) || !key %in% facts$key) return(NA_character_)
  as.character(facts$value[match(key, facts$key)])
}

# --------------------------------------------------------------------------
# 1. Golden invariance. threshold_sides = "both" is the argument omitted.
# --------------------------------------------------------------------------

test_that("threshold_sides = 'both' is identical to omitting the argument", {
  ts_walk(function(sm, outcome_type, small_values, nm, ci) {
    omitted <- ts_assess(sm, outcome_type, small_values, ci)
    both    <- ts_assess(sm, outcome_type, small_values, ci,
                         threshold_sides = "both")
    expect_identical(
      both, omitted,
      info = paste0(sm, " / ", small_values, " / ", nm))
  })
})

test_that("the full default vector resolves to 'both' as match.arg would", {
  m <- ts_meta(-0.2, 1.5)
  omitted <- suppressWarnings(assess_imprecision(
    m, small_values = "desirable", threshold_internal = TS_THRESHOLD,
    threshold_for_imprecision = TS_THRESHOLD))
  forwarded <- suppressWarnings(assess_imprecision(
    m, small_values = "desirable", threshold_internal = TS_THRESHOLD,
    threshold_for_imprecision = TS_THRESHOLD,
    threshold_sides = THRESHOLD_SIDES))
  expect_identical(forwarded, omitted)
})

# --------------------------------------------------------------------------
# 2. Monotonicity on Fig 4's crossing branch.
# --------------------------------------------------------------------------
# Non-inferiority is a strictly weaker claim about one interval than
# equivalence, so asking it must never earn a DEEPER downgrade. On Fig 4's
# "Yes" branch that is an absolute rule, and it is the rule the rejected
# one-sided `-2` restatement broke: a CI of (-0.5T, +1.5T) crosses the
# worse-side threshold, so it lands inside this assertion.

test_that("crosses_both_thresholds is identical on both sidedness settings", {
  # The whole `-2` argument rests on this staying two-sided. Asserted through
  # the string that branch writes rather than on the private boolean, so it
  # survives a refactor of the internals.
  ts_walk(function(sm, outcome_type, small_values, nm, ci) {
    both  <- ts_assess(sm, outcome_type, small_values, ci,
                       threshold_sides = "both")
    worse <- ts_assess(sm, outcome_type, small_values, ci,
                       threshold_sides = "worse_only")
    expect_identical(
      grepl("crosses BOTH Thresholds", worse$notes, fixed = TRUE),
      grepl("crosses BOTH Thresholds", both$notes,  fixed = TRUE),
      info = paste0(sm, " / ", small_values, " / ", nm))
  })
})

test_that("on the crossing branch worse_only is never more severe than both", {
  seen_crossing <- 0L
  ts_walk(function(sm, outcome_type, small_values, nm, ci) {
    both  <- ts_assess(sm, outcome_type, small_values, ci,
                       threshold_sides = "both")
    worse <- ts_assess(sm, outcome_type, small_values, ci,
                       threshold_sides = "worse_only")
    # Restricted to the branch the claim is about: once worse_only stops
    # crossing, Fig 4's OIS branch takes over and has two-level rules of its
    # own (next test).
    if (!identical(ts_fact(worse, "threshold_zone"), "crosses")) return()
    seen_crossing <<- seen_crossing + 1L
    expect_gte(worse$downgrade, both$downgrade)
    expect_identical(
      worse$downgrade, both$downgrade,
      info = paste0("a crossing interval must rate the same either way: ",
                    sm, " / ", small_values, " / ", nm))
  })
  # A restriction that admitted nothing would make the assertion vacuous.
  expect_gt(seen_crossing, 0L)
})

test_that("the global inequality is broken only by Fig 4's own OIS branch", {
  # Documented rather than worked around; SPEC.md 4.5.1b names these two.
  # Ceasing to cross the threshold moves the interval onto Fig 4's "No"
  # branch, where a large binary effect with a CI ratio at or above 3 earns
  # two levels that the crossing branch never consults ("sample size not
  # considered on this path"). That is Fig 4 answering its own question about
  # an interval it was not asked about before, not a sidedness bug.
  violations <- character(0)
  ts_walk(function(sm, outcome_type, small_values, nm, ci) {
    both  <- ts_assess(sm, outcome_type, small_values, ci,
                       threshold_sides = "both")
    worse <- ts_assess(sm, outcome_type, small_values, ci,
                       threshold_sides = "worse_only")
    if (worse$downgrade < both$downgrade) {
      violations <<- c(violations, paste(sm, small_values, nm, sep = "/"))
    }
  })
  expect_identical(
    sort(violations),
    sort(c("RR/desirable/crosses_lower", "RR/undesirable/crosses_upper")))

  # And the mechanism, pinned by name on one of them.
  worse <- ts_assess("RR", "relative", "desirable",
                     TS_INTERVALS$crosses_lower,
                     threshold_sides = "worse_only")
  expect_identical(worse$judgment, "very_serious")
  expect_identical(ts_fact(worse, "threshold_zone"), "within")
  expect_match(worse$notes, "CI ratio 3.67 >= 3.0", fixed = TRUE)
})

# --------------------------------------------------------------------------
# 3. The boolean table, both signs (SPEC.md 4.5.1b).
# --------------------------------------------------------------------------

test_that("the worse side is +Threshold for desirable, -Threshold for undesirable", {
  # Read against .ois_target_increase()'s reading of the same argument:
  # "undesirable" means the EVENTS are desirable, so a benefit is an increase
  # and the worse side is therefore the decrease.
  expect_identical(pmatools:::.threshold_worse_sign("desirable"), 1)
  expect_identical(pmatools:::.threshold_worse_sign("undesirable"), -1)
  expect_true(pmatools:::.ois_target_increase("undesirable", 0.3)$increase)
  expect_false(pmatools:::.ois_target_increase("desirable", 0.3)$increase)
})

test_that("threshold_zone follows the boolean table on the positive worse side", {
  zone <- function(nm) {
    ts_fact(ts_assess("RR", "relative", "desirable", TS_INTERVALS[[nm]],
                      threshold_sides = "worse_only"),
            "threshold_zone")
  }
  # Worse side = +0.5.
  expect_identical(zone("wholly_below"),     "within")   # never reached +T
  expect_identical(zone("crosses_lower"),    "within")   # past -T only
  expect_identical(zone("inside_band"),      "within")
  expect_identical(zone("crosses_upper"),    "crosses")
  expect_identical(zone("wholly_above"),     "beyond")
  expect_identical(zone("crosses_both"),     "crosses")
  expect_identical(zone("on_both_limits"),   "within")   # upper == +T
  expect_identical(zone("lower_on_upper_t"), "beyond")   # lower == +T
})

test_that("threshold_zone mirrors on the negative worse side", {
  zone <- function(nm) {
    ts_fact(ts_assess("RR", "relative", "undesirable", TS_INTERVALS[[nm]],
                      threshold_sides = "worse_only"),
            "threshold_zone")
  }
  # Worse side = -0.5.
  expect_identical(zone("wholly_below"),     "beyond")
  expect_identical(zone("crosses_lower"),    "crosses")
  expect_identical(zone("inside_band"),      "within")
  expect_identical(zone("crosses_upper"),    "within")   # past +T only
  expect_identical(zone("wholly_above"),     "within")
  expect_identical(zone("crosses_both"),     "crosses")
  expect_identical(zone("on_both_limits"),   "within")   # lower == -T
  expect_identical(zone("upper_on_lower_t"), "beyond")   # upper == -T
})

test_that("a CI past the better threshold only does not rate down one-sidedly", {
  # The headline case: an interval far past the BETTER-side threshold is not
  # evidence against non-inferiority. Continuous, so Fig 4's binary CI-ratio
  # rule cannot intervene and the contrast is the sidedness alone.
  ci    <- TS_INTERVALS$crosses_lower
  both  <- ts_assess("MD", "absolute", "desirable", ci,
                     threshold_sides = "both")
  worse <- ts_assess("MD", "absolute", "desirable", ci,
                     threshold_sides = "worse_only")
  expect_identical(both$judgment,  "serious")
  expect_identical(worse$judgment, "not_serious")
})

test_that("a CI spanning both thresholds still reaches -2 under worse_only", {
  worse <- ts_assess("MD", "absolute", "desirable", TS_INTERVALS$crosses_both,
                     threshold_sides = "worse_only")
  expect_identical(worse$judgment, "very_serious")
  expect_equal(worse$downgrade, -2)
  expect_match(worse$notes, "deliberately NOT one-sided", fixed = TRUE)
})

# --------------------------------------------------------------------------
# 4. threshold_zone as a vocabulary.
# --------------------------------------------------------------------------

test_that("threshold_zone is exhaustive and mutually exclusive over the grid", {
  seen <- character(0)
  ts_walk(function(sm, outcome_type, small_values, nm, ci) {
    for (sides in THRESHOLD_SIDES) {
      row   <- ts_assess(sm, outcome_type, small_values, ci,
                         threshold_sides = sides)
      facts <- attr(row, "facts")
      # Exactly one zone per assessment: recorded once, never twice, never
      # absent while a threshold applies.
      expect_identical(sum(facts$key == "threshold_zone"), 1L,
                       info = paste(sm, small_values, nm, sides))
      zone <- as.character(facts$value[facts$key == "threshold_zone"])
      expect_true(zone %in% PMA_IMPRE_THRESHOLD_ZONES,
                  info = paste(sm, small_values, nm, sides, zone))
      seen <<- union(seen, zone)
    }
  })
  # All three reachable, so none of the above is vacuous.
  expect_identical(sort(seen), sort(PMA_IMPRE_THRESHOLD_ZONES))
})

test_that("threshold_zone is absent when no threshold zone applies", {
  m <- ts_meta(-0.2, 1.5)
  row <- suppressWarnings(assess_imprecision(
    m, small_values = "desirable", threshold_type = "null",
    threshold_for_imprecision = 0))
  facts <- attr(row, "facts")
  expect_false("threshold_zone" %in% facts$key)
  expect_false("threshold_position" %in% facts$key)
})

test_that("threshold_zone never reaches a reader as prose", {
  # Machine-only, like flow_path: threshold_position already says it in words.
  expect_true("threshold_zone" %in% pmatools:::.FACT_KEYS_MACHINE_ONLY)
  facts <- attr(ts_assess("RR", "relative", "desirable",
                          TS_INTERVALS$crosses_upper), "facts")
  expect_false("threshold_zone" %in%
                 pmatools:::.drop_machine_only_facts(facts)$key)
})

# --------------------------------------------------------------------------
# 5. The strings a reviewer reads.
# --------------------------------------------------------------------------

test_that("the Fig 4 path names the worse side and the small_values", {
  row <- ts_assess("MD", "absolute", "desirable", TS_INTERVALS$crosses_upper,
                   threshold_sides = "worse_only")
  expect_match(row$notes, "the Threshold on the worse side (+Threshold)",
               fixed = TRUE)
  expect_match(row$notes, "crosses the Threshold on the worse side",
               fixed = TRUE)
  expect_match(row$notes,
               "worse side = +Threshold, from small_values = 'desirable'",
               fixed = TRUE)

  mirrored <- ts_assess("MD", "absolute", "undesirable",
                        TS_INTERVALS$crosses_lower,
                        threshold_sides = "worse_only")
  expect_match(mirrored$notes, "the Threshold on the worse side (-Threshold)",
               fixed = TRUE)
  expect_match(mirrored$notes,
               "worse side = -Threshold, from small_values = 'undesirable'",
               fixed = TRUE)
})

test_that("the one-sided strings replace the two-sided vocabulary, not join it", {
  row <- ts_assess("MD", "absolute", "desirable", TS_INTERVALS$crosses_lower,
                   threshold_sides = "worse_only")
  expect_match(row$notes, "stays inside the Threshold on the worse side",
               fixed = TRUE)
  # "within Threshold (trivial effect)" would be false of this interval.
  expect_false(grepl("within Threshold (trivial effect)", row$notes,
                     fixed = TRUE))
  expect_false(grepl("crosses one Threshold", row$notes, fixed = TRUE))

  beyond <- ts_assess("MD", "absolute", "desirable",
                      TS_INTERVALS$lower_on_upper_t,
                      threshold_sides = "worse_only")
  expect_match(beyond$notes, "entirely beyond the Threshold on the worse side",
               fixed = TRUE)
})

test_that("no user-visible string added by threshold_sides says MID", {
  for (nm in names(TS_INTERVALS)) {
    for (small_values in SMALL_VALUES_LEVELS) {
      row <- ts_assess("MD", "absolute", small_values, TS_INTERVALS[[nm]],
                       threshold_sides = "worse_only")
      expect_false(grepl("\\bMID\\b", row$notes),
                   info = paste(nm, small_values))
      facts <- attr(row, "facts")
      expect_false(any(grepl("\\bMID\\b", facts$value)),
                   info = paste(nm, small_values))
      expect_false(any(grepl("\\bMID\\b", facts$label)),
                   info = paste(nm, small_values))
    }
  }
})

test_that("the worse_only two-level prompt names the null-to-margin region", {
  row <- ts_assess("MD", "absolute", "desirable", TS_INTERVALS$crosses_upper,
                   threshold_sides = "worse_only")
  expect_match(row$notes,
               paste0("the CI is consistent with the intervention being ",
                      "better AND with it being worse by more than the ",
                      "Threshold"),
               fixed = TRUE)
  # The two-sided prompt is unchanged where it still applies.
  both <- ts_assess("MD", "absolute", "desirable", TS_INTERVALS$crosses_upper,
                    threshold_sides = "both")
  expect_match(both$notes,
               "Second Fig 4 two-level condition NOT auto-assessed:",
               fixed = TRUE)
})

# --------------------------------------------------------------------------
# 6. The gate.
# --------------------------------------------------------------------------

test_that("an unrecognised threshold_sides aborts as a threshold gate", {
  m <- ts_meta(-0.2, 1.5)
  cnd <- tryCatch(
    assess_imprecision(m, small_values = "desirable",
                       threshold_internal = TS_THRESHOLD,
                       threshold_for_imprecision = TS_THRESHOLD,
                       threshold_sides = "worse"),
    condition = function(e) e)
  expect_s3_class(cnd, "pmatools_threshold_gate")
  expect_match(conditionMessage(cnd),
               "threshold_sides must be 'both' or 'worse_only'", fixed = TRUE)
})

test_that("worse_only with no threshold aborts as a threshold gate, with no number", {
  m <- ts_meta(-0.2, 1.5)
  cnd <- tryCatch(
    assess_imprecision(m, small_values = "desirable",
                       threshold_type = "null",
                       threshold_for_imprecision = 0,
                       threshold_sides = "worse_only"),
    condition = function(e) e)
  expect_s3_class(cnd, "pmatools_threshold_gate")
  msg <- conditionMessage(cnd)
  expect_match(msg, "needs a threshold with two sides", fixed = TRUE)
  expect_match(msg, "the null has no worse side", fixed = TRUE)
  # A margin is a protocol design value, so the message offers no candidate.
  expect_match(msg, PMA_NO_MARGIN_PLACEHOLDER, fixed = TRUE)
  # The only digits in it belong to the Core GRADE figures it cites, so strip
  # those and assert nothing numeric survives that could be read as a
  # candidate margin.
  expect_false(grepl("[0-9]", gsub("(Core GRADE|Fig) [0-9]", "", msg)))
})

test_that("the enum is checked before the threshold requirement", {
  # A typo must not be reported as a missing margin, and must abort even on an
  # analysis that has no threshold to be missing.
  m <- ts_meta(-0.2, 1.5)
  expect_error(
    assess_imprecision(m, small_values = "desirable", threshold_type = "null",
                       threshold_for_imprecision = 0,
                       threshold_sides = "worse"),
    "threshold_sides must be", fixed = TRUE)
})
