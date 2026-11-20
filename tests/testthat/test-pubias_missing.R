# The Missing results (RoB-ME) tipping point (R/pubias_missing.R).
#
# The property these tests exist for above all others is the ORDER. Adding
# studies shrinks the pooled standard error, so a body of evidence can cross
# the decision threshold on precision alone - with the missing studies
# reporting exactly what the observed ones did. Step 6 asked on its own finds
# the tipping point far from everything and calls that case reassuring, when
# it is the opposite: the conclusion is one unpublished trial away from
# changing and the trial does not even have to disagree. So steps 3 and 4 run
# first, and the third block below is what holds them there.
#
# Nothing here rates anything. RoB-ME is not part of the Core GRADE
# algorithm; see test-pubias_status.R for the standing guard on that.

library(testthat)

# Ten observed studies, deliberately ordinary: the imputation borrows
# median(seTE * sqrt(n)) from them, and a pathological set would make every
# expectation below a statement about the pathology instead of about the
# algorithm.
SE_OBS <- c(0.20, 0.25, 0.30, 0.22, 0.28, 0.24, 0.26, 0.21, 0.23, 0.27)
N_OBS  <- c(100, 80, 60, 90, 70, 85, 75, 110, 95, 65)

# One analysis, varied one field at a time. `te_obs` is negative throughout
# (a benefit on a scale where smaller is better), so sign(TE_obs) = -1 and
# the direction gate has something to be asymmetric about.
tip <- function(results_known, te_obs = -0.40, se_pooled = 0.09, ...) {
  args <- list(
    results_known = results_known,
    n_missing     = rep(100, length(results_known)),
    te_obs        = te_obs,
    se_pooled     = se_pooled,
    tau2          = 0.05,
    ci_lower      = te_obs - 1.96 * se_pooled,
    ci_upper      = te_obs + 1.96 * se_pooled,
    pi_lower      = te_obs - 0.55,
    pi_upper      = te_obs + 0.55,
    se_studies    = SE_OBS,
    n_studies     = N_OBS,
    k             = length(SE_OBS)
  )
  do.call(.pubias_missing_tipping, modifyList(args, list(...)))
}

RK_EXTRACT  <- "Reported but data not extractable"
RK_NOT_MEAS <- "Not measured"
RK_NULLWARD <- "Measured but not reported (suspect P > 0.05)"
RK_FURTHER  <- "Measured but not reported (suspect P < 0.05)"
RK_OPPOSITE <- "Measured but not reported (in the opposite direction)"

# ---------------------------------------------------------------------------
# The six ordered outcomes
# ---------------------------------------------------------------------------

test_that("step 1: nothing is missing, so nothing can overturn the result", {
  res <- tip(character(0))
  expect_equal(res$step, 1L)
  expect_equal(res$state, "green")
  expect_match(res$reason, "No missing results")
})

test_that("step 2: every input the model needs, absent, is white", {
  # No pooled effect to move.
  expect_equal(tip(RK_EXTRACT, te_obs = NA_real_)$state, "unknown")
  expect_equal(tip(RK_EXTRACT, se_pooled = 0)$state, "unknown")

  # Fewer than three studies: meta computes no usable prediction interval,
  # and step 6 is anchored on it.
  below_k <- tip(RK_EXTRACT, k = 2)
  expect_equal(below_k$step, 2L)
  expect_match(below_k$reason, "prediction interval")

  # tau^2 at zero: there IS an interval, but it says nothing about
  # between-study spread, which is the quantity step 6 reads it for.
  expect_equal(tip(RK_EXTRACT, tau2 = 0)$state, "unknown")
  expect_equal(tip(RK_EXTRACT, tau2 = NA_real_)$state, "unknown")

  # No prediction interval reported at all.
  expect_equal(tip(RK_EXTRACT, pi_lower = NA_real_,
                   pi_upper = NA_real_)$state, "unknown")

  # No standard errors to borrow, so a missing study's precision cannot be
  # imputed by any route.
  no_se <- tip(RK_EXTRACT, se_studies = rep(NA_real_, 10))
  expect_equal(no_se$state, "unknown")
  expect_match(no_se$reason, "borrow")

  # Every white dot states why. A marker that says "not computed" without
  # saying what stopped it is worse than no marker.
  expect_true(all(nzchar(c(
    tip(RK_EXTRACT, k = 2)$reason,
    tip(RK_EXTRACT, tau2 = 0)$reason,
    tip(RK_EXTRACT, se_studies = rep(NA_real_, 10))$reason))))
})

test_that("step 3: the conclusion changes on precision alone", {
  # TE = -0.15 with se = 0.09 spans the null: -0.15 + 1.96*0.09 = +0.026.
  # Twenty missing studies shrink the standard error enough that the same
  # point estimate no longer spans it. Nothing has disagreed with anything.
  res <- tip(rep(RK_EXTRACT, 20), te_obs = -0.15)
  expect_equal(res$step, 3L)
  expect_equal(res$state, "red")
  expect_match(res$reason, "precision alone")
  # se_new is what did it, and it is smaller than the observed one.
  expect_lt(res$se_new, 0.09)
})

test_that("step 4: studies too imprecise to move anything cannot overturn", {
  # A guard, not a realistic dataset. Under the linear model of SPEC 3.4.8a
  # there is always a finite tipping point when the missing studies carry any
  # weight at all, because TE_new is affine and increasing in delta and
  # covers the whole real line. The one exception is W_miss = 0, and these
  # standard errors are large enough that 1/(se^2 + tau2) underflows to it.
  # The step exists so that the division by W_miss at step 6 cannot produce
  # an infinity that is then compared against the prediction interval and
  # comes out green by accident rather than by reasoning.
  res <- tip(rep(RK_EXTRACT, 3),
             se_studies = rep(1e160, 10), n_studies = rep(1, 10))
  expect_equal(res$step, 4L)
  expect_equal(res$state, "green")
  expect_match(res$reason, "too imprecise")
})

test_that("step 5: the direction gate fires when delta* lies the other way", {
  # The tipping point is +0.030 while the observed effect is -0.40, so the
  # missing studies would have to report something NULL-WARD of the observed
  # effect to change the conclusion. Labelling them "suspect P < 0.05" says
  # the opposite - that they lie further from the null - so nothing they
  # could plausibly report overturns the result.
  res <- tip(rep(RK_FURTHER, 30))
  expect_equal(res$step, 5L)
  expect_equal(res$state, "green")
  expect_match(res$reason, "suspected of lying in one direction")
})

test_that("step 5: the gate does not fire when delta* is inside the region", {
  # Same tipping point, opposite suspicion: "suspect P > 0.05" puts the
  # missing effects null-ward, which is exactly where delta* is, so the gate
  # is silent and the magnitude decides.
  res <- tip(rep(RK_NULLWARD, 30))
  expect_equal(res$step, 6L)
  expect_equal(res$state, "amber")

  # "In the opposite direction" is the third constrained label: with
  # sign(TE_obs) = -1 the suspected region is delta > 0, and delta* is +0.030.
  expect_equal(tip(rep(RK_OPPOSITE, 30))$step, 6L)
})

test_that("step 6: delta* against the CI and the prediction interval", {
  # Inside the pooled 95% CI: an ORDINARY missing result changes it.
  red <- tip(rep(RK_EXTRACT, 20), te_obs = -0.20)
  expect_equal(red$step, 6L)
  expect_equal(red$state, "red")
  expect_gte(red$delta_star, -0.20 - 1.96 * 0.09)
  expect_lte(red$delta_star, -0.20 + 1.96 * 0.09)
  expect_match(red$reason, "confidence interval")

  # Outside the CI, inside the prediction interval: a PLAUSIBLE one does.
  amber <- tip(rep(RK_EXTRACT, 30))
  expect_equal(amber$state, "amber")
  expect_match(amber$reason, "prediction interval")

  # Outside the prediction interval: only a study unlike any observed does.
  green <- tip(rep(RK_EXTRACT, 2))
  expect_equal(green$state, "green")
  expect_gt(green$delta_star, -0.40 + 0.55)
})

# ---------------------------------------------------------------------------
# The direction gate's union rule
# ---------------------------------------------------------------------------

test_that("one unconstrained row makes the union everything", {
  # 29 rows that would fire the gate on their own, plus one whose label says
  # nothing about direction. An unconstrained row means no direction can be
  # ruled out, so the gate must stay silent - the conservative answer, and
  # the right one.
  fires <- tip(rep(RK_FURTHER, 30))
  expect_equal(fires$step, 5L)

  union <- tip(c(rep(RK_FURTHER, 29), RK_EXTRACT))
  expect_equal(union$step, 6L)
  expect_equal(union$delta_star, fires$delta_star)

  # Free text is unconstrained for the same reason: the app's column accepts
  # anything, and an unrecognised answer rules nothing out.
  expect_equal(tip(c(rep(RK_FURTHER, 29), "lost in a house move"))$step, 6L)

  # "Not measured" is NOT: it leaves the computation before the gate, so it
  # cannot hold the gate shut (see the block on excluded rows below).
  expect_equal(tip(c(rep(RK_FURTHER, 29), RK_NOT_MEAS))$step, 5L)
})

test_that("the labels are matched loosely, and anything unknown is free text", {
  expect_equal(.pubias_missing_mechanism(RK_NOT_MEAS), "not_measured")
  expect_equal(.pubias_missing_mechanism(RK_EXTRACT), "unconstrained")
  expect_equal(.pubias_missing_mechanism(RK_NULLWARD), "null_ward")
  expect_equal(.pubias_missing_mechanism(RK_FURTHER), "further_out")
  expect_equal(.pubias_missing_mechanism(RK_OPPOSITE), "opposite")

  # The column is free text behind an autocomplete datalist, so spacing and
  # case are the reviewer's, not the app's. The spec writes the last label
  # without the app's "in the".
  expect_equal(.pubias_missing_mechanism(
    "measured but not reported (opposite direction)"), "opposite")
  expect_equal(.pubias_missing_mechanism(
    "  Measured but not reported (suspect P>0.05)  "), "null_ward")
  expect_equal(.pubias_missing_mechanism("no idea"), "unconstrained")
  expect_equal(.pubias_missing_mechanism(NA_character_), "unconstrained")

  # The vocabulary is closed. A sixth mechanism would need a region in
  # .pubias_missing_in_suspected() before it could mean anything, and this is
  # what stops one appearing without it.
  every <- .pubias_missing_mechanism(c(
    RK_NOT_MEAS, RK_EXTRACT, RK_NULLWARD, RK_FURTHER, RK_OPPOSITE, "", "?"))
  expect_true(all(every %in% .PUBIAS_MISSING_MECHANISMS))
  expect_setequal(unique(every), .PUBIAS_MISSING_MECHANISMS)
})

# ---------------------------------------------------------------------------
# "Not measured" rows are excluded, not capped
# ---------------------------------------------------------------------------
# Under RoB-ME (Page et al. BMJ 2023) a result that was never measured cannot
# have been selectively unreported, so it is not a missing result in the bias
# sense. Such rows leave the computation before m is counted: they add no
# weight, and they take no part in the direction gate.

# An analysis whose conclusion spans the decision threshold (T = 0.5 on the
# TE scale), so that the nearer crossing lies FURTHER from the null than the
# observed effect: the missing studies would have to report a more
# favourable effect to change the conclusion.
RK_ASSUMED_NS <- "Measured, result not available (assumed P > 0.05)"
tip_spans <- function(results_known, ...) {
  tip(results_known, threshold_internal = 0.5, ...)
}

test_that("one 'Not measured' row no longer holds the gate shut", {
  # The absenteeism case that prompted the change: four results suspected
  # null-ward, one never measured, and a tipping value more favourable than
  # the observed effect. Before, the never-measured row was unconstrained and
  # the dot came out red; nothing the four could plausibly report moves the
  # conclusion, so the dot is green.
  res <- tip_spans(c(RK_NOT_MEAS, rep(RK_ASSUMED_NS, 4)))
  expect_equal(.pubias_missing_mechanism(RK_ASSUMED_NS), "null_ward")
  expect_lt(res$delta_star, -0.40)
  expect_equal(res$step, 5L)
  expect_equal(res$state, "green")
  expect_equal(res$n_not_measured, 1L)
  expect_equal(res$n_undirected, 0L)
  # The excluded row is mentioned, and "Reference only" still closes it.
  expect_match(res$reason, "1 missing result recorded as not measured is left out")
  expect_match(res$reason, "Reference only - it rates nothing.$")
})

test_that("an all-'Not measured' table is answered at step 1", {
  res <- tip(rep(RK_NOT_MEAS, 20), te_obs = -0.20)
  expect_equal(res$step, 1L)
  expect_equal(res$state, "green")
  expect_equal(res$n_not_measured, 20L)
  expect_match(res$reason, "never measured")
  expect_match(res$reason, "RoB-ME does not count")
  expect_true(is.na(res$delta_star))
})

test_that("'Not measured' rows do not change the tipping point", {
  without <- tip(rep(RK_NULLWARD, 30))
  with_nm <- tip(c(rep(RK_NULLWARD, 30), rep(RK_NOT_MEAS, 5)))
  expect_equal(with_nm$delta_star, without$delta_star)
  expect_equal(with_nm$se_new, without$se_new)
  expect_equal(with_nm$state, without$state)
  expect_equal(with_nm$n_not_measured, 5L)
  expect_match(with_nm$reason, "5 missing results recorded as not measured")
  expect_false(grepl("not measured", without$reason, fixed = TRUE))

  # n_missing is dropped in parallel: giving the never-measured rows a huge
  # sample size changes nothing, because their weight is never computed.
  big_nm <- tip(c(rep(RK_NULLWARD, 30), rep(RK_NOT_MEAS, 5)),
                n_missing = c(rep(100, 30), rep(1e6, 5)))
  expect_equal(big_nm$delta_star, without$delta_star)
})

test_that("undirected rows keep the gate shut, and the reason says so", {
  # The directed rows alone would fire the gate ...
  directed <- tip_spans(rep(RK_ASSUMED_NS, 4))
  expect_equal(directed$step, 5L)

  # ... and one undirected row keeps it shut: conservative, and unchanged.
  # The reason now tells the reviewer that this row is the whole reason.
  res <- tip_spans(c(rep(RK_ASSUMED_NS, 4), RK_EXTRACT))
  expect_false(identical(res$step, 5L))
  expect_equal(res$n_undirected, 1L)
  expect_match(res$reason, "1 missing result carries no suspected direction")
  expect_match(res$reason, "assumed P > 0.05", fixed = TRUE)
  expect_match(res$reason, "Reference only - it rates nothing.$")

  # No warning where the directed rows would not have fired the gate either:
  # there the undirected row changed nothing.
  inside <- tip(c(rep(RK_NULLWARD, 29), RK_EXTRACT))
  expect_equal(inside$step, 6L)
  expect_false(grepl("no suspected direction", inside$reason, fixed = TRUE))

  # Nor where there is nothing directed at all.
  all_undirected <- tip_spans(rep(RK_EXTRACT, 5))
  expect_equal(all_undirected$n_undirected, 5L)
  expect_false(grepl("no suspected direction", all_undirected$reason,
                     fixed = TRUE))
})

test_that("every return path carries the same names", {
  expected <- c("step", "state", "reason", "delta_star", "se_new",
                "n_not_measured", "n_undirected")
  paths <- list(
    tip(character(0)),                                    # step 1
    tip(rep(RK_NOT_MEAS, 3)),                             # step 1, all NM
    tip(RK_EXTRACT, k = 2),                               # step 2
    tip(rep(RK_EXTRACT, 20), te_obs = -0.15),             # step 3
    tip(rep(RK_EXTRACT, 3), se_studies = rep(1e160, 10),
        n_studies = rep(1, 10)),                          # step 4
    tip(rep(RK_FURTHER, 30)),                             # step 5
    tip(rep(RK_EXTRACT, 30)))                             # step 6
  for (res in paths) expect_named(res, expected)
  expect_equal(vapply(paths, `[[`, integer(1), "step"),
               c(1L, 1L, 2L, 3L, 4L, 5L, 6L))
})

# ---------------------------------------------------------------------------
# The imputation
# ---------------------------------------------------------------------------

test_that("a missing study's se is borrowed as c_med / sqrt(n)", {
  se_j <- .pubias_missing_impute_se(n_missing = c(100, 25),
                                    se_studies = SE_OBS, n_studies = N_OBS)
  c_med <- stats::median(SE_OBS * sqrt(N_OBS))
  expect_equal(se_j, c(c_med / 10, c_med / 5))
  # A quarter of the sample size doubles the standard error, which is the
  # whole point of the 1/sqrt(n) form: it holds for SMD, MD, log OR and
  # log RR alike, so one formula covers every measure the app pools.
  expect_equal(se_j[2] / se_j[1], 2)
})

test_that("a blank n falls back to the median observed se", {
  se_j <- .pubias_missing_impute_se(n_missing = c(NA, 0, 100),
                                    se_studies = SE_OBS, n_studies = N_OBS)
  se_med <- stats::median(SE_OBS)
  expect_equal(se_j[1], se_med)
  expect_equal(se_j[2], se_med)
  expect_false(isTRUE(all.equal(se_j[3], se_med)))

  # A dataset that carries no arm sizes at all uses the median for every
  # row, rather than failing: a median standard error is still a better
  # guess than none.
  expect_equal(
    .pubias_missing_impute_se(n_missing = c(100, 50),
                              se_studies = SE_OBS,
                              n_studies = rep(NA_real_, 10)),
    rep(se_med, 2))
})

test_that("no usable observed se means the imputation is impossible", {
  expect_null(.pubias_missing_impute_se(100, rep(NA_real_, 3), c(10, 20, 30)))
  expect_null(.pubias_missing_impute_se(100, c(0, -1), c(10, 20)))
})

# ---------------------------------------------------------------------------
# The dot wrapper
# ---------------------------------------------------------------------------

test_that("the dot is the tipping point's state and reason, and no more", {
  res <- .pubias_missing_tipping(
    results_known = rep(RK_EXTRACT, 20), n_missing = rep(100, 20),
    te_obs = -0.20, se_pooled = 0.09, tau2 = 0.05,
    ci_lower = -0.376, ci_upper = -0.024,
    pi_lower = -0.75, pi_upper = 0.35,
    se_studies = SE_OBS, n_studies = N_OBS, k = 10)
  dot <- .pubias_missing_dot(
    results_known = rep(RK_EXTRACT, 20), n_missing = rep(100, 20),
    te_obs = -0.20, se_pooled = 0.09, tau2 = 0.05,
    ci_lower = -0.376, ci_upper = -0.024,
    pi_lower = -0.75, pi_upper = 0.35,
    se_studies = SE_OBS, n_studies = N_OBS, k = 10)

  expect_equal(dot, list(state = res$state, reason = res$reason))
  expect_true(dot$state %in% PMA_PUBIAS_DOT_STATES)
  # The tipping point itself does not travel: it is a number the tab could
  # print, not something a caller should branch on as if it were a rating.
  expect_null(dot$delta_star)
})
