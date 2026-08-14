library(testthat)

skip_if_not_installed("meta")

# A study reporting a denominator but no event count (eg it contributed a
# continuous outcome only) used to be dropped from event.c while staying in
# n.c. That left the two vectors different lengths, metaprop() errored, and the
# tryCatch handed back the crude proportion labelled as a pooled estimate.
gappy_metabin <- function() {
  d <- data.frame(
    studlab = paste0("S", 1:6),
    event_e = c(12,  30,  4, 18,  9, 21),
    n_e     = c(60, 140, 55, 90, 70, 110),
    event_c = c(20,  46, NA,  6, 15, 34),
    n_c     = c(60, 140, 58, 90, 70, 110)
  )
  suppressWarnings(
    meta::metabin(d$event_e, d$n_e, d$event_c, d$n_c, d$studlab,
                  sm = "OR", method = "Inverse", method.tau = "REML")
  )
}

test_that("a study with a missing event.c but a present n.c keeps both vectors aligned", {
  m <- gappy_metabin()

  # Precondition: the object really does carry the ragged pair.
  expect_equal(length(m$event.c), length(m$n.c))
  expect_equal(sum(is.na(m$event.c)), 1L)
  expect_equal(sum(is.na(m$n.c)), 0L)

  keep <- !is.na(m$event.c) & !is.na(m$n.c) & m$n.c > 0
  expect_equal(
    .compute_control_risk(m, method = "simple"),
    sum(m$event.c[keep]) / sum(m$n.c[keep])
  )

  # The old code divided the complete-case numerator by the full denominator,
  # so the incomplete study's 58 controls diluted the crude risk.
  stale <- sum(m$event.c[keep]) / sum(m$n.c)
  expect_false(isTRUE(all.equal(.compute_control_risk(m, method = "simple"), stale)))
})

test_that("metaprop path runs instead of falling back when a study lacks event.c", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("lme4")

  m <- gappy_metabin()

  # The fallback announces itself; reaching it at all is the regression.
  expect_no_warning(res <- .compute_control_risk(m, method = "metaprop"))

  keep <- !is.na(m$event.c) & !is.na(m$n.c) & m$n.c > 0
  fit <- meta::metaprop(event = m$event.c[keep], n = m$n.c[keep],
                        method = "GLMM", sm = "PLOGIT", method.tau = "ML")
  expect_equal(res, stats::plogis(fit$TE.random))

  # ...and it is a genuinely different number from the crude proportion, so a
  # silent fallback would have been visible in a SoF absolute-risk column.
  crude <- sum(m$event.c[keep]) / sum(m$n.c[keep])
  expect_false(isTRUE(all.equal(res, crude, tolerance = 1e-4)))
})

test_that("bundled cbti_depression reaches the metaprop estimate, not the crude one", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("lme4")

  d <- pmatools::cbti_depression
  long <- data.frame(
    studlab = d$study,
    treat   = ifelse(d$treatment == "CBT-I", "experimental", "control"),
    n       = d$n_randomized,
    event   = d$d_r
  )
  m <- run_ma(long, outcome_type = "binary", sm = "OR")

  expect_no_warning(mp <- .compute_control_risk(m, method = "metaprop"))
  simple <- .compute_control_risk(m, method = "simple")

  # ~156 vs ~176 per 1,000: far enough apart to move an absolute-risk column.
  expect_equal(mp * 1000, 155.6, tolerance = 0.5)
  expect_equal(simple * 1000, 175.5, tolerance = 0.5)
})

test_that("control risk is NULL when no study has a usable control arm", {
  d <- data.frame(
    studlab = c("S1", "S2"),
    event_e = c(5, 8), n_e = c(40, 50),
    event_c = c(NA_real_, NA_real_), n_c = c(40, 50)
  )
  m <- suppressWarnings(
    meta::metabin(d$event_e, d$n_e, d$event_c, d$n_c, d$studlab, sm = "OR")
  )

  expect_null(.compute_control_risk(m, method = "simple"))
  expect_null(.compute_control_risk(m, method = "metaprop"))
})

# --------------------------------------------------------------------------
# One control-arm risk, three arguments (v0.5.1)
#
# threshold_baseline, ois_p0 and baseline_risk all name the control-arm event
# rate. A value given to any one of them must reach all three, an explicit
# value must never be displaced by an inherited one, and whichever won has to
# say so.
# --------------------------------------------------------------------------

# Pooled CER = (18 + 27) / (100 + 150) = 0.18, so an inherited value of 0.25 is
# distinguishable from the pooled default in every assertion below.
control_risk_metabin <- function(sm = "RR") {
  suppressWarnings(meta::metabin(
    event.e = c(12, 20), n.e = c(100, 150),
    event.c = c(18, 27), n.c = c(100, 150),
    studlab = c("Study A", "Study B"), sm = sm, method = "MH"
  ))
}

# The resolution recorded on a rating that exercises all three uses: an ARD
# threshold needs a baseline to convert, a binary OIS needs one to be powered
# from, and a relative outcome prints one in the SoF.
control_risk_used <- function(...) {
  g <- suppressWarnings(grade_meta(
    control_risk_metabin(),
    threshold       = 0.05,
    threshold_scale = "ard",
    outcome_name    = "Control risk",
    small_values    = "desirable",
    ...
  ))
  g$control_risk
}

test_that("a value given to any one argument reaches all three uses", {
  for (arg in CONTROL_RISK_ARGS) {
    res <- do.call(control_risk_used, stats::setNames(list(0.25), arg))

    expect_equal(res$value, 0.25)
    expect_identical(res$donor, arg)
    expect_setequal(res$inherited, setdiff(CONTROL_RISK_ARGS, arg))

    expect_equal(res$used$threshold_baseline, 0.25)
    expect_equal(res$used$ois_p0, 0.25)
    expect_equal(res$used$baseline_risk, 0.25)
  }
})

test_that("an explicitly passed value beats an inherited one", {
  # The Summary of Findings may be drawn against a named risk group while the
  # threshold is converted at the trials' own control rate -- a difference the
  # fallback must not erase.
  res <- control_risk_used(threshold_baseline = 0.25, baseline_risk = 0.40)

  expect_equal(res$used$threshold_baseline, 0.25)
  expect_equal(res$used$baseline_risk, 0.40)
  # ois_p0 was the only one left unset, so it inherits from the first donor.
  expect_equal(res$used$ois_p0, 0.25)
  expect_identical(res$donor, "threshold_baseline")
  expect_identical(res$inherited, "ois_p0")

  # ...and in the other direction: ois_p0 keeps its own value while
  # baseline_risk inherits.
  res2 <- control_risk_used(threshold_baseline = 0.25, ois_p0 = 0.10)
  expect_equal(res2$used$ois_p0, 0.10)
  expect_equal(res2$used$threshold_baseline, 0.25)
  expect_equal(res2$used$baseline_risk, 0.25)
})

test_that("with none of the three given, all three take the pooled default", {
  res <- control_risk_used()

  expect_null(res$value)
  expect_null(res$donor)
  expect_length(res$inherited, 0L)
  expect_null(res$note)

  expect_equal(res$used$threshold_baseline, 0.18)
  expect_equal(res$used$ois_p0, 0.18)
  expect_equal(res$used$baseline_risk, 0.18)
})

test_that("the provenance names the donor and every argument that inherited", {
  res <- control_risk_used(ois_p0 = 0.25)
  expect_match(res$note, "supplied as `ois_p0`", fixed = TRUE)
  expect_match(res$note, "`threshold_baseline`", fixed = TRUE)
  expect_match(res$note, "`baseline_risk`", fixed = TRUE)
  expect_match(res$note, "0.2500", fixed = TRUE)

  # Where a reader meets it: the Imprecision domain notes, which summary(),
  # the Evidence Profile and the exported bundle all reproduce.
  g <- suppressWarnings(grade_meta(
    small_values = "desirable",
    control_risk_metabin(), threshold = 0.05, threshold_scale = "ard",
    ois_p0 = 0.25, outcome_name = "Control risk"))
  impre <- g$domain_assessments$notes[
    g$domain_assessments$domain == "Imprecision"]
  expect_match(impre, "supplied as `ois_p0`", fixed = TRUE)

  # Nothing to report when nothing was inherited: three explicit values leave
  # the domain notes alone.
  quiet <- suppressWarnings(grade_meta(
    small_values = "desirable",
    control_risk_metabin(), threshold = 0.05, threshold_scale = "ard",
    threshold_baseline = 0.25, ois_p0 = 0.30, baseline_risk = 0.40,
    outcome_name = "Control risk"))
  expect_null(quiet$control_risk$note)
  expect_no_match(
    quiet$domain_assessments$notes[
      quiet$domain_assessments$domain == "Imprecision"],
    "inherited it", fixed = TRUE)
})

test_that("a character baseline_risk names a method, so it does not donate", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("lme4")

  res <- control_risk_used(baseline_risk = "metaprop")

  expect_null(res$donor)
  # The table gets the GLMM-pooled proportion; the other two uses each compute
  # their own pooled default on the analysis they are judging. (On this dataset
  # both control arms sit at exactly 0.18, so the two agree numerically -- what
  # is asserted is that the method reached the table and nowhere else.)
  expect_equal(res$used$threshold_baseline, 0.18)
  expect_equal(res$used$ois_p0, 0.18)
  expect_equal(res$used$baseline_risk,
               suppressWarnings(.compute_control_risk(control_risk_metabin(),
                                                      method = "metaprop")))

  # A dataset whose control arms disagree separates the two numbers, so a
  # donated "metaprop" would be visible.
  spread <- suppressWarnings(meta::metabin(
    event.e = c(6, 40), n.e = c(100, 150),
    event.c = c(10, 60), n.c = c(100, 150),
    studlab = c("Study A", "Study B"), sm = "RR", method = "MH"))
  g <- suppressWarnings(grade_meta(
    small_values = "desirable",
    spread, threshold = 0.05, threshold_scale = "ard",
    baseline_risk = "metaprop", outcome_name = "Control risk"))
  expect_equal(g$control_risk$used$threshold_baseline,
               .compute_control_risk(spread, method = "simple"))
  expect_equal(g$control_risk$used$ois_p0,
               .compute_control_risk(spread, method = "simple"))
  expect_false(isTRUE(all.equal(g$control_risk$used$baseline_risk,
                                g$control_risk$used$ois_p0,
                                tolerance = 1e-4)))
})

test_that("a value at the edge of the interval stays where it was put", {
  # baseline_risk accepts the closed interval, threshold_baseline does not.
  # Donating a 0 would turn a working call into an abort somewhere else.
  res <- .resolve_control_risk(baseline_risk = 0)
  expect_null(res$donor)
  expect_null(res$threshold_baseline)
  expect_null(res$ois_p0)
  expect_equal(res$baseline_risk, 0)
})

test_that("NA is treated as unset, so it inherits like NULL", {
  res <- .resolve_control_risk(threshold_baseline = 0.25, ois_p0 = NA_real_)
  expect_equal(res$ois_p0, 0.25)
  expect_equal(res$baseline_risk, 0.25)
})

test_that("suggest_threshold covers the risk-difference measure metabin emits", {
  d <- data.frame(
    studlab = paste0("S", 1:3),
    event_e = c(5, 8, 11), n_e = c(40, 50, 60),
    event_c = c(9, 14, 20), n_c = c(40, 50, 60)
  )
  m <- suppressWarnings(
    meta::metabin(d$event_e, d$n_e, d$event_c, d$n_c, d$studlab, sm = "RD")
  )
  expect_identical(m$sm, "RD")

  s <- suggest_threshold(m)
  expect_equal(s$threshold_user, 0.05)
  expect_equal(s$threshold_scale, "ard")
  expect_equal(s$source, "package_convention")

  # threshold_scale = "auto" must resolve the same measure without erroring.
  conv <- threshold_to_te_scale(0.05, "auto", m$sm)
  expect_equal(conv$threshold_kind, "ard")
  expect_equal(conv$threshold_internal, 0.05)
})
