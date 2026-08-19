# The publication-bias reference-tab status dots (R/pubias_status.R).
#
# Three properties these tests exist for.
#
# First, the dot RATES NOTHING. No value produced here may reach
# assess_pubias() or grade_meta(); the wizard's answers stay the only thing
# that rates the domain. The last block below is the standing guard on that.
#
# Second, "not computed" is its own state and never a colour. Each of these
# tabs declines to compute on exactly the sparse data where reporting bias is
# most likely, so a green dot standing in for "we did not look" would be
# backwards.
#
# Third, the trim-and-fill comparison runs on a scale whose null is zero. Read
# off a raw odds ratio, |OR| = 2.0 and |OR| = 0.5 - equidistant from the null
# in fact - come out four-fold apart, and every zone and magnitude rule the
# check applies would be wrong for one of them.

library(testthat)

# The app's step3_p1_from_ratio(), injected the way the app injects it. The
# package deliberately does not own this map (see R/pubias_status.R): what is
# under test is the DECISION about which scale to run on, not the arithmetic
# of an event rate.
p1_from_ratio <- function(sm, p0, ratio) {
  if (identical(sm, "OR")) {
    odds <- (p0 / (1 - p0)) * ratio
    odds / (1 + odds)
  } else {
    p0 * ratio
  }
}

# ---------------------------------------------------------------------------
# Funnel
# ---------------------------------------------------------------------------

test_that("Egger's p falls into the three bands at 0.05 and 0.01", {
  expect_equal(.pubias_funnel_dot(0.40)$state, "green")
  expect_equal(.pubias_funnel_dot(0.05)$state, "green")   # boundary is green
  expect_equal(.pubias_funnel_dot(0.049)$state, "amber")
  expect_equal(.pubias_funnel_dot(0.01)$state, "amber")   # boundary is amber
  expect_equal(.pubias_funnel_dot(0.009)$state, "red")

  # The tooltip names the number it branched on, so a reviewer can check the
  # dot against the callout printed under the funnel.
  expect_match(.pubias_funnel_dot(0.009)$reason, "0.009")
  expect_match(.pubias_funnel_dot(0.40)$reason, "rates nothing")
})

test_that("every funnel path that did not run is white, with a reason", {
  below_k <- .pubias_funnel_dot(0.001, k_ok = FALSE)
  expect_equal(below_k$state, "unknown")
  expect_match(below_k$reason, "10 studies")

  # Rare-event data is checked BEFORE the k gate: at k >= 10 the k gate says
  # nothing, and letting an invalid p value paint a red dot is the outcome
  # the ordering exists to avoid.
  rare <- .pubias_funnel_dot(0.001, k_ok = TRUE, rare_flow = TRUE)
  expect_equal(rare$state, "unknown")
  expect_match(rare$reason, "rare-event")

  # Both at once still reports the rare-event reason, which is the more
  # important of the two: underpowered is not the same as invalid.
  expect_match(.pubias_funnel_dot(0.001, k_ok = FALSE, rare_flow = TRUE)$reason,
               "rare-event")

  expect_equal(.pubias_funnel_dot(NA_real_)$state, "unknown")
  expect_equal(.pubias_funnel_dot(0.02, feasible = FALSE)$state, "unknown")
  expect_equal(.pubias_funnel_dot(numeric(0))$state, "unknown")
})

# ---------------------------------------------------------------------------
# Trim-and-fill: the scale
# ---------------------------------------------------------------------------

test_that("a binary outcome is compared on the absolute risk difference", {
  # OR 0.60 -> 0.80 at a control risk of 200 per 1,000.
  scaled <- .pubias_trimfill_scale(
    te_original = log(0.60), te_adjusted = log(0.80),
    sm = "OR", binary = TRUE, baseline_risk = 0.20,
    threshold_abs1000 = 50, p1_from_ratio = p1_from_ratio)

  expect_true(scaled$ok)
  expect_equal(scaled$scale, "absolute1000")
  # odds(0.20) = 0.25; 0.25 * 0.60 = 0.15 -> p1 = 0.1304 -> -69.6 per 1,000.
  expect_equal(scaled$te_original, 1000 * (0.15 / 1.15 - 0.20),
               tolerance = 1e-8)
  expect_equal(scaled$te_adjusted, 1000 * (0.20 / 1.20 - 0.20),
               tolerance = 1e-8)
  expect_equal(scaled$threshold, 50)
  # sm is dropped: the values are risk differences now, and the direction
  # check would exponentiate them for display if it were still told "OR".
  expect_null(scaled$sm)
})

test_that("a continuous outcome keeps its internal scale untouched", {
  scaled <- .pubias_trimfill_scale(
    te_original = -0.50, te_adjusted = -0.40,
    sm = "SMD", binary = FALSE, threshold_internal = 0.20)

  expect_true(scaled$ok)
  expect_equal(scaled$scale, "internal")
  expect_equal(scaled$te_original, -0.50)
  expect_equal(scaled$te_adjusted, -0.40)
  expect_equal(scaled$threshold, 0.20)
  expect_equal(scaled$sm, "SMD")

  # RoM is a ratio, but on a continuous outcome there is no event rate to
  # convert to, so it stays on the log scale - whose null is 0, which is all
  # the direction rules need.
  rom <- .pubias_trimfill_scale(te_original = log(1.30),
                                te_adjusted = log(1.10),
                                sm = "RoM", binary = FALSE)
  expect_true(rom$ok)
  expect_equal(rom$scale, "internal")
})

test_that("a risk-difference analysis needs no ratio conversion", {
  scaled <- .pubias_trimfill_scale(
    te_original = -0.070, te_adjusted = -0.030,
    sm = "RD", binary = TRUE, baseline_risk = 0.20,
    threshold_abs1000 = 50)
  expect_true(scaled$ok)
  expect_equal(scaled$te_original, -70)
  expect_equal(scaled$te_adjusted, -30)
})

test_that("no baseline risk means no dot, never a silent scale change", {
  for (bad in list(NULL, NA_real_, 0, 1, -0.2)) {
    scaled <- .pubias_trimfill_scale(
      te_original = log(0.60), te_adjusted = log(0.80),
      sm = "OR", binary = TRUE, baseline_risk = bad,
      p1_from_ratio = p1_from_ratio)
    expect_false(scaled$ok)
    expect_match(scaled$reason, "baseline")
  }
})

# ---------------------------------------------------------------------------
# Trim-and-fill: the dot
# ---------------------------------------------------------------------------

test_that("the trim-and-fill dot is the risk-of-bias direction verdict", {
  # Rule 2 (same zone, inflation within the 20% mark) -> not_serious -> green.
  green <- .pubias_trimfill_dot(
    te_original = -0.55, te_adjusted = -0.50, small_values = "desirable",
    sm = "SMD", threshold_internal = 0.20)
  expect_equal(green$state, "green")

  # Rule 3 (same zone, bias-favouring inflation past the 20% mark) -> one
  # level -> amber.
  amber <- .pubias_trimfill_dot(
    te_original = -0.80, te_adjusted = -0.50, small_values = "desirable",
    sm = "SMD", threshold_internal = 0.20)
  expect_equal(amber$state, "amber")

  # Rule 5 (the zone changes across the null, with a threshold supplied) ->
  # two levels -> red.
  red <- .pubias_trimfill_dot(
    te_original = -0.50, te_adjusted = 0.50, small_values = "desirable",
    sm = "SMD", threshold_internal = 0.20)
  expect_equal(red$state, "red")

  expect_match(green$reason, "rates nothing")
})

test_that("the 20% mark is PMA_ROB_INFLATION_THRESHOLD, not a second copy", {
  # Just inside and just outside the shared threshold, with everything else
  # held equal. If this file ever grew its own constant, one of these two
  # would keep passing after PMA_ROB_INFLATION_THRESHOLD moved.
  inside <- 0.50 * (1 + PMA_ROB_INFLATION_THRESHOLD - 0.01)
  outside <- 0.50 * (1 + PMA_ROB_INFLATION_THRESHOLD + 0.01)
  expect_equal(.pubias_trimfill_dot(
    te_original = -inside, te_adjusted = -0.50,
    small_values = "desirable", sm = "SMD",
    threshold_internal = 0.20)$state, "green")
  expect_equal(.pubias_trimfill_dot(
    te_original = -outside, te_adjusted = -0.50,
    small_values = "desirable", sm = "SMD",
    threshold_internal = 0.20)$state, "amber")
})

test_that("the binary dot judges on the absolute scale, not the log ratio", {
  # OR 0.50 vs OR 0.80 at a control risk of 200 per 1,000 with a 50 per 1,000
  # threshold. On the log scale the two are 0.693 and 0.223 apart from the
  # null; on the absolute scale they are -88.9 and -33.3 per 1,000. Only the
  # second pair can be compared against a threshold stated per 1,000, which
  # is the one the reviewer typed.
  dot <- .pubias_trimfill_dot(
    te_original = log(0.50), te_adjusted = log(0.80),
    small_values = "desirable", sm = "OR", binary = TRUE,
    baseline_risk = 0.20, threshold_abs1000 = 50,
    p1_from_ratio = p1_from_ratio)
  # -88.9 is below -50, -33.3 is inside the trivial zone: the zones differ
  # without a sign flip, which is rule 4 -> one level -> amber.
  expect_equal(dot$state, "amber")
  expect_match(dot$reason, "absolute risk difference")

  # An adjusted effect of exactly OR 1.00 lands on 0 per 1,000, so the
  # inflation ratio is undefined - but the ZONE comparison still works, and
  # rule 4 is what decides. The dot is a colour, not a white one: the
  # undefined ratio is a missing input to one rule, not to the check.
  at_null <- .pubias_trimfill_dot(
    te_original = log(0.50), te_adjusted = 0, small_values = "desirable",
    sm = "OR", binary = TRUE, baseline_risk = 0.20,
    threshold_abs1000 = 50, p1_from_ratio = p1_from_ratio)
  expect_equal(at_null$state, "amber")
})

test_that("every trim-and-fill path that did not run is white", {
  base <- list(te_original = -0.50, te_adjusted = -0.40,
               small_values = "desirable", sm = "SMD")

  expect_equal(do.call(.pubias_trimfill_dot,
                       c(base, list(k_ok = FALSE)))$state, "unknown")

  no_dir <- do.call(.pubias_trimfill_dot,
                    c(base[c("te_original", "te_adjusted", "sm")],
                      list(small_values = NULL)))
  expect_equal(no_dir$state, "unknown")
  expect_match(no_dir$reason, "direction")

  # trimfill() failed, so there is no adjusted effect to compare against.
  expect_equal(.pubias_trimfill_dot(
    te_original = -0.50, te_adjusted = NA_real_,
    small_values = "desirable", sm = "SMD")$state, "unknown")

  # Binary, but no event-rate map was supplied for the ratio measure.
  expect_equal(.pubias_trimfill_dot(
    te_original = log(0.60), te_adjusted = log(0.80),
    small_values = "desirable", sm = "OR", binary = TRUE,
    baseline_risk = 0.20)$state, "unknown")

  # Binary, but no baseline risk.
  expect_equal(.pubias_trimfill_dot(
    te_original = log(0.60), te_adjusted = log(0.80),
    small_values = "desirable", sm = "OR", binary = TRUE,
    p1_from_ratio = p1_from_ratio)$state, "unknown")

  # A measure the absolute conversion does not know.
  expect_equal(.pubias_trimfill_dot(
    te_original = -0.50, te_adjusted = -0.40, small_values = "desirable",
    sm = "wat", binary = TRUE, baseline_risk = 0.20,
    p1_from_ratio = p1_from_ratio)$state, "unknown")
})

# ---------------------------------------------------------------------------
# The dot rates nothing
# ---------------------------------------------------------------------------

test_that("no dot value can reach assess_pubias() or grade_meta()", {
  # Structural, because a runtime check can only prove that today's inputs do
  # not carry a dot. The three dot entry points are called from the Shiny app
  # and from these tests, and from nowhere else in the package - so no rating
  # path can read one however it is called.
  dot_fns <- c(".pubias_funnel_dot", ".pubias_trimfill_dot",
               ".pubias_missing_dot", ".pubias_missing_tipping")
  pkg_r <- test_path("..", "..", "R")
  skip_if_not(dir.exists(pkg_r), "package sources not laid out as expected")

  sources <- list.files(pkg_r, pattern = "[.]R$", full.names = TRUE)
  callers <- setdiff(basename(sources),
                     c("pubias_status.R", "pubias_missing.R"))
  offenders <- character(0)
  for (f in file.path(pkg_r, callers)) {
    txt <- readLines(f, warn = FALSE)
    # Comments are prose about the dots and are not a call.
    txt <- sub("#.*$", "", txt)
    for (fn in dot_fns) {
      if (any(grepl(paste0(fn, "("), txt, fixed = TRUE))) {
        offenders <- c(offenders, paste0(basename(f), " calls ", fn))
      }
    }
  }
  expect_equal(offenders, character(0))

  # And the dot vocabulary is not a judgment vocabulary: nothing that reaches
  # a GRADE judgment can be confused for one of these four states.
  expect_equal(intersect(PMA_PUBIAS_DOT_STATES,
                         c("not_serious", "serious", "very_serious")),
               character(0))
})

test_that("a rated object carries no trace of a dot", {
  skip_if_not_installed("meta")
  set.seed(20260818)
  k <- 12
  dat <- data.frame(
    studlab = paste0("S", seq_len(k)),
    event.e = rbinom(k, 100, 0.12), n.e = 100,
    event.c = rbinom(k, 100, 0.20), n.c = 100
  )
  ma <- suppressWarnings(meta::metabin(
    event.e, n.e, event.c, n.c, studlab = studlab, data = dat,
    sm = "OR", random = TRUE, prediction = TRUE))

  g <- suppressWarnings(grade_meta(ma, small_values = "desirable",
                                   threshold = 1.25,
                                   threshold_scale = "ratio"))
  flat <- paste(utils::capture.output(str(g, max.level = 4)), collapse = " ")
  expect_false(grepl("pubias_dot", flat, fixed = TRUE))
  expect_false(grepl("delta_star", flat, fixed = TRUE))
})
