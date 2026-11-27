# test-imprecision-ois-judgment.R - the v0.11.0 knobs on Fig 4's large-effect path
#
# Covers the three changes made to R/domain_imprecision.R in 0.11.0, each
# forwarded through grade_meta():
#   - large_effect / large_effect_rationale: the reviewer answers Fig 4's
#     "implausibly large?" node in place of the magnitude rule;
#   - ci_ratio_shortcut / ci_ratio_scale: the binary CI-ratio two-level rule
#     stands in for an OIS that could not be calculated, and an OR fit's ratio
#     is read from an RR refit;
#   - ois_rrr = "threshold": the binary OIS powered against the threshold.
#
# The older Fig 4 tests live in test-imprecision.R; a test belongs here when
# it exercises one of these arguments.

library(testthat)
library(meta)

skip_if_not_installed("meta")

# A large protective OR effect whose CI clears the null, with a CI ratio past
# 2.5 on the OR scale and a total N well short of the OIS.
large_or_meta <- function() {
  metabin(
    event.e = c(3, 4, 2),
    n.e     = c(50, 50, 50),
    event.c = c(12, 14, 10),
    n.c     = c(50, 50, 50),
    studlab = c("S1", "S2", "S3"),
    sm = "OR", method = "Inverse", random = TRUE, common = FALSE
  )
}

# A large OR effect with common events: the OR CI ratio passes 2.5 while the
# RR CI ratio of the same data stays under 3.
common_event_or_meta <- function() {
  metabin(
    event.e = c(12, 13),
    n.e     = c(50, 50),
    event.c = c(30, 30),
    n.c     = c(50, 50),
    studlab = c("S1", "S2"),
    sm = "OR", method = "Inverse", random = TRUE, common = FALSE
  )
}

# A moderate RR effect (RRR about 20%) whose CI clears the null, with a total
# N short of the OIS a 20% RRR needs.
moderate_rr_meta <- function() {
  metabin(
    event.e = c(48, 48, 48),
    n.e     = c(200, 200, 200),
    event.c = c(60, 60, 60),
    n.c     = c(200, 200, 200),
    studlab = c("S1", "S2", "S3"),
    sm = "RR", method = "Inverse", random = TRUE, common = FALSE
  )
}

impre_null <- function(m, ...) {
  suppressWarnings(assess_imprecision(
    m, outcome_type = "relative", threshold_type = "null",
    small_values = "desirable", ...))
}

fact_value <- function(row, key) {
  f <- attr(row, "facts")
  f$value[f$key == key]
}

fact_number <- function(row, key) {
  f <- attr(row, "facts")
  f$numeric[f$key == key]
}

# --------------------------------------------------------------------------
# large_effect
# --------------------------------------------------------------------------

test_that("large_effect = 'plausible' takes the moderate edge and quotes the rationale", {
  m <- large_or_meta()
  auto <- impre_null(m)
  expect_false(identical(auto$judgment, "not_serious"))

  why <- "Dose-response and a strong mechanism make this effect credible."
  row <- impre_null(m, large_effect = "plausible", large_effect_rationale = why)
  expect_identical(row$judgment, "not_serious")
  expect_match(row$notes, why, fixed = TRUE)
  expect_match(row$notes, "magnitude rule: effect implausibly large",
               fixed = TRUE)
  expect_match(row$notes, "reviewer judged the effect plausible", fixed = TRUE)
  expect_match(row$notes, "OIS not applied on this Fig 4 path", fixed = TRUE)
  expect_match(fact_value(row, "large_effect"), why, fixed = TRUE)
  flow <- strsplit(fact_value(row, "flow_path"), " ", fixed = TRUE)[[1]]
  expect_true("pma-impre-edge-large-no" %in% flow)
  expect_false("pma-impre-edge-large-yes" %in% flow)
})

test_that("a reviewer large_effect judgment needs a rationale", {
  m <- large_or_meta()
  expect_error(impre_null(m, large_effect = "plausible"),
               regexp = "large_effect_rationale")
  expect_error(impre_null(m, large_effect = "implausible",
                          large_effect_rationale = "  "),
               regexp = "large_effect_rationale")
  expect_error(
    suppressWarnings(grade_meta(m, threshold_type = "null",
                                small_values = "desirable",
                                large_effect = "plausible")),
    regexp = "large_effect_rationale")
})

test_that("large_effect and the CI-ratio enums reject typos", {
  m <- large_or_meta()
  expect_error(impre_null(m, large_effect = "plausable",
                          large_effect_rationale = "x"),
               regexp = "large_effect must be one of")
  expect_error(impre_null(m, ci_ratio_shortcut = "never"),
               regexp = "ci_ratio_shortcut must be one of")
  expect_error(impre_null(m, ci_ratio_scale = "or"),
               regexp = "ci_ratio_scale must be one of")
  expect_error(
    suppressWarnings(grade_meta(m, threshold_type = "null",
                                small_values = "desirable",
                                ci_ratio_scale = "log")),
    regexp = "ci_ratio_scale must be one of")
})

test_that("large_effect = 'implausible' forces the OIS approach for a moderate effect", {
  m <- moderate_rr_meta()
  auto <- impre_null(m)
  expect_identical(auto$judgment, "not_serious")
  expect_identical(fact_value(auto, "ois_used"), "no")

  row <- impre_null(m, large_effect = "implausible",
                    large_effect_rationale = "Implausible for this mechanism.")
  expect_identical(fact_value(row, "ois_used"), "yes")
  expect_identical(row$judgment, "serious")
  expect_match(row$notes, "magnitude rule: effect moderate", fixed = TRUE)
  flow <- strsplit(fact_value(row, "flow_path"), " ", fixed = TRUE)[[1]]
  expect_true("pma-impre-edge-large-yes" %in% flow)
})

test_that("grade_meta() forwards large_effect and records the settings", {
  m <- large_or_meta()
  why <- "Credible effect."
  g <- suppressWarnings(grade_meta(m, threshold_type = "null",
                                   small_values = "desirable",
                                   large_effect = "plausible",
                                   large_effect_rationale = why))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_identical(row$judgment, "not_serious")
  expect_match(row$notes, why, fixed = TRUE)
  expect_identical(g$imprecision_settings$large_effect, "plausible")
  expect_identical(g$imprecision_settings$ci_ratio_shortcut,
                   "when_ois_unavailable")
  expect_identical(g$imprecision_settings$ci_ratio_scale, "rr")
})

# --------------------------------------------------------------------------
# ci_ratio_shortcut / ci_ratio_scale
# --------------------------------------------------------------------------

test_that("a calculated OIS decides; the CI-ratio shortcut is not used", {
  m <- large_or_meta()
  # Precondition: large effect, CI clear of the null, OR CI ratio >= 2.5.
  expect_lt(m$upper.random, 0)
  expect_gte(exp(m$upper.random - m$lower.random), 2.5)

  row <- impre_null(m)
  expect_identical(row$judgment, "serious")
  expect_match(row$notes, "N < OIS -> rate down one level", fixed = TRUE)
  expect_match(row$notes, "CI-ratio shortcut not used", fixed = TRUE)
  expect_match(row$notes, "for information, CI ratio", fixed = TRUE)
  flow <- strsplit(fact_value(row, "flow_path"), " ", fixed = TRUE)[[1]]
  expect_true("pma-impre-leaf-down1-ois" %in% flow)

  old <- impre_null(m, ci_ratio_shortcut = "always", ci_ratio_scale = "fitted")
  expect_identical(old$judgment, "very_serious")
  expect_match(old$notes, ">= 2.5 -> consider rating down two levels",
               fixed = TRUE)

  g <- suppressWarnings(grade_meta(m, threshold_type = "null",
                                   small_values = "desirable"))
  expect_identical(
    g$domain_assessments$judgment[g$domain_assessments$domain == "Imprecision"],
    "serious")
})

test_that("rule_set = 'core_grade_bmj_2025' keeps the pre-0.11 CI-ratio behaviour", {
  m <- large_or_meta()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null",
                                   small_values = "desirable",
                                   rule_set = "core_grade_bmj_2025"))
  expect_identical(
    g$domain_assessments$judgment[g$domain_assessments$domain == "Imprecision"],
    "very_serious")
})

test_that("without an OIS, an OR fit's CI ratio is read from an RR refit against 3", {
  m <- common_event_or_meta()
  or_ratio <- exp(m$upper.random - m$lower.random)
  rr_fit   <- update(m, sm = "RR")
  rr_ratio <- exp(rr_fit$upper.random - rr_fit$lower.random)
  expect_gte(or_ratio, 2.5)
  expect_lt(rr_ratio, 3)
  expect_lt(m$upper.random, 0)

  # Make the OIS uncomputable so that the shortcut is what decides.
  local_mocked_bindings(
    .compute_ois_pct = function(meta_obj, ois_events, ois_n) {
      list(pct = NA_real_, observed = NA_integer_, target = NA_integer_,
           unit = NA_character_)
    })

  row <- impre_null(m)
  expect_false(identical(row$judgment, "very_serious"))
  expect_match(row$notes, "RR refit of the same analysis", fixed = TRUE)

  fitted <- impre_null(m, ci_ratio_scale = "fitted")
  expect_identical(fitted$judgment, "very_serious")
  expect_match(fitted$notes, "CI-ratio shortcut used because the OIS could not",
               fixed = TRUE)
})

test_that("an OR fit without arm data falls back to the fitted scale and says so", {
  src <- common_event_or_meta()
  m <- metagen(TE = src$TE, seTE = src$seTE, studlab = src$studlab,
               sm = "OR", random = TRUE, common = FALSE)
  row <- impre_null(m, ois_n = 10000)
  info <- pmatools:::.ci_ratio_on_scale(m, m$lower.random, m$upper.random,
                                        "OR", "rr")
  expect_identical(info$sm, "OR")
  expect_equal(info$cut, 2.5)
  expect_match(info$note, "no arm-level event counts", fixed = TRUE)
  expect_s3_class(row, "tbl_df")
})

# --------------------------------------------------------------------------
# ois_rrr = "threshold"
# --------------------------------------------------------------------------

test_that("ois_rrr = 'threshold' powers the OIS against a ratio threshold", {
  m <- moderate_rr_meta()
  # Increase direction: small_values = "undesirable" makes the events desirable.
  row <- suppressWarnings(assess_imprecision(
    m, outcome_type = "relative", small_values = "undesirable",
    threshold_internal = log(1.25), ois_p0 = 0.20, ois_rrr = "threshold"))
  expect_equal(fact_number(row, "ois_target_rate"), 0.20 * 1.25,
               tolerance = 1e-10)
  expect_match(row$notes, "ois_p1 from the imprecision threshold", fixed = TRUE)
  expect_match(fact_value(row, "ois_target_rate"),
               "derived from the imprecision threshold", fixed = TRUE)

  # Reduction direction divides instead.
  red <- suppressWarnings(assess_imprecision(
    m, outcome_type = "relative", small_values = "desirable",
    threshold_internal = log(1.25), ois_p0 = 0.20, ois_rrr = "threshold"))
  expect_equal(fact_number(red, "ois_target_rate"), 0.20 / 1.25,
               tolerance = 1e-10)

  # An OR fit moves the odds.
  m_or <- large_or_meta()
  row_or <- suppressWarnings(assess_imprecision(
    m_or, outcome_type = "relative", small_values = "undesirable",
    threshold_internal = log(1.25), ois_p0 = 0.20, ois_rrr = "threshold"))
  odds1 <- (0.20 / 0.80) * 1.25
  expect_equal(fact_number(row_or, "ois_target_rate"), odds1 / (1 + odds1),
               tolerance = 1e-10)
})

test_that("ois_rrr = 'threshold' uses an ARD threshold as an ARD", {
  m <- moderate_rr_meta()
  g <- suppressWarnings(grade_meta(
    m, threshold = 0.05, threshold_scale = "ard", threshold_baseline = 0.30,
    small_values = "desirable", ois_rrr = "threshold"))
  f <- domain_facts(g, "Imprecision")
  expect_equal(f$numeric[f$key == "ois_target_rate"], 0.30 - 0.05,
               tolerance = 1e-10)
})

test_that("ois_rrr = 'threshold' without a threshold aborts", {
  m <- moderate_rr_meta()
  expect_error(
    suppressWarnings(assess_imprecision(
      m, outcome_type = "relative", small_values = "desirable",
      ois_rrr = "threshold")),
    regexp = "needs a threshold|has none")
  expect_error(
    suppressWarnings(assess_imprecision(
      m, outcome_type = "relative", small_values = "desirable",
      ois_rrr = "thresh")),
    regexp = "ois_rrr")
})

test_that("an explicit ois_p1 still wins over ois_rrr = 'threshold'", {
  m <- moderate_rr_meta()
  row <- suppressWarnings(assess_imprecision(
    m, outcome_type = "relative", small_values = "undesirable",
    threshold_internal = log(1.25), ois_p0 = 0.20, ois_p1 = 0.33,
    ois_rrr = "threshold"))
  expect_match(row$notes, "p1=0.330", fixed = TRUE)
  expect_false(grepl("ois_p1 from the imprecision threshold", row$notes,
                     fixed = TRUE))
})

test_that("ois_rrr = 'threshold' aborts when p1 leaves (0, 1)", {
  m <- moderate_rr_meta()
  expect_error(
    suppressWarnings(assess_imprecision(
      m, outcome_type = "relative", small_values = "undesirable",
      threshold_internal = log(2), ois_p0 = 0.60, ois_rrr = "threshold")),
    regexp = "not a probability")
})
