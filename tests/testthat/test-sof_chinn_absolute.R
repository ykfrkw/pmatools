# test-sof_chinn_absolute.R - v0.6:
# The Summary of Findings row of a continuous outcome presented through Chinn's
# formula. Three things moved: the rates the conversion computes are extracted
# once (.chinn_rates()) instead of being recomputed per cell, the Difference
# column reports the absolute risk difference those rates imply rather than the
# pooled estimate in standard deviation units, and the Effect column gains the
# risk ratio between them. The fourth is new: keep_effect_scale = TRUE shows the
# outcome on its own scale AND as responder proportions in one row.

library(testthat)
library(meta)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

quiet_grade <- function(...) suppressWarnings(grade_meta(...))

.footer_text <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")
.body_col    <- function(ft, i) as.character(ft$body$dataset[[i]])

CHINN_P0 <- 0.30

# Intervention arm scores HIGHER, so the pooled SMD is positive: Chinn's formula
# then puts the odds ratio above 1 with invert = FALSE, and below 1 with
# invert = TRUE. The reversed fixture below is the mirror image, and the two
# together are what pins the direction words.
mk_meta_up <- function(sm = "SMD") {
  meta::metacont(
    n.e = c(50, 60, 70), mean.e = c(20, 22, 21), sd.e = c(10, 11, 12),
    n.c = c(50, 60, 70), mean.c = c(8, 9, 8),    sd.c = c(10, 11, 12),
    studlab = c("A", "B", "C"), sm = sm
  )
}

# Intervention arm scores LOWER (a symptom scale), the case chinn_invert exists
# for: a negative SMD is the good direction, and inverting it has to leave the
# intervention with MORE responders, not fewer.
mk_meta_down <- function(sm = "SMD") {
  meta::metacont(
    n.e = c(50, 60, 70), mean.e = c(8, 9, 8),     sd.e = c(10, 11, 12),
    n.c = c(50, 60, 70), mean.c = c(20, 22, 21),  sd.c = c(10, 11, 12),
    studlab = c("A", "B", "C"), sm = sm
  )
}

mk_grade <- function(m, name = "Depression") {
  quiet_grade(m, study_design = "RCT", rob = "no", small_values = "desirable",
              rob_rationale = "Consensus RoB2: all domains low risk",
              indirectness = "no", outcome_name = name,
              outcome_type = "absolute", threshold_type = "null")
}

# --------------------------------------------------------------------------
# .chinn_rates(): the numbers every other cell is now built from
# --------------------------------------------------------------------------

test_that(".chinn_rates() reproduces the formula by hand", {
  m      <- mk_meta_up()
  pooled <- pmatools:::.pooled_estimate(m)
  rates  <- pmatools:::.chinn_rates(m, CHINN_P0, invert = FALSE)

  or_of <- function(smd) exp(smd * pi / sqrt(3))
  p1_of <- function(or) CHINN_P0 * or / (1 + CHINN_P0 * (or - 1))

  expect_equal(rates$or,       or_of(pooled$est))
  expect_equal(rates$or_lower, or_of(pooled$lower))
  expect_equal(rates$or_upper, or_of(pooled$upper))
  expect_equal(rates$p1,    p1_of(or_of(pooled$est)))
  expect_equal(rates$p1_lo, p1_of(or_of(pooled$lower)))
  expect_equal(rates$p1_hi, p1_of(or_of(pooled$upper)))
  # The pooled SMD is positive here, so the conversion lifts the proportion.
  expect_gt(rates$p1, CHINN_P0)
  expect_lt(rates$p1_lo, rates$p1_hi)
})

test_that(".chinn_rates(invert = TRUE) flips the sign and swaps the bounds", {
  m      <- mk_meta_up()
  pooled <- pmatools:::.pooled_estimate(m)
  rates  <- pmatools:::.chinn_rates(m, CHINN_P0, invert = TRUE)

  or_of <- function(smd) exp(smd * pi / sqrt(3))
  p1_of <- function(or) CHINN_P0 * or / (1 + CHINN_P0 * (or - 1))

  expect_equal(rates$or,       or_of(-pooled$est))
  # The lower bound of the inverted effect is the negated UPPER bound of the
  # original: without the swap the interval would be reported backwards.
  expect_equal(rates$or_lower, or_of(-pooled$upper))
  expect_equal(rates$or_upper, or_of(-pooled$lower))
  expect_equal(rates$p1, p1_of(or_of(-pooled$est)))
  expect_lt(rates$p1, CHINN_P0)
  expect_lt(rates$p1_lo, rates$p1_hi)
})

test_that(".chinn_rates() returns NULL when an ingredient is missing", {
  m <- mk_meta_up()
  expect_null(pmatools:::.chinn_rates(m, NULL))

  m_na <- m
  m_na$TE.random <- NA_real_
  m_na$TE.common <- NA_real_
  expect_null(pmatools:::.chinn_rates(m_na, CHINN_P0))
})

test_that(".format_ier_chinn() is unchanged by the extraction", {
  m <- mk_meta_up()
  # Byte-for-byte, in both number formats: the extraction moved the arithmetic
  # out of this function and must not have moved a digit with it.
  expect_identical(
    pmatools:::.format_ier_chinn(m, CHINN_P0, 1000, invert = FALSE,
                                 big_mark = FALSE, ci_sep = " to "),
    "772 per 1000\n(693 to 836)")
  expect_identical(
    pmatools:::.format_ier_chinn(m, CHINN_P0, 1000, invert = TRUE,
                                 big_mark = TRUE, ci_sep = "; "),
    "51 per 1,000\n(35; 75)")
  expect_identical(pmatools:::.format_ier_chinn(m, NULL), "-")
})

# --------------------------------------------------------------------------
# The Difference column
# --------------------------------------------------------------------------

test_that("the Difference cell is a per-1000 risk difference, not SD units", {
  ft <- sof_table(mk_grade(mk_meta_up()), style = "bmj",
                  convert_smd_to_or = TRUE, baseline_risk = CHINN_P0)
  diff <- .body_col(ft, 6)

  expect_identical(diff, "472 more per 1000 (393 more to 536 more)")
  expect_no_match(diff, "standard deviations", fixed = TRUE)

  # It is exactly what the two arm cells differ by, which is the whole point of
  # putting it in a column headed "Absolute effects".
  rates <- pmatools:::.chinn_rates(mk_meta_up(), CHINN_P0)
  expect_identical(round((rates$p1 - CHINN_P0) * 1000), 472)
})

test_that("the direction word follows the responders, not the sign of the SMD", {
  # A symptom scale: the pooled SMD is negative and the intervention is better,
  # so inverting it must leave the intervention with MORE responders.
  ft_inv <- sof_table(mk_grade(mk_meta_down()), style = "bmj",
                      convert_smd_to_or = TRUE, baseline_risk = CHINN_P0,
                      chinn_invert = TRUE)
  expect_match(.body_col(ft_inv, 6), "^472 more per 1000 \\(")

  # The same object read without the inversion runs the other way.
  ft_raw <- sof_table(mk_grade(mk_meta_down()), style = "bmj",
                      convert_smd_to_or = TRUE, baseline_risk = CHINN_P0,
                      chinn_invert = FALSE)
  expect_match(.body_col(ft_raw, 6), "^249 fewer per 1000 \\(")
  # Both CI bounds carry their own direction word and are ordered low to high.
  expect_match(.body_col(ft_raw, 6), "\\(265 fewer to 225 fewer\\)$")
})

test_that("a converted row in a multi-outcome table gets the same Difference", {
  g_conv <- mk_grade(mk_meta_up(), "Depression")
  attr(g_conv, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, baseline_risk = CHINN_P0, chinn_invert = FALSE)
  ft <- grade_table(list("Depression" = g_conv), style = "bmj")

  solo <- sof_table(mk_grade(mk_meta_up(), "Depression"), style = "bmj",
                    convert_smd_to_or = TRUE, baseline_risk = CHINN_P0)
  expect_identical(.body_col(ft, 6)[1], .body_col(solo, 6))
})

# --------------------------------------------------------------------------
# The Effect column
# --------------------------------------------------------------------------

test_that("the Effect cell carries the derived risk ratio under the estimate", {
  ft  <- sof_table(mk_grade(mk_meta_up()), style = "bmj",
                   convert_smd_to_or = TRUE, baseline_risk = CHINN_P0)
  eff <- .body_col(ft, 3)

  lines <- strsplit(eff, "\n", fixed = TRUE)[[1]]
  expect_length(lines, 2L)
  expect_match(lines[1], "^Standardised mean difference ")

  rates <- pmatools:::.chinn_rates(mk_meta_up(), CHINN_P0)
  expect_identical(
    lines[2],
    sprintf("Derived risk ratio %.2f (%.2f to %.2f)",
            rates$p1 / CHINN_P0, rates$p1_lo / CHINN_P0,
            rates$p1_hi / CHINN_P0))
})

test_that("the derived risk ratio is absent when nothing was converted", {
  ft <- sof_table(mk_grade(mk_meta_up()), style = "bmj")
  expect_no_match(.body_col(ft, 3), "Derived risk ratio", fixed = TRUE)
})

test_that("the footnote names the assumed proportion and calls it derived", {
  txt <- .footer_text(sof_table(mk_grade(mk_meta_up()), style = "bmj",
                                convert_smd_to_or = TRUE,
                                baseline_risk = CHINN_P0))
  expect_match(txt, "assumed control responder proportion of 0.3", fixed = TRUE)
  expect_match(txt, "Both are DERIVED from the pooled estimate", fixed = TRUE)
  expect_match(txt, "move with the assumed control proportion", fixed = TRUE)
})

# --------------------------------------------------------------------------
# keep_effect_scale: both presentations in one row
# --------------------------------------------------------------------------

both_table <- function(m = mk_meta_up(), ...) {
  sof_table(mk_grade(m), style = "bmj", convert_smd_to_or = TRUE,
            keep_effect_scale = TRUE, baseline_risk = CHINN_P0,
            unit = "points", ...)
}

test_that("both mode puts two presentations in one row, not two rows", {
  ft <- both_table()
  expect_equal(nrow(ft$body$dataset), 1L)
  # Seven or eight columns, exactly as any other BMJ row: no column was added.
  expect_equal(ncol(ft$body$dataset), ncol(sof_table(
    mk_grade(mk_meta_up()), style = "bmj")$body$dataset))
})

test_that("both mode gives each arm cell the mean scale then the rate", {
  ft  <- both_table()
  cer <- strsplit(.body_col(ft, 4), "\n", fixed = TRUE)[[1]]
  ier <- strsplit(.body_col(ft, 5), "\n", fixed = TRUE)[[1]]

  # Control: the pooled control mean, then the assumed responder proportion.
  expect_length(cer, 2L)
  expect_match(cer[1], " points$")
  expect_identical(cer[2], "300 per 1000 *")

  # Intervention: mean and its interval, then rate and its interval.
  expect_match(ier[1], " points$")
  expect_match(ier[2], "^\\([-0-9.]+ to [-0-9.]+\\)$")
  expect_identical(ier[3], "772 per 1000")
  expect_identical(ier[4], "(693 to 836) *")
})

test_that("both mode gives the Difference cell both scales, own first", {
  lines <- strsplit(.body_col(both_table(), 6), "\n", fixed = TRUE)[[1]]
  expect_length(lines, 2L)
  expect_match(lines[1], "standard deviations", fixed = TRUE)
  expect_identical(lines[2], "472 more per 1000 (393 more to 536 more)")
})

test_that("both mode explains both derivations in the footer", {
  txt <- .footer_text(both_table())
  # The mean-scale half.
  expect_match(txt, "inverse-variance weighted mean of the control arms",
               fixed = TRUE)
  expect_match(txt, "pooled within-arm standard deviation of the control arms",
               fixed = TRUE)
  # The responder half, and the sentence saying the derived numbers are derived.
  expect_match(txt, "dichotomised via Chinn's formula", fixed = TRUE)
  expect_match(txt, "Both are DERIVED from the pooled estimate", fixed = TRUE)
  # The rate sentence stays: it is the only thing explaining the second line of
  # each arm cell.
  expect_match(txt, "the intervention-arm rate and the difference are computed",
               fixed = TRUE)
})

test_that("both mode is the default-off addition it is meant to be", {
  responder_only <- sof_table(mk_grade(mk_meta_up()), style = "bmj",
                              convert_smd_to_or = TRUE,
                              baseline_risk = CHINN_P0, unit = "points")
  expect_no_match(.body_col(responder_only, 4), "\n", fixed = TRUE)
  expect_no_match(.body_col(responder_only, 6), "standard deviations",
                  fixed = TRUE)
})

# --------------------------------------------------------------------------
# Degradation: both was asked for and only one half is computable
# --------------------------------------------------------------------------

# An SMD whose control arms carry no usable within-arm SD cannot be put back on
# the outcome's own scale, so .format_arm_values_cont() refuses. The responder
# half is unaffected, and that is what the row must fall back to.
mk_meta_no_sd <- function() {
  m <- mk_meta_up()
  m$sd.c <- rep(NA_real_, length(m$sd.c))
  m
}

test_that("both mode degrades to responder-only rather than erroring", {
  ft <- expect_no_error(both_table(mk_meta_no_sd()))

  # Arm cells: the responder pair alone, exactly as responder-only mode.
  expect_identical(.body_col(ft, 4), "300 per 1000 *")
  expect_identical(.body_col(ft, 5), "772 per 1000\n(693 to 836) *")
  expect_true(nzchar(.body_col(ft, 4)))

  # And the Difference cell degrades with them, so half the row does not show
  # one presentation and half the other.
  expect_identical(.body_col(ft, 6), "472 more per 1000 (393 more to 536 more)")
})

test_that("the degradation names its reason in a footnote", {
  txt <- .footer_text(both_table(mk_meta_no_sd()))
  expect_match(txt, "the arm columns show the responder proportions only",
               fixed = TRUE)
  expect_match(txt, "no usable within-arm standard deviation", fixed = TRUE)
})

# --------------------------------------------------------------------------
# A combined table holding rows presented differently
# --------------------------------------------------------------------------

test_that("grade_table() renders a both row beside an effect row", {
  g_both <- mk_grade(mk_meta_up(), "Depression")
  attr(g_both, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, keep_effect_scale = TRUE,
    baseline_risk = CHINN_P0, chinn_invert = FALSE,
    threshold_label = ">=50% drop in PHQ-9")
  g_plain <- mk_grade(mk_meta_up(), "Anxiety")

  ft <- expect_no_error(grade_table(
    list("Depression" = g_both, "Anxiety" = g_plain),
    style = "bmj", unit = c("Depression" = "points")))

  # Row 1 carries both scales; row 2 carries neither conversion nor its wording.
  expect_match(.body_col(ft, 5)[1], "points", fixed = TRUE)
  expect_match(.body_col(ft, 5)[1], "772 per 1000", fixed = TRUE)
  expect_match(.body_col(ft, 3)[1], "Derived risk ratio", fixed = TRUE)
  expect_match(.body_col(ft, 6)[1], "472 more per 1000", fixed = TRUE)

  expect_no_match(.body_col(ft, 5)[2], "per 1000", fixed = TRUE)
  expect_no_match(.body_col(ft, 3)[2], "Derived risk ratio", fixed = TRUE)
  expect_no_match(.body_col(ft, 6)[2], "per 1000", fixed = TRUE)

  # The converted row's own footnote states the proportion it was converted
  # against and that its arm cells hold two scales.
  txt <- .footer_text(ft)
  expect_match(txt, "[Depression] Responder presentation:", fixed = TRUE)
  expect_match(txt, "Assumed control responder proportion: 0.3.", fixed = TRUE)
  expect_match(txt, "on its own scale on the first line", fixed = TRUE)
})

test_that("a both row and a responder-only row coexist without corrupting", {
  g_both <- mk_grade(mk_meta_up(), "Depression")
  attr(g_both, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, keep_effect_scale = TRUE,
    baseline_risk = CHINN_P0, chinn_invert = FALSE)
  g_resp <- mk_grade(mk_meta_up(), "Anxiety")
  attr(g_resp, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, baseline_risk = CHINN_P0, chinn_invert = FALSE)

  ft <- grade_table(list("Depression" = g_both, "Anxiety" = g_resp),
                    style = "bmj", unit = c("Depression" = "points"))

  expect_match(.body_col(ft, 6)[1], "standard deviations", fixed = TRUE)
  expect_no_match(.body_col(ft, 6)[2], "standard deviations", fixed = TRUE)
  # Both rows still report the same responder rates.
  expect_match(.body_col(ft, 5)[1], "772 per 1000", fixed = TRUE)
  expect_match(.body_col(ft, 5)[2], "772 per 1000", fixed = TRUE)
})

test_that("keep_effect_scale is a legal pmatools_display name", {
  g <- mk_grade(mk_meta_up(), "Depression")
  attr(g, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, keep_effect_scale = TRUE, baseline_risk = CHINN_P0)
  expect_no_error(pmatools:::.check_outcome_display(g, "Depression"))
  expect_true("keep_effect_scale" %in% pmatools:::PMATOOLS_RESPONDER_FIELDS)
})
