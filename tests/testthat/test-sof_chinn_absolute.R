# test-sof_chinn_absolute.R - v0.6:
# The Summary of Findings row of a continuous outcome presented through Chinn's
# formula. The rates the conversion computes are extracted once
# (.chinn_rates()) instead of being recomputed per cell, the Difference column
# reports the absolute risk difference those rates imply, and the Effect column
# reports the ODDS ratio the formula itself emits -- not the risk ratio, which
# exists only once an assumed control proportion is laid on top of it. The
# fourth block is the two-row presentation keep_effect_scale = TRUE produces:
# the effect on its own scale above, the dichotomised reading below.

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
# The Effect column: an ODDS ratio, not a risk ratio
# --------------------------------------------------------------------------

test_that("the derived line is the odds ratio Chinn's formula emits", {
  for (invert in c(FALSE, TRUE)) {
    ft <- sof_table(mk_grade(mk_meta_up()), style = "bmj",
                    convert_smd_to_or = TRUE, baseline_risk = CHINN_P0,
                    chinn_invert = invert)
    eff   <- .body_col(ft, 3)
    lines <- strsplit(eff, "\n", fixed = TRUE)[[1]]

    # The derived odds ratio rides UNDER the pooled estimate. It must not
    # replace it: the column header is built from meta_obj$sm, so a cell
    # holding only an odds ratio would sit under "Standardized mean
    # difference", and the estimate every domain was rated on would be absent
    # from the table that reports the certainty.
    expect_length(lines, 2L)
    expect_match(lines[1], "mean difference", fixed = TRUE)
    expect_match(lines[2], "^Derived odds ratio ")
    expect_no_match(eff, "risk ratio", fixed = TRUE)

    # exp(SMD * pi / sqrt(3)), to the precision the cell prints.
    smd <- pmatools:::.pooled_estimate(mk_meta_up())$est
    or  <- exp((if (invert) -smd else smd) * pi / sqrt(3))
    expect_identical(
      sub("^Derived odds ratio ([0-9.]+) .*$", "\\1", lines[2]),
      sprintf("%.2f", or))

    # And it is NOT the risk ratio, which is what the cell used to print.
    rates <- pmatools:::.chinn_rates(mk_meta_up(), CHINN_P0, invert = invert)
    expect_false(identical(sprintf("%.2f", or),
                           sprintf("%.2f", rates$p1 / CHINN_P0)))
  }
})

test_that("the odds ratio does not move when the assumed proportion does", {
  or_of <- function(p0) {
    ft <- sof_table(mk_grade(mk_meta_up()), style = "bmj",
                    convert_smd_to_or = TRUE, baseline_risk = p0)
    sub("^Derived odds ratio ([0-9.]+) .*$", "\\1", .body_col(ft, 3))
  }
  expect_identical(or_of(0.30), or_of(0.10))
})

test_that("the derived odds ratio is absent when nothing was converted", {
  ft <- sof_table(mk_grade(mk_meta_up()), style = "bmj")
  expect_no_match(.body_col(ft, 3), "Derived", fixed = TRUE)
  expect_match(.body_col(ft, 3), "^Standardised mean difference ")
})

test_that("the footnote separates what depends on the proportion from what does not", {
  txt <- .footer_text(sof_table(mk_grade(mk_meta_up()), style = "bmj",
                                convert_smd_to_or = TRUE,
                                baseline_risk = CHINN_P0))
  expect_match(txt, paste0("derived odds ratio comes from the formula above ",
                           "alone and does NOT depend on the assumed control ",
                           "responder proportion of 0.3"), fixed = TRUE)
  expect_match(txt, paste0("arm rates and the Difference column's absolute ",
                           "risk difference DO depend on it"), fixed = TRUE)
})

# --------------------------------------------------------------------------
# The "effect" presentation: nothing in the absolute-effect columns
# --------------------------------------------------------------------------

test_that("an unconverted continuous row empties the three absolute cells", {
  ft <- sof_table(mk_grade(mk_meta_up()), style = "bmj", unit = "points")
  expect_identical(.body_col(ft, 4), "")
  expect_identical(.body_col(ft, 5), "")
  # An SMD's difference restates the Effect column, so it goes too.
  expect_identical(.body_col(ft, 6), "")
})

test_that("an MD keeps its Difference where an SMD loses it", {
  ft <- sof_table(mk_grade(mk_meta_up("MD")), style = "bmj", unit = "points")
  expect_identical(.body_col(ft, 4), "")
  expect_identical(.body_col(ft, 5), "")
  # The pooled contrast is not built from arm means and survives the deletion.
  expect_match(.body_col(ft, 6), "^[0-9.]+ more points \\(")
})

# --------------------------------------------------------------------------
# keep_effect_scale: one outcome, two rows
# --------------------------------------------------------------------------

both_table <- function(m = mk_meta_up(), ...) {
  sof_table(mk_grade(m), style = "bmj", convert_smd_to_or = TRUE,
            keep_effect_scale = TRUE, baseline_risk = CHINN_P0,
            unit = "points", ...)
}

test_that("both mode renders two body rows and adds no column", {
  ft <- both_table()
  expect_equal(nrow(ft$body$dataset), 2L)
  expect_equal(ncol(ft$body$dataset), ncol(sof_table(
    mk_grade(mk_meta_up()), style = "bmj")$body$dataset))
})

test_that("both mode puts the effect above and the dichotomised reading below", {
  ft <- both_table()

  # Upper row: the effect on its own scale, absolute-effect cells empty.
  expect_match(.body_col(ft, 3)[1], "^Standardised mean difference ")
  expect_identical(.body_col(ft, 4)[1], "")
  expect_identical(.body_col(ft, 5)[1], "")
  expect_identical(.body_col(ft, 6)[1], "")

  # Lower row: the odds ratio, the two rates and the risk difference.
  expect_match(.body_col(ft, 3)[2], "^Derived odds ratio ")
  expect_identical(.body_col(ft, 4)[2], "300 per 1000 *")
  expect_identical(.body_col(ft, 5)[2], "772 per 1000\n(693 to 836) *")
  expect_identical(.body_col(ft, 6)[2],
                   "472 more per 1000 (393 more to 536 more)")
})

test_that("both mode merges the four columns that do not split", {
  ft <- both_table()
  # $columns is the VERTICAL span: 2 on the cell that renders, 0 on the one it
  # swallows. ($rows is the horizontal span the group-label rows use.)
  spans <- ft$body$spans$columns

  # Outcome, participants, certainty and plain language span both rows; the
  # four in between are their own cells.
  for (j in c(1L, 2L, 7L, 8L)) {
    expect_equal(spans[1L, j], 2, info = paste("merged column", j))
    expect_equal(spans[2L, j], 0, info = paste("merged column", j))
  }
  for (j in 3:6) {
    expect_equal(spans[1L, j], 1, info = paste("split column", j))
    expect_equal(spans[2L, j], 1, info = paste("split column", j))
  }

  # The lower row carries no outcome name of its own, so nothing repeats.
  expect_identical(.body_col(ft, 1)[2], "")
})

test_that("both mode rules between the two rows in the table's own weight", {
  ft <- both_table()
  weight <- flextable::get_flextable_defaults()$border.width
  widths <- unname(ft$body$styles$cells$border.width.bottom$data[1L, ])
  # The four splitting columns get the body rule; the merged four keep the
  # heavier outer border, so no rule cuts through a cell being merged.
  expect_equal(widths[3:6], rep(weight, 4L))
  expect_true(all(widths[c(1L, 2L, 7L, 8L)] > weight))
})

test_that("both mode keeps an MD difference on the upper row", {
  ft <- both_table(mk_meta_up("MD"))
  expect_match(.body_col(ft, 6)[1], "^[0-9.]+ more points \\(")
  expect_match(.body_col(ft, 6)[2], "^[0-9]+ more per 1000 \\(")
})

test_that("both mode explains itself in the footer", {
  txt <- .footer_text(both_table())
  expect_match(txt, "dichotomised via Chinn's formula", fixed = TRUE)
  expect_match(txt, "does NOT depend on the assumed control", fixed = TRUE)
  # Nothing mean-derived is claimed any more.
  expect_no_match(txt, "inverse-variance weighted mean", fixed = TRUE)
  expect_no_match(txt, "pooled within-arm standard deviation", fixed = TRUE)
})

test_that("both mode is the default-off addition it is meant to be", {
  responder_only <- sof_table(mk_grade(mk_meta_up()), style = "bmj",
                              convert_smd_to_or = TRUE,
                              baseline_risk = CHINN_P0, unit = "points")
  expect_equal(nrow(responder_only$body$dataset), 1L)
  expect_identical(.body_col(responder_only, 4), "300 per 1000 *")
})

test_that("both mode splits the gradepro layout too", {
  ft <- sof_table(mk_grade(mk_meta_up()), convert_smd_to_or = TRUE,
                  keep_effect_scale = TRUE, baseline_risk = CHINN_P0)
  expect_equal(nrow(ft$body$dataset), 2L)
  expect_match(.body_col(ft, 5)[1], "^SMD ")
  expect_match(.body_col(ft, 5)[2], "^Derived odds ratio ")
  expect_identical(.body_col(ft, 3)[1], "")
  expect_identical(.body_col(ft, 3)[2], "300 per 1,000 *")
})

# --------------------------------------------------------------------------
# A combined table holding rows presented differently
# --------------------------------------------------------------------------

# One of each presentation, in this order: a two-row "both", a single-row
# "responder" and a single-row "effect". Everything grade_table() indexes by
# row has to survive the pair in the middle of the table.
mk_mixed <- function() {
  g_both <- mk_grade(mk_meta_up(), "Depression")
  attr(g_both, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, keep_effect_scale = TRUE,
    baseline_risk = CHINN_P0, chinn_invert = FALSE,
    threshold_label = ">=50% drop in PHQ-9")
  g_resp <- mk_grade(mk_meta_up(), "Anxiety")
  attr(g_resp, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, baseline_risk = CHINN_P0, chinn_invert = FALSE)
  g_eff <- mk_grade(mk_meta_down(), "Insomnia")
  list("Depression" = g_both, "Anxiety" = g_resp, "Insomnia" = g_eff)
}

test_that("a both row, a responder row and an effect row share one table", {
  for (style in c("bmj", "gradepro")) {
    ft  <- grade_table(mk_mixed(), style = style)
    eff <- if (identical(style, "bmj")) 3L else 5L
    cer <- if (identical(style, "bmj")) 4L else 3L

    # Four body rows: the both outcome takes two, the other two take one each.
    expect_equal(nrow(ft$body$dataset), 4L, info = style)

    # The "both" pair splits across rows 1 and 2; the single-row "responder"
    # on row 3 carries the same two lines inside one cell.
    expect_match(.body_col(ft, eff)[1], "mean difference|^SMD ")
    expect_no_match(.body_col(ft, eff)[1], "Derived", fixed = TRUE)
    expect_match(.body_col(ft, eff)[2], "^Derived odds ratio ")
    expect_match(.body_col(ft, eff)[3], "mean difference|^SMD ")
    expect_match(.body_col(ft, eff)[3], "\nDerived odds ratio ")
    expect_no_match(.body_col(ft, eff)[4], "Derived", fixed = TRUE)

    expect_identical(.body_col(ft, cer)[1], "")
    expect_match(.body_col(ft, cer)[2], "^300 per 1,?000 \\*$")
    expect_match(.body_col(ft, cer)[3], "^300 per 1,?000 \\*$")
    expect_identical(.body_col(ft, cer)[4], "")
  }
})

test_that("every certainty cell is coloured on the right row of the mixed table", {
  outcomes <- mk_mixed()
  for (style in c("bmj", "gradepro")) {
    ft   <- grade_table(outcomes, style = style)
    cert <- if (identical(style, "bmj")) 7L else 6L
    bg   <- ft$body$styles$cells$background.color$data[, cert]
    pal  <- CERTAINTY_PALETTES[["pastel"]]

    # Rows 1-2 are one outcome, so both halves of the merged cell are painted;
    # rows 3 and 4 are the other two outcomes.
    expected <- c(pal[[outcomes$Depression$certainty]]$bg,
                  pal[[outcomes$Depression$certainty]]$bg,
                  pal[[outcomes$Anxiety$certainty]]$bg,
                  pal[[outcomes$Insomnia$certainty]]$bg)
    expect_identical(unname(bg), expected, info = style)

    # And the certainty text is on the upper row of the pair, not below it.
    expect_match(.body_col(ft, cert)[1], paste0("^",
                                                outcomes$Depression$certainty))
    expect_identical(.body_col(ft, cert)[2], "")
  }
})

test_that("the footnote markers still point at the right outcomes", {
  outcomes <- mk_mixed()
  # A risk-of-bias analysis-set note on the SECOND outcome only: its [n] marker
  # must land on that outcome's name, which sits below a two-row outcome.
  outcomes$Anxiety$rob_analysis_set <- "low_only"

  ft  <- grade_table(outcomes, style = "bmj")
  txt <- .footer_text(ft)

  # Anxiety is the third BODY row, because the outcome above it takes two.
  expect_match(.body_col(ft, 1)[3], "^Anxiety \\[1\\]")
  expect_no_match(.body_col(ft, 1)[1], "[1]", fixed = TRUE)
  expect_match(txt, "[1] Core GRADE 4 Fig 2 recommends restricting",
               fixed = TRUE)

  # The per-row responder notes are keyed by name, not by row index, and both
  # converted outcomes still get theirs.
  expect_match(txt, "[Depression] Responder presentation:", fixed = TRUE)
  expect_match(txt, "[Anxiety] Responder presentation:", fixed = TRUE)
  expect_match(txt, "Shown on two rows", fixed = TRUE)
})

test_that("keep_effect_scale is a legal pmatools_display name", {
  g <- mk_grade(mk_meta_up(), "Depression")
  attr(g, "pmatools_display") <- list(
    convert_smd_to_or = TRUE, keep_effect_scale = TRUE, baseline_risk = CHINN_P0)
  expect_no_error(pmatools:::.check_outcome_display(g, "Depression"))
  expect_true("keep_effect_scale" %in% pmatools:::PMATOOLS_RESPONDER_FIELDS)
})

# --------------------------------------------------------------------------
# The CSV mirror: one row per outcome, both scales in the cell
# --------------------------------------------------------------------------

test_that("the CSV mirror keeps a both outcome on one row", {
  set <- suppressWarnings(grade_meta_multi(
    list("Depression" = mk_meta_up(), "Insomnia" = mk_meta_down()),
    common = list(study_design = "RCT", rob = "no",
                  small_values = "desirable",
                  rob_rationale = "Consensus RoB2: all domains low risk",
                  indirectness = "no", outcome_type = "absolute",
                  threshold_type = "null")))
  attr(set$outcomes[["Depression"]], "pmatools_display") <- list(
    convert_smd_to_or = TRUE, keep_effect_scale = TRUE,
    baseline_risk = CHINN_P0, chinn_invert = FALSE)

  df <- pmatools:::.sof_set_dataframe(set)
  expect_equal(nrow(df), 2L)

  # A merged cell has no meaning in CSV, so the two scales share the cell and
  # the newline between them is flattened like every other in this file.
  eff <- df$effect[df$outcome == "Depression"]
  expect_match(eff, "^Standardised mean difference ")
  expect_match(eff, "Derived odds ratio ")
  expect_no_match(eff, "\n", fixed = TRUE)
})
