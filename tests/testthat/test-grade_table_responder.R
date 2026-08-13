# test-grade_table_responder.R - v0.5.1:
# grade_table() presenting a continuous outcome as a proportion of responders,
# per row. sof_table() takes the same presentation as arguments and keeps its
# hard abort (test-sof_bmj.R); the combined table reads it off each rated
# object and a row that cannot be converted falls back instead of aborting.

library(testthat)
library(meta)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

quiet_grade <- function(...) suppressWarnings(grade_meta(...))

.footer_text  <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")
.footer_lines <- function(ft) as.character(ft$footer$dataset[[1]])
.body_col     <- function(ft, i) as.character(ft$body$dataset[[i]])

# The four names of the responder presentation, stamped the way the Shiny app
# stamps them when the reviewer picks that route.
stamp_responder <- function(g, baseline_risk = 0.30, threshold_label = NULL,
                            chinn_invert = FALSE, convert = TRUE) {
  attr(g, "pmatools_display") <- list(
    convert_smd_to_or = convert,
    baseline_risk     = baseline_risk,
    threshold_label   = threshold_label,
    chinn_invert      = chinn_invert
  )
  g
}

mk_continuous <- function(name = "Sleep duration", sm = "MD") {
  m <- meta::metacont(
    n.e = c(50, 60, 70), mean.e = c(20, 22, 21), sd.e = c(10, 11, 12),
    n.c = c(50, 60, 70), mean.c = c(8, 9, 8),    sd.c = c(10, 11, 12),
    studlab = c("A", "B", "C"), sm = sm
  )
  quiet_grade(m, study_design = "RCT", rob = "no",
              rob_rationale = "Consensus RoB2: all domains low risk",
              indirectness = "no", outcome_name = name,
              outcome_type = "absolute", threshold_type = "null")
}

mk_binary <- function(name = "Mortality") {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = "RR")
  quiet_grade(ma, study_design = "RCT", rob = "no",
              rob_rationale = "Consensus RoB2: all domains low risk",
              indirectness = "no", outcome_name = name,
              threshold_type = "null", baseline_risk = 0.30)
}

# --------------------------------------------------------------------------
# Requirement 1: a mixed table
# --------------------------------------------------------------------------

test_that("one converted row, one binary row and one unconverted row coexist", {
  outcomes <- list(
    "Depression"  = stamp_responder(mk_continuous("Depression"),
                                    baseline_risk = 0.30),
    "Mortality"   = mk_binary(),
    "Sleep hours" = mk_continuous("Sleep hours")
  )

  for (style in c("bmj", "gradepro")) {
    ft <- grade_table(outcomes, style = style)
    j  <- if (identical(style, "bmj")) c(4L, 5L) else c(3L, 4L)
    cer <- .body_col(ft, j[1])
    ier <- .body_col(ft, j[2])

    # Row 1 is dichotomised: responder rates, marked with the '*' that links
    # them to the Chinn footnote.
    expect_match(cer[1], "^300 per 1,?000 \\*$")
    expect_match(ier[1], "per 1,?000\n\\([0-9]+[;to ]+[0-9]+\\) \\*$")

    # Row 2 is a binary outcome: event rates off its own baseline risk, and no
    # marker, because nothing about it was converted.
    expect_match(cer[2], "^300 per 1,?000$")
    expect_no_match(ier[2], "\\*")

    # Row 3 is the same continuous outcome shown as itself: arm-level means.
    expect_match(cer[3], "^[0-9.]+$")
    expect_no_match(ier[3], "\\*")
  }
})

test_that("the converted row's rates are Chinn's, not the arm means", {
  outcomes <- list(
    "Depression"  = stamp_responder(mk_continuous("Depression"),
                                    baseline_risk = 0.30),
    "Sleep hours" = mk_continuous("Sleep hours")
  )
  ft <- grade_table(outcomes, style = "bmj")

  # Same numbers the single-outcome table produces for that outcome, so the
  # combined table cannot drift from sof_table().
  solo <- sof_table(mk_continuous("Depression"), style = "bmj",
                    convert_smd_to_or = TRUE, baseline_risk = 0.30)
  expect_identical(.body_col(ft, 4)[1], .body_col(solo, 4))
  expect_identical(.body_col(ft, 5)[1], .body_col(solo, 5))

  # And the arm-derivation footnote still describes the row that still needs
  # it, not the converted one.
  expect_match(.footer_text(ft), "inverse-variance weighted mean", fixed = TRUE)
})

test_that("two rows converted in opposite directions each state their own", {
  outcomes <- list(
    "Depression" = stamp_responder(mk_continuous("Depression"),
                                   threshold_label = ">=50% drop in PHQ-9",
                                   chinn_invert = TRUE),
    "Anxiety"    = stamp_responder(mk_continuous("Anxiety"),
                                   chinn_invert = FALSE)
  )
  txt <- .footer_text(grade_table(outcomes, style = "bmj"))

  expect_match(txt, "[Depression] Responder presentation: OR direction inverted",
               fixed = TRUE)
  expect_match(txt, "Threshold definition: >=50% drop in PHQ-9.", fixed = TRUE)
  expect_match(txt, "[Anxiety] Responder presentation: OR direction as given",
               fixed = TRUE)
  # The method paragraph is written once however many rows used it.
  lines <- .footer_lines(grade_table(outcomes, style = "bmj"))
  expect_equal(sum(grepl("dichotomised via Chinn", lines, fixed = TRUE)), 1L)
})

# --------------------------------------------------------------------------
# Requirement 3: a row that cannot convert falls back
# --------------------------------------------------------------------------

test_that("a binary row that asks for the conversion falls back, not aborts", {
  outcomes <- list(
    "Depression" = stamp_responder(mk_continuous("Depression")),
    "Mortality"  = stamp_responder(mk_binary())
  )
  ft <- expect_no_error(grade_table(outcomes, style = "bmj"))
  txt <- .footer_text(ft)

  expect_match(txt, "could not be applied: its effect measure is RR",
               fixed = TRUE)
  expect_match(txt, "shows the unconverted presentation instead", fixed = TRUE)
  # The reason is keyed to the row by a numbered marker on the outcome cell.
  expect_match(.body_col(ft, 1)[2], "^Mortality \\[[0-9]+\\]")
  # The row itself is unharmed: its own baseline-risk rates, unmarked.
  expect_match(.body_col(ft, 4)[2], "^300 per 1000$")

  # sof_table() still aborts on exactly this outcome - the old path is
  # unchanged, and only the combined table falls back.
  expect_error(sof_table(mk_binary(), convert_smd_to_or = TRUE,
                         baseline_risk = 0.3),
               "requires meta_obj\\$sm")
})

test_that("a missing or out-of-range responder proportion falls back", {
  for (p0 in list(NULL, 0, 1, 1.5, NA_real_)) {
    outcomes <- list(
      "Depression" = stamp_responder(mk_continuous("Depression"),
                                     baseline_risk = p0))
    ft <- expect_no_error(grade_table(outcomes, style = "bmj"))
    expect_match(.footer_text(ft),
                 "no control-group responder proportion in (0, 1) was recorded",
                 fixed = TRUE)
    # Fallen back means presented as itself: arm-level means, no '*'.
    expect_no_match(.body_col(ft, 4)[1], "\\*")
  }
})

test_that("a fallback row keeps its analysis-set note as well as its reason", {
  # Two notes on one row: the register numbers both and the outcome cell
  # carries both markers.
  g <- stamp_responder(mk_binary())
  g$rob_refit <- TRUE
  g$meta_full <- g$meta
  ft <- grade_table(list("Mortality" = g), style = "bmj")
  expect_match(.body_col(ft, 1)[1], "^Mortality \\[1\\]\\[2\\]$")
})

# --------------------------------------------------------------------------
# Requirement 4: the footnote appears when used, and only then
# --------------------------------------------------------------------------

test_that("the Chinn footnote appears only when a row actually used it", {
  used <- grade_table(
    list("Depression" = stamp_responder(mk_continuous("Depression"))),
    style = "bmj")
  expect_match(.footer_text(used), "dichotomised via Chinn's formula",
               fixed = TRUE)

  none <- grade_table(list("Sleep hours" = mk_continuous("Sleep hours"),
                           "Mortality"   = mk_binary()), style = "bmj")
  expect_no_match(.footer_text(none), "Chinn", fixed = TRUE)

  # Asked for and refused is not "used": the method paragraph would describe a
  # conversion no cell in the table went through.
  asked <- grade_table(list("Mortality" = stamp_responder(mk_binary())),
                       style = "bmj")
  expect_no_match(.footer_text(asked), "dichotomised via Chinn", fixed = TRUE)
  expect_match(.footer_text(asked), "could not be applied", fixed = TRUE)
})

test_that("convert_smd_to_or = FALSE on the attribute converts nothing", {
  ft <- grade_table(
    list("Depression" = stamp_responder(mk_continuous("Depression"),
                                        convert = FALSE)),
    style = "bmj")
  expect_no_match(.footer_text(ft), "Chinn", fixed = TRUE)
  expect_no_match(.body_col(ft, 4)[1], "\\*")
})

test_that("an unknown name in the display attribute aborts", {
  g <- mk_continuous("Depression")
  attr(g, "pmatools_display") <- list(convert_smd_to_ors = TRUE)
  expect_error(grade_table(list("Depression" = g)), "Unknown 'pmatools_display'")
})

# --------------------------------------------------------------------------
# Requirement 5: the bundle carries it
# --------------------------------------------------------------------------

test_that("the bundled table, CSV and analysis.R all carry the conversion", {
  skip_if_not_installed("zip")

  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    outcome = "Depression",
    n       = c(50, 50, 60, 60, 70, 70),
    mean    = c(20, 8, 22, 9, 21, 8),
    sd      = c(10, 10, 11, 11, 12, 12),
    stringsAsFactors = FALSE
  )
  ma_list <- run_ma_multi(data, sm = "MD")
  set <- suppressWarnings(grade_meta_multi(
    ma_list,
    common = list(study_design = "RCT", rob = "no",
                  rob_rationale = "Consensus RoB2: all domains low risk",
                  indirectness = "no", outcome_type = "absolute",
                  threshold_type = "null")))
  set$outcomes[["Depression"]] <- stamp_responder(
    set$outcomes[["Depression"]], baseline_risk = 0.30,
    threshold_label = ">=50% drop in PHQ-9", chinn_invert = TRUE)

  out_dir <- withr::local_tempdir()
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "responder",
                  include = c("sof", "script")))
  unpacked <- file.path(out_dir, "unpacked")
  zip::unzip(zip_path, exdir = unpacked)

  # The one Summary of Findings in the ZIP shows the converted row.
  expect_true(file.exists(file.path(unpacked, "summary_of_findings.docx")))
  csv <- utils::read.csv(file.path(unpacked, "summary_of_findings.csv"),
                         stringsAsFactors = FALSE)
  expect_match(csv$risk_control[1], "^300 per 1000 \\*$")
  expect_match(csv$risk_intervention[1], "\\*$")

  # And the script re-stamps the presentation onto the set it rebuilds, so
  # re-running it reproduces the same table rather than the unconverted one.
  script <- paste(readLines(file.path(unpacked, "analysis.R")), collapse = "\n")
  expect_match(script, "pmatools_display", fixed = TRUE)
  expect_match(script, "convert_smd_to_or = TRUE", fixed = TRUE)
  expect_match(script, "baseline_risk     = 0.3", fixed = TRUE)
  expect_match(script, "chinn_invert      = TRUE", fixed = TRUE)
})

test_that("a set with no converted outcome gets no re-stamp block", {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    outcome = "Depression",
    n       = c(50, 50, 60, 60, 70, 70),
    mean    = c(20, 8, 22, 9, 21, 8),
    sd      = c(10, 10, 11, 11, 12, 12),
    stringsAsFactors = FALSE
  )
  set <- suppressWarnings(grade_meta_multi(
    run_ma_multi(data, sm = "MD"),
    common = list(study_design = "RCT", rob = "no",
                  rob_rationale = "Consensus RoB2: all domains low risk",
                  indirectness = "no", outcome_type = "absolute",
                  threshold_type = "null")))
  expect_identical(pmatools:::.responder_stamp_block(set), "")
})
