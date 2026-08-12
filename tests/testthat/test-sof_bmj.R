# test-sof_bmj.R — v0.5 (Phase D):
# BMJ Core GRADE presentation of the Summary of Findings table
# (style = "bmj"), the Core GRADE 6 Box 1 plain language statements, and
# the propagation of the Core GRADE 4 analysis-set note into grade_table() /
# grade_report().

library(testthat)
library(meta)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

quiet_grade <- function(...) suppressWarnings(grade_meta(...))

.footer_text <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")
.body_col    <- function(ft, i) as.character(ft$body$dataset[[i]])

# Binary fixture. `benefit = FALSE` flips the arms so the pooled effect points
# at harm, which is what makes the fewer/more direction observable.
make_binary <- function(benefit = TRUE, sm = "RR", ...) {
  ev_e <- c(10, 15, 20)
  ev_c <- c(15, 20, 25)
  if (!benefit) { tmp <- ev_e; ev_e <- ev_c; ev_c <- tmp }
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(ev_e[1], ev_c[1], ev_e[2], ev_c[2], ev_e[3], ev_c[3]),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = sm)
  quiet_grade(ma, study_design = "RCT", rob = "no",
              rob_rationale = "Consensus RoB2: all domains low risk",
              indirectness = "no", outcome_name = "Mortality",
              threshold_type = "null", ...)
}

make_continuous <- function() {
  m <- meta::metacont(
    n.e = c(50, 60, 70), mean.e = c(20, 22, 21), sd.e = c(10, 11, 12),
    n.c = c(50, 60, 70), mean.c = c(8, 9, 8),    sd.c = c(10, 11, 12),
    studlab = c("A", "B", "C"), sm = "MD"
  )
  quiet_grade(m, study_design = "RCT", rob = "no",
              rob_rationale = "Consensus RoB2: all domains low risk",
              indirectness = "no", outcome_name = "Sleep duration",
              outcome_type = "absolute", threshold_type = "null")
}

# metagen fixtures for the Core GRADE 4 refit leaf (mirrors
# test-rob_flowchart.R: exact inverse-variance weights via tau.preset = 0).
mk_gen <- function(te, w, studlab = paste0("S", seq_along(te)), sm = "RR") {
  meta::metagen(TE = te, seTE = sqrt(1 / w), studlab = studlab, sm = sm,
                tau.preset = 0)
}

make_refit <- function() {
  quiet_grade(mk_gen(te = c(1.2, 0.02, 0.02, 0.02),
                     w  = c(400, 400 / 3, 400 / 3, 400 / 3),
                     studlab = c("High-1", "Low-1", "Low-2", "Low-3")),
              rob = c("serious", "no", "no", "no"),
              small_values = "undesirable",
              threshold = 1.05, threshold_scale = "ratio",
              outcome_name = "Refitted outcome")
}

make_no_refit <- function() {
  quiet_grade(mk_gen(c(0.8, 0.02, 0.02), c(3, 1, 1)),
              rob = c("serious", "no", "no"),
              small_values = "undesirable",
              threshold = 1.05, threshold_scale = "ratio",
              outcome_name = "All studies outcome")
}

# --- D-1 / D-2: the BMJ layout ---------------------------------------------

test_that("style = 'bmj' returns a flextable with the spanning absolute header", {
  g  <- make_binary()
  ft <- sof_table(g, style = "bmj",
                  follow_up = "Follow-up: longest, range 7.7-60 months")

  expect_s3_class(ft, "flextable")

  hdrs <- names(ft$body$dataset)
  expect_identical(hdrs[1:7], c(
    "Outcome and follow-up",
    "No of participants\n(No of studies and type)",
    "Relative effect\n(95% CI)",
    "With control",
    "With intervention",
    "Difference",
    "Certainty of evidence\n(quality of evidence)"
  ))
  expect_identical(hdrs[8], "Plain language summary")

  # Two header rows: the spanning "Absolute effects (95% CI)" strip on top.
  expect_equal(nrow(ft$header$dataset), 2L)
  top <- as.character(unlist(ft$header$dataset[1, ]))
  expect_true("Absolute effects (95% CI)" %in% top)
  expect_equal(sum(top == "Absolute effects (95% CI)"), 3L)

  # Follow-up sits under the outcome name; participants use the BMJ wording.
  expect_match(.body_col(ft, 1), "^Mortality\nFollow-up: longest")
  expect_match(.body_col(ft, 2), "^360 \\(3 randomised controlled trials\\)$")
  # Relative effect spells the measure out and uses "to" as the separator.
  expect_match(.body_col(ft, 3), "^Risk ratio [0-9.]+ \\([0-9.]+ to [0-9.]+\\)$")
})

test_that("the bmj table can be written to docx", {
  skip_if_not_installed("officer")
  g   <- make_binary()
  ft  <- sof_table(g, style = "bmj")
  out <- tempfile(fileext = ".docx")
  expect_no_error(flextable::save_as_docx(ft, path = out))
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
  unlink(out)
})

test_that("follow_up = NULL leaves the outcome cell without a time-frame line", {
  g  <- make_binary()
  ft <- sof_table(g, style = "bmj")
  expect_identical(.body_col(ft, 1), "Mortality")
})

# --- D-1: the GRADEpro default is untouched --------------------------------

test_that("style = 'gradepro' (the default) reproduces the existing table", {
  g  <- make_binary()
  ft <- sof_table(g)

  expect_identical(names(ft$body$dataset), c(
    "Outcome",
    "No. of participants\n(studies)",
    "Risk with control\n(per 1,000)",
    "Risk with intervention\n(per 1,000)",
    "Relative effect\n(95% CI)",
    "Certainty of the evidence\n(Core GRADE series)"
  ))
  expect_identical(.body_col(ft, 1), "Mortality")
  expect_identical(.body_col(ft, 2), "360 (3 RCTs)")
  expect_match(.body_col(ft, 5), "^RR [0-9.]+ \\([0-9.]+; [0-9.]+\\)$")
  expect_match(.body_col(ft, 6), "^High\n")

  # One header row (no spanning strip) and exactly two footer lines for this
  # fixture: the base note plus the publication-bias note.
  expect_equal(nrow(ft$header$dataset), 1L)
  expect_equal(nrow(ft$footer$dataset), 2L)

  # No BMJ vocabulary leaks into the default style.
  txt <- paste(c(names(ft$body$dataset), unlist(ft$body$dataset),
                 .footer_text(ft)), collapse = " ")
  expect_no_match(txt, "Plain language")
  expect_no_match(txt, "Absolute effects")

  # The default argument really is gradepro.
  ft2 <- sof_table(g, style = "gradepro")
  expect_identical(names(ft$body$dataset), names(ft2$body$dataset))
  expect_identical(unlist(ft$body$dataset), unlist(ft2$body$dataset))
})

test_that("grade_table style = 'gradepro' keeps its column headers", {
  g  <- make_binary()
  ft <- grade_table(list("Mortality" = g))
  expect_identical(names(ft$body$dataset)[1:6], c(
    "Outcome",
    "No. of participants\n(studies)",
    "Risk with control\n(per 1,000)",
    "Risk with intervention\n(per 1,000)",
    "Relative effect\n(95% CI)",
    "Certainty of the evidence\n(Core GRADE series)"
  ))
  expect_identical(.body_col(ft, 1), "Mortality")
})

# --- number formatting: BMJ drops the thousands separator, "to" everywhere ---

test_that("bmj rates print without a thousands separator", {
  ft <- sof_table(make_binary(), style = "bmj")
  expect_identical(.body_col(ft, 4), "333 per 1000")
  expect_no_match(.body_col(ft, 4), ",")
  expect_no_match(.body_col(ft, 5), ",")
  expect_no_match(.body_col(ft, 6), ",")
})

test_that("the bmj intervention CI is separated by 'to', not ';'", {
  ft  <- sof_table(make_binary(), style = "bmj")
  ier <- .body_col(ft, 5)
  expect_identical(ier, "251 per 1000\n(203 to 310)")
  expect_no_match(ier, ";")
  # The relative effect column agrees (Phase D already used "to").
  expect_identical(.body_col(ft, 3), "Risk ratio 0.75 (0.61 to 0.93)")
  expect_no_match(.body_col(ft, 3), ";")
})

test_that("all three absolute-effect columns share one 'per N' format", {
  for (p in c(1000, 100)) {
    ft <- sof_table(make_binary(), style = "bmj", per = p)
    want <- paste0("per ", p)          # "per 1000" / "per 100", no separator
    for (j in 4:6) {
      expect_match(.body_col(ft, j), want, fixed = TRUE)
      expect_no_match(.body_col(ft, j), ",")
    }
  }
})

test_that("per = 100 keeps the bmj cells intact", {
  ft <- sof_table(make_binary(), style = "bmj", per = 100)
  expect_identical(.body_col(ft, 4), "33 per 100")
  expect_identical(.body_col(ft, 5), "25 per 100\n(20 to 31)")
  expect_identical(.body_col(ft, 6), "8 fewer per 100 (13 fewer to 2 fewer)")
})

test_that("the multi-outcome bmj table formats numbers the same way", {
  g  <- make_binary()
  ft <- grade_table(list("Mortality" = g), style = "bmj")
  expect_identical(.body_col(ft, 4), "333 per 1000")
  expect_identical(.body_col(ft, 5), "251 per 1000\n(203 to 310)")
  expect_identical(.body_col(ft, 6), "82 fewer per 1000 (131 fewer to 23 fewer)")
})

test_that("the Chinn rate columns follow the style they are rendered in", {
  g <- make_continuous()

  bmj <- sof_table(g, style = "bmj", convert_smd_to_or = TRUE,
                   baseline_risk = 0.3)
  expect_match(.body_col(bmj, 5), "per 1000\n\\([0-9]+ to [0-9]+\\) \\*$")
  expect_no_match(.body_col(bmj, 4), ",")
  expect_no_match(.body_col(bmj, 5), ";")

  gp <- sof_table(g, convert_smd_to_or = TRUE, baseline_risk = 0.3)
  expect_match(.body_col(gp, 3), "^[0-9]+ per 1,000 \\*$")
  expect_match(.body_col(gp, 4), "per 1,000\n\\([0-9]+; [0-9]+\\) \\*$")
})

test_that("the gradepro cells are byte-for-byte what they were", {
  # Hard-coded snapshot: the shared .format_cer()/.format_ier() helpers gained
  # big_mark/ci_sep arguments for the BMJ style, and their defaults must keep
  # reproducing the GRADEpro output exactly.
  g <- make_binary()

  ft <- sof_table(g)
  expect_identical(unname(unlist(lapply(ft$body$dataset, as.character))), c(
    "Mortality",
    "360 (3 RCTs)",
    "333 per 1,000",
    "251 per 1,000\n(203; 310)",
    "RR 0.75 (0.61; 0.93)",
    paste0("High\n", CERTAINTY_SYMBOLS_UNICODE[["High"]])
  ))

  ft100 <- sof_table(g, per = 100)
  expect_identical(unname(unlist(lapply(ft100$body$dataset, as.character))), c(
    "Mortality",
    "360 (3 RCTs)",
    "33 per 100",
    "25 per 100\n(20; 31)",
    "RR 0.75 (0.61; 0.93)",
    paste0("High\n", CERTAINTY_SYMBOLS_UNICODE[["High"]])
  ))

  # grade_table()'s GRADEpro rows share the same helpers.
  gt <- grade_table(list("Mortality" = g))
  expect_identical(.body_col(gt, 3), "333 per 1,000")
  expect_identical(.body_col(gt, 4), "251 per 1,000\n(203; 310)")
})

test_that("the shared helpers default to the gradepro formatting", {
  g <- make_binary()
  expect_identical(.format_cer(0.333, 1000), "333 per 1,000")
  expect_identical(.format_cer(0.333, 1000, big_mark = FALSE), "333 per 1000")
  expect_identical(.format_ier(g$meta, 0.333, 1000),
                   .format_ier(g$meta, 0.333, 1000,
                               big_mark = TRUE, ci_sep = "; "))
  expect_match(.format_ier(g$meta, 0.333, 1000, big_mark = FALSE,
                           ci_sep = " to "),
               "^[0-9]+ per 1000\n\\([0-9]+ to [0-9]+\\)$")

  expect_identical(.bmj_number_format("gradepro"),
                   list(big_mark = TRUE, ci_sep = "; "))
  expect_identical(.bmj_number_format("bmj"),
                   list(big_mark = FALSE, ci_sep = " to "))
})

# --- D-2: the Difference column --------------------------------------------

test_that("a beneficial effect reads 'fewer' and a harmful one 'more'", {
  ft_benefit <- sof_table(make_binary(benefit = TRUE),  style = "bmj")
  ft_harm    <- sof_table(make_binary(benefit = FALSE), style = "bmj")

  d_benefit <- .body_col(ft_benefit, 6)
  d_harm    <- .body_col(ft_harm,    6)

  expect_match(d_benefit,
    "^[0-9]+ fewer per 1000 \\([0-9]+ fewer to [0-9]+ fewer\\)$")
  expect_match(d_harm,
    "^[0-9]+ more per 1000 \\([0-9]+ more to [0-9]+ more\\)$")
  expect_no_match(d_benefit, "more")
  expect_no_match(d_harm, "fewer")
})

test_that(".format_difference renders the BMJ example format", {
  # RR 0.75 on a baseline of 352/1000 -> 88 fewer per 1000.
  m <- mk_gen(te = log(c(0.78, 0.78)), w = c(400, 400))
  s <- .format_difference(m, baseline_risk = 0.4, per = 1000)
  expect_match(s, "^[0-9]+ fewer per 1000 \\([0-9]+ fewer to [0-9]+ fewer\\)$")

  # A CI spanning the null gets a direction word per bound.
  m2 <- mk_gen(te = log(c(0.95, 0.95)), w = c(20, 20))
  s2 <- .format_difference(m2, baseline_risk = 0.4, per = 1000)
  expect_match(s2, "fewer to [0-9]+ more\\)$")
})

test_that("a continuous outcome gets a unit-bearing difference", {
  g  <- make_continuous()
  ft <- sof_table(g, style = "bmj", unit = "days")
  d  <- .body_col(ft, 6)
  expect_match(d,
    "^[0-9.]+ (more|fewer) days \\([0-9.]+ (more|fewer) to [0-9.]+ (more|fewer)\\)$")
  # Mean difference of +12.96: "more days", never "fewer days".
  expect_match(d, "^[0-9.]+ more days ")

  # Without a unit, the number stands alone.
  ft_nounit <- sof_table(g, style = "bmj")
  expect_match(.body_col(ft_nounit, 6),
               "^[0-9.]+ more \\([0-9.]+ (more|fewer) to [0-9.]+ more\\)$")
})

test_that("no baseline risk falls back to '-' in the Difference column", {
  g <- make_no_refit()          # metagen: no arm-level event counts
  expect_null(g$baseline_risk)
  ft <- sof_table(g, style = "bmj")
  expect_identical(.body_col(ft, 6), "-")
  expect_identical(.format_difference(g$meta, NULL, 1000), "-")
})

# --- D-3: Core GRADE 6 Box 1, verbatim -------------------------------------
#
# Expectations updated for the Core GRADE 6 Box 1 rewrite: the statements used
# to come from Core GRADE 2 Table 1, whose "benefit" wording inverted the
# meaning of harm outcomes. Box 1 names the direction of the effect instead, so
# every statement now needs a direction and an outcome. The exhaustive Box 1
# transcription lives in test-plain_language.R; what is kept here is the
# integration with the bmj table.

test_that("plain language matches Core GRADE 6 Box 1 for all 8 cells", {
  # Null effect as threshold
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "increase"),
    "Treatment increases the outcome")
  expect_identical(
    .plain_language("Moderate", "null", "non_null_effect",
                    direction = "increase"),
    "Treatment probably (likely) increases the outcome")
  expect_identical(
    .plain_language("Low", "null", "non_null_effect", direction = "increase"),
    "Treatment may (possibly) increase the outcome")
  expect_identical(
    .plain_language("Very Low", "null", "non_null_effect",
                    direction = "increase"),
    "We are very uncertain about the effect of treatment on the outcome")

  # MID as threshold
  expect_identical(
    .plain_language("High", "mid", "important_effect", direction = "increase"),
    "Treatment results in an important increase in the outcome")
  expect_identical(
    .plain_language("Moderate", "mid", "important_effect",
                    direction = "increase"),
    "Treatment probably (likely) results in an important increase in the outcome")
  expect_identical(
    .plain_language("Low", "mid", "important_effect", direction = "increase"),
    "Treatment may (possibly) result in an important increase in the outcome")
  expect_identical(
    .plain_language("Very Low", "mid", "important_effect",
                    direction = "increase"),
    "We are very uncertain about the effect of treatment on the outcome")
})

test_that("the little-to-no target selects the Box 1 no-direction wording", {
  expect_identical(
    .plain_language("High", "mid", "little_to_no_difference"),
    "Treatment has little to no important effect on the outcome")
  expect_identical(
    .plain_language("Moderate", "mid", "little_to_no_difference"),
    "Treatment probably has little to no important effect on the outcome")
  expect_identical(
    .plain_language("Low", "mid", "little_to_no_difference"),
    "Treatment may (possibly) have little to no important effect on the outcome")
  # The null column has its own wording in Box 1 ("has little to no effect"),
  # so it no longer borrows the MID column's "important" phrasing.
  expect_identical(
    .plain_language("Low", "null", "little_to_no_difference"),
    "Treatment may (possibly) have little to no effect on the outcome")
})

test_that("the outcome label is named in the statement", {
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "decrease",
                    outcome_label = "sleep quality"),
    "Treatment reduces sleep quality")
  expect_identical(
    .plain_language("Moderate", "null", "non_null_effect",
                    direction = "increase", outcome_label = "sleep quality"),
    "Treatment probably (likely) increases sleep quality")
  expect_identical(
    .plain_language("Low", "null", "non_null_effect", direction = "increase",
                    outcome_label = "sleep quality"),
    "Treatment may (possibly) increase sleep quality")
})

test_that("the intervention label opens the Box 1 statement", {
  expect_identical(
    .plain_language("High", "mid", "important_effect", direction = "increase",
                    intervention_label = "CBT-I",
                    outcome_label = "sleep quality"),
    "CBT-I results in an important increase in sleep quality")
  expect_identical(
    .plain_language("Very Low", "mid", "important_effect",
                    intervention_label = "CBT-I",
                    outcome_label = "sleep quality"),
    "We are very uncertain about the effect of CBT-I on sleep quality")
})

test_that("the bmj table carries the plain language column and its footnote", {
  g  <- make_binary()
  ft <- sof_table(g, style = "bmj")
  # make_binary() pools RR < 1 on "Mortality": Box 1 wording is directional.
  expect_identical(.body_col(ft, 8), "Treatment reduces mortality")
  expect_match(.footer_text(ft), "Core GRADE 6 box 1", fixed = TRUE)
  expect_match(.footer_text(ft),
               "does not say whether that effect is a benefit or a harm",
               fixed = TRUE)

  # The harm direction of the same fixture must not read as a benefit.
  h  <- make_binary(benefit = FALSE)
  fh <- sof_table(h, style = "bmj")
  expect_identical(.body_col(fh, 8), "Treatment increases mortality")
  expect_no_match(.body_col(fh, 8), "benefit", fixed = TRUE)
})

test_that("an object without a rating target drops the column, without error", {
  g <- make_binary()
  g$rating_target <- NULL
  expect_null(g$rating_target)
  expect_null(.plain_language(g$certainty, g$threshold_type, g$rating_target,
                              direction = "decrease"))

  ft <- expect_no_error(sof_table(g, style = "bmj"))
  expect_equal(ncol(ft$body$dataset), 7L)
  expect_false("Plain language summary" %in% names(ft$body$dataset))
  expect_no_match(.footer_text(ft), "Core GRADE 6 box 1", fixed = TRUE)
  # The spanning header still lines up over the three absolute-effect columns.
  top <- as.character(unlist(ft$header$dataset[1, ]))
  expect_equal(sum(top == "Absolute effects (95% CI)"), 3L)
})

# --- D-4: the analysis-set note survives every output ----------------------

test_that("the bmj footer states the low-RoB refit", {
  g <- make_refit()
  expect_true(g$rob_refit)
  ft <- sof_table(g, style = "bmj")
  expect_match(.footer_text(ft),
               "Effect estimate restricted to low risk of bias studies")
  expect_match(.footer_text(ft), "(n = 3 of 4)", fixed = TRUE)
})

test_that("grade_table marks the refitted row and footnotes it", {
  g_refit    <- make_refit()
  g_no_refit <- make_no_refit()
  expect_true(g_refit$rob_refit)
  expect_false(g_no_refit$rob_refit)
  expect_identical(g_no_refit$rob_analysis_set, "all")

  for (sty in c("gradepro", "bmj")) {
    ft <- grade_table(list("Refitted" = g_refit, "All studies" = g_no_refit),
                      style = sty)
    outcome_cells <- .body_col(ft, 1)
    expect_identical(outcome_cells[1], "Refitted [1]")
    # The unaffected outcome carries no marker.
    expect_identical(outcome_cells[2], "All studies")

    expect_match(.footer_text(ft),
                 "[1] Effect estimate restricted to low risk of bias studies",
                 fixed = TRUE)
  }
})

test_that("grade_table adds no marker when no outcome was refitted", {
  g  <- make_binary()
  ft <- grade_table(list("Mortality" = g))
  expect_identical(.body_col(ft, 1), "Mortality")
  expect_no_match(.footer_text(ft), "restricted to low risk of bias")
})

test_that("grade_report records the analysis set for the refitted outcome", {
  md <- .build_report_md(
    list("Refitted" = make_refit(), "All studies" = make_no_refit()),
    primary = NULL, title = "t", show_domains = TRUE)
  expect_match(md, "Analysis set: Effect estimate restricted to low risk of bias")
  # Exactly one outcome section carries the note.
  expect_equal(length(gregexpr("Analysis set:", md)[[1]]), 1L)
})

# --- D-5: rate-down reasons in the certainty cell --------------------------

fake_grade <- function(judgments, downgrades, study_design = "RCT",
                       starting_quality = "High") {
  structure(list(
    study_design     = study_design,
    starting_quality = starting_quality,
    domain_assessments = data.frame(
      domain    = c("Risk of bias", "Indirectness", "Inconsistency",
                    "Imprecision", "Publication bias"),
      judgment  = judgments,
      downgrade = downgrades,
      stringsAsFactors = FALSE
    )
  ), class = "pmatools")
}

test_that("a shared severity is stated once across the downgraded domains", {
  g <- fake_grade(
    judgments  = c("some_concerns", "no", "no", "some_concerns", "no"),
    downgrades = c(-1, 0, 0, -1, 0))
  expect_identical(.certainty_rate_down_reason(g),
                   "Due to serious risk of bias and imprecision")
})

test_that("a -2 domain reads 'very serious'", {
  g <- fake_grade(
    judgments  = c("no", "no", "no", "serious", "no"),
    downgrades = c(0, 0, 0, -2, 0))
  expect_identical(.certainty_rate_down_reason(g),
                   "Due to very serious imprecision")

  g_mixed <- fake_grade(
    judgments  = c("some_concerns", "no", "no", "serious", "no"),
    downgrades = c(-1, 0, 0, -2, 0))
  expect_identical(.certainty_rate_down_reason(g_mixed),
                   "Due to serious risk of bias and very serious imprecision")
})

test_that("three downgraded domains are enumerated with commas", {
  g <- fake_grade(
    judgments  = c("some_concerns", "some_concerns", "some_concerns", "no", "no"),
    downgrades = c(-1, -1, -1, 0, 0))
  expect_identical(
    .certainty_rate_down_reason(g),
    "Due to serious risk of bias, indirectness and inconsistency")
})

test_that("an observational start is named as such", {
  g <- fake_grade(
    judgments  = rep("no", 5), downgrades = rep(0, 5),
    study_design = "obs", starting_quality = "Low")
  expect_identical(.certainty_rate_down_reason(g), "Due to non-randomised studies")

  g2 <- fake_grade(
    judgments  = c("no", "no", "no", "some_concerns", "no"),
    downgrades = c(0, 0, 0, -1, 0),
    study_design = "obs", starting_quality = "Low")
  expect_identical(.certainty_rate_down_reason(g2),
                   "Due to non-randomised studies and serious imprecision")
})

test_that("nothing downgraded means no reason line", {
  g <- fake_grade(judgments = rep("no", 5), downgrades = rep(0, 5))
  expect_null(.certainty_rate_down_reason(g))

  ft <- sof_table(make_binary(), style = "bmj")
  expect_identical(.body_col(ft, 7), "High")
})

test_that("the certainty cell shows certainty and reason on separate lines", {
  g  <- make_refit()
  ft <- sof_table(g, style = "bmj")
  cell <- .body_col(ft, 7)
  expect_match(cell, paste0("^", g$certainty))
  reason <- .certainty_rate_down_reason(g)
  if (!is.null(reason)) {
    expect_identical(cell, paste0(g$certainty, "\n", reason))
  } else {
    expect_identical(cell, g$certainty)
  }
})
