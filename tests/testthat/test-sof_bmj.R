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
              rob = c("very_serious", "no", "no", "no"),
              small_values = "undesirable",
              threshold = 1.05, threshold_scale = "ratio",
              outcome_name = "Refitted outcome")
}

make_no_refit <- function() {
  quiet_grade(mk_gen(c(0.8, 0.02, 0.02), c(3, 1, 1)),
              rob = c("very_serious", "no", "no"),
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

# --- D-3: Core GRADE 6 Box 1 -----------------------------------------------
#
# Expectations updated for the Core GRADE 6 Box 1 rewrite: the statements used
# to come from Core GRADE 2 Table 1, whose "benefit" wording inverted the
# meaning of harm outcomes. Box 1 names the direction of the effect instead, so
# every statement now needs a direction and an outcome. The exhaustive Box 1
# transcription lives in test-plain_language.R; what is kept here is the
# integration with the bmj table.
#
# Updated again in v0.5.0 for the single-adverb rule: Box 1's qualifier list
# prints two adverbs per certainty level ("probably (likely)", "may
# (possibly)") but none of CG6's own summary of findings tables prints both, so
# pmatools emits the first word of each pair. The Moderate expectations below
# therefore read "probably" and the Low ones "may". Rationale and the verbatim
# Box 1 record are in R/plain_language.R.

test_that("plain language matches Core GRADE 6 Box 1 for all 8 cells", {
  # Null effect as threshold
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "increase"),
    "Treatment increases the outcome")
  expect_identical(
    .plain_language("Moderate", "null", "non_null_effect",
                    direction = "increase"),
    "Treatment probably increases the outcome")
  expect_identical(
    .plain_language("Low", "null", "non_null_effect", direction = "increase"),
    "Treatment may increase the outcome")
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
    "Treatment probably results in an important increase in the outcome")
  expect_identical(
    .plain_language("Low", "mid", "important_effect", direction = "increase"),
    "Treatment may result in an important increase in the outcome")
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
    "Treatment may have little to no important effect on the outcome")
  # The null column has its own wording in Box 1 ("has little to no effect"),
  # so it no longer borrows the MID column's "important" phrasing.
  expect_identical(
    .plain_language("Low", "null", "little_to_no_difference"),
    "Treatment may have little to no effect on the outcome")
})

test_that("the outcome label is named in the statement", {
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "decrease",
                    outcome_label = "sleep quality"),
    "Treatment reduces sleep quality")
  expect_identical(
    .plain_language("Moderate", "null", "non_null_effect",
                    direction = "increase", outcome_label = "sleep quality"),
    "Treatment probably increases sleep quality")
  expect_identical(
    .plain_language("Low", "null", "non_null_effect", direction = "increase",
                    outcome_label = "sleep quality"),
    "Treatment may increase sleep quality")
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
    judgments  = c("serious", "not_serious", "not_serious", "serious", "not_serious"),
    downgrades = c(-1, 0, 0, -1, 0))
  expect_identical(.certainty_rate_down_reason(g),
                   "Due to serious risk of bias and imprecision")
})

test_that("a -2 domain reads 'very serious'", {
  g <- fake_grade(
    judgments  = c("not_serious", "not_serious", "not_serious", "very_serious", "not_serious"),
    downgrades = c(0, 0, 0, -2, 0))
  expect_identical(.certainty_rate_down_reason(g),
                   "Due to very serious imprecision")

  g_mixed <- fake_grade(
    judgments  = c("serious", "not_serious", "not_serious", "very_serious", "not_serious"),
    downgrades = c(-1, 0, 0, -2, 0))
  expect_identical(.certainty_rate_down_reason(g_mixed),
                   "Due to serious risk of bias and very serious imprecision")
})

test_that("three downgraded domains are enumerated with commas", {
  g <- fake_grade(
    judgments  = c("serious", "serious", "serious", "not_serious", "not_serious"),
    downgrades = c(-1, -1, -1, 0, 0))
  expect_identical(
    .certainty_rate_down_reason(g),
    "Due to serious risk of bias, indirectness and inconsistency")
})

test_that("an observational start is named as such", {
  g <- fake_grade(
    judgments  = rep("not_serious", 5), downgrades = rep(0, 5),
    study_design = "obs", starting_quality = "Low")
  expect_identical(.certainty_rate_down_reason(g), "Due to non-randomised studies")

  g2 <- fake_grade(
    judgments  = c("not_serious", "not_serious", "not_serious", "serious", "not_serious"),
    downgrades = c(0, 0, 0, -1, 0),
    study_design = "obs", starting_quality = "Low")
  expect_identical(.certainty_rate_down_reason(g2),
                   "Due to non-randomised studies and serious imprecision")
})

test_that("nothing downgraded means no reason line", {
  g <- fake_grade(judgments = rep("not_serious", 5), downgrades = rep(0, 5))
  expect_null(.certainty_rate_down_reason(g))

  ft <- sof_table(make_binary(), style = "bmj")
  expect_identical(.body_col(ft, 7), "High")
})

test_that("the certainty cell shows certainty and reason on separate lines", {
  g  <- make_refit()
  ft <- sof_table(g, style = "bmj")
  cell <- .body_col(ft, 7)
  expect_match(cell, paste0("^", g$certainty))
  # v0.5.1: a rated-down domain that recorded structured facts also carries
  # the numbered marker of its footnote inside the reason sentence, so the
  # expected cell has to be built with the same marker register the table used.
  doms    <- .rated_down_fact_domains(g)
  markers <- stats::setNames(seq_along(doms), doms)
  reason  <- .certainty_rate_down_reason(g, markers = markers)
  if (!is.null(reason)) {
    expect_identical(cell, paste0(g$certainty, "\n", reason))
  } else {
    expect_identical(cell, g$certainty)
  }
})

# --- Core GRADE 6: arm-level columns for a continuous outcome ---------------
#
# Core GRADE 6 calls it the preferred presentation to give the outcome in the
# comparison group, in the intervention group AND the difference. A metacont
# object has no baseline risk, so the two arm columns used to fall back to "-"
# and only the Difference column carried anything.

make_continuous_smd <- function() {
  m <- meta::metacont(
    n.e = c(50, 60, 70), mean.e = c(20, 22, 21), sd.e = c(10, 11, 12),
    n.c = c(50, 60, 70), mean.c = c(8, 9, 8),    sd.c = c(10, 11, 12),
    studlab = c("A", "B", "C"), sm = "SMD"
  )
  quiet_grade(m, study_design = "RCT", rob = "no",
              rob_rationale = "Consensus RoB2: all domains low risk",
              indirectness = "no", outcome_name = "Sleep duration",
              outcome_type = "absolute", threshold_type = "null")
}

# Leading number of an arm cell ("8.33 days" / "21.00 days\n(18.72 to 23.27)").
.arm_num <- function(cell) as.numeric(sub("^(-?[0-9.]+).*$", "\\1", cell))

test_that("a metacont MD outcome fills both arm columns from the control arms", {
  g  <- make_continuous()
  ft <- sof_table(g, style = "bmj", unit = "days")

  cer <- .body_col(ft, 4)
  ier <- .body_col(ft, 5)
  expect_false(cer == "-")
  expect_false(ier == "-")
  expect_match(cer, "^[0-9.]+ days$")
  expect_match(ier, "^[0-9.]+ days\n\\([0-9.]+ to [0-9.]+\\)$")

  # Control column = inverse-variance weighted mean of the control arms,
  # weights n/SD^2 (mean.c = 8, 9, 8; n.c = 50, 60, 70; sd.c = 10, 11, 12).
  w <- c(50, 60, 70) / c(10, 11, 12)^2
  expect_equal(.arm_num(cer), sum(w * c(8, 9, 8)) / sum(w), tolerance = 1e-3)

  # Intervention column = control column + pooled mean difference.
  expect_equal(.arm_num(ier), .arm_num(cer) + g$meta$TE.random,
               tolerance = 1e-2)

  # The footnote says where the two came from.
  expect_match(.footer_text(ft), "inverse-variance weighted mean of the control arms",
               fixed = TRUE)
})

test_that("a metacont SMD outcome is re-expressed on the outcome's own scale", {
  g  <- make_continuous_smd()
  ft <- sof_table(g, style = "bmj", unit = "days")

  cer <- .body_col(ft, 4)
  ier <- .body_col(ft, 5)
  expect_match(cer, "^[0-9.]+ days$")
  expect_match(ier, "^[0-9.]+ days\n\\([0-9.]+ to [0-9.]+\\)$")

  # An SMD cannot be added to a mean as it stands: it is multiplied by the
  # pooled within-arm SD of the control arms first.
  n_c    <- c(50, 60, 70); sd_c <- c(10, 11, 12)
  sd_ref <- sqrt(sum((n_c - 1) * sd_c^2) / sum(n_c - 1))
  expect_equal(.control_reference_sd(g$meta), sd_ref, tolerance = 1e-9)
  expect_equal(.arm_num(ier), .arm_num(cer) + g$meta$TE.random * sd_ref,
               tolerance = 1e-2)

  # The Difference column stays in SD units and must not borrow the outcome's
  # unit, or the reader would subtract the arm columns and find it disagrees.
  d <- .body_col(ft, 6)
  expect_match(d, "^[0-9.]+ more standard deviations \\(")
  expect_no_match(d, "days", fixed = TRUE)
  expect_gt(abs((.arm_num(ier) - .arm_num(cer)) - .arm_num(d)), 1)

  expect_match(.footer_text(ft), "re-expressed in the outcome's own units",
               fixed = TRUE)
  expect_match(.footer_text(ft), sprintf("SD = %.2f", sd_ref), fixed = TRUE)
})

test_that("the gradepro layout fills the same columns and relabels them", {
  g  <- make_continuous()
  ft <- sof_table(g, unit = "days")

  # "Risk with control (per 1,000)" would misdescribe a mean.
  hdrs <- names(ft$body$dataset)
  expect_identical(hdrs[3:4], c("With control", "With intervention"))
  expect_match(.body_col(ft, 3), "^[0-9.]+ days$")
  expect_match(.body_col(ft, 4), "^[0-9.]+ days\n\\([0-9.]+; [0-9.]+\\)$")
  expect_equal(.arm_num(.body_col(ft, 4)),
               .arm_num(.body_col(ft, 3)) + g$meta$TE.random, tolerance = 1e-2)

  # The rate sentence goes with the rates.
  expect_no_match(.footer_text(ft), "computed from baseline risk", fixed = TRUE)

  # A binary outcome keeps the GRADEpro vocabulary untouched.
  hb <- names(sof_table(make_binary())$body$dataset)
  expect_identical(hb[3:4], c("Risk with control\n(per 1,000)",
                             "Risk with intervention\n(per 1,000)"))
})

test_that("grade_table carries the continuous arm columns in both styles", {
  g <- make_continuous()

  bmj <- grade_table(list("Sleep duration" = g), style = "bmj", unit = "days")
  expect_match(.body_col(bmj, 4), "^[0-9.]+ days$")
  expect_match(.body_col(bmj, 5), "^[0-9.]+ days\n\\(")
  # The note is written once, however many rows it covers.
  expect_equal(
    sum(grepl("inverse-variance weighted mean",
              as.character(bmj$footer$dataset[[1]]), fixed = TRUE)),
    1L)

  gp <- grade_table(list("Sleep duration" = g), unit = "days")
  expect_identical(names(gp$body$dataset)[3:4],
                   c("With control", "With intervention"))
  expect_match(.body_col(gp, 3), "^[0-9.]+ days$")
})

test_that("without a unit the arm cells are bare numbers", {
  ft <- sof_table(make_continuous(), style = "bmj")
  expect_match(.body_col(ft, 4), "^[0-9.]+$")
  expect_match(.body_col(ft, 5), "^[0-9.]+\n\\([0-9.]+ to [0-9.]+\\)$")
})

test_that("the Chinn responder rates still win over the derived arm means", {
  g  <- make_continuous()
  ft <- sof_table(g, style = "bmj", convert_smd_to_or = TRUE,
                  baseline_risk = 0.3, unit = "days")

  # Chinn supplies a different quantity (a proportion above a threshold), and
  # it keeps its own cells and its own footnote.
  expect_identical(.body_col(ft, 4), "300 per 1000 *")
  expect_match(.body_col(ft, 5), "per 1000\n\\([0-9]+ to [0-9]+\\) \\*$")
  expect_no_match(.footer_text(ft), "inverse-variance weighted mean",
                  fixed = TRUE)
  expect_match(.footer_text(ft), "Chinn's formula", fixed = TRUE)
})

test_that("an object with no arm-level data still falls back to '-'", {
  g <- make_no_refit()          # metagen: no mean.c / sd.c / n.c
  ft <- sof_table(g, style = "bmj")
  expect_identical(.body_col(ft, 4), "-")
  expect_identical(.body_col(ft, 5), "-")
  expect_null(.pooled_control_mean(g$meta))
})

test_that("unusable control SDs fall back to sample-size weighting", {
  m <- meta::metacont(
    n.e = c(50, 60), mean.e = c(20, 22), sd.e = c(10, 11),
    n.c = c(50, 60), mean.c = c(8, 9),   sd.c = c(10, 11),
    studlab = c("A", "B"), sm = "MD"
  )
  m$sd.c <- c(NA_real_, NA_real_)
  got <- .pooled_control_mean(m)
  expect_identical(got$weighting, "sample-size")
  expect_equal(got$mean, (50 * 8 + 60 * 9) / 110, tolerance = 1e-9)
  expect_match(.cont_arm_note("sample-size"), "sample-size weighted mean",
               fixed = TRUE)
})

# --- an overridden domain reaches the footer (0.5.1) ------------------------
#
# The bug this closes: a reviewer overrode publication bias, the certainty cell
# and the "Due to ..." sentence moved with them, and the footnote underneath
# went on reciting the automatic reasoning - because it is built from
# domain_facts, which record what the ALGORITHM found and are not rewritten by
# an override. The fix is keyed on the "Manual override" head that
# make_domain_row() writes, so it works for every domain rather than for the
# one that was reported.

# The shape the Shiny app writes when a reviewer sets a domain by hand: the
# override clause, the separator, and the automatic note left underneath it.
.override_domain <- function(g, domain, judgment, rationale) {
  idx <- which(g$domain_assessments$domain == domain)
  g$domain_assessments$judgment[idx]  <- judgment
  g$domain_assessments$auto[idx]      <- FALSE
  g$domain_assessments$downgrade[idx] <- -1L
  g$domain_assessments$notes[idx] <- paste0(
    sprintf("Manual override (%s): %s", judgment, rationale),
    " | ", g$domain_assessments$notes[idx])
  g
}

test_that("an overridden domain's footnote states the reviewer's rationale", {
  g <- .override_domain(make_binary(), "Publication bias", "serious",
                        "Two registered trials with no results posted")
  footer <- .footer_text(sof_table(g, style = "bmj"))

  expect_match(footer, "Two registered trials with no results posted",
               fixed = TRUE)
  expect_match(footer, "not by the algorithm", fixed = TRUE)
  # The automatic numbers are kept, but named as what they are rather than
  # left standing as the justification for a rating they did not produce.
  expect_match(footer, "The automatic assessment recorded", fixed = TRUE)

  # Only the override clause travels into the footnote, not the flowchart
  # prose behind the "|", which is written for a reader following Figure 5 and
  # is far too long for a table footer. Asserted on the footnote itself: the
  # BMJ footer also carries the separate publication-bias qualitative-
  # assessment sentence, which has always quoted the note in full.
  expect_no_match(.domain_fact_note(g, "Publication bias"), "Q1:",
                  fixed = TRUE)
})

test_that("a domain with no facts still gets a footnote once overridden", {
  # Indirectness emits no facts at all, so before this it was the one domain
  # that could rate the evidence down and explain nothing.
  g <- .override_domain(make_binary(), "Indirectness", "serious",
                        "Surrogate outcome only")
  footer <- .footer_text(sof_table(g, style = "bmj"))

  expect_true("Indirectness" %in% .rated_down_fact_domains(g))
  expect_match(footer, "Surrogate outcome only", fixed = TRUE)
  expect_no_match(footer, "The automatic assessment recorded", fixed = TRUE)
})

test_that("an automatic rating's footnote is unchanged", {
  g <- make_refit()
  footer <- .footer_text(sof_table(g, style = "bmj"))
  expect_no_match(footer, "not by the algorithm", fixed = TRUE)
  expect_no_match(footer, "The automatic assessment recorded", fixed = TRUE)
})

test_that("the override head is read from the notes, not inferred from auto", {
  # auto = FALSE also means "the reviewer supplied an input the algorithm
  # cannot compute" - an answered pubias_small_industry, say - where the
  # flowchart still decided the rating and the facts still explain it.
  expect_null(.parse_override_note("Manual override (serious): why",
                                   auto = TRUE))
  expect_null(.parse_override_note("Q1: answered by hand -> serious.",
                                   auto = FALSE))
  expect_null(.parse_override_note(NA_character_, auto = FALSE))

  parsed <- .parse_override_note("Manual override (serious): why | Q1: auto.",
                                 auto = FALSE)
  expect_identical(parsed$judgment, "serious")
  expect_identical(parsed$rationale, "why")
})
