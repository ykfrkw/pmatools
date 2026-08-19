library(testthat)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

make_common_only_grade <- function() {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = "OR",
               random = FALSE, common = TRUE)
  g  <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    small_values = "desirable",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Common only", threshold_type = "null"))
  g
}

.footer_text <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")

test_that(".pooled_estimate falls back to common when random is absent", {
  g <- make_common_only_grade()
  pooled <- .pooled_estimate(g$meta)
  expect_true(is.finite(pooled$est))
  expect_equal(pooled$est, g$meta$TE.common)
  expect_equal(pooled$lower, g$meta$lower.common)
})

test_that("sof_table shows the effect for a common-effect-only analysis", {
  g  <- make_common_only_grade()
  ft <- sof_table(g)
  eff <- ft$body$dataset[[grep("^Relative effect", names(ft$body$dataset))]]
  expect_false(identical(eff, "NR"))
  expect_match(eff, "^OR ")
  # Intervention rate column derived from the common-effect pool, not "-"
  ier <- ft$body$dataset[[grep("^Risk with intervention",
                               names(ft$body$dataset))]]
  expect_false(identical(ier, "-"))
})

test_that("sof_table uses GRADEpro-aligned column headers", {
  g  <- make_common_only_grade()
  ft <- sof_table(g)
  hdrs <- names(ft$body$dataset)
  expect_identical(hdrs, c(
    "Outcome",
    "No. of participants\n(studies)",
    "Risk with control\n(per 1,000)",
    "Risk with intervention\n(per 1,000)",
    "Relative effect\n(95% CI)",
    "Certainty of the evidence\n(Core GRADE series)"
  ))
  # No legacy experimental-rate vocabulary anywhere
  expect_no_match(paste(hdrs, collapse = " "), "Exp\\. rate")
  # Combined participants (studies) cell: "330 (3 RCTs)"
  np <- ft$body$dataset[[hdrs[2]]]
  expect_match(np, "^360 \\(3 RCTs\\)$")
  # Footnote uses the Intervention rate wording
  expect_match(.footer_text(ft), "Intervention rate")
  expect_no_match(.footer_text(ft), "Exp\\. rate")
})

test_that("sof_table honours custom arm labels", {
  g  <- make_common_only_grade()
  ft <- sof_table(g, label_intervention = "CBT-I", label_control = "placebo")
  hdrs <- names(ft$body$dataset)
  expect_true("Risk with placebo\n(per 1,000)" %in% hdrs)
  expect_true("Risk with CBT-I\n(per 1,000)" %in% hdrs)
})

test_that("the arm labels reach the footnotes, not only the headers", {
  # They reached the headers from the start and stopped there, so one table
  # could name the arms two ways: "Risk with placebo" over a column, and
  # "intervention-arm event rate" in the sentence describing that column.
  g   <- make_common_only_grade()
  ft  <- sof_table(g, label_intervention = "CBT-I", label_control = "placebo")
  txt <- .footer_text(ft)
  expect_match(txt, "CBT-I rate (Risk with CBT-I)", fixed = TRUE)
  expect_match(txt, "CBT-I-arm event rate", fixed = TRUE)
  expect_no_match(txt, "Intervention rate (Risk with", fixed = TRUE)
  expect_no_match(txt, "intervention-arm event rate", fixed = TRUE)
})

test_that("the default footnote wording is byte-identical to before", {
  # The whole substitution is a no-op at the defaults. That is what keeps a
  # review which never named its arms producing the table it always did, and
  # it is why every other test in this file could stay untouched.
  g <- make_common_only_grade()
  expect_match(
    .footer_text(sof_table(g)),
    "Intervention rate (Risk with intervention) = intervention-arm event rate",
    fixed = TRUE)
})

test_that("a label starting a sentence is capitalised, an acronym is not", {
  # Labels are free text a reviewer typed. Only the first character moves:
  # toupper() on the whole string would shout "CBT-I" back as "CBT-I" and
  # "usual care" as "USUAL CARE".
  expect_identical(pmatools:::.arm_label_cap("placebo"), "Placebo")
  expect_identical(pmatools:::.arm_label_cap("CBT-I"), "CBT-I")
  expect_identical(pmatools:::.arm_label_cap("usual care"), "Usual care")
  expect_identical(pmatools:::.arm_label_cap(""), "")

  # And the sentence SUBJECT falls back to the generic word, because the
  # package default is a column label rather than something a sentence can be
  # about: "OR > 1 = intervention better" is not a sentence anyone writes.
  expect_identical(pmatools:::.arm_subject("intervention"), "treatment")
  expect_identical(pmatools:::.arm_subject("intervention", "Treatment"),
                   "Treatment")
  expect_identical(pmatools:::.arm_subject("CBT-I"), "CBT-I")
})

test_that("grade_table shows the effect for a common-effect-only analysis", {
  g  <- make_common_only_grade()
  ft <- grade_table(list("Common only" = g))
  eff <- ft$body$dataset[[grep("^Relative effect", names(ft$body$dataset))]]
  expect_false(any(eff == "NR"))
})

test_that("grade_table uses GRADEpro-aligned column headers", {
  g  <- make_common_only_grade()
  ft <- grade_table(list("Common only" = g))
  hdrs <- names(ft$body$dataset)
  expect_identical(hdrs[1:6], c(
    "Outcome",
    "No. of participants\n(studies)",
    "Risk with control\n(per 1,000)",
    "Risk with intervention\n(per 1,000)",
    "Relative effect\n(95% CI)",
    "Certainty of the evidence\n(Core GRADE series)"
  ))
  np <- ft$body$dataset[[hdrs[2]]]
  expect_match(np, "^360 \\(3 RCTs\\)$")
})

test_that("random-effects pool still preferred when available", {
  m <- meta::metabin(
    event.e = c(10, 15, 20), n.e = c(50, 60, 70),
    event.c = c(15, 20, 25), n.c = c(50, 60, 70),
    studlab = c("A", "B", "C"), sm = "OR",
    random = TRUE, common = TRUE, method = "Inverse"
  )
  pooled <- .pooled_estimate(m)
  expect_equal(pooled$est, m$TE.random)
})

# --- Publication bias qualitative-assessment note propagation ---------------

test_that("qualitative pubias note propagates to SoF and grade_table footers", {
  g <- make_common_only_grade()  # k = 3 < 10, pubias_unpublished not given
  expect_false(is.null(.pubias_qualitative_note(g)))

  ft_sof <- sof_table(g)
  expect_match(.footer_text(ft_sof), "QUALITATIVE ASSESSMENT REQUIRED")

  ft_gt <- grade_table(list("Common only" = g))
  expect_match(.footer_text(ft_gt), "QUALITATIVE ASSESSMENT REQUIRED")
})

test_that("evidence_profile flags publication bias as not formally assessed", {
  g  <- make_common_only_grade()
  ft <- evidence_profile(g)
  other <- ft$body$dataset[["Other considerations"]]
  expect_match(other, "publication bias not formally assessed")
  expect_match(.footer_text(ft), "QUALITATIVE ASSESSMENT REQUIRED")
})

test_that("no qualitative note when pubias was decided manually", {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = "OR")
  g  <- suppressWarnings(grade_meta(ma, pubias_unpublished = "no",
                                    small_values = "desirable",
                                    outcome_name = "Manual", threshold_type = "null"))
  expect_null(.pubias_qualitative_note(g))
  ft <- sof_table(g)
  expect_no_match(.footer_text(ft), "QUALITATIVE ASSESSMENT REQUIRED")
})
