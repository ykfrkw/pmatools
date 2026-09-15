# pma_question_note() (R/sof_display.R) - the Summary of Findings footer line
# that says which of the four clinical questions the certainty rating answered.
#
# Two things are asserted here, and only the second is about wording.
#
# The first is PROVENANCE. A Low rating means something different depending on
# the claim it is a rating in, and the footnote is the only place a reader of
# the table learns which claim that was. So every number in it has to be the
# number the rating used: the threshold comes off the rated object, never off
# the Configuration tab's live inputs, and which sides were tested comes off
# `threshold_sides` rather than being re-derived from the question. The tests
# below build the objects with REAL grade_meta() calls wherever the fact under
# test is one grade_meta() decides - which side Fig 2 put the rating against,
# what it stored - and with stubs only where the fact is the footnote's own
# formatting.
#
# The second is that the word "MID" is in none of it. That audit lives in
# test-step3-threshold.R, over every string the feature can emit, including
# PMA_QUESTION_NOTE_QUESTIONS from this file.

library(testthat)

# The one fit every question is rated on, so a difference between two notes is
# a difference in the question and not in the data. Modest event rates and a
# clear effect, which puts the interval beyond the 1.20 threshold - the
# interesting case, because it is where the four questions disagree.
pma_note_fit <- function() {
  meta::metabin(
    event.e = c(10, 12, 9), n.e = c(100, 110, 90),
    event.c = c(20, 22, 19), n.c = c(100, 110, 90),
    sm = "RR", random = TRUE, common = FALSE)
}

# A rating of that fit, for one question, through the app's own mapping. `...`
# overrides any of the fixed arguments, so a test that cares about the
# threshold can name a different one without the default being passed twice.
pma_note_rating <- function(pkg, question, ...) {
  fixed <- list(small_values = "desirable", threshold = 1.20,
                threshold_scale = "ratio", pubias_unpublished = "no")
  extra <- list(...)
  fixed <- fixed[setdiff(names(fixed), names(extra))]
  suppressWarnings(do.call(pkg$grade_meta, c(
    list(pma_note_fit()), fixed, pma_question_grade_args(question), extra)))
}

pma_skip_without_bundle <- function() {
  skip_if_not_installed("meta")
  pkg <- pma_vendored_pkg()
  skip_if(is.null(pkg), "no staged bundle - run Rscript shiny/stage_bundle.R")
  pkg
}

test_that("the note names the question that was rated, per question", {
  pkg <- pma_skip_without_bundle()

  for (q in PMA_CLINICAL_QUESTIONS) {
    note <- pma_question_note(pma_note_rating(pkg, q))
    expect_true(is.character(note) && length(note) == 1L, info = q)
    expect_match(note, "^Question rated: ", info = q)
    # The four are told apart by the prose question, not by a bare name: the
    # footnote is read alone, under a table, possibly in a .docx a year later.
    expect_match(note, PMA_QUESTION_NOTE_QUESTIONS[[q]], fixed = TRUE, info = q)
    # And every one of them ends with what the rating is a rating IN, which is
    # the sentence the whole footnote exists for.
    expect_match(note,
                 "Certainty is rated in that claim, not in the size of the effect.",
                 fixed = TRUE, info = q)
  }
})

test_that("only non-inferiority says one side, and it names which", {
  pkg <- pma_skip_without_bundle()

  ni <- pma_question_note(pma_note_rating(pkg, "non_inferiority"))
  expect_match(ni, "tested on the worse side only", fixed = TRUE)
  # small_values = "desirable" means a small value is good, so larger is worse
  # and the worse side is the high one. The same reading .threshold_worse_sign()
  # gives the rating, not a second one.
  expect_match(ni, "higher values of this outcome are the worse ones",
               fixed = TRUE)

  # The mirror, from the same fit with the direction answered the other way.
  ni_lo <- pma_question_note(suppressWarnings(do.call(pkg$grade_meta, c(
    list(pma_note_fit(), small_values = "undesirable", threshold = 1.20,
         threshold_scale = "ratio", pubias_unpublished = "no"),
    pma_question_grade_args("non_inferiority")))))
  expect_match(ni_lo, "lower values of this outcome are the worse ones",
               fixed = TRUE)

  # Everything else tests both sides and must not claim otherwise.
  for (q in c("important_superiority", "equivalence")) {
    note <- pma_question_note(pma_note_rating(pkg, q))
    expect_match(note, "tested on both sides", fixed = TRUE, info = q)
    expect_false(grepl("worse side", note, fixed = TRUE), info = q)
  }
})

test_that("superiority reports no threshold when the rating used none", {
  pkg <- pma_skip_without_bundle()

  # threshold_type = "null" with a point estimate well away from the null:
  # Fig 2 settles on the non-null-effect target, whose threshold is the null.
  # The footnote must not print the 1.20 that was passed but never used.
  sup <- pma_note_rating(pkg, "superiority")
  expect_identical(sup$rating_target, "non_null_effect")
  note <- pma_question_note(sup)
  expect_match(note, "The rating is against the null, so no threshold was used.",
               fixed = TRUE)
  expect_false(grepl("1.2", note, fixed = TRUE))
  expect_false(grepl("Threshold used", note, fixed = TRUE))
})

test_that("superiority says so when Fig 2 read the threshold after all", {
  pkg <- pma_skip_without_bundle()

  # The near-null branch: threshold_type = "null", but the pooled estimate sits
  # inside the threshold, so Core GRADE 2 Fig 2 switches the target to little
  # or no difference and imprecision IS judged against the threshold. A
  # footnote saying "no threshold was used" would then be false, and the
  # reviewer had every reason to believe it.
  flat <- meta::metabin(
    event.e = c(20, 22, 19), n.e = c(400, 440, 380),
    event.c = c(20, 22, 19), n.c = c(400, 440, 380),
    sm = "RR", random = TRUE, common = FALSE)
  g <- suppressWarnings(do.call(pkg$grade_meta, c(
    list(flat, small_values = "desirable", threshold = 1.20,
         threshold_scale = "ratio", pubias_unpublished = "no"),
    pma_question_grade_args("superiority"))))
  expect_identical(g$rating_target, "little_to_no_difference")

  note <- pma_question_note(g)
  expect_match(note, "superiority - is there any effect at all?", fixed = TRUE)
  expect_match(note, "Threshold used: RR 1.2", fixed = TRUE)
  expect_match(note, "read the threshold after all", fixed = TRUE)
})

test_that("the threshold in the note is the one the rating was given", {
  pkg <- pma_skip_without_bundle()

  # Not 1.20: a different number, so a hard-coded or re-derived value cannot
  # pass by coincidence.
  g <- pma_note_rating(pkg, "equivalence", threshold = 1.35)
  note <- pma_question_note(g)
  expect_match(note, "Threshold used: RR 1.35", fixed = TRUE)
  expect_false(grepl("1.2", note, fixed = TRUE))

  # And the measure travels with it, so "1.35" cannot be read as a risk
  # difference.
  expect_match(note, "RR 1.35", fixed = TRUE)
})

test_that("an absolute threshold is reported per N, in the unit asked for", {
  pkg <- pma_skip_without_bundle()

  # threshold_scale = "ard" is the one route on which the object itself knows
  # the absolute value, and Core GRADE 2 asks for the absolute scale, so that
  # is the form the footnote prefers.
  g <- suppressWarnings(do.call(pkg$grade_meta, c(
    list(pma_note_fit(), small_values = "desirable", threshold = 0.05,
         threshold_scale = "ard", pubias_unpublished = "no"),
    pma_question_grade_args("equivalence"))))
  expect_equal(g$threshold_ard, 0.05)

  expect_match(pma_question_note(g), "Threshold used: 50 per 1,000",
               fixed = TRUE)
  # The display unit the Configuration tab is on, so the footnote and the
  # table cannot print two denominators for one number.
  expect_match(pma_question_note(g, per = 100L), "Threshold used: 5 per 100",
               fixed = TRUE)
})

test_that("a caller may name the question rather than have it recovered", {
  pkg <- pma_skip_without_bundle()

  g <- pma_note_rating(pkg, "equivalence")
  expect_identical(pma_question_note(g),
                   pma_question_note(g, question = "equivalence"))
  # The argument is what the Step 4 loop uses when it already knows, and it
  # must actually be read rather than ignored.
  expect_match(pma_question_note(g, question = "non_inferiority"),
               "non-inferiority", fixed = TRUE)
})

test_that("the note is NULL for anything that was not rated", {
  # It is dropped straight into a flextable footer, so the NULL cases matter as
  # much as the wording. threshold_type is the marker: grade_meta() has always
  # stored it, and a hand-built stub does not carry one.
  expect_null(pma_question_note(NULL))
  expect_null(pma_question_note(list()))
  expect_null(pma_question_note(structure(list(certainty = "Moderate"),
                                          class = "pmatools")))
  # pma_sof_add_notes() drops a NULL without being asked to, which is what
  # makes the call site a one-liner at all three surfaces.
  expect_null(pma_sof_add_notes(NULL, pma_question_note(NULL)))
})

test_that("a pre-0.5.1 object still gets a footnote, and the right one", {
  # None of the three new fields exists on an object rated before the feature,
  # so the note has to describe the question the app was asking then rather
  # than refusing to describe anything.
  legacy <- list(threshold_type = "mid", rating_target = "important_effect",
                 rating_target_auto = TRUE, threshold = 1.25,
                 meta = list(sm = "OR"))
  note <- pma_question_note(legacy)
  expect_match(note, "clinically important superiority", fixed = TRUE)
  expect_match(note, "Threshold used: OR 1.25, tested on both sides.",
               fixed = TRUE)
})
