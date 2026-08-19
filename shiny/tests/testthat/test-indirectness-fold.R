# The Indirectness worst-case fold and its rationale gate
# (R/step3_threshold.R).
#
# The overall rating ships PRESELECTED. Before that it shipped blank, and
# blank was how a reviewer said "accept the fold of the four PICO answers" -
# so the rationale gate could key on "is anything selected at all?" and be
# right. Preselecting turns that blank into a real answer, and a gate that
# still asked "is anything selected?" would demand a written reason for a
# default nobody chose. These tests hold the replacement to that: the demand
# fires on a genuine departure from the fold, and on nothing else.

test_that("the fold takes the most severe answered level", {
  expect_identical(
    step3_indir_worst_case(rep("not_serious", 4L)), "not_serious")
  expect_identical(
    step3_indir_worst_case(c("not_serious", "serious", "not_serious",
                             "not_serious")),
    "serious")

  # The regression this fold was rewritten for. Its predecessor ranked
  # "no" / "some_concerns" / "serious", which is the vocabulary 0.5.1
  # replaced: every level the PICO answers actually produce missed the table,
  # so four answers containing a "No" folded to nothing and the domain was
  # reported as folding to "not serious".
  expect_identical(
    step3_indir_worst_case(c("not_serious", "not_serious", "very_serious",
                             "not_serious")),
    "very_serious")
  expect_identical(
    step3_indir_worst_case(c("serious", "very_serious")), "very_serious")
  expect_identical(
    step3_indir_worst_case(c("very_serious", "extremely_serious")),
    "extremely_serious")
})

test_that("the fold reports nothing rather than guessing", {
  expect_null(step3_indir_worst_case(NULL))
  expect_null(step3_indir_worst_case(character(0)))
  expect_null(step3_indir_worst_case(NA_character_))
  # An unrecognised level is dropped, not ranked: ranking it would put a
  # judgment nobody made at the top of the fold.
  expect_null(step3_indir_worst_case(c("some_concerns", "no")))
  expect_identical(
    step3_indir_worst_case(c("some_concerns", "serious")), "serious")
})

test_that("the preselected default owes no written reason", {
  # What the tab ships with: four "Yes" answers folding to not_serious, and an
  # overall rating restating it.
  expect_identical(STEP3_INDIR_DEFAULT_LEVEL, "not_serious")
  worst <- step3_indir_worst_case(rep(STEP3_INDIR_DEFAULT_LEVEL, 4L))
  expect_identical(worst, STEP3_INDIR_DEFAULT_LEVEL)
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL,
                                              worst))

  # No fold at all (nothing answered) reads as the default, which is what
  # grade_obj() sends on the scalar path.
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL, NULL))
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL, ""))
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL,
                                              NA_character_))

  # A restatement of a downgraded fold is still a restatement.
  expect_false(step3_indir_rationale_required("serious", "serious"))
})

test_that("a genuine departure from the fold owes one", {
  # Rating down further than the four answers warrant.
  expect_true(step3_indir_rationale_required("serious", "not_serious"))
  # And rating down LESS than they warrant, which is the direction most in
  # need of a written reason: the preselected default sitting over a PICO
  # element the reviewer has just downgraded.
  expect_true(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL,
                                             "very_serious"))
  expect_true(step3_indir_rationale_required("extremely_serious", "serious"))
})

test_that("an unanswered overall rating demands nothing", {
  for (empty in list(NULL, "", character(0), NA_character_)) {
    expect_false(step3_indir_rationale_required(empty, "very_serious"))
  }
})

test_that("the Indirectness tab ships the overall rating preselected", {
  # The UI half of the same contract. step3_ui() needs a Shiny session to
  # render, so the source is read: what matters is that the radio names the
  # constant rather than a second literal that could drift away from it, and
  # that the rationale panel is gated on the fold comparison rather than on
  # "is anything selected".
  lines <- readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                     warn = FALSE)
  src <- paste(lines, collapse = "\n")

  at <- grep('radioButtons("indirectness", NULL,', lines, fixed = TRUE)
  expect_length(at, 1L)
  # The two lines that follow the widget's own: the choices, then what it
  # ships selected.
  expect_true(any(grepl("selected = STEP3_INDIR_DEFAULT_LEVEL",
                        lines[at + (1:2)], fixed = TRUE)))
  expect_false(any(grepl("selected = character(0)",
                         lines[at + (1:2)], fixed = TRUE)))

  expect_true(grepl('"output.indir_override_active === true"', src,
                    fixed = TRUE))
  expect_false(grepl("(input.indirectness || '') != ''", src, fixed = TRUE))
})
