# One resolver for the review's own arm names, shared by Step 3 and Step 4.
#
# It used to be a closure inside step4_server(), which is why the Step 3
# Summary of Findings preview rendered "With control" and a Core GRADE 6 Box 1
# subject of "Treatment" one screen before Step 4's combined table rendered the
# arm values the reviewer had picked in Step 2 -- the same table, naming the
# arms two ways.

test_that("pma_arm_labels() falls back to the pmatools defaults", {
  # The fallbacks are pmatools' own defaults for label_intervention /
  # label_control, so an unnamed analysis renders exactly as the package does
  # and nothing about the untouched path moves.
  default <- list(intervention = "intervention", control = "control")
  expect_identical(pma_arm_labels(list()), default)
  expect_identical(pma_arm_labels(list(arm_e = NULL, arm_c = "")), default)
  expect_identical(pma_arm_labels(list(arm_e = NA_character_)), default)
  expect_identical(pma_arm_labels(NULL), default)
})

test_that("pma_arm_labels() returns the Step 2 arm values when they exist", {
  expect_identical(pma_arm_labels(list(arm_e = "CBT-I", arm_c = "placebo")),
                   list(intervention = "CBT-I", control = "placebo"))
  # One side named and not the other is a real state -- the reviewer picks two
  # selects -- and each falls back on its own.
  expect_identical(pma_arm_labels(list(arm_e = "CBT-I")),
                   list(intervention = "CBT-I", control = "control"))
})

test_that("the Step 3 preview asks sof_table() for the same labels", {
  # Source-level, because output$sof_preview needs a Shiny session this suite
  # does not have. What it pins is that the call site passes them at all: it
  # was the one sof_table() call in the app that did not, which is the whole
  # bug.
  src <- paste(readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                         warn = FALSE), collapse = "\n")
  preview <- regmatches(src, regexpr(
    "(?s)output\\$sof_preview <- shiny::renderUI\\(\\{.*?outputOptions",
    src, perl = TRUE))
  expect_true(nzchar(preview))
  expect_match(preview, "pma_arm_labels(state)", fixed = TRUE)
  expect_match(preview, "label_intervention = arms$intervention", fixed = TRUE)
  expect_match(preview, "label_control      = arms$control", fixed = TRUE)
})

test_that("the notes under the table name the same arms as its headers", {
  # A footnote that calls a column something the header does not is a footnote
  # about a different table. "the value with X" mirrors the column head
  # "With X"; the old "<label>-group value" shape does not survive a free-text
  # label ("CBT-I-group value").
  arms <- list(intervention = "CBT-I", control = "placebo")
  note <- pma_sof_limitations_note(arms)
  expect_match(note, "the value with placebo, the value with CBT-I",
               fixed = TRUE)
  expect_no_match(note, "control-group value", fixed = TRUE)

  # Default output is the wording it always had.
  expect_match(pma_sof_limitations_note(),
               "the value with control, the value with intervention",
               fixed = TRUE)
  expect_match(pma_sof_limitations_note(), "Arm-level values", fixed = TRUE)
})

test_that("CER and EER keep their acronyms and gain their columns", {
  # Deliberately NOT substituted into "control event rate" / "intervention
  # event rate": CER and EER are the cited source's own acronyms and stop
  # deriving from the words the moment the words change. What the reviewer
  # needs is to find the columns, so the columns are named instead.
  note <- pma_sof_cer_eer_note(list(intervention = "CBT-I",
                                    control = "placebo"))
  expect_match(note, "control event rate (CER", fixed = TRUE)
  expect_match(note, "intervention event rate (EER", fixed = TRUE)
  expect_match(note, '"With placebo" column', fixed = TRUE)
  expect_match(note, '"With CBT-I" column', fixed = TRUE)
})
