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
  # about a different table. The rare-event caution is the note that survives
  # on every exported SoF, and it does both the things a footnote can get
  # wrong: it names the two arms in prose and it quotes a column head
  # verbatim, so it is the anchor for the invariant.
  arms <- list(intervention = "CBT-I", control = "placebo")
  rare <- list(meta = list(event.e = 9, n.e = 1000,
                           event.c = 10, n.c = 1000),
               baseline_risk = NA_real_)
  detail <- pma_rare_event_alert(rare, labels = arms)$detail
  expect_match(detail, "in the placebo arm", fixed = TRUE)
  expect_match(detail, "in the CBT-I arm", fixed = TRUE)
  expect_match(detail, '"With CBT-I" column', fixed = TRUE)
  expect_no_match(detail, "With intervention", fixed = TRUE)

  # Default output is the wording it always had.
  expect_match(pma_rare_event_alert(rare)$detail,
               '"With intervention" column', fixed = TRUE)
})

# --- the tool-description notes, deleted on purpose ------------------------
#
# pma_sof_limitations_note() and pma_sof_cer_eer_note() rode on every SoF
# table unconditionally and described the TOOL rather than this body of
# evidence: which Core GRADE 6 features pmatools has not built, and how a
# reviewer ought to present event rates. Their substance lives in README.md
# ("Limitations and future work") and in the "With <arm>" column heads the
# CER/EER note only pointed at. Reinstating either would put tool
# documentation back into every exported .docx, so both the definitions and
# the call sites are pinned absent.

test_that("the tool-description SoF notes stay deleted", {
  expect_false(exists("pma_sof_limitations_note", mode = "function"))
  expect_false(exists("pma_sof_cer_eer_note", mode = "function"))

  for (f in c("sof_display.R", "step3_grade.R", "step4_export.R")) {
    src <- paste(readLines(file.path(PMA_APP_ROOT, "R", f), warn = FALSE),
                 collapse = "\n")
    expect_no_match(src, "pma_sof_limitations_note", fixed = TRUE)
    expect_no_match(src, "pma_sof_cer_eer_note", fixed = TRUE)
  }
})
