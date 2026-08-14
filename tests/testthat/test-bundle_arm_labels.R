# Arm labels reach the generated multi-outcome analysis.R.
#
# The bundle's summary_of_findings.docx is drawn with the review's own arm
# names; the script that rebuilds it has to ask for the same ones, or it
# reproduces every number of the shipped table under "With control" /
# "With intervention".

.labels_test_set <- function() {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    outcome = "Insomnia severity",
    n       = c(50, 50, 60, 60, 70, 70),
    mean    = c(20, 8, 22, 9, 21, 8),
    sd      = c(10, 10, 11, 11, 12, 12),
    stringsAsFactors = FALSE
  )
  suppressWarnings(grade_meta_multi(
    run_ma_multi(data, sm = "MD"),
    common = list(study_design = "RCT", rob = "no",
                  rob_rationale = "Consensus RoB2: all domains low risk",
                  indirectness = "no", outcome_type = "absolute",
                  threshold_type = "null")))
}

test_that("the bundled analysis.R asks grade_table() for the review's arms", {
  skip_if_not_installed("zip")

  set <- .labels_test_set()
  out_dir <- withr::local_tempdir()
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "labels",
                  include = c("sof", "script"),
                  label_intervention = "CBT-I",
                  label_control      = "placebo"))
  unpacked <- file.path(out_dir, "unpacked")
  zip::unzip(zip_path, exdir = unpacked)

  script <- paste(readLines(file.path(unpacked, "analysis.R")), collapse = "\n")
  expect_match(script, 'label_intervention = "CBT-I"', fixed = TRUE)
  expect_match(script, 'label_control      = "placebo"', fixed = TRUE)

  # And the rebuilt table really does carry the headers, i.e. the arguments
  # landed on the grade_table() call rather than somewhere else in the script.
  expect_match(script, "grade_table(set", fixed = TRUE)
  ft <- grade_table(set, style = "bmj", per = 1000, prediction = FALSE,
                    label_intervention = "CBT-I", label_control = "placebo")
  hdrs <- names(ft$body$dataset)
  expect_true(any(grepl("placebo", hdrs, fixed = TRUE)))
  expect_true(any(grepl("CBT-I", hdrs, fixed = TRUE)))
})

test_that("default arm labels leave the generated grade_table() call alone", {
  set <- .labels_test_set()
  out_dir <- withr::local_tempdir()
  script_path <- file.path(out_dir, "analysis.R")
  pmatools:::.render_analysis_script_multi(set, per = 1000, prediction = FALSE,
                                           style = "bmj",
                                           out_path = script_path)
  script <- paste(readLines(script_path), collapse = "\n")
  expect_false(grepl("label_intervention", script, fixed = TRUE))
  expect_false(grepl("label_control", script, fixed = TRUE))

  expect_identical(pmatools:::.sof_arm_label_args(), "")
  expect_identical(
    pmatools:::.sof_arm_label_args("intervention", "control"), "")
})

test_that("one non-default arm label is rendered on its own", {
  expect_identical(
    pmatools:::.sof_arm_label_args("CBT-I", "control"),
    paste0(",\n", strrep(" ", 19L), 'label_intervention = "CBT-I"'))
  expect_identical(
    pmatools:::.sof_arm_label_args("intervention", "placebo"),
    paste0(",\n", strrep(" ", 19L), 'label_control      = "placebo"'))
})

test_that("a free-text arm label survives as a parseable literal", {
  set <- .labels_test_set()
  out_dir <- withr::local_tempdir()
  script_path <- file.path(out_dir, "analysis.R")
  # An apostrophe is what breaks a shQuote()-style literal.
  pmatools:::.render_analysis_script_multi(
    set, per = 1000, prediction = FALSE, style = "bmj",
    label_intervention = "clinicians' usual care",
    label_control      = "waiting list",
    out_path = script_path)

  script <- paste(readLines(script_path), collapse = "\n")
  expect_match(script, "clinicians' usual care", fixed = TRUE)
  expect_silent(parse(text = script))
})
