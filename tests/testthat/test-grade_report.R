library(testthat)

skip_if_not_installed("meta")

make_metabin_gr <- function() {
  meta::metabin(
    event.e = c(10, 15, 20),
    n.e     = c(50, 60, 70),
    event.c = c(15, 20, 25),
    n.c     = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm      = "OR",
    method  = "Inverse"
  )
}

make_outcomes_gr <- function() {
  m  <- make_metabin_gr()
  g1 <- suppressWarnings(grade_meta(m, study_design = "RCT", rob = "no",
                                    small_values = "desirable",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Outcome 1", threshold_type = "null"))
  g2 <- suppressWarnings(grade_meta(m, study_design = "RCT", rob = "some_concerns",
                                    small_values = "desirable",
                                    rob_rationale = "Consensus RoB2: some concerns overall",
                                    indirectness = "no",
                                    outcome_name = "Outcome 2", threshold_type = "null"))
  list("Outcome 1" = g1, "Outcome 2" = g2)
}

test_that("grade_report writes a markdown report", {
  outcomes <- make_outcomes_gr()
  out_dir  <- tempfile("grade_report_md_")

  paths <- grade_report(outcomes,
                        primary     = "Outcome 1",
                        format      = "md",
                        output_dir  = out_dir,
                        output_file = "report_test")

  expect_length(paths, 1L)
  expect_true(file.exists(paths[[1]]))
  expect_match(paths[[1]], "\\.md$")
  content <- readLines(paths[[1]], warn = FALSE)
  expect_true(any(grepl("Summary of Findings", content)))
  expect_true(any(grepl("Outcome 1", content)))
  expect_true(any(grepl("Domain-by-Domain Rationale", content)))
})

test_that("grade_report writes a docx report", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  outcomes <- make_outcomes_gr()
  out_dir  <- tempfile("grade_report_docx_")

  paths <- grade_report(outcomes,
                        primary     = "Outcome 1",
                        format      = "docx",
                        output_dir  = out_dir,
                        output_file = "report_test")

  expect_length(paths, 1L)
  expect_true(file.exists(paths[[1]]))
  expect_match(paths[[1]], "\\.docx$")
  expect_gt(file.size(paths[[1]]), 0)
})

test_that("grade_report handles multiple formats in one call", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  outcomes <- make_outcomes_gr()
  out_dir  <- tempfile("grade_report_multi_")

  paths <- grade_report(outcomes,
                        format      = c("md", "docx"),
                        output_dir  = out_dir,
                        output_file = "report_multi")

  expect_length(paths, 2L)
  expect_true(all(file.exists(paths)))
})

test_that("grade_report rejects non-pmatools outcome lists", {
  expect_error(grade_report(list(a = 1, b = 2)),
               regexp = "pmatools")
})
