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

test_that("the docx report starts each rationale section on its own page", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("zip")

  outcomes <- make_outcomes_gr()
  out_dir  <- tempfile("grade_report_breaks_")
  path <- grade_report(outcomes, primary = "Outcome 1", format = "docx",
                       output_dir = out_dir, output_file = "breaks")[[1]]

  ex <- tempfile(); dir.create(ex)
  zip::unzip(path, files = "word/document.xml", exdir = ex)
  xml <- paste(readLines(file.path(ex, "word", "document.xml"),
                         warn = FALSE), collapse = "")

  # "Domain-by-Domain Rationale" opens a new page because it opens a new
  # SECTION: the Summary of Findings above it is landscape and the rationale is
  # portrait, and the section break between them is a next-page break. Every
  # per-outcome section after the first then opens with an explicit page
  # break, so length(outcomes) - 1 of those. The title and "Summary of
  # Findings" take none -- the document already starts a page and a break
  # before them would open on a near-empty one.
  #
  # This is what stops a heading being stranded at the foot of a page under
  # the PREVIOUS table's footnotes. <w:keepNext/> does not: officer's heading
  # styles already carry it (in word/styles.xml, not here) and Word drops
  # keep-with-next when the block that follows cannot fit a page of its own.
  count <- function(pattern) {
    hits <- gregexpr(pattern, xml)[[1]]
    if (hits[1] == -1L) 0L else length(hits)
  }
  expect_equal(count('w:br w:type="page"'), length(outcomes) - 1L)
  # One section break inside the body (the landscape SoF), and the body's own
  # closing sectPr (the portrait rationale).
  expect_equal(count("<w:sectPr"), 2L)
})

test_that("the docx report puts the Summary of Findings on a landscape page", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("zip")

  outcomes <- make_outcomes_gr()
  out_dir  <- tempfile("grade_report_orient_")
  path <- grade_report(outcomes, primary = "Outcome 1", format = "docx",
                       output_dir = out_dir, output_file = "orient")[[1]]

  ex <- tempfile(); dir.create(ex)
  zip::unzip(path, files = "word/document.xml", exdir = ex)
  xml <- paste(readLines(file.path(ex, "word", "document.xml"),
                         warn = FALSE), collapse = "")

  # Sections in document order: the title and the SoF table, then the
  # rationale.
  orients <- regmatches(xml, gregexpr('w:orient="[a-z]+"', xml))[[1]]
  expect_equal(orients, c('w:orient="landscape"', 'w:orient="portrait"'))

  # Every table fits between the margins of the section it sits in: the SoF
  # within a landscape A4 text block, each domain table within a portrait one.
  grids <- regmatches(xml, gregexpr("<w:tblGrid>.*?</w:tblGrid>", xml))[[1]]
  widths <- vapply(grids, function(g) {
    sum(as.numeric(regmatches(g, gregexpr('(?<=w:w=")[0-9]+', g,
                                          perl = TRUE))[[1]])) / 1440
  }, numeric(1), USE.NAMES = FALSE)
  expect_lte(widths[1], .docx_text_width("landscape") + 0.01)
  expect_true(all(widths[-1] <= .docx_text_width("portrait") + 0.01))
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

test_that("the markdown report names the review's own arms", {
  # grade_report() has taken label_intervention / label_control since they
  # existed and threaded them into the .docx branch. The markdown branch never
  # received them: .md_sof_table() built "Control rate (per 1,000)" /
  # "Intervention rate (per 1,000)" from literals, so a review whose arms were
  # "CBT-I" and "placebo" got generic headers over its own numbers.
  outcomes <- make_outcomes_gr()
  out_dir  <- tempfile("grade_report_arms_")

  paths <- grade_report(outcomes,
                        primary            = "Outcome 1",
                        format             = "md",
                        output_dir         = out_dir,
                        output_file        = "report_arms",
                        label_intervention = "CBT-I",
                        label_control      = "placebo")
  content <- paste(readLines(paths[[1]], warn = FALSE), collapse = "\n")

  expect_match(content, "CBT-I rate (per 1,000)", fixed = TRUE)
  expect_match(content, "Placebo rate (per 1,000)", fixed = TRUE)
  expect_no_match(content, "Intervention rate (per 1,000)", fixed = TRUE)
  expect_no_match(content, "Control rate (per 1,000)", fixed = TRUE)
})

test_that("the markdown report is unchanged when no arms were named", {
  outcomes <- make_outcomes_gr()
  out_dir  <- tempfile("grade_report_default_")
  paths <- grade_report(outcomes, primary = "Outcome 1", format = "md",
                        output_dir = out_dir, output_file = "report_default")
  content <- paste(readLines(paths[[1]], warn = FALSE), collapse = "\n")
  expect_match(content, "Control rate (per 1,000)", fixed = TRUE)
  expect_match(content, "Intervention rate (per 1,000)", fixed = TRUE)
})
