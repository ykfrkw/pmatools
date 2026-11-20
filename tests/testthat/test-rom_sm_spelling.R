# test-rom_sm_spelling.R - a ratio of means survives {meta}'s "ROM" spelling
#
# metacont() stores sm = "RoM" as "ROM". pmatools compares against its own
# "RoM", so these tests run a RoM analysis end to end and check that every
# surface reads it as a ratio: back-transformed effects and thresholds,
# threshold_scale = "auto", and a forest plot with finite log limits.

make_long_rom <- function() {
  data.frame(
    studlab = rep(c("A", "B", "C", "D"), each = 2),
    treat   = rep(c("experimental", "control"), 4),
    n       = c(50, 50, 60, 60, 70, 70, 40, 40),
    mean    = c(6, 10, 7, 12, 5, 9, 8, 11),
    sd      = c(3, 4, 3, 4, 3, 4, 3, 4),
    stringsAsFactors = FALSE
  )
}

grade_rom <- function(threshold_scale) {
  meta_rom <- run_ma(make_long_rom(), outcome_type = "continuous", sm = "RoM")
  suppressWarnings(grade_meta(meta_rom, threshold = 1.1,
                              threshold_scale = threshold_scale,
                              small_values = "desirable"))
}

test_that("{meta} stores RoM as ROM and pmatools canonicalises it", {
  meta_rom <- run_ma(make_long_rom(), outcome_type = "continuous", sm = "RoM")
  expect_identical(meta_rom$sm, "ROM")
  expect_identical(.sm_of(meta_rom), "RoM")
  expect_identical(.canon_sm("rom"), "RoM")
  expect_identical(.canon_sm("or"), "OR")
  expect_null(.canon_sm(NULL))
})

test_that("threshold_scale auto and ratio agree on a RoM analysis", {
  graded_auto  <- grade_rom("auto")
  graded_ratio <- grade_rom("ratio")
  expect_equal(graded_auto$threshold_internal, log(1.1))
  expect_equal(graded_ratio$threshold_internal, log(1.1))
  expect_identical(graded_auto$meta$sm, "ROM")
})

test_that("grade_table(style = 'bmj') prints the back-transformed RoM", {
  table_bmj <- grade_table(list(Absenteeism = grade_rom("auto")),
                           style = "bmj")
  body_text <- paste(as.matrix(table_bmj$body$dataset), collapse = " | ")
  expect_match(body_text, "Ratio of means 0\\.61 \\(0\\.51 to 0\\.74\\)")
  expect_false(grepl("-0\\.49", body_text))
})

test_that("grade_report(format = 'md') prints back-transformed RoM and threshold", {
  report_dir <- withr::local_tempdir()
  suppressWarnings(grade_report(list(Absenteeism = grade_rom("ratio")),
                                format = "md", output_dir = report_dir))
  report_file <- list.files(report_dir, pattern = "\\.md$", full.names = TRUE)
  report_text <- paste(readLines(report_file[[1]]), collapse = "\n")
  expect_match(report_text, "RoM 0\\.61 \\(0\\.51; 0\\.74\\)")
  expect_match(report_text, "threshold RoM 1\\.1")
  expect_false(grepl("RoM -0\\.", report_text))
})

test_that("plot_forest draws a RoM analysis without NaN warnings", {
  meta_rom <- run_ma(make_long_rom(), outcome_type = "continuous", sm = "RoM")
  plot_file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_warning(plot_forest(meta_rom))
})
