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

# --------------------------------------------------------------------------
# An explicit sm = "ROM" string, not only an object's $sm
# --------------------------------------------------------------------------
# Every internal function that takes an `sm` argument canonicalises it on
# entry, so a caller that hands one {meta}'s raw spelling still gets a ratio.

test_that("run_ma() accepts {meta}'s ROM spelling", {
  meta_rom <- run_ma(make_long_rom(), outcome_type = "continuous", sm = "ROM")
  expect_identical(.sm_of(meta_rom), "RoM")
  expect_equal(exp(meta_rom$TE.random), 0.61, tolerance = 0.01)
})

test_that("helpers taking an sm argument read 'ROM' as a ratio of means", {
  expect_identical(.effect_header("ROM"), .effect_header("RoM"))
  expect_match(.effect_header("ROM"), "Relative effect")
  expect_identical(.bmj_measure_name("ROM"), "Ratio of means")
  target <- .derive_rating_target(te_point = log(0.61),
                                  threshold_internal = log(1.1),
                                  threshold_type = "mid", sm = "ROM")
  expect_match(target$note, "relative-effect scale (RoM)", fixed = TRUE)
  expect_identical(threshold_to_te_scale(1.1, sm = "ROM")$threshold_internal,
                   log(1.1))
  expect_equal(.p1(0.2, log(2), "rr"), 0.4)
})

test_that("the SoF effect column back-transforms a ROM object", {
  table_gradepro <- grade_table(list(Absenteeism = grade_rom("auto")))
  body_text <- paste(as.matrix(table_gradepro$body$dataset), collapse = " | ")
  expect_match(body_text, "RoM 0\\.61 \\(0\\.51; 0\\.74\\)")
  expect_false(grepl("-0\\.49", body_text))
  meta_rom <- run_ma(make_long_rom(), outcome_type = "continuous", sm = "RoM")
  expect_identical(format_effect(meta_rom, "relative"),
                   "RoM 0.61 (0.51; 0.74)")
})

test_that("forest effect text for a ROM object is on the natural scale", {
  meta_rom <- run_ma(make_long_rom(), outcome_type = "continuous", sm = "RoM")
  forest_calls <- list()
  local_mocked_bindings(
    forest = function(x, ...) {
      forest_calls[[length(forest_calls) + 1L]] <<- list(x = x, ...)
      invisible(TRUE)
    },
    .package = "meta")

  plot_forest(meta_rom, title = NULL)
  forest_args <- forest_calls[[1]]
  expect_identical(forest_args$rightlabs[[1]], "RoM (95% CI)")
  # Log-scale limits snapped around the ratio, not a linear axis through 0.
  expect_true(all(forest_args$xlim > 0))

  forest_calls <- list()
  missing_rows <- data.frame(studlab = "E", n = 80L,
                             results_known = "Not measured")
  plot_forest_pubias_subgroup(meta_rom, missing_df = missing_rows)
  subgroup_args <- forest_calls[[1]]
  effect_text <- subgroup_args$x$effect_status
  expect_identical(subgroup_args$rightlabs, "RoM (95% CI)")
  expect_true(all(grepl("^0\\.[0-9]{2} \\(", effect_text[1:4])))
  expect_false(any(grepl("^-", effect_text)))
})
