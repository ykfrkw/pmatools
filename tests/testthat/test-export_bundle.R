library(testthat)

skip_if_not_installed("meta")
skip_if_not_installed("zip")

make_meta_for_bundle <- function() {
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

test_that("export_bundle creates ZIP with expected files (data + script + results)", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    indirectness = "no",
                                    outcome_name = "Test"))
  out_dir <- tempfile()
  dir.create(out_dir)

  zip_path <- export_bundle(ma, g,
                            output_dir = out_dir,
                            bundle_name = "test_bundle",
                            include = c("data", "script", "results"))

  expect_true(file.exists(zip_path))
  files <- zip::zip_list(zip_path)$filename
  expect_true("data_long.csv" %in% files)
  expect_true("analysis.R" %in% files)
  expect_true("results.txt" %in% files)
})

test_that("export_bundle generated analysis.R parses as valid R", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    indirectness = "no",
                                    outcome_name = "Test"))
  out_dir <- tempfile()
  dir.create(out_dir)

  zip_path <- export_bundle(ma, g,
                            output_dir = out_dir,
                            bundle_name = "test_bundle",
                            include = c("script"))

  # Extract analysis.R and parse
  unz_dir <- tempfile(); dir.create(unz_dir)
  zip::unzip(zip_path, exdir = unz_dir)
  script_path <- file.path(unz_dir, "analysis.R")
  expect_true(file.exists(script_path))
  parsed <- tryCatch(parse(file = script_path), error = function(e) NULL)
  expect_false(is.null(parsed))
})
