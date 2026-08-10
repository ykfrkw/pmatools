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
                                    rob_rationale = "Consensus RoB2: all domains low risk",
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
                                    rob_rationale = "Consensus RoB2: all domains low risk",
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

test_that("analysis.R renders all GRADE arguments and run_ma arm labels", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Test"))
  out_dir <- tempfile()
  dir.create(out_dir)

  zip_path <- export_bundle(
    ma, g,
    output_dir  = out_dir,
    bundle_name = "args_bundle",
    include     = c("script"),
    ma_args     = list(
      experimental_label = list(origin = "scalar", value = "drug"),
      control_label      = list(origin = "scalar", value = "placebo")
    ),
    grade_args  = list(
      baseline_risk                    = list(origin = "scalar", value = 0.25),
      inconsistency_ci_diff            = list(origin = "scalar", value = "yes"),
      inconsistency_threshold_side     = list(origin = "scalar",
                                              value = "majority_one_side"),
      inconsistency_subgroup_explained = list(origin = "scalar", value = "no"),
      ois_events                       = list(origin = "scalar", value = 300),
      ois_alpha                        = list(origin = "scalar", value = 0.05),
      ois_beta                         = list(origin = "scalar", value = 0.10),
      pubias_registry_complete         = list(origin = "scalar", value = "yes")
    )
  )

  unz_dir <- tempfile(); dir.create(unz_dir)
  zip::unzip(zip_path, exdir = unz_dir)
  txt <- paste(readLines(file.path(unz_dir, "analysis.R"), warn = FALSE),
               collapse = "\n")

  expect_match(txt, "baseline_risk           = 0.25",           fixed = TRUE)
  expect_match(txt, "inconsistency_ci_diff            = 'yes'", fixed = TRUE)
  expect_match(txt, "inconsistency_threshold_side     = 'majority_one_side'",
               fixed = TRUE)
  expect_match(txt, "inconsistency_subgroup_explained = 'no'",  fixed = TRUE)
  expect_match(txt, "ois_events              = 300",            fixed = TRUE)
  expect_match(txt, "ois_alpha               = 0.05",           fixed = TRUE)
  expect_match(txt, "ois_beta                = 0.1",            fixed = TRUE)
  expect_match(txt, "pubias_registry_complete = 'yes'",         fixed = TRUE)
  expect_match(txt, "experimental_label = 'drug'",              fixed = TRUE)
  expect_match(txt, "control_label      = 'placebo'",           fixed = TRUE)

  # And the rendered script must still parse as valid R
  parsed <- tryCatch(parse(text = txt), error = function(e) NULL)
  expect_false(is.null(parsed))
})

test_that("analysis.R falls back to sensible GRADE defaults when specs absent", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Test"))
  out_dir <- tempfile()
  dir.create(out_dir)

  zip_path <- export_bundle(ma, g,
                            output_dir = out_dir,
                            bundle_name = "defaults_bundle",
                            include = c("script"))

  unz_dir <- tempfile(); dir.create(unz_dir)
  zip::unzip(zip_path, exdir = unz_dir)
  txt <- paste(readLines(file.path(unz_dir, "analysis.R"), warn = FALSE),
               collapse = "\n")

  expect_match(txt, "ois_events              = NULL",           fixed = TRUE)
  expect_match(txt, "ois_alpha               = 0.05",           fixed = TRUE)
  expect_match(txt, "ois_beta                = 0.2",            fixed = TRUE)
  expect_match(txt, "pubias_registry_complete = NULL",          fixed = TRUE)
  expect_match(txt, "inconsistency_ci_diff            = NULL",  fixed = TRUE)
  # baseline_risk auto-resolved to the pooled control rate (60/180)
  expect_match(txt, "baseline_risk           = 0.333",          fixed = TRUE)
  # No arm labels supplied -> the run_ma call carries none
  expect_no_match(txt, "experimental_label")

  parsed <- tryCatch(parse(text = txt), error = function(e) NULL)
  expect_false(is.null(parsed))
})

test_that("export_bundle includes rare-event artifacts when supplied", {
  d <- ingest_data(testthat::test_path("../../inst/extdata/rare_events_mock.csv"),
                   format = "long")
  rare <- run_rare_ma(d, effect_scale = "OR")
  ma <- rare$primary
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Rare Test"))
  out_dir <- tempfile()
  dir.create(out_dir)

  zip_path <- export_bundle(ma, g,
                            output_dir = out_dir,
                            bundle_name = "rare_bundle",
                            include = c("results"),
                            rare = rare)

  files <- zip::zip_list(zip_path)$filename
  expect_true("rare_event_diagnostics.csv" %in% files)
  expect_true("rare_event_method_table.csv" %in% files)
  expect_true("rare_event_method_forest.pdf" %in% files)
  expect_true("rare_event_method_forest.png" %in% files)
})

test_that("export_bundle script reruns rare-event methods when rare object supplied", {
  d <- ingest_data(testthat::test_path("../../inst/extdata/rare_events_mock.csv"),
                   format = "long")
  rare <- run_rare_ma(d, effect_scale = "OR")
  ma <- rare$primary
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Rare Test"))
  out_dir <- tempfile()
  dir.create(out_dir)

  zip_path <- export_bundle(ma, g,
                            output_dir = out_dir,
                            bundle_name = "rare_script_bundle",
                            include = c("script"),
                            rare = rare)

  unz_dir <- tempfile(); dir.create(unz_dir)
  zip::unzip(zip_path, exdir = unz_dir)
  script <- readLines(file.path(unz_dir, "analysis.R"), warn = FALSE)
  expect_true(any(grepl("run_rare_ma", script, fixed = TRUE)))
  expect_true(any(grepl("rare_event_method_forest.pdf", script, fixed = TRUE)))
})
