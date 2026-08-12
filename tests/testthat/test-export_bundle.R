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
                                    outcome_name = "Test", threshold_type = "null"))
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
                                    outcome_name = "Test", threshold_type = "null"))
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
                                    outcome_name = "Test", threshold_type = "null"))
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
                                    outcome_name = "Test", threshold_type = "null"))
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
  # Core GRADE 2 entry gate: the target/threshold choice must round-trip.
  expect_match(txt, 'threshold_type          = "null"',         fixed = TRUE)
  expect_match(txt, "require_threshold       = TRUE",           fixed = TRUE)
  expect_match(txt, "rating_target           = NULL",           fixed = TRUE)
  expect_match(txt, "rating_target_rationale = NULL",           fixed = TRUE)
  expect_match(txt, "pubias_registry_complete = NULL",          fixed = TRUE)
  expect_match(txt, "inconsistency_ci_diff            = NULL",  fixed = TRUE)
  # baseline_risk auto-resolved to the pooled control rate (60/180)
  expect_match(txt, "baseline_risk           = 0.333",          fixed = TRUE)
  # No arm labels supplied -> the run_ma call carries none
  expect_no_match(txt, "experimental_label")

  parsed <- tryCatch(parse(text = txt), error = function(e) NULL)
  expect_false(is.null(parsed))
})

test_that("analysis.R reproduces a manual rating-target override", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(
    ma, study_design = "RCT", outcome_name = "Test",
    threshold_type = "null",
    rating_target = "non_null_effect",
    rating_target_rationale = "Panel rated certainty in any true effect"
  ))
  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- export_bundle(ma, g, output_dir = out_dir,
                            bundle_name = "target_bundle",
                            include = c("script"))
  unz_dir <- tempfile(); dir.create(unz_dir)
  zip::unzip(zip_path, exdir = unz_dir)
  txt <- paste(readLines(file.path(unz_dir, "analysis.R"), warn = FALSE),
               collapse = "\n")

  expect_match(txt, "rating_target           = 'non_null_effect'", fixed = TRUE)
  # The rationale is mandatory for the override, so the script must carry it
  # or it would abort on re-run.
  expect_match(txt, "Panel rated certainty in any true effect", fixed = TRUE)
  expect_false(is.null(tryCatch(parse(text = txt), error = function(e) NULL)))
})

# ---- .arg_lit() origin validation ------------------------------------------
# Regression: an unrecognised origin used to fall through to the plain-value
# branch, which cannot handle a list, and returned the "NULL" fallback. The
# bundled analysis.R then silently dropped the argument and reproduced a
# different analysis.

test_that(".arg_lit round-trips every recognised origin", {
  expect_equal(.arg_lit(list(origin = "null")), "NULL")
  expect_equal(.arg_lit(list(origin = "column", col = "rob_d")), "data$rob_d")
  expect_equal(.arg_lit(list(origin = "scalar", value = "undesirable")),
               shQuote("undesirable"))
  expect_equal(.arg_lit(list(origin = "scalar", value = TRUE)), "TRUE")
  expect_equal(.arg_lit(list(origin = "scalar", value = 0.25)), "0.25")
  expect_equal(.arg_lit(list(origin = "vector", value = c("no", "serious"))),
               "c('no', 'serious')")
  # Plain values and absent specs keep their old behaviour
  expect_equal(.arg_lit(NULL, fallback = "0.05"), "0.05")
  expect_equal(.arg_lit("yes"), shQuote("yes"))
})

test_that(".arg_lit aborts on an unknown origin instead of emitting NULL", {
  expect_error(.arg_lit(list(origin = "value", value = "desirable")),
               regexp = "Unknown argument spec origin")
  expect_error(.arg_lit(list(origin = "value", value = "desirable")),
               regexp = "null, column, scalar, vector", fixed = TRUE)
  expect_error(.arg_lit(list(origin = 42L, value = 1)),
               regexp = "Unknown argument spec origin")
})

test_that("export_bundle surfaces a typo'd grade_args origin", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all low",
                                    outcome_name = "Test",
                                    threshold_type = "null"))
  out_dir <- tempfile(); dir.create(out_dir)
  expect_error(
    export_bundle(ma, g, output_dir = out_dir, bundle_name = "bad_origin",
                  include = c("script"),
                  grade_args = list(
                    small_values = list(origin = "value", value = "desirable")
                  )),
    regexp = "Unknown argument spec origin"
  )
})

# ---- generated-script syntax check -----------------------------------------

test_that(".check_script_parses accepts valid R and aborts on broken R", {
  expect_true(.check_script_parses("x <- c(1, 2)\nsummary(x)\n"))
  expect_error(.check_script_parses("grade_meta(\n  rob = c(1, 2\n"),
               regexp = "not syntactically valid R")
  expect_error(.check_script_parses("grade_meta(\n  rob = c(1, 2\n"),
               regexp = "bug in pmatools")
})

test_that(".render_analysis_script refuses to write an unparseable script", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all low",
                                    outcome_name = "Test",
                                    threshold_type = "null"))
  out_path <- tempfile(fileext = ".R")

  # Stand in for a future literalisation bug: the renderer must propagate the
  # syntax-check failure and leave no half-written analysis.R behind.
  local_mocked_bindings(
    .check_script_parses = function(rendered) {
      rlang::abort(paste0(
        "The generated analysis.R is not syntactically valid R and would not ",
        "be reproducible."
      ))
    }
  )
  expect_error(
    .render_analysis_script(ma, g, ma_args = list(), grade_args = list(),
                            per = 1000, prediction = FALSE,
                            convert_smd_to_or = FALSE, baseline_risk = NULL,
                            threshold_label = NULL, out_path = out_path),
    regexp = "not syntactically valid R"
  )
  expect_false(file.exists(out_path))
})

# ---- Summary of Findings layout (style=) ------------------------------------
# Regression: export_bundle() had no `style` argument, so a caller rendering the
# BMJ layout on screen could only export the GRADEpro one, and the bundled
# analysis.R hardcoded a styleless sof_table() call that reproduced GRADEpro
# whatever the bundle held.

grade_for_style <- function(...) {
  suppressWarnings(grade_meta(make_meta_for_bundle(), study_design = "RCT",
                              rob = "no",
                              rob_rationale = "Consensus RoB2: all domains low risk",
                              indirectness = "no",
                              outcome_name = "Test", threshold_type = "null",
                              ...))
}

# Text of every paragraph and table cell of a .docx, in document order.
docx_text <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path))
  paste(s$text[!is.na(s$text)], collapse = "\n")
}

# Unzip a bundle and return the extraction directory.
unbundle <- function(zip_path) {
  d <- tempfile(); dir.create(d)
  zip::unzip(zip_path, exdir = d)
  d
}

bundle_with_style <- function(g, name, ...) {
  ma <- g$meta
  out_dir <- tempfile(); dir.create(out_dir)
  unbundle(export_bundle(ma, g, output_dir = out_dir, bundle_name = name,
                         include = c("script", "grade_table"), ...))
}

test_that("export_bundle writes the SoF layout it was asked for", {
  g <- grade_for_style()

  bmj <- docx_text(file.path(bundle_with_style(g, "style_bmj", style = "bmj"),
                             "sof_table.docx"))
  expect_match(bmj, "Outcome and follow-up", fixed = TRUE)
  expect_match(bmj, "Difference", fixed = TRUE)
  expect_no_match(bmj, "Risk with control", fixed = TRUE)

  gp <- docx_text(file.path(bundle_with_style(g, "style_gp", style = "gradepro"),
                            "sof_table.docx"))
  expect_match(gp, "Risk with control", fixed = TRUE)
  expect_no_match(gp, "Outcome and follow-up", fixed = TRUE)
})

test_that("export_bundle defaults to the BMJ layout, as the set method does", {
  g   <- grade_for_style()
  dir <- bundle_with_style(g, "style_default")
  expect_match(docx_text(file.path(dir, "sof_table.docx")),
               "Outcome and follow-up", fixed = TRUE)
  expect_match(paste(readLines(file.path(dir, "analysis.R"), warn = FALSE),
                     collapse = "\n"),
               'style = "bmj"', fixed = TRUE)
})

test_that("the bundled analysis.R carries the style that produced the bundle", {
  g <- grade_for_style()
  for (st in c("bmj", "gradepro")) {
    txt <- paste(readLines(file.path(
      bundle_with_style(g, paste0("style_script_", st), style = st),
      "analysis.R"), warn = FALSE), collapse = "\n")
    # Both the SoF table and the appendix report must be regenerated in the
    # exported layout, or re-running the script yields a different bundle.
    expect_match(txt, paste0('sof_table(g, style = "', st, '"'), fixed = TRUE)
    expect_match(txt, paste0('style       = "', st, '"'), fixed = TRUE)
    expect_false(is.null(tryCatch(parse(text = txt), error = function(e) NULL)))
  }
})

test_that("follow_up and unit reach the BMJ table and the bundled script", {
  g   <- grade_for_style()
  dir <- bundle_with_style(g, "style_followup", style = "bmj",
                           follow_up = "Follow-up: 12 months",
                           unit      = "days")

  expect_match(docx_text(file.path(dir, "sof_table.docx")),
               "Follow-up: 12 months", fixed = TRUE)
  txt <- paste(readLines(file.path(dir, "analysis.R"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, 'follow_up = "Follow-up: 12 months"', fixed = TRUE)
  expect_match(txt, 'unit = "days"', fixed = TRUE)
  expect_false(is.null(tryCatch(parse(text = txt), error = function(e) NULL)))
})

test_that("follow_up and unit fall back to the rated object", {
  # grade_meta() takes neither, but grade_meta_multi() stores both on the object
  # it rates, so a set member exported on its own keeps its follow-up line.
  g <- grade_for_style()
  g$follow_up <- "Follow-up: longest reported"
  g$unit      <- "points"

  dir <- bundle_with_style(g, "style_followup_obj", style = "bmj")
  expect_match(docx_text(file.path(dir, "sof_table.docx")),
               "Follow-up: longest reported", fixed = TRUE)
  expect_match(paste(readLines(file.path(dir, "analysis.R"), warn = FALSE),
                     collapse = "\n"),
               'follow_up = "Follow-up: longest reported"', fixed = TRUE)
})

test_that("an apostrophe in follow_up still yields a parseable analysis.R", {
  # shQuote()'s single-quoted literal would leave this unparseable and the
  # bundle would abort in .check_script_parses().
  g   <- grade_for_style()
  dir <- bundle_with_style(g, "style_apostrophe", style = "bmj",
                           follow_up = "Follow-up: patient's last visit")
  txt <- paste(readLines(file.path(dir, "analysis.R"), warn = FALSE),
               collapse = "\n")
  expect_false(is.null(tryCatch(parse(text = txt), error = function(e) NULL)))
  expect_match(txt, "patient's last visit", fixed = TRUE)
})

test_that("export_bundle rejects an unknown style instead of silently exporting one", {
  g <- grade_for_style()
  out_dir <- tempfile(); dir.create(out_dir)
  expect_error(
    export_bundle(g$meta, g, output_dir = out_dir, bundle_name = "bad_style",
                  include = c("script"), style = "grade_pro"),
    regexp = "should be one of"
  )
})

test_that("export_bundle includes rare-event artifacts when supplied", {
  d <- ingest_data(system.file("extdata", "rare_events_mock.csv", package = "pmatools"),
                   format = "long")
  rare <- run_rare_ma(d, effect_scale = "OR")
  ma <- rare$primary
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Rare Test", threshold_type = "null"))
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
  d <- ingest_data(system.file("extdata", "rare_events_mock.csv", package = "pmatools"),
                   format = "long")
  rare <- run_rare_ma(d, effect_scale = "OR")
  ma <- rare$primary
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Rare Test", threshold_type = "null"))
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

test_that("legacy export_bundle(ma = ) named call works with a deprecation warning", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Test", threshold_type = "null"))
  out_dir <- tempfile()
  dir.create(out_dir)

  # The warning fires once per session, so reset the rlang counter to keep this
  # test independent of the order the tests run in.
  rlang::reset_warning_verbosity("export_bundle_ma_arg")

  expect_warning(
    zip_path <- export_bundle(ma = ma, grade = g,
                              output_dir = out_dir,
                              bundle_name = "legacy_named",
                              include = c("data", "results")),
    regexp = "deprecated"
  )

  expect_true(file.exists(zip_path))
  files <- zip::zip_list(zip_path)$filename
  expect_true("data_long.csv" %in% files)
  expect_true("results.txt" %in% files)
})

test_that("legacy ma = call dispatches on a pmatools object too", {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Test", threshold_type = "null"))
  out_dir <- tempfile()
  dir.create(out_dir)

  rlang::reset_warning_verbosity("export_bundle_ma_arg")

  expect_warning(
    zip_path <- export_bundle(ma = g,
                              output_dir = out_dir,
                              bundle_name = "legacy_grade",
                              include = c("results")),
    regexp = "deprecated"
  )
  expect_true(file.exists(zip_path))
})

test_that("export_bundle errors when no object is passed at all", {
  expect_error(export_bundle(output_dir = tempdir()))
})
