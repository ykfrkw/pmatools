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

# ---- grade_args names are matched exactly ----------------------------------
#
# `$` on a list partial-matches. Before these tests, a bundle carrying only an
# `inconsistency_ci_diff` spec rendered that spec's value into the script's
# `inconsistency =` slot as well, so the "reproducible" analysis.R issued a
# manual Inconsistency override the reviewer never made.

render_script_txt <- function(ma, g, grade_args = list(), ma_args = list()) {
  out_path <- tempfile(fileext = ".R")
  .render_analysis_script(ma, g, ma_args = ma_args, grade_args = grade_args,
                          per = 1000, prediction = FALSE,
                          convert_smd_to_or = FALSE, baseline_risk = NULL,
                          threshold_label = NULL, out_path = out_path)
  paste(readLines(out_path, warn = FALSE), collapse = "\n")
}

# The single line the generated grade_meta() call devotes to `arg`, e.g.
#   "  inconsistency           = NULL,"
arg_line <- function(txt, arg) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  hit <- grep(paste0("^\\s*", arg, "\\s*="), lines)
  if (length(hit) == 0L) return(NA_character_)
  paste(trimws(lines[hit]), collapse = " ~~ ")
}

grade_args_fixture <- function() {
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                   rob_rationale = "Consensus RoB2: all low",
                                   indirectness = "no",
                                   outcome_name = "Test",
                                   threshold_type = "null"))
  list(ma = ma, g = g)
}

test_that("an inconsistency_ci_diff spec does not leak into inconsistency", {
  f <- grade_args_fixture()
  txt <- render_script_txt(
    f$ma, f$g,
    grade_args = list(inconsistency_ci_diff = list(origin = "scalar",
                                                   value = 0.4))
  )

  expect_match(txt, "inconsistency_ci_diff            = 0.4", fixed = TRUE)
  # ... and the scalar Inconsistency override stays at grade_meta()'s default.
  expect_equal(arg_line(txt, "inconsistency"), "inconsistency           = NULL,")
})

test_that("supplying a long grade_args name never answers for its prefix", {
  f <- grade_args_fixture()
  legal <- .grade_arg_names()

  # Every (short, long) pair in the registry where `$` would partial-match.
  pairs <- do.call(rbind, lapply(legal, function(short) {
    longs <- setdiff(legal[startsWith(legal, short)], short)
    if (length(longs) == 0L) return(NULL)
    data.frame(short = short, long = longs, stringsAsFactors = FALSE)
  }))
  expect_true(nrow(pairs) > 10L)   # registry sanity: the hazard is widespread

  # The baseline render is the documented fallback for every argument: NULL for
  # most, but object-derived for `indirectness` and `threshold`. Comparing
  # against it therefore asserts the right thing for those two as well.
  base_txt <- render_script_txt(f$ma, f$g)

  for (i in seq_len(nrow(pairs))) {
    short <- pairs$short[i]
    long  <- pairs$long[i]
    base_line <- arg_line(base_txt, short)
    # A registry name the template does not render (nothing to leak into).
    if (is.na(base_line)) next

    spec <- list(list(origin = "scalar", value = 0.4321))
    names(spec) <- long
    txt <- render_script_txt(f$ma, f$g, grade_args = spec)

    expect_equal(arg_line(txt, short), base_line,
                 info = paste0("grade_args$", long, " leaked into ", short))
  }
})

test_that("an unknown grade_args name aborts instead of being dropped", {
  f <- grade_args_fixture()
  expect_error(
    render_script_txt(f$ma, f$g,
                      grade_args = list(inconsistancy = list(origin = "scalar",
                                                             value = "yes"))),
    regexp = "inconsistancy"
  )
  expect_error(
    render_script_txt(f$ma, f$g,
                      grade_args = list(inconsistancy = list(origin = "scalar",
                                                             value = "yes"))),
    regexp = "Unknown grade_args name"
  )
  # The abort points at the argument that was meant.
  expect_error(
    render_script_txt(f$ma, f$g,
                      grade_args = list(inconsistancy = list(origin = "scalar",
                                                             value = "yes"))),
    regexp = "inconsistency"
  )
  # Unnamed specs cannot be matched either.
  expect_error(
    render_script_txt(f$ma, f$g,
                      grade_args = list(list(origin = "scalar", value = "yes"))),
    regexp = "must be named"
  )
  out_dir <- tempfile(); dir.create(out_dir)
  expect_error(
    export_bundle(f$ma, f$g, output_dir = out_dir,
                  bundle_name = "bad_name", include = c("script"),
                  grade_args = list(inconsistancy = list(origin = "scalar",
                                                         value = "yes"))),
    regexp = "Unknown grade_args name"
  )
})

# ---- threshold_baseline round-trip -----------------------------------------

test_that("the script pins the reviewer's threshold_baseline, not a re-derived one", {
  # Pooled control-arm risk here is 60/180 = 0.333; the reviewer anchors the
  # absolute threshold to 0.12 instead, so re-deriving it on re-run would
  # rescale the threshold and can change the rating.
  ma <- make_meta_for_bundle()
  g <- suppressWarnings(grade_meta(
    ma, study_design = "RCT", rob = "no",
    rob_rationale = "Consensus RoB2: all low",
    indirectness = "no", outcome_name = "Test",
    threshold_type = "mid", threshold = 0.05,
    threshold_scale = "ard", threshold_baseline = 0.12
  ))
  expect_equal(g$threshold_baseline, 0.12)

  txt <- render_script_txt(ma, g)

  # The rendered text actually carries the reviewer's value ...
  expect_match(txt, "threshold_baseline      = 0.12", fixed = TRUE)

  # ... and re-issuing the generated grade_meta() call reproduces the rating.
  script_path <- tempfile(fileext = ".R")
  writeLines(txt, script_path)
  exprs <- parse(file = script_path)
  is_grade_call <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("grade_meta"))
  }, logical(1))
  expect_equal(sum(is_grade_call), 1L)

  env <- new.env(parent = environment())
  env$ma <- ma
  suppressWarnings(eval(exprs[is_grade_call][[1]], envir = env))
  g2 <- env$g

  expect_equal(g2$threshold_baseline, g$threshold_baseline)
  expect_equal(g2$threshold_internal, g$threshold_internal)
  expect_equal(g2$certainty, g$certainty)
  expect_equal(g2$domain_assessments$judgment, g$domain_assessments$judgment)
})

test_that("threshold_baseline stays NULL when the rating never resolved one", {
  f <- grade_args_fixture()   # threshold_type = "null", no ARD threshold
  expect_null(f$g$threshold_baseline)
  expect_equal(arg_line(render_script_txt(f$ma, f$g), "threshold_baseline"),
               "threshold_baseline      = NULL,")
})

test_that("the multi-outcome script carries threshold_baseline through", {
  raw <- rbind(
    data.frame(studlab = c("A", "B", "C"), outcome = "Mortality",
               treat = "experimental", n = c(50, 60, 70), event = c(10, 15, 20),
               mean = NA_real_, sd = NA_real_, stringsAsFactors = FALSE),
    data.frame(studlab = c("A", "B", "C"), outcome = "Mortality",
               treat = "control", n = c(50, 60, 70), event = c(15, 20, 25),
               mean = NA_real_, sd = NA_real_, stringsAsFactors = FALSE)
  )
  d  <- suppressMessages(ingest_data(raw, format = "long"))
  ml <- suppressWarnings(run_ma_multi(d, sm = list("Mortality" = "OR")))
  set <- suppressWarnings(grade_meta_multi(
    ml,
    common = list(study_design = "RCT", rob = "no",
                  rob_rationale = "Consensus RoB2: all low",
                  indirectness = "no",
                  threshold_type = "mid", threshold = 0.05,
                  threshold_scale = "ard", threshold_baseline = 0.12)
  ))

  out_path <- tempfile(fileext = ".R")
  .render_analysis_script_multi(set, per = 1000, prediction = FALSE,
                                style = "bmj", out_path = out_path)
  txt <- paste(readLines(out_path, warn = FALSE), collapse = "\n")

  expect_match(txt, "threshold_baseline", fixed = TRUE)
  expect_match(txt, "threshold_baseline = 0.12", fixed = TRUE)
})
