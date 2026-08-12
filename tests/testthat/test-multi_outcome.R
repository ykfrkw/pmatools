library(testthat)

skip_if_not_installed("meta")
skip_if_not_installed("zip")

# --------------------------------------------------------------------------
# Fixtures
# --------------------------------------------------------------------------

STUDIES <- c("Adams 2019", "Baker 2020", "Chen 2021", "Diaz 2022", "Evans 2023")

bin_rows <- function(outcome, studlab, ee, ne, ec, nc, rob) {
  rbind(
    data.frame(studlab = studlab, outcome = outcome, treat = "experimental",
               n = ne, event = ee, mean = NA_real_, sd = NA_real_, rob = rob,
               stringsAsFactors = FALSE),
    data.frame(studlab = studlab, outcome = outcome, treat = "control",
               n = nc, event = ec, mean = NA_real_, sd = NA_real_, rob = rob,
               stringsAsFactors = FALSE)
  )
}

cont_rows <- function(outcome, studlab, me, sde, ne, mc, sdc, nc, rob) {
  rbind(
    data.frame(studlab = studlab, outcome = outcome, treat = "experimental",
               n = ne, event = NA_real_, mean = me, sd = sde, rob = rob,
               stringsAsFactors = FALSE),
    data.frame(studlab = studlab, outcome = outcome, treat = "control",
               n = nc, event = NA_real_, mean = mc, sd = sdc, rob = rob,
               stringsAsFactors = FALSE)
  )
}

# Three outcomes: one binary, one continuous, one binary. All low risk of bias,
# so no outcome triggers the Core GRADE 4 Fig 2 refit.
multi_raw <- function() {
  rob <- rep("no", length(STUDIES))
  rbind(
    bin_rows("Mortality", STUDIES,
             c(10, 12, 8, 15, 9), c(80, 90, 70, 100, 85),
             c(18, 20, 15, 25, 17), c(80, 90, 70, 100, 85), rob),
    cont_rows("Depression severity", STUDIES,
              c(12, 11, 13, 10, 12), c(4, 4.5, 5, 4.2, 3.9),
              c(80, 90, 70, 100, 85),
              c(15, 14.5, 16, 13.5, 15.2), c(4.1, 4.6, 5.2, 4.3, 4.0),
              c(80, 90, 70, 100, 85), rob),
    bin_rows("Serious adverse events", STUDIES,
             c(3, 4, 2, 5, 3), c(80, 90, 70, 100, 85),
             c(2, 3, 2, 4, 2), c(80, 90, 70, 100, 85), rob)
  )
}

multi_data <- function() suppressMessages(ingest_data(multi_raw(), format = "long"))

quiet_ma_multi   <- function(...) suppressWarnings(run_ma_multi(...))
quiet_grade_multi <- function(...) suppressWarnings(grade_meta_multi(...))

# The standard three-outcome set used by most tests below.
make_set <- function(data = multi_data()) {
  ml <- quiet_ma_multi(data, sm = list("Mortality" = "OR",
                                       "Depression severity" = "SMD",
                                       "Serious adverse events" = "RR"))
  quiet_grade_multi(
    ml,
    common = list(study_design = "RCT", threshold_type = "null",
                  indirectness = "no", small_values = "undesirable",
                  follow_up = "12 months"),
    per_outcome = list(
      "Mortality" = list(threshold_type = "mid", threshold = 1.25,
                         threshold_scale = "ratio", follow_up = "24 months")
    ),
    primary = "Mortality"
  )
}

# --------------------------------------------------------------------------
# 1. End-to-end
# --------------------------------------------------------------------------

test_that("ingest -> run_ma_multi -> grade_meta_multi -> grade_table -> export_bundle", {
  d <- multi_data()
  expect_true("outcome" %in% names(d))

  ml <- quiet_ma_multi(d)
  expect_length(ml, 3L)
  expect_true(all(vapply(ml, inherits, logical(1), "meta")))

  set <- quiet_grade_multi(ml, common = list(study_design = "RCT",
                                             threshold_type = "null",
                                             indirectness = "no"))
  expect_s3_class(set, "pmatools_set")
  expect_equal(set$order, c("Mortality", "Depression severity",
                            "Serious adverse events"))
  expect_true(all(vapply(set$outcomes, inherits, logical(1), "pmatools")))

  ft <- grade_table(set, style = "bmj")
  expect_s3_class(ft, "flextable")

  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "multi",
                  include = c("sof", "results", "readme")))
  expect_true(file.exists(zip_path))

  # print()/summary() surface certainty and analysis set per outcome
  txt <- paste(utils::capture.output(print(set)), collapse = "\n")
  expect_match(txt, "Mortality")
  expect_match(txt, "all studies")
  expect_match(paste(utils::capture.output(summary(set)), collapse = "\n"),
               "Imprecision")
})

# --------------------------------------------------------------------------
# 2. Per-outcome effect measures
# --------------------------------------------------------------------------

test_that("sm can differ per outcome, mixing binary and continuous", {
  ml <- quiet_ma_multi(multi_data(),
                       sm = list("Mortality" = "OR",
                                 "Depression severity" = "SMD",
                                 "Serious adverse events" = "RR"))
  expect_equal(ml[["Mortality"]]$sm, "OR")
  expect_equal(ml[["Depression severity"]]$sm, "SMD")
  expect_equal(ml[["Serious adverse events"]]$sm, "RR")
  expect_s3_class(ml[["Depression severity"]], "metacont")
  expect_s3_class(ml[["Mortality"]], "metabin")

  # Mixed measures leave the BMJ Effect header generic, and the table says so.
  set <- make_set()
  ft  <- grade_table(set, style = "bmj")
  hdr <- names(ft$body$dataset)
  expect_true(any(grepl("^Effect", hdr)))
  footer <- paste(unlist(ft$footer$content$data), collapse = " ")
  expect_match(footer, "different effect measures")
  # ... while every cell still names its own measure.
  effects <- ft$body$dataset[[3]]
  expect_true(any(grepl("Odds ratio", effects)))
  expect_true(any(grepl("Standardised mean difference", effects)))
})

test_that("run_ma_multi validates the outcome selection", {
  d <- multi_data()
  expect_error(run_ma_multi(d, outcomes = c("Mortality", "Nope")),
               regexp = "not found in the data")
  expect_error(run_ma_multi(d, outcomes = c("Mortality", "Mortality")),
               regexp = "duplicates")
  ml <- quiet_ma_multi(d, outcomes = "Mortality")
  expect_length(ml, 1L)

  no_outcome <- d[, setdiff(names(d), "outcome"), drop = FALSE]
  expect_error(run_ma_multi(no_outcome), regexp = "no 'outcome' column")
})

# --------------------------------------------------------------------------
# 3. per_outcome overrides common
# --------------------------------------------------------------------------

test_that("per_outcome arguments override common ones", {
  ml <- quiet_ma_multi(multi_data())
  set <- quiet_grade_multi(
    ml,
    common = list(study_design = "RCT", threshold_type = "null",
                  indirectness = "no"),
    per_outcome = list(
      "Mortality" = list(study_design = "obs",
                         indirectness = "serious",
                         indirectness_rationale = "Surrogate population")
    )
  )
  expect_equal(set$outcomes[["Mortality"]]$study_design, "obs")
  expect_equal(set$outcomes[["Depression severity"]]$study_design, "RCT")

  ind <- function(g) {
    d <- g$domain_assessments
    d$judgment[d$domain == "Indirectness"]
  }
  expect_equal(ind(set$outcomes[["Mortality"]]), "serious")
  expect_equal(ind(set$outcomes[["Depression severity"]]), "no")

  expect_error(grade_meta_multi(ml, per_outcome = list("Nope" = list())),
               regexp = "per_outcome names not found")
})

# --------------------------------------------------------------------------
# 4. The Core GRADE 2 entry gate is never demoted to a warning
# --------------------------------------------------------------------------

test_that("a missing MID aborts grade_meta_multi rather than skipping the outcome", {
  ml <- quiet_ma_multi(multi_data())

  # threshold_type = "mid" (the default) with no threshold: the gate must
  # propagate out of the batch loop untouched.
  expect_error(
    suppressWarnings(grade_meta_multi(ml, common = list(study_design = "RCT"))),
    regexp = "requires a threshold"
  )
  # ... and it must still be the classed entry-gate condition, not a warning
  # that happens to mention a threshold.
  cnd <- tryCatch(
    suppressWarnings(grade_meta_multi(ml, common = list(study_design = "RCT"))),
    error = function(e) e)
  expect_s3_class(cnd, "pmatools_threshold_gate")

  # Only one outcome missing its MID is enough to abort the whole batch.
  expect_error(
    suppressWarnings(grade_meta_multi(
      ml,
      common = list(study_design = "RCT", threshold_type = "null"),
      per_outcome = list("Mortality" = list(threshold_type = "mid")))),
    class = "pmatools_threshold_gate"
  )

  # The manual rating-target gate is classed the same way.
  expect_true(.is_threshold_gate(
    tryCatch(rlang::abort("x requires a threshold (MID)"), error = function(e) e)))
})

# --------------------------------------------------------------------------
# 5. One failing outcome does not take the session down
# --------------------------------------------------------------------------

test_that("a failing outcome yields NULL plus a warning, the rest complete", {
  raw <- rbind(
    multi_raw(),
    # An outcome that was listed but never extracted: sample sizes only.
    data.frame(studlab = rep(STUDIES, 2), outcome = "Relapse",
               treat = rep(c("experimental", "control"), each = length(STUDIES)),
               n = rep(c(80, 90, 70, 100, 85), 2),
               event = NA_real_, mean = NA_real_, sd = NA_real_, rob = "no",
               stringsAsFactors = FALSE)
  )
  d <- suppressMessages(ingest_data(raw, format = "long"))

  warns <- testthat::capture_warnings(ml <- run_ma_multi(d))
  expect_true(any(grepl("run_ma\\(\\) failed for outcome 'Relapse'", warns)))
  expect_null(ml[["Relapse"]])
  expect_length(ml, 4L)
  expect_true(all(vapply(ml[c("Mortality", "Depression severity",
                              "Serious adverse events")],
                         inherits, logical(1), "meta")))

  set <- quiet_grade_multi(ml, common = list(study_design = "RCT",
                                             threshold_type = "null",
                                             indirectness = "no"))
  expect_length(set$outcomes, 3L)
  expect_false("Relapse" %in% set$order)
})

test_that("grade_meta_multi warns and drops an outcome it cannot rate", {
  ml <- quiet_ma_multi(multi_data())
  warns <- testthat::capture_warnings(
    set <- grade_meta_multi(
      ml,
      common = list(study_design = "RCT", threshold_type = "null",
                    indirectness = "no"),
      per_outcome = list(
        # An override without its mandatory rationale: a plain per-outcome
        # failure, so the batch continues.
        "Mortality" = list(imprecision = "serious")
      ))
  )
  expect_true(any(grepl("grade_meta\\(\\) failed for outcome 'Mortality'", warns)))
  expect_false("Mortality" %in% names(set$outcomes))
  expect_length(set$outcomes, 2L)
})

# --------------------------------------------------------------------------
# 6/7. Ordering
# --------------------------------------------------------------------------

test_that("reorder_outcomes drives both the table rows and the sub-directory numbers", {
  set <- make_set()
  new_order <- c("Serious adverse events", "Mortality", "Depression severity")
  set <- reorder_outcomes(set, new_order)
  expect_equal(set$order, new_order)

  # Table rows: no primary grouping here, so rows follow set$order exactly.
  plain <- set_primary(set, NULL)
  ft    <- grade_table(plain, style = "bmj")
  first_col <- ft$body$dataset[[1]]
  expect_equal(sub("\n.*$", "", first_col), new_order)

  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "ordered",
                  include = c("results")))
  files <- zip::zip_list(zip_path)$filename
  expect_true("outcomes/01_serious_adverse_events/results.txt" %in% files)
  expect_true("outcomes/02_mortality/results.txt" %in% files)
  expect_true("outcomes/03_depression_severity/results.txt" %in% files)
})

test_that("reorder_outcomes and set_primary reject bad input", {
  set <- make_set()
  expect_error(reorder_outcomes(set, c("Mortality")), regexp = "every outcome")
  expect_error(reorder_outcomes(set, c("Mortality", "Mortality",
                                       "Depression severity",
                                       "Serious adverse events")),
               regexp = "duplicated")
  expect_error(reorder_outcomes(set, c("Mortality", "Depression severity",
                                       "Serious adverse events", "Nope")),
               regexp = "unknown outcome")
  expect_error(reorder_outcomes(set, 1:3), regexp = "character vector")
  expect_error(reorder_outcomes(list(), "x"), regexp = "pmatools_set")

  expect_error(set_primary(set, "Nope"), regexp = "unknown outcome")
  expect_equal(set_primary(set, NULL)$primary, character(0))
  expect_equal(set_primary(set, "Mortality")$primary, "Mortality")
})

# --------------------------------------------------------------------------
# 8. slug()
# --------------------------------------------------------------------------

test_that("slug reduces outcome names to safe directory names", {
  expect_equal(.slug("Serious adverse events"), "serious_adverse_events")
  expect_equal(.slug("All-cause mortality (30 d)"), "all_cause_mortality_30_d")
  expect_equal(.slug("  spaced  out  "), "spaced_out")

  # Non-ASCII-only names collapse to nothing and fall back to the position.
  expect_equal(.slug("死亡", index = 3L), "outcome_03")
  expect_equal(.slug("——", index = 1L), "outcome_01")
  expect_equal(.slug(""), "outcome")

  # Numbering preserves order; identical slugs are disambiguated.
  dirs <- .outcome_dir_names(c("Mortality", "mortality!", "死亡",
                               "再発"))
  expect_equal(dirs, c("01_mortality", "02_mortality_2", "03_outcome_03",
                       "04_outcome_04"))
  expect_equal(length(unique(dirs)), 4L)
})

test_that("non-ASCII outcome names still export to unique directories", {
  raw <- multi_raw()
  raw$outcome[raw$outcome == "Mortality"] <- "死亡"
  raw$outcome[raw$outcome == "Serious adverse events"] <- "重篤な有害事象"
  d  <- suppressMessages(ingest_data(raw, format = "long"))
  ml <- quiet_ma_multi(d)
  set <- quiet_grade_multi(ml, common = list(study_design = "RCT",
                                             threshold_type = "null",
                                             indirectness = "no"))
  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "jp",
                  include = c("results")))
  files <- zip::zip_list(zip_path)$filename
  dirs  <- unique(dirname(files))
  expect_true(all(grepl("^outcomes/[0-9]{2}_[a-z0-9_]+$", dirs)))
  expect_equal(length(dirs), 3L)
})

# --------------------------------------------------------------------------
# 9. ZIP layout
# --------------------------------------------------------------------------

test_that("the multi-outcome ZIP has the specified layout", {
  set <- make_set()
  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "layout"))
  files <- zip::zip_list(zip_path)$filename

  for (f in c("summary_of_findings.docx", "summary_of_findings.csv",
              "evidence_profile.docx", "analysis.R", "data_long.csv",
              "README.txt")) {
    expect_true(f %in% files, info = f)
  }
  # Summary artifacts live at the top level, per-outcome ones do not.
  expect_false(any(grepl("^outcomes/.*summary_of_findings", files)))

  for (stem in c("forest_plot.pdf", "forest_plot.png", "funnel_plot.pdf",
                 "funnel_plot.png", "results.txt", "data_long.csv",
                 "evidence_profile.docx")) {
    expect_true(paste0("outcomes/01_mortality/", stem) %in% files, info = stem)
  }
  expect_true("outcomes/02_depression_severity/results.txt" %in% files)
  expect_true("outcomes/03_serious_adverse_events/results.txt" %in% files)

  # The per-outcome CSV holds only that outcome.
  ex <- tempfile(); dir.create(ex)
  zip::unzip(zip_path, exdir = ex)
  one <- utils::read.csv(file.path(ex, "outcomes", "01_mortality",
                                   "data_long.csv"))
  expect_equal(unique(one$outcome), "Mortality")
  all_rows <- utils::read.csv(file.path(ex, "data_long.csv"))
  expect_equal(length(unique(all_rows$outcome)), 3L)

  # README records the order and the per-outcome analysis sets.
  readme <- paste(readLines(file.path(ex, "README.txt")), collapse = "\n")
  expect_match(readme, "01_mortality")
  expect_match(readme, "primary outcome", fixed = TRUE)

  # The CSV mirror of the summary table follows set$order.
  sof <- utils::read.csv(file.path(ex, "summary_of_findings.csv"))
  expect_equal(sof$outcome, set$order)
  expect_equal(sof$group, c("primary", "secondary", "secondary"))
})

# --------------------------------------------------------------------------
# 10/11. Conditional per-outcome artifacts
# --------------------------------------------------------------------------

# One outcome whose risk-of-bias flowchart lands on "use low risk of bias
# studies only", one whose does not.
refit_data <- function() {
  st <- c("High-1", "Low-1", "Low-2", "Low-3")
  rob <- c("serious", "no", "no", "no")
  raw <- rbind(
    bin_rows("Mortality", st,
             c(40, 30, 31, 29), c(100, 100, 100, 100),
             c(10, 28, 29, 27), c(100, 100, 100, 100), rob),
    bin_rows("Serious adverse events", st,
             c(12, 11, 13, 12), c(100, 100, 100, 100),
             c(10, 10, 11, 10), c(100, 100, 100, 100), rob)
  )
  suppressMessages(ingest_data(raw, format = "long"))
}

test_that("forest_plot_full is written only for outcomes that were refitted", {
  d  <- refit_data()
  ml <- quiet_ma_multi(d, sm = "OR")
  set <- quiet_grade_multi(
    ml,
    common = list(study_design = "RCT", indirectness = "no",
                  small_values = "undesirable",
                  rob = c("serious", "no", "no", "no"),
                  threshold = 1.05, threshold_scale = "ratio")
  )
  skip_if_not(isTRUE(set$outcomes[["Mortality"]]$rob_refit),
              "fixture did not trigger the low-RoB refit")
  expect_false(isTRUE(set$outcomes[["Serious adverse events"]]$rob_refit))

  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "refit",
                  include = c("forest", "forest_full", "results")))
  files <- zip::zip_list(zip_path)$filename
  expect_true("outcomes/01_mortality/forest_plot_full.pdf" %in% files)
  expect_true("outcomes/01_mortality/forest_plot.pdf" %in% files)
  expect_false("outcomes/02_serious_adverse_events/forest_plot_full.pdf" %in% files)

  # The mixed analysis sets are stated, not left to be inferred.
  txt <- paste(utils::capture.output(print(set)), collapse = "\n")
  expect_match(txt, "low RoB only")
  expect_match(txt, "analysis set differs between outcomes")
})

test_that("indirectness_table.docx is written only where subdomains exist", {
  sub_tbl <- data.frame(
    subdomain = c("Population", "Intervention", "Comparator", "Outcome"),
    target    = c("Adults", "Drug A", "Placebo", "Mortality"),
    evidence  = c("Adults", "Drug A", "Placebo", "All-cause death"),
    judgment  = c("no", "no", "no", "probably_yes"),
    stringsAsFactors = FALSE
  )
  ml <- quiet_ma_multi(multi_data())
  set <- quiet_grade_multi(
    ml,
    common = list(study_design = "RCT", threshold_type = "null",
                  indirectness = "no"),
    per_outcome = list(
      "Mortality" = list(indirectness = NULL,
                         indirectness_subdomains = sub_tbl)
    )
  )
  expect_false(is.null(set$outcomes[["Mortality"]]$indirectness_subdomains))
  expect_null(set$outcomes[["Depression severity"]]$indirectness_subdomains)

  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "indir",
                  include = c("indirectness", "results")))
  files <- zip::zip_list(zip_path)$filename
  expect_true("outcomes/01_mortality/indirectness_table.docx" %in% files)
  expect_false("outcomes/02_depression_severity/indirectness_table.docx" %in% files)
})

# --------------------------------------------------------------------------
# 12. The bundled analysis.R round-trips
# --------------------------------------------------------------------------

test_that("the bundled multi-outcome analysis.R parses and reproduces the set", {
  sub_tbl <- data.frame(
    subdomain = c("Population", "Intervention", "Comparator", "Outcome"),
    target    = c("Adults", "Drug A", "Placebo", "Mortality"),
    evidence  = c("Adults", "Drug A", "Placebo", "All-cause death"),
    judgment  = c("no", "no", "no", "probably_yes"),
    stringsAsFactors = FALSE
  )
  d  <- multi_data()
  ml <- quiet_ma_multi(d, sm = list("Mortality" = "OR",
                                    "Depression severity" = "SMD",
                                    "Serious adverse events" = "RR"),
                       method.tau = "DL")
  set <- quiet_grade_multi(
    ml,
    common = list(study_design = "RCT", threshold_type = "null",
                  small_values = "undesirable", indirectness = "no",
                  rob_some_concerns = "high", rob_dominant_threshold = 0.60,
                  rob_refit = TRUE, follow_up = "12 months"),
    per_outcome = list(
      "Mortality" = list(
        threshold_type          = "mid",
        threshold               = 1.25,
        threshold_scale         = "ratio",
        require_threshold       = TRUE,
        rating_target           = "important_effect",
        rating_target_rationale = "Panel judged the point estimate important",
        indirectness            = NULL,
        indirectness_subdomains = sub_tbl,
        rob                     = rep("no", 5),
        rob_rationale           = "Consensus RoB2: all domains low risk",
        rob_overrides           = c("Adams 2019" = "serious"),
        rob_override_rationale  = c("Adams 2019" = "Unblinded outcome adjudication")
      )
    ),
    primary = "Mortality"
  )
  set <- reorder_outcomes(set, c("Depression severity", "Mortality",
                                 "Serious adverse events"))

  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "script",
                  include = c("data", "script")))
  ex <- tempfile(); dir.create(ex)
  zip::unzip(zip_path, exdir = ex)
  script <- file.path(ex, "analysis.R")
  expect_true(file.exists(script))

  txt <- readLines(script, warn = FALSE)
  expect_false(is.null(tryCatch(parse(text = paste(txt, collapse = "\n")),
                                error = function(e) NULL)))

  # Every Phase A-C argument survives into the script.
  joined <- paste(txt, collapse = "\n")
  for (arg in c("threshold_type", "rating_target", "rating_target_rationale",
                "require_threshold", "rob_some_concerns", "rob_overrides",
                "rob_override_rationale", "rob_dominant_threshold",
                "rob_refit", "indirectness_subdomains", "method.tau")) {
    expect_match(joined, arg, fixed = TRUE)
  }

  # Re-running it must rebuild the same set. library(pmatools) is dropped
  # because the package under test is loaded, not installed.
  txt <- sub("^library\\(pmatools\\)$", "", txt)
  writeLines(txt, script)
  owd <- setwd(ex); on.exit(setwd(owd), add = TRUE)
  env <- new.env(parent = environment())
  invisible(utils::capture.output(suppressWarnings(suppressMessages(
    eval(parse(text = paste(txt, collapse = "\n")), envir = env)))))
  setwd(owd)

  set2 <- get("set", envir = env)
  expect_s3_class(set2, "pmatools_set")
  expect_equal(set2$order, set$order)
  expect_equal(set2$primary, set$primary)
  cert <- function(s) vapply(s$outcomes[s$order],
                             function(g) g$certainty, character(1))
  expect_equal(cert(set2), cert(set))
  tgt <- function(s) vapply(s$outcomes[s$order],
                            function(g) g$rating_target %||% "", character(1))
  expect_equal(tgt(set2), tgt(set))
  expect_equal(set2$outcomes[["Mortality"]]$indirectness_subdomains$judgment,
               set$outcomes[["Mortality"]]$indirectness_subdomains$judgment)
  expect_equal(set2$outcomes[["Mortality"]]$follow_up, "12 months")
  expect_equal(set2$outcomes[["Depression severity"]]$meta$sm, "SMD")
})

test_that("a hand-built set exports without a script rather than a wrong one", {
  set <- make_set()
  set$grade_args <- NULL
  out_dir <- tempfile(); dir.create(out_dir)
  expect_warning(
    zip_path <- export_bundle(set, output_dir = out_dir,
                              bundle_name = "noscript",
                              include = c("script", "results")),
    regexp = "could not be rendered"
  )
  expect_false("analysis.R" %in% zip::zip_list(zip_path)$filename)
})

# --------------------------------------------------------------------------
# 13. The single-outcome bundle is untouched
# --------------------------------------------------------------------------

test_that("export_bundle on a single pmatools object produces the same flat ZIP", {
  ma <- meta::metabin(
    event.e = c(10, 15, 20), n.e = c(50, 60, 70),
    event.c = c(15, 20, 25), n.c = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm = "OR", method = "Inverse"
  )
  g <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                   rob_rationale = "Consensus RoB2: all low",
                                   indirectness = "no", outcome_name = "Test",
                                   threshold_type = "null"))
  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(ma, g, output_dir = out_dir, bundle_name = "flat",
                  include = c("data", "script", "results", "forest", "funnel",
                              "grade_table")))
  files <- sort(zip::zip_list(zip_path)$filename)
  expect_equal(files, sort(c("data_long.csv", "analysis.R", "results.txt",
                             "forest_plot.pdf", "forest_plot.png",
                             "funnel_plot.pdf", "funnel_plot.png",
                             "grade_table.docx", "sof_table.docx")))
  # Flat: nothing is nested under a directory.
  expect_false(any(grepl("/", files, fixed = TRUE)))

  # The pmatools method is a convenience wrapper over the same layout.
  zip2 <- suppressWarnings(
    export_bundle(g, output_dir = out_dir, bundle_name = "flat2",
                  include = c("data", "results")))
  expect_equal(sort(zip::zip_list(zip2)$filename),
               c("data_long.csv", "results.txt"))

  expect_error(export_bundle(data.frame(x = 1)), regexp = "must be a meta object")
})
