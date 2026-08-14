library(testthat)

skip_if_not_installed("meta")
skip_if_not_installed("zip")

# --------------------------------------------------------------------------
# Fixtures (same shape as test-multi_outcome.R)
# --------------------------------------------------------------------------

STUDIES_NR <- c("Adams 2019", "Baker 2020", "Chen 2021", "Diaz 2022")

bin_rows_nr <- function(outcome, studlab, ee, ne, ec, nc, rob) {
  rbind(
    data.frame(studlab = studlab, outcome = outcome, treat = "experimental",
               n = ne, event = ee, mean = NA_real_, sd = NA_real_, rob = rob,
               stringsAsFactors = FALSE),
    data.frame(studlab = studlab, outcome = outcome, treat = "control",
               n = nc, event = ec, mean = NA_real_, sd = NA_real_, rob = rob,
               stringsAsFactors = FALSE)
  )
}

# Two binary outcomes, all studies at low risk of bias (so no Core GRADE 4
# Fig 2 refit unless a test asks for one).
nr_raw <- function(rob = rep("no", length(STUDIES_NR))) {
  rbind(
    bin_rows_nr("Mortality", STUDIES_NR,
                c(10, 12, 8, 15), c(80, 90, 70, 100),
                c(18, 20, 15, 25), c(80, 90, 70, 100), rob),
    bin_rows_nr("Serious adverse events", STUDIES_NR,
                c(3, 4, 2, 5), c(80, 90, 70, 100),
                c(2, 3, 2, 4), c(80, 90, 70, 100), rob)
  )
}

nr_data <- function(...) suppressMessages(ingest_data(nr_raw(...),
                                                      format = "long"))

quiet_ma   <- function(...) suppressWarnings(run_ma_multi(...))
quiet_grade <- function(...) suppressWarnings(grade_meta_multi(...))

# A two-outcome set plus one outcome nobody reported.
make_nr_set <- function(reason = NULL, rob = rep("no", length(STUDIES_NR))) {
  ml  <- quiet_ma(nr_data(rob), sm = "RR")
  set <- quiet_grade(
    ml,
    common = list(study_design = "RCT", threshold_type = "null",
                  indirectness = "no", small_values = "undesirable"),
    primary = "Mortality"
  )
  add_not_reported(set, "Quality of life", follow_up = "12 months",
                   reason = reason)
}

# A rated outcome whose two arms are identical in every study: the pooled
# estimate is exactly the null on the TE scale, so .plain_language_for() has no
# direction to put in a Core GRADE 6 Box 1 frame and returns NULL. A table of
# such outcomes carried no plain-language column before not-reported rows
# existed, and must not gain one because of them.
make_no_plain_set <- function() {
  raw <- bin_rows_nr("Mortality", STUDIES_NR,
                     c(10, 12, 8, 15), c(80, 90, 70, 100),
                     c(10, 12, 8, 15), c(80, 90, 70, 100),
                     rep("no", length(STUDIES_NR)))
  ml  <- quiet_ma(suppressMessages(ingest_data(raw, format = "long")),
                  sm = "RR")
  quiet_grade(ml, common = list(study_design = "RCT", threshold_type = "null",
                                indirectness = "no",
                                small_values = "undesirable"))
}

# A single rated outcome plus one not-reported outcome.
make_one_rated_set <- function() {
  ml  <- quiet_ma(nr_data(), outcomes = "Mortality", sm = "RR")
  set <- quiet_grade(ml, common = list(study_design = "RCT",
                                       threshold_type = "null",
                                       indirectness = "no",
                                       small_values = "undesirable"))
  add_not_reported(set, "Quality of life")
}

.nr_footer <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")
.nr_body   <- function(ft, j) as.character(ft$body$dataset[[j]])
.nr_row    <- function(ft, pattern) grep(pattern, .nr_body(ft, 1L))[1]

# --------------------------------------------------------------------------
# 1. Construction and validation
# --------------------------------------------------------------------------

test_that("not_reported_outcome() validates its arguments", {
  nr <- not_reported_outcome("Quality of life")
  expect_s3_class(nr, "pmatools_not_reported")
  # It must NOT be a pmatools object: every existing guard has to reject it.
  expect_false(inherits(nr, "pmatools"))
  expect_equal(nr$outcome_name, "Quality of life")
  expect_null(nr$follow_up)
  expect_null(nr$reason)
  expect_equal(nr$label, "Not reported")

  expect_error(not_reported_outcome(""), regexp = "non-empty")
  expect_error(not_reported_outcome(NA_character_), regexp = "non-empty")
  expect_error(not_reported_outcome(c("a", "b")), regexp = "non-empty")
  expect_error(not_reported_outcome("QoL", label = ""), regexp = "'label'")
  expect_error(not_reported_outcome("QoL", label = NULL), regexp = "'label'")
  expect_error(not_reported_outcome("QoL", follow_up = 12),
               regexp = "'follow_up'")

  # Empty / NA optional strings normalise to NULL, so downstream code only
  # ever tests for NULL.
  expect_null(not_reported_outcome("QoL", reason = "")$reason)
  expect_null(not_reported_outcome("QoL", reason = NA_character_)$reason)

  txt <- paste(utils::capture.output(
    print(not_reported_outcome("QoL", follow_up = "6 mo", reason = "None"))),
    collapse = "\n")
  expect_match(txt, "QoL")
  expect_match(txt, "no certainty rating")
})

test_that("add_not_reported() guards names and honours 'after'", {
  ml  <- quiet_ma(nr_data(), sm = "RR")
  set <- quiet_grade(ml, common = list(study_design = "RCT",
                                       threshold_type = "null",
                                       indirectness = "no"))

  s1 <- add_not_reported(set, "Quality of life")
  expect_equal(s1$order, c("Mortality", "Serious adverse events",
                           "Quality of life"))
  expect_true(.is_not_reported(s1$outcomes[["Quality of life"]]))

  s2 <- add_not_reported(set, "Quality of life", after = "Mortality")
  expect_equal(s2$order[2], "Quality of life")
  s3 <- add_not_reported(set, "Quality of life", after = 0)
  expect_equal(s3$order[1], "Quality of life")

  expect_error(add_not_reported(set, "Mortality"), regexp = "already holds")
  expect_error(add_not_reported(set, "QoL", after = "Nope"),
               regexp = "not in the set")
  expect_error(add_not_reported(set, "QoL", after = -1), regexp = "'after'")
  expect_error(add_not_reported(list(), "QoL"), regexp = "pmatools_set")
})

# --------------------------------------------------------------------------
# 2. GRADEpro layout
# --------------------------------------------------------------------------

test_that("the gradepro row reads 'Not reported' and 'Not rated'", {
  set <- make_nr_set()
  ft  <- grade_table(set, style = "gradepro", show_domains = TRUE)
  expect_s3_class(ft, "flextable")

  i <- .nr_row(ft, "Quality of life")
  expect_false(is.na(i))
  # Follow-up rides under the name, as it does in the BMJ outcome cell.
  expect_match(.nr_body(ft, 1L)[i], "12 months")

  for (j in 2:5) expect_equal(.nr_body(ft, j)[i], "Not reported")
  expect_equal(.nr_body(ft, 6L)[i], "Not rated")
  # No certainty symbol: the symbols are a four-level scale this row is not on.
  expect_false(grepl("+", .nr_body(ft, 6L)[i], fixed = TRUE))

  # Domain cells: the not-reported symbol (an en dash), visually distinct from
  # "?" (= judgment unknown). Compared against the constant, not a literal, so
  # the test cannot drift from it and this file stays ASCII.
  for (j in 7:11) {
    expect_equal(.nr_body(ft, j)[i], NOT_REPORTED_DOMAIN_SYMBOL)
    expect_false(identical(.nr_body(ft, j)[i], "?"))
  }

  # A rated row is untouched.
  im <- .nr_row(ft, "^Mortality")
  expect_false(.nr_body(ft, 6L)[im] == "Not rated")
})

# --------------------------------------------------------------------------
# 3. BMJ layout
# --------------------------------------------------------------------------

test_that("the bmj row reads 'Not reported', including the Difference cell", {
  set <- make_nr_set()
  ft  <- grade_table(set, style = "bmj")

  i <- .nr_row(ft, "Quality of life")
  expect_false(is.na(i))
  expect_match(.nr_body(ft, 1L)[i], "12 months")

  # n / effect / control / intervention / Difference
  for (j in 2:6) expect_equal(.nr_body(ft, j)[i], "Not reported")
  expect_equal(.nr_body(ft, 6L)[i], "Not reported")   # Difference, explicitly
  expect_equal(.nr_body(ft, 7L)[i], "Not rated")
  expect_match(.nr_body(ft, 8L)[i], "No included study reported this outcome")
})

test_that("a not-reported row alone does not add the plain language column", {
  set <- make_no_plain_set()
  skip_if_not(is.null(.plain_language_for(set$outcomes[["Mortality"]])),
              "fixture's rated outcome does carry a Box 1 statement")

  # Baseline: no such column before the not-reported row is added.
  ft_rated <- grade_table(set, style = "bmj")
  expect_false("Plain language summary" %in% names(ft_rated$body$dataset))

  # ... and adding one must not conjure it, nor the Box 1 footer that would
  # then misattribute the not-reported sentence to Box 1.
  ft <- grade_table(add_not_reported(set, "Quality of life"), style = "bmj")
  expect_false("Plain language summary" %in% names(ft$body$dataset))
  expect_no_match(.nr_footer(ft), "Core GRADE 6 box 1", fixed = TRUE)

  # The positive direction is unchanged: when the rated outcomes do supply a
  # Box 1 statement the column exists, and the not-reported row fills it with
  # its own sentence.
  ft2 <- grade_table(make_nr_set(), style = "bmj")
  j   <- match("Plain language summary", names(ft2$body$dataset))
  expect_false(is.na(j))
  expect_match(.nr_body(ft2, j)[.nr_row(ft2, "Quality of life")],
               "No included study reported this outcome")
  expect_match(.nr_footer(ft2), "Core GRADE 6 box 1", fixed = TRUE)
})

# --------------------------------------------------------------------------
# 4. The shared footnote appears exactly once, in both layouts
# --------------------------------------------------------------------------

test_that("the not-reported footnote is emitted once per table", {
  set <- make_nr_set()
  # Two not-reported outcomes: the note is about the label, not the row.
  set <- add_not_reported(set, "Cost")

  # One footer *row* per note; add_footer_lines() repeats the text across the
  # row's cells, so the first column is what has to be counted.
  count_note <- function(ft) {
    sum(grepl("prespecified in the review but no included study",
              as.character(ft$footer$dataset[[1]]), fixed = TRUE))
  }
  expect_equal(count_note(grade_table(set, style = "gradepro")), 1L)
  expect_equal(count_note(grade_table(set, style = "bmj")), 1L)

  # A table without such a row never mentions it.
  ml   <- quiet_ma(nr_data(), sm = "RR")
  rated <- quiet_grade(ml, common = list(study_design = "RCT",
                                         threshold_type = "null",
                                         indirectness = "no"))
  expect_equal(count_note(grade_table(rated, style = "bmj")), 0L)
})

# --------------------------------------------------------------------------
# 5. The reason shares the numbered footnote pool with the RoB notes
# --------------------------------------------------------------------------

test_that("a reason gets a [n] marker alongside a real analysis-set note", {
  set <- make_nr_set(reason = "Measured in two trials, reported only as a figure")
  ft  <- grade_table(set, style = "bmj")
  i   <- .nr_row(ft, "Quality of life")
  expect_match(.nr_body(ft, 1L)[i], "\\[1\\]")
  expect_match(.nr_footer(ft), "[1] Not reported: Measured in two trials",
               fixed = TRUE)

  # Now a set whose rated outcomes carry their own analysis-set notes, so both
  # kinds of note share one numbered pool.
  ml   <- quiet_ma(nr_data(c("very_serious", "no", "no", "no")), sm = "OR")
  set2 <- quiet_grade(
    ml,
    # rob_inflation_threshold pinned at the pre-0.5.1 default. This fixture's
    # inflation sits between the two values, so on the current default of 0.20
    # it stops refitting, the analysis-set note it exists to produce never
    # appears, and the skip below silently retires the numbering assertions.
    # The subject here is the shared footnote pool, not the threshold.
    common = list(study_design = "RCT", indirectness = "no",
                  small_values = "undesirable",
                  rob = c("very_serious", "no", "no", "no"),
                  rob_inflation_threshold = 0.10,
                  threshold = 1.05, threshold_scale = "ratio")
  )
  skip_if_not(any(vapply(set2$outcomes, function(g) isTRUE(g$rob_refit),
                         logical(1))),
              "fixture did not trigger the low-RoB refit")

  set2 <- add_not_reported(set2, "Quality of life",
                           reason = "Nobody measured it")
  ft2     <- grade_table(set2, style = "gradepro")
  footer2 <- .nr_footer(ft2)
  expect_match(footer2, "low risk of bias")
  expect_match(footer2, "Not reported: Nobody measured it", fixed = TRUE)

  # The not-reported marker is the last number issued, after the RoB ones, and
  # it is the number its own footer line carries.
  i2  <- .nr_row(ft2, "Quality of life")
  mk  <- sub(".*\\[([0-9]+)\\].*", "\\1", .nr_body(ft2, 1L)[i2])
  expect_true(nzchar(mk) && !is.na(suppressWarnings(as.integer(mk))))
  expect_match(footer2, paste0("[", mk, "] Not reported: Nobody measured it"),
               fixed = TRUE)
  expect_true(as.integer(mk) > 1L)
})

# --------------------------------------------------------------------------
# 6. reorder_outcomes() / set_primary() keep working
# --------------------------------------------------------------------------

test_that("a not-reported row can be reordered and marked primary", {
  set <- make_nr_set()
  set <- reorder_outcomes(set, c("Quality of life", "Mortality",
                                 "Serious adverse events"))
  set <- set_primary(set, "Quality of life")
  expect_equal(set$order[1], "Quality of life")
  expect_equal(set$primary, "Quality of life")

  for (style in c("gradepro", "bmj")) {
    ft   <- grade_table(set, style = style)
    col1 <- .nr_body(ft, 1L)
    # Row 1 is the "Primary outcome" group header, row 2 the not-reported row.
    expect_match(col1[1], "Primary outcome")
    expect_match(col1[2], "Quality of life")
    expect_match(col1[3], "Secondary outcomes")
    expect_true(grep("^Mortality", col1)[1] > 3L)
  }
})

# --------------------------------------------------------------------------
# 7. The effect header of a homogeneous table is not degraded
# --------------------------------------------------------------------------

test_that("one not-reported outcome keeps the sm-specific effect header", {
  set <- make_one_rated_set()

  ft <- grade_table(set, style = "gradepro")
  expect_equal(names(ft$body$dataset)[5], "Relative effect\n(95% CI)")

  ft_bmj <- grade_table(set, style = "bmj")
  expect_equal(names(ft_bmj$body$dataset)[3], "Relative effect\n(95% CI)")
  expect_no_match(.nr_footer(ft_bmj), "different effect measures", fixed = TRUE)
})

# --------------------------------------------------------------------------
# 8. Single-outcome entry points refuse it, and say where it belongs
# --------------------------------------------------------------------------

test_that("sof_table() and evidence_profile() refuse a not-reported outcome", {
  nr <- not_reported_outcome("Quality of life")
  expect_error(sof_table(nr), regexp = "no analysis to summarise")
  expect_error(sof_table(nr), regexp = "grade_table")
  expect_error(evidence_profile(nr), regexp = "five domains")
  expect_error(evidence_profile(nr), regexp = "grade_table")
  expect_error(export_bundle(nr), regexp = "add_not_reported")
})

# --------------------------------------------------------------------------
# 9. Export bundle
# --------------------------------------------------------------------------

test_that("export_bundle() keeps a numbered directory with a results.txt", {
  set <- make_nr_set(reason = "No included trial measured it")
  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "nr",
                  include = c("sof", "script", "results", "readme")))
  expect_true(file.exists(zip_path))

  files <- zip::zip_list(zip_path)$filename
  expect_true("outcomes/03_quality_of_life/results.txt" %in% files)

  ex <- tempfile(); dir.create(ex)
  zip::unzip(zip_path, exdir = ex)

  txt <- paste(readLines(file.path(ex, "outcomes/03_quality_of_life",
                                   "results.txt")), collapse = "\n")
  expect_match(txt, "Quality of life")
  expect_match(txt, "No included trial measured it")

  csv <- utils::read.csv(file.path(ex, "summary_of_findings.csv"),
                         stringsAsFactors = FALSE)
  row <- csv[csv$outcome == "Quality of life", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$certainty, "Not rated")
  expect_equal(row$participants, "Not reported")
  expect_equal(row$difference, "Not reported")
  expect_equal(row$analysis_set, "not reported")
  expect_match(row$plain_language, "No included study reported")

  script <- paste(readLines(file.path(ex, "analysis.R")), collapse = "\n")
  expect_match(script, "add_not_reported(", fixed = TRUE)
  expect_match(script, "Quality of life", fixed = TRUE)
  expect_silent(parse(text = script))

  readme <- paste(readLines(file.path(ex, "README.txt")), collapse = "\n")
  expect_match(readme, "not reported")
})

test_that("the not-reported results.txt is gated on include, like every other", {
  set <- make_nr_set(reason = "No included trial measured it")
  out_dir <- tempfile(); dir.create(out_dir)

  # No per-outcome artifact was asked for, so there is no outcomes/ tree at
  # all - the same ZIP shape a set of only rated outcomes would produce.
  zip_sof <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "nr_sof",
                  include = "sof"))
  expect_false(any(grepl("^outcomes/", zip::zip_list(zip_sof)$filename)))

  # The default include does ask for "results", so the file is still there.
  zip_def <- suppressWarnings(
    export_bundle(set, output_dir = out_dir, bundle_name = "nr_default"))
  expect_true("outcomes/03_quality_of_life/results.txt" %in%
                zip::zip_list(zip_def)$filename)
})

# --------------------------------------------------------------------------
# 10. grade_report()
# --------------------------------------------------------------------------

test_that("grade_report(format = 'md') carries the outcome and the label", {
  set <- make_nr_set(reason = "No included trial measured it")
  out_dir <- tempfile(); dir.create(out_dir)
  path <- suppressMessages(
    grade_report(set, format = "md", output_dir = out_dir,
                 output_file = "nr_report"))
  md <- paste(readLines(path[1]), collapse = "\n")

  expect_match(md, "Quality of life")
  expect_match(md, "Not reported", fixed = TRUE)
  expect_match(md, "Not rated", fixed = TRUE)
  expect_match(md, "No included trial measured it", fixed = TRUE)
  # No domain table for that outcome, but the rated ones still have theirs.
  expect_match(md, "| Domain | Judgment | Downgrade | Notes |", fixed = TRUE)
})

# --------------------------------------------------------------------------
# 11. print() / summary() on a set holding one
# --------------------------------------------------------------------------

test_that("print() and summary() survive a not-reported member", {
  set <- make_nr_set(reason = "No included trial measured it")

  txt <- paste(utils::capture.output(print(set)), collapse = "\n")
  expect_match(txt, "Quality of life")
  expect_match(txt, "<not reported>", fixed = TRUE)
  # ... and the analysis-set note must not fire: every rated outcome shares one.
  expect_no_match(txt, "analysis set differs between outcomes", fixed = TRUE)

  s <- paste(utils::capture.output(summary(set)), collapse = "\n")
  expect_match(s, "No included study reported this outcome")
  expect_match(s, "No included trial measured it", fixed = TRUE)
  # The rated outcomes still print their domain tables.
  expect_match(s, "Imprecision")
})
