# test-indirectness_subdomains.R — Core GRADE 5 Indirectness subdomains
# (Population / Intervention / Comparison / Outcome, 4-point judgments).

library(testthat)

skip_if_not_installed("meta")

make_metabin_ind <- function() {
  meta::metabin(
    event.e = c(10, 15, 20),
    n.e     = c(50, 60, 70),
    event.c = c(15, 20, 25),
    n.c     = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm      = "RR",
    method  = "MH"
  )
}

# BMJ Core GRADE 5 worked example (heparin in advanced cancer)
bmj_subdomains <- function(outcome_judgment = "probably_no") {
  data.frame(
    subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
    target    = c("All patients with advanced cancer",
                  "Heparins (any type)",
                  "No anticoagulation",
                  "Symptomatic venous thromboembolism"),
    evidence  = c(paste("18 RCTs involving various cancer types. Populations",
                        "were representative of clinical practice."),
                  paste("Trials included both low molecular weight heparin and",
                        "unfractionated heparin."),
                  "All trials used placebo injections",
                  paste("Venous thromboembolism was identified through",
                        "screening and included both asymptomatic and",
                        "symptomatic cases.")),
    judgment  = c("yes", "yes", "yes", outcome_judgment),
    stringsAsFactors = FALSE
  )
}

grade_with_subdomains <- function(sub, ...) {
  suppressWarnings(
    grade_meta(make_metabin_ind(), threshold_type = "null",
               indirectness_subdomains = sub, ...)
  )
}

indir_row <- function(g) {
  d <- g$domain_assessments
  d[d$domain == "Indirectness", ]
}

# ---- worst-case aggregation ----------------------------------------------

test_that("all-yes subdomains give no downgrade", {
  sub <- bmj_subdomains(outcome_judgment = "yes")
  g <- grade_with_subdomains(sub)
  row <- indir_row(g)
  expect_equal(row$judgment, "no")
  expect_equal(row$downgrade, 0)
  expect_match(row$notes, "Overall \\(worst case\\): no\\.")
})

test_that("probably_yes still gives no downgrade", {
  sub <- bmj_subdomains(outcome_judgment = "probably_yes")
  expect_equal(indir_row(grade_with_subdomains(sub))$judgment, "no")
})

test_that("one probably_no subdomain gives some_concerns (worst case)", {
  g <- grade_with_subdomains(bmj_subdomains())
  row <- indir_row(g)
  expect_equal(row$judgment, "some_concerns")
  expect_equal(row$downgrade, -1)
  expect_match(row$notes, "Outcome: probably no")
  expect_match(row$notes, "Overall \\(worst case\\): some concerns\\.")
})

test_that("one 'no' subdomain gives serious (worst case, -2)", {
  sub <- bmj_subdomains(outcome_judgment = "no")
  row <- indir_row(grade_with_subdomains(sub))
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -2)
})

test_that("worst case wins over the order of subdomains", {
  sub <- data.frame(
    subdomain = c("Outcome", "Population"),
    judgment  = c("no", "yes"),
    stringsAsFactors = FALSE
  )
  expect_equal(indir_row(grade_with_subdomains(sub))$judgment, "serious")
})

# ---- input validation -----------------------------------------------------

test_that("invalid 4-point answers abort", {
  sub <- bmj_subdomains()
  sub$judgment[2] <- "maybe"
  expect_error(grade_with_subdomains(sub), regexp = "invalid value")
})

test_that("case and separator aliases are accepted", {
  sub <- data.frame(
    subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
    judgment  = c("Yes", "probably yes", "Probably-Yes", "Probably No"),
    stringsAsFactors = FALSE
  )
  g <- grade_with_subdomains(sub)
  expect_equal(g$indirectness_subdomains$judgment,
               c("yes", "probably_yes", "probably_yes", "probably_no"))
  expect_equal(indir_row(g)$judgment, "some_concerns")
})

test_that("duplicate subdomain labels abort", {
  sub <- data.frame(
    subdomain = c("Population", "population"),
    judgment  = c("yes", "no"),
    stringsAsFactors = FALSE
  )
  expect_error(grade_with_subdomains(sub), regexp = "duplicate label")
})

test_that("missing required columns abort", {
  sub <- data.frame(subdomain = "Population", stringsAsFactors = FALSE)
  expect_error(grade_with_subdomains(sub), regexp = "missing required column")
})

test_that("list input is accepted (rows and columns)", {
  as_rows <- list(
    list(subdomain = "Population", target = "Adults", evidence = "10 RCTs",
         judgment = "yes"),
    list(subdomain = "Outcome", judgment = "probably_no")
  )
  g_rows <- grade_with_subdomains(as_rows)
  expect_equal(indir_row(g_rows)$judgment, "some_concerns")
  expect_equal(nrow(g_rows$indirectness_subdomains), 2L)

  as_cols <- list(
    subdomain = c("Population", "Outcome"),
    judgment  = c("yes", "probably_no")
  )
  g_cols <- grade_with_subdomains(as_cols)
  expect_equal(indir_row(g_cols)$judgment, "some_concerns")
})

# ---- manual override gate -------------------------------------------------

test_that("override differing from the worst case requires a rationale", {
  expect_error(
    grade_with_subdomains(bmj_subdomains(), indirectness = "serious"),
    regexp = "Overriding the Indirectness judgment requires indirectness_rationale"
  )
})

test_that("BMJ worked example reproduces Serious Indirectness via override", {
  g <- grade_with_subdomains(
    bmj_subdomains(),
    indirectness          = "serious",
    indirectness_rationale = paste(
      "The evidence is directly relevant, though some studies raise",
      "indirectness concerns due to their reliance on surrogate outcomes."
    )
  )
  row <- indir_row(g)
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -2)
  expect_match(row$notes, "Manual override \\(serious\\): The evidence is directly relevant")
  expect_match(row$notes, "Worst-case default \\(some concerns\\) replaced")
  # Subdomain judgments are preserved alongside the override
  expect_equal(g$indirectness_subdomains$judgment,
               c("yes", "yes", "yes", "probably_no"))
})

test_that("restating the default judgment needs no rationale", {
  g <- grade_with_subdomains(bmj_subdomains(), indirectness = "some_concerns")
  row <- indir_row(g)
  expect_equal(row$judgment, "some_concerns")
  expect_false(grepl("Manual override", row$notes))
})

test_that("subdomains cannot be combined with per-study vector input", {
  expect_error(
    grade_with_subdomains(bmj_subdomains(),
                          indirectness = c("no", "no", "serious")),
    regexp = "cannot be combined with a per-study"
  )
})

test_that("subdomains cannot be combined with column-name input", {
  expect_error(
    grade_with_subdomains(bmj_subdomains(), indirectness = "indir_col"),
    regexp = "column-name input is not allowed"
  )
})

# ---- object schema --------------------------------------------------------

test_that("subdomains live on the object, not in domain_assessments", {
  g <- grade_with_subdomains(bmj_subdomains())
  sub <- g$indirectness_subdomains
  expect_s3_class(sub, "tbl_df")
  expect_equal(names(sub),
               c("subdomain", "target", "evidence", "judgment", "grade_level"))
  expect_equal(nrow(sub), 4L)
  expect_equal(sub$grade_level,
               c("no", "no", "no", "some_concerns"))

  d <- g$domain_assessments
  expect_equal(nrow(d), 5L)
  expect_true(all(vapply(d, is.atomic, logical(1))))
  expect_type(d$notes, "character")
})

test_that("calls without subdomains are unaffected", {
  g <- suppressWarnings(grade_meta(make_metabin_ind(), threshold_type = "null"))
  expect_null(g$indirectness_subdomains)
  expect_equal(indir_row(g)$judgment, "no")

  g2 <- suppressWarnings(
    grade_meta(make_metabin_ind(), threshold_type = "null",
               indirectness = "serious",
               indirectness_rationale = "Surrogate outcome only")
  )
  expect_equal(indir_row(g2)$judgment, "serious")
  expect_null(g2$indirectness_subdomains)
})

# ---- indirectness_table() -------------------------------------------------

test_that("indirectness_table returns a flextable and saves to docx", {
  skip_if_not_installed("flextable")
  g <- grade_with_subdomains(bmj_subdomains())
  ft <- indirectness_table(g)
  expect_s3_class(ft, "flextable")
  # 4 subdomain rows + "Judgment across subdomains"
  expect_equal(nrow(ft$body$dataset), 5L)
  expect_equal(ncol(ft$body$dataset), 6L)
  expect_true(any(grepl("Judgment across subdomains",
                        as.character(ft$body$dataset[[1]]))))
  expect_true(any(grepl("Serious indirectness",
                        as.character(ft$body$dataset[[3]]))))

  path <- tempfile(fileext = ".docx")
  flextable::save_as_docx(ft, path = path)
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("indirectness_table aborts without subdomain judgments", {
  g <- suppressWarnings(grade_meta(make_metabin_ind(), threshold_type = "null"))
  expect_error(indirectness_table(g),
               regexp = "no Indirectness subdomain judgments")
  expect_error(indirectness_table(list()),
               regexp = "must be a pmatools object")
})

test_that("indirectness_table shows the override rationale in the last row", {
  g <- grade_with_subdomains(
    bmj_subdomains(),
    indirectness           = "serious",
    indirectness_rationale = "Surrogate outcomes in several trials"
  )
  ft <- indirectness_table(g)
  expect_true(any(grepl("Surrogate outcomes in several trials",
                        as.character(ft$body$dataset[[2]]))))

  ft2 <- indirectness_table(g, summary_text = "Custom summary")
  expect_true(any(grepl("Custom summary", as.character(ft2$body$dataset[[2]]))))
})

# ---- reproducibility script ----------------------------------------------

test_that("analysis.R round-trips the subdomain table", {
  skip_if_not_installed("zip")
  ma <- make_metabin_ind()
  g <- grade_with_subdomains(
    bmj_subdomains(),
    indirectness           = "serious",
    indirectness_rationale = "Surrogate outcomes in several trials"
  )
  out_dir <- tempfile(); dir.create(out_dir)
  script <- file.path(out_dir, "analysis.R")
  pmatools:::.render_analysis_script(ma, g, NULL, NULL,
                                     per = 1000, prediction = FALSE,
                                     convert_smd_to_or = FALSE,
                                     baseline_risk = NULL,
                                     threshold_label = NULL,
                                     out_path = script)
  txt <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_true(grepl("indirectness_subdomains = data.frame", txt, fixed = TRUE))
  expect_true(grepl("Symptomatic venous thromboembolism", txt, fixed = TRUE))
  expect_true(grepl("\"probably_no\"", txt, fixed = TRUE))
  expect_true(grepl("indirectness            = 'serious'", txt, fixed = TRUE))
  expect_true(grepl("Surrogate outcomes in several trials", txt, fixed = TRUE))
  expect_silent(parse(script))
})

test_that("analysis.R omits the scalar override when the worst case stands", {
  skip_if_not_installed("zip")
  ma <- make_metabin_ind()
  g <- grade_with_subdomains(bmj_subdomains())
  out_dir <- tempfile(); dir.create(out_dir)
  script <- file.path(out_dir, "analysis.R")
  pmatools:::.render_analysis_script(ma, g, NULL, NULL,
                                     per = 1000, prediction = FALSE,
                                     convert_smd_to_or = FALSE,
                                     baseline_risk = NULL,
                                     threshold_label = NULL,
                                     out_path = script)
  txt <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_true(grepl("indirectness            = NULL", txt, fixed = TRUE))
  expect_true(grepl("indirectness_subdomains = data.frame", txt, fixed = TRUE))
  expect_silent(parse(script))
})
