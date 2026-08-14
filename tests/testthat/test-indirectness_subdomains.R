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

# Illustrative fixture: heparin in advanced cancer, written by pmatools to
# exercise the subdomain table. It is NOT a Core GRADE 5 worked example -- the
# article body contains no such table and never mentions heparin or venous
# thromboembolism (verified against the published PDF).
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
  expect_equal(row$judgment, "not_serious")
  expect_equal(row$downgrade, 0)
  # Domain notes render the Core GRADE 1 wording, which since 0.5.1 is also
  # what the stored level is spelled like bar the underscore.
  expect_match(row$notes, "Overall \\(worst case\\): not serious indirectness\\.")
})

test_that("probably_yes still gives no downgrade", {
  sub <- bmj_subdomains(outcome_judgment = "probably_yes")
  expect_equal(indir_row(grade_with_subdomains(sub))$judgment, "not_serious")
})

test_that("one probably_no subdomain gives serious (worst case)", {
  g <- grade_with_subdomains(bmj_subdomains())
  row <- indir_row(g)
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -1)
  expect_match(row$notes, "Outcome: probably no")
  expect_match(row$notes, "Overall \\(worst case\\): serious indirectness\\.")
})

test_that("one 'no' subdomain gives very_serious (worst case, -2)", {
  sub <- bmj_subdomains(outcome_judgment = "no")
  row <- indir_row(grade_with_subdomains(sub))
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2)
})

test_that("worst case wins over the order of subdomains", {
  sub <- data.frame(
    subdomain = c("Outcome", "Population"),
    judgment  = c("no", "yes"),
    stringsAsFactors = FALSE
  )
  expect_equal(indir_row(grade_with_subdomains(sub))$judgment, "very_serious")
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
  expect_equal(indir_row(g)$judgment, "serious")
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
  expect_equal(indir_row(g_rows)$judgment, "serious")
  expect_equal(nrow(g_rows$indirectness_subdomains), 2L)

  as_cols <- list(
    subdomain = c("Population", "Outcome"),
    judgment  = c("yes", "probably_no")
  )
  g_cols <- grade_with_subdomains(as_cols)
  expect_equal(indir_row(g_cols)$judgment, "serious")
})

# ---- manual override gate -------------------------------------------------

test_that("override differing from the worst case requires a rationale", {
  expect_error(
    grade_with_subdomains(bmj_subdomains(), indirectness = "very_serious"),
    regexp = "Overriding the Indirectness judgment requires indirectness_rationale"
  )
})

test_that("the illustrative fixture reaches very serious indirectness via override", {
  g <- grade_with_subdomains(
    bmj_subdomains(),
    indirectness          = "very_serious",
    indirectness_rationale = paste(
      "The evidence is directly relevant, though some studies raise",
      "indirectness concerns due to their reliance on surrogate outcomes."
    )
  )
  row <- indir_row(g)
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2)
  expect_match(row$notes, "Manual override \\(very_serious\\): The evidence is directly relevant")
  expect_match(row$notes, "Worst-case default \\(serious indirectness\\) replaced")
  # Subdomain judgments are preserved alongside the override
  expect_equal(g$indirectness_subdomains$judgment,
               c("yes", "yes", "yes", "probably_no"))
})

test_that("restating the default judgment needs no rationale", {
  g <- grade_with_subdomains(bmj_subdomains(), indirectness = "some_concerns")
  row <- indir_row(g)
  expect_equal(row$judgment, "serious")
  expect_false(grepl("Manual override", row$notes))
})

test_that("subdomains cannot be combined with per-study vector input", {
  expect_error(
    grade_with_subdomains(bmj_subdomains(),
                          indirectness = c("no", "no", "very_serious")),
    regexp = "cannot be combined with a per-study"
  )
})

test_that("the override error tells the user how to keep the worst case", {
  expect_error(
    grade_with_subdomains(bmj_subdomains(), indirectness = "very_serious"),
    regexp = "omit `indirectness` or pass `indirectness = NULL`",
    fixed  = FALSE
  )
  expect_error(
    grade_with_subdomains(bmj_subdomains(), indirectness = "very_serious"),
    regexp = "worst-case subdomain judgment \\(serious indirectness\\)"
  )
})

# Regression: the override used to be detected with missing(), which is FALSE
# for do.call() and for UIs that always pass every argument. Forwarding the
# old "no" default then looked like an override and aborted on the missing
# rationale. NULL — not missing() — now encodes "no manual judgment".
test_that("do.call() with indirectness = NULL keeps the subdomain worst case", {
  args <- list(
    meta_obj                = make_metabin_ind(),
    threshold_type          = "null",
    indirectness            = NULL,
    indirectness_rationale  = NULL,
    indirectness_subdomains = bmj_subdomains()
  )
  g <- suppressWarnings(do.call(grade_meta, args))
  expect_equal(indir_row(g)$judgment, "serious")
  expect_match(indir_row(g)$notes, "Overall \\(worst case\\): serious indirectness\\.")
  expect_false(grepl("Manual override", indir_row(g)$notes))
})

test_that("do.call() forwarding an explicit 'no' is still an override", {
  # Explicitly saying "no" alongside a worst case of some_concerns remains a
  # deliberate override and keeps requiring a rationale.
  args <- list(
    meta_obj                = make_metabin_ind(),
    threshold_type          = "null",
    indirectness            = "no",
    indirectness_subdomains = bmj_subdomains()
  )
  expect_error(suppressWarnings(do.call(grade_meta, args)),
               regexp = "requires indirectness_rationale")

  args$indirectness_rationale <- "Panel judged the outcome definition adequate"
  g <- suppressWarnings(do.call(grade_meta, args))
  expect_equal(indir_row(g)$judgment, "not_serious")
  expect_match(indir_row(g)$notes, "Worst-case default \\(serious indirectness\\) replaced")
})

test_that("do.call() without subdomains is unchanged by the NULL default", {
  base_args <- list(meta_obj = make_metabin_ind(), threshold_type = "null")
  g_omitted <- suppressWarnings(do.call(grade_meta, base_args))
  g_null    <- suppressWarnings(
    do.call(grade_meta, c(base_args, list(indirectness = NULL)))
  )
  g_no      <- suppressWarnings(
    do.call(grade_meta, c(base_args, list(indirectness = "no")))
  )
  expect_equal(indir_row(g_omitted)$judgment, "not_serious")
  expect_equal(indir_row(g_null)$judgment, "not_serious")
  expect_equal(indir_row(g_no)$judgment, "not_serious")
  expect_equal(indir_row(g_null)$notes, indir_row(g_no)$notes)
  expect_equal(indir_row(g_omitted)$notes, indir_row(g_no)$notes)
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
               c("not_serious", "not_serious", "not_serious", "serious"))

  d <- g$domain_assessments
  expect_equal(nrow(d), 5L)
  expect_true(all(vapply(d, is.atomic, logical(1))))
  expect_type(d$notes, "character")
})

test_that("calls without subdomains are unaffected", {
  g <- suppressWarnings(grade_meta(make_metabin_ind(), threshold_type = "null"))
  expect_null(g$indirectness_subdomains)
  expect_equal(indir_row(g)$judgment, "not_serious")

  g2 <- suppressWarnings(
    grade_meta(make_metabin_ind(), threshold_type = "null",
               indirectness = "very_serious",
               indirectness_rationale = "Surrogate outcome only")
  )
  expect_equal(indir_row(g2)$judgment, "very_serious")
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
    indirectness           = "very_serious",
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
    indirectness           = "very_serious",
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
  expect_true(grepl("indirectness            = 'very_serious'", txt, fixed = TRUE))
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
