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
               small_values = "desirable",
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
    small_values            = "desirable",
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
    small_values            = "desirable",
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
  base_args <- list(meta_obj = make_metabin_ind(), threshold_type = "null",
                    small_values = "desirable")
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
  g <- suppressWarnings(grade_meta(make_metabin_ind(), threshold_type = "null",
    small_values = "desirable"))
  expect_null(g$indirectness_subdomains)
  expect_equal(indir_row(g)$judgment, "not_serious")

  g2 <- suppressWarnings(
    grade_meta(make_metabin_ind(), threshold_type = "null",
               small_values = "desirable",
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

# Same two-line helper the other footer tests use (test-sof_bmj.R,
# test-grade_table_responder.R); testthat gives each file its own environment,
# so it is copied rather than shared.
.footer_text <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")

test_that("the indirectness footer explains the table, not the tool", {
  skip_if_not_installed("flextable")
  footer <- .footer_text(indirectness_table(grade_with_subdomains(
    bmj_subdomains())))

  # Which layout conventions are pmatools' own rather than Core GRADE 5's is
  # attribution, and it belongs where a reader looks it up once:
  # ?indirectness_table's @section Attribution, SPEC.md 4.13 and README.md.
  # In the footer it rode along on every exported copy of the table.
  expect_no_match(footer, "pmatools conventions", fixed = TRUE)
  expect_no_match(footer, "4-point answer scale", fixed = TRUE)
  expect_no_match(footer, "sufficiently direct", fixed = TRUE)

  # What stays is what a reader needs to read THIS table: the provenance the
  # disclaimer hangs off, the mark key, the rate-down rule, the worst-case
  # default, and the asymmetry Core GRADE 5 Table 2 grades and this fold does
  # not.
  expect_match(footer, "implemented by pmatools from the", fixed = TRUE)
  expect_match(footer, "Not an official GRADE Working Group assessment.",
               fixed = TRUE)
  expect_match(footer, "marks the recorded judgment", fixed = TRUE)
  expect_match(footer, "rates down 2 levels", fixed = TRUE)
  expect_match(footer, "worst case", fixed = TRUE)
  expect_match(footer, "asymmetrically", fixed = TRUE)
})

test_that("indirectness_table aborts without subdomain judgments", {
  g <- suppressWarnings(grade_meta(make_metabin_ind(), threshold_type = "null",
    small_values = "desirable"))
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

# ---- Core GRADE Guide Fig 8-2 answer codes --------------------------------

fig82_subdomains <- function(population = "no_difference",
                             intervention = "no_difference",
                             comparison = "no_difference",
                             outcome = "similar_definition") {
  data.frame(
    subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
    judgment  = c(population, intervention, comparison, outcome),
    stringsAsFactors = FALSE
  )
}

test_that("each Fig 8-2 code maps to the level of the leaf it reaches", {
  pic <- c(no_difference = "not_serious", similar = "not_serious",
           dissimilar = "serious", very_dissimilar = "very_serious")
  for (code in names(pic)) {
    g <- grade_with_subdomains(fig82_subdomains(intervention = code))
    expect_equal(indir_row(g)$judgment, unname(pic[code]), info = code)
  }
  out <- c(similar_definition = "not_serious",
           different_definition = "serious",
           surrogate_very_close = "not_serious",
           surrogate_close = "serious",
           surrogate_far = "very_serious")
  for (code in names(out)) {
    g <- grade_with_subdomains(fig82_subdomains(outcome = code))
    expect_equal(indir_row(g)$judgment, unname(out[code]), info = code)
  }
})

test_that("Fig 8-2 codes accept case and separator variants", {
  sub <- fig82_subdomains(population = "No difference",
                          comparison = "Very-Dissimilar",
                          outcome = "Surrogate far")
  g <- grade_with_subdomains(sub)
  expect_equal(g$indirectness_subdomains$judgment,
               c("no_difference", "no_difference", "very_dissimilar",
                 "surrogate_far"))
  expect_equal(indir_row(g)$judgment, "very_serious")
})

test_that("a Comparator label is the comparison element", {
  sub <- fig82_subdomains(comparison = "dissimilar")
  sub$subdomain[3] <- "Comparator"
  g <- grade_with_subdomains(sub)
  expect_equal(indir_row(g)$judgment, "serious")
  f <- domain_facts(g, "Indirectness")
  expect_match(f$value[f$key == "flow_path"], "pma-indirect-node-comparison",
               fixed = TRUE)
})

test_that("an answer from the other branch of the figure aborts", {
  expect_error(
    grade_with_subdomains(fig82_subdomains(outcome = "similar")),
    regexp = "cannot be used on the 'Outcome' row")
  expect_error(
    grade_with_subdomains(fig82_subdomains(population = "surrogate_far")),
    regexp = "cannot be used on the 'Population' row")
  # A custom label is not the Outcome branch, so Outcome codes are refused.
  expect_error(
    grade_with_subdomains(data.frame(subdomain = "Setting",
                                     judgment = "surrogate_close")),
    regexp = "Outcome branch")
  # Element matching is case-insensitive.
  expect_error(
    grade_with_subdomains(data.frame(subdomain = "outcome",
                                     judgment = "very_dissimilar")),
    regexp = "cannot be used on the 'outcome' row")
})

test_that("legacy answers keep their levels on every row", {
  legacy <- c(yes = "not_serious", probably_yes = "not_serious",
              probably_no = "serious", no = "very_serious")
  for (ans in names(legacy)) {
    for (row in 1:4) {
      sub <- fig82_subdomains()
      sub$judgment[row] <- ans
      g <- grade_with_subdomains(sub)
      expect_equal(indir_row(g)$judgment, unname(legacy[ans]),
                   info = paste(ans, row))
    }
  }
  # And the legacy-to-route map preserves the level it draws.
  map <- pmatools:::INDIRECTNESS_LEGACY_TO_PIC
  expect_identical(
    unname(pmatools:::INDIRECTNESS_ANSWER_TO_GRADE[names(map)]),
    unname(pmatools:::INDIRECTNESS_ANSWER_TO_GRADE[unname(map)]))
})

test_that("flow_path is the union of every row's route", {
  g <- grade_with_subdomains(fig82_subdomains(
    population = "no_difference", intervention = "similar",
    comparison = "very_dissimilar", outcome = "surrogate_close"))
  f <- domain_facts(g, "Indirectness")
  ids <- strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
  expected <- unique(c(
    pmatools:::.indirectness_row_route("population", "no_difference"),
    pmatools:::.indirectness_row_route("intervention", "similar"),
    pmatools:::.indirectness_row_route("comparison", "very_dissimilar"),
    pmatools:::.indirectness_row_route("outcome", "surrogate_close")))
  expect_setequal(ids, expected)
  expect_false(anyDuplicated(ids) > 0L)
  expect_true(all(c("pma-indirect-leaf-population-nodown",
                    "pma-indirect-leaf-similarity-nodown",
                    "pma-indirect-leaf-similarity-down2",
                    "pma-indirect-leaf-pathway-down1") %in% ids))
  # Not the routes nobody took.
  expect_false("pma-indirect-node-definition" %in% ids)
  expect_false("pma-indirect-leaf-similarity-down1" %in% ids)
})

test_that("legacy P/I/C answers are drawn on their Fig 8-2 routes", {
  g <- grade_with_subdomains(bmj_subdomains(outcome_judgment = "yes"))
  f <- domain_facts(g, "Indirectness")
  ids <- strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
  # yes -> no_difference on all three P/I/C rows.
  expect_true(all(paste0("pma-indirect-leaf-",
                         c("population", "intervention", "comparison"),
                         "-nodown") %in% ids))
  # The legacy Outcome answer stops at the surrogate question.
  expect_true("pma-indirect-node-surrogate" %in% ids)
  expect_false(any(c("pma-indirect-node-pathway",
                     "pma-indirect-node-definition") %in% ids))
  expect_match(indir_row(g)$notes,
               "route through Core GRADE Guide Fig 8-2 was not determined",
               fixed = TRUE)
  expect_match(indir_row(g)$notes, "pre-Fig-8-2 scale", fixed = TRUE)
})

test_that("domain notes name each row's route in plain words", {
  g <- grade_with_subdomains(fig82_subdomains(
    intervention = "dissimilar", outcome = "surrogate_far"))
  notes <- indir_row(g)$notes
  expect_match(notes,
               "Intervention: differences; factors dissimilar (rate down once)",
               fixed = TRUE)
  expect_match(notes, paste0("Outcome: surrogate, far from patient-important ",
                             "outcome (rate down twice)"),
               fixed = TRUE)
  expect_false(grepl(" | ", notes, fixed = TRUE))
  expect_false(grepl("was not determined", notes, fixed = TRUE))
})

test_that("an override keeps the route it overrode", {
  g <- grade_with_subdomains(
    fig82_subdomains(outcome = "surrogate_close"),
    indirectness = "very_serious",
    indirectness_rationale = "Panel judged the surrogate far, not close")
  expect_equal(indir_row(g)$judgment, "very_serious")
  f <- domain_facts(g, "Indirectness")
  expect_match(f$value[f$key == "flow_path"],
               "pma-indirect-leaf-pathway-down1", fixed = TRUE)
})

test_that("scalar indirectness records no flow_path", {
  g <- suppressWarnings(grade_meta(make_metabin_ind(), threshold_type = "null",
                                   small_values = "desirable",
                                   indirectness = "very_serious",
                                   indirectness_rationale = "Older population"))
  expect_null(domain_facts(g, "Indirectness"))
})

test_that("indirectness_table renders Fig 8-2 answers readably", {
  skip_if_not_installed("flextable")
  g <- grade_with_subdomains(fig82_subdomains(
    comparison = "very_dissimilar", outcome = "surrogate_far"))
  ft <- indirectness_table(g)
  answers <- as.character(ft$body$dataset[[3]])
  expect_true("Differences; factors very dissimilar" %in% answers)
  expect_true("Surrogate, far from patient-important outcome" %in% answers)
  expect_true("No difference from the target" %in% answers)
  # The mark sits under the leaf each answer reached.
  twice <- as.character(ft$body$dataset[[6]])
  expect_identical(twice[3:4], rep("\u2611", 2))
  expect_identical(twice[1], "\u2610")

  # Legacy answers say which scale they are on.
  g2 <- grade_with_subdomains(bmj_subdomains())
  expect_true("Probably no, on the pre-Fig 8-2 scale" %in%
                as.character(indirectness_table(g2)$body$dataset[[3]]))
})

test_that("the machine-only flow_path does not count as a footnote reason", {
  # Indirectness's only fact is its Fig 8-2 route, which no prose renderer
  # prints. Counting it would give the domain a footnote marker with nothing
  # under it.
  g <- grade_with_subdomains(fig82_subdomains(outcome = "surrogate_close"))
  expect_false("Indirectness" %in% pmatools:::.rated_down_fact_domains(g))

  g2 <- grade_with_subdomains(fig82_subdomains(outcome = "surrogate_close"),
                              indirectness = "very_serious",
                              indirectness_rationale = "Surrogate is far")
  expect_true("Indirectness" %in% pmatools:::.rated_down_fact_domains(g2))
})
