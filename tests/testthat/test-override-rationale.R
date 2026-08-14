# test-override-rationale.R — v0.4.0 breaking change:
# manual overrides of automated domain judgments require a rationale
# (Core GRADE transparency principle).

library(testthat)
library(meta)

skip_if_not_installed("meta")

make_metabin_or <- function() {
  metabin(
    event.e = c(10, 15, 20),
    n.e     = c(50, 60, 70),
    event.c = c(15, 20, 25),
    n.c     = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm      = "RR",
    method  = "MH"
  )
}

# k >= 10 so the publication-bias manual funnel-asymmetry branch is exercised
make_metabin_k10 <- function() {
  set.seed(42)
  metabin(
    event.e = c(10, 12, 14, 9, 11, 13, 15, 10, 12, 14),
    n.e     = rep(60, 10),
    event.c = c(15, 17, 19, 14, 16, 18, 20, 15, 17, 19),
    n.c     = rep(60, 10),
    studlab = paste("Study", 1:10),
    sm      = "RR",
    method  = "MH"
  )
}

domain_row <- function(g, domain) {
  g$domain_assessments[g$domain_assessments$domain == domain, ]
}

# ---- Risk of bias ---------------------------------------------------------

test_that("rob scalar override with rationale succeeds and records it in notes", {
  m <- make_metabin_or()
  g <- suppressWarnings(
    grade_meta(m, rob = "very_serious",
               rob_rationale = "RoB2 consensus: high risk in most domains", threshold_type = "null")
  )
  row <- domain_row(g, "Risk of bias")
  expect_equal(row$judgment, "very_serious")
  expect_false(row$auto)
  expect_match(row$notes, "Manual override \\(very_serious\\): RoB2 consensus")
})

test_that("rob scalar override without rationale errors", {
  m <- make_metabin_or()
  expect_error(
    grade_meta(m, rob = "very_serious", threshold_type = "null"),
    regexp = "Overriding the Risk of Bias judgment requires rob_rationale"
  )
})

test_that("rob scalar override with empty or whitespace rationale errors", {
  m <- make_metabin_or()
  expect_error(grade_meta(m, rob = "no", rob_rationale = "", threshold_type = "null"),
               regexp = "rob_rationale")
  expect_error(grade_meta(m, rob = "no", rob_rationale = "   ", threshold_type = "null"),
               regexp = "rob_rationale")
})

test_that("rob per-study vector (automated path) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, rob = c("no", "some", "very_serious"), threshold_type = "null"))
  row <- domain_row(g, "Risk of bias")
  expect_match(row$notes, "by count")
  expect_no_match(row$notes, "Manual override")
})

test_that("rob = NULL (default) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null"))
  row <- domain_row(g, "Risk of bias")
  expect_equal(row$judgment, "not_serious")
})

# ---- Indirectness ---------------------------------------------------------

test_that("indirectness = 'no' (default value) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, indirectness = "no", threshold_type = "null"))
  row <- domain_row(g, "Indirectness")
  expect_equal(row$judgment, "not_serious")
  expect_no_match(row$notes, "Manual override")
})

test_that("indirectness scalar other than 'no' requires rationale", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, indirectness = "very_serious", threshold_type = "null")),
    regexp = "Overriding the Indirectness judgment requires indirectness_rationale"
  )
})

test_that("indirectness override with rationale succeeds and notes carry it", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, indirectness = "some_concerns",
    indirectness_rationale = "Population restricted to inpatients", threshold_type = "null"
  ))
  row <- domain_row(g, "Indirectness")
  expect_equal(row$judgment, "serious")
  expect_match(row$notes,
               "Manual override \\(serious\\): Population restricted")
})

test_that("indirectness per-study vector (aggregation path) needs no rationale", {
  # Updated (v0.5): the aggregation is now weight-share based, so a single
  # indirect study out of three no longer rates the body of evidence down.
  # Studies B and C carry 78% of the weight here, which does.
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, indirectness = c("no", "some_concerns", "some_concerns"),
    threshold_type = "null"))
  row <- domain_row(g, "Indirectness")
  expect_equal(row$judgment, "serious")
  expect_no_match(row$notes, "Manual override")
})

# ---- Inconsistency --------------------------------------------------------

test_that("inconsistency scalar override requires rationale", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, inconsistency = "very_serious", threshold_type = "null")),
    regexp = "Overriding the Inconsistency judgment requires inconsistency_rationale"
  )
})

test_that("inconsistency override with rationale succeeds and notes carry it", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, inconsistency = "very_serious",
    inconsistency_rationale = "Clinically divergent effects across settings", threshold_type = "null"
  ))
  row <- domain_row(g, "Inconsistency")
  expect_equal(row$judgment, "very_serious")
  expect_false(row$auto)
  expect_match(row$notes, "Manual override \\(very_serious\\): Clinically divergent")
})

test_that("inconsistency auto path and manual flowchart need no rationale", {
  m <- make_metabin_or()
  g_auto <- suppressWarnings(grade_meta(m, threshold_type = "null"))
  expect_true(domain_row(g_auto, "Inconsistency")$auto)

  g_flow <- suppressWarnings(grade_meta(m, inconsistency_ci_diff = "no", threshold_type = "null"))
  row <- domain_row(g_flow, "Inconsistency")
  expect_equal(row$judgment, "not_serious")
  expect_no_match(row$notes, "Manual override")
})

# ---- Imprecision ----------------------------------------------------------

test_that("imprecision scalar override requires rationale", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, imprecision = "very_serious", threshold_type = "null")),
    regexp = "Overriding the Imprecision judgment requires imprecision_rationale"
  )
})

test_that("imprecision override bypasses automated assessment and notes carry rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, imprecision = "very_serious",
    imprecision_rationale = "CI includes both important benefit and harm", threshold_type = "null"
  ))
  row <- domain_row(g, "Imprecision")
  expect_equal(row$judgment, "very_serious")
  expect_equal(row$downgrade, -2)
  expect_false(row$auto)
  expect_match(row$notes, "Manual override \\(very_serious\\): CI includes both")
  expect_match(row$notes, "automated assessment not applied")
})

test_that("imprecision auto path (default) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null"))
  expect_true(domain_row(g, "Imprecision")$auto)
})

test_that("invalid imprecision scalar errors", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, imprecision = "bogus",
                                imprecision_rationale = "x", threshold_type = "null")),
    regexp = "invalid GRADE level"
  )
})

# ---- Rating target (Core GRADE 2 Fig 2) -----------------------------------

test_that("rating_target override requires rationale", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, threshold_type = "null",
                                rating_target = "non_null_effect")),
    regexp = "Overriding the rating target judgment requires rating_target_rationale"
  )
})

test_that("rating_target override with empty or whitespace rationale errors", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, threshold_type = "null",
                                rating_target = "non_null_effect",
                                rating_target_rationale = "")),
    regexp = "rating_target_rationale"
  )
  expect_error(
    suppressWarnings(grade_meta(m, threshold_type = "null",
                                rating_target = "non_null_effect",
                                rating_target_rationale = "   ")),
    regexp = "rating_target_rationale"
  )
})

test_that("rating_target override with rationale succeeds and notes carry it", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, threshold = 1.2, threshold_scale = "ratio",
    rating_target = "little_to_no_difference",
    rating_target_rationale = "Panel targets an unimportant effect"
  ))
  expect_equal(g$rating_target, "little_to_no_difference")
  expect_false(g$rating_target_auto)
  row <- domain_row(g, "Imprecision")
  expect_match(row$notes,
               "Manual override \\(little_to_no_difference\\): Panel targets")
})

test_that("auto-derived rating target needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null"))
  expect_true(g$rating_target_auto)
  expect_no_match(domain_row(g, "Imprecision")$notes, "Manual override")
})

# ---- Publication bias -----------------------------------------------------

test_that("pubias_funnel_asymmetry requires pubias_rationale", {
  m <- make_metabin_k10()
  expect_error(
    suppressWarnings(grade_meta(m, pubias_funnel_asymmetry = "yes", threshold_type = "null")),
    regexp = "Overriding the Publication bias judgment requires pubias_rationale"
  )
})

test_that("pubias visual override with rationale succeeds and notes carry it", {
  m <- make_metabin_k10()
  g <- suppressWarnings(grade_meta(
    m, pubias_funnel_asymmetry = "yes",
    pubias_rationale = "Contour-enhanced funnel plot clearly asymmetric", threshold_type = "null"
  ))
  row <- domain_row(g, "Publication bias")
  expect_equal(row$judgment, "serious")
  expect_false(row$auto)
  expect_match(row$notes,
               "Manual override \\(serious\\): Contour-enhanced")
})

test_that("pubias auto Egger path and informational inputs need no rationale", {
  m <- make_metabin_k10()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null"))
  row <- domain_row(g, "Publication bias")
  expect_no_match(row$notes, "Manual override")

  # informational (non-override) inputs stay rationale-free
  m3 <- make_metabin_or()
  g2 <- suppressWarnings(grade_meta(m3, pubias_unpublished = "yes", threshold_type = "null"))
  expect_equal(domain_row(g2, "Publication bias")$judgment, "serious")
  g3 <- suppressWarnings(grade_meta(m3, pubias_small_industry = "yes", threshold_type = "null"))
  expect_equal(domain_row(g3, "Publication bias")$judgment, "serious")
  g4 <- suppressWarnings(grade_meta(m3, pubias_registry_complete = "yes", threshold_type = "null"))
  expect_equal(domain_row(g4, "Publication bias")$judgment, "not_serious")
})

# ---- Propagation to outputs ----------------------------------------------

test_that("override rationale propagates to evidence_profile footnotes", {
  skip_if_not_installed("flextable")
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, rob = "very_serious",
    rob_rationale = "RoB2 consensus: high risk in most domains",
    outcome_name = "Rationale Outcome", threshold_type = "null"
  ))
  ft <- evidence_profile(g)
  txt <- paste(c(unlist(ft$body$dataset), unlist(ft$footer$dataset)),
               collapse = " ")
  expect_match(txt, "Manual override \\(very_serious\\): RoB2 consensus")
})

test_that("override rationale propagates to grade_report markdown", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, rob = "very_serious",
    rob_rationale = "RoB2 consensus: high risk in most domains",
    outcome_name = "Rationale Outcome", threshold_type = "null"
  ))
  out_dir <- tempfile("rationale_report_")
  paths <- grade_report(list("Rationale Outcome" = g),
                        primary     = "Rationale Outcome",
                        format      = "md",
                        output_dir  = out_dir,
                        output_file = "rationale_test")
  content <- paste(readLines(paths[[1]], warn = FALSE), collapse = "\n")
  expect_match(content, "Manual override \\(very_serious\\): RoB2 consensus")
})
