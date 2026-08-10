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
    grade_meta(m, rob = "serious",
               rob_rationale = "RoB2 consensus: high risk in most domains")
  )
  row <- domain_row(g, "Risk of bias")
  expect_equal(row$judgment, "serious")
  expect_false(row$auto)
  expect_match(row$notes, "Manual override \\(serious\\): RoB2 consensus")
})

test_that("rob scalar override without rationale errors", {
  m <- make_metabin_or()
  expect_error(
    grade_meta(m, rob = "serious"),
    regexp = "Overriding the Risk of Bias judgment requires rob_rationale"
  )
})

test_that("rob scalar override with empty or whitespace rationale errors", {
  m <- make_metabin_or()
  expect_error(grade_meta(m, rob = "no", rob_rationale = ""),
               regexp = "rob_rationale")
  expect_error(grade_meta(m, rob = "no", rob_rationale = "   "),
               regexp = "rob_rationale")
})

test_that("rob per-study vector (automated path) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, rob = c("no", "some", "serious")))
  row <- domain_row(g, "Risk of bias")
  expect_match(row$notes, "by count")
  expect_no_match(row$notes, "Manual override")
})

test_that("rob = NULL (default) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m))
  row <- domain_row(g, "Risk of bias")
  expect_equal(row$judgment, "no")
})

# ---- Indirectness ---------------------------------------------------------

test_that("indirectness = 'no' (default value) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, indirectness = "no"))
  row <- domain_row(g, "Indirectness")
  expect_equal(row$judgment, "no")
  expect_no_match(row$notes, "Manual override")
})

test_that("indirectness scalar other than 'no' requires rationale", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, indirectness = "serious")),
    regexp = "Overriding the Indirectness judgment requires indirectness_rationale"
  )
})

test_that("indirectness override with rationale succeeds and notes carry it", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, indirectness = "some_concerns",
    indirectness_rationale = "Population restricted to inpatients"
  ))
  row <- domain_row(g, "Indirectness")
  expect_equal(row$judgment, "some_concerns")
  expect_match(row$notes,
               "Manual override \\(some_concerns\\): Population restricted")
})

test_that("indirectness per-study vector (aggregation path) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m, indirectness = c("no", "some_concerns", "no")))
  row <- domain_row(g, "Indirectness")
  expect_equal(row$judgment, "some_concerns")
  expect_no_match(row$notes, "Manual override")
})

# ---- Inconsistency --------------------------------------------------------

test_that("inconsistency scalar override requires rationale", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, inconsistency = "serious")),
    regexp = "Overriding the Inconsistency judgment requires inconsistency_rationale"
  )
})

test_that("inconsistency override with rationale succeeds and notes carry it", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, inconsistency = "serious",
    inconsistency_rationale = "Clinically divergent effects across settings"
  ))
  row <- domain_row(g, "Inconsistency")
  expect_equal(row$judgment, "serious")
  expect_false(row$auto)
  expect_match(row$notes, "Manual override \\(serious\\): Clinically divergent")
})

test_that("inconsistency auto path and manual flowchart need no rationale", {
  m <- make_metabin_or()
  g_auto <- suppressWarnings(grade_meta(m))
  expect_true(domain_row(g_auto, "Inconsistency")$auto)

  g_flow <- suppressWarnings(grade_meta(m, inconsistency_ci_diff = "no"))
  row <- domain_row(g_flow, "Inconsistency")
  expect_equal(row$judgment, "no")
  expect_no_match(row$notes, "Manual override")
})

# ---- Imprecision ----------------------------------------------------------

test_that("imprecision scalar override requires rationale", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, imprecision = "serious")),
    regexp = "Overriding the Imprecision judgment requires imprecision_rationale"
  )
})

test_that("imprecision override bypasses automated assessment and notes carry rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, imprecision = "serious",
    imprecision_rationale = "CI includes both important benefit and harm"
  ))
  row <- domain_row(g, "Imprecision")
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -2)
  expect_false(row$auto)
  expect_match(row$notes, "Manual override \\(serious\\): CI includes both")
  expect_match(row$notes, "automated assessment not applied")
})

test_that("imprecision auto path (default) needs no rationale", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(m))
  expect_true(domain_row(g, "Imprecision")$auto)
})

test_that("invalid imprecision scalar errors", {
  m <- make_metabin_or()
  expect_error(
    suppressWarnings(grade_meta(m, imprecision = "bogus",
                                imprecision_rationale = "x")),
    regexp = "invalid GRADE level"
  )
})

# ---- Publication bias -----------------------------------------------------

test_that("pubias_funnel_asymmetry requires pubias_rationale", {
  m <- make_metabin_k10()
  expect_error(
    suppressWarnings(grade_meta(m, pubias_funnel_asymmetry = "yes")),
    regexp = "Overriding the Publication bias judgment requires pubias_rationale"
  )
})

test_that("pubias visual override with rationale succeeds and notes carry it", {
  m <- make_metabin_k10()
  g <- suppressWarnings(grade_meta(
    m, pubias_funnel_asymmetry = "yes",
    pubias_rationale = "Contour-enhanced funnel plot clearly asymmetric"
  ))
  row <- domain_row(g, "Publication bias")
  expect_equal(row$judgment, "some_concerns")
  expect_false(row$auto)
  expect_match(row$notes,
               "Manual override \\(some_concerns\\): Contour-enhanced")
})

test_that("pubias auto Egger path and informational inputs need no rationale", {
  m <- make_metabin_k10()
  g <- suppressWarnings(grade_meta(m))
  row <- domain_row(g, "Publication bias")
  expect_no_match(row$notes, "Manual override")

  # informational (non-override) inputs stay rationale-free
  m3 <- make_metabin_or()
  g2 <- suppressWarnings(grade_meta(m3, pubias_unpublished = "yes"))
  expect_equal(domain_row(g2, "Publication bias")$judgment, "some_concerns")
  g3 <- suppressWarnings(grade_meta(m3, pubias_small_industry = "yes"))
  expect_equal(domain_row(g3, "Publication bias")$judgment, "some_concerns")
  g4 <- suppressWarnings(grade_meta(m3, pubias_registry_complete = "yes"))
  expect_equal(domain_row(g4, "Publication bias")$judgment, "no")
})

# ---- Propagation to outputs ----------------------------------------------

test_that("override rationale propagates to evidence_profile footnotes", {
  skip_if_not_installed("flextable")
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, rob = "serious",
    rob_rationale = "RoB2 consensus: high risk in most domains",
    outcome_name = "Rationale Outcome"
  ))
  ft <- evidence_profile(g)
  txt <- paste(c(unlist(ft$body$dataset), unlist(ft$footer$dataset)),
               collapse = " ")
  expect_match(txt, "Manual override \\(serious\\): RoB2 consensus")
})

test_that("override rationale propagates to grade_report markdown", {
  m <- make_metabin_or()
  g <- suppressWarnings(grade_meta(
    m, rob = "serious",
    rob_rationale = "RoB2 consensus: high risk in most domains",
    outcome_name = "Rationale Outcome"
  ))
  out_dir <- tempfile("rationale_report_")
  paths <- grade_report(list("Rationale Outcome" = g),
                        primary     = "Rationale Outcome",
                        format      = "md",
                        output_dir  = out_dir,
                        output_file = "rationale_test")
  content <- paste(readLines(paths[[1]], warn = FALSE), collapse = "\n")
  expect_match(content, "Manual override \\(serious\\): RoB2 consensus")
})
