# test-indirectness_dominance.R — v0.5.1
#
# Per-study indirectness vectors / column names are aggregated by WEIGHT SHARE,
# not worst case. Core GRADE 5 (p2-3), verbatim:
#
#   "However, if Core GRADE users are interested in effects in elderly people
#    but all or almost all evidence comes from younger people, in low dose but
#    all or almost all evidence comes from high dose, or in long follow-up but
#    all or almost all evidence comes from short follow-up, they lack the data
#    to test whether effects differ across these variables."
#
# The numeric threshold has no basis in the source; 0.55 is a pmatools
# convention aligned with rob_dominant_threshold, and the domain note says so.

library(testthat)
library(meta)

skip_if_not_installed("meta")

# metagen with tau.preset = 0 so the IV weights are exactly 1/seTE^2.
mk_w <- function(w, te = rep(0.10, length(w))) {
  metagen(TE = te, seTE = sqrt(1 / w), studlab = paste0("S", seq_along(w)),
          sm = "RR", tau.preset = 0)
}

ind_row <- function(g) {
  g$domain_assessments[g$domain_assessments$domain == "Indirectness", ]
}

# 20 studies: study 1 carries 5% of the weight, the rest share the other 95%.
mk_20 <- function() mk_w(c(5, rep(95 / 19, 19)))

test_that("1 indirect study of 20 carrying 5% of the weight does not rate down", {
  # Pre-v0.5.1 this returned "serious" via the worst-case fold.
  g <- suppressWarnings(grade_meta(
    mk_20(),
    indirectness   = c("serious", rep("no", 19)),
    threshold_type = "null"
  ))
  row <- ind_row(g)
  expect_equal(row$judgment, "no")
  expect_equal(row$downgrade, 0)
  expect_match(row$notes, "carry only 5% of the weight", fixed = TRUE)
  expect_match(row$notes, "pmatools convention", fixed = TRUE)
})

test_that("indirect studies carrying >= 55% of the weight do rate down", {
  # Study 1 carries 60%; the remaining 19 share 40%.
  m <- mk_w(c(60, rep(40 / 19, 19)))
  g <- suppressWarnings(grade_meta(
    m,
    indirectness   = c("some_concerns", rep("no", 19)),
    threshold_type = "null"
  ))
  row <- ind_row(g)
  expect_equal(row$judgment, "some_concerns")
  expect_equal(row$downgrade, -1L)
  expect_match(row$notes, "60% of the weight", fixed = TRUE)
})

test_that("'serious' studies dominating the weight give -2", {
  m <- mk_w(c(60, rep(40 / 19, 19)))
  g <- suppressWarnings(grade_meta(
    m,
    indirectness   = c("serious", rep("no", 19)),
    threshold_type = "null"
  ))
  row <- ind_row(g)
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -2L)
})

test_that("the boundary is inclusive and follows indirectness_dominant_threshold", {
  # 11 / 20 = 0.55 exactly (chosen so sqrt(1/w) round-trips cleanly).
  m <- mk_w(c(11, 4.5, 4.5))
  at_default <- suppressWarnings(grade_meta(
    m, indirectness = c("some_concerns", "no", "no"), threshold_type = "null"))
  expect_equal(ind_row(at_default)$judgment, "some_concerns")

  raised <- suppressWarnings(grade_meta(
    m, indirectness = c("some_concerns", "no", "no"),
    indirectness_dominant_threshold = 0.60, threshold_type = "null"))
  expect_equal(ind_row(raised)$judgment, "no")

  lowered <- suppressWarnings(grade_meta(
    mk_20(), indirectness = c("serious", rep("no", 19)),
    indirectness_dominant_threshold = 0.05, threshold_type = "null"))
  expect_equal(ind_row(lowered)$judgment, "serious")
})

test_that("indirectness_dominant_threshold is validated", {
  m <- mk_w(c(11, 4.5, 4.5))
  expect_error(
    suppressWarnings(grade_meta(m, indirectness = c("no", "no", "no"),
                                indirectness_dominant_threshold = 0,
                                threshold_type = "null")),
    regexp = "indirectness_dominant_threshold"
  )
  expect_error(
    suppressWarnings(grade_meta(m, indirectness = c("no", "no", "no"),
                                indirectness_dominant_threshold = 1.5,
                                threshold_type = "null")),
    regexp = "indirectness_dominant_threshold"
  )
})

test_that("column-name input takes the same weight-share route", {
  m <- mk_20()
  m$data <- data.frame(ind = c("serious", rep("no", 19)),
                       stringsAsFactors = FALSE)
  g <- suppressWarnings(grade_meta(m, indirectness = "ind",
                                   threshold_type = "null"))
  row <- ind_row(g)
  expect_equal(row$judgment, "no")
  expect_match(row$notes, "by weight share", fixed = TRUE)
})

test_that("the count share is used, and flagged, when weights are unavailable", {
  m <- mk_20()
  m$w.random <- NULL
  m$w.common <- NULL
  m$w.fixed  <- NULL
  m$seTE     <- NULL
  row <- pmatools:::assess_indirectness(
    c("serious", rep("no", 19)), m
  )
  expect_equal(row$judgment, "no")   # 1/20 = 5% by count
  expect_match(row$notes, "COUNT shares", fixed = TRUE)
})

test_that("the subdomain table keeps its worst-case fold", {
  # Subdomains are facets of one judgment, not units of evidence, so the
  # weight-share rule deliberately does not apply to them.
  g <- suppressWarnings(grade_meta(
    mk_20(),
    indirectness_subdomains = data.frame(
      subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
      judgment  = c("yes", "yes", "yes", "probably_no"),
      stringsAsFactors = FALSE
    ),
    threshold_type = "null"
  ))
  row <- ind_row(g)
  expect_equal(row$judgment, "some_concerns")
  expect_match(row$notes, "Overall (worst case)", fixed = TRUE)
})
