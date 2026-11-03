# test-rule_set.R - grade_meta(rule_set =), the pmatools 0.5.x rule set
#
# "core_grade_guide" (default) must rate exactly as 0.7.x did. The BMJ 2025
# set ("core_grade_bmj_2025") changes three things and nothing else: the
# risk-of-bias direction check and the "some concerns" fold default to their
# 0.5.x values (an explicit argument still wins), and publication bias skips
# Guide Fig 9-1's Q0. These tests pin each of them, the contradiction abort,
# and the bundled analysis.R that has to replay a legacy rating.

library(testthat)

skip_if_not_installed("meta")

LEGACY <- "core_grade_bmj_2025"

# k = 10 with Egger p ~ 0.0003: the Q3 branch rates down whenever it is reached.
make_asymmetric_k10 <- function() {
  ev_e <- c(20, 18, 15, 12, 10,  8,  5,  3,  2,  1)
  ev_c <- c(20, 19, 17, 15, 13, 11,  8,  6,  4,  2)
  n_e  <- rep(500, 10) - c(0, 0, 100, 200, 300, 350, 400, 430, 450, 470)
  meta::metabin(ev_e, n_e, ev_c, n_e, sm = "OR",
                studlab = paste0("S", seq_along(ev_e)),
                random = TRUE, method = "Inverse")
}

# Ten studies with a three-level RoB vector, so the fold matters.
ROB_VECTOR <- c("low", "low", "some_concerns", "some_concerns", "high",
                "low", "low", "some_concerns", "low", "low")

pubias_row <- function(g) {
  g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
}
pubias_path <- function(g) {
  f <- domain_facts(g, "Publication bias")
  strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
}

# The estimate sits inside an OR threshold of 3, so Q0 answers no under the
# Guide; Egger is significant, so Q3 rates down when Q0 is skipped.
grade_inside_threshold <- function(...) {
  suppressWarnings(grade_meta(make_asymmetric_k10(), small_values = "desirable",
                              pubias_small_industry = "no",
                              threshold = 3, threshold_scale = "ratio", ...))
}

test_that("the default rule set is the Guide and rates as before", {
  m <- make_asymmetric_k10()
  base <- suppressWarnings(grade_meta(m, small_values = "desirable",
                                      rob = ROB_VECTOR,
                                      threshold = 3, threshold_scale = "ratio"))
  named <- suppressWarnings(grade_meta(m, small_values = "desirable",
                                       rob = ROB_VECTOR,
                                       threshold = 3, threshold_scale = "ratio",
                                       rule_set = "core_grade_guide"))
  expect_identical(base$rule_set, "core_grade_guide")
  expect_false(isTRUE(base$rob_direction_check))
  expect_identical(base$rob_some_concerns, "low")
  expect_identical(base$domain_assessments, named$domain_assessments)
  expect_identical(base$certainty, named$certainty)
  # A default call prints no rule-set line.
  expect_false(any(grepl("Rule set", capture.output(print(base)), fixed = TRUE)))
})

test_that("the legacy set turns on the direction check and folds some concerns high", {
  g <- suppressWarnings(grade_meta(make_asymmetric_k10(),
                                   small_values = "desirable", rob = ROB_VECTOR,
                                   threshold = 1.01, threshold_scale = "ratio",
                                   rule_set = LEGACY))
  expect_identical(g$rule_set, LEGACY)
  expect_true(isTRUE(g$rob_direction_check))
  expect_identical(g$rob_some_concerns, "high")
  rob <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob$notes, "Beyond Core GRADE", fixed = TRUE)
  expect_true(any(grepl("BMJ 2025 Core GRADE series",
                        capture.output(print(g)), fixed = TRUE)))
})

test_that("explicitly passed risk-of-bias arguments win over the legacy set", {
  g <- suppressWarnings(grade_meta(make_asymmetric_k10(),
                                   small_values = "desirable", rob = ROB_VECTOR,
                                   threshold = 1.01, threshold_scale = "ratio",
                                   rule_set = LEGACY,
                                   rob_direction_check = FALSE,
                                   rob_some_concerns = "low"))
  expect_false(isTRUE(g$rob_direction_check))
  expect_identical(g$rob_some_concerns, "low")
})

test_that("the legacy set skips Q0: rated down where the Guide stops at Q0", {
  guide  <- grade_inside_threshold()
  legacy <- grade_inside_threshold(rule_set = LEGACY)

  expect_identical(pubias_row(guide)$judgment, "not_serious")
  expect_identical(pubias_row(legacy)$judgment, "serious")

  notes <- pubias_row(legacy)$notes
  expect_match(notes, "Q0 not asked (pmatools 0.5.x rule set", fixed = TRUE)
  expect_match(notes, "Q3 (auto): Egger's test p < 0.05", fixed = TRUE)
  expect_false(grepl("Q0 (auto)", notes, fixed = TRUE))

  f <- domain_facts(legacy, "Publication bias")
  value <- f$value[f$key == "important_effect"]
  expect_match(value, "^not asked")
  expect_true(is.na(f$numeric[f$key == "important_effect"]))

  ids <- pubias_path(legacy)
  expect_false(any(grepl("q0", ids, fixed = TRUE)))
  expect_identical(ids[1], "pma-pubias-node-q1")
  expect_true(all(c("pma-pubias-node-q3", "pma-pubias-leaf-down1-q3") %in% ids))
})

test_that("Q0 skip lights no Q0 id on the early-exit branches either", {
  m <- make_asymmetric_k10()
  q1_yes <- suppressWarnings(grade_meta(m, small_values = "desirable",
                                        pubias_small_industry = "yes",
                                        threshold_type = "null",
                                        rule_set = LEGACY))
  expect_identical(pubias_path(q1_yes),
                   c("pma-pubias-node-q1", "pma-pubias-edge-q1-yes",
                     "pma-pubias-leaf-down1-q1"))
  registry <- suppressWarnings(grade_meta(m, small_values = "desirable",
                                          pubias_registry_complete = "yes",
                                          threshold_type = "null",
                                          rule_set = LEGACY))
  expect_false(any(grepl("q0", pubias_path(registry), fixed = TRUE)))
  expect_identical(pubias_row(registry)$judgment, "not_serious")
})

test_that("pubias_important_effect with the legacy set aborts", {
  expect_error(
    grade_inside_threshold(rule_set = LEGACY, pubias_important_effect = "yes"),
    "pubias_important_effect cannot be combined with rule_set")
})

test_that("an unknown rule set aborts with both names", {
  expect_error(grade_inside_threshold(rule_set = "core_grade_2011"),
               "core_grade_bmj_2025")
})

test_that("assess_pubias(pubias_ask_q0 = FALSE) works standalone", {
  m <- make_asymmetric_k10()
  row <- suppressWarnings(pmatools:::assess_pubias(
    m, pubias_small_industry = "no", threshold_chosen = log(3),
    pubias_ask_q0 = FALSE))
  expect_identical(row$judgment, "serious")
  expect_error(pmatools:::assess_pubias(m, pubias_ask_q0 = FALSE,
                                        pubias_important_effect = "yes"),
               "pubias_ask_q0 = FALSE")
  expect_error(pmatools:::assess_pubias(m, pubias_ask_q0 = NA),
               "pubias_ask_q0 must be TRUE or FALSE")
})

test_that("the bundled analysis.R writes the legacy set and reproduces the rating", {
  skip_if_not_installed("zip")
  m <- make_asymmetric_k10()
  g <- grade_inside_threshold(rule_set = LEGACY, outcome_name = "Legacy")
  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- export_bundle(m, g, output_dir = out_dir,
                            bundle_name = "legacy_bundle",
                            include = c("script"))
  unz_dir <- tempfile(); dir.create(unz_dir)
  zip::unzip(zip_path, exdir = unz_dir)
  script_path <- file.path(unz_dir, "analysis.R")
  txt <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
  expect_match(txt, "rule_set                = \"core_grade_bmj_2025\",",
               fixed = TRUE)

  exprs <- parse(file = script_path)
  is_grade <- vapply(exprs, function(e) {
    is.call(e) && identical(as.character(e[[1]]), "<-") &&
      is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "grade_meta")
  }, logical(1))
  env <- new.env(parent = environment(grade_meta))
  assign("ma", m, envir = env)
  again <- suppressWarnings(eval(exprs[is_grade][[1]][[3]], envir = env))
  expect_identical(again$rule_set, LEGACY)
  expect_identical(again$domain_assessments$judgment,
                   g$domain_assessments$judgment)
  expect_identical(again$certainty, g$certainty)
})

test_that("a default bundle's analysis.R carries no rule_set line", {
  skip_if_not_installed("zip")
  m <- make_asymmetric_k10()
  g <- grade_inside_threshold(outcome_name = "Default")
  out_dir <- tempfile(); dir.create(out_dir)
  zip_path <- export_bundle(m, g, output_dir = out_dir,
                            bundle_name = "default_bundle",
                            include = c("script"))
  unz_dir <- tempfile(); dir.create(unz_dir)
  zip::unzip(zip_path, exdir = unz_dir)
  txt <- paste(readLines(file.path(unz_dir, "analysis.R"), warn = FALSE),
               collapse = "\n")
  expect_false(grepl("rule_set", txt, fixed = TRUE))
})
