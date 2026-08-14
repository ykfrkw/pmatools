# test-grade-levels.R — the domain-judgment vocabulary itself (R/utils.R).
#
# Three properties, and the whole file exists because breaking any of them is
# silent:
#   1. every accepted spelling resolves to the downgrade it has always carried;
#   2. a bare "serious" aborts rather than being read as one of its two
#      historical meanings (0.5.0's -2, 0.5.1's -1);
#   3. no automated path can reach "extremely_serious" (-3). Core GRADE 1 calls
#      that level rare and no flowchart in the series describes a three-level
#      downgrade, so it stays a human's judgment.

library(testthat)
library(meta)

skip_if_not_installed("meta")

make_levels_metabin <- function() {
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

domain_of <- function(g, domain) {
  g$domain_assessments[g$domain_assessments$domain == domain, ]
}

# ---- 1. Every spelling carries the downgrade it always carried -------------

test_that("the canonical levels carry Core GRADE's four downgrades", {
  expect_identical(
    unname(GRADE_DOWNGRADE[c("not_serious", "serious", "very_serious",
                             "extremely_serious")]),
    c(0, -1, -2, -3)
  )
  expect_identical(names(GRADE_DOWNGRADE), GRADE_LEVELS)
})

test_that("every alias resolves to the level it has always meant", {
  # The aliases are exactly the spellings whose meaning did NOT move in the
  # rename. Each is checked against the number, not against the new name, so
  # this fails if a future edit repoints one of them.
  expect_identical(pmatools:::.grade_level_downgrade("no"), 0L)
  expect_identical(pmatools:::.grade_level_downgrade("not_serious"), 0L)
  expect_identical(pmatools:::.grade_level_downgrade("some"), -1L)
  expect_identical(pmatools:::.grade_level_downgrade("some_concerns"), -1L)
  expect_identical(pmatools:::.grade_level_downgrade("very_serious"), -2L)
  expect_identical(pmatools:::.grade_level_downgrade("extremely_serious"), -3L)

  expect_identical(pmatools:::.normalize_grade_level(
    c("no", "some", "some_concerns", "very_serious", "extremely_serious")),
    c("not_serious", "serious", "serious", "very_serious",
      "extremely_serious"))

  # An unrecognised level contributes nothing rather than aborting a render.
  expect_identical(pmatools:::.grade_level_downgrade("something_new"), 0L)
})

test_that("the display wording is Core GRADE's, for every level and alias", {
  expect_identical(
    pmatools:::.grade_level_wording(GRADE_LEVELS),
    c("not serious", "serious", "very serious", "extremely serious"))
  expect_identical(pmatools:::.grade_level_wording("no"), "not serious")
  expect_identical(pmatools:::.grade_level_wording("some_concerns"), "serious")
  expect_identical(pmatools:::.grade_level_wording("very_serious", sentence = TRUE),
                   "Very serious")
})

test_that("a stored judgment is always canonical, whichever alias went in", {
  m <- make_levels_metabin()
  for (alias in c("no", "some", "some_concerns", "very_serious")) {
    g <- suppressWarnings(grade_meta(
      m,
      small_values = "desirable", rob = alias, rob_rationale = "Recorded by the review team",
      threshold_type = "null"))
    row <- domain_of(g, "Risk of bias")
    expect_identical(row$judgment,
                     pmatools:::.normalize_grade_level(alias),
                     info = alias)
    expect_identical(as.integer(row$downgrade),
                     pmatools:::.grade_level_downgrade(alias),
                     info = alias)
  }
})

# ---- 2. A bare "serious" aborts, naming both readings ----------------------

test_that("a bare 'serious' aborts and names both of its historical meanings", {
  m <- make_levels_metabin()
  err <- tryCatch(
    suppressWarnings(grade_meta(m, rob = "serious",
                                small_values = "desirable",
                                rob_rationale = "Anything",
                                threshold_type = "null")),
    error = function(e) conditionMessage(e))
  expect_type(err, "character")

  # Both readings are stated, each with the spelling that expresses it, so a
  # caller can resolve this without opening NEWS.md.
  expect_match(err, "rate down 1 level", fixed = TRUE)
  expect_match(err, "rate down 2 levels", fixed = TRUE)
  expect_match(err, "rob = \"some_concerns\"", fixed = TRUE)
  expect_match(err, "rob = \"very_serious\"", fixed = TRUE)
  # And it says the refusal is temporary, so nobody rewrites a corpus twice.
  expect_match(err, "in a later release", fixed = TRUE)
})

test_that("every domain argument refuses a bare 'serious' by its own name", {
  m <- make_levels_metabin()
  expect_error(
    suppressWarnings(grade_meta(m, inconsistency = "serious",
                                small_values = "desirable",
                                inconsistency_rationale = "x",
                                threshold_type = "null")),
    "inconsistency = \"serious\"", fixed = TRUE)
  expect_error(
    suppressWarnings(grade_meta(m, indirectness = "serious",
                                small_values = "desirable",
                                indirectness_rationale = "x",
                                threshold_type = "null")),
    "indirectness = \"serious\"", fixed = TRUE)
  expect_error(
    suppressWarnings(grade_meta(m, imprecision = "serious",
                                small_values = "desirable",
                                imprecision_rationale = "x",
                                threshold_type = "null")),
    "imprecision = \"serious\"", fixed = TRUE)
  # Per-study input is just as ambiguous: "serious" named the high stratum in
  # 0.5.0 and the middle one now.
  expect_error(
    suppressWarnings(grade_meta(m, rob = c("no", "serious", "no"),
                                small_values = "desirable",
                                threshold_type = "null")),
    "rob = \"serious\"", fixed = TRUE)
  expect_error(rob_strata(c("low", "serious")), "serious", fixed = TRUE)
})

test_that("the unambiguous spellings still pass silently", {
  m <- make_levels_metabin()
  expect_no_error(suppressWarnings(grade_meta(
    m,
    small_values = "desirable",
      rob = "some_concerns", rob_rationale = "Recorded", threshold_type = "null")))
  expect_no_error(suppressWarnings(grade_meta(
    m,
    small_values = "desirable",
      rob = "very_serious", rob_rationale = "Recorded", threshold_type = "null")))
  expect_identical(rob_strata(c("no", "some_concerns", "very_serious")),
                   c("low", "some", "high"))
})

# ---- 3. "extremely_serious" is a human's judgment, never an assessor's -----

test_that("nothing that builds a domain row names 'extremely_serious'", {
  # The strongest form of "manual only": a function cannot emit a level whose
  # name does not occur in its source. The population swept is every function
  # in the namespace that calls make_domain_row() -- i.e. everything that can
  # produce a domain judgment at all -- rather than a fixture grid, which can
  # only cover the branches it happens to reach.
  #
  # rob_strata() and .rob_high_levels() are deliberately NOT in this set. They
  # classify a level a caller supplied; they do not produce one.
  ns  <- asNamespace("pmatools")
  src <- vapply(ls(ns, all.names = TRUE), function(nm) {
    obj <- get(nm, envir = ns)
    if (!is.function(obj)) return("")
    paste(deparse(obj), collapse = "\n")
  }, character(1))

  producers <- names(src)[grepl("make_domain_row", src, fixed = TRUE)]
  producers <- setdiff(producers, "make_domain_row")
  expect_gt(length(producers), 5L)  # the sweep found the assessors at all

  for (nm in producers) {
    expect_false(grepl("extremely_serious", src[[nm]], fixed = TRUE), info = nm)
  }

  # And the cap the docs promise is the one the constant states.
  expect_identical(GRADE_LEVEL_AUTO_MAX, "very_serious")
  expect_identical(GRADE_DOWNGRADE[[GRADE_LEVEL_AUTO_MAX]], -2)
})

test_that("an automated rating never rates one domain down more than 2", {
  m <- make_levels_metabin()
  # A grid over the manual flowchart answers, all of which take automated
  # paths. GRADE_LEVEL_AUTO_MAX is the documented cap; this asserts the code
  # honours it.
  cap <- GRADE_DOWNGRADE[[GRADE_LEVEL_AUTO_MAX]]
  grid <- expand.grid(
    ci_diff    = c("yes", "no"),
    side       = c("majority_one_side", "opposite_sides"),
    explained  = c("yes", "no"),
    unpublished = c("yes", "no"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(grid))) {
    g <- suppressWarnings(grade_meta(
      m,
      small_values = "desirable",
      rob                              = c("no", "some_concerns", "very_serious"),
      inconsistency_ci_diff            = grid$ci_diff[i],
      inconsistency_threshold_side     = grid$side[i],
      inconsistency_subgroup_explained = grid$explained[i],
      pubias_unpublished               = grid$unpublished[i],
      threshold_type                   = "null"
    ))
    auto <- g$domain_assessments[isTRUE(g$domain_assessments$auto) |
                                   g$domain_assessments$auto, ]
    expect_gte(min(auto$downgrade), cap, label = paste("row", i))
    expect_false(any(auto$judgment == "extremely_serious"))
  }
})

test_that("extremely_serious is reachable by hand, on every domain argument", {
  m <- make_levels_metabin()

  g <- suppressWarnings(grade_meta(
    m,
    small_values = "desirable", rob = "extremely_serious",
    rob_rationale = "Every trial unblinded, with outcome adjudication by the sponsor",
    threshold_type = "null"))
  row <- domain_of(g, "Risk of bias")
  expect_identical(row$judgment, "extremely_serious")
  expect_equal(row$downgrade, -3)
  expect_match(row$notes, "Manual override (extremely_serious)", fixed = TRUE)

  # ...and the rationale gate is not weakened for it.
  expect_error(
    suppressWarnings(grade_meta(m, rob = "extremely_serious",
                                small_values = "desirable",
                                threshold_type = "null")),
    "rob_rationale", fixed = TRUE)
  expect_error(
    suppressWarnings(grade_meta(m, indirectness = "extremely_serious",
                                small_values = "desirable",
                                threshold_type = "null")),
    "indirectness_rationale", fixed = TRUE)
  expect_error(
    suppressWarnings(grade_meta(m, imprecision = "extremely_serious",
                                small_values = "desirable",
                                threshold_type = "null")),
    "imprecision_rationale", fixed = TRUE)
})

test_that("the Evidence Profile words the manual -3 as Core GRADE does", {
  m <- make_levels_metabin()
  g <- suppressWarnings(grade_meta(
    m,
    small_values = "desirable", indirectness = "extremely_serious",
    indirectness_rationale = "Surrogate outcome, unrelated population, and a dose nobody prescribes",
    threshold_type = "null"))
  ep  <- evidence_profile(g)
  txt <- paste(unlist(lapply(ep$body$dataset, as.character)), collapse = " ")
  expect_match(txt, "extremely serious", fixed = TRUE)
})

# ---- 4. The certainty floor ------------------------------------------------

test_that("certainty stops at Very Low however far past it the sum goes", {
  m <- make_levels_metabin()

  # -3 from one domain already reaches the floor from a High start.
  g1 <- suppressWarnings(grade_meta(
    m,
    small_values = "desirable", rob = "extremely_serious", rob_rationale = "Recorded",
    threshold_type = "null"))
  expect_equal(domain_of(g1, "Risk of bias")$downgrade, -3)
  expect_lte(sum(g1$domain_assessments$downgrade), -3)
  expect_identical(g1$certainty, "Very Low")
  expect_equal(g1$certainty_score, 1)

  # -3 plus another -2 is below it, and stays reported as Very Low: there is
  # no rating under Very Low on the GRADE scale, so the score clamps rather
  # than running negative into CERTAINTY_LABELS.
  g2 <- suppressWarnings(grade_meta(
    m,
    small_values = "desirable", rob = "extremely_serious", rob_rationale = "Recorded",
    indirectness = "very_serious", indirectness_rationale = "Recorded",
    threshold_type = "null"))
  expect_lte(sum(g2$domain_assessments$downgrade), -5)
  expect_identical(g2$certainty, "Very Low")
  expect_equal(g2$certainty_score, 1)

  # The clamp is score_to_certainty()'s own, so it holds for a caller that
  # arrives at the score by any other route.
  expect_identical(score_to_certainty(-4), "Very Low")
  expect_identical(score_to_certainty(0), "Very Low")
  expect_identical(score_to_certainty(1), "Very Low")
  expect_identical(score_to_certainty(5), "High")

  # An observational review starts at Low (2), so a single -3 is already past
  # the floor there.
  g3 <- suppressWarnings(grade_meta(
    m,
    small_values = "desirable", study_design = "obs", rob = "extremely_serious",
    rob_rationale = "Recorded", threshold_type = "null"))
  expect_identical(g3$certainty, "Very Low")
  expect_equal(g3$certainty_score, 1)
})
