# test-rob_flowchart.R — v0.5 (Phase B):
# BMJ Core GRADE 4 Fig 2 risk-of-bias flowchart — binary low/high
# classification, the reinstated weight-share dominance gate, and the
# automatic refit on the "use low risk of bias studies only" leaf.
#
# All fixtures use metagen() with tau.preset = 0 so the inverse-variance
# weights are exactly 1/seTE^2 and the weight share is reproducible to the
# last bit (the dominance gate compares with `>=`).

library(testthat)
library(meta)

skip_if_not_installed("meta")

# Build a metagen whose study weights are exactly `w`.
mk <- function(te, w, studlab = paste0("S", seq_along(te)), sm = "RR") {
  metagen(TE = te, seTE = sqrt(1 / w), studlab = studlab, sm = sm,
          tau.preset = 0)
}

rob_row <- function(g) {
  g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
}
row_of <- function(g, domain) {
  g$domain_assessments[g$domain_assessments$domain == domain, ]
}

# Not dominated (50% < 60%) and the low-RoB estimate is in a different zone,
# so Fig 2 lands on "use low risk of bias studies only".
make_low_only <- function() {
  mk(te = c(1.2, 0.02, 0.02, 0.02),
     w  = c(400, 400 / 3, 400 / 3, 400 / 3),
     studlab = c("High-1", "Low-1", "Low-2", "Low-3"))
}

quiet_grade <- function(...) suppressWarnings(grade_meta(...))

# ---- B-1: binary classification is configurable ---------------------------

test_that("rob_some_concerns folds 'some concerns' into the chosen side", {
  # Weights 30 / 35 / 35: with the default fold the high group is study 1
  # alone (30%); folding 'some concerns' high adds study 2 (65%), which flips
  # the dominance gate.
  m <- mk(te = c(0.8, 0.8, 0.02), w = c(30, 35, 35))
  rob <- c("very_serious", "some_concerns", "no")

  g_low <- quiet_grade(m, rob = rob, small_values = "undesirable",
                       threshold = 1.05, threshold_scale = "ratio")
  g_high <- quiet_grade(m, rob = rob, rob_some_concerns = "high",
                        small_values = "undesirable",
                        threshold = 1.05, threshold_scale = "ratio")

  expect_match(rob_row(g_low)$notes,  "30% by weight", fixed = TRUE)
  expect_match(rob_row(g_low)$notes,  "dominated: no", fixed = TRUE)
  expect_match(rob_row(g_low)$notes,
               "'some concerns' folded into the low risk group", fixed = TRUE)

  expect_match(rob_row(g_high)$notes, "65% by weight", fixed = TRUE)
  expect_match(rob_row(g_high)$notes, "dominated: yes", fixed = TRUE)
  expect_match(rob_row(g_high)$notes,
               "'some concerns' folded into the high risk group", fixed = TRUE)
})

test_that("rob_some_concerns rejects anything but 'low' / 'high'", {
  m <- mk(te = c(0.8, 0.02, 0.02), w = c(30, 35, 35))
  expect_error(
    grade_meta(m, rob = c("very_serious", "some_concerns", "no"),
               small_values = "desirable",
               rob_some_concerns = "medium", threshold_type = "null"),
    regexp = "rob_some_concerns"
  )
})

# ---- B-2: dominance gate, inclusive boundary ------------------------------

test_that("dominance gate switches at 55% with an inclusive boundary", {
  # Updated (v0.5): the default is 0.55, the ">=55% weight
  # = possibly dominating" candidate in the Core GRADE 4 Fig 2 footnote.
  # 54% -> not dominated; exactly 55% -> dominated; 56% -> dominated.
  # The 55% fixture uses c(11, 4.5, 4.5) rather than c(55, 22.5, 22.5): the
  # latter round-trips through sqrt(1/w) to 0.5499999999999999, one ulp below
  # the threshold, which is exactly the boundary this test is about.
  g54 <- quiet_grade(mk(c(0.8, 0.02, 0.02), c(54, 23, 23)),
                     rob = c("very_serious", "no", "no"),
                     small_values = "undesirable",
                     threshold = 1.05, threshold_scale = "ratio",
                     rob_refit = FALSE)
  g55 <- quiet_grade(mk(c(0.8, 0.02, 0.02), c(11, 4.5, 4.5)),
                     rob = c("very_serious", "no", "no"),
                     small_values = "undesirable",
                     threshold = 1.05, threshold_scale = "ratio")
  g56 <- quiet_grade(mk(c(0.8, 0.02, 0.02), c(56, 22, 22)),
                     rob = c("very_serious", "no", "no"),
                     small_values = "undesirable",
                     threshold = 1.05, threshold_scale = "ratio")

  expect_match(rob_row(g54)$notes, "dominated: no",  fixed = TRUE)
  expect_match(rob_row(g55)$notes, "dominated: yes", fixed = TRUE)
  expect_match(rob_row(g56)$notes, "dominated: yes", fixed = TRUE)

  # The 55% fixture sits exactly on the threshold: nudging the threshold above
  # it flips the gate, which is what makes the `>=` boundary observable.
  g55_strict <- quiet_grade(mk(c(0.8, 0.02, 0.02), c(11, 4.5, 4.5)),
                            rob = c("very_serious", "no", "no"),
                            small_values = "undesirable",
                            threshold = 1.05, threshold_scale = "ratio",
                            rob_dominant_threshold = 0.5500001,
                            rob_refit = FALSE)
  expect_match(rob_row(g55_strict)$notes, "dominated: no", fixed = TRUE)

  # The other candidate the footnote names (>65%) is still selectable.
  g60_at_65 <- quiet_grade(mk(c(0.8, 0.02, 0.02), c(3, 1, 1)),
                           rob = c("very_serious", "no", "no"),
                           small_values = "undesirable",
                           threshold = 1.05, threshold_scale = "ratio",
                           rob_dominant_threshold = 0.65,
                           rob_refit = FALSE)
  expect_match(rob_row(g60_at_65)$notes, "dominated: no", fixed = TRUE)
})

test_that("rob_dominant_threshold is validated", {
  m <- mk(c(0.8, 0.02, 0.02), c(3, 1, 1))
  expect_error(
    grade_meta(m, rob = c("very_serious", "no", "no"),
               small_values = "desirable",
               rob_dominant_threshold = 0, threshold_type = "null"),
    regexp = "rob_dominant_threshold"
  )
  expect_error(
    grade_meta(m, rob = c("very_serious", "no", "no"),
               small_values = "desirable",
               rob_dominant_threshold = 1.5, threshold_type = "null"),
    regexp = "rob_dominant_threshold"
  )
})

test_that("dominated = yes uses the direction-of-bias check verdict", {
  # 75% weight on the high-RoB study; same non-trivial zone but a large
  # bias-favouring inflation -> rule 3 -> rate down 1.
  m <- mk(te = c(0.60, 0.40, 0.40), w = c(75, 12.5, 12.5))
  g <- quiet_grade(m, rob = c("very_serious", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.20, threshold_scale = "ratio")
  row <- rob_row(g)
  expect_match(row$notes, "dominated: yes", fixed = TRUE)
  expect_match(row$notes, "Rule 3")
  expect_equal(row$judgment, "serious")
  expect_equal(row$downgrade, -1)
  expect_equal(g$rob_analysis_set, "all")
  expect_false(g$rob_refit)
})

# ---- B-2b: the undominated branch reads the five-rule verdict --------------
#
# "Similar or substantially different magnitudes of effect?" is answered by the
# same check that decides the dominated branch: a rule that rates down means
# substantially different, a rule that does not means similar. The fixtures
# below walk that mapping rule by rule and assert the route through the figure
# as well as the leaf, because the analysis set is what the leaf changes and a
# picture that disagreed with it would be the harder bug to see.

.rob_path <- function(g) {
  f <- domain_facts(g, "Risk of bias")
  strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
}

test_that("not dominated + substantial difference: no downgrade, low_only", {
  # make_low_only(): TE_all = 0.61 (above), TE_low = 0.02 (trivial) -- a zone
  # change on the same side of the null, so rule 4.
  g <- quiet_grade(make_low_only(), rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  row <- rob_row(g)
  expect_equal(row$judgment, "not_serious")
  expect_equal(row$downgrade, 0)
  expect_equal(g$rob_analysis_set, "low_only")
  expect_true(g$rob_refit)
  expect_lt(g$meta$k, g$meta_full$k)
  expect_equal(g$meta$k, 3L)
  expect_equal(g$meta_full$k, 4L)
  expect_match(row$notes, "use low risk of bias studies only", fixed = TRUE)
  expect_match(row$notes, "reaches rule 4", fixed = TRUE)
  expect_true(all(c("pma-rob-node-magnitude",
                    "pma-rob-edge-magnitude-different",
                    "pma-rob-leaf-lowonly") %in% .rob_path(g)))
  expect_false("pma-rob-edge-magnitude-similar" %in% .rob_path(g))
})

test_that("not dominated + no substantial difference: no downgrade, all", {
  # Same weights, but the high-RoB study agrees with the low-RoB studies, so
  # both estimates stay in the trivial zone (rule 1). Rule 1's exemption is the
  # reason this is "similar" despite a 25% relative change between two
  # near-null numbers.
  m <- mk(te = c(0.03, 0.02, 0.02, 0.02),
          w  = c(400, 400 / 3, 400 / 3, 400 / 3))
  g <- quiet_grade(m, rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  row <- rob_row(g)
  expect_equal(row$judgment, "not_serious")
  expect_equal(row$downgrade, 0)
  expect_equal(g$rob_analysis_set, "all")
  expect_false(g$rob_refit)
  expect_equal(g$meta$k, g$meta_full$k)
  expect_match(row$notes, "Similar magnitudes of effect", fixed = TRUE)
  expect_match(row$notes, "reaches rule 1", fixed = TRUE)
  expect_true(all(c("pma-rob-node-magnitude",
                    "pma-rob-edge-magnitude-similar",
                    "pma-rob-leaf-all") %in% .rob_path(g)))
})

test_that("not dominated: a bias-favouring shift past the threshold restricts", {
  # Same non-trivial zone (both above +log(1.05)), TE_all = 0.65 against
  # TE_low = 0.50, a 30% relative change in the bias-favouring direction under
  # small_values = "undesirable". Rule 3 -> substantially different.
  m <- mk(te = c(0.80, 0.50, 0.50, 0.50),
          w  = c(400, 400 / 3, 400 / 3, 400 / 3))
  g <- quiet_grade(m, rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  row <- rob_row(g)
  expect_equal(row$judgment, "not_serious")        # this branch never rates down
  expect_equal(g$rob_analysis_set, "low_only")
  expect_true(g$rob_refit)
  expect_equal(g$meta$k, 3L)
  expect_match(row$notes, "Substantially different magnitudes of effect",
               fixed = TRUE)
  expect_match(row$notes, "reaches rule 3", fixed = TRUE)
  expect_match(row$notes, "direction gate (bias-favouring shift): yes",
               fixed = TRUE)
  expect_true("pma-rob-edge-magnitude-different" %in% .rob_path(g))
})

test_that("not dominated: the mirror-image shift is similar, and says why", {
  # Mirror image of the fixture above: the LOW-RoB studies show the larger
  # effect (TE_low = 0.50 against TE_all = 0.35, a 30% relative change), so the
  # shift is not bias-favouring under small_values = "undesirable" and the
  # five-rule check reaches rule 2.
  #
  # Up to and including 0.5.0 this branch judged magnitude alone and called the
  # same pair of estimates substantially different, restricting the analysis to
  # the three low-RoB studies. That is deliberately gone: the assertions below
  # pin the new answer AND the absence of the old one, because a silently
  # dropped expectation here would change which studies an analysis is run on.
  m <- mk(te = c(0.20, 0.50, 0.50, 0.50),
          w  = c(400, 400 / 3, 400 / 3, 400 / 3))
  g <- quiet_grade(m, rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  row <- rob_row(g)
  expect_equal(row$judgment, "not_serious")
  expect_equal(g$rob_analysis_set, "all")
  expect_false(g$rob_refit)
  expect_equal(g$meta$k, g$meta_full$k)
  expect_equal(g$meta$k, 4L)

  expect_match(row$notes, "Similar magnitudes of effect", fixed = TRUE)
  expect_match(row$notes, "reaches rule 2", fixed = TRUE)
  expect_false(grepl("Substantially different magnitudes of effect",
                     row$notes, fixed = TRUE))
  expect_false(grepl("use low risk of bias studies only", row$notes,
                     fixed = TRUE))

  # The direction gate is what changed the answer, so the note has to explain
  # it rather than leave a reader with "similar" over a 30% shift.
  expect_match(row$notes, "direction gate (bias-favouring shift): no",
               fixed = TRUE)
  expect_match(row$notes, "exceeding the 20% threshold", fixed = TRUE)
  expect_match(row$notes, "the unfavourable direction given small_values",
               fixed = TRUE)
  expect_match(row$notes, "not in the bias-favouring direction", fixed = TRUE)

  expect_true(all(c("pma-rob-edge-magnitude-similar", "pma-rob-leaf-all")
                  %in% .rob_path(g)))
  expect_false("pma-rob-leaf-lowonly" %in% .rob_path(g))
})

test_that("not dominated: a sign flip is substantially different, at no depth", {
  # TE_all = 0.35 (above), TE_low = -0.50 (below): the zones flip across the
  # null, which is rule 5. The undominated branch has no levels to rate down,
  # so rule 5's depth must not surface anywhere on it -- the leaf, the
  # downgrade and the notes all have to read the same as any other
  # "substantially different" answer.
  m <- mk(te = c(2.90, -0.50, -0.50, -0.50),
          w  = c(400, 400 / 3, 400 / 3, 400 / 3))
  g <- quiet_grade(m, rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  row <- rob_row(g)
  expect_match(row$notes, "reaches rule 5", fixed = TRUE)
  expect_equal(g$rob_analysis_set, "low_only")
  expect_equal(row$judgment, "not_serious")
  expect_equal(row$downgrade, 0)
  expect_false(grepl("rate down 2", row$notes, fixed = TRUE))
  expect_false(grepl("two levels", row$notes, fixed = TRUE))
  # The rule verdict's own prose belongs to the dominated branch and stays
  # there, cap note and all; only the leaf crosses over.
  expect_false(grepl("Rule 5:", row$notes, fixed = TRUE))
  expect_false(grepl("two-level downgrade", row$notes, fixed = TRUE))
  expect_true("pma-rob-edge-magnitude-different" %in% .rob_path(g))
})

test_that("not dominated: an unassessable comparison takes its own edge", {
  # Low-RoB studies that exist and carry weight but have no usable standard
  # error. metagen() would drop them, so the assessor is driven directly.
  fake <- list(
    TE = c(-1.00, -0.9, -1.1, -1.0), seTE = c(0.1, NA, NA, NA),
    w.random = c(1, 1, 1, 1), random = TRUE,
    TE.random = -1.0, seTE.random = 0.2, sm = "RR")
  row <- pmatools:::.flowchart_rob(
    c("very_serious", "no", "no", "no"), fake,
    small_values = "desirable", threshold_internal = 0.5)
  ids <- strsplit(attr(row, "facts")$value[
    attr(row, "facts")$key == "flow_path"], " ", fixed = TRUE)[[1L]]

  expect_equal(row$judgment, "not_serious")
  expect_equal(attr(row, "analysis_set"), "all")
  expect_match(row$notes, "the magnitude question is never asked", fixed = TRUE)
  # The route shares the "similar" edge, which the figure labels
  # "Similar/Not assessable". Both answers reach the same leaf and rate down
  # neither way, so one arrow carries the pair; which of the two happened is
  # recorded in the notes and in fig2_branch, not in the shape.
  #
  # It used to have an edge of its own, and before that it was drawn on the
  # appreciable node's "no" edge -- deleted as unreachable, since not
  # dominating leaves the low-RoB studies at least the 35-45% of the weight
  # Core GRADE 4 calls appreciable.
  expect_true(all(c("pma-rob-edge-appreciable-yes",
                    "pma-rob-edge-magnitude-similar",
                    "pma-rob-leaf-all") %in% ids))
  expect_false("pma-rob-edge-magnitude-notassessable" %in% ids)
  expect_false("pma-rob-edge-appreciable-no" %in% ids)
  expect_false("pma-rob-leaf-lowonly" %in% ids)
  # And the note no longer claims the low-RoB evidence was not appreciable.
  expect_false(grepl("do not provide appreciable evidence", row$notes,
                     fixed = TRUE))
})

test_that("both leaves of the magnitude node disclose the directional reading", {
  # Core GRADE 4 words the node symmetrically; pmatools answers it with a
  # directional check. That departure is stated wherever the node is answered,
  # in the same register as the inconsistency two-level note.
  g_diff <- quiet_grade(make_low_only(), rob = c("very_serious", "no", "no", "no"),
                        small_values = "undesirable",
                        threshold = 1.05, threshold_scale = "ratio")
  g_sim <- quiet_grade(mk(te = c(0.03, 0.02, 0.02, 0.02),
                          w  = c(400, 400 / 3, 400 / 3, 400 / 3)),
                       rob = c("very_serious", "no", "no", "no"),
                       small_values = "undesirable",
                       threshold = 1.05, threshold_scale = "ratio")
  for (row in list(rob_row(g_diff), rob_row(g_sim))) {
    expect_match(row$notes, "Departure from the source", fixed = TRUE)
    expect_match(row$notes, "words this node symmetrically", fixed = TRUE)
    expect_match(row$notes, "same directional five-rule check", fixed = TRUE)
  }

  # A dominated assessment answers a different node and must not carry it.
  g_dom <- quiet_grade(mk(te = c(0.60, 0.40, 0.40), w = c(75, 12.5, 12.5)),
                       rob = c("very_serious", "no", "no"),
                       small_values = "undesirable",
                       threshold = 1.20, threshold_scale = "ratio")
  expect_false(grepl("words this node symmetrically", rob_row(g_dom)$notes,
                     fixed = TRUE))
})

test_that("both branches now read the same estimates the same way", {
  # The mirror-image fixture above with the weight moved: the high-RoB study
  # carries 60% instead of 50%, so Fig 2 takes the "check direction of bias"
  # branch. Same two estimates, same shift, same direction gate -- and the
  # point of reading the rule verdict on both branches is that the two sides of
  # the figure can no longer disagree about them.
  m <- mk(te = c(0.20, 0.50, 0.50, 0.50),
          w  = c(600, 400 / 3, 400 / 3, 400 / 3))
  g <- quiet_grade(m, rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  row <- rob_row(g)
  expect_match(row$notes, "dominated: yes", fixed = TRUE)
  expect_equal(row$judgment, "not_serious")
  expect_match(row$notes, "Rule 2", fixed = TRUE)
  expect_equal(g$rob_analysis_set, "all")

  # The undominated twin: same estimates, 50% weight, and the same rule.
  g_undom <- quiet_grade(mk(te = c(0.20, 0.50, 0.50, 0.50),
                            w  = c(400, 400 / 3, 400 / 3, 400 / 3)),
                         rob = c("very_serious", "no", "no", "no"),
                         small_values = "undesirable",
                         threshold = 1.05, threshold_scale = "ratio")
  expect_match(rob_row(g_undom)$notes, "dominated: no", fixed = TRUE)
  expect_match(rob_row(g_undom)$notes, "reaches rule 2", fixed = TRUE)
  expect_equal(g_undom$rob_analysis_set, g$rob_analysis_set)
})

# ---- B-1: study-level overrides -------------------------------------------

test_that("rob_overrides without a rationale aborts", {
  m <- mk(c(0.8, 0.02, 0.02), c(3, 1, 1),
          studlab = c("Smith 2020", "Jones 2019", "Lee 2021"))
  expect_error(
    grade_meta(m, rob = c("no", "no", "no"),
               small_values = "desirable",
               rob_overrides = c("Smith 2020" = "high"),
               threshold_type = "null"),
    regexp = "rob_override_rationale"
  )
  # A rationale for a *different* study does not satisfy the gate.
  expect_error(
    grade_meta(m, rob = c("no", "no", "no"),
               small_values = "desirable",
               rob_overrides = c("Smith 2020" = "high"),
               rob_override_rationale = c("Jones 2019" = "unblinded"),
               threshold_type = "null"),
    regexp = "rob_override_rationale"
  )
})

test_that("rob_overrides with an unknown studlab aborts and lists the labels", {
  m <- mk(c(0.8, 0.02, 0.02), c(3, 1, 1),
          studlab = c("Smith 2020", "Jones 2019", "Lee 2021"))
  expect_error(
    grade_meta(m, rob = c("no", "no", "no"),
               small_values = "desirable",
               rob_overrides = c("Smyth 2020" = "high"),
               rob_override_rationale = c("Smyth 2020" = "typo"),
               threshold_type = "null"),
    regexp = "Smyth 2020"
  )
  expect_error(
    grade_meta(m, rob = c("no", "no", "no"),
               small_values = "desirable",
               rob_overrides = c("Smyth 2020" = "high"),
               rob_override_rationale = c("Smyth 2020" = "typo"),
               threshold_type = "null"),
    regexp = "Lee 2021"
  )
})

test_that("every override is recorded in the domain notes", {
  m <- mk(c(0.8, 0.02, 0.02), c(3, 1, 1),
          studlab = c("Smith 2020", "Jones 2019", "Lee 2021"))
  g <- quiet_grade(
    m, rob = c("no", "no", "no"),
    rob_overrides = c("Smith 2020" = "high", "Jones 2019" = "Some concerns"),
    rob_override_rationale = c(
      "Smith 2020" = "Unblinded outcome assessment found after publication",
      "Jones 2019" = "Deviations from the registered analysis plan"
    ),
    small_values = "undesirable",
    threshold = 1.05, threshold_scale = "ratio"
  )
  notes <- rob_row(g)$notes
  expect_match(notes,
    "Study-level override: Smith 2020 not_serious -> very_serious (Unblinded outcome assessment found after publication)",
    fixed = TRUE)
  expect_match(notes,
    "Study-level override: Jones 2019 not_serious -> serious (Deviations from the registered analysis plan)",
    fixed = TRUE)
  # The override actually changed the classification: Smith 2020 now carries
  # 60% of the weight, so the evidence is dominated.
  expect_match(notes, "dominated: yes", fixed = TRUE)
})

test_that("rob_overrides rejects an unrecognized level", {
  m <- mk(c(0.8, 0.02, 0.02), c(3, 1, 1),
          studlab = c("Smith 2020", "Jones 2019", "Lee 2021"))
  expect_error(
    grade_meta(m, rob = c("no", "no", "no"),
               small_values = "desirable",
               rob_overrides = c("Smith 2020" = "catastrophic"),
               rob_override_rationale = c("Smith 2020" = "because"),
               threshold_type = "null"),
    regexp = "not a recognized risk-of-bias level"
  )
})

# ---- B-3: the refit drives every downstream domain ------------------------

test_that("downstream domains use the refitted (low-RoB) analysis", {
  m       <- make_low_only()
  rob_vec <- c("very_serious", "no", "no", "no")

  g_refit <- quiet_grade(m, rob = rob_vec, small_values = "undesirable",
                         threshold = 1.05, threshold_scale = "ratio")
  g_full  <- quiet_grade(m, rob = rob_vec, small_values = "undesirable",
                         threshold = 1.05, threshold_scale = "ratio",
                         rob_refit = FALSE)
  # Ground truth: the same meta-analysis restricted by hand.
  m_low   <- mk(te = c(0.02, 0.02, 0.02), w = rep(400 / 3, 3),
                studlab = c("Low-1", "Low-2", "Low-3"))
  g_low   <- quiet_grade(m_low, threshold = 1.05, threshold_scale = "ratio",
    small_values = "desirable")

  # The refit must actually change the answers, otherwise the test is vacuous.
  expect_false(identical(row_of(g_refit, "Imprecision")$judgment,
                         row_of(g_full,  "Imprecision")$judgment))
  expect_false(identical(row_of(g_refit, "Inconsistency")$judgment,
                         row_of(g_full,  "Inconsistency")$judgment))

  expect_equal(row_of(g_refit, "Imprecision")$judgment,
               row_of(g_low,   "Imprecision")$judgment)
  expect_equal(row_of(g_refit, "Inconsistency")$judgment,
               row_of(g_low,   "Inconsistency")$judgment)
})

test_that("the rating target is derived from the refitted point estimate", {
  m       <- make_low_only()
  rob_vec <- c("very_serious", "no", "no", "no")

  g_refit <- quiet_grade(m, rob = rob_vec, small_values = "undesirable",
                         threshold = 1.05, threshold_scale = "ratio")
  g_full  <- quiet_grade(m, rob = rob_vec, small_values = "undesirable",
                         threshold = 1.05, threshold_scale = "ratio",
                         rob_refit = FALSE)

  # Full analysis: |TE| = 0.61 > log(1.05) -> important effect.
  expect_equal(g_full$rating_target,  "important_effect")
  # Restricted analysis: |TE| = 0.02 <= log(1.05) -> little or no difference.
  expect_equal(g_refit$rating_target, "little_to_no_difference")
})

test_that("rob_refit = FALSE returns the recommendation without refitting", {
  g <- quiet_grade(make_low_only(), rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio",
                   rob_refit = FALSE)
  expect_equal(g$rob_analysis_set, "low_only")
  expect_false(g$rob_refit)
  expect_equal(g$meta$k, g$meta_full$k)
  expect_match(rob_row(g)$notes, "rob_refit = FALSE", fixed = TRUE)
})

test_that("a refit that would leave k < 2 is skipped with a warning", {
  # Two studies: the high-RoB one carries 40% (not dominated) but restricting
  # to low RoB leaves a single study, which cannot be pooled.
  m <- mk(te = c(1.2, 0.02), w = c(100, 150), studlab = c("High-1", "Low-1"))
  expect_warning(
    g <- grade_meta(m, rob = c("very_serious", "no"), small_values = "undesirable",
                    threshold = 1.05, threshold_scale = "ratio",
                    pubias_unpublished = "no"),
    regexp = "cannot be pooled"
  )
  expect_equal(g$rob_analysis_set, "low_only")
  expect_false(g$rob_refit)
  expect_equal(g$meta$k, g$meta_full$k)
})

# ---- B-3b: k-space vs studlab-space alignment ------------------------------
#
# {meta} keeps $studlab / $TE at the length of the original data rows but
# counts only the estimable ones in $k. The flowchart works in k-space; the
# refit's `subset =` and `rob_overrides` work in studlab space. The gap study
# sits in the MIDDLE of these fixtures on purpose: an off-by-one alignment
# would silently exclude the wrong study rather than error.
make_low_only_gap <- function() {
  # make_low_only() with a non-estimable study inserted at position 2.
  mk(te = c(1.2, NA, 0.02, 0.02, 0.02),
     w  = c(400, NA, 400 / 3, 400 / 3, 400 / 3),
     studlab = c("High-1", "Gap-1", "Low-1", "Low-2", "Low-3"))
}

test_that("k == length(studlab): high_idx is unchanged", {
  g <- quiet_grade(make_low_only(), rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  d_rob <- assess_rob(c("very_serious", "no", "no", "no"), make_low_only(),
                      small_values = "undesirable",
                      threshold_internal = log(1.05))
  expect_equal(attr(d_rob, "high_idx"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(g$rob_analysis_set, "low_only")
  expect_true(g$rob_refit)
  expect_equal(g$meta$studlab, c("Low-1", "Low-2", "Low-3"))
})

test_that("k < length(studlab): a k-length rob still refits, on the right studies", {
  m <- make_low_only_gap()
  expect_equal(m$k, 4L)
  expect_equal(length(m$studlab), 5L)

  d_rob <- assess_rob(c("very_serious", "no", "no", "no"), m,
                      small_values = "undesirable",
                      threshold_internal = log(1.05))
  # Studlab-aligned, and the non-estimable study is not "high": a k-length
  # vector carries no judgment for it.
  expect_equal(attr(d_rob, "high_idx"),
               c(TRUE, FALSE, FALSE, FALSE, FALSE))

  g <- quiet_grade(m, rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  expect_equal(g$rob_analysis_set, "low_only")
  expect_true(g$rob_refit)
  # Only High-1 is dropped; Gap-1 is retained but contributes nothing, so the
  # pool is the three low-RoB studies.
  expect_equal(g$meta$studlab, c("Gap-1", "Low-1", "Low-2", "Low-3"))
  expect_equal(g$meta$k, 3L)
  expect_equal(g$meta_full$k, 4L)
  expect_match(rob_row(g)$notes, "(3 of 4 studies)", fixed = TRUE)
})

test_that("k < length(studlab): a studlab-length rob gives the same flowchart", {
  m <- make_low_only_gap()
  d_k    <- assess_rob(c("very_serious", "no", "no", "no"), m,
                       small_values = "undesirable",
                       threshold_internal = log(1.05))
  d_slab <- assess_rob(c("very_serious", "very_serious", "no", "no", "no"), m,
                       small_values = "undesirable",
                       threshold_internal = log(1.05))
  # The extra study is trimmed before the k-space maths, so the judgment and
  # every note (weight share, count share, level table) are identical.
  expect_equal(d_slab$judgment, d_k$judgment)
  expect_equal(d_slab$notes, d_k$notes)
  expect_match(d_slab$notes, "High-RoB studies: 1/4", fixed = TRUE)
  expect_equal(attr(d_slab, "analysis_set"), attr(d_k, "analysis_set"))

  # ... but the non-estimable study's own High rating does exclude it from
  # the refit subset.
  expect_equal(attr(d_slab, "high_idx"),
               c(TRUE, TRUE, FALSE, FALSE, FALSE))
  g <- quiet_grade(m, rob = c("very_serious", "very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  expect_true(g$rob_refit)
  expect_equal(g$meta$studlab, c("Low-1", "Low-2", "Low-3"))
})

test_that("rob must be length k or length(studlab)", {
  m <- make_low_only_gap()
  expect_error(
    assess_rob(c("very_serious", "no", "no"), m, threshold_internal = log(1.05),
      small_values = "desirable"),
    regexp = "length k \\(4\\) or length\\(meta_obj\\$studlab\\) \\(5\\)"
  )
})

test_that("rob_overrides key on studlab when k < length(studlab)", {
  m <- make_low_only_gap()
  d <- assess_rob(c("very_serious", "no", "no", "no"), m,
                  rob_overrides          = c("Low-3" = "high"),
                  rob_override_rationale = c("Low-3" = "Unblinded outcome assessment"),
                  small_values = "undesirable",
                  threshold_internal = log(1.05))
  expect_match(d$notes,
    "Study-level override: Low-3 not_serious -> very_serious (Unblinded outcome assessment)",
    fixed = TRUE)
  expect_equal(attr(d, "high_idx"), c(TRUE, FALSE, FALSE, FALSE, TRUE))

  # A study {meta} could not pool can be overridden too: it has no assessed
  # level in a k-length vector, but it is still a row of the data.
  d_gap <- assess_rob(c("very_serious", "no", "no", "no"), m,
                      rob_overrides          = c("Gap-1" = "high"),
                      rob_override_rationale = c("Gap-1" = "Results never reported"),
                      small_values = "undesirable",
                      threshold_internal = log(1.05))
  expect_match(d_gap$notes,
    "Study-level override: Gap-1 not estimable -> very_serious (Results never reported)",
    fixed = TRUE)
  expect_equal(attr(d_gap, "high_idx"), c(TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("a metabin that drops a double-zero study refits end to end", {
  # method = "Inverse" drops the double-zero study (Gap-1) from the pool, so
  # k = 4 while studlab has 5 entries -- the shape that used to make the refit
  # abort with "does not align with the meta object".
  m <- metabin(event.e = c(60, 0, 51, 51, 51), n.e = rep(c(100, 50, 100), c(1, 1, 3)),
               event.c = c(30, 0, 50, 50, 50), n.c = rep(c(100, 50, 100), c(1, 1, 3)),
               studlab = c("High-1", "Gap-1", "Low-1", "Low-2", "Low-3"),
               sm = "RR", method = "Inverse")
  expect_equal(m$k, 4L)
  expect_equal(length(m$studlab), 5L)

  g <- quiet_grade(m, rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  expect_equal(g$rob_analysis_set, "low_only")
  expect_true(g$rob_refit)
  expect_equal(g$meta$k, 3L)
  expect_equal(g$meta_full$k, 4L)
  expect_lt(g$meta$k, g$meta_full$k)
  expect_false(isTRUE(all.equal(g$meta$TE.random, g$meta_full$TE.random)))
  expect_equal(g$meta$studlab, c("Gap-1", "Low-1", "Low-2", "Low-3"))
})

# ---- B-3: the refit is never silent ---------------------------------------

test_that("sof_table footer states the low-RoB restriction", {
  skip_if_not_installed("flextable")
  footer_text <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")

  g <- quiet_grade(make_low_only(), rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  expect_match(
    footer_text(sof_table(g)),
    "Effect estimate restricted to low risk of bias studies (n = 3 of 4)",
    fixed = TRUE
  )

  # The unapplied recommendation is stated too.
  g_no_refit <- quiet_grade(make_low_only(), rob = c("very_serious", "no", "no", "no"),
                            small_values = "undesirable",
                            threshold = 1.05, threshold_scale = "ratio",
                            rob_refit = FALSE)
  expect_match(footer_text(sof_table(g_no_refit)),
               "rob_refit = FALSE", fixed = TRUE)

  # An ordinary all-studies analysis gains no extra footer line.
  g_all <- quiet_grade(mk(c(0.03, 0.02, 0.02), c(1, 1, 1)),
                       small_values = "desirable",
                       rob = c("no", "no", "no"),
                       threshold = 1.05, threshold_scale = "ratio")
  expect_no_match(footer_text(sof_table(g_all)),
                  "low risk of bias studies")
})

test_that("print.pmatools reports the analysis set after a refit", {
  g <- quiet_grade(make_low_only(), rob = c("very_serious", "no", "no", "no"),
                   small_values = "undesirable",
                   threshold = 1.05, threshold_scale = "ratio")
  expect_output(print(g), "low risk of bias studies only \\(3 of 4 studies\\)")
})

# ---- B-4: the bundled script reproduces the new arguments -----------------

test_that("the reproducibility script carries the new RoB arguments", {
  skip_if_not_installed("flextable")
  m <- mk(c(0.8, 0.02, 0.02), c(3, 1, 1),
          studlab = c("Smith 2020", "Jones 2019", "Lee 2021"))
  g <- quiet_grade(
    m, rob = c("no", "no", "no"),
    rob_overrides = c("Smith 2020" = "high"),
    rob_override_rationale = c("Smith 2020" = "Unblinded outcome assessment"),
    small_values = "undesirable",
    threshold = 1.05, threshold_scale = "ratio"
  )

  out <- file.path(tempdir(), "analysis_rob_args.R")
  pmatools:::.render_analysis_script(
    m, g,
    ma_args    = NULL,
    grade_args = list(
      rob_some_concerns      = list(value = "high",  origin = "scalar"),
      rob_overrides          = list(value = c("Smith 2020" = "high"),
                                    origin = "vector"),
      rob_override_rationale = list(
        value  = c("Smith 2020" = "Unblinded outcome assessment"),
        origin = "vector"),
      rob_dominant_threshold = list(value = 0.55, origin = "scalar"),
      rob_refit              = list(value = FALSE, origin = "scalar")
    ),
    per = 1000, prediction = FALSE,
    convert_smd_to_or = FALSE, baseline_risk = NULL, threshold_label = NULL,
    out_path = out
  )
  script <- paste(readLines(out), collapse = "\n")

  expect_match(script, 'rob_some_concerns       = "high"', fixed = TRUE)
  expect_match(script, "rob_overrides           = c('Smith 2020' = 'high')",
               fixed = TRUE)
  expect_match(script,
    "rob_override_rationale  = c('Smith 2020' = 'Unblinded outcome assessment')",
    fixed = TRUE)
  expect_match(script, "rob_dominant_threshold  = 0.55", fixed = TRUE)
  expect_match(script, "rob_refit               = FALSE", fixed = TRUE)
})

test_that("the reproducibility script defaults are valid R", {
  skip_if_not_installed("flextable")
  m <- mk(c(0.03, 0.02, 0.02), c(1, 1, 1))
  g <- quiet_grade(m, rob = c("no", "no", "no"),
                   small_values = "desirable",
                   threshold = 1.05, threshold_scale = "ratio")

  out <- file.path(tempdir(), "analysis_rob_defaults.R")
  pmatools:::.render_analysis_script(
    m, g, ma_args = NULL, grade_args = NULL,
    per = 1000, prediction = FALSE,
    convert_smd_to_or = FALSE, baseline_risk = NULL, threshold_label = NULL,
    out_path = out
  )
  script <- paste(readLines(out), collapse = "\n")

  expect_match(script, 'rob_some_concerns       = "low"', fixed = TRUE)
  expect_match(script, "rob_overrides           = NULL", fixed = TRUE)
  expect_match(script, "rob_override_rationale  = NULL", fixed = TRUE)
  expect_match(script, "rob_refit               = TRUE", fixed = TRUE)
  expect_silent(parse(text = script))
})
