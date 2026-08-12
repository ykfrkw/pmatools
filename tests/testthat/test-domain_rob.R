library(testthat)

skip_if_not_installed("meta")

# These fixtures exercise the direction-of-bias check (the 5 zone rules). Every
# one carries 80% of the weight on the high-RoB study, so under the v0.5
# Core GRADE 4 Fig 2 flowchart they all take the *dominated* branch, where the
# 5-rule verdict is used verbatim — which is why none of the expectations below
# changed when the dominance gate was reinstated. The non-dominated branch (no
# downgrade; analysis-set recommendation instead) is covered in
# test-rob_flowchart.R.
#
# Mock dominated meta object: 1 large high-RoB study + 2 small low-RoB studies.
# TE values supplied here are on the analysis scale (log scale for RR/OR).
# `te_all` is the random-effects pooled estimate; `te_low_only` is set as the
# study-level TE for the two low-RoB studies, which becomes TE_low (the IV-
# weighted mean of low-RoB studies) by construction.
make_mock_dominated <- function(te_all, te_low_only,
                                seTE.random = 0.10,
                                seTE        = c(0.10, 0.45, 0.45),
                                sm          = "RR") {
  m <- list(
    k            = 3L,
    w.random     = c(80, 10, 10),
    TE           = c(te_all, te_low_only, te_low_only),
    seTE         = seTE,
    TE.random    = te_all,
    seTE.random  = seTE.random,
    lower.random = te_all - 0.4,
    upper.random = te_all + 0.4,
    sm           = sm,
    I2           = 0.10,
    tau2         = 0.01,
    pval.Q       = 0.30,
    event.e      = c(40, 5, 5),
    event.c      = c(10, 4, 4),
    n.e          = c(200, 20, 20),
    n.c          = c(200, 20, 20),
    studlab      = c("Large-A", "Small-B", "Small-C"),
    data         = NULL
  )
  class(m) <- "meta"
  m
}

# --- Rule 1: same trivial zone -----------------------------------------------
test_that("Rule 1: TE_all and TE_low both in trivial zone -> no", {
  # log(1.20) ~ 0.182; both 0.05 and 0.05 fall inside +/-0.182.
  m <- make_mock_dominated(te_all = 0.05, te_low_only = 0.05)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_match(rob_row$notes, "Rule 1")
})

# --- Rule 2: same non-trivial zone, inflation within threshold ---------------
test_that("Rule 2: same non-trivial zone, inflation <= 10% -> no", {
  # Both te_all=0.50 and te_low=0.48 are above +log(1.20)=0.182 (zone 'above').
  # inflation = (0.50 - 0.48) / 0.48 = 4.2% < 10%.
  m <- make_mock_dominated(te_all = 0.50, te_low_only = 0.48)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_match(rob_row$notes, "Rule 2")
})

test_that("Rule 2: same non-trivial zone, deflating direction -> no", {
  # te_all=0.40 < te_low=0.60: high-RoB pulls *toward* null; not bias-favouring.
  # Both still in 'above' zone (above +0.182). -> Rule 2.
  m <- make_mock_dominated(te_all = 0.40, te_low_only = 0.60)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_match(rob_row$notes, "Rule 2")
})

# --- Rule 3: same non-trivial zone, bias-favouring inflation > threshold -----
test_that("Rule 3: same non-trivial zone, inflation > 10% -> some_concerns", {
  # Both te_all=0.60 and te_low=0.40 in 'above' zone; inflation = 50% > 10%.
  m <- make_mock_dominated(te_all = 0.60, te_low_only = 0.40)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_match(rob_row$notes, "Rule 3")
})

# --- Rule 4: zone changes without sign flip ----------------------------------
test_that("Rule 4: 'above' -> 'trivial' zone change -> some_concerns", {
  # te_all=0.50 (above), te_low=0.10 (trivial). Zones differ; no sign flip.
  m <- make_mock_dominated(te_all = 0.50, te_low_only = 0.10)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_match(rob_row$notes, "Rule 4")
})

test_that("Rule 4: 'trivial' -> 'above' zone change -> some_concerns", {
  # high-RoB pulls into trivial: te_all=0.10 (trivial), te_low=0.50 (above).
  m <- make_mock_dominated(te_all = 0.10, te_low_only = 0.50)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_match(rob_row$notes, "Rule 4")
})

# --- Rule 5: zone change with sign flip --------------------------------------
# Updated (v0.5.1): rule 5 now caps at -1. Core GRADE 4 describes no automatic
# two-level risk-of-bias downgrade (every Fig 2 leaf is "rate down" / "do not
# rate down"); -2 requires the scalar rob override.
test_that("Rule 5: 'above' <-> 'below' sign flip -> some_concerns (capped at -1)", {
  m <- make_mock_dominated(te_all = 0.50, te_low_only = -0.50)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_equal(rob_row$downgrade, -1L)
  expect_match(rob_row$notes, "Rule 5")
  expect_match(rob_row$notes, "capped at one level", fixed = TRUE)
})

test_that("Rule 5 can still reach -2 through the scalar rob override", {
  m <- make_mock_dominated(te_all = 0.50, te_low_only = -0.50)
  g <- grade_meta(m, rob = "serious",
                  rob_rationale = "Sign flip when high-RoB studies are removed",
                  threshold = 1.20, threshold_scale = "ratio")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_equal(rob_row$downgrade, -2L)
})

# --- Fallback: Threshold not supplied ---------------------------------------
# Updated (v0.5.1): same -1 cap as above.
test_that("Fallback: Threshold not supplied + sign flip -> some_concerns (rule 5)", {
  m <- make_mock_dominated(te_all = 1.0, te_low_only = -0.5)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10, threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_match(rob_row$notes, "Threshold not supplied")
})

test_that("Fallback: Threshold not supplied + same-sign small inflation -> no (rule 2)", {
  # Without Threshold, trivial zone collapses to {0}; both 0.05 and 0.04 are 'above'.
  # te_all < te_low under small_values='undesirable' -> direction_ok FALSE -> rule 2.
  m <- make_mock_dominated(te_all = 0.04, te_low_only = 0.05)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10, threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_match(rob_row$notes, "Rule 2")
})

# --- Direction gate transparency ---------------------------------------------
test_that("Direction gate blocks downgrade despite ratio > threshold -> no + explanation", {
  # Both te_all = -0.60 and te_low = -0.40 sit in the 'below' zone
  # (below -log(1.20) ~ -0.182). |TE| change = (0.60 - 0.40)/0.40 = 50% > 10%,
  # but under small_values = 'undesirable' only te_all > te_low is
  # bias-favouring; here te_all < te_low (shift toward smaller values), so the
  # direction gate blocks the downgrade (rule 2) and the note must say why.
  m <- make_mock_dominated(te_all = -0.60, te_low_only = -0.40)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_match(rob_row$notes, "Rule 2")
  expect_match(rob_row$notes, "direction gate \\(bias-favouring shift\\): no")
  expect_match(rob_row$notes, "exceeding the 10% threshold", fixed = TRUE)
  expect_match(rob_row$notes, "small_values = 'undesirable'", fixed = TRUE)
  expect_match(rob_row$notes, "no downgrade for this criterion", fixed = TRUE)
})

test_that("Direction gate result is always reported in notes", {
  # Bias-favouring case (rule 3): gate = yes.
  m <- make_mock_dominated(te_all = 0.60, te_low_only = 0.40)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob_row$notes, "direction gate \\(bias-favouring shift\\): yes")
  expect_match(rob_row$notes, "relative inflation")
  expect_match(rob_row$notes, "threshold 10%", fixed = TRUE)
})

test_that("small_values = NULL: warning when |TE| assumption drives a rule-3 downgrade", {
  # Same non-trivial zone, ratio 50% > 10%; with small_values = NULL the gate
  # falls back to |TE_all| > |TE_low|, which decides the downgrade -> warn once.
  m <- make_mock_dominated(te_all = 0.60, te_low_only = 0.40)
  expect_warning(
    g <- grade_meta(m, rob = c("serious", "no", "no"),
                    small_values = NULL,
                    threshold = 1.20, threshold_scale = "ratio",
                    rob_inflation_threshold = 0.10),
    regexp = "small_values"
  )
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_match(rob_row$notes, "Rule 3")
})

test_that("small_values = NULL: no warning when the gate is not decisive", {
  # Deflating direction (ratio negative): no downgrade regardless of
  # small_values, so no warning should be emitted.
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  expect_no_warning(
    grade_meta(m, rob = c("serious", "no", "no"),
               small_values = NULL,
               rob_inflation_threshold = 0.10, threshold_type = "null"),
    message = "small_values"
  )
})

# --- small_values direction handling ----------------------------------------
test_that("small_values = NULL: high-RoB toward null does NOT rate down", {
  # |te_all|=0.2 < |te_low|=1.1 -> direction_ok FALSE; both 'above' (no Threshold).
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10, threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
})

# --- Threshold = 0 backward compatibility -----------------------------------
test_that("Threshold = 0 inside same non-trivial zone rates down for any inflation", {
  # te_all=0.20 (above), te_low=0.19 (above); 5% inflation; threshold 0 -> rule 3.
  m <- make_mock_dominated(te_all = 0.20, te_low_only = 0.19)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_match(rob_row$notes, "Rule 3")
})

# --- Reporting ---------------------------------------------------------------
test_that("weight_note reports both count % and weight %", {
  m <- make_mock_dominated(te_all = 0.05, te_low_only = 0.05)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob_row$notes, "by count")
  expect_match(rob_row$notes, "by weight")
})

test_that("All studies high-RoB -> some_concerns (1 level down, no comparator pool)", {
  # Updated (v0.5.1): this used to rate down 2 levels. Core GRADE 4 supports no
  # automatic two-level risk-of-bias downgrade, so the automated judgment is
  # capped at -1; -2 requires rob = "serious" + rob_rationale.
  m <- make_mock_dominated(te_all = 0.30, te_low_only = 0.30)
  g <- grade_meta(m, rob = c("serious", "serious", "serious"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "some_concerns")
  expect_equal(rob_row$downgrade, -1L)
  expect_match(rob_row$notes, "All studies high-RoB", fixed = TRUE)
  expect_match(rob_row$notes, "capped at one level", fixed = TRUE)
})

test_that("diff_note reports zone labels and relative inflation", {
  m <- make_mock_dominated(te_all = 0.60, te_low_only = 0.40)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob_row$notes, "\\[zone = above\\]")
  expect_match(rob_row$notes, "relative inflation")
  expect_false(grepl("\\|TE_all\\|", rob_row$notes))
  expect_false(grepl("CI overlap", rob_row$notes))
})
