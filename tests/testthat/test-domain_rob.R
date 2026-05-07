library(testthat)

skip_if_not_installed("meta")

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
test_that("Rule 5: 'above' <-> 'below' sign flip -> serious", {
  m <- make_mock_dominated(te_all = 0.50, te_low_only = -0.50)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Rule 5")
})

# --- Fallback: Threshold not supplied ---------------------------------------
test_that("Fallback: Threshold not supplied + sign flip -> serious (rule 5)", {
  m <- make_mock_dominated(te_all = 1.0, te_low_only = -0.5)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Threshold not supplied")
})

test_that("Fallback: Threshold not supplied + same-sign small inflation -> no (rule 2)", {
  # Without Threshold, trivial zone collapses to {0}; both 0.05 and 0.04 are 'above'.
  # te_all < te_low under small_values='undesirable' -> direction_ok FALSE -> rule 2.
  m <- make_mock_dominated(te_all = 0.04, te_low_only = 0.05)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_match(rob_row$notes, "Rule 2")
})

# --- small_values direction handling ----------------------------------------
test_that("small_values = NULL: high-RoB toward null does NOT rate down", {
  # |te_all|=0.2 < |te_low|=1.1 -> direction_ok FALSE; both 'above' (no Threshold).
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10)
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

test_that("All studies high-RoB -> serious (2 levels down, no comparator pool)", {
  # Every study is rated 'serious'. The zone-based check cannot run because
  # there is no low/some-RoB comparator pool. The entire body of evidence
  # rests on high-RoB studies, so we rate down 2 levels.
  m <- make_mock_dominated(te_all = 0.30, te_low_only = 0.30)
  g <- grade_meta(m, rob = c("serious", "serious", "serious"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_equal(rob_row$downgrade, -2L)
  expect_match(rob_row$notes, "All studies high-RoB", fixed = TRUE)
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
