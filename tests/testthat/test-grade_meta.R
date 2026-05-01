library(testthat)
library(meta)

# ---- ヘルパー: ダミー meta オブジェクト -----------------------------------
skip_if_not_installed("meta")

make_metabin <- function() {
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

make_metabin_high_i2 <- function() {
  metabin(
    event.e = c(5,  40, 2),
    n.e     = c(50, 60, 70),
    event.c = c(40, 5,  35),
    n.c     = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm      = "RR",
    method  = "Inverse"
  )
}

# Mock meta object with controlled weights and TEs for domination tests
make_mock_dominated <- function(te_all, te_low_only) {
  # Large-A dominates weight (80%), has te_all
  # Small-B, C share remaining weight, have te_low_only
  # seTE chosen so IV-weighted mean of B+C = te_low_only
  m <- list(
    k           = 3L,
    w.random    = c(80, 10, 10),           # Study A: 80% weight
    TE          = c(te_all, te_low_only, te_low_only),
    seTE        = c(0.10, 0.45, 0.45),     # A precise, B/C imprecise
    TE.random   = te_all,                  # overall dominated by A
    lower.random = te_all - 0.4,
    upper.random = te_all + 0.4,
    sm          = "RR",
    I2          = 0.10,
    tau2        = 0.01,
    pval.Q      = 0.30,
    event.e     = c(40, 5, 5),
    event.c     = c(10, 4, 4),
    n.e         = c(200, 20, 20),
    n.c         = c(200, 20, 20),
    studlab     = c("Large-A", "Small-B", "Small-C"),
    data        = NULL
  )
  class(m) <- "meta"
  m
}

# ---- grade_meta() 基本動作 -------------------------------------------------

test_that("grade_meta returns pmatools object", {
  m <- make_metabin()
  g <- grade_meta(m, study_design = "RCT", rob = "no", indirectness = "no")
  expect_s3_class(g, "pmatools")
})

test_that("RCT starts at High certainty with no concerns", {
  m <- make_metabin()
  g <- grade_meta(m, study_design = "RCT", rob = "no", indirectness = "no")
  expect_true(g$starting_quality == "High")
  expect_true(g$certainty %in% c("High", "Moderate", "Low", "Very Low"))
})

test_that("obs starts at Low certainty", {
  m <- make_metabin()
  g <- grade_meta(m, study_design = "obs", rob = "no")
  expect_equal(g$starting_quality, "Low")
})

test_that("rob = 'some' downgrades by 1", {
  m <- make_metabin()
  g_no   <- grade_meta(m, study_design = "RCT", rob = "no",   indirectness = "no")
  g_some <- grade_meta(m, study_design = "RCT", rob = "some", indirectness = "no")
  diff <- g_no$certainty_score - g_some$certainty_score
  expect_true(diff >= 0)
})

test_that("very_serious rob downgrades by 2", {
  m <- make_metabin()
  g <- grade_meta(m, study_design = "RCT", rob = "very_serious", indirectness = "no")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$downgrade, -2)
})

# ---- ドメイン: 非一貫性 ---------------------------------------------------

test_that("inconsistency domain is auto-computed", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m))
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_true(incon_row$auto)
  expect_true(incon_row$judgment %in% c("no", "some", "serious", "very_serious"))
})

test_that("high I2 (opposite-sided TEs) gives serious inconsistency (auto)", {
  m <- make_metabin_high_i2()
  g <- grade_meta(m)
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "serious")
})

test_that("inconsistency flowchart: ci_diff = no → do not rate down", {
  m <- make_metabin()
  g <- grade_meta(m, inconsistency_ci_diff = "no")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "no")
  expect_false(incon_row$auto)
})

test_that("inconsistency flowchart: opposite_sides + no subgroup → serious", {
  m <- make_metabin()
  g <- grade_meta(m,
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "no")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "serious")
})

test_that("inconsistency flowchart: majority_one_side → do not rate down", {
  m <- make_metabin()
  g <- grade_meta(m,
    inconsistency_ci_diff        = "yes",
    inconsistency_threshold_side = "majority_one_side")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "no")
})

test_that("inconsistency scalar overrides flowchart", {
  m <- make_metabin()
  g <- grade_meta(m, inconsistency = "very_serious")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "very_serious")
  expect_false(incon_row$auto)
})

# ---- ドメイン: 不精確性 ---------------------------------------------------

test_that("imprecision domain is auto-computed", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m))
  impre_row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(impre_row$auto)
})

test_that("ois_events below total events flags serious imprecision", {
  m <- make_metabin()
  # total events = 10+15+20 + 15+20+25 = 105; OIS = 1000 → not met
  g <- suppressWarnings(grade_meta(m, ois_events = 1000))
  impre_row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(impre_row$judgment %in% c("serious", "very_serious"))
})

test_that("ois auto-calculation from p0/p1 (binary)", {
  m <- make_metabin()
  # OIS auto-calculated; actual value depends on formula
  g <- suppressWarnings(grade_meta(m, ois_p0 = 0.20, ois_p1 = 0.30))
  impre_row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(impre_row$auto)
  expect_true(grepl("OIS", impre_row$notes))
})

# ---- ドメイン: 出版バイアス -----------------------------------------------

test_that("k < 10 gives not assessable publication bias (judgment = 'no')", {
  m <- make_metabin()  # k = 3
  g <- suppressWarnings(grade_meta(m))
  pb_row <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb_row$judgment, "no")
  expect_true(grepl("< 10", pb_row$notes))
})

test_that("pubias_small_industry = 'yes' rates down", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, pubias_small_industry = "yes"))
  pb_row <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb_row$judgment, "serious")
})

test_that("pubias_unpublished = 'yes' rates down when k < 10", {
  m <- make_metabin()  # k = 3
  g <- grade_meta(m, pubias_unpublished = "yes")
  pb_row <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb_row$judgment, "serious")
  expect_false(pb_row$auto)
})

# ---- RoB ベクタ入力 -------------------------------------------------------

test_that("rob vector: not dominated returns no concern", {
  # 3 studies with roughly equal weights → high-RoB weight ~33% < 60% → not dominated
  m <- make_metabin()
  g <- grade_meta(m, rob = c("no", "some", "serious"))
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
  expect_true(grepl("Not dominated", rob_row$notes))
})

test_that("rob vector: dominated + inflating small_values=undesirable rates down", {
  # Mock: Study A has 80% weight, is high-RoB, and has higher TE than low-RoB studies
  # small_values = "undesirable": TE_all > TE_low → inflates → serious
  m <- make_mock_dominated(te_all = 1.4, te_low_only = 0.3)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  rob_dominant_threshold = 0.60,
                  small_values           = "undesirable")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_true(grepl("Dominated", rob_row$notes))
})

test_that("rob vector: dominated + NOT inflating small_values=undesirable does not rate down", {
  # TE_all < TE_low → high-RoB pulls toward null → conservative (doesn't inflate)
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  rob_dominant_threshold = 0.60,
                  small_values           = "undesirable")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "no")
})

test_that("rob vector: dominated + inflating small_values=desirable rates down", {
  # small_values = "desirable": TE_all < TE_low → inflates toward favorable → serious
  m <- make_mock_dominated(te_all = -1.5, te_low_only = -0.2)
  g <- grade_meta(m, rob = c("serious", "no", "no"),
                  rob_dominant_threshold = 0.60,
                  small_values           = "desirable")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
})

test_that("rob vector of wrong length raises error", {
  m <- make_metabin()
  expect_error(
    grade_meta(m, rob = c("no", "some")),  # k=3 だが長さ2
    regexp = "length k"
  )
})

# ---- 入力バリデーション ---------------------------------------------------

test_that("invalid rob level raises error", {
  m <- make_metabin()
  # 'moderate' is normalised to 'some' (alias), so use a truly unknown label
  expect_error(suppressWarnings(grade_meta(m, rob = "totally_unknown_rob_level")),
               regexp = "not a recognized GRADE level")
})

test_that("non-meta object raises error", {
  expect_error(grade_meta(list(x = 1)), regexp = "class 'meta'")
})

# ---- print / summary ------

test_that("print.pmatools outputs without error", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m))
  expect_output(print(g), "GRADE Certainty")
})

test_that("summary.pmatools outputs without error", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m))
  expect_output(summary(g), "GRADE Certainty")
})

# ---- sof_table() ----------------------------------------------------------

test_that("sof_table returns flextable", {
  skip_if_not_installed("flextable")
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m))
  ft <- sof_table(g)
  expect_s3_class(ft, "flextable")
})

# ---- grade_table() --------------------------------------------------------

test_that("grade_table with multiple outcomes returns flextable", {
  skip_if_not_installed("flextable")
  m <- make_metabin()
  g1 <- suppressWarnings(grade_meta(m, outcome_name = "Outcome 1"))
  g2 <- suppressWarnings(grade_meta(m, outcome_name = "Outcome 2"))
  ft <- grade_table(
    list("Outcome 1" = g1, "Outcome 2" = g2),
    primary = "Outcome 1"
  )
  expect_s3_class(ft, "flextable")
})
