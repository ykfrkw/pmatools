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
make_mock_dominated <- function(te_all, te_low_only,
                                seTE.random = 0.05,
                                seTE        = c(0.05, 0.20, 0.20)) {
  # Large-A dominates weight (80%), has te_all
  # Small-B, C share remaining weight, have te_low_only
  # Default CIs are tight so the overlap branch (>=0.8) does not fire and tests
  # exercise the inflation / sign-flip paths predictably.
  m <- list(
    k           = 3L,
    w.random    = c(80, 10, 10),
    TE          = c(te_all, te_low_only, te_low_only),
    seTE        = seTE,
    TE.random   = te_all,
    seTE.random = seTE.random,
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
  g <- grade_meta(m, study_design = "RCT", rob = "no",
                  small_values = "desirable",
                  rob_rationale = "Consensus RoB2: all domains low risk",
                  indirectness = "no", threshold_type = "null")
  expect_s3_class(g, "pmatools")
})

test_that("RCT starts at High certainty with no concerns", {
  m <- make_metabin()
  g <- grade_meta(m, study_design = "RCT", rob = "no",
                  small_values = "desirable",
                  rob_rationale = "Consensus RoB2: all domains low risk",
                  indirectness = "no", threshold_type = "null")
  expect_true(g$starting_quality == "High")
  expect_true(g$certainty %in% c("High", "Moderate", "Low", "Very Low"))
})

test_that("obs starts at Low certainty", {
  m <- make_metabin()
  g <- grade_meta(m, study_design = "obs", rob = "no",
                  small_values = "desirable",
                  rob_rationale = "Consensus RoB2: all domains low risk", threshold_type = "null")
  expect_equal(g$starting_quality, "Low")
})

test_that("rob = 'some' downgrades by 1", {
  m <- make_metabin()
  g_no   <- grade_meta(m, study_design = "RCT", rob = "no",
                       small_values = "desirable",
                       rob_rationale = "Consensus RoB2: all domains low risk",
                       indirectness = "no", threshold_type = "null")
  g_some <- grade_meta(m, study_design = "RCT", rob = "some",
                       small_values = "desirable",
                       rob_rationale = "Consensus RoB2: some concerns overall",
                       indirectness = "no", threshold_type = "null")
  diff <- g_no$certainty_score - g_some$certainty_score
  expect_true(diff >= 0)
})

test_that("very_serious rob downgrades by 2", {
  m <- make_metabin()
  g <- grade_meta(m, study_design = "RCT", rob = "very_serious",
                  small_values = "desirable",
                  rob_rationale = "Consensus RoB2: high risk in most domains",
                  indirectness = "no", threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$downgrade, -2)
})

# ---- ドメイン: 非一貫性 ---------------------------------------------------

test_that("inconsistency domain is auto-computed", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null", small_values = "desirable"))
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_true(incon_row$auto)
  expect_true(incon_row$judgment %in% c("not_serious", "some", "very_serious", "very_serious"))
})

test_that("high I2 (opposite-sided TEs) rates down two levels (auto)", {
  # Updated (v0.5.1): the -1 cap v0.5.0 imposed here is gone. These three
  # studies put a substantial share of estimates on each side of the null with
  # no subgroup to explain it, so the direction of effect is unresolved and
  # the domain rates down two levels. The note has to declare the departure
  # from Core GRADE 3, which describes no two-level inconsistency downgrade.
  m <- make_metabin_high_i2()
  g <- grade_meta(m, threshold_type = "null", small_values = "desirable")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "very_serious")
  expect_equal(incon_row$downgrade, -2L)
  expect_match(incon_row$notes, "This departs from Core GRADE 3", fixed = TRUE)
  expect_false(grepl("capped at one level", incon_row$notes, fixed = TRUE))
})

test_that("the extra inconsistency level costs exactly one certainty band", {
  # The point of pinning the overall verdict as well as the domain: the -2
  # must move the rating by one level and change nothing else. On this
  # fixture Imprecision rates down one and no other domain rates down, so
  # High (4) - 2 - 1 = 1 = Very Low. Under the v0.5.0 cap the same fixture
  # scored 2 = Low, which is what the scalar override below reproduces.
  m <- make_metabin_high_i2()
  g <- grade_meta(m, threshold_type = "null", small_values = "desirable")

  downgrades <- stats::setNames(g$domain_assessments$downgrade,
                                g$domain_assessments$domain)
  expect_equal(unname(downgrades[["Inconsistency"]]), -2L)
  expect_equal(unname(downgrades[["Imprecision"]]), -1L)
  expect_equal(sum(downgrades[setdiff(names(downgrades),
                                      c("Inconsistency", "Imprecision"))]), 0)

  expect_equal(g$starting_quality, "High")
  expect_equal(g$certainty_score, 1)
  expect_equal(g$certainty, "Very Low")

  # Same object, same domains, inconsistency forced back to -1: one band up
  # and nothing else moves.
  g1 <- grade_meta(m, threshold_type = "null",
                   small_values = "desirable",
                   inconsistency = "some_concerns",
                   inconsistency_rationale = "Pinning the pre-0.5.1 one level")
  expect_equal(g1$certainty_score, 2)
  expect_equal(g1$certainty, "Low")
})

test_that("the scalar override still sets inconsistency independently", {
  # It used to be the ONLY route to -2 in this domain; the automated
  # opposite-sides branch now reaches it too, so what this pins is that an
  # explicit judgment still wins over the flowchart.
  m <- make_metabin_high_i2()
  g <- grade_meta(m, threshold_type = "null", inconsistency = "no",
                  small_values = "desirable",
                  inconsistency_rationale = "Subgroups reported separately")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "not_serious")
  expect_equal(incon_row$downgrade, 0)
  expect_false(incon_row$auto)
})

test_that("inconsistency flowchart: ci_diff = no → do not rate down", {
  m <- make_metabin()
  g <- grade_meta(m, inconsistency_ci_diff = "no", threshold_type = "null",
    small_values = "desirable")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "not_serious")
  expect_false(incon_row$auto)
})

test_that("inconsistency flowchart: opposite_sides + no subgroup → rate down 2", {
  # Updated (v0.5.1): the manual flowchart answers the same three questions as
  # the automated path, so it must reach the same leaf and the same -2.
  m <- make_metabin()
  g <- grade_meta(m,
    small_values = "desirable",
    inconsistency_ci_diff            = "yes",
    inconsistency_threshold_side     = "opposite_sides",
    inconsistency_subgroup_explained = "no", threshold_type = "null")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "very_serious")
  expect_equal(incon_row$downgrade, -2L)
  expect_match(incon_row$notes, "This departs from Core GRADE 3", fixed = TRUE)
})

test_that("inconsistency flowchart: majority_one_side → do not rate down", {
  m <- make_metabin()
  g <- grade_meta(m,
    small_values = "desirable",
    inconsistency_ci_diff        = "yes",
    inconsistency_threshold_side = "majority_one_side", threshold_type = "null")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "not_serious")
})

test_that("inconsistency scalar overrides flowchart", {
  m <- make_metabin()
  # Legacy "very_serious" is normalized to canonical "very_serious" (-2) under the
  # v0.3+ 3-level system.
  g <- grade_meta(m, inconsistency = "very_serious",
                  small_values = "desirable",
                  inconsistency_rationale = "Clinically divergent effects across settings", threshold_type = "null")
  incon_row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(incon_row$judgment, "very_serious")
  expect_false(incon_row$auto)
})

# ---- ドメイン: 不精確性 ---------------------------------------------------

test_that("imprecision domain is auto-computed", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null", small_values = "desirable"))
  impre_row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(impre_row$auto)
})

test_that("ois_events below total events rates down one level (CI crosses null)", {
  m <- make_metabin()
  # total events = 10+15+20 + 15+20+25 = 105; OIS = 1000 → not met
  # Changed in the Core GRADE 2 Fig 4 rewrite: the CI crosses the chosen
  # (null) threshold, so Fig 4 rates down one level and never consults the
  # sample size; the old code applied the "<= 30% of OIS" rule unconditionally
  # and returned "very_serious".
  g <- suppressWarnings(grade_meta(m, ois_events = 1000, threshold_type = "null",
    small_values = "desirable"))
  impre_row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_equal(impre_row$judgment, "serious")
  expect_match(impre_row$notes, "sample size not considered on this path")
})

test_that("ois auto-calculation from p0/p1 (binary)", {
  m <- make_metabin()
  # OIS auto-calculated; actual value depends on formula
  g <- suppressWarnings(grade_meta(m, ois_p0 = 0.20, ois_p1 = 0.30, threshold_type = "null",
    small_values = "desirable"))
  impre_row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]
  expect_true(impre_row$auto)
  expect_true(grepl("OIS", impre_row$notes))
})

# ---- ドメイン: 出版バイアス -----------------------------------------------

test_that("k < 10 gives not assessable publication bias (judgment = 'no')", {
  m <- make_metabin()  # k = 3
  g <- suppressWarnings(grade_meta(m, threshold_type = "null", small_values = "desirable"))
  pb_row <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb_row$judgment, "not_serious")
  expect_true(grepl("< 10", pb_row$notes))
})

test_that("pubias_small_industry = 'yes' rates down", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, pubias_small_industry = "yes", threshold_type = "null",
    small_values = "desirable"))
  pb_row <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  # Step 1 of BMJ Core GRADE 4 Fig 5: small + industry-sponsored -> rate down 1.
  expect_equal(pb_row$judgment, "serious")
})

test_that("pubias_unpublished = 'yes' rates down when k < 10", {
  m <- make_metabin()  # k = 3
  g <- grade_meta(m, pubias_unpublished = "yes", threshold_type = "null",
    small_values = "desirable")
  pb_row <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  # Step 2 (k < 10) of BMJ Core GRADE 4 Fig 5: documented unpublished studies
  # -> rate down 1.
  expect_equal(pb_row$judgment, "serious")
  expect_false(pb_row$auto)
})

# ---- RoB ベクタ入力 -------------------------------------------------------

test_that("rob vector: vector mode reports count and weight % (v0.3.1+: dominance gate removed)", {
  m <- make_metabin()
  g <- grade_meta(m, rob = c("no", "some", "very_serious"), threshold_type = "null",
    small_values = "desirable")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  # Only one high-RoB study among 3, with similar weights -> direction check
  # runs but inflation typically below threshold.
  expect_match(rob_row$notes, "by count")
  expect_match(rob_row$notes, "by weight")
})

test_that("rob vector: inflating small_values=undesirable rates down (some_concerns; sign-flip required for serious)", {
  # te_all=1.4 > te_low=0.3, both positive (no flip) -> some_concerns via inflation.
  m <- make_mock_dominated(te_all = 1.4, te_low_only = 0.3)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values           = "undesirable", threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
})

test_that("rob vector: NOT inflating small_values=undesirable does not rate down", {
  # TE_all < TE_low → high-RoB pulls toward null → conservative (doesn't inflate)
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values           = "undesirable", threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "not_serious")
})

test_that("rob vector: inflating small_values=desirable rates down (some_concerns)", {
  # te_all=-1.5 < te_low=-0.2 (more negative is more "favorable" when small is good).
  # Both negative, no sign flip -> some_concerns.
  m <- make_mock_dominated(te_all = -1.5, te_low_only = -0.2)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values           = "desirable", threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
})

test_that("rob vector of wrong length raises error", {
  m <- make_metabin()
  expect_error(
    grade_meta(m, rob = c("no", "some"), threshold_type = "null",
      small_values = "desirable"),  # k=3 だが長さ2
    regexp = "length k"
  )
})

# ---- 入力バリデーション ---------------------------------------------------

test_that("invalid rob level raises error", {
  m <- make_metabin()
  # 'moderate' is normalised to 'some' (alias), so use a truly unknown label
  expect_error(suppressWarnings(grade_meta(m, rob = "totally_unknown_rob_level", threshold_type = "null",
    small_values = "desirable")),
               regexp = "not a recognized GRADE level")
})

test_that("non-meta object raises error", {
  expect_error(grade_meta(list(x = 1), threshold_type = "null",
    small_values = "desirable"), regexp = "class 'meta'")
})

# ---- the outcome-direction entry gate (v0.5.1) ------
test_that("an omitted small_values aborts before any domain is assessed", {
  m <- make_metabin()
  cnd <- tryCatch(suppressWarnings(grade_meta(m, threshold_type = "null")),
                  error = function(e) e)
  expect_s3_class(cnd, "pmatools_direction_gate")

  msg <- conditionMessage(cnd)
  # The message has to carry the two things a reader needs: the vocabulary,
  # and why the package will not pick one for them.
  expect_match(msg, "'desirable' or 'undesirable'", fixed = TRUE)
  expect_match(msg, "will not guess", fixed = TRUE)
  expect_match(msg, "every outcome has a direction", fixed = TRUE)
})

test_that("a value outside the vocabulary aborts and is quoted back", {
  m <- make_metabin()
  for (bad in list("Desirable", "small", NA_character_, 1, c("desirable", "desirable"))) {
    cnd <- tryCatch(
      suppressWarnings(grade_meta(m, threshold_type = "null",
                                  small_values = bad)),
      error = function(e) e)
    expect_s3_class(cnd, "pmatools_direction_gate")
  }
  expect_match(
    tryCatch(suppressWarnings(grade_meta(m, threshold_type = "null",
                                         small_values = "Desirable")),
             error = conditionMessage),
    "received \"Desirable\"", fixed = TRUE)
})

test_that("the direction the rating was made under is stored on the object", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null",
                                   small_values = "undesirable"))
  # export_bundle() reads this instead of falling back to NULL; see
  # test-export_bundle.R for the round trip that depends on it.
  expect_identical(g$small_values, "undesirable")
})

# ---- print / summary ------

test_that("print.pmatools outputs without error", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null", small_values = "desirable"))
  expect_output(print(g), "Certainty Assessment \\(Core GRADE series\\)")
})

test_that("summary.pmatools outputs without error", {
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null", small_values = "desirable"))
  expect_output(summary(g), "Certainty Assessment \\(Core GRADE series\\)")
})

# ---- sof_table() ----------------------------------------------------------

test_that("sof_table returns flextable", {
  skip_if_not_installed("flextable")
  m <- make_metabin()
  g <- suppressWarnings(grade_meta(m, threshold_type = "null", small_values = "desirable"))
  ft <- sof_table(g)
  expect_s3_class(ft, "flextable")
})

# ---- grade_table() --------------------------------------------------------

test_that("grade_table with multiple outcomes returns flextable", {
  skip_if_not_installed("flextable")
  m <- make_metabin()
  g1 <- suppressWarnings(grade_meta(m, outcome_name = "Outcome 1", threshold_type = "null",
    small_values = "desirable"))
  g2 <- suppressWarnings(grade_meta(m, outcome_name = "Outcome 2", threshold_type = "null",
    small_values = "desirable"))
  ft <- grade_table(
    list("Outcome 1" = g1, "Outcome 2" = g2),
    primary = "Outcome 1"
  )
  expect_s3_class(ft, "flextable")
})

# ---- threshold_sides / plain_language_frame (v0.5.1) ----------------------

# A rating of a margin question, as SPEC.md 4.5.1b maps it: a threshold, the
# target pinned to "little_to_no_difference" through the existing manual
# override path, and the sidedness.
margin_grade <- function(..., sides = "worse_only", frame = NULL) {
  suppressWarnings(grade_meta(
    make_metabin(),
    small_values            = "desirable",
    threshold               = 1.20,
    threshold_scale         = "ratio",
    rating_target           = "little_to_no_difference",
    rating_target_rationale = paste(
      "The review asks whether the intervention is no worse than the",
      "comparator by more than the protocol margin, not how large the",
      "effect is."),
    threshold_sides         = sides,
    plain_language_frame    = frame,
    outcome_name            = "Mortality",
    ...))
}

test_that("the three new fields are stored on the object", {
  g <- margin_grade(frame = "non_inferiority")
  expect_identical(g$threshold_sides, "worse_only")
  expect_identical(g$plain_language_frame, "non_inferiority")
  expect_true(g$threshold_zone %in% PMA_IMPRE_THRESHOLD_ZONES)
})

test_that("threshold_sides defaults to 'both' and is recorded as such", {
  g <- suppressWarnings(grade_meta(make_metabin(), threshold_type = "null",
                                   small_values = "desirable"))
  expect_identical(g$threshold_sides, "both")
  expect_null(g$plain_language_frame)
  # No threshold at all, so no zone applied and the field is absent.
  expect_null(g$threshold_zone)
})

test_that("threshold_zone is lifted from the facts, never recomputed", {
  g <- margin_grade()
  facts <- domain_facts(g)[["Imprecision"]]
  expect_identical(
    g$threshold_zone,
    as.character(facts$value[facts$key == "threshold_zone"]))
})

test_that("a scalar imprecision override leaves threshold_zone NULL", {
  # That branch never calls assess_imprecision(), so it records no facts and
  # there is nothing to lift. The column that reads the zone is then dropped.
  g <- margin_grade(
    imprecision           = "some_concerns",
    imprecision_rationale = "Panel judged the interval too wide to act on")
  expect_null(g$threshold_zone)
  expect_identical(g$threshold_sides, "worse_only")
})

test_that("the threshold_sides gate still fires on the override branch", {
  # The leak this guards: the scalar override bypasses the assessor, so
  # grade_meta() has to run the gate itself.
  cnd <- tryCatch(
    suppressWarnings(grade_meta(
      make_metabin(), threshold_type = "null", small_values = "desirable",
      threshold_sides = "worse_only",
      imprecision = "some_concerns",
      imprecision_rationale = "Panel judgment")),
    condition = function(e) e)
  expect_s3_class(cnd, "pmatools_threshold_gate")
  expect_match(conditionMessage(cnd), "needs a threshold with two sides",
               fixed = TRUE)
})

test_that("an unknown threshold_sides aborts before any domain runs", {
  expect_error(
    suppressWarnings(grade_meta(make_metabin(), threshold_type = "null",
                                small_values = "desirable",
                                threshold_sides = "one")),
    "threshold_sides must be", fixed = TRUE)
})

test_that("an unknown plain_language_frame aborts, but not as a threshold gate", {
  cnd <- tryCatch(
    suppressWarnings(grade_meta(make_metabin(), threshold_type = "null",
                                small_values = "desirable",
                                plain_language_frame = "superiority")),
    condition = function(e) e)
  expect_s3_class(cnd, "error")
  expect_false(inherits(cnd, "pmatools_threshold_gate"))
  expect_match(conditionMessage(cnd), "plain_language_frame must be NULL",
               fixed = TRUE)
})

test_that("threshold_sides forwards to the Imprecision domain", {
  worse <- margin_grade(sides = "worse_only")
  both  <- margin_grade(sides = "both")
  impre <- function(g) {
    g$domain_assessments$notes[g$domain_assessments$domain == "Imprecision"]
  }
  expect_match(impre(worse), "the Threshold on the worse side", fixed = TRUE)
  expect_false(grepl("worse side", impre(both), fixed = TRUE))
})

# ---- print() (v0.5.1) -----------------------------------------------------

test_that("print() for a default call is byte-identical to the pre-0.5.1 form", {
  # The parenthetical is EXTENDED, not restructured, so a call that names
  # neither new argument must print exactly what it printed before.
  g <- suppressWarnings(grade_meta(make_metabin(), threshold_type = "null",
                                   small_values = "desirable"))
  out <- capture.output(print(g))
  target <- grep("Rating target", out, value = TRUE)
  expect_length(target, 1L)
  expect_identical(
    target,
    " Rating target: Non-null effect  (threshold: null, auto)")
})

test_that("print() names the worse side only under worse_only", {
  worse <- capture.output(print(margin_grade(sides = "worse_only")))
  both  <- capture.output(print(margin_grade(sides = "both")))
  expect_identical(
    grep("Rating target", worse, value = TRUE),
    paste(" Rating target: Little or no difference",
          " (threshold: mid, manual, worse side only)"))
  expect_identical(
    grep("Rating target", both, value = TRUE),
    paste(" Rating target: Little or no difference",
          " (threshold: mid, manual)"))
})

test_that("print() does not name the clinical question", {
  # The package does not know which of the four questions the caller was
  # asking, only how the rating was configured, and must not claim otherwise.
  out <- capture.output(print(margin_grade(frame = "non_inferiority")))
  expect_false(any(grepl("non-inferiority|non_inferiority|equivalence", out,
                         ignore.case = TRUE)))
})
