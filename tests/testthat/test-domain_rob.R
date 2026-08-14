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
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "not_serious")
  expect_match(rob_row$notes, "Rule 1")
})

# --- Rule 2: same non-trivial zone, inflation within threshold ---------------
test_that("Rule 2: same non-trivial zone, inflation <= 10% -> no", {
  # Both te_all=0.50 and te_low=0.48 are above +log(1.20)=0.182 (zone 'above').
  # inflation = (0.50 - 0.48) / 0.48 = 4.2% < 10%.
  m <- make_mock_dominated(te_all = 0.50, te_low_only = 0.48)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "not_serious")
  expect_match(rob_row$notes, "Rule 2")
})

test_that("Rule 2: same non-trivial zone, deflating direction -> no", {
  # te_all=0.40 < te_low=0.60: high-RoB pulls *toward* null; not bias-favouring.
  # Both still in 'above' zone (above +0.182). -> Rule 2.
  m <- make_mock_dominated(te_all = 0.40, te_low_only = 0.60)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "not_serious")
  expect_match(rob_row$notes, "Rule 2")
})

# --- Rule 3: same non-trivial zone, bias-favouring inflation > threshold -----
test_that("Rule 3: same non-trivial zone, inflation > 10% -> some_concerns", {
  # Both te_all=0.60 and te_low=0.40 in 'above' zone; inflation = 50% > 10%.
  m <- make_mock_dominated(te_all = 0.60, te_low_only = 0.40)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Rule 3")
})

# --- Rule 4: zone changes without sign flip ----------------------------------
test_that("Rule 4: 'above' -> 'trivial' zone change -> some_concerns", {
  # te_all=0.50 (above), te_low=0.10 (trivial). Zones differ; no sign flip.
  m <- make_mock_dominated(te_all = 0.50, te_low_only = 0.10)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Rule 4")
})

test_that("Rule 4: 'trivial' -> 'above' zone change -> some_concerns", {
  # high-RoB pulls into trivial: te_all=0.10 (trivial), te_low=0.50 (above).
  m <- make_mock_dominated(te_all = 0.10, te_low_only = 0.50)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Rule 4")
})

# --- Rule 5: zone change with sign flip --------------------------------------
# Updated (v0.5): rule 5 now caps at -1. Core GRADE 4 describes no automatic
# two-level risk-of-bias downgrade (every Fig 2 leaf is "rate down" / "do not
# rate down"); -2 requires the scalar rob override.
test_that("Rule 5: 'above' <-> 'below' sign flip -> some_concerns (capped at -1)", {
  m <- make_mock_dominated(te_all = 0.50, te_low_only = -0.50)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_equal(rob_row$downgrade, -1L)
  expect_match(rob_row$notes, "Rule 5")
  expect_match(rob_row$notes, "capped at one level", fixed = TRUE)
})

test_that("Rule 5's own wording says one level, not two", {
  # Three documents claimed "rate down 2" for rule 5 long after the code
  # stopped doing it (SPEC.md's rule table, the block comment duplicating that
  # table in R/domain_rob.R, and the Shiny app's "How is this judged?" copy).
  # This pins the sentence the code itself emits, so a doc that drifts back can
  # be caught against something executable.
  m <- make_mock_dominated(te_all = 0.50, te_low_only = -0.50)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob_row$notes,
               "Rule 5: zone changes across null (benefit <-> harm) -> rate down 1",
               fixed = TRUE)
  expect_false(grepl("rate down 2", rob_row$notes, fixed = TRUE))
  expect_true(rob_row$downgrade >= -1L)
})

test_that("Rule 5 can still reach -2 through the scalar rob override", {
  m <- make_mock_dominated(te_all = 0.50, te_low_only = -0.50)
  g <- grade_meta(m, rob = "very_serious",
                  rob_rationale = "Sign flip when high-RoB studies are removed",
                  threshold = 1.20, threshold_scale = "ratio")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "very_serious")
  expect_equal(rob_row$downgrade, -2L)
})

# --- Fallback: Threshold not supplied ---------------------------------------
# Updated (v0.5): same -1 cap as above.
test_that("Fallback: Threshold not supplied + sign flip -> some_concerns (rule 5)", {
  m <- make_mock_dominated(te_all = 1.0, te_low_only = -0.5)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10, threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Threshold not supplied")
})

test_that("Fallback: Threshold not supplied + same-sign small inflation -> no (rule 2)", {
  # Without Threshold, trivial zone collapses to {0}; both 0.05 and 0.04 are 'above'.
  # te_all < te_low under small_values='undesirable' -> direction_ok FALSE -> rule 2.
  m <- make_mock_dominated(te_all = 0.04, te_low_only = 0.05)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  rob_inflation_threshold = 0.10, threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "not_serious")
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
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "not_serious")
  expect_match(rob_row$notes, "Rule 2")
  expect_match(rob_row$notes, "direction gate \\(bias-favouring shift\\): no")
  expect_match(rob_row$notes, "exceeding the 10% threshold", fixed = TRUE)
  expect_match(rob_row$notes, "small_values = 'undesirable'", fixed = TRUE)
  expect_match(rob_row$notes, "no downgrade for this criterion", fixed = TRUE)
})

test_that("Direction gate result is always reported in notes", {
  # Bias-favouring case (rule 3): gate = yes.
  m <- make_mock_dominated(te_all = 0.60, te_low_only = 0.40)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
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
    g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                    small_values = NULL,
                    threshold = 1.20, threshold_scale = "ratio",
                    rob_inflation_threshold = 0.10),
    regexp = "small_values"
  )
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Rule 3")
})

test_that("small_values = NULL: no warning when the gate is not decisive", {
  # Deflating direction (ratio negative): no downgrade regardless of
  # small_values, so no warning should be emitted.
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  expect_no_warning(
    grade_meta(m, rob = c("very_serious", "no", "no"),
               small_values = NULL,
               rob_inflation_threshold = 0.10, threshold_type = "null"),
    message = "small_values"
  )
})

# --- small_values direction handling ----------------------------------------
test_that("small_values = NULL: high-RoB toward null does NOT rate down", {
  # |te_all|=0.2 < |te_low|=1.1 -> direction_ok FALSE; both 'above' (no Threshold).
  m <- make_mock_dominated(te_all = 0.2, te_low_only = 1.1)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = NULL,
                  rob_inflation_threshold = 0.10, threshold_type = "null")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "not_serious")
})

# --- Threshold = 0 backward compatibility -----------------------------------
test_that("Threshold = 0 inside same non-trivial zone rates down for any inflation", {
  # te_all=0.20 (above), te_low=0.19 (above); 5% inflation; threshold 0 -> rule 3.
  m <- make_mock_dominated(te_all = 0.20, te_low_only = 0.19)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_match(rob_row$notes, "Rule 3")
})

# --- Reporting ---------------------------------------------------------------
test_that("weight_note reports both count % and weight %", {
  m <- make_mock_dominated(te_all = 0.05, te_low_only = 0.05)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio")
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob_row$notes, "by count")
  expect_match(rob_row$notes, "by weight")
})

test_that("All studies high-RoB -> some_concerns (1 level down, no comparator pool)", {
  # Updated (v0.5): this used to rate down 2 levels. Core GRADE 4 supports no
  # automatic two-level risk-of-bias downgrade, so the automated judgment is
  # capped at -1; -2 requires rob = "very_serious" + rob_rationale.
  m <- make_mock_dominated(te_all = 0.30, te_low_only = 0.30)
  g <- grade_meta(m, rob = c("very_serious", "very_serious", "very_serious"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_row$judgment, "serious")
  expect_equal(rob_row$downgrade, -1L)
  expect_match(rob_row$notes, "All studies high-RoB", fixed = TRUE)
  expect_match(rob_row$notes, "capped at one level", fixed = TRUE)
})

test_that("diff_note reports zone labels and relative inflation", {
  m <- make_mock_dominated(te_all = 0.60, te_low_only = 0.40)
  g <- grade_meta(m, rob = c("very_serious", "no", "no"),
                  small_values = "undesirable",
                  threshold = 1.20, threshold_scale = "ratio",
                  rob_inflation_threshold = 0.10)
  rob_row <- g$domain_assessments[g$domain_assessments$domain == "Risk of bias", ]
  expect_match(rob_row$notes, "\\[zone = above\\]")
  expect_match(rob_row$notes, "relative inflation")
  expect_false(grepl("\\|TE_all\\|", rob_row$notes))
  expect_false(grepl("CI overlap", rob_row$notes))
})

# --- k-space <-> studlab-space alignment --------------------------------------
# .rob_studlab_index() is the single mapping between the studies {meta} pools
# ($k) and the original data rows ($studlab). It never guesses: when no rule
# reproduces exactly n rows it returns NULL and its callers keep their
# pre-existing abort/skip behaviour.

make_mock_gap <- function(te = c(0.60, NA, 0.02, 0.02),
                          studlab = c("Large-A", "Gap-B", "Small-C", "Small-D")) {
  m <- list(
    k           = sum(!is.na(te)),
    TE          = te,
    seTE        = ifelse(is.na(te), NA_real_, c(0.10, NA, 0.45, 0.45)),
    w.random    = ifelse(is.na(te), NA_real_, c(80, NA, 10, 10)),
    random      = TRUE,
    TE.random   = 0.50,
    seTE.random = 0.10,
    sm          = "RR",
    studlab     = studlab
  )
  class(m) <- "meta"
  m
}

test_that(".rob_studlab_index maps k onto the estimable studlab positions", {
  m <- make_mock_gap()
  expect_equal(pmatools:::.rob_studlab_index(m, 3L), c(1L, 3L, 4L))
  # k == length(studlab): the identity.
  expect_equal(pmatools:::.rob_studlab_index(m, 4L), 1:4)
  # No rule reproduces n rows, or there is nothing to map onto.
  expect_null(pmatools:::.rob_studlab_index(m, 2L))
  m_nostud <- m; m_nostud$studlab <- NULL
  expect_null(pmatools:::.rob_studlab_index(m_nostud, 3L))
  # $TE that is not itself in studlab space cannot anchor the alignment.
  m_short <- m; m_short$TE <- c(0.60, 0.02, 0.02)
  expect_null(pmatools:::.rob_studlab_index(m_short, 3L))
})

test_that(".rob_alignment carries the mapping and both lengths", {
  m  <- make_mock_gap()
  al <- pmatools:::.rob_alignment(m, m$k)
  expect_equal(al$idx, c(1L, 3L, 4L))
  expect_equal(al$k, 3L)
  expect_equal(al$n_slab, 4L)
  expect_equal(al$studlab, m$studlab)

  # Coincident spaces: the mapping is the identity and both lengths agree.
  m_full  <- make_mock_gap(te = c(0.60, 0.30, 0.02, 0.02))
  al_full <- pmatools:::.rob_alignment(m_full, m_full$k)
  expect_equal(al_full$idx, 1:4)
  expect_equal(al_full$k, al_full$n_slab)

  # Unresolvable: $TE is already in k-space, so nothing maps 3 onto 4 rows.
  m_bad     <- make_mock_gap()
  m_bad$TE  <- c(0.60, 0.02, 0.02)
  al_bad    <- pmatools:::.rob_alignment(m_bad, 3L)
  expect_null(al_bad$idx)
  expect_equal(al_bad$n_slab, 4L)
})

test_that(".rob_expand / .rob_contract move a vector between the two spaces", {
  al      <- pmatools:::.rob_alignment(make_mock_gap(), 3L)
  al_full <- pmatools:::.rob_alignment(
    make_mock_gap(te = c(0.60, 0.30, 0.02, 0.02)), 4L)
  rob_k   <- c("very_serious", "no", "no")

  # k-space -> studlab space: NA padding on the row {meta} could not pool.
  expect_equal(pmatools:::.rob_expand(rob_k, al),
               c("very_serious", NA, "no", "no"))
  # ... and back again (round trip is the identity on k-space vectors).
  expect_equal(pmatools:::.rob_contract(pmatools:::.rob_expand(rob_k, al), al),
               rob_k)
  # studlab space -> k-space drops the unpooled row.
  expect_equal(
    pmatools:::.rob_contract(c("very_serious", "no", "no", "serious"), al),
    c("very_serious", "no", "serious"))

  # A ready-made studlab-space `fill` supplies the unpooled rows: that is how
  # "high_idx" keeps a dropped study high only when the caller judged it so.
  expect_equal(
    pmatools:::.rob_expand(c(TRUE, FALSE, FALSE), al,
                           fill = c(FALSE, TRUE, FALSE, FALSE)),
    c(TRUE, TRUE, FALSE, FALSE))

  # Coincident spaces: both directions are the identity.
  expect_equal(pmatools:::.rob_expand(letters[1:4], al_full), letters[1:4])
  expect_equal(pmatools:::.rob_contract(letters[1:4], al_full), letters[1:4])

  # Unresolvable, or a vector that is in neither space: NULL, so the caller
  # keeps its own abort/skip path instead of guessing.
  m_bad    <- make_mock_gap()
  m_bad$TE <- c(0.60, 0.02, 0.02)
  al_bad   <- pmatools:::.rob_alignment(m_bad, 3L)
  expect_null(pmatools:::.rob_expand(rob_k, al_bad))
  expect_null(pmatools:::.rob_contract(c("no", "no", "no", "no"), al_bad))
  expect_null(pmatools:::.rob_expand(c("no", "no"), al))
  expect_null(pmatools:::.rob_contract(c("no", "no"), al))
})

test_that("an unresolvable alignment skips the refit instead of mis-subsetting", {
  # $TE is in k-space here, so no rule maps the 3 pooled studies onto the 4
  # study labels; high_idx stays in k-space and .refit_low_rob() must refuse.
  m <- make_mock_gap()
  m$TE       <- c(0.60, 0.02, 0.02)
  m$seTE     <- c(0.10, 0.45, 0.45)
  m$w.random <- c(80, 10, 10)
  m$k        <- 3L
  d <- assess_rob(c("very_serious", "no", "no"), m, small_values = "undesirable",
                  threshold_internal = log(1.20))
  expect_equal(length(attr(d, "high_idx")), 3L)
  expect_warning(
    res <- pmatools:::.refit_low_rob(m, attr(d, "high_idx")),
    regexp = "does not align with the meta object"
  )
  expect_false(res$refit)
  expect_identical(res$meta, m)
})

test_that("a studlab-length rob aborts when the alignment is unresolvable", {
  m <- make_mock_gap()
  m$TE       <- c(0.60, 0.02, 0.02)
  m$seTE     <- c(0.10, 0.45, 0.45)
  m$w.random <- c(80, 10, 10)
  m$k        <- 3L
  expect_error(
    assess_rob(c("very_serious", "no", "no", "no"), m,
               threshold_internal = log(1.20)),
    regexp = "estimable rows could not be identified"
  )
})

# --- The default inflation threshold -----------------------------------------
# Every test above passes rob_inflation_threshold explicitly, so none of them
# would notice the default moving. These two pin it down: a 15% relative
# inflation sits between the old default (0.10) and the current one (0.20), so
# the verdict flips on the default alone.
test_that("the default inflation threshold is PMA_ROB_INFLATION_THRESHOLD = 0.20", {
  expect_equal(pmatools:::PMA_ROB_INFLATION_THRESHOLD, 0.20)
  expect_equal(formals(pmatools:::assess_rob)$rob_inflation_threshold,
               quote(PMA_ROB_INFLATION_THRESHOLD))
  expect_equal(formals(pmatools::grade_meta)$rob_inflation_threshold,
               quote(PMA_ROB_INFLATION_THRESHOLD))
})

test_that("a 15% inflation does not rate down under the default threshold", {
  # Both te_all = 0.46 and te_low = 0.40 are above +log(1.20) = 0.182, so the
  # zones agree; inflation = (0.46 - 0.40) / 0.40 = 15%.
  m <- make_mock_dominated(te_all = 0.46, te_low_only = 0.40)

  g_default <- grade_meta(m, rob = c("very_serious", "no", "no"),
                          small_values = "undesirable",
                          threshold = 1.20, threshold_scale = "ratio")
  rob_default <- g_default$domain_assessments[
    g_default$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_default$judgment, "not_serious")
  expect_match(rob_default$notes, "Rule 2")

  g_old <- grade_meta(m, rob = c("very_serious", "no", "no"),
                      small_values = "undesirable",
                      threshold = 1.20, threshold_scale = "ratio",
                      rob_inflation_threshold = 0.10)
  rob_old <- g_old$domain_assessments[
    g_old$domain_assessments$domain == "Risk of bias", ]
  expect_equal(rob_old$judgment, "serious")
  expect_match(rob_old$notes, "Rule 3")
})
