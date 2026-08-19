# What Step 3 does differently when it is rating a rare-event analysis
# (shiny/SPEC.md 3.4.14, R/rare_step3.R and the three domain assessors).
#
# THE PROPERTY EVERY TEST HERE IS REALLY ABOUT: rare mode changes what is
# computed and what is said, and never changes a rating by itself. So the last
# block is the load-bearing one - it asserts that switching the flag on
# ordinary data moves no domain judgment at all - and each block above it says
# which computation changed and why the judgment did not follow it.

library(testthat)
library(meta)

skip_if_not_installed("meta")

# Sparse binary data: 14 studies at event rates well under 1%, two of them with
# a zero-event arm.
#
# Twelve of the fourteen have events in BOTH arms on purpose. `.pubias_effective_k()`
# counts studies with a usable estimate, and a zero-event arm with no continuity
# correction has none - so a fixture built out of zero-arm studies would sit
# below Fig 5's k >= 10 gate and would prove nothing about the publication-bias
# change. The whole point of that change is that k is no longer what answers Q2.
sparse_meta <- function() {
  metabin(
    event.e = c(2, 3, 1, 4, 2, 3, 1, 2, 4, 3, 2, 1, 0, 0),
    n.e     = rep(4000, 14),
    event.c = c(5, 6, 4, 8, 5, 7, 3, 5, 9, 6, 4, 3, 2, 3),
    n.c     = rep(4000, 14),
    studlab = paste("Sparse", 1:14),
    sm      = "OR",
    method  = "MH",
    incr    = 0,
    method.incr = "only0",
    MH.exact = TRUE,
    allstudies = TRUE,
    random  = TRUE,
    common  = FALSE,
    warn    = FALSE
  )
}

# Ordinary data, event rates around 25%, k = 12. Nothing about it is rare; it
# is the control the last block rates twice.
common_meta <- function() {
  metabin(
    event.e = c(20, 18, 25, 22, 30, 15, 19, 24, 28, 21, 17, 26),
    n.e     = c(100, 90, 120, 110, 140, 80, 95, 115, 130, 105, 88, 125),
    event.c = c(28, 25, 34, 30, 41, 21, 27, 33, 39, 29, 24, 36),
    n.c     = c(100, 90, 120, 110, 140, 80, 95, 115, 130, 105, 88, 125),
    studlab = paste("Common", 1:12),
    sm      = "OR",
    method  = "MH",
    random  = TRUE,
    common  = FALSE,
    warn    = FALSE
  )
}

# One arm with no events at all, anywhere.
one_arm_zero_meta <- function() {
  metabin(
    event.e = c(0, 0, 0, 0, 0),
    n.e     = c(500, 400, 600, 450, 520),
    event.c = c(2, 1, 3, 1, 2),
    n.c     = c(500, 400, 600, 450, 520),
    studlab = paste("Zero", 1:5),
    sm      = "OR",
    method  = "MH",
    incr    = 0,
    method.incr = "only0",
    MH.exact = TRUE,
    allstudies = TRUE,
    random  = FALSE,
    common  = TRUE,
    warn    = FALSE
  )
}

# A minimal pma_rare_meta, built by hand rather than fitted. The suite's
# CONTENT is what these tests are about, and fitting one takes an mmeta sampler
# and several seconds; the shape is small enough to state exactly, which also
# lets a disagreement be constructed rather than hunted for.
fake_suite <- function(estimates, effect_scale = "OR", primary = "BB_CR") {
  tab <- do.call(rbind, lapply(names(estimates), function(id) {
    e <- estimates[[id]]
    data.frame(
      role        = if (identical(id, primary)) "Primary" else "Sensitivity",
      method_id   = id,
      label       = paste("Method", id),
      estimate    = e[[1]],
      ci_low      = e[[2]],
      ci_high     = e[[3]],
      stringsAsFactors = FALSE
    )
  }))
  structure(
    list(primary_method = primary,
         method_table   = tibble::as_tibble(tab),
         effect_scale   = effect_scale),
    class = "pma_rare_meta"
  )
}

# --------------------------------------------------------------------------
# 1. The optimal information size switches to an event basis
# --------------------------------------------------------------------------
# An OIS in participants is the wrong denominator when the events are what is
# scarce: at these rates a "sufficiently large" participant count carries a
# dozen events. The rule Fig 4 applies to the percentage is unchanged; the
# quantity the percentage is OF is not.

test_that(".calc_ois returns participants by default and events under event_basis", {
  n_basis <- .calc_ois("relative", 0.05, 0.20, ois_p0 = 0.005, ois_p1 = 0.004,
                       ois_delta = NULL, ois_sd = NULL)
  e_basis <- .calc_ois("relative", 0.05, 0.20, ois_p0 = 0.005, ois_p1 = 0.004,
                       ois_delta = NULL, ois_sd = NULL, event_basis = TRUE)

  expect_equal(n_basis$type, "n")
  expect_equal(e_basis$type, "events")
  # Same power calculation, so the event target is the participant target times
  # the mean of the two rates. That is the whole difference.
  expect_equal(e_basis$value, ceiling(n_basis$value * (0.005 + 0.004) / 2),
               tolerance = 1)
  expect_lt(e_basis$value, n_basis$value)
  expect_match(e_basis$formula, "EVENT basis", fixed = TRUE)
  expect_match(n_basis$formula, "target N=", fixed = TRUE)
})

test_that("event_basis is binary-only: the continuous OIS has no events to switch to", {
  cont <- .calc_ois("absolute", 0.05, 0.20, ois_p0 = NULL, ois_p1 = NULL,
                    ois_delta = 0.2, ois_sd = 1, event_basis = TRUE)
  expect_equal(cont$type, "n")
})

test_that("rare_flow puts the OIS comparison on events and says which basis it used", {
  m <- sparse_meta()
  regular <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable"))
  rare <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable", rare_flow = TRUE))

  basis <- function(g) {
    f <- domain_facts(g, "Imprecision")
    f$value[f$key == "ois_basis"]
  }
  expect_match(basis(regular), "participants", fixed = TRUE)
  expect_match(basis(rare), "total events", fixed = TRUE)

  # And the basis is named in the prose too, because the notes are what the
  # exported record carries.
  rare_note <- rare$domain_assessments$notes[
    rare$domain_assessments$domain == "Imprecision"]
  expect_match(rare_note, "EVENT basis", fixed = TRUE)
  expect_match(rare_note, "events", fixed = TRUE)
})

test_that("an explicit ois_n override puts the basis back on participants", {
  # The fact has to describe what was actually compared, not what rare_flow
  # would have chosen: a reviewer who typed a participant target gets one.
  g <- suppressWarnings(grade_meta(
    sparse_meta(), threshold_type = "null", small_values = "desirable",
    rare_flow = TRUE, ois_n = 5000))
  f <- domain_facts(g, "Imprecision")
  expect_match(f$value[f$key == "ois_basis"], "participants", fixed = TRUE)
})

# --------------------------------------------------------------------------
# 2. One arm with no events at all: imprecision is not assessable
# --------------------------------------------------------------------------

test_that("one_arm_total_zero reports imprecision as not assessable, in those words", {
  g <- suppressWarnings(grade_meta(
    one_arm_zero_meta(), threshold_type = "null", small_values = "desirable",
    rare_flow = TRUE, rare_one_arm_total_zero = TRUE))
  row <- g$domain_assessments[g$domain_assessments$domain == "Imprecision", ]

  expect_match(row$notes, "IMPRECISION NOT ASSESSABLE", fixed = TRUE)
  # No automatic downgrade: a rating derived from a computation that did not
  # happen is exactly what the governing rule forbids.
  expect_equal(row$downgrade, 0L)
  f <- domain_facts(g, "Imprecision")
  expect_match(f$value[f$key == "imprecision_assessable"], "^no")
})

test_that("the diagnostics agree with the flags the app would pass", {
  # The app reads both flags off rare_event_diagnostics(); this holds the
  # fixtures and the detector in step, so the tests that pass them by hand are
  # still testing the cases the app produces.
  d <- rare_event_diagnostics(one_arm_zero_meta())
  expect_true(d$one_arm_total_zero)
  expect_true(d$rare_flow)

  s <- rare_event_diagnostics(sparse_meta())
  expect_true(s$rare_flow)
  expect_false(s$one_arm_total_zero)
  expect_lt(s$event_rate_overall, 0.01)

  expect_false(rare_event_diagnostics(common_meta())$rare_flow)
})

# --------------------------------------------------------------------------
# 3. The suite as a sensitivity analysis for the RATING
# --------------------------------------------------------------------------

test_that("every method is asked the primary's question and unanimity is reported", {
  # All four intervals straddle OR 1, so all four cross a null threshold.
  suite <- fake_suite(list(
    BB_CR    = c(0.62, 0.30, 1.28),
    MH_no_cc = c(0.60, 0.28, 1.29),
    GLMM     = c(0.64, 0.31, 1.32),
    Peto     = c(0.59, 0.27, 1.30)
  ))
  cross <- rare_suite_crossing(suite, threshold_internal = 0)

  expect_equal(cross$k_methods, 4L)
  expect_true(cross$primary)
  expect_true(cross$unanimous)
  expect_length(cross$disagree, 0L)

  note <- rare_suite_crossing_note(cross, 0)
  expect_match(note, "Every other fitted method gives the same answer",
               fixed = TRUE)
  expect_match(note, "does not rest on the choice of method", fixed = TRUE)
})

test_that("a disagreeing method is named, with its interval", {
  # Peto's interval clears the null; the other three straddle it. That IS the
  # case worth having: the imprecision judgment then rests on a method choice.
  suite <- fake_suite(list(
    BB_CR    = c(0.62, 0.30, 1.28),
    MH_no_cc = c(0.60, 0.28, 1.29),
    Peto     = c(0.55, 0.32, 0.95)
  ))
  cross <- rare_suite_crossing(suite, threshold_internal = 0)

  expect_true(cross$primary)
  expect_false(cross$unanimous)
  expect_equal(cross$disagree, "Peto")

  note <- rare_suite_crossing_note(cross, 0)
  expect_match(note, "1 fitted method disagrees", fixed = TRUE)
  expect_match(note, "rests on the choice of method", fixed = TRUE)
  # Named AND quantified: "which methods disagree and by how much".
  expect_match(note, "Method Peto", fixed = TRUE)
  expect_match(note, "0.95", fixed = TRUE)
})

test_that("the crossing question uses the +/- band when the threshold is not the null", {
  # threshold_internal = log(1.25) = 0.223. An interval of 0.90 to 1.10 lies
  # inside the band and crosses neither side; 0.90 to 1.40 crosses the upper.
  thr <- log(1.25)
  inside  <- fake_suite(list(BB_CR = c(1.00, 0.90, 1.10)))
  outside <- fake_suite(list(BB_CR = c(1.10, 0.90, 1.40)))

  expect_false(rare_suite_crossing(inside,  thr)$primary)
  expect_true(rare_suite_crossing(outside, thr)$primary)
})

test_that("a method that produced no usable interval is dropped, not counted", {
  suite <- fake_suite(list(
    BB_CR    = c(0.62, 0.30, 1.28),
    MH_no_cc = c(NA_real_, NA_real_, NA_real_)
  ))
  cross <- rare_suite_crossing(suite, 0)
  expect_equal(cross$k_methods, 1L)
  # One answer is not a consensus, and must not be reported as one.
  expect_true(is.na(cross$unanimous))
  expect_match(rare_suite_crossing_note(cross, 0),
               "nothing to compare it with", fixed = TRUE)
})

test_that("the crossing question matches the threshold the rating used", {
  # A non-null-effect target rates imprecision against the null even though a
  # threshold exists, so the suite must be asked against the null too - or the
  # sensitivity would answer a question the primary was never asked.
  g <- suppressWarnings(grade_meta(
    common_meta(), threshold = 1.05, threshold_scale = "ratio",
    threshold_type = "null", small_values = "desirable"))
  expect_equal(g$rating_target, "non_null_effect")
  expect_equal(.rated_threshold_for_imprecision(g), 0)

  g2 <- suppressWarnings(grade_meta(
    common_meta(), threshold = 1.25, threshold_scale = "ratio",
    small_values = "desirable"))
  expect_equal(.rated_threshold_for_imprecision(g2), g2$threshold_internal)
})

# --------------------------------------------------------------------------
# 4. Inconsistency: the I-squared proxy is withdrawn, not reinterpreted
# --------------------------------------------------------------------------

test_that("rare_flow withdraws the I2 surrogate and reports no number", {
  m <- sparse_meta()
  regular <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable"))
  rare <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable", rare_flow = TRUE))

  regular_keys <- domain_facts(regular, "Inconsistency")$key
  rare_keys    <- domain_facts(rare, "Inconsistency")$key
  expect_true("i2" %in% regular_keys)
  # Not "reported with a caveat": the caveat would be read past and the number
  # would not.
  expect_false("i2" %in% rare_keys)
  expect_false("tau2" %in% rare_keys)
  expect_false("q_pvalue" %in% rare_keys)
  expect_true("i2_assessable" %in% rare_keys)

  row <- rare$domain_assessments[
    rare$domain_assessments$domain == "Inconsistency", ]
  expect_match(row$notes, "NOT ASSESSABLE BY THE AUTOMATED PATH", fixed = TRUE)
  expect_false(grepl("I2 = ", row$notes, fixed = TRUE))
  # Withdrawing an unusable statistic cannot be grounds for a downgrade.
  expect_equal(row$downgrade, 0L)
})

test_that("the manual inconsistency flowchart is unchanged under rare_flow", {
  m <- sparse_meta()
  manual <- function(rare) {
    suppressWarnings(grade_meta(
      m, threshold_type = "null", small_values = "desirable",
      rare_flow = rare,
      inconsistency_ci_diff = "yes",
      inconsistency_threshold_side = "opposite_sides",
      inconsistency_subgroup_explained = "no"))
  }
  a <- manual(FALSE)$domain_assessments
  b <- manual(TRUE)$domain_assessments
  pick <- function(d) d[d$domain == "Inconsistency", c("judgment", "downgrade")]
  expect_equal(pick(a), pick(b))
  expect_equal(pick(b)$judgment, "very_serious")
})

test_that("a scalar inconsistency override still wins under rare_flow", {
  g <- suppressWarnings(grade_meta(
    sparse_meta(), threshold_type = "null", small_values = "desirable",
    rare_flow = TRUE,
    inconsistency = "some_concerns",
    inconsistency_rationale = "Forest plot shows two clearly opposed clusters."))
  row <- g$domain_assessments[g$domain_assessments$domain == "Inconsistency", ]
  expect_equal(row$judgment, "serious")
  expect_false(row$auto)
})

# --------------------------------------------------------------------------
# 5. Publication bias: the k < 10 route, whatever k is
# --------------------------------------------------------------------------

test_that("rare_flow takes Fig 5's k < 10 branch at k >= 10", {
  m <- sparse_meta()
  k <- .pubias_effective_k(m)
  # The premise of the whole test: without it the branch below would be the one
  # k chose anyway, and the assertion would pass for the wrong reason.
  expect_gte(k, 10)

  regular <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable"))
  rare <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable", rare_flow = TRUE))

  flow <- function(g) {
    f <- domain_facts(g, "Publication bias")
    strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1]]
  }
  # Same figure, different edge out of the same node. NO NODE IS ADDED.
  expect_true("pma-pubias-edge-q2-yes" %in% flow(regular))
  expect_true("pma-pubias-node-q3"     %in% flow(regular))
  expect_true("pma-pubias-edge-q2-no"  %in% flow(rare))
  expect_true("pma-pubias-node-q4"     %in% flow(rare))
  expect_false("pma-pubias-node-q3"    %in% flow(rare))
  expect_true(all(flow(rare) %in% .PUBIAS_FIG5_NODE_IDS))

  note <- rare$domain_assessments$notes[
    rare$domain_assessments$domain == "Publication bias"]
  # The reviewer has to see that the study count was not what decided it.
  expect_match(note, sprintf("despite k = %d >= 10", k), fixed = TRUE)
  expect_match(note, "rare-event analysis", fixed = TRUE)
})

test_that("the Q4 answer is the one that rates the domain under rare_flow", {
  # The point of routing the wizard and the package the same way: an answer
  # given at Q4 must not be collected and then ignored in favour of Egger.
  down <- suppressWarnings(grade_meta(
    sparse_meta(), threshold_type = "null", small_values = "desirable",
    rare_flow = TRUE, pubias_unpublished = "yes"))
  keep <- suppressWarnings(grade_meta(
    sparse_meta(), threshold_type = "null", small_values = "desirable",
    rare_flow = TRUE, pubias_unpublished = "no"))
  judge <- function(g) {
    g$domain_assessments$judgment[
      g$domain_assessments$domain == "Publication bias"]
  }
  expect_equal(judge(down), "serious")
  expect_equal(judge(keep), "not_serious")
})

# --------------------------------------------------------------------------
# 6. The method is named, and the record carries it
# --------------------------------------------------------------------------

test_that("the rated object records the method and the absent correction", {
  g <- suppressWarnings(grade_meta(
    sparse_meta(), threshold_type = "null", small_values = "desirable",
    rare_flow = TRUE, rare_method = "BB_CR"))

  expect_true(isTRUE(g$rare$flow))
  expect_equal(g$rare$method, "BB_CR")
  expect_equal(g$rare$method_label,
               "Beta-binomial with correlated responses")
  expect_match(g$rare$method_statement, "BB_CR", fixed = TRUE)
  expect_match(g$rare$no_cc_note, "No continuity correction", fixed = TRUE)
})

test_that("an ordinary rating carries no rare record at all", {
  g <- suppressWarnings(grade_meta(
    common_meta(), threshold_type = "null", small_values = "desirable"))
  expect_null(g$rare)
})

test_that("an unknown method id degrades to the id rather than vanishing", {
  expect_equal(.rare_method_label("NOT_A_METHOD"), "NOT_A_METHOD")
  expect_true(is.na(.rare_method_label(NULL)))
  expect_true(is.na(rare_method_statement(NULL)))
})

test_that("results.txt names the method and states the missing correction", {
  m <- sparse_meta()
  g <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable",
    rare_flow = TRUE, rare_method = "MH_no_cc"))
  path <- withr::local_tempfile(fileext = ".txt")
  .write_results_txt(m, g, path)
  txt <- paste(readLines(path), collapse = " ")

  expect_match(txt, "Analysis method - rare events", fixed = TRUE)
  expect_match(txt, "Mantel-Haenszel exact", fixed = TRUE)
  expect_match(txt, "No continuity correction", fixed = TRUE)
})

test_that("an ordinary rating writes no rare-events block", {
  m <- common_meta()
  g <- suppressWarnings(grade_meta(
    m, threshold_type = "null", small_values = "desirable"))
  path <- withr::local_tempfile(fileext = ".txt")
  .write_results_txt(m, g, path)
  expect_false(any(grepl("rare events", readLines(path), fixed = TRUE)))
})

# --------------------------------------------------------------------------
# 7. THE GOVERNING RULE
# --------------------------------------------------------------------------
# Rare mode changes what is computed and what is said, and never changes a
# rating by itself. On data that is not sparse, turning the flag on must move
# nothing: no domain judgment, no downgrade, no final certainty. If this ever
# fails, one of the changes above has become a rule rather than a correction.

test_that("no rare-mode change alters a domain judgment on non-rare data", {
  m <- common_meta()
  args <- list(meta_obj = m, threshold = 1.25, threshold_scale = "ratio",
               small_values = "desirable",
               pubias_small_industry = "no")
  off <- suppressWarnings(do.call(grade_meta, args))
  on  <- suppressWarnings(do.call(grade_meta, c(args, list(rare_flow = TRUE))))

  # What is COMPUTED does move - that is the whole point - so compare the
  # judgments, not the notes.
  cols <- c("domain", "judgment", "downgrade")
  expect_equal(off$domain_assessments[, cols],
               on$domain_assessments[, cols])
  expect_equal(off$certainty, on$certainty)
  expect_equal(off$certainty_score, on$certainty_score)
})

test_that("the rare-mode corrections leave every OTHER domain untouched on sparse data", {
  # Risk of bias and Indirectness read nothing about rarity, and must not
  # acquire a dependency on it by accident.
  m <- sparse_meta()
  args <- list(meta_obj = m, threshold_type = "null",
               small_values = "desirable")
  off <- suppressWarnings(do.call(grade_meta, args))
  on  <- suppressWarnings(do.call(grade_meta, c(args, list(rare_flow = TRUE))))

  pick <- function(g, dom) {
    g$domain_assessments[g$domain_assessments$domain == dom,
                         c("judgment", "downgrade")]
  }
  for (dom in c("Risk of bias", "Indirectness")) {
    expect_equal(pick(off, dom), pick(on, dom))
  }
  # And Imprecision's DECISION RULE is unchanged: the CI still crosses the
  # same null it crossed before, so the judgment is the same on both. Only the
  # OIS basis moved, and Fig 4 does not consult the OIS on this path.
  expect_equal(pick(off, "Imprecision"), pick(on, "Imprecision"))
})
