# test-plain_language.R — Core GRADE 6 Box 1 plain language summaries
#
# Guyatt G, Yao L, Murad MH, et al. Core GRADE 6: presenting the evidence in
# summary of findings tables. BMJ 2025;389:e083866.
#   Box 1   -- the standardised statements (p 3)
#   Table 1 -- the Very low wording as it appears in a table (p 2)
#   Table 3 -- "an important reduction in pain" (p 5)
#
# The regression this file exists for: pmatools used to take its statements
# from Core GRADE 2 Table 1, which is written entirely in terms of "benefit".
# A harm outcome with RR 2.42 for serious adverse events was therefore
# summarised as "Treatment likely has an important benefit" -- the opposite of
# what the evidence says. Box 1 names the direction of the effect instead.

library(testthat)
library(meta)

skip_if_not_installed("meta")

# --- fixtures ---------------------------------------------------------------

# Binary fixture with a controllable direction. `harm = TRUE` puts the extra
# events in the intervention arm, so the pooled RR is above 1.
pl_binary <- function(harm = FALSE, sm = "RR", outcome_name = "Mortality",
                      threshold_type = "null", ...) {
  ev_e <- c(10, 15, 20)
  ev_c <- c(15, 20, 25)
  if (harm) { tmp <- ev_e; ev_e <- ev_c; ev_c <- tmp }
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(ev_e[1], ev_c[1], ev_e[2], ev_c[2], ev_e[3], ev_c[3]),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = sm)
  suppressWarnings(grade_meta(
    ma, study_design = "RCT", rob = "no",
    rob_rationale = "Consensus RoB2: all domains low risk",
    indirectness = "no", outcome_name = outcome_name,
    threshold_type = threshold_type, ...))
}

# A large, precise harm signal: RR well above 2, mirroring the serious adverse
# events row that exposed the bug.
pl_sae <- function(threshold_type = "mid", ...) {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(300, 300, 320, 320, 340, 340),
    event   = c(72, 30, 78, 32, 84, 35),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = "RR")
  suppressWarnings(grade_meta(
    ma, study_design = "RCT", rob = "no",
    rob_rationale = "Consensus RoB2: all domains low risk",
    indirectness = "no", outcome_name = "Serious adverse events",
    threshold_type = threshold_type, ...))
}

# --- 1. the regression: harm outcomes must not read as a benefit ------------

test_that("a harm outcome (RR > 1) is summarised as an increase, not a benefit", {
  g <- pl_sae(threshold_type = "mid", threshold = 1.25)

  # The fixture is the shape that produced the wrong statement: a large,
  # precise increase in serious adverse events.
  expect_gt(exp(.pooled_estimate(g$meta)$est), 2)

  s <- .plain_language_for(g)
  expect_false(is.null(s))

  # The core of the fix.
  expect_no_match(s, "benefit", fixed = TRUE)
  expect_match(s, "increase")
  expect_match(s, "serious adverse events", fixed = TRUE)
})

test_that("every certainty level reads as an increase for a harm outcome", {
  g <- pl_sae(threshold_type = "null")
  for (cert in c("High", "Moderate", "Low")) {
    for (thr in c("null", "mid")) {
      target <- if (thr == "mid") "important_effect" else "non_null_effect"
      s <- .plain_language(cert, thr, target,
                           direction = .plain_language_direction(g$meta),
                           outcome_label = "serious adverse events")
      expect_no_match(s, "benefit", fixed = TRUE)
      expect_no_match(s, "reduc")
      expect_match(s, "increase")
    }
  }
})

# --- 2. benefit outcomes read as a reduction --------------------------------

test_that("a benefit outcome (RR < 1) is summarised as a reduction", {
  g <- pl_binary(harm = FALSE, outcome_name = "Mortality")
  expect_lt(exp(.pooled_estimate(g$meta)$est), 1)
  expect_identical(.plain_language_for(g), "Treatment reduces mortality")
})

test_that("the direction flips with the sign of the pooled estimate", {
  benefit <- pl_binary(harm = FALSE, outcome_name = "Mortality")
  harm    <- pl_binary(harm = TRUE,  outcome_name = "Mortality")
  expect_identical(.plain_language_direction(benefit$meta), "decrease")
  expect_identical(.plain_language_direction(harm$meta),    "increase")
  expect_identical(.plain_language_for(harm), "Treatment increases mortality")
})

test_that("the direction is read off the TE scale for every effect measure", {
  # Ratio measures are stored as logs, absolute measures raw; on the TE scale
  # the null is 0 for both, so the sign is the direction.
  expect_identical(.plain_language_direction_key(log(2.42)), "increase")
  expect_identical(.plain_language_direction_key(log(0.78)), "decrease")
  expect_identical(.plain_language_direction_key(5.38),      "increase")   # MD
  expect_identical(.plain_language_direction_key(-0.16),     "decrease")   # SMD
  expect_null(.plain_language_direction_key(0))
  expect_null(.plain_language_direction_key(NA_real_))
  expect_null(.plain_language_direction_key(NULL))

  # Continuous outcomes take the same route.
  m <- meta::metacont(
    n.e = c(50, 60, 70), mean.e = c(20, 22, 21), sd.e = c(10, 11, 12),
    n.c = c(50, 60, 70), mean.c = c(8, 9, 8),    sd.c = c(10, 11, 12),
    studlab = c("A", "B", "C"), sm = "MD")
  g <- suppressWarnings(grade_meta(
    m, study_design = "RCT", rob = "no",
    rob_rationale = "Consensus RoB2: all domains low risk",
    indirectness = "no", outcome_name = "Sleep duration",
    outcome_type = "absolute", threshold_type = "null"))
  expect_identical(.plain_language_for(g), "Treatment increases sleep duration")
})

# --- 3. little to no effect -------------------------------------------------

test_that("the little-to-no-difference target drops the direction word", {
  for (thr in c("null", "mid")) {
    for (cert in c("High", "Moderate", "Low")) {
      s <- .plain_language(cert, thr, "little_to_no_difference",
                           direction = "increase",
                           outcome_label = "function")
      expect_match(s, "little to no", fixed = TRUE)
      expect_no_match(s, "benefit", fixed = TRUE)
      # The direction argument is ignored for this target.
      expect_identical(
        s,
        .plain_language(cert, thr, "little_to_no_difference",
                        direction = "decrease", outcome_label = "function"))
    }
  }
})

test_that("a MID-threshold analysis inside the MID reads as little to no effect", {
  # A modest effect with a generous MID lands on little_to_no_difference.
  g <- pl_binary(harm = FALSE, threshold_type = "mid", threshold = 2,
                 threshold_scale = "ratio")
  expect_identical(g$rating_target, "little_to_no_difference")
  expect_identical(.plain_language_for(g),
                   "Treatment has little to no important effect on mortality")
})

# --- 4. Box 1, verbatim apart from the single-adverb rule -------------------

# Box 1's worked examples all use "knee arthroscopy" and "function". pmatools
# applies exactly two transformations: sentence case on the first character
# (which is how CG6 Table 1 prints the cells), and the single-adverb rule.
#
# SINGLE-ADVERB RULE (v0.5.0). Box 1's qualifier list prints two adverbs per
# certainty level, "probably (likely)" and "may (possibly)", which read as a
# double-barrelled statement in a table cell. No CG6 summary of findings table
# prints both: Table 1 has "may decrease mortality", Table 3 has "possibly
# increases", and the Box 1 MID example has "probably has little to no
# important effect". pmatools emits the FIRST word of each pair, so the
# expectations below carry "probably" and "may" rather than the parenthesised
# forms. Where that makes a statement no longer a verbatim quotation, the
# comment says so.

test_that("the null-threshold statements match Box 1 (single adverb)", {
  pl <- function(cert) {
    .plain_language(cert, "null", "non_null_effect", direction = "increase",
                    outcome_label = "function",
                    intervention_label = "knee arthroscopy")
  }
  # Box 1, verbatim: "High certainty: knee arthroscopy increases function"
  expect_identical(pl("High"), "Knee arthroscopy increases function")
  # Box 1: "Moderate certainty: knee arthroscopy probably (likely) increases
  #         function" -- no longer verbatim: "(likely)" is dropped.
  expect_identical(pl("Moderate"),
                   "Knee arthroscopy probably increases function")
  # Box 1: "Low certainty: knee arthroscopy may (possibly) increase function"
  #         -- no longer verbatim: "(possibly)" is dropped.
  expect_identical(pl("Low"),
                   "Knee arthroscopy may increase function")
  # Box 1: "Very low certainty: the effect of knee arthroscopy on function is
  #         very uncertain" -- rendered in the CG6 Table 1 sentence form.
  expect_identical(
    pl("Very Low"),
    "We are very uncertain about the effect of knee arthroscopy on function")
})

test_that("the MID-threshold statements match Box 1 (single adverb)", {
  imp <- function(cert) {
    .plain_language(cert, "mid", "important_effect", direction = "increase",
                    outcome_label = "function",
                    intervention_label = "knee arthroscopy")
  }
  lit <- function(cert) {
    .plain_language(cert, "mid", "little_to_no_difference",
                    outcome_label = "function",
                    intervention_label = "knee arthroscopy")
  }
  # Box 1: "High certainty of an important effect: knee arthroscopy results in
  #         an important increase in function"
  expect_identical(imp("High"),
                   "Knee arthroscopy results in an important increase in function")
  # Box 1, verbatim: "Moderate certainty of little to no effect: knee
  #         arthroscopy probably has little to no important effect on
  #         function". Box 1 already writes this one with a single "probably",
  #         which is the precedent the single-adverb rule follows, so this cell
  #         is unchanged by it and stays verbatim.
  expect_identical(
    lit("Moderate"),
    "Knee arthroscopy probably has little to no important effect on function")
  # Box 1: "Low certainty of an important effect: knee arthroscopy may
  #         (possibly) result in an important increase in function"
  #         -- no longer verbatim: "(possibly)" is dropped.
  expect_identical(
    imp("Low"),
    "Knee arthroscopy may result in an important increase in function")
  # Box 1: "Very low certainty: the effect of knee arthroscopy on function is
  #         very uncertain"
  expect_identical(
    imp("Very Low"),
    "We are very uncertain about the effect of knee arthroscopy on function")

  # Cells Box 1 gives no worked example for, assembled from its qualifier list
  # (with the single-adverb rule applied to that list).
  expect_identical(
    imp("Moderate"),
    "Knee arthroscopy probably results in an important increase in function")
  expect_identical(
    lit("High"),
    "Knee arthroscopy has little to no important effect on function")
  expect_identical(
    lit("Low"),
    "Knee arthroscopy may have little to no important effect on function")
})

test_that("no statement ever carries both adverbs of a Box 1 qualifier pair", {
  # The single-adverb rule, asserted across the whole frame table rather than
  # cell by cell: a parenthesised alternative must never reach the output.
  for (thr in c("null", "mid")) {
    for (cert in c("High", "Moderate", "Low", "Very Low")) {
      for (target in c("important_effect", "non_null_effect",
                       "little_to_no_difference")) {
        s <- .plain_language(cert, thr, target, direction = "increase",
                             outcome_label = "function",
                             intervention_label = "knee arthroscopy")
        if (is.null(s)) next
        expect_no_match(s, "(likely)", fixed = TRUE)
        expect_no_match(s, "(possibly)", fixed = TRUE)
        expect_no_match(s, "(", fixed = TRUE)
      }
    }
  }
  # Moderate is "probably", never "likely"; Low is "may", never "possibly".
  expect_match(
    .plain_language("Moderate", "null", "non_null_effect",
                    direction = "increase", outcome_label = "function"),
    "^Treatment probably increases")
  expect_match(
    .plain_language("Low", "null", "non_null_effect",
                    direction = "increase", outcome_label = "function"),
    "^Treatment may increase")
})

test_that("the reduction mirror follows Table 3's wording", {
  # CG6 Table 3: "... an important reduction in pain"
  expect_identical(
    .plain_language("High", "mid", "important_effect", direction = "decrease",
                    outcome_label = "pain",
                    intervention_label = "knee arthroscopy"),
    "Knee arthroscopy results in an important reduction in pain")
  # Box 1 qualifier list: "High certainty: reduces, increases, ..."
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "decrease",
                    outcome_label = "pain",
                    intervention_label = "knee arthroscopy"),
    "Knee arthroscopy reduces pain")
  # Single adverb: the qualifier list's "may (possibly) reduce" emits "may",
  # matching CG6 Table 1's own cell ("may decrease mortality").
  expect_identical(
    .plain_language("Low", "null", "non_null_effect", direction = "decrease",
                    outcome_label = "pain",
                    intervention_label = "knee arthroscopy"),
    "Knee arthroscopy may reduce pain")
})

test_that("the null-threshold little-to-no wording follows the qualifier list", {
  # Box 1 qualifier list: "... or has little to no effect"
  expect_identical(
    .plain_language("High", "null", "little_to_no_difference",
                    outcome_label = "function",
                    intervention_label = "knee arthroscopy"),
    "Knee arthroscopy has little to no effect on function")
  # Single adverb: "may (possibly) ... have little to no effect" -> "may".
  expect_identical(
    .plain_language("Low", "null", "little_to_no_difference",
                    outcome_label = "function",
                    intervention_label = "knee arthroscopy"),
    "Knee arthroscopy may have little to no effect on function")
})

# --- 5. Very low is direction neutral and names the outcome -----------------

test_that("Very low names the outcome and carries no direction", {
  # CG6 Table 1: "We are very uncertain about the effect of intensive
  # antileukaemic treatment on serious adverse events"
  s <- .plain_language(
    "Very Low", "mid", "important_effect", direction = "increase",
    outcome_label = "serious adverse events",
    intervention_label = "intensive antileukaemic treatment")
  expect_identical(
    s,
    paste("We are very uncertain about the effect of intensive antileukaemic",
          "treatment on serious adverse events"))
  expect_no_match(s, "increase")
  expect_no_match(s, "reduc")
  expect_no_match(s, "benefit", fixed = TRUE)

  # The direction argument cannot change it, and it is not even needed.
  expect_identical(
    s,
    .plain_language("Very Low", "mid", "important_effect",
                    direction = "decrease",
                    outcome_label = "serious adverse events",
                    intervention_label = "intensive antileukaemic treatment"))
  expect_identical(
    s,
    .plain_language("Very Low", "mid", "important_effect",
                    outcome_label = "serious adverse events",
                    intervention_label = "intensive antileukaemic treatment"))
})

# --- 6. missing pieces degrade instead of erroring --------------------------

test_that("an object without a rating target yields no statement", {
  g <- pl_binary()
  g$rating_target <- NULL
  expect_null(.plain_language_for(g))
  expect_null(.plain_language(g$certainty, g$threshold_type, NULL,
                              direction = "increase"))
  expect_null(.plain_language("High", "null", NA_character_,
                              direction = "increase"))
})

test_that("an unusable direction drops the statement rather than guessing it", {
  # Box 1 has no direction-free wording for a non-null / important effect.
  expect_null(.plain_language("High", "null", "non_null_effect"))
  expect_null(.plain_language("High", "mid", "important_effect",
                              direction = NA_character_))
  # ... but Very low and little-to-no need no direction.
  expect_false(is.null(.plain_language("Very Low", "null", "non_null_effect")))
  expect_false(is.null(
    .plain_language("High", "mid", "little_to_no_difference")))
})

test_that("unknown certainty and threshold labels return NULL", {
  expect_null(.plain_language("Unrated", "null", "non_null_effect",
                              direction = "increase"))
  expect_null(.plain_language("High", "banana", "non_null_effect",
                              direction = "increase"))
  expect_null(.plain_language(NA_character_, "null", "non_null_effect",
                              direction = "increase"))
})

# --- 7. labels --------------------------------------------------------------

test_that("a missing outcome label falls back to a generic noun", {
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "increase"),
    "Treatment increases the outcome")
  # grade_meta()'s own placeholder counts as missing.
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "increase",
                    outcome_label = "Outcome"),
    "Treatment increases the outcome")
  expect_identical(
    .plain_language("Very Low", "null", "non_null_effect"),
    "We are very uncertain about the effect of treatment on the outcome")
})

test_that("sentence-cased labels are lowered mid-sentence, acronyms are not", {
  expect_identical(.plain_language_lower_first("Mortality"), "mortality")
  expect_identical(.plain_language_lower_first("Serious adverse events"),
                   "serious adverse events")
  expect_identical(.plain_language_lower_first("HbA1c"), "HbA1c")
  expect_identical(.plain_language_lower_first("CBT-I"), "CBT-I")
  expect_identical(.plain_language_lower_first("SGLT-2 inhibitors"),
                   "SGLT-2 inhibitors")

  expect_identical(
    .plain_language("Moderate", "null", "non_null_effect",
                    direction = "increase", outcome_label = "HbA1c",
                    intervention_label = "CBT-I"),
    # Single adverb (v0.5.0): Moderate emits "probably", not
    # "probably (likely)".
    "CBT-I probably increases HbA1c")
})

test_that("the intervention label opens the sentence", {
  expect_identical(
    .plain_language("High", "mid", "important_effect", direction = "decrease",
                    outcome_label = "Mortality",
                    intervention_label = "intensive antileukaemic treatment"),
    paste("Intensive antileukaemic treatment results in an important",
          "reduction in mortality"))
  expect_identical(
    .plain_language("Low", "null", "non_null_effect", direction = "decrease",
                    outcome_label = "Mortality",
                    intervention_label = "CBT-I"),
    # Single adverb (v0.5.0): Low emits "may", not "may (possibly)".
    "CBT-I may reduce mortality")
})

test_that("an explicit outcome_label overrides the object's outcome_name", {
  g <- pl_binary(harm = TRUE, outcome_name = "Mortality")
  expect_identical(.plain_language_for(g, outcome_label = "all cause death"),
                   "Treatment increases all cause death")
})
