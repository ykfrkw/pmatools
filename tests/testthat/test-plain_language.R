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
    small_values = "desirable",
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
    small_values = "desirable",
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
    m,
    small_values = "desirable", study_design = "RCT", rob = "no",
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

# ==========================================================================
# 8. The two margin families (v0.5.1; SPEC.md 5.5c)
# ==========================================================================
#
# Neither is GRADE wording: Core GRADE 6 Box 1 has no statement for an
# equivalence or a non-inferiority question, and every cell of both families is
# tagged accordingly. The golden table comes first, because none of the rest
# means anything if a call that names no family has stopped reaching the cell
# it always reached.

# --- 8a. golden table: frame_family = NULL is unchanged ---------------------

test_that("frame_family = NULL reaches exactly the cell it did before", {
  # Every reachable combination of the two Box 1 families, asserted verbatim.
  # A zone is passed alongside on purpose: the direction axis must keep
  # deciding for these families even when a zone is available.
  # cert | threshold_type | rating_target | direction -> predicate
  cell <- function(cert, thr, target, dir, predicate) {
    list(cert = cert, thr = thr, target = target, dir = dir,
         predicate = predicate)
  }
  incr <- "increase"
  decr <- "decrease"
  none <- "non_null_effect"
  litt <- "little_to_no_difference"
  impt <- "important_effect"
  golden <- list(
    cell("High",     "null", none, incr, "increases mortality"),
    cell("High",     "null", none, decr, "reduces mortality"),
    cell("High",     "null", litt, NULL,
         "has little to no effect on mortality"),
    cell("Moderate", "null", none, incr, "probably increases mortality"),
    cell("Moderate", "null", none, decr, "probably reduces mortality"),
    cell("Moderate", "null", litt, NULL,
         "probably has little to no effect on mortality"),
    cell("Low",      "null", none, incr, "may increase mortality"),
    cell("Low",      "null", none, decr, "may reduce mortality"),
    cell("Low",      "null", litt, NULL,
         "may have little to no effect on mortality"),
    cell("High",     "mid",  impt, incr,
         "results in an important increase in mortality"),
    cell("High",     "mid",  impt, decr,
         "results in an important reduction in mortality"),
    cell("High",     "mid",  litt, NULL,
         "has little to no important effect on mortality"),
    cell("Moderate", "mid",  impt, incr,
         "probably results in an important increase in mortality"),
    cell("Moderate", "mid",  impt, decr,
         "probably results in an important reduction in mortality"),
    cell("Moderate", "mid",  litt, NULL,
         "probably has little to no important effect on mortality"),
    cell("Low",      "mid",  impt, incr,
         "may result in an important increase in mortality"),
    cell("Low",      "mid",  impt, decr,
         "may result in an important reduction in mortality"),
    cell("Low",      "mid",  litt, NULL,
         "may have little to no important effect on mortality")
  )
  for (row in golden) {
    for (zone in list(NULL, "within", "crosses", "beyond")) {
      expect_identical(
        .plain_language(row$cert, row$thr, row$target, direction = row$dir,
                        outcome_label = "Mortality",
                        frame_family = NULL, threshold_zone = zone),
        paste0("Treatment ", row$predicate),
        info = paste(row$cert, row$thr, row$target,
                     if (is.null(zone)) "no zone" else zone))
    }
  }
})

# --- 8b. the 18 new sentences, verbatim -------------------------------------

test_that("the equivalence family reads as specified", {
  say <- function(cert, zone) {
    .plain_language(cert, "mid", "little_to_no_difference",
                    outcome_label = "Mortality",
                    frame_family = "equivalence", threshold_zone = zone)
  }
  expect_identical(say("High", "within"), paste(
    "Treatment results in a difference in mortality that lies within the",
    "equivalence threshold"))
  expect_identical(say("High", "crosses"), paste(
    "Treatment results in little to no difference in mortality, but a",
    "difference beyond the equivalence threshold is not excluded"))
  expect_identical(say("High", "beyond"), paste(
    "Treatment results in a difference in mortality that lies beyond the",
    "equivalence threshold"))

  expect_identical(say("Moderate", "within"), paste(
    "Treatment probably results in a difference in mortality that lies within",
    "the equivalence threshold"))
  expect_identical(say("Moderate", "crosses"), paste(
    "Treatment probably results in little to no difference in mortality, but a",
    "difference beyond the equivalence threshold is not excluded"))
  expect_identical(say("Moderate", "beyond"), paste(
    "Treatment probably results in a difference in mortality that lies beyond",
    "the equivalence threshold"))

  expect_identical(say("Low", "within"), paste(
    "Treatment may result in a difference in mortality that lies within the",
    "equivalence threshold"))
  expect_identical(say("Low", "crosses"), paste(
    "Treatment may result in little to no difference in mortality, but a",
    "difference beyond the equivalence threshold is not excluded"))
  expect_identical(say("Low", "beyond"), paste(
    "Treatment may result in a difference in mortality that lies beyond the",
    "equivalence threshold"))
})

test_that("the non-inferiority family reads as specified", {
  say <- function(cert, zone) {
    .plain_language(cert, "mid", "little_to_no_difference",
                    outcome_label = "Mortality",
                    frame_family = "non_inferiority", threshold_zone = zone)
  }
  expect_identical(say("High", "within"), paste(
    "Treatment is not worse than the comparator in mortality by more than the",
    "non-inferiority threshold"))
  expect_identical(say("High", "crosses"), paste(
    "Treatment is not worse than the comparator in mortality by more than the",
    "non-inferiority threshold, but a larger difference is not excluded"))
  expect_identical(say("High", "beyond"), paste(
    "Treatment is worse than the comparator in mortality by more than the",
    "non-inferiority threshold"))

  expect_identical(say("Moderate", "within"), paste(
    "Treatment is probably not worse than the comparator in mortality by more",
    "than the non-inferiority threshold"))
  expect_identical(say("Moderate", "crosses"), paste(
    "Treatment is probably not worse than the comparator in mortality by more",
    "than the non-inferiority threshold, but a larger difference is not",
    "excluded"))
  expect_identical(say("Moderate", "beyond"), paste(
    "Treatment is probably worse than the comparator in mortality by more than",
    "the non-inferiority threshold"))

  expect_identical(say("Low", "within"), paste(
    "Treatment may be no worse than the comparator in mortality by more than",
    "the non-inferiority threshold"))
  expect_identical(say("Low", "crosses"), paste(
    "Treatment may be no worse than the comparator in mortality by more than",
    "the non-inferiority threshold, but a larger difference is not excluded"))
  expect_identical(say("Low", "beyond"), paste(
    "Treatment may be worse than the comparator in mortality by more than the",
    "non-inferiority threshold"))
})

test_that("the actor and the outcome are substituted as in every other family", {
  expect_identical(
    .plain_language("Moderate", "mid", "little_to_no_difference",
                    outcome_label = "HbA1c", intervention_label = "CBT-I",
                    frame_family = "non_inferiority",
                    threshold_zone = "within"),
    paste("CBT-I is probably not worse than the comparator in HbA1c by more",
          "than the non-inferiority threshold"))
  # grade_meta()'s own placeholder still counts as a missing label.
  expect_match(
    .plain_language("High", "mid", "little_to_no_difference",
                    outcome_label = "Outcome", frame_family = "equivalence",
                    threshold_zone = "beyond"),
    "difference in the outcome that lies beyond", fixed = TRUE)
})

# --- 8c. the zone axis ------------------------------------------------------

test_that("the margin families ignore the direction entirely", {
  # Direction-free by design: a margin question is symmetric about the
  # comparator, and the direction lives in the effect column of the table.
  for (fam in PLAIN_LANGUAGE_ZONE_FAMILIES) {
    base <- .plain_language("Moderate", "mid", "little_to_no_difference",
                            outcome_label = "Mortality", frame_family = fam,
                            threshold_zone = "crosses")
    for (dir in list("increase", "decrease", log(2.42), NULL)) {
      expect_identical(
        .plain_language("Moderate", "mid", "little_to_no_difference",
                        direction = dir, outcome_label = "Mortality",
                        frame_family = fam, threshold_zone = "crosses"),
        base, info = fam)
    }
  }
})

test_that("a missing or unrecognised zone drops the column", {
  for (fam in PLAIN_LANGUAGE_ZONE_FAMILIES) {
    for (zone in list(NULL, NA_character_, "", "middle", "Within", 3)) {
      expect_null(
        .plain_language("Moderate", "mid", "little_to_no_difference",
                        direction = "increase", outcome_label = "Mortality",
                        frame_family = fam, threshold_zone = zone),
        info = paste(fam, "/", paste(deparse(zone), collapse = "")))
    }
  }
})

test_that("an unrecognised frame_family falls back to threshold_type", {
  # Same shape as the zone rule: no abort here, because .plain_language() is
  # a renderer. grade_meta() is where a bad family is refused.
  expect_identical(
    .plain_language("High", "null", "non_null_effect", direction = "increase",
                    outcome_label = "Mortality", frame_family = "superiority"),
    "Treatment increases mortality")
})

test_that("Very low ignores both the family and the zone", {
  # PLAIN_LANGUAGE_VERY_LOW is already zone- and direction-neutral and spans
  # every family; its early return sits above all family resolution.
  expected <- "We are very uncertain about the effect of treatment on mortality"
  for (fam in c(list(NULL), as.list(PLAIN_LANGUAGE_ZONE_FAMILIES))) {
    for (zone in list(NULL, "within", "crosses", "beyond", "nonsense")) {
      expect_identical(
        .plain_language("Very low", "mid", "little_to_no_difference",
                        outcome_label = "Mortality", frame_family = fam,
                        threshold_zone = zone),
        expected)
    }
  }
})

test_that("the zone vocabulary is the assessor's, not a second copy", {
  for (fam in PLAIN_LANGUAGE_ZONE_FAMILIES) {
    for (cert in c("High", "Moderate", "Low")) {
      expect_identical(sort(names(PLAIN_LANGUAGE_FRAMES[[fam]][[cert]])),
                       sort(PMA_IMPRE_THRESHOLD_ZONES))
    }
  }
})

test_that("every cell of both new families exists", {
  # A missing cell returns NULL and silently drops the whole column, so the
  # unreachable High x crosses cells are kept and asserted too.
  for (fam in PLAIN_LANGUAGE_ZONE_FAMILIES) {
    for (cert in c("High", "Moderate", "Low")) {
      for (zone in PMA_IMPRE_THRESHOLD_ZONES) {
        cell <- PLAIN_LANGUAGE_FRAMES[[fam]][[cert]][[zone]]
        expect_true(is.character(cell) && length(cell) == 1L && nzchar(cell),
                    info = paste(fam, cert, zone))
        expect_match(cell, "%s", fixed = TRUE, info = paste(fam, cert, zone))
      }
    }
  }
})

# --- 8d. the audits ---------------------------------------------------------

# Every cell in the table, flattened, with a label for the failure message.
pl_all_cells <- function() {
  out <- character(0)
  for (fam in names(PLAIN_LANGUAGE_FRAMES)) {
    for (cert in names(PLAIN_LANGUAGE_FRAMES[[fam]])) {
      cells <- PLAIN_LANGUAGE_FRAMES[[fam]][[cert]]
      out <- c(out, stats::setNames(
        unlist(cells, use.names = FALSE),
        paste(fam, cert, names(cells), sep = "/")))
    }
  }
  out
}

test_that("no cell anywhere in PLAIN_LANGUAGE_FRAMES says MID", {
  # A standing guard, not a one-off: "MID" is internal vocabulary and
  # "Threshold" is what a user reads (SPEC.md 4.5.1). Word-boundary and
  # case-sensitive, so "amid" and "mid" pass and only the acronym fails.
  cells <- pl_all_cells()
  hits  <- names(cells)[grepl("\\bMID\\b", cells)]
  expect_identical(hits, character(0))
  # And the same for the two standing strings beside the table.
  expect_false(grepl("\\bMID\\b", PLAIN_LANGUAGE_VERY_LOW))
  expect_false(grepl("\\bMID\\b", PLAIN_LANGUAGE_TABLE_NOTE))
})

test_that("no cell carries two modal adverbs", {
  # The single-adverb rule: Box 1 prints "probably (likely)" and
  # "may (possibly)", and pmatools emits only the first word of each pair.
  cells <- pl_all_cells()
  modals <- c("probably", "likely", "may", "possibly")
  for (nm in names(cells)) {
    found <- modals[vapply(modals,
                           function(w) grepl(paste0("\\b", w, "\\b"), cells[[nm]]),
                           logical(1))]
    expect_lte(length(found), 1L,
               label = paste0(nm, " carries: ", paste(found, collapse = " + ")))
  }
})

test_that("the certainty level and the adverb agree in every family", {
  for (fam in names(PLAIN_LANGUAGE_FRAMES)) {
    for (cell in PLAIN_LANGUAGE_FRAMES[[fam]][["High"]]) {
      expect_false(grepl("\\b(probably|may|likely|possibly)\\b", cell),
                   info = paste(fam, cell))
    }
    for (cell in PLAIN_LANGUAGE_FRAMES[[fam]][["Moderate"]]) {
      expect_match(cell, "\\bprobably\\b", info = paste(fam, cell))
    }
    for (cell in PLAIN_LANGUAGE_FRAMES[[fam]][["Low"]]) {
      expect_match(cell, "\\bmay\\b", info = paste(fam, cell))
    }
  }
})

test_that("every cell of the two new families carries the provenance tag", {
  # Read from the source, because the tag is the comment beside the cell: it
  # exists so that the next editor of THIS FILE cannot add an untagged margin
  # sentence, which no runtime check could catch.
  src_path <- test_path("..", "..", "R", "plain_language.R")
  skip_if_not(file.exists(src_path),
              "package sources not laid out as expected")
  src <- readLines(src_path, warn = FALSE)

  tag <- "# [pmatools; no Box 1 counterpart]"
  # 18 cells, one tag each, plus the entry in the provenance comment block
  # that documents the tag alongside [Box 1] / [Table 3] / [composed].
  n_cells <- length(PLAIN_LANGUAGE_ZONE_FAMILIES) * 3L *
             length(PMA_IMPRE_THRESHOLD_ZONES)
  expect_identical(n_cells, 18L)
  expect_gte(sum(grepl(tag, src, fixed = TRUE)), n_cells)
  expect_true(any(grepl("[pmatools; no Box 1 counterpart]", src,
                        fixed = TRUE)))

  # The two families must claim neither of the tags reserved for CG6 wording.
  from <- grep("^  equivalence = list\\(", src)
  to   <- grep("^PLAIN_LANGUAGE_ZONE_FAMILIES", src)
  expect_length(from, 1L)
  expect_length(to, 1L)
  block <- src[from:to]
  expect_false(any(grepl("[Box 1]", block, fixed = TRUE)))
  expect_false(any(grepl("[Table 3]", block, fixed = TRUE)))
  expect_false(any(grepl("[composed", block, fixed = TRUE)))
})

# --- 8e. the gate -----------------------------------------------------------

test_that(".check_plain_language_frame accepts NULL and the two families", {
  expect_null(.check_plain_language_frame(NULL))
  for (fam in PLAIN_LANGUAGE_ZONE_FAMILIES) {
    expect_identical(.check_plain_language_frame(fam), fam)
  }
})

test_that(".check_plain_language_frame refuses anything else, and is not a threshold gate", {
  for (bad in list("mid", "null", "superiority", NA_character_, 1, c("a", "b"))) {
    cnd <- tryCatch(.check_plain_language_frame(bad),
                    condition = function(e) e)
    expect_s3_class(cnd, "error")
    # It changes no judgment, so grade_meta_multi() must be free to demote it
    # like any other per-outcome failure rather than re-raise it.
    expect_false(inherits(cnd, "pmatools_threshold_gate"))
    expect_match(conditionMessage(cnd),
                 "must be NULL, 'equivalence' or 'non_inferiority'",
                 fixed = TRUE)
  }
})

# --- 8f. resolution off a rated object --------------------------------------

test_that("the family is resolved from the object in one place", {
  expect_null(.plain_language_frame_of(list()))
  expect_identical(
    .plain_language_frame_of(list(plain_language_frame = "equivalence")),
    "equivalence")
  # threshold_sides = "worse_only" and non-inferiority travel together.
  expect_identical(
    .plain_language_frame_of(list(threshold_sides = "worse_only")),
    "non_inferiority")
  expect_null(.plain_language_frame_of(list(threshold_sides = "both")))
  # An explicit frame wins over the fallback.
  expect_identical(
    .plain_language_frame_of(list(threshold_sides = "worse_only",
                                  plain_language_frame = "equivalence")),
    "equivalence")
  # A nonsense field is ignored rather than trusted.
  expect_null(.plain_language_frame_of(list(plain_language_frame = "bogus")))
})

test_that("equivalence is never inferred from a pinned target alone", {
  # The trap this guards: threshold_type = "mid" plus a manually pinned
  # "little_to_no_difference" target is a legitimate PRE-EXISTING override,
  # and reading it as an equivalence question would silently change the
  # wording of ratings already made.
  pre_existing <- list(
    certainty      = "Moderate",
    threshold_type = "mid",
    rating_target  = "little_to_no_difference",
    outcome_name   = "Mortality",
    meta           = NULL
  )
  expect_null(.plain_language_frame_of(pre_existing))
  expect_identical(
    .plain_language_for(pre_existing),
    "Treatment probably has little to no important effect on mortality")
})

test_that("a pre-existing-shaped object has no new fields and is unaffected", {
  g <- pl_binary(harm = TRUE, outcome_name = "Mortality")
  before <- .plain_language_for(g)
  g$threshold_sides      <- NULL
  g$threshold_zone       <- NULL
  g$plain_language_frame <- NULL
  expect_identical(.plain_language_for(g), before)
  expect_identical(before, "Treatment increases mortality")
})

# --------------------------------------------------------------------------
# "MID" is internal vocabulary: a standing package-wide guard
# --------------------------------------------------------------------------
# SPEC.md 4.5.1 settles the vocabulary: "MID" names an internal quantity and
# "Threshold" is the word a reader sees. The guard above holds the line for
# PLAIN_LANGUAGE_FRAMES; this one holds it for every other string the package
# can put in front of a reader -- an abort message, a domain fact, a Summary
# of Findings footnote -- because those reach a screen by a different route
# and were where the word actually survived (the Chinn footnote said "more
# than the MID" while the sentence directly above it said "Threshold
# definition").
#
# Scans source rather than calling every builder: a string literal cannot be
# reached by a test that does not know the argument combination that renders
# it, and a rule about a forbidden word has to fail where the word is typed.
# Comments and roxygen (`#`, `#'`) are excluded on purpose -- the internals
# keep the name, and the two roxygen occurrences that remain are VERBATIM
# QUOTATIONS from Core GRADE 6 and 7 ("whether the MID for mortality is 2%,
# 1%, or less than 1% ..."). Editing a quotation to satisfy a house style is
# misquoting the source, so the exclusion is a decision and not an oversight.

test_that("no string literal in R/ says MID", {
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources not laid out as expected")

  offenders <- character(0)
  for (f in list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    # Drop whole-line comments and roxygen; a trailing comment after code is
    # kept, which is the conservative direction for a forbidden-word check.
    code <- lines[!grepl("^\\s*#", lines)]
    hits <- grep("\\bMID\\b", code, value = TRUE)
    if (length(hits) > 0) {
      offenders <- c(offenders, paste0(basename(f), ": ", trimws(hits)))
    }
  }
  expect_identical(offenders, character(0))
})

test_that("the Chinn footnote says Threshold, not MID", {
  # The one that got away, pinned at the rendered string rather than at the
  # source, so a rewording that reintroduces the word fails here too.
  note <- .chinn_note(threshold_label = "50 percent improvement in symptoms",
                      baseline_risk = 0.3)
  expect_false(grepl("\\bMID\\b", note))
  expect_match(note, "more than the Threshold", fixed = TRUE)
  expect_match(note, "uses no Threshold", fixed = TRUE)
  # The neighbouring sentence it used to contradict.
  expect_match(note, "Threshold definition:", fixed = TRUE)
})
