# plain_language.R — Core GRADE 6 Box 1 plain language summaries
#
# Reference (the canonical source for summary of findings tables):
#   Guyatt G, Yao L, Murad MH, et al.
#     Core GRADE 6: presenting the evidence in summary of findings tables.
#     BMJ. 2025;389:e083866. doi:10.1136/bmj-2024-083866
#     -- Box 1 "Writing standardised GRADE plain language summaries in summary
#        of findings tables" (p 3 of the published version), plus the plain
#        language summary column of Table 1 (p 2) and Table 3 (p 5).
#
# Core GRADE 6 introduces Box 1 as a superset of the earlier Core GRADE 2
# Table 1: Box 1 "summarises this guidance as well as additional guidance
# related to the null and MID thresholds that are the focus of Core GRADE"
# (CG6, p 2-3). pmatools followed CG2 Table 1 up to v0.4.0; from v0.5.0 the
# statements come from CG6 Box 1.
#
# The practical difference is direction. CG2 Table 1 is written entirely in
# terms of "benefit" (with a footnote saying harm reads the same way), which
# produces literally inverted statements for harm outcomes -- an outcome with
# RR 2.42 for serious adverse events was summarised as "Treatment likely has an
# important benefit". CG6 Box 1 instead names the direction of the effect on
# the outcome itself: reduces / increases / has little to no effect. That is
# always readable, whether the outcome is a benefit or a harm.
#
# --- Box 1, transcribed verbatim -------------------------------------------
#
#   Box 1: Writing standardised GRADE plain language summaries in summary of
#   findings tables
#
#   Standardised plain language summaries should convey, for each outcome,
#   information about the certainty of the evidence and the effect of the
#   intervention. The following qualifiers then inform the direction of the
#   effect:
#   * High certainty: reduces, increases, or has little to no effect
#   * Moderate certainty: probably (likely) reduces, increases, or has little
#     to no effect
#   * Low certainty: may (possibly) reduce, increase, or have little to no
#     effect
#   * Very low certainty: the evidence is very uncertain; or the effect is very
#     uncertain
#
#   When focusing on the target of certainty in relation to the null, plain
#   language summaries should communicate that there is a benefit or harm,
#   which is to be understood as a non-null effect. Examples:
#   * High certainty: knee arthroscopy increases function
#   * Moderate certainty: knee arthroscopy probably (likely) increases function
#   * Low certainty: knee arthroscopy may (possibly) increase function
#   * Very low certainty: the effect of knee arthroscopy on function is very
#     uncertain
#
#   When focusing on the minimal important difference, plain language summaries
#   should communicate that there is an important benefit or harm, or
#   alternatively that there is little to no important effect. Examples:
#   * High certainty of an important effect: knee arthroscopy results in an
#     important increase in function
#   * Moderate certainty of little to no effect: knee arthroscopy probably has
#     little to no important effect on function
#   * Low certainty of an important effect: knee arthroscopy may (possibly)
#     result in an important increase in function
#   * Very low certainty: the effect of knee arthroscopy on function is very
#     uncertain
#
#   GRADE=Grading of Recommendations Assessment, Development and Evaluation.
#
# --- Very low: the wording actually used in a table -------------------------
#
# Box 1 gives the Very low form as "the effect of knee arthroscopy on function
# is very uncertain". The plain language column of CG6 Table 1 renders the same
# statement as a full sentence, e.g.
#
#   "We are very uncertain about the effect of intensive antileukaemic
#    treatment on complete remission"
#   "We are very uncertain about the effect of intensive antileukaemic
#    treatment on serious adverse events"
#   "We are very uncertain about the effect of intensive antileukaemic
#    treatment on duration of hospital stay"
#
# pmatools emits the Table 1 form (it is the one CG6 itself puts in a summary
# of findings table); the Box 1 form is kept below as
# PLAIN_LANGUAGE_VERY_LOW_BOX1 for reference. Both are direction neutral and
# both name the outcome.
#
# --- Reduction wording ------------------------------------------------------
#
# The mirror of "an important increase in X" is "an important reduction in X",
# taken verbatim from the plain language column of CG6 Table 3:
#   "Knee arthroscopy probably does not result in an important reduction in
#    pain"
#   "Knee arthroscopy possibly increases the number of patients with an
#    important reduction in pain"

# --- Box 1 statement frames -------------------------------------------------

# Each frame is the predicate that follows the intervention label; "%s" is the
# outcome. Cells marked [Box 1] / [Table 3] are transcribed verbatim from the
# cited example; cells marked [composed] have no worked example in CG6 and are
# assembled from the Box 1 qualifier list ("The following qualifiers then
# inform the direction of the effect") applied to the frame of the verbatim
# example in the same column. Nothing here is paraphrased.
PLAIN_LANGUAGE_FRAMES <- list(
  # "When focusing on the target of certainty in relation to the null"
  null = list(
    "High" = list(
      # [Box 1] "knee arthroscopy increases function"
      increase = "increases %s",
      # [Box 1 qualifier list] "High certainty: reduces, ..."
      decrease = "reduces %s",
      # [Box 1 qualifier list] "... or has little to no effect" [composed: "on %s"]
      little   = "has little to no effect on %s"
    ),
    "Moderate" = list(
      # [Box 1] "knee arthroscopy probably (likely) increases function"
      increase = "probably (likely) increases %s",
      # [Box 1 qualifier list] "Moderate certainty: probably (likely) reduces, ..."
      decrease = "probably (likely) reduces %s",
      # [composed] qualifier list + "on %s"
      little   = "probably (likely) has little to no effect on %s"
    ),
    "Low" = list(
      # [Box 1] "knee arthroscopy may (possibly) increase function"
      increase = "may (possibly) increase %s",
      # [Box 1 qualifier list] "Low certainty: may (possibly) reduce, ..."
      decrease = "may (possibly) reduce %s",
      # [composed] qualifier list + "on %s"
      little   = "may (possibly) have little to no effect on %s"
    )
  ),
  # "When focusing on the minimal important difference"
  mid = list(
    "High" = list(
      # [Box 1] "knee arthroscopy results in an important increase in function"
      increase = "results in an important increase in %s",
      # [Table 3] "... an important reduction in pain"
      decrease = "results in an important reduction in %s",
      # [composed] the Moderate Box 1 example without its qualifier
      little   = "has little to no important effect on %s"
    ),
    "Moderate" = list(
      # [composed] Box 1 qualifier "probably (likely)" + the High frame
      increase = "probably (likely) results in an important increase in %s",
      decrease = "probably (likely) results in an important reduction in %s",
      # [Box 1] "knee arthroscopy probably has little to no important effect on
      # function" -- Box 1 drops "(likely)" in this one example; it is kept as
      # printed rather than harmonised with the qualifier list.
      little   = "probably has little to no important effect on %s"
    ),
    "Low" = list(
      # [Box 1] "knee arthroscopy may (possibly) result in an important
      # increase in function"
      increase = "may (possibly) result in an important increase in %s",
      # [Table 3] reduction mirror of the same frame
      decrease = "may (possibly) result in an important reduction in %s",
      # [composed] Box 1 qualifier "may (possibly) ... have little to no
      # effect" + the MID object "important effect on %s"
      little   = "may (possibly) have little to no important effect on %s"
    )
  )
)

# Very low spans both threshold columns and is direction neutral.
# Frame arguments: intervention, outcome.
PLAIN_LANGUAGE_VERY_LOW <-
  "We are very uncertain about the effect of %s on %s"

# The Box 1 rendering of the same statement, kept for reference.
PLAIN_LANGUAGE_VERY_LOW_BOX1 <- "the effect of %s on %s is very uncertain"

# Placeholder used when the outcome has no usable label.
PLAIN_LANGUAGE_OUTCOME_FALLBACK <- "the outcome"

# Footnote accompanying the column. Box 1 has no footnote of its own; this
# states what the direction word means, which is the part a reader of a harm
# outcome needs.
PLAIN_LANGUAGE_TABLE_NOTE <- paste0(
  "The direction word (reduces / increases / little to no effect) describes ",
  "the effect of the intervention on the outcome itself, taken from the ",
  "pooled point estimate; it does not say whether that effect is a benefit ",
  "or a harm."
)

# --- Label handling ---------------------------------------------------------

# Normalise a certainty label onto the Box 1 rows. Returns NULL when the label
# is not recognised, so callers can drop the column instead of erroring.
.plain_language_certainty <- function(certainty) {
  if (is.null(certainty) || length(certainty) != 1L || is.na(certainty)) {
    return(NULL)
  }
  switch(
    tolower(as.character(certainty)),
    "high"     = "High",
    "moderate" = "Moderate",
    "low"      = "Low",
    "very low" = "Very low",
    "very_low" = "Very low",
    NULL
  )
}

.plain_language_usable <- function(s) {
  !is.null(s) && length(s) == 1L && !is.na(s) && nzchar(as.character(s))
}

# CG6 writes labels in running prose ("the effect of intensive antileukaemic
# treatment on complete remission"), so a label that is merely sentence-cased
# is lowered before it is dropped into the middle of a sentence. Acronyms and
# proper nouns are left alone: only a first word whose remaining characters are
# all lower case is touched ("Mortality" -> "mortality", but "HbA1c", "CBT-I"
# and "SGLT-2 inhibitors" are kept).
.plain_language_lower_first <- function(s) {
  s <- as.character(s)
  first <- sub("[[:space:]].*$", "", s)
  rest_of_first <- substring(first, 2L)
  if (nzchar(rest_of_first) && rest_of_first != tolower(rest_of_first)) {
    return(s)
  }
  paste0(tolower(substring(s, 1L, 1L)), substring(s, 2L))
}

# Sentence case for the assembled statement (CG6 Table 1 capitalises the first
# word of every plain language cell).
.plain_language_sentence <- function(s) {
  paste0(toupper(substring(s, 1L, 1L)), substring(s, 2L))
}

# The intervention as it appears inside the sentence.
.plain_language_actor <- function(intervention_label) {
  if (!.plain_language_usable(intervention_label)) return("treatment")
  .plain_language_lower_first(intervention_label)
}

# The outcome as it appears inside the sentence. "Outcome" is grade_meta()'s
# own placeholder for an unnamed outcome, so it is treated as absent.
.plain_language_object <- function(outcome_label) {
  if (!.plain_language_usable(outcome_label)) {
    return(PLAIN_LANGUAGE_OUTCOME_FALLBACK)
  }
  lbl <- as.character(outcome_label)
  if (identical(lbl, "Outcome") || identical(lbl, "outcome")) {
    return(PLAIN_LANGUAGE_OUTCOME_FALLBACK)
  }
  .plain_language_lower_first(lbl)
}

# --- Direction --------------------------------------------------------------

# Box 1 chooses between "reduces" and "increases" from the direction of the
# effect, so the direction has to come from the pooled point estimate.
#
# pmatools stores pooled estimates on the TE scale throughout (see
# .derive_rating_target() in rating_target.R: "log scale for ratio effect
# measures, raw scale for MD / SMD / ARD"). On that scale the null is 0 for
# every effect measure, so the sign of TE is the direction:
#
#   TE > 0  -- RR/OR/HR/IRR/RoM > 1, or MD/SMD/RD > 0  -> increases
#   TE < 0  -- RR/OR/HR/IRR/RoM < 1, or MD/SMD/RD < 0  -> reduces
#
# No "higher is better" flag is consulted: Box 1's direction word describes the
# outcome, not its desirability, which is exactly why it reads correctly for
# harm outcomes.
.plain_language_direction_key <- function(direction) {
  if (is.null(direction) || length(direction) != 1L) return(NULL)
  if (is.numeric(direction)) {
    if (!is.finite(direction) || direction == 0) return(NULL)
    return(if (direction > 0) "increase" else "decrease")
  }
  if (is.na(direction)) return(NULL)
  switch(
    tolower(as.character(direction)),
    "increase"  = "increase",
    "increases" = "increase",
    "decrease"  = "decrease",
    "decreases" = "decrease",
    "reduce"    = "decrease",
    "reduces"   = "decrease",
    NULL
  )
}

# Direction of the pooled effect of a meta object, as "increase" / "decrease",
# or NULL when there is no usable pooled estimate.
.plain_language_direction <- function(meta_obj) {
  if (is.null(meta_obj)) return(NULL)
  pooled <- .pooled_estimate(meta_obj)
  .plain_language_direction_key(pooled$est)
}

# --- Lookup -----------------------------------------------------------------

#' Plain language summary for a certainty rating (Core GRADE 6 Box 1)
#'
#' Internal assembly of the Box 1 statements: an intervention label, a
#' certainty qualifier, a direction word taken from the pooled point estimate,
#' and the outcome.
#'
#' @param certainty Certainty label (\code{"High"}, \code{"Moderate"},
#'   \code{"Low"}, \code{"Very Low"}).
#' @param threshold_type \code{"mid"} or \code{"null"}. Box 1 gives separate
#'   examples for the null threshold ("increases function") and the MID
#'   threshold ("results in an important increase in function").
#' @param rating_target One of \code{"important_effect"},
#'   \code{"little_to_no_difference"}, \code{"non_null_effect"}. \code{NULL}
#'   (pmatools objects created before the Core GRADE 2 entry gate) returns
#'   \code{NULL} so callers can omit the column entirely.
#' @param direction Direction of the pooled effect: \code{"increase"},
#'   \code{"decrease"}, or a numeric point estimate on the TE scale (log scale
#'   for ratio measures) whose sign is used. Ignored when
#'   \code{rating_target = "little_to_no_difference"} and for Very low
#'   certainty. \code{NULL} with any other target returns \code{NULL}: Box 1
#'   has no direction-free wording, so the column is dropped rather than
#'   guessed.
#' @param outcome_label Outcome name substituted into the statement. Defaults
#'   to a generic "the outcome".
#' @param intervention_label Intervention name; defaults to "Treatment".
#'
#' @return A single string, or \code{NULL} when no statement applies.
#'
#' @keywords internal
#' @noRd
.plain_language <- function(certainty, threshold_type, rating_target,
                            direction = NULL,
                            outcome_label = NULL,
                            intervention_label = "Treatment") {
  cert <- .plain_language_certainty(certainty)
  if (is.null(cert)) return(NULL)

  # Pre-Phase-A objects carry no rating target: the Box 1 column (null vs MID)
  # cannot be chosen, so the statement is dropped rather than guessed.
  if (is.null(rating_target) || length(rating_target) != 1L ||
      is.na(rating_target)) {
    return(NULL)
  }
  if (is.null(threshold_type) || length(threshold_type) != 1L ||
      is.na(threshold_type) || !threshold_type %in% c("mid", "null")) {
    return(NULL)
  }

  actor  <- .plain_language_actor(intervention_label)
  object <- .plain_language_object(outcome_label)

  if (identical(cert, "Very low")) {
    return(.plain_language_sentence(
      sprintf(PLAIN_LANGUAGE_VERY_LOW, actor, object)))
  }

  dir <- if (identical(rating_target, "little_to_no_difference")) {
    "little"
  } else {
    .plain_language_direction_key(direction)
  }
  if (is.null(dir)) return(NULL)

  frame <- PLAIN_LANGUAGE_FRAMES[[threshold_type]][[cert]][[dir]]
  if (is.null(frame)) return(NULL)

  .plain_language_sentence(paste0(actor, " ", sprintf(frame, object)))
}

# Convenience wrapper taking a pmatools object. The direction comes from the
# object's own pooled estimate and the outcome from its outcome_name, so a
# summary of findings row never has to be told which way the effect points.
.plain_language_for <- function(x, outcome_label = NULL,
                                intervention_label = "Treatment") {
  .plain_language(
    certainty          = x$certainty,
    threshold_type     = x$threshold_type,
    rating_target      = x$rating_target,
    direction          = .plain_language_direction(x$meta),
    outcome_label      = outcome_label %||% x$outcome_name,
    intervention_label = intervention_label
  )
}
