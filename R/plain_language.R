# plain_language.R — Core GRADE 2 Table 1 plain language statements
#
# References:
#   Guyatt G, Zeng L, Brignardello-Petersen R, et al.
#     Core GRADE 2: choosing the target of certainty rating and assessing
#     imprecision. BMJ. 2025;389:e081904. doi:10.1136/bmj-2024-081904
#     -- Table 1 "GRADE plain language statements when using the null effect
#        or MID thresholds" (p 6 of the published version).
#
# Table 1 is a 2-column grid: certainty (High / Moderate / Low / Very low) x
# threshold ("Null effect as threshold" / "MID as threshold"). Every cell above
# "Very low" offers two alternative statements separated by ", or"; the Very low
# row spans both columns with a single sentence.
#
# The strings below are transcribed VERBATIM from Table 1. Do not paraphrase
# them: the whole point of the table is that the wording is standardised, so a
# reader can map a statement back onto a certainty level and a threshold.
#
# Which of the two alternatives is used:
#   * MID column  -- the alternatives are "an important benefit" vs "little to
#     no benefit", i.e. the two possible targets of the certainty rating
#     (Core GRADE 2 Fig 2). `rating_target` selects between them.
#   * Null column -- the alternatives are two phrasings of the same claim, a
#     generic one ("has a benefit") and an outcome-specific one ("improves
#     outcome X"). `outcome_label` selects the outcome-specific phrasing and
#     substitutes for X.
#   * Null column + rating_target = "little_to_no_difference" -- Core GRADE 2
#     states that with the null as threshold and a point estimate near the
#     null, users "will instead rate certainty in little to no effect". Table 1
#     provides no null-column wording for that case, so the little-to-no
#     wording of the MID column (same certainty row) is used.

# --- Table 1, verbatim -----------------------------------------------------

PLAIN_LANGUAGE_TABLE <- list(
  # "Null effect as threshold"
  null = list(
    "High" = list(
      generic = "Treatment has a benefit",
      outcome = "Treatment improves outcome X"
    ),
    "Moderate" = list(
      generic = "Treatment likely has a benefit",
      outcome = "Treatment likely improves outcome X"
    ),
    "Low" = list(
      generic = "Treatment may have a benefit",
      outcome = "Treatment may improve outcome X"
    )
  ),
  # "MID as threshold"
  mid = list(
    "High" = list(
      important = "Treatment has an important benefit",
      little    = "Treatment has little to no benefit"
    ),
    "Moderate" = list(
      important = "Treatment likely has an important benefit",
      little    = "Treatment likely has little to no benefit"
    ),
    "Low" = list(
      important = "Treatment may have an important benefit",
      little    = "Treatment may have little to no benefit"
    )
  )
)

# Very low spans both threshold columns.
PLAIN_LANGUAGE_VERY_LOW <-
  "We are very uncertain about whether treatment has a benefit"

# Table 1 footnote, verbatim.
PLAIN_LANGUAGE_TABLE_NOTE <- paste0(
  "The plain language summary pertains to both beneficial and harmful ",
  "outcomes. Benefit was chosen here for illustration."
)

# --- Lookup ----------------------------------------------------------------

# Normalise a certainty label onto the Table 1 row names. Returns NULL when the
# label is not recognised, so callers can drop the column instead of erroring.
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

# Swap the placeholders of Table 1 ("Treatment", "outcome X") for the labels of
# the analysis at hand. The default intervention label reproduces Table 1
# exactly.
.plain_language_relabel <- function(s, intervention_label = "Treatment",
                                    outcome_label = NULL) {
  if (!is.null(outcome_label) && length(outcome_label) == 1L &&
      !is.na(outcome_label) && nzchar(outcome_label)) {
    s <- sub("outcome X", outcome_label, s, fixed = TRUE)
  }
  if (!is.null(intervention_label) && length(intervention_label) == 1L &&
      !is.na(intervention_label) && nzchar(intervention_label) &&
      !identical(intervention_label, "Treatment")) {
    s <- sub("^Treatment", intervention_label, s)
    s <- sub("whether treatment", paste0("whether ", intervention_label), s,
             fixed = TRUE)
  }
  s
}

#' Plain language statement for a certainty rating (Core GRADE 2 Table 1)
#'
#' Internal lookup into the verbatim Table 1 grid.
#'
#' @param certainty Certainty label (\code{"High"}, \code{"Moderate"},
#'   \code{"Low"}, \code{"Very Low"}).
#' @param threshold_type \code{"mid"} or \code{"null"}.
#' @param rating_target One of \code{"important_effect"},
#'   \code{"little_to_no_difference"}, \code{"non_null_effect"}. \code{NULL}
#'   (pmatools objects created before the Core GRADE 2 entry gate) returns
#'   \code{NULL} so callers can omit the column entirely.
#' @param outcome_label Optional outcome name substituted for the "outcome X"
#'   placeholder of the null-threshold column.
#' @param intervention_label Label substituted for the "Treatment" placeholder.
#'
#' @return A single string, or \code{NULL} when no statement applies.
#'
#' @keywords internal
#' @noRd
.plain_language <- function(certainty, threshold_type, rating_target,
                            outcome_label = NULL,
                            intervention_label = "Treatment") {
  cert <- .plain_language_certainty(certainty)
  if (is.null(cert)) return(NULL)

  # Pre-Phase-A objects carry no rating target: the statement cannot be chosen
  # from Table 1, so the column is dropped rather than guessed.
  if (is.null(rating_target) || length(rating_target) != 1L ||
      is.na(rating_target)) {
    return(NULL)
  }
  if (is.null(threshold_type) || length(threshold_type) != 1L ||
      is.na(threshold_type) || !threshold_type %in% c("mid", "null")) {
    return(NULL)
  }

  if (identical(cert, "Very low")) {
    return(.plain_language_relabel(PLAIN_LANGUAGE_VERY_LOW,
                                   intervention_label, outcome_label))
  }

  little <- identical(rating_target, "little_to_no_difference")

  s <- if (identical(threshold_type, "mid")) {
    if (little) {
      PLAIN_LANGUAGE_TABLE$mid[[cert]]$little
    } else {
      PLAIN_LANGUAGE_TABLE$mid[[cert]]$important
    }
  } else if (little) {
    # No null-column little-to-no wording in Table 1; borrow the MID column.
    PLAIN_LANGUAGE_TABLE$mid[[cert]]$little
  } else if (!is.null(outcome_label) && length(outcome_label) == 1L &&
             !is.na(outcome_label) && nzchar(outcome_label)) {
    PLAIN_LANGUAGE_TABLE$null[[cert]]$outcome
  } else {
    PLAIN_LANGUAGE_TABLE$null[[cert]]$generic
  }

  .plain_language_relabel(s, intervention_label, outcome_label)
}

# Convenience wrapper taking a pmatools object.
.plain_language_for <- function(x, outcome_label = NULL,
                                intervention_label = "Treatment") {
  .plain_language(
    certainty          = x$certainty,
    threshold_type     = x$threshold_type,
    rating_target      = x$rating_target,
    outcome_label      = outcome_label,
    intervention_label = intervention_label
  )
}
