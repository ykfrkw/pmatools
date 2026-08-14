# pubias_trimfill.R - trim-and-fill as a diagnostic the reviewer reads
#
# Purpose : compare the original pooled effect with the trim-and-fill adjusted
#           one and say whether the original is exaggerated in the direction
#           that favours the intervention.
# Inputs  : two pooled effects on the internal (log, for ratio measures) scale,
#           and the outcome's small_values direction.
# Outputs : a list of flags for a caller to branch on, and one sentence to
#           print. Nothing here reaches a GRADE judgment.
# Depends : PMA_ROB_INFLATION_THRESHOLD (R/domain_rob.R), rlang.

# --------------------------------------------------------------------------
# Trim-and-fill exaggeration check -- a DISPLAY, never a decision
#
# Same shape as the risk-of-bias direction check (.assess_bias_direction() in
# domain_rob.R): take the estimate that may be exaggerated, take the comparator
# that is meant to be free of the bias, and ask whether the first sits more
# than a fifth further in the direction that flatters the intervention. Only
# the comparator differs. Risk of bias compares the whole body against its low
# risk of bias subset; here the ORIGINAL pooled effect is the possibly
# exaggerated side and the TRIM-AND-FILL ADJUSTED effect is the reference,
# because trim-and-fill is what is supposed to have removed the bias. Getting
# those two the wrong way round would report exaggeration exactly when there is
# none.
#
# This rates nothing, and must not start to. assess_pubias() implements
# Core GRADE 4 Fig 5, which has no trim-and-fill node at all; what the figure
# asks (Q3) is whether asymmetry "strongly suggests publication bias", and that
# is a reviewer's judgment. This function supplies one of the numbers that
# judgment is made on, next to the trim-and-fill funnel in the Shiny app.
#
# Why it shares PMA_ROB_INFLATION_THRESHOLD instead of declaring its own 0.20:
# the two checks ask one question -- "is the favourable direction exaggerated
# by more than a fifth?" -- and a reviewer reads their answers on adjacent
# tabs, so two constants that merely happened to hold the same number would
# mislead the first time one of them moved. What is deliberately NOT shared is
# the per-analysis knob: grade_meta(rob_inflation_threshold = ) tunes a RATING,
# and letting it reach here would let a rating parameter move a display that
# rates nothing.
#
# Rule 1 of the risk-of-bias check -- both estimates inside the trivial zone,
# so no percentage between them means anything -- has no counterpart here,
# because this comparison is not given a threshold. A pair of near-null
# estimates can therefore report a large percentage; .pubias_trimfill_line()
# prints both estimates beside it so the reader can see that is what happened.
# --------------------------------------------------------------------------

# Below this, |TE_adjusted| is treated as zero and the ratio is undefined.
# Same guard, and the same value, as .assess_bias_direction().
.PUBIAS_TRIMFILL_NEAR_ZERO <- 1e-9

# Is the ORIGINAL pooled effect exaggerated, relative to the trim-and-fill
# adjusted effect, in the direction that favours the intervention?
#
# Returns a list, always with the same names:
#   assessable   both estimates finite and |te_adjusted| above the zero guard
#   ratio        (|te_original| - |te_adjusted|) / |te_adjusted|, else NA
#   favourable   TRUE when the shift from adjusted to original is toward the
#                side small_values calls desirable (NA when not assessable)
#   exaggerated  favourable AND ratio > threshold
#   threshold    the threshold actually applied, for the caller to print
.pubias_trimfill_inflation <- function(te_original, te_adjusted,
                                       small_values = NULL,
                                       threshold = PMA_ROB_INFLATION_THRESHOLD) {
  unassessable <- list(assessable = FALSE, ratio = NA_real_,
                       favourable = NA, exaggerated = FALSE,
                       threshold = threshold)

  if (length(te_original) != 1L || length(te_adjusted) != 1L) {
    return(unassessable)
  }
  te_original <- as.numeric(te_original)
  te_adjusted <- as.numeric(te_adjusted)
  if (!is.finite(te_original) || !is.finite(te_adjusted)) return(unassessable)
  if (abs(te_adjusted) <= .PUBIAS_TRIMFILL_NEAR_ZERO) return(unassessable)

  if (!is.null(small_values) &&
      !identical(small_values, "desirable") &&
      !identical(small_values, "undesirable")) {
    rlang::abort(
      "small_values must be 'desirable', 'undesirable' or NULL.")
  }

  # Which way is "flattering". Read straight off .assess_bias_direction():
  # small values desirable means the desirable side is downward, so an
  # exaggerating shift moves the original estimate BELOW the adjusted one.
  # With no small_values the direction is unknown and only the magnitude can
  # be compared, exactly as the risk-of-bias check does.
  favourable <- if (is.null(small_values)) {
    abs(te_original) > abs(te_adjusted)
  } else if (identical(small_values, "undesirable")) {
    te_original > te_adjusted
  } else {
    te_original < te_adjusted
  }

  ratio <- (abs(te_original) - abs(te_adjusted)) / abs(te_adjusted)

  list(assessable  = TRUE,
       ratio       = ratio,
       favourable  = favourable,
       exaggerated = isTRUE(favourable) && is.finite(ratio) &&
                     ratio > threshold,
       threshold   = threshold)
}

# The sentence the app prints under the trim-and-fill funnel. Kept here rather
# than in the app so the wording is unit-tested alongside the arithmetic it
# describes, and so it cannot start claiming the diagnostic rated something.
#
# `format_te` turns an internal-scale effect into whatever the caller wants the
# reader to see (exponentiated for a ratio measure, say); the default prints
# the internal value.
.pubias_trimfill_line <- function(inflation, te_original, te_adjusted,
                                  format_te = function(v) sprintf("%.3f", v)) {
  if (!isTRUE(inflation$assessable)) {
    return(paste0(
      "Exaggeration check not assessable: the original and adjusted pooled ",
      "effects are not both usable, or the adjusted effect is zero."))
  }

  verdict <- if (isTRUE(inflation$exaggerated)) {
    sprintf(paste0(
      "the original estimate is %.0f%% larger in the direction that favours ",
      "the intervention, above the %.0f%% mark"),
      100 * inflation$ratio, 100 * inflation$threshold)
  } else if (isTRUE(inflation$favourable)) {
    sprintf(paste0(
      "the original estimate is %.0f%% larger in the direction that favours ",
      "the intervention, within the %.0f%% mark"),
      100 * inflation$ratio, 100 * inflation$threshold)
  } else {
    "the adjustment does not move the estimate away from the favourable side"
  }

  sprintf(paste0(
    "Exaggeration check (reference only, rates nothing): original %s vs ",
    "trim-and-fill adjusted %s - %s."),
    format_te(te_original), format_te(te_adjusted), verdict)
}
