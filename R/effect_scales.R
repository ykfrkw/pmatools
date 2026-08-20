# effect_scales.R - the same effect, restated on another scale
#
# Split out of utils.R. An SMD read as an odds ratio (Chinn's formula), a
# placeholder threshold per effect measure, and the translation of the user's
# threshold onto the scale meta stores TE on -- including the absolute-to-ratio
# conversion, which is the one that needs a baseline risk. Every function here
# answers "the same quantity, expressed how?", and none of them judges anything.
#
# A new helper belongs here when it restates a number on a different scale. The
# baseline risk the absolute-to-ratio conversion runs through is resolved in
# R/meta_quantities.R.

# --------------------------------------------------------------------------
# Chinn's formula: SMD <-> log(OR) conversion
# --------------------------------------------------------------------------

#' Convert SMD to OR (Chinn's formula)
#'
#' Convert a standardized mean difference (SMD) and optionally its CI bounds to
#' an odds ratio (OR) using Chinn's formula: \eqn{\log(OR) = SMD \times \pi /
#' \sqrt{3}}. The conversion assumes a logistic latent-variable distribution
#' (Cox 1970; Hasselblad & Hedges 1995; Chinn 2000).
#'
#' @section Relation to Core GRADE 6 "option 2":
#' Core GRADE 6 also converts a continuous outcome to a binary one, but by a
#' \strong{different method}, and the two must not be conflated. Core GRADE 6
#' option 2 works from the MID, verbatim: "If systematic reviewers or guideline
#' developers know what the MID is for each of the instruments and assume a
#' normal distribution of results, they can calculate the proportion of people
#' who experience an improvement larger than the MID within each arm, thereby
#' obtaining a risk ratio or risk difference for each of the studies. They can
#' then pool these proportions across studies." That is a
#' normal-distribution-plus-MID calculation done \emph{per study, before
#' pooling}.
#'
#' Chinn's formula instead assumes a \emph{logistic} latent variable, needs no
#' MID, and is applied \emph{after} pooling, to the summary SMD. It answers a
#' different question and will not in general reproduce the option 2 numbers.
#' Core GRADE 6's option 2 is not implemented in pmatools.
#'
#' @param smd Numeric. Standardized mean difference (effect size).
#' @param ci_lower,ci_upper Optional numeric CI bounds on the SMD scale.
#'
#' @return A list with elements \code{or}, \code{or_lower}, \code{or_upper},
#'   and \code{factor} (the \eqn{\pi / \sqrt{3}} multiplier). NA inputs
#'   propagate to NA outputs.
#'
#' @references
#' Chinn S. A simple method for converting an odds ratio to effect size for use
#' in meta-analysis. Stat Med. 2000;19(22):3127-3131.
#'
#' @examples
#' chinn_smd_to_or(-0.5)
#' chinn_smd_to_or(-0.5, ci_lower = -0.7, ci_upper = -0.3)
#'
#' @export
chinn_smd_to_or <- function(smd, ci_lower = NULL, ci_upper = NULL) {
  factor <- pi / sqrt(3)
  list(
    or       = exp(smd * factor),
    or_lower = if (!is.null(ci_lower)) exp(ci_lower * factor) else NA_real_,
    or_upper = if (!is.null(ci_upper)) exp(ci_upper * factor) else NA_real_,
    factor   = factor
  )
}

# --------------------------------------------------------------------------
# Threshold auto-default per effect measure
# --------------------------------------------------------------------------

#' Suggest a placeholder Threshold based on the effect measure
#'
#' Returns a placeholder clinical decision Threshold (a minimally important
#' effect on the analysis scale) suitable for pre-filling the input field in
#' interactive UIs. \strong{These are pmatools conventions, not Core GRADE
#' values}, with one partial exception (SMD; see below). Replace them with a
#' published or expert-derived MID for the outcome in hand before reporting
#' anything.
#'
#' @param meta_obj A meta object (from \code{\link[meta]{metabin}} or
#'   \code{\link[meta]{metacont}}).
#'
#' @return A list with \code{threshold_user} (user-facing value),
#'   \code{threshold_scale} (one of \code{"ratio"}, \code{"te_scale"},
#'   \code{"ard"}) and \code{source} (\code{"core_grade_6"} or
#'   \code{"package_convention"} — where the number comes from).
#'
#'   For binary ratio measures (OR / RR / HR) the \strong{first candidate is
#'   the absolute one}: \code{threshold_user} / \code{threshold_scale} describe
#'   an absolute risk difference of 0.05 (50 per 1,000), the same list is
#'   repeated under \code{threshold_absolute}, and the ratio-scale fallback is
#'   available under \code{threshold_ratio}. This ordering follows the source:
#'   Core GRADE 1, 6 and 7 contain no ratio-scale MID at all, and every binary
#'   MID they discuss is on the absolute scale (e.g. Core GRADE 7 lists MIDs
#'   "associated with mortality of 1\%, stroke of 2\%, myocardial infarction of
#'   3\%, and serious gastrointestinal bleeding of 5\%"; Core GRADE 2 discusses
#'   "an MID of 5 deaths per" 1000).
#'
#'   Returns \code{NULL} if the effect measure is unrecognized.
#'
#' @section Where these numbers come from:
#' \describe{
#'   \item{SMD 0.20 (\code{source = "core_grade_6"})}{The only default with a
#'     source. Core GRADE 6 does cite it — "an SMD of 0.2 is the threshold for
#'     a small and important effect" — but immediately qualifies it, verbatim:
#'     "clinicians may be appropriately sceptical of this threshold, which is
#'     limited by large variability in the methods investigators use to
#'     calculate the SMD".}
#'   \item{Everything else (\code{source = "package_convention"})}{OR 1.25,
#'     RR 1.20, HR 1.20, RoM 1.10, MD 0.20 \eqn{\times} pooled SD and ARD 0.05
#'     have \strong{no basis in the Core GRADE series}. They exist only so that
#'     an input field can be pre-filled.}
#' }
#'
#' @section Why a single default conflicts with Core GRADE:
#' \itemize{
#'   \item \strong{No ratio-scale MIDs exist in the source.} Core GRADE 1, 6
#'     and 7 give no example of a MID on a ratio scale; binary MIDs are always
#'     absolute (per 1000 or percent). A ratio-scale default is therefore an
#'     extrapolation by pmatools.
#'   \item \strong{The MID belongs to the outcome, not to the effect measure.}
#'     Core GRADE 7, verbatim: "MIDs associated with mortality of 1\%, stroke of
#'     2\%, myocardial infarction of 3\%, and serious gastrointestinal bleeding
#'     of 5\% reflect the gradient of importance across these outcomes." One
#'     default shared by every outcome erases exactly that gradient.
#'   \item \strong{The procedure runs the other way round.} Core GRADE 7 has
#'     users look at the CI first and establish a MID only where the answer
#'     depends on it ("whether the MID for mortality is 2\%, 1\%, or less than
#'     1\%, the CI does not cross the MID threshold ... one need not specify a
#'     single particular value"). Starting from a pre-filled default inverts
#'     that order.
#' }
#'
#' @examples
#' \dontrun{
#' s <- suggest_threshold(m)
#' s$threshold_user   # absolute risk difference for binary outcomes
#' s$source           # "package_convention" -> replace it
#' s$threshold_ratio  # ratio-scale fallback, binary outcomes only
#' }
#'
#' @export
suggest_threshold <- function(meta_obj) {
  sm <- meta_obj$sm
  if (is.null(sm)) return(NULL)

  ard_suggest <- list(threshold_user = 0.05, threshold_scale = "ard",
                      source = "package_convention")

  # Binary ratio measures: the absolute suggestion leads (see @return), with
  # the ratio-scale value kept as a secondary candidate.
  binary_ratio <- function(ratio_value) {
    c(ard_suggest,
      list(
        threshold_absolute = ard_suggest,
        threshold_ratio    = list(threshold_user  = ratio_value,
                                  threshold_scale = "ratio",
                                  source          = "package_convention")
      ))
  }

  switch(sm,
    "OR"  = binary_ratio(1.25),
    "RR"  = binary_ratio(1.20),
    "HR"  = binary_ratio(1.20),
    "RoM" = list(threshold_user = 1.10, threshold_scale = "ratio",
                 source = "package_convention"),
    # "RD" is what metabin() emits for a risk difference; "ARD" is the internal
    # scale name, accepted here so a hand-built list is not silently rejected.
    "RD"  = ard_suggest,
    "ARD" = ard_suggest,
    "SMD" = list(threshold_user = 0.20, threshold_scale = "te_scale",
                 source = "core_grade_6"),
    "MD"  = {
      sd_pooled <- compute_pooled_sd(meta_obj)
      if (is.null(sd_pooled) || is.na(sd_pooled) || sd_pooled <= 0) {
        return(NULL)
      }
      list(threshold_user = 0.20 * sd_pooled, threshold_scale = "te_scale",
           source = "package_convention")
    },
    NULL
  )
}

#' Convert a user-supplied Threshold to the meta TE scale
#'
#' Internal helper. Translates the user's Threshold input (with its declared
#' scale) into a value on the same scale as \code{meta_obj$TE}. Used by the
#' Inconsistency and Imprecision domains to anchor judgments to a clinical
#' decision Threshold.
#'
#' @param threshold Numeric Threshold value.
#' @param threshold_scale One of \code{"auto"}, \code{"te_scale"},
#'   \code{"ratio"}, or \code{"ard"}.
#' @param sm The effect measure from \code{meta_obj$sm}, used when
#'   \code{threshold_scale = "auto"} and to decide whether an
#'   \code{"ard"} Threshold needs conversion to the ratio scale.
#' @param threshold_baseline Optional baseline (control-arm) risk as a
#'   proportion in (0, 1). Only used when \code{threshold_scale = "ard"} and
#'   \code{sm} is a ratio measure (OR / RR / HR / RoM); see Details.
#' @param meta_obj Optional meta object. When \code{threshold_baseline} is
#'   \code{NULL}, the pooled control event rate
#'   (\eqn{\sum event_c / \sum n_c}) is used as the baseline risk fallback.
#'
#' @return A list with:
#'   \describe{
#'     \item{threshold_internal}{Numeric on the TE scale (log scale for ratio
#'       measures).}
#'     \item{threshold_kind}{The resolved scale (useful for downstream
#'       branching like ARD-vs-ratio in OIS).}
#'     \item{threshold_ard}{The raw absolute risk difference. Non-\code{NULL}
#'       only when an \code{"ard"} Threshold was converted to the ratio scale.}
#'     \item{threshold_note}{Human-readable conversion note (eg,
#'       \code{"Absolute threshold 50 per 1000 at baseline risk 180 per 1000
#'       (equivalent RR 1.28)"}). Non-\code{NULL} only on ARD-to-ratio
#'       conversion.}
#'     \item{threshold_baseline}{The baseline risk actually used for the
#'       conversion. Non-\code{NULL} only on ARD-to-ratio conversion.}
#'   }
#'
#' @details
#' When \code{threshold_scale = "ard"} and \code{sm} is a ratio measure, the
#' ARD Threshold is converted to an equivalent ratio at the baseline risk
#' \eqn{p_0} (from \code{threshold_baseline}, else the pooled control event
#' rate of \code{meta_obj}; an error is raised if neither is available):
#' \itemize{
#'   \item RR: \eqn{T = (p_0 + ARD) / p_0}
#'   \item OR: \eqn{T = odds(p_0 + ARD) / odds(p_0)} with
#'     \eqn{odds(p) = p / (1 - p)}
#'   \item HR / RoM: approximated by the RR formula. Caveat: the RR
#'     approximation for HR is accurate only for low event rates / short
#'     follow-up; interpret with care.
#' }
#' \code{threshold_internal} is then \eqn{\log T}. For non-ratio effect
#' measures, \code{threshold_scale = "ard"} keeps the previous pass-through
#' behaviour (\code{threshold_internal = threshold}).
#'
#' @keywords internal
threshold_to_te_scale <- function(threshold, threshold_scale = "auto", sm = NULL,
                                  threshold_baseline = NULL, meta_obj = NULL) {
  if (is.null(threshold) || is.na(threshold)) {
    return(list(threshold_internal = NULL, threshold_kind = NULL,
                threshold_ard = NULL, threshold_note = NULL,
                threshold_baseline = NULL))
  }

  if (!is.numeric(threshold) || length(threshold) != 1) {
    rlang::abort("threshold must be a single numeric value or NULL.")
  }

  scale <- if (identical(threshold_scale, "auto")) {
    if (is.null(sm)) {
      rlang::abort("threshold_scale = 'auto' requires meta_obj$sm to be set.")
    }
    switch(sm,
      "OR"  = "ratio",
      "RR"  = "ratio",
      "HR"  = "ratio",
      "RoM" = "ratio",
      "RD"  = "ard",
      "ARD" = "ard",
      "SMD" = "te_scale",
      "MD"  = "te_scale",
      rlang::abort(sprintf(
        paste0("Cannot auto-detect threshold_scale for sm = '%s'. ",
               "Specify threshold_scale explicitly."), sm))
    )
  } else {
    threshold_scale
  }

  if (!scale %in% c("te_scale", "ratio", "ard")) {
    rlang::abort("threshold_scale must be one of 'auto', 'te_scale', 'ratio', 'ard'.")
  }

  # ARD Threshold with a ratio effect measure: convert to the ratio scale at
  # the baseline risk (previously a silent pass-through, which compared a raw
  # ARD against log-ratio TEs).
  if (scale == "ard" && !is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM")) {
    return(.ard_threshold_to_ratio(threshold, sm, threshold_baseline, meta_obj))
  }

  threshold_internal <- switch(scale,
    "te_scale" = threshold,
    "ratio"    = log(threshold),
    "ard"      = threshold
  )

  list(threshold_internal = threshold_internal, threshold_kind = scale,
       threshold_ard = NULL, threshold_note = NULL, threshold_baseline = NULL)
}

#' Convert an absolute risk difference Threshold to the log-ratio scale
#'
#' @param ard Positive absolute risk difference (proportion, eg 0.05).
#' @param sm Ratio effect measure ("OR", "RR", "HR", "RoM").
#' @param threshold_baseline Baseline (control-arm) risk in (0, 1) or NULL.
#' @param meta_obj meta object used for the pooled-CER fallback, or NULL.
#' @return Same list structure as \code{threshold_to_te_scale()}.
#' @keywords internal
#' @noRd
.ard_threshold_to_ratio <- function(ard, sm, threshold_baseline = NULL,
                                    meta_obj = NULL) {
  if (!is.finite(ard) || ard <= 0) {
    rlang::abort(paste0(
      "threshold_scale = 'ard' with sm = '", sm, "' requires a positive ",
      "absolute risk difference expressed as a proportion ",
      "(e.g., 0.05 for 50 per 1,000)."
    ))
  }

  # Resolve baseline risk: explicit threshold_baseline > pooled control event
  # rate from the meta object > actionable error.
  p0 <- NULL
  if (!is.null(threshold_baseline)) {
    if (!is.numeric(threshold_baseline) || length(threshold_baseline) != 1 ||
        !is.finite(threshold_baseline) ||
        threshold_baseline <= 0 || threshold_baseline >= 1) {
      rlang::abort(paste0(
        "threshold_baseline must be a single control-arm risk strictly ",
        "between 0 and 1 (e.g., 0.18 for 180 per 1,000)."
      ))
    }
    p0 <- threshold_baseline
  } else if (!is.null(meta_obj)) {
    cer <- tryCatch(.compute_control_risk(meta_obj, method = "simple"),
                    error = function(e) NULL)
    if (!is.null(cer) && is.finite(cer) && cer > 0 && cer < 1) {
      p0 <- cer
    }
  }
  if (is.null(p0)) {
    rlang::abort(paste0(
      "An absolute (ARD) threshold with sm = '", sm, "' requires a baseline ",
      "(control-arm) risk to convert it to the ratio scale. Supply ",
      "threshold_baseline (a proportion in (0, 1), e.g., 0.18 for 180 per ",
      "1,000), or use a meta-analysis with control-arm event data ",
      "(event.c / n.c) so the pooled control event rate can be used."
    ))
  }

  p1 <- p0 + ard
  if (p1 >= 1) {
    rlang::abort(sprintf(paste0(
      "threshold (ARD = %g) plus baseline risk (%g) implies an event rate ",
      ">= 1 (%g). Use a smaller ARD threshold or baseline risk."),
      ard, p0, p1
    ))
  }

  t_ratio <- if (identical(sm, "OR")) {
    (p1 / (1 - p1)) / (p0 / (1 - p0))
  } else {
    # RR exact; HR and RoM approximated as RR (see Details / caveat).
    p1 / p0
  }

  approx_str <- if (sm %in% c("HR", "RoM")) {
    sprintf("; %s approximated as RR", sm)
  } else {
    ""
  }
  note <- sprintf(
    "Absolute threshold %g per 1000 at baseline risk %g per 1000 (equivalent %s %.2f%s)",
    1000 * ard, 1000 * p0, sm, t_ratio, approx_str
  )

  list(
    threshold_internal = log(t_ratio),
    threshold_kind     = "ard",
    threshold_ard      = ard,
    threshold_note     = note,
    threshold_baseline = p0
  )
}
