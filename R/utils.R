# utils.R — 共通ユーティリティ

# 3-level system (v0.3+): -1 = "some_concerns", -2 = "serious".
# Legacy names ("some", "very_serious") are accepted via .normalize_grade_level()
# below for backward compatibility.
#
# ==========================================================================
# INTERNAL NAME  <->  CORE GRADE WORDING.  READ THIS BEFORE TOUCHING SIGNS.
# ==========================================================================
# The internal names do NOT match the Core GRADE vocabulary, and the mismatch
# is off by exactly one step, so it is easy to read a domain judgment one level
# too mild or too severe. Core GRADE 1, verbatim: "We characterise limitations
# in each of these domains involved in rating down certainty as not serious;
# serious; very serious; or, rarely, extremely serious."
#
#   internal name    | Core GRADE wording  | levels down
#   -----------------|---------------------|-------------
#   "no"             | not serious         |  0
#   "some_concerns"  | serious             | -1
#   "serious"        | very serious        | -2
#   (not implemented)| extremely serious   | -3
#
# In particular the internal "serious" is the source's VERY SERIOUS (-2), not
# its "serious" (-1). "extremely serious" (-3) has no internal name because
# pmatools does not implement it; the maximum downgrade from any single domain
# is -2.
#
# Legacy aliases: "some" -> "some_concerns", "very_serious" -> "serious"
# ("very_serious" is the honest name for that level and is still accepted).
GRADE_LEVELS <- c("no", "some_concerns", "serious")
GRADE_DOWNGRADE <- c(no = 0, some_concerns = -1, serious = -2)

# Internal level -> Core GRADE wording, for user-facing display.
GRADE_LEVEL_SOURCE_WORDING <- c(
  no            = "not serious",
  some_concerns = "serious",
  serious       = "very serious"
)

# Map legacy / synonym labels to canonical ones.
.normalize_grade_level <- function(x) {
  if (is.null(x)) return(x)
  out <- as.character(x)
  out[out == "some"]         <- "some_concerns"
  out[out == "very_serious"] <- "serious"
  out
}
CERTAINTY_LABELS <- c("Very Low", "Low", "Moderate", "High")
CERTAINTY_SYMBOLS <- c(
  "High"       = "++++",
  "Moderate"   = "+++o",
  "Low"        = "++oo",
  "Very Low"   = "+ooo"
)

# Unicode rendering for SoF flextable / browser HTML (rich output targets)
# Use \u escapes so source is ASCII-safe regardless of file encoding.
CERTAINTY_SYMBOLS_UNICODE <- c(
  "High"       = "\u2295\u2295\u2295\u2295",
  "Moderate"   = "\u2295\u2295\u2295\u25cb",
  "Low"        = "\u2295\u2295\u25cb\u25cb",
  "Very Low"   = "\u2295\u25cb\u25cb\u25cb"
)

# Certainty color palettes (bg + text color pairs)
# pastel: soft backgrounds, colored text — readable on screen and in print
# classic: saturated backgrounds, white text — matches netmetaviz classic palette
CERTAINTY_PALETTES <- list(
  pastel = list(
    "High"     = list(bg = "#d7e8d3", text = "#238b21"),
    "Moderate" = list(bg = "#cccce9", text = "#01008b"),
    "Low"      = list(bg = "#f8edd7", text = "#daa521"),
    "Very Low" = list(bg = "#e8d0d0", text = "#8b0000")
  ),
  classic = list(
    "High"     = list(bg = "#1e8449", text = "#ffffff"),
    "Moderate" = list(bg = "#2471a3", text = "#ffffff"),
    "Low"      = list(bg = "#e67e22", text = "#ffffff"),
    "Very Low" = list(bg = "#c0392b", text = "#ffffff")
  )
)

# スコアを確実性ラベルに変換
score_to_certainty <- function(score) {
  score <- max(1L, min(4L, as.integer(round(score))))
  c(1L, 2L, 3L, 4L) |>
    (\(.) CERTAINTY_LABELS[. == score])()
}

# GRADE 判定の検証 (legacy "some" / "very_serious" も受け入れて正規化する)
validate_grade_level <- function(x, arg = "argument") {
  valid <- c("no", "some", "some_concerns", "serious", "very_serious")
  bad <- setdiff(x, valid)
  if (length(bad) > 0) {
    rlang::abort(
      paste0(arg, " contains invalid GRADE level(s): ", paste(bad, collapse = ", "),
             ". Use one of: 'no', 'some_concerns', 'serious'.")
    )
  }
  invisible(x)
}

# --------------------------------------------------------------------------
# Baseline risk helpers
# --------------------------------------------------------------------------

#' Resolve baseline risk to a single numeric probability
#'
#' @param baseline_risk NULL, a numeric scalar, "simple", or "metaprop"
#' @param meta_obj meta object (used for auto-computation)
#' @param ois_p0 Fallback when baseline_risk is NULL
#' @return Numeric scalar in 0..1 or NULL
#' @keywords internal
#' @noRd
.resolve_baseline_risk <- function(baseline_risk, meta_obj, ois_p0 = NULL) {
  # 1. Explicit numeric
  if (is.numeric(baseline_risk)) {
    if (baseline_risk < 0 || baseline_risk > 1)
      rlang::abort("baseline_risk must be between 0 and 1.")
    return(baseline_risk)
  }
  # 2. "simple" or "metaprop"
  if (is.character(baseline_risk) && baseline_risk %in% c("simple", "metaprop")) {
    return(.compute_control_risk(meta_obj, method = baseline_risk))
  }
  # 3. NULL -> fallback to ois_p0, then simple auto-compute
  if (is.null(baseline_risk)) {
    if (!is.null(ois_p0) && is.numeric(ois_p0)) return(ois_p0)
    return(.compute_control_risk(meta_obj, method = "simple"))
  }
  NULL
}

#' Compute control-arm event rate from a metabin object
#' @param meta_obj A meta object (from metabin).
#' @param method One of "simple" or "metaprop".
#' @keywords internal
#' @noRd
.compute_control_risk <- function(meta_obj, method = "simple") {
  ec <- meta_obj$event.c
  nc <- meta_obj$n.c
  if (is.null(ec) || is.null(nc) || length(nc) == 0 || sum(nc, na.rm = TRUE) == 0) {
    return(NULL)
  }
  ec <- ec[!is.na(ec) & !is.na(nc)]
  nc <- nc[!is.na(nc)]

  if (method == "simple") {
    return(sum(ec) / sum(nc))
  }

  if (method == "metaprop") {
    mp <- tryCatch(
      meta::metaprop(event = ec, n = nc,
                     method = "GLMM", sm = "PLOGIT",
                     method.tau = "ML"),
      error = function(e) NULL
    )
    if (!is.null(mp) && !is.na(mp$TE.random)) {
      return(stats::plogis(mp$TE.random))
    }
    warning("metaprop() failed; falling back to simple pooled proportion.")
    return(sum(ec) / sum(nc))
  }
  NULL
}

# 確実性ドメイン判定をサマリ tibble にまとめる
#
# rationale: free-text justification for a manual override of an automated
# domain judgment (Core GRADE transparency principle). When non-NULL it is
# composed into `notes` as "Manual override (<judgment>): <rationale>",
# prepended with the existing " | " separator style so downstream consumers
# (evidence_profile footnotes via .first_sentence(), grade_report notes
# columns) surface the rationale automatically.
make_domain_row <- function(domain, judgment, auto, notes = NA_character_,
                            rationale = NULL) {
  judgment <- .normalize_grade_level(judgment)
  if (!is.null(rationale)) {
    override_note <- sprintf("Manual override (%s): %s", judgment,
                             trimws(rationale))
    notes <- if (is.na(notes) || !nzchar(notes)) {
      override_note
    } else {
      paste(override_note, notes, sep = " | ")
    }
  }
  tibble::tibble(
    domain    = domain,
    judgment  = judgment,
    downgrade = GRADE_DOWNGRADE[[judgment]],
    auto      = auto,
    notes     = notes
  )
}

# GRADE transparency gate for manual overrides (v0.4.0, breaking change).
# Overriding an automated domain judgment requires a written justification.
# Aborts unless `rationale` is a single non-NA, non-empty, non-whitespace
# string. Returns the rationale invisibly on success. `hint` appends a
# call-site-specific sentence telling the user how to avoid the override
# altogether (used by the Indirectness subdomain path).
.check_override_rationale <- function(rationale, arg, domain_label,
                                      hint = NULL) {
  ok <- is.character(rationale) && length(rationale) == 1L &&
        !is.na(rationale) && nzchar(trimws(rationale))
  if (!ok) {
    msg <- sprintf(
      paste0(
        "Overriding the %s judgment requires %s: state why the automated ",
        "assessment was replaced (Core GRADE transparency principle)."
      ),
      domain_label, arg
    )
    if (!is.null(hint)) msg <- paste(msg, hint)
    rlang::abort(msg)
  }
  invisible(rationale)
}

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

#' Compute sample-size-weighted pooled SD across studies
#'
#' For continuous-outcome meta-analyses (\code{\link[meta]{metacont}}), returns
#' the pooled standard deviation across studies, sample-size weighted.
#'
#' @param meta_obj A meta object (typically from
#'   \code{\link[meta]{metacont}}).
#'
#' @return A single numeric pooled SD, or \code{NULL} if input data are
#'   insufficient.
#'
#' @details
#' Per-study pooled SD uses Cohen's pooled formula:
#' \deqn{SD_{pooled} = \sqrt{\frac{(n_e - 1) SD_e^2 + (n_c - 1) SD_c^2}{n_e + n_c - 2}}}
#' Across studies, the per-study pooled SDs are averaged with weights equal to
#' the total per-study sample size (\eqn{n_e + n_c}).
#'
#' If \code{sd.e}/\code{sd.c} are unavailable, falls back to
#' \code{weighted.mean(seTE * sqrt(n_total), n_total)}.
#'
#' @export
compute_pooled_sd <- function(meta_obj) {
  n_e  <- meta_obj$n.e
  n_c  <- meta_obj$n.c
  sd_e <- meta_obj$sd.e
  sd_c <- meta_obj$sd.c

  if (!is.null(n_e) && !is.null(n_c) && !is.null(sd_e) && !is.null(sd_c) &&
      length(n_e) == length(sd_e)) {
    sd_per_study <- sqrt(
      ((n_e - 1) * sd_e^2 + (n_c - 1) * sd_c^2) /
      pmax(n_e + n_c - 2, 1)
    )
    weights <- n_e + n_c
    keep <- is.finite(sd_per_study) & is.finite(weights) & weights > 0
    if (any(keep)) {
      return(stats::weighted.mean(sd_per_study[keep], weights[keep]))
    }
  }

  # Fallback: derive from seTE
  seTE <- meta_obj$seTE
  if (!is.null(n_e) && !is.null(n_c) && !is.null(seTE)) {
    n_total <- n_e + n_c
    keep <- is.finite(seTE) & is.finite(n_total) & n_total > 0
    if (any(keep)) {
      # MD: SE ≈ sd_pooled * sqrt(1/n_e + 1/n_c) ≈ sd_pooled * sqrt(4/n_total)
      sd_approx <- seTE[keep] * sqrt(n_total[keep] / 4)
      return(stats::weighted.mean(sd_approx, n_total[keep]))
    }
  }

  NULL
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
      "ARD" = "ard",
      "SMD" = "te_scale",
      "MD"  = "te_scale",
      rlang::abort(sprintf(
        "Cannot auto-detect threshold_scale for sm = '%s'. Specify threshold_scale explicitly.", sm))
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
