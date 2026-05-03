# grade_meta.R — メイン GRADE 評価関数
#
# BMJ 2025 Core GRADE シリーズに準拠した確実性評価を {meta} オブジェクトから実施する。

#' Assess GRADE certainty of evidence from a meta-analysis object
#'
#' @description
#' Run the BMJ 2025 Core GRADE assessment on a meta-analysis object and
#' return per-domain judgments and final certainty.
#'
#' @param meta_obj A meta-analysis object from the \{meta\} package
#'   (\code{metabin}, \code{metacont}, \code{metagen}, etc.).
#' @param study_design Starting certainty. \code{"RCT"} (default, starts at High)
#'   or \code{"obs"} (observational, starts at Low).
#' @param rob Risk of bias judgment. One of:
#'   \itemize{
#'     \item A scalar string: \code{"no"}, \code{"some"}, \code{"serious"}, \code{"very_serious"}
#'       — used as-is (flowchart not applied).
#'     \item A character vector of length k (one per study) — BMJ 2025 Core GRADE 4
#'       Fig 2 flowchart applied: domination check then direction-of-bias check.
#'     \item A column name in \code{meta_obj$data} containing per-study RoB judgments
#'       — same flowchart logic applied.
#'     \item \code{NULL} (default): treated as \code{"no"}.
#'   }
#' @param rob_dominant_threshold Deprecated (v0.3.1+; accepted but ignored).
#'   The Risk-of-Bias flowchart no longer uses a weight-share dominance gate;
#'   the direction-and-magnitude check is now run whenever at least one
#'   high-RoB study is present.
#' @param rob_inflation_threshold (v0.2) Minimum *relative* inflation
#'   \eqn{(|TE_{all}| - |TE_{low}|) / |TE_{low}|} required to rate down for
#'   risk of bias when the evidence is dominated. Default \code{0.10}
#'   (10 percent). Set to \code{0} to restore v0.1.0 behaviour (any direction-
#'   consistent change rates down). Only used when \code{rob} is a vector
#'   or column name.
#' @param small_values Are small outcome values desirable?
#'   \code{"desirable"} if small values are good (eg, mortality, symptom severity) or
#'   \code{"undesirable"} if small values are bad (eg, response rate, remission OR > 1).
#'   Used to automatically determine whether dominated high-RoB studies inflate the
#'   apparent effect. If \code{NULL} (default), a conservative rate-down is applied
#'   when dominated. Only used when \code{rob} is a vector or column name.
#'   (Consistent with \code{netmetaviz} \code{small_values} parameter.)
#' @param indirectness Indirectness judgment. Same format as \code{rob} (scalar/vector/column).
#'   Default \code{"no"}.
#' @param inconsistency Overall inconsistency scalar judgment. One of
#'   \code{"no"}, \code{"some"}, \code{"serious"}, \code{"very_serious"}.
#'   If provided, flowchart parameters are ignored.
#' @param inconsistency_ci_diff \code{"yes"} / \code{"no"}: Are there important
#'   differences in point estimates AND limited CI overlap? (BMJ Core GRADE 3 Fig 2
#'   Step 1). Required for flowchart; if NULL, falls back to I^2-based assessment.
#' @param inconsistency_threshold_side Required when \code{inconsistency_ci_diff = "yes"}.
#'   \code{"majority_one_side"} or \code{"opposite_sides"}: are most estimates on one
#'   side of the clinical threshold, or spread across both sides?
#' @param inconsistency_subgroup_explained Required when
#'   \code{inconsistency_threshold_side = "opposite_sides"}.
#'   \code{"yes"} / \code{"no"}: is the inconsistency explained by a credible subgroup?
#' @param mid (v0.2) Numeric minimally important difference. Used as the
#'   clinical decision threshold in Inconsistency Step 2 (3-zone
#'   classification of point estimates around \eqn{\pm}MID), and also
#'   feeds the OIS calculation in Imprecision when explicit OIS values
#'   are not supplied. The same MID is used for both domains.
#' @param mid_scale (v0.2) How to interpret \code{mid}. One of:
#'   \itemize{
#'     \item \code{"auto"} (default): infer from \code{meta_obj$sm}
#'       (OR/RR/HR/RoM \eqn{\to} ratio scale; SMD/MD \eqn{\to} TE scale;
#'       ARD \eqn{\to} ARD scale).
#'     \item \code{"te_scale"}: \code{mid} is already on the same scale as
#'       \code{meta_obj$TE} (log scale for ratios, raw for MD/SMD).
#'     \item \code{"ratio"}: \code{mid} is supplied as a ratio (e.g.,
#'       \code{1.25} for a 25 percent relative effect); internally
#'       converted to \code{log(mid)}.
#'     \item \code{"ard"}: \code{mid} is an absolute risk difference
#'       (binary outcomes only).
#'   }
#' @param outcome_name Optional label for the outcome (used in SoF table).
#' @param outcome_type \code{"relative"} (RR/OR/HR, null = 1) or
#'   \code{"absolute"} (MD/SMD, null = 0). Default \code{"relative"}.
#' @param ois_events For binary outcomes: target number of events for the
#'   Optimal Information Size (OIS). Takes precedence over auto-calculated OIS.
#' @param ois_n For continuous outcomes: target total sample size for OIS.
#'   Takes precedence over auto-calculated OIS.
#' @param ois_alpha Type I error rate for OIS calculation (default 0.05, two-sided).
#' @param ois_beta Type II error rate for OIS calculation (default 0.20, ie 80 percent power).
#' @param ois_p0 For binary outcomes: baseline (control) event rate for OIS calculation.
#'   Used with \code{ois_p1} to auto-compute target events.
#' @param ois_p1 For binary outcomes: experimental arm event rate for OIS calculation.
#' @param ois_delta For continuous outcomes: minimally important difference for OIS
#'   calculation. Used with \code{ois_sd}.
#' @param ois_sd For continuous outcomes: pooled SD for OIS calculation.
#' @param baseline_risk Baseline (control-arm) event rate used for computing
#'   absolute risk differences (ARD per 1,000) in the SoF table. Accepts:
#'   \itemize{
#'     \item A numeric scalar in \code{[0, 1]}: used directly.
#'     \item \code{"simple"}: pooled control-arm proportion
#'       (\eqn{\sum events_c / \sum n_c}).
#'     \item \code{"metaprop"}: GLMM-pooled proportion via
#'       \code{meta::metaprop()} (logit back-transform); falls back to simple
#'       if convergence fails.
#'     \item \code{NULL} (default): uses \code{ois_p0} if supplied, otherwise
#'       auto-computes via \code{"simple"} for binary outcomes.
#'   }
#'   Only meaningful for binary outcomes with a relative effect measure
#'   (RR, OR, HR, IRR). Set to \code{NULL} explicitly to suppress ARD display.
#'
#' @param pubias_small_industry \code{"yes"} / \code{"no"}: Are most or all studies
#'   small AND industry-sponsored? (BMJ Core GRADE Fig 5 Step 1). Default \code{NULL}
#'   (treated as \code{"no"}).
#' @param pubias_funnel_asymmetry \code{"yes"} / \code{"no"}: Does visual funnel plot
#'   asymmetry and/or statistical test strongly suggest publication bias?
#'   Only used when k \eqn{\geq} 10. If \code{NULL} (default), Egger's test is
#'   run automatically.
#' @param pubias_unpublished \code{"yes"} / \code{"no"}: Is there documentation of
#'   unpublished studies (eg, in trial registry or FDA)? Only used when k < 10.
#'   If \code{NULL} (default), assumed \code{"no"} with a warning.
#'
#' @return An S3 object of class \code{pmatools} containing:
#'   \describe{
#'     \item{domain_assessments}{A tibble with one row per GRADE domain.}
#'     \item{certainty}{Final certainty label: "High", "Moderate", "Low", or "Very Low".}
#'     \item{certainty_score}{Numeric score (1–4).}
#'     \item{starting_quality}{Starting certainty label.}
#'     \item{outcome_name}{Outcome label.}
#'     \item{meta}{The original meta object.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(meta)
#' m <- metabin(Ee, Ne, Ec, Nc, studlab = study, data = Olkin1995, sm = "RR")
#' g <- grade_meta(m, study_design = "RCT", rob = "some", outcome_name = "Mortality")
#' print(g)
#' sof_table(g)
#' }
#'
#' @export
grade_meta <- function(meta_obj,
                       study_design                     = c("RCT", "obs"),
                       rob                              = NULL,
                       rob_dominant_threshold           = 0.60,
                       rob_inflation_threshold          = 0.10,
                       small_values                     = NULL,
                       indirectness                     = "no",
                       inconsistency                    = NULL,
                       inconsistency_ci_diff            = NULL,
                       inconsistency_threshold_side     = NULL,
                       inconsistency_subgroup_explained = NULL,
                       mid                              = NULL,
                       mid_scale                        = "auto",
                       outcome_name                     = NULL,
                       outcome_type                     = c("relative", "absolute"),
                       ois_events                       = NULL,
                       ois_n                            = NULL,
                       ois_alpha                        = 0.05,
                       ois_beta                         = 0.20,
                       ois_p0                           = NULL,
                       ois_p1                           = NULL,
                       ois_delta                        = NULL,
                       ois_sd                           = NULL,
                       baseline_risk                    = NULL,
                       pubias_small_industry            = NULL,
                       pubias_funnel_asymmetry          = NULL,
                       pubias_unpublished               = NULL) {

  # --- input check ---
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("meta_obj must be an object of class 'meta' (from the {meta} package).")
  }
  study_design <- match.arg(study_design)
  outcome_type <- match.arg(outcome_type)

  # --- starting certainty ---
  start_score     <- if (study_design == "RCT") 4L else 2L
  starting_quality <- score_to_certainty(start_score)

  # --- resolve MID to TE scale (used by Inconsistency and Imprecision) ---
  mid_resolved <- mid_to_te_scale(mid, mid_scale, meta_obj$sm)
  mid_internal <- mid_resolved$mid_internal
  mid_kind     <- mid_resolved$mid_kind

  # --- domain assessments ---
  d_rob   <- assess_rob(rob, meta_obj,
                        rob_dominant_threshold  = rob_dominant_threshold,
                        rob_inflation_threshold = rob_inflation_threshold,
                        small_values            = small_values)

  d_indir <- assess_indirectness(indirectness, meta_obj)

  d_incon <- assess_inconsistency(
    meta_obj,
    inconsistency                    = inconsistency,
    inconsistency_ci_diff            = inconsistency_ci_diff,
    inconsistency_threshold_side     = inconsistency_threshold_side,
    inconsistency_subgroup_explained = inconsistency_subgroup_explained,
    mid_internal                     = mid_internal
  )

  d_impre <- assess_imprecision(
    meta_obj,
    outcome_type = outcome_type,
    ois_events   = ois_events,
    ois_n        = ois_n,
    ois_alpha    = ois_alpha,
    ois_beta     = ois_beta,
    ois_p0       = ois_p0,
    ois_p1       = ois_p1,
    ois_delta    = ois_delta,
    ois_sd       = ois_sd,
    mid_internal = mid_internal,
    mid_kind     = mid_kind
  )

  d_pubias <- assess_pubias(
    meta_obj,
    pubias_small_industry   = pubias_small_industry,
    pubias_funnel_asymmetry = pubias_funnel_asymmetry,
    pubias_unpublished      = pubias_unpublished
  )

  domains <- dplyr::bind_rows(d_rob, d_indir, d_incon, d_impre, d_pubias)

  # --- 確実性スコア計算 ---
  total_downgrade <- sum(domains$downgrade)
  final_score     <- max(1L, start_score + total_downgrade)
  certainty       <- score_to_certainty(final_score)

  # --- output object ---
  structure(
    list(
      domain_assessments = domains,
      certainty          = certainty,
      certainty_score    = final_score,
      starting_quality   = starting_quality,
      study_design       = study_design,
      outcome_name       = if (is.null(outcome_name)) "Outcome" else outcome_name,
      outcome_type       = outcome_type,
      baseline_risk      = .resolve_baseline_risk(baseline_risk, meta_obj, ois_p0),
      mid                = mid,
      mid_scale          = mid_scale,
      mid_internal       = mid_internal,
      meta               = meta_obj
    ),
    class = "pmatools"
  )
}

#' @export
print.pmatools <- function(x, ...) {
  cat("\n-- GRADE Certainty Assessment ---------------------------\n")
  cat(sprintf(" Outcome      : %s\n", x$outcome_name))
  cat(sprintf(" Study design : %s  (starting quality: %s)\n",
              x$study_design, x$starting_quality))
  cat("\n Domain assessments:\n")

  d <- x$domain_assessments
  for (i in seq_len(nrow(d))) {
    row <- d[i, ]
    dg  <- if (row$downgrade < 0) sprintf(" [%d]", row$downgrade) else "    "
    cat(sprintf("  %-20s %-14s %s\n",
                row$domain, row$judgment, dg))
  }

  cat(sprintf("\n Final certainty : %s  %s\n",
              x$certainty, CERTAINTY_SYMBOLS[[x$certainty]]))
  cat("----------------------------------------------------------\n\n")
  invisible(x)
}

#' @export
summary.pmatools <- function(object, ...) {
  print(object, ...)
  cat("Domain details:\n")
  d <- object$domain_assessments
  for (i in seq_len(nrow(d))) {
    row <- d[i, ]
    if (!is.na(row$notes)) {
      cat(sprintf("  [%s] %s\n", row$domain, row$notes))
    }
  }
  invisible(object)
}
