# grade_meta.R — メイン GRADE 評価関数
#
# BMJ 2025 Core GRADE シリーズに準拠した確実性評価を {meta} オブジェクトから実施する。

#' Assess certainty of evidence (Core GRADE series) from a meta-analysis object
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
#'   \strong{Breaking change (v0.4.0)}: passing a scalar GRADE level bypasses
#'   the automated flowchart and therefore requires \code{rob_rationale}.
#' @param rob_rationale Free-text justification, required whenever \code{rob}
#'   is supplied as a scalar GRADE level (manual override of the automated
#'   Risk of Bias flowchart). Recorded in the domain notes as
#'   \code{"Manual override (<judgment>): <rationale>"} and propagated to
#'   \code{\link{evidence_profile}}, \code{\link{grade_report}} and
#'   \code{\link{export_bundle}} outputs (Core GRADE transparency principle).
#'   Not used for per-study vectors or column-name input (automated
#'   assessment). Default \code{NULL}.
#' @param rob_dominant_threshold Deprecated (v0.3.1+; accepted but ignored).
#'   The Risk-of-Bias flowchart no longer uses a weight-share dominance gate;
#'   the direction-and-magnitude check is now run whenever at least one
#'   high-RoB study is present.
#' @param rob_inflation_threshold (v0.2) Threshold for the relative change of
#'   the pooled estimate when high-RoB studies are excluded, computed on the
#'   absolute analysis scale: \eqn{(|TE_{all}| - |TE_{low}|) / |TE_{low}|},
#'   where \eqn{TE_{low}} is the inverse-variance weighted mean of the
#'   low/some-concerns RoB studies. Default \code{0.10} (10 percent).
#'   A downgrade under this criterion requires BOTH (a) the relative change to
#'   be *strictly* greater than the threshold (\code{>}; a change exactly at
#'   the threshold does not rate down) AND (b) the shift to be in the
#'   bias-favouring direction per \code{small_values}: only shifts that would
#'   make the apparent effect look more favourable (over-estimation) count.
#'   Shifts toward a smaller or less favourable effect never rate down under
#'   this criterion, even when their magnitude exceeds the threshold; in that
#'   case the domain note states explicitly why no downgrade was applied.
#'   When every study is high-RoB, no low/some-RoB comparator pool exists and
#'   the domain is rated \code{"serious"} (rate down 2 levels) unconditionally.
#'   Set to \code{0} to restore v0.1.0 behavior (any bias-favouring change
#'   rates down). Only used when \code{rob} is a vector or column name.
#' @param small_values Are small outcome values desirable?
#'   \code{"desirable"} if small values are good (eg, mortality, symptom severity) or
#'   \code{"undesirable"} if small values are bad (eg, response rate, remission OR > 1).
#'   Used to automatically determine whether dominated high-RoB studies inflate the
#'   apparent effect. If \code{NULL} (default), a conservative rate-down is applied
#'   when dominated. Only used when \code{rob} is a vector or column name.
#'   (Consistent with \code{netmetaviz} \code{small_values} parameter.)
#' @param indirectness Indirectness judgment. Same format as \code{rob} (scalar/vector/column).
#'   Default \code{"no"}.
#'   \strong{Breaking change (v0.4.0)}: a scalar value other than the default
#'   \code{"no"} is a manual override and requires
#'   \code{indirectness_rationale}. \code{"no"} (no downgrade) never requires
#'   a rationale, so default calls are unaffected.
#' @param indirectness_rationale Free-text justification, required whenever
#'   \code{indirectness} is supplied as a scalar GRADE level other than
#'   \code{"no"}. See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
#' @param indirectness_subdomains (v0.5) Optional Core GRADE 5 subdomain
#'   judgment table. A data.frame / tibble (or an equivalent list) with:
#'   \describe{
#'     \item{subdomain}{Required. Subdomain label, typically
#'       \code{"Population"}, \code{"Intervention"}, \code{"Comparison"},
#'       \code{"Outcome"}. Any order, subset, or extra subdomain is accepted;
#'       duplicate labels abort.}
#'     \item{target}{Optional. The target question for that subdomain (display
#'       only).}
#'     \item{evidence}{Optional. The evidence found (display only).}
#'     \item{judgment}{Required. One of \code{"yes"}, \code{"probably_yes"},
#'       \code{"probably_no"}, \code{"no"} — the answer to "Is the evidence
#'       sufficiently direct?". Case and separator variants
#'       (\code{"Probably No"}, \code{"probably-no"}) are accepted.}
#'   }
#'   \code{yes} / \code{probably_yes} contribute \code{"no"},
#'   \code{probably_no} contributes \code{"some_concerns"} and \code{no}
#'   contributes \code{"serious"}; the domain judgment defaults to the worst
#'   case across subdomains. Supplying \code{indirectness} as a scalar
#'   alongside overrides that default and then requires
#'   \code{indirectness_rationale} (a restatement of the default value needs
#'   none). Cannot be combined with per-study vector or column-name
#'   \code{indirectness} input. The normalised table is returned as
#'   \code{indirectness_subdomains} on the result object and rendered by
#'   \code{\link{indirectness_table}}. Default \code{NULL}.
#' @param inconsistency Overall inconsistency scalar judgment. One of
#'   \code{"no"}, \code{"some"}, \code{"serious"}, \code{"very_serious"}.
#'   If provided, flowchart parameters are ignored.
#'   \strong{Breaking change (v0.4.0)}: this scalar override requires
#'   \code{inconsistency_rationale}. The manual flowchart inputs
#'   (\code{inconsistency_ci_diff} etc.) do not.
#' @param inconsistency_rationale Free-text justification, required whenever
#'   \code{inconsistency} is supplied (manual override of the flowchart /
#'   automated assessment). See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
#' @param inconsistency_ci_diff \code{"yes"} / \code{"no"}: Are there important
#'   differences in point estimates AND limited CI overlap? (BMJ Core GRADE 3 Fig 2
#'   Step 1). Required for flowchart; if NULL, falls back to I^2-based assessment.
#' @param inconsistency_threshold_side Required when \code{inconsistency_ci_diff = "yes"}.
#'   \code{"majority_one_side"} or \code{"opposite_sides"}: are most estimates on one
#'   side of the clinical threshold, or spread across both sides?
#' @param inconsistency_subgroup_explained Required when
#'   \code{inconsistency_threshold_side = "opposite_sides"}.
#'   \code{"yes"} / \code{"no"}: is the inconsistency explained by a credible subgroup?
#' @param threshold_type (v0.5) Which threshold the certainty rating is
#'   anchored to (Core GRADE 2 Fig 2, step 1). One of:
#'   \itemize{
#'     \item \code{"mid"} (default): "Are you interested in whether there is
#'       an important effect or not?" — requires a minimal important
#'       difference (\code{threshold}).
#'     \item \code{"null"}: "Are you interested in whether there is a true
#'       underlying effect, benefit or harm?" — the null is the threshold and
#'       a MID is optional.
#'   }
#'   With \code{threshold_type = "mid"} and no \code{threshold}, the call
#'   aborts unless \code{require_threshold = FALSE}.
#' @param require_threshold (v0.5) Gate for the above. \code{TRUE} (default)
#'   makes a MID mandatory when \code{threshold_type = "mid"}. Set to
#'   \code{FALSE} to run without a MID (backward-compatible escape hatch); the
#'   rating target then falls back to a non-null effect and imprecision is
#'   judged against the null.
#' @param rating_target (v0.5) Target of the certainty rating (Core GRADE 2
#'   Fig 2). \code{NULL} (default) derives it automatically from the pooled
#'   point estimate:
#'   \tabular{llll}{
#'     \strong{threshold_type} \tab \strong{point estimate} \tab
#'       \strong{target} \tab \strong{imprecision threshold} \cr
#'     \code{"mid"}  \tab \eqn{|TE| >} MID    \tab \code{"important_effect"}        \tab \eqn{\pm}MID \cr
#'     \code{"mid"}  \tab \eqn{|TE| \le} MID  \tab \code{"little_to_no_difference"} \tab \eqn{\pm}MID \cr
#'     \code{"null"} \tab very near null      \tab \code{"little_to_no_difference"} \tab \eqn{\pm}MID \cr
#'     \code{"null"} \tab not near null       \tab \code{"non_null_effect"}         \tab null (0) \cr
#'   }
#'   "Very near null" is operationalized as \eqn{|TE| \le} MID; with no MID
#'   supplied, nearness cannot be judged and the target falls back to
#'   \code{"non_null_effect"} (recorded in \code{rating_target_note}).
#'   Supplying a value overrides the automatic derivation and requires
#'   \code{rating_target_rationale}.
#' @param rating_target_rationale Free-text justification, required whenever
#'   \code{rating_target} is supplied (manual override of the Core GRADE 2
#'   Fig 2 derivation). See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
#' @param threshold (v0.2) Numeric clinical decision Threshold (a minimally
#'   important effect). This is a cross-cutting parameter shared by the three
#'   Threshold-aware domains — it is not a Risk-of-Bias-specific setting:
#'   \itemize{
#'     \item Inconsistency: Step 2 3-zone classification of study point
#'       estimates around \eqn{\pm}Threshold.
#'     \item Imprecision: CI-vs-Threshold check (Zeng et al. BMJ 2025) and
#'       the OIS calculation when explicit OIS values are not supplied.
#'     \item Risk of bias: sensitivity comparison of the pooled estimate with
#'       vs without high-RoB studies, zone-classified around
#'       \eqn{\pm}Threshold.
#'   }
#'   The same Threshold is used across all three domains.
#' @param threshold_scale (v0.2) How to interpret \code{threshold}. One of:
#'   \itemize{
#'     \item \code{"auto"} (default): infer from \code{meta_obj$sm}
#'       (OR/RR/HR/RoM \eqn{\to} ratio scale; SMD/MD \eqn{\to} TE scale;
#'       ARD \eqn{\to} ARD scale).
#'     \item \code{"te_scale"}: \code{threshold} is already on the same
#'       scale as \code{meta_obj$TE} (log scale for ratios, raw for MD/SMD).
#'     \item \code{"ratio"}: \code{threshold} is supplied as a ratio (e.g.,
#'       \code{1.25} for a 25 percent relative effect); internally
#'       converted to \code{log(threshold)}.
#'     \item \code{"ard"}: \code{threshold} is an absolute risk difference
#'       (binary outcomes only; a proportion, e.g., \code{0.05} for 50 per
#'       1,000). When the effect measure is a ratio (OR/RR/HR; RoM
#'       approximated as RR), the ARD Threshold is converted to an
#'       equivalent ratio at the baseline risk (see
#'       \code{threshold_baseline}): RR uses
#'       \eqn{T = (p_0 + ARD) / p_0}, OR uses
#'       \eqn{T = odds(p_0 + ARD) / odds(p_0)}, and HR is approximated by
#'       the RR formula (accurate only for low event rates; interpret with
#'       care).
#'   }
#' @param threshold_baseline (v0.4) Baseline (control-arm) risk as a
#'   proportion in (0, 1) (e.g., \code{0.18} for 180 per 1,000), used to
#'   convert an absolute Threshold (\code{threshold_scale = "ard"}) to the
#'   ratio scale when the effect measure is OR/RR/HR/RoM. If \code{NULL}
#'   (default), the pooled control event rate
#'   (\eqn{\sum event_c / \sum n_c}) of \code{meta_obj} is used; if that is
#'   unavailable too, an informative error is raised. Ignored unless an ARD
#'   Threshold requires conversion.
#' @param imprecision Optional overall imprecision scalar judgment. One of
#'   \code{"no"}, \code{"some_concerns"}, \code{"serious"} (legacy
#'   \code{"some"} / \code{"very_serious"} accepted). If provided, the
#'   automated imprecision assessment (CI-vs-null/Threshold and OIS checks)
#'   is bypassed entirely and \code{imprecision_rationale} is required.
#'   Default \code{NULL} (automated assessment).
#' @param imprecision_rationale Free-text justification, required whenever
#'   \code{imprecision} is supplied (manual override of the automated
#'   assessment). See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
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
#'   \strong{Breaking change (v0.4.0)}: supplying this argument replaces the
#'   automated Egger's test with a manual visual judgment and therefore
#'   requires \code{pubias_rationale}.
#' @param pubias_rationale Free-text justification, required whenever
#'   \code{pubias_funnel_asymmetry} is supplied (manual override of the
#'   automated Egger's test). The informational inputs
#'   \code{pubias_small_industry}, \code{pubias_unpublished} and
#'   \code{pubias_registry_complete} do not require a rationale. See
#'   \code{rob_rationale} for how it is recorded. Default \code{NULL}.
#' @param pubias_unpublished \code{"yes"} / \code{"no"}: Is there documentation of
#'   unpublished studies (eg, in trial registry or FDA)? Only used when k < 10.
#'   If \code{NULL} (default), assumed \code{"no"} with a warning.
#' @param pubias_registry_complete \code{"yes"} / \code{"no"}: Top-level structural
#'   rule-out for fields where pre-registration is universal and all registered
#'   trials can be accounted for. \code{"yes"} short-circuits the publication
#'   bias domain to "no" regardless of Egger's test or k. Default \code{NULL}.
#'
#' @return An S3 object of class \code{pmatools} containing:
#'   \describe{
#'     \item{domain_assessments}{A tibble with one row per GRADE domain.}
#'     \item{certainty}{Final certainty label: "High", "Moderate", "Low", or "Very Low".}
#'     \item{certainty_score}{Numeric score (1–4).}
#'     \item{starting_quality}{Starting certainty label.}
#'     \item{outcome_name}{Outcome label.}
#'     \item{threshold_type}{\code{"mid"} or \code{"null"} (Core GRADE 2 Fig 2).}
#'     \item{rating_target}{Target of the certainty rating.}
#'     \item{rating_target_note}{How the target was derived (or overridden).}
#'     \item{rating_target_auto}{\code{TRUE} when the target was derived
#'       automatically rather than supplied by the user.}
#'     \item{indirectness_subdomains}{The normalised Core GRADE 5 subdomain
#'       tibble (\code{subdomain}, \code{target}, \code{evidence},
#'       \code{judgment}, \code{grade_level}), or \code{NULL} when none was
#'       supplied. Render it with \code{\link{indirectness_table}}.}
#'     \item{meta}{The original meta object.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(meta)
#' m <- metabin(Ee, Ne, Ec, Nc, studlab = study, data = Olkin1995, sm = "RR")
#' # threshold_type defaults to "mid", which requires a threshold (MID).
#' g <- grade_meta(m, study_design = "RCT", rob = "some",
#'                 rob_rationale = "RoB2 consensus: some concerns from missing outcome data",
#'                 threshold = 1.2, threshold_scale = "ratio",
#'                 outcome_name = "Mortality")
#' print(g)
#' print(g$rating_target)
#' sof_table(g)
#'
#' # Rating certainty in a true underlying effect instead (null threshold).
#' g_null <- grade_meta(m, threshold_type = "null", outcome_name = "Mortality")
#' }
#'
#' @export
grade_meta <- function(meta_obj,
                       study_design                     = c("RCT", "obs"),
                       rob                              = NULL,
                       rob_rationale                    = NULL,
                       rob_dominant_threshold           = 0.60,
                       rob_inflation_threshold          = 0.10,
                       small_values                     = NULL,
                       indirectness                     = "no",
                       indirectness_rationale           = NULL,
                       indirectness_subdomains          = NULL,
                       inconsistency                    = NULL,
                       inconsistency_rationale          = NULL,
                       inconsistency_ci_diff            = NULL,
                       inconsistency_threshold_side     = NULL,
                       inconsistency_subgroup_explained = NULL,
                       imprecision                      = NULL,
                       imprecision_rationale            = NULL,
                       threshold_type                   = c("mid", "null"),
                       threshold                        = NULL,
                       threshold_scale                  = "auto",
                       threshold_baseline               = NULL,
                       rating_target                    = NULL,
                       rating_target_rationale          = NULL,
                       require_threshold                = TRUE,
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
                       pubias_unpublished               = NULL,
                       pubias_registry_complete         = NULL,
                       pubias_rationale                 = NULL) {

  # --- input check ---
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("meta_obj must be an object of class 'meta' (from the {meta} package).")
  }
  study_design   <- match.arg(study_design)
  outcome_type   <- match.arg(outcome_type)
  threshold_type <- match.arg(threshold_type)

  # --- Core GRADE 5 Indirectness subdomains (PICO) ---
  # With a subdomain table the domain judgment defaults to the worst case, and
  # `indirectness` becomes an optional manual override. The documented default
  # ("no") must therefore be distinguishable from an explicit user value.
  indirectness_sub_tbl <-
    .normalize_indirectness_subdomains(indirectness_subdomains)
  indirectness_override <- if (missing(indirectness)) NULL else indirectness

  # --- Core GRADE 2 Fig 2 step 1: the chosen threshold must be explicit ---
  # "mid" means importance is being judged, which is impossible without a MID.
  .check_threshold_type_gate(meta_obj, threshold_type, threshold,
                             require_threshold)

  # --- starting certainty ---
  start_score     <- if (study_design == "RCT") 4L else 2L
  starting_quality <- score_to_certainty(start_score)

  # --- resolve Threshold to TE scale (used by RoB, Inconsistency, Imprecision) ---
  threshold_resolved <- threshold_to_te_scale(
    threshold, threshold_scale, meta_obj$sm,
    threshold_baseline = threshold_baseline,
    meta_obj           = meta_obj
  )
  threshold_internal <- threshold_resolved$threshold_internal
  threshold_kind     <- threshold_resolved$threshold_kind
  threshold_ard      <- threshold_resolved$threshold_ard
  threshold_note     <- threshold_resolved$threshold_note
  threshold_p0       <- threshold_resolved$threshold_baseline

  # --- Core GRADE 2 Fig 2 steps 2-3: target of the certainty rating ---
  auto_target <- .derive_rating_target(
    te_point           = .pooled_te(meta_obj),
    threshold_internal = threshold_internal,
    threshold_type     = threshold_type,
    sm                 = meta_obj$sm,
    threshold_kind     = threshold_kind
  )
  target_info <- .resolve_rating_target(rating_target, rating_target_rationale,
                                        auto_target, threshold_internal)

  # --- domain assessments ---
  d_rob   <- assess_rob(rob, meta_obj,
                        rob_dominant_threshold  = rob_dominant_threshold,
                        rob_inflation_threshold = rob_inflation_threshold,
                        small_values            = small_values,
                        threshold_internal      = threshold_internal,
                        rationale               = rob_rationale)

  d_indir <- assess_indirectness(
    if (is.null(indirectness_sub_tbl)) indirectness else indirectness_override,
    meta_obj,
    rationale  = indirectness_rationale,
    subdomains = indirectness_sub_tbl
  )

  d_incon <- assess_inconsistency(
    meta_obj,
    inconsistency                    = inconsistency,
    inconsistency_ci_diff            = inconsistency_ci_diff,
    inconsistency_threshold_side     = inconsistency_threshold_side,
    inconsistency_subgroup_explained = inconsistency_subgroup_explained,
    threshold_internal               = threshold_internal,
    rationale                        = inconsistency_rationale
  )

  # Imprecision: scalar override bypasses the automated assessment entirely
  # (v0.4.0). Requires imprecision_rationale (Core GRADE transparency).
  d_impre <- if (!is.null(imprecision)) {
    if (!is.character(imprecision) || length(imprecision) != 1L) {
      rlang::abort(paste0(
        "imprecision must be a single GRADE level ",
        "('no', 'some_concerns', 'serious') or NULL."
      ))
    }
    validate_grade_level(imprecision, "imprecision")
    .check_override_rationale(imprecision_rationale, "imprecision_rationale",
                              "Imprecision")
    make_domain_row(
      domain    = "Imprecision",
      judgment  = imprecision,
      auto      = FALSE,
      notes     = paste0("Overall judgment provided by user (scalar; ",
                         "automated assessment not applied)."),
      rationale = imprecision_rationale
    )
  } else {
    assess_imprecision(
      meta_obj,
      outcome_type       = outcome_type,
      ois_events         = ois_events,
      ois_n              = ois_n,
      ois_alpha          = ois_alpha,
      ois_beta           = ois_beta,
      ois_p0             = ois_p0,
      ois_p1             = ois_p1,
      ois_delta          = ois_delta,
      ois_sd             = ois_sd,
      threshold_internal = threshold_internal,
      threshold_kind     = threshold_kind,
      threshold_ard      = threshold_ard,
      threshold_p0       = threshold_p0,
      rating_target      = target_info$target,
      threshold_type     = threshold_type,
      threshold_for_imprecision = target_info$threshold_for_imprecision
    )
  }

  # Record how the rating target was chosen in the Imprecision notes: the
  # target decides which threshold Fig 4 evaluates the CI against, so the two
  # must be auditable together (and the note then propagates to
  # evidence_profile / grade_report / export_bundle).
  d_impre$notes <- ifelse(is.na(d_impre$notes), target_info$note,
                          paste0(d_impre$notes, " | ", target_info$note))

  # Absolute-threshold conversion note: surface it in every Threshold-aware
  # domain so the baseline-risk assumption is auditable per domain.
  if (!is.null(threshold_note)) {
    append_threshold_note <- function(d) {
      d$notes <- ifelse(is.na(d$notes), threshold_note,
                        paste0(d$notes, " | ", threshold_note))
      d
    }
    d_rob   <- append_threshold_note(d_rob)
    d_incon <- append_threshold_note(d_incon)
    d_impre <- append_threshold_note(d_impre)
  }

  d_pubias <- assess_pubias(
    meta_obj,
    pubias_small_industry    = pubias_small_industry,
    pubias_funnel_asymmetry  = pubias_funnel_asymmetry,
    pubias_unpublished       = pubias_unpublished,
    pubias_registry_complete = pubias_registry_complete,
    rationale                = pubias_rationale
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
      threshold_type     = threshold_type,
      rating_target      = target_info$target,
      rating_target_note = target_info$note,
      rating_target_auto = is.null(rating_target),
      threshold          = threshold,
      threshold_scale    = threshold_scale,
      threshold_internal = threshold_internal,
      threshold_ard      = threshold_ard,
      threshold_note     = threshold_note,
      threshold_baseline = threshold_p0,
      # Kept at the top level (not as a list-column of domain_assessments,
      # which must stay one row per domain with atomic columns).
      indirectness_subdomains = indirectness_sub_tbl,
      meta               = meta_obj
    ),
    class = "pmatools"
  )
}

#' @export
print.pmatools <- function(x, ...) {
  cat("\n-- Certainty Assessment (Core GRADE series) -------------\n")
  cat(sprintf(" Outcome      : %s\n", x$outcome_name))
  cat(sprintf(" Study design : %s  (starting quality: %s)\n",
              x$study_design, x$starting_quality))
  if (!is.null(x$rating_target)) {
    target_label <- unname(RATING_TARGET_LABELS[x$rating_target])
    if (is.na(target_label)) target_label <- x$rating_target
    cat(sprintf(" Rating target: %s  (threshold: %s%s)\n",
                target_label,
                x$threshold_type %||% "?",
                if (isTRUE(x$rating_target_auto)) ", auto" else ", manual"))
  }
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
