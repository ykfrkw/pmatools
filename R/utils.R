# utils.R — 共通ユーティリティ

GRADE_LEVELS <- c("no", "some", "serious", "very_serious")
GRADE_DOWNGRADE <- c(no = 0, some = -1, serious = -1, very_serious = -2)
CERTAINTY_LABELS <- c("Very Low", "Low", "Moderate", "High")
CERTAINTY_SYMBOLS <- c(
  "High"       = "\u2295\u2295\u2295\u2295",
  "Moderate"   = "\u2295\u2295\u2295\u25cb",
  "Low"        = "\u2295\u2295\u25cb\u25cb",
  "Very Low"   = "\u2295\u25cb\u25cb\u25cb"
)

# Certainty colour palettes (bg + text colour pairs)
# pastel: soft backgrounds, coloured text — readable on screen and in print
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

# GRADE 判定の検証
validate_grade_level <- function(x, arg = "argument") {
  valid <- c("no", "some", "serious", "very_serious")
  bad <- setdiff(x, valid)
  if (length(bad) > 0) {
    rlang::abort(
      paste0(arg, " contains invalid GRADE level(s): ", paste(bad, collapse = ", "),
             ". Use one of: ", paste(valid, collapse = ", "))
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
  # 3. NULL → fallback to ois_p0, then simple auto-compute
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
make_domain_row <- function(domain, judgment, auto, notes = NA_character_) {
  tibble::tibble(
    domain    = domain,
    judgment  = judgment,
    downgrade = GRADE_DOWNGRADE[[judgment]],
    auto      = auto,
    notes     = notes
  )
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
# MID auto-default per effect measure
# --------------------------------------------------------------------------

#' Suggest a conventional default MID based on the effect measure
#'
#' Returns a conventional minimally important difference (MID) suitable for
#' pre-filling the input field in interactive UIs. Users should override with
#' a published or expert-derived MID whenever available.
#'
#' @param meta_obj A meta object (from \code{\link[meta]{metabin}} or
#'   \code{\link[meta]{metacont}}).
#'
#' @return A list with \code{mid_user} (user-facing value) and \code{mid_scale}
#'   (one of \code{"ratio"}, \code{"te_scale"}, \code{"ard"}). Returns
#'   \code{NULL} if the effect measure is unrecognized.
#'
#' @details
#' Defaults:
#' \itemize{
#'   \item OR / RR / HR: 1.20–1.25 on the ratio scale
#'   \item RoM: 1.10 (10 percent ratio of means)
#'   \item SMD: 0.20 (Cohen's small)
#'   \item MD: 0.20 \eqn{\times} pooled SD (Cohen's small in raw units)
#'   \item ARD: 0.05 (5 percent absolute risk difference)
#' }
#'
#' @export
suggest_mid <- function(meta_obj) {
  sm <- meta_obj$sm
  if (is.null(sm)) return(NULL)

  switch(sm,
    "OR"  = list(mid_user = 1.25,  mid_scale = "ratio"),
    "RR"  = list(mid_user = 1.20,  mid_scale = "ratio"),
    "HR"  = list(mid_user = 1.20,  mid_scale = "ratio"),
    "RoM" = list(mid_user = 1.10,  mid_scale = "ratio"),
    "ARD" = list(mid_user = 0.05,  mid_scale = "ard"),
    "SMD" = list(mid_user = 0.20,  mid_scale = "te_scale"),
    "MD"  = {
      sd_pooled <- compute_pooled_sd(meta_obj)
      if (is.null(sd_pooled) || is.na(sd_pooled) || sd_pooled <= 0) {
        return(NULL)
      }
      list(mid_user = 0.20 * sd_pooled, mid_scale = "te_scale")
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

#' Convert a user-supplied MID to the meta TE scale
#'
#' Internal helper. Translates the user's MID input (with its declared scale)
#' into a value on the same scale as \code{meta_obj$TE}. Used by the
#' Inconsistency and Imprecision domains to anchor judgments to a clinical
#' threshold.
#'
#' @param mid Numeric MID value.
#' @param mid_scale One of \code{"auto"}, \code{"te_scale"}, \code{"ratio"},
#'   or \code{"ard"}.
#' @param sm The effect measure from \code{meta_obj$sm}, used when
#'   \code{mid_scale = "auto"}.
#'
#' @return A list with \code{mid_internal} (numeric on TE scale) and
#'   \code{mid_kind} (the resolved scale, useful for downstream branching like
#'   ARD-vs-ratio in OIS).
#'
#' @keywords internal
mid_to_te_scale <- function(mid, mid_scale = "auto", sm = NULL) {
  if (is.null(mid) || is.na(mid)) {
    return(list(mid_internal = NULL, mid_kind = NULL))
  }

  if (!is.numeric(mid) || length(mid) != 1) {
    rlang::abort("mid must be a single numeric value or NULL.")
  }

  scale <- if (identical(mid_scale, "auto")) {
    if (is.null(sm)) {
      rlang::abort("mid_scale = 'auto' requires meta_obj$sm to be set.")
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
        "Cannot auto-detect mid_scale for sm = '%s'. Specify mid_scale explicitly.", sm))
    )
  } else {
    mid_scale
  }

  if (!scale %in% c("te_scale", "ratio", "ard")) {
    rlang::abort("mid_scale must be one of 'auto', 'te_scale', 'ratio', 'ard'.")
  }

  mid_internal <- switch(scale,
    "te_scale" = mid,
    "ratio"    = log(mid),
    "ard"      = mid
  )

  list(mid_internal = mid_internal, mid_kind = scale)
}
