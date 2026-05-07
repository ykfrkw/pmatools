# domain_imprecision.R — 不精確性ドメイン自動評価
#
# BMJ 2025 Core GRADE 4: Imprecision
#
# References:
#   Zeng L, Brignardello-Petersen R, Hultcrantz M, et al.
#     GRADE Guidance 34: update on rating imprecision using a minimally
#     contextualised approach. BMJ. 2025;389:e083087. (PMID 40360206)
#   Guyatt GH, Oxman AD, Kunz R, et al. GRADE guidelines 6.
#     Rating the quality of evidence — imprecision (Optimal Information
#     Size). J Clin Epidemiol. 2011;64(12):1283-1293.
#     doi:10.1016/j.jclinepi.2011.01.012 (PMID 21839614)
#
# 判定基準 (BMJ 2025 Core GRADE 4 / Zeng GRADE Guidance 34, 3-level):
#
#   No concern (-0)    : CI が null を跨がない かつ OIS を満たす（または未指定）
#   Some concerns (-1) : CI が null を跨ぐ、または OIS を満たさない（30% 超）
#   Serious (-2)       :
#     (a) Threshold 提示時: 95% CI が ±Threshold の両方を含む（clinically
#         important benefit と harm の両方が確からしい）, または
#     (b) 総 N（連続）／総イベント数（バイナリ）が OIS の 30% 以下
#
# Null の定義:
#   {meta} の lower.random / upper.random は RR/OR/HR で log スケール、
#   MD/SMD で原スケール。いずれも null_val = 0 として crosses-null を判定できる。
# Threshold:
#   threshold_internal は TE スケール（ratio は log、絶対値は原）の正値。閾値は
#   ±threshold_internal。Threshold 未提示時は (a) は適用せず crosses_null のみで判断。
#
# OIS (Optimal Information Size) の計算方法:
#
#   方法 1 — 直接指定:
#     ois_events: バイナリ結果の目標総イベント数
#     ois_n     : 連続結果の目標総サンプル数
#
#   方法 2 — 自動計算（方法 1 が未指定の場合に使用）:
#     バイナリ結果: ois_p0 と ois_p1 を指定
#       n_arm = (z_alpha/2 + z_beta)^2 × [p0(1-p0) + p1(1-p1)] / (p0-p1)^2
#       OIS_events ≈ 2 × n_arm × p̄  (p̄ = (p0+p1)/2)
#     連続結果: ois_delta と ois_sd を指定
#       n_arm = 2 × (z_alpha/2 + z_beta)^2 × sigma^2 / delta^2
#       OIS_n = 2 × n_arm
#     既定: ois_alpha = 0.05 (両側), ois_beta = 0.20 (検出力 80%)

assess_imprecision <- function(meta_obj,
                               outcome_type       = "relative",
                               ois_events         = NULL,
                               ois_n              = NULL,
                               ois_alpha          = 0.05,
                               ois_beta           = 0.20,
                               ois_p0             = NULL,
                               ois_p1             = NULL,
                               ois_delta          = NULL,
                               ois_sd             = NULL,
                               threshold_internal = NULL,
                               threshold_kind     = NULL) {
  lower <- meta_obj$lower.random
  upper <- meta_obj$upper.random

  if (is.null(lower) || is.null(upper) || is.na(lower) || is.na(upper)) {
    return(make_domain_row(
      domain   = "Imprecision",
      judgment = "no",
      auto     = TRUE,
      notes    = "CI not available; imprecision not assessed."
    ))
  }

  # null = 0 (log scale for RR/OR/HR; original scale for MD/SMD)
  null_val    <- 0.0
  crosses_null <- (lower < null_val) && (upper > null_val)

  # Threshold-based check (Zeng et al. BMJ 2025, GRADE Guidance 34).
  # A CI "crosses" a threshold T iff T lies inside the CI: lower < T AND upper > T.
  # Four states relative to the [-Threshold, +Threshold] trivial zone:
  #   crosses_both_thresholds : CI contains both -T and +T (lower < -T AND upper > +T)
  #                             -> serious (-2)
  #   crosses_one_threshold   : CI contains exactly one of {-T, +T}
  #                             -> some_concerns (-1)
  #   within_thresholds       : CI lies entirely in trivial zone
  #                             (-T <= lower AND upper <= +T) -> no concern
  #   beyond_thresholds       : CI lies entirely outside trivial zone on one side
  #                             (upper <= -T OR lower >= +T) -> no concern
  #                             (definitively important effect)
  has_threshold <- !is.null(threshold_internal) &&
                   length(threshold_internal) > 0 &&
                   !is.na(threshold_internal) &&
                   is.finite(threshold_internal) &&
                   threshold_internal > 0
  if (has_threshold) {
    crosses_lower_threshold <- (lower < -threshold_internal) && (upper > -threshold_internal)
    crosses_upper_threshold <- (lower <  threshold_internal) && (upper >  threshold_internal)
    crosses_both_thresholds <- crosses_lower_threshold && crosses_upper_threshold
    crosses_one_threshold   <- xor(crosses_lower_threshold, crosses_upper_threshold)
    within_thresholds       <- (lower >= -threshold_internal) && (upper <= threshold_internal)
    beyond_thresholds       <- (upper <= -threshold_internal) || (lower >= threshold_internal)
  } else {
    crosses_both_thresholds <- NA
    crosses_one_threshold   <- NA
    within_thresholds       <- NA
    beyond_thresholds       <- NA
  }

  # Defensive: treat NA as NULL
  if (!is.null(ois_events) && (length(ois_events) == 0 || is.na(ois_events))) ois_events <- NULL
  if (!is.null(ois_n)      && (length(ois_n)      == 0 || is.na(ois_n)))      ois_n      <- NULL
  if (!is.null(ois_p0)     && (length(ois_p0)     == 0 || is.na(ois_p0)))     ois_p0     <- NULL
  if (!is.null(ois_p1)     && (length(ois_p1)     == 0 || is.na(ois_p1)))     ois_p1     <- NULL
  if (!is.null(ois_delta)  && (length(ois_delta)  == 0 || is.na(ois_delta)))  ois_delta  <- NULL
  if (!is.null(ois_sd)     && (length(ois_sd)     == 0 || is.na(ois_sd)))     ois_sd     <- NULL

  # v0.2: derive ois_p1/ois_delta from threshold_internal when not explicitly provided
  threshold_used_note <- ""
  if (is.null(ois_events) && is.null(ois_n) &&
      !is.null(threshold_internal) && !is.na(threshold_internal) &&
      threshold_internal != 0) {
    if (outcome_type == "relative") {
      # Auto-fall back ois_p0 to control-arm pooled proportion if missing
      if (is.null(ois_p0)) {
        cer <- tryCatch(.compute_control_risk(meta_obj, method = "simple"),
                        error = function(e) NULL)
        if (!is.null(cer) && is.finite(cer) && cer > 0 && cer < 1) {
          ois_p0 <- cer
          threshold_used_note <- sprintf(
            " (ois_p0 auto from data = %.4f)", ois_p0
          )
        }
      }
      if (is.null(ois_p1) && !is.null(ois_p0)) {
        sm_local <- meta_obj$sm %||% ""
        if (identical(threshold_kind, "ard")) {
          ois_p1 <- ois_p0 + threshold_internal
        } else if (identical(sm_local, "OR")) {
          # OR scale: invert odds, not risk. RR-style p1 = p0 * exp(Threshold)
          # is only accurate when p0 is small; for p0 ~ 0.5 it can be
          # ~10% off and biases the OIS estimate.
          or_val <- exp(threshold_internal)
          ois_p1 <- (ois_p0 * or_val) / (1 - ois_p0 + ois_p0 * or_val)
        } else {
          # RR / HR / RoM: log scale, ois_p1 = p0 * exp(Threshold).
          ois_p1 <- ois_p0 * exp(threshold_internal)
        }
        ois_p1 <- max(min(ois_p1, 1 - 1e-6), 1e-6)
        threshold_used_note <- paste0(threshold_used_note, sprintf(
          " (ois_p1 derived from Threshold: ois_p1 = %.4f)", ois_p1
        ))
      }
    } else {
      # Continuous outcomes
      if (is.null(ois_delta)) {
        ois_delta <- threshold_internal
        threshold_used_note <- sprintf(
          " (ois_delta = Threshold = %.4f)", ois_delta
        )
      }
    }
  }

  # OIS auto-calculation (explicit ois_events/ois_n take precedence)
  ois_calc_note <- ""
  if (is.null(ois_events) && is.null(ois_n)) {
    auto_ois <- .calc_ois(outcome_type, ois_alpha, ois_beta,
                          ois_p0, ois_p1, ois_delta, ois_sd)
    if (!is.null(auto_ois)) {
      ois_calc_note <- paste0(auto_ois$formula, threshold_used_note)
      if (auto_ois$type == "events") ois_events <- auto_ois$value
      if (auto_ois$type == "n")      ois_n      <- auto_ois$value
    }
  }

  ois_info <- .compute_ois_pct(meta_obj, ois_events, ois_n)
  ois_pct  <- ois_info$pct
  ois_met  <- if (is.na(ois_pct)) NA else (ois_pct >= 1.0)
  judgment <- .classify_imprecision(
    crosses_null, crosses_both_thresholds, crosses_one_threshold,
    ois_met, ois_pct, has_threshold
  )

  # Display CI on natural scale (exp for ratio sm; raw for MD/SMD)
  sm <- meta_obj$sm
  if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")) {
    ci_disp_lo <- exp(lower)
    ci_disp_hi <- exp(upper)
    null_disp  <- 1
    ci_str <- sprintf("%s [%.2f, %.2f]", sm, ci_disp_lo, ci_disp_hi)
  } else {
    ci_str <- sprintf("%s [%.3f, %.3f]",
                       if (is.null(sm)) "Effect" else sm, lower, upper)
    null_disp <- 0
  }

  ois_str <- if (is.na(ois_met)) {
    "OIS not specified"
  } else if (ois_met) {
    sprintf("OIS met (%.0f%%; observed %d / target %d %s)",
            100 * ois_pct, ois_info$observed, ois_info$target, ois_info$unit)
  } else if (!is.na(ois_pct) && ois_pct <= 0.30) {
    sprintf("OIS not met; observed %d / target %d %s = %.0f%% (<= 30%%)",
            ois_info$observed, ois_info$target, ois_info$unit, 100 * ois_pct)
  } else {
    sprintf("OIS not met (observed %d / target %d %s = %.0f%%)",
            ois_info$observed, ois_info$target, ois_info$unit, 100 * ois_pct)
  }

  thresh_str <- if (!has_threshold) {
    ""
  } else if (isTRUE(crosses_both_thresholds)) {
    "; crosses BOTH Thresholds"
  } else if (isTRUE(crosses_one_threshold)) {
    "; crosses one Threshold"
  } else if (isTRUE(within_thresholds)) {
    "; within Threshold (trivial effect)"
  } else {
    "; beyond Threshold (definitively important effect)"
  }

  notes <- sprintf(
    "95%% CI %s; null = %g; crosses null = %s%s; %s%s",
    ci_str, null_disp,
    if (crosses_null) "YES" else "no",
    thresh_str,
    ois_str,
    if (nchar(ois_calc_note) > 0) paste0(" (", ois_calc_note, ")") else ""
  )

  make_domain_row(
    domain   = "Imprecision",
    judgment = judgment,
    auto     = TRUE,
    notes    = notes
  )
}

# --------------------------------------------------------------------------
# OIS 自動計算
# --------------------------------------------------------------------------
.calc_ois <- function(outcome_type, ois_alpha, ois_beta,
                      ois_p0, ois_p1, ois_delta, ois_sd) {
  # Defensive: treat NA as NULL (Shiny may pass NA from blank numericInput)
  if (!is.null(ois_p0)    && (length(ois_p0)    == 0 || is.na(ois_p0)))    ois_p0    <- NULL
  if (!is.null(ois_p1)    && (length(ois_p1)    == 0 || is.na(ois_p1)))    ois_p1    <- NULL
  if (!is.null(ois_delta) && (length(ois_delta) == 0 || is.na(ois_delta))) ois_delta <- NULL
  if (!is.null(ois_sd)    && (length(ois_sd)    == 0 || is.na(ois_sd)))    ois_sd    <- NULL

  za <- stats::qnorm(1 - ois_alpha / 2)
  zb <- stats::qnorm(1 - ois_beta)

  if (outcome_type == "relative" && !is.null(ois_p0) && !is.null(ois_p1)) {
    if (ois_p0 <= 0 || ois_p0 >= 1 || ois_p1 <= 0 || ois_p1 >= 1) {
      rlang::abort("ois_p0 and ois_p1 must be probabilities (0 < p < 1).")
    }
    if (ois_p0 == ois_p1) {
      rlang::abort("ois_p0 and ois_p1 must differ for OIS calculation.")
    }
    n_arm        <- (za + zb)^2 * (ois_p0 * (1 - ois_p0) + ois_p1 * (1 - ois_p1)) /
                    (ois_p0 - ois_p1)^2
    p_bar        <- (ois_p0 + ois_p1) / 2
    total_events <- ceiling(2 * n_arm * p_bar)
    formula_str  <- sprintf(
      "OIS: p0=%.3f, p1=%.3f, alpha=%.2f, beta=%.2f -> target %d events",
      ois_p0, ois_p1, ois_alpha, ois_beta, total_events
    )
    return(list(type = "events", value = total_events, formula = formula_str))
  }

  if (outcome_type == "absolute" && !is.null(ois_delta) && !is.null(ois_sd)) {
    if (ois_delta == 0) rlang::abort("ois_delta must be non-zero.")
    n_arm   <- 2 * (za + zb)^2 * ois_sd^2 / ois_delta^2
    total_n <- ceiling(2 * n_arm)
    formula_str <- sprintf(
      "OIS: delta=%.3f, sigma=%.3f, alpha=%.2f, beta=%.2f -> target N=%d",
      ois_delta, ois_sd, ois_alpha, ois_beta, total_n
    )
    return(list(type = "n", value = total_n, formula = formula_str))
  }

  NULL
}

# --------------------------------------------------------------------------
# OIS 達成率（達成判定 / serious 判定の双方に使用）
#
# Returns list(pct, observed, target, unit). `unit` is "events" (binary) or
# "N" (continuous). When neither is computable, all four fields are NA.
# --------------------------------------------------------------------------
.compute_ois_pct <- function(meta_obj, ois_events, ois_n) {
  na_out <- list(pct = NA_real_, observed = NA_integer_,
                 target = NA_integer_, unit = NA_character_)

  if (!is.null(ois_events) && is.finite(ois_events) && ois_events > 0) {
    events_e <- if (!is.null(meta_obj$event.e)) sum(meta_obj$event.e, na.rm = TRUE) else NA
    events_c <- if (!is.null(meta_obj$event.c)) sum(meta_obj$event.c, na.rm = TRUE) else NA
    if (!is.na(events_e) && !is.na(events_c)) {
      observed <- as.integer(events_e + events_c)
      return(list(pct      = observed / ois_events,
                  observed = observed,
                  target   = as.integer(ois_events),
                  unit     = "events"))
    }
  }
  if (!is.null(ois_n) && is.finite(ois_n) && ois_n > 0) {
    n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA
    n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA
    if (!is.na(n_e) && !is.na(n_c)) {
      observed <- as.integer(n_e + n_c)
      return(list(pct      = observed / ois_n,
                  observed = observed,
                  target   = as.integer(ois_n),
                  unit     = "N"))
    }
  }
  na_out
}

# --------------------------------------------------------------------------
# 判定分類 (Zeng et al. BMJ 2025, GRADE Guidance 34)
# --------------------------------------------------------------------------
.classify_imprecision <- function(crosses_null,
                                  crosses_both_thresholds,
                                  crosses_one_threshold,
                                  ois_met,
                                  ois_pct,
                                  has_threshold) {
  # -2 (serious):
  #   (a) CI contains both ±Thresholds (when Threshold is provided), OR
  #   (b) total events / total N <= 30% of OIS.
  # -1 (some_concerns):
  #   When Threshold provided : CI crosses exactly one Threshold.
  #   When Threshold absent   : CI crosses null.
  #   Always                  : OIS not met (and > 30%).
  # -0 (no): CI within or beyond Threshold (or, no Threshold, doesn't cross null) AND OIS met.
  serious_ois <- !is.na(ois_pct) && is.finite(ois_pct) && ois_pct <= 0.30
  if (isTRUE(crosses_both_thresholds) || serious_ois) return("serious")

  ci_some <- if (has_threshold) isTRUE(crosses_one_threshold)
             else               isTRUE(crosses_null)
  if (ci_some || isFALSE(ois_met)) return("some_concerns")
  "no"
}
