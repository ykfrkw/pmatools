# domain_imprecision.R — 不精確性ドメイン自動評価
#
# BMJ 2025 Core GRADE 4: Imprecision
#
# 判定基準:
#   No concern    : 95% CI が null を跨がない かつ OIS を満たす（または未指定）
#   Serious       : CI が null を跨ぐ、または OIS を満たさない
#   Very serious  : CI が null を跨ぎ かつ OIS も満たさない
#
# Null の定義:
#   {meta} の lower.random / upper.random は RR/OR/HR で log スケール、
#   MD/SMD で原スケール。いずれも null_val = 0 として crosses-null を判定できる。
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
                               outcome_type = "relative",
                               ois_events   = NULL,
                               ois_n        = NULL,
                               ois_alpha    = 0.05,
                               ois_beta     = 0.20,
                               ois_p0       = NULL,
                               ois_p1       = NULL,
                               ois_delta    = NULL,
                               ois_sd       = NULL,
                               mid_internal = NULL,
                               mid_kind     = NULL) {
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

  # Defensive: treat NA as NULL
  if (!is.null(ois_events) && (length(ois_events) == 0 || is.na(ois_events))) ois_events <- NULL
  if (!is.null(ois_n)      && (length(ois_n)      == 0 || is.na(ois_n)))      ois_n      <- NULL
  if (!is.null(ois_p0)     && (length(ois_p0)     == 0 || is.na(ois_p0)))     ois_p0     <- NULL
  if (!is.null(ois_p1)     && (length(ois_p1)     == 0 || is.na(ois_p1)))     ois_p1     <- NULL
  if (!is.null(ois_delta)  && (length(ois_delta)  == 0 || is.na(ois_delta)))  ois_delta  <- NULL
  if (!is.null(ois_sd)     && (length(ois_sd)     == 0 || is.na(ois_sd)))     ois_sd     <- NULL

  # v0.2: derive ois_p1/ois_delta from mid_internal when not explicitly provided
  mid_used_note <- ""
  if (is.null(ois_events) && is.null(ois_n) &&
      !is.null(mid_internal) && !is.na(mid_internal) && mid_internal != 0) {
    if (outcome_type == "relative") {
      # Auto-fall back ois_p0 to control-arm pooled proportion if missing
      if (is.null(ois_p0)) {
        cer <- tryCatch(.compute_control_risk(meta_obj, method = "simple"),
                        error = function(e) NULL)
        if (!is.null(cer) && is.finite(cer) && cer > 0 && cer < 1) {
          ois_p0 <- cer
          mid_used_note <- sprintf(
            " (ois_p0 auto from data = %.4f)", ois_p0
          )
        }
      }
      if (is.null(ois_p1) && !is.null(ois_p0)) {
        if (identical(mid_kind, "ard")) {
          ois_p1 <- ois_p0 + mid_internal
        } else {
          ois_p1 <- ois_p0 * exp(mid_internal)
        }
        ois_p1 <- max(min(ois_p1, 1 - 1e-6), 1e-6)
        mid_used_note <- paste0(mid_used_note, sprintf(
          " (ois_p1 derived from Threshold: ois_p1 = %.4f)", ois_p1
        ))
      }
    } else {
      # Continuous outcomes
      if (is.null(ois_delta)) {
        ois_delta <- mid_internal
        mid_used_note <- sprintf(
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
      ois_calc_note <- paste0(auto_ois$formula, mid_used_note)
      if (auto_ois$type == "events") ois_events <- auto_ois$value
      if (auto_ois$type == "n")      ois_n      <- auto_ois$value
    }
  }

  ois_met <- .check_ois(meta_obj, ois_events, ois_n)
  judgment <- .classify_imprecision(crosses_null, ois_met)

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
    "OIS met"
  } else {
    "OIS not met"
  }

  notes <- sprintf(
    "95%% CI %s; null = %g; crosses null = %s; %s%s",
    ci_str, null_disp,
    if (crosses_null) "YES" else "no",
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
# OIS 達成チェック
# --------------------------------------------------------------------------
.check_ois <- function(meta_obj, ois_events, ois_n) {
  if (!is.null(ois_events)) {
    events_e <- if (!is.null(meta_obj$event.e)) sum(meta_obj$event.e, na.rm = TRUE) else NA
    events_c <- if (!is.null(meta_obj$event.c)) sum(meta_obj$event.c, na.rm = TRUE) else NA
    total_events <- if (!is.na(events_e) && !is.na(events_c)) events_e + events_c else NA
    if (!is.na(total_events)) return(total_events >= ois_events)
  }
  if (!is.null(ois_n)) {
    n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA
    n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA
    total_n <- if (!is.na(n_e) && !is.na(n_c)) n_e + n_c else NA
    if (!is.na(total_n)) return(total_n >= ois_n)
  }
  NA
}

# --------------------------------------------------------------------------
# 判定分類
# --------------------------------------------------------------------------
.classify_imprecision <- function(crosses_null, ois_met) {
  if (isTRUE(crosses_null) && isFALSE(ois_met)) return("very_serious")
  if (isTRUE(crosses_null))  return("serious")
  if (isFALSE(ois_met))      return("serious")
  "no"
}
