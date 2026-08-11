# domain_imprecision.R — 不精確性ドメイン自動評価
#
# BMJ 2025 Core GRADE 2: Imprecision
#
# References:
#   Guyatt G, Brignardello-Petersen R, Hultcrantz M, et al.
#     Core GRADE 2: choosing the target of certainty rating and assessing
#     imprecision. BMJ. 2025;389:e081904.
#     doi:10.1136/bmj-2024-081904
#   Zeng L, Brignardello-Petersen R, Hultcrantz M, et al.
#     GRADE Guidance 34: update on rating imprecision using a minimally
#     contextualised approach. BMJ. 2025;389:e083087. (companion methodology
#     paper)
#   Guyatt GH, Oxman AD, Kunz R, et al. GRADE guidelines 6.
#     Rating the quality of evidence — imprecision (Optimal Information
#     Size). J Clin Epidemiol. 2011;64(12):1283-1293.
#     doi:10.1016/j.jclinepi.2011.01.012 (PMID 21839614)
#
# 判定基準 (BMJ 2025 Core GRADE 2 Fig 4 の逐語構造):
#
#   Evaluate CI in relation to chosen threshold — does CI cross threshold?
#
#   Yes -> Rate down one level                                    [-1]
#          Consider rating down two levels if:                    [-2]
#            - CI crosses two thresholds (both important benefit and
#              important harm), or
#            - the most appropriate plain language description suggests more
#              uncertainty ("may" rather than "likely").
#          サンプルサイズ／OIS はこの経路では参照しない (本文: "Core GRADE
#          users will rate down for imprecision and do not need to consider
#          sample size")。
#
#   No  -> Moderate effect -> Do not rate down                    [-0]
#       -> Large effect    -> Proceed to OIS approach
#
#          OIS approach (Fig 4 下段):
#            Continuous outcome:
#              N >= OIS (or 800)   -> Do not rate down            [-0]
#              N <  OIS            -> Rate down one level         [-1]
#              N <  30% of OIS     -> Consider rating down two    [-2]
#            Binary outcome:
#              Relative risk CI ratio >= 3 or odds ratio CI ratio >= 2.5
#                                  -> Consider rating down two    [-2]
#              otherwise -> Calculate OIS
#                              N >= OIS -> Do not rate down       [-0]
#                              N <  OIS -> Rate down one level    [-1]
#
#   CI ratio (Fig 4 caption): 「CI 上限 / CI 下限」を比スケールで取った値。
#   "Large effect" = implausibly large: 本文は二値アウトカムについて
#   "implausibly large (certainly relative risk reduction >40%, possibly
#   >30%)" とだけ操作化している。連続アウトカムの「大きい効果」は原論文に
#   定義がないため、pmatools は Cohen の慣例 (標準化効果量 >= 0.8) を用い、
#   その旨を note に明記する（原論文の記述ではない）。
#
# Rating target との関係 (Core GRADE 2 Fig 2 / R/rating_target.R):
#   target = non_null_effect            -> 閾値は null (= 0)
#   target = important_effect /
#            little_to_no_difference    -> 閾値は ±MID
#   assess_imprecision() は threshold_for_imprecision 引数でこの選択を受け取る。
#
# Null の定義:
#   {meta} の lower.random / upper.random は RR/OR/HR で log スケール、
#   MD/SMD で原スケール。いずれも null_val = 0 として crosses-null を判定できる。
# Threshold:
#   threshold_internal は TE スケール（ratio は log、絶対値は原）の正値。OIS の
#   自動計算（ois_p1 / ois_delta の導出）には常に MID を使う。
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
                               threshold_kind     = NULL,
                               threshold_ard      = NULL,
                               threshold_p0       = NULL,
                               rating_target      = NULL,
                               threshold_type     = NULL,
                               threshold_for_imprecision = NULL) {
  if (isTRUE(meta_obj$random)) {
    lower <- meta_obj$lower.random
    upper <- meta_obj$upper.random
  } else {
    lower <- meta_obj$lower.common
    upper <- meta_obj$upper.common
  }
  if (is.null(lower) || is.null(upper) ||
      length(lower) == 0L || length(upper) == 0L ||
      !is.finite(lower) || !is.finite(upper)) {
    if (isTRUE(meta_obj$random)) {
      lower <- meta_obj$lower.common
      upper <- meta_obj$upper.common
    } else {
      lower <- meta_obj$lower.random
      upper <- meta_obj$upper.random
    }
  }

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

  # Which threshold does Fig 4 evaluate the CI against? The rating target
  # decides (Core GRADE 2 Fig 2): a non-null-effect target uses the null,
  # every other target uses +/-MID. threshold_for_imprecision carries that
  # choice down from grade_meta(); when absent (direct calls / older code)
  # fall back to the MID.
  thr_eff <- if (!is.null(threshold_for_imprecision)) {
    threshold_for_imprecision
  } else {
    threshold_internal
  }

  # A CI "crosses" a threshold T iff T lies inside the CI: lower < T AND upper > T.
  # Four states relative to the [-Threshold, +Threshold] trivial zone:
  #   crosses_both_thresholds : CI contains both -T and +T (lower < -T AND upper > +T)
  #   crosses_one_threshold   : CI contains exactly one of {-T, +T}
  #   within_thresholds       : CI lies entirely in trivial zone
  #                             (-T <= lower AND upper <= +T)
  #   beyond_thresholds       : CI lies entirely outside trivial zone on one side
  #                             (upper <= -T OR lower >= +T)
  has_threshold <- .has_mid(thr_eff)
  if (has_threshold) {
    crosses_lower_threshold <- (lower < -thr_eff) && (upper > -thr_eff)
    crosses_upper_threshold <- (lower <  thr_eff) && (upper >  thr_eff)
    crosses_both_thresholds <- crosses_lower_threshold && crosses_upper_threshold
    crosses_one_threshold   <- xor(crosses_lower_threshold, crosses_upper_threshold)
    within_thresholds       <- (lower >= -thr_eff) && (upper <= thr_eff)
    beyond_thresholds       <- (upper <= -thr_eff) || (lower >= thr_eff)
    crosses_threshold       <- crosses_lower_threshold || crosses_upper_threshold
  } else {
    crosses_both_thresholds <- NA
    crosses_one_threshold   <- NA
    within_thresholds       <- NA
    beyond_thresholds       <- NA
    # Null threshold (target = non-null effect, or no MID available).
    crosses_threshold       <- crosses_null
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
      # ARD Threshold converted to the ratio scale: anchor ois_p0 to the same
      # baseline risk that was used for the conversion, for consistency.
      if (is.null(ois_p0) && !is.null(threshold_ard) &&
          !is.null(threshold_p0) && is.finite(threshold_p0)) {
        ois_p0 <- threshold_p0
        threshold_used_note <- sprintf(
          " (ois_p0 from threshold baseline risk = %.4f)", ois_p0
        )
      }
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
        if (!is.null(threshold_ard) && is.finite(threshold_ard)) {
          # ARD Threshold with a ratio sm: threshold_internal is on the log
          # scale, so use the raw ARD for the risk arithmetic.
          ois_p1 <- ois_p0 + threshold_ard
        } else if (identical(threshold_kind, "ard")) {
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

  # --- Core GRADE 2 Fig 4 -------------------------------------------------
  sm        <- meta_obj$sm
  te_point  <- .pooled_te(meta_obj)
  is_binary <- .is_binary_outcome(meta_obj)
  large     <- .is_implausibly_large(te_point, sm,
                                     sd_pooled = if (is_binary) NULL
                                                 else compute_pooled_sd(meta_obj))
  ci_ratio     <- .ci_ratio(lower, upper, sm)
  ci_ratio_cut <- .ci_ratio_cut(sm)
  n_total      <- .total_n(meta_obj)

  fig4 <- .classify_imprecision(
    crosses_threshold       = crosses_threshold,
    crosses_both_thresholds = crosses_both_thresholds,
    large                   = large,
    is_binary               = is_binary,
    ois_met                 = ois_met,
    ois_pct                 = ois_pct,
    n_total                 = n_total,
    ci_ratio                = ci_ratio,
    ci_ratio_cut            = ci_ratio_cut,
    threshold_label         = if (has_threshold) "the Threshold (+/-MID)"
                              else "the null threshold"
  )
  judgment <- fig4$judgment

  # Display CI on natural scale (exp for ratio sm; raw for MD/SMD)
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

  ois_detail <- if (is.na(ois_met)) {
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
  # Fig 4 only reaches the OIS approach when the CI does not cross the chosen
  # threshold AND the effect is implausibly large. On every other path the OIS
  # figures are reported for information only and did not drive the judgment.
  ois_str <- if (isTRUE(fig4$ois_used)) {
    ois_detail
  } else {
    paste0("OIS not applied on this Fig 4 path [", ois_detail, "]")
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

  # The rating target itself is appended by grade_meta() (it owns the Fig 2
  # derivation); here we only record which threshold Fig 4 was applied to.
  notes <- sprintf(
    "95%% CI %s; null = %g; crosses null = %s%s; %s%s | %s",
    ci_str, null_disp,
    if (crosses_null) "YES" else "no",
    thresh_str,
    ois_str,
    if (nchar(ois_calc_note) > 0) paste0(" (", ois_calc_note, ")") else "",
    fig4$path
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
# 効果量が「implausibly large」か (Core GRADE 2 本文 6 ページ目)
#
#   "when the CI does not cross the threshold or thresholds of interest and
#    effects on binary outcomes are implausibly large (certainly relative risk
#    reduction >40%, possibly >30%), Core GRADE users should consider rating
#    down for imprecision if the sample size and number of events across all
#    contributing studies are limited"
#
# 二値: RRR > 30% を「possibly」、> 40% を「certainly」とし、OIS 経路へ進む
#       トリガーには保守的に 30% を用いる。比スケールでは
#       |log(effect)| > -log(0.70) が RRR > 30%、
#       |log(effect)| > -log(0.60) が RRR > 40% に対応する
#       （OR は risk ratio の近似として扱う）。
# 連続: 原論文に「大きい効果」の定義がないため、Cohen の慣例
#       （標準化効果量 |d| >= 0.8 = large）を pmatools の操作的定義として使う。
#       MD は pooled SD で標準化する。これは Core GRADE 2 の記述ではない。
# --------------------------------------------------------------------------
.is_implausibly_large <- function(te, sm, sd_pooled = NULL) {
  none <- list(large = FALSE, level = NA_character_,
               note = "effect moderate")
  if (is.null(te) || length(te) != 1L || is.na(te) || !is.finite(te)) {
    return(list(large = FALSE, level = NA_character_,
                note = "effect magnitude not available; treated as moderate"))
  }

  ratio_sm <- c("OR", "RR", "HR", "RoM", "IRR")
  if (!is.null(sm) && sm %in% ratio_sm) {
    rrr <- 1 - exp(-abs(te))   # |log ratio| -> relative risk reduction
    if (rrr > 0.40) {
      return(list(large = TRUE, level = "certain",
                  note = sprintf(
                    "effect implausibly large (relative risk reduction %.0f%% > 40%%)",
                    100 * rrr)))
    }
    if (rrr > 0.30) {
      return(list(large = TRUE, level = "possible",
                  note = sprintf(
                    "effect possibly implausibly large (relative risk reduction %.0f%% > 30%%)",
                    100 * rrr)))
    }
    return(list(large = FALSE, level = NA_character_,
                note = sprintf(
                  "effect moderate (relative risk reduction %.0f%% <= 30%%)",
                  100 * rrr)))
  }

  # Continuous outcomes: standardize, then apply Cohen's large-effect
  # convention. Flagged in the note because Core GRADE 2 operationalizes
  # "large effect" only for binary outcomes.
  std <- if (identical(sm, "SMD")) {
    abs(te)
  } else if (!is.null(sd_pooled) && is.finite(sd_pooled) && sd_pooled > 0) {
    abs(te) / sd_pooled
  } else {
    NA_real_
  }
  if (is.na(std)) {
    return(list(large = FALSE, level = NA_character_,
                note = paste0("effect magnitude could not be standardized ",
                              "(no pooled SD); treated as moderate")))
  }
  if (std >= 0.8) {
    return(list(large = TRUE, level = "possible",
                note = sprintf(paste0(
                  "effect large (standardized effect %.2f >= 0.80; Cohen's ",
                  "convention, not specified by Core GRADE 2)"), std)))
  }
  list(large = FALSE, level = NA_character_,
       note = sprintf(paste0(
         "effect moderate (standardized effect %.2f < 0.80; Cohen's ",
         "convention, not specified by Core GRADE 2)"), std))
}

# --------------------------------------------------------------------------
# CI 比 (Core GRADE 2 Fig 4 caption)
#   "The relative risk CI ratio represents the upper boundary divided by lower
#    boundary of CI of relative risk, and the odds ratio CI ratio represents
#    the upper boundary divided by lower boundary of CI of odds ratio."
# TE が log スケールなので exp(upper) / exp(lower) = exp(upper - lower)。
# 比スケール以外の効果指標では定義されないので NA を返す。
# --------------------------------------------------------------------------
.ci_ratio <- function(lower, upper, sm) {
  if (is.null(sm) || !sm %in% c("OR", "RR", "HR", "RoM", "IRR")) return(NA_real_)
  if (!is.finite(lower) || !is.finite(upper)) return(NA_real_)
  exp(upper - lower)
}

# Fig 4: relative risk CI ratio >= 3, odds ratio CI ratio >= 2.5.
# HR / IRR / RoM は risk ratio と同じ 3 を適用する。
.ci_ratio_cut <- function(sm) {
  if (is.null(sm)) return(NA_real_)
  switch(sm, "OR" = 2.5, "RR" = 3, "HR" = 3, "IRR" = 3, "RoM" = 3, NA_real_)
}

# 二値 (event ベース) アウトカムか。metabin 由来のイベント数があるか、
# 効果指標が二値向けなら二値扱い。Fig 4 の連続／二値の分岐に使う
# (grade_meta の outcome_type は OIS 計算用の "relative"/"absolute" であって
#  連続／二値の区別ではないため、ここでは使わない)。
.is_binary_outcome <- function(meta_obj) {
  if (!is.null(meta_obj$event.e) && length(meta_obj$event.e) > 0) return(TRUE)
  sm <- meta_obj$sm
  if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "IRR", "RD", "ARD")) return(TRUE)
  FALSE
}

# 総サンプルサイズ（連続アウトカムの "N >= OIS (or 800)" 判定に使う）
.total_n <- function(meta_obj) {
  n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA_real_
  n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA_real_
  if (is.na(n_e) || is.na(n_c)) return(NA_real_)
  n_e + n_c
}

# --------------------------------------------------------------------------
# 判定分類 (Core GRADE 2 Fig 4)
#
# 返り値: list(judgment, path, ois_used)
#   path     — 通過した Fig 4 の経路（notes に記録し、監査可能にする）
#   ois_used — OIS 経路を実際に使ったか（notes の表現を切り替える）
# --------------------------------------------------------------------------
.classify_imprecision <- function(crosses_threshold,
                                  crosses_both_thresholds,
                                  large,
                                  is_binary,
                                  ois_met,
                                  ois_pct,
                                  n_total,
                                  ci_ratio,
                                  ci_ratio_cut,
                                  threshold_label = "the Threshold") {
  out <- function(judgment, path, ois_used = FALSE) {
    list(judgment = judgment,
         path     = paste0("Fig 4 path: ", path),
         ois_used = ois_used)
  }

  # --- Yes branch: CI crosses the chosen threshold -------------------------
  if (isTRUE(crosses_threshold)) {
    if (isTRUE(crosses_both_thresholds)) {
      return(out("serious", sprintf(paste0(
        "CI crosses %s -> rate down; CI crosses TWO thresholds (important ",
        "benefit and important harm) -> rate down two levels"),
        threshold_label)))
    }
    return(out("some_concerns", sprintf(paste0(
      "CI crosses %s -> rate down one level (sample size not considered on ",
      "this path)"), threshold_label)))
  }

  # --- No branch: CI does not cross the threshold --------------------------
  if (!isTRUE(large$large)) {
    return(out("no", sprintf(paste0(
      "CI does not cross %s -> %s -> do not rate down (OIS not applied)"),
      threshold_label, large$note)))
  }

  # Large effect -> OIS approach
  prefix <- sprintf("CI does not cross %s -> %s -> OIS approach",
                    threshold_label, large$note)

  if (is_binary) {
    if (!is.na(ci_ratio) && !is.na(ci_ratio_cut) && ci_ratio >= ci_ratio_cut) {
      return(out("serious", sprintf(paste0(
        "%s (binary): CI ratio %.2f >= %.1f -> consider rating down two ",
        "levels"), prefix, ci_ratio, ci_ratio_cut), ois_used = TRUE))
    }
  } else {
    # Continuous rule of thumb: 400 patients per group (total sample size 800).
    if (!is.na(n_total) && n_total >= 800) {
      return(out("no", sprintf(paste0(
        "%s (continuous): total N = %.0f >= 800 (rule of thumb) -> do not ",
        "rate down"), prefix, n_total), ois_used = TRUE))
    }
  }

  if (is.na(ois_met)) {
    return(out("no", sprintf(paste0(
      "%s: OIS could not be computed -> do not rate down"), prefix),
      ois_used = TRUE))
  }
  if (isTRUE(ois_met)) {
    return(out("no", sprintf("%s: N >= OIS -> do not rate down", prefix),
               ois_used = TRUE))
  }
  # N < OIS
  if (!is_binary && !is.na(ois_pct) && ois_pct < 0.30) {
    return(out("serious", sprintf(paste0(
      "%s (continuous): N < 30%% of OIS -> consider rating down two levels"),
      prefix), ois_used = TRUE))
  }
  out("some_concerns", sprintf("%s: N < OIS -> rate down one level", prefix),
      ois_used = TRUE)
}
