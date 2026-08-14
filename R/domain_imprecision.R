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
#   両側 MID を跨いだときの -2 は、閾値として null を選んだ経路にも適用される
#   (本文 6 ページ目 逐語):
#     "The two considerations also apply to imprecision judgments when Core
#      GRADE users choose the null as the threshold of interest. For example,
#      consider a situation in which users rate their certainty in a benefit
#      (threshold the null) but the CI also includes clearly important harm.
#      The finding that the CI is consistent with both benefit and important
#      harm motivates a plain language summary stating that the intervention
#      'may' result in a benefit, and rating down two levels for imprecision."
#   したがって rating target = non_null_effect (閾値 = null) でも、MID が
#   与えられていれば ±MID を跨ぐかを別途評価し、両側を跨ぐなら -2 とする。
#   -1 / -0 の判定は従来どおり null (= 0) を基準にする。MID がない場合は
#   両側判定が不能なので -1 止まりになる。
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
#   比較の単位は「参加者数」(Core GRADE 2 Fig 4 caption 逐語:
#   "N=number of participants; OIS=optimal information size"、本文 6 ページ目:
#   "If the total sample size of all the studies included in a meta-analysis
#    exceeds the OIS, one does not rate down")。二値アウトカムでも総イベント数
#   ではなく総サンプルサイズ (n.e + n.c) を OIS と比較する。総イベント数は参考値
#   として notes に併記する。ois_events を明示指定した場合のみイベント数比較に
#   なる（後方互換）。
#
#   方法 1 — 直接指定:
#     ois_events: バイナリ結果の目標総イベント数（後方互換の経路）
#     ois_n     : 目標総サンプル数
#
#   方法 2 — 自動計算（方法 1 が未指定の場合に使用）:
#     バイナリ結果: ois_p0 と ois_p1 を指定
#       n_arm = (z_alpha/2 + z_beta)^2 × [p0(1-p0) + p1(1-p1)] / (p0-p1)^2
#       OIS_n = 2 × n_arm                （参加者数）
#       参考: OIS_events ≈ 2 × n_arm × p̄  (p̄ = (p0+p1)/2)
#     連続結果: ois_delta と ois_sd を指定
#       n_arm = 2 × (z_alpha/2 + z_beta)^2 × sigma^2 / delta^2
#       OIS_n = 2 × n_arm
#     既定: ois_alpha = 0.05 (両側), ois_beta = 0.20 (検出力 80%)
#
#   ois_p1 の導出 (二値):
#     Core GRADE 2 本文 6 ページ目 逐語:
#       "For binary outcomes, these involve specifying the acceptable error
#        rates: alpha (typically 0.05) and beta (typically 0.20), the control
#        group event rate (chosen from the context), and a modest relative risk
#        reduction, typically 20% or 25%."
#     すなわち二値の OIS は MID ではなく「控えめな相対リスク減少」で決める。
#     pmatools は ois_rrr (既定 0.20) を用いて ois_p1 を導出する。
#     ois_p1 を明示指定した場合はそちらが優先される。
#     連続アウトカムは同じ段落で書き分けられており ("by specifying the smallest
#     difference between intervention and control that one would want to avoid
#     missing (ie, the MID)")、従来どおり MID を ois_delta に使う。
#
#   Direction of the binary OIS alternative rate (v0.5.1):
#     Core GRADE 2 writes "reduction" because its worked example has an
#     UNDESIRABLE event. For an outcome whose events are desirable (response,
#     remission) a benefit is an INCREASE in the event rate, and powering the
#     OIS against p0 * (1 - rrr) targets the wrong tail. `small_values` decides
#     the sign and the pooled effect is reported alongside it; see
#     .ois_target_increase(). With small_values = NULL the pre-0.5.1
#     behaviour (p0 * (1 - rrr)) is preserved exactly.
#
#   ois_sd の自動導出 (連続, v0.5.1):
#     .calc_ois() は連続アウトカムで ois_delta と ois_sd の両方を要求するが、
#     ois_sd は利用者が入力しなければ NULL のままだった。結果として連続
#     アウトカムの OIS は黙って計算不能になっていた。ois_sd 未指定時は
#     compute_pooled_sd() から導出し、導出した旨を note に書く。

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
                               ois_rrr            = 0.20,
                               # Outcome direction, as collected in the app's
                               # Step 2 and already forwarded to assess_rob():
                               # "desirable" / "undesirable" / NULL. It decides
                               # which way the modest RRR moves the OIS
                               # alternative rate, and the wording of the
                               # large-effect note.
                               small_values       = NULL,
                               threshold_internal = NULL,
                               threshold_kind     = NULL,
                               threshold_ard      = NULL,
                               threshold_p0       = NULL,
                               # UNUSED (kept only so that grade_meta() can
                               # forward its full argument list without a
                               # special case). The rating target and the
                               # threshold type reach this function ALREADY
                               # RESOLVED, as threshold_for_imprecision: Core
                               # GRADE 2 Fig 2 picks the threshold, and
                               # rating_target.R hands the chosen value down.
                               # Reading rating_target / threshold_type here
                               # would risk a second, divergent resolution.
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
      judgment = "not_serious",
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

  # The +/-MID zone is evaluated whenever a MID exists, even when the rating
  # threshold is the null: Core GRADE 2 (p6) explicitly extends the two-level
  # consideration ("the CI also includes clearly important harm") to the
  # null-threshold path. On that path the -1 / -0 decision still uses the null;
  # only the "crosses BOTH thresholds -> rate down two levels" branch consults
  # +/-MID.
  mid_zone <- if (has_threshold) {
    thr_eff
  } else if (.has_mid(threshold_internal)) {
    threshold_internal
  } else {
    NULL
  }
  has_mid_zone <- !is.null(mid_zone)

  if (has_mid_zone) {
    crosses_lower_threshold <- (lower < -mid_zone) && (upper > -mid_zone)
    crosses_upper_threshold <- (lower <  mid_zone) && (upper >  mid_zone)
    crosses_both_thresholds <- crosses_lower_threshold && crosses_upper_threshold
    crosses_one_threshold   <- xor(crosses_lower_threshold, crosses_upper_threshold)
    within_thresholds       <- (lower >= -mid_zone) && (upper <= mid_zone)
    beyond_thresholds       <- (upper <= -mid_zone) || (lower >= mid_zone)
  } else {
    crosses_lower_threshold <- NA
    crosses_upper_threshold <- NA
    crosses_both_thresholds <- NA
    crosses_one_threshold   <- NA
    within_thresholds       <- NA
    beyond_thresholds       <- NA
  }

  crosses_threshold <- if (has_threshold) {
    crosses_lower_threshold || crosses_upper_threshold
  } else {
    # Null threshold (target = non-null effect, or no MID available).
    crosses_null
  }

  # Defensive: treat NA as NULL
  if (!is.null(ois_events) && (length(ois_events) == 0 || is.na(ois_events))) ois_events <- NULL
  if (!is.null(ois_n)      && (length(ois_n)      == 0 || is.na(ois_n)))      ois_n      <- NULL
  if (!is.null(ois_p0)     && (length(ois_p0)     == 0 || is.na(ois_p0)))     ois_p0     <- NULL
  if (!is.null(ois_p1)     && (length(ois_p1)     == 0 || is.na(ois_p1)))     ois_p1     <- NULL
  if (!is.null(ois_delta)  && (length(ois_delta)  == 0 || is.na(ois_delta)))  ois_delta  <- NULL
  if (!is.null(ois_sd)     && (length(ois_sd)     == 0 || is.na(ois_sd)))     ois_sd     <- NULL

  # The summary measure and the pooled effect are wanted twice: here, to decide
  # which way the modest RRR moves the OIS alternative rate, and below for
  # Fig 4's large-effect check. Computed once.
  sm       <- meta_obj$sm
  te_point <- .pooled_te(meta_obj)

  # Derive the OIS inputs that were not supplied explicitly.
  #   binary     : ois_p0 (control-arm risk) + ois_rrr (modest RRR, Core GRADE 2)
  #   continuous : ois_delta = MID           (Core GRADE 2, same paragraph)
  #                ois_sd    = pooled within-study SD, when not supplied
  threshold_used_note <- ""
  ois_direction   <- .ois_target_increase(small_values, te_point)
  ois_p1_derived  <- FALSE
  ois_sd_derived  <- FALSE
  # Where a derived ois_sd came from, in words. The pooled-SD wording is not
  # true of every derivation any more (the SMD takes 1), so the fact below
  # quotes this rather than re-asserting a provenance it cannot see.
  ois_sd_source   <- ""
  has_mid_for_ois <- !is.null(threshold_internal) && !is.na(threshold_internal) &&
                     threshold_internal != 0
  if (is.null(ois_events) && is.null(ois_n)) {
    if (outcome_type == "relative") {
      # ARD Threshold converted to the ratio scale: anchor ois_p0 to the same
      # baseline risk that was used for the conversion, for consistency.
      if (is.null(ois_p0) && has_mid_for_ois && !is.null(threshold_ard) &&
          !is.null(threshold_p0) && is.finite(threshold_p0)) {
        ois_p0 <- threshold_p0
        threshold_used_note <- sprintf(
          " (ois_p0 from threshold baseline risk = %.4f)", ois_p0
        )
      }
      # Auto-fall back ois_p0 to control-arm pooled proportion if missing.
      # Core GRADE 2 calls for "the control group event rate (chosen from the
      # context)", so the observed control-arm risk is the natural default.
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
        # Core GRADE 2 (p6): binary OIS uses "a modest relative risk reduction,
        # typically 20% or 25%" -- NOT the MID. The MID is reserved for the
        # continuous branch, which the same paragraph writes out separately.
        # Which SIDE of ois_p0 the alternative rate sits on is decided by
        # .ois_target_increase(); the paper's "reduction" wording assumes an
        # undesirable event and does not generalise on its own.
        rrr     <- .check_ois_rrr(ois_rrr)
        raw_p1  <- if (isTRUE(ois_direction$increase)) {
          ois_p0 * (1 + rrr)
        } else {
          ois_p0 * (1 - rrr)
        }
        ois_p1  <- max(min(raw_p1, 1 - 1e-6), 1e-6)
        clamped <- !isTRUE(all.equal(ois_p1, raw_p1))
        ois_p1_derived <- TRUE
        threshold_used_note <- paste0(threshold_used_note, sprintf(
          paste0(" (ois_p1 from a modest relative risk %s, ois_rrr = ",
                 "%.0f%%: ois_p1 = %.4f%s; direction: %s; Core GRADE 2 ",
                 "specifies an RRR rather than the threshold for binary ",
                 "outcomes)"),
          if (isTRUE(ois_direction$increase)) "increase" else "reduction",
          100 * rrr, ois_p1,
          if (clamped) sprintf(
            paste0(", clamped into (0, 1) from %.4f -- the control-group risk ",
                   "is too high for a %.0f%% relative increase to stay a ",
                   "probability, so the OIS below is powered against the ",
                   "clamped rate"), raw_p1, 100 * rrr) else "",
          ois_direction$reason
        ))
      }
    } else {
      # Continuous outcomes: Core GRADE 2 keeps the MID here ("by specifying
      # the smallest difference between intervention and control that one would
      # want to avoid missing (ie, the MID)").
      if (is.null(ois_delta) && has_mid_for_ois) {
        ois_delta <- threshold_internal
        threshold_used_note <- sprintf(
          " (ois_delta = Threshold = %.4f)", ois_delta
        )
      }
      # .calc_ois() needs a standard deviation as well, and nothing ever
      # derived one: an ois_sd left blank meant the continuous OIS was silently
      # unavailable and Fig 4's large-effect path fell through to "OIS could
      # not be computed -> do not rate down". The pooled within-study SD is the
      # natural default and is already computed a few lines below for the
      # large-effect check, so derive it here rather than skipping the OIS.
      #
      # EXCEPT for the SMD, where delta and sigma must share a scale and the
      # SMD is ALREADY in within-study SD units: n_arm = 2(z_a+z_b)^2 sigma^2 /
      # delta^2 with the standardized delta of 0.20 and a RAW-scale sigma of,
      # say, 8 inflates the OIS by sigma^2 (64x here), which can flip Fig 4's
      # large-effect path from "not_serious" to "serious" via the "< 30% of OIS" rule.
      # For the SMD sigma is 1 by construction.
      if (is.null(ois_sd) && !is.null(ois_delta)) {
        if (identical(sm, "SMD")) {
          ois_sd <- 1
          ois_sd_derived <- TRUE
          ois_sd_source <- paste0(
            "1 by construction: the SMD is expressed in within-study SD ",
            "units, so the threshold above is already standardized and the ",
            "pooled SD must not be applied to it a second time")
          threshold_used_note <- paste0(
            threshold_used_note, " (ois_sd = ", ois_sd_source, ")")
        } else {
          sd_auto <- tryCatch(compute_pooled_sd(meta_obj), error = function(e) NULL)
          if (!is.null(sd_auto) && length(sd_auto) == 1L &&
              is.finite(sd_auto) && sd_auto > 0) {
            ois_sd <- as.numeric(sd_auto)
            ois_sd_derived <- TRUE
            ois_sd_source <- sprintf(
              paste0("%.4f, derived from the pooled within-study SD of the ",
                     "contributing studies -- not supplied by the caller"),
              ois_sd)
            threshold_used_note <- paste0(
              threshold_used_note, " (ois_sd = ", ois_sd_source, ")")
          }
        }
      }
    }
  }

  # OIS auto-calculation (explicit ois_events/ois_n take precedence)
  ois_calc_note <- ""
  if (is.null(ois_events) && is.null(ois_n)) {
    auto_ois <- .calc_ois(outcome_type, ois_alpha, ois_beta,
                          ois_p0, ois_p1, ois_delta, ois_sd)
    # `.calc_ois()` returns type = "n" for binary outcomes too (Core GRADE 2
    # Fig 4 compares participants, not events).
    if (!is.null(auto_ois)) {
      ois_calc_note <- paste0(auto_ois$formula, threshold_used_note)
      if (auto_ois$type == "events") ois_events <- auto_ois$value
      if (auto_ois$type == "n")      ois_n      <- auto_ois$value
    }
  }

  ois_info <- .compute_ois_pct(meta_obj, ois_events, ois_n)
  ois_pct  <- ois_info$pct
  ois_met  <- if (is.na(ois_pct)) NA else (ois_pct >= 1.0)

  # When the OIS is unavailable, name the input that was missing. Fig 4's
  # large-effect path used to report a bare "OIS could not be computed", which
  # reads as a property of the evidence rather than of the arguments supplied.
  ois_missing_reason <- if (!is.na(ois_pct)) {
    NULL
  } else if (is.null(ois_events) && is.null(ois_n)) {
    miss <- character()
    if (identical(outcome_type, "relative")) {
      if (is.null(ois_p0)) {
        miss <- c(miss, paste0("ois_p0, the control-group event rate (no ",
                               "arm-level event counts to derive it from)"))
      }
      if (is.null(ois_p1)) miss <- c(miss, "ois_p1, the alternative event rate")
    } else {
      if (is.null(ois_delta)) {
        miss <- c(miss, paste0("ois_delta, the smallest difference worth ",
                               "detecting (no Threshold was supplied)"))
      }
      if (is.null(ois_sd)) {
        miss <- c(miss, paste0("ois_sd, the pooled SD (not supplied, and it ",
                               "could not be derived from the study data)"))
      }
    }
    if (!length(miss)) {
      miss <- paste0("the OIS target could not be derived from the inputs ",
                     "supplied (outcome_type = '", outcome_type, "')")
    }
    paste0("missing ", paste(miss, collapse = " and "))
  } else {
    paste0("an OIS target was computed but the observed total is unknown: ",
           "the analysis carries no complete arm-level sample sizes ",
           "(n.e / n.c)")
  }

  # --- Core GRADE 2 Fig 4 -------------------------------------------------
  # `sm` and `te_point` were computed above, before the OIS derivation.
  is_binary <- .is_binary_outcome(meta_obj)
  large     <- .is_implausibly_large(te_point, sm,
                                     sd_pooled = if (is_binary) NULL
                                                 else compute_pooled_sd(meta_obj))
  ci_ratio     <- .ci_ratio(lower, upper, sm)
  ci_ratio_cut <- .ci_ratio_cut(sm)
  n_total      <- .total_n_strict(meta_obj)

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
    ois_missing_reason      = ois_missing_reason,
    # These two are rendered into the Fig 4 path string a reviewer reads, so
    # they say "Threshold" -- pmatools' own term for the band, and the word on
    # the Configuration tab that set it. They used to say "MID"; the concept is
    # the same and the vocabulary was not.
    threshold_label         = if (has_threshold) "the +/-Threshold band"
                              else "the null threshold",
    two_level_label         = if (has_threshold) {
                                "TWO thresholds (important benefit and important harm)"
                              } else {
                                paste0("BOTH Thresholds (+/-Threshold) -- the CI is ",
                                       "consistent with benefit and with clearly ",
                                       "important harm (Core GRADE 2, null-threshold ",
                                       "path)")
                              },
    sm                      = sm
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
  } else if (!is.na(ois_pct) && ois_pct < 0.30) {
    # Fig 4's node reads "N<30% of OIS"; the decision in
    # .classify_imprecision() uses a strict `<`, and this label must agree.
    sprintf("OIS not met; observed %d / target %d %s = %.0f%% (< 30%%)",
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

  # The +/-MID zone description is reported whenever a MID exists. On the
  # null-threshold path it is informational for -1/-0 but decisive for -2.
  # The variable keeps the internal name; the string a reviewer reads says
  # "Threshold", like every other user-facing string in this file.
  mid_suffix <- if (has_threshold) {
    ""
  } else {
    " [+/-Threshold zone; rating threshold = null]"
  }
  thresh_str <- if (!has_mid_zone) {
    ""
  } else if (isTRUE(crosses_both_thresholds)) {
    paste0("; crosses BOTH Thresholds", mid_suffix)
  } else if (isTRUE(crosses_one_threshold)) {
    paste0("; crosses one Threshold", mid_suffix)
  } else if (isTRUE(within_thresholds)) {
    paste0("; within Threshold (trivial effect)", mid_suffix)
  } else {
    paste0("; beyond Threshold (definitively important effect)", mid_suffix)
  }

  # Fig 4's "Yes" branch offers TWO reasons to consider rating down two
  # levels, and pmatools automates only the first. Verbatim:
  #   "Consider rating down two levels if:
  #    - CI crosses two thresholds-eg, both important benefit and important
  #      harm
  #    - Most appropriate plain language description of results suggests more
  #      uncertainty-eg, "may" rather than "likely" (assuming no concern
  #      related to other 4 grade domains)"
  # The second condition is a judgment about wording, not a computation, so it
  # is surfaced rather than applied.
  two_level_manual <- if (isTRUE(crosses_threshold) &&
                          !isTRUE(crosses_both_thresholds)) {
    paste0(
      " [Second Fig 4 two-level condition NOT auto-assessed: Core GRADE 2 ",
      "also says to consider rating down two levels when the 'most ",
      "appropriate plain language description of results suggests more ",
      "uncertainty-eg, \"may\" rather than \"likely\"'. Read the plain ",
      "language summary in the SoF table (sof_table(style = 'bmj')) against ",
      "the message you intend to convey, and override with imprecision = ",
      "'serious' + imprecision_rationale if it applies.]"
    )
  } else ""

  # The rating target itself is appended by grade_meta() (it owns the Fig 2
  # derivation); here we only record which threshold Fig 4 was applied to.
  notes <- sprintf(
    "95%% CI %s; null = %g; crosses null = %s%s; %s%s | %s%s",
    ci_str, null_disp,
    if (crosses_null) "YES" else "no",
    thresh_str,
    ois_str,
    if (nchar(ois_calc_note) > 0) paste0(" (", ois_calc_note, ")") else "",
    fig4$path,
    two_level_manual
  )

  # Structured companions to the sentence above. The Fig 4 path and the two
  # yes/no facts exist so a caller can branch on the path WITHOUT re-parsing
  # the prose (which is what "sub('^.*Fig 4 path: ', '', notes)" downstream
  # amounts to); the prose stays authoritative and unchanged.
  facts <- .facts(
    .fact("confidence_interval", "95% confidence interval", ci_str),
    .fact("crosses_null", "Crosses the null", if (crosses_null) "yes" else "no"),
    if (has_mid_zone) {
      .fact("threshold_position", "Position relative to the threshold",
            sub("^; ", "", thresh_str))
    } else NULL,
    .fact(
      "ois", "Optimal information size",
      {
        detail <- if (is.na(ois_pct)) {
          "not specified"
        } else {
          sprintf("observed %d / target %d %s = %.0f%%",
                  ois_info$observed, ois_info$target, ois_info$unit,
                  100 * ois_pct)
        }
        # Fig 4 only consults the OIS on one branch; everywhere else the
        # figures are informational, and ois_str says so in the notes.
        if (isTRUE(fig4$ois_used)) {
          detail
        } else {
          paste0("not applied on this Fig 4 path; ", detail)
        }
      },
      ois_pct
    ),
    if (ois_p1_derived) {
      .fact("ois_target_rate", "OIS alternative event rate",
            sprintf("%.4f (%s on a control-group risk of %.4f) -- %s",
                    ois_p1,
                    if (isTRUE(ois_direction$increase)) "an increase"
                    else "a reduction",
                    ois_p0, ois_direction$reason),
            ois_p1)
    } else NULL,
    if (ois_sd_derived) {
      .fact("ois_sd_source", "OIS standard deviation", ois_sd_source, ois_sd)
    } else NULL,
    .fact("fig4_path", "Core GRADE 2 Fig 4 path",
          sub("^Fig 4 path: ", "", fig4$path)),
    .fact("ois_used", "OIS approach applied",
          if (isTRUE(fig4$ois_used)) "yes" else "no"),
    # The same route as `fig4_path`, in the vocabulary the figure understands.
    .flow_path_fact(fig4$flow)
  )

  make_domain_row(
    domain   = "Imprecision",
    judgment = judgment,
    auto     = TRUE,
    notes    = notes,
    facts    = facts
  )
}

# --------------------------------------------------------------------------
# OIS 自動計算
# --------------------------------------------------------------------------

# Validate the modest relative risk reduction used for the binary OIS
# (Core GRADE 2: "a modest relative risk reduction, typically 20% or 25%").
.check_ois_rrr <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is.numeric(x) ||
      !is.finite(x) || x <= 0 || x >= 1) {
    rlang::abort(paste0(
      "ois_rrr must be a single relative risk reduction in (0, 1), e.g. 0.20 ",
      "for a 20% RRR (Core GRADE 2: 'a modest relative risk reduction, ",
      "typically 20% or 25%')."
    ))
  }
  as.numeric(x)
}

# Which side of the control-group risk does the binary OIS alternative rate
# sit on? Returns list(increase = <logical>, reason = <character>).
#
# WHY THIS IS NOT SIMPLY "REDUCTION"
# ----------------------------------
# `ois_rrr` is Core GRADE 2's "modest relative risk reduction". The paper says
# *reduction* because its worked example has an undesirable event: the good
# intervention makes the event rarer. Two facts decide the sign in general.
#
#   1. `small_values` describes the OUTCOME VALUE, not the event. For a binary
#      outcome the value is the event rate, so small_values = "undesirable"
#      ("a smaller value is worse") means the EVENTS ARE THE GOOD THING --
#      response, remission -- and a beneficial intervention makes them MORE
#      common. small_values = "desirable" (mortality, relapse) is the mirror.
#   2. The pooled effect says which way THIS body of evidence actually moves
#      the event rate. On Fig 4's OIS path the CI is clear of the chosen
#      threshold, so the sign of the pooled effect is unambiguous.
#
# WHICH OF THE TWO DECIDES, AND WHY IT IS THE DIRECTION
# -----------------------------------------------------
# `small_values` decides; the pooled effect is consulted, reported, and does
# NOT override it. The OIS is an a-priori power calculation: it asks how many
# participants a body of evidence would need to detect the smallest effect
# worth not missing. That effect is a property of the QUESTION -- a modest
# benefit -- and the direction of benefit is exactly what `small_values`
# states. Letting the observed estimate pick the side would make the target
# partly data-driven, and, worse, would collapse the distinction this argument
# exists to draw: with a pooled ratio above the null, "desirable" and
# "undesirable" would then produce the identical OIS.
#
# The pooled effect still earns its place in the reason string. When the two
# agree, the sentence reads as one fact ("the intervention increases a
# desirable event"). When they disagree the evidence describes a HARM on this
# outcome, and the note says so, because the reader is then looking at an OIS
# powered for the benefit direction while the estimate runs the other way --
# which is worth seeing, not worth silently papering over.
#
# small_values = NULL keeps the pre-0.5.1 behaviour EXACTLY -- p0 * (1 - rrr),
# whatever the data show -- so no existing caller changes silently.
.ois_target_increase <- function(small_values, te_point) {
  if (is.null(small_values) || length(small_values) != 1L ||
      is.na(small_values) || !nzchar(as.character(small_values))) {
    return(list(
      increase = FALSE,
      reason   = paste0("no outcome direction supplied (small_values = NULL), ",
                        "so Core GRADE 2's relative risk REDUCTION is used as ",
                        "written")
    ))
  }
  sv <- as.character(small_values)
  # "undesirable" = a smaller outcome value is bad = the events are desirable.
  events_desirable <- identical(sv, "undesirable")
  increase <- events_desirable
  te_known <- !is.null(te_point) && length(te_point) == 1L &&
              !is.na(te_point) && is.finite(te_point) && te_point != 0
  base <- sprintf(
    paste0("small_values = '%s' (events are %s), so a benefit is a modest %s ",
           "in the event rate"),
    sv,
    if (events_desirable) "desirable" else "undesirable",
    if (increase) "increase" else "reduction")
  if (!te_known) {
    return(list(increase = increase,
                reason   = paste0(base, "; no usable pooled effect to compare ",
                                  "it with")))
  }
  observed_increase <- te_point > 0
  list(
    increase = increase,
    reason   = paste0(
      base,
      if (identical(observed_increase, events_desirable)) {
        sprintf("; the pooled effect is %s the null, i.e. the intervention %s",
                if (observed_increase) "above" else "below",
                if (observed_increase) "increases a desirable event"
                else "reduces an undesirable event")
      } else {
        sprintf(paste0("; NOTE the pooled effect runs the other way (%s the ",
                       "null), i.e. this evidence describes a harm -- the OIS ",
                       "is still powered for the benefit direction"),
                if (observed_increase) "above" else "below")
      })
  )
}

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
    total_n      <- ceiling(2 * n_arm)
    total_events <- ceiling(2 * n_arm * p_bar)
    # Core GRADE 2 Fig 4 compares PARTICIPANTS with the OIS ("N=number of
    # participants"), so the binary OIS is returned as a target sample size.
    # The implied event count is reported alongside it for information.
    formula_str  <- sprintf(
      paste0("OIS: p0=%.3f, p1=%.3f, alpha=%.2f, beta=%.2f -> target N=%d ",
             "participants (implies ~%d events; Core GRADE 2 Fig 4 compares ",
             "participants)"),
      ois_p0, ois_p1, ois_alpha, ois_beta, total_n, total_events
    )
    return(list(type = "n", value = total_n, formula = formula_str))
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
# Returns list(pct, observed, target, unit). `unit` is "N" (participants,
# Core GRADE 2 Fig 4: "N=number of participants") or "events" when the caller
# supplied `ois_events` explicitly (kept for backward compatibility).
# When neither is computable, all four fields are NA.
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
# Wording for the ratio-scale magnitude 1 - exp(-|log ratio|).
#
# The number is symmetric: RR 0.60 and RR 1.667 both give 40%, and Core GRADE
# 2's ">40% / >30%" cut-offs are applied to it either way. The WORDING is not
# symmetric, and up to v0.5.0 this always said "relative risk reduction", so a
# pooled OR of 2.33 -- an increase -- was reported as a 57% reduction. Above
# the null the same magnitude is the reduction seen with the two arms
# exchanged, and the label says so rather than renaming 57% as an increase
# (as an increase, RR 1.667 is +67%, a different number).
.rrr_direction_label <- function(te, rrr) {
  if (!is.na(te) && te > 0) {
    sprintf(paste0("relative risk increase, equivalent to a %.0f%% reduction ",
                   "with the arms exchanged"), 100 * rrr)
  } else {
    sprintf("relative risk reduction %.0f%%", 100 * rrr)
  }
}

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
    lab <- .rrr_direction_label(te, rrr)
    if (rrr > 0.40) {
      return(list(large = TRUE, level = "certain",
                  note = sprintf(
                    "effect implausibly large (%s > 40%%)", lab)))
    }
    if (rrr > 0.30) {
      return(list(large = TRUE, level = "possible",
                  note = sprintf(
                    "effect possibly implausibly large (%s > 30%%)", lab)))
    }
    return(list(large = FALSE, level = NA_character_,
                note = sprintf("effect moderate (%s <= 30%%)", lab)))
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
#
# CAVEAT: Core GRADE 2 names ONLY those two measures. Applying the risk-ratio
# value of 3 to HR / IRR / RoM is a pmatools extrapolation with no support in
# the source; it rests on nothing more than these being ratio measures read on
# a comparable scale. The domain notes state it whenever the cut-off decides a
# two-level downgrade.
.ci_ratio_cut <- function(sm) {
  if (is.null(sm)) return(NA_real_)
  switch(sm, "OR" = 2.5, "RR" = 3, "HR" = 3, "IRR" = 3, "RoM" = 3, NA_real_)
}

# Which effect measures get the CI-ratio cut-off straight from Core GRADE 2.
.CI_RATIO_SOURCED_SM <- c("OR", "RR")

# --------------------------------------------------------------------------
# Flowchart node vocabulary (inst/figures/impre.svg)
#
# See the note on .ROB_FIG2_NODE_IDS in domain_rob.R. The three OIS outcomes
# are drawn as three edges out of one OIS node rather than one edge per
# sub-rule: the sub-rule that fired is already recorded verbatim by the
# "fig4_path" fact, and splitting the node six ways made the picture harder
# to read than the sentence it was meant to replace.
.IMPRE_FIG4_NODE_IDS <- c(
  "pma-impre-node-crosses",
  "pma-impre-edge-crosses-yes",
  "pma-impre-node-both",
  "pma-impre-edge-both-no",
  "pma-impre-leaf-down1",
  "pma-impre-edge-both-yes",
  "pma-impre-leaf-down2-both",
  "pma-impre-edge-crosses-no",
  "pma-impre-node-large",
  "pma-impre-edge-large-no",
  "pma-impre-leaf-nodown-moderate",
  "pma-impre-edge-large-yes",
  "pma-impre-node-ois",
  "pma-impre-edge-ois-nodown",
  "pma-impre-leaf-nodown-ois",
  "pma-impre-edge-ois-down1",
  "pma-impre-leaf-down1-ois",
  "pma-impre-edge-ois-down2",
  "pma-impre-leaf-down2-ois"
)

# Shared prefixes, so a change to the shape of the figure is a change in one
# place rather than in each of the nine returns of .classify_imprecision().
.IMPRE_FLOW_CROSSES <- c("pma-impre-node-crosses",
                         "pma-impre-edge-crosses-yes",
                         "pma-impre-node-both")
.IMPRE_FLOW_LARGE   <- c("pma-impre-node-crosses",
                         "pma-impre-edge-crosses-no",
                         "pma-impre-node-large")
.IMPRE_FLOW_OIS     <- c(.IMPRE_FLOW_LARGE,
                         "pma-impre-edge-large-yes",
                         "pma-impre-node-ois")

# 二値 (event ベース) アウトカムか。metabin 由来のイベント数があるか、
# 効果指標が二値向けなら二値扱い。Fig 4 の連続／二値の分岐に使う
# (grade_meta の outcome_type は OIS 計算用の "relative"/"absolute" であって
#  連続／二値の区別ではないため、ここでは使わない)。
#
# NOTE — RoM. RoM is deliberately NOT listed here: a ratio of means is a
# continuous-outcome summary, so Fig 4's continuous branch (N >= 800 rule of
# thumb, N < 30% of OIS) is the right one and the CI-ratio branch never runs
# for it. .is_implausibly_large() nevertheless routes RoM through the BINARY
# "relative risk reduction > 30% / > 40%" rule, because 1 - exp(-|log RoM|) is
# a usable magnitude proxy on a ratio scale and Core GRADE 2 offers no
# "large effect" definition for continuous outcomes at all. The two functions
# therefore classify RoM differently on purpose:
#   .is_binary_outcome()      RoM -> continuous  (drives the Fig 4 branch)
#   .is_implausibly_large()   RoM -> ratio rule  (drives the magnitude label)
# Both paths label their reasoning in the notes; neither is claimed to be
# Core GRADE 2's.
.is_binary_outcome <- function(meta_obj) {
  if (!is.null(meta_obj$event.e) && length(meta_obj$event.e) > 0) return(TRUE)
  sm <- meta_obj$sm
  if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "IRR", "RD", "ARD")) return(TRUE)
  FALSE
}

# 総サンプルサイズ（連続アウトカムの "N >= OIS (or 800)" 判定に使う）
#
# 意図的に strict にしてある。800 の rule of thumb は「400 patients per group」なので、
# 二群の実測合計が揃っているときにしか適用できない。片群しかない meta
# （metaprop / metamean など、n.e / n.c を持たず meta_obj$n だけを持つ
# オブジェクト）では NA を返し、meta_obj$n へのフォールバックはしない。
# 同じファイルの .compute_ois_pct() も n.e / n.c が揃わなければ OIS を
# 計算しないので、そちらと足並みを揃えている。
# 表示用の寛容版（N 列に出す総参加者数）は sof_table.R の .total_n() のほう。
.total_n_strict <- function(meta_obj) {
  n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA_real_
  n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA_real_
  if (is.na(n_e) || is.na(n_c)) return(NA_real_)
  n_e + n_c
}

# --------------------------------------------------------------------------
# 判定分類 (Core GRADE 2 Fig 4)
#
# 返り値: list(judgment, path, ois_used, flow)
#   path     — 通過した Fig 4 の経路（notes に記録し、監査可能にする）
#   ois_used — OIS 経路を実際に使ったか（notes の表現を切り替える）
#   flow     — the same route as inst/figures/impre.svg node ids, so the
#              caller can record it as the "flow_path" fact without parsing
#              `path` back out of the prose (see .IMPRE_FIG4_NODE_IDS)
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
                                  ois_missing_reason = NULL,
                                  threshold_label = "the Threshold",
                                  two_level_label =
                                    "TWO thresholds (important benefit and important harm)",
                                  sm = NULL) {
  ci_ratio_src_note <- if (!is.null(sm) && !sm %in% .CI_RATIO_SOURCED_SM) {
    sprintf(paste0(" [Core GRADE 2 Fig 4 names CI-ratio cut-offs only for ",
                   "relative risk (3) and odds ratio (2.5); applying 3 to %s ",
                   "is a pmatools extrapolation]"), sm)
  } else ""
  out <- function(judgment, path, ois_used = FALSE, flow = character(0)) {
    list(judgment = judgment,
         path     = paste0("Fig 4 path: ", path),
         ois_used = ois_used,
         flow     = flow)
  }

  # --- Yes branch: CI crosses the chosen threshold -------------------------
  if (isTRUE(crosses_threshold)) {
    if (isTRUE(crosses_both_thresholds)) {
      return(out("very_serious", sprintf(
        "CI crosses %s -> rate down; CI crosses %s -> rate down two levels",
        threshold_label, two_level_label),
        flow = c(.IMPRE_FLOW_CROSSES, "pma-impre-edge-both-yes",
                 "pma-impre-leaf-down2-both")))
    }
    return(out("serious", sprintf(paste0(
      "CI crosses %s -> rate down one level (sample size not considered on ",
      "this path)"), threshold_label),
      flow = c(.IMPRE_FLOW_CROSSES, "pma-impre-edge-both-no",
               "pma-impre-leaf-down1")))
  }

  # --- No branch: CI does not cross the threshold --------------------------
  if (!isTRUE(large$large)) {
    return(out("not_serious", sprintf(paste0(
      "CI does not cross %s -> %s -> do not rate down (OIS not applied)"),
      threshold_label, large$note),
      flow = c(.IMPRE_FLOW_LARGE, "pma-impre-edge-large-no",
               "pma-impre-leaf-nodown-moderate")))
  }

  # Large effect -> OIS approach
  prefix <- sprintf("CI does not cross %s -> %s -> OIS approach",
                    threshold_label, large$note)

  if (is_binary) {
    if (!is.na(ci_ratio) && !is.na(ci_ratio_cut) && ci_ratio >= ci_ratio_cut) {
      return(out("very_serious", sprintf(paste0(
        "%s (binary): CI ratio %.2f >= %.1f -> consider rating down two ",
        "levels%s"), prefix, ci_ratio, ci_ratio_cut, ci_ratio_src_note),
        ois_used = TRUE,
        flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-down2",
                 "pma-impre-leaf-down2-ois")))
    }
  } else {
    # Continuous rule of thumb: 400 patients per group (total sample size 800).
    if (!is.na(n_total) && n_total >= 800) {
      return(out("not_serious", sprintf(paste0(
        "%s (continuous): total N = %.0f >= 800 (rule of thumb) -> do not ",
        "rate down"), prefix, n_total), ois_used = TRUE,
        flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-nodown",
                 "pma-impre-leaf-nodown-ois")))
    }
  }

  if (is.na(ois_met)) {
    return(out("not_serious", sprintf(
      "%s: OIS could not be computed (%s) -> do not rate down",
      prefix,
      if (is.null(ois_missing_reason)) "inputs unavailable"
      else ois_missing_reason),
      ois_used = TRUE,
      flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-nodown",
               "pma-impre-leaf-nodown-ois")))
  }
  if (isTRUE(ois_met)) {
    return(out("not_serious", sprintf("%s: N >= OIS -> do not rate down", prefix),
               ois_used = TRUE,
               flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-nodown",
                        "pma-impre-leaf-nodown-ois")))
  }
  # N < OIS
  if (!is_binary && !is.na(ois_pct) && ois_pct < 0.30) {
    return(out("very_serious", sprintf(paste0(
      "%s (continuous): N < 30%% of OIS -> consider rating down two levels"),
      prefix), ois_used = TRUE,
      flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-down2",
               "pma-impre-leaf-down2-ois")))
  }
  out("serious", sprintf("%s: N < OIS -> rate down one level", prefix),
      ois_used = TRUE,
      flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-down1",
               "pma-impre-leaf-down1-ois"))
}
