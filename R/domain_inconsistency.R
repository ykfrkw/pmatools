# domain_inconsistency.R — 非一貫性ドメイン評価
#
# BMJ 2025 Core GRADE 3, Fig 2 フローチャートに準拠:
#
#   Step 1. Are there IMPORTANT DIFFERENCES in point estimates AND limited CI overlap?
#     → 臨床的・視覚的判断。I² や Q 検定は補助情報に過ぎない。
#
#   NO  → Do not rate down (judgment = "no")
#
#   YES → Evaluate point estimates in relation to chosen clinical threshold
#
#     Majority on one side of threshold → Do not rate down (judgment = "no")
#
#     Substantial proportion on OPPOSITE sides of threshold →
#       Is inconsistency explained by credible subgroup analyses?
#         YES → Present results separately per subgroup (judgment = "no" + note)
#         NO  → Rate down (judgment = "serious")
#
# 入力形式:
#   (a) inconsistency スカラ ("no"/"some"/"serious"/"very_serious"):
#       フローチャートを使わず、ユーザー判定をそのまま使用
#   (b) フローチャートパラメータ群:
#       inconsistency_ci_diff           : "yes" / "no"
#       inconsistency_threshold_side    : "majority_one_side" / "opposite_sides"
#       inconsistency_subgroup_explained: "yes" / "no"
#   (c) 全パラメータ NULL → I² 統計に fallback（後方互換、警告付き）
#
# I² / tau² / Q 統計は常に計算してノートに記録するが、
# 判定のドライバーにはしない（補助情報として提示）。

assess_inconsistency <- function(meta_obj,
                                 inconsistency                    = NULL,
                                 inconsistency_ci_diff            = NULL,
                                 inconsistency_threshold_side     = NULL,
                                 inconsistency_subgroup_explained = NULL) {

  # --- I² 統計（常に計算・ノート用） ---
  i2_pct <- if (!is.null(meta_obj$I2) && !is.na(meta_obj$I2)) {
    meta_obj$I2 * 100
  } else NA_real_

  tau2   <- meta_obj$tau2
  pval_q <- meta_obj$pval.Q

  stat_note <- sprintf(
    "I\u00b2 = %.1f%%, tau\u00b2 = %.4f, Q p = %.3f (supplementary; not the primary criterion)",
    if (is.na(i2_pct)) 0 else i2_pct,
    if (is.null(tau2) || is.na(tau2)) 0 else tau2,
    if (is.null(pval_q) || is.na(pval_q)) 1 else pval_q
  )

  # ─── (a) スカラ指定: フローチャートをスキップ ──────────────────────────
  if (!is.null(inconsistency)) {
    validate_grade_level(inconsistency, "inconsistency")
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = inconsistency,
      auto     = FALSE,
      notes    = paste0(
        "Overall judgment provided by user (scalar; flowchart not applied). ",
        stat_note
      )
    ))
  }

  # ─── (b) フローチャート: Step 1 ───────────────────────────────────────
  if (is.null(inconsistency_ci_diff)) {
    # 全パラメータ NULL → auto-detect from data
    return(.auto_inconsistency(meta_obj, i2_pct, pval_q, stat_note))
  }

  if (!inconsistency_ci_diff %in% c("yes", "no")) {
    rlang::abort("inconsistency_ci_diff must be 'yes' or 'no'.")
  }

  # Step 1: No important differences → Do not rate down
  if (inconsistency_ci_diff == "no") {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        "FLOWCHART Step 1: No important differences in point estimates / ",
        "adequate CI overlap \u2192 do not rate down. | ", stat_note
      )
    ))
  }

  # Step 1: YES → evaluate threshold
  if (is.null(inconsistency_threshold_side)) {
    rlang::abort(paste0(
      "inconsistency_ci_diff = 'yes' requires inconsistency_threshold_side = ",
      "'majority_one_side' or 'opposite_sides'."
    ))
  }

  if (!inconsistency_threshold_side %in% c("majority_one_side", "opposite_sides")) {
    rlang::abort(
      "inconsistency_threshold_side must be 'majority_one_side' or 'opposite_sides'."
    )
  }

  # Majority on one side → Do not rate down
  if (inconsistency_threshold_side == "majority_one_side") {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        "FLOWCHART: Important CI differences exist, but majority of point estimates ",
        "are on one side of the clinical threshold \u2192 do not rate down. | ",
        stat_note
      )
    ))
  }

  # Opposite sides → check subgroup explanation
  if (is.null(inconsistency_subgroup_explained)) {
    rlang::abort(paste0(
      "inconsistency_threshold_side = 'opposite_sides' requires ",
      "inconsistency_subgroup_explained = 'yes' or 'no'."
    ))
  }

  if (!inconsistency_subgroup_explained %in% c("yes", "no")) {
    rlang::abort("inconsistency_subgroup_explained must be 'yes' or 'no'.")
  }

  if (inconsistency_subgroup_explained == "yes") {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        "FLOWCHART: Opposite-sided estimates; inconsistency IS explained by ",
        "credible subgroup \u2192 do not rate down; present subgroup results separately. | ",
        stat_note
      )
    ))
  }

  # Opposite sides + NOT explained → Rate down
  make_domain_row(
    domain   = "Inconsistency",
    judgment = "serious",
    auto     = FALSE,
    notes    = paste0(
      "FLOWCHART: Opposite-sided estimates; inconsistency NOT explained by ",
      "subgroup \u2192 rate down. | ", stat_note
    )
  )
}

# --------------------------------------------------------------------------
# 自動判定: inconsistency_ci_diff / threshold_side を data から推定
# --------------------------------------------------------------------------
.auto_inconsistency <- function(meta_obj, i2_pct, pval_q, stat_note) {

  # --- Step 1 proxy: important differences in point estimates AND limited CI overlap?
  # Use I² > 25% OR Q p < 0.10 as a proxy for "yes"
  has_i2 <- !is.na(i2_pct)
  ci_diff_yes <- has_i2 && (
    i2_pct > 25 ||
    (!is.null(pval_q) && !is.na(pval_q) && pval_q < 0.10)
  )

  if (!ci_diff_yes) {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "no",
      auto     = TRUE,
      notes    = paste0(
        "AUTO Step 1: No important heterogeneity detected (I\u00b2 \u2264 25% and Q p \u2265 0.10) ",
        "\u2192 do not rate down. | ", stat_note
      )
    ))
  }

  # --- Step 2 proxy: are most point estimates on one side of null?
  # meta$TE is always on a null = 0 scale:
  #   OR / RR / HR / IRR → log scale, so null = log(1) = 0
  #   MD / SMD           → raw scale, null = 0
  # Therefore TE > 0 correctly identifies "above null" for all measures.
  te_vec <- meta_obj$TE
  threshold_side <- if (!is.null(te_vec) && length(te_vec) >= 2) {
    pct_positive <- mean(te_vec > 0, na.rm = TRUE)
    if (pct_positive >= 0.75 || pct_positive <= 0.25) "majority_one_side" else "opposite_sides"
  } else {
    "majority_one_side"  # conservative: assume one side when data unavailable
  }

  side_note <- if (!is.null(te_vec)) {
    pct_pos <- round(mean(te_vec > 0, na.rm = TRUE) * 100)
    sprintf("AUTO Step 2: %d%% of study-level estimates TE > 0 \u2192 '%s'.",
            pct_pos, threshold_side)
  } else {
    "AUTO Step 2: Study-level TEs unavailable; assumed majority_one_side."
  }

  if (threshold_side == "majority_one_side") {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "some",
      auto     = TRUE,
      notes    = paste0(
        "AUTO Step 1: Heterogeneity detected (I\u00b2 > 25% or Q p < 0.10). ",
        side_note,
        " Majority on one side of null \u2192 some concern (not rate down per flowchart, ",
        "but I\u00b2 suggests heterogeneity). | ", stat_note
      )
    ))
  }

  # Opposite sides: rate down (cannot auto-check subgroup explanation)
  make_domain_row(
    domain   = "Inconsistency",
    judgment = "serious",
    auto     = TRUE,
    notes    = paste0(
      "AUTO Step 1: Heterogeneity detected. ",
      side_note,
      " Estimates on opposite sides of null; subgroup explanation not assessable automatically ",
      "\u2192 rate down. Supply inconsistency_subgroup_explained = 'yes' to override. | ",
      stat_note
    )
  )
}
