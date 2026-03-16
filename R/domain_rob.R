# domain_rob.R — Risk of Bias ドメイン処理
#
# BMJ 2025 Core GRADE 4, Fig 2 フローチャートに準拠:
#
#   Step 1. Is evidence DOMINATED by high-RoB studies?
#     Threshold (注釈): >65% = dominating; ≥55% = possibly dominating
#     rob_dominant_threshold: デフォルト 0.60
#
#   YES → Step 2. Check direction of bias（自動判定）
#     high-RoB 除外後の TE_low と全研究の TE_all を比較:
#       small_values = "undesirable": TE_all > TE_low → 高RoB が有利方向に誇張 → Rate down
#       small_values = "desirable"  : TE_all < TE_low → 高RoB が有利方向に誇張 → Rate down
#     逆方向の場合 → 効果を保守的にしている → Do not rate down
#
#   NO  → Appreciable low-RoB evidence exists
#         → Do not rate down (judgment = "no")
#         ※ NO 分岐の sub-question（高/低 RoB 間の効果差）はスキップ
#
# 入力形式:
#   (a) スカラ: "no" / "some" / "serious" / "very_serious"
#       → フローチャートをスキップ、ユーザー判定をそのまま使用
#   (b) 長さ k のベクタ: 各研究の RoB 判定 → フローチャート適用
#   (c) 文字列 (列名): meta_obj$data 内の列を参照 → フローチャート適用
#
# small_values の説明 (netmetaviz と統一):
#   "undesirable" : 小さい値が undesirable（例: response rate, OR for beneficial treatment）
#                    大きい値が好ましい。TE_all > TE_low で誇張と判定。
#   "desirable"   : 小さい値が desirable（例: mortality rate, symptom severity score）
#                    小さい値が好ましい。TE_all < TE_low で誇張と判定。
#   NULL          : 未指定。dominated の場合は保守的に rate down + 警告

assess_rob <- function(rob, meta_obj,
                       rob_dominant_threshold = 0.60,
                       small_values           = NULL) {
  k <- meta_obj$k

  # NULL → デフォルト "no"
  if (is.null(rob)) {
    return(make_domain_row(
      domain   = "Risk of bias",
      judgment = "no",
      auto     = FALSE,
      notes    = "Not assessed (rob = NULL). Assumed no concern."
    ))
  }

  # ベクタ / 列名の場合は RoB level を正規化
  # スカラ GRADE level かチェック（正規化後）
  if (length(rob) == 1 && is.character(rob)) {
    rob_norm <- .normalize_rob_level(rob)
    if (rob_norm %in% GRADE_LEVELS) {
      return(make_domain_row(
        domain   = "Risk of bias",
        judgment = rob_norm,
        auto     = FALSE,
        notes    = "Overall judgment provided by user (scalar; flowchart not applied)."
      ))
    }
    # 列名参照 → ベクタに展開
    col  <- rob
    data <- meta_obj$data
    if (is.null(data) || !col %in% names(data)) {
      rlang::abort(paste0(
        "rob = '", col, "' is not a recognized GRADE level and was not found as a column ",
        "in the meta object's data. Check column names with names(meta_obj$data)."
      ))
    }
    rob <- as.character(data[[col]])
  }

  # ベクタ: 正規化 + 長さ k チェック
  rob <- .normalize_rob_levels(rob)
  if (length(rob) != k) {
    rlang::abort(paste0(
      "rob must be a scalar GRADE level, a column name in meta_obj$data, ",
      "or a vector of length k (", k, "). Got length ", length(rob), "."
    ))
  }

  validate_grade_level(rob, "rob")
  .flowchart_rob(rob, meta_obj, rob_dominant_threshold, small_values)
}

# --------------------------------------------------------------------------
# フローチャート判定
# --------------------------------------------------------------------------
.flowchart_rob <- function(rob_vec, meta_obj, threshold, small_values) {

  # 高 RoB = "serious" または "very_serious"
  high_idx <- rob_vec %in% c("serious", "very_serious")
  n_high   <- sum(high_idx)
  n_total  <- length(rob_vec)

  # --- ランダム効果重みによる支配度計算 ---
  weights <- meta_obj$w.random
  if (!is.null(weights) && length(weights) == n_total && sum(weights) > 0) {
    pct_high_w <- sum(weights[high_idx]) / sum(weights)
    weight_note <- sprintf(
      "High-RoB studies: %d/%d (%.1f%% of random-effects weight)",
      n_high, n_total, pct_high_w * 100
    )
    dominated <- pct_high_w >= threshold
  } else {
    pct_high_w <- n_high / n_total
    weight_note <- sprintf(
      "High-RoB studies: %d/%d (%.1f%% by count; weights unavailable)",
      n_high, n_total, pct_high_w * 100
    )
    dominated <- pct_high_w >= threshold
  }

  tbl_note <- paste(
    paste0(names(table(rob_vec)), ": n=", as.integer(table(rob_vec))),
    collapse = "; "
  )

  # ─── NO 分岐: dominated でない → rate down しない ──────────────────────
  if (!dominated) {
    return(make_domain_row(
      domain   = "Risk of bias",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        "FLOWCHART: Not dominated (threshold ", round(threshold * 100), "%). ",
        weight_note, ". \u2192 Do not rate down. | ",
        tbl_note
      )
    ))
  }

  # ─── YES 分岐: dominated → バイアス方向を自動判定 ──────────────────────
  dir <- .assess_bias_direction(
    te_all       = meta_obj$TE.random,
    te_vec       = meta_obj$TE,
    se_vec       = meta_obj$seTE,
    low_idx      = !high_idx,
    small_values = small_values
  )

  judgment <- if (dir$inflates) "serious" else "no"

  make_domain_row(
    domain   = "Risk of bias",
    judgment = judgment,
    auto     = FALSE,
    notes    = paste0(
      "FLOWCHART: Dominated (threshold ", round(threshold * 100), "%). ",
      weight_note, ". ",
      dir$note, " | ",
      tbl_note
    )
  )
}

# --------------------------------------------------------------------------
# バイアス方向の自動判定
#
# Logic:
#   高RoB 研究を除外した推定値 (TE_low) と全研究の TE_all を比較する。
#   TE_all が TE_low より「好ましい方向」に偏っていれば、高RoB が効果を誇張している。
#     small_values = "undesirable": TE_all > TE_low → 誇張 → inflates = TRUE
#     small_values = "desirable"  : TE_all < TE_low → 誇張 → inflates = TRUE
#   逆方向: 高RoB が効果を保守的にしている → inflates = FALSE → rate down しない
# --------------------------------------------------------------------------
.assess_bias_direction <- function(te_all, te_vec, se_vec, low_idx, small_values) {

  n_low <- sum(low_idx)

  # small_values 未指定 → 保守的に rate down
  if (is.null(small_values)) {
    rlang::warn(paste0(
      "small_values is NULL; conservatively rating down RoB when dominated. ",
      "Specify small_values = 'desirable' or 'undesirable' for automatic direction check ",
      "(e.g., 'desirable' for mortality/severity; 'undesirable' for response/remission)."
    ))
    return(list(
      inflates = TRUE,
      note     = paste0(
        "small_values not specified; conservative rate-down applied. ",
        "TE(all) = ", round(te_all, 3), "."
      )
    ))
  }

  if (!small_values %in% c("desirable", "undesirable")) {
    rlang::abort("small_values must be 'desirable' or 'undesirable'.")
  }

  # low-RoB 研究が存在しない場合 → 保守的に rate down
  if (n_low == 0 || is.null(te_vec) || is.null(se_vec)) {
    return(list(
      inflates = TRUE,
      note     = paste0(
        "No low/some-RoB studies to compare with; conservative rate-down applied. ",
        "TE(all) = ", round(te_all, 3), "."
      )
    ))
  }

  # TE_low: high-RoB 除外後の inverse-variance 加重平均（fixed-effects プーリング）
  w_low  <- 1 / se_vec[low_idx]^2
  te_low <- sum(w_low * te_vec[low_idx]) / sum(w_low)

  diff_note <- sprintf(
    "TE(all studies) = %.3f; TE(excl. high-RoB) = %.3f; small_values = '%s'",
    te_all, te_low, small_values
  )

  if (small_values == "undesirable") {
    # 大きい値が desirable（response rate など）: TE_all > TE_low → 誇張
    inflates <- te_all > te_low
    dir_desc <- if (inflates) {
      "TE(all) > TE(excl. high-RoB) \u2192 high-RoB inflates toward favorable \u2192 rate down"
    } else {
      "TE(all) \u2264 TE(excl. high-RoB) \u2192 high-RoB does not inflate effect \u2192 do not rate down"
    }
  } else {
    # 小さい値が desirable（mortality など）: TE_all < TE_low → 誇張
    inflates <- te_all < te_low
    dir_desc <- if (inflates) {
      "TE(all) < TE(excl. high-RoB) \u2192 high-RoB inflates toward favorable \u2192 rate down"
    } else {
      "TE(all) \u2265 TE(excl. high-RoB) \u2192 high-RoB does not inflate effect \u2192 do not rate down"
    }
  }

  list(
    inflates = inflates,
    note     = paste0(diff_note, ". ", dir_desc)
  )
}

# --------------------------------------------------------------------------
# RoB level 正規化ヘルパー
# Cochrane RoB2 / 平語 → 内部 GRADE level ("no"/"some"/"serious"/"very_serious")
# --------------------------------------------------------------------------
.normalize_rob_level <- function(x) {
  aliases <- c(
    # Cochrane RoB2
    "No concerns"       = "no",
    "Some concerns"     = "some",
    "Serious concerns"  = "serious",
    "Critical concerns" = "very_serious",
    # Plain / alternate capitalisation
    "low"          = "no",    "Low"          = "no",
    "moderate"     = "some",  "Moderate"     = "some",
    "high"         = "serious", "High"       = "serious",
    "very high"    = "very_serious", "Very high" = "very_serious",
    # Internal (pass-through)
    "no"           = "no",
    "some"         = "some",
    "serious"      = "serious",
    "very_serious" = "very_serious"
  )
  if (x %in% names(aliases)) aliases[[x]] else x
}

.normalize_rob_levels <- function(rob_vec) {
  result <- vapply(rob_vec, .normalize_rob_level, character(1))
  unknown <- !result %in% GRADE_LEVELS
  if (any(unknown)) {
    rlang::abort(paste0(
      "Unrecognized RoB level(s): ", paste(unique(rob_vec[unknown]), collapse = ", "),
      ". Accepted values: 'no', 'some', 'serious', 'very_serious', or Cochrane RoB2 labels ",
      "('No concerns', 'Some concerns', 'Serious concerns', 'Critical concerns')."
    ))
  }
  unname(result)
}
