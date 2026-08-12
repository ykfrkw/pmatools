# rating_target.R — 確実性評価の「対象 (target)」決定
#
# BMJ 2025 Core GRADE 2: choosing the target of the certainty rating
#
# References:
#   Guyatt G, Zeng L, Brignardello-Petersen R, et al.
#     Core GRADE 2: choosing the target of certainty rating and assessing
#     imprecision. BMJ. 2025;389:e081904. doi:10.1136/bmj-2024-081904
#     -- Fig 1 (thresholds), Fig 2 (three steps for deciding the target),
#        Fig 3 (point estimate above / below the MID).
#
# Core GRADE 2 Fig 2 の 3 ステップ:
#   1. Choose threshold of interest
#        "Are you interested in whether there is an important effect or not?"
#          -> Choose MID                       (threshold_type = "mid")
#        "Are you interested in whether there is a true underlying effect,
#         benefit or harm?"
#          -> Choose null                      (threshold_type = "null")
#   2. Establish absolute effect (weighted mean difference or risk difference)
#   3. Choose target of certainty based on the point estimate
#
# 分岐表 (Fig 2 下段 + 本文 3 ページ目):
#
#   threshold_type | 点推定値の位置        | target                  | imprecision の閾値
#   ---------------+-----------------------+-------------------------+-------------------
#   mid            | |TE| >  MID           | important_effect        | +/-MID
#   mid            | |TE| <= MID           | little_to_no_difference | +/-MID
#   null           | null 近傍             | little_to_no_difference | +/-MID
#   null           | null 近傍でない       | non_null_effect         | null (= 0)
#
# 「null 近傍 (very near null)」の操作的定義は原論文にない (本文は "the point
# estimate is near the null" / "clearly suggests an unimportant effect" と述べる
# のみ)。pmatools は MID が与えられている場合に限り |TE| <= MID を近傍と定義する。
# MID がない場合は近傍か否かを判定できないため non_null_effect に倒し、その旨を
# note に明記する (本文: "although choosing the null usually avoids specifying
# MIDs, it will not always do so"、supplementary appendix 4 参照)。
#
# 絶対効果ベースの導出:
#   Core GRADE は target を絶対効果 (risk difference / weighted mean difference)
#   で決めると明記している。threshold_scale = "ard" 指定時は
#   threshold_to_te_scale() が baseline risk を使って ARD を比スケールへ換算済み
#   なので、比較はその換算後の閾値の上で成立する。ARD 閾値が与えられていない場合
#   は比スケール上で比較し、その旨を note に残す。

# 妥当な rating target と人間可読ラベル
RATING_TARGETS <- c("important_effect", "little_to_no_difference",
                    "non_null_effect")

RATING_TARGET_LABELS <- c(
  important_effect        = "Important effect",
  little_to_no_difference = "Little or no difference",
  non_null_effect         = "Non-null effect"
)

# プールされた点推定値を TE スケールで取り出す。
# random / common のどちらが有効かは meta_obj$random に従い、欠損時は他方に
# フォールバックする (assess_imprecision() の CI 取り出しと同じ方針)。
.pooled_te <- function(meta_obj) {
  te <- if (isTRUE(meta_obj$random)) meta_obj$TE.random else meta_obj$TE.common
  if (is.null(te) || length(te) == 0L || !all(is.finite(te))) {
    te <- if (isTRUE(meta_obj$random)) meta_obj$TE.common else meta_obj$TE.random
  }
  if (is.null(te) || length(te) == 0L) return(NA_real_)
  as.numeric(te)[1]
}

# MID が使える形で与えられているか
.has_mid <- function(threshold_internal) {
  !is.null(threshold_internal) &&
    length(threshold_internal) > 0 &&
    !is.na(threshold_internal) &&
    is.finite(threshold_internal) &&
    threshold_internal > 0
}

#' Derive the target of the certainty rating (Core GRADE 2 Fig 2)
#'
#' Internal helper implementing the Fig 2 branch table above.
#'
#' @param te_point Pooled point estimate on the TE scale (log scale for ratio
#'   effect measures, raw scale for MD / SMD / ARD).
#' @param threshold_internal MID on the TE scale (positive), or \code{NULL}.
#' @param threshold_type \code{"mid"} or \code{"null"}.
#' @param sm Effect measure (\code{meta_obj$sm}), used only for the note.
#' @param threshold_kind Resolved threshold scale from
#'   \code{\link{threshold_to_te_scale}} (\code{"ard"} when the MID was
#'   supplied as an absolute risk difference).
#'
#' @return A list with \code{target}, \code{target_label},
#'   \code{threshold_for_imprecision} (the threshold the Imprecision domain
#'   must use: \eqn{\pm}MID, or 0 for the null threshold) and \code{note}.
#'
#' @keywords internal
#' @noRd
.derive_rating_target <- function(te_point, threshold_internal, threshold_type,
                                  sm = NULL, threshold_kind = NULL) {
  has_mid <- .has_mid(threshold_internal)
  te_ok   <- !is.null(te_point) && length(te_point) == 1L &&
             !is.na(te_point) && is.finite(te_point)

  # 絶対効果で導出できたか (Core GRADE 2: "consider absolute rather than
  # relative effects")。
  scale_note <- if (!has_mid) {
    ""
  } else if (identical(threshold_kind, "ard")) {
    " Target derived from the absolute-effect threshold (risk difference)."
  } else if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")) {
    paste0(" Target derived on the relative-effect scale (", sm,
           "); Core GRADE 2 recommends an absolute-effect threshold ",
           "(threshold_scale = 'ard') where a baseline risk is available.")
  } else {
    " Target derived on the absolute-effect scale (mean difference)."
  }

  if (!te_ok) {
    return(list(
      target                    = "non_null_effect",
      target_label              = RATING_TARGET_LABELS[["non_null_effect"]],
      threshold_for_imprecision = 0,
      note = paste0(
        "Rating target: Non-null effect (default). The pooled point estimate ",
        "was not available, so the Core GRADE 2 Fig 2 branch could not be ",
        "evaluated."
      )
    ))
  }

  if (identical(threshold_type, "mid")) {
    if (!has_mid) {
      # require_threshold = FALSE で MID なしを許容した場合の逃げ道。
      return(list(
        target                    = "non_null_effect",
        target_label              = RATING_TARGET_LABELS[["non_null_effect"]],
        threshold_for_imprecision = 0,
        note = paste0(
          "Rating target: Non-null effect (fallback). threshold_type = 'mid' ",
          "was requested without a threshold (require_threshold = FALSE), so ",
          "importance cannot be judged; imprecision falls back to the null ",
          "threshold."
        )
      ))
    }
    if (abs(te_point) > threshold_internal) {
      return(list(
        target                    = "important_effect",
        target_label              = RATING_TARGET_LABELS[["important_effect"]],
        threshold_for_imprecision = threshold_internal,
        note = paste0(sprintf(
          paste0("Rating target: Important effect (Core GRADE 2 Fig 2, MID ",
                 "threshold; |point estimate| = %.4f > MID = %.4f on the TE ",
                 "scale)."),
          abs(te_point), threshold_internal), scale_note)
      ))
    }
    return(list(
      target                    = "little_to_no_difference",
      target_label              = RATING_TARGET_LABELS[["little_to_no_difference"]],
      threshold_for_imprecision = threshold_internal,
      note = paste0(sprintf(
        paste0("Rating target: Little or no difference (Core GRADE 2 Fig 2, ",
               "MID threshold; |point estimate| = %.4f <= MID = %.4f on the ",
               "TE scale)."),
        abs(te_point), threshold_internal), scale_note)
    ))
  }

  # threshold_type == "null"
  if (!has_mid) {
    return(list(
      target                    = "non_null_effect",
      target_label              = RATING_TARGET_LABELS[["non_null_effect"]],
      threshold_for_imprecision = 0,
      note = paste0(
        "Rating target: Non-null effect (Core GRADE 2 Fig 2, null threshold). ",
        "No MID was supplied, so whether the point estimate is very near the ",
        "null cannot be judged; certainty is rated in a true underlying ",
        "effect and imprecision uses the null threshold."
      )
    ))
  }
  if (abs(te_point) <= threshold_internal) {
    # 本文: "If, however, the point estimate is near the null ... they will
    # rate their certainty in an unimportant effect".
    return(list(
      target                    = "little_to_no_difference",
      target_label              = RATING_TARGET_LABELS[["little_to_no_difference"]],
      threshold_for_imprecision = threshold_internal,
      note = paste0(sprintf(
        paste0("Rating target: Little or no difference (Core GRADE 2 Fig 2, ",
               "null threshold, point estimate very near the null; |point ",
               "estimate| = %.4f <= MID = %.4f on the TE scale). Imprecision ",
               "is judged against the MID."),
        abs(te_point), threshold_internal), scale_note)
    ))
  }
  list(
    target                    = "non_null_effect",
    target_label              = RATING_TARGET_LABELS[["non_null_effect"]],
    threshold_for_imprecision = 0,
    note = paste0(sprintf(
      paste0("Rating target: Non-null effect (Core GRADE 2 Fig 2, null ",
             "threshold; |point estimate| = %.4f > MID = %.4f on the TE ",
             "scale, so the estimate is not very near the null)."),
      abs(te_point), threshold_internal), scale_note)
  )
}

# --------------------------------------------------------------------------
# grade_meta() の入口ゲート
# --------------------------------------------------------------------------

# threshold_type = "mid" は MID を必須にする (Core GRADE 2: importance can be
# judged only against a MID)。エラーメッセージには suggest_threshold() の実際の
# 返り値を埋め込み、そのままコピーできる形にする。
.check_threshold_type_gate <- function(meta_obj, threshold_type, threshold,
                                       require_threshold) {
  if (!identical(threshold_type, "mid")) return(invisible(NULL))
  if (!is.null(threshold) && !is.na(threshold)) return(invisible(NULL))
  if (!isTRUE(require_threshold)) return(invisible(NULL))

  sugg <- tryCatch(suggest_threshold(meta_obj), error = function(e) NULL)
  sm   <- meta_obj$sm %||% "this"
  hint <- if (!is.null(sugg) && !is.null(sugg$threshold_user)) {
    src <- sugg$source %||% "package_convention"
    provenance <- if (identical(src, "core_grade_6")) {
      paste0(
        "That value is cited in Core GRADE 6, which qualifies it: ",
        "'clinicians may be appropriately sceptical of this threshold, which ",
        "is limited by large variability in the methods investigators use to ",
        "calculate the SMD'."
      )
    } else {
      paste0(
        "That value is a pmatools placeholder (source = 'package_convention'), ",
        "NOT a Core GRADE number: the Core GRADE series contains no ratio-scale ",
        "MID, and every binary MID it discusses is on the absolute scale (per ",
        "1000 or percent)."
      )
    }
    alt <- if (!is.null(sugg$threshold_ratio)) {
      sprintf(
        paste0(" A ratio-scale fallback is available as threshold = %s with ",
               "threshold_scale = 'ratio'."),
        format(signif(sugg$threshold_ratio$threshold_user, 4))
      )
    } else ""
    paste0(
      sprintf(
        paste0("suggest_threshold() recommends threshold = %s with ",
               "threshold_scale = '%s' for this %s meta-analysis."),
        format(signif(sugg$threshold_user, 4)), sugg$threshold_scale, sm
      ),
      alt, " ", provenance,
      " Core GRADE 7 ties the MID to the outcome, not to the effect measure ",
      "('MIDs associated with mortality of 1%, stroke of 2%, myocardial ",
      "infarction of 3%, and serious gastrointestinal bleeding of 5% reflect ",
      "the gradient of importance across these outcomes'), and asks users to ",
      "read the CI first and pin down a MID only where the verdict depends on ",
      "it. Treat the number above as a placeholder to replace, not as a ",
      "recommendation."
    )
  } else {
    paste0("No placeholder default is available for sm = '", sm,
           "'; supply a published or expert-derived MID.")
  }

  # Classed so batch orchestration (grade_meta_multi()) can tell the entry gate
  # apart from an ordinary per-outcome failure: every other error there is
  # demoted to a warning, but this one must keep aborting.
  rlang::abort(paste0(
    "threshold_type = 'mid' requires a threshold (the minimal important ",
    "difference). ", hint,
    " Pass require_threshold = FALSE to proceed without one, or use ",
    "threshold_type = 'null' to rate certainty in a true underlying effect."
  ), class = "pmatools_threshold_gate")
}

# rating_target の手動指定を検証し、target/note を組み立てる。
# 手動指定は自動導出 (Fig 2) の上書きなので rationale を必須にする。
.resolve_rating_target <- function(rating_target, rating_target_rationale,
                                   auto_target, threshold_internal) {
  if (is.null(rating_target)) return(auto_target)

  if (!is.character(rating_target) || length(rating_target) != 1L ||
      is.na(rating_target) || !rating_target %in% RATING_TARGETS) {
    rlang::abort(paste0(
      "rating_target must be one of 'important_effect', ",
      "'little_to_no_difference', 'non_null_effect', or NULL (auto-derived ",
      "from the point estimate per Core GRADE 2 Fig 2)."
    ))
  }
  .check_override_rationale(rating_target_rationale, "rating_target_rationale",
                            "rating target")

  # important_effect / little_to_no_difference は MID を基準に imprecision を
  # 判定するため MID が必須 (Core GRADE 2 supplementary appendix 4)。
  if (rating_target %in% c("important_effect", "little_to_no_difference") &&
      !.has_mid(threshold_internal)) {
    # Same gate, reached from the manual-override side; classed for the same
    # reason (see .check_threshold_type_gate()).
    rlang::abort(sprintf(paste0(
      "rating_target = '%s' requires a threshold (MID): imprecision for this ",
      "target is judged against +/-MID, not against the null. Supply ",
      "threshold (and threshold_scale)."), rating_target),
      class = "pmatools_threshold_gate")
  }

  list(
    target                    = rating_target,
    target_label              = RATING_TARGET_LABELS[[rating_target]],
    threshold_for_imprecision = if (rating_target == "non_null_effect") 0
                                else threshold_internal,
    note = sprintf(
      paste0("Rating target: %s | Manual override (%s): %s | Auto-derived ",
             "target would have been: %s."),
      RATING_TARGET_LABELS[[rating_target]], rating_target,
      trimws(rating_target_rationale), auto_target$target
    )
  )
}
