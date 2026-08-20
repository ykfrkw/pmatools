# rating_target.R - which claim the certainty rating is about
#
# BMJ 2025 Core GRADE 2: choosing the target of the certainty rating
#
# Core GRADE rates certainty in a claim, not in a number, and the claim has to be
# settled before any domain is judged. That choice lives here. This file owns the
# three rating targets, the Fig 2 branch table that derives one of them from the
# pooled point estimate, the entry gate that refuses threshold_type = "mid" with
# no MID in hand, and the manual-override path. What it hands back each time is
# the target, its display label, the threshold the Imprecision domain must then
# evaluate the CI against, and a note naming the branch that produced it.
#
# grade_meta() (R/grade_meta.R) is the caller that matters: it resolves the
# target here and passes the chosen threshold down to assess_imprecision()
# (R/domain_imprecision.R) as threshold_for_imprecision. Imprecision must never
# re-derive it -- a second derivation is a second chance to disagree with the
# first, and the two would then disagree silently.
#
# A helper belongs here when it decides WHAT the rating is a rating of. When it
# decides how well the evidence supports that claim, it belongs in a domain file.
#
# References:
#   Guyatt G, Zeng L, Brignardello-Petersen R, et al.
#     Core GRADE 2: choosing the target of certainty rating and assessing
#     imprecision. BMJ. 2025;389:e081904. doi:10.1136/bmj-2024-081904
#     -- Fig 1 (thresholds), Fig 2 (three steps for deciding the target),
#        Fig 3 (point estimate above / below the MID).
#
# Core GRADE 2 Fig 2, in three steps:
#   1. Choose threshold of interest
#        "Are you interested in whether there is an important effect or not?"
#          -> Choose MID                       (threshold_type = "mid")
#        "Are you interested in whether there is a true underlying effect,
#         benefit or harm?"
#          -> Choose null                      (threshold_type = "null")
#   2. Establish absolute effect (weighted mean difference or risk difference)
#   3. Choose target of certainty based on the point estimate
#
# The branch table (Fig 2, lower half, plus the body text on p3):
#
#   threshold_type | point estimate      | target                  | imprecision threshold
#   ---------------+---------------------+-------------------------+----------------------
#   mid            | |TE| >  MID         | important_effect        | +/-MID
#   mid            | |TE| <= MID         | little_to_no_difference | +/-MID
#   null           | near the null       | little_to_no_difference | +/-MID
#   null           | not near the null   | non_null_effect         | null (= 0)
#
# "Very near the null" has no operational definition in the source: the body text
# says only that "the point estimate is near the null" and that it "clearly
# suggests an unimportant effect". pmatools therefore reads near-the-null as
# |TE| <= MID, and only where a MID exists. Without one the question cannot be
# asked at all, so the derivation falls to non_null_effect and the note says why.
# That gap is the paper's own: "although choosing the null usually avoids
# specifying MIDs, it will not always do so" (supplementary appendix 4).
#
# Derived on the absolute effect, wherever there is one:
#   Core GRADE says plainly that the target is decided on the absolute effect
#   (risk difference, weighted mean difference). With threshold_scale = "ard"
#   that is what happens, indirectly: threshold_to_te_scale() has already
#   converted the ARD to the ratio scale using the baseline risk, so comparing
#   against the converted threshold IS the absolute-effect comparison, done in
#   the units the pooled estimate arrives in. With no ARD threshold there is
#   nothing to convert, the comparison runs on the relative scale, and the note
#   records which of the two it was rather than letting them look alike.

# The three valid rating targets, and the wording each is displayed in.
RATING_TARGETS <- c("important_effect", "little_to_no_difference",
                    "non_null_effect")

RATING_TARGET_LABELS <- c(
  important_effect        = "Important effect",
  little_to_no_difference = "Little or no difference",
  non_null_effect         = "Non-null effect"
)

# The pooled point estimate, on the TE scale. meta_obj$random says which of the
# two fits is in force; a missing or non-finite value falls back to the other,
# which is the same rule assess_imprecision() follows when it picks up the CI.
# Keeping the two rules identical is what stops the target and the interval it is
# compared against from coming out of different fits.
.pooled_te <- function(meta_obj) {
  te <- if (isTRUE(meta_obj$random)) meta_obj$TE.random else meta_obj$TE.common
  if (is.null(te) || length(te) == 0L || !all(is.finite(te))) {
    te <- if (isTRUE(meta_obj$random)) meta_obj$TE.common else meta_obj$TE.random
  }
  if (is.null(te) || length(te) == 0L) return(NA_real_)
  as.numeric(te)[1]
}

# Is there a MID the branch table can actually use: present, finite and strictly
# positive? Zero is excluded on purpose - it is the null threshold wearing a MID's
# name, and the branches below must be able to tell those two cases apart.
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

  # Which scale the derivation actually ran on (Core GRADE 2: "consider absolute
  # rather than relative effects"). Appended to every note that had a MID to
  # compare against, so a reader can see whether the paper's preference was met
  # on this analysis or only approximated on the relative scale.
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
      # The only way to arrive here is require_threshold = FALSE: the entry gate
      # in .check_threshold_type_gate() rejects every other route to
      # threshold_type = "mid" with no MID. Importance cannot be judged without
      # one, so the target falls back to the null-threshold answer.
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
          paste0("Rating target: Important effect (Core GRADE 2 Fig 2, ",
                 "clinical decision threshold; |point estimate| = %.4f > ",
                 "threshold = %.4f on the TE scale)."),
          abs(te_point), threshold_internal), scale_note)
      ))
    }
    return(list(
      target                    = "little_to_no_difference",
      target_label              = RATING_TARGET_LABELS[["little_to_no_difference"]],
      threshold_for_imprecision = threshold_internal,
      note = paste0(sprintf(
        paste0("Rating target: Little or no difference (Core GRADE 2 Fig 2, ",
               "clinical decision threshold; |point estimate| = %.4f <= ",
               "threshold = %.4f on the TE scale)."),
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
        "No threshold was supplied, so whether the point estimate is very ",
        "near the null cannot be judged; certainty is rated in a true ",
        "underlying effect and imprecision uses the null threshold."
      )
    ))
  }
  if (abs(te_point) <= threshold_internal) {
    # Core GRADE 2, body text: "If, however, the point estimate is near the null
    # ... they will rate their certainty in an unimportant effect".
    return(list(
      target                    = "little_to_no_difference",
      target_label              = RATING_TARGET_LABELS[["little_to_no_difference"]],
      threshold_for_imprecision = threshold_internal,
      note = paste0(sprintf(
        paste0("Rating target: Little or no difference (Core GRADE 2 Fig 2, ",
               "null threshold, point estimate very near the null; |point ",
               "estimate| = %.4f <= threshold = %.4f on the TE scale). ",
               "Imprecision is judged against the threshold."),
        abs(te_point), threshold_internal), scale_note)
    ))
  }
  list(
    target                    = "non_null_effect",
    target_label              = RATING_TARGET_LABELS[["non_null_effect"]],
    threshold_for_imprecision = 0,
    note = paste0(sprintf(
      paste0("Rating target: Non-null effect (Core GRADE 2 Fig 2, null ",
             "threshold; |point estimate| = %.4f > threshold = %.4f on the ",
             "TE scale, so the estimate is not very near the null)."),
      abs(te_point), threshold_internal), scale_note)
  )
}

# --------------------------------------------------------------------------
# grade_meta()'s entry gate
# --------------------------------------------------------------------------

# threshold_type = "mid" makes a MID mandatory: importance can only be judged
# against one (Core GRADE 2). The abort embeds suggest_threshold()'s ACTUAL
# return value for this analysis rather than describing where to find it, so the
# caller can copy the argument straight out of the error message. It then says at
# length where that number came from, because most of them are pmatools
# placeholders rather than Core GRADE numbers, and an error message that hands
# over a figure without that warning invites it to be pasted in unread.
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
        "threshold, and every binary threshold it discusses is on the absolute ",
        "scale (per 1000 or percent)."
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
      # The bracketed sentence is Core GRADE 7 verbatim and keeps the source's
      # own word ("MIDs"); pmatools' own prose around it says "threshold".
      " Core GRADE 7 ties the threshold to the outcome, not to the effect ",
      "measure ('MIDs associated with mortality of 1%, stroke of 2%, ",
      "myocardial infarction of 3%, and serious gastrointestinal bleeding of ",
      "5% reflect the gradient of importance across these outcomes'), and ",
      "asks users to read the CI first and pin down a threshold only where ",
      "the verdict depends on it. Treat the number above as a placeholder to ",
      "replace, not as a recommendation."
    )
  } else {
    paste0("No placeholder default is available for sm = '", sm,
           "'; supply a published or expert-derived threshold.")
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

# Validate a manually supplied rating_target and build the target / note pair
# from it. A manual target overrides the Fig 2 derivation, so a rationale is
# mandatory, and the note records both the reason given and the target the
# derivation would have reached. That pairing is what makes the override
# auditable: a reader can see what was overridden as well as why.
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

  # important_effect and little_to_no_difference both have Imprecision judge the
  # CI against +/-MID, so neither can be chosen without one (Core GRADE 2
  # supplementary appendix 4).
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

# The threshold the Imprecision domain was ACTUALLY rated against, recovered
# from a rated object.
#
# Every branch above sets threshold_for_imprecision to 0 for the
# non-null-effect target and to threshold_internal for the other two, so the
# recovery is that one rule and nothing else. It lives here, beside the rule,
# because a caller that re-derived it from `threshold_internal` alone would ask
# a different question of the same evidence - which is exactly what the
# rare-event method sensitivity must not do (shiny/SPEC.md 3.4.14: every method
# is asked "the same threshold-crossing question the primary was asked").
#
# Returns 0 when the object carries no usable threshold, which is what a
# null-threshold rating means.
.rated_threshold_for_imprecision <- function(g) {
  if (identical(g$rating_target, "non_null_effect")) return(0)
  thr <- g$threshold_internal
  if (is.null(thr) || length(thr) != 1L || !is.finite(thr) || thr <= 0) {
    return(0)
  }
  as.numeric(thr)
}
