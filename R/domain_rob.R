# domain_rob.R - Risk of Bias domain assessment
#
# v0.6: the domain follows the Core GRADE Guide Fig 7-2 flowchart (Guyatt G,
# et al. Core GRADE Guide, https://core-grade.gitbook.io/org, accessed
# 2026-11-02), which replaces BMJ 2025 Core GRADE 4 Fig 2. The two figures
# share their first node and their undominated branch; what changed is the
# dominated branch, which no longer checks the direction of bias.
#
# This file owns the risk-of-bias judgment from the per-study ratings to the
# domain row: the binary fold, the study-level overrides, the dominance gate,
# both branches of Fig 7-2, the legacy direction-of-bias check, the refit on
# the low-RoB subset, and the RoB label vocabulary (rob_strata()). It reads a
# {meta} object and a rob vector, and hands back one make_domain_row() tibble
# (R/domain_row.R) carrying the analysis-set recommendation as attributes,
# which grade_meta() (R/grade_meta.R) reads before anything else is rated.
# R/pubias_status.R and R/pubias_trimfill.R borrow .assess_bias_direction()
# and the zone helpers below. A function belongs here when it decides, or
# feeds, the risk-of-bias judgment; one that only renders it belongs with the
# renderer.
#
# --------------------------------------------------------------------------
# Step 0. Binary classification of each study
#
#   rob_some_concerns = "low" (default)  : {not_serious, serious} -> low
#                                          {very_serious}         -> high
#   rob_some_concerns = "high"           : {not_serious}          -> low
#                                          {serious, very_serious} -> high
#
#   THE DEFAULT IS "low" as of 0.6.0, by owner decision: a study rated "some
#   concerns" counts on the low risk of bias side unless the reviewer says
#   otherwise. (It was "high" in 0.5.1 and 0.5.2, and "low" before that.) The
#   source does not settle the fold either way -- see PROVENANCE below -- so
#   the default is a pmatools choice, not a Core GRADE rule. "high" remains
#   available for reviewers who read "some concerns" as the stricter of Core
#   GRADE 4's worked boundaries, which calls a trial high risk of bias "if at
#   least one item was rated as high risk of bias".
#
#   PROVENANCE: the binary verdict is Core GRADE's (verbatim, Core GRADE 4:
#   "For simplicity, however, Core GRADE users can assess the overall risk of
#   bias in individual studies as low or high"), but the FOLD is not. The
#   string "some concerns" does not occur in Core GRADE 4; it belongs to
#   three-level tools such as RoB 2. Core GRADE 4 sets the low/high boundary by
#   counting high-risk ITEMS, its three worked examples use three different
#   counts (one item, two of seven, three of eight), and it leaves the choice
#   open: "The choice of threshold--high risk of bias in only one or more than
#   one item or category--may be an issue that will be impossible to resolve".
#   rob_some_concerns is therefore a pmatools convenience for callers who
#   arrive with a three-level rating already made, not a rule from the source.
#
#   Study-level overrides (`rob_overrides`, keyed on studlab) are applied
#   before the fold and each one requires a written rationale.
#
# --------------------------------------------------------------------------
# Step 1. Dominance gate (Guide Fig 7-2, first node; unchanged from Core
#         GRADE 4 Fig 2)
#
#   w_high = IV weight share carried by high-RoB studies.
#   dominated  <=>  w_high >= rob_dominant_threshold
#
#   Guide Fig 7-2 footnote, verbatim:
#     "*Possible thresholds for high risk of bias 'dominating': >65% weight or
#      >=55% weight=possibly dominating."
#   pmatools defaults to the conservative candidate, 0.55, with a `>=`
#   comparison so that exactly 55% counts as dominated. Set
#   rob_dominant_threshold = 0.65 for the stricter reading.
#
#   If the weight share cannot be computed, the count share is used instead
#   (stated in the notes); if neither can be computed, dominance is assumed
#   (conservative). The count-share fallback has NO basis in the source, which
#   speaks only of weight in the meta-analysis. It exists so that hand-built
#   meta objects without usable weights still produce a verdict, and every note
#   produced on that path says so.
#
# --------------------------------------------------------------------------
# Step 2a. dominated = Yes -> "Use results from all studies and rate down"
#
#   The Guide's leaf, verbatim, and the default (rob_direction_check = FALSE):
#   judgment "serious" (-1), analysis_set "all", no comparison of estimates.
#   This includes the n_low == 0 case (every study high risk of bias).
#
#   THE "CHECK DIRECTION OF BIAS" NODE IS GONE from the source. Core GRADE 4
#   Fig 2 had one; Guide Fig 7-2 does not, and the Guide's chapter 7 moves the
#   consideration out of Core GRADE: users "may consider the direction of
#   bias" before rating down, "However, we consider this complex
#   consideration beyond Core GRADE" (Guide Appendix 11, "Beyond Core GRADE.
#   Rating risk of bias across bodies of evidence").
#
#   LEGACY OPT-IN: rob_direction_check = TRUE restores the 0.5.x behaviour on
#   BOTH branches, exactly: the five-rule check below decides the dominated
#   branch (including rule 5's two levels), and the same directional verdict
#   answers the undominated node. It is Beyond Core GRADE, retained so that
#   0.5.x analyses reproduce, and every judgment made with it carries
#   .ROB_LEGACY_NOTE. The Shiny app does not expose it.
#
#   Legacy five rules (.assess_bias_direction(), zones by +/-Threshold):
#     Rule 1: za == zl == "trivial"                    -> "not_serious"
#     Rule 2: za == zl, non-trivial, inflation <= 20%  -> "not_serious"
#     Rule 3: za == zl, non-trivial, bias-favouring
#             inflation > 20%                          -> "serious"  (-1)
#     Rule 4: za != zl, no sign flip across null       -> "serious"  (-1)
#     Rule 5: za != zl, sign flip (above <-> below)    -> "very_serious" (-2),
#             or "serious" (-1) when no threshold was supplied
#   Rule 5's two levels departed from Core GRADE 4, which describes no
#   two-level risk-of-bias downgrade; .ROB_TWO_LEVEL_NOTE says so wherever the
#   branch fires. Guide Fig 7-2 has no box for the check, so in legacy mode
#   flow_path lights the dominance node and its Yes edge only, and the rule
#   number rides on the fig2_branch fact.
#
# --------------------------------------------------------------------------
# Step 2b. dominated = No -> "Appreciable evidence exists from low risk of
#          bias studies" -> "Are there substantial differences in estimate of
#          effect between high and low risk of bias studies?"
#
#   Neither answer rates down. What differs is the recommended analysis set:
#     Yes -> "Use only low risk of bias studies and do not rate down"
#            (analysis_set = "low_only")
#     No  -> "Use results from all studies and do not rate down"
#            (analysis_set = "all")
#
#   The appreciable box is a statement in Guide Fig 7-2, not a question, and
#   the arithmetic agrees: not dominating leaves the low-RoB studies more than
#   45% of the weight at the default gate and more than 35% at 0.65, which is
#   what the footnote's "corresponding thresholds" (>=35% or >=45%) call
#   appreciable. It is drawn with one outgoing edge.
#
#   DEFAULT READING (rob_direction_check = FALSE) IS SYMMETRIC, as the node is
#   worded. .assess_rob_magnitude() answers it:
#     substantially different  <=>  zone(TE_all) != zone(TE_low), or both in
#       the same non-trivial zone with |(|TE_all| - |TE_low|) / |TE_low||
#       > rob_inflation_threshold (either direction).
#   The Guide's criteria are qualitative ("differing clinical decisions on the
#   basis of low or high risk of bias studies or, alternatively, point
#   estimates on different sides of and far from the decision threshold"), so the
#   numbers are pmatools' and .ROB_SYMMETRIC_NODE_NOTE says so. The Guide also
#   asks for "formal tests of subgroup differences between trials", which
#   pmatools does not run; the note points the reader at them. A comparison
#   that cannot be made keeps all studies.
#
#   In legacy mode the node is read with the directional five-rule verdict
#   instead (rules 3-5 = different, 1-2 = similar), as in 0.5.1, with
#   .ROB_DIRECTIONAL_NODE_NOTE stating that departure.
#
#   grade_meta() acts on analysis_set = "low_only" by refitting the meta
#   object on the low-RoB subset (rob_refit = TRUE, the default).
#
# --------------------------------------------------------------------------
# Return value
#
#   A 1-row tibble (make_domain_row()) carrying two attributes:
#     attr(row, "analysis_set") : "all" | "low_only"
#     attr(row, "high_idx")     : logical vector of high-RoB studies, aligned
#                                 to meta_obj$studlab (NOT to k, so that
#                                 update.meta(subset = ) can use it), or NULL
#
# Inputs:
#   (a) scalar GRADE level: bypass flowchart
#   (b) length-k vector: apply flowchart
#   (c) column name in meta_obj$data: expand to vector and apply flowchart
#
# small_values is required only with rob_direction_check = TRUE (the legacy
# check needs the bias-favouring direction; see .check_small_values()):
#   "undesirable": small values are bad; TE_all > TE_low is inflation
#   "desirable"  : small values are good; TE_all < TE_low is inflation
# The default symmetric reading has no direction and does not read it.

# --------------------------------------------------------------------------
# Constants
# --------------------------------------------------------------------------

# Relative change of the pooled estimate that counts as "substantially
# different" on the undominated branch (either direction by default; only the
# bias-favouring direction under the legacy check) and, under the legacy
# check, as inflation worth rating down for (rule 3).
#
# Neither Core GRADE 4 nor the Guide puts a number on these nodes -- the
# dominance node is the only one a footnote quantifies -- so this is a
# pmatools convention. It was 0.10 through v0.5.0; 0.20 since, because a 10%
# relative change in a pooled estimate is routinely within what re-fitting on
# a subset produces on its own (TE_low is a fixed-effect estimate while TE_all
# usually is not -- see the CAVEAT on .assess_bias_direction()).
#
# Every default that names it (assess_rob(), .flowchart_rob(), the two
# comparison functions) and export_bundle()'s fallback read this one value; the
# comparison stays strict (`>`), so a change exactly at the threshold counts as
# similar.
PMA_ROB_INFLATION_THRESHOLD <- 0.20

# --------------------------------------------------------------------------
# Flowchart node vocabulary (inst/figures/rob.svg, Guide Fig 7-2)
#
# Every id below is a <g id="..."> in that file, and the "flow_path" fact
# emitted by .flowchart_rob() names the subset this assessment traversed. The
# Shiny app reads the fact, hands the ids to www/flowchart.js, and the picture
# lights up the route the judgment actually took; ?grade_flowcharts renders
# the same figure unhighlighted. tests/testthat/test-flowchart-nodes.R asserts
# that every id here is drawn and that no emitted path names an id that is not
# here, so a branch added without being drawn fails the build.
#
# The constant keeps its FIG2 name from the Core GRADE 4 era: other files and
# the tests refer to it by that name, and renaming it buys nothing. The ids
# themselves are Guide Fig 7-2's three leaves and three boxes. The 0.5.x
# direction-check boxes (pma-rob-node-direction, -bias-responsible,
# -bias-conservative, -leaf-noratedown and their edges) are gone with the node
# the Guide removed; the legacy check, which still exists, lights only the
# dominance node and its Yes edge, because the figure has no box for it.
#
# The appreciable box has one outgoing edge. In Fig 7-2 it is a statement, not
# a question, and below the dominance gate the low-RoB weight share always
# clears the footnote's 35-45%, so there is nothing for a second edge to mean.
# Its id dropped the "-yes" it carried while the box was drawn as a question.
.ROB_FIG2_NODE_IDS <- c(
  "pma-rob-node-dominance",
  "pma-rob-edge-dominance-yes",
  "pma-rob-leaf-ratedown",
  "pma-rob-edge-dominance-no",
  "pma-rob-node-appreciable",
  "pma-rob-edge-appreciable-magnitude",
  "pma-rob-node-magnitude",
  "pma-rob-edge-magnitude-similar",
  "pma-rob-leaf-all",
  "pma-rob-edge-magnitude-different",
  "pma-rob-leaf-lowonly"
)

# The route every undominated assessment takes before its magnitude answer.
.ROB_FLOW_UNDOMINATED <- c(
  "pma-rob-node-dominance", "pma-rob-edge-dominance-no",
  "pma-rob-node-appreciable", "pma-rob-edge-appreciable-magnitude",
  "pma-rob-node-magnitude"
)

# The label the fig2_branch fact is displayed under. The key keeps its old
# name for the same reason the vocabulary constant does.
.ROB_FIG_LABEL <- "Core GRADE Guide Fig 7-2"

# The one place a traversed path is turned into a fact. Space-separated
# because a fact's `value` is a single pre-formatted string; `numeric` stays
# NA because a route is not a quantity.
.flow_path_fact <- function(ids) {
  .fact("flow_path", "Flowchart path",
        paste(ids, collapse = " "), NA_real_)
}

# The default dominated leaf. States the leaf and why nothing else was
# consulted, because a reader who knew 0.5.x expects a direction check here.
.ROB_DOMINATED_NOTE <- paste0(
  "Per Core GRADE Guide Fig 7-2 (formerly Core GRADE 4 Fig 2): the evidence ",
  "is dominated by high risk of bias studies -> use results from all studies ",
  "and rate down 1 level (serious). The Guide notes that users may consider ",
  "the direction of bias before rating down, but treats that consideration as ",
  "beyond Core GRADE (Guide Appendix 11); pmatools does not apply it by ",
  "default. If two levels are genuinely warranted, supply the scalar override ",
  "rob = 'very_serious' with rob_rationale."
)

# Appended to every judgment made with rob_direction_check = TRUE, on both
# branches, so that no legacy verdict can be read as the Guide's.
.ROB_LEGACY_NOTE <- paste0(
  "Legacy direction-of-bias check applied (rob_direction_check = TRUE): ",
  "this is Beyond Core GRADE (Core GRADE Guide Appendix 11), the pmatools ",
  "0.5.x five-rule behaviour retained for reproducibility of 0.5.x analyses. ",
  "Core GRADE Guide Fig 7-2 has no direction-of-bias node."
)

# Appended wherever the default undominated branch answers the "substantial
# differences" node, on both of its leaves.
.ROB_SYMMETRIC_NODE_NOTE <- paste0(
  "The Guide's criteria for 'substantially different' are qualitative ",
  "(differing clinical decisions on the basis of low or high risk of bias ",
  "studies, or point estimates on different sides of and far from the decision ",
  "threshold); the numeric operationalisation -- a change of zone relative to ",
  "the +/-threshold, or a relative change in magnitude above the inflation ",
  "threshold in either direction -- is pmatools'. The Guide also asks for ",
  "formal tests of subgroup differences between the trials, bearing in mind ",
  "that they are often underpowered; pmatools does not run one here, so ",
  "inspect plot_forest_rob() and its subgroup test before relying on this ",
  "answer."
)

# Appended where a LEGACY automated path stops at one level although a
# reviewer might have gone deeper: the all-studies-high-RoB case, which reaches
# no zone comparison because there is no comparator pool.
.ROB_CAP_NOTE <- paste0(
  "Core GRADE describes no automatic two-level downgrade for risk of bias ",
  "(every leaf of Core GRADE 4 Fig 2 and of Guide Fig 7-2 is 'rate down' / ",
  "'do not rate down'), and this path is capped at one level. The legacy ",
  "check rates down two on one branch only -- the sign-flip rule, where the ",
  "restricted analysis lands beyond the threshold on the opposite side of the ",
  "null -- and this is not that branch, because no comparison of the two ",
  "estimates was possible here. If two levels are genuinely warranted, supply ",
  "the scalar override rob = 'very_serious' with rob_rationale."
)

# Appended to the legacy sign-flip branch, and to nothing else: the ONE place
# in this file that rates down two levels, a declared departure from the
# source.
.ROB_TWO_LEVEL_NOTE <- paste0(
  "Rated down TWO levels (very serious). This departs from Core GRADE 4, ",
  "which describes no two-level risk-of-bias downgrade at all: every leaf of ",
  "its Fig 2 reads 'rate down' or 'do not rate down', and the only two-level ",
  "move in the paper is rating UP observational evidence. pmatools applies ",
  "one here because this rule is not 'the pooled estimate moved when the high ",
  "risk of bias studies were dropped' -- that is rules 3 and 4, which still ",
  "rate down one level. Rule 5 fires only when the pooled estimate sits ",
  "beyond the chosen threshold on one side of the null and the estimate ",
  "restricted to the low/some-concerns risk of bias studies sits beyond it on ",
  "the other, so the direction of the effect is what the high risk of bias ",
  "studies produced, and reporting the body of evidence as moderate certainty ",
  "would overstate it. Supply the scalar override rob = 'some_concerns' ",
  "(= Core GRADE 'serious', rate down 1) with rob_rationale to rate down one ",
  "level instead."
)

# Appended to the legacy sign-flip branch when no threshold was supplied, in
# place of .ROB_TWO_LEVEL_NOTE.
.ROB_SIGN_FLIP_NO_THRESHOLD_NOTE <- paste0(
  "Rated down ONE level, not two: the two-level sign-flip result requires a ",
  "supplied threshold. Without one the trivial zone collapses to {0}, so ",
  "'opposite sides of the null' means only that the two estimates carry ",
  "opposite signs, which two near-null estimates can satisfy by an ",
  "arbitrarily small movement. What makes a sign flip worth two levels is ",
  "that both estimates clear the threshold, on opposite sides. Supply ",
  "`threshold` to have this rule judged on that basis, or the scalar override ",
  "rob = 'very_serious' with rob_rationale to record two levels here."
)

# Appended wherever the LEGACY undominated branch answers the "substantial
# differences" node, on both of its leaves, because legacy mode answers that
# symmetric node with a directional check. Not used by the default reading,
# which is symmetric and needs no such note.
.ROB_DIRECTIONAL_NODE_NOTE <- paste0(
  "Departure from the source: Core GRADE words this node symmetrically -- ",
  "whether low and high risk of bias studies show substantially different ",
  "estimates of effect -- and names no direction, whereas the legacy check ",
  "answers it with the same directional five-rule check that decides its ",
  "dominated branch. A shift past the inflation threshold that does not run ",
  "in the bias-favouring direction implied by small_values is therefore read ",
  "as 'similar' here, and the analysis is not restricted. The default ",
  "(rob_direction_check = FALSE) reads the node symmetrically."
)

#' Assess the Risk of Bias domain (Core GRADE series; internal)
#'
#' Applies the Core GRADE Guide Fig 7-2 flowchart documented at the top of
#' this file whenever `rob` is a per-study vector (or column name); scalar
#' GRADE levels bypass the flowchart.
#'
#' @param rob Scalar GRADE level, per-study vector, or column name in
#'   `meta_obj$data`.
#' @param meta_obj A `meta::metagen`-like object.
#' @param rob_some_concerns `"low"` (default) or `"high"`: which side of the
#'   binary low/high classification `"serious"` (Cochrane "some concerns")
#'   studies are folded into. The default is `"low"` as of 0.6.0; it was
#'   `"high"` in 0.5.1 and 0.5.2, and `"low"` before that.
#' @param rob_overrides Named character vector of study-level Risk-of-Bias
#'   overrides, keyed on `meta_obj$studlab`. Every key needs a matching
#'   rationale in `rob_override_rationale`.
#' @param rob_override_rationale Named character vector of rationales, same
#'   keys as `rob_overrides`.
#' @param rob_dominant_threshold Weight share (0, 1] at or above which the
#'   evidence counts as dominated by high-RoB studies (Guide Fig 7-2, first
#'   node). Default `0.55`, compared with `>=`.
#' @param rob_inflation_threshold Relative change of the pooled estimate when
#'   high-RoB studies are excluded,
#'   \eqn{(|TE_{all}| - |TE_{low}|) / |TE_{low}|}, above which the undominated
#'   branch reads "substantially different". By default the change counts in
#'   either direction; under `rob_direction_check = TRUE` only a
#'   bias-favouring change counts, and the same comparison is rule 3 of the
#'   legacy check. Default `PMA_ROB_INFLATION_THRESHOLD` (`0.20`), strict `>`.
#' @param rob_direction_check `FALSE` (default) or `TRUE`. `TRUE` restores the
#'   0.5.x five-rule direction-of-bias check on both branches -- Beyond Core
#'   GRADE (Guide Appendix 11), legacy, retained for reproducibility of 0.5.x
#'   analyses. Every judgment made with it says so in its notes.
#' @param small_values `"desirable"` or `"undesirable"`: the bias-favouring
#'   direction. Required when `rob_direction_check = TRUE`; otherwise
#'   validated if supplied and not used.
#' @param threshold_internal Clinical decision threshold on the analysis
#'   scale (defines the trivial zone). Under the legacy check it also decides
#'   how deep rule 5 goes.
#' @return A 1-row tibble with attributes `"analysis_set"` (`"all"` or
#'   `"low_only"`) and `"high_idx"` (a logical vector aligned to
#'   `meta_obj$studlab`, so it can be passed straight to
#'   `update.meta(subset = )`, or `NULL`).
#' @noRd
assess_rob <- function(rob, meta_obj,
                       rob_some_concerns       = "low",
                       rob_overrides           = NULL,
                       rob_override_rationale  = NULL,
                       rob_dominant_threshold  = 0.55,
                       rob_inflation_threshold = PMA_ROB_INFLATION_THRESHOLD,
                       small_values            = NULL,
                       threshold_internal      = NULL,
                       rationale               = NULL,
                       rob_direction_check     = FALSE) {
  k <- meta_obj$k

  rob_direction_check <- .check_rob_direction_check(rob_direction_check)
  # The legacy check cannot run without a direction, so it keeps the 0.5.1
  # entry gate. The default reading has no direction and only rejects a
  # malformed value, never a missing one.
  if (rob_direction_check || !is.null(small_values)) {
    .check_small_values(small_values)
  }
  rob_some_concerns      <- .check_rob_some_concerns(rob_some_concerns)
  rob_dominant_threshold <- .check_dominant_threshold(rob_dominant_threshold)

  # NULL -> default "no"
  if (is.null(rob)) {
    return(.rob_row(make_domain_row(
      domain   = "Risk of bias",
      judgment = "not_serious",
      auto     = FALSE,
      notes    = "Not assessed (rob = NULL). Assumed no concern."
    )))
  }

  # Defensive: coerce data.frame / tibble (1-col) or list inputs to plain vector
  if (is.data.frame(rob)) {
    if (ncol(rob) == 1) rob <- rob[[1]]
    else rlang::abort("rob must be a single column, not a multi-column data.frame.")
  }
  if (is.list(rob) && !is.data.frame(rob)) {
    rob <- unlist(rob, use.names = FALSE)
  }
  if (is.factor(rob)) rob <- as.character(rob)

  # Before ANY normalisation: a bare "serious" named the high stratum up to
  # 0.5.0 and the middle one from 0.5.1, in both the scalar and the per-study
  # reading, and .normalize_rob_level() would resolve it silently either way.
  .check_grade_level_input(
    rob, "rob",
    extra = paste0(
      "A risk-of-bias label of \"serious\" no longer means what it meant in ",
      "pmatools 0.5.0, whether it is a scalar override or one study's rating."
    )
  )

  # Scalar GRADE level (after normalisation): bypass flowchart.
  # v0.4.0 (breaking): a scalar override replaces the automated flowchart, so
  # a written justification (rob_rationale) is mandatory.
  if (length(rob) == 1 && is.character(rob)) {
    rob_norm <- .normalize_rob_level(rob)
    if (rob_norm %in% GRADE_LEVELS) {
      .check_override_rationale(rationale, "rob_rationale", "Risk of Bias")
      return(.rob_row(make_domain_row(
        domain    = "Risk of bias",
        judgment  = rob_norm,
        auto      = FALSE,
        notes     = "Overall judgment provided by user (scalar; flowchart not applied).",
        rationale = rationale
      )))
    }
    # Treat as column name
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

  # Vector: normalise + length check. Both spaces are accepted as input (see
  # the alignment section below); rob_full is NULL when they cannot be mapped
  # onto each other, and every collaborator then falls back to its pre-existing
  # abort/skip path rather than guessing.
  rob      <- .normalize_rob_levels(rob)
  align    <- .rob_alignment(meta_obj, k)
  n_slab   <- align$n_slab
  rob_full <- NULL
  if (length(rob) == k) {
    rob_k    <- rob
    rob_full <- .rob_expand(rob, align)
  } else if (n_slab > 0L && length(rob) == n_slab) {
    rob_full <- rob
    rob_k    <- .rob_contract(rob, align)
    if (is.null(rob_k)) rlang::abort(paste0(
      "rob has one entry per study label (", n_slab, ") but the meta object ",
      "pools only k = ", k, " studies, and the estimable rows could not be ",
      "identified from meta_obj$TE. Supply a vector of length k (", k,
      ") instead."
    ))
  } else {
    rlang::abort(paste0(
      "rob must be a scalar GRADE level, a column name in meta_obj$data, ",
      "or a vector of length k (", k, ") or length(meta_obj$studlab) (",
      n_slab, "). Got length ", length(rob), "."
    ))
  }

  # Already through .normalize_rob_levels(), so the ambiguity guard has had its
  # turn on the raw input above and must not fire on a normalised "serious".
  validate_grade_level(rob, "rob", check_ambiguous = FALSE)

  # Study-level overrides (keyed on studlab) are applied on the normalised
  # vector, before the binary low/high fold, and every one is recorded — in
  # studlab space when that is resolvable, so that an override can also name a
  # study {meta} could not pool. They come back in the space they went in.
  ovr <- .apply_rob_overrides(rob_full %||% rob_k, align, rob_overrides,
                              rob_override_rationale)
  if (is.null(rob_full)) rob_k <- ovr$rob else {
    rob_full <- ovr$rob
    rob_k    <- .rob_contract(rob_full, align)
  }

  row <- .flowchart_rob(rob_k, meta_obj,
                        dominant_threshold  = rob_dominant_threshold,
                        inflation_threshold = rob_inflation_threshold,
                        small_values        = small_values,
                        threshold_internal  = threshold_internal,
                        some_concerns_as    = rob_some_concerns,
                        override_notes      = ovr$notes,
                        direction_check     = rob_direction_check)

  # .flowchart_rob() works in k-space, but the only consumer of "high_idx"
  # (grade_meta() -> .refit_low_rob() -> update.meta(subset = )) indexes the
  # original data rows. The fill keeps the rule that a study {meta} could not
  # pool counts as high only when the caller judged it so (NA stays FALSE).
  high_k <- attr(row, "high_idx")
  if (!is.null(rob_full) && n_slab != k && !is.null(high_k) &&
      length(high_k) == length(rob_k)) {
    attr(row, "high_idx") <- .rob_expand(
      high_k, align, fill = rob_full %in% .rob_high_levels(rob_some_concerns))
  }
  row
}

# A logical flag, never NA. Shared by assess_rob() and grade_meta() so that a
# bad value aborts before any domain is rated, with the same message.
.check_rob_direction_check <- function(x) {
  if (is.null(x)) return(FALSE)
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    rlang::abort(paste0(
      "rob_direction_check must be TRUE or FALSE. TRUE restores the pmatools ",
      "0.5.x direction-of-bias check (Beyond Core GRADE, Guide Appendix 11); ",
      "FALSE (default) follows Core GRADE Guide Fig 7-2."
    ))
  }
  x
}

# --------------------------------------------------------------------------
# k-space <-> studlab-space alignment
#
# {meta} keeps $studlab / $TE / $seTE / $w.random at the length of the
# ORIGINAL data rows but counts only the estimable ones in $k (a study with
# missing results, or a double-zero study under method = "Inverse", is
# dropped from the pool but not from the data). The Guide Fig 7-2 maths
# in .flowchart_rob() is written in k-space; `rob_overrides` keys and
# update.meta(subset = ) live in studlab space. This is the single place that
# maps the n estimable studies onto their studlab positions.
#
# It never guesses: the two candidate rules are the ones the alignment blocks
# in pick_weights() and .flowchart_rob() already use (!is.na(TE), then
# is.finite(TE)), and unless one of them reproduces exactly n rows the answer
# is NULL and the caller keeps its existing abort/skip behaviour.
# .rob_alignment() resolves it once per assess_rob() call; .rob_expand() and
# .rob_contract() move vectors between the spaces, so nothing re-derives it.
# --------------------------------------------------------------------------
.rob_studlab_index <- function(meta_obj, n) {
  studlab <- meta_obj$studlab
  if (is.null(studlab)) return(NULL)
  if (length(n) != 1L || !is.numeric(n) || !is.finite(n)) return(NULL)
  n      <- as.integer(n)
  n_slab <- length(studlab)
  if (n_slab == n) return(seq_len(n_slab))

  te <- meta_obj$TE
  if (is.null(te) || length(te) != n_slab) return(NULL)
  hit <- which(!is.na(te))
  if (length(hit) == n) return(hit)
  hit <- which(is.finite(te))
  if (length(hit) == n) return(hit)
  NULL
}

# The resolved mapping (NULL when unresolvable) plus the lengths and labels.
.rob_alignment <- function(meta_obj, k) list(
  idx = .rob_studlab_index(meta_obj, k), k = k, studlab = meta_obj$studlab,
  n_slab = length(meta_obj$studlab %||% character(0)))

# k-space -> studlab space. `fill` (a scalar, or a ready-made studlab-space
# vector) supplies the rows {meta} could not pool and a k-length input has no
# judgment for. NULL when unresolvable, in both directions.
.rob_expand <- function(x, align, fill = NA_character_) {
  if (is.null(align$idx) || length(x) != length(align$idx)) return(NULL)
  out <- if (length(fill) == align$n_slab) fill else rep(fill, align$n_slab)
  out[align$idx] <- x
  out
}

# studlab space -> k-space; the inverse of .rob_expand().
.rob_contract <- function(x, align) {
  if (is.null(align$idx) || length(x) != align$n_slab) return(NULL)
  x[align$idx]
}

# Binary low/high fold of the normalised levels. Shared by .flowchart_rob()
# (which classifies the pooled studies) and assess_rob() (which classifies the
# studies {meta} dropped, so that "high_idx" is complete in studlab space).
.rob_high_levels <- function(some_concerns_as = "low") {
  high_levels <- c("very_serious", "extremely_serious")
  if (identical(some_concerns_as, "high")) {
    high_levels <- c(high_levels, "serious")
  }
  high_levels
}

# --------------------------------------------------------------------------
# Argument validation + attribute helpers
# --------------------------------------------------------------------------

# Attach the analysis-set recommendation to the 1-row domain tibble. Keeping
# the schema at "one row per domain" means the extra information has to travel
# as attributes; grade_meta() reads them immediately after assess_rob(),
# before dplyr::bind_rows() drops them.
.rob_row <- function(row, analysis_set = "all", high_idx = NULL) {
  attr(row, "analysis_set") <- analysis_set
  attr(row, "high_idx")     <- high_idx
  row
}

.check_rob_some_concerns <- function(x) {
  # NULL is "not supplied", and the default it resolves to is "low" as of
  # 0.6.0 ("high" in 0.5.1-0.5.2; see the Step 0 block at the top of this
  # file). This is the single place that decides it: assess_rob() and
  # grade_meta() both route through here, so the two cannot drift.
  if (is.null(x)) return("low")
  x <- as.character(x)[1]
  if (!x %in% c("low", "high")) {
    rlang::abort(paste0(
      "rob_some_concerns must be 'low' or 'high': it decides whether studies ",
      "rated 'some concerns' are folded into the low or the high risk-of-bias ",
      "group for the Core GRADE Guide Fig 7-2 dominance gate."
    ))
  }
  x
}

.check_dominant_threshold <- function(x) {
  if (is.null(x)) return(0.55)
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x > 1) {
    rlang::abort(paste0(
      "rob_dominant_threshold must be a single weight share in (0, 1] ",
      "(e.g., 0.55, the Core GRADE Guide Fig 7-2 'possibly dominating' threshold, ",
      "or 0.65 for the stricter 'dominating' reading)."
    ))
  }
  as.numeric(x)
}

# --------------------------------------------------------------------------
# Study-level Risk-of-Bias overrides
#
# `rob_overrides` is keyed on studlab so that a reviewer can correct a single
# study without rebuilding the whole vector. Each override is a manual
# replacement of an assessed judgment, so — like every other override in the
# package — it requires a written rationale (.check_override_rationale()).
# Keys that match no studlab abort rather than being silently ignored: a typo
# would otherwise change nothing and leave no trace.
# --------------------------------------------------------------------------
.apply_rob_overrides <- function(rob, align, rob_overrides,
                                 rob_override_rationale) {
  if (is.null(rob_overrides) || length(rob_overrides) == 0L) {
    return(list(rob = rob, notes = character(0)))
  }

  if (is.list(rob_overrides)) rob_overrides <- unlist(rob_overrides)
  if (!is.character(rob_overrides) || is.null(names(rob_overrides)) ||
      any(!nzchar(names(rob_overrides)))) {
    rlang::abort(paste0(
      "rob_overrides must be a named character vector keyed on study labels, ",
      "e.g. c(\"Smith 2020\" = \"high\")."
    ))
  }
  if (!is.null(rob_override_rationale) && is.list(rob_override_rationale)) {
    rob_override_rationale <- unlist(rob_override_rationale)
  }

  # The keys are studlab labels, so the vector has to be matched against the
  # study labels it actually describes: studlab itself in studlab space, the
  # estimable positions in k-space (shorter whenever {meta} dropped a study).
  # Only a genuinely unresolvable alignment aborts.
  studlab_of_rob <- if (length(rob) == align$n_slab) align$studlab else
    .rob_contract(align$studlab, align)
  if (is.null(studlab_of_rob) || length(studlab_of_rob) != length(rob)) {
    rlang::abort(paste0(
      "rob_overrides requires meta_obj$studlab to be available and the same ",
      "length as the per-study risk-of-bias vector (got ",
      align$n_slab, " study labels for ", length(rob),
      " risk-of-bias judgments)."
    ))
  }
  studlab <- as.character(studlab_of_rob)

  keys    <- names(rob_overrides)
  unknown <- setdiff(keys, studlab)
  if (length(unknown) > 0) {
    rlang::abort(paste0(
      "rob_overrides names must match meta_obj$studlab. Unmatched key(s): ",
      paste(shQuote(unknown), collapse = ", "),
      ". Available study labels: ", paste(shQuote(studlab), collapse = ", "), "."
    ))
  }

  notes <- character(0)
  for (key in keys) {
    rat <- if (!is.null(rob_override_rationale) &&
               key %in% names(rob_override_rationale)) {
      unname(rob_override_rationale[[key]])
    } else {
      NULL
    }
    .check_override_rationale(
      rat,
      sprintf("rob_override_rationale[[%s]]", shQuote(key)),
      sprintf("study-level Risk of Bias for %s", shQuote(key))
    )

    .check_grade_level_input(unname(rob_overrides[[key]]),
                             sprintf("rob_overrides[[%s]]", shQuote(key)))
    to <- .normalize_rob_level(unname(rob_overrides[[key]]))
    if (!to %in% GRADE_LEVELS) {
      rlang::abort(paste0(
        "rob_overrides[[", shQuote(key), "]] = ",
        shQuote(unname(rob_overrides[[key]])),
        " is not a recognized risk-of-bias level. Accepted values: ",
        "'not_serious', 'some_concerns', 'very_serious', the Cochrane RoB 2 ",
        "judgments ('Low risk of bias', 'Some concerns', ",
        "'High risk of bias') or the ROBINS-I ones ('Low risk of bias', ",
        "'Moderate risk of bias', 'Serious risk of bias', ",
        "'Critical risk of bias')."
      ))
    }

    hit <- which(studlab == key)
    for (i in hit) {
      # NA is only possible for a study {meta} could not pool, reached with a
      # k-length input: there is no assessed level to report as the "from".
      from <- if (is.na(rob[i])) "not estimable" else rob[i]
      notes <- c(notes, sprintf("Study-level override: %s %s -> %s (%s)",
                                key, from, to, trimws(rat)))
      rob[i] <- to
    }
  }

  list(rob = rob, notes = notes)
}

# Print a display-scale estimate without adding or removing precision:
# .assess_bias_direction()'s .disp() has already rounded it (3 decimals, after
# back-transforming a ratio measure), so a second sprintf() format here would
# either pad zeros onto "0.74" or truncate a genuinely 3-decimal value.
.rob_fact_num <- function(x) {
  if (is.null(x) || length(x) != 1L || !is.finite(x)) return("NA")
  format(x, scientific = FALSE, trim = TRUE)
}

# --------------------------------------------------------------------------
# Flowchart (v0.6: Core GRADE Guide Fig 7-2)
# --------------------------------------------------------------------------
.flowchart_rob <- function(rob_vec, meta_obj,
                           dominant_threshold = 0.55,
                           inflation_threshold = PMA_ROB_INFLATION_THRESHOLD,
                           small_values = NULL,
                           threshold_internal = NULL,
                           some_concerns_as = "low",
                           override_notes = NULL,
                           direction_check = FALSE) {

  # Binary low/high classification, on levels .normalize_rob_levels() has
  # already canonicalised. "very_serious" and "extremely_serious" are always
  # high; "serious" (Cochrane's "some concerns") goes to whichever side
  # rob_some_concerns selects.
  high_levels <- .rob_high_levels(some_concerns_as)
  high_idx <- rob_vec %in% high_levels
  n_high   <- sum(high_idx)
  n_total  <- length(rob_vec)

  # IV weight share carried by high-RoB studies. {meta} can drop studies (e.g.,
  # double-zero events with method = "Inverse") so $TE/$seTE/$w.random retain
  # the original length while $k counts only valid rows; align by `is.finite`
  # before indexing with `high_idx` (which has length n_total = k).
  pick_weights <- function(meta_obj, n_total) {
    align <- function(v) {
      if (is.null(v)) return(NULL)
      if (length(v) == n_total) return(v)
      keep <- is.finite(v) & v > 0
      if (sum(keep) == n_total) return(v[keep])
      keep_te <- is.finite(meta_obj$TE)
      if (length(keep_te) == length(v) && sum(keep_te) == n_total) return(v[keep_te])
      NULL
    }
    for (slot in c("w.random", "w.common", "w.fixed")) {
      v <- align(meta_obj[[slot]])
      if (!is.null(v)) {
        ok <- is.finite(v) & v > 0
        if (any(ok) && sum(v[ok]) > 0) return(v)
      }
    }
    se <- align(meta_obj$seTE)
    if (!is.null(se)) {
      v <- 1 / se^2
      if (any(is.finite(v) & v > 0)) return(v)
    }
    NULL
  }
  w_vec <- pick_weights(meta_obj, n_total)
  # Keep the share itself (not the percentage) as the primitive: the dominance
  # gate compares it against rob_dominant_threshold with `>=`, and a round trip
  # through *100 / 100 can move a boundary case off the threshold.
  w_high_share <- if (!is.null(w_vec) && length(w_vec) == n_total) {
    ok <- is.finite(w_vec)
    w_total <- sum(w_vec[ok], na.rm = TRUE)
    if (w_total > 0) {
      sum(w_vec[high_idx & ok], na.rm = TRUE) / w_total
    } else NA_real_
  } else NA_real_
  w_high_pct <- 100 * w_high_share

  count_pct <- 100 * (n_high / max(1L, n_total))
  weight_note <- if (is.finite(w_high_pct)) {
    sprintf("High-RoB studies: %d/%d (%.0f%% by count, %.0f%% by weight)",
            n_high, n_total, count_pct, w_high_pct)
  } else {
    sprintf("High-RoB studies: %d/%d (%.0f%% by count)",
            n_high, n_total, count_pct)
  }
  fold_note <- sprintf(
    "'some concerns' folded into the %s risk group (rob_some_concerns = '%s')",
    some_concerns_as, some_concerns_as
  )

  # Structured companion to weight_note. Recorded on every path, including the
  # ones that do not rate down: the renderers decide what to show, the
  # assessors only decide what is true.
  f_high <- .fact(
    "high_rob_studies", "High risk of bias studies",
    if (is.finite(w_high_pct)) {
      sprintf("%d of %d (%.0f%% by count, %.0f%% by weight)",
              n_high, n_total, count_pct, w_high_pct)
    } else {
      sprintf("%d of %d (%.0f%% by count)", n_high, n_total, count_pct)
    },
    n_high
  )

  tbl_note <- paste(
    paste0(names(table(rob_vec)), ": n=", as.integer(table(rob_vec))),
    collapse = "; "
  )
  if (length(override_notes) > 0) {
    tbl_note <- paste0(tbl_note, " | ", paste(override_notes, collapse = " | "))
  }
  legacy_tail <- if (direction_check) paste0(" ", .ROB_LEGACY_NOTE) else ""

  # No high-RoB studies at all: the dominance share is 0, below any threshold
  # in (0, 1], and with nothing to exclude the restricted estimate IS the
  # pooled one. The ordinary undominated route therefore describes this case
  # exactly, and the chart lights it like any other. Identical in both modes:
  # the legacy check had nothing to compare here either.
  if (n_high == 0) {
    return(.rob_row(make_domain_row(
      domain   = "Risk of bias",
      judgment = "not_serious",
      auto     = FALSE,
      notes    = paste0(
        "No high-RoB studies. ", weight_note, "; ", fold_note,
        ". -> Do not rate down. | ", tbl_note
      ),
      facts    = .facts(f_high, .flow_path_fact(c(
        .ROB_FLOW_UNDOMINATED, "pma-rob-edge-magnitude-similar",
        "pma-rob-leaf-all")))
    ), analysis_set = "all", high_idx = high_idx))
  }

  # ---- Guide Fig 7-2, node 1: is the evidence dominated by high-RoB
  # studies? Boundary is inclusive (`>=`).
  if (is.finite(w_high_share)) {
    dom_share <- w_high_share
    dom_basis <- "weight"
  } else if (n_total > 0) {
    dom_share <- n_high / n_total
    dom_basis <- "count"
  } else {
    dom_share <- 1
    dom_basis <- "none"
  }
  dominated <- dom_share >= dominant_threshold

  dom_note <- switch(dom_basis,
    "weight" = sprintf(
      "High-RoB weight share %.0f%% %s dominance threshold %.0f%% -> dominated: %s",
      100 * dom_share, if (dominated) ">=" else "<",
      100 * dominant_threshold, if (dominated) "yes" else "no"
    ),
    "count" = sprintf(
      paste0("Study weights could not be computed; dominance judged on the ",
             "count share instead: %.0f%% %s dominance threshold %.0f%% -> ",
             "dominated: %s. The count-share fallback is a pmatools ",
             "convention with no basis in Core GRADE, which speaks only of ",
             "'weight in the meta-analysis'"),
      100 * dom_share, if (dominated) ">=" else "<",
      100 * dominant_threshold, if (dominated) "yes" else "no"
    ),
    sprintf(paste0("Neither the weight share nor the count share of high-RoB ",
                   "studies could be computed; dominance assumed (conservative) ",
                   "at threshold %.0f%%"), 100 * dominant_threshold)
  )

  # Structured companion to dom_note. The basis switch is mirrored so the fact
  # never claims a weight share that was in fact a count-share fallback.
  f_weight <- .fact(
    "high_rob_weight_share", "Weight carried by high risk of bias studies",
    switch(dom_basis,
      "weight" = sprintf("%.0f%% (dominance threshold %.0f%%; dominated: %s)",
                         100 * dom_share, 100 * dominant_threshold,
                         if (dominated) "yes" else "no"),
      "count"  = sprintf(paste0("not computable; judged on the count share ",
                                "%.0f%% instead (dominance threshold %.0f%%; ",
                                "dominated: %s)"),
                         100 * dom_share, 100 * dominant_threshold,
                         if (dominated) "yes" else "no"),
      sprintf(paste0("not computable, and neither was the count share; ",
                     "dominance assumed (dominance threshold %.0f%%; ",
                     "dominated: yes)"), 100 * dominant_threshold)
    ),
    dom_share
  )

  # ---- Node 2a, default: "Use results from all studies and rate down". No
  # comparison is made, so none is recorded: an estimate shift beside this
  # leaf would read as the reason for it, and the Guide's leaf has no reason
  # other than dominance.
  if (dominated && !direction_check) {
    f_branch <- .fact(
      "fig2_branch", .ROB_FIG_LABEL,
      paste0("dominated by high risk of bias studies -> use results from all ",
             "studies and rate down"),
      NA_real_
    )
    all_high_note <- if (n_high == n_total) {
      "All studies high-RoB; no low/some-RoB comparator pool. "
    } else ""
    return(.rob_row(make_domain_row(
      domain   = "Risk of bias",
      judgment = "serious",
      auto     = FALSE,
      notes    = paste0(
        all_high_note, weight_note, "; ", fold_note, ". ", dom_note, ". ",
        .ROB_DOMINATED_NOTE, " | ", tbl_note
      ),
      facts    = .facts(f_high, f_weight, f_branch, .flow_path_fact(c(
        "pma-rob-node-dominance", "pma-rob-edge-dominance-yes",
        "pma-rob-leaf-ratedown")))
    ), analysis_set = "all", high_idx = high_idx))
  }

  # Both remaining routes compare the pooled estimate with the one restricted
  # to the low/some-RoB studies. Align TE / seTE to length k so logical
  # indexing with high_idx is correct.
  te_vec  <- meta_obj$TE
  se_vec  <- meta_obj$seTE
  if (!is.null(te_vec) && length(te_vec) != n_total) {
    keep <- !is.na(te_vec)
    if (sum(keep) != n_total) keep <- is.finite(te_vec)
    if (sum(keep) == n_total) {
      if (!is.null(se_vec) && length(se_vec) == length(te_vec)) {
        se_vec <- se_vec[keep]
      }
      te_vec <- te_vec[keep]
    }
  }

  if (isTRUE(meta_obj$random)) {
    te_all <- meta_obj$TE.random
    se_all <- meta_obj$seTE.random
  } else {
    te_all <- meta_obj$TE.common
    se_all <- meta_obj$seTE.common
  }
  if (is.null(te_all) || length(te_all) == 0L || !is.finite(te_all)) {
    if (isTRUE(meta_obj$random)) {
      te_all <- meta_obj$TE.common
      se_all <- meta_obj$seTE.common
    } else {
      te_all <- meta_obj$TE.random
      se_all <- meta_obj$seTE.random
    }
  }

  cmp <- if (direction_check) {
    .assess_bias_direction(
      te_all              = te_all,
      se_all              = se_all,
      te_vec              = te_vec,
      se_vec              = se_vec,
      low_idx             = !high_idx,
      small_values        = small_values,
      inflation_threshold = inflation_threshold,
      sm                  = meta_obj$sm,
      threshold_internal  = threshold_internal
    )
  } else {
    .assess_rob_magnitude(
      te_all              = te_all,
      te_vec              = te_vec,
      se_vec              = se_vec,
      low_idx             = !high_idx,
      inflation_threshold = inflation_threshold,
      sm                  = meta_obj$sm,
      threshold_internal  = threshold_internal
    )
  }
  f_shift <- .rob_shift_fact(cmp)

  # ---- Node 2a, legacy: the five-rule check decides the judgment. The figure
  # has no box for it, so the path stops on the dominance node's Yes edge and
  # the rule rides on fig2_branch, as a number.
  if (dominated) {
    f_branch <- .fact(
      "fig2_branch", .ROB_FIG_LABEL,
      paste0(
        if (!is.na(cmp$rule)) {
          sprintf("dominated by high risk of bias studies; direction-of-bias rule %d (%s)",
                  cmp$rule,
                  switch(cmp$judgment,
                         not_serious  = "do not rate down",
                         very_serious = "rate down 2 levels",
                         "rate down 1 level"))
        } else {
          paste0("dominated by high risk of bias studies; direction of bias ",
                 "not assessable (rate down)")
        },
        " [legacy rob_direction_check = TRUE; beyond Core GRADE]"),
      as.numeric(cmp$rule)
    )
    return(.rob_row(make_domain_row(
      domain   = "Risk of bias",
      judgment = cmp$judgment,
      auto     = FALSE,
      notes    = paste0(
        weight_note, "; ", fold_note, ". ", dom_note, ". ",
        cmp$note, legacy_tail, " | ",
        tbl_note
      ),
      facts    = .facts(f_high, f_weight, f_shift, f_branch,
                        .flow_path_fact(c("pma-rob-node-dominance",
                                          "pma-rob-edge-dominance-yes")))
    ), analysis_set = "all", high_idx = high_idx))
  }

  # ---- Node 2b: not dominated. Neither answer rates down; only the
  # recommended analysis set changes. Default mode answers the "substantial
  # differences" node symmetrically; legacy mode with the directional rule
  # verdict (rules 3-5 = different, 1-2 = similar), whose depth does not cross
  # into a branch that has no levels to rate down.
  if (direction_check) {
    assessable  <- !is.na(cmp$rule %||% NA)
    substantial <- assessable && !identical(cmp$judgment, "not_serious")
    basis_txt   <- if (assessable) {
      sprintf("the legacy direction-of-bias check reaches rule %d", cmp$rule)
    } else ""
    node_note   <- .ROB_DIRECTIONAL_NODE_NOTE
  } else {
    assessable  <- isTRUE(cmp$assessable)
    substantial <- assessable && isTRUE(cmp$substantial)
    basis_txt   <- cmp$basis %||% ""
    node_note   <- .ROB_SYMMETRIC_NODE_NOTE
  }

  branch_note <- if (!assessable) {
    paste0(
      "The high-vs-low risk of bias comparison is not assessable, so the ",
      "substantial-differences question cannot be answered. Per Core GRADE ",
      "Guide Fig 7-2 the non-dominated branch does not rate down; the ",
      "analysis retains all studies."
    )
  } else if (substantial) {
    paste0(
      "Substantial differences in the estimate of effect between high and ",
      "low risk of bias studies (", basis_txt, "). Per Core GRADE Guide Fig ",
      "7-2: use only low risk of bias studies and do not rate down ",
      "(analysis_set = 'low_only'). ", node_note
    )
  } else {
    paste0(
      "No substantial differences in the estimate of effect between high and ",
      "low risk of bias studies (", basis_txt,
      if (direction_check && !is.null(cmp$gate_note)) {
        paste0("; the two estimates differ by more than the inflation ",
               "threshold in magnitude but not in the bias-favouring ",
               "direction, so rule 2 applies rather than rule 3")
      } else "",
      "). Per Core GRADE Guide Fig 7-2: use results from all studies and do ",
      "not rate down (analysis_set = 'all'). ", node_note
    )
  }

  f_branch <- .fact(
    "fig2_branch", .ROB_FIG_LABEL,
    paste0(
      if (!assessable) {
        paste0("not dominated; high-vs-low risk of bias comparison not ",
               "assessable -> use all studies")
      } else if (substantial) {
        paste0("not dominated; substantially different estimates of effect ",
               "-> use low risk of bias studies only")
      } else {
        "not dominated; no substantial difference -> use all studies"
      },
      if (direction_check) {
        " [legacy rob_direction_check = TRUE; beyond Core GRADE]"
      } else ""),
    if (direction_check) as.numeric(cmp$rule) else NA_real_
  )

  # "Not assessable" shares the No edge: both keep every study and rate down
  # neither way. Which of the two happened is recorded in f_branch and notes.
  flow_ids <- c(.ROB_FLOW_UNDOMINATED,
                if (substantial) {
                  c("pma-rob-edge-magnitude-different", "pma-rob-leaf-lowonly")
                } else {
                  c("pma-rob-edge-magnitude-similar", "pma-rob-leaf-all")
                })

  .rob_row(make_domain_row(
    domain   = "Risk of bias",
    judgment = "not_serious",
    auto     = FALSE,
    notes    = paste0(
      weight_note, "; ", fold_note, ". ", dom_note, ". ",
      branch_note, " ",
      cmp$diff_note %||% cmp$note, legacy_tail, " | ",
      tbl_note
    ),
    facts    = .facts(f_high, f_weight, f_shift, f_branch,
                      .flow_path_fact(flow_ids))
  ), analysis_set = if (substantial) "low_only" else "all",
     high_idx = high_idx)
}

# Structured companion to the comparison's prose: the two pooled estimates,
# the zone each falls in, and the relative change between them. NULL when no
# comparator estimate exists. Both comparison functions return the fields it
# reads, which is what lets one fact describe either reading.
.rob_shift_fact <- function(cmp) {
  if (is.null(cmp$te_low_disp)) return(NULL)
  change_str <- if (is.finite(cmp$inflation_ratio)) {
    sprintf("relative change %.0f%% (threshold %.0f%%)",
            100 * cmp$inflation_ratio, 100 * cmp$inflation_threshold)
  } else {
    sprintf(paste0("relative change undefined (comparator estimate ~ 0; ",
                   "threshold %.0f%%)"), 100 * cmp$inflation_threshold)
  }
  .fact(
    "estimate_shift", "Pooled estimate excluding high risk of bias studies",
    sprintf(paste0("%s %s (all studies, zone %s) vs %s %s (excluding high ",
                   "risk of bias, zone %s); %s"),
            cmp$sm_label, .rob_fact_num(cmp$te_all_disp), cmp$zone_all,
            cmp$sm_label, .rob_fact_num(cmp$te_low_disp), cmp$zone_low,
            change_str),
    cmp$inflation_ratio
  )
}

# --------------------------------------------------------------------------
# Shared comparison helpers
#
# The pieces both readings of the high-vs-low comparison need, so the default
# symmetric reading and the legacy directional one cannot drift apart on what
# a zone is or how TE_low is pooled. R/pubias_status.R reaches the same
# helpers through .assess_bias_direction().
# --------------------------------------------------------------------------

# Measure label and display transform: ratio measures are judged on the log
# scale and shown back-transformed.
.rob_scale <- function(sm) {
  log_scale <- !is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM")
  list(
    log_scale = log_scale,
    sm_label  = if (!is.null(sm) && nzchar(sm)) sm else "TE",
    disp      = function(x) if (log_scale) round(exp(x), 3) else round(x, 3)
  )
}

# Half-width of the trivial zone. NULL / NA / non-positive thresholds collapse
# the zone to {0}, so only a sign change separates the zones.
.rob_zone_bound <- function(threshold_internal) {
  if (!is.null(threshold_internal) && length(threshold_internal) == 1L &&
      is.finite(threshold_internal) && threshold_internal > 0) {
    threshold_internal
  } else {
    0
  }
}

# Zone of one estimate relative to +/-M.
.rob_zone_of <- function(te, M) {
  if (length(te) != 1L || !is.finite(te)) return(NA_character_)
  if (te >  +M) "above"
  else if (te < -M) "below"
  else "trivial"
}

# The threshold sentence both readings print.
.rob_threshold_note <- function(M, scale, sm) {
  if (M > 0) {
    scale_label <- if (scale$log_scale) {
      paste0("log ", scale$sm_label, " scale")
    } else scale$sm_label
    sprintf("Threshold = +/-%s (%s)", format(scale$disp(M), nsmall = 0),
            scale_label)
  } else {
    "Threshold not supplied; trivial zone collapsed to {0} (sign-flip rule only)"
  }
}

# TE_low: the inverse-variance weighted mean of the finite low/some-RoB
# studies, or the reason it could not be formed. A FIXED-EFFECT estimate
# regardless of the parent model (see the CAVEAT on .assess_bias_direction()).
#   problem: NULL, "no_low", "sparse", "no_low_usable" or "nonfinite".
.rob_low_pool <- function(te_vec, se_vec, low_idx) {
  if (sum(low_idx) == 0 || is.null(te_vec) || is.null(se_vec)) {
    return(list(te_low = NA_real_, problem = "no_low"))
  }
  usable <- is.finite(te_vec) & is.finite(se_vec) & se_vec > 0
  if (length(usable) != length(low_idx) || !any(usable)) {
    return(list(te_low = NA_real_, problem = "sparse"))
  }
  low_usable <- low_idx & usable
  if (!any(low_usable)) {
    return(list(te_low = NA_real_, problem = "no_low_usable"))
  }
  w_low  <- 1 / se_vec[low_usable]^2
  te_low <- sum(w_low * te_vec[low_usable]) / sum(w_low)
  if (!is.finite(te_low)) {
    return(list(te_low = NA_real_, problem = "nonfinite"))
  }
  list(te_low = te_low, problem = NULL)
}

# Relative change in magnitude, (|TE_all| - |TE_low|) / |TE_low|; NA when
# TE_low is ~0 and the ratio is undefined.
.rob_relative_change <- function(te_all, te_low) {
  if (is.finite(te_low) && abs(te_low) > 1e-9) {
    (abs(te_all) - abs(te_low)) / abs(te_low)
  } else {
    NA_real_
  }
}

# --------------------------------------------------------------------------
# Default reading of Guide Fig 7-2's "substantial differences" node
#
# Symmetric, as the node is worded:
#   substantially different  <=>  zone(TE_all) != zone(TE_low)
#                             or  same non-trivial zone and
#                                 |relative change| > inflation_threshold
# The relative change counts in EITHER direction, so a deflation is as
# different as an inflation. Both estimates in the trivial zone are "similar"
# whatever their ratio: two near-null estimates lead to the same decision,
# which is the Guide's first criterion. No `small_values`, no rating: this
# function picks an analysis set and nothing else.
# --------------------------------------------------------------------------
.assess_rob_magnitude <- function(te_all, te_vec, se_vec, low_idx,
                                  inflation_threshold =
                                    PMA_ROB_INFLATION_THRESHOLD,
                                  sm = NULL, threshold_internal = NULL) {
  scale <- .rob_scale(sm)
  M     <- .rob_zone_bound(threshold_internal)
  threshold_note <- .rob_threshold_note(M, scale, sm)

  pool <- .rob_low_pool(te_vec, se_vec, low_idx)
  if (!is.null(pool$problem) || length(te_all) != 1L || !is.finite(te_all)) {
    why <- switch(pool$problem %||% "all",
      no_low        = "no low/some-RoB comparator pool exists",
      sparse        = "study-level effects are sparse or non-finite",
      no_low_usable = paste0("no finite low/some-RoB comparator studies ",
                             "remain after sparse-data filtering"),
      nonfinite     = paste0("the finite low/some-RoB comparator estimate ",
                             "could not be computed"),
      "the pooled estimate is not finite")
    note <- sprintf("High-vs-low risk of bias comparison not assessable: %s. %s(all) = %s. %s.",
                    why, scale$sm_label, .rob_fact_num(scale$disp(te_all)),
                    threshold_note)
    return(list(assessable = FALSE, substantial = FALSE, basis = "",
                diff_note = note, note = note))
  }

  te_low <- pool$te_low
  za     <- .rob_zone_of(te_all, M)
  zl     <- .rob_zone_of(te_low, M)
  ratio  <- .rob_relative_change(te_all, te_low)

  zone_change      <- !identical(za, zl)
  magnitude_change <- !zone_change && !identical(za, "trivial") &&
                      is.finite(ratio) && abs(ratio) > inflation_threshold
  substantial      <- zone_change || magnitude_change

  basis <- if (zone_change) {
    sprintf("the estimates fall in different zones: %s vs %s", za, zl)
  } else if (magnitude_change) {
    sprintf(paste0("same zone (%s), but the magnitude changes by %.0f%%, ",
                   "beyond the %.0f%% threshold"),
            za, 100 * abs(ratio), 100 * inflation_threshold)
  } else if (identical(za, "trivial")) {
    "both estimates in the trivial zone"
  } else if (is.finite(ratio)) {
    sprintf(paste0("same zone (%s), magnitude change %.0f%%, within the ",
                   "%.0f%% threshold"),
            za, 100 * abs(ratio), 100 * inflation_threshold)
  } else {
    sprintf("same zone (%s); relative change undefined (|TE_low| ~ 0)", za)
  }

  change_str <- if (is.finite(ratio)) {
    sprintf("relative change = %.1f%% (threshold %.0f%%, either direction)",
            100 * ratio, 100 * inflation_threshold)
  } else {
    sprintf("relative change = undefined (|TE_low| ~ 0; threshold %.0f%%)",
            100 * inflation_threshold)
  }
  diff_note <- sprintf(
    "%s(all) = %.3f [zone = %s]; %s(excl. high-RoB) = %.3f [zone = %s]; %s; %s",
    scale$sm_label, scale$disp(te_all), za,
    scale$sm_label, scale$disp(te_low), zl,
    change_str, threshold_note
  )

  list(
    assessable          = TRUE,
    substantial         = substantial,
    basis               = basis,
    rule                = NA_integer_,
    zone_all            = za,
    zone_low            = zl,
    inflation_ratio     = ratio,
    te_all_disp         = scale$disp(te_all),
    te_low_disp         = scale$disp(te_low),
    sm_label            = scale$sm_label,
    inflation_threshold = inflation_threshold,
    diff_note           = diff_note,
    note                = diff_note
  )
}

# --------------------------------------------------------------------------
# LEGACY: MECE 5-rule zone-and-magnitude check (v0.4 - v0.5.x)
#
# Reached by .flowchart_rob() only with rob_direction_check = TRUE (Beyond
# Core GRADE, Guide Appendix 11), where it decides the dominated branch and
# answers the undominated node with the SAME verdict read as a leaf (rules 3-5
# = substantially different, rules 1-2 = similar). R/pubias_status.R also
# calls it, for a display. Its logic is frozen at 0.5.1 so that legacy
# analyses reproduce; only its zone and pooling arithmetic moved into the
# shared helpers above.
#
#   * `rule` is NA on the bail() paths and only there, which is the
#     undominated caller's "not assessable" test.
#   * `gate_note` is returned separately because the two callers draw a
#     different consequence from it.
#
# Decision tree (za = zone(TE_all), zl = zone(TE_low)):
#   za == zl == "trivial"                                    -> "not_serious" (rule 1)
#   za == zl, non-trivial, no bias-favouring inflation > 20% -> "not_serious" (rule 2)
#   za == zl, non-trivial, bias-favouring inflation > 20%    -> "serious"     (rule 3)
#   za != zl, no sign flip across null                       -> "serious"     (rule 4)
#   za != zl, sign flip, threshold supplied              -> "very_serious"    (rule 5)
#   za != zl, sign flip, no threshold supplied           -> "serious"         (rule 5)
#
# CAVEAT — TE_low is ALWAYS a fixed-effect (common-effect) estimate.
#   It ignores tau^2 and therefore does NOT track the parent model: when
#   meta_obj was fitted with random = TRUE, te_all is a random-effects
#   estimate while te_low is a fixed-effect one, so part of any observed shift
#   can come from the estimator difference rather than from risk of bias.
#   Refitting on the low-RoB subset (rob_refit = TRUE) is the route that keeps
#   both estimates on the same footing. The same caveat applies to
#   .assess_rob_magnitude(), which pools TE_low the same way.
# --------------------------------------------------------------------------
.assess_bias_direction <- function(te_all, se_all, te_vec, se_vec, low_idx,
                                   small_values,
                                   inflation_threshold =
                                     PMA_ROB_INFLATION_THRESHOLD,
                                   sm = NULL, threshold_internal = NULL) {

  # Uniform shape for the early "cannot be assessed" returns: rule = NA marks
  # the outcome as not assessable for the non-dominated caller.
  bail <- function(judgment, note) {
    list(judgment = judgment, rule = NA_integer_, gate_note = NULL,
         diff_note = note, note = note)
  }

  # `small_values` is not re-checked here. assess_rob() gates it on the way in
  # whenever this function can be reached, and pubias_status.R gates it
  # itself.

  scale    <- .rob_scale(sm)
  sm_label <- scale$sm_label
  .disp    <- scale$disp

  M <- .rob_zone_bound(threshold_internal)
  threshold_supplied <- M > 0
  threshold_note <- .rob_threshold_note(M, scale, sm)

  pool <- .rob_low_pool(te_vec, se_vec, low_idx)

  # All studies are high-RoB (no comparator pool exists) -> rate down 1 level.
  # This is NOT the sign-flip case and must not borrow its two levels: there is
  # no restricted estimate to have landed anywhere.
  if (identical(pool$problem, "no_low")) {
    return(bail(
      judgment = "serious",
      note     = paste0(
        "All studies high-RoB; no low/some-RoB comparator pool. ",
        "Rate down 1 level (serious). ", .ROB_CAP_NOTE, " ",
        sm_label, "(all) = ", .disp(te_all), ". ", threshold_note, "."
      )
    ))
  }
  if (identical(pool$problem, "sparse")) {
    return(bail(
      judgment = "serious",
      note     = paste0(
        "Risk-of-bias direction check not assessable because study-level ",
        "effects are sparse or non-finite. Rate down 1 level ",
        "(serious). ", sm_label, "(all) = ", .disp(te_all), ". ",
        threshold_note, "."
      )
    ))
  }
  if (identical(pool$problem, "no_low_usable")) {
    return(bail(
      judgment = "serious",
      note     = paste0(
        "Risk-of-bias direction check not assessable because no finite ",
        "low/some-RoB comparator studies remain after sparse-data filtering. ",
        "Rate down 1 level (serious). ", sm_label, "(all) = ",
        .disp(te_all), ". ", threshold_note, "."
      )
    ))
  }
  if (identical(pool$problem, "nonfinite")) {
    return(bail(
      judgment = "serious",
      note     = paste0(
        "Risk-of-bias direction check not assessable because the finite ",
        "low/some-RoB comparator estimate could not be computed. Rate down ",
        "1 level (serious). ", sm_label, "(all) = ", .disp(te_all),
        ". ", threshold_note, "."
      )
    ))
  }
  te_low <- pool$te_low

  za <- .rob_zone_of(te_all, M)
  zl <- .rob_zone_of(te_low, M)

  # Direction of the bias contribution (only relevant for rule 3)
  direction_ok <- if (small_values == "undesirable") {
    te_all > te_low
  } else {
    te_all < te_low
  }

  # Relative inflation (defined only when |TE_low| > 0; rule 3 requires it).
  inflation_ratio <- .rob_relative_change(te_all, te_low)
  inflates <- isTRUE(direction_ok) &&
              is.finite(inflation_ratio) &&
              (inflation_ratio > inflation_threshold)

  # Sign flip across the null (rule 5 vs rule 4 disambiguation).
  sign_flips <- identical(za, "above") && identical(zl, "below") ||
                identical(za, "below") && identical(zl, "above")

  # 5-rule decision
  gate_note <- NULL
  if (identical(za, zl)) {
    if (identical(za, "trivial")) {
      judgment <- "not_serious"; rule <- 1L
      rule_desc <- "Rule 1: TE_all and TE_low both in trivial zone -> do not rate down"
    } else if (inflates) {
      judgment <- "serious"; rule <- 3L
      rule_desc <- paste0(
        "Rule 3: same non-trivial zone but bias-favouring inflation > ",
        "threshold -> rate down 1"
      )
    } else {
      judgment <- "not_serious"; rule <- 2L
      rule_desc <- paste0(
        "Rule 2: same non-trivial zone, inflation within threshold ",
        "(or not bias-favouring) -> do not rate down"
      )
      # Transparency: the magnitude of the shift exceeds the threshold, but the
      # direction gate blocked the downgrade. The consequence clause is the
      # caller's, not this note's.
      if (!isTRUE(direction_ok) && is.finite(inflation_ratio) &&
          abs(inflation_ratio) > inflation_threshold) {
        shift_expl <- if (identical(small_values, "undesirable")) {
          paste0("the shift is toward smaller values, the unfavourable ",
                 "direction given small_values = 'undesirable', so bias ",
                 "would not inflate the apparent benefit")
        } else {
          paste0("the shift is toward larger values, the unfavourable ",
                 "direction given small_values = 'desirable', so bias ",
                 "would not inflate the apparent benefit")
        }
        gate_note <- sprintf(
          paste0("Pooled estimate shifts by %.0f%% in absolute magnitude ",
                 "when restricted to low/some-concerns RoB studies, ",
                 "exceeding the %.0f%% threshold, but %s."),
          100 * abs(inflation_ratio), 100 * inflation_threshold, shift_expl
        )
      }
    }
  } else {
    if (sign_flips) {
      rule <- 5L
      # The threshold decides the DEPTH here, not the zones: with M = 0 the two
      # zones are separated by nothing, so the two-level departure is not
      # earned.
      if (threshold_supplied) {
        judgment  <- "very_serious"
        rule_desc <- paste0(
          "Rule 5: zone changes across null (benefit <-> harm) -> rate down 2 ",
          "(very serious). ", .ROB_TWO_LEVEL_NOTE
        )
      } else {
        judgment  <- "serious"
        rule_desc <- paste0(
          "Rule 5: zone changes across null (benefit <-> harm) -> rate down 1 ",
          "(serious). ", .ROB_SIGN_FLIP_NO_THRESHOLD_NOTE
        )
      }
    } else {
      judgment <- "serious"; rule <- 4L
      rule_desc <- "Rule 4: zone changes without sign flip -> rate down 1 (serious)"
    }
  }

  sv_desc <- sprintf("small_values = '%s'", small_values)

  inflation_str <- if (is.finite(inflation_ratio)) {
    sprintf("relative inflation = %.1f%% (threshold %.0f%%)",
            100 * inflation_ratio, 100 * inflation_threshold)
  } else {
    sprintf("relative inflation = undefined (|TE_low| ~ 0; threshold %.0f%%)",
            100 * inflation_threshold)
  }

  gate_desc <- sprintf("direction gate (bias-favouring shift): %s",
                       if (isTRUE(direction_ok)) "yes" else "no")

  diff_note <- sprintf(
    "%s(all) = %.3f [zone = %s]; %s(excl. high-RoB) = %.3f [zone = %s]; %s; %s; %s; %s",
    sm_label, .disp(te_all), za,
    sm_label, .disp(te_low), zl,
    inflation_str, gate_desc, threshold_note, sv_desc
  )

  list(
    judgment        = judgment,
    rule            = rule,
    gate_note       = gate_note,
    zone_all        = za,
    zone_low        = zl,
    sign_flips      = sign_flips,
    inflates        = inflates,
    direction_ok    = direction_ok,
    inflation_ratio = inflation_ratio,
    # Display-scale copies of the two estimates plus the measure label and the
    # threshold they were judged against, for .rob_shift_fact().
    te_all_disp         = .disp(te_all),
    te_low_disp         = .disp(te_low),
    sm_label            = sm_label,
    inflation_threshold = inflation_threshold,
    # diff_note carries the numbers only plus gate_note's arithmetic; `note`
    # adds the rule verdict and the dominated branch's consequence.
    diff_note       = paste0(
      diff_note,
      if (!is.null(gate_note)) paste0(". ", gate_note) else ""
    ),
    note            = paste0(
      diff_note, ". ", rule_desc,
      if (!is.null(gate_note)) {
        paste0(". ", gate_note,
               " Per Core GRADE guidance, no downgrade for this criterion.")
      } else ""
    )
  )
}

# --------------------------------------------------------------------------
# RoB level normalisation
# Per-study risk-of-bias label -> internal GRADE level
#
# Three vocabularies, deliberately kept apart. RoB 2 (randomised trials) has
# exactly THREE judgments; ROBINS-I (non-randomised studies) has four, and its
# extra severity is what folds onto very_serious. Conflating them is how this
# table came to advertise a four-level "RoB 2" that RoB 2 never defined.
#
# Note this is not the Core GRADE domain vocabulary the values map ONTO: a
# per-study label describes one study, a GRADE level describes the body of
# evidence. GRADE_LEVELS in R/grade_vocabulary.R owns that second vocabulary.
# --------------------------------------------------------------------------
.normalize_rob_level <- function(x) {
  aliases <- c(
    # Cochrane RoB 2, verbatim (Sterne JAC, et al. BMJ 2019;366:l4898).
    "Low risk of bias"  = "not_serious",
    "Some concerns"     = "serious",
    "High risk of bias" = "very_serious",
    # ROBINS-I, verbatim (Sterne JAC, et al. BMJ 2016;355:i4919). Four levels;
    # both of the top two rate down 2, because Core GRADE describes no
    # three-level risk-of-bias downgrade for the flowchart to reach.
    "Moderate risk of bias" = "serious",
    "Serious risk of bias"  = "very_serious",
    "Critical risk of bias" = "very_serious",
    # pmatools' own "... concerns" phrasings, kept because scripts and
    # extraction sheets written against releases up to 0.5.1 store them. Only
    # "Some concerns" is anyone else's wording; the other three are ours.
    "No concerns"       = "not_serious",
    "Serious concerns"  = "very_serious",
    "Critical concerns" = "very_serious",
    # Single-letter shortcuts
    "L" = "not_serious", "l" = "not_serious",
    "S" = "serious", "s" = "serious",
    "M" = "serious", "m" = "serious",
    "H" = "very_serious", "h" = "very_serious",
    "C" = "very_serious", "c" = "very_serious",
    "*" = "serious",
    # Plain / alternate capitalisation
    "low"          = "not_serious",  "Low"          = "not_serious",
    "moderate"     = "serious",      "Moderate"     = "serious",
    "unclear"      = "serious",      "Unclear"      = "serious",  # RoB1 wording
    "high"         = "very_serious", "High"         = "very_serious",
    "very high"    = "very_serious", "Very high"    = "very_serious",
    # Internal (pass-through + legacy)
    "not_serious"       = "not_serious",
    "no"                = "not_serious",  # legacy alias
    "some"              = "serious",      # legacy alias
    "some_concerns"     = "serious",      # legacy alias
    "serious"           = "serious",
    "very_serious"      = "very_serious",
    "extremely_serious" = "extremely_serious"
  )
  if (is.na(x)) return(NA_character_)
  if (x %in% names(aliases)) return(unname(aliases[[x]]))
  # Case-insensitive fallback ("SOME CONCERNS", "some concerns", ...)
  i <- match(tolower(trimws(x)), tolower(names(aliases)))
  if (!is.na(i)) return(unname(aliases[[i]]))
  x
}

# --------------------------------------------------------------------------
# Forest-plot strata ("low" / "some" / "high" / "unknown")
#
# Shares the alias vocabulary of .normalize_rob_level() so that grade_meta()
# and the stratified forest plots accept exactly the same labels — including
# the Cochrane RoB 2 and ROBINS-I judgments documented in README.
# Unlike .normalize_rob_levels(), unrecognised labels warn instead of
# aborting: a plot should still be drawn, but never silently.
# --------------------------------------------------------------------------

#' Normalise risk-of-bias labels to the pmatools plotting strata
#'
#' Maps free-text risk-of-bias judgments onto the four strata pmatools groups
#' studies by: \code{"low"}, \code{"some"}, \code{"high"} and \code{"unknown"}.
#'
#' @section Why this is exported:
#' Risk of bias is the one input where pmatools accepts a wide vocabulary --
#' single letters, plain words, the Cochrane RoB 2 and ROBINS-I judgments, and
#' the package's own internal level names all mean something. That vocabulary
#' is defined inside pmatools, so any caller that stores or edits RoB judgments
#' of its own (a data-entry grid with a RoB dropdown, an import step reading
#' someone else's extraction sheet) has to agree about what a label means
#' or the two will silently disagree about which studies are at high risk.
#' \code{rob_strata()} is that agreement made callable: run the labels through
#' it and you get back exactly the strata \code{\link{grade_meta}} and
#' \code{\link{plot_forest_rob}} will use, with no second copy of the alias
#' table to keep in sync.
#'
#' \strong{Cochrane RoB 2} (randomised trials) has three judgments, and they
#' are accepted verbatim:
#' \tabular{ll}{
#'   \code{"Low risk of bias"}  \tab -> \code{"low"}  \cr
#'   \code{"Some concerns"}     \tab -> \code{"some"} \cr
#'   \code{"High risk of bias"} \tab -> \code{"high"}
#' }
#'
#' \strong{ROBINS-I} (non-randomised studies) has four, also accepted
#' verbatim: \code{"Low risk of bias"} -> \code{"low"},
#' \code{"Moderate risk of bias"} -> \code{"some"},
#' \code{"Serious risk of bias"} and \code{"Critical risk of bias"} ->
#' \code{"high"}. Its extra severity folds because Core GRADE describes no
#' three-level risk-of-bias downgrade.
#'
#' Every other accepted label, case-insensitively and after trimming
#' whitespace:
#' \itemize{
#'   \item \strong{low}: \code{"not_serious"}, \code{"no"}, \code{"low"},
#'     \code{"L"}, \code{"No concerns"}
#'   \item \strong{some}: \code{"some_concerns"}, \code{"some"},
#'     \code{"S"}, \code{"M"}, \code{"*"}, \code{"moderate"},
#'     \code{"unclear"} (RoB 1 wording)
#'   \item \strong{high}: \code{"very_serious"}, \code{"extremely_serious"},
#'     \code{"high"}, \code{"very high"}, \code{"H"}, \code{"C"},
#'     \code{"Serious concerns"}, \code{"Critical concerns"}
#'   \item \strong{unknown}: \code{NA}, \code{""}, \code{"?"},
#'     \code{"unknown"}, \code{"na"}
#' }
#'
#' \code{"No concerns"}, \code{"Serious concerns"} and
#' \code{"Critical concerns"} are pmatools' own phrasings, not RoB 2's -- the
#' package advertised them as RoB 2 up to 0.5.1 and did not. They keep
#' working, so no stored extraction sheet breaks; write the RoB 2 or ROBINS-I
#' judgments in new work.
#'
#' A bare \code{"serious"} is \strong{rejected} in this release. It named the
#' \code{"high"} stratum up to 0.5.0 and names the \code{"some"} stratum from
#' 0.5.1; write \code{"some_concerns"} or \code{"very_serious"} instead, both
#' of which mean what they always did. See \code{\link{grade_meta}}.
#'
#' Anything else also becomes \code{"unknown"}, but with a warning naming the
#' offending labels. It deliberately does \strong{not} abort: this function
#' feeds plots, and a plot with an "unknown" stratum is more useful than no
#' plot at all. Callers that need a hard failure on bad input should check the
#' result for \code{"unknown"} themselves.
#'
#' @param x A character vector (or anything coercible) of risk-of-bias labels,
#'   one per study.
#' @param arg Label used to prefix the warning message, so a caller can say
#'   which of its own arguments the bad labels came from. Defaults to
#'   \code{"rob"}.
#'
#' @return A character vector the same length as \code{x}, each element one of
#'   \code{"low"}, \code{"some"}, \code{"high"}, \code{"unknown"}.
#'
#' @seealso \code{\link{plot_forest_rob}}, which strata its subgroups this way;
#'   \code{\link{grade_meta}}, whose \code{rob} argument accepts the same
#'   vocabulary.
#'
#' @examples
#' # Single letters, published judgments and internal names all map cleanly.
#' rob_strata(c("L", "S", "H"))
#' rob_strata(c("Low risk of bias", "Some concerns", "High risk of bias"))
#' rob_strata(c("Moderate risk of bias", "Critical risk of bias"))
#' rob_strata(c("not_serious", "some_concerns", "very_serious"))
#'
#' # Missing and explicitly unknown judgments become "unknown" quietly.
#' rob_strata(c("low", NA, "", "?"))
#'
#' # Anything unrecognised becomes "unknown" with a warning, never an error.
#' suppressWarnings(rob_strata(c("low", "not sure yet")))
#'
#' # The `arg` prefix names the caller's own argument in that warning.
#' suppressWarnings(rob_strata("not sure yet", arg = "my_app: rob column"))
#'
#' @export
rob_strata <- function(x, arg = "rob") {
  v <- trimws(as.character(x))
  blank <- is.na(v) | !nzchar(v) | tolower(v) %in% c("na", "?", "unknown")

  out <- rep("unknown", length(v))
  if (!any(!blank)) return(out)

  .check_grade_level_input(
    v[!blank], arg,
    extra = paste0(
      "A study-level risk-of-bias label of \"serious\" no longer means the ",
      "same stratum it did in pmatools 0.5.0."
    )
  )

  lvl <- vapply(v[!blank], .normalize_rob_level, character(1), USE.NAMES = FALSE)
  strata <- unname(c(not_serious = "low", serious = "some",
                     very_serious = "high", extremely_serious = "high")[lvl])

  bad <- is.na(strata)
  if (any(bad)) {
    rlang::warn(paste0(
      arg, ": unrecognized label(s) -> \"unknown\" stratum: ",
      paste(unique(v[!blank][bad]), collapse = ", "),
      ". Accepted values: 'not_serious'/'no'/'low'/'L', ",
      "'some_concerns'/'some'/'S', 'very_serious'/'high'/'H', ",
      "the Cochrane RoB 2 judgments ('Low risk of bias', 'Some concerns', ",
      "'High risk of bias') or the ROBINS-I ones ('Low risk of bias', ",
      "'Moderate risk of bias', 'Serious risk of bias', ",
      "'Critical risk of bias')."
    ))
    strata[bad] <- "unknown"
  }

  out[!blank] <- strata
  out
}

# Internal alias kept so existing call sites (plot_forest_rob.R,
# plot_forest_indirectness.R) do not move.
.rob_plot_strata <- function(x, arg = "rob") rob_strata(x, arg = arg)

# --------------------------------------------------------------------------
# Automatic refit on the low-RoB subset
#
# Core GRADE Guide Fig 7-2's non-dominated branch can end in "use low risk of bias
# studies only". Making that recommendation without acting on it would leave
# the SoF table showing an estimate the flowchart just told the reader not to
# use, so grade_meta() refits the meta object on the low-RoB subset and every
# downstream domain works from the refitted object.
#
# {meta}'s update.meta() is used (via the `update` generic — the method is
# registered but not exported) because it preserves every setting of the
# original call. Failures never stop the assessment: the recommendation is
# still returned, with a warning, and the full analysis is retained.
# --------------------------------------------------------------------------
.refit_low_rob <- function(meta_obj, high_idx) {
  skip <- function(note, warn = TRUE) {
    if (warn) rlang::warn(note)
    list(meta = meta_obj, refit = FALSE, note = note, n_low = NA_integer_)
  }

  if (is.null(high_idx) || !is.logical(high_idx) || !any(high_idx)) {
    return(skip(paste0(
      "Risk of bias: the low-RoB refit was requested but no high-RoB studies ",
      "were identified; the full analysis is retained."
    ), warn = FALSE))
  }

  # update.meta()'s `subset =` indexes the ORIGINAL data rows, so `high_idx`
  # must be studlab-aligned; assess_rob() runs it through .rob_expand() out of
  # k-space before attaching it. The length check is kept as the guard for the
  # case where that expansion was not possible (alignment unresolvable):
  # skipping with a warning is the only safe answer, because subsetting with a
  # short logical vector would recycle it and silently keep the wrong studies.
  n_studies <- length(meta_obj$studlab %||% meta_obj$TE)
  if (length(high_idx) != n_studies) {
    return(skip(paste0(
      "Risk of bias: could not refit on low risk of bias studies because the ",
      "per-study risk-of-bias vector (length ", length(high_idx), ") does not ",
      "align with the meta object (", n_studies, " studies). The full ",
      "analysis is retained; consider running the restricted analysis manually."
    )))
  }

  keep  <- !high_idx
  n_low <- sum(keep)
  if (n_low < 2L) {
    return(skip(sprintf(paste0(
      "Risk of bias: Core GRADE Guide Fig 7-2 recommends restricting the analysis ",
      "to low risk of bias studies, but only %d such study remains, which ",
      "cannot be pooled. The full analysis is retained."), n_low)))
  }

  refit <- tryCatch(stats::update(meta_obj, subset = keep),
                    error = function(e) e, warning = function(w) w)
  if (inherits(refit, "condition") || !inherits(refit, "meta")) {
    msg <- if (inherits(refit, "condition")) conditionMessage(refit) else "unknown cause"
    return(skip(paste0(
      "Risk of bias: refitting the meta-analysis on low risk of bias studies ",
      "failed (", msg, "). The recommendation to restrict the analysis stands, ",
      "but the full analysis is retained."
    )))
  }
  # update() rebuilds the object, and with it drops pmatools' own attributes.
  # The score types are keyed by studlab, so the full vector serves the subset.
  attr(refit, "pmatools_score_type") <- attr(meta_obj, "pmatools_score_type")
  if (is.null(refit$k) || !is.finite(refit$k) || refit$k < 2L) {
    return(skip(paste0(
      "Risk of bias: refitting on low risk of bias studies left fewer than 2 ",
      "poolable studies. The full analysis is retained."
    )))
  }

  note <- sprintf(paste0(
    "Analysis refitted on low risk of bias studies only (%d of %d studies) ",
    "per Core GRADE Guide Fig 7-2; all downstream domains and the effect estimate ",
    "use the restricted analysis."), refit$k, meta_obj$k)
  rlang::inform(note)
  list(meta = refit, refit = TRUE, note = note, n_low = as.integer(refit$k))
}

.normalize_rob_levels <- function(rob_vec) {
  result <- vapply(rob_vec, .normalize_rob_level, character(1))
  unknown <- !result %in% GRADE_LEVELS
  if (any(unknown)) {
    rlang::abort(paste0(
      "Unrecognized RoB level(s): ", paste(unique(rob_vec[unknown]), collapse = ", "),
      ". Accepted values: 'no', 'some_concerns', 'very_serious', the Cochrane ",
      "RoB 2 judgments ('Low risk of bias', 'Some concerns', ",
      "'High risk of bias') or the ROBINS-I ones ('Low risk of bias', ",
      "'Moderate risk of bias', 'Serious risk of bias', ",
      "'Critical risk of bias')."
    ))
  }
  unname(result)
}
