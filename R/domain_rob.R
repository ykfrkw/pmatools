# domain_rob.R - Risk of Bias domain assessment
#
# v0.5 (Phase B): the domain follows the BMJ 2025 Core GRADE 4 Fig 2
# flowchart literally. The weight-share dominance gate — retired in v0.3.1 —
# is reinstated because it is the first decision node of that figure.
#
# --------------------------------------------------------------------------
# Step 0. Binary classification of each study
#
#   rob_some_concerns = "low"  (default) : {not_serious, serious} -> low
#                                          {very_serious}         -> high
#   rob_some_concerns = "high"           : {not_serious}          -> low
#                                          {serious, very_serious} -> high
#
#   PROVENANCE: the binary verdict is Core GRADE 4's (verbatim: "For
#   simplicity, however, Core GRADE users can assess the overall risk of bias
#   in individual studies as low or high"), but the FOLD is not. The string
#   "some concerns" does not occur anywhere in Core GRADE 4; it belongs to
#   three-level tools such as RoB 2. Core GRADE 4 instead sets the low/high
#   boundary by counting high-risk ITEMS, and its three worked examples use
#   three different counts:
#     "if at least one item was rated as high risk of bias, authors considered
#      the trial as overall high risk of bias"
#     "required two or more of the seven items (authors omitted one irrelevant
#      item) rated as high risk of bias to consider the overall risk of bias as
#      high"
#     "considered a study at overall high risk of bias only if three or more of
#      the eight items were assessed as high risk of bias"
#   and it leaves the choice open: "The choice of threshold--high risk of bias
#   in only one or more than one item or category--may be an issue that will be
#   impossible to resolve". rob_some_concerns is therefore a pmatools
#   convenience for callers who arrive with a three-level rating already made,
#   not a rule taken from the source.
#
#   Study-level overrides (`rob_overrides`, keyed on studlab) are applied
#   before the fold and each one requires a written rationale.
#
# --------------------------------------------------------------------------
# Step 1. Dominance gate (Core GRADE 4 Fig 2, first node)
#
#   w_high = IV weight share carried by high-RoB studies.
#   dominated  <=>  w_high >= rob_dominant_threshold
#
#   Core GRADE 4 Fig 2 footnote, verbatim:
#     "*Possible thresholds for high risk of bias 'dominating': >65% weight or
#      >=55% weight=possibly dominating."
#   The figure offers two candidates; pmatools defaults to the conservative
#   one, 0.55, with a `>=` comparison so that exactly 55% counts as dominated
#   (matching the ">=55%" wording). Set rob_dominant_threshold = 0.65 for the
#   stricter reading.
#
#   If the weight share cannot be computed, the count share is used instead
#   (stated in the notes); if neither can be computed, dominance is assumed
#   (conservative).
#
#   The count-share fallback has NO basis in Core GRADE 4, which speaks only of
#   "weight in the meta-analysis" (Fig 2 footnote and the surrounding text:
#   "thresholds of weight in the meta-analysis of >65% or >=55% of the
#   weight"). It exists so that mock / hand-built meta objects without usable
#   inverse-variance weights still produce a verdict, and it can differ
#   substantially from the weight share when the studies are of uneven size.
#   Every note produced on that path says so.
#
# --------------------------------------------------------------------------
# Step 2a. dominated = Yes -> "Check direction of bias"
#
#   Implemented by .assess_bias_direction(), the MECE 5-rule zone-based check
#   introduced in v0.4. TE_all and TE_low are each classified into one of
#   three zones defined by +/-Threshold:
#     above   : TE > +Threshold
#     trivial : -Threshold <= TE <= +Threshold
#     below   : TE < -Threshold
#
#   Decision (zone(TE_all) = za, zone(TE_low) = zl):
#     Rule 1: za == zl == "trivial"                    -> "not_serious"
#     Rule 2: za == zl, non-trivial, inflation <= 10%  -> "not_serious"
#     Rule 3: za == zl, non-trivial, inflation > 10%   -> "serious"  (-1)
#     Rule 4: za != zl, no sign flip across null       -> "serious"  (-1)
#     Rule 5: za != zl, sign flip (above <-> below)    -> "serious"  (-1)
#
#   Rate down at most ONE level (v0.5). Core GRADE 4 describes no two-level
#   risk-of-bias downgrade: the only "two levels" in the paper is about rating
#   UP observational evidence, and every leaf of Fig 2 reads "rate down" /
#   "do not rate down". Rule 5 (sign flip) and the all-studies-high-RoB case
#   used to return "very_serious" (-2); both are now capped at "serious".
#   "very_serious" stays reachable through the scalar `rob` override, which
#   requires rob_rationale.
#
#   Rules 1-2 are the figure's "bias would under-estimate an existing effect /
#   over-estimate an absent one" branch (do not rate down); rules 3-5 are its
#   "bias could account for the apparent effect (or its absence)" branch
#   (rate down, using all studies).
#
#   inflation_ratio = (|TE_all| - |TE_low|) / |TE_low|, evaluated only when the
#   bias direction is bias-favouring (per `small_values`); a deflation in the
#   bias-favouring direction never triggers a downgrade.
#
#   Fallback: when `threshold_internal` is NULL/NA/<=0 the trivial zone
#   collapses to {0}, so only sign-flip (rule 5) can trigger a zone change.
#
# --------------------------------------------------------------------------
# Step 2b. dominated = No -> "Is there appreciable evidence from low RoB
#          studies?" -> "Substantial difference between high and low RoB
#          estimates?"
#
#   Neither answer rates down (this is the behaviour change of v0.5: the
#   non-dominated branch of Fig 2 never rates the domain down). What differs
#   is the recommended analysis set:
#
#     substantial difference = Yes -> analysis_set = "low_only"
#                                     (use low risk of bias studies only)
#     substantial difference = No  -> analysis_set = "all"
#
#   "Substantial difference" is judged on MAGNITUDE ONLY (v0.5): a zone
#   change, or |relative change| > `rob_inflation_threshold` in either
#   direction. Core GRADE 4 (p6) verbatim:
#     "In contrast, when appreciable evidence from low risk of bias studies
#      exists, with reasonable thresholds for appreciable being >=35 to >=45%
#      of the weight in the pooled analysis, Core GRADE users should consider,
#      for each outcome of interest, whether low and high risk of bias studies
#      suggest similar or substantially different magnitudes of effect."
#   That node is symmetric — it does not ask whether the difference runs in
#   the bias-favouring direction — so the `small_values` direction gate is NOT
#   applied here. It remains in force on the dominated branch, whose node is
#   explicitly "Check direction of bias".
#
#   grade_meta() acts on `analysis_set = "low_only"` by refitting the meta
#   object on the low-RoB subset (`rob_refit = TRUE`, the default), so every
#   downstream domain and the SoF table use the restricted estimate.
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
# --------------------------------------------------------------------------
# Inputs:
#   (a) scalar GRADE level: bypass flowchart
#   (b) length-k vector: apply flowchart
#   (c) column name in meta_obj$data: expand to vector and apply flowchart
#
# Edge case: when every study is high-RoB (n_low == 0) the weight share is
# 100%, so the dominated branch is taken; there is no low/some-RoB comparator
# pool, and the domain is rated "serious" (rate down 1 level). Up to
# v0.4.0 this returned "very_serious" (-2); Core GRADE 4 supports no automatic
# two-level risk-of-bias downgrade, so a reviewer who judges -2 appropriate
# must say so with rob = "very_serious" + rob_rationale.
#
# small_values:
#   "undesirable": small values are bad (e.g., response rate, OR for benefit)
#                  TE_all > TE_low indicates inflation toward favorable
#   "desirable"  : small values are good (e.g., mortality, severity)
#                  TE_all < TE_low indicates inflation toward favorable
#   NULL         : direction unknown; use |TE_all| > |TE_low| (further from null)

# --------------------------------------------------------------------------
# Flowchart node vocabulary (inst/figures/rob.svg)
#
# Every id below is a <g id="..."> in that file, and the "flow_path" fact
# emitted by .flowchart_rob() names the subset this assessment traversed. The
# Shiny app reads the fact, hands the ids to www/flowchart.js, and the picture
# lights up the route the judgment actually took; ?grade_flowcharts renders
# the same figure unhighlighted.
#
# The constant exists so the two halves cannot drift apart in silence:
# tests/testthat/test-flowchart-nodes.R asserts that every id here is present
# in the SVG and that no emitted path names an id that is not here. Adding a
# branch without drawing it therefore fails the build.
#
# There is no entry node. Fig 2 as pmatools draws it opens at the dominance
# question, so the "no high risk of bias study at all" case is routed through
# it rather than around it -- see the n_high == 0 return below for why that is
# the same decision and not a convenient fiction.
.ROB_FIG2_NODE_IDS <- c(
  "pma-rob-node-dominance",
  "pma-rob-edge-dominance-yes",
  "pma-rob-node-direction",
  "pma-rob-edge-direction-rules",
  "pma-rob-leaf-rule1",
  "pma-rob-leaf-rule2",
  "pma-rob-leaf-rule3",
  "pma-rob-leaf-rule4",
  "pma-rob-leaf-rule5",
  "pma-rob-leaf-rulena",
  "pma-rob-edge-dominance-no",
  "pma-rob-node-appreciable",
  "pma-rob-edge-appreciable-no",
  "pma-rob-edge-appreciable-yes",
  "pma-rob-node-magnitude",
  "pma-rob-edge-magnitude-similar",
  "pma-rob-leaf-all",
  "pma-rob-edge-magnitude-different",
  "pma-rob-leaf-lowonly"
)

# The one place a traversed path is turned into a fact. Space-separated
# because a fact's `value` is a single pre-formatted string; `numeric` stays
# NA because a route is not a quantity.
.flow_path_fact <- function(ids) {
  .fact("flow_path", "Flowchart path",
        paste(ids, collapse = " "), NA_real_)
}

# Note appended wherever an automated risk-of-bias path used to rate down two
# levels. Core GRADE 4 does not describe a two-level risk-of-bias downgrade.
.ROB_CAP_NOTE <- paste0(
  "Core GRADE 4 describes no automatic two-level downgrade for risk of bias ",
  "(every leaf of Fig 2 is 'rate down' / 'do not rate down'), so this ",
  "automated judgment is capped at one level. If two levels are genuinely ",
  "warranted, supply the scalar override rob = 'serious' with rob_rationale."
)

#' Assess the Risk of Bias domain (Core GRADE series; internal)
#'
#' Applies the BMJ Core GRADE 4 Fig 2 flowchart documented at the top of this
#' file whenever `rob` is a per-study vector (or column name); scalar GRADE
#' levels bypass the flowchart.
#'
#' @param rob Scalar GRADE level, per-study vector, or column name in
#'   `meta_obj$data`.
#' @param meta_obj A `meta::metagen`-like object.
#' @param rob_some_concerns `"low"` (default) or `"high"`: which side of the
#'   binary low/high classification `"serious"` studies are folded into.
#' @param rob_overrides Named character vector of study-level Risk-of-Bias
#'   overrides, keyed on `meta_obj$studlab`. Every key needs a matching
#'   rationale in `rob_override_rationale`.
#' @param rob_override_rationale Named character vector of rationales, same
#'   keys as `rob_overrides`.
#' @param rob_dominant_threshold Weight share (0, 1] at or above which the
#'   evidence counts as dominated by high-RoB studies (Core GRADE 4 Fig 2,
#'   first node). The figure's footnote offers two candidates —
#'   `">65% weight or >=55% weight=possibly dominating"` — and the default is
#'   the conservative one, `0.55`; the comparison is `>=`, matching the
#'   `">=55%"` wording.
#' @param rob_inflation_threshold Threshold for the relative change of the
#'   pooled estimate when high-RoB studies are excluded, computed on the
#'   absolute analysis scale:
#'   \eqn{(|TE_{all}| - |TE_{low}|) / |TE_{low}|}, where \eqn{TE_{low}} is the
#'   inverse-variance weighted mean of the low/some-concerns RoB studies.
#'   Default `0.10`. A downgrade under rule 3 requires BOTH (a) the relative
#'   change to be strictly greater than this threshold (`>`, so a change
#'   exactly at the threshold does not rate down) AND (b) the shift to be in
#'   the bias-favouring direction per `small_values`: only shifts that would
#'   make the apparent effect look more favourable (over-estimation) count.
#'   Shifts toward a smaller or less favourable effect never trigger a
#'   downgrade under this criterion, even when their magnitude exceeds the
#'   threshold; when that happens the domain note states it explicitly. The
#'   direction requirement applies only to the *dominated* branch ("check
#'   direction of bias"); on the non-dominated branch the "substantially
#'   different magnitudes of effect" node is symmetric, so a shift of either
#'   direction beyond the threshold counts.
#'   When every study is high-RoB, no low/some-RoB comparator pool exists,
#'   the check cannot run, and the domain is rated `"serious"` (rate
#'   down 1 level).
#' @param small_values `"desirable"`, `"undesirable"`, or `NULL`. Defines the
#'   bias-favouring direction; when `NULL`, `|TE_all| > |TE_low|` is used and
#'   a warning is emitted if that assumption drives a downgrade.
#' @param threshold_internal Clinical decision threshold on the analysis
#'   scale (defines the trivial zone).
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
                       rob_inflation_threshold = 0.10,
                       small_values            = NULL,
                       threshold_internal      = NULL,
                       rationale               = NULL) {
  k <- meta_obj$k

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
                        override_notes      = ovr$notes)

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

# --------------------------------------------------------------------------
# k-space <-> studlab-space alignment
#
# {meta} keeps $studlab / $TE / $seTE / $w.random at the length of the
# ORIGINAL data rows but counts only the estimable ones in $k (a study with
# missing results, or a double-zero study under method = "Inverse", is
# dropped from the pool but not from the data). The Core GRADE 4 Fig 2 maths
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
  if (is.null(x)) return("low")
  x <- as.character(x)[1]
  if (!x %in% c("low", "high")) {
    rlang::abort(paste0(
      "rob_some_concerns must be 'low' or 'high': it decides whether studies ",
      "rated 'some concerns' are folded into the low or the high risk-of-bias ",
      "group for the Core GRADE 4 Fig 2 dominance gate."
    ))
  }
  x
}

.check_dominant_threshold <- function(x) {
  if (is.null(x)) return(0.55)
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x > 1) {
    rlang::abort(paste0(
      "rob_dominant_threshold must be a single weight share in (0, 1] ",
      "(e.g., 0.55, the Core GRADE 4 Fig 2 'possibly dominating' threshold, ",
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
        "'not_serious', 'some_concerns', 'very_serious', or Cochrane RoB2 ",
        "labels ('No concerns', 'Some concerns', 'Serious concerns', ",
        "'Critical concerns')."
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
# Flowchart (v0.5: BMJ Core GRADE 4 Fig 2, dominance gate reinstated)
# --------------------------------------------------------------------------
.flowchart_rob <- function(rob_vec, meta_obj,
                           dominant_threshold = 0.55,
                           inflation_threshold = 0.10, small_values = NULL,
                           threshold_internal = NULL,
                           some_concerns_as = "low",
                           override_notes = NULL) {

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

  # If no high-RoB studies at all, no possibility of bias-driven inflation.
  #
  # The path walks the ordinary undominated route rather than a branch of its
  # own, and that is exact rather than convenient: with no high-RoB study the
  # dominance share is 0, which is below any threshold in (0, 1], and "analyse
  # all studies" is the right answer when there is nothing to exclude. Every
  # id below is a node the reader can see, so the picture lights up instead of
  # showing an unlit chart with no explanation.
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
        "pma-rob-node-dominance", "pma-rob-edge-dominance-no",
        "pma-rob-node-appreciable", "pma-rob-edge-appreciable-no",
        "pma-rob-leaf-all")))
    ), analysis_set = "all", high_idx = high_idx))
  }

  # ---- Core GRADE 4 Fig 2, node 1: is the evidence dominated by high-RoB
  # studies? Boundary is inclusive (`>=`), so exactly 60% counts as dominated.
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
             "convention with no basis in Core GRADE 4, which speaks only of ",
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

  # Direction-and-magnitude check (always run when at least one high-RoB
  # study is present). Align TE / seTE to length k so logical indexing with
  # high_idx is correct.
  te_vec  <- meta_obj$TE
  se_vec  <- meta_obj$seTE
  if (!is.null(te_vec) && length(te_vec) != n_total) {
    keep <- !is.na(te_vec)
    if (sum(keep) == n_total) {
      te_vec <- te_vec[keep]
      if (!is.null(se_vec) && length(se_vec) == length(meta_obj$TE)) {
        se_vec <- se_vec[keep]
      }
    } else {
      keep <- is.finite(te_vec)
      if (sum(keep) == n_total) {
        te_vec <- te_vec[keep]
        if (!is.null(se_vec) && length(se_vec) == length(meta_obj$TE)) {
          se_vec <- se_vec[keep]
        }
      }
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

  dir <- .assess_bias_direction(
    te_all              = te_all,
    se_all              = se_all,
    te_vec              = te_vec,
    se_vec              = se_vec,
    low_idx             = !high_idx,
    small_values        = small_values,
    inflation_threshold = inflation_threshold,
    sm                  = meta_obj$sm,
    threshold_internal  = threshold_internal,
    # The non-dominated branch neither rates down nor consults the direction
    # gate (its Fig 2 node is symmetric), so the small_values warning would be
    # doubly wrong there; only the dominated "check direction of bias" branch
    # can be driven by the assumption.
    warn_direction_assumption = dominated
  )

  # Structured companion to diff_note: the two pooled estimates, the zone each
  # falls in, and the relative change between them. Omitted on the bail() paths
  # of .assess_bias_direction(), where no comparator estimate exists.
  f_shift <- if (!is.null(dir$te_low_disp)) {
    change_str <- if (is.finite(dir$inflation_ratio)) {
      sprintf("relative change %.0f%% (threshold %.0f%%)",
              100 * dir$inflation_ratio, 100 * dir$inflation_threshold)
    } else {
      sprintf(paste0("relative change undefined (comparator estimate ~ 0; ",
                     "threshold %.0f%%)"), 100 * dir$inflation_threshold)
    }
    .fact(
      "estimate_shift", "Pooled estimate excluding high risk of bias studies",
      sprintf(paste0("%s %s (all studies, zone %s) vs %s %s (excluding high ",
                     "risk of bias, zone %s); %s"),
              dir$sm_label, .rob_fact_num(dir$te_all_disp), dir$zone_all,
              dir$sm_label, .rob_fact_num(dir$te_low_disp), dir$zone_low,
              change_str),
      dir$inflation_ratio
    )
  } else NULL

  # ---- Node 2a: dominated -> "check direction of bias" (the 5-rule check).
  if (dominated) {
    f_branch <- .fact(
      "fig2_branch", "Core GRADE 4 Fig 2",
      if (!is.na(dir$rule)) {
        sprintf("dominated by high risk of bias studies; direction-of-bias rule %d (%s)",
                dir$rule,
                if (identical(dir$judgment, "not_serious")) "do not rate down" else "rate down")
      } else {
        paste0("dominated by high risk of bias studies; direction of bias not ",
               "assessable (rate down)")
      },
      as.numeric(dir$rule)
    )
    # The rule number picks the leaf; an unassessable direction has its own.
    rule_leaf <- if (is.na(dir$rule)) {
      "pma-rob-leaf-rulena"
    } else {
      paste0("pma-rob-leaf-rule", dir$rule)
    }
    return(.rob_row(make_domain_row(
      domain   = "Risk of bias",
      judgment = dir$judgment,
      auto     = FALSE,
      notes    = paste0(
        weight_note, "; ", fold_note, ". ", dom_note, ". ",
        dir$note, " | ",
        tbl_note
      ),
      facts    = .facts(f_high, f_weight, f_shift, f_branch,
                        .flow_path_fact(c(
                          "pma-rob-node-dominance",
                          "pma-rob-edge-dominance-yes",
                          "pma-rob-node-direction",
                          "pma-rob-edge-direction-rules",
                          rule_leaf)))
    ), analysis_set = "all", high_idx = high_idx))
  }

  # ---- Node 2b: not dominated -> appreciable low-RoB evidence? substantial
  # difference between the high- and low-RoB estimates? Neither answer rates
  # the domain down; only the recommended analysis set changes.
  # v0.5: the "substantially different magnitudes of effect" node of Core
  # GRADE 4 Fig 2 is symmetric, so this branch judges magnitude only -- the
  # `small_values` direction gate (dir$direction_ok, which gates rule 3) is
  # deliberately NOT consulted here. Without that, a body of evidence whose
  # low-RoB studies show the LARGER effect used to be reported as "no
  # substantial difference".
  assessable  <- !is.na(dir$magnitude_substantial %||% NA)
  substantial <- assessable && isTRUE(dir$magnitude_substantial)

  branch_note <- if (!assessable) {
    paste0(
      "Low-RoB studies do not provide appreciable evidence (the high-vs-low ",
      "comparison is not assessable), so a substantial difference cannot be ",
      "established. Per Core GRADE 4 Fig 2 the non-dominated branch does not ",
      "rate down; the analysis retains all studies."
    )
  } else if (substantial) {
    paste0(
      "Substantially different magnitudes of effect between the high-RoB and ",
      "low-RoB estimates (magnitude only; Core GRADE 4's node asks whether ",
      "the two suggest 'similar or substantially different magnitudes of ",
      "effect' and does not require the difference to run in the ",
      "bias-favouring direction). Per Core GRADE 4 Fig 2: do not rate down, ",
      "but use low risk of bias studies only (analysis_set = 'low_only')."
    )
  } else {
    paste0(
      "No substantial difference in magnitude between the high-RoB and ",
      "low-RoB estimates (same zone and relative change within the ",
      "threshold). Per Core GRADE 4 Fig 2: do not rate down; all studies are ",
      "used (analysis_set = 'all')."
    )
  }

  # The rule number is recorded whenever the 5-rule check produced one, even
  # though on this branch it did not decide anything: Core GRADE 4's
  # non-dominated node asks only about magnitude.
  f_branch <- .fact(
    "fig2_branch", "Core GRADE 4 Fig 2",
    if (!assessable) {
      paste0("not dominated; high-vs-low risk of bias comparison not ",
             "assessable -> use all studies")
    } else if (substantial) {
      paste0("not dominated; substantially different magnitudes of effect ",
             "-> use low risk of bias studies only")
    } else {
      "not dominated; similar magnitudes of effect -> use all studies"
    },
    as.numeric(dir$rule)
  )

  # Fig 2's non-dominated side has two nodes, and "not assessable" is the
  # answer to the FIRST of them ("is there appreciable evidence from the low
  # risk of bias studies?"), not a third answer to the second. Routing it
  # through the appreciable node's "no" edge rather than the magnitude node's
  # "similar" edge is what keeps the picture honest: the magnitude question
  # was never asked.
  flow_ids <- c("pma-rob-node-dominance", "pma-rob-edge-dominance-no",
                "pma-rob-node-appreciable")
  flow_ids <- if (!assessable) {
    c(flow_ids, "pma-rob-edge-appreciable-no", "pma-rob-leaf-all")
  } else if (substantial) {
    c(flow_ids, "pma-rob-edge-appreciable-yes", "pma-rob-node-magnitude",
      "pma-rob-edge-magnitude-different", "pma-rob-leaf-lowonly")
  } else {
    c(flow_ids, "pma-rob-edge-appreciable-yes", "pma-rob-node-magnitude",
      "pma-rob-edge-magnitude-similar", "pma-rob-leaf-all")
  }

  .rob_row(make_domain_row(
    domain   = "Risk of bias",
    judgment = "not_serious",
    auto     = FALSE,
    notes    = paste0(
      weight_note, "; ", fold_note, ". ", dom_note, ". ",
      branch_note, " ",
      dir$diff_note %||% dir$note, " | ",
      tbl_note
    ),
    facts    = .facts(f_high, f_weight, f_shift, f_branch,
                      .flow_path_fact(flow_ids))
  ), analysis_set = if (substantial) "low_only" else "all",
     high_idx = high_idx)
}

# --------------------------------------------------------------------------
# MECE 5-rule zone-and-magnitude check (v0.4+)
#
# v0.5: unchanged logic; it is now the implementation of the "check direction
# of bias" node of Core GRADE 4 Fig 2 (reached on the dominated branch) and,
# on the non-dominated branch, the source of the "substantial difference"
# answer (rules 3-5 = yes). Two additions for that second caller:
#   * `rule` and `diff_note` are returned on every path (NA / the full note on
#     the early not-assessable returns) so the caller can re-word the outcome;
#   * `warn_direction_assumption` suppresses the small_values warning when the
#     caller emits its own (the non-dominated branch never rates down).
#
# Zones (defined by +/-Threshold on the analysis scale):
#   above   : TE > +Threshold
#   trivial : -Threshold <= TE <= +Threshold
#   below   : TE < -Threshold
# Fallback when Threshold is NULL/NA/<=0: trivial zone collapses to {0}, so
# above means TE > 0 and below means TE < 0; only sign flips can trigger zone
# change.
#
# Decision tree (za = zone(TE_all), zl = zone(TE_low)):
#   za == zl == "trivial"                                    -> "not_serious" (rule 1)
#   za == zl, non-trivial, no bias-favouring inflation > 10% -> "not_serious" (rule 2)
#   za == zl, non-trivial, bias-favouring inflation > 10%    -> "serious"     (rule 3)
#   za != zl, no sign flip across null                       -> "serious"     (rule 4)
#   za != zl, sign flip (above <-> below)                    -> "serious"     (rule 5)
#
# Rule 5 returned "very_serious" (-2) up to v0.4. Since v0.5.0 every automated
# risk-of-bias path is capped at one level (see .ROB_CAP_NOTE above): Core
# GRADE 4 describes no two-level risk-of-bias downgrade. "very_serious" is reachable
# only through the scalar `rob` override, which requires rob_rationale.
#
# CAVEAT — TE_low is ALWAYS a fixed-effect (common-effect) estimate.
#   te_low <- sum(w * TE) / sum(w) with w = 1 / seTE^2, computed over the
#   low/some-RoB studies only. It ignores tau^2 and therefore does NOT track
#   the parent model: when meta_obj was fitted with random = TRUE, te_all is a
#   random-effects estimate while te_low is a fixed-effect one, so part of any
#   observed shift can come from the estimator difference rather than from
#   risk of bias. The gap widens with heterogeneity and with unequal study
#   sizes. Core GRADE 4 does not specify how to recompute the restricted
#   estimate; refitting the model on the low-RoB subset (rob_refit = TRUE) is
#   the route that keeps both estimates on the same footing.
# --------------------------------------------------------------------------
.assess_bias_direction <- function(te_all, se_all, te_vec, se_vec, low_idx,
                                   small_values, inflation_threshold = 0.10,
                                   sm = NULL, threshold_internal = NULL,
                                   warn_direction_assumption = TRUE) {

  n_low <- sum(low_idx)

  # Uniform shape for the early "cannot be assessed" returns: rule = NA marks
  # the outcome as not assessable for the non-dominated caller.
  bail <- function(judgment, note) {
    list(judgment = judgment, rule = NA_integer_,
         magnitude_substantial = NA, diff_note = note, note = note)
  }

  if (!is.null(small_values) && !small_values %in% c("desirable", "undesirable")) {
    rlang::abort("small_values must be 'desirable' or 'undesirable'.")
  }

  # Display label and back-transform for ratio measures
  log_scale <- !is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM")
  sm_label  <- if (!is.null(sm) && nzchar(sm)) sm else "TE"
  .disp <- function(x) if (log_scale) round(exp(x), 3) else round(x, 3)

  # Threshold handling
  M <- if (!is.null(threshold_internal) && is.finite(threshold_internal) && threshold_internal > 0) {
    threshold_internal
  } else {
    0
  }
  threshold_supplied <- M > 0
  zone_of <- function(te) {
    if (!is.finite(te)) return(NA_character_)
    if (te >  +M) "above"
    else if (te < -M) "below"
    else "trivial"
  }
  threshold_disp <- if (threshold_supplied) sprintf("+/-%s", format(.disp(M), nsmall = 0)) else "+/-0"
  threshold_note <- if (threshold_supplied) {
    sprintf("Threshold = %s (%s)", threshold_disp, if (log_scale) paste0("log ", sm_label, " scale") else sm_label)
  } else {
    "Threshold not supplied; trivial zone collapsed to {0} (sign-flip rule only)"
  }

  # All studies are high-RoB (no comparator pool exists) -> rate down 1 level.
  # Core GRADE 4 never describes an automatic two-level risk-of-bias downgrade
  # (every Fig 2 leaf is "rate down" / "do not rate down"), so this is capped
  # at "serious"; use rob = "very_serious" + rob_rationale for -2.
  if (n_low == 0 || is.null(te_vec) || is.null(se_vec)) {
    return(bail(
      judgment = "serious",
      note     = paste0(
        "All studies high-RoB; no low/some-RoB comparator pool. ",
        "Rate down 1 level (serious). ", .ROB_CAP_NOTE, " ",
        sm_label, "(all) = ", .disp(te_all), ". ", threshold_note, "."
      )
    ))
  }

  usable <- is.finite(te_vec) & is.finite(se_vec) & se_vec > 0
  if (length(usable) != length(low_idx) || !any(usable)) {
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

  low_usable <- low_idx & usable
  if (!any(low_usable)) {
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

  # TE_low: inverse-variance weighted mean of finite low/some-RoB studies.
  # This is a FIXED-EFFECT estimate regardless of the parent model (see the
  # caveat in the block comment above): tau^2 plays no part here, so a
  # random-effects te_all is compared against a common-effect te_low.
  w_low  <- 1 / se_vec[low_usable]^2
  te_low <- sum(w_low * te_vec[low_usable]) / sum(w_low)
  if (!is.finite(te_low)) {
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

  za <- zone_of(te_all)
  zl <- zone_of(te_low)

  # Direction of the bias contribution (only relevant for rule 3)
  direction_ok <- if (is.null(small_values)) {
    abs(te_all) > abs(te_low)
  } else if (small_values == "undesirable") {
    te_all > te_low
  } else {
    te_all < te_low
  }

  # Relative inflation (defined only when |TE_low| > 0; rule 3 requires it).
  if (abs(te_low) > 1e-9) {
    inflation_ratio <- (abs(te_all) - abs(te_low)) / abs(te_low)
  } else {
    inflation_ratio <- NA_real_
  }
  inflates <- isTRUE(direction_ok) &&
              is.finite(inflation_ratio) &&
              (inflation_ratio > inflation_threshold)

  # Sign flip across the null (rule 5 vs rule 4 disambiguation).
  sign_flips <- identical(za, "above") && identical(zl, "below") ||
                identical(za, "below") && identical(zl, "above")

  # Direction-free "substantially different magnitudes of effect" (Core GRADE 4
  # p6). Used only by the non-dominated branch, whose figure node is symmetric.
  # This is rules 3/4/5 with the `direction_ok` gate removed: a zone change, or
  # a same-non-trivial-zone relative change beyond the threshold in EITHER
  # direction (including the low-RoB studies showing the LARGER effect). Rule
  # 1's exemption survives: when both estimates sit inside the trivial zone
  # their magnitudes are not substantially different however large the
  # percentage change between two near-null numbers looks.
  magnitude_substantial <- !identical(za, zl) ||
    (!identical(za, "trivial") &&
       is.finite(inflation_ratio) &&
       abs(inflation_ratio) > inflation_threshold)

  # 5-rule decision
  gate_note <- NULL
  if (identical(za, zl)) {
    if (identical(za, "trivial")) {
      judgment <- "not_serious"; rule <- 1L
      rule_desc <- "Rule 1: TE_all and TE_low both in trivial zone -> do not rate down"
    } else if (inflates) {
      judgment <- "serious"; rule <- 3L
      rule_desc <- "Rule 3: same non-trivial zone but bias-favouring inflation > threshold -> rate down 1"
      if (is.null(small_values) && isTRUE(warn_direction_assumption)) {
        rlang::warn(paste0(
          "assess_rob(): small_values was not supplied, so the bias-direction ",
          "gate assumed that a larger absolute pooled effect ",
          "(|TE_all| > |TE_low|) indicates bias-favouring inflation. This ",
          "assumption determined the risk-of-bias downgrade (rule 3). Supply ",
          "small_values = 'desirable' or 'undesirable' to make the direction ",
          "of bias explicit."
        ))
      }
    } else {
      judgment <- "not_serious"; rule <- 2L
      rule_desc <- "Rule 2: same non-trivial zone, inflation within threshold (or not bias-favouring) -> do not rate down"
      # Transparency: the magnitude of the shift exceeds the threshold, but the
      # direction gate blocked the downgrade. Say so explicitly so readers do
      # not conclude the threshold was ignored.
      if (!isTRUE(direction_ok) && is.finite(inflation_ratio) &&
          abs(inflation_ratio) > inflation_threshold) {
        shift_expl <- if (is.null(small_values)) {
          paste0("the pooled estimate moves closer to the null when high-RoB ",
                 "studies are included, so bias would not inflate the ",
                 "apparent effect (small_values not supplied; |TE| ",
                 "comparison used)")
        } else if (identical(small_values, "undesirable")) {
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
                 "exceeding the %.0f%% threshold, but %s; per Core GRADE ",
                 "guidance, no downgrade for this criterion."),
          100 * abs(inflation_ratio), 100 * inflation_threshold, shift_expl
        )
      }
    }
  } else {
    if (sign_flips) {
      judgment <- "serious"; rule <- 5L
      rule_desc <- paste0(
        "Rule 5: zone changes across null (benefit <-> harm) -> rate down 1 ",
        "(serious). ", .ROB_CAP_NOTE
      )
    } else {
      judgment <- "serious"; rule <- 4L
      rule_desc <- "Rule 4: zone changes without sign flip -> rate down 1 (serious)"
    }
  }

  sv_desc <- if (is.null(small_values)) {
    "small_values = NULL (using |TE| comparison for inflation direction)"
  } else {
    sprintf("small_values = '%s'", small_values)
  }

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
    magnitude_substantial = magnitude_substantial,
    zone_all        = za,
    zone_low        = zl,
    sign_flips      = sign_flips,
    inflates        = inflates,
    direction_ok    = direction_ok,
    inflation_ratio = inflation_ratio,
    # Display-scale copies of the two estimates (back-transformed for ratio
    # measures) plus the measure label and the threshold they were judged
    # against. diff_note interpolates the same numbers into prose; the caller
    # needs them as values to record the structured `estimate_shift` fact
    # without re-deriving (and possibly re-rounding) them.
    te_all_disp         = .disp(te_all),
    te_low_disp         = .disp(te_low),
    sm_label            = sm_label,
    inflation_threshold = inflation_threshold,
    # diff_note carries the numbers only (zones, inflation, gate, threshold);
    # `note` adds the rule verdict. The non-dominated branch of the flowchart
    # uses diff_note so it can state its own (non-downgrading) verdict.
    diff_note       = paste0(
      diff_note,
      if (!is.null(gate_note)) paste0(". ", gate_note) else ""
    ),
    note            = paste0(
      diff_note, ". ", rule_desc,
      if (!is.null(gate_note)) paste0(". ", gate_note) else ""
    )
  )
}

# --------------------------------------------------------------------------
# RoB level normalisation
# Cochrane RoB2 / plain English -> internal GRADE level
# --------------------------------------------------------------------------
.normalize_rob_level <- function(x) {
  aliases <- c(
    # Cochrane RoB2 (3-level mapping: critical folds into very_serious)
    "No concerns"       = "not_serious",
    "Some concerns"     = "serious",
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
# the Cochrane RoB2 wording documented in README ("Some concerns", ...).
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
#' single letters, plain words, the Cochrane RoB 2 sentences, and the package's
#' own internal level names all mean something. That vocabulary is defined
#' inside pmatools, so any caller that stores or edits RoB judgments of its own
#' (a data-entry grid with a RoB dropdown, an import step reading someone
#' else's extraction sheet) has to agree with pmatools about what a label means
#' or the two will silently disagree about which studies are at high risk.
#' \code{rob_strata()} is that agreement made callable: run the labels through
#' it and you get back exactly the strata \code{\link{grade_meta}} and
#' \code{\link{plot_forest_rob}} will use, with no second copy of the alias
#' table to keep in sync.
#'
#' Accepted labels, case-insensitively and after trimming whitespace:
#' \itemize{
#'   \item \strong{low}: \code{"not_serious"}, \code{"no"}, \code{"low"},
#'     \code{"L"}, \code{"No concerns"}
#'   \item \strong{some}: \code{"some_concerns"}, \code{"some"},
#'     \code{"S"}, \code{"M"}, \code{"*"}, \code{"moderate"},
#'     \code{"unclear"} (RoB 1 wording), \code{"Some concerns"}
#'   \item \strong{high}: \code{"very_serious"}, \code{"extremely_serious"},
#'     \code{"high"}, \code{"very high"}, \code{"H"}, \code{"C"},
#'     \code{"Serious concerns"}, \code{"Critical concerns"}
#'   \item \strong{unknown}: \code{NA}, \code{""}, \code{"?"},
#'     \code{"unknown"}, \code{"na"}
#' }
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
#' # Single letters, RoB 2 sentences and internal names all map cleanly.
#' rob_strata(c("L", "S", "H"))
#' rob_strata(c("No concerns", "Some concerns", "Critical concerns"))
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
      "or Cochrane RoB2 labels ('No concerns', ",
      "'Some concerns', 'Serious concerns', 'Critical concerns')."
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
# Core GRADE 4 Fig 2's non-dominated branch can end in "use low risk of bias
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
      "Risk of bias: Core GRADE 4 Fig 2 recommends restricting the analysis ",
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
  if (is.null(refit$k) || !is.finite(refit$k) || refit$k < 2L) {
    return(skip(paste0(
      "Risk of bias: refitting on low risk of bias studies left fewer than 2 ",
      "poolable studies. The full analysis is retained."
    )))
  }

  note <- sprintf(paste0(
    "Analysis refitted on low risk of bias studies only (%d of %d studies) ",
    "per Core GRADE 4 Fig 2; all downstream domains and the effect estimate ",
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
      ". Accepted values: 'no', 'some', 'serious', 'very_serious', or Cochrane RoB2 labels ",
      "('No concerns', 'Some concerns', 'Serious concerns', 'Critical concerns')."
    ))
  }
  unname(result)
}
