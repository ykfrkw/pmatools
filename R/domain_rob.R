# domain_rob.R - Risk of Bias domain assessment
#
# v0.5 (Phase B): the domain follows the BMJ 2025 Core GRADE 4 Fig 2
# flowchart literally. The weight-share dominance gate — retired in v0.3.1 —
# is reinstated because it is the first decision node of that figure.
#
# --------------------------------------------------------------------------
# Step 0. Binary classification of each study
#
#   rob_some_concerns = "low"  (default) : {no, some_concerns} -> low
#                                          {serious}           -> high
#   rob_some_concerns = "high"           : {no}                -> low
#                                          {some_concerns, serious} -> high
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
#     Rule 1: za == zl == "trivial"                    -> "no"
#     Rule 2: za == zl, non-trivial, inflation <= 10%  -> "no"
#     Rule 3: za == zl, non-trivial, inflation > 10%   -> "some_concerns"  (-1)
#     Rule 4: za != zl, no sign flip across null       -> "some_concerns"  (-1)
#     Rule 5: za != zl, sign flip (above <-> below)    -> "some_concerns"  (-1)
#
#   Rate down at most ONE level (v0.5.1). Core GRADE 4 describes no two-level
#   risk-of-bias downgrade: the only "two levels" in the paper is about rating
#   UP observational evidence, and every leaf of Fig 2 reads "rate down" /
#   "do not rate down". Rule 5 (sign flip) and the all-studies-high-RoB case
#   used to return "serious" (-2); both are now capped at "some_concerns".
#   "serious" stays reachable through the scalar `rob` override, which
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
#   "Substantial difference" is judged on MAGNITUDE ONLY (v0.5.1): a zone
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
#     attr(row, "high_idx")     : logical vector of high-RoB studies, or NULL
#
# --------------------------------------------------------------------------
# Inputs:
#   (a) scalar GRADE level: bypass flowchart
#   (b) length-k vector: apply flowchart
#   (c) column name in meta_obj$data: expand to vector and apply flowchart
#
# Edge case: when every study is high-RoB (n_low == 0) the weight share is
# 100%, so the dominated branch is taken; there is no low/some-RoB comparator
# pool, and the domain is rated "some_concerns" (rate down 1 level). Before
# v0.5.1 this returned "serious" (-2); Core GRADE 4 supports no automatic
# two-level risk-of-bias downgrade, so a reviewer who judges -2 appropriate
# must say so with rob = "serious" + rob_rationale.
#
# small_values:
#   "undesirable": small values are bad (e.g., response rate, OR for benefit)
#                  TE_all > TE_low indicates inflation toward favorable
#   "desirable"  : small values are good (e.g., mortality, severity)
#                  TE_all < TE_low indicates inflation toward favorable
#   NULL         : direction unknown; use |TE_all| > |TE_low| (further from null)

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
#'   binary low/high classification `"some_concerns"` studies are folded into.
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
#'   the check cannot run, and the domain is rated `"some_concerns"` (rate
#'   down 1 level).
#' @param small_values `"desirable"`, `"undesirable"`, or `NULL`. Defines the
#'   bias-favouring direction; when `NULL`, `|TE_all| > |TE_low|` is used and
#'   a warning is emitted if that assumption drives a downgrade.
#' @param threshold_internal Clinical decision threshold on the analysis
#'   scale (defines the trivial zone).
#' @return A 1-row tibble with attributes `"analysis_set"` (`"all"` or
#'   `"low_only"`) and `"high_idx"` (logical vector or `NULL`).
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
      judgment = "no",
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

  # Vector: normalise + length check
  rob <- .normalize_rob_levels(rob)
  if (length(rob) != k) {
    rlang::abort(paste0(
      "rob must be a scalar GRADE level, a column name in meta_obj$data, ",
      "or a vector of length k (", k, "). Got length ", length(rob), "."
    ))
  }

  validate_grade_level(rob, "rob")

  # Study-level overrides (keyed on studlab) are applied on the normalised
  # vector, before the binary low/high fold, and every one is recorded.
  ovr <- .apply_rob_overrides(rob, meta_obj, rob_overrides,
                              rob_override_rationale)

  .flowchart_rob(ovr$rob, meta_obj,
                 dominant_threshold  = rob_dominant_threshold,
                 inflation_threshold = rob_inflation_threshold,
                 small_values        = small_values,
                 threshold_internal  = threshold_internal,
                 some_concerns_as    = rob_some_concerns,
                 override_notes      = ovr$notes)
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
.apply_rob_overrides <- function(rob, meta_obj, rob_overrides,
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

  studlab <- meta_obj$studlab
  if (is.null(studlab) || length(studlab) != length(rob)) {
    rlang::abort(paste0(
      "rob_overrides requires meta_obj$studlab to be available and the same ",
      "length as the per-study risk-of-bias vector (got ",
      length(studlab %||% character(0)), " study labels for ", length(rob),
      " risk-of-bias judgments)."
    ))
  }
  studlab <- as.character(studlab)

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

    to <- .normalize_rob_level(unname(rob_overrides[[key]]))
    if (!to %in% GRADE_LEVELS) {
      rlang::abort(paste0(
        "rob_overrides[[", shQuote(key), "]] = ",
        shQuote(unname(rob_overrides[[key]])),
        " is not a recognized risk-of-bias level. Accepted values: 'no', ",
        "'some_concerns', 'serious', or Cochrane RoB2 labels ('No concerns', ",
        "'Some concerns', 'Serious concerns', 'Critical concerns')."
      ))
    }

    hit <- which(studlab == key)
    for (i in hit) {
      notes <- c(notes, sprintf("Study-level override: %s %s -> %s (%s)",
                                key, rob[i], to, trimws(rat)))
      rob[i] <- to
    }
  }

  list(rob = rob, notes = notes)
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

  # Binary low/high classification. "serious" is always high (legacy
  # "very_serious" is still recognized after .normalize_rob_levels);
  # "some_concerns" goes to whichever side rob_some_concerns selects.
  high_levels <- c("serious", "very_serious")
  if (identical(some_concerns_as, "high")) {
    high_levels <- c(high_levels, "some_concerns", "some")
  }
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

  tbl_note <- paste(
    paste0(names(table(rob_vec)), ": n=", as.integer(table(rob_vec))),
    collapse = "; "
  )
  if (length(override_notes) > 0) {
    tbl_note <- paste0(tbl_note, " | ", paste(override_notes, collapse = " | "))
  }

  # If no high-RoB studies at all, no possibility of bias-driven inflation.
  if (n_high == 0) {
    return(.rob_row(make_domain_row(
      domain   = "Risk of bias",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        "No high-RoB studies. ", weight_note, "; ", fold_note,
        ". -> Do not rate down. | ", tbl_note
      )
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

  # ---- Node 2a: dominated -> "check direction of bias" (the 5-rule check).
  if (dominated) {
    return(.rob_row(make_domain_row(
      domain   = "Risk of bias",
      judgment = dir$judgment,
      auto     = FALSE,
      notes    = paste0(
        weight_note, "; ", fold_note, ". ", dom_note, ". ",
        dir$note, " | ",
        tbl_note
      )
    ), analysis_set = "all", high_idx = high_idx))
  }

  # ---- Node 2b: not dominated -> appreciable low-RoB evidence? substantial
  # difference between the high- and low-RoB estimates? Neither answer rates
  # the domain down; only the recommended analysis set changes.
  # v0.5.1: the "substantially different magnitudes of effect" node of Core
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

  .rob_row(make_domain_row(
    domain   = "Risk of bias",
    judgment = "no",
    auto     = FALSE,
    notes    = paste0(
      weight_note, "; ", fold_note, ". ", dom_note, ". ",
      branch_note, " ",
      dir$diff_note %||% dir$note, " | ",
      tbl_note
    )
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
#   za == zl == "trivial"                                     -> "no"            (rule 1)
#   za == zl, non-trivial, no bias-favouring inflation > 10%  -> "no"            (rule 2)
#   za == zl, non-trivial, bias-favouring inflation > 10%     -> "some_concerns" (rule 3)
#   za != zl, no sign flip across null                        -> "some_concerns" (rule 4)
#   za != zl, sign flip (above <-> below)                     -> "serious"       (rule 5)
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
  # at "some_concerns"; use rob = "serious" + rob_rationale for -2.
  if (n_low == 0 || is.null(te_vec) || is.null(se_vec)) {
    return(bail(
      judgment = "some_concerns",
      note     = paste0(
        "All studies high-RoB; no low/some-RoB comparator pool. ",
        "Rate down 1 level (some concerns). ", .ROB_CAP_NOTE, " ",
        sm_label, "(all) = ", .disp(te_all), ". ", threshold_note, "."
      )
    ))
  }

  usable <- is.finite(te_vec) & is.finite(se_vec) & se_vec > 0
  if (length(usable) != length(low_idx) || !any(usable)) {
    return(bail(
      judgment = "some_concerns",
      note     = paste0(
        "Risk-of-bias direction check not assessable because study-level ",
        "effects are sparse or non-finite. Rate down 1 level ",
        "(some concerns). ", sm_label, "(all) = ", .disp(te_all), ". ",
        threshold_note, "."
      )
    ))
  }

  low_usable <- low_idx & usable
  if (!any(low_usable)) {
    return(bail(
      judgment = "some_concerns",
      note     = paste0(
        "Risk-of-bias direction check not assessable because no finite ",
        "low/some-RoB comparator studies remain after sparse-data filtering. ",
        "Rate down 1 level (some concerns). ", sm_label, "(all) = ",
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
      judgment = "some_concerns",
      note     = paste0(
        "Risk-of-bias direction check not assessable because the finite ",
        "low/some-RoB comparator estimate could not be computed. Rate down ",
        "1 level (some concerns). ", sm_label, "(all) = ", .disp(te_all),
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
      judgment <- "no"; rule <- 1L
      rule_desc <- "Rule 1: TE_all and TE_low both in trivial zone -> do not rate down"
    } else if (inflates) {
      judgment <- "some_concerns"; rule <- 3L
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
      judgment <- "no"; rule <- 2L
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
      judgment <- "some_concerns"; rule <- 5L
      rule_desc <- paste0(
        "Rule 5: zone changes across null (benefit <-> harm) -> rate down 1 ",
        "(some concerns). ", .ROB_CAP_NOTE
      )
    } else {
      judgment <- "some_concerns"; rule <- 4L
      rule_desc <- "Rule 4: zone changes without sign flip -> rate down 1 (some_concerns)"
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
    # Cochrane RoB2 (3-level mapping: critical -> serious in v0.3+)
    "No concerns"       = "no",
    "Some concerns"     = "some_concerns",
    "Serious concerns"  = "serious",
    "Critical concerns" = "serious",
    # Single-letter shortcuts
    "L" = "no", "l" = "no",
    "S" = "some_concerns", "s" = "some_concerns",
    "M" = "some_concerns", "m" = "some_concerns",
    "H" = "serious", "h" = "serious",
    "C" = "serious", "c" = "serious",
    "*" = "some_concerns",
    # Plain / alternate capitalisation
    "low"          = "no",            "Low"          = "no",
    "moderate"     = "some_concerns", "Moderate"     = "some_concerns",
    "unclear"      = "some_concerns", "Unclear"      = "some_concerns",  # RoB1 wording
    "high"         = "serious",       "High"         = "serious",
    "very high"    = "serious",       "Very high"    = "serious",
    # Internal (pass-through + legacy)
    "no"            = "no",
    "some"          = "some_concerns",   # legacy alias
    "some_concerns" = "some_concerns",
    "serious"       = "serious",
    "very_serious"  = "serious"           # legacy alias
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
.rob_plot_strata <- function(x, arg = "rob") {
  v <- trimws(as.character(x))
  blank <- is.na(v) | !nzchar(v) | tolower(v) %in% c("na", "?", "unknown")

  out <- rep("unknown", length(v))
  if (!any(!blank)) return(out)

  lvl <- vapply(v[!blank], .normalize_rob_level, character(1), USE.NAMES = FALSE)
  strata <- unname(c(no = "low", some_concerns = "some", serious = "high")[lvl])

  bad <- is.na(strata)
  if (any(bad)) {
    rlang::warn(paste0(
      arg, ": unrecognized label(s) -> \"unknown\" stratum: ",
      paste(unique(v[!blank][bad]), collapse = ", "),
      ". Accepted values: 'no'/'low'/'L', 'some_concerns'/'some'/'S', ",
      "'serious'/'high'/'H', or Cochrane RoB2 labels ('No concerns', ",
      "'Some concerns', 'Serious concerns', 'Critical concerns')."
    ))
    strata[bad] <- "unknown"
  }

  out[!blank] <- strata
  out
}

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
