# domain_indirectness.R — 非直接性ドメイン処理
#
# BMJ 2025 Core GRADE 5: 非直接性は臨床的判断が必要なため手動入力。
# domain_rob.R と同じ入力パターンをサポート。
#
# Inputs (mutually exclusive with the subdomain table):
#   (a) scalar GRADE level ("not_serious" / "serious" / "very_serious" /
#       "extremely_serious")
#   (b) length-k vector of per-study GRADE levels (weight-share aggregation)
#   (c) column name in meta_obj$data (expanded to (b))
#   (d) `indirectness_subdomains`: a PICO subdomain table (Core GRADE 5)
#
# (b)/(c) Per-study aggregation: weight-share dominance, not worst case
# ---------------------------------------------------------------------------
# Indirectness is a judgment about the body of evidence, and Core GRADE 5
# frames it in terms of where the bulk of the evidence sits (p2-3, verbatim):
#
#   "However, if Core GRADE users are interested in effects in elderly people
#    but all or almost all evidence comes from younger people, in low dose but
#    all or almost all evidence comes from high dose, or in long follow-up but
#    all or almost all evidence comes from short follow-up, they lack the data
#    to test whether effects differ across these variables."
#
# A worst-case fold (used up to v0.4.0) rated the whole body of evidence down
# when a single study out of eighteen was indirect, which is the opposite of
# "all or almost all". Since v0.5 the aggregation is:
#
#   w_serious = inverse-variance weight share of studies rated "very_serious"
#   w_any     = weight share of studies rated "serious" or "very_serious"
#
#   w_serious >= indirectness_dominant_threshold -> "very_serious" (-2)
#   w_any     >= indirectness_dominant_threshold -> "serious"      (-1)
#   otherwise                                    -> "not_serious"  ( 0)
#
# "extremely_serious" (-3) is never produced here; it is a manual override
# only, like everywhere else in the package.
#
# CAVEAT: the threshold itself has NO basis in Core GRADE 5, which offers no
# numeric operationalisation of "all or almost all". The default 0.55 is a
# pmatools convention, chosen to match rob_dominant_threshold (whose value
# *is* sourced, from Core GRADE 4 Fig 2). This is stated in the domain notes
# on every aggregated judgment. When study weights cannot be computed the
# count share is used instead and the note says so.
#
# The subdomain table (d) keeps its worst-case fold: subdomains are facets of
# one judgment, not units of evidence, so weighting them is meaningless.
#
# (d) Subdomain judgments — a pmatools implementation of Core GRADE 5's
#     per-PICO reasoning (NOT a Core GRADE 5 publication format)
# ---------------------------------------------------------------------------
# ATTRIBUTION. Core GRADE 5 asks the indirectness question separately for each
# PICO element, and pmatools implements that as a per-subdomain table. The
# TABLE ITSELF, the 4-point answer scale and the question wording "Is the
# evidence sufficiently direct?" are pmatools conventions: the published
# article body carries only Table 1 (an adapted summary of findings table) and
# Table 2 ("Summary of indirectness issues"), and contains none of the strings
# "sufficiently direct", "probably yes" or "probably no". (The online
# supplementary appendices have not been checked.)
#
# HOW CORE GRADE 5 ACTUALLY POSES THE QUESTION. It does not ask a yes/no
# directness question; it asks how likely it is that the effect differs
# substantially between the target PICO and the available evidence, and Table 2
# grades that likelihood per PICO element (verbatim column header: "Likelihood
# of rating down"), asymmetrically:
#
#   Population   -> "Low likelihood because relative effects are typically
#                    similar across populations"
#   Intervention -> "Intermediate likelihood depending on underlying biology
#                    and on magnitude of issues such as non-adherence and
#                    frequency of switching"
#   Comparison   -> "Substantial likelihood in trials of new agents when an
#                    effective treatment already exists, particularly more
#                    than one effective treatment"
#   Outcome      -> "High likelihood because of frequent disappointing results
#                    in randomised controlled trials examining patient
#                    important outcomes"
#
# The worst-case fold below is SYMMETRIC across the four elements and therefore
# does not reproduce that gradient; users must weigh it themselves.
#
# Rating down two levels: Core GRADE 5 (verbatim) — "Although one might
# consider rating down more than one level for indirectness for any PICO
# element, this possibility is typically more salient for surrogate outcomes"
# and "the decision to rate down one or two levels depends on one's
# understanding of the likelihood that change in the patient important outcome
# will follow change in the surrogate".
#
# Each subdomain (typically Population / Intervention / Comparison / Outcome,
# but any set is accepted) answers the pmatools 4-point scale. Each answer maps
# to a GRADE level:
#
#   answer          | meaning                          | GRADE level
#   ----------------|----------------------------------|---------------
#   yes             | directly applicable              | no
#   probably_yes    | minor, unimportant differences   | no
#   probably_no     | important differences            | some_concerns  (-1)
#   no              | evidence is not applicable       | serious        (-2)
#
# NOTE ON VOCABULARY. The level names are shared with Risk of bias and are Core
# GRADE's own words ("not serious; serious; very serious; or, rarely, extremely
# serious", Core GRADE 1). See R/utils.R for the table and for why a bare
# "serious" is refused in this release. Domain notes render the level through
# .indirectness_level_label(), which defers to .grade_level_wording().
#
# The overall domain judgment defaults to the WORST case across subdomains
# (unlike the per-study aggregation above, which is weight-share based: a
# subdomain is a facet of one judgment, not a unit of evidence). Because
# Core GRADE 5 treats indirectness as a clinical-judgment domain, a scalar
# `indirectness` may still be supplied alongside the subdomain table to
# override that default; a value different from the default requires
# `indirectness_rationale` (Core GRADE transparency principle). Supplying the
# same value as the default is a harmless restatement and needs no rationale.
# "No override" is `indirectness = NULL` (or the argument left out): NULL — not
# the string "no" — is what grade_meta() defaults to, so callers that always
# pass every argument do not accidentally override the subdomain worst case.

# 4-point subdomain answer scale (Core GRADE 5)
INDIRECTNESS_ANSWERS <- c("yes", "probably_yes", "probably_no", "no")

# Answer -> GRADE level contribution (see the table above). The keys are the
# 4-point answers to "Is the evidence sufficiently direct?", NOT GRADE levels;
# only the values moved when the level vocabulary did.
INDIRECTNESS_ANSWER_TO_GRADE <- c(
  yes          = "not_serious",
  probably_yes = "not_serious",
  probably_no  = "serious",
  no           = "very_serious"
)

# Human-readable answer labels (table rendering / domain notes)
INDIRECTNESS_ANSWER_LABELS <- c(
  yes          = "Yes",
  probably_yes = "Probably yes",
  probably_no  = "Probably no",
  no           = "No"
)

# The domain notes used to carry their own copy of the level -> wording table.
# There is one display vocabulary for the whole package (.grade_level_wording()
# in R/utils.R) and this domain has no reason to be the exception.
.indirectness_level_label <- function(level) {
  .grade_level_wording(level)
}

# Overall-judgment labels for the subdomain table footer row
INDIRECTNESS_OVERALL_LABELS <- c(
  not_serious       = "No serious indirectness\n(no rating down)",
  serious           = "Serious indirectness\n(rate down 1 level)",
  very_serious      = "Very serious indirectness\n(rate down 2 levels)",
  extremely_serious = "Extremely serious indirectness\n(rate down 3 levels)"
)

#' Assess the Indirectness domain (Core GRADE series; internal)
#'
#' @param indirectness Scalar GRADE level, per-study vector, column name, or
#'   \code{NULL}. When \code{subdomains} is supplied this argument acts as a
#'   manual override of the worst-case subdomain judgment (scalar only).
#' @param meta_obj A meta object.
#' @param rationale Free-text justification for a manual override.
#' @param subdomains Optional PICO subdomain table (see
#'   \code{\link{grade_meta}}).
#' @param dominant_threshold Weight share at or above which per-study
#'   indirectness dominates the body of evidence. Default \code{0.55}; see the
#'   caveat at the top of this file (the value has no basis in Core GRADE 5).
#' @return A one-row tibble from \code{make_domain_row()}.
#' @keywords internal
#' @noRd
assess_indirectness <- function(indirectness, meta_obj, rationale = NULL,
                                subdomains = NULL,
                                dominant_threshold = 0.55) {
  # Subdomain table takes over the whole domain (Core GRADE 5 style).
  if (!is.null(subdomains)) {
    return(.assess_indirectness_subdomains(subdomains, indirectness, rationale))
  }

  k <- meta_obj$k
  dominant_threshold <- .check_indirectness_dominant_threshold(dominant_threshold)

  # デフォルト: スカラ "not_serious"
  if (is.null(indirectness)) indirectness <- "not_serious"

  # 列名参照. The legacy spellings have to be recognised here too, or a scalar
  # `indirectness = "no"` is taken for a column name and aborts.
  if (length(indirectness) == 1 && is.character(indirectness) &&
      !indirectness %in% c(GRADE_LEVELS, names(GRADE_LEVEL_ALIASES))) {
    col <- indirectness
    data <- meta_obj$data
    if (is.null(data) || !col %in% names(data)) {
      rlang::abort(paste0(
        "indirectness = '", col, "' is not a valid GRADE level and was not found ",
        "as a column in the meta object's data."
      ))
    }
    ind_vec <- as.character(data[[col]])
    return(.aggregate_indirectness(ind_vec, meta_obj, dominant_threshold))
  }

  # スカラ
  # v0.4.0 (breaking): any scalar other than the default "not_serious" is a
  # manual override and requires indirectness_rationale. "not_serious" (= no
  # downgrade, the documented default) stays exempt so default calls keep
  # working.
  if (length(indirectness) == 1) {
    validate_grade_level(indirectness, "indirectness")
    ind_norm <- .normalize_grade_level(indirectness)
    if (!identical(ind_norm, "not_serious")) {
      .check_override_rationale(rationale, "indirectness_rationale",
                                "Indirectness")
    }
    return(make_domain_row(
      domain    = "Indirectness",
      judgment  = ind_norm,
      auto      = FALSE,
      notes     = "Overall judgment provided by user.",
      rationale = rationale
    ))
  }

  # ベクタ
  if (length(indirectness) == k) {
    validate_grade_level(indirectness, "indirectness")
    return(.aggregate_indirectness(indirectness, meta_obj, dominant_threshold))
  }

  rlang::abort(paste0(
    "indirectness must be a scalar GRADE level, a column name, ",
    "or a vector of length k (", k, "). Got length ", length(indirectness), "."
  ))
}

# Validate indirectness_dominant_threshold. Mirrors
# .check_dominant_threshold() (Risk of bias) so the two arguments behave alike.
.check_indirectness_dominant_threshold <- function(x) {
  if (is.null(x)) return(0.55)
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x > 1) {
    rlang::abort(paste0(
      "indirectness_dominant_threshold must be a single weight share in ",
      "(0, 1] (default 0.55). It sets how much of the pooled weight must come ",
      "from indirect studies before the body of evidence counts as indirect ",
      "(Core GRADE 5: 'all or almost all evidence comes from ...'). Core ",
      "GRADE 5 gives no numeric threshold; 0.55 is a pmatools convention."
    ))
  }
  as.numeric(x)
}

# Caveat text attached to every aggregated per-study judgment.
.INDIRECTNESS_THRESHOLD_CAVEAT <- paste0(
  "Core GRADE 5 describes indirectness of the body of evidence qualitatively ",
  "('all or almost all evidence comes from ...') and gives no numeric ",
  "threshold; indirectness_dominant_threshold is a pmatools convention ",
  "(default 0.55, aligned with rob_dominant_threshold)."
)

# Inverse-variance weight share carried by the studies flagged by `idx`.
# Returns NA_real_ when no usable weights are available (the caller then falls
# back to the count share). Mirrors the alignment logic of the Risk-of-bias
# dominance gate: {meta} can keep full-length $TE/$w.* while $k counts fewer
# rows, so vectors are realigned to n_total before indexing.
.indirectness_weight_share <- function(meta_obj, idx, n_total) {
  if (is.null(meta_obj) || n_total <= 0L) return(NA_real_)
  align <- function(v) {
    if (is.null(v)) return(NULL)
    if (length(v) == n_total) return(v)
    keep <- is.finite(v) & v > 0
    if (sum(keep) == n_total) return(v[keep])
    keep_te <- is.finite(meta_obj$TE)
    if (length(keep_te) == length(v) && sum(keep_te) == n_total) return(v[keep_te])
    NULL
  }
  w_vec <- NULL
  for (slot in c("w.random", "w.common", "w.fixed")) {
    v <- align(meta_obj[[slot]])
    if (!is.null(v)) {
      ok <- is.finite(v) & v > 0
      if (any(ok) && sum(v[ok]) > 0) { w_vec <- v; break }
    }
  }
  if (is.null(w_vec)) {
    se <- align(meta_obj$seTE)
    if (!is.null(se)) {
      v <- 1 / se^2
      if (any(is.finite(v) & v > 0)) w_vec <- v
    }
  }
  if (is.null(w_vec) || length(w_vec) != n_total) return(NA_real_)
  ok <- is.finite(w_vec)
  w_total <- sum(w_vec[ok], na.rm = TRUE)
  if (!is.finite(w_total) || w_total <= 0) return(NA_real_)
  sum(w_vec[idx & ok], na.rm = TRUE) / w_total
}

.aggregate_indirectness <- function(ind_vec, meta_obj = NULL,
                                    dominant_threshold = 0.55) {
  validate_grade_level(ind_vec, "indirectness")
  ind_vec <- .normalize_grade_level(ind_vec)
  n   <- length(ind_vec)
  tbl <- table(ind_vec)
  counts_note <- paste(
    paste0(names(tbl), ": n=", as.integer(tbl)), collapse = "; "
  )

  idx_serious <- ind_vec == "very_serious"
  idx_any     <- ind_vec %in% c("serious", "very_serious")

  share_serious <- .indirectness_weight_share(meta_obj, idx_serious, n)
  share_any     <- .indirectness_weight_share(meta_obj, idx_any, n)

  if (is.na(share_serious) || is.na(share_any)) {
    share_serious <- sum(idx_serious) / max(1L, n)
    share_any     <- sum(idx_any)     / max(1L, n)
    basis      <- "count"
    basis_note <- paste0(
      "Study weights could not be computed, so the shares below are COUNT ",
      "shares rather than inverse-variance weight shares. The count-share ",
      "fallback is a pmatools convention with no basis in Core GRADE 5, ",
      "which frames the body of evidence qualitatively; a count share can ",
      "differ substantially from the weight share when study sizes are ",
      "uneven."
    )
  } else {
    basis      <- "weight"
    basis_note <- ""
  }

  if (share_serious >= dominant_threshold) {
    judgment <- "very_serious"
    decision <- sprintf(
      paste0("Studies with a per-study rating of 'serious' carry %.0f%% of ",
             "the %s >= %.0f%% -> the body of evidence is dominated by ",
             "evidence that is not applicable; very serious indirectness, ",
             "rate down 2 levels."),
      100 * share_serious, basis, 100 * dominant_threshold
    )
  } else if (share_any >= dominant_threshold) {
    judgment <- "serious"
    decision <- sprintf(
      paste0("Indirect studies (per-study rating 'some concerns' or ",
             "'serious') carry %.0f%% of the %s >= %.0f%% -> serious ",
             "indirectness; rate down 1 level."),
      100 * share_any, basis, 100 * dominant_threshold
    )
  } else {
    judgment <- "not_serious"
    decision <- sprintf(
      paste0("Indirect studies carry only %.0f%% of the %s (< %.0f%%), so the ",
             "body of evidence is not dominated by indirect evidence -> do ",
             "not rate down."),
      100 * share_any, basis, 100 * dominant_threshold
    )
  }

  make_domain_row(
    domain   = "Indirectness",
    judgment = judgment,
    auto     = FALSE,
    notes    = paste0(
      "Aggregated from ", n, " studies by ", basis, " share. ", counts_note,
      ". ", decision,
      if (nzchar(basis_note)) paste0(" ", basis_note) else "",
      " ", .INDIRECTNESS_THRESHOLD_CAVEAT
    )
  )
}

# --------------------------------------------------------------------------
# Subdomain (PICO) assessment — Core GRADE 5
# --------------------------------------------------------------------------

# Map a user-supplied 4-point answer to its canonical form. Accepts case and
# separator variants ("Probably No", "probably-no", "PROBABLY YES", ...) in the
# spirit of .normalize_rob_level().
.normalize_indirectness_answer <- function(x) {
  if (is.null(x)) {
    rlang::abort("indirectness_subdomains$judgment must not be NULL.")
  }
  raw <- as.character(x)
  key <- tolower(trimws(raw))
  key <- gsub("[[:space:]._-]+", "_", key)

  alias <- c(
    yes            = "yes",
    y              = "yes",
    probably_yes   = "probably_yes",
    prob_yes       = "probably_yes",
    py             = "probably_yes",
    probably_no    = "probably_no",
    prob_no        = "probably_no",
    pn             = "probably_no",
    no             = "no",
    n              = "no"
  )

  out <- unname(alias[key])
  bad <- raw[is.na(out)]
  if (length(bad) > 0) {
    rlang::abort(paste0(
      "indirectness_subdomains$judgment contains invalid value(s): ",
      paste(unique(bad), collapse = ", "),
      ". Use one of: ", paste(INDIRECTNESS_ANSWERS, collapse = ", "),
      " (aliases such as 'Probably No' are accepted)."
    ))
  }
  out
}

# Normalise the subdomain input (data.frame / tibble / list) into a tibble with
# columns subdomain, target, evidence, judgment, grade_level.
# Idempotent: a previously normalised tibble round-trips unchanged.
.normalize_indirectness_subdomains <- function(x) {
  if (is.null(x)) return(NULL)

  tbl <- if (is.data.frame(x)) {
    tibble::as_tibble(x)
  } else if (is.list(x)) {
    .subdomain_list_to_tibble(x)
  } else {
    rlang::abort(paste0(
      "indirectness_subdomains must be a data.frame, tibble, or list with ",
      "'subdomain' and 'judgment' entries."
    ))
  }

  if (nrow(tbl) == 0) {
    rlang::abort("indirectness_subdomains must contain at least one subdomain.")
  }
  missing_cols <- setdiff(c("subdomain", "judgment"), names(tbl))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0(
      "indirectness_subdomains is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Required: 'subdomain', 'judgment'; optional: 'target', 'evidence'."
    ))
  }

  sub <- trimws(as.character(tbl$subdomain))
  if (any(is.na(sub) | !nzchar(sub))) {
    rlang::abort("indirectness_subdomains$subdomain must be non-empty labels.")
  }
  dup <- unique(sub[duplicated(tolower(sub))])
  if (length(dup) > 0) {
    rlang::abort(paste0(
      "indirectness_subdomains$subdomain contains duplicate label(s): ",
      paste(dup, collapse = ", "),
      ". Each subdomain must appear exactly once."
    ))
  }

  judgment <- .normalize_indirectness_answer(tbl$judgment)

  opt_col <- function(nm) {
    if (!nm %in% names(tbl)) return(rep(NA_character_, nrow(tbl)))
    v <- as.character(tbl[[nm]])
    v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
    v
  }

  tibble::tibble(
    subdomain   = sub,
    target      = opt_col("target"),
    evidence    = opt_col("evidence"),
    judgment    = judgment,
    grade_level = unname(INDIRECTNESS_ANSWER_TO_GRADE[judgment])
  )
}

# Accept both list-of-rows (list(list(subdomain = , judgment = ), ...)) and
# list-of-columns (list(subdomain = c(...), judgment = c(...))).
.subdomain_list_to_tibble <- function(x) {
  is_row <- length(x) > 0 &&
    all(vapply(x, function(e) is.list(e) || (!is.null(names(e)) &&
                                             "judgment" %in% names(e)),
               logical(1)))
  if (is_row) {
    rows <- lapply(x, function(e) {
      e <- as.list(e)
      if (is.null(e$subdomain) || is.null(e$judgment)) {
        rlang::abort(paste0(
          "Each indirectness_subdomains entry must have 'subdomain' and ",
          "'judgment' elements."
        ))
      }
      tibble::tibble(
        subdomain = as.character(e$subdomain)[1],
        target    = if (is.null(e$target))   NA_character_ else as.character(e$target)[1],
        evidence  = if (is.null(e$evidence)) NA_character_ else as.character(e$evidence)[1],
        judgment  = as.character(e$judgment)[1]
      )
    })
    return(dplyr::bind_rows(rows))
  }

  if (!all(c("subdomain", "judgment") %in% names(x))) {
    rlang::abort(paste0(
      "indirectness_subdomains given as a list must either be a list of ",
      "per-subdomain lists or a named list of columns including 'subdomain' ",
      "and 'judgment'."
    ))
  }
  lens <- vapply(x, length, integer(1))
  if (length(unique(lens)) != 1L) {
    rlang::abort("indirectness_subdomains columns must all have the same length.")
  }
  tibble::as_tibble(lapply(x, as.character))
}

# Worst case across subdomains (deliberately NOT the weight-share rule used for
# per-study vectors: subdomains are facets of a single judgment).
.indirectness_worst_case <- function(sub_tbl) {
  # Severity order is the downgrade itself, so the four levels stay ranked
  # without a second table that could fall out of step with GRADE_DOWNGRADE.
  levels_here <- .normalize_grade_level(sub_tbl$grade_level)
  levels_here[which.min(.grade_level_downgrade(levels_here))]
}

# Compact one-line summary of the subdomain judgments for the domain notes.
# Kept free of " | " so .first_sentence() (evidence_profile footnotes) keeps
# the whole summary.
.indirectness_subdomain_notes <- function(sub_tbl, worst) {
  parts <- paste0(sub_tbl$subdomain, ": ",
                  tolower(unname(INDIRECTNESS_ANSWER_LABELS[sub_tbl$judgment])))
  paste0("Subdomains - ", paste(parts, collapse = "; "),
         ". Overall (worst case): ", .indirectness_level_label(worst),
         " indirectness.")
}

# Core GRADE 5 subdomain assessment, with optional scalar override.
.assess_indirectness_subdomains <- function(subdomains, indirectness = NULL,
                                            rationale = NULL) {
  sub_tbl <- .normalize_indirectness_subdomains(subdomains)
  worst   <- .indirectness_worst_case(sub_tbl)
  notes   <- .indirectness_subdomain_notes(sub_tbl, worst)

  if (is.null(indirectness)) {
    return(make_domain_row(
      domain   = "Indirectness",
      judgment = worst,
      auto     = FALSE,
      notes    = notes
    ))
  }

  # Only a scalar GRADE level may accompany the subdomain table: per-study
  # vectors and column names would make the source of the judgment ambiguous.
  if (length(indirectness) != 1L || !is.character(indirectness)) {
    rlang::abort(paste0(
      "indirectness_subdomains cannot be combined with a per-study ",
      "indirectness vector. Supply either the subdomain table or the ",
      "per-study input, and (optionally) a scalar GRADE level to override ",
      "the worst-case subdomain judgment."
    ))
  }
  .check_grade_level_input(indirectness, "indirectness")
  if (!indirectness %in% c(GRADE_LEVELS, names(GRADE_LEVEL_ALIASES))) {
    rlang::abort(paste0(
      "indirectness = '", indirectness, "' is not a GRADE level. When ",
      "indirectness_subdomains is supplied, column-name input is not allowed; ",
      "only a scalar GRADE level may override the worst-case subdomain ",
      "judgment."
    ))
  }

  ind_norm <- .normalize_grade_level(indirectness)
  if (identical(ind_norm, worst)) {
    # Harmless restatement of the default: no rationale required.
    return(make_domain_row(
      domain    = "Indirectness",
      judgment  = worst,
      auto      = FALSE,
      notes     = notes,
      rationale = rationale
    ))
  }

  # The hint matters for programmatic callers (do.call(), Shiny) that pass every
  # argument: forwarding a default "not_serious" alongside a subdomain table reads as an
  # override here, and omitting the argument (or passing NULL) is the fix.
  .check_override_rationale(
    rationale, "indirectness_rationale", "Indirectness",
    hint = paste0(
      "If no override was intended, omit `indirectness` or pass ",
      "`indirectness = NULL` so the worst-case subdomain judgment (",
      .indirectness_level_label(worst), " indirectness) is used."
    )
  )
  make_domain_row(
    domain    = "Indirectness",
    judgment  = ind_norm,
    auto      = FALSE,
    notes     = paste0(notes, " Worst-case default (",
                       .indirectness_level_label(worst),
                       " indirectness) replaced by user judgment."),
    rationale = rationale
  )
}
