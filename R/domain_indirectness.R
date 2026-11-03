# domain_indirectness.R - the domain nothing can compute for you
#
# BMJ 2025 Core GRADE 5. Indirectness is the one domain pmatools cannot assess
# from the fit. Whether the evidence answers the question actually being asked
# is a clinical judgment about populations, doses, comparators and follow-up,
# and nothing in a {meta} object knows what question was asked of it. So the
# judgment arrives from the reviewer, and the work here is to accept it in any
# of four shapes, fold it down to the single domain row every domain returns,
# and record in the notes how that fold was made. The four shapes are
# deliberately the ones domain_rob.R accepts, so a caller learns the pattern
# once and uses it twice.
#
# Inputs (mutually exclusive with the subdomain table):
#   (a) scalar GRADE level ("not_serious" / "serious" / "very_serious" /
#       "extremely_serious")
#   (b) length-k vector of per-study GRADE levels (weight-share aggregation)
#   (c) column name in meta_obj$data (expanded to (b))
#   (d) `indirectness_subdomains`: a PICO subdomain table, each row answered
#       on the living Core GRADE Guide's Fig 8-2 (or on the legacy scale)
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
# (d) Subdomain judgments - the living Core GRADE Guide's Fig 8-2
# ---------------------------------------------------------------------------
# ATTRIBUTION. Asking the indirectness question separately for each PICO
# element is Core GRADE 5's, and since the living Core GRADE Guide (chapter 8,
# online 2025-2026) the per-element questions and their answers are the
# Guide's too: its Fig 8-2, "Decision-algorithm for assessing indirectness in
# core GRADE", is a four-branch decision tree, and the answer codes below
# encode its routes one-to-one. What remains pmatools' is (i) the TABLE that
# carries one answer per element, (ii) the worst-case fold that combines the
# four branches (the figure draws four parallel branches and does not say how
# they combine), and (iii) the legacy 4-point scale described further down.
#
# Fig 8-2, as encoded here (one answer per row encodes the whole route):
#
#   Population / Intervention / Comparison (any non-outcome label):
#     "Are there differences ... versus the target?"
#       No                                          no_difference   not_serious
#       Yes -> "Are the biological or social factors sufficiently similar ...?"
#         Yes                                       similar         not_serious
#         No, dissimilar                            dissimilar      serious
#         No, very dissimilar                       very_dissimilar very_serious
#
#   Outcome:
#     "Is the outcome a surrogate of the outcome of interest?"
#       No -> "Are the criteria used to define outcomes and time frames ...
#              similar to the target?"
#         Yes                                       similar_definition    not_serious
#         No                                        different_definition  serious
#       Yes -> "How far is the surrogate outcome placed in the putative
#               causal pathway from the patient-important endpoints?"
#         Very close (footnote *)                   surrogate_very_close  not_serious
#         Close                                     surrogate_close       serious
#         Far                                       surrogate_far         very_serious
#
# The footnote * is the Guide's: some surrogates are so directly linked to the
# disease process that they always represent a patient-important outcome.
#
# A "VERY CLOSE" SURROGATE NEEDS A WRITTEN REASON (pmatools 0.9, stricter than
# Fig 8-2). The footnote is all the Guide offers: neither it nor Core GRADE 5
# gives a single worked example of a surrogate close enough not to rate down,
# whereas every surrogate either source does name (bone density, coronary
# calcification, mitral valve area, calcium and phosphate measures) is rated
# down once or twice. With no example to measure an answer against,
# "surrogate_very_close" is the one Fig 8-2 answer that removes a rating down
# on the reviewer's word alone, so a row giving it must carry a non-empty
# `rationale` or the call aborts; the alternative offered is
# "surrogate_close". The code -> level mapping is the figure's and does not
# change. The legacy scale is unaffected: its Outcome answers never said
# whether the outcome was a surrogate. Any row MAY carry a rationale, and when
# it does the notes quote it after that row's route.
#
# A P/I/C code on the Outcome row, or an Outcome code on any other row, is a
# route the figure does not have and aborts. The element is read from the
# subdomain label (.indirectness_element()); a label that names no PICO
# element is accepted with the P/I/C codes, and its route is not drawn.
#
# The figure is inst/figures/indirect.svg (data-raw/build_figures.R), and an
# assessment from the subdomain table records the union of the element ids
# every row traversed as its "flow_path" fact, exactly like the four
# flowcharted domains. The scalar, per-study and column-name shapes record no
# flow_path: they never walk the figure.
#
# Core GRADE 5 Table 2 also grades how LIKELY each element is to warrant rating
# down (verbatim column header: "Likelihood of rating down"), asymmetrically:
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
# Rating down two levels: Core GRADE 5 (verbatim) - "Although one might
# consider rating down more than one level for indirectness for any PICO
# element, this possibility is typically more salient for surrogate outcomes".
# Fig 8-2 makes that concrete: "far" surrogates and "very dissimilar"
# populations, interventions or comparators rate down twice.
#
# THE LEGACY 4-POINT SCALE (pmatools' own, before Fig 8-2). Up to 0.5.x each
# subdomain answered "Is the evidence sufficiently direct?" on a pmatools
# scale that appears nowhere in Core GRADE. It is still accepted, with its
# levels unchanged, so stored analyses and scripts keep rating the same:
#
#   answer          | GRADE level    | Fig 8-2 route it is drawn on (P/I/C)
#   ----------------|----------------|-------------------------------------
#   yes             | not_serious    | no_difference
#   probably_yes    | not_serious    | similar
#   probably_no     | serious        | dissimilar
#   no              | very_serious   | very_dissimilar
#
# On the Outcome row a legacy answer does not say whether the outcome was a
# surrogate, so it determines no route: only the root and the surrogate
# question are lit, and the notes say the route was not determined.
#
# NOTE ON VOCABULARY. The level names are shared with Risk of bias and are Core
# GRADE's own words ("not serious; serious; very serious; or, rarely, extremely
# serious", Core GRADE 1). See R/grade_vocabulary.R for the table and for why a
# bare "serious" is refused in this release. Domain notes render the level through
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
# "No override" is `indirectness = NULL` (or the argument left out): NULL - not
# the string "no" - is what grade_meta() defaults to, so callers that always
# pass every argument do not accidentally override the subdomain worst case.
#
# ADMISSION RULE. Anything that reads or writes the indirectness judgment
# itself (answer vocabulary, fold, notes, flow_path, the very-close reason
# gate) belongs here. Rendering
# the subdomain table belongs in R/indirectness_table.R; drawing the figure in
# data-raw/build_figures.R; the Shiny tab in shiny/R/step3_grade.R.

# Fig 8-2 answer codes (living Core GRADE Guide, chapter 8).
INDIRECTNESS_PIC_ANSWERS <- c("no_difference", "similar", "dissimilar",
                              "very_dissimilar")
INDIRECTNESS_OUTCOME_ANSWERS <- c("similar_definition", "different_definition",
                                  "surrogate_very_close", "surrogate_close",
                                  "surrogate_far")
# The Fig 8-2 answers a row may give only with a written `rationale` (see
# "A VERY CLOSE SURROGATE NEEDS A WRITTEN REASON" above).
INDIRECTNESS_RATIONALE_REQUIRED <- "surrogate_very_close"
# pmatools' pre-Fig-8-2 scale; accepted on any row, levels unchanged.
INDIRECTNESS_LEGACY_ANSWERS <- c("yes", "probably_yes", "probably_no", "no")

# Every accepted answer, Fig 8-2 codes first.
INDIRECTNESS_ANSWERS <- c(INDIRECTNESS_PIC_ANSWERS,
                          INDIRECTNESS_OUTCOME_ANSWERS,
                          INDIRECTNESS_LEGACY_ANSWERS)

# Answer -> GRADE level contribution (see the trees above).
INDIRECTNESS_ANSWER_TO_GRADE <- c(
  no_difference        = "not_serious",
  similar              = "not_serious",
  dissimilar           = "serious",
  very_dissimilar      = "very_serious",
  similar_definition   = "not_serious",
  different_definition = "serious",
  surrogate_very_close = "not_serious",
  surrogate_close      = "serious",
  surrogate_far        = "very_serious",
  yes                  = "not_serious",
  probably_yes         = "not_serious",
  probably_no          = "serious",
  no                   = "very_serious"
)

# Human-readable answer labels (table rendering / domain notes). Kept free of
# " | " so .first_sentence() keeps a whole note.
INDIRECTNESS_ANSWER_LABELS <- c(
  no_difference        = "No difference from the target",
  similar              = "Differences; factors sufficiently similar",
  dissimilar           = "Differences; factors dissimilar",
  very_dissimilar      = "Differences; factors very dissimilar",
  similar_definition   = "Not a surrogate; definition and time frame similar",
  different_definition = "Not a surrogate; definition or time frame differs",
  surrogate_very_close = "Surrogate, very close to patient-important outcome",
  surrogate_close      = "Surrogate, close to patient-important outcome",
  surrogate_far        = "Surrogate, far from patient-important outcome",
  yes                  = "Yes, on the pre-Fig 8-2 scale",
  probably_yes         = "Probably yes, on the pre-Fig 8-2 scale",
  probably_no          = "Probably no, on the pre-Fig 8-2 scale",
  no                   = "No, on the pre-Fig 8-2 scale"
)

# Legacy answer -> the Fig 8-2 P/I/C code whose route it is drawn on. Level-
# preserving by construction (the test suite pins that).
INDIRECTNESS_LEGACY_TO_PIC <- c(
  yes          = "no_difference",
  probably_yes = "similar",
  probably_no  = "dissimilar",
  no           = "very_dissimilar"
)

# The domain notes used to carry their own copy of the level -> wording table.
# There is one display vocabulary for the whole package (.grade_level_wording()
# in R/grade_vocabulary.R) and this domain has no reason to be the exception.
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

  # The documented default: the scalar "not_serious", i.e. no downgrade.
  if (is.null(indirectness)) indirectness <- "not_serious"

  # (c) Column-name reference. The legacy spellings have to be recognised here
  # too, or a scalar `indirectness = "no"` is taken for a column name and aborts.
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

  # (a) Scalar GRADE level.
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

  # (b) Per-study vector: one GRADE level per study, folded by weight share.
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

# Map a user-supplied answer to its canonical code. Accepts case and
# separator variants ("Probably No", "very-dissimilar", "Surrogate far", ...)
# in the spirit of .normalize_rob_level().
.normalize_indirectness_answer <- function(x) {
  if (is.null(x)) {
    rlang::abort("indirectness_subdomains$judgment must not be NULL.")
  }
  raw <- as.character(x)
  key <- tolower(trimws(raw))
  key <- gsub("[[:space:]._-]+", "_", key)

  alias <- c(
    stats::setNames(INDIRECTNESS_ANSWERS, INDIRECTNESS_ANSWERS),
    y        = "yes",
    prob_yes = "probably_yes",
    py       = "probably_yes",
    prob_no  = "probably_no",
    pn       = "probably_no",
    n        = "no"
  )

  out <- unname(alias[key])
  bad <- raw[is.na(out)]
  if (length(bad) > 0) {
    rlang::abort(paste0(
      "indirectness_subdomains$judgment contains invalid value(s): ",
      paste(unique(bad), collapse = ", "),
      ". Use the Core GRADE Guide Fig 8-2 codes - Population / Intervention ",
      "/ Comparison: ", paste(INDIRECTNESS_PIC_ANSWERS, collapse = ", "),
      "; Outcome: ", paste(INDIRECTNESS_OUTCOME_ANSWERS, collapse = ", "),
      " - or the legacy scale: ",
      paste(INDIRECTNESS_LEGACY_ANSWERS, collapse = ", "),
      " (case and separator variants such as 'Probably No' are accepted)."
    ))
  }
  out
}

# Which Fig 8-2 branch a subdomain label names: "population",
# "intervention", "comparison" (also "comparator"), "outcome", or NA for a
# label that names no PICO element. Case-insensitive, exact after trimming.
.indirectness_element <- function(label) {
  key <- tolower(trimws(as.character(label)))
  out <- rep(NA_character_, length(key))
  out[key == "population"]                    <- "population"
  out[key == "intervention"]                  <- "intervention"
  out[key %in% c("comparison", "comparator")] <- "comparison"
  out[key == "outcome"]                       <- "outcome"
  out
}

# Refuse a route the figure does not have: an Outcome code on a non-outcome
# row, or a Population / Intervention / Comparison code on the Outcome row.
.check_indirectness_answer_element <- function(subdomain, judgment) {
  is_outcome  <- .indirectness_element(subdomain) %in% "outcome"
  bad_outcome <- is_outcome & judgment %in% INDIRECTNESS_PIC_ANSWERS
  bad_other   <- !is_outcome & judgment %in% INDIRECTNESS_OUTCOME_ANSWERS
  if (any(bad_outcome)) {
    rlang::abort(paste0(
      "indirectness_subdomains: '", judgment[bad_outcome][1], "' answers the ",
      "Population / Intervention / Comparison branch of Core GRADE Guide ",
      "Fig 8-2 and cannot be used on the '", subdomain[bad_outcome][1],
      "' row. The Outcome branch takes: ",
      paste(INDIRECTNESS_OUTCOME_ANSWERS, collapse = ", "), "."
    ))
  }
  if (any(bad_other)) {
    rlang::abort(paste0(
      "indirectness_subdomains: '", judgment[bad_other][1], "' answers the ",
      "Outcome branch of Core GRADE Guide Fig 8-2 and cannot be used on the '",
      subdomain[bad_other][1], "' row. Population / Intervention / ",
      "Comparison rows take: ",
      paste(INDIRECTNESS_PIC_ANSWERS, collapse = ", "), "."
    ))
  }
  invisible(TRUE)
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
      ". Required: 'subdomain', 'judgment'; optional: 'target', 'evidence', ",
      "'rationale'."
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
  .check_indirectness_answer_element(sub, judgment)

  opt_col <- function(nm) {
    if (!nm %in% names(tbl)) return(rep(NA_character_, nrow(tbl)))
    v <- as.character(tbl[[nm]])
    v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
    v
  }

  rationale <- opt_col("rationale")
  .check_indirectness_rationale(sub, judgment, rationale)

  tibble::tibble(
    subdomain   = sub,
    target      = opt_col("target"),
    evidence    = opt_col("evidence"),
    judgment    = judgment,
    rationale   = rationale,
    grade_level = unname(INDIRECTNESS_ANSWER_TO_GRADE[judgment])
  )
}

# Refuse "surrogate_very_close" without a written reason. Core GRADE gives no
# worked example of such a surrogate (see the header), so not rating down
# rests on the reviewer's reason alone, and the reason must be on record.
.check_indirectness_rationale <- function(subdomain, judgment, rationale) {
  is_missing <- judgment %in% INDIRECTNESS_RATIONALE_REQUIRED &
    (is.na(rationale) | !nzchar(trimws(rationale)))
  if (!any(is_missing)) return(invisible(TRUE))
  rlang::abort(paste0(
    "indirectness_subdomains: the '", subdomain[is_missing][1], "' row ",
    "answers 'surrogate_very_close' (do not rate down) but gives no ",
    "rationale. Core GRADE gives no worked example of a surrogate that is ",
    "very close to the patient-important outcome - Fig 8-2 offers only its ",
    "footnote, that some surrogates are so directly linked to the disease ",
    "process that they always represent a patient-important outcome - so ",
    "pmatools requires a written reason for not rating down. Add a non-empty ",
    "'rationale' to that row saying why this surrogate effectively is the ",
    "patient-important outcome, or answer 'surrogate_close' (rate down once)."
  ))
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
      optional <- function(value) {
        if (is.null(value)) NA_character_ else as.character(value)[1]
      }
      tibble::tibble(
        subdomain = as.character(e$subdomain)[1],
        target    = optional(e$target),
        evidence  = optional(e$evidence),
        judgment  = as.character(e$judgment)[1],
        rationale = optional(e$rationale)
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

# --------------------------------------------------------------------------
# Fig 8-2 node vocabulary (inst/figures/indirect.svg)
#
# Every id below is a <g id="..."> in that file, drawn by
# data-raw/build_figures.R. tests/testthat/test-flowchart-nodes.R asserts that
# every id here is drawn and that no emitted flow_path names an id that is not
# here, exactly as for the four other figures.
# --------------------------------------------------------------------------
.INDIRECT_FIG82_NODE_IDS <- c(
  "pma-indirect-node-root",
  # Population / Intervention / Comparison: one question each, one "no" leaf
  # each, and a "yes" edge each into the shared similarity node.
  unlist(lapply(c("population", "intervention", "comparison"), function(el) {
    c(paste0("pma-indirect-edge-root-", el),
      paste0("pma-indirect-node-", el),
      paste0("pma-indirect-edge-", el, "-no"),
      paste0("pma-indirect-leaf-", el, "-nodown"),
      paste0("pma-indirect-edge-", el, "-yes"))
  })),
  "pma-indirect-node-similarity",
  "pma-indirect-edge-similarity-yes",
  "pma-indirect-leaf-similarity-nodown",
  "pma-indirect-edge-similarity-dissimilar",
  "pma-indirect-leaf-similarity-down1",
  "pma-indirect-edge-similarity-verydissimilar",
  "pma-indirect-leaf-similarity-down2",
  # Outcome.
  "pma-indirect-edge-root-outcome",
  "pma-indirect-node-surrogate",
  "pma-indirect-edge-surrogate-no",
  "pma-indirect-node-definition",
  "pma-indirect-edge-definition-yes",
  "pma-indirect-leaf-definition-nodown",
  "pma-indirect-edge-definition-no",
  "pma-indirect-leaf-definition-down1",
  "pma-indirect-edge-surrogate-yes",
  "pma-indirect-node-pathway",
  "pma-indirect-edge-pathway-veryclose",
  "pma-indirect-leaf-pathway-nodown",
  "pma-indirect-edge-pathway-close",
  "pma-indirect-leaf-pathway-down1",
  "pma-indirect-edge-pathway-far",
  "pma-indirect-leaf-pathway-down2"
)

# The ids one row traverses. Legacy answers on a P/I/C row are drawn on the
# route INDIRECTNESS_LEGACY_TO_PIC names; on the Outcome row they stop at the
# surrogate question, because that scale never said whether the outcome was a
# surrogate. A label naming no PICO element is not drawn beyond the root.
.indirectness_row_route <- function(element, judgment) {
  root <- "pma-indirect-node-root"
  if (is.na(element)) return(root)
  if (identical(element, "outcome")) {
    head <- c(root, "pma-indirect-edge-root-outcome",
              "pma-indirect-node-surrogate")
    tail <- switch(judgment,
      similar_definition   = c("pma-indirect-edge-surrogate-no",
                               "pma-indirect-node-definition",
                               "pma-indirect-edge-definition-yes",
                               "pma-indirect-leaf-definition-nodown"),
      different_definition = c("pma-indirect-edge-surrogate-no",
                               "pma-indirect-node-definition",
                               "pma-indirect-edge-definition-no",
                               "pma-indirect-leaf-definition-down1"),
      surrogate_very_close = c("pma-indirect-edge-surrogate-yes",
                               "pma-indirect-node-pathway",
                               "pma-indirect-edge-pathway-veryclose",
                               "pma-indirect-leaf-pathway-nodown"),
      surrogate_close      = c("pma-indirect-edge-surrogate-yes",
                               "pma-indirect-node-pathway",
                               "pma-indirect-edge-pathway-close",
                               "pma-indirect-leaf-pathway-down1"),
      surrogate_far        = c("pma-indirect-edge-surrogate-yes",
                               "pma-indirect-node-pathway",
                               "pma-indirect-edge-pathway-far",
                               "pma-indirect-leaf-pathway-down2"),
      character(0))
    return(c(head, tail))
  }
  code <- if (judgment %in% INDIRECTNESS_LEGACY_ANSWERS) {
    unname(INDIRECTNESS_LEGACY_TO_PIC[judgment])
  } else {
    judgment
  }
  head <- c(root, paste0("pma-indirect-edge-root-", element),
            paste0("pma-indirect-node-", element))
  if (identical(code, "no_difference")) {
    return(c(head, paste0("pma-indirect-edge-", element, "-no"),
             paste0("pma-indirect-leaf-", element, "-nodown")))
  }
  similarity <- c(paste0("pma-indirect-edge-", element, "-yes"),
                  "pma-indirect-node-similarity")
  tail <- switch(code,
    similar         = c("pma-indirect-edge-similarity-yes",
                        "pma-indirect-leaf-similarity-nodown"),
    dissimilar      = c("pma-indirect-edge-similarity-dissimilar",
                        "pma-indirect-leaf-similarity-down1"),
    very_dissimilar = c("pma-indirect-edge-similarity-verydissimilar",
                        "pma-indirect-leaf-similarity-down2"))
  c(head, similarity, tail)
}

# Union of every row's route, in the vocabulary's order so the fact is stable
# whatever order the rows arrive in.
.indirectness_flow_path <- function(sub_tbl) {
  elements <- .indirectness_element(sub_tbl$subdomain)
  ids <- unique(unlist(Map(.indirectness_row_route, elements,
                           sub_tbl$judgment), use.names = FALSE))
  .INDIRECT_FIG82_NODE_IDS[.INDIRECT_FIG82_NODE_IDS %in% ids]
}

# Lower-case only the first letter, so "Fig" inside a label survives.
.lc_first <- function(x) {
  paste0(tolower(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

# What a level reads as on the figure's leaves.
.indirectness_leaf_words <- function(level) {
  unname(c(not_serious  = "do not rate down",
           serious      = "rate down once",
           very_serious = "rate down twice")[level])
}

# Compact one-line summary of the subdomain judgments for the domain notes:
# each row's Fig 8-2 route in plain words and the leaf it reached, then the
# row's reason when one was given. Kept free of " | " so .first_sentence()
# (evidence_profile footnotes) keeps the whole summary.
.indirectness_subdomain_notes <- function(sub_tbl, worst) {
  # A table normalised before 0.9 has no rationale column.
  row_rationale <- sub_tbl$rationale %||% rep(NA_character_, nrow(sub_tbl))
  # A reviewer's " | " would split the note at .first_sentence().
  reason <- ifelse(is.na(row_rationale), "",
                   paste0(" - reason: ", gsub(" | ", " / ",
                                              trimws(row_rationale),
                                              fixed = TRUE)))
  parts <- paste0(
    sub_tbl$subdomain, ": ",
    .lc_first(unname(INDIRECTNESS_ANSWER_LABELS[sub_tbl$judgment])),
    " (", .indirectness_leaf_words(sub_tbl$grade_level), ")", reason)
  elements <- .indirectness_element(sub_tbl$subdomain)
  legacy_outcome <- elements %in% "outcome" &
    sub_tbl$judgment %in% INDIRECTNESS_LEGACY_ANSWERS
  unplaced <- is.na(elements)
  extra <- c(
    if (any(legacy_outcome)) paste0(
      "The Outcome route through Core GRADE Guide Fig 8-2 was not ",
      "determined, because the answer used the pre-Fig-8-2 scale, which does ",
      "not say whether the outcome is a surrogate."),
    if (any(unplaced)) paste0(
      "Not drawn on Fig 8-2 (no PICO element named): ",
      paste(sub_tbl$subdomain[unplaced], collapse = ", "), ".")
  )
  paste0("Subdomains (Core GRADE Guide Fig 8-2) - ",
         paste(parts, collapse = "; "),
         ". Overall (worst case): ", .indirectness_level_label(worst),
         " indirectness.",
         if (length(extra)) paste0(" ", paste(extra, collapse = " ")) else "")
}

# Fig 8-2 subdomain assessment, with optional scalar override.
.assess_indirectness_subdomains <- function(subdomains, indirectness = NULL,
                                            rationale = NULL) {
  sub_tbl <- .normalize_indirectness_subdomains(subdomains)
  worst   <- .indirectness_worst_case(sub_tbl)
  notes   <- .indirectness_subdomain_notes(sub_tbl, worst)
  # The route is recorded on an override too: the subdomain answers still
  # walked the figure, and the override is stated in the notes beside it.
  facts   <- .facts(.flow_path_fact(.indirectness_flow_path(sub_tbl)))

  if (is.null(indirectness)) {
    return(make_domain_row(
      domain   = "Indirectness",
      judgment = worst,
      auto     = FALSE,
      notes    = notes,
      facts    = facts
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
      rationale = rationale,
      facts     = facts
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
    rationale = rationale,
    facts     = facts
  )
}
