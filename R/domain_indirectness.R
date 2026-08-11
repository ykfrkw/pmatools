# domain_indirectness.R — 非直接性ドメイン処理
#
# BMJ 2025 Core GRADE 5: 非直接性は臨床的判断が必要なため手動入力。
# domain_rob.R と同じ入力パターンをサポート。
#
# Inputs (mutually exclusive with the subdomain table):
#   (a) scalar GRADE level ("no" / "some_concerns" / "serious")
#   (b) length-k vector of per-study GRADE levels (worst case aggregation)
#   (c) column name in meta_obj$data (expanded to (b))
#   (d) `indirectness_subdomains`: a PICO subdomain table (Core GRADE 5)
#
# (d) Subdomain judgments (Core GRADE 5, BMJ publication format)
# ---------------------------------------------------------------------------
# Each subdomain (typically Population / Intervention / Comparison / Outcome,
# but any set is accepted) answers "Is the evidence sufficiently direct?" on a
# 4-point scale. Each answer maps to a GRADE level:
#
#   answer          | meaning                          | GRADE level
#   ----------------|----------------------------------|---------------
#   yes             | directly applicable              | no
#   probably_yes    | minor, unimportant differences   | no
#   probably_no     | important differences            | some_concerns  (-1)
#   no              | evidence is not applicable       | serious        (-2)
#
# The overall domain judgment defaults to the WORST case across subdomains
# (same principle as .aggregate_indirectness() for per-study vectors). Because
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

# Answer -> GRADE level contribution (see the table above)
INDIRECTNESS_ANSWER_TO_GRADE <- c(
  yes          = "no",
  probably_yes = "no",
  probably_no  = "some_concerns",
  no           = "serious"
)

# Human-readable answer labels (table rendering / domain notes)
INDIRECTNESS_ANSWER_LABELS <- c(
  yes          = "Yes",
  probably_yes = "Probably yes",
  probably_no  = "Probably no",
  no           = "No"
)

# Overall-judgment labels for the subdomain table footer row
INDIRECTNESS_OVERALL_LABELS <- c(
  no            = "No serious indirectness\n(no rating down)",
  some_concerns = "Serious indirectness\n(rate down 1 level)",
  serious       = "Very serious indirectness\n(rate down 2 levels)"
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
#' @return A one-row tibble from \code{make_domain_row()}.
#' @keywords internal
#' @noRd
assess_indirectness <- function(indirectness, meta_obj, rationale = NULL,
                                subdomains = NULL) {
  # Subdomain table takes over the whole domain (Core GRADE 5 style).
  if (!is.null(subdomains)) {
    return(.assess_indirectness_subdomains(subdomains, indirectness, rationale))
  }

  k <- meta_obj$k

  # デフォルト: スカラ "no"
  if (is.null(indirectness)) indirectness <- "no"

  # 列名参照
  if (length(indirectness) == 1 && is.character(indirectness) &&
      !indirectness %in% GRADE_LEVELS) {
    col <- indirectness
    data <- meta_obj$data
    if (is.null(data) || !col %in% names(data)) {
      rlang::abort(paste0(
        "indirectness = '", col, "' is not a valid GRADE level and was not found ",
        "as a column in the meta object's data."
      ))
    }
    ind_vec <- as.character(data[[col]])
    return(.aggregate_indirectness(ind_vec))
  }

  # スカラ
  # v0.4.0 (breaking): any scalar other than the default "no" is a manual
  # override and requires indirectness_rationale. "no" (= no downgrade, the
  # documented default) stays exempt so default calls keep working.
  if (length(indirectness) == 1) {
    validate_grade_level(indirectness, "indirectness")
    ind_norm <- .normalize_grade_level(indirectness)
    if (!identical(ind_norm, "no")) {
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
    return(.aggregate_indirectness(indirectness))
  }

  rlang::abort(paste0(
    "indirectness must be a scalar GRADE level, a column name, ",
    "or a vector of length k (", k, "). Got length ", length(indirectness), "."
  ))
}

.aggregate_indirectness <- function(ind_vec) {
  validate_grade_level(ind_vec, "indirectness")
  ind_vec <- .normalize_grade_level(ind_vec)
  order_map <- c(no = 1, some_concerns = 2, serious = 3)
  worst <- names(which.max(order_map[ind_vec]))
  n <- length(ind_vec)
  tbl <- table(ind_vec)
  notes <- paste(
    paste0(names(tbl), ": n=", as.integer(tbl)), collapse = "; "
  )
  make_domain_row(
    domain   = "Indirectness",
    judgment = worst,
    auto     = FALSE,
    notes    = paste0("Aggregated from ", n, " studies (worst case). ", notes)
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

# Worst case across subdomains (same principle as .aggregate_indirectness()).
.indirectness_worst_case <- function(sub_tbl) {
  order_map <- c(no = 1, some_concerns = 2, serious = 3)
  names(which.max(order_map[sub_tbl$grade_level]))
}

# Compact one-line summary of the subdomain judgments for the domain notes.
# Kept free of " | " so .first_sentence() (evidence_profile footnotes) keeps
# the whole summary.
.indirectness_subdomain_notes <- function(sub_tbl, worst) {
  parts <- paste0(sub_tbl$subdomain, ": ",
                  tolower(unname(INDIRECTNESS_ANSWER_LABELS[sub_tbl$judgment])))
  paste0("Subdomains - ", paste(parts, collapse = "; "),
         ". Overall (worst case): ", gsub("_", " ", worst), ".")
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
  if (!indirectness %in% c("no", "some", "some_concerns", "serious",
                           "very_serious")) {
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
  # argument: forwarding a default "no" alongside a subdomain table reads as an
  # override here, and omitting the argument (or passing NULL) is the fix.
  .check_override_rationale(
    rationale, "indirectness_rationale", "Indirectness",
    hint = paste0(
      "If no override was intended, omit `indirectness` or pass ",
      "`indirectness = NULL` so the worst-case subdomain judgment (",
      gsub("_", " ", worst), ") is used."
    )
  )
  make_domain_row(
    domain    = "Indirectness",
    judgment  = ind_norm,
    auto      = FALSE,
    notes     = paste0(notes, " Worst-case default (", gsub("_", " ", worst),
                       ") replaced by user judgment."),
    rationale = rationale
  )
}
