# multi_outcome.R - One session, many outcomes (ingest -> MA -> GRADE -> table)
#
# The single-outcome pipeline stays exactly as it is: run_ma() still refuses
# data holding more than one outcome, and grade_meta() still rates one meta
# object. This file is the orchestration layer above them:
#
#   run_ma_multi()      one run_ma() per outcome (per-outcome sm / outcome_type)
#   grade_meta_multi()  one grade_meta() per outcome -> `pmatools_set`
#   pmatools_set        ordered container: outcomes / order / primary / data
#   reorder_outcomes()  row order of the SoF table and of the export sub-dirs
#   set_primary()       which outcomes get the "Primary outcome" group header
#
# Failure policy: a single outcome that cannot be fitted or rated must not take
# the whole session down, so failures are recorded as NULL and warned about.
# The one exception is the Core GRADE 2 entry gate (threshold_type = "mid"
# without a MID): silently turning that abort into a warning would let a batch
# run bypass the gate the single-outcome path enforces, so it is re-raised.

# Arguments accepted by grade_meta_multi() that are *not* grade_meta()
# arguments: they describe how the outcome is presented, and are stored on the
# resulting object for grade_table(style = "bmj").
PMATOOLS_DISPLAY_ARGS <- c("follow_up", "unit")

# --------------------------------------------------------------------------
# Meta-analysis, one per outcome
# --------------------------------------------------------------------------

#' Run one meta-analysis per outcome
#'
#' @description
#' Splits canonical long-format data on its `outcome` column and calls
#' \code{\link{run_ma}} once per outcome. \code{run_ma()} itself keeps
#' refusing multi-outcome data; this function is the supported way to analyse
#' several outcomes in one session.
#'
#' An outcome whose meta-analysis fails (for example a single study on a
#' continuous outcome) is recorded as \code{NULL} and warned about, so the
#' remaining outcomes still complete.
#'
#' @param data Canonical long-format data from \code{\link{ingest_data}},
#'   containing an `outcome` column.
#' @param outcomes Character vector of outcomes to analyse, or \code{NULL}
#'   (default) for every unique value of `data$outcome`. Names not present in
#'   the data abort.
#' @param sm Effect measure: a single value applied to every outcome, or a
#'   named vector/list keyed by outcome name. Outcomes not named fall back to
#'   \code{run_ma()}'s default.
#' @param outcome_type \code{"binary"} / \code{"continuous"}, with the same
#'   named-vector convention as `sm`. \code{NULL} (default) detects the type
#'   per outcome from the columns that are populated.
#' @param ... Further arguments passed to \code{\link{run_ma}} for every
#'   outcome (for example `method.tau`, `random`, `incr`).
#'
#' @return A named list of `meta` objects (\code{NULL} for outcomes that
#'   failed), carrying the source data as an attribute so
#'   \code{\link{grade_meta_multi}} can pass it on to the resulting set.
#'
#' @examples
#' \dontrun{
#' d  <- ingest_data("outcomes_long.csv", format = "long")
#' ml <- run_ma_multi(d, sm = list("Mortality" = "RR", "Depression" = "SMD"))
#' }
#'
#' @export
run_ma_multi <- function(data,
                         outcomes     = NULL,
                         sm           = NULL,
                         outcome_type = NULL,
                         ...) {
  if (!is.data.frame(data)) {
    rlang::abort("run_ma_multi: 'data' must be a data.frame in canonical long format.")
  }
  if (!"outcome" %in% names(data)) {
    rlang::abort(paste0(
      "run_ma_multi: 'data' has no 'outcome' column. Add one (one row per ",
      "study x outcome x arm) and re-run ingest_data(), or call run_ma() ",
      "directly for a single-outcome analysis."
    ))
  }

  available <- unique(as.character(data$outcome))
  available <- available[!is.na(available) & nzchar(available)]
  if (length(available) == 0L) {
    rlang::abort("run_ma_multi: the 'outcome' column has no usable values.")
  }

  outcomes <- .check_outcome_selection(outcomes, available, "run_ma_multi")

  dots <- list(...)
  out  <- vector("list", length(outcomes))
  names(out) <- outcomes

  for (nm in outcomes) {
    sub <- data[!is.na(data$outcome) & as.character(data$outcome) == nm, ,
                drop = FALSE]
    # `out[nm] <- list(...)`, not `out[[nm]] <- ...`: assigning NULL with [[<-
    # would drop the element instead of recording the failure.
    out[nm] <- list(tryCatch({
      ot <- .per_outcome_arg(outcome_type, nm) %||% .detect_outcome_type(sub, nm)
      args <- c(list(data = sub, outcome_type = ot), dots)
      sm_nm <- .per_outcome_arg(sm, nm)
      if (!is.null(sm_nm)) args$sm <- sm_nm
      do.call(run_ma, args)
    }, error = function(e) {
      rlang::warn(sprintf(
        paste0("run_ma() failed for outcome '%s': %s The outcome is recorded ",
               "as NULL and the remaining outcomes continue."),
        nm, conditionMessage(e)
      ))
      NULL
    }))
  }

  attr(out, "pmatools_data") <- data
  attr(out, "pmatools_ma_args") <- list(
    outcomes     = outcomes,
    sm           = sm,
    outcome_type = outcome_type,
    dots         = dots
  )
  out
}

# Which run_ma() branch does this outcome's data support? Binary needs
# `event`, continuous needs `mean`/`sd`; an outcome carrying both is
# ambiguous and has to be resolved by the caller.
.detect_outcome_type <- function(sub, nm = "this outcome") {
  has_bin  <- "event" %in% names(sub) && any(!is.na(sub$event))
  has_cont <- all(c("mean", "sd") %in% names(sub)) &&
    any(!is.na(sub$mean)) && any(!is.na(sub$sd))

  if (has_bin && !has_cont)  return("binary")
  if (has_cont && !has_bin)  return("continuous")
  if (has_bin && has_cont) {
    rlang::abort(sprintf(paste0(
      "Outcome '%s' has both event counts and means/SDs, so its type is ",
      "ambiguous. Pass outcome_type = c('%s' = 'binary') (or 'continuous')."),
      nm, nm))
  }
  rlang::abort(sprintf(paste0(
    "Outcome '%s' has neither event counts nor means/SDs; nothing to pool."),
    nm))
}

# Shared validation of an outcome subset request.
.check_outcome_selection <- function(outcomes, available, fn) {
  if (is.null(outcomes)) return(available)
  if (!is.character(outcomes) || anyNA(outcomes) || !all(nzchar(outcomes))) {
    rlang::abort(sprintf(
      "%s: 'outcomes' must be a character vector of outcome names.", fn))
  }
  if (anyDuplicated(outcomes)) {
    rlang::abort(sprintf("%s: 'outcomes' contains duplicates: %s.", fn,
                         paste(unique(outcomes[duplicated(outcomes)]),
                               collapse = ", ")))
  }
  unknown <- setdiff(outcomes, available)
  if (length(unknown) > 0) {
    rlang::abort(sprintf(
      "%s: outcome(s) not found in the data: %s. Available: %s.",
      fn, paste(unknown, collapse = ", "), paste(available, collapse = ", ")))
  }
  outcomes
}

# --------------------------------------------------------------------------
# GRADE certainty, one per outcome
# --------------------------------------------------------------------------

#' Rate certainty for several outcomes at once
#'
#' @description
#' Calls \code{\link{grade_meta}} once per element of `ma_list` and collects
#' the results into a \code{pmatools_set}. Arguments in `common` apply to every
#' outcome; `per_outcome` supplies (and overrides) arguments for one outcome.
#'
#' An outcome that fails to rate is recorded as \code{NULL} with a warning, so
#' the rest of the batch completes. The Core GRADE 2 entry gate is the
#' exception: an abort caused by \code{threshold_type = "mid"} without a
#' threshold is re-raised unchanged, because a batch run must not become a way
#' around it.
#'
#' @param ma_list Named list of `meta` objects, typically from
#'   \code{\link{run_ma_multi}}. \code{NULL} elements (failed analyses) are
#'   dropped.
#' @param common Named list of \code{\link{grade_meta}} arguments applied to
#'   every outcome.
#' @param per_outcome Named list keyed by outcome name; each element is a named
#'   list of \code{\link{grade_meta}} arguments that override `common` for that
#'   outcome.
#' @param data Optional canonical long-format data stored on the set (used by
#'   \code{\link{export_bundle}}). Defaults to the data recorded by
#'   \code{run_ma_multi()}.
#' @param primary Character vector of primary outcome names. Default none.
#'
#' @details
#' `common` and `per_outcome` also accept two presentation arguments that
#' \code{grade_meta()} itself does not take: `follow_up` and `unit`. They are
#' stored on the rated object and picked up by
#' \code{grade_table(style = "bmj")}.
#'
#' @return A `pmatools_set`.
#'
#' @examples
#' \dontrun{
#' set <- grade_meta_multi(
#'   ml,
#'   common = list(study_design = "RCT", threshold_type = "null"),
#'   per_outcome = list("Mortality" = list(threshold = 1.25,
#'                                         threshold_type = "mid",
#'                                         threshold_scale = "ratio"))
#' )
#' }
#'
#' @export
grade_meta_multi <- function(ma_list,
                             common      = list(),
                             per_outcome = list(),
                             data        = NULL,
                             primary     = NULL) {
  if (!is.list(ma_list) || length(ma_list) == 0L) {
    rlang::abort("grade_meta_multi: 'ma_list' must be a non-empty named list.")
  }
  nms <- names(ma_list)
  if (is.null(nms) || any(!nzchar(nms))) {
    rlang::abort(paste0(
      "grade_meta_multi: every element of 'ma_list' must be named with its ",
      "outcome name (run_ma_multi() does this for you)."
    ))
  }
  if (!is.list(common)) {
    rlang::abort("grade_meta_multi: 'common' must be a named list of grade_meta() arguments.")
  }
  if (!is.list(per_outcome)) {
    rlang::abort("grade_meta_multi: 'per_outcome' must be a named list keyed by outcome name.")
  }
  if (length(per_outcome) > 0) {
    po_nms <- names(per_outcome)
    if (is.null(po_nms) || any(!nzchar(po_nms))) {
      rlang::abort("grade_meta_multi: 'per_outcome' must be keyed by outcome name.")
    }
    unknown <- setdiff(po_nms, nms)
    if (length(unknown) > 0) {
      rlang::abort(sprintf(
        "grade_meta_multi: per_outcome names not found in ma_list: %s. Known: %s.",
        paste(unknown, collapse = ", "), paste(nms, collapse = ", ")))
    }
  }

  data <- data %||% attr(ma_list, "pmatools_data")

  rated      <- list()
  used_args  <- list()
  for (nm in nms) {
    ma <- ma_list[[nm]]
    if (is.null(ma)) next   # run_ma_multi() already warned
    if (!inherits(ma, "meta")) {
      rlang::warn(sprintf(
        "grade_meta_multi: '%s' is not a meta object; skipped.", nm))
      next
    }

    args <- utils::modifyList(as.list(common),
                              as.list(per_outcome[[nm]] %||% list()))
    display <- args[intersect(names(args), PMATOOLS_DISPLAY_ARGS)]
    args[PMATOOLS_DISPLAY_ARGS] <- NULL
    if (is.null(args$outcome_name)) args$outcome_name <- nm

    g <- tryCatch(
      do.call(grade_meta, c(list(meta_obj = ma), args)),
      error = function(e) {
        # Core GRADE 2 entry gate: never downgraded to a warning.
        if (.is_threshold_gate(e)) stop(e)
        rlang::warn(sprintf(
          paste0("grade_meta() failed for outcome '%s': %s The outcome is ",
                 "recorded as NULL and the remaining outcomes continue."),
          nm, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(g)) next

    for (dn in names(display)) g[[dn]] <- display[[dn]]

    rated[[nm]]     <- g
    used_args[[nm]] <- args
  }

  if (length(rated) == 0L) {
    rlang::abort(paste0(
      "grade_meta_multi: no outcome could be rated. See the warnings above ",
      "for the per-outcome reasons."
    ))
  }

  set <- .new_pmatools_set(
    outcomes    = rated,
    order       = names(rated),
    primary     = character(0),
    data        = data,
    ma_args     = attr(ma_list, "pmatools_ma_args"),
    common      = common,
    per_outcome = per_outcome,
    grade_args  = used_args
  )
  if (!is.null(primary)) set <- set_primary(set, primary)
  set
}

# The Core GRADE 2 entry gate aborts with class "pmatools_threshold_gate"
# (see .check_threshold_type_gate()). The message test is a fallback for
# gate-like aborts raised by code paths that predate the class.
.is_threshold_gate <- function(e) {
  if (inherits(e, "pmatools_threshold_gate")) return(TRUE)
  msg <- tryCatch(conditionMessage(e), error = function(...) "")
  grepl("requires a threshold", msg, fixed = TRUE)
}

# --------------------------------------------------------------------------
# pmatools_set
# --------------------------------------------------------------------------

.new_pmatools_set <- function(outcomes,
                              order,
                              primary     = character(0),
                              data        = NULL,
                              ma_args     = NULL,
                              common      = NULL,
                              per_outcome = NULL,
                              grade_args  = NULL) {
  structure(
    list(
      outcomes    = outcomes,
      order       = order,
      primary     = primary,
      data        = data,
      # Recorded so export_bundle() can render a faithful multi-outcome
      # analysis.R; NULL for hand-built sets, which then export without one.
      ma_args     = ma_args,
      common      = common,
      per_outcome = per_outcome,
      grade_args  = grade_args
    ),
    class = "pmatools_set"
  )
}

.check_pmatools_set <- function(set, fn) {
  if (!inherits(set, "pmatools_set")) {
    rlang::abort(sprintf("%s: 'set' must be a pmatools_set from grade_meta_multi().", fn))
  }
  invisible(set)
}

#' Reorder the outcomes of a certainty set
#'
#' @description
#' The order drives everything downstream: the row order of
#' \code{\link{grade_table}} and the numbered `outcomes/NN_name/`
#' sub-directories written by \code{\link{export_bundle}}.
#'
#' @param set A `pmatools_set`.
#' @param order Character vector: every outcome of the set, exactly once.
#'
#' @return The set, reordered.
#' @export
reorder_outcomes <- function(set, order) {
  .check_pmatools_set(set, "reorder_outcomes")
  known <- names(set$outcomes)

  if (!is.character(order) || anyNA(order) || !all(nzchar(order))) {
    rlang::abort("reorder_outcomes: 'order' must be a character vector of outcome names.")
  }
  dup <- unique(order[duplicated(order)])
  if (length(dup) > 0) {
    rlang::abort(sprintf(
      "reorder_outcomes: duplicated outcome name(s) in 'order': %s.",
      paste(dup, collapse = ", ")))
  }
  unknown <- setdiff(order, known)
  if (length(unknown) > 0) {
    rlang::abort(sprintf(
      "reorder_outcomes: unknown outcome name(s): %s. The set holds: %s.",
      paste(unknown, collapse = ", "), paste(known, collapse = ", ")))
  }
  missing <- setdiff(known, order)
  if (length(missing) > 0) {
    rlang::abort(sprintf(paste0(
      "reorder_outcomes: 'order' must list every outcome exactly once; ",
      "missing: %s."), paste(missing, collapse = ", ")))
  }

  set$order <- order
  set
}

#' Mark primary outcomes of a certainty set
#'
#' @param set A `pmatools_set`.
#' @param primary Character vector of outcome names, or \code{NULL} to clear.
#'
#' @return The set, with `primary` updated.
#' @export
set_primary <- function(set, primary) {
  .check_pmatools_set(set, "set_primary")
  if (is.null(primary)) {
    set$primary <- character(0)
    return(set)
  }
  if (!is.character(primary) || anyNA(primary) || !all(nzchar(primary))) {
    rlang::abort("set_primary: 'primary' must be a character vector of outcome names.")
  }
  unknown <- setdiff(primary, names(set$outcomes))
  if (length(unknown) > 0) {
    rlang::abort(sprintf(
      "set_primary: unknown outcome name(s): %s. The set holds: %s.",
      paste(unknown, collapse = ", "), paste(names(set$outcomes), collapse = ", ")))
  }
  set$primary <- unique(primary)
  set
}

# Outcomes in display order, as a plain named list (the shape the existing
# grade_table() / grade_report() API takes).
.set_outcome_list <- function(set) {
  set$outcomes[set$order]
}

# Short analysis-set label for print()/README: the low-RoB refit changes every
# number reported for that outcome, so it is never left implicit, and it can
# differ from outcome to outcome within one set.
.analysis_set_label <- function(g) {
  # No studies, so no analysis set - and no "all studies" claim to make.
  if (.is_not_reported(g)) return("not reported")
  if (isTRUE(g$rob_refit)) {
    return(sprintf("low RoB only (%d of %d)", g$meta$k %||% NA_integer_,
                   g$meta_full$k %||% NA_integer_))
  }
  if (identical(g$rob_analysis_set, "low_only")) {
    return("all studies (low-RoB refit declined)")
  }
  "all studies"
}

#' @export
print.pmatools_set <- function(x, ...) {
  cat("\n-- Multi-outcome Certainty Set (Core GRADE series) -------\n")
  cat(sprintf(" Outcomes : %d\n", length(x$order)))
  cat(sprintf(" Primary  : %s\n",
              if (length(x$primary)) paste(x$primary, collapse = ", ") else "(none set)"))
  if (!is.null(x$data)) {
    cat(sprintf(" Data     : %d rows of long-format data\n", nrow(x$data)))
  }

  # Rated outcomes only: "not reported" is not an analysis set, so it must not
  # make a homogeneous set look as if its analysis sets differ.
  mixed <- length(unique(vapply(.rated_outcomes(x$outcomes),
                                function(g) .analysis_set_label(g),
                                character(1)))) > 1L
  cat("\n  #  Outcome / certainty / rating target / analysis set\n")
  for (i in seq_along(x$order)) {
    nm <- x$order[i]
    g  <- x$outcomes[[nm]]
    # A not-reported outcome has no certainty, no rating target and no
    # analysis set; the columns are kept so the rows still line up.
    nr  <- .is_not_reported(g)
    tgt <- if (nr || is.null(g$rating_target)) "-" else {
      lbl <- unname(RATING_TARGET_LABELS[g$rating_target])
      if (is.na(lbl)) g$rating_target else lbl
    }
    cat(sprintf(" %2d  %s%s\n", i, nm,
                if (nm %in% x$primary) "  [primary]" else ""))
    cat(sprintf("     %-10s | %-24s | %s\n",
                if (nr) "<not reported>" else g$certainty, tgt,
                if (nr) "-" else .analysis_set_label(g)))
  }
  if (mixed) {
    cat(paste0("\n Note: the analysis set differs between outcomes; pooled ",
               "numbers for a\n       low-RoB outcome come from that subset ",
               "only (Core GRADE 4 Fig 2).\n"))
  }
  cat("----------------------------------------------------------\n\n")
  invisible(x)
}

#' @export
summary.pmatools_set <- function(object, ...) {
  print(object, ...)
  for (nm in object$order) {
    g <- object$outcomes[[nm]]
    if (.is_not_reported(g)) {
      # No domain table to print: there is no body of evidence to rate.
      cat(sprintf("[%s] %s\n", nm, .not_reported_label(g)))
      if (!is.null(g$follow_up)) {
        cat(sprintf("   follow-up: %s\n", g$follow_up))
      }
      if (!is.null(g$reason)) cat(sprintf("   reason   : %s\n", g$reason))
      cat("   No included study reported this outcome; no certainty rating.\n")
      cat("\n")
      next
    }
    cat(sprintf("[%s] %s (starting: %s, design: %s)\n",
                nm, g$certainty, g$starting_quality, g$study_design))
    d <- g$domain_assessments
    for (i in seq_len(nrow(d))) {
      dg <- if (d$downgrade[i] < 0) sprintf(" [%d]", d$downgrade[i]) else "    "
      cat(sprintf("   %-20s %-14s %s\n", d$domain[i], d$judgment[i], dg))
    }
    cat("\n")
  }
  invisible(object)
}

# --------------------------------------------------------------------------
# Filesystem-safe outcome directory names
# --------------------------------------------------------------------------

# Reduce an outcome name to lower-case ASCII alphanumerics and underscores.
# Names that carry no ASCII letters or digits at all (a Japanese outcome name,
# for instance) would otherwise collapse to an empty string, so they fall back
# to `outcome_NN` using the outcome's position.
.slug <- function(x, index = NULL, max_chars = 40L) {
  s <- as.character(x)[1]
  if (is.na(s)) s <- ""
  # Transliterate what can be transliterated (accents); anything else is
  # dropped rather than turned into a filesystem-hostile byte.
  s2 <- suppressWarnings(iconv(s, to = "ASCII//TRANSLIT", sub = ""))
  if (is.na(s2)) s2 <- iconv(s, to = "ASCII", sub = "")
  if (is.na(s2)) s2 <- ""
  s2 <- tolower(s2)
  s2 <- gsub("[^a-z0-9]+", "_", s2)
  s2 <- gsub("_+", "_", s2)
  s2 <- gsub("^_|_$", "", s2)
  if (nchar(s2) > max_chars) {
    s2 <- gsub("_$", "", substr(s2, 1L, max_chars))
  }
  if (!nzchar(s2)) {
    s2 <- if (is.null(index)) "outcome" else sprintf("outcome_%02d", index)
  }
  s2
}

# `NN_slug` directory names, in set order. The numeric prefix preserves the
# order; the de-duplication suffix keeps two outcomes that slug identically
# (e.g. two different Japanese names) from colliding.
.outcome_dir_names <- function(nms) {
  slugs <- vapply(seq_along(nms), function(i) .slug(nms[i], index = i),
                  character(1))
  seen <- character(0)
  for (i in seq_along(slugs)) {
    base <- slugs[i]
    cand <- base
    j <- 1L
    while (cand %in% seen) {
      j <- j + 1L
      cand <- paste0(base, "_", j)
    }
    slugs[i] <- cand
    seen <- c(seen, cand)
  }
  sprintf("%02d_%s", seq_along(slugs), slugs)
}
