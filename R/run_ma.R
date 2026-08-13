# run_ma.R - Pairwise meta-analysis wrapper around {meta}
#
# Accepts a canonical long-format tibble (from ingest_data) and dispatches to
# meta::metabin (binary) or meta::metacont (continuous) with sensible defaults.

#' Run a pairwise meta-analysis
#'
#' @param data Canonical long-format data, typically from
#'   \code{\link{ingest_data}}: one row per study x arm with columns
#'   `studlab`, `treat`, `n`, plus `event` (binary) or `mean`/`sd` (continuous).
#'   An `outcome` column may name a different measurement scale per study --
#'   several scales pool fine under `sm = "SMD"` -- but no single `studlab` may
#'   carry more than one outcome, which would double-count that study.
#' @param outcome_type One of `"binary"` or `"continuous"`.
#' @param sm Effect measure. Binary: `"OR"` or `"RR"`. Continuous: `"SMD"`,
#'   `"MD"`, or `"RoM"`. Defaults: `"OR"` for binary, `"SMD"` for continuous.
#' @param method Pooling method (binary only). One of `"Inverse"`, `"MH"`,
#'   `"Peto"`. Defaults: `"Inverse"` for OR, `"MH"` for RR.
#' @param method.tau Heterogeneity estimator. One of `"REML"` (default),
#'   `"PM"`, `"DL"`, `"SJ"`, `"ML"` or `"EB"`.
#' @param random,common Logical. Use random-effects (default `TRUE`) and/or
#'   common-effects (default `FALSE`) pooling.
#' @param hakn Hartung-Knapp-Sidik-Jonkman adjustment. NULL (default) uses
#'   `TRUE` if k >= 3, else `FALSE`. `TRUE` and `FALSE` are honoured as given;
#'   `TRUE` below k = 3 warns, because the interval it produces is very wide.
#' @param prediction Compute 95 percent prediction interval. NULL (default) uses
#'   `TRUE` if k >= 3, else `FALSE`.
#' @param incr Continuity correction for zero events (binary). Default 0.5.
#' @param subgroup Optional column name in `data` for subgroup analysis.
#' @param experimental_label,control_label Values of `treat` identifying the
#'   intervention (experimental) and comparator (control) arms. Supplied values
#'   must exist in `data$treat` and be distinct. If NULL, the function looks
#'   for `"experimental"`/`"control"` first; with exactly two other treat
#'   values it falls back to alphabetical assignment (first = control,
#'   second = experimental) and emits a warning, because the effect direction
#'   depends on this choice.
#'
#' @return An object of class `meta` (from the \{meta\} package).
#'
#' @export
run_ma <- function(data,
                   outcome_type = c("binary", "continuous"),
                   sm           = NULL,
                   method       = NULL,
                   method.tau   = c("REML", "PM", "DL", "SJ", "ML", "EB"),
                   random       = TRUE,
                   common       = FALSE,
                   hakn         = NULL,
                   prediction   = NULL,
                   incr         = 0.5,
                   subgroup     = NULL,
                   experimental_label = NULL,
                   control_label      = NULL) {
  outcome_type <- match.arg(outcome_type)
  method.tau   <- match.arg(method.tau)

  # A continuous review whose studies each measured depression on a different
  # scale carries one outcome label per study, and pooling all of them under
  # SMD is the whole point of the measure. What cannot be pooled is the same
  # study appearing under two outcomes: that study would be counted twice.
  .abort_on_multi_outcome_studies(data)

  if (is.null(sm)) {
    sm <- if (outcome_type == "binary") "OR" else "SMD"
  }

  # Validate sm
  valid_sm <- if (outcome_type == "binary") c("OR", "RR") else c("SMD", "MD", "RoM")
  if (!sm %in% valid_sm) {
    rlang::abort(sprintf(
      "sm = '%s' is not valid for outcome_type = '%s'. Use one of: %s",
      sm, outcome_type, paste(valid_sm, collapse = ", ")
    ))
  }

  # Pivot long -> wide for {meta}
  wide <- .long_to_wide(data, experimental_label, control_label)

  k <- nrow(wide)
  if (k < 1) {
    rlang::abort("run_ma: at least one study is required.")
  }

  # Auto-defaults
  hakn_forced_on <- isTRUE(hakn)
  if (is.null(hakn))       hakn       <- (k >= 3 && random)
  if (is.null(prediction)) prediction <- (k >= 3 && random)
  # The caller overrode the k >= 3 rule, so the request is honoured -- but the
  # Hartung-Knapp t quantile below three studies is large enough to make the
  # interval close to uninformative, and that is worth saying out loud.
  if (hakn_forced_on && k < 3) {
    rlang::warn(sprintf(
      paste0(
        "hakn = TRUE was requested with k = %d. The Hartung-Knapp confidence ",
        "interval is very wide with fewer than three studies; it has been ",
        "applied as asked."
      ),
      k
    ))
  }

  # Subgroup vector
  subgroup_vec <- NULL
  if (!is.null(subgroup)) {
    if (!subgroup %in% names(data)) {
      rlang::abort(sprintf("Subgroup column '%s' not found in data.", subgroup))
    }
    # subgroup is per-study; pick from experimental rows of original data
    sub_lookup <- data[, c("studlab", subgroup), drop = FALSE]
    sub_lookup <- sub_lookup[!duplicated(sub_lookup$studlab), ]
    subgroup_vec <- sub_lookup[[subgroup]][match(wide$studlab, sub_lookup$studlab)]
  }

  if (outcome_type == "binary") {
    if (is.null(method)) {
      method <- if (sm == "OR") "Inverse" else "MH"
    }
    if (!method %in% c("Inverse", "MH", "Peto")) {
      rlang::abort("method must be one of 'Inverse', 'MH', 'Peto'.")
    }
    args <- list(
      event.e = wide$event_e,
      n.e     = wide$n_e,
      event.c = wide$event_c,
      n.c     = wide$n_c,
      studlab = wide$studlab,
      sm         = sm,
      method     = method,
      method.tau = method.tau,
      random     = random,
      common     = common,
      method.random.ci = if (isTRUE(hakn)) "HK" else "classic",
      prediction = prediction,
      incr       = incr
    )
    if (!is.null(subgroup_vec)) args$subgroup <- subgroup_vec
    return(do.call(meta::metabin, args))
  }

  # Continuous
  args <- list(
    n.e     = wide$n_e,
    mean.e  = wide$mean_e,
    sd.e    = wide$sd_e,
    n.c     = wide$n_c,
    mean.c  = wide$mean_c,
    sd.c    = wide$sd_c,
    studlab = wide$studlab,
    sm         = sm,
    method.tau = method.tau,
    random     = random,
    common     = common,
    method.random.ci = if (isTRUE(hakn)) "HK" else "classic",
    prediction = prediction
  )
  if (!is.null(subgroup_vec)) args$subgroup <- subgroup_vec
  do.call(meta::metacont, args)
}

# --------------------------------------------------------------------------
# One outcome per study
# --------------------------------------------------------------------------
# Naming several outcomes across the data set is legitimate (one measurement
# scale per study, pooled as SMD); naming several for one study is not.
# .long_to_wide()'s "exactly one intervention and one control arm" check would
# also stop it, but only with a message about arms, which is not the problem.
.abort_on_multi_outcome_studies <- function(data) {
  if (!all(c("outcome", "studlab") %in% names(data))) return(invisible(NULL))

  outcome_labels <- as.character(data$outcome)
  named <- !is.na(outcome_labels) & nzchar(outcome_labels)
  if (!any(named)) return(invisible(NULL))

  outcomes_per_study <- tapply(
    outcome_labels[named],
    as.character(data$studlab)[named],
    function(labels) length(unique(labels))
  )
  offenders <- names(outcomes_per_study)[outcomes_per_study > 1]
  if (length(offenders) == 0) return(invisible(NULL))

  max_listed <- 5L
  listed <- paste(utils::head(offenders, max_listed), collapse = ", ")
  if (length(offenders) > max_listed) {
    listed <- sprintf("%s, and %d more", listed,
                      length(offenders) - max_listed)
  }
  rlang::abort(sprintf(
    paste0(
      "run_ma() received more than one outcome for the same study (%s). ",
      "Each study may contribute only once, so reduce the data to one ",
      "outcome per study before calling run_ma()."
    ),
    listed
  ))
}

# --------------------------------------------------------------------------
# Which random-effects CI a fitted object actually used
# --------------------------------------------------------------------------
# {meta} keeps `hakn` as a legacy alias of `method.random.ci`, so the field the
# pooling is really controlled by is the one to read.
.uses_hartung_knapp <- function(meta_obj) {
  ci <- meta_obj$method.random.ci
  if (is.null(ci) || length(ci) == 0) return(isTRUE(meta_obj$hakn))
  identical(as.character(ci)[1], "HK")
}

# --------------------------------------------------------------------------
# Long -> wide pivot for {meta} input
# --------------------------------------------------------------------------
.long_to_wide <- function(data, experimental_label = NULL, control_label = NULL) {
  if (!all(c("studlab", "treat", "n") %in% names(data))) {
    rlang::abort("data must have columns 'studlab', 'treat', 'n'.")
  }

  treats <- unique(data$treat)

  # Validate user-supplied arm labels (exist in data, distinct)
  for (lbl in list(list(experimental_label, "experimental_label"),
                   list(control_label,      "control_label"))) {
    if (!is.null(lbl[[1]]) && !lbl[[1]] %in% treats) {
      rlang::abort(sprintf(
        "%s = '%s' not found in treat values: %s.",
        lbl[[2]], lbl[[1]], paste(treats, collapse = ", ")
      ))
    }
  }
  if (!is.null(experimental_label) && !is.null(control_label) &&
      identical(experimental_label, control_label)) {
    rlang::abort(
      "experimental_label and control_label must be distinct treat values."
    )
  }

  # If only one label is supplied and there are exactly 2 groups, infer the other
  if (xor(is.null(experimental_label), is.null(control_label)) &&
      length(treats) == 2) {
    supplied <- experimental_label %||% control_label
    other    <- setdiff(treats, supplied)
    if (is.null(experimental_label)) experimental_label <- other
    if (is.null(control_label))      control_label      <- other
  }

  if (is.null(experimental_label) || is.null(control_label)) {
    if (all(c("experimental", "control") %in% treats)) {
      experimental_label <- "experimental"
      control_label      <- "control"
    } else if (length(treats) == 2) {
      # Pick the alphabetically larger as experimental, smaller as control
      sorted <- sort(treats)
      control_label      <- sorted[1]
      experimental_label <- sorted[2]
      rlang::warn(sprintf(
        paste0(
          "Treat labels do not include 'experimental'/'control'; arms were ",
          "assigned alphabetically: control = '%s', experimental = '%s'. ",
          "The pooled effect direction depends on this assignment. ",
          "If it is wrong, set experimental_label/control_label in run_ma() ",
          "(or ingest_data())."
        ),
        control_label, experimental_label
      ))
    } else {
      rlang::abort(paste0(
        "Cannot infer experimental vs control arms from treat values: ",
        paste(treats, collapse = ", "),
        ". Specify experimental_label and control_label."
      ))
    }
  }

  studs <- unique(data$studlab)
  rows  <- lapply(studs, function(s) {
    sub <- data[data$studlab == s, , drop = FALSE]
    e_row <- sub[sub$treat == experimental_label, , drop = FALSE]
    c_row <- sub[sub$treat == control_label,      , drop = FALSE]
    if (nrow(e_row) != 1 || nrow(c_row) != 1) {
      rlang::abort(sprintf(
        "Study '%s' does not have exactly one intervention and one control arm.",
        s
      ))
    }
    out <- data.frame(
      studlab = s,
      n_e     = e_row$n,
      n_c     = c_row$n,
      stringsAsFactors = FALSE
    )
    if ("event" %in% names(sub)) {
      out$event_e <- e_row$event
      out$event_c <- c_row$event
    }
    if ("mean" %in% names(sub)) {
      out$mean_e <- e_row$mean
      out$mean_c <- c_row$mean
    }
    if ("sd" %in% names(sub)) {
      out$sd_e <- e_row$sd
      out$sd_c <- c_row$sd
    }
    out
  })

  do.call(rbind, rows)
}
