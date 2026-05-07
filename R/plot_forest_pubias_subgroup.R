# plot_forest_pubias_subgroup.R - Available vs Missing results forest plot
#
# Reference-only diagnostic for the Reporting Bias domain (ROB-ME, Page et al.,
# BMJ 2023, doi:10.1136/bmj-2023-076754). Mirrors the BMJ figure where two
# subgroups ("Available results" / "Missing results") are stacked on a single
# forest plot. Missing rows display a "Results known" status string in the
# effect column instead of a point estimate, since their effect is unknown.

#' Forest plot of available vs missing study results
#'
#' Renders the supplied meta-analysis as a two-subgroup forest plot:
#' \describe{
#'   \item{Available results}{Studies in \code{meta_obj} with a finite TE/seTE,
#'     drawn with their usual point estimates, weights, and the pooled
#'     (random-effects) diamond.}
#'   \item{Missing results}{Two sources are merged here:
#'     (i) studies present in \code{meta_obj$studlab} but whose effect estimate
#'     is \code{NA} (e.g. all-zero events, dropped from pooling); these are
#'     auto-detected and labelled
#'     \code{"Reported but data not extractable"} by default.
#'     (ii) trials supplied via \code{missing_df} (registry / protocol /
#'     conference abstract entries with no extractable estimate). In both
#'     cases the row shows only \code{studlab}, sample size, and a
#'     "Results known" status string in place of an effect estimate, and
#'     contributes nothing to the pooled estimate.}
#' }
#'
#' This is a reference-only diagnostic. It does not drive the GRADE judgment.
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param missing_df Optional \code{data.frame} of *additional* missing trials
#'   (i.e. trials not present in \code{meta_obj} at all). Columns:
#'   \itemize{
#'     \item \code{studlab} (chr)
#'     \item \code{n} (int) total sample size or NA
#'     \item \code{results_known} (chr) free-text status string. Recommended
#'       values: "Reported but data not extractable", "Not measured",
#'       "Measured but not reported (suspect P > 0.05 / P < 0.05 / opposite
#'       direction)".
#'   }
#'   May be \code{NULL} or have 0 rows.
#' @param ... Additional arguments forwarded to \code{\link[meta]{forest.meta}}.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_forest_pubias_subgroup <- function(meta_obj, missing_df = NULL, ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest_pubias_subgroup: meta_obj must be a meta-analysis object.")
  }
  empty_missing <- data.frame(
    studlab = character(0),
    n = integer(0),
    results_known = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(missing_df)) missing_df <- empty_missing
  if (!is.data.frame(missing_df)) {
    rlang::abort("plot_forest_pubias_subgroup: missing_df must be a data.frame.")
  }
  required_cols <- c("studlab", "n", "results_known")
  missing_cols <- setdiff(required_cols, names(missing_df))
  if (length(missing_cols)) {
    rlang::abort(sprintf(
      "plot_forest_pubias_subgroup: missing_df must have columns %s; missing: %s.",
      paste(required_cols, collapse = ", "),
      paste(missing_cols, collapse = ", ")
    ))
  }

  # ----------------------------------------------------------------
  # Align vectors to length(meta_obj$TE) (some metabin variants keep
  # extra rows in $studlab/$n.e/$n.c that are not in $TE).
  # ----------------------------------------------------------------
  k_te <- length(meta_obj$TE)
  if (k_te == 0L) {
    rlang::abort("plot_forest_pubias_subgroup: meta_obj has no TE rows.")
  }
  te        <- meta_obj$TE
  seTE      <- meta_obj$seTE
  lower_obj <- meta_obj$lower
  upper_obj <- meta_obj$upper
  sm        <- meta_obj$sm %||% ""
  is_ratio  <- nzchar(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")

  studlab_obj <- as.character(meta_obj$studlab)
  if (length(studlab_obj) > k_te) studlab_obj <- studlab_obj[seq_len(k_te)]
  if (length(studlab_obj) < k_te) {
    studlab_obj <- c(studlab_obj,
                     sprintf("(unnamed %d)", seq_len(k_te - length(studlab_obj))))
  }
  n_obj <- if (!is.null(meta_obj$n.e) && !is.null(meta_obj$n.c) &&
               length(meta_obj$n.e) >= k_te &&
               length(meta_obj$n.c) >= k_te) {
    meta_obj$n.e[seq_len(k_te)] + meta_obj$n.c[seq_len(k_te)]
  } else {
    rep(NA_integer_, k_te)
  }

  # Partition meta_obj's studies by TE finiteness.
  te_finite     <- is.finite(te) & is.finite(seTE)
  avail_idx     <- which(te_finite)
  auto_miss_idx <- which(!te_finite)

  # Auto-detected missing rows from meta_obj (NA TE).
  auto_miss_df <- if (length(auto_miss_idx) > 0L) {
    data.frame(
      studlab = studlab_obj[auto_miss_idx],
      n = suppressWarnings(as.integer(n_obj[auto_miss_idx])),
      results_known = "Reported but data not extractable",
      stringsAsFactors = FALSE
    )
  } else empty_missing

  # User-supplied missing rows are coerced to the canonical schema.
  user_miss_df <- if (nrow(missing_df) > 0L) {
    data.frame(
      studlab = as.character(missing_df$studlab),
      n = suppressWarnings(as.integer(missing_df$n)),
      results_known = as.character(missing_df$results_known),
      stringsAsFactors = FALSE
    )
  } else empty_missing

  full_missing_df <- rbind(auto_miss_df, user_miss_df)

  # If there are no missing trials at all and TE is fully populated, fall
  # back to the standard one-panel forest with sample-size column shown by
  # default (N = n.e + n.c per study).
  if (nrow(full_missing_df) == 0L) {
    plot_forest(meta_obj, auto_layout = TRUE, show_n = TRUE, ...)
    return(invisible(NULL))
  }

  # ----------------------------------------------------------------
  # Build a synthetic two-subgroup metagen.
  # ----------------------------------------------------------------
  k_avail <- length(avail_idx)
  k_miss  <- nrow(full_missing_df)

  fmt_one <- function(t, lo, up) {
    if (any(is.na(c(t, lo, up)))) return("")
    if (is_ratio) sprintf("%.2f (%.2f to %.2f)", exp(t), exp(lo), exp(up))
    else          sprintf("%.2f (%.2f to %.2f)",     t,      lo,      up)
  }
  effect_text_avail <- if (k_avail > 0L) {
    vapply(avail_idx, function(i) {
      fmt_one(te[i], lower_obj[i], upper_obj[i])
    }, character(1))
  } else character(0)
  effect_status <- c(effect_text_avail, full_missing_df$results_known)

  studlab_combined  <- c(studlab_obj[avail_idx], full_missing_df$studlab)
  n_combined        <- c(n_obj[avail_idx],       full_missing_df$n)
  subgroup_combined <- factor(
    c(rep("Available results", k_avail),
      rep("Missing results",   k_miss)),
    levels = c("Available results", "Missing results")
  )
  TE_combined   <- c(te[avail_idx],   rep(NA_real_, k_miss))
  seTE_combined <- c(seTE[avail_idx], rep(NA_real_, k_miss))

  # De-duplicate study labels (meta::metagen rejects exact duplicates).
  studlab_combined <- make.unique(studlab_combined, sep = " #")

  m_err <- NULL
  m_combined <- tryCatch(
    suppressWarnings(meta::metagen(
      TE            = TE_combined,
      seTE          = seTE_combined,
      studlab       = studlab_combined,
      subgroup      = subgroup_combined,
      subgroup.name = "",
      sm            = if (nzchar(sm)) sm else "MD",
      common        = FALSE,
      random        = TRUE
    )),
    error = function(e) { m_err <<- conditionMessage(e); NULL }
  )

  if (is.null(m_combined)) {
    rlang::abort(sprintf(
      "plot_forest_pubias_subgroup: failed to build combined meta object: %s",
      m_err %||% "unknown error"
    ))
  }

  m_combined$No            <- n_combined
  m_combined$effect_status <- effect_status

  par_old <- graphics::par(mar = c(5, 4, 1, 2))
  on.exit(graphics::par(par_old), add = TRUE)

  effect_lab <- if (is_ratio) sprintf("%s (95%% CI)", sm) else "Effect (95% CI)"

  args <- list(
    x         = m_combined,
    leftcols  = c("studlab", "No"),
    leftlabs  = c("Study",   "No"),
    rightcols = "effect_status",
    rightlabs = effect_lab,
    common    = FALSE,
    random    = TRUE,
    print.subgroup.labels = TRUE,
    test.subgroup = FALSE,
    spacing   = 0.9
  )
  extra <- list(...)
  args[names(extra)] <- extra

  res <- tryCatch(
    do.call(meta::forest, args),
    error = function(e) NULL
  )
  if (is.null(res)) {
    args$leftcols  <- "studlab"
    args$leftlabs  <- "Study"
    args$rightcols <- "effect_status"
    args$rightlabs <- effect_lab
    tryCatch(do.call(meta::forest, args), error = function(e) NULL)
  }

  invisible(NULL)
}
