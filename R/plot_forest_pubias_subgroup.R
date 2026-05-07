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
#'   \item{Available results}{Studies included in \code{meta_obj}, with usual
#'     point estimates, weights, and the pooled (random-effects) diamond.}
#'   \item{Missing results}{Trials known to exist (registry / protocol /
#'     conference abstract) but whose effect estimate is unavailable. These
#'     rows show only \code{studlab}, sample size, and a "Results known"
#'     status string in place of an effect estimate. They contribute nothing
#'     to the pooled estimate.}
#' }
#'
#' This is a reference-only diagnostic. It does not drive the GRADE judgment.
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param missing_df A \code{data.frame} with columns:
#'   \itemize{
#'     \item \code{studlab} (chr): trial label.
#'     \item \code{n} (int): total sample size (or NA).
#'     \item \code{results_known} (chr): one of the recommended ROB-ME
#'       categories, e.g. "Not measured",
#'       "Measured but not reported (suspect P > 0.05)",
#'       "Measured but not reported (suspect P < 0.05)",
#'       "Measured but not reported (in the opposite direction)".
#'   }
#'   If \code{nrow(missing_df) == 0}, falls back to a normal
#'   \code{\link{plot_forest}}.
#' @param ... Additional arguments forwarded to \code{\link[meta]{forest.meta}}.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_forest_pubias_subgroup <- function(meta_obj, missing_df, ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest_pubias_subgroup: meta_obj must be a meta-analysis object.")
  }
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

  if (nrow(missing_df) == 0) {
    plot_forest(meta_obj, auto_layout = TRUE, ...)
    return(invisible(NULL))
  }

  k_avail <- length(meta_obj$TE)
  k_miss  <- nrow(missing_df)
  sm      <- meta_obj$sm %||% ""
  is_ratio <- nzchar(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")

  fmt_one <- function(te, lo, up) {
    if (any(is.na(c(te, lo, up)))) return("")
    if (is_ratio) sprintf("%.2f (%.2f to %.2f)", exp(te), exp(lo), exp(up))
    else          sprintf("%.2f (%.2f to %.2f)",     te,      lo,      up)
  }
  effect_text_avail <- vapply(seq_len(k_avail), function(i) {
    fmt_one(meta_obj$TE[i], meta_obj$lower[i], meta_obj$upper[i])
  }, character(1))

  effect_status <- c(effect_text_avail, as.character(missing_df$results_known))

  # Some `meta` objects keep more rows in $studlab/$n.e/$n.c than in $TE
  # (rows with insufficient data are pre-filtered before pooling). Align all
  # vectors to the length of $TE so subgroup/length-checks inside meta::metagen
  # do not abort.
  studlab_avail <- as.character(meta_obj$studlab)
  if (length(studlab_avail) != k_avail) {
    studlab_avail <- studlab_avail[seq_len(k_avail)]
  }

  n_avail <- if (!is.null(meta_obj$n.e) && !is.null(meta_obj$n.c) &&
                 length(meta_obj$n.e) >= k_avail &&
                 length(meta_obj$n.c) >= k_avail) {
    meta_obj$n.e[seq_len(k_avail)] + meta_obj$n.c[seq_len(k_avail)]
  } else {
    rep(NA_integer_, k_avail)
  }
  n_combined <- c(n_avail, suppressWarnings(as.integer(missing_df$n)))

  studlab_combined <- c(studlab_avail, as.character(missing_df$studlab))
  subgroup_combined <- factor(
    c(rep("Available results", k_avail),
      rep("Missing results",   k_miss)),
    levels = c("Available results", "Missing results")
  )

  TE_combined   <- c(meta_obj$TE,   rep(NA_real_, k_miss))
  seTE_combined <- c(meta_obj$seTE, rep(NA_real_, k_miss))

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

  # Attach custom columns referenced by leftcols / rightcols below.
  m_combined$No            <- n_combined
  m_combined$effect_status <- effect_status

  par_old <- graphics::par(mar = c(5, 4, 1, 2))
  on.exit(graphics::par(par_old), add = TRUE)

  effect_lab <- if (is_ratio) sprintf("%s (95%% CI)", sm) else "Effect (95% CI)"

  args <- list(
    x        = m_combined,
    leftcols = c("studlab", "No"),
    leftlabs = c("Study",   "No"),
    rightcols = "effect_status",
    rightlabs = effect_lab,
    common   = FALSE,
    random   = TRUE,
    print.subgroup.labels = TRUE,
    test.subgroup = FALSE,
    spacing  = 0.9
  )
  extra <- list(...)
  args[names(extra)] <- extra

  res <- tryCatch(
    do.call(meta::forest, args),
    error = function(e) NULL
  )
  if (is.null(res)) {
    # Fallback: drop optional columns, retry minimally
    args$leftcols  <- "studlab"
    args$leftlabs  <- "Study"
    args$rightcols <- "effect_status"
    args$rightlabs <- effect_lab
    tryCatch(do.call(meta::forest, args), error = function(e) NULL)
  }

  invisible(NULL)
}
