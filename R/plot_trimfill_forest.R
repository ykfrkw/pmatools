# plot_trimfill_forest.R - Forest plot of meta-analysis after trim-and-fill
#
# Trim-and-fill no longer drives the GRADE judgment (the 2-tier Egger rule
# in assess_pubias() supersedes it), but the imputed studies and the
# adjusted random-effects summary remain useful diagnostics. This helper
# renders them as a forest plot for the Reporting bias tab.

#' Forest plot of trim-and-fill imputed meta-analysis
#'
#' Runs \code{meta::trimfill()} on the supplied meta-analysis and renders the
#' augmented object as a forest plot, so observed and imputed (filled) studies
#' are shown together with the adjusted random-effects summary.
#'
#' @param x Either a \code{meta} object or a \code{pmatools} object returned by
#'   \code{\link{grade_meta}}. For \code{pmatools} objects the embedded `meta`
#'   field is extracted.
#' @param ... Additional arguments passed to \code{\link[meta]{forest.meta}}.
#'
#' @return Invisibly the trim-and-fill object (invisibly NULL if trim-and-fill
#'   could not be computed).
#'
#' @export
plot_trimfill_forest <- function(x, ...) {
  if (inherits(x, "pmatools")) {
    meta_obj <- x$meta
  } else {
    meta_obj <- x
  }
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_trimfill_forest: x must be a 'meta' or 'pmatools' object.")
  }

  k <- meta_obj$k
  if (is.null(k) || is.na(k) || k < 2) {
    rlang::abort(sprintf(
      "plot_trimfill_forest: trim-and-fill needs k >= 2 (got k = %s).",
      if (is.null(k)) "NULL" else as.character(k)
    ))
  }

  tf <- tryCatch(
    suppressWarnings(meta::trimfill(meta_obj)),
    error = function(e) NULL
  )
  if (is.null(tf)) {
    rlang::abort("plot_trimfill_forest: trim-and-fill failed to run on the supplied meta object.")
  }

  k_filled <- if (!is.null(tf$k0)) tf$k0 else 0L
  te_orig  <- meta_obj$TE.random
  te_adj   <- tf$TE.random
  is_ratio <- !is.null(meta_obj$sm) &&
              meta_obj$sm %in% c("OR", "RR", "HR", "RoM", "IRR")
  fmt_te <- function(v) {
    if (is.null(v) || is.na(v)) return("NA")
    if (is_ratio) sprintf("%.3f", exp(v)) else sprintf("%.3f", v)
  }
  sub_label <- sprintf(
    "Trim-and-fill: %d imputed stud%s; pooled %s = %s -> %s after adjustment.",
    k_filled, if (k_filled == 1L) "y" else "ies",
    if (!is.null(meta_obj$sm)) meta_obj$sm else "TE",
    fmt_te(te_orig), fmt_te(te_adj)
  )

  meta::forest(tf, ...)
  tryCatch(
    graphics::mtext(sub_label, side = 1, line = 3.0, cex = 0.85, adj = 0),
    error = function(e) invisible(NULL)
  )

  invisible(tf)
}
