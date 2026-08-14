# plot_forest_indirectness.R - Forest plot stratified by Indirectness level

#' Forest plot stratified by Indirectness subgroup
#'
#' Re-runs the meta-analysis with an Indirectness subgroup and draws a forest
#' plot showing the overall pooled estimate alongside the per-stratum pooled
#' estimates. Useful for visualizing whether high-indirectness studies inflate
#' or distort the apparent effect.
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param indirectness A character vector of length \code{meta_obj$k}. Accepts
#'   the same labels as \code{\link{plot_forest_rob}} (case-insensitive):
#'   \code{"L"/"S"/"H"}, \code{"low"/"some"/"high"} and the internal levels
#'   \code{"not_serious"/"serious"/"very_serious"}. The risk-of-bias tools'
#'   own judgments are accepted too because the alias table is shared, but
#'   they name a different construct -- there is no RoB 2 for indirectness, so
#'   prefer the plain three. \code{NA}, \code{""} and \code{"?"} are
#'   tolerated (kept as their own group labeled \code{"unknown"}); any other
#'   unrecognized label is bucketed into \code{"unknown"} with a warning.
#' @param ... Additional arguments forwarded to \code{\link{plot_forest}}.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_forest_indirectness <- function(meta_obj, indirectness, ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest_indirectness: meta_obj must be a meta-analysis object.")
  }

  k <- meta_obj$k
  n_orig <- length(meta_obj$studlab)
  if (is.null(indirectness) || !length(indirectness) %in% c(k, n_orig)) {
    graphics::plot.new()
    graphics::title(main = "Indirectness subgroup not available",
                    sub  = sprintf("indirectness length must be %d (k) or %d (studlab); got %s",
                                   k, n_orig,
                                   if (is.null(indirectness)) "NULL"
                                   else as.character(length(indirectness))))
    return(invisible(NULL))
  }

  indir_norm <- .normalise_indirectness(indirectness)
  indir_factor <- factor(indir_norm,
                         levels = c("low", "some", "high", "unknown"))

  update_obj <- .subgroup_update_object(meta_obj)
  m_sg <- tryCatch(
    suppressWarnings(stats::update(
      update_obj,
      subgroup      = indir_factor,
      subgroup.name = "Indirectness"
    )),
    error = function(e) NULL
  )

  if (is.null(m_sg)) {
    plot_forest(meta_obj, auto_layout = TRUE, ...)
    return(invisible(NULL))
  }

  m_sg <- .restore_rare_overall(m_sg, meta_obj)
  plot_forest(m_sg, auto_layout = TRUE, ...)
  invisible(NULL)
}

# Normalise per-study Indirectness labels to plot strata. Same vocabulary as
# RoB (see .rob_plot_strata in domain_rob.R).
.normalise_indirectness <- function(x) {
  .rob_plot_strata(x, arg = "plot_forest_indirectness: indirectness")
}
