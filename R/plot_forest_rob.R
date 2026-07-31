# plot_forest_rob.R - Forest plot stratified by Risk-of-Bias level

#' Forest plot stratified by Risk-of-Bias subgroup
#'
#' Re-runs the meta-analysis with a Risk-of-Bias subgroup and draws a forest
#' plot showing the overall pooled estimate alongside the per-stratum pooled
#' estimates. Useful for visualizing whether high-RoB studies inflate the
#' apparent effect.
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param rob A character vector of length \code{meta_obj$k}. Accepts the same
#'   labels as \code{\link{grade_meta}} (case-insensitive): \code{"L"/"S"/"H"},
#'   \code{"low"/"some"/"high"}, the internal levels
#'   \code{"no"/"some_concerns"/"serious"}, and the Cochrane RoB2 wording
#'   \code{"No concerns"}, \code{"Some concerns"}, \code{"Serious concerns"},
#'   \code{"Critical concerns"}. \code{NA}, \code{""} and \code{"?"} are
#'   tolerated (kept as their own group labeled \code{"unknown"}); any other
#'   unrecognized label is bucketed into \code{"unknown"} with a warning.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_forest_rob <- function(meta_obj, rob, ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest_rob: meta_obj must be a meta-analysis object.")
  }

  k <- meta_obj$k
  n_orig <- length(meta_obj$studlab)
  if (is.null(rob) || !length(rob) %in% c(k, n_orig)) {
    graphics::plot.new()
    graphics::title(main = "Risk-of-Bias subgroup not available",
                    sub  = sprintf("RoB length must be %d (k) or %d (studlab); got %s",
                                   k, n_orig,
                                   if (is.null(rob)) "NULL" else as.character(length(rob))))
    return(invisible(NULL))
  }

  rob_norm <- .normalise_rob(rob)
  rob_factor <- factor(rob_norm,
                       levels = c("low", "some", "high", "unknown"))

  update_obj <- .subgroup_update_object(meta_obj)
  m_sg <- tryCatch(
    suppressWarnings(stats::update(
      update_obj,
      subgroup      = rob_factor,
      subgroup.name = "Risk of bias"
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

# Normalise per-study RoB labels to plot strata. Delegates to the shared
# grade_meta() vocabulary (.normalize_rob_level) so both entry points accept
# the same labels; NA / "" / "?" stay "unknown", anything else unrecognised
# warns before being bucketed there.
.normalise_rob <- function(rob) {
  .rob_plot_strata(rob, arg = "plot_forest_rob: rob")
}

.subgroup_update_object <- function(meta_obj) {
  obj <- meta_obj
  if (identical(attr(obj, "pma_rare_engine"), "mmeta") &&
      !is.null(obj$method) &&
      !(obj$method %in% c("Inverse", "MH", "Peto", "GLMM", "LRP", "SSW"))) {
    obj$method <- "MH"
  }
  obj
}

.restore_rare_overall <- function(m_sg, meta_obj) {
  if (!identical(attr(meta_obj, "pma_rare_engine"), "mmeta")) return(m_sg)
  for (nm in c("TE.common", "seTE.common", "lower.common", "upper.common",
               "statistic.common", "pval.common", "text.common",
               "common", "random", "overall")) {
    if (!is.null(meta_obj[[nm]])) m_sg[[nm]] <- meta_obj[[nm]]
  }
  m_sg$k <- length(m_sg$studlab)
  m_sg$k.TE <- length(m_sg$studlab)
  m_sg$k.all <- length(m_sg$studlab)
  attr(m_sg, "pma_rare_engine") <- attr(meta_obj, "pma_rare_engine")
  attr(m_sg, "pma_rare_method") <- attr(meta_obj, "pma_rare_method")
  attr(m_sg, "pma_rare_model") <- attr(meta_obj, "pma_rare_model")
  m_sg
}
