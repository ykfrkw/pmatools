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
#' @param some_concerns_as \code{NULL} (default), \code{"low"} or
#'   \code{"high"}. \code{NULL} keeps the four descriptive strata above.
#'   Supplying \code{"low"} or \code{"high"} instead draws the \strong{two}
#'   groups the algorithm actually analyses -- \code{"Low risk of bias"} and
#'   \code{"High risk of bias"} -- folded with the same internal rule
#'   \code{\link{grade_meta}} applies to its own \code{rob_some_concerns}
#'   argument, which this one is named after. Studies left unrated follow
#'   whichever side \code{"some concerns"} takes, exactly as they do in the
#'   rating. Use it whenever the plot sits next to a rating: with the default
#'   four strata a reader sees four groups beside a judgment made on two.
#' @param ... Additional arguments passed to \code{\link{plot_forest}}.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_forest_rob <- function(meta_obj, rob, some_concerns_as = NULL, ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest_rob: meta_obj must be a meta-analysis object.")
  }
  if (!is.null(some_concerns_as) &&
      !(length(some_concerns_as) == 1L &&
        some_concerns_as %in% c("low", "high"))) {
    rlang::abort(paste0(
      "plot_forest_rob: some_concerns_as must be NULL, 'low' or 'high'."))
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
  if (is.null(some_concerns_as)) {
    rob_factor    <- factor(rob_norm,
                            levels = c("low", "some", "high", "unknown"))
    subgroup_name <- "Risk of bias"
  } else {
    rob_factor    <- .rob_analysis_strata(rob_norm, some_concerns_as)
    subgroup_name <- "Risk of bias (as analysed)"
  }

  update_obj <- .subgroup_update_object(meta_obj)
  m_sg <- tryCatch(
    suppressWarnings(stats::update(
      update_obj,
      subgroup      = rob_factor,
      subgroup.name = subgroup_name
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

# The two-group fold, as a factor ready for stats::update(subgroup = ).
#
# The fold itself is NOT re-implemented here: .rob_high_levels() is the same
# internal assess_rob() consults, so a study on the "high" side of the plot is
# a study on the "high" side of the rating. The only mapping this function
# owns is plot strata -> internal levels, which is the inverse of the one
# rob_strata() applies on the way in ("unknown" -> "some_concerns", because an
# unrated study reaches grade_meta() as "*" and normalises there the same way).
#
# The factor carries BOTH levels even when one is empty, so the legend and the
# subgroup rows are stable as the reviewer moves the boundary.
.rob_analysis_strata <- function(rob_norm, some_concerns_as) {
  internal <- c(low = "no", some = "some_concerns", high = "serious",
                unknown = "some_concerns")[rob_norm]
  high <- unname(internal) %in% .rob_high_levels(some_concerns_as)
  factor(ifelse(high, "High risk of bias", "Low risk of bias"),
         levels = c("Low risk of bias", "High risk of bias"))
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
