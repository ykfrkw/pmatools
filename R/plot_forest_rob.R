# plot_forest_rob.R - Forest plot stratified by Risk-of-Bias level

#' Forest plot stratified by Risk-of-Bias subgroup
#'
#' Re-runs the meta-analysis with a Risk-of-Bias subgroup and draws a forest
#' plot showing the overall pooled estimate alongside the per-stratum pooled
#' estimates. Useful for visualising whether high-RoB studies inflate the
#' apparent effect.
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param rob A character vector of length \code{meta_obj$k}, with values in
#'   \code{c("L","S","H","low","some","high")} (case-insensitive) or other
#'   recognisable aliases. NA values are tolerated (kept as their own group
#'   labelled \code{"unknown"}).
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

  m_sg <- tryCatch(
    suppressWarnings(stats::update(
      meta_obj,
      subgroup      = rob_factor,
      subgroup.name = "Risk of bias"
    )),
    error = function(e) NULL
  )

  if (is.null(m_sg)) {
    plot_forest(meta_obj, auto_layout = TRUE, ...)
    return(invisible(NULL))
  }

  plot_forest(m_sg, auto_layout = TRUE, ...)
  invisible(NULL)
}

# Normalise per-study RoB labels (case-insensitive single-letter or word forms).
.normalise_rob <- function(rob) {
  x <- tolower(trimws(as.character(rob)))
  out <- ifelse(
    is.na(x) | x == "" | x %in% c("na", "*", "?"),
    "unknown",
    ifelse(x %in% c("l", "low"), "low",
      ifelse(x %in% c("s", "some", "moderate", "m", "unclear"), "some",
        ifelse(x %in% c("h", "high"), "high", "unknown")
      )
    )
  )
  out
}
