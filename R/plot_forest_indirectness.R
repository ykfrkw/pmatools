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
#' @param indirectness A character vector of length \code{meta_obj$k}, with
#'   values in \code{c("L","S","H","low","some","high")} (case-insensitive) or
#'   other recognizable aliases. NA values are tolerated (kept as their own
#'   group labeled \code{"unknown"}).
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

  m_sg <- tryCatch(
    suppressWarnings(stats::update(
      meta_obj,
      subgroup      = indir_factor,
      subgroup.name = "Indirectness"
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

# Normalise per-study Indirectness labels (case-insensitive single-letter or
# word forms). Same vocabulary as RoB.
.normalise_indirectness <- function(x) {
  v <- tolower(trimws(as.character(x)))
  out <- ifelse(
    is.na(v) | v == "" | v %in% c("na", "*", "?"),
    "unknown",
    ifelse(v %in% c("l", "low"), "low",
      ifelse(v %in% c("s", "some", "moderate", "m", "unclear"), "some",
        ifelse(v %in% c("h", "high"), "high", "unknown")
      )
    )
  )
  out
}
