# plot_funnel.R - Contour-enhanced funnel plot with optional Egger annotation

#' Contour-enhanced funnel plot with Egger's test annotation
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param contour Numeric vector of significance levels for contour shading.
#'   Default `c(0.9, 0.95, 0.99)`.
#' @param show_egger Annotate with Egger's regression test result when k >= 10
#'   (default TRUE).
#' @param auto_layout Reserve top margin for the Egger annotation
#'   (default TRUE).
#' @param ... Additional arguments passed to \code{\link[meta]{funnel}}.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_funnel <- function(meta_obj,
                        contour     = c(0.9, 0.95, 0.99),
                        show_egger  = TRUE,
                        auto_layout = TRUE,
                        ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_funnel: meta_obj must be a meta-analysis object.")
  }

  par_old <- NULL
  if (isTRUE(auto_layout)) {
    par_old <- graphics::par(mar = c(4, 4, 3, 4))
    on.exit(graphics::par(par_old), add = TRUE)
  }

  contour_cols <- c("#dddddd", "#bbbbbb", "#888888")[seq_along(contour)]

  args <- list(
    x       = meta_obj,
    contour = contour,
    col.contour = contour_cols,
    ...
  )

  res <- tryCatch(
    do.call(meta::funnel, args),
    error = function(e) {
      # Some {meta} versions don't accept col.contour; retry without
      args$col.contour <- NULL
      do.call(meta::funnel, args)
    }
  )

  graphics::legend(
    "topright",
    legend = paste0(format(contour * 100), "% CI"),
    fill = contour_cols,
    bty = "n",
    cex = 0.75
  )

  if (isTRUE(show_egger)) {
    .annotate_egger(meta_obj)
  }

  invisible(NULL)
}

.annotate_egger <- function(meta_obj) {
  k <- meta_obj$k
  if (is.null(k) || is.na(k)) k <- 0L

  if (k < 10) {
    graphics::mtext("Egger's test not run (k < 10)",
                    side = 3, line = 0.3, cex = 0.85, col = "grey40")
    return(invisible(NULL))
  }

  res <- tryCatch(
    suppressWarnings(meta::metabias(meta_obj, method.bias = "linreg")),
    error = function(e) NULL
  )

  if (is.null(res) || is.null(res$p.value) || is.na(res$p.value)) {
    graphics::mtext("Egger's test failed to run",
                    side = 3, line = 0.3, cex = 0.85, col = "grey40")
    return(invisible(NULL))
  }

  msg <- if (!is.null(res$statistic) && !is.null(res$parameter) &&
             !is.na(res$statistic) && !is.na(res$parameter)) {
    sprintf("Egger's test: t = %.2f, df = %g, p = %.3f",
            as.numeric(res$statistic), as.numeric(res$parameter), res$p.value)
  } else {
    sprintf("Egger's test: p = %.3f", res$p.value)
  }
  graphics::mtext(msg, side = 3, line = 0.3, cex = 0.85)
}
