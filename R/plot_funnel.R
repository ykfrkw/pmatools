# plot_funnel.R - Contour-enhanced funnel plot
#
# Uses meta::funnel() defaults for the visual style, and adds a single
# top-right legend box that lists each significance zone, the Studies
# marker, and the Egger test result.

#' Contour-enhanced funnel plot with annotated legend
#'
#' @param meta_obj A `meta` object.
#' @param contour Numeric vector of (1 - alpha) significance levels for
#'   contour boundaries. Default \code{c(0.9, 0.95, 0.99)} produces 4 zones:
#'   p > 0.10, 0.05-0.10, 0.01-0.05, < 0.01.
#' @param show_egger Annotate with Egger's regression test result inside the
#'   legend box (default TRUE).
#' @param auto_layout Tight margins (default TRUE).
#' @param ... Additional arguments passed to \code{\link[meta]{funnel}}.
#'
#' @return Invisibly NULL.
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

  if (isTRUE(auto_layout)) {
    par_old <- graphics::par(mar = c(4, 4, 1, 2))
    on.exit(graphics::par(par_old), add = TRUE)
  }

  args <- list(
    x       = meta_obj,
    contour = contour,
    ...
  )

  res <- tryCatch(
    do.call(meta::funnel, args),
    error = function(e) NULL
  )

  # Annotation swatch palette. Innermost (p > 0.10) is white. The remaining
  # non-white zones go innermost -> outermost as DARK -> LIGHT, mirroring the
  # default {meta} rendering (where bands closer to the funnel apex appear
  # darker than bands further out).
  band_cols <- c("#ffffff", "#9c9c9c", "#bcbcbc")[seq_along(contour)]
  panel_bg  <- "#dcdcdc"

  # Build top-right legend: zone labels (innermost first), Studies marker, Egger.
  alpha_lvls <- 1 - contour
  zone_labels  <- character(0)
  zone_fills   <- character(0)
  zone_borders <- character(0)

  zone_labels  <- c(zone_labels,  sprintf("%.2f < p <= 1.00", max(alpha_lvls)))
  zone_fills   <- c(zone_fills,   band_cols[1])
  zone_borders <- c(zone_borders, "#000000")

  if (length(alpha_lvls) >= 2) {
    sorted_a <- sort(alpha_lvls, decreasing = TRUE)  # 0.10, 0.05, 0.01
    for (i in seq_len(length(sorted_a) - 1)) {
      hi <- sorted_a[i]; lo <- sorted_a[i + 1]
      zone_labels  <- c(zone_labels, sprintf("%.2f < p <= %.2f", lo, hi))
      band_idx <- i + 1
      zone_fills   <- c(zone_fills, band_cols[band_idx])
      zone_borders <- c(zone_borders, "#000000")
    }
  }

  zone_labels  <- c(zone_labels,  sprintf("0.00 < p <= %.2f", min(alpha_lvls)))
  zone_fills   <- c(zone_fills,   panel_bg)
  zone_borders <- c(zone_borders, "#000000")

  zone_labels  <- c(zone_labels,  "Studies")
  zone_fills   <- c(zone_fills,   NA)
  zone_borders <- c(zone_borders, NA)

  egger_label <- if (isTRUE(show_egger)) .egger_label(meta_obj) else NULL
  if (!is.null(egger_label)) {
    zone_labels  <- c(zone_labels,  egger_label)
    zone_fills   <- c(zone_fills,   NA)
    zone_borders <- c(zone_borders, NA)
  }

  pch_col <- rep(NA_integer_, length(zone_labels))
  studies_idx <- which(zone_labels == "Studies")
  if (length(studies_idx)) pch_col[studies_idx] <- 16

  graphics::legend(
    "topright",
    legend   = zone_labels,
    fill     = zone_fills,
    border   = zone_borders,
    pch      = pch_col,
    pt.cex   = 0.9,
    bty      = "o",
    bg       = "#ffffff",
    box.col  = "#000000",
    text.col = "#000000",
    cex      = 0.78,
    inset    = 0.02
  )

  invisible(NULL)
}

.egger_label <- function(meta_obj) {
  k <- meta_obj$k
  if (is.null(k) || is.na(k)) k <- 0L
  if (k < 10) {
    return(sprintf("Egger's test: not run (k = %d < 10)", k))
  }
  res <- tryCatch(
    suppressWarnings(meta::metabias(meta_obj, method.bias = "linreg")),
    error = function(e) NULL
  )
  if (is.null(res) || is.null(res$p.value) || is.na(res$p.value)) {
    return("Egger's test: failed to run")
  }
  if (!is.null(res$statistic) && !is.null(res$parameter) &&
      !is.na(res$statistic) && !is.na(res$parameter)) {
    sprintf("Egger's test: t = %.2f, df = %g, p = %.3f",
            as.numeric(res$statistic), as.numeric(res$parameter), res$p.value)
  } else {
    sprintf("Egger's test: p = %.3f", res$p.value)
  }
}
