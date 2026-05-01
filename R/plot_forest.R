# plot_forest.R - Forest plot wrapper with auto-layout
#
# Wraps meta::forest() with sensible defaults. auto_layout = TRUE handles
# x-axis scaling (log for ratios, quantile-based for continuous), top margin
# scaling with k, and label cex shrinking for long study names.

#' Forest plot for a meta-analysis with auto-layout
#'
#' @description
#' Draws a forest plot via \code{\link[meta]{forest}} with sensible defaults.
#' When \code{auto_layout = TRUE}, x-axis scale, top margin, and label sizes
#' are tuned automatically.
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param title Optional plot title (passed as `smlab`).
#' @param label_e Label for the experimental arm.
#' @param label_c Label for the control arm.
#' @param xlim Optional numeric vector of length 2; if NULL and
#'   \code{auto_layout = TRUE}, computed from the data.
#' @param prediction Show 95 percent prediction interval (default TRUE if available).
#' @param auto_layout Apply automatic margin/x-axis/cex tweaks (default TRUE).
#' @param ... Additional arguments passed to \code{\link[meta]{forest}}.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_forest <- function(meta_obj,
                        title       = NULL,
                        label_e     = NULL,
                        label_c     = NULL,
                        xlim        = NULL,
                        prediction  = TRUE,
                        auto_layout = TRUE,
                        ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest: meta_obj must be a meta-analysis object.")
  }

  k  <- meta_obj$k
  sm <- meta_obj$sm

  # Resolve labels
  if (is.null(label_e)) label_e <- meta_obj$label.e
  if (is.null(label_c)) label_c <- meta_obj$label.c

  # auto_layout: x-limits and margin
  par_old <- NULL
  if (isTRUE(auto_layout)) {
    if (is.null(xlim)) {
      xlim <- .auto_xlim(meta_obj)
    }
    top_mar <- 2 + ceiling(k / 8)
    par_old <- graphics::par(mar = c(4, 4, top_mar, 2))
    on.exit(graphics::par(par_old), add = TRUE)
  }

  # Auto cex for long study labels
  studlab <- meta_obj$studlab
  long_lbl <- !is.null(studlab) && any(nchar(as.character(studlab)) > 30)
  fs_lab <- if (isTRUE(auto_layout) && long_lbl) 0.85 else 1

  args <- list(
    x          = meta_obj,
    smlab      = if (is.null(title)) "" else title,
    prediction = prediction,
    fs.study   = 9,
    fs.heading = 11,
    leftcols   = c("studlab"),
    leftlabs   = c("Study"),
    rightcols  = c("effect.ci", "w.random"),
    rightlabs  = c("Effect (95% CI)", "Weight (%)"),
    ...
  )
  if (!is.null(label_e)) args$label.e <- label_e
  if (!is.null(label_c)) args$label.c <- label_c
  if (!is.null(xlim))    args$xlim    <- xlim
  if (long_lbl && isTRUE(auto_layout)) {
    args$fs.study <- 9 * fs_lab
  }

  # Some {meta} versions may complain about unknown args — try/catch then retry
  res <- tryCatch(
    do.call(meta::forest, args),
    error = function(e) {
      args$leftcols <- NULL
      args$leftlabs <- NULL
      args$rightcols <- NULL
      args$rightlabs <- NULL
      do.call(meta::forest, args)
    }
  )

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Auto x-limit calculation
# --------------------------------------------------------------------------
.auto_xlim <- function(meta_obj) {
  sm <- meta_obj$sm
  lo <- meta_obj$lower
  hi <- meta_obj$upper

  if (is.null(lo) || is.null(hi) || all(is.na(lo)) || all(is.na(hi))) {
    return(NULL)  # let meta::forest auto-compute
  }

  if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM")) {
    # Log-scale axis
    lo_e <- exp(lo)
    hi_e <- exp(hi)
    qrange <- stats::quantile(c(lo_e, hi_e), c(0.01, 0.99), na.rm = TRUE)
    out <- c(max(0.01, qrange[1] * 0.8), min(100, qrange[2] * 1.2))
    if (any(is.na(out)) || out[1] >= out[2]) return(NULL)
    return(unname(out))
  }

  # Linear scale (MD/SMD)
  qrange <- stats::quantile(c(lo, hi), c(0.01, 0.99), na.rm = TRUE)
  pad    <- 0.1 * abs(diff(qrange))
  out <- c(qrange[1] - pad, qrange[2] + pad)
  if (any(is.na(out)) || out[1] >= out[2]) return(NULL)
  unname(out)
}
