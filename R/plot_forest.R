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
#' @param mid_lines Optional numeric scalar on the TE scale (log scale for
#'   ratio sm). When non-NULL, vertical dashed lines are drawn at
#'   \code{-mid_lines} and \code{+mid_lines} (or at \code{exp(-mid)} and
#'   \code{exp(mid)} on a log-scale axis) to indicate the threshold for
#'   clinical importance.
#' @param show_n Logical; if TRUE, add per-arm sample size columns
#'   (\code{n.e}, \code{n.c}) to the left of the forest.
#' @param show_events Logical; if TRUE and binary outcome, add per-arm
#'   event count columns (\code{event.e}, \code{event.c}).
#' @param favors_left,favors_right Optional character labels positioned on
#'   the left and right of the x-axis (e.g., "Favors Control" / "Favors
#'   Treatment"). Passed to \code{meta::forest()} as \code{label.left} /
#'   \code{label.right}.
#' @param ... Additional arguments passed to \code{\link[meta]{forest}}.
#'
#' @return Invisibly NULL. Side effect: draws on the active graphics device.
#'
#' @export
plot_forest <- function(meta_obj,
                        title        = NULL,
                        label_e      = NULL,
                        label_c      = NULL,
                        xlim         = NULL,
                        prediction   = TRUE,
                        auto_layout  = TRUE,
                        mid_lines    = NULL,
                        show_n       = FALSE,
                        show_events  = FALSE,
                        favors_left  = NULL,
                        favors_right = NULL,
                        addrow_above = 0,
                        addrow_below = 1,
                        ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest: meta_obj must be a meta-analysis object.")
  }

  k  <- meta_obj$k
  sm <- meta_obj$sm
  is_ratio <- !is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")

  # Resolve labels
  if (is.null(label_e)) label_e <- meta_obj$label.e
  if (is.null(label_c)) label_c <- meta_obj$label.c

  # auto_layout: x-limits + tight margins (top minimal, bottom enough for
  # axis tick labels AND heterogeneity row without overlap)
  par_old <- NULL
  if (isTRUE(auto_layout)) {
    if (is.null(xlim)) xlim <- .auto_xlim(meta_obj)
    par_old <- graphics::par(mar = c(6.5, 4, 0.5, 2), oma = c(0, 0, 0, 0))
    on.exit(graphics::par(par_old), add = TRUE)
  }

  # Auto cex for long study labels
  studlab <- meta_obj$studlab
  long_lbl <- !is.null(studlab) && any(nchar(as.character(studlab)) > 30)
  fs_lab <- if (isTRUE(auto_layout) && long_lbl) 0.85 else 1

  # Optional N + event columns: Exp event, Exp N, Ctrl event, Ctrl N order.
  has_events <- isTRUE(show_events) &&
                !is.null(meta_obj$event.e) && !is.null(meta_obj$event.c)
  has_n      <- isTRUE(show_n) &&
                !is.null(meta_obj$n.e) && !is.null(meta_obj$n.c)

  left_cols <- "studlab"
  left_labs <- "Study"
  if (has_events) {
    left_cols <- c(left_cols, "event.e")
    left_labs <- c(left_labs, "Events Exp")
  }
  if (has_n) {
    left_cols <- c(left_cols, "n.e")
    left_labs <- c(left_labs, "N Exp")
  }
  if (has_events) {
    left_cols <- c(left_cols, "event.c")
    left_labs <- c(left_labs, "Events Ctrl")
  }
  if (has_n) {
    left_cols <- c(left_cols, "n.c")
    left_labs <- c(left_labs, "N Ctrl")
  }

  effect_label <- if (!is.null(sm) && nzchar(sm)) {
    paste0(sm, " (95% CI)")
  } else "Effect (95% CI)"

  args <- list(
    x          = meta_obj,
    smlab      = if (is.null(title)) "" else title,
    prediction = prediction,
    fs.study   = 9,
    fs.heading = 11,
    leftcols   = left_cols,
    leftlabs   = left_labs,
    rightcols  = c("effect.ci", "w.random"),
    rightlabs  = c(effect_label, "Weight (%)"),
    spacing    = 0.9,
    addrow.overall       = addrow_above > 0,
    addrows.below.overall = addrow_below,
    ...
  )
  if (!is.null(label_e))      args$label.e     <- label_e
  if (!is.null(label_c))      args$label.c     <- label_c
  if (!is.null(favors_left)  && nzchar(favors_left))  args$label.left  <- favors_left
  if (!is.null(favors_right) && nzchar(favors_right)) args$label.right <- favors_right

  # Snap xlim to nice log ticks for ratio measures
  if (isTRUE(auto_layout) && is_ratio && !is.null(xlim)) {
    snapped <- .snap_log_xlim(xlim)
    args$xlim <- snapped
    args$at   <- .nice_log_ticks(snapped)
  } else if (!is.null(xlim)) {
    args$xlim <- xlim
    if (isTRUE(auto_layout) && !is_ratio) {
      args$at <- .nice_lin_ticks(xlim)
    }
  }

  if (long_lbl && isTRUE(auto_layout)) {
    args$fs.study <- 9 * fs_lab
  }

  # Try meta-native xline first if mid_lines provided
  if (!is.null(mid_lines) && is.numeric(mid_lines) &&
      length(mid_lines) == 1 && is.finite(mid_lines) && mid_lines > 0) {
    if (is_ratio) {
      args$xline <- c(exp(-mid_lines), exp(mid_lines))
    } else {
      args$xline <- c(-mid_lines, mid_lines)
    }
  }

  # Some {meta} versions may complain about unknown args - try/catch then retry
  res <- tryCatch(
    do.call(meta::forest, args),
    error = function(e) {
      args$leftcols <- NULL
      args$leftlabs <- NULL
      args$rightcols <- NULL
      args$rightlabs <- NULL
      args$xline <- NULL
      args$addrow.overall <- NULL
      args$addrows.below.overall <- NULL
      args$label.left  <- NULL
      args$label.right <- NULL
      tryCatch(do.call(meta::forest, args), error = function(e2) NULL)
    }
  )

  # Fallback: draw MID lines via abline if xline was not honoured
  if (!is.null(mid_lines) && is.numeric(mid_lines) &&
      length(mid_lines) == 1 && is.finite(mid_lines) && mid_lines > 0) {
    v <- if (is_ratio) c(exp(-mid_lines), exp(mid_lines))
         else          c(-mid_lines, mid_lines)
    tryCatch(graphics::abline(v = v, lty = 2, col = "#888888"),
             error = function(e) NULL)
  }

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Auto x-limit calculation (snap to standard log ticks for ratio scales)
# --------------------------------------------------------------------------
.auto_xlim <- function(meta_obj) {
  sm <- meta_obj$sm
  lo <- meta_obj$lower
  hi <- meta_obj$upper

  if (is.null(lo) || is.null(hi) || all(is.na(lo)) || all(is.na(hi))) {
    return(NULL)
  }

  if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")) {
    lo_e <- exp(lo); hi_e <- exp(hi)
    finite_vals <- c(lo_e[is.finite(lo_e)], hi_e[is.finite(hi_e)])
    if (length(finite_vals) == 0) return(NULL)
    qrange <- stats::quantile(finite_vals, c(0.05, 0.95), na.rm = TRUE)
    out <- c(qrange[1], qrange[2])
    if (any(is.na(out)) || out[1] >= out[2]) return(NULL)
    return(unname(out))
  }

  qrange <- stats::quantile(c(lo, hi), c(0.05, 0.95), na.rm = TRUE)
  pad <- 0.1 * abs(diff(qrange))
  out <- c(qrange[1] - pad, qrange[2] + pad)
  if (any(is.na(out)) || out[1] >= out[2]) return(NULL)
  unname(out)
}

# Snap (lo, hi) outward to the nearest standard log ticks
.snap_log_xlim <- function(xlim) {
  std <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05,
           0.1, 0.2, 0.5, 1, 2, 5,
           10, 20, 50, 100, 200, 500, 1000)
  lo <- xlim[1]; hi <- xlim[2]
  xmin <- max(std[std <= lo], na.rm = TRUE)
  xmax <- min(std[std >= hi], na.rm = TRUE)
  if (!is.finite(xmin)) xmin <- min(std)
  if (!is.finite(xmax)) xmax <- max(std)
  c(xmin, xmax)
}

# Nice log ticks within (xmin, xmax)
.nice_log_ticks <- function(xlim) {
  std <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05,
           0.1, 0.2, 0.5, 1, 2, 5,
           10, 20, 50, 100, 200, 500, 1000)
  ticks <- std[std >= xlim[1] & std <= xlim[2]]
  if (length(ticks) >= 4) ticks else std[std >= xlim[1] & std <= xlim[2] * 2]
}

# Nice linear ticks within (xmin, xmax) using pretty()
.nice_lin_ticks <- function(xlim) {
  grDevices::axisTicks(usr = xlim, log = FALSE, nint = 6)
}
