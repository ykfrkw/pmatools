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
#' @param title Optional plot title. Drawn on its own line(s) above the
#'   column headers, word-wrapped to the device width, so a long title
#'   cannot collide with the \code{Events / N} and \code{OR (95\% CI)}
#'   headings. A newline in the title is honoured as an explicit line break
#'   and each resulting line is wrapped on its own; empty lines are dropped.
#'   Not passed to \code{\link[meta]{forest}} as \code{smlab};
#'   see \code{.draw_forest_title()}.
#' @param label_e Label for the experimental arm.
#' @param label_c Label for the control arm.
#' @param xlim Optional numeric vector of length 2; if NULL and
#'   \code{auto_layout = TRUE}, computed from the data.
#' @param prediction Show 95 percent prediction interval (default TRUE if available).
#' @param auto_layout Apply automatic margin/x-axis/cex tweaks (default TRUE).
#' @param threshold_lines Optional numeric scalar on the TE scale (log scale
#'   for ratio sm). When non-NULL, vertical dashed lines are drawn at
#'   \code{-threshold_lines} and \code{+threshold_lines} (or at
#'   \code{exp(-threshold)} and \code{exp(threshold)} on a log-scale axis) to
#'   indicate the clinical decision Threshold.
#' @param show_n Logical; if TRUE, add per-arm sample size columns
#'   (\code{n.e}, \code{n.c}) to the left of the forest.
#' @param show_events Logical; if TRUE, add the per-arm raw data columns that
#'   the object carries: event counts (\code{event.e}, \code{event.c}) for a
#'   binary outcome, or means and standard deviations (\code{mean.e},
#'   \code{sd.e}, \code{mean.c}, \code{sd.c}) for a continuous outcome.
#'   Ignored for objects that carry neither.
#' @param favors_left,favors_right Optional character labels positioned on
#'   the left and right of the x-axis (e.g., "Favors Control" / "Favors
#'   Treatment"). Passed to \code{meta::forest()} as \code{label.left} /
#'   \code{label.right}.
#' @param addrow_above Blank row above the pooled summary. Passed to
#'   \code{meta::forest()} as \code{addrow.overall}, which that function
#'   validates as a \emph{logical}, so only zero versus non-zero carries
#'   meaning here: the magnitude of a larger number is discarded by design,
#'   and \code{NA} or \code{NULL} is treated as \code{0}.
#' @param addrow_below Number of blank rows between the pooled summary and
#'   the heterogeneity/test statistics printed at the bottom (passed as
#'   \code{addrows.below.overall}). Default \code{NULL} computes the value
#'   from the drawn content so the heterogeneity text clears the x-axis
#'   band, the \code{label.left}/\code{label.right} row, and any
#'   \code{xlab} row (see \code{.auto_addrow_below()}). \code{NA}, a
#'   negative value, or anything not a length-1 finite number falls back to
#'   that automatic derivation.
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
                        auto_layout     = TRUE,
                        threshold_lines = NULL,
                        show_n          = FALSE,
                        show_events  = FALSE,
                        favors_left  = NULL,
                        favors_right = NULL,
                        addrow_above = 0,
                        addrow_below = NULL,
                        ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest: meta_obj must be a meta-analysis object.")
  }

  # Dynamic bottom spacing: meta::forest() prints the heterogeneity/test
  # lines `addrows.below.overall` rows below the pooled summary, while the
  # x-axis band (with tick labels), the label.left/label.right row, and any
  # xlab occupy the same vertical region. A fixed value of 1 made the
  # heterogeneity text overlap those elements (reviewer report), so when
  # the caller does not pin a value we derive it from the drawn content.
  #
  # Sanitise the addrow_* inputs BEFORE the auto-derivation below. An NA or
  # non-finite addrow_below would otherwise skip .auto_addrow_below() (it is
  # not NULL) and reach meta::forest(), which errors on it; the tryCatch retry
  # further down then re-runs the plot with leftcols/leftlabs stripped, so the
  # data columns silently vanish instead of surfacing the bad input.
  if (is.null(addrow_above) || length(addrow_above) != 1L ||
      is.na(addrow_above) || !is.finite(addrow_above)) {
    addrow_above <- 0
  }
  if (!is.null(addrow_below) &&
      (length(addrow_below) != 1L || is.na(addrow_below) ||
       !is.finite(addrow_below) || addrow_below < 0)) {
    addrow_below <- NULL
  }

  dots <- list(...)
  if (is.null(addrow_below)) {
    has_favors <- .nzchar1(favors_left) || .nzchar1(favors_right) ||
                  .nzchar1(dots$label.left) || .nzchar1(dots$label.right)
    has_xlab   <- .nzchar1(dots$xlab)
    addrow_below <- .auto_addrow_below(has_favors = has_favors,
                                       has_xlab   = has_xlab)
  }

  k  <- meta_obj$k
  sm <- meta_obj$sm
  is_ratio <- !is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")

  # Resolve arm labels (see .resolve_arm_labels() for the "Experimental"
  # default that {meta} bakes into the pooled object).
  arm_labs <- .resolve_arm_labels(label_e, label_c, meta_obj)
  label_e  <- arm_labs$e
  label_c  <- arm_labs$c

  # auto_layout: x-limits + base-graphics margins. Note: meta::forest() draws
  # with grid, so par(mar) only affects base-graphics fallbacks (plot.new /
  # abline); bottom-of-plot spacing is handled via addrow_below above.
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

  # Optional per-arm data columns, grouped intervention-first then control:
  #   studlab [event.e] [mean.e sd.e] [n.e] [event.c] [mean.c sd.c] [n.c]
  # `show_events` gates both the binary event counts and the continuous
  # mean/SD pair: a metabin object carries event.e/event.c but no mean.e,
  # and a metacont object carries mean.e/sd.e but no event.e, so the two
  # branches are mutually exclusive in practice and one flag serves both.
  has_events <- isTRUE(show_events) &&
                !is.null(meta_obj$event.e) && !is.null(meta_obj$event.c)
  has_meansd <- isTRUE(show_events) &&
                !is.null(meta_obj$mean.e) && !is.null(meta_obj$mean.c) &&
                !is.null(meta_obj$sd.e)   && !is.null(meta_obj$sd.c)
  has_n      <- isTRUE(show_n) &&
                !is.null(meta_obj$n.e) && !is.null(meta_obj$n.c)

  # The labels stay bare column names ("Events", "N", "Mean", "SD"): the arm
  # name must NOT be repeated here. Whenever per-arm columns are drawn,
  # meta::forest() always prints label.e/label.c as a spanning heading over
  # them (see .resolve_arm_labels() below, which supplies those strings), and
  # that heading lands in the same header cell as the first line of leftlabs.
  # Putting the arm name in both places made the two collide and render as
  # "CBTN" / "ControN". Bare labels give the intended two-level header:
  #                   CBT-I           Control
  #   Study    Events   N       Events   N
  left_cols <- "studlab"
  left_labs <- "Study"
  add_col <- function(cols, labs) {
    left_cols <<- c(left_cols, cols)
    left_labs <<- c(left_labs, labs)
  }
  if (has_events) add_col("event.e", "Events")
  if (has_meansd) add_col(c("mean.e", "sd.e"), c("Mean", "SD"))
  if (has_n)      add_col("n.e", "N")
  if (has_events) add_col("event.c", "Events")
  if (has_meansd) add_col(c("mean.c", "sd.c"), c("Mean", "SD"))
  if (has_n)      add_col("n.c", "N")

  effect_label <- if (!is.null(sm) && nzchar(sm)) {
    paste0(sm, " (95% CI)")
  } else "Effect (95% CI)"

  # The title is NOT smlab. {meta} draws smlab inside the header row, centred
  # over the forest column, so a title wider than that column overruns the
  # neighbouring header cells and renders as "EvenDepression response ...
  # GR (95% CI)" (reviewer report I-7). Titles here are outcome names chosen
  # by the caller, so no length bound holds -- shortening what callers append
  # only moves the threshold, and {meta} refuses an smlab of more than two
  # lines, so wrapping it in place does not generalise either. The title is
  # drawn afterwards instead, above the header; see .draw_forest_title().
  args <- list(
    x          = meta_obj,
    smlab      = "",
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

  # Try meta-native xline first if threshold_lines provided
  if (!is.null(threshold_lines) && is.numeric(threshold_lines) &&
      length(threshold_lines) == 1 && is.finite(threshold_lines) &&
      threshold_lines > 0) {
    if (is_ratio) {
      args$xline <- c(exp(-threshold_lines), exp(threshold_lines))
    } else {
      args$xline <- c(-threshold_lines, threshold_lines)
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

  # Drawn after the forest so the block it must clear has already reported its
  # own height; see .draw_forest_title().
  .draw_forest_title(title, res)

  # Fallback: draw Threshold lines via abline if xline was not honored
  if (!is.null(threshold_lines) && is.numeric(threshold_lines) &&
      length(threshold_lines) == 1 && is.finite(threshold_lines) &&
      threshold_lines > 0) {
    v <- if (is_ratio) c(exp(-threshold_lines), exp(threshold_lines))
         else          c(-threshold_lines, threshold_lines)
    tryCatch(graphics::abline(v = v, lty = 2, col = "#888888"),
             error = function(e) NULL)
  }

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Title, drawn above the column headers
# --------------------------------------------------------------------------
# Point size of the title, the fraction of the device width one line may
# occupy, and the clearance between the title and the top of the forest block.
# The width margin keeps a wrapped line off the device edge; the clearance is
# roughly half a line at PMA_FOREST_TITLE_FONTSIZE.
PMA_FOREST_TITLE_FONTSIZE <- 12
PMA_FOREST_TITLE_MAX_WIDTH <- 0.94
PMA_FOREST_TITLE_CLEARANCE_IN <- 0.10

# Draw the (wrapped) title above the forest, hugging it.
#
# `forest_result` is what meta::forest() returned, so this runs AFTER the
# forest is drawn -- deliberately. {meta} sizes its block to the device and
# centres it vertically, leaving equal margins top and bottom, and it reports
# the block's height as figheight$total_height. Reserving a band up front
# instead (grid layout + new = FALSE) shrinks the region {meta} centres in
# without shrinking the block, which strands the title above a large gap on
# the tall canvas the Shiny app renders to. Reading the height back and
# anchoring the title to the block's own top keeps the two together at every
# device size.
#
# Returns invisible(NULL); a failure to measure degrades to the device top
# rather than dropping the title.
.draw_forest_title <- function(title, forest_result = NULL) {
  lines <- .wrap_forest_title(title)
  if (is.null(lines)) return(invisible(NULL))

  tryCatch({
    gp <- grid::gpar(fontsize = PMA_FOREST_TITLE_FONTSIZE, fontface = "bold")
    device_height <- grDevices::dev.size("in")[2]
    title_height  <- grid::convertHeight(
      grid::grobHeight(grid::textGrob(paste(lines, collapse = "\n"), gp = gp)),
      "in", valueOnly = TRUE)

    block_height <- suppressWarnings(
      as.numeric(forest_result$figheight$total_height %||% NA_real_))[1]
    block_top <- if (is.na(block_height) || block_height <= 0 ||
                     block_height >= device_height) {
      device_height
    } else {
      (device_height - block_height) / 2
    }

    # grid measures y from the bottom. Sit the title on top of the block, then
    # clamp so a title taller than the margin runs off nothing but its own
    # clearance.
    #
    # The anchor is the title's BOTTOM, so extra lines -- wrapped or explicitly
    # broken (see .wrap_forest_title()) -- grow upward into the margin and the
    # last line stays the same distance above the column headers. title_height
    # is measured on the joined string, so the clamp tracks the line count too.
    baseline <- device_height - block_top + PMA_FOREST_TITLE_CLEARANCE_IN
    baseline <- min(baseline, device_height - title_height)
    baseline <- max(baseline, 0)

    grid::grid.text(
      paste(lines, collapse = "\n"),
      y    = grid::unit(baseline, "in"),
      just = c("centre", "bottom"),
      gp   = gp)
  }, error = function(e) NULL)

  invisible(NULL)
}

# Split the title on the caller's own line breaks, then wrap each piece to the
# device. The break has to be honoured BEFORE the wrap: the greedy pass below
# tokenises on "[[:space:]]+", which counts "\n" as ordinary white space, so a
# break typed into the title used to be eaten and the plot came back on one
# line. Callers rely on this to keep a long suffix off the column headers (see
# .forest_title_suffix in the app's step3_grade.R).
#
# Returns NULL for an absent or blank title.
.wrap_forest_title <- function(title) {
  if (!.nzchar1(title)) return(NULL)
  segments <- strsplit(as.character(title)[1], "[\r\n]+")[[1]]
  # An empty segment (a leading, trailing or doubled break) yields no words and
  # therefore no line: a title must not open with a blank line above it, and
  # "a\n\nb" reads as two lines rather than three. Dropping it is also what
  # keeps a title without breaks wrapping exactly as it did before.
  lines <- unlist(lapply(segments, .wrap_forest_title_segment),
                  use.names = FALSE)
  if (!length(lines)) return(NULL)
  lines
}

# Greedy word wrap of ONE line of the title, measured against the real device
# rather than a character count: the title is proportional-font text, so
# "Illness" and "WWWWWWW" do not cost the same. Returns character(0) for a
# blank segment. A single word wider than the device is left on its own line --
# there is nothing to break it on, and it is still the only thing on that line.
.wrap_forest_title_segment <- function(segment) {
  words <- strsplit(trimws(as.character(segment)), "[[:space:]]+")[[1]]
  words <- words[nzchar(words)]
  if (!length(words)) return(character(0))

  gp <- grid::gpar(fontsize = PMA_FOREST_TITLE_FONTSIZE, fontface = "bold")
  fits <- function(txt) {
    width <- tryCatch(
      grid::convertWidth(
        grid::grobWidth(grid::textGrob(txt, gp = gp)), "npc", valueOnly = TRUE),
      error = function(e) 0)
    width <= PMA_FOREST_TITLE_MAX_WIDTH
  }

  lines   <- character(0)
  current <- words[1]
  for (word in words[-1]) {
    candidate <- paste(current, word)
    if (fits(candidate)) {
      current <- candidate
    } else {
      lines   <- c(lines, current)
      current <- word
    }
  }
  c(lines, current)
}

# --------------------------------------------------------------------------
# Dynamic bottom spacing below the pooled summary
# --------------------------------------------------------------------------
# meta::forest() is grid-based, so par(mar) does not move its layout; the
# lever that separates the bottom heterogeneity/test text from the x-axis is
# `addrows.below.overall`. The axis line plus tick labels occupy ~2 rows;
# label.left/label.right add one more row under the axis, and a non-empty
# xlab yet another (mirrors meta's own default heuristic in forest.meta).
# Subgroup heterogeneity lines print inline under each subgroup diamond and
# the bottom text block (overall heterogeneity + test-for-subgroup lines)
# grows the grid layout row by row on its own, so no subgroup-count term is
# needed here.
.auto_addrow_below <- function(has_favors = FALSE, has_xlab = FALSE) {
  2L + as.integer(isTRUE(has_favors)) + as.integer(isTRUE(has_xlab))
}

# TRUE when x is a length>=1 non-NA character-like scalar with content
.nzchar1 <- function(x) {
  if (is.null(x) || length(x) == 0L) return(FALSE)
  x <- as.character(x)[1]
  !is.na(x) && nzchar(x)
}

# --------------------------------------------------------------------------
# Arm labels for the forest header
# --------------------------------------------------------------------------
# run_ma() does not set label.e/label.c, so meta::metabin()/metacont() fall
# back to meta::gs("label.e"), which is the string "Experimental". That
# default then travels with the pooled object into every forest header. We
# rewrite it here, in the display layer only: patching run_ma() would also
# change summary() output on stored meta objects and the results.txt written
# by export_bundle(). A caller-supplied label always wins.
.resolve_arm_labels <- function(label_e, label_c, meta_obj) {
  if (!.nzchar1(label_e)) label_e <- meta_obj$label.e
  if (!.nzchar1(label_c)) label_c <- meta_obj$label.c

  label_e <- if (!.nzchar1(label_e)) {
    "Intervention"
  } else if (identical(as.character(label_e)[1], "Experimental")) {
    # {meta}'s package default, not a deliberate caller choice.
    "Intervention"
  } else {
    as.character(label_e)[1]
  }

  label_c <- if (!.nzchar1(label_c)) "Control" else as.character(label_c)[1]

  list(e = label_e, c = label_c)
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
    finite_vals <- c(lo_e[is.finite(lo_e) & lo_e > 0],
                     hi_e[is.finite(hi_e) & hi_e > 0])
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
  if (!is.finite(lo) || lo <= 0) lo <- min(std)
  if (!is.finite(hi) || hi <= lo) hi <- max(lo * 10, 1)
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
