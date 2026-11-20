# plot_forest_pubias_subgroup.R - Available vs Missing results forest plot
#
# Reference-only diagnostic for the Reporting Bias domain (RoB-ME, Page et al.,
# BMJ 2023, doi:10.1136/bmj-2023-076754). Mirrors the BMJ figure where two
# subgroups ("Available results" / "Missing results") are stacked on a single
# forest plot.
#
# Reads: a {meta} object and an optional missing-trials table. Produces: the
# drawing, and invisibly the two-subgroup meta object it drew. The Available
# block prints effect [95% CI] numbers like plot_forest() does, for every study,
# the pooled row and the prediction interval row, all from the analysis' own
# model settings. Missing rows have no estimate, so their effect cell is blank;
# their status string sits in its own "Results known" column.
#
# The row partition, the refit and its check against the analysis live in
# R/pubias_subgroup_meta.R; this file validates the arguments, lays out the
# forest and falls back to plot_forest() when nothing is missing. Callers:
# R/export_bundle.R and shiny/R/step3_pubias.R.
#
# A helper belongs here when it decides how the forest is laid out or which
# {meta} display options it is drawn with; which rows it holds and what numbers
# the meta object carries belong next door.

#' Forest plot of available vs missing study results
#'
#' Renders the supplied meta-analysis as a two-subgroup forest plot:
#' \describe{
#'   \item{Available results}{Studies in \code{meta_obj} with a finite TE/seTE,
#'     drawn with their effect estimates and 95\% CIs printed in the right
#'     column, the pooled (random-effects) diamond and row, the 95\%
#'     prediction interval (see \code{prediction}) and the heterogeneity line.
#'     The pool is refitted with \code{meta_obj}'s own model settings
#'     (\code{method.tau}, \code{method.random.ci}, \code{method.predict},
#'     the confidence levels, \code{exclude}, ...), so it reproduces the
#'     analysis' pooled estimate, CI and prediction interval; ratio measures
#'     are back-transformed. If the refit cannot reproduce them (e.g. a GLMM
#'     analysis), the function warns and draws the refit. This is the only
#'     pooled diamond on the plot: the missing rows carry no estimate, so an
#'     overall pool would repeat it.}
#'   \item{Missing results}{Two sources are merged here:
#'     (i) studies present in \code{meta_obj$studlab} but whose effect estimate
#'     is \code{NA} (e.g. all-zero events, dropped from pooling); these are
#'     auto-detected and labelled
#'     \code{"Reported but data not extractable"} by default.
#'     (ii) trials supplied via \code{missing_df} (registry / protocol /
#'     conference abstract entries with no extractable estimate). In both
#'     cases the row shows only \code{studlab}, sample size, a blank effect
#'     cell and, optionally, the "Results known" status string, and
#'     contributes nothing to the pooled estimate.}
#' }
#'
#' This is a reference-only diagnostic. It does not drive the certainty judgment.
#'
#' @param meta_obj A `meta` object (from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}}/\code{\link[meta]{metacont}}).
#' @param missing_df Optional \code{data.frame} of missing trials. Columns:
#'   \itemize{
#'     \item \code{studlab} (chr)
#'     \item \code{n} (int) total sample size or NA
#'     \item \code{results_known} (chr) free-text status string. Recommended
#'       values: "Reported but data not extractable", "Not measured",
#'       "Measured but not reported (suspect P > 0.05 / P < 0.05 / opposite
#'       direction)".
#'   }
#'   May be \code{NULL} or have 0 rows. With \code{auto_detect = TRUE}
#'   (default), entries are merged with any auto-detected NA-TE rows. With
#'   \code{auto_detect = FALSE}, the caller is responsible for supplying the
#'   complete set (typical when an interactive editor manages the list).
#' @param auto_detect Logical (default \code{TRUE}). If \code{TRUE}, studies
#'   in \code{meta_obj} with NA TE/seTE are automatically classified into
#'   the Missing-results subgroup and labelled
#'   \code{"Reported but data not extractable"}. If \code{FALSE}, the
#'   function uses \code{missing_df} verbatim and excludes NA-TE rows from
#'   \emph{both} subgroups.
#' @param title Optional plot title (passed as \code{smlab}).
#' @param label_e,label_c Optional arm labels forwarded to
#'   \code{\link[meta]{forest.meta}}.
#' @param xlim Optional length-2 numeric vector for the x-axis range. For ratio
#'   summary measures (OR/RR/HR/RoM/IRR), the range is snapped outward to
#'   standard log ticks.
#' @param favors_left,favors_right Optional labels positioned on the left and
#'   right of the x-axis (\code{label.left} / \code{label.right}).
#' @param show_n Logical. Only honoured when \code{meta_obj} has no missing
#'   rows (so this function falls back to a standard one-panel forest); the
#'   subgroup view always shows a single combined "N" (total sample size)
#'   column.
#' @param show_events Logical, fallback-only (see \code{show_n}).
#' @param addrow_above,addrow_below Number of blank rows above/below the
#'   overall pooled effect line. \code{addrow_below = NULL} (default) is
#'   computed from the drawn content so the bottom heterogeneity/test text
#'   clears the x-axis band and any \code{label.left}/\code{label.right}
#'   row (see \code{.auto_addrow_below()}). The two-subgroup view draws no
#'   overall summary (the Available-results pool is the only diamond), so
#'   \code{addrow_below} matters there only if \code{overall = TRUE} is
#'   passed through \code{...}.
#' @param show_results_known Logical (default \code{TRUE}). When \code{TRUE},
#'   a second right-hand column, "Results known", shows each Missing-results
#'   row's \code{results_known} status string (blank on available rows).
#'   When \code{FALSE}, that column is omitted, for a figure that lists the
#'   missing studies without annotating why each one is missing.
#' @param prediction Logical (default \code{TRUE}). When \code{TRUE} and at
#'   least three available studies are pooled, the Available subgroup's 95\%
#'   prediction interval is drawn and printed (\code{prediction.subgroup}).
#'   No overall prediction interval is drawn either way. In the no-missing
#'   fallback it is forwarded to \code{\link{plot_forest}}.
#' @param ... Additional arguments forwarded to \code{\link[meta]{forest.meta}}.
#'
#' @return Invisibly, the two-subgroup \code{meta} object that was drawn (its
#'   \code{TE.random.w["Available results"]} etc. hold the plotted pool). When
#'   nothing is missing, the function draws \code{\link{plot_forest}} and
#'   returns \code{meta_obj} invisibly. Side effect: draws on the active
#'   graphics device.
#'
#' @export
plot_forest_pubias_subgroup <- function(meta_obj, missing_df = NULL,
                                        auto_detect  = TRUE,
                                        title        = NULL,
                                        label_e      = NULL,
                                        label_c      = NULL,
                                        xlim         = NULL,
                                        favors_left  = NULL,
                                        favors_right = NULL,
                                        show_n       = TRUE,
                                        show_events  = FALSE,
                                        addrow_above = 0,
                                        addrow_below = NULL,
                                        show_results_known = TRUE,
                                        prediction   = TRUE,
                                        ...) {
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("plot_forest_pubias_subgroup: meta_obj must be a meta-analysis object.")
  }
  if (is.null(missing_df)) missing_df <- .pubias_empty_missing()
  if (!is.data.frame(missing_df)) {
    rlang::abort("plot_forest_pubias_subgroup: missing_df must be a data.frame.")
  }
  required_cols <- c("studlab", "n", "results_known")
  missing_cols <- setdiff(required_cols, names(missing_df))
  if (length(missing_cols)) {
    rlang::abort(sprintf(
      "plot_forest_pubias_subgroup: missing_df must have columns %s; missing: %s.",
      paste(required_cols, collapse = ", "),
      paste(missing_cols, collapse = ", ")
    ))
  }

  if (length(meta_obj$TE) == 0L) {
    rlang::abort("plot_forest_pubias_subgroup: meta_obj has no TE rows.")
  }
  sm       <- .sm_of(meta_obj) %||% ""
  is_ratio <- nzchar(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")

  rows <- .pubias_subgroup_rows(meta_obj, missing_df, auto_detect)
  full_missing_df <- rows$missing_rows

  # If there are no missing trials at all and TE is fully populated, fall
  # back to the standard one-panel forest. Display options are passed
  # through so the user's choices in the calling app still apply.
  if (nrow(full_missing_df) == 0L) {
    plot_forest(meta_obj,
                auto_layout  = TRUE,
                title        = title,
                label_e      = label_e,
                label_c      = label_c,
                xlim         = xlim,
                favors_left  = favors_left,
                favors_right = favors_right,
                show_n       = isTRUE(show_n),
                show_events  = isTRUE(show_events),
                addrow_above = addrow_above,
                addrow_below = addrow_below,
                prediction   = isTRUE(prediction),
                ...)
    return(invisible(meta_obj))
  }

  m_combined <- .pubias_subgroup_meta(meta_obj, rows)

  # {meta} computes a prediction interval from three pooled studies upward.
  k_pooled <- sum(m_combined$subgroup == PUBIAS_SUBGROUP_AVAILABLE &
                    !m_combined$exclude, na.rm = TRUE)
  draw_prediction <- isTRUE(prediction) && k_pooled >= 3L

  par_old <- graphics::par(mar = c(5, 4, 1, 2))
  on.exit(graphics::par(par_old), add = TRUE)

  # Dynamic bottom spacing (see plot_forest.R), kept for a caller that turns
  # the overall summary back on through `...`. With the overall summary off
  # (below) {meta} draws no lines under the pooled block, so this count no
  # longer pushes the x-axis down.
  #
  # Why the "Favours" labels used to collide with the tick labels: {meta}
  # puts label.left/label.right a fixed 2.5 lines below the bottom of the
  # drawn block, but places the axis at `spacing` times that bottom, so with
  # spacing < 1 the gap shrinks by (1 - spacing) of every line drawn below
  # the pooled diamond. The duplicate overall diamond and heterogeneity line,
  # plus the blank rows reserved for them, were those lines. Dropping them
  # restores the full gap; no separate offset is needed.
  dots <- list(...)
  if (is.null(addrow_below)) {
    addrow_below <- .auto_addrow_below(
      has_favors = .nzchar1(favors_left) || .nzchar1(favors_right) ||
                   .nzchar1(dots$label.left) || .nzchar1(dots$label.right),
      has_xlab   = .nzchar1(dots$xlab)
    )
  }

  # Same header as plot_forest(). {meta}'s effect.ci prints nothing for the
  # missing rows, whose TE is NA, so their effect cell stays blank.
  effect_lab <- if (nzchar(sm)) paste0(sm, " (95% CI)") else "Effect (95% CI)"
  right_cols <- "effect.ci"
  right_labs <- effect_lab
  if (isTRUE(show_results_known)) {
    right_cols <- c(right_cols, "results_known")
    right_labs <- c(right_labs, "Results known")
  }

  args <- list(
    x         = m_combined,
    smlab     = if (is.null(title)) "" else title,
    leftcols  = c("studlab", "n_total"),
    leftlabs  = c("Study",   "N"),
    rightcols = right_cols,
    rightlabs = right_labs,
    common    = FALSE,
    random    = TRUE,
    print.subgroup.labels = TRUE,
    # ONE pooled diamond and ONE heterogeneity line. Missing rows carry NA TE,
    # so the overall pool is the Available subgroup's pool drawn a second
    # time, under the Missing list, where it reads as if the missing studies
    # had been pooled. The Available subgroup's own diamond and heterogeneity
    # line are kept, so the figure still reads "Available results" pool +
    # "Missing results" list; the overall summary and its statistics go.
    random.subgroup     = TRUE,
    prediction          = FALSE,
    prediction.subgroup = draw_prediction,
    subgroup.hetstat = TRUE,
    overall          = FALSE,
    overall.hetstat  = FALSE,
    test.overall     = FALSE,
    test.subgroup    = FALSE,
    spacing   = 0.9,
    addrow.overall        = addrow_above > 0,
    addrows.below.overall = addrow_below
  )
  if (!is.null(label_e)      && nzchar(label_e))      args$label.e     <- label_e
  if (!is.null(label_c)      && nzchar(label_c))      args$label.c     <- label_c
  if (!is.null(favors_left)  && nzchar(favors_left))  args$label.left  <- favors_left
  if (!is.null(favors_right) && nzchar(favors_right)) args$label.right <- favors_right
  if (!is.null(xlim) && length(xlim) == 2L && all(is.finite(xlim)) && xlim[1] < xlim[2]) {
    if (is_ratio) {
      snapped <- .snap_log_xlim(xlim)
      args$xlim <- snapped
      args$at   <- .nice_log_ticks(snapped)
    } else {
      args$xlim <- xlim
      args$at   <- .nice_lin_ticks(xlim)
    }
  }
  args[names(dots)] <- dots

  res <- tryCatch(
    do.call(meta::forest, args),
    error = function(e) NULL
  )
  if (is.null(res)) {
    # A narrower layout for a {meta} that rejects the N column; the effect and
    # status columns are kept.
    args$leftcols <- "studlab"
    args$leftlabs <- "Study"
    tryCatch(do.call(meta::forest, args), error = function(e) NULL)
  }

  invisible(m_combined)
}
