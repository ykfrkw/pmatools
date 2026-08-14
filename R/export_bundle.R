# export_bundle.R - Pack analysis artifacts into a reproducible ZIP
#
# export_bundle() is an S3 generic dispatching on its first argument:
#   meta          -> the single-outcome flat bundle in this file
#   pmatools      -> convenience wrapper for the same (grade object first)
#   pmatools_set  -> the multi-outcome layout in export_bundle_multi.R

#' Export a reproducible analysis bundle as a ZIP
#'
#' @description
#' Bundles every artifact of the analysis into a single ZIP: the long-format
#' CSV, a reproducible `analysis.R` script, results.txt, forest/funnel plots,
#' the SoF table, and the certainty appendix (Core GRADE series). The
#' bundled `analysis.R` runs
#' standalone with `library(pmatools)` and the bundled CSV.
#'
#' Passing a \code{pmatools_set} from \code{\link{grade_meta_multi}} instead
#' produces the multi-outcome layout: the summary tables at the top level and
#' one numbered `outcomes/NN_name/` sub-directory per outcome. See
#' \code{\link{export_bundle.pmatools_set}}.
#'
#' @param x A `meta` object from \code{\link{run_ma}} (the single-outcome
#'   entry point), a `pmatools` object, or a `pmatools_set`.
#' @param grade A `pmatools` object from \code{\link{grade_meta}}.
#' @param output_dir Directory where the ZIP is created.
#' @param bundle_name Bundle base name (no extension).
#' @param include Which artifacts to include. See Details.
#' @param style (v0.5.1) Summary-of-findings layout, `"bmj"` (default) or
#'   `"gradepro"`. Passed to \code{\link{sof_table}} for `sof_table.docx` and
#'   to \code{\link{grade_report}} for the certainty appendix, and rendered
#'   into the bundled `analysis.R` so re-running the script reproduces the
#'   layout that was exported. Same default as
#'   \code{\link{export_bundle.pmatools_set}}; note that `sof_table()` itself
#'   still defaults to `"gradepro"`.
#' @param follow_up (v0.5.1) Optional free-text time frame shown under the
#'   outcome name in the `"bmj"` layout, e.g.
#'   \code{"Follow-up: longest, range 7.7-60 months"}. Defaults to
#'   `grade$follow_up` when the rated object carries one (objects rated by
#'   \code{\link{grade_meta_multi}} do). Ignored by the `"gradepro"` layout.
#' @param unit (v0.5.1) Optional unit for the Difference column of the
#'   `"bmj"` layout with continuous outcomes, e.g. \code{"days"}. Defaults to
#'   `grade$unit` on the same terms as `follow_up`.
#' @param sof_notes (v0.5.1) Optional character vector of extra footnote lines
#'   for the bundled `sof_table.docx`, appended by
#'   \code{\link{sof_add_notes}} after the table's own footnotes and rendered
#'   into `analysis.R`. For annotations pmatools cannot derive — a host
#'   application's rare-event alert or scope caveat, a registration number.
#'   Applies to `sof_table.docx` only, not to the certainty appendix.
#' @param per Denominator for SoF rate columns. Default 1000.
#' @param prediction Show 95 percent prediction interval in SoF Effect column.
#' @param convert_smd_to_or Logical. Passed to \code{\link{sof_table}} for
#'   continuous-outcome dichotomisation.
#' @param keep_effect_scale Logical (default \code{FALSE}). Passed to
#'   \code{\link{sof_table}}: shows the outcome on its own scale AND as a
#'   proportion of responders in one row. Only relevant when
#'   \code{convert_smd_to_or = TRUE}.
#' @param baseline_risk Numeric in (0,1). Passed to \code{\link{sof_table}}
#'   when \code{convert_smd_to_or = TRUE}.
#' @param threshold_label Optional free-text label describing the
#'   dichotomisation threshold.
#' @param chinn_invert Logical (default \code{FALSE}). Passed to
#'   \code{\link{sof_table}}: flips the SMD sign before applying Chinn's
#'   formula so that a negative-is-better SMD yields OR > 1 in the
#'   dichotomised rate columns. Only relevant when
#'   \code{convert_smd_to_or = TRUE}.
#' @param other_text Optional free-text "Other considerations" note passed to
#'   \code{\link{evidence_profile}} for the bundled `grade_table.docx`.
#' @param other_downgrade Integer \code{0}/\code{-1}/\code{-2} (default
#'   \code{0L}). Extra downgrade applied on top of the automatic domain
#'   calculations; passed to \code{\link{evidence_profile}}.
#' @param data Optional canonical long-format tibble from
#'   \code{\link{ingest_data}}; if provided, written to `data_long.csv`. If
#'   NULL, the function attempts to reconstruct from `ma$data`.
#' @param grade_args Optional named list of `grade_meta()` argument
#'   specifications with `value`/`origin`/`col` slots, used to render
#'   `analysis.R` faithfully. `origin` must be one of `"null"`, `"column"`,
#'   `"scalar"`, or `"vector"`; any other value aborts rather than rendering
#'   the argument as `NULL`. Names are matched **exactly** against
#'   `grade_meta()`'s arguments (no partial matching, so an
#'   `inconsistency_ci_diff` spec can never answer for `inconsistency`); a
#'   name that is not a `grade_meta()` argument aborts rather than being
#'   silently dropped from the generated script. See SPEC.md.
#' @param ma_args Optional named list of `run_ma()` argument specifications.
#' @param forest_display Optional named list of arguments forwarded to
#'   \code{\link{plot_forest}} when rendering the bundled forest plot.
#'   Recognised names: `title`, `label_e`, `label_c`, `xlim`,
#'   `favors_left`, `favors_right`, `show_n`, `show_events`,
#'   `addrow_above`, `addrow_below`, `digits_mean`, `digits_sd`.
#' @param rob Optional character vector of per-study Risk-of-Bias labels
#'   (length \code{length(meta_obj$studlab)} or \code{meta_obj$k}). Required
#'   when `"forest_rob"` is in `include` to render the stratified forest plot.
#' @param forest_display_rob Optional named list of `plot_forest` arguments
#'   for the RoB-stratified forest plot bundled when `"forest_rob"` is in
#'   `include`. Same recognized names as `forest_display`.
#' @param rare Optional \code{pma_rare_meta} object from
#'   \code{\link{run_rare_ma}}. When supplied, rare-event diagnostics,
#'   the method table, and the method-comparison forest plot are bundled.
#' @param rare_forest_display Optional named list of display arguments for
#'   \code{\link{plot_rare_sensitivity_forest}}.
#' @param pubias_missing_df Optional data.frame of studies with unavailable
#'   results, with columns `studlab`, `n`, and `results_known`; forwarded to
#'   \code{\link{plot_forest_pubias_subgroup}} when
#'   `"pubias_missing_forest"` is in `include` (rendered only when k >= 10).
#'
#' @param ... Passed to the method.
#'
#' @section Legacy `ma =` calls:
#' Before version 0.5.0 `export_bundle()` was a plain function whose first
#' argument was named `ma`. It is now an S3 generic whose first argument is
#' `x`. Named calls of the form `export_bundle(ma = m, grade = g, ...)` are
#' still honoured, with a deprecation warning issued once per session; they
#' are dispatched as if `ma` had been passed as `x`. Update such calls to
#' pass the meta object positionally (or as `x =`).
#'
#' @section Version stamp for vendored sources:
#' The bundle records the pmatools version in `results.txt` and in the header
#' of the generated `analysis.R`. That version normally comes from the
#' installed package. A host application that *vendors* the pmatools sources
#' (i.e. `source()`s the `R/*.R` files instead of installing the package) has
#' no installed DESCRIPTION to read, so it should set
#' `options(pmatools.version_stamp = "0.5.0")` to the version of the sources
#' it vendored; the bundle then reports `0.5.0 (vendored)`. The option must be
#' a single non-empty string; anything else, or leaving it unset, makes the
#' bundle report `(vendored; version unknown)`. The option is ignored whenever
#' pmatools is genuinely installed.
#'
#' @return Character. Absolute path to the created ZIP file.
#'
#' @export
export_bundle <- function(x, ...) {
  if (missing(x)) {
    args <- list(...)
    if (!is.null(args$ma)) {
      # v0.5.0 renamed the first formal from 'ma' to 'x' when export_bundle()
      # became an S3 generic. Keep legacy named calls working for now.
      rlang::warn(
        paste0(
          "export_bundle(ma = ) is deprecated as of pmatools 0.5.0.\n",
          "export_bundle() is now an S3 generic; its first argument is 'x'.\n",
          "Pass the meta object positionally, e.g. export_bundle(m, grade = g)."
        ),
        .frequency    = "once",
        .frequency_id = "export_bundle_ma_arg"
      )
      x <- args$ma
      args$ma <- NULL
      return(do.call(export_bundle, c(list(x), args)))
    }
  }
  UseMethod("export_bundle")
}

#' @rdname export_bundle
#' @export
export_bundle.default <- function(x, ...) {
  if (.is_not_reported(x)) {
    rlang::abort(paste0(
      "export_bundle: a not-reported outcome has no analysis to bundle on its ",
      "own. Add it to the set with add_not_reported() and call ",
      "export_bundle() on the set."))
  }
  rlang::abort("export_bundle: 'ma' must be a meta object.")
}

#' @rdname export_bundle
#' @export
export_bundle.pmatools <- function(x, ...) {
  # Convenience: the grade object knows the meta object it rated (the low-RoB
  # refit, when one happened), so export_bundle(g) is unambiguous.
  export_bundle.meta(x$meta, grade = x, ...)
}

#' @rdname export_bundle
#' @export
export_bundle.meta <- function(x,
                          grade,
                          output_dir   = ".",
                          bundle_name  = "pmatools_results",
                          include      = c("data", "script", "results",
                                           "forest", "forest_rob", "funnel",
                                           "funnel_trimfill",
                                           "pubias_missing_forest",
                                           "grade_table"),
                          style        = c("bmj", "gradepro"),
                          per          = 1000,
                          prediction   = FALSE,
                          follow_up    = NULL,
                          unit         = NULL,
                          sof_notes    = NULL,
                          convert_smd_to_or = FALSE,
                          keep_effect_scale = FALSE,
                          baseline_risk     = NULL,
                          threshold_label   = NULL,
                          chinn_invert      = FALSE,
                          other_text         = NULL,
                          other_downgrade    = 0L,
                          data               = NULL,
                          grade_args         = NULL,
                          ma_args            = NULL,
                          forest_display     = NULL,
                          rob                = NULL,
                          forest_display_rob = NULL,
                          rare               = NULL,
                          rare_forest_display = NULL,
                          pubias_missing_df  = NULL,
                          ...) {
  ma <- x
  if (!inherits(ma, "meta")) {
    rlang::abort("export_bundle: 'ma' must be a meta object.")
  }
  if (!inherits(grade, "pmatools")) {
    rlang::abort("export_bundle: 'grade' must be a pmatools object.")
  }
  style <- match.arg(style)
  # follow_up / unit are presentation-only and grade_meta() does not take them,
  # but grade_meta_multi() stores them on the object it rates. Fall back to what
  # the object carries so a set member exported on its own keeps the follow-up
  # line the multi-outcome table would have shown.
  follow_up <- .display_arg(follow_up %||% grade$follow_up)
  unit      <- .display_arg(unit      %||% grade$unit)
  sof_notes <- .usable_notes(sof_notes)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  work_dir <- file.path(tempdir(), paste0("pmatools_bundle_", as.integer(Sys.time())))
  dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  files_in_zip <- character()

  # 1. data_long.csv
  if ("data" %in% include) {
    if (is.null(data)) {
      data <- .reconstruct_long_from_ma(ma)
    }
    if (!is.null(data)) {
      csv_path <- file.path(work_dir, "data_long.csv")
      utils::write.csv(data, csv_path, row.names = FALSE)
      files_in_zip <- c(files_in_zip, csv_path)
    }
  }

  # 2. analysis.R
  if ("script" %in% include) {
    script_path <- file.path(work_dir, "analysis.R")
    .render_analysis_script(ma, grade, ma_args, grade_args,
                            per, prediction,
                            convert_smd_to_or, baseline_risk, threshold_label,
                            script_path,
                            keep_effect_scale = isTRUE(keep_effect_scale),
                            rare = rare,
                            style = style, follow_up = follow_up, unit = unit,
                            sof_notes = sof_notes)
    files_in_zip <- c(files_in_zip, script_path)
  }

  # 3. results.txt
  if ("results" %in% include) {
    results_path <- file.path(work_dir, "results.txt")
    .write_results_txt(ma, grade, results_path)
    files_in_zip <- c(files_in_zip, results_path)
  }

  # 4. forest plot
  if ("forest" %in% include) {
    pdf_path <- file.path(work_dir, "forest_plot.pdf")
    fd <- if (is.list(forest_display)) forest_display else list()
    if (is.null(fd$title) || !nzchar(fd$title %||% "")) fd$title <- grade$outcome_name
    .save_plot_pdf(
      function() do.call(plot_forest, c(list(meta_obj = ma), fd)),
      pdf_path,
      width = max(7, 3 + 0.3 * ma$k),
      height = max(5, 1.5 + 0.35 * ma$k)
    )
    files_in_zip <- c(files_in_zip, pdf_path)
  }

  # 4b. forest plot stratified by RoB
  if ("forest_rob" %in% include && !is.null(rob)) {
    pdf_path <- file.path(work_dir, "forest_plot_rob.pdf")
    fdr <- if (is.list(forest_display_rob)) forest_display_rob else list()
    if (is.null(fdr$title) || !nzchar(fdr$title %||% "")) {
      fdr$title <- paste0(grade$outcome_name, " (stratified by RoB)")
    }
    k_extra <- if (!is.null(ma$k)) ma$k else 0L
    .save_plot_pdf(
      function() do.call(plot_forest_rob,
                         c(list(meta_obj = ma, rob = rob), fdr)),
      pdf_path,
      width  = max(8, 3 + 0.3 * k_extra),
      height = max(7, 3 + 0.4 * (k_extra + 4))
    )
    files_in_zip <- c(files_in_zip, pdf_path)
  }

  # 4c. rare-events sensitivity outputs
  if (!is.null(rare) && inherits(rare, "pma_rare_meta")) {
    files_in_zip <- c(files_in_zip,
                      file.path(work_dir, .write_rare_tables(rare, work_dir)))
    spec <- .rare_forest_drawer(rare, rare_forest_display)
    pdf_path <- file.path(work_dir, "rare_event_method_forest.pdf")
    .save_plot_pdf(spec$draw, pdf_path,
                   width = spec$width, height = spec$height)
    files_in_zip <- c(files_in_zip, pdf_path)
  }

  # 5. funnel plot
  if ("funnel" %in% include) {
    pdf_path <- file.path(work_dir, "funnel_plot.pdf")
    .save_plot_pdf(
      function() plot_funnel(ma),
      pdf_path,
      width = 7, height = 6
    )
    files_in_zip <- c(files_in_zip, pdf_path)
  }

  # 5b. trim-and-fill funnel (k >= 10)
  if ("funnel_trimfill" %in% include && (ma$k %||% 0L) >= 10) {
    pdf_path <- file.path(work_dir, "funnel_trimfill.pdf")
    .save_plot_pdf(.trimfill_funnel_drawer(ma), pdf_path,
                   width = 7, height = 6)
    files_in_zip <- c(files_in_zip, pdf_path)
  }

  # 5c. publication bias missing-results forest (k >= 10)
  if ("pubias_missing_forest" %in% include && (ma$k %||% 0L) >= 10) {
    pdf_path <- file.path(work_dir, "pubias_missing_forest.pdf")
    spec <- .pubias_missing_drawer(ma, pubias_missing_df)
    .save_plot_pdf(spec$draw, pdf_path,
                   width = spec$width, height = spec$height)
    files_in_zip <- c(files_in_zip, pdf_path)
  }

  # 6a. grade_table.docx — Evidence Profile (Core GRADE series)
  if ("grade_table" %in% include) {
    ep_ft <- evidence_profile(grade,
                              other_text      = other_text,
                              other_downgrade = other_downgrade)
    ep_path <- file.path(work_dir, "grade_table.docx")
    .save_landscape_docx(ep_ft, ep_path)
    files_in_zip <- c(files_in_zip, ep_path)

    # 6b. sof_table.docx — Summary of Findings
    sof_ft <- sof_table(grade, style = style,
                        per = per, prediction = prediction,
                        follow_up         = follow_up,
                        unit              = unit,
                        convert_smd_to_or = convert_smd_to_or,
                        keep_effect_scale = isTRUE(keep_effect_scale),
                        baseline_risk     = baseline_risk,
                        threshold_label   = threshold_label,
                        chinn_invert      = isTRUE(chinn_invert))
    sof_ft <- sof_add_notes(sof_ft, sof_notes)
    sof_path <- file.path(work_dir, "sof_table.docx")
    .save_landscape_docx(sof_ft, sof_path)
    files_in_zip <- c(files_in_zip, sof_path)
  }

  # 7. grade_appendix.docx
  if ("grade_appendix" %in% include) {
    appendix_path <- tryCatch({
      out <- grade_report(
        outcomes    = stats::setNames(list(grade), grade$outcome_name),
        primary     = grade$outcome_name,
        # One layout per bundle: the appendix embeds a SoF table of its own, so
        # leaving it at grade_report()'s default would ship two layouts in the
        # same ZIP.
        style       = style,
        format      = "docx",
        output_dir  = work_dir,
        output_file = "grade_appendix"
      )
      if (is.character(out) && length(out) == 1) out else
        file.path(work_dir, "grade_appendix.docx")
    }, error = function(e) {
      warning(sprintf("grade_report() failed: %s", conditionMessage(e)))
      NULL
    })
    if (!is.null(appendix_path) && file.exists(appendix_path)) {
      files_in_zip <- c(files_in_zip, appendix_path)
    }
  }

  # Build ZIP
  zip_path <- file.path(normalizePath(output_dir, mustWork = FALSE),
                        paste0(bundle_name, ".zip"))
  if (file.exists(zip_path)) file.remove(zip_path)

  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = zip_path,
             files   = basename(files_in_zip),
             root    = work_dir)
  } else {
    old_wd <- setwd(work_dir); on.exit(setwd(old_wd), add = TRUE)
    utils::zip(zipfile = zip_path, files = basename(files_in_zip))
  }

  normalizePath(zip_path)
}

# --------------------------------------------------------------------------
# Shared writers
# --------------------------------------------------------------------------

# Write a flextable into a landscape-orientation .docx using officer directly.
# Avoids flextable::save_as_docx(pr_section = ...), whose argument is not
# present in older flextable versions.
.save_landscape_docx <- function(ft, path) {
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_end_section_landscape(doc, w = 11, h = 8.5)
  print(doc, target = path)
  invisible(path)
}

# --------------------------------------------------------------------------
# Reconstruct canonical long format from a meta object (best effort)
# --------------------------------------------------------------------------
.reconstruct_long_from_ma <- function(ma) {
  studlab <- ma$studlab
  if (is.null(studlab)) return(NULL)

  if (!is.null(ma$event.e) && !is.null(ma$event.c)) {
    e <- data.frame(studlab = studlab, treat = "experimental",
                    n = ma$n.e, event = ma$event.e, stringsAsFactors = FALSE)
    c <- data.frame(studlab = studlab, treat = "control",
                    n = ma$n.c, event = ma$event.c, stringsAsFactors = FALSE)
    return(rbind(e, c))
  }
  if (!is.null(ma$mean.e) && !is.null(ma$mean.c)) {
    e <- data.frame(studlab = studlab, treat = "experimental",
                    n = ma$n.e, mean = ma$mean.e, sd = ma$sd.e,
                    stringsAsFactors = FALSE)
    c <- data.frame(studlab = studlab, treat = "control",
                    n = ma$n.c, mean = ma$mean.c, sd = ma$sd.c,
                    stringsAsFactors = FALSE)
    return(rbind(e, c))
  }
  NULL
}

.rare_diagnostics_table <- function(x) {
  if (is.null(x)) {
    return(data.frame(metric = character(), value = character(),
                      stringsAsFactors = FALSE))
  }
  keep <- names(x)[vapply(x, function(z) {
    is.atomic(z) && length(z) == 1L
  }, logical(1))]
  data.frame(
    metric = keep,
    value = vapply(x[keep], function(z) {
      if (is.logical(z)) {
        if (isTRUE(z)) "TRUE" else "FALSE"
      } else if (is.numeric(z)) {
        format(z, scientific = FALSE, trim = TRUE)
      } else {
        as.character(z)
      }
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

# --------------------------------------------------------------------------
# Artifacts shared by the flat and the per-outcome layouts
# --------------------------------------------------------------------------
# Each returns what to draw rather than drawing it, because the two layouts
# differ in what a failed plot costs: the flat bundle is one analysis and
# aborts, the multi-outcome one warns and carries on with the other outcomes
# (.bundle_plot(), export_bundle_multi.R).

# The two rare-event tables. Returns the file names it wrote, relative to
# `dir`, so a caller that records ZIP-relative paths can prefix them.
.write_rare_tables <- function(rare, dir) {
  utils::write.csv(.rare_diagnostics_table(rare$diagnostics),
                   file.path(dir, "rare_event_diagnostics.csv"),
                   row.names = FALSE)
  utils::write.csv(as.data.frame(rare$method_table),
                   file.path(dir, "rare_event_method_table.csv"),
                   row.names = FALSE)
  c("rare_event_diagnostics.csv", "rare_event_method_table.csv")
}

.rare_forest_drawer <- function(rare, rare_forest_display = NULL) {
  rfd <- if (is.list(rare_forest_display)) rare_forest_display else list()
  rfd <- rfd[intersect(names(rfd), c("title", "xlim",
                                     "favors_left", "favors_right"))]
  if (is.null(rfd$title) || !nzchar(rfd$title %||% "")) {
    rfd$title <- "Rare-event method sensitivity"
  }
  n_methods <- nrow(as.data.frame(rare$method_table))
  list(
    draw   = function() do.call(plot_rare_sensitivity_forest,
                                c(list(x = rare), rfd)),
    width  = max(8, 4 + 0.4 * n_methods),
    height = max(5, 2.5 + 0.45 * n_methods)
  )
}

# Trim-and-fill funnel: the imputed studies are drawn in red so the reader can
# see how many were filled and where. Trim-and-fill that cannot be computed
# yields a titled blank page rather than an error, because the plot is a
# diagnostic and its absence is itself the finding.
.trimfill_funnel_drawer <- function(ma) {
  tf <- tryCatch(suppressWarnings(meta::trimfill(ma)), error = function(e) NULL)
  function() {
    if (is.null(tf)) {
      graphics::plot.new()
      graphics::title(main = "Trim-and-fill could not be computed")
      return(invisible(NULL))
    }
    n_total <- length(tf$TE)
    is_imp  <- if (!is.null(tf$trimfill)) {
      as.logical(tf$trimfill)
    } else {
      k0 <- if (!is.null(tf$k0)) as.integer(tf$k0) else
            (n_total - (ma$k %||% 0L))
      c(rep(FALSE, n_total - k0), rep(TRUE, k0))
    }
    meta::funnel(tf,
                 contour = c(0.9, 0.95, 0.99),
                 pch = rep(21L, n_total),
                 col = ifelse(is_imp, "red", "black"),
                 bg  = ifelse(is_imp, "red", "darkgray"),
                 cex = ifelse(is_imp, 1.6, 1.0))
    graphics::legend(
      "topright",
      legend = c("Observed studies", "Imputed (filled) studies"),
      pch    = c(21, 21),
      col    = c("black", "red"),
      pt.bg  = c("darkgray", "red"),
      pt.cex = c(1.0, 1.4),
      bty    = "o", bg = "#ffffff", cex = 0.8
    )
  }
}

.pubias_missing_drawer <- function(ma, pubias_missing_df = NULL) {
  m_df <- if (is.data.frame(pubias_missing_df) &&
              all(c("studlab", "n", "results_known") %in%
                  names(pubias_missing_df))) {
    pubias_missing_df
  } else {
    data.frame(studlab = character(0), n = integer(0),
               results_known = character(0), stringsAsFactors = FALSE)
  }
  k_avail <- length(ma$TE)
  k_miss  <- nrow(m_df)
  list(
    draw   = function() plot_forest_pubias_subgroup(meta_obj    = ma,
                                                    missing_df  = m_df,
                                                    auto_detect = FALSE),
    width  = max(8, 3 + 0.3 * (k_avail + k_miss)),
    height = max(7, 3 + 0.4 * (k_avail + k_miss + 4))
  )
}

# --------------------------------------------------------------------------
# Plot saving (PDF)
# --------------------------------------------------------------------------
.save_plot_pdf <- function(draw_fn, pdf_path, width = 8, height = 6) {
  grDevices::pdf(pdf_path, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_fn()
  grDevices::dev.off()
  on.exit()  # clear

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Results text
# --------------------------------------------------------------------------

# Two meta objects describe the same analysis set when they pool the same
# studies; the refit rebuilds the object, so identical() is too strict.
.same_analysis_set <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  identical(as.integer(a$k %||% NA_integer_), as.integer(b$k %||% NA_integer_)) &&
    identical(as.character(a$studlab), as.character(b$studlab))
}

# Which analysis set `ma` is, phrased for a results.txt heading (longer than
# .analysis_set_label(), which labels a table cell). Only called when a low-RoB
# refit happened, so both `grade$meta` and `grade$meta_full` are populated and
# describe different sets of studies.
.results_txt_set_label <- function(ma, grade) {
  k_low  <- grade$meta$k
  k_full <- grade$meta_full$k
  if (.same_analysis_set(ma, grade$meta)) {
    sprintf("low risk of bias studies only (%d of %d studies; rated below)",
            k_low, k_full)
  } else if (.same_analysis_set(ma, grade$meta_full)) {
    sprintf("all studies (%d studies; NOT the analysis rated below)", k_full)
  } else {
    "analysis as supplied (NOT the analysis rated below)"
  }
}

.write_results_txt <- function(ma, grade, path) {
  con <- file(path, "w")
  on.exit(close(con), add = TRUE)

  writeLines(c(
    "================================================================",
    sprintf("pmatools analysis - generated %s", format(Sys.time())),
    sprintf("Outcome: %s", grade$outcome_name),
    "================================================================",
    ""
  ), con)

  # After a Core GRADE 4 Fig 2 low-RoB refit the rated analysis is a subset of
  # the full one, so an unqualified "[ Meta-analysis summary ]" would let a
  # reader scanning the top of the file take a pooled estimate that the
  # certainty assessment below was not computed on. Name the analysis set in
  # the heading, and print the rated analysis too when `ma` is not it.
  if (isTRUE(grade$rob_refit)) {
    writeLines(sprintf("[ Meta-analysis summary - %s ]",
                       .results_txt_set_label(ma, grade)), con)
  } else {
    writeLines("[ Meta-analysis summary ]", con)
  }
  ma_summary <- utils::capture.output(summary(ma))
  writeLines(ma_summary, con)
  writeLines("", con)

  if (isTRUE(grade$rob_refit) && !.same_analysis_set(ma, grade$meta)) {
    writeLines(sprintf(
      "[ Meta-analysis summary - low risk of bias studies only (%d of %d studies; rated below) ]",
      grade$meta$k, grade$meta_full$k
    ), con)
    writeLines(utils::capture.output(summary(grade$meta)), con)
    writeLines("", con)
  }

  writeLines("================================================================", con)
  writeLines("[ Certainty assessment (Core GRADE series) ]", con)
  writeLines("================================================================", con)
  grade_print <- utils::capture.output(print(grade))
  writeLines(grade_print, con)
  writeLines("", con)

  writeLines("[ Domain notes ]", con)
  d <- grade$domain_assessments
  for (i in seq_len(nrow(d))) {
    if (!is.na(d$notes[i])) {
      writeLines(sprintf("- [%s] %s", d$domain[i], d$notes[i]), con)
    }
  }
  writeLines("", con)

  .safe_ver <- function(pkg, fallback = "(vendored)") {
    tryCatch(as.character(utils::packageVersion(pkg)),
             error = function(e) fallback)
  }
  writeLines("================================================================", con)
  writeLines("[ Software versions ]", con)
  writeLines("================================================================", con)
  writeLines(sprintf("pmatools : %s", .pmatools_version()), con)
  writeLines(sprintf("meta     : %s", .safe_ver("meta")), con)
  writeLines(sprintf("R        : %s", paste(R.version$major, R.version$minor, sep = ".")), con)

  invisible(path)
}

# --------------------------------------------------------------------------
# analysis.R rendering via glue
# --------------------------------------------------------------------------
.render_analysis_script <- function(ma, grade,
                                    ma_args, grade_args,
                                    per, prediction,
                                    convert_smd_to_or, baseline_risk, threshold_label,
                                    out_path,
                                    keep_effect_scale = FALSE,
                                    rare = NULL,
                                    style = "bmj",
                                    follow_up = NULL, unit = NULL,
                                    sof_notes = NULL) {

  tpl_path <- system.file("templates", "analysis_script.R.tpl",
                          package = "pmatools")
  if (!nzchar(tpl_path) || !file.exists(tpl_path)) {
    # Fallback: use source-tree path during devtools::load_all()
    tpl_path <- file.path("inst", "templates", "analysis_script.R.tpl")
  }

  tpl <- paste(readLines(tpl_path), collapse = "\n")

  ma_args    <- ma_args    %||% list()
  grade_args <- grade_args %||% list()

  # Every grade_args lookup below is exact (`[[`, not `$`), so a name that is
  # not a grade_meta() argument can never answer for one that is. Reject those
  # names outright rather than dropping them.
  .check_grade_arg_names(grade_args)

  outcome_type_ma <- if (!is.null(ma$event.e)) "binary" else "continuous"
  sm <- ma$sm %||% (if (outcome_type_ma == "binary") "OR" else "SMD")

  values <- list(
    timestamp        = format(Sys.time()),
    pmatools_version = .pmatools_version(),
    outcome_type     = outcome_type_ma,
    sm               = sm,
    # Exact lookup: "method" is a prefix of "method.tau", so a `$` read here
    # would hand the tau estimator's spec to run_ma()'s `method`.
    method_arg       = .arg_lit(ma_args[["method", exact = TRUE]],
                                                    fallback = if (outcome_type_ma == "binary")
                                                                shQuote(ma$method %||% "Inverse")
                                                              else "NULL"),
    method_tau       = ma$method.tau %||% "REML",
    random           = if (isTRUE(ma$random))     "TRUE" else "FALSE",
    common           = if (isTRUE(ma$common))     "TRUE" else "FALSE",
    # Read off method.random.ci, not the legacy `hakn` alias: the alias is what
    # {meta} keeps for back-compatibility, and a user who forced the CI method
    # in Step 2 must see that choice in the reproducibility script.
    hakn             = if (.uses_hartung_knapp(ma))  "TRUE" else "FALSE",
    prediction       = if (isTRUE(ma$prediction)) "TRUE" else "FALSE",
    incr             = ma$incr %||% 0.5,
    arm_labels_arg   = .arm_labels_arg(ma_args[["experimental_label", exact = TRUE]],
                                       ma_args[["control_label", exact = TRUE]]),
    subgroup_arg     = .subgroup_arg(ma_args[["subgroup", exact = TRUE]]),
    study_design     = grade$study_design,
    rob_arg          = .arg_lit(grade_args[["rob", exact = TRUE]],           fallback = "NULL"),
    rob_rationale_arg = .arg_lit(grade_args[["rob_rationale", exact = TRUE]], fallback = "NULL"),
    rob_some_concerns = grade_args[["rob_some_concerns", exact = TRUE]][["value"]] %||% "low",
    rob_overrides_arg = .named_chr_lit(grade_args[["rob_overrides", exact = TRUE]]),
    rob_override_rationale_arg =
      .named_chr_lit(grade_args[["rob_override_rationale", exact = TRUE]]),
    rob_dom_threshold= grade_args[["rob_dominant_threshold", exact = TRUE]][["value"]] %||% 0.55,
    # rob_refit: fall back to what the stored object actually did, so the
    # bundled script reproduces the analysis set that produced these numbers.
    rob_refit_arg    = .arg_lit(
      grade_args[["rob_refit", exact = TRUE]],
      fallback = if (identical(grade$rob_analysis_set, "low_only") &&
                     !isTRUE(grade$rob_refit)) "FALSE" else "TRUE"
    ),
    rob_inf_threshold= grade_args[["rob_inflation_threshold", exact = TRUE]][["value"]] %||%
      PMA_ROB_INFLATION_THRESHOLD,
    # small_values: read the rated object when the caller did not route it
    # through grade_args. This used to fall back to "NULL", which was the bug
    # that made the argument required: a bundle exported without grade_args
    # produced a script whose OIS was powered on the wrong side of the modest
    # RRR, so the "reproducible" analysis.R documented a different analysis
    # from the one it came from. grade_meta() requires the argument now, so the
    # object always carries it and this is a plain read, not a guess.
    small_values_arg = .arg_lit(grade_args[["small_values", exact = TRUE]],
                                fallback = .small_values_lit(grade)),
    # Indirectness: with a Core GRADE 5 subdomain table the scalar argument and
    # its rationale are derived from the recorded judgment, so the bundled
    # script reproduces (or omits) the override exactly.
    indirectness_arg = if (!is.null(grade$indirectness_subdomains)) {
      .indirectness_arg_lit(grade)
    } else {
      # Fall back to NULL, grade_meta()'s documented default, rather than to a
      # literal "no": a hardcoded scalar would read as a manual override if the
      # regenerated script were later given an indirectness_subdomains table.
      .arg_lit(grade_args[["indirectness", exact = TRUE]], fallback = "NULL")
    },
    indirectness_dom_threshold =
      grade_args[["indirectness_dominant_threshold", exact = TRUE]][["value"]] %||% 0.55,
    indirectness_rationale_arg = if (!is.null(grade$indirectness_subdomains)) {
      .indirectness_rationale_lit(grade)
    } else {
      .arg_lit(grade_args[["indirectness_rationale", exact = TRUE]], fallback = "NULL")
    },
    indirectness_subdomains_arg = .indirectness_subdomains_lit(
      grade_args[["indirectness_subdomains", exact = TRUE]] %||%
        grade$indirectness_subdomains
    ),
    inconsistency_arg= .arg_lit(grade_args[["inconsistency", exact = TRUE]], fallback = "NULL"),
    inconsistency_rationale_arg =
      .arg_lit(grade_args[["inconsistency_rationale", exact = TRUE]],          fallback = "NULL"),
    inconsistency_ci_diff_arg =
      .arg_lit(grade_args[["inconsistency_ci_diff", exact = TRUE]],            fallback = "NULL"),
    inconsistency_side_arg =
      .arg_lit(grade_args[["inconsistency_threshold_side", exact = TRUE]],     fallback = "NULL"),
    inconsistency_subgroup_arg =
      .arg_lit(grade_args[["inconsistency_subgroup_explained", exact = TRUE]], fallback = "NULL"),
    imprecision_arg  = .arg_lit(grade_args[["imprecision", exact = TRUE]],     fallback = "NULL"),
    imprecision_rationale_arg =
      .arg_lit(grade_args[["imprecision_rationale", exact = TRUE]],            fallback = "NULL"),
    threshold_arg    = .arg_lit(grade_args[["threshold", exact = TRUE]],       fallback = if (!is.null(grade$threshold)) format(grade$threshold) else "NULL"),
    threshold_scale  = grade_args[["threshold_scale", exact = TRUE]][["value"]] %||% (grade$threshold_scale %||% "auto"),
    # threshold_baseline: pin the baseline the rating was actually made with.
    # An ARD threshold is anchored to a control-arm risk, and when the reviewer
    # did not set one grade_meta() derives it with .compute_control_risk(); a
    # re-run can land on a different number (a restricted analysis set, an
    # updated dataset), which would silently re-scale the threshold and change
    # the rating. Emitting the stored resolved baseline makes the script
    # reproduce the rating that is in the bundle.
    threshold_baseline_arg = .arg_lit(
      grade_args[["threshold_baseline", exact = TRUE]],
      fallback = if (!is.null(grade$threshold_baseline) &&
                     is.numeric(grade$threshold_baseline)) {
        paste(deparse(grade$threshold_baseline), collapse = "")
      } else {
        "NULL"
      }
    ),
    threshold_type   = grade_args[["threshold_type", exact = TRUE]][["value"]] %||% (grade$threshold_type %||% "mid"),
    # require_threshold: the bundled script must reproduce the original call
    # even when it deliberately ran without a MID.
    require_threshold_arg = .arg_lit(
      grade_args[["require_threshold", exact = TRUE]],
      fallback = if (identical(grade$threshold_type, "mid") &&
                     is.null(grade$threshold)) "FALSE" else "TRUE"
    ),
    rating_target_arg = .arg_lit(
      grade_args[["rating_target", exact = TRUE]],
      fallback = if (isFALSE(grade$rating_target_auto) &&
                     !is.null(grade$rating_target)) {
        shQuote(grade$rating_target)
      } else {
        "NULL"
      }
    ),
    rating_target_rationale_arg =
      .arg_lit(grade_args[["rating_target_rationale", exact = TRUE]],
               fallback = .rating_target_rationale_lit(grade)),
    ois_outcome_type = grade$outcome_type,
    ois_events_arg   = .arg_lit(grade_args[["ois_events", exact = TRUE]], fallback = "NULL"),
    ois_n_arg        = .arg_lit(grade_args[["ois_n", exact = TRUE]],      fallback = "NULL"),
    ois_alpha_arg    = .arg_lit(grade_args[["ois_alpha", exact = TRUE]],  fallback = "0.05"),
    ois_beta_arg     = .arg_lit(grade_args[["ois_beta", exact = TRUE]],   fallback = "0.2"),
    # ois_p0: pin the control-arm rate the OIS was actually powered from, for
    # the same reason threshold_baseline is pinned above. It matters more since
    # v0.5.1, because the three control-risk arguments now inherit from one
    # another: leaving ois_p0 blank while baseline_risk is emitted as a literal
    # would let the re-run inherit the SoF baseline into the OIS, and those two
    # can legitimately be different numbers (a "metaprop" baseline_risk, or a
    # SoF drawn against a named risk group). Emitting all three closes it.
    ois_p0_arg       = .arg_lit(
      grade_args[["ois_p0", exact = TRUE]],
      fallback = if (!is.null(grade$control_risk$used$ois_p0) &&
                     is.numeric(grade$control_risk$used$ois_p0)) {
        paste(deparse(grade$control_risk$used$ois_p0), collapse = "")
      } else {
        "NULL"
      }
    ),
    ois_p1_arg       = .arg_lit(grade_args[["ois_p1", exact = TRUE]],     fallback = "NULL"),
    ois_rrr_arg      = .arg_lit(grade_args[["ois_rrr", exact = TRUE]],    fallback = "0.2"),
    ois_delta_arg    = .arg_lit(grade_args[["ois_delta", exact = TRUE]],  fallback = "NULL"),
    ois_sd_arg       = .arg_lit(grade_args[["ois_sd", exact = TRUE]],     fallback = "NULL"),
    baseline_risk_arg = .arg_lit(
      grade_args[["baseline_risk", exact = TRUE]],
      fallback = if (!is.null(grade$baseline_risk) &&
                     is.numeric(grade$baseline_risk)) {
        deparse(grade$baseline_risk)
      } else {
        "NULL"
      }
    ),
    pubias_small_industry_arg =
      .arg_lit(grade_args[["pubias_small_industry", exact = TRUE]],    fallback = "NULL"),
    pubias_funnel_arg         =
      .arg_lit(grade_args[["pubias_funnel_asymmetry", exact = TRUE]],  fallback = "NULL"),
    pubias_unpub_arg          =
      .arg_lit(grade_args[["pubias_unpublished", exact = TRUE]],       fallback = "NULL"),
    pubias_registry_arg       =
      .arg_lit(grade_args[["pubias_registry_complete", exact = TRUE]], fallback = "NULL"),
    pubias_rationale_arg      =
      .arg_lit(grade_args[["pubias_rationale", exact = TRUE]],         fallback = "NULL"),
    outcome_name     = grade$outcome_name,
    per              = per,
    sof_style        = style,
    sof_prediction   = if (isTRUE(prediction)) "TRUE" else "FALSE",
    display_args     = .display_args_str(follow_up, unit),
    sof_notes_block  = .sof_notes_block(sof_notes, "sof"),
    convert_args     = .convert_args_str(convert_smd_to_or, baseline_risk,
                                         threshold_label, keep_effect_scale),
    rare_block       = .rare_script_block(rare)
  )

  rendered <- glue::glue_data(values, tpl, .open = "{{", .close = "}}",
                              .trim = FALSE, .literal = FALSE,
                              .transformer = function(text, envir) {
                                if (text %in% names(envir)) envir[[text]]
                                else ""
                              })

  # Safety net: a literalisation helper that emits malformed R would otherwise
  # ship a bundle whose analysis.R cannot even be sourced. Fail here instead.
  .check_script_parses(rendered)

  writeLines(rendered, out_path)
  invisible(out_path)
}

# Abort when the rendered analysis.R is not syntactically valid R, quoting the
# parser message (which carries the offending line) so the faulty literal is
# findable.
.check_script_parses <- function(rendered) {
  txt <- paste(as.character(rendered), collapse = "\n")
  err <- tryCatch({
    parse(text = txt)
    NULL
  }, error = function(e) conditionMessage(e))
  if (is.null(err)) return(invisible(TRUE))

  rlang::abort(paste0(
    "The generated analysis.R is not syntactically valid R and would not be ",
    "reproducible. This is a bug in pmatools' script rendering. Parser said: ",
    err
  ))
}

.rare_script_block <- function(rare) {
  if (is.null(rare) || !inherits(rare, "pma_rare_meta")) return("")
  effect_scale <- rare$effect_scale %||% "OR"
  primary_method <- rare$primary_method %||% "BB_CR"
  paste0(
    "\n# ----- 4b. Rare-events sensitivity analyses -----\n",
    "rare <- run_rare_ma(\n",
    "  data,\n",
    "  effect_scale = ", shQuote(effect_scale), ",\n",
    "  primary_method = ", shQuote(primary_method), "\n",
    ")\n",
    "rare_diag <- rare$diagnostics\n",
    "rare_diag <- data.frame(\n",
    "  metric = names(rare_diag),\n",
    "  value = vapply(rare_diag, function(x) {\n",
    "    if (is.atomic(x) && length(x) == 1L) as.character(x) else paste(x, collapse = '; ')\n",
    "  }, character(1)),\n",
    "  stringsAsFactors = FALSE\n",
    ")\n",
    "write.csv(rare_diag, \"rare_event_diagnostics.csv\", row.names = FALSE)\n",
    "write.csv(as.data.frame(rare$method_table), \"rare_event_method_table.csv\", row.names = FALSE)\n",
    "grDevices::pdf(\"rare_event_method_forest.pdf\", width = 8, height = 5)\n",
    "plot_rare_sensitivity_forest(rare)\n",
    "grDevices::dev.off()\n"
  )
}

# Convert a {value, origin, col} spec (or plain value) to an R literal string
# Recover the rating-target rationale from the stored note so the bundled
# script reproduces a manual target override (grade_meta() requires the
# rationale whenever rating_target is supplied). The note is written by
# .resolve_rating_target() as "... | Manual override (<target>): <rationale>
# | Auto-derived target would have been: <target>."
.rating_target_rationale_lit <- function(grade) {
  if (isTRUE(grade$rating_target_auto) || is.null(grade$rating_target)) {
    return("NULL")
  }
  note <- grade$rating_target_note
  if (is.null(note) || is.na(note)) return("NULL")
  m <- regmatches(note, regexec("Manual override \\([^)]*\\): (.*?) \\| Auto-derived",
                                note))[[1]]
  if (length(m) < 2L || !nzchar(m[2])) return("NULL")
  deparse(m[2])
}

# --------------------------------------------------------------------------
# Indirectness subdomains (Core GRADE 5) -> analysis.R literals
# --------------------------------------------------------------------------

# Scalar `indirectness` literal for the bundled script: NULL when the recorded
# judgment is the worst-case subdomain default, the recorded level otherwise
# (which grade_meta() then treats as a manual override).
# The outcome direction the rating was made under, as an R literal. Every
# object grade_meta() returns carries one (it is a required argument since
# 0.5.1); an object built by hand, or unpickled from before that, does not, and
# aborting is the only honest answer -- a bundle cannot claim to reproduce a
# rating whose direction it does not know.
.small_values_lit <- function(grade) {
  sv <- grade$small_values
  if (is.character(sv) && length(sv) == 1L && !is.na(sv) && nzchar(sv)) {
    return(shQuote(sv))
  }
  rlang::abort(paste0(
    "This rated object carries no small_values, so the bundled analysis.R ",
    "cannot state the outcome direction the rating was made under, and a ",
    "script that omitted it would re-run the optimal information size on the ",
    "wrong side of the modest relative risk reduction. Re-rate the outcome ",
    "with grade_meta(..., small_values = 'desirable' | 'undesirable'), or ",
    "pass the direction through grade_args."
  ))
}

.indirectness_arg_lit <- function(grade) {
  sub <- grade$indirectness_subdomains
  if (is.null(sub) || !nrow(sub)) return(shQuote("no"))
  worst  <- .indirectness_worst_case(sub)
  actual <- .indirectness_domain_judgment(grade)
  if (identical(worst, actual)) "NULL" else shQuote(actual)
}

# Recover the override rationale from the domain notes so the regenerated call
# passes grade_meta()'s transparency gate.
.indirectness_rationale_lit <- function(grade) {
  if (is.null(grade$indirectness_subdomains)) return("NULL")
  r <- .indirectness_override_rationale(grade)
  if (is.null(r)) return("NULL")
  paste(deparse(r, width.cutoff = 500L), collapse = "")
}

# Literalise the subdomain table as a data.frame() call. Accepts a plain
# data.frame or a {value, ...} spec.
.indirectness_subdomains_lit <- function(spec) {
  df <- if (is.data.frame(spec)) {
    spec
  } else if (is.list(spec) && is.data.frame(spec$value)) {
    spec$value
  } else {
    NULL
  }
  if (is.null(df) || nrow(df) == 0) return("NULL")

  cols <- intersect(c("subdomain", "target", "evidence", "judgment"), names(df))
  if (length(cols) == 0) return("NULL")

  vec_lit <- function(v) {
    paste(deparse(as.character(v), width.cutoff = 500L), collapse = "")
  }
  lines <- paste0("    ", format(cols), " = ",
                  vapply(cols, function(cl) vec_lit(df[[cl]]), character(1)))
  paste0("data.frame(\n", paste(lines, collapse = ",\n"),
         ",\n    stringsAsFactors = FALSE\n  )")
}

# Render a *named* character vector as an R literal, e.g.
#   c('Smith 2020' = 'high', 'Jones 2019' = 'low')
# .arg_lit()'s "vector" origin drops names, which would silently break
# rob_overrides / rob_override_rationale (both keyed on studlab) in the
# bundled script. Names are quoted rather than backticked so labels with
# spaces round-trip.
.named_chr_lit <- function(spec, fallback = "NULL") {
  v <- if (is.list(spec) && !is.null(spec$origin)) spec$value else spec
  if (is.list(v)) v <- unlist(v)
  if (is.null(v) || length(v) == 0L) return(fallback)
  nms <- names(v)
  if (is.null(nms) || any(is.na(nms)) || any(!nzchar(nms))) return(fallback)
  paste0("c(",
         paste(sprintf("%s = %s", shQuote(nms), shQuote(as.character(v))),
               collapse = ", "),
         ")")
}

# The legal `grade_args` names: grade_meta()'s own formals, minus the meta
# object. Derived at call time rather than stored as a constant, both so it can
# never drift from the function and because export_bundle.R is collated before
# grade_meta.R (a top-level formals() call would fail at build time).
.grade_arg_names <- function() setdiff(names(formals(grade_meta)), "meta_obj")

# grade_args names are matched exactly (see .render_analysis_script()), so a
# name that is not a grade_meta() argument matches nothing and the argument
# never reaches the bundled analysis.R. Reject it here instead: a silently
# dropped argument is the one failure mode a "reproducible" script must not
# have.
.check_grade_arg_names <- function(grade_args) {
  if (length(grade_args) == 0L) return(invisible(TRUE))
  if (!is.list(grade_args)) {
    rlang::abort(paste0(
      "grade_args must be a named list of grade_meta() argument ",
      "specifications, not ", class(grade_args)[1], "."
    ))
  }
  nms <- names(grade_args)
  if (is.null(nms) || any(is.na(nms)) || !all(nzchar(nms))) {
    rlang::abort(paste0(
      "Every element of grade_args must be named with the grade_meta() ",
      "argument it specifies. Unnamed elements cannot be matched to an ",
      "argument and would be silently dropped from the bundled analysis.R, ",
      "so the \"reproducible\" script would reproduce a different rating."
    ))
  }
  legal <- .grade_arg_names()
  bad   <- setdiff(nms, legal)
  if (length(bad) == 0L) return(invisible(TRUE))

  described <- vapply(bad, function(b) {
    d    <- utils::adist(b, legal, ignore.case = TRUE)[1, ]
    near <- legal[d == min(d)]
    paste0("'", b, "' (closest legal name",
           if (length(near) > 1L) "s: " else ": ",
           paste(near, collapse = ", "), ")")
  }, character(1))

  rlang::abort(paste0(
    "Unknown grade_args name", if (length(bad) > 1L) "s" else "", ": ",
    paste(described, collapse = "; "),
    ". grade_args names are matched exactly against grade_meta()'s arguments. ",
    "An unrecognised name matches no argument, so it would be silently ",
    "dropped from the bundled analysis.R and the \"reproducible\" script ",
    "would reproduce a different rating."
  ))
}

# Origins understood by .arg_lit(). Anything else is a caller bug: silently
# falling through would emit `NULL` for that argument and the "reproducible"
# script would then reproduce a different analysis.
ARG_LIT_ORIGINS <- c("null", "column", "scalar", "vector")

.arg_lit <- function(spec, fallback = "NULL") {
  if (is.null(spec)) return(fallback)
  if (is.list(spec) && !is.null(spec$origin)) {
    origin <- spec$origin
    if (length(origin) != 1L || !is.character(origin) ||
        !origin %in% ARG_LIT_ORIGINS) {
      rlang::abort(paste0(
        "Unknown argument spec origin: ",
        paste(deparse(origin, width.cutoff = 500L), collapse = ""),
        ". Accepted origins are: ", paste(ARG_LIT_ORIGINS, collapse = ", "),
        ". An unrecognised origin would silently render this argument as NULL ",
        "in the bundled analysis.R and break reproducibility."
      ))
    }
    if (origin == "null") return("NULL")
    if (origin == "column") return(paste0("data$", spec$col))
    if (origin == "scalar") {
      v <- spec$value
      if (is.null(v)) return("NULL")
      if (is.character(v)) return(shQuote(v))
      if (is.logical(v))   return(as.character(v))
      if (is.numeric(v))   return(format(v))
    }
    if (origin == "vector") {
      v <- spec$value
      return(paste0("c(", paste(if (is.character(v)) shQuote(v) else v,
                                collapse = ", "), ")"))
    }
  }
  # Plain value fallback
  if (is.character(spec) && length(spec) == 1) return(shQuote(spec))
  if (is.numeric(spec)   && length(spec) == 1) return(format(spec))
  fallback
}

# Render optional run_ma() arm-label arguments (Item: alphabetical-fallback
# override). Accepts {value, ...} specs or plain strings; returns "" when
# neither label was supplied so the template line stays unchanged.
.arm_labels_arg <- function(exp_spec, ctl_spec) {
  get_val <- function(spec) {
    v <- if (is.list(spec)) spec$value else spec
    if (is.null(v) || !is.character(v) || length(v) != 1 || !nzchar(v)) {
      return(NULL)
    }
    v
  }
  e <- get_val(exp_spec)
  c_ <- get_val(ctl_spec)
  out <- ""
  if (!is.null(e)) {
    out <- paste0(out, ",\n  experimental_label = ", shQuote(e))
  }
  if (!is.null(c_)) {
    out <- paste0(out, ",\n  control_label      = ", shQuote(c_))
  }
  out
}

.subgroup_arg <- function(spec) {
  v <- if (is.list(spec)) spec$value else spec
  if (is.null(v) || identical(v, "")) return("")
  paste0(",\n  subgroup = ", shQuote(v))
}

# Normalise a presentation argument (follow_up / unit) to a single non-empty
# string or NULL. Anything else - NA, a zero-length value, a vector picked up
# from an object built by hand - would reach sof_table() as a cell of its own,
# so it is dropped here instead.
.display_arg <- function(v) {
  if (is.null(v) || length(v) != 1L) return(NULL)
  v <- as.character(v)
  if (is.na(v) || !nzchar(v)) return(NULL)
  v
}

# Render follow_up / unit as trailing sof_table() arguments for the bundled
# analysis.R. Returns "" when neither is set, so the template line is unchanged.
.display_args_str <- function(follow_up, unit) {
  out <- ""
  fu <- .display_arg(follow_up)
  un <- .display_arg(unit)
  # deparse(), not shQuote(): both are free text and an apostrophe ("patient's
  # last visit") would leave shQuote()'s single-quoted literal unparseable.
  if (!is.null(fu)) {
    out <- paste0(out, ", follow_up = ",
                  paste(deparse(fu, width.cutoff = 500L), collapse = ""))
  }
  if (!is.null(un)) {
    out <- paste0(out, ", unit = ",
                  paste(deparse(un, width.cutoff = 500L), collapse = ""))
  }
  out
}

# Render the caller's extra footnotes as a sof_add_notes() call on `obj` for
# the bundled analysis.R. Returns "" when there is nothing to append, so the
# script of a bundle without notes is byte-for-byte what it was before.
.sof_notes_block <- function(notes, obj) {
  notes <- .usable_notes(notes)
  if (length(notes) == 0L) return("")
  lits <- vapply(notes,
                 function(n) paste(deparse(n, width.cutoff = 500L),
                                   collapse = ""),
                 character(1), USE.NAMES = FALSE)
  paste0("\n", obj, " <- sof_add_notes(", obj, ", c(\n  ",
         paste(lits, collapse = ",\n  "), "\n))\n")
}

.convert_args_str <- function(convert_smd_to_or, baseline_risk,
                              threshold_label, keep_effect_scale = FALSE) {
  if (!isTRUE(convert_smd_to_or)) return("")
  parts <- ", convert_smd_to_or = TRUE"
  # Emitted only when TRUE: FALSE is the default, and a script that spells out
  # every default reads as though each one were a decision the review took.
  if (isTRUE(keep_effect_scale)) {
    parts <- paste0(parts, ", keep_effect_scale = TRUE")
  }
  if (!is.null(baseline_risk)) {
    parts <- paste0(parts, ", baseline_risk = ", format(baseline_risk))
  }
  if (!is.null(threshold_label) && nzchar(threshold_label)) {
    parts <- paste0(parts, ", threshold_label = ", shQuote(threshold_label))
  }
  parts
}

`%||%` <- function(a, b) if (is.null(a)) b else a
