# export_bundle_multi.R - Multi-outcome ZIP layout for a pmatools_set
#
# Layout (the single-outcome bundle in export_bundle.R keeps its flat one):
#
#   summary_of_findings.docx   BMJ-style table, rows in set$order
#   summary_of_findings.csv    the same table as plain text
#   evidence_profile.docx      one evidence profile per outcome
#   analysis.R                 multi-outcome reproducibility script
#   data_long.csv              every outcome
#   README.txt                 outcome order and per-outcome analysis sets
#   outcomes/NN_slug/          one directory per outcome, numbered by set$order
#     forest_plot.*            the analysis actually rated
#     forest_plot_full.*       all studies - only when a low-RoB refit happened
#     forest_plot_rob.*        stratified by RoB - only when labels are known
#     funnel_plot.*
#     funnel_trimfill.pdf      only when k >= 10
#     pubias_missing_forest.pdf  only when k >= 10
#     results.txt
#     data_long.csv            this outcome only
#     evidence_profile.docx
#     indirectness_table.docx  only when subdomain judgments were recorded
#     rare_event_*             only when the outcome carries a rare-event fit
#
# Everything below outcomes/NN_slug/ is shaped by that outcome's own display
# arguments where it carries them (PMATOOLS_DISPLAY_ATTR, multi_outcome.R) and
# by this function's set-wide arguments otherwise.

#' Export a multi-outcome analysis bundle as a ZIP
#'
#' @description
#' The \code{\link{export_bundle}} method for a \code{pmatools_set}: the
#' summary artifacts at the top level of the ZIP and one numbered
#' `outcomes/NN_name/` directory per outcome, in the set's order.
#'
#' @details
#' An outcome added with \code{\link{add_not_reported}} keeps its numbered
#' `outcomes/NN_name/` directory so the numbering stays aligned with the set's
#' order, but the directory holds only a `results.txt` saying that no included
#' study reported the outcome: there is no analysis to plot, profile or
#' tabulate. That file, like every other per-outcome artifact, is written only
#' when `include` contains `"results"` (it does by default); an `include`
#' without it leaves the outcome no files at all, and an empty directory never
#' reaches the ZIP. It still occupies a row of `summary_of_findings.docx` / `.csv`
#' (certainty `"Not rated"`), a paragraph of `evidence_profile.docx`, and an
#' `add_not_reported()` call in the generated `analysis.R`.
#'
#' @param x A `pmatools_set` from \code{\link{grade_meta_multi}}.
#' @param output_dir Directory where the ZIP is created.
#' @param bundle_name Bundle base name (no extension).
#' @param include Which artifacts to include. Any of `"data"`, `"script"`,
#'   `"results"`, `"forest"`, `"forest_full"`, `"forest_rob"`, `"funnel"`,
#'   `"funnel_trimfill"`, `"pubias_missing_forest"`, `"sof"`,
#'   `"evidence_profile"`, `"indirectness"`, `"readme"`.
#' @param style Summary-of-findings layout, `"bmj"` (default) or
#'   `"gradepro"`. Passed to \code{\link{grade_table}} for
#'   `summary_of_findings.docx` and rendered into the bundled `analysis.R`.
#'   Same default as the single-outcome \code{\link{export_bundle.meta}}.
#'   Per-outcome `follow_up` / `unit` need no argument here: `grade_table()`
#'   reads them off the rated objects, as does the generated script.
#' @param sof_notes (v0.5.1) Optional character vector of extra footnote lines
#'   for `summary_of_findings.docx`, appended by \code{\link{sof_add_notes}}
#'   after the table's own footnotes and rendered into `analysis.R`. See
#'   \code{\link{export_bundle.meta}}.
#' @param per Denominator for SoF rate columns. Default 1000.
#' @param prediction Show 95 percent prediction interval in the Effect column.
#' @param rob Optional per-study Risk-of-Bias labels for the RoB-stratified
#'   forest plots: a named list keyed by outcome name, or a single vector used
#'   for every outcome. Defaults to the `rob` column of the set's data.
#' @param forest_display Optional named list of \code{\link{plot_forest}}
#'   display arguments applied to every outcome.
#' @param forest_display_rob Optional named list of \code{\link{plot_forest}}
#'   display arguments for the RoB-stratified forest plot.
#' @param rare Optional \code{pma_rare_meta} from \code{\link{run_rare_ma}};
#'   when supplied, each outcome's directory gets the rare-event diagnostics,
#'   the method table and the method-comparison forest plot.
#' @param rare_forest_display Optional display arguments for
#'   \code{\link{plot_rare_sensitivity_forest}}.
#' @param pubias_missing_df Optional data.frame of studies with unavailable
#'   results (columns `studlab`, `n`, `results_known`), forwarded to
#'   \code{\link{plot_forest_pubias_subgroup}}.
#' @param other_text,other_downgrade Passed to \code{\link{evidence_profile}}.
#' @param label_intervention,label_control Arm labels for the SoF table, used
#'   for its "With ..." column headers and its plain-language subject. Rendered
#'   into the bundled `analysis.R` as well, so re-running the script reproduces
#'   the headers that were exported and not the generic defaults. A label left
#'   at its default is omitted from the generated call.
#' @param ... Unused; present for S3 consistency.
#'
#' @section Per-outcome display arguments:
#' `rob`, `forest_display`, `forest_display_rob`, `rare`,
#' `rare_forest_display` and `pubias_missing_df` describe one analysis, so a
#' set assembled outcome by outcome has a different answer for each. Such a
#' caller attaches the arguments to the rated object as the `"pmatools_display"`
#' attribute — a named list holding any of `forest_display`,
#' `forest_display_rob`, `rare`, `rare_forest_display`, `pubias_missing_df` —
#' and this function reads them per outcome, falling back to the arguments
#' above where an outcome carries none. It is the same arrangement that lets
#' `follow_up` / `unit` differ per row of the summary table. An unrecognised
#' name in the attribute aborts rather than being ignored.
#'
#' The same attribute carries how a continuous outcome is presented in
#' `summary_of_findings.docx` / `.csv`: `convert_smd_to_or`, `baseline_risk`,
#' `threshold_label` and `chinn_invert`, each the \code{\link{sof_table}}
#' argument of the same name. \code{\link{grade_table}} reads them per row, so
#' one outcome can be shown as a proportion of responders while another is
#' shown as its effect, and the generated `analysis.R` re-stamps them onto the
#' set it rebuilds.
#'
#' @return Character. Absolute path to the created ZIP file.
#'
#' @seealso \code{\link{export_bundle}} for the single-outcome layout.
#'
#' @export
export_bundle.pmatools_set <- function(x,
                                       output_dir  = ".",
                                       bundle_name = "pmatools_results",
                                       include     = c("data", "script",
                                                       "results", "forest",
                                                       "forest_full",
                                                       "forest_rob", "funnel",
                                                       "funnel_trimfill",
                                                       "pubias_missing_forest",
                                                       "sof",
                                                       "evidence_profile",
                                                       "indirectness",
                                                       "readme"),
                                       style       = c("bmj", "gradepro"),
                                       sof_notes   = NULL,
                                       per         = 1000,
                                       prediction  = FALSE,
                                       rob         = NULL,
                                       forest_display     = NULL,
                                       forest_display_rob = NULL,
                                       rare                = NULL,
                                       rare_forest_display = NULL,
                                       pubias_missing_df   = NULL,
                                       other_text      = NULL,
                                       other_downgrade = 0L,
                                       label_intervention = "intervention",
                                       label_control      = "control",
                                       ...) {
  set   <- x
  style <- match.arg(style)
  include <- match.arg(include, several.ok = TRUE)
  sof_notes <- .usable_notes(sof_notes)

  if (length(set$order) == 0L) {
    rlang::abort("export_bundle: the pmatools_set holds no outcomes.")
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  work_dir <- file.path(tempdir(),
                        paste0("pmatools_set_", as.integer(Sys.time())))
  dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  rel_files <- character()   # paths relative to work_dir, in ZIP order
  add <- function(...) rel_files <<- c(rel_files, c(...))

  outcomes <- .set_outcome_list(set)
  dir_nms  <- .outcome_dir_names(set$order)

  # --- top level ------------------------------------------------------------

  if ("sof" %in% include) {
    ft <- grade_table(set, style = style, per = per, prediction = prediction,
                      label_intervention = label_intervention,
                      label_control      = label_control)
    ft <- sof_add_notes(ft, sof_notes)
    .save_landscape_docx(ft, file.path(work_dir, "summary_of_findings.docx"))
    add("summary_of_findings.docx")

    utils::write.csv(.sof_set_dataframe(set, per = per,
                                        prediction = prediction,
                                        label_intervention = label_intervention),
                     file.path(work_dir, "summary_of_findings.csv"),
                     row.names = FALSE)
    add("summary_of_findings.csv")
  }

  if ("evidence_profile" %in% include) {
    .write_set_evidence_profile(set, file.path(work_dir, "evidence_profile.docx"),
                                other_text = other_text,
                                other_downgrade = other_downgrade)
    add("evidence_profile.docx")
  }

  if ("data" %in% include && !is.null(set$data)) {
    utils::write.csv(set$data, file.path(work_dir, "data_long.csv"),
                     row.names = FALSE)
    add("data_long.csv")
  }

  if ("script" %in% include) {
    ok <- tryCatch({
      .render_analysis_script_multi(set, per = per, prediction = prediction,
                                    style = style, sof_notes = sof_notes,
                                    rare = rare,
                                    label_intervention = label_intervention,
                                    label_control      = label_control,
                                    out_path = file.path(work_dir, "analysis.R"))
      TRUE
    }, error = function(e) {
      # The offending call, not only the message: the loss is a file that is
      # quietly absent from the ZIP, and a bare "the condition has length > 1"
      # says nothing about which of the renderer's dozens of helpers raised it.
      rlang::warn(sprintf(
        paste0("The multi-outcome analysis.R could not be rendered (%s), so ",
               "the bundle is written without it. Raised by: %s"),
        conditionMessage(e),
        paste(deparse(conditionCall(e)), collapse = " ")))
      FALSE
    })
    if (ok) add("analysis.R")
  }

  # --- one directory per outcome -------------------------------------------

  for (i in seq_along(set$order)) {
    nm  <- set$order[i]
    g   <- outcomes[[nm]]
    sub_rel <- file.path("outcomes", dir_nms[i])
    sub_dir <- file.path(work_dir, sub_rel)
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)

    # An outcome nobody reported has no analysis, so there is nothing to plot,
    # profile or tabulate. The directory is still created and numbered, so the
    # outcomes/NN_slug/ numbering keeps lining up with set$order, and it gets a
    # results.txt saying why it is otherwise empty. That file is gated on
    # "results" like every other artifact of this loop: a caller who asked for
    # neither results nor any per-outcome file must not get an outcomes/ tree
    # just because one outcome is not reported. "results" is in the default
    # `include`, so the ordinary bundle still always carries the file.
    if (.is_not_reported(g)) {
      if ("results" %in% include) {
        .write_not_reported_txt(g, nm, file.path(sub_dir, "results.txt"))
        add(file.path(sub_rel, "results.txt"))
      }
      next
    }

    .check_outcome_display(g, nm)

    if ("forest" %in% include) {
      fd <- .outcome_display(g, "forest_display", forest_display)
      fd <- if (is.list(fd)) fd else list()
      if (is.null(fd$title) || !nzchar(fd$title %||% "")) fd$title <- nm
      add(.bundle_plot(function() do.call(plot_forest,
                                          c(list(meta_obj = g$meta), fd)),
                       sub_dir, sub_rel, "forest_plot",
                       width  = max(7, 3 + 0.3 * (g$meta$k %||% 0L)),
                       height = max(5, 1.5 + 0.35 * (g$meta$k %||% 0L))))
    }

    # The full-analysis forest is only meaningful when the rated analysis is a
    # subset of it, i.e. after a Core GRADE 4 Fig 2 low-RoB refit.
    if ("forest_full" %in% include && isTRUE(g$rob_refit)) {
      k_full <- g$meta_full$k %||% 0L
      add(.bundle_plot(function() plot_forest(g$meta_full,
                                              title = paste0(nm, " (all studies)")),
                       sub_dir, sub_rel, "forest_plot_full",
                       width  = max(7, 3 + 0.3 * k_full),
                       height = max(5, 1.5 + 0.35 * k_full)))
    }

    if ("forest_rob" %in% include) {
      rob_vec <- .rob_labels_for_outcome(set, nm, g, rob)
      if (!is.null(rob_vec)) {
        fdr <- .outcome_display(g, "forest_display_rob", forest_display_rob)
        fdr <- if (is.list(fdr)) fdr else list()
        if (is.null(fdr$title) || !nzchar(fdr$title %||% "")) {
          fdr$title <- paste0(nm, " (stratified by RoB)")
        }
        k_extra <- g$meta$k %||% 0L
        add(.bundle_plot(function() do.call(plot_forest_rob,
                           c(list(meta_obj = g$meta, rob = rob_vec), fdr)),
                         sub_dir, sub_rel, "forest_plot_rob",
                         width  = max(8, 3 + 0.3 * k_extra),
                         height = max(7, 3 + 0.4 * (k_extra + 4))))
      }
    }

    if ("funnel" %in% include) {
      add(.bundle_plot(function() plot_funnel(g$meta),
                       sub_dir, sub_rel, "funnel_plot",
                       width = 7, height = 6))
    }

    # Both small-study diagnostics need enough studies to say anything, so
    # they carry the same k >= 10 gate as the single-outcome bundle.
    if ("funnel_trimfill" %in% include && (g$meta$k %||% 0L) >= 10) {
      add(.bundle_plot(.trimfill_funnel_drawer(g$meta),
                       sub_dir, sub_rel, "funnel_trimfill",
                       width = 7, height = 6))
    }

    if ("pubias_missing_forest" %in% include && (g$meta$k %||% 0L) >= 10) {
      spec <- .pubias_missing_drawer(
        g$meta, .outcome_display(g, "pubias_missing_df", pubias_missing_df))
      add(.bundle_plot(spec$draw, sub_dir, sub_rel, "pubias_missing_forest",
                       width = spec$width, height = spec$height))
    }

    # Rare-event artifacts are gated on the outcome having a rare-event fit
    # rather than on `include`, exactly as in the single-outcome bundle: they
    # exist only for an outcome whose events are rare enough to have been
    # re-analysed, and that is the question `include` cannot answer.
    rare_i <- .outcome_display(g, "rare", rare)
    if (inherits(rare_i, "pma_rare_meta")) {
      add(file.path(sub_rel, .write_rare_tables(rare_i, sub_dir)))
      spec <- .rare_forest_drawer(
        rare_i, .outcome_display(g, "rare_forest_display",
                                 rare_forest_display))
      add(.bundle_plot(spec$draw, sub_dir, sub_rel,
                       "rare_event_method_forest",
                       width = spec$width, height = spec$height))
    }

    if ("results" %in% include) {
      .write_results_txt(g$meta, g, file.path(sub_dir, "results.txt"))
      add(file.path(sub_rel, "results.txt"))
    }

    if ("data" %in% include) {
      d_out <- .outcome_long_data(set, nm, g)
      if (!is.null(d_out)) {
        utils::write.csv(d_out, file.path(sub_dir, "data_long.csv"),
                         row.names = FALSE)
        add(file.path(sub_rel, "data_long.csv"))
      }
    }

    if ("evidence_profile" %in% include) {
      ep <- evidence_profile(g, other_text = .other_text(g, other_text),
                             other_downgrade = .other_downgrade(g,
                                                                other_downgrade))
      .save_landscape_docx(ep, file.path(sub_dir, "evidence_profile.docx"))
      add(file.path(sub_rel, "evidence_profile.docx"))
    }

    # indirectness_table() aborts on an object without subdomain judgments, so
    # the presence of the table is the gate, not a tryCatch.
    if ("indirectness" %in% include && !is.null(g$indirectness_subdomains)) {
      it <- indirectness_table(g)
      .save_landscape_docx(it, file.path(sub_dir, "indirectness_table.docx"))
      add(file.path(sub_rel, "indirectness_table.docx"))
    }
  }

  if ("readme" %in% include) {
    .write_set_readme(set, dir_nms, file.path(work_dir, "README.txt"),
                      rel_files)
    add("README.txt")
  }

  zip_path <- file.path(normalizePath(output_dir, mustWork = FALSE),
                        paste0(bundle_name, ".zip"))
  if (file.exists(zip_path)) file.remove(zip_path)

  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = zip_path, files = rel_files, root = work_dir)
  } else {
    old_wd <- setwd(work_dir); on.exit(setwd(old_wd), add = TRUE)
    utils::zip(zipfile = zip_path, files = rel_files)
  }

  normalizePath(zip_path)
}

# --------------------------------------------------------------------------
# Per-outcome helpers
# --------------------------------------------------------------------------

# "Other considerations" is a judgment about one body of evidence, so an
# outcome that carries its own wins over the set-wide argument - otherwise one
# outcome's note would be printed against every evidence profile in the ZIP.
.other_text <- function(g, fallback = NULL) {
  # Exact, like every other lookup on a rated object here: `$` partial-matches
  # and "other_" prefixes two fields.
  v <- g[["other_text", exact = TRUE]]
  if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(trimws(v))) {
    return(fallback)
  }
  v
}

.other_downgrade <- function(g, fallback = 0L) {
  v <- g[["other_downgrade", exact = TRUE]]
  if (is.null(v) || length(v) != 1L || is.na(v)) return(fallback)
  as.integer(v)
}

# Render one plot to PDF and return the relative path. A plot that cannot be
# drawn (funnel plots of very small analyses, for instance) must not take the
# whole bundle down, so the half-written file is removed and a warning is
# emitted instead.
.bundle_plot <- function(draw_fn, sub_dir, sub_rel, stem,
                         width = 8, height = 6) {
  pdf_path <- file.path(sub_dir, paste0(stem, ".pdf"))
  ok <- tryCatch({
    .save_plot_pdf(draw_fn, pdf_path, width = width, height = height)
    TRUE
  }, error = function(e) {
    while (grDevices::dev.cur() > 1L) grDevices::dev.off()
    rlang::warn(sprintf("Could not render %s: %s", stem, conditionMessage(e)))
    FALSE
  })
  if (!ok) {
    unlink(pdf_path)
    return(character(0))
  }
  file.path(sub_rel, paste0(stem, ".pdf"))
}

# results.txt for an outcome nobody reported: the only file its directory ever
# gets, so it has to say by itself why the directory holds nothing else.
.write_not_reported_txt <- function(g, nm, path) {
  lines <- c(
    "================================================================",
    sprintf("pmatools analysis - generated %s", format(Sys.time())),
    sprintf("Outcome: %s", nm),
    "================================================================",
    "",
    sprintf("Status: %s", .not_reported_label(g)),
    if (!is.null(g$follow_up)) sprintf("Follow-up: %s", g$follow_up),
    if (!is.null(g$reason)) sprintf("Reason: %s", g$reason),
    "",
    "This outcome was prespecified in the review, but no included study",
    "reported usable data for it. There is therefore no meta-analysis, no",
    "effect estimate, no forest or funnel plot, and no certainty rating.",
    "",
    "It is listed here, and as a row of summary_of_findings.docx, so that the",
    "bundle covers every patient-important outcome the review set out to",
    "address (Core GRADE 6)."
  )
  writeLines(lines, path)
  invisible(path)
}

# Long-format rows for one outcome: the set's data when it has them, otherwise
# a best-effort reconstruction from the meta object that was rated.
.outcome_long_data <- function(set, nm, g) {
  d <- set$data
  if (is.data.frame(d) && "outcome" %in% names(d)) {
    sub <- d[!is.na(d$outcome) & as.character(d$outcome) == nm, , drop = FALSE]
    if (nrow(sub) > 0) return(sub)
  }
  if (is.data.frame(d) && length(set$order) == 1L) return(d)
  .reconstruct_long_from_ma(g$meta)
}

# Per-study RoB labels lined up with the studies of the *rated* analysis (which
# after a low-RoB refit is a subset of the data). Returns NULL when they cannot
# be resolved, and the stratified forest plot is then skipped rather than drawn
# with mismatched labels.
.rob_labels_for_outcome <- function(set, nm, g, rob = NULL) {
  studlab <- g$meta$studlab
  if (is.null(studlab)) return(NULL)

  supplied <- if (is.list(rob) && !is.null(names(rob))) rob[[nm]] else rob
  if (!is.null(supplied)) {
    if (length(supplied) %in% c(length(studlab), g$meta$k %||% -1L)) {
      return(as.character(supplied))
    }
    return(NULL)
  }

  d <- set$data
  if (!is.data.frame(d) || !all(c("studlab", "rob") %in% names(d))) return(NULL)
  if ("outcome" %in% names(d)) {
    d <- d[!is.na(d$outcome) & as.character(d$outcome) == nm, , drop = FALSE]
  }
  if (nrow(d) == 0) return(NULL)
  lookup <- d[!duplicated(d$studlab), c("studlab", "rob"), drop = FALSE]
  out <- as.character(lookup$rob[match(as.character(studlab),
                                       as.character(lookup$studlab))])
  if (anyNA(out)) return(NULL)
  out
}

# --------------------------------------------------------------------------
# Top-level artifacts
# --------------------------------------------------------------------------

# Plain-text mirror of the summary table, one row per outcome and in set order.
# Built from the same BMJ cell helpers as the .docx so the two cannot drift.
.sof_set_dataframe <- function(set, per = 1000, prediction = FALSE,
                               label_intervention = "intervention") {
  outcomes <- .set_outcome_list(set)
  # The .docx is drawn in the BMJ layout, so the plain-text mirror resolves the
  # responder presentation with the same number formatting.
  nf <- .bmj_number_format("bmj")
  responder <- .resolve_responder(outcomes, set$order, per,
                                  big_mark = nf$big_mark, ci_sep = nf$ci_sep)
  rows <- lapply(set$order, function(nm) {
    g <- outcomes[[nm]]

    # Same columns in the same order for a not-reported outcome: a consumer
    # reading this CSV must not have to special-case its shape, only its
    # values.
    if (.is_not_reported(g)) {
      lbl <- .not_reported_label(g)
      return(data.frame(
        order            = which(set$order == nm),
        outcome          = nm,
        group            = if (nm %in% set$primary) "primary" else "secondary",
        follow_up        = g$follow_up %||% NA_character_,
        participants     = lbl,
        effect           = lbl,
        risk_control     = lbl,
        risk_intervention = lbl,
        difference       = lbl,
        certainty        = NOT_REPORTED_CERTAINTY,
        certainty_reason = g$reason %||% NA_character_,
        rating_target    = NA_character_,
        analysis_set     = "not reported",
        plain_language   = .not_reported_plain(),
        stringsAsFactors = FALSE
      ))
    }

    arm <- responder[[nm]]$arm
    v <- .bmj_row_values(nm, g, per = per, prediction = prediction,
                         follow_up = g$follow_up, unit = g$unit,
                         cer_str = arm$cer, ier_str = arm$ier,
                         label_intervention = label_intervention)
    data.frame(
      order            = which(set$order == nm),
      outcome          = nm,
      group            = if (nm %in% set$primary) "primary" else "secondary",
      follow_up        = g$follow_up %||% NA_character_,
      participants     = v$n,
      effect           = gsub("\n", " ", v$effect),
      risk_control     = gsub("\n", " ", v$cer),
      risk_intervention = gsub("\n", " ", v$ier),
      difference       = gsub("\n", " ", v$diff),
      certainty        = g$certainty,
      certainty_reason = gsub("\n", " ", sub("^[^\n]*\n?", "", v$certainty)),
      rating_target    = g$rating_target %||% NA_character_,
      analysis_set     = .analysis_set_label(g),
      plain_language   = v$plain %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# One .docx holding every outcome's evidence profile, in set order.
.write_set_evidence_profile <- function(set, path, other_text = NULL,
                                        other_downgrade = 0L) {
  outcomes <- .set_outcome_list(set)
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Evidence profiles (Core GRADE series)",
                               style = "heading 1")
  doc <- officer::body_add_par(doc, paste0(
    .PMA_CORE_GRADE_FOOTNOTE, " Generated: ",
    format(Sys.time(), "%Y-%m-%d %H:%M")), style = "Normal")

  for (nm in set$order) {
    g <- outcomes[[nm]]
    tag <- if (nm %in% set$primary) " (Primary)" else ""
    doc <- officer::body_add_par(doc, paste0(nm, tag), style = "heading 2")

    # Prose, not a table row: all five domain columns of an evidence profile
    # are judgments about a body of evidence, and there is none here.
    if (.is_not_reported(g)) {
      parts <- c(
        paste0(.not_reported_label(g), "."),
        if (!is.null(g$follow_up)) paste0("Follow-up: ", g$follow_up, "."),
        if (!is.null(g$reason)) g$reason,
        "No included study reported this outcome; no certainty rating."
      )
      doc <- officer::body_add_par(doc, paste(parts, collapse = " "),
                                   style = "Normal")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      next
    }

    note <- .rob_analysis_set_note(g)
    if (!is.null(note)) {
      doc <- officer::body_add_par(doc, paste0("Analysis set: ", note),
                                   style = "Normal")
    }
    ft <- evidence_profile(g, other_text = .other_text(g, other_text),
                           other_downgrade = .other_downgrade(g,
                                                              other_downgrade))
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }
  doc <- officer::body_end_section_landscape(doc, w = 11, h = 8.5)
  print(doc, target = path)
  invisible(path)
}

.write_set_readme <- function(set, dir_nms, path, rel_files) {
  outcomes <- .set_outcome_list(set)
  ver <- .pmatools_version()

  lines <- c(
    "pmatools multi-outcome bundle",
    sprintf("Generated: %s", format(Sys.time())),
    sprintf("pmatools version: %s", ver),
    "",
    "Outcomes, in the order used by summary_of_findings.docx and by the",
    "numbered sub-directories of outcomes/:",
    ""
  )
  for (i in seq_along(set$order)) {
    nm <- set$order[i]
    g  <- outcomes[[nm]]
    header <- sprintf("  %s  %s%s", dir_nms[i], nm,
                      if (nm %in% set$primary) "  [primary outcome]" else "")
    if (.is_not_reported(g)) {
      lines <- c(lines, header,
                 "      certainty    : not reported",
                 if (!is.null(g$reason)) sprintf("      reason       : %s",
                                                 g$reason),
                 "")
      next
    }
    lines <- c(lines, header,
      sprintf("      certainty    : %s", g$certainty),
      sprintf("      rating target: %s", g$rating_target %||% "-"),
      sprintf("      analysis set : %s", .analysis_set_label(g)),
      sprintf("      effect measure: %s", g$meta$sm %||% "-"),
      "")
  }

  # Rated outcomes only: "not reported" is not an analysis set, so it must not
  # trigger the mixed-analysis-set note below.
  sets <- unique(vapply(.rated_outcomes(outcomes), .analysis_set_label,
                        character(1)))
  if (length(sets) > 1L) {
    lines <- c(lines,
      "Note: the analysis set is not the same for every outcome. Where it reads",
      "'low RoB only', every pooled number reported for that outcome - and the",
      "domains rated from it - comes from the low risk-of-bias subset that",
      "Core GRADE 4 Fig 2 called for, not from all studies. forest_plot.pdf in",
      "that outcome's directory shows the rated analysis and",
      "forest_plot_full.pdf the full one.",
      "")
  }

  lines <- c(lines,
    "Files at the top level:",
    "  summary_of_findings.docx  summary table, all outcomes",
    "  summary_of_findings.csv   the same content as plain text",
    "  evidence_profile.docx     one evidence profile per outcome",
    "  analysis.R                re-runs the whole session from data_long.csv",
    "  data_long.csv             long-format data, all outcomes",
    "",
    "Files written under outcomes/NN_name/:",
    paste0("  ", unique(basename(grep("^outcomes/", rel_files, value = TRUE)))),
    "",
    "To reproduce: unzip, then run `Rscript analysis.R` in this directory.")

  writeLines(lines, path)
  invisible(path)
}

# --------------------------------------------------------------------------
# Multi-outcome analysis.R rendering
# --------------------------------------------------------------------------

# The set records the exact `run_ma_multi()` / `grade_meta_multi()` arguments
# it was built from, so the script re-issues those two calls rather than
# guessing per-outcome arguments back out of the rated objects. A set that was
# assembled by hand has no such record and cannot be rendered faithfully; that
# aborts, and export_bundle() then ships the bundle without a script instead of
# shipping one that reproduces something else.
.render_analysis_script_multi <- function(set, per, prediction, style,
                                          out_path, sof_notes = NULL,
                                          rare = NULL,
                                          label_intervention = "intervention",
                                          label_control      = "control") {
  if (is.null(set$grade_args)) {
    rlang::abort(paste0(
      "This pmatools_set carries no record of the grade_meta() arguments it ",
      "was built from (it was not created by grade_meta_multi()), so a ",
      "faithful multi-outcome analysis.R cannot be rendered."
    ))
  }

  tpl_path <- system.file("templates", "analysis_script_multi.R.tpl",
                          package = "pmatools")
  if (!nzchar(tpl_path) || !file.exists(tpl_path)) {
    tpl_path <- file.path("inst", "templates", "analysis_script_multi.R.tpl")
  }
  tpl <- paste(readLines(tpl_path), collapse = "\n")

  ma_args <- set$ma_args %||% list()
  # Exact lookups throughout: `$` partial-matches, so a caller-supplied name
  # could otherwise answer for a different (shorter) argument.
  dots    <- ma_args[["dots", exact = TRUE]] %||% list()

  # Common run_ma() arguments beyond outcomes/sm/outcome_type, one per line so
  # the generated call reads like a hand-written one.
  ma_extra <- if (length(dots)) {
    paste0(",\n  ",
           paste(vapply(names(dots), function(k) {
             paste0(k, " = ", .multi_arg_lit(dots[[k]]))
           }, character(1)), collapse = ",\n  "))
  } else ""

  # per_outcome is re-derived from what was actually used, minus the arguments
  # already carried by `common`, so overrides stay visible as overrides.
  common_args <- set$common %||% list()
  po_args     <- set$per_outcome %||% list()

  primary_line <- if (length(set$primary)) {
    paste0("set <- set_primary(set, ", .multi_arg_lit(set$primary), ")\n")
  } else ""

  values <- list(
    timestamp        = format(Sys.time()),
    pmatools_version = .pmatools_version(),
    outcomes_arg     = .multi_arg_lit(ma_args[["outcomes", exact = TRUE]] %||%
                                        names(.rated_outcomes(set$outcomes))),
    sm_arg           = .multi_arg_lit(ma_args[["sm", exact = TRUE]]),
    outcome_type_arg = .multi_arg_lit(ma_args[["outcome_type", exact = TRUE]]),
    ma_extra_args    = ma_extra,
    rare_block       = .rare_block_multi(set, rare),
    common_arg       = .multi_arg_lit(common_args, indent = 4L),
    per_outcome_arg  = .multi_arg_lit(po_args, indent = 4L),
    not_reported_block = .not_reported_block(set),
    responder_block  = .responder_stamp_block(set),
    order_arg        = .multi_arg_lit(set$order),
    primary_line     = primary_line,
    dir_names_arg    = .multi_arg_lit(.outcome_dir_names(set$order)),
    style            = style,
    per              = format(per),
    sof_prediction   = if (isTRUE(prediction)) "TRUE" else "FALSE",
    sof_label_args   = .sof_arm_label_args(label_intervention, label_control),
    sof_notes_block  = .sof_notes_block(sof_notes, "sof")
  )

  rendered <- glue::glue_data(values, tpl, .open = "{{", .close = "}}",
                              .trim = FALSE, .literal = FALSE,
                              .transformer = function(text, envir) {
                                if (text %in% names(envir)) envir[[text]]
                                else ""
                              })

  # Same safety net as the single-outcome renderer: never ship a bundle whose
  # analysis.R cannot be parsed.
  .check_script_parses(rendered)

  writeLines(rendered, out_path)
  invisible(out_path)
}

# Rare-event outcomes, for the generated analysis.R. run_ma_multi() pools every
# outcome with run_ma(), which drops a double-zero study; an outcome the
# reviewer analysed with the rare-event method suite was NOT rated on that
# analysis, so the script re-runs run_rare_ma() for it and substitutes its
# primary fit back into ma_list before anything is rated. Without this the
# script reproduces a different analysis for that outcome without saying so.
# Returns "" when no outcome carries a rare fit, so an ordinary bundle's script
# is byte-for-byte what it was before.
.rare_block_multi <- function(set, rare = NULL) {
  dir_nms <- .outcome_dir_names(set$order)
  lit <- function(s) paste(deparse(s, width.cutoff = 500L), collapse = "")

  blocks <- character(0)
  for (i in seq_along(set$order)) {
    nm <- set$order[i]
    r  <- .outcome_display(set$outcomes[[nm]], "rare", rare)
    if (!inherits(r, "pma_rare_meta")) next
    obj <- sprintf("rare_%02d", i)
    blocks <- c(blocks, paste0(
      "\n", obj, "_data <- data[!is.na(data$outcome) & data$outcome == ",
      lit(nm), ", , drop = FALSE]\n",
      obj, " <- run_rare_ma(\n",
      "  ", obj, "_data,\n",
      "  effect_scale = ", lit(r$effect_scale %||% "OR"), ",\n",
      "  primary_method = ", lit(r$primary_method %||% "BB_CR"), "\n",
      ")\n",
      "ma_list[[", lit(nm), "]] <- ", obj, "$primary\n",
      obj, "_dir <- file.path(\"outcomes\", ", lit(dir_nms[i]), ")\n",
      "dir.create(", obj, "_dir, recursive = TRUE, showWarnings = FALSE)\n",
      "write.csv(as.data.frame(", obj, "$method_table),\n",
      "          file.path(", obj, "_dir, \"rare_event_method_table.csv\"),\n",
      "          row.names = FALSE)\n",
      "grDevices::pdf(file.path(", obj,
      "_dir, \"rare_event_method_forest.pdf\"),\n",
      "               width = 8, height = 5)\n",
      "plot_rare_sensitivity_forest(", obj, ")\n",
      "grDevices::dev.off()\n"))
  }
  if (length(blocks) == 0L) return("")

  paste0("\n# ----- 2b. Rare-events outcomes -----\n",
         "# Pooled with the rare-event method suite, not with run_ma(): the\n",
         "# rating below was made on this fit.\n",
         paste(blocks, collapse = ""))
}

# Arm labels as trailing grade_table() arguments for the generated analysis.R.
# They name the review's own arms in the column headers ("With placebo" /
# "With CBT-I") and in the plain-language subject, so a script that omits them
# reproduces every number of summary_of_findings.docx under generic headers -
# the same silent presentation drift the responder block above exists to
# prevent. Returns "" for the grade_table() defaults, so an ordinary bundle's
# script is byte-for-byte what it was before.
.sof_arm_label_args <- function(label_intervention = "intervention",
                                label_control      = "control") {
  # deparse(), not shQuote(): the labels are free text and an apostrophe
  # ("physicians' usual care") would leave a single-quoted literal unparseable.
  lit <- function(v) paste(deparse(v, width.cutoff = 500L), collapse = "")

  parts <- character(0)
  ti <- .display_arg(label_intervention)
  tc <- .display_arg(label_control)
  if (!is.null(ti) && !identical(ti, "intervention")) {
    parts <- c(parts, paste0("label_intervention = ", lit(ti)))
  }
  if (!is.null(tc) && !identical(tc, "control")) {
    parts <- c(parts, paste0("label_control      = ", lit(tc)))
  }
  if (length(parts) == 0L) return("")

  # Aligned under the template's own grade_table() arguments.
  arg_sep <- paste0(",\n", strrep(" ", 19L))
  paste0(arg_sep, paste(parts, collapse = arg_sep))
}

# Re-stamp the responder presentation onto the rebuilt set, for the generated
# analysis.R. The grade_meta_multi() call above cannot carry it: grade_meta()
# takes none of these arguments, and its own `baseline_risk` is the control-arm
# event rate rather than the proportion of control patients who respond. The
# choice rides on the rated object as the "pmatools_display" attribute instead
# (PMATOOLS_RESPONDER_FIELDS, multi_outcome.R), and grade_table() below reads it
# per row - so without this block the script would reproduce every number of
# summary_of_findings.docx except how its continuous rows are presented.
# Returns "" when no outcome asked for the conversion, so an ordinary bundle's
# script is byte-for-byte what it was before.
#
# "_stamp_" is not decoration: the Shiny app source()s these files into one
# environment alongside its own, and shiny/R/step3_threshold.R already defines
# a `.responder_block()` (the widget). A package helper of that name is
# shadowed in the app - which is where this one runs - and the collision shows
# up only as a bundle silently missing its analysis.R.
.responder_stamp_block <- function(set) {
  lit <- function(v) paste(deparse(v, width.cutoff = 500L), collapse = "")

  calls <- character(0)
  for (nm in set$order) {
    args <- .responder_args(set$outcomes[[nm]])
    if (is.null(args)) next
    calls <- c(calls, paste0(
      "attr(set$outcomes[[", lit(nm), "]], ", lit(PMATOOLS_DISPLAY_ATTR),
      ") <- list(\n",
      "  convert_smd_to_or = TRUE,\n",
      "  baseline_risk     = ", lit(args$baseline_risk), ",\n",
      "  threshold_label   = ", lit(args$threshold_label), ",\n",
      "  chinn_invert      = ", lit(isTRUE(args$chinn_invert)), "\n",
      ")"))
  }
  if (length(calls) == 0L) return("")

  paste0("\n# Continuous outcomes the review chose to present as a proportion\n",
         "# of responders (Chinn's formula). A presentation, not a rating\n",
         "# input: grade_meta() never saw it.\n",
         paste(calls, collapse = "\n"), "\n")
}

# One add_not_reported() call per not-reported outcome, in set$order, for the
# generated analysis.R. They have to be emitted before reorder_outcomes(),
# which insists on being given every outcome of the set exactly once. Returns
# "" when the set holds none, so an ordinary bundle's script is byte-for-byte
# what it was before.
.not_reported_block <- function(set) {
  nms <- set$order[vapply(set$outcomes[set$order], .is_not_reported,
                          logical(1))]
  if (length(nms) == 0L) return("")

  lit <- function(s) paste(deparse(s, width.cutoff = 500L), collapse = "")
  calls <- vapply(nms, function(nm) {
    g <- set$outcomes[[nm]]
    args <- c(
      sprintf("set, %s", lit(nm)),
      if (!is.null(g$follow_up)) sprintf("follow_up = %s", lit(g$follow_up)),
      if (!is.null(g$reason))    sprintf("reason = %s",    lit(g$reason)),
      # Only when it differs from the default, so the common call stays short.
      if (!identical(g$label, "Not reported")) sprintf("label = %s",
                                                       lit(g$label))
    )
    sprintf("set <- add_not_reported(%s)", paste(args, collapse = ", "))
  }, character(1), USE.NAMES = FALSE)

  paste0("\n# Outcomes prespecified by the review that no included study\n",
         "# reported (Core GRADE 6).\n",
         paste(calls, collapse = "\n"), "\n")
}

# R literal for one argument value. Argument *specs* ({value, origin, col}) are
# handed to .arg_lit(), so `origin = "column"` still renders as `data$rob` and
# an unrecognised origin still aborts rather than silently becoming NULL.
.multi_arg_lit <- function(v, indent = 4L) {
  if (is.null(v)) return("NULL")
  if (is.list(v) && !is.data.frame(v) && !is.null(v$origin)) return(.arg_lit(v))
  if (is.data.frame(v)) {
    lit <- .indirectness_subdomains_lit(v)
    if (!identical(lit, "NULL")) return(lit)
    return(paste(deparse(v, width.cutoff = 500L), collapse = ""))
  }
  if (is.list(v)) return(.multi_list_lit(v, indent))
  paste(deparse(v, width.cutoff = 500L), collapse = "")
}

.multi_list_lit <- function(x, indent = 4L) {
  if (length(x) == 0L) return("list()")
  pad  <- strrep(" ", indent)
  pad0 <- strrep(" ", max(0L, indent - 2L))
  nms  <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  parts <- vapply(seq_along(x), function(i) {
    key <- if (nzchar(nms[i])) paste0(.multi_name_lit(nms[i]), " = ") else ""
    paste0(pad, key, .multi_arg_lit(x[[i]], indent = indent + 2L))
  }, character(1))
  paste0("list(\n", paste(parts, collapse = ",\n"), "\n", pad0, ")")
}

# Outcome names are free text ("Serious adverse events"), so a list name is
# quoted unless it is already a syntactic R name.
.multi_name_lit <- function(nm) {
  if (identical(make.names(nm), nm)) nm else deparse(nm)
}
