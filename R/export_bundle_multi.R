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
#     results.txt
#     data_long.csv            this outcome only
#     evidence_profile.docx
#     indirectness_table.docx  only when subdomain judgments were recorded

#' Export a multi-outcome analysis bundle as a ZIP
#'
#' @description
#' The \code{\link{export_bundle}} method for a \code{pmatools_set}: the
#' summary artifacts at the top level of the ZIP and one numbered
#' `outcomes/NN_name/` directory per outcome, in the set's order.
#'
#' @param x A `pmatools_set` from \code{\link{grade_meta_multi}}.
#' @param output_dir Directory where the ZIP is created.
#' @param bundle_name Bundle base name (no extension).
#' @param include Which artifacts to include. Any of `"data"`, `"script"`,
#'   `"results"`, `"forest"`, `"forest_full"`, `"forest_rob"`, `"funnel"`,
#'   `"sof"`, `"evidence_profile"`, `"indirectness"`, `"readme"`.
#' @param style Summary-of-findings layout, `"bmj"` (default) or
#'   `"gradepro"`. Passed to \code{\link{grade_table}} for
#'   `summary_of_findings.docx` and rendered into the bundled `analysis.R`.
#'   Same default as the single-outcome \code{\link{export_bundle.meta}}.
#'   Per-outcome `follow_up` / `unit` need no argument here: `grade_table()`
#'   reads them off the rated objects, as does the generated script.
#' @param per Denominator for SoF rate columns. Default 1000.
#' @param prediction Show 95 percent prediction interval in the Effect column.
#' @param rob Optional per-study Risk-of-Bias labels for the RoB-stratified
#'   forest plots: a named list keyed by outcome name, or a single vector used
#'   for every outcome. Defaults to the `rob` column of the set's data.
#' @param forest_display Optional named list of \code{\link{plot_forest}}
#'   display arguments applied to every outcome.
#' @param other_text,other_downgrade Passed to \code{\link{evidence_profile}}.
#' @param label_intervention,label_control Arm labels for the SoF table.
#' @param ... Unused; present for S3 consistency.
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
                                                       "sof",
                                                       "evidence_profile",
                                                       "indirectness",
                                                       "readme"),
                                       style       = c("bmj", "gradepro"),
                                       per         = 1000,
                                       prediction  = FALSE,
                                       rob         = NULL,
                                       forest_display = NULL,
                                       other_text      = NULL,
                                       other_downgrade = 0L,
                                       label_intervention = "intervention",
                                       label_control      = "control",
                                       ...) {
  set   <- x
  style <- match.arg(style)
  include <- match.arg(include, several.ok = TRUE)

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
                                    style = style,
                                    out_path = file.path(work_dir, "analysis.R"))
      TRUE
    }, error = function(e) {
      rlang::warn(sprintf(
        paste0("The multi-outcome analysis.R could not be rendered (%s); the ",
               "bundle is written without it."), conditionMessage(e)))
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

    if ("forest" %in% include) {
      fd <- if (is.list(forest_display)) forest_display else list()
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
        k_extra <- g$meta$k %||% 0L
        add(.bundle_plot(function() plot_forest_rob(
                           meta_obj = g$meta, rob = rob_vec,
                           title = paste0(nm, " (stratified by RoB)")),
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
      ep <- evidence_profile(g, other_text = other_text,
                             other_downgrade = other_downgrade)
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

# Render one plot to PDF + PNG and return the relative paths. A plot that
# cannot be drawn (funnel plots of very small analyses, for instance) must not
# take the whole bundle down, so the half-written files are removed and a
# warning is emitted instead.
.bundle_plot <- function(draw_fn, sub_dir, sub_rel, stem,
                         width = 8, height = 6) {
  pdf_path <- file.path(sub_dir, paste0(stem, ".pdf"))
  png_path <- file.path(sub_dir, paste0(stem, ".png"))
  ok <- tryCatch({
    .save_plot_pdf_png(draw_fn, pdf_path, png_path,
                       width = width, height = height)
    TRUE
  }, error = function(e) {
    while (grDevices::dev.cur() > 1L) grDevices::dev.off()
    rlang::warn(sprintf("Could not render %s: %s", stem, conditionMessage(e)))
    FALSE
  })
  if (!ok) {
    unlink(c(pdf_path, png_path))
    return(character(0))
  }
  file.path(sub_rel, paste0(stem, c(".pdf", ".png")))
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
  rows <- lapply(set$order, function(nm) {
    g <- outcomes[[nm]]
    v <- .bmj_row_values(nm, g, per = per, prediction = prediction,
                         follow_up = g$follow_up, unit = g$unit,
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
    "Reference: BMJ 2025 Core GRADE series (Guyatt et al.). ",
    "Not an official GRADE Working Group assessment. Generated: ",
    format(Sys.time(), "%Y-%m-%d %H:%M")), style = "Normal")

  for (nm in set$order) {
    g <- outcomes[[nm]]
    tag <- if (nm %in% set$primary) " (Primary)" else ""
    doc <- officer::body_add_par(doc, paste0(nm, tag), style = "heading 2")
    note <- .rob_analysis_set_note(g)
    if (!is.null(note)) {
      doc <- officer::body_add_par(doc, paste0("Analysis set: ", note),
                                   style = "Normal")
    }
    ft <- evidence_profile(g, other_text = other_text,
                           other_downgrade = other_downgrade)
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
    lines <- c(lines,
      sprintf("  %s  %s%s", dir_nms[i], nm,
              if (nm %in% set$primary) "  [primary outcome]" else ""),
      sprintf("      certainty    : %s", g$certainty),
      sprintf("      rating target: %s", g$rating_target %||% "-"),
      sprintf("      analysis set : %s", .analysis_set_label(g)),
      sprintf("      effect measure: %s", g$meta$sm %||% "-"),
      "")
  }

  sets <- unique(vapply(outcomes, .analysis_set_label, character(1)))
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
                                          out_path) {
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
                                        names(set$outcomes)),
    sm_arg           = .multi_arg_lit(ma_args[["sm", exact = TRUE]]),
    outcome_type_arg = .multi_arg_lit(ma_args[["outcome_type", exact = TRUE]]),
    ma_extra_args    = ma_extra,
    common_arg       = .multi_arg_lit(common_args, indent = 4L),
    per_outcome_arg  = .multi_arg_lit(po_args, indent = 4L),
    order_arg        = .multi_arg_lit(set$order),
    primary_line     = primary_line,
    dir_names_arg    = .multi_arg_lit(.outcome_dir_names(set$order)),
    style            = style,
    per              = format(per),
    sof_prediction   = if (isTRUE(prediction)) "TRUE" else "FALSE"
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
