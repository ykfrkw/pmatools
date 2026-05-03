# export_bundle.R - Pack analysis artifacts into a reproducible ZIP

#' Export a reproducible analysis bundle as a ZIP
#'
#' @description
#' Bundles every artifact of the analysis into a single ZIP: the long-format
#' CSV, a reproducible `analysis.R` script, results.txt, forest/funnel plots,
#' the SoF table, and the GRADE appendix. The bundled `analysis.R` runs
#' standalone with `library(pmatools)` and the bundled CSV.
#'
#' @param ma A `meta` object from \code{\link{run_ma}}.
#' @param grade A `pmatools` object from \code{\link{grade_meta}}.
#' @param output_dir Directory where the ZIP is created.
#' @param bundle_name Bundle base name (no extension).
#' @param include Which artifacts to include. See Details.
#' @param per Denominator for SoF rate columns. Default 1000.
#' @param prediction Show 95 percent prediction interval in SoF Effect column.
#' @param convert_smd_to_or Logical. Passed to \code{\link{sof_table}} for
#'   continuous-outcome dichotomisation.
#' @param baseline_risk Numeric in (0,1). Passed to \code{\link{sof_table}}
#'   when \code{convert_smd_to_or = TRUE}.
#' @param threshold_label Optional free-text label describing the
#'   dichotomisation threshold.
#' @param data Optional canonical long-format tibble from
#'   \code{\link{ingest_data}}; if provided, written to `data_long.csv`. If
#'   NULL, the function attempts to reconstruct from `ma$data`.
#' @param grade_args Optional named list of `grade_meta()` argument
#'   specifications with `value`/`origin`/`col` slots, used to render
#'   `analysis.R` faithfully. See SPEC.md.
#' @param ma_args Optional named list of `run_ma()` argument specifications.
#' @param forest_display Optional named list of arguments forwarded to
#'   \code{\link{plot_forest}} when rendering the bundled forest plot.
#'   Recognised names: `title`, `label_e`, `label_c`, `xlim`,
#'   `favors_left`, `favors_right`, `show_n`, `show_events`,
#'   `addrow_above`, `addrow_below`.
#' @param rob Optional character vector of per-study Risk-of-Bias labels
#'   (length \code{length(meta_obj$studlab)} or \code{meta_obj$k}). Required
#'   when `"forest_rob"` is in `include` to render the stratified forest plot.
#' @param forest_display_rob Optional named list of `plot_forest` arguments
#'   for the RoB-stratified forest plot bundled when `"forest_rob"` is in
#'   `include`. Same recognised names as `forest_display`.
#'
#' @return Character. Absolute path to the created ZIP file.
#'
#' @export
export_bundle <- function(ma,
                          grade,
                          output_dir   = ".",
                          bundle_name  = "pmatools_results",
                          include      = c("data", "script", "results",
                                           "forest", "forest_rob", "funnel",
                                           "grade_table"),
                          per          = 1000,
                          prediction   = FALSE,
                          convert_smd_to_or = FALSE,
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
                          forest_display_rob = NULL) {
  if (!inherits(ma, "meta")) {
    rlang::abort("export_bundle: 'ma' must be a meta object.")
  }
  if (!inherits(grade, "pmatools")) {
    rlang::abort("export_bundle: 'grade' must be a pmatools object.")
  }
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
                            script_path)
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
    png_path <- file.path(work_dir, "forest_plot.png")
    fd <- if (is.list(forest_display)) forest_display else list()
    if (is.null(fd$title) || !nzchar(fd$title %||% "")) fd$title <- grade$outcome_name
    .save_plot_pdf_png(
      function() do.call(plot_forest, c(list(meta_obj = ma), fd)),
      pdf_path, png_path,
      width = max(7, 3 + 0.3 * ma$k),
      height = max(5, 1.5 + 0.35 * ma$k)
    )
    files_in_zip <- c(files_in_zip, pdf_path, png_path)
  }

  # 4b. forest plot stratified by RoB
  if ("forest_rob" %in% include && !is.null(rob)) {
    pdf_path <- file.path(work_dir, "forest_plot_rob.pdf")
    png_path <- file.path(work_dir, "forest_plot_rob.png")
    fdr <- if (is.list(forest_display_rob)) forest_display_rob else list()
    if (is.null(fdr$title) || !nzchar(fdr$title %||% "")) {
      fdr$title <- paste0(grade$outcome_name, " (stratified by RoB)")
    }
    k_extra <- if (!is.null(ma$k)) ma$k else 0L
    .save_plot_pdf_png(
      function() do.call(plot_forest_rob,
                         c(list(meta_obj = ma, rob = rob), fdr)),
      pdf_path, png_path,
      width  = max(8, 3 + 0.3 * k_extra),
      height = max(7, 3 + 0.4 * (k_extra + 4))
    )
    files_in_zip <- c(files_in_zip, pdf_path, png_path)
  }

  # 5. funnel plot
  if ("funnel" %in% include) {
    pdf_path <- file.path(work_dir, "funnel_plot.pdf")
    png_path <- file.path(work_dir, "funnel_plot.png")
    .save_plot_pdf_png(
      function() plot_funnel(ma),
      pdf_path, png_path,
      width = 7, height = 6
    )
    files_in_zip <- c(files_in_zip, pdf_path, png_path)
  }

  # Helper: write a flextable into a landscape-orientation .docx using
  # officer directly. Avoids flextable::save_as_docx(pr_section = ...),
  # whose argument is not present in older flextable versions.
  .save_landscape_docx <- function(ft, path) {
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_end_section_landscape(doc, w = 11, h = 8.5)
    print(doc, target = path)
    invisible(path)
  }

  # 6a. grade_table.docx — GRADE Evidence Profile
  if ("grade_table" %in% include) {
    ep_ft <- evidence_profile(grade,
                              other_text      = other_text,
                              other_downgrade = other_downgrade)
    ep_path <- file.path(work_dir, "grade_table.docx")
    .save_landscape_docx(ep_ft, ep_path)
    files_in_zip <- c(files_in_zip, ep_path)

    # 6b. sof_table.docx — Summary of Findings
    sof_ft <- sof_table(grade, per = per, prediction = prediction,
                        convert_smd_to_or = convert_smd_to_or,
                        baseline_risk     = baseline_risk,
                        threshold_label   = threshold_label,
                        chinn_invert      = isTRUE(chinn_invert))
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

# --------------------------------------------------------------------------
# Plot saving (PDF + PNG)
# --------------------------------------------------------------------------
.save_plot_pdf_png <- function(draw_fn, pdf_path, png_path,
                                width = 8, height = 6) {
  grDevices::pdf(pdf_path, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_fn()
  grDevices::dev.off()
  on.exit()  # clear

  grDevices::png(png_path, width = width * 100, height = height * 100,
                 res = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_fn()
  grDevices::dev.off()
  on.exit()  # clear

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Results text
# --------------------------------------------------------------------------
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

  writeLines("[ Meta-analysis summary ]", con)
  ma_summary <- utils::capture.output(summary(ma))
  writeLines(ma_summary, con)
  writeLines("", con)

  writeLines("================================================================", con)
  writeLines("[ GRADE assessment ]", con)
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
  writeLines(sprintf("pmatools : %s", .safe_ver("pmatools", "0.3.0 (vendored)")), con)
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
                                    out_path) {

  tpl_path <- system.file("templates", "analysis_script.R.tpl",
                          package = "pmatools")
  if (!nzchar(tpl_path) || !file.exists(tpl_path)) {
    # Fallback: use source-tree path during devtools::load_all()
    tpl_path <- file.path("inst", "templates", "analysis_script.R.tpl")
  }

  tpl <- paste(readLines(tpl_path), collapse = "\n")

  ma_args    <- ma_args    %||% list()
  grade_args <- grade_args %||% list()

  outcome_type_ma <- if (!is.null(ma$event.e)) "binary" else "continuous"
  sm <- ma$sm %||% (if (outcome_type_ma == "binary") "OR" else "SMD")

  values <- list(
    timestamp        = format(Sys.time()),
    pmatools_version = tryCatch(
      as.character(utils::packageVersion("pmatools")),
      error = function(e) "0.3.0 (vendored)"
    ),
    outcome_type     = outcome_type_ma,
    sm               = sm,
    method_arg       = .arg_lit(ma_args$method,     fallback = if (outcome_type_ma == "binary")
                                                                shQuote(ma$method %||% "Inverse")
                                                              else "NULL"),
    method_tau       = ma$method.tau %||% "REML",
    random           = if (isTRUE(ma$random))     "TRUE" else "FALSE",
    common           = if (isTRUE(ma$common))     "TRUE" else "FALSE",
    hakn             = if (isTRUE(ma$hakn))       "TRUE" else "FALSE",
    prediction       = if (isTRUE(ma$prediction)) "TRUE" else "FALSE",
    incr             = ma$incr %||% 0.5,
    subgroup_arg     = .subgroup_arg(ma_args$subgroup),
    study_design     = grade$study_design,
    rob_arg          = .arg_lit(grade_args$rob,                    fallback = "NULL"),
    rob_dom_threshold= grade_args$rob_dominant_threshold$value     %||% 0.60,
    rob_inf_threshold= grade_args$rob_inflation_threshold$value    %||% 0.10,
    small_values_arg = .arg_lit(grade_args$small_values,           fallback = "NULL"),
    indirectness_arg = .arg_lit(grade_args$indirectness,           fallback = shQuote("no")),
    inconsistency_arg= .arg_lit(grade_args$inconsistency,          fallback = "NULL"),
    mid_arg          = .arg_lit(grade_args$mid,                    fallback = if (!is.null(grade$mid)) format(grade$mid) else "NULL"),
    mid_scale        = grade_args$mid_scale$value                  %||% (grade$mid_scale %||% "auto"),
    ois_outcome_type = grade$outcome_type,
    ois_p0_arg       = .arg_lit(grade_args$ois_p0,                 fallback = "NULL"),
    ois_p1_arg       = .arg_lit(grade_args$ois_p1,                 fallback = "NULL"),
    ois_delta_arg    = .arg_lit(grade_args$ois_delta,              fallback = "NULL"),
    ois_sd_arg       = .arg_lit(grade_args$ois_sd,                 fallback = "NULL"),
    pubias_small_industry_arg = .arg_lit(grade_args$pubias_small_industry,   fallback = "NULL"),
    pubias_funnel_arg         = .arg_lit(grade_args$pubias_funnel_asymmetry, fallback = "NULL"),
    pubias_unpub_arg          = .arg_lit(grade_args$pubias_unpublished,      fallback = "NULL"),
    outcome_name     = grade$outcome_name,
    per              = per,
    sof_prediction   = if (isTRUE(prediction)) "TRUE" else "FALSE",
    convert_args     = .convert_args_str(convert_smd_to_or, baseline_risk, threshold_label)
  )

  rendered <- glue::glue_data(values, tpl, .open = "{{", .close = "}}",
                              .trim = FALSE, .literal = FALSE,
                              .transformer = function(text, envir) {
                                if (text %in% names(envir)) envir[[text]]
                                else ""
                              })

  writeLines(rendered, out_path)
  invisible(out_path)
}

# Convert a {value, origin, col} spec (or plain value) to an R literal string
.arg_lit <- function(spec, fallback = "NULL") {
  if (is.null(spec)) return(fallback)
  if (is.list(spec) && !is.null(spec$origin)) {
    if (spec$origin == "null") return("NULL")
    if (spec$origin == "column") return(paste0("data$", spec$col))
    if (spec$origin == "scalar") {
      v <- spec$value
      if (is.null(v)) return("NULL")
      if (is.character(v)) return(shQuote(v))
      if (is.logical(v))   return(as.character(v))
      if (is.numeric(v))   return(format(v))
    }
    if (spec$origin == "vector") {
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

.subgroup_arg <- function(spec) {
  v <- if (is.list(spec)) spec$value else spec
  if (is.null(v) || identical(v, "")) return("")
  paste0(",\n  subgroup = ", shQuote(v))
}

.convert_args_str <- function(convert_smd_to_or, baseline_risk, threshold_label) {
  if (!isTRUE(convert_smd_to_or)) return("")
  parts <- ", convert_smd_to_or = TRUE"
  if (!is.null(baseline_risk)) {
    parts <- paste0(parts, ", baseline_risk = ", format(baseline_risk))
  }
  if (!is.null(threshold_label) && nzchar(threshold_label)) {
    parts <- paste0(parts, ", threshold_label = ", shQuote(threshold_label))
  }
  parts
}

`%||%` <- function(a, b) if (is.null(a)) b else a
