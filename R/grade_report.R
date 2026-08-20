# grade_report.R — Appendix 用 GRADE レポート生成
#
# grade_report():
#   複数の pmatools オブジェクトから、Appendix
#   にそのまま使えるレポートを生成する。
#   含まれる内容:
#     1. Summary of Findings テーブル（grade_table）
#     2. 各ドメインの判断根拠（ドメイン別詳細テキスト）
#   出力形式: "docx" / "html" / "pdf" / "md"
#
# 依存: officer (docx), rmarkdown (html/pdf/md), flextable

#' Generate a certainty-of-evidence appendix report (Core GRADE series)
#'
#' @param outcomes A named list of \code{pmatools} objects, or a
#'   \code{pmatools_set} from \code{\link{grade_meta_multi}} (its order and
#'   primary outcomes are then used).
#' @param primary Character vector of primary outcome names (passed to \code{grade_table}).
#' @param palette Color palette: \code{"pastel"} (default) or \code{"classic"}.
#' @param style (v0.5) Summary-of-findings layout passed to
#'   \code{\link{grade_table}}: \code{"gradepro"} (default) or \code{"bmj"}.
#' @param format Output format(s): one or more of \code{"docx"}, \code{"html"},
#'   \code{"pdf"}, \code{"md"}. Default \code{"docx"}.
#' @param output_dir Directory for output files. Default \code{getwd()}.
#' @param output_file Base filename without extension. Default \code{"grade_report"}.
#' @param title Report title. Default
#'   \code{"Certainty of Evidence Assessment (Core GRADE series)"}.
#' @param show_domains Logical. Show per-domain columns in the SoF table (default TRUE).
#' @param per Denominator for SoF rate columns (default 1000).
#' @param prediction Logical. Show 95 percent prediction interval in the Effect column
#'   (default FALSE).
#' @param label_intervention,label_control Arm labels for the
#'   "Risk with ..." columns of the SoF flextable (passed to
#'   \code{grade_table}). Defaults are \code{"intervention"} and
#'   \code{"control"}.
#'
#' @return Invisibly returns a character vector of output file paths.
#'
#' @examples
#' \dontrun{
#' grade_report(
#'   outcomes = list("Depression response" = g1, "Insomnia remission" = g2),
#'   primary  = "Depression response",
#'   format   = c("docx", "html"),
#'   output_dir = "outputs/"
#' )
#' }
#'
#' @export
grade_report <- function(outcomes,
                         primary      = NULL,
                         palette      = c("pastel", "classic"),
                         style        = c("gradepro", "bmj"),
                         format       = "docx",
                         output_dir   = getwd(),
                         output_file  = "grade_report",
                         title        = "Certainty of Evidence Assessment (Core GRADE series)",
                         show_domains = TRUE,
                         per          = 1000,
                         prediction   = FALSE,
                         label_intervention = "intervention",
                         label_control      = "control") {

  if (inherits(outcomes, "pmatools_set")) {
    set      <- outcomes
    outcomes <- .set_outcome_list(set)
    if (is.null(primary) && length(set$primary) > 0) primary <- set$primary
  }
  ok_element <- if (is.list(outcomes)) {
    vapply(outcomes, function(g) {
      inherits(g, "pmatools") || .is_not_reported(g)
    }, logical(1))
  } else FALSE
  if (!is.list(outcomes) || !all(ok_element)) {
    rlang::abort(paste0(
      "outcomes must be a named list of pmatools objects from grade_meta(), ",
      "or pmatools_not_reported objects from not_reported_outcome()."))
  }
  palette <- match.arg(palette)
  style   <- match.arg(style)
  format  <- match.arg(format, choices = c("docx", "html", "pdf", "md"),
                       several.ok = TRUE)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  out_paths <- character(0)

  md_content <- .build_report_md(outcomes, primary, title, show_domains, per,
                                 prediction, label_intervention, label_control)

  for (fmt in format) {
    base_path <- file.path(output_dir, paste0(output_file, ".", fmt))

    if (fmt == "md") {
      writeLines(md_content, con = base_path)
      out_paths <- c(out_paths, base_path)
      message("Written: ", base_path)

    } else if (fmt %in% c("html", "pdf")) {
      .check_pkg("rmarkdown")
      tmp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(c(
        "---",
        paste0('title: "', title, '"'),
        paste0('output: ', if (fmt == "html") 'html_document' else 'pdf_document'),
        "---",
        "",
        md_content
      ), con = tmp_rmd)
      rmarkdown::render(
        input       = tmp_rmd,
        output_file = base_path,
        quiet       = TRUE
      )
      out_paths <- c(out_paths, base_path)
      message("Written: ", base_path)

    } else if (fmt == "docx") {
      .check_pkg("officer")
      .check_pkg("flextable")
      out_paths <- c(out_paths,
        .write_docx(outcomes, primary, palette, title, show_domains,
                    per, prediction, base_path,
                    style              = style,
                    label_intervention = label_intervention,
                    label_control      = label_control))
      message("Written: ", base_path)
    }
  }

  invisible(out_paths)
}

# --------------------------------------------------------------------------
# Markdown report builder
# --------------------------------------------------------------------------
.build_report_md <- function(outcomes, primary, title, show_domains,
                              per = 1000, prediction = FALSE,
                              label_intervention = "intervention",
                              label_control = "control") {
  nms <- names(outcomes)
  if (is.null(nms)) nms <- vapply(outcomes, function(g) g$outcome_name, character(1))

  prim_nms <- if (!is.null(primary)) nms[nms %in% primary]  else character(0)
  sec_nms  <- if (!is.null(primary)) nms[!nms %in% primary] else character(0)

  prim_lbl <- if (length(prim_nms) == 1L) "**Primary outcome**" else "**Primary outcomes**"
  sec_lbl  <- if (length(sec_nms)  == 1L) "**Secondary outcome**" else "**Secondary outcomes**"

  lines <- c(
    paste0("# ", title),
    "",
    paste0("> Generated by `pmatools` package. ",
           .PMA_CORE_GRADE_FOOTNOTE),
    "",
    "## Summary of Findings",
    "",
    "*See Word/HTML output for the full color-coded flextable.*",
    "",
    .md_sof_table(outcomes, primary, nms, prim_lbl, sec_lbl, per, prediction,
                  label_intervention, label_control),
    "",
    "## Domain-by-Domain Rationale",
    ""
  )

  for (i in seq_along(outcomes)) {
    nm <- nms[i]
    g  <- outcomes[[i]]
    group_tag <- if (!is.null(primary)) {
      if (nm %in% primary) " *(Primary)*" else " *(Secondary)*"
    } else ""

    # A not-reported outcome has no domains to tabulate, so the section is a
    # short paragraph instead of the domain table.
    if (.is_not_reported(g)) {
      parts <- c(
        paste0("**", .not_reported_label(g), ".**"),
        if (!is.null(g$follow_up)) paste0("Follow-up: ", g$follow_up, "."),
        if (!is.null(g$reason)) g$reason,
        "No included study reported this outcome; no certainty rating."
      )
      lines <- c(lines,
                 paste0("### ", nm, group_tag),
                 "",
                 paste(parts, collapse = " "),
                 "")
      next
    }

    lines <- c(
      lines,
      paste0("### ", nm, group_tag),
      "",
      sprintf(
        "**Final certainty: %s %s** (starting: %s, study design: %s)",
        g$certainty, CERTAINTY_SYMBOLS[[g$certainty]],
        g$starting_quality, g$study_design
      ),
      ""
    )

    # Core GRADE 4 Fig 2 analysis set: a low-RoB refit changes every number
    # reported for this outcome, so the note travels with the outcome.
    rob_set_note <- .rob_analysis_set_note(g)
    if (!is.null(rob_set_note)) {
      lines <- c(lines, paste0("*Analysis set: ", rob_set_note, "*"), "")
    }

    lines <- c(
      lines,
      "| Domain | Judgment | Downgrade | Notes |",
      "|--------|----------|-----------|-------|"
    )

    d <- g$domain_assessments
    for (j in seq_len(nrow(d))) {
      row <- d[j, ]
      dg  <- if (row$downgrade < 0) as.character(row$downgrade) else "0"
      note_clean <- gsub("\\|", "\\\\|", if (is.na(row$notes)) "" else row$notes)
      lines <- c(lines,
        sprintf("| %s | %s | %s | %s |",
                row$domain, row$judgment, dg, note_clean))
    }
    lines <- c(lines, "")
  }

  lines <- c(lines,
    "---",
    "",
    paste0("*Report generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "*")
  )

  paste(lines, collapse = "\n")
}

# --------------------------------------------------------------------------
# Plain-text SoF table for Markdown
# --------------------------------------------------------------------------
.md_sof_table <- function(outcomes, primary, nms, prim_lbl, sec_lbl,
                           per = 1000, prediction = FALSE,
                           label_intervention = "intervention",
                           label_control = "control") {
  per_str <- format(per, big.mark = ",", scientific = FALSE)

  # A continuous outcome leaves the two arm columns empty, so the header drops
  # the rate wording as soon as one appears: "Control rate (per 1,000)" over an
  # empty cell describes a rate the row never had.
  arms <- lapply(outcomes, function(g) {
    .sof_arm_cells(g$meta, g$baseline_risk, per)
  })
  cont_any <- any(vapply(arms, function(a) isTRUE(a$continuous), logical(1)))
  # Same arm words as every other output. The .docx branch has threaded the
  # two labels through grade_table() since they existed; this branch never
  # received them, so a review whose arms were "CBT-I" and "placebo" got a
  # markdown table headed "Intervention rate" over its own numbers.
  arm_hdr  <- if (cont_any) {
    paste0("| With ", label_control, " | With ", label_intervention, " ")
  } else {
    paste0("| ", .arm_label_cap(label_control), " rate (per ", per_str,
           ") | ", .arm_label_cap(label_intervention), " rate (per ",
           per_str, ") ")
  }
  hdr <- paste0("| Outcome | k | N ", arm_hdr, "| Effect (95% CI) | Certainty |")
  sep <- "|---|---|---|---|---|---|---|"

  # Group label rows are emitted the same way for every outcome, so the
  # not-reported branch below only replaces the value cells.
  group_label <- function(i, nm) {
    if (is.null(primary)) return(character(0))
    if (i == 1L || (nm %in% primary) != (nms[i - 1L] %in% primary)) {
      label <- if (nm %in% primary) prim_lbl else sec_lbl
      return(paste0("| ", label, " | | | | | | |"))
    }
    character(0)
  }

  rows <- c()
  for (i in seq_along(outcomes)) {
    nm <- nms[i]
    g  <- outcomes[[i]]

    # Branch before sprintf("%d", k): k is NULL here and would error.
    if (.is_not_reported(g)) {
      lbl  <- .not_reported_label(g)
      rows <- c(rows, group_label(i, nm),
                sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                        nm, lbl, lbl, lbl, lbl, lbl,
                        NOT_REPORTED_CERTAINTY))
      next
    }

    k  <- g$meta$k
    n  <- .total_n(g$meta)
    cer  <- gsub("\n", " ", arms[[i]]$cer)
    ier  <- gsub("\n", " ", arms[[i]]$ier)
    eff  <- gsub("\n", " ", .format_effect(g$meta, g$outcome_type, prediction))
    cert <- paste0(g$certainty, " ", CERTAINTY_SYMBOLS[[g$certainty]])

    rows <- c(rows, group_label(i, nm))

    rows <- c(rows, sprintf("| %s | %d | %s | %s | %s | %s | %s |",
      nm, k,
      if (is.na(n)) "NR" else format(n, big.mark = ","),
      cer, ier, eff, cert))
  }

  c(hdr, sep, rows)
}

# --------------------------------------------------------------------------
# Word (docx) output using officer + flextable
# --------------------------------------------------------------------------
.write_docx <- function(outcomes, primary, palette, title, show_domains,
                         per, prediction, path,
                         style              = "gradepro",
                         label_intervention = "intervention",
                         label_control      = "control") {
  doc <- officer::read_docx()

  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- officer::body_add_par(doc, paste0(
    .PMA_CORE_GRADE_FOOTNOTE,
    " Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")
  ), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  doc <- officer::body_add_par(doc, "Summary of Findings", style = "heading 2")

  ft <- grade_table(outcomes, primary = primary, palette = palette,
                    style = style,
                    show_domains = show_domains, per = per,
                    prediction = prediction,
                    label_intervention = label_intervention,
                    label_control      = label_control)
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(doc, "", style = "Normal")

  doc <- officer::body_add_par(doc, "Domain-by-Domain Rationale", style = "heading 2")

  nms <- names(outcomes)
  if (is.null(nms)) nms <- vapply(outcomes, function(g) g$outcome_name, character(1))

  for (i in seq_along(outcomes)) {
    nm <- nms[i]
    g  <- outcomes[[i]]
    group_tag <- if (!is.null(primary)) {
      if (nm %in% primary) " (Primary)" else " (Secondary)"
    } else ""

    doc <- officer::body_add_par(doc, paste0(nm, group_tag), style = "heading 3")

    # Same treatment as the Markdown report: no domain table, because there is
    # no body of evidence whose domains could be judged.
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

    doc <- officer::body_add_par(doc, sprintf(
      "Final certainty: %s %s  |  Starting: %s  |  Design: %s",
      g$certainty, CERTAINTY_SYMBOLS[[g$certainty]],
      g$starting_quality, g$study_design
    ), style = "Normal")

    rob_set_note <- .rob_analysis_set_note(g)
    if (!is.null(rob_set_note)) {
      doc <- officer::body_add_par(doc, paste0("Analysis set: ", rob_set_note),
                                   style = "Normal")
    }

    d <- g$domain_assessments
    detail_df <- data.frame(
      Domain    = d$domain,
      Judgment  = d$judgment,
      Downgrade = as.character(d$downgrade),
      Notes     = ifelse(is.na(d$notes), "", d$notes),
      stringsAsFactors = FALSE
    )
    ft_detail <- flextable::flextable(detail_df)
    ft_detail <- flextable::theme_vanilla(ft_detail)
    ft_detail <- flextable::fontsize(ft_detail, size = 9, part = "all")
    ft_detail <- flextable::font(ft_detail, fontname = .PMA_TABLE_FONT,
                                 part = "all")
    ft_detail <- flextable::width(ft_detail, j = 1, width = 1.2)
    ft_detail <- flextable::width(ft_detail, j = 2, width = 1.0)
    ft_detail <- flextable::width(ft_detail, j = 3, width = 0.8)
    ft_detail <- flextable::width(ft_detail, j = 4, width = 4.0)
    ft_detail <- flextable::bg(ft_detail,   bg = "#F0F0F0", part = "header")
    ft_detail <- flextable::bold(ft_detail, part = "header")
    doc <- flextable::body_add_flextable(doc, ft_detail)
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  print(doc, target = path)
  path
}

# --------------------------------------------------------------------------
# Package check helper
# --------------------------------------------------------------------------
.check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(sprintf(
      "Package '%s' is required for this output format. Install with: install.packages('%s')",
      pkg, pkg
    ))
  }
}
