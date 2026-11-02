# indirectness_table.R - the Indirectness subdomain table (Core GRADE 5,
# answered on the living Core GRADE Guide's Fig 8-2)
#
# Renders x$indirectness_subdomains (normalised by R/domain_indirectness.R) as
# a flextable: one row per subdomain with its target, the evidence found, the
# Fig 8-2 answer in words, and a mark under the leaf that answer reached (do
# not rate down / rate down once / rate down twice), plus a closing row with
# the overall domain judgment. The answer vocabulary, its levels and the
# worst-case fold all live in R/domain_indirectness.R; this file only paints
# them. export_bundle() and export_bundle_multi() save it as a .docx.
#
# ADMISSION RULE: presentation of the subdomain table only. A helper that
# decides a level belongs in R/domain_indirectness.R.

# Per-leaf colours for the three judgment columns. The "on" backgrounds are a
# green -> pale amber -> pale red gradient in the same pastel family as
# CERTAINTY_PALETTES$pastel; the "off" backgrounds are washed-out versions so
# the reached leaf stands out.
INDIRECTNESS_LEVEL_COLUMNS <- c(
  "Do not rate down" = "not_serious",
  "Rate down once"   = "serious",
  "Rate down twice"  = "very_serious"
)
INDIRECTNESS_ANSWER_COLORS <- list(
  "not_serious"  = list(on = "#d7e8d3", off = "#f3f8f2", text = "#238b21"),
  "serious"      = list(on = "#f8edd7", off = "#fdfaf3", text = "#8a6d1f"),
  "very_serious" = list(on = "#e8d0d0", off = "#faf2f2", text = "#8b0000")
)

# Colours for the "Judgment across subdomains" cell (mirrors the certainty
# pastel palette: green = no rating down, amber = 1 level, red = 2 levels).
# "extremely_serious" (-3) is a manual override only, and shares the red of
# -2 rather than inventing a fifth swatch the palette has no room for.
INDIRECTNESS_OVERALL_COLORS <- list(
  "not_serious"       = list(bg = "#d7e8d3", text = "#238b21"),
  "serious"           = list(bg = "#f8edd7", text = "#8a6d1f"),
  "very_serious"      = list(bg = "#e8d0d0", text = "#8b0000"),
  "extremely_serious" = list(bg = "#e8d0d0", text = "#8b0000")
)

# Checkbox glyphs (U+2611 = checked, U+2610 = empty); \u escapes keep the
# source ASCII-safe regardless of file encoding (same convention as
# CERTAINTY_SYMBOLS_UNICODE in house_style.R).
INDIRECTNESS_MARK_ON  <- "\u2611"
INDIRECTNESS_MARK_OFF <- "\u2610"

#' Indirectness subdomain judgment table (Core GRADE 5)
#'
#' @description
#' Renders the Population / Intervention / Comparison / Outcome subdomain
#' judgments recorded by \code{\link{grade_meta}}: one row per subdomain with
#' its target question, the evidence found, the answer given on the living
#' Core GRADE Guide's Fig 8-2 decision algorithm in words, and a mark under
#' the leaf that answer reached (do not rate down / rate down once / rate down
#' twice), plus a closing row with the overall domain judgment.
#'
#' @section Attribution:
#' The per-element questions and their answers are the Core GRADE Guide's
#' (chapter 8, Fig 8-2; see \code{\link{grade_flowcharts}}). The
#' \strong{table layout} is a \strong{pmatools} implementation; it is not a
#' Core GRADE publication table. Core GRADE 5 carries only Table 1 (an
#' adaptation of a summary of findings table) and Table 2 ("Summary of
#' indirectness issues", with the columns PICO element / Reason for rating
#' down / Examples / Likelihood of rating down).
#'
#' The \strong{worst-case fold} into the closing row is also pmatools': Fig
#' 8-2 draws four parallel branches and does not say how they combine. Answers
#' on pmatools' pre-Fig-8-2 legacy scale (\code{"yes"} /
#' \code{"probably_yes"} / \code{"probably_no"} / \code{"no"}, the answer to
#' "Is the evidence sufficiently direct?") are shown as such, under the leaf
#' their level reaches.
#'
#' Core GRADE 5 Table 2 also treats the four PICO elements
#' \emph{asymmetrically}: Population carries a "Low likelihood" of rating down,
#' Intervention "Intermediate", Comparison "Substantial", and Outcome "High
#' likelihood". The worst-case fold treats the four elements symmetrically,
#' so a dissimilar population weighs the same as a close surrogate. The table
#' footer repeats this caveat.
#'
#' @param x A \code{pmatools} object created by \code{\link{grade_meta}} with
#'   \code{indirectness_subdomains} supplied.
#' @param summary_text Optional free-text description for the
#'   "Judgment across subdomains" row. Defaults to the override rationale when
#'   the overall judgment was overridden, otherwise to a generated worst-case
#'   sentence.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{flextable} object suitable for printing, Word export, etc.
#'
#' @examples
#' \dontrun{
#' g <- grade_meta(
#'   m,
#'   threshold_type = "null",
#'   indirectness_subdomains = data.frame(
#'     subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
#'     target    = c("All patients with advanced cancer", "Heparins (any type)",
#'                   "No anticoagulation", "Symptomatic venous thromboembolism"),
#'     evidence  = c("18 RCTs involving various cancer types.",
#'                   "Both LMWH and unfractionated heparin.",
#'                   "All trials used placebo injections.",
#'                   "Screening detected asymptomatic cases too."),
#'     judgment  = c("no_difference", "similar", "no_difference",
#'                   "different_definition")
#'   )
#' )
#' indirectness_table(g)
#' flextable::save_as_docx(indirectness_table(g), path = "indirectness.docx")
#' }
#'
#' @export
indirectness_table <- function(x, summary_text = NULL, ...) {
  if (!inherits(x, "pmatools")) {
    rlang::abort("x must be a pmatools object from grade_meta().")
  }
  sub_tbl <- x$indirectness_subdomains
  if (is.null(sub_tbl) || !nrow(sub_tbl)) {
    rlang::abort(paste0(
      "This pmatools object has no Indirectness subdomain judgments. ",
      "Re-run grade_meta() with indirectness_subdomains = data.frame(",
      "subdomain = , target = , evidence = , judgment = ) to record the ",
      "Core GRADE 5 Population / Intervention / Comparison / Outcome ",
      "judgments."
    ))
  }

  overall  <- .indirectness_domain_judgment(x)
  n_sub    <- nrow(sub_tbl)
  last_row <- n_sub + 1L

  first_col <- ifelse(
    is.na(sub_tbl$target) | !nzchar(sub_tbl$target),
    sub_tbl$subdomain,
    paste0(sub_tbl$subdomain, ": ", sub_tbl$target)
  )
  desc_col <- ifelse(is.na(sub_tbl$evidence), "", sub_tbl$evidence)

  mark <- function(level) {
    ifelse(sub_tbl$grade_level == level, INDIRECTNESS_MARK_ON,
           INDIRECTNESS_MARK_OFF)
  }

  overall_label <- unname(INDIRECTNESS_OVERALL_LABELS[overall])
  if (is.na(overall_label)) overall_label <- overall

  answer_col <- unname(INDIRECTNESS_ANSWER_LABELS[sub_tbl$judgment])

  headers <- c(
    "Subdomain (target question)",
    "Description (evidence found)",
    "Answer",
    names(INDIRECTNESS_LEVEL_COLUMNS)
  )

  df <- data.frame(
    c1 = c(first_col, "Judgment across subdomains"),
    c2 = c(desc_col, .indirectness_summary_text(x, sub_tbl, overall,
                                                summary_text)),
    c3 = c(answer_col,             overall_label),
    c4 = c(mark("not_serious"),    ""),
    c5 = c(mark("serious"),        ""),
    c6 = c(mark("very_serious"),   ""),
    stringsAsFactors = FALSE
  )
  names(df) <- headers

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft, .list = stats::setNames(as.list(headers), headers)
  )
  ft <- flextable::add_header_row(
    ft,
    values    = c("Subdomain (target question)",
                  "Description (evidence found)",
                  "Judgment (Core GRADE Guide Fig 8-2)"),
    colwidths = c(1, 1, 4),
    top       = TRUE
  )
  ft <- flextable::merge_at(ft, i = 1:2, j = 1, part = "header")
  ft <- flextable::merge_at(ft, i = 1:2, j = 2, part = "header")
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = .PMA_TABLE_FONT, part = "all")

  ft <- flextable::bg(ft,    bg = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft,  part = "header")
  ft <- flextable::align(ft, align = "center", part = "header")

  ft <- flextable::align(ft, j = 1:2, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 3, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 4:6, align = "center", part = "body")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::bold(ft, j = 1, part = "body")

  # Leaf cells: gradient background, marker on the leaf each answer reached.
  for (jj in seq_along(INDIRECTNESS_LEVEL_COLUMNS)) {
    level <- INDIRECTNESS_LEVEL_COLUMNS[[jj]]
    col   <- jj + 3L
    cols  <- INDIRECTNESS_ANSWER_COLORS[[level]]
    for (i in seq_len(n_sub)) {
      selected <- identical(sub_tbl$grade_level[i], level)
      ft <- flextable::bg(ft, i = i, j = col,
                          bg = if (selected) cols$on else cols$off,
                          part = "body")
      ft <- flextable::color(ft, i = i, j = col,
                             color = if (selected) cols$text else "#999999",
                             part = "body")
      if (selected) ft <- flextable::bold(ft, i = i, j = col, part = "body")
    }
  }

  # Closing row: merged judgment cells carrying the overall domain judgment.
  ft <- flextable::merge_at(ft, i = last_row, j = 3:6, part = "body")
  ov_cols <- INDIRECTNESS_OVERALL_COLORS[[overall]] %||%
    list(bg = "#eeeeee", text = "#333333")
  ft <- flextable::bg(ft,    i = last_row, j = 3:6, bg = ov_cols$bg,
                      part = "body")
  ft <- flextable::color(ft, i = last_row, j = 3:6, color = ov_cols$text,
                         part = "body")
  ft <- flextable::bold(ft,  i = last_row, j = 3:6, part = "body")
  ft <- flextable::align(ft, i = last_row, j = 3:6, align = "center",
                         part = "body")
  ft <- flextable::bg(ft, i = last_row, j = 1:2, bg = "#f2f2f2", part = "body")

  ft <- flextable::width(ft, j = 1, width = 1.7)
  ft <- flextable::width(ft, j = 2, width = 2.4)
  ft <- flextable::width(ft, j = 3, width = 1.6)
  for (j in 4:6) ft <- flextable::width(ft, j = j, width = 0.8)

  ft <- flextable::add_footer_lines(ft, values = c(
    paste0(
      "Indirectness subdomain judgments, implemented by pmatools from the ",
      "per-PICO reasoning in ", .core_grade_ref(5), " and the decision ",
      "algorithm in Fig 8-2 of the living Core GRADE Guide (chapter 8). ",
      "Not an official GRADE Working Group assessment. ",
      INDIRECTNESS_MARK_ON, " marks the recorded judgment: the leaf of Fig ",
      "8-2 each answer reached (rate down once = 1 level; rate down twice ",
      "rates down 2 levels). The overall judgment defaults to the worst case ",
      "across subdomains."
    ),
    paste0(
      "Core GRADE 5 Table 2 grades the four PICO elements asymmetrically ",
      "(Population: 'Low likelihood' of rating down; Intervention: ",
      "'Intermediate'; Comparison: 'Substantial'; Outcome: 'High likelihood'). ",
      "The worst-case fold used here is symmetric, so a concern recorded ",
      "against Population weighs as much as one against Outcome. Rating down ",
      "two levels is, per Core GRADE 5, 'typically more salient for surrogate ",
      "outcomes'."
    )
  ))
  ft <- .style_table_footer(ft)

  ft
}

# Recorded Indirectness domain judgment (may differ from the worst case when
# the user overrode it).
.indirectness_domain_judgment <- function(x) {
  d <- x$domain_assessments
  row <- d[d$domain == "Indirectness", ]
  if (nrow(row) == 0) {
    return(.indirectness_worst_case(x$indirectness_subdomains))
  }
  row$judgment[1]
}

# Free text for the "Judgment across subdomains" description cell.
.indirectness_summary_text <- function(x, sub_tbl, overall, summary_text) {
  if (!is.null(summary_text) && nzchar(trimws(summary_text))) {
    return(trimws(summary_text))
  }
  rationale <- .indirectness_override_rationale(x)
  if (!is.null(rationale)) return(rationale)

  worst   <- .indirectness_worst_case(sub_tbl)
  drivers <- sub_tbl$subdomain[sub_tbl$grade_level == worst]
  if (identical(worst, "not_serious")) {
    return(paste0(
      "No subdomain reached a rate-down leaf, so the evidence is not rated ",
      "down for indirectness."
    ))
  }
  paste0(
    "Worst case across subdomains: ",
    paste(drivers, collapse = ", "),
    " raise(s) indirectness concerns."
  )
}

# Recover the override rationale recorded by make_domain_row() as
# "Manual override (<judgment>): <rationale>" at the head of the domain notes.
.indirectness_override_rationale <- function(x) {
  d <- x$domain_assessments
  row <- d[d$domain == "Indirectness", ]
  if (nrow(row) == 0) return(NULL)
  notes <- row$notes[1]
  if (is.na(notes) || !nzchar(notes)) return(NULL)
  m <- regmatches(notes,
                  regexec("^Manual override \\([^)]*\\): (.*)$", notes))[[1]]
  if (length(m) < 2L) return(NULL)
  out <- strsplit(m[2], " | ", fixed = TRUE)[[1]][1]
  if (!nzchar(out)) return(NULL)
  out
}
