# grade_table.R — 複数アウトカム GRADE テーブル

#' Summary of Findings table for multiple outcomes
#'
#' @param outcomes A named list of \code{pmatools} objects, one per outcome
#'   (names become the outcome labels), or a \code{pmatools_set} from
#'   \code{\link{grade_meta_multi}}, in which case its stored order and primary
#'   outcomes are used.
#' @param primary Character vector of outcome names that are classified as
#'   primary outcomes. All others are treated as secondary.
#'   If \code{NULL} (default), no grouping header is added — except for a
#'   \code{pmatools_set}, which supplies its own.
#' @param style (v0.5) Table layout: \code{"gradepro"} (default) or
#'   \code{"bmj"}. See \code{\link{sof_table}}.
#' @param palette Color palette for certainty cells.
#'   \code{"pastel"} (default) uses soft backgrounds with colored text.
#'   \code{"classic"} uses saturated backgrounds with white text.
#' @param show_domains Logical (default \code{TRUE}). Add domain symbol columns.
#'   Ignored by the \code{"bmj"} style, which has no domain columns.
#' @param follow_up (v0.5) Follow-up / time frame text for the \code{"bmj"}
#'   style: a character vector named by outcome, or a single unnamed value
#'   applied to every outcome. \code{NULL} (default) omits the line.
#' @param unit (v0.5) Unit for the Difference column of the \code{"bmj"} style
#'   with continuous outcomes; same named-vector convention as
#'   \code{follow_up}.
#' @param per Denominator for SoF rate columns. \code{1000} (default) or
#'   \code{100}.
#' @param prediction Logical (default \code{FALSE}); when \code{TRUE}, the
#'   95 percent prediction interval is shown in the Effect column.
#' @param label_intervention,label_control Arm labels used in the
#'   "Risk with ..." column headers (GRADEpro vocabulary).
#'   Defaults are \code{"intervention"} and \code{"control"}.
#'
#' @return A \code{flextable} object.
#'
#' @examples
#' \dontrun{
#' g1 <- grade_meta(m1, outcome_name = "Depression response")
#' g2 <- grade_meta(m2, outcome_name = "Insomnia remission")
#' grade_table(
#'   list("Depression response" = g1, "Insomnia remission" = g2),
#'   primary = "Depression response"
#' )
#' }
#'
#' @export
grade_table <- function(outcomes,
                        primary      = NULL,
                        style        = c("gradepro", "bmj"),
                        palette      = c("pastel", "classic"),
                        show_domains = TRUE,
                        per          = 1000,
                        prediction   = FALSE,
                        follow_up    = NULL,
                        unit         = NULL,
                        label_intervention = "intervention",
                        label_control      = "control") {
  # A pmatools_set carries its own row order and primary outcomes; unwrap it to
  # the named-list form the rest of this function (and the v0.4 API) uses.
  if (inherits(outcomes, "pmatools_set")) {
    set      <- outcomes
    outcomes <- .set_outcome_list(set)
    if (is.null(primary) && length(set$primary) > 0) primary <- set$primary
  }
  if (!is.list(outcomes) || length(outcomes) == 0) {
    rlang::abort("outcomes must be a non-empty named list of pmatools objects.")
  }
  if (!all(vapply(outcomes, inherits, logical(1), "pmatools"))) {
    rlang::abort("All elements of outcomes must be pmatools objects.")
  }

  style   <- match.arg(style)
  palette <- match.arg(palette)
  pal     <- CERTAINTY_PALETTES[[palette]]

  # Ensure names exist
  nms <- names(outcomes)
  if (is.null(nms) || any(nms == "")) {
    for (i in seq_along(outcomes)) {
      if (is.null(nms) || nms[i] == "") nms[i] <- outcomes[[i]]$outcome_name
    }
    names(outcomes) <- nms
  }

  # Partition into primary / secondary
  if (!is.null(primary)) {
    prim_nms <- nms[nms %in% primary]
    sec_nms  <- nms[!nms %in% primary]
  } else {
    prim_nms <- character(0)
    sec_nms  <- character(0)
  }

  # Risk-of-bias analysis set (Core GRADE 4 Fig 2). The set can differ between
  # outcomes, so the note is attached to the row it applies to via a numbered
  # marker rather than stated once for the whole table.
  rob_notes  <- character(0)
  rob_marker <- stats::setNames(rep(NA_integer_, length(nms)), nms)
  for (nm in nms) {
    note <- .rob_analysis_set_note(outcomes[[nm]])
    if (!is.null(note)) {
      rob_notes <- c(rob_notes, note)
      rob_marker[[nm]] <- length(rob_notes)
    }
  }
  disp <- function(nm) {
    if (is.na(rob_marker[[nm]])) nm else paste0(nm, " [", rob_marker[[nm]], "]")
  }

  # Domain-fact footnotes share the analysis-set register rather than starting
  # a second one, so a reader never sees two different [1]s in one footer.
  # They are numbered here, once, and consumed by whichever style renders.
  fact_counter <- length(rob_notes)
  fact_notes   <- character(0)
  fact_markers <- list()
  for (nm in nms) {
    mk <- integer(0)
    for (dm in .rated_down_fact_domains(outcomes[[nm]])) {
      note <- .domain_fact_note(outcomes[[nm]], dm, outcome_name = nm)
      if (is.null(note)) next
      fact_counter <- fact_counter + 1L
      fact_notes   <- c(fact_notes, sprintf("[%d] %s", fact_counter, note))
      mk[[dm]]     <- fact_counter
    }
    if (length(mk) > 0L) fact_markers[[nm]] <- mk
  }

  if (identical(style, "bmj")) {
    # Per-outcome follow-up / unit ride on the rated objects themselves when
    # grade_meta_multi() was given them, so a multi-outcome caller does not
    # have to re-assemble a parallel named vector here.
    follow_up <- follow_up %||% .display_arg_from_outcomes(outcomes, "follow_up")
    unit      <- unit      %||% .display_arg_from_outcomes(outcomes, "unit")

    ft <- .grade_table_bmj(
      outcomes, nms = nms, prim_nms = prim_nms, sec_nms = sec_nms,
      primary = primary, pal = pal, per = per, prediction = prediction,
      follow_up = follow_up, unit = unit,
      label_intervention = label_intervention,
      label_control      = label_control,
      disp = disp, rob_notes = rob_notes,
      fact_notes = fact_notes, fact_markers = fact_markers
    )
    # Mixed effect measures (the norm once binary and continuous outcomes share
    # a table) leave the BMJ header generic. Each cell still spells its own
    # measure out ("Odds ratio 0.62 ..."), and the footnote says so, so nothing
    # is left to be inferred from the header.
    sms <- unique(vapply(outcomes,
                         function(g) as.character(g$meta$sm %||% ""),
                         character(1)))
    sms <- sms[nzchar(sms)]
    if (length(sms) > 1L) {
      ft <- flextable::add_footer_lines(ft, values = paste0(
        "Outcomes are reported on different effect measures (",
        paste(sms, collapse = ", "),
        "), so the Effect column header is generic; each cell names the ",
        "measure it reports."))
      ft <- flextable::fontsize(ft, size = 8, part = "footer")
      ft <- flextable::color(ft, color = "#555555", part = "footer")
    }
    return(ft)
  }

  # Effect header: sm-specific when all outcomes share one, generic otherwise
  eff_hdrs <- unique(vapply(outcomes, function(g) .effect_header(g$meta$sm),
                            character(1)))
  eff_hdr  <- if (length(eff_hdrs) == 1L) eff_hdrs else "Effect\n(95% CI)"

  hdrs  <- .col_headers(show_domains, per, eff_hdr,
                        label_intervention, label_control)
  ncols <- length(hdrs)

  # Build rows, inserting group-label rows where needed
  all_rows    <- list()
  label_rows  <- integer(0)   # row indices of group-label rows
  outcome_map <- list()        # row index (char) → outcome name
  row_idx <- 0L

  add_label <- function(text) {
    row_idx <<- row_idx + 1L
    r <- as.data.frame(matrix("", 1L, ncols), stringsAsFactors = FALSE)
    r[1L, 1L] <- text
    names(r) <- hdrs
    all_rows[[length(all_rows) + 1L]] <<- r
    label_rows <<- c(label_rows, row_idx)
  }

  add_outcome <- function(nm) {
    row_idx <<- row_idx + 1L
    r <- .build_row(disp(nm), outcomes[[nm]], show_domains, per, prediction,
                    markers = fact_markers[[nm]])
    names(r) <- hdrs
    all_rows[[length(all_rows) + 1L]] <<- r
    outcome_map[[as.character(row_idx)]] <<- nm
  }

  if (!is.null(primary)) {
    if (length(prim_nms) > 0) {
      add_label(if (length(prim_nms) == 1L) "Primary outcome" else "Primary outcomes")
      for (nm in prim_nms) add_outcome(nm)
    }
    if (length(sec_nms) > 0) {
      add_label(if (length(sec_nms) == 1L) "Secondary outcome" else "Secondary outcomes")
      for (nm in sec_nms) add_outcome(nm)
    }
  } else {
    for (nm in nms) add_outcome(nm)
  }

  df <- do.call(rbind, all_rows)

  # --- flextable ---
  ft <- flextable::flextable(df)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 2, align = "center", part = "body")

  # Group-label rows: merge, grey background, bold italic
  for (lr in label_rows) {
    ft <- flextable::merge_h(ft,  i = lr, part = "body")
    ft <- flextable::bg(ft,       i = lr, bg = "#EBEBEB",  part = "body")
    ft <- flextable::bold(ft,     i = lr,                  part = "body")
    ft <- flextable::italic(ft,   i = lr,                  part = "body")
    ft <- flextable::fontsize(ft, i = lr, size = 9,        part = "body")
    ft <- flextable::align(ft,    i = lr, align = "left",  part = "body")
  }

  # Certainty cell color per outcome row
  cert_col <- hdrs[6]
  for (ri in names(outcome_map)) {
    i  <- as.integer(ri)
    nm <- outcome_map[[ri]]
    p  <- pal[[outcomes[[nm]]$certainty]]
    ft <- flextable::bg(ft,    i = i, j = cert_col, bg    = p$bg,   part = "body")
    ft <- flextable::color(ft, i = i, j = cert_col, color = p$text, part = "body")
    ft <- flextable::bold(ft,  i = i, j = cert_col,                 part = "body")
    ft <- flextable::align(ft, i = i, j = cert_col, align = "center", part = "body")
  }

  # Header style
  ft <- flextable::bg(ft,    bg = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft,  part = "header")

  # Column widths (6 base cols: Outcome, N (studies), CER, IER, Effect,
  # Certainty)
  ft <- flextable::width(ft, j = 1, width = 1.4)
  ft <- flextable::width(ft, j = 2, width = 1.1)
  ft <- flextable::width(ft, j = 3, width = 1.2)
  ft <- flextable::width(ft, j = 4, width = 1.3)
  ft <- flextable::width(ft, j = 5, width = 1.4)
  ft <- flextable::width(ft, j = 6, width = 1.4)
  if (show_domains) {
    for (j in 7:11) ft <- flextable::width(ft, j = j, width = 0.7)
  }

  # Footer
  footnote <- paste0(
    "Certainty of the evidence (Core GRADE series). ",
    "Based on the BMJ 2025 Core GRADE series (Guyatt et al.); ",
    "not an official GRADE Working Group assessment. ",
    CERTAINTY_SYMBOLS[["High"]], "=High  ",
    CERTAINTY_SYMBOLS[["Moderate"]], "=Moderate  ",
    CERTAINTY_SYMBOLS[["Low"]], "=Low  ",
    CERTAINTY_SYMBOLS[["Very Low"]], "=Very Low  ",
    "CI=confidence interval.",
    if (show_domains) " Domain columns: RoB=Risk of bias; Ind=Indirectness; Inc=Inconsistency; Imp=Imprecision; PB=Publication bias." else ""
  )
  ft <- flextable::add_footer_lines(ft, values = footnote)

  # Per-outcome risk-of-bias analysis-set notes, keyed to the [n] markers on
  # the Outcome cells.
  for (i in seq_along(rob_notes)) {
    ft <- flextable::add_footer_lines(
      ft, values = sprintf("[%d] %s", i, rob_notes[i]))
  }

  # Domain-fact footnotes continue the same [n] register (already numbered).
  for (line in fact_notes) {
    ft <- flextable::add_footer_lines(ft, values = line)
  }

  # Publication bias not formally assessed -> per-outcome qualitative-judgment
  # footnote (see domain_pubias.R)
  for (nm in nms) {
    pubias_qual_note <- .pubias_qualitative_note(outcomes[[nm]])
    if (!is.null(pubias_qual_note)) {
      ft <- flextable::add_footer_lines(
        ft, values = paste0("[", nm, "] Publication bias: ", pubias_qual_note)
      )
    }
  }

  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::color(ft, color = "#555555", part = "footer")

  ft
}

# --------------------------------------------------------------------------
# Internal helpers
# --------------------------------------------------------------------------

# Collect a presentation field (follow_up / unit) stored on the rated objects
# by grade_meta_multi() into the named-vector form .per_outcome_arg() reads.
# Returns NULL when no outcome carries one.
.display_arg_from_outcomes <- function(outcomes, field) {
  vals <- lapply(outcomes, function(g) {
    v <- g[[field]]
    if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(as.character(v))) {
      NULL
    } else {
      as.character(v)
    }
  })
  keep <- !vapply(vals, is.null, logical(1))
  if (!any(keep)) return(NULL)
  stats::setNames(unlist(vals[keep], use.names = FALSE), names(outcomes)[keep])
}

.build_row <- function(nm, g, show_domains, per = 1000, prediction = FALSE,
                       markers = NULL) {
  meta_obj <- g$meta
  k        <- meta_obj$k
  n_total  <- .total_n(meta_obj)
  cer_str  <- .format_cer(g$baseline_risk, per)
  ier_str  <- .format_ier(meta_obj, g$baseline_risk, per)
  eff      <- .format_effect(meta_obj, g$outcome_type, prediction = prediction)
  # Domain-fact markers sit after the certainty symbol; unchanged when NULL.
  cert_str <- paste0(g$certainty, "\n", CERTAINTY_SYMBOLS[[g$certainty]],
                     .fact_marker_suffix(markers))

  row <- data.frame(
    col1 = nm,
    col2 = .n_participants_studies(k, n_total, g$study_design),
    col3 = cer_str,
    col4 = ier_str,
    col5 = eff,
    col6 = cert_str,
    stringsAsFactors = FALSE
  )

  if (show_domains) {
    d   <- g$domain_assessments
    dom <- function(name) {
      r <- d[d$domain == name, ]
      if (nrow(r) == 0) return("?")
      .domain_symbol(r$judgment[1])
    }
    row$d1 <- dom("Risk of bias")
    row$d2 <- dom("Indirectness")
    row$d3 <- dom("Inconsistency")
    row$d4 <- dom("Imprecision")
    row$d5 <- dom("Publication bias")
  }
  row
}

.domain_symbol <- function(judgment) {
  switch(judgment,
    "no"            = "OK",   # no concern
    "some"          = "!",    # legacy
    "some_concerns" = "!",    # -1
    "serious"       = "!!",   # -2
    "very_serious"  = "!!",   # legacy (collapsed to serious)
    "?"
  )
}

.col_headers <- function(show_domains, per = 1000,
                         effect_header      = "Effect\n(95% CI)",
                         label_intervention = "intervention",
                         label_control      = "control") {
  per_str <- format(per, big.mark = ",", scientific = FALSE)
  base <- c("Outcome",
            "No. of participants\n(studies)",
            paste0("Risk with ", label_control, "\n(per ", per_str, ")"),
            paste0("Risk with ", label_intervention, "\n(per ", per_str, ")"),
            effect_header,
            "Certainty of the evidence\n(Core GRADE series)")
  doms <- c("RoB", "Indir.", "Incon.", "Impre.", "PB")
  if (show_domains) c(base, doms) else base
}
