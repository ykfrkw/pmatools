# grade_table.R — 複数アウトカム GRADE テーブル

#' Summary of Findings table for multiple outcomes
#'
#' @param outcomes A named list of \code{pmatools} objects, one per outcome
#'   (names become the outcome labels), or a \code{pmatools_set} from
#'   \code{\link{grade_meta_multi}}, in which case its stored order and primary
#'   outcomes are used. An element may also be a `pmatools_not_reported` from
#'   \code{\link{not_reported_outcome}} — an outcome the review prespecified
#'   that no included study reported. Its row names the outcome and reads
#'   "Not reported" in every value cell, with "Not rated" for certainty
#'   (Core GRADE 6).
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
#'   when \code{sm = "MD"} (see \code{\link{sof_table}}); same named-vector
#'   convention as \code{follow_up}.
#' @param per Denominator for SoF rate columns. \code{1000} (default); any
#'   positive denominator is accepted (see \code{\link{sof_table}}).
#' @param prediction Logical (default \code{FALSE}); when \code{TRUE}, the
#'   95 percent prediction interval is shown in the Effect column.
#' @param label_intervention,label_control Arm labels used in the
#'   "Risk with ..." column headers (GRADEpro vocabulary).
#'   Defaults are \code{"intervention"} and \code{"control"}.
#'
#' @section Presenting a continuous outcome as a proportion of responders:
#' \code{\link{sof_table}} takes the responder presentation as arguments
#' (\code{convert_smd_to_or}, \code{keep_effect_scale}, \code{baseline_risk},
#' \code{threshold_label}, \code{chinn_invert}), which is right for a table of
#' one row. A combined table has to answer it per row -- one continuous outcome
#' converted, another not, a binary one that cannot be -- so the choice rides
#' on each rated object as the \code{"pmatools_display"} attribute, a named
#' list holding those five names (see
#' \code{\link{export_bundle.pmatools_set}} for the same attribute's export
#' arguments). Both layouts fill the converted row's two arm columns with the
#' dichotomised rates, marked \code{*} and explained in a footnote written once
#' for the table however many rows used it -- and not at all when none did.
#'
#' A row asking for \code{keep_effect_scale} occupies \strong{two table rows},
#' the effect on its own scale above and the dichotomised reading below, with
#' the outcome, participant, certainty and domain cells merged across the pair.
#' It is still one outcome, and it can sit beside single-row outcomes without
#' either of them moving.
#'
#' A row that asks for the conversion and cannot support it -- a non-SMD/MD
#' effect measure, no responder proportion in (0, 1), no usable pooled estimate
#' -- keeps its unconverted presentation rather than taking the table down, and
#' the reason is stated as a numbered footnote against that row.
#' \code{sof_table()} aborts on the same conditions because its table is that
#' one row; here the other rows are still worth rendering.
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
  ok_element <- vapply(outcomes, function(g) {
    inherits(g, "pmatools") || .is_not_reported(g)
  }, logical(1))
  if (!all(ok_element)) {
    rlang::abort(paste0(
      "All elements of outcomes must be pmatools objects from grade_meta(), ",
      "or pmatools_not_reported objects from not_reported_outcome()."))
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

  # Number formatting follows the layout, as it does in sof_table(): the BMJ
  # tables print "578 per 1000" and separate every interval with "to".
  nf <- .bmj_number_format(style)

  # How each row asked to be presented (.responder_args(), sof_table.R), and
  # what came of it: the converted arm cells, or the reason the conversion could
  # not be applied to that row. Resolved before anything is built, because the
  # reason has to reach the footnote register below.
  responder     <- .resolve_responder(outcomes, nms, per,
                                      big_mark = nf$big_mark,
                                      ci_sep   = nf$ci_sep,
                                      label_control = label_control)
  converted_nms <- .converted_outcomes(responder)

  # One numbered footnote pool for every note that belongs to a single row
  # rather than to the table: the risk-of-bias analysis set (Core GRADE 4
  # Fig 2), which can differ between outcomes, the reason a not-reported
  # outcome went unreported, and the reason a row that asked for the responder
  # presentation did not get it. Each is attached to its row by a [n] marker,
  # and one row can carry more than one.
  row_notes  <- character(0)
  row_marker <- stats::setNames(vector("list", length(nms)), nms)
  for (nm in nms) {
    g <- outcomes[[nm]]
    notes <- if (.is_not_reported(g)) {
      if (is.null(g$reason)) NULL else paste0("Not reported: ", g$reason)
    } else {
      c(.rob_analysis_set_note(g),
        if (is.null(responder[[nm]]$reason)) NULL else
          .responder_fallback_note(responder[[nm]]$reason))
    }
    for (note in notes) {
      row_notes <- c(row_notes, note)
      row_marker[[nm]] <- c(row_marker[[nm]], length(row_notes))
    }
  }
  disp <- function(nm) paste0(nm, .fact_marker_suffix(row_marker[[nm]]))

  # Domain-fact footnotes share the row-note register rather than starting a
  # second one, so a reader never sees two different [1]s in one footer.
  # They are numbered here, once, and consumed by whichever style renders.
  fact_counter <- length(row_notes)
  fact_notes   <- character(0)
  fact_markers <- list()
  for (nm in nms) {
    # A not-reported outcome has no domain judgments, so no facts behind them.
    if (.is_not_reported(outcomes[[nm]])) next
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

  # Per-outcome follow-up / unit ride on the rated objects themselves when
  # grade_meta_multi() was given them, so a multi-outcome caller does not have
  # to re-assemble a parallel named vector here. Only the BMJ style reads
  # `unit` (see below); the GRADEpro layout ignores it.
  follow_up <- follow_up %||% .display_arg_from_outcomes(outcomes, "follow_up")
  unit      <- unit      %||% .display_arg_from_outcomes(outcomes, "unit")

  if (identical(style, "bmj")) {
    ft <- .grade_table_bmj(
      outcomes, nms = nms, prim_nms = prim_nms, sec_nms = sec_nms,
      primary = primary, pal = pal, per = per, prediction = prediction,
      follow_up = follow_up, unit = unit,
      label_intervention = label_intervention,
      label_control      = label_control,
      disp = disp, row_notes = row_notes,
      fact_notes = fact_notes, fact_markers = fact_markers,
      responder = responder, converted_nms = converted_nms
    )
    # Mixed effect measures (the norm once binary and continuous outcomes share
    # a table) leave the BMJ header generic. Each cell still spells its own
    # measure out ("Odds ratio 0.62 ..."), and the footnote says so, so nothing
    # is left to be inferred from the header. A not-reported outcome has no
    # measure at all and must not make a homogeneous table look mixed.
    sms <- unique(vapply(.rated_outcomes(outcomes),
                         function(g) as.character(g$meta$sm %||% ""),
                         character(1)))
    sms <- sms[nzchar(sms)]
    if (length(sms) > 1L) {
      ft <- flextable::add_footer_lines(ft, values = paste0(
        "Outcomes are reported on different effect measures (",
        paste(sms, collapse = ", "),
        "), so the Effect column header is generic; each cell names the ",
        "measure it reports."))
      ft <- .style_table_footer(ft)
    }
    return(ft)
  }

  # Effect header: sm-specific when all *rated* outcomes share one, generic
  # otherwise. A not-reported outcome carries no effect measure, so it must not
  # be allowed to degrade the header of an otherwise homogeneous table.
  eff_hdrs <- unique(vapply(.rated_outcomes(outcomes),
                            function(g) .effect_header(g$meta$sm),
                            character(1)))
  eff_hdr  <- if (length(eff_hdrs) == 1L) eff_hdrs else "Effect\n(95% CI)"

  # The arm cells are built before the headers because they decide them: a
  # continuous outcome leaves them empty, and the "Risk with control (per
  # 1,000)" wording over an empty pair describes a rate the row never had.
  arms <- lapply(nms, function(nm) {
    g <- outcomes[[nm]]
    # A not-reported outcome has no arms to describe: NULL here leaves it out
    # of the continuous-header vote, and .build_row() never reads it.
    if (.is_not_reported(g)) return(NULL)
    # A converted row supplies its own pair: responder rates.
    if (!is.null(responder[[nm]]$arm)) return(responder[[nm]]$arm)
    .sof_arm_cells(g$meta, g$baseline_risk, per)
  })
  names(arms) <- nms
  cont_any <- any(vapply(arms, function(a) isTRUE(a$continuous), logical(1)))

  hdrs  <- .col_headers(show_domains, per, eff_hdr,
                        label_intervention, label_control,
                        continuous = cont_any)
  ncols <- length(hdrs)

  # Build rows, inserting group-label rows where needed
  all_rows    <- list()
  label_rows  <- integer(0)   # row indices of group-label rows
  outcome_map <- list()        # row index (char) → outcome name
  split_rows  <- integer(0)    # first row of every two-row outcome
  row_idx <- 0L

  add_label <- function(text) {
    row_idx <<- row_idx + 1L
    r <- as.data.frame(matrix("", 1L, ncols), stringsAsFactors = FALSE)
    r[1L, 1L] <- text
    names(r) <- hdrs
    all_rows[[length(all_rows) + 1L]] <<- r
    label_rows <<- c(label_rows, row_idx)
  }

  # .build_row() returns TWO rows for an outcome shown on both scales, and it
  # is still one outcome: outcome_map records the first of the pair and
  # split_rows remembers that a second follows, so the certainty colouring and
  # the merges below can index by row without assuming one row per outcome.
  add_outcome <- function(nm) {
    r <- .build_row(disp(nm), outcomes[[nm]], show_domains, per, prediction,
                    arm = arms[[nm]], markers = fact_markers[[nm]],
                    chinn = if (is.null(responder[[nm]]$arm)) NULL else
                      responder[[nm]]$args)
    names(r) <- hdrs
    all_rows[[length(all_rows) + 1L]] <<- r
    outcome_map[[as.character(row_idx + 1L)]] <<- nm
    if (nrow(r) > 1L) split_rows <<- c(split_rows, row_idx + 1L)
    row_idx <<- row_idx + nrow(r)
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
  ft <- flextable::font(ft, fontname = .PMA_TABLE_FONT, part = "all")
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
    # Both rows of a split outcome: the certainty column is merged over the
    # pair, and a background applied to half a span leaves the unpainted half
    # showing through under it.
    i  <- as.integer(ri)
    i  <- if (i %in% split_rows) c(i, i + 1L) else i
    nm <- outcome_map[[ri]]
    if (.is_not_reported(outcomes[[nm]])) {
      # Neutral grey italics: the certainty palette encodes a rating, and there
      # is no rating here, so the cell must not borrow any of its colours.
      ft <- flextable::bg(ft,    i = i, j = cert_col, bg = "#F5F5F5",
                          part = "body")
      ft <- flextable::color(ft, i = i, j = cert_col, color = "#666666",
                             part = "body")
      ft <- flextable::italic(ft, i = i, j = cert_col, part = "body")
      ft <- flextable::align(ft, i = i, j = cert_col, align = "center",
                             part = "body")
      next
    }
    p  <- pal[[outcomes[[nm]]$certainty]]
    ft <- flextable::bg(ft,    i = i, j = cert_col, bg    = p$bg,   part = "body")
    ft <- flextable::color(ft, i = i, j = cert_col, color = p$text, part = "body")
    ft <- flextable::bold(ft,  i = i, j = cert_col,                 part = "body")
    ft <- flextable::align(ft, i = i, j = cert_col, align = "center", part = "body")
  }

  # Merged after the colouring, so the merge sees the finished cells. The
  # domain columns do not split either, so they join the merge.
  for (sr in split_rows) {
    ft <- .pma_merge_split_row(
      ft, i = sr,
      merge_cols = c(GRADEPRO_MERGED_COLS, if (show_domains) 7:11),
      rule_cols  = GRADEPRO_SPLIT_COLS)
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
    .PMA_CORE_GRADE_FOOTNOTE, " ",
    CERTAINTY_SYMBOLS[["High"]], "=High  ",
    CERTAINTY_SYMBOLS[["Moderate"]], "=Moderate  ",
    CERTAINTY_SYMBOLS[["Low"]], "=Low  ",
    CERTAINTY_SYMBOLS[["Very Low"]], "=Very Low  ",
    "CI=confidence interval.",
    if (show_domains) {
      paste0(" Domain columns: RoB=Risk of bias; Ind=Indirectness; ",
             "Inc=Inconsistency; Imp=Imprecision; PB=Publication bias.")
    } else {
      ""
    }
  )
  ft <- flextable::add_footer_lines(ft, values = footnote)

  # What "Not reported" means, stated once for the table however many such rows
  # it has.
  if (.has_not_reported(outcomes)) {
    ft <- flextable::add_footer_lines(ft, values = .not_reported_table_note())
  }

  # Per-row notes (risk-of-bias analysis set, not-reported reason), keyed to
  # the [n] markers on the Outcome cells.
  for (i in seq_along(row_notes)) {
    ft <- flextable::add_footer_lines(
      ft, values = sprintf("[%d] %s", i, row_notes[i]))
  }

  # Domain-fact footnotes continue the same [n] register (already numbered).
  for (line in fact_notes) {
    ft <- flextable::add_footer_lines(ft, values = line)
  }

  # Publication bias not formally assessed -> per-outcome qualitative-judgment
  # footnote (see domain_pubias.R)
  for (nm in nms) {
    if (.is_not_reported(outcomes[[nm]])) next
    pubias_qual_note <- .pubias_qualitative_note(outcomes[[nm]])
    if (!is.null(pubias_qual_note)) {
      ft <- flextable::add_footer_lines(
        ft, values = paste0("[", nm, "] Publication bias: ", pubias_qual_note)
      )
    }
  }

  ft <- .add_responder_notes(ft, responder, converted_nms,
                             label_intervention, label_control)

  ft <- .style_table_footer(ft)

  ft
}

# The '*' footnote explaining the responder presentation, written once however
# many rows used it and not at all when none did, followed by one line per
# converted row for the direction and threshold that row was converted against.
.add_responder_notes <- function(ft, responder, converted_nms,
                                 label_intervention = "intervention",
                                 label_control = "control") {
  if (length(converted_nms) == 0L) return(ft)
  ft <- flextable::add_footer_lines(
    ft, values = .chinn_note(reading = TRUE,
                             label_intervention = label_intervention,
                             label_control = label_control))
  for (nm in converted_nms) {
    ft <- flextable::add_footer_lines(
      ft, values = .responder_row_note(nm, responder[[nm]]$args,
                                       label_intervention, label_control))
  }
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
                       arm = NULL, markers = NULL, chinn = NULL) {
  if (.is_not_reported(g)) return(.build_row_not_reported(nm, g, show_domains))


  meta_obj <- g$meta
  k        <- meta_obj$k
  n_total  <- .total_n(meta_obj)
  arm      <- arm %||% .sof_arm_cells(meta_obj, g$baseline_risk, per)
  cer_str  <- arm$cer
  ier_str  <- arm$ier
  eff      <- .format_effect(meta_obj, g$outcome_type, prediction = prediction)
  # The derived odds ratio, in the Effect cell. This layout has no Difference
  # column, so it is the only thing here relating the two responder proportions
  # to each other.
  or_line <- if (is.null(chinn)) NULL else {
    .chinn_or_line(meta_obj, chinn$baseline_risk,
                   invert = isTRUE(chinn$chinn_invert))
  }
  # Both presentations means two table rows, the effect above and the
  # dichotomised reading below; the caller merges everything else over the
  # pair. On one row they would overwrite each other.
  split <- !is.null(or_line) && isTRUE(chinn$keep_effect_scale)
  # On one row the two lines stack; they never overwrite each other. The
  # column header is built from the effect measure, so a cell holding only the
  # derived odds ratio would sit under a heading naming the standardised mean
  # difference -- and the estimate every domain was rated on would be missing
  # from the table reporting the certainty. Single-row and two-row differ in
  # layout, not in content.
  if (!split && !is.null(or_line)) {
    eff <- paste(c(eff, or_line)[nzchar(c(eff, or_line))], collapse = "\n")
  }
  # Domain-fact markers sit after the certainty symbol; unchanged when NULL.
  cert_str <- paste0(g$certainty, "\n", CERTAINTY_SYMBOLS[[g$certainty]],
                     .fact_marker_suffix(markers))

  row <- if (split) {
    data.frame(
      col1 = c(nm, ""),
      col2 = c(.n_participants_studies(k, n_total, g$study_design), ""),
      col3 = c("", cer_str),
      col4 = c("", ier_str),
      col5 = c(eff, or_line),
      col6 = c(cert_str, ""),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      col1 = nm,
      col2 = .n_participants_studies(k, n_total, g$study_design),
      col3 = cer_str,
      col4 = ier_str,
      col5 = eff,
      col6 = cert_str,
      stringsAsFactors = FALSE
    )
  }

  if (show_domains) {
    d   <- g$domain_assessments
    dom <- function(name) {
      r <- d[d$domain == name, ]
      if (nrow(r) == 0) return("?")
      .domain_symbol(r$judgment[1])
    }
    # Padded to the row count: the domain columns are merged over a split pair,
    # so only the upper cell is rendered.
    sym <- function(name) c(dom(name), rep("", nrow(row) - 1L))
    row$d1 <- sym("Risk of bias")
    row$d2 <- sym("Indirectness")
    row$d3 <- sym("Inconsistency")
    row$d4 <- sym("Imprecision")
    row$d5 <- sym("Publication bias")
  }
  row
}

# GRADEpro row for an outcome nobody reported. `nm` already carries the [n]
# marker disp() applied, if any.
.build_row_not_reported <- function(nm, g, show_domains) {
  lbl <- .not_reported_label(g)

  # The GRADEpro layout has no follow-up element of its own, so the follow-up
  # goes under the name - the same shape the BMJ outcome cell uses.
  name_cell <- if (is.null(g$follow_up)) nm else paste0(nm, "\n", g$follow_up)

  row <- data.frame(
    col1 = name_cell,
    col2 = lbl,
    col3 = lbl,
    col4 = lbl,
    col5 = lbl,
    # No certainty symbol: the symbols are a four-level scale, and this row is
    # not on it.
    col6 = NOT_REPORTED_CERTAINTY,
    stringsAsFactors = FALSE
  )

  if (show_domains) {
    # En dash, not "?": "?" means the judgment is unknown, which invites
    # somebody to go and find it. Here there is nothing to judge. \u escape,
    # like CERTAINTY_SYMBOLS_UNICODE, so the source stays ASCII-safe.
    for (j in paste0("d", 1:5)) row[[j]] <- NOT_REPORTED_DOMAIN_SYMBOL
  }
  row
}

# One "!" per level rated down, so the column reads as severity rather than as
# a lookup the reader has to memorise. Keyed on the downgrade rather than on
# the level name, which is what keeps legacy spellings and any level added
# later working without a fifth branch here.
.domain_symbol <- function(judgment) {
  lv <- .normalize_grade_level(judgment)
  if (!lv %in% GRADE_LEVELS) return("?")
  dg <- .grade_level_downgrade(lv)
  if (dg == 0L) "OK" else strrep("!", abs(dg))
}

.col_headers <- function(show_domains, per = 1000,
                         effect_header      = "Effect\n(95% CI)",
                         label_intervention = "intervention",
                         label_control      = "control",
                         continuous         = FALSE) {
  base <- c("Outcome",
            "No. of participants\n(studies)",
            .arm_headers(continuous, per, label_intervention, label_control),
            effect_header,
            "Certainty of the evidence\n(Core GRADE series)")
  doms <- c("RoB", "Indir.", "Incon.", "Impre.", "PB")
  if (show_domains) c(base, doms) else base
}
