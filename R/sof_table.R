# sof_table.R - Summary of Findings flextable

#' Generate a Summary of Findings (SoF) table as a flextable
#'
#' @param x A \code{pmatools} object (from \code{\link{grade_meta}}).
#' @param style (v0.5) Table layout. \code{"gradepro"} (default) keeps the
#'   GRADEpro-style layout. \code{"bmj"} switches to the BMJ Core GRADE
#'   presentation: outcome and follow-up, participants and design, relative
#'   effect, a spanning "Absolute effects (95% CI)" block (control /
#'   intervention / difference), certainty with its rate-down reason, and a
#'   plain language summary (Core GRADE 6 Box 1, which supersedes the earlier
#'   Core GRADE 2 Table 1 guidance).
#' @param follow_up (v0.5) Optional free-text time frame shown under the
#'   outcome name in the \code{"bmj"} style, e.g.
#'   \code{"Follow-up: longest, range 7.7-60 months"}. \code{NULL} (default)
#'   omits the line. Ignored by the \code{"gradepro"} style.
#' @param unit (v0.5) Optional unit for the Difference column of the
#'   \code{"bmj"} style when \code{sm = "MD"}, e.g. \code{"days"}. Ignored for
#'   every other effect measure: a standardised mean difference is not on the
#'   outcome's scale, and a ratio measure has no unit at all.
#' @section Absolute effects for a continuous outcome:
#' The three absolute-effect cells -- "With control", "With intervention" and
#' "Difference" -- hold nothing mean-derived for a continuous outcome, and the
#' first two are empty unless the responder conversion below is active.
#'
#' A continuous meta-analysis routinely pools endpoint scores together with
#' change-from-baseline scores. The pooled contrast survives that; a pooled
#' \emph{control-arm mean} does not, because the two kinds of arm summary are
#' not measurements of the same quantity, and neither does anything built on
#' one. Until v0.6 the arm columns held an inverse-variance weighted mean of the
#' control arms and that mean plus the pooled difference, which is a number no
#' reader could act on in the ordinary mixed case. The pooled difference itself
#' is unaffected, so \code{sm = "MD"} keeps its Difference cell in the outcome's
#' own units; \code{sm = "SMD"} leaves it empty, since a standard-deviation
#' string there only restates the Effect column.
#' @section Absolute effects on the responder path:
#' With \code{convert_smd_to_or = TRUE} the arm columns hold responder
#' proportions and the Difference column the \strong{absolute risk difference}
#' between them, per \code{per} patients, worded like every other absolute
#' difference in the table ("177 more per 1000 (72 more to 271 more)"). The
#' Effect column reads "Derived odds ratio r (lo to hi)": that odds ratio is
#' Chinn's formula's own output, \eqn{\exp(SMD \times \pi / \sqrt{3})}, and is
#' the one derived quantity here that does \emph{not} depend on the assumed
#' control proportion. The two arm rates and the risk difference beside them
#' do. The footnote draws that line and names the proportion used.
#' @param keep_effect_scale (v0.6) Logical, only read when
#'   \code{convert_smd_to_or = TRUE}. \code{FALSE} (default) presents the
#'   outcome as responder proportions alone. \code{TRUE} presents \emph{both},
#'   which is what Core GRADE 6 recommends: the outcome stays one logical row
#'   and renders as two table rows, the effect on its own scale above and the
#'   dichotomised reading below, with the outcome, participant, certainty and
#'   plain-language cells merged across the pair. No extra columns.
#' @param palette Color palette for the certainty cell.
#'   \code{"pastel"} (default) uses soft backgrounds with colored text.
#'   \code{"classic"} uses saturated backgrounds with white text.
#' @param per Denominator for event rate columns. \code{1000} (default). Any
#'   positive denominator is accepted and used as a plain multiplier; the Shiny
#'   app offers 100 and 1000, and 10000 / 100000 as well on rare-event data.
#'   Controls the scale of the "Risk with <control>" and
#'   "Risk with <intervention>" columns.
#' @param prediction Logical. If \code{TRUE} (default \code{FALSE}), the
#'   Effect column also shows the 95 percent prediction interval on a second line,
#'   provided the meta object was run with \code{prediction = TRUE}.
#' @param convert_smd_to_or (v0.2) Logical. If \code{TRUE} and the meta
#'   object uses \code{sm = "SMD"} or \code{"MD"}, the "Risk with control" /
#'   "Risk with intervention" columns display dichotomised event rates derived via Chinn's
#'   formula (\eqn{\log OR = SMD \times \pi / \sqrt{3}}). Requires
#'   \code{baseline_risk} (numeric in (0,1)) representing the proportion
#'   of control patients meeting the threshold of clinical interest. The
#'   Difference and Effect columns follow the arm columns onto that scale; see
#'   "Absolute effects on the responder path" below.
#' @param baseline_risk Numeric in (0,1), required when
#'   \code{convert_smd_to_or = TRUE}. Otherwise inherited from the
#'   pmatools object.
#' @param threshold_label (v0.2) Optional free-text label for the
#'   dichotomisation threshold (e.g., \code{">=50 percent reduction in PHQ-9"}).
#'   Shown in the table footer when \code{convert_smd_to_or = TRUE}.
#' @param chinn_invert Logical (default \code{FALSE}). Flips the SMD sign
#'   before applying Chinn's formula so that a negative-is-better SMD (e.g.,
#'   symptom severity reduction) yields OR > 1 in the dichotomised rate
#'   columns. Only relevant when \code{convert_smd_to_or = TRUE}.
#' @section Plain language summaries -- one adverb per certainty level:
#' The \code{"bmj"} style adds a plain language summary column built from the
#' Core GRADE 6 Box 1 statements. Box 1's qualifier list gives two adverbs per
#' certainty level ("Moderate certainty: probably (likely) reduces, increases,
#' or has little to no effect"; "Low certainty: may (possibly) reduce, ..."),
#' but no summary of findings table in Core GRADE 6 prints both: Table 1 uses
#' "may decrease mortality", Table 3 uses "possibly increases", and the Box 1
#' MID example uses "probably has little to no important effect". pmatools
#' therefore emits the \strong{first} word of each pair -- \code{"probably"}
#' for Moderate and \code{"may"} for Low -- giving cells such as "Treatment
#' probably results in an important increase in serious adverse events" and
#' "Treatment may reduce mortality". This single-adverb rendering is a pmatools
#' choice, not a quotation of the qualifier list; High and Very low certainty
#' carry no qualifier and are unchanged.
#'
#' @param label_intervention,label_control Arm labels used in the
#'   "Risk with ..." column headers (GRADEpro vocabulary), e.g.
#'   \code{label_intervention = "CBT-I"}, \code{label_control = "placebo"}.
#'   Defaults are \code{"intervention"} and \code{"control"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{flextable} object suitable for printing, Word export, etc.
#'
#' @examples
#' \dontrun{
#' g <- grade_meta(m, study_design = "RCT", rob = "some",
#'                 rob_rationale = "RoB2 consensus: some concerns from missing outcome data")
#' sof_table(g)
#' sof_table(g, per = 100)
#' sof_table(g, prediction = TRUE)
#' sof_table(g, palette = "classic")
#' sof_table(g, style = "bmj",
#'           follow_up = "Follow-up: longest, range 7.7-60 months")
#' flextable::save_as_docx(sof_table(g), path = "sof.docx")
#' }
#'
#' @export
sof_table <- function(x, style = c("gradepro", "bmj"),
                      palette = c("pastel", "classic"),
                      per = 1000, prediction = FALSE,
                      follow_up = NULL,
                      unit      = NULL,
                      convert_smd_to_or = FALSE,
                      keep_effect_scale = FALSE,
                      baseline_risk     = NULL,
                      threshold_label   = NULL,
                      chinn_invert      = FALSE,
                      label_intervention = "intervention",
                      label_control      = "control",
                      ...) {
  if (.is_not_reported(x)) {
    rlang::abort(paste0(
      "sof_table: 'x' must be a pmatools object from grade_meta(); a ",
      "not-reported outcome has no analysis to summarise; put it in a ",
      "multi-outcome table with grade_table()."))
  }
  if (!inherits(x, "pmatools")) {
    rlang::abort("x must be a pmatools object from grade_meta().")
  }
  style   <- match.arg(style)
  palette <- match.arg(palette)
  pal     <- CERTAINTY_PALETTES[[palette]]

  meta_obj <- x$meta

  # v0.2 Chinn conversion path
  chinn_active <- FALSE
  if (isTRUE(convert_smd_to_or)) {
    sm <- meta_obj$sm
    if (is.null(sm) || !sm %in% c("SMD", "MD")) {
      rlang::abort(
        "convert_smd_to_or = TRUE requires meta_obj$sm in c('SMD','MD')."
      )
    }
    if (is.null(baseline_risk) || !is.numeric(baseline_risk) ||
        length(baseline_risk) != 1 || baseline_risk <= 0 || baseline_risk >= 1) {
      rlang::abort(
        "convert_smd_to_or = TRUE requires baseline_risk as a single numeric in (0, 1)."
      )
    }
    chinn_active <- TRUE
  }

  baseline_for_display <- if (chinn_active) baseline_risk else x$baseline_risk

  k           <- meta_obj$k
  n_total     <- .total_n(meta_obj)
  # Number formatting differs by style: GRADEpro keeps "306 per 1,000" and a
  # semicolon-separated CI, BMJ prints "306 per 1000" with a "to" separator
  # throughout (see .bmj_number_format()).
  nf          <- .bmj_number_format(style)
  # Chinn dichotomisation replaces both arm cells with responder rates derived
  # from a user-supplied control event rate; everything else reads the arm
  # cells off the object -- the baseline risk for a binary outcome, nothing at
  # all for a continuous one, whose arm columns no longer carry a pooled
  # control-arm mean (see .sof_arm_cells()).
  chinn_args <- list(baseline_risk     = baseline_risk,
                     chinn_invert      = isTRUE(chinn_invert),
                     threshold_label   = threshold_label,
                     keep_effect_scale = isTRUE(keep_effect_scale))
  if (chinn_active) {
    arm <- .responder_arm_cells(meta_obj, chinn_args, per,
                                big_mark = nf$big_mark, ci_sep = nf$ci_sep)
  } else {
    arm <- .sof_arm_cells(meta_obj, x$baseline_risk, per,
                          big_mark = nf$big_mark, ci_sep = nf$ci_sep)
  }
  cer_str <- arm$cer
  ier_str <- arm$ier

  # BMJ Core GRADE presentation is a different table shape entirely, so it is
  # built by its own routine; everything below stays the GRADEpro layout.
  if (identical(style, "bmj")) {
    # Only the Chinn cells are handed down; otherwise .bmj_row_values() derives
    # the arm cells itself, which is also the path the multi-outcome BMJ table
    # takes, so both tables fill their columns the same way.
    return(.sof_table_bmj(
      x, pal = pal, per = per, prediction = prediction,
      cer_str = if (chinn_active) cer_str else NULL,
      ier_str = if (chinn_active) ier_str else NULL,
      baseline_for_display = baseline_for_display,
      follow_up = follow_up, unit = unit,
      chinn_active = chinn_active, chinn_invert = chinn_invert,
      threshold_label = threshold_label,
      keep_effect_scale = isTRUE(keep_effect_scale),
      label_intervention = label_intervention,
      label_control      = label_control
    ))
  }

  effect_str  <- .format_effect(meta_obj, x$outcome_type,
                                prediction = prediction)
  # The derived odds ratio rides in the Effect column in both layouts, not only
  # the BMJ one: the conversion is a property of how the row is presented, and a
  # reader who switched layouts to get the GRADEpro column set would otherwise
  # lose the only ratio the responder cells can be read against.
  or_line <- if (chinn_active) {
    .chinn_or_line(meta_obj, baseline_risk, invert = isTRUE(chinn_invert),
                   ci_sep = nf$ci_sep)
  } else NULL
  # The responder presentation IS the dichotomised reading, so its Effect cell
  # carries the derived odds ratio instead of the pooled estimate rather than
  # under it. Asking for both keeps the pooled estimate, on a row of its own.
  split_row <- chinn_active && isTRUE(keep_effect_scale) && !is.null(or_line)
  if (chinn_active && !split_row) effect_str <- or_line %||% effect_str

  certainty_label <- x$certainty
  certainty_sym   <- CERTAINTY_SYMBOLS_UNICODE[[certainty_label]]
  cell_colors     <- pal[[certainty_label]]

  headers <- c(
    "Outcome",
    "No. of participants\n(studies)",
    .arm_headers(arm$continuous, per, label_intervention, label_control),
    .effect_header(meta_obj$sm),
    "Certainty of the evidence\n(Core GRADE series)"
  )

  # Numbered footnotes for the domains that pulled the rating down, with the
  # markers on the certainty cell. The register starts at [1]: the
  # analysis-set and publication-bias sentences below stay unnumbered.
  fact_domains <- .rated_down_fact_domains(x)
  fact_notes   <- character(0)
  fact_markers <- integer(0)
  for (dm in fact_domains) {
    note <- .domain_fact_note(x, dm)
    if (is.null(note)) next
    fact_notes   <- c(fact_notes, note)
    fact_markers <- c(fact_markers, length(fact_notes))
  }

  certainty_cell <- paste0(certainty_label, "\n", certainty_sym,
                           .fact_marker_suffix(fact_markers))

  # One logical row, one or two table rows: the effect on its own scale above
  # and the dichotomised reading below when both were asked for. The columns
  # that do not split are merged over the pair below, so the lower row leaves
  # them empty.
  df <- if (split_row) {
    data.frame(
      col1 = c(x$outcome_name, ""),
      col2 = c(.n_participants_studies(k, n_total, x$study_design), ""),
      col3 = c("", cer_str),
      col4 = c("", ier_str),
      col5 = c(effect_str, or_line),
      col6 = c(certainty_cell, ""),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      col1 = x$outcome_name,
      col2 = .n_participants_studies(k, n_total, x$study_design),
      col3 = cer_str,
      col4 = ier_str,
      col5 = effect_str,
      col6 = certainty_cell,
      stringsAsFactors = FALSE
    )
  }
  names(df) <- headers

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, .list = stats::setNames(as.list(headers), headers))
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = .PMA_TABLE_FONT, part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 2, align = "center", part = "body")

  cert_col <- headers[6]
  ft <- flextable::bg(ft,    j = cert_col, bg    = cell_colors$bg,   part = "body")
  ft <- flextable::color(ft, j = cert_col, color = cell_colors$text, part = "body")
  ft <- flextable::bold(ft,  j = cert_col, part = "body")
  ft <- flextable::align(ft, j = cert_col, align = "center", part = "body")

  if (split_row) {
    ft <- .pma_merge_split_row(ft, i = 1L,
                               merge_cols = GRADEPRO_MERGED_COLS,
                               rule_cols  = GRADEPRO_SPLIT_COLS)
  }

  ft <- flextable::width(ft, j = 1, width = 1.4)
  ft <- flextable::width(ft, j = 2, width = 1.2)
  ft <- flextable::width(ft, j = 3, width = 1.3)
  ft <- flextable::width(ft, j = 4, width = 1.4)
  ft <- flextable::width(ft, j = 5, width = 1.5)
  ft <- flextable::width(ft, j = 6, width = 1.5)

  ft <- flextable::bg(ft,    bg = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft,  part = "header")

  pi_note <- if (prediction) " PrI = 95 percent prediction interval." else ""

  base_note <- paste0(
    "Certainty rating (Core GRADE series): ", certainty_label, ". ",
    .PMA_CORE_GRADE_FOOTNOTE, " ",
    "CI = confidence interval.", pi_note,
    # A continuous outcome's arm columns are empty unless the conversion filled
    # them with rates, and a sentence describing how a rate was computed is
    # noise over two empty cells.
    if (arm$continuous) "" else paste0(
      " ", .arm_label_cap(label_intervention), " rate (Risk with ",
      label_intervention, ") = ", label_intervention, "-arm event rate ",
      "computed from baseline risk and pooled relative effect.")
  )
  ft <- flextable::add_footer_lines(ft, values = base_note)

  # What actually drove each downgrade, keyed to the [n] markers on the
  # certainty cell.
  for (i in seq_along(fact_notes)) {
    ft <- flextable::add_footer_lines(
      ft, values = sprintf("[%d] %s", i, fact_notes[i]))
  }

  # Risk-of-bias analysis set (Core GRADE 4 Fig 2). A refit silently changes
  # every number in this table, so it must always be stated; the unapplied
  # recommendation is stated too, so the reader knows the shown estimate is
  # not the one the flowchart points at.
  rob_set_note <- .rob_analysis_set_note(x)
  if (!is.null(rob_set_note)) {
    ft <- flextable::add_footer_lines(ft, values = rob_set_note)
  }

  # Publication bias not formally assessed -> prominent qualitative-judgment
  # footnote (see domain_pubias.R)
  pubias_qual_note <- .pubias_qualitative_note(x)
  if (!is.null(pubias_qual_note)) {
    ft <- flextable::add_footer_lines(
      ft, values = paste0("Publication bias: ", pubias_qual_note)
    )
  }

  # Chinn-specific footnote with explicit '*' link and citations
  if (chinn_active) {
    ft <- flextable::add_footer_lines(
      ft, values = .chinn_note(invert = isTRUE(chinn_invert),
                               threshold_label = threshold_label,
                               reading = TRUE,
                               baseline_risk = baseline_risk,
                               label_intervention = label_intervention,
                               label_control = label_control))
  }

  ft <- .style_table_footer(ft)

  ft
}

#' Append caller footnotes to a Summary of Findings table
#'
#' @description
#' Adds one footer line per note to a table built by \code{\link{sof_table}} or
#' \code{\link{grade_table}}, styled like the footnotes those functions write
#' themselves. A host application that annotates its tables with something
#' pmatools cannot know about — a rare-event alert, a scope caveat, a local
#' registration number — can therefore hand the same annotation to
#' \code{\link{export_bundle}} via its `sof_notes` argument instead of writing
#' the .docx itself.
#'
#' @param x A \code{flextable}, typically from \code{\link{sof_table}} or
#'   \code{\link{grade_table}}.
#' @param notes Character vector (or list) of footnote lines, one footer line
#'   each, appended in order after the table's own footnotes. \code{NULL},
#'   \code{NA} and empty entries are dropped; a `notes` argument with nothing
#'   usable in it returns `x` unchanged.
#'
#' @return The \code{flextable} with the notes appended.
#'
#' @examples
#' \dontrun{
#' ft <- sof_table(g, style = "bmj")
#' sof_add_notes(ft, "Event rates below 1%: analyse risk differences directly.")
#' }
#'
#' @export
sof_add_notes <- function(x, notes) {
  if (!inherits(x, "flextable")) {
    rlang::abort("sof_add_notes: 'x' must be a flextable.")
  }
  notes <- .usable_notes(notes)
  if (length(notes) == 0L) return(x)

  for (nt in notes) {
    x <- flextable::add_footer_lines(x, values = nt)
  }
  # Re-applied over the whole footer, matching what sof_table() and
  # grade_table() do to their own footnotes, so appended lines cannot be told
  # apart from the built-in ones.
  x <- .style_table_footer(x)
  x
}

# Flatten a notes argument to the character vector actually worth printing.
# Shared by sof_add_notes() and the export_bundle() script renderers, so a note
# dropped from the table is dropped from the generated script too.
.usable_notes <- function(notes) {
  if (is.null(notes)) return(character(0))
  if (is.list(notes)) notes <- unlist(notes, use.names = FALSE)
  if (length(notes) == 0L) return(character(0))
  notes <- as.character(notes)
  notes[!is.na(notes) & nzchar(notes)]
}

# --------------------------------------------------------------------------
# Helpers (shared with grade_table.R via package namespace)
# --------------------------------------------------------------------------

# Footer sentence describing which studies the effect estimate rests on.
# Returns NULL for the ordinary "all studies" case.
.rob_analysis_set_note <- function(x) {
  if (isTRUE(x$rob_refit)) {
    k_low  <- x$meta$k
    k_full <- x$meta_full$k %||% k_low
    return(sprintf(paste0(
      "Effect estimate restricted to low risk of bias studies (n = %d of %d) ",
      "per Core GRADE 4 Fig 2."), k_low, k_full))
  }
  if (identical(x$rob_analysis_set, "low_only")) {
    return(paste0(
      "Core GRADE 4 Fig 2 recommends restricting the analysis to low risk of ",
      "bias studies; the effect estimate shown includes all studies ",
      "(rob_refit = FALSE)."
    ))
  }
  NULL
}

# --------------------------------------------------------------------------
# Structured domain facts as footnote text
#
# The prose in domain_assessments$notes is written for a reader following the
# Core GRADE flowcharts and is far too long for a table footer; the facts are
# the same numbers already formatted for printing, so a footnote can state
# what drove a downgrade in one line. Shared by the SoF renderers and
# evidence_profile() so the two cannot word the same fact differently.
# --------------------------------------------------------------------------

# "<Label>: <value>. <Label>: <value>." -- the clauses only, no domain name.
# NULL when the domain recorded nothing. Trailing periods are stripped per
# clause so a value that already ends in one (Inconsistency's zone_decision)
# does not produce "..".
.domain_fact_body <- function(facts) {
  if (is.null(facts) || !is.data.frame(facts) || nrow(facts) == 0L) return(NULL)
  facts <- .drop_machine_only_facts(facts)
  if (nrow(facts) == 0L) return(NULL)
  clauses <- sprintf("%s: %s", facts$label, facts$value)
  clauses <- sub("\\.+$", "", clauses)
  clauses <- clauses[nzchar(clauses)]
  if (length(clauses) == 0L) return(NULL)
  paste0(paste(clauses, collapse = ". "), ".")
}

# One footnote line's worth of text for a domain, or NULL. `outcome_name`
# names the row in a multi-outcome table, where one footer serves several
# ratings.
#
# An overridden domain leads with the override. Until 0.5.1 the footnote was
# built from the facts alone, and the facts are not rewritten when a reviewer
# overrides a judgment -- they cannot be, they record what the algorithm found.
# So the certainty cell and the "Due to ..." sentence moved with the override
# while the footnote under them went on stating the automatic reasoning, which
# read as the justification for a rating it had not produced. The two are now
# separated in words: the reviewer's rationale first, the automatic assessment
# named as such after it, and the facts kept, because "what the algorithm found
# and the panel overrode" is exactly what a reader of a SoF footnote wants.
#
# Deliberately not keyed on the domain name: an override is possible on every
# domain, and a branch that named one would be a bug in waiting on the others.
.domain_fact_note <- function(x, domain, outcome_name = NULL) {
  body     <- .domain_fact_body((x$domain_facts %||% list())[[domain]])
  override <- .domain_override_note(x, domain)
  if (is.null(body) && is.null(override)) return(NULL)
  head <- if (!is.null(outcome_name) && length(outcome_name) == 1L &&
               !is.na(outcome_name) && nzchar(outcome_name)) {
    sprintf("%s (%s).", domain, outcome_name)
  } else {
    paste0(domain, ".")
  }
  if (is.null(override)) return(paste(head, body))

  override_clause <- sprintf(
    "Rated %s by the reviewer, not by the algorithm: %s",
    .grade_level_wording(override$judgment),
    sub("\\.*$", ".", override$rationale))
  if (is.null(body)) return(paste(head, override_clause))
  paste(head, override_clause, paste("The automatic assessment recorded:",
                                     body))
}

# Domains that pulled the rating down AND have something to say about why, in
# domain_assessments order. A domain that did not rate down needs no
# explanation in the footer.
#
# "Something to say" is facts OR a reviewer's override rationale. Indirectness
# emits no facts at all, so before 0.5.1 a panel that rated it down by hand got
# a "Due to indirectness" sentence with nothing under it saying why -- the one
# case where the footer had a reason available and printed none.
.rated_down_fact_domains <- function(x) {
  d <- x$domain_assessments
  if (is.null(d) || nrow(d) == 0L) return(character(0))
  all_facts <- x$domain_facts %||% list()
  dg   <- d$downgrade
  doms <- d$domain[!is.na(dg) & dg < 0]
  has_reason <- vapply(doms, function(dm) {
    dm %in% names(all_facts) || !is.null(.domain_override_note(x, dm))
  }, logical(1))
  doms[has_reason]
}

# The certainty-cell marker for the GRADEpro layouts: " [1]", " [1][2]".
# Empty string when nothing is marked, so the cell text is unchanged.
.fact_marker_suffix <- function(markers) {
  if (is.null(markers) || length(markers) == 0L) return("")
  paste0(" ", paste0("[", as.integer(markers), "]", collapse = ""))
}

# Combined "No of participants (studies)" cell, GRADEpro style:
# "1,234 (12 RCTs)"; falls back to "(12 studies)" when the study design
# is unavailable.
.n_participants_studies <- function(k, n_total, study_design = NULL) {
  k <- as.integer(k)
  design_lbl <- if (is.null(study_design) || length(study_design) != 1L ||
                    is.na(study_design) || !nzchar(study_design)) {
    if (k == 1L) "study" else "studies"
  } else if (toupper(study_design) == "RCT") {
    if (k == 1L) "RCT" else "RCTs"
  } else if (tolower(study_design) %in% c("obs", "observational")) {
    if (k == 1L) "observational study" else "observational studies"
  } else {
    if (k == 1L) "study" else "studies"
  }
  n_str <- if (is.na(n_total)) "NR" else format(n_total, big.mark = ",")
  sprintf("%s (%d %s)", n_str, k, design_lbl)
}

# The two arm-column headers of the GRADEpro layout. "Risk with control
# (per 1,000)" is the wording for an event rate; when the cells hold the
# outcome's own value instead (a continuous outcome's pooled arm means), the
# rate vocabulary and the denominator would both be wrong, so the headers fall

# An arm label at the START of a sentence. Labels are free text a reviewer
# typed -- "CBT-I", "usual care", "placebo" -- so only the first character is
# touched. toupper() on the whole string would shout an acronym back at them.
.arm_label_cap <- function(x) {
  if (!length(x) || is.na(x) || !nzchar(x)) return(x)
  paste0(toupper(substring(x, 1L, 1L)), substring(x, 2L))
}

# The arm as the SUBJECT of a sentence, which is not the same job as labelling
# a column. The package default "intervention" is a column label; "OR > 1 =
# intervention better" is not a sentence anyone writes. So the default falls
# back to the generic word these footnotes have always used, and a
# caller-supplied label replaces it.
#
# Consequence, and it is the point: at the default labels every footnote below
# is byte-identical to what it was before the labels reached them, so a review
# that never named its arms gets exactly the table it always got.
.arm_subject <- function(label_intervention, generic = "treatment") {
  if (identical(label_intervention, "intervention")) generic else
    label_intervention
}

# back to the measure-neutral "... with control".
.arm_headers <- function(continuous, per = 1000,
                         label_intervention = "intervention",
                         label_control      = "control") {
  if (isTRUE(continuous)) {
    return(c(paste0("With ", label_control),
             paste0("With ", label_intervention)))
  }
  per_str <- format(per, big.mark = ",", scientific = FALSE)
  c(paste0("Risk with ", label_control, "\n(per ", per_str, ")"),
    paste0("Risk with ", label_intervention, "\n(per ", per_str, ")"))
}

# GRADEpro-style effect column header, by summary measure
.effect_header <- function(sm) {
  if (!is.null(sm) && length(sm) == 1L && !is.na(sm) &&
      sm %in% c("RR", "OR", "HR", "IRR", "RoM")) {
    "Relative effect\n(95% CI)"
  } else if (identical(sm, "MD")) {
    "Mean difference\n(95% CI)"
  } else if (identical(sm, "SMD")) {
    "Standardized mean difference\n(95% CI)"
  } else {
    "Effect\n(95% CI)"
  }
}

# Package-wide *display* helper: total participants for the N column. Lenient
# by design -- when the arm totals are missing it falls back to meta_obj$n, so
# a single-arm meta (metaprop / metamean) still prints its real N instead of
# "NR". domain_imprecision.R deliberately does NOT use this one: its
# .total_n_strict() refuses the meta_obj$n fallback, because the "800 (400 per
# group)" rule of thumb presupposes a genuine two-arm total.
.total_n <- function(meta_obj) {
  n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA
  n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA
  if (!is.na(n_e) && !is.na(n_c)) return(n_e + n_c)
  if (!is.null(meta_obj$n)) return(sum(meta_obj$n, na.rm = TRUE))
  NA_integer_
}

# Pooled estimate with model fallback (mirrors domain_imprecision.R /
# domain_rob.R): prefer the random-effects pool when random = TRUE, otherwise
# the common-effect pool; fall back to the other model when the preferred
# one is unavailable (e.g. run_ma(random = FALSE, common = TRUE)).
.pooled_estimate <- function(meta_obj) {
  pick <- function(model) {
    if (model == "random") {
      list(est   = meta_obj$TE.random,
           lower = meta_obj$lower.random,
           upper = meta_obj$upper.random)
    } else {
      list(est   = meta_obj$TE.common,
           lower = meta_obj$lower.common,
           upper = meta_obj$upper.common)
    }
  }
  ok <- function(x) {
    !is.null(x$est) && length(x$est) == 1L && is.finite(x$est)
  }
  primary <- if (isTRUE(meta_obj$random)) "random" else "common"
  out <- pick(primary)
  if (!ok(out)) {
    out <- pick(if (primary == "random") "common" else "random")
  }
  out
}

#' Format a pooled effect estimate as a display string
#'
#' Renders the pooled estimate of a \code{meta} object the way pmatools prints
#' it in a Summary of Findings table: the effect measure, the point estimate and
#' its 95 percent confidence interval, back-transformed out of the log scale for
#' ratio measures. This is the exact string \code{\link{sof_table}},
#' \code{\link{grade_table}} and \code{\link{grade_report}} put in their Effect
#' column.
#'
#' Exported so that a caller building its own view of the same analysis -- an
#' interactive results panel, a custom table, a plot annotation -- can show the
#' effect in the same wording and to the same precision as the pmatools tables,
#' instead of re-deriving it and drifting out of step (choosing the wrong model
#' when only one of random/common was fitted, or forgetting to exponentiate).
#'
#' Which model is read follows the object: the random-effects pool when
#' \code{meta_obj$random} is \code{TRUE}, otherwise the common-effect pool, with
#' a fallback to the other model when the preferred one was not fitted.
#'
#' @param meta_obj A \code{meta} object, e.g. from \code{\link{run_ma}} or
#'   \code{\link[meta]{metabin}} / \code{\link[meta]{metacont}}.
#' @param outcome_type \code{"relative"} for ratio measures (RR, OR, HR, IRR),
#'   whose estimate and CI are exponentiated before printing, or
#'   \code{"absolute"} for measures already on their natural scale (MD, SMD,
#'   RD). Matches the \code{outcome_type} argument of \code{\link{grade_meta}}.
#' @param prediction Logical. When \code{TRUE} and the meta object carries a
#'   prediction interval, a second line \code{"PrI (lo; hi)"} is appended,
#'   separated by a newline. Default \code{FALSE}.
#'
#' @return A single string such as \code{"RR 0.55 (0.38; 0.79)"}, or
#'   \code{"NR"} when the object has no usable pooled estimate. With
#'   \code{prediction = TRUE} the string may contain an embedded newline.
#'
#' @seealso \code{\link{sof_table}}, \code{\link{grade_table}}.
#'
#' @examples
#' m <- meta::metabin(
#'   event.e = c(10, 12, 8), n.e = c(50, 60, 40),
#'   event.c = c(20, 22, 18), n.c = c(50, 60, 40),
#'   studlab = c("Trial 1", "Trial 2", "Trial 3"),
#'   sm = "RR", random = TRUE, prediction = TRUE
#' )
#' format_effect(m, outcome_type = "relative")
#' cat(format_effect(m, outcome_type = "relative", prediction = TRUE), "\n")
#'
#' @export
format_effect <- function(meta_obj, outcome_type, prediction = FALSE) {
  sm  <- meta_obj$sm
  pooled <- .pooled_estimate(meta_obj)
  est <- pooled$est
  lo  <- pooled$lower
  hi  <- pooled$upper

  if (is.null(est) || is.na(est)) return("NR")

  if (outcome_type == "relative" && !is.null(sm) &&
      sm %in% c("RR", "OR", "HR", "IRR")) {
    est <- exp(est); lo <- exp(lo); hi <- exp(hi)
    s <- sprintf("%s %.2f (%.2f; %.2f)", sm, est, lo, hi)
  } else {
    s <- sprintf("%s %.2f (%.2f; %.2f)",
                 if (is.null(sm)) "Effect" else sm, est, lo, hi)
  }

  if (prediction) {
    pi_lo <- meta_obj$lower.predict
    pi_hi <- meta_obj$upper.predict
    if (!is.null(pi_lo) && !is.null(pi_hi) &&
        !is.na(pi_lo) && !is.na(pi_hi)) {
      if (outcome_type == "relative" && !is.null(sm) &&
          sm %in% c("RR", "OR", "HR", "IRR")) {
        pi_lo <- exp(pi_lo); pi_hi <- exp(pi_hi)
      }
      s <- paste0(s, sprintf("\nPrI (%.2f; %.2f)", pi_lo, pi_hi))
    }
  }

  s
}

# Internal alias kept so existing call sites (sof_table.R, grade_table.R,
# grade_report.R) do not move.
.format_effect <- function(meta_obj, outcome_type, prediction = FALSE) {
  format_effect(meta_obj, outcome_type, prediction = prediction)
}

# Denominator label for the "per N" phrasing. GRADEpro keeps the thousands
# separator ("per 1,000"); BMJ prints it bare ("per 1000"), so callers pass
# big_mark = FALSE. Defaults reproduce the GRADEpro output exactly.
.per_label <- function(per, big_mark = TRUE) {
  if (isTRUE(big_mark)) {
    format(per, big.mark = ",", scientific = FALSE)
  } else {
    format(per, scientific = FALSE, trim = TRUE)
  }
}

# Control event rate: baseline_risk displayed per 'per' units (no CI)
.format_cer <- function(baseline_risk, per = 1000, big_mark = TRUE) {
  if (is.null(baseline_risk)) return("-")
  per_str <- .per_label(per, big_mark)
  sprintf("%d per %s", round(baseline_risk * per), per_str)
}

# Experimental (intervention) event rate: derived from baseline + relative effect
.format_ier <- function(meta_obj, baseline_risk, per = 1000,
                        big_mark = TRUE, ci_sep = "; ") {
  if (is.null(baseline_risk)) return("-")
  sm <- meta_obj$sm
  if (is.null(sm) || !sm %in% c("RR", "OR", "HR", "IRR")) return("-")

  pooled <- .pooled_estimate(meta_obj)
  if (is.null(pooled$est) || is.na(pooled$est)) return("-")

  p1_est <- .p1(baseline_risk, pooled$est,   sm)
  p1_lo  <- .p1(baseline_risk, pooled$lower, sm)
  p1_hi  <- .p1(baseline_risk, pooled$upper, sm)

  if (is.null(p1_est)) return("-")

  per_str <- .per_label(per, big_mark)
  sprintf("%d per %s\n(%d%s%d)",
          round(p1_est * per), per_str,
          round(p1_lo  * per), ci_sep,
          round(p1_hi  * per))
}

# The intervention-arm proportions a Chinn dichotomisation implies, and the odds
# ratio they were built from. NULL when any ingredient is missing.
#
# This used to live inside .format_ier_chinn(), which computed the three
# proportions, printed one cell out of them and threw the rest away. Three other
# cells of the same row are functions of exactly these numbers -- the per-1000
# risk difference in the Difference column, the derived risk ratio on the second
# line of the Effect column, and the responder line of a both-scales arm cell --
# and each of them re-deriving the conversion is how the four end up disagreeing
# after somebody edits one. They all read this instead.
#
# `invert = TRUE` flips the SMD sign before applying the formula, so a
# negative-is-better SMD (e.g., depression severity reduction) yields OR > 1.
# The bounds are swapped with the sign, which keeps or_lower below or_upper and
# therefore keeps p1_lo below p1_hi whichever direction the outcome runs in.
.chinn_rates <- function(meta_obj, baseline_risk, invert = FALSE) {
  if (is.null(baseline_risk)) return(NULL)
  pooled <- .pooled_estimate(meta_obj)
  est <- pooled$est
  lo  <- pooled$lower
  hi  <- pooled$upper
  if (is.null(est) || is.na(est)) return(NULL)

  if (isTRUE(invert)) {
    est_eff <- -est
    lo_eff  <- -hi
    hi_eff  <- -lo
  } else {
    est_eff <- est
    lo_eff  <- lo
    hi_eff  <- hi
  }
  conv <- chinn_smd_to_or(est_eff, ci_lower = lo_eff, ci_upper = hi_eff)
  log_or_est <- log(conv$or)
  log_or_lo  <- log(conv$or_lower)
  log_or_hi  <- log(conv$or_upper)

  p1_est <- .p1(baseline_risk, log_or_est, "OR")
  p1_lo  <- .p1(baseline_risk, log_or_lo,  "OR")
  p1_hi  <- .p1(baseline_risk, log_or_hi,  "OR")

  if (is.null(p1_est)) return(NULL)

  list(p1 = p1_est, p1_lo = p1_lo, p1_hi = p1_hi,
       or = conv$or, or_lower = conv$or_lower, or_upper = conv$or_upper)
}

# Experimental rate via Chinn (SMD/MD -> OR -> p1)
.format_ier_chinn <- function(meta_obj, baseline_risk, per = 1000, invert = FALSE,
                              big_mark = TRUE, ci_sep = "; ") {
  rates <- .chinn_rates(meta_obj, baseline_risk, invert = invert)
  if (is.null(rates)) return("-")

  per_str <- .per_label(per, big_mark)
  sprintf("%d per %s\n(%d%s%d)",
          round(rates$p1    * per), per_str,
          round(rates$p1_lo * per), ci_sep,
          round(rates$p1_hi * per))
}

# The odds ratio a Chinn dichotomisation implies: exp(SMD x pi / sqrt(3)),
# which is what the formula actually emits.
#
# This used to print the derived RISK ratio, p1 / p0. That number is
# arithmetically correct, but it exists only once an assumed control proportion
# has been laid on top of the formula's output, and the odds ratio is the
# formula's output. Printing the odds ratio puts one fewer assumption between
# the pooled estimate and the number on the page, and it is the only derived
# quantity in the row that does not move when the reviewer revises p0 -- which
# is exactly what the footnote now says (.chinn_derived_sentence()).
#
# It goes in the Effect column rather than in a column of its own, because it
# is not a second analysis: it is the same pooled SMD read on a different
# scale, and a column would present it as an independent result. NULL when the
# conversion has no usable rates, so the caller leaves the Effect cell exactly
# as .format_effect() wrote it.
#
# `baseline_risk` is still taken, even though the odds ratio does not depend on
# it: it is what .chinn_rates() needs to answer at all, and a caller that had
# no proportion would have no converted row to put this line in either.
.chinn_or_line <- function(meta_obj, baseline_risk, invert = FALSE,
                           ci_sep = "; ") {
  rates <- .chinn_rates(meta_obj, baseline_risk, invert = invert)
  if (is.null(rates)) return(NULL)
  vals <- c(rates$or, rates$or_lower, rates$or_upper)
  if (!all(is.finite(vals))) return(NULL)
  b <- sort(vals[-1L])
  sprintf("Derived odds ratio %.2f (%.2f%s%.2f)", vals[1L], b[1L], ci_sep, b[2L])
}

# --------------------------------------------------------------------------
# The responder presentation, as one row of any Summary of Findings table
# --------------------------------------------------------------------------
#
# sof_table() takes the choice as arguments, which is right for a table of one
# row. grade_table() builds the only Summary of Findings a multi-outcome bundle
# carries, so there the choice has to be readable per row: it rides on each
# rated object as the "pmatools_display" attribute (PMATOOLS_RESPONDER_FIELDS,
# multi_outcome.R). The helpers below are what both paths share.

# The responder arguments this row asked for, or NULL when it asked for none.
.responder_args <- function(g) {
  if (.is_not_reported(g)) return(NULL)
  if (!isTRUE(.outcome_display(g, "convert_smd_to_or"))) return(NULL)
  list(
    baseline_risk     = .outcome_display(g, "baseline_risk"),
    threshold_label   = .outcome_display(g, "threshold_label"),
    chinn_invert      = isTRUE(.outcome_display(g, "chinn_invert")),
    keep_effect_scale = isTRUE(.outcome_display(g, "keep_effect_scale"))
  )
}

# Why this row cannot be presented as a proportion of responders, or NULL when
# it can. The first two conditions are the ones sof_table() aborts on; the third
# is the pooled estimate the conversion is applied to.
#
# sof_table() aborts because its table IS that row: with the conversion refused
# there is nothing left to render. A combined table has other rows, and taking
# the whole document away from a reviewer because one outcome cannot be
# converted is the worse answer, so grade_table() falls back to the unconverted
# presentation and prints this sentence against the row instead.
.responder_unavailable_reason <- function(g, args,
                                          label_control = "control") {
  sm <- as.character(g$meta$sm %||% "")
  if (!sm %in% c("SMD", "MD")) {
    return(sprintf(paste0(
      "its effect measure is %s, and Chinn's formula converts a standardised ",
      "mean difference or a mean difference only"),
      if (nzchar(sm)) sm else "not recorded"))
  }
  p0 <- args$baseline_risk
  if (is.null(p0) || !is.numeric(p0) || length(p0) != 1L || is.na(p0) ||
      p0 <= 0 || p0 >= 1) {
    return(paste0("no ", label_control, "-group responder proportion in ",
                  "(0, 1) was recorded for it"))
  }
  est <- .pooled_estimate(g$meta)$est
  if (is.null(est) || length(est) != 1L || !is.finite(est)) {
    return("it has no usable pooled estimate to convert")
  }
  NULL
}

# The two arm cells of a converted row, each marked with the '*' that links it
# to the Chinn footnote. The cells hold event rates whatever the outcome was
# measured on, so `continuous = FALSE` picks the rate headers.
#
# `keep_effect_scale` does not reach here. It used to: the both-scales row put
# the mean-scale value on the first line of each cell and the responder rate on
# the second. Both mean-scale halves are gone -- a pooled control-arm mean is
# not interpretable when endpoint and change-from-baseline scores are mixed,
# which is the ordinary case -- so asking for both now splits the row in two
# rather than stacking two scales inside one cell, and these cells are the
# lower row of that pair whichever way the question was answered.
.responder_arm_cells <- function(meta_obj, args, per = 1000,
                                 big_mark = TRUE, ci_sep = "; ") {
  cer <- .format_cer(args$baseline_risk, per, big_mark = big_mark)
  ier <- .format_ier_chinn(meta_obj, args$baseline_risk, per,
                           invert = isTRUE(args$chinn_invert),
                           big_mark = big_mark, ci_sep = ci_sep)
  if (cer != "-") cer <- paste0(cer, " *")
  if (ier != "-") ier <- paste0(ier, " *")
  list(cer = cer, ier = ier, continuous = FALSE)
}

# The footnote the '*' on the converted arm cells points at. `invert` and
# `threshold_label` are woven in for a single-outcome table, whose one row owns
# the whole footnote; a combined table passes neither, because it can hold rows
# converted in opposite directions against different thresholds, and states
# those per row (.responder_row_note()). `reading` appends the two references.
#
# `baseline_risk` names the assumed control responder proportion the derived
# quantities were computed against. A combined table passes none, for the same
# reason it passes no direction: its rows can be converted against different
# proportions, and .responder_row_note() states each row's own.
.chinn_note <- function(invert = NULL, threshold_label = NULL,
                        reading = FALSE, baseline_risk = NULL,
                        label_intervention = "intervention",
                        label_control = "control") {
  invert_str <- if (is.null(invert)) {
    ""
  } else if (isTRUE(invert)) {
    paste0(" (OR direction inverted: OR > 1 = ",
           .arm_subject(label_intervention), " better)")
  } else {
    " (OR direction as given: positive SMD -> OR > 1)"
  }
  threshold_str <- if (!is.null(threshold_label) && nzchar(threshold_label)) {
    paste0(" Threshold definition: ", threshold_label, ".")
  } else ""

  paste0(
    "* Continuous outcome dichotomised via Chinn's formula ",
    "(log OR = SMD x pi / sqrt(3))", invert_str,
    ". ", .arm_label_cap(label_control), " event rate user-specified.",
    threshold_str,
    " This is NOT Core GRADE 6's option 2, which assumes a normal ",
    "distribution and computes, per study and before pooling, the ",
    "proportion in each arm improving by more than the MID; Chinn's formula ",
    "assumes a logistic latent variable, uses no MID and is applied to the ",
    "pooled SMD. The two do not generally agree.",
    " ", .chinn_derived_sentence(baseline_risk, label_control),
    if (reading) paste0(
      " Recommended reading: ",
      "Chinn S. Stat Med. 2000; ",
      "Heimke F, et al. BMJ Ment Health. 2024.") else ""
  )
}

# Which of the converted row's numbers rest on the assumed control proportion
# and which do not. Every one of them is DERIVED rather than fitted, but they
# are not derived from the same ingredients, and that difference is the whole
# reason the Effect column now reports the odds ratio: the odds ratio is
# exp(SMD x pi / sqrt(3)) and nothing else, so revising the assumed proportion
# leaves it where it is, while the two arm rates and the risk difference beside
# them all move. A reader who cannot tell the two apart either distrusts the
# whole row or trusts all of it, and neither is right.
#
# Said in one sentence shared by the single- and multi-outcome footers, because
# the two stating it differently is how a reader ends up trusting one table more
# than the other.
.chinn_derived_sentence <- function(baseline_risk = NULL,
                                    label_control = "control") {
  p0_str <- if (!is.null(baseline_risk) && length(baseline_risk) == 1L &&
                is.numeric(baseline_risk) && is.finite(baseline_risk)) {
    sprintf(" of %s", format(baseline_risk))
  } else ""
  paste0(
    "The Effect column's derived odds ratio comes from the formula above ",
    "alone and does NOT depend on the assumed ", label_control,
    " responder proportion",
    p0_str, ": revising that proportion leaves it unchanged. The two arm ",
    "rates and the Difference column's absolute risk difference DO depend on ",
    "it, and move with it. All of them are derived from the pooled estimate ",
    "rather than fitted, and all carry the formula's logistic ",
    "latent-variable assumption."
  )
}

# What .chinn_note() leaves out for a combined table: the direction and the
# threshold of ONE converted row. Keyed by outcome name, like the per-outcome
# publication-bias sentences of the same footer.
.responder_row_note <- function(nm, args,
                                label_intervention = "intervention",
                                label_control = "control") {
  dir_str <- if (isTRUE(args$chinn_invert)) {
    paste0("OR direction inverted (OR > 1 = ",
           .arm_subject(label_intervention), " better)")
  } else {
    "OR direction as given (positive SMD -> OR > 1)"
  }
  threshold_str <- if (!is.null(args$threshold_label) &&
                       nzchar(args$threshold_label)) {
    paste0(" Threshold definition: ", args$threshold_label, ".")
  } else ""
  # The assumed proportion belongs here rather than in .chinn_note(), because
  # the arm rates and the risk difference of this row are computed against it
  # and a combined table can hold rows converted against different ones. The
  # derived odds ratio is not computed against it; .chinn_derived_sentence()
  # says which is which, once, for the whole table.
  p0 <- args$baseline_risk
  p0_str <- if (!is.null(p0) && length(p0) == 1L && is.numeric(p0) &&
                is.finite(p0)) {
    sprintf(" Assumed %s responder proportion: %s.", label_control,
            format(p0))
  } else ""
  scale_str <- if (isTRUE(args$keep_effect_scale)) {
    paste0(" Shown on two rows: the effect on its own scale above, the ",
           "dichotomised reading below.")
  } else ""
  paste0("[", nm, "] Responder presentation: ", dir_str, ".", p0_str,
         threshold_str, scale_str)
}

# The row-note for an outcome that asked for the conversion and could not have
# it. Carried in grade_table()'s numbered register, so the marker sits on the
# outcome name where a reader of that row is already looking.
.responder_fallback_note <- function(reason) {
  paste0("The responder presentation was asked for but could not be applied: ",
         reason, ". This row shows the unconverted presentation instead.")
}

# What every row of a multi-outcome table asked for and what came of it: a named
# list holding, per outcome, the arguments (`args`), the converted arm cells
# (`arm`) or the reason there are none (`reason`). One resolution, shared by
# grade_table() and by the plain-text mirror of it that the bundle writes
# (.sof_set_dataframe(), export_bundle_multi.R), so the .docx and the .csv
# cannot disagree about how a row is presented.
.resolve_responder <- function(outcomes, nms = names(outcomes), per = 1000,
                               big_mark = TRUE, ci_sep = "; ",
                               label_control = "control") {
  out <- lapply(nms, function(nm) {
    g <- outcomes[[nm]]
    .check_outcome_display(g, nm)
    args <- .responder_args(g)
    if (is.null(args)) return(NULL)
    why <- .responder_unavailable_reason(g, args, label_control)
    if (!is.null(why)) return(list(args = args, reason = why, arm = NULL))
    list(args   = args,
         reason = NULL,
         arm    = .responder_arm_cells(g$meta, args, per,
                                       big_mark = big_mark, ci_sep = ci_sep))
  })
  stats::setNames(out, nms)
}

# The outcomes of a resolved table that are actually being converted.
.converted_outcomes <- function(responder) {
  nms <- names(responder)
  nms[vapply(responder, function(r) !is.null(r$arm), logical(1))]
}

# --------------------------------------------------------------------------
# The two arm cells of any Summary of Findings row
# --------------------------------------------------------------------------
#
# Core GRADE 6 calls it the preferred presentation to give the outcome in the
# comparison group, in the intervention group and the difference between the
# two. For a binary outcome the first two come from grade_meta()'s
# baseline_risk and the pooled relative effect.
#
# A continuous outcome gets neither, and the two cells are left blank. Until
# v0.6 they were filled from the arm-level summaries a metacont object carries:
# an inverse-variance weighted mean of the control arms, and that mean plus the
# pooled difference (re-expressed through a pooled within-arm reference SD when
# the measure was an SMD). Everything that machinery produced rested on a
# pooled control-arm MEAN, and a continuous meta-analysis routinely pools
# endpoint scores together with change-from-baseline scores -- the pooled
# contrast survives that, an average over the two kinds of arm summary does
# not. That case is the ordinary one rather than the exception, so the cells
# are empty rather than conditionally empty: a number that is right only when
# the reviewer happened to pool one kind of score is a number nobody can read
# without knowing something the table does not say.
#
# .pooled_control_mean(), .control_reference_sd(), .format_arm_values_cont(),
# .cont_arm_note() and .cont_arm_unavailable_reason() were deleted with the
# cells they fed. The pooled contrast is untouched: an MD still reports its
# difference in the outcome's own units (.format_difference(), sof_bmj.R).

# The pair of arm cells for any outcome. `continuous` tells the caller which
# kind it got, since the column headers differ: the rate wording and the `per`
# denominator would misdescribe a pair of cells belonging to an outcome that
# has no rates at all.
.sof_arm_cells <- function(meta_obj, baseline_risk, per = 1000,
                           big_mark = TRUE, ci_sep = "; ") {
  sm <- as.character(meta_obj$sm %||% "")
  if (sm %in% c("MD", "SMD")) {
    # Empty strings rather than "-": "-" is this table's mark for a number that
    # was expected and could not be computed, and here none was expected.
    return(list(cer = "", ier = "", continuous = TRUE))
  }
  list(cer = .format_cer(baseline_risk, per, big_mark = big_mark),
       ier = .format_ier(meta_obj, baseline_risk, per,
                         big_mark = big_mark, ci_sep = ci_sep),
       continuous = FALSE)
}

# --------------------------------------------------------------------------
# One logical outcome, two table rows
# --------------------------------------------------------------------------
#
# Asking for the effect AND the proportion of responders (keep_effect_scale)
# used to stack the two scales inside each cell, one line each. With the
# mean-scale arm cells gone there is nothing left to stack: the effect
# presentation fills the Effect column and leaves the three absolute-effect
# cells empty, the responder presentation fills all four. On one row the two
# would overwrite each other and "both" would render as "responder"; on two
# rows they read as what they are, one outcome on two scales.

# Columns of the GRADEpro layout that do not split, and those that do.
# grade_table() appends five domain columns beyond the sixth; those do not
# split either, so it extends `merge_cols` rather than editing this.
GRADEPRO_MERGED_COLS <- c(1L, 2L, 6L)
GRADEPRO_SPLIT_COLS  <- 3:5

# Merge `merge_cols` vertically across rows i and i+1 and rule the rest, so the
# pair reads as one outcome shown twice rather than as two outcomes sharing a
# certainty cell. Merging and ruling are one operation because they only work
# together: a merge with no rule hides the split, and a rule with no merge
# repeats the outcome name, the participant count and the rating.
#
# The rule is the body border theme_vanilla() already draws between rows, read
# off the flextable defaults rather than written out here. A second border
# weight in the same table would read as a second kind of division, and the
# split is not a different kind of division from the row boundaries around it.
# It is applied only to the columns that split: a rule across a merged column
# would cut the cell it is merging.
.pma_merge_split_row <- function(ft, i, merge_cols, rule_cols) {
  for (j in merge_cols) {
    ft <- flextable::merge_at(ft, i = i:(i + 1L), j = j, part = "body")
  }
  defaults <- flextable::get_flextable_defaults()
  ft <- flextable::hline(
    ft, i = i, j = rule_cols,
    border = officer::fp_border(width = defaults$border.width,
                                color = defaults$border.color),
    part = "body")
  flextable::fix_border_issues(ft)
}

# Compute experimental arm event rate from log-scale relative effect
.p1 <- function(p0, log_re, sm) {
  re <- exp(log_re)
  if (sm %in% c("RR", "HR", "IRR")) return(min(1, max(0, p0 * re)))
  if (sm == "OR") {
    p1 <- p0 * re / (1 + p0 * (re - 1))
    return(min(1, max(0, p1)))
  }
  NULL
}
