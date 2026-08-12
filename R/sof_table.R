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
#'   \code{"bmj"} style with continuous outcomes, e.g. \code{"days"}.
#'   Ignored for relative effect measures and by the \code{"gradepro"} style.
#' @param palette Color palette for the certainty cell.
#'   \code{"pastel"} (default) uses soft backgrounds with colored text.
#'   \code{"classic"} uses saturated backgrounds with white text.
#' @param per Denominator for event rate columns. \code{1000} (default) or
#'   \code{100}. Controls the scale of the "Risk with control" and
#'   "Risk with intervention" columns.
#' @param prediction Logical. If \code{TRUE} (default \code{FALSE}), the
#'   Effect column also shows the 95 percent prediction interval on a second line,
#'   provided the meta object was run with \code{prediction = TRUE}.
#' @param convert_smd_to_or (v0.2) Logical. If \code{TRUE} and the meta
#'   object uses \code{sm = "SMD"} or \code{"MD"}, the "Risk with control" /
#'   "Risk with intervention" columns display dichotomised event rates derived via Chinn's
#'   formula (\eqn{\log OR = SMD \times \pi / \sqrt{3}}). Requires
#'   \code{baseline_risk} (numeric in (0,1)) representing the proportion
#'   of control patients meeting the threshold of clinical interest.
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
                      baseline_risk     = NULL,
                      threshold_label   = NULL,
                      chinn_invert      = FALSE,
                      label_intervention = "intervention",
                      label_control      = "control",
                      ...) {
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
  cer_str     <- .format_cer(baseline_for_display, per, big_mark = nf$big_mark)
  ier_str     <- if (chinn_active) {
    .format_ier_chinn(meta_obj, baseline_risk, per, invert = isTRUE(chinn_invert),
                      big_mark = nf$big_mark, ci_sep = nf$ci_sep)
  } else {
    .format_ier(meta_obj, x$baseline_risk, per,
                big_mark = nf$big_mark, ci_sep = nf$ci_sep)
  }
  # Asterisk-mark CER/EER when Chinn dichotomisation is active
  if (chinn_active) {
    if (cer_str != "-") cer_str <- paste0(cer_str, " *")
    if (ier_str != "-") ier_str <- paste0(ier_str, " *")
  }

  # BMJ Core GRADE presentation is a different table shape entirely, so it is
  # built by its own routine; everything below stays the GRADEpro layout.
  if (identical(style, "bmj")) {
    return(.sof_table_bmj(
      x, pal = pal, per = per, prediction = prediction,
      cer_str = cer_str, ier_str = ier_str,
      baseline_for_display = baseline_for_display,
      follow_up = follow_up, unit = unit,
      chinn_active = chinn_active, chinn_invert = chinn_invert,
      threshold_label = threshold_label,
      label_intervention = label_intervention,
      label_control      = label_control
    ))
  }

  effect_str  <- .format_effect(meta_obj, x$outcome_type,
                                prediction = prediction)

  certainty_label <- x$certainty
  certainty_sym   <- CERTAINTY_SYMBOLS_UNICODE[[certainty_label]]
  cell_colors     <- pal[[certainty_label]]

  per_str <- format(per, big.mark = ",", scientific = FALSE)
  headers <- c(
    "Outcome",
    "No. of participants\n(studies)",
    paste0("Risk with ", label_control, "\n(per ", per_str, ")"),
    paste0("Risk with ", label_intervention, "\n(per ", per_str, ")"),
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

  df <- data.frame(
    col1 = x$outcome_name,
    col2 = .n_participants_studies(k, n_total, x$study_design),
    col3 = cer_str,
    col4 = ier_str,
    col5 = effect_str,
    col6 = certainty_cell,
    stringsAsFactors = FALSE
  )
  names(df) <- headers

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, .list = stats::setNames(as.list(headers), headers))
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 2, align = "center", part = "body")

  cert_col <- headers[6]
  ft <- flextable::bg(ft,    j = cert_col, bg    = cell_colors$bg,   part = "body")
  ft <- flextable::color(ft, j = cert_col, color = cell_colors$text, part = "body")
  ft <- flextable::bold(ft,  j = cert_col, part = "body")
  ft <- flextable::align(ft, j = cert_col, align = "center", part = "body")

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
    "Assessment based on BMJ 2025 Core GRADE series (Guyatt et al.); ",
    "not an official GRADE Working Group assessment. ",
    "CI = confidence interval.", pi_note, " ",
    "Intervention rate (Risk with ", label_intervention, ") = ",
    "intervention-arm event rate computed from baseline risk and pooled ",
    "relative effect."
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
    invert_str <- if (isTRUE(chinn_invert)) {
      " (OR direction inverted: OR > 1 = treatment better)"
    } else {
      " (OR direction as given: positive SMD -> OR > 1)"
    }
    threshold_str <- if (!is.null(threshold_label) && nzchar(threshold_label)) {
      paste0(" Threshold definition: ", threshold_label, ".")
    } else ""

    chinn_note <- paste0(
      "* Continuous outcome dichotomised via Chinn's formula ",
      "(log OR = SMD x pi / sqrt(3))", invert_str,
      ". Control event rate user-specified.", threshold_str,
      " This is NOT Core GRADE 6's option 2, which assumes a normal ",
      "distribution and computes, per study and before pooling, the ",
      "proportion in each arm improving by more than the MID; Chinn's formula ",
      "assumes a logistic latent variable, uses no MID and is applied to the ",
      "pooled SMD. The two do not generally agree.",
      " Recommended reading: ",
      "Chinn S. Stat Med 2000;19:3127-3131. ",
      "doi:10.1002/1097-0258(20001130)19:22<3127::aid-sim784>3.0.co;2-m. ",
      "Heimke F, Furukawa Y, Siafis S, et al. ",
      "BMJ Ment Health 2024;27:e300978. ",
      "doi:10.1136/bmjment-2023-300978."
    )
    ft <- flextable::add_footer_lines(ft, values = chinn_note)
  }

  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::color(ft, color = "#555555", part = "footer")

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
  x <- flextable::fontsize(x, size = 8, part = "footer")
  x <- flextable::color(x, color = "#555555", part = "footer")
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
  clauses <- sprintf("%s: %s", facts$label, facts$value)
  clauses <- sub("\\.+$", "", clauses)
  clauses <- clauses[nzchar(clauses)]
  if (length(clauses) == 0L) return(NULL)
  paste0(paste(clauses, collapse = ". "), ".")
}

# One footnote line's worth of text for a domain, or NULL. `outcome_name`
# names the row in a multi-outcome table, where one footer serves several
# ratings.
.domain_fact_note <- function(x, domain, outcome_name = NULL) {
  body <- .domain_fact_body((x$domain_facts %||% list())[[domain]])
  if (is.null(body)) return(NULL)
  head <- if (!is.null(outcome_name) && length(outcome_name) == 1L &&
               !is.na(outcome_name) && nzchar(outcome_name)) {
    sprintf("%s (%s).", domain, outcome_name)
  } else {
    paste0(domain, ".")
  }
  paste(head, body)
}

# Domains that pulled the rating down AND have facts to show for it, in
# domain_assessments order. A domain that did not rate down needs no
# explanation in the footer.
.rated_down_fact_domains <- function(x) {
  all_facts <- x$domain_facts %||% list()
  if (length(all_facts) == 0L) return(character(0))
  d <- x$domain_assessments
  if (is.null(d) || nrow(d) == 0L) return(character(0))
  dg   <- d$downgrade
  doms <- d$domain[!is.na(dg) & dg < 0]
  doms[doms %in% names(all_facts)]
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

# Experimental rate via Chinn (SMD/MD -> OR -> p1)
# `invert = TRUE` flips the SMD sign before applying the formula, so a
# negative-is-better SMD (e.g., depression severity reduction) yields OR > 1.
.format_ier_chinn <- function(meta_obj, baseline_risk, per = 1000, invert = FALSE,
                              big_mark = TRUE, ci_sep = "; ") {
  if (is.null(baseline_risk)) return("-")
  pooled <- .pooled_estimate(meta_obj)
  est <- pooled$est
  lo  <- pooled$lower
  hi  <- pooled$upper
  if (is.null(est) || is.na(est)) return("-")

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

  if (is.null(p1_est)) return("-")

  per_str <- .per_label(per, big_mark)
  sprintf("%d per %s\n(%d%s%d)",
          round(p1_est * per), per_str,
          round(p1_lo  * per), ci_sep,
          round(p1_hi  * per))
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
