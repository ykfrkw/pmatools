# sof_display.R - the Summary of Findings table as the app presents it
#
# Split out of ui_helpers.R, and the section separator it carried is kept
# below. Three surfaces render a Summary of Findings - the Step 3 preview, the
# Step 4 combined table and the exported sof_table.docx - and they are built
# from the constants and helpers here so that they cannot disagree. That is not
# a tidiness argument: the Step 3 preview once said "With intervention" one
# screen before Step 4 said "With CBT-I", from the same analysis, because
# pma_arm_labels() was a closure inside step4_server() that Step 3 could not
# reach.
#
# PMA_SOF_STYLE pins the Core GRADE 6 eight-column layout everywhere. The
# app does NOT offer the package default ("gradepro", six columns) as an
# alternative, for the two reasons written above the constant.
#
# THE RULE FOR A NEW HELPER: it belongs here when a Summary of Findings is what
# it is for - a column's wording, a footnote, the arm words, an event-rate
# derivation, the rare-event alert that hangs above the table. A helper that
# builds the table itself does not: grade_table() is the package's, and the
# app must keep feeding it rather than growing a second implementation.
#
# pma_arm_labels() reads `state`, never `input`, and every SoF helper that
# needs arm words takes them as an argument defaulting to
# PMA_ARM_LABELS_DEFAULT. Step 2's two selects are destroyed when the wizard
# leaves the step, so an `input` read would silently fall back to the generic
# wording exactly when a real label existed. Preserve that split.
#
# PMA_ALERT_BG / PMA_ALERT_FG are read from R/ui_helpers.R at call time: the
# rare-event banner is painted in the app's one alert colour rather than a
# second amber of its own.

# ==========================================================================
# Summary of Findings presentation (Core GRADE 6)
# ==========================================================================
#
# Core GRADE 6 (Guyatt G, Yao L, Murad MH, et al. BMJ 2025;389:e083866)
# presents an eight-column summary of findings table. pmatools 0.5 ships that
# layout as style = "bmj"; the package default, "gradepro", is a six-column
# layout that omits follow-up, study design, the difference between arms, the
# rate-down reason and the plain language summary.
#
# The app renders the Core GRADE 6 layout everywhere and does NOT offer the
# GRADEpro layout as an alternative. Two reasons:
#   1. Every other output in the wizard is written against the Core GRADE
#      series, so a second layout would be a second, unsourced standard.
#   2. The on-screen table, the exported sof_table.docx and the combined
#      table are all built from this one constant, so they cannot disagree.
# The Evidence Profile is a different table (nine columns, Core GRADE's
# evidence-profile format) and is unaffected.
PMA_SOF_STYLE   <- "bmj"
PMA_SOF_PALETTE <- "pastel"

# The arm words every Summary of Findings in the app speaks, resolved once.
#
# `state`, never `input`: Step 2's two selects are destroyed when the wizard
# leaves the step, so by the time a SoF is rendered the inputs read NULL and
# both labels fall back to the generic wording. state$arm_e / state$arm_c are
# written in step2_ma.R and survive the round trip.
#
# This lived inside step4_server() as a closure, which is why the Step 3
# preview could not call it and rendered "With intervention" one screen before
# Step 4 rendered "With CBT-I" -- the same table, naming the arms two ways.
# Both steps take it from here now, so they cannot disagree again.
#
# The fallbacks are pmatools' own defaults for label_intervention /
# label_control, so an unmapped analysis renders exactly as the package does.
PMA_ARM_LABELS_DEFAULT <- list(intervention = "intervention",
                               control      = "control")

pma_arm_labels <- function(state) {
  .usable <- function(x) !is.null(x) && length(x) == 1L && !is.na(x) &&
    nzchar(as.character(x))
  e  <- if (!is.null(state)) state$arm_e else NULL
  c_ <- if (!is.null(state)) state$arm_c else NULL
  list(
    intervention = if (.usable(e))  as.character(e)  else "intervention",
    control      = if (.usable(c_)) as.character(c_) else "control"
  )
}

# Percentage with enough resolution to distinguish the two rare-event bands.
pma_fmt_pct <- function(p, digits = 2) {
  if (is.null(p) || length(p) != 1L || !is.finite(p)) return("not estimable")
  paste0(formatC(100 * p, format = "f", digits = digits), "%")
}

# Free-text follow-up as it is printed under the outcome name. Core GRADE 6
# writes the cell as "Follow-up: ...", so the prefix is supplied here when the
# user did not type one.
pma_sof_follow_up <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(NULL)
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(NULL)
  if (grepl("^follow[ -]?up", x, ignore.case = TRUE)) x else paste0("Follow-up: ", x)
}

# Unit for the Difference column of a continuous outcome, and for nothing else.
#
# The unit the reviewer typed describes the measurement scale, so it is the
# right label for a mean difference, which is on that scale. Everything else
# gets NULL: an SMD's Difference cell is empty as of v0.6 (a standard-deviation
# string there only restates the Effect column), and a ratio measure has no
# unit at all, its difference being printed "per 1000".
#
# This used to return "standard deviation units" for an SMD, and the same value
# reached sof_table()'s `unit`, which then labelled the ARM columns with it: a
# control mean already re-expressed on the outcome's own scale printed as
# "13.89 standard deviation units". Those columns are gone, so the mislabel
# went with them, and `unit` now has exactly one destination.
pma_sof_unit <- function(g, unit = NULL) {
  sm <- as.character((g$meta$sm %||% "")[1])
  if (!identical(sm, "MD")) return(NULL)
  if (is.null(unit) || length(unit) != 1L || is.na(unit)) return(NULL)
  unit <- trimws(as.character(unit))
  if (nzchar(unit)) unit else NULL
}

# Crude arm-level and pooled event rates, straight off the metabin arms.
# Returns NULL for anything that is not a binary meta-analysis.
pma_sof_event_rates <- function(meta_obj) {
  if (is.null(meta_obj)) return(NULL)
  ee <- meta_obj$event.e; ne <- meta_obj$n.e
  ec <- meta_obj$event.c; nc <- meta_obj$n.c
  if (is.null(ee) || is.null(ne) || is.null(ec) || is.null(nc)) return(NULL)
  if (length(ee) != length(ne) || length(ec) != length(nc)) return(NULL)
  keep_e <- is.finite(ee) & is.finite(ne) & ne > 0
  keep_c <- is.finite(ec) & is.finite(nc) & nc > 0
  if (!any(keep_e) && !any(keep_c)) return(NULL)
  ev_e <- sum(ee[keep_e]); n_e <- sum(ne[keep_e])
  ev_c <- sum(ec[keep_c]); n_c <- sum(nc[keep_c])
  n_tot <- n_e + n_c
  if (!is.finite(n_tot) || n_tot <= 0) return(NULL)
  list(
    intervention = if (n_e > 0) ev_e / n_e else NA_real_,
    control      = if (n_c > 0) ev_c / n_c else NA_real_,
    overall      = (ev_e + ev_c) / n_tot,
    events       = ev_e + ev_c,
    n            = n_tot
  )
}

# Core GRADE 6's rare-event trap.
#
# The Difference column and the "With intervention" column are both computed
# by applying the pooled relative effect to a baseline risk. Core GRADE 6
# warns that this misleads when the outcome is rare, naming two bands --
# "event rates <2% and most problematic <1%" -- and recommends that review
# authors generally conduct meta-analyses of risk differences instead.
#
# `g`             a pmatools object.
# `baseline_risk` the risk the table is actually drawn against when it is not
#                 the object's own (the responder proportion of a Chinn
#                 dichotomisation).
#
# Returns NULL when nothing is rare, otherwise a list with the band, the
# computed rates, a one-line headline and the full note. The note is the same
# text on screen and in the exported docx.
PMA_RARE_BAND_1 <- 0.01
PMA_RARE_BAND_2 <- 0.02

pma_rare_event_alert <- function(g, baseline_risk = NULL, label = NULL,
                                 labels = PMA_ARM_LABELS_DEFAULT) {
  if (is.null(g)) return(NULL)
  rates <- pma_sof_event_rates(g$meta)
  br    <- baseline_risk
  if (is.null(br)) br <- g$baseline_risk
  if (!is.numeric(br) || length(br) != 1L || !is.finite(br)) br <- NA_real_

  # Every rate the absolute-effect columns rest on: the two observed arm
  # rates, the pooled rate, and the baseline risk the table is drawn against
  # (which the reviewer may have set by hand).
  cand <- c(rates$overall, rates$control, rates$intervention, br)
  cand <- cand[is.finite(cand) & cand > 0]
  if (length(cand) == 0) return(NULL)
  lowest <- min(cand)
  if (lowest >= PMA_RARE_BAND_2) return(NULL)

  band <- if (lowest < PMA_RARE_BAND_1) "below 1%" else "below 2%"

  observed <- if (is.null(rates)) {
    ""
  } else {
    sprintf(paste0(
      "Observed event rates: %s overall (%s of %s participants), %s in the ",
      "%s arm, %s in the %s arm. "),
      pma_fmt_pct(rates$overall),
      format(rates$events, big.mark = ",", scientific = FALSE, trim = TRUE),
      format(rates$n,      big.mark = ",", scientific = FALSE, trim = TRUE),
      pma_fmt_pct(rates$control), labels$control,
      pma_fmt_pct(rates$intervention), labels$intervention)
  }
  baseline_txt <- if (is.finite(br)) {
    sprintf("Baseline risk used for the absolute columns: %s. ", pma_fmt_pct(br))
  } else ""

  headline <- sprintf("Rare outcome%s - lowest event rate %s (%s).",
                      if (is.null(label)) "" else paste0(" (", label, ")"),
                      pma_fmt_pct(lowest), band)

  detail <- paste0(
    observed, baseline_txt,
    "Core GRADE 6 warns that applying a relative effect to a baseline risk is ",
    "misleading for rare outcomes, at \"event rates <2% and most problematic ",
    "<1%\", and recommends that review authors generally conduct ",
    "meta-analyses of risk differences instead ",
    "(Guyatt et al. BMJ 2025;389:e083866). ",
    "The Difference column and the \"With ", labels$intervention,
    "\" column are still ",
    "computed from the baseline risk and the pooled relative effect, and are ",
    "shown unchanged; read them with that warning in mind, and consider ",
    "reporting the risk difference from Step 2's rare-events workflow instead."
  )
  note <- paste0("Rare-event caution (Core GRADE 6). ", headline, " ", detail)

  list(band = band, lowest = lowest, rates = rates, baseline_risk = br,
       headline = headline, detail = detail, note = note, label = label)
}

# Amber banner for a rare-event alert. NULL-safe so callers can drop the
# result straight into a tagList.
pma_rare_event_banner <- function(alert) {
  if (is.null(alert)) return(NULL)
  htmltools::div(
    style = paste0(
      "padding: 0.75rem 1rem; margin-bottom: 1rem; ",
      "background: ", PMA_ALERT_BG, "; border-left: 4px solid ", PMA_ALERT_FG,
      "; border-radius: 4px; font-size: 0.9rem;"),
    htmltools::strong(paste0("Rare events. ", alert$headline, " ")),
    alert$detail
  )
}

# What is left of the Core GRADE 6 features pmatools does not fully implement.
# Two of the three have since shrunk to caveats: pmatools now fills the
# arm-level columns of a continuous outcome from the control arms (but only
# when the analysis carries them), and now footnotes the numbers behind a
# downgrade (but only for the three domains that record them). The third -
# "\"Not reported\" rows: outcomes the evidence base did not measure are
# absent from this table" - is gone entirely: the reviewer adds those rows from
# Step 4's "+ Add next outcome" (pma_not_reported_modal()).
#
# It lives in the table footer only: a second copy used to sit under the table
# as page text, which said the same thing twice in two different fonts, and the
# footer is the copy that travels into the .docx.
# `labels` rather than a constant, because this sentence names the table's own
# arm columns and a footnote that calls them something the headers do not is a
# footnote about a different table. "the value with X" mirrors the column head
# "With X" exactly; the old "<label>-group value" shape does not survive a
# free-text label ("CBT-I-group value").
pma_sof_limitations_note <- function(labels = PMA_ARM_LABELS_DEFAULT) paste0(
  "Not implemented in this table (Core GRADE 6 features pmatools does not yet ",
  "produce). Arm-level values for continuous outcomes -- the value with ",
  labels$control, ", the value with ", labels$intervention,
  " and the difference, ",
  "which Core GRADE 6 calls its preferred approach -- are now reported, ",
  "except where the analysis carries no arm-level means (a generic ",
  "inverse-variance analysis) or uses a ratio-of-means measure; those still ",
  "leave the two arm columns empty. Per-domain footnotes now state what drove ",
  "each downgrade for risk of bias, inconsistency and imprecision; a rated ",
  "down indirectness or publication bias domain is still only named in the ",
  "certainty cell, with its reasoning left in the Evidence Profile and in ",
  "that domain's notes."
)

# Core GRADE 6's own presentation advice, as a footnote on the table it is
# about. It used to be a standing italic paragraph on the Final certainty tab,
# where it was page text and therefore did not travel into the exported .docx.
# The arm words are NOT substituted into "control event rate" / "intervention
# event rate": CER and EER are the cited source's own acronyms and they stop
# deriving from the words the moment the words change ("the placebo event rate
# (CER)"). What the reviewer needs is to find the columns, so the columns are
# named instead -- and those follow the labels, because the headers do.
pma_sof_cer_eer_note <- function(labels = PMA_ARM_LABELS_DEFAULT) paste0(
  "Recommended: report both the control event rate (CER, the \"With ",
  labels$control, "\" column) and the intervention event rate (EER, the ",
  "\"With ", labels$intervention, "\" column) alongside the relative ",
  "effect, to aid clinical interpretation ",
  "(Heimke F, et al. BMJ Ment Health. 2024)."
)

# Append free-text footer lines to a Summary of Findings flextable, keeping
# the footer styling the vendored builders apply. Used for the rare-event
# caution and the limitations statement, so both reach the exported .docx.
pma_sof_add_notes <- function(ft, notes) {
  if (is.null(ft)) return(ft)
  notes <- notes[!vapply(notes, function(z) is.null(z) || is.na(z) ||
                           !nzchar(z), logical(1))]
  if (length(notes) == 0) return(ft)
  for (nt in notes) {
    ft <- flextable::add_footer_lines(ft, values = as.character(nt))
  }
  ft <- .style_table_footer(ft)
  ft
}

# The Core GRADE 6 layout is wider than the GRADEpro one (eight fixed-width
# columns totalling ~10.3 in against ~8.3 in), so every on-screen SoF table is
# wrapped in a horizontal scroller rather than being allowed to stretch the
# card. Colours, fonts and the dark header are unchanged.
pma_sof_scroller <- function(...) {
  htmltools::div(style = "overflow-x: auto; margin-top: 0.5rem;", ...)
}
