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
# pma_question_note() is here under that rule and not under the four clinical
# questions in R/step3_threshold.R, which is where the rest of the feature
# lives. What it builds is a footer line, from a rated object, for the same
# three surfaces pma_rare_event_alert() serves. It reads pma_question_of() and
# step3_per_label() out of that file - defined later in app.R's order, which is
# fine because both are called rather than evaluated at source time - the way
# every helper here reads PMA_ALERT_* out of R/ui_helpers.R.
# pma_question_notes_footer() is the combined-table form of it: one note per
# outcome, because the rows of one table can answer different questions.
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

# The question the rating answered, as a footer line.
#
# Same admission rule as pma_rare_event_alert() above, and it is here for the
# same reason: a Summary of Findings is what it is for. It builds one sentence
# that hangs off the table, three surfaces render it (the Step 3 preview, the
# Step 4 combined table, the exported .docx), and they must not word it three
# ways. It goes FIRST among the footnotes, because it frames every other one:
# a Low rating means something different depending on which claim was rated.
#
# EVERY NUMBER COMES OFF THE RATED OBJECT. `g$threshold` is what grade_meta()
# was given, `g$threshold_ard` the absolute form when it had one,
# `g$rating_target` which threshold Core GRADE 2 Fig 2 settled on, and
# `g$threshold_sides` which sides Imprecision then tested. Re-deriving any of
# it from the Configuration tab's live inputs would let the footnote name a
# number the rating did not use - which is the failure mode the tab's widget
# sync already exists to close (R/step3_threshold.R).
#
# `question` defaults to pma_question_of(g) (R/step3_threshold.R, sourced after
# this file; the default is evaluated at call time, so the order is fine). A
# caller that already knows the question passes it rather than having it
# recovered twice.
#
# NULL for a NULL object and for an unrated one. `threshold_type` is the
# marker: grade_meta() has always stored it, so every rated object carries it
# and a hand-built stub does not - and inventing "Question rated: clinically
# important superiority" for an object that was never rated would be a
# fabrication, not a default.
#
# `closing` drops the shared last sentence, PMA_QUESTION_NOTE_CLOSING. It is
# TRUE for a single-outcome table, where the note is the whole footer and the
# sentence is the point of it. A COMBINED table carries one note per outcome -
# the questions can differ per row, so one footnote for the table would be
# false of some of them - and the closing sentence is identical in every one of
# those notes, so five outcomes print it five times. The Step 4 path therefore
# asks for the notes without it and appends the sentence once, below them
# (pma_question_notes_footer()).
pma_question_note <- function(g, per = STEP3_PER_DEFAULT, question = NULL,
                              closing = TRUE) {
  if (is.null(g) || !is.list(g)) return(NULL)
  ttype <- as.character(g$threshold_type)
  if (length(ttype) != 1L || is.na(ttype) || !nzchar(ttype)) return(NULL)

  if (is.null(question)) question <- pma_question_of(g)
  asked <- PMA_QUESTION_NOTE_QUESTIONS[[question]]

  # Fig 2 decides which threshold Imprecision was handed; the "non_null_effect"
  # target is the one that means "none of it - the rating was against the null".
  rated_against_null <- identical(as.character(g$rating_target),
                                  "non_null_effect")
  thr <- if (rated_against_null) NULL else .pma_question_threshold_words(g, per)

  middle <- if (is.null(thr)) {
    paste0("The rating is against the null, so no threshold was used.")
  } else {
    sides <- if (identical(as.character(g$threshold_sides), "worse_only")) {
      sprintf("the worse side only (%s)",
              .pma_worse_side_words(g$small_values))
    } else {
      "both sides"
    }
    flip <- if (identical(question, "superiority")) {
      paste0(" The pooled estimate lies very near the null, so Core GRADE 2 ",
             "Fig 2 rated certainty in little or no difference and read the ",
             "threshold after all.")
    } else ""
    paste0(sprintf("Threshold used: %s, tested on %s.", thr, sides), flip)
  }

  paste0("Question rated: ", asked, " ", middle,
         if (isTRUE(closing)) paste0(" ", PMA_QUESTION_NOTE_CLOSING) else "")
}

# What a certainty rating IS a rating in, said once. The sentence the whole
# footnote exists for: a reader who takes "Low" as a verdict on the size of the
# effect has read the table backwards, and on an equivalence question they have
# read it backwards in the direction that flatters the intervention.
#
# A constant because two callers emit it in two places - pma_question_note()
# ends a single-outcome note with it, and pma_question_notes_footer() appends
# it once under a combined table's per-outcome notes - and a copy edit applied
# to one of them would leave a review's own two tables disagreeing about what
# their certainty ratings mean.
PMA_QUESTION_NOTE_CLOSING <-
  "Certainty is rated in that claim, not in the size of the effect."

# The question footnotes for a table of one or more outcomes: one line per
# rated outcome, then the shared closing sentence once.
#
# `outcomes` is a named list of rated objects (a not-reported row has no
# threshold_type, so pma_question_note() drops it). `per` is the display unit,
# for the same reason pma_question_note() takes one.
#
# One note per outcome rather than one per table, because a combined Summary of
# Findings can mix questions across its rows: a review may rate superiority of
# one outcome and non-inferiority of another, and a single footnote would be
# false of whichever rows it did not describe. The row is named in the note so
# a reader can tell which is which.
#
# character(0) when nothing in the table was rated, so pma_sof_add_notes()
# leaves the flextable alone.
pma_question_notes_footer <- function(outcomes, per = STEP3_PER_DEFAULT) {
  nms <- names(outcomes) %||% rep("", length(outcomes))
  notes <- character(0)
  for (i in seq_along(outcomes)) {
    note <- pma_question_note(outcomes[[i]], per = per, closing = FALSE)
    if (is.null(note)) next
    label <- as.character(nms[[i]] %||% "")
    notes <- c(notes, if (nzchar(trimws(label))) {
      paste0(label, ": ", note)
    } else {
      note
    })
  }
  if (!length(notes)) return(character(0))
  c(notes, PMA_QUESTION_NOTE_CLOSING)
}

# The footnote's name for each question. Deliberately NOT the radio labels in
# EDU_COPY$config_tab$question_labels: those are choices in a list the reviewer
# is reading top to bottom, and these are read alone, under a table, possibly
# in a .docx a year later. Same four questions, said so they stand up out of
# context - "the intervention" rather than "it".
#
# The word "MID" appears in none of them; the screen says Threshold
# (shiny/SPEC.md 4.5.1), and a footnote is the screen.
PMA_QUESTION_NOTE_QUESTIONS <- c(
  superiority = paste0(
    "superiority - is there any effect at all?"),
  important_superiority = paste0(
    "clinically important superiority - is the effect large enough to ",
    "matter?"),
  equivalence = paste0(
    "equivalence - is the difference small enough to be unimportant in ",
    "either direction?"),
  non_inferiority = paste0(
    "non-inferiority - is the intervention no worse than the comparator by ",
    "more than the threshold?")
)

# The threshold the rating used, in the units it was given in. The absolute
# form wins when there is one, because that is the scale Core GRADE 2 asks for
# and the one the reviewer typed; otherwise the value with its measure in
# front, so "1.25" cannot be read as a risk difference.
#
# NULL when the object carries no usable threshold, which the caller renders as
# the against-the-null sentence rather than as a blank.
.pma_question_threshold_words <- function(g, per = STEP3_PER_DEFAULT) {
  .usable <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)

  ard <- g$threshold_ard
  if (.usable(ard)) return(step3_per_label(1000 * ard, per))

  thr <- g$threshold
  if (!.usable(thr)) return(NULL)
  num <- format(signif(thr, 4), trim = TRUE, scientific = FALSE)
  sm  <- as.character((g$meta$sm %||% "")[1])
  if (!is.na(sm) && nzchar(sm)) paste(sm, num) else num
}

# The worse side named for a reader of the table, from the same
# .threshold_worse_sign() the rating asked. Never a guess: an unanswered
# direction says so, because a footnote claiming a side the analysis did not
# record would be the one sentence in the table nobody could check.
.pma_worse_side_words <- function(small_values) {
  answered <- is.character(small_values) && length(small_values) == 1L &&
    !is.na(small_values) && small_values %in% c("desirable", "undesirable")
  if (!answered) return("the worse side was not recorded for this outcome")
  if (.threshold_worse_sign(small_values) > 0) {
    "higher values of this outcome are the worse ones"
  } else {
    "lower values of this outcome are the worse ones"
  }
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

# Append free-text footer lines to a Summary of Findings flextable, keeping
# the footer styling the vendored builders apply. What it carries now is the
# rare-event caution, one line per affected outcome, so that caution reaches
# the exported .docx and not just the screen. It stays general over a vector
# of notes: a caller passing none (no outcome triggered the caution) gets the
# flextable back untouched.
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
