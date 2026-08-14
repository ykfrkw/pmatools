# sof_bmj.R — BMJ Core GRADE presentation of the Summary of Findings table
#
# References:
#   Guyatt G, Yao L, Murad MH, et al.
#     Core GRADE 6: presenting the evidence in summary of findings tables.
#     BMJ. 2025;389:e083866. -- Box 1 (plain language summaries) and the
#     column layout reproduced here:
#
#     Outcome and     | No of participants | Relative | Absolute effects (95% CI) | Certainty  | Plain
#     follow-up       | (No of studies     | effect   |---------------------------| of         | language
#                     |  and type)         | (95% CI) | control | interv. | Diff  | evidence   | summary
#
# The GRADEpro layout stays the default (see sof_table()); everything here is
# reached only through style = "bmj".

# --- small formatting helpers ----------------------------------------------

# Number formatting switch for the shared SoF helpers (.format_cer(),
# .format_ier(), .format_ier_chinn() in sof_table.R).
#
# The BMJ tables print rates without a thousands separator ("578 per 1000")
# and separate every confidence interval with "to" ("129 fewer to 42 fewer",
# "0.69 to 0.89"), so all three absolute-effect columns read alike. GRADEpro
# keeps "306 per 1,000" and "(174; 245)"; those are the helpers' defaults, so
# passing the "gradepro" values here is a no-op by construction.
.bmj_number_format <- function(style = c("gradepro", "bmj")) {
  style <- match.arg(style)
  if (identical(style, "bmj")) {
    list(big_mark = FALSE, ci_sep = " to ")
  } else {
    list(big_mark = TRUE,  ci_sep = "; ")
  }
}

# BMJ spells the effect measure out ("Hazard ratio 0.78") rather than
# abbreviating it ("HR 0.78").
.bmj_measure_name <- function(sm) {
  switch(
    as.character(sm %||% ""),
    "RR"  = "Risk ratio",
    "OR"  = "Odds ratio",
    "HR"  = "Hazard ratio",
    "IRR" = "Incidence rate ratio",
    "RoM" = "Ratio of means",
    "RD"  = "Risk difference",
    "MD"  = "Mean difference",
    "SMD" = "Standardised mean difference",
    "Effect"
  )
}

# "Hazard ratio 0.78 (0.69 to 0.89)" — spelled-out measure, "to" separator.
.format_effect_bmj <- function(meta_obj, outcome_type, prediction = FALSE) {
  sm     <- meta_obj$sm
  pooled <- .pooled_estimate(meta_obj)
  est <- pooled$est
  lo  <- pooled$lower
  hi  <- pooled$upper
  if (is.null(est) || is.na(est)) return("NR")

  ratio <- identical(outcome_type, "relative") && !is.null(sm) &&
    sm %in% c("RR", "OR", "HR", "IRR")
  if (ratio) {
    est <- exp(est); lo <- exp(lo); hi <- exp(hi)
  }
  s <- sprintf("%s %.2f (%.2f to %.2f)", .bmj_measure_name(sm), est, lo, hi)

  if (prediction) {
    pi_lo <- meta_obj$lower.predict
    pi_hi <- meta_obj$upper.predict
    if (!is.null(pi_lo) && !is.null(pi_hi) && !is.na(pi_lo) && !is.na(pi_hi)) {
      if (ratio) { pi_lo <- exp(pi_lo); pi_hi <- exp(pi_hi) }
      s <- paste0(s, sprintf("\n95%% prediction interval %.2f to %.2f",
                             pi_lo, pi_hi))
    }
  }
  s
}

# "1821 (11 non-randomised studies)" / "240 (one randomised controlled trial)".
# BMJ prints participant counts without a thousands separator and spells out a
# single study.
.n_participants_studies_bmj <- function(k, n_total, study_design = NULL) {
  k <- as.integer(k)
  single <- !is.na(k) && k == 1L

  design_lbl <- if (is.null(study_design) || length(study_design) != 1L ||
                    is.na(study_design) || !nzchar(study_design)) {
    if (single) "study" else "studies"
  } else if (toupper(study_design) == "RCT") {
    if (single) "randomised controlled trial" else "randomised controlled trials"
  } else if (tolower(study_design) %in% c("obs", "observational")) {
    if (single) "non-randomised study" else "non-randomised studies"
  } else {
    if (single) "study" else "studies"
  }

  k_str <- if (single) "one" else as.character(k)
  n_str <- if (is.na(n_total)) "NR" else format(n_total, scientific = FALSE,
                                                trim = TRUE)
  sprintf("%s (%s %s)", n_str, k_str, design_lbl)
}

# Natural-language enumeration: "a", "a and b", "a, b and c".
.and_list <- function(x) {
  x <- x[nzchar(x)]
  n <- length(x)
  if (n == 0L) return("")
  if (n == 1L) return(x)
  if (n == 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-n], collapse = ", "), " and ", x[n])
}

# Severity adjective for the "Due to ..." sentence. This used to be a second
# hand-written wording map, kept in step with evidence_profile() by hand; the
# two drifting apart is exactly the failure .grade_level_wording() exists to
# prevent, so it now defers.
.fmt_severity <- function(judgment) {
  .grade_level_wording(judgment)
}

# "Due to serious risk of bias and imprecision" — the second line of the BMJ
# certainty cell. Returns NULL when nothing pulled the rating down.
#
# `markers` is a named integer vector keyed by domain name; when supplied, the
# footnote marker for a domain is appended to that domain's name inside the
# sentence ("Due to serious risk of bias [1] and inconsistency [2]"), which is
# where a reader looks for it. A NULL or empty `markers` leaves the sentence
# byte-identical to what it has always been.
.certainty_rate_down_reason <- function(x, markers = NULL) {
  sd  <- x$study_design %||% ""
  obs <- (length(sd) == 1L && !is.na(sd) &&
            tolower(sd) %in% c("obs", "observational")) ||
    identical(x$starting_quality, "Low")

  dom_parts <- character(0)
  d <- x$domain_assessments
  if (!is.null(d) && nrow(d) > 0) {
    dg  <- d$downgrade
    sel <- !is.na(dg) & dg < 0
    if (any(sel)) {
      dn   <- d[sel, , drop = FALSE]
      adj  <- vapply(dn$judgment, .fmt_severity, character(1),
                     USE.NAMES = FALSE)
      doms <- tolower(dn$domain)
      if (!is.null(markers) && length(markers) > 0L) {
        mk   <- markers[dn$domain]
        doms <- ifelse(is.na(mk), doms, sprintf("%s [%d]", doms, mk))
      }
      dom_parts <- if (length(unique(adj)) == 1L) {
        # Shared adjective is stated once ("serious risk of bias and
        # imprecision"), as in the BMJ tables.
        paste0(adj[1], " ", .and_list(doms))
      } else {
        paste(adj, doms)
      }
    }
  }

  reasons <- c(if (obs) "non-randomised studies", dom_parts)
  if (length(reasons) == 0L) return(NULL)
  paste0("Due to ", .and_list(reasons))
}

# --- Difference column -----------------------------------------------------

# Render an absolute difference with its CI, choosing "fewer"/"more" from the
# sign of each value. A negative difference means fewer events (or a lower
# score) with the intervention.
#
# Binary:     "88 fewer per 1000 (129 fewer to 42 fewer)"
# Continuous: "12.96 more days (16.23 fewer to 42.15 more)"
.difference_string <- function(d, d_lo, d_hi, unit = NULL, digits = 0L) {
  vals <- c(d, d_lo, d_hi)
  if (length(vals) != 3L || !all(is.finite(vals))) return("-")
  # CI bounds are reported low-to-high on the difference scale, whichever
  # direction the effect measure runs in.
  if (d_lo > d_hi) {
    tmp <- d_lo; d_lo <- d_hi; d_hi <- tmp
  }

  direction <- function(v) if (v < 0) "fewer" else "more"
  fmt <- function(v) {
    if (digits <= 0L) {
      format(round(abs(v)), scientific = FALSE, trim = TRUE)
    } else {
      sprintf(paste0("%.", digits, "f"), abs(v))
    }
  }

  unit_str <- if (!is.null(unit) && length(unit) == 1L && !is.na(unit) &&
                  nzchar(unit)) paste0(" ", unit) else ""

  sprintf("%s %s%s (%s %s to %s %s)",
          fmt(d), direction(d), unit_str,
          fmt(d_lo), direction(d_lo),
          fmt(d_hi), direction(d_hi))
}

# Absolute difference between arms, derived from the baseline risk and the
# pooled relative effect (binary) or straight from the pooled estimate
# (continuous). Falls back to "-" whenever the ingredients are missing.
.format_difference <- function(meta_obj, baseline_risk, per = 1000,
                               unit = NULL, outcome_type = "relative") {
  pooled <- .pooled_estimate(meta_obj)
  est <- pooled$est
  lo  <- pooled$lower
  hi  <- pooled$upper
  if (is.null(est) || length(est) != 1L || !is.finite(est)) return("-")

  sm <- meta_obj$sm %||% ""
  # Same denominator label as the control/intervention columns (no separator).
  per_str <- .per_label(per, big_mark = FALSE)

  if (sm %in% c("RR", "OR", "HR", "IRR")) {
    if (is.null(baseline_risk)) return("-")
    p1    <- .p1(baseline_risk, est, sm)
    p1_lo <- .p1(baseline_risk, lo,  sm)
    p1_hi <- .p1(baseline_risk, hi,  sm)
    if (is.null(p1) || is.null(p1_lo) || is.null(p1_hi)) return("-")
    return(.difference_string(
      (p1    - baseline_risk) * per,
      (p1_lo - baseline_risk) * per,
      (p1_hi - baseline_risk) * per,
      unit = paste0("per ", per_str), digits = 0L
    ))
  }

  if (identical(sm, "RD")) {
    return(.difference_string(est * per, lo * per, hi * per,
                              unit = paste0("per ", per_str), digits = 0L))
  }

  if (identical(sm, "MD")) {
    return(.difference_string(est, lo, hi, unit = unit, digits = 2L))
  }

  # An SMD is in standard deviation units, so it never takes the outcome's own
  # unit -- and now that the two arm columns beside it can be re-expressed on
  # the original scale, labelling it "days" would invite the reader to subtract
  # one from the other and find the numbers do not agree.
  if (identical(sm, "SMD")) {
    return(.difference_string(est, lo, hi, unit = "standard deviations",
                              digits = 2L))
  }

  "-"
}

# --- Table assembly --------------------------------------------------------

# One BMJ row's worth of cell text. `cer_str` / `ier_str` may be supplied by
# the caller when a Chinn dichotomisation already computed them.
.bmj_row_values <- function(nm, g, per = 1000, prediction = FALSE,
                            follow_up = NULL, unit = NULL,
                            cer_str = NULL, ier_str = NULL,
                            label_intervention = "intervention",
                            markers = NULL) {
  if (.is_not_reported(g)) {
    return(.bmj_row_values_not_reported(nm, g, follow_up))
  }


  meta_obj <- g$meta

  outcome_cell <- if (!is.null(follow_up) && length(follow_up) == 1L &&
                      !is.na(follow_up) && nzchar(follow_up)) {
    paste0(nm, "\n", follow_up)
  } else {
    nm
  }

  reason    <- .certainty_rate_down_reason(g, markers = markers)
  cert_cell <- if (is.null(reason)) g$certainty else {
    paste0(g$certainty, "\n", reason)
  }

  # Box 1's placeholder is "Treatment"; a caller-supplied arm label replaces
  # it, but the package default ("intervention") is not a sentence subject.
  pl_tx <- if (identical(label_intervention, "intervention")) {
    "Treatment"
  } else {
    label_intervention
  }

  nf <- .bmj_number_format("bmj")

  # Both arm columns of the "Absolute effects" block. A Chinn dichotomisation
  # supplies its own pair; otherwise they come off the object -- the baseline
  # risk for a binary outcome, the pooled control arms for a continuous one.
  arm <- .sof_arm_cells(meta_obj, g$baseline_risk, per,
                        big_mark = nf$big_mark, ci_sep = nf$ci_sep,
                        unit = unit)
  caller_cells <- !is.null(cer_str) || !is.null(ier_str)

  list(
    outcome   = outcome_cell,
    n         = .n_participants_studies_bmj(meta_obj$k, .total_n(meta_obj),
                                            g$study_design),
    effect    = .format_effect_bmj(meta_obj, g$outcome_type,
                                   prediction = prediction),
    cer       = cer_str %||% arm$cer,
    ier       = ier_str %||% arm$ier,
    diff      = .format_difference(meta_obj, g$baseline_risk, per, unit,
                                   g$outcome_type),
    certainty = cert_cell,
    plain     = .plain_language_for(g, intervention_label = pl_tx),
    arm_note  = if (caller_cells) NULL else arm$note
  )
}

# BMJ row for an outcome nobody reported. `nm` already carries the [n] marker
# disp() applied, if any. The caller's per-outcome follow_up still wins over
# the one stored on the object, so the two lookups behave alike for rated and
# not-reported outcomes.
.bmj_row_values_not_reported <- function(nm, g, follow_up = NULL) {
  lbl <- .not_reported_label(g)

  fu <- if (!is.null(follow_up) && length(follow_up) == 1L &&
            !is.na(follow_up) && nzchar(follow_up)) {
    as.character(follow_up)
  } else {
    g$follow_up
  }
  outcome_cell <- if (is.null(fu)) nm else paste0(nm, "\n", fu)

  list(
    outcome   = outcome_cell,
    n         = lbl,
    effect    = lbl,
    cer       = lbl,
    ier       = lbl,
    diff      = lbl,
    # No rate-down reason line: nothing was rated down because nothing was
    # rated.
    certainty = NOT_REPORTED_CERTAINTY,
    plain     = .not_reported_plain()
  )
}

.bmj_headers <- function(sm, has_plain,
                         label_intervention = "intervention",
                         label_control      = "control") {
  hdrs <- c(
    "Outcome and follow-up",
    "No of participants\n(No of studies and type)",
    .effect_header(sm),
    paste0("With ", label_control),
    paste0("With ", label_intervention),
    "Difference",
    "Certainty of evidence\n(quality of evidence)"
  )
  if (has_plain) hdrs <- c(hdrs, "Plain language summary")
  hdrs
}

# Shared flextable chrome: spanning "Absolute effects (95% CI)" header,
# fonts, alignment, widths.
.bmj_decorate <- function(ft, has_plain) {
  vals  <- c("", "", "", "Absolute effects (95% CI)", "", "")
  cw    <- c(1, 1, 1, 3, 1, 1)
  if (!has_plain) { vals <- vals[-6]; cw <- cw[-6] }
  ft <- flextable::add_header_row(ft, top = TRUE, values = vals, colwidths = cw)

  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::font(ft, fontname = .PMA_TABLE_FONT, part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 2, align = "center", part = "body")
  ft <- flextable::valign(ft, valign = "top", part = "body")

  ft <- flextable::bg(ft,    bg = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft,  part = "header")

  widths <- c(1.5, 1.2, 1.3, 0.9, 0.9, 1.5, 1.3, if (has_plain) 1.7)
  for (j in seq_along(widths)) {
    ft <- flextable::width(ft, j = j, width = widths[j])
  }
  ft
}

# Footer shared by the single- and multi-outcome BMJ tables.
# `rate_arms` says whether any row's arm columns hold event rates derived from
# a baseline risk. A table whose rows are all continuous outcomes drops that
# sentence, since nothing in it was computed that way.
.bmj_base_note <- function(label_intervention = "intervention",
                           prediction = FALSE, rate_arms = TRUE) {
  paste0(
    .PMA_CORE_GRADE_FOOTNOTE, " ",
    "CI = confidence interval.",
    if (prediction) " PrI = 95 percent prediction interval." else "",
    if (rate_arms) paste0(
      " Absolute effects: the ", label_intervention, "-arm rate and the ",
      "difference are computed from the control-arm (baseline) risk and the ",
      "pooled relative effect.") else ""
  )
}

.bmj_plain_language_note <- function() {
  paste0(
    "Plain language summaries follow Core GRADE 6 box 1. ",
    PLAIN_LANGUAGE_TABLE_NOTE
  )
}

# --- Single-outcome BMJ table ----------------------------------------------

.sof_table_bmj <- function(x, pal, per, prediction,
                           cer_str, ier_str, baseline_for_display,
                           follow_up = NULL, unit = NULL,
                           chinn_active = FALSE, chinn_invert = FALSE,
                           threshold_label = NULL,
                           label_intervention = "intervention",
                           label_control      = "control") {
  meta_obj <- x$meta

  # Numbered footnotes for the domains that pulled the rating down; the
  # markers ride on the domain names inside the "Due to ..." sentence. The
  # register starts at [1] -- the analysis-set and publication-bias sentences
  # below stay unnumbered, as they always have been.
  fact_domains <- .rated_down_fact_domains(x)
  fact_notes   <- character(0)
  fact_markers <- integer(0)
  for (dm in fact_domains) {
    note <- .domain_fact_note(x, dm)
    if (is.null(note)) next
    fact_notes <- c(fact_notes, note)
    fact_markers[[dm]] <- length(fact_notes)
  }

  vals <- .bmj_row_values(
    x$outcome_name, x, per = per, prediction = prediction,
    follow_up = follow_up, unit = unit,
    cer_str = cer_str, ier_str = ier_str,
    label_intervention = label_intervention,
    markers = fact_markers
  )
  # Chinn dichotomisation replaces the arm rates with derived ones; the risk
  # difference implied by them is not the pooled continuous difference, so the
  # Difference column keeps the continuous estimate.
  if (chinn_active) {
    vals$diff <- .format_difference(meta_obj, baseline_for_display, per, unit,
                                    x$outcome_type)
  }

  has_plain <- !is.null(vals$plain)
  hdrs <- .bmj_headers(meta_obj$sm, has_plain,
                       label_intervention, label_control)

  cells <- c(vals$outcome, vals$n, vals$effect, vals$cer, vals$ier,
             vals$diff, vals$certainty, if (has_plain) vals$plain)
  df <- as.data.frame(matrix(cells, nrow = 1L), stringsAsFactors = FALSE)
  names(df) <- hdrs

  ft <- flextable::flextable(df)
  ft <- .bmj_decorate(ft, has_plain)

  cell_colors <- pal[[x$certainty]]
  ft <- flextable::bg(ft,    j = 7, bg    = cell_colors$bg,   part = "body")
  ft <- flextable::color(ft, j = 7, color = cell_colors$text, part = "body")
  ft <- flextable::bold(ft,  j = 7, part = "body")
  ft <- flextable::align(ft, j = 7, align = "center", part = "body")

  ft <- flextable::add_footer_lines(
    ft, values = .bmj_base_note(label_intervention, prediction,
                                rate_arms = is.null(vals$arm_note)))

  # How the two arm columns were derived when they hold arm-level means rather
  # than event rates (see .cont_arm_note()).
  if (!is.null(vals$arm_note)) {
    ft <- flextable::add_footer_lines(ft, values = vals$arm_note)
  }

  for (i in seq_along(fact_notes)) {
    ft <- flextable::add_footer_lines(
      ft, values = sprintf("[%d] %s", i, fact_notes[i]))
  }

  # Risk-of-bias analysis set (Core GRADE 4 Fig 2). A refit silently changes
  # every number in this table, so it must always be stated — in this style
  # too.
  rob_set_note <- .rob_analysis_set_note(x)
  if (!is.null(rob_set_note)) {
    ft <- flextable::add_footer_lines(ft, values = rob_set_note)
  }

  pubias_qual_note <- .pubias_qualitative_note(x)
  if (!is.null(pubias_qual_note)) {
    ft <- flextable::add_footer_lines(
      ft, values = paste0("Publication bias: ", pubias_qual_note))
  }

  if (chinn_active) {
    ft <- flextable::add_footer_lines(
      ft, values = .chinn_note(invert = isTRUE(chinn_invert),
                               threshold_label = threshold_label))
  }

  if (has_plain) {
    ft <- flextable::add_footer_lines(ft, values = .bmj_plain_language_note())
  }

  ft <- .style_table_footer(ft)
  ft
}

# --- Multi-outcome BMJ table -----------------------------------------------

# Per-outcome argument lookup: a named vector/list keyed by outcome name, or a
# single unnamed value applied to every outcome.
.per_outcome_arg <- function(v, nm) {
  if (is.null(v)) return(NULL)
  if (!is.null(names(v))) {
    if (nm %in% names(v)) return(v[[nm]])
    return(NULL)
  }
  if (length(v) == 1L) return(v[[1L]])
  NULL
}

.grade_table_bmj <- function(outcomes, nms, prim_nms, sec_nms, primary,
                             pal, per, prediction, follow_up, unit,
                             label_intervention, label_control,
                             disp, row_notes,
                             fact_notes = character(0),
                             fact_markers = list(),
                             responder = list(),
                             converted_nms = character(0)) {
  row_vals <- lapply(nms, function(nm) {
    # A converted row hands its own arm cells down, exactly as sof_table() does
    # for a single-outcome Chinn table; every other row lets .bmj_row_values()
    # derive them.
    arm <- responder[[nm]]$arm
    .bmj_row_values(disp(nm), outcomes[[nm]], per = per,
                    prediction = prediction,
                    follow_up = .per_outcome_arg(follow_up, nm),
                    unit      = .per_outcome_arg(unit, nm),
                    cer_str   = arm$cer, ier_str = arm$ier,
                    label_intervention = label_intervention,
                    markers   = fact_markers[[nm]])
  })
  names(row_vals) <- nms

  # Rated outcomes only, for the same reason as the effect-measure header
  # below. A not-reported row always carries a sentence, but that sentence is
  # not a Core GRADE 6 Box 1 statement; letting it decide the column would add
  # the whole "Plain language summary" column - and the Box 1 footer that
  # .bmj_plain_language_note() attributes it to - to a table whose rated
  # outcomes have no Box 1 statement at all. The column's presence is therefore
  # decided exactly as it was before not-reported rows existed; when it does
  # exist, a not-reported row still fills it (add_outcome() below).
  rated_nms <- names(.rated_outcomes(outcomes[nms]))
  has_plain <- any(vapply(row_vals[rated_nms], function(v) !is.null(v$plain),
                          logical(1)))

  # Rated outcomes only: a not-reported outcome has no effect measure, so it
  # must not turn a single-measure header into the generic one.
  sms <- unique(vapply(.rated_outcomes(outcomes),
                       function(g) as.character(g$meta$sm %||% ""),
                       character(1)))
  hdrs  <- .bmj_headers(if (length(sms) == 1L) sms else NULL, has_plain,
                        label_intervention, label_control)
  ncols <- length(hdrs)

  all_rows   <- list()
  label_rows <- integer(0)
  cert_rows  <- list()   # row index (char) -> outcome name
  row_idx    <- 0L

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
    v <- row_vals[[nm]]
    cells <- c(v$outcome, v$n, v$effect, v$cer, v$ier, v$diff, v$certainty,
               if (has_plain) v$plain %||% "")
    r <- as.data.frame(matrix(cells, nrow = 1L), stringsAsFactors = FALSE)
    names(r) <- hdrs
    all_rows[[length(all_rows) + 1L]] <<- r
    cert_rows[[as.character(row_idx)]] <<- nm
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
  ft <- flextable::flextable(df)
  ft <- .bmj_decorate(ft, has_plain)

  for (lr in label_rows) {
    ft <- flextable::merge_h(ft,  i = lr, part = "body")
    ft <- flextable::bg(ft,       i = lr, bg = "#EBEBEB", part = "body")
    ft <- flextable::bold(ft,     i = lr,                 part = "body")
    ft <- flextable::italic(ft,   i = lr,                 part = "body")
    ft <- flextable::align(ft,    i = lr, align = "left", part = "body")
  }

  for (ri in names(cert_rows)) {
    i <- as.integer(ri)
    g <- outcomes[[cert_rows[[ri]]]]
    if (.is_not_reported(g)) {
      # Neutral grey italics: the palette encodes a rating this row does not
      # have.
      ft <- flextable::bg(ft,    i = i, j = 7, bg = "#F5F5F5", part = "body")
      ft <- flextable::color(ft, i = i, j = 7, color = "#666666",
                             part = "body")
      ft <- flextable::italic(ft, i = i, j = 7, part = "body")
      ft <- flextable::align(ft, i = i, j = 7, align = "center", part = "body")
      next
    }
    p <- pal[[g$certainty]]
    ft <- flextable::bg(ft,    i = i, j = 7, bg    = p$bg,   part = "body")
    ft <- flextable::color(ft, i = i, j = 7, color = p$text, part = "body")
    ft <- flextable::bold(ft,  i = i, j = 7,                 part = "body")
    ft <- flextable::align(ft, i = i, j = 7, align = "center", part = "body")
  }

  # Continuous outcomes derive their arm columns differently; the note is
  # written once however many rows it covers, and the rate sentence is kept
  # only while some row still reports rates. Counted over the rated rows only:
  # a not-reported row reports neither rates nor arm-level means, so letting it
  # stand in for a rate-reporting row would keep a sentence describing columns
  # that no row fills.
  rated_vals <- row_vals[names(.rated_outcomes(outcomes[nms]))]
  n_cont     <- sum(vapply(rated_vals, function(v) !is.null(v$arm_note),
                           logical(1)))
  arm_notes  <- unique(unlist(lapply(rated_vals, function(v) v$arm_note),
                              use.names = FALSE))
  ft <- flextable::add_footer_lines(
    ft, values = .bmj_base_note(label_intervention, prediction,
                                rate_arms = n_cont < length(rated_vals)))
  for (nt in arm_notes) {
    ft <- flextable::add_footer_lines(ft, values = nt)
  }

  # What "Not reported" means, stated once for the table however many such rows
  # it has.
  if (.has_not_reported(outcomes)) {
    ft <- flextable::add_footer_lines(ft, values = .not_reported_table_note())
  }

  # Per-row notes (risk-of-bias analysis set, not-reported reason), keyed to
  # the [n] markers on the outcome cells (the analysis set can differ from
  # outcome to outcome).
  for (i in seq_along(row_notes)) {
    ft <- flextable::add_footer_lines(
      ft, values = sprintf("[%d] %s", i, row_notes[i]))
  }

  # Domain-fact footnotes continue the same [n] register, already numbered by
  # the caller so both table styles share one counter.
  for (line in fact_notes) {
    ft <- flextable::add_footer_lines(ft, values = line)
  }

  for (nm in nms) {
    if (.is_not_reported(outcomes[[nm]])) next
    pubias_qual_note <- .pubias_qualitative_note(outcomes[[nm]])
    if (!is.null(pubias_qual_note)) {
      ft <- flextable::add_footer_lines(
        ft, values = paste0("[", nm, "] Publication bias: ", pubias_qual_note))
    }
  }

  ft <- .add_responder_notes(ft, responder, converted_nms)

  if (has_plain) {
    ft <- flextable::add_footer_lines(ft, values = .bmj_plain_language_note())
  }

  ft <- .style_table_footer(ft)
  ft
}
