# step3_threshold.R - pure helpers behind the Step 3 Configuration tab
#
# Split out of step3_grade.R, which had grown past four thousand lines. Every
# function here is pure: it reads its arguments (and file-scope constants) and
# nothing else - no `input`, no `output`, no `session`, no `state`. That is what
# makes them testable without Shiny, and it is the property to preserve when
# adding to this file. Sourced BEFORE R/step3_grade.R (see local_files in
# app.R), though R only needs the definitions to exist by call time.

# Map a suggest_threshold() return onto the two threshold reactiveVals used by
# the Configuration tab.
#
# pmatools >= 0.5 leads with the ABSOLUTE candidate for binary ratio measures
# (OR/RR/HR): the top level is threshold_user 0.05 / threshold_scale "ard", and
# the ratio-scale value (e.g. 1.25 for OR) sits in $threshold_ratio. Other
# measures return a flat list whose $threshold_scale is "ratio" (RoM), "ard"
# (ARD) or "te_scale" (SMD, MD); unsupported measures return NULL.
#
# Returns list(relative =, absolute1000 =): `relative` feeds threshold_state()
# (the ratio / te-scale input) and `absolute1000` feeds threshold_abs_state()
# (events per 1,000). Either element is NA when the object offers no candidate
# on that scale.
step3_threshold_suggestions <- function(s) {
  out <- list(relative = NA_real_, absolute1000 = NA_real_)
  if (!is.list(s)) return(out)

  .candidate <- function(cand) {
    if (!is.list(cand)) return(NULL)
    v <- cand$threshold_user
    if (is.null(v) || length(v) != 1L || !is.numeric(v) ||
        !is.finite(v) || v <= 0) {
      return(NULL)
    }
    list(value = v, scale = cand$threshold_scale %||% "")
  }

  cands <- list(.candidate(s),
                .candidate(s$threshold_absolute),
                .candidate(s$threshold_ratio))
  for (cand in cands) {
    if (is.null(cand)) next
    if (identical(cand$scale, "ard")) {
      if (is.na(out$absolute1000)) out$absolute1000 <- 1000 * cand$value
    } else if (is.na(out$relative)) {
      out$relative <- cand$value
    }
  }
  out
}

# Does this analysis have an absolute (event-rate) scale at all?
#
# BUG FIX: the Configuration panel used to branch on `sm %in% c("OR", "RR")`,
# so a binary outcome analysed as ARD, RD or HR fell through to the
# continuous branch and lost the absolute-scale interface entirely - even
# though its control-arm event data is exactly what that interface needs.
# The question is about the outcome, not about the summary measure, so the
# class of the meta object decides: metabin always carries event.c / n.c,
# metacont never does. `outcome_type` (the Step 2 radio, mirrored into
# input$outcome_type) is only the fallback for the pre-analysis state.
step3_is_binary_outcome <- function(obj, outcome_type = NULL) {
  if (!is.null(obj)) {
    if (inherits(obj, "metabin")) return(TRUE)
    if (inherits(obj, "metacont")) return(FALSE)
    # Rare-event engines return objects that are not metabin but still carry
    # arm-level counts; those are binary too.
    if (!is.null(obj$event.c) && !is.null(obj$n.c) &&
        length(obj$event.c) > 0 && length(obj$n.c) > 0) {
      return(TRUE)
    }
  }
  identical(outcome_type, "binary")
}

# Pooled control-group risk, with the fallback made visible.
#
# The number the reviewer sees is a meta::metaprop random-effects pooled
# proportion (GLMM, logit link, back-transformed) via the vendored
# .compute_control_risk(). That function falls back to the crude
# sum(event.c) / sum(n.c) with a warning when the GLMM fails; the warning is
# captured here rather than swallowed, so the UI can say which of the two it
# is actually showing. A crude ratio must never be presented as pooled.
#
# Up to pmatools 0.5.0 the input had to be sanitised to complete
# (event.c, n.c) pairs before calling in, because .compute_control_risk()
# dropped NA events but not the matching denominators and metaprop() then
# errored on every such dataset. pmatools 0.5.1 filters both vectors on one
# complete-case predicate, so the meta object is now passed through untouched.
# `keep` survives only to report how many studies the estimate rests on.
#
# Known limitation, stated not fixed: .compute_control_risk() returns a bare
# scalar and discards metaprop's confidence interval, so the uncertainty in
# the pooled control risk cannot be shown alongside it.
step3_control_risk <- function(meta_obj) {
  out <- list(value = NA_real_, method = "none", crude = NA_real_,
              k_used = 0L, k_dropped = 0L)
  if (is.null(meta_obj)) return(out)
  ec <- meta_obj$event.c
  nc <- meta_obj$n.c
  if (is.null(ec) || is.null(nc) || length(ec) != length(nc) ||
      length(nc) == 0) {
    return(out)
  }
  keep <- !is.na(ec) & !is.na(nc) & nc > 0
  if (!any(keep)) return(out)
  out$k_used    <- sum(keep)
  out$k_dropped <- sum(!keep)
  crude <- .compute_control_risk(meta_obj, method = "simple")
  out$crude <- if (is.null(crude)) NA_real_ else crude

  fell_back <- FALSE
  val <- withCallingHandlers(
    tryCatch(.compute_control_risk(meta_obj, method = "metaprop"),
             error = function(e) NULL),
    warning = function(w) {
      if (grepl("metaprop", conditionMessage(w), fixed = TRUE)) {
        fell_back <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )
  if (is.null(val) || length(val) != 1L || !is.finite(val) ||
      val <= 0 || val >= 1) {
    return(out)
  }
  out$value  <- val
  out$method <- if (fell_back) "simple_fallback" else "metaprop"
  out
}

# Event rate <-> ratio at a fixed control-group risk. Kept at file scope
# because the equivalence table, the directed conversion and the notes all
# need the same two maps; OR works on the odds, everything else on the risk.
step3_ratio_from_p1 <- function(sm, p0, p1) {
  if (identical(sm, "OR")) (p1 / (1 - p1)) / (p0 / (1 - p0)) else p1 / p0
}
step3_p1_from_ratio <- function(sm, p0, ratio) {
  if (identical(sm, "OR")) {
    odds <- (p0 / (1 - p0)) * ratio
    odds / (1 + odds)
  } else {
    p0 * ratio
  }
}

# Both directions of the absolute threshold, plus the mirror of the increase
# side.
#
# The rating algorithm works on the log scale with a symmetric
# +/- threshold_internal, so whichever ratio it is given, the opposite side
# it applies is that ratio inverted. Inverting the increase-side ratio T does
# NOT give the ratio implied by p0 - ard: e.g. RR with p0 = 0.18 and
# ard = 0.05 gives T = 0.23 / 0.18 = 1.278 on the increase side, whose mirror
# 1 / 1.278 = 0.782 implies p1 = 0.141, an absolute difference of -0.039
# rather than -0.050. The gap is larger for OR. `mirror_*` below is that
# mirror of the increase side; step3_directed_threshold() decides which of
# the two sides the app makes exact.
#
# Returns NULL unless p0 and the threshold are usable. `down_ok` is FALSE
# when p0 - ard would leave the (0, 1) interval, in which case only the
# increase side and the mirror are meaningful.
step3_ard_equivalence <- function(sm, abs1000, base1000) {
  if (is.null(abs1000) || is.null(base1000)) return(NULL)
  if (length(abs1000) != 1L || length(base1000) != 1L) return(NULL)
  if (is.na(abs1000) || is.na(base1000)) return(NULL)
  if (!is.finite(abs1000) || !is.finite(base1000)) return(NULL)
  p0  <- base1000 / 1000
  ard <- abs1000 / 1000
  if (p0 <= 0 || p0 >= 1 || ard <= 0) return(NULL)
  p1_up <- p0 + ard
  if (p1_up >= 1) return(NULL)
  p1_dn <- p0 - ard

  .ratio  <- function(p1)    step3_ratio_from_p1(sm, p0, p1)
  # Invert a ratio back to an event rate at the same p0 (the side the
  # algorithm mirrors).
  .invert <- function(ratio) step3_p1_from_ratio(sm, p0, ratio)

  ratio_up     <- .ratio(p1_up)
  mirror_ratio <- 1 / ratio_up
  mirror_p1    <- .invert(mirror_ratio)

  list(
    sm            = sm,
    p0            = p0,
    ard           = ard,
    p1_up         = p1_up,
    ratio_up      = ratio_up,
    down_ok       = p1_dn > 0,
    p1_dn         = p1_dn,
    ratio_dn      = if (p1_dn > 0) .ratio(p1_dn) else NA_real_,
    mirror_ratio  = mirror_ratio,
    mirror_p1     = mirror_p1,
    mirror_ard    = mirror_p1 - p0,
    # RR / OR equivalents of the increase side, shown side by side so the
    # reader can see how much the choice of summary measure matters.
    rr_up         = p1_up / p0,
    or_up         = (p1_up / (1 - p1_up)) / (p0 / (1 - p0))
  )
}

# --------------------------------------------------------------------------
# Directed conversion of the absolute threshold
# --------------------------------------------------------------------------
# grade_meta() takes one scalar threshold and every domain judges against the
# symmetric band +/- threshold_internal, so exactly one of the two sides can
# be exact on the absolute scale. Which one matters is settled by where the
# pooled effect lies: that is the crossing Risk of Bias, Inconsistency and
# Imprecision actually turn on. So the app converts the absolute threshold on
# that side and passes the result as threshold_scale = "ratio", instead of
# handing pmatools the ARD (which always converts on the increase side).
#
# Effects closer to the null than this tolerance on the TE (log) scale have no
# meaningful direction; the increase side is then made exact by convention and
# the UI says so.
STEP3_TE_NULL_TOL <- 1e-6

step3_threshold_direction <- function(te_point) {
  if (is.null(te_point) || length(te_point) != 1L || !is.numeric(te_point) ||
      is.na(te_point) || !is.finite(te_point)) {
    return("unavailable")
  }
  if (abs(te_point) <= STEP3_TE_NULL_TOL) return("indeterminate")
  if (te_point < 0) "decrease" else "increase"
}

# Pooled TE of a meta object, on the TE (log) scale. Mirrors the vendored
# .pooled_te() but is defined here so the app does not depend on an internal.
step3_pooled_te <- function(obj) {
  if (is.null(obj)) return(NA_real_)
  te <- if (isTRUE(obj$random)) obj$TE.random else obj$TE.common
  if (is.null(te) || length(te) == 0L || !all(is.finite(te))) {
    te <- if (isTRUE(obj$random)) obj$TE.common else obj$TE.random
  }
  if (is.null(te) || length(te) == 0L) return(NA_real_)
  as.numeric(te)[1]
}

# The ratio to pass to grade_meta(threshold =, threshold_scale = "ratio").
#
# `eq` is step3_ard_equivalence(); `direction` is step3_threshold_direction().
# Returns NULL when `eq` is unusable. Fields:
#   ratio        value to pass; always > 1, so threshold_internal =
#                log(ratio) > 0 as pmatools requires. On the decrease side
#                that is 1 / T_down, i.e. threshold_internal = |log(T_down)|.
#   exact_side   "increase" or "decrease" - the side that is exact per 1,000
#   exact_ratio  the equivalent effect measure on the exact side (< 1 when
#                the exact side is the decrease side)
#   exact_p1 / exact_ard     event rate and absolute difference it implies
#   approx_ratio / approx_p1 / approx_ard   the mirrored, opposite side
#   caveat       why the requested direction was not honoured, or NA
step3_directed_threshold <- function(eq, direction = "increase") {
  if (is.null(eq)) return(NULL)
  sm <- eq$sm
  p0 <- eq$p0
  caveat <- NA_character_
  want_down <- identical(direction, "decrease")

  # Edge case 1: p0 - ard <= 0. The decrease-side conversion is undefined
  # (no event rate is `ard` per 1,000 below the control-group risk), so the
  # increase side is used and the decrease side stays the mirrored value.
  if (want_down && !isTRUE(eq$down_ok)) {
    caveat <- sprintf(paste0(
      "The pooled effect is below the null, but the threshold (%g per 1,000) ",
      "is not smaller than the control-group risk (%g per 1,000), so no event ",
      "rate lies that far below it and the decrease-side conversion is ",
      "undefined. The increase side is used instead; the decrease side ",
      "remains the mirrored approximation."),
      1000 * eq$ard, 1000 * p0)
    want_down <- FALSE
  }
  # Edge case 2: the pooled effect sits on the null (or is unavailable), so
  # neither side is the one that decides the judgments.
  if (identical(direction, "indeterminate")) {
    caveat <- paste0(
      "The pooled effect is at (or indistinguishable from) the null, so ",
      "neither direction is the one the judgments turn on. The increase side ",
      "is made exact by convention.")
  } else if (identical(direction, "unavailable")) {
    caveat <- paste0(
      "The pooled effect is not available, so the direction could not be ",
      "read from it. The increase side is made exact by convention.")
  }

  exact_ratio <- if (want_down) eq$ratio_dn else eq$ratio_up
  if (is.null(exact_ratio) || !is.finite(exact_ratio) || exact_ratio <= 0) {
    return(NULL)
  }
  ratio_arg    <- if (exact_ratio < 1) 1 / exact_ratio else exact_ratio
  approx_ratio <- 1 / exact_ratio
  approx_p1    <- step3_p1_from_ratio(sm, p0, approx_ratio)
  exact_p1     <- step3_p1_from_ratio(sm, p0, exact_ratio)

  list(
    sm           = sm,
    p0           = p0,
    ard          = eq$ard,
    direction    = direction,
    exact_side   = if (want_down) "decrease" else "increase",
    approx_side  = if (want_down) "increase" else "decrease",
    ratio        = ratio_arg,
    exact_ratio  = exact_ratio,
    exact_p1     = exact_p1,
    exact_ard    = exact_p1 - p0,
    approx_ratio = approx_ratio,
    approx_p1    = approx_p1,
    approx_ard   = approx_p1 - p0,
    caveat       = caveat
  )
}

# One sentence stating, in absolute terms, the threshold that was used and
# which side of it is exact. This replaces the pmatools $threshold_note that
# threshold_scale = "ard" used to produce: with threshold_scale = "ratio" the
# package no longer knows the absolute value, so the app has to say it, or the
# Evidence Profile footnote and the domain notes would lose the provenance.
step3_threshold_note <- function(dir) {
  if (is.null(dir)) return(NULL)
  sm <- dir$sm
  note <- sprintf(paste0(
    "Absolute threshold %g per 1,000 at a baseline risk %g per 1,000, ",
    "converted on the %s side, where it is exact (equivalent %s %.3f: ",
    "%.0f -> %.0f per 1,000, %+.0f per 1,000). Domains judge against the ",
    "symmetric band +/- log(%.3f), so the %s side is the mirrored value ",
    "%s %.3f, implying %+.0f per 1,000 rather than %+.0f"),
    1000 * dir$ard, 1000 * dir$p0, dir$exact_side, sm, dir$exact_ratio,
    1000 * dir$p0, 1000 * dir$exact_p1, 1000 * dir$exact_ard,
    dir$ratio, dir$approx_side, sm, dir$approx_ratio,
    1000 * dir$approx_ard, -1000 * dir$exact_ard)
  if (!is.na(dir$caveat)) note <- paste0(note, ". ", sub("[.]$", "", dir$caveat))
  note
}

# Append a sentence to ONE domain's notes, in the " | " style the vendored
# .append_domain_note() uses across the whole table. Needed because the app
# now writes the threshold note itself (see grade_obj()).
step3_append_domain_note <- function(d, domain, note) {
  if (is.null(d) || is.null(note) || !length(note) || !nzchar(note)) return(d)
  idx <- which(d$domain == domain)
  if (!length(idx)) return(d)
  d$notes[idx] <- ifelse(is.na(d$notes[idx]), note,
                         paste0(d$notes[idx], " | ", note))
  d
}

# --------------------------------------------------------------------------
# Configuration-tab presentation
# --------------------------------------------------------------------------
# Lifted out of step3_server() unchanged. They were closures only by accident
# of where they were written: each reads its arguments, the constants above,
# and EDU_COPY / the PMA_ALERT_* palette from ui_helpers.R. None of them
# touches input, output, session or state, so keeping them inside a 2,900-line
# server body bought nothing.

# Responder-conversion state (continuous outcomes). The control-group
# proportion has no auto value: Core GRADE 6 says only that the rate is
# "chosen from the context", and nothing in pmatools proposes a default.
# 20 percent is therefore an app convention and is labelled as one until
# the reviewer confirms or replaces it.
RESPONDER_P0_DEFAULT <- 0.20

.mic_note <- function() {
  htmltools::p(
    class = "pma-card-subtitle",
    style = "font-style: italic;",
    paste0(
      "Note: do not equate this threshold with a Minimally Important ",
      "Change (MIC). MIC is a within-individual change over time, while ",
      "a meta-analysis pools between-group differences across studies. ",
      "These are distinct quantities and should not be substituted for ",
      "each other when judging clinical importance."
    )
  )
}

# Section heading + note helpers, so the Configuration blocks look alike.
.config_section <- function(title, ...) {
  htmltools::div(
    style = paste(
      "border: 1px solid hsl(var(--border)); border-radius: 8px;",
      "padding: 0.75rem 1rem; margin-bottom: 1rem;"),
    htmltools::h5(title, style = "margin: 0 0 0.5rem; font-size: 1rem;"),
    ...
  )
}
.config_note <- function(...) {
  htmltools::p(class = "pma-card-subtitle", style = "margin-bottom: 0.5rem;",
               ...)
}
.warn_badge <- function(text) {
  htmltools::span(
    class = "pma-badge",
    style = sprintf(paste0("background: %s; color: %s; border: 1px solid %s;",
                           " white-space: nowrap; margin-left: 0.4rem;"),
                    PMA_ALERT_BG, PMA_ALERT_FG, PMA_ALERT_FG),
    text)
}
.ok_badge <- function(text) {
  htmltools::span(
    class = "pma-badge",
    style = paste0("background: hsl(var(--muted)); ",
                   "color: hsl(var(--muted-foreground)); ",
                   "white-space: nowrap; margin-left: 0.4rem;"),
    text)
}

.source_badge <- function(src) {
  if (identical(src, "core_grade_6")) {
    return(.ok_badge("source: Core GRADE 6"))
  }
  .warn_badge("source: pmatools convention, not Core GRADE")
}

# ----- Configuration tab: responder conversion block (continuous) ------
# Core GRADE 6 ranks three presentations of a continuous outcome and
# recommends the mean difference and the responder proportion together.
# This app implements the responder proportion only; the departure is
# stated on screen rather than left implicit, and so is the fact that the
# conversion used is Chinn's formula, not Core GRADE 6's own procedure.
.responder_block <- function(sm) {
  convertible <- sm %in% c("SMD", "MD")
  if (!convertible) {
    return(.config_section(
      "Presentation of this outcome",
      .config_note(EDU_COPY$config_tab$continuous_intro),
      .config_note(EDU_COPY$config_tab$continuous_departure),
      htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        sprintf(paste0(
          "The responder conversion is unavailable for %s: it is defined ",
          "on the standardized mean difference (and on the mean ",
          "difference via the pooled SD) only. The Summary of Findings ",
          "table will report the %s itself."), sm, sm))
    ))
  }
  # The badge is its own output on purpose: if this renderUI depended on
  # the confirmation state, ticking the box (or typing a rationale) would
  # rebuild the panel and reset the very widget being used.
  .config_section(
    htmltools::tagList(
      "Proportion of control patients meeting the threshold",
      shiny::uiOutput("responder_p0_badge", inline = TRUE)),
    .config_note(EDU_COPY$config_tab$continuous_intro),
    .config_note(EDU_COPY$config_tab$continuous_departure),
    .config_note(EDU_COPY$config_tab$chinn_caveat),
    shiny::checkboxInput("convert_smd_to_or",
      paste0("Present this outcome as a proportion of responders ",
             "(recommended; Core GRADE 6 option 2)"),
      value = TRUE),
    shiny::conditionalPanel(
      "input.convert_smd_to_or",
      shiny::numericInput("baseline_risk_chinn",
        paste0("Proportion of control patients meeting the threshold of ",
               "clinical interest"),
        value = RESPONDER_P0_DEFAULT, min = 0.01, max = 0.99, step = 0.01),
      .config_note(EDU_COPY$config_tab$responder_default),
      # This is not a risk and Core GRADE has no notion of baseline risk
      # for a continuous outcome, so it does not reuse the binary label.
      shiny::conditionalPanel(
        sprintf("input.baseline_risk_chinn != %s", RESPONDER_P0_DEFAULT),
        shiny::textAreaInput("responder_p0_rationale",
          "Rationale (required when the default is replaced)",
          rows = 2, width = "100%",
          placeholder = paste0(
            "e.g., 31 percent of control participants met the 50 percent ",
            "reduction criterion in the three trials that reported it; ",
            "taken from the placebo arm of Jones 2019."))
      ),
      shiny::conditionalPanel(
        sprintf("input.baseline_risk_chinn == %s", RESPONDER_P0_DEFAULT),
        shiny::checkboxInput("responder_p0_confirm",
          paste0("I have considered this rate and accept 20 percent ",
                 "(200 per 1,000) for this outcome"),
          value = FALSE)
      ),
      shiny::textInput("threshold_label",
        "Definition of the threshold of clinical interest (free text)",
        placeholder = "e.g., >=50 percent reduction in PHQ-9 from baseline"),
      shiny::uiOutput("chinn_direction_echo")
    )
  )
}

# Live equivalent-effect display for the absolute mode. Shows BOTH
# directions and names the one that is exact.
#
# The algorithm is symmetric on the log scale: whichever ratio it is given,
# the opposite side it applies is that ratio inverted. So the app converts
# the absolute threshold on the side the pooled effect lies (that is the
# crossing the judgments turn on) and hands grade_meta() the ratio rather
# than the ARD. The residual asymmetry cannot be removed - it is moved to
# the other side, and named here. Same wording as threshold_summary_text()
# below; these two are the app's copies.
.equiv_lines <- function(eq, dir = NULL) {
  sm <- eq$sm
  up <- sprintf("Increase: %.0f -> %.0f per 1,000, equivalent %s %.3f",
                1000 * eq$p0, 1000 * eq$p1_up, sm, eq$ratio_up)
  dn <- if (isTRUE(eq$down_ok)) {
    sprintf("Decrease: %.0f -> %.0f per 1,000, equivalent %s %.3f",
            1000 * eq$p0, 1000 * eq$p1_dn, sm, eq$ratio_dn)
  } else {
    "Decrease: not shown - the threshold exceeds the control-group risk."
  }
  if (is.null(dir)) {
    return(list(up = up, dn = dn, alg = character(), approx = character()))
  }
  alg <- sprintf(
    paste0("What the algorithm uses: a symmetric +/- log(%.3f) band, ",
           "converted on the %s side because the pooled effect lies %s ",
           "the null. That side is exact - %s %.3f is %+.0f per 1,000 at ",
           "this control-group risk."),
    dir$ratio, dir$exact_side,
    if (identical(dir$exact_side, "decrease")) "below" else "above",
    sm, dir$exact_ratio, 1000 * dir$exact_ard)
  approx <- sprintf(
    paste0("The %s side is therefore the approximate one: the band's ",
           "mirror is %s %.3f, which implies %+.0f per 1,000 rather than ",
           "%+.0f. Imprecision's two-level rule asks whether the ",
           "confidence interval crosses both thresholds, so that one ",
           "crossing is judged against the mirrored value."),
    dir$approx_side, sm, dir$approx_ratio,
    1000 * dir$approx_ard, -1000 * dir$exact_ard)
  if (!is.na(dir$caveat)) approx <- paste(approx, dir$caveat)
  list(up = up, dn = dn, alg = alg, approx = approx)
}
