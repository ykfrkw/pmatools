# step3_threshold.R - pure helpers behind the Step 3 Configuration tab
#
# Split out of step3_grade.R, which had grown past four thousand lines. Every
# function here is pure: it reads its arguments (and file-scope constants) and
# nothing else - no `input`, no `output`, no `session`, no `state`. That is what
# makes them testable without Shiny, and it is the property to preserve when
# adding to this file. Sourced BEFORE R/step3_grade.R (see local_files in
# app.R), though R only needs the definitions to exist by call time.

# ----- Why is there no analysis? ------------------------------------------
#
# When Step 2 cannot run, `ma()` sets state$ma to NULL and records WHAT WAS
# MISSING in state$ma_blocked, a character vector of human labels. Step 3 then
# has to explain itself instead of printing "Run analysis and configure
# domains." at a reviewer who did run one and then emptied a required field.
#
# Three Step 3 outputs say it (final_certainty, sof_preview,
# cert_incomplete_banner) and a fourth reads the identity subset
# (outcome_name_echo), so the sentence is built here, once, rather than four
# times. Pure: labels in, string out, no session.

# The Step 2 outcome-identity fields, spelled exactly as ma() records them in
# state$ma_blocked. Kept beside the message builder because these two are the
# only things that must agree on the wording; step2_ma.R reads this constant
# rather than repeating the strings.
STEP2_IDENTITY_FIELD_LABELS <- c(
  outcome_name = "Outcome name",
  small_values = "Direction (smaller = favorable?)"
)

# Readable names for the column-mapping selectors, so a blocked message can
# name a missing column the way the Step 2 label does rather than by its
# canonical data-frame name.
STEP2_COLUMN_FIELD_LABELS <- c(
  studlab = "Study label column",
  treat   = "Arm / treatment column",
  n       = "Sample size column",
  event   = "Events column",
  mean    = "Mean column",
  sd      = "SD column"
)

# Canonical column name -> the label a blocked message prints. An unmapped
# name is passed through rather than dropped, so a new required column shows
# up as itself instead of vanishing from the sentence.
step2_column_labels <- function(cols) {
  if (is.null(cols) || !length(cols)) return(character(0))
  cols <- as.character(cols)
  lab <- unname(STEP2_COLUMN_FIELD_LABELS[cols])
  ifelse(is.na(lab), cols, lab)
}

step3_blocked_fields <- function(blocked) {
  if (is.null(blocked)) return(character(0))
  blocked <- as.character(blocked)
  blocked <- blocked[!is.na(blocked) & nzchar(blocked)]
  unique(blocked)
}

# The subset of state$ma_blocked that names an outcome-identity field, i.e.
# something the reviewer answers in Step 2's first card rather than in the
# column mapping. output$outcome_name_echo uses it to stop printing a stale
# outcome name as though it were still in force.
step3_blocked_identity <- function(blocked) {
  intersect(step3_blocked_fields(blocked), unname(STEP2_IDENTITY_FIELD_LABELS))
}

# The one sentence. NULL when nothing is recorded, which callers read as
# "no analysis has been attempted" and answer with their idle placeholder.
step3_blocked_message <- function(blocked) {
  fields <- step3_blocked_fields(blocked)
  if (!length(fields)) return(NULL)
  paste0("No analysis. Step 2 is missing: ", paste(fields, collapse = ", "),
         ". Go back to Step 2 and complete it.")
}

# --------------------------------------------------------------------------
# The per-N display unit
# --------------------------------------------------------------------------
# ONE setting for the whole app, owned by the Configuration tab
# (input$per). It changes how event rates are DISPLAYED and what
# sof_table(per =) / export_bundle(per =) are given. It changes nothing the
# rating is computed from.
#
# Internal storage stays per-1,000 throughout: threshold_abs_state(),
# threshold_baseline_state(), .threshold_grade_args() and ois_p0_value() all
# keep their /1000 arithmetic. Only the number on screen, its label and the
# two export arguments move. That is what keeps a reviewer's switch from 1,000
# to 100 from silently re-scaling a threshold they already justified in
# writing.
STEP3_PER_UNITS <- c(100L, 1000L)
STEP3_PER_DEFAULT <- 1000L

# A radioButtons() value arrives as a character. sof_table() wants a number,
# and validation is cheap enough to do at every boundary.
step3_per_unit <- function(x) {
  v <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (length(v) != 1L || is.na(v) || !v %in% STEP3_PER_UNITS) {
    return(STEP3_PER_DEFAULT)
  }
  v
}

# per-1,000 -> the chosen unit, and back. Both are plain rescalings; they
# exist so no call site writes `* per / 1000` by hand and gets the direction
# wrong.
step3_to_per <- function(v_per1000, per) {
  per <- step3_per_unit(per)
  if (is.null(v_per1000) || length(v_per1000) != 1L ||
      !is.numeric(v_per1000) || !is.finite(v_per1000)) {
    return(NA_real_)
  }
  v_per1000 * per / 1000
}
step3_from_per <- function(v_per, per) {
  per <- step3_per_unit(per)
  if (is.null(v_per) || length(v_per) != 1L || !is.numeric(v_per) ||
      !is.finite(v_per)) {
    return(NA_real_)
  }
  v_per * 1000 / per
}

# Round a per-1,000 value so that it is a WHOLE NUMBER OF EVENTS in the
# chosen unit. "15.6 events per 100 patients" is not a thing a reviewer can
# read off a trial, and the box used to offer one decimal at per = 1,000 and
# would have offered two at per = 100.
#
# Note the cost, which is real: at per = 100 the grid is ten times coarser, so
# a control-group risk of 156 per 1,000 becomes 160 per 1,000 (16 per 100).
# shiny/SPEC.md states this rather than leaving a reviewer to discover it.
step3_quantise_per1000 <- function(v_per1000, per = STEP3_PER_DEFAULT) {
  per <- step3_per_unit(per)
  if (is.null(v_per1000) || length(v_per1000) != 1L ||
      !is.numeric(v_per1000) || !is.finite(v_per1000)) {
    return(NA_real_)
  }
  round(v_per1000 * per / 1000) * 1000 / per
}

# THE formatter. Every "per 1,000" string in Step 3 goes through it, so a
# reviewer who switches the unit does not find one box relabelled and three
# notes still claiming per 1,000.
step3_per_label <- function(v_per1000, per = STEP3_PER_DEFAULT,
                            digits = 0L) {
  per <- step3_per_unit(per)
  unit <- format(per, big.mark = ",", scientific = FALSE, trim = TRUE)
  v <- step3_to_per(v_per1000, per)
  if (is.na(v)) return(sprintf("not set (per %s)", unit))
  sprintf("%s per %s",
          formatC(v, format = "f", digits = digits, big.mark = ","),
          unit)
}

# Just the unit, for a widget label or the tail of a sentence.
step3_per_unit_label <- function(per = STEP3_PER_DEFAULT) {
  sprintf("per %s", format(step3_per_unit(per), big.mark = ",",
                           scientific = FALSE, trim = TRUE))
}

# --------------------------------------------------------------------------
# Indirectness: the worst-case fold, and when a rationale is owed
# --------------------------------------------------------------------------
# The overall Indirectness radio ships PRESELECTED to STEP3_INDIR_DEFAULT_LEVEL
# (step3_grade.R). Before that it shipped blank, and blank was the way a
# reviewer said "accept the fold of the four PICO answers" - so the rationale
# gate could key on "is anything selected at all?" and be right. Preselecting
# turns that blank into a real answer, and a gate that still asked "is anything
# selected?" would demand a written reason for a default nobody chose.
#
# So the gate has to compare the overall rating against the fold itself, which
# is what these two do. Pure, and therefore testable without a session: the
# server reactives are only the wiring that maps the four radios onto levels.
STEP3_INDIR_DEFAULT_LEVEL <- "not_serious"

# Severity order of the GRADE levels a fold can produce. Written out rather
# than derived from .grade_level_downgrade(), because this file is sourced
# before the vendored package and must not depend on it.
#
# The previous ordering vector spelled the levels "no" / "some_concerns" /
# "serious", which is the vocabulary 0.5.1 replaced. Every level the PICO
# answers actually produce ("not_serious" / "serious" / "very_serious") missed
# it, so a fold over four answers containing a "No" (very serious) returned
# NULL and the domain was reported as folding to "not serious".
STEP3_INDIR_LEVEL_SEVERITY <- c(
  not_serious       = 0L,
  serious           = 1L,
  very_serious      = 2L,
  extremely_serious = 3L
)

# The most severe of the levels the answered subdomains map to, or NULL when
# nothing recognisable was answered. Mirrors .indirectness_worst_case() in
# R/domain_indirectness.R, so the app can tell a restatement of the automatic
# judgment from a real override without a second grade_meta() call.
step3_indir_worst_case <- function(levels) {
  if (is.null(levels) || !length(levels)) return(NULL)
  levels <- as.character(levels)
  known  <- names(STEP3_INDIR_LEVEL_SEVERITY)
  levels <- levels[!is.na(levels) & levels %in% known]
  if (!length(levels)) return(NULL)
  unname(levels[which.max(STEP3_INDIR_LEVEL_SEVERITY[levels])])
}

# TRUE when the overall rating on screen departs from the fold and therefore
# owes a written reason. An unanswered overall owes nothing, and neither does
# one that restates the fold - which is exactly the case the preselected
# default lands in while all four PICO answers are "Yes".
step3_indir_rationale_required <- function(overall, worst = NULL) {
  overall <- if (is.null(overall) || length(overall) != 1L || is.na(overall)) {
    ""
  } else {
    as.character(overall)
  }
  if (!nzchar(overall)) return(FALSE)
  auto <- if (is.null(worst) || length(worst) != 1L || is.na(worst) ||
              !nzchar(as.character(worst))) {
    STEP3_INDIR_DEFAULT_LEVEL
  } else {
    as.character(worst)
  }
  !identical(overall, auto)
}

# --------------------------------------------------------------------------
# Publication bias: which Fig 5 node is being asked
# --------------------------------------------------------------------------
# DERIVED from the answers, never stored as a free-running cursor. Changing an
# earlier answer therefore re-derives everything downstream instead of leaving
# the wizard parked on a node the algorithm no longer reaches.
#
# The chain below is assess_pubias()'s own evaluation order
# (R/domain_pubias.R): Q1 first and terminal on "yes"; then the pmatools
# registry-coverage input, terminal only on "yes" (which short-circuits the
# package); then the k gate, which is computed and never asked; then Q3 or Q4.
#
# `reopen` is a breadcrumb click. It wins over the derivation, and only for a
# node that is actually reachable - so re-opening Q1 and answering "yes" does
# not strand the reviewer on a Q3 that no longer exists.
STEP3_PUBIAS_NODES <- c("q1", "extra", "q3", "q4", "result")

# A radio is "answered" when it carries a non-empty value, so every node's
# widget starts with NOTHING selected and no answer can be confused with an
# unreached node.
#
# The visual-override select still needs an explicit "I looked and I accept the
# test" VALUE, because on that node the honest answer "leave it to Egger"
# would otherwise be a blank. The registry question no longer needs one: it
# used to offer a third "leave it to the Figure 5 nodes" option alongside a
# "no" that forced rate-down 1 on its own, and 0.5.1 made "no" mean exactly
# what deferring meant - carry on down the chart.
STEP3_PUBIAS_USE_EGGER <- "egger"

.pubias_answered <- function(v) {
  !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v))
}
.pubias_chr <- function(v) {
  if (!.pubias_answered(v)) "" else as.character(v)[1]
}

# Egger's verdict as the answer the Q3 node would have carried, so the chart
# can be lit from it. NA is what an infeasible or failed test reports, and it
# has to read as "no answer" rather than as FALSE - a test that could not run
# is not a symmetric funnel.
.pubias_egger_answer <- function(egger_asymmetric) {
  if (is.null(egger_asymmetric) || length(egger_asymmetric) != 1L ||
      !is.logical(egger_asymmetric) || is.na(egger_asymmetric)) {
    return("")
  }
  if (isTRUE(egger_asymmetric)) "yes" else "no"
}

step3_pubias_node <- function(small_industry = NULL,
                              registry_complete = NULL,
                              funnel_asymmetry = NULL,
                              unpublished = NULL,
                              k = 0L,
                              reopen = NULL) {
  path <- step3_pubias_reachable(small_industry, registry_complete, k)
  if (!is.null(reopen) && length(reopen) == 1L && !is.na(reopen) &&
      as.character(reopen) %in% path) {
    return(as.character(reopen))
  }

  if (!.pubias_answered(small_industry)) return("q1")
  # Fig 5 node 1 is terminal on "yes": nothing after it can undo the concern.
  if (identical(.pubias_chr(small_industry), "yes")) return("result")

  if (!.pubias_answered(registry_complete)) return("extra")
  # Terminal on "yes" only, which is what short-circuits assess_pubias(). "no"
  # decides nothing by itself and falls through to the Figure 5 nodes.
  if (identical(.pubias_chr(registry_complete), "yes")) return("result")

  # Q2 is not a question - k decides it. See step3_pubias_k_line().
  if (isTRUE(step3_pubias_statistical(k))) {
    if (!.pubias_answered(funnel_asymmetry)) return("q3")
  } else {
    if (!.pubias_answered(unpublished)) return("q4")
  }
  "result"
}

# Q2, computed. k >= 10 routes to the statistical branch, below it to the
# registry question. Same rule as assess_pubias().
step3_pubias_statistical <- function(k) {
  k <- suppressWarnings(as.numeric(k))
  if (length(k) != 1L || is.na(k)) return(FALSE)
  k >= 10
}

# The one-line automatic step the breadcrumb shows in place of a Q2 screen.
# Names the step rather than numbering it: the wizard prints no question
# numbers, because the chart beside it puts a pmatools node between Fig 5's
# Q1 and Q2 and the numbering matched neither.
step3_pubias_k_line <- function(k) {
  k <- suppressWarnings(as.numeric(k))
  if (length(k) != 1L || is.na(k)) k <- 0
  if (step3_pubias_statistical(k)) {
    sprintf("Statistical analysis feasible - k = %g >= 10, funnel / Egger", k)
  } else {
    sprintf("Statistical analysis not feasible - k = %g < 10, registry route",
            k)
  }
}

# "Question 2 of 3" for the node now on screen, or NULL for anything that is
# not a question (the terminal "result", or a node the current answers have
# taken off the path).
#
# The TOTAL is only printed once the answers settle the route, i.e. once
# "result" has joined the reachable path. Before that the reviewer's own next
# answer decides whether two more questions follow - Q1 = "yes" ends the wizard
# after one - so a total taken from the path so far would always equal the
# current index and would tell every reviewer they were on the last question.
step3_pubias_question_line <- function(node, path) {
  if (is.null(node) || length(node) != 1L || is.na(node)) return(NULL)
  questions <- setdiff(as.character(path), "result")
  at <- match(as.character(node), questions)
  if (is.na(at)) return(NULL)
  if (!"result" %in% as.character(path)) return(sprintf("Question %d", at))
  sprintf("Question %d of %d", at, length(questions))
}

# The nodes the CURRENT answers put on the path, in wizard order. Drives the
# breadcrumb (which links only the answered ones) and gates `reopen`, so a
# breadcrumb click can never strand the reviewer on a node the algorithm no
# longer reaches.
step3_pubias_reachable <- function(small_industry = NULL,
                                   registry_complete = NULL,
                                   k = 0L) {
  out <- "q1"
  if (!.pubias_answered(small_industry)) return(out)
  if (identical(.pubias_chr(small_industry), "yes")) return(c(out, "result"))
  out <- c(out, "extra")
  if (!.pubias_answered(registry_complete)) return(out)
  if (identical(.pubias_chr(registry_complete), "yes")) {
    return(c(out, "result"))
  }
  c(out, if (step3_pubias_statistical(k)) "q3" else "q4", "result")
}

# The figure ids the answers so far have LIT, for the chart that sits above the
# wizard (inst/figures/pubias.svg, ids catalogued as .PUBIAS_FIG5_NODE_IDS in
# R/domain_pubias.R).
#
# Why this is not pma_flow_path_ids(): that reads the `flow_path` fact, which
# does not exist until grade_meta() has RATED the domain. The chart's job here
# is the opposite one - it is a progress indicator shown from the first node,
# so it has to light up from the answers alone, before any rating exists.
#
# Two vocabularies meet here. The wizard's node keys (q1 / extra / q3 / q4 /
# result) are not the figure's ids, and the mapping is not one-to-one: `extra`
# is the pmatools registry node, and the k gate is the figure's q2, which the
# wizard never asks. Hence a pure function with tests rather than inline logic.
#
# Nothing is lit until something is answered: an unlit chart says "you have not
# started", and lighting the entry node before the reviewer has touched it
# would say the opposite.
#
# STEP3_PUBIAS_USE_EGGER is an answer, not a blank: it says "I looked, and I
# accept the automated test". It used to stop the trail at pma-pubias-node-q3
# because this function had no p value, so a reviewer who accepted Egger saw a
# chart that looked unfinished for the rest of the assessment.
# `egger_asymmetric` is that p value's verdict, passed in by the caller (the
# reactive that runs metabias() for the callout) rather than computed here -
# the function stays pure and side-effect free, which is what makes it
# unit-testable. NULL or NA still stops at the node, because then the leaf
# genuinely is not decided.
step3_pubias_flow_ids <- function(small_industry = NULL,
                                  registry_complete = NULL,
                                  funnel_asymmetry = NULL,
                                  unpublished = NULL,
                                  k = 0L,
                                  egger_asymmetric = NULL) {
  if (!.pubias_answered(small_industry)) return(character(0))

  ids <- "pma-pubias-node-q1"
  if (identical(.pubias_chr(small_industry), "yes")) {
    return(c(ids, "pma-pubias-edge-q1-yes", "pma-pubias-leaf-down1-q1"))
  }
  ids <- c(ids, "pma-pubias-edge-q1-no", "pma-pubias-node-registry")

  if (!.pubias_answered(registry_complete)) return(ids)
  registry <- .pubias_chr(registry_complete)
  if (identical(registry, "yes")) {
    return(c(ids, "pma-pubias-edge-registry-yes",
             "pma-pubias-leaf-nodown-registry"))
  }
  # "no" carries on down the chart. The k gate below it is computed rather
  # than asked, so lighting its node AND the edge out of it is the only way
  # the reviewer sees which branch the study count chose for them.
  ids <- c(ids, "pma-pubias-edge-registry-no", "pma-pubias-node-q2")
  if (step3_pubias_statistical(k)) {
    ids <- c(ids, "pma-pubias-edge-q2-yes", "pma-pubias-node-q3")
    asymmetry <- .pubias_chr(funnel_asymmetry)
    if (identical(asymmetry, STEP3_PUBIAS_USE_EGGER)) {
      asymmetry <- .pubias_egger_answer(egger_asymmetric)
    }
    if (identical(asymmetry, "yes")) {
      return(c(ids, "pma-pubias-edge-q3-yes", "pma-pubias-leaf-down1-q3"))
    }
    if (identical(asymmetry, "no")) {
      return(c(ids, "pma-pubias-edge-q3-no", "pma-pubias-leaf-nodown-q3"))
    }
    return(ids)
  }

  ids <- c(ids, "pma-pubias-edge-q2-no", "pma-pubias-node-q4")
  documented <- .pubias_chr(unpublished)
  if (identical(documented, "yes")) {
    return(c(ids, "pma-pubias-edge-q4-yes", "pma-pubias-leaf-down1-q4"))
  }
  if (identical(documented, "no")) {
    return(c(ids, "pma-pubias-edge-q4-no", "pma-pubias-leaf-nodown-q4"))
  }
  ids
}

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

# Should the on-screen widget be re-pushed from its reactiveVal, and to what?
#
# BUG FIX: the Configuration boxes could show the PREVIOUS outcome's number
# while the app rated against the current one. output$threshold_panel seeds
# every widget from its reactiveVal under isolate(), but app.R's provenance
# guard resets those reactiveVals AFTER the panel has already flushed - that
# observer is created later than step3_server's outputs, so it runs later in
# the same flush. The re-created widget therefore carries the old value, and
# Shiny's client suppresses the re-send of a value the server already holds,
# so no input event ever corrects it. Confirmed by instrumenting both sides:
# after switching the event column the box read 74.3 per 1,000 while
# threshold_baseline_state() held 127.0, and the rating used 127.0. A reviewer
# reading a baseline risk off the screen and accepting it was accepting a
# number the app was not using.
#
# The caller keys the observer on the reactiveVal, NOT on the input, which is
# what makes the two guards below safe: emptying a box does not change the
# state, so a cleared field is never refilled behind the reviewer's back.
#
# Returns the value to push, or NULL to leave the widget alone.
step3_widget_sync_value <- function(state_value, input_value,
                                    tolerance = 1e-8) {
  if (is.null(state_value) || length(state_value) != 1L ||
      !is.numeric(state_value) || !is.finite(state_value)) {
    # An unseeded (NA) state is not a correction to make. The panel falls back
    # to the pooled control-group risk / the suggestion on purpose while the
    # seeding observers catch up (their order is not guaranteed), so pushing
    # NA here would blank a box that is showing the right number.
    return(NULL)
  }
  if (!is.null(input_value) && length(input_value) == 1L &&
      is.numeric(input_value) && !is.na(input_value) &&
      isTRUE(all.equal(as.numeric(input_value), as.numeric(state_value),
                       tolerance = tolerance))) {
    # Already agrees - normally because the reviewer has just typed it.
    # Re-pushing an identical value would move the caret to the end of the box
    # while they are still editing it.
    return(NULL)
  }
  as.numeric(state_value)
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

# .mic_note() was deleted here. It warned the reviewer not to equate the
# decision threshold with a Minimally Important Change - a term this project
# is retiring. The API is threshold / threshold_type / threshold_scale, and
# this was the last place the UI still named MIC at all.

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
# This app implements the responder proportion only, through Chinn's formula
# rather than Core GRADE 6's own procedure. Both departures are still stated
# on screen; the recitation of what Core GRADE 6 ranks
# (EDU_COPY$config_tab$continuous_intro) is not, because a reviewer answered
# nothing with it.
#
# The two presentations are offered as an either/or (input$sof_presentation),
# defaulting to the effect itself. NEITHER changes the certainty rating: the
# conversion reaches sof_table() and nothing else, while Imprecision is rated
# on the SMD/MD against the threshold set in the section rendered just above
# this block.
#
# `p0` is the seed for the proportion box, passed in by the caller from the
# reactiveVal that owns it - the widget must not re-assert the constant on
# every rebuild, or a proportion the reviewer replaced and justified is thrown
# away whenever the panel re-renders. RESPONDER_P0_DEFAULT stays the default
# argument so the block still stands alone, and the two conditionalPanel
# conditions below deliberately keep comparing against the CONSTANT: what
# obliges a rationale is departing from the app convention, not departing from
# whatever happens to be seeded.
.responder_block <- function(sm, p0 = RESPONDER_P0_DEFAULT) {
  convertible <- sm %in% c("SMD", "MD")
  if (!convertible) {
    return(.config_section(
      "Presentation of this outcome",
      # continuous_departure describes the CHOICE between the two
      # presentations, and there is no choice on this branch. The italic note
      # below states this measure's own departure, and states it exactly.
      htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        sprintf(paste0(
          "The responder conversion is defined on the standardized mean ",
          "difference only, so the Summary of Findings table reports the %s ",
          "itself, and the certainty rating reads the decision threshold ",
          "above either way."), sm))
    ))
  }
  # The badge is its own output on purpose: if this renderUI depended on
  # the confirmation state, ticking the box (or typing a rationale) would
  # rebuild the panel and reset the very widget being used.
  .config_section(
    htmltools::tagList(
      "Presentation of this outcome",
      shiny::uiOutput("responder_p0_badge", inline = TRUE)),
    .config_note(EDU_COPY$config_tab$continuous_departure),
    .config_note(EDU_COPY$config_tab$chinn_caveat),
    # A two-way radio rather than a tick-box, and defaulting to the effect
    # itself. The conversion used to be on by default, which read as though
    # the rating REQUIRED a binary presentation. It does not: grade_meta()
    # never sees the conversion, Imprecision is rated on the SMD/MD against
    # the threshold below either way, and this choice only reaches
    # sof_table(). Presenting the two as an explicit either/or says so.
    shiny::radioButtons("sof_presentation",
      "How should the Summary of Findings table present this outcome?",
      choices = stats::setNames(
        c("effect", "responder"),
        c(sprintf("The %s itself, on its own scale", sm),
          paste0("The proportion of responders, converted with Chinn's ",
                 "formula (Core GRADE 6 option 2)"))),
      selected = "effect"),
    shiny::conditionalPanel(
      "input.sof_presentation == 'responder'",
      shiny::numericInput("baseline_risk_chinn",
        paste0("Proportion of control patients meeting the threshold of ",
               "clinical interest"),
        value = if (length(p0) == 1L && is.numeric(p0) && is.finite(p0)) {
          p0
        } else {
          RESPONDER_P0_DEFAULT
        },
        min = 0.01, max = 0.99, step = 0.01),
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
.equiv_lines <- function(eq, dir = NULL, per = STEP3_PER_DEFAULT) {
  sm <- eq$sm
  .lbl <- function(p) step3_per_label(1000 * p, per)
  up <- sprintf("Increase: %s -> %s, equivalent %s %.3f",
                .lbl(eq$p0), .lbl(eq$p1_up), sm, eq$ratio_up)
  dn <- if (isTRUE(eq$down_ok)) {
    sprintf("Decrease: %s -> %s, equivalent %s %.3f",
            .lbl(eq$p0), .lbl(eq$p1_dn), sm, eq$ratio_dn)
  } else {
    "Decrease: not shown - the threshold exceeds the control-group risk."
  }
  if (is.null(dir)) {
    return(list(up = up, dn = dn, alg = character(), approx = character()))
  }
  # The reason clause has to come from the pooled effect's own direction, not
  # from the side that ended up exact. They usually agree, but they diverge on
  # the fallback path: when the threshold is not smaller than the control-group
  # risk the decrease-side conversion is undefined, so a below-the-null effect
  # is converted on the increase side. Reading the word off exact_side there
  # made this sentence assert "lies above the null" two lines above a caveat
  # saying the opposite. dir$caveat explains the fallback, so say only that the
  # side was chosen, not why, when the two disagree.
  side_matches <- identical(dir$exact_side, dir$direction)
  reason <- if (side_matches) {
    sprintf(" because the pooled effect lies %s the null",
            if (identical(dir$direction, "decrease")) "below" else "above")
  } else {
    ""
  }
  # Signed, so the reader can see which way the band moves; the unit follows
  # the Configuration setting like every other rate on the tab.
  .signed <- function(p1000) {
    v <- step3_to_per(p1000, per)
    sprintf("%+.0f %s", v, step3_per_unit_label(per))
  }
  alg <- sprintf(
    paste0("What the algorithm uses: a symmetric +/- log(%.3f) band, ",
           "converted on the %s side%s. That side is exact - %s %.3f is ",
           "%s at this control-group risk."),
    dir$ratio, dir$exact_side, reason,
    sm, dir$exact_ratio, .signed(1000 * dir$exact_ard))
  approx <- sprintf(
    paste0("The %s side is therefore the approximate one: the band's ",
           "mirror is %s %.3f, which implies %s rather than ",
           "%s. Imprecision's two-level rule asks whether the ",
           "confidence interval crosses both thresholds, so that one ",
           "crossing is judged against the mirrored value."),
    dir$approx_side, sm, dir$approx_ratio,
    .signed(1000 * dir$approx_ard), .signed(-1000 * dir$exact_ard))
  if (!is.na(dir$caveat)) approx <- paste(approx, dir$caveat)
  list(up = up, dn = dn, alg = alg, approx = approx)
}
