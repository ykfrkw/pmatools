# outcome_provenance.R - does this answer still belong to this outcome?
#
# Split out of ui_helpers.R along with the bank it guards. Every helper here
# answers one question in a different tense:
#
#   - was this row rated on the dataset now loaded in Step 1? (the dataset,
#     outcome and analysis signatures, and the staleness comparison built on
#     them)
#   - which inputs belong to the outcome now open, so that opening the next one
#     can clear exactly those and nothing else? (PMA_OUTCOME_INPUT_IDS and the
#     confirmation ids under it)
#   - which of those answers may be pushed back into a widget Shiny has just
#     rebuilt, and which must be re-given by hand? (the restore groups and
#     pma_restorable_value())
#   - where did each grade_meta() argument come from, so the exported
#     analysis.R can say so? (pma_arg_spec() and the exported-argument list)
#
# They are one file because they are one hazard: an answer outliving the thing
# it was an answer to. Splitting them would let the registry of ids drift from
# the signature that decides when to clear them.
#
# THE RULE FOR A NEW HELPER: it belongs here when it decides whether something
# already recorded is still valid, or names the ids such a decision ranges
# over. A helper that RENDERS the verdict is display - pma_stale_badge() and
# pma_stale_warning_banner() are here only because they are the sole readers of
# the staleness count and would otherwise be two lines of HTML with no home.
#
# Pure throughout, with one exception: pma_clear_outcome_confirmations() takes
# a `session` because clearing a checkbox is a message to the browser. Keep it
# the only one.
#
# CROSS-FILE: PMA_GRADE_ARGS_ATTR is defined here and also read by
# pma_export_set() in R/outcome_bank.R and by grade_obj() in R/step3_grade.R,
# which stamps it. It stays here because the attribute records provenance, not
# bank membership. PMA_DATASET_SIGNATURE_ATTR is read by R/step3_grade.R and
# PMA_SIGNATURE_IGNORE_COLS by R/step1_data.R for the same reason. Both reads
# happen at call time, so source order does not matter.

# ----- Dataset provenance guard -------------------------------------------
# A saved outcome carries the signature of the dataset it was rated on, so
# Step 3 / Step 4 can flag outcomes that came from a DIFFERENT dataset than
# the one currently loaded. Mixing outcomes from different datasets into one
# Summary of Findings table is a serious scientific error, but saved work is
# never silently discarded: the app warns, the user decides.
PMA_DATASET_SIGNATURE_ATTR <- "pma_dataset_signature"

# Columns that describe the app's own per-study JUDGMENTS rather than the
# dataset. Step 3 writes RoB / Indirectness edits back into state$data, so
# including them would flag outcomes saved earlier from the very same data.
PMA_SIGNATURE_IGNORE_COLS <- c("rob", "indirectness")

# Stable signature of a long-format dataset. Pure function, no {digest}
# dependency: sorted structural features plus a coarse numeric fingerprint,
# pasted together. Same data -> same string (row order and column order do
# not matter); different studies, rows, columns or numbers -> different
# string. Returns NA_character_ when there is no usable data frame, which
# callers treat as "unknown" (never stale).
pma_dataset_signature <- function(d) {
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0L || ncol(d) == 0L) {
    return(NA_character_)
  }
  keep <- !(names(d) %in% PMA_SIGNATURE_IGNORE_COLS)
  d <- d[, keep, drop = FALSE]
  if (ncol(d) == 0L) return(NA_character_)
  parts <- c(
    paste0("nrow=", nrow(d)),
    paste0("cols=", paste(sort(names(d)), collapse = "|"))
  )
  if ("studlab" %in% names(d)) {
    studies <- sort(unique(as.character(d$studlab)))
    parts <- c(parts,
               paste0("k=", length(studies)),
               paste0("studies=", paste(studies, collapse = "|")))
  }
  if ("outcome" %in% names(d)) {
    parts <- c(parts, paste0("outcomes=", paste(
      sort(unique(as.character(d$outcome))), collapse = "|")))
  }
  # Numeric fingerprint: per-column (sum, NA count). Order-independent, so
  # re-sorting rows is not mistaken for a new dataset, while different
  # effect data on the same studies still is.
  num_cols <- sort(names(d)[vapply(d, is.numeric, logical(1))])
  if (length(num_cols)) {
    fp <- vapply(num_cols, function(cn) {
      x <- as.numeric(d[[cn]])
      sprintf("%s:%s:%d", cn,
              format(round(sum(x, na.rm = TRUE), 6),
                     scientific = FALSE, trim = TRUE),
              sum(is.na(x)))
    }, character(1))
    parts <- c(parts, paste0("num=", paste(fp, collapse = "|")))
  }
  paste(parts, collapse = "\r")
}

# Signature recorded on one saved outcome (NA when it carries none).
pma_outcome_signature <- function(g) {
  sig <- attr(g, PMA_DATASET_SIGNATURE_ATTR, exact = TRUE)
  if (is.null(sig) || length(sig) != 1) return(NA_character_)
  as.character(sig)
}

# Which saved outcomes came from a different dataset than `signature`?
# Returns a named logical vector aligned with pma_outcomes_list(outcomes).
# Unknown signatures (either side) are NOT flagged: the guard only fires on
# positive evidence of a mismatch.
pma_outcomes_stale <- function(outcomes, signature = NULL) {
  outcomes <- pma_outcomes_list(outcomes)
  out <- rep(FALSE, length(outcomes))
  names(out) <- names(outcomes)
  if (length(outcomes) == 0) return(out)
  if (is.null(signature) || length(signature) != 1 || is.na(signature)) {
    return(out)
  }
  for (i in seq_along(outcomes)) {
    sig <- pma_outcome_signature(outcomes[[i]])
    out[i] <- !is.na(sig) && !identical(sig, as.character(signature))
  }
  out
}

# ----- Outcome provenance guard -------------------------------------------
# pma_dataset_signature() above answers "was this rated on other DATA?". It
# cannot answer "was this rated on another OUTCOME?", because every outcome of
# a review lives in one dataset, so switching between them leaves that
# signature untouched. The Step 3 confirmations depend on the second question,
# and it gets its own signature here.
#
# What counts as a different outcome
# ----------------------------------
# NOT the outcome name. A reviewer who corrects a typo in "Depresion
# response", or relabels a row for the Summary of Findings table, has not
# changed a single judgment, and voiding a finished assessment over a rename
# would punish good bookkeeping. What the Step 3 answers are ABOUT is the body
# of evidence handed to run_ma(), so the identity is built from that - which
# studies, and the arm-level numbers of each - together with the Step 2
# direction answer, because the direction decides which side of the threshold
# counts as benefit and so flips Risk of Bias, Inconsistency and Imprecision
# without changing a number.
#
# Deliberate consequences:
#   * remapping the events (or mean / SD) column - a different outcome
#     measured on the same studies - changes the arm-level numbers, so the
#     identity changes and Step 3 is voided;
#   * flipping the direction voids Step 3, for the reason above;
#   * loading different data voids Step 3 (and the dataset guard fires too);
#   * renaming the outcome, editing its follow-up or unit, or re-running with
#     a different pooling method, tau-squared estimator or summary measure
#     leaves the identity alone. Those change the estimate, not the question:
#     the five domains are recomputed from the new fit anyway, and a change of
#     scale already re-seeds the threshold on its own (threshold_seed_key in
#     step3_grade.R).
#
# Returns NA_character_ when there is no analysis, which callers read as
# "unknown" and never treat as a change.
pma_analysis_signature <- function(ma, small_values = NULL) {
  if (is.null(ma) || !inherits(ma, "meta")) return(NA_character_)
  studlab <- as.character(ma$studlab %||% character(0))
  if (length(studlab) == 0L) return(NA_character_)
  # Sorted, so re-running after a row re-sort is not mistaken for a new
  # outcome; the arm-level vectors follow the same order.
  ord <- order(studlab)
  .field <- function(nm) {
    v <- ma[[nm]]
    if (is.null(v) || length(v) != length(studlab)) return(paste0(nm, "="))
    paste0(nm, "=", paste(format(v[ord], scientific = FALSE, trim = TRUE),
                          collapse = ","))
  }
  paste(
    paste0("studies=", paste(studlab[ord], collapse = "|")),
    .field("event.e"), .field("n.e"), .field("event.c"), .field("n.c"),
    .field("mean.e"), .field("sd.e"), .field("mean.c"), .field("sd.c"),
    paste0("direction=",
           if (is.null(small_values) || length(small_values) != 1L) ""
           else as.character(small_values)),
    sep = "\r"
  )
}

# ----- Inputs that belong to ONE outcome ----------------------------------
# THE single registry of Step 2 / Step 3 input ids whose answers describe the
# outcome currently being rated, rather than the studies, the dataset or how a
# plot is drawn. Two things read it, and nothing else in the app enumerates
# these ids:
#
#   1. the freshness guard in step3_server(). Shiny keeps the last value of an
#      input whose widget has been torn down, so between "the outcome changed"
#      and "Step 3 was rendered again" every one of these still reports the
#      PREVIOUS outcome's answer. The guard records which outcome each id was
#      last touched for, and domain_confirmed() ignores the ones that are out
#      of date;
#   2. pma_clear_outcome_confirmations(), which unticks the confirmation boxes
#      on screen the moment the outcome changes.
#
# ADDING A NEW DOMAIN INPUT? Put its id in the group below that names its tab.
# An id that is missing here is an id whose stale answer can still count
# towards the export gate.
#
# What is deliberately NOT here: the column mapping and model settings (they
# usually carry over to the next outcome unchanged), the per-study risk-of-bias
# and indirectness tables (properties of the studies, not of the outcome), and
# every forest / funnel display field (presentation, re-derived per plot).
PMA_OUTCOME_INPUT_IDS <- list(
  # Step 2 - outcome identity
  identity = c("outcome_name", "small_values", "outcome_type",
               "outcome_follow_up", "outcome_unit"),
  # Step 3 - Configuration tab (threshold, control-group risk, responder
  # conversion). Only some of these exist at a time: the binary and continuous
  # branches of output$threshold_panel build different widgets.
  configuration = c("threshold_mode", "threshold_abs", "threshold_ratio",
                    "threshold_cont", "threshold_baseline_input",
                    "threshold_baseline_rationale", "sof_presentation",
                    "baseline_risk_chinn", "responder_p0_rationale",
                    "responder_p0_confirm", "threshold_label",
                    "threshold_confirm"),
  # Step 3 - Risk of Bias. rob_some_concerns is absent on purpose: it is a
  # review-wide convention rather than an answer about this outcome, and it
  # persists across a change of outcome. Moving its widget to this tab did not
  # change that. (rob_inf_threshold used to sit beside it here; the slider is
  # gone and the package default applies unconditionally.)
  rob = c("rob_override", "rob_override_rationale", "rob_confirm_na"),
  # Step 3 - Inconsistency. ci_diff and threshold_side are gone: Core GRADE
  # 3's Steps 1 and 2 are derived by .auto_inconsistency(), and the app no
  # longer asks questions the algorithm has already answered. Step 3
  # (subgroup_explained) is the one a human has to answer.
  inconsistency = c("subgroup_explained",
                    "incon_override", "incon_override_rationale",
                    "incon_confirm_na"),
  # Step 3 - Indirectness (the four Core GRADE 5 PICO questions, plus the
  # optional override of their worst-case fold)
  indirectness = c("indir_population", "indir_intervention", "indir_comparator",
                   "indir_outcome", "indirectness", "indir_rationale",
                   "indir_confirm_na"),
  # Step 3 - Imprecision
  imprecision = c("ois_rrr", "ois_sd", "ois_events_override",
                  "ois_n_override", "impre_override",
                  "impre_override_rationale", "impre_confirm_na"),
  # Step 3 - Publication bias
  pubias = c("pubias_registry_complete", "pubias_small_industry",
             "pubias_unpublished", "pubias_funnel_asymmetry",
             "pubias_fa_rationale", "pubias_override",
             "pubias_override_rationale", "pubias_confirm_na"),
  # Step 3 - Final certainty ("Other considerations")
  final = c("other_text", "other_downgrade")
)

pma_outcome_input_ids <- function() {
  unname(unlist(PMA_OUTCOME_INPUT_IDS, use.names = FALSE))
}

# The subset whose cleared value is unambiguous: a confirmation is either
# given or it is not, so these can be pushed to the client without restating
# any widget's declared default.
#
# Everything else in the registry is reset by one of two other mechanisms, and
# which one applies is worth knowing before changing either:
#
#   - The Configuration tab's numeric answers (the thresholds, the
#     control-group risk, the responder proportion) live in reactiveVals in
#     step3_server(). state$step3_reset() puts those back, and
#     output$threshold_panel re-renders from them because it depends on
#     state$outcome_gen. A rebuild alone does NOT reset them, which is the
#     point: re-rendering the panel within one outcome must not discard a
#     number the reviewer entered and justified.
#   - Everything else clears itself when app.R rebuilds the step body from
#     step3_ui(), because a freshly built widget pushes its own declared
#     default back to the server.
#
# So no default is written twice, but only the second group is self-clearing.
PMA_OUTCOME_CONFIRM_IDS <- c("threshold_confirm", "responder_p0_confirm",
                             "rob_confirm_na", "incon_confirm_na",
                             "indir_confirm_na", "impre_confirm_na",
                             "pubias_confirm_na")

pma_clear_outcome_confirmations <- function(session) {
  for (id in PMA_OUTCOME_CONFIRM_IDS) {
    shiny::updateCheckboxInput(session, id, value = FALSE)
  }
  invisible(NULL)
}

# ----- Putting an answer back after Step 3 is rebuilt ----------------------
# app.R renders output$step_body from step3_ui() on every entry, so leaving
# Step 3 and coming back destroys every widget on it and builds it again from
# its declared defaults. Measured before this was added: entering an override
# of the pooled control-group risk (210 per 1,000), justifying it in writing,
# then visiting Step 4 and returning left the box reading 155.6 - the pooled
# value - with the rationale, the risk-of-bias override rationale and the
# free-text "other considerations" all blank. Nothing warned; the reviewer's
# override was simply gone, and the rebuilt widgets reported their defaults to
# the server as though those were the answers.
#
# These are the ids worth restoring. Two groups are deliberately excluded:
#
#   - `identity`, which is Step 2's own UI. It is not on screen when Step 3 is
#     rebuilt, and Step 2 already keeps those answers in state$.
#   - PMA_OUTCOME_CONFIRM_IDS. A confirmation says "I have looked at what is on
#     screen"; re-arming it after a rebuild costs one tick and keeps the export
#     gate conservative, which is the direction this app errs in everywhere
#     else. Restoring the answers WITHOUT the confirmations is what makes that
#     re-tick cheap - the reviewer re-confirms, they do not re-type.
PMA_OUTCOME_RESTORE_GROUPS <- c("configuration", "rob", "inconsistency",
                                "indirectness", "imprecision", "pubias",
                                "final")

pma_restorable_input_ids <- function() {
  ids <- unlist(PMA_OUTCOME_INPUT_IDS[PMA_OUTCOME_RESTORE_GROUPS],
                use.names = FALSE)
  setdiff(unname(ids), PMA_OUTCOME_CONFIRM_IDS)
}

# Is a remembered answer worth pushing back into a freshly built widget?
#
# `stamp` is the outcome generation the answer was given in and `gen` the one
# now open; they must match, for exactly the reason the freshness guard in
# step3_server() exists - an answer left behind by the previous outcome must
# never be reinstated as if it belonged to this one.
#
# Empty answers are skipped rather than pushed. Restoring "" over a widget that
# was just built empty changes nothing, and skipping keeps the message small.
# The one thing this gives up is re-clearing a field whose declared default is
# non-empty; every such field in the registry is a numeric backed by a
# reactiveVal, which is restored by its own render and not by this path.
pma_restorable_value <- function(value, stamp, gen) {
  if (!identical(stamp, gen)) return(FALSE)
  if (is.null(value) || length(value) == 0L) return(FALSE)
  if (length(value) == 1L && is.na(value)) return(FALSE)
  if (is.character(value) && length(value) == 1L && !nzchar(trimws(value))) {
    return(FALSE)
  }
  TRUE
}

# ----- grade_meta() argument specs for the exported analysis.R ------------
# export_bundle() renders analysis.R from `grade_args`: a named list of
# {value, origin, col} specs, one per grade_meta() argument. An argument it is
# not given falls back to a template default, which is usually the literal
# NULL - so an argument the app supplied but did not declare here disappears
# from the "reproducible" script, and the script reproduces a different rating
# from the one it ships with.
#
# `origin` must be one of "null" / "column" / "scalar" / "vector"; anything
# else aborts (pmatools 0.5.0 breaking change - it used to render NULL in
# silence, which is exactly the failure this list exists to prevent).
PMA_GRADE_ARGS_ATTR <- "pma_grade_args"

pma_arg_spec <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(list(value = NULL, origin = "null"))
  }
  # indirectness_subdomains is a data frame; export_bundle() literalises it
  # from the frame itself (.indirectness_subdomains_lit) and never consults
  # the origin, so any valid origin will do.
  if (is.data.frame(value) || is.list(value)) {
    return(list(value = value, origin = "scalar"))
  }
  if (length(value) == 1L) {
    # NA is "not supplied", not the string "NA": rendering shQuote(NA) would
    # put 'NA' into the script and change the call.
    if (is.na(value)) return(list(value = NULL, origin = "null"))
    return(list(value = value, origin = "scalar"))
  }
  list(value = value, origin = "vector")
}

# grade_meta() arguments the app supplies that the bundled analysis.R would
# otherwise drop. Excluded on purpose because export_bundle() already recovers
# them from the rated object: study_design, outcome_type, outcome_name, and
# (whenever a subdomain table exists) indirectness / indirectness_rationale.
#
# threshold_baseline belongs here as of pmatools 0.5.1: the app converts an
# absolute threshold to the analysis scale at a baseline risk it chooses (a
# pooled metaprop estimate, or a manual override with a written rationale),
# and the template now has a slot for it. Without the argument the regenerated
# call re-derives the baseline from the pooled control-arm risk, which is not
# in general the number the reviewer set.
PMA_GRADE_ARGS_EXPORTED <- c(
  # rob_inflation_threshold is off this list as of 0.5.1: the app no longer
  # sets it, so it could never be emitted, and export_bundle() already writes
  # the package default (PMA_ROB_INFLATION_THRESHOLD) into the bundled
  # analysis.R.
  "rob", "rob_rationale", "rob_some_concerns",
  "small_values",
  "indirectness", "indirectness_rationale", "indirectness_subdomains",
  "inconsistency", "inconsistency_rationale", "inconsistency_ci_diff",
  "inconsistency_threshold_side", "inconsistency_subgroup_explained",
  "imprecision", "imprecision_rationale",
  "threshold", "threshold_scale", "threshold_baseline",
  "ois_p0", "ois_rrr", "ois_sd", "ois_events", "ois_n",
  "pubias_small_industry", "pubias_funnel_asymmetry", "pubias_rationale",
  "pubias_unpublished", "pubias_registry_complete",
  # The rare-event facts change three computations (the OIS basis, the
  # Inconsistency I2 surrogate, the Fig 5 route), so a script that omitted them
  # would re-run the same data and report a different rating. The single-outcome
  # template recovers them from the rated object as a fallback; the
  # multi-outcome one renders per_outcome and nothing else, so they have to be
  # declared here as well.
  "rare_flow", "rare_one_arm_total_zero", "rare_method"
)

# Only the arguments the app actually set are emitted. Before pmatools 0.5.1
# every name above had to be emitted unconditionally, with a "null" spec
# standing in for the ones the app never passed: export_bundle() looked the
# specs up with `grade_args$<name>`, and `$` partial-matches on lists, so with
# `inconsistency` absent `grade_args$inconsistency` returned the
# `inconsistency_ci_diff` spec and wrote that answer into the wrong argument.
# A complete list was the workaround. export_bundle() now looks every name up
# exactly and rejects one it does not know, so a partial list is safe again -
# and a typo here fails loudly instead of vanishing from the script.
pma_grade_arg_specs <- function(args) {
  out <- list()
  for (nm in intersect(PMA_GRADE_ARGS_EXPORTED, names(args))) {
    out[[nm]] <- pma_arg_spec(args[[nm]])
  }
  out
}

# Small "different dataset" badge shown on a stale saved-outcome row.
pma_stale_badge <- function(text = "different dataset") {
  htmltools::span(
    class = "pma-badge",
    style = sprintf(
      "background: %s; color: %s; border: 1px solid %s; white-space: nowrap;",
      PMA_ALERT_BG, PMA_ALERT_FG, PMA_ALERT_FG),
    text
  )
}

# Warning banner above the combined SoF preview. Returns NULL when nothing
# is stale, so callers can drop it straight into a tagList.
pma_stale_warning_banner <- function(n_stale) {
  if (is.null(n_stale) || is.na(n_stale) || n_stale < 1) return(NULL)
  htmltools::div(
    style = paste0(
      "padding: 0.75rem 1rem; margin-bottom: 1rem; ",
      "background: ", PMA_ALERT_BG, "; border-left: 4px solid ", PMA_ALERT_FG,
      "; border-radius: 4px; font-size: 0.9rem;"),
    htmltools::strong("Different dataset detected. "),
    sprintf(paste0(
      "%d of the saved outcomes below %s saved from a dataset other than the ",
      "one currently loaded in Step 1 (marked \"different dataset\"). A ",
      "Summary of Findings table must describe one body of evidence: check ",
      "these rows before combining or exporting them. "),
      n_stale, if (n_stale == 1) "was" else "were"),
    "Nothing has been removed - remove the rows that do not belong, or ",
    "reload the dataset they came from."
  )
}
