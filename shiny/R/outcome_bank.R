# outcome_bank.R - the multi-outcome bank: what a saved outcome is, and how the
# list of them is built, shown and exported
#
# Split out of ui_helpers.R, which had grown past two and a half thousand lines
# by being the file with no subject. This one has a subject: state$outcomes,
# the named list of rated pmatools objects (and pmatools_not_reported rows)
# that Step 4 turns into a combined Summary of Findings. Minting the uid that
# survives a rename, upserting a row under it, stamping and reading the display
# and source attributes, rebuilding a pmatools_set out of the lot, reconciling
# the meta-analysis arguments the rows were fitted with, listing the bank on
# screen, and the two modals that add the next outcome.
#
# THE RULE FOR A NEW HELPER: it belongs here when its subject is THE LIST or
# ONE ROW OF IT. A helper that asks whether a row still matches the loaded
# dataset is provenance and belongs in R/outcome_provenance.R beside the
# signature it compares; a helper that renders a rating rather than a row
# belongs in R/judgment_display.R.
#
# Nothing here is reactive. Every function takes the outcome list as an
# argument and returns a value or HTML, which is what lets the bank be tested
# without a Shiny session - preserve that.
#
# Built on the package's R/not_reported.R (`.is_not_reported()`,
# `.rated_outcomes()`, `not_reported_outcome()`), which the app gets from the
# staged copy under R/_pmatools/. Two constants are read from elsewhere at call
# time: PMA_GRADE_ARGS_ATTR (R/outcome_provenance.R), which pma_export_set()
# reads off a banked object, and PMA_ALERT_FG (R/ui_helpers.R), which colours
# the "different dataset" line under a stale bank row.

# ----- Saved outcomes (multi-outcome Summary of Findings) -----------------
# state$outcomes is a NAMED LIST of pmatools objects (the value of
# state$grade the last time every certainty domain was confirmed), keyed by
# outcome label, in insertion order. It is exactly the shape grade_table()
# expects, so it can be passed straight through without reshaping. The write
# time is carried as attr(<obj>, "pma_saved_at") because attributes survive
# both the list round-trip and grade_table()'s inherits() check.
#
# The attribute name is historical: there is no Save button any more (see
# shiny/SPEC.md 3.4.14), so what it holds is when the row was last recomputed.
# On screen it is labelled "last updated" for that reason; renaming the
# attribute would only mean rewriting every reader of it.
PMA_SAVED_AT_ATTR <- "pma_saved_at"

# ----- Outcome identity: a uid, not the display name ----------------------
# Under auto-save a row is re-banked on every recompute, so "which row is
# this?" has to survive a rename - otherwise correcting a typo in the outcome
# name adds a second row rather than relabelling the first. That was true
# before auto-save too; auto-save would have made it constant.
#
# The uid is minted per outcome by app.R's begin_new_outcome() and carried as
# an attribute, the way the dataset signature is. names(outcomes) stays the
# DISPLAY name: grade_table(), pma_saved_outcomes_ui(), .outcome_set() and
# set$order all key on it.
PMA_OUTCOME_UID_ATTR <- "pma_outcome_uid"

# The uid stamped on one saved outcome (NA when it carries none - anything
# banked before this existed, which then behaves as it always did).
pma_outcome_uid <- function(g) {
  uid <- attr(g, PMA_OUTCOME_UID_ATTR, exact = TRUE)
  if (is.null(uid) || length(uid) != 1L || is.na(uid)) return(NA_character_)
  as.character(uid)
}

# Insert `g` under `name`, or update the row that already carries `uid`.
#
# Pure, so it is testable without a Shiny session; the rename-in-place case is
# the one that matters. Position is preserved on an update, because row order
# is a statement about priority the reviewer set by hand.
pma_upsert_outcome <- function(outcomes, name, g, uid = NULL) {
  outcomes <- pma_outcomes_list(outcomes)
  name <- trimws(as.character(name %||% "")[1L])
  # A row with no name cannot be printed, reordered or removed - every one of
  # those keys on names(outcomes). Callers gate on this before they get here.
  if (is.na(name) || !nzchar(name)) stop("a saved outcome needs a name")
  uid <- as.character(uid %||% NA_character_)[1L]
  attr(g, PMA_OUTCOME_UID_ATTR) <- uid
  known <- vapply(outcomes, pma_outcome_uid, character(1))
  at <- if (is.na(uid)) integer(0) else which(!is.na(known) & known == uid)
  if (!length(at)) {
    # New row, or a different outcome claiming a name already in use. The
    # second case overwrites, which is what a named list does anyway and what
    # every reader of names(outcomes) needs: two rows cannot share one name.
    outcomes[[name]] <- g
    return(outcomes)
  }
  at <- at[1L]
  outcomes[[at]] <- g
  names(outcomes)[at] <- name
  # Renaming onto a name some OTHER row holds would leave a duplicate. The
  # other row is the older claim on that name and loses it, exactly as it
  # would have under the plain insert above.
  displaced <- setdiff(which(names(outcomes) == name), at)
  if (length(displaced)) outcomes <- outcomes[-displaced]
  outcomes
}

# ----- Per-outcome export material ----------------------------------------
# The ZIP is built from a pmatools_set, so everything export_bundle() needs
# about ONE outcome has to travel on that outcome rather than being read off
# the live Step 2 / Step 3 fields at download time - which describe whichever
# outcome is on screen, not the three banked before it. Two attributes carry
# it, split by who reads them:
#
#   PMATOOLS_DISPLAY_ATTR   a pmatools contract: the display arguments
#                           export_bundle.pmatools_set() reads per outcome.
#   PMA_OUTCOME_SOURCE_ATTR the app's own: what it needs to rebuild a set that
#                           can be re-run, i.e. the data the outcome was rated
#                           on and the arm labels it was pooled with.
PMA_OUTCOME_SOURCE_ATTR <- "pma_outcome_source"

# The pmatools_display attribute for the outcome being banked. Only the
# arguments that describe THIS analysis: `per` and `prediction` are properties
# of the table, and the bundler takes those once for the whole set.
#
# The responder presentation is NOT one of them. It is a property of the row -
# two continuous outcomes in one review can be presented differently and a
# binary one cannot be converted at all - so it is banked with the outcome, and
# grade_table() reads it back per row when it builds the combined Summary of
# Findings the ZIP carries.
pma_outcome_display <- function(display = list(), pubias_missing = NULL,
                                rare = NULL) {
  display <- display %||% list()
  display_facts <- list(
    forest_display      = display$forest_step2,
    forest_display_rob  = display$forest_rob,
    rare                = rare,
    rare_forest_display = display$forest_step2,
    pubias_missing_df   = pubias_missing
  )
  display_facts <- display_facts[!vapply(display_facts, is.null, logical(1))]

  # state$display$convert is the GUARDED boolean (shiny/SPEC.md §3.2): Step 3
  # writes it only after the effect measure and the responder proportion have
  # been checked, so an outcome shown as its effect carries nothing here.
  if (!isTRUE(display$convert)) return(display_facts)
  c(display_facts, list(
    convert_smd_to_or = TRUE,
    # Banked alongside the conversion, never instead of it: grade_table() reads
    # it only on a row that converts, and a row banked without it renders as
    # responder proportions alone - which is what the reviewer chose.
    keep_effect_scale = isTRUE(display$keep_effect_scale),
    baseline_risk     = display$baseline_risk,
    threshold_label   = display$threshold_label,
    chinn_invert      = isTRUE(display$chinn_invert)
  ))
}

pma_outcome_source <- function(data = NULL, experimental_label = NULL,
                               control_label = NULL) {
  list(data = data,
       experimental_label = experimental_label,
       control_label      = control_label)
}

# Stamp both attributes onto the object about to be banked. One call so the
# two can never be written apart.
pma_bank_export_material <- function(g, display = list(),
                                     pubias_missing = NULL, rare = NULL,
                                     data = NULL,
                                     experimental_label = NULL,
                                     control_label = NULL) {
  attr(g, PMATOOLS_DISPLAY_ATTR) <- pma_outcome_display(
    display = display, pubias_missing = pubias_missing, rare = rare)
  attr(g, PMA_OUTCOME_SOURCE_ATTR) <- pma_outcome_source(
    data = data, experimental_label = experimental_label,
    control_label = control_label)
  g
}

# ----- Rebuilding a pmatools_set from the banked outcomes ------------------

# Long-format data covering every banked outcome, which is what the bundled
# analysis.R re-ingests and splits with run_ma_multi(). Each outcome brings the
# dataset it was rated on and contributes it under its own name, so a review
# whose outcomes came from separate files still exports one data_long.csv that
# reproduces all of them. An `outcome` column already in the data is
# overwritten: it describes the measurement scale within one analysis, and here
# the column has to name the analysis.
pma_export_data <- function(outcomes) {
  # Rated only: a not-reported outcome carries no data by construction, and
  # naming it in data_long.csv's `outcome` column would tell the bundled
  # analysis.R to pool an outcome with no rows.
  outcomes <- pma_rated_outcomes(outcomes)
  if (length(outcomes) == 0L) return(NULL)
  frames <- list()
  for (nm in names(outcomes)) {
    src <- attr(outcomes[[nm]], PMA_OUTCOME_SOURCE_ATTR, exact = TRUE)
    source_data   <- if (is.list(src)) src$data else NULL
    if (!is.data.frame(source_data) || nrow(source_data) == 0L) next
    source_data <- pma_name_arms(source_data, src$experimental_label, src$control_label)
    source_data$outcome <- nm
    frames[[nm]] <- source_data
  }
  if (length(frames) == 0L) return(NULL)
  pma_bind_rows_union(frames)
}

# Which arm is which is a per-outcome answer - the reviewer picks it from the
# values of THIS outcome's data - and `run_ma_multi()` takes one
# experimental_label for every outcome. Rather than let two outcomes with
# different arm values fight over that one argument, the exported data says
# which arm is which in the column itself, which is also what run_ma() falls
# back to. Getting this wrong is not a cosmetic difference: the pooled effect
# comes back inverted and every judgment that reads its direction with it.
#
# The reviewer's own words for the arms are kept in `treat_label`, so nothing
# the dataset said is lost.
pma_name_arms <- function(data, experimental_label = NULL,
                          control_label = NULL) {
  usable <- function(v) {
    !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  }
  if (!usable(experimental_label) || !usable(control_label)) return(data)
  if (!"treat" %in% names(data)) return(data)
  arms <- as.character(data$treat)
  if (!all(c(experimental_label, control_label) %in% arms)) return(data)
  data$treat_label <- arms
  arms[arms == experimental_label] <- "experimental"
  arms[arms == control_label]      <- "control"
  data$treat <- arms
  data
}

# rbind over frames whose columns need not match: two outcomes can come from
# datasets that carry different optional columns (one has `rob`, one does not),
# and dropping to the intersection would throw away the very column the rating
# was made from.
pma_bind_rows_union <- function(frames) {
  cols <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(d) {
    for (missing_col in setdiff(cols, names(d))) d[[missing_col]] <- NA
    d[, cols, drop = FALSE]
  })
  do.call(rbind, c(frames, list(stringsAsFactors = FALSE,
                                make.row.names = FALSE)))
}

# run_ma() settings for the bundled analysis.R, keyed by outcome where they can
# differ and passed once where they cannot. run_ma_multi() applies its `...` to
# every outcome, so a setting the outcomes disagree about is omitted and the
# script falls back to run_ma()'s own default rather than claiming a value that
# was true for only some of them.
PMA_MA_UNIFORM_ARGS <- c("method.tau", "random", "common", "incr")

pma_set_ma_args <- function(outcomes) {
  # Rated only. `outcomes` here becomes run_ma_multi(outcomes = ) in the
  # generated analysis.R, and a not-reported outcome has nothing to run: the
  # script declares it with add_not_reported() after grade_meta_multi()
  # instead (.not_reported_block(), R/export_bundle_multi.R).
  outcomes <- pma_rated_outcomes(outcomes)
  per_outcome_value <- function(f) {
    values <- lapply(outcomes, f)
    values[!vapply(values, is.null, logical(1))]
  }
  list(
    outcomes     = names(outcomes),
    sm           = per_outcome_value(function(g) g$meta$sm),
    outcome_type = per_outcome_value(function(g) {
      if (!is.null(g$meta$event.e)) "binary" else "continuous"
    }),
    dots = pma_uniform_ma_dots(outcomes)
  )
}

pma_uniform_ma_dots <- function(outcomes) {
  uniform <- function(values) {
    values <- values[!vapply(values, is.null, logical(1))]
    if (!length(values)) return(NULL)
    if (length(unique(values)) != 1L) return(NULL)
    values[[1]]
  }
  dots <- list()
  for (arg in PMA_MA_UNIFORM_ARGS) {
    v <- uniform(lapply(outcomes, function(g) g$meta[[arg]]))
    if (!is.null(v)) dots[[arg]] <- v
  }
  # hakn is `method.random.ci` on the fitted object; run_ma() takes the flag.
  hk <- uniform(lapply(outcomes, function(g) {
    identical(g$meta$method.random.ci %||% "classic", "HK")
  }))
  if (!is.null(hk)) dots$hakn <- hk
  # No arm labels here on purpose: they are the one setting that cannot be
  # left to a set-wide default, so the exported data names the arms instead
  # (pma_name_arms()).
  dots
}

# grade_meta() arguments for one outcome, as the bundled multi-outcome
# analysis.R renders them. The single-outcome template recovers the arguments
# it is not given from the rated object; the multi-outcome one renders the
# grade_meta_multi(per_outcome = ) list and nothing else, so an argument
# missing here comes back as grade_meta()'s default - and for threshold_type
# that is not a different rating but an abort on the Core GRADE 2 entry gate.
pma_outcome_grade_args <- function(g) {
  # Exact lookups throughout: `$` partial-matches, and `threshold` is a prefix
  # of four other fields of a rated object, so `g$threshold` on an outcome
  # rated without a MID answers with whichever of them is unambiguous.
  field <- function(nm) g[[nm, exact = TRUE]]
  recovered <- list(
    study_design   = field("study_design"),
    outcome_type   = field("outcome_type"),
    threshold_type = field("threshold_type"),
    follow_up      = field("follow_up"),
    unit           = field("unit")
  )
  # The reviewer rated without a MID on purpose; re-running under the gate's
  # default would abort instead of reproducing that decision.
  if (identical(field("threshold_type"), "mid") &&
      is.null(field("threshold"))) {
    recovered$require_threshold <- FALSE
  }
  # The low-RoB refit was offered and declined: without this the script refits
  # and reports pooled numbers the rating was not made on.
  if (identical(field("rob_analysis_set"), "low_only") &&
      !isTRUE(field("rob_refit"))) {
    recovered$rob_refit <- FALSE
  }
  if (!is.null(field("baseline_risk"))) {
    recovered$baseline_risk <- field("baseline_risk")
  }
  # A rare-event rating re-run without these three is a DIFFERENT rating on the
  # same data: the OIS goes back to a participant basis, Inconsistency reads an
  # I2 the analysis withdrew, and the publication-bias wizard takes the Egger
  # branch. grade_meta() records them on the object as $rare, so they can be
  # recovered here for a set assembled out of banked outcomes.
  rare <- field("rare")
  if (is.list(rare) && isTRUE(rare$flow)) {
    recovered$rare_flow <- TRUE
    recovered$rare_one_arm_total_zero <- isTRUE(rare$one_arm_total_zero)
    if (is.character(rare$method) && length(rare$method) == 1L) {
      recovered$rare_method <- rare$method
    }
  }
  recovered <- recovered[!vapply(recovered, is.null, logical(1))]
  # The specs win: they are the arguments the app actually passed, recorded
  # beside the grade_meta() call that used them.
  utils::modifyList(recovered,
                    attr(g, PMA_GRADE_ARGS_ATTR, exact = TRUE) %||% list())
}

# The pmatools_set the ZIP is built from. Always a set, even for one outcome:
# the app ships one bundle layout, so a single-outcome download has the same
# shape as a three-outcome one and a reader learns it once.
pma_export_set <- function(outcomes, primary = character(0)) {
  outcomes <- pma_outcomes_list(outcomes)
  # Not-reported rows travel in `outcomes` and `order` - they are rows of the
  # Summary of Findings and get their own numbered directory - but a bundle of
  # nothing but them has no analysis, no data and no script to write, and the
  # combined table would have no effect measure to head its columns.
  if (length(pma_rated_outcomes(outcomes)) == 0L) {
    stop("pma_export_set: at least one rated outcome is required")
  }
  # Rated only, for the same reason as pma_set_ma_args(): these become the
  # grade_meta_multi(per_outcome = ) list, and grade_meta() is never called for
  # an outcome nobody reported.
  grade_args <- lapply(pma_rated_outcomes(outcomes), pma_outcome_grade_args)
  .new_pmatools_set(
    outcomes    = outcomes,
    order       = names(outcomes),
    primary     = intersect(as.character(primary %||% character(0)),
                            names(outcomes)),
    data        = pma_export_data(outcomes),
    ma_args     = pma_set_ma_args(outcomes),
    # Every argument is per-outcome: two outcomes rated in separate passes
    # share no argument by construction, so there is nothing `common` could
    # hold that would not be a guess.
    common      = list(),
    per_outcome = grade_args,
    grade_args  = grade_args
  )
}

# Normalizes whatever is in state$outcomes into a valid named list.
#
# Two classes get through, not one. A row the reviewer declared not reported
# (Core GRADE 6) is a `pmatools_not_reported`, which deliberately does NOT
# inherit "pmatools" so that no consumer of a rated object can be handed one by
# accident (see R/not_reported.R). Every filter it meets therefore has to name
# it, and this is the first: while it did not, an outcome added as not reported
# vanished from state$outcomes the moment anything read the list back.
pma_outcomes_list <- function(outcomes) {
  if (is.null(outcomes) || !is.list(outcomes) || length(outcomes) == 0) {
    return(list())
  }
  keep <- vapply(outcomes, function(g) {
    inherits(g, "pmatools") || .is_not_reported(g)
  }, logical(1))
  outcomes[keep]
}

# The rated subset of a saved-outcome list: everything the app can pool, plot
# or re-run. `.rated_outcomes()` is pmatools'; this wrapper exists so callers
# read one vocabulary (`pma_outcomes_list()` then `pma_rated_outcomes()`) and
# so the normalisation cannot be forgotten in front of it.
pma_rated_outcomes <- function(outcomes) {
  .rated_outcomes(pma_outcomes_list(outcomes))
}

# Clock time a row was last written, as the reviewer reads it. Empty string
# when the object carries no stamp, so the caller can paste it unconditionally.
pma_outcome_updated_label <- function(g) {
  ts <- attr(g, PMA_SAVED_AT_ATTR, exact = TRUE)
  if (is.null(ts) || length(ts) != 1L || is.na(ts)) return("")
  time_label <- tryCatch(format(as.POSIXct(ts), "%H:%M"), error = function(e) "")
  if (is.na(time_label)) "" else time_label
}

# One-row-per-outcome summary used by the saved-outcome list UI.
# `signature` is the signature of the dataset currently loaded; when given,
# the `stale` column marks outcomes saved from a different dataset.
pma_outcome_summary_df <- function(outcomes, signature = NULL) {
  outcomes <- pma_outcomes_list(outcomes)
  if (length(outcomes) == 0) {
    return(data.frame(name = character(0), k = character(0),
                      effect = character(0), certainty = character(0),
                      updated = character(0), stale = logical(0),
                      not_reported = logical(0),
                      stringsAsFactors = FALSE))
  }
  # A not-reported row has no analysis behind it, so k / effect / certainty
  # would all read "-" and look like a rated row whose numbers failed to
  # compute. It says what it is instead, in the one line the list shows.
  data.frame(
    name = names(outcomes),
    k = vapply(outcomes, function(g) {
      if (.is_not_reported(g)) return("0")
      k <- g$meta$k %||% NA_integer_
      if (is.na(k)) "-" else as.character(k)
    }, character(1)),
    effect = vapply(outcomes, function(g) {
      if (.is_not_reported(g)) return(.not_reported_label(g))
      # format_effect() is pmatools public API as of 0.5.0 (the dot-prefixed
      # .format_effect() is only a back-compat alias) -- keep the public name.
      effect_text <- tryCatch(format_effect(g$meta, g$outcome_type),
                              error = function(e) NA_character_)
      if (is.null(effect_text) || is.na(effect_text)) "-" else gsub("\n", "; ", effect_text)
    }, character(1)),
    certainty = vapply(outcomes, function(g) {
      if (.is_not_reported(g)) return(NOT_REPORTED_CERTAINTY)
      g$certainty %||% "-"
    }, character(1)),
    updated = vapply(outcomes, pma_outcome_updated_label, character(1)),
    stale = unname(pma_outcomes_stale(outcomes, signature)),
    not_reported = vapply(outcomes, .is_not_reported, logical(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# Saved-outcome list with per-row Move up / Move down, Primary and Remove
# controls. Every button writes the outcome name to a single input id via
# Shiny.setInputValue rather than creating one observer per row, so rows can
# come and go freely. The move and primary payloads carry a nonce because two
# consecutive clicks on the same row would otherwise send an identical value
# and the observer would not fire the second time.
pma_saved_outcomes_ui <- function(outcomes, delete_input_id = "outcome_delete",
                                  empty_text = NULL, signature = NULL,
                                  move_input_id = "outcome_move",
                                  primary_input_id = "outcome_primary",
                                  primary = character(0)) {
  summary_rows <- pma_outcome_summary_df(outcomes, signature = signature)
  if (nrow(summary_rows) == 0) {
    if (is.null(empty_text)) return(NULL)
    return(htmltools::p(class = "pma-card-subtitle", empty_text))
  }
  primary <- as.character(primary %||% character(0))
  .name_lit <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
  .row_button <- function(input_id, name, extra = NULL, label, title,
                          disabled = FALSE) {
    payload <- c(list(name = name), extra, list(nonce = 0))
    js <- sprintf(
      "Shiny.setInputValue('%s', Object.assign(%s, {nonce: Math.random()}), {priority: 'event'})",
      input_id, jsonlite::toJSON(payload, auto_unbox = TRUE))
    htmltools::tags$button(
      type  = "button",
      class = "btn btn-secondary",
      title = title,
      disabled = if (isTRUE(disabled)) NA else NULL,
      style = paste("padding: 0.2rem 0.5rem; font-size: 0.8rem;",
                    if (isTRUE(disabled)) "opacity: 0.4;" else ""),
      onclick = if (isTRUE(disabled)) NULL else js,
      label)
  }
  rows <- lapply(seq_len(nrow(summary_rows)), function(i) {
    is_primary <- summary_rows$name[i] %in% primary
    htmltools::div(
      style = paste(
        "display: flex; align-items: center; gap: 0.75rem;",
        "padding: 0.5rem 0.25rem;",
        "border-top: 1px solid hsl(var(--border));"),
      # Row order is a statement about priority in a Summary of Findings
      # table, so it is under the reviewer's control rather than fixed to the
      # order the outcomes happened to be saved in.
      htmltools::div(
        style = "display: flex; flex-direction: column; gap: 0.15rem;",
        .row_button(move_input_id, summary_rows$name[i], list(dir = "up"),
                    label = htmltools::HTML("&#9650;"),
                    title = "Move up", disabled = i == 1L),
        .row_button(move_input_id, summary_rows$name[i], list(dir = "down"),
                    label = htmltools::HTML("&#9660;"),
                    title = "Move down", disabled = i == nrow(summary_rows))
      ),
      htmltools::div(
        style = "flex: 1 1 auto; min-width: 0;",
        htmltools::div(style = "font-weight: 600;", summary_rows$name[i]),
        htmltools::div(
          style = "font-size: 0.8rem; color: hsl(var(--muted-foreground));",
          # "last updated", not "saved at": nobody presses Save, so the stamp
          # says when the row was last recomputed (shiny/SPEC.md 3.4.14).
          sprintf("k = %s | %s%s", summary_rows$k[i], summary_rows$effect[i],
                  if (nzchar(summary_rows$updated[i]))
                    paste0(" | last updated ", summary_rows$updated[i]) else "")),
        if (isTRUE(summary_rows$stale[i])) htmltools::div(
          style = sprintf("font-size: 0.78rem; margin-top: 0.15rem; color: %s;",
                          PMA_ALERT_FG),
          "Saved from a dataset other than the one currently loaded."
        ) else NULL
      ),
      if (isTRUE(summary_rows$stale[i])) htmltools::div(pma_stale_badge()) else NULL,
      htmltools::div(pma_certainty_badge(summary_rows$certainty[i])),
      htmltools::tags$button(
        type  = "button",
        class = if (is_primary) "btn btn-primary" else "btn btn-secondary",
        title = paste0("Outcomes marked primary are grouped under a ",
                       "\"Primary outcomes\" heading in the table."),
        style = "padding: 0.2rem 0.6rem; font-size: 0.8rem;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {name: %s, nonce: Math.random()}, {priority: 'event'})",
          primary_input_id, .name_lit(summary_rows$name[i])),
        if (is_primary) "Primary" else "Mark primary"),
      htmltools::tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = "padding: 0.2rem 0.6rem; font-size: 0.8rem;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %s, {priority: 'event'})",
          delete_input_id, .name_lit(summary_rows$name[i])),
        "Remove")
    )
  })
  htmltools::div(style = "margin-top: 0.5rem;", rows)
}

# "+ Add next outcome", at the bottom of Step 4's combined Summary of Findings
# card: straight from the finished table to the next row of it. It used to be
# rendered at the end of Step 3's Final certainty tab as well, which is why it
# writes to its input id via Shiny.setInputValue rather than being an
# actionButton - two actionButtons cannot share an id, and the two copies had
# to drive one observer. There is one call site now, but the mechanism stays:
# an actionButton would make the id an assumption about there being exactly
# one, and this button is the kind that gets rendered twice again.
pma_add_next_outcome_button <- function(input_id = "add_next_outcome",
                                        style = NULL) {
  htmltools::tags$button(
    type  = "button",
    class = "btn btn-secondary",
    style = paste("margin-top: 0.5rem;", style),
    onclick = sprintf(
      "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
      input_id),
    "+ Add next outcome")
}

# ----- Adding an outcome nobody reported ----------------------------------
# Core GRADE 6 asks the Summary of Findings table to cover every outcome the
# review prespecified, including the ones the evidence base turns out to be
# silent on. Those have no data to map and no analysis to run, so "+ Add next
# outcome" asks which kind is being added before it clears Step 2.
#
# The three ids below are written from hand-written JavaScript in the modals
# and read by observers in app.R. Named constants because a typo on either
# side is silent: nothing happens and there is no error to find.
PMA_ADD_OUTCOME_ANALYSE_ID      <- "add_outcome_analyse"
PMA_ADD_OUTCOME_NOT_REPORTED_ID <- "add_outcome_not_reported"
PMA_NOT_REPORTED_SAVE_ID        <- "not_reported_save"

# Full-width choice button. Shiny.setInputValue with priority "event" rather
# than an actionButton for the reason pma_add_next_outcome_button() gives: a
# modal is destroyed and rebuilt on every showing, and a rebuilt actionButton
# reports 0 before it reports 1, which every observer then has to guard.
.pma_choice_button <- function(input_id, title, detail) {
  htmltools::tags$button(
    type  = "button",
    class = "btn btn-secondary",
    style = paste("display: block; width: 100%; text-align: left;",
                  "padding: 0.75rem 1rem; margin-bottom: 0.5rem;",
                  "white-space: normal;"),
    onclick = sprintf(
      "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
      input_id),
    htmltools::div(style = "font-weight: 600;", title),
    htmltools::div(
      style = paste("font-size: 0.82rem; font-weight: 400; margin-top: 0.2rem;",
                    "color: hsl(var(--muted-foreground));"),
      detail))
}

pma_add_outcome_choice_modal <- function() {
  shiny::modalDialog(
    title     = "Add the next outcome",
    easyClose = TRUE,
    htmltools::p(
      class = "pma-card-subtitle",
      "A Summary of Findings table covers every outcome the review ",
      "prespecified, including those no included study reported ",
      "(Core GRADE 6)."),
    .pma_choice_button(
      PMA_ADD_OUTCOME_ANALYSE_ID,
      "Analyse it from the data",
      paste0("Returns to Step 2 with the outcome name, direction, follow-up ",
             "and every Step 3 answer cleared, ready for the next analysis.")),
    .pma_choice_button(
      PMA_ADD_OUTCOME_NOT_REPORTED_ID,
      "Record it as not reported",
      paste0("Adds a row saying the review looked for this outcome and found ",
             "no usable data. No analysis, no effect estimate, no certainty ",
             "rating.")),
    footer = shiny::modalButton("Cancel"))
}

pma_not_reported_modal <- function() {
  shiny::modalDialog(
    title     = "Outcome not reported by any included study",
    easyClose = TRUE,
    shiny::textInput("not_reported_name", "Outcome name", width = "100%",
                     placeholder = "e.g. Quality of life"),
    shiny::textInput("not_reported_follow_up", "Follow-up (optional)",
                     width = "100%", placeholder = "e.g. 12 months"),
    shiny::textAreaInput(
      "not_reported_reason", "Why nothing was reported (optional)",
      width = "100%", rows = 2,
      placeholder = paste0("Prespecified in the protocol; no included trial ",
                           "measured it.")),
    htmltools::p(
      class = "pma-card-subtitle",
      "The reason becomes a numbered footnote on the row."),
    footer = htmltools::tagList(
      shiny::modalButton("Cancel"),
      htmltools::tags$button(
        type    = "button",
        class   = "btn btn-primary",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
          PMA_NOT_REPORTED_SAVE_ID),
        "Add to the table")))
}

# Validate the "not reported" form and build the row it describes. Returns
# list(ok = TRUE, name = , outcome = ) or list(ok = FALSE, message = ).
#
# Pure, and separate from the observer that calls it, because these are the
# rules a reviewer actually meets and a Shiny observer is the one place this
# app cannot test. `existing` is names(state$outcomes).
pma_not_reported_entry <- function(name, follow_up = NULL, reason = NULL,
                                   existing = character(0)) {
  clean <- function(v) {
    v <- trimws(as.character(v %||% "")[1L])
    if (is.na(v) || !nzchar(v)) NULL else v
  }
  name <- clean(name)
  if (is.null(name)) {
    return(list(ok = FALSE, message = paste0(
      "Name the outcome. Every row of the table, and every message about it, ",
      "is keyed on its name.")))
  }
  if (name %in% as.character(existing %||% character(0))) {
    # Same rule, and near enough the same words, as add_not_reported()'s own
    # collision check - reached earlier, so the reviewer sees it in the form
    # rather than as an abort from the table builder.
    return(list(ok = FALSE, message = sprintf(paste0(
      "There is already a saved outcome named \"%s\". An outcome is either ",
      "rated or not reported, not both."), name)))
  }
  list(ok      = TRUE,
       name    = name,
       outcome = not_reported_outcome(name,
                                      follow_up = clean(follow_up),
                                      reason    = clean(reason)))
}
