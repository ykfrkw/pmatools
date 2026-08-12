# ui_helpers.R - shadcn-style component helpers (HTML wrappers)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Card with optional title and subtitle
pma_card <- function(..., title = NULL, subtitle = NULL, id = NULL) {
  htmltools::div(
    class = "pma-card", id = id,
    if (!is.null(title)) htmltools::div(class = "pma-card-header", title) else NULL,
    if (!is.null(subtitle)) htmltools::div(class = "pma-card-subtitle", subtitle) else NULL,
    ...
  )
}

# Step header with title + "What this step does"
pma_step_header <- function(title, what, why = NULL) {
  htmltools::div(
    class = "pma-step-header",
    htmltools::h2(title, style = "margin-top: 0;"),
    htmltools::p(class = "pma-card-subtitle", what),
    if (!is.null(why)) htmltools::p(class = "pma-why", why) else NULL
  )
}

# Inline tooltip help (?) icon
pma_help <- function(text) {
  htmltools::tags$span(
    class = "pma-help",
    title = text,
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = "top",
    "(?)"
  )
}

# Stepper (4 steps) - each step is a clickable actionLink
pma_stepper <- function(current_step) {
  steps <- c("Data", "Meta-analysis", "Certainty", "Export")
  htmltools::div(
    class = "pma-stepper",
    lapply(seq_along(steps), function(i) {
      cls <- if (i == current_step) "pma-step current"
             else if (i < current_step) "pma-step done"
             else "pma-step"
      htmltools::tagList(
        shiny::actionLink(
          inputId = paste0("step_jump_", i),
          label   = htmltools::tagList(
            htmltools::span(class = "num", i),
            htmltools::span(steps[i])
          ),
          class = cls,
          style = "text-decoration: none; cursor: pointer;"
        ),
        if (i < length(steps)) htmltools::span(class = "sep", " > ") else NULL
      )
    })
  )
}

# ----- W4-A output gate: shared confirmation-domain labels -----
# Named after the keys of the state$domain_confirmed logical vector set in
# step3_server(); used by both Step 3 (banner/badge) and Step 4 (export gate).
PMA_DOMAIN_LABELS <- c(
  threshold     = "Decision threshold",
  rob           = "Risk of Bias",
  inconsistency = "Inconsistency",
  indirectness  = "Indirectness",
  imprecision   = "Imprecision",
  pubias        = "Publication bias"
)

# Human-readable labels of the domains not yet confirmed. `conf` is the
# named logical vector from state$domain_confirmed (NULL = nothing
# confirmed yet, e.g. before Step 3 was opened).
pma_unconfirmed_domains <- function(conf) {
  keys <- names(PMA_DOMAIN_LABELS)
  if (is.null(conf)) return(unname(PMA_DOMAIN_LABELS))
  ok <- vapply(keys, function(k) {
    k %in% names(conf) && isTRUE(conf[[k]])
  }, logical(1))
  unname(PMA_DOMAIN_LABELS[keys[!ok]])
}

# ----- Saved outcomes (multi-outcome Summary of Findings) -----------------
# state$outcomes is a NAMED LIST of pmatools objects (the value of
# state$grade at the moment the user pressed "Save"), keyed by outcome
# label, in insertion order. It is exactly the shape grade_table() expects,
# so it can be passed straight through without reshaping. The save time is
# carried as attr(<obj>, "pma_saved_at") because attributes survive both the
# list round-trip and grade_table()'s inherits() check.

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

# Alert colours follow the existing warning treatment in Step 3
# (output$cert_incomplete_banner): amber #fef3c7 / #b45309, reserved for
# genuine alerts.
PMA_ALERT_BG <- "#fef3c7"
PMA_ALERT_FG <- "#b45309"

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

# Normalizes whatever is in state$outcomes into a valid named list.
pma_outcomes_list <- function(outcomes) {
  if (is.null(outcomes) || !is.list(outcomes) || length(outcomes) == 0) {
    return(list())
  }
  keep <- vapply(outcomes, inherits, logical(1), "pmatools")
  outcomes[keep]
}

# One-row-per-outcome summary used by the saved-outcome list UI.
# `signature` is the signature of the dataset currently loaded; when given,
# the `stale` column marks outcomes saved from a different dataset.
pma_outcome_summary_df <- function(outcomes, signature = NULL) {
  outcomes <- pma_outcomes_list(outcomes)
  if (length(outcomes) == 0) {
    return(data.frame(name = character(0), k = character(0),
                      effect = character(0), certainty = character(0),
                      stale = logical(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    name = names(outcomes),
    k = vapply(outcomes, function(g) {
      k <- g$meta$k %||% NA_integer_
      if (is.na(k)) "-" else as.character(k)
    }, character(1)),
    effect = vapply(outcomes, function(g) {
      out <- tryCatch(.format_effect(g$meta, g$outcome_type), error = function(e) NA_character_)
      if (is.null(out) || is.na(out)) "-" else gsub("\n", "; ", out)
    }, character(1)),
    certainty = vapply(outcomes, function(g) g$certainty %||% "-", character(1)),
    stale = unname(pma_outcomes_stale(outcomes, signature)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# Saved-outcome list with a per-row Remove button. The buttons write the
# outcome name to `delete_input_id` via Shiny.setInputValue rather than
# creating one observer per row, so rows can come and go freely.
pma_saved_outcomes_ui <- function(outcomes, delete_input_id = "outcome_delete",
                                  empty_text = NULL, signature = NULL) {
  df <- pma_outcome_summary_df(outcomes, signature = signature)
  if (nrow(df) == 0) {
    if (is.null(empty_text)) return(NULL)
    return(htmltools::p(class = "pma-card-subtitle", empty_text))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    htmltools::div(
      style = paste(
        "display: flex; align-items: center; gap: 0.75rem;",
        "padding: 0.5rem 0.25rem;",
        "border-top: 1px solid hsl(var(--border));"),
      htmltools::div(
        style = "flex: 1 1 auto; min-width: 0;",
        htmltools::div(style = "font-weight: 600;", df$name[i]),
        htmltools::div(
          style = "font-size: 0.8rem; color: hsl(var(--muted-foreground));",
          sprintf("k = %s | %s", df$k[i], df$effect[i])),
        if (isTRUE(df$stale[i])) htmltools::div(
          style = sprintf("font-size: 0.78rem; margin-top: 0.15rem; color: %s;",
                          PMA_ALERT_FG),
          "Saved from a dataset other than the one currently loaded."
        ) else NULL
      ),
      if (isTRUE(df$stale[i])) htmltools::div(pma_stale_badge()) else NULL,
      htmltools::div(pma_certainty_badge(df$certainty[i])),
      htmltools::tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = "padding: 0.2rem 0.6rem; font-size: 0.8rem;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %s, {priority: 'event'})",
          delete_input_id,
          jsonlite::toJSON(df$name[i], auto_unbox = TRUE)),
        "Remove")
    )
  })
  htmltools::div(style = "margin-top: 0.5rem;", rows)
}

# GRADE certainty badge
pma_certainty_badge <- function(label) {
  cls <- switch(tolower(label),
    "high"     = "grade-high",
    "moderate" = "grade-moderate",
    "low"      = "grade-low",
    "very low" = "grade-vlow",
    "grade-low"
  )
  symbol <- switch(label,
    "High"     = htmltools::HTML("&#8853;&#8853;&#8853;&#8853;"),
    "Moderate" = htmltools::HTML("&#8853;&#8853;&#8853;&#9675;"),
    "Low"      = htmltools::HTML("&#8853;&#8853;&#9675;&#9675;"),
    "Very Low" = htmltools::HTML("&#8853;&#9675;&#9675;&#9675;"),
    htmltools::HTML("")
  )
  htmltools::span(class = paste("pma-badge", cls),
                  htmltools::HTML(paste0(label, " ", as.character(symbol))))
}

# Downgrade chip - shows -0/-1/-2 next to judgment badge
pma_downgrade_chip <- function(judgment) {
  j  <- judgment %||% "no"
  # 3-level mapping (v0.3+); legacy "some"/"very_serious" still mapped.
  dg <- c(no = 0, some = -1, some_concerns = -1,
          serious = -2, very_serious = -2)[[j]]
  if (is.null(dg) || is.na(dg)) dg <- 0
  cls <- if (dg == 0) "grade-high"
         else if (dg == -1) "grade-low"
         else "grade-vlow"
  label <- if (dg == 0) "+0" else as.character(dg)
  htmltools::span(class = paste("pma-badge pma-chip", cls), label)
}

# Generic judgment badge
pma_judgment_badge <- function(judgment) {
  cls <- switch(judgment,
    "no"            = "grade-high",
    "some"          = "grade-low",
    "some_concerns" = "grade-low",
    "serious"       = "grade-vlow",
    "very_serious"  = "grade-vlow",
    "grade-low"
  )
  label <- switch(judgment,
    "no"            = "No concern",
    "some"          = "Some concerns",
    "some_concerns" = "Some concerns",
    "serious"       = "Serious",
    "very_serious"  = "Serious",
    judgment
  )
  htmltools::span(class = paste("pma-badge", cls), label)
}

# Render a base R plot to a temp PNG and trim white margins via {magick}.
# Returns a list compatible with shiny::renderImage().
pma_render_trimmed <- function(plot_fn,
                               width    = 1400,
                               height   = 2400,
                               res      = 130,
                               fuzz     = 3,
                               bg       = "white") {
  png_path <- tempfile(fileext = ".png")
  grDevices::png(png_path, width = width, height = height,
                 res = res, bg = bg)
  ok <- tryCatch({ plot_fn(); TRUE },
                 error = function(e) FALSE)
  grDevices::dev.off()

  if (!isTRUE(ok)) {
    return(list(src = png_path, contentType = "image/png",
                width = "100%"))
  }

  if (requireNamespace("magick", quietly = TRUE)) {
    img <- tryCatch({
      i <- magick::image_read(png_path)
      i <- magick::image_trim(i, fuzz = fuzz)
      i <- magick::image_border(i, color = bg, geometry = "30x30")
      magick::image_write(i, png_path)
      TRUE
    }, error = function(e) FALSE)
  }

  list(src = png_path, contentType = "image/png", width = "100%")
}

# Funnel plot display panel — sizing controls for the renderImage canvas
# plus optional xlim and Egger-annotation toggle. Shared between Step2
# (Meta-analysis tab) and Step3 (Publication bias tab). `include_egger =
# FALSE` for the trim-and-fill funnel where Egger does not apply.
pma_funnel_display_panel <- function(prefix, include_egger = TRUE) {
  htmltools::tags$details(
    style = "margin-top: 0.5rem;",
    htmltools::tags$summary("Funnel plot display"),
    htmltools::div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(4, minmax(140px, 1fr));",
        "gap: 0.75rem 1rem;",
        "padding: 0.75rem 0.25rem 0.25rem;"),
      shiny::numericInput(paste0(prefix, "_funnel_width"),
                          "Width (px)",  value = 1400,
                          min = 400, step = 100, width = "100%"),
      shiny::numericInput(paste0(prefix, "_funnel_height"),
                          "Height (px)", value = 1400,
                          min = 400, step = 100, width = "100%"),
      shiny::numericInput(paste0(prefix, "_funnel_xlim_lo"),
                          "x-min", value = NA, width = "100%"),
      shiny::numericInput(paste0(prefix, "_funnel_xlim_hi"),
                          "x-max", value = NA, width = "100%"),
      if (isTRUE(include_egger)) htmltools::div(
        style = "grid-column: span 4;",
        shiny::checkboxInput(paste0(prefix, "_funnel_show_egger"),
                             "Show Egger annotation", TRUE)
      ) else NULL
    )
  )
}

# Server-side helper to collect funnel display inputs into a list.
pma_funnel_display_args <- function(input, prefix, include_egger = TRUE) {
  lo <- input[[paste0(prefix, "_funnel_xlim_lo")]]
  hi <- input[[paste0(prefix, "_funnel_xlim_hi")]]
  xlim <- if (!is.null(lo) && !is.null(hi) &&
              !is.na(lo) && !is.na(hi) && lo < hi) c(lo, hi) else NULL
  w  <- input[[paste0(prefix, "_funnel_width")]]
  h  <- input[[paste0(prefix, "_funnel_height")]]
  list(
    width      = if (is.numeric(w) && !is.na(w) && w > 0) as.integer(w) else 1400L,
    height     = if (is.numeric(h) && !is.na(h) && h > 0) as.integer(h) else 1400L,
    xlim       = xlim,
    show_egger = if (isTRUE(include_egger))
                   isTRUE(input[[paste0(prefix, "_funnel_show_egger")]])
                 else NA
  )
}

# ---------------------------------------------------------------------------
# Forest plot display: smart defaults
#
# Every forest panel in the app (Step 2 and the four Step 3 domain tabs) has
# the same five text fields plus two "blank rows" numerics. The helpers below
# (a) name those input ids in one place, (b) derive what the app WOULD fill in
# from the outcome definition, and (c) prefill them without ever clobbering a
# value the user typed.
# ---------------------------------------------------------------------------

# Input ids of the five prefillable text fields of a forest display panel.
# `prefix = NULL` (or "") means the Step 2 panel, whose ids carry no prefix.
pma_forest_label_ids <- function(prefix = NULL) {
  p <- prefix %||% ""
  if (length(p) != 1 || is.na(p) || !nzchar(p)) {
    return(c(title        = "forest_title",
             label_e      = "label_e",
             label_c      = "label_c",
             favors_left  = "favors_left",
             favors_right = "favors_right"))
  }
  c(title        = paste0(p, "_title"),
    label_e      = paste0(p, "_label_e"),
    label_c      = paste0(p, "_label_c"),
    favors_left  = paste0(p, "_favors_left"),
    favors_right = paste0(p, "_favors_right"))
}

# Input ids of the two "blank rows around the pooled result" numerics.
# Step 2 and Step 3 disagree on the suffix, so never build these by hand.
pma_forest_addrow_ids <- function(prefix = NULL) {
  p <- prefix %||% ""
  if (length(p) != 1 || is.na(p) || !nzchar(p)) {
    return(c(above = "addrows_above_overall",
             below = "addrows_below_overall"))
  }
  c(above = paste0(p, "_addrows_above"),
    below = paste0(p, "_addrows_below"))
}

# Derive the "Favors ..." axis labels from the outcome direction.
#
# `small_values` is the pmatools vocabulary set in Step 2:
#   "undesirable" - a smaller value is worse (response, remission), so a
#                   larger effect favours the intervention: right = intervention.
#   "desirable"   - a smaller value is better (mortality, symptom score), so
#                   the sides are mirrored.
# Anything else (not yet chosen, unrecognised) or a missing arm name yields
# empty strings: better no prefill than a wrong direction on the axis.
pma_favors_labels <- function(small_values, intervention, control) {
  .clean <- function(x) {
    if (is.null(x) || length(x) != 1 || is.na(x)) return("")
    trimws(as.character(x))
  }
  sv <- .clean(small_values)
  iv <- .clean(intervention)
  ct <- .clean(control)
  none <- list(left = "", right = "")
  if (!nzchar(iv) || !nzchar(ct)) return(none)
  if (identical(sv, "undesirable")) {
    list(left = paste("Favors", ct), right = paste("Favors", iv))
  } else if (identical(sv, "desirable")) {
    list(left = paste("Favors", iv), right = paste("Favors", ct))
  } else {
    none
  }
}

# Coerce the "blank rows below the pooled result" input. Blank / NA / invalid
# means "let plot_forest() decide" (its .auto_addrow_below() heuristic), which
# is expressed as NULL.
pma_addrow_below <- function(x) {
  if (is.null(x) || length(x) != 1) return(NULL)
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x) || x < 0) return(NULL)
  x
}

# Same for "blank rows above the pooled result", where there is no auto mode;
# a blank field falls back to the historical default of one row.
pma_addrow_above <- function(x, default = 1) {
  if (is.null(x) || length(x) != 1) return(default)
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x) || x < 0) return(default)
  x
}

# Prefill a textInput without ever overwriting something the user typed.
#
# Design notes (this generalises the former Step 3 `.auto_name` observer):
#  * `shiny::observe()`, not `observeEvent()`: the field must also be filled
#    the first time the input appears, i.e. when the step body is rendered.
#  * `mem` is a plain environment, NOT a reactiveVal: it records the last value
#    this observer wrote, and a reactiveVal would make our own write re-trigger
#    the observer.
#  * The field is only updated when its current value is one we could have put
#    there ourselves - empty, an explicitly whitelisted `extra_auto` value, or
#    our own previous write. Anything else is the user's text and is left alone.
pma_autofill_text <- function(input, session, input_id, expected_fn,
                              extra_auto = character(0)) {
  mem <- new.env(parent = emptyenv())
  mem$last <- NULL
  shiny::observe({
    cur <- input[[input_id]]
    if (is.null(cur)) return()
    expected <- tryCatch(expected_fn(), error = function(e) NULL)
    if (is.null(expected) || length(expected) != 1 || is.na(expected)) return()
    expected <- as.character(expected)
    auto_filled <- unique(c("", extra_auto, mem$last))
    if (cur %in% auto_filled && !identical(cur, expected)) {
      mem$last <- expected
      shiny::updateTextInput(session, input_id, value = expected)
    }
  })
  invisible(NULL)
}

# Wire all five prefillable text fields of one forest display panel.
#
# `values_fn` is a reactive (or plain function) returning a list with the names
# title / label_e / label_c / favors_left / favors_right. `title_suffix` is
# appended to a non-empty title only, so panels that plot a stratified version
# of the outcome carry the same title the export bundle writes.
pma_autofill_forest_panel <- function(input, session, prefix = NULL,
                                      values_fn, title_suffix = "") {
  ids <- pma_forest_label_ids(prefix)
  for (key in names(ids)) {
    # local() is required: without it every closure below would capture the
    # same loop variable and all five observers would end up watching the last
    # key only.
    local({
      k  <- key
      id <- ids[[k]]
      sfx <- if (identical(k, "title")) title_suffix else ""
      pma_autofill_text(
        input, session, id,
        expected_fn = function() {
          vals <- values_fn()
          v <- vals[[k]] %||% ""
          if (length(v) != 1 || is.na(v)) v <- ""
          v <- as.character(v)
          if (nzchar(v) && nzchar(sfx)) paste0(v, sfx) else v
        }
      )
    })
  }
  invisible(NULL)
}

# Banner (used for Indirectness review reminder)
pma_banner <- function(text) {
  htmltools::div(class = "pma-banner", text)
}

# Reference line with optional DOI link.
# Call as pma_reference(text, doi) or pma_reference(text) for plain text.
pma_reference <- function(text, doi = NULL) {
  ref_content <- if (!is.null(doi) && nzchar(doi)) {
    htmltools::tags$a(
      href = paste0("https://doi.org/", doi),
      target = "_blank",
      text
    )
  } else text
  htmltools::p(class = "pma-reference",
    style = "font-style: italic; color: hsl(var(--muted-foreground)); font-size: 0.85rem;",
    "Reference: ", ref_content
  )
}

# Collapsible "How is this judged?" block — clickable to expand long
# educational copy.
pma_how_collapse <- function(body) {
  htmltools::tags$details(
    class = "pma-how-details",
    htmltools::tags$summary("How is this judged? (click to expand)"),
    htmltools::div(
      class = "pma-how-body",
      style = "margin-top: 0.6rem; line-height: 1.55; font-size: 0.9rem;",
      htmltools::p(body)
    )
  )
}

# Wizard navigation buttons (Back / Next).
# Use HTML entities to dodge Latin-1 encoding issues on shinyapps.io build.
pma_wizard_nav <- function(current_step, max_step = 4,
                           back_id = "btn_back", next_id = "btn_next",
                           next_label = NULL,
                           next_disabled = FALSE) {
  arrow_left  <- htmltools::HTML("&#8592;")  # left arrow
  arrow_right <- htmltools::HTML("&#8594;")  # right arrow
  next_label  <- next_label %||% htmltools::tagList("Next ", arrow_right)
  back_label  <- htmltools::tagList(arrow_left, " Back")
  htmltools::div(
    style = "display: flex; justify-content: space-between; margin-top: 1.5rem;",
    if (current_step > 1) {
      shiny::actionButton(back_id, back_label, class = "btn btn-secondary")
    } else htmltools::div(),
    if (current_step < max_step) {
      shiny::actionButton(next_id, next_label,
                          class = "btn btn-primary",
                          disabled = if (next_disabled) "disabled" else NULL)
    } else htmltools::div()
  )
}
