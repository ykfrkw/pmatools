# plot_panels.R - the display controls under every forest and funnel plot
#
# Split out of ui_helpers.R. Six surfaces draw a forest plot (Step 2 and the
# four rated Step 3 domain tabs, plus the trim-and-fill overlay) and each one
# carries the same panel of five text fields and two "blank rows" numerics. The
# panel is built once here, its input ids are named once here, and the values
# the app WOULD fill in are derived once here - so the Step 2 panel and the
# Imprecision panel cannot end up offering different controls for the same
# plot.
#
# The prefix argument is what makes one panel serve six: `prefix = NULL` means
# the Step 2 panel, whose ids carry no prefix, and any other value prepends
# itself. Ids are bare, never namespaced, for the same reason as everywhere
# else in this app - step2_ui() and step3_ui() spell them out and so does the
# suite.
#
# THE RULE FOR A NEW HELPER: it belongs here when it is about SHOWING a plot,
# not about computing one. Reading input values back off a panel
# (pma_funnel_display_args()) counts; deciding what to plot does not, and lives
# with the step that decides it. The autofill pair
# (pma_autofill_text() / pma_autofill_forest_panel()) is the one place here
# that touches `input` and `session`, because prefilling is by definition a
# message to a live widget - and it must never clobber a value the reviewer
# typed. Keep both properties.
#
# pma_render_trimmed() opens the file because it is display too: it renders a
# base R plot to a temp PNG and trims the white margins off it with {magick},
# which is the difference between a readable forest plot in an embedded iframe
# and a postage stamp adrift in white space.

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
      class = "pma-display-grid",
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
        class = "pma-span-4",
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

# The whole "Forest plot display" <details>, in one place.
#
# Step 2 and each of the four Step 3 domain tabs used to carry a hand-copied
# version of this panel; the two copies had already drifted apart in their id
# scheme (Step 2's `addrows_above_overall` against Step 3's
# `<prefix>_addrows_above`), which is exactly what pma_forest_addrow_ids()
# exists to absorb. Ids are therefore never built by hand here either: the five
# text fields come from pma_forest_label_ids() and the two blank-row numerics
# from pma_forest_addrow_ids(). Only x-min / x-max and the per-arm column
# toggle follow the plain `<prefix>_<name>` rule in both steps, so those three
# are derived locally.
#
# `prefix = NULL` (or "") builds the Step 2 panel, whose ids carry no prefix.
pma_forest_display_panel <- function(prefix = NULL) {
  labels  <- pma_forest_label_ids(prefix)
  addrows <- pma_forest_addrow_ids(prefix)
  p <- prefix %||% ""
  bare <- length(p) != 1 || is.na(p) || !nzchar(p)
  .id <- function(name) if (bare) name else paste0(p, "_", name)

  htmltools::tags$details(
    style = "margin-top: 0.5rem;",
    htmltools::tags$summary("Forest plot display"),
    htmltools::div(
      class = "pma-display-grid",
      # A textarea, not a textInput. plot_forest() honours a newline in the
      # title as an explicit line break (SPEC.md 4.3), and <input type="text">
      # cannot carry one: the HTML value sanitisation algorithm strips CR/LF,
      # so a break would be swallowed both when the user typed it and when the
      # autofill below pushed the stratified default in - joining the suffix
      # onto the outcome name with no separator at all. updateTextInput() and
      # updateTextAreaInput() send the same message, so pma_autofill_text()
      # drives this field unchanged.
      htmltools::div(
        class = "pma-span-4",
        shiny::textAreaInput(labels[["title"]], "Title (line breaks honoured)",
                             value = "", rows = 2, width = "100%")),

      shiny::textInput(labels[["label_e"]], "Intervention label", value = "", width = "100%"),
      shiny::textInput(labels[["label_c"]], "Control label",      value = "", width = "100%"),
      shiny::textInput(labels[["favors_left"]],  "Favors (left)",
                       placeholder = "e.g., Favors Control", width = "100%"),
      shiny::textInput(labels[["favors_right"]], "Favors (right)",
                       placeholder = "e.g., Favors CBT-I",   width = "100%"),

      # Two per row rather than four: a row holding only x-min and x-max left
      # the third and fourth columns empty, so the fields below it sat a column
      # out of step with the ones above. .pma-span-2 fills the row instead.
      htmltools::div(class = "pma-span-2",
        shiny::numericInput(.id("xlim_lo"), "x-min", value = NA, width = "100%")),
      htmltools::div(class = "pma-span-2",
        shiny::numericInput(.id("xlim_hi"), "x-max", value = NA, width = "100%")),

      # Blank rows around the pooled result. Always visible: they matter most
      # once the per-arm columns are hidden (that is when the heterogeneity
      # footer can collide with the x-axis) but they are legitimate spacing
      # controls at any time, and the conditionalPanels that used to hide them
      # only toggled display anyway.
      #
      # Defaults are NOT symmetric, and deliberately so:
      #  * above = 1 reproduces the blank row meta::forest() draws by default
      #    (pma_addrow_above() has always treated blank as 1). Rendered with 0
      #    the pooled "Random effects model" row butts straight up against the
      #    last study row.
      #  * below = 0, which is tighter than what plot_forest() derives on its
      #    own. Blank still means automatic and still reaches
      #    .auto_addrow_below(), which reserves 2 to 4 rows for the axis band,
      #    the Favors labels and the xlab; that heuristic buys clearance the
      #    plot usually does not need, and it bought it by adding whitespace
      #    to every forest. 0 gives the room back. If the heterogeneity text
      #    ends up sitting on the x-axis - most likely with the per-arm
      #    columns hidden - clearing this field restores the old behaviour.
      htmltools::p(class = "pma-card-subtitle pma-span-4",
        paste0("Blank rows around the pooled result. If the ",
               "heterogeneity text overlaps the x-axis - most ",
               "likely once the per-arm columns are hidden - use ",
               "these to move it up or down. Above: 0 removes the ",
               "blank row before the pooled result. Below: clear ",
               "the field for automatic spacing.")),
      htmltools::div(class = "pma-span-2",
        shiny::numericInput(addrows[["above"]], "Blank rows above pooled result",
                            value = 1, min = 0, step = 1, width = "100%")),
      htmltools::div(class = "pma-span-2",
        shiny::numericInput(addrows[["below"]], "Blank rows below pooled result",
                            value = 0, min = 0, step = 1, width = "100%")),

      # Decimal places in the per-arm Mean and SD columns. Both default to 1
      # here and in plot_forest(), rather than to {meta}'s own 2 and 4: an SD
      # printed to twice the precision of its mean is not what any trial
      # reports, and the four-decimal SD column was wide enough to squeeze the
      # forest itself. Only meaningful for a continuous outcome; shown
      # unconditionally all the same, because the panel is shared with the
      # Step 3 tabs and a control that appears and disappears with the outcome
      # type is harder to find than one that is simply inert.
      htmltools::div(class = "pma-span-2",
        shiny::numericInput(.id("digits_mean"), "Mean decimals",
                            value = 1, min = 0, step = 1, width = "100%")),
      htmltools::div(class = "pma-span-2",
        shiny::numericInput(.id("digits_sd"), "SD decimals",
                            value = 1, min = 0, step = 1, width = "100%")),

      # One checkbox, not two: plot_forest() keeps show_n and show_events as
      # separate arguments (correct for a library), but there is no case where
      # a user wants the N columns without the per-arm data columns, so the UI
      # drives both from a single value.
      htmltools::div(class = "pma-span-4",
        shiny::checkboxInput(.id("show_arm_columns"),
          "Show per-arm data columns (events or mean & SD, and N)",
          TRUE))
    )
  )
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

# Same again for the Mean / SD decimal-place spinners, where a blank field also
# has no auto mode: plot_forest() would receive NA, meta::forest() would reject
# it, and its caller's error retry drops the data columns rather than reporting
# the bad value - so the fallback has to happen before the value leaves here.
pma_forest_digits <- function(x, default = 1) {
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
