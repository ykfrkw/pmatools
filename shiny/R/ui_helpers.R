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
  steps <- c("Data", "Meta-analysis", "GRADE", "Export")
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
