# step3_grade.R - Step 3: GRADE 5-domain assessment + Final certainty (sub-tabs)

step3_ui <- function() {
  s <- EDU_COPY$steps$step3

  .domain_header <- function(name, badge_id, chip_id) {
    htmltools::div(
      class = "pma-accordion-title",
      style = "margin-bottom: 0.5rem;",
      htmltools::h4(class = "pma-domain-name",
                    style = "margin: 0; font-size: 1.1rem;", name),
      shiny::uiOutput(badge_id, inline = TRUE),
      shiny::uiOutput(chip_id,  inline = TRUE)
    )
  }

  .inputs_details <- function(..., title = "Inputs for this domain", open = TRUE) {
    htmltools::tags$details(
      open = if (isTRUE(open)) NA else NULL,
      htmltools::tags$summary(title),
      htmltools::div(...)
    )
  }
  .override_details <- function(..., title = "Override final judgment") {
    htmltools::tags$details(
      htmltools::tags$summary(title),
      htmltools::div(...)
    )
  }
  .grade_nav <- function(back_id, back_label, next_id, next_label = "Next") {
    htmltools::div(
      style = paste(
        "display: flex;",
        "justify-content: space-between;",
        "margin-top: 1.5rem;"),
      shiny::actionButton(back_id, back_label,
        class = "btn btn-secondary"),
      shiny::actionButton(next_id, next_label,
        class = "btn btn-primary")
    )
  }

  # Forest plot display panel (used in RoB and Inconsistency tabs)
  .forest_display_panel <- function(prefix) {
    htmltools::tags$details(
      style = "margin-top: 0.5rem;",
      htmltools::tags$summary("Forest plot display"),
      htmltools::div(
        style = paste(
          "display: grid;",
          "grid-template-columns: repeat(4, minmax(140px, 1fr));",
          "gap: 0.75rem 1rem;",
          "padding: 0.75rem 0.25rem 0.25rem;"),
        htmltools::div(style = "grid-column: span 4;",
          shiny::textInput(paste0(prefix, "_title"), "Title", value = "", width = "100%")),
        shiny::textInput(paste0(prefix, "_label_e"),  "Exp. label",     value = "", width = "100%"),
        shiny::textInput(paste0(prefix, "_label_c"),  "Ctrl label",     value = "", width = "100%"),
        shiny::textInput(paste0(prefix, "_favors_left"),  "Favors (left)",  placeholder = "e.g., Favors Control", width = "100%"),
        shiny::textInput(paste0(prefix, "_favors_right"), "Favors (right)", placeholder = "e.g., Favors CBT-I",   width = "100%"),
        shiny::numericInput(paste0(prefix, "_xlim_lo"), "x-min", value = NA, width = "100%"),
        shiny::numericInput(paste0(prefix, "_xlim_hi"), "x-max", value = NA, width = "100%"),
        shiny::numericInput(paste0(prefix, "_addrows_above"), "addrows.above.overall",
                            value = 1, min = 0, step = 1, width = "100%"),
        shiny::numericInput(paste0(prefix, "_addrows_below"), "addrows.below.overall",
                            value = 1, min = 0, step = 1, width = "100%"),
        htmltools::div(style = "grid-column: span 2;",
          shiny::checkboxInput(paste0(prefix, "_show_n"), "Show N columns (Exp / Ctrl)", FALSE)),
        htmltools::div(style = "grid-column: span 2;",
          shiny::checkboxInput(paste0(prefix, "_show_events"), "Show event columns (binary)", FALSE))
      )
    )
  }

  htmltools::tagList(
    pma_step_header(s$title, s$what),

    # Sticky Final certainty banner
    htmltools::div(
      class = "pma-sticky-cert",
      htmltools::span(class = "pma-cert-label", "Final certainty"),
      shiny::uiOutput("sticky_cert_badge", inline = TRUE),
      htmltools::span(
        style = "color: hsl(var(--muted-foreground)); font-size: 0.8rem;",
        shiny::uiOutput("sticky_cert_summary", inline = TRUE)
      )
    ),

    pma_card(
      title = "GRADE assessment",
      shiny::tabsetPanel(
        id = "grade_tabs",

        # --- Risk of Bias ---
        shiny::tabPanel("Risk of Bias",
          .domain_header("Risk of Bias", "rob_badge", "rob_chip"),
          pma_how_collapse(EDU_COPY$domains$rob$how),
          pma_reference(EDU_COPY$domains$rob$ref_text, EDU_COPY$domains$rob$doi),
          htmltools::p(
            style = "font-size: 0.85rem; color: hsl(var(--muted-foreground)); font-style: italic;",
            "See also: ",
            htmltools::tags$a(href = "https://www.bmj.com/content/366/bmj.l4898",
                              target = "_blank", "Sterne et al. RoB 2 (BMJ 2019)"),
            " for study-level risk of bias assessment."
          ),
          shiny::uiOutput("threshold_block_rob"),
          htmltools::h5("Evaluation"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("rob_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),
          htmltools::tags$details(
            class = "pma-edit-details",
            htmltools::tags$summary(
              class = "pma-edit-summary",
              htmltools::HTML(
                "&#9998;&nbsp; <strong>Edit per-study Risk of Bias</strong> ",
                "<span class='pma-edit-hint'>(click to expand &middot; ",
                "manually override each study or apply bulk presets)</span>"
              )
            ),
            htmltools::div(
              class = "pma-edit-body",
              htmltools::p(class = "pma-card-subtitle",
                           "Click a cell in the table to type a value (low / some / high). ",
                           "Use the bulk buttons to set all studies at once. ",
                           "Changes here are synced with Step 1."),
              htmltools::div(
                style = "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 0.5rem;",
                shiny::actionButton("step3_rob_set_low",  "Set all to Low",  class = "btn-sm"),
                shiny::actionButton("step3_rob_set_some", "Set all to Some", class = "btn-sm"),
                shiny::actionButton("step3_rob_set_high", "Set all to High", class = "btn-sm"),
                shiny::actionButton("step3_rob_clear",    "Clear all",       class = "btn-sm")
              ),
              DT::DTOutput("step3_rob_editor")
            )
          ),
          htmltools::h5("Forest plot stratified by RoB", style = "margin-top: 1rem;"),
          htmltools::div(class = "pma-forest-image",
            shinycssloaders::withSpinner(
              shiny::imageOutput("rob_forest", height = "auto"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "320px")),
          .forest_display_panel("rob"),
          .inputs_details(open = FALSE, title = "Inputs for this domain",
            shiny::sliderInput("rob_inf_threshold", "Inflation threshold",
              min = 0.05, max = 0.5, value = 0.10, step = 0.05),
            shiny::radioButtons("small_values", "Outcome direction",
              choices = c("(no override)" = "",
                          "Desirable (small = good, e.g., mortality)" = "desirable",
                          "Undesirable (small = bad, e.g., response)" = "undesirable"),
              inline = FALSE)
          ),
          .override_details(
            shiny::selectInput("rob_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious"))
          ),
          .grade_nav("grade_back_rob", "Back: Meta-analysis",
                     "grade_next_rob", "Next: Inconsistency")
        ),

        # --- Inconsistency ---
        shiny::tabPanel("Inconsistency",
          .domain_header("Inconsistency", "incon_badge", "incon_chip"),
          pma_how_collapse(EDU_COPY$domains$inconsistency$how),
          pma_reference(EDU_COPY$domains$inconsistency$ref_text,
                        EDU_COPY$domains$inconsistency$doi),
          shiny::uiOutput("threshold_block_inco"),
          htmltools::h5("Evaluation"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("incon_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),
          htmltools::h5("Forest plot"),
          htmltools::div(class = "pma-forest-image",
            shinycssloaders::withSpinner(
              shiny::imageOutput("incon_forest", height = "auto"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "320px")),
          .forest_display_panel("incon"),
          .inputs_details(open = FALSE, title = "Inputs for this domain",
            shiny::selectInput("ci_diff",
              "Step 1: Important differences in point estimates AND limited CI overlap?",
              choices = c("(no override)" = "", "No" = "no", "Yes" = "yes")),
            shiny::conditionalPanel(
              "input.ci_diff == 'yes'",
              shiny::radioButtons("threshold_side",
                "Step 2: Where do point estimates fall vs the threshold?",
                choices = c("Majority on one side"  = "majority_one_side",
                            "Opposite sides"        = "opposite_sides"),
                selected = character(0))
            ),
            shiny::conditionalPanel(
              "input.threshold_side == 'opposite_sides'",
              shiny::radioButtons("subgroup_explained",
                "Step 3: Explained by credible subgroup analysis?",
                choices = c("Yes" = "yes", "No" = "no"),
                selected = character(0))
            )
          ),
          .override_details(
            shiny::selectInput("incon_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious"))
          ),
          .grade_nav("grade_back_incon", "Back: Risk of Bias",
                     "grade_next_incon", "Next: Indirectness")
        ),

        # --- Indirectness ---
        shiny::tabPanel("Indirectness",
          .domain_header("Indirectness", "indir_badge", "indir_chip"),
          shiny::uiOutput("indirectness_banner"),
          pma_how_collapse(EDU_COPY$domains$indirectness$how),
          pma_reference(EDU_COPY$domains$indirectness$ref_text,
                        EDU_COPY$domains$indirectness$doi),
          shiny::radioButtons("indirectness", "Overall indirectness rating",
            choices = c("No" = "no",
                        "Some concerns" = "some_concerns",
                        "Serious" = "serious"),
            selected = "no", inline = TRUE),
          htmltools::tags$details(
            class = "pma-edit-details",
            htmltools::tags$summary(
              class = "pma-edit-summary",
              htmltools::HTML(
                "&#9998;&nbsp; <strong>Edit per-study Indirectness</strong> ",
                "<span class='pma-edit-hint'>(click to expand &middot; ",
                "optional per-study notes; the overall rating above is what feeds GRADE)</span>"
              )
            ),
            htmltools::div(
              class = "pma-edit-body",
              htmltools::p(class = "pma-card-subtitle",
                           "Click a cell in the table to type a value (low / some / high). ",
                           "Use the bulk buttons to set all studies at once. ",
                           "Changes here are synced with Step 1."),
              htmltools::div(
                style = "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 0.5rem;",
                shiny::actionButton("step3_indir_set_low",  "Set all to Low",  class = "btn-sm"),
                shiny::actionButton("step3_indir_set_some", "Set all to Some", class = "btn-sm"),
                shiny::actionButton("step3_indir_set_high", "Set all to High", class = "btn-sm"),
                shiny::actionButton("step3_indir_clear",    "Clear all",       class = "btn-sm")
              ),
              DT::DTOutput("step3_indir_editor")
            )
          ),
          htmltools::tags$details(
            htmltools::tags$summary("Considerations"),
            htmltools::div(
              shiny::radioButtons("indir_population",
                "Trial population sufficiently similar to target patients?",
                choices = c("Yes", "Some concern", "Serious concern"), inline = TRUE,
                selected = character(0)),
              htmltools::p(
                style = "font-size: 0.8rem; color: hsl(var(--muted-foreground)); margin-top: -0.4rem; margin-bottom: 0.6rem;",
                "Note: differences in trial population rarely affect relative effects in most clinical contexts (",
                htmltools::tags$a(href = "https://doi.org/10.1503/cmaj.200077",
                                  target = "_blank", "ICEMAN; Schandelmaier et al., CMAJ 2020"),
                ")."
              ),
              shiny::radioButtons("indir_intervention",
                "Intervention deliverable as studied?",
                choices = c("Yes", "Some concern", "Serious concern"), inline = TRUE,
                selected = character(0)),
              shiny::radioButtons("indir_comparator",
                "Comparator representative of usual care?",
                choices = c("Yes", "Some concern", "Serious concern"), inline = TRUE,
                selected = character(0)),
              shiny::radioButtons("indir_outcome",
                "Outcome patient-important (vs surrogate)?",
                choices = c("Yes", "Some concern", "Serious concern"), inline = TRUE,
                selected = character(0))
            )
          ),
          shiny::uiOutput("indir_forest_image_block"),
          .forest_display_panel("indir"),
          .grade_nav("grade_back_indir", "Back: Inconsistency",
                     "grade_next_indir", "Next: Imprecision")
        ),

        # --- Imprecision ---
        shiny::tabPanel("Imprecision",
          .domain_header("Imprecision", "impre_badge", "impre_chip"),
          pma_how_collapse(EDU_COPY$domains$imprecision$how),
          pma_reference(EDU_COPY$domains$imprecision$ref_text,
                        EDU_COPY$domains$imprecision$doi),
          shiny::uiOutput("threshold_block_impre"),
          htmltools::h5("Evaluation"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("impre_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),
          .inputs_details(open = TRUE, title = "Inputs for this domain",
            shiny::conditionalPanel(
              "input.outcome_type == 'binary'",
              shiny::uiOutput("ois_p0_ui")
            ),
            shiny::conditionalPanel(
              "input.outcome_type == 'continuous'",
              shiny::uiOutput("ois_sd_ui")
            ),
            shiny::numericInput("ois_events_override",
              "Override OIS - target events (binary)",
              value = NA, min = 0, step = 1),
            shiny::numericInput("ois_n_override",
              "Override OIS - target N (continuous)",
              value = NA, min = 0, step = 1)
          ),
          .override_details(
            shiny::selectInput("impre_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious"))
          ),
          .grade_nav("grade_back_impre", "Back: Indirectness",
                     "grade_next_impre", "Next: Publication bias")
        ),

        # --- Publication bias ---
        shiny::tabPanel("Publication bias",
          .domain_header("Publication bias", "pubias_badge", "pubias_chip"),
          pma_how_collapse(EDU_COPY$domains$pubias$how),
          pma_reference(EDU_COPY$domains$pubias$ref_text,
                        EDU_COPY$domains$pubias$doi),
          htmltools::p(
            style = "font-size: 0.85rem; color: hsl(var(--muted-foreground)); font-style: italic;",
            "You may also consider applying ",
            htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2023-076754",
                              target = "_blank", "RoB-ME"),
            " to assess risk of bias due to missing evidence (Page et al., BMJ 2023)."
          ),

          # ----- Q1: overall judgment gate ---------------------------------
          htmltools::h5("Q1. Does the situation argue against reporting bias?",
                        style = "margin-top: 1rem;"),
          htmltools::div(class = "pma-card-subtitle",
            htmltools::p(htmltools::strong("Suspect reporting bias when:")),
            htmltools::tags$ul(
              htmltools::tags$li("Unpublished data and grey literature were not searched."),
              htmltools::tags$li("The synthesis rests on a small number of positive early findings (eg, a newly marketed drug, where early evidence tends to overestimate efficacy and safety)."),
              htmltools::tags$li("Prior empirical evidence documents reporting bias for this comparison (eg, Turner et al. 2008 for placebo-controlled antidepressant trials).")
            ),
            htmltools::p(htmltools::strong("Reporting bias is unlikely when:")),
            htmltools::tags$ul(
              htmltools::tags$li("Unpublished studies have been identified and their findings agree with the published evidence."),
              htmltools::tags$li("Prospective trial registration is the field standard, and registered protocols / registries do not show important discrepancies with published reports.")
            )
          ),
          shiny::radioButtons("pubias_registry_complete",
            "Overall, does the situation argue against reporting bias?",
            choices = c(
              "(no overall judgment yet)"               = "",
              "No - reporting bias is plausible (rate down 1)"      = "no",
              "Yes - reporting bias is unlikely (no rate down)"     = "yes"
            ),
            inline = FALSE),
          htmltools::hr(),

          # ----- Q2: small + industry-sponsored ----------------------------
          htmltools::h5("Q2. Most or all studies small AND industry-sponsored?"),
          htmltools::p(class = "pma-card-subtitle",
            "A 'yes' answer is sufficient evidence on its own (rate down 1; some concerns)."
          ),
          shiny::radioButtons("pubias_small_industry", NULL,
            choices = c("(use default: no)" = "", "No" = "no", "Yes" = "yes"),
            inline = TRUE),
          htmltools::hr(),

          # ----- Q3 + Q4 + reference materials (server-rendered) ----------
          shiny::uiOutput("pubias_main_block"),

          # Reference: Subgroup analysis (Available vs Missing results) - RoB-ME.
          # DT::DTOutput must be statically placed; placing it inside the
          # uiOutput("pubias_main_block") above prevents DT/htmlwidgets from
          # binding cleanly. This block is shown unconditionally and the
          # server-side render returns NULL when k < 10.
          htmltools::hr(),
          htmltools::h5("Reference: Subgroup analysis (available vs missing results)"),
          htmltools::p(class = "pma-card-subtitle",
            "Studies with no extractable effect estimate are automatically ",
            "moved into the Missing-results subgroup. You can also add ",
            "trials that exist (registry / protocol / conference abstract) ",
            "but were not even loaded into this meta-analysis. The forest ",
            "plot renders Available and Missing results as two subgroups, ",
            "mirroring ",
            htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2023-076754",
                              target = "_blank", "Page et al., BMJ 2023 (RoB-ME)"),
            htmltools::HTML(paste0(
              ". This information is <strong>not part of the automated ",
              "GRADE algorithm</strong>, but you are encouraged to consider ",
              "it when finalising the publication-bias judgment manually ",
              "(use the override below)."))),
          htmltools::div(
            style = "display: flex; gap: 0.5rem; margin-bottom: 0.5rem;",
            shiny::actionButton("pubias_missing_add", "+ Add missing trial", class = "btn-sm")
          ),
          htmltools::p(class = "pma-card-subtitle",
            "Click any cell to edit. The Results known cell offers an ",
            "autocomplete list of recommended RoB-ME labels but also accepts ",
            "free text. Auto-classified rows from the dataset cannot be ",
            "removed (they are part of the meta-analysis); user-added rows ",
            "remain fully editable."),
          # Datalist powering the in-cell autocomplete for results_known.
          htmltools::tags$datalist(id = "pubias_rk_datalist",
            htmltools::tags$option(value = "Reported but data not extractable"),
            htmltools::tags$option(value = "Not measured"),
            htmltools::tags$option(value = "Measured but not reported (suspect P > 0.05)"),
            htmltools::tags$option(value = "Measured but not reported (suspect P < 0.05)"),
            htmltools::tags$option(value = "Measured but not reported (in the opposite direction)")
          ),
          # When DT injects an <input type="text"> on cell edit for the
          # results_known column (index 2 of the visible columns), give it a
          # `list` attribute so the browser shows the datalist suggestions
          # while still allowing free-text typing.
          htmltools::tags$script(htmltools::HTML(
            "$(document).on('focusin', '#pubias_missing_editor input[type=text]', function(){",
            "  var $td = $(this).closest('td');",
            "  var col = $td.parent().children().index($td);",
            "  if (col === 2) { $(this).attr('list', 'pubias_rk_datalist'); }",
            "});"
          )),
          DT::DTOutput("pubias_missing_editor"),
          shinycssloaders::withSpinner(
            shiny::imageOutput("pubias_missing_forest", height = "auto"),
            type = 4, color = "#0f172a", size = 0.6,
            proxy.height = "320px"),
          .forest_display_panel("pubias"),

          htmltools::hr(),
          htmltools::h5("Evaluation result"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("pubias_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),

          .override_details(
            shiny::selectInput("pubias_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious"))
          ),
          .grade_nav("grade_back_pubias", "Back: Imprecision",
                     "grade_next_pubias", "Next: Final certainty")
        ),

        # --- Final certainty (6th tab) ---
        shiny::tabPanel("Final certainty",
          htmltools::h5("GRADE Evidence Profile"),
          htmltools::div(
            style = "margin-top: 0.5rem; margin-bottom: 1rem;",
            shinycssloaders::withSpinner(
              shiny::uiOutput("final_certainty"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "200px")
          ),
          htmltools::hr(),
          htmltools::h5("Summary of Findings"),
          htmltools::div(
            style = "margin-top: 0.5rem; margin-bottom: 1rem;",
            shinycssloaders::withSpinner(
              shiny::uiOutput("sof_preview"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "200px")
          ),
          htmltools::p(
            style = "font-size: 0.85rem; color: hsl(var(--muted-foreground)); font-style: italic;",
            "Recommended: report both control event rate (CER) and experimental event rate (EER) ",
            "alongside the relative effect to aid clinical interpretation (",
            htmltools::tags$a(href = "https://doi.org/10.1136/bmjment-2023-300978",
                              target = "_blank", "Heimke et al., BMJ Ment Health 2024"),
            ")."
          ),
          htmltools::hr(),
          htmltools::h5("Display options"),
          htmltools::div(
            style = paste(
              "display: grid;",
              "grid-template-columns: repeat(2, minmax(220px, 1fr));",
              "gap: 0.75rem 1rem;"),
            shiny::textInput("outcome_name", "Outcome label",
              value = "Outcome"),
            shiny::numericInput("per", "Display rates per N patients",
              value = 1000, min = 1, step = 100),
            htmltools::div(style = "grid-column: span 2;",
              shiny::checkboxInput("prediction",
                "Show 95 percent prediction interval in Effect column", FALSE)),
            htmltools::div(style = "grid-column: span 2;",
              shiny::textInput("other_text",
                "Other considerations (free text shown in Evidence Profile)",
                placeholder = "e.g., All trials conducted in a single country; reporting bias")),
            htmltools::div(style = "grid-column: span 2;",
              shiny::radioButtons("other_downgrade",
                "Apply additional downgrade for the above?",
                choices = c("No (-0)" = "0",
                            "Yes, by 1 level (-1)" = "-1",
                            "Yes, by 2 levels (-2)" = "-2"),
                selected = "0", inline = TRUE)),
            shiny::conditionalPanel(
              "input.outcome_type == 'continuous' && (input.sm_cont == 'SMD' || input.sm_cont == 'MD')",
              htmltools::div(style = "grid-column: span 2;",
                shiny::checkboxInput("convert_smd_to_or",
                  "Show as dichotomous outcome (Chinn's formula)", FALSE)),
              shiny::conditionalPanel(
                "input.convert_smd_to_or",
                shiny::numericInput("baseline_risk_chinn",
                  "Control event rate (proportion responding)",
                  value = 0.30, min = 0.01, max = 0.99, step = 0.01),
                shiny::textInput("threshold_label",
                  "Threshold definition (free text)",
                  placeholder = "e.g., >=50 percent reduction in PHQ-9"),
                htmltools::div(style = "grid-column: span 2;",
                  shiny::checkboxInput("chinn_invert",
                    paste0("Invert OR direction (use this if a negative SMD ",
                           "represents the desirable / 'response' direction, ",
                           "so OR > 1 = treatment better)"),
                    value = FALSE))
              )
            )
          ),
          .grade_nav("grade_back_final", "Back: Publication bias",
                     "grade_next_final", "Next: Export")
        )
      )
    )
  )
}

step3_server <- function(input, output, session, state) {

  grade_tab_sequence <- c(
    "Risk of Bias",
    "Inconsistency",
    "Indirectness",
    "Imprecision",
    "Publication bias",
    "Final certainty"
  )

  advance_grade_tab <- function(current = NULL) {
    current <- current %||% shiny::isolate(input$grade_tabs) %||%
      grade_tab_sequence[1]
    idx <- match(current, grade_tab_sequence)
    if (is.na(idx)) idx <- 1L
    if (idx < length(grade_tab_sequence)) {
      shiny::updateTabsetPanel(
        session,
        "grade_tabs",
        selected = grade_tab_sequence[[idx + 1L]]
      )
      session$sendCustomMessage("scroll_top", list())
      return(invisible(TRUE))
    }
    commit <- state$step3_commit %||% function() TRUE
    if (isTRUE(commit())) {
      state$step <- 4L
      session$sendCustomMessage("scroll_top", list())
    }
    invisible(TRUE)
  }

  retreat_grade_tab <- function(current = NULL) {
    current <- current %||% shiny::isolate(input$grade_tabs) %||%
      grade_tab_sequence[1]
    idx <- match(current, grade_tab_sequence)
    if (is.na(idx)) idx <- 1L
    if (idx > 1L) {
      shiny::updateTabsetPanel(
        session,
        "grade_tabs",
        selected = grade_tab_sequence[[idx - 1L]]
      )
      session$sendCustomMessage("scroll_top", list())
      return(invisible(TRUE))
    }
    state$step <- 2L
    session$sendCustomMessage("scroll_top", list())
    invisible(TRUE)
  }

  shiny::observeEvent(input$grade_back_rob, {
    retreat_grade_tab("Risk of Bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_incon, {
    retreat_grade_tab("Inconsistency")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_indir, {
    retreat_grade_tab("Indirectness")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_impre, {
    retreat_grade_tab("Imprecision")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_pubias, {
    retreat_grade_tab("Publication bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_final, {
    retreat_grade_tab("Final certainty")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_rob, {
    advance_grade_tab("Risk of Bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_incon, {
    advance_grade_tab("Inconsistency")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_indir, {
    advance_grade_tab("Indirectness")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_impre, {
    advance_grade_tab("Imprecision")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_pubias, {
    advance_grade_tab("Publication bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_final, {
    advance_grade_tab("Final certainty")
  }, ignoreInit = TRUE)
  # ----- Threshold state (independent of UI render so Final certainty
  # doesn't flip as the user clicks tabs) ---------------------------------
  threshold_state <- shiny::reactiveVal(NA_real_)

  # Initialise threshold_state from suggest_threshold() as soon as state$ma is
  # available. This used to live inside .render_threshold_block, but that
  # ran via renderUI which Shiny suspends while the containing tab is
  # hidden -- so grade_obj() saw threshold = NA on first render and the
  # Final certainty silently changed when the user clicked Inconsistency
  # / Imprecision for the first time.
  shiny::observe({
    obj <- state$ma
    if (is.null(obj)) return()
    if (!is.na(threshold_state())) return()
    s <- tryCatch(suggest_threshold(obj), error = function(e) NULL)
    if (!is.null(s) && !is.null(s$threshold_user) && !is.na(s$threshold_user)) {
      threshold_state(round(s$threshold_user, 4))
    }
  })

  .render_threshold_block <- function(input_id) {
    obj <- state$ma
    if (is.null(obj)) return(htmltools::p("Run analysis first."))
    sm  <- obj$sm %||% "OR"
    val <- threshold_state()
    if (is.na(val)) {
      s <- tryCatch(suggest_threshold(obj), error = function(e) NULL)
      val <- if (!is.null(s) && !is.null(s$threshold_user)) round(s$threshold_user, 4) else NA_real_
    }
    hlp <- EDU_COPY$threshold_help[[sm]]   %||% ""
    lab <- EDU_COPY$threshold_labels[[sm]] %||% "Threshold for clinical importance"
    lab_html <- htmltools::HTML(paste0('<span style="white-space:nowrap">', lab, '</span>'))

    htmltools::tagList(
      htmltools::p(
        class = "pma-card-subtitle",
        paste0(
          "Threshold = the smallest effect that would be clinically meaningful. ",
          "Used by both Inconsistency (decision threshold) and Imprecision ",
          "(target effect for OIS). Editing here syncs with the other domain."
        )
      ),
      shiny::numericInput(input_id, lab_html, value = val, min = 0, step = 0.01),
      htmltools::p(class = "pma-card-subtitle", hlp),
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
    )
  }

  output$threshold_block_rob   <- shiny::renderUI(.render_threshold_block("threshold_rob"))
  output$threshold_block_inco  <- shiny::renderUI(.render_threshold_block("threshold_inco"))
  output$threshold_block_impre <- shiny::renderUI(.render_threshold_block("threshold_impre"))
  shiny::outputOptions(output, "threshold_block_rob",   suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_inco",  suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_impre", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$threshold_rob, {
    v <- input$threshold_rob
    if (is.null(v) || length(v) == 0 || is.na(v)) return()
    if (!isTRUE(all.equal(v, threshold_state()))) {
      threshold_state(v)
      shiny::updateNumericInput(session, "threshold_inco",  value = v)
      shiny::updateNumericInput(session, "threshold_impre", value = v)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$threshold_inco, {
    v <- input$threshold_inco
    if (is.null(v) || length(v) == 0 || is.na(v)) return()
    if (!isTRUE(all.equal(v, threshold_state()))) {
      threshold_state(v)
      shiny::updateNumericInput(session, "threshold_rob",   value = v)
      shiny::updateNumericInput(session, "threshold_impre", value = v)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$threshold_impre, {
    v <- input$threshold_impre
    if (is.null(v) || length(v) == 0 || is.na(v)) return()
    if (!isTRUE(all.equal(v, threshold_state()))) {
      threshold_state(v)
      shiny::updateNumericInput(session, "threshold_rob",  value = v)
      shiny::updateNumericInput(session, "threshold_inco", value = v)
    }
  }, ignoreInit = TRUE)

  # ----- OIS default values -----
  output$ois_p0_ui <- shiny::renderUI({
    obj <- state$ma
    val <- if (!is.null(obj) && !is.null(obj$event.c) && !is.null(obj$n.c) &&
               sum(obj$n.c, na.rm = TRUE) > 0) {
      round(sum(obj$event.c, na.rm = TRUE) / sum(obj$n.c, na.rm = TRUE), 4)
    } else NA
    shiny::numericInput("ois_p0",
      "Baseline (control) event rate for OIS (auto from data)",
      value = val, min = 0, max = 1, step = 0.01)
  })

  output$ois_sd_ui <- shiny::renderUI({
    obj <- state$ma
    val <- if (!is.null(obj)) {
      sd_pooled <- tryCatch(compute_pooled_sd(obj),
                            error = function(e) NULL)
      if (!is.null(sd_pooled) && is.finite(sd_pooled) && sd_pooled > 0) {
        round(sd_pooled, 4)
      } else NA
    } else NA
    shiny::numericInput("ois_sd",
      "Pooled SD for OIS (auto from data)",
      value = val, min = 0, step = 0.1)
  })

  .na_null <- function(x) {
    if (is.null(x)) return(NULL)
    if (length(x) == 0) return(NULL)
    if (is.numeric(x) && all(is.na(x))) return(NULL)
    if (is.character(x) && (!nzchar(x) || all(is.na(x)))) return(NULL)
    x
  }

  .study_labels_for_grade <- function(obj) {
    studs <- as.character(obj$studlab)
    if (!length(studs)) return(character())
    if (identical(attr(obj, "pma_rare_engine"), "mmeta")) return(studs)
    if (!is.null(obj$k) && length(studs) == obj$k) return(studs)
    keep <- !is.na(obj$TE)
    if (length(keep) == length(studs)) return(studs[keep])
    studs
  }

  .study_covariate <- function(labels, col, default = NA_character_) {
    labels <- as.character(labels)
    source <- NULL
    rt <- state$rob_table
    if (!is.null(rt) && col %in% names(rt)) {
      source <- rt[, c("studlab", col), drop = FALSE]
    } else {
      d <- state$data
      if (!is.null(d) && col %in% names(d)) {
        first_per_study <- !duplicated(d$studlab)
        source <- d[first_per_study, c("studlab", col), drop = FALSE]
      }
    }
    if (is.null(source)) return(NULL)
    lookup <- as.character(source[[col]])
    names(lookup) <- as.character(source$studlab)
    out <- unname(lookup[labels])
    if (!is.null(default)) out[is.na(out) | !nzchar(trimws(out))] <- default
    out
  }

  .effective_pubias_k <- function(obj) {
    te <- obj$TE
    se <- obj$seTE
    if (!is.null(te) && !is.null(se) &&
        length(te) == length(se) && length(te) > 0L) {
      return(sum(is.finite(te) & is.finite(se) & se > 0))
    }
    obj$k %||% 0L
  }

  grade_obj <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(NULL)

    rob_arg <- .study_covariate(.study_labels_for_grade(obj), "rob", default = "*")

    rob_override <- if (nzchar(input$rob_override %||% "")) input$rob_override else NULL
    if (!is.null(rob_override)) rob_arg <- rob_override

    sv <- if (nzchar(input$small_values %||% "")) input$small_values else NULL

    incon_override <- if (nzchar(input$incon_override %||% "")) input$incon_override else NULL
    ci_diff <- if (nzchar(input$ci_diff %||% "")) input$ci_diff else NULL
    threshold_side <- if (!is.null(input$threshold_side) &&
                          length(input$threshold_side) > 0 &&
                          nzchar(input$threshold_side)) input$threshold_side else NULL
    subgroup_expl <- if (!is.null(input$subgroup_explained) &&
                         length(input$subgroup_explained) > 0 &&
                         nzchar(input$subgroup_explained)) input$subgroup_explained else NULL

    impre_override <- if (nzchar(input$impre_override %||% "")) input$impre_override else NULL

    pubias_si <- if (nzchar(input$pubias_small_industry %||% "")) input$pubias_small_industry else NULL
    pubias_fa <- if (nzchar(input$pubias_funnel_asymmetry %||% "")) input$pubias_funnel_asymmetry else NULL
    pubias_un <- if (nzchar(input$pubias_unpublished %||% "")) input$pubias_unpublished else NULL
    pubias_rc <- if (nzchar(input$pubias_registry_complete %||% "")) input$pubias_registry_complete else NULL
    pubias_ov <- if (nzchar(input$pubias_override %||% "")) input$pubias_override else NULL

    th <- threshold_state()
    th_arg <- if (is.numeric(th) && !is.na(th) && th > 0) th else NULL

    args <- list(
      meta_obj                 = obj,
      study_design             = "RCT",
      rob                      = rob_arg,
      rob_inflation_threshold  = input$rob_inf_threshold %||% 0.10,
      small_values             = sv,
      indirectness             = input$indirectness %||% "no",
      inconsistency            = incon_override,
      inconsistency_ci_diff            = ci_diff,
      inconsistency_threshold_side     = threshold_side,
      inconsistency_subgroup_explained = subgroup_expl,
      threshold       = th_arg,
      threshold_scale = "auto",
      outcome_type = if (identical(input$outcome_type, "binary")) "relative" else "absolute",
      ois_p0       = .na_null(input$ois_p0),
      ois_sd       = .na_null(input$ois_sd),
      ois_events   = .na_null(input$ois_events_override),
      ois_n        = .na_null(input$ois_n_override),
      pubias_small_industry    = pubias_si,
      pubias_funnel_asymmetry  = pubias_fa,
      pubias_unpublished       = pubias_un,
      # Q1: only "yes" (denied) is forwarded to the package short-circuit;
      # "no" (suspected) is handled by a post-override below so it forces
      # rate-down regardless of Q2-Q5.
      pubias_registry_complete = if (identical(pubias_rc, "yes")) "yes" else NULL,
      outcome_name = if (!is.null(input$outcome_name) && nzchar(input$outcome_name))
                       input$outcome_name else "Outcome"
    )

    g <- tryCatch(
      suppressWarnings(do.call(grade_meta, args)),
      error = function(e) {
        shiny::showNotification(paste("grade_meta error:", conditionMessage(e)),
                                type = "error")
        NULL
      }
    )

    if (!is.null(g)) {
      if (!is.null(impre_override)) {
        idx <- which(g$domain_assessments$domain == "Imprecision")
        if (length(idx)) {
          g$domain_assessments$judgment[idx] <- impre_override
          g$domain_assessments$auto[idx]     <- FALSE
          g$domain_assessments$downgrade[idx] <- pmatools_GRADE_DOWNGRADE(impre_override)
        }
      }
      # Q1 = "no" (reporting bias suspected) forces rate-down 1 regardless
      # of Q2-Q5. Run BEFORE pubias_ov so the manual override below can still
      # win if the user explicitly sets it.
      if (identical(pubias_rc, "no")) {
        idx <- which(g$domain_assessments$domain == "Publication bias")
        if (length(idx)) {
          g$domain_assessments$judgment[idx]  <- "some_concerns"
          g$domain_assessments$auto[idx]      <- FALSE
          g$domain_assessments$downgrade[idx] <- -1L
          g$domain_assessments$notes[idx] <- paste0(
            "Q1: reporting bias suspected based on the overall judgment ",
            "of the listed conditions; rate down 1 (some concerns) regardless ",
            "of Q2-Q5. | ", g$domain_assessments$notes[idx])
        }
      }
      if (!is.null(pubias_ov)) {
        idx <- which(g$domain_assessments$domain == "Publication bias")
        if (length(idx)) {
          g$domain_assessments$judgment[idx] <- pubias_ov
          g$domain_assessments$auto[idx]     <- FALSE
          g$domain_assessments$downgrade[idx] <- pmatools_GRADE_DOWNGRADE(pubias_ov)
        }
      }
      # Additional user-specified downgrade from "Other considerations"
      other_dg <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
      if (is.na(other_dg)) other_dg <- 0L
      other_dg <- min(0L, other_dg)  # cannot rate UP via this control

      total_dg <- sum(g$domain_assessments$downgrade) + other_dg
      score    <- max(1L, 4L + total_dg)
      g$certainty_score    <- score
      g$certainty          <- c("Very Low","Low","Moderate","High")[score]
      g$other_text         <- input$other_text
      g$other_downgrade    <- other_dg
    }

    g
  })

  shiny::observe({
    g <- grade_obj()
    if (!is.null(g)) state$grade <- g
  })

  pmatools_GRADE_DOWNGRADE <- function(j) {
    # 3-level system (v0.3+): -1 = some_concerns, -2 = serious. Legacy
    # labels are still mapped so old user input doesn't break.
    c(no = 0, some = -1, some_concerns = -1,
      serious = -2, very_serious = -2)[[j]]
  }

  domain_judgment <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    row <- g$domain_assessments[g$domain_assessments$domain == domain, ]
    if (nrow(row) == 0) return(NULL)
    row$judgment[1]
  }

  domain_notes <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return("")
    row <- g$domain_assessments[g$domain_assessments$domain == domain, ]
    if (nrow(row) == 0) return("")
    row$notes[1]
  }

  output$rob_badge    <- shiny::renderUI(pma_judgment_badge(domain_judgment("Risk of bias")    %||% "no"))
  output$incon_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Inconsistency")   %||% "no"))
  output$indir_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Indirectness")    %||% "no"))
  output$impre_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Imprecision")     %||% "no"))
  output$pubias_badge <- shiny::renderUI(pma_judgment_badge(domain_judgment("Publication bias")%||% "no"))

  output$rob_chip    <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Risk of bias")    %||% "no"))
  output$incon_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Inconsistency")   %||% "no"))
  output$indir_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Indirectness")    %||% "no"))
  output$impre_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Imprecision")     %||% "no"))
  output$pubias_chip <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Publication bias")%||% "no"))

  output$sticky_cert_badge <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) return(htmltools::span("--"))
    pma_certainty_badge(g$certainty)
  })

  output$sticky_cert_summary <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) return(htmltools::span(""))
    d <- g$domain_assessments
    total_dg <- sum(d$downgrade)
    parts <- vapply(seq_len(nrow(d)), function(i) {
      sprintf("%s:%s",
              substr(d$domain[i], 1, 4),
              switch(d$judgment[i],
                     no             = "OK",
                     some           = "S",   # legacy
                     some_concerns  = "S",
                     serious        = "X",
                     very_serious   = "X",   # legacy (now -2 same as serious)
                     "?"))
    }, character(1))
    htmltools::span(
      sprintf("(start High; total %+d) ", total_dg),
      paste(parts, collapse = " | ")
    )
  })

  output$rob_notes    <- shiny::renderText(domain_notes("Risk of bias"))
  output$incon_notes  <- shiny::renderText(domain_notes("Inconsistency"))
  output$impre_notes  <- shiny::renderText(domain_notes("Imprecision"))
  output$pubias_notes <- shiny::renderText(domain_notes("Publication bias"))

  # Helper to collect display-panel inputs for a given prefix.
  .display_args <- function(prefix) {
    pick_text <- function(id) {
      v <- input[[id]]
      if (is.null(v) || !nzchar(v)) NULL else v
    }
    lo <- input[[paste0(prefix, "_xlim_lo")]]
    hi <- input[[paste0(prefix, "_xlim_hi")]]
    xlim <- if (!is.null(lo) && !is.null(hi) && !is.na(lo) && !is.na(hi) && lo < hi) {
      c(lo, hi)
    } else NULL
    list(
      title        = pick_text(paste0(prefix, "_title")),
      label_e      = pick_text(paste0(prefix, "_label_e")),
      label_c      = pick_text(paste0(prefix, "_label_c")),
      favors_left  = pick_text(paste0(prefix, "_favors_left")),
      favors_right = pick_text(paste0(prefix, "_favors_right")),
      xlim         = xlim,
      show_n       = isTRUE(input[[paste0(prefix, "_show_n")]]),
      show_events  = isTRUE(input[[paste0(prefix, "_show_events")]]),
      addrow_above = input[[paste0(prefix, "_addrows_above")]] %||% 1,
      addrow_below = input[[paste0(prefix, "_addrows_below")]] %||% 1
    )
  }

  # Mirror per-plot display options into state for downstream (export) use
  shiny::observe({
    state$display$forest_rob    <- .display_args("rob")
    state$display$forest_incon  <- .display_args("incon")
    state$display$forest_indir  <- .display_args("indir")
    state$display$forest_pubias <- .display_args("pubias")
    state$display$funnel_pub    <- pma_funnel_display_args(input, "funnel_pub")
    state$display$funnel_trim   <- pma_funnel_display_args(input, "funnel_trim",
                                                           include_egger = FALSE)
  })

  # ----- RoB stratified forest plot (render generous, trim, display) -----
  output$rob_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    rob_vec <- if (!is.null(state$data) && "rob" %in% names(state$data)) {
      .study_covariate(as.character(obj$studlab), "rob", default = "*")
    } else .study_covariate(as.character(obj$studlab), "rob", default = "*")
    da <- .display_args("rob")
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (obj$k %||% 0L) + 600,
      plot_fn = function() {
        do.call(plot_forest_rob,
                c(list(meta_obj = obj, rob = rob_vec), da))
      }
    )
  }, deleteFile = TRUE)

  .has_indir_forest_input <- function() {
    vals <- character()
    rt <- state$rob_table
    if (!is.null(rt) && "indirectness" %in% names(rt)) {
      vals <- c(vals, as.character(rt$indirectness))
    }
    d <- state$data
    if (!is.null(d) && "indirectness" %in% names(d)) {
      vals <- c(vals, as.character(d$indirectness))
    }
    vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
    length(vals) > 0
  }

  output$indir_forest_image_block <- shiny::renderUI({
    if (!.has_indir_forest_input()) return(NULL)
    htmltools::tagList(
      htmltools::h5("Forest plot stratified by Indirectness",
                    style = "margin-top: 1rem;"),
      htmltools::div(class = "pma-forest-image",
        shinycssloaders::withSpinner(
          shiny::imageOutput("indir_forest", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px"))
    )
  })

  # ----- Indirectness stratified forest plot (mirror of RoB) -----
  output$indir_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj) || !.has_indir_forest_input()) return(NULL)
    indir_vec <- if (!is.null(state$data) && "indirectness" %in% names(state$data)) {
      .study_covariate(as.character(obj$studlab), "indirectness", default = "low")
    } else .study_covariate(as.character(obj$studlab), "indirectness", default = "low")
    da <- .display_args("indir")
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (obj$k %||% 0L) + 600,
      plot_fn = function() {
        do.call(plot_forest_indirectness,
                c(list(meta_obj = obj, indirectness = indir_vec), da))
      }
    )
  }, deleteFile = TRUE)

  # ----- Inconsistency forest plot (render generous, trim, display) -----
  output$incon_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    da <- .display_args("incon")
    pma_render_trimmed(
      width  = 1400,
      height = 200 + 80 * (obj$k %||% 0L) + 400,
      plot_fn = function() {
        do.call(plot_forest,
                c(list(meta_obj = obj, auto_layout = TRUE), da))
      }
    )
  }, deleteFile = TRUE)

  # ----- Publication bias: Q3 + Q4 (or Q5) flowchart-ordered block -----
  output$pubias_main_block <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) {
      return(htmltools::p("Run analysis first."))
    }
    k <- .effective_pubias_k(obj)

    if (k >= 10) {
      htmltools::tagList(
        # Q3
        htmltools::h5(sprintf(
          "Q3. Statistical analysis feasible - k = %d >= 10", k)),
        htmltools::p(class = "pma-card-subtitle",
          "Egger's linear regression test is run automatically and shown ",
          "below the funnel plot."),

        # Q4 funnel + Egger auto + visual override
        htmltools::h5("Q4. Funnel plot inspection + Egger's test",
                      style = "margin-top: 1rem;"),
        htmltools::p(class = "pma-card-subtitle",
          "Egger's p < 0.01 -> rate down 2 (serious); ",
          "0.01 <= p < 0.05 -> rate down 1 (some concerns); ",
          "p >= 0.05 -> do not rate down."),
        shinycssloaders::withSpinner(
          shiny::imageOutput("pubias_funnel", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px"),
        pma_funnel_display_panel("funnel_pub"),
        shiny::uiOutput("pubias_egger_result"),
        shiny::selectInput("pubias_funnel_asymmetry",
          "Visual override of Egger",
          choices = c("(use Egger)" = "",
                      "Funnel symmetric"  = "no",
                      "Funnel asymmetric" = "yes")),

        # Reference materials: trim-and-fill funnel + summary text
        htmltools::hr(),
        htmltools::h5("Reference: trim-and-fill"),
        htmltools::p(class = "pma-card-subtitle",
          "Filled (imputed) studies appear alongside observed studies on the ",
          "funnel plot. The numerical summary below shows how the pooled ",
          "estimate would shift if these imputed studies actually existed. ",
          htmltools::HTML(paste0(
            "This information is <strong>not part of the automated GRADE ",
            "algorithm</strong>, but you are encouraged to consider it ",
            "when finalising the publication-bias judgment manually."))),
        shinycssloaders::withSpinner(
          shiny::imageOutput("pubias_trimfill_funnel", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px"),
        pma_funnel_display_panel("funnel_trim", include_egger = FALSE),
        shiny::uiOutput("pubias_trimfill_summary")
      )
    } else {
      htmltools::tagList(
        htmltools::h5(sprintf(
          "Q3. Statistical analysis NOT feasible - k = %d < 10", k)),
        htmltools::p(class = "pma-card-subtitle",
          "Egger's test would be unreliable below 10 studies. The algorithm ",
          "relies on a registry / regulatory-database search instead."),

        htmltools::h5("Q5. Documentation of unpublished studies",
                      style = "margin-top: 1rem;"),
        htmltools::p(class = "pma-card-subtitle",
          "If unpublished trials are documented in a registry ",
          "(eg, ClinicalTrials.gov, FDA), rate down 1."),
        shiny::radioButtons("pubias_unpublished",
          "Q5. Unpublished studies documented?",
          choices = c("(use default: no)" = "", "No" = "no", "Yes" = "yes"),
          inline = TRUE)
      )
    }
  })
  shiny::outputOptions(output, "pubias_main_block", suspendWhenHidden = FALSE)

  # Contour-enhanced funnel plot (Q4 visual)
  output$pubias_funnel <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    da <- pma_funnel_display_args(input, "funnel_pub")
    show_egger <- if (is.na(da$show_egger)) TRUE else da$show_egger
    pma_render_trimmed(
      width  = da$width,
      height = da$height,
      plot_fn = function() {
        if (!is.null(da$xlim))
          plot_funnel(obj, show_egger = show_egger, xlim = da$xlim)
        else
          plot_funnel(obj, show_egger = show_egger)
      }
    )
  }, deleteFile = TRUE)

  # Egger's auto judgment displayed as colour-coded callout
  output$pubias_egger_result <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj) || .effective_pubias_k(obj) < 10) return(NULL)
    res <- tryCatch(
      suppressWarnings(meta::metabias(obj, method.bias = "linreg")),
      error = function(e) NULL
    )
    if (is.null(res) || is.null(res$p.value) || is.na(res$p.value)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "Egger's test could not be computed."))
    }
    pval <- res$p.value
    judgment <- if (pval < 0.01) {
      list(text = sprintf("p = %.3f < 0.01 - strong evidence of asymmetry. Auto judgment: serious (rate down 2).", pval),
           color = "#a02020")
    } else if (pval < 0.05) {
      list(text = sprintf("p = %.3f (0.01 <= p < 0.05) - moderate evidence of asymmetry. Auto judgment: some concerns (rate down 1).", pval),
           color = "#c07020")
    } else {
      list(text = sprintf("p = %.3f >= 0.05 - no strong evidence of asymmetry. Auto judgment: no rate down.", pval),
           color = "#208050")
    }
    htmltools::div(
      style = sprintf(
        "padding: 0.6rem 0.85rem; background: #f5f5f5; border-left: 4px solid %s; margin: 0.5rem 0;",
        judgment$color),
      htmltools::p(style = "margin: 0;",
        htmltools::strong("Egger's regression: "),
        judgment$text)
    )
  })

  # Trim-and-fill funnel plot (reference only, NOT forest)
  # Imputed (filled) studies are drawn as solid red filled circles so they
  # stand out from observed studies (default dark-gray fill, black border).
  # We pass per-point vectors of pch / col / bg directly to meta::funnel(),
  # which forwards them to its single internal points() call (line 400 of
  # meta::funnel.meta). This is more reliable than drawing an overlay on
  # top, since the y-axis transformation in meta::funnel is not necessarily
  # raw seTE.
  output$pubias_trimfill_funnel <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj) || .effective_pubias_k(obj) < 10) {
      return(list(src = "", contentType = "image/png",
                  alt = "Trim-and-fill requires k >= 10.", width = "100%"))
    }
    tf <- tryCatch(suppressWarnings(meta::trimfill(obj)),
                   error = function(e) NULL)
    da <- pma_funnel_display_args(input, "funnel_trim", include_egger = FALSE)

    pma_render_trimmed(
      width  = da$width,
      height = da$height,
      plot_fn = function() {
        if (is.null(tf)) {
          graphics::plot.new()
          graphics::title(main = "Trim-and-fill could not be computed")
          return(invisible(NULL))
        }

        par_old <- graphics::par(mar = c(4, 4, 1, 2))
        on.exit(graphics::par(par_old), add = TRUE)

        n_total <- length(tf$TE)
        is_imp  <- if (!is.null(tf$trimfill)) {
          as.logical(tf$trimfill)
        } else {
          k0 <- if (!is.null(tf$k0)) as.integer(tf$k0) else
                (n_total - (obj$k %||% 0L))
          c(rep(FALSE, n_total - k0), rep(TRUE, k0))
        }

        # Per-point styling. pch = 21 is a filled circle that respects both
        # `col` (border) and `bg` (fill).
        pch_vec <- rep(21L, n_total)
        col_vec <- ifelse(is_imp, "red", "black")
        bg_vec  <- ifelse(is_imp, "red", "darkgray")
        cex_vec <- ifelse(is_imp, 1.6, 1.0)

        funnel_args <- list(tf,
                            contour = c(0.9, 0.95, 0.99),
                            pch = pch_vec,
                            col = col_vec,
                            bg  = bg_vec,
                            cex = cex_vec)
        if (!is.null(da$xlim)) funnel_args$xlim <- da$xlim
        do.call(meta::funnel, funnel_args)

        graphics::legend(
          "topright",
          legend = c("Observed studies", "Imputed (filled) studies"),
          pch    = c(21, 21),
          col    = c("black", "red"),
          pt.bg  = c("darkgray", "red"),
          pt.cex = c(1.0, 1.4),
          bty    = "o", bg = "#ffffff", cex = 0.8
        )
      }
    )
  }, deleteFile = TRUE)

  # Trim-and-fill numerical summary
  output$pubias_trimfill_summary <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj) || (obj$k %||% 0L) < 10) return(NULL)
    tf <- tryCatch(suppressWarnings(meta::trimfill(obj)),
                   error = function(e) NULL)
    if (is.null(tf)) return(NULL)

    k_imputed <- length(tf$TE) - length(obj$TE)
    te_orig <- obj$TE.random
    te_adj  <- tf$TE.random
    is_log  <- !is.null(obj$sm) && obj$sm %in% c("OR", "RR", "HR", "RoM", "IRR")

    fmt <- function(x) {
      if (!is.finite(x)) return("NA")
      if (is_log) sprintf("%.3f (log %s = %.3f)", exp(x), obj$sm, x)
      else sprintf("%.3f", x)
    }

    sign_flips <- is.finite(te_orig) && is.finite(te_adj) &&
                  (sign(te_orig) != sign(te_adj)) &&
                  (abs(te_orig) > 1e-6) && (abs(te_adj) > 1e-6)

    htmltools::div(
      style = paste0(
        "padding: 0.6rem 0.85rem; background: #f9f9f9; ",
        "border: 1px solid #ddd; margin: 0.5rem 0; ",
        "font-family: monospace; font-size: 0.85rem;"
      ),
      htmltools::p(style = "margin: 0 0 0.25rem;",
        htmltools::strong("Trim-and-fill summary (reference only)")),
      htmltools::p(style = "margin: 0;",
        sprintf("Imputed studies: %d", k_imputed)),
      htmltools::p(style = "margin: 0;",
        sprintf("Original pooled TE.random  = %s", fmt(te_orig))),
      htmltools::p(style = "margin: 0;",
        sprintf("Adjusted pooled TE.random = %s%s", fmt(te_adj),
                if (sign_flips) "  [direction flips]" else ""))
    )
  })

  # ----- Reference: Subgroup analysis (Available vs Missing results) -----
  # Schema: studlab (chr), n (int), results_known (chr), source (chr).
  # source = "auto" for dataset-derived rows (NA TE in meta_obj); "user"
  # for rows added via "+ Add missing trial".
  .pubias_missing_empty <- function() {
    data.frame(studlab = character(0), n = integer(0),
               results_known = character(0),
               source = character(0),
               stringsAsFactors = FALSE)
  }

  # Auto-seed: when state$ma changes, refresh the auto rows from NA-TE
  # studies. Preserve any user edits to existing auto rows (matched by
  # studlab) and keep all "+ Add" rows untouched.
  shiny::observe({
    obj <- state$ma
    if (is.null(obj)) return()
    k_te <- length(obj$TE)
    if (k_te == 0L) return()

    studlab_obj <- as.character(obj$studlab)
    if (length(studlab_obj) > k_te) studlab_obj <- studlab_obj[seq_len(k_te)]
    n_obj <- if (!is.null(obj$n.e) && !is.null(obj$n.c) &&
                 length(obj$n.e) >= k_te && length(obj$n.c) >= k_te) {
      obj$n.e[seq_len(k_te)] + obj$n.c[seq_len(k_te)]
    } else {
      rep(NA_integer_, k_te)
    }
    auto_idx <- which(!(is.finite(obj$TE) & is.finite(obj$seTE)))

    auto_df <- if (length(auto_idx)) {
      data.frame(
        studlab = studlab_obj[auto_idx],
        n = suppressWarnings(as.integer(n_obj[auto_idx])),
        results_known = "Reported but data not extractable",
        source = "auto",
        stringsAsFactors = FALSE
      )
    } else .pubias_missing_empty()

    cur <- state$pubias_missing
    new_state <- if (is.null(cur) || nrow(cur) == 0L) {
      auto_df
    } else {
      src_col <- if ("source" %in% names(cur)) cur$source else rep("user", nrow(cur))
      user_rows <- cur[src_col == "user", , drop = FALSE]
      prev_auto <- cur[src_col == "auto", , drop = FALSE]
      if (nrow(auto_df) && nrow(prev_auto)) {
        m <- match(auto_df$studlab, prev_auto$studlab)
        have <- !is.na(m)
        auto_df$results_known[have] <- prev_auto$results_known[m[have]]
        auto_df$n[have]              <- prev_auto$n[m[have]]
      }
      rbind(auto_df, user_rows)
    }
    if (!identical(new_state, cur)) state$pubias_missing <- new_state
  })

  shiny::observeEvent(input$pubias_missing_add, {
    cur <- state$pubias_missing %||% .pubias_missing_empty()
    cur <- rbind(cur, data.frame(
      studlab = "(new trial)",
      n = NA_integer_,
      results_known = "Measured but not reported (suspect P > 0.05)",
      source = "user",
      stringsAsFactors = FALSE))
    state$pubias_missing <- cur
  })

  output$pubias_missing_editor <- DT::renderDT({
    d <- state$pubias_missing %||% .pubias_missing_empty()
    display <- d[, c("studlab", "n", "results_known"), drop = FALSE]
    DT::datatable(
      display,
      editable = list(target = "cell", disable = list(columns = 0)),
      options  = list(dom = "tp", pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  shiny::outputOptions(output, "pubias_missing_editor", suspendWhenHidden = FALSE)

  # Cell edits accept free text. studlab is read-only (auto rows must
  # match meta_obj; user-added rows can change studlab via a future
  # iteration if needed). n and results_known are freely editable.
  shiny::observeEvent(input$pubias_missing_editor_cell_edit, {
    info <- input$pubias_missing_editor_cell_edit
    if (is.null(info)) return()
    d <- state$pubias_missing %||% .pubias_missing_empty()
    if (nrow(d) == 0) return()
    col_name <- c("studlab", "n", "results_known")[info$col + 1]
    new_val <- info$value
    if (col_name == "n") {
      d$n[info$row] <- suppressWarnings(as.integer(new_val))
    } else {
      d[[col_name]][info$row] <- as.character(new_val)
    }
    state$pubias_missing <- d
  })

  output$pubias_missing_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj) || (obj$k %||% 0L) < 10) {
      return(list(src = "", contentType = "image/png",
                  alt = "Missing-results forest requires k >= 10.",
                  width = "100%"))
    }
    m_df <- state$pubias_missing %||% .pubias_missing_empty()
    da <- .display_args("pubias")
    # Adaptive canvas: 1 row per available study + 1 row per missing study
    # plus margin for two subgroup labels and the overall pooled diamond.
    k_avail <- length(obj$TE)
    k_miss  <- nrow(m_df)
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (k_avail + k_miss) + 600,
      plot_fn = function() {
        do.call(plot_forest_pubias_subgroup,
                c(list(meta_obj = obj, missing_df = m_df,
                       auto_detect = FALSE), da))
      }
    )
  }, deleteFile = TRUE)

  shiny::observeEvent(input$indirectness, {
    state$indir_reviewed <- TRUE
  })

  output$indirectness_banner <- shiny::renderUI({
    if (isTRUE(state$indir_reviewed)) return(NULL)
    pma_banner(EDU_COPY$domains$indirectness$banner)
  })

  output$final_certainty <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) {
      return(htmltools::p("Run analysis and configure domains."))
    }
    other_dg <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
    if (is.na(other_dg)) other_dg <- 0L
    ft <- tryCatch(
      evidence_profile(g,
                       other_text      = input$other_text,
                       other_downgrade = other_dg),
      error = function(e) NULL
    )
    if (is.null(ft)) {
      return(htmltools::p("(Evidence Profile not yet available)"))
    }
    htmltools::tags$div(
      style = "margin-top: 1rem;",
      tryCatch(flextable::htmltools_value(ft),
               error = function(e) htmltools::p(paste("Render error:",
                                                      conditionMessage(e))))
    )
  })

  output$sof_preview <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) return(htmltools::p("..."))
    convert <- isTRUE(input$convert_smd_to_or)
    args <- list(
      x          = g,
      per        = input$per %||% 1000,
      prediction = isTRUE(input$prediction)
    )
    if (convert) {
      args$convert_smd_to_or <- TRUE
      args$baseline_risk     <- input$baseline_risk_chinn
      args$threshold_label   <- input$threshold_label
      args$chinn_invert      <- isTRUE(input$chinn_invert)
    }
    ft <- tryCatch(do.call(sof_table, args),
                   error = function(e) NULL)
    if (is.null(ft)) return(htmltools::p("(SoF not yet available)"))
    tryCatch(flextable::htmltools_value(ft),
             error = function(e) htmltools::p(paste("SoF render error:", conditionMessage(e))))
  })

  # ----- Outcome name default: track outcome_type unless the user has
  # manually typed something custom. Uses observe (not observeEvent) so it
  # fires both when outcome_type changes AND when outcome_name first appears
  # (i.e., when Step 3 UI is rendered).
  shiny::observe({
    cur <- input$outcome_name
    if (is.null(cur)) return()
    ot  <- input$outcome_type %||% "binary"
    expected <- if (identical(ot, "binary"))
                  "Depression response" else "Depression severity"
    if (cur %in% c("", "Outcome", "Depression response", "Depression severity") &&
        !identical(cur, expected)) {
      shiny::updateTextInput(session, "outcome_name", value = expected)
    }
  })

  # ----- Per-study RoB / Indirectness editors (synced with Step 1) -----
  .step3_bulk_set <- function(col, value) {
    d <- state$rob_table
    if (is.null(d)) return()
    d[[col]] <- value
    state$rob_table <- d
  }

  shiny::observeEvent(input$step3_rob_set_low,    { .step3_bulk_set("rob", "low")  })
  shiny::observeEvent(input$step3_rob_set_some,   { .step3_bulk_set("rob", "some") })
  shiny::observeEvent(input$step3_rob_set_high,   { .step3_bulk_set("rob", "high") })
  shiny::observeEvent(input$step3_rob_clear,      { .step3_bulk_set("rob", NA_character_) })
  shiny::observeEvent(input$step3_indir_set_low,  { .step3_bulk_set("indirectness", "low")  })
  shiny::observeEvent(input$step3_indir_set_some, { .step3_bulk_set("indirectness", "some") })
  shiny::observeEvent(input$step3_indir_set_high, { .step3_bulk_set("indirectness", "high") })
  shiny::observeEvent(input$step3_indir_clear,    { .step3_bulk_set("indirectness", NA_character_) })

  output$step3_rob_editor <- DT::renderDT({
    d <- state$rob_table
    if (is.null(d)) {
      return(DT::datatable(data.frame(message = "Load data in Step 1 first."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(
      d[, c("studlab", "rob"), drop = FALSE],
      editable = list(target = "cell", disable = list(columns = 0)),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$step3_indir_editor <- DT::renderDT({
    d <- state$rob_table
    if (is.null(d)) {
      return(DT::datatable(data.frame(message = "Load data in Step 1 first."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(
      d[, c("studlab", "indirectness"), drop = FALSE],
      editable = list(target = "cell", disable = list(columns = 0)),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
  })

  .step3_validate_value <- function(val) {
    if (is.na(val) || !nzchar(val)) return(NA_character_)
    val <- tolower(trimws(val))
    if (!val %in% c("low", "some", "high")) {
      shiny::showNotification(
        "Value must be 'low', 'some', or 'high' (case-insensitive).",
        type = "warning"
      )
      return(NULL)
    }
    val
  }

  shiny::observeEvent(input$step3_rob_editor_cell_edit, {
    info <- input$step3_rob_editor_cell_edit
    if (is.null(info)) return()
    d <- state$rob_table
    if (is.null(d)) return()
    val <- .step3_validate_value(as.character(info$value))
    if (is.null(val)) return()
    d$rob[info$row] <- val
    state$rob_table <- d
  })

  shiny::observeEvent(input$step3_indir_editor_cell_edit, {
    info <- input$step3_indir_editor_cell_edit
    if (is.null(info)) return()
    d <- state$rob_table
    if (is.null(d)) return()
    val <- .step3_validate_value(as.character(info$value))
    if (is.null(val)) return()
    d$indirectness[info$row] <- val
    state$rob_table <- d
  })

  # Sync state$rob_table back into state$data so grade_meta picks up edits
  # made on Step 3 immediately (without re-running Step 1 commit).
  shiny::observe({
    rt <- state$rob_table
    d  <- state$data
    if (is.null(rt) || is.null(d)) return()
    shiny::isolate({
      idx <- match(as.character(d$studlab), as.character(rt$studlab))
      changed <- FALSE
      if (any(!is.na(rt$rob))) {
        new_rob <- rt$rob[idx]
        cur_rob <- d$rob %||% rep(NA_character_, nrow(d))
        if (!identical(as.character(cur_rob), new_rob)) {
          d$rob <- new_rob
          changed <- TRUE
        }
      }
      if (any(!is.na(rt$indirectness))) {
        new_indir <- rt$indirectness[idx]
        cur_indir <- d$indirectness %||% rep(NA_character_, nrow(d))
        if (!identical(as.character(cur_indir), new_indir)) {
          d$indirectness <- new_indir
          changed <- TRUE
        }
      }
      if (changed) state$data <- d
    })
  })

  state$step3_commit <- function() TRUE
}
