# step3_grade.R - Step 3: GRADE 5-domain assessment + Final certainty (sub-tabs)

# Map a suggest_threshold() return onto the two threshold reactiveVals used by
# the Decision threshold tab.
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
  # Rationale textarea shown only while the paired override select has a
  # non-empty value. A written rationale is mandatory for manual overrides
  # (pmatools v0.4.0 breaking change; Core GRADE transparency principle).
  .override_rationale <- function(select_id, rationale_id) {
    shiny::conditionalPanel(
      sprintf("(input['%s'] || '') != ''", select_id),
      shiny::textAreaInput(
        rationale_id,
        "Rationale (required for override)",
        rows = 2, width = "100%",
        placeholder = "State why the automated assessment was replaced."
      )
    )
  }
  # Explicit per-domain confirmation checkbox (output gate W4-A). Checking
  # it marks the domain as reviewed even if no substantive input was given.
  .confirm_checkbox <- function(id,
                                label = paste0(
                                  "I have reviewed this domain ",
                                  "(it may remain unassessed / at its default)")) {
    htmltools::div(
      style = paste(
        "margin-top: 1rem; padding: 0.5rem 0.75rem;",
        "border: 1px dashed hsl(var(--border)); border-radius: 6px;"),
      shiny::checkboxInput(id, label, value = FALSE, width = "100%")
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
        class = "pma-display-grid",
        htmltools::div(class = "pma-span-4",
          shiny::textInput(paste0(prefix, "_title"), "Title", value = "", width = "100%")),
        shiny::textInput(paste0(prefix, "_label_e"),  "Intervention label", value = "", width = "100%"),
        shiny::textInput(paste0(prefix, "_label_c"),  "Control label",      value = "", width = "100%"),
        shiny::textInput(paste0(prefix, "_favors_left"),  "Favors (left)",  placeholder = "e.g., Favors Control", width = "100%"),
        shiny::textInput(paste0(prefix, "_favors_right"), "Favors (right)", placeholder = "e.g., Favors CBT-I",   width = "100%"),
        shiny::numericInput(paste0(prefix, "_xlim_lo"), "x-min", value = NA, width = "100%"),
        shiny::numericInput(paste0(prefix, "_xlim_hi"), "x-max", value = NA, width = "100%"),
        # Blank rows around the pooled result. Always visible (they used to be
        # wrapped in conditionalPanels that only revealed them when both column
        # checkboxes were off, which conditionalPanel implemented as a display
        # toggle anyway). Defaults mirror the Step 2 panel and are asymmetric
        # on purpose: above = 1 is the blank row meta::forest() draws by
        # default, while a blank "below" keeps plot_forest()'s automatic
        # derivation that holds the heterogeneity line clear of the x-axis.
        htmltools::p(class = "pma-card-subtitle pma-span-4",
          paste0("Blank rows around the pooled result. If the heterogeneity ",
                 "text overlaps the x-axis - most likely once the per-arm ",
                 "columns are hidden - use these to move it up or down. ",
                 "Above: 0 removes the blank row before the pooled result. ",
                 "Below: blank = automatic.")),
        shiny::numericInput(paste0(prefix, "_addrows_above"),
                            "Blank rows above pooled result",
                            value = 1, min = 0, step = 1, width = "100%"),
        shiny::numericInput(paste0(prefix, "_addrows_below"),
                            "Blank rows below pooled result",
                            value = NA, min = 0, step = 1, width = "100%"),
        # One checkbox for both per-arm column groups: plot_forest() keeps
        # show_n and show_events separate (correct for a library), but no user
        # wants the N columns without the event / mean-and-SD columns.
        htmltools::div(class = "pma-span-4",
          shiny::checkboxInput(paste0(prefix, "_show_arm_columns"),
                               paste0("Show per-arm data columns ",
                                      "(events or mean & SD, and N)"),
                               TRUE))
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

    # Which studies the numbers on this step came from. Deliberately its own
    # top-level output, sibling to (not nested in) the sticky certainty bar,
    # so it survives that bar's removal untouched: one uiOutput here, one
    # renderer below, no shared state. Renders nothing when the analysis
    # rests on all studies, and the bare uiOutput wrapper is unstyled, so the
    # "all studies" case adds no box and no whitespace.
    shiny::uiOutput("analysis_set_indicator"),

    pma_card(
      title = "Certainty assessment (Core GRADE series)",
      shiny::tabsetPanel(
        id = "grade_tabs",

        # --- Decision threshold (cross-cutting; set once, used by RoB /
        #     Inconsistency / Imprecision) ---
        shiny::tabPanel("Decision threshold",
          htmltools::h4("Decision threshold",
                        style = "margin: 0 0 0.5rem; font-size: 1.1rem;"),
          htmltools::p(class = "pma-card-subtitle",
            EDU_COPY$threshold_tab$intro),
          shiny::uiOutput("threshold_panel"),
          .confirm_checkbox("threshold_confirm",
            paste0("I have reviewed and confirm this decision threshold ",
                   "(required before export; the default value is fine ",
                   "if you agree with it)")),
          .grade_nav("grade_back_thresh", "Back: Meta-analysis",
                     "grade_next_thresh", "Next: Risk of Bias")
        ),

        # --- Risk of Bias ---
        shiny::tabPanel("Risk of Bias",
          .domain_header("Risk of Bias", "rob_badge", "rob_chip"),
          shiny::uiOutput("analysis_set_banner_rob"),
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
            shiny::sliderInput("rob_inf_threshold",
              "Sensitivity-analysis change threshold (RoB-specific)",
              min = 0.05, max = 0.5, value = 0.10, step = 0.05)
          ),
          .override_details(
            shiny::selectInput("rob_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious")),
            .override_rationale("rob_override", "rob_override_rationale")
          ),
          .confirm_checkbox("rob_confirm_na"),
          .grade_nav("grade_back_rob", "Back: Decision threshold",
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
                          "Serious" = "serious")),
            .override_rationale("incon_override", "incon_override_rationale")
          ),
          .confirm_checkbox("incon_confirm_na"),
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
          # No preselected value (W4-A): the user must actively choose a
          # rating. grade_meta() receives "no" while unselected, but the
          # domain does not count as confirmed until a choice is made.
          shiny::radioButtons("indirectness", "Overall indirectness rating",
            choices = c("No" = "no",
                        "Some concerns" = "some_concerns",
                        "Serious" = "serious"),
            selected = character(0), inline = TRUE),
          shiny::conditionalPanel(
            "input.indirectness == 'some_concerns' || input.indirectness == 'serious'",
            shiny::textAreaInput(
              "indir_rationale",
              "Rationale (required for any rating other than 'No')",
              rows = 2, width = "100%",
              placeholder = paste0(
                "State which aspect (population / intervention / comparator ",
                "/ outcome) raises concern and why.")
            )
          ),
          htmltools::tags$details(
            class = "pma-edit-details",
            htmltools::tags$summary(
              class = "pma-edit-summary",
              htmltools::HTML(
                "&#9998;&nbsp; <strong>Edit per-study Indirectness</strong> ",
                "<span class='pma-edit-hint'>(click to expand &middot; ",
                "optional per-study notes; the overall rating above is what feeds Core GRADE)</span>"
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
          .confirm_checkbox("indir_confirm_na"),
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
                          "Serious" = "serious")),
            .override_rationale("impre_override", "impre_override_rationale")
          ),
          .confirm_checkbox("impre_confirm_na"),
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
              "Core GRADE algorithm</strong>, but you are encouraged to consider ",
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
                          "Serious" = "serious")),
            .override_rationale("pubias_override", "pubias_override_rationale")
          ),
          .confirm_checkbox("pubias_confirm_na"),
          .grade_nav("grade_back_pubias", "Back: Imprecision",
                     "grade_next_pubias", "Next: Final certainty")
        ),

        # --- Final certainty (7th tab) ---
        shiny::tabPanel("Final certainty",
          shiny::uiOutput("cert_incomplete_banner"),
          shiny::uiOutput("analysis_set_banner_cert"),
          htmltools::h5("Core GRADE Evidence Profile"),
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
            "Recommended: report both control event rate (CER) and intervention event rate (EER) ",
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
            htmltools::div(style = "grid-column: span 2;",
              shiny::uiOutput("outcome_name_echo")),
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
          htmltools::hr(),

          # ----- Save this outcome for the multi-outcome SoF table -----
          # Sits at the end of the Final certainty tab: this is the point in
          # the wizard where the rating for one outcome is complete, and the
          # natural place to bank it before going back to Step 2 for the
          # next outcome.
          htmltools::h5("Saved outcomes for the Summary of Findings table"),
          htmltools::p(class = "pma-card-subtitle",
                       EDU_COPY$multi_outcome$save_intro),
          shiny::uiOutput("save_outcome_panel"),
          shiny::uiOutput("saved_outcomes_list"),

          .grade_nav("grade_back_final", "Back: Publication bias",
                     "grade_next_final", "Next: Export")
        )
      )
    )
  )
}

step3_server <- function(input, output, session, state) {

  grade_tab_sequence <- c(
    "Decision threshold",
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

  shiny::observeEvent(input$grade_back_thresh, {
    retreat_grade_tab("Decision threshold")
  }, ignoreInit = TRUE)
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
  shiny::observeEvent(input$grade_next_thresh, {
    advance_grade_tab("Decision threshold")
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
  # ----- Threshold state (single source of truth: Decision threshold tab;
  # independent of UI render so Final certainty doesn't flip as the user
  # clicks tabs) -----------------------------------------------------------
  # threshold_state          : relative (ratio) or te-scale value
  # threshold_mode_state     : "absolute" / "relative" (binary ratio SMs only)
  # threshold_abs_state      : absolute threshold, events per 1,000
  # threshold_baseline_state : baseline (control-group) risk, per 1,000
  threshold_state          <- shiny::reactiveVal(NA_real_)
  threshold_mode_state     <- shiny::reactiveVal("absolute")
  threshold_abs_state      <- shiny::reactiveVal(NA_real_)
  threshold_baseline_state <- shiny::reactiveVal(NA_real_)

  # Initialise defaults from suggest_threshold() / pooled CER as soon as
  # state$ma is available. These observers live outside renderUI on purpose:
  # renderUI is suspended while the tab is hidden, so grade_obj() would
  # otherwise see NA thresholds until the user first opened the tab.
  shiny::observe({
    obj <- state$ma
    if (is.null(obj)) return()
    if (!is.na(threshold_state()) && !is.na(threshold_abs_state())) return()
    s <- tryCatch(suggest_threshold(obj), error = function(e) NULL)
    sug <- step3_threshold_suggestions(s)
    # Only ever prefill a reactiveVal that is still NA: a value the user typed
    # must never be overwritten.
    if (is.na(threshold_state()) && !is.na(sug$relative)) {
      threshold_state(round(sug$relative, 4))
    }
    if (is.na(threshold_abs_state()) && !is.na(sug$absolute1000)) {
      threshold_abs_state(round(sug$absolute1000, 1))
    }
  })
  shiny::observe({
    obj <- state$ma
    if (is.null(obj)) return()
    if (is.na(threshold_baseline_state())) {
      ec <- obj$event.c; nc <- obj$n.c
      if (!is.null(ec) && !is.null(nc) && sum(nc, na.rm = TRUE) > 0) {
        cer <- sum(ec, na.rm = TRUE) / sum(nc, na.rm = TRUE)
        if (is.finite(cer) && cer > 0 && cer < 1) {
          threshold_baseline_state(round(1000 * cer, 1))
        }
      }
    }
  })

  # Mirror Decision-threshold-tab inputs into the reactiveVals.
  shiny::observeEvent(input$threshold_mode, {
    if (nzchar(input$threshold_mode %||% "")) {
      threshold_mode_state(input$threshold_mode)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$threshold_abs, {
    v <- input$threshold_abs
    if (!is.null(v) && length(v) == 1 && !is.na(v)) threshold_abs_state(v)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$threshold_baseline_input, {
    v <- input$threshold_baseline_input
    if (!is.null(v) && length(v) == 1 && !is.na(v)) threshold_baseline_state(v)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$threshold_ratio, {
    v <- input$threshold_ratio
    if (!is.null(v) && length(v) == 1 && !is.na(v)) threshold_state(v)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$threshold_cont, {
    v <- input$threshold_cont
    if (!is.null(v) && length(v) == 1 && !is.na(v)) threshold_state(v)
  }, ignoreInit = TRUE)

  # Equivalent ratio for an absolute (per 1,000) threshold at a given
  # baseline risk (mirrors vendored .ard_threshold_to_ratio maths).
  .ard_equiv_ratio <- function(sm, abs1000, base1000) {
    if (!is.finite(abs1000) || !is.finite(base1000)) return(NULL)
    p0 <- base1000 / 1000
    p1 <- p0 + abs1000 / 1000
    if (p0 <= 0 || p0 >= 1 || p1 >= 1 || abs1000 <= 0) return(NULL)
    if (identical(sm, "OR")) (p1 / (1 - p1)) / (p0 / (1 - p0)) else p1 / p0
  }

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

  # ----- Decision threshold tab: centralized input panel -----------------
  output$threshold_panel <- shiny::renderUI({
    obj <- state$ma
    ot  <- input$outcome_type
    if (is.null(obj)) {
      return(htmltools::p("Run the analysis in Step 2 first."))
    }
    sm <- obj$sm %||% "OR"

    if (identical(ot, "binary") && sm %in% c("OR", "RR")) {
      htmltools::tagList(
        shiny::radioButtons("threshold_mode", "Threshold scale",
          choices = c(
            "Absolute (per 1,000 patients) - recommended" = "absolute",
            "Relative (ratio)"                            = "relative"),
          selected = shiny::isolate(threshold_mode_state())),
        shiny::conditionalPanel(
          "input.threshold_mode == 'absolute'",
          htmltools::p(class = "pma-card-subtitle",
            paste0(
              "Core GRADE recommends expressing the threshold on the ",
              "absolute scale: the smallest difference in events per 1,000 ",
              "patients that would matter for a decision. It is converted ",
              "to the ", sm, " scale at the baseline risk below.")),
          shiny::numericInput("threshold_abs",
            "Threshold (events per 1,000 patients)",
            value = shiny::isolate(threshold_abs_state()),
            min = 0, step = 5),
          shiny::numericInput("threshold_baseline_input",
            "Baseline (control-group) risk (per 1,000 patients)",
            value = shiny::isolate(threshold_baseline_state()),
            min = 0, max = 1000, step = 5),
          htmltools::p(class = "pma-card-subtitle",
            "Baseline risk is prefilled from the pooled control-group ",
            "event rate of your data; replace it with a better estimate ",
            "for your target population if you have one."),
          shiny::uiOutput("threshold_equiv")
        ),
        shiny::conditionalPanel(
          "input.threshold_mode == 'relative'",
          shiny::numericInput("threshold_ratio",
            EDU_COPY$threshold_labels[[sm]] %||%
              "Threshold for clinical importance",
            value = shiny::isolate({
              v <- threshold_state()
              if (is.na(v)) NA else v
            }),
            min = 0, step = 0.01),
          htmltools::p(class = "pma-card-subtitle",
                       EDU_COPY$threshold_help[[sm]] %||% "")
        ),
        .mic_note()
      )
    } else {
      htmltools::tagList(
        shiny::numericInput("threshold_cont",
          EDU_COPY$threshold_labels[[sm]] %||%
            "Threshold for clinical importance",
          value = shiny::isolate({
            v <- threshold_state()
            if (is.na(v)) NA else v
          }),
          min = 0, step = 0.01),
        htmltools::p(class = "pma-card-subtitle",
                     EDU_COPY$threshold_help[[sm]] %||% ""),
        .mic_note()
      )
    }
  })
  shiny::outputOptions(output, "threshold_panel", suspendWhenHidden = FALSE)

  # Live equivalent-ratio display for the absolute mode.
  output$threshold_equiv <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    sm <- obj$sm %||% "OR"
    ta <- input$threshold_abs           %||% threshold_abs_state()
    tb <- input$threshold_baseline_input %||% threshold_baseline_state()
    eq <- .ard_equiv_ratio(sm, ta, tb)
    if (is.null(eq)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "Enter a positive threshold and a baseline risk between 0 and ",
        "1,000 (threshold + baseline must stay below 1,000) to see the ",
        "equivalent relative effect."))
    }
    p0 <- tb / 1000; p1 <- p0 + ta / 1000
    or_eq <- (p1 / (1 - p1)) / (p0 / (1 - p0))
    rr_eq <- p1 / p0
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f5f5f5; ",
        "border-left: 4px solid #0f172a; margin: 0.5rem 0; ",
        "font-size: 0.85rem;"),
      htmltools::strong(sprintf("Equivalent %s = %.2f", sm, eq)),
      htmltools::span(sprintf(
        " (at baseline %g per 1,000: RR %.2f, OR %.2f)", tb, rr_eq, or_eq))
    )
  })
  shiny::outputOptions(output, "threshold_equiv", suspendWhenHidden = FALSE)

  # Human-readable summary of the active threshold (for read-only blocks).
  threshold_summary_text <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return("No threshold set - run the analysis first.")
    sm <- obj$sm %||% "OR"
    if (identical(input$outcome_type, "binary") && sm %in% c("OR", "RR") &&
        identical(threshold_mode_state(), "absolute")) {
      ta <- threshold_abs_state()
      tb <- threshold_baseline_state()
      if (!is.finite(ta) || ta <= 0) {
        return("Absolute threshold not set yet.")
      }
      eq <- .ard_equiv_ratio(sm, ta, tb)
      if (is.null(eq)) {
        return(sprintf(
          "Absolute threshold: %g per 1,000 (baseline risk missing/invalid)",
          ta))
      }
      return(sprintf(
        "Absolute threshold: %g per 1,000 at baseline %g per 1,000 (equivalent %s %.2f)",
        ta, tb, sm, eq))
    }
    th <- threshold_state()
    if (!is.finite(th)) return("Threshold not set yet.")
    sprintf("Threshold: %s = %g", sm, th)
  })

  # Read-only threshold display inside RoB / Inconsistency / Imprecision.
  .render_threshold_readonly <- function() {
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f9f9f9; ",
        "border: 1px solid #e5e5e5; border-radius: 6px; margin: 0.5rem 0;"),
      htmltools::p(style = "margin: 0; font-size: 0.9rem;",
        htmltools::strong(threshold_summary_text())),
      htmltools::p(
        class = "pma-card-subtitle",
        style = "margin: 0.25rem 0 0;",
        "This decision threshold is shared by Risk of Bias, Inconsistency, ",
        "and Imprecision. Change it in the 'Decision threshold' tab.")
    )
  }
  output$threshold_block_rob   <- shiny::renderUI(.render_threshold_readonly())
  output$threshold_block_inco  <- shiny::renderUI(.render_threshold_readonly())
  output$threshold_block_impre <- shiny::renderUI(.render_threshold_readonly())
  shiny::outputOptions(output, "threshold_block_rob",   suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_inco",  suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_impre", suspendWhenHidden = FALSE)

  # grade_meta() threshold arguments derived from the active mode.
  .threshold_grade_args <- function(obj) {
    sm <- obj$sm %||% "OR"
    if (identical(shiny::isolate(input$outcome_type), "binary") &&
        sm %in% c("OR", "RR") &&
        identical(threshold_mode_state(), "absolute")) {
      ta <- threshold_abs_state()
      tb <- threshold_baseline_state()
      if (is.finite(ta) && ta > 0) {
        base <- if (is.finite(tb) && tb > 0 && tb < 1000 &&
                    (tb + ta) < 1000) tb / 1000 else NULL
        return(list(threshold          = ta / 1000,
                    threshold_scale    = "ard",
                    threshold_baseline = base))
      }
      return(list(threshold = NULL, threshold_scale = "auto",
                  threshold_baseline = NULL))
    }
    th <- threshold_state()
    list(
      threshold = if (is.numeric(th) && !is.na(th) && th > 0) th else NULL,
      threshold_scale    = "auto",
      threshold_baseline = NULL
    )
  }

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
    # An empty numericInput arrives as logical NA, not NA_real_, so test for
    # NA before looking at the type: is.numeric(NA) is FALSE and a type-first
    # check let empty OIS fields count as user input (W4-A gate) and reach
    # grade_meta() as NA instead of NULL.
    if (all(is.na(x))) return(NULL)
    if (is.character(x) && !any(nzchar(x))) return(NULL)
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

  # Non-empty scalar select value or NULL.
  .sel_val <- function(id) {
    v <- input[[id]]
    if (is.null(v) || length(v) == 0 || !nzchar(v)) NULL else v
  }
  # Trimmed non-empty rationale text or NULL.
  .rat_val <- function(id) {
    v <- input[[id]]
    if (is.null(v) || length(v) == 0) return(NULL)
    v <- trimws(as.character(v)[1])
    if (nzchar(v)) v else NULL
  }
  # Resolve an override select + its mandatory rationale. Returns
  # list(value=, rationale=); value is NULL (override ignored, with a
  # notification) while the rationale is missing, so grade_meta() can
  # never see a rationale-less override (v0.4.0 would abort).
  .override_or_ignore <- function(sel_id, rat_id, domain_label) {
    sel <- .sel_val(sel_id)
    if (is.null(sel)) return(list(value = NULL, rationale = NULL))
    rat <- .rat_val(rat_id)
    if (is.null(rat)) {
      shiny::showNotification(
        sprintf(paste0("%s: override ignored - a written rationale is ",
                       "required for manual overrides."), domain_label),
        id = paste0(sel_id, "_rationale_missing"),
        type = "warning", duration = 6)
      return(list(value = NULL, rationale = NULL))
    }
    list(value = sel, rationale = rat)
  }

  grade_obj <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(NULL)

    # --- Risk of bias: per-study vector, or scalar override + rationale ---
    rob_arg <- .study_covariate(.study_labels_for_grade(obj), "rob", default = "*")
    rob_rationale <- NULL
    rob_ov <- .override_or_ignore("rob_override", "rob_override_rationale",
                                  "Risk of Bias")
    if (!is.null(rob_ov$value)) {
      rob_arg       <- rob_ov$value
      rob_rationale <- rob_ov$rationale
    }

    # Outcome direction is a required Step 2 answer, mirrored into state.
    sv <- state$small_values

    # --- Inconsistency: scalar override + rationale, or manual flowchart ---
    incon_ov <- .override_or_ignore("incon_override",
                                    "incon_override_rationale",
                                    "Inconsistency")
    ci_diff <- if (nzchar(input$ci_diff %||% "")) input$ci_diff else NULL
    threshold_side <- if (!is.null(input$threshold_side) &&
                          length(input$threshold_side) > 0 &&
                          nzchar(input$threshold_side)) input$threshold_side else NULL
    subgroup_expl <- if (!is.null(input$subgroup_explained) &&
                         length(input$subgroup_explained) > 0 &&
                         nzchar(input$subgroup_explained)) input$subgroup_explained else NULL

    # --- Indirectness: active selection; non-"no" needs a rationale.
    # While unselected (or rationale missing) grade_meta() receives the
    # safe default "no" - the confirmation gate (not an error) is what
    # tells the user the domain is still unassessed.
    indir_arg       <- "no"
    indir_rationale <- NULL
    indir_sel <- input$indirectness
    if (!is.null(indir_sel) && length(indir_sel) == 1 && nzchar(indir_sel) &&
        !identical(indir_sel, "no")) {
      r <- .rat_val("indir_rationale")
      if (is.null(r)) {
        shiny::showNotification(
          paste0("Indirectness: rating ignored - a written rationale is ",
                 "required for any rating other than 'No'."),
          id = "indir_rationale_missing", type = "warning", duration = 6)
      } else {
        indir_arg       <- indir_sel
        indir_rationale <- r
      }
    }

    # --- Imprecision: scalar override + rationale (vendored v0.4.0 API) ---
    impre_ov <- .override_or_ignore("impre_override",
                                    "impre_override_rationale",
                                    "Imprecision")

    # --- Publication bias ---
    pubias_si <- if (nzchar(input$pubias_small_industry %||% "")) input$pubias_small_industry else NULL
    pubias_un <- if (nzchar(input$pubias_unpublished %||% "")) input$pubias_unpublished else NULL
    pubias_rc <- if (nzchar(input$pubias_registry_complete %||% "")) input$pubias_registry_complete else NULL
    # Visual override of Egger's test: v0.4.0 requires pubias_rationale
    # whenever pubias_funnel_asymmetry is supplied.
    pubias_fa       <- NULL
    pubias_rationale <- NULL
    fa_ov <- .override_or_ignore("pubias_funnel_asymmetry",
                                 "pubias_fa_rationale",
                                 "Publication bias (visual override of Egger)")
    if (!is.null(fa_ov$value)) {
      pubias_fa        <- fa_ov$value
      pubias_rationale <- fa_ov$rationale
    }
    # Final scalar override (app-level; grade_meta has no scalar
    # publication-bias override parameter).
    pubias_ov_res <- .override_or_ignore("pubias_override",
                                         "pubias_override_rationale",
                                         "Publication bias")
    pubias_ov <- pubias_ov_res$value

    th_args <- .threshold_grade_args(obj)

    args <- list(
      meta_obj                 = obj,
      study_design             = "RCT",
      rob                      = rob_arg,
      rob_rationale            = rob_rationale,
      rob_inflation_threshold  = input$rob_inf_threshold %||% 0.10,
      small_values             = sv,
      indirectness             = indir_arg,
      indirectness_rationale   = indir_rationale,
      inconsistency            = incon_ov$value,
      inconsistency_rationale  = incon_ov$rationale,
      inconsistency_ci_diff            = ci_diff,
      inconsistency_threshold_side     = threshold_side,
      inconsistency_subgroup_explained = subgroup_expl,
      imprecision              = impre_ov$value,
      imprecision_rationale    = impre_ov$rationale,
      threshold          = th_args$threshold,
      threshold_scale    = th_args$threshold_scale,
      threshold_baseline = th_args$threshold_baseline,
      outcome_type = if (identical(input$outcome_type, "binary")) "relative" else "absolute",
      ois_p0       = .na_null(input$ois_p0),
      ois_sd       = .na_null(input$ois_sd),
      ois_events   = .na_null(input$ois_events_override),
      ois_n        = .na_null(input$ois_n_override),
      pubias_small_industry    = pubias_si,
      pubias_funnel_asymmetry  = pubias_fa,
      pubias_rationale         = pubias_rationale,
      pubias_unpublished       = pubias_un,
      # Q1: only "yes" (denied) is forwarded to the package short-circuit;
      # "no" (suspected) is handled by a post-override below so it forces
      # rate-down regardless of Q2-Q5.
      pubias_registry_complete = if (identical(pubias_rc, "yes")) "yes" else NULL,
      outcome_name = state$outcome_name %||% "Outcome"
    )

    # TEMPORARY BRIDGE (pmatools >= 0.5). grade_meta() now defaults to
    # threshold_type = "mid" with require_threshold = TRUE, so a NULL threshold
    # aborts the call. The app can still be in that state (no absolute
    # threshold entered yet, or suggest_threshold() has not fired because
    # state$ma was NULL), so opt out of the gate for exactly those cases and
    # degrade to the pre-0.5 behaviour instead of erroring. When a threshold
    # IS present the package default (TRUE) is left alone.
    # Follow-up: the Configuration-tab rework makes the decision threshold a
    # required, confirmed input; this flag is removed then.
    if (is.null(th_args$threshold)) {
      args$require_threshold <- FALSE
    }

    g <- tryCatch(
      suppressWarnings(do.call(grade_meta, args)),
      error = function(e) {
        shiny::showNotification(paste("grade_meta error:", conditionMessage(e)),
                                type = "error")
        NULL
      }
    )

    if (!is.null(g)) {
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
      # Final scalar publication-bias override (app-level; recorded in the
      # notes in the same "Manual override (<judgment>): <rationale>"
      # format the vendored make_domain_row() uses).
      if (!is.null(pubias_ov)) {
        idx <- which(g$domain_assessments$domain == "Publication bias")
        if (length(idx)) {
          g$domain_assessments$judgment[idx] <- pubias_ov
          g$domain_assessments$auto[idx]     <- FALSE
          g$domain_assessments$downgrade[idx] <- pmatools_GRADE_DOWNGRADE(pubias_ov)
          g$domain_assessments$notes[idx] <- paste0(
            sprintf("Manual override (%s): %s", pubias_ov,
                    pubias_ov_res$rationale),
            " | ", g$domain_assessments$notes[idx])
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

  # ----- W4-A: per-domain confirmation state (output gate) ----------------
  # A domain counts as confirmed when it has substantive user input, or
  # when its explicit "I have reviewed this domain" checkbox is ticked.
  # Progression through tabs stays free; only outputs are gated.
  .valid_override <- function(sel_id, rat_id) {
    sel <- input[[sel_id]]
    rat <- input[[rat_id]]
    !is.null(sel) && length(sel) == 1 && nzchar(sel) &&
      !is.null(rat) && nzchar(trimws(rat))
  }
  .answered <- function(id) {
    v <- input[[id]]
    !is.null(v) && length(v) > 0 && nzchar(v[1])
  }

  domain_confirmed <- shiny::reactive({
    rt <- state$rob_table
    rob_data <- !is.null(rt) && "rob" %in% names(rt) &&
      any(!is.na(rt$rob) & nzchar(trimws(as.character(rt$rob))))

    indir_sel <- input$indirectness
    indir_active <- !is.null(indir_sel) && length(indir_sel) == 1 &&
      nzchar(indir_sel) &&
      (identical(indir_sel, "no") ||
         nzchar(trimws(input$indir_rationale %||% "")))

    c(
      threshold = isTRUE(input$threshold_confirm),
      rob = rob_data ||
        .valid_override("rob_override", "rob_override_rationale") ||
        isTRUE(input$rob_confirm_na),
      inconsistency = .answered("ci_diff") ||
        .valid_override("incon_override", "incon_override_rationale") ||
        isTRUE(input$incon_confirm_na),
      indirectness = indir_active || isTRUE(input$indir_confirm_na),
      imprecision = !is.null(.na_null(input$ois_events_override)) ||
        !is.null(.na_null(input$ois_n_override)) ||
        .valid_override("impre_override", "impre_override_rationale") ||
        isTRUE(input$impre_confirm_na),
      pubias = .answered("pubias_registry_complete") ||
        .answered("pubias_small_industry") ||
        .answered("pubias_unpublished") ||
        .valid_override("pubias_funnel_asymmetry", "pubias_fa_rationale") ||
        .valid_override("pubias_override", "pubias_override_rationale") ||
        isTRUE(input$pubias_confirm_na)
    )
  })

  # Mirror into state so Step 4 (export gate) can read it.
  shiny::observe({
    state$domain_confirmed <- domain_confirmed()
  })

  # Banner on the Final certainty tab while domains remain unconfirmed.
  output$cert_incomplete_banner <- shiny::renderUI({
    unconf <- pma_unconfirmed_domains(domain_confirmed())
    if (!length(unconf)) return(NULL)
    htmltools::div(
      style = paste0(
        "padding: 0.75rem 1rem; margin-bottom: 1rem; ",
        "background: #fef3c7; border-left: 4px solid #b45309; ",
        "border-radius: 4px; font-size: 0.9rem;"),
      htmltools::strong("Assessment incomplete. "),
      sprintf(paste0(
        "The certainty shown below is provisional until every domain has ",
        "been reviewed. Unconfirmed: %s. "), paste(unconf, collapse = ", ")),
      "Provide inputs in each tab, or tick 'I have reviewed this domain' ",
      "to confirm it as-is. Export (Step 4) stays locked until then."
    )
  })
  shiny::outputOptions(output, "cert_incomplete_banner",
                       suspendWhenHidden = FALSE)

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

  # ----- Risk-of-bias analysis set (Core GRADE 4 Fig 2) -------------------
  # Display only: these three outputs read the rated object and change no
  # judgment, no number and no rating. They exist because grade_meta() can
  # refit the analysis on the low risk-of-bias subset and only says so via
  # an R-level message() that never reaches the browser.
  output$analysis_set_indicator <- shiny::renderUI(
    pma_analysis_set_indicator(grade_obj()))
  shiny::outputOptions(output, "analysis_set_indicator",
                       suspendWhenHidden = FALSE)

  output$analysis_set_banner_rob <- shiny::renderUI(
    pma_analysis_set_banner(grade_obj()))
  output$analysis_set_banner_cert <- shiny::renderUI(
    pma_analysis_set_banner(grade_obj()))

  output$sticky_cert_badge <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) return(htmltools::span("--"))
    unconf <- pma_unconfirmed_domains(domain_confirmed())
    htmltools::tagList(
      pma_certainty_badge(g$certainty),
      if (length(unconf)) {
        htmltools::span(
          style = paste0("color: #b45309; font-weight: 600; ",
                         "font-size: 0.8rem; margin-left: 0.4rem;"),
          title = paste0("Unconfirmed: ", paste(unconf, collapse = ", ")),
          "(incomplete)")
      }
    )
  })

  output$sticky_cert_summary <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) return(htmltools::span(""))
    unconf <- pma_unconfirmed_domains(domain_confirmed())
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
      paste(parts, collapse = " | "),
      if (length(unconf)) {
        htmltools::span(
          style = "color: #b45309;",
          paste0(" -- unconfirmed: ", paste(unconf, collapse = ", ")))
      }
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
    addrow_ids <- pma_forest_addrow_ids(prefix)
    list(
      title        = pick_text(paste0(prefix, "_title")),
      label_e      = pick_text(paste0(prefix, "_label_e")),
      label_c      = pick_text(paste0(prefix, "_label_c")),
      favors_left  = pick_text(paste0(prefix, "_favors_left")),
      favors_right = pick_text(paste0(prefix, "_favors_right")),
      xlim         = xlim,
      # An absent checkbox means the tab has not been rendered yet; fall back
      # to the UI default (TRUE) rather than to isTRUE(NULL). One checkbox
      # feeds both keys - plot_forest()'s two arguments are kept as they are.
      show_n       = isTRUE(input[[paste0(prefix, "_show_arm_columns")]] %||% TRUE),
      show_events  = isTRUE(input[[paste0(prefix, "_show_arm_columns")]] %||% TRUE),
      addrow_above = pma_addrow_above(input[[addrow_ids[["above"]]]]),
      addrow_below = pma_addrow_below(input[[addrow_ids[["below"]]]])
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
        shiny::conditionalPanel(
          "(input.pubias_funnel_asymmetry || '') != ''",
          shiny::textAreaInput(
            "pubias_fa_rationale",
            "Rationale (required for the visual override)",
            rows = 2, width = "100%",
            placeholder = paste0(
              "State why your visual judgment replaces the automated ",
              "Egger's test."))
        ),

        # Reference materials: trim-and-fill funnel + summary text
        htmltools::hr(),
        htmltools::h5("Reference: trim-and-fill"),
        htmltools::p(class = "pma-card-subtitle",
          "Filled (imputed) studies appear alongside observed studies on the ",
          "funnel plot. The numerical summary below shows how the pooled ",
          "estimate would shift if these imputed studies actually existed. ",
          htmltools::HTML(paste0(
            "This information is <strong>not part of the automated Core GRADE ",
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

  # Read-only echo of the outcome name: it is owned by Step 2, so Step 3 shows
  # it (the SoF row label depends on it) without offering a second, divergent
  # field to edit.
  output$outcome_name_echo <- shiny::renderUI({
    nm <- state$outcome_name %||% "(not set)"
    htmltools::p(class = "pma-card-subtitle",
      "Outcome name: ", htmltools::tags$strong(nm),
      " - set in Step 2 (Model configuration).")
  })

  # Smart defaults for the four Step 3 forest display panels. Only state$ is
  # read: while the user is on Step 3, the Step 2 body (and therefore
  # input$experimental_label / input$control_label) does not exist.
  #
  # The title suffix mirrors what export_bundle.R writes for each stratified
  # plot, so the on-screen title and the exported one agree.
  .forest_label_defaults <- shiny::reactive({
    iv  <- state$arm_e %||% ""
    ct  <- state$arm_c %||% ""
    fav <- pma_favors_labels(state$small_values, iv, ct)
    list(title = state$outcome_name %||% "", label_e = iv, label_c = ct,
         favors_left = fav$left, favors_right = fav$right)
  })
  .forest_title_suffix <- c(
    rob    = " (stratified by Risk of Bias)",
    incon  = "",
    indir  = " (stratified by Indirectness)",
    pubias = " (available vs missing results)"
  )
  for (.pfx in names(.forest_title_suffix)) {
    # local() binds the prefix per iteration; without it all four panels would
    # be wired to the last prefix.
    local({
      p <- .pfx
      pma_autofill_forest_panel(input, session, prefix = p,
                                values_fn = .forest_label_defaults,
                                title_suffix = .forest_title_suffix[[p]])
    })
  }

  # ----- Saving the current outcome into state$outcomes -------------------
  # Key for the saved outcome: the Outcome name entered in Step 2. The label
  # is what grade_table() prints in the Outcome column, so keeping the two
  # identical avoids a second, divergent name field.
  .save_key <- shiny::reactive({
    nm <- trimws(state$outcome_name %||% "")
    if (nzchar(nm)) nm else "Outcome"
  })

  .save_blocked_reasons <- shiny::reactive({
    reasons <- character()
    if (is.null(state$ma)) {
      reasons <- c(reasons, "run the meta-analysis in Step 2")
    }
    if (is.null(grade_obj())) {
      reasons <- c(reasons, "produce a certainty rating")
    }
    unconf <- pma_unconfirmed_domains(domain_confirmed())
    if (length(unconf)) {
      reasons <- c(reasons, paste0("review and confirm: ",
                                   paste(unconf, collapse = ", ")))
    }
    reasons
  })

  output$save_outcome_panel <- shiny::renderUI({
    reasons <- .save_blocked_reasons()
    if (length(reasons)) {
      # Same locked-note treatment as the Step 4 download gate: an
      # unconfirmed assessment must not be banked into the SoF table.
      return(htmltools::div(
        class = "pma-card-subtitle",
        style = paste(
          "border: 1px dashed hsl(var(--border)); border-radius: 6px;",
          "padding: 0.75rem; margin-top: 0.5rem;"),
        htmltools::p(style = "margin: 0;",
          htmltools::strong("Saving locked - certainty assessment incomplete.")),
        htmltools::p(style = "margin: 0.25rem 0 0;",
          paste0("To save this outcome, ", paste(reasons, collapse = "; "), "."))
      ))
    }
    key <- .save_key()
    htmltools::div(
      style = "margin-top: 0.5rem;",
      shiny::actionButton(
        "save_outcome",
        sprintf("Save this outcome's assessment as \"%s\"", key),
        class = "btn btn-primary", style = "width: 100%;"),
      htmltools::p(
        class = "pma-card-subtitle",
        style = "margin-top: 0.4rem;",
        "Saved under the Outcome name set in Step 2 - change it there to ",
        "relabel the Summary of Findings row.")
    )
  })
  shiny::outputOptions(output, "save_outcome_panel", suspendWhenHidden = FALSE)

  # Signature of the dataset currently loaded in Step 1. Used both to stamp
  # newly saved outcomes and to flag already-saved ones that came from a
  # different dataset (see pma_dataset_signature()).
  .current_signature <- shiny::reactive(pma_dataset_signature(state$data))

  .store_outcome <- function(key, g) {
    outs <- pma_outcomes_list(state$outcomes)
    attr(g, "pma_saved_at") <- Sys.time()
    # Provenance stamp: which dataset this rating was made on.
    attr(g, PMA_DATASET_SIGNATURE_ATTR) <- pma_dataset_signature(state$data)
    outs[[key]] <- g
    state$outcomes <- outs
    shiny::showNotification(
      sprintf("Saved \"%s\" (%s certainty). %d outcome(s) ready for the combined Summary of Findings table.",
              key, g$certainty %||% "-", length(outs)),
      type = "message", duration = 5)
  }

  shiny::observeEvent(input$save_outcome, {
    if (length(.save_blocked_reasons())) {
      shiny::showNotification(
        "Cannot save: review and confirm every certainty domain first.",
        type = "error", duration = 6)
      return()
    }
    g <- grade_obj()
    if (is.null(g)) return()
    key <- .save_key()
    # grade_table() labels rows by list name, so the pmatools object's own
    # outcome_name is aligned with the key for any downstream single-outcome
    # use of the saved object.
    g$outcome_name <- key
    if (key %in% names(pma_outcomes_list(state$outcomes))) {
      shiny::showModal(shiny::modalDialog(
        title = "Outcome already saved",
        htmltools::p(sprintf(
          "\"%s\" is already in the saved list. Replace it with the current assessment?",
          key)),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("save_outcome_overwrite", "Replace",
                              class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
      return()
    }
    .store_outcome(key, g)
  })

  shiny::observeEvent(input$save_outcome_overwrite, {
    shiny::removeModal()
    if (length(.save_blocked_reasons())) return()
    g <- grade_obj()
    if (is.null(g)) return()
    key <- .save_key()
    g$outcome_name <- key
    .store_outcome(key, g)
  })

  output$saved_outcomes_list <- shiny::renderUI({
    outs <- pma_outcomes_list(state$outcomes)
    sig  <- .current_signature()
    n_stale <- sum(pma_outcomes_stale(outs, sig))
    htmltools::tagList(
      pma_stale_warning_banner(n_stale),
      pma_saved_outcomes_ui(outs,
                            delete_input_id = "outcome_delete",
                            empty_text = EDU_COPY$multi_outcome$list_empty,
                            signature = sig)
    )
  })
  shiny::outputOptions(output, "saved_outcomes_list", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$outcome_delete, {
    key  <- as.character(input$outcome_delete)[1]
    outs <- pma_outcomes_list(state$outcomes)
    if (!key %in% names(outs)) return()
    outs[[key]] <- NULL
    state$outcomes <- outs
    shiny::showNotification(sprintf("Removed \"%s\" from the saved outcomes.", key),
                            type = "message", duration = 4)
  }, ignoreInit = TRUE)

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
