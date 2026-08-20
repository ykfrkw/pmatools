# step2_ma.R - Step 2: Meta-analysis configuration + plots

# --------------------------------------------------------------------------
# Abbreviations, spelled out where they are first read
# --------------------------------------------------------------------------
# "RoM", "MH" and "EB" say nothing to a reviewer meeting them cold, and the
# model controls were the one place in the app that showed them bare.
#
# The table maps the abbreviation to its expansion ONLY. The abbreviation is
# the input VALUE that every branch in the app and every saved outcome compares
# against ("OR", "REML", ...), so it must not move; building the label from
# this table rather than typing it beside the value is what keeps the two from
# drifting apart. Two readers share the table: the choices in step2_ui() /
# output$sm_cont_ui, and step2_model_summary_line(), which echoes whatever code
# it finds on the fitted object.
PMA_ABBREVIATION_EXPANSIONS <- c(
  OR   = "odds ratio",
  RR   = "risk ratio",
  MD   = "mean difference",
  SMD  = "standardised mean difference",
  RoM  = "ratio of means",
  MH   = "Mantel-Haenszel",
  REML = "restricted maximum likelihood",
  PM   = "Paule-Mandel",
  DL   = "DerSimonian-Laird",
  SJ   = "Sidik-Jonkman",
  ML   = "maximum likelihood",
  EB   = "empirical Bayes"
)

# "REML" -> "REML (restricted maximum likelihood)".
#
# `note` joins the SAME bracket ("REML (restricted maximum likelihood,
# default)") instead of opening a second one. A code the table does not carry
# comes back unchanged: "Inverse" and "Peto" are names rather than
# abbreviations and must not gain an empty bracket.
pma_spell_out <- function(code, note = NULL) {
  code <- as.character(code)[1]
  if (is.na(code) || !nzchar(code)) return(code)
  expansion <- unname(PMA_ABBREVIATION_EXPANSIONS[code])
  inside <- c(if (!is.na(expansion)) expansion, note)
  if (!length(inside)) return(code)
  paste0(code, " (", paste(inside, collapse = ", "), ")")
}

# A choices vector for radioButtons()/selectInput(): spelled-out label, value
# untouched. `notes` is an optional code -> note map, used for the "default"
# marker on the tau-squared estimator.
pma_spelled_choices <- function(codes, notes = NULL) {
  labels <- vapply(codes, function(code) {
    pma_spell_out(code, note = if (code %in% names(notes)) notes[[code]])
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(codes, labels)
}

# The Run analysis button read as a one-shot request rather than a latch.
#
# `run_clicks` is `input$run_ma`: an actionButton counter that only ever
# increases while the widget lives, and that starts again at 0 when app.R's
# step_body renderUI rebuilds the Step 2 body (a 2 -> 3 -> 2 round trip).
# `clicks_spent` is the count the analysis reactive has already served.
#
# Returns `pending` - this execution has a press to serve - and `spent`, the
# baseline to carry forward. `spent` follows a rebuilt counter back DOWN,
# because after a rebuild the reviewer's next press is numbered 1 and a stale
# baseline of 3 would swallow it, which is the inert Run button again.
#
# Reading the counter as a plain `> 0L` instead is what made "auto-rerun off"
# stop meaning anything: the flag latched TRUE for the rest of the session, so
# one press put the analysis back on every debounced input change - including
# run_rare_ma()'s multi-method suite, the cost the OFF default exists to avoid
# on rare-event data (SPEC 3.3.3).
step2_run_request <- function(run_clicks, clicks_spent) {
  clicks <- run_clicks %||% 0L
  spent  <- min(clicks_spent %||% 0L, clicks)
  list(pending = clicks > spent, spent = spent)
}

step2_ui <- function(state = NULL) {
  s <- EDU_COPY$steps$step2

  # Rare-event datasets trigger an expensive multi-method suite (run_rare_ma)
  # on every rerun, so the auto-rerun toggle defaults to OFF for them (the
  # one-time updateCheckboxInput in step2_server handles the live transition;
  # this default keeps the choice sticky when the step body is re-rendered).
  # isolate(): this UI is built inside app.R's step_body renderUI, which must
  # not take a reactive dependency on the diagnostics.
  auto_rerun_default <- TRUE
  if (!is.null(state)) {
    diag <- shiny::isolate(state$rare_diagnostics)
    if (!is.null(diag) && isTRUE(diag$rare_flow)) auto_rerun_default <- FALSE
  }

  # Outcome identity (name + direction) is required before the analysis runs.
  # app.R's step_body renderUI rebuilds this whole UI on every step change, so
  # a freshly created widget pushes its DOM default back to the server: hard
  # -coding value = "" / selected = character(0) would wipe the user's answers
  # on every 2 -> 3 -> 2 round trip. Seeding from the mirrored state (same
  # trick as auto_rerun_default above) keeps the two fields sticky.
  outcome_name_default  <- ""
  small_values_default  <- character(0)
  # Follow-up and unit are optional presentation fields for the Core GRADE 6
  # Summary of Findings table (see ui_helpers.R). They are seeded from state
  # for the same reason the two required fields are.
  follow_up_default     <- ""
  unit_default          <- ""
  # The outcome type is sticky for exactly the same reason, and was not.
  # A hard-coded selected = "binary" reset it on every 3 -> 2 -> 3 round trip,
  # which on continuous data pushed ma() onto the "missing Events column"
  # branch and sent grade_meta() outcome_type = "relative" for a metacont fit,
  # where the OIS cannot be computed at all.
  outcome_type_default  <- "binary"
  if (!is.null(state)) {
    nm <- shiny::isolate(state$outcome_name)
    if (!is.null(nm) && length(nm) == 1 && !is.na(nm)) outcome_name_default <- nm
    sv <- shiny::isolate(state$small_values)
    if (!is.null(sv) && length(sv) == 1 && nzchar(sv)) small_values_default <- sv
    fu <- shiny::isolate(state$outcome_follow_up)
    if (!is.null(fu) && length(fu) == 1 && !is.na(fu)) follow_up_default <- fu
    un <- shiny::isolate(state$outcome_unit)
    if (!is.null(un) && length(un) == 1 && !is.na(un)) unit_default <- un
    ot <- shiny::isolate(state$outcome_type)
    if (!is.null(ot) && length(ot) == 1 && ot %in% c("binary", "continuous")) {
      outcome_type_default <- ot
    }
  }

  # One panel open at a time, and on arrival it is "Outcome": the only panel
  # holding something no default can supply. Opening another closes it (see
  # `multiple = FALSE` below), so the sidebar is never taller than the
  # question being answered and the sticky action bar stays in view.
  # A mapping select that is blank when the reviewer actually asks for an
  # analysis is handled from the other end, by required-fields.js, which opens
  # the panel holding it; that check cannot be made here because the selects
  # are populated by the server after this UI is built.
  open_panels <- "outcome"

  htmltools::tagList(
    # Registers the `pma_required_fields` custom message handler used by the
    # required-field highlighting below. It lives here rather than in app.R's
    # <head> because app.R is out of scope for this change; loading it as part
    # of the Step 2 body also means it re-executes on every rebuild of that
    # body, which is exactly what repaints the marks (see required-fields.js).
    htmltools::tags$script(src = "required-fields.js"),

    pma_step_header(s$title),

    htmltools::div(
      class = "row",
      style = "display: flex; gap: 1.5rem; flex-wrap: wrap;",

      # Sidebar. `flex: 0 1 300px`, and each of the three numbers is load
      # bearing:
      #  * grow 0 - with `1` the sidebar took its share of every spare pixel,
      #    so on a wide screen a column of short selects grew past 500px while
      #    the forest plot next to it stayed small. The right pane is the one
      #    that can use the room.
      #  * shrink 1 - the sidebar MUST still be able to give width back. A
      #    fixed `0 0` basis is what once made a 375px phone render a row wider
      #    than the viewport and scroll the whole document sideways.
      #  * basis 300px - below the width at which the accordion labels wrap,
      #    and never above 100% of a phone viewport.
      htmltools::div(
        style = "flex: 0 1 300px;",
        pma_card(
          title = "Model configuration",

          # Four accordion panels rather than one 120-line column of controls.
          # The order is the order the questions arise, and exactly one of them
          # is open at a time (see `open_panels` above).
          bslib::accordion(
            multiple = FALSE,
            open     = open_panels,

            bslib::accordion_panel(
              "Outcome", value = "outcome",
              shiny::textInput("outcome_name", "Outcome name (required)",
                               value = outcome_name_default, width = "100%",
                               placeholder = "e.g., Depression response"),
              # No preselected direction: the user must actively choose. The
              # values "desirable" / "undesirable" are the vocabulary the
              # vendored pmatools validates, so only the labels are re-worded.
              #
              # The question is asked once, in the label, and the options are
              # the two answers to it. They used to restate it ("Favorable -
              # smaller is better (e.g., mortality, symptom score)"), which put
              # the same sentence on screen three times and wrapped each option
              # onto four lines in a 300px sidebar. One word each fits inline,
              # beside the Outcome type radio it now matches.
              shiny::radioButtons("small_values",
                "A SMALLER value of this outcome is... (required)",
                choices = c("Favorable" = "desirable",
                            "Unfavorable" = "undesirable"),
                selected = small_values_default, inline = TRUE),
              # Binary / continuous sits with the outcome's identity rather
              # than with the column mapping: it is a property of the outcome,
              # and it decides which of the optional fields below applies.
              shiny::radioButtons("outcome_type", "Outcome type",
                choices = c("Binary" = "binary", "Continuous" = "continuous"),
                selected = outcome_type_default, inline = TRUE),
              # Follow-up belongs to the outcome's identity, not to the display
              # settings: Core GRADE 6's first column is "Outcome and
              # follow-up", and a review that pools two outcomes measured over
              # different time frames needs one value per outcome.
              shiny::textInput("outcome_follow_up",
                               "Follow-up / time frame (optional)",
                               value = follow_up_default, width = "100%",
                               placeholder = "e.g., longest, range 8-52 weeks"),
              shiny::conditionalPanel(
                "input.outcome_type == 'continuous'",
                shiny::textInput("outcome_unit",
                                 "Unit of the scale (optional, continuous only)",
                                 value = unit_default, width = "100%",
                                 placeholder = "e.g., points on the PHQ-9, days"),
                htmltools::p(class = "pma-card-subtitle",
                  paste0("Labels the Difference column of the Summary of Findings ",
                         "table for a mean difference. A standardized mean ",
                         "difference is not on the original scale, so its ",
                         "difference is always labelled in standard deviation ",
                         "units; a ratio measure carries no unit."))
              )
            ),

            bslib::accordion_panel(
              "Data mapping", value = "mapping",
              # Selectize widgets, NOT `selectize = FALSE`. A native <select>
              # is token-styled while closed but its open list is drawn by the
              # operating system and cannot be styled at all, so these six
              # dropdowns changed appearance the moment they were used - the
              # one moment they are being read. A selectize dropdown is our own
              # DOM and www/shadcn.css repaints it whole.
              shiny::selectInput("col_studlab", "Study label (studlab)",
                                 choices = NULL),
              shiny::selectInput("col_treat", "Arm / treatment (treat)",
                                 choices = NULL),
              shiny::uiOutput("arm_assignment_ui"),
              shiny::selectInput("col_n", "Sample size (n)",
                                 choices = NULL),
              shiny::conditionalPanel(
                "input.outcome_type == 'binary'",
                shiny::selectInput("col_event", "Events",
                                   choices = NULL)
              ),
              shiny::conditionalPanel(
                "input.outcome_type == 'continuous'",
                shiny::selectInput("col_mean", "Mean",
                                   choices = NULL),
                shiny::selectInput("col_sd",   "SD",
                                   choices = NULL)
              )
            ),

            bslib::accordion_panel(
              "Model details", value = "model",
              # Regular-workflow controls. Each panel uses a single, flat
              # conditionalPanel expression so that the JS evaluator can hide /
              # show them reliably across step transitions (nested conditional
              # panels were not consistently re-evaluated when
              # input.outcome_type changed in step 2).
              shiny::conditionalPanel(
                "input.outcome_type == 'binary' && input.use_rare_workflow != true",
                shiny::radioButtons("sm_bin", "Summary measure",
                  choices = pma_spelled_choices(c("OR", "RR")),
                  selected = "OR", inline = TRUE)
              ),
              shiny::conditionalPanel(
                "input.outcome_type == 'continuous' && input.use_rare_workflow != true",
                shiny::uiOutput("sm_cont_ui")
              ),
              shiny::conditionalPanel(
                "input.use_rare_workflow != true",
                shiny::radioButtons("model", "Model",
                  choices = c("Random" = "random", "Common (Fixed)" = "common"),
                  selected = "random", inline = TRUE)
              ),
              shiny::conditionalPanel(
                "input.outcome_type == 'binary' && input.use_rare_workflow != true",
                shiny::selectInput("method", "Pooling method",
                  choices = pma_spelled_choices(c("Inverse", "MH", "Peto")),
                  selected = "Inverse")
              ),
              shiny::conditionalPanel(
                "input.model == 'random' && input.use_rare_workflow != true",
                shiny::selectInput("method_tau", "tau-squared estimator",
                  choices = pma_spelled_choices(
                    c("REML", "PM", "DL", "SJ", "ML", "EB"),
                    notes = c(REML = "default")),
                  selected = "REML"),
                # The Hartung-Knapp adjustment was applied automatically at
                # k >= 3 and never mentioned anywhere, so nobody could either
                # see it or turn it off. "Auto" is that same rule, named.
                shiny::selectInput("random_ci", "Random-effects CI",
                  choices = c("Auto (Hartung-Knapp when k >= 3)" = "auto",
                              "Hartung-Knapp"                    = "hk",
                              "Classic (Wald)"                   = "classic"),
                  selected = "auto")
              ),
              shiny::conditionalPanel(
                "input.outcome_type == 'binary' && input.use_rare_workflow != true",
                shiny::numericInput("incr", "Continuity correction (zero events)",
                  value = 0.5, min = 0, step = 0.1)
              )
            ),

            bslib::accordion_panel(
              "Subgroup", value = "subgroup",
              shiny::selectInput("subgroup_col", "Subgroup column",
                                 choices = c("(none)" = ""),
                                 selected = "", selectize = FALSE),
              shiny::uiOutput("subgroup_order_ui")
            )
          ),

          # Sticky action bar. The sidebar is taller than a laptop viewport
          # with every panel open, and the primary action used to sit at the
          # very bottom of it, so changing a model setting meant scrolling back
          # down to act on it. Bleeds to the card's edges (see .pma-step2-actions
          # in www/shadcn.css).
          htmltools::div(
            class = "pma-step2-actions",
            shiny::actionButton("run_ma", "Run analysis",
              class = "btn btn-primary", style = "width: 100%;"),
            shiny::checkboxInput("auto_rerun",
              "Auto-rerun on change (500ms debounce)",
              value = auto_rerun_default)
          )
        )
      ),

      # Right pane. `min-width: min(480px, 100%)` rather than a flat 480px:
      # the flat floor is what pushed the document past a 375px viewport once
      # the two columns had wrapped onto separate rows. `flex: 1` against the
      # sidebar's `flex: 0 ...` means every spare pixel on a wide screen lands
      # here, which is where the forest plot is.
      htmltools::div(
        style = "flex: 1; min-width: min(480px, 100%);",
        shiny::uiOutput("rare_events_panel"),

        # Before the first run the Results card is an empty tab strip, which
        # says nothing about what to do next. Hidden and replaced by one line,
        # rather than unrendered: the card holds the forest-display widgets,
        # and a renderUI swap would throw away every value typed into them
        # (the same hazard output$step2_nav exists to avoid). The condition is
        # written so that the placeholder, not the empty card, is what shows
        # before output.pma_has_ma has arrived from the server.
        shiny::conditionalPanel(
          "!output.pma_has_ma",
          pma_card(
            htmltools::p(
              class = "pma-card-subtitle", style = "margin-bottom: 0;",
              htmltools::HTML(
                "Press <strong>Run analysis</strong> to pool the studies."))
          )
        ),
        shiny::conditionalPanel(
          "output.pma_has_ma",
          pma_card(
            title = "Results",
            # Above the tabs, not inside "Text results": the Hartung-Knapp
            # adjustment used to be applied silently, and a setting nobody can
            # see is a setting nobody can question. Names what the fit actually
            # did, read off the fitted object rather than off the controls.
            shiny::uiOutput("ma_model_summary"),
            shiny::tabsetPanel(
              id = "ma_tabs",
              shiny::tabPanel("Forest plot",
                htmltools::div(class = "pma-forest-image",
                  shinycssloaders::withSpinner(
                    shiny::imageOutput("forest_plot", height = "auto"),
                    type = 4, color = "#0f172a", size = 0.6,
                    proxy.height = "320px")),
                shiny::uiOutput("rare_sensitivity_block"),
                # Ids and layout are shared with the four Step 3 domain
                # panels; see pma_forest_display_panel() in ui_helpers.R.
                # NULL prefix = the unprefixed Step 2 ids.
                pma_forest_display_panel(NULL)
              ),
              shiny::tabPanel("Funnel plot",
                shinycssloaders::withSpinner(
                  shiny::imageOutput("funnel_plot", height = "auto"),
                  type = 4, color = "#0f172a", size = 0.6,
                  proxy.height = "320px"),
                pma_funnel_display_panel("funnel_step2"),
                htmltools::p(class = "pma-card-subtitle",
                  "Egger's test annotation appears when k >= 10.")
              ),
              shiny::tabPanel("Text results",
                htmltools::div(
                  class = "pma-results-wrap",
                  shiny::actionButton("copy_results", "Copy",
                    icon = shiny::icon("copy"),
                    class = "btn btn-sm btn-secondary pma-copy-btn"),
                  shinycssloaders::withSpinner(
                    shiny::verbatimTextOutput("ma_summary"),
                    type = 4, color = "#0f172a", size = 0.5,
                    proxy.height = "80px")
                )
              )
            )
          )
        )
      )
    ),

    # Own output, not part of this body: see output$step2_nav in
    # step2_server(). A rebuild of the body here would throw away every
    # unsaved widget value (forest title, labels, ...).
    shiny::uiOutput("step2_nav")
  )
}

# One line naming the model that produced the numbers on screen, read off the
# fitted object so it cannot drift from the controls that were set when the run
# started. A pure function of the object, so it is testable without a session.
step2_model_summary_line <- function(meta_obj) {
  if (is.null(meta_obj)) return(NULL)
  k <- meta_obj$k %||% length(meta_obj$TE %||% numeric(0))
  parts <- if (isTRUE(meta_obj$random)) {
    # {meta} keeps `hakn` only as a legacy alias of method.random.ci.
    uses_hk <- if (is.null(meta_obj$method.random.ci)) {
      isTRUE(meta_obj$hakn)
    } else {
      identical(as.character(meta_obj$method.random.ci)[1], "HK")
    }
    # The estimator is its own comma-separated part rather than "Random
    # effects (REML)": spelling the code out (same table the control uses) puts
    # a bracket inside the label, and a nested bracket reads worse than a
    # fourth item in a list that already has three.
    c("Random effects",
      pma_spell_out(meta_obj$method.tau %||% "REML"),
      if (uses_hk) "Hartung-Knapp CI" else "classic (Wald) CI")
  } else {
    "Common (fixed) effect"
  }
  paste(c(parts, sprintf("k = %d", as.integer(k))), collapse = ", ")
}

step2_server <- function(input, output, session, state) {

  .pick <- function(prefs, pool, fallback = "") {
    for (p in prefs) if (p %in% pool) return(p)
    pool_l <- tolower(pool)
    for (p in prefs) {
      idx <- match(tolower(p), pool_l)
      if (!is.na(idx)) return(pool[[idx]])
    }
    fallback
  }

  .pick_current <- function(current, pool, default = "") {
    if (!is.null(current) && length(current) == 1 &&
        nzchar(current) && current %in% pool) {
      return(current)
    }
    default
  }

  .mapping_choices <- function(cols) {
    stats::setNames(c("", cols), c("(select)", cols))
  }

  # Arm assignment UI: shown once data is loaded
  output$arm_assignment_ui <- shiny::renderUI({
    if (is.null(state$data)) {
      return(htmltools::p(class = "pma-card-subtitle",
                          "Load data in Step 1 to assign arms."))
    }
    treat_col <- input$col_treat %||% "treat"
    if (!nzchar(treat_col) || !treat_col %in% names(state$data)) {
      return(htmltools::p(class = "pma-card-subtitle",
                          "Select the treat column before assigning arms."))
    }
    arms <- sort(unique(as.character(state$data[[treat_col]])))
    arms <- arms[!is.na(arms) & nzchar(arms)]
    if (length(arms) < 2) {
      return(htmltools::p(class = "pma-card-subtitle",
                          paste0("Only one arm value found: ", paste(arms, collapse = ","))))
    }
    # Heuristic defaults: prefer "Control" (case-insensitive) for control,
    # the other for experimental
    ctrl_default <- {
      ctrl_idx <- grepl("^control$|^placebo$|^waitlist$|^tau$", arms, ignore.case = TRUE)
      if (any(ctrl_idx)) arms[which(ctrl_idx)[1]] else arms[1]
    }
    exp_default <- setdiff(arms, ctrl_default)[1]
    cur_exp <- shiny::isolate(input$experimental_label)
    cur_ctrl <- shiny::isolate(input$control_label)
    exp_selected <- .pick_current(cur_exp, arms, exp_default)
    ctrl_selected <- .pick_current(cur_ctrl, arms, ctrl_default)
    if (identical(exp_selected, ctrl_selected)) {
      exp_selected <- setdiff(arms, ctrl_selected)[1] %||% exp_selected
    }
    htmltools::tagList(
      shiny::selectInput("experimental_label", "Intervention arm value",
                         choices = arms, selected = exp_selected),
      shiny::selectInput("control_label", "Control arm value",
                         choices = arms, selected = ctrl_selected)
    )
  })
  # This output is the only source of input$experimental_label and
  # input$control_label, and it lives inside the "Data mapping" accordion
  # panel, which is closed every time Step 2 is built (open_panels above, and
  # multiple = FALSE). Under Shiny's default the panel being closed suspends
  # the output, the two selects are never created, and the `ma` reactive below
  # bails on NULL arm labels -- so Run analysis did nothing whatsoever, with no
  # toast, no spinner and no log. Rendering regardless of visibility is what
  # keeps the analysis reachable without the reviewer first opening a panel
  # they have no reason to open.
  shiny::outputOptions(output, "arm_assignment_ui", suspendWhenHidden = FALSE)

  # Summary measure for continuous outcomes. RoM (ratio of means) is only
  # meaningful when every mean value is positive, so include it conditionally.
  output$sm_cont_ui <- shiny::renderUI({
    study_data <- state$data
    has_positive_means <- FALSE
    mean_col <- input$col_mean %||% "mean"
    if (!is.null(study_data) && nzchar(mean_col) && mean_col %in% names(study_data)) {
      m <- suppressWarnings(as.numeric(study_data[[mean_col]]))
      if (length(m) > 0 && all(is.finite(m)) && all(m > 0)) {
        has_positive_means <- TRUE
      }
    }
    choices <- c("MD", "SMD")
    if (has_positive_means) choices <- c(choices, "RoM")
    # SMD, not MD: a review that pools continuous outcomes at all is usually
    # pooling several instruments (PHQ-9, HAMD, BDI), and a mean difference
    # across two scales is not a quantity. MD stays one click away for the
    # single-instrument case.
    current <- shiny::isolate(input$sm_cont) %||% "SMD"
    if (!current %in% choices) current <- "SMD"
    shiny::radioButtons("sm_cont", "Summary measure",
                        choices = pma_spelled_choices(choices),
                        selected = current, inline = TRUE)
  })

  # ----- Column mapping: populate choices from current data -----
  # Static placeholders live in step2_ui(); these observers fill them so that
  # user selections are NOT reset on every MA rerun (renderUI re-creates inputs
  # from scratch and resets selections; updateSelectInput preserves them).

  shiny::observe({
    if (!identical(as.integer(state$step %||% 0L), 2L)) return()
    study_data <- state$data
    if (is.null(study_data)) return()
    all_cols <- names(study_data)
    numeric_cols <- names(study_data)[vapply(study_data, is.numeric, logical(1))]
    is_factor_like <- function(x) is.character(x) || is.factor(x)
    factor_cols  <- names(study_data)[vapply(study_data, is_factor_like,
                                             logical(1))]
    all_choices <- .mapping_choices(all_cols)
    numeric_choices <- .mapping_choices(numeric_cols)
    studlab_default <- .pick(c("studlab", "id", "study_name", "study_id",
                               "trial_id", "trial", "study", "name", "label"),
                             all_cols)
    treat_default   <- .pick(c("treat", "t", "treatment", "arm",
                               "intervention", "group", "condition"),
                             all_cols)
    n_default     <- .pick(c("n", "n_randomized", "n_total", "sample_size", "N"),
                           numeric_cols)
    event_default <- .pick(c("event", "events", "d_response", "d_r",
                             "responders", "n_events"),
                           numeric_cols)
    mean_default  <- .pick(c("mean", "means", "d_ep_m", "severity_endpoint_mean"),
                           numeric_cols)
    sd_default    <- .pick(c("sd", "stdev", "stddev", "d_ep_sd",
                             "severity_endpoint_sd"),
                           numeric_cols)

    # Populate column-mapping choices, preserve current selection if still valid
    cur_studlab <- shiny::isolate(input$col_studlab)
    cur_treat   <- shiny::isolate(input$col_treat)
    cur_n       <- shiny::isolate(input$col_n)
    cur_event   <- shiny::isolate(input$col_event)
    cur_mean    <- shiny::isolate(input$col_mean)
    cur_sd      <- shiny::isolate(input$col_sd)
    cur_sub     <- shiny::isolate(input$subgroup_col)

    studlab_selected <- .pick_current(cur_studlab, all_cols, studlab_default)
    treat_selected   <- .pick_current(cur_treat, all_cols, treat_default)
    factor_cols <- setdiff(factor_cols,
                           c("studlab", "treat", "outcome",
                             studlab_selected, treat_selected))

    shiny::updateSelectInput(session, "col_studlab", choices = all_choices,
                             selected = studlab_selected)
    shiny::updateSelectInput(session, "col_treat", choices = all_choices,
                             selected = treat_selected)
    shiny::updateSelectInput(session, "col_n",     choices = numeric_choices,
                             selected = .pick_current(cur_n, numeric_cols, n_default))
    shiny::updateSelectInput(session, "col_event", choices = numeric_choices,
                             selected = .pick_current(cur_event, numeric_cols, event_default))
    shiny::updateSelectInput(session, "col_mean",  choices = numeric_choices,
                             selected = .pick_current(cur_mean, numeric_cols, mean_default))
    shiny::updateSelectInput(session, "col_sd",    choices = numeric_choices,
                             selected = .pick_current(cur_sd, numeric_cols, sd_default))
    shiny::updateSelectInput(session, "subgroup_col",
                             choices  = c("(none)" = "", factor_cols),
                             selected = if (!is.null(cur_sub)) cur_sub else "")
  })

  # When outcome_type changes, prefer outcome-appropriate defaults (only if
  # the user hasn't picked something already valid for the new outcome).
  shiny::observeEvent(input$outcome_type, {
    study_data <- state$data
    if (is.null(study_data)) return()
    numeric_cols <- names(study_data)[vapply(study_data, is.numeric, logical(1))]
    if (length(numeric_cols) == 0) return()

    if (identical(input$outcome_type, "binary")) {
      shiny::updateSelectInput(session, "col_n",
        selected = .pick(c("n", "n_randomized", "n_total", "sample_size", "N"),
                         numeric_cols))
      shiny::updateSelectInput(session, "col_event",
        selected = .pick(c("event", "events", "d_response", "d_r",
                           "responders", "n_events"),
                         numeric_cols))
    } else {
      shiny::updateSelectInput(session, "col_n",
        selected = .pick(c("n", "d_ep_n", "n_randomized", "n_total",
                           "sample_size", "N"),
                         numeric_cols))
      shiny::updateSelectInput(session, "col_mean",
        selected = .pick(c("mean", "means", "d_ep_m",
                           "severity_endpoint_mean"),
                         numeric_cols))
      shiny::updateSelectInput(session, "col_sd",
        selected = .pick(c("sd", "stdev", "stddev", "d_ep_sd",
                           "severity_endpoint_sd"),
                         numeric_cols))
    }
  }, ignoreInit = FALSE)

  # RoM (ratio of means) is only mathematically valid when all means are
  # positive. Hide RoM from the sm_cont radio when the chosen mean column
  # contains zero or negative values.
  shiny::observe({
    study_data   <- state$data
    col <- input$col_mean
    if (is.null(study_data) || is.null(col) || !nzchar(col) || !col %in% names(study_data)) return()
    vals <- study_data[[col]]
    if (!is.numeric(vals)) return()
    has_non_positive <- any(vals <= 0, na.rm = TRUE)
    choices  <- if (has_non_positive) c("MD", "SMD") else c("MD", "SMD", "RoM")
    cur_sm   <- shiny::isolate(input$sm_cont) %||% "SMD"
    selected <- if (cur_sm %in% choices) cur_sm else "SMD"
    # pma_spelled_choices(), matching output$sm_cont_ui above. Passing the bare
    # codes here rewrote the control with unspelled labels the moment the mean
    # column changed, so "SMD (standardised mean difference)" silently became
    # "SMD" - and §3.3.3 of shiny/SPEC.md says every abbreviation in this panel
    # is spelled out on sight. The values are unchanged either way, which is
    # exactly why nothing downstream noticed.
    shiny::updateRadioButtons(session, "sm_cont",
                              choices = pma_spelled_choices(choices),
                              selected = selected,
                              inline = TRUE)
  })

  output$subgroup_order_ui <- shiny::renderUI({
    if (is.null(input$subgroup_col) || !nzchar(input$subgroup_col)) return(NULL)
    study_data <- state$data
    if (is.null(study_data) || !(input$subgroup_col %in% names(study_data))) return(NULL)
    lv <- unique(as.character(study_data[[input$subgroup_col]]))
    lv <- lv[!is.na(lv) & nzchar(lv)]
    if (length(lv) < 2) {
      return(htmltools::p(class = "pma-card-subtitle",
                          "This column has fewer than 2 non-empty levels."))
    }
    shiny::selectizeInput(
      "subgroup_order",
      "Subgroup order (drag to reorder)",
      choices  = lv,
      selected = lv,
      multiple = TRUE,
      options  = list(plugins = list("drag_drop"))
    )
  })

  # Bundle inputs that should drive a re-run when they change.
  ma_inputs <- shiny::reactive({
    list(
      data         = state$data,
      outcome_type = input$outcome_type,
      sm           = if (identical(input$outcome_type, "binary")) input$sm_bin else input$sm_cont,
      method       = if (identical(input$outcome_type, "binary")) input$method else NULL,
      method.tau   = input$method_tau %||% "REML",
      random_ci    = input$random_ci %||% "auto",
      random       = identical(input$model, "random"),
      common       = identical(input$model, "common"),
      incr         = input$incr %||% 0.5,
      experimental_label = input$experimental_label,
      control_label      = input$control_label,
      col_studlab    = input$col_studlab,
      col_treat      = input$col_treat,
      col_n          = input$col_n,
      col_event      = input$col_event,
      col_mean       = input$col_mean,
      col_sd         = input$col_sd,
      subgroup_col   = if (nzchar(input$subgroup_col %||% "")) input$subgroup_col else NULL,
      subgroup_order = input$subgroup_order,
      # Booleans, not the values themselves: the raw outcome name would make
      # this bundle change on every keystroke (after the 500 ms debounce),
      # re-running run_ma() - and for rare-event data the whole multi-method
      # suite - while the user is still typing. A boolean only invalidates on
      # the empty <-> non-empty transition, which is all the gate below needs.
      outcome_name_set = nzchar(trimws(input$outcome_name %||% "")),
      small_values_set = { sv <- input$small_values
                           !is.null(sv) && length(sv) == 1L && nzchar(sv) }
    )
  }) |> shiny::debounce(500)

  # How much of input$run_ma this reactive has already served. See
  # step2_run_request() above for why the raw counter cannot answer that.
  run_clicks_spent <- shiny::reactiveVal(0L)

  ma <- shiny::reactive({
    args <- ma_inputs()
    # isolate(): the one-time "auto-rerun OFF" default applied when rare
    # events are detected (observer below) must not itself re-trigger this
    # heavy reactive. Toggling the checkbox therefore no longer forces an
    # immediate rerun; the next input change (or Run analysis click) does.
    auto <- isTRUE(shiny::isolate(input$auto_rerun))
    run_clicks <- input$run_ma %||% 0L
    # TWO questions get asked of that counter below, and only one of them
    # wants a latch.
    #
    #  * `run_pending` - "is a press of Run analysis still waiting to be
    #    served?" This is the gate for auto-rerun-off, and it MUST be a
    #    one-shot: the press is spent by the run it asks for (just above
    #    withProgress below), so the next input change is not a re-run.
    #  * `ever_run_requested` - "has the reviewer asked for an analysis at
    #    all?" Here the latch IS the meaning, and the two warning branches
    #    below want it exactly as it stands: before the first request a
    #    half-filled form is a normal state and stays quiet; afterwards it is
    #    worth a toast, however the reviewer got back to it. This is the
    #    behaviour on the common path (auto-rerun left ON) and it does not
    #    change.
    request <- step2_run_request(run_clicks, shiny::isolate(run_clicks_spent()))
    shiny::isolate(run_clicks_spent(request$spent))
    run_pending <- isTRUE(request$pending)
    ever_run_requested <- run_clicks > 0L
    # When auto-rerun is off, require an unserved Run analysis press
    if (!auto && !run_pending) return(NULL)
    if (is.null(args$data)) return(NULL)

    # Apply column mapping (rename user-selected columns to canonical names).
    # `outcome` is not consulted here: it is a descriptive column, not an
    # analysis partition key. Data whose studies each measured a different
    # scale used to be sliced down to the first scale, which on a PHQ-9 /
    # HAMD / BDI review left one study standing and no message saying why.
    # run_ma() stops the one case that genuinely cannot be pooled -- the same
    # study under two outcomes -- and the tryCatch below shows what it said.
    study_data <- args$data

    missing_cols <- character()
    if (is.null(args$col_studlab) || !nzchar(args$col_studlab) ||
        !args$col_studlab %in% names(study_data)) {
      missing_cols <- c(missing_cols, "studlab")
    }
    if (is.null(args$col_treat) || !nzchar(args$col_treat) ||
        !args$col_treat %in% names(study_data)) {
      missing_cols <- c(missing_cols, "treat")
    }
    missing_cols <- c(missing_cols, "n")
    if (!is.null(args$col_n) && nzchar(args$col_n) &&
        args$col_n %in% names(study_data)) {
      missing_cols <- setdiff(missing_cols, "n")
    }
    if (identical(args$outcome_type, "binary")) {
      if (is.null(args$col_event) || !nzchar(args$col_event) ||
          !args$col_event %in% names(study_data)) {
        missing_cols <- c(missing_cols, "event")
      }
    } else {
      if (is.null(args$col_mean) || !nzchar(args$col_mean) ||
          !args$col_mean %in% names(study_data)) {
        missing_cols <- c(missing_cols, "mean")
      }
      if (is.null(args$col_sd) || !nzchar(args$col_sd) ||
          !args$col_sd %in% names(study_data)) {
        missing_cols <- c(missing_cols, "sd")
      }
    }
    # Outcome identity is as mandatory as the column mapping, but it lives in
    # its own list so the warning can name the fields rather than pretend they
    # are columns.
    missing_required <- character()
    if (!isTRUE(args$outcome_name_set)) {
      missing_required <- c(missing_required,
                            STEP2_IDENTITY_FIELD_LABELS[["outcome_name"]])
    }
    if (!isTRUE(args$small_values_set)) {
      missing_required <- c(missing_required,
                            STEP2_IDENTITY_FIELD_LABELS[["small_values"]])
    }

    if (length(missing_cols) > 0 || length(missing_required) > 0) {
      # Step 3 renders off state$ma and had no way to say WHY it was empty, so
      # a blanked required field showed up there as "Run analysis and
      # configure domains." - the screen blaming the reviewer for the one
      # thing they had already done. Record what is missing, in labels Step 3
      # can print as they stand (see step3_blocked_message()).
      had_ma <- !is.null(shiny::isolate(state$ma))
      state$ma <- NULL
      state$ma_blocked <- c(
        step2_column_labels(unique(missing_cols)),
        missing_required
      )
      # The existing gate keeps the first page load quiet (auto_rerun defaults
      # to TRUE and run_ma has not been clicked), so the user only gets told
      # off once they actually ask for an analysis. `had_ma` is the exception:
      # withdrawing an analysis that WAS working is never a quiet event, no
      # matter how the reviewer got there.
      if (!auto || ever_run_requested || had_ma) {
        msgs <- character()
        if (had_ma) {
          msgs <- c(msgs, paste0(
            "The analysis has been cleared, and every Step 3 judgment with ",
            "it."))
        }
        if (length(missing_cols) > 0) {
          msgs <- c(msgs, paste("Select required column(s):",
                                paste(unique(missing_cols), collapse = ", ")))
        }
        if (length(missing_required) > 0) {
          msgs <- c(msgs, paste("Complete required field(s):",
                                paste(missing_required, collapse = ", ")))
        }
        # `ever_run_requested` latches TRUE once Run analysis has been pressed
        # -- deliberately, see the gate above -- so this branch can fire on
        # every later input change. A fixed id makes each new toast replace the
        # previous one instead of stacking them up.
        shiny::showNotification(
          paste(msgs, collapse = " "),
          id = "step2_required_fields", type = "warning", duration = 8
        )
      }
      return(NULL)
    }

    study_data$studlab <- study_data[[args$col_studlab]]
    study_data$treat <- study_data[[args$col_treat]]
    if (is.null(args$experimental_label) || is.null(args$control_label) ||
        identical(args$experimental_label, args$control_label)) {
      # This was the one exit in this reactive that said nothing at all, and a
      # suspended arm_assignment_ui (see outputOptions above) turned that
      # silence into an app whose Run analysis button was inert. The suspension
      # is fixed at its source; this message is what makes the next one visible
      # rather than inert, and it is the only feedback the reviewer gets when
      # they pick the same arm value twice.
      #
      # Quiet on a first page load, for the reason the required-fields branch
      # above is: auto defaults TRUE and run_ma has not been clicked, so the
      # reviewer is told off only once they have actually asked for a run.
      if (!auto || ever_run_requested) {
        same_arm <- !is.null(args$experimental_label) &&
          identical(args$experimental_label, args$control_label)
        shiny::showNotification(
          if (same_arm) {
            paste0("The intervention and control arms are both set to \"",
                   args$experimental_label, "\". Pick two different arm ",
                   "values under Data mapping.")
          } else {
            paste0("The analysis did not run: the intervention and control ",
                   "arms are not set. Open Data mapping in the sidebar and ",
                   "pick them.")
          },
          id = "step2_arm_assignment", type = "warning", duration = 8
        )
      }
      return(NULL)
    }
    # Guard: when the user just swapped data, input$experimental_label /
    # input$control_label may still hold values from the previous dataset.
    # Running run_ma() with arm labels that do not appear in study_data$treat
    # produces a misleading "Study X does not have exactly one intervention
    # and one control arm" error for every row. Wait silently for the
    # arm_assignment_ui to re-render with valid defaults.
    arms_in_data <- unique(as.character(study_data$treat))
    arms_in_data <- arms_in_data[!is.na(arms_in_data) & nzchar(arms_in_data)]
    if (!(args$experimental_label %in% arms_in_data) ||
        !(args$control_label %in% arms_in_data)) {
      # "Wait silently for arm_assignment_ui to re-render" holds only while
      # Step 2 is on screen: it is a Step 2 uiOutput and cannot re-render from
      # Step 3, so the wait can last the rest of the session while Step 3 shows
      # either nothing or the previous run's numbers. Once this outcome has run
      # successfully at least once (state$regular_ma), say so. A fresh form
      # stays quiet, which is what the silence was protecting.
      if (!is.null(shiny::isolate(state$regular_ma))) {
        shiny::showNotification(
          paste0("The analysis did not re-run: the selected intervention / ",
                 "control arms (", args$experimental_label, ", ",
                 args$control_label, ") are not present in the data. Anything ",
                 "still on screen is from the previous run. Go back to Step 2 ",
                 "and pick the arms again."),
          id = "step2_arm_labels", type = "warning", duration = 10
        )
      }
      return(NULL)
    }

    if (!is.null(args$col_n) && nzchar(args$col_n) && args$col_n != "n" &&
        args$col_n %in% names(study_data)) {
      study_data$n <- study_data[[args$col_n]]
    }
    if (identical(args$outcome_type, "binary")) {
      if (!is.null(args$col_event) && nzchar(args$col_event) &&
          args$col_event != "event" && args$col_event %in% names(study_data)) {
        study_data$event <- study_data[[args$col_event]]
      }
    } else {
      if (!is.null(args$col_mean) && nzchar(args$col_mean) &&
          args$col_mean != "mean" && args$col_mean %in% names(study_data)) {
        study_data$mean <- study_data[[args$col_mean]]
      }
      if (!is.null(args$col_sd) && nzchar(args$col_sd) &&
          args$col_sd != "sd" && args$col_sd %in% names(study_data)) {
        study_data$sd <- study_data[[args$col_sd]]
      }
    }

    # Apply subgroup factor levels for user-specified order
    if (!is.null(args$subgroup_col) && args$subgroup_col %in% names(study_data) &&
        !is.null(args$subgroup_order) && length(args$subgroup_order) >= 2) {
      study_data[[args$subgroup_col]] <- factor(as.character(study_data[[args$subgroup_col]]),
                                       levels = args$subgroup_order)
    }

    # Cochrane Handbook 6.5.2.10: combine arms with the same study unit and
    # treat value. ingest_data already does this on the canonical columns,
    # but the user can pick different studlab / treat columns above, which
    # may re-introduce duplicates (e.g. a multi-arm trial where two
    # intervention sub-arms share the same canonical treat label).
    # combine_arms() is pmatools public API as of 0.5.0 (the dot-prefixed
    # .combine_arms() is only a back-compat alias) -- keep the public name.
    n_before <- nrow(study_data)
    study_data <- tryCatch(combine_arms(study_data), error = function(e) study_data)
    if (nrow(study_data) < n_before) {
      unit_label <- if ("outcome" %in% names(study_data)) {
        "(studlab, outcome, treat)"
      } else {
        "(studlab, treat)"
      }
      shiny::showNotification(
        sprintf(
          "Combined %d duplicate %s row%s before meta-analysis (Cochrane Handbook 6.5.2.10).",
          n_before - nrow(study_data),
          unit_label,
          if ((n_before - nrow(study_data)) > 1L) "s" else ""
        ),
        type = "message",
        duration = 6
      )
    }

    # Strip non-run_ma args
    run_args <- list(
      data         = study_data,
      outcome_type = args$outcome_type,
      sm           = args$sm,
      method       = args$method,
      method.tau   = args$method.tau,
      # NULL is run_ma()'s own "decide from k", and the line below this list
      # drops NULL entries, so "auto" reaches run_ma() as no argument at all.
      hakn         = switch(args$random_ci %||% "auto",
                            auto = NULL, hk = TRUE, classic = FALSE),
      random       = args$random,
      common       = args$common,
      incr         = args$incr,
      experimental_label = args$experimental_label,
      control_label      = args$control_label,
      subgroup     = args$subgroup_col
    )
    run_args <- run_args[!vapply(run_args, is.null, logical(1))]

    # The pending press is spent HERE, not at the gate above, and the distance
    # between the two is the point: every exit in between is a cheap guard on
    # something the reviewer is expected to go and fix (a blank required field,
    # arm labels left over from the previous dataset). A press held across
    # those is served the moment the blocker clears, which is what keeps Run
    # analysis from looking inert while arm_assignment_ui re-renders. A press
    # that reaches run_ma() is spent whether the run succeeds or the tryCatch
    # below turns it into a notification: either way it has been answered.
    shiny::isolate(run_clicks_spent(run_clicks))

    shiny::withProgress(
      message = "Running meta-analysis...", value = 0.4,
      tryCatch({
        ma_result <- do.call(run_ma, run_args)
        attr(ma_result, "pmatools_input_data") <- study_data
        attr(ma_result, "pmatools_run_args") <- run_args
        ma_result
      },
        error = function(e) {
          msg <- conditionMessage(e)
          hint <- if (grepl("does not have exactly one intervention", msg, fixed = TRUE)) {
            paste0(
              " Same (studlab, treat) rows are auto-combined per Cochrane ",
              "Handbook 6.5.2.10. If a study still has more than one ",
              "intervention or control arm, pick a single arm per study in the ",
              "intervention/control selectors above, or remove the extra rows."
            )
          } else {
            ""
          }
          shiny::showNotification(
            paste0("Meta-analysis stopped: ", msg, hint),
            type = "warning",
            duration = 10
          )
          NULL
        }
      )
    )
  })

  .rare_mode_on <- function() {
    rare_diagnostics <- state$rare_diagnostics
    if (is.null(rare_diagnostics) || !isTRUE(rare_diagnostics$rare_flow)) return(FALSE)
    if (is.null(input$use_rare_workflow)) {
      isTRUE(state$rare_mode_requested %||% TRUE)
    } else {
      isTRUE(input$use_rare_workflow)
    }
  }

  .rare_primary_choices <- function() {
    c(
      "Beta-binomial with correlated responses" = "BB_CR",
      "Mantel-Haenszel exact, no continuity correction" = "MH_no_cc",
      "GLMM (one-stage logistic random effects)" = "GLMM",
      "Peto" = "Peto",
      "Random-effects IV, treatment-arm correction (DL)" = "REIV_TACC",
      "Random-effects IV, fixed 0.5 correction (DL)" = "REIV_CC",
      "Mantel-Haenszel, fixed 0.5 correction" = "MH_CC"
    )
  }

  .rare_events_control_ui <- function() {
    choices <- .rare_primary_choices()
    selected <- state$rare_primary_method %||%
      shiny::isolate(input$rare_primary_method) %||% "BB_CR"
    if (!selected %in% unname(choices)) selected <- "BB_CR"

    htmltools::tagList(
      htmltools::hr(),
      htmltools::h6("Rare-events workflow"),
      htmltools::div(
        class = "pma-switch-row",
        shiny::checkboxInput("use_rare_workflow",
          "Use rare-events workflow",
          value = isTRUE(state$rare_mode_requested %||% TRUE))
      ),
      shiny::conditionalPanel(
        "input.use_rare_workflow == true",
        htmltools::div(
          class = "rare-primary-method",
          shiny::selectInput("rare_primary_method", "Primary method",
            choices = choices, selected = selected,
            selectize = FALSE, width = "100%")
        ),
        htmltools::p(class = "pma-card-subtitle",
          "Keep the default unless a method was prespecified in the protocol.")
      )
    )
  }

  # Recompute regular MA, rare diagnostics, and rare MA whenever ma() fires.
  # observeEvent(ma()) so the handler only runs when the analysis result
  # changes -- not when input$use_rare_workflow or input$rare_primary_method
  # change (those are handled by the dedicated observers below). Without this
  # split, writes to state$rare_primary_method/state$rare_mode_active here
  # invalidate the rare_events_panel renderUI, which re-creates the
  # checkbox/select inputs, which re-fires this observer -> infinite loop.
  shiny::observeEvent(ma(), {
    obj <- ma()
    if (is.null(obj)) return()
    state$rare_mode_requested <- if (is.null(input$use_rare_workflow)) {
      state$rare_mode_requested %||% TRUE
    } else {
      isTRUE(input$use_rare_workflow)
    }
    state$regular_ma <- obj
    state$rare <- NULL
    state$rare_primary_method <- NULL
    state$rare_mode_active <- FALSE

    run_args <- attr(obj, "pmatools_run_args")
    input_data <- attr(obj, "pmatools_input_data")
    checked_rare <- FALSE
    if (!is.null(run_args) && identical(run_args$outcome_type, "binary") &&
        !is.null(input_data)) {
      checked_rare <- TRUE
      diag <- tryCatch(
        rare_event_diagnostics(
          input_data,
          experimental_label = run_args$experimental_label,
          control_label = run_args$control_label
        ),
        error = function(e) NULL
      )
      state$rare_diagnostics <- diag
      if (!is.null(diag) && isTRUE(diag$rare_flow)) {
        primary_method <- input$rare_primary_method %||% "BB_CR"
        rare <- shiny::withProgress(
          message = "Running rare-events method suite...",
          detail = "Comparing sparse-data methods; this can take a while.",
          value = 0.4,
          tryCatch(
            run_rare_ma(
              input_data,
              effect_scale = "OR",
              primary_method = primary_method,
              random = isTRUE(run_args$random),
              common = isTRUE(run_args$common),
              method.tau = run_args$method.tau %||% "REML",
              experimental_label = run_args$experimental_label,
              control_label = run_args$control_label
            ),
            error = function(e) {
              shiny::showNotification(
                paste("Rare-event analysis failed:", conditionMessage(e)),
                type = "warning"
              )
              NULL
            }
          )
        )
        if (!is.null(rare) && inherits(rare$primary, "meta")) {
          state$rare <- rare
          state$rare_primary_method <- rare$primary_method
          if (isTRUE(.rare_mode_on())) {
            state$ma <- rare$primary
            state$ma_blocked <- NULL
            state$rare_mode_active <- TRUE
            return()
          }
        }
      }
    }
    if (!isTRUE(checked_rare)) state$rare_diagnostics <- NULL
    state$ma <- obj
    # Every writer of state$ma clears the blocked record alongside it, so
    # Step 3 can never explain a NULL that is no longer NULL.
    state$ma_blocked <- NULL
  }, ignoreNULL = FALSE)

  # User toggled the rare-workflow checkbox: swap state$ma between cached
  # regular and rare results without re-running run_ma / rare_event_diagnostics.
  shiny::observeEvent(input$use_rare_workflow, {
    requested <- isTRUE(input$use_rare_workflow)
    state$rare_mode_requested <- requested
    diag_flow <- isTRUE(state$rare_diagnostics$rare_flow)
    if (requested && diag_flow && !is.null(state$rare) &&
        inherits(state$rare$primary, "meta")) {
      state$ma <- state$rare$primary
      state$ma_blocked <- NULL
      state$rare_mode_active <- TRUE
    } else if (!is.null(state$regular_ma)) {
      state$ma <- state$regular_ma
      state$ma_blocked <- NULL
      state$rare_mode_active <- FALSE
    }
  }, ignoreInit = TRUE)

  # User picked a different rare-event primary method: re-run run_rare_ma()
  # with the new choice. Cached regular_ma + diagnostics drive the inputs, so
  # the heavy ma() reactive does not have to fire again.
  shiny::observeEvent(input$rare_primary_method, {
    method_id <- input$rare_primary_method
    if (is.null(method_id) || !nzchar(method_id)) return()
    if (!isTRUE(state$rare_diagnostics$rare_flow)) return()
    obj <- state$regular_ma
    if (is.null(obj)) return()
    run_args <- attr(obj, "pmatools_run_args")
    input_data <- attr(obj, "pmatools_input_data")
    if (is.null(run_args) || is.null(input_data) ||
        !identical(run_args$outcome_type, "binary")) return()
    rare <- shiny::withProgress(
      message = "Recomputing rare-events method suite...",
      detail = "Applying the new primary method.",
      value = 0.4,
      tryCatch(
        run_rare_ma(
          input_data,
          effect_scale = "OR",
          primary_method = method_id,
          random = isTRUE(run_args$random),
          common = isTRUE(run_args$common),
          method.tau = run_args$method.tau %||% "REML",
          experimental_label = run_args$experimental_label,
          control_label = run_args$control_label
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Rare-event recompute failed:", conditionMessage(e)),
            type = "warning"
          )
          NULL
        }
      )
    )
    if (!is.null(rare) && inherits(rare$primary, "meta")) {
      state$rare <- rare
      state$rare_primary_method <- rare$primary_method
      if (isTRUE(state$rare_mode_active)) {
        state$ma <- rare$primary
        state$ma_blocked <- NULL
      }
    }
  }, ignoreInit = TRUE)

  # Rare events detected: default the auto-rerun toggle OFF, once per
  # detection episode. The multi-method rare-event suite is expensive, so
  # silently re-running it on every input change is a poor default on the
  # shared shinyapps.io tier; the user can re-enable the checkbox at any
  # time. Loop safety follows the same conventions as the observers above:
  # this observer only writes the checkbox input (never state$ma /
  # state$rare / state$rare_diagnostics), the ma() reactive reads
  # input$auto_rerun through isolate() so the update cannot re-trigger the
  # heavy chain, and the local reactiveVal is read/written under isolate()
  # so the observer depends only on state$rare_diagnostics.
  auto_rerun_rare_defaulted <- shiny::reactiveVal(FALSE)
  shiny::observe({
    diag <- state$rare_diagnostics
    if (is.null(diag) || !isTRUE(diag$rare_flow)) {
      # Non-rare (or cleared) diagnostics: re-arm for the next rare dataset.
      shiny::isolate(auto_rerun_rare_defaulted(FALSE))
      return()
    }
    if (isTRUE(shiny::isolate(auto_rerun_rare_defaulted()))) return()
    shiny::isolate(auto_rerun_rare_defaulted(TRUE))
    if (isTRUE(shiny::isolate(input$auto_rerun))) {
      shiny::updateCheckboxInput(session, "auto_rerun", value = FALSE)
      shiny::showNotification(
        paste0(
          "Rare events detected: 'Auto-rerun on change' has been switched ",
          "off because the rare-event method suite is computationally ",
          "expensive. Click 'Run analysis' after changing settings, or ",
          "re-enable auto-rerun if you prefer."
        ),
        type = "message", duration = 10
      )
    }
  })

  .pct <- function(x, digits = 2) {
    if (is.null(x) || length(x) == 0 || !is.finite(x)) return("NA")
    paste0(format(round(100 * x, digits), nsmall = digits), "%")
  }

  output$rare_events_panel <- shiny::renderUI({
    rare_diagnostics <- state$rare_diagnostics
    if (is.null(rare_diagnostics) || !isTRUE(rare_diagnostics$rare_flow)) return(NULL)
    pma_card(
      id = "rare-events-detected-card",
      title = "Rare events suspected",
      htmltools::p(class = "pma-card-subtitle",
        "Rare-events mode uses OR and compares sparse-data methods. ",
        "The default primary method is beta-binomial with correlated responses; ",
        "use a prespecified method if the protocol names one."
      ),
      htmltools::tags$ul(
        style = "font-size: 0.82rem; margin-bottom: 0.4rem;",
        htmltools::tags$li(sprintf("Overall event rate: %s",
                                   .pct(rare_diagnostics$event_rate_overall))),
        htmltools::tags$li(sprintf("Single-zero studies: %d", rare_diagnostics$single_zero_k)),
        htmltools::tags$li(sprintf("Double-zero studies: %d", rare_diagnostics$double_zero_k)),
        htmltools::tags$li(sprintf("Total events: %d", rare_diagnostics$total_events))
      ),
      if (isTRUE(rare_diagnostics$very_sparse_flag)) {
        htmltools::p(class = "pma-card-subtitle",
          style = "font-weight: 600; color: #92400e;",
          "Very sparse warning: interpret pooled estimates with extra caution."
        )
      } else NULL,
      htmltools::p(class = "pma-card-subtitle",
        "Mode: ",
        if (isTRUE(state$rare_mode_active)) "Rare-events workflow" else "Regular workflow"
      ),
      pma_reference(
        "Efthimiou O. Evid Based Ment Health. 2018; ",
        "Tsujimoto Y, et al. Res Synth Methods. 2024."
      ),
      .rare_events_control_ui()
    )
  })

  output$rare_sensitivity_block <- shiny::renderUI({
    rare <- state$rare
    if (is.null(rare) || !isTRUE(state$rare_mode_active)) return(NULL)
    htmltools::tagList(
      htmltools::hr(),
      htmltools::h5("Rare-events sensitivity forest"),
      htmltools::div(
        class = "pma-forest-image",
        shinycssloaders::withSpinner(
          shiny::imageOutput("rare_sensitivity_forest", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px")
      ),
      htmltools::tags$details(
        style = "margin-top: 0.5rem;",
        htmltools::tags$summary("Method table"),
        htmltools::div(DT::DTOutput("rare_method_table"))
      )
    )
  })

  output$rare_sensitivity_forest <- shiny::renderImage({
    rare <- state$rare
    if (is.null(rare)) {
      return(list(src = "", contentType = "image/png", width = "100%"))
    }
    xlim <- NULL
    lo <- input$xlim_lo; hi <- input$xlim_hi
    if (!is.null(lo) && !is.null(hi) &&
        !is.na(lo) && !is.na(hi) && lo < hi) {
      xlim <- c(lo, hi)
    }
    pma_render_trimmed(
      width = 1400,
      height = 900,
      plot_fn = function() {
        plot_rare_sensitivity_forest(
          rare,
          title = if (nzchar(input$forest_title %||% "")) {
            paste(input$forest_title, "method sensitivity", sep = " - ")
          } else {
            "Rare-event method sensitivity"
          },
          xlim = xlim,
          favors_left = if (nzchar(input$favors_left %||% "")) input$favors_left else NULL,
          favors_right = if (nzchar(input$favors_right %||% "")) input$favors_right else NULL
        )
      }
    )
  }, deleteFile = TRUE)

  output$rare_method_table <- DT::renderDT({
    rare <- state$rare
    if (is.null(rare)) return(DT::datatable(data.frame()))
    tab <- as.data.frame(rare$method_table, stringsAsFactors = FALSE)
    for (nm in c("estimate", "ci_low", "ci_high")) {
      tab[[nm]] <- ifelse(is.na(tab[[nm]]), NA, round(tab[[nm]], 3))
    }
    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(paging = FALSE, searching = FALSE, info = FALSE,
                     scrollX = TRUE, dom = "t")
    )
  })

  output$forest_plot <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }

    xlim <- NULL
    lo <- input$xlim_lo; hi <- input$xlim_hi
    if (!is.null(lo) && !is.null(hi) &&
        !is.na(lo) && !is.na(hi) && lo < hi) {
      xlim <- c(lo, hi)
    }

    pma_render_trimmed(
      width  = 1400,
      height = 200 + 80 * (obj$k %||% 0L) + 400,  # generous; trimmed afterwards
      plot_fn = function() {
        plot_forest(
          obj,
          title              = if (nzchar(input$forest_title %||% "")) input$forest_title else NULL,
          label_e            = if (nzchar(input$label_e %||% ""))      input$label_e      else NULL,
          label_c            = if (nzchar(input$label_c %||% ""))      input$label_c      else NULL,
          xlim               = xlim,
          # One checkbox drives both arguments; see the UI comment.
          show_n             = isTRUE(input$show_arm_columns %||% TRUE),
          show_events        = isTRUE(input$show_arm_columns %||% TRUE),
          favors_left        = if (nzchar(input$favors_left %||% ""))  input$favors_left  else NULL,
          favors_right       = if (nzchar(input$favors_right %||% "")) input$favors_right else NULL,
          addrow_above       = pma_addrow_above(input$addrows_above_overall),
          addrow_below       = pma_addrow_below(input$addrows_below_overall),
          digits_mean        = pma_forest_digits(input$digits_mean),
          digits_sd          = pma_forest_digits(input$digits_sd)
        )
      }
    )
  }, deleteFile = TRUE)

  # Mirror Forest plot display options into state for export
  shiny::observe({
    pick_text <- function(id) {
      v <- input[[id]]
      if (is.null(v) || !nzchar(v)) NULL else v
    }
    lo <- input$xlim_lo; hi <- input$xlim_hi
    xlim_is_usable <- !is.null(lo) && !is.null(hi) &&
      !is.na(lo) && !is.na(hi) && lo < hi
    xlim <- if (xlim_is_usable) c(lo, hi) else NULL
    state$display$forest_step2 <- list(
      title        = pick_text("forest_title"),
      label_e      = pick_text("label_e"),
      label_c      = pick_text("label_c"),
      favors_left  = pick_text("favors_left"),
      favors_right = pick_text("favors_right"),
      xlim         = xlim,
      # The Step 2 body may not have been rendered yet, so an absent checkbox
      # must fall back to the UI default (TRUE) rather than to isTRUE(NULL).
      # Both keys are written from the single checkbox: R/step4_export.R reads
      # show_n and show_events off this list and must keep working unchanged.
      show_n       = isTRUE(input$show_arm_columns %||% TRUE),
      show_events  = isTRUE(input$show_arm_columns %||% TRUE),
      addrow_above = pma_addrow_above(input$addrows_above_overall),
      addrow_below = pma_addrow_below(input$addrows_below_overall),
      digits_mean  = pma_forest_digits(input$digits_mean),
      digits_sd    = pma_forest_digits(input$digits_sd)
    )
  })

  # Mirror the outcome identity and the arm labels into state so Step 3 and
  # Step 4 can read them while the Step 2 widgets do not exist.
  #
  # NULL / empty is never written back. Leaving and re-entering Step 2 tears
  # the widgets down and rebuilds them, and a freshly built widget pushes its
  # own default to the server before step2_ui()'s seeding has any effect; a
  # blank write at that moment would destroy exactly the values Step 3 needs.
  # The cost is an asymmetry - clearing "Outcome name" in Step 2 leaves
  # state$outcome_name at its previous value - which is harmless because the
  # required-field checks read input$ directly, and step2_ui() reseeds from
  # state only when a non-empty value is there.
  shiny::observe({
    nm <- input$outcome_name
    if (!is.null(nm) && length(nm) == 1 && nzchar(trimws(nm))) {
      state$outcome_name <- trimws(nm)
    }
    sv <- input$small_values
    if (!is.null(sv) && length(sv) == 1 && nzchar(sv)) state$small_values <- sv
    ot <- input$outcome_type
    if (!is.null(ot) && length(ot) == 1 && nzchar(ot)) state$outcome_type <- ot
    # Follow-up and unit are optional, so an empty value is a legitimate
    # answer and IS written back - otherwise a follow-up could never be
    # cleared. Safe here because step2_ui() seeds both widgets from state, so
    # a rebuilt widget pushes back the value state already holds; only the
    # NULL a torn-down widget reports is ignored.
    fu <- input$outcome_follow_up
    if (!is.null(fu) && length(fu) == 1 && !is.na(fu)) {
      state$outcome_follow_up <- trimws(fu)
    }
    un <- input$outcome_unit
    if (!is.null(un) && length(un) == 1 && !is.na(un)) {
      state$outcome_unit <- trimws(un)
    }
    ae <- input$experimental_label
    if (!is.null(ae) && length(ae) == 1 && nzchar(ae)) state$arm_e <- ae
    ac <- input$control_label
    if (!is.null(ac) && length(ac) == 1 && nzchar(ac)) state$arm_c <- ac
  })

  # Smart defaults for the Forest plot display panel: the outcome name becomes
  # the title, the two arm selectors become the arm labels, and the outcome
  # direction decides which side each "Favors ..." label goes on. Nothing the
  # user typed is ever overwritten (see pma_autofill_text()).
  .forest_label_defaults <- shiny::reactive({
    iv  <- input$experimental_label %||% state$arm_e %||% ""
    ct  <- input$control_label      %||% state$arm_c %||% ""
    fav <- pma_favors_labels(state$small_values, iv, ct)
    list(title = state$outcome_name %||% "", label_e = iv, label_c = ct,
         favors_left = fav$left, favors_right = fav$right)
  })
  pma_autofill_forest_panel(input, session, prefix = NULL,
                            values_fn = .forest_label_defaults)

  output$funnel_plot <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    da <- pma_funnel_display_args(input, "funnel_step2")
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

  .results_text <- function(obj) {
    body <- utils::capture.output(summary(obj))
    safe_ver <- function(pkg, fallback = "(vendored)") {
      tryCatch(as.character(utils::packageVersion(pkg)),
               error = function(e) fallback)
    }
    footer <- c(
      "",
      "--- Software versions ---",
      # pmatools is vendored, not installed: resolved via the shared helper
      # in ui_helpers.R, which consults options(pmatools.version_stamp).
      sprintf("pmatools : %s", pma_pmatools_version()),
      sprintf("meta     : %s", safe_ver("meta")),
      sprintf("R        : %s", paste(R.version$major, R.version$minor, sep = "."))
    )
    paste(c(body, footer), collapse = "\n")
  }

  # Drives the two conditionalPanels in the right pane: the one-line "Press Run
  # analysis" placeholder before the first run, the Results card after it. A
  # plain output flag rather than a renderUI swap, because the Results card
  # holds the forest-display widgets and re-rendering it would reset every one
  # of them. suspendWhenHidden = FALSE because the flag itself is never on
  # screen, so Shiny would otherwise never compute it.
  output$pma_has_ma <- shiny::reactive(!is.null(state$ma))
  shiny::outputOptions(output, "pma_has_ma", suspendWhenHidden = FALSE)

  output$ma_model_summary <- shiny::renderUI({
    line <- step2_model_summary_line(state$ma)
    if (is.null(line)) return(NULL)
    htmltools::p(class = "pma-card-subtitle", style = "margin-bottom: 0.75rem;",
                 line)
  })

  output$ma_summary <- shiny::renderPrint({
    obj <- state$ma
    if (is.null(obj)) return(cat("No analysis run yet."))
    cat(.results_text(obj))
  })

  shiny::observeEvent(input$copy_results, {
    obj <- state$ma
    if (is.null(obj)) {
      shiny::showNotification("Run analysis first.", type = "warning")
      return()
    }
    session$sendCustomMessage("copy_to_clipboard", .results_text(obj))
  })

  # ----- Required-field highlighting -------------------------------------
  # Same `ever_run_requested` idea as ma() and state$step2_commit below: a
  # latch is what is wanted here too, and for the same reason. Nothing is
  # marked red until the user has actually asked for an analysis (Run
  # analysis, or Next, which runs the commit hook). A first page load with
  # both fields empty is a normal state, not an error.
  required_touched <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$run_ma, {
    required_touched(TRUE)
  }, ignoreInit = TRUE)

  # Called by app.R's begin_new_outcome(). Starting a new outcome empties the
  # two required fields on purpose, so the marks are disarmed again rather
  # than painting the fresh form red before the reviewer has typed anything.
  state$step2_reset <- function() required_touched(FALSE)

  # The ids managed here; `unset` is recomputed from input$ on every change,
  # so the marks clear the moment a field is filled.
  # PMA_STEP2_REQUIRED and pma_step2_required_unset() live in R/ui_helpers.R,
  # so the rule the marks are painted from is testable without a session.
  #
  # TWO TIERS, and the split is the point. `unset` is now computed
  # unconditionally, so a fresh form wears a MUTED "required" pill from the
  # first paint and the reviewer can see what is being asked of them.
  # `armed` is the old required_touched() gate, and it is what turns that pill
  # destructive-red: still nothing painted red until the reviewer has actually
  # asked for an analysis, which is the behaviour the comment above defends.
  #
  # The column-mapping selects ride on the same message (see
  # PMA_STEP2_MAPPING_ALL in R/ui_helpers.R). They are required in exactly the
  # same sense, and since the accordion can hide them, the client needs to be
  # told which are blank in order to open the panel that hides one.
  shiny::observe({
    unset <- c(
      pma_step2_required_unset(input$outcome_name, input$small_values),
      pma_step2_mapping_unset(
        input$outcome_type,
        list(col_studlab = input$col_studlab, col_treat = input$col_treat,
             col_n       = input$col_n,       col_event = input$col_event,
             col_mean    = input$col_mean,    col_sd    = input$col_sd))
    )
    # as.list() so a single id (or none) still serialises as a JSON array.
    session$sendCustomMessage(
      "pma_required_fields",
      list(all   = as.list(c(PMA_STEP2_REQUIRED, PMA_STEP2_MAPPING_ALL)),
           unset = as.list(unset),
           armed = isTRUE(required_touched()))
    )
  })

  # Same three conditions state$step2_commit enforces below, in the same
  # order, read off the same inputs: both required fields, then an analysis.
  # Keeping the two in lockstep is what stops the button and the toast from
  # disagreeing. (input$, not state$: the mirrored copies are only refreshed
  # when an analysis runs, so they lag what the user has just typed.)
  step2_can_advance <- shiny::reactive({
    sv <- input$small_values
    nzchar(trimws(input$outcome_name %||% "")) &&
      !is.null(sv) && length(sv) == 1L && nzchar(sv) &&
      !is.null(state$ma)
  })

  # Wizard nav as its own output: re-rendering it when the preconditions flip
  # leaves the rest of the step body, and every value typed into it, alone.
  output$step2_nav <- shiny::renderUI({
    pma_wizard_nav(current_step = 2,
                   next_disabled = !isTRUE(step2_can_advance()))
  })

  # Advance hook for step dispatcher
  state$step2_commit <- function() {
    # Pressing Next counts as asking for the analysis, so it arms the
    # required-field marks too.
    required_touched(TRUE)
    # Outcome identity is checked before the analysis, because "run the
    # analysis first" would be misleading advice when the reason no analysis
    # exists is a blank required field.
    missing_required <- character()
    if (!nzchar(trimws(input$outcome_name %||% ""))) {
      missing_required <- c(missing_required, "Outcome name")
    }
    sv <- input$small_values
    if (is.null(sv) || length(sv) != 1L || !nzchar(sv)) {
      missing_required <- c(missing_required, "Direction (smaller = favorable?)")
    }
    if (length(missing_required) > 0) {
      shiny::showNotification(
        paste("Complete required field(s):",
              paste(missing_required, collapse = ", ")),
        id = "step2_required_fields", type = "warning", duration = 8
      )
      return(FALSE)
    }
    if (is.null(state$ma)) {
      shiny::showNotification(
        "Please run the analysis first.", type = "warning"
      )
      return(FALSE)
    }
    TRUE
  }
}
