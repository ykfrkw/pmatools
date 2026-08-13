# app.R - pmatools wizard (4 steps): Data -> MA -> GRADE -> Export
#
# Backend: pmatools sources are vendored under R/_pmatools/ rather than
#   installed from a package, because shinyapps.io's free / standard tier
#   build server caches a stale GITHUB_PAT that returns HTTP 401 when it
#   tries to remotes::install_github(ykfrkw/pmatools). Shipping the sources
#   in the bundle bypasses that install step entirely.
# Deployment: shinyapps.io (account: yuki-furukawa)

library(shiny)
library(bslib)
library(htmltools)
library(DT)
library(flextable)

# Cap uploads at 10 MB (shinyapps.io free tier is memory-constrained; a
# study-level long-format table should never come close to this).
options(shiny.maxRequestSize = 10 * 1024^2)

# Source vendored pmatools (~17 files); order doesn't matter because each
# file defines functions only.
for (f in list.files("R/_pmatools", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

# Tell pmatools which version it is. Because the sources above are sourced
# and not installed, utils::packageVersion("pmatools") always errors here;
# R/_pmatools/VERSION (written by stage_bundle.R) is the only record of what
# was staged. A missing, unreadable or blank file leaves the option unset,
# and callers fall back to "(vendored; version unknown)".
local({
  vfile <- "R/_pmatools/VERSION"
  if (!file.exists(vfile)) return(invisible(NULL))
  ver <- tryCatch(trimws(readLines(vfile, n = 1L, warn = FALSE)[1L]),
                  error = function(e) NA_character_)
  if (length(ver) == 1L && !is.na(ver) && nzchar(ver)) {
    options(pmatools.version_stamp = ver)
  }
})

# The staged templates and sample data are addressed relative to the app
# directory -- getOption("pmatools.vendored_root") defaults to "." and
# R/step1_data.R reads _pmatools_inst/extdata/ the same way. Pin the directory
# at startup so a later setwd() (ours, Shiny's, or a package's) cannot move
# the target out from under them.
options(pmatools.vendored_root = normalizePath(getwd(), winslash = "/"))

# Source local Shiny modules
local_files <- c(
  "R/educational_copy.R",
  "R/ui_helpers.R",
  "R/step1_data.R",
  "R/step2_ma.R",
  "R/step3_threshold.R",
  "R/step3_grade.R",
  "R/step4_export.R"
)
for (f in local_files) source(f, local = TRUE)

# ================== UI ==================

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(
    version    = 5,
    bootswatch = "default",
    primary    = "#0f172a"
  ),
  htmltools::tags$head(
    htmltools::includeCSS("www/shadcn.css"),
    # 埋め込み親ページ（yukifurukawa.jp/pmatools/）へ中身の高さを通知する。
    # www/ はアプリのルートで配信されるので src はファイル名だけでよい。
    htmltools::tags$script(src = "embed-height.js"),
    # iframe が中身の実高まで伸びると内部にスクロール余地が無くなり
    # window.scrollTo が事実上効かないので、埋め込み時は親にも通知する。
    htmltools::tags$script(htmltools::HTML(
      "Shiny.addCustomMessageHandler('scroll_top', function(_msg){
         window.scrollTo({top:0, behavior:'smooth'});
         if (typeof window.pmaNotifyScrollTop === 'function') { window.pmaNotifyScrollTop(); }
       });
       Shiny.addCustomMessageHandler('copy_to_clipboard', function(text){
         if (navigator.clipboard && navigator.clipboard.writeText) {
           navigator.clipboard.writeText(text).then(function(){
             Shiny.setInputValue('clipboard_copied', Math.random());
           });
         } else {
           var ta = document.createElement('textarea');
           ta.value = text; document.body.appendChild(ta);
           ta.select(); document.execCommand('copy');
           document.body.removeChild(ta);
           Shiny.setInputValue('clipboard_copied', Math.random());
         }
       });"
    ))
  ),

  htmltools::div(
    style = "padding: 1.5rem 0; border-bottom: 1px solid hsl(var(--border)); margin-bottom: 1rem;",
    htmltools::h1("pmatools",
                  style = "margin: 0; font-size: 1.5rem; display: inline-block;"),
    htmltools::span(
      style = "color: hsl(var(--muted-foreground)); font-size: 0.95rem; margin-left: 0.75rem;",
      "pairwise meta-analysis with Core GRADE"
    )
  ),

  shiny::uiOutput("stepper_ui"),
  shiny::uiOutput("step_body"),

  htmltools::hr(),
  htmltools::div(
    style = "color: hsl(var(--muted-foreground)); font-size: 0.8rem; padding: 1rem 0;",
    htmltools::HTML(
      "Powered by <a href='https://yukifurukawa.jp/pmatools/' target='_blank'>yukifurukawa.jp/pmatools/</a>."
    ),
    # Which pmatools produced the numbers on screen. It was reachable only by
    # opening Step 2's "Text results" tab, which is no place to look for the
    # version of the tool you are citing. pma_pmatools_version() is the one
    # supported way to ask -- the app sources pmatools instead of installing
    # it, so utils::packageVersion() errors here.
    htmltools::span(style = "margin-left: 0.5rem;",
                    paste0("pmatools ", pma_pmatools_version()))
  )
)

# ================== SERVER ==================

server <- function(input, output, session) {

  state <- shiny::reactiveValues(
    step           = 1L,
    data           = NULL,
    data_edits     = NULL,
    rob_table      = NULL,
    ma             = NULL,
    # Why `ma` is NULL, when Step 2 knows: a character vector of the required
    # Step 2 fields that were empty at the moment the analysis was withdrawn
    # (see step3_blocked_message() in step3_threshold.R). Cleared by every
    # writer that sets a non-NULL `ma`. Step 3 reads it so a blanked field
    # cannot show up there as "Run analysis and configure domains."
    ma_blocked     = NULL,
    regular_ma     = NULL,
    rare           = NULL,
    rare_diagnostics = NULL,
    rare_mode_requested = TRUE,
    rare_mode_active = FALSE,
    grade          = NULL,
    # Certainty assessments banked on the Step 3 "Final certainty" tab, as a
    # named list of pmatools objects keyed by outcome label (see
    # pma_outcomes_list() in ui_helpers.R), plus the subset the reviewer has
    # marked primary for the combined Summary of Findings table.
    outcomes       = NULL,
    sof_primary    = character(0),
    # Which certainty domains have been reviewed for the outcome currently
    # open; written by step3_server(), read by Step 4's export gate.
    domain_confirmed = NULL,
    # Where "some concerns" falls on the binary risk-of-bias split. Review-wide
    # rather than outcome-scoped: it is a convention the reviewer sets once for
    # the whole review, so begin_new_outcome() deliberately leaves it alone.
    # Mirrored here because the Step 3 widget is destroyed whenever another
    # step's body renders, and a rebuilt widget pushes its default back.
    rob_some_concerns = "high",
    # Outcome provenance. `outcome_sig` is pma_analysis_signature() of the
    # analysis the Step 3 answers were given for, and `outcome_gen` counts how
    # many outcomes this session has begun. Every Step 3 answer is stamped
    # with the generation it was given in, so an answer left behind by the
    # previous outcome can be told apart from one given for this one.
    outcome_sig    = NULL,
    outcome_gen    = 1L,
    # Outcome identity, collected in Step 2 and consumed by Step 3 / Step 4.
    # Held in state (not read straight off input$) because the Step 2 widgets
    # are destroyed whenever another step's body is rendered.
    outcome_name   = NULL,
    small_values   = NULL,
    # Binary / continuous. Mirrored for the same reason as the two above: the
    # Step 2 radio is destroyed whenever another step's body renders, and a
    # rebuilt widget pushes its own default back to the server.
    outcome_type   = NULL,
    # Optional presentation fields for the Core GRADE 6 Summary of Findings
    # table: the follow-up printed under the outcome name, and the unit of a
    # continuous scale. Collected in Step 2 beside the outcome name because
    # they describe the outcome, not the table; copied onto each saved
    # assessment in Step 3 so the Step 4 combined table can show a different
    # follow-up per row.
    outcome_follow_up = NULL,
    outcome_unit      = NULL,
    arm_e          = NULL,
    arm_c          = NULL,
    display = list(
      per                = 1000,
      prediction         = FALSE,
      convert            = FALSE,
      baseline_risk      = NULL,
      threshold_label    = NULL,
      follow_up          = NULL,
      unit               = NULL
    )
  )

  # Stepper indicator. The Certainty node carries how much of Step 3 is
  # confirmed, so the count is readable from the other three steps - Step 4's
  # download lock is the same number said again.
  #
  # Not before Step 3 has been opened, though. step3_server() is wired at
  # startup and writes state$domain_confirmed immediately, so without this the
  # stepper reads "Certainty 0/6" to someone who has not yet loaded a dataset,
  # and "six of what?" is the wrong first question to raise. Same "seen it
  # yet?" semantics as the dot on a domain tab.
  certainty_opened <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(state$step, {
    if (identical(as.integer(state$step), 3L)) certainty_opened(TRUE)
  }, ignoreInit = FALSE)

  output$stepper_ui <- shiny::renderUI({
    confirmed <- if (isTRUE(certainty_opened())) {
      length(PMA_DOMAIN_LABELS) -
        length(pma_unconfirmed_domains(state$domain_confirmed))
    } else NULL
    pma_stepper(state$step, certainty_confirmed = confirmed)
  })

  # Render the current step body
  output$step_body <- shiny::renderUI({
    switch(state$step,
      `1` = step1_ui(),
      `2` = step2_ui(state),
      `3` = step3_ui(state),
      `4` = step4_ui(),
      step1_ui()
    )
  })

  # ================== Orientation modal ==================
  #
  # Shown once, at the start of the session, and never again. The claim it
  # carries used to head Step 1 as body copy, where it was reprinted on every
  # return to the step and read by nobody.
  #
  # The guard is a session-scoped reactiveVal on purpose. localStorage or a
  # cookie would suppress the modal for a returning reviewer, and a returning
  # reviewer is a new session rating a new review, not someone who has already
  # been told today. An observe() with no reactive dependency also fires only
  # once, but the flag says so out loud and keeps that true if this ever gains
  # a dependency.
  intro_modal_shown <- shiny::reactiveVal(FALSE)
  shiny::observe({
    if (isTRUE(shiny::isolate(intro_modal_shown()))) return()
    intro_modal_shown(TRUE)
    shiny::showModal(shiny::modalDialog(
      title     = EDU_COPY$intro_modal$title,
      EDU_COPY$intro_modal$body,
      easyClose = TRUE,
      footer    = shiny::modalButton(EDU_COPY$intro_modal$dismiss)
    ))
  })

  # Wire up each step's server logic
  step1_server(input, output, session, state)
  step2_server(input, output, session, state)
  step3_server(input, output, session, state)
  step4_server(input, output, session, state)

  # ================== Starting a new outcome ==================
  #
  # A Summary of Findings table has one row per patient-important outcome, so
  # a session normally rates several. Everything that belongs to the outcome
  # being rated is cleared in ONE place, whether the reviewer got there by
  # pressing "+ Add next outcome" or by walking back to Step 2 and changing
  # the analysis - the stepper allows free jumping, so a button-only reset
  # would leave the second route unguarded.
  #
  # `identity = TRUE` additionally clears the Step 2 outcome fields and the
  # analysis itself. The provenance guard below leaves those alone, because
  # the reviewer is standing in Step 2 having just typed them.
  #
  # Never cleared, in either mode: the loaded data, the per-study risk-of-bias
  # and indirectness table (properties of the studies, not of the outcome),
  # the saved outcomes, and the forest / funnel display preferences.
  begin_new_outcome <- function(identity = FALSE) {
    state$outcome_gen      <- (state$outcome_gen %||% 1L) + 1L
    state$grade            <- NULL
    state$domain_confirmed <- NULL
    state$pubias_missing   <- NULL
    # Presentation values describing the outcome rather than a plot.
    state$display$threshold_label <- NULL
    state$display$baseline_risk   <- NULL
    state$display$convert         <- FALSE
    state$display$chinn_invert    <- FALSE
    state$display$follow_up       <- NULL
    state$display$unit            <- NULL
    if (isTRUE(identity)) {
      state$outcome_name       <- NULL
      state$small_values       <- NULL
      state$outcome_type       <- NULL
      state$outcome_follow_up  <- NULL
      state$outcome_unit       <- NULL
      state$ma                 <- NULL
      state$ma_blocked         <- NULL
      state$regular_ma         <- NULL
      state$rare               <- NULL
      state$rare_diagnostics   <- NULL
      state$rare_mode_active   <- FALSE
      state$rare_primary_method <- NULL
      # Cleared last: with no analysis the signature is unknown, and the guard
      # must not read the next one as a second change.
      state$outcome_sig        <- NULL
    }
    # Step-local server state that no input carries (the Step 3 threshold
    # reactiveVals, Step 2's required-field arming).
    if (is.function(state$step2_reset)) state$step2_reset()
    if (is.function(state$step3_reset)) state$step3_reset()
    # Untick the confirmation boxes that are on screen right now. The rest of
    # Step 3 clears itself: output$step_body rebuilds the step from step3_ui()
    # on every entry, and a freshly built widget pushes its own declared
    # default back to the server, so no default has to be restated here.
    pma_clear_outcome_confirmations(session)
  }

  # Provenance guard. pma_analysis_signature() defines what makes an outcome a
  # different outcome; see the long note at its definition in ui_helpers.R.
  outcome_signature <- shiny::reactive(
    pma_analysis_signature(state$ma, state$small_values))

  shiny::observeEvent(outcome_signature(), {
    sig <- outcome_signature()
    if (is.na(sig)) return()
    if (identical(state$outcome_sig, sig)) return()
    changed <- !is.null(state$outcome_sig)
    state$outcome_sig <- sig
    if (!changed) return()   # first analysis of a fresh outcome, nothing to void
    begin_new_outcome(identity = FALSE)
    shiny::showNotification(
      paste0("This is a different outcome from the one Step 3 was answered ",
             "for, so the certainty assessment has been cleared. Saved ",
             "outcomes, the loaded data and the per-study risk-of-bias and ",
             "indirectness ratings are untouched."),
      id = "pma_outcome_changed", type = "warning", duration = 10)
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$add_next_outcome, {
    begin_new_outcome(identity = TRUE)
    state$step <- 2L
    session$sendCustomMessage("scroll_top", list())
    shiny::showNotification(
      paste0("Ready for the next outcome. Name it, set its direction and ",
             "follow-up, map its columns and run the analysis; the saved ",
             "outcomes and the per-study ratings are kept."),
      type = "message", duration = 8)
  }, ignoreInit = TRUE)

  # Single dispatcher for Next / Back to avoid observer cascade
  shiny::observeEvent(input$btn_next, {
    cur <- shiny::isolate(state$step)
    commit <- switch(as.character(cur),
      "1" = state$step1_commit,
      "2" = state$step2_commit,
      "3" = state$step3_commit,
      function() TRUE)
    if (!is.null(commit) && isTRUE(commit())) {
      state$step <- min(cur + 1L, 4L)
      session$sendCustomMessage("scroll_top", list())
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$btn_back, {
    cur <- shiny::isolate(state$step)
    state$step <- max(cur - 1L, 1L)
    session$sendCustomMessage("scroll_top", list())
  }, ignoreInit = TRUE)

  # Stepper click -> jump to step (always allowed; user controls flow)
  lapply(1:4, function(i) {
    shiny::observeEvent(input[[paste0("step_jump_", i)]], {
      state$step <- i
      session$sendCustomMessage("scroll_top", list())
    }, ignoreInit = TRUE)
  })

  # Notification when clipboard copy succeeds
  shiny::observeEvent(input$clipboard_copied, {
    shiny::showNotification("Copied to clipboard.",
                            type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # Mirror display options into state for export
  shiny::observe({
    # The Configuration radio yields a character; sof_table(per =) and
    # export_bundle(per =) want a number, and step3_per_unit() is the one
    # place that validates the pair of units the app offers.
    state$display$per             <- step3_per_unit(input$per)
    state$display$prediction      <- isTRUE(input$prediction)
    state$display$threshold_label <- input$threshold_label
    # convert / baseline_risk / chinn_invert are written by step3_server()
    # instead of being read straight off input$ here. The reviewer's choice
    # arrives as input$sof_presentation, but sof_table() aborts when the
    # responder route is taken and the summary measure or the control-group
    # proportion does not support the conversion, and Step 3 is where those
    # preconditions are known - so state$display$convert is the GUARDED
    # boolean, not a mirror of the radio. chinn_invert has no widget at all,
    # being derived from the Step 2 outcome-direction answer.
    state$display$other_text      <- input$other_text
    state$display$other_downgrade <- {
      v <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
      if (is.na(v)) 0L else v
    }
  })
}

# ================== RUN ==================
shinyApp(ui, server)
