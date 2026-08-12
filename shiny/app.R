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

# Source local Shiny modules
local_files <- c(
  "R/educational_copy.R",
  "R/ui_helpers.R",
  "R/step1_data.R",
  "R/step2_ma.R",
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
      "pairwise meta-analysis with GRADE"
    )
  ),

  shiny::uiOutput("stepper_ui"),
  shiny::uiOutput("step_body"),

  htmltools::hr(),
  htmltools::div(
    style = "color: hsl(var(--muted-foreground)); font-size: 0.8rem; padding: 1rem 0;",
    htmltools::HTML(
      "Powered by <a href='https://yukifurukawa.jp/pmatools/' target='_blank'>yukifurukawa.jp/pmatools/</a>."
    )
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
    regular_ma     = NULL,
    rare           = NULL,
    rare_diagnostics = NULL,
    rare_mode_requested = TRUE,
    rare_mode_active = FALSE,
    grade          = NULL,
    indir_reviewed = FALSE,
    # Outcome identity, collected in Step 2 and consumed by Step 3 / Step 4.
    # Held in state (not read straight off input$) because the Step 2 widgets
    # are destroyed whenever another step's body is rendered.
    outcome_name   = NULL,
    small_values   = NULL,
    arm_e          = NULL,
    arm_c          = NULL,
    display = list(
      per                = 1000,
      prediction         = FALSE,
      convert            = FALSE,
      baseline_risk      = NULL,
      threshold_label    = NULL
    )
  )

  # Stepper indicator
  output$stepper_ui <- shiny::renderUI({
    pma_stepper(state$step)
  })

  # Render the current step body
  output$step_body <- shiny::renderUI({
    switch(state$step,
      `1` = step1_ui(),
      `2` = step2_ui(state),
      `3` = step3_ui(),
      `4` = step4_ui(),
      step1_ui()
    )
  })

  # Wire up each step's server logic
  step1_server(input, output, session, state)
  step2_server(input, output, session, state)
  step3_server(input, output, session, state)
  step4_server(input, output, session, state)

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
    state$display$per             <- input$per             %||% 1000
    state$display$prediction      <- isTRUE(input$prediction)
    state$display$convert         <- isTRUE(input$convert_smd_to_or)
    state$display$baseline_risk   <- input$baseline_risk_chinn
    state$display$threshold_label <- input$threshold_label
    state$display$chinn_invert    <- isTRUE(input$chinn_invert)
    state$display$other_text      <- input$other_text
    state$display$other_downgrade <- {
      v <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
      if (is.na(v)) 0L else v
    }
  })
}

# ================== RUN ==================
shinyApp(ui, server)
