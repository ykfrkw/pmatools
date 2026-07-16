# step4_export.R - Step 4: ZIP export

step4_ui <- function() {
  s <- EDU_COPY$steps$step4

  htmltools::tagList(
    pma_step_header(s$title, s$what),

    pma_card(
      title = "Bundle settings",
      shiny::textInput("bundle_name", "Bundle name (no extension)",
                       value = "pmatools_results"),
      shiny::checkboxGroupInput("include", "Artifacts to include",
        choices = c(
          "Long-format CSV"                                            = "data",
          "Reproducible R script"                                      = "script",
          "Results text"                                               = "results",
          "Forest plot (PDF + PNG)"                                    = "forest",
          "Forest plot stratified by RoB (PDF + PNG)"                  = "forest_rob",
          "Funnel plot (PDF + PNG)"                                    = "funnel",
          "Trim-and-fill funnel (PDF + PNG, k>=10)"                    = "funnel_trimfill",
          "Publication bias missing-results forest (PDF + PNG, k>=10)" = "pubias_missing_forest",
          "GRADE Evidence Profile + SoF table (docx)"                  = "grade_table"
        ),
        selected = c("data","script","results","forest","forest_rob",
                     "funnel","funnel_trimfill","pubias_missing_forest",
                     "grade_table")),
      shiny::uiOutput("rare_export_note"),
      shiny::downloadButton("download_zip", "Download ZIP",
                            class = "btn btn-primary",
                            style = "width: 100%; margin-top: 0.5rem;")
    ),

    pma_card(
      title = "How to cite",
      htmltools::p(paste0(
        "Pairwise meta-analysis was performed using the {meta} R package ",
        "(Balduzzi et al. 2019). Certainty of evidence was rated using the GRADE ",
        "approach following the BMJ 2025 Core GRADE series (Guyatt et al. ",
        "2025), implemented in pmatools (Furukawa Y, ",
        "https://yukifurukawa.jp/pmatools/).")),
      htmltools::tags$ul(
        htmltools::tags$li(
          "Furukawa Y. pmatools. ",
          htmltools::tags$a(href = "https://yukifurukawa.jp/pmatools/",
                            target = "_blank", "https://yukifurukawa.jp/pmatools/"),
          "."),
        htmltools::tags$li(paste0(
          "Balduzzi S, Ruecker G, Schwarzer G. ",
          "How to perform a meta-analysis with R: a practical tutorial. ",
          "J Stat Softw. 2019;91(1):1-37.")),
        htmltools::tags$li(
          htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2024-081903",
                            target = "_blank",
                            "Guyatt G, et al. Core GRADE 1: Overview. BMJ 2025.")),
        htmltools::tags$li(
          htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2024-081905",
                            target = "_blank",
                            "Guyatt G, et al. Core GRADE 3: Inconsistency. BMJ 2025.")),
        htmltools::tags$li(
          htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2024-083864",
                            target = "_blank",
                            "Guyatt G, et al. Core GRADE 4: Risk of bias, publication bias. BMJ 2025.")),
        htmltools::tags$li(
          htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2024-083865",
                            target = "_blank",
                            "Guyatt G, et al. Core GRADE 5: Indirectness. BMJ 2025."))
      ),
      htmltools::p(
        style = "margin-top: 1rem; font-style: italic;",
        htmltools::HTML(paste0(
          "<strong>Statistical pooling is only a small part of a systematic review. ",
          "A high-quality SR&amp;MA also needs a detailed, prespecified and pre-registered protocol ",
          "(e.g., on PROSPERO, OSF or other platforms), a comprehensive search, dual independent ",
          "screening and data extraction, and risk-of-bias assessment - all ",
          "completed BEFORE the analysis.</strong>"
        ))
      )
    ),

    pma_wizard_nav(current_step = 4)
  )
}

step4_server <- function(input, output, session, state) {

  output$rare_export_note <- shiny::renderUI({
    if (!isTRUE(state$rare_mode_active)) return(NULL)
    htmltools::p(
      class = "pma-card-subtitle",
      style = "margin-top: 0.5rem;",
      "Rare-events mode: the ZIP also includes rare-event diagnostics, ",
      "the method table, the method-sensitivity forest plot, and ",
      "analysis.R code to rerun the rare-event method set."
    )
  })

  .export_covariate <- function(ma, col, default = NA_character_) {
    labels <- as.character(ma$studlab)
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

  output$download_zip <- shiny::downloadHandler(
    filename = function() {
      paste0(input$bundle_name %||% "pmatools_results", ".zip")
    },
    content = function(file) {
      if (is.null(state$ma)) {
        shiny::showNotification(
          "Cannot export: Step 2 (run analysis) must be completed first.",
          type = "error", duration = NULL
        )
        return()
      }
      if (is.null(state$grade)) {
        shiny::showNotification(
          "Cannot export: please open Step 3 (GRADE) at least once before downloading.",
          type = "error", duration = NULL
        )
        return()
      }

      tryCatch({
        tmp_dir <- tempfile()
        dir.create(tmp_dir)

        include <- input$include %||% c("data","script","results",
                                        "forest","forest_rob","funnel",
                                        "funnel_trimfill",
                                        "pubias_missing_forest",
                                        "grade_table")

        rob_vec <- .export_covariate(state$ma, "rob", default = "*")

        out <- export_bundle(
          ma           = state$ma,
          grade        = state$grade,
          output_dir   = tmp_dir,
          bundle_name  = input$bundle_name %||% "pmatools_results",
          include      = include,
          per          = state$display$per             %||% 1000,
          prediction   = state$display$prediction      %||% FALSE,
          convert_smd_to_or = state$display$convert    %||% FALSE,
          baseline_risk     = state$display$baseline_risk,
          threshold_label   = state$display$threshold_label,
          chinn_invert      = isTRUE(state$display$chinn_invert),
          other_text         = state$display$other_text,
          other_downgrade    = state$display$other_downgrade %||% 0L,
          data               = state$data,
          forest_display     = state$display$forest_step2,
          rob                = rob_vec,
          forest_display_rob = state$display$forest_rob,
          rare               = if (isTRUE(state$rare_mode_active)) state$rare else NULL,
          rare_forest_display = state$display$forest_step2,
          pubias_missing_df  = state$pubias_missing
        )
        file.copy(out, file)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        trace <- paste(utils::capture.output(traceback()), collapse = "\n")
        shiny::showNotification(
          paste("Export failed:", msg),
          type = "error", duration = NULL
        )
        # Write a *valid* ZIP that contains an ERROR.txt explaining what
        # went wrong, so the browser doesn't hand the user a malformed
        # 58-byte download that "Unable to expand".
        err_dir <- tempfile()
        dir.create(err_dir)
        err_txt <- file.path(err_dir, "ERROR.txt")
        writeLines(c(
          "pmatools export failed.",
          "",
          paste("Time   :", format(Sys.time())),
          paste("Message:", msg),
          "",
          "--- Traceback ---",
          trace
        ), err_txt)
        # zip::zipr writes a real ZIP archive
        zip::zipr(zipfile = file, files = err_txt)
      })
    },
    contentType = "application/zip"
  )

}
