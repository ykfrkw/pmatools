# step1_data.R - Step 1: Data import + RoB/Indirectness assignment + preview
#
# Returns the canonical long tibble (via ingest_data) in
# `state$data`. RoB and Indirectness can be added/edited per study even
# when the source dataset lacks those columns.

step1_ui <- function() {
  s <- EDU_COPY$steps$step1

  htmltools::tagList(
    pma_step_header(s$title, s$what, s$why),

    pma_card(
      title = "Load data",
      shiny::radioButtons(
        "input_method", NULL,
        choices = c(
          "Use sample dataset"  = "sample",
          "Upload file"         = "file",
          "Paste from Excel"    = "paste"
        ),
        selected = "sample",
        inline = TRUE
      ),
      shiny::conditionalPanel(
        "input.input_method == 'sample'",
        shiny::selectInput(
          "sample_dataset",
          "Sample dataset",
          choices = c(
            "CBT-I depression response (regular binary sample)" = "regular",
            "Synthetic rare events example" = "rare"
          ),
          selected = "regular",
          selectize = FALSE
        ),
        shiny::uiOutput("sample_dataset_copy")
      ),
      shiny::conditionalPanel(
        "input.input_method == 'file'",
        shiny::fileInput("data_file", "Choose a .csv, .tsv, or .xlsx",
                         accept = c(".csv", ".tsv", ".xlsx"))
      ),
      shiny::conditionalPanel(
        "input.input_method == 'paste'",
        shiny::textAreaInput(
          "data_paste", "Paste tab- or comma-separated data",
          rows = 8,
          placeholder = "studlab\ttreat\tn\tevent\nA\texperimental\t50\t10\nA\tcontrol\t50\t15\n..."
        )
      ),
      htmltools::p(
        class = "pma-card-subtitle",
        "Accepted format: long only (one row per study x arm, or study x outcome x arm)."
      ),
      htmltools::div(
        class = "pma-card-subtitle",
        style = "border-left: 3px solid hsl(var(--accent)); padding: 0.5rem 0.75rem; background: hsl(var(--accent) / 0.08); border-radius: 4px; margin-top: 0.5rem;",
        htmltools::strong("Tip (Reporting bias / RoB-ME):"),
        " Also include eligible studies from your systematic review that did ",
        "not report the outcome of interest. Enter ",
        htmltools::code("studlab"), " and ",
        htmltools::code("n"),
        " for both arms but leave ", htmltools::code("event"), " / ",
        htmltools::code("mean"), " / ", htmltools::code("sd"),
        " blank. Such studies are auto-classified into the Missing-results ",
        "subgroup in Step 3 (Publication bias) and feed the RoB-ME forest ",
        "plot (",
        htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2023-076754",
                          target = "_blank", "Page et al., BMJ 2023"),
        ")."
      ),
      shiny::actionButton("load_data", "Load data",
                          class = "btn btn-primary",
                          style = "margin-top: 0.75rem;")
    ),

    shiny::uiOutput("data_preview_card"),

    pma_wizard_nav(current_step = 1)
  )
}

# state: top-level reactiveValues from app.R
step1_server <- function(input, output, session, state) {

  loaded_signature <- shiny::reactiveVal(NULL)

  .study_level_for_editor <- function(x) {
    v <- tolower(trimws(as.character(x)))
    out <- ifelse(
      is.na(v) | !nzchar(v), NA_character_,
      ifelse(v %in% c("l", "low", "no"), "low",
        ifelse(v %in% c("s", "some", "some_concerns", "moderate", "m", "unclear"), "some",
          ifelse(v %in% c("h", "high", "serious", "very_serious"), "high", v)
        )
      )
    )
    out
  }

  current_signature <- shiny::reactive({
    method <- input$input_method %||% ""
    sample_sig <- if (identical(method, "sample")) input$sample_dataset %||% "regular" else ""
    file_sig <- if (identical(method, "file") && !is.null(input$data_file)) {
      paste(input$data_file$name %||% "", input$data_file$size %||% "", sep = "::")
    } else {
      ""
    }
    paste(method, sample_sig, file_sig, input$data_paste %||% "", sep = "\r")
  })

  output$sample_dataset_copy <- shiny::renderUI({
    sample <- input$sample_dataset %||% "regular"
    if (identical(sample, "rare")) {
      return(htmltools::p(
        class = "pma-card-subtitle",
        htmltools::HTML(paste0(
          "Synthetic rare-events example: 10 mock trials with event rates ",
          "below 1 percent, multiple single-zero studies, and two double-zero ",
          "studies. This dataset is simulated for workflow testing only; it is ",
          "not real clinical evidence."
        ))
      ))
    }
    htmltools::p(
      class = "pma-card-subtitle",
      htmltools::HTML(paste0(
        "Sample dataset: 17 RCTs of CBT-I for depression response in MDD ",
        "with comorbid insomnia. Source: ",
        "<a href='https://doi.org/10.1016/j.jad.2024.09.017' target='_blank'>",
        "Furukawa Y, Nagaoka D, Sato S, et al. ",
        "<i>J Affect Disord</i>. 2024;367:359-366. ",
        "doi:10.1016/j.jad.2024.09.017</a>."
      ))
    )
  })

  shiny::observeEvent(current_signature(), {
    if (!is.null(loaded_signature()) &&
        !identical(loaded_signature(), current_signature())) {
      loaded_signature(NULL)
      state$data <- NULL
      state$data_edits <- NULL
      state$ma <- NULL
      state$grade <- NULL
    }
  }, ignoreInit = TRUE)

  # Load raw data based on input_method
  raw <- shiny::reactive({
    method <- input$input_method
    if (is.null(method) || !length(method) || !nzchar(method)) return(NULL)
    if (method == "sample") {
      sample <- input$sample_dataset %||% "regular"
      file <- if (identical(sample, "rare")) {
        "rare_events_mock.csv"
      } else {
        "cbti_depression.csv"
      }
      path <- file.path("_pmatools_inst", "extdata", file)
      if (!file.exists(path)) {
        # Fallback: package install (during local development)
        path <- system.file("extdata", file, package = "pmatools")
        if (!nzchar(path)) return(NULL)
      }
      utils::read.csv(path, stringsAsFactors = FALSE)
    } else if (method == "file") {
      f <- input$data_file
      if (is.null(f)) return(NULL)
      ext <- tolower(tools::file_ext(f$name))
      if (ext == "csv") utils::read.csv(f$datapath, stringsAsFactors = FALSE)
      else if (ext == "tsv") utils::read.delim(f$datapath, stringsAsFactors = FALSE)
      else if (ext %in% c("xlsx", "xls")) {
        if (requireNamespace("readxl", quietly = TRUE))
          as.data.frame(readxl::read_excel(f$datapath), stringsAsFactors = FALSE)
        else NULL
      } else NULL
    } else if (method == "paste") {
      txt <- input$data_paste
      if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
      sep <- if (grepl("\t", strsplit(txt, "\n", fixed = TRUE)[[1]][1])) "\t" else ","
      utils::read.table(text = txt, sep = sep, header = TRUE,
                        stringsAsFactors = FALSE, na.strings = c("", "NA", "."))
    } else NULL
  })

  # Run ingest_data only when the user explicitly clicks Load data.
  ingested <- shiny::eventReactive(input$load_data, {
    df <- raw()
    loaded_signature(current_signature())
    state$data_edits <- NULL
    if (is.null(df)) {
      return(list(error = "No data source selected, or the selected source is empty."))
    }
    tryCatch(
      withCallingHandlers(
        ingest_data(df, format = "long"),
        message = function(m) {
          msg <- conditionMessage(m)
          if (grepl("Combined", msg, fixed = TRUE)) {
            shiny::showNotification(trimws(msg), type = "message", duration = 6)
          }
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )
  }, ignoreInit = TRUE)

  loaded_current <- shiny::reactive({
    !is.null(loaded_signature()) &&
      identical(loaded_signature(), current_signature())
  })

  commit_loaded_data <- function(res) {
    rt <- state$rob_table
    if (!is.null(rt)) {
      idx <- match(as.character(res$studlab), as.character(rt$studlab))
      if (any(!is.na(rt$rob))) {
        res$rob <- rt$rob[idx]
      }
      if (any(!is.na(rt$indirectness))) {
        res$indirectness <- rt$indirectness[idx]
      }
    }

    state$data <- res
    state$ma <- NULL
    state$grade <- NULL
    TRUE
  }

  output$data_preview_card <- shiny::renderUI({
    if (!isTRUE(loaded_current())) return(NULL)
    pma_card(
      title = "Preview & edit",
      htmltools::p(class = "pma-card-subtitle",
                   paste0(
                     "Long-format view (one row per study x arm, or study x ",
                     "outcome x arm when an outcome column is present). Edit ",
                     "cells inline if needed. The table is automatically ",
                     "re-validated."
                   )),
      DT::DTOutput("data_preview"),
      htmltools::br(),
      shiny::verbatimTextOutput("data_status")
    )
  })

  # ----- Initialize state$rob_table from ingested data -----
  # The per-study editor lives in Step 3 (RoB / Indirectness tabs); here we
  # only seed the table when ingest finishes so that Step 3 has values to
  # show / edit.
  shiny::observe({
    res <- ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(res) || (is.list(res) && !is.null(res$error))) return()
    studs <- unique(as.character(res$studlab))
    rob_init <- if ("rob" %in% names(res)) {
      vals <- as.character(res$rob)
      lookup <- vals[!duplicated(res$studlab)]
      names(lookup) <- as.character(res$studlab)[!duplicated(res$studlab)]
      .study_level_for_editor(unname(lookup[studs]))
    } else rep(NA_character_, length(studs))
    indir_init <- if ("indirectness" %in% names(res)) {
      vals <- as.character(res$indirectness)
      lookup <- vals[!duplicated(res$studlab)]
      names(lookup) <- as.character(res$studlab)[!duplicated(res$studlab)]
      .study_level_for_editor(unname(lookup[studs]))
    } else rep(NA_character_, length(studs))

    cur <- state$rob_table
    if (is.null(cur) || !setequal(cur$studlab, studs)) {
      state$rob_table <- data.frame(
        studlab      = studs,
        rob          = rob_init,
        indirectness = indir_init,
        stringsAsFactors = FALSE
      )
    }
  })

  # ----- Preview DT -----

  output$data_preview <- DT::renderDT({
    res <- state$data_edits %||% ingested()
    if (!isTRUE(loaded_current())) {
      return(DT::datatable(data.frame(message = "Click Load data to preview.")))
    }
    if (is.null(res)) {
      return(DT::datatable(data.frame(message = "No data loaded yet.")))
    }
    if (is.list(res) && !is.null(res$error)) {
      return(DT::datatable(data.frame(error = res$error)))
    }
    DT::datatable(
      res,
      editable = list(target = "cell"),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$data_status <- shiny::renderText({
    res <- ingested()
    if (!isTRUE(loaded_current())) return("Status: click Load data to preview.")
    if (is.null(res)) return("Status: no data loaded.")
    if (is.list(res) && !is.null(res$error)) {
      return(paste0("ERROR: ", res$error))
    }
    if ("outcome" %in% names(res)) {
      study_outcomes <- unique(paste(res$studlab, res$outcome, sep = "\r"))
      sprintf("Status: %d rows, %d studies, %d study-outcomes (long format).",
              nrow(res), length(unique(res$studlab)), length(study_outcomes))
    } else {
      sprintf("Status: %d rows, %d studies (long format).",
              nrow(res), length(unique(res$studlab)))
    }
  })

  shiny::observe({
    res <- ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(res) || (is.list(res) && !is.null(res$error))) return()
    commit_loaded_data(res)
  })

  # ----- Advance hook -----

  state$step1_can_advance <- shiny::reactive({
    res <- ingested()
    isTRUE(loaded_current()) &&
      !is.null(res) && !(is.list(res) && !is.null(res$error))
  })

  state$step1_commit <- function() {
    res <- ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(res) || (is.list(res) && !is.null(res$error))) {
      shiny::showNotification(
        "Cannot advance: click Load data and confirm the preview first.",
        type = "error"
      )
      return(FALSE)
    }

    commit_loaded_data(state$data_edits %||% res)
  }

  # Apply DT cell edits to state$data so direct stepper navigation uses them.
  shiny::observeEvent(input$data_preview_cell_edit, {
    info <- input$data_preview_cell_edit
    if (is.null(info)) return()
    res <- state$data_edits %||% ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(res) || (is.list(res) && !is.null(res$error))) return()
    new_value <- DT::coerceValue(info$value, res[[info$col + 1]][info$row])
    res[info$row, info$col + 1] <- new_value
    state$data_edits <- res
    commit_loaded_data(res)
  })
}
