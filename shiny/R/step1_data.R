# step1_data.R - Step 1: Data import + RoB/Indirectness assignment + preview
#
# Returns the canonical long tibble (via ingest_data) in
# `state$data`. RoB and Indirectness can be added/edited per study even
# when the source dataset lacks those columns.

# Minimal downloadable templates for the Upload / Paste branches. Written
# from literals on purpose: they document the data contract, so they must
# not drift when the bundled sample datasets change.
#
# Three studies, not two, so that rob and indirectness each show all three
# accepted values in the data itself. They are deliberately not aligned
# study-by-study: the two columns are independent judgments.
PMA_TEMPLATE_CSV <- list(
  binary = c(
    "studlab,treat,n,event,rob,indirectness",
    "Example A 2021,intervention,60,18,low,low",
    "Example A 2021,control,58,9,low,low",
    "Example B 2023,intervention,45,14,some,high",
    "Example B 2023,control,47,6,some,high",
    "Example C 2024,intervention,80,22,high,some",
    "Example C 2024,control,78,17,high,some"
  ),
  continuous = c(
    "studlab,treat,n,mean,sd,rob,indirectness",
    "Example A 2021,intervention,60,12.4,5.1,low,low",
    "Example A 2021,control,58,15.8,5.6,low,low",
    "Example B 2023,intervention,45,11.9,4.8,some,high",
    "Example B 2023,control,47,14.2,5.2,some,high",
    "Example C 2024,intervention,80,13.1,5.4,high,some",
    "Example C 2024,control,78,15.0,5.5,high,some"
  )
)

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
        shiny::fileInput("data_file", "Choose a .csv, .tsv, or .xlsx (max 10 MB)",
                         accept = c(".csv", ".tsv", ".xlsx")),
        htmltools::p(class = "pma-card-subtitle",
                     "Files larger than 10 MB are rejected by the server.")
      ),
      shiny::conditionalPanel(
        "input.input_method == 'paste'",
        shiny::textAreaInput(
          "data_paste", "Paste tab- or comma-separated data",
          rows = 8,
          placeholder = paste0(
            "studlab\ttreat\tn\tevent\trob\tindirectness\n",
            "Example A 2021\tintervention\t60\t18\tlow\tlow\n",
            "Example A 2021\tcontrol\t58\t9\tlow\tlow\n..."
          )
        )
      ),
      # Shown for both Upload and Paste: neither branch has anything on
      # screen to copy the required shape from.
      shiny::conditionalPanel(
        "input.input_method != 'sample'",
        htmltools::div(
          class = "pma-card-subtitle",
          style = "margin-top: 0.5rem;",
          "Need the exact shape? Download a template: one row per study-arm, ",
          "and the ", htmltools::code("treat"), " values become the ",
          "Intervention / Control choices in Step 2."
        ),
        htmltools::div(
          style = "margin-bottom: 0.5rem;",
          shiny::downloadButton("template_binary", "Binary template (.csv)",
                                class = "btn btn-secondary btn-sm"),
          shiny::downloadButton("template_continuous",
                                "Continuous template (.csv)",
                                class = "btn btn-secondary btn-sm",
                                style = "margin-left: 0.5rem;")
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

    # The Next button carries its own precondition (data loaded), so the nav
    # has to re-render whenever that precondition flips. It is an output of
    # its own rather than part of this body: app.R rebuilds the whole step
    # body with renderUI, and a rebuild mid-step would discard the user's
    # unsaved input. See output$step1_nav in step1_server().
    shiny::uiOutput("step1_nav")
  )
}

# state: top-level reactiveValues from app.R
step1_server <- function(input, output, session, state) {

  loaded_signature <- shiny::reactiveVal(NULL)

  # ----- Example templates (Upload / Paste branches) -----
  # Generated from PMA_TEMPLATE_CSV, never read off disk, so the download
  # cannot pick up a changed sample dataset. Read-path untouched.
  .template_download <- function(kind) {
    shiny::downloadHandler(
      filename    = function() paste0("pmatools_template_", kind, ".csv"),
      contentType = "text/csv",
      content     = function(file) {
        writeLines(PMA_TEMPLATE_CSV[[kind]], con = file)
      }
    )
  }
  output$template_binary     <- .template_download("binary")
  output$template_continuous <- .template_download("continuous")

  # Map an uploaded RoB / Indirectness column to the Step 3 editor vocabulary
  # ("low" / "some" / "high"; NA = not set). Delegates to the vendored
  # rob_strata() so the editor, grade_meta() and the stratified forest
  # plots all accept the same labels -- including the Cochrane RoB2 wording
  # ("No concerns", "Some concerns", "Serious concerns", "Critical concerns").
  # rob_strata() is pmatools public API as of 0.5.0 (the dot-prefixed
  # .rob_plot_strata() is only a back-compat alias) -- do not "fix" this back
  # to the dot-name to match the rest of the vendored code.
  .study_level_for_editor <- function(x) {
    v <- trimws(as.character(x))
    out <- rep(NA_character_, length(v))
    known <- !is.na(v) & nzchar(v)
    if (any(known)) {
      lvl <- rob_strata(v[known], arg = "Uploaded RoB/Indirectness column")
      lvl[lvl == "unknown"] <- NA_character_   # unrecognized -> leave unset
      out[known] <- lvl
    }
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

  # Run a read expression, converting parse failures into the same
  # list(error = ...) shape that ingested() already surfaces to the user,
  # instead of letting a raw R error crash the reactive chain.
  .read_or_error <- function(what, expr) {
    tryCatch(
      expr,
      error = function(e) {
        list(error = paste0(
          "Could not read the ", what, ": ", conditionMessage(e), ". ",
          "Check that the input is plain tabular data with a single header ",
          "row (long format: one row per study x arm)."
        ))
      }
    )
  }

  # Count occurrences of a single character in a string (0 when absent).
  .count_char <- function(x, ch) {
    m <- gregexpr(ch, x, fixed = TRUE)[[1]]
    if (m[1] == -1L) 0L else length(m)
  }

  # Load raw data based on input_method. Returns a data.frame, NULL (no
  # source selected yet), or list(error = <message>) on a read failure.
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
      .read_or_error("bundled sample dataset",
                     utils::read.csv(path, stringsAsFactors = FALSE))
    } else if (method == "file") {
      f <- input$data_file
      if (is.null(f)) return(NULL)
      ext <- tolower(tools::file_ext(f$name))
      if (ext == "csv") {
        .read_or_error(".csv file",
                       utils::read.csv(f$datapath, stringsAsFactors = FALSE))
      } else if (ext == "tsv") {
        .read_or_error(".tsv file",
                       utils::read.delim(f$datapath, stringsAsFactors = FALSE))
      } else if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          return(list(error = paste0(
            "The readxl package is required to read .xlsx files, but it is ",
            "not installed on this server. Please save the sheet as .csv ",
            "and upload that instead."
          )))
        }
        .read_or_error(".xlsx file",
                       as.data.frame(readxl::read_excel(f$datapath),
                                     stringsAsFactors = FALSE))
      } else {
        list(error = paste0(
          "Unsupported file extension '.", ext,
          "'. Please upload a .csv, .tsv, or .xlsx file."
        ))
      }
    } else if (method == "paste") {
      txt <- input$data_paste
      if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
      # Sniff the delimiter over the WHOLE pasted block (the first line
      # alone is fragile, e.g. a stray comma in the header of otherwise
      # tab-separated data). Tabs win ties because Excel pastes tabs.
      n_tab   <- .count_char(txt, "\t")
      n_comma <- .count_char(txt, ",")
      sep <- if (n_tab >= n_comma && n_tab > 0L) "\t" else ","
      .read_or_error("pasted text",
                     utils::read.table(text = txt, sep = sep, header = TRUE,
                                       stringsAsFactors = FALSE,
                                       na.strings = c("", "NA", ".")))
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
    # Read-stage failure from raw(): forward the friendly error as is.
    if (!is.data.frame(df) && is.list(df) && !is.null(df$error)) {
      return(df)
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

  # Saved outcomes survive a data reload on purpose (never silently
  # discarded), but a reload is exactly when a Summary of Findings table can
  # start mixing datasets. Say so once per load; Step 3 / Step 4 then flag
  # the individual rows that came from other data.
  shiny::observeEvent(ingested(), {
    res <- ingested()
    if (is.null(res) ||
        (!is.data.frame(res) && is.list(res) && !is.null(res$error))) return()
    n <- length(pma_outcomes_list(state$outcomes))
    if (n == 0) return()
    shiny::showNotification(
      sprintf(paste0(
        "%d saved outcome(s) are kept. Any that were saved from a different ",
        "dataset are marked \"different dataset\" in the saved-outcome list ",
        "(Step 3) and warned about above the combined Summary of Findings ",
        "table (Step 4). Nothing was removed."), n),
      type = "warning", duration = 12)
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
                     "outcome x arm when an outcome column is present). To ",
                     "edit a cell, double-click it, type the new value, then ",
                     "click outside the cell to apply it (pressing Enter on ",
                     "its own does not). The table is automatically ",
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

  # commit_loaded_data() reads state$rob_table, so this observer re-runs on
  # every Step 3 RoB / Indirectness change. Commit the edited table, not the
  # raw ingest: otherwise a RoB edit silently reverts Step 1 cell edits in
  # state$data while the preview (which reads state$data_edits) still shows
  # them, and Step 2 / Step 3 quietly analyse different numbers.
  shiny::observe({
    res <- ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(res) || (is.list(res) && !is.null(res$error))) return()
    commit_loaded_data(state$data_edits %||% res)
  })

  # ----- Advance hook -----

  # Exactly the condition state$step1_commit enforces below, so the button
  # state and the commit toast can never disagree.
  state$step1_can_advance <- shiny::reactive({
    # ingested() is an eventReactive(ignoreInit = TRUE): reading it before
    # the first Load data click raises the silent cancel condition, which
    # would leave output$step1_nav below rendering an empty div instead of a
    # disabled button. Nothing can be loaded at that point anyway.
    if (!isTRUE((input$load_data %||% 0) > 0)) return(FALSE)
    res <- ingested()
    isTRUE(loaded_current()) &&
      !is.null(res) && !(is.list(res) && !is.null(res$error))
  })

  # Wizard nav. Rendered as its own output so that flipping Next between
  # disabled and enabled re-renders these two buttons only, leaving the rest
  # of the step body (and anything typed into it) untouched.
  output$step1_nav <- shiny::renderUI({
    pma_wizard_nav(current_step = 1,
                   next_disabled = !isTRUE(state$step1_can_advance()))
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
