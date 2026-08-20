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
    pma_step_header(s$title),

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
        style = paste0(
          "border-left: 3px solid hsl(var(--accent)); ",
          "padding: 0.5rem 0.75rem; ",
          "background: hsl(var(--accent) / 0.08); ",
          "border-radius: 4px; margin-top: 0.5rem;"
        ),
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
        "Page MJ, et al. BMJ. 2023)."
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

# Is this the list(error = "...") shape that raw() and ingested() return on a
# failure, rather than the ingested tibble?
#
# Written as a name test, NOT as `!is.null(x$error)`. A tibble IS a list, so the
# `$` in the old predicate ran on the success path too and tibble warned
# "Unknown or uninitialised column: `error`" every time -- fifteen times in a
# single end-to-end walk-through, which buried the warnings that matter. The
# failure path returns a plain list whose only element is named "error", so the
# two predicates agree on every value either function can produce.
pma_is_error_result <- function(x) {
  is.list(x) && "error" %in% names(x)
}

# state: top-level reactiveValues from app.R
step1_server <- function(input, output, session, state) {

  loaded_signature <- shiny::reactiveVal(NULL)

  # The column names as they arrived, captured at ingest so the detected-
  # columns strip describes the data that was loaded rather than whatever the
  # file input currently points at. ingest_data() renames source columns onto
  # their role names, so the ingested tibble alone can no longer say "studlab
  # came from `study`".
  loaded_raw_names <- shiny::reactiveVal(NULL)

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
        "Furukawa Y, et al. <i>J Affect Disord</i>. 2024."
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

  # ----- What each bundled sample is an analysis OF -----------------------
  # The sample datasets ship with a known outcome, direction and follow-up.
  # Making the reviewer retype them in Step 2 to try the app out serves
  # nothing, and getting the direction wrong there silently changes the
  # Risk-of-Bias direction gate and the OIS target.
  #
  # A named list, keyed by input$sample_dataset, so a third sample joins by
  # adding a row rather than by editing a branch. The synthetic rare-events
  # set has no real clinical identity, so it seeds only what is honest.
  PMA_SAMPLE_OUTCOME_DEFAULTS <- list(
    regular = list(
      outcome_name      = "Depression response",
      small_values      = "undesirable",
      outcome_follow_up = "Post-treatment"
    ),
    rare = list(
      outcome_name      = "Serious adverse event",
      small_values      = "desirable",
      outcome_follow_up = "End of follow-up"
    )
  )

  # Seed the three Step 2 identity fields from the sample being loaded.
  #
  # BLANKS ONLY. A reviewer who has already typed an outcome name and then
  # reloads the data must not find it replaced. step2_ui() seeds all three
  # widgets from state, so nothing in Step 2 needs to change.
  #
  # Hooked to the LOAD path, not to commit_loaded_data(): that function is
  # called from an observer that depends on state$rob_table and therefore
  # re-runs on every per-study Risk-of-Bias edit made in Step 3, which would
  # make this fire over and over.
  .seed_sample_outcome_defaults <- function() {
    if (!identical(input$input_method, "sample")) return(invisible(NULL))
    d <- PMA_SAMPLE_OUTCOME_DEFAULTS[[input$sample_dataset %||% "regular"]]
    if (is.null(d)) return(invisible(NULL))
    .blank <- function(v) {
      is.null(v) || length(v) != 1L || is.na(v) || !nzchar(trimws(as.character(v)))
    }
    if (.blank(state$outcome_name))      state$outcome_name      <- d$outcome_name
    if (.blank(state$small_values))      state$small_values      <- d$small_values
    if (.blank(state$outcome_follow_up)) state$outcome_follow_up <- d$outcome_follow_up
    invisible(NULL)
  }

  # Run ingest_data only when the user explicitly clicks Load data.
  ingested <- shiny::eventReactive(input$load_data, {
    df <- raw()
    loaded_signature(current_signature())
    state$data_edits <- NULL
    loaded_raw_names(NULL)
    if (is.null(df)) {
      return(list(error = "No data source selected, or the selected source is empty."))
    }
    # Read-stage failure from raw(): forward the friendly error as is.
    if (!is.data.frame(df) && pma_is_error_result(df)) {
      return(df)
    }
    loaded_raw_names(names(df))
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

  # Fires once per click on Load data, and only then.
  shiny::observeEvent(input$load_data, {
    .seed_sample_outcome_defaults()
  }, ignoreInit = TRUE)

  # Saved outcomes survive a data reload on purpose (never silently
  # discarded), but a reload is exactly when a Summary of Findings table can
  # start mixing datasets. Say so once per load; Step 4 then flags the
  # individual rows that came from other data.
  shiny::observeEvent(ingested(), {
    ingest_result <- ingested()
    if (is.null(ingest_result) ||
        (!is.data.frame(ingest_result) && pma_is_error_result(ingest_result))) return()
    n <- length(pma_outcomes_list(state$outcomes))
    if (n == 0) return()
    shiny::showNotification(
      sprintf(paste0(
        "%d saved outcome(s) are kept. Any that were saved from a different ",
        "dataset are marked \"different dataset\" in the saved-outcome list ",
        "on Step 4, and warned about above the combined Summary of Findings ",
        "table there. Nothing was removed."), n),
      type = "warning", duration = 12)
  }, ignoreInit = TRUE)

  loaded_current <- shiny::reactive({
    !is.null(loaded_signature()) &&
      identical(loaded_signature(), current_signature())
  })

  commit_loaded_data <- function(ingest_result) {
    rt <- state$rob_table
    if (!is.null(rt)) {
      idx <- match(as.character(ingest_result$studlab), as.character(rt$studlab))
      if (any(!is.na(rt$rob))) {
        ingest_result$rob <- rt$rob[idx]
      }
      if (any(!is.na(rt$indirectness))) {
        ingest_result$indirectness <- rt$indirectness[idx]
      }
    }

    # This function is called from a plain observe() that depends on
    # state$rob_table (see the comment above that observer), so it re-runs on
    # every per-study Risk-of-Bias / Indirectness edit made in Step 3 - and it
    # used to null the analysis and the rating unconditionally. That withdrew
    # the analysis under the reviewer's feet, and nothing put it back: the
    # observeEvent(ma()) handler in step2_ma.R returns early on NULL, and after
    # a Step 3 -> Step 2 -> Step 3 round trip input$run_ma is a rebuilt
    # actionButton reporting 0, so ma() exits before it can recompute. It was
    # also silent, because pma_analysis_signature(NULL) is NA and app.R's
    # provenance guard ignores NA.
    #
    # Only a change to the DATASET invalidates the analysis. A RoB relabel is a
    # property of the studies, not of the outcome - which is exactly what
    # begin_new_outcome() already promises - and pma_dataset_signature()
    # already excludes the `rob` and `indirectness` columns for that reason
    # (PMA_SIGNATURE_IGNORE_COLS), so it is the right comparator here.
    changed <- !identical(pma_dataset_signature(state$data),
                          pma_dataset_signature(ingest_result))
    state$data <- ingest_result
    if (changed) {
      state$ma <- NULL
      state$ma_blocked <- NULL
      state$grade <- NULL
    }
    TRUE
  }

  output$data_preview_card <- shiny::renderUI({
    if (!isTRUE(loaded_current())) return(NULL)
    pma_card(
      title = "Preview & edit",
      shiny::uiOutput("data_load_banner"),
      shiny::uiOutput("data_roles_strip"),
      htmltools::p(class = "pma-card-subtitle",
                   paste0(
                     "Long-format view (one row per study x arm, or study x ",
                     "outcome x arm when an outcome column is present). To ",
                     "edit a cell, double-click it, type the new value, then ",
                     "click outside the cell to apply it (pressing Enter on ",
                     "its own does not). The table is automatically ",
                     "re-validated."
                   )),
      # Defaults to the analysis columns: the bundled sample is 39 columns
      # wide and the ten the analysis reads are the ones worth checking. The
      # full table is one click away -- it is the default that was wrong,
      # not its existence.
      shiny::radioButtons(
        "preview_columns", NULL,
        choices  = c("Analysis columns" = "analysis", "All columns" = "all"),
        selected = "analysis",
        inline   = TRUE
      ),
      DT::DTOutput("data_preview"),
      htmltools::tags$hr(),
      # Assigning Risk of Bias across every study is data-entry work, so it
      # belongs on the data-entry step. The Step 3 copies stay: correcting one
      # study while looking at the certainty verdict is a real workflow, and
      # both write the same state$rob_table.
      htmltools::div(
        class = "pma-card-subtitle",
        htmltools::strong("Risk of bias for every study. "),
        "Set them all here, then override individual studies in Step 3."
      ),
      htmltools::div(
        style = "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-top: 0.5rem;",
        shiny::actionButton("step1_rob_set_low",  "Set all to Low",  class = "btn-sm"),
        shiny::actionButton("step1_rob_set_some", "Set all to Some", class = "btn-sm"),
        shiny::actionButton("step1_rob_set_high", "Set all to High", class = "btn-sm"),
        shiny::actionButton("step1_rob_clear",    "Clear all",       class = "btn-sm")
      )
    )
  })

  detected_roles <- shiny::reactive({
    cols <- loaded_raw_names()
    if (is.null(cols)) return(NULL)
    detect_column_roles(cols)
  })

  output$data_roles_strip <- shiny::renderUI({
    detected <- detected_roles()
    if (!isTRUE(loaded_current()) || is.null(detected)) return(NULL)
    pma_column_roles_strip(detected, state$rob_table)
  })

  output$data_load_banner <- shiny::renderUI({
    ingest_result <- ingested()
    if (!isTRUE(loaded_current())) return(NULL)
    if (is.null(ingest_result)) return(NULL)
    if (pma_is_error_result(ingest_result)) {
      return(pma_banner(htmltools::strong("Could not read this data. "),
                        ingest_result$error))
    }
    pma_banner(tone = "success",
               htmltools::strong("Data loaded. "),
               pma_load_summary(ingest_result))
  })

  # ----- Initialize state$rob_table from ingested data -----
  # The per-study editor lives in Step 3 (RoB / Indirectness tabs); here we
  # only seed the table when ingest finishes so that Step 3 has values to
  # show / edit.
  shiny::observe({
    ingest_result <- ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(ingest_result) || pma_is_error_result(ingest_result)) return()
    studs <- unique(as.character(ingest_result$studlab))
    rob_init <- if ("rob" %in% names(ingest_result)) {
      vals <- as.character(ingest_result$rob)
      lookup <- vals[!duplicated(ingest_result$studlab)]
      names(lookup) <- as.character(ingest_result$studlab)[!duplicated(ingest_result$studlab)]
      .study_level_for_editor(unname(lookup[studs]))
    } else rep(NA_character_, length(studs))
    indir_init <- if ("indirectness" %in% names(ingest_result)) {
      vals <- as.character(ingest_result$indirectness)
      lookup <- vals[!duplicated(ingest_result$studlab)]
      names(lookup) <- as.character(ingest_result$studlab)[!duplicated(ingest_result$studlab)]
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
    ingest_result <- state$data_edits %||% ingested()
    if (!isTRUE(loaded_current())) {
      return(DT::datatable(data.frame(message = "Click Load data to preview.")))
    }
    if (is.null(ingest_result)) {
      return(DT::datatable(data.frame(message = "No data loaded yet.")))
    }
    if (pma_is_error_result(ingest_result)) {
      return(DT::datatable(data.frame(error = ingest_result$error)))
    }
    # HIDE the extra columns; do not subset the frame. DT reports a cell edit
    # as `col`, the DataTables column index, which counts hidden columns --
    # and input$data_preview_cell_edit is applied below as ingest_result[[info$col + 1]]
    # against the FULL frame. Passing a subset here would silently write the
    # edit into whichever column happened to sit at that index in the subset.
    hidden <- which(!names(ingest_result) %in% pma_analysis_columns(ingest_result)) - 1L
    DT::datatable(
      ingest_result,
      editable = list(target = "cell"),
      options = list(
        pageLength = 10,
        scrollX    = TRUE,
        columnDefs = if (identical(input$preview_columns %||% "analysis",
                                   "analysis") && length(hidden)) {
          list(list(visible = FALSE, targets = hidden))
        } else {
          list()
        }
      ),
      rownames = FALSE
    )
  })

  # commit_loaded_data() reads state$rob_table, so this observer re-runs on
  # every Step 3 RoB / Indirectness change. Commit the edited table, not the
  # raw ingest: otherwise a RoB edit silently reverts Step 1 cell edits in
  # state$data while the preview (which reads state$data_edits) still shows
  # them, and Step 2 / Step 3 quietly analyse different numbers.
  shiny::observe({
    ingest_result <- ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(ingest_result) || pma_is_error_result(ingest_result)) return()
    commit_loaded_data(state$data_edits %||% ingest_result)
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
    ingest_result <- ingested()
    isTRUE(loaded_current()) &&
      !is.null(ingest_result) && !pma_is_error_result(ingest_result)
  })

  # Wizard nav. Rendered as its own output so that flipping Next between
  # disabled and enabled re-renders these two buttons only, leaving the rest
  # of the step body (and anything typed into it) untouched.
  output$step1_nav <- shiny::renderUI({
    pma_wizard_nav(current_step = 1,
                   next_disabled = !isTRUE(state$step1_can_advance()))
  })

  state$step1_commit <- function() {
    ingest_result <- ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(ingest_result) || pma_is_error_result(ingest_result)) {
      shiny::showNotification(
        "Cannot advance: click Load data and confirm the preview first.",
        type = "error"
      )
      return(FALSE)
    }

    commit_loaded_data(state$data_edits %||% ingest_result)
  }

  # Apply DT cell edits to state$data so direct stepper navigation uses them.
  shiny::observeEvent(input$data_preview_cell_edit, {
    info <- input$data_preview_cell_edit
    if (is.null(info)) return()
    ingest_result <- state$data_edits %||% ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(ingest_result) || pma_is_error_result(ingest_result)) return()
    new_value <- DT::coerceValue(info$value, ingest_result[[info$col + 1]][info$row])
    ingest_result[info$row, info$col + 1] <- new_value
    state$data_edits <- ingest_result
    commit_loaded_data(ingest_result)
  })

  # ----- Bulk Risk of Bias (the Step 3 buttons, on the data-entry step) -----
  # state$rob_table is the single source of truth that Step 3's editor and the
  # rating read, so writing it is what makes these buttons work at all. The
  # preview is written too: it reads state$data_edits, and a reviewer who
  # presses "Set all to Low" and sees the `rob` column unchanged has been told
  # the button did nothing.
  #
  # commit_loaded_data() re-overlays state$rob_table on top, which agrees with
  # what we just wrote -- except for "Clear all", where its
  # any(!is.na(rt$rob)) guard declines to overwrite. That is why the preview
  # is set here rather than left to the overlay.
  .step1_bulk_rob <- function(value) {
    rob_table <- state$rob_table
    if (is.null(rob_table)) return(invisible(NULL))
    rob_table$rob <- value
    state$rob_table <- rob_table

    ingest_result <- state$data_edits %||% ingested()
    if (!isTRUE(loaded_current()) ||
        is.null(ingest_result) || pma_is_error_result(ingest_result)) return(invisible(NULL))
    ingest_result$rob <- value
    state$data_edits <- ingest_result
    commit_loaded_data(ingest_result)
    invisible(NULL)
  }

  shiny::observeEvent(input$step1_rob_set_low,  { .step1_bulk_rob("low")  })
  shiny::observeEvent(input$step1_rob_set_some, { .step1_bulk_rob("some") })
  shiny::observeEvent(input$step1_rob_set_high, { .step1_bulk_rob("high") })
  shiny::observeEvent(input$step1_rob_clear,    { .step1_bulk_rob(NA_character_) })
}
