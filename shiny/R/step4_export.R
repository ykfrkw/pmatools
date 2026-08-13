# step4_export.R - Step 4: ZIP export

step4_ui <- function() {
  s <- EDU_COPY$steps$step4

  htmltools::tagList(
    pma_step_header(s$title),

    # Multi-outcome Summary of Findings, assembled from the assessments
    # saved on the Step 3 "Final certainty" tab. Shown before the bundle
    # settings so the user can check the table they are about to export.
    pma_card(
      title = "Summary of Findings (all saved outcomes)",
      shiny::uiOutput("sof_intro_block"),
      # Dataset-provenance guard: warns when saved outcomes came from a
      # dataset other than the one currently loaded. Warning only - the
      # export is never blocked (see output$sof_stale_warning).
      shiny::uiOutput("sof_stale_warning"),
      shinycssloaders::withSpinner(
        shiny::uiOutput("combined_sof_block"),
        type = 4, color = "#0f172a", size = 0.6,
        proxy.height = "120px"),
      # Straight from the finished table to the next row of it.
      pma_add_next_outcome_button(),
      htmltools::p(
        class = "pma-card-subtitle",
        style = "margin-top: 0.4rem;",
        "Returns to Step 2 with the outcome name, direction, follow-up and ",
        "every Step 3 answer cleared, ready for the next outcome. The saved ",
        "outcomes above, the loaded data and the per-study risk-of-bias and ",
        "indirectness ratings are kept.")
    ),

    pma_card(
      title = "Bundle settings",
      shiny::textInput("bundle_name", "Bundle name (no extension)",
                       value = "pmatools_results"),
      shiny::checkboxGroupInput("include", "Artifacts to include",
        choices = c(
          "Long-format CSV"                                      = "data",
          "Reproducible R script"                                = "script",
          "Results text"                                         = "results",
          "Forest plot (PDF)"                                    = "forest",
          "Forest plot stratified by RoB (PDF)"                  = "forest_rob",
          "Funnel plot (PDF)"                                    = "funnel",
          "Trim-and-fill funnel (PDF, k>=10)"                    = "funnel_trimfill",
          "Publication bias missing-results forest (PDF, k>=10)" = "pubias_missing_forest",
          "Core GRADE Evidence Profile + SoF table (docx)"       = "grade_table",
          "Combined SoF table across saved outcomes (docx)"      = "sof_combined"
        ),
        selected = c("data","script","results","forest","forest_rob",
                     "funnel","funnel_trimfill","pubias_missing_forest",
                     "grade_table","sof_combined")),
      shiny::uiOutput("rare_export_note"),
      # The Download button is rendered server-side so it only appears once
      # Steps 2-3 have produced results (see output$download_zip_ui).
      shiny::uiOutput("download_zip_ui")
    ),

    pma_card(
      title = "How to cite",
      htmltools::p(paste0(
        "Pairwise meta-analysis was performed using the {meta} R package ",
        "(Balduzzi et al. 2019). Certainty of evidence was rated following ",
        "the BMJ 2025 Core GRADE series (Guyatt et al. ",
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
          "Evid Based Ment Health. 2019;22(4):153-160.")),
        htmltools::tags$li(
          htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2024-081903",
                            target = "_blank",
                            "Guyatt G, et al. Core GRADE 1: Overview. BMJ 2025.")),
        htmltools::tags$li(
          htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2024-081904",
                            target = "_blank",
                            "Guyatt G, et al. Core GRADE 2: Certainty rating target, imprecision. BMJ 2025.")),
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
      )
      # The "pooling is only a small part of a systematic review" paragraph
      # used to be restated here, verbatim, from the Step 1 header. It is now
      # EDU_COPY$intro_modal and is shown once at the start of the session.
    ),

    pma_wizard_nav(current_step = 4)
  )
}

step4_server <- function(input, output, session, state) {

  # ----- Multi-outcome Summary of Findings --------------------------------
  # state$outcomes is a named list of pmatools objects saved on the Step 3
  # "Final certainty" tab (see pma_outcomes_list()). It is exactly what the
  # vendored grade_table() consumes, so no reshaping is needed here.
  saved_outcomes <- shiny::reactive(pma_outcomes_list(state$outcomes))

  # Signature of the dataset currently loaded in Step 1; saved outcomes whose
  # own signature differs were rated on other data (pma_outcomes_stale()).
  current_signature <- shiny::reactive(pma_dataset_signature(state$data))
  n_stale_outcomes  <- shiny::reactive(
    sum(pma_outcomes_stale(saved_outcomes(), current_signature())))

  # Arm labels for the "Risk with ..." column headers. Reuse the Step 2 arm
  # values when they exist so the combined table speaks the same
  # Intervention / Control vocabulary as the rest of the wizard.
  .arm_labels <- function() {
    e <- input$experimental_label
    c_ <- input$control_label
    list(
      intervention = if (!is.null(e) && length(e) == 1 && nzchar(e)) e else "intervention",
      control      = if (!is.null(c_) && length(c_) == 1 && nzchar(c_)) c_ else "control"
    )
  }

  # One rare-event alert per saved outcome (Core GRADE 6). NULL entries are
  # dropped, so an empty list means nothing in the table is rare.
  combined_rare_alerts <- shiny::reactive({
    outs <- saved_outcomes()
    if (length(outs) == 0) return(list())
    alerts <- lapply(names(outs), function(nm) {
      pma_rare_event_alert(outs[[nm]], label = nm)
    })
    alerts[!vapply(alerts, is.null, logical(1))]
  })

  combined_sof <- shiny::reactive({
    outs <- saved_outcomes()
    if (length(outs) == 0) return(NULL)
    # Row order and the primary grouping both come from state now, driven by
    # the per-row controls in pma_saved_outcomes_ui() and validated by
    # pmatools' reorder_outcomes() / set_primary() (see step3_server()). The
    # standalone "Primary outcome(s)" selector that used to sit above the
    # table is gone: two controls for one property is one too many.
    primary <- intersect(state$sof_primary %||% character(0), names(outs))
    if (length(primary) == 0) primary <- NULL
    arms <- .arm_labels()
    tryCatch({
      ft <- grade_table(
        outs,
        primary            = primary,
        # Same Core GRADE 6 layout as the Step 3 preview and the exported
        # single-outcome table (PMA_SOF_STYLE, ui_helpers.R).
        style              = PMA_SOF_STYLE,
        palette            = PMA_SOF_PALETTE,
        per                = state$display$per        %||% 1000,
        prediction         = isTRUE(state$display$prediction),
        # follow_up / unit are deliberately NOT passed: grade_table() reads
        # them off each saved object (.display_arg_from_outcomes), which is
        # what lets two rows carry different follow-up times.
        label_intervention = arms$intervention,
        label_control      = arms$control
      )
      notes <- c(vapply(combined_rare_alerts(), function(a) a$note,
                        character(1)),
                 PMA_SOF_LIMITATIONS_NOTE)
      pma_sof_add_notes(ft, notes)
    },
      error = function(e) {
        structure(list(message = conditionMessage(e)), class = "pma_sof_error")
      }
    )
  })

  output$sof_intro_block <- shiny::renderUI({
    htmltools::p(
      class = "pma-card-subtitle",
      if (length(saved_outcomes()) == 0) EDU_COPY$multi_outcome$step4_empty
      else EDU_COPY$multi_outcome$step4_intro)
  })

  # Warning banner above the combined SoF preview. Deliberately does NOT
  # gate the download: the user decides whether the mixed rows belong.
  output$sof_stale_warning <- shiny::renderUI({
    pma_stale_warning_banner(n_stale_outcomes())
  })
  shiny::outputOptions(output, "sof_stale_warning", suspendWhenHidden = FALSE)

  output$combined_sof_block <- shiny::renderUI({
    outs <- saved_outcomes()
    if (length(outs) == 0) return(NULL)
    ft <- combined_sof()
    body <- if (inherits(ft, "pma_sof_error")) {
      htmltools::p(paste("Combined SoF render error:", ft$message))
    } else {
      tryCatch(flextable::htmltools_value(ft),
               error = function(e)
                 htmltools::p(paste("Combined SoF render error:",
                                    conditionMessage(e))))
    }
    htmltools::tagList(
      lapply(combined_rare_alerts(), pma_rare_event_banner),
      pma_sof_scroller(body),
      pma_sof_limitations_ui(),
      pma_saved_outcomes_ui(outs, delete_input_id = "outcome_delete",
                            signature = current_signature(),
                            primary = state$sof_primary)
    )
  })

  # Write a flextable into a landscape .docx. Mirrors the helper that lives
  # inside the vendored export_bundle(); duplicated here because that one is
  # function-local and the vendored package must not be edited.
  .save_landscape_docx <- function(ft, path) {
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_end_section_landscape(doc, w = 11, h = 8.5)
    print(doc, target = path)
    invisible(path)
  }

  # Append sof_table_combined.docx to the ZIP that export_bundle() produced.
  # Done here rather than inside export_bundle() so the vendored package
  # stays untouched; zip::zip_append writes into the existing archive.
  .append_combined_sof <- function(zip_path) {
    outs <- saved_outcomes()
    if (length(outs) == 0) return(invisible(FALSE))
    ft <- combined_sof()
    if (is.null(ft) || inherits(ft, "pma_sof_error")) {
      shiny::showNotification(
        paste0("Combined SoF table skipped: ",
               if (inherits(ft, "pma_sof_error")) ft$message else "not available"),
        type = "warning", duration = 8)
      return(invisible(FALSE))
    }
    dir <- tempfile("pmatools_sof_combined_")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    dir.create(dir)
    path <- file.path(dir, "sof_table_combined.docx")
    ok <- tryCatch({
      .save_landscape_docx(ft, path)
      zip::zip_append(zipfile = zip_path,
                      files   = basename(path),
                      root    = dir)
      TRUE
    }, error = function(e) {
      shiny::showNotification(
        paste("Combined SoF table could not be added to the ZIP:",
              conditionMessage(e)),
        type = "warning", duration = 8)
      FALSE
    })
    invisible(ok)
  }

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

  # Gate the Download button on Steps 2-3 being complete. Without this,
  # pressing the always-active button before running the analysis produced
  # a confusing ERROR.zip. The downloadHandler keeps its own guards as a
  # second line of defense (e.g. state cleared between render and click).
  .blocked_note <- function(...) {
    htmltools::div(
      class = "pma-card-subtitle",
      style = paste(
        "border: 1px dashed hsl(var(--border));",
        "border-radius: 6px;",
        "padding: 0.75rem;",
        "margin-top: 0.5rem;",
        "text-align: center;"
      ),
      ...
    )
  }

  output$download_zip_ui <- shiny::renderUI({
    if (is.null(state$ma) || is.null(state$grade)) {
      missing <- c(
        if (is.null(state$ma)) "Step 2 (run the meta-analysis)",
        if (is.null(state$grade)) "Step 3 (open the Certainty assessment)"
      )
      return(.blocked_note(
        paste0("Download unavailable - complete ",
               paste(missing, collapse = " and "), " first.")
      ))
    }
    # W4-A output gate: the ZIP (which includes the GRADE Evidence Profile /
    # SoF docx) stays locked until every certainty domain has been reviewed
    # and confirmed in Step 3. Navigation itself is never blocked.
    keys <- pma_unconfirmed_domain_keys(state$domain_confirmed)
    if (length(keys)) {
      return(.blocked_note(
        htmltools::p(style = "margin: 0;",
          htmltools::strong("Download locked - certainty assessment incomplete.")),
        htmltools::p(style = "margin: 0.25rem 0 0;",
          # Each name is a link back to the tab that clears it, so the lock
          # names its own remedy and gets the reviewer there in one click.
          pma_domain_jump_links(
            keys, "dl_jump_",
            before = "Tick 'I have reviewed this domain' in Step 3 for: ",
            after = "."))
      ))
    }
    shiny::downloadButton("download_zip", "Download ZIP",
                          class = "btn btn-primary",
                          style = "width: 100%; margin-top: 0.5rem;")
  })

  # The download lock names the domains that hold it; each name jumps back to
  # its Step 3 tab. Two moves, in this order: the step first, so app.R rebuilds
  # the Step 3 body and the tabset exists, then the tab selection. Shiny
  # dispatches rendered values before input messages within one flush, which is
  # what makes that order hold (see the restore observer in R/step3_grade.R).
  #
  # Declared once over the fixed domain keys, because the links themselves are
  # rebuilt with the note and a freshly rendered actionLink reports 0.
  for (.domain_key in names(PMA_DOMAIN_LABELS)) {
    local({
      key <- .domain_key
      link_id <- paste0("dl_jump_", key)
      shiny::observeEvent(input[[link_id]], {
        if (!isTRUE((input[[link_id]] %||% 0L) > 0L)) return()
        state$step <- 3L
        shiny::updateTabsetPanel(session, "grade_tabs",
                                 selected = PMA_DOMAIN_LABELS[[key]])
        session$sendCustomMessage("scroll_top", list())
      }, ignoreInit = TRUE)
    })
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
          "Cannot export: please open Step 3 (Certainty assessment) at least once before downloading.",
          type = "error", duration = NULL
        )
        return()
      }
      # Second line of defense for the W4-A output gate (the button should
      # not even render while domains are unconfirmed).
      unconf <- pma_unconfirmed_domains(state$domain_confirmed)
      if (length(unconf)) {
        shiny::showNotification(
          paste0("Cannot export: review and confirm every certainty domain ",
                 "in Step 3 first. Unconfirmed: ",
                 paste(unconf, collapse = ", "), "."),
          type = "error", duration = NULL
        )
        return()
      }

      # Create the staging dir at function scope and register cleanup
      # immediately, so it is removed on success AND on error (item 11).
      tmp_dir <- tempfile("pmatools_export_")
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      dir.create(tmp_dir)

      shiny::withProgress(
        message = "Building export bundle", value = 0,
      tryCatch({
        shiny::incProgress(0.05, detail = "Collecting settings...")
        include <- input$include %||% c("data","script","results",
                                        "forest","forest_rob","funnel",
                                        "funnel_trimfill",
                                        "pubias_missing_forest",
                                        "grade_table", "sof_combined")

        rob_vec <- .export_covariate(state$ma, "rob", default = "*")

        # export_bundle() renders every plot, writes the tables and docx
        # report, and zips them in one vendored call, so the bulk of the
        # work sits inside this single step.
        shiny::incProgress(
          0.10,
          detail = "Rendering plots, tables, and report (this may take a while)..."
        )
        # Without grade_args, the analysis.R written into every bundle renders
        # each grade_meta() argument the app supplied as its template default
        # - `small_values = NULL` among them, even though Step 2 now REQUIRES
        # the reviewer to answer it - so the "reproducible" script did not
        # reproduce the rating it shipped with. The specs are built beside the
        # grade_meta() call that used them and carried on the rated object
        # (pma_grade_arg_specs(), ui_helpers.R).
        grade_args <- attr(state$grade, PMA_GRADE_ARGS_ATTR, exact = TRUE)

        # Footnotes the bundler cannot derive: the rare-event alert (computed
        # from the same rates the SoF absolute columns rest on) and the
        # not-implemented note shown under every on-screen table.
        sof_notes <- c(
          pma_rare_event_alert(
            state$grade,
            baseline_risk = if (isTRUE(state$display$convert)) {
              state$display$baseline_risk
            })$note,
          PMA_SOF_LIMITATIONS_NOTE)

        # export_bundle() is an S3 generic as of pmatools 0.5.0 and its first
        # formal is 'x'; pass the meta object positionally.
        out <- export_bundle(
          state$ma,
          grade        = state$grade,
          grade_args   = grade_args,
          output_dir   = tmp_dir,
          bundle_name  = input$bundle_name %||% "pmatools_results",
          include      = include,
          # Same Core GRADE 6 layout, follow-up line and footnotes as the
          # on-screen table (pmatools >= 0.5.1); the bundler writes
          # grade_table.docx and sof_table.docx itself, and the analysis.R it
          # generates reproduces exactly what it wrote.
          style        = PMA_SOF_STYLE,
          follow_up    = state$display$follow_up,
          unit         = state$display$unit,
          sof_notes    = sof_notes,
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
        shiny::incProgress(0.75, detail = "Packaging ZIP...")
        # Multi-outcome SoF: added to the archive after export_bundle() has
        # built it. Additive - the single-outcome Evidence Profile and SoF
        # docx above are untouched.
        if ("sof_combined" %in% include) {
          shiny::incProgress(0.02, detail = "Adding combined SoF table...")
          .append_combined_sof(out)
        }
        file.copy(out, file)
        shiny::incProgress(0.08, detail = "Done.")
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
        err_dir <- tempfile("pmatools_export_error_")
        # Cleanup on handler exit; zip::zipr below reads err_txt first.
        on.exit(unlink(err_dir, recursive = TRUE), add = TRUE)
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
      )
    },
    contentType = "application/zip"
  )

}
