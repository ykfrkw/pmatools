# step4_export.R - Step 4: ZIP export

# Every artifact export_bundle.pmatools_set() can write. Named here rather
# than repeated in the checkbox and again in the download handler's fallback,
# which is how the two drifted apart before.
PMA_EXPORT_INCLUDE_DEFAULT <- c(
  "data", "script", "results", "forest", "forest_full", "forest_rob",
  "funnel", "funnel_trimfill", "pubias_missing_forest", "sof",
  "evidence_profile", "indirectness", "readme")

step4_ui <- function() {
  s <- EDU_COPY$steps$step4

  htmltools::tagList(
    pma_step_header(s$title),

    # Multi-outcome Summary of Findings, assembled from the assessments the
    # app banks whenever every certainty domain of an outcome is confirmed
    # (shiny/SPEC.md 3.4.14). Shown before the bundle settings so the user can
    # check the table they are about to export.
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
      # The values ARE export_bundle()'s `include` vocabulary, passed through
      # untranslated. A remap in the download handler would hide which
      # artifact each box actually controls from the next reader of either
      # side (shiny/SPEC.md 3.5.2).
      shiny::checkboxGroupInput("include", "Artifacts to include",
        choices = c(
          "Long-format CSV"                                      = "data",
          "Reproducible R script"                                = "script",
          "Results text"                                         = "results",
          "Forest plot (PDF)"                                    = "forest",
          "Forest plot, all studies (PDF, after a low-RoB refit)" = "forest_full",
          "Forest plot stratified by RoB (PDF)"                  = "forest_rob",
          "Funnel plot (PDF)"                                    = "funnel",
          "Trim-and-fill funnel (PDF, k>=10)"                    = "funnel_trimfill",
          "Publication bias missing-results forest (PDF, k>=10)" = "pubias_missing_forest",
          "Summary of Findings table, all outcomes (docx + csv)" = "sof",
          "Core GRADE Evidence Profile (docx)"                   = "evidence_profile",
          "Indirectness table (docx)"                            = "indirectness",
          "README"                                               = "readme"
        ),
        selected = PMA_EXPORT_INCLUDE_DEFAULT),
      shiny::uiOutput("rare_export_note"),
      # The Download button is rendered server-side so it only appears once
      # Steps 2-3 have produced results (see output$download_zip_ui).
      shiny::uiOutput("download_zip_ui")
    ),

    pma_card(
      title = "How to cite",
      htmltools::p(paste0(
        "Pairwise meta-analysis was performed using the {meta} R package ",
        "(Balduzzi S, et al. Evid Based Ment Health. 2019). Certainty of ",
        "evidence was rated following the BMJ 2025 Core GRADE series ",
        "(Guyatt G, et al. BMJ. 2025), implemented in pmatools ",
        "(Furukawa Y, https://yukifurukawa.jp/pmatools/).")),
      # No DOIs and no hyperlinks, but software is not an article: a reader
      # copying this into a manuscript needs somewhere to find pmatools, and
      # there is no journal, volume or DOI to point them at. The URL is plain
      # text, so the block still reads as one style rather than one linked
      # entry among six.
      htmltools::tags$ul(
        htmltools::tags$li(
          "Furukawa Y. pmatools. 2025. https://yukifurukawa.jp/pmatools/"),
        htmltools::tags$li(
          "Balduzzi S, et al. Evid Based Ment Health. 2019."),
        htmltools::tags$li("Core GRADE 1. Guyatt G, et al. BMJ. 2025."),
        htmltools::tags$li("Core GRADE 2. Guyatt G, et al. BMJ. 2025."),
        htmltools::tags$li("Core GRADE 3. Guyatt G, et al. BMJ. 2025."),
        htmltools::tags$li("Core GRADE 4. Guyatt G, et al. BMJ. 2025."),
        htmltools::tags$li("Core GRADE 5. Guyatt G, et al. BMJ. 2025.")
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
  # state$outcomes is a named list of pmatools objects, banked automatically
  # once every certainty domain of an outcome is confirmed (see
  # pma_outcomes_list()). It is exactly what the vendored grade_table()
  # consumes, so no reshaping is needed here.
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
    # state, not input: Step 2's widgets no longer exist by the time this step
    # is on screen, so the inputs read NULL and both headers fell back to the
    # generic wording.
    e <- state$arm_e
    c_ <- state$arm_c
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

  # The rendered table half of the block below, split out so the block itself
  # reads as "table when there is one, list always".
  .combined_sof_table_block <- function() {
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
      pma_sof_scroller(body)
    )
  }

  # The table and the list of rows that build it. The list used to be rendered
  # here only when there was at least one outcome, and a second copy of it sat
  # on Step 3; there is one copy now, and its empty state is a sentence rather
  # than nothing - see shiny/SPEC.md 3.5.5.
  output$combined_sof_block <- shiny::renderUI({
    outs <- saved_outcomes()
    has_rows <- length(outs) > 0
    htmltools::tagList(
      if (has_rows) .combined_sof_table_block(),
      # The reviewer's question on arriving here is "did the outcome I just
      # confirmed land?", and counting rows was the only way to answer it.
      if (has_rows) htmltools::p(
        class = "pma-card-subtitle",
        style = "margin-top: 0.75rem; margin-bottom: 0;",
        sprintf("%d outcome%s saved - add another, or download below.",
                length(outs), if (length(outs) == 1) "" else "s")),
      pma_saved_outcomes_ui(outs, delete_input_id = "outcome_delete",
                            empty_text = EDU_COPY$multi_outcome$list_empty,
                            signature = current_signature(),
                            primary = state$sof_primary)
    )
  })

  # The outcomes the ZIP is built from. Normally the banked ones; the rating
  # on screen when there are none. Step 3 banks an outcome once every domain
  # is confirmed AND it has a name, while the download unlocks on the domains
  # alone, so an unnamed outcome can reach this button with nothing banked.
  .export_outcomes <- function() {
    outs <- saved_outcomes()
    if (length(outs) > 0) return(outs)
    g <- state$grade
    if (is.null(g)) return(list())
    g$follow_up <- state$display$follow_up
    g$unit      <- state$display$unit
    g <- pma_bank_export_material(
      g, display = state$display, pubias_missing = state$pubias_missing,
      rare = if (isTRUE(state$rare_mode_active)) state$rare,
      data = state$data,
      experimental_label = state$arm_e, control_label = state$arm_c)
    named <- function(v) !is.null(v) && length(v) == 1L && nzchar(trimws(v))
    nm <- if (named(state$outcome_name)) trimws(state$outcome_name)
          else if (named(g$outcome_name)) trimws(g$outcome_name)
          # The name is what every row of the table, every directory in the ZIP
          # and every message about this outcome is keyed on. It cannot be
          # blank, and this is the one path that can reach here without one.
          else "Outcome"
    stats::setNames(list(g), nm)
  }

  output$rare_export_note <- shiny::renderUI({
    if (!isTRUE(state$rare_mode_active)) return(NULL)
    htmltools::p(
      class = "pma-card-subtitle",
      style = "margin-top: 0.5rem;",
      "Rare-events mode: the outcome's own directory in the ZIP also holds ",
      "the rare-event diagnostics, the method table and the ",
      "method-sensitivity forest plot."
    )
  })

  # Risk-of-Bias labels for the stratified forest plots, keyed by outcome.
  # Each outcome's labels are read from the data IT was rated on: a review
  # whose outcomes came from separate files has a different study list per
  # outcome, and the set-wide `rob` vector export_bundle() also accepts could
  # only be right for one of them. A study with no label is drawn as "*"
  # rather than dropping the whole plot.
  .export_rob <- function(outs) {
    labels <- lapply(names(outs), function(nm) {
      g   <- outs[[nm]]
      src <- attr(g, PMA_OUTCOME_SOURCE_ATTR, exact = TRUE)
      d   <- if (is.list(src)) src$data else NULL
      if (!is.data.frame(d) || !all(c("studlab", "rob") %in% names(d))) {
        return(NULL)
      }
      lookup <- d[!duplicated(d$studlab), c("studlab", "rob"), drop = FALSE]
      out <- as.character(lookup$rob[match(as.character(g$meta$studlab),
                                           as.character(lookup$studlab))])
      out[is.na(out) | !nzchar(trimws(out))] <- "*"
      out
    })
    names(labels) <- names(outs)
    labels[!vapply(labels, is.null, logical(1))]
  }

  # Footnotes for the exported Summary of Findings that the bundler cannot
  # derive: one rare-event alert per outcome (Core GRADE 6) and the
  # not-implemented note shown under every on-screen table. Built from the
  # outcomes being exported rather than from combined_rare_alerts(), so the
  # single rating on screen gets its alert too.
  .export_sof_notes <- function(outs) {
    alerts <- lapply(names(outs), function(nm) {
      pma_rare_event_alert(outs[[nm]], label = nm)
    })
    alerts <- alerts[!vapply(alerts, is.null, logical(1))]
    c(vapply(alerts, function(a) a$note, character(1)),
      PMA_SOF_LIMITATIONS_NOTE)
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
        include <- input$include %||% PMA_EXPORT_INCLUDE_DEFAULT

        outs <- .export_outcomes()
        arms <- .arm_labels()

        # export_bundle() renders every plot, writes the tables and docx
        # report, and zips them in one vendored call, so the bulk of the
        # work sits inside this single step.
        shiny::incProgress(
          0.10,
          detail = "Rendering plots, tables, and report (this may take a while)..."
        )

        # One pmatools_set for every outcome, so the ZIP gets the combined
        # Summary of Findings at its root and one outcomes/NN_name/ directory
        # per outcome. The per-outcome display arguments, the grade_meta()
        # specs the bundled analysis.R is rendered from and the data each
        # outcome was rated on all travel ON the outcomes (see
        # pma_export_set(), ui_helpers.R): read from the live state here they
        # would describe whichever outcome is on screen.
        out <- export_bundle(
          pma_export_set(outs, primary = state$sof_primary),
          output_dir   = tmp_dir,
          bundle_name  = input$bundle_name %||% "pmatools_results",
          include      = include,
          # Same Core GRADE 6 layout and footnotes as the combined table on
          # this page; the bundler writes summary_of_findings.docx itself, and
          # the analysis.R it generates reproduces exactly what it wrote.
          style        = PMA_SOF_STYLE,
          sof_notes    = .export_sof_notes(outs),
          per          = state$display$per        %||% 1000,
          prediction   = state$display$prediction %||% FALSE,
          rob          = .export_rob(outs),
          label_intervention = arms$intervention,
          label_control      = arms$control
        )
        shiny::incProgress(0.75, detail = "Packaging ZIP...")
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
