# step3_pubias.R - the Step 3 publication-bias wizard, whole
#
# Split out of step3_grade.R, whose step3_server() had grown past three and a
# half thousand lines and buried the wizard's reactive graph in the middle of
# it. Everything Figure 5 needs is here: the derived node, the breadcrumb, the
# four question cards, the funnel / trim-and-fill / missing-results reference
# tabs, and the three status dots on their titles. Nothing here rates anything
# - the answers still reach grade_meta() through grade_obj(), which stayed in
# step3_grade.R.
#
# WHY THIS IS NOT A SHINY MODULE, and the invariant to preserve:
# step3_pubias_server() is a plain function of `input`, `output`, `session` and
# `state`, NOT a shiny::moduleServer() behind an NS(). Namespacing would rename
# every input and output id the wizard touches, and those ids are spelled out
# in step3_ui() (still in step3_grade.R), in the conditionalPanel conditions on
# this tab, and in the app suite. So: no NS(), no moduleServer(), ids stay
# bare. That is what let the split be pure motion.
#
# The dot-named arguments are deliberate for the same reason. `.rare_active`,
# `.display_args` and `.threshold_grade_args` are step3_server()'s own
# closures, which the wizard reads by those names; naming the parameters after
# them keeps every call site in the body identical to what it was before the
# move.
#
# Sourced BEFORE R/step3_grade.R (see local_files in app.R), though R only
# needs the definitions to exist by call time.

# The studies that can actually enter a funnel-plot test: finite TE, finite and
# positive seTE. obj$k counts studies with missing results too, so gating on it
# let the k >= 10 surfaces disagree about whether statistical assessment was
# feasible at all. File scope, because it reads only its argument.
.effective_pubias_k <- function(obj) {
  te <- obj$TE
  se <- obj$seTE
  if (!is.null(te) && !is.null(se) &&
      length(te) == length(se) && length(te) > 0L) {
    return(sum(is.finite(te) & is.finite(se) & se > 0))
  }
  obj$k %||% 0L
}

# The wizard's whole server half. Called from step3_server() at the point the
# block used to sit at, so source order - and therefore the order outputs are
# registered in - is unchanged. `session` is taken but unused: every server
# fragment in this app has the same four leading arguments, and a fragment
# that quietly drops one is a trap for the next reactive that needs it.
#
# Returns pubias_reopen, the one piece of stored state, so that
# state$step3_reset() can clear it when the reviewer moves to another outcome.
step3_pubias_server <- function(input, output, session, state,
                                threshold_abs_state,
                                threshold_baseline_state,
                                .rare_active,
                                .display_args,
                                .threshold_grade_args) {
  # Forced here rather than at first use, so that a caller passing something
  # it later rebinds cannot change what the wizard reads mid-session.
  force(threshold_abs_state)
  force(threshold_baseline_state)
  force(.rare_active)
  force(.display_args)
  force(.threshold_grade_args)

  # ----- Publication bias: Figure 5 as a wizard --------------------------
  # The node on screen is DERIVED from the answers by step3_pubias_node()
  # (R/step3_threshold.R, pure and unit-tested), never stored as a cursor of
  # its own. Changing Q1 therefore re-derives everything downstream instead of
  # leaving the reviewer parked on a question the algorithm no longer reaches.
  #
  # pubias_reopen is the one piece of stored state: a breadcrumb click. The
  # derivation honours it ahead of itself, but only for a node the current
  # answers actually put on the path, so re-opening Q1 and answering "yes"
  # cannot strand the reviewer on a Q3 that no longer exists.
  pubias_reopen <- shiny::reactiveVal(NULL)

  pubias_k <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(0L)
    .effective_pubias_k(obj)
  })

  # Egger's regression, computed once and read by three surfaces: the callout
  # under the funnel, the same callout inside the Q3 wizard node, and the
  # flowchart above the wizard. It used to be computed inline inside
  # output$pubias_egger_result, which is why the chart could not read it - and
  # a reviewer who accepted the automated test therefore saw the lit trail stop
  # dead at the Q3 node for the rest of the assessment.
  #
  # `feasible` is not the same thing as a missing p value: below k = 10 the
  # test is not run at all and nothing is shown, whereas a test that ran and
  # failed says so. Collapsing the two would print "could not be computed" at
  # every small meta-analysis.
  pubias_egger <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj) || .effective_pubias_k(obj) < 10) {
      return(list(feasible = FALSE, p = NA_real_, asymmetric = NA))
    }
    res <- tryCatch(
      suppressWarnings(meta::metabias(obj, method.bias = "linreg")),
      error = function(e) NULL
    )
    pval <- if (is.null(res)) NULL else res$p.value
    if (is.null(pval) || length(pval) != 1L || is.na(pval)) {
      return(list(feasible = TRUE, p = NA_real_, asymmetric = NA))
    }
    pval <- as.numeric(pval)
    list(feasible = TRUE, p = pval, asymmetric = pval < STEP3_EGGER_ALPHA)
  })

  pubias_node <- shiny::reactive({
    step3_pubias_node(
      small_industry    = input$pubias_small_industry,
      registry_complete = input$pubias_registry_complete,
      funnel_asymmetry  = input$pubias_funnel_asymmetry,
      unpublished       = input$pubias_unpublished,
      k                 = pubias_k(),
      reopen            = pubias_reopen(),
      # Fig 5's Q2 has two ways to answer "not feasible", and the wizard has to
      # take the same one assess_pubias() does, or the reviewer answers a
      # question the rating then ignores.
      rare_flow         = .rare_active()
    )
  })

  # Advancing happens ON ANSWER: each input clears the re-open, and the
  # derivation moves on by itself. No node cursor to keep in step, and no
  # Next button that could disagree with the algorithm.
  for (.pb_id in c("pubias_small_industry", "pubias_registry_complete",
                   "pubias_funnel_asymmetry", "pubias_unpublished")) {
    local({
      id <- .pb_id
      shiny::observeEvent(input[[id]], {
        pubias_reopen(NULL)
      }, ignoreInit = TRUE)
    })
  }
  # Breadcrumb clicks. One observer per node id; the link is only rendered
  # for nodes that are both answered and on the current path.
  for (.pb_node in c("q1", "extra", "q3", "q4")) {
    local({
      nd <- .pb_node
      shiny::observeEvent(input[[paste0("pubias_open_", nd)]], {
        pubias_reopen(nd)
      }, ignoreInit = TRUE)
    })
  }

  # Unnumbered. The node KEYS keep Fig 5's q1/q3/q4, but the reviewer never
  # sees the numbers: the chart interleaves the registry node between Q1 and
  # Q2, so on screen the numbering described neither Fig 5 nor the route.
  #
  # These name the breadcrumb's re-open links and nothing else. The breadcrumb
  # no longer restates each answer beside them - the lit chart above shows the
  # route, in the algorithm's own shape, and did it better than a prose trail.
  PUBIAS_NODE_TITLES <- c(
    q1     = "Small and industry-sponsored?",
    extra  = "Overall reporting-bias judgment",
    q3     = "Funnel plot asymmetry",
    q4     = "Unpublished studies documented?",
    result = "Result"
  )

  # Figure 5, from the first node onwards, lit by the answers so far rather
  # than by the `flow_path` fact: that fact arrives only once grade_meta() has
  # rated the domain, which is exactly when a progress indicator is of no
  # further use. step3_pubias_flow_ids() (R/step3_threshold.R, pure and
  # unit-tested) does the wizard-key -> figure-id translation.
  output$pubias_flowchart <- shiny::renderUI({
    if (is.null(state$ma)) return(NULL)
    htmltools::tagList(
      pma_flowchart(
        PMA_FLOWCHART_FIGS[["Publication bias"]]$fig,
        on_ids = step3_pubias_flow_ids(
          small_industry    = input$pubias_small_industry,
          registry_complete = input$pubias_registry_complete,
          funnel_asymmetry  = input$pubias_funnel_asymmetry,
          unpublished       = input$pubias_unpublished,
          k                 = pubias_k(),
          # Accepting the automated test is an ANSWER, so the chart has to be
          # able to light the leaf it reaches. The sentinel is still not
          # forwarded to grade_meta() (see grade_obj()): "egger" means "let
          # assess_pubias() decide", and lighting the chart is a display
          # concern that must not change what is rated.
          egger_asymmetric  = pubias_egger()$asymmetric,
          rare_flow         = .rare_active()),
        caption = pma_algorithm_source("Publication bias")),
      # The chart's second node is the k gate, which is computed rather than
      # asked, so the chart can light the branch but cannot print the number
      # it turned on. It used to be a breadcrumb line; it belongs under the
      # picture of the node it decides.
      htmltools::div(class = "pma-crumb pma-crumb-auto",
                     step3_pubias_k_line(pubias_k(), .rare_active()))
    )
  })
  shiny::outputOptions(output, "pubias_flowchart", suspendWhenHidden = FALSE)

  # Links only. This used to restate every answer in prose beside the node
  # title; the lit chart above says the same thing in one glance and says it
  # in the algorithm's own shape. What prose could not replace is the
  # "change" affordance - without a way back into an answered node, a
  # one-question-at-a-time wizard is a trap - so that is all that is left.
  output$pubias_breadcrumb <- shiny::renderUI({
    if (is.null(state$ma)) return(NULL)
    node <- pubias_node()
    path <- step3_pubias_reachable(input$pubias_small_industry,
                                   input$pubias_registry_complete,
                                   pubias_k(), .rare_active())
    .answered <- function(nd) {
      v <- switch(nd,
        q1    = input$pubias_small_industry,
        extra = input$pubias_registry_complete,
        q3    = input$pubias_funnel_asymmetry,
        q4    = input$pubias_unpublished,
        NULL)
      !is.null(v) && length(v) == 1L && nzchar(v)
    }
    # Walked in path order, so the links read in the order the algorithm took.
    crumbs <- lapply(setdiff(path, "result"), function(nd) {
      if (identical(nd, node) || !.answered(nd)) return(NULL)
      htmltools::div(
        class = "pma-crumb",
        shiny::actionLink(paste0("pubias_open_", nd),
                          paste("Change:", PUBIAS_NODE_TITLES[[nd]]))
      )
    })
    crumbs <- Filter(Negate(is.null), crumbs)
    if (!length(crumbs)) return(NULL)
    htmltools::div(class = "pma-crumbs", crumbs)
  })
  shiny::outputOptions(output, "pubias_breadcrumb", suspendWhenHidden = FALSE)

  output$pubias_wizard <- shiny::renderUI({
    if (is.null(state$ma)) return(htmltools::p("Run analysis first."))
    node <- pubias_node()
    k    <- pubias_k()
    rare <- .rare_active()
    path <- step3_pubias_reachable(input$pubias_small_industry,
                                   input$pubias_registry_complete, k, rare)

    # ONE container for every node. Each node used to return a bare tagList,
    # so the live question - the only thing on the tab that can be answered -
    # was visually indistinguishable from the reference plots below it and
    # from the override <details> below those. A reviewer could not tell what
    # they were being asked. The card, the accent border and the progress line
    # are all that distinction takes, and they are built here rather than
    # pasted into four nodes so the four cannot drift apart again.
    #
    # The convention the nodes now share: the question is the HEADING, and
    # the widget's own label is NULL. Two of them used to carry a second,
    # differently worded question string in the widget label, which asked the
    # reviewer to decide which of the two wordings was the real question.
    .question <- function(heading, ...) {
      progress <- step3_pubias_question_line(node, path)
      htmltools::div(
        class = "pma-wizard-question",
        if (!is.null(progress)) {
          htmltools::div(class = "pma-wizard-progress", progress)
        },
        htmltools::h5(heading),
        ...
      )
    }

    if (identical(node, "q1")) {
      return(.question(
        "Most or all studies small AND industry-sponsored?",
        htmltools::p(class = "pma-card-subtitle",
          paste0("A 'yes' rates down 1 on its own and ends the assessment; ",
                 "nothing after it can undo the concern.")),
        shiny::radioButtons("pubias_small_industry", NULL,
          choices = c("No" = "no", "Yes" = "yes"),
          selected = character(0), inline = FALSE)
      ))
    }

    if (identical(node, "extra")) {
      return(.question(
        "Overall, does the situation argue against reporting bias?",
        # The provenance paragraph that used to open this node is deleted: the
        # three radio labels below already say what each answer does.
        #
        # The criteria that follow are not. They are the grounds for a
        # judgment the algorithm cannot compute, so they are two visible
        # sentences rather than a <details> full of examples.
        htmltools::p(class = "pma-card-subtitle",
          paste0("Suspect reporting bias when grey literature went ",
                 "unsearched, the evidence is a few early positive trials, ",
                 "or prior work documents it for this comparison.")),
        htmltools::p(class = "pma-card-subtitle",
          paste0("It is unlikely when unpublished studies were found and ",
                 "agree, or prospective registration is the field standard ",
                 "with no discrepancies.")),
        # Two answers, and only "Yes" decides anything. "No" used to force
        # rate down 1 on its own, which is a rule Core GRADE 4 Fig 5 does not
        # have; it now means what a third "leave it to the Figure 5 nodes"
        # option used to mean, so that option is gone with the rule.
        shiny::radioButtons("pubias_registry_complete", NULL,
          choices = c(
            "No - reporting bias is possible; go on to the Figure 5 nodes"
              = "no",
            "Yes - reporting bias is unlikely; do not rate down"
              = "yes"
          ),
          selected = character(0), inline = FALSE)
      ))
    }

    if (identical(node, "q3")) {
      return(.question(
        "Does funnel plot asymmetry strongly suggest publication bias?",
        # The <details> under this used to give the provenance of p < 0.05.
        # Deleted: the flowchart caption on this tab names the implementing
        # function, and the sentence changed no answer.
        htmltools::p(class = "pma-card-subtitle",
          sprintf(paste0("Egger's test is run on the funnel plot below, at ",
                         "p < %.2f. Accept it, or replace it with your own ",
                         "visual judgment."), STEP3_EGGER_ALPHA)),
        # The same callout the Funnel sub-tab prints. It lives two clicks away
        # from here, which is where the reviewer is asked to accept or reject
        # the very number it reports.
        .pubias_egger_callout(pubias_egger()),
        # Still a select rather than radios: four options, and one of them is
        # the sentinel that means "I looked and I accept the test".
        shiny::selectInput("pubias_funnel_asymmetry", NULL,
          choices = c(
            "(choose)"                             = "",
            "Accept the automated Egger test"      = STEP3_PUBIAS_USE_EGGER,
            "Funnel symmetric (visual override)"   = "no",
            "Funnel asymmetric (visual override)"  = "yes")),
        shiny::conditionalPanel(
          "input.pubias_funnel_asymmetry == 'no' || input.pubias_funnel_asymmetry == 'yes'",
          shiny::textAreaInput(
            "pubias_fa_rationale",
            "Rationale (required for the visual override)",
            rows = 2, width = "100%",
            placeholder = paste0(
              "State why your visual judgment replaces the automated ",
              "Egger's test."))
        )
      ))
    }

    if (identical(node, "q4")) {
      return(.question(
        "Unpublished studies documented?",
        htmltools::p(class = "pma-card-subtitle",
          if (rare) {
            # k is named as well as the reason: a reviewer looking at 14
            # studies and a registry question needs to see that the study
            # count was not what sent them here.
            sprintf(paste0("Egger's test loses validity on sparse binary ",
                           "data, so Figure 5 routes here even at k = %d. ",
                           "Documented unpublished trials rate down 1."), k)
          } else {
            sprintf(paste0("Egger's test is unreliable at k = %d, so Figure 5 ",
                           "routes here. Documented unpublished trials rate ",
                           "down 1."), k)
          }),
        shiny::radioButtons("pubias_unpublished", NULL,
          choices = c("No" = "no", "Yes" = "yes"),
          selected = character(0), inline = FALSE)
      ))
    }

    NULL
  })
  shiny::outputOptions(output, "pubias_wizard", suspendWhenHidden = FALSE)

  # The verdict is the one block still gated on a node: it is the wizard's
  # conclusion, and printing it before the wizard has run says the domain was
  # rated when nothing has been answered. The reference plots below it are no
  # longer gated at all - a companion pubias_show_funnel used to keep the
  # funnel to node q3, which hid it exactly when a reviewer wanted to check it
  # against a different question.
  #
  # Assigning a reactive to an output is what makes it readable from a
  # conditionalPanel condition; suspendWhenHidden = FALSE because the panel it
  # gates is initially hidden, and a suspended output never evaluates.
  output$pubias_show_result <- shiny::reactive({
    !is.null(state$ma) && identical(pubias_node(), "result")
  })
  shiny::outputOptions(output, "pubias_show_result", suspendWhenHidden = FALSE)

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

  # Egger's auto judgment displayed as colour-coded callout, under the funnel
  # it is computed from and again beside the Q3 question that consumes it.
  output$pubias_egger_result <- shiny::renderUI({
    .pubias_egger_callout(pubias_egger())
  })
  shiny::outputOptions(output, "pubias_egger_result",
                       suspendWhenHidden = FALSE)

  # The trim-and-fill fit itself, computed once. Three surfaces read it - the
  # funnel, the numerical summary and the tab's status dot - and meta::trimfill()
  # refits the model, so three independent calls would triple that work on
  # every reactive flush. The k gate lives here rather than in each reader,
  # which is also what keeps the three from disagreeing about when
  # trim-and-fill exists at all.
  pubias_trimfill_fit <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj) || !step3_pubias_statistical(.effective_pubias_k(obj))) {
      return(NULL)
    }
    tryCatch(suppressWarnings(meta::trimfill(obj)), error = function(e) NULL)
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
    tf <- pubias_trimfill_fit()
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
    # Same k as Q2, the funnel block and the missing-results forest. This
    # used to gate on the raw obj$k, which counts studies with missing
    # results too, so a dataset with missing-results studies could show the
    # trim-and-fill summary while Q2 said statistical analysis was not
    # feasible (and vice versa).
    if (is.null(obj) || .effective_pubias_k(obj) < 10) return(NULL)
    tf <- pubias_trimfill_fit()
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

    # The same 20% exaggeration question the Risk of bias tab asks of the low
    # risk of bias subset, asked here of the trim-and-fill adjustment. It is
    # material for the funnel-asymmetry question above and NOTHING else: Core
    # GRADE 4 Fig 5 has no trim-and-fill node, so the arithmetic and the
    # wording both live in the package (R/pubias_trimfill.R), where a test can
    # hold them to that.
    # Step 2 makes small_values required, so it is set by the time a fitted
    # analysis exists; anything else is normalised to NULL rather than passed
    # on, because the package function rejects a value it does not know and an
    # aborting renderUI would replace this panel with a stack trace.
    sv_direction <- if (identical(state$small_values, "desirable") ||
                        identical(state$small_values, "undesirable")) {
      state$small_values
    } else {
      NULL
    }
    inflation <- .pubias_trimfill_inflation(
      te_original  = te_orig,
      te_adjusted  = te_adj,
      small_values = sv_direction)
    inflation_line <- .pubias_trimfill_line(
      inflation, te_original = te_orig, te_adjusted = te_adj,
      format_te = fmt)

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
                if (sign_flips) "  [direction flips]" else "")),
      htmltools::p(
        style = paste0(
          "margin: 0.4rem 0 0; padding-left: 0.5rem; border-left: 3px solid ",
          if (isTRUE(inflation$exaggerated)) "#c07020" else "#208050", ";"),
        inflation_line)
    )
  })

  # ----- Reference: Subgroup analysis (Available vs Missing results) -----
  # Schema: studlab (chr), n (int), results_known (chr), source (chr), built by
  # .pubias_missing_empty() at file scope. source = "auto" for dataset-derived
  # rows (NA TE in meta_obj); "user" for rows added via "+ Add missing trial".

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
    # Effective k, not obj$k: see output$pubias_trimfill_summary.
    if (is.null(obj) || .effective_pubias_k(obj) < 10) {
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

  # ----- Reference tabs: the status dots ---------------------------------
  # One dot per reference tab, on the tab title, saying what that tab's
  # diagnostic found. They exist because the three tabs are a tabset: a
  # reviewer who never clicks past Funnel never learns that the RoB-ME table
  # they filled in is enough to overturn the result.
  #
  # NOTHING HERE RATES ANYTHING. No value below reaches grade_obj(),
  # assess_pubias() or grade_meta() - the wizard's answers stay the only
  # thing that rates the domain. The arithmetic and the wording both live in
  # the package (R/pubias_status.R, R/pubias_missing.R) where a test can hold
  # them to that; what is here is the wiring and only the wiring.

  # The Core GRADE threshold on the TE scale, which is where both the
  # trim-and-fill zones and the RoB-ME conclusion are measured. Derived with
  # the package's own threshold_to_te_scale(), i.e. the same call grade_meta()
  # makes on the same arguments, so the dot cannot end up judging against a
  # different threshold from the rating beside it. NULL when the app has no
  # usable threshold, which both dots read as "the null".
  .dot_threshold_internal <- function(obj) {
    if (is.null(obj)) return(NULL)
    th <- .threshold_grade_args(obj)
    if (is.null(th$threshold)) return(NULL)
    res <- tryCatch(
      threshold_to_te_scale(
        threshold          = th$threshold,
        threshold_scale    = th$threshold_scale,
        sm                 = obj$sm,
        threshold_baseline = th$threshold_baseline,
        meta_obj           = obj),
      error = function(e) NULL)
    v <- res$threshold_internal
    if (is.null(v) || length(v) != 1L || !is.finite(v) || v <= 0) return(NULL)
    v
  }

  # Step 2 makes small_values required, so it is set by the time a fitted
  # analysis exists; anything else is normalised to NULL rather than passed
  # on, exactly as output$pubias_trimfill_summary does.
  .dot_small_values <- function() {
    sv <- state$small_values
    if (identical(sv, "desirable") || identical(sv, "undesirable")) sv else NULL
  }

  output$pubias_dot_funnel <- shiny::renderUI({
    if (is.null(state$ma)) return(NULL)
    egger <- pubias_egger()
    pma_tab_status_dot(.pubias_funnel_dot(
      p         = egger$p,
      feasible  = egger$feasible,
      k_ok      = step3_pubias_statistical(pubias_k()),
      # Egger loses validity on sparse binary data, so a rare-event analysis
      # gets no colour at all rather than an invalid p value painting a red
      # one. state$rare_diagnostics is what Step 2 already keeps.
      rare_flow = isTRUE(state$rare_diagnostics$rare_flow),
      alpha     = STEP3_EGGER_ALPHA))
  })
  shiny::outputOptions(output, "pubias_dot_funnel", suspendWhenHidden = FALSE)

  output$pubias_dot_trimfill <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    tf  <- pubias_trimfill_fit()
    k_ok <- step3_pubias_statistical(.effective_pubias_k(obj))
    binary <- step3_is_binary_outcome(obj, input$outcome_type)
    baseline <- threshold_baseline_state()
    pma_tab_status_dot(.pubias_trimfill_dot(
      te_original  = obj$TE.random,
      te_adjusted  = if (is.null(tf)) NA_real_ else tf$TE.random,
      small_values = .dot_small_values(),
      k_ok         = k_ok,
      sm           = obj$sm,
      binary       = binary,
      # Per 1,000 on screen, a proportion here. NA when the reviewer has
      # cleared the box, which is one of the ways the dot goes uncoloured
      # rather than silently changing scale.
      baseline_risk      = if (is.finite(baseline)) baseline / 1000
                           else NA_real_,
      threshold_abs1000  = threshold_abs_state(),
      threshold_internal = .dot_threshold_internal(obj),
      # The event-rate map the Configuration tab already uses, injected
      # rather than reimplemented in the package: see R/pubias_status.R.
      p1_from_ratio      = step3_p1_from_ratio))
  })
  shiny::outputOptions(output, "pubias_dot_trimfill",
                       suspendWhenHidden = FALSE)

  output$pubias_dot_missing <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    miss <- state$pubias_missing %||% .pubias_missing_empty()
    random <- isTRUE(obj$random)
    n_total <- if (!is.null(obj$n.e) && !is.null(obj$n.c) &&
                   length(obj$n.e) == length(obj$seTE) &&
                   length(obj$n.c) == length(obj$seTE)) {
      obj$n.e + obj$n.c
    } else {
      rep(NA_real_, length(obj$seTE))
    }
    pma_tab_status_dot(.pubias_missing_dot(
      results_known = miss$results_known,
      n_missing     = miss$n,
      te_obs        = step3_pooled_te(obj),
      se_pooled     = if (random) obj$seTE.random else obj$seTE.common,
      tau2          = obj$tau2,
      ci_lower      = if (random) obj$lower.random else obj$lower.common,
      ci_upper      = if (random) obj$upper.random else obj$upper.common,
      pi_lower      = obj$lower.predict,
      pi_upper      = obj$upper.predict,
      se_studies    = obj$seTE,
      n_studies     = n_total,
      threshold_internal = .dot_threshold_internal(obj),
      k             = .effective_pubias_k(obj)))
  })
  shiny::outputOptions(output, "pubias_dot_missing", suspendWhenHidden = FALSE)

  # The breadcrumb's stored state, handed back to step3_server() so that
  # state$step3_reset() can clear it. Nothing else in the wizard escapes.
  pubias_reopen
}
