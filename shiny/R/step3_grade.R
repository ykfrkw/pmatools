# step3_grade.R - Step 3: GRADE 5-domain assessment + Final certainty (sub-tabs)
#
# The pure helpers behind the Configuration tab (the threshold conversions, the
# pooled control-group risk, and the presentation pieces that only read their
# arguments) live in R/step3_threshold.R.

# --------------------------------------------------------------------------
# Indirectness subdomains (Core GRADE 5 / pmatools indirectness_subdomains)
# --------------------------------------------------------------------------
# The four PICO questions are inputs to grade_meta(indirectness_subdomains =),
# so the UI and the server have to agree on the input ids, the subdomain
# labels pmatools expects, and the 4-point answer scale. File scope, because
# both step3_ui() and step3_server() read them.
#
# Asking the indirectness question per PICO element is Core GRADE 5's; the
# 4-point scale below and the worst-case fold pmatools applies to it are
# pmatools conventions (see EDU_COPY$domains$indirectness$gradient).
STEP3_INDIR_SUBDOMAINS <- c(
  Population   = "indir_population",
  Intervention = "indir_intervention",
  Comparison   = "indir_comparator",
  Outcome      = "indir_outcome"
)

STEP3_INDIR_ANSWERS <- c(
  "Yes"          = "yes",
  "Probably yes" = "probably_yes",
  "Probably no"  = "probably_no",
  "No"           = "no"
)

# pmatools maps the 4-point answer onto a GRADE level; mirrored here so the
# app can tell whether an overall rating restates the worst case (no rationale
# needed) or overrides it (rationale required) without a second grade_meta()
# call.
STEP3_INDIR_ANSWER_TO_LEVEL <- c(
  yes          = "no",
  probably_yes = "no",
  probably_no  = "some_concerns",
  no           = "serious"
)

# --------------------------------------------------------------------------
# Pure helpers of step3_server()
# --------------------------------------------------------------------------
# Lifted out of the server body without a change of text. Each reads only its
# arguments (plus vendored functions), so nothing was gained by holding them in
# a closure over input / output / session / state. The ones that DO read those
# - .rob_some_concerns_setting(), .study_covariate(), .sel_val(), .rat_val(),
# .override_or_ignore(), .step3_validate_value(), .display_args(),
# .step3_bulk_set(), .control_risk_block() - stayed where they were.

.na_null <- function(x) {
  if (is.null(x)) return(NULL)
  if (length(x) == 0) return(NULL)
  # An empty numericInput arrives as logical NA, not NA_real_, so test for
  # NA before looking at the type: is.numeric(NA) is FALSE and a type-first
  # check let empty OIS fields count as user input (W4-A gate) and reach
  # grade_meta() as NA instead of NULL.
  if (all(is.na(x))) return(NULL)
  if (is.character(x) && !any(nzchar(x))) return(NULL)
  x
}

.study_labels_for_grade <- function(obj) {
  studs <- as.character(obj$studlab)
  if (!length(studs)) return(character())
  if (identical(attr(obj, "pma_rare_engine"), "mmeta")) return(studs)
  if (!is.null(obj$k) && length(studs) == obj$k) return(studs)
  keep <- !is.na(obj$TE)
  if (length(keep) == length(studs)) return(studs[keep])
  studs
}

.effective_pubias_k <- function(obj) {
  te <- obj$TE
  se <- obj$seTE
  if (!is.null(te) && !is.null(se) &&
      length(te) == length(se) && length(te) > 0L) {
    return(sum(is.finite(te) & is.finite(se) & se > 0))
  }
  obj$k %||% 0L
}

pmatools_GRADE_DOWNGRADE <- function(j) {
  # 3-level system (v0.3+): -1 = some_concerns, -2 = serious. Legacy
  # labels are still mapped so old user input doesn't break.
  c(no = 0, some = -1, some_concerns = -1,
    serious = -2, very_serious = -2)[[j]]
}

.pubias_missing_empty <- function() {
  data.frame(studlab = character(0), n = integer(0),
             results_known = character(0),
             source = character(0),
             stringsAsFactors = FALSE)
}

.outcome_set <- function(outs, primary = character(0)) {
  .new_pmatools_set(outcomes = outs, order = names(outs),
                    primary = intersect(as.character(primary), names(outs)))
}

# Binary consequence of a three-level entry, under the reviewer's chosen
# boundary. "some" and an unrated study follow `some_as`; an explicit "low"
# is always low and an explicit "high" always high. Purely a display of
# what grade_meta() will do; it changes nothing.
.rob_risk_group <- function(v, some_as = "high") {
  side <- if (identical(some_as, "high")) "High" else "Low"
  v <- tolower(trimws(as.character(v)))
  out <- rep(paste0(side, " (unrated)"), length(v))
  out[!is.na(v) & v == "low"]  <- "Low"
  out[!is.na(v) & v == "some"] <- paste0(side, " (some concerns)")
  out[!is.na(v) & v == "high"] <- "High"
  out
}

# Sub-tab navigation inside Step 3. File scope rather than local to
# step3_ui(), because step3_server() renders EVERY copy of it: each Next is
# gated on something that can flip while the tab is on screen, and a statically
# built button could not follow (see output$grade_nav_<key>).
.grade_nav <- function(back_id, back_label, next_id, next_label = "Next",
                       next_disabled = FALSE, next_title = NULL) {
  htmltools::div(
    style = paste(
      "display: flex;",
      "justify-content: space-between;",
      "margin-top: 1.5rem;"),
    shiny::actionButton(back_id, back_label,
      class = "btn btn-secondary"),
    # See pma_wizard_nav(): TRUE / NULL, never a string, never FALSE.
    shiny::actionButton(next_id, next_label,
      class = "btn btn-primary",
      title = next_title,
      disabled = if (isTRUE(next_disabled)) TRUE else NULL)
  )
}

# The Back / Next pair on each of the five domain tabs, keyed by the domain key
# of PMA_DOMAIN_LABELS. step3_ui() places uiOutput("grade_nav_<key>") and
# step3_server() renders it; the ids are the ones the existing
# observeEvent(input$grade_next_*) handlers already listen on.
STEP3_DOMAIN_NAVS <- list(
  rob = list(
    back_id = "grade_back_rob",    back_label = "Back: Configuration",
    next_id = "grade_next_rob",    next_label = "Next: Inconsistency"),
  inconsistency = list(
    back_id = "grade_back_incon",  back_label = "Back: Risk of Bias",
    next_id = "grade_next_incon",  next_label = "Next: Indirectness"),
  indirectness = list(
    back_id = "grade_back_indir",  back_label = "Back: Inconsistency",
    next_id = "grade_next_indir",  next_label = "Next: Imprecision"),
  imprecision = list(
    back_id = "grade_back_impre",  back_label = "Back: Indirectness",
    next_id = "grade_next_impre",  next_label = "Next: Publication bias"),
  pubias = list(
    back_id = "grade_back_pubias", back_label = "Back: Imprecision",
    next_id = "grade_next_pubias", next_label = "Next: Final certainty")
)

# Why the Next is dead, on the button itself: the checkbox that revives it is
# one screen up, and a greyed button with no explanation reads as a bug.
STEP3_CONFIRM_GATE_TITLE <-
  "Tick 'I have reviewed this domain' to continue"

step3_ui <- function(state = NULL) {
  s <- EDU_COPY$steps$step3

  # app.R's step_body renderUI rebuilds this whole body on every step change,
  # and a freshly built widget pushes its declared default back to the server.
  # Everything else on this step is outcome-scoped and is meant to be cleared
  # that way; rob_some_concerns is not. It is a review-wide convention that
  # persists across outcomes, so a hard-coded selected = "high" would silently
  # undo a reviewer's "low" on every 3 -> 2 -> 3 round trip. Seeding from the
  # mirrored state is the same trick step2_ui() uses for the outcome fields.
  rob_some_concerns_default <- "high"
  if (!is.null(state)) {
    v <- shiny::isolate(state$rob_some_concerns)
    if (!is.null(v) && length(v) == 1L && v %in% c("low", "high")) {
      rob_some_concerns_default <- v
    }
  }

  .domain_header <- function(name, badge_id, chip_id) {
    htmltools::div(
      class = "pma-accordion-title",
      style = "margin-bottom: 0.5rem;",
      htmltools::h4(class = "pma-domain-name",
                    style = "margin: 0; font-size: 1.1rem;", name),
      shiny::uiOutput(badge_id, inline = TRUE),
      shiny::uiOutput(chip_id,  inline = TRUE)
    )
  }

  .inputs_details <- function(..., title = "Inputs for this domain", open = TRUE) {
    htmltools::tags$details(
      open = if (isTRUE(open)) NA else NULL,
      htmltools::tags$summary(title),
      htmltools::div(...)
    )
  }
  .override_details <- function(..., title = "Override final judgment") {
    htmltools::tags$details(
      htmltools::tags$summary(title),
      htmltools::div(...)
    )
  }
  # Rationale textarea shown only while the paired override select has a
  # non-empty value. A written rationale is mandatory for manual overrides
  # (pmatools v0.4.0 breaking change; Core GRADE transparency principle).
  .override_rationale <- function(select_id, rationale_id) {
    shiny::conditionalPanel(
      sprintf("(input['%s'] || '') != ''", select_id),
      shiny::textAreaInput(
        rationale_id,
        "Rationale (required for override)",
        rows = 2, width = "100%",
        placeholder = "State why the automated assessment was replaced."
      )
    )
  }
  # Title of a gated tab: the label, plus the slot its progress marker is
  # painted into. tabsetPanel accepts a tag list as a title, but tabPanel then
  # has no string to derive its value from, so every gated tab states its
  # `value` explicitly - it is what updateTabsetPanel() and grade_tab_sequence
  # match on, and what PMA_DOMAIN_LABELS maps a domain key to.
  .tab_title <- function(key) {
    htmltools::tagList(
      PMA_DOMAIN_LABELS[[key]],
      shiny::uiOutput(paste0("grade_tab_mark_", key), inline = TRUE)
    )
  }

  # Explicit per-domain confirmation checkbox (output gate W4-A). Ticking it is
  # the ONE thing that confirms the domain (pma_domain_confirmations()), so it
  # is also what un-greys the Next button below it.
  .confirm_checkbox <- function(id,
                                label = "I have reviewed this domain") {
    htmltools::div(
      style = paste(
        "margin-top: 1rem; padding: 0.5rem 0.75rem;",
        "border: 1px dashed hsl(var(--border)); border-radius: 6px;"),
      shiny::checkboxInput(id, label, value = FALSE, width = "100%")
    )
  }

  htmltools::tagList(
    # Highlights the branch each domain judgment took in its flowchart (see
    # pma_flowchart_details() and www/flowchart.js). Loaded as part of the
    # Step 3 body, exactly as R/step2_ma.R loads required-fields.js, so it
    # re-executes on every renderUI rebuild of that body -- which is what
    # repaints the highlight after the DOM is thrown away.
    htmltools::tags$script(src = "flowchart.js"),

    pma_step_header(s$title),

    # Which studies the numbers on this step came from. Renders nothing when
    # the analysis rests on all studies, and the bare uiOutput wrapper is
    # unstyled, so the "all studies" case adds no box and no whitespace.
    # (A sticky "FINAL CERTAINTY" bar used to sit above this one; it was
    # removed because the Final certainty tab states the same thing properly.)
    shiny::uiOutput("analysis_set_indicator"),

    pma_card(
      title = htmltools::tagList(
        "Certainty assessment (Core GRADE series)",
        # How much of the assessment is done, beside its name. The same count
        # is on the stepper, where it is visible from the other three steps.
        shiny::uiOutput("grade_progress_badge", inline = TRUE)
      ),
      shiny::tabsetPanel(
        id = "grade_tabs",

        # --- Configuration (cross-cutting; everything the five domains
        #     depend on is established and confirmed here, in the order a
        #     reviewer needs to decide it: control-group risk, then the
        #     threshold, then how the effect is presented) ---
        # The 115-word `EDU_COPY$config_tab$intro` that opened this tab is
        # gone. Every boxed section below states its own purpose beside the
        # control it belongs to, which is where a reviewer reads it.
        shiny::tabPanel(.tab_title("threshold"), value = "Configuration",
          htmltools::h4("Configuration",
                        style = "margin: 0 0 0.5rem; font-size: 1.1rem;"),
          shiny::uiOutput("threshold_panel"),
          # How event rates are DISPLAYED, for the whole app. It used to be a
          # numericInput on the Final certainty tab, three screens away from
          # the control-group risk and the absolute threshold it relabels.
          shiny::uiOutput("per_panel"),
          # The risk-of-bias conventions used to be a boxed section here. Both
          # have gone: `rob_inf_threshold` is deleted outright (the package
          # default of 0.10 applies unconditionally), and `rob_some_concerns`
          # moved to the Risk of Bias tab, next to the verdict it produces.
          # Its SCOPE is unchanged - still one review-wide setting that
          # persists across outcomes - only the point of edit moved.
          shiny::uiOutput("config_status"),
          .confirm_checkbox("threshold_confirm",
            paste0("I have reviewed and confirm this configuration ",
                   "(required before export; the default values are fine ",
                   "if you agree with them)")),
          # Unlike every other sub-tab, this Next IS gated. The threshold
          # drives Risk of Bias, Inconsistency and Imprecision, so it has to
          # be settled before the reviewer works through them; letting them
          # walk the domains against a provisional threshold would mean
          # re-doing the work. Rendered as its own output so the gate can
          # flip without rebuilding the tab. See also output$grade_nav_final.
          shiny::uiOutput("grade_nav_config")
        ),

        # --- Risk of Bias ---
        shiny::tabPanel(.tab_title("rob"), value = "Risk of Bias",
          .domain_header("Risk of Bias", "rob_badge", "rob_chip"),
          shiny::uiOutput("analysis_set_banner_rob"),
          # The "How is this judged?" accordion is gone from all five domain
          # tabs; the flowchart under the verdict draws the same algorithm and
          # lights up the branch taken. output$rob_how_body went with it, and
          # so did output$rob_rule_note before that.
          pma_reference(EDU_COPY$domains$rob$ref),
          # Review-wide, and edited here because this is the tab where it
          # decides something: it sets which side of the binary split each
          # study falls on, and the stratified forest below draws exactly that
          # split. It persists across outcomes, unchanged by this move.
          .inputs_details(open = TRUE, title = "Inputs for this domain",
            shiny::radioButtons("rob_some_concerns",
              "Where do studies rated 'some concerns' belong?",
              choices = c(
                "Some concerns count as high risk of bias (default)" = "high",
                "Some concerns count as low risk of bias"            = "low"),
              selected = rob_some_concerns_default),
            htmltools::p(class = "pma-card-subtitle",
              paste0("Core GRADE 4 leaves this boundary open, so it is a ",
                     "review decision. Unrated studies follow the same ",
                     "side."))
          ),
          shiny::uiOutput("threshold_block_rob"),
          shiny::uiOutput("rob_evaluation"),
          htmltools::tags$details(
            class = "pma-edit-details",
            htmltools::tags$summary(
              class = "pma-edit-summary",
              htmltools::HTML(
                "&#9998;&nbsp; <strong>Edit per-study Risk of Bias</strong> ",
                "<span class='pma-edit-hint'>(click to expand &middot; ",
                "manually override each study or apply bulk presets)</span>"
              )
            ),
            htmltools::div(
              class = "pma-edit-body",
              htmltools::p(class = "pma-card-subtitle",
                           "Click a cell to type low / some / high, or set ",
                           "them all at once. Synced with Step 1."),
              htmltools::div(
                style = "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 0.5rem;",
                shiny::actionButton("step3_rob_set_low",  "Set all to Low",  class = "btn-sm"),
                shiny::actionButton("step3_rob_set_some", "Set all to Some", class = "btn-sm"),
                shiny::actionButton("step3_rob_set_high", "Set all to High", class = "btn-sm"),
                shiny::actionButton("step3_rob_clear",    "Clear all",       class = "btn-sm")
              ),
              DT::DTOutput("step3_rob_editor")
            )
          ),
          # Two strata, not four: the plot now draws the same low/high fold
          # the algorithm analysed (plot_forest_rob(some_concerns_as = )).
          # The heading says "as analysed" for the same reason.
          htmltools::h5("Forest plot stratified by risk of bias (as analysed)",
                        style = "margin-top: 1rem;"),
          htmltools::div(class = "pma-forest-image",
            shinycssloaders::withSpinner(
              shiny::imageOutput("rob_forest", height = "auto"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "320px")),
          pma_forest_display_panel("rob"),
          .override_details(
            shiny::selectInput("rob_override", NULL,
              choices = pma_judgment_choices()),
            .override_rationale("rob_override", "rob_override_rationale")
          ),
          .confirm_checkbox("rob_confirm_na"),
          shiny::uiOutput("grade_nav_rob")
        ),

        # --- Inconsistency ---
        shiny::tabPanel(.tab_title("inconsistency"), value = "Inconsistency",
          .domain_header("Inconsistency", "incon_badge", "incon_chip"),
          pma_reference(EDU_COPY$domains$inconsistency$ref),
          shiny::uiOutput("threshold_block_inco"),
          shiny::uiOutput("incon_evaluation"),
          htmltools::h5("Forest plot"),
          htmltools::div(class = "pma-forest-image",
            shinycssloaders::withSpinner(
              shiny::imageOutput("incon_forest", height = "auto"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "320px")),
          pma_forest_display_panel("incon"),
          # Core GRADE 3 has three steps and the app used to ask for all
          # three. Two of them are DERIVED: Step 1 from the I-squared
          # surrogate and Step 2 from the zone tally over the study estimates,
          # both computed by .auto_inconsistency() and both now reported as
          # facts above. Asking a reviewer to answer a question the algorithm
          # has already answered - and then, on the manual path, aborting
          # unless they answered a second one too - was the worst of both.
          #
          # Step 3 is genuinely not auto-detectable (subgroup credibility;
          # Core GRADE 3 points at ICEMAN), so it stays open - but only when
          # the automated path has actually landed on the opposite-sides
          # branch where it decides anything. suspendWhenHidden = FALSE on the
          # flag, or the panel would never appear.
          shiny::conditionalPanel(
            "output.incon_subgroup_relevant === true",
            .inputs_details(open = TRUE, title = "Inputs for this domain",
              htmltools::p(class = "pma-card-subtitle",
                paste0("Study estimates fall on both sides of the threshold. ",
                       "Does a credible subgroup explain that?")),
              shiny::radioButtons("subgroup_explained",
                "Explained by a credible subgroup analysis?",
                choices = c("Yes" = "yes", "No" = "no"),
                selected = character(0))
            )
          ),
          .override_details(
            shiny::selectInput("incon_override", NULL,
              choices = pma_judgment_choices()),
            .override_rationale("incon_override", "incon_override_rationale")
          ),
          .confirm_checkbox("incon_confirm_na"),
          shiny::uiOutput("grade_nav_inconsistency")
        ),

        # --- Indirectness ---
        shiny::tabPanel(.tab_title("indirectness"), value = "Indirectness",
          .domain_header("Indirectness", "indir_badge", "indir_chip"),
          pma_reference(EDU_COPY$domains$indirectness$ref),

          # ----- The four Core GRADE 5 PICO questions ----------------------
          # Every one PRESELECTED to "yes" - the default is now on screen
          # rather than in the code. Leaving them blank used to send
          # indirectness = "no" to grade_meta() (see grade_obj()), so the
          # domain scored no downgrade while the screen showed four unanswered
          # questions. The judgment is identical either way; what changes is
          # that the reviewer can see what they are accepting, and downgrades
          # the elements they have a concern about.
          #
          # This is why pma_domain_confirmations() no longer counts
          # "substantive input": a preselected radio would satisfy it the
          # moment it mounts. The checkbox below is the whole gate.
          .inputs_details(open = TRUE,
            title = "Inputs for this domain (Core GRADE 5 subdomains)",
            htmltools::p(class = "pma-card-subtitle",
              paste0("Is the evidence sufficiently direct? Answer each PICO ",
                     "element; the worst answer decides the domain.")),
            shiny::radioButtons("indir_population",
              "Population - trial population sufficiently similar to target patients?",
              choices = STEP3_INDIR_ANSWERS, inline = TRUE,
              selected = "yes"),
            shiny::radioButtons("indir_intervention",
              "Intervention - deliverable as studied?",
              choices = STEP3_INDIR_ANSWERS, inline = TRUE,
              selected = "yes"),
            shiny::radioButtons("indir_comparator",
              "Comparison - representative of usual care?",
              choices = STEP3_INDIR_ANSWERS, inline = TRUE,
              selected = "yes"),
            shiny::radioButtons("indir_outcome",
              "Outcome - patient-important, rather than a surrogate?",
              choices = STEP3_INDIR_ANSWERS, inline = TRUE,
              selected = "yes"),
            htmltools::p(class = "pma-card-subtitle",
                         EDU_COPY$domains$indirectness$surrogate),
            htmltools::p(class = "pma-card-subtitle",
                         EDU_COPY$domains$indirectness$gradient)
          ),

          # The subdomain table pmatools built from those answers. Surfaced
          # because it is the only rendering of exactly what was sent to
          # grade_meta(): it shows which element drove the worst-case fold,
          # and its footer repeats the Core GRADE 5 Table 2 gradient caveat
          # next to the judgment rather than only in the collapsed copy.
          shiny::uiOutput("indir_subdomain_table"),

          # Still no preselected value: blank means "accept the fold", which
          # is a different statement from any of the three ratings, and the
          # four PICO radios above are where the default now shows itself.
          htmltools::h5("Overall indirectness rating",
                        style = "margin-top: 1.25rem;"),
          htmltools::p(class = "pma-card-subtitle",
            paste0("Blank accepts the worst case above. Choose a rating to ",
                   "override it, with a written reason.")),
          shiny::radioButtons("indirectness", NULL,
            choices = pma_judgment_choices(include_blank = FALSE),
            selected = character(0), inline = TRUE),
          shiny::conditionalPanel(
            "(input.indirectness || '') != ''",
            shiny::textAreaInput(
              "indir_rationale",
              paste0("Rationale (required whenever this rating differs from ",
                     "the automatic judgment)"),
              rows = 2, width = "100%",
              placeholder = paste0(
                "State which element (population / intervention / comparison ",
                "/ outcome) drives the rating and why it outweighs the ",
                "worst-case fold.")
            )
          ),
          htmltools::tags$details(
            class = "pma-edit-details",
            htmltools::tags$summary(
              class = "pma-edit-summary",
              htmltools::HTML(
                "&#9998;&nbsp; <strong>Edit per-study Indirectness</strong> ",
                "<span class='pma-edit-hint'>(click to expand &middot; ",
                "optional per-study notes; they label the stratified forest ",
                "plot only. The subdomain answers above, or an overall ",
                "rating, are what feed Core GRADE)</span>"
              )
            ),
            htmltools::div(
              class = "pma-edit-body",
              htmltools::p(class = "pma-card-subtitle",
                           "Click a cell to type low / some / high, or set ",
                           "them all at once. Synced with Step 1."),
              htmltools::div(
                style = "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 0.5rem;",
                shiny::actionButton("step3_indir_set_low",  "Set all to Low",  class = "btn-sm"),
                shiny::actionButton("step3_indir_set_some", "Set all to Some", class = "btn-sm"),
                shiny::actionButton("step3_indir_set_high", "Set all to High", class = "btn-sm"),
                shiny::actionButton("step3_indir_clear",    "Clear all",       class = "btn-sm")
              ),
              DT::DTOutput("step3_indir_editor")
            )
          ),
          shiny::uiOutput("indir_forest_image_block"),
          pma_forest_display_panel("indir"),
          .confirm_checkbox("indir_confirm_na"),
          shiny::uiOutput("grade_nav_indirectness")
        ),

        # --- Imprecision ---
        shiny::tabPanel(.tab_title("imprecision"), value = "Imprecision",
          .domain_header("Imprecision", "impre_badge", "impre_chip"),
          pma_reference(EDU_COPY$domains$imprecision$ref),
          shiny::uiOutput("threshold_block_impre"),
          # Which Fig 4 branch this analysis took. Stated on the tab because
          # the branch decides whether sample size is consulted at all: on
          # the CI-crosses-threshold path the OIS is never reached, and a
          # reviewer reading OIS figures further down must be able to see
          # that they did not drive the judgment.
          htmltools::h5("Core GRADE 2 Figure 4 branch taken",
                        style = "margin-top: 1rem;"),
          shiny::uiOutput("impre_branch"),
          shiny::uiOutput("impre_evaluation"),
          .inputs_details(open = TRUE, title = "Inputs for this domain",
            shiny::conditionalPanel(
              "input.outcome_type == 'binary'",
              shiny::uiOutput("ois_p0_ui"),
              # Core GRADE 2 parameterises the BINARY OIS by a modest
              # relative risk reduction, not by the threshold, and names two
              # values. Both are offered; the absolute-scale equivalent is
              # shown alongside so the target can be read in the same units
              # as the threshold.
              shiny::radioButtons("ois_rrr",
                paste0("Modest relative risk reduction the OIS is powered to ",
                       "detect (Core GRADE 2 names 20% and 25%)"),
                choices = c("20 percent (default)" = "0.20",
                            "25 percent"           = "0.25"),
                selected = "0.20", inline = TRUE),
              shiny::uiOutput("ois_rrr_equiv")
            ),
            shiny::conditionalPanel(
              "input.outcome_type == 'continuous'",
              shiny::uiOutput("ois_sd_ui"),
              htmltools::p(class = "pma-card-subtitle",
                paste0("Core GRADE 2 points the continuous OIS at the ",
                       "threshold rather than a relative risk reduction, so ",
                       "none is asked for."))
            ),
            shiny::numericInput("ois_events_override",
              "Override OIS - target events (binary)",
              value = NA, min = 0, step = 1),
            shiny::numericInput("ois_n_override",
              "Override OIS - target N (continuous)",
              value = NA, min = 0, step = 1),
            # The Figure 4 quotation that used to follow was the caption of
            # the imprecision flowchart on this same tab.
            htmltools::p(class = "pma-card-subtitle",
              paste0("Either override replaces the calculated OIS. The events ",
                     "override also switches the comparison from participants ",
                     "to events."))
          ),
          .override_details(
            # The only sentence at the override, out of the nested <details>
            # it used to hide in: Figure 4's second two-level condition is the
            # one judgment the algorithm cannot make.
            htmltools::p(class = "pma-card-subtitle",
              paste0("Rate down two levels when the plain language summary ",
                     "warrants 'may' rather than 'likely'.")),
            shiny::selectInput("impre_override", NULL,
              choices = pma_judgment_choices()),
            .override_rationale("impre_override", "impre_override_rationale")
          ),
          .confirm_checkbox("impre_confirm_na"),
          shiny::uiOutput("grade_nav_imprecision")
        ),

        # --- Publication bias ---
        shiny::tabPanel(.tab_title("pubias"), value = "Publication bias",
          .domain_header("Publication bias", "pubias_badge", "pubias_chip"),
          pma_reference(EDU_COPY$domains$pubias$ref),
          # ----- Figure 5 as a wizard, one node at a time -------------------
          # The whole of Fig 5 used to render at once: a static Q1, a static
          # non-Fig-5 overall judgment with two bulleted lists, a
          # server-rendered Q2-Q4 block, a funnel, a trim-and-fill funnel and
          # an RoB-ME table - all on screen whether or not the algorithm had
          # reached them. The reviewer had no way to tell which questions were
          # actually live.
          #
          # Now: output$pubias_wizard renders exactly the current node, and
          # the node is DERIVED from the answers by step3_pubias_node()
          # (R/step3_threshold.R), mirroring assess_pubias()'s own
          # short-circuit order. Changing an earlier answer re-derives
          # everything after it. The breadcrumb above it re-opens any answered
          # node - without it a one-question-at-a-time wizard is a trap.
          shiny::uiOutput("pubias_breadcrumb"),
          shiny::uiOutput("pubias_wizard"),

          # The funnel plots are STATICALLY placed and shown by a
          # conditionalPanel, not rendered inside the wizard's renderUI:
          # imageOutput / the funnel display panels re-bind badly when the
          # container they live in is replaced on every answer.
          shiny::conditionalPanel(
            "output.pubias_show_funnel === true",
            shinycssloaders::withSpinner(
              shiny::imageOutput("pubias_funnel", height = "auto"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "320px"),
            pma_funnel_display_panel("funnel_pub"),
            shiny::uiOutput("pubias_egger_result"),
            htmltools::tags$details(
              htmltools::tags$summary("Reference: trim-and-fill"),
              htmltools::div(
                htmltools::p(class = "pma-card-subtitle",
                  "How the pooled estimate would shift if the imputed studies ",
                  "existed. ",
                  htmltools::HTML(paste0(
                    "<strong>Not part of the Core GRADE algorithm</strong>."))),
                shinycssloaders::withSpinner(
                  shiny::imageOutput("pubias_trimfill_funnel", height = "auto"),
                  type = 4, color = "#0f172a", size = 0.6,
                  proxy.height = "320px"),
                pma_funnel_display_panel("funnel_trim", include_egger = FALSE),
                shiny::uiOutput("pubias_trimfill_summary")
              )
            )
          ),

          # ----- The verdict, and the RoB-ME reference beside it ------------
          # DT::DTOutput must stay statically placed: inside a renderUI,
          # DT / htmlwidgets does not bind cleanly. It is therefore wrapped in
          # a conditionalPanel rather than moved. The output already carries
          # suspendWhenHidden = FALSE, so hiding it this way keeps the server
          # side alive.
          shiny::conditionalPanel(
            "output.pubias_show_result === true",
            shiny::uiOutput("pubias_evaluation"),
            htmltools::tags$details(
              htmltools::tags$summary(
                "Reference: available vs missing results (RoB-ME)"),
              htmltools::div(
                htmltools::p(class = "pma-card-subtitle",
                  "Studies with no extractable estimate arrive here ",
                  "automatically; add trials that exist but were never loaded. ",
                  "After RoB-ME (Page MJ, et al. BMJ. 2023)",
                  htmltools::HTML(paste0(
                    ". <strong>Not part of the Core GRADE algorithm</strong> - ",
                    "act on it through the override below."))),
                htmltools::div(
                  style = "display: flex; gap: 0.5rem; margin-bottom: 0.5rem;",
                  shiny::actionButton("pubias_missing_add",
                                      "+ Add missing trial", class = "btn-sm")
                ),
                htmltools::p(class = "pma-card-subtitle",
                  "Click any cell to edit; Results known suggests RoB-ME ",
                  "labels and accepts free text. Auto-classified rows cannot ",
                  "be removed."),
                # Datalist powering the in-cell autocomplete for results_known.
                htmltools::tags$datalist(id = "pubias_rk_datalist",
                  htmltools::tags$option(value = "Reported but data not extractable"),
                  htmltools::tags$option(value = "Not measured"),
                  htmltools::tags$option(value = "Measured but not reported (suspect P > 0.05)"),
                  htmltools::tags$option(value = "Measured but not reported (suspect P < 0.05)"),
                  htmltools::tags$option(value = "Measured but not reported (in the opposite direction)")
                ),
                # When DT injects an <input type="text"> on cell edit for the
                # results_known column (index 2 of the visible columns), give
                # it a `list` attribute so the browser shows the datalist
                # suggestions while still allowing free-text typing.
                htmltools::tags$script(htmltools::HTML(
                  "$(document).on('focusin', '#pubias_missing_editor input[type=text]', function(){",
                  "  var $td = $(this).closest('td');",
                  "  var col = $td.parent().children().index($td);",
                  "  if (col === 2) { $(this).attr('list', 'pubias_rk_datalist'); }",
                  "});"
                )),
                DT::DTOutput("pubias_missing_editor"),
                shinycssloaders::withSpinner(
                  shiny::imageOutput("pubias_missing_forest", height = "auto"),
                  type = 4, color = "#0f172a", size = 0.6,
                  proxy.height = "320px"),
                pma_forest_display_panel("pubias")
              )
            )
          ),

          .override_details(
            shiny::selectInput("pubias_override", NULL,
              choices = pma_judgment_choices()),
            .override_rationale("pubias_override", "pubias_override_rationale")
          ),
          .confirm_checkbox("pubias_confirm_na"),
          shiny::uiOutput("grade_nav_pubias")
        ),

        # --- Final certainty (7th tab) ---
        shiny::tabPanel("Final certainty",
          shiny::uiOutput("cert_incomplete_banner"),
          shiny::uiOutput("analysis_set_banner_cert"),
          htmltools::h5("Core GRADE Evidence Profile"),
          htmltools::div(
            style = "margin-top: 0.5rem; margin-bottom: 1rem;",
            shinycssloaders::withSpinner(
              shiny::uiOutput("final_certainty"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "200px")
          ),
          htmltools::hr(),
          htmltools::h5("Summary of Findings"),
          htmltools::div(
            style = "margin-top: 0.5rem; margin-bottom: 1rem;",
            shinycssloaders::withSpinner(
              shiny::uiOutput("sof_preview"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "200px")
          ),
          # The Heimke recommendation used to sit here as a standing italic
          # paragraph. It is now a footnote on the table it is about, written
          # by pma_sof_add_notes() - so it travels into the exported .docx
          # too, which it never did as page text.
          htmltools::hr(),

          # "Other considerations" is an ANSWER, so it stays open. Everything
          # else here is a display preference and collapses.
          htmltools::h5("Other considerations"),
          shiny::textInput("other_text",
            "Free text shown in the Evidence Profile", width = "100%",
            placeholder = "e.g., All trials conducted in a single country; reporting bias"),
          shiny::radioButtons("other_downgrade",
            "Apply an additional downgrade for the above?",
            choices = c("No (-0)" = "0",
                        "Yes, by 1 level (-1)" = "-1",
                        "Yes, by 2 levels (-2)" = "-2"),
            selected = "0", inline = TRUE),
          htmltools::tags$details(
            htmltools::tags$summary("Display options"),
            htmltools::div(
              shiny::uiOutput("outcome_name_echo"),
              # input$per moved to the Configuration tab, where it now
              # relabels the control-group risk and the absolute threshold as
              # well as the SoF. This is the read-only echo of that setting.
              shiny::uiOutput("per_echo"),
              shiny::checkboxInput("prediction",
                "Show 95 percent prediction interval in Effect column", FALSE),
              # The responder-conversion controls (the presentation choice,
              # baseline_risk_chinn, threshold_label) used to live here. They
              # moved to the Configuration tab: the control-group responder
              # proportion and the definition of the threshold of clinical
              # interest are not display preferences, they are inputs the
              # rating is read against, and they belong with the threshold
              # they mirror. The old convert_smd_to_or tick-box became the
              # two-way input$sof_presentation radio there; Step 4 reads the
              # guarded state$display mirror rather than the input, so the
              # rename did not reach it. chinn_invert lost its checkbox
              # entirely - it is now derived from the Step 2 direction answer
              # (see chinn_invert_derived()).
              shiny::uiOutput("display_options_config_note")
            )
          ),
          htmltools::hr(),

          # ----- Save this outcome for the multi-outcome SoF table -----
          # Sits at the end of the Final certainty tab: this is the point in
          # the wizard where the rating for one outcome is complete, and the
          # natural place to bank it before going back to Step 2 for the
          # next outcome.
          htmltools::h5("Saved outcomes for the Summary of Findings table"),
          htmltools::p(class = "pma-card-subtitle",
                       EDU_COPY$multi_outcome$save_intro),
          shiny::uiOutput("save_outcome_panel"),
          shiny::uiOutput("saved_outcomes_list"),

          # The only Next in Step 3 that leaves the step (every other one
          # just moves to the following sub-tab, which stays free), so it is
          # the only one gated on the domain confirmations. Rendered as its
          # own output so that gate can flip without rebuilding this tab.
          shiny::uiOutput("grade_nav_final")
        )
      )
    )
  )
}

step3_server <- function(input, output, session, state) {

  grade_tab_sequence <- c(
    "Configuration",
    "Risk of Bias",
    "Inconsistency",
    "Indirectness",
    "Imprecision",
    "Publication bias",
    "Final certainty"
  )

  advance_grade_tab <- function(current = NULL) {
    current <- current %||% shiny::isolate(input$grade_tabs) %||%
      grade_tab_sequence[1]
    idx <- match(current, grade_tab_sequence)
    if (is.na(idx)) idx <- 1L
    if (idx < length(grade_tab_sequence)) {
      shiny::updateTabsetPanel(
        session,
        "grade_tabs",
        selected = grade_tab_sequence[[idx + 1L]]
      )
      session$sendCustomMessage("scroll_top", list())
      return(invisible(TRUE))
    }
    commit <- state$step3_commit %||% function() TRUE
    if (isTRUE(commit())) {
      state$step <- 4L
      session$sendCustomMessage("scroll_top", list())
    }
    invisible(TRUE)
  }

  retreat_grade_tab <- function(current = NULL) {
    current <- current %||% shiny::isolate(input$grade_tabs) %||%
      grade_tab_sequence[1]
    idx <- match(current, grade_tab_sequence)
    if (is.na(idx)) idx <- 1L
    if (idx > 1L) {
      shiny::updateTabsetPanel(
        session,
        "grade_tabs",
        selected = grade_tab_sequence[[idx - 1L]]
      )
      session$sendCustomMessage("scroll_top", list())
      return(invisible(TRUE))
    }
    state$step <- 2L
    session$sendCustomMessage("scroll_top", list())
    invisible(TRUE)
  }

  shiny::observeEvent(input$grade_back_thresh, {
    retreat_grade_tab("Configuration")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_rob, {
    retreat_grade_tab("Risk of Bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_incon, {
    retreat_grade_tab("Inconsistency")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_indir, {
    retreat_grade_tab("Indirectness")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_impre, {
    retreat_grade_tab("Imprecision")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_pubias, {
    retreat_grade_tab("Publication bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_final, {
    retreat_grade_tab("Final certainty")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_thresh, {
    advance_grade_tab("Configuration")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_rob, {
    advance_grade_tab("Risk of Bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_incon, {
    advance_grade_tab("Inconsistency")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_indir, {
    advance_grade_tab("Indirectness")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_impre, {
    advance_grade_tab("Imprecision")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_pubias, {
    advance_grade_tab("Publication bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_final, {
    advance_grade_tab("Final certainty")
  }, ignoreInit = TRUE)
  # ----- Threshold state (single source of truth: Configuration tab;
  # independent of UI render so Final certainty doesn't flip as the user
  # clicks tabs) -----------------------------------------------------------
  # threshold_state          : relative (ratio) or te-scale value
  # threshold_mode_state     : "absolute" / "relative" (binary ratio SMs only)
  # threshold_abs_state      : absolute threshold, events per 1,000
  # threshold_baseline_state : baseline (control-group) risk, per 1,000
  THRESHOLD_MODE_DEFAULT   <- "absolute"
  threshold_state          <- shiny::reactiveVal(NA_real_)
  threshold_mode_state     <- shiny::reactiveVal(THRESHOLD_MODE_DEFAULT)
  threshold_abs_state      <- shiny::reactiveVal(NA_real_)
  threshold_baseline_state <- shiny::reactiveVal(NA_real_)
  # display_per_state        : the per-N DISPLAY unit (100 or 1,000), owned by
  # the Configuration tab and read by every rate on Step 3 plus sof_table().
  # Held in a reactiveVal for exactly the reason the thresholds are: leaving
  # Step 3 and coming back rebuilds every widget from its declared default, so
  # a radio declared with selected = "1000" would silently undo a reviewer's
  # switch to 100 on every 3 -> 2 -> 3 round trip.
  #
  # Deliberately NOT reset by state$step3_reset(): unlike a threshold, the
  # display unit is a property of the review, not of the outcome being rated.
  display_per_state        <- shiny::reactiveVal(STEP3_PER_DEFAULT)

  # Initialise defaults from suggest_threshold() / pooled CER as soon as
  # state$ma is available. These observers live outside renderUI on purpose:
  # renderUI is suspended while the tab is hidden, so grade_obj() would
  # otherwise see NA thresholds until the user first opened the tab.
  # observeEvent on state$ma, with the threshold reactiveVals read under
  # isolate(): a plain observe() also depended on them, so clearing the
  # threshold field immediately re-seeded it from suggest_threshold() and the
  # "no threshold" state was unreachable. Seed once per analysis instead.
  # Which analysis the current thresholds were seeded for. A threshold is
  # only meaningful on one scale, so when the summary measure changes (OR ->
  # SMD, say) the old value is discarded and the new suggestion applied;
  # otherwise an OR of 1.25 would silently become an SMD threshold of 1.25.
  threshold_seed_key <- shiny::reactiveVal(NA_character_)

  # Prefill the threshold reactiveVals from suggest_threshold(). Extracted
  # from the observer below so that voiding an outcome can re-run it: the
  # observer only fires on a change of state$ma, and it may already have fired
  # for this analysis by the time the reset lands.
  .seed_thresholds <- function() {
    obj <- shiny::isolate(state$ma)
    if (is.null(obj)) return(invisible(NULL))
    key <- paste(class(obj)[1], obj$sm %||% "", sep = "/")
    fresh <- !identical(key, shiny::isolate(threshold_seed_key()))
    if (fresh) {
      threshold_seed_key(key)
      threshold_state(NA_real_)
      threshold_abs_state(NA_real_)
      threshold_baseline_state(NA_real_)
    }
    s <- tryCatch(suggest_threshold(obj), error = function(e) NULL)
    sug <- step3_threshold_suggestions(s)
    # Only ever prefill a reactiveVal that is still NA: a value the user typed
    # must never be overwritten.
    if (is.na(shiny::isolate(threshold_state())) && !is.na(sug$relative)) {
      threshold_state(round(sug$relative, 4))
    }
    if (is.na(shiny::isolate(threshold_abs_state())) &&
        !is.na(sug$absolute1000)) {
      # Quantised to a whole number of events in the DISPLAYED unit: the box
      # the reviewer reads it out of offers integers only.
      threshold_abs_state(step3_quantise_per1000(
        sug$absolute1000, shiny::isolate(display_per_state())))
    }
    invisible(NULL)
  }

  # ----- Voiding this outcome's Step 3 answers ------------------------------
  # Called by app.R's begin_new_outcome(). The threshold values live in
  # reactiveVals, not in input$, so rebuilding the Step 3 body does not touch
  # them: without this, a threshold entered for one outcome would be re-seeded
  # into the next outcome's field and silently rated against.
  #
  # Both the threshold suggestion and the pooled control-group risk are asked
  # for again here rather than left to the observers that normally seed them.
  # Those observers fire on a change of state$ma, which may already have
  # happened by the time the reset lands - and the reset can also be triggered
  # by a change of direction alone, which does not touch state$ma at all.
  state$step3_reset <- function() {
    threshold_seed_key(NA_character_)
    threshold_state(NA_real_)
    threshold_abs_state(NA_real_)
    threshold_baseline_state(NA_real_)
    threshold_mode_state(THRESHOLD_MODE_DEFAULT)
    # The publication-bias wizard goes back to deriving its own node; a
    # breadcrumb click left over from the previous outcome must not decide
    # which question the next one opens on.
    pubias_reopen(NULL)
    # Back to the app convention, not to NA: 0.20 is what .responder_block()
    # offers a fresh outcome, and it is labelled an unconfirmed assumption
    # until the reviewer accepts or replaces it.
    responder_p0_state(RESPONDER_P0_DEFAULT)
    .seed_thresholds()
    cr <- shiny::isolate(control_risk())
    if (is.finite(cr$value)) {
      threshold_baseline_state(step3_quantise_per1000(
        1000 * cr$value, shiny::isolate(display_per_state())))
    }
  }

  # Which outcome was each per-outcome input last answered for? Shiny keeps
  # the last value of an input whose widget has been torn down, so between
  # "the outcome changed" (app.R bumps state$outcome_gen) and "Step 3 was
  # rendered again" every input below still reports the PREVIOUS outcome's
  # answer. That is the whole bug: Step 4's export gate reads
  # state$domain_confirmed, which is computed from these inputs, so an outcome
  # nobody had looked at exported as reviewed.
  #
  # state$outcome_gen is read under isolate() on purpose. A reactive read would
  # make every one of these observers re-fire when the generation is bumped,
  # re-stamping the stale answers as current and defeating the guard.
  #
  # The same observers also remember the answer itself, so it can be put back
  # when app.R rebuilds Step 3 from step3_ui(). A plain environment, not
  # reactiveValues: only the restore observer below reads it, and it reads
  # under isolate(), so making these writes reactive would buy nothing and
  # would invalidate that observer on every keystroke.
  .answer_gen <- shiny::reactiveValues()
  .answer_val <- new.env(parent = emptyenv())
  for (.outcome_input_id in pma_outcome_input_ids()) {
    local({
      id <- .outcome_input_id
      shiny::observeEvent(input[[id]], {
        .answer_gen[[id]] <- shiny::isolate(state$outcome_gen)
        assign(id, input[[id]], envir = .answer_val)
      }, ignoreInit = FALSE, ignoreNULL = FALSE)
    })
  }
  # Leaving Step 3 and coming back destroys every widget on it and builds it
  # again from its declared defaults, discarding whatever the reviewer had
  # entered - see the note at pma_restorable_input_ids(). Put the answers back.
  #
  # Keyed on state$step so it fires on the rebuild and on nothing else. The
  # ordering works out: app.R creates output$step_body (app.R:176) before it
  # calls step3_server() (app.R:189), so the rebuilt HTML is already in this
  # flush's payload, and shiny.js dispatches "values" before "inputMessages" -
  # the restored values land on the new widgets, not on the ones they replaced.
  #
  # sendInputMessage() rather than a typed update*Input(): the ids span
  # numeric, text, textarea, select, radio and checkbox widgets, and every one
  # of those bindings reads `value` off the message. The value being sent came
  # from that same input, so it is already the right type.
  # Counts entries into Step 3. output$threshold_panel depends on THIS and not
  # on state$step directly: a bare state$step read also fires on the way OUT,
  # and re-rendering the panel while Step 3 is being torn down rebuilt its
  # widgets empty inside the still-present div, which reported those blanks to
  # the server and overwrote the very answers this restore is meant to keep.
  # Measured: the control-group rationale survived every rebuild except the one
  # its own panel caused.
  step3_entries <- shiny::reactiveVal(0L)
  shiny::observeEvent(state$step, {
    if (!identical(as.integer(state$step %||% 0L), 3L)) return()
    step3_entries(shiny::isolate(step3_entries()) + 1L)
  }, ignoreInit = FALSE)

  shiny::observeEvent(state$step, {
    if (!identical(as.integer(state$step %||% 0L), 3L)) return()
    gen <- shiny::isolate(state$outcome_gen)
    for (id in pma_restorable_input_ids()) {
      if (!exists(id, envir = .answer_val, inherits = FALSE)) next
      v <- get(id, envir = .answer_val, inherits = FALSE)
      if (!pma_restorable_value(v, shiny::isolate(.answer_gen[[id]]), gen)) next
      session$sendInputMessage(id, list(value = v))
    }
  }, ignoreInit = TRUE)
  # An answer counts only if it was given for the outcome now open. Failing
  # closed is the safe direction: a wrongly-stale answer locks the gate, it
  # never opens it.
  .fresh <- function(id) identical(.answer_gen[[id]], state$outcome_gen)

  shiny::observeEvent(state$ma, { .seed_thresholds() }, ignoreNULL = TRUE)
  # Pooled control-group risk. One computation, cached in a reactive, feeding
  # BOTH the Configuration threshold baseline and the Imprecision OIS p0, so
  # the two can no longer disagree. Previously each site recomputed a crude
  # sum(event.c) / sum(n.c) of its own while the on-screen copy claimed the
  # number was pooled.
  control_risk <- shiny::reactive({
    step3_control_risk(state$ma)
  })

  shiny::observe({
    cr <- control_risk()
    if (is.na(threshold_baseline_state()) && is.finite(cr$value)) {
      threshold_baseline_state(
        step3_quantise_per1000(1000 * cr$value, display_per_state()))
    }
  })

  # The auto (pooled) value in events per 1,000, or NA. Used to decide
  # whether the reviewer has overridden it - and therefore owes a rationale.
  #
  # Quantised with the same function that quantises the state, or the
  # comparison in baseline_overridden() below would report every fresh
  # analysis as overridden the moment the pooled proportion is not already a
  # whole number of events, and demand a rationale for a value nobody touched.
  control_risk_auto1000 <- shiny::reactive({
    cr <- control_risk()
    if (is.finite(cr$value)) {
      step3_quantise_per1000(1000 * cr$value, display_per_state())
    } else {
      NA_real_
    }
  })

  baseline_overridden <- shiny::reactive({
    auto <- control_risk_auto1000()
    cur  <- threshold_baseline_state()
    if (!is.finite(auto)) return(FALSE)
    if (!is.finite(cur)) return(TRUE)
    !isTRUE(all.equal(auto, cur, tolerance = 1e-8))
  })

  baseline_rationale_ok <- shiny::reactive({
    if (!baseline_overridden()) return(TRUE)
    nzchar(trimws(input$threshold_baseline_rationale %||% ""))
  })

  # Mirror Configuration-tab inputs into the reactiveVals.
  shiny::observeEvent(input$threshold_mode, {
    if (nzchar(input$threshold_mode %||% "")) {
      threshold_mode_state(input$threshold_mode)
    }
  }, ignoreInit = TRUE)
  # The per-N display unit. The reactiveVal is the source of truth for the
  # same reason the thresholds are (see its declaration); the coercion is
  # step3_per_unit(), because a radioButtons value arrives as a character.
  shiny::observeEvent(input$per, {
    display_per_state(step3_per_unit(input$per))
  }, ignoreInit = TRUE)
  # An emptied threshold field now clears the state rather than silently
  # leaving the previous value in force: the reviewer sees an empty box, so
  # the app must not keep rating against a number they removed. NULL (the
  # widget does not exist, e.g. on the continuous branch) is left alone.
  #
  # The two absolute-scale boxes carry the DISPLAYED unit; the reactiveVals
  # behind them stay per-1,000 (see display_per_state). step3_from_per()
  # converts on the way in, step3_to_per() on the way out, and
  # step3_quantise_per1000() keeps the stored value on the integer grid the
  # box offers so a round trip cannot drift.
  shiny::observeEvent(input$threshold_abs, {
    v <- input$threshold_abs
    if (is.null(v) || length(v) != 1) return()
    per <- display_per_state()
    threshold_abs_state(
      if (is.na(v)) NA_real_
      else step3_quantise_per1000(step3_from_per(v, per), per))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(input$threshold_baseline_input, {
    v <- input$threshold_baseline_input
    if (!is.null(v) && length(v) == 1 && !is.na(v)) {
      per <- display_per_state()
      threshold_baseline_state(
        step3_quantise_per1000(step3_from_per(v, per), per))
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$threshold_ratio, {
    v <- input$threshold_ratio
    if (is.null(v) || length(v) != 1) return()
    threshold_state(if (is.na(v)) NA_real_ else v)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(input$threshold_cont, {
    v <- input$threshold_cont
    if (is.null(v) || length(v) != 1) return()
    threshold_state(if (is.na(v)) NA_real_ else v)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ... and back the other way, so the box cannot show one number while the
  # app rates against another. The reactiveVals are the single source of
  # truth, but output$threshold_panel only reads them under isolate() (a
  # reactive read would rebuild the panel on every keystroke and destroy the
  # widget being typed into), so a state change that lands AFTER the panel has
  # rendered never reaches the DOM on its own. That is exactly what app.R's
  # provenance guard does when the outcome changes. See the long note at
  # step3_widget_sync_value(), which decides whether a push is warranted.
  #
  # Keyed on the reactiveVal, never on the input: an observer that also
  # depended on the input would fight the reviewer as they type, and would
  # refill a box they had just emptied.
  .sync_widget <- function(id, value) {
    v <- step3_widget_sync_value(value, shiny::isolate(input[[id]]))
    if (is.null(v)) return(invisible(NULL))
    shiny::updateNumericInput(session, id, value = v)
    invisible(NULL)
  }
  # The two absolute-scale boxes also have to be re-pushed when the DISPLAY
  # UNIT changes, not only when their state does - switching to per 100 must
  # turn 156 into 16, and the state behind it does not move. One plain
  # observe() over both reactiveVals plus display_per_state() does that; it is
  # still keyed only on state, never on the inputs.
  shiny::observe({
    per <- display_per_state()
    .sync_widget("threshold_baseline_input",
                 step3_to_per(threshold_baseline_state(), per))
    .sync_widget("threshold_abs", step3_to_per(threshold_abs_state(), per))
  })
  # threshold_state() backs two widgets - the relative box on the binary
  # branch and the single box on the continuous one - and only ever one of
  # them is on screen. An input message addressed to a widget that is not in
  # the DOM is dropped by the client, so both are addressed unconditionally
  # rather than re-deriving which branch output$threshold_panel took.
  shiny::observeEvent(threshold_state(), {
    .sync_widget("threshold_ratio", threshold_state())
    .sync_widget("threshold_cont",  threshold_state())
  }, ignoreNULL = FALSE)

  # Responder-conversion state (continuous outcomes). The app-convention
  # starting value is RESPONDER_P0_DEFAULT, at file scope in
  # R/step3_threshold.R beside .responder_block(), which is the widget that
  # offers it.
  #
  # Held in a reactiveVal for the same reason as the thresholds above. It used
  # to be read straight off input$baseline_risk_chinn with the widget rendered
  # from the constant, so every rebuild of output$threshold_panel reset a
  # replaced proportion to 0.20 - including rebuilds WITHIN one outcome, where
  # nothing had changed that should discard the reviewer's number. That failed
  # closed (the freshly built confirmation box is unticked, so Next stayed
  # gated) but it silently threw away an answer they had justified in writing.
  # Scoped to the outcome: state$step3_reset() puts it back to the convention.
  responder_p0_state <- shiny::reactiveVal(RESPONDER_P0_DEFAULT)
  # NA is mirrored through rather than ignored, matching threshold_abs: an
  # emptied box must not leave the app converting against a number the
  # reviewer has removed. responder_p0_valid() then closes the Next gate.
  shiny::observeEvent(input$baseline_risk_chinn, {
    v <- input$baseline_risk_chinn
    if (is.null(v) || length(v) != 1) return()
    responder_p0_state(if (is.na(v)) NA_real_ else v)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(responder_p0_state(), {
    .sync_widget("baseline_risk_chinn", responder_p0_state())
  }, ignoreNULL = FALSE)
  responder_p0 <- shiny::reactive({
    v <- responder_p0_state()
    if (is.null(v) || length(v) != 1L || is.na(v)) return(NA_real_)
    v
  })
  responder_p0_valid <- shiny::reactive({
    v <- responder_p0()
    is.finite(v) && v > 0 && v < 1
  })
  responder_p0_overridden <- shiny::reactive({
    v <- responder_p0()
    if (!is.finite(v)) return(TRUE)
    !isTRUE(all.equal(v, RESPONDER_P0_DEFAULT, tolerance = 1e-8))
  })
  # Confirmed = the reviewer either ticked the confirmation box, or replaced
  # the app-convention default and said why. An unconfirmed default is an
  # assumption nobody has looked at, so it gates this tab's Next.
  responder_p0_confirmed <- shiny::reactive({
    if (!responder_p0_valid()) return(FALSE)
    if (responder_p0_overridden()) {
      return(nzchar(trimws(input$responder_p0_rationale %||% "")))
    }
    isTRUE(input$responder_p0_confirm)
  })

  # The one definition of "the reviewer asked for the responder presentation".
  # Everything downstream - the Next gate, sof_convert_args(), the state mirror
  # Step 4 exports from - reads this rather than input$sof_presentation, so the
  # radio's encoding lives in exactly one place. NULL (before the radio reports
  # in, and on every outcome whose sm is not SMD/MD) means the effect itself,
  # which is also the widget's default.
  responder_mode <- shiny::reactive({
    identical(input$sof_presentation, "responder")
  })

  # chinn_invert is DERIVED from the Step 2 direction answer rather than
  # asked again. Chinn's formula gives OR = exp(SMD * pi / sqrt(3)), so a
  # symptom scale where smaller is better produces a negative SMD for an
  # effective treatment and an OR below 1 - the wrong side. Flipping the sign
  # puts the intervention above 1. Verified empirically on the bundled
  # CBT-I data (pooled SMD -0.49, control 200 per 1,000): invert = TRUE
  # gives 377 per 1,000 for the intervention, invert = FALSE gives 94.
  chinn_invert_derived <- shiny::reactive({
    identical(state$small_values, "desirable")
  })

  # suggest_threshold() carries a $source telling the reader whether the
  # prefilled number comes from Core GRADE 6 itself or is only a pmatools
  # convention. Never present a package convention as a Core GRADE number.
  threshold_suggestion <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    tryCatch(suggest_threshold(obj), error = function(e) NULL)
  })

  # ----- Configuration tab: control-group risk block (binary) ------------
  # First on the tab. The absolute threshold is only interpretable against a
  # control-group risk, and the same number is the Optimal Information Size
  # baseline in Imprecision, so it is settled before anything else.
  .control_risk_block <- function(per = STEP3_PER_DEFAULT) {
    cr   <- control_risk()
    auto <- control_risk_auto1000()
    # The box now holds the DISPLAYED unit, so the conditionalPanel that asks
    # for a rationale has to compare against the displayed auto value; against
    # the per-1,000 one it would fire on every fresh analysis at per = 100.
    auto_shown <- step3_to_per(auto, per)
    cond <- if (is.finite(auto_shown)) {
      sprintf("input.threshold_baseline_input != %s",
              format(auto_shown, scientific = FALSE))
    } else {
      "true"
    }
    provenance <- if (identical(cr$method, "metaprop")) {
      htmltools::tagList(
        .ok_badge("pooled (random-effects metaprop)"),
        # The badge names the method; what the sentence has to carry is the
        # NUMBERS a reviewer weighs before replacing the value - how many
        # studies it rests on, and how far the crude ratio sits from it.
        .config_note(sprintf(
          "%s pooled over %d stud%s%s; crude ratio %s.",
          step3_per_label(auto, per), cr$k_used,
          if (cr$k_used == 1L) "y" else "ies",
          if (cr$k_dropped > 0L) {
            sprintf(", %d excluded for no control-arm count", cr$k_dropped)
          } else "",
          step3_per_label(1000 * cr$crude, per, digits = 1)))
      )
    } else if (identical(cr$method, "simple_fallback")) {
      htmltools::tagList(
        .warn_badge("not pooled: crude event rate"),
        .config_note(sprintf(paste0(
          "metaprop did not converge, so this is the crude ratio %s over %d ",
          "stud%s. Replace it if you have a pooled estimate."),
          step3_per_label(1000 * cr$crude, per, digits = 1), cr$k_used,
          if (cr$k_used == 1L) "y" else "ies"))
      )
    } else {
      .warn_badge("no control-arm data")
    }

    .config_section(
      htmltools::tagList("Control-group risk", provenance),
      shiny::numericInput("threshold_baseline_input",
        sprintf("Control-group risk (events %s patients)",
                step3_per_unit_label(per)),
        # Fall back to the pooled value directly: this render and the
        # observer that seeds threshold_baseline_state() both hang off
        # control_risk(), and their order is not guaranteed.
        value = {
          v <- shiny::isolate(threshold_baseline_state())
          step3_to_per(if (is.finite(v)) v else auto, per)
        },
        # step = 1 and a whole-number grid: an event rate is a count of
        # patients, and "15.6 per 100" is not one.
        min = 0, max = step3_per_unit(per), step = 1),
      .config_note(
        "Converts the absolute threshold to the analysis scale, and seeds ",
        "the Optimal Information Size. Replace it if you have a better ",
        "estimate."),
      # Same pattern as the domain-tab overrides: replacing an automated
      # value requires a written justification (Core GRADE transparency).
      shiny::conditionalPanel(
        cond,
        shiny::textAreaInput("threshold_baseline_rationale",
          "Rationale (required when the pooled value is replaced)",
          rows = 2, width = "100%",
          placeholder = paste0(
            "e.g., rounded to 175 per 1,000; taken from the untreated arm ",
            "of the Smith 2021 cohort; adjusted downwards for a primary ",
            "care population with milder disease."))
      )
    )
  }

  # ----- Configuration tab: the per-N display unit ------------------------
  # Its own uiOutput, seeded under isolate() from display_per_state() and
  # re-rendered on entry into Step 3, for the same reason
  # output$threshold_panel is: a statically declared radio would push its
  # default back to the server on every rebuild of the step body and undo the
  # reviewer's choice.
  output$per_panel <- shiny::renderUI({
    step3_entries()
    .config_section(
      "Presentation of event rates",
      shiny::radioButtons("per", "Report event rates per",
        choices = c("100 patients" = "100", "1,000 patients" = "1000"),
        selected = as.character(shiny::isolate(display_per_state())),
        inline = TRUE),
      .config_note(
        "One setting for the whole app - display only, never what is ",
        "computed. Values are entered as whole events in the unit chosen ",
        "here.")
    )
  })
  shiny::outputOptions(output, "per_panel", suspendWhenHidden = FALSE)

  # Read-only echo on the Final certainty tab, where the input used to live.
  output$per_echo <- shiny::renderUI({
    htmltools::p(class = "pma-card-subtitle",
      "Event rates reported ",
      htmltools::tags$strong(step3_per_unit_label(display_per_state())),
      " patients - set on the Configuration tab.")
  })
  shiny::outputOptions(output, "per_echo", suspendWhenHidden = FALSE)

  output$responder_p0_badge <- shiny::renderUI({
    # The badge reports on the responder proportion, which only exists on the
    # responder route. On the effect route there is no assumption to confirm,
    # and an "unconfirmed assumption" warning beside a section the reviewer
    # has declined would be pure noise.
    if (!responder_mode()) return(NULL)
    if (responder_p0_confirmed()) {
      .ok_badge("confirmed")
    } else {
      .warn_badge("unconfirmed assumption")
    }
  })
  shiny::outputOptions(output, "responder_p0_badge",
                       suspendWhenHidden = FALSE)

  # ----- Configuration tab: centralized input panel -----------------
  output$threshold_panel <- shiny::renderUI({
    # Re-render when the outcome changes, so the seeds below are read AFTER
    # app.R's provenance guard has reset them rather than before. The guard
    # bumps outcome_gen and calls state$step3_reset() in one observer, which
    # runs later in the flush than this output (it is created later, at
    # app.R:252 vs step3_server() at app.R:189) - so on a change of state$ma
    # alone this panel rebuilds from the PREVIOUS outcome's thresholds. Taking
    # a dependency on the generation forces a second render once the reset has
    # landed.
    #
    # This is the load-bearing half of the fix. The updateNumericInput()
    # observers above cannot cover this case on their own: the reviewer is
    # normally still standing in Step 2 when the guard fires, the Step 3 body
    # is not in the DOM, and an input message addressed to a widget that does
    # not exist is dropped by the client with nothing to re-send it later.
    #
    # outcome_gen is the right trigger precisely because it changes only when
    # the outcome does. Depending on the threshold reactiveVals instead would
    # rebuild the panel on every keystroke and destroy the widget being typed
    # into, which is why they are read under isolate() below.
    state$outcome_gen
    # And on a rebuild of Step 3, for a different reason: this panel is a
    # uiOutput nested inside step3_ui(), so when app.R rebuilds the step body
    # the client re-inserts the last HTML this output sent - which was rendered
    # before the reviewer touched anything, and so re-asserts the seeds it
    # carried then. Re-rendering on entry seeds the boxes from the reactiveVals
    # as they stand now. Entries only - see step3_entries() above for why the
    # exit must not trigger this - and navigation never coincides with a
    # keystroke, so this does not destroy a widget being typed into.
    step3_entries()
    # And on a change of the per-N display unit, which relabels the two
    # absolute-scale boxes and rescales their contents. This one IS a reactive
    # read: a unit switch is a deliberate click on the same tab, never
    # something that happens while a box is being typed into, and the labels
    # would otherwise go on saying "per 1,000" beside a value in hundreds.
    per <- display_per_state()
    obj <- state$ma
    if (is.null(obj)) {
      return(htmltools::p("Run the analysis in Step 2 first."))
    }
    sm <- obj$sm %||% "OR"
    # Gate on the OUTCOME, not on the summary measure: a binary outcome
    # analysed as ARD, RD or HR used to fall through to the continuous
    # branch and lose the absolute-scale interface entirely.
    is_binary <- step3_is_binary_outcome(obj, input$outcome_type)
    sug_obj <- threshold_suggestion()
    src <- sug_obj$source
    # Same ordering caveat as the control-group risk above: fall back to the
    # suggestion when the seeding observer has not run yet.
    sug <- step3_threshold_suggestions(sug_obj)
    # In the DISPLAYED unit, and on the whole-number grid the box offers.
    .abs_value <- function() {
      v <- shiny::isolate(threshold_abs_state())
      step3_to_per(step3_quantise_per1000(
        if (is.finite(v)) v else sug$absolute1000, per), per)
    }
    .rel_value <- function() {
      v <- shiny::isolate(threshold_state())
      if (is.finite(v)) v else round(sug$relative, 4)
    }

    if (is_binary) {
      htmltools::tagList(
        .control_risk_block(per),
        shiny::uiOutput("direction_echo"),
        .config_section(
          htmltools::tagList("Decision threshold", .source_badge(src)),
          shiny::radioButtons("threshold_mode", "Threshold scale",
            choices = stats::setNames(
              c("absolute", "relative"),
              c(sprintf("Absolute (%s patients) - recommended",
                        step3_per_unit_label(per)),
                "Relative (ratio)")),
            selected = shiny::isolate(threshold_mode_state())),
          shiny::conditionalPanel(
            "input.threshold_mode == 'absolute'",
            .config_note(
              "The smallest difference in events ", step3_per_unit_label(per),
              " patients that would change a decision. Converted to the ",
              sm, " scale at the control-group risk above."),
            shiny::numericInput("threshold_abs",
              sprintf("Threshold (events %s patients)",
                      step3_per_unit_label(per)),
              value = .abs_value(), min = 0,
              max = step3_per_unit(per), step = 1),
            shiny::uiOutput("threshold_equiv")
          ),
          shiny::conditionalPanel(
            "input.threshold_mode == 'relative'",
            shiny::numericInput("threshold_ratio",
              EDU_COPY$threshold_labels[[sm]] %||%
                "Threshold for clinical importance",
              value = .rel_value(), min = 0, step = 0.01),
            .config_note(EDU_COPY$threshold_help[[sm]] %||% "")
          )
        )
      )
    } else {
      htmltools::tagList(
        # The threshold comes FIRST on the continuous branch, ahead of the
        # presentation choice: it is the number the certainty rating turns on,
        # while the presentation only changes how the SoF displays the effect.
        # The old order put the responder conversion at the top of the tab,
        # which read as though converting were a step on the way to a rating.
        .config_section(
          htmltools::tagList("Decision threshold", .source_badge(src)),
          shiny::numericInput("threshold_cont",
            EDU_COPY$threshold_labels[[sm]] %||%
              "Threshold for clinical importance",
            value = .rel_value(), min = 0, step = 0.01),
          .config_note(EDU_COPY$threshold_help[[sm]] %||% ""),
          .config_note(
            "The certainty rating reads this threshold whichever presentation ",
            "is chosen below: Imprecision compares the confidence interval ",
            "with it on the ", sm, " scale itself. ",
            if (identical(sm, "SMD")) {
              paste0("The 0.20 prefilled above is Core GRADE 6's own ",
                     "threshold for a small and important effect. ")
            } else "",
            "The responder conversion below changes only how the Summary of ",
            "Findings table presents the effect - it never reaches the ",
            "rating."),
          if (identical(sm, "SMD")) {
            htmltools::p(
              class = "pma-card-subtitle", style = "font-style: italic;",
              paste0(
                "Core GRADE 6 calls 0.2 a small important effect, then warns ",
                "that scepticism is appropriate: standardized mean ",
                "differences vary widely with how they are calculated."))
          } else {
            .config_note(
              "Prefilled at ",
              if (identical(sm, "MD")) "0.20 x the pooled SD" else "1.10",
              ". Replace it with a published threshold for this instrument ",
              "whenever one exists.")
          }
        ),
        shiny::uiOutput("direction_echo"),
        # isolate() for the same reason as the threshold seeds: a reactive read
        # would rebuild the panel on every keystroke in this very box.
        .responder_block(sm, shiny::isolate(responder_p0_state()))
      )
    }
  })
  shiny::outputOptions(output, "threshold_panel", suspendWhenHidden = FALSE)

  # The direction the pooled effect lies in, and the conversion the app
  # therefore uses. Read from state$ma, the Step 2 all-studies analysis: it
  # is the only analysis available before grade_meta() runs, and it is the
  # one Risk of Bias is assessed on. When Core GRADE 4 sends grade_meta()
  # off to refit on the low-risk subset and that refit flips the sign,
  # grade_obj() re-runs with the corrected direction (see there).
  .threshold_direction <- shiny::reactive(
    step3_threshold_direction(step3_pooled_te(state$ma)))

  output$threshold_equiv <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    sm <- obj$sm %||% "OR"
    per <- display_per_state()
    # The reactiveVals, not input$: the boxes now carry the DISPLAYED unit
    # while step3_ard_equivalence() works per-1,000, and the mirror observers
    # above keep the states current on every keystroke anyway. Reading both
    # would mix two scales in one call.
    ta <- threshold_abs_state()
    tb <- threshold_baseline_state()
    eq <- step3_ard_equivalence(sm, ta, tb)
    if (is.null(eq)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        sprintf(paste0(
          "Threshold plus control-group risk must be positive and stay below ",
          "%s to convert."),
          format(step3_per_unit(per), big.mark = ","))))
    }
    dir <- step3_directed_threshold(eq, .threshold_direction())
    ln  <- .equiv_lines(eq, dir, per)
    .exact_first <- identical(dir$exact_side %||% "increase", "decrease")
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f5f5f5; ",
        "border-left: 4px solid #0f172a; margin: 0.5rem 0; ",
        "font-size: 0.85rem;"),
      # The exact side is emboldened, so the reader can see at a glance which
      # of the two the judgments are anchored to.
      htmltools::div(if (.exact_first) ln$up else htmltools::strong(ln$up)),
      htmltools::div(if (.exact_first) htmltools::strong(ln$dn) else ln$dn),
      htmltools::div(
        style = "margin-top: 0.35rem;",
        sprintf("On other scales, the increase side is RR %.3f / OR %.3f.",
                eq$rr_up, eq$or_up)),
      htmltools::div(
        style = paste0("margin-top: 0.35rem; font-style: italic; ",
                       "color: hsl(var(--muted-foreground));"),
        ln$alg),
      if (length(ln$approx)) {
        htmltools::div(
          style = paste0("margin-top: 0.35rem; font-style: italic; ",
                         "color: hsl(var(--muted-foreground));"),
          ln$approx)
      }
    )
  })
  shiny::outputOptions(output, "threshold_equiv", suspendWhenHidden = FALSE)

  # Human-readable summary of the active threshold (for read-only blocks).
  # Returns a list so the read-only block can show the same two-direction
  # wording as output$threshold_equiv rather than a third variant.
  threshold_summary <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(head = "No threshold set - run the analysis first.",
                  lines = character()))
    }
    sm <- obj$sm %||% "OR"
    per <- display_per_state()
    if (step3_is_binary_outcome(obj, input$outcome_type) &&
        identical(threshold_mode_state(), "absolute")) {
      ta <- threshold_abs_state()
      tb <- threshold_baseline_state()
      if (!is.finite(ta) || ta <= 0) {
        return(list(head = "Absolute threshold not set yet.",
                    lines = character()))
      }
      eq <- step3_ard_equivalence(sm, ta, tb)
      if (is.null(eq)) {
        return(list(head = sprintf(
          "Absolute threshold: %s (control-group risk missing or invalid)",
          step3_per_label(ta, per)), lines = character()))
      }
      dir <- step3_directed_threshold(eq, .threshold_direction())
      ln  <- .equiv_lines(eq, dir, per)
      return(list(
        head = sprintf(
          "Absolute threshold: %s at a control-group risk of %s",
          step3_per_label(ta, per), step3_per_label(tb, per)),
        lines = c(ln$up, ln$dn, ln$alg),
        approx = ln$approx))
    }
    th <- threshold_state()
    if (!is.finite(th)) {
      return(list(head = "Threshold not set yet.", lines = character()))
    }
    list(head = sprintf("Threshold: %s = %g", sm, th), lines = character())
  })
  threshold_summary_text <- shiny::reactive(threshold_summary()$head)

  # Read-only threshold display inside RoB / Inconsistency / Imprecision.
  #
  # `detail = FALSE` prints the head line alone. The equivalence block below it
  # says what the absolute threshold becomes on the analysis scale in each
  # direction, and which of the two conversions is exact - a derivation, not an
  # answer, on every tab but one. Risk of Bias compares two pooled estimates
  # against the band and Inconsistency reads a zone tally the algorithm has
  # already computed; on neither does a reviewer do anything with the numbers.
  #
  # `detail = TRUE` is Imprecision, where they ARE operative: Core GRADE 2's
  # two-level rule tests the confidence interval against the important-benefit
  # AND important-harm thresholds by eye, so both bounds have to be on screen -
  # and the residual-asymmetry sentence with them, because by construction only
  # one of the two conversions is exact on the absolute scale.
  #
  # The trailing "change it in the Configuration tab" sentence is now the tab's
  # own name, as a link. One id prefix per domain: all seven tab panels live in
  # the DOM at once, so three copies of one actionLink id would collide.
  .render_threshold_readonly <- function(domain = NULL, detail = TRUE) {
    ts <- threshold_summary()
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f9f9f9; ",
        "border: 1px solid #e5e5e5; border-radius: 6px; margin: 0.5rem 0;"),
      htmltools::p(style = "margin: 0; font-size: 0.9rem;",
        htmltools::strong(ts$head)),
      if (isTRUE(detail) && length(ts$lines)) {
        htmltools::div(
          style = paste0("margin: 0.25rem 0 0; font-size: 0.85rem; ",
                         "color: hsl(var(--muted-foreground));"),
          lapply(ts$lines, htmltools::div))
      },
      if (isTRUE(detail) && length(ts$approx %||% character())) {
        htmltools::div(
          style = paste0("margin: 0.35rem 0 0; font-size: 0.85rem; ",
                         "font-style: italic; ",
                         "color: hsl(var(--muted-foreground));"),
          ts$approx)
      },
      htmltools::p(
        class = "pma-card-subtitle",
        style = "margin: 0.25rem 0 0;",
        pma_domain_jump_links("threshold",
                              paste0("threshold_block_jump_", domain, "_"),
                              before = "Set in ", after = "."))
    )
  }
  output$threshold_block_rob   <-
    shiny::renderUI(.render_threshold_readonly("rob",   detail = FALSE))
  output$threshold_block_inco  <-
    shiny::renderUI(.render_threshold_readonly("inco",  detail = FALSE))
  output$threshold_block_impre <-
    shiny::renderUI(.render_threshold_readonly("impre", detail = TRUE))
  shiny::outputOptions(output, "threshold_block_rob",   suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_inco",  suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_impre", suspendWhenHidden = FALSE)

  # The observers behind those links, beside the message they belong to (the
  # same pattern as cert_jump_* below).
  for (.block_domain in c("rob", "inco", "impre")) {
    local({
      jump_id <- paste0("threshold_block_jump_", .block_domain, "_threshold")
      shiny::observeEvent(input[[jump_id]], {
        shiny::updateTabsetPanel(session, "grade_tabs",
                                 selected = PMA_DOMAIN_LABELS[["threshold"]])
      }, ignoreInit = TRUE)
    })
  }

  # grade_meta() threshold arguments derived from the active mode.
  # Gated on the outcome type, matching output$threshold_panel: a binary
  # outcome analysed as something other than OR / RR still has an absolute
  # threshold to convert.
  #
  # For a binary outcome with an absolute threshold the app converts to the
  # ratio scale ITSELF and passes threshold_scale = "ratio", rather than
  # handing pmatools the ARD. threshold_scale = "ard" always converts on the
  # increase side (T = ratio implied by p0 + ard) and every domain then judges
  # against the symmetric band +/- log(T), whose decrease side 1 / T is not
  # the ratio implied by p0 - ard. Converting in the direction the pooled
  # effect actually lies makes the comparison that decides each judgment exact
  # on the absolute scale, which is the scale Core GRADE 7 puts the threshold
  # on. `direction`, `dir` and `note` are carried along so grade_obj() can
  # detect a refit-induced flip and rebuild the provenance note that
  # threshold_scale = "ratio" no longer gets from pmatools.
  #
  # `te_point` overrides the direction source; it defaults to the Step 2
  # all-studies analysis.
  #
  # Falls back to the previous threshold_scale = "ard" call whenever the app
  # cannot do the conversion (control-group risk missing or out of range), so
  # pmatools' own validation and error messages still apply there.
  .threshold_grade_args <- function(obj, te_point = NULL) {
    sm <- obj$sm %||% "OR"
    if (step3_is_binary_outcome(obj, shiny::isolate(input$outcome_type)) &&
        identical(threshold_mode_state(), "absolute")) {
      ta <- threshold_abs_state()
      tb <- threshold_baseline_state()
      if (is.finite(ta) && ta > 0) {
        base <- if (is.finite(tb) && tb > 0 && tb < 1000 &&
                    (tb + ta) < 1000) tb / 1000 else NULL
        direction <- step3_threshold_direction(
          if (is.null(te_point)) step3_pooled_te(obj) else te_point)
        dir <- step3_directed_threshold(step3_ard_equivalence(sm, ta, tb),
                                        direction)
        if (!is.null(dir) && is.finite(dir$ratio) && dir$ratio > 1) {
          return(list(threshold          = dir$ratio,
                      threshold_scale    = "ratio",
                      threshold_baseline = NULL,
                      direction          = direction,
                      dir                = dir,
                      note               = step3_threshold_note(dir)))
        }
        return(list(threshold          = ta / 1000,
                    threshold_scale    = "ard",
                    threshold_baseline = base,
                    direction          = NULL, dir = NULL, note = NULL))
      }
      return(list(threshold = NULL, threshold_scale = "auto",
                  threshold_baseline = NULL,
                  direction = NULL, dir = NULL, note = NULL))
    }
    th <- threshold_state()
    list(
      threshold = if (is.numeric(th) && !is.na(th) && th > 0) th else NULL,
      threshold_scale    = "auto",
      threshold_baseline = NULL,
      direction = NULL, dir = NULL, note = NULL
    )
  }

  # ----- OIS default values -----
  # The OIS baseline is no longer a second, independently computed crude
  # ratio: it IS the Configuration control-group risk, shown read-only, so
  # the two cannot disagree. grade_obj() passes the same number as ois_p0.
  output$ois_p0_ui <- shiny::renderUI({
    tb <- threshold_baseline_state()
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f9f9f9; ",
        "border: 1px solid #e5e5e5; border-radius: 6px; margin: 0.5rem 0;"),
      htmltools::p(style = "margin: 0; font-size: 0.9rem;",
        htmltools::strong(
          if (is.finite(tb)) {
            sprintf("Control-group event rate for the OIS: %.4f (%s)",
                    tb / 1000, step3_per_label(tb, display_per_state()))
          } else {
            "Control-group event rate for the OIS: not set"
          })),
      htmltools::p(class = "pma-card-subtitle", style = "margin: 0.25rem 0 0;",
        "Taken from the Configuration tab, where it is set once. Change it ",
        "there.")
    )
  })
  shiny::outputOptions(output, "ois_p0_ui", suspendWhenHidden = FALSE)

  # Control-group risk as a proportion, for grade_meta(ois_p0 = ).
  ois_p0_value <- shiny::reactive({
    tb <- threshold_baseline_state()
    if (is.finite(tb) && tb > 0 && tb < 1000) tb / 1000 else NULL
  })

  # The modest relative risk reduction the binary OIS is powered to detect.
  # Core GRADE 2 names 20 percent and 25 percent; the radio offers both and
  # 0.20 is pmatools' default.
  ois_rrr_value <- shiny::reactive({
    v <- suppressWarnings(as.numeric(input$ois_rrr %||% "0.20"))
    if (!is.finite(v) || v <= 0 || v >= 1) 0.20 else v
  })

  # The RRR read on the absolute scale, so the OIS target can be compared
  # with the Configuration threshold in the same units. The control-group
  # risk is the one the Configuration tab established; it is not recomputed
  # here.
  output$ois_rrr_equiv <- shiny::renderUI({
    rrr <- ois_rrr_value()
    tb  <- threshold_baseline_state()
    if (!is.finite(tb) || tb <= 0 || tb >= 1000) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        paste0("Set a control-group risk on the Configuration tab to see the ",
               "absolute-scale equivalent of this relative risk reduction.")))
    }
    # Which way the modest RRR moves the rate is the package's decision, not a
    # second one taken here: .ois_target_increase() is vendored with the rest
    # of R/, so the echo and assess_imprecision() cannot disagree.
    dirn <- .ois_target_increase(state$small_values, step3_pooled_te(state$ma))
    up   <- isTRUE(dirn$increase)
    per  <- display_per_state()
    # Round the control-group risk before applying the RRR so the three
    # displayed numbers add up; the calculation itself uses the unrounded
    # rate (ois_p0 = threshold baseline / 1,000).
    p0d <- round(tb)
    p1d <- round(p0d * (if (up) 1 + rrr else 1 - rrr))
    p1d <- min(max(p1d, 0), 1000)
    dif <- abs(p0d - p1d)
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f5f5f5; ",
        "border-left: 4px solid #0f172a; margin: 0.5rem 0; ",
        "font-size: 0.85rem;"),
      htmltools::p(style = "margin: 0;",
        htmltools::strong(sprintf(
          "Relative risk %s %.0f%% = %s -> %s (%s %s)",
          if (up) "increase" else "reduction", 100 * rrr,
          step3_per_label(p0d, per), step3_per_label(p1d, per),
          step3_per_label(dif, per), if (up) "more" else "fewer"))),
      # The parameters, because they are what the OIS is; and the warning that
      # this is not the decision threshold, because the two are both rates on
      # the same scale and a reviewer WILL read one for the other.
      htmltools::p(style = "margin: 0.25rem 0 0;",
        sprintf(paste0("Powered at alpha 0.05 and beta 0.20 from the ",
                       "control-group risk of %s. Not the decision ",
                       "threshold."),
                step3_per_label(tb, per))),
      htmltools::p(style = "margin: 0.25rem 0 0;", dirn$reason, ".")
      # The Core GRADE 2 verbatim quotation that sat here in a <details> is
      # deleted. Its own comment said it: provenance of the two RRR values,
      # not something the reviewer answers. A <details> is for content.
    )
  })
  shiny::outputOptions(output, "ois_rrr_equiv", suspendWhenHidden = FALSE)

  output$ois_sd_ui <- shiny::renderUI({
    obj <- state$ma
    # The OIS formula is 2(z_a+z_b)^2 sigma^2 / delta^2, so sigma and the
    # threshold have to be on the same scale. An SMD threshold is already in
    # within-study SD units, which makes sigma 1; prefilling the raw pooled SD
    # there squared a number that does not belong in the formula at all. MD and
    # RoM stay on the pooled SD, where the threshold is on the raw scale.
    is_smd <- identical(obj$sm %||% "", "SMD")
    val <- if (is.null(obj)) {
      NA
    } else if (is_smd) {
      1
    } else {
      sd_pooled <- tryCatch(compute_pooled_sd(obj),
                            error = function(e) NULL)
      if (!is.null(sd_pooled) && is.finite(sd_pooled) && sd_pooled > 0) {
        round(sd_pooled, 4)
      } else NA
    }
    label <- if (is_smd) {
      paste0("SD for OIS (1: the SMD is already expressed in SD units, so ",
             "the threshold above is standardized)")
    } else {
      "Pooled SD for OIS (auto from data)"
    }
    shiny::numericInput("ois_sd", label, value = val, min = 0, step = 0.1)
  })
  # Inside a conditionalPanel keyed on input$outcome_type, so it is hidden
  # whenever the analysis is binary -- and a suspended output never runs, which
  # left input$ois_sd NULL on the continuous path this widget exists to serve.
  shiny::outputOptions(output, "ois_sd_ui", suspendWhenHidden = FALSE)

  # Which side of the binary low/high split "some concerns" (and, through the
  # "*" default of the rob vector, an unrated study) falls on. Reviewer
  # choice; "high" until the radio group reports in, so the first render
  # matches the documented default rather than the vendored one.
  #
  # The mirror in state is what makes the choice survive a rebuild of the Step
  # 3 body. It is also the fallback here: between a rebuild and the rebuilt
  # radio reporting its value there is a window in which input$ is NULL, and
  # falling back to the bare "high" in that window would rate one pass of the
  # domains against the opposite convention.
  .rob_some_concerns_setting <- function() {
    v <- input$rob_some_concerns
    if (is.null(v) || length(v) != 1L || !v %in% c("low", "high")) {
      v <- state$rob_some_concerns
    }
    if (is.null(v) || length(v) != 1L || !v %in% c("low", "high")) "high" else v
  }

  shiny::observeEvent(input$rob_some_concerns, {
    v <- input$rob_some_concerns
    if (length(v) == 1L && v %in% c("low", "high")) state$rob_some_concerns <- v
  }, ignoreInit = FALSE)

  .study_covariate <- function(labels, col, default = NA_character_) {
    labels <- as.character(labels)
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

  # Non-empty scalar select value or NULL.
  .sel_val <- function(id) {
    v <- input[[id]]
    if (is.null(v) || length(v) == 0 || !nzchar(v)) NULL else v
  }
  # Trimmed non-empty rationale text or NULL.
  .rat_val <- function(id) {
    v <- input[[id]]
    if (is.null(v) || length(v) == 0) return(NULL)
    v <- trimws(as.character(v)[1])
    if (nzchar(v)) v else NULL
  }
  # Resolve an override select + its mandatory rationale. Returns
  # list(value=, rationale=); value is NULL (override ignored, with a
  # notification) while the rationale is missing, so grade_meta() can
  # never see a rationale-less override (v0.4.0 would abort).
  .override_or_ignore <- function(sel_id, rat_id, domain_label) {
    sel <- .sel_val(sel_id)
    if (is.null(sel)) return(list(value = NULL, rationale = NULL))
    rat <- .rat_val(rat_id)
    if (is.null(rat)) {
      shiny::showNotification(
        sprintf(paste0("%s: override ignored - a written rationale is ",
                       "required for manual overrides."), domain_label),
        id = paste0(sel_id, "_rationale_missing"),
        type = "warning", duration = 6)
      return(list(value = NULL, rationale = NULL))
    }
    list(value = sel, rationale = rat)
  }

  # ----- Indirectness subdomains (Core GRADE 5 / pmatools 0.5) ------------
  # The four PICO answers, shaped as the data frame grade_meta() documents
  # for indirectness_subdomains: one row per subdomain, `subdomain` and
  # `judgment` required, `target` / `evidence` optional and display-only.
  # NULL while nothing has been answered, so an unanswered domain falls back
  # to the scalar path rather than asserting four "yes" answers.
  indir_subdomains <- shiny::reactive({
    ans <- vapply(STEP3_INDIR_SUBDOMAINS, function(id) {
      v <- input[[id]]
      if (is.null(v) || length(v) != 1L || !nzchar(v)) NA_character_
      else as.character(v)
    }, character(1))
    keep <- !is.na(ans) & ans %in% STEP3_INDIR_ANSWERS
    if (!any(keep)) return(NULL)
    data.frame(
      subdomain = names(STEP3_INDIR_SUBDOMAINS)[keep],
      judgment  = unname(ans[keep]),
      stringsAsFactors = FALSE
    )
  })

  # Worst case across the answered subdomains. Mirrors the symmetric fold
  # pmatools applies (.indirectness_worst_case), so the app can tell a
  # restatement of the automatic judgment (no rationale needed) from a real
  # override (rationale required) without a second grade_meta() call.
  indir_worst_case <- shiny::reactive({
    sd <- indir_subdomains()
    if (is.null(sd)) return(NULL)
    lv  <- unname(STEP3_INDIR_ANSWER_TO_LEVEL[sd$judgment])
    ord <- c(no = 1L, some_concerns = 2L, serious = 3L)
    names(which.max(ord[lv]))
  })

  grade_obj <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(NULL)

    # The `require_threshold = FALSE` bridge added for pmatools 0.5.1 is
    # gone. It was a temporary opt-out that let grade_meta() rate an outcome
    # with no threshold at all, which is exactly the silent behaviour Core
    # GRADE warns against - three of the five domains would have been judged
    # against nothing. Deleting it alone would crash the tab, because
    # grade_meta() aborts on a NULL threshold under the default
    # threshold_type = "mid", and a reviewer can still clear the field. So
    # the app never makes that call: with no threshold, grade_obj() returns
    # NULL and threshold_missing() drives an explicit on-screen state
    # (output$config_status, the read-only domain blocks and
    # output$final_certainty). No error toast, and no rating computed
    # without a threshold.
    if (is.null(.threshold_grade_args(obj)$threshold)) return(NULL)

    # --- Risk of bias: per-study vector, or scalar override + rationale ---
    rob_arg <- .study_covariate(.study_labels_for_grade(obj), "rob", default = "*")
    rob_rationale <- NULL
    rob_ov <- .override_or_ignore("rob_override", "rob_override_rationale",
                                  "Risk of Bias")
    if (!is.null(rob_ov$value)) {
      rob_arg       <- rob_ov$value
      rob_rationale <- rob_ov$rationale
    }

    # Outcome direction is a required Step 2 answer, mirrored into state.
    sv <- state$small_values

    # --- Inconsistency: scalar override + rationale, or manual flowchart ---
    incon_ov <- .override_or_ignore("incon_override",
                                    "incon_override_rationale",
                                    "Inconsistency")
    # inconsistency_ci_diff and inconsistency_threshold_side are never sent.
    # Both are DERIVED by .auto_inconsistency() - Step 1 from the I-squared
    # surrogate, Step 2 from the zone tally over the study estimates - and
    # supplying either switched the domain onto the manual path, which then
    # aborted unless the other was supplied too. The two widgets are gone
    # from the tab and the zone tally is surfaced as facts instead.
    #
    # Step 3 is the one node no algorithm can reach (subgroup credibility),
    # so it is still asked, and since pmatools 0.5.1 the AUTO path reads it.
    subgroup_expl <- if (!is.null(input$subgroup_explained) &&
                         length(input$subgroup_explained) > 0 &&
                         nzchar(input$subgroup_explained)) input$subgroup_explained else NULL

    # --- Indirectness: the four Core GRADE 5 subdomain answers, plus an
    # optional scalar override of their worst-case fold.
    #
    # `indirectness` MUST be NULL - not "no" - whenever no override is
    # intended: grade_meta() reads any non-NULL scalar alongside a subdomain
    # table as a manual override and demands indirectness_rationale for it.
    # With no subdomain answers the scalar path is unchanged and "no" is the
    # safe default; the confirmation gate (not an error) is what tells the
    # user the domain is still unassessed.
    indir_sub       <- indir_subdomains()
    indir_worst     <- indir_worst_case()
    indir_arg       <- if (is.null(indir_sub)) "no" else NULL
    indir_rationale <- NULL
    indir_sel <- input$indirectness
    if (!is.null(indir_sel) && length(indir_sel) == 1 && nzchar(indir_sel)) {
      auto_level <- indir_worst %||% "no"
      if (identical(indir_sel, auto_level)) {
        # A restatement of the automatic judgment: accepted without a
        # rationale, and it changes nothing.
        indir_arg <- indir_sel
      } else {
        r <- .rat_val("indir_rationale")
        if (is.null(r)) {
          shiny::showNotification(
            paste0("Indirectness: overall rating ignored - a written ",
                   "rationale is required whenever it differs from the ",
                   "automatic judgment."),
            id = "indir_rationale_missing", type = "warning", duration = 6)
        } else {
          indir_arg       <- indir_sel
          indir_rationale <- r
        }
      }
    }

    # --- Imprecision: scalar override + rationale (vendored v0.4.0 API) ---
    impre_ov <- .override_or_ignore("impre_override",
                                    "impre_override_rationale",
                                    "Imprecision")

    # --- Publication bias ---
    # The wizard needs each node to distinguish "not reached yet" from "the
    # reviewer looked and has no opinion", so the two optional widgets carry
    # an explicit deferral VALUE rather than the empty string. Neither value
    # reaches grade_meta(): both mean "let the algorithm decide", which is
    # what NULL means to assess_pubias().
    pubias_si <- if (nzchar(input$pubias_small_industry %||% "")) input$pubias_small_industry else NULL
    pubias_un <- if (nzchar(input$pubias_unpublished %||% "")) input$pubias_unpublished else NULL
    pubias_rc <- if (nzchar(input$pubias_registry_complete %||% "")) input$pubias_registry_complete else NULL
    if (identical(pubias_rc, STEP3_PUBIAS_DEFER)) pubias_rc <- NULL
    # Visual override of Egger's test: v0.4.0 requires pubias_rationale
    # whenever pubias_funnel_asymmetry is supplied. "egger" is the explicit
    # "accept the automated test" answer and is NOT an override, so it must
    # not be routed through .override_or_ignore() - that would demand a
    # rationale for declining to override.
    pubias_fa       <- NULL
    pubias_rationale <- NULL
    if (!identical(input$pubias_funnel_asymmetry %||% "",
                   STEP3_PUBIAS_USE_EGGER)) {
      fa_ov <- .override_or_ignore("pubias_funnel_asymmetry",
                                   "pubias_fa_rationale",
                                   "Publication bias (visual override of Egger)")
      if (!is.null(fa_ov$value)) {
        pubias_fa        <- fa_ov$value
        pubias_rationale <- fa_ov$rationale
      }
    }
    # Final scalar override (app-level; grade_meta has no scalar
    # publication-bias override parameter).
    pubias_ov_res <- .override_or_ignore("pubias_override",
                                         "pubias_override_rationale",
                                         "Publication bias")
    pubias_ov <- pubias_ov_res$value

    th_args <- .threshold_grade_args(obj)

    args <- list(
      meta_obj                 = obj,
      study_design             = "RCT",
      rob                      = rob_arg,
      rob_rationale            = rob_rationale,
      # Where the low/high boundary falls. The app defaults to "high" (only
      # studies explicitly rated low are low), not the vendored default
      # "low"; unrated studies normalise to 'some concerns' (the "*" default
      # of rob_arg above) and so follow the same side. Core GRADE 4 endorses
      # the binary split but leaves the boundary open, so it is a reviewer
      # choice, exposed on the tab.
      rob_some_concerns        = .rob_some_concerns_setting(),
      # rob_inflation_threshold is deliberately NOT passed. The app used to
      # expose it as a slider; it is a pmatools convention rather than a Core
      # GRADE 4 rule, and a reviewer had no basis on which to move it. The
      # package default of 0.10 (R/domain_rob.R) now applies unconditionally,
      # and export_bundle() writes the same 0.10 into the bundled analysis.R.
      small_values             = sv,
      indirectness             = indir_arg,
      indirectness_rationale   = indir_rationale,
      # Core GRADE 5 asks the indirectness question per PICO element; the
      # four answers on the tab are that table. pmatools folds them
      # worst-case, which does NOT reproduce the Table 2 gradient - the tab
      # says so next to the questions.
      indirectness_subdomains  = indir_sub,
      inconsistency            = incon_ov$value,
      inconsistency_rationale  = incon_ov$rationale,
      inconsistency_ci_diff            = NULL,
      inconsistency_threshold_side     = NULL,
      inconsistency_subgroup_explained = subgroup_expl,
      imprecision              = impre_ov$value,
      imprecision_rationale    = impre_ov$rationale,
      threshold          = th_args$threshold,
      threshold_scale    = th_args$threshold_scale,
      threshold_baseline = th_args$threshold_baseline,
      # Derived from the fitted object, not from input$outcome_type. The Step 2
      # radio is rebuilt on every step change and, before state$outcome_type
      # existed, reported "binary" again on every 3 -> 2 -> 3 round trip; a
      # metacont fit was then sent outcome_type = "relative", for which
      # .calc_ois() wants ois_p0 / ois_p1 that a continuous analysis cannot
      # supply, and the OIS silently vanished. step3_is_binary_outcome() reads
      # the class and the arm-level counts first and only falls back to the
      # radio, and it is what .threshold_grade_args() and config_status
      # already use.
      outcome_type = if (step3_is_binary_outcome(obj, state$outcome_type)) {
        "relative"
      } else {
        "absolute"
      },
      # Same control-group risk the Configuration tab shows, not a second
      # crude computation of its own.
      ois_p0       = ois_p0_value(),
      # Core GRADE 2 parameterises the binary OIS by a modest relative risk
      # reduction ("typically 20% or 25%"), not by the threshold. Reviewer
      # choice between the two values the paper names.
      ois_rrr      = ois_rrr_value(),
      ois_sd       = .na_null(input$ois_sd),
      ois_events   = .na_null(input$ois_events_override),
      ois_n        = .na_null(input$ois_n_override),
      pubias_small_industry    = pubias_si,
      pubias_funnel_asymmetry  = pubias_fa,
      pubias_rationale         = pubias_rationale,
      pubias_unpublished       = pubias_un,
      # Q1: only "yes" (denied) is forwarded to the package short-circuit;
      # "no" (suspected) is handled by a post-override below so it forces
      # rate-down regardless of Q2-Q5.
      pubias_registry_complete = if (identical(pubias_rc, "yes")) "yes" else NULL,
      outcome_name = state$outcome_name %||% "Outcome"
    )

    .run_grade <- function(th) {
      args$threshold          <- th$threshold
      args$threshold_scale    <- th$threshold_scale
      args$threshold_baseline <- th$threshold_baseline
      tryCatch(
        suppressWarnings(do.call(grade_meta, args)),
        error = function(e) {
          shiny::showNotification(
            paste("grade_meta error:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    }
    g <- .run_grade(th_args)

    # Core GRADE 4 can send grade_meta() off to refit on the low risk-of-bias
    # subset, and the rating target plus the other four domains are then read
    # off THAT analysis. The directed conversion above used the all-studies
    # pooled effect, because it is the only one available before the call. If
    # the refit put the pooled effect on the other side of the null, the
    # threshold was converted in the wrong direction, so re-run once against
    # the refitted analysis. One correction, not a fixed point: the threshold
    # feeds back into the Risk of Bias analysis-set decision, so iterating
    # could oscillate.
    if (!is.null(g) && !is.null(th_args$direction) && isTRUE(g$rob_refit)) {
      th2 <- .threshold_grade_args(obj, te_point = step3_pooled_te(g$meta))
      if (!identical(th2$direction, th_args$direction) &&
          !is.null(th2$threshold)) {
        g2 <- .run_grade(th2)
        if (!is.null(g2)) {
          th_args <- th2
          g       <- g2
          g$domain_assessments <- step3_append_domain_note(
            g$domain_assessments, "Risk of bias",
            paste0("The absolute threshold was re-converted on the ",
                   th2$dir$exact_side, " side after the refit on the low ",
                   "risk of bias studies moved the pooled effect to the ",
                   "other side of the null."))
        }
      }
    }

    # threshold_scale = "ratio" means pmatools no longer returns
    # $threshold_ard or the $threshold_note it used to append to the three
    # threshold-aware domains and to the Evidence Profile footnote. Rebuild
    # both here, in absolute terms, so nothing loses that provenance.
    if (!is.null(g) && !is.null(th_args$note)) {
      g$threshold_note     <- th_args$note
      g$threshold_ard      <- th_args$dir$ard
      g$threshold_baseline <- th_args$dir$p0
      for (dom in c("Risk of bias", "Inconsistency", "Imprecision")) {
        g$domain_assessments <- step3_append_domain_note(
          g$domain_assessments, dom, th_args$note)
      }
      # .derive_rating_target() writes a scale note off threshold_kind, which
      # is now "ratio", so it says the target was derived on the relative
      # scale and recommends threshold_scale = "ard". Both are wrong here -
      # the threshold IS absolute, converted app-side - so swap the sentence
      # for one that describes what happened. A no-op if the vendored wording
      # ever changes.
      sm_g   <- obj$sm %||% "OR"
      stale  <- paste0(
        " Target derived on the relative-effect scale (", sm_g,
        "); Core GRADE 2 recommends an absolute-effect threshold ",
        "(threshold_scale = 'ard') where a baseline risk is available.")
      fresh <- sprintf(paste0(
        " Target derived from the absolute-effect threshold (%g per 1,000 at ",
        "a baseline risk %g per 1,000), converted to the %s scale on the %s ",
        "side."), 1000 * th_args$dir$ard, 1000 * th_args$dir$p0, sm_g,
        th_args$dir$exact_side)
      if (!is.null(g$rating_target_note)) {
        g$rating_target_note <- sub(stale, fresh, g$rating_target_note,
                                    fixed = TRUE)
      }
      idx <- which(g$domain_assessments$domain == "Imprecision")
      if (length(idx)) {
        g$domain_assessments$notes[idx] <-
          sub(stale, fresh, g$domain_assessments$notes[idx], fixed = TRUE)
      }
    }

    if (!is.null(g)) {
      # Q1 = "no" (reporting bias suspected) forces rate-down 1 regardless
      # of Q2-Q5. Run BEFORE pubias_ov so the manual override below can still
      # win if the user explicitly sets it.
      if (identical(pubias_rc, "no")) {
        idx <- which(g$domain_assessments$domain == "Publication bias")
        if (length(idx)) {
          g$domain_assessments$judgment[idx]  <- "some_concerns"
          g$domain_assessments$auto[idx]      <- FALSE
          g$domain_assessments$downgrade[idx] <- -1L
          g$domain_assessments$notes[idx] <- paste0(
            "Q1: reporting bias suspected based on the overall judgment ",
            "of the listed conditions; rate down 1 (some concerns) regardless ",
            "of Q2-Q5. | ", g$domain_assessments$notes[idx])
        }
      }
      # Final scalar publication-bias override (app-level; recorded in the
      # notes in the same "Manual override (<judgment>): <rationale>"
      # format the vendored make_domain_row() uses).
      if (!is.null(pubias_ov)) {
        idx <- which(g$domain_assessments$domain == "Publication bias")
        if (length(idx)) {
          g$domain_assessments$judgment[idx] <- pubias_ov
          g$domain_assessments$auto[idx]     <- FALSE
          g$domain_assessments$downgrade[idx] <- pmatools_GRADE_DOWNGRADE(pubias_ov)
          g$domain_assessments$notes[idx] <- paste0(
            sprintf("Manual override (%s): %s", pubias_ov,
                    pubias_ov_res$rationale),
            " | ", g$domain_assessments$notes[idx])
        }
      }
      # Additional user-specified downgrade from "Other considerations"
      other_dg <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
      if (is.na(other_dg)) other_dg <- 0L
      other_dg <- min(0L, other_dg)  # cannot rate UP via this control

      total_dg <- sum(g$domain_assessments$downgrade) + other_dg
      score    <- max(1L, 4L + total_dg)
      g$certainty_score    <- score
      g$certainty          <- c("Very Low","Low","Moderate","High")[score]
      g$other_text         <- input$other_text
      g$other_downgrade    <- other_dg

      # The exact grade_meta() call that produced this rating, shaped as the
      # {value, origin, col} specs export_bundle() renders analysis.R from
      # (see pma_grade_arg_specs() in ui_helpers.R). Carried on the object
      # rather than in a separate reactive value so it cannot drift away from
      # the rating it describes; Step 4 reads it back off state$grade.
      #
      # th_args, not args$threshold: the low-risk-of-bias refit above can
      # re-convert the threshold, and the script must reproduce the conversion
      # that was actually rated against.
      args$threshold          <- th_args$threshold
      args$threshold_scale    <- th_args$threshold_scale
      args$threshold_baseline <- th_args$threshold_baseline
      attr(g, PMA_GRADE_ARGS_ATTR) <- pma_grade_arg_specs(args)
    }

    g
  })

  shiny::observe({
    g <- grade_obj()
    if (!is.null(g)) state$grade <- g
  })

  # ----- W4-A: per-domain confirmation state (output gate) ----------------
  # A domain counts as confirmed when its "I have reviewed this domain" box is
  # ticked FOR THIS OUTCOME, and only then; see domain_confirmed() below and
  # pma_domain_confirmations() in R/ui_helpers.R. The .valid_override() /
  # .answered() / .confirmed_na() helpers that used to stand here are gone with
  # the disjunction they served.
  #
  # Every read still goes through .fresh(): an answer given for a PREVIOUS
  # outcome must not confirm this one, however the reviewer got here (see the
  # .answer_gen note above and begin_new_outcome() in app.R).

  # ----- Configuration gate -----------------------------------------------
  # Everything that must be settled before the reviewer starts on the five
  # domains, as a list of human-readable blockers. Empty means ready.
  #
  # Every sub-tab's Next is now gated on that tab having been confirmed, and
  # Configuration's gate is the widest of them: the threshold it sets drives
  # Risk of Bias, Inconsistency and Imprecision, so a tick alone will not do -
  # the values themselves have to be there, and these blockers say which are
  # not. The tab STRIP is never gated: the reviewer can always look ahead.
  threshold_missing <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(TRUE)
    is.null(.threshold_grade_args(obj)$threshold)
  })

  config_blockers <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return("run the meta-analysis in Step 2")
    out <- character()
    if (threshold_missing()) {
      out <- c(out, "enter a decision threshold above zero")
    }
    if (step3_is_binary_outcome(obj, input$outcome_type)) {
      if (!baseline_rationale_ok()) {
        out <- c(out, paste0("give a rationale for replacing the pooled ",
                             "control-group risk"))
      }
    } else if (responder_mode()) {
      if (!responder_p0_valid()) {
        out <- c(out, paste0("enter a control-group responder proportion ",
                             "between 0 and 1"))
      } else if (!responder_p0_confirmed()) {
        out <- c(out, paste0("confirm the 20 percent control-group responder ",
                             "proportion, or replace it and say why"))
      }
    }
    if (!isTRUE(input$threshold_confirm)) {
      out <- c(out, "tick the confirmation box below")
    }
    out
  })

  output$grade_nav_config <- shiny::renderUI({
    .grade_nav("grade_back_thresh", "Back: Meta-analysis",
               "grade_next_thresh", "Next: Risk of Bias",
               next_disabled = length(config_blockers()) > 0)
  })
  shiny::outputOptions(output, "grade_nav_config", suspendWhenHidden = FALSE)

  # Explicit on-screen state for the Configuration tab, including the
  # "no threshold" case that used to be papered over by require_threshold.
  output$config_status <- shiny::renderUI({
    blockers <- config_blockers()
    if (!length(blockers)) {
      return(htmltools::div(
        style = paste0(
          "padding: 0.5rem 0.75rem; margin: 0.5rem 0; ",
          "background: #ecfdf5; border-left: 4px solid #047857; ",
          "border-radius: 4px; font-size: 0.9rem;"),
        htmltools::strong("Configuration complete. "),
        "The five certainty domains are rated against these values."))
    }
    htmltools::div(
      style = paste0(
        "padding: 0.75rem 1rem; margin: 0.5rem 0; ",
        "background: ", PMA_ALERT_BG, "; border-left: 4px solid ",
        PMA_ALERT_FG, "; border-radius: 4px; font-size: 0.9rem;"),
      htmltools::strong("Configuration incomplete. "),
      if (threshold_missing()) {
        paste0("No decision threshold is set, so no certainty rating is ",
               "computed: three of the five domains are judged against it. ")
      } else "",
      sprintf("Still to do: %s.", paste(blockers, collapse = "; "))
    )
  })
  shiny::outputOptions(output, "config_status", suspendWhenHidden = FALSE)

  # What confirms a domain: its own checkbox, ticked for the outcome now open,
  # and nothing else. The rule itself is pma_domain_confirmations() in
  # R/ui_helpers.R - a pure function of these two vectors, so it can be tested
  # without a session, and so this reactive is only the wiring.
  #
  # It used to also count substantive input (a filled RoB table, an answered
  # PICO radio, a valid override). That disjunction is deleted: it told
  # reviewers a domain was unconfirmed while the checkbox they could see was
  # ticked, and it would have opened the export gate by itself once the
  # Indirectness radios ship preselected. See the note at the helper.
  domain_confirmed <- shiny::reactive({
    ids <- unname(PMA_DOMAIN_CONFIRM_INPUTS)
    ticked <- vapply(ids, function(id) isTRUE(input[[id]]), logical(1))
    fresh  <- vapply(ids, .fresh, logical(1))
    names(ticked) <- names(fresh) <- ids
    # Configuration additionally gates on the values it collects being set,
    # which is what config_blockers() lists; the tick alone is not enough
    # there, because three domains are judged against those values.
    pma_domain_confirmations(
      ticked, fresh, config_ready = length(config_blockers()) == 0L)
  })

  # Mirror into state so Step 4 (export gate) can read it.
  shiny::observe({
    state$domain_confirmed <- domain_confirmed()
  })

  confirmed_count <- shiny::reactive({
    length(PMA_DOMAIN_LABELS) -
      length(pma_unconfirmed_domains(domain_confirmed()))
  })

  output$grade_progress_badge <- shiny::renderUI({
    htmltools::span(
      class = "pma-progress-badge",
      sprintf("%d/%d confirmed", confirmed_count(),
              length(PMA_DOMAIN_LABELS)))
  })
  shiny::outputOptions(output, "grade_progress_badge",
                       suspendWhenHidden = FALSE)

  # ----- Progress markers on the tab strip ---------------------------------
  # "Visited" is the one piece of state no input carries: it is the reviewer
  # having opened the tab, which only the tabset reports.
  grade_tab_visited <- shiny::reactiveValues()
  shiny::observeEvent(input$grade_tabs, {
    grade_tab_visited[[input$grade_tabs]] <- TRUE
  }, ignoreNULL = TRUE)
  # A new outcome clears the confirmations (app.R's begin_new_outcome()), so
  # leaving the dots behind would mark tabs as seen for an outcome nobody has
  # opened them for. The tab standing open is still open, so it stays visited.
  shiny::observeEvent(state$outcome_gen, {
    for (tab in names(grade_tab_visited)) grade_tab_visited[[tab]] <- FALSE
    here <- shiny::isolate(input$grade_tabs)
    if (!is.null(here)) grade_tab_visited[[here]] <- TRUE
  }, ignoreInit = TRUE)

  for (.domain_key in names(PMA_DOMAIN_LABELS)) {
    local({
      key <- .domain_key
      tab <- PMA_DOMAIN_LABELS[[key]]
      out_id <- paste0("grade_tab_mark_", key)
      output[[out_id]] <- shiny::renderUI({
        pma_tab_mark(confirmed = isTRUE(domain_confirmed()[[key]]),
                     visited   = isTRUE(grade_tab_visited[[tab]]))
      })
      # The marker lives in the tab STRIP, which is on screen whichever tab is
      # selected - but a suspended output keeps its last painted HTML, and a
      # marker that lies about the state it names is worse than none.
      shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
    })
  }

  # ----- Nav on the five domain tabs ---------------------------------------
  # Each Next waits on that domain's confirmation box. Moving BACKWARDS is
  # never gated, and neither is the tab strip: the gate says "you have not
  # finished here yet", not "you may not leave".
  #
  # suspendWhenHidden = FALSE on every one of them. Six of the seven navs are
  # hidden at any moment, and a suspended output keeps the HTML it last
  # painted - which is exactly a stale gate: the reviewer ticks the box on a
  # tab that is not the selected one (via the restore path, or a new outcome
  # clearing it) and finds a button whose state was decided one outcome ago.
  for (.domain_key in names(STEP3_DOMAIN_NAVS)) {
    local({
      key <- .domain_key
      spec <- STEP3_DOMAIN_NAVS[[key]]
      out_id <- paste0("grade_nav_", key)
      output[[out_id]] <- shiny::renderUI({
        blocked <- !isTRUE(domain_confirmed()[[key]])
        .grade_nav(spec$back_id, spec$back_label,
                   spec$next_id, spec$next_label,
                   next_disabled = blocked,
                   next_title = if (blocked) STEP3_CONFIRM_GATE_TITLE)
      })
      shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
    })
  }

  # Nav on the Final certainty tab. Its Next is the one that leaves Step 3,
  # so it carries the same signal as the Step 4 download gate: enabled only
  # once every domain is confirmed.
  output$grade_nav_final <- shiny::renderUI({
    .grade_nav("grade_back_final", "Back: Publication bias",
               "grade_next_final", "Next: Export",
               next_disabled = length(
                 pma_unconfirmed_domains(domain_confirmed())) > 0)
  })
  shiny::outputOptions(output, "grade_nav_final", suspendWhenHidden = FALSE)

  # Clicking a domain named in the banner below opens that domain's tab. The
  # ids are rebuilt with the banner, so the observers are declared once here,
  # over the fixed set of domain keys, and guard against the 0 a freshly
  # rendered actionLink reports.
  for (.domain_key in names(PMA_DOMAIN_LABELS)) {
    local({
      key <- .domain_key
      link_id <- paste0("cert_jump_", key)
      shiny::observeEvent(input[[link_id]], {
        if (!isTRUE((input[[link_id]] %||% 0L) > 0L)) return()
        shiny::updateTabsetPanel(session, "grade_tabs",
                                 selected = PMA_DOMAIN_LABELS[[key]])
        session$sendCustomMessage("scroll_top", list())
      }, ignoreInit = TRUE)
    })
  }

  # Banner on the Final certainty tab while domains remain unconfirmed.
  output$cert_incomplete_banner <- shiny::renderUI({
    # A withdrawn analysis outranks an unfinished one: with no analysis there
    # is nothing provisional to warn about, only a Step 2 field to go and fill
    # in. Same sentence as the panel below it (step3_blocked_message()).
    blocked <- step3_blocked_message(state$ma_blocked)
    if (is.null(state$ma) && !is.null(blocked)) {
      return(.alert_box("Assessment blocked. ", blocked))
    }
    keys <- pma_unconfirmed_domain_keys(domain_confirmed())
    if (!length(keys)) return(NULL)
    htmltools::div(
      style = paste0(
        "padding: 0.75rem 1rem; margin-bottom: 1rem; ",
        "background: #fef3c7; border-left: 4px solid #b45309; ",
        "border-radius: 4px; font-size: 0.9rem;"),
      htmltools::strong("Assessment incomplete. "),
      # Named, and each name is the way there: the reviewer should not have to
      # find the tab a message just told them about.
      pma_domain_jump_links(
        keys, "cert_jump_",
        before = paste0("The certainty shown below is provisional until every ",
                        "domain has been reviewed. Unconfirmed: "),
        after = paste0(". Open each and tick 'I have reviewed this domain'. ",
                       "Export (Step 4) stays locked until then."))
    )
  })
  shiny::outputOptions(output, "cert_incomplete_banner",
                       suspendWhenHidden = FALSE)

  domain_judgment <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    row <- g$domain_assessments[g$domain_assessments$domain == domain, ]
    if (nrow(row) == 0) return(NULL)
    row$judgment[1]
  }

  domain_notes <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return("")
    row <- g$domain_assessments[g$domain_assessments$domain == domain, ]
    if (nrow(row) == 0) return("")
    row$notes[1]
  }

  output$rob_badge    <- shiny::renderUI(pma_judgment_badge(domain_judgment("Risk of bias")    %||% "no"))
  output$incon_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Inconsistency")   %||% "no"))
  output$indir_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Indirectness")    %||% "no"))
  output$impre_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Imprecision")     %||% "no"))
  output$pubias_badge <- shiny::renderUI(pma_judgment_badge(domain_judgment("Publication bias")%||% "no"))

  output$rob_chip    <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Risk of bias")    %||% "no"))
  output$incon_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Inconsistency")   %||% "no"))
  output$indir_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Indirectness")    %||% "no"))
  output$impre_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Imprecision")     %||% "no"))
  output$pubias_chip <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Publication bias")%||% "no"))

  # ----- Risk-of-bias analysis set (Core GRADE 4 Fig 2) -------------------
  # Display only: these three outputs read the rated object and change no
  # judgment, no number and no rating. They exist because grade_meta() can
  # refit the analysis on the low risk-of-bias subset and only says so via
  # an R-level message() that never reaches the browser.
  output$analysis_set_indicator <- shiny::renderUI(
    pma_analysis_set_indicator(grade_obj()))
  shiny::outputOptions(output, "analysis_set_indicator",
                       suspendWhenHidden = FALSE)

  output$analysis_set_banner_rob <- shiny::renderUI(
    pma_analysis_set_banner(grade_obj()))
  output$analysis_set_banner_cert <- shiny::renderUI(
    pma_analysis_set_banner(grade_obj()))

  # output$rob_how_body was deleted here, with pma_how_collapse() and the five
  # EDU_COPY `how` bodies. It was live only because it interpolated
  # input$rob_inf_threshold, and that slider is gone too - producer and
  # consumer in one change. The flowchart under the verdict draws the five
  # rules and lights up the one that fired.

  # ----- One evaluation shape for all five domain tabs --------------------
  # The verdict in Core GRADE's words, the numbers the assessor recorded as a
  # short list, and the flowchart with the branch taken lit up.
  #
  # The verbatim note used to hang below all three in a <details>. It is gone
  # from the screen, and it is NOT lost: domain_notes() still reaches
  # evidence_profile() and the exported .docx unchanged, which is where a
  # verbatim record is read. What the screen owed the reviewer was why this
  # judgment, and the picture answers that better than the prose did.
  #
  # `keys` picks which facts come forward and in what order; NULL takes them
  # as the assessor emitted them.
  domain_downgrade <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    row <- g$domain_assessments[g$domain_assessments$domain == domain, ]
    if (nrow(row) == 0) return(NULL)
    row$downgrade[1]
  }

  domain_fact_table <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    tryCatch(domain_facts(g, domain), error = function(e) NULL)
  }

  # A backend failure must not print R's error text where the judgment badge
  # belongs. Seen live: a domain tab rendered the string "Error: could not
  # find function '.grade_level_wording'" as its verdict, which reads as a
  # rating rather than as a broken build. tryCatch() around the whole body,
  # not around each helper: any of them can be the one that fails, and the
  # reviewer's next move is the same whichever it was.
  .domain_evaluation <- function(domain, keys = NULL) {
    if (is.null(grade_obj())) {
      return(htmltools::tagList(
        htmltools::h5("Evaluation"),
        htmltools::p(class = "pma-card-subtitle", style = "font-style: italic;",
                     "Run the analysis and set a threshold to see this domain's judgment.")
      ))
    }
    tryCatch({
      facts <- domain_fact_table(domain)
      htmltools::tagList(
        htmltools::h5("Evaluation"),
        pma_domain_verdict(domain_judgment(domain) %||% "no",
                           domain_downgrade(domain)),
        pma_facts_list(facts, keys = keys),
        # The picture of the decision, with the branch this analysis took lit
        # up, directly under the verdict it explains. NULL for Indirectness,
        # which has no flowchart to draw (Core GRADE 5 Table 2 is a gradient).
        pma_flowchart_details(domain, facts)
      )
    }, error = function(e) {
      htmltools::tagList(
        htmltools::h5("Evaluation"),
        .alert_box("This domain could not be evaluated. ",
                   "Re-run Step 2, or report this.")
      )
    })
  }

  # suspendWhenHidden = FALSE on all four. A suspended output keeps whatever
  # the browser last painted, so standing on the Final certainty tab while the
  # analysis is withdrawn used to leave the domain tabs showing a full
  # evaluation of an analysis that no longer exists -- next to a Final
  # certainty panel saying there was none.
  output$rob_evaluation    <- shiny::renderUI(
    .domain_evaluation("Risk of bias"))
  # The two questions the deleted ci_diff / threshold_side widgets used to
  # ask are exactly what these facts report, so the reviewer can see what
  # they were answered with.
  output$incon_evaluation  <- shiny::renderUI(
    .domain_evaluation("Inconsistency",
                       keys = c("zone_decision", "zone_counts", "i2", "tau2",
                                "q_pvalue")))
  output$impre_evaluation  <- shiny::renderUI(
    .domain_evaluation("Imprecision"))
  output$pubias_evaluation <- shiny::renderUI(
    .domain_evaluation("Publication bias"))
  for (.id in c("rob_evaluation", "incon_evaluation", "impre_evaluation",
                "pubias_evaluation")) {
    shiny::outputOptions(output, .id, suspendWhenHidden = FALSE)
  }

  # Whether Core GRADE 3's Step 3 question is live: the automated zone tally
  # reached the opposite-sides branch, which is the only place a credible
  # subgroup explanation changes the judgment. Drives the conditionalPanel
  # around input$subgroup_explained.
  output$incon_subgroup_relevant <- shiny::reactive({
    f <- domain_fact_table("Inconsistency")
    if (is.null(f) || !"key" %in% names(f)) return(FALSE)
    dec <- f$value[f$key == "zone_decision"]
    if (!length(dec)) return(FALSE)
    # The opposite-sides branch is the one whose decision note reports
    # substantial mass in BOTH directions (.auto_inconsistency()).
    grepl("Both directions have substantial mass", dec[1], fixed = TRUE)
  })
  shiny::outputOptions(output, "incon_subgroup_relevant",
                       suspendWhenHidden = FALSE)

  # ----- Which Core GRADE 2 Fig 4 branch the analysis took ----------------
  # Read from the STRUCTURED facts the assessor records ("fig4_path" and
  # "ois_used", R/domain_imprecision.R), not by regex over the prose note.
  # The facts exist precisely so a caller can branch on the path without
  # re-parsing sentences, and the old sub("^.*Fig 4 path: ", "", notes) chain
  # would have silently produced the wrong headline the first time the note
  # wording moved.
  output$impre_branch <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "Run the analysis and set a threshold to see which branch applies."))
    }
    f <- domain_fact_table("Imprecision")
    .fact_value <- function(key) {
      if (is.null(f) || !"key" %in% names(f)) return("")
      v <- f$value[f$key == key]
      if (!length(v) || is.na(v[1])) "" else as.character(v[1])
    }
    path <- .fact_value("fig4_path")
    if (!nzchar(path)) {
      return(htmltools::div(
        style = paste0(
          "padding: 0.6rem 0.85rem; background: #f5f5f5; ",
          "border-left: 4px solid #6b7280; margin: 0.5rem 0; ",
          "font-size: 0.85rem;"),
        htmltools::p(style = "margin: 0;",
          htmltools::strong("Figure 4 was not applied."),
          " The imprecision judgment was supplied manually through the ",
          "override below, which bypasses the automated assessment.")))
    }
    crosses  <- grepl("^CI crosses", path)
    ois_used <- identical(.fact_value("ois_used"), "yes")
    head <- if (crosses) {
      "Yes branch - the CI crosses the chosen threshold."
    } else if (ois_used) {
      "No branch, implausibly large effect - the OIS approach was applied."
    } else {
      "No branch, moderate effect - do not rate down."
    }
    detail <- if (crosses) {
      paste0("Sample size is NOT considered on this path: the Optimal ",
             "Information Size is not consulted, and any OIS figures in the ",
             "evaluation below are reported for information only. Rate down ",
             "one level; two only if the CI crosses two thresholds ",
             "(important benefit and important harm), or if the most ",
             "appropriate plain language summary warrants 'may' rather than ",
             "'likely'. The second condition is a reviewer judgment and is ",
             "not assessed automatically - record it through the override ",
             "below.")
    } else if (ois_used) {
      paste0("This is the only route to the OIS: the CI stays clear of the ",
             "threshold AND the effect is implausibly large. The participant ",
             "count therefore did drive the judgment. Figure 4 compares the ",
             "OIS against participants, not events.")
    } else {
      paste0("Figure 4 stops here. A moderate effect whose CI stays clear of ",
             "the threshold does not rate down, and sample size never enters ",
             "the decision: the OIS is reached only when the effect is ",
             "implausibly large.")
    }
    color <- if (crosses) "#c07020" else if (ois_used) "#0f172a" else "#208050"
    htmltools::div(
      style = sprintf(paste0(
        "padding: 0.6rem 0.85rem; background: #f5f5f5; ",
        "border-left: 4px solid %s; margin: 0.5rem 0; font-size: 0.85rem;"),
        color),
      htmltools::p(style = "margin: 0;", htmltools::strong(head)),
      htmltools::p(style = "margin: 0.25rem 0 0;", detail),
      htmltools::p(
        style = paste0("margin: 0.35rem 0 0; font-family: monospace; ",
                       "font-size: 0.78rem; color: #444;"),
        path)
    )
  })
  shiny::outputOptions(output, "impre_branch", suspendWhenHidden = FALSE)

  # ----- Indirectness subdomain table (pmatools indirectness_table) -------
  # Surfaced deliberately: it is the only rendering of exactly what the app
  # sent to grade_meta(), it shows which element drove the worst-case fold,
  # and its footer repeats the Core GRADE 5 Table 2 gradient caveat next to
  # the judgment rather than only inside the collapsed explanation.
  output$indir_subdomain_table <- shiny::renderUI({
    if (is.null(indir_subdomains())) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "No subdomain answers, so the domain rests on the overall rating."))
    }
    g <- grade_obj()
    if (is.null(g) || is.null(g$indirectness_subdomains)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "(Subdomain table not yet available - set a threshold first.)"))
    }
    ft <- tryCatch(indirectness_table(g), error = function(e) NULL)
    if (is.null(ft)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "(Subdomain table could not be rendered.)"))
    }
    htmltools::div(
      style = "margin-top: 0.75rem;",
      tryCatch(flextable::htmltools_value(ft),
               error = function(e) htmltools::p(paste("Render error:",
                                                      conditionMessage(e))))
    )
  })

  # Helper to collect display-panel inputs for a given prefix.
  .display_args <- function(prefix) {
    pick_text <- function(id) {
      v <- input[[id]]
      if (is.null(v) || !nzchar(v)) NULL else v
    }
    lo <- input[[paste0(prefix, "_xlim_lo")]]
    hi <- input[[paste0(prefix, "_xlim_hi")]]
    xlim <- if (!is.null(lo) && !is.null(hi) && !is.na(lo) && !is.na(hi) && lo < hi) {
      c(lo, hi)
    } else NULL
    addrow_ids <- pma_forest_addrow_ids(prefix)
    list(
      title        = pick_text(paste0(prefix, "_title")),
      label_e      = pick_text(paste0(prefix, "_label_e")),
      label_c      = pick_text(paste0(prefix, "_label_c")),
      favors_left  = pick_text(paste0(prefix, "_favors_left")),
      favors_right = pick_text(paste0(prefix, "_favors_right")),
      xlim         = xlim,
      # An absent checkbox means the tab has not been rendered yet; fall back
      # to the UI default (TRUE) rather than to isTRUE(NULL). One checkbox
      # feeds both keys - plot_forest()'s two arguments are kept as they are.
      show_n       = isTRUE(input[[paste0(prefix, "_show_arm_columns")]] %||% TRUE),
      show_events  = isTRUE(input[[paste0(prefix, "_show_arm_columns")]] %||% TRUE),
      addrow_above = pma_addrow_above(input[[addrow_ids[["above"]]]]),
      addrow_below = pma_addrow_below(input[[addrow_ids[["below"]]]])
    )
  }

  # Mirror per-plot display options into state for downstream (export) use
  shiny::observe({
    state$display$forest_rob    <- .display_args("rob")
    state$display$forest_incon  <- .display_args("incon")
    state$display$forest_indir  <- .display_args("indir")
    state$display$forest_pubias <- .display_args("pubias")
    state$display$funnel_pub    <- pma_funnel_display_args(input, "funnel_pub")
    state$display$funnel_trim   <- pma_funnel_display_args(input, "funnel_trim",
                                                           include_egger = FALSE)
  })

  # ----- RoB stratified forest plot (render generous, trim, display) -----
  output$rob_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    rob_vec <- if (!is.null(state$data) && "rob" %in% names(state$data)) {
      .study_covariate(as.character(obj$studlab), "rob", default = "*")
    } else .study_covariate(as.character(obj$studlab), "rob", default = "*")
    da <- .display_args("rob")
    some_as <- .rob_some_concerns_setting()
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (obj$k %||% 0L) + 600,
      plot_fn = function() {
        # Two strata, folded by the package with the SAME internal the
        # assessor uses (.rob_high_levels). The plot used to split studies
        # four ways - low / some / high / unknown - beside a judgment made on
        # two, so with the default boundary it disagreed with the evaluation
        # printed next to it. Not pre-folded here: rob_strata() owns that
        # vocabulary and would warn on labels invented at this call site.
        do.call(plot_forest_rob,
                c(list(meta_obj = obj, rob = rob_vec,
                       some_concerns_as = some_as), da))
      }
    )
  }, deleteFile = TRUE)

  .has_indir_forest_input <- function() {
    vals <- character()
    rt <- state$rob_table
    if (!is.null(rt) && "indirectness" %in% names(rt)) {
      vals <- c(vals, as.character(rt$indirectness))
    }
    d <- state$data
    if (!is.null(d) && "indirectness" %in% names(d)) {
      vals <- c(vals, as.character(d$indirectness))
    }
    vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
    length(vals) > 0
  }

  output$indir_forest_image_block <- shiny::renderUI({
    if (!.has_indir_forest_input()) return(NULL)
    htmltools::tagList(
      htmltools::h5("Forest plot stratified by Indirectness",
                    style = "margin-top: 1rem;"),
      htmltools::div(class = "pma-forest-image",
        shinycssloaders::withSpinner(
          shiny::imageOutput("indir_forest", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px"))
    )
  })

  # ----- Indirectness stratified forest plot (mirror of RoB) -----
  output$indir_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj) || !.has_indir_forest_input()) return(NULL)
    indir_vec <- if (!is.null(state$data) && "indirectness" %in% names(state$data)) {
      .study_covariate(as.character(obj$studlab), "indirectness", default = "low")
    } else .study_covariate(as.character(obj$studlab), "indirectness", default = "low")
    da <- .display_args("indir")
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (obj$k %||% 0L) + 600,
      plot_fn = function() {
        do.call(plot_forest_indirectness,
                c(list(meta_obj = obj, indirectness = indir_vec), da))
      }
    )
  }, deleteFile = TRUE)

  # ----- Inconsistency forest plot (render generous, trim, display) -----
  output$incon_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    da <- .display_args("incon")
    pma_render_trimmed(
      width  = 1400,
      height = 200 + 80 * (obj$k %||% 0L) + 400,
      plot_fn = function() {
        do.call(plot_forest,
                c(list(meta_obj = obj, auto_layout = TRUE), da))
      }
    )
  }, deleteFile = TRUE)

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

  pubias_node <- shiny::reactive({
    step3_pubias_node(
      small_industry    = input$pubias_small_industry,
      registry_complete = input$pubias_registry_complete,
      funnel_asymmetry  = input$pubias_funnel_asymmetry,
      unpublished       = input$pubias_unpublished,
      k                 = pubias_k(),
      reopen            = pubias_reopen()
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
  PUBIAS_NODE_TITLES <- c(
    q1     = "Small and industry-sponsored?",
    extra  = "Overall reporting-bias judgment",
    q3     = "Funnel plot asymmetry",
    q4     = "Unpublished studies documented?",
    result = "Result"
  )

  # One line per answered node, each a link back to it, plus the computed Q2
  # step - which is not a question and never gets a screen of its own.
  output$pubias_breadcrumb <- shiny::renderUI({
    if (is.null(state$ma)) return(NULL)
    node <- pubias_node()
    path <- step3_pubias_reachable(input$pubias_small_industry,
                                   input$pubias_registry_complete,
                                   pubias_k())
    .answer_of <- function(nd) {
      v <- switch(nd,
        q1    = input$pubias_small_industry,
        extra = input$pubias_registry_complete,
        q3    = input$pubias_funnel_asymmetry,
        q4    = input$pubias_unpublished,
        NULL)
      if (is.null(v) || length(v) != 1L || !nzchar(v)) return(NULL)
      switch(as.character(v),
        "yes"   = "Yes",
        "no"    = "No",
        "defer" = "left to the Figure 5 nodes",
        "egger" = "accept the automated Egger test",
        as.character(v))
    }
    # Walked in path order, so the trail reads in the order the algorithm
    # took - including Q2, which is COMPUTED and therefore reported in place
    # rather than asked as a screen of its own.
    crumbs <- lapply(setdiff(path, "result"), function(nd) {
      k_line <- if (nd %in% c("q3", "q4")) {
        htmltools::div(class = "pma-crumb pma-crumb-auto",
                       step3_pubias_k_line(pubias_k()))
      } else NULL
      ans <- .answer_of(nd)
      crumb <- if (is.null(ans) || identical(nd, node)) NULL else {
        htmltools::div(
          class = "pma-crumb",
          htmltools::span(PUBIAS_NODE_TITLES[[nd]]), " ",
          htmltools::strong(ans), " ",
          shiny::actionLink(paste0("pubias_open_", nd), "change")
        )
      }
      if (is.null(k_line) && is.null(crumb)) return(NULL)
      htmltools::tagList(k_line, crumb)
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

    if (identical(node, "q1")) {
      return(htmltools::tagList(
        htmltools::h5("Most or all studies small AND industry-sponsored?"),
        htmltools::p(class = "pma-card-subtitle",
          paste0("A 'yes' rates down 1 on its own and ends the assessment; ",
                 "nothing after it can undo the concern.")),
        shiny::radioButtons("pubias_small_industry", NULL,
          choices = c("No" = "no", "Yes" = "yes"),
          selected = character(0), inline = TRUE)
      ))
    }

    if (identical(node, "extra")) {
      return(htmltools::tagList(
        htmltools::h5("Overall reporting-bias judgment"),
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
        shiny::radioButtons("pubias_registry_complete",
          "Overall, does the situation argue against reporting bias?",
          choices = c(
            "No - reporting bias is plausible (rate down 1)"  = "no",
            "Yes - reporting bias is unlikely (no rate down)" = "yes",
            "Leave it to the Figure 5 nodes"                  = STEP3_PUBIAS_DEFER
          ),
          selected = character(0), inline = FALSE)
      ))
    }

    if (identical(node, "q3")) {
      return(htmltools::tagList(
        htmltools::h5("Does funnel plot asymmetry strongly suggest publication bias?"),
        # The <details> under this used to give the provenance of p < 0.05.
        # Deleted: the flowchart caption on this tab names the implementing
        # function, and the sentence changed no answer.
        htmltools::p(class = "pma-card-subtitle",
          paste0("Egger's test is run on the funnel plot below, at p < 0.05. ",
                 "Accept it, or replace it with your own visual judgment.")),
        shiny::selectInput("pubias_funnel_asymmetry",
          "Your answer",
          choices = c(
            "(choose)"                             = "",
            "Accept the automated Egger test"      = STEP3_PUBIAS_USE_EGGER,
            "Funnel symmetric (visual override)"   = "no",
            "Funnel asymmetric (visual override)"  = "yes")),
        shiny::conditionalPanel(
          sprintf("input.pubias_funnel_asymmetry == 'no' || input.pubias_funnel_asymmetry == 'yes'"),
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
      return(htmltools::tagList(
        htmltools::h5("Documentation of unpublished studies"),
        htmltools::p(class = "pma-card-subtitle",
          sprintf(paste0("Egger's test is unreliable at k = %d, so Figure 5 ",
                         "routes here. Documented unpublished trials rate ",
                         "down 1."), k)),
        shiny::radioButtons("pubias_unpublished",
          "Unpublished studies documented?",
          choices = c("No" = "no", "Yes" = "yes"),
          selected = character(0), inline = TRUE)
      ))
    }

    NULL
  })
  shiny::outputOptions(output, "pubias_wizard", suspendWhenHidden = FALSE)

  # Flags for the two statically-placed blocks. Assigning a reactive to an
  # output is what makes it readable from a conditionalPanel condition;
  # suspendWhenHidden = FALSE because the panel it gates is initially hidden,
  # and a suspended output never evaluates.
  output$pubias_show_funnel <- shiny::reactive({
    !is.null(state$ma) && step3_pubias_statistical(pubias_k()) &&
      identical(pubias_node(), "q3")
  })
  shiny::outputOptions(output, "pubias_show_funnel", suspendWhenHidden = FALSE)

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

  # Egger's auto judgment displayed as colour-coded callout
  output$pubias_egger_result <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj) || .effective_pubias_k(obj) < 10) return(NULL)
    res <- tryCatch(
      suppressWarnings(meta::metabias(obj, method.bias = "linreg")),
      error = function(e) NULL
    )
    if (is.null(res) || is.null(res$p.value) || is.na(res$p.value)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "Egger's test could not be computed."))
    }
    pval <- res$p.value
    # Single tier. pmatools 0.5 removed the p < 0.01 -> "serious" (-2) rule
    # because Core GRADE 4 never rates down two levels for publication bias.
    judgment <- if (pval < 0.05) {
      list(text = sprintf(paste0("p = %.3f < 0.05 - evidence of asymmetry. ",
                                 "Auto judgment: some concerns (rate down 1)."),
                          pval),
           color = "#c07020")
    } else {
      list(text = sprintf(paste0("p = %.3f >= 0.05 - no strong evidence of ",
                                 "asymmetry. Auto judgment: no rate down."),
                          pval),
           color = "#208050")
    }
    htmltools::div(
      style = sprintf(
        "padding: 0.6rem 0.85rem; background: #f5f5f5; border-left: 4px solid %s; margin: 0.5rem 0;",
        judgment$color),
      # The p < 0.05 provenance caveat that used to be repeated here is now
      # stated once, in the Q3 node's own <details> immediately above.
      htmltools::p(style = "margin: 0;",
        htmltools::strong("Egger's regression: "),
        judgment$text)
    )
  })
  shiny::outputOptions(output, "pubias_egger_result",
                       suspendWhenHidden = FALSE)

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
    tf <- tryCatch(suppressWarnings(meta::trimfill(obj)),
                   error = function(e) NULL)
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
    tf <- tryCatch(suppressWarnings(meta::trimfill(obj)),
                   error = function(e) NULL)
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
                if (sign_flips) "  [direction flips]" else ""))
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

  # output$indirectness_banner and the state$indir_reviewed flag behind it were
  # deleted here. The banner said "no indirectness judgment recorded yet", and
  # with the four PICO radios preselected there is always one - it would have
  # been dismissed by its own widgets on mount, every time. What it was really
  # reporting is now the tab's own progress marker and the Final certainty
  # banner, both driven by the confirmation checkbox.

  # The amber "you cannot get a rating until you do X" box, in one place: the
  # Final certainty panel, the SoF preview and the incomplete banner all use
  # it, and an actionable blocked state must not look different depending on
  # which of the three the reviewer is looking at.
  .alert_box <- function(head, ...) {
    htmltools::div(
      style = paste0(
        "padding: 0.75rem 1rem; background: ", PMA_ALERT_BG,
        "; border-left: 4px solid ", PMA_ALERT_FG,
        "; border-radius: 4px; font-size: 0.9rem;"),
      htmltools::strong(head), ...)
  }

  # Why is there no rating? Three answers, and until 0.5.1 two of them printed
  # the third one's text. `NULL` means "nothing has been attempted yet", which
  # is the only case the idle placeholder is honest about.
  #
  #   1. Step 2 could not run           -> amber, names the missing fields
  #   2. no decision threshold          -> amber, unchanged
  #   3. nothing attempted yet          -> the plain idle line
  no_rating_reason <- shiny::reactive({
    blocked <- step3_blocked_message(state$ma_blocked)
    if (is.null(state$ma) && !is.null(blocked)) {
      return(list(kind = "blocked", text = blocked))
    }
    if (!is.null(state$ma) && threshold_missing()) {
      return(list(kind = "threshold", text = paste0(
        "The decision threshold is empty. Risk of Bias, Inconsistency and ",
        "Imprecision are all judged against it, so no rating is computed ",
        "until it is set on the Configuration tab.")))
    }
    list(kind = "idle", text = "Run analysis and configure domains.")
  })

  output$final_certainty <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) {
      why <- no_rating_reason()
      if (identical(why$kind, "idle")) return(htmltools::p(why$text))
      return(.alert_box("No certainty rating. ", why$text))
    }
    other_dg <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
    if (is.na(other_dg)) other_dg <- 0L
    ft <- tryCatch(
      evidence_profile(g,
                       other_text      = input$other_text,
                       other_downgrade = other_dg),
      error = function(e) NULL
    )
    if (is.null(ft)) {
      return(htmltools::p("(Evidence Profile not yet available)"))
    }
    htmltools::tags$div(
      style = "margin-top: 1rem;",
      tryCatch(flextable::htmltools_value(ft),
               error = function(e) htmltools::p(paste("Render error:",
                                                      conditionMessage(e))))
    )
  })
  # Matches cert_incomplete_banner, which sits directly above it on the same
  # tab: the panel has to be computed before the tab is revealed, or the
  # reviewer sees the previous state for a beat.
  shiny::outputOptions(output, "final_certainty", suspendWhenHidden = FALSE)

  # Whether the SoF can safely be rendered through the responder conversion.
  # sof_table() HARD-ABORTS when convert_smd_to_or = TRUE and baseline_risk
  # is absent or outside (0, 1), and when meta_obj$sm is not SMD / MD
  # (R/_pmatools/sof_table.R). All three preconditions are checked here so
  # Step 3 never renders into that abort.
  sof_convert_args <- shiny::reactive({
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    sm <- g$meta$sm %||% ""
    if (!responder_mode()) return(NULL)
    if (!sm %in% c("SMD", "MD")) return(NULL)
    p0 <- responder_p0()
    if (!responder_p0_valid()) return(NULL)
    list(
      convert_smd_to_or = TRUE,
      baseline_risk     = p0,
      threshold_label   = input$threshold_label,
      chinn_invert      = chinn_invert_derived()
    )
  })

  # Presentation fields collected in Step 2 (see step2_ui()), resolved here
  # into the exact values sof_table() wants.
  sof_follow_up <- shiny::reactive(pma_sof_follow_up(state$outcome_follow_up))
  sof_unit <- shiny::reactive({
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    pma_sof_unit(g, state$outcome_unit)
  })

  # Core GRADE 6's rare-event trap: the Difference and "With intervention"
  # columns are both derived by applying the pooled relative effect to a
  # baseline risk, which misleads when events are rare. Computed from the
  # analysis itself, against the risk the table is actually drawn on (the
  # responder proportion when the Chinn conversion is active).
  sof_rare_alert <- shiny::reactive({
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    pma_rare_event_alert(g, baseline_risk = sof_convert_args()$baseline_risk)
  })

  output$sof_preview <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) {
      # Was a bare "...", which told the reviewer nothing at all. Same three
      # answers as the certainty panel above, from the same reactive.
      why <- no_rating_reason()
      if (identical(why$kind, "idle")) return(htmltools::p(why$text))
      return(.alert_box("No Summary of Findings table. ", why$text))
    }
    args <- c(
      list(x          = g,
           # Core GRADE 6 layout for every SoF the app renders or exports;
           # see PMA_SOF_STYLE in ui_helpers.R for why it is not an option.
           style      = PMA_SOF_STYLE,
           palette    = PMA_SOF_PALETTE,
           # The reactiveVal, not input$per: the radio lives on the
           # Configuration tab and is rebuilt with the step body, so the
           # state is the only thing that survives a 3 -> 2 -> 3 round trip.
           per        = display_per_state(),
           prediction = isTRUE(input$prediction),
           follow_up  = sof_follow_up(),
           unit       = sof_unit()),
      sof_convert_args() %||% list()
    )
    ft <- tryCatch(do.call(sof_table, args),
                   error = function(e) NULL)
    if (is.null(ft)) return(htmltools::p("(SoF not yet available)"))
    alert <- sof_rare_alert()
    # All three notes go into the flextable footer as well as onto the page,
    # so they travel into the exported .docx.
    ft <- pma_sof_add_notes(ft, c(alert$note, PMA_SOF_CER_EER_NOTE,
                                  PMA_SOF_LIMITATIONS_NOTE))
    htmltools::tagList(
      pma_rare_event_banner(alert),
      pma_sof_scroller(
        tryCatch(flextable::htmltools_value(ft),
                 error = function(e)
                   htmltools::p(paste("SoF render error:",
                                      conditionMessage(e)))))
    )
  })
  shiny::outputOptions(output, "sof_preview", suspendWhenHidden = FALSE)

  # The responder-conversion settings are owned by Step 3 (Configuration
  # tab), not by app.R's display observer. Step 4's export_bundle() gets the
  # same guarded values the Step 3 preview uses, so it cannot walk into
  # sof_table()'s abort - which is reachable otherwise, because an
  # input$sof_presentation left on "responder" from an earlier SMD run
  # survives a switch to RoM (hiding a radio does not reset it).
  shiny::observe({
    ca <- sof_convert_args()
    state$display$convert       <- !is.null(ca)
    state$display$baseline_risk <- ca$baseline_risk
    state$display$chinn_invert  <- isTRUE(ca$chinn_invert)
    # Resolved presentation fields, so Step 4 builds the exported SoF from
    # exactly the values this preview used.
    state$display$follow_up     <- sof_follow_up()
    state$display$unit          <- sof_unit()
  })

  # Read-only echo of the outcome name: it is owned by Step 2, so Step 3 shows
  # it (the SoF row label depends on it) without offering a second, divergent
  # field to edit.
  output$outcome_name_echo <- shiny::renderUI({
    # state$outcome_name is only ever written on a SUCCESSFUL run and is
    # deliberately never cleared (see the observer in step2_ma.R), so once the
    # reviewer empties the Step 2 field this echo goes on printing the old name
    # next to a Final certainty panel that says there is no analysis. Read the
    # live blocked state instead: when the analysis is held up on an
    # outcome-identity field, say that rather than a value that is no longer in
    # the form.
    identity_missing <- step3_blocked_identity(state$ma_blocked)
    name_missing <- STEP2_IDENTITY_FIELD_LABELS[["outcome_name"]] %in%
                    identity_missing
    nm <- if (name_missing) "(cleared in Step 2)" else
            state$outcome_name %||% "(not set)"
    # Raw value, not sof_follow_up(): that one already carries the
    # "Follow-up: " prefix the table cell needs, and the label supplies it
    # here.
    fu <- state$outcome_follow_up
    if (!is.null(fu) && !nzchar(trimws(fu))) fu <- NULL
    un <- sof_unit()
    htmltools::tagList(
      if (length(identity_missing)) htmltools::p(
        class = "pma-card-subtitle",
        htmltools::tags$strong("No analysis. "),
        paste0("Step 2 is missing: ", paste(identity_missing, collapse = ", "),
               ". Nothing below is being rated until it is filled in.")
      ) else NULL,
      htmltools::p(class = "pma-card-subtitle",
        "Outcome name: ", htmltools::tags$strong(nm),
        " - set in Step 2 (Model configuration)."),
      htmltools::p(class = "pma-card-subtitle",
        "Follow-up: ",
        htmltools::tags$strong(fu %||% "(not set)"),
        " - shown under the outcome name in the Summary of Findings table ",
        "and saved with this outcome. Set in Step 2."),
      if (!is.null(un)) htmltools::p(class = "pma-card-subtitle",
        "Unit of the Difference column: ", htmltools::tags$strong(un),
        ".") else NULL
    )
  })

  # Read-only echo of the outcome direction. small_values is set in Step 2 and
  # was invisible in Step 3 even though it flips the direction gate in Risk of
  # Bias (and, on the continuous path, the sign of the responder odds ratio).
  # Boxed like Control-group risk, Decision threshold, Presentation of event
  # rates: it is one of the things the five domains depend on, and it used to
  # float between the boxes as though it were a caption for one of them.
  # Editing stays in Step 2.
  output$direction_echo <- shiny::renderUI({
    sv <- state$small_values
    label <- if (identical(sv, "desirable")) {
      "Favorable - a smaller value of this outcome is better"
    } else if (identical(sv, "undesirable")) {
      "Unfavorable - a smaller value of this outcome is worse"
    } else {
      "(not set)"
    }
    .config_section(
      "Outcome direction",
      htmltools::p(class = "pma-card-subtitle", style = "margin: 0;",
        htmltools::tags$strong(label),
        " - set in Step 2. It sets the bias direction Risk of Bias checks.")
    )
  })
  shiny::outputOptions(output, "direction_echo", suspendWhenHidden = FALSE)

  # How the derived direction lands in the responder conversion.
  output$chinn_direction_echo <- shiny::renderUI({
    inv <- chinn_invert_derived()
    htmltools::p(class = "pma-card-subtitle",
      "Direction of the responder odds ratio: ",
      htmltools::tags$strong(
        if (inv) "sign flipped" else "as analysed"),
      if (inv) {
        paste0(" - a smaller value of this outcome is better, so the sign of ",
               "the standardized mean difference is flipped to put the ",
               "intervention above 1. Derived from the Step 2 direction ",
               "answer; not asked again here.")
      } else {
        paste0(" - a larger value of this outcome is better, so the ",
               "standardized mean difference is used as analysed. Derived ",
               "from the Step 2 direction answer; not asked again here.")
      })
  })
  shiny::outputOptions(output, "chinn_direction_echo",
                       suspendWhenHidden = FALSE)

  # Pointer left where the responder-conversion controls used to be.
  output$display_options_config_note <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    if (step3_is_binary_outcome(obj, input$outcome_type)) return(NULL)
    htmltools::p(class = "pma-card-subtitle",
      "How this outcome is presented - as a proportion of responders or as ",
      "the summary measure itself - is set on the Configuration tab, ",
      "together with the control-group responder proportion and the ",
      "definition of the threshold of clinical interest.")
  })

  # Smart defaults for the four Step 3 forest display panels. Only state$ is
  # read: while the user is on Step 3, the Step 2 body (and therefore
  # input$experimental_label / input$control_label) does not exist.
  #
  # The title suffix mirrors what export_bundle.R writes for each stratified
  # plot, so the on-screen title and the exported one agree.
  .forest_label_defaults <- shiny::reactive({
    iv  <- state$arm_e %||% ""
    ct  <- state$arm_c %||% ""
    fav <- pma_favors_labels(state$small_values, iv, ct)
    list(title = state$outcome_name %||% "", label_e = iv, label_c = ct,
         favors_left = fav$left, favors_right = fav$right)
  })
  .forest_title_suffix <- c(
    rob    = " (stratified by Risk of Bias)",
    incon  = "",
    indir  = " (stratified by Indirectness)",
    pubias = " (available vs missing results)"
  )
  for (.pfx in names(.forest_title_suffix)) {
    # local() binds the prefix per iteration; without it all four panels would
    # be wired to the last prefix.
    local({
      p <- .pfx
      pma_autofill_forest_panel(input, session, prefix = p,
                                values_fn = .forest_label_defaults,
                                title_suffix = .forest_title_suffix[[p]])
    })
  }

  # ----- Saving the current outcome into state$outcomes -------------------
  # Key for the saved outcome: the Outcome name entered in Step 2. The label
  # is what grade_table() prints in the Outcome column, so keeping the two
  # identical avoids a second, divergent name field.
  .save_key <- shiny::reactive({
    nm <- trimws(state$outcome_name %||% "")
    if (nzchar(nm)) nm else "Outcome"
  })

  .save_blocked_reasons <- shiny::reactive({
    reasons <- character()
    if (is.null(state$ma)) {
      reasons <- c(reasons, "run the meta-analysis in Step 2")
    }
    if (is.null(grade_obj())) {
      reasons <- c(reasons, "produce a certainty rating")
    }
    unconf <- pma_unconfirmed_domains(domain_confirmed())
    if (length(unconf)) {
      reasons <- c(reasons, paste0("review and confirm: ",
                                   paste(unconf, collapse = ", ")))
    }
    reasons
  })

  output$save_outcome_panel <- shiny::renderUI({
    reasons <- .save_blocked_reasons()
    if (length(reasons)) {
      # Same locked-note treatment as the Step 4 download gate: an
      # unconfirmed assessment must not be banked into the SoF table.
      return(htmltools::div(
        class = "pma-card-subtitle",
        style = paste(
          "border: 1px dashed hsl(var(--border)); border-radius: 6px;",
          "padding: 0.75rem; margin-top: 0.5rem;"),
        htmltools::p(style = "margin: 0;",
          htmltools::strong("Saving locked - certainty assessment incomplete.")),
        htmltools::p(style = "margin: 0.25rem 0 0;",
          paste0("To save this outcome, ", paste(reasons, collapse = "; "), "."))
      ))
    }
    key <- .save_key()
    htmltools::div(
      style = "margin-top: 0.5rem;",
      shiny::actionButton(
        "save_outcome",
        sprintf("Save this outcome's assessment as \"%s\"", key),
        class = "btn btn-primary", style = "width: 100%;"),
      # Sits beside Save so the reviewer can carry straight on to the next
      # outcome. It returns to Step 2 and clears everything that belongs to
      # this outcome (app.R's begin_new_outcome()).
      pma_add_next_outcome_button(style = "width: 100%;"),
      htmltools::p(
        class = "pma-card-subtitle",
        style = "margin-top: 0.4rem;",
        "Saved under the Outcome name set in Step 2 - change it there to ",
        "relabel the Summary of Findings row. \"+ Add next outcome\" returns ",
        "to Step 2 with this outcome's name, direction, follow-up and every ",
        "certainty answer cleared; the saved outcomes, the loaded data and ",
        "the per-study risk-of-bias and indirectness ratings are kept.")
    )
  })
  shiny::outputOptions(output, "save_outcome_panel", suspendWhenHidden = FALSE)

  # Signature of the dataset currently loaded in Step 1. Used both to stamp
  # newly saved outcomes and to flag already-saved ones that came from a
  # different dataset (see pma_dataset_signature()).
  .current_signature <- shiny::reactive(pma_dataset_signature(state$data))

  .store_outcome <- function(key, g) {
    outs <- pma_outcomes_list(state$outcomes)
    # Follow-up and unit are per-outcome, so they are banked ON the saved
    # object rather than read from the live Step 2 fields at render time -
    # otherwise the combined Step 4 table would print the current outcome's
    # follow-up against every earlier row. grade_table(style = "bmj") picks
    # these up per row via .display_arg_from_outcomes() (grade_table.R), which
    # is why Step 4 passes no follow_up / unit argument of its own.
    g$follow_up <- sof_follow_up()
    g$unit      <- pma_sof_unit(g, state$outcome_unit)
    attr(g, "pma_saved_at") <- Sys.time()
    # Provenance stamp: which dataset this rating was made on.
    attr(g, PMA_DATASET_SIGNATURE_ATTR) <- pma_dataset_signature(state$data)
    outs[[key]] <- g
    state$outcomes <- outs
    shiny::showNotification(
      sprintf("Saved \"%s\" (%s certainty). %d outcome(s) ready for the combined Summary of Findings table.",
              key, g$certainty %||% "-", length(outs)),
      type = "message", duration = 5)
  }

  shiny::observeEvent(input$save_outcome, {
    if (length(.save_blocked_reasons())) {
      shiny::showNotification(
        "Cannot save: review and confirm every certainty domain first.",
        type = "error", duration = 6)
      return()
    }
    g <- grade_obj()
    if (is.null(g)) return()
    key <- .save_key()
    # grade_table() labels rows by list name, so the pmatools object's own
    # outcome_name is aligned with the key for any downstream single-outcome
    # use of the saved object.
    g$outcome_name <- key
    if (key %in% names(pma_outcomes_list(state$outcomes))) {
      shiny::showModal(shiny::modalDialog(
        title = "Outcome already saved",
        htmltools::p(sprintf(
          "\"%s\" is already in the saved list. Replace it with the current assessment?",
          key)),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("save_outcome_overwrite", "Replace",
                              class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
      return()
    }
    .store_outcome(key, g)
  })

  shiny::observeEvent(input$save_outcome_overwrite, {
    shiny::removeModal()
    if (length(.save_blocked_reasons())) return()
    g <- grade_obj()
    if (is.null(g)) return()
    key <- .save_key()
    g$outcome_name <- key
    .store_outcome(key, g)
  })

  output$saved_outcomes_list <- shiny::renderUI({
    outs <- pma_outcomes_list(state$outcomes)
    sig  <- .current_signature()
    n_stale <- sum(pma_outcomes_stale(outs, sig))
    htmltools::tagList(
      pma_stale_warning_banner(n_stale),
      pma_saved_outcomes_ui(outs,
                            delete_input_id = "outcome_delete",
                            empty_text = EDU_COPY$multi_outcome$list_empty,
                            signature = sig,
                            primary = state$sof_primary)
    )
  })
  shiny::outputOptions(output, "saved_outcomes_list", suspendWhenHidden = FALSE)

  # ----- Row order and primary outcomes -----------------------------------
  # state$outcomes stays a plain named list (see the note on
  # pma_outcomes_list()), so the reordering rules are borrowed rather than
  # reimplemented: a pmatools_set is built for the call, reorder_outcomes() /
  # set_primary() validate and apply it, and only the result is kept. Moving
  # the app's storage to the class would change the ZIP's directory layout,
  # which is a separate step.
  #
  # Both observers live here, beside Remove, and serve BOTH saved-outcome
  # lists: the Step 3 one and the Step 4 one write to the same input ids, and
  # only one step body is mounted at a time.

  shiny::observeEvent(input$outcome_delete, {
    key  <- as.character(input$outcome_delete)[1]
    outs <- pma_outcomes_list(state$outcomes)
    if (!key %in% names(outs)) return()
    outs[[key]] <- NULL
    state$outcomes <- outs
    state$sof_primary <- intersect(state$sof_primary %||% character(0),
                                   names(outs))
    shiny::showNotification(sprintf("Removed \"%s\" from the saved outcomes.", key),
                            type = "message", duration = 4)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$outcome_move, {
    info <- input$outcome_move
    key  <- as.character(info$name %||% "")[1]
    dir  <- as.character(info$dir  %||% "")[1]
    outs <- pma_outcomes_list(state$outcomes)
    nms  <- names(outs)
    i <- match(key, nms)
    if (is.na(i)) return()
    j <- if (identical(dir, "up")) i - 1L else i + 1L
    if (j < 1L || j > length(nms)) return()
    new_order <- nms
    new_order[c(i, j)] <- nms[c(j, i)]
    set <- tryCatch(
      reorder_outcomes(.outcome_set(outs, state$sof_primary), new_order),
      error = function(e) {
        shiny::showNotification(paste("Could not reorder:", conditionMessage(e)),
                                type = "error", duration = 6)
        NULL
      })
    if (is.null(set)) return()
    state$outcomes <- set$outcomes[set$order]
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$outcome_primary, {
    key  <- as.character(input$outcome_primary$name %||% "")[1]
    outs <- pma_outcomes_list(state$outcomes)
    if (!key %in% names(outs)) return()
    cur <- intersect(state$sof_primary %||% character(0), names(outs))
    new <- if (key %in% cur) setdiff(cur, key) else c(cur, key)
    set <- tryCatch(
      set_primary(.outcome_set(outs), if (length(new)) new else NULL),
      error = function(e) {
        shiny::showNotification(
          paste("Could not set the primary outcomes:", conditionMessage(e)),
          type = "error", duration = 6)
        NULL
      })
    if (is.null(set)) return()
    state$sof_primary <- set$primary
  }, ignoreInit = TRUE)

  # ----- Per-study RoB / Indirectness editors (synced with Step 1) -----
  .step3_bulk_set <- function(col, value) {
    d <- state$rob_table
    if (is.null(d)) return()
    d[[col]] <- value
    state$rob_table <- d
  }

  shiny::observeEvent(input$step3_rob_set_low,    { .step3_bulk_set("rob", "low")  })
  shiny::observeEvent(input$step3_rob_set_some,   { .step3_bulk_set("rob", "some") })
  shiny::observeEvent(input$step3_rob_set_high,   { .step3_bulk_set("rob", "high") })
  shiny::observeEvent(input$step3_rob_clear,      { .step3_bulk_set("rob", NA_character_) })
  shiny::observeEvent(input$step3_indir_set_low,  { .step3_bulk_set("indirectness", "low")  })
  shiny::observeEvent(input$step3_indir_set_some, { .step3_bulk_set("indirectness", "some") })
  shiny::observeEvent(input$step3_indir_set_high, { .step3_bulk_set("indirectness", "high") })
  shiny::observeEvent(input$step3_indir_clear,    { .step3_bulk_set("indirectness", NA_character_) })

  output$step3_rob_editor <- DT::renderDT({
    d <- state$rob_table
    if (is.null(d)) {
      return(DT::datatable(data.frame(message = "Load data in Step 1 first."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    some_as <- .rob_some_concerns_setting()
    tbl <- d[, c("studlab", "rob"), drop = FALSE]
    tbl[["Risk group"]] <- .rob_risk_group(d$rob, some_as)
    dt <- DT::datatable(
      tbl,
      # Only the rating is editable; "Risk group" is derived from it.
      editable = list(target = "cell", disable = list(columns = c(0, 2))),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
    DT::formatStyle(
      dt, "Risk group",
      color = DT::styleEqual(
        c("Low", "Low (some concerns)", "Low (unrated)"),
        rep("#166534", 3), default = "#b45309"),
      fontWeight = "600")
  })
  # The editor sits in a collapsed <details>. Suspended, it does not pick up a
  # change to the low/high boundary made while it is closed, so the derived
  # "Risk group" column would be stale the next time it is opened.
  shiny::outputOptions(output, "step3_rob_editor", suspendWhenHidden = FALSE)

  output$step3_indir_editor <- DT::renderDT({
    d <- state$rob_table
    if (is.null(d)) {
      return(DT::datatable(data.frame(message = "Load data in Step 1 first."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(
      d[, c("studlab", "indirectness"), drop = FALSE],
      editable = list(target = "cell", disable = list(columns = 0)),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
  })

  .step3_validate_value <- function(val) {
    if (is.na(val) || !nzchar(val)) return(NA_character_)
    val <- tolower(trimws(val))
    if (!val %in% c("low", "some", "high")) {
      shiny::showNotification(
        "Value must be 'low', 'some', or 'high' (case-insensitive).",
        type = "warning"
      )
      return(NULL)
    }
    val
  }

  shiny::observeEvent(input$step3_rob_editor_cell_edit, {
    info <- input$step3_rob_editor_cell_edit
    if (is.null(info)) return()
    d <- state$rob_table
    if (is.null(d)) return()
    val <- .step3_validate_value(as.character(info$value))
    if (is.null(val)) return()
    d$rob[info$row] <- val
    state$rob_table <- d
  })

  shiny::observeEvent(input$step3_indir_editor_cell_edit, {
    info <- input$step3_indir_editor_cell_edit
    if (is.null(info)) return()
    d <- state$rob_table
    if (is.null(d)) return()
    val <- .step3_validate_value(as.character(info$value))
    if (is.null(val)) return()
    d$indirectness[info$row] <- val
    state$rob_table <- d
  })

  # Sync state$rob_table back into state$data so grade_meta picks up edits
  # made on Step 3 immediately (without re-running Step 1 commit).
  shiny::observe({
    rt <- state$rob_table
    d  <- state$data
    if (is.null(rt) || is.null(d)) return()
    shiny::isolate({
      idx <- match(as.character(d$studlab), as.character(rt$studlab))
      changed <- FALSE
      if (any(!is.na(rt$rob))) {
        new_rob <- rt$rob[idx]
        cur_rob <- d$rob %||% rep(NA_character_, nrow(d))
        if (!identical(as.character(cur_rob), new_rob)) {
          d$rob <- new_rob
          changed <- TRUE
        }
      }
      if (any(!is.na(rt$indirectness))) {
        new_indir <- rt$indirectness[idx]
        cur_indir <- d$indirectness %||% rep(NA_character_, nrow(d))
        if (!identical(as.character(cur_indir), new_indir)) {
          d$indirectness <- new_indir
          changed <- TRUE
        }
      }
      if (changed) state$data <- d
    })
  })

  state$step3_commit <- function() TRUE
}
