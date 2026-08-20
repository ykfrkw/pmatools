# judgment_display.R - how a rating looks once it has been made
#
# Split out of ui_helpers.R. Everything here takes a judgment that pmatools has
# already reached and turns it into something on screen: the certainty badge,
# the downgrade chip, the per-domain verdict line, the facts list under it, the
# per-study Risk of Bias / Indirectness pickers and the client-side script that
# makes the bulk buttons work, and the four Core GRADE decision flowcharts with
# the branch actually taken lit up.
#
# THE RULE FOR A NEW HELPER: nothing here may DECIDE anything. Every rating
# rule lives in the package (grade_meta() and the domain functions) and every
# flowchart figure is a static SVG staged under _pmatools_inst/figures/, named
# by PMA_FLOWCHART_FIGS; this file only picks a colour, a word and a set of
# node ids to light up. The moment a helper here starts choosing a certainty level rather than
# painting one, it has become app-side rating logic and belongs in the package
# instead. The wording itself comes from GRADE_LEVEL_SOURCE_WORDING /
# .grade_level_wording() in the package's R/grade_vocabulary.R for exactly that reason -
# a badge and the Evidence Profile cannot be allowed to name one judgment two
# ways.
#
# Pure: labels and fact frames in, HTML out. pma_study_level_column() builds a
# column of selectInputs rather than a Shiny module for the same reason
# R/step3_pubias.R is not a module - the ids are spelled out in step3_ui() and
# in the app suite, and namespacing them would rename all of it.

# GRADE certainty badge
pma_certainty_badge <- function(label) {
  cls <- switch(tolower(label),
    "high"     = "grade-high",
    "moderate" = "grade-moderate",
    "low"      = "grade-low",
    "very low" = "grade-vlow",
    # NOT_REPORTED_CERTAINTY. Named rather than left to fall through to the
    # "low" default, which would colour an unrated outcome as a rated one.
    "not rated" = "grade-unrated",
    "grade-low"
  )
  symbol <- switch(label,
    "High"     = htmltools::HTML("&#8853;&#8853;&#8853;&#8853;"),
    "Moderate" = htmltools::HTML("&#8853;&#8853;&#8853;&#9675;"),
    "Low"      = htmltools::HTML("&#8853;&#8853;&#9675;&#9675;"),
    "Very Low" = htmltools::HTML("&#8853;&#9675;&#9675;&#9675;"),
    htmltools::HTML("")
  )
  htmltools::span(class = paste("pma-badge", cls),
                  htmltools::HTML(paste0(label, " ", as.character(symbol))))
}

# Downgrade chip - shows -0/-1/-2/-3 next to judgment badge.
#
# The level -> downgrade table is the vendored .grade_level_downgrade(), not a
# copy: the app used to carry its own and it had to be edited by hand every
# time the package gained a level.
pma_downgrade_chip <- function(judgment) {
  j  <- judgment %||% "not_serious"
  dg <- .grade_level_downgrade(j)
  cls <- if (dg == 0) "grade-high"
         else if (dg == -1) "grade-low"
         else "grade-vlow"
  label <- if (dg == 0) "+0" else as.character(dg)
  htmltools::span(class = paste("pma-badge pma-chip", cls), label)
}

# Generic judgment badge.
#
# The words are NOT chosen here. .grade_level_wording() (vendored R/grade_vocabulary.R,
# driven by GRADE_LEVEL_SOURCE_WORDING) is the app's single display
# vocabulary, shared with the Evidence Profile: "Not serious" / "Serious" /
# "Very serious" / "Extremely serious", which is Core GRADE's own wording. The
# badge used to print "No concern" / "Some concerns" / "Serious" from a second
# hand-written switch, so the same judgment read one way on the tab and another
# way in the exported table. Only the CSS class mapping is this function's own,
# and it is keyed on the downgrade so a new level cannot arrive unstyled.
pma_judgment_badge <- function(judgment) {
  j  <- judgment %||% "not_serious"
  dg <- .grade_level_downgrade(j)
  # An unrecognised level keeps the neutral-amber class it has always had:
  # .grade_level_downgrade() reports 0 for it, which would otherwise paint it
  # green and read as "no concern".
  cls <- if (!.normalize_grade_level(j) %in% GRADE_LEVELS) "grade-low"
         else if (dg == 0) "grade-high"
         else if (dg == -1) "grade-low"
         else "grade-vlow"
  htmltools::span(class = paste("pma-badge", cls),
                  pma_judgment_label(j))
}

# One judgment, worded for display. Wraps the vendored helper so the app has
# a single call site to change if the package ever moves it, and so the tests
# have something app-side to assert against.
pma_judgment_label <- function(judgment) {
  .grade_level_wording(judgment %||% "not_serious", sentence = TRUE)
}

# Choices for every override widget on the Step 3 domain tabs, and for the
# Indirectness overall radio.
#
# The values are the stored GRADE levels, which since 0.5.1 are Core GRADE's
# own words; the labels keep the downgrade in brackets because a reviewer
# choosing "Serious" wants to see what it costs.
#
# "Extremely serious (-3)" is offered here and nowhere automatic. Core GRADE 1
# calls it rare ("or, rarely, extremely serious") and no flowchart in the
# package reaches it, so this menu is the only way into the level -- and it
# lands in the same rationale gate every other override does.
#
# `include_blank` is the "(no override)" entry the four selectInputs need and
# the Indirectness radio does not. That radio ships PRESELECTED to
# STEP3_INDIR_DEFAULT_LEVEL, so it has no blank to offer: accepting the
# automatic worst-case fold is now a rating that equals it, not an empty
# group, and step3_indir_rationale_required() is what tells the two apart.
pma_judgment_choices <- function(include_blank = TRUE,
                                 blank_label = "(no override)") {
  out <- c("Not serious (-0)"       = "not_serious",
           "Serious (-1)"           = "serious",
           "Very serious (-2)"      = "very_serious",
           "Extremely serious (-3)" = "extremely_serious")
  if (isTRUE(include_blank)) out <- c(stats::setNames("", blank_label), out)
  out
}

# ----- Per-study level pickers (Step 3 RoB / Indirectness editors) --------
#
# A DIFFERENT vocabulary from pma_judgment_choices() above. That one names a
# judgment about the whole body of evidence, in Core GRADE's words; this one
# names a judgment about ONE study, in the words of the tool that made it.
# Reviewers type these two into each other constantly, which is why they sit
# next to each other here with the distinction written down.
#
# Cochrane RoB 2 (Sterne JAC, et al. BMJ 2019;366:l4898) defines exactly three
# judgments and these are its own words. Four-level wordings belong to
# ROBINS-I, which the app cannot reach: step3_grade.R passes
# study_design = "RCT" unconditionally.
PMA_ROB2_CHOICES <- c("Low risk of bias"  = "low",
                      "Some concerns"     = "some",
                      "High risk of bias" = "high")

# Indirectness has no RoB 2. These are pmatools' own forest-plot strata
# (rob_strata()), worded so that nothing here can be mistaken for a published
# risk-of-bias judgment.
PMA_INDIRECTNESS_CHOICES <- c("Low indirectness"  = "low",
                              "Some indirectness" = "some",
                              "High indirectness" = "high")

# One <select> for one cell of a DT editor.
#
# Why the cell holds a real <select> rather than DT's own editor: DT 0.34's
# `editable=` injects <input type=text> (or number / textarea / date) and has
# no dropdown type at all -- its factor/selectize support is for COLUMN
# FILTERS, not for editing. The Publication bias tab reaches its closed-ish
# vocabulary with a <datalist> bolted onto that injected input, but a datalist
# is autocomplete: it still accepts anything typed, which is the failure mode
# this control exists to remove. So the column is rendered as HTML with
# escape = FALSE and left out of `editable`, and the change event carries the
# row index the cell was RENDERED with -- not DT's `col`, which counts hidden
# columns and has bitten this app before.
pma_study_level_select <- function(row, value, input_id, choices) {
  offered  <- c(stats::setNames("", "(not set)"), choices)
  selected <- as.character(value %||% "")[1]
  # A value the control does not offer reads as "(not set)", explicitly. It
  # cannot arrive from the dropdown; it can arrive from an uploaded column,
  # and leaving nothing marked would let the browser pick the first option and
  # show a judgment nobody made.
  if (length(selected) != 1L || is.na(selected) ||
      !selected %in% unname(offered)) {
    selected <- ""
  }

  options <- paste0(
    "<option value=\"", unname(offered), "\"",
    ifelse(unname(offered) == selected, " selected", ""), ">",
    names(offered), "</option>",
    collapse = "")

  sprintf(
    paste0("<select class=\"pma-level-select form-select form-select-sm\" ",
           "data-input=\"%s\" data-row=\"%d\">%s</select>"),
    input_id, as.integer(row), options)
}

# The whole column, vectorised over a data frame's rows.
pma_study_level_column <- function(values, input_id, choices) {
  if (length(values) == 0L) return(character(0))
  vapply(seq_along(values),
         function(i) pma_study_level_select(i, values[[i]], input_id, choices),
         character(1))
}

# Delegated change handler for every pma_study_level_select() on the page.
# Delegated so a re-rendered DT needs no re-binding.
#
# `.pmaLevel` and the `off()` are load-bearing, not tidiness: app.R rebuilds
# the whole Step 3 body on every step change and Shiny re-executes inline
# scripts in what it inserts, so a plain `on()` would stack a second handler
# on every 3 -> 2 -> 3 round trip and report each change once per rebuild.
pma_study_level_script <- function() {
  htmltools::tags$script(htmltools::HTML(paste0(
    "$(document).off('change.pmaLevel')",
    ".on('change.pmaLevel', 'select.pma-level-select', function(){",
    "  Shiny.setInputValue(this.dataset.input,",
    "    {row: parseInt(this.dataset.row, 10), value: this.value},",
    "    {priority: 'event'});",
    "});"
  )))
}

# ----- One evaluation shape for all five domain tabs ----------------------
# Every domain tab used to print the machine-generated note string raw, into
# a verbatimTextOutput several hundred characters long, under the heading
# "Evaluation". Three helpers replace it: the verdict is one line, the numbers
# behind it come forward as a short list, and the full prose moves one click
# away. Nothing is deleted - the note is still there, verbatim, inside the
# <details>.

# The headline line: what this domain was rated, in Core GRADE's words, with
# the downgrade it carries.
pma_domain_verdict <- function(judgment, downgrade = NULL) {
  j  <- judgment %||% "not_serious"
  dg <- downgrade
  if (is.null(dg) || length(dg) != 1L || is.na(dg)) {
    # .grade_level_downgrade() reports 0 for an unrecognised level rather than
    # aborting, which is what a tab needs: a rendering failure here would take
    # the whole panel down.
    dg <- .grade_level_downgrade(j)
  }
  if (is.na(dg)) dg <- 0
  dg <- as.integer(dg)
  htmltools::div(
    class = "pma-domain-verdict",
    htmltools::strong(pma_judgment_label(j)),
    htmltools::span(
      class = "pma-domain-verdict-dg",
      if (dg == 0L) " - do not rate down"
      else sprintf(" - rate down %d level%s", abs(dg),
                   if (abs(dg) == 1L) "" else "s"))
  )
}

# The numbers, as a compact definition list. `facts` is a domain_facts()
# tibble (key / label / value / numeric) or NULL. `keys` restricts and orders
# the rows; NULL takes the first `max_rows` as the assessor emitted them.
#
# Returns NULL when there is nothing to show, so a caller can drop it into a
# tagList without a conditional.
pma_facts_list <- function(facts, keys = NULL, max_rows = 6L) {
  if (is.null(facts) || !is.data.frame(facts) || nrow(facts) == 0L) {
    return(NULL)
  }
  if (!all(c("key", "label", "value") %in% names(facts))) return(NULL)
  # flow_path exists for www/flowchart.js, not for a reader: it is a list of
  # SVG element ids. The chart above this list is what it says.
  facts <- facts[!facts$key %in% "flow_path", , drop = FALSE]
  if (nrow(facts) == 0L) return(NULL)
  if (!is.null(keys)) {
    idx <- match(keys, facts$key)
    idx <- idx[!is.na(idx)]
    facts <- facts[idx, , drop = FALSE]
  }
  if (nrow(facts) == 0L) return(NULL)
  if (nrow(facts) > max_rows) facts <- facts[seq_len(max_rows), , drop = FALSE]
  htmltools::tags$dl(
    class = "pma-facts",
    lapply(seq_len(nrow(facts)), function(i) {
      htmltools::tagList(
        htmltools::tags$dt(facts$label[i]),
        htmltools::tags$dd(facts$value[i])
      )
    })
  )
}

# ----- Core GRADE decision flowcharts -------------------------------------
# The picture of the decision, with the branch this analysis took highlighted.
# Four of the five domains have one; Indirectness does not, because Core GRADE
# 5 Table 2 is a gradient rather than a flowchart (the subdomain table on that
# tab is its equivalent).
#
# WHY THE FILE IS READ HERE AND NOT IN THE PACKAGE. shiny/stage_bundle.R
# rewrites system.file(..., package = "pmatools") lookups in the VENDORED
# tree, but only ones asking for "templates" (TPL_LOOKUP_PAT), and it then
# fails the deploy on any survivor. A figure lookup inside R/ would therefore
# resolve to "" in the app AND break `stage_bundle.R --check-only`. The
# loader lives app-side instead, reading the staged copy directly, exactly the
# way R/step1_data.R reads _pmatools_inst/extdata/.

# Which domain each figure belongs to, and the function that implements it.
# One table so the caption under the chart and the roxygen in R/flowcharts.R
# cannot drift into naming different functions.
PMA_FLOWCHART_FIGS <- list(
  # `departure` is where a chart says what in it is NOT the source's. It exists
  # because the Fig 2 drawing lost its footnote when it was redrawn to the
  # source's shape (SPEC.md 5.1a): the closer the picture gets to the paper, the
  # more the reader needs telling which parts are ours, and the figure itself is
  # no longer the place that tells them. Omit the field for a chart that departs
  # from its source in nothing.
  "Risk of bias"     = list(fig = "rob",
                            fn  = "assess_rob()",
                            file = "R/domain_rob.R",
                            src = "Core GRADE 4 Fig 2",
                            # Reworded with the 0.5.1 redraw: the rules are no
                            # longer drawn, so a sentence placing them "between"
                            # two boxes described a picture that is not there.
                            # This is now the ONLY place in the UI that states
                            # the two-level rule 5, since the red leaf gave up
                            # its annotation -- test-flowchart-ui.R pins both
                            # halves of it.
                            departure = paste(
                              "The direction-of-bias question is decided by",
                              "five rules of pmatools' own, not the source's,",
                              "and they are reported in the notes rather than",
                              "drawn; rule 5 rates down two levels, which Core",
                              "GRADE 4 never does.")),
  "Inconsistency"    = list(fig = "incon",
                            fn  = "assess_inconsistency()",
                            file = "R/domain_inconsistency.R",
                            src = "Core GRADE 3 Fig 2"),
  "Imprecision"      = list(fig = "impre",
                            fn  = "assess_imprecision()",
                            file = "R/domain_imprecision.R",
                            src = "Core GRADE 2 Fig 4"),
  "Publication bias" = list(fig = "pubias",
                            fn  = "assess_pubias()",
                            file = "R/domain_pubias.R",
                            src = "Core GRADE 4 Fig 5")
)

# The candidate locations, in the order they are tried. `dir` short-circuits
# the search and exists so the helper is testable from tests/testthat, whose
# working directory is neither the app root nor a package install.
pma_flowchart_path <- function(figkey, dir = NULL) {
  f <- paste0(figkey, ".svg")
  candidates <- if (!is.null(dir)) {
    file.path(dir, f)
  } else {
    c(
      # Deployed / running app: pinned at startup by app.R, so a later
      # setwd() cannot move the target.
      file.path(getOption("pmatools.vendored_root", "."),
                "_pmatools_inst", "figures", f),
      # Same thing relative to the app root, for a run that never set it.
      file.path("_pmatools_inst", "figures", f),
      # Local development against an installed package.
      {
        p <- system.file("figures", f, package = "pmatools")
        if (nzchar(p)) p else NULL
      },
      # Local development in the source tree, app not staged.
      file.path("..", "inst", "figures", f)
    )
  }
  hit <- Filter(function(p) !is.null(p) && nzchar(p) && file.exists(p),
                candidates)
  if (length(hit)) hit[[1L]] else NA_character_
}

# The chart itself. `on_ids` is the domain's `flow_path` fact, split; passing
# character(0) renders the plain figure. Returns a placeholder rather than
# erroring when the file is absent: a missing figure must never take a domain
# tab down with it.
pma_flowchart <- function(figkey, on_ids = character(0), caption = NULL,
                          dir = NULL) {
  path <- pma_flowchart_path(figkey, dir = dir)
  if (is.na(path)) {
    return(htmltools::p(
      class = "pma-flowchart-missing",
      "The decision flowchart for this domain is not available in this build."
    ))
  }
  svg <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                  error = function(e) NULL)
  if (is.null(svg) || !nzchar(trimws(svg))) {
    return(htmltools::p(
      class = "pma-flowchart-missing",
      "The decision flowchart for this domain could not be read."
    ))
  }
  ids <- on_ids
  ids <- ids[!is.na(ids) & nzchar(ids)]
  htmltools::tagList(
    htmltools::div(
      class = "pma-flowchart",
      # www/flowchart.js reads this and adds pma-fc-on to the named elements.
      # Empty when nothing is highlighted, which is a valid state, not a bug.
      `data-pma-path` = paste(ids, collapse = " "),
      htmltools::HTML(svg)
    ),
    if (!is.null(caption)) {
      htmltools::p(class = "pma-card-subtitle", caption)
    }
  )
}

# "Where is this implemented?", in one sentence, from the same table the
# figure is chosen from. Rendered as the chart's caption.
pma_algorithm_source <- function(domain) {
  spec <- PMA_FLOWCHART_FIGS[[domain]]
  if (is.null(spec)) return(NULL)
  provenance <- sprintf(
    "%s, as implemented by %s in %s of the pmatools package.",
    spec$src, spec$fn, spec$file)
  if (is.null(spec$departure)) return(provenance)
  paste(provenance, spec$departure)
}

# The flow_path fact, split into ids. `facts` is a domain_facts() tibble or
# NULL; anything else yields character(0), so a caller can hand the result
# straight to pma_flowchart().
pma_flow_path_ids <- function(facts) {
  if (is.null(facts) || !is.data.frame(facts) ||
      !all(c("key", "value") %in% names(facts))) {
    return(character(0))
  }
  v <- facts$value[facts$key == "flow_path"]
  if (length(v) != 1L || is.na(v) || !nzchar(trimws(v))) return(character(0))
  ids <- strsplit(trimws(v), "\\s+")[[1L]]
  ids[nzchar(ids)]
}

# The whole block that goes under a domain verdict: the chart in a <details>
# the reviewer can shut, captioned with where the algorithm lives.
#
# Open by default. It answers "why this judgment", which is exactly the
# question the verdict raises, and the user's rule is that answers stay
# visible; <details> rather than a plain div so a reviewer who has seen it can
# put it away.
pma_flowchart_details <- function(domain, facts,
                                  summary_text =
                                    "Which path did this assessment take?",
                                  dir = NULL) {
  spec <- PMA_FLOWCHART_FIGS[[domain]]
  if (is.null(spec)) return(NULL)
  htmltools::tags$details(
    class = "pma-flowchart-details", open = NA,
    htmltools::tags$summary(summary_text),
    pma_flowchart(spec$fig,
                  on_ids  = pma_flow_path_ids(facts),
                  caption = pma_algorithm_source(domain),
                  dir     = dir)
  )
}

# pma_notes_collapse() was deleted here. It parked the machine-generated note
# under every domain verdict, in a <details> that was a place to put prose
# rather than content: the flowchart above it draws the same decision and
# lights up the branch taken. The note itself is NOT lost - it travels into
# evidence_profile() and into the exported .docx exactly as before, which is
# where a verbatim record belongs.
