# column_roles.R - which column filled which role, and which studies survived
#
# Split out of ui_helpers.R. Three subjects, one file, because all three are
# the app telling the reviewer WHAT IT IS ACTUALLY ANALYSING rather than what
# they believe they asked for:
#
#   - Step 2's required fields and column mapping: the answers Run analysis
#     cannot proceed without, and the set of mapping selects that varies with
#     the outcome type.
#   - Step 1's detected-columns strip. An upload gets column detection wrong,
#     and a 39-column preview table answers "here is your data" instead of "did
#     it load correctly?". The strip states, per role, which column filled it.
#     The roles and their order come from the package's detect_column_roles(),
#     which resolves them exactly as ingest_data() does; only the human labels
#     and the traffic light are here.
#   - The risk-of-bias analysis set (Core GRADE 4 Fig 2). When the flowchart
#     reaches "use low risk of bias studies only", pmatools refits on that
#     subset and reports it through an R-level message() that never reaches a
#     browser. These helpers put the dropped studies on screen instead.
#
# THE RULE FOR A NEW HELPER: it belongs here when it describes the INPUT to an
# analysis - the columns, the fields, the studies - rather than the analysis's
# result. A helper about the pooled estimate belongs elsewhere; a helper about
# which rows went into it belongs here.
#
# Pure: a detect_column_roles() frame or a GRADE object in, a data.frame or
# HTML out, no reactives. The Step 2 helpers are pure for a specific reason -
# step2_server() holds the reactive that decides whether the required-field
# marks are ARMED, so the rule they are painted from stays testable on its own.

# ----- Step 2 required fields ---------------------------------------------
# Which of the two required Step 2 fields are still blank. Pure, so the rule
# the marks are painted from is testable without a session; step2_server()
# holds the reactive that calls it and decides whether the marks are ARMED
# (see PMA_STEP2_REQUIRED and the two-tier CSS in www/shadcn.css).
PMA_STEP2_REQUIRED <- c("outcome_name", "small_values")

pma_step2_required_unset <- function(outcome_name, small_values) {
  unset <- character(0)
  nm <- outcome_name
  if (is.null(nm) || length(nm) != 1L || is.na(nm) ||
      !nzchar(trimws(as.character(nm)))) {
    unset <- c(unset, "outcome_name")
  }
  sv <- small_values
  if (is.null(sv) || length(sv) != 1L || is.na(sv) ||
      !nzchar(as.character(sv))) {
    unset <- c(unset, "small_values")
  }
  unset
}

# ----- Step 2 column mapping ----------------------------------------------
# The mapping selects are required too, but the set of them is not fixed: a
# binary outcome needs `event`, a continuous one `mean` and `sd`. They ride on
# the same `pma_required_fields` message as the two fields above -- the client
# paints them with the same two-tier mark, and (once ARMED) opens the accordion
# panel hiding any that is blank, so pressing Run analysis can never leave the
# reviewer looking at a collapsed panel with no idea what is wrong.
#
# ALL is what the message declares it manages, and stays fixed on purpose: the
# client caches per-id flags, so an id dropped from the list would keep its
# last mark rather than losing it when the outcome type changes.
PMA_STEP2_MAPPING_ALL <- c("col_studlab", "col_treat", "col_n",
                           "col_event", "col_mean", "col_sd")

pma_step2_mapping_required <- function(outcome_type) {
  measure <- if (identical(outcome_type, "continuous")) {
    c("col_mean", "col_sd")
  } else {
    "col_event"
  }
  c("col_studlab", "col_treat", "col_n", measure)
}

# `values` is a named list of the current select values, keyed by input id.
pma_step2_mapping_unset <- function(outcome_type, values) {
  required <- pma_step2_mapping_required(outcome_type)
  is_blank <- function(id) {
    v <- values[[id]]
    is.null(v) || length(v) != 1L || is.na(v) ||
      !nzchar(trimws(as.character(v)))
  }
  required[vapply(required, is_blank, logical(1))]
}

# --------------------------------------------------------------------------
# Detected-columns strip (Step 1)
# --------------------------------------------------------------------------
# Step 1's job is to answer "did my data load correctly?", and a 39-column
# table answers "here is your data" instead. The strip states, per role, which
# column filled it -- which is precisely what an upload gets wrong, and what
# nothing on the screen used to say.
#
# The roles and their order come from detect_column_roles(), which resolves
# them exactly as ingest_data() does. Only the human labels and the traffic
# light live here.
PMA_ROLE_LABELS <- c(
  studlab      = "Study",
  treat        = "Arm",
  n            = "Sample size",
  event        = "Events",
  mean         = "Mean",
  sd           = "SD",
  outcome      = "Outcome",
  rob          = "Risk of bias",
  indirectness = "Indirectness",
  subgroup     = "Subgroup"
)

# Roles whose absence is ordinary rather than a problem: an analysis of one
# outcome with no strata is the common case, so flagging them amber would
# make the strip noise.
PMA_ROLE_OPTIONAL <- c("outcome", "subgroup")

# The measure columns. A binary outcome needs `event`; a continuous one needs
# `mean` and `sd`. Whichever branch the data did not take is not missing, so
# the unused half is reported as optional rather than amber.
PMA_ROLE_MEASURE_BINARY     <- "event"
PMA_ROLE_MEASURE_CONTINUOUS <- c("mean", "sd")

# Per-role status for the strip. Pure: same inputs, same rows, no reactives.
#
# `detected` is a detect_column_roles() frame. `judgments` is the per-study
# Risk of Bias / Indirectness table (state$rob_table) or NULL -- those two
# roles report how much of the review has been RATED, not what the file
# happened to carry, because a reviewer can fill them here with the bulk
# buttons and the chip has to follow.
#
# Returns a data.frame of role, label, column, status ("found" / "missing" /
# "optional") and hint.
pma_column_role_status <- function(detected, judgments = NULL) {
  has_binary     <- .role_found(detected, PMA_ROLE_MEASURE_BINARY)
  has_continuous <- all(vapply(PMA_ROLE_MEASURE_CONTINUOUS,
                               function(r) .role_found(detected, r), logical(1)))

  rows <- lapply(seq_len(nrow(detected)), function(i) {
    role   <- detected$role[i]
    column <- detected$column[i]
    found  <- isTRUE(detected$found[i])

    if (role %in% c("rob", "indirectness")) {
      return(.judgment_role_row(role, column, judgments))
    }

    is_unused_measure <-
      (role == PMA_ROLE_MEASURE_BINARY && has_continuous && !has_binary) ||
      (role %in% PMA_ROLE_MEASURE_CONTINUOUS && has_binary && !has_continuous)

    # With neither measure branch satisfied nothing is "unused", so all three
    # measure roles fall through to missing -- which is the right answer: the
    # analysis has no numbers to pool.
    status <- if (found) {
      "found"
    } else if (role %in% PMA_ROLE_OPTIONAL || is_unused_measure) {
      "optional"
    } else {
      "missing"
    }

    data.frame(role = role, label = unname(PMA_ROLE_LABELS[role]),
               column = column, status = status,
               hint = .role_hint(role, status),
               stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

.role_found <- function(detected, role) {
  isTRUE(detected$found[match(role, detected$role)])
}

# Risk of Bias / Indirectness are judgments, not data. The chip counts rated
# studies so that assigning them here turns the chip green, and so that a file
# carrying an unreadable label cannot show green on the strength of the column
# existing.
.judgment_role_row <- function(role, column, judgments) {
  values <- if (is.data.frame(judgments) && role %in% names(judgments)) {
    as.character(judgments[[role]])
  } else {
    character(0)
  }
  total <- length(values)
  rated <- sum(!is.na(values) & nzchar(trimws(values)))

  status <- if (total > 0 && rated == total) "found" else "missing"
  hint <- if (status == "found") {
    ""
  } else if (total == 0) {
    "not rated yet"
  } else {
    sprintf("%d of %d studies rated", rated, total)
  }

  data.frame(role = role, label = unname(PMA_ROLE_LABELS[role]),
             column = column, status = status, hint = hint,
             stringsAsFactors = FALSE)
}

.role_hint <- function(role, status) {
  if (status == "found") return("")
  if (status == "optional") return("not in your data")
  switch(
    role,
    studlab = "no study column",
    treat   = "no arm column",
    n       = "no sample-size column",
    event   = "no events column",
    mean    = "no mean column",
    sd      = "no SD column",
    "not in your data"
  )
}

# The columns the analysis actually reads, in role order, restricted to those
# the data carries. Everything else is context the reviewer brought along.
pma_analysis_columns <- function(data) {
  intersect(names(PMA_ROLE_LABELS), names(data))
}

# What was loaded, as a sentence. Replaces the monospace "Status: 36 rows, 18
# studies (long format)." line -- same facts, in the banner that says them.
pma_load_summary <- function(data) {
  rows    <- nrow(data)
  studies <- length(unique(as.character(data$studlab)))
  if (!"outcome" %in% names(data)) {
    return(sprintf("%d rows, %d studies, long format.", rows, studies))
  }
  units <- length(unique(paste(data$studlab, data$outcome, sep = "\r")))
  sprintf("%d rows, %d studies, %d study-outcomes, long format.",
          rows, studies, units)
}

# One chip per role. The source column is named only when it differs from the
# role, so a canonical table reads as a row of plain green names rather than
# ten copies of "studlab from studlab".
pma_column_roles_strip <- function(detected, judgments = NULL) {
  status <- pma_column_role_status(detected, judgments)

  chips <- lapply(seq_len(nrow(status)), function(i) {
    row <- status[i, ]
    detail <- if (nzchar(row$hint)) {
      row$hint
    } else if (!is.na(row$column) && !identical(row$column, row$role)) {
      row$column
    } else {
      NULL
    }
    htmltools::div(
      class = paste0("pma-role-chip pma-role-", row$status),
      htmltools::span(class = "pma-role-chip-label", row$label),
      if (!is.null(detail)) {
        htmltools::span(class = "pma-role-chip-detail", detail)
      }
    )
  })

  htmltools::div(
    htmltools::div(class = "pma-role-strip-title", "Detected columns"),
    htmltools::div(class = "pma-role-strip", chips)
  )
}

# --------------------------------------------------------------------------
# Risk-of-bias analysis set (Core GRADE 4 Fig 2)
# --------------------------------------------------------------------------
# pmatools 0.5 refits the meta-analysis on the low risk-of-bias subset when
# the flowchart reaches "use low risk of bias studies only" (rob_refit =
# TRUE, the default). Every domain, the rating target, the baseline risk and
# the Summary of Findings then rest on that restricted estimate, so the
# pooled effect shown in Step 3 can differ from the one computed in Step 2.
# The package reports this through an R-level message() that never reaches
# the browser; the helpers below put the same facts on screen.
#
# Returns NULL whenever the analysis rests on all studies, so every caller
# can render nothing at all rather than an empty container.
pma_analysis_set_info <- function(g) {
  if (is.null(g)) return(NULL)
  if (!identical(g$rob_analysis_set %||% "all", "low_only")) return(NULL)

  meta_full <- g$meta_full %||% g$meta
  meta_used <- g$meta
  if (is.null(meta_used) || is.null(meta_full)) return(NULL)

  # $rob_refit is FALSE when the restriction was recommended but skipped
  # (fewer than two low risk-of-bias studies, or update.meta() failed). The
  # recommendation still stands, but $meta is then the full analysis.
  refit <- isTRUE(g$rob_refit)

  studlab_full <- as.character(meta_full$studlab %||% character(0))
  studlab_used <- as.character(meta_used$studlab %||% character(0))
  dropped <- if (refit) setdiff(studlab_full, studlab_used) else character(0)

  .k <- function(m, lab) {
    k <- suppressWarnings(as.integer(m$k %||% NA_integer_))
    if (is.na(k)) length(lab) else k
  }
  .eff <- function(m) {
    out <- tryCatch(format_effect(m, g$outcome_type, prediction = FALSE),
                    error = function(e) NA_character_)
    if (is.null(out) || length(out) != 1 || is.na(out)) "not estimable"
    else gsub("\n", "; ", out)
  }

  list(
    refit      = refit,
    k_full     = .k(meta_full, studlab_full),
    k_used     = .k(meta_used, studlab_used),
    dropped    = dropped,
    effect_all = .eff(meta_full),
    effect_low = if (refit) .eff(meta_used) else NULL
  )
}

# One-line state notice for the persistent Step 3 indicator. Self-contained:
# it reads the rated object only, so it is unaffected by anything the sticky
# certainty badge / summary do.
pma_analysis_set_indicator <- function(g) {
  info <- pma_analysis_set_info(g)
  if (is.null(info)) return(NULL)
  htmltools::div(
    class = "pma-analysis-set",
    htmltools::span(class = "pma-analysis-set-label", "Analysis set"),
    if (info$refit) {
      sprintf("low risk-of-bias studies only (%d of %d studies)",
              info$k_used, info$k_full)
    } else {
      sprintf(paste0("all %d studies; the recommended low risk-of-bias ",
                     "restriction could not be applied"), info$k_full)
    }
  )
}

# Full explanatory banner for the Risk of Bias and Final certainty tabs.
# Excluded study labels are folded into a <details> once the list grows past
# eight, so a large meta-analysis cannot push the rest of the tab off screen.
pma_analysis_set_banner <- function(g) {
  info <- pma_analysis_set_info(g)
  if (is.null(info)) return(NULL)

  rule <- paste0(
    "Core GRADE 4 Fig 2 recommends restricting the analysis to low ",
    "risk-of-bias studies when they and the high risk-of-bias studies give ",
    "substantially different estimates.")

  if (!info$refit) {
    return(pma_banner(
      htmltools::strong("Restriction recommended but not applied. "),
      rule,
      sprintf(paste0(" Here the restriction could not be carried out (fewer ",
                     "than two low risk-of-bias studies remained, or the ",
                     "refit failed), so every number shown in Step 3 is ",
                     "still the all-studies analysis of %d studies: %s."),
              info$k_full, info$effect_all)
    ))
  }

  estimate_row <- function(label, value) {
    htmltools::div(
      class = "pma-analysis-set-estimate",
      htmltools::span(class = "pma-analysis-set-estimate-label", label),
      htmltools::span(class = "pma-analysis-set-estimate-value", value)
    )
  }
  dropped_list <- htmltools::span(
    class = "pma-analysis-set-studies",
    paste(info$dropped, collapse = ", "))

  pma_banner(
    htmltools::strong("Analysis restricted to low risk-of-bias studies. "),
    rule,
    sprintf(paste0(" Every domain judgment, the rating target, the baseline ",
                   "risk and the Summary of Findings below therefore rest on ",
                   "%d of %d studies, not on the pooled estimate reported in ",
                   "Step 2."), info$k_used, info$k_full),
    htmltools::div(
      class = "pma-analysis-set-compare",
      estimate_row(sprintf("All studies (%d)", info$k_full), info$effect_all),
      estimate_row(sprintf("Low risk of bias only (%d)", info$k_used),
                   info$effect_low)
    ),
    if (length(info$dropped)) {
      htmltools::div(
        class = "pma-analysis-set-dropped",
        sprintf("%d %s excluded from the estimate above: ",
                length(info$dropped),
                if (length(info$dropped) == 1) "study" else "studies"),
        if (length(info$dropped) > 8) {
          htmltools::tags$details(
            class = "pma-analysis-set-details",
            htmltools::tags$summary("Show the excluded study labels"),
            dropped_list)
        } else {
          dropped_list
        }
      )
    }
  )
}
