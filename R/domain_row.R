# domain_row.R - the row a domain assessor returns, and the gates around it
#
# Split out of utils.R. Every Core GRADE domain assessor ends by calling
# make_domain_row(), so this file owns that row's shape: the judgment, the
# levels down it carries, the notes prose, the structured facts attached to it,
# and the "Manual override (...)" clause a reviewer's rationale is composed into
# and parsed back out of. The two input gates live here as well, because both
# guard what may reach a row -- a manual override needs a written rationale, and
# no domain may be assessed without the outcome's direction.
#
# A new helper belongs here when it shapes a domain row or reads one back. The
# public accessor over the facts recorded here is domain_facts() in
# R/domain_facts.R; the judgment vocabulary itself is R/grade_vocabulary.R.

# Gather one certainty-domain judgment into the summary tibble row that every
# domain assessor returns. One row, one domain, the same columns every time --
# which is what lets grade_meta() bind five assessors' output into a single
# domain_assessments table without knowing which of them produced what.
#
# rationale: free-text justification for a manual override of an automated
# domain judgment (Core GRADE transparency principle). When non-NULL it is
# composed into `notes` as "Manual override (<judgment>): <rationale>",
# prepended with the existing " | " separator style so downstream consumers
# (evidence_profile footnotes via .first_sentence(), grade_report notes
# columns) surface the rationale automatically.
make_domain_row <- function(domain, judgment, auto, notes = NA_character_,
                            rationale = NULL, facts = NULL) {
  judgment <- .normalize_grade_level(judgment)
  if (!is.null(rationale)) {
    override_note <- sprintf("Manual override (%s): %s", judgment,
                             trimws(rationale))
    notes <- if (is.na(notes) || !nzchar(notes)) {
      override_note
    } else {
      paste(override_note, notes, sep = " | ")
    }
  }
  row <- tibble::tibble(
    domain    = domain,
    judgment  = judgment,
    downgrade = GRADE_DOWNGRADE[[judgment]],
    auto      = auto,
    notes     = notes
  )
  # Structured facts travel as an attribute, exactly like the analysis-set
  # recommendation assess_rob() carries (.rob_row()): the domain tibble must
  # stay one row per domain with atomic columns, so a list-column is not an
  # option. grade_meta() lifts the attribute off the row before
  # dplyr::bind_rows() drops it.
  if (!is.null(facts)) attr(row, "facts") <- facts
  row
}

# The inverse of the composition above: recover the override clause from a
# domain's `notes`, or NULL when the domain was not overridden.
#
# Why parse instead of reading `auto`: `auto = FALSE` does not mean "the
# reviewer overrode the rating". It also means "the reviewer supplied an input
# the algorithm cannot compute" -- assess_pubias() records auto = FALSE for an
# answered pubias_small_industry or pubias_unpublished, where the flowchart
# still decided the judgment and the facts still explain it. Only the
# "Manual override (...)" head marks a rating the reviewer SET, and only those
# need the notes to reach a footnote. `auto` is still checked, as a guard
# against a domain note that merely quotes the phrase.
#
# The rationale runs to the first " | ": make_domain_row() and the Shiny app's
# app-level overrides both join the override clause to the automatic note with
# that separator, and the automatic note is the flowchart prose, which is far
# too long for a table footer and whose numbers the facts already carry.
.parse_override_note <- function(notes, auto = FALSE) {
  if (isTRUE(auto)) return(NULL)
  if (is.null(notes) || length(notes) != 1L || is.na(notes) ||
      !nzchar(notes)) {
    return(NULL)
  }
  m <- regmatches(notes,
                  regexec("^Manual override \\(([^)]*)\\): (.*)$", notes))[[1]]
  if (length(m) < 3L) return(NULL)
  rationale <- trimws(strsplit(m[3], " | ", fixed = TRUE)[[1]][1])
  if (is.na(rationale) || !nzchar(rationale)) return(NULL)
  list(judgment = m[2], rationale = rationale)
}

# The same, keyed by domain on a pmatools object's domain_assessments.
.domain_override_note <- function(x, domain) {
  d <- x$domain_assessments
  if (!is.data.frame(d) || !all(c("domain", "notes", "auto") %in% names(d))) {
    return(NULL)
  }
  row <- d[d$domain == domain, , drop = FALSE]
  if (nrow(row) == 0L) return(NULL)
  .parse_override_note(row$notes[1], auto = row$auto[1])
}

# --------------------------------------------------------------------------
# Structured facts behind a domain judgment
#
# `notes` stays the authoritative prose (it is printed by print.pmatools(),
# grade_report() and export_bundle(), and parsed by hand downstream), so these
# record the SAME numbers in a machine-readable shape ALONGSIDE it, never
# instead of it. Callers that want "how many high risk of bias studies?" or
# "what was I2?" should read the facts rather than regex the sentences.
#
# Only Risk of bias, Inconsistency and Imprecision emit facts today;
# Indirectness and Publication bias keep prose-only notes. The container is
# domain-agnostic so they can adopt it without a change here or in the
# renderers.
# --------------------------------------------------------------------------

# One structured fact behind a domain judgment.
#   key     : stable snake_case machine key, e.g. "high_rob_studies"
#   label   : sentence-case label used when the fact is rendered as a footnote
#   value   : single pre-formatted string, ready to print
#   numeric : the raw number when the fact is scalar-numeric, else NA_real_
# Facts that exist for a renderer to compute with, not for a reader. They are
# returned by domain_facts() like any other -- that is the whole point of a
# machine-readable companion -- but every PROSE renderer drops them, because
# "Flowchart path: pma-rob-node-dominance pma-rob-edge-dominance-yes ..." is not a
# footnote anybody wants under a Summary of Findings table.
.FACT_KEYS_MACHINE_ONLY <- c("flow_path")

.drop_machine_only_facts <- function(facts) {
  if (is.null(facts) || !is.data.frame(facts) || !"key" %in% names(facts)) {
    return(facts)
  }
  facts[!facts$key %in% .FACT_KEYS_MACHINE_ONLY, , drop = FALSE]
}

.fact <- function(key, label, value, numeric = NA_real_) {
  tibble::tibble(
    key     = as.character(key),
    label   = as.character(label),
    value   = as.character(value),
    numeric = as.numeric(numeric)
  )
}

# Bind .fact() results into one tibble. NULL entries are dropped (so a caller
# can build a fact conditionally with an `if` that yields NULL), a single list
# argument is accepted, and an empty result is NULL rather than a 0-row tibble
# so that "this domain recorded nothing" is one test everywhere.
.facts <- function(...) {
  parts <- list(...)
  if (length(parts) == 1L && is.list(parts[[1]]) &&
      !is.data.frame(parts[[1]])) {
    parts <- parts[[1]]
  }
  parts <- parts[!vapply(parts, is.null, logical(1))]
  parts <- parts[vapply(parts, is.data.frame, logical(1))]
  if (length(parts) == 0L) return(NULL)
  out <- dplyr::bind_rows(parts)
  if (nrow(out) == 0L) return(NULL)
  out
}

# GRADE transparency gate for manual overrides (v0.4.0, breaking change).
# Overriding an automated domain judgment requires a written justification.
# Aborts unless `rationale` is a single non-NA, non-empty, non-whitespace
# string. Returns the rationale invisibly on success. `hint` appends a
# call-site-specific sentence telling the user how to avoid the override
# altogether (used by the Indirectness subdomain path).
.check_override_rationale <- function(rationale, arg, domain_label,
                                      hint = NULL) {
  ok <- is.character(rationale) && length(rationale) == 1L &&
        !is.na(rationale) && nzchar(trimws(rationale))
  if (!ok) {
    msg <- sprintf(
      paste0(
        "Overriding the %s judgment requires %s: state why the automated ",
        "assessment was replaced (Core GRADE transparency principle)."
      ),
      domain_label, arg
    )
    if (!is.null(hint)) msg <- paste(msg, hint)
    rlang::abort(msg)
  }
  invisible(rationale)
}

# --------------------------------------------------------------------------
# Outcome-direction entry gate (v0.5.1, breaking)
# --------------------------------------------------------------------------
# `small_values` says which way benefit runs for this outcome, and two domains
# cannot be assessed without it: the Core GRADE 4 Fig 2 direction-of-bias check
# needs to know which shift would flatter the intervention, and the Core GRADE 2
# optimal information size needs to know whether a benefit is a modest relative
# reduction or a modest relative increase in the event rate. Both used to guess
# when the argument was absent, and the risk-of-bias guess was loud enough to
# warn that it had decided the downgrade.
#
# NO ESCAPE HATCH, and that is the difference from require_threshold. Rating
# without a MID is a legitimate choice (Core GRADE 7 asks users to pin one down
# only where the verdict depends on it), so require_threshold = FALSE exists.
# Rating without a direction is not a choice: every outcome a review rates has
# one, and "direction unknown" only ever means the outcome has not finished
# being specified.
SMALL_VALUES_LEVELS <- c("desirable", "undesirable")

.check_small_values <- function(small_values, arg = "small_values") {
  ok <- is.character(small_values) && length(small_values) == 1L &&
        !is.na(small_values) && small_values %in% SMALL_VALUES_LEVELS
  if (ok) return(invisible(small_values))

  got <- if (is.null(small_values)) {
    "nothing was supplied"
  } else {
    paste0("received ",
           paste(deparse(small_values, width.cutoff = 500L), collapse = ""))
  }
  rlang::abort(sprintf(paste0(
    "%s is required and must be 'desirable' or 'undesirable' (%s). ",
    "'desirable' means a small value of this outcome is good (mortality, ",
    "symptom severity); 'undesirable' means a small value is bad (response ",
    "rate, remission). Risk of bias needs it to know which way bias would ",
    "flatter the intervention, and the optimal information size needs it to ",
    "know whether a benefit is a modest relative reduction or a modest ",
    "relative increase in the event rate. pmatools will not guess: it used ",
    "to, and the guess decided ratings. There is no way to proceed without ",
    "the answer, because every outcome has a direction."),
    arg, got), class = "pmatools_direction_gate")
}
