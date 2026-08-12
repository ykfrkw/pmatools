# not_reported.R - Outcomes the review prespecified but nobody reported
#
# Core GRADE 6 asks the summary of findings table to cover every
# patient-important outcome the review set out to address, including the ones
# the evidence base turns out to be silent on. Every row of grade_table() is
# otherwise derived from `x$meta`, so such an outcome could not be expressed at
# all: there is no meta-analysis to derive it from.
#
# WHY A SEPARATE CLASS, NOT A `pmatools` WITH A NULL $meta
# --------------------------------------------------------------------------
# grade_table.R, sof_bmj.R, export_bundle_multi.R and grade_report.R between
# them dereference `g$meta$...` roughly forty times. A `pmatools` object with a
# NULL `$meta` would flow through every one of them and silently evaluate to
# NULL - a table full of blank cells, an export bundle missing files, and no
# error anywhere to say why. `pmatools_not_reported` deliberately does NOT
# inherit `"pmatools"`, so it fails every existing `inherits(x, "pmatools")`
# guard loudly and each consumer has to opt in on purpose. The consumers that
# do are listed in the roxygen of not_reported_outcome() below.

# The certainty cell of a not-reported row. Not blank: a blank cell cannot be
# told apart from a forgotten one, which is the very argument for showing the
# row in the first place.
NOT_REPORTED_CERTAINTY <- "Not rated"

# Domain cell of a not-reported row in the GRADEpro layout: an en dash, which
# must stay visually distinct from .domain_symbol()'s "?" (= judgment unknown,
# go and find it). \u escape, like CERTAINTY_SYMBOLS_UNICODE, so the source
# stays ASCII-safe whatever the file encoding.
NOT_REPORTED_DOMAIN_SYMBOL <- "\u2013"

#' Declare an outcome that no included study reported
#'
#' @description
#' Builds a `pmatools_not_reported` object: a prespecified outcome for which no
#' included study supplied usable data. It carries no meta-analysis, no effect
#' estimate and no certainty rating, but it still occupies a row of the summary
#' of findings table, so a reader can see that the review addressed the outcome
#' and found nothing (Core GRADE 6).
#'
#' Use \code{\link{add_not_reported}} to put one into a `pmatools_set`; the
#' object is accepted by \code{\link{grade_table}},
#' \code{\link{grade_report}} and \code{\link{export_bundle}} and rejected,
#' with a message saying so, by \code{\link{sof_table}} and
#' \code{\link{evidence_profile}}.
#'
#' @param outcome_name Single non-empty string: the outcome as the review
#'   prespecified it.
#' @param follow_up Optional single string, shown under the outcome name in the
#'   same place a rated outcome's follow-up appears.
#' @param reason Optional single string explaining why nothing was reported
#'   ("Measured in two trials but reported only as a figure"). It becomes a
#'   numbered footnote on the table row.
#' @param label Single non-empty string used in every value cell of the row.
#'   Default `"Not reported"`.
#'
#' @details
#' Two judgment calls are baked in.
#'
#' First, the certainty cell reads `"Not rated"` rather than being left blank.
#' A blank cell is indistinguishable from a cell somebody forgot to fill in -
#' the same argument that motivates showing the row at all - and there is no
#' body of evidence here to rate, so naming the absence is more honest than
#' implying a low rating.
#'
#' Second, the outcome appears in the summary of findings table and as a prose
#' line in the evidence profile, but never as an evidence-profile table row.
#' All five domain columns of that table are judgments about a body of
#' evidence; without studies they are undefined, not "not serious".
#'
#' @return An object of class `pmatools_not_reported`.
#'
#' @seealso \code{\link{add_not_reported}}, \code{\link{grade_table}}.
#'
#' @examples
#' \dontrun{
#' nr <- not_reported_outcome(
#'   "Quality of life",
#'   follow_up = "12 months",
#'   reason    = "Prespecified in the protocol; no included trial measured it."
#' )
#' print(nr)
#'
#' # In a multi-outcome table, next to the rated outcomes:
#' grade_table(list("Mortality" = g1, "Quality of life" = nr))
#' }
#'
#' @export
not_reported_outcome <- function(outcome_name,
                                 follow_up = NULL,
                                 reason    = NULL,
                                 label     = "Not reported") {
  if (!is.character(outcome_name) || length(outcome_name) != 1L ||
      is.na(outcome_name) || !nzchar(outcome_name)) {
    rlang::abort(paste0(
      "not_reported_outcome: 'outcome_name' must be a single non-empty ",
      "character string."))
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) ||
      !nzchar(label)) {
    rlang::abort(paste0(
      "not_reported_outcome: 'label' must be a single non-empty character ",
      "string (the text shown in every value cell of the row)."))
  }

  structure(
    list(
      outcome_name = outcome_name,
      follow_up    = .nr_opt_string(follow_up, "follow_up"),
      reason       = .nr_opt_string(reason,    "reason"),
      label        = label
    ),
    class = "pmatools_not_reported"
  )
}

# Optional single string: NULL, NA and "" all normalise to NULL, so downstream
# code only ever has to test for NULL.
.nr_opt_string <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (length(x) != 1L || !is.character(x)) {
    rlang::abort(sprintf(
      "not_reported_outcome: '%s' must be NULL or a single character string.",
      arg))
  }
  if (is.na(x) || !nzchar(x)) return(NULL)
  x
}

.is_not_reported <- function(x) inherits(x, "pmatools_not_reported")

.not_reported_label <- function(x) x$label %||% "Not reported"

# The one footnote shared by every not-reported row of a table, emitted once
# per table rather than once per row.
.not_reported_table_note <- function() {
  paste0(
    "Not reported = the outcome was prespecified in the review but no ",
    "included study reported usable data for it. Such outcomes carry no ",
    "effect estimate and therefore no certainty rating; they are listed so ",
    "the table covers every patient-important outcome the review set out to ",
    "address (Core GRADE 6)."
  )
}

# Plain-language cell / prose line for a not-reported outcome.
.not_reported_plain <- function() {
  "No included study reported this outcome."
}

.has_not_reported <- function(outcomes) {
  any(vapply(outcomes, .is_not_reported, logical(1)))
}

# The rated subset. Effect-measure headers and every domain-derived footnote
# are computed over this, so one not-reported outcome cannot degrade a table
# that is otherwise homogeneous.
.rated_outcomes <- function(outcomes) {
  outcomes[!vapply(outcomes, .is_not_reported, logical(1))]
}

#' @export
print.pmatools_not_reported <- function(x, ...) {
  cat("\n-- Outcome not reported by any included study ------------\n")
  cat(sprintf(" Outcome  : %s\n", x$outcome_name))
  if (!is.null(x$follow_up)) cat(sprintf(" Follow-up: %s\n", x$follow_up))
  if (!is.null(x$reason))    cat(sprintf(" Reason   : %s\n", x$reason))
  cat(" No included study reported this outcome; no certainty rating.\n")
  cat("----------------------------------------------------------\n\n")
  invisible(x)
}

# --------------------------------------------------------------------------
# Set integration
# --------------------------------------------------------------------------

#' Add a not-reported outcome to a certainty set
#'
#' @description
#' Appends an outcome that no included study reported to a `pmatools_set`, so
#' it takes its place among the rated outcomes in \code{\link{grade_table}},
#' \code{\link{grade_report}} and \code{\link{export_bundle}}.
#'
#' \code{\link{reorder_outcomes}} and \code{\link{set_primary}} treat the new
#' outcome exactly like a rated one: both key off names only, so a not-reported
#' outcome can be moved to the top of the table or marked primary.
#'
#' @param set A `pmatools_set` from \code{\link{grade_meta_multi}}.
#' @param outcome_name Single non-empty string, not already in the set.
#' @param follow_up,reason,label Passed to
#'   \code{\link{not_reported_outcome}}.
#' @param after Where to insert the outcome in the set's order: `NULL`
#'   (default) appends it at the end, an existing outcome name inserts it just
#'   after that outcome, and a non-negative integer inserts it after that many
#'   outcomes (`0` puts it first).
#'
#' @details
#' See \code{\link{not_reported_outcome}} for the two judgment calls this
#' feature makes: the certainty cell reads `"Not rated"` rather than being
#' blank, and the outcome appears in the summary of findings table and as a
#' prose line in the evidence profile but never as an evidence-profile table
#' row.
#'
#' @return The set, with the outcome added.
#'
#' @seealso \code{\link{not_reported_outcome}}.
#'
#' @examples
#' \dontrun{
#' set <- grade_meta_multi(ml, common = list(study_design = "RCT",
#'                                           threshold_type = "null"))
#' set <- add_not_reported(
#'   set, "Quality of life",
#'   follow_up = "12 months",
#'   reason    = "Prespecified; no included trial measured it.",
#'   after     = "Mortality"
#' )
#' grade_table(set, style = "bmj")
#' }
#'
#' @export
add_not_reported <- function(set,
                             outcome_name,
                             follow_up = NULL,
                             reason    = NULL,
                             label     = "Not reported",
                             after     = NULL) {
  .check_pmatools_set(set, "add_not_reported")

  nr <- not_reported_outcome(outcome_name, follow_up = follow_up,
                             reason = reason, label = label)

  if (outcome_name %in% names(set$outcomes)) {
    rlang::abort(sprintf(paste0(
      "add_not_reported: the set already holds an outcome named '%s'. ",
      "An outcome is either rated or not reported, not both."), outcome_name))
  }

  set$outcomes[[outcome_name]] <- nr
  set$order <- .insert_after(set$order, outcome_name, after)
  set
}

# Insert `what` into `order` at the position `after` names. `after` is an
# existing outcome name, a non-negative integer position, or NULL for the end.
.insert_after <- function(order, what, after) {
  if (is.null(after)) return(c(order, what))

  pos <- if (is.character(after) && length(after) == 1L && !is.na(after)) {
    idx <- match(after, order)
    if (is.na(idx)) {
      rlang::abort(sprintf(paste0(
        "add_not_reported: 'after' names an outcome that is not in the set: ",
        "%s. The set holds: %s."), after, paste(order, collapse = ", ")))
    }
    idx
  } else if (is.numeric(after) && length(after) == 1L && !is.na(after) &&
             after >= 0 && after == as.integer(after) &&
             after <= length(order)) {
    as.integer(after)
  } else {
    rlang::abort(paste0(
      "add_not_reported: 'after' must be NULL, an outcome name already in the ",
      "set, or a single integer between 0 and the number of outcomes."))
  }

  append(order, what, after = pos)
}
