# utils.R — 共通ユーティリティ

# ==========================================================================
# THE DOMAIN-JUDGMENT VOCABULARY.  READ THIS BEFORE TOUCHING SIGNS.
# ==========================================================================
# The stored values ARE Core GRADE's words. Core GRADE 1, verbatim: "We
# characterise limitations in each of these domains involved in rating down
# certainty as not serious; serious; very serious; or, rarely, extremely
# serious."
#
#   value                 | Core GRADE wording  | levels down
#   ----------------------|---------------------|-------------
#   "not_serious"         | not serious         |  0
#   "serious"             | serious             | -1
#   "very_serious"        | very serious        | -2
#   "extremely_serious"   | extremely serious   | -3
#
# Until 0.5.0 the values were "no" / "some_concerns" / "serious" and the third
# of those meant the source's VERY serious (-2). Renaming moved "serious" from
# -2 to -1 without changing its spelling, which is the one change a script
# cannot notice, so a bare "serious" is REJECTED for this release -- see
# .check_grade_level_input() below.
#
# "extremely_serious" (-3) is reachable only by a human: no assessor in this
# package emits it, because no Core GRADE flowchart describes a three-level
# downgrade. It exists so a reviewer who judges one appropriate can record it
# with the written rationale every manual override already requires.
GRADE_LEVELS <- c("not_serious", "serious", "very_serious",
                  "extremely_serious")
GRADE_DOWNGRADE <- c(not_serious = 0, serious = -1,
                     very_serious = -2, extremely_serious = -3)

# The most severe level any automated path may produce. Asserted by the test
# suite against every assessor, because "manual only" is a property of the
# code, not of a comment.
GRADE_LEVEL_AUTO_MAX <- "very_serious"

# Spellings that never changed meaning, and so stay accepted silently.
# "no" is everywhere -- every assessor's do-not-rate-down leaf used to write
# it -- and it has always meant 0. "some" / "some_concerns" have always meant
# -1, which is what the new "serious" means, so they are also the unambiguous
# way to write -1 while a bare "serious" is refused.
GRADE_LEVEL_ALIASES <- c(
  no            = "not_serious",
  some          = "serious",
  some_concerns = "serious"
)

# Level -> Core GRADE wording, for user-facing display.
#
# This is a quotation from the source, not a transformation of the value.
# Deriving it with sub("_", " ", .) would give the same four strings today and
# would silently invent source wording for any level added later, so the table
# stays written out.
GRADE_LEVEL_SOURCE_WORDING <- c(
  not_serious       = "not serious",
  serious           = "serious",
  very_serious      = "very serious",
  extremely_serious = "extremely serious"
)

# Map legacy / synonym labels to canonical ones.
.normalize_grade_level <- function(x) {
  if (is.null(x)) return(x)
  out <- as.character(x)
  hit <- match(out, names(GRADE_LEVEL_ALIASES))
  out[!is.na(hit)] <- unname(GRADE_LEVEL_ALIASES[hit[!is.na(hit)]])
  out
}

# Levels down for a judgment, tolerant of legacy spellings and of anything
# unrecognised (which contributes 0 rather than aborting a render).
.grade_level_downgrade <- function(x) {
  lv  <- .normalize_grade_level(x)
  out <- unname(GRADE_DOWNGRADE[lv])
  out[is.na(out)] <- 0
  as.integer(out)
}

# --------------------------------------------------------------------------
# The one-release speed bump on a bare "serious".
#
# 0.5.0 stored "serious" for -2; 0.5.1 stores it for -1. A script written
# against either release keeps running and produces a DIFFERENT certainty
# rating, with no error and no warning -- so this release refuses the string
# instead of guessing which release the caller had in mind. Both replacement
# spellings offered below mean today exactly what they meant in 0.5.0.
#
# TEMPORARY. Once one release has passed, delete this function and its call
# sites; "serious" is then simply the -1 level like any other value.
# --------------------------------------------------------------------------
.check_grade_level_input <- function(x, arg = "argument", extra = NULL) {
  if (is.null(x) || !any(as.character(x) == "serious", na.rm = TRUE)) {
    return(invisible(x))
  }
  rlang::abort(paste0(
    if (!is.null(extra)) paste0(extra, "\n"),
    arg, " = \"serious\" is ambiguous across pmatools versions and is ",
    "rejected in this release.\n",
    "Up to 0.5.0 \"serious\" was this package's own name for Core GRADE's ",
    "\"very serious\" and rated the domain down 2 levels. From 0.5.1 the ",
    "stored values are Core GRADE's own words, so \"serious\" carries the ",
    "source's meaning and rates down 1 level. The same script would keep ",
    "running and report a different certainty, so say which you mean:\n",
    "  * rate down 1 level  (Core GRADE \"serious\")      -> ",
    arg, " = \"some_concerns\"\n",
    "  * rate down 2 levels (Core GRADE \"very serious\") -> ",
    arg, " = \"very_serious\"\n",
    "Both spellings mean in 0.5.1 exactly what they meant in 0.5.0. A plain ",
    "\"serious\" will be accepted again, as rate down 1 level, in a later ",
    "release."
  ))
}

# THE display vocabulary. Every user-facing rendering of a domain judgment -
# the Evidence Profile cells, the Shiny badges, the override menus, the BMJ
# SoF certainty sentence - goes through this one function, so the app and the
# exported table can never word the same judgment differently. Legacy labels
# are normalised first; anything still unrecognised is returned unchanged
# rather than replaced by a placeholder, so a new level shows up as itself.
#
# The seam survived the rename that made the values Core GRADE's own words. It
# still resolves the legacy spellings a stored object or a user argument may
# carry, it still turns a value into prose ("very_serious" is not what a table
# cell should read), and it is still the single place to change if the display
# vocabulary ever diverges from the stored one again.
#
# `sentence = TRUE` capitalises the first letter, which is what a badge or a
# menu entry wants; the Evidence Profile prints it lower-case mid-sentence.
.grade_level_wording <- function(x, sentence = FALSE) {
  lv  <- .normalize_grade_level(x)
  out <- unname(GRADE_LEVEL_SOURCE_WORDING[lv])
  out[is.na(out)] <- as.character(lv)[is.na(out)]
  if (isTRUE(sentence)) {
    out <- paste0(toupper(substring(out, 1L, 1L)), substring(out, 2L))
  }
  out
}
CERTAINTY_LABELS <- c("Very Low", "Low", "Moderate", "High")
CERTAINTY_SYMBOLS <- c(
  "High"       = "++++",
  "Moderate"   = "+++o",
  "Low"        = "++oo",
  "Very Low"   = "+ooo"
)

# Unicode rendering for SoF flextable / browser HTML (rich output targets)
# Use \u escapes so source is ASCII-safe regardless of file encoding.
CERTAINTY_SYMBOLS_UNICODE <- c(
  "High"       = "\u2295\u2295\u2295\u2295",
  "Moderate"   = "\u2295\u2295\u2295\u25cb",
  "Low"        = "\u2295\u2295\u25cb\u25cb",
  "Very Low"   = "\u2295\u25cb\u25cb\u25cb"
)

# Certainty color palettes (bg + text color pairs)
# pastel: soft backgrounds, colored text — readable on screen and in print
# classic: saturated backgrounds, white text — matches netmetaviz classic palette
CERTAINTY_PALETTES <- list(
  pastel = list(
    "High"     = list(bg = "#d7e8d3", text = "#238b21"),
    "Moderate" = list(bg = "#cccce9", text = "#01008b"),
    "Low"      = list(bg = "#f8edd7", text = "#daa521"),
    "Very Low" = list(bg = "#e8d0d0", text = "#8b0000")
  ),
  classic = list(
    "High"     = list(bg = "#1e8449", text = "#ffffff"),
    "Moderate" = list(bg = "#2471a3", text = "#ffffff"),
    "Low"      = list(bg = "#e67e22", text = "#ffffff"),
    "Very Low" = list(bg = "#c0392b", text = "#ffffff")
  )
)

# One family for every table this package builds, chosen for the .docx these
# tables are made to be dropped into: a word processor resolves a named face,
# not a CSS stack. The Shiny app restyles its on-screen copy in CSS instead of
# changing this, so the exported document keeps the face it was designed for.
.PMA_TABLE_FONT <- "Arial"

# Footer notes: 8pt grey, and the same family as the body.
#
# The family has to be re-applied here rather than left to font(part = "all").
# add_footer_lines() creates its rows AFTER that call has run, and a fresh row
# takes flextable's own default (Helvetica) instead of inheriting the table's,
# so every footer used to render in a different face from the body it annotated.
# Re-applying at the end makes the footer independent of call order.
.style_table_footer <- function(ft) {
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::color(ft, color = "#555555", part = "footer")
  flextable::font(ft, fontname = .PMA_TABLE_FONT, part = "footer")
}

# House style for every citation this package renders: first author, "et al.",
# journal abbreviation, year. No volume, no pages, no DOI, no URL.
#
# The Core GRADE papers defeat the bare form -- all six are Guyatt, all BMJ,
# all 2025, so they collapse into one indistinguishable string. They carry the
# series number as a prefix instead, and .core_grade_ref() is the only place
# that shape is written down.
.core_grade_ref <- function(number = NULL) {
  series <- if (is.null(number)) "Core GRADE series" else paste0("Core GRADE ", number)
  paste0(series, ". Guyatt G, et al. BMJ. 2025")
}

# Core GRADE series number -> DOI. One paper per number, so the number is the
# whole key; a caller that has the number never has to parse it back out of the
# citation string .core_grade_ref() built.
#
# The DOIs live here rather than in the app because the app is not the only
# thing that may want to reach the papers, and because a number-keyed map is
# the one shape that cannot drift from .core_grade_ref() beside it: both are
# indexed by the series number and nothing else. The citation STRING is still
# DOI-free house style (see above) -- what this adds is a destination to hang
# on it, not a longer citation.
PMA_CORE_GRADE_DOIS <- c(
  "1" = "10.1136/bmj-2024-081903",
  "2" = "10.1136/bmj-2024-081904",
  "3" = "10.1136/bmj-2024-081905",
  "4" = "10.1136/bmj-2024-083864",
  "5" = "10.1136/bmj-2024-083865"
)

# Resolvable URL for a Core GRADE paper, or NULL when the number is absent or
# is not one this map knows. NULL rather than an error: a missing link is a
# reference that renders as plain text, which is what the app did before, and
# no caller should lose a whole tab over it. Papers 6 and 7 of the series have
# no entry, so they take the NULL path until their DOIs are added.
.core_grade_doi_url <- function(number) {
  if (is.null(number) || length(number) != 1L || is.na(number)) return(NULL)
  # `[` and not `[[`: an unknown name yields NA here, where `[[` would abort.
  doi <- unname(PMA_CORE_GRADE_DOIS[as.character(number)])
  if (is.na(doi)) return(NULL)
  paste0("https://doi.org/", doi)
}

# The disclaimer that follows the series citation on every table this package
# builds. One literal, because eight footnotes used to word it four ways.
.PMA_CORE_GRADE_FOOTNOTE <- paste0(
  "Reference: ", .core_grade_ref(),
  ". Not an official GRADE Working Group assessment."
)

# Score -> certainty label.
#
# Very Low is the floor of the GRADE scale, so the clamp is not defensive
# tidying: four levels of downgrade from High (or the single -3 that
# "extremely_serious" carries, on top of anything else) lands below 1, and
# there is no rating below Very Low to report. Certainty stops there, which is
# why grade_meta() records `certainty_score` alongside the label -- the
# unclamped sum is not recoverable from "Very Low".
score_to_certainty <- function(score) {
  score <- max(1L, min(4L, as.integer(round(score))))
  c(1L, 2L, 3L, 4L) |>
    (\(.) CERTAINTY_LABELS[. == score])()
}

# GRADE 判定の検証 (legacy "no" / "some" / "some_concerns" も受け入れて正規化する)
#
# `check_ambiguous = FALSE` is for the one caller that validates values it has
# ALREADY normalised (assess_rob(), which maps Cochrane RoB2 labels first and
# guards the raw input separately). Running the ambiguity check there would
# reject "Some concerns", which normalises to "serious" and was never
# ambiguous.
validate_grade_level <- function(x, arg = "argument", check_ambiguous = TRUE) {
  if (isTRUE(check_ambiguous)) .check_grade_level_input(x, arg)
  valid <- c(GRADE_LEVELS, names(GRADE_LEVEL_ALIASES))
  bad <- setdiff(x, valid)
  if (length(bad) > 0) {
    rlang::abort(
      paste0(arg, " contains invalid GRADE level(s): ", paste(bad, collapse = ", "),
             ". Use one of: ", paste0("'", GRADE_LEVELS, "'", collapse = ", "), ".")
    )
  }
  invisible(x)
}

# --------------------------------------------------------------------------
# Baseline risk helpers
# --------------------------------------------------------------------------

#' Resolve baseline risk to a single numeric probability
#'
#' @param baseline_risk NULL, a numeric scalar, "simple", or "metaprop"
#' @param meta_obj meta object (used for auto-computation)
#' @param ois_p0 Fallback when baseline_risk is NULL
#' @return Numeric scalar in 0..1 or NULL
#' @keywords internal
#' @noRd
.resolve_baseline_risk <- function(baseline_risk, meta_obj, ois_p0 = NULL) {
  # 1. Explicit numeric
  if (is.numeric(baseline_risk)) {
    if (baseline_risk < 0 || baseline_risk > 1)
      rlang::abort("baseline_risk must be between 0 and 1.")
    return(baseline_risk)
  }
  # 2. "simple" or "metaprop"
  if (is.character(baseline_risk) && baseline_risk %in% c("simple", "metaprop")) {
    return(.compute_control_risk(meta_obj, method = baseline_risk))
  }
  # 3. NULL -> fallback to ois_p0, then simple auto-compute
  if (is.null(baseline_risk)) {
    if (!is.null(ois_p0) && is.numeric(ois_p0)) return(ois_p0)
    return(.compute_control_risk(meta_obj, method = "simple"))
  }
  NULL
}

# The three grade_meta() arguments that all name the control-arm event rate,
# in the order a value inherits from them (see .resolve_control_risk()).
CONTROL_RISK_ARGS <- c("threshold_baseline", "ois_p0", "baseline_risk")

# Human labels for the resolution note, so it reads as prose rather than as
# three argument names in a row.
CONTROL_RISK_USES <- c(
  threshold_baseline = "the absolute-threshold conversion",
  ois_p0             = "the optimal information size",
  baseline_risk      = "the Summary of Findings baseline"
)

#' Share one control-arm risk across the three arguments that name it
#'
#' \code{threshold_baseline}, \code{ois_p0} and \code{baseline_risk} are three
#' names for the control-arm event rate, used by three different calculations.
#' A caller who has one number for all three had to pass it three times. This
#' resolves the value once: an argument that was supplied keeps its own value,
#' and one that was left \code{NULL} inherits the first value supplied to any
#' of the others, in the order given by \code{CONTROL_RISK_ARGS}.
#'
#' The order is not arbitrary. \code{threshold_baseline} is the risk of the
#' population the decision threshold is about, and the Shiny app makes the
#' reviewer confirm or justify it in writing; \code{ois_p0} is Core GRADE 2's
#' "control group event rate (chosen from the context)"; \code{baseline_risk}
#' is presentational, and is the one that can legitimately describe a different
#' population from the other two (a Summary of Findings table routinely prints
#' several baseline risks for one effect estimate). So the most deliberate
#' value donates first and the most presentational donates last -- and none of
#' them ever displaces a value the caller supplied.
#'
#' Nothing is invented here: an argument that is still \code{NULL} afterwards
#' reaches its own calculation as \code{NULL} and takes that calculation's own
#' pooled-control-rate default, which is computed on the analysis actually
#' being rated (the low-RoB refit, when one happened).
#'
#' Why not one argument: consolidating the three onto \code{baseline_risk} is
#' the eventual destination, and it is a breaking rename of three public
#' arguments. v0.5.1 already carries a breaking rename of the domain judgment
#' vocabulary, and stacking a second migration on one release costs users two
#' passes over their scripts for one release's benefit. See SPEC.md §4.5.4.
#'
#' @param threshold_baseline,ois_p0,baseline_risk The three arguments as
#'   \code{grade_meta()} received them.
#' @return A list with the three resolved arguments under their own names,
#'   plus \code{donor} (the argument the shared value came from, or
#'   \code{NULL}), \code{value} (the shared value, or \code{NULL}),
#'   \code{inherited} (the arguments that took it) and \code{note} (one
#'   sentence naming both, or \code{NULL}).
#' @keywords internal
#' @noRd
.resolve_control_risk <- function(threshold_baseline = NULL, ois_p0 = NULL,
                                  baseline_risk = NULL) {
  supplied <- list(threshold_baseline = threshold_baseline,
                   ois_p0             = ois_p0,
                   baseline_risk      = baseline_risk)
  out <- c(supplied, list(donor = NULL, value = NULL,
                          inherited = character(0), note = NULL))

  # A donor has to be a number that every one of the three uses would accept.
  # threshold_baseline rejects 0 and 1 outright, so an exact 0 or 1 supplied to
  # baseline_risk (which does allow the closed interval) stays where it was put
  # rather than turning a working call into an error somewhere else. A
  # character baseline_risk ("simple" / "metaprop") names a computation over
  # the analysis, not a value, and each use already performs that computation
  # on the analysis it is judging -- so it does not donate either.
  .is_donor <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 && x < 1
  }
  .is_unset <- function(x) {
    is.null(x) || length(x) == 0L || (is.numeric(x) && is.na(x))
  }

  donors <- CONTROL_RISK_ARGS[vapply(supplied[CONTROL_RISK_ARGS], .is_donor,
                                     logical(1))]
  if (length(donors) == 0L) return(out)

  donor <- donors[1]
  value <- supplied[[donor]]
  takers <- CONTROL_RISK_ARGS[vapply(supplied[CONTROL_RISK_ARGS], .is_unset,
                                     logical(1))]
  if (length(takers) == 0L) {
    # All three were supplied. Still worth recording which value each use got,
    # because they may legitimately differ and the record is what says so.
    out$donor <- donor
    out$value <- value
    return(out)
  }

  for (nm in takers) out[[nm]] <- value
  out$donor     <- donor
  out$value     <- value
  out$inherited <- takers
  out$note      <- sprintf(
    paste0("Control-group risk %.4f supplied as `%s`; %s inherited it ",
           "(one value reaches all three; a value passed explicitly is never ",
           "displaced)."),
    value, donor,
    paste(sprintf("`%s` (%s)", takers, CONTROL_RISK_USES[takers]),
          collapse = " and ")
  )
  out
}

#' Compute control-arm event rate from a metabin object
#' @param meta_obj A meta object (from metabin).
#' @param method One of "simple" or "metaprop".
#' @keywords internal
#' @noRd
.compute_control_risk <- function(meta_obj, method = "simple") {
  ec <- meta_obj$event.c
  nc <- meta_obj$n.c
  if (is.null(ec) || is.null(nc) || length(nc) == 0 || sum(nc, na.rm = TRUE) == 0) {
    return(NULL)
  }
  if (length(ec) != length(nc)) return(NULL)

  # Both vectors must be filtered on the same studies. A study that reports a
  # denominator but no event count (eg it contributed a continuous outcome
  # only) otherwise drops out of `ec` while staying in `nc`, which inflates the
  # crude denominator and hands metaprop() two vectors of different lengths --
  # the latter error was swallowed below and returned the crude proportion
  # under the guise of a random-effects pooled estimate.
  keep <- !is.na(ec) & !is.na(nc) & nc > 0
  if (!any(keep)) return(NULL)
  ec <- ec[keep]
  nc <- nc[keep]

  if (method == "simple") {
    return(sum(ec) / sum(nc))
  }

  if (method == "metaprop") {
    mp <- tryCatch(
      meta::metaprop(event = ec, n = nc,
                     method = "GLMM", sm = "PLOGIT",
                     method.tau = "ML"),
      error = function(e) NULL
    )
    if (!is.null(mp) && !is.na(mp$TE.random)) {
      return(stats::plogis(mp$TE.random))
    }
    warning("metaprop() failed; falling back to simple pooled proportion.")
    return(sum(ec) / sum(nc))
  }
  NULL
}

# 確実性ドメイン判定をサマリ tibble にまとめる
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

# --------------------------------------------------------------------------
# Chinn's formula: SMD <-> log(OR) conversion
# --------------------------------------------------------------------------

#' Convert SMD to OR (Chinn's formula)
#'
#' Convert a standardized mean difference (SMD) and optionally its CI bounds to
#' an odds ratio (OR) using Chinn's formula: \eqn{\log(OR) = SMD \times \pi /
#' \sqrt{3}}. The conversion assumes a logistic latent-variable distribution
#' (Cox 1970; Hasselblad & Hedges 1995; Chinn 2000).
#'
#' @section Relation to Core GRADE 6 "option 2":
#' Core GRADE 6 also converts a continuous outcome to a binary one, but by a
#' \strong{different method}, and the two must not be conflated. Core GRADE 6
#' option 2 works from the MID, verbatim: "If systematic reviewers or guideline
#' developers know what the MID is for each of the instruments and assume a
#' normal distribution of results, they can calculate the proportion of people
#' who experience an improvement larger than the MID within each arm, thereby
#' obtaining a risk ratio or risk difference for each of the studies. They can
#' then pool these proportions across studies." That is a
#' normal-distribution-plus-MID calculation done \emph{per study, before
#' pooling}.
#'
#' Chinn's formula instead assumes a \emph{logistic} latent variable, needs no
#' MID, and is applied \emph{after} pooling, to the summary SMD. It answers a
#' different question and will not in general reproduce the option 2 numbers.
#' Core GRADE 6's option 2 is not implemented in pmatools.
#'
#' @param smd Numeric. Standardized mean difference (effect size).
#' @param ci_lower,ci_upper Optional numeric CI bounds on the SMD scale.
#'
#' @return A list with elements \code{or}, \code{or_lower}, \code{or_upper},
#'   and \code{factor} (the \eqn{\pi / \sqrt{3}} multiplier). NA inputs
#'   propagate to NA outputs.
#'
#' @references
#' Chinn S. A simple method for converting an odds ratio to effect size for use
#' in meta-analysis. Stat Med. 2000;19(22):3127-3131.
#'
#' @examples
#' chinn_smd_to_or(-0.5)
#' chinn_smd_to_or(-0.5, ci_lower = -0.7, ci_upper = -0.3)
#'
#' @export
chinn_smd_to_or <- function(smd, ci_lower = NULL, ci_upper = NULL) {
  factor <- pi / sqrt(3)
  list(
    or       = exp(smd * factor),
    or_lower = if (!is.null(ci_lower)) exp(ci_lower * factor) else NA_real_,
    or_upper = if (!is.null(ci_upper)) exp(ci_upper * factor) else NA_real_,
    factor   = factor
  )
}

# --------------------------------------------------------------------------
# Threshold auto-default per effect measure
# --------------------------------------------------------------------------

#' Suggest a placeholder Threshold based on the effect measure
#'
#' Returns a placeholder clinical decision Threshold (a minimally important
#' effect on the analysis scale) suitable for pre-filling the input field in
#' interactive UIs. \strong{These are pmatools conventions, not Core GRADE
#' values}, with one partial exception (SMD; see below). Replace them with a
#' published or expert-derived MID for the outcome in hand before reporting
#' anything.
#'
#' @param meta_obj A meta object (from \code{\link[meta]{metabin}} or
#'   \code{\link[meta]{metacont}}).
#'
#' @return A list with \code{threshold_user} (user-facing value),
#'   \code{threshold_scale} (one of \code{"ratio"}, \code{"te_scale"},
#'   \code{"ard"}) and \code{source} (\code{"core_grade_6"} or
#'   \code{"package_convention"} — where the number comes from).
#'
#'   For binary ratio measures (OR / RR / HR) the \strong{first candidate is
#'   the absolute one}: \code{threshold_user} / \code{threshold_scale} describe
#'   an absolute risk difference of 0.05 (50 per 1,000), the same list is
#'   repeated under \code{threshold_absolute}, and the ratio-scale fallback is
#'   available under \code{threshold_ratio}. This ordering follows the source:
#'   Core GRADE 1, 6 and 7 contain no ratio-scale MID at all, and every binary
#'   MID they discuss is on the absolute scale (e.g. Core GRADE 7 lists MIDs
#'   "associated with mortality of 1\%, stroke of 2\%, myocardial infarction of
#'   3\%, and serious gastrointestinal bleeding of 5\%"; Core GRADE 2 discusses
#'   "an MID of 5 deaths per" 1000).
#'
#'   Returns \code{NULL} if the effect measure is unrecognized.
#'
#' @section Where these numbers come from:
#' \describe{
#'   \item{SMD 0.20 (\code{source = "core_grade_6"})}{The only default with a
#'     source. Core GRADE 6 does cite it — "an SMD of 0.2 is the threshold for
#'     a small and important effect" — but immediately qualifies it, verbatim:
#'     "clinicians may be appropriately sceptical of this threshold, which is
#'     limited by large variability in the methods investigators use to
#'     calculate the SMD".}
#'   \item{Everything else (\code{source = "package_convention"})}{OR 1.25,
#'     RR 1.20, HR 1.20, RoM 1.10, MD 0.20 \eqn{\times} pooled SD and ARD 0.05
#'     have \strong{no basis in the Core GRADE series}. They exist only so that
#'     an input field can be pre-filled.}
#' }
#'
#' @section Why a single default conflicts with Core GRADE:
#' \itemize{
#'   \item \strong{No ratio-scale MIDs exist in the source.} Core GRADE 1, 6
#'     and 7 give no example of a MID on a ratio scale; binary MIDs are always
#'     absolute (per 1000 or percent). A ratio-scale default is therefore an
#'     extrapolation by pmatools.
#'   \item \strong{The MID belongs to the outcome, not to the effect measure.}
#'     Core GRADE 7, verbatim: "MIDs associated with mortality of 1\%, stroke of
#'     2\%, myocardial infarction of 3\%, and serious gastrointestinal bleeding
#'     of 5\% reflect the gradient of importance across these outcomes." One
#'     default shared by every outcome erases exactly that gradient.
#'   \item \strong{The procedure runs the other way round.} Core GRADE 7 has
#'     users look at the CI first and establish a MID only where the answer
#'     depends on it ("whether the MID for mortality is 2\%, 1\%, or less than
#'     1\%, the CI does not cross the MID threshold ... one need not specify a
#'     single particular value"). Starting from a pre-filled default inverts
#'     that order.
#' }
#'
#' @examples
#' \dontrun{
#' s <- suggest_threshold(m)
#' s$threshold_user   # absolute risk difference for binary outcomes
#' s$source           # "package_convention" -> replace it
#' s$threshold_ratio  # ratio-scale fallback, binary outcomes only
#' }
#'
#' @export
suggest_threshold <- function(meta_obj) {
  sm <- meta_obj$sm
  if (is.null(sm)) return(NULL)

  ard_suggest <- list(threshold_user = 0.05, threshold_scale = "ard",
                      source = "package_convention")

  # Binary ratio measures: the absolute suggestion leads (see @return), with
  # the ratio-scale value kept as a secondary candidate.
  binary_ratio <- function(ratio_value) {
    c(ard_suggest,
      list(
        threshold_absolute = ard_suggest,
        threshold_ratio    = list(threshold_user  = ratio_value,
                                  threshold_scale = "ratio",
                                  source          = "package_convention")
      ))
  }

  switch(sm,
    "OR"  = binary_ratio(1.25),
    "RR"  = binary_ratio(1.20),
    "HR"  = binary_ratio(1.20),
    "RoM" = list(threshold_user = 1.10, threshold_scale = "ratio",
                 source = "package_convention"),
    # "RD" is what metabin() emits for a risk difference; "ARD" is the internal
    # scale name, accepted here so a hand-built list is not silently rejected.
    "RD"  = ard_suggest,
    "ARD" = ard_suggest,
    "SMD" = list(threshold_user = 0.20, threshold_scale = "te_scale",
                 source = "core_grade_6"),
    "MD"  = {
      sd_pooled <- compute_pooled_sd(meta_obj)
      if (is.null(sd_pooled) || is.na(sd_pooled) || sd_pooled <= 0) {
        return(NULL)
      }
      list(threshold_user = 0.20 * sd_pooled, threshold_scale = "te_scale",
           source = "package_convention")
    },
    NULL
  )
}

#' Compute sample-size-weighted pooled SD across studies
#'
#' For continuous-outcome meta-analyses (\code{\link[meta]{metacont}}), returns
#' the pooled standard deviation across studies, sample-size weighted.
#'
#' @param meta_obj A meta object (typically from
#'   \code{\link[meta]{metacont}}).
#'
#' @return A single numeric pooled SD, or \code{NULL} if input data are
#'   insufficient.
#'
#' @details
#' Per-study pooled SD uses Cohen's pooled formula:
#' \deqn{SD_{pooled} = \sqrt{\frac{(n_e - 1) SD_e^2 + (n_c - 1) SD_c^2}{n_e + n_c - 2}}}
#' Across studies, the per-study pooled SDs are averaged with weights equal to
#' the total per-study sample size (\eqn{n_e + n_c}).
#'
#' If \code{sd.e}/\code{sd.c} are unavailable, falls back to
#' \code{weighted.mean(seTE * sqrt(n_total), n_total)}.
#'
#' @export
compute_pooled_sd <- function(meta_obj) {
  n_e  <- meta_obj$n.e
  n_c  <- meta_obj$n.c
  sd_e <- meta_obj$sd.e
  sd_c <- meta_obj$sd.c

  if (!is.null(n_e) && !is.null(n_c) && !is.null(sd_e) && !is.null(sd_c) &&
      length(n_e) == length(sd_e)) {
    sd_per_study <- sqrt(
      ((n_e - 1) * sd_e^2 + (n_c - 1) * sd_c^2) /
      pmax(n_e + n_c - 2, 1)
    )
    weights <- n_e + n_c
    keep <- is.finite(sd_per_study) & is.finite(weights) & weights > 0
    if (any(keep)) {
      return(stats::weighted.mean(sd_per_study[keep], weights[keep]))
    }
  }

  # Fallback: derive from seTE
  seTE <- meta_obj$seTE
  if (!is.null(n_e) && !is.null(n_c) && !is.null(seTE)) {
    n_total <- n_e + n_c
    keep <- is.finite(seTE) & is.finite(n_total) & n_total > 0
    if (any(keep)) {
      # MD: SE ≈ sd_pooled * sqrt(1/n_e + 1/n_c) ≈ sd_pooled * sqrt(4/n_total)
      sd_approx <- seTE[keep] * sqrt(n_total[keep] / 4)
      return(stats::weighted.mean(sd_approx, n_total[keep]))
    }
  }

  NULL
}

#' Convert a user-supplied Threshold to the meta TE scale
#'
#' Internal helper. Translates the user's Threshold input (with its declared
#' scale) into a value on the same scale as \code{meta_obj$TE}. Used by the
#' Inconsistency and Imprecision domains to anchor judgments to a clinical
#' decision Threshold.
#'
#' @param threshold Numeric Threshold value.
#' @param threshold_scale One of \code{"auto"}, \code{"te_scale"},
#'   \code{"ratio"}, or \code{"ard"}.
#' @param sm The effect measure from \code{meta_obj$sm}, used when
#'   \code{threshold_scale = "auto"} and to decide whether an
#'   \code{"ard"} Threshold needs conversion to the ratio scale.
#' @param threshold_baseline Optional baseline (control-arm) risk as a
#'   proportion in (0, 1). Only used when \code{threshold_scale = "ard"} and
#'   \code{sm} is a ratio measure (OR / RR / HR / RoM); see Details.
#' @param meta_obj Optional meta object. When \code{threshold_baseline} is
#'   \code{NULL}, the pooled control event rate
#'   (\eqn{\sum event_c / \sum n_c}) is used as the baseline risk fallback.
#'
#' @return A list with:
#'   \describe{
#'     \item{threshold_internal}{Numeric on the TE scale (log scale for ratio
#'       measures).}
#'     \item{threshold_kind}{The resolved scale (useful for downstream
#'       branching like ARD-vs-ratio in OIS).}
#'     \item{threshold_ard}{The raw absolute risk difference. Non-\code{NULL}
#'       only when an \code{"ard"} Threshold was converted to the ratio scale.}
#'     \item{threshold_note}{Human-readable conversion note (eg,
#'       \code{"Absolute threshold 50 per 1000 at baseline risk 180 per 1000
#'       (equivalent RR 1.28)"}). Non-\code{NULL} only on ARD-to-ratio
#'       conversion.}
#'     \item{threshold_baseline}{The baseline risk actually used for the
#'       conversion. Non-\code{NULL} only on ARD-to-ratio conversion.}
#'   }
#'
#' @details
#' When \code{threshold_scale = "ard"} and \code{sm} is a ratio measure, the
#' ARD Threshold is converted to an equivalent ratio at the baseline risk
#' \eqn{p_0} (from \code{threshold_baseline}, else the pooled control event
#' rate of \code{meta_obj}; an error is raised if neither is available):
#' \itemize{
#'   \item RR: \eqn{T = (p_0 + ARD) / p_0}
#'   \item OR: \eqn{T = odds(p_0 + ARD) / odds(p_0)} with
#'     \eqn{odds(p) = p / (1 - p)}
#'   \item HR / RoM: approximated by the RR formula. Caveat: the RR
#'     approximation for HR is accurate only for low event rates / short
#'     follow-up; interpret with care.
#' }
#' \code{threshold_internal} is then \eqn{\log T}. For non-ratio effect
#' measures, \code{threshold_scale = "ard"} keeps the previous pass-through
#' behaviour (\code{threshold_internal = threshold}).
#'
#' @keywords internal
threshold_to_te_scale <- function(threshold, threshold_scale = "auto", sm = NULL,
                                  threshold_baseline = NULL, meta_obj = NULL) {
  if (is.null(threshold) || is.na(threshold)) {
    return(list(threshold_internal = NULL, threshold_kind = NULL,
                threshold_ard = NULL, threshold_note = NULL,
                threshold_baseline = NULL))
  }

  if (!is.numeric(threshold) || length(threshold) != 1) {
    rlang::abort("threshold must be a single numeric value or NULL.")
  }

  scale <- if (identical(threshold_scale, "auto")) {
    if (is.null(sm)) {
      rlang::abort("threshold_scale = 'auto' requires meta_obj$sm to be set.")
    }
    switch(sm,
      "OR"  = "ratio",
      "RR"  = "ratio",
      "HR"  = "ratio",
      "RoM" = "ratio",
      "RD"  = "ard",
      "ARD" = "ard",
      "SMD" = "te_scale",
      "MD"  = "te_scale",
      rlang::abort(sprintf(
        "Cannot auto-detect threshold_scale for sm = '%s'. Specify threshold_scale explicitly.", sm))
    )
  } else {
    threshold_scale
  }

  if (!scale %in% c("te_scale", "ratio", "ard")) {
    rlang::abort("threshold_scale must be one of 'auto', 'te_scale', 'ratio', 'ard'.")
  }

  # ARD Threshold with a ratio effect measure: convert to the ratio scale at
  # the baseline risk (previously a silent pass-through, which compared a raw
  # ARD against log-ratio TEs).
  if (scale == "ard" && !is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM")) {
    return(.ard_threshold_to_ratio(threshold, sm, threshold_baseline, meta_obj))
  }

  threshold_internal <- switch(scale,
    "te_scale" = threshold,
    "ratio"    = log(threshold),
    "ard"      = threshold
  )

  list(threshold_internal = threshold_internal, threshold_kind = scale,
       threshold_ard = NULL, threshold_note = NULL, threshold_baseline = NULL)
}

#' Convert an absolute risk difference Threshold to the log-ratio scale
#'
#' @param ard Positive absolute risk difference (proportion, eg 0.05).
#' @param sm Ratio effect measure ("OR", "RR", "HR", "RoM").
#' @param threshold_baseline Baseline (control-arm) risk in (0, 1) or NULL.
#' @param meta_obj meta object used for the pooled-CER fallback, or NULL.
#' @return Same list structure as \code{threshold_to_te_scale()}.
#' @keywords internal
#' @noRd
.ard_threshold_to_ratio <- function(ard, sm, threshold_baseline = NULL,
                                    meta_obj = NULL) {
  if (!is.finite(ard) || ard <= 0) {
    rlang::abort(paste0(
      "threshold_scale = 'ard' with sm = '", sm, "' requires a positive ",
      "absolute risk difference expressed as a proportion ",
      "(e.g., 0.05 for 50 per 1,000)."
    ))
  }

  # Resolve baseline risk: explicit threshold_baseline > pooled control event
  # rate from the meta object > actionable error.
  p0 <- NULL
  if (!is.null(threshold_baseline)) {
    if (!is.numeric(threshold_baseline) || length(threshold_baseline) != 1 ||
        !is.finite(threshold_baseline) ||
        threshold_baseline <= 0 || threshold_baseline >= 1) {
      rlang::abort(paste0(
        "threshold_baseline must be a single control-arm risk strictly ",
        "between 0 and 1 (e.g., 0.18 for 180 per 1,000)."
      ))
    }
    p0 <- threshold_baseline
  } else if (!is.null(meta_obj)) {
    cer <- tryCatch(.compute_control_risk(meta_obj, method = "simple"),
                    error = function(e) NULL)
    if (!is.null(cer) && is.finite(cer) && cer > 0 && cer < 1) {
      p0 <- cer
    }
  }
  if (is.null(p0)) {
    rlang::abort(paste0(
      "An absolute (ARD) threshold with sm = '", sm, "' requires a baseline ",
      "(control-arm) risk to convert it to the ratio scale. Supply ",
      "threshold_baseline (a proportion in (0, 1), e.g., 0.18 for 180 per ",
      "1,000), or use a meta-analysis with control-arm event data ",
      "(event.c / n.c) so the pooled control event rate can be used."
    ))
  }

  p1 <- p0 + ard
  if (p1 >= 1) {
    rlang::abort(sprintf(paste0(
      "threshold (ARD = %g) plus baseline risk (%g) implies an event rate ",
      ">= 1 (%g). Use a smaller ARD threshold or baseline risk."),
      ard, p0, p1
    ))
  }

  t_ratio <- if (identical(sm, "OR")) {
    (p1 / (1 - p1)) / (p0 / (1 - p0))
  } else {
    # RR exact; HR and RoM approximated as RR (see Details / caveat).
    p1 / p0
  }

  approx_str <- if (sm %in% c("HR", "RoM")) {
    sprintf("; %s approximated as RR", sm)
  } else {
    ""
  }
  note <- sprintf(
    "Absolute threshold %g per 1000 at baseline risk %g per 1000 (equivalent %s %.2f%s)",
    1000 * ard, 1000 * p0, sm, t_ratio, approx_str
  )

  list(
    threshold_internal = log(t_ratio),
    threshold_kind     = "ard",
    threshold_ard      = ard,
    threshold_note     = note,
    threshold_baseline = p0
  )
}

# ==========================================================================
# VERSION STAMP FOR PROVENANCE LINES IN EXPORTED ARTIFACTS
# ==========================================================================
# A host that vendors the pmatools sources (source()s R/*.R instead of
# installing the package) has no installed DESCRIPTION, so
# utils::packageVersion("pmatools") always errors there. Such a host sets
#   options(pmatools.version_stamp = "0.5.1")
# so the exported bundles report the version of the sources it vendored.
# The option is ignored whenever the package is genuinely installed.

#' Version stamp used when pmatools is not installed
#'
#' Returns `getOption("pmatools.version_stamp")` suffixed with
#' `" (vendored)"` when it is a single non-empty string. Anything else
#' (unset, `NULL`, non-character, length != 1, `NA`, blank) yields
#' `"(vendored; version unknown)"`.
#'
#' @param stamp The option value. Exposed as an argument so that the
#'   validation can be tested without touching the session options.
#' @return Character scalar.
#' @keywords internal
#' @noRd
.vendored_version_stamp <- function(stamp = getOption("pmatools.version_stamp")) {
  ok <- is.character(stamp) && length(stamp) == 1L &&
    !is.na(stamp) && nzchar(trimws(stamp))
  if (ok) paste0(trimws(stamp), " (vendored)") else "(vendored; version unknown)"
}

#' Resolve the pmatools version for provenance stamps
#'
#' Precedence: the installed package version, then the
#' `pmatools.version_stamp` option, then `"(vendored; version unknown)"`.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
.pmatools_version <- function() {
  tryCatch(as.character(utils::packageVersion("pmatools")),
           error = function(e) .vendored_version_stamp())
}
