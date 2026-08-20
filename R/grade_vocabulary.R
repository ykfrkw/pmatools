# grade_vocabulary.R - the words a domain judgment is stored and shown in
#
# Split out of utils.R, which had become the package's grab-bag. This file owns
# the four judgment levels, the spellings accepted for each, the wording a
# judgment is displayed in, and the four certainty ratings a run of judgments
# adds up to. Nothing here decides a judgment: the domain assessors do that and
# hand their verdict back in this vocabulary.
#
# A new helper belongs here when it decides how a judgment or a certainty rating
# is spelled, validated or worded -- not when it decides how one is drawn
# (R/house_style.R), nor what it means for a domain row (R/domain_row.R).

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
