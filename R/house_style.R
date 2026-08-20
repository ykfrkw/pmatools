# house_style.R - what this package prints around its numbers
#
# Split out of utils.R. The certainty glyphs and colours, the one font every
# exported table is built in, the footer styling, the form the Core GRADE papers
# are cited in, their DOIs, the standing disclaimer, and the version stamp a
# provenance line reports. All of it is presentation shared by every renderer,
# and it is in one file because that is what stops it drifting: eight footnotes
# once worded the same disclaimer four ways.
#
# A new helper belongs here when it fixes how something is worded, coloured or
# stamped for the reader, rather than what it says.
#
# .PMA_CORE_GRADE_FOOTNOTE is built at load time by calling .core_grade_ref(),
# so the two stay together in this file: the app source()s R/_pmatools/*.R in
# whatever order list.files() returns, and splitting them would depend on it.

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
