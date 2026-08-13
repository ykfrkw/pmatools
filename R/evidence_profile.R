# evidence_profile.R - Evidence Profile flextable (Core GRADE series)
#
# Single-row, single-outcome layout matching the BMJ 2025 Core GRADE
# series Evidence Profile presentation:
#
#   Outcome | No of studies (N) | Design | Risk of bias | Inconsistency |
#                       | Indirectness | Imprecision |
#                       | Other considerations | Quality of evidence

#' Evidence Profile flextable (Core GRADE series) for a single outcome
#'
#' @param grade A `pmatools` object from \code{\link{grade_meta}}.
#' @param palette Colour palette for the certainty cell.
#' @param study_design Design label. Defaults to "randomized trials".
#' @param other_text Optional free-text "Other considerations" note.
#' @param other_downgrade Integer 0/-1/-2; extra downgrade applied on top
#'   of the auto-domain calculations.
#'
#' @return A `flextable`.
#'
#' @export
evidence_profile <- function(grade,
                             palette         = c("pastel", "classic"),
                             study_design    = NULL,
                             other_text      = NULL,
                             other_downgrade = 0L) {
  if (.is_not_reported(grade)) {
    rlang::abort(paste0(
      "evidence_profile: 'grade' must be a pmatools object; an evidence ",
      "profile rates five domains against a body of evidence, and a ",
      "not-reported outcome has none. It still appears in the summary of ",
      "findings table via grade_table()."))
  }
  if (!inherits(grade, "pmatools")) {
    rlang::abort("evidence_profile: 'grade' must be a pmatools object.")
  }
  palette <- match.arg(palette)
  pal     <- CERTAINTY_PALETTES[[palette]]

  meta_obj <- grade$meta
  k        <- meta_obj$k %||% NA_integer_
  n_total  <- .total_n(meta_obj)
  studies_str <- if (is.na(n_total)) {
    sprintf("%d", k)
  } else {
    sprintf("%d studies\n(N = %s)", k, format(n_total, big.mark = ","))
  }

  if (is.null(study_design) || !nzchar(study_design)) {
    sd <- grade$study_design %||% "RCT"
    study_design <- if (toupper(sd) == "RCT") "randomized trials" else sd
  }

  d <- grade$domain_assessments
  pick <- function(name) {
    r <- d[d$domain == name, , drop = FALSE]
    if (nrow(r) == 0) {
      list(judgment = "no", notes = NA_character_)
    } else {
      list(judgment = r$judgment[1], notes = r$notes[1])
    }
  }

  rob   <- pick("Risk of bias")
  inco  <- pick("Inconsistency")
  indi  <- pick("Indirectness")
  impr  <- pick("Imprecision")
  pubi  <- pick("Publication bias")

  # The display vocabulary is .grade_level_wording() in R/utils.R, not a
  # second switch() here: the Shiny badges and the override menus read the
  # same function, so the app and this table cannot word a judgment
  # differently.
  fmt_judgment <- function(judgment) .grade_level_wording(judgment)

  fn <- list()
  add_fn <- function(judgment, reason) {
    if (judgment %in% c("no", "")) return("")
    if (is.null(reason) || is.na(reason) || !nzchar(reason)) {
      reason <- "no further detail available"
    }
    fn[[length(fn) + 1L]] <<- reason
    paste0(" [", length(fn), "]")
  }

  # What a domain footnote says. For a rated-down domain that recorded the
  # numbers behind its judgment, the structured facts say it better than the
  # first sentence of the notes, which is where the flowchart step happens to
  # start; the domain name is left off because the Evidence Profile's marker
  # already sits in that domain's own column. Domains with no facts (and the
  # ones that did not rate down) keep the first-sentence behaviour.
  dom_reason <- function(name, judgment, notes) {
    if (!as.character(judgment) %in% c("no", "")) {
      body <- .domain_fact_body((grade$domain_facts %||% list())[[name]])
      if (!is.null(body)) return(body)
    }
    .first_sentence(notes)
  }

  rob_str  <- paste0(fmt_judgment(rob$judgment),
                     add_fn(rob$judgment,
                            dom_reason("Risk of bias", rob$judgment, rob$notes)))
  inco_str <- paste0(fmt_judgment(inco$judgment),
                     add_fn(inco$judgment,
                            dom_reason("Inconsistency", inco$judgment, inco$notes)))
  indi_str <- paste0(fmt_judgment(indi$judgment),
                     add_fn(indi$judgment,
                            dom_reason("Indirectness", indi$judgment, indi$notes)))
  impr_str <- paste0(fmt_judgment(impr$judgment),
                     add_fn(impr$judgment,
                            dom_reason("Imprecision", impr$judgment, impr$notes)))

  other_parts <- character(0)
  pubias_qual_note <- .pubias_qualitative_note(grade)
  if (pubi$judgment != "no") {
    other_parts <- c(other_parts,
                     paste0("publication bias suspected",
                            add_fn(pubi$judgment, .first_sentence(pubi$notes))))
  } else if (!is.null(pubias_qual_note)) {
    # Not formally assessed -> make the limitation visible with a footnote
    # prompting a qualitative judgment (see domain_pubias.R)
    fn[[length(fn) + 1L]] <- pubias_qual_note
    other_parts <- c(other_parts,
                     paste0("publication bias not formally assessed",
                            " [", length(fn), "]"))
  }
  has_user_other <- !is.null(other_text) && nzchar(trimws(other_text %||% ""))
  user_dg <- as.integer(other_downgrade %||% 0L)
  if (is.na(user_dg)) user_dg <- 0L
  if (has_user_other) {
    user_note <- trimws(other_text)
    if (user_dg < 0) {
      other_parts <- c(other_parts,
                       paste0(user_note, add_fn("serious", user_note)))
    } else {
      other_parts <- c(other_parts, user_note)
    }
  }
  other_str <- if (length(other_parts) == 0) "none" else paste(other_parts, collapse = "; ")

  total_auto_dg <- sum(d$downgrade %||% 0)
  effective_dg  <- total_auto_dg + min(0L, user_dg)
  cert_score   <- max(1L, 4L + effective_dg)
  cert_label   <- c("Very Low", "Low", "Moderate", "High")[cert_score]
  cert_symbol  <- CERTAINTY_SYMBOLS_UNICODE[[cert_label]] %||% ""
  cert_str     <- paste0(cert_symbol, " ", cert_label)
  cert_colors  <- pal[[cert_label]]

  headers <- c(
    "Outcome", "No of studies (N)", "Design",
    "Risk of bias", "Inconsistency", "Indirectness",
    "Imprecision", "Other considerations", "Certainty of evidence"
  )

  df <- data.frame(
    c1 = grade$outcome_name,
    c2 = studies_str,
    c3 = study_design,
    c4 = rob_str,
    c5 = inco_str,
    c6 = indi_str,
    c7 = impr_str,
    c8 = other_str,
    c9 = cert_str,
    stringsAsFactors = FALSE
  )
  names(df) <- headers

  ft <- flextable::flextable(df)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")

  ft <- flextable::bg(ft,    bg    = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white",   part = "header")
  ft <- flextable::bold(ft,                     part = "header")
  ft <- flextable::align(ft, align = "center",  part = "header")

  ft <- flextable::align(ft, j = 1L,    align = "left",   part = "body")
  ft <- flextable::align(ft, j = 2L,    align = "center", part = "body")
  ft <- flextable::align(ft, j = 3:8,   align = "left",   part = "body")
  ft <- flextable::align(ft, j = 9L,    align = "center", part = "body")
  ft <- flextable::valign(ft, valign  = "top", part = "body")

  ft <- flextable::bg(ft,    j = 9L, bg    = cert_colors$bg,   part = "body")
  ft <- flextable::color(ft, j = 9L, color = cert_colors$text, part = "body")
  ft <- flextable::bold(ft,  j = 9L,                           part = "body")

  widths <- c(1.50, 0.95, 0.95, 1.00, 1.05, 1.00, 1.05, 1.40, 1.10)
  for (j in seq_along(widths)) ft <- flextable::width(ft, j = j, width = widths[j])

  if (length(fn) > 0) {
    fn_lines <- vapply(seq_along(fn),
                       function(i) sprintf("[%d] %s", i, fn[[i]]),
                       character(1))
    for (line in fn_lines) {
      ft <- flextable::add_footer_lines(ft, values = line)
    }
  }

  thr_note <- grade$threshold_note %||% NULL
  if (!is.null(thr_note) && !is.na(thr_note) && nzchar(thr_note)) {
    ft <- flextable::add_footer_lines(
      ft,
      values = paste0("Threshold: ", thr_note, ".")
    )
  }

  ft <- flextable::add_footer_lines(
    ft,
    values = paste0(
      "Evidence Profile (Core GRADE series). Certainty levels: ",
      paste(CERTAINTY_SYMBOLS_UNICODE, names(CERTAINTY_SYMBOLS_UNICODE),
            sep = " ", collapse = "  "),
      ". Based on the BMJ 2025 Core GRADE series (Guyatt et al.); ",
      "not an official GRADE Working Group assessment."
    )
  )

  ft <- flextable::fontsize(ft, size = 8,        part = "footer")
  ft <- flextable::color(ft,    color = "#555555", part = "footer")
  ft
}

.first_sentence <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return("")
  parts <- strsplit(s, " | ", fixed = TRUE)[[1]]
  out <- parts[1]
  if (nchar(out) > 180) out <- paste0(substr(out, 1, 177), "...")
  out
}
