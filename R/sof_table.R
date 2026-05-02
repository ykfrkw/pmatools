# sof_table.R - Summary of Findings flextable

#' Generate a Summary of Findings (SoF) table as a flextable
#'
#' @param x A \code{pmatools} object (from \code{\link{grade_meta}}).
#' @param palette Colour palette for the certainty cell.
#'   \code{"pastel"} (default) uses soft backgrounds with coloured text.
#'   \code{"classic"} uses saturated backgrounds with white text.
#' @param per Denominator for event rate columns. \code{1000} (default) or
#'   \code{100}. Controls the scale of the "Control rate" and "Exp. rate"
#'   columns.
#' @param prediction Logical. If \code{TRUE} (default \code{FALSE}), the
#'   Effect column also shows the 95 percent prediction interval on a second line,
#'   provided the meta object was run with \code{prediction = TRUE}.
#' @param convert_smd_to_or (v0.2) Logical. If \code{TRUE} and the meta
#'   object uses \code{sm = "SMD"} or \code{"MD"}, the Control rate /
#'   Exp. rate columns display dichotomised event rates derived via Chinn's
#'   formula (\eqn{\log OR = SMD \times \pi / \sqrt{3}}). Requires
#'   \code{baseline_risk} (numeric in (0,1)) representing the proportion
#'   of control patients meeting the threshold of clinical interest.
#' @param baseline_risk Numeric in (0,1), required when
#'   \code{convert_smd_to_or = TRUE}. Otherwise inherited from the
#'   pmatools object.
#' @param threshold_label (v0.2) Optional free-text label for the
#'   dichotomisation threshold (e.g., \code{">=50 percent reduction in PHQ-9"}).
#'   Shown in the table footer when \code{convert_smd_to_or = TRUE}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{flextable} object suitable for printing, Word export, etc.
#'
#' @examples
#' \dontrun{
#' g <- grade_meta(m, study_design = "RCT", rob = "some")
#' sof_table(g)
#' sof_table(g, per = 100)
#' sof_table(g, prediction = TRUE)
#' sof_table(g, palette = "classic")
#' flextable::save_as_docx(sof_table(g), path = "sof.docx")
#' }
#'
#' @export
sof_table <- function(x, palette = c("pastel", "classic"),
                      per = 1000, prediction = FALSE,
                      convert_smd_to_or = FALSE,
                      baseline_risk     = NULL,
                      threshold_label   = NULL,
                      ...) {
  if (!inherits(x, "pmatools")) {
    rlang::abort("x must be a pmatools object from grade_meta().")
  }
  palette <- match.arg(palette)
  pal     <- CERTAINTY_PALETTES[[palette]]

  meta_obj <- x$meta

  # v0.2 Chinn conversion path
  chinn_active <- FALSE
  if (isTRUE(convert_smd_to_or)) {
    sm <- meta_obj$sm
    if (is.null(sm) || !sm %in% c("SMD", "MD")) {
      rlang::abort(
        "convert_smd_to_or = TRUE requires meta_obj$sm in c('SMD','MD')."
      )
    }
    if (is.null(baseline_risk) || !is.numeric(baseline_risk) ||
        length(baseline_risk) != 1 || baseline_risk <= 0 || baseline_risk >= 1) {
      rlang::abort(
        "convert_smd_to_or = TRUE requires baseline_risk as a single numeric in (0, 1)."
      )
    }
    chinn_active <- TRUE
  }

  baseline_for_display <- if (chinn_active) baseline_risk else x$baseline_risk

  k           <- meta_obj$k
  n_total     <- .total_n(meta_obj)
  cer_str     <- .format_cer(baseline_for_display, per)
  ier_str     <- if (chinn_active) {
    .format_ier_chinn(meta_obj, baseline_risk, per)
  } else {
    .format_ier(meta_obj, x$baseline_risk, per)
  }
  effect_str  <- .format_effect(meta_obj, x$outcome_type,
                                prediction = prediction)

  certainty_label <- x$certainty
  certainty_sym   <- CERTAINTY_SYMBOLS_UNICODE[[certainty_label]]
  cell_colors     <- pal[[certainty_label]]

  per_str <- format(per, big.mark = ",", scientific = FALSE)
  headers <- c(
    "Outcome", "Studies (k)", "Participants (N)",
    paste0("Control rate\n(per ", per_str, ")"),
    paste0("Exp. rate\n(per ", per_str, ")"),
    "Effect (95% CI)",
    "Certainty of evidence"
  )

  certainty_cell <- paste0(certainty_label, "\n", certainty_sym)

  df <- data.frame(
    col1 = x$outcome_name,
    col2 = as.character(k),
    col3 = if (is.na(n_total)) "NR" else format(n_total, big.mark = ","),
    col4 = cer_str,
    col5 = ier_str,
    col6 = effect_str,
    col7 = certainty_cell,
    stringsAsFactors = FALSE
  )
  names(df) <- headers

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, .list = stats::setNames(as.list(headers), headers))
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 2:3, align = "center", part = "body")

  cert_col <- headers[7]
  ft <- flextable::bg(ft,    j = cert_col, bg    = cell_colors$bg,   part = "body")
  ft <- flextable::color(ft, j = cert_col, color = cell_colors$text, part = "body")
  ft <- flextable::bold(ft,  j = cert_col, part = "body")
  ft <- flextable::align(ft, j = cert_col, align = "center", part = "body")

  ft <- flextable::width(ft, j = 1, width = 1.4)
  ft <- flextable::width(ft, j = 2, width = 0.6)
  ft <- flextable::width(ft, j = 3, width = 0.9)
  ft <- flextable::width(ft, j = 4, width = 1.3)
  ft <- flextable::width(ft, j = 5, width = 1.4)
  ft <- flextable::width(ft, j = 6, width = 1.6)
  ft <- flextable::width(ft, j = 7, width = 1.4)

  ft <- flextable::bg(ft,    bg = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft,  part = "header")

  pi_note <- if (prediction) " PrI = 95 percent prediction interval." else ""
  chinn_note <- if (chinn_active) {
    paste0(
      " Continuous outcome dichotomised via Chinn's formula ",
      "(log OR = SMD x pi/sqrt(3)); control event rate user-specified",
      if (!is.null(threshold_label) && nzchar(threshold_label)) {
        paste0(" (threshold: ", threshold_label, ")")
      } else "",
      "."
    )
  } else ""
  footnote_text <- paste0(
    "GRADE certainty: ", certainty_label, ". ",
    "Assessment based on BMJ 2025 Core GRADE series (Guyatt et al.). ",
    "CI = confidence interval.", pi_note, " ",
    "Exp. rate = experimental (intervention) arm event rate computed from ",
    "baseline risk and pooled relative effect.",
    chinn_note
  )
  ft <- flextable::add_footer_lines(ft, values = footnote_text)
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::color(ft, color = "#555555", part = "footer")

  ft
}

# --------------------------------------------------------------------------
# Helpers (shared with grade_table.R via package namespace)
# --------------------------------------------------------------------------

.total_n <- function(meta_obj) {
  n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA
  n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA
  if (!is.na(n_e) && !is.na(n_c)) return(n_e + n_c)
  if (!is.null(meta_obj$n)) return(sum(meta_obj$n, na.rm = TRUE))
  NA_integer_
}

.format_effect <- function(meta_obj, outcome_type, prediction = FALSE) {
  sm  <- meta_obj$sm
  est <- meta_obj$TE.random
  lo  <- meta_obj$lower.random
  hi  <- meta_obj$upper.random

  if (is.null(est) || is.na(est)) return("NR")

  if (outcome_type == "relative" && !is.null(sm) &&
      sm %in% c("RR", "OR", "HR", "IRR")) {
    est <- exp(est); lo <- exp(lo); hi <- exp(hi)
    s <- sprintf("%s %.2f (%.2f; %.2f)", sm, est, lo, hi)
  } else {
    s <- sprintf("%s %.2f (%.2f; %.2f)",
                 if (is.null(sm)) "Effect" else sm, est, lo, hi)
  }

  if (prediction) {
    pi_lo <- meta_obj$lower.predict
    pi_hi <- meta_obj$upper.predict
    if (!is.null(pi_lo) && !is.null(pi_hi) &&
        !is.na(pi_lo) && !is.na(pi_hi)) {
      if (outcome_type == "relative" && !is.null(sm) &&
          sm %in% c("RR", "OR", "HR", "IRR")) {
        pi_lo <- exp(pi_lo); pi_hi <- exp(pi_hi)
      }
      s <- paste0(s, sprintf("\nPrI (%.2f; %.2f)", pi_lo, pi_hi))
    }
  }

  s
}

# Control event rate: baseline_risk displayed per 'per' units (no CI)
.format_cer <- function(baseline_risk, per = 1000) {
  if (is.null(baseline_risk)) return("-")
  per_str <- format(per, big.mark = ",", scientific = FALSE)
  sprintf("%d per %s", round(baseline_risk * per), per_str)
}

# Experimental (intervention) event rate: derived from baseline + relative effect
.format_ier <- function(meta_obj, baseline_risk, per = 1000) {
  if (is.null(baseline_risk)) return("-")
  sm <- meta_obj$sm
  if (is.null(sm) || !sm %in% c("RR", "OR", "HR", "IRR")) return("-")

  p1_est <- .p1(baseline_risk, meta_obj$TE.random,    sm)
  p1_lo  <- .p1(baseline_risk, meta_obj$lower.random, sm)
  p1_hi  <- .p1(baseline_risk, meta_obj$upper.random, sm)

  if (is.null(p1_est)) return("-")

  per_str <- format(per, big.mark = ",", scientific = FALSE)
  sprintf("%d per %s\n(%d; %d)",
          round(p1_est * per), per_str,
          round(p1_lo  * per),
          round(p1_hi  * per))
}

# Experimental rate via Chinn (SMD/MD -> OR -> p1)
.format_ier_chinn <- function(meta_obj, baseline_risk, per = 1000) {
  if (is.null(baseline_risk)) return("-")
  est <- meta_obj$TE.random
  lo  <- meta_obj$lower.random
  hi  <- meta_obj$upper.random
  if (is.null(est) || is.na(est)) return("-")

  conv <- chinn_smd_to_or(est, ci_lower = lo, ci_upper = hi)
  log_or_est <- log(conv$or)
  log_or_lo  <- log(conv$or_lower)
  log_or_hi  <- log(conv$or_upper)

  p1_est <- .p1(baseline_risk, log_or_est, "OR")
  p1_lo  <- .p1(baseline_risk, log_or_lo,  "OR")
  p1_hi  <- .p1(baseline_risk, log_or_hi,  "OR")

  if (is.null(p1_est)) return("-")

  per_str <- format(per, big.mark = ",", scientific = FALSE)
  sprintf("%d per %s\n(%d; %d)",
          round(p1_est * per), per_str,
          round(p1_lo  * per),
          round(p1_hi  * per))
}

# Compute experimental arm event rate from log-scale relative effect
.p1 <- function(p0, log_re, sm) {
  re <- exp(log_re)
  if (sm %in% c("RR", "HR", "IRR")) return(min(1, max(0, p0 * re)))
  if (sm == "OR") {
    p1 <- p0 * re / (1 + p0 * (re - 1))
    return(min(1, max(0, p1)))
  }
  NULL
}
