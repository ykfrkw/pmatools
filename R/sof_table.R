# sof_table.R - Summary of Findings flextable

#' Generate a Summary of Findings (SoF) table as a flextable
#'
#' @param x A \code{pmatools} object (from \code{\link{grade_meta}}).
#' @param palette Color palette for the certainty cell.
#'   \code{"pastel"} (default) uses soft backgrounds with colored text.
#'   \code{"classic"} uses saturated backgrounds with white text.
#' @param per Denominator for event rate columns. \code{1000} (default) or
#'   \code{100}. Controls the scale of the "Risk with control" and
#'   "Risk with intervention" columns.
#' @param prediction Logical. If \code{TRUE} (default \code{FALSE}), the
#'   Effect column also shows the 95 percent prediction interval on a second line,
#'   provided the meta object was run with \code{prediction = TRUE}.
#' @param convert_smd_to_or (v0.2) Logical. If \code{TRUE} and the meta
#'   object uses \code{sm = "SMD"} or \code{"MD"}, the "Risk with control" /
#'   "Risk with intervention" columns display dichotomised event rates derived via Chinn's
#'   formula (\eqn{\log OR = SMD \times \pi / \sqrt{3}}). Requires
#'   \code{baseline_risk} (numeric in (0,1)) representing the proportion
#'   of control patients meeting the threshold of clinical interest.
#' @param baseline_risk Numeric in (0,1), required when
#'   \code{convert_smd_to_or = TRUE}. Otherwise inherited from the
#'   pmatools object.
#' @param threshold_label (v0.2) Optional free-text label for the
#'   dichotomisation threshold (e.g., \code{">=50 percent reduction in PHQ-9"}).
#'   Shown in the table footer when \code{convert_smd_to_or = TRUE}.
#' @param chinn_invert Logical (default \code{FALSE}). Flips the SMD sign
#'   before applying Chinn's formula so that a negative-is-better SMD (e.g.,
#'   symptom severity reduction) yields OR > 1 in the dichotomised rate
#'   columns. Only relevant when \code{convert_smd_to_or = TRUE}.
#' @param label_intervention,label_control Arm labels used in the
#'   "Risk with ..." column headers (GRADEpro vocabulary), e.g.
#'   \code{label_intervention = "CBT-I"}, \code{label_control = "placebo"}.
#'   Defaults are \code{"intervention"} and \code{"control"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{flextable} object suitable for printing, Word export, etc.
#'
#' @examples
#' \dontrun{
#' g <- grade_meta(m, study_design = "RCT", rob = "some",
#'                 rob_rationale = "RoB2 consensus: some concerns from missing outcome data")
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
                      chinn_invert      = FALSE,
                      label_intervention = "intervention",
                      label_control      = "control",
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
    .format_ier_chinn(meta_obj, baseline_risk, per, invert = isTRUE(chinn_invert))
  } else {
    .format_ier(meta_obj, x$baseline_risk, per)
  }
  # Asterisk-mark CER/EER when Chinn dichotomisation is active
  if (chinn_active) {
    if (cer_str != "-") cer_str <- paste0(cer_str, " *")
    if (ier_str != "-") ier_str <- paste0(ier_str, " *")
  }
  effect_str  <- .format_effect(meta_obj, x$outcome_type,
                                prediction = prediction)

  certainty_label <- x$certainty
  certainty_sym   <- CERTAINTY_SYMBOLS_UNICODE[[certainty_label]]
  cell_colors     <- pal[[certainty_label]]

  per_str <- format(per, big.mark = ",", scientific = FALSE)
  headers <- c(
    "Outcome",
    "No. of participants\n(studies)",
    paste0("Risk with ", label_control, "\n(per ", per_str, ")"),
    paste0("Risk with ", label_intervention, "\n(per ", per_str, ")"),
    .effect_header(meta_obj$sm),
    "Certainty of the evidence\n(Core GRADE series)"
  )

  certainty_cell <- paste0(certainty_label, "\n", certainty_sym)

  df <- data.frame(
    col1 = x$outcome_name,
    col2 = .n_participants_studies(k, n_total, x$study_design),
    col3 = cer_str,
    col4 = ier_str,
    col5 = effect_str,
    col6 = certainty_cell,
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
  ft <- flextable::align(ft, j = 2, align = "center", part = "body")

  cert_col <- headers[6]
  ft <- flextable::bg(ft,    j = cert_col, bg    = cell_colors$bg,   part = "body")
  ft <- flextable::color(ft, j = cert_col, color = cell_colors$text, part = "body")
  ft <- flextable::bold(ft,  j = cert_col, part = "body")
  ft <- flextable::align(ft, j = cert_col, align = "center", part = "body")

  ft <- flextable::width(ft, j = 1, width = 1.4)
  ft <- flextable::width(ft, j = 2, width = 1.2)
  ft <- flextable::width(ft, j = 3, width = 1.3)
  ft <- flextable::width(ft, j = 4, width = 1.4)
  ft <- flextable::width(ft, j = 5, width = 1.5)
  ft <- flextable::width(ft, j = 6, width = 1.5)

  ft <- flextable::bg(ft,    bg = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft,  part = "header")

  pi_note <- if (prediction) " PrI = 95 percent prediction interval." else ""

  base_note <- paste0(
    "Certainty rating (Core GRADE series): ", certainty_label, ". ",
    "Assessment based on BMJ 2025 Core GRADE series (Guyatt et al.); ",
    "not an official GRADE Working Group assessment. ",
    "CI = confidence interval.", pi_note, " ",
    "Intervention rate (Risk with ", label_intervention, ") = ",
    "intervention-arm event rate computed from baseline risk and pooled ",
    "relative effect."
  )
  ft <- flextable::add_footer_lines(ft, values = base_note)

  # Risk-of-bias analysis set (Core GRADE 4 Fig 2). A refit silently changes
  # every number in this table, so it must always be stated; the unapplied
  # recommendation is stated too, so the reader knows the shown estimate is
  # not the one the flowchart points at.
  rob_set_note <- .rob_analysis_set_note(x)
  if (!is.null(rob_set_note)) {
    ft <- flextable::add_footer_lines(ft, values = rob_set_note)
  }

  # Publication bias not formally assessed -> prominent qualitative-judgment
  # footnote (see domain_pubias.R)
  pubias_qual_note <- .pubias_qualitative_note(x)
  if (!is.null(pubias_qual_note)) {
    ft <- flextable::add_footer_lines(
      ft, values = paste0("Publication bias: ", pubias_qual_note)
    )
  }

  # Chinn-specific footnote with explicit '*' link and citations
  if (chinn_active) {
    invert_str <- if (isTRUE(chinn_invert)) {
      " (OR direction inverted: OR > 1 = treatment better)"
    } else {
      " (OR direction as given: positive SMD -> OR > 1)"
    }
    threshold_str <- if (!is.null(threshold_label) && nzchar(threshold_label)) {
      paste0(" Threshold definition: ", threshold_label, ".")
    } else ""

    chinn_note <- paste0(
      "* Continuous outcome dichotomised via Chinn's formula ",
      "(log OR = SMD x pi / sqrt(3))", invert_str,
      ". Control event rate user-specified.", threshold_str,
      " Recommended reading: ",
      "Chinn S. Stat Med 2000;19:3127-3131. ",
      "doi:10.1002/1097-0258(20001130)19:22<3127::aid-sim784>3.0.co;2-m. ",
      "Heimke F, Furukawa Y, Siafis S, et al. ",
      "BMJ Ment Health 2024;27:e300978. ",
      "doi:10.1136/bmjment-2023-300978."
    )
    ft <- flextable::add_footer_lines(ft, values = chinn_note)
  }

  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::color(ft, color = "#555555", part = "footer")

  ft
}

# --------------------------------------------------------------------------
# Helpers (shared with grade_table.R via package namespace)
# --------------------------------------------------------------------------

# Footer sentence describing which studies the effect estimate rests on.
# Returns NULL for the ordinary "all studies" case.
.rob_analysis_set_note <- function(x) {
  if (isTRUE(x$rob_refit)) {
    k_low  <- x$meta$k
    k_full <- x$meta_full$k %||% k_low
    return(sprintf(paste0(
      "Effect estimate restricted to low risk of bias studies (n = %d of %d) ",
      "per Core GRADE 4 Fig 2."), k_low, k_full))
  }
  if (identical(x$rob_analysis_set, "low_only")) {
    return(paste0(
      "Core GRADE 4 Fig 2 recommends restricting the analysis to low risk of ",
      "bias studies; the effect estimate shown includes all studies ",
      "(rob_refit = FALSE)."
    ))
  }
  NULL
}

# Combined "No of participants (studies)" cell, GRADEpro style:
# "1,234 (12 RCTs)"; falls back to "(12 studies)" when the study design
# is unavailable.
.n_participants_studies <- function(k, n_total, study_design = NULL) {
  k <- as.integer(k)
  design_lbl <- if (is.null(study_design) || length(study_design) != 1L ||
                    is.na(study_design) || !nzchar(study_design)) {
    if (k == 1L) "study" else "studies"
  } else if (toupper(study_design) == "RCT") {
    if (k == 1L) "RCT" else "RCTs"
  } else if (tolower(study_design) %in% c("obs", "observational")) {
    if (k == 1L) "observational study" else "observational studies"
  } else {
    if (k == 1L) "study" else "studies"
  }
  n_str <- if (is.na(n_total)) "NR" else format(n_total, big.mark = ",")
  sprintf("%s (%d %s)", n_str, k, design_lbl)
}

# GRADEpro-style effect column header, by summary measure
.effect_header <- function(sm) {
  if (!is.null(sm) && length(sm) == 1L && !is.na(sm) &&
      sm %in% c("RR", "OR", "HR", "IRR", "RoM")) {
    "Relative effect\n(95% CI)"
  } else if (identical(sm, "MD")) {
    "Mean difference\n(95% CI)"
  } else if (identical(sm, "SMD")) {
    "Standardized mean difference\n(95% CI)"
  } else {
    "Effect\n(95% CI)"
  }
}

.total_n <- function(meta_obj) {
  n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA
  n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA
  if (!is.na(n_e) && !is.na(n_c)) return(n_e + n_c)
  if (!is.null(meta_obj$n)) return(sum(meta_obj$n, na.rm = TRUE))
  NA_integer_
}

# Pooled estimate with model fallback (mirrors domain_imprecision.R /
# domain_rob.R): prefer the random-effects pool when random = TRUE, otherwise
# the common-effect pool; fall back to the other model when the preferred
# one is unavailable (e.g. run_ma(random = FALSE, common = TRUE)).
.pooled_estimate <- function(meta_obj) {
  pick <- function(model) {
    if (model == "random") {
      list(est   = meta_obj$TE.random,
           lower = meta_obj$lower.random,
           upper = meta_obj$upper.random)
    } else {
      list(est   = meta_obj$TE.common,
           lower = meta_obj$lower.common,
           upper = meta_obj$upper.common)
    }
  }
  ok <- function(x) {
    !is.null(x$est) && length(x$est) == 1L && is.finite(x$est)
  }
  primary <- if (isTRUE(meta_obj$random)) "random" else "common"
  out <- pick(primary)
  if (!ok(out)) {
    out <- pick(if (primary == "random") "common" else "random")
  }
  out
}

.format_effect <- function(meta_obj, outcome_type, prediction = FALSE) {
  sm  <- meta_obj$sm
  pooled <- .pooled_estimate(meta_obj)
  est <- pooled$est
  lo  <- pooled$lower
  hi  <- pooled$upper

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

  pooled <- .pooled_estimate(meta_obj)
  if (is.null(pooled$est) || is.na(pooled$est)) return("-")

  p1_est <- .p1(baseline_risk, pooled$est,   sm)
  p1_lo  <- .p1(baseline_risk, pooled$lower, sm)
  p1_hi  <- .p1(baseline_risk, pooled$upper, sm)

  if (is.null(p1_est)) return("-")

  per_str <- format(per, big.mark = ",", scientific = FALSE)
  sprintf("%d per %s\n(%d; %d)",
          round(p1_est * per), per_str,
          round(p1_lo  * per),
          round(p1_hi  * per))
}

# Experimental rate via Chinn (SMD/MD -> OR -> p1)
# `invert = TRUE` flips the SMD sign before applying the formula, so a
# negative-is-better SMD (e.g., depression severity reduction) yields OR > 1.
.format_ier_chinn <- function(meta_obj, baseline_risk, per = 1000, invert = FALSE) {
  if (is.null(baseline_risk)) return("-")
  pooled <- .pooled_estimate(meta_obj)
  est <- pooled$est
  lo  <- pooled$lower
  hi  <- pooled$upper
  if (is.null(est) || is.na(est)) return("-")

  if (isTRUE(invert)) {
    est_eff <- -est
    lo_eff  <- -hi
    hi_eff  <- -lo
  } else {
    est_eff <- est
    lo_eff  <- lo
    hi_eff  <- hi
  }
  conv <- chinn_smd_to_or(est_eff, ci_lower = lo_eff, ci_upper = hi_eff)
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
