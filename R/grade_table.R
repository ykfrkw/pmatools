# grade_table.R — 複数アウトカム GRADE テーブル

#' Summary of Findings table for multiple outcomes
#'
#' @param outcomes A named list of \code{pmatools} objects, one per outcome.
#'   Names become the outcome labels.
#' @param primary Character vector of outcome names that are classified as
#'   primary outcomes. All others are treated as secondary.
#'   If \code{NULL} (default), no grouping header is added.
#' @param palette Colour palette for certainty cells.
#'   \code{"pastel"} (default) uses soft backgrounds with coloured text.
#'   \code{"classic"} uses saturated backgrounds with white text.
#' @param show_domains Logical (default \code{TRUE}). Add domain symbol columns.
#'
#' @return A \code{flextable} object.
#'
#' @examples
#' \dontrun{
#' g1 <- grade_meta(m1, outcome_name = "Depression response")
#' g2 <- grade_meta(m2, outcome_name = "Insomnia remission")
#' grade_table(
#'   list("Depression response" = g1, "Insomnia remission" = g2),
#'   primary = "Depression response"
#' )
#' }
#'
#' @export
grade_table <- function(outcomes,
                        primary      = NULL,
                        palette      = c("pastel", "classic"),
                        show_domains = TRUE,
                        per          = 1000,
                        prediction   = FALSE) {
  if (!is.list(outcomes) || length(outcomes) == 0) {
    rlang::abort("outcomes must be a non-empty named list of pmatools objects.")
  }
  if (!all(vapply(outcomes, inherits, logical(1), "pmatools"))) {
    rlang::abort("All elements of outcomes must be pmatools objects.")
  }

  palette <- match.arg(palette)
  pal     <- CERTAINTY_PALETTES[[palette]]

  # Ensure names exist
  nms <- names(outcomes)
  if (is.null(nms) || any(nms == "")) {
    for (i in seq_along(outcomes)) {
      if (is.null(nms) || nms[i] == "") nms[i] <- outcomes[[i]]$outcome_name
    }
    names(outcomes) <- nms
  }

  # Partition into primary / secondary
  if (!is.null(primary)) {
    prim_nms <- nms[nms %in% primary]
    sec_nms  <- nms[!nms %in% primary]
  } else {
    prim_nms <- character(0)
    sec_nms  <- character(0)
  }

  hdrs  <- .col_headers(show_domains, per)
  ncols <- length(hdrs)

  # Build rows, inserting group-label rows where needed
  all_rows    <- list()
  label_rows  <- integer(0)   # row indices of group-label rows
  outcome_map <- list()        # row index (char) → outcome name
  row_idx <- 0L

  add_label <- function(text) {
    row_idx <<- row_idx + 1L
    r <- as.data.frame(matrix("", 1L, ncols), stringsAsFactors = FALSE)
    r[1L, 1L] <- text
    names(r) <- hdrs
    all_rows[[length(all_rows) + 1L]] <<- r
    label_rows <<- c(label_rows, row_idx)
  }

  add_outcome <- function(nm) {
    row_idx <<- row_idx + 1L
    r <- .build_row(nm, outcomes[[nm]], show_domains, per, prediction)
    names(r) <- hdrs
    all_rows[[length(all_rows) + 1L]] <<- r
    outcome_map[[as.character(row_idx)]] <<- nm
  }

  if (!is.null(primary)) {
    if (length(prim_nms) > 0) {
      add_label(if (length(prim_nms) == 1L) "Primary outcome" else "Primary outcomes")
      for (nm in prim_nms) add_outcome(nm)
    }
    if (length(sec_nms) > 0) {
      add_label(if (length(sec_nms) == 1L) "Secondary outcome" else "Secondary outcomes")
      for (nm in sec_nms) add_outcome(nm)
    }
  } else {
    for (nm in nms) add_outcome(nm)
  }

  df <- do.call(rbind, all_rows)

  # --- flextable ---
  ft <- flextable::flextable(df)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "left",   part = "body")
  ft <- flextable::align(ft, j = 2:3, align = "center", part = "body")

  # Group-label rows: merge, grey background, bold italic
  for (lr in label_rows) {
    ft <- flextable::merge_h(ft,  i = lr, part = "body")
    ft <- flextable::bg(ft,       i = lr, bg = "#EBEBEB",  part = "body")
    ft <- flextable::bold(ft,     i = lr,                  part = "body")
    ft <- flextable::italic(ft,   i = lr,                  part = "body")
    ft <- flextable::fontsize(ft, i = lr, size = 9,        part = "body")
    ft <- flextable::align(ft,    i = lr, align = "left",  part = "body")
  }

  # Certainty cell colour per outcome row
  cert_col <- "Certainty"
  for (ri in names(outcome_map)) {
    i  <- as.integer(ri)
    nm <- outcome_map[[ri]]
    p  <- pal[[outcomes[[nm]]$certainty]]
    ft <- flextable::bg(ft,    i = i, j = cert_col, bg    = p$bg,   part = "body")
    ft <- flextable::color(ft, i = i, j = cert_col, color = p$text, part = "body")
    ft <- flextable::bold(ft,  i = i, j = cert_col,                 part = "body")
    ft <- flextable::align(ft, i = i, j = cert_col, align = "center", part = "body")
  }

  # Header style
  ft <- flextable::bg(ft,    bg = "#2C3E50", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft,  part = "header")

  # Column widths (7 base cols: Outcome, k, N, CER, IER, Effect, Certainty)
  ft <- flextable::width(ft, j = 1, width = 1.4)
  ft <- flextable::width(ft, j = 2, width = 0.6)
  ft <- flextable::width(ft, j = 3, width = 0.9)
  ft <- flextable::width(ft, j = 4, width = 1.2)
  ft <- flextable::width(ft, j = 5, width = 1.3)
  ft <- flextable::width(ft, j = 6, width = 1.5)
  ft <- flextable::width(ft, j = 7, width = 1.3)
  if (show_domains) {
    for (j in 8:12) ft <- flextable::width(ft, j = j, width = 0.7)
  }

  # Footer
  footnote <- paste0(
    "GRADE certainty of evidence. BMJ 2025 Core GRADE (Guyatt et al.). ",
    CERTAINTY_SYMBOLS[["High"]], "=High  ",
    CERTAINTY_SYMBOLS[["Moderate"]], "=Moderate  ",
    CERTAINTY_SYMBOLS[["Low"]], "=Low  ",
    CERTAINTY_SYMBOLS[["Very Low"]], "=Very Low  ",
    "CI=confidence interval.",
    if (show_domains) " Domain columns: RoB=Risk of bias; Ind=Indirectness; Inc=Inconsistency; Imp=Imprecision; PB=Publication bias." else ""
  )
  ft <- flextable::add_footer_lines(ft, values = footnote)
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::color(ft, color = "#555555", part = "footer")

  ft
}

# --------------------------------------------------------------------------
# Internal helpers
# --------------------------------------------------------------------------

.build_row <- function(nm, g, show_domains, per = 1000, prediction = FALSE) {
  meta_obj <- g$meta
  k        <- meta_obj$k
  n_total  <- .total_n(meta_obj)
  cer_str  <- .format_cer(g$baseline_risk, per)
  ier_str  <- .format_ier(meta_obj, g$baseline_risk, per)
  eff      <- .format_effect(meta_obj, g$outcome_type, prediction = prediction)
  cert_str <- paste0(g$certainty, "\n", CERTAINTY_SYMBOLS[[g$certainty]])

  row <- data.frame(
    col1 = nm,
    col2 = as.character(k),
    col3 = if (is.na(n_total)) "NR" else format(n_total, big.mark = ","),
    col4 = cer_str,
    col5 = ier_str,
    col6 = eff,
    col7 = cert_str,
    stringsAsFactors = FALSE
  )

  if (show_domains) {
    d   <- g$domain_assessments
    dom <- function(name) {
      r <- d[d$domain == name, ]
      if (nrow(r) == 0) return("?")
      .domain_symbol(r$judgment[1])
    }
    row$d1 <- dom("Risk of bias")
    row$d2 <- dom("Indirectness")
    row$d3 <- dom("Inconsistency")
    row$d4 <- dom("Imprecision")
    row$d5 <- dom("Publication bias")
  }
  row
}

.domain_symbol <- function(judgment) {
  switch(judgment,
    "no"           = "\u2714",       # ✔ no concern
    "some"         = "\u25b2",       # ▲ some concern
    "serious"      = "\u2193",       # ↓ serious
    "very_serious" = "\u2193\u2193", # ↓↓ very serious
    "?"
  )
}

.col_headers <- function(show_domains, per = 1000) {
  per_str <- format(per, big.mark = ",", scientific = FALSE)
  base <- c("Outcome", "Studies (k)", "Participants (N)",
            paste0("Control rate\n(per ", per_str, ")"),
            paste0("Exp. rate\n(per ", per_str, ")"),
            "Effect (95% CI)", "Certainty")
  doms <- c("RoB", "Indir.", "Incon.", "Impre.", "PB")
  if (show_domains) c(base, doms) else base
}
