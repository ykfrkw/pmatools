# domain_rob.R - Risk of Bias domain assessment
#
# v0.3.1+: dominance-threshold gate removed. Direction-and-magnitude check
# is always run regardless of how much weight high-RoB studies carry.
#
#   sign(TE_all) != sign(TE_low) (sign flips when high-RoB removed)
#                                          -> "serious"      (-2)
#   else, |TE_all| / |TE_low| inflated > rob_inflation_threshold (default 0.10)
#                                          -> "some_concerns" (-1)
#   else                                   -> "no"
#
# `rob_dominant_threshold` is accepted but ignored for backward compatibility.
#
# Inputs:
#   (a) scalar GRADE level: bypass flowchart
#   (b) length-k vector: apply flowchart
#   (c) column name in meta_obj$data: expand to vector and apply flowchart
#
# small_values:
#   "undesirable": small values are bad (e.g., response rate, OR for benefit)
#                  TE_all > TE_low indicates inflation toward favorable
#   "desirable"  : small values are good (e.g., mortality, severity)
#                  TE_all < TE_low indicates inflation toward favorable
#   NULL         : direction unknown; use |TE_all| > |TE_low| (further from null)

assess_rob <- function(rob, meta_obj,
                       rob_dominant_threshold  = NULL,   # accepted but ignored (deprecated)
                       rob_inflation_threshold = 0.10,
                       small_values            = NULL) {
  k <- meta_obj$k

  # NULL -> default "no"
  if (is.null(rob)) {
    return(make_domain_row(
      domain   = "Risk of bias",
      judgment = "no",
      auto     = FALSE,
      notes    = "Not assessed (rob = NULL). Assumed no concern."
    ))
  }

  # Defensive: coerce data.frame / tibble (1-col) or list inputs to plain vector
  if (is.data.frame(rob)) {
    if (ncol(rob) == 1) rob <- rob[[1]]
    else rlang::abort("rob must be a single column, not a multi-column data.frame.")
  }
  if (is.list(rob) && !is.data.frame(rob)) {
    rob <- unlist(rob, use.names = FALSE)
  }
  if (is.factor(rob)) rob <- as.character(rob)

  # Scalar GRADE level (after normalisation): bypass flowchart
  if (length(rob) == 1 && is.character(rob)) {
    rob_norm <- .normalize_rob_level(rob)
    if (rob_norm %in% GRADE_LEVELS) {
      return(make_domain_row(
        domain   = "Risk of bias",
        judgment = rob_norm,
        auto     = FALSE,
        notes    = "Overall judgment provided by user (scalar; flowchart not applied)."
      ))
    }
    # Treat as column name
    col  <- rob
    data <- meta_obj$data
    if (is.null(data) || !col %in% names(data)) {
      rlang::abort(paste0(
        "rob = '", col, "' is not a recognized GRADE level and was not found as a column ",
        "in the meta object's data. Check column names with names(meta_obj$data)."
      ))
    }
    rob <- as.character(data[[col]])
  }

  # Vector: normalise + length check
  rob <- .normalize_rob_levels(rob)
  if (length(rob) != k) {
    rlang::abort(paste0(
      "rob must be a scalar GRADE level, a column name in meta_obj$data, ",
      "or a vector of length k (", k, "). Got length ", length(rob), "."
    ))
  }

  validate_grade_level(rob, "rob")
  .flowchart_rob(rob, meta_obj,
                 inflation_threshold = rob_inflation_threshold,
                 small_values        = small_values)
}

# --------------------------------------------------------------------------
# Flowchart (v0.3.1+: dominance gate removed; always run direction check)
# --------------------------------------------------------------------------
.flowchart_rob <- function(rob_vec, meta_obj,
                           inflation_threshold = 0.10, small_values = NULL) {

  # high-RoB = "serious" (legacy "very_serious" still recognised after
  # .normalize_rob_levels)
  high_idx <- rob_vec %in% c("serious", "very_serious")
  n_high   <- sum(high_idx)
  n_total  <- length(rob_vec)

  weight_note <- sprintf("High-RoB studies: %d/%d (%.0f%% by count)",
                         n_high, n_total,
                         100 * (n_high / max(1L, n_total)))

  tbl_note <- paste(
    paste0(names(table(rob_vec)), ": n=", as.integer(table(rob_vec))),
    collapse = "; "
  )

  # If no high-RoB studies at all, no possibility of bias-driven inflation.
  if (n_high == 0) {
    return(make_domain_row(
      domain   = "Risk of bias",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        "No high-RoB studies. ", weight_note, ". -> Do not rate down. | ",
        tbl_note
      )
    ))
  }

  # Direction-and-magnitude check (always run when at least one high-RoB
  # study is present). Align TE / seTE to length k so logical indexing with
  # high_idx is correct.
  te_vec  <- meta_obj$TE
  se_vec  <- meta_obj$seTE
  if (!is.null(te_vec) && length(te_vec) != n_total) {
    keep <- is.finite(te_vec)
    if (sum(keep) == n_total) {
      te_vec <- te_vec[keep]
      if (!is.null(se_vec) && length(se_vec) == length(meta_obj$TE)) {
        se_vec <- se_vec[keep]
      }
    }
  }

  dir <- .assess_bias_direction(
    te_all              = meta_obj$TE.random,
    te_vec              = te_vec,
    se_vec              = se_vec,
    low_idx             = !high_idx,
    small_values        = small_values,
    inflation_threshold = inflation_threshold
  )

  judgment <- if (isTRUE(dir$sign_flips)) {
    "serious"        # -2: removing high-RoB studies flips the pooled TE direction
  } else if (isTRUE(dir$inflates)) {
    "some_concerns"  # -1: inflation beyond threshold
  } else {
    "no"
  }

  make_domain_row(
    domain   = "Risk of bias",
    judgment = judgment,
    auto     = FALSE,
    notes    = paste0(
      weight_note, ". ",
      dir$note, " | ",
      tbl_note
    )
  )
}

# --------------------------------------------------------------------------
# Direction-and-magnitude check (v0.2)
# --------------------------------------------------------------------------
.assess_bias_direction <- function(te_all, te_vec, se_vec, low_idx,
                                   small_values, inflation_threshold = 0.10) {

  n_low <- sum(low_idx)

  if (!is.null(small_values) && !small_values %in% c("desirable", "undesirable")) {
    rlang::abort("small_values must be 'desirable' or 'undesirable'.")
  }

  # No low/some-RoB studies -> conservative rate-down
  if (n_low == 0 || is.null(te_vec) || is.null(se_vec)) {
    return(list(
      inflates = TRUE,
      note     = paste0(
        "No low/some-RoB studies to compare with; conservative rate-down applied. ",
        "TE(all) = ", round(te_all, 3), "."
      )
    ))
  }

  # TE_low: inverse-variance weighted mean of low-RoB studies
  w_low  <- 1 / se_vec[low_idx]^2
  te_low <- sum(w_low * te_vec[low_idx]) / sum(w_low)

  # |TE_low| approx 0 -> ratio undefined -> conservative rate-down
  if (abs(te_low) < 1e-9) {
    return(list(
      inflates = TRUE,
      note     = paste0(
        "TE(excl. high-RoB) approx 0; relative inflation undefined; ",
        "conservative rate-down applied. ",
        sprintf("TE(all) = %.3f, TE(low) = %.3f.", te_all, te_low)
      )
    ))
  }

  inflation_ratio <- (abs(te_all) - abs(te_low)) / abs(te_low)

  direction_ok <- if (is.null(small_values)) {
    abs(te_all) > abs(te_low)
  } else if (small_values == "undesirable") {
    te_all > te_low
  } else {
    te_all < te_low
  }

  inflates <- isTRUE(direction_ok) && (inflation_ratio > inflation_threshold)

  # -2 trigger: removing high-RoB studies flips the sign of the pooled TE.
  # (Treat |TE_all| / |TE_low| close to 0 as a non-flip to avoid spurious
  # serious downgrade from negligible effects on either side.)
  sign_flips <- (sign(te_all) != sign(te_low)) &&
                (abs(te_all) > 1e-6) &&
                (abs(te_low) > 1e-6)

  sv_desc <- if (is.null(small_values)) {
    "small_values = NULL (using |TE| comparison)"
  } else {
    sprintf("small_values = '%s'", small_values)
  }

  diff_note <- sprintf(
    "TE(all) = %.3f; TE(excl. high-RoB) = %.3f; |TE_all| = %.3f, |TE_low| = %.3f; relative inflation = %.1f%% (threshold %.0f%%); %s",
    te_all, te_low, abs(te_all), abs(te_low),
    100 * inflation_ratio, 100 * inflation_threshold, sv_desc
  )

  dir_desc <- if (sign_flips) {
    "DIRECTION FLIPS when high-RoB studies are removed -> rate down 2 (serious)"
  } else if (inflates) {
    "high-RoB inflates effect beyond threshold -> rate down 1 (some_concerns)"
  } else if (!isTRUE(direction_ok)) {
    "high-RoB does NOT inflate (direction check failed) -> do not rate down"
  } else {
    "inflation below threshold; treated as random variation -> do not rate down"
  }

  list(
    inflates   = inflates,
    sign_flips = sign_flips,
    note       = paste0(diff_note, ". ", dir_desc)
  )
}

# --------------------------------------------------------------------------
# RoB level normalisation
# Cochrane RoB2 / plain English -> internal GRADE level
# --------------------------------------------------------------------------
.normalize_rob_level <- function(x) {
  aliases <- c(
    # Cochrane RoB2 (3-level mapping: critical -> serious in v0.3+)
    "No concerns"       = "no",
    "Some concerns"     = "some_concerns",
    "Serious concerns"  = "serious",
    "Critical concerns" = "serious",
    # Single-letter shortcuts
    "L" = "no", "l" = "no",
    "S" = "some_concerns", "s" = "some_concerns",
    "M" = "some_concerns", "m" = "some_concerns",
    "H" = "serious", "h" = "serious",
    "C" = "serious", "c" = "serious",
    "*" = "some_concerns",
    # Plain / alternate capitalisation
    "low"          = "no",            "Low"          = "no",
    "moderate"     = "some_concerns", "Moderate"     = "some_concerns",
    "high"         = "serious",       "High"         = "serious",
    "very high"    = "serious",       "Very high"    = "serious",
    # Internal (pass-through + legacy)
    "no"            = "no",
    "some"          = "some_concerns",   # legacy alias
    "some_concerns" = "some_concerns",
    "serious"       = "serious",
    "very_serious"  = "serious"           # legacy alias
  )
  if (x %in% names(aliases)) aliases[[x]] else x
}

.normalize_rob_levels <- function(rob_vec) {
  result <- vapply(rob_vec, .normalize_rob_level, character(1))
  unknown <- !result %in% GRADE_LEVELS
  if (any(unknown)) {
    rlang::abort(paste0(
      "Unrecognized RoB level(s): ", paste(unique(rob_vec[unknown]), collapse = ", "),
      ". Accepted values: 'no', 'some', 'serious', 'very_serious', or Cochrane RoB2 labels ",
      "('No concerns', 'Some concerns', 'Serious concerns', 'Critical concerns')."
    ))
  }
  unname(result)
}
