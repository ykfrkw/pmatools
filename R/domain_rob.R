# domain_rob.R - Risk of Bias domain assessment
#
# BMJ 2025 Core GRADE 4, Fig 2 flowchart:
#
#   Step 1. Is evidence DOMINATED by high-RoB studies?
#     Threshold: rob_dominant_threshold (default 0.60)
#
#   YES -> Step 2. Direction-and-magnitude check (auto)
#     inflation_ratio = (|TE_all| - |TE_low|) / |TE_low|
#     direction_ok    = (depends on small_values)
#     rate_down       = direction_ok AND (inflation_ratio > rob_inflation_threshold)
#
#   NO  -> Appreciable low-RoB evidence; do not rate down.
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
                       rob_dominant_threshold  = 0.60,
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
                 threshold           = rob_dominant_threshold,
                 inflation_threshold = rob_inflation_threshold,
                 small_values        = small_values)
}

# --------------------------------------------------------------------------
# Flowchart
# --------------------------------------------------------------------------
.flowchart_rob <- function(rob_vec, meta_obj, threshold,
                           inflation_threshold = 0.10, small_values = NULL) {

  # high-RoB = "serious" or "very_serious"
  high_idx <- rob_vec %in% c("serious", "very_serious")
  n_high   <- sum(high_idx)
  n_total  <- length(rob_vec)

  # Random-effects weight share
  weights <- meta_obj$w.random
  if (!is.null(weights) && length(weights) == n_total && sum(weights) > 0) {
    pct_high_w <- sum(weights[high_idx]) / sum(weights)
    weight_note <- sprintf(
      "High-RoB studies: %d/%d (%.1f%% of random-effects weight)",
      n_high, n_total, pct_high_w * 100
    )
    dominated <- pct_high_w >= threshold
  } else {
    pct_high_w <- n_high / n_total
    weight_note <- sprintf(
      "High-RoB studies: %d/%d (%.1f%% by count; weights unavailable)",
      n_high, n_total, pct_high_w * 100
    )
    dominated <- pct_high_w >= threshold
  }

  tbl_note <- paste(
    paste0(names(table(rob_vec)), ": n=", as.integer(table(rob_vec))),
    collapse = "; "
  )

  # NO branch: not dominated -> do not rate down
  if (!dominated) {
    return(make_domain_row(
      domain   = "Risk of bias",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        "FLOWCHART: Not dominated (threshold ", round(threshold * 100), "%). ",
        weight_note, ". -> Do not rate down. | ",
        tbl_note
      )
    ))
  }

  # YES branch: dominated -> direction-and-magnitude check
  dir <- .assess_bias_direction(
    te_all              = meta_obj$TE.random,
    te_vec              = meta_obj$TE,
    se_vec              = meta_obj$seTE,
    low_idx             = !high_idx,
    small_values        = small_values,
    inflation_threshold = inflation_threshold
  )

  judgment <- if (dir$inflates) "serious" else "no"

  make_domain_row(
    domain   = "Risk of bias",
    judgment = judgment,
    auto     = FALSE,
    notes    = paste0(
      "FLOWCHART: Dominated (threshold ", round(threshold * 100), "%). ",
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

  dir_desc <- if (inflates) {
    "high-RoB inflates effect beyond threshold -> rate down"
  } else if (!isTRUE(direction_ok)) {
    "high-RoB does NOT inflate (direction check failed) -> do not rate down"
  } else {
    "inflation below threshold; treated as random variation -> do not rate down"
  }

  list(
    inflates = inflates,
    note     = paste0(diff_note, ". ", dir_desc)
  )
}

# --------------------------------------------------------------------------
# RoB level normalisation
# Cochrane RoB2 / plain English -> internal GRADE level
# --------------------------------------------------------------------------
.normalize_rob_level <- function(x) {
  aliases <- c(
    # Cochrane RoB2
    "No concerns"       = "no",
    "Some concerns"     = "some",
    "Serious concerns"  = "serious",
    "Critical concerns" = "very_serious",
    # Single-letter shortcuts
    "L" = "no", "l" = "no",
    "S" = "some", "s" = "some",
    "M" = "some", "m" = "some",
    "H" = "serious", "h" = "serious",
    "C" = "very_serious", "c" = "very_serious",
    "*" = "some",
    # Plain / alternate capitalisation
    "low"          = "no",    "Low"          = "no",
    "moderate"     = "some",  "Moderate"     = "some",
    "high"         = "serious", "High"       = "serious",
    "very high"    = "very_serious", "Very high" = "very_serious",
    # Internal (pass-through)
    "no"           = "no",
    "some"         = "some",
    "serious"      = "serious",
    "very_serious" = "very_serious"
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
