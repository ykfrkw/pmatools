# domain_rob.R - Risk of Bias domain assessment
#
# v0.4+: MECE 5-rule zone-based decision. TE_all and TE_low are each
# classified into one of three zones defined by +/-MID:
#   above   : TE > +MID
#   trivial : -MID <= TE <= +MID
#   below   : TE < -MID
#
# Decision (zone(TE_all) = za, zone(TE_low) = zl):
#   Rule 1: za == zl == "trivial"                    -> "no"
#   Rule 2: za == zl, non-trivial, inflation <= 10%  -> "no"
#   Rule 3: za == zl, non-trivial, inflation > 10%   -> "some_concerns"  (-1)
#   Rule 4: za != zl, no sign flip across null       -> "some_concerns"  (-1)
#   Rule 5: za != zl, sign flip (above <-> below)    -> "serious"        (-2)
#
# inflation_ratio = (|TE_all| - |TE_low|) / |TE_low|, evaluated only when the
# bias direction is bias-favouring (per `small_values`); a deflation in the
# bias-favouring direction never triggers a downgrade.
#
# Fallback: when `mid_internal` is NULL/NA/<=0 the trivial zone collapses to
# {0}, so only sign-flip (rule 5) can trigger a zone change.
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
                       small_values            = NULL,
                       mid_internal            = NULL) {
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
                 small_values        = small_values,
                 mid_internal        = mid_internal)
}

# --------------------------------------------------------------------------
# Flowchart (v0.3.1+: dominance gate removed; always run direction check)
# --------------------------------------------------------------------------
.flowchart_rob <- function(rob_vec, meta_obj,
                           inflation_threshold = 0.10, small_values = NULL,
                           mid_internal = NULL) {

  # high-RoB = "serious" (legacy "very_serious" still recognized after
  # .normalize_rob_levels)
  high_idx <- rob_vec %in% c("serious", "very_serious")
  n_high   <- sum(high_idx)
  n_total  <- length(rob_vec)

  # IV weight share carried by high-RoB studies. {meta} can drop studies (e.g.,
  # double-zero events with method = "Inverse") so $TE/$seTE/$w.random retain
  # the original length while $k counts only valid rows; align by `is.finite`
  # before indexing with `high_idx` (which has length n_total = k).
  pick_weights <- function(meta_obj, n_total) {
    align <- function(v) {
      if (is.null(v)) return(NULL)
      if (length(v) == n_total) return(v)
      keep <- is.finite(v) & v > 0
      if (sum(keep) == n_total) return(v[keep])
      keep_te <- is.finite(meta_obj$TE)
      if (length(keep_te) == length(v) && sum(keep_te) == n_total) return(v[keep_te])
      NULL
    }
    for (slot in c("w.random", "w.common", "w.fixed")) {
      v <- align(meta_obj[[slot]])
      if (!is.null(v)) {
        ok <- is.finite(v) & v > 0
        if (any(ok) && sum(v[ok]) > 0) return(v)
      }
    }
    se <- align(meta_obj$seTE)
    if (!is.null(se)) {
      v <- 1 / se^2
      if (any(is.finite(v) & v > 0)) return(v)
    }
    NULL
  }
  w_vec <- pick_weights(meta_obj, n_total)
  w_high_pct <- if (!is.null(w_vec) && length(w_vec) == n_total) {
    ok <- is.finite(w_vec)
    w_total <- sum(w_vec[ok], na.rm = TRUE)
    if (w_total > 0) {
      100 * sum(w_vec[high_idx & ok], na.rm = TRUE) / w_total
    } else NA_real_
  } else NA_real_

  count_pct <- 100 * (n_high / max(1L, n_total))
  weight_note <- if (is.finite(w_high_pct)) {
    sprintf("High-RoB studies: %d/%d (%.0f%% by count, %.0f%% by weight)",
            n_high, n_total, count_pct, w_high_pct)
  } else {
    sprintf("High-RoB studies: %d/%d (%.0f%% by count)",
            n_high, n_total, count_pct)
  }

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
    se_all              = meta_obj$seTE.random,
    te_vec              = te_vec,
    se_vec              = se_vec,
    low_idx             = !high_idx,
    small_values        = small_values,
    inflation_threshold = inflation_threshold,
    sm                  = meta_obj$sm,
    mid_internal        = mid_internal
  )

  judgment <- dir$judgment

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
# MECE 5-rule zone-and-magnitude check (v0.4+)
#
# Zones (defined by +/-MID on the analysis scale):
#   above   : TE > +MID
#   trivial : -MID <= TE <= +MID
#   below   : TE < -MID
# Fallback when MID is NULL/NA/<=0: trivial zone collapses to {0}, so above
# means TE > 0 and below means TE < 0; only sign flips can trigger zone change.
#
# Decision tree (za = zone(TE_all), zl = zone(TE_low)):
#   za == zl == "trivial"                                     -> "no"            (rule 1)
#   za == zl, non-trivial, no bias-favouring inflation > 10%  -> "no"            (rule 2)
#   za == zl, non-trivial, bias-favouring inflation > 10%     -> "some_concerns" (rule 3)
#   za != zl, no sign flip across null                        -> "some_concerns" (rule 4)
#   za != zl, sign flip (above <-> below)                     -> "serious"       (rule 5)
# --------------------------------------------------------------------------
.assess_bias_direction <- function(te_all, se_all, te_vec, se_vec, low_idx,
                                   small_values, inflation_threshold = 0.10,
                                   sm = NULL, mid_internal = NULL) {

  n_low <- sum(low_idx)

  if (!is.null(small_values) && !small_values %in% c("desirable", "undesirable")) {
    rlang::abort("small_values must be 'desirable' or 'undesirable'.")
  }

  # Display label and back-transform for ratio measures
  log_scale <- !is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM")
  sm_label  <- if (!is.null(sm) && nzchar(sm)) sm else "TE"
  .disp <- function(x) if (log_scale) round(exp(x), 3) else round(x, 3)

  # MID handling
  M <- if (!is.null(mid_internal) && is.finite(mid_internal) && mid_internal > 0) {
    mid_internal
  } else {
    0
  }
  mid_supplied <- M > 0
  zone_of <- function(te) {
    if (!is.finite(te)) return(NA_character_)
    if (te >  +M) "above"
    else if (te < -M) "below"
    else "trivial"
  }
  mid_disp <- if (mid_supplied) sprintf("+/-%s", format(.disp(M), nsmall = 0)) else "+/-0"
  mid_note <- if (mid_supplied) {
    sprintf("MID threshold = %s (%s)", mid_disp, if (log_scale) paste0("log ", sm_label, " scale") else sm_label)
  } else {
    "MID not supplied; trivial zone collapsed to {0} (sign-flip rule only)"
  }

  # No low/some-RoB studies -> conservative rate-down
  if (n_low == 0 || is.null(te_vec) || is.null(se_vec)) {
    return(list(
      judgment = "some_concerns",
      note     = paste0(
        "No low/some-RoB studies to compare with; conservative rate-down applied. ",
        sm_label, "(all) = ", .disp(te_all), ". ", mid_note, "."
      )
    ))
  }

  # TE_low: inverse-variance weighted mean of low-RoB studies
  w_low  <- 1 / se_vec[low_idx]^2
  te_low <- sum(w_low * te_vec[low_idx]) / sum(w_low)

  za <- zone_of(te_all)
  zl <- zone_of(te_low)

  # Direction of the bias contribution (only relevant for rule 3)
  direction_ok <- if (is.null(small_values)) {
    abs(te_all) > abs(te_low)
  } else if (small_values == "undesirable") {
    te_all > te_low
  } else {
    te_all < te_low
  }

  # Relative inflation (defined only when |TE_low| > 0; rule 3 requires it).
  if (abs(te_low) > 1e-9) {
    inflation_ratio <- (abs(te_all) - abs(te_low)) / abs(te_low)
  } else {
    inflation_ratio <- NA_real_
  }
  inflates <- isTRUE(direction_ok) &&
              is.finite(inflation_ratio) &&
              (inflation_ratio > inflation_threshold)

  # Sign flip across the null (rule 5 vs rule 4 disambiguation).
  sign_flips <- identical(za, "above") && identical(zl, "below") ||
                identical(za, "below") && identical(zl, "above")

  # 5-rule decision
  if (identical(za, zl)) {
    if (identical(za, "trivial")) {
      judgment <- "no"; rule <- 1L
      rule_desc <- "Rule 1: TE_all and TE_low both in trivial zone -> do not rate down"
    } else if (inflates) {
      judgment <- "some_concerns"; rule <- 3L
      rule_desc <- "Rule 3: same non-trivial zone but bias-favouring inflation > threshold -> rate down 1"
    } else {
      judgment <- "no"; rule <- 2L
      rule_desc <- "Rule 2: same non-trivial zone, inflation within threshold (or not bias-favouring) -> do not rate down"
    }
  } else {
    if (sign_flips) {
      judgment <- "serious"; rule <- 5L
      rule_desc <- "Rule 5: zone changes across null (benefit <-> harm) -> rate down 2 (serious)"
    } else {
      judgment <- "some_concerns"; rule <- 4L
      rule_desc <- "Rule 4: zone changes without sign flip -> rate down 1 (some_concerns)"
    }
  }

  sv_desc <- if (is.null(small_values)) {
    "small_values = NULL (using |TE| comparison for inflation direction)"
  } else {
    sprintf("small_values = '%s'", small_values)
  }

  inflation_str <- if (is.finite(inflation_ratio)) {
    sprintf("relative inflation = %.1f%% (threshold %.0f%%)",
            100 * inflation_ratio, 100 * inflation_threshold)
  } else {
    sprintf("relative inflation = undefined (|TE_low| ~ 0; threshold %.0f%%)",
            100 * inflation_threshold)
  }

  diff_note <- sprintf(
    "%s(all) = %.3f [zone = %s]; %s(excl. high-RoB) = %.3f [zone = %s]; %s; %s; %s",
    sm_label, .disp(te_all), za,
    sm_label, .disp(te_low), zl,
    inflation_str, mid_note, sv_desc
  )

  list(
    judgment        = judgment,
    rule            = rule,
    zone_all        = za,
    zone_low        = zl,
    sign_flips      = sign_flips,
    inflates        = inflates,
    inflation_ratio = inflation_ratio,
    note            = paste0(diff_note, ". ", rule_desc)
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
