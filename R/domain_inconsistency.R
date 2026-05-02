# domain_inconsistency.R - Inconsistency domain assessment
#
# BMJ 2025 Core GRADE 3, Fig 2 flowchart (preserved from v0.1.0).
# v0.2 enhancement: when `mid_internal` is supplied, Step 2 uses +/-MID as the
# clinical decision threshold (3-zone classification) instead of null = 0.
#
# Steps:
#   Step 1. Are there important differences in point estimates AND limited CI overlap?
#     NO  -> judgment = "no" (do not rate down)
#     YES -> Step 2
#
#   Step 2. Where do point estimates fall vs the clinical decision threshold?
#     majority_one_side -> judgment = "no" (manual) or "some" (auto, conservative)
#     opposite_sides    -> Step 3
#
#   Step 3. Is opposite-sided inconsistency explained by credible subgroup?
#     yes -> judgment = "no" + note
#     no  -> judgment = "serious"
#
# Auto Step 1 proxy: I^2 > 25%  (Q-test no longer used; v0.1.0 used "I^2 > 25% OR Q p < 0.10")
# Auto Step 2 proxy:
#   With mid_internal:
#     classify TE per study into 3 zones around +/-MID;
#     pct_one_side = max((above+trivial)/k, (below+trivial)/k);
#     >=0.75 -> majority_one_side
#   Without mid_internal:
#     fall back to v0.1.0 null=0 behavior (pct_positive >= 0.75 or <= 0.25)
# Auto Step 3: cannot be auto-detected -> opposite_sides leads to "serious"
#
# I^2 / tau^2 / Q statistics are always shown in notes but never drive the judgment.

assess_inconsistency <- function(meta_obj,
                                 inconsistency                    = NULL,
                                 inconsistency_ci_diff            = NULL,
                                 inconsistency_threshold_side     = NULL,
                                 inconsistency_subgroup_explained = NULL,
                                 mid_internal                     = NULL) {

  # ----- Statistics (always computed for notes) -----
  i2_pct <- if (!is.null(meta_obj$I2) && !is.na(meta_obj$I2)) {
    meta_obj$I2 * 100
  } else NA_real_

  tau2   <- meta_obj$tau2
  pval_q <- meta_obj$pval.Q

  stat_note <- sprintf(
    "I2 = %.1f%%, tau2 = %.4f, Q p = %.3f (supplementary; not the primary criterion)",
    if (is.na(i2_pct)) 0 else i2_pct,
    if (is.null(tau2) || is.na(tau2)) 0 else tau2,
    if (is.null(pval_q) || is.na(pval_q)) 1 else pval_q
  )

  # ----- Path A: scalar override -----
  if (!is.null(inconsistency)) {
    validate_grade_level(inconsistency, "inconsistency")
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = inconsistency,
      auto     = FALSE,
      notes    = paste0(
        "Overall judgment provided by user (scalar; flowchart not applied). ",
        stat_note
      )
    ))
  }

  # ----- Path B: manual flowchart -----
  if (!is.null(inconsistency_ci_diff)) {

    if (!inconsistency_ci_diff %in% c("yes", "no")) {
      rlang::abort("inconsistency_ci_diff must be 'yes' or 'no'.")
    }

    # Step 1: no important differences -> do not rate down
    if (inconsistency_ci_diff == "no") {
      return(make_domain_row(
        domain   = "Inconsistency",
        judgment = "no",
        auto     = FALSE,
        notes    = paste0(
          "FLOWCHART Step 1: No important differences in point estimates / ",
          "adequate CI overlap -> do not rate down. | ", stat_note
        )
      ))
    }

    # Step 1: yes -> Step 2
    if (is.null(inconsistency_threshold_side)) {
      rlang::abort(paste0(
        "inconsistency_ci_diff = 'yes' requires inconsistency_threshold_side = ",
        "'majority_one_side' or 'opposite_sides'."
      ))
    }

    if (!inconsistency_threshold_side %in% c("majority_one_side", "opposite_sides")) {
      rlang::abort(
        "inconsistency_threshold_side must be 'majority_one_side' or 'opposite_sides'."
      )
    }

    if (inconsistency_threshold_side == "majority_one_side") {
      return(make_domain_row(
        domain   = "Inconsistency",
        judgment = "no",
        auto     = FALSE,
        notes    = paste0(
          "FLOWCHART Step 2: Important CI differences exist, but majority of point ",
          "estimates are on one side of clinical threshold -> do not rate down ",
          "(per BMJ Core GRADE 3 flowchart). | ",
          stat_note
        )
      ))
    }

    # opposite_sides -> Step 3
    if (is.null(inconsistency_subgroup_explained)) {
      rlang::abort(paste0(
        "inconsistency_threshold_side = 'opposite_sides' requires ",
        "inconsistency_subgroup_explained = 'yes' or 'no'."
      ))
    }

    if (!inconsistency_subgroup_explained %in% c("yes", "no")) {
      rlang::abort("inconsistency_subgroup_explained must be 'yes' or 'no'.")
    }

    if (inconsistency_subgroup_explained == "yes") {
      return(make_domain_row(
        domain   = "Inconsistency",
        judgment = "no",
        auto     = FALSE,
        notes    = paste0(
          "FLOWCHART Step 3: Opposite-sided estimates explained by credible subgroup ",
          "-> do not rate down; present subgroup results separately. | ",
          stat_note
        )
      ))
    }

    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "serious",
      auto     = FALSE,
      notes    = paste0(
        "FLOWCHART Step 3: Opposite-sided estimates not explained by subgroup ",
        "-> rate down. | ", stat_note
      )
    ))
  }

  # ----- Path C: auto-detect -----
  .auto_inconsistency(meta_obj, i2_pct, stat_note, mid_internal)
}

# --------------------------------------------------------------------------
# Auto-detect path
# --------------------------------------------------------------------------
.auto_inconsistency <- function(meta_obj, i2_pct, stat_note, mid_internal = NULL) {

  # Step 1 proxy: I^2 > 25%
  has_i2 <- !is.na(i2_pct)
  ci_diff_yes <- has_i2 && (i2_pct > 25)

  if (!ci_diff_yes) {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "no",
      auto     = TRUE,
      notes    = paste0(
        "AUTO Step 1: No important heterogeneity (I2 <= 25%) -> do not rate down. | ",
        stat_note
      )
    ))
  }

  # Step 2 proxy
  te_vec <- meta_obj$TE
  if (is.null(te_vec) || length(te_vec) < 2) {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "some_concerns",
      auto     = TRUE,
      notes    = paste0(
        "AUTO Step 1: I2 > 25%; AUTO Step 2 not assessable (study-level TEs unavailable); ",
        "judgment = 'some_concerns' (conservative). | ", stat_note
      )
    ))
  }

  k <- length(te_vec)
  te_vec <- te_vec[!is.na(te_vec)]

  # 3-level inconsistency classification:
  #   max single-zone share >= 80%               -> "no" (consistent direction)
  #   both directions have substantial mass      -> "serious" (-2)
  #     (n_above/k >= 20% AND n_below/k >= 20%)
  #   else                                       -> "some_concerns" (-1)
  #
  # Reference: CINeMA (Nikolakopoulou 2020) for the 80% one-side threshold;
  # the substantial-both-directions criterion captures clinically opposite
  # effects across studies, which corresponds to BMJ Core GRADE 3's
  # "point estimates on opposite sides of threshold" qualitative trigger.
  ZONE_MAJORITY    <- 0.80
  OPPOSITE_EACH    <- 0.20

  if (!is.null(mid_internal) && !is.na(mid_internal) && mid_internal > 0) {
    M <- mid_internal
    n_above   <- sum(te_vec > +M)
    n_below   <- sum(te_vec < -M)
    n_trivial <- length(te_vec) - n_above - n_below
  } else {
    # Fallback: null = 0 (no MID supplied). "trivial" zone collapses to 0.
    M <- 0
    n_above   <- sum(te_vec > 0)
    n_below   <- sum(te_vec < 0)
    n_trivial <- 0L
  }

  n_total       <- length(te_vec)
  zone_counts   <- c(n_above, n_trivial, n_below)
  pct_max_zone  <- max(zone_counts) / n_total
  pct_each_side <- min(n_above, n_below) / n_total

  threshold_label <- if (M > 0) {
    sprintf("vs +/-Threshold = +/-%g", M)
  } else {
    "vs null = 0 (Threshold not specified)"
  }

  if (pct_max_zone >= ZONE_MAJORITY) {
    threshold_side <- "majority_one_side"
    judgment_auto  <- "no"
    decision_note  <- sprintf(
      "Largest single-zone share %.0f%% >= %.0f%% -> direction consistent, do not rate down.",
      pct_max_zone * 100, ZONE_MAJORITY * 100
    )
  } else if (pct_each_side >= OPPOSITE_EACH) {
    threshold_side <- "opposite_substantial"
    judgment_auto  <- "serious"
    decision_note  <- sprintf(
      "Both directions have substantial mass: n_above = %d (%.0f%%) AND n_below = %d (%.0f%%) >= %.0f%% each -> rate down 2 (clinically opposite).",
      n_above, n_above / n_total * 100,
      n_below, n_below / n_total * 100,
      OPPOSITE_EACH * 100
    )
  } else {
    threshold_side <- "heterogeneous"
    judgment_auto  <- "some_concerns"
    decision_note  <- sprintf(
      "Largest single-zone share %.0f%% < %.0f%% but neither direction reaches %.0f%% -> rate down 1 (heterogeneous magnitude).",
      pct_max_zone * 100, ZONE_MAJORITY * 100, OPPOSITE_EACH * 100
    )
  }

  side_note <- sprintf(
    "AUTO Step 2 (%s): zone counts (k = %d): above_threshold = %d, trivial = %d, below_threshold = %d. %s",
    threshold_label, n_total,
    n_above, n_trivial, n_below,
    decision_note
  )

  if (judgment_auto == "no") {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "no",
      auto     = TRUE,
      notes    = paste0(
        "AUTO Step 1: I2 > 25% -> important heterogeneity detected. ",
        side_note,
        " | ", stat_note
      )
    ))
  }

  if (judgment_auto == "serious") {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "serious",
      auto     = TRUE,
      notes    = paste0(
        "AUTO Step 1: I2 > 25% -> important heterogeneity detected. ",
        side_note,
        " Subgroup explanation not auto-detectable; supply ",
        "inconsistency_subgroup_explained = 'yes' to override. | ",
        stat_note
      )
    ))
  }

  # some_concerns
  make_domain_row(
    domain   = "Inconsistency",
    judgment = "some_concerns",
    auto     = TRUE,
    notes    = paste0(
      "AUTO Step 1: I2 > 25% -> important heterogeneity detected. ",
      side_note, " | ", stat_note
    )
  )
}
