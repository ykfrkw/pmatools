# domain_inconsistency.R - Inconsistency domain assessment
#
# BMJ 2025 Core GRADE 3, Fig 2 flowchart (preserved from v0.1.0).
#
# Threshold (v0.5): Core GRADE 3 Fig 2 node 2 reads, verbatim, "Evaluate point
# estimates of studies **in relation to chosen threshold**". The chosen
# threshold is the one Core GRADE 2 Fig 2 resolved for the rating target, so
# `threshold_chosen` is the SAME value the Imprecision domain uses
# (target_info$threshold_for_imprecision): +/-MID for the important-effect and
# little-to-no-difference targets, and the null (0) for a non-null-effect
# target. Before v0.5 this domain received the raw MID even when Imprecision
# was rating against the null, so the two domains could disagree about the
# boundary; Fig 4 of Core GRADE 3 demonstrates that the choice reverses the
# inconsistency verdict, so they must agree.
#
# Steps:
#   Step 1. Are there important differences in point estimates AND limited CI overlap?
#     NO  -> judgment = "not_serious" (do not rate down)
#     YES -> Step 2
#
#   Step 2. Where do point estimates fall vs the chosen threshold?
#     majority_one_side -> judgment = "not_serious" (manual) or "serious" (auto, conservative)
#     opposite_sides    -> Step 3
#
#   Step 3. Is opposite-sided inconsistency explained by credible subgroup?
#     yes -> judgment = "not_serious" + note
#     no  -> judgment = "very_serious" (-2; see the departure below)
#
# THE OPPOSITE-SIDED BRANCH RATES DOWN TWO LEVELS, AND THAT DEPARTS FROM THE
# SOURCE. Core GRADE 3 (p5-6) verbatim:
#   "A final issue is consideration of rating down twice for inconsistency.
#    Although this is a theoretical possibility, we have found compelling
#    reason to rate down twice for inconsistency sufficiently unusual that it
#    need not concern users of Core GRADE."
# v0.5.0 read that as a cap and stopped every path at "serious" (-1).
# v0.5.1 does not, on the following reasoning, which is stated here rather
# than hidden because the source says otherwise.
#
#   The opposite-sided branch is not "the studies disagree more than the eye
#   likes" -- that is the neighbouring heterogeneous branch, which still rates
#   down one level and is unchanged. This branch fires only when a substantial
#   share of point estimates sits ABOVE the chosen threshold, a substantial
#   share sits BELOW it, and no credible subgroup explains the split. The
#   reviewer cannot say which direction the intervention works in. Reporting
#   such a body of evidence as moderate certainty overstates it.
#
#   Core GRADE 3 calls the two-level case unusual, not wrong, and the 20%
#   each-side gate is exactly what makes it unusual here: ordinary
#   disagreement never reaches it.
#
# .INCONSISTENCY_TWO_LEVEL_NOTE states the departure in the notes wherever
# the branch fires, so no reader meets the -2 without the reasoning. Every
# other path in this file still stops at "serious" (-1), and the scalar
# `inconsistency` override (with a written rationale) remains the way to
# record a judgment the flowchart does not reach -- in either direction.
#
# Auto Step 1 proxy: I^2 > 30%  (v0.5; v0.1.0-v0.4.0 used 25%, and v0.1.0
#   also used "OR Q p < 0.10").
#
#   30% is the ONLY numeric value Core GRADE 3 offers, and it offers it
#   grudgingly. Verbatim:
#     "It is natural that review authors desire hard and fast rules for
#      interpreting I2. The limitations of the statistic make such rules
#      problematic. The best we can do is suggest that one will seldom see
#      serious inconsistency with I2 values <30%, and as I2 rises beyond that
#      value, the possible need to rate down certainty increases."
#   Core GRADE 3's actual Step 1 is a VISUAL judgment, not a statistic
#   (summary points, verbatim): "To address rating down for inconsistency,
#   Core GRADE relies on the visual inspection of forest plots for the
#   magnitude of differences in point estimates, the overlap of confidence
#   intervals, and the relation of study estimates to the chosen threshold of
#   the null effect or minimal important difference."
#   The I^2 gate here is therefore an automation SURROGATE for that visual
#   step. Supply the manual flowchart inputs (inconsistency_ci_diff etc.)
#   after looking at plot_forest() to follow the source faithfully. Every
#   auto-path note states this.
#
# Auto Step 2 proxy:
#   With threshold_chosen > 0:
#     classify TE per study into 3 zones around +/-threshold_chosen;
#     largest single-zone share >= 80% -> majority_one_side -> "not_serious"
#   Without threshold_chosen (null threshold, or none supplied):
#     the trivial zone collapses to {0} and the same 80% rule is applied
#     around the null.
#
#   The 80% (ZONE_MAJORITY) and 20% (OPPOSITE_EACH) cut-offs are NOT from Core
#   GRADE. Core GRADE 3 Fig 2 words the node qualitatively -- its only vocabulary
#   is "Majority are on one side of threshold" and "Point estimates of
#   substantial proportion of [studies on opposite sides]" -- and gives no
#   numeric definition of "majority" or "substantial proportion". 0.80 follows
#   CINeMA (Nikolakopoulou 2020); 0.20 is a pmatools convention. Both are
#   stated in the domain notes.
#
# Auto Step 3: cannot be auto-detected, so it is ASKED. When the zone tally
#   lands on opposite sides, `inconsistency_subgroup_explained` is read on the
#   auto path exactly as it is on the manual one: "yes" -> "not_serious" (do not rate
#   down, present the subgroups separately), "no" -> "very_serious" (-2),
#   unanswered -> "very_serious" (-2) with a note pointing at the argument.
#   Before v0.5.1 the note pointed at an argument the auto path ignored;
#   answering it switched the domain onto the manual path, which then demanded
#   inconsistency_threshold_side as well.
#
# I^2 / tau^2 / Q statistics are always shown in notes but never drive the
# judgment beyond the Step 1 gate above.

# --------------------------------------------------------------------------
# Flowchart node vocabulary (inst/figures/incon.svg)
#
# See the note on .ROB_FIG2_NODE_IDS in domain_rob.R: these are the <g> ids in
# that file, the "flow_path" fact names the ones this assessment traversed,
# and tests/testthat/test-flowchart-nodes.R holds the two in step.
#
# The manual flowchart path (inconsistency_ci_diff etc.) and the automated one
# answer the SAME three questions, so both emit paths through these ids; only
# the scalar override records none, because it does not run the flowchart.
.INCON_FIG2_NODE_IDS <- c(
  "pma-incon-node-step1",
  "pma-incon-edge-step1-no",
  "pma-incon-leaf-nodown1",
  "pma-incon-edge-step1-yes",
  "pma-incon-node-step2",
  "pma-incon-edge-step2-majority",
  "pma-incon-leaf-nodown2",
  "pma-incon-edge-step2-scattered",
  "pma-incon-leaf-down1-scattered",
  "pma-incon-edge-step2-opposite",
  "pma-incon-node-step3",
  "pma-incon-edge-step3-yes",
  "pma-incon-leaf-nodown3",
  "pma-incon-edge-step3-no",
  "pma-incon-leaf-down2"
)

# Appended to the opposite-sided branch, and to nothing else. That branch is
# the ONE place in this file that rates down two levels, and it is a declared
# departure from the source rather than an implementation detail, so the note
# says so in the same sentence that reports the judgment. The neighbouring
# heterogeneous branch still stops at -1 and carries no such note, because
# nothing about it departs from anything.
.INCONSISTENCY_TWO_LEVEL_NOTE <- paste0(
  "Rated down TWO levels (serious). This departs from Core GRADE 3, which ",
  "states that a compelling reason to rate down twice for inconsistency is ",
  "sufficiently unusual that it need not concern Core GRADE users, and which ",
  "therefore describes no two-level inconsistency downgrade. pmatools applies ",
  "one here because this branch is not ordinary disagreement between studies: ",
  "it fires only when a substantial share of point estimates lies above the ",
  "chosen threshold, a substantial share lies below it, and no credible ",
  "subgroup explains the split, so the direction of the effect is unresolved ",
  "and reporting the body of evidence as moderate certainty would overstate ",
  "it. Core GRADE 3 calls the case unusual rather than wrong, and the 20% ",
  "each-side gate is what keeps it unusual: scattered estimates that do not ",
  "reach it take the heterogeneous branch and rate down one level. Supply the ",
  "scalar override inconsistency = 'some_concerns' (= Core GRADE 'serious', rate down 1) with ",
  "inconsistency_rationale to rate down one level instead."
)

# Step 1 cut-off for the automated path. Core GRADE 3's only number, quoted in
# the file header; see there for why it is a surrogate rather than a rule.
INCONSISTENCY_I2_CUT <- 30

# Attached to every AUTO-path note: names the surrogate and points at the
# faithful manual route.
.INCONSISTENCY_I2_CAVEAT <- paste0(
  "The I2 gate is a pmatools automation surrogate for Core GRADE 3 Step 1, ",
  "which is a visual judgment ('Core GRADE relies on the visual inspection of ",
  "forest plots'). Core GRADE 3 gives 30% as its only numeric hint while ",
  "warning that 'the limitations of the statistic make such rules ",
  "problematic'. Inspect plot_forest() and supply inconsistency_ci_diff / ",
  "inconsistency_threshold_side to follow the source directly."
)

# Attached to every AUTO Step 2 note: names the operational cut-offs.
.INCONSISTENCY_ZONE_CAVEAT <- paste0(
  "Core GRADE 3 Fig 2 words this node qualitatively ('Majority are on one ",
  "side of threshold' / 'substantial proportion') and defines no numbers; the ",
  "80% majority share follows CINeMA (Nikolakopoulou A, et al. PLoS Med. ",
  "2020) and the 20% ",
  "each-side share is a pmatools convention."
)

# Attached wherever the subgroup question decides the judgment.
.INCONSISTENCY_SUBGROUP_CAVEAT <- paste0(
  "Subgroup credibility is not auto-detectable. Core GRADE 3 keys it to the ",
  "interaction P value, whether the comparison is within-study, and whether a ",
  "small number of direction-specifying a priori hypotheses was made, and ",
  "points at ICEMAN (Schandelmaier S, et al. CMAJ. 2020). Core GRADE 3 also ",
  "states that 'a conclusion of moderate ",
  "or high credibility warrants the creation of separate PICO questions for ",
  "each subgroup', so a credible subgroup effect should be split into ",
  "separate ratings rather than reported as a single pooled estimate."
)

# Structured companions to stat_note. Emitted on every path that has the
# statistics (all of them), including those that do not rate down: the
# renderers decide what to show. A statistic {meta} did not produce is simply
# absent rather than reported as a placeholder, which is what stat_note's
# 0 / 0 / 1 substitutions would otherwise imply.
.inconsistency_stat_facts <- function(i2_pct, tau2, pval_q) {
  f_i2 <- if (!is.null(i2_pct) && length(i2_pct) == 1L && is.finite(i2_pct)) {
    .fact("i2", "I-squared", sprintf("%.1f%%", i2_pct), i2_pct)
  } else NULL
  f_tau2 <- if (!is.null(tau2) && length(tau2) == 1L && is.finite(tau2)) {
    .fact("tau2", "Tau-squared", sprintf("%.4f", tau2), tau2)
  } else NULL
  f_q <- if (!is.null(pval_q) && length(pval_q) == 1L && is.finite(pval_q)) {
    .fact("q_pvalue", "Cochran Q p value", sprintf("%.3f", pval_q), pval_q)
  } else NULL
  list(f_i2, f_tau2, f_q)
}

assess_inconsistency <- function(meta_obj,
                                 inconsistency                    = NULL,
                                 inconsistency_ci_diff            = NULL,
                                 inconsistency_threshold_side     = NULL,
                                 inconsistency_subgroup_explained = NULL,
                                 threshold_chosen                 = NULL,
                                 rationale                        = NULL) {

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

  stat_facts <- .inconsistency_stat_facts(i2_pct, tau2, pval_q)

  # ----- Path A: scalar override -----
  # v0.4.0 (breaking): the scalar override replaces the flowchart / automated
  # assessment, so inconsistency_rationale is mandatory.
  if (!is.null(inconsistency)) {
    validate_grade_level(inconsistency, "inconsistency")
    .check_override_rationale(rationale, "inconsistency_rationale",
                              "Inconsistency")
    return(make_domain_row(
      domain    = "Inconsistency",
      judgment  = inconsistency,
      auto      = FALSE,
      notes     = paste0(
        "Overall judgment provided by user (scalar; flowchart not applied). ",
        stat_note
      ),
      rationale = rationale,
      facts     = .facts(stat_facts)
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
        judgment = "not_serious",
        auto     = FALSE,
        notes    = paste0(
          "FLOWCHART Step 1: No important differences in point estimates / ",
          "adequate CI overlap -> do not rate down. | ", stat_note
        ),
        facts    = .facts(c(stat_facts, list(.flow_path_fact(c(
          "pma-incon-node-step1", "pma-incon-edge-step1-no",
          "pma-incon-leaf-nodown1")))))
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
        judgment = "not_serious",
        auto     = FALSE,
        notes    = paste0(
          "FLOWCHART Step 2: Important CI differences exist, but majority of point ",
          "estimates are on one side of clinical Threshold -> do not rate down ",
          "(per BMJ Core GRADE 3 flowchart). | ",
          stat_note
        ),
        facts    = .facts(c(stat_facts, list(.flow_path_fact(c(
          "pma-incon-node-step1", "pma-incon-edge-step1-yes",
          "pma-incon-node-step2", "pma-incon-edge-step2-majority",
          "pma-incon-leaf-nodown2")))))
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
        judgment = "not_serious",
        auto     = FALSE,
        notes    = paste0(
          "FLOWCHART Step 3: Opposite-sided estimates explained by credible subgroup ",
          "-> do not rate down; present subgroup results separately. ",
          .INCONSISTENCY_SUBGROUP_CAVEAT, " | ",
          stat_note
        ),
        facts    = .facts(c(stat_facts, list(.flow_path_fact(c(
          "pma-incon-node-step1", "pma-incon-edge-step1-yes",
          "pma-incon-node-step2", "pma-incon-edge-step2-opposite",
          "pma-incon-node-step3", "pma-incon-edge-step3-yes",
          "pma-incon-leaf-nodown3")))))
      ))
    }

    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "very_serious",
      auto     = FALSE,
      notes    = paste0(
        "FLOWCHART Step 3: Opposite-sided estimates not explained by subgroup ",
        "-> rate down two levels. ", .INCONSISTENCY_SUBGROUP_CAVEAT, " ",
        .INCONSISTENCY_TWO_LEVEL_NOTE, " | ", stat_note
      ),
      facts    = .facts(c(stat_facts, list(.flow_path_fact(c(
        "pma-incon-node-step1", "pma-incon-edge-step1-yes",
        "pma-incon-node-step2", "pma-incon-edge-step2-opposite",
        "pma-incon-node-step3", "pma-incon-edge-step3-no",
        "pma-incon-leaf-down2")))))
    ))
  }

  # ----- Path C: auto-detect -----
  # inconsistency_subgroup_explained travels into the auto path too. Core
  # GRADE 3's Step 3 is the one node the algorithm cannot reach on its own
  # (subgroup credibility is a human judgment; see
  # .INCONSISTENCY_SUBGROUP_CAVEAT), and the auto note at the opposite-sides
  # branch has always told the reviewer to supply it. Until v0.5.1 that advice
  # was a no-op: answering it switched the whole domain onto the manual path,
  # which then demanded inconsistency_threshold_side as well.
  .auto_inconsistency(meta_obj, i2_pct, stat_note, threshold_chosen,
                      stat_facts = stat_facts,
                      inconsistency_subgroup_explained =
                        inconsistency_subgroup_explained)
}

# --------------------------------------------------------------------------
# Auto-detect path
# --------------------------------------------------------------------------
.auto_inconsistency <- function(meta_obj, i2_pct, stat_note,
                                threshold_chosen = NULL, stat_facts = NULL,
                                inconsistency_subgroup_explained = NULL) {

  # Validated here as well as on the manual path: the argument now has two
  # entry points and a typo must not be silently read as "not answered".
  if (!is.null(inconsistency_subgroup_explained) &&
      !inconsistency_subgroup_explained %in% c("yes", "no")) {
    rlang::abort("inconsistency_subgroup_explained must be 'yes' or 'no'.")
  }

  # Step 1 surrogate: I^2 > 30% (see INCONSISTENCY_I2_CUT / file header)
  cut <- INCONSISTENCY_I2_CUT
  has_i2 <- !is.na(i2_pct)
  ci_diff_yes <- has_i2 && (i2_pct > cut)

  if (!ci_diff_yes) {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "not_serious",
      auto     = TRUE,
      notes    = paste0(
        sprintf(paste0("AUTO Step 1: No important heterogeneity (I2 <= %d%%) ",
                       "-> do not rate down. "), cut),
        .INCONSISTENCY_I2_CAVEAT, " | ",
        stat_note
      ),
      # Step 2 never ran, so there are no zone facts to record.
      facts    = .facts(c(stat_facts, list(.flow_path_fact(c(
        "pma-incon-node-step1", "pma-incon-edge-step1-no",
        "pma-incon-leaf-nodown1")))))
    ))
  }

  # Step 2 proxy
  te_vec <- meta_obj$TE
  if (is.null(te_vec) || length(te_vec) < 2) {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "serious",
      auto     = TRUE,
      notes    = paste0(
        sprintf(paste0("AUTO Step 1: I2 > %d%%; AUTO Step 2 not assessable ",
                       "(study-level TEs unavailable); judgment = ",
                       "'serious' (conservative). "), cut),
        .INCONSISTENCY_I2_CAVEAT, " | ", stat_note
      ),
      # The path stops at the Step 2 node on purpose: the question was
      # reached but could not be answered, so no branch out of it is
      # highlighted and the picture shows exactly how far the algorithm got.
      facts    = .facts(c(stat_facts, list(.flow_path_fact(c(
        "pma-incon-node-step1", "pma-incon-edge-step1-yes",
        "pma-incon-node-step2")))))
    ))
  }

  k <- length(te_vec)
  te_vec <- te_vec[!is.na(te_vec)]

  # Zone classification:
  #   max single-zone share >= 80%               -> "not_serious" (consistent direction)
  #   both directions have substantial mass      -> "very_serious" (-2)
  #     (n_above/k >= 20% AND n_below/k >= 20%)
  #   else                                       -> "serious" (-1)
  #
  # PROVENANCE: neither number is from Core GRADE. 0.80 follows CINeMA
  # (Nikolakopoulou 2020); 0.20 is a pmatools convention. Core GRADE 3 Fig 2
  # says only "Majority are on one side of threshold" and "Point estimates of
  # substantial proportion of [studies]" and never quantifies either phrase.
  # The substantial-both-directions criterion is our operationalisation of
  # Core GRADE 3's "point estimates on opposite sides of threshold" trigger,
  # and the only path here that reaches -2 -- deliberately, and against the
  # source; the file header and .INCONSISTENCY_TWO_LEVEL_NOTE give the
  # reasoning. The 20% gate is what separates it from the scattered tally
  # below, which stays at -1.
  ZONE_MAJORITY    <- 0.80
  OPPOSITE_EACH    <- 0.20

  if (!is.null(threshold_chosen) && !is.na(threshold_chosen) && threshold_chosen > 0) {
    M <- threshold_chosen
    n_above   <- sum(te_vec > +M)
    n_below   <- sum(te_vec < -M)
    n_trivial <- length(te_vec) - n_above - n_below
  } else {
    # Fallback: null = 0 (no Threshold supplied). "trivial" zone collapses to 0.
    M <- 0
    n_above   <- sum(te_vec > 0)
    n_below   <- sum(te_vec < 0)
    n_trivial <- 0L
  }

  n_total       <- length(te_vec)
  zone_counts   <- c(n_above, n_trivial, n_below)
  pct_max_zone  <- max(zone_counts) / n_total
  pct_each_side <- min(n_above, n_below) / n_total

  # The label names the CHOSEN threshold (Core GRADE 3 Fig 2: "Evaluate point
  # estimates of studies in relation to chosen threshold"), which is the same
  # one the Imprecision domain rates against.
  threshold_label <- if (M > 0) {
    sprintf("vs +/-Threshold = +/-%g (chosen threshold; same as Imprecision)", M)
  } else {
    paste0("vs null = 0 (chosen threshold is the null; same as Imprecision)")
  }

  if (pct_max_zone >= ZONE_MAJORITY) {
    threshold_side <- "majority_one_side"
    judgment_auto  <- "not_serious"
    decision_note  <- sprintf(
      "Largest single-zone share %.0f%% >= %.0f%% -> direction consistent, do not rate down.",
      pct_max_zone * 100, ZONE_MAJORITY * 100
    )
  } else if (pct_each_side >= OPPOSITE_EACH) {
    threshold_side <- "opposite_substantial"
    judgment_auto  <- "very_serious"
    decision_note  <- sprintf(
      "Both directions have substantial mass: n_above = %d (%.0f%%) AND n_below = %d (%.0f%%) >= %.0f%% each -> rate down 2 (clinically opposite).",
      n_above, n_above / n_total * 100,
      n_below, n_below / n_total * 100,
      OPPOSITE_EACH * 100
    )
  } else {
    threshold_side <- "heterogeneous"
    judgment_auto  <- "serious"
    decision_note  <- sprintf(
      "Largest single-zone share %.0f%% < %.0f%% but neither direction reaches %.0f%% -> rate down 1 (heterogeneous magnitude).",
      pct_max_zone * 100, ZONE_MAJORITY * 100, OPPOSITE_EACH * 100
    )
  }

  side_note <- sprintf(
    "AUTO Step 2 (%s): zone counts (k = %d): above_threshold = %d, trivial = %d, below_threshold = %d. %s %s",
    threshold_label, n_total,
    n_above, n_trivial, n_below,
    decision_note, .INCONSISTENCY_ZONE_CAVEAT
  )

  step1_note <- sprintf(
    "AUTO Step 1: I2 > %d%% -> important heterogeneity detected. %s ",
    cut, .INCONSISTENCY_I2_CAVEAT
  )

  # Structured companions to side_note. The threshold named here is the CHOSEN
  # one (Core GRADE 3 Fig 2 node 2), the same one Imprecision rates against.
  f_zone_counts <- .fact(
    "zone_counts", "Study estimates relative to the chosen threshold",
    if (M > 0) {
      sprintf("%d above, %d within, %d below (threshold +/-%.3g, k = %d)",
              n_above, n_trivial, n_below, M, n_total)
    } else {
      sprintf("%d above, %d below (threshold: the null, k = %d)",
              n_above, n_below, n_total)
    },
    n_total
  )
  f_zone_decision <- .fact(
    "zone_decision", "Zone shares", decision_note, pct_max_zone
  )
  zone_facts <- list(f_zone_counts, f_zone_decision)

  # Everything below reached Step 2 through Step 1's "yes" edge; which edge it
  # leaves by is the zone tally's verdict.
  flow_to_step2 <- c("pma-incon-node-step1", "pma-incon-edge-step1-yes",
                     "pma-incon-node-step2")

  if (judgment_auto == "not_serious") {
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "not_serious",
      auto     = TRUE,
      notes    = paste0(
        step1_note,
        side_note,
        " | ", stat_note
      ),
      facts    = .facts(c(stat_facts, zone_facts, list(.flow_path_fact(c(
        flow_to_step2, "pma-incon-edge-step2-majority",
        "pma-incon-leaf-nodown2")))))
    ))
  }

  if (identical(threshold_side, "opposite_substantial")) {
    # AUTO Step 3. The zone tally has put the estimates on opposite sides of
    # the threshold, which is exactly the node Core GRADE 3 hands to a human:
    # a credible subgroup explanation removes the concern (and, per
    # .INCONSISTENCY_SUBGROUP_CAVEAT, should be split into separate PICO
    # questions). Unanswered keeps the conservative -1.
    if (identical(inconsistency_subgroup_explained, "yes")) {
      return(make_domain_row(
        domain   = "Inconsistency",
        judgment = "not_serious",
        auto     = TRUE,
        notes    = paste0(
          step1_note,
          side_note,
          " AUTO Step 3: opposite-sided estimates explained by a credible ",
          "subgroup (inconsistency_subgroup_explained = 'yes') -> do not ",
          "rate down; present subgroup results separately. ",
          .INCONSISTENCY_SUBGROUP_CAVEAT, " | ",
          stat_note
        ),
        facts    = .facts(c(stat_facts, zone_facts, list(.flow_path_fact(c(
          flow_to_step2, "pma-incon-edge-step2-opposite",
          "pma-incon-node-step3", "pma-incon-edge-step3-yes",
          "pma-incon-leaf-nodown3")))))
      ))
    }
    step3_note <- if (identical(inconsistency_subgroup_explained, "no")) {
      paste0(" AUTO Step 3: opposite-sided estimates NOT explained by a ",
             "credible subgroup (inconsistency_subgroup_explained = 'no') ",
             "-> rate down two levels. ")
    } else {
      " Supply inconsistency_subgroup_explained = 'yes' to override. "
    }
    return(make_domain_row(
      domain   = "Inconsistency",
      judgment = "very_serious",
      auto     = TRUE,
      notes    = paste0(
        step1_note,
        side_note,
        step3_note,
        .INCONSISTENCY_SUBGROUP_CAVEAT, " ",
        .INCONSISTENCY_TWO_LEVEL_NOTE, " | ",
        stat_note
      ),
      # Unanswered lands here too, and takes the same "no" edge: the default
      # is the conservative one, and the picture should show which leaf the
      # judgment actually came from rather than stopping at the question.
      facts    = .facts(c(stat_facts, zone_facts, list(.flow_path_fact(c(
        flow_to_step2, "pma-incon-edge-step2-opposite",
        "pma-incon-node-step3", "pma-incon-edge-step3-no",
        "pma-incon-leaf-down2")))))
    ))
  }

  # serious (rate down 1 level)
  make_domain_row(
    domain   = "Inconsistency",
    judgment = "serious",
    auto     = TRUE,
    notes    = paste0(
      step1_note,
      side_note, " | ", stat_note
    ),
    facts    = .facts(c(stat_facts, zone_facts, list(.flow_path_fact(c(
      flow_to_step2, "pma-incon-edge-step2-scattered",
      "pma-incon-leaf-down1-scattered")))))
  )
}
