# domain_pubias.R - Publication bias domain assessment
#
# v0.6: Core GRADE Guide Fig 9-1 (Guyatt G, et al. Core GRADE Guide,
# https://core-grade.gitbook.io/org, accessed 2026-11-02), which replaces BMJ
# 2025 Core GRADE 4 Fig 5. The Guide adds one node in front of Fig 5's four
# and rewords Q3; the rest is unchanged.
#
# This file owns the publication-bias judgment: it reads a {meta} object, the
# reviewer's answers to the nodes the algorithm cannot compute, and the
# rating-target threshold grade_meta() resolved (R/rating_target.R), and hands
# back one make_domain_row() tibble (R/domain_row.R) with its facts. The Shiny
# wizard (shiny/R/step3_pubias.R, step3_threshold.R) mirrors its evaluation
# order and must move with it. A function belongs here when it decides the
# publication-bias judgment; trim-and-fill and the missing-results displays,
# which decide nothing, live in R/pubias_trimfill.R and R/pubias_missing.R.
#
# Guide Fig 9-1, verbatim nodes:
#   Q0. "Did the studies show an important effect?"       No -> Do not rate down
#   Q1. "Are most or all of the studies small and industry sponsored?"
#                                                          Yes -> Rate down
#   Q2. "Are there enough studies (>=10) to examine funnel plot asymmetry?"
#   Q3. "Does visual inspection of funnel plot demonstrate substantial
#        asymmetry? and/or Is funnel plot asymmetry statistically
#        significant?"                                     Yes -> Rate down
#   Q4. "Did the review team discover and document unpublished studies of
#        sufficient size to appreciably influence effect estimates (e.g.,
#        registry of FDA)"                                 Yes -> Rate down
# Every leaf is "Rate down" / "Do not rate down"; the figure never rates down
# two levels for publication bias.
#
# pmatools implementation:
#
#   Q0. Important effect? Auto-derived unless pubias_important_effect is given:
#         YES iff the pooled point estimate lies beyond the rating-target
#         threshold (threshold_chosen, analysis scale, measured from the null
#         on either side, strict `>`). With a null threshold (NULL / NA / 0)
#         any non-null estimate is "yes", so the node effectively always
#         answers yes and the note says so. An unavailable estimate is
#         assumed "yes" (conservative). NO -> "not_serious", stop.
#         The "Q0:" note prefix keeps Fig 5's Q1-Q4 prefixes where exports
#         already have them.
#         A manual "no" overrides the derivation and needs pubias_rationale;
#         a manual "yes" only removes a reason not to look and needs none.
#
#   Q1. pubias_small_industry == "yes" -> "serious" (-1)
#         else -> registry check, then Q2 (default "no" assumption with note)
#
#   [After Q1] pubias_registry_complete == "yes"
#         -> "not_serious" (a pmatools convenience input, NOT a node of Fig
#            9-1 or of Fig 5; see the note text). Evaluated AFTER Q1 so that a
#            body of small industry-sponsored trials still rates down.
#
#   Q2. k >= 10 ?  YES -> Q3 (.pubias_statistical)  NO -> Q4 (.pubias_registry)
#
#   Q3 (k >= 10): pubias_funnel_asymmetry "yes"/"no" (manual, needs
#       pubias_rationale), else Egger p < 0.05 -> "serious"; a test that fails
#       to run -> "not_serious" + "QUALITATIVE ASSESSMENT REQUIRED". The
#       p-value cut-off is a pmatools convention: the Guide's "statistically
#       significant" names no alpha, and Fig 5 asked only whether asymmetry
#       "strongly suggests publication bias".
#
#   Q4 (k < 10): pubias_unpublished "yes" -> "serious"; "no" -> "not_serious";
#       NULL -> assume "no" with a warning.
#
# Rate down at most ONE level: "very_serious" (-2) is reachable only through a
# scalar override elsewhere in the package, never from this flowchart.
#
# Trim-and-fill is not used to drive the GRADE judgment; see R/pubias_trimfill.R.

# --------------------------------------------------------------------------
# Flowchart node vocabulary (inst/figures/pubias.svg)
#
# See the note on .ROB_FIG2_NODE_IDS in domain_rob.R. The constant keeps its
# FIG5 name for the same reason. The registry node is NOT a node of the
# source -- its only registry node is Q4, reached when k < 10 -- and the box
# says so on its own third line.
#
# The ids keep their q0..q4 slugs, but the FIGURE prints no numbers: it
# interleaves the registry node between Q1 and Q2, so numbering on screen
# described neither the source nor the route. The "Q0:" - "Q4:" prefixes in
# the notes are unaffected -- they are the exported record.
#
# There is no "qualitative assessment required" leaf. Both branches that used
# to have one judge the domain "no", so they light the "do not rate down" leaf
# they actually reach; the caveat travels in the note and in an rlang::warn().
.PUBIAS_FIG5_NODE_IDS <- c(
  "pma-pubias-node-q0",
  "pma-pubias-edge-q0-no",
  "pma-pubias-leaf-nodown-q0",
  "pma-pubias-edge-q0-yes",
  "pma-pubias-node-q1",
  "pma-pubias-edge-q1-yes",
  "pma-pubias-leaf-down1-q1",
  "pma-pubias-edge-q1-no",
  "pma-pubias-node-registry",
  "pma-pubias-edge-registry-yes",
  "pma-pubias-leaf-nodown-registry",
  "pma-pubias-edge-registry-no",
  "pma-pubias-node-q2",
  "pma-pubias-edge-q2-yes",
  "pma-pubias-node-q3",
  "pma-pubias-edge-q3-yes",
  "pma-pubias-leaf-down1-q3",
  "pma-pubias-edge-q3-no",
  "pma-pubias-leaf-nodown-q3",
  "pma-pubias-edge-q2-no",
  "pma-pubias-node-q4",
  "pma-pubias-edge-q4-yes",
  "pma-pubias-leaf-down1-q4",
  "pma-pubias-edge-q4-no",
  "pma-pubias-leaf-nodown-q4"
)

# Every path past Q0 starts here.
.PUBIAS_FLOW_Q0_YES <- c("pma-pubias-node-q0", "pma-pubias-edge-q0-yes")

# Everything past Q1 has come through the registry node, whether or not the
# reviewer answered it.
.PUBIAS_FLOW_TO_Q2 <- c(.PUBIAS_FLOW_Q0_YES,
                        "pma-pubias-node-q1", "pma-pubias-edge-q1-no",
                        "pma-pubias-node-registry",
                        "pma-pubias-edge-registry-no",
                        "pma-pubias-node-q2")

# The study count the flowchart's Q2 actually branches on. Recorded on every
# path: k is the one number the reviewer needs to see to check that Q2 was
# answered the way they expect. The bare count, because a SoF footnote has no
# flowchart beside it to say what a "Q2" is.
.pubias_k_fact <- function(k) {
  .fact("k", "Studies contributing a usable estimate",
        sprintf("%d", as.integer(k)), as.numeric(k))
}

# Marker prepended to the publication-bias note whenever no statistical test
# and no manual input decided the judgment, so downstream outputs (SoF table,
# Evidence Profile, grade_table, grade_report) can surface it prominently.
.PUBIAS_QUALITATIVE_MARKER <- "QUALITATIVE ASSESSMENT REQUIRED"

# Guidance text shared by the two not-formally-assessed branches.
.pubias_qualitative_guidance <- function() {
  paste0(
    "Judgment defaults to 'no' (no downgrade) pending a qualitative ",
    "assessment: inspect the contour-enhanced funnel plot for asymmetry, ",
    "review the comprehensiveness of the search, and check trial-registry ",
    "completeness for unpublished studies. Record the conclusion via ",
    "pubias_funnel_asymmetry / pubias_unpublished ",
    "(or pubias_registry_complete = 'yes')."
  )
}

# Return the publication-bias note from a pmatools object (or a
# domain_assessments tibble) when the domain could not be formally assessed
# and requires a qualitative judgment; NULL otherwise.
.pubias_qualitative_note <- function(x) {
  d <- if (inherits(x, "pmatools")) x$domain_assessments else x
  if (!is.data.frame(d) || !all(c("domain", "notes") %in% names(d))) {
    return(NULL)
  }
  r <- d[d$domain == "Publication bias", , drop = FALSE]
  if (nrow(r) == 0) return(NULL)
  note <- r$notes[1]
  if (is.na(note) ||
      !grepl(.PUBIAS_QUALITATIVE_MARKER, note, fixed = TRUE)) {
    return(NULL)
  }
  note
}

# "yes" / "no" / NULL, or an abort naming the argument.
.check_pubias_yes_no <- function(x, arg) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !x %in% c("yes", "no")) {
    rlang::abort(paste0(arg, " must be 'yes' or 'no'."))
  }
  invisible(x)
}

# --------------------------------------------------------------------------
# Q0: "Did the studies show an important effect?" (Guide Fig 9-1, first node)
#
# Answered from the pooled point estimate of the analysis being rated and the
# rating-target threshold grade_meta() resolved: +/-MID for an important-effect
# or little-or-no-difference target, the null for a non-null-effect target.
# The comparison is strict, matching the rating target's own |TE| > MID.
#
# Publication bias inflates apparent effects, so an evidence body that shows
# no important effect has nothing for it to have manufactured -- that is the
# reading the node encodes, and why "yes" is the safe assumption whenever the
# estimate cannot be read.
# --------------------------------------------------------------------------
.pubias_important_effect <- function(meta_obj, threshold_chosen = NULL,
                                     manual = NULL) {
  te    <- .pooled_te(meta_obj)
  M     <- if (.has_mid(threshold_chosen)) threshold_chosen else 0
  scale <- .rob_scale(meta_obj$sm)
  te_txt <- if (is.finite(te)) {
    sprintf("%s %s", scale$sm_label, .rob_fact_num(scale$disp(te)))
  } else "not available"
  thr_txt <- if (M > 0) {
    sprintf("beyond the threshold (+/-%s on the analysis scale)",
            format(signif(M, 3)))
  } else "away from the null (null threshold)"

  if (!is.null(manual)) {
    note <- sprintf(paste0(
      "Q0 (manual): the studies %s an important effect (pubias_important_",
      "effect = '%s'; pooled estimate %s)%s"),
      if (manual == "yes") "show" else "do not show", manual, te_txt,
      if (manual == "yes") ". " else " -> do not rate down. ")
    return(list(answer = manual, auto = FALSE, note = note,
                fact = .fact("important_effect",
                             "Did the studies show an important effect?",
                             sprintf("%s (manual)", manual),
                             as.numeric(manual == "yes"))))
  }

  if (!is.finite(te)) {
    answer <- "yes"
    why    <- paste0("the pooled estimate is not available, so an important ",
                     "effect is assumed (conservative)")
  } else if (M > 0) {
    answer <- if (abs(te) > M) "yes" else "no"
    why    <- sprintf("pooled estimate %s (|TE| = %s) %s %s", te_txt,
                      format(signif(abs(te), 3)),
                      if (answer == "yes") "lies" else "does not lie", thr_txt)
  } else {
    answer <- if (abs(te) > 0) "yes" else "no"
    why    <- sprintf(paste0(
      "pooled estimate %s compared with the null (the rating target sets no ",
      "threshold); with a null threshold this node effectively always answers ",
      "yes"), te_txt)
  }
  note <- sprintf("Q0 (auto): %s -> important effect: %s%s", why, answer,
                  if (answer == "yes") ". " else "; do not rate down. ")
  list(answer = answer, auto = TRUE, note = note,
       fact = .fact("important_effect",
                    "Did the studies show an important effect?",
                    sprintf("%s (auto; %s)", answer, why),
                    as.numeric(answer == "yes")))
}

assess_pubias <- function(meta_obj,
                          pubias_small_industry    = NULL,
                          pubias_funnel_asymmetry  = NULL,
                          pubias_unpublished       = NULL,
                          pubias_registry_complete = NULL,
                          rationale                = NULL,
                          # Rare-event routing; see the Q2 gate below. Defaults
                          # off, so an ordinary analysis is unaffected.
                          rare_flow                = FALSE,
                          # Q0. The threshold is the rating target's (analysis
                          # scale); NULL means a null threshold.
                          pubias_important_effect  = NULL,
                          threshold_chosen         = NULL) {
  k <- .pubias_effective_k(meta_obj)
  if (is.null(k) || is.na(k)) k <- 0L

  # Every input is validated before Q0 can stop the walk, so a malformed
  # answer aborts whether or not its node is reached.
  .check_pubias_yes_no(pubias_important_effect,  "pubias_important_effect")
  .check_pubias_yes_no(pubias_small_industry,    "pubias_small_industry")
  .check_pubias_yes_no(pubias_funnel_asymmetry,  "pubias_funnel_asymmetry")
  .check_pubias_yes_no(pubias_unpublished,       "pubias_unpublished")
  .check_pubias_yes_no(pubias_registry_complete, "pubias_registry_complete")

  # v0.4.0 (breaking): pubias_funnel_asymmetry is the explicit override path
  # (a manual visual judgment replaces the automated Egger's test), so it
  # always requires pubias_rationale. v0.6: so does a manual "no" at Q0, which
  # replaces the derivation and ends the assessment. The other inputs supply
  # information the automated path cannot know and are not overrides.
  if (!is.null(pubias_funnel_asymmetry)) {
    .check_override_rationale(rationale, "pubias_rationale",
                              "Publication bias")
  }
  if (identical(pubias_important_effect, "no")) {
    .check_override_rationale(rationale, "pubias_rationale",
                              "Publication bias (important effect)")
  }

  # --- Q0: important effect? (Guide Fig 9-1, first node) -------------------
  q0 <- .pubias_important_effect(meta_obj, threshold_chosen,
                                 pubias_important_effect)
  if (identical(q0$answer, "no")) {
    supplied <- c(
      pubias_small_industry    = !is.null(pubias_small_industry),
      pubias_registry_complete = !is.null(pubias_registry_complete),
      pubias_funnel_asymmetry  = !is.null(pubias_funnel_asymmetry),
      pubias_unpublished       = !is.null(pubias_unpublished))
    return(make_domain_row(
      domain    = "Publication bias",
      judgment  = "not_serious",
      auto      = q0$auto,
      notes     = paste0(
        q0$note,
        "Per Core GRADE Guide Fig 9-1 the remaining nodes are not evaluated.",
        if (any(supplied)) {
          paste0(" Supplied but not evaluated: ",
                 paste(names(supplied)[supplied], collapse = ", "), ".")
        } else ""
      ),
      rationale = if (!q0$auto) rationale else NULL,
      facts     = .facts(.pubias_k_fact(k), q0$fact, .flow_path_fact(c(
        "pma-pubias-node-q0", "pma-pubias-edge-q0-no",
        "pma-pubias-leaf-nodown-q0")))
    ))
  }
  q0_note <- q0$note

  # --- Q1: Small + industry-sponsored ----------------------------------------
  if (!is.null(pubias_small_industry)) {
    if (pubias_small_industry == "yes") {
      return(make_domain_row(
        domain   = "Publication bias",
        judgment = "serious",
        auto     = FALSE,
        notes    = paste0(
          q0_note,
          "Q1: Most/all studies are small AND industry-sponsored ",
          "-> rate down 1 (serious).",
          if (identical(pubias_registry_complete, "yes")) paste0(
            " pubias_registry_complete = 'yes' was also supplied, but it is ",
            "evaluated only after Q1: the flowchart's small-and-industry-",
            "sponsored node comes first, and asserting registry coverage ",
            "does not remove that concern."
          ) else ""
        ),
        facts    = .facts(.pubias_k_fact(k), q0$fact, .flow_path_fact(c(
          .PUBIAS_FLOW_Q0_YES,
          "pma-pubias-node-q1", "pma-pubias-edge-q1-yes",
          "pma-pubias-leaf-down1-q1")))
      ))
    }
    q1_note <- paste0(q0_note,
                      "Q1: Not dominated by small industry-sponsored studies. ")
  } else {
    q1_note <- paste0(q0_note,
                      "Q1: pubias_small_industry not specified; assumed 'no'. ")
  }

  # --- Post-Q1: user-asserted complete pre-registration coverage ------------
  # Not a node of the source. Its only registry node is Q4, reached when
  # k < 10. This input records the USER'S claim that every registered trial
  # can be accounted for and short-circuits the remaining nodes; the note says
  # whose claim it is.
  if (identical(pubias_registry_complete, "yes")) {
    return(make_domain_row(
      domain   = "Publication bias",
      judgment = "not_serious",
      auto     = FALSE,
      notes    = paste0(
        q1_note,
        "pubias_registry_complete = 'yes': the user asserts that ",
        "pre-registration is universal in this field and that all registered ",
        "trials are accounted for, so the remaining nodes are not ",
        "evaluated -> do not rate down. This rule-out is a pmatools input, ",
        "not a decision node of Core GRADE Guide Fig 9-1 (or of Core GRADE 4 ",
        "Fig 5), and rests entirely on that assertion."
      ),
      facts     = .facts(.pubias_k_fact(k), q0$fact, .flow_path_fact(c(
        .PUBIAS_FLOW_Q0_YES,
        "pma-pubias-node-q1", "pma-pubias-edge-q1-no",
        "pma-pubias-node-registry", "pma-pubias-edge-registry-yes",
        "pma-pubias-leaf-nodown-registry")))
    ))
  }

  # --- Q2: enough studies (k >= 10) to examine funnel plot asymmetry? -------
  # k < 10 is the usual reason a funnel-plot analysis is not possible. A
  # rare-event analysis is the other: Egger's regression of the effect on its
  # standard error breaks down on sparse binary data, where the two are
  # mathematically coupled through the cell counts. So `rare_flow` sends the
  # assessment down the same "no" edge k < 10 takes. This adds NO node; the
  # note says which of the two conditions sent it there.
  if (k >= 10 && !isTRUE(rare_flow)) {
    row <- .pubias_statistical(
      meta_obj                = meta_obj,
      k                       = k,
      pubias_funnel_asymmetry = pubias_funnel_asymmetry,
      q1_note                 = q1_note,
      rationale               = rationale
    )
  } else {
    row <- .pubias_registry(
      k                  = k,
      pubias_unpublished = pubias_unpublished,
      q1_note            = q1_note,
      rare_flow          = isTRUE(rare_flow) && k >= 10
    )
  }
  .pubias_add_fact(row, q0$fact)
}

# Insert one fact after the k fact of a row the Q2 branches built, so Q0's
# answer is recorded on every path without threading it through both.
.pubias_add_fact <- function(row, fact) {
  facts <- attr(row, "facts")
  if (is.null(facts) || is.null(fact)) return(row)
  at <- match("k", facts$key)
  attr(row, "facts") <- if (is.na(at)) {
    .facts(fact, facts)
  } else {
    dplyr::bind_rows(facts[seq_len(at), ], fact,
                     facts[-seq_len(at), ])
  }
  row
}

.pubias_effective_k <- function(meta_obj) {
  te <- meta_obj$TE
  se <- meta_obj$seTE
  if (!is.null(te) && !is.null(se) && length(te) == length(se) && length(te) > 0L) {
    return(sum(is.finite(te) & is.finite(se) & se > 0))
  }
  meta_obj$k %||% 0L
}

# --------------------------------------------------------------------------
# Q3: k >= 10 -- statistical / visual asymmetry branch (single-tier Egger)
#
# Guide Fig 9-1 asks whether asymmetry is "statistically significant"
# (Core GRADE 4 Fig 5: "strongly suggests"); neither names an alpha, so p < 0.05 is a
# pmatools operational convention, stated as such in the domain note. The
# figure has a single "Rate down" leaf here, so the judgment never exceeds
# one level.
# --------------------------------------------------------------------------
.PUBIAS_EGGER_CONVENTION <- paste0(
  "The p < 0.05 cut-off is a pmatools operational convention, not a Core ",
  "GRADE criterion: Core GRADE Guide Fig 9-1 asks whether funnel-plot ",
  "asymmetry is substantial on visual inspection and/or statistically ",
  "significant, and specifies no p-value threshold, nor any two-level ", "downgrade."
)
.pubias_statistical <- function(meta_obj, k, pubias_funnel_asymmetry, q1_note,
                                rationale = NULL) {

  # Manual override: visual inspection wins over Egger
  # (pubias_rationale already validated in assess_pubias)
  if (!is.null(pubias_funnel_asymmetry)) {
    if (!pubias_funnel_asymmetry %in% c("yes", "no")) {
      rlang::abort("pubias_funnel_asymmetry must be 'yes' or 'no'.")
    }
    if (pubias_funnel_asymmetry == "yes") {
      judgment <- "serious"
      flow_end <- c("pma-pubias-edge-q3-yes", "pma-pubias-leaf-down1-q3")
      asym_desc <- paste0(
        "Q3 (manual): visual inspection of contour-enhanced funnel plot ",
        "indicates asymmetry suggestive of publication bias -> rate down 1 (serious)."
      )
    } else {
      judgment <- "not_serious"
      flow_end <- c("pma-pubias-edge-q3-no", "pma-pubias-leaf-nodown-q3")
      asym_desc <- paste0(
        "Q3 (manual): visual inspection rules out funnel-plot asymmetry ",
        "-> do not rate down."
      )
    }
    return(make_domain_row(
      domain    = "Publication bias",
      judgment  = judgment,
      auto      = FALSE,
      notes     = paste0(
        q1_note,
        sprintf("Q2: Statistical analysis feasible (k = %d >= 10). ", k),
        asym_desc, " [manual]"
      ),
      rationale = rationale,
      # The manual override answers the same Q3 node the test does, so it
      # highlights the same route; only the note says which decided it.
      facts     = .facts(.pubias_k_fact(k), .flow_path_fact(c(
        .PUBIAS_FLOW_TO_Q2, "pma-pubias-edge-q2-yes",
        "pma-pubias-node-q3", flow_end)))
    ))
  }

  # Auto: Egger linear regression test, 2-tier rule
  egger <- tryCatch(
    suppressWarnings(meta::metabias(meta_obj, method.bias = "linreg")),
    error = function(e) NULL
  )
  pval <- if (!is.null(egger) && !is.null(egger$p.value) && !is.na(egger$p.value)) {
    egger$p.value
  } else {
    NA_real_
  }

  if (is.na(pval)) {
    qual_note <- paste0(
      .PUBIAS_QUALITATIVE_MARKER, ": Egger's test could not be computed ",
      sprintf("(k = %d)", k),
      ", so publication bias was NOT formally assessed. ",
      .pubias_qualitative_guidance()
    )
    rlang::warn(paste0(
      "Publication bias could not be formally assessed: Egger's test failed ",
      "to run (k = ", k, "). A qualitative assessment is required ",
      "(funnel plot asymmetry, search comprehensiveness, registry ",
      "completeness). Assuming 'no' (no downgrade); specify ",
      "pubias_funnel_asymmetry = 'yes'/'no' to record your judgment."
    ))
    return(make_domain_row(
      domain   = "Publication bias",
      judgment = "not_serious",
      auto     = TRUE,
      notes    = paste0(
        qual_note, " ",
        q1_note,
        sprintf("Q2: Statistical analysis feasible (k = %d >= 10) but ", k),
        "Egger's test failed to run. [auto (Egger's test)]"
      ),
      # The judgment is "no", so the chart shows the "no" leaf. The reason it
      # is "no" -- an assumption rather than a test result -- is what
      # qual_note and the warning above are for.
      facts    = .facts(.pubias_k_fact(k), .flow_path_fact(c(
        .PUBIAS_FLOW_TO_Q2, "pma-pubias-edge-q2-yes",
        "pma-pubias-node-q3", "pma-pubias-edge-q3-no",
        "pma-pubias-leaf-nodown-q3")))
    ))
  } else if (pval < 0.05) {
    egger_note <- sprintf("Egger's test: p = %.4f.", pval)
    judgment   <- "serious"
    flow_end   <- c("pma-pubias-edge-q3-yes", "pma-pubias-leaf-down1-q3")
    asym_desc  <- paste0(
      "Q3 (auto): Egger's test p < 0.05 -> evidence of funnel-plot asymmetry ",
      "-> rate down 1 (serious)."
    )
  } else {
    egger_note <- sprintf("Egger's test: p = %.3f.", pval)
    judgment   <- "not_serious"
    flow_end   <- c("pma-pubias-edge-q3-no", "pma-pubias-leaf-nodown-q3")
    asym_desc  <- paste0(
      "Q3 (auto): Egger's test p >= 0.05 -> no strong evidence of ",
      "funnel-plot asymmetry -> do not rate down."
    )
  }

  make_domain_row(
    domain   = "Publication bias",
    judgment = judgment,
    auto     = TRUE,
    notes    = paste0(
      q1_note,
      sprintf("Q2: Statistical analysis feasible (k = %d >= 10). ", k),
      asym_desc, " ", egger_note, " ", .PUBIAS_EGGER_CONVENTION,
      " [auto (Egger's test)]"
    ),
    # The p value is the number the Q3 verdict turns on, so it belongs in the
    # facts and not only in the sentence. Recorded only where it exists: the
    # manual and failed-test paths above have none.
    facts    = .facts(
      .pubias_k_fact(k),
      .fact("egger_p", "Egger's test p value",
            sprintf("%.4f (pmatools cut-off: 0.05)", pval), pval),
      .flow_path_fact(c(.PUBIAS_FLOW_TO_Q2, "pma-pubias-edge-q2-yes",
                        "pma-pubias-node-q3", flow_end))
    )
  )
}

# --------------------------------------------------------------------------
# Q4: k < 10 -- registry / unpublished studies branch
# --------------------------------------------------------------------------
#
# `rare_flow` is TRUE only when this branch was reached DESPITE k >= 10, i.e.
# when sparse data rather than the study count answered Q2. It changes the
# sentence and nothing else: the same node, the same two answers, the same
# judgments.
.pubias_registry <- function(k, pubias_unpublished, q1_note,
                             rare_flow = FALSE) {

  q2_note <- if (isTRUE(rare_flow)) {
    sprintf(paste0("Q2: Statistical analysis not feasible despite k = %d >= ",
                   "10 -- this is a rare-event analysis, and Egger's ",
                   "regression loses validity on sparse binary data. The ",
                   "flowchart's 'no' edge is taken for the reason it already has ",
                   "it. "), k)
  } else {
    sprintf("Q2: Statistical analysis not feasible (k = %d < 10). ", k)
  }

  if (is.null(pubias_unpublished)) {
    rlang::warn(paste0(
      "pubias_unpublished not specified and ",
      if (isTRUE(rare_flow)) {
        paste0("Egger's test is not valid on this rare-event analysis (k = ",
               k, "). ")
      } else {
        paste0("k < 10 (k = ", k, "). ")
      },
      "Statistical analysis is not feasible. ",
      "Provide pubias_unpublished = 'yes' or 'no' based on registry search ",
      "(eg, ClinicalTrials.gov, FDA) for unpublished trials. ",
      "Assuming 'no' (no documented unpublished studies)."
    ))
    unpublished <- "no"
    src_note    <- paste0(
      " ", .PUBIAS_QUALITATIVE_MARKER,
      ": no statistical test possible (",
      if (isTRUE(rare_flow)) "rare-event analysis" else "k < 10",
      ") and no manual input given, ",
      "so publication bias was NOT formally assessed. ",
      .pubias_qualitative_guidance(),
      " [assumed 'no'; specify pubias_unpublished to override]"
    )
    auto_flag   <- TRUE
  } else {
    if (!pubias_unpublished %in% c("yes", "no")) {
      rlang::abort("pubias_unpublished must be 'yes' or 'no'.")
    }
    unpublished <- pubias_unpublished
    src_note    <- " [manual]"
    auto_flag   <- FALSE
  }

  if (unpublished == "yes") {
    judgment   <- "serious"
    flow_end   <- c("pma-pubias-edge-q4-yes", "pma-pubias-leaf-down1-q4")
    unpub_desc <- paste0(
      "Q4: Documentation of unpublished studies identified (registry/FDA) ",
      "-> rate down 1 (serious)."
    )
  } else {
    judgment   <- "not_serious"
    # An assumed "no" and an answered "no" reach the same leaf, because they
    # are the same judgment. `auto_flag` still separates them everywhere it
    # matters: src_note carries the qualitative-assessment marker, the caller
    # was warned, and the row records auto = TRUE.
    flow_end   <- c("pma-pubias-edge-q4-no", "pma-pubias-leaf-nodown-q4")
    unpub_desc <- "Q4: No documentation of unpublished studies -> do not rate down."
  }

  make_domain_row(
    domain   = "Publication bias",
    judgment = judgment,
    auto     = auto_flag,
    notes    = paste0(q1_note, q2_note, unpub_desc, src_note),
    facts    = .facts(
      .pubias_k_fact(k),
      if (isTRUE(rare_flow)) {
        .fact("rare_flow", "Rare-event analysis",
              paste0("yes - the k < 10 route was taken at k = ", k,
                     " because Egger's test loses validity on sparse binary ",
                     "data"))
      } else NULL,
      .flow_path_fact(c(
        .PUBIAS_FLOW_TO_Q2, "pma-pubias-edge-q2-no",
        "pma-pubias-node-q4", flow_end)))
  )
}
