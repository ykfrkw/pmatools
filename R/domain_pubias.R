# domain_pubias.R - Publication bias domain assessment
#
# BMJ 2025 Core GRADE 4 Figure 5 flowchart, faithfully.
#
# Fig 5 has exactly four decision nodes and no entry-level rule-out:
#   1. "Most or all studies small and industry sponsored"
#   2. "Is statistical analysis of publication bias feasible? (ie, meta-analysis
#       was performed, >=10 studies)"
#   3. "Visual asymmetry of funnel plot and/or statistical test strongly
#       suggests publication bias"
#   4. "Documentation of unpublished studies (eg, in registry of FDA)"
# Every leaf is "Rate down" / "Do not rate down"; the figure never rates down
# two levels for publication bias.
#
# pmatools implementation:
#
#   Q1. pubias_small_industry == "yes"
#         -> "some_concerns" (-1)
#         else -> registry check, then Q2 (default "no" assumption with note)
#
#   [After Q1] pubias_registry_complete == "yes"
#         -> "no" (a pmatools convenience input, NOT a node of Fig 5; see the
#            note text). It is evaluated AFTER Q1 (v0.5) so that a body of
#            small industry-sponsored trials still rates down even when the
#            user asserts complete registry coverage.
#
#   Q2. k >= 10 ?
#         YES -> Q3 (.pubias_statistical)
#         NO  -> Q4 (.pubias_registry)
#
#   Q3 (k >= 10): asymmetry detection
#         pubias_funnel_asymmetry "yes" (manual) -> "some_concerns" (-1)
#         pubias_funnel_asymmetry "no"  (manual) -> "no"
#         auto Egger p < 0.05                    -> "some_concerns"  (-1)
#         auto Egger p >= 0.05                   -> "no"
#         Egger fails to run                     -> "no" + prominent
#           "QUALITATIVE ASSESSMENT REQUIRED" note (propagated to SoF /
#           Evidence Profile / grade_table / grade_report footnotes)
#
#       The p-value cut-off is NOT in the source. Core GRADE 4 Fig 5 asks only
#       whether asymmetry "strongly suggests publication bias" -- a qualitative
#       judgment with no significance threshold. p < 0.05 is a pmatools
#       operational convention and is labelled as such in the domain notes.
#       The former p < 0.01 -> "serious" (-2) tier was removed in v0.5: Core
#       GRADE 4 never describes a two-level publication-bias downgrade.
#
#   Q4 (k < 10):
#         pubias_unpublished == "yes" -> "some_concerns" (-1)
#         pubias_unpublished == "no"  -> "no"
#         NULL -> assume "no" with warning
#
# Rate down at most ONE level: "serious" (-2) is reachable only through a
# scalar override elsewhere in the package, never from this flowchart.
#
# Trim-and-fill is not used to drive the GRADE judgment. The {meta}
# trim-and-fill computation remains available via plot_trimfill_forest() for
# the Reporting bias tab in the companion Shiny app.

# --------------------------------------------------------------------------
# Flowchart node vocabulary (inst/figures/pubias.svg)
#
# See the note on .ROB_FIG2_NODE_IDS in domain_rob.R. The registry node is
# drawn dashed and named "pmatools input" because it is NOT a node of Core
# GRADE 4 Fig 5; the figure's only registry node is Q4, reached when k < 10.
#
# The ids keep their q1..q4 slugs, but the FIGURE no longer prints those
# numbers: it interleaves the registry node between Q1 and Q2, so numbering on
# screen described neither Fig 5 nor the route. The "Q1:" - "Q4:" prefixes in
# the notes below are unaffected -- they are the exported record.
#
# There is no "qualitative assessment required" leaf. Both branches that used
# to have one (an Egger test that would not run, and a Q4 nobody answered)
# judge the domain "no", so they light the "do not rate down" leaf they
# actually reach. What made them different from an answered "no" is a caveat,
# not a decision, and it travels in the note and in an rlang::warn() where a
# reader can act on it -- a leaf in a picture cannot be read by a pipeline and
# was not read by anything else either.
.PUBIAS_FIG5_NODE_IDS <- c(
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

# Everything past Q1 has come through the registry node, whether or not the
# reviewer answered it.
.PUBIAS_FLOW_TO_Q2 <- c("pma-pubias-node-q1", "pma-pubias-edge-q1-no",
                        "pma-pubias-node-registry",
                        "pma-pubias-edge-registry-no",
                        "pma-pubias-node-q2")

# The study count the flowchart's Q2 actually branches on. Recorded on every
# path: k is the one number the reviewer needs to see to check that Q2 was
# answered the way they expect, and until v0.5.2 this domain recorded nothing
# at all, so it was only available by re-reading the note.
.pubias_k_fact <- function(k) {
  .fact("k", "Studies contributing a usable estimate",
        sprintf("%d (Q2 threshold: 10)", as.integer(k)), as.numeric(k))
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

assess_pubias <- function(meta_obj,
                          pubias_small_industry    = NULL,
                          pubias_funnel_asymmetry  = NULL,
                          pubias_unpublished       = NULL,
                          pubias_registry_complete = NULL,
                          rationale                = NULL) {
  k <- .pubias_effective_k(meta_obj)
  if (is.null(k) || is.na(k)) k <- 0L

  # v0.4.0 (breaking): pubias_funnel_asymmetry is the explicit override path
  # (a manual visual judgment replaces the automated Egger's test), so it
  # always requires pubias_rationale. The other inputs
  # (pubias_small_industry / pubias_unpublished / pubias_registry_complete)
  # supply information the automated path cannot know and are not overrides.
  if (!is.null(pubias_funnel_asymmetry)) {
    .check_override_rationale(rationale, "pubias_rationale",
                              "Publication bias")
  }

  # Validate up front; the registry input is consumed after Q1 (see below).
  if (!is.null(pubias_registry_complete) &&
      !pubias_registry_complete %in% c("yes", "no")) {
    rlang::abort("pubias_registry_complete must be 'yes' or 'no'.")
  }

  # --- Q1: Small + industry-sponsored (Core GRADE 4 Fig 5, first node) ------
  if (!is.null(pubias_small_industry)) {
    if (!pubias_small_industry %in% c("yes", "no")) {
      rlang::abort("pubias_small_industry must be 'yes' or 'no'.")
    }
    if (pubias_small_industry == "yes") {
      return(make_domain_row(
        domain   = "Publication bias",
        judgment = "some_concerns",
        auto     = FALSE,
        notes    = paste0(
          "Q1: Most/all studies are small AND industry-sponsored ",
          "-> rate down 1 (some_concerns).",
          if (identical(pubias_registry_complete, "yes")) paste0(
            " pubias_registry_complete = 'yes' was also supplied, but it is ",
            "evaluated only after Q1: Core GRADE 4 Fig 5 begins with the ",
            "small-and-industry-sponsored node, and asserting registry ",
            "coverage does not remove that concern."
          ) else ""
        ),
        facts    = .facts(.pubias_k_fact(k), .flow_path_fact(c(
          "pma-pubias-node-q1", "pma-pubias-edge-q1-yes",
          "pma-pubias-leaf-down1-q1")))
      ))
    }
    q1_note <- "Q1: Not dominated by small industry-sponsored studies. "
  } else {
    q1_note <- "Q1: pubias_small_industry not specified; assumed 'no'. "
  }

  # --- Post-Q1: user-asserted complete pre-registration coverage ------------
  # Not a node of Core GRADE 4 Fig 5. The figure's only registry node is Q4
  # ("Documentation of unpublished studies"), reached when k < 10. This input
  # records the USER'S claim that every registered trial can be accounted for
  # and short-circuits the remaining nodes; the note says whose claim it is.
  if (identical(pubias_registry_complete, "yes")) {
    return(make_domain_row(
      domain   = "Publication bias",
      judgment = "no",
      auto     = FALSE,
      notes    = paste0(
        q1_note,
        "pubias_registry_complete = 'yes': the user asserts that ",
        "pre-registration is universal in this field and that all registered ",
        "trials are accounted for, so the remaining Fig 5 nodes are not ",
        "evaluated -> do not rate down. This rule-out is a pmatools input, ",
        "not a decision node of Core GRADE 4 Fig 5, and rests entirely on ",
        "that assertion."
      ),
      facts     = .facts(.pubias_k_fact(k), .flow_path_fact(c(
        "pma-pubias-node-q1", "pma-pubias-edge-q1-no",
        "pma-pubias-node-registry", "pma-pubias-edge-registry-yes",
        "pma-pubias-leaf-nodown-registry")))
    ))
  }

  # --- Q2: Statistical feasibility (k >= 10) --------------------------------
  if (k >= 10) {
    return(.pubias_statistical(
      meta_obj                = meta_obj,
      k                       = k,
      pubias_funnel_asymmetry = pubias_funnel_asymmetry,
      q1_note                 = q1_note,
      rationale               = rationale
    ))
  } else {
    return(.pubias_registry(
      k                  = k,
      pubias_unpublished = pubias_unpublished,
      q1_note            = q1_note
    ))
  }
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
# Core GRADE 4 Fig 5 phrases this node qualitatively ("strongly suggests
# publication bias") and gives no p-value cut-off; p < 0.05 below is a
# pmatools operational convention, stated as such in the domain note. The
# figure has a single "Rate down" leaf here, so the judgment never exceeds
# one level.
# --------------------------------------------------------------------------
.PUBIAS_EGGER_CONVENTION <- paste0(
  "The p < 0.05 cut-off is a pmatools operational convention, not a Core ",
  "GRADE criterion: Core GRADE 4 Fig 5 asks qualitatively whether funnel-plot ",
  "asymmetry and/or a statistical test 'strongly suggests publication bias' ",
  "and specifies no p-value threshold, nor any two-level downgrade."
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
      judgment <- "some_concerns"
      flow_end <- c("pma-pubias-edge-q3-yes", "pma-pubias-leaf-down1-q3")
      asym_desc <- paste0(
        "Q3 (manual): visual inspection of contour-enhanced funnel plot ",
        "indicates asymmetry suggestive of publication bias -> rate down 1 (some_concerns)."
      )
    } else {
      judgment <- "no"
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
      judgment = "no",
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
    judgment   <- "some_concerns"
    flow_end   <- c("pma-pubias-edge-q3-yes", "pma-pubias-leaf-down1-q3")
    asym_desc  <- paste0(
      "Q3 (auto): Egger's test p < 0.05 -> evidence of funnel-plot asymmetry ",
      "-> rate down 1 (some_concerns)."
    )
  } else {
    egger_note <- sprintf("Egger's test: p = %.3f.", pval)
    judgment   <- "no"
    flow_end   <- c("pma-pubias-edge-q3-no", "pma-pubias-leaf-nodown-q3")
    asym_desc  <- "Q3 (auto): Egger's test p >= 0.05 -> no strong evidence of funnel-plot asymmetry -> do not rate down."
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
.pubias_registry <- function(k, pubias_unpublished, q1_note) {

  if (is.null(pubias_unpublished)) {
    rlang::warn(paste0(
      "pubias_unpublished not specified and k < 10 (k = ", k, "). ",
      "Statistical analysis is not feasible. ",
      "Provide pubias_unpublished = 'yes' or 'no' based on registry search ",
      "(eg, ClinicalTrials.gov, FDA) for unpublished trials. ",
      "Assuming 'no' (no documented unpublished studies)."
    ))
    unpublished <- "no"
    src_note    <- paste0(
      " ", .PUBIAS_QUALITATIVE_MARKER,
      ": no statistical test possible (k < 10) and no manual input given, ",
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
    judgment   <- "some_concerns"
    flow_end   <- c("pma-pubias-edge-q4-yes", "pma-pubias-leaf-down1-q4")
    unpub_desc <- paste0(
      "Q4: Documentation of unpublished studies identified (registry/FDA) ",
      "-> rate down 1 (some_concerns)."
    )
  } else {
    judgment   <- "no"
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
    notes    = paste0(
      q1_note,
      sprintf("Q2: Statistical analysis not feasible (k = %d < 10). ", k),
      unpub_desc, src_note
    ),
    facts    = .facts(.pubias_k_fact(k), .flow_path_fact(c(
      .PUBIAS_FLOW_TO_Q2, "pma-pubias-edge-q2-no",
      "pma-pubias-node-q4", flow_end)))
  )
}
