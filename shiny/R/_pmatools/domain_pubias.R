# domain_pubias.R - Publication bias domain assessment
#
# BMJ 2025 Core GRADE 4 Figure 5 flowchart, faithfully:
#
#   [Top-level] pubias_registry_complete == "yes"
#                 -> "no" (structural rule-out by complete pre-registration coverage)
#
#   Q1. pubias_small_industry == "yes"
#         -> "some_concerns" (-1)
#         else -> Q2 (default "no" assumption with note)
#
#   Q2. k >= 10 ?
#         YES -> Q3 (.pubias_statistical)
#         NO  -> Q4 (.pubias_registry)
#
#   Q3 (k >= 10): asymmetry detection
#         pubias_funnel_asymmetry "yes" (manual) -> "some_concerns" (-1)
#         pubias_funnel_asymmetry "no"  (manual) -> "no"
#         auto Egger p < 0.01                    -> "serious"        (-2)
#         auto 0.01 <= Egger p < 0.05            -> "some_concerns"  (-1)
#         auto Egger p >= 0.05                   -> "no"
#         Egger fails to run                     -> "no" (with note)
#
#   Q4 (k < 10):
#         pubias_unpublished == "yes" -> "some_concerns" (-1)
#         pubias_unpublished == "no"  -> "no"
#         NULL -> assume "no" with warning
#
# Trim-and-fill is no longer used to drive the GRADE judgment (the 2-tier Egger
# rule subsumes the previous sign-flip escalation). The {meta} trim-and-fill
# computation remains available via plot_trimfill_forest() for the Reporting
# bias tab in the companion Shiny app.

assess_pubias <- function(meta_obj,
                          pubias_small_industry    = NULL,
                          pubias_funnel_asymmetry  = NULL,
                          pubias_unpublished       = NULL,
                          pubias_registry_complete = NULL) {
  k <- .pubias_effective_k(meta_obj)
  if (is.null(k) || is.na(k)) k <- 0L

  # --- Top-level: complete pre-registration coverage rules out pub bias -----
  if (!is.null(pubias_registry_complete)) {
    if (!pubias_registry_complete %in% c("yes", "no")) {
      rlang::abort("pubias_registry_complete must be 'yes' or 'no'.")
    }
    if (pubias_registry_complete == "yes") {
      return(make_domain_row(
        domain   = "Publication bias",
        judgment = "no",
        auto     = FALSE,
        notes    = paste0(
          "STRUCTURAL RULE-OUT: pubias_registry_complete = 'yes'. ",
          "Comprehensive pre-registration coverage assumed; ",
          "publication bias is structurally ruled out -> do not rate down."
        )
      ))
    }
  }

  # --- Q1: Small + industry-sponsored ---------------------------------------
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
          "-> rate down 1 (some_concerns)."
        )
      ))
    }
    q1_note <- "Q1: Not dominated by small industry-sponsored studies. "
  } else {
    q1_note <- "Q1: pubias_small_industry not specified; assumed 'no'. "
  }

  # --- Q2: Statistical feasibility (k >= 10) --------------------------------
  if (k >= 10) {
    return(.pubias_statistical(
      meta_obj                = meta_obj,
      k                       = k,
      pubias_funnel_asymmetry = pubias_funnel_asymmetry,
      q1_note                 = q1_note
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
# Q3: k >= 10 -- statistical / visual asymmetry branch (2-tier Egger)
# --------------------------------------------------------------------------
.pubias_statistical <- function(meta_obj, k, pubias_funnel_asymmetry, q1_note) {

  # Manual override: visual inspection wins over Egger
  if (!is.null(pubias_funnel_asymmetry)) {
    if (!pubias_funnel_asymmetry %in% c("yes", "no")) {
      rlang::abort("pubias_funnel_asymmetry must be 'yes' or 'no'.")
    }
    if (pubias_funnel_asymmetry == "yes") {
      judgment <- "some_concerns"
      asym_desc <- paste0(
        "Q3 (manual): visual inspection of contour-enhanced funnel plot ",
        "indicates asymmetry suggestive of publication bias -> rate down 1 (some_concerns)."
      )
    } else {
      judgment <- "no"
      asym_desc <- paste0(
        "Q3 (manual): visual inspection rules out funnel-plot asymmetry ",
        "-> do not rate down."
      )
    }
    return(make_domain_row(
      domain   = "Publication bias",
      judgment = judgment,
      auto     = FALSE,
      notes    = paste0(
        q1_note,
        sprintf("Q2: Statistical analysis feasible (k = %d >= 10). ", k),
        asym_desc, " [manual]"
      )
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
    egger_note <- sprintf("Egger's test could not be computed (k = %d).", k)
    judgment   <- "no"
    asym_desc  <- "Egger's test failed to run; defaulted to 'no' (no evidence of asymmetry available)."
  } else if (pval < 0.01) {
    egger_note <- sprintf("Egger's test: p = %.4f.", pval)
    judgment   <- "serious"
    asym_desc  <- "Q3 (auto): Egger's test p < 0.01 -> strong evidence of funnel-plot asymmetry -> rate down 2 (serious)."
  } else if (pval < 0.05) {
    egger_note <- sprintf("Egger's test: p = %.3f.", pval)
    judgment   <- "some_concerns"
    asym_desc  <- "Q3 (auto): Egger's test 0.01 <= p < 0.05 -> evidence of funnel-plot asymmetry -> rate down 1 (some_concerns)."
  } else {
    egger_note <- sprintf("Egger's test: p = %.3f.", pval)
    judgment   <- "no"
    asym_desc  <- "Q3 (auto): Egger's test p >= 0.05 -> no strong evidence of funnel-plot asymmetry -> do not rate down."
  }

  make_domain_row(
    domain   = "Publication bias",
    judgment = judgment,
    auto     = TRUE,
    notes    = paste0(
      q1_note,
      sprintf("Q2: Statistical analysis feasible (k = %d >= 10). ", k),
      asym_desc, " ", egger_note,
      " [auto (Egger's test, 2-tier)]"
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
    src_note    <- " [assumed 'no'; specify pubias_unpublished to override]"
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
    unpub_desc <- paste0(
      "Q4: Documentation of unpublished studies identified (registry/FDA) ",
      "-> rate down 1 (some_concerns)."
    )
  } else {
    judgment   <- "no"
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
    )
  )
}
