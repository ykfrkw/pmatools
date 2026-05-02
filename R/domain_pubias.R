# domain_pubias.R — 出版バイアスドメイン評価
#
# BMJ 2025 Core GRADE 4, Fig 5 フローチャートに準拠:
#
#   Step 1. Are most or all studies small and industry sponsored?
#     YES -> Rate down (judgment = "serious")
#     NO  -> Step 2
#
#   Step 2. Is statistical analysis of publication bias feasible?
#           (ie, meta-analysis performed, >=10 studies)
#
#     YES (k >= 10) -> Visual asymmetry of funnel plot AND/OR statistical test
#                    strongly suggests publication bias?
#       YES -> Rate down (judgment = "serious")
#       NO  -> Do not rate down
#
#     NO (k < 10)  -> Documentation of unpublished studies
#                    (eg, in registry or FDA)?
#       YES -> Rate down (judgment = "serious")
#       NO  -> Do not rate down
#
# 引数:
#   pubias_small_industry   : "yes" / "no" / NULL (デフォルト = "no"として扱う)
#   pubias_funnel_asymmetry : "yes" / "no" / NULL
#       NULL かつ k >= 10 -> Egger 検定を自動実施し、p < 0.10 で asymmetry 判定
#   pubias_unpublished      : "yes" / "no" / NULL
#       NULL かつ k < 10  -> "no" と仮定（注記付き）

assess_pubias <- function(meta_obj,
                          pubias_small_industry   = NULL,
                          pubias_funnel_asymmetry = NULL,
                          pubias_unpublished      = NULL) {
  k <- meta_obj$k
  if (is.null(k) || is.na(k)) k <- 0L

  # --- Step 1: Small + industry-sponsored --------------------------------
  if (!is.null(pubias_small_industry)) {
    if (!pubias_small_industry %in% c("yes", "no")) {
      rlang::abort("pubias_small_industry must be 'yes' or 'no'.")
    }
    if (pubias_small_industry == "yes") {
      return(make_domain_row(
        domain   = "Publication bias",
        judgment = "serious",
        auto     = FALSE,
        notes    = paste0(
          "FLOWCHART Step 1: Most/all studies are small AND industry-sponsored ",
          "-> rate down."
        )
      ))
    }
    # "no" -> proceed to Step 2
    step1_note <- "FLOWCHART Step 1: Not dominated by small industry-sponsored studies. "
  } else {
    step1_note <- "FLOWCHART Step 1: pubias_small_industry not specified; assumed 'no'. "
  }

  # --- Step 2: Statistical feasibility (k >= 10) --------------------------
  if (k >= 10) {
    # Statistical analysis feasible
    return(.pubias_statistical(
      meta_obj                = meta_obj,
      k                       = k,
      pubias_funnel_asymmetry = pubias_funnel_asymmetry,
      step1_note              = step1_note
    ))
  } else {
    # Statistical analysis NOT feasible -> check registry / unpublished
    return(.pubias_registry(
      k                  = k,
      pubias_unpublished = pubias_unpublished,
      step1_note         = step1_note
    ))
  }
}

# --------------------------------------------------------------------------
# k >= 10: funnel plot / statistical test branch
# --------------------------------------------------------------------------
.pubias_statistical <- function(meta_obj, k, pubias_funnel_asymmetry, step1_note) {

  # Auto-compute Egger's test if pubias_funnel_asymmetry is NULL
  egger_note <- NA_character_
  if (is.null(pubias_funnel_asymmetry)) {
    egger <- tryCatch(
      meta::metabias(meta_obj, method = "linreg"),
      error   = function(e) NULL,
      warning = function(w) suppressWarnings(meta::metabias(meta_obj, method = "linreg"))
    )
    if (!is.null(egger) && !is.null(egger$p.value) && !is.na(egger$p.value)) {
      pval <- egger$p.value
      egger_note <- sprintf("Egger's test: p = %.3f (k = %d).", pval, k)
      pubias_funnel_asymmetry_auto <- if (pval < 0.10) "yes" else "no"
    } else {
      egger_note <- sprintf("Egger's test could not be computed (k = %d).", k)
      pubias_funnel_asymmetry_auto <- "no"
    }
    judgment_src <- "auto (Egger's test)"
    asymmetry <- pubias_funnel_asymmetry_auto
  } else {
    if (!pubias_funnel_asymmetry %in% c("yes", "no")) {
      rlang::abort("pubias_funnel_asymmetry must be 'yes' or 'no'.")
    }
    judgment_src <- "manual"
    asymmetry <- pubias_funnel_asymmetry
    egger_note <- if (!is.null(egger_note)) egger_note else ""
  }

  auto_flag <- (judgment_src == "auto (Egger's test)")

  if (asymmetry == "yes") {
    judgment <- "serious"
    asym_desc <- "funnel plot asymmetry / statistical test strongly suggests bias -> rate down"
  } else {
    judgment <- "no"
    asym_desc <- "no strong evidence of funnel plot asymmetry -> do not rate down"
  }

  notes <- paste0(
    step1_note,
    sprintf("Step 2: Statistical analysis feasible (k = %d >= 10). ", k),
    asym_desc, ". ",
    if (!is.na(egger_note)) egger_note else "",
    " [", judgment_src, "]"
  )

  make_domain_row(
    domain   = "Publication bias",
    judgment = judgment,
    auto     = auto_flag,
    notes    = trimws(notes)
  )
}

# --------------------------------------------------------------------------
# k < 10: registry / unpublished studies branch
# --------------------------------------------------------------------------
.pubias_registry <- function(k, pubias_unpublished, step1_note) {

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
    judgment  <- "serious"
    unpub_desc <- paste0(
      "Documentation of unpublished studies identified (registry/FDA) ",
      "-> rate down."
    )
  } else {
    judgment  <- "no"
    unpub_desc <- "No documentation of unpublished studies -> do not rate down."
  }

  notes <- paste0(
    step1_note,
    sprintf(
      "Step 2: Statistical analysis not feasible (k = %d < 10). ", k
    ),
    unpub_desc, src_note
  )

  make_domain_row(
    domain   = "Publication bias",
    judgment = judgment,
    auto     = auto_flag,
    notes    = trimws(notes)
  )
}
