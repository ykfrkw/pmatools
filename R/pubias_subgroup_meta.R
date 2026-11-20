# pubias_subgroup_meta.R - the meta object the available-vs-missing forest draws
#
# plot_forest_pubias_subgroup() stacks "Available results" over "Missing
# results" by refitting the analysis as a two-subgroup metagen. This file owns
# that refit. Its one promise is that the Available block shows the analysis'
# own pooled estimate, confidence interval and prediction interval: before
# 0.10.0 the refit ran on {meta}'s defaults, so a Hartung-Knapp or REML analysis
# was redrawn with a different interval from the one rated beside it.
#
# Reads: the analysis' `meta_obj`, the caller's `missing_df` and the
# `auto_detect` switch. Produces: the row partition (available rows are those
# with finite TE and seTE; missing rows are the auto-detected NA-TE rows plus
# `missing_df`, in one canonical table), and from it a metagen object with both subgroups, carrying `n_total` (the "N"
# column) and `results_known` (the status column, "" on available rows). It
# checks the refit's Available pool against `meta_obj`, retries once with tau
# preset to the analysis' tau, and warns if the two still disagree (a GLMM
# analysis, which metagen cannot reproduce, is the known case).
#
# Used only by R/plot_forest_pubias_subgroup.R, which owns the drawing.
#
# A helper belongs here when it decides which rows the subgroup forest holds or
# what numbers its meta object carries; how they are laid out on the page, and
# the argument checks, belong to the plotting file.

PUBIAS_SUBGROUP_AVAILABLE <- "Available results"
PUBIAS_SUBGROUP_MISSING   <- "Missing results"

# Settings of the analysis that change its pooled random-effects estimate, CI
# or prediction interval, or how they are back-transformed. Only those that
# the installed metagen() takes and the object actually holds are passed, so
# an older or newer {meta} drops what it does not know. `text.random` is left
# out on purpose: metagen() then labels the pooled row from the method, as in
# "Random effects model (HK)", where a copied label can lose the "(HK)".
.PUBIAS_MODEL_FIELDS <- c(
  "method.tau", "method.tau.ci", "method.random.ci", "adhoc.hakn.ci",
  "method.predict", "adhoc.hakn.pi", "seed.predict",
  "level", "level.ma", "level.predict", "level.hetstat",
  "method.I2", "method.common.ci", "backtransf", "null.effect"
)

# The pooled numbers that must survive the refit, in the order compared.
.PUBIAS_POOLED_FIELDS <- c("TE.random", "lower.random", "upper.random",
                           "lower.predict", "upper.predict")

# The analysis' model settings as metagen() arguments.
.pubias_model_settings <- function(meta_obj) {
  metagen_args <- names(formals(meta::metagen))
  field_names  <- intersect(intersect(.PUBIAS_MODEL_FIELDS, metagen_args),
                            names(meta_obj))
  settings <- unclass(meta_obj)[field_names]
  settings <- settings[!vapply(settings, is.null, logical(1))]
  # {meta} before 6.0 recorded Hartung-Knapp only as `hakn`.
  is_legacy_hakn <- is.null(settings$method.random.ci) &&
    isTRUE(meta_obj$hakn) && "hakn" %in% metagen_args
  if (is_legacy_hakn) settings$hakn <- TRUE
  has_subgroup_seed <- !is.null(settings$seed.predict) &&
    "seed.predict.subgroup" %in% metagen_args
  if (has_subgroup_seed) settings$seed.predict.subgroup <- settings$seed.predict
  settings
}

# Does the refit's Available subgroup pool to exactly what `meta_obj` pooled?
.pubias_pool_matches <- function(m_combined, meta_obj) {
  pooled_refit <- vapply(.PUBIAS_POOLED_FIELDS, function(field) {
    by_subgroup <- m_combined[[paste0(field, ".w")]]
    if (is.null(by_subgroup)) return(NA_real_)
    available_value <- unname(by_subgroup[PUBIAS_SUBGROUP_AVAILABLE])
    as.numeric(available_value)[1]
  }, numeric(1))
  pooled_analysis <- vapply(.PUBIAS_POOLED_FIELDS, function(field) {
    as.numeric(meta_obj[[field]] %||% NA_real_)[1]
  }, numeric(1))
  isTRUE(all.equal(unname(pooled_refit), unname(pooled_analysis)))
}

# The canonical (empty) missing-rows table.
.pubias_empty_missing <- function() {
  data.frame(studlab = character(0), n = integer(0),
             results_known = character(0), stringsAsFactors = FALSE)
}

# Which of meta_obj's rows are available, plus the full missing-rows table:
# auto-detected NA-TE rows (when `auto_detect`) followed by `missing_df`.
.pubias_subgroup_rows <- function(meta_obj, missing_df, auto_detect) {
  # Align vectors to length(meta_obj$TE) (some metabin variants keep extra
  # rows in $studlab/$n.e/$n.c that are not in $TE).
  k_te <- length(meta_obj$TE)
  studlab_obj <- as.character(meta_obj$studlab)
  if (length(studlab_obj) > k_te) studlab_obj <- studlab_obj[seq_len(k_te)]
  if (length(studlab_obj) < k_te) {
    studlab_obj <- c(studlab_obj,
                     sprintf("(unnamed %d)", seq_len(k_te - length(studlab_obj))))
  }
  has_arm_sizes <- !is.null(meta_obj$n.e) && !is.null(meta_obj$n.c) &&
    length(meta_obj$n.e) >= k_te && length(meta_obj$n.c) >= k_te
  n_obj <- if (has_arm_sizes) {
    meta_obj$n.e[seq_len(k_te)] + meta_obj$n.c[seq_len(k_te)]
  } else {
    rep(NA_integer_, k_te)
  }

  te_finite     <- is.finite(meta_obj$TE) & is.finite(meta_obj$seTE)
  auto_miss_idx <- which(!te_finite)

  # Skipped when the caller manages the missing list itself (e.g. an
  # interactive editor that has already imported the auto rows).
  auto_miss_df <- if (isTRUE(auto_detect) && length(auto_miss_idx) > 0L) {
    data.frame(
      studlab = studlab_obj[auto_miss_idx],
      n = suppressWarnings(as.integer(n_obj[auto_miss_idx])),
      results_known = "Reported but data not extractable",
      stringsAsFactors = FALSE
    )
  } else .pubias_empty_missing()

  user_miss_df <- if (nrow(missing_df) > 0L) {
    data.frame(
      studlab = as.character(missing_df$studlab),
      n = suppressWarnings(as.integer(missing_df$n)),
      results_known = as.character(missing_df$results_known),
      stringsAsFactors = FALSE
    )
  } else .pubias_empty_missing()

  list(avail_idx    = which(te_finite),
       studlab_obj  = studlab_obj,
       n_obj        = n_obj,
       missing_rows = rbind(auto_miss_df, user_miss_df))
}

# Build, check and annotate the two-subgroup metagen from
# .pubias_subgroup_rows()'s partition.
.pubias_subgroup_meta <- function(meta_obj, rows) {
  avail_idx    <- rows$avail_idx
  studlab_obj  <- rows$studlab_obj
  n_obj        <- rows$n_obj
  missing_rows <- rows$missing_rows
  k_avail <- length(avail_idx)
  k_miss  <- nrow(missing_rows)
  blank_miss <- rep(NA_real_, k_miss)
  exclude_obj <- meta_obj$exclude
  exclude_avail <- if (length(exclude_obj) >= max(avail_idx, 0L)) {
    as.logical(exclude_obj[avail_idx])
  } else rep(FALSE, k_avail)

  # De-duplicate study labels (meta::metagen rejects exact duplicates).
  studlab_combined <- make.unique(c(studlab_obj[avail_idx],
                                    missing_rows$studlab), sep = " #")
  refit_args <- c(
    list(
      TE      = c(meta_obj$TE[avail_idx],   blank_miss),
      seTE    = c(meta_obj$seTE[avail_idx], blank_miss),
      # The analysis' own study CIs, so a t-based metacont keeps them.
      lower   = c(meta_obj$lower[avail_idx], blank_miss),
      upper   = c(meta_obj$upper[avail_idx], blank_miss),
      studlab = studlab_combined,
      exclude = c(exclude_avail, rep(FALSE, k_miss)),
      subgroup = factor(
        rep(c(PUBIAS_SUBGROUP_AVAILABLE, PUBIAS_SUBGROUP_MISSING),
            c(k_avail, k_miss)),
        levels = c(PUBIAS_SUBGROUP_AVAILABLE, PUBIAS_SUBGROUP_MISSING)),
      subgroup.name = "",
      # {meta}'s own spelling ("ROM"), which its back-transformation needs.
      sm      = if (nzchar(meta_obj$sm %||% "")) meta_obj$sm else "MD",
      common  = FALSE,
      random  = TRUE,
      prediction = FALSE,
      prediction.subgroup = TRUE
    ),
    .pubias_model_settings(meta_obj)
  )

  fit <- function(args) {
    fit_error <- NULL
    fitted <- tryCatch(
      # {meta} warns on NA rows and on tau.preset forcing tau.common; both
      # are expected here.
      suppressWarnings(do.call(meta::metagen, args)),
      error = function(e) { fit_error <<- conditionMessage(e); NULL }
    )
    if (is.null(fitted)) {
      rlang::abort(sprintf(
        "plot_forest_pubias_subgroup: failed to build combined meta object: %s",
        fit_error %||% "unknown error"))
    }
    fitted
  }

  m_combined <- fit(refit_args)
  if (!.pubias_pool_matches(m_combined, meta_obj)) {
    # A pooling method whose tau metagen does not reproduce may still pool
    # the same once tau is fixed at the analysis' own value.
    tau_analysis <- as.numeric(meta_obj$tau %||% NA_real_)[1]
    if (is.finite(tau_analysis)) {
      m_combined <- fit(c(refit_args, list(tau.preset = tau_analysis)))
    }
    if (!.pubias_pool_matches(m_combined, meta_obj)) {
      rlang::warn(paste0(
        "plot_forest_pubias_subgroup: the Available results pool differs from ",
        "the analysis' own pooled estimate or intervals (method = \"",
        meta_obj$method %||% "unknown", "\"); the plot is drawn with the ",
        "refit's values."))
    }
  }

  m_combined$n_total <- c(n_obj[avail_idx], missing_rows$n)
  m_combined$results_known <- c(rep("", k_avail), missing_rows$results_known)
  m_combined
}
