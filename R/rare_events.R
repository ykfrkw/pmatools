# rare_events.R - Rare-event pairwise meta-analysis helpers

#' Diagnose rare-event patterns in binary pairwise data
#'
#' @param x Canonical long-format data or a \code{metabin} object.
#' @param experimental_label,control_label Optional arm labels for long data.
#' @param event_rate_threshold Event-rate threshold used as the primary rare
#'   event signal. Default is 0.01 (1 percent).
#' @param zero_study_fraction_threshold Fraction of studies with at least one
#'   zero-event arm used as a sparse-data warning. Default is 0.50.
#'
#' @return A list with event rates, zero-cell counts, and \code{rare_flow}.
#' @export
rare_event_diagnostics <- function(x,
                                   experimental_label = NULL,
                                   control_label = NULL,
                                   event_rate_threshold = 0.01,
                                   zero_study_fraction_threshold = 0.50) {
  wide <- .rare_to_wide(x, experimental_label, control_label)
  .rare_diagnostics_from_wide(
    wide,
    event_rate_threshold = event_rate_threshold,
    zero_study_fraction_threshold = zero_study_fraction_threshold
  )
}

#' Run a small suite of rare-event meta-analysis methods
#'
#' @param data Canonical long-format binary data.
#' @param effect_scale \code{"OR"} (recommended) or \code{"RR"}.
#' @param primary_method Optional method id to force as primary.
#' @param random,common Logical model flags.
#' @param method.tau Between-study variance estimator for methods that use it.
#' @param experimental_label,control_label Optional arm labels.
#'
#' @return An object of class \code{pma_rare_meta}.
#' @export
run_rare_ma <- function(data,
                        effect_scale = c("OR", "RR"),
                        primary_method = NULL,
                        random = TRUE,
                        common = FALSE,
                        method.tau = c("REML", "DL"),
                        experimental_label = NULL,
                        control_label = NULL) {
  effect_scale <- match.arg(effect_scale)
  method.tau <- match.arg(method.tau)
  wide <- .rare_to_wide(data, experimental_label, control_label)
  diagnostics <- .rare_diagnostics_from_wide(wide)
  specs <- .rare_method_specs(effect_scale)

  fits <- lapply(specs, function(spec) {
    .fit_rare_method(wide, spec, random = random, common = common,
                     method.tau = method.tau)
  })
  names(fits) <- vapply(specs, `[[`, character(1), "id")

  ok <- vapply(fits, function(z) inherits(z$meta, "meta"), logical(1))
  if (is.null(primary_method)) {
    primary_method <- if (effect_scale == "OR" && isTRUE(ok[["BB_CR"]])) {
      "BB_CR"
    } else if (effect_scale == "OR" && isTRUE(ok[["MH_no_cc"]])) {
      "MH_no_cc"
    } else {
      names(ok)[which(ok)[1]]
    }
  }
  if (length(primary_method) != 1 || !primary_method %in% names(fits) ||
      !isTRUE(ok[[primary_method]])) {
    primary_method <- names(ok)[which(ok)[1]]
  }
  primary <- if (length(primary_method) && !is.na(primary_method)) {
    fits[[primary_method]]$meta
  } else {
    NULL
  }

  method_table <- do.call(rbind, lapply(fits, function(z) {
    .rare_method_row(z, diagnostics, primary_method)
  }))
  primary_idx <- which(method_table$method_id == primary_method)
  if (length(primary_idx) == 1L) {
    method_table <- method_table[c(primary_idx,
                                   setdiff(seq_len(nrow(method_table)), primary_idx)),
                                 , drop = FALSE]
  }
  rownames(method_table) <- NULL

  structure(
    list(
      primary = primary,
      primary_method = primary_method,
      fits = fits,
      method_table = tibble::as_tibble(method_table),
      diagnostics = diagnostics,
      effect_scale = effect_scale
    ),
    class = "pma_rare_meta"
  )
}

#' Plot rare-event sensitivity estimates in one forest plot
#'
#' @param x A \code{pma_rare_meta} object from \code{\link{run_rare_ma}}.
#' @param title Optional title.
#' @param xlim Optional x-axis limits on the back-transformed scale.
#' @param threshold_lines Optional positive log-scale threshold. Drawn at
#'   exp(-threshold) and exp(threshold).
#' @param favors_left,favors_right Optional labels shown on the left and right
#'   of the x-axis.
#'
#' @return Invisibly \code{NULL}. Draws on the active graphics device.
#' @export
plot_rare_sensitivity_forest <- function(x,
                                         title = NULL,
                                         xlim = NULL,
                                         threshold_lines = NULL,
                                         favors_left = NULL,
                                         favors_right = NULL,
                                         ...) {
  if (!inherits(x, "pma_rare_meta")) {
    rlang::abort("x must be a pma_rare_meta object from run_rare_ma().")
  }
  tab <- as.data.frame(x$method_table, stringsAsFactors = FALSE)
  if (nrow(tab) == 0L) {
    graphics::plot.new()
    graphics::title(main = title %||% "Rare-event sensitivity analysis")
    return(invisible(NULL))
  }

  valid <- is.finite(tab$estimate) & is.finite(tab$ci_low) & is.finite(tab$ci_high) &
    tab$estimate > 0 & tab$ci_low > 0 & tab$ci_high > 0
  seTE <- rep(NA_real_, nrow(tab))
  seTE[valid] <- (log(tab$ci_high[valid]) - log(tab$ci_low[valid])) /
    (2 * stats::qnorm(0.975))
  valid <- valid & is.finite(seTE) & seTE > 0
  if (!any(valid)) {
    graphics::plot.new()
    graphics::title(main = title %||% "Rare-event sensitivity analysis")
    return(invisible(NULL))
  }

  if (is.null(xlim)) {
    vals <- c(tab$ci_low[valid], tab$ci_high[valid], 1)
    vals <- vals[is.finite(vals) & vals > 0]
    if (length(vals) == 0L) vals <- c(0.1, 10)
    rng <- range(vals)
    pad <- exp(0.15 * diff(log(rng)))
    xlim <- c(rng[1] / pad, rng[2] * pad)
    xlim <- c(max(xlim[1], 0.001), min(xlim[2], 1000))
  }

  lab <- tab$label
  if ("role" %in% names(tab)) {
    primary_row <- tab$role == "Primary"
    lab[primary_row] <- paste0(lab[primary_row], " (primary)")
  }
  peto_note <- any(tab$method_id == "Peto" &
                     grepl("Peto assumptions may not hold", tab$status %||% ""),
                   na.rm = TRUE)
  m <- meta::metagen(
    TE = log(tab$estimate[valid]),
    seTE = seTE[valid],
    studlab = lab[valid],
    sm = x$effect_scale,
    common = FALSE,
    random = FALSE,
    backtransf = TRUE,
    method.tau = "DL",
    warn = FALSE
  )

  args <- list(
    x = m,
    common = FALSE,
    random = FALSE,
    prediction = FALSE,
    smlab = title %||% "Rare-event method sensitivity",
    leftcols = "studlab",
    leftlabs = "Method",
    rightcols = "effect.ci",
    rightlabs = sprintf("%s (95%% CI)", x$effect_scale),
    xlim = xlim,
    at = .rare_log_ticks(xlim),
    fs.study = 9,
    fs.heading = 10,
    spacing = 0.9,
    ...
  )
  if (!is.null(favors_left) && nzchar(favors_left)) {
    args$label.left <- favors_left
  }
  if (!is.null(favors_right) && nzchar(favors_right)) {
    args$label.right <- favors_right
  }
  if (!is.null(threshold_lines) && is.numeric(threshold_lines) &&
      length(threshold_lines) == 1 && is.finite(threshold_lines) &&
      threshold_lines > 0) {
    args$xline <- c(exp(-threshold_lines), exp(threshold_lines))
  }

  tryCatch(
    {
      do.call(meta::forest, args)
      if (peto_note) .rare_draw_peto_note()
    },
    error = function(e) {
      args$leftcols <- NULL
      args$leftlabs <- NULL
      args$rightcols <- NULL
      args$rightlabs <- NULL
      args$xline <- NULL
      args$label.left <- NULL
      args$label.right <- NULL
      tryCatch(do.call(meta::forest, args), error = function(e2) {
        graphics::plot.new()
        graphics::title(main = title %||% "Rare-event sensitivity analysis")
      })
      if (peto_note) .rare_draw_peto_note()
    }
  )
  invisible(NULL)
}

.rare_to_wide <- function(x, experimental_label = NULL, control_label = NULL) {
  if (inherits(x, "meta")) {
    need <- c("event.e", "event.c", "n.e", "n.c", "studlab")
    if (!all(need %in% names(x))) {
      rlang::abort("rare_event_diagnostics() requires binary metabin arm data.")
    }
    return(data.frame(
      studlab = x$studlab,
      event_e = x$event.e,
      n_e = x$n.e,
      event_c = x$event.c,
      n_c = x$n.c,
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(x)) {
    rlang::abort("x must be a data.frame or meta object.")
  }
  if (!"event" %in% names(x)) {
    rlang::abort("rare-event diagnostics require an 'event' column.")
  }
  .long_to_wide(x, experimental_label, control_label)
}

.rare_diagnostics_from_wide <- function(wide,
                                        event_rate_threshold = 0.01,
                                        zero_study_fraction_threshold = 0.50) {
  event_e <- suppressWarnings(as.numeric(wide$event_e))
  event_c <- suppressWarnings(as.numeric(wide$event_c))
  n_e <- suppressWarnings(as.numeric(wide$n_e))
  n_c <- suppressWarnings(as.numeric(wide$n_c))

  keep <- is.finite(event_e) & is.finite(event_c) & is.finite(n_e) & is.finite(n_c) &
    n_e > 0 & n_c > 0
  event_e <- event_e[keep]; event_c <- event_c[keep]
  n_e <- n_e[keep]; n_c <- n_c[keep]

  k <- length(event_e)
  if (k == 0L) {
    rlang::abort("No complete binary studies available for rare-event diagnostics.")
  }

  single_zero <- xor(event_e == 0, event_c == 0)
  double_zero <- event_e == 0 & event_c == 0
  zero_cell <- event_e == 0 | event_c == 0
  both_events <- event_e > 0 & event_c > 0

  total_events_e <- sum(event_e)
  total_events_c <- sum(event_c)
  total_n_e <- sum(n_e)
  total_n_c <- sum(n_c)
  total_events <- total_events_e + total_events_c
  total_n <- total_n_e + total_n_c

  event_rate_e <- if (total_n_e > 0) total_events_e / total_n_e else NA_real_
  event_rate_c <- if (total_n_c > 0) total_events_c / total_n_c else NA_real_
  event_rate_overall <- if (total_n > 0) total_events / total_n else NA_real_
  zero_study_fraction <- mean(zero_cell)
  arm_imbalance_max <- max(pmax(n_e / n_c, n_c / n_e), na.rm = TRUE)

  rare_rate_flag <- any(c(event_rate_overall, event_rate_e, event_rate_c) <
                          event_rate_threshold, na.rm = TRUE)
  one_arm_total_zero <- identical(total_events_e, 0) || identical(total_events_c, 0)
  sparse_zero_pattern <- any(zero_cell) &&
    is.finite(event_rate_overall) && event_rate_overall < 0.05
  high_zero_fraction <- zero_study_fraction >= zero_study_fraction_threshold &&
    is.finite(event_rate_overall) && event_rate_overall < 0.05
  few_informative <- sum(both_events) <= 1L && any(zero_cell) &&
    is.finite(event_rate_overall) && event_rate_overall < 0.10

  rare_flow <- rare_rate_flag || one_arm_total_zero ||
    sparse_zero_pattern || high_zero_fraction || few_informative

  very_sparse_flag <- total_events < 20 || sum(both_events) <= 1L || one_arm_total_zero

  recommendation <- if (rare_flow) {
    paste(
      "Rare-event workflow recommended. Use OR as the default scale and compare",
      "continuity-correction-free or sparse-data methods in sensitivity analysis."
    )
  } else {
    "Rare-event workflow not triggered; use the regular pairwise MA workflow."
  }

  structure(
    list(
      rare_flow = isTRUE(rare_flow),
      rare_rate_flag = isTRUE(rare_rate_flag),
      zero_cell_flag = any(zero_cell),
      very_sparse_flag = isTRUE(very_sparse_flag),
      one_arm_total_zero = isTRUE(one_arm_total_zero),
      k = k,
      event_rate_overall = event_rate_overall,
      event_rate_e = event_rate_e,
      event_rate_c = event_rate_c,
      total_events = total_events,
      total_events_e = total_events_e,
      total_events_c = total_events_c,
      total_n = total_n,
      single_zero_k = sum(single_zero),
      double_zero_k = sum(double_zero),
      zero_cell_k = sum(zero_cell),
      both_arms_events_k = sum(both_events),
      zero_study_fraction = zero_study_fraction,
      arm_imbalance_max = arm_imbalance_max,
      event_rate_threshold = event_rate_threshold,
      recommendation = recommendation
    ),
    class = "pma_rare_diagnostics"
  )
}

.rare_method_specs <- function(effect_scale) {
  if (effect_scale == "RR") {
    return(list(
      list(id = "MH_no_cc_RR", label = "MH exact, no CC (RR)",
           method = "MH", sm = "RR", incr = 0, method.incr = "only0",
           MH.exact = TRUE, allstudies = TRUE,
           zero = "No continuity correction"),
      list(id = "IV_TACC_RR", label = "Inverse variance, TACC (RR)",
           method = "Inverse", sm = "RR", incr = "TACC", method.incr = "only0",
           MH.exact = NULL, allstudies = TRUE,
           zero = "Treatment-arm continuity correction")
    ))
  }
  list(
    list(id = "BB_CR", label = "Beta-binomial with correlated responses",
         engine = "mmeta", sm = "OR", package = "mmeta",
         zero = "No continuity correction; includes double-zero studies"),
    list(id = "MH_no_cc", label = "Mantel-Haenszel exact, no CC",
         method = "MH", sm = "OR", incr = 0, method.incr = "only0",
         MH.exact = TRUE, allstudies = TRUE, common = TRUE, random = FALSE,
         zero = "No continuity correction"),
    list(id = "GLMM", label = "GLMM (one-stage logistic random effects)",
         method = "GLMM", sm = "OR", package = "metafor",
         model.glmm = "UM.FS", common = FALSE, random = TRUE, method.tau = "ML",
         zero = "No continuity correction"),
    list(id = "Peto", label = "Peto, common effect",
         method = "Peto", sm = "OR", common = TRUE, random = FALSE,
         zero = "No continuity correction"),
    list(id = "REIV_TACC", label = "Random-effects IV, TACC (DL)",
         method = "Inverse", sm = "OR", incr = "TACC", method.incr = "only0",
         allstudies = TRUE, common = FALSE, random = TRUE, method.tau = "DL",
         zero = "Treatment-arm continuity correction"),
    list(id = "REIV_CC", label = "Random-effects IV, fixed 0.5 correction (DL)",
         method = "Inverse", sm = "OR", incr = 0.5, method.incr = "only0",
         allstudies = TRUE, common = FALSE, random = TRUE, method.tau = "DL",
         zero = "Fixed 0.5 continuity correction"),
    list(id = "MH_CC", label = "Mantel-Haenszel, fixed 0.5 correction",
         method = "MH", sm = "OR", incr = 0.5, method.incr = "only0",
         allstudies = TRUE, common = TRUE, random = FALSE,
         zero = "Fixed 0.5 continuity correction")
  )
}

.fit_rare_method <- function(wide, spec, random, common, method.tau) {
  if (!is.null(spec$package) &&
      !requireNamespace(spec$package, quietly = TRUE)) {
    return(list(spec = spec, meta = NULL,
                error = sprintf("Package '%s' is not installed.", spec$package)))
  }

  if (identical(spec$engine, "mmeta")) {
    return(.fit_beta_binomial_correlated(wide, spec))
  }

  common_arg <- if (!is.null(spec$common)) isTRUE(spec$common) else TRUE
  random_arg <- if (!is.null(spec$random)) isTRUE(spec$random) else isTRUE(random)

  args <- list(
    event.e = wide$event_e,
    n.e = wide$n_e,
    event.c = wide$event_c,
    n.c = wide$n_c,
    studlab = wide$studlab,
    sm = spec$sm,
    method = spec$method,
    common = common_arg,
    random = random_arg,
    method.tau = spec$method.tau %||% method.tau,
    method.random.ci = "classic",
    warn = FALSE
  )
  for (nm in c("incr", "method.incr", "MH.exact", "allstudies", "model.glmm")) {
    if (!is.null(spec[[nm]])) args[[nm]] <- spec[[nm]]
  }

  err <- NULL
  obj <- tryCatch(
    suppressWarnings(do.call(meta::metabin, args)),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )
  list(spec = spec, meta = obj, error = err)
}

.fit_beta_binomial_correlated <- function(wide, spec) {
  base <- tryCatch(
    suppressWarnings(meta::metabin(
      event.e = wide$event_e,
      n.e = wide$n_e,
      event.c = wide$event_c,
      n.c = wide$n_c,
      studlab = wide$studlab,
      sm = "OR",
      method = "MH",
      incr = 0,
      method.incr = "only0",
      MH.exact = TRUE,
      allstudies = TRUE,
      common = TRUE,
      random = FALSE,
      warn = FALSE
    )),
    error = function(e) NULL
  )
  if (!inherits(base, "meta")) {
    return(list(spec = spec, meta = NULL,
                error = "Could not create a display object for beta-binomial results."))
  }

  dat <- data.frame(
    y1 = wide$event_c,
    n1 = wide$n_c,
    y2 = wide$event_e,
    n2 = wide$n_e,
    study_name = make.unique(as.character(wide$studlab)),
    stringsAsFactors = FALSE
  )

  err <- NULL
  fit <- tryCatch({
    mt <- mmeta::MultipleTables.create(data = dat, measure = "OR", model = "Sarmanov")
    mt <- mmeta::MultipleTables.modelFit(
      mt,
      method = "sampling",
      verbose = FALSE,
      control = list(n_samples = 1000)
    )
    mmeta::MultipleTables.summary(mt, alpha = 0.05, verbose = FALSE)
  }, error = function(e) {
    err <<- conditionMessage(e)
    NULL
  })
  if (is.null(fit) || is.null(fit$overall_measure_estimation)) {
    return(list(spec = spec, meta = NULL,
                error = err %||% "Beta-binomial model failed."))
  }

  overall <- fit$overall_measure_estimation
  est <- suppressWarnings(as.numeric(overall$point))
  ci <- suppressWarnings(as.numeric(overall$confindent_interval))
  if (length(ci) < 2L || !all(is.finite(c(est, ci))) || any(c(est, ci) <= 0)) {
    return(list(spec = spec, meta = NULL,
                error = "Beta-binomial model returned a non-finite estimate."))
  }

  obj <- .rare_override_pooled_estimate(
    base,
    estimate = est,
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    model_label = "beta-binomial"
  )
  attr(obj, "pma_rare_engine") <- "mmeta"
  attr(obj, "pma_rare_method") <- spec$id
  attr(obj, "pma_rare_model") <- "Beta-binomial with correlated responses"

  list(
    spec = spec,
    meta = obj,
    error = NULL,
    model = "beta-binomial",
    k_included = nrow(wide)
  )
}

.rare_override_pooled_estimate <- function(obj, estimate, ci_low, ci_high,
                                           model_label) {
  TE <- log(estimate)
  lower <- log(ci_low)
  upper <- log(ci_high)
  seTE <- (upper - lower) / (2 * stats::qnorm(0.975))

  obj$TE.common <- TE
  obj$lower.common <- lower
  obj$upper.common <- upper
  obj$seTE.common <- seTE
  obj$common <- TRUE
  obj$random <- FALSE
  obj$overall <- TRUE
  obj$text.common <- model_label
  obj$k <- length(obj$studlab)
  obj$k.TE <- length(obj$studlab)
  obj$k.all <- length(obj$studlab)
  obj
}

.rare_method_row <- function(fit, diagnostics, primary_method) {
  spec <- fit$spec
  obj <- fit$meta
  estimate <- ci_low <- ci_high <- NA_real_
  model <- NA_character_
  status <- ""
  k_included <- NA_integer_
  if (inherits(obj, "meta")) {
    ext <- .rare_extract_estimate(obj)
    estimate <- ext$estimate
    ci_low <- ext$ci_low
    ci_high <- ext$ci_high
    model <- fit$model %||% ext$model
    k_included <- fit$k_included %||% obj$k
    if (is.finite(k_included) && k_included < diagnostics$k) {
      status <- sprintf("Excluded %d study/studies", diagnostics$k - k_included)
    }
  } else {
    status <- fit$error %||% "Method failed"
  }

  if (identical(spec$id, "Peto") &&
      (diagnostics$arm_imbalance_max > 2 ||
       diagnostics$event_rate_overall >= 0.01 ||
       (is.finite(estimate) && (estimate < 0.5 || estimate > 2)))) {
    status <- trimws(paste(status, "Peto assumptions may not hold"))
  }

  data.frame(
    role = if (identical(spec$id, primary_method)) "Primary" else "Sensitivity",
    method_id = spec$id,
    label = spec$label,
    effect_scale = spec$sm,
    model = model,
    zero_cell_handling = spec$zero %||% "",
    estimate = estimate,
    ci_low = ci_low,
    ci_high = ci_high,
    k_included = k_included,
    status = status,
    stringsAsFactors = FALSE
  )
}

.rare_extract_estimate <- function(obj) {
  if (isTRUE(obj$random) && !is.null(obj$TE.random) &&
      is.finite(obj$TE.random)) {
    return(list(
      estimate = exp(obj$TE.random),
      ci_low = exp(obj$lower.random),
      ci_high = exp(obj$upper.random),
      model = "random"
    ))
  }
  if (!is.null(obj$TE.common) && is.finite(obj$TE.common)) {
    return(list(
      estimate = exp(obj$TE.common),
      ci_low = exp(obj$lower.common),
      ci_high = exp(obj$upper.common),
      model = "common"
    ))
  }
  list(estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, model = NA_character_)
}

.rare_draw_peto_note <- function() {
  graphics::mtext(
    "Note: Peto may be inappropriate unless events are <1%, groups are balanced, and effects are small.",
    side = 1,
    line = 3,
    adj = 0,
    cex = 0.62,
    col = "gray35"
  )
}

.rare_log_ticks <- function(xlim) {
  std <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05,
           0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
  ticks <- std[std >= xlim[1] & std <= xlim[2]]
  if (length(ticks) >= 3L) ticks else pretty(xlim)
}
