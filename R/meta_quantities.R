# meta_quantities.R - numbers read off the analysis rather than supplied
#
# Split out of utils.R. Two quantities a caller may leave unstated, which the
# package then computes from the meta object's own arms: the control-group event
# rate (crude, or metaprop-pooled) and the sample-size-weighted pooled SD. The
# resolution of the three grade_meta() arguments that all name the control risk
# lives here too, because it is what decides whether that computation happens
# at all.
#
# A new helper belongs here when it derives a number from a fitted meta object
# and invents nothing. Restating a number on another scale is R/effect_scales.R.

# --------------------------------------------------------------------------
# Baseline risk helpers
# --------------------------------------------------------------------------

#' Resolve baseline risk to a single numeric probability
#'
#' @param baseline_risk NULL, a numeric scalar, "simple", or "metaprop"
#' @param meta_obj meta object (used for auto-computation)
#' @param ois_p0 Fallback when baseline_risk is NULL
#' @return Numeric scalar in 0..1 or NULL
#' @keywords internal
#' @noRd
.resolve_baseline_risk <- function(baseline_risk, meta_obj, ois_p0 = NULL) {
  # 1. Explicit numeric
  if (is.numeric(baseline_risk)) {
    if (baseline_risk < 0 || baseline_risk > 1)
      rlang::abort("baseline_risk must be between 0 and 1.")
    return(baseline_risk)
  }
  # 2. "simple" or "metaprop"
  if (is.character(baseline_risk) && baseline_risk %in% c("simple", "metaprop")) {
    return(.compute_control_risk(meta_obj, method = baseline_risk))
  }
  # 3. NULL -> fallback to ois_p0, then simple auto-compute
  if (is.null(baseline_risk)) {
    if (!is.null(ois_p0) && is.numeric(ois_p0)) return(ois_p0)
    return(.compute_control_risk(meta_obj, method = "simple"))
  }
  NULL
}

# The three grade_meta() arguments that all name the control-arm event rate,
# in the order a value inherits from them (see .resolve_control_risk()).
CONTROL_RISK_ARGS <- c("threshold_baseline", "ois_p0", "baseline_risk")

# Human labels for the resolution note, so it reads as prose rather than as
# three argument names in a row.
CONTROL_RISK_USES <- c(
  threshold_baseline = "the absolute-threshold conversion",
  ois_p0             = "the optimal information size",
  baseline_risk      = "the Summary of Findings baseline"
)

#' Share one control-arm risk across the three arguments that name it
#'
#' \code{threshold_baseline}, \code{ois_p0} and \code{baseline_risk} are three
#' names for the control-arm event rate, used by three different calculations.
#' A caller who has one number for all three had to pass it three times. This
#' resolves the value once: an argument that was supplied keeps its own value,
#' and one that was left \code{NULL} inherits the first value supplied to any
#' of the others, in the order given by \code{CONTROL_RISK_ARGS}.
#'
#' The order is not arbitrary. \code{threshold_baseline} is the risk of the
#' population the decision threshold is about, and the Shiny app makes the
#' reviewer confirm or justify it in writing; \code{ois_p0} is Core GRADE 2's
#' "control group event rate (chosen from the context)"; \code{baseline_risk}
#' is presentational, and is the one that can legitimately describe a different
#' population from the other two (a Summary of Findings table routinely prints
#' several baseline risks for one effect estimate). So the most deliberate
#' value donates first and the most presentational donates last -- and none of
#' them ever displaces a value the caller supplied.
#'
#' Nothing is invented here: an argument that is still \code{NULL} afterwards
#' reaches its own calculation as \code{NULL} and takes that calculation's own
#' pooled-control-rate default, which is computed on the analysis actually
#' being rated (the low-RoB refit, when one happened).
#'
#' Why not one argument: consolidating the three onto \code{baseline_risk} is
#' the eventual destination, and it is a breaking rename of three public
#' arguments. v0.5.1 already carries a breaking rename of the domain judgment
#' vocabulary, and stacking a second migration on one release costs users two
#' passes over their scripts for one release's benefit. See SPEC.md §4.5.4.
#'
#' @param threshold_baseline,ois_p0,baseline_risk The three arguments as
#'   \code{grade_meta()} received them.
#' @return A list with the three resolved arguments under their own names,
#'   plus \code{donor} (the argument the shared value came from, or
#'   \code{NULL}), \code{value} (the shared value, or \code{NULL}),
#'   \code{inherited} (the arguments that took it) and \code{note} (one
#'   sentence naming both, or \code{NULL}).
#' @keywords internal
#' @noRd
.resolve_control_risk <- function(threshold_baseline = NULL, ois_p0 = NULL,
                                  baseline_risk = NULL) {
  supplied <- list(threshold_baseline = threshold_baseline,
                   ois_p0             = ois_p0,
                   baseline_risk      = baseline_risk)
  out <- c(supplied, list(donor = NULL, value = NULL,
                          inherited = character(0), note = NULL))

  # A donor has to be a number that every one of the three uses would accept.
  # threshold_baseline rejects 0 and 1 outright, so an exact 0 or 1 supplied to
  # baseline_risk (which does allow the closed interval) stays where it was put
  # rather than turning a working call into an error somewhere else. A
  # character baseline_risk ("simple" / "metaprop") names a computation over
  # the analysis, not a value, and each use already performs that computation
  # on the analysis it is judging -- so it does not donate either.
  .is_donor <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 && x < 1
  }
  .is_unset <- function(x) {
    is.null(x) || length(x) == 0L || (is.numeric(x) && is.na(x))
  }

  donors <- CONTROL_RISK_ARGS[vapply(supplied[CONTROL_RISK_ARGS], .is_donor,
                                     logical(1))]
  if (length(donors) == 0L) return(out)

  donor <- donors[1]
  value <- supplied[[donor]]
  takers <- CONTROL_RISK_ARGS[vapply(supplied[CONTROL_RISK_ARGS], .is_unset,
                                     logical(1))]
  if (length(takers) == 0L) {
    # All three were supplied. Still worth recording which value each use got,
    # because they may legitimately differ and the record is what says so.
    out$donor <- donor
    out$value <- value
    return(out)
  }

  for (nm in takers) out[[nm]] <- value
  out$donor     <- donor
  out$value     <- value
  out$inherited <- takers
  out$note      <- sprintf(
    paste0("Control-group risk %.4f supplied as `%s`; %s inherited it ",
           "(one value reaches all three; a value passed explicitly is never ",
           "displaced)."),
    value, donor,
    paste(sprintf("`%s` (%s)", takers, CONTROL_RISK_USES[takers]),
          collapse = " and ")
  )
  out
}

#' Compute control-arm event rate from a metabin object
#' @param meta_obj A meta object (from metabin).
#' @param method One of "simple" or "metaprop".
#' @keywords internal
#' @noRd
.compute_control_risk <- function(meta_obj, method = "simple") {
  ec <- meta_obj$event.c
  nc <- meta_obj$n.c
  if (is.null(ec) || is.null(nc) || length(nc) == 0 || sum(nc, na.rm = TRUE) == 0) {
    return(NULL)
  }
  if (length(ec) != length(nc)) return(NULL)

  # Both vectors must be filtered on the same studies. A study that reports a
  # denominator but no event count (eg it contributed a continuous outcome
  # only) otherwise drops out of `ec` while staying in `nc`, which inflates the
  # crude denominator and hands metaprop() two vectors of different lengths --
  # the latter error was swallowed below and returned the crude proportion
  # under the guise of a random-effects pooled estimate.
  keep <- !is.na(ec) & !is.na(nc) & nc > 0
  if (!any(keep)) return(NULL)
  ec <- ec[keep]
  nc <- nc[keep]

  if (method == "simple") {
    return(sum(ec) / sum(nc))
  }

  if (method == "metaprop") {
    mp <- tryCatch(
      meta::metaprop(event = ec, n = nc,
                     method = "GLMM", sm = "PLOGIT",
                     method.tau = "ML"),
      error = function(e) NULL
    )
    if (!is.null(mp) && !is.na(mp$TE.random)) {
      return(stats::plogis(mp$TE.random))
    }
    warning("metaprop() failed; falling back to simple pooled proportion.")
    return(sum(ec) / sum(nc))
  }
  NULL
}

#' Compute sample-size-weighted pooled SD across studies
#'
#' For continuous-outcome meta-analyses (\code{\link[meta]{metacont}}), returns
#' the pooled standard deviation across studies, sample-size weighted.
#'
#' @param meta_obj A meta object (typically from
#'   \code{\link[meta]{metacont}}).
#'
#' @return A single numeric pooled SD, or \code{NULL} if input data are
#'   insufficient.
#'
#' @details
#' Per-study pooled SD uses Cohen's pooled formula:
#' \deqn{SD_{pooled} = \sqrt{\frac{(n_e - 1) SD_e^2 + (n_c - 1) SD_c^2}{n_e + n_c - 2}}}
#' Across studies, the per-study pooled SDs are averaged with weights equal to
#' the total per-study sample size (\eqn{n_e + n_c}).
#'
#' If \code{sd.e}/\code{sd.c} are unavailable, falls back to
#' \code{weighted.mean(seTE * sqrt(n_total), n_total)}.
#'
#' @export
compute_pooled_sd <- function(meta_obj) {
  n_e  <- meta_obj$n.e
  n_c  <- meta_obj$n.c
  sd_e <- meta_obj$sd.e
  sd_c <- meta_obj$sd.c

  if (!is.null(n_e) && !is.null(n_c) && !is.null(sd_e) && !is.null(sd_c) &&
      length(n_e) == length(sd_e)) {
    sd_per_study <- sqrt(
      ((n_e - 1) * sd_e^2 + (n_c - 1) * sd_c^2) /
      pmax(n_e + n_c - 2, 1)
    )
    weights <- n_e + n_c
    keep <- is.finite(sd_per_study) & is.finite(weights) & weights > 0
    if (any(keep)) {
      return(stats::weighted.mean(sd_per_study[keep], weights[keep]))
    }
  }

  # Fallback: derive from seTE
  seTE <- meta_obj$seTE
  if (!is.null(n_e) && !is.null(n_c) && !is.null(seTE)) {
    n_total <- n_e + n_c
    keep <- is.finite(seTE) & is.finite(n_total) & n_total > 0
    if (any(keep)) {
      # MD: SE ≈ sd_pooled * sqrt(1/n_e + 1/n_c) ≈ sd_pooled * sqrt(4/n_total)
      sd_approx <- seTE[keep] * sqrt(n_total[keep] / 4)
      return(stats::weighted.mean(sd_approx, n_total[keep]))
    }
  }

  NULL
}
