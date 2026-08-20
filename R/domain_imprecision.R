# domain_imprecision.R - the Imprecision domain, rated automatically
#
# BMJ 2025 Core GRADE 2: Imprecision
#
# This file owns one question and everything needed to answer it: given a pooled
# {meta} fit and a chosen threshold, does the confidence interval leave enough
# doubt to rate certainty down, and by how much? assess_imprecision() takes the
# fit plus the OIS inputs and hands back one make_domain_row(): a judgment, the
# sentence that justifies it, and the structured facts a caller can branch on
# without re-parsing that sentence. Everything else here is machinery it needs --
# the OIS calculation, the "implausibly large" test, the CI-ratio cut-offs, and
# the Fig 4 classifier that turns those into a verdict and a path string.
#
# What it does NOT decide is which threshold it is rating against. Core GRADE 2
# Fig 2 makes that choice and R/rating_target.R implements it; grade_meta() hands
# the answer down as threshold_for_imprecision. A helper belongs here when it
# reads the interval or sizes the evidence; when it decides what the rating is a
# rating OF, it belongs next door in rating_target.R.
#
# References:
#   Guyatt G, Brignardello-Petersen R, Hultcrantz M, et al.
#     Core GRADE 2: choosing the target of certainty rating and assessing
#     imprecision. BMJ. 2025;389:e081904.
#     doi:10.1136/bmj-2024-081904
#   Zeng L, Brignardello-Petersen R, Hultcrantz M, et al.
#     GRADE Guidance 34: update on rating imprecision using a minimally
#     contextualised approach. BMJ. 2025;389:e083087. (companion methodology
#     paper)
#   Guyatt GH, Oxman AD, Kunz R, et al. GRADE guidelines 6.
#     Rating the quality of evidence — imprecision (Optimal Information
#     Size). J Clin Epidemiol. 2011;64(12):1283-1293.
#     doi:10.1016/j.jclinepi.2011.01.012 (PMID 21839614)
#
# The decision rule, laid out in Fig 4's own structure (BMJ 2025 Core GRADE 2):
#
#   Evaluate CI in relation to chosen threshold — does CI cross threshold?
#
#   Yes -> Rate down one level                                    [-1]
#          Consider rating down two levels if:                    [-2]
#            - CI crosses two thresholds (both important benefit and
#              important harm), or
#            - the most appropriate plain language description suggests more
#              uncertainty ("may" rather than "likely").
#          Sample size and the OIS are NOT consulted on this path (body text:
#          "Core GRADE users will rate down for imprecision and do not need to
#          consider sample size").
#
#   No  -> Moderate effect -> Do not rate down                    [-0]
#       -> Large effect    -> Proceed to OIS approach
#
#          OIS approach (Fig 4, lower half):
#            Continuous outcome:
#              N >= OIS (or 800)   -> Do not rate down            [-0]
#              N <  OIS            -> Rate down one level         [-1]
#              N <  30% of OIS     -> Consider rating down two    [-2]
#            Binary outcome:
#              Relative risk CI ratio >= 3 or odds ratio CI ratio >= 2.5
#                                  -> Consider rating down two    [-2]
#              otherwise -> Calculate OIS
#                              N >= OIS -> Do not rate down       [-0]
#                              N <  OIS -> Rate down one level    [-1]
#
#   The two-level downgrade for crossing BOTH MIDs applies on the null-threshold
#   path as well, not only where a MID was the chosen threshold. Body text, p6,
#   verbatim:
#     "The two considerations also apply to imprecision judgments when Core
#      GRADE users choose the null as the threshold of interest. For example,
#      consider a situation in which users rate their certainty in a benefit
#      (threshold the null) but the CI also includes clearly important harm.
#      The finding that the CI is consistent with both benefit and important
#      harm motivates a plain language summary stating that the intervention
#      'may' result in a benefit, and rating down two levels for imprecision."
#   So with rating target = non_null_effect (threshold = null), a MID that
#   exists is still put to work: whether the CI crosses +/-MID is evaluated
#   separately, and crossing both sides earns -2. The -1 / -0 decision is
#   untouched by this and goes on being made against the null (= 0). With no MID
#   the both-sides question cannot be asked at all, so that path stops at -1.
#
#   CI ratio (Fig 4 caption): the upper CI limit divided by the lower limit, on
#   the ratio scale.
#   "Large effect" means implausibly large, and the body text operationalises it
#   for BINARY outcomes only: "implausibly large (certainly relative risk
#   reduction >40%, possibly >30%)". For continuous outcomes the source offers
#   no definition of a large effect whatsoever, so pmatools falls back on
#   Cohen's convention (standardized effect >= 0.8) and says so in the notes
#   every time it fires. That convention is pmatools', not Core GRADE 2's, and
#   the note exists so nobody reads it back out as the paper's.
#
# How the rating target picks the threshold (Core GRADE 2 Fig 2;
# R/rating_target.R):
#   target = non_null_effect            -> the threshold is the null (= 0)
#   target = important_effect /
#            little_to_no_difference    -> the threshold is +/-MID
#   assess_imprecision() receives that choice ALREADY MADE, as the
#   threshold_for_imprecision argument, and does not re-derive it.
#
# Where the null sits:
#   {meta}'s lower.random / upper.random are on the log scale for RR / OR / HR
#   and on the raw scale for MD / SMD. The null is 0 on both, so a single
#   null_val = 0 answers the crosses-the-null question for every measure.
# Threshold:
#   threshold_internal is a positive value on the TE scale (log for ratio
#   measures, raw for absolute ones). The automatic OIS derivations read
#   threshold_internal, never threshold_for_imprecision. For a continuous
#   outcome the MID becomes ois_delta directly. For a binary one it does not:
#   ois_p1 comes from a modest relative risk reduction instead (see below), and
#   the MID reaches the OIS only where the threshold arrived as an ARD, whose
#   baseline risk is then reused to anchor ois_p0.
#
# How the OIS (optimal information size) is computed:
#
#   The unit of comparison is PARTICIPANTS. Core GRADE 2 Fig 4 caption, verbatim:
#   "N=number of participants; OIS=optimal information size"; body text, p6:
#   "If the total sample size of all the studies included in a meta-analysis
#    exceeds the OIS, one does not rate down". Binary outcomes are no exception:
#   it is the total sample size (n.e + n.c) that is compared with the OIS, not
#   the total event count, which is reported alongside for information only. An
#   explicitly supplied ois_events is the one thing that moves the comparison
#   onto events, and it exists for backward compatibility. (The rare-event flow
#   moves it too, for a quite different reason -- see RARE EVENTS below.)
#
#   Route 1 -- supplied directly:
#     ois_events: target total events for a binary outcome (backward-compatible)
#     ois_n     : target total sample size
#
#   Route 2 -- computed, used when route 1 supplied neither:
#     Binary outcome: supply ois_p0 and ois_p1
#       n_arm = (z_alpha/2 + z_beta)^2 × [p0(1-p0) + p1(1-p1)] / (p0-p1)^2
#       OIS_n = 2 × n_arm                (participants)
#       for information: OIS_events ≈ 2 × n_arm × p̄  (p̄ = (p0+p1)/2)
#     Continuous outcome: supply ois_delta and ois_sd
#       n_arm = 2 × (z_alpha/2 + z_beta)^2 × sigma^2 / delta^2
#       OIS_n = 2 × n_arm
#     Defaults: ois_alpha = 0.05 (two-sided), ois_beta = 0.20 (80% power)
#
#   Deriving ois_p1 (binary):
#     Core GRADE 2, body text p6, verbatim:
#       "For binary outcomes, these involve specifying the acceptable error
#        rates: alpha (typically 0.05) and beta (typically 0.20), the control
#        group event rate (chosen from the context), and a modest relative risk
#        reduction, typically 20% or 25%."
#     So the binary OIS is set by a MODEST RELATIVE RISK REDUCTION and not by
#     the MID. pmatools derives ois_p1 from ois_rrr (default 0.20); an
#     explicitly supplied ois_p1 takes precedence over the derivation. The same
#     paragraph writes the continuous case out separately ("by specifying the
#     smallest difference between intervention and control that one would want
#     to avoid missing (ie, the MID)"), and there the MID does become
#     ois_delta, as it always has.
#
#   Direction of the binary OIS alternative rate (v0.5.1):
#     Core GRADE 2 writes "reduction" because its worked example has an
#     UNDESIRABLE event. For an outcome whose events are desirable (response,
#     remission) a benefit is an INCREASE in the event rate, and powering the
#     OIS against p0 * (1 - rrr) targets the wrong tail. `small_values` decides
#     the sign and the pooled effect is reported alongside it; see
#     .ois_target_increase(). `small_values` is required as of 0.5.1, so there
#     is no "direction unknown" case left to fall back on.
#
#   Deriving ois_sd (continuous, v0.5.1):
#     .calc_ois() wants both ois_delta and ois_sd for a continuous outcome, and
#     until 0.5.1 nothing ever filled ois_sd in: left blank by the caller it
#     stayed NULL, so the continuous OIS was silently uncomputable and Fig 4's
#     large-effect path fell through to "do not rate down" with no explanation
#     of why the OIS was missing. An unsupplied ois_sd is now derived from
#     compute_pooled_sd(), and the notes record that it was derived rather than
#     supplied. See the SMD exception where the derivation happens.
#
# RARE EVENTS (`rare_flow`; shiny/SPEC.md 3.4.14)
# ------------------------------------------------
# Two corrections, and NOTHING else. Fig 4's rule is untouched: whether the
# confidence interval crosses the chosen threshold stays exactly Core GRADE 2's
# question, and sparse data earns no automatic downgrade of its own.
#
#   1. The OIS switches to an EVENT basis. Core GRADE 2 Fig 4 compares
#      participants ("N=number of participants") because that is the quantity
#      that limits an ordinary trial. When the events are what is scarce it is
#      the wrong denominator: at a 0.5% event rate a "sufficiently large"
#      participant count can carry a dozen events, and the OIS then reports a
#      body of evidence as adequately sized when nothing in it could have
#      detected anything. `.calc_ois()` already computes the implied event
#      count alongside the participant target; under `rare_flow` that count
#      becomes the target and `.compute_ois_pct()` compares total events with
#      it. The basis is named in the notes and in the "ois_basis" fact, because
#      an OIS percentage means two different things on the two bases.
#
#      This is not a stricter reading of Core GRADE 2. It is the reading that
#      is arithmetically meaningful on data the paper's example does not cover.
#
#   2. `rare_one_arm_total_zero` makes the domain NOT ASSESSABLE. With no
#      events at all in one arm across every study there is no finite odds
#      ratio and no interval to compare with a threshold, so Fig 4 has no
#      question to answer. The row records "not_serious" - no automatic
#      downgrade, per the governing rule - and says in those words that the
#      domain was not assessed. The reviewer's own confirmation is what carries
#      the decision from there.

# The judgment recorded when a domain could not be assessed at all. NOT a
# verdict of "no concern": it is the absence of one, and the note beside it
# says so. Named because Inconsistency withdraws its own automated path on the
# same data and must record the same thing (R/domain_inconsistency.R).
PMA_NOT_ASSESSABLE_JUDGMENT <- "not_serious"

assess_imprecision <- function(meta_obj,
                               outcome_type       = "relative",
                               ois_events         = NULL,
                               ois_n              = NULL,
                               ois_alpha          = 0.05,
                               ois_beta           = 0.20,
                               ois_p0             = NULL,
                               ois_p1             = NULL,
                               ois_delta          = NULL,
                               ois_sd             = NULL,
                               ois_rrr            = 0.20,
                               # Outcome direction, as collected in the app's
                               # Step 2 and already forwarded to assess_rob():
                               # "desirable" or "undesirable", required. It
                               # decides which way the modest RRR moves the OIS
                               # alternative rate, and the wording of the
                               # large-effect note.
                               small_values,
                               threshold_internal = NULL,
                               threshold_kind     = NULL,
                               threshold_ard      = NULL,
                               threshold_p0       = NULL,
                               # UNUSED (kept only so that grade_meta() can
                               # forward its full argument list without a
                               # special case). The rating target and the
                               # threshold type reach this function ALREADY
                               # RESOLVED, as threshold_for_imprecision: Core
                               # GRADE 2 Fig 2 picks the threshold, and
                               # rating_target.R hands the chosen value down.
                               # Reading rating_target / threshold_type here
                               # would risk a second, divergent resolution.
                               rating_target      = NULL,
                               threshold_type     = NULL,
                               threshold_for_imprecision = NULL,
                               # Rare-event corrections; see the file header.
                               # Both default off, so nothing changes for an
                               # analysis that never met the rare-event
                               # workflow.
                               rare_flow          = FALSE,
                               rare_one_arm_total_zero = FALSE) {
  # Validated here as well as at grade_meta()'s entry gate: an assessor called
  # directly is a caller too, and a lenient one would re-open the guessing hole
  # the gate exists to close. The formal carries no default so that the
  # requirement is visible in the signature; missing() then routes an omission
  # to the same explanatory abort a NULL gets, rather than to R's bare
  # "argument is missing".
  if (missing(small_values)) small_values <- NULL
  .check_small_values(small_values)

  # One arm with no events at all: Fig 4 has nothing to evaluate. Returned
  # before the CI is even read, because the interval that exists in such a fit
  # is an artefact of whichever sparse-data model produced it rather than a
  # range the threshold can be compared with.
  if (isTRUE(rare_one_arm_total_zero)) {
    return(make_domain_row(
      domain   = "Imprecision",
      judgment = PMA_NOT_ASSESSABLE_JUDGMENT,
      auto     = TRUE,
      notes    = paste0(
        "IMPRECISION NOT ASSESSABLE: one arm has no events at all, in any ",
        "study. There is no finite odds ratio and no confidence interval to ",
        "compare with the threshold, so Core GRADE 2 Fig 4's question cannot ",
        "be asked. No downgrade is applied automatically - that would be a ",
        "rating derived from a computation that did not happen. Judge the ",
        "domain yourself and record it with imprecision = <level> plus ",
        "imprecision_rationale, or confirm the domain as it stands."
      ),
      facts    = .facts(
        .fact("imprecision_assessable", "Imprecision assessable",
              "no - one arm has no events at all"),
        .fact("rare_flow", "Rare-event analysis", "yes")
      )
    ))
  }

  if (isTRUE(meta_obj$random)) {
    lower <- meta_obj$lower.random
    upper <- meta_obj$upper.random
  } else {
    lower <- meta_obj$lower.common
    upper <- meta_obj$upper.common
  }
  if (is.null(lower) || is.null(upper) ||
      length(lower) == 0L || length(upper) == 0L ||
      !is.finite(lower) || !is.finite(upper)) {
    if (isTRUE(meta_obj$random)) {
      lower <- meta_obj$lower.common
      upper <- meta_obj$upper.common
    } else {
      lower <- meta_obj$lower.random
      upper <- meta_obj$upper.random
    }
  }

  if (is.null(lower) || is.null(upper) || is.na(lower) || is.na(upper)) {
    return(make_domain_row(
      domain   = "Imprecision",
      judgment = "not_serious",
      auto     = TRUE,
      notes    = "CI not available; imprecision not assessed."
    ))
  }

  # null = 0 (log scale for RR/OR/HR; original scale for MD/SMD)
  null_val    <- 0.0
  crosses_null <- (lower < null_val) && (upper > null_val)

  # Which threshold does Fig 4 evaluate the CI against? The rating target
  # decides (Core GRADE 2 Fig 2): a non-null-effect target uses the null,
  # every other target uses +/-MID. threshold_for_imprecision carries that
  # choice down from grade_meta(); when absent (direct calls / older code)
  # fall back to the MID.
  thr_eff <- if (!is.null(threshold_for_imprecision)) {
    threshold_for_imprecision
  } else {
    threshold_internal
  }

  # A CI "crosses" a threshold T iff T lies inside the CI: lower < T AND upper > T.
  # Four states relative to the [-Threshold, +Threshold] trivial zone:
  #   crosses_both_thresholds : CI contains both -T and +T (lower < -T AND upper > +T)
  #   crosses_one_threshold   : CI contains exactly one of {-T, +T}
  #   within_thresholds       : CI lies entirely in trivial zone
  #                             (-T <= lower AND upper <= +T)
  #   beyond_thresholds       : CI lies entirely outside trivial zone on one side
  #                             (upper <= -T OR lower >= +T)
  has_threshold <- .has_mid(thr_eff)

  # The +/-MID zone is evaluated whenever a MID exists, even when the rating
  # threshold is the null: Core GRADE 2 (p6) explicitly extends the two-level
  # consideration ("the CI also includes clearly important harm") to the
  # null-threshold path. On that path the -1 / -0 decision still uses the null;
  # only the "crosses BOTH thresholds -> rate down two levels" branch consults
  # +/-MID.
  mid_zone <- if (has_threshold) {
    thr_eff
  } else if (.has_mid(threshold_internal)) {
    threshold_internal
  } else {
    NULL
  }
  has_mid_zone <- !is.null(mid_zone)

  if (has_mid_zone) {
    crosses_lower_threshold <- (lower < -mid_zone) && (upper > -mid_zone)
    crosses_upper_threshold <- (lower <  mid_zone) && (upper >  mid_zone)
    crosses_both_thresholds <- crosses_lower_threshold && crosses_upper_threshold
    crosses_one_threshold   <- xor(crosses_lower_threshold, crosses_upper_threshold)
    within_thresholds       <- (lower >= -mid_zone) && (upper <= mid_zone)
    beyond_thresholds       <- (upper <= -mid_zone) || (lower >= mid_zone)
  } else {
    crosses_lower_threshold <- NA
    crosses_upper_threshold <- NA
    crosses_both_thresholds <- NA
    crosses_one_threshold   <- NA
    within_thresholds       <- NA
    beyond_thresholds       <- NA
  }

  crosses_threshold <- if (has_threshold) {
    crosses_lower_threshold || crosses_upper_threshold
  } else {
    # Null threshold (target = non-null effect, or no MID available).
    crosses_null
  }

  # Defensive: treat NA as NULL
  if (!is.null(ois_events) && (length(ois_events) == 0 || is.na(ois_events))) ois_events <- NULL
  if (!is.null(ois_n)      && (length(ois_n)      == 0 || is.na(ois_n)))      ois_n      <- NULL
  if (!is.null(ois_p0)     && (length(ois_p0)     == 0 || is.na(ois_p0)))     ois_p0     <- NULL
  if (!is.null(ois_p1)     && (length(ois_p1)     == 0 || is.na(ois_p1)))     ois_p1     <- NULL
  if (!is.null(ois_delta)  && (length(ois_delta)  == 0 || is.na(ois_delta)))  ois_delta  <- NULL
  if (!is.null(ois_sd)     && (length(ois_sd)     == 0 || is.na(ois_sd)))     ois_sd     <- NULL

  # The summary measure and the pooled effect are wanted twice: here, to decide
  # which way the modest RRR moves the OIS alternative rate, and below for
  # Fig 4's large-effect check. Computed once.
  sm       <- meta_obj$sm
  te_point <- .pooled_te(meta_obj)

  # Derive the OIS inputs that were not supplied explicitly.
  #   binary     : ois_p0 (control-arm risk) + ois_rrr (modest RRR, Core GRADE 2)
  #   continuous : ois_delta = MID           (Core GRADE 2, same paragraph)
  #                ois_sd    = pooled within-study SD, when not supplied
  threshold_used_note <- ""
  ois_direction   <- .ois_target_increase(small_values, te_point)
  ois_p1_derived  <- FALSE
  ois_sd_derived  <- FALSE
  # Where a derived ois_sd came from, in words. The pooled-SD wording is not
  # true of every derivation any more (the SMD takes 1), so the fact below
  # quotes this rather than re-asserting a provenance it cannot see.
  ois_sd_source   <- ""
  has_mid_for_ois <- !is.null(threshold_internal) && !is.na(threshold_internal) &&
                     threshold_internal != 0
  if (is.null(ois_events) && is.null(ois_n)) {
    if (outcome_type == "relative") {
      # ARD Threshold converted to the ratio scale: anchor ois_p0 to the same
      # baseline risk that was used for the conversion, for consistency.
      if (is.null(ois_p0) && has_mid_for_ois && !is.null(threshold_ard) &&
          !is.null(threshold_p0) && is.finite(threshold_p0)) {
        ois_p0 <- threshold_p0
        threshold_used_note <- sprintf(
          " (ois_p0 from threshold baseline risk = %.4f)", ois_p0
        )
      }
      # Auto-fall back ois_p0 to control-arm pooled proportion if missing.
      # Core GRADE 2 calls for "the control group event rate (chosen from the
      # context)", so the observed control-arm risk is the natural default.
      if (is.null(ois_p0)) {
        cer <- tryCatch(.compute_control_risk(meta_obj, method = "simple"),
                        error = function(e) NULL)
        if (!is.null(cer) && is.finite(cer) && cer > 0 && cer < 1) {
          ois_p0 <- cer
          threshold_used_note <- sprintf(
            " (ois_p0 auto from data = %.4f)", ois_p0
          )
        }
      }
      if (is.null(ois_p1) && !is.null(ois_p0)) {
        # Core GRADE 2 (p6): binary OIS uses "a modest relative risk reduction,
        # typically 20% or 25%" -- NOT the MID. The MID is reserved for the
        # continuous branch, which the same paragraph writes out separately.
        # Which SIDE of ois_p0 the alternative rate sits on is decided by
        # .ois_target_increase(); the paper's "reduction" wording assumes an
        # undesirable event and does not generalise on its own.
        rrr     <- .check_ois_rrr(ois_rrr)
        raw_p1  <- if (isTRUE(ois_direction$increase)) {
          ois_p0 * (1 + rrr)
        } else {
          ois_p0 * (1 - rrr)
        }
        ois_p1  <- max(min(raw_p1, 1 - 1e-6), 1e-6)
        clamped <- !isTRUE(all.equal(ois_p1, raw_p1))
        ois_p1_derived <- TRUE
        threshold_used_note <- paste0(threshold_used_note, sprintf(
          paste0(" (ois_p1 from a modest relative risk %s, ois_rrr = ",
                 "%.0f%%: ois_p1 = %.4f%s; direction: %s; Core GRADE 2 ",
                 "specifies an RRR rather than the threshold for binary ",
                 "outcomes)"),
          if (isTRUE(ois_direction$increase)) "increase" else "reduction",
          100 * rrr, ois_p1,
          if (clamped) sprintf(
            paste0(", clamped into (0, 1) from %.4f -- the control-group risk ",
                   "is too high for a %.0f%% relative increase to stay a ",
                   "probability, so the OIS below is powered against the ",
                   "clamped rate"), raw_p1, 100 * rrr) else "",
          ois_direction$reason
        ))
      }
    } else {
      # Continuous outcomes: Core GRADE 2 keeps the MID here ("by specifying
      # the smallest difference between intervention and control that one would
      # want to avoid missing (ie, the MID)").
      if (is.null(ois_delta) && has_mid_for_ois) {
        ois_delta <- threshold_internal
        threshold_used_note <- sprintf(
          " (ois_delta = Threshold = %.4f)", ois_delta
        )
      }
      # .calc_ois() needs a standard deviation as well, and nothing ever
      # derived one: an ois_sd left blank meant the continuous OIS was silently
      # unavailable and Fig 4's large-effect path fell through to "OIS could
      # not be computed -> do not rate down". The pooled within-study SD is the
      # natural default and is already computed a few lines below for the
      # large-effect check, so derive it here rather than skipping the OIS.
      #
      # EXCEPT for the SMD, where delta and sigma must share a scale and the
      # SMD is ALREADY in within-study SD units: n_arm = 2(z_a+z_b)^2 sigma^2 /
      # delta^2 with the standardized delta of 0.20 and a RAW-scale sigma of,
      # say, 8 inflates the OIS by sigma^2 (64x here), which can flip Fig 4's
      # large-effect path from "not_serious" to "serious" via the "< 30% of OIS" rule.
      # For the SMD sigma is 1 by construction.
      if (is.null(ois_sd) && !is.null(ois_delta)) {
        if (identical(sm, "SMD")) {
          ois_sd <- 1
          ois_sd_derived <- TRUE
          ois_sd_source <- paste0(
            "1 by construction: the SMD is expressed in within-study SD ",
            "units, so the threshold above is already standardized and the ",
            "pooled SD must not be applied to it a second time")
          threshold_used_note <- paste0(
            threshold_used_note, " (ois_sd = ", ois_sd_source, ")")
        } else {
          sd_auto <- tryCatch(compute_pooled_sd(meta_obj), error = function(e) NULL)
          if (!is.null(sd_auto) && length(sd_auto) == 1L &&
              is.finite(sd_auto) && sd_auto > 0) {
            ois_sd <- as.numeric(sd_auto)
            ois_sd_derived <- TRUE
            ois_sd_source <- sprintf(
              paste0("%.4f, derived from the pooled within-study SD of the ",
                     "contributing studies -- not supplied by the caller"),
              ois_sd)
            threshold_used_note <- paste0(
              threshold_used_note, " (ois_sd = ", ois_sd_source, ")")
          }
        }
      }
    }
  }

  # OIS auto-calculation (explicit ois_events/ois_n take precedence)
  #
  # `event_basis` is the rare-event correction: the same power calculation,
  # compared against total events rather than total participants. It applies
  # only to the binary branch, because the continuous OIS has no event count
  # to switch to.
  ois_event_basis <- isTRUE(rare_flow) && identical(outcome_type, "relative")
  ois_calc_note <- ""
  if (is.null(ois_events) && is.null(ois_n)) {
    auto_ois <- .calc_ois(outcome_type, ois_alpha, ois_beta,
                          ois_p0, ois_p1, ois_delta, ois_sd,
                          event_basis = ois_event_basis)
    # `.calc_ois()` returns type = "n" for binary outcomes too (Core GRADE 2
    # Fig 4 compares participants, not events).
    if (!is.null(auto_ois)) {
      ois_calc_note <- paste0(auto_ois$formula, threshold_used_note)
      if (auto_ois$type == "events") ois_events <- auto_ois$value
      if (auto_ois$type == "n")      ois_n      <- auto_ois$value
    }
  }

  ois_info <- .compute_ois_pct(meta_obj, ois_events, ois_n)
  ois_pct  <- ois_info$pct
  ois_met  <- if (is.na(ois_pct)) NA else (ois_pct >= 1.0)

  # When the OIS is unavailable, name the input that was missing. Fig 4's
  # large-effect path used to report a bare "OIS could not be computed", which
  # reads as a property of the evidence rather than of the arguments supplied.
  ois_missing_reason <- if (!is.na(ois_pct)) {
    NULL
  } else if (is.null(ois_events) && is.null(ois_n)) {
    miss <- character()
    if (identical(outcome_type, "relative")) {
      if (is.null(ois_p0)) {
        miss <- c(miss, paste0("ois_p0, the control-group event rate (no ",
                               "arm-level event counts to derive it from)"))
      }
      if (is.null(ois_p1)) miss <- c(miss, "ois_p1, the alternative event rate")
    } else {
      if (is.null(ois_delta)) {
        miss <- c(miss, paste0("ois_delta, the smallest difference worth ",
                               "detecting (no Threshold was supplied)"))
      }
      if (is.null(ois_sd)) {
        miss <- c(miss, paste0("ois_sd, the pooled SD (not supplied, and it ",
                               "could not be derived from the study data)"))
      }
    }
    if (!length(miss)) {
      miss <- paste0("the OIS target could not be derived from the inputs ",
                     "supplied (outcome_type = '", outcome_type, "')")
    }
    paste0("missing ", paste(miss, collapse = " and "))
  } else {
    paste0("an OIS target was computed but the observed total is unknown: ",
           "the analysis carries no complete arm-level sample sizes ",
           "(n.e / n.c)")
  }

  # --- Core GRADE 2 Fig 4 -------------------------------------------------
  # `sm` and `te_point` were computed above, before the OIS derivation.
  is_binary <- .is_binary_outcome(meta_obj)
  large     <- .is_implausibly_large(te_point, sm,
                                     sd_pooled = if (is_binary) NULL
                                                 else compute_pooled_sd(meta_obj))
  ci_ratio     <- .ci_ratio(lower, upper, sm)
  ci_ratio_cut <- .ci_ratio_cut(sm)
  n_total      <- .total_n_strict(meta_obj)

  fig4 <- .classify_imprecision(
    crosses_threshold       = crosses_threshold,
    crosses_both_thresholds = crosses_both_thresholds,
    large                   = large,
    is_binary               = is_binary,
    ois_met                 = ois_met,
    ois_pct                 = ois_pct,
    n_total                 = n_total,
    ci_ratio                = ci_ratio,
    ci_ratio_cut            = ci_ratio_cut,
    ois_missing_reason      = ois_missing_reason,
    # These two are rendered into the Fig 4 path string a reviewer reads, so
    # they say "Threshold" -- pmatools' own term for the band, and the word on
    # the Configuration tab that set it. They used to say "MID"; the concept is
    # the same and the vocabulary was not.
    threshold_label         = if (has_threshold) "the +/-Threshold band"
                              else "the null threshold",
    two_level_label         = if (has_threshold) {
                                "TWO thresholds (important benefit and important harm)"
                              } else {
                                paste0("BOTH Thresholds (+/-Threshold) -- the CI is ",
                                       "consistent with benefit and with clearly ",
                                       "important harm (Core GRADE 2, null-threshold ",
                                       "path)")
                              },
    sm                      = sm
  )
  judgment <- fig4$judgment

  # Display CI on natural scale (exp for ratio sm; raw for MD/SMD)
  if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "RoM", "IRR")) {
    ci_disp_lo <- exp(lower)
    ci_disp_hi <- exp(upper)
    null_disp  <- 1
    ci_str <- sprintf("%s [%.2f, %.2f]", sm, ci_disp_lo, ci_disp_hi)
  } else {
    ci_str <- sprintf("%s [%.3f, %.3f]",
                       if (is.null(sm)) "Effect" else sm, lower, upper)
    null_disp <- 0
  }

  ois_detail <- if (is.na(ois_met)) {
    "OIS not specified"
  } else if (ois_met) {
    sprintf("OIS met (%.0f%%; observed %d / target %d %s)",
            100 * ois_pct, ois_info$observed, ois_info$target, ois_info$unit)
  } else if (!is.na(ois_pct) && ois_pct < 0.30) {
    # Fig 4's node reads "N<30% of OIS"; the decision in
    # .classify_imprecision() uses a strict `<`, and this label must agree.
    sprintf("OIS not met; observed %d / target %d %s = %.0f%% (< 30%%)",
            ois_info$observed, ois_info$target, ois_info$unit, 100 * ois_pct)
  } else {
    sprintf("OIS not met (observed %d / target %d %s = %.0f%%)",
            ois_info$observed, ois_info$target, ois_info$unit, 100 * ois_pct)
  }
  # Fig 4 only reaches the OIS approach when the CI does not cross the chosen
  # threshold AND the effect is implausibly large. On every other path the OIS
  # figures are reported for information only and did not drive the judgment.
  ois_str <- if (isTRUE(fig4$ois_used)) {
    ois_detail
  } else {
    paste0("OIS not applied on this Fig 4 path [", ois_detail, "]")
  }

  # The +/-MID zone description is reported whenever a MID exists. On the
  # null-threshold path it is informational for -1/-0 but decisive for -2.
  # The variable keeps the internal name; the string a reviewer reads says
  # "Threshold", like every other user-facing string in this file.
  mid_suffix <- if (has_threshold) {
    ""
  } else {
    " [+/-Threshold zone; rating threshold = null]"
  }
  thresh_str <- if (!has_mid_zone) {
    ""
  } else if (isTRUE(crosses_both_thresholds)) {
    paste0("; crosses BOTH Thresholds", mid_suffix)
  } else if (isTRUE(crosses_one_threshold)) {
    paste0("; crosses one Threshold", mid_suffix)
  } else if (isTRUE(within_thresholds)) {
    paste0("; within Threshold (trivial effect)", mid_suffix)
  } else {
    paste0("; beyond Threshold (definitively important effect)", mid_suffix)
  }

  # Fig 4's "Yes" branch offers TWO reasons to consider rating down two
  # levels, and pmatools automates only the first. Verbatim:
  #   "Consider rating down two levels if:
  #    - CI crosses two thresholds-eg, both important benefit and important
  #      harm
  #    - Most appropriate plain language description of results suggests more
  #      uncertainty-eg, "may" rather than "likely" (assuming no concern
  #      related to other 4 grade domains)"
  # The second condition is a judgment about wording, not a computation, so it
  # is surfaced rather than applied.
  two_level_manual <- if (isTRUE(crosses_threshold) &&
                          !isTRUE(crosses_both_thresholds)) {
    paste0(
      " [Second Fig 4 two-level condition NOT auto-assessed: Core GRADE 2 ",
      "also says to consider rating down two levels when the 'most ",
      "appropriate plain language description of results suggests more ",
      "uncertainty-eg, \"may\" rather than \"likely\"'. Read the plain ",
      "language summary in the SoF table (sof_table(style = 'bmj')) against ",
      "the message you intend to convey, and override with imprecision = ",
      "'serious' + imprecision_rationale if it applies.]"
    )
  } else ""

  # The rating target itself is appended by grade_meta() (it owns the Fig 2
  # derivation); here we only record which threshold Fig 4 was applied to.
  notes <- sprintf(
    "95%% CI %s; null = %g; crosses null = %s%s; %s%s | %s%s",
    ci_str, null_disp,
    if (crosses_null) "YES" else "no",
    thresh_str,
    ois_str,
    if (nchar(ois_calc_note) > 0) paste0(" (", ois_calc_note, ")") else "",
    fig4$path,
    two_level_manual
  )

  # Structured companions to the sentence above. The Fig 4 path and the two
  # yes/no facts exist so a caller can branch on the path WITHOUT re-parsing
  # the prose (which is what "sub('^.*Fig 4 path: ', '', notes)" downstream
  # amounts to); the prose stays authoritative and unchanged.
  facts <- .facts(
    .fact("confidence_interval", "95% confidence interval", ci_str),
    .fact("crosses_null", "Crosses the null", if (crosses_null) "yes" else "no"),
    if (has_mid_zone) {
      .fact("threshold_position", "Position relative to the threshold",
            sub("^; ", "", thresh_str))
    } else NULL,
    .fact(
      "ois", "Optimal information size",
      {
        detail <- if (is.na(ois_pct)) {
          "not specified"
        } else {
          sprintf("observed %d / target %d %s = %.0f%%",
                  ois_info$observed, ois_info$target, ois_info$unit,
                  100 * ois_pct)
        }
        # Fig 4 only consults the OIS on one branch; everywhere else the
        # figures are informational, and ois_str says so in the notes.
        if (isTRUE(fig4$ois_used)) {
          detail
        } else {
          paste0("not applied on this Fig 4 path; ", detail)
        }
      },
      ois_pct
    ),
    # Which denominator the percentage above is a percentage OF. Recorded on
    # every path, not only the rare one: "83% of the OIS" is two different
    # claims on the two bases, and a reader who has to infer which one from the
    # prose is one step away from comparing them.
    # `ois_info$unit` and not `ois_event_basis`: an explicit ois_n override
    # puts the comparison back on participants whatever the data looks like,
    # and the fact has to describe what was actually compared.
    .fact("ois_basis", "Optimal information size basis",
          if (identical(ois_info$unit, "events")) {
            if (isTRUE(ois_event_basis)) {
              paste0("total events (rare-event analysis: an OIS in ",
                     "participants is the wrong denominator when the events ",
                     "are what is scarce)")
            } else {
              "total events (an events target was supplied by the caller)"
            }
          } else if (identical(ois_info$unit, "N")) {
            "total participants (Core GRADE 2 Fig 4: 'N=number of participants')"
          } else {
            "not applicable - no OIS was computed"
          }),
    if (isTRUE(rare_flow)) {
      .fact("rare_flow", "Rare-event analysis",
            paste0("yes - the estimate comes from the rare-event workflow, ",
                   "and the OIS is computed on an event basis. Fig 4's rule ",
                   "is unchanged."))
    } else NULL,
    if (ois_p1_derived) {
      .fact("ois_target_rate", "OIS alternative event rate",
            sprintf("%.4f (%s on a control-group risk of %.4f) -- %s",
                    ois_p1,
                    if (isTRUE(ois_direction$increase)) "an increase"
                    else "a reduction",
                    ois_p0, ois_direction$reason),
            ois_p1)
    } else NULL,
    if (ois_sd_derived) {
      .fact("ois_sd_source", "OIS standard deviation", ois_sd_source, ois_sd)
    } else NULL,
    .fact("fig4_path", "Core GRADE 2 Fig 4 path",
          sub("^Fig 4 path: ", "", fig4$path)),
    .fact("ois_used", "OIS approach applied",
          if (isTRUE(fig4$ois_used)) "yes" else "no"),
    # The same route as `fig4_path`, in the vocabulary the figure understands.
    .flow_path_fact(fig4$flow)
  )

  row <- make_domain_row(
    domain   = "Imprecision",
    judgment = judgment,
    auto     = TRUE,
    notes    = notes,
    facts    = facts
  )
  # The control-arm rate the OIS was powered from, after every default above
  # has run. grade_meta() reads it off the row (an attribute, like
  # assess_rob()'s analysis-set recommendation) and records it, so
  # export_bundle() can pin the number into the bundled analysis.R rather than
  # leaving the re-run to derive one of its own.
  attr(row, "ois_p0") <- ois_p0
  row
}

# --------------------------------------------------------------------------
# Computing the OIS
# --------------------------------------------------------------------------

# Validate the modest relative risk reduction used for the binary OIS
# (Core GRADE 2: "a modest relative risk reduction, typically 20% or 25%").
.check_ois_rrr <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is.numeric(x) ||
      !is.finite(x) || x <= 0 || x >= 1) {
    rlang::abort(paste0(
      "ois_rrr must be a single relative risk reduction in (0, 1), e.g. 0.20 ",
      "for a 20% RRR (Core GRADE 2: 'a modest relative risk reduction, ",
      "typically 20% or 25%')."
    ))
  }
  as.numeric(x)
}

# Which side of the control-group risk does the binary OIS alternative rate
# sit on? Returns list(increase = <logical>, reason = <character>).
#
# WHY THIS IS NOT SIMPLY "REDUCTION"
# ----------------------------------
# `ois_rrr` is Core GRADE 2's "modest relative risk reduction". The paper says
# *reduction* because its worked example has an undesirable event: the good
# intervention makes the event rarer. Two facts decide the sign in general.
#
#   1. `small_values` describes the OUTCOME VALUE, not the event. For a binary
#      outcome the value is the event rate, so small_values = "undesirable"
#      ("a smaller value is worse") means the EVENTS ARE THE GOOD THING --
#      response, remission -- and a beneficial intervention makes them MORE
#      common. small_values = "desirable" (mortality, relapse) is the mirror.
#   2. The pooled effect says which way THIS body of evidence actually moves
#      the event rate. On Fig 4's OIS path the CI is clear of the chosen
#      threshold, so the sign of the pooled effect is unambiguous.
#
# WHICH OF THE TWO DECIDES, AND WHY IT IS THE DIRECTION
# -----------------------------------------------------
# `small_values` decides; the pooled effect is consulted, reported, and does
# NOT override it. The OIS is an a-priori power calculation: it asks how many
# participants a body of evidence would need to detect the smallest effect
# worth not missing. That effect is a property of the QUESTION -- a modest
# benefit -- and the direction of benefit is exactly what `small_values`
# states. Letting the observed estimate pick the side would make the target
# partly data-driven, and, worse, would collapse the distinction this argument
# exists to draw: with a pooled ratio above the null, "desirable" and
# "undesirable" would then produce the identical OIS.
#
# The pooled effect still earns its place in the reason string. When the two
# agree, the sentence reads as one fact ("the intervention increases a
# desirable event"). When they disagree the evidence describes a HARM on this
# outcome, and the note says so, because the reader is then looking at an OIS
# powered for the benefit direction while the estimate runs the other way --
# which is worth seeing, not worth silently papering over.
#
# There is no "direction unknown" branch. Up to 0.5.0 a missing `small_values`
# meant "use the paper's REDUCTION as written, whatever the outcome is", which
# for an outcome whose events are the desirable thing powered the OIS against
# the wrong tail. `small_values` is required as of 0.5.1
# (see .check_small_values()), so the caller has always answered.
#
# Not re-validated here: assess_imprecision() gates it, and the app's read-only
# echo of the RRR direction (step3_grade.R) calls this before the reviewer can
# have answered anything, so an abort would surface as a red box on a tab that
# is merely waiting for an analysis. It reads `$increase` only, and an
# unanswered direction yields FALSE there exactly as it did before 0.5.1.
.ois_target_increase <- function(small_values, te_point) {
  sv <- as.character(small_values)
  # "undesirable" = a smaller outcome value is bad = the events are desirable.
  events_desirable <- identical(sv, "undesirable")
  increase <- events_desirable
  te_known <- !is.null(te_point) && length(te_point) == 1L &&
              !is.na(te_point) && is.finite(te_point) && te_point != 0
  base <- sprintf(
    paste0("small_values = '%s' (events are %s), so a benefit is a modest %s ",
           "in the event rate"),
    sv,
    if (events_desirable) "desirable" else "undesirable",
    if (increase) "increase" else "reduction")
  if (!te_known) {
    return(list(increase = increase,
                reason   = paste0(base, "; no usable pooled effect to compare ",
                                  "it with")))
  }
  observed_increase <- te_point > 0
  list(
    increase = increase,
    reason   = paste0(
      base,
      if (identical(observed_increase, events_desirable)) {
        sprintf("; the pooled effect is %s the null, i.e. the intervention %s",
                if (observed_increase) "above" else "below",
                if (observed_increase) "increases a desirable event"
                else "reduces an undesirable event")
      } else {
        sprintf(paste0("; NOTE the pooled effect runs the other way (%s the ",
                       "null), i.e. this evidence describes a harm -- the OIS ",
                       "is still powered for the benefit direction"),
                if (observed_increase) "above" else "below")
      })
  )
}

.calc_ois <- function(outcome_type, ois_alpha, ois_beta,
                      ois_p0, ois_p1, ois_delta, ois_sd,
                      # Binary only. FALSE returns the participant target Core
                      # GRADE 2 Fig 4 compares against; TRUE returns the event
                      # count the same calculation implies, which is the
                      # denominator that means something when the events are
                      # what is scarce (see the rare-event section of the file
                      # header).
                      event_basis = FALSE) {
  # Defensive: treat NA as NULL (Shiny may pass NA from blank numericInput)
  if (!is.null(ois_p0)    && (length(ois_p0)    == 0 || is.na(ois_p0)))    ois_p0    <- NULL
  if (!is.null(ois_p1)    && (length(ois_p1)    == 0 || is.na(ois_p1)))    ois_p1    <- NULL
  if (!is.null(ois_delta) && (length(ois_delta) == 0 || is.na(ois_delta))) ois_delta <- NULL
  if (!is.null(ois_sd)    && (length(ois_sd)    == 0 || is.na(ois_sd)))    ois_sd    <- NULL

  za <- stats::qnorm(1 - ois_alpha / 2)
  zb <- stats::qnorm(1 - ois_beta)

  if (outcome_type == "relative" && !is.null(ois_p0) && !is.null(ois_p1)) {
    if (ois_p0 <= 0 || ois_p0 >= 1 || ois_p1 <= 0 || ois_p1 >= 1) {
      rlang::abort("ois_p0 and ois_p1 must be probabilities (0 < p < 1).")
    }
    if (ois_p0 == ois_p1) {
      rlang::abort("ois_p0 and ois_p1 must differ for OIS calculation.")
    }
    n_arm        <- (za + zb)^2 * (ois_p0 * (1 - ois_p0) + ois_p1 * (1 - ois_p1)) /
                    (ois_p0 - ois_p1)^2
    p_bar        <- (ois_p0 + ois_p1) / 2
    total_n      <- ceiling(2 * n_arm)
    total_events <- ceiling(2 * n_arm * p_bar)
    # Core GRADE 2 Fig 4 compares PARTICIPANTS with the OIS ("N=number of
    # participants"), so the binary OIS is returned as a target sample size.
    # The implied event count is reported alongside it for information -- and
    # under `event_basis` the two swap roles, because on sparse data the
    # participant count is the figure that misleads.
    if (isTRUE(event_basis)) {
      formula_str <- sprintf(
        paste0("OIS on an EVENT basis (rare-event analysis): p0=%.4f, ",
               "p1=%.4f, alpha=%.2f, beta=%.2f -> target %d events (the same ",
               "power calculation implies %d participants; Core GRADE 2 Fig 4 ",
               "compares participants, which is the wrong denominator when ",
               "the events are what is scarce)"),
        ois_p0, ois_p1, ois_alpha, ois_beta, total_events, total_n
      )
      return(list(type = "events", value = total_events,
                  formula = formula_str))
    }
    formula_str  <- sprintf(
      paste0("OIS: p0=%.3f, p1=%.3f, alpha=%.2f, beta=%.2f -> target N=%d ",
             "participants (implies ~%d events; Core GRADE 2 Fig 4 compares ",
             "participants)"),
      ois_p0, ois_p1, ois_alpha, ois_beta, total_n, total_events
    )
    return(list(type = "n", value = total_n, formula = formula_str))
  }

  if (outcome_type == "absolute" && !is.null(ois_delta) && !is.null(ois_sd)) {
    if (ois_delta == 0) rlang::abort("ois_delta must be non-zero.")
    n_arm   <- 2 * (za + zb)^2 * ois_sd^2 / ois_delta^2
    total_n <- ceiling(2 * n_arm)
    formula_str <- sprintf(
      "OIS: delta=%.3f, sigma=%.3f, alpha=%.2f, beta=%.2f -> target N=%d",
      ois_delta, ois_sd, ois_alpha, ois_beta, total_n
    )
    return(list(type = "n", value = total_n, formula = formula_str))
  }

  NULL
}

# --------------------------------------------------------------------------
# How much of the OIS the evidence actually reaches. Every OIS decision reads
# this one number -- whether the OIS was met, and the "< 30% of OIS" rule that
# escalates to two levels -- so they cannot reach contradictory conclusions
# about the same body of evidence.
#
# Returns list(pct, observed, target, unit). `unit` is "N" (participants,
# Core GRADE 2 Fig 4: "N=number of participants") or "events" when the caller
# supplied `ois_events` explicitly (kept for backward compatibility).
# When neither is computable, all four fields are NA.
# --------------------------------------------------------------------------
.compute_ois_pct <- function(meta_obj, ois_events, ois_n) {
  na_out <- list(pct = NA_real_, observed = NA_integer_,
                 target = NA_integer_, unit = NA_character_)

  if (!is.null(ois_events) && is.finite(ois_events) && ois_events > 0) {
    events_e <- if (!is.null(meta_obj$event.e)) sum(meta_obj$event.e, na.rm = TRUE) else NA
    events_c <- if (!is.null(meta_obj$event.c)) sum(meta_obj$event.c, na.rm = TRUE) else NA
    if (!is.na(events_e) && !is.na(events_c)) {
      observed <- as.integer(events_e + events_c)
      return(list(pct      = observed / ois_events,
                  observed = observed,
                  target   = as.integer(ois_events),
                  unit     = "events"))
    }
  }
  if (!is.null(ois_n) && is.finite(ois_n) && ois_n > 0) {
    n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA
    n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA
    if (!is.na(n_e) && !is.na(n_c)) {
      observed <- as.integer(n_e + n_c)
      return(list(pct      = observed / ois_n,
                  observed = observed,
                  target   = as.integer(ois_n),
                  unit     = "N"))
    }
  }
  na_out
}

# --------------------------------------------------------------------------
# Is the effect "implausibly large"? (Core GRADE 2, body text p6)
#
#   "when the CI does not cross the threshold or thresholds of interest and
#    effects on binary outcomes are implausibly large (certainly relative risk
#    reduction >40%, possibly >30%), Core GRADE users should consider rating
#    down for imprecision if the sample size and number of events across all
#    contributing studies are limited"
#
# Binary: an RRR above 30% is "possibly" implausibly large and above 40% is
#       "certainly", and the trigger onto the OIS path takes the conservative
#       30%. On the ratio scale |log(effect)| > -log(0.70) is an RRR above 30%
#       and |log(effect)| > -log(0.60) is an RRR above 40% (the OR is treated as
#       an approximation to the risk ratio here, as it is throughout Fig 4).
# Continuous: the source defines no "large effect" for continuous outcomes at
#       all, so pmatools adopts Cohen's convention (standardized |d| >= 0.8) as
#       its own operational definition and standardizes an MD by the pooled SD
#       to reach it. This is NOT Core GRADE 2's rule, and the note says so
#       wherever it decides anything.
# --------------------------------------------------------------------------
# Wording for the ratio-scale magnitude 1 - exp(-|log ratio|).
#
# The number is symmetric: RR 0.60 and RR 1.667 both give 40%, and Core GRADE
# 2's ">40% / >30%" cut-offs are applied to it either way. The WORDING is not
# symmetric, and up to v0.5.0 this always said "relative risk reduction", so a
# pooled OR of 2.33 -- an increase -- was reported as a 57% reduction. Above
# the null the same magnitude is the reduction seen with the two arms
# exchanged, and the label says so rather than renaming 57% as an increase
# (as an increase, RR 1.667 is +67%, a different number).
.rrr_direction_label <- function(te, rrr) {
  if (!is.na(te) && te > 0) {
    sprintf(paste0("relative risk increase, equivalent to a %.0f%% reduction ",
                   "with the arms exchanged"), 100 * rrr)
  } else {
    sprintf("relative risk reduction %.0f%%", 100 * rrr)
  }
}

.is_implausibly_large <- function(te, sm, sd_pooled = NULL) {
  none <- list(large = FALSE, level = NA_character_,
               note = "effect moderate")
  if (is.null(te) || length(te) != 1L || is.na(te) || !is.finite(te)) {
    return(list(large = FALSE, level = NA_character_,
                note = "effect magnitude not available; treated as moderate"))
  }

  ratio_sm <- c("OR", "RR", "HR", "RoM", "IRR")
  if (!is.null(sm) && sm %in% ratio_sm) {
    rrr <- 1 - exp(-abs(te))   # |log ratio| -> relative risk reduction
    lab <- .rrr_direction_label(te, rrr)
    if (rrr > 0.40) {
      return(list(large = TRUE, level = "certain",
                  note = sprintf(
                    "effect implausibly large (%s > 40%%)", lab)))
    }
    if (rrr > 0.30) {
      return(list(large = TRUE, level = "possible",
                  note = sprintf(
                    "effect possibly implausibly large (%s > 30%%)", lab)))
    }
    return(list(large = FALSE, level = NA_character_,
                note = sprintf("effect moderate (%s <= 30%%)", lab)))
  }

  # Continuous outcomes: standardize, then apply Cohen's large-effect
  # convention. Flagged in the note because Core GRADE 2 operationalizes
  # "large effect" only for binary outcomes.
  std <- if (identical(sm, "SMD")) {
    abs(te)
  } else if (!is.null(sd_pooled) && is.finite(sd_pooled) && sd_pooled > 0) {
    abs(te) / sd_pooled
  } else {
    NA_real_
  }
  if (is.na(std)) {
    return(list(large = FALSE, level = NA_character_,
                note = paste0("effect magnitude could not be standardized ",
                              "(no pooled SD); treated as moderate")))
  }
  if (std >= 0.8) {
    return(list(large = TRUE, level = "possible",
                note = sprintf(paste0(
                  "effect large (standardized effect %.2f >= 0.80; Cohen's ",
                  "convention, not specified by Core GRADE 2)"), std)))
  }
  list(large = FALSE, level = NA_character_,
       note = sprintf(paste0(
         "effect moderate (standardized effect %.2f < 0.80; Cohen's ",
         "convention, not specified by Core GRADE 2)"), std))
}

# --------------------------------------------------------------------------
# The CI ratio (Core GRADE 2 Fig 4 caption)
#   "The relative risk CI ratio represents the upper boundary divided by lower
#    boundary of CI of relative risk, and the odds ratio CI ratio represents
#    the upper boundary divided by lower boundary of CI of odds ratio."
# TE is on the log scale, so exp(upper) / exp(lower) = exp(upper - lower).
# The quantity is undefined for anything but a ratio measure -- there is no
# "upper divided by lower" to read off a mean difference -- so those return NA
# and the CI-ratio branch of Fig 4 never fires for them.
# --------------------------------------------------------------------------
.ci_ratio <- function(lower, upper, sm) {
  if (is.null(sm) || !sm %in% c("OR", "RR", "HR", "RoM", "IRR")) return(NA_real_)
  if (!is.finite(lower) || !is.finite(upper)) return(NA_real_)
  exp(upper - lower)
}

# Fig 4: relative risk CI ratio >= 3, odds ratio CI ratio >= 2.5.
#
# CAVEAT: Core GRADE 2 names ONLY those two measures. Applying the risk-ratio
# value of 3 to HR / IRR / RoM is a pmatools extrapolation with no support in
# the source; it rests on nothing more than these being ratio measures read on
# a comparable scale. The domain notes state it whenever the cut-off decides a
# two-level downgrade.
.ci_ratio_cut <- function(sm) {
  if (is.null(sm)) return(NA_real_)
  switch(sm, "OR" = 2.5, "RR" = 3, "HR" = 3, "IRR" = 3, "RoM" = 3, NA_real_)
}

# Which effect measures get the CI-ratio cut-off straight from Core GRADE 2.
.CI_RATIO_SOURCED_SM <- c("OR", "RR")

# --------------------------------------------------------------------------
# Flowchart node vocabulary (inst/figures/impre.svg)
#
# See the note on .ROB_FIG2_NODE_IDS in domain_rob.R. The three OIS outcomes
# are drawn as three edges out of one OIS node rather than one edge per
# sub-rule: the sub-rule that fired is already recorded verbatim by the
# "fig4_path" fact, and splitting the node six ways made the picture harder
# to read than the sentence it was meant to replace.
.IMPRE_FIG4_NODE_IDS <- c(
  "pma-impre-node-crosses",
  "pma-impre-edge-crosses-yes",
  "pma-impre-node-both",
  "pma-impre-edge-both-no",
  "pma-impre-leaf-down1",
  "pma-impre-edge-both-yes",
  "pma-impre-leaf-down2-both",
  "pma-impre-edge-crosses-no",
  "pma-impre-node-large",
  "pma-impre-edge-large-no",
  "pma-impre-leaf-nodown-moderate",
  "pma-impre-edge-large-yes",
  "pma-impre-node-ois",
  "pma-impre-edge-ois-nodown",
  "pma-impre-leaf-nodown-ois",
  "pma-impre-edge-ois-down1",
  "pma-impre-leaf-down1-ois",
  "pma-impre-edge-ois-down2",
  "pma-impre-leaf-down2-ois"
)

# Shared prefixes, so a change to the shape of the figure is a change in one
# place rather than in each of the nine returns of .classify_imprecision().
.IMPRE_FLOW_CROSSES <- c("pma-impre-node-crosses",
                         "pma-impre-edge-crosses-yes",
                         "pma-impre-node-both")
.IMPRE_FLOW_LARGE   <- c("pma-impre-node-crosses",
                         "pma-impre-edge-crosses-no",
                         "pma-impre-node-large")
.IMPRE_FLOW_OIS     <- c(.IMPRE_FLOW_LARGE,
                         "pma-impre-edge-large-yes",
                         "pma-impre-node-ois")

# Is this an event-based (binary) outcome? Event counts from a metabin fit, or a
# summary measure that only makes sense for events, are each enough to say yes.
# This is what Fig 4's continuous-versus-binary fork reads.
#
# grade_meta()'s `outcome_type` is deliberately NOT consulted: it says
# "relative" or "absolute", which is a statement about the OIS formula, not
# about whether the outcome counts events. Reading it here would fold two
# different distinctions into one and send, say, a risk difference down the
# continuous branch.
#
# NOTE — RoM. RoM is deliberately NOT listed here: a ratio of means is a
# continuous-outcome summary, so Fig 4's continuous branch (N >= 800 rule of
# thumb, N < 30% of OIS) is the right one and the CI-ratio branch never runs
# for it. .is_implausibly_large() nevertheless routes RoM through the BINARY
# "relative risk reduction > 30% / > 40%" rule, because 1 - exp(-|log RoM|) is
# a usable magnitude proxy on a ratio scale and Core GRADE 2 offers no
# "large effect" definition for continuous outcomes at all. The two functions
# therefore classify RoM differently on purpose:
#   .is_binary_outcome()      RoM -> continuous  (drives the Fig 4 branch)
#   .is_implausibly_large()   RoM -> ratio rule  (drives the magnitude label)
# Both paths label their reasoning in the notes; neither is claimed to be
# Core GRADE 2's.
.is_binary_outcome <- function(meta_obj) {
  if (!is.null(meta_obj$event.e) && length(meta_obj$event.e) > 0) return(TRUE)
  sm <- meta_obj$sm
  if (!is.null(sm) && sm %in% c("OR", "RR", "HR", "IRR", "RD", "ARD")) return(TRUE)
  FALSE
}

# Total sample size, for the continuous "N >= OIS (or 800)" test.
#
# Strict on purpose. The 800 rule of thumb is really "400 patients per group",
# so it only means anything when both arms were actually counted. A one-arm meta
# -- metaprop, metamean and the like, which carry meta_obj$n but no n.e / n.c --
# returns NA here rather than falling back to meta_obj$n, because a single-arm
# total of 800 is not the quantity the rule of thumb is about and comparing it
# would quietly pass analyses the rule was meant to catch.
#
# .compute_ois_pct() in this file refuses the same inputs for the same reason,
# so the two stay in step: no OIS percentage, and no 800 test either.
#
# The permissive version -- total participants for the SoF table's N column,
# where the number is being displayed rather than decided on -- is .total_n() in
# R/sof_table.R. Do not swap one for the other.
.total_n_strict <- function(meta_obj) {
  n_e <- if (!is.null(meta_obj$n.e)) sum(meta_obj$n.e, na.rm = TRUE) else NA_real_
  n_c <- if (!is.null(meta_obj$n.c)) sum(meta_obj$n.c, na.rm = TRUE) else NA_real_
  if (is.na(n_e) || is.na(n_c)) return(NA_real_)
  n_e + n_c
}

# --------------------------------------------------------------------------
# Turning the inputs into a verdict (Core GRADE 2 Fig 4)
#
# Returns list(judgment, path, ois_used, flow)
#   path     - the Fig 4 route actually taken, in words. Recorded in the notes,
#              which is what makes the judgment auditable rather than asserted
#   ois_used - whether the OIS branch was really reached. The notes read
#              differently when it was not, because the OIS figures are then
#              reported for information and did not decide anything
#   flow     — the same route as inst/figures/impre.svg node ids, so the
#              caller can record it as the "flow_path" fact without parsing
#              `path` back out of the prose (see .IMPRE_FIG4_NODE_IDS)
# --------------------------------------------------------------------------
.classify_imprecision <- function(crosses_threshold,
                                  crosses_both_thresholds,
                                  large,
                                  is_binary,
                                  ois_met,
                                  ois_pct,
                                  n_total,
                                  ci_ratio,
                                  ci_ratio_cut,
                                  ois_missing_reason = NULL,
                                  threshold_label = "the Threshold",
                                  two_level_label =
                                    "TWO thresholds (important benefit and important harm)",
                                  sm = NULL) {
  ci_ratio_src_note <- if (!is.null(sm) && !sm %in% .CI_RATIO_SOURCED_SM) {
    sprintf(paste0(" [Core GRADE 2 Fig 4 names CI-ratio cut-offs only for ",
                   "relative risk (3) and odds ratio (2.5); applying 3 to %s ",
                   "is a pmatools extrapolation]"), sm)
  } else ""
  out <- function(judgment, path, ois_used = FALSE, flow = character(0)) {
    list(judgment = judgment,
         path     = paste0("Fig 4 path: ", path),
         ois_used = ois_used,
         flow     = flow)
  }

  # --- Yes branch: CI crosses the chosen threshold -------------------------
  if (isTRUE(crosses_threshold)) {
    if (isTRUE(crosses_both_thresholds)) {
      return(out("very_serious", sprintf(
        "CI crosses %s -> rate down; CI crosses %s -> rate down two levels",
        threshold_label, two_level_label),
        flow = c(.IMPRE_FLOW_CROSSES, "pma-impre-edge-both-yes",
                 "pma-impre-leaf-down2-both")))
    }
    return(out("serious", sprintf(paste0(
      "CI crosses %s -> rate down one level (sample size not considered on ",
      "this path)"), threshold_label),
      flow = c(.IMPRE_FLOW_CROSSES, "pma-impre-edge-both-no",
               "pma-impre-leaf-down1")))
  }

  # --- No branch: CI does not cross the threshold --------------------------
  if (!isTRUE(large$large)) {
    return(out("not_serious", sprintf(paste0(
      "CI does not cross %s -> %s -> do not rate down (OIS not applied)"),
      threshold_label, large$note),
      flow = c(.IMPRE_FLOW_LARGE, "pma-impre-edge-large-no",
               "pma-impre-leaf-nodown-moderate")))
  }

  # Large effect -> OIS approach
  prefix <- sprintf("CI does not cross %s -> %s -> OIS approach",
                    threshold_label, large$note)

  if (is_binary) {
    if (!is.na(ci_ratio) && !is.na(ci_ratio_cut) && ci_ratio >= ci_ratio_cut) {
      return(out("very_serious", sprintf(paste0(
        "%s (binary): CI ratio %.2f >= %.1f -> consider rating down two ",
        "levels%s"), prefix, ci_ratio, ci_ratio_cut, ci_ratio_src_note),
        ois_used = TRUE,
        flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-down2",
                 "pma-impre-leaf-down2-ois")))
    }
  } else {
    # Continuous rule of thumb: 400 patients per group (total sample size 800).
    if (!is.na(n_total) && n_total >= 800) {
      return(out("not_serious", sprintf(paste0(
        "%s (continuous): total N = %.0f >= 800 (rule of thumb) -> do not ",
        "rate down"), prefix, n_total), ois_used = TRUE,
        flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-nodown",
                 "pma-impre-leaf-nodown-ois")))
    }
  }

  if (is.na(ois_met)) {
    return(out("not_serious", sprintf(
      "%s: OIS could not be computed (%s) -> do not rate down",
      prefix,
      if (is.null(ois_missing_reason)) "inputs unavailable"
      else ois_missing_reason),
      ois_used = TRUE,
      flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-nodown",
               "pma-impre-leaf-nodown-ois")))
  }
  if (isTRUE(ois_met)) {
    return(out("not_serious", sprintf("%s: N >= OIS -> do not rate down", prefix),
               ois_used = TRUE,
               flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-nodown",
                        "pma-impre-leaf-nodown-ois")))
  }
  # N < OIS
  if (!is_binary && !is.na(ois_pct) && ois_pct < 0.30) {
    return(out("very_serious", sprintf(paste0(
      "%s (continuous): N < 30%% of OIS -> consider rating down two levels"),
      prefix), ois_used = TRUE,
      flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-down2",
               "pma-impre-leaf-down2-ois")))
  }
  out("serious", sprintf("%s: N < OIS -> rate down one level", prefix),
      ois_used = TRUE,
      flow = c(.IMPRE_FLOW_OIS, "pma-impre-edge-ois-down1",
               "pma-impre-leaf-down1-ois"))
}
