# pubias_status.R - the status dots on the three publication-bias reference
# tabs.
#
# Purpose : say, in one glyph per tab, what that tab's diagnostic found, so a
#           reviewer who never opens the tab still learns it disagreed with
#           the answer they gave the wizard.
# Inputs  : an Egger result (funnel); the original and trim-and-fill adjusted
#           pooled effects plus the outcome's scale (trim-and-fill). The
#           RoB-ME dot is R/pubias_missing.R.
# Outputs : a list(state =, reason =) per dot. `state` is one of
#           PMA_PUBIAS_DOT_STATES; `reason` is the tooltip.
# Depends : PMA_ROB_INFLATION_THRESHOLD and .assess_bias_direction()
#           (R/domain_rob.R).

# --------------------------------------------------------------------------
# The dot rates NOTHING, and must not start to.
#
# Core GRADE 4 Fig 5 has no node for a funnel plot's p value, for
# trim-and-fill, or for missing results, so none of these three can reach
# assess_pubias() or grade_meta() without inventing a rating rule the source
# does not have. What they are for is the opposite problem: the three
# reference tabs sit behind a tabset, one at a time, and a reviewer who never
# clicks past the first one never learns that the second disagreed with the
# answer they just gave. The dot is a nudge toward looking.
#
# Four states, and the fourth is deliberately not a colour. Three colours
# alone make "not computed" read as "nothing found", which is backwards for
# every tab here: each one declines to compute on exactly the sparse data
# where reporting bias is most likely. "unknown" therefore carries its own
# glyph and states the reason as its tooltip.
#
# The three colours mean the same thing everywhere they appear:
#   green   the diagnostic looked and found nothing to worry about
#   amber   it found something a reviewer should read before moving on
#   red     it found the strongest signal this tab can produce
# --------------------------------------------------------------------------

PMA_PUBIAS_DOT_STATES <- c("green", "amber", "red", "unknown")

# Build a dot. The reason is mandatory: an "unknown" with no tooltip is the
# failure mode this whole vocabulary exists to prevent, and a colour with no
# tooltip is a coloured shape nobody can act on.
.pubias_dot <- function(state, reason) {
  state <- match.arg(state, PMA_PUBIAS_DOT_STATES)
  list(state = state, reason = as.character(reason)[1])
}

# Convenience for the four call sites that all say "we did not compute it".
.pubias_dot_unknown <- function(reason) .pubias_dot("unknown", reason)

# --------------------------------------------------------------------------
# Funnel: Egger's p, in three bands
# --------------------------------------------------------------------------
# The bands are the app's existing Egger alpha (0.05) plus a stronger one at
# 0.01. `alpha` is a formal rather than a literal because the app already
# names that number (STEP3_EGGER_ALPHA) for the sentence it prints under the
# funnel, and a second literal here is how a displayed threshold drifts away
# from the tested one. The 0.01 band exists only on the dot, so it is named
# here and nowhere else.
#
# Two separate reasons force "unknown", and the rare-event one is checked
# FIRST even though the k gate is the older of the two: below k = 10 Egger is
# merely underpowered, whereas on sparse binary data it loses validity
# outright, and the second is the more important thing to tell a reviewer who
# is in both situations at once. Letting an invalid p value paint a red dot
# is the specific outcome this ordering avoids.
PMA_PUBIAS_EGGER_STRONG_ALPHA <- 0.01

.pubias_funnel_dot <- function(p, feasible = TRUE, k_ok = TRUE,
                               rare_flow = FALSE,
                               alpha = 0.05,
                               strong_alpha = PMA_PUBIAS_EGGER_STRONG_ALPHA) {
  if (isTRUE(rare_flow)) {
    return(.pubias_dot_unknown(paste0(
      "Not computed: Egger's test loses validity on sparse (rare-event) ",
      "binary data, so its p value is not read here.")))
  }
  if (!isTRUE(k_ok)) {
    return(.pubias_dot_unknown(paste0(
      "Not computed: Egger's test is underpowered below 10 studies and the ",
      "app does not run it.")))
  }
  if (!isTRUE(feasible) || length(p) != 1L || !is.finite(p)) {
    return(.pubias_dot_unknown(
      "Not computed: Egger's test could not be computed for this analysis."))
  }

  if (p < strong_alpha) {
    return(.pubias_dot("red", sprintf(
      paste0("Egger's test p = %.3f, below %.2f: strong evidence of funnel ",
             "asymmetry. Reference only - it rates nothing."),
      p, strong_alpha)))
  }
  if (p < alpha) {
    return(.pubias_dot("amber", sprintf(
      paste0("Egger's test p = %.3f, below %.2f: evidence of funnel ",
             "asymmetry. Reference only - it rates nothing."),
      p, alpha)))
  }
  .pubias_dot("green", sprintf(
    paste0("Egger's test p = %.3f, at or above %.2f: no strong evidence of ",
           "funnel asymmetry. Reference only - it rates nothing."),
    p, alpha))
}

# --------------------------------------------------------------------------
# Trim-and-fill: which scale the comparison runs on
# --------------------------------------------------------------------------
# The direction-of-bias rules measure magnitude as |TE| and zones as +/-T,
# and both of those mean "distance from the null" only on a scale whose null
# is zero. Read off a raw summary measure that breaks for every ratio: an
# odds ratio's null is 1, so OR = 2.0 and OR = 0.5 - equidistant from the
# null in fact - come out four-fold apart.
#
# So the comparison runs on whichever scale puts the null at zero:
#
#   binary (OR, RR, and any measure on a metabin)
#       absolute risk difference per 1,000 at the outcome's baseline risk p0.
#       Null is 0, and it is the scale the reviewer's threshold is already
#       stated in on the Configuration tab, so the dot and the number they
#       read are on one scale rather than two.
#   continuous difference measures (MD, SMD)
#       the internal scale unchanged - already a difference with the null
#       at 0.
#   RoM
#       the internal (log) scale. A ratio on a continuous outcome has no
#       event rate to convert to; log puts its null at 0.
#
# Where no baseline risk is available - a metabin whose control arm gives no
# usable p0, or a reviewer who has cleared it - the dot is "unknown" rather
# than falling back to the internal scale. A silent scale change is what this
# whole block exists to prevent.
#
# `p1_from_ratio` is INJECTED rather than reimplemented. The event-rate map
# (odds for OR, risk for everything else) is already written, tested and used
# by the app's Configuration tab as step3_p1_from_ratio(); a second copy here
# is exactly the pair of implementations that drift. The package therefore
# owns the DECISION - which scale, and when the answer is "unknown" - and the
# caller supplies the one-line map it already has. With no map supplied a
# binary outcome is "unknown", never silently converted by a guess.
.PUBIAS_LOG_SCALE_SM   <- c("OR", "RR", "HR", "IRR", "RoM")
.PUBIAS_ABSOLUTE_SM    <- c("RD", "ARD")

# Returns list(ok =, reason =, te_original =, te_adjusted =, threshold =,
# scale =, sm =), where `sm` is the measure label to hand
# .assess_bias_direction() - NULL once the values are no longer on the
# internal scale, so it cannot exponentiate a risk difference for display.
.pubias_trimfill_scale <- function(te_original, te_adjusted,
                                   sm = NULL, binary = FALSE,
                                   baseline_risk = NULL,
                                   threshold_abs1000 = NULL,
                                   threshold_internal = NULL,
                                   p1_from_ratio = NULL) {
  .fail <- function(reason) {
    list(ok = FALSE, reason = reason, te_original = NA_real_,
         te_adjusted = NA_real_, threshold = NULL, scale = NA_character_,
         sm = NULL)
  }
  .num <- function(x) {
    if (is.null(x) || length(x) != 1L) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }

  te_original <- .num(te_original)
  te_adjusted <- .num(te_adjusted)
  if (!is.finite(te_original) || !is.finite(te_adjusted)) {
    return(.fail(paste0(
      "Not computed: the original and trim-and-fill adjusted pooled effects ",
      "are not both available.")))
  }

  if (!isTRUE(binary)) {
    # MD, SMD and RoM alike: the internal scale already has its null at zero
    # (log, for RoM), so nothing is converted and the threshold is the one
    # the domains are judged against.
    return(list(ok = TRUE, reason = NA_character_,
                te_original = te_original, te_adjusted = te_adjusted,
                threshold = threshold_internal, scale = "internal", sm = sm))
  }

  p0 <- .num(baseline_risk)
  if (!is.finite(p0) || p0 <= 0 || p0 >= 1) {
    return(.fail(paste0(
      "Not computed: this binary outcome has no usable baseline (control-",
      "group) risk, and the comparison is not run on the internal scale, ",
      "whose null is not zero for a ratio measure.")))
  }

  sm_chr <- if (is.null(sm)) "" else as.character(sm)[1]
  to_ard1000 <- if (sm_chr %in% .PUBIAS_ABSOLUTE_SM) {
    # Already an absolute risk difference; per 1,000 is a change of unit and
    # needs no baseline risk at all. p0 is still required above, because the
    # threshold it is compared against is stated at that baseline.
    function(te) 1000 * te
  } else if (sm_chr %in% .PUBIAS_LOG_SCALE_SM) {
    if (!is.function(p1_from_ratio)) {
      return(.fail(paste0(
        "Not computed: no event-rate conversion was supplied for this ratio ",
        "measure, and its internal scale does not put the null at zero.")))
    }
    function(te) {
      p1 <- p1_from_ratio(sm_chr, p0, exp(te))
      if (length(p1) != 1L) return(NA_real_)
      1000 * (as.numeric(p1) - p0)
    }
  } else {
    return(.fail(sprintf(paste0(
      "Not computed: '%s' is not a measure the absolute-risk comparison ",
      "knows how to convert."), sm_chr)))
  }

  ard_original <- suppressWarnings(to_ard1000(te_original))
  ard_adjusted <- suppressWarnings(to_ard1000(te_adjusted))
  # An extreme ratio can put the implied event rate outside (0, 1) and the
  # difference is then meaningless rather than merely large, so a non-finite
  # result is reported as "not computed" instead of being clamped into a
  # number the reviewer would read as real.
  if (!is.finite(ard_original) || !is.finite(ard_adjusted)) {
    return(.fail(paste0(
      "Not computed: the pooled effects do not convert to an event rate at ",
      "this baseline risk.")))
  }

  threshold <- if (is.null(threshold_abs1000)) NULL else .num(threshold_abs1000)
  if (!is.null(threshold) && (!is.finite(threshold) || threshold <= 0)) {
    threshold <- NULL
  }

  list(ok = TRUE, reason = NA_character_,
       te_original = ard_original, te_adjusted = ard_adjusted,
       threshold = threshold, scale = "absolute1000",
       # NULL, not sm: the values are risk differences per 1,000 now, and
       # .assess_bias_direction() would exponentiate them for display if it
       # were still told the measure was an odds ratio.
       sm = NULL)
}

# --------------------------------------------------------------------------
# Trim-and-fill: the dot
# --------------------------------------------------------------------------
# The verdict is the risk-of-bias direction check, read off the pair
# (original pooled effect, trim-and-fill adjusted effect) instead of the pair
# (whole body, low risk-of-bias subset). It is the same question - "is the
# estimate that may be biased more than a fifth further in the direction that
# flatters the intervention, and does it sit in a different zone?" - so it is
# the same five rules, and PMA_ROB_INFLATION_THRESHOLD stays shared rather
# than being restated here.
#
# HOW the pair reaches .assess_bias_direction(), which takes study-level
# vectors rather than a pair: the adjusted effect is passed as a
# single-element low-risk-of-bias pool with unit standard error. That
# function's te_low is an inverse-variance weighted mean of the low subset,
# and the weighted mean of one value is that value, so te_low IS the adjusted
# effect exactly. se_all is a formal it never reads. This is a call, not a
# copy: the five rules stay in one place, and a change to them moves this dot
# with them, which is the point.
#
# Judgment -> dot, per the tab's own vocabulary: not_serious is green, one
# level is amber, two levels is red. `rule = NA` is that function's own "not
# assessable" signal and becomes "unknown" rather than borrowing the
# "serious" it returns alongside - a dot that cannot be computed must not
# render as a colour.
.PUBIAS_TRIMFILL_DOT_BY_JUDGMENT <- c(
  not_serious  = "green",
  serious      = "amber",
  very_serious = "red"
)

# The three sentences, next to the arithmetic that produces them rather than
# in the app, for the same reason .pubias_trimfill_line() is here: a wording
# that claims the diagnostic rated something is a bug a test should catch.
.PUBIAS_TRIMFILL_DOT_WORDING <- c(
  green = paste0(
    "the adjustment leaves the estimate in the same zone and does not ",
    "reveal a bias-favouring exaggeration."),
  amber = paste0(
    "the adjustment moves the estimate enough to matter - either past the ",
    "exaggeration threshold in the favourable direction, or into a ",
    "different zone."),
  red = paste0(
    "the adjustment moves the estimate across the null, from one side of ",
    "the decision threshold to the other.")
)

.pubias_trimfill_dot <- function(te_original, te_adjusted,
                                 small_values = NULL,
                                 k_ok = TRUE,
                                 sm = NULL, binary = FALSE,
                                 baseline_risk = NULL,
                                 threshold_abs1000 = NULL,
                                 threshold_internal = NULL,
                                 p1_from_ratio = NULL,
                                 inflation_threshold =
                                   PMA_ROB_INFLATION_THRESHOLD) {
  if (!isTRUE(k_ok)) {
    return(.pubias_dot_unknown(paste0(
      "Not computed: the trim-and-fill panel needs at least 10 studies with ",
      "a usable effect.")))
  }
  # .assess_bias_direction() reads small_values without re-checking it,
  # because assess_rob() gates it on the way in. Nothing gates it here, so
  # the gate is here instead: without a direction there is no "flattering"
  # side and rules 2 and 3 cannot be told apart.
  if (!identical(small_values, "desirable") &&
      !identical(small_values, "undesirable")) {
    return(.pubias_dot_unknown(paste0(
      "Not computed: the outcome's direction (whether smaller values are ",
      "desirable) is not set, so no direction of bias can be read.")))
  }

  scaled <- .pubias_trimfill_scale(
    te_original = te_original, te_adjusted = te_adjusted,
    sm = sm, binary = binary, baseline_risk = baseline_risk,
    threshold_abs1000 = threshold_abs1000,
    threshold_internal = threshold_internal,
    p1_from_ratio = p1_from_ratio)
  if (!isTRUE(scaled$ok)) return(.pubias_dot_unknown(scaled$reason))

  dir <- .assess_bias_direction(
    te_all   = scaled$te_original,
    se_all   = NA_real_,
    te_vec   = scaled$te_adjusted,
    se_vec   = 1,
    low_idx  = TRUE,
    small_values        = small_values,
    inflation_threshold = inflation_threshold,
    sm                  = scaled$sm,
    threshold_internal  = scaled$threshold)

  if (is.na(dir$rule)) {
    return(.pubias_dot_unknown(paste0(
      "Not computed: the direction-of-bias check could not be run on this ",
      "pair of pooled effects.")))
  }

  # A judgment the map does not know is a new .assess_bias_direction() rule
  # that nobody has decided a colour for. Rendering it as one would be a
  # guess; "not computed" is the honest answer until someone extends the map.
  if (!dir$judgment %in% names(.PUBIAS_TRIMFILL_DOT_BY_JUDGMENT)) {
    return(.pubias_dot_unknown(sprintf(paste0(
      "Not computed: the direction-of-bias check returned '%s', which this ",
      "tab has no reading for."), dir$judgment)))
  }
  state <- .PUBIAS_TRIMFILL_DOT_BY_JUDGMENT[[dir$judgment]]
  scale_note <- if (identical(scaled$scale, "absolute1000")) {
    " Compared on the absolute risk difference per 1,000."
  } else {
    ""
  }
  .pubias_dot(state, paste0(
    "Trim-and-fill vs the original pooled effect: ",
    .PUBIAS_TRIMFILL_DOT_WORDING[[state]], scale_note,
    " Reference only - it rates nothing."))
}
