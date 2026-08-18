# pubias_missing.R - the tipping point behind the Missing results (RoB-ME)
# status dot.
#
# Purpose : answer one question - how far from the observed pooled effect
#           would the missing studies have to lie before the conclusion
#           changes? Far means the missing evidence cannot overturn the
#           result; near means it can.
# Inputs  : the reviewer's missing-results table (results_known and n, one
#           row per missing study) and the fitted analysis's pooled effect,
#           standard error, tau^2, confidence interval and prediction
#           interval.
# Outputs : a list carrying the ordered step that decided the answer, the
#           tipping point itself, and the dot (state + tooltip). Nothing
#           here reaches a GRADE judgment.
# Depends : PMA_PUBIAS_DOT_STATES / .pubias_dot() (R/pubias_status.R), stats.

# --------------------------------------------------------------------------
# Nothing here rates the domain. RoB-ME is not part of the Core GRADE
# algorithm and the tab it sits on already says so. What the dot adds is a
# reason to open that tab: a reviewer who never does never learns that the
# studies they listed as missing are enough to overturn the result.
#
# THE MODEL. Assume the m missing studies share one effect delta, and hold
# tau^2 at its observed value. Placing every missing study at a single delta
# would otherwise shrink tau^2 artificially, which flatters the result - the
# missing evidence would look more consistent with the observed evidence than
# anything could justify.
#
#   W_obs = 1 / seTE_pooled^2
#   w_j   = 1 / (se_j^2 + tau2)              W_miss = sum(w_j)
#   TE_new(delta) = (W_obs*TE_obs + W_miss*delta) / (W_obs + W_miss)
#   se_new        = 1 / sqrt(W_obs + W_miss)        -- independent of delta
#
# se_new not depending on delta is what makes this cheap enough to redraw on
# every edit of the table: the interval's width is constant, TE_new is linear
# in delta, and the crossing solves directly as
#   delta* = (W_tot*(T +/- 1.96*se_new) - W_obs*TE_obs) / W_miss
# No root-finding.
#
# THE CONCLUSION, precisely. "The conclusion" is which side of the decision
# threshold the pooled 95% interval lies on:
#
#   above  : TE - 1.96*se >  +T        the effect exceeds the threshold
#   below  : TE + 1.96*se <  -T        it exceeds it the other way
#   spans  : neither                   nothing is established
#
# with T the Core GRADE threshold already chosen for this outcome (the MID,
# or 0 when the rating target is the null). No second threshold is
# introduced. The observed conclusion is read with 1.96*seTE_pooled rather
# than off the reported confidence limits, so that the before and after are
# computed the same way: a Hartung-Knapp interval, say, is wider than
# 1.96*se, and comparing one against the other would report a change that
# came from the quantile rather than from the missing studies. The REPORTED
# limits are used at step 6, where the spec asks for the pooled 95% CI as a
# region delta* is judged against rather than as a decision rule.
#
# ORDERED DECISION, cheapest and most decisive first:
#
#   1. m = 0                                          -> green
#   2. no prediction interval, or se imputation
#      impossible                                     -> unknown
#   3. delta = TE_obs already changes the conclusion   -> red
#   4. no delta changes the conclusion                 -> green
#   5. direction gate: delta* outside the suspected
#      region                                         -> green
#   6. magnitude: delta* against the intervals        -> red / amber / green
#
# STEPS 3 AND 4 MUST PRECEDE STEP 6. Adding studies shrinks se_new, so a body
# of evidence can cross the threshold on precision alone, with the missing
# studies reporting exactly what the observed ones did. Step 6 asked on its
# own would find delta* far from everything and call that case reassuring,
# when it is the opposite: the conclusion is one unpublished trial away from
# changing and the trial does not even have to disagree.
# --------------------------------------------------------------------------

# The normal quantile the whole file works in. Named because it appears in
# the observed conclusion, in the new one, and in the tipping point, and
# three literals is how two of them come to disagree.
.PUBIAS_MISSING_Z95 <- 1.959964

# Below three studies meta computes no usable prediction interval, and with
# tau^2 at zero the interval it does compute is not a statement about
# between-study spread. Step 6 is anchored on that interval, so both cases
# are "not computed" rather than a colour.
.PUBIAS_MISSING_MIN_K <- 3L

# --------------------------------------------------------------------------
# Imputing a missing study's standard error
# --------------------------------------------------------------------------
# Borrow from the observed studies rather than assuming an SD, a control-
# group risk or an allocation ratio - none of which exist for every effect
# measure the app pools.
#
#   c_med = median(seTE_i * sqrt(n_i))   over observed studies
#   se_j  = c_med / sqrt(n_j)            n_j blank -> median(seTE_i)
#
# se proportional to 1/sqrt(n) holds for SMD, MD, log OR and log RR alike, so
# one formula covers every measure. This is the reviewer's "assume the same
# SD" generalised to measures that have no SD.
#
# Returns a numeric vector the length of `n_missing`, or NULL when even the
# fallback is unavailable - which is step 2's "se imputation impossible".
.pubias_missing_impute_se <- function(n_missing, se_studies, n_studies) {
  se_studies <- suppressWarnings(as.numeric(se_studies))
  n_studies  <- suppressWarnings(as.numeric(n_studies))
  n_missing  <- suppressWarnings(as.numeric(n_missing))

  usable_se <- is.finite(se_studies) & se_studies > 0
  if (!any(usable_se)) return(NULL)

  se_median <- stats::median(se_studies[usable_se])

  # c_med needs BOTH a standard error and a sample size, which is a stricter
  # filter than se_median's. A dataset that carries no arm sizes therefore
  # falls back to se_median for every row rather than failing, because a
  # median standard error is still a better guess than nothing.
  paired <- if (length(n_studies) == length(se_studies)) {
    usable_se & is.finite(n_studies) & n_studies > 0
  } else {
    rep(FALSE, length(se_studies))
  }
  c_med <- if (any(paired)) {
    stats::median(se_studies[paired] * sqrt(n_studies[paired]))
  } else {
    NA_real_
  }

  has_n <- is.finite(n_missing) & n_missing > 0
  se_j <- rep(se_median, length(n_missing))
  if (is.finite(c_med) && any(has_n)) {
    se_j[has_n] <- c_med / sqrt(n_missing[has_n])
  }
  if (!all(is.finite(se_j) & se_j > 0)) return(NULL)
  se_j
}

# --------------------------------------------------------------------------
# Step 5: direction as a GATE, not a second scale
# --------------------------------------------------------------------------
# `results_known` records WHY a result is missing, which is what RoB-ME is
# actually about, and three of its five labels also imply WHICH WAY the
# missing effect lies. With the null at 0 (log scale for ratio measures) and
# s = sign(TE_obs), writing u = s*delta and a = s*TE_obs = |TE_obs|:
#
#   Not measured                            no mechanism; unconstrained, but
#                                           step 6 caps at amber
#   Reported but data not extractable       mechanism unknown; unconstrained
#   Measured but not reported (P > 0.05)    null-ward,   u < a
#   Measured but not reported (P < 0.05)    further out, u > a
#   Measured but not reported (opposite)    u < 0
#   free text                               unconstrained
#
# Take the UNION across rows. One unconstrained row makes the union
# everything and the gate never fires, which is the conservative answer and
# the right one: an unconstrained row means no direction can be ruled out.
#
# The gate earns its place on cases like this one: the missing results are
# suspected null-ward, but delta* is FURTHER from the null than the observed
# effect. Nothing the missing studies could plausibly report moves the
# conclusion in the direction they are suspected of lying, so the dot is
# green whatever its magnitude would have said. This is the direction-of-bias
# step of the Risk of bias flowchart, asked of missing evidence instead of
# high-risk-of-bias evidence.
.PUBIAS_MISSING_MECHANISMS <- c("not_measured", "unconstrained",
                                "null_ward", "further_out", "opposite")

# Matching is deliberately loose. The column is free text with an
# autocomplete datalist behind it, so a reviewer can and does type a variant;
# anything unrecognised falls to "unconstrained", which is the answer that
# rules nothing out.
.pubias_missing_mechanism <- function(results_known) {
  label <- tolower(trimws(as.character(results_known %||% "")))
  label[is.na(label)] <- ""
  squashed <- gsub(" ", "", label, fixed = TRUE)

  out <- rep("unconstrained", length(label))
  out[grepl("notmeasured", squashed, fixed = TRUE)]  <- "not_measured"
  out[grepl("opposite", squashed, fixed = TRUE)]     <- "opposite"
  out[grepl("p>0.05", squashed, fixed = TRUE)]       <- "null_ward"
  out[grepl("p<0.05", squashed, fixed = TRUE)]       <- "further_out"
  out
}

# Is u inside the union of the suspected regions? `a` is |TE_obs|.
.pubias_missing_in_suspected <- function(mechanisms, u, a) {
  if (!length(mechanisms)) return(TRUE)
  any(vapply(mechanisms, function(m) {
    switch(m,
      not_measured  = TRUE,
      unconstrained = TRUE,
      null_ward     = u < a,
      further_out   = u > a,
      opposite      = u < 0,
      TRUE)
  }, logical(1)))
}

# --------------------------------------------------------------------------
# The ordered decision
# --------------------------------------------------------------------------
# Returns a list, always with the same names:
#   step        1..6, the step that decided the answer
#   state       one of PMA_PUBIAS_DOT_STATES
#   reason      the tooltip
#   delta_star  the tipping point on the TE scale (NA before step 5)
#   se_new      the pooled standard error once the m studies are added
#   capped      TRUE when the "Not measured" cap turned a red into an amber
#
# `threshold_internal` is the Core GRADE threshold on the TE scale, or NULL /
# 0 when the rating target is the null.
.pubias_missing_tipping <- function(results_known = character(0),
                                    n_missing = numeric(0),
                                    te_obs = NA_real_,
                                    se_pooled = NA_real_,
                                    tau2 = NA_real_,
                                    ci_lower = NA_real_, ci_upper = NA_real_,
                                    pi_lower = NA_real_, pi_upper = NA_real_,
                                    se_studies = numeric(0),
                                    n_studies = numeric(0),
                                    threshold_internal = NULL,
                                    k = NA_integer_) {

  .out <- function(step, state, reason, delta_star = NA_real_,
                   se_new = NA_real_, capped = FALSE) {
    list(step = as.integer(step), state = state, reason = reason,
         delta_star = as.numeric(delta_star), se_new = as.numeric(se_new),
         capped = isTRUE(capped))
  }
  .unknown <- function(reason) .out(2L, "unknown", reason)

  results_known <- as.character(results_known)
  m <- length(results_known)

  # ---- Step 1 -------------------------------------------------------------
  if (m == 0L) {
    return(.out(1L, "green", paste0(
      "No missing results are recorded for this outcome, so there is ",
      "nothing that could overturn the pooled result. Reference only - it ",
      "rates nothing.")))
  }

  # ---- Step 2: is the model computable at all? ----------------------------
  te_obs    <- suppressWarnings(as.numeric(te_obs)[1])
  se_pooled <- suppressWarnings(as.numeric(se_pooled)[1])
  if (!is.finite(te_obs) || !is.finite(se_pooled) || se_pooled <= 0) {
    return(.unknown(paste0(
      "Not computed: this outcome has no usable pooled effect and standard ",
      "error to move.")))
  }

  k <- suppressWarnings(as.numeric(k)[1])
  if (is.finite(k) && k < .PUBIAS_MISSING_MIN_K) {
    return(.unknown(sprintf(paste0(
      "Not computed: fewer than %d studies, so there is no prediction ",
      "interval to anchor the comparison on."), .PUBIAS_MISSING_MIN_K)))
  }
  tau2 <- suppressWarnings(as.numeric(tau2)[1])
  if (!is.finite(tau2)) {
    return(.unknown(paste0(
      "Not computed: this analysis reports no between-study variance, which ",
      "the imputed missing studies' weights are built from.")))
  }
  if (tau2 <= 0) {
    return(.unknown(paste0(
      "Not computed: between-study variance is estimated at zero, so the ",
      "prediction interval this comparison is anchored on says nothing ",
      "about how far a missing study could lie.")))
  }
  pi_lower <- suppressWarnings(as.numeric(pi_lower)[1])
  pi_upper <- suppressWarnings(as.numeric(pi_upper)[1])
  if (!is.finite(pi_lower) || !is.finite(pi_upper) || pi_lower >= pi_upper) {
    return(.unknown(paste0(
      "Not computed: this analysis has no 95% prediction interval, which is ",
      "what the comparison is anchored on.")))
  }

  se_j <- .pubias_missing_impute_se(n_missing = n_missing,
                                    se_studies = se_studies,
                                    n_studies = n_studies)
  if (is.null(se_j) || length(se_j) != m) {
    return(.unknown(paste0(
      "Not computed: the observed studies carry no standard errors to ",
      "borrow, so a missing study's precision cannot be imputed.")))
  }

  # ---- The model ----------------------------------------------------------
  w_obs  <- 1 / se_pooled^2
  w_miss <- sum(1 / (se_j^2 + tau2))
  w_tot  <- w_obs + w_miss
  if (!is.finite(w_obs) || !is.finite(w_miss) || !is.finite(w_tot)) {
    return(.unknown(paste0(
      "Not computed: the weights this comparison rests on could not be ",
      "computed for this analysis.")))
  }
  se_new <- 1 / sqrt(w_tot)

  threshold <- suppressWarnings(as.numeric(threshold_internal %||% 0)[1])
  if (!is.finite(threshold) || threshold < 0) threshold <- 0

  # Which side of the threshold the pooled interval lies on. Same rule
  # before and after, with only the standard error changing.
  .conclusion <- function(te, se) {
    half <- .PUBIAS_MISSING_Z95 * se
    if (te - half >  threshold) return("above")
    if (te + half < -threshold) return("below")
    "spans"
  }
  conclusion_obs <- .conclusion(te_obs, se_pooled)

  # ---- Step 3: precision alone ------------------------------------------
  # TE_new(TE_obs) is TE_obs exactly - a weighted average of one value - so
  # the only thing that has changed here is se_new. If that flips the
  # conclusion, the missing studies do not have to disagree with anything.
  if (!identical(.conclusion(te_obs, se_new), conclusion_obs)) {
    return(.out(3L, "red", paste0(
      "The conclusion changes on precision alone: adding the missing ",
      "studies moves the pooled interval across the decision threshold ",
      "even if every one of them reports exactly what the observed studies ",
      "did. Reference only - it rates nothing."),
      se_new = se_new))
  }

  # ---- Step 4: is there a crossing at all? --------------------------------
  # The delta at which TE_new lands exactly on a conclusion boundary. This is
  # the closed form the header states; se_new is already fixed, so it is one
  # division.
  #
  # A NOTE ON WHEN THIS FIRES, because the answer is "rarely, and only on
  # degenerate input". TE_new is increasing and affine in delta and covers
  # the whole real line, and each conclusion is an interval in TE_new, so
  # whenever the missing studies carry any weight at all there IS a finite
  # delta that changes the conclusion. The only way there is not is
  # W_miss = 0: studies so imprecise that adding them moves nothing, in which
  # case TE_new(delta) = TE_obs for every delta and the conclusion is
  # genuinely untouchable. That is what "cannot be overturned" means here,
  # and it is a green rather than a white dot because the model gives a real
  # answer - it is not a case where the model could not be run.
  #
  # The step keeps its place in the order regardless. It must precede step 6
  # because step 6 divides by W_miss too, and a division producing +/-Inf
  # would otherwise be compared against the prediction interval and come out
  # green by accident rather than by reasoning.
  .delta_at <- function(bound) (w_tot * bound - w_obs * te_obs) / w_miss
  half_new <- .PUBIAS_MISSING_Z95 * se_new
  bound_hi <-  (threshold + half_new)
  bound_lo <- -(threshold + half_new)

  # The set of delta that LEAVE the conclusion unchanged is an interval,
  # because TE_new is increasing in delta and each conclusion is an interval
  # in TE_new. delta* is whichever endpoint of it is nearer to TE_obs: the
  # smallest departure from "the missing studies say what the observed ones
  # said" that changes the answer.
  candidates <- switch(conclusion_obs,
    above = .delta_at(bound_hi),
    below = .delta_at(bound_lo),
    c(.delta_at(bound_lo), .delta_at(bound_hi)))
  candidates <- candidates[is.finite(candidates)]
  if (!length(candidates)) {
    return(.out(4L, "green", paste0(
      "The conclusion cannot be overturned: the missing studies are too ",
      "imprecise to move the pooled interval across the decision threshold, ",
      "whatever they report. Reference only - it rates nothing."),
      se_new = se_new))
  }
  delta_star <- candidates[which.min(abs(candidates - te_obs))]

  # ---- Step 5: the direction gate ----------------------------------------
  mechanisms <- .pubias_missing_mechanism(results_known)
  sign_obs   <- if (te_obs < 0) -1 else 1
  if (!.pubias_missing_in_suspected(mechanisms,
                                    u = sign_obs * delta_star,
                                    a = abs(te_obs))) {
    return(.out(5L, "green", paste0(
      "The missing results are suspected of lying in one direction, and the ",
      "value that would change the conclusion lies the other way. Nothing ",
      "they could plausibly report overturns the result. Reference only - ",
      "it rates nothing."),
      delta_star = delta_star, se_new = se_new))
  }

  # ---- Step 6: magnitude, against the prediction interval ----------------
  # A fixed cutoff in SMD units was considered and rejected: SMD does not
  # exist for a binary outcome, so it cannot be the app's one rule. The
  # prediction interval is already computed, needs no cutoff, and works on
  # whatever scale the model was fit on. It is conservative in the safe
  # direction - delta* is the MEAN of m studies but is judged against the
  # spread of a single one, which errs toward red.
  ci_lower <- suppressWarnings(as.numeric(ci_lower)[1])
  ci_upper <- suppressWarnings(as.numeric(ci_upper)[1])
  if (!is.finite(ci_lower) || !is.finite(ci_upper)) {
    ci_lower <- te_obs - .PUBIAS_MISSING_Z95 * se_pooled
    ci_upper <- te_obs + .PUBIAS_MISSING_Z95 * se_pooled
  }

  inside_ci <- delta_star >= ci_lower && delta_star <= ci_upper
  inside_pi <- delta_star >= pi_lower && delta_star <= pi_upper

  state <- if (inside_ci) "red" else if (inside_pi) "amber" else "green"
  wording <- switch(state,
    red = paste0(
      "An ordinary missing result changes the conclusion: the value that ",
      "would overturn it sits inside the pooled 95% confidence interval."),
    amber = paste0(
      "A plausible missing result changes the conclusion: the value that ",
      "would overturn it sits outside the pooled 95% confidence interval ",
      "but inside the 95% prediction interval."),
    paste0(
      "Only a study unlike any observed changes the conclusion: the value ",
      "that would overturn it lies outside the 95% prediction interval."))

  # ---- The "Not measured" cap --------------------------------------------
  # An outcome that was never assessed cannot have been suppressed for what
  # it showed, so its absence is incompleteness rather than bias, and
  # incompleteness does not earn the strongest warning the tab can give.
  #
  # The cap applies only when EVERY missing row is "Not measured". A single
  # row with any other label is a row whose absence could be selective, and
  # capping the dot because it shares the table with never-assessed outcomes
  # would suppress exactly the warning the tab exists to give. "Reported but
  # data not extractable" - the label auto-seeded onto every NA-effect row,
  # and therefore the most common one - is deliberately NOT capped: "not
  # significant, data not shown" is textbook selective reporting and the
  # label cannot rule it out.
  capped <- identical(state, "red") && all(mechanisms == "not_measured")
  if (capped) {
    state   <- "amber"
    wording <- paste0(
      wording, " Capped from red because every missing result is recorded ",
      "as never measured, which is incompleteness rather than suppression.")
  }

  .out(6L, state, paste0(wording, " Reference only - it rates nothing."),
       delta_star = delta_star, se_new = se_new, capped = capped)
}

# The dot itself. One line, so that the app has one call site and the rich
# return above stays available to the tests that hold the six ordered
# outcomes to the spec.
.pubias_missing_dot <- function(...) {
  res <- .pubias_missing_tipping(...)
  .pubias_dot(res$state, res$reason)
}
