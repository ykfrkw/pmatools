# pubias_missing.R - the tipping point behind the Missing results (RoB-ME)
# status dot.
#
# Purpose : answer one question - how far from the observed pooled effect
#           would the missing studies have to lie before the conclusion
#           changes? Far means the missing evidence cannot overturn the
#           result; near means it can.
# Inputs  : the reviewer's missing-results table (results_known and n, one
#           row per missing study; "Not measured" rows are counted and
#           then excluded) and the fitted analysis's pooled effect,
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
#   0. drop every "Not measured" row (see step 5's
#      comment); m counts what is left
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

# The sentence every coloured reason closes on. Named because the notes on
# excluded and undirected rows are inserted in front of it, which needs the
# exact string.
.PUBIAS_MISSING_REFERENCE_ONLY <- "Reference only - it rates nothing."

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
# actually about (Page MJ et al. Risk Of Bias due to Missing Evidence
# (ROB-ME): a new tool for assessing risk of non-reporting biases in evidence
# syntheses. BMJ 2023;383:e076754, doi:10.1136/bmj-2023-076754), and three of
# its labels also imply WHICH WAY the missing effect lies. With the null at 0
# (log scale for ratio measures) and s = sign(TE_obs), writing u = s*delta
# and a = s*TE_obs = |TE_obs|:
#
#   Not measured                            EXCLUDED from the computation
#   Reported but data not extractable       mechanism unknown; unconstrained
#   Measured but not reported (P > 0.05)    null-ward,   u < a
#   Measured but not reported (P < 0.05)    further out, u > a
#   Measured but not reported (opposite)    u < 0
#   free text                               unconstrained
#
# "Not measured" is not a missing result in the bias sense. RoB-ME asks
# whether results were left out of the synthesis BECAUSE of what they showed,
# and a result that was never measured showed nothing and so cannot have been
# selectively unreported. Such rows are therefore dropped before the model
# runs: from m, from the imputed weights W_miss, and from the direction gate.
# Counting them would do harm in both directions. In W_miss they would add
# precision the evidence never had a chance to contribute; in the gate they
# used to be unconstrained, so one never-measured row made the union
# "everything" and silenced the gate for every other row - the dot turned red
# on a table whose only directed rows said the tipping value was implausible.
# A table that is all "Not measured" has no missing results at all and is
# answered at step 1. Classification is unchanged: .pubias_missing_mechanism()
# still labels such rows "not_measured", because the caller counts them.
#
# Take the UNION across the remaining rows. One unconstrained row makes the
# union everything and the gate never fires, which is the conservative answer
# and the right one: an unconstrained row means no direction can be ruled out.
# Undirected rows are deliberately NOT defaulted to null-ward, because "data
# not extractable" is exactly where "not significant, data not shown" hides.
# What the dot does instead is say so: when the directed rows alone would put
# delta* outside their suspected region, the reason tells the reviewer that
# the undirected rows are what kept the gate shut, and that recording why
# each result is missing would let it apply.
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

# Is u inside the union of the suspected regions? `a` is |TE_obs|. The
# tipping computation never passes "not_measured" rows here; the arm that
# treats one as unconstrained is the safe answer for any other caller.
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
# Returns a list, always with the same names on every return path:
#   step            1..6, the step that decided the answer
#   state           one of PMA_PUBIAS_DOT_STATES
#   reason          the tooltip
#   delta_star      the tipping point on the TE scale (NA before step 5)
#   se_new          the pooled standard error once the m studies are added
#   n_not_measured  rows recorded as "Not measured", excluded from all of it
#   n_undirected    remaining rows whose mechanism is "unconstrained"
#
# m below is the number of rows LEFT once "Not measured" rows are dropped.
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

  n_not_measured <- 0L
  n_undirected   <- 0L
  undirected_warning <- NULL

  # Every reason that is not step 1's carries the note on excluded rows, and
  # the undirected warning when step 6 set it; both go before the closing
  # "Reference only" so that sentence stays last.
  .out <- function(step, state, reason, delta_star = NA_real_,
                   se_new = NA_real_) {
    notes <- character(0)
    if (step != 1L && n_not_measured > 0L) {
      notes <- c(notes, sprintf(paste0(
        "%d missing result%s recorded as not measured %s left out: RoB-ME ",
        "does not count an outcome that was never measured as a missing ",
        "result."), n_not_measured, if (n_not_measured == 1L) "" else "s",
        if (n_not_measured == 1L) "is" else "are"))
    }
    notes <- c(notes, undirected_warning)
    if (length(notes)) {
      has_tail <- endsWith(reason, .PUBIAS_MISSING_REFERENCE_ONLY)
      body <- if (has_tail) {
        trimws(substr(reason, 1L,
                      nchar(reason) - nchar(.PUBIAS_MISSING_REFERENCE_ONLY)))
      } else {
        reason
      }
      reason <- paste(c(body, notes,
                        if (has_tail) .PUBIAS_MISSING_REFERENCE_ONLY),
                      collapse = " ")
    }
    list(step = as.integer(step), state = state, reason = reason,
         delta_star = as.numeric(delta_star), se_new = as.numeric(se_new),
         n_not_measured = as.integer(n_not_measured),
         n_undirected = as.integer(n_undirected))
  }
  .unknown <- function(reason) .out(2L, "unknown", reason)

  results_known <- as.character(results_known)

  # ---- Step 1 -------------------------------------------------------------
  if (!length(results_known)) {
    return(.out(1L, "green", paste0(
      "No missing results are recorded for this outcome, so there is ",
      "nothing that could overturn the pooled result. ",
      .PUBIAS_MISSING_REFERENCE_ONLY)))
  }

  # "Not measured" rows leave here, before m is counted; see the comment
  # above .PUBIAS_MISSING_MECHANISMS for why. n_missing is subset in
  # parallel only when it is parallel - a ragged pair is left for the
  # imputation's length check to report.
  mechanisms_all <- .pubias_missing_mechanism(results_known)
  is_measured    <- mechanisms_all != "not_measured"
  n_not_measured <- sum(!is_measured)
  if (length(n_missing) == length(results_known)) {
    n_missing <- n_missing[is_measured]
  }
  results_known <- results_known[is_measured]
  mechanisms    <- mechanisms_all[is_measured]
  n_undirected  <- sum(mechanisms == "unconstrained")
  m <- length(results_known)

  if (m == 0L) {
    return(.out(1L, "green", paste0(
      "The only missing entries for this outcome are outcomes that were ",
      "never measured, which RoB-ME does not count as missing results, so ",
      "there is nothing that could overturn the pooled result. ",
      .PUBIAS_MISSING_REFERENCE_ONLY)))
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
      "did. ", .PUBIAS_MISSING_REFERENCE_ONLY),
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
      "whatever they report. ", .PUBIAS_MISSING_REFERENCE_ONLY),
      se_new = se_new))
  }
  delta_star <- candidates[which.min(abs(candidates - te_obs))]

  # ---- Step 5: the direction gate ----------------------------------------
  sign_obs <- if (te_obs < 0) -1 else 1
  u_star   <- sign_obs * delta_star
  if (!.pubias_missing_in_suspected(mechanisms, u = u_star,
                                    a = abs(te_obs))) {
    return(.out(5L, "green", paste0(
      "The missing results are suspected of lying in one direction, and the ",
      "value that would change the conclusion lies the other way. Nothing ",
      "they could plausibly report overturns the result. ",
      .PUBIAS_MISSING_REFERENCE_ONLY),
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

  # The gate stayed shut. If the directed rows alone would have opened it,
  # the undirected rows are the whole reason, and the reviewer can fix that
  # by recording why each result is missing.
  directed <- mechanisms[mechanisms != "unconstrained"]
  if (n_undirected > 0L && length(directed) &&
      !.pubias_missing_in_suspected(directed, u = u_star, a = abs(te_obs))) {
    undirected_warning <- sprintf(paste0(
      "%d missing result%s carr%s no suspected direction, which keeps the ",
      "direction gate from ruling the tipping value out; recording why each ",
      "result is missing (e.g. \"assumed P > 0.05\") would let it apply."),
      n_undirected, if (n_undirected == 1L) "" else "s",
      if (n_undirected == 1L) "ies" else "y")
  }

  .out(6L, state, paste(wording, .PUBIAS_MISSING_REFERENCE_ONLY),
       delta_star = delta_star, se_new = se_new)
}

# The dot itself. One line, so that the app has one call site and the rich
# return above stays available to the tests that hold the six ordered
# outcomes to the spec.
.pubias_missing_dot <- function(...) {
  res <- .pubias_missing_tipping(...)
  .pubias_dot(res$state, res$reason)
}
