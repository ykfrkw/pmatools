# step3_imprecision.R - pure helpers behind the Imprecision tab's judgment inputs
#
# pmatools 0.11.0 gave grade_meta() four Imprecision arguments a reviewer can
# answer: `large_effect` (who answers Core GRADE 2 Fig 4's "implausibly large
# effect?" node), its `large_effect_rationale`, and the two settings of the
# binary CI-ratio rule, `ci_ratio_shortcut` and `ci_ratio_scale`. It also
# accepts `ois_rrr = "threshold"`. This file turns the tab's widgets into those
# arguments and decides when a combination must not reach grade_meta() at all.
#
# What it reads: the raw values of the tab's radios and textarea, whether a
# threshold is in force, the condition of an aborted grade_meta() call, and
# the staged package constants that name the legal values
# (PMA_LARGE_EFFECT_CHOICES, PMA_CI_RATIO_SHORTCUT_CHOICES,
# PMA_CI_RATIO_SCALE_CHOICES, PMA_OIS_RRR_FROM_THRESHOLD and
# PMA_OIS_THRESHOLD_OUT_OF_RANGE_CLASS in R/domain_imprecision.R), so the app
# can never offer a value the package would refuse. What it produces: named argument lists for grade_obj() in
# R/step3_grade.R to splice, the widgets' choice vectors, and the sentences the
# tab prints. Every function is pure - no `input`, `output`, `session` or
# `state` - so tests/testthat/test-step3-imprecision-args.R can call them
# without Shiny.
#
# WHY AN ARGUMENT IS OMITTED RATHER THAN SENT. grade_meta() reads missing() on
# `ci_ratio_shortcut` and `ci_ratio_scale`: under rule_set =
# "core_grade_bmj_2025" an unsupplied pair defaults to "always" / "fitted", the
# 0.5.x behaviour, and an explicit value wins. A widget always has a value, so
# the radios carry a "rule-set default" choice that sends nothing, and an
# explicit NULL is never sent either (the package aborts on it). The same rule
# keeps an untouched `large_effect = "auto"` out of the exported analysis.R.
#
# WHY A "MATCH THE THRESHOLD" RATING CAN BE RE-RUN AT 0.20. The threshold can
# put the OIS alternative rate outside (0, 1) - a control-group risk of 156
# per 1,000 and an absolute threshold of 900 per 1,000 ask for 1,056 per
# 1,000 - and grade_meta() then aborts with the classed condition
# PMA_OIS_THRESHOLD_OUT_OF_RANGE_CLASS (pmatools 0.11.1). An abort takes all
# five domains down with it, for an input that only ever chose the OIS
# target. So step3_ois_threshold_retry_args() lets grade_obj() re-run once at
# STEP3_OIS_RRR_DEFAULT, the rating carries STEP3_OIS_FALLBACK_ATTR, and the
# tab says inline what happened and how to choose differently.
#
# THE RULE FOR A NEW HELPER: it belongs here when it maps one of the
# Imprecision tab's reviewer answers to a grade_meta() argument, or words what
# that answer did. Anything about the threshold itself belongs in
# R/step3_threshold.R; anything that renders a rated object's facts in general
# belongs in R/judgment_display.R.

# The choice that sends nothing, so grade_meta() resolves the setting from the
# rule set. Not a package value; never forwarded.
STEP3_RULE_SET_DEFAULT_CHOICE <- "rule_set_default"

# Core GRADE 2's default modest relative risk reduction. The radio's other
# numeric value is 0.25, the second one the paper names.
STEP3_OIS_RRR_DEFAULT <- 0.20

# The attribute a rating carries when "Match the threshold" was replaced by
# STEP3_OIS_RRR_DEFAULT because the threshold-derived rate left (0, 1). Its
# value is list(p0, p1, how), as the package's condition reported them.
STEP3_OIS_FALLBACK_ATTR <- "pma_ois_threshold_fallback"

STEP3_LARGE_EFFECT_CHOICES <- c(
  "Automatic: the magnitude rule (default)"           = "auto",
  "Plausible: do not rate down for a limited sample"  = "plausible",
  "Implausible: check the optimal information size"   = "implausible"
)

STEP3_CI_RATIO_SHORTCUT_CHOICES <- c(
  "Rule-set default"                       = STEP3_RULE_SET_DEFAULT_CHOICE,
  "Only when the OIS cannot be calculated" = "when_ois_unavailable",
  "Always, before the OIS"                 = "always"
)

STEP3_CI_RATIO_SCALE_CHOICES <- c(
  "Rule-set default"                              = STEP3_RULE_SET_DEFAULT_CHOICE,
  "Risk ratio refit, compared with 3"             = "rr",
  "Fitted scale (an odds ratio compared with 2.5)" = "fitted"
)

# The words the tab uses for each resolved CI-ratio setting, keyed by the
# package value recorded in $imprecision_settings.
STEP3_CI_RATIO_SHORTCUT_WORDS <- c(
  when_ois_unavailable = "only when the OIS cannot be calculated",
  always               = "always, before the OIS"
)
STEP3_CI_RATIO_SCALE_WORDS <- c(
  rr     = "a risk ratio refit, compared with 3",
  fitted = "the fitted scale"
)

# Is `value` one usable string from `choices`?
.step3_is_choice <- function(value, choices) {
  is.character(value) && length(value) == 1L && !is.na(value) &&
    value %in% choices
}

# The trimmed rationale, or NULL when there is none to quote.
.step3_rationale_or_null <- function(rationale) {
  if (is.null(rationale) || length(rationale) == 0L) return(NULL)
  text <- trimws(as.character(rationale)[1])
  if (is.na(text) || !nzchar(text)) NULL else text
}

# What the ois_rrr radio sends: "threshold", or a number in (0, 1). Anything
# else is the 0.20 default, which is what the radio ships selected.
step3_ois_rrr_value <- function(value) {
  if (identical(value, PMA_OIS_RRR_FROM_THRESHOLD)) {
    return(PMA_OIS_RRR_FROM_THRESHOLD)
  }
  rrr <- suppressWarnings(as.numeric(value %||% STEP3_OIS_RRR_DEFAULT))
  if (length(rrr) != 1L || !is.finite(rrr) || rrr <= 0 || rrr >= 1) {
    return(STEP3_OIS_RRR_DEFAULT)
  }
  rrr
}

# The ois_rrr actually sent. "threshold" needs a threshold to power against,
# and grade_meta() aborts without one; a superiority question has none, so
# the OIS falls back to the default RRR there and the tab says so.
step3_ois_rrr_for_grade <- function(rrr, has_threshold) {
  if (identical(rrr, PMA_OIS_RRR_FROM_THRESHOLD) && !isTRUE(has_threshold)) {
    return(STEP3_OIS_RRR_DEFAULT)
  }
  rrr
}

# The large-effect answer as grade_meta() arguments.
#
# Returns list(args, blocked). `args` is empty for "auto" (the package
# default) and for a judgment that has no rationale yet; `blocked` is TRUE in
# the second case only, so the tab can say the answer is not in force. A
# rationale-less judgment is never sent: grade_meta() would abort, and an
# aborted rating would take the other four domains down with it.
step3_large_effect_args <- function(choice, rationale) {
  if (!.step3_is_choice(choice, PMA_LARGE_EFFECT_CHOICES) ||
      identical(choice, "auto")) {
    return(list(args = list(), blocked = FALSE))
  }
  reason <- .step3_rationale_or_null(rationale)
  if (is.null(reason)) return(list(args = list(), blocked = TRUE))
  list(args = list(large_effect = choice, large_effect_rationale = reason),
       blocked = FALSE)
}

# The CI-ratio settings as grade_meta() arguments: only the ones the reviewer
# set explicitly, and only for a binary outcome, the one scale the rule reads.
step3_ci_ratio_args <- function(shortcut, scale, is_binary) {
  if (!isTRUE(is_binary)) return(list())
  args <- list()
  if (.step3_is_choice(shortcut, PMA_CI_RATIO_SHORTCUT_CHOICES)) {
    args$ci_ratio_shortcut <- shortcut
  }
  if (.step3_is_choice(scale, PMA_CI_RATIO_SCALE_CHOICES)) {
    args$ci_ratio_scale <- scale
  }
  args
}

# The inline message under the large-effect radio while its rationale is
# missing, or NULL when there is nothing to say.
step3_large_effect_blocked_message <- function(choice) {
  sprintf(paste0(
    "A written reason is required for '%s'. Until one is given, the rating ",
    "uses the automatic magnitude rule."), choice)
}

# One sentence naming the CI-ratio settings a rating was MADE with, read off
# $imprecision_settings, so a "rule-set default" radio still shows what it
# resolved to. NULL for a rating that records no settings (pre-0.11.0).
step3_ci_ratio_settings_sentence <- function(settings) {
  shortcut <- settings$ci_ratio_shortcut
  scale    <- settings$ci_ratio_scale
  if (!.step3_is_choice(shortcut, names(STEP3_CI_RATIO_SHORTCUT_WORDS)) ||
      !.step3_is_choice(scale, names(STEP3_CI_RATIO_SCALE_WORDS))) {
    return(NULL)
  }
  sprintf(paste0("This rating consulted the CI-ratio rule %s, read on %s ",
                 "for an odds ratio fit."),
          STEP3_CI_RATIO_SHORTCUT_WORDS[[shortcut]],
          STEP3_CI_RATIO_SCALE_WORDS[[scale]])
}

# The reviewer's large-effect judgment as the rated object records it: the
# `large_effect` fact's value, or NULL when the magnitude rule answered.
step3_large_effect_fact <- function(facts) {
  if (!is.data.frame(facts) || !all(c("key", "value") %in% names(facts))) {
    return(NULL)
  }
  hit <- facts$value[facts$key %in% "large_effect"]
  if (length(hit) == 0L) NULL else hit[[1]]
}

# Move the named fact keys to the front of a fact table, so a short list
# (pma_facts_list() shows six rows) cannot cut off the one a reviewer typed.
step3_facts_pinned_first <- function(facts, pinned) {
  if (!is.data.frame(facts) || !"key" %in% names(facts) || !length(pinned)) {
    return(facts)
  }
  first <- facts$key %in% pinned
  facts[c(which(first), which(!first)), , drop = FALSE]
}

# The retry decision for a grade_meta() call that aborted. Returns the
# arguments to re-run with (`ois_rrr` replaced by STEP3_OIS_RRR_DEFAULT) when
# the abort is the classed out-of-range one AND the app had sent "threshold";
# NULL for every other condition, which stays an error. Both halves are
# checked: replacing a number the reviewer chose is not this helper's
# business, whatever the condition says.
step3_ois_threshold_retry_args <- function(args, cnd) {
  if (!inherits(cnd, PMA_OIS_THRESHOLD_OUT_OF_RANGE_CLASS)) return(NULL)
  if (!identical(args$ois_rrr, PMA_OIS_RRR_FROM_THRESHOLD)) return(NULL)
  args$ois_rrr <- STEP3_OIS_RRR_DEFAULT
  args
}

# What the fallback records, read off the package's condition: the
# control-group risk and the derived rate as proportions, and how the rate
# was derived. A missing field stays NA rather than failing the rating.
step3_ois_threshold_fallback_info <- function(cnd) {
  num <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 1L && !is.na(x)) x else NA_real_
  }
  how <- cnd$how
  list(p0  = num(cnd$p0),
       p1  = num(cnd$p1),
       how = if (is.character(how) && length(how) == 1L) how else
         NA_character_)
}

# The inline warning under the ois_rrr radio after a fallback, in the
# reviewer's display unit. `info` is step3_ois_threshold_fallback_info().
step3_ois_threshold_fallback_message <- function(info,
                                                 per = STEP3_PER_DEFAULT) {
  sprintf(paste0(
    "The Threshold gives an alternative rate of %s at a control-group risk ",
    "of %s, which is not a probability, so the OIS was powered against the ",
    "%.0f percent modest relative risk reduction instead. Choose 20 or 25 ",
    "percent, or change the Threshold."),
    step3_per_label(1000 * info$p1, per),
    step3_per_label(1000 * info$p0, per),
    100 * STEP3_OIS_RRR_DEFAULT)
}

# The same fact for the Imprecision domain's notes, which are read away from
# the tab (Evidence Profile, export), so it names the argument and the
# package's own derivation rather than a display unit.
step3_ois_threshold_fallback_note <- function(info) {
  sprintf(paste0(
    "ois_rrr = 'threshold' was not used: the imprecision threshold (%s) ",
    "gives an OIS alternative rate of %.4f at a control-group risk of %.4f, ",
    "which is not a probability, so the OIS was powered against ",
    "ois_rrr = %.2f instead."),
    info$how, info$p1, info$p0, STEP3_OIS_RRR_DEFAULT)
}
