# rare_step3.R - what a certainty rating needs to know about a rare-event
# analysis
#
# run_rare_ma() (R/rare_events.R) fits a suite of sparse-data methods and hands
# one of them back as the primary. Everything downstream of that - the domain
# assessors, the app's Step 3, the exported record - then rates the primary
# without knowing it IS a primary. The helpers here are what closes that gap:
# they name the method, ask the suite the rating's own question, and state the
# one assumption the suite does not make.
#
# All pure. The suite is passed in, never fitted here: fitting it a second time
# would be a second chance for the two fits to disagree, and the app already
# holds the one run_rare_ma() produced (shiny/SPEC.md 3.4.14).
#
# NOTHING HERE RATES ANYTHING. Every function returns a fact or a sentence.
# Sparse data earns arithmetic that is valid on the data at hand and a record
# of how much the answer depended on a method choice; it does not earn a
# domain, an automatic downgrade, or a change to any domain's decision rule.

# The continuity correction that was NOT applied, in one sentence.
#
# Core GRADE says nothing about continuity corrections, so this is not a
# quotation - it is the assumption behind every number downstream, stated
# rather than left invisible. A fixed 0.5 added to each cell of a zero-event
# study biases the pooled estimate toward the null, which on sparse data is
# the difference between "no signal" and "no power to see one". The suite's
# recommended methods (BB_CR, MH_no_cc, GLMM, Peto) are correction-free by
# construction, so the reader has to be told that the 0.5 is absent as well as
# that it would have mattered.
PMA_RARE_NO_CC_NOTE <- paste0(
  "No continuity correction was applied: the rare-event methods this estimate ",
  "comes from handle zero-event cells without adding 0.5 to any cell. A 0.5 ",
  "correction biases the pooled estimate toward the null and would otherwise ",
  "be an invisible assumption behind every number below. The correction-free ",
  "methods and the corrected ones sit side by side in the Step 2 method table."
)

# Label for a method id, taken from the specs run_rare_ma() itself fits, so a
# renamed method cannot end up with two names. Falls back to the id, which is
# still more informative than dropping the statement.
.rare_method_label <- function(method_id, effect_scale = "OR") {
  if (is.null(method_id) || length(method_id) != 1L || is.na(method_id) ||
      !nzchar(method_id)) {
    return(NA_character_)
  }
  scale <- if (identical(effect_scale, "RR")) "RR" else "OR"
  specs <- .rare_method_specs(scale)
  ids   <- vapply(specs, `[[`, character(1), "id")
  at    <- match(method_id, ids)
  if (is.na(at)) return(as.character(method_id))
  specs[[at]]$label
}

# The sentence that names the method where the rating is set up.
#
# A reader of the Summary of Findings cannot otherwise tell a beta-binomial
# estimate from an inverse-variance one: both arrive as an odds ratio with a
# 95% interval, and only one of them is valid on data this sparse.
rare_method_statement <- function(method_id, effect_scale = "OR") {
  label <- .rare_method_label(method_id, effect_scale)
  if (is.na(label)) return(NA_character_)
  sprintf(paste0(
    "The pooled estimate rated below comes from the rare-event workflow, not ",
    "from the regular pairwise analysis. Primary method: %s (%s), on the %s ",
    "scale."),
    label, method_id, if (identical(effect_scale, "RR")) "RR" else "OR")
}

# --------------------------------------------------------------------------
# The suite as a sensitivity analysis FOR THE RATING
# --------------------------------------------------------------------------
# Step 2 already shows the suite as a sensitivity analysis for the ESTIMATE:
# seven confidence intervals on one forest plot. That answers "how much does
# the effect depend on the method", which is not the question Core GRADE 2
# asks. Its question is whether the interval crosses the chosen threshold, and
# a suite of intervals can be asked exactly that - once each.
#
# Unanimity is worth having on the record. Disagreement means the imprecision
# judgment rests on a method choice rather than on the evidence, and the
# reviewer is entitled to know which methods disagree and where their
# intervals sit. Either way it costs no new statistics: every fit already
# exists.

# Does a confidence interval cross the chosen threshold? The same test
# assess_imprecision() applies to the primary, on the same (TE / log) scale,
# written once so the sensitivity answer and the rated answer cannot come from
# two different rules.
#
# `thr` is the threshold on the TE scale, always positive; NULL or 0 means the
# threshold is the null, and then the question is whether the interval spans 0.
.rare_crosses_threshold <- function(lower, upper, thr = NULL) {
  if (!is.finite(lower) || !is.finite(upper)) return(NA)
  if (is.null(thr) || length(thr) != 1L || !is.finite(thr) || thr <= 0) {
    return((lower < 0) && (upper > 0))
  }
  ((lower < -thr) && (upper > -thr)) || ((lower < thr) && (upper > thr))
}

# Ask every fitted method the primary's question.
#
# `rare` is a pma_rare_meta from run_rare_ma(); `threshold_internal` is the
# threshold on the TE scale, exactly as grade_meta() resolved it (NULL = the
# null). Returns:
#   answers    named logical, one per method that produced a usable interval
#   primary    the primary method's own answer (NA when it produced none)
#   unanimous  TRUE when every usable method answered the same way
#   disagree   ids of the methods that answered differently from the primary
#   k_methods  how many methods could be asked
#   table      the rows behind the answers, for the sentence and the panel
# A suite with fewer than two usable methods is reported as unanimous = NA:
# one answer is not a consensus, and calling it one would be the same mistake
# as reporting a single study as consistent.
rare_suite_crossing <- function(rare, threshold_internal = NULL) {
  empty <- list(answers = logical(0), primary = NA, unanimous = NA,
                disagree = character(0), k_methods = 0L,
                table = NULL, effect_scale = NA_character_)
  if (!inherits(rare, "pma_rare_meta")) return(empty)
  tab <- as.data.frame(rare$method_table, stringsAsFactors = FALSE)
  if (!nrow(tab)) return(empty)

  scale <- rare$effect_scale %||% "OR"
  # Ratio measures are stored back-transformed in method_table and judged on
  # the log scale, which is where threshold_internal lives.
  usable <- is.finite(tab$estimate) & is.finite(tab$ci_low) &
    is.finite(tab$ci_high) & tab$ci_low > 0 & tab$ci_high > 0
  tab <- tab[usable, , drop = FALSE]
  if (!nrow(tab)) return(empty)

  lower <- log(tab$ci_low)
  upper <- log(tab$ci_high)
  answers <- vapply(seq_len(nrow(tab)), function(i) {
    .rare_crosses_threshold(lower[i], upper[i], threshold_internal)
  }, logical(1))
  names(answers) <- tab$method_id

  primary_id <- rare$primary_method
  primary_at <- match(primary_id, tab$method_id)
  primary    <- if (is.na(primary_at)) NA else answers[[primary_at]]

  known <- answers[!is.na(answers)]
  unanimous <- if (length(known) < 2L) NA else length(unique(known)) == 1L
  disagree <- if (is.na(primary)) {
    character(0)
  } else {
    names(known)[known != primary]
  }

  list(answers      = answers,
       primary      = primary,
       unanimous    = unanimous,
       disagree     = disagree,
       k_methods    = length(known),
       table        = tab,
       effect_scale = scale)
}

# The sensitivity result in words, for the domain note and for the panel.
#
# The wording deliberately reports the QUESTION and not a judgment: "every
# method agrees the interval crosses the threshold" is a fact about the suite,
# and the domain's own rule - unchanged - is what turns the primary's answer
# into a rating.
rare_suite_crossing_note <- function(cross, threshold_internal = NULL) {
  if (is.null(cross) || !cross$k_methods) return(NA_character_)
  thr_label <- if (is.null(threshold_internal) ||
                   !is.finite(threshold_internal) || threshold_internal <= 0) {
    "the null"
  } else {
    "the chosen threshold"
  }
  answer_word <- function(a) if (isTRUE(a)) "crosses" else "does not cross"

  if (is.na(cross$primary)) {
    return(sprintf(paste0(
      "Rare-event method sensitivity: the primary method produced no usable ",
      "interval, so the %d other fitted method(s) could not be compared with ",
      "it."), cross$k_methods))
  }

  head <- sprintf(paste0(
    "Rare-event method sensitivity (%d fitted method%s asked the same ",
    "question Core GRADE 2 asks the primary: does the 95%% CI cross %s?): ",
    "the primary %s it."),
    cross$k_methods, if (cross$k_methods == 1L) "" else "s", thr_label,
    answer_word(cross$primary))

  if (isTRUE(cross$unanimous)) {
    return(paste0(head, " Every other fitted method gives the same answer, so ",
                  "the imprecision judgment does not rest on the choice of ",
                  "method."))
  }
  if (is.na(cross$unanimous)) {
    return(paste0(head, " Only one method produced a usable interval, so ",
                  "there is nothing to compare it with."))
  }

  tab <- cross$table
  at  <- match(cross$disagree, tab$method_id)
  detail <- paste(sprintf("%s (%s %.2f, 95%% CI %.2f to %.2f)",
                          tab$label[at], cross$effect_scale,
                          tab$estimate[at], tab$ci_low[at], tab$ci_high[at]),
                  collapse = "; ")
  paste0(head, sprintf(paste0(
    " %d fitted method%s disagree%s, so the imprecision judgment rests on the ",
    "choice of method and not on the evidence alone: %s."),
    length(cross$disagree),
    if (length(cross$disagree) == 1L) "" else "s",
    if (length(cross$disagree) == 1L) "s" else "",
    detail))
}
