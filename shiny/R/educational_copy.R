# educational_copy.R - American English educational copy for the wizard
#
# All user-facing text lives here as named lists, so that copy edits do not
# require touching the UI / server code. Values may be plain strings or
# templates (with `{{slot}}` placeholders) used by the result blocks.

EDU_COPY <- list(

  # Step headers carry the title and nothing else. Each step used to open with
  # a paragraph describing what it does and, on Step 1, why the app is only
  # part of a systematic review. That prose was re-read on every visit, pushed
  # the first control below the fold, and said the same thing four times. What
  # is genuinely once-per-session moved to `intro_modal` below; what described
  # an individual control moved next to that control.
  steps = list(
    step1 = list(title = "Step 1: Data"),
    step2 = list(title = "Step 2: Meta-analysis"),
    step3 = list(title = "Step 3: Certainty assessment (Core GRADE series)"),
    step4 = list(title = "Step 4: Export")
  ),

  # Shown once per session, from app.R's server body, before the reviewer
  # touches anything. This is the one claim in the app that is about the work
  # AROUND the analysis rather than about a control on screen, so it is stated
  # once and dismissed rather than reprinted above every step.
  intro_modal = list(
    title = "Before you start",
    body  = htmltools::HTML(paste0(
      # The abbreviation is expanded here, in the first sentence, because the
      # next one uses it: "SR&MA" arriving undefined is the reader's first
      # encounter with the whole activity the app sits inside.
      "<p><strong>Statistical pooling is only a small part of a systematic ",
      "review and meta-analysis (SR&amp;MA). A high-quality SR&amp;MA also ",
      "needs a detailed, prespecified and pre-registered protocol (e.g., on ",
      "PROSPERO, OSF or other platforms), a comprehensive search, dual ",
      "independent screening and data extraction, and risk-of-bias ",
      "assessment - all completed BEFORE the analysis.</strong></p>",
      "<p>This app handles the pooling and Core GRADE certainty steps; make ",
      "sure the upstream review work is in place first.</p>"
    )),
    dismiss = "Got it"
  ),

  # `$ref` is the reference text for a domain tab: one string, house style, no
  # DOI *in the string itself* (see shiny/SPEC.md). The six Core GRADE papers
  # are all Guyatt / BMJ / 2025, so the bare form cannot tell them apart and
  # they carry the series number as a prefix. `pmid_url()` was deleted with the
  # old per-domain DOI fields; it had no call sites left.
  #
  # `$core_grade` is which paper of the series that is, as a number.
  # pma_domain_reference() hands it to .core_grade_doi_url() (R/house_style.R) to
  # open the paper in a new tab. It is a field rather than something recovered
  # from `$ref` because the prefix is display text: a regex over it would turn
  # any rewording of the citation into a silently dead link, and the number is
  # what the DOI map is keyed on anyway.
  domains = list(

    rob = list(
      header     = "Risk of Bias",
      ref        = "Core GRADE 4. Guyatt G, et al. BMJ. 2025",
      core_grade = 4
    ),

    inconsistency = list(
      header     = "Inconsistency",
      ref        = "Core GRADE 3. Guyatt G, et al. BMJ. 2025",
      core_grade = 3
    ),

    indirectness = list(
      header     = "Indirectness",
      ref        = "Core GRADE 5. Guyatt G, et al. BMJ. 2025",
      core_grade = 5,
      # Sits beside the four subdomain questions, because it is the reason the
      # overall override below them exists.
      gradient = paste0(
        "Core GRADE 5 Table 2 ranks Population lowest and Outcome highest; ",
        "the worst-case fold used here is symmetric and ignores that ranking."
      ),
      # Beside the Population question. That radio's own wording ("sufficiently
      # similar") invites the wrong test: reviewers rate down because the trial
      # population is demographically unlike theirs, which is not what Core
      # GRADE 5 asks. It asks whether the treatment EFFECT would differ, and
      # its Table 2 puts Population at the bottom of the gradient for exactly
      # that reason (R/domain_indirectness.R quotes it: "Low likelihood because
      # relative effects are typically similar across populations"). The
      # ranking itself is `gradient`'s job below; this says which test to
      # apply, which was on the tab nowhere.
      population = paste0(
        "Ask whether the treatment effect would differ, not whether the ",
        "population resembles yours. Relative effects are rarely different ",
        "across populations."
      ),
      # Beside the Outcome question. Both halves are judgments the reviewer
      # cannot read off the data: how far a surrogate warrants rating down,
      # and whether it belongs in the same analysis. The second half read
      # "Never pool the two" until 0.5.1; Core GRADE 5 states no such
      # prohibition, so the flat imperative claimed more than the source does.
      surrogate = paste0(
        "A surrogate outcome is grounds to consider rating down, depending ",
        "on how closely it tracks the patient-important one. Pooling the two ",
        "is not recommended."
      )
    ),

    imprecision = list(
      header     = "Imprecision",
      ref        = "Core GRADE 2. Guyatt G, et al. BMJ. 2025",
      core_grade = 2
    ),

    pubias = list(
      header     = "Publication bias",
      ref        = "Core GRADE 4. Guyatt G, et al. BMJ. 2025",
      core_grade = 4
    )
  ),

  # ----- Configuration tab (formerly "Decision threshold") ----------------
  # Everything the five certainty domains depend on is established here, in
  # the order a reviewer needs to decide it: the control-group risk first
  # (because the absolute threshold is only interpretable against it), then
  # the threshold itself, then how the effect is presented.
  # `intro` is gone. It restated in 115 words what the three boxed sections
  # below it already say beside the controls they belong to, and Core GRADE's
  # cross-cutting use of the threshold is the caption of three flowcharts.
  #
  # `continuous_intro` is gone for the same reason as the five domain `how`
  # bodies: it was a recitation of what Core GRADE 6 ranks, and the reviewer
  # answers nothing with it.
  config_tab = list(
    # The third option prints the two presentations on two rows of one
    # outcome, so Core GRADE 6's agreement check is reachable and the old "not
    # side by side" caveat is no longer true. What none of the three touches is
    # the rating, and a reviewer picking between them here has every reason to
    # think it might.
    continuous_departure = paste0(
      "Core GRADE 6 recommends showing the effect and the responder ",
      "proportion together; the third option does that. None of the three ",
      "changes the rating."
    ),
    chinn_caveat = paste0(
      "Chinn's formula, not Core GRADE 6's per-instrument procedure. The two ",
      "disagree, and the Summary of Findings table says so."
    ),
    responder_default = paste0(
      "The 20 percent starting value is an app convention. Replace it with a ",
      "rate from your data, or confirm it."
    ),

    # ----- The four clinical questions ------------------------------------
    # The vocabulary is PMA_CLINICAL_QUESTIONS (R/ui_helpers.R); everything a
    # reviewer READS about it is here. Five entries, and they are not
    # interchangeable:
    #
    #   question_section / question_label   the section heading and the radio
    #                                       label. Widget chrome, like
    #                                       `threshold_labels` below, so not in
    #                                       the subtitle registry.
    #   question_intro                      the one muted line under the radio.
    #                                       This IS a `.pma-card-subtitle` and
    #                                       IS registered and capped.
    #   question_labels                     the four radio choice labels. Prose
    #                                       questions, because the value the
    #                                       reviewer is choosing between is a
    #                                       question and naming it
    #                                       ("non-inferiority") alone assumes
    #                                       they already know which one it is.
    #   question_headings                   the Decision-threshold section
    #                                       heading, per question.
    #   question_threshold_units            the scale phrase for a MARGIN input
    #                                       label, per summary measure.
    #   question_help                       the body copy under the threshold
    #                                       input, per question. NOT registered
    #                                       and NOT capped - see the comment
    #                                       above EDU_COPY_SUBTITLE_FIELDS.
    #
    # The word "MID" appears in none of it, here or anywhere else a reviewer
    # reads (shiny/SPEC.md 4.5.1). The internals keep the name; the screen says
    # Threshold. test-step3-threshold.R holds the line over every string these
    # entries and the four helpers in R/step3_threshold.R can emit.
    question_section = "Clinical question",
    question_label   = "Which clinical question does this certainty rating answer?",
    question_intro = paste0(
      "The certainty rating answers the question you pick here. It decides ",
      "which threshold is read and how the Summary of Findings sentence is ",
      "worded."
    ),

    # The superiority label deliberately does not mention a threshold. On that
    # question a threshold is optional, and a label implying otherwise would
    # send a reviewer looking for a protocol value they do not need.
    question_labels = c(
      superiority = paste0(
        "Superiority - is there any effect at all?"
      ),
      important_superiority = paste0(
        "Clinically important superiority - is the effect large enough to ",
        "matter?"
      ),
      equivalence = paste0(
        "Equivalence - is the difference small enough to be unimportant in ",
        "either direction?"
      ),
      non_inferiority = paste0(
        "Non-inferiority - is the intervention no worse than the comparator ",
        "by more than a set amount?"
      )
    ),

    question_headings = c(
      superiority           = "Decision threshold (optional)",
      important_superiority = "Decision threshold",
      equivalence           = "Equivalence threshold",
      non_inferiority       = "Non-inferiority threshold"
    ),

    # The scale a MARGIN is entered on, per summary measure, and no example
    # value. `threshold_labels` below carries one ("e.g., 1.25") because a
    # threshold of clinical importance has a placeholder to illustrate; a
    # margin has none by design, and an example printed in its label is a
    # number a reviewer can read as a suggestion. "above 1" is the constraint
    # the internal conversion imposes: a ratio below 1 makes the threshold
    # negative on the TE scale and trips a gate.
    question_threshold_units = c(
      OR  = "as an odds ratio above 1",
      RR  = "as a risk ratio above 1",
      HR  = "as a hazard ratio above 1",
      RoM = "as a ratio of means above 1",
      SMD = "in standardized units",
      MD  = "in outcome units",
      ARD = "as an absolute risk difference"
    )
  ),

  # The per-question body copy under the threshold input. A separate top-level
  # entry rather than a sixth `config_tab$question_*` field, because
  # `important_superiority` is not a string: it is "whatever
  # `threshold_help[[sm]]` says", which is what keeps the default question
  # byte-identical to the pre-0.5.1 app. step3_threshold_copy() resolves that,
  # so nothing else has to know.
  #
  # `superiority` states the one thing about that route a reviewer cannot read
  # off the screen: an empty box is a complete answer, and a filled one is
  # still read - Core GRADE 2 Fig 2 switches the rating target to little or no
  # difference when the pooled estimate sits very near the threshold, and
  # Imprecision is then judged against it after all.
  #
  # The two margin entries end by quoting PMA_NO_MARGIN_PLACEHOLDER
  # (R/domain_imprecision.R) verbatim rather than paraphrasing it. That
  # sentence is the package's single statement of why neither margin gets a
  # suggested value, and it is cited by the two gates that refuse a margin
  # question with no margin - so the reason on screen and the reason in the
  # abort are one string.
  question_help = list(
    superiority = paste0(
      "The rating is against the null, so any effect counts and this box may ",
      "be left empty. If you do enter a threshold it is still read: when the ",
      "pooled estimate sits very near the null, Core GRADE 2 Fig 2 rates ",
      "certainty in little or no difference instead, and judges imprecision ",
      "against the threshold you entered."
    ),
    equivalence = paste0(
      "The largest difference you would still call unimportant. Both sides ",
      "are tested: the rating asks whether the confidence interval stays ",
      "inside the threshold, and rates down when the interval reaches past ",
      "it in either direction."
    ),
    non_inferiority = paste0(
      "The largest amount by which the intervention may be worse than the ",
      "comparator and still be acceptable. Only the worse side is tested: a ",
      "confidence interval running far past the threshold on the BETTER side ",
      "never rates down, and that is what separates this question from ",
      "equivalence."
    ),

    # Shared by the two margin questions and assembled LAST, after the
    # worse-side echo, because an instruction reads best at the end of the
    # note. One entry rather than a clause repeated in both, so a copy edit
    # cannot leave the two questions saying different things.
    margin_source = paste0(
      "Enter the margin your protocol specifies; there is no default for one."
    )
  ),

  threshold_labels = list(
    OR  = "Threshold (as OR ratio, e.g., 1.25 = 25 percent relative odds change)",
    RR  = "Threshold (as risk ratio, e.g., 1.20 = 20 percent relative risk change)",
    HR  = "Threshold (as hazard ratio, e.g., 1.20)",
    RoM = "Threshold (as ratio of means, e.g., 1.10)",
    SMD = "Threshold (in standardized units, e.g., 0.20 = Cohen's small)",
    MD  = "Threshold (in outcome units; default = 0.20 x pooled SD)",
    ARD = "Threshold (as absolute risk difference, e.g., 0.05 = 5 percent)"
  ),

  threshold_help = list(
    OR  = paste0("An OR of 1.25 vs 1.0 represents a 25 percent relative ",
                 "change in odds - a typical small but clinically ",
                 "meaningful effect."),
    RR  = "An RR of 1.20 vs 1.0 represents a 20 percent relative change in risk.",
    HR  = "An HR of 1.20 represents a 20 percent relative change in hazard.",
    RoM = paste0("A 10 percent ratio of means is a typical small clinically ",
                 "meaningful difference for continuous outcomes."),
    SMD = paste0("Cohen's small effect size (0.20) is widely accepted as ",
                 "the smallest clinically meaningful SMD."),
    MD  = paste0("Auto-suggested as 0.20 times the pooled SD (Cohen's small ",
                 "in raw units). Replace with a published threshold for your ",
                 "outcome whenever possible."),
    ARD = "A 5 percent absolute risk difference is a typical small clinically meaningful effect."
  ),

  # ----- Multiple outcomes -> one combined Summary of Findings table -----
  # A systematic review normally reports every patient-important outcome in
  # a single SoF table. This app rates one outcome at a time (Steps 2-3), and
  # banks each rating the moment its last certainty domain is confirmed. All
  # of these said "press Save" until 0.5.1; there is no Save button.
  #
  # `save_intro` was a fourth string, explaining the automatic save at the end
  # of Step 3's Final certainty tab. That whole section is gone: the saved rows
  # are listed on Step 4, beside the table they build, and the explanation
  # belongs where the rows are.
  multi_outcome = list(
    list_empty = paste0(
      "No outcomes saved yet. An outcome is saved once every certainty ",
      "domain is confirmed in Step 3."
    ),
    step4_intro = paste0(
      "One row per saved outcome. Reorder with the arrows below; mark the ",
      "outcomes your protocol prespecified as primary to group them."
    ),
    step4_empty = paste0(
      "No outcomes saved yet. Confirm every certainty domain in Step 3 and ",
      "the outcome is saved here automatically."
    )
  )
)

# ----- The one-line cap on a card subtitle --------------------------------
# A `.pma-card-subtitle` is the muted line under a control, and a reviewer
# reads it while deciding that control. Past 25 words it wraps to a second
# desktop line and stops being read at all, so it is capped: a sentence that
# cannot be said in one line was not answering the control it sat under, and
# belongs deleted rather than shortened.
EDU_COPY_SUBTITLE_WORD_CAP <- 25L

# Every EDU_COPY string the app renders INTO a `.pma-card-subtitle`, named
# explicitly rather than derived, because two groups of strings are
# deliberately not capped and a blanket rule could not tell them apart:
#
#   * `steps$*$title` / `$why` - a step header defines the step rather than
#     annotating a control, and `$why` is the Step 1 warning about doing the
#     review before the pooling, which is the one place length is the point;
#   * `threshold_labels`, `config_tab$question_section`,
#     `config_tab$question_label`, `config_tab$question_labels` and
#     `config_tab$question_threshold_units` - widget labels and headings, not
#     subtitles;
#   * `question_help$*` - the per-question body copy under the threshold
#     input. It renders through `.config_note()`, which does hang the
#     `.pma-card-subtitle` class on it, so this is the one exemption that
#     needs arguing rather than pointing at. The cap exists for a MUTED LINE
#     THAT ANNOTATES A CONTROL: past one desktop line it stops being read, and
#     an annotation nobody reads was not answering the control. These three
#     strings do not annotate the threshold input, they DEFINE THE QUESTION it
#     answers - which side of the threshold is tested, whether an empty box is
#     a complete answer, and why no value is offered. Delete-first cannot
#     apply: a reviewer cannot answer the control without them. Shorten-second
#     cannot either, because two of the three close by quoting
#     PMA_NO_MARGIN_PLACEHOLDER verbatim and trimming a quotation is
#     misquoting it. `config_tab$question_intro`, the one line that really
#     does annotate the radio, IS registered below and IS capped. The app has
#     long-form `.config_note()` body copy already - `PMA_RARE_NO_CC_NOTE` and
#     the two threshold notes in `output$threshold_panel` - and it is
#     uncapped for the same reason: none of it lives in this deck.
#
# `multi_outcome$*` was a third exemption, granted because a later phase owned
# the saved-outcome UI. That phase has happened (the Save button is gone), so
# the strings are capped like everything else.
#
# A new subtitle string belongs in this vector. Leaving it out is a decision,
# not an oversight, and the test names the file that will tell you so.
EDU_COPY_SUBTITLE_FIELDS <- c(
  "domains$indirectness$gradient",
  "domains$indirectness$population",
  "domains$indirectness$surrogate",
  "config_tab$continuous_departure",
  "config_tab$chinn_caveat",
  "config_tab$responder_default",
  "config_tab$question_intro",
  "multi_outcome$list_empty",
  "multi_outcome$step4_intro",
  "multi_outcome$step4_empty",
  paste0("threshold_help$", c("OR", "RR", "HR", "RoM", "SMD", "MD", "ARD"))
)

# One `a$b$c` path, resolved against EDU_COPY. NULL for a path that no longer
# exists, so the test reports a stale registry entry rather than erroring.
edu_copy_field <- function(path, copy = EDU_COPY) {
  for (key in strsplit(path, "$", fixed = TRUE)[[1L]]) {
    if (is.null(copy)) return(NULL)
    copy <- copy[[key]]
  }
  copy
}

# Words in a copy string, counted the way a reader sees them: whitespace-
# separated tokens.
edu_copy_word_count <- function(text) {
  words <- strsplit(trimws(as.character(text)), "\\s+")[[1L]]
  length(words[nzchar(words)])
}
