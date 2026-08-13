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
      "<p><strong>Statistical pooling is only a small part of a systematic ",
      "review. A high-quality SR&amp;MA also needs a detailed, prespecified ",
      "and pre-registered protocol (e.g., on PROSPERO, OSF or other ",
      "platforms), a comprehensive search, dual independent screening and ",
      "data extraction, and risk-of-bias assessment - all completed BEFORE ",
      "the analysis.</strong></p>",
      "<p>This app handles the pooling and Core GRADE certainty steps; make ",
      "sure the upstream review work is in place first.</p>"
    )),
    dismiss = "Got it"
  ),

  pmid_url = function(pmid) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"),

  domains = list(

    rob = list(
      header   = "Risk of Bias",
      doi      = "10.1136/bmj-2024-083864",
      ref_text = "BMJ Core GRADE 4 (Guyatt et al., 2025)"
    ),

    inconsistency = list(
      header   = "Inconsistency",
      doi      = "10.1136/bmj-2024-081905",
      ref_text = "BMJ Core GRADE 3 (Guyatt et al., 2025)"
    ),

    indirectness = list(
      header   = "Indirectness",
      doi      = "10.1136/bmj-2024-083865",
      ref_text = "BMJ Core GRADE 5 (Guyatt et al., 2025)",
      # Sits beside the four subdomain questions, because it is the reason the
      # overall override below them exists.
      gradient = paste0(
        "Core GRADE 5 Table 2 ranks Population lowest and Outcome highest; ",
        "the worst-case fold used here is symmetric and ignores that ranking."
      ),
      # Beside the Outcome question. Both halves are judgments the reviewer
      # cannot read off the data: how far a surrogate warrants rating down,
      # and that it does not belong in the same analysis.
      surrogate = paste0(
        "A surrogate outcome is grounds to consider rating down - how far ",
        "depends on how closely it tracks the patient-important one. Never ",
        "pool the two."
      )
    ),

    imprecision = list(
      header   = "Imprecision",
      doi      = "10.1136/bmj-2024-081904",
      ref_text = "BMJ Core GRADE 2 (Guyatt et al., 2025)"
    ),

    pubias = list(
      header   = "Publication bias",
      doi      = "10.1136/bmj-2024-083864",
      ref_text = "BMJ Core GRADE 4 (Guyatt et al., 2025)"
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
    continuous_departure = paste0(
      "Only the responder proportion is offered, so Core GRADE 6's ",
      "agreement check is unavailable; read magnitude cautiously."
    ),
    chinn_caveat = paste0(
      "Chinn's formula, not Core GRADE 6's per-instrument procedure. The two ",
      "disagree, and the Summary of Findings table says so."
    ),
    responder_default = paste0(
      "The 20 percent starting value is an app convention. Replace it with a ",
      "rate from your data, or confirm it."
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
    OR  = "An OR of 1.25 vs 1.0 represents a 25 percent relative change in odds - a typical small but clinically meaningful effect.",
    RR  = "An RR of 1.20 vs 1.0 represents a 20 percent relative change in risk.",
    HR  = "An HR of 1.20 represents a 20 percent relative change in hazard.",
    RoM = "A 10 percent ratio of means is a typical small clinically meaningful difference for continuous outcomes.",
    SMD = "Cohen's small effect size (0.20) is widely accepted as the smallest clinically meaningful SMD.",
    MD  = "Auto-suggested as 0.20 times the pooled SD (Cohen's small in raw units). Replace with a published threshold for your outcome whenever possible.",
    ARD = "A 5 percent absolute risk difference is a typical small clinically meaningful effect."
  ),

  # ----- Multiple outcomes -> one combined Summary of Findings table -----
  # A systematic review normally reports every patient-important outcome in
  # a single SoF table. This app rates one outcome at a time (Steps 2-3);
  # saving each completed rating builds the multi-outcome table for Step 4.
  multi_outcome = list(
    save_intro = paste0(
      "A Summary of Findings table normally reports every patient-important ",
      "outcome of the review, one row per outcome. This app rates one ",
      "outcome at a time. When you are satisfied with the certainty rating ",
      "shown above, save it here, then press '+ Add next outcome': the app ",
      "returns to Step 2 with this outcome's answers cleared, ready for the ",
      "next one. Step 4 assembles every saved outcome into a single Summary ",
      "of Findings table."
    ),
    save_locked = paste0(
      "Saving is locked until every certainty domain has been reviewed and ",
      "confirmed. Provide inputs in each tab, or tick 'I have reviewed this ",
      "domain', then come back here."
    ),
    list_empty = paste0(
      "No outcomes saved yet. Saved outcomes stay in this session even when ",
      "you go back to Step 2 and rate a different outcome."
    ),
    step4_intro = paste0(
      "One row per saved outcome, using the certainty rating that was in ",
      "place when you saved it. Row order is a statement about priority, so ",
      "set it yourself with the arrows in the list below the table; mark the ",
      "outcomes your protocol prespecified as primary to group them under a ",
      "'Primary outcomes' heading, or mark none for an ungrouped table. The ",
      "single-outcome Evidence Profile and Summary of Findings for the ",
      "outcome currently open in Step 3 are exported as well."
    ),
    step4_empty = paste0(
      "No saved outcomes yet. Save a certainty assessment on the Step 3 ",
      "'Final certainty' tab to build a multi-outcome Summary of Findings ",
      "table. Without saved outcomes the export contains the single-outcome ",
      "Evidence Profile and Summary of Findings only."
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
# explicitly rather than derived, because three groups of strings are
# deliberately not capped and a blanket rule could not tell them apart:
#
#   * `steps$*$title` / `$why` - a step header defines the step rather than
#     annotating a control, and `$why` is the Step 1 warning about doing the
#     review before the pooling, which is the one place length is the point;
#   * `multi_outcome$*` - the saved-outcome UI, which a later phase owns;
#   * `threshold_labels` - widget labels, not subtitles.
#
# A new subtitle string belongs in this vector. Leaving it out is a decision,
# not an oversight, and the test names the file that will tell you so.
EDU_COPY_SUBTITLE_FIELDS <- c(
  "domains$indirectness$gradient",
  "domains$indirectness$surrogate",
  "config_tab$continuous_departure",
  "config_tab$chinn_caveat",
  "config_tab$responder_default",
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
