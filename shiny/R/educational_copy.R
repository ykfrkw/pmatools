# educational_copy.R - American English educational copy for the wizard
#
# All user-facing text lives here as named lists, so that copy edits do not
# require touching the UI / server code. Values may be plain strings or
# templates (with `{{slot}}` placeholders) used by the result blocks.

EDU_COPY <- list(

  steps = list(
    step1 = list(
      title = "Step 1: Data",
      what  = paste0(
        "This step loads your study-level dataset and validates it. ",
        "The app accepts long-format data only: one row per study-arm pair, ",
        "or one row per study-outcome-arm pair when an outcome column is present. ",
        "Required columns include studlab, treat, n, and either event or mean/sd. ",
        "You can paste from ",
        "Excel, upload a .csv or .xlsx, or load the bundled sample dataset."
      ),
      why   = htmltools::HTML(paste0(
        "<strong>Statistical pooling is only a small part of a systematic ",
        "review. A high-quality SR&amp;MA also needs a detailed, prespecified ",
        "and pre-registered protocol (e.g., on PROSPERO, OSF or other platforms), ",
        "a comprehensive search, dual independent ",
        "screening and data extraction, and risk-of-bias assessment - all ",
        "completed BEFORE the analysis.</strong> ",
        "This app handles the pooling and Core GRADE certainty steps; make sure ",
        "the upstream review work is in place first."
      ))
    ),
    step2 = list(
      title = "Step 2: Meta-analysis",
      what  = paste0(
        "This step pools effect estimates across studies using the {meta} ",
        "R package. You choose the outcome type (binary or continuous), ",
        "the effect measure (e.g., OR, RR, SMD), the pooling method, and ",
        "the heterogeneity estimator. The forest plot visualizes study ",
        "estimates and the pooled effect; the funnel plot helps detect ",
        "small-study effects and possible publication bias."
      ),
      why   = NULL
    ),
    step3 = list(
      title = "Step 3: Certainty assessment (Core GRADE series)",
      what  = paste0(
        "This step rates the certainty of evidence: your confidence that ",
        "the estimate above reflects the true effect. Core GRADE starts at ",
        "High for randomized trials (or Low for observational studies) ",
        "and rates DOWN for concerns in five domains: Risk of Bias, ",
        "Inconsistency, Indirectness, Imprecision, and Publication Bias. ",
        "First set the decision threshold (it drives three of the five ",
        "domains), then work through each domain: it explains its ",
        "algorithm, shows the computed judgment, and lets you override if ",
        "your clinical judgment differs - a written rationale is required ",
        "for every manual override. You can move freely between tabs, but ",
        "the final certainty and the Step 4 export stay marked incomplete ",
        "until every domain has been reviewed and confirmed."
      ),
      why   = NULL
    ),
    step4 = list(
      title = "Step 4: Export",
      what  = paste0(
        "This step bundles every artifact you have generated into a single ",
        "ZIP - including a fully reproducible analysis.R script. Anyone ",
        "(including future-you) can re-run the analysis from the CSV and ",
        "the script alone, with library(pmatools). This is what makes the ",
        "work reproducible and citable."
      ),
      why   = NULL
    )
  ),

  pmid_url = function(pmid) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"),

  domains = list(

    rob = list(
      header   = "Risk of Bias",
      doi      = "10.1136/bmj-2024-083864",
      ref_text = "BMJ Core GRADE 4 (Guyatt et al., 2025)",
      # `how` is a function, not a string, because two of its inputs are live:
      # the sensitivity-analysis change threshold (quoted in rules 2 and 3)
      # and the reviewer's low/high boundary. Called from the Risk of Bias tab
      # with input$rob_inf_threshold and input$rob_some_concerns.
      how      = function(inflation_threshold = 0.10,
                          some_concerns_as = "high") {
        pct <- format(round(100 * inflation_threshold, 1),
                      trim = TRUE, scientific = FALSE)
        high_side <- identical(some_concerns_as, "high")
        paste0(
          "Each study is classified as low or high risk of bias. The binary ",
          "split is Core GRADE 4's (\"Core GRADE users can assess the ",
          "overall risk of bias in individual studies as low or high\"), but ",
          "the position of the boundary is not: Core GRADE 4 defines it by ",
          "counting high-risk items, uses three different counts in its ",
          "three worked examples, and leaves the choice open as one that ",
          "\"may be an issue that will be impossible to resolve\". Where the ",
          "boundary falls is therefore a review decision, set under 'Inputs ",
          "for this domain'. ",
          if (high_side) {
            paste0("It is currently set so that only studies explicitly ",
                   "rated low count as low: studies rated 'some concerns', ",
                   "studies rated high, and studies left unrated are all ",
                   "placed in the high-risk group. ")
          } else {
            paste0("It is currently set so that studies rated 'some ",
                   "concerns' count as low, together with studies rated low ",
                   "and studies left unrated; only studies rated high are ",
                   "placed in the high-risk group. ")
          },
          "The phrase 'some concerns' belongs to three-level tools such as ",
          "RoB 2 and does not appear in Core GRADE 4; this app keeps the ",
          "three-level input because reviewers assess with RoB 2, and folds ",
          "it onto whichever side the review decision selects. ",
          # The five rules, the three zones and the shape of the branch used
          # to be spelled out here at length. The flowchart under the verdict
          # draws all of that and shows which rule fired, so what is left is
          # only what a picture cannot say: what the zones are measured on,
          # where the number comes from, and what the source does and does
          # not license.
          "The two estimates the flowchart compares are the pooled estimate ",
          "from all studies (TE_all) and the inverse-variance pooled ",
          "estimate of the low risk-of-bias studies only (TE_low). Each is ",
          "placed in one of three zones set by +/-Threshold: above, trivial ",
          "(within +/-Threshold of the null), or below. ",
          "The ", pct, " percent figure quoted in rules 2 and 3 is the ",
          "sensitivity-analysis change threshold set below; of the five ",
          "rules only rule 3 consults it. It also decides, when the high ",
          "risk-of-bias studies do not dominate, whether the analysis is ",
          "restricted to the low risk-of-bias studies, so it governs both ",
          "rating down and restriction. ",
          "Provenance: the five rules are this app's, not Core GRADE 4's. ",
          "Figure 2 has a single node reading 'check direction of bias' and ",
          "does not enumerate how. Rule 5 rated down 2 up to pmatools 0.4 ",
          "and no longer does: Core GRADE 4 describes no automatic ",
          "two-level downgrade for risk of bias, every leaf of its Figure 2 ",
          "reading 'rate down' or 'do not rate down', so every automated ",
          "judgment here is capped at one level. Two levels remain ",
          "available as an explicit override with a written rationale, ",
          "below. ",
          "Note: the Threshold is set once in the Configuration tab ",
          "and shared with Inconsistency and Imprecision; one Threshold ",
          "drives all three domains."
        )
      }
    ),

    inconsistency = list(
      header   = "Inconsistency",
      doi      = "10.1136/bmj-2024-081905",
      ref_text = "BMJ Core GRADE 3 (Guyatt et al., 2025)",
      # The three steps used to be restated here in sequence. The flowchart
      # under the verdict draws them and shows which edge was taken, so this
      # keeps only the principle and the provenance of the numbers on it.
      how      = paste0(
        "Core GRADE rates down for inconsistency when there are IMPORTANT ",
        "differences in effect across studies AND those differences cannot ",
        "be explained. The BMJ Core GRADE 3 flowchart asks that in three ",
        "steps, drawn above. ",
        "Provenance of the numbers on the chart: none of them is Core ",
        "GRADE 3's. The source puts Step 1 to the reviewer as a visual ",
        "inspection of the forest plot, and words Step 2 as 'majority on ",
        "one side' against 'a substantial proportion on opposite sides', ",
        "quantifying neither. I-squared above 30 percent is the only figure ",
        "Core GRADE 3 offers, and it offers it grudgingly; the 80 percent ",
        "single-zone share follows CINeMA and the 20 percent each-side ",
        "share is this app's convention. ",
        "I-squared, tau-squared and the Q test are supportive context ",
        "only - beyond the Step 1 gate they do not drive the judgment."
      )
    ),

    indirectness = list(
      header   = "Indirectness",
      doi      = "10.1136/bmj-2024-083865",
      ref_text = "BMJ Core GRADE 5 (Guyatt et al., 2025)",
      how      = paste0(
        "Indirectness asks whether the evidence answers the question you are ",
        "actually asking. It cannot be computed from the data: it rests on ",
        "judgment about how far the trials depart from the target question. ",
        "Core GRADE 5 poses that judgment separately for each PICO element, ",
        "so this tab asks four questions - Population, Intervention, ",
        "Comparison, Outcome - and sends the answers to pmatools as the ",
        "indirectness subdomain table. They drive the domain judgment. ",
        "Answer scale and fold, both pmatools conventions rather than Core ",
        "GRADE 5 wording: each element is answered on a four-point scale - ",
        "yes, probably yes, probably no, no - to the question 'Is the ",
        "evidence sufficiently direct?'. Yes and probably yes contribute no ",
        "downgrade; probably no contributes one level (some concerns); no ",
        "contributes two levels (serious). The domain judgment is the worst ",
        "case across the four elements. ",
        "Disclosure: Core GRADE 5 Table 2 does NOT weigh the four elements ",
        "equally. It grades how likely each is to justify rating down as ",
        "Population low, Intervention intermediate, Comparison substantial, ",
        "and Outcome high. The worst-case fold used here is symmetric and ",
        "does not reproduce that gradient, so a 'probably no' on Population ",
        "counts exactly as much as one on Outcome. Weigh the elements ",
        "yourself, and use the overall override when the fold misplaces the ",
        "judgment (any override requires a written rationale). ",
        "A separate pmatools convention, the indirectness dominant threshold ",
        "of 0.55, applies only to per-study indirectness vectors - the ",
        "per-study editor on this tab - and never to the subdomain table, ",
        "which always folds worst-case. Core GRADE 5 gives no number here: it ",
        "operationalises indirectness of the body of evidence only ",
        "qualitatively, as evidence where 'all or almost all evidence comes ",
        "from' the indirect source."
      ),
      # Repeated next to the four subdomain questions, where the fold takes
      # effect, so the disclosure is not buried in the collapsed explanation.
      gradient = paste0(
        "Core GRADE 5 Table 2 grades the likelihood that each element ",
        "justifies rating down as Population low, Intervention intermediate, ",
        "Comparison substantial, Outcome high. pmatools folds these four ",
        "answers with a symmetric worst-case rule that does not reproduce ",
        "that gradient: the worst single answer decides, whichever element it ",
        "belongs to. The four-point answer scale and the wording 'Is the ",
        "evidence sufficiently direct?' are pmatools conventions and do not ",
        "appear in the Core GRADE 5 article body."
      ),
      mapping = paste0(
        "How these answers become a judgment (pmatools mapping, stated here ",
        "rather than implied): yes and probably yes contribute no downgrade; ",
        "probably no contributes one level (recorded as 'some concerns'); no ",
        "contributes two levels (recorded as 'serious'). The domain takes the ",
        "worst case across the elements you answer. Unanswered elements are ",
        "omitted from the table rather than assumed direct."
      ),
      # Placed next to the Outcome question. Every claim here is attributed:
      # the two-level gradient and the three exceptions are the source's, the
      # no-pooling recommendation is this app's.
      surrogate = paste0(
        "Surrogate outcomes. Core GRADE 5 names surrogate rather than ",
        "patient-important outcomes as one of three situations - alongside ",
        "non-adherence to interventions and problematic comparators - that ",
        "warrant considering a downgrade even on an ordinary search for ",
        "direct evidence. The source does not discuss prespecification, so ",
        "'we planned to use the surrogate' is not an exemption: choosing not ",
        "to rate down can still be defended, but it is a judgment to be ",
        "argued rather than a default, and the interpretation needs care. ",
        "How far to rate down is the source's: 'the decision to rate down one ",
        "or two levels depends on one's understanding of the likelihood that ",
        "change in the patient important outcome will follow change in the ",
        "surrogate' - a distant surrogate warrants more than a close one. ",
        "This app's recommendation, explicitly NOT a Core GRADE rule: do not ",
        "combine surrogate and patient-important outcomes in one ",
        "meta-analysis. Whether they may be pooled is not addressed anywhere ",
        "in Core GRADE. The framework asks the indirectness question per ",
        "outcome, which implicitly makes them separate rows, but there is no ",
        "statement to cite. Rate them as separate outcomes and give each its ",
        "own Summary of Findings row."
      ),
      banner   = paste0(
        "No indirectness judgment recorded yet. Answer the four Core GRADE 5 ",
        "subdomain questions below (or set an overall rating, or explicitly ",
        "confirm the domain); the certainty rating stays marked incomplete ",
        "until then."
      )
    ),

    imprecision = list(
      header   = "Imprecision",
      doi      = "10.1136/bmj-2024-081904",
      ref_text = "BMJ Core GRADE 2 (Guyatt et al., 2025)",
      # The Yes/No walk through Figure 4 used to be restated here. The
      # flowchart under the verdict draws it and lights up the route taken;
      # what remains is the two things the picture cannot carry - how the OIS
      # is parameterised, and where each number comes from.
      how      = paste0(
        "Imprecision follows Core GRADE 2 Figure 4, drawn above. Two ",
        "consequences of its shape are easy to miss. Sample size is not ",
        "considered at all when the confidence interval crosses the ",
        "threshold: the Optimal Information Size is never consulted on that ",
        "path. And the OIS is reached only when the interval does NOT cross ",
        "the threshold and the effect is implausibly large. The pre-0.5 rule ",
        "that a total sample below 30 percent of the OIS forced two levels ",
        "unconditionally has been removed. ",
        "Figure 4's second two-level condition on the crossing path - ",
        "whether the plain language summary warrants 'may' rather than ",
        "'likely' - is a reviewer judgment about wording and is not assessed ",
        "automatically; apply it through the override below. ",
        "The OIS itself is parameterised differently for the two outcome ",
        "types, and for binary outcomes it is NOT parameterised by the ",
        "threshold. Core GRADE 2, verbatim: 'For binary outcomes, these ",
        "involve specifying the acceptable error rates: alpha (typically ",
        "0.05) and beta (typically 0.20), the control group event rate ",
        "(chosen from the context), and a modest relative risk reduction, ",
        "typically 20% or 25%.' The same paragraph directs continuous ",
        "outcomes to the threshold instead. Core GRADE's separate statement ",
        "that binary thresholds belong on the absolute scale is about ",
        "thresholds, not about the OIS; the two are different quantities and ",
        "do not conflict. The comparison is against participants, not ",
        "events - Figure 4's caption reads 'N=number of participants', and ",
        "the body reads 'If the total sample size of all the studies included ",
        "in a meta-analysis exceeds the OIS, one does not rate down.' ",
        "Provenance of the numbers. 'Implausibly large' for binary outcomes ",
        "is the source's: certainly a relative risk reduction above 40 ",
        "percent, possibly above 30 percent. For continuous outcomes the ",
        "source gives no definition of a large effect, so pmatools uses ",
        "Cohen's convention of a standardized effect of 0.8 or more - a ",
        "pmatools operational choice, not a Core GRADE number. ",
        "The Optimal Information Size approach is described in Guyatt et al. ",
        "(2011) GRADE guidelines 6: rating the quality of evidence - ",
        "imprecision (J Clin Epidemiol 64:1283-93)."
      )
    ),

    pubias = list(
      header   = "Publication bias",
      doi      = "10.1136/bmj-2024-083864",
      ref_text = "BMJ Core GRADE 4 (Guyatt et al., 2025)",
      # Q1 to Q4 used to be walked through one by one here. The flowchart
      # under the verdict draws them, with the pmatools registry node marked
      # as the addition it is, and lights up the route taken. What is left is
      # the provenance, the app-level behaviour the package figure does not
      # cover, and what the reference plots are for.
      how      = paste0(
        "Core GRADE 4 Figure 5 has exactly four decision nodes and no ",
        "entry-level rule-out. pmatools follows them in the source's order, ",
        "and the headings on this tab use the same Q1 to Q4 numbering as the ",
        "chart above. Q2 is evaluated automatically from the number of ",
        "studies contributing a usable estimate. ",
        "Provenance: Figure 5 asks the Q3 asymmetry question qualitatively ",
        "and names no p-value, so the operational cut-off of p < 0.05 used ",
        "here is a pmatools convention, not a Core GRADE criterion. There is ",
        "no second tier: Core GRADE 4 never rates down two levels for ",
        "publication bias, so the two-tier Egger rule pmatools applied ",
        "before 0.5 was removed and the judgment is now at most one level ",
        "down either way. ",
        "The registry input - 'Overall, does the situation argue against ",
        "reporting bias?', drawn dashed on the chart - is likewise NOT a ",
        "node of Figure 5. pmatools 0.5 moved its effect to after Q1 rather ",
        "than treating it as an entry rule-out, so a body of small ",
        "industry-sponsored trials still rates down even when registry ",
        "coverage is asserted to be complete. 'Yes' then short-circuits the ",
        "domain to no rate down; 'No' is an app-level rule that forces rate ",
        "down 1 regardless of Q2-Q4, and is the one branch the chart does ",
        "not draw, because it exists only in this app and not in the ",
        "pmatools package. ",
        "The funnel plots, the trim-and-fill plot and summary, and the ",
        "available-versus-missing-results subgroup forest are reference ",
        "materials and do not drive the Core GRADE judgment."
      )
    )
  ),

  # ----- Configuration tab (formerly "Decision threshold") ----------------
  # Everything the five certainty domains depend on is established here, in
  # the order a reviewer needs to decide it: the control-group risk first
  # (because the absolute threshold is only interpretable against it), then
  # the threshold itself, then how the effect is presented.
  config_tab = list(
    intro = paste0(
      "This tab settles everything the five certainty domains depend on. ",
      "The decision threshold is the smallest effect that would be ",
      "clinically meaningful - the effect size at which you would change ",
      "a management decision. It plays a cross-cutting role in Core ",
      "GRADE: Risk of Bias asks whether restricting to low-risk-of-bias ",
      "studies moves the estimate across the threshold; Inconsistency ",
      "asks whether study estimates fall on opposite sides of it; and ",
      "Imprecision uses it as the target effect for the Optimal ",
      "Information Size and confidence-interval judgments. Set it once ",
      "here - the domain tabs display it read-only. For binary outcomes ",
      "Core GRADE recommends thinking on the absolute scale (events per ",
      "1,000 patients), which is the default below, so the control-group ",
      "risk that converts it to the analysis scale is asked for first."
    ),
    # Core GRADE 6 ranks three presentations of a continuous outcome: (1) the
    # mean difference read against the threshold, (2) conversion to a
    # proportion of responders, (3) the SMD. It recommends presenting 1 and 2
    # TOGETHER. This app offers them as an either/or (input$sof_presentation),
    # so the agreement check is still out of reach; that departure is stated
    # on screen rather than left implicit.
    continuous_intro = paste0(
      "Core GRADE 6 ranks three ways of presenting a continuous outcome: ",
      "the mean difference interpreted against the threshold; conversion ",
      "to a proportion of patients who respond; and the standardized mean ",
      "difference, which it calls often the least satisfactory and ",
      "reserves for outcomes reported on multiple scales with no credible ",
      "threshold available for any single instrument. It recommends ",
      "presenting the first two together, so that agreement between them ",
      "licenses strong inferences about magnitude and disagreement forces ",
      "weaker ones."
    ),
    continuous_departure = paste0(
      "Departure from the source, stated here rather than left implicit: ",
      "the two presentations are offered here as a choice, one or the other, ",
      "not side by side in one table as Core GRADE 6 recommends. The ",
      "agreement check between them therefore cannot be performed here, and ",
      "inferences about the magnitude of effect should be correspondingly ",
      "weaker. Neither choice changes the certainty rating: the rating reads ",
      "the decision threshold above, on the scale the outcome was analysed ",
      "on."
    ),
    chinn_caveat = paste0(
      "The responder conversion used here is not Core GRADE 6's option 2. ",
      "That procedure needs a threshold for each instrument, assumes a ",
      "normal distribution, and computes the proportion above threshold ",
      "per study before pooling. Reviews that pool across instruments and ",
      "across endpoint and change definitions cannot supply per-instrument ",
      "thresholds, so the app applies Chinn's formula instead: it assumes ",
      "a logistic latent variable, requires no threshold, and is applied ",
      "to the pooled standardized mean difference. The two approaches do ",
      "not generally agree, and the Summary of Findings table carries a ",
      "footnote saying so."
    ),
    responder_default = paste0(
      "Unconfirmed assumption. The 20 percent (200 per 1,000) starting ",
      "value is an app convention, not a Core GRADE or pmatools number: ",
      "Core GRADE 6 only says the control-group rate is chosen from the ",
      "context. Replace it with a rate from your own data or from an ",
      "external source, or confirm it explicitly, before the rating is ",
      "used."
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

# The one supported way to read a certainty domain's "How is this judged?" copy.
#
# Most domains store `how` as a finished string. Risk of Bias stores a function
# instead, because two of the numbers its copy quotes are live: the
# sensitivity-analysis change threshold, and where the reviewer put the low /
# high boundary. That left `how` with two shapes and no way to tell them apart
# without looking, so every caller had to know which domain it was reading --
# `pma_how_collapse(EDU_COPY$domains$inconsistency$how)` for four of them and
# `EDU_COPY$domains$rob$how(a, b)` for the fifth. Turning any other domain's
# copy into a template would then have silently rendered a closure into the
# page.
#
# The contract is this accessor: give it the domain key plus whatever arguments
# that domain's copy interpolates, and get a character string back. Arguments
# are ignored (not an error) for a domain whose copy is already a string, so a
# call site does not have to change when a domain gains or loses a slot.
edu_domain_how <- function(domain, ...) {
  how <- EDU_COPY$domains[[domain]]$how
  if (is.function(how)) how(...) else how
}
