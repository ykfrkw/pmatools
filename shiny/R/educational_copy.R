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
      how      = paste0(
        "pmatools applies a single MECE 5-rule decision that operationalises ",
        "the BMJ Core GRADE 4 Figure 2 question: would risk of bias change ",
        "the clinical conclusion? The pooled estimate from all studies ",
        "(TE_all) and the IV-weighted pooled estimate of low / some-RoB ",
        "studies only (TE_low) are each classified into one of three zones ",
        "defined by +/-Threshold: above (TE > +Threshold), trivial (within ",
        "+/-Threshold of the null), or below (TE < -Threshold). Decision: ",
        "(1) both in the trivial zone -> no rate down; (2) same non-trivial ",
        "zone with relative inflation <= 10 percent (or in the bias- ",
        "deflating direction) -> no rate down; (3) same non-trivial zone ",
        "with bias-favouring inflation > 10 percent -> rate down 1; (4) ",
        "zones differ but on the same side of the null -> rate down 1; (5) ",
        "zones differ across the null (above <-> below) -> rate down 2. ",
        "Note: the Threshold is set once in the 'Decision threshold' tab ",
        "and shared with Inconsistency and Imprecision; one Threshold ",
        "drives all three domains."
      )
    ),

    inconsistency = list(
      header   = "Inconsistency",
      doi      = "10.1136/bmj-2024-081905",
      ref_text = "BMJ Core GRADE 3 (Guyatt et al., 2025)",
      how      = paste0(
        "Core GRADE rates down for inconsistency when there are IMPORTANT ",
        "differences in effect across studies AND those differences ",
        "cannot be explained. The BMJ Core GRADE 3 flowchart asks three ",
        "questions in sequence. Step 1: are there important differences ",
        "in point estimates AND limited overlap of confidence intervals? ",
        "If no, do not rate down. If yes, continue. Step 2: where do ",
        "point estimates fall relative to the clinical decision threshold? ",
        "If a clear majority sits on one side of the threshold, ",
        "the direction of effect is consistent - do not rate down. If a ",
        "substantial proportion fall on opposite sides, continue. Step 3: ",
        "can the opposite-sided difference be explained by a credible ",
        "subgroup analysis? If yes, present the subgroups separately and ",
        "do not rate down; if no, rate down for serious inconsistency. ",
        "I-squared, tau-squared, and Q-test statistics are supportive ",
        "context only - they do not drive the judgment."
      )
    ),

    indirectness = list(
      header   = "Indirectness",
      doi      = "10.1136/bmj-2024-083865",
      ref_text = "BMJ Core GRADE 5 (Guyatt et al., 2025)",
      how      = paste0(
        "Indirectness CANNOT be automated - it requires expert judgment ",
        "about whether the trial evidence applies to the question of ",
        "interest. Core GRADE asks you to consider four things. Population: ",
        "do trial participants resemble the target patients? ",
        "Intervention: is the intervention deliverable as studied? ",
        "Comparator: is it representative of usual care? Outcome: is it ",
        "patient-important, or a surrogate? The app does not preselect a ",
        "rating: this is the only domain whose value comes purely from ",
        "your judgment, so choose a rating yourself before exporting ",
        "(any rating other than 'No' requires a written rationale)."
      ),
      banner   = paste0(
        "No rating selected yet - please choose one below. This is the ",
        "only domain that cannot be informed by your data; the certainty ",
        "rating stays marked incomplete until you select a rating (or ",
        "explicitly confirm the domain)."
      )
    ),

    imprecision = list(
      header   = "Imprecision",
      doi      = "10.1136/bmj-2024-081904",
      ref_text = "BMJ Core GRADE 2 (Guyatt et al., 2025)",
      how      = paste0(
        "Imprecision asks whether the pooled estimate's 95 percent ",
        "confidence interval is narrow enough to support a clinical ",
        "decision. The algorithm checks two conditions. (a) Does the ",
        "pooled 95 percent CI cross the null? (b) Is the Optimal ",
        "Information Size (OIS) met? OIS is the sample size a single ",
        "well-powered RCT would need to detect the threshold for clinical ",
        "importance - the same threshold used in Inconsistency. If both ",
        "conditions are ",
        "met (CI does not cross null AND OIS reached), no downgrade. ",
        "If only one fails, rate down 1 level. If both fail, rate down ",
        "2 levels. The Optimal Information Size approach is described in ",
        "Guyatt et al. (2011) GRADE guidelines 6: rating the quality of ",
        "evidence - imprecision (J Clin Epidemiol 64:1283-93)."
      )
    ),

    pubias = list(
      header   = "Publication bias",
      doi      = "10.1136/bmj-2024-083864",
      ref_text = "BMJ Core GRADE 4 (Guyatt et al., 2025)",
      how      = paste0(
        "pmatools follows the BMJ Core GRADE 4 Figure 5 flowchart with an ",
        "overall judgment gate (Q-pre) added on top. ",
        "Q-pre (overall judgment): based on a paraphrased list of conditions ",
        "for suspected vs denied reporting bias, the reviewer makes a single ",
        "overall call. 'Yes - denied' short-circuits to 'no rate down'; ",
        "'No - suspected' forces rate down 1 (some concerns); leaving Q-pre ",
        "blank falls through to the algorithmic Q1-Q4 path below. ",
        "Q1: are most or all studies small AND industry-sponsored? Yes -> ",
        "rate down 1. ",
        "Q2: is statistical analysis feasible (k >= 10)? Auto-evaluated. ",
        "Q3 (k >= 10): the contour-enhanced funnel plot is shown together ",
        "with Egger's linear regression. p < 0.01 -> rate down 2 (serious); ",
        "0.01 <= p < 0.05 -> rate down 1 (some concerns); p >= 0.05 -> no ",
        "rate down. The reviewer can override Egger with a visual judgment. ",
        "A trim-and-fill funnel plot and numerical summary are shown as ",
        "reference materials but do not drive the Core GRADE judgment. ",
        "Q4 (k < 10): Egger is unreliable, so the algorithm asks whether ",
        "unpublished studies are documented in registries or regulatory ",
        "databases. Yes -> rate down 1."
      )
    )
  ),

  threshold_tab = list(
    intro = paste0(
      "The decision threshold is the smallest effect that would be ",
      "clinically meaningful - the effect size at which you would change ",
      "a management decision. It plays a cross-cutting role in Core ",
      "GRADE: Risk of Bias asks whether restricting to low-risk-of-bias ",
      "studies moves the estimate across the threshold; Inconsistency ",
      "asks whether study estimates fall on opposite sides of it; and ",
      "Imprecision uses it as the target effect for the Optimal ",
      "Information Size and CI judgments. Set it once here - the domain ",
      "tabs display it read-only. For binary outcomes Core GRADE ",
      "recommends thinking on the absolute scale (events per 1,000 ",
      "patients), which is the default below."
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
      "shown above, save it here: then go back to Step 2, select the next ",
      "outcome, work through Step 3 again, and save that one too. Step 4 ",
      "assembles every saved outcome into a single Summary of Findings table."
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
      "place when you saved it. Mark the outcomes that your protocol ",
      "prespecified as primary to group them under a 'Primary outcomes' ",
      "heading; leave the field empty for an ungrouped table. The ",
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
