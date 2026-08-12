# pmatools 0.5.0

## Breaking changes

* Core GRADE 2 entry gate: `grade_meta()` gains `threshold_type`, which
  defaults to `"mid"` and makes a `threshold` (minimal important difference)
  mandatory. Calls without a MID now abort; the error quotes the value
  `suggest_threshold()` recommends for that effect measure. Use
  `threshold_type = "null"` to rate certainty in a true underlying effect, or
  `require_threshold = FALSE` to keep the previous MID-free behaviour.
* Imprecision now follows the Core GRADE 2 Fig 4 flowchart. The optimal
  information size is consulted only when the CI does **not** cross the chosen
  threshold and the effect is implausibly large; previously the
  "N <= 30% of OIS" rule forced a two-level rate-down regardless of where the
  CI sat. Analyses with a moderate effect, a CI clear of the threshold and a
  small sample size no longer rate down.
* Risk of bias now follows the Core GRADE 4 Fig 2 flowchart literally, and the
  **weight-share dominance gate is reinstated**. `rob_dominant_threshold` was
  deprecated in v0.3.1 ("accepted but ignored") on the reasoning that the
  zone-and-magnitude comparison subsumed it; **that decision is retracted**,
  because the gate is the first decision node of Fig 2 and the two branches
  below it are not interchangeable. The direction-of-bias check now runs only
  when high-RoB studies carry at least `rob_dominant_threshold` (default 0.60,
  compared with `>=`) of the inverse-variance weight. The non-dominated branch
  of the figure **never rates the domain down**; it decides which studies the
  analysis should use. Risk of bias therefore rates down noticeably less often
  than in v0.3.1–v0.4.0, and a body of evidence in which a minority of the
  weight is at high risk of bias can no longer be downgraded for it.
* When the flowchart reaches "use low risk of bias studies only", the
  meta-analysis is **refitted on the low-RoB subset** by default
  (`rob_refit = TRUE`). Every downstream domain, the rating target, the
  baseline risk and the SoF table then use the restricted estimate, so pooled
  numbers can change without any change to the input data. The refit is
  announced with a message, recorded in the Risk-of-bias notes, shown by
  `print()`, and footnoted in `sof_table()`. Set `rob_refit = FALSE` to keep
  the full analysis and receive the recommendation only.
* `export_bundle()` is now an S3 generic, so its **first argument is named
  `x`** rather than `ma`. Positional calls (`export_bundle(m, g, ...)`) are
  unaffected. Legacy named calls (`export_bundle(ma = m, grade = g, ...)`)
  still work but emit a deprecation warning once per session and will be
  removed in a future release; pass the object positionally or as `x =`.
* `grade_args$origin` in `export_bundle()` must now be one of `"null"`,
  `"column"`, `"scalar"` or `"vector"`. Any other value aborts instead of
  silently rendering the argument as `NULL` in the reproducibility script.

## New features

* Multi-outcome workflow: `run_ma_multi()` splits long-format data on its
  `outcome` column and runs one `run_ma()` per outcome (`sm` and
  `outcome_type` may be single values or named by outcome, so binary and
  continuous outcomes can share one session); `grade_meta_multi()` runs one
  `grade_meta()` per outcome from a `common` argument list plus per-outcome
  overrides, and returns the new `pmatools_set`. `run_ma()` itself is
  unchanged and still refuses data holding more than one outcome.
  An outcome that fails is recorded as `NULL` with a warning so the rest of
  the batch completes — **except** for the Core GRADE 2 entry gate
  (`threshold_type = "mid"` without a MID), which now aborts with condition
  class `"pmatools_threshold_gate"` and is re-raised unchanged, so a batch run
  cannot become a way around the gate.
* New `pmatools_set` class with `print()` / `summary()` methods listing each
  outcome's certainty, rating target and analysis set (a low-risk-of-bias
  refit is called out per outcome, and a set mixing analysis sets says so).
  `reorder_outcomes()` and `set_primary()` set the order and grouping, which
  drive both the Summary of Findings row order and the numbering of the export
  sub-directories.
* `grade_table()` and `grade_report()` accept a `pmatools_set` directly, using
  its order and primary outcomes; the named-list API is unchanged.
  `grade_report()` gains `style` (`"gradepro"` / `"bmj"`). In the BMJ style,
  per-outcome `follow_up` / `unit` recorded by `grade_meta_multi()` are picked
  up automatically, and a table mixing effect measures keeps a generic Effect
  header with a footnote pointing at the per-cell measure names.
* `export_bundle()` is now an S3 generic. The single-outcome ZIP is unchanged
  (`export_bundle(ma, grade, ...)`, flat layout — see Breaking changes for the
  first argument's rename); passing a `pmatools_set`
  writes the multi-outcome layout instead: `summary_of_findings.docx`/`.csv`,
  `evidence_profile.docx`, `analysis.R`, `data_long.csv` and `README.txt` at
  the top level, plus one `outcomes/NN_name/` directory per outcome (forest,
  RoB-stratified forest and funnel plots, results.txt, that outcome's data,
  its evidence profile, and `indirectness_table.docx` when subdomains were
  recorded; `forest_plot_full.*` only when the outcome was refitted on
  low-risk-of-bias studies). Directory names carry the set order as a numeric
  prefix; non-ASCII outcome names fall back to `outcome_NN`.
* The bundled `analysis.R` has a multi-outcome form that re-issues the
  `run_ma_multi()` / `grade_meta_multi()` / `reorder_outcomes()` /
  `set_primary()` calls with the arguments actually used, including every
  Phase A-C argument, and is syntax-checked before it is written.
* BMJ Core GRADE Summary of Findings layout: `sof_table()` and `grade_table()`
  gain `style = "bmj"` (the GRADEpro layout stays the default and is
  unchanged). The BMJ style presents outcome and follow-up, participants with
  the study design spelled out, the relative effect with its measure spelled
  out, a spanning "Absolute effects (95% CI)" block (control arm, intervention
  arm and a new **Difference** column, e.g. "88 fewer per 1000 (129 fewer to
  42 fewer)"), certainty with the domains that pulled it down, and a plain
  language summary. New arguments `follow_up` and `unit` supply the time frame
  and the unit of a continuous difference.
* Plain language summaries (**Core GRADE 6 Box 1**): the statements are carried
  verbatim and chosen from the certainty level, `threshold_type`,
  `rating_target` and the **direction of the pooled point estimate**. Core
  GRADE 6 Box 1 is the canonical source for summary of findings tables — it
  summarises the earlier Core GRADE 2 Table 1 guidance and adds the guidance
  specific to the null and MID thresholds. The practical consequence is that
  the statements name the direction of the effect on the outcome (`reduces` /
  `increases` / `has little to no effect`, e.g. "Treatment increases serious
  adverse events") instead of Core GRADE 2 Table 1's fixed "benefit" wording,
  which inverted the meaning of every harm outcome: an outcome with RR 2.42 for
  serious adverse events used to be summarised as "Treatment likely has an
  important benefit". Very low certainty now follows the Core GRADE 6 Table 1
  wording, "We are very uncertain about the effect of X on Y". Objects created
  before the Core GRADE 2 entry gate (no `$rating_target`) still simply omit
  the column, as do rows with no usable pooled estimate to take a direction
  from.
* The Core GRADE 4 analysis-set note now travels with every output. In
  `grade_table()` the refitted outcome's row carries a numbered footnote
  marker, so a table mixing analysis sets says which rows were restricted; the
  `grade_report()` outcome sections state it too.
* `grade_meta()` gains `rob_some_concerns` (`"low"`, default, or `"high"`):
  which side of the binary low/high classification studies rated "some
  concerns" are folded into. It changes the high-RoB weight share and
  therefore the dominance gate.
* `grade_meta()` gains `rob_overrides` / `rob_override_rationale`, named
  character vectors keyed on `studlab`, for correcting a single study's
  risk-of-bias level without rebuilding the whole vector. Every override needs
  a rationale and is recorded in the domain notes; a key that matches no study
  label aborts rather than being silently ignored.
* `grade_meta()` returns `$meta_full` (the all-studies analysis),
  `$rob_analysis_set` (`"all"` / `"low_only"`) and `$rob_refit`. `$meta` is now
  the analysis every domain was assessed on, which is the refitted one when a
  refit happened.
* Rating target (Core GRADE 2 Fig 2): `grade_meta()` derives the target of the
  certainty rating from the pooled point estimate
  (`"important_effect"` / `"little_to_no_difference"` / `"non_null_effect"`)
  and exposes it as `$rating_target`, `$rating_target_note`,
  `$rating_target_auto` and `$threshold_type`; `print()` shows it. The target
  decides which threshold Imprecision evaluates the CI against. Supplying
  `rating_target` manually overrides the derivation and requires
  `rating_target_rationale`.
* Indirectness subdomains (Core GRADE 5): `grade_meta()` gains
  `indirectness_subdomains`, a Population / Intervention / Comparison /
  Outcome table judged on the 4-point scale `"yes"` / `"probably_yes"` /
  `"probably_no"` / `"no"` (aliases such as `"Probably No"` accepted).
  `yes`/`probably_yes` do not rate down, `probably_no` rates down 1 level and
  `no` rates down 2; the domain judgment defaults to the worst case across
  subdomains, and a scalar `indirectness` may still override it with
  `indirectness_rationale`. The normalised table is returned as
  `$indirectness_subdomains` (`domain_assessments` keeps its one-row-per-domain
  schema).
* New `indirectness_table()` renders those subdomain judgments as a flextable
  in the BMJ Core GRADE 5 publication format: target question, evidence found,
  a colour-graded 4-option judgment row with the recorded answer ticked, and a
  merged "Judgment across subdomains" row carrying the overall judgment.
* Imprecision notes record which Fig 4 path produced the judgment, including
  the CI ratio rule for binary outcomes (relative risk CI ratio >= 3, odds
  ratio CI ratio >= 2.5) and the continuous 400-per-group (total N 800) rule
  of thumb.
* The reproducibility script in `export_bundle()` renders the new arguments,
  including the rationale for a manual target override, the risk-of-bias
  settings (`rob_some_concerns`, `rob_overrides`, `rob_override_rationale`,
  `rob_dominant_threshold`, `rob_refit`), and the full
  `indirectness_subdomains` table (with the scalar override only when it
  actually replaced the worst-case default).

# pmatools 0.4.0

## Breaking changes

* Manual domain-judgment overrides now require a rationale. Supplying a
  scalar `rob`, `indirectness` other than `"no"`, `inconsistency`,
  `imprecision`, or `pubias_funnel_asymmetry` to `grade_meta()` without the
  matching `*_rationale` argument is an error. Rationales are recorded in the
  domain notes and surfaced in `sof_table()`, `grade_report()`, and
  `export_bundle()` outputs (Core GRADE transparency principle).
* Summary of Findings vocabulary aligned with GRADEpro: column headers are now
  "Risk with <control>" / "Risk with <intervention>" (was "Control rate" /
  "Exp. rate") and "Certainty of the evidence (Core GRADE series)". String
  matching against the old headers will break.

## New features

* Absolute-scale clinical decision Threshold: `threshold_scale = "ard"` plus
  the new `threshold_baseline` argument convert an absolute risk difference
  Threshold to the ratio scale at a specified (or pooled) baseline risk, for
  OR/RR/HR/RoM effect measures.
* Manual `imprecision` override: a scalar imprecision judgment now bypasses
  the automated CI-vs-Threshold/OIS assessment entirely (with mandatory
  `imprecision_rationale`).
* Risk-of-bias direction-gate transparency: when the sensitivity shift
  exceeds the inflation threshold but the direction gate (bias-favouring
  shift check) blocks the downgrade, the RoB domain notes now say so
  explicitly, including the direction reasoning, so readers do not conclude
  the threshold was ignored.
* Forest plots use dynamic bottom spacing, eliminating overlap between the
  heterogeneity/test text and the x-axis band, "favors" labels, and axis
  titles; trim-and-fill and Peto annotations now render on dedicated rows.

## Branding

* Package-wide rebranding: titles, table footnotes, report headings, and
  documentation now describe the implementation as following the BMJ 2025
  Core GRADE series (Guyatt et al.). pmatools implements the Core GRADE
  series, which summarizes GRADE guidance; it is not an official GRADE
  Working Group tool. Function and argument names are unchanged.

# pmatools 0.3.4 and earlier

See the git history (<https://github.com/ykfrkw/pmatools>) for changes in
0.3.4 and earlier releases.
