# pmatools (development version)

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

## New features

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
  including the rationale for a manual target override and the full
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
