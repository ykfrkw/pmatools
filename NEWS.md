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
  plot body and the heterogeneity text for large numbers of studies.

## Branding

* Package-wide rebranding: titles, table footnotes, report headings, and
  documentation now describe the implementation as following the BMJ 2025
  Core GRADE series (Guyatt et al.). pmatools implements the Core GRADE
  series, which summarizes GRADE guidance; it is not an official GRADE
  Working Group tool. Function and argument names are unchanged.

# pmatools 0.3.4 and earlier

See the git history (<https://github.com/ykfrkw/pmatools>) for changes in
0.3.4 and earlier releases.
