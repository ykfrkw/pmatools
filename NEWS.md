# pmatools 0.5.1 (development version)

## New features

* The Risk of bias, Inconsistency and Imprecision assessors now record the
  numbers behind their judgment in a structured form, reachable with the new
  exported `domain_facts(x, domain = NULL)` and stored on the rated object as
  `$domain_facts`: a tibble per domain with a stable machine `key`, a human
  `label`, a pre-formatted `value` and the raw `numeric` when the fact is
  scalar-numeric. Risk of bias records the high-risk study count and weight
  share, the pooled estimate with and without those studies, and which Core
  GRADE 4 Fig 2 branch was taken; Inconsistency records I-squared, tau-squared,
  the Cochran Q p value and the zone tally against the chosen threshold;
  Imprecision records the confidence interval, whether it crosses the null and
  where it sits relative to the threshold, the optimal information size, and
  the Core GRADE 2 Fig 4 path with a flag for whether the OIS approach was
  applied. This replaces regex-parsing `domain_assessments$notes` — a host
  application that wanted the Fig 4 path had to strip it back out of a sentence
  — with values it can read and compute with. `notes` is unchanged, down to the
  byte: the facts are a machine-readable companion to the prose, not a
  replacement for it, and the prose remains the authoritative record of why a
  domain was rated the way it was. Indirectness and Publication bias keep
  prose-only notes; the container is domain-agnostic, so they can adopt it
  later without a change to the accessor or the renderers.
* `sof_table()`, `grade_table()` (both `"gradepro"` and `"bmj"` layouts) and
  `evidence_profile()` render those facts as numbered footnotes for the domains
  that pulled the rating down, with the marker on the certainty cell — after
  the symbol in the GRADEpro layouts, and beside the domain name inside the BMJ
  "Due to serious risk of bias [1] ..." sentence. A reader can now see *why* a
  rating fell without opening the notes. In `grade_table()` the domain-fact
  footnotes continue the same `[n]` register as the per-outcome analysis-set
  notes and name the outcome they belong to, so one footer never shows two
  different `[1]`s; the analysis-set and publication-bias sentences keep their
  existing numbering and wording.

* `export_bundle()` takes a `style` argument on both methods, so a caller can
  export the Summary of Findings layout it renders on screen. Previously
  `export_bundle.meta()` had no such argument and always wrote the GRADEpro
  layout; a host application that showed the BMJ Core GRADE table could only
  match it by withholding `"grade_table"` from `include` and writing
  `sof_table.docx` itself. `style` is forwarded to `sof_table()` for
  `sof_table.docx` and to `grade_report()` for the certainty appendix, so a
  bundle no longer mixes two layouts, and it is rendered into the generated
  `analysis.R` — the script now regenerates the table the bundle actually
  ships. The same fix applies to `grade_table()` in the multi-outcome script.
* `export_bundle.meta()` also takes `follow_up` and `unit`, the two
  presentation arguments of the BMJ layout ("Outcome and follow-up" is its
  first column). Both fall back to `grade$follow_up` / `grade$unit`, which is
  where `grade_meta_multi()` records them, so an outcome rated as part of a set
  keeps its follow-up line when exported on its own; both are rendered into
  `analysis.R`. The `pmatools_set` method needs no such argument:
  `grade_table()` reads them off the rated objects, and so does the generated
  script.

* New exported `sof_add_notes(x, notes)`: appends caller footnote lines to a
  `sof_table()` / `grade_table()` flextable, styled like the footnotes those
  functions write themselves. Both `export_bundle()` methods take the matching
  `sof_notes` argument — appended to `sof_table.docx` and to
  `summary_of_findings.docx` respectively, and rendered into `analysis.R` as a
  `sof_add_notes()` call, so the script still reproduces the table that was
  exported. This is the last thing that forced a host application to write the
  SoF .docx outside `export_bundle()`: an annotation pmatools cannot derive (a
  rare-event alert, a scope caveat, a registration number) can now be handed to
  the bundler. `sof_notes` does not reach the certainty appendix.

## Behaviour changes

* The default `style` of the single-outcome bundle changed from GRADEpro to
  `"bmj"`, matching `export_bundle.pmatools_set()`, which has defaulted to the
  BMJ layout since v0.5.0. One rule now holds for both: a bundle ships the
  Core GRADE layout unless asked otherwise. `sof_table()` and `grade_table()`
  are unchanged and still default to `"gradepro"`, so only the exported
  `sof_table.docx` (and the appendix's embedded table) moves. Pass
  `style = "gradepro"` to `export_bundle()` to keep the old layout.

## Bug fixes

* `export_bundle()` read its `grade_args` and `ma_args` specifications with
  `$`, which partial-matches: a bundle carrying only an
  `inconsistency_ci_diff` spec had that spec answer for `inconsistency` as
  well, so the generated `analysis.R` issued a manual Inconsistency override
  the reviewer never made — and re-running the "reproducible" script could
  return a different certainty than the bundle it came in. Every one of the
  ~40 affected lookups (`rob`/`rob_rationale`, `imprecision`/
  `imprecision_rationale`, `threshold`/`threshold_scale`,
  `rating_target`/`rating_target_rationale`, `run_ma()`'s `method`/
  `method.tau`, and the rest) now uses exact `[[` indexing. `grade_args`
  names are additionally checked against `grade_meta()`'s formals at render
  time: an unknown name — a typo such as `inconsistancy`, or an argument
  belonging to another function — now aborts with the closest legal name
  rather than being silently dropped from the script.
* The single-outcome `analysis.R` template had no `threshold_baseline` slot,
  so a rating made with an absolute (ARD) threshold anchored to a
  reviewer-supplied baseline generated a script that re-derived the baseline
  from the pooled control-arm risk instead. On a meta-analysis whose pooled
  control risk is 0.33 and whose reviewer-set baseline was 0.12, the
  regenerated rating used a threshold on the internal scale of 0.22 where the
  bundle's was 0.41. The generated call now passes the resolved
  `threshold_baseline` the rating was actually made with. The multi-outcome
  template was never affected: it literalises `common`/`per_outcome`
  wholesale.
* Baseline (control-arm) risk: `event.c` and `n.c` were filtered with different
  predicates, so a study reporting a denominator but no event count — one that
  contributed a continuous outcome only, say — was dropped from the numerator
  while its controls stayed in the denominator. Two consequences, both silent.
  The crude pooled proportion was diluted by controls that could never
  contribute an event; and the two vectors reached `meta::metaprop()` at
  different lengths, so `baseline_risk = "metaprop"` errored, warned, and
  returned that same crude proportion in place of the random-effects estimate
  it advertises. On the bundled `cbti_depression` dataset the pooled baseline
  risk was reported as 173.8 per 1,000 where the metaprop estimate is 155.6 and
  the crude proportion is 175.5 — a gap wide enough to move the absolute-risk
  column of a Summary of Findings table. Both vectors now use one complete-case
  filter (`!is.na(event.c) & !is.na(n.c) & n.c > 0`), and a meta object with no
  complete control arm returns `NULL` rather than `NaN`. Analyses that left
  `baseline_risk` at its default and had at least one such study will see the
  absolute risks move.
* `suggest_threshold()` returned `NULL` for a risk-difference meta-analysis:
  its `switch` handled the internal scale name `"ARD"` but not `"RD"`, which is
  what `meta::metabin(sm = "RD")` actually reports (`"ARD"` is not a {meta}
  effect measure, so that branch was unreachable). `"RD"` now yields the
  absolute 0.05 suggestion, and `threshold_to_te_scale(threshold_scale =
  "auto")` resolves it to the `"ard"` scale instead of aborting. Note that
  `run_ma()` does not emit `sm = "RD"`; this affects meta objects built
  directly with {meta}, which the risk-difference paths in the SoF and
  Imprecision code already support.
* Risk of bias: the whole flowchart works in "estimable study" space (length
  `meta_obj$k`), but the refit on the low risk of bias subset and the
  study-level `rob_overrides` work in study-label space (length
  `meta_obj$studlab`). The two differ whenever {meta} drops a study from the
  pool — a trial with missing results, or a double-zero trial under
  `method = "Inverse"` — and both collaborators then refused to run: the
  Core GRADE 4 Fig 2 "use low risk of bias studies only" leaf came back with
  `rob_refit = FALSE` and a "does not align with the meta object" warning, and
  `rob_overrides` aborted with a study-label count that could never match. The
  two spaces are now mapped onto each other explicitly, so the refit happens
  and the overrides apply. `attr(<rob domain row>, "high_idx")` is
  consequently study-label aligned, which is also what `update.meta(subset = )`
  needs; when the alignment genuinely cannot be established (no study labels,
  or no rule that reproduces `k` rows) nothing is guessed and the previous
  skip-with-a-warning behaviour stands.
* `assess_rob()` now accepts a per-study `rob` vector of length `k` **or** of
  length `length(meta_obj$studlab)`; the second form lets a reviewer keep one
  row per study in the data even when {meta} could not pool them all, and it is
  what a `rob` column name in `meta_obj$data` yields. The length-mismatch error
  names both accepted lengths. A study {meta} could not pool never counts as
  high risk of bias unless the reviewer rated (or overrode) it as such.

# pmatools 0.5.0

0.5.0 rebuilds all five certainty domains on the BMJ 2025 Core GRADE flowcharts
(Core GRADE 2–5), adds the Core GRADE 2 entry gate and rating target, a
multi-outcome workflow, and the BMJ Summary of Findings layout with Core GRADE 6
plain language summaries. Several domains rate down **less** often than in
0.4.0; re-running an existing analysis can therefore change its certainty
rating without any change to the input data. Read the breaking changes first.

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
* Imprecision can reach two levels on the **null-threshold** path as well. Core
  GRADE 2 (p6): "The two considerations also apply to imprecision judgments
  when Core GRADE users choose the null as the threshold of interest ... The
  finding that the CI is consistent with both benefit and important harm
  motivates a plain language summary stating that the intervention 'may' result
  in a benefit, and rating down two levels for imprecision." The −1 / −0
  decision still turns on the null; only the two-level branch consults the MID,
  so without a MID the judgment stops at −1.
* The **binary** optimal information size is no longer parameterised from the
  MID. Core GRADE 2 reserves the MID for continuous outcomes and asks binary
  OIS for "a modest relative risk reduction, typically 20% or 25%": the new
  `ois_rrr` (default `0.20`) gives `ois_p1 = ois_p0 * (1 - ois_rrr)`. An
  explicit `ois_p1` still wins. The comparison is now in **participants**
  (`sum(n.e) + sum(n.c)`) rather than events, following the Fig 4 caption
  ("N=number of participants"); the implied event count is reported in the
  notes, and supplying `ois_events` explicitly keeps the event-based
  comparison. Continuous outcomes are unchanged and still use the MID
  (`ois_delta`).
* Risk of bias now follows the Core GRADE 4 Fig 2 flowchart literally, and the
  **weight-share dominance gate is reinstated**. `rob_dominant_threshold` was
  deprecated in v0.3.1 ("accepted but ignored") on the reasoning that the
  zone-and-magnitude comparison subsumed it; **that decision is retracted**,
  because the gate is the first decision node of Fig 2 and the two branches
  below it are not interchangeable. The direction-of-bias check now runs only
  when high-RoB studies carry at least `rob_dominant_threshold` of the
  inverse-variance weight. The default is **0.55**, compared with `>=` — the
  conservative of the two candidates in the Fig 2 footnote (">65% weight or
  >=55% weight=possibly dominating"); pass `0.65` for the stricter reading.
  The non-dominated branch of the figure **never rates the domain down**; it
  decides which studies the analysis should use. Risk of bias therefore rates
  down noticeably less often than in v0.3.1–v0.4.0, and a body of evidence in
  which a minority of the weight is at high risk of bias can no longer be
  downgraded for it.
* On that non-dominated branch, "substantially different magnitudes of effect"
  is judged on **magnitude alone**. Core GRADE 4 (p6) words the node
  symmetrically, so the `small_values` direction gate is not applied there; it
  stays on the dominated branch, whose node is explicitly "Check direction of
  bias". A body of evidence whose low-risk-of-bias studies show the *larger*
  effect no longer reads as "no substantial difference".
* When the flowchart reaches "use low risk of bias studies only", the
  meta-analysis is **refitted on the low-RoB subset** by default
  (`rob_refit = TRUE`). Every downstream domain, the rating target, the
  baseline risk and the SoF table then use the restricted estimate, so pooled
  numbers can change without any change to the input data. The refit is
  announced with a message, recorded in the Risk-of-bias notes, shown by
  `print()`, and footnoted in `sof_table()`. Set `rob_refit = FALSE` to keep
  the full analysis and receive the recommendation only.
* **Automated risk-of-bias judgments now cap at one level.** The sign-flip rule
  and the all-studies-high-RoB case used to return `"serious"` (−2). Core
  GRADE 4 describes no automatic two-level risk-of-bias downgrade — every leaf
  of Fig 2 reads "rate down" / "do not rate down", and the paper's only "two
  levels" is about rating *up* observational evidence. −2 stays reachable
  through the scalar override `rob = "serious"` with `rob_rationale`.
* **Inconsistency: the automated Step 1 cut-off moves from `I² > 25%` to
  `I² > 30%`** — the only number Core GRADE 3 puts on paper ("one will seldom
  see serious inconsistency with I2 values <30%"). 25% had no source. Analyses
  with 25% < I² ≤ 30% now stop at Step 1 and are not rated down.
* Inconsistency now evaluates point estimates against the **chosen threshold**
  rather than the raw MID. Core GRADE 3 Fig 2 node 2 reads "Evaluate point
  estimates of studies in relation to chosen threshold", and that is the value
  the rating target resolved for Imprecision: ±MID for an important-effect or
  little-to-no-difference target, the null for a non-null-effect target.
  Previously the two domains could judge the same analysis against different
  boundaries, which Core GRADE 3 Fig 4 shows can reverse the verdict.
* **Automated inconsistency judgments now cap at one level.** Opposite-sided
  point estimates with no credible subgroup explanation used to return
  `"serious"` (−2). Core GRADE 3: "we have found compelling reason to rate down
  twice for inconsistency sufficiently unusual that it need not concern users
  of Core GRADE." −2 requires `inconsistency = "serious"` with
  `inconsistency_rationale`.
* **Publication bias: the `p < 0.01 → "serious" (−2)` tier is removed.** Core
  GRADE 4 Fig 5 never rates down two levels for publication bias, and its
  asymmetry node is qualitative ("strongly suggests publication bias") with no
  significance threshold attached. The surviving `p < 0.05` cut-off is labelled
  a pmatools operational convention in the domain notes.
* `pubias_registry_complete` is now consumed **after** the "most or all studies
  small and industry sponsored" question, not as an entry-level rule-out. Fig 5
  has no such node; evaluating the flag first let a body of small
  industry-sponsored trials escape the Q1 downgrade on the user's assertion
  alone. The note states that the rule-out is that assertion, not a figure node.
* **Indirectness: per-study vectors and column names are aggregated by weight
  share, not worst case.** A single indirect study out of twenty used to rate
  the whole body of evidence down, which is the opposite of Core GRADE 5's
  framing ("all or almost all evidence comes from younger people"). The share
  of weight rated `"serious"` is tested first, then the share rated
  `"some_concerns"` or `"serious"`, each against the new
  `indirectness_dominant_threshold` (default `0.55`, matching
  `rob_dominant_threshold`). Core GRADE 5 gives **no** numeric threshold, so
  this one is a pmatools convention and every aggregated note says so. Weights
  come from the inverse-variance study weights, with a count-share fallback.
  The `indirectness_subdomains` table keeps its worst-case fold — subdomains
  are facets of one judgment, not units of evidence.
* `suggest_threshold()` gains a **`source`** field (`"core_grade_6"` or
  `"package_convention"`), and for binary outcomes the **absolute** candidate
  (ARD 0.05) is now the first candidate, with the ratio value moved to
  `$threshold_ratio`. The Core GRADE series contains no ratio-scale MID, and
  every binary MID it discusses is on the absolute scale. SMD 0.20 is the only
  default with a source, and Core GRADE 6 hedges it. The entry-gate error
  message says all of this. `threshold_scale = "auto"` in `grade_meta()` is
  unaffected.
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
* Plain language summaries (**Core GRADE 6 Box 1**): the statements are chosen
  from the certainty level, `threshold_type`, `rating_target` and the
  **direction of the pooled point estimate**. Box 1 is the canonical source for
  summary of findings tables — it summarises the earlier Core GRADE 2 Table 1
  guidance and adds the guidance specific to the null and MID thresholds.
  Because Box 1's qualifiers name the direction of the effect on the outcome
  (`reduces` / `increases` / `has little to no effect`, e.g. "Treatment
  increases serious adverse events"), the statement reads correctly for harms;
  Core GRADE 2 Table 1's fixed "benefit" wording would have summarised an RR of
  2.42 for serious adverse events as "Treatment likely has an important
  benefit". Very low certainty uses the Core GRADE 6 Table 1 sentence, "We are
  very uncertain about the effect of X on Y". Box 1's qualifier list offers two
  adverbs per certainty level ("probably (likely)", "may (possibly)"); pmatools
  emits the **first word of each pair**, so a cell reads "Treatment probably
  results in an important increase in serious adverse events" and "Treatment
  may reduce mortality" rather than carrying the parenthesised alternative into
  the table. No summary of findings table in Core GRADE 6 prints both words
  either — Table 1 has "may decrease mortality", Table 3 has "possibly
  increases", and Box 1's own MID example has "probably has little to no
  important effect" — so the parenthesis reads as an editorial "either word
  will do". The single-adverb rendering is a pmatools choice, not a quotation,
  and is recorded as such: the verbatim Box 1 transcription is kept in the
  source of `R/plain_language.R`, and every frame is tagged with its provenance
  (quoted, composed, or quoted-minus-the-parenthesis). Box 1 leaves several
  cells without a worked example; those statements are composed by applying its
  qualifier list to the frame of the verbatim example in the same column.
  Objects created before the Core GRADE 2 entry gate (no `$rating_target`)
  simply omit the column, as do rows with no usable pooled estimate to take a
  direction from.
* Three helpers that were previously internal are now part of the public API,
  so a front end can depend on pmatools instead of reaching into it:
  `combine_arms()` merges the arms of a multi-arm trial into one row per study
  unit (Cochrane Handbook 6.5.2.10) — the step `ingest_data()` already applies,
  exported for callers assembling their own long data frame; `format_effect()`
  renders a pooled estimate as the exact string the SoF and evidence-profile
  tables print, back-transforming ratio measures and picking the same model the
  tables do; and `rob_strata()` normalises risk-of-bias labels onto the
  `"low"` / `"some"` / `"high"` / `"unknown"` strata, which is how a caller that
  edits or imports RoB judgments of its own can share pmatools' label
  vocabulary rather than keeping a second copy of it. `rob_strata()` warns and
  returns `"unknown"` for an unrecognised label instead of aborting, since it
  feeds plots. The internal `.combine_arms()` / `.format_effect()` /
  `.rob_plot_strata()` names still work as thin aliases.
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
* New `indirectness_table()` renders those subdomain judgments as a flextable:
  target question, evidence found, a colour-graded 4-option judgment row with
  the recorded answer ticked, and a merged "Judgment across subdomains" row
  carrying the overall judgment. The layout is a pmatools implementation of
  Core GRADE 5's per-PICO reasoning, **not** a Core GRADE 5 publication format:
  no table of that shape appears in the article body, and the table footer and
  the documentation say so.
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

## Bug fixes

* `grade_meta()` decided whether a supplied `indirectness` was a manual
  override of the subdomain worst case with `missing()`, which is `FALSE`
  whenever the caller passes every argument — exactly what the Shiny app and
  any `do.call()` wrapper do. The formal now defaults to `NULL` and the check
  is `is.null()`. An explicit `"no"` is still an override, and the error says
  to omit the argument (or pass `NULL`) when that was not the intent.
* Imprecision: the OIS note read `<= 30%` while the decision used `< 30%`; the
  display now matches the Fig 4 node "N<30% of OIS".
* The bundled `sample.R` passed `outcome_favors =`, which stopped being a
  `grade_meta()` formal when it was renamed to `small_values`, so the worked
  example had not run for some time. It now also demonstrates the entry gate, a
  rating-target override, indirectness subdomains and `rob_some_concerns`.

## Documentation and provenance

A line-by-line comparison against the published Core GRADE 1–7 articles turned
up places where pmatools presented its own operational choices as if they came
from the source. The judgments those readings changed are listed under Breaking
changes above; the items below change wording only.

* The count-share fallbacks (Risk of bias and Indirectness), the 80% majority
  share (CINeMA) and 20% each-side share used by the inconsistency zone tally,
  the `p < 0.05` publication-bias cut-off, both dominance thresholds, and the
  extension of the CI-ratio cut-off of 3 to HR / IRR are now flagged as
  pmatools conventions in the domain notes.
* The inconsistency notes state that the I² gate is an automation surrogate for
  a Step 1 that Core GRADE 3 describes as visual ("Core GRADE relies on the
  visual inspection of forest plots"), and point at `plot_forest()` plus the
  manual flowchart inputs as the faithful route.
* `rob_some_concerns` no longer implies Core GRADE 4 defines the fold. The
  phrase "some concerns" does not occur in that article; it sets the binary
  boundary by counting high-risk items and explicitly declines to settle the
  count.
* `TE_low` in the risk-of-bias direction check is documented as **always a
  fixed-effect estimate**, even under a random-effects parent model.
* `chinn_smd_to_or()` and the SoF footnotes now state that Chinn's formula is
  **not** Core GRADE 6's option 2 (normal distribution, MID-based, per study
  before pooling), which is not implemented.
* Indirectness domain notes use the Core GRADE wording ("not serious" /
  "serious" / "very serious") instead of the risk-of-bias-derived "some
  concerns". The stored level names are unchanged.
* `?grade_meta` and the README gain an **internal-name vs Core GRADE wording
  table** (`"some_concerns"` = the source's "serious"; `"serious"` = its "very
  serious"), and the README documents what Core GRADE covers that pmatools
  does not: rating up non-randomised evidence for large effects and
  dose-response, "extremely serious" (−3), the cross-domain gestalt step, and
  four summary of findings features Core GRADE 6 asks for.
* The README indirectness section gains the guideline / health technology
  assessment distinction, the indirectness-vs-inconsistency test, the two
  search scenarios, and the surrogate-outcome basis for rating down two levels
  — all with the source's own wording.
* The inconsistency documentation points at ICEMAN for subgroup credibility and
  notes that pmatools is more permissive than Core GRADE 3, which asks for
  separate PICO questions once credibility is moderate or high.
* SPEC §5.2 rewritten to match the code (it still described a `≥ 0.75`
  cut-off and a `(n_above + n_trivial)/k` formula the code no longer uses).
* Imprecision: the unused `rating_target` / `threshold_type` arguments are
  documented as unused, and the notes now say that Fig 4's *second* two-level
  condition (the plain language description suggesting "may" rather than
  "likely") is not auto-assessed.

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
