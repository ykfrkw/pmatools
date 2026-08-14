# pmatools 0.5.1 (development version)

## Breaking changes

* **The domain judgment values are now Core GRADE's own words, `"serious"` has
  moved from −2 to −1, and passing a bare `"serious"` is an error for this
  release.** The stored vocabulary was `"no"` / `"some_concerns"` / `"serious"`,
  and it disagreed with the source by exactly one step: pmatools' `"serious"`
  was Core GRADE's *very serious*. A reader with a Core GRADE paper in one hand
  and this code in the other was one step out on every domain, and the package
  needed a whole display layer to paper over it. The values are now
  `"not_serious"` (0), `"serious"` (−1), `"very_serious"` (−2) and
  `"extremely_serious"` (−3), which is Core GRADE 1's list verbatim.

  The rename moves `"serious"` from −2 to −1 **without changing its spelling**,
  which is the one change a script cannot notice: it would keep running and
  report a different certainty rating, and silently fail to reproduce a
  published analysis. So pmatools refuses the bare string rather than guessing
  which release you wrote it against, and the error names both readings:

  | you meant | write |
  |---|---|
  | rate down 1 level — Core GRADE's *serious* | `"some_concerns"` |
  | rate down 2 levels — Core GRADE's *very serious* | `"very_serious"` |

  Both spellings mean in 0.5.1 exactly what they meant in 0.5.0, so migrating
  is a search-and-replace with no judgment calls in it. The refusal covers the
  scalar domain arguments, per-study `rob` / `indirectness` vectors,
  `rob_overrides` values and `rob_strata()`. **It is temporary**: one release
  later it will be deleted and a plain `"serious"` will be accepted as −1 like
  any other value.

  `"no"`, `"some"` and `"some_concerns"` stay accepted permanently and
  silently, because their meaning never moved. Everything that *reads* a
  judgment — `$domain_assessments$judgment`, the domain notes, the Evidence
  Profile, the Summary of Findings tables, the exported `.docx` — now contains
  the canonical spelling, so a consumer matching on judgment strings must match
  the new form. `$downgrade` is unchanged and remains the reliable thing to
  read.

* **`small_values` is required, and a call without it aborts.** It says which
  way benefit runs for the outcome: `"desirable"` when a small value is good
  (mortality, symptom severity), `"undesirable"` when a small value is bad
  (response rate, remission). Migration is one argument per `grade_meta()` call
  — or one entry in `grade_meta_multi()`'s `common` list, which covers a whole
  batch — and there is nothing to decide that the review has not already
  decided.

  It was optional, and two domains guessed in its absence. Risk of bias fell
  back to "further from the null" (`|TE_all| > |TE_low|`) for the Core GRADE 4
  Fig 2 direction-of-bias check, and then **warned that the assumption had
  determined the downgrade** — a package that has to say "this guess decided
  your rating" is saying the argument should never have been optional. The
  optimal information size used Core GRADE 2's relative risk *reduction* as
  written, so for an outcome whose events are the desirable thing the
  alternative event rate landed on the wrong side of the modest RRR and the OIS
  was powered against the wrong tail. Both guessing branches, and the warning,
  are gone.

  **The bug that motivated it:** `export_bundle()` wrote `small_values = NULL`
  into the bundled `analysis.R` whenever the caller had not routed the argument
  through `grade_args`, so a "reproducible" script re-ran the OIS on the other
  side of the RRR and documented a different analysis from the one it came
  from. The rated object now carries `$small_values` and the bundle reads it.

  **There is no escape hatch, and that is deliberate.** `require_threshold =
  FALSE` exists because rating without a MID is a legitimate methodological
  choice — Core GRADE 7 asks users to read the CI first and pin down a MID only
  where the verdict depends on it. Rating without a direction is not a choice:
  every outcome a review rates has one, and "direction unknown" only ever means
  the outcome has not finished being specified. The abort carries condition
  class `"pmatools_direction_gate"`, and `grade_meta_multi()` re-raises it
  rather than demoting it to a per-outcome warning, exactly as it does the
  threshold gate. `assess_rob()` and `assess_imprecision()` require it too.

  **This is the third breaking change in 0.5.1, and the only one that cannot
  change a number behind your back.** The judgment-vocabulary rename and the
  risk-of-bias labels above both had to be defended against silence — a script
  that kept running and reported a different rating — which is why the
  vocabulary rename spends a whole release erroring on `"serious"`. A missing
  required argument has the opposite failure mode: the call stops, names what
  is missing, and nothing is rated until you answer. That is why it is
  acceptable to add a third break here rather than wait.

* **A fourth domain level, `"extremely_serious"` (−3), exists and is manual
  only.** Core GRADE 1 lists it ("or, rarely, extremely serious") and pmatools
  had no value for it. It is now reachable through any scalar domain argument,
  with the written rationale those already require, and through the override
  menu on all five Step 3 domain tabs in the Shiny app. **No automated path
  produces it, and none may** — none of the Core GRADE flowcharts describes a
  three-level downgrade, so there is no rule to implement; `GRADE_LEVEL_AUTO_MAX`
  records the cap and a test asserts it against every function that can build a
  domain row. It is deliberately absent from the app's "Other considerations"
  control, which stays 0 / −1 / −2: that control is not a Core GRADE domain.
  Certainty still stops at Very Low, so a −3 that takes the total past the
  floor is reported as Very Low with `certainty_score = 1`.

* **The analysis is no longer filtered to one level of an `outcome` column, and
  the Step 2 "Outcome" selector is gone.** Data carrying an `outcome` column
  with more than one level used to render a selector and pool only the rows
  matching it. On a continuous review that is backwards: an `outcome` column
  naming one measurement scale per study (PHQ-9, HAMD, BDI) is the ordinary
  shape of the data a standardized mean difference exists to pool, and the
  filter cut it down to whichever scale sorted first — leaving, in the common
  case, a single study. Selecting nothing withdrew the analysis silently, so
  Step 3 reported it as an unconfigured domain rather than as a missing choice.
  `outcome` is now a descriptive column and every row is pooled. `run_ma()`
  still refuses the one shape that cannot be pooled — the same `studlab` under
  two outcomes, which would count that study twice — but its abort now fires
  only on that condition and names the offending studies, where it previously
  fired on any file with more than one outcome label anywhere. A caller that
  relied on the old abort to catch a multi-outcome file must filter the data
  itself, or check that each study appears once.

* **Continuous outcomes default to presenting the SMD or MD itself, not a
  proportion of responders.** The Configuration tab's responder conversion was
  a checkbox ticked by default, which read as though converting were a step on
  the way to a rating. It never was: Chinn's SMD-to-OR conversion reaches
  `sof_table()` only and `grade_meta()` has never seen it. It is now a two-way
  choice (`input$sof_presentation`, replacing `input$convert_smd_to_or`)
  defaulting to the effect itself, with the threshold section moved above it.
  A Summary of Findings table that used to come out in responder form will now
  report the SMD or MD unless the responder option is picked. The certainty
  rating is unaffected either way — it has always read the decision threshold
  on the analysis scale. `sof_table()`'s and `export_bundle()`'s
  `convert_smd_to_or` arguments are unchanged and still default to `FALSE`.

* **The optimal information size for an SMD outcome changes, and with it some
  Imprecision ratings.** The continuous OIS combined a threshold in
  standardized units with a standard deviation on the raw scale, so a review
  with a pooled SD of 4 asked for 12,991 participants where the correct target
  is 785. An SMD is already expressed in standard deviations, so its sigma is
  1; mean differences and ratios of means keep the pooled SD, which is correct
  for them. Figure 4 only consults the OIS on the large-effect path, but there
  the error inflated the shortfall: on the fixture added with this change the
  verdict moves from "consider rating down two levels" to "rate down one
  level". Any saved SMD rating that turned on the OIS should be re-checked.
  `SPEC.md` §5.5 described the multiplication the code never performed and now
  describes what runs.

* **Shiny app: the exported ZIP has the multi-outcome layout, always.** The app
  used to build the flat single-outcome bundle for whichever outcome was on
  screen and then append one extra `sof_table_combined.docx` covering the
  saved ones. A ZIP therefore mixed two things: one outcome's plots, results
  and tables at the root, and a summary table describing outcomes whose plots
  were nowhere in the file. It now exports the layout `export_bundle()` already
  had for a `pmatools_set`: `summary_of_findings.docx` / `.csv`,
  `evidence_profile.docx`, `data_long.csv`, `analysis.R` and `README.txt` at the
  root, and one numbered `outcomes/NN_name/` directory per outcome — with a
  single outcome getting the same tree, one directory deep. Every path in the
  ZIP moves: `forest_plot.pdf` is now `outcomes/01_<slug>/forest_plot.pdf`,
  `grade_table.docx` is `outcomes/01_<slug>/evidence_profile.docx`,
  `sof_table.docx` and `sof_table_combined.docx` are both now the root
  `summary_of_findings.docx`, and `analysis.R` re-runs every outcome rather than
  one. The Step 4 checkboxes change with it — their values are now the
  bundler's own `include` vocabulary, so `grade_table` splits into `sof` and
  `evidence_profile` and `sof_combined` becomes `sof`. The responder
  presentation of a continuous outcome travels with it: `grade_table()` now
  applies the SMD-to-odds-ratio conversion per row (see the feature entry
  below), so the presentation the reviewer picked in Step 3 is what the root
  `summary_of_findings.docx` shows.

* **Export bundles no longer contain PNG plots.** Every plot used to ship twice,
  once as a PDF and once as a raster PNG of the same figure, in both the
  single-outcome and the multi-outcome layout. Only the PDF ships now, so a ZIP
  that used to hold ten plot files holds five, and `forest_plot.png`,
  `forest_plot_rob.png`, `forest_plot_full.png`, `funnel_plot.png`,
  `funnel_trimfill.png`, `pubias_missing_forest.png` and
  `rare_event_method_forest.png` are gone. The generated `analysis.R` drops its
  PNG device calls to match. A pipeline that picks a plot out of the bundle by
  name has to read the `.pdf`; the PDF is the higher-fidelity copy of the two
  and was always written alongside, so nothing is lost but the raster. The
  app's on-screen plot previews are a separate path and are unaffected.

* **Shiny app: a certainty domain is confirmed by ticking its box, and by
  nothing else.** The export gate used to accept substantive input in the
  domain as a confirmation — a filled per-study risk-of-bias table, an answered
  Indirectness PICO question, an OIS override, a judgment override with a
  rationale — and the checkbox was one route among several. It is now the only
  route. A review that used to reach Step 4 with Risk of Bias confirmed by its
  table alone will now find the download locked until that box is ticked, and
  the same is true of every other domain. The stricter gate is intended: the
  old rule could report a domain as unconfirmed while the checkbox on screen
  was ticked (a tick left over from the previous outcome), and confirm a domain
  whose box was visibly empty, so the one thing the reviewer could see was the
  one thing that did not decide it. It would also have opened the gate by
  itself for any widget that ships preselected. Configuration is unchanged: it
  still requires its values to be set as well as its box ticked. Nothing about
  navigation is locked — the tab strip and the stepper still move freely, and
  what a domain is *rated* as is untouched. See `shiny/SPEC.md` §3.4.13.

* **Shiny app: there is no Save button. An outcome is banked automatically.**
  The "Save this outcome's assessment as …" button on the Step 3 "Final
  certainty" tab, the "already saved — replace?" modal behind it and the
  saved-outcome list beside it are all deleted. An outcome is written into the
  Summary of Findings table the moment its sixth certainty domain is
  confirmed, and rewritten whenever the rating it holds changes (debounced
  750 ms). Confirming all six domains was already the reviewer's statement
  that the rating was finished — the button behind that statement could only
  be forgotten, and was: six ticks, then an empty Step 4 table. A session that
  relied on *not* pressing Save to keep a provisional rating out of the table
  no longer can; delete the row on Step 4 instead, or leave a domain
  unconfirmed. Two behaviours change with it. **Renaming an outcome in Step 2
  now renames its saved row** rather than adding a second one — outcomes carry
  a session-stable uid, and `pma_upsert_outcome()` matches on that rather than
  on the display name; a review that had deliberately duplicated a row by
  renaming it will find one row. **An outcome with a blank name is not
  saveable**: the old key fell back to the literal string `"Outcome"`, which
  under an automatic save would be banked as a row every time "+ Add next
  outcome" blanked the name. **The saved-outcome list moved to Step 4**, below
  the combined table it feeds; the per-row Move / Mark primary / Remove
  controls are unchanged and there is now one copy of them rather than two.
  See `shiny/SPEC.md` §3.4.14 and §3.5.5.

* **Shiny app: the Risk of Bias sensitivity-analysis change threshold is no
  longer settable.** `rob_inf_threshold` — the slider on the Configuration tab
  labelled "Sensitivity-analysis change threshold (Risk of Bias only)" — is
  deleted, and the app no longer passes `rob_inflation_threshold` to
  `grade_meta()` at all. The package default of `0.10` now applies
  unconditionally, so a review that had moved the slider will rate Risk of Bias
  against 10 percent instead, and the judgment can change on the one rule that
  consults it (a bias-favouring shift within the same non-trivial zone) and on
  whether the analysis is restricted to the low risk-of-bias studies. The
  control was a pmatools convention rather than a Core GRADE 4 rule, and a
  reviewer had no basis on which to move it. `grade_meta()` still takes the
  argument, so a script that sets it is unaffected; the bundled `analysis.R`
  writes the same `0.10` it always did when the app had not been touched.

* **Shiny app: Indirectness now takes the subdomain path by default, which
  changes the exported bundle.** The four Core GRADE 5 PICO radios ship
  preselected to "yes" instead of blank. The *rating* is unchanged — blank used
  to send `indirectness = "no"`, four "yes" answers fold worst-case to the same
  "no", and certainty, every domain judgment and every downgrade are identical
  either way — but `indirectness_subdomains` is now populated for every outcome
  rather than absent. Consequently `indirectness_table()` stops aborting on an
  app-rated object; the multi-outcome bundle gains
  `outcomes/<nn>_<outcome>/indirectness_table.docx`, which it writes only when
  subdomain judgments exist; the bundled `analysis.R` carries an
  `indirectness_subdomains = data.frame(...)` literal in place of `NULL`; and
  `results.txt` reports the four answers rather than "Overall judgment provided
  by user." A pipeline that keys on the presence of the indirectness artifact
  will start seeing it. The point of the change is that the old default was
  silent: the domain scored no downgrade while the screen showed four
  unanswered questions.

* **Inconsistency rates down two levels when the estimates point in opposite
  directions, and this deliberately departs from Core GRADE 3.** The automated
  and manual flowcharts used to cap that branch at `"some_concerns"` (−1),
  citing Core GRADE 3's own sentence that a compelling reason to rate down
  twice for inconsistency is "sufficiently unusual that it need not concern
  users of Core GRADE". The reasoning for reversing it: the branch is not
  "studies disagree more than the eye likes" — that is the neighbouring
  `heterogeneous` branch, which still rates down one level and is unchanged.
  This branch fires only when a substantial share of point estimates sits above
  the chosen threshold, a substantial share sits below it, and no credible
  subgroup explains the split. The reviewer cannot say which direction the
  intervention works in, and leaving such a body of evidence at Moderate
  overstates it. Core GRADE 3 calls the two-level case unusual rather than
  wrong, and pmatools' 20%-each-side gate is what makes it unusual: ordinary
  disagreement never reaches it. An analysis that scored −1 here now scores −2,
  so overall certainty can drop one band without any change to the input data
  — check `domain_assessments` before comparing a rating against an earlier
  run. `.INCONSISTENCY_CAP_NOTE` is replaced by
  `.INCONSISTENCY_TWO_LEVEL_NOTE`, which states the departure in the notes
  wherever the branch fires; the risk-of-bias one-level cap
  (`.ROB_CAP_NOTE`) is untouched. See `SPEC.md` §5.2.

* **The decision flowcharts drop three things that were not decisions.** The
  Risk-of-Bias chart no longer opens with "Any study at high risk of bias?":
  with no high-risk study the dominance share is 0, which is below the gate,
  and there is nothing to exclude, so that case now routes through the
  surviving chart instead of through a node of its own. The Publication-bias
  chart drops the `Q1`–`Q4` prefixes from its question nodes and from the app's
  wizard headings and breadcrumb — the numbering is Core GRADE 4 Fig 5's, but
  the chart interleaves a pmatools node between Q1 and Q2, so on screen it
  numbered neither the source nor the route — and drops its two "qualitative
  assessment required" leaves, whose judgment was `"no"` and whose caveat lives
  in the notes and in the warning. The `"Q1:"`–`"Q4:"` prefixes inside the
  domain **notes** are deliberately kept: they travel into `evidence_profile()`
  and the exported `.docx` as the ordered record of the assessment. Code
  reading `flow_path` against a hard-coded id list must drop
  `pma-rob-node-anyhigh`, `pma-rob-leaf-nohigh`, `pma-rob-edge-anyhigh-no`,
  `pma-rob-edge-anyhigh-yes`, `pma-pubias-leaf-qual-q3`,
  `pma-pubias-leaf-qual-q4`, `pma-pubias-edge-q3-na` and
  `pma-pubias-edge-q4-na`, and rename `pma-incon-leaf-down1` to
  `pma-incon-leaf-down2`.

* **The Risk-of-bias inflation threshold now defaults to 0.20, not 0.10.**
  `rob_inflation_threshold` is the relative change of the pooled estimate that
  counts as bias-favouring inflation on Core GRADE 4 Fig 2's *dominated* branch
  (rule 3, "rate down") and as a "substantially different magnitude" on its
  *non-dominated* branch (`analysis_set = "low_only"`, which by default refits
  the model). Doubling it means analyses that used to rate down for risk of
  bias, or to be refitted on the low-RoB subset, may now do neither — **so a
  stored analysis re-run under this release can report a different certainty
  rating and different pooled numbers with no change to the input data.**

  The old value was too tight to be about bias. `TE_low` is always a
  fixed-effect estimate while `TE_all` usually is not, so the two differ by the
  estimator alone; with any real heterogeneity that gap routinely clears 10%,
  and the domain rated down on arithmetic rather than on risk of bias. Core
  GRADE 4 puts no number on either node — the dominance gate is the only one
  its Fig 2 footnote quantifies — so both values are pmatools conventions and
  neither is the source's.

  Pass `rob_inflation_threshold = 0.10` to `grade_meta()` to restore the old
  behaviour. The default now lives in one place,
  `PMA_ROB_INFLATION_THRESHOLD` in `R/domain_rob.R`, which `assess_rob()`,
  `.flowchart_rob()`, `.assess_bias_direction()` and `export_bundle()`'s
  fallback all read; it used to be a literal `0.10` repeated at each of those
  four sites. The comparison is unchanged and still strict (`>`), so a relative
  change of exactly 0.20 does not rate down. The Shiny app has exposed no
  slider for this since 0.5.1 and takes the package default, so the app moves
  with it.

* **"Reporting bias is plausible" no longer rates the evidence down on its own
  (Shiny app).** The Publication bias tab's overall reporting-bias question
  offered three answers, and the middle one was a rule Core GRADE 4 Fig 5 does
  not contain: a `"no"` was rewritten, after `grade_meta()` had returned, into
  a forced rate-down 1 *regardless of the remaining nodes*. A reviewer who
  thought reporting bias plausible and then went on to answer the funnel-plot
  question found the funnel answer had counted for nothing, and the domain note
  said the rating had been decided by a question they had answered two screens
  earlier.

  The question now has **two** answers and only one of them decides anything.
  `"Yes — reporting bias is unlikely; do not rate down"` is the pmatools
  short-circuit it always was and still reaches
  `grade_meta(pubias_registry_complete = "yes")`. `"No — reporting bias is
  plausible; go on to the Figure 5 nodes"` carries on down the chart and lets
  Q2–Q4 decide, which is exactly what the deleted third option ("leave it to
  the Figure 5 nodes") used to do — so that option is gone with the rule, and
  `STEP3_PUBIAS_DEFER` with it rather than being left unused. The `(rate down
  1)` promise is off the label because nothing promises it any more.

  **What changes for a reviewer:** an analysis that answered `"no"` and rated
  the domain *serious* on that answer alone now takes whichever judgment the
  Figure 5 nodes reach, which for most bodies of evidence is *not serious*.
  Certainty can therefore come out one level higher than it did in 0.5.0 on the
  same answers. To keep the old rating, use the tab's **Override** control,
  which asks for the written rationale the deleted rule never did. The package
  API is unchanged: `assess_pubias()` never had this rule.

* **The publication-bias `k` fact is the bare study count.** `domain_facts(g,
  "Publication bias")$value` for `key = "k"` read `"12 (Q2 threshold: 10)"` and
  now reads `"12"`. A Summary of Findings footnote is the one place a reader
  meets that string with no flowchart beside it to say what a "Q2" is. The
  threshold is unchanged and still stated in the domain note (`"Q2: Statistical
  analysis feasible (k = 12 >= 10)"`), where the sentence around it gives it a
  meaning. Consumers matching on the fact's `value` string must adjust;
  `numeric` was always the count and is unchanged.

## New features

* **The Shiny app can put an outcome nobody reported into the Summary of
  Findings table.** `not_reported_outcome()` and `add_not_reported()` have been
  package API since earlier in this cycle, and the tables, the exported `.docx`
  and the generated `analysis.R` all handled such a row — but nothing in the
  app could create one, so the only way to satisfy Core GRADE 6's "cover every
  patient-important outcome, including the ones the evidence base is silent on"
  was to write R by hand. Step 4's **+ Add next outcome** now asks which kind of
  outcome is being added: one to analyse from the data (the previous
  behaviour), or one to record as not reported. The second route collects an
  outcome name, an optional follow-up and an optional reason — the reason
  becomes a numbered footnote on the row — and the row then travels through the
  combined table, the ZIP's `outcomes/NN_name/results.txt`, and an
  `add_not_reported()` call in the bundle's `analysis.R`, exactly as a
  hand-built set's would. The app's footnote claiming such rows were absent
  from its tables is gone with it.

  A `pmatools_not_reported` deliberately does not inherit `"pmatools"`, so
  everything in the app that filters saved outcomes now says which classes it
  wants: rows with no analysis stay out of `run_ma_multi(outcomes = )`,
  `data_long.csv`, `grade_meta_multi(per_outcome = )` and the risk-of-bias
  labels, and stay in the table's row order and the ZIP's directory numbering.
  A bundle whose outcomes are *all* not reported is refused: it has no analysis
  to build from.

* **Trim-and-fill is stated as a 20% exaggeration check, next to the funnel it
  belongs to.** The Reporting bias tab printed the original and the
  trim-and-fill adjusted pooled effects and left the reviewer to compare them
  by eye. `.pubias_trimfill_inflation()` and `.pubias_trimfill_line()` in the
  new `R/pubias_trimfill.R` now ask the same question the risk-of-bias
  direction check asks of the low risk of bias subset — is the estimate that
  may be exaggerated more than a fifth further in the direction that favours
  the intervention? — of the trim-and-fill adjustment, sharing
  `PMA_ROB_INFLATION_THRESHOLD` so the two cannot drift apart. **It rates
  nothing.** Core GRADE 4 Fig 5 has no trim-and-fill node, `assess_pubias()`
  does not read this function, and the printed sentence says so: it is material
  for the reviewer answering the funnel-asymmetry question, and the
  per-analysis knob `rob_inflation_threshold` is deliberately not routed here,
  so tuning a *rating* cannot move a *display*.

* **Cochrane RoB 2's three judgments and ROBINS-I's four are accepted
  verbatim, and the app's per-study editors are dropdowns rather than free
  text.** The package documented a four-level "Cochrane RoB 2.0" vocabulary —
  `"No concerns"` / `"Some concerns"` / `"Serious concerns"` /
  `"Critical concerns"` — and RoB 2 defines no such thing. It has **three**
  judgments (low risk of bias / some concerns / high risk of bias); the extra
  severity belongs to ROBINS-I, which is for non-randomised studies. Only
  `"Some concerns"` in that list was anyone's published wording, and neither
  tool's own labels for its other levels were accepted at all.

  `grade_meta()`, `rob_strata()`, `plot_forest_rob()` and
  `plot_forest_indirectness()` now take `"Low risk of bias"`,
  `"Some concerns"` and `"High risk of bias"` (RoB 2), and
  `"Low risk of bias"`, `"Moderate risk of bias"`, `"Serious risk of bias"`
  and `"Critical risk of bias"` (ROBINS-I, whose top two fold onto the `high`
  stratum because Core GRADE describes no three-level risk-of-bias
  downgrade). **Nothing was removed**: the three older pmatools phrasings keep
  working permanently, because they are what extraction sheets and scripts
  written against v0.4–v0.5.1 contain. They are simply no longer presented as
  RoB 2's, in `README.md`, `SPEC.md` and every error and warning message that
  used to list them.

  In the Shiny app, the per-study Risk of Bias and Indirectness grids on Step
  3 offer a three-value dropdown instead of a cell to type into. A mistyped
  label used to land the study in the `"unknown"` stratum — `rob_strata()`
  warns, and the app showed that warning nowhere. Risk of Bias is labelled in
  RoB 2's words; Indirectness has no such instrument, so its three stay
  pmatools' own (**Low / Some / High indirectness**) rather than borrowing a
  vocabulary that would claim more than the column means. Both still store
  `"low"` / `"some"` / `"high"`, so bulk buttons, Step 1 and banked outcomes
  are unaffected.

* **New `detect_column_roles()` reports which column filled each
  `ingest_data()` role.** Given a data frame or a vector of column names, it
  returns one row per canonical long-format role (`studlab`, `treat`, `n`,
  `event`, `mean`, `sd`, `outcome`, `rob`, `indirectness`, `subgroup`) naming
  the source column that fills it, whether the match was canonical or by
  alias, and whether `ingest_data()` aborts without it. The alias list it reads
  is the one ingest itself renames by — extracted to `PMA_INGEST_ROLE_ALIASES`
  and shared — so a role reported as filled is a role `ingest_data()` fills,
  including the order rules (canonical beats alias; the first alias listed
  wins; `group` goes to `treat`, leaving `subgroup` empty). Nothing about the
  `ingest_data()` contract changes.

* **Step 1 of the Shiny app says which column was recognised as what.** The
  preview card opens with a green load banner ("36 rows, 18 studies, long
  format.", replacing a monospace `Status:` line) and a detected-columns strip
  built from `detect_column_roles()`: one chip per role, green with the source
  column when filled, amber with a hint when a role the analysis needs is not,
  muted when its absence is ordinary. `rob` and `indirectness` report how many
  studies are rated rather than whether a column exists. The preview itself now
  defaults to the analysis columns — the bundled sample is 39 columns wide and
  five of them are the analysis — with an **All columns** toggle beside it, and
  the bulk risk-of-bias buttons that previously existed only inside Step 3's
  Risk of Bias tab are repeated below the table, where the data-entry pass
  happens. Both sets write the same `state$rob_table`; the Step 3 copies stay.

* **`grade_table()` presents a continuous outcome as a proportion of
  responders, row by row.** `sof_table()` has taken `convert_smd_to_or` /
  `baseline_risk` / `threshold_label` / `chinn_invert` since v0.2, which is the
  right shape for a table of one row and the wrong one for a combined table:
  the answer differs per outcome, and a binary row has no answer at all. The
  combined table now reads the same four names off each rated object's
  `"pmatools_display"` attribute — the channel that already carries the
  per-outcome export arguments — and applies them per row, in both layouts. One
  table can therefore hold a converted continuous outcome, an unconverted one
  and a binary one; the `*` footnote explaining Chinn's formula is written once
  when any row used the conversion and not at all when none did, with each
  converted row's direction and threshold on its own line. **A row that asks
  for the conversion and cannot support it — a non-SMD/MD effect measure, no
  responder proportion in (0, 1), no usable pooled estimate — keeps its
  unconverted presentation and says why in a numbered footnote against that
  row, rather than aborting.** `sof_table()` still aborts on the same
  conditions, because its table *is* that row; in a combined table one outcome
  must not cost the reviewer the whole document. The Shiny app banks the choice
  with the outcome, `export_bundle()` carries it into the root
  `summary_of_findings.docx` and `.csv`, and the generated multi-outcome
  `analysis.R` re-stamps it so re-running the script reproduces the same table.
  See `SPEC.md` §4.9.

* **The heterogeneity estimator and the random-effects confidence interval are
  both choices now, and the model that ran is printed above the results.**
  `run_ma()` and `run_rare_ma()` accept `method.tau` values `"PM"`, `"SJ"`,
  `"ML"` and `"EB"` alongside the existing `"REML"` and `"DL"`; REML remains the
  default. Cochrane's Handbook (§10.10.4.1) no longer endorses DerSimonian-Laird
  and the simulation literature favours REML for continuous outcomes and
  Paule-Mandel for binary ones, so both are now reachable without leaving the
  app. Separately, the Hartung-Knapp adjustment was being applied automatically
  at k ≥ 3 and appeared in no control, no summary and no output — and the app
  never passed `hakn` to `run_ma()` at all, so the automatic rule could not be
  overridden from the interface. Step 2 gains a "Random-effects CI" selector
  (Auto / Hartung-Knapp / Classic) that defaults to that same automatic rule, so
  existing analyses are unchanged, and a line above the results now names what
  the fit actually did: `Random effects (REML), Hartung-Knapp CI, k = 12`.
  Forcing Hartung-Knapp below three studies warns and applies it as asked.

* **Shiny app: the Publication bias wizard shows the whole route from the
  start, and its three reference plots become a tabset.** The Figure 5 chart
  used to appear only after the domain had been rated, under the verdict, so a
  reviewer answering the third question could not tell that two more were
  coming — a one-question-at-a-time wizard that never said how long it was. The
  chart now sits **above** the wizard and is drawn from the first node onwards:
  unlit to begin with, lighting up node by node as answers arrive. It is lit
  from the answers rather than from the `flow_path` fact, by a new pure
  `step3_pubias_flow_ids()`, because that fact does not exist until the domain
  has been rated. The wizard itself is unchanged and is still the only place
  anything is answered; the copy under the verdict no longer repeats the
  figure, and the breadcrumb is now the "change" links alone — the lit chart
  says what its prose trail used to. Below the wizard, the funnel, the
  trim-and-fill funnel and the missing-results (RoB-ME) editor become three
  panels of one tabset, each at full width, instead of a funnel that appeared
  only at one question and two blocks folded into `<details>`. None of the
  three is gated on a wizard node any more: all three are reference material,
  all three are computable as soon as the analysis exists, and gating them hid
  each one exactly when a reviewer might want to check it against a different
  question. No judgment, note or export changes.

* **Shiny app: Step 3 says how far through it you are, and where the next click
  goes.** Each domain tab's Next stays greyed (with the reason on hover) until
  that domain's box is ticked; the tab strip marks a confirmed tab with a tick
  and an opened-but-unconfirmed one with a dot; the card header and the stepper
  both read "n/6", so the count is legible from the other three steps. Every
  message that names a domain still to be confirmed — the Final certainty
  banner and the Step 4 download lock — now names it as a link that opens that
  tab. The seven-tab strip scrolls sideways on its own rather than widening the
  page, which is what it was doing on a phone.

* **Shiny app: Step 3 stops explaining itself at length.** The five collapsed
  "How is this judged?" accordions are gone (about 600 words), and so is the
  Configuration tab's 115-word opener, the verbatim machine-generated note
  parked under every domain verdict, the Indirectness "no judgment recorded
  yet" banner, the MIC warning under the Decision threshold, and the
  provenance notes on Publication bias and Imprecision. What replaces them is
  what was already on screen: the flowchart under each verdict draws the
  algorithm and lights up the branch taken, and the reference line names the
  source paper. Nothing is hidden instead of deleted, and nothing is lost from
  the record — the verbatim note still travels into the Evidence Profile and
  the exported `.docx`. Muted explanatory lines are now capped at one desktop
  line and a test enforces it. Two other things moved: the outcome-direction
  echo gained a box of its own on Configuration, and `rob_some_concerns` moved
  from Configuration to the Risk of Bias tab, next to the verdict it produces
  (its review-wide scope is unchanged).

* pmatools now ships **drawings of the decision flowcharts it implements**, and
  the app highlights the path a given analysis actually took. Four domains have
  one — Risk of bias (Core GRADE 4 Fig 2), Inconsistency (Core GRADE 3 Fig 2),
  Imprecision (Core GRADE 2 Fig 4) and Publication bias (Core GRADE 4 Fig 5).
  Indirectness does not, and that is not an omission: Core GRADE 5 Table 2
  grades it on a gradient across the four PICO elements, so there is no branch
  to draw. The figures live in `inst/figures/`, are documented under the new
  `?grade_flowcharts` topic — which also names the function implementing each
  algorithm — and are reproduced in `man/figures/` for the help pages, with a
  test asserting the two copies never drift. They are pmatools' own diagrams
  rather than reproductions of the BMJ figures, because the algorithm differs
  from the source in ways a reader has to see: five enumerated direction rules
  the source does not enumerate, a Figure 5 node that is not one of Figure 5's
  four, and Inconsistency edges labelled with numeric surrogates Core GRADE
  declines to quantify. Each figure says so.
* Every flowcharted assessor now records the path it took as a `flow_path`
  fact — a space-separated list of node ids matching the ids in the SVG — so a
  renderer can highlight the route without parsing prose back out of `notes`.
  The ids each assessor may emit are declared beside it and checked against the
  drawing by `tests/testthat/test-flowchart-nodes.R`, so adding a branch without
  drawing it fails the build. `flow_path` is machine-only and is filtered out
  before facts are rendered as prose, so it never appears in a Summary of
  Findings footnote.
* `assess_pubias()` records facts for the first time (`k`, `egger_p` where the
  test ran, and `flow_path`), and `grade_meta()` now lifts them into
  `$domain_facts`. Indirectness remains the one domain with no facts, for the
  reason above; `domain_facts()`'s documentation no longer implies otherwise.
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
* `plot_forest_rob()` gains `some_concerns_as`, so the stratified forest can
  show the **two** groups the algorithm analysed rather than the four
  descriptive strata. `NULL` (the default) is the existing four-way
  `low` / `some` / `high` / `unknown` split, byte-for-byte; `"low"` or
  `"high"` folds to `Low risk of bias` / `High risk of bias` under
  `subgroup.name = "Risk of bias (as analysed)"`. The fold is not a second
  implementation of the rule — it asks the same internal `assess_rob()` uses
  for `rob_some_concerns`, which is why the argument carries that name, and
  unrated studies land on the side they land on in the rating. Pass the value
  you passed to `grade_meta()` whenever the plot sits next to a rating: with
  the four strata and the common `rob_some_concerns = "high"`, the figure and
  the judgment beside it disagree about how many groups there are. Folding at
  the call site is still not supported — `rob_strata()` owns that vocabulary
  and warns on labels invented elsewhere. See SPEC.md §4.3a.

* A single display vocabulary for domain judgments. `GRADE_LEVEL_SOURCE_WORDING`
  had no consumers and two hand-written copies of it had drifted: the internal
  level `"serious"` is Core GRADE's **very serious** (−2), so a renderer that
  wrote its own `switch()` could print "Serious" for −1 in one place and −2 in
  another. `.grade_level_wording()` (`R/utils.R`) is now the only function that
  turns a judgment into words, `evidence_profile()` calls it instead of its own
  duplicate, and the Shiny app's badges, verdict lines and override menus read
  the same function. Output is unchanged — `evidence_profile()` was already
  correct — but there is now one place to change it. See SPEC.md §5.0.

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

* Both Summary of Findings layouts now fill **both arm-level columns for
  continuous outcomes** — Core GRADE 6's preferred presentation — instead of the
  difference alone: the control cell is the inverse-variance weighted mean of
  the control arms and the intervention cell is that value plus the pooled
  difference, with an SMD rescaled by the pooled within-arm SD of the control
  arms (Cochrane Handbook 15.5.3.2) first; both derivations are footnoted and
  binary tables are byte-for-byte unchanged.

* New exported `not_reported_outcome()` and `add_not_reported()`: a Summary of
  Findings table can now carry an outcome the review prespecified that **no
  included study reported**. Core GRADE 6 asks the table to cover every
  patient-important outcome the review addressed, including the ones the
  evidence base is silent on, but every row of `grade_table()` was derived from
  `x$meta`, so such an outcome could not be expressed at all. Its row names the
  outcome (and its follow-up), reads "Not reported" in the participants,
  effect, arm-level and Difference cells, and "Not rated" in the certainty
  cell — not blank, because a blank cell cannot be told apart from a forgotten
  one, which is the whole argument for showing the row. A supplied `reason`
  becomes a numbered footnote on the row, sharing the pool with the
  risk-of-bias analysis-set notes. Both table layouts are supported, as are
  `grade_report()` and `export_bundle()` on the set; `reorder_outcomes()` and
  `set_primary()` treat the outcome like any other. The row is deliberately
  *not* an `evidence_profile()` row — all five domain columns are judgments
  about a body of evidence, and there is none — so `evidence_profile()` and
  `sof_table()` refuse it with a message saying where it belongs instead.

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

* **The control-arm risk is supplied once, and `threshold_baseline`, `ois_p0`
  and `baseline_risk` now inherit it from one another.** All three name the
  same quantity — the control-arm event rate — and they feed three different
  calculations: converting an absolute (ARD) threshold to the analysis scale,
  powering the Optimal Information Size, and printing the Summary of Findings
  absolute-risk columns. Callers had to pass the same number to two or three of
  them, and the README's own worked example did exactly that. Now an argument
  left `NULL` takes the first value supplied to any of the others, in the order
  `threshold_baseline`, `ois_p0`, `baseline_risk`; an argument still `NULL`
  after that falls back to the pooled control event rate, as before. **A value
  you passed explicitly is never displaced by an inherited one**, because the
  three can legitimately differ — a Summary of Findings table drawn against a
  named risk group while the OIS is powered from the trials' own control arms.

  **Which calls now compute something different.** Nothing errors, and no call
  that already passed the same number to every argument it used changes at all.
  What changes is a call that supplied *one* of the three and let the others
  default:

  | you passed | what used to happen | what happens now |
  |---|---|---|
  | `baseline_risk` only, with `threshold_scale = "ard"` | the threshold was converted at the pooled control rate, and the OIS powered from it | both use your `baseline_risk` |
  | `threshold_baseline` only | the SoF printed the pooled control rate | the SoF prints your `threshold_baseline` |
  | `ois_p0` only, with `threshold_scale = "ard"` | the threshold was converted at the pooled control rate | it is converted at your `ois_p0` (the SoF already inherited `ois_p0`, and is unchanged) |

  The old numbers are recoverable by passing the pooled value explicitly to
  whichever argument should keep it. Two things are deliberately **not**
  inherited: a value of exactly `0` or `1` (`baseline_risk` accepts the closed
  interval and the other two do not, so donating one would turn a working call
  into an abort elsewhere), and a character `baseline_risk` (`"simple"` /
  `"metaprop"`), which names a computation rather than a value.

  **A silent fallback would be worse than asking twice, so it is not silent.**
  The new `$control_risk` field records the donor, the arguments that inherited
  and the number each of the three uses ended up with, and the same sentence is
  appended to the Imprecision domain notes — reaching `summary()`, the Evidence
  Profile, `grade_report()` and the exported bundle. `export_bundle()` now pins
  all three into the bundled `analysis.R`, `ois_p0` included, so a re-run
  reproduces the rating instead of inheriting a baseline the original never
  used. Nothing is appended and nothing is pinned differently when all three
  were supplied.

  The Shiny app already routed one control-group risk into both the threshold
  conversion and the OIS, so its output is unchanged. Collapsing the three
  arguments into one `baseline_risk` remains the eventual destination and is
  deliberately not in this release, which already carries a breaking rename;
  see `SPEC.md` §4.5.4.

* **The BMJ Summary of Findings header merges every column that is not an
  absolute effect.** The layout has two header rows, and only the three
  absolute-effect columns ever used both: the other five — outcome,
  participants, relative effect, certainty, plain language summary — left an
  empty cell above their label. The header background is one solid navy, so
  those blanks read as a full-width band with "Absolute effects (95% CI)"
  floating on it rather than as a heading over three columns. Each of the five
  is now merged vertically across both rows, with its label in the top row (a
  flextable span renders its top-left cell, so a blank top cell would have
  erased the label), and the header is bottom-aligned so all seven labels share
  a baseline. The merge reaches the exported `.docx` as `w:vMerge`. Cell
  contents are unchanged, in both the single- and multi-outcome tables.

* **An overridden domain's Summary of Findings footnote states the reviewer's
  reason.** The per-domain rate-down footnotes are built from `domain_facts`,
  which record what the *algorithm* found and are not rewritten when a reviewer
  overrides a judgment — they cannot be. So an override moved the certainty
  cell and the "Due to …" sentence while the footnote under them went on
  reciting the automatic reasoning, reading as the justification for a rating
  it had not produced. It was reported against publication bias and was never
  specific to it. `.domain_fact_note()` now leads with the override — *"Rated
  serious by the reviewer, not by the algorithm: `<rationale>`. The automatic
  assessment recorded: `<facts>`."* — for every domain, in `sof_table()`,
  `grade_table()` and the BMJ layout alike. Two consequences worth knowing:
  the signal is the `"Manual override (…)"` head that `make_domain_row()`
  writes and **not** `auto == FALSE`, which also marks a reviewer-supplied
  *input* the flowchart then acted on; and **Indirectness**, which emits no
  facts at all, gets a footnote for the first time when it is rated down by
  hand.

* **The publication-bias flowchart draws every box with a solid outline, and
  two of them say what kind of box they are.** The registry-coverage node was
  drawn dashed to mark it as a pmatools input that is not in Core GRADE 4
  Fig 5. On a chart whose whole job is to show which boxes an analysis went
  through, a dash reads as "provisional" or "not reached yet" instead. That box
  now says *"A pmatools input; Figure 5 has no such node."* on a third line, and
  the study-count box — computed from the analysis, never put to the reviewer —
  says *"Computed from the analysis, never asked."* on its own. The
  `.pma-fc-pmatools` class and its `stroke-dasharray` rule are deleted from
  `data-raw/build_figures.R`, along with `fc_box()`'s now-unused `extra_class`
  argument. **No node or edge id changed**, so no `flow_path` fact moved and
  nothing downstream needs updating; the SVG grew from 592 to 626 units and
  every coordinate below the registry box shifted down.

* **The app's publication-bias chart lights the study-count node and the edge
  out of it.** Following from the two-answer question above, a `"no"` now walks
  on to the k gate instead of ending the wizard, and
  `step3_pubias_flow_ids()` lights `pma-pubias-node-q2` together with
  `pma-pubias-edge-q2-yes` or `-q2-no`. That node is the one the reviewer is
  never asked about, so the edge is the only thing that says which branch the
  study count chose for them. `assess_pubias()`'s own `flow_path` already
  recorded both.

* **A forest plot's title moves out of the column-header row onto its own line
  above it.** `plot_forest(title =)` used to reach `meta::forest()` as `smlab`,
  which `{meta}` draws inside the header row, centred over the forest column. A
  title wider than that column overran its neighbours and rendered as
  `EvenDepression response (stratified by Risk of Bias)GR (95% CI)` — reported
  on the risk-of-bias stratified plot, where the outcome name is already a
  sentence before the plot appends its suffix, and reproducible from any long
  `forest_display$title`. The title is now word-wrapped to the device width and
  drawn above the headers, anchored to the top of the block `meta::forest()`
  reports, so it stays with the plot on any canvas. `plot_forest_rob()` and
  `plot_forest_indirectness()` inherit the fix; no suffix was shortened, and
  titles that already fitted are unmoved apart from sitting one line higher.

* **Every reference is written the same way, and none of them is a link.** The
  house style is now first author, `et al.`, journal abbreviation, year —
  `Furukawa Y, et al. J Affect Disord. 2024` — with no volume, no pages, no DOI
  and no hyperlink. It applies to everything either artifact renders: the
  flextable footnotes on the Evidence Profile, both Summary of Findings
  layouts, both `grade_table()` layouts and the Indirectness table; the `.docx`
  header paragraphs of `grade_report()` and `export_bundle()`; the caveat
  strings that reach `notes`; and, in the app, the reference line on all five
  Step 3 domain tabs, the Step 1 sample-dataset line, the Step 2 rare-events
  references, the RoB-ME notes and the Step 4 "How to cite" card. The six BMJ
  2025 Core GRADE papers are all Guyatt, all BMJ, all 2025, so the bare form
  collapses them into one indistinguishable string; a specific paper carries
  its series number as a prefix instead — `Core GRADE 4. Guyatt G, et al. BMJ.
  2025`. The phrase "BMJ 2025 Core GRADE series (Guyatt et al.)" existed as
  eight literals across seven files with four different lead-ins ("Reference:",
  "Based on the", "Assessment based on", "rated with the"); it is now one
  internal constant, `.PMA_CORE_GRADE_FOOTNOTE` in `R/utils.R`. Nothing about
  what any of these documents *says* changed, only how the citation in it is
  set. Short parentheticals that point at a figure — "(Core GRADE 4 Fig 2)" —
  are pointers rather than citations and are untouched. In the app,
  `pma_reference()` loses its `doi` argument, `EDU_COPY$domains$*$ref_text` and
  `$doi` collapse to a single `$ref`, and the unused `EDU_COPY$pmid_url()` is
  deleted; a regex test over every `$ref` now pins the format. See SPEC.md
  §"Citation style" and `shiny/SPEC.md` §3.4.11.

* **Every table is set in one font, and the app's tables are set in the app's
  font.** Two mismatches, one of them a live bug. (1) `add_footer_lines()`
  creates its rows *after* `font(part = "all")` has run, and a row added later
  does not inherit the table's family — it takes flextable's own default. So
  every footnote pmatools has ever rendered came out in Helvetica under an
  Arial body, in the Evidence Profile, both Summary of Findings layouts, both
  `grade_table()` layouts and the Indirectness table. The family is now
  re-applied to the footer once the notes are in place, by the shared
  `.style_table_footer()`, so it cannot drift from the body again whatever
  order the calls come in; the domain-detail table in `grade_report()`, which
  named no family at all, is set with the rest. The family itself is one
  internal constant. (2) The exported .docx keeps **Arial** — a word processor
  resolves a named face, and changing it would change every document pmatools
  has produced — but the app's *on-screen* copy of the same table now inherits
  the page's `--font-sans` through a CSS rule on flextable's `.tabwid`
  wrapper, so a Summary of Findings preview no longer sits in the page like a
  quotation from another document. Sizes, colours, borders and column widths
  are untouched in both media. Also in the app: the Core GRADE 6 "Not
  implemented in this table" statement was printed twice under every Summary of
  Findings — once as page text and once in the table footer, in two different
  fonts — and only the footer copy is kept, since that is the one that travels
  into the export. `--muted-foreground` is darkened from 47% to 40% lightness
  (4.72:1 to 6.08:1 on white); nearly everything it colours is set below
  0.875rem. See `SPEC.md` §4.6 and `shiny/SPEC.md` §4.1.

* **The Shiny app's Steps 1 and 2 stop lecturing and start fitting on a
  phone.** Three changes, all user-visible, none breaking. (1) Every step used
  to open with a paragraph describing the step, and Step 1 with a further note
  that pooling is only a small part of a systematic review — reprinted verbatim
  in Step 4's "How to cite" card. The paragraphs are deleted and the note is
  now a modal shown **once per session**, guarded by a session-scoped
  `reactiveVal` rather than `localStorage`, so a returning reviewer sees it
  again but a reviewer changing step does not. (2) Step 2's sidebar is four
  `bslib` accordion panels — Outcome, Data mapping, Model details, Subgroup —
  with only what needs an answer open, and the *Run analysis* button and
  *Auto-rerun* checkbox in a bar stuck to the bottom of the card instead of
  below a column of controls taller than the viewport. Every input id is
  unchanged. A blank required column select opens the panel hiding it, using
  the existing required-field message rather than a second mechanism. (3) Step
  2's two columns can now shrink: at a 375px viewport the document no longer
  scrolls sideways. Also: the right pane says "Press **Run analysis** to pool
  the studies" before the first run instead of showing three empty tabs, and
  the running pmatools version moved from inside Step 2's "Text results" tab to
  the page footer. See `shiny/SPEC.md` §3.1.1, §3.2.2, §3.3.1 and §3.3.3.

* **`inconsistency_subgroup_explained` now works on the automated path**, which
  is where the domain notes had been telling reviewers to use it all along.
  When `inconsistency_ci_diff` is `NULL` the automated zone tally runs, and its
  opposite-sides branch has always written "Supply
  `inconsistency_subgroup_explained = 'yes'` to override" — advice that was a
  no-op: answering it switched the domain onto the manual flowchart, which then
  aborted unless `inconsistency_threshold_side` was supplied too, and that is
  the very thing the automated tally had just derived. `.auto_inconsistency()`
  now takes the argument. On the opposite-sides branch `"yes"` returns `"no"`
  (do not rate down; present the subgroups separately) with
  Core GRADE 3's ICEMAN credibility caveat attached, `"no"` returns
  `"some_concerns"` saying so, and leaving it unanswered is unchanged. On the
  other two automated branches Core GRADE 3 never reaches Step 3, so an answer
  there changes nothing. The value is validated on both paths. Callers that do
  not pass it see no change at all. See SPEC.md §5.2.

* The default `style` of the single-outcome bundle changed from GRADEpro to
  `"bmj"`, matching `export_bundle.pmatools_set()`, which has defaulted to the
  BMJ layout since v0.5.0. One rule now holds for both: a bundle ships the
  Core GRADE layout unless asked otherwise. `sof_table()` and `grade_table()`
  are unchanged and still default to `"gradepro"`, so only the exported
  `sof_table.docx` (and the appendix's embedded table) moves. Pass
  `style = "gradepro"` to `export_bundle()` to keep the old layout.

* **Imprecision now knows which way the outcome runs.** `grade_meta()` forwards
  `small_values` to `assess_imprecision()` as well as to `assess_rob()`, and the
  binary OIS alternative rate follows it: Core GRADE 2's "modest relative risk
  reduction" is written for an undesirable event, so an outcome whose events are
  the desirable thing (`small_values = "undesirable"` — response, remission) is
  now powered against `ois_p0 * (1 + ois_rrr)` rather than
  `ois_p0 * (1 - ois_rrr)`. On the bundled
  CBT-I example that moves the OIS target rate from 125 to 187 per 1,000, which
  changes the OIS and can change the Fig 4 verdict. `ois_p1` is clamped into
  (0, 1) and the note says so if it clamps. **Callers that do not pass
  `small_values` are unaffected**: with `small_values = NULL` the reduction is
  used exactly as before. New fact key `ois_target_rate`; see SPEC.md §5.5.
* Continuous outcomes get an OIS. `ois_sd` now falls back to
  `compute_pooled_sd(meta_obj)` when the caller supplies none, instead of
  leaving `.calc_ois()` without a standard deviation. Previously every
  continuous outcome that reached Fig 4's large-effect path found no OIS and
  landed on "do not rate down" with no explanation. New fact key
  `ois_sd_source`; where the OIS is still unavailable, the Fig 4 path string
  now names the input that was missing.

## Bug fixes

* Imprecision's large-effect note called every ratio-scale effect a "relative
  risk reduction", so a pooled odds ratio of 2.33 — an increase — was reported
  as "relative risk reduction 57%". The magnitude was right (the statistic is
  symmetric) but the wording was direction-blind; an effect above the null is
  now labelled a relative risk increase, given as the equivalent reduction with
  the arms exchanged.
* Three places still stated that risk-of-bias rule 5 (the pooled estimate's
  zone flipping across the null) rates down **two** levels. It has been capped
  at one level since v0.5.0, which `.assess_bias_direction()` implements and
  `.ROB_CAP_NOTE` explains. SPEC.md §5.1's rule table, the duplicated table in
  the `R/domain_rob.R` block comment, and the Shiny app's own "How is this
  judged?" copy are corrected.
* Shiny app: an analysis could be withdrawn silently and never come back.
  Editing a per-study risk-of-bias or indirectness cell in Step 3 re-ran Step
  1's commit observer, which nulled `state$ma` and `state$grade`
  unconditionally; nothing restored them, because `observeEvent(ma())` returns
  early on `NULL` and a Step 3 → Step 2 → Step 3 round trip resets the "Run
  analysis" button to 0. The commit now nulls them only when the dataset
  signature actually changes — a risk-of-bias relabel is a property of the
  studies, not of the outcome. Alongside it: clearing a required Step 2 field
  now records what is missing, so the Final certainty tab, the SoF preview, the
  incomplete banner and the outcome-name echo say *which* field instead of
  printing "Run analysis and configure domains." (or a bare `"..."`) at a
  reviewer who had run one; the outcome type is sticky across step changes
  rather than silently reverting to Binary, which used to send a continuous
  analysis into the binary OIS branch; and the five domain panels no longer
  keep a stale evaluation painted while a later tab is on screen.
* Two internal helpers were both named `.total_n()`, one in
  `R/domain_imprecision.R` and one in `R/sof_table.R`. Because R collates
  `R/*.R` alphabetically, the `sof_table.R` definition silently won
  package-wide and the imprecision one never ran. The two differ on single-arm
  meta-analyses: the display version falls back to `meta_obj$n` (the total a
  `metaprop()` or `metamean()` records), whereas the imprecision version
  deliberately returns `NA` there. `assess_imprecision()` was therefore
  applying Core GRADE 2 Fig 4's "total N >= 800 (400 per group)" rule of thumb
  to single-arm meta-analyses, where no per-group total exists at all. The
  imprecision helper is now called `.total_n_strict()`, so each call site gets
  the semantics it needs. In the affected cases the certainty *judgment* is
  unchanged — those analyses now fall through to the "OIS could not be
  computed -> do not rate down" branch, which is also `"no"` — but the
  imprecision **note text** changes, since it no longer reports a rule of thumb
  that was never applicable. The Summary of Findings and evidence profile
  N columns are untouched and still show the real N for single-arm analyses.
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
* The multi-outcome bundle's `analysis.R` dropped the arm labels.
  `export_bundle()` passed `label_intervention` / `label_control` to
  `grade_table()` for `summary_of_findings.docx` but rendered no such arguments
  into the script, so a bundle from a review of CBT-I against placebo shipped a
  table headed "With placebo" / "With CBT-I" alongside a script that rebuilt
  every number of it under "With control" / "With intervention", with
  "Treatment" as the plain-language subject. The labels are now rendered onto
  the script's `grade_table()` call; ones left at their defaults are omitted, so
  a bundle that named no arms gets the script it always did.

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

* `results.txt` in an exported bundle headed its pooled estimate
  `[ Meta-analysis summary ]` even after a Core GRADE 4 Fig 2 low-risk-of-bias
  refit, so the block could report the all-studies analysis while the certainty
  assessment printed below it was computed on the low-RoB subset. The heading
  now names the analysis set (`- all studies (4 studies; NOT the analysis rated
  below)` / `- low risk of bias studies only (3 of 4 studies; rated below)`),
  and when the caller passes the all-studies object the rated analysis is
  printed as a second, separately headed block. Without a refit the heading is
  unchanged.
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
