# pairwise_meta_analysis — Shiny App Specification

> Authoritative specification for the pairwise_meta_analysis Shiny app (v2 — wizard refactor). The package-level R logic (data ingestion, MA pipeline, GRADE assessment, SoF table, export) is provided by [pmatools](../pmatools/SPEC.md). This app is a **UI layer** on top of pmatools.

**Public URL:** https://yuki-furukawa.shinyapps.io/pairwise_meta_analysis/
**Deployment:** shinyapps.io (account: `yuki-furukawa`, appId: `15217423`)
**Target version:** 2.0.0 (succeeds the existing single-page app)

---

## 1. Scope

This app provides a **4-step wizard** for end-to-end pairwise meta-analysis with GRADE certainty assessment:

1. **Data** — load + map + validate study-level data
2. **Meta-analysis** — pool effect estimates, render forest/funnel
3. **GRADE** — five-domain certainty assessment with educational explanations
4. **Export** — download a reproducible ZIP bundle

The app is **entirely a thin UI**: every analytical operation calls a function in `pmatools`. No statistical logic should be re-implemented here.

The app is also **educational**: every step explains what it does, every input has tooltip help, and every GRADE domain explains its algorithm in plain American English with citations to the BMJ 2025 Core GRADE series.

---

## 2. Architecture

### 2.1 Dependencies

| Package | Source | Role |
|---|---|---|
| pmatools | github::ykfrkw/pmatools | All MA + GRADE + SoF + export logic |
| shiny | CRAN | Reactive framework |
| bslib | CRAN | Bootstrap 5 theming, accordion, cards |
| htmltools | CRAN | HTML helpers, tooltips |
| DT | CRAN | Editable data table for Step 1 |
| flextable | CRAN | SoF preview rendering |
| meta | CRAN | Required transitively (also direct for any Shiny-only uses) |

`pairwise_meta_analysis/DESCRIPTION` is added (lightweight, project-only):

```dcf
Type: Project
Package: pairwise.meta.analysis.app
Title: pairwise_meta_analysis Shiny app
Version: 2.0.0
Imports:
    shiny,
    bslib,
    htmltools,
    DT,
    flextable,
    pmatools,
    meta
Remotes:
    github::ykfrkw/pmatools
```

This file is consumed by `rsconnect::deployApp()` to resolve `pmatools` from GitHub.

### 2.2 File layout

```
pairwise_meta_analysis/
├── app.R                          # entrypoint: ui + server
├── DESCRIPTION                    # rsconnect dependency manifest
├── R/
│   ├── step1_data.R               # data import module (UI + server)
│   ├── step2_ma.R                 # MA module
│   ├── step3_grade.R              # GRADE module (5 accordion sub-modules)
│   ├── step4_export.R             # export/download module
│   ├── ui_helpers.R               # shadcn-style component helpers (card, badge, stepper)
│   └── educational_copy.R         # American English copy as named-list constants
├── www/
│   ├── shadcn.css                 # design tokens + component CSS
│   └── pmatools_logo.svg          # optional branding
├── rsconnect/                     # existing (do not touch)
├── SPEC.md                        # this file
└── (legacy files kept for history): app_1407.R, app_20250817.R, ...
```

Legacy files are listed in the existing `ignoredFiles` field of `rsconnect/shinyapps.io/yuki-furukawa/pairwise_meta_analysis.dcf` and remain untouched.

### 2.3 Reactivity model

A single top-level reactive store:

```r
state <- reactiveValues(
  step      = 1L,                  # current step (1–4)
  data      = NULL,                # tibble (long format) from pmatools::ingest_data()
  data_raw  = NULL,                # original input pre-mapping
  ma_args   = list(),              # arguments passed to run_ma()
  ma        = NULL,                # meta object from pmatools::run_ma()
  ma_blocked= NULL,                # chr: the Step 2 fields that withdrew it
  outcome_type = NULL,             # "binary" / "continuous", mirrored from Step 2
  grade_args= list(),              # arguments passed to grade_meta()
  grade     = NULL,                # pmatools object from pmatools::grade_meta()
  display   = list(                # SoF display options
    per                = 1000,
    prediction         = FALSE,
    convert_smd_to_or  = FALSE,
    baseline_risk      = NULL,
    threshold_label    = NULL
  )
)
```

Step transitions (`Next` / `Back` buttons) update `state$step`. Each step's UI is conditionally shown via `conditionalPanel(condition = "input.step == 'N'", ...)`.

**Re-computation rules:**

- `state$data` recomputes when Step 1 inputs change (debounced 300ms).
- `state$ma` recomputes only when user clicks **"Run analysis"** in Step 2.
- **`state$ma` is withdrawn only by something that genuinely invalidates it.** Step 1's commit observer depends on `state$rob_table` (so a Step 3 risk-of-bias edit cannot revert Step 1 cell edits), which means it re-runs on every RoB edit; it nulls `state$ma` / `state$grade` **only when `pma_dataset_signature()` changes**. A per-study RoB or indirectness relabel is a property of the studies, not of the outcome — the same contract `begin_new_outcome()` states — and the signature already excludes those two columns. Before v0.5.1 the null was unconditional and unrecoverable: `observeEvent(ma())` returns early on `NULL`, and after a Step 3 → Step 2 → Step 3 round trip `input$run_ma` is a rebuilt button reporting 0, so `ma()` never re-ran.
- **A withdrawn analysis is never silent.** Every path in `ma()` that returns `NULL` after a successful run has been recorded either notifies or records why. Missing required inputs set `state$ma_blocked` to a character vector of Step 2 field labels; arm labels absent from the data raise a notification once `state$regular_ma` exists.

#### `state$ma_blocked`

`NULL` when no analysis has been attempted or when one succeeded; otherwise the Step 2 fields that were empty when the analysis was withdrawn (`"Outcome name"`, `"Direction (smaller = favorable?)"`, `"Events column"`, …). Written by `ma()` in `R/step2_ma.R`; cleared by **every** writer that sets a non-`NULL` `state$ma`.

Four Step 3 outputs read it, all through the pure helpers in `R/step3_threshold.R` so the wording cannot drift:

| output | with a block recorded | otherwise |
|---|---|---|
| `final_certainty` | amber box naming the missing fields | amber "no threshold" box, or the plain idle line |
| `sof_preview` | the same amber box | the SoF, or the plain idle line (was a bare `"..."`) |
| `cert_incomplete_banner` | amber "Assessment blocked" | the unconfirmed-domains banner |
| `outcome_name_echo` | `"(cleared in Step 2)"` plus a line naming the missing identity fields | the mirrored `state$outcome_name` |

`state$outcome_name` is mirrored only on a successful run and is deliberately never cleared, so the echo must read the live block rather than the mirror; otherwise it prints an outcome name that is no longer in the form.

**Sticky `state$outcome_type`.** Binary/continuous is mirrored into state and re-seeded into the Step 2 radio, like `outcome_name` and `small_values`, because a rebuilt widget otherwise pushes its hard-coded `"binary"` default back on every 3 → 2 → 3 round trip. `grade_obj()` derives `outcome_type` for `grade_meta()` from the fitted object via `step3_is_binary_outcome()` rather than from the raw input.
- `state$grade` recomputes whenever `state$ma` is set OR any Step 3 input changes (debounced 500ms; cheap to compute).
- Forest/funnel plots are rendered from `state$ma`.
- SoF preview rendered from `state$grade` and `state$display`.

---

## 3. Wizard structure

### 3.1 Top-level chrome

- **Header**: app title "pmatools — pairwise meta-analysis with GRADE", small nav with link to docs (pmatools README on GitHub) and shinyapps.io status.
- **Stepper**: horizontal 4-step indicator under the header. Steps shown as: `1 Data — 2 Meta-analysis — 3 GRADE — 4 Export`. Current step bold + filled circle; completed steps green check; future steps muted.
- **Footer**: small print "Powered by yukifurukawa.jp/pmatools/", followed by the running pmatools version from `pma_pmatools_version()` (`R/ui_helpers.R`). The version is footer chrome because it belongs to the whole session, not to one step; it was previously reachable only by opening Step 2's "Text results" tab, which is no place to look for the version of the tool you are about to cite.

#### 3.1.1 Orientation modal

`EDU_COPY$intro_modal` (title / body / dismiss) is shown from `app.R`'s server
body with `showModal(modalDialog(...))`, **once per session**, before the
reviewer touches anything.

It carries the one claim in the app that is about the work *around* the
analysis: statistical pooling is a small part of a systematic review, which
also needs a prespecified and pre-registered protocol, a comprehensive search,
dual independent screening and extraction, and risk-of-bias assessment, all
completed before the analysis.

Two rules:

- **Session-scoped guard, never client storage.** A `reactiveVal(FALSE)` in the
  server body. No `localStorage`, no cookie: a returning reviewer is a new
  session rating a new review, not someone who has already been told today.
- **It must not re-appear on a step change.** The guard is set before the modal
  is shown, and the observer that shows it takes no reactive dependency on
  `state$step`.

The text was formerly `EDU_COPY$steps$step1$why`, rendered as body copy at the
top of Step 1 and restated verbatim in Step 4's "How to cite" card. Both copies
are gone.

### 3.2 Step 1 — Data

#### 3.2.1 Layout

Single column (`bslib::page_fluid`), 3 cards stacked:

1. **Load data** — input source selection
2. **Column mapping** — visible after data is loaded
3. **Preview & edit** — `DT::DTOutput` with editable cells

Below: the step header, and a **Next →** button (disabled until `state$data` is non-NULL and validates).

#### 3.2.2 Step header

The title, and nothing else — `pma_step_header(EDU_COPY$steps$step1$title)`.

`pma_step_header()` takes a title and no other argument. Every step used to
open with a `$what` paragraph describing the step and, on Step 1, a `$why`
note; all five fields are deleted. The paragraphs were re-read on every visit
to the step, pushed the first control below the fold, and said much the same
thing four times. What was genuinely once-per-session became §3.1.1's modal;
what described a single control now sits beside that control.

#### 3.2.3 Inputs

**Card "Load data":**

- `radioButtons("input_method", label, choices = c("Upload file" = "file", "Paste from Excel" = "paste", "Use sample dataset" = "sample"), selected = "sample")`
- If `input_method == "file"`: `fileInput("data_file", accept = c(".csv", ".tsv", ".xlsx"))`
- If `input_method == "paste"`: `textAreaInput("data_paste", rows = 8, placeholder = "Paste tab- or comma-separated data here...")`
- If `input_method == "sample"`: read `system.file("extdata/cbti_depression.csv", package = "pmatools")`
- `radioButtons("data_format", "Data format", c("Auto-detect" = "auto", "Long" = "long", "Wide" = "wide"), selected = "auto")`

**Card "Column mapping":**

Shown only after data is loaded. For each canonical column required by pmatools (studlab, treat, n, event/mean/sd, rob, indirectness, subgroup), render a `selectInput()` whose choices are the column names of the loaded data. Pre-fill with best-guess matches (`studlab` ← `study|study_id|trial`, etc.).

For wide format, mapping prompts for `studlab, n_e, n_c, event_e, event_c` (binary) or `n_e, n_c, mean_e, mean_c, sd_e, sd_c` (continuous).

**Card "Preview & edit":**

`DT::DTOutput("data_preview")` rendered from the post-mapping long tibble. Cells are editable (so RoB / Indirectness columns can be added or corrected). Edits write back to `state$data`.

#### 3.2.4 Validation

When the user clicks **Next →**:

- Call `pmatools::ingest_data(state$data_raw, format = state$format, mapping = state$mapping)`.
- On error: render error message in a Bootstrap alert above the data preview; do not advance.
- On success: store result in `state$data`, advance to Step 2.

#### 3.2.5 Why this matters

Not a screen. The Step 1 "why this matters" copy is §3.1.1's once-per-session modal.

### 3.3 Step 2 — Meta-analysis

#### 3.3.1 Layout

Two columns in one flex row: left sidebar with model controls, right pane with tabbed plots and result text.

Both columns must be able to shrink. The sidebar is `flex: 1 1 320px` and the
right pane `flex: 1; min-width: min(480px, 100%)`. A fixed `flex: 0 0 320px`
basis and a flat `min-width: 480px` floor were what made a 375px viewport
render a 492px document and scroll the whole page sideways; the rule the app
has to satisfy is `document.scrollWidth <= document.clientWidth` at 375px.
`tests/testthat/test-step2-layout.R` pins both declarations — there is no
browser driver here, so what is asserted is the CSS, not the measurement.

#### 3.3.2 Step header

The title, and nothing else. See §3.2.2.

#### 3.3.3 Inputs (sidebar)

One `pma_card("Model configuration")` holding a `bslib::accordion(multiple = TRUE)` of four panels, then a sticky action bar.

| Panel (`value`) | Contents | Open on build |
|---|---|---|
| Outcome (`outcome`) | `outcome_name`, `small_values`, `outcome_type`, `outcome_filter_ui`, `outcome_follow_up`, `outcome_unit` (continuous only) | always |
| Data mapping (`mapping`) | `col_studlab`, `col_treat`, `arm_assignment_ui`, `col_n`, `col_event` (binary) / `col_mean` + `col_sd` (continuous) | while `state$ma` is NULL |
| Model details (`model`) | `sm_bin` / `sm_cont_ui`, `model`, `method`, `method_tau`, `incr` | never |
| Subgroup (`subgroup`) | `subgroup_col`, `subgroup_order_ui` | never |

- **Outcome type is identity, not mapping.** `outcome_type` and
  `outcome_filter_ui` sit in the Outcome panel: they say *which* outcome is
  being rated, and `outcome_type` decides which of the panel's own optional
  fields (`outcome_unit`) applies.
- **Data mapping's open state is decided at build time from `state$ma`**, not
  from the selects themselves — the selects are populated by the server
  *after* this UI is built, so at build time they are all blank whatever the
  data holds. A non-NULL `state$ma` is proof the mapping resolved, so the
  panel stays shut on every return trip from Step 3. A select that is blank
  when the reviewer actually asks for an analysis is handled from the other
  end, by `www/required-fields.js` (§3.3.6).
- **Every input id is unchanged by the restructure.** Step 3 reads most of
  these off `input$` directly, so a rename is silent everywhere else;
  `tests/testthat/test-step2-layout.R` asserts each id renders.
- **Sticky action bar** (`.pma-step2-actions`, `position: sticky; bottom: 0`)
  closing the card, holding `actionButton("run_ma")` and
  `checkboxInput("auto_rerun")`. The sidebar is taller than a laptop viewport
  with every panel open, and the primary action used to sit at the bottom of
  it, so changing a model setting meant scrolling back down to act on it.

#### 3.3.4 Outputs (right pane)

Tabset with 3 tabs:

- **Forest plot** — `plotOutput("forest", height = "auto")` rendered via `pmatools::plot_forest(state$ma, title = input$forest_title, label_e = ..., label_c = ..., xlim = parsed_input)`. Auto-height: `350 + 30 * k` pixels.
- **Funnel plot** — `plotOutput("funnel")` via `pmatools::plot_funnel(state$ma, show_egger = TRUE)`.
- **Text results** — `verbatimTextOutput("ma_summary")` showing `summary(state$ma)`.

Below the tabset: collapsible "Forest plot adjustments" with title, label_e, label_c, xlim min/max overrides.

Before the first run the tabset would be three empty tabs, so the card is
**hidden** and one line shows in its place: "Press **Run analysis** to pool the
studies." The swap is two `conditionalPanel()`s on a single server flag,
`output$pma_has_ma` (`reactive(!is.null(state$ma))`, with
`outputOptions(suspendWhenHidden = FALSE)` because the flag is never itself on
screen). It is deliberately not a `renderUI()` swap: the Results card holds the
forest- and funnel-display widgets, and re-rendering it would reset every value
typed into them — the same hazard `output$step2_nav` exists to avoid. The
conditions are written so the placeholder, not the empty card, is what shows
before the flag has arrived from the server.

#### 3.3.5 Why this matters

Not a screen; see §3.2.5.

#### 3.3.6 Required fields — two visual tiers

`PMA_STEP2_REQUIRED` (`R/ui_helpers.R`) names the two fields Step 2 cannot
proceed without: `outcome_name` and `small_values`. Which of them is still
blank is decided by the pure `pma_step2_required_unset()`, so the rule is
testable without a session; `www/required-fields.js` paints it.

The **column-mapping selects ride on the same message**, and there is
deliberately no second mechanism for them. `PMA_STEP2_MAPPING_ALL` is what the
message declares it manages and is fixed; `pma_step2_mapping_required()` names
the subset that applies to the current outcome type (`col_event` for binary,
`col_mean` + `col_sd` for continuous) and `pma_step2_mapping_unset()` the
subset of that which is blank. `ALL` stays fixed because the client caches a
flag per id: an id dropped from the list would keep its last mark rather than
lose it when the outcome type changes.

Because §3.3.3's accordion can hide a blank select, `required-fields.js` also
**opens the panel** containing one — but only when `armed`, and only once per
panel per DOM build. Before the reviewer has asked for an analysis the panel
state is theirs, and every mapping select is legitimately blank for the first
few hundred milliseconds of each build while the server populates it; opening
on that would fight the user and flash on every return from Step 3. The
once-only latch lives on the panel element, which the `renderUI` rebuild throws
away with the rest of the DOM.

| state | classes | appearance |
|---|---|---|
| blank, from the first render | `.pma-required-unset` | **muted** "required" pill on the label |
| blank, after a failed Next / Run analysis | `.pma-required-unset.pma-required-armed` | destructive pill, border and option-group rule |

The split exists because the two behaviours were in conflict. Painting a fresh
form red is wrong — nobody has done anything wrong yet — but the previous
single tier armed only after a failed Next, so a fresh form said **nothing at
all** about what was required. The muted tier is legible from the start; the
armed tier is the old destructive treatment, unchanged, and `armed` is the same
`required_touched()` flag as before, now sent to the client as a third field of
the `pma_required_fields` message. Both classes are cached on `window`
alongside the per-id flags, because `app.R` rebuilds the Step 2 body on every
step change and throws the DOM away.

#### 3.3.7 Sample-dataset outcome defaults

`PMA_SAMPLE_OUTCOME_DEFAULTS` in `R/step1_data.R` is a named list keyed by
`input$sample_dataset`, holding the outcome name, direction and follow-up each
bundled sample is an analysis *of* (`regular` → "Depression response" /
`undesirable` / "Post-treatment"). Seeded into `state` by an
`observeEvent(input$load_data)`, so it fires once per load.

Two rules:

- **Blanks only.** A field the reviewer has already filled is never
  overwritten, however many times they reload.
- **Hooked to the load path, not to `commit_loaded_data()`.** That function is
  called from an observer that depends on `state$rob_table`, so it re-runs on
  every per-study Risk-of-Bias edit made in Step 3.

`step2_ui()` already seeds all three widgets from `state`, so nothing in Step 2
changes.

### 3.4 Step 3 — GRADE

#### 3.4.1 Layout

Top: Step header. Then a `bslib::accordion` with 5 panels (one per domain). Below: a sticky **Final certainty** summary card and **Display options** card.

#### 3.4.2 Step header copy

> **Step 3: GRADE.** This step rates the **certainty of evidence** — your confidence that the estimate above reflects the true effect. GRADE starts at *High* for randomized trials (or *Low* for observational studies) and rates **down** for concerns in five domains: **Risk of Bias**, **Inconsistency**, **Indirectness**, **Imprecision**, and **Publication bias**. Each domain below explains what it checks, shows the algorithm's automatic judgment, and lets you override if your clinical judgment differs. The only domain that requires your input is **Indirectness** — it cannot be derived from your data.

#### 3.4.3 Common accordion panel structure

```
[Domain Name] [auto-judgment badge]
─────────────────────────────────────
How this is judged:
  <2-4 sentence American English explanation>
  Reference: BMJ Core GRADE N (PMID ...)

Auto-evaluation result:
  • <bullet 1: input data>
  • <bullet 2: derived statistic>
  • <bullet 3: ...>
  Resulting judgment: <None | Some | Serious | Very serious>

[Adjust this domain (optional) ▾]   ← collapsed by default
  <override controls>
```

For Indirectness, the structure is:

```
[Indirectness] [⚠ Auto-defaulted to "No" — please review]
─────────────────────────────────────
How this is judged:
  <explanation>

⚠ Banner: Auto-defaulted to "No" — please review and confirm or override below.

Confirm or override (defaults to No):
  ( ) No (default)  ( ) Some  ( ) Serious  ( ) Very serious

[Educational sub-prompts (optional, do not affect rating) ▾]  ← collapsed
  <PICO sub-questions>
```

#### 3.4.4 Educational copy (final, American English) — RoB

Stored in `R/educational_copy.R` as `EDU_COPY$rob$how_judged`:

> **How this is judged.** GRADE rates down for risk of bias when the body of evidence is *dominated* by studies at high risk of bias. The algorithm runs a two-step check. **Step 1**: it calculates the share of total random-effects weight that comes from high-RoB studies. If this share exceeds the dominance threshold (default 60%), the evidence is considered dominated. **Step 2**: it checks whether removing the high-RoB studies would meaningfully change the estimate. The pooled estimate including all studies (TE_all) is compared with the pooled estimate restricted to low or some-RoB studies (TE_low). The relative inflation is `(|TE_all| − |TE_low|) / |TE_low|`. If this exceeds your inflation threshold (default 10%), we rate down for risk of bias. A small change is treated as random variation, not a true effect of bias. Reference: BMJ Core GRADE 4 (Guyatt et al., 2025).

**Auto-evaluation result template:**

```
• Per-study RoB labels: from Step 1 `rob` column ({{n_studies_with_rob}} of {{k}} have labels)
• High-RoB random-effects weight share: {{weight_share_pct}}% (threshold {{dom_threshold_pct}}%) → {{"dominated" | "not dominated"}}
• |TE_all| = {{TE_all}}, |TE_low| = {{TE_low}}, relative inflation = {{inflation_pct}}% (threshold {{inf_threshold_pct}}%) → {{"rate down" | "no rate-down"}}
Resulting judgment: {{judgment}}
```

**Override controls (collapsed by default):**

- `selectInput("rob_override", "Override RoB judgment", c("(use auto)" = "", "No" = "no", "Some" = "some", "Serious" = "serious", "Very serious" = "very_serious"))`
- `sliderInput("rob_dom_threshold", "Dominance threshold", min = 0.5, max = 0.7, value = 0.6, step = 0.05)`
- ~~`sliderInput("rob_inf_threshold", …)`~~ — deleted in 0.5.1; see §3.4.11
- `radioButtons("small_values", "Small values are...", c("Desirable (e.g., mortality, severity)" = "desirable", "Undesirable (e.g., response rate)" = "undesirable", "(use auto)" = ""))`

#### 3.4.5 Educational copy — Inconsistency

> **How this is judged.** GRADE rates down for inconsistency when there are *important differences in effect across studies* AND those differences cannot be explained. The BMJ Core GRADE 3 flowchart asks three questions in sequence. **Step 1**: Are there important differences in point estimates AND limited overlap of confidence intervals? If no, do not rate down. If yes, continue. **Step 2**: Where do the point estimates fall relative to the **clinical decision threshold** (the MID)? If a clear majority sits on one side of the threshold, the direction of effect is consistent — do not rate down. If a substantial proportion fall on opposite sides, continue. **Step 3**: Can the opposite-sided difference be explained by a credible subgroup analysis (e.g., RCTs vs observational, adults vs children)? If yes, present the subgroups separately and do not rate down; if no, rate down for serious inconsistency. I² is shown as *supportive context only* — the decision is anchored in clinical judgment about whether the differences are important and whether the directions are consistent. (τ² and Q-test results are also displayed for transparency but never drive the judgment.) Reference: BMJ Core GRADE 3 (Guyatt et al., 2025).

**Auto-evaluation result template:**

```
Method: auto (statistical proxies for the BMJ flowchart)

AUTO Step 1: I² = {{i2_pct}}%
  → {{"important differences detected (I² > 25%)" | "no important heterogeneity (I² ≤ 25%)"}}

{{#if has_mid}}
AUTO Step 2 (vs ±MID = ±{{mid}}):
  Zone counts (k = {{k}}): above_mid = {{n_above}}, trivial = {{n_trivial}}, below_mid = {{n_below}}
  Largest one-side proportion = {{pct_one_side}}%
  → {{"majority on one side" | "opposite sides"}}
{{else}}
AUTO Step 2 (vs null = 0; MID not specified):
  {{pct_positive}}% of point estimates above null
  → {{"majority on one side" | "opposite sides"}}
{{/if}}

AUTO Step 3: subgroup explanation cannot be auto-detected.

Supportive context (display only, not used in judgment):
• I² = {{i2_pct}}%
• τ² = {{tau2}}, Q p = {{q_p}} (supplementary)
• Per-study TE range: [{{te_min}}, {{te_max}}]
• Direction tally: {{n_pos}}/{{k}} positive, {{n_neg}}/{{k}} negative

Resulting judgment: {{judgment}}
```

**Note on auto vs manual asymmetry:** When auto Step 1 detects heterogeneity AND auto Step 2 sees majority on one side, the auto judgment is **"some"** (not "no"). This is because the auto Step 1 (I² > 25%) is a *statistical* proxy and cannot confirm that the differences are clinically important — supplying manual flowchart parameters lets you assert "no important difference" or "majority on one side" with clinical authority, yielding the BMJ-faithful "no" judgment.

**Recommended input — single MID field (always shown, dynamically labeled):**

The label and default value adapt to the meta-analysis effect measure (`meta_obj$sm`). The Shiny app calls `pmatools::suggest_mid(state$ma)` to pre-fill:

| `sm` | Input label | Default | Help text |
|---|---|---|---|
| OR | "MID (as OR ratio, e.g., 1.25 = 25% relative odds change)" | 1.25 | "An OR of 1.25 vs 1.0 represents a 25% relative change in odds — a typical small but clinically meaningful effect." |
| RR | "MID (as risk ratio, e.g., 1.20 = 20% relative risk change)" | 1.20 | "An RR of 1.20 vs 1.0 represents a 20% relative change in risk." |
| HR | "MID (as hazard ratio)" | 1.20 | "An HR of 1.20 vs 1.0 represents a 20% relative change in hazard." |
| RoM | "MID (as ratio of means, e.g., 1.10 = 10% ratio change)" | 1.10 | "A 10% ratio of means is a typical small clinically meaningful difference for continuous outcomes." |
| SMD | "MID (in standardized units, e.g., 0.20 = Cohen's small)" | 0.20 | "Cohen's small effect size (0.20) is widely accepted as the smallest clinically meaningful SMD." |
| MD | "MID (in outcome units, e.g., 3 PHQ-9 points)" | **0.20 × pooled SD = {{computed}}** | "Auto-suggested as 0.20 × pooled SD (Cohen's small in raw units). Replace with a published MID for your outcome if available." |
| ARD | "MID (as absolute risk difference, e.g., 0.05 = 5%)" | 0.05 | "A 5% absolute risk difference is a typical small clinically meaningful effect." |

Note below the input: "*This MID is shared with Imprecision below — enter once, used for both.* The default is a conventional value; please replace with a published or expert-derived MID for your specific outcome whenever possible."

**Advanced (collapsed by default — for users who want to specify scale explicitly):**

- `selectInput("mid_scale", "MID scale (advanced)", c("Auto-detect from effect measure" = "auto", "Already on TE scale (log for ratios)" = "te_scale", "Ratio scale (e.g., OR=1.25)" = "ratio", "Absolute risk difference" = "ard"), selected = "auto")`

**Manual flowchart inputs (BMJ-faithful path; collapsed by default):**

The BMJ flowchart can be driven manually for a fully clinically-informed judgment. Each Step's question is asked in plain language:

- *Step 1: Are there important differences in point estimates AND limited overlap of confidence intervals?*
  - `selectInput("inconsistency_ci_diff", c("(use auto)" = "", "No" = "no", "Yes" = "yes"))`
  - Help: "This is a clinical-visual judgment. Look at the forest plot above: do the point estimates differ by a clinically meaningful amount, and do the CIs fail to overlap substantially?"
- *Step 2: Where do the point estimates fall relative to the clinical decision threshold (MID)?*
  - Visible only if Step 1 = "yes"
  - `radioButtons("inconsistency_threshold_side", c("Majority on one side of MID" = "majority_one_side", "Substantial proportion on opposite sides" = "opposite_sides"))`
- *Step 3: Is the opposite-sided difference explained by a credible subgroup analysis?*
  - Visible only if Step 2 = "opposite_sides"
  - `radioButtons("inconsistency_subgroup_explained", c("Yes" = "yes", "No" = "no"))`

**Override controls (collapsed by default):**

- `selectInput("inconsistency_override", "Override Inconsistency judgment with a single value", c("(use flowchart)" = "", "No" = "no", "Some" = "some", "Serious" = "serious", "Very serious" = "very_serious"))`

#### 3.4.6 Educational copy — Indirectness

> **How this is judged.** Indirectness *cannot be automated* — it requires expert judgment about whether the trial evidence applies to the question of interest. GRADE asks you to consider four things: **Population** (do trial participants resemble the target patients?), **Intervention** (is the intervention deliverable as studied?), **Comparator** (is it representative of usual care?), and **Outcome** (is it patient-important, or a surrogate?). The app defaults to **"No concerns"**, but this is the only domain whose value comes purely from your judgment, so please review before exporting. Reference: BMJ Core GRADE 5 (Guyatt et al., 2025).

**Banner (shown until user clicks any rating, including re-clicking "No"):**

```
⚠ Auto-defaulted to "No" — please review and confirm or override below.
This is the only domain that cannot be informed by your data.
```

**Required (always visible):**

- `radioButtons("indirectness", "Overall indirectness rating", c("No" = "no", "Some" = "some", "Serious" = "serious", "Very serious" = "very_serious"), selected = "no")`

**Educational sub-prompts (collapsed by default, do not affect rating):**

- `radioButtons("indir_population", "Is the trial population sufficiently similar to the target patients?", c("Yes", "Some concern", "Serious concern"))`
- `radioButtons("indir_intervention", "Is the intervention deliverable as studied?", same)`
- `radioButtons("indir_comparator", "Is the comparator representative of usual care?", same)`
- `radioButtons("indir_outcome", "Is the outcome patient-important (vs. surrogate)?", same)`

State logic: `indirectness_reviewed <- reactive(input$indirectness_clicked >= 1)`. The banner is hidden once `indirectness_reviewed()` is TRUE.

#### 3.4.7 Educational copy — Imprecision

> **How this is judged.** Imprecision asks whether the **pooled estimate's 95% confidence interval** is narrow enough to support a clinical decision — distinct from Inconsistency, which asks how much true effects vary across studies. The algorithm checks two conditions: **(a)** does the pooled 95% CI cross the null value? and **(b)** is the **Optimal Information Size (OIS)** met? OIS is the sample size a single well-powered RCT would need to detect the **MID** (the same MID you specified for Inconsistency above — the values are linked). If both conditions are met (CI does not cross null AND OIS is reached), no downgrade. If only one fails, rate down 1 level. If both fail, rate down 2 levels. Reference: BMJ Core GRADE 4 (Guyatt et al., 2025).

**Auto-evaluation result template:**

```
• 95% CI: {{ci}} — {{"crosses null" | "does not cross null"}}
• MID: {{mid_value | "(not specified)"}}
• OIS target: {{ois_target}} {{"events" | "participants"}} (auto-computed); observed: {{ois_observed}} → {{"met" | "not met" | "not assessable"}}
Resulting judgment: {{judgment}}
```

**Override controls (collapsed by default):**

- `numericInput("ois_p0", "Baseline (control) event rate for OIS", value = NA, min = 0, max = 1)` *(binary only)*
- `numericInput("ois_sd", "Pooled SD for OIS", value = NA, min = 0)` *(continuous only)*
- `numericInput("ois_events", "Override OIS — target events", value = NA, min = 0)` *(binary only)*
- `numericInput("ois_n", "Override OIS — target N", value = NA, min = 0)` *(continuous only)*
- `selectInput("imprecision_override", "Override Imprecision judgment", same options)`

#### 3.4.8 Educational copy — Publication bias

> **How this is judged.** GRADE rates down for publication bias when there is reason to suspect that studies with unfavorable results are missing from the synthesis. The algorithm follows a 2-step decision tree. **Step 1**: are the studies mostly small *and* industry-sponsored? If yes, rate down. **Step 2**: with k ≥ 10 studies, run Egger's test and inspect the funnel plot for asymmetry. With k < 10, Egger's test is underpowered, so the algorithm asks instead whether unpublished studies are documented in trial registries or FDA submissions. Reference: BMJ Core GRADE 4 (Guyatt et al., 2025).

**Auto-evaluation result template:**

```
• k = {{k}} studies
{{#if k_ge_10}}
• Egger's test: t = {{egger_t}}, df = {{egger_df}}, p = {{egger_p}} → {{"asymmetric" | "symmetric"}}
{{else}}
• Egger's test not run (k < 10)
{{/if}}
• Small-and-industry-sponsored check: {{small_industry | "assumed 'no'"}}
Resulting judgment: {{judgment}}
```

**Override controls (collapsed by default):**

- `radioButtons("pubias_small_industry", "Are most or all studies small AND industry-sponsored?", c("No" = "no", "Yes" = "yes"))`
- `selectInput("pubias_funnel_asymmetry", "Override Egger with visual funnel inspection", c("(use Egger)" = "", "Funnel asymmetric" = "yes", "Funnel symmetric" = "no"))` *(k ≥ 10 only)*
- `radioButtons("pubias_unpublished", "Are unpublished studies documented in registries or FDA?", c("No" = "no", "Yes" = "yes", "Unsure" = ""))` *(k < 10 only)*
- `selectInput("pubias_override", "Override Publication bias judgment", same options)`

#### 3.4.9 Final certainty summary

Below the accordion, a `pma-card` with:

- Large heading: `Final certainty: {{certainty}} {{symbol}}` (symbol = ⊕⊕⊕⊕ etc.)
- Color-coded by CINeMA pastel palette
- "Why this rating?" expandable showing per-domain judgment + 1-line justification
- Embedded SoF preview rendered from `pmatools::sof_table()` via `htmltools_value(ft)`

#### 3.4.10 Display options card

- `checkboxInput("prediction", "Show 95% prediction interval in Effect column")`
- `checkboxInput("convert_smd_to_or", "Show as dichotomous outcome (Chinn's formula)")` *— SMD/MD outcomes only*
- If `convert_smd_to_or` checked:
  - `numericInput("baseline_risk", "Control event rate (proportion responding)", value = 0.30, min = 0.01, max = 0.99)`
  - `textInput("threshold_label", "Threshold definition (free text)", placeholder = "e.g., ≥50% reduction in PHQ-9 from baseline")`
  - Inline note: *"This is the proportion of control patients who would meet the threshold of clinical interest. Continuous effect sizes are statistically rigorous but hard for clinicians and patients to interpret. Showing 'X out of 1,000 patients respond' alongside the SMD often communicates the same evidence more accessibly."*

#### 3.4.11 Information design — what is open, what is collapsed, what was deleted

> The governing rule, from the reviewer this app is for: **delete first,
> shorten second, hide never.** A muted explanatory sentence survives only if a
> reviewer cannot answer the control it sits under without it. Provenance,
> source-departure notes and "where this number comes from" are deleted
> outright — the flowchart caption and the one-line reference already carry
> them. `<details>` is for *content* (reference plots, per-study grids, verbatim
> tables), never a parking space for prose that failed the first test. A change
> to a Step 3 tab is measured against that rule, not against a character count.

**The operational cap: 25 words.** A `.pma-card-subtitle` is the muted line
under a control, read while the reviewer decides that control; past one desktop
line it stops being read. `EDU_COPY_SUBTITLE_FIELDS` /
`EDU_COPY_SUBTITLE_WORD_CAP` (`R/educational_copy.R`) name the copy-deck strings
that render as one and pin the cap; `test-edu-copy.R` asserts it. Step headers,
the intro modal and the saved-outcome copy are deliberately outside the
registry, and the comment there says why.

**There are no tooltips, and there will not be.** `pma_help()` — a `(?)` span
with a Bootstrap tooltip — had no call site and nothing ever initialised
Bootstrap tooltips, so it had never rendered. It is deleted rather than wired
up: a tooltip is still a sentence somebody has to write, review and keep true,
and it hides that sentence from the reviewer who needed it.

**One evaluation shape on every domain tab.** Each tab replaced its raw
`verbatimTextOutput("<domain>_notes")` under the heading "Evaluation" with
`pma_domain_verdict()` / `pma_facts_list()` / `pma_flowchart_details()`
(`R/ui_helpers.R`):

1. the verdict, one line, in Core GRADE's own words plus the downgrade;
2. the numbers behind it, 3–6 rows of a `<dl>` read from `domain_facts()`;
3. the flowchart, with the branch this analysis took lit up (§3.4.12).

`pma_notes_collapse()`, which parked the verbatim machine-generated note under
all three, is **deleted** (v0.5.1). The picture answers "why this judgment"
better than the prose did, and a `<details>` full of prose is exactly what the
rule above forbids. **The note is not lost:** `domain_notes()` still travels
into `evidence_profile()` and into the exported `.docx` unchanged, which is
where a verbatim record is read.

**A failed evaluation renders a card, not an error string.** `.domain_evaluation()`
wraps its whole body in `tryCatch()`; a backend failure emits *"This domain
could not be evaluated. Re-run Step 2, or report this."* in the standard alert
box. Seen live before this: a domain tab printed `Error: could not find
function ".grade_level_wording"` where the judgment badge belongs, which reads
as a rating rather than as a broken build.

**The five "How is this judged?" accordions are deleted.** `pma_how_collapse()`
and the five `EDU_COPY$domains$*$how` bodies (≈600 words) are gone with their
accessor `edu_domain_how()` and the live `output$rob_how_body`. Four of the five
domains draw their algorithm as a flowchart with the branch taken lit up;
Indirectness has no flowchart and its PICO question labels and subdomain table
carry the same ground. `pma_reference()` still names the source paper on every
tab, and is now the only pointer to it.

**Citation style [v0.5.1].** Every reference the app renders is written in one
house style: **first author, `et al.`, journal abbreviation, year**. No volume,
no pages, **no DOI and no `<a href>`** — a hyperlink bought nothing the citation
itself did not carry, and it made the same paper render four different ways
depending on which call site rendered it. `pma_reference(...)` takes citation
strings and nothing else; its `doi` argument and the `<a href>` branch are gone,
as is the unused `EDU_COPY$pmid_url()`. Each rated domain carries its reference
as the single field `EDU_COPY$domains$*$ref`, replacing the `$ref_text` / `$doi`
pair. The six BMJ 2025 Core GRADE papers are all Guyatt, all BMJ, all 2025, so
the bare form cannot tell them apart; a specific paper carries its series number
as a prefix — `Core GRADE 4. Guyatt G, et al. BMJ. 2025`. Risk of Bias and
Publication bias both cite Core GRADE 4 and so render identically, which is
correct. `test-edu-copy.R` pins the shape with a regex over every `$ref`, so the
format cannot drift back. The rule reaches the whole app, not just Step 3: the
Step 1 sample-dataset line, the Step 2 rare-events references, the RoB-ME notes
on Steps 1 and 3, and the Step 4 "How to cite" card all follow it.

**Judgment wording.** Badges, verdict lines and the four override
`selectInput`s read `.grade_level_wording()` from the package (SPEC.md §5.0),
so they say *Not serious* / *Serious* / *Very serious*. The override **values**
are unchanged (`no` / `some_concerns` / `serious`), and their labels carry the
downgrade — `"Serious (-1)"` — because "serious" alone is ambiguous between
Core GRADE's −1 and the internal level name for −2.

**Configuration owns the cross-cutting settings**, and each is in a
`.config_section()` box of its own — Control-group risk, **Outcome direction**,
Decision threshold, Presentation of event rates. `output$direction_echo` gained
its box in 0.5.1; it used to float between the others as though it were a
caption for one of them.

The tab opens on the first box. `EDU_COPY$config_tab$intro` (115 words) is
**deleted**: every section states its own purpose beside the control it belongs
to, and the threshold's cross-cutting role is the caption of three flowcharts.
So is `config_tab$continuous_intro`, which recited what Core GRADE 6 ranks. The
`continuous_departure` note stays — it tells the reviewer their inferences about
magnitude must be weaker, which they cannot read off the screen.

`.mic_note()` — the paragraph under both Decision threshold boxes warning
against equating the threshold with a Minimally Important Change — is
**deleted** (0.5.1). MIC is a term this project is retiring; the API is
`threshold` / `threshold_type` / `threshold_scale`, and this was the last place
the UI still named MIC at all.

| input | where it lives | why |
|---|---|---|
| `per` | Configuration | it relabels the control-group risk, the absolute threshold and the OIS figures, none of which are on Final certainty (Final certainty keeps a read-only echo) |
| `rob_some_concerns` | **Risk of Bias**, under `Inputs for this domain` | it decides which side of the binary split each study falls on, and the stratified forest on that tab draws exactly that split. Its **scope is unchanged** — still one review-wide setting that persists across outcomes, still absent from `PMA_OUTCOME_INPUT_IDS$rob`. Only the point of edit moved (0.5.1; it was on Configuration for one release, and on a closed `<details>` on Risk of Bias before that). Seeded from `state$rob_some_concerns` — see below |
| `rob_inf_threshold` | **deleted** (0.5.1) | a pmatools convention rather than a Core GRADE 4 rule, and a reviewer had no basis on which to move it. The package default `rob_inflation_threshold = 0.10` (`R/domain_rob.R`) now applies unconditionally; the app no longer passes the argument at all, and `export_bundle()` writes the same 0.10 into the bundled `analysis.R`. Deleting the slider also removed the only consumer of the RoB `how` closure's `inflation_threshold` argument — producer and consumer died together |

**The some-concerns boundary survives a rebuild.** `state$rob_some_concerns`
holds the setting; `step3_ui(state)` seeds the radio from it under `isolate()`,
and an `observeEvent` mirrors the input back. Without the seed a hard-coded
`selected = "high"` would undo a reviewer's `"low"` on every 3 → 2 → 3 round
trip, because `output$step_body` rebuilds the whole step and a freshly built
widget pushes its declared default to the server. That is the intended
behaviour for everything else on Step 3 — the rest is outcome-scoped and is
meant to clear — which is exactly why this one setting needs the exception.
`begin_new_outcome()` deliberately does not touch it: the scope is the review,
not the outcome. `.rob_some_concerns_setting()` falls back to the same state
value, closing the window between a rebuild and the rebuilt radio reporting in,
during which the domains would otherwise be rated against the opposite
convention.

**The per-N display unit.** `radioButtons("per", …)` offers 100 or 1,000 and is
backed by the `display_per_state()` reactiveVal, seeded under `isolate()` and
synced back with `.sync_widget()` — the same machinery the threshold values use,
because a statically declared radio would push its default back on every
3 → 2 → 3 round trip.

**Internal storage stays per-1,000.** `threshold_abs_state()`,
`threshold_baseline_state()`, `.threshold_grade_args()` and `ois_p0_value()`
keep their `/1000` arithmetic whatever the reviewer picks. Only the displayed
value, its label and `sof_table(per =)` / `export_bundle(per =)` follow the
setting. `step3_per_label()` (`R/step3_threshold.R`) is the one formatter every
rate string on Step 3 goes through.

> **Exception, deliberate.** `step3_threshold_note()` — the provenance sentence
> appended to the domain notes and the Evidence Profile footnote — stays in the
> per-1,000 storage unit. It is a record of the conversion that was rated
> against, and it travels into the exported object and `analysis.R`, so it must
> not change with a display preference.

**Control-group risk is a whole number of events.** Every write into
`threshold_baseline_state()` / `threshold_abs_state()` goes through
`step3_quantise_per1000()`, and the two boxes are `step = 1`, `max = per`. An
event rate is a count of patients; "15.6 per 100" is not one.

> **Cost, stated rather than discovered.** At `per = 100` the grid is ten times
> coarser than at `per = 1,000`: a control-group risk of 156 per 1,000 is
> displayed and stored as 160 per 1,000 (16 per 100). Switching units therefore
> can move the number the rating is computed from. The rationale
> `conditionalPanel` compares against the quantised auto value, so a fresh
> analysis is not reported as overridden.

**Publication bias is a wizard over Figure 5.** `output$pubias_wizard` renders
exactly one node. The node is **derived**, never stored as a cursor, by
`step3_pubias_node()` (`R/step3_threshold.R`, pure and unit-tested), mirroring
`assess_pubias()`'s own short-circuit order:

```
!answered(pubias_small_industry)                    -> "q1"
pubias_small_industry == "yes"                      -> "result"   (terminal)
!answered(pubias_registry_complete)                 -> "extra"
pubias_registry_complete %in% c("yes", "no")        -> "result"   (terminal both ways)
# only the explicit "defer" falls through
k >= 10 : !answered(pubias_funnel_asymmetry) ? "q3" : "result"
k <  10 : !answered(pubias_unpublished)      ? "q4" : "result"
```

- **Q2 is not a question.** k decides it (`.pubias_effective_k()`), so it is
  reported as a one-line automatic step in the breadcrumb, never a screen.
- **The reviewer sees no question numbers (0.5.1).** `PUBIAS_NODE_TITLES`, the
  three wizard `h5()` headings and `step3_pubias_k_line()` state the question
  and drop the `Q1` / `Q2` / `Q3` / `Q4` prefix, and `inst/figures/pubias.svg`
  drops it too. The numbering is Core GRADE 4 Fig 5's, but the chart puts a
  pmatools node between Q1 and Q2, so on screen it numbered neither the source
  nor the route the reviewer was walking. The **node keys** (`"q1"`, `"q3"`,
  `"q4"`) and the `"Q1:"`–`"Q4:"` prefixes inside the package's domain notes
  are unchanged: the first are internal, the second are the exported record.
- **Two nodes carry an explicit deferral VALUE** rather than a blank:
  `pubias_registry_complete = "defer"` ("leave it to the Figure 5 nodes") and
  `pubias_funnel_asymmetry = "egger"` ("accept the automated Egger test").
  Without them, "the reviewer looked and has no opinion" is indistinguishable
  from "the reviewer has not reached this yet" and the wizard can never advance
  past an optional node. Neither value reaches `grade_meta()`: both are mapped
  to `NULL`, which is what "let the algorithm decide" means to
  `assess_pubias()`. In particular `"egger"` must not be routed through
  `.override_or_ignore()`, which would demand a rationale for declining to
  override.
- **Advancing happens on answer.** One `observeEvent` per input clears
  `pubias_reopen`; the derivation moves on by itself. No `updateTabsetPanel`,
  no manual Next.
- **A breadcrumb re-opens any answered node.** `pubias_reopen` is honoured
  ahead of the derivation, but only for a node the current answers put on the
  path — so re-opening Q1 and answering "yes" cannot strand the reviewer on a
  Q3 that no longer exists. Reset by `state$step3_reset()`.
- **Structural constraints.** The funnel and trim-and-fill `imageOutput`s and
  the RoB-ME `DT::DTOutput` are **statically placed** and gated by
  `conditionalPanel` on `output.pubias_show_funnel` /
  `output.pubias_show_result`, not moved inside the `renderUI`: DT does not
  bind cleanly inside one. Both flags carry
  `outputOptions(suspendWhenHidden = FALSE)`, or the panel they gate would
  never appear.

**Inconsistency asks one question, not three.** `ci_diff` and `threshold_side`
are gone: `.auto_inconsistency()` derives Core GRADE 3's Steps 1 and 2, and the
app passes `inconsistency_ci_diff = NULL` and
`inconsistency_threshold_side = NULL` unconditionally. The zone tally is shown
instead, through `pma_facts_list()`, so the reviewer sees what the two deleted
questions were answered with. `subgroup_explained` stays open — Step 3 is not
auto-detectable — but only when the automated path reached the opposite-sides
branch, via a `conditionalPanel` on `output.incon_subgroup_relevant`
(`suspendWhenHidden = FALSE`). The package reads it on the automated path as of
0.5.1 (SPEC.md §5.2).

> **That question is now worth two levels (0.5.1).** Leaving
> `subgroup_explained` unanswered on the opposite-sides branch rates
> Inconsistency `serious` (−2), not `some_concerns` (−1): with a substantial
> share of estimates on each side of the threshold and no credible subgroup,
> the direction of effect is unresolved. The app renders whatever the package
> returns — no app-side arithmetic changes — but the domain badge, the
> certainty verdict and the flowchart leaf all move with it, and the leaf now
> reads "Rate down 2 levels". See SPEC.md §5.2 for why this departs from Core
> GRADE 3.

**Risk of Bias.** `output$rob_rule_note` (a ~180-word standing statement of the
binary rule) and the "See also RoB 2" paragraph are deleted, not collapsed: the
`rob_some_concerns` radio states the rule in one sentence beside itself, the
two-group forest *shows* it, and `pma_reference()` already carries the source.
`output$rob_forest` passes
`plot_forest_rob(some_concerns_as = .rob_some_concerns_setting())`, so the plot
and the judgment beside it agree about how many groups there are.

**The read-only threshold block takes a `detail` argument.**
`.render_threshold_readonly(domain, detail)` prints the head line
("Absolute threshold: 50 per 1,000 at a control-group risk of 156 per 1,000")
always, and the equivalence block only when `detail = TRUE`.

| tab | `detail` | why |
|---|---|---|
| Risk of Bias | `FALSE` | it compares two pooled estimates against the band; the conversion arithmetic answers nothing |
| Inconsistency | `FALSE` | the zone tally is computed for the reviewer and reported through `pma_facts_list()`; they never read a bound themselves |
| Imprecision | `TRUE` | Core GRADE 2's two-level rule tests the confidence interval against the important-benefit **and** important-harm thresholds by eye, so both bounds — and the residual-asymmetry sentence, since only one conversion is exact on the absolute scale — are operative |

The trailing *"This decision threshold is shared by … Change it in the
Configuration tab"* sentence is now the tab's own name as a link, built by
`pma_domain_jump_links()` (§3.4.13). One id prefix per domain
(`threshold_block_jump_<domain>_`): all seven tab panels are in the DOM at once,
so three copies of one `actionLink` id would collide.

**Indirectness: the default is on screen.** All four PICO radios are
**preselected to `"yes"`** (0.5.1). Leaving them blank used to send
`indirectness = "no"` to `grade_meta()` while the screen showed four unanswered
questions — the domain scored no downgrade silently. Preselection makes that
default visible and leaves the reviewer to downgrade the elements they have
concerns about.

The **judgment is unchanged** and this is verified, not assumed:
`indir_subdomains()` now returns four rows instead of `NULL`, so `grade_obj()`
takes the *subdomain* path rather than the scalar one; `indir_worst_case()`
folds four `"yes"` answers to `"no"`; and the override-rationale logic compares
`input$indirectness` against `indir_worst %||% "no"`, which is the same value it
compared against before. Rated on the bundled CBT-I sample, certainty, all five
domain judgments and every downgrade are identical either way.

One downstream effect is real, and is the reason this is a breaking change:
`grade$indirectness_subdomains` is now populated for every outcome, so

- `indirectness_table(g)` **stops aborting** and returns a four-row table;
- the multi-outcome bundle gains
  `outcomes/<nn>_<outcome>/indirectness_table.docx`, which
  `export_bundle.pmatools_set()` writes only when subdomain judgments exist;
- the bundled `analysis.R` carries an `indirectness_subdomains = data.frame(…)`
  literal in place of `NULL`, and `results.txt` reports the four answers instead
  of *"Overall judgment provided by user."*

Nothing else branches on subdomains being present: `evidence_profile()`,
`sof_table()` and the SoF footnotes are byte-identical across the change.

The two boxed departure notes and the three per-element footnotes that sat in a
shared `<details>` are **deleted**; what survives is two capped subtitles beside
the questions — `EDU_COPY$domains$indirectness$surrogate` (a surrogate outcome
is grounds to consider rating down; never pool the two) and `$gradient` (the
fold is symmetric and ignores Table 2's ranking, which is why the override
exists). `$mapping` and `$banner` are gone, and with `$banner` went
`output$indirectness_banner` and `state$indir_reviewed`: the banner said "no
indirectness judgment recorded yet", and with the radios preselected there
always is one.

**Imprecision.** `output$impre_branch` reads the `fig4_path` / `ois_used`
**facts** instead of regex-parsing the note string. The `.override_details`
preamble is deleted (it restated the branch text), and the nested `<details>`
inside it is unwrapped: the one sentence a reviewer needs at the override —
*"Rate down two levels when the plain language summary warrants 'may' rather
than 'likely'"* — is now the only thing there, visible.
`.inputs_details(open = TRUE)` stays open.

**Final certainty.** `other_text` / `other_downgrade` are answers and stay
open; the rest of Display options collapses. The Heimke CER/EER recommendation
is now `PMA_SOF_CER_EER_NOTE`, written into the SoF footer by
`pma_sof_add_notes()`, so it travels into the exported .docx — which it never
did as page text.

### 3.5 Step 4 — Export

#### 3.5.1 Step header copy

> **Step 4: Export.** This step bundles every artifact you have generated into a single ZIP — including a fully reproducible `analysis.R` script. Anyone (including future-you) can re-run the analysis from the CSV and the script alone, with `library(pmatools)`. This is what makes the work reproducible and citable.

#### 3.5.2 Inputs

- `textInput("bundle_name", "Bundle name", value = "pmatools_results")`
- `checkboxGroupInput("include", "Include in bundle", choices = c("Long-format CSV" = "data", "R script" = "script", "Results text" = "results", "Forest plot" = "forest", "Funnel plot" = "funnel", "GRADE table (docx)" = "grade_table", "GRADE appendix (docx)" = "grade_appendix"), selected = all)`
- `downloadButton("download_zip", "Download ZIP", class = "btn-primary")`

Below: individual download buttons for each artifact (forest.pdf, funnel.pdf, grade_appendix.docx) for users who want them separately.

#### 3.5.3 Server logic

```r
output$download_zip <- downloadHandler(
  filename = function() paste0(input$bundle_name, ".zip"),
  content  = function(file) {
    out <- pmatools::export_bundle(
      ma          = state$ma,
      grade       = state$grade,
      output_dir  = tempdir(),
      bundle_name = input$bundle_name,
      include     = input$include,
      per         = state$display$per,
      prediction  = state$display$prediction,
      convert_smd_to_or = state$display$convert_smd_to_or,
      baseline_risk     = state$display$baseline_risk,
      threshold_label   = state$display$threshold_label
    )
    file.copy(out, file)
  }
)
```

#### 3.5.4 "How to cite this analysis" expandable

```
Bibtex entries for:
- pmatools (this package)
- BMJ Core GRADE series (papers 1, 3, 4, 5)
- {meta} R package
- CINeMA approach for Inconsistency

Plus a paragraph:
"Pairwise meta-analysis was performed using the {meta} R package (Schwarzer 2007).
Certainty of evidence was rated using the GRADE approach following the BMJ 2025 Core GRADE series
(Guyatt et al. 2025), implemented in the pmatools R package
(https://github.com/ykfrkw/pmatools)."
```

### 3.4.12 Domain flowcharts (v0.5.1)

Each of the four flowcharted domains — Risk of Bias, Inconsistency, Imprecision,
Publication bias — shows the decision tree it was judged by, **with the path this
analysis actually took highlighted**. Indirectness has no flowchart; Core GRADE 5
Table 2 is a gradient, and its `indir_subdomain_table` stays the visual.

Two renderings of the same file, both through `pma_flowchart()` in `R/ui_helpers.R`:

| where | `on_ids` | why |
|---|---|---|
| under the verdict, in `<details class="pma-flowchart-details" open>` | the path taken | it answers "why this judgment", so it is open by default — but a reviewer who does not want it can shut it |
| inside the collapsed "How is this judged?" accordion | none | the plain diagram, as reference |

`pma_flowchart()` reads `_pmatools_inst/figures/<figkey>.svg` (staged path first, a
local-development fallback second — the same shape `step1_data.R` uses for `extdata`,
and deliberately **not** `system.file()`, which `stage_bundle.R` does not rewrite for
anything but templates). A missing file yields a placeholder paragraph, never an error.

The path comes from the package, not from parsing prose: `domain_facts(g, <domain>)`
carries a `flow_path` fact listing the SVG node ids traversed (see `SPEC.md` §5.7).
`pma_flowchart()` puts them on the wrapper as `data-pma-path`, and `www/flowchart.js`
adds the class `pma-fc-on` to each. That script follows `required-fields.js`'s contract
— idempotent, cached on `window`, re-applied on `shiny:value` — because the Step 3 body
is rebuilt by `renderUI` and would otherwise lose the highlight.

Styling lives in `www/shadcn.css` under `.pma-flowchart`. **Every selector there carries
the wrapper class on purpose:** the SVG ships its own `<style>` block so it still reads
correctly in the package help pages, and because the SVG is inlined into the body that
block comes later in document order and would win any tie at equal specificity. The
extra class is what lets the app rules outrank it without `!important` — asserted by a
test. The highlight is carried by `stroke-width` as well as colour, because these get
printed.

`pma_algorithm_source(domain)` supplies the caption naming the implementing function, so
the app and the roxygen topic `?grade_flowcharts` quote the same file and function.

### 3.4.13 Confirmation, progress and the forward path (v0.5.1)

**What confirms a domain: its checkbox, and nothing else.** Six of the seven
Step 3 tabs are gated — Configuration plus the five domains — and each carries
one `I have reviewed this domain` checkbox. `pma_domain_confirmations()`
(`R/ui_helpers.R`) is the rule, a pure function of two named logical vectors
keyed by input id: the checkbox values, and the freshness stamps of the same
ids. A domain is confirmed **iff** its box is ticked *for the outcome now
open*. `PMA_DOMAIN_CONFIRM_INPUTS` maps a domain key to its checkbox;
`domain_confirmed()` in `R/step3_grade.R` is only the wiring, and mirrors the
result into `state$domain_confirmed` for Step 4.

This is **narrower than it was**, deliberately. The rule used to be a
disjunction: substantive input in the domain (a filled risk-of-bias table, an
answered PICO radio, a non-empty OIS override), *or* a valid override with a
rationale, *or* the checkbox. Two things were wrong with it.

- It could report `Unconfirmed: Indirectness` with that tab's checkbox ticked
  (a tick left over from another outcome), and confirm a domain whose box was
  visibly empty. The tick the reviewer can see must be the verdict.
- Any widget that arrives **preselected** satisfies "substantive input" the
  moment it mounts, which would open the export gate for an outcome nobody had
  looked at.

Configuration keeps one extra condition, and only it: `config_blockers()` must
be empty. That gate is about values being *set* — three of the five domains are
judged against the threshold — so a tick alone will not do.

**Every Next is gated; nothing else is.** `output$grade_nav_<key>` renders the
Back/Next pair of each domain tab from `STEP3_DOMAIN_NAVS`, and the Next is
disabled until that domain is confirmed, carrying
`title = "Tick 'I have reviewed this domain' to continue"` while it is. Back is
never gated, and neither is the tab strip: the reviewer can always look ahead,
and the stepper still jumps freely. Every one of the seven navs is an output
with `outputOptions(suspendWhenHidden = FALSE)` — six of the seven are hidden at
any moment, and a suspended output keeps the HTML it last painted, which is a
gate whose state was decided one outcome ago.

**Progress is visible in three places, all reading the same count.**

| where | what it shows |
|---|---|
| the tab strip | `pma_tab_mark()`: a tick on a confirmed tab, a dot on one the reviewer has opened but not confirmed, nothing before that |
| the Step 3 card header | `output$grade_progress_badge`, "n/6 confirmed" |
| the stepper, once Step 3 has been opened | `pma_stepper(current_step, certainty_confirmed =)`, "Certainty n/6" |

The stepper count is withheld until the reviewer first reaches Step 3.
`step3_server()` is wired at startup and writes `state$domain_confirmed`
immediately, so without the guard the stepper reads "Certainty 0/6" to someone
who has not yet loaded a dataset — the same "have you seen this yet?" semantics
the dot on a domain tab carries. Passing `certainty_confirmed = NULL` renders
the bare label.

The markers are `uiOutput`s *inside the tab titles*, so the count follows a
tick without the tabset being rebuilt. That is why every gated `tabPanel` now
states its `value` explicitly: a tag-list title leaves `tabPanel()` no string to
derive one from, and the value is what `updateTabsetPanel()`, `grade_tabs` and
`PMA_DOMAIN_LABELS` all match on. "Visited" is the one piece of state no input
carries; a `reactiveValues` set written by `observeEvent(input$grade_tabs)`
holds it, and a change of outcome clears it with the confirmations.

**Every "still to confirm X" names X as a link to X.** `cert_incomplete_banner`
(Step 3) and the download lock (Step 4) both build their domain list with
`pma_domain_jump_links(keys, id_prefix)`; clicking one calls
`updateTabsetPanel(session, "grade_tabs", selected = PMA_DOMAIN_LABELS[[key]])`,
and from Step 4 sets `state$step <- 3L` first so the tabset exists when the
input message lands. The two copies take different id prefixes because both can
be alive in one session.

**The tab strip scrolls.** Seven tabs plus the markers are wider than a 375px
viewport, and left alone the strip is what sets the page's minimum width — a
phone scrolled the *whole page* sideways to read a paragraph. `.pma-card
.nav-tabs` is `overflow-x: auto` with proximity scroll-snap.

---

## 4. Design system

### 4.1 CSS design tokens (`www/shadcn.css`)

```css
:root {
  /* Base shadcn-like palette */
  --background:            0 0% 100%;
  --foreground:            222 47% 11%;
  --card:                  0 0% 100%;
  --card-foreground:       222 47% 11%;
  --popover:               0 0% 100%;
  --popover-foreground:    222 47% 11%;
  --primary:               222 47% 11%;
  --primary-foreground:    210 40% 98%;
  --secondary:             210 40% 96%;
  --secondary-foreground:  222 47% 11%;
  --muted:                 210 40% 96%;
  --muted-foreground:      215 16% 40%;   /* v0.5.1: was 47% (4.72:1 on white) */
  --accent:                210 40% 96%;
  --accent-foreground:     222 47% 11%;
  --destructive:           0 84% 60%;
  --destructive-foreground:210 40% 98%;
  --border:                214 32% 91%;
  --input:                 214 32% 91%;
  --ring:                  222 47% 11%;
  --radius:                0.5rem;

  /* GRADE certainty palette (CINeMA pastel; identical to pmatools v0.1.0 SoF) */
  --grade-high-bg:      #d7e8d3;  --grade-high-fg:      #238b21;
  --grade-moderate-bg:  #cccce9;  --grade-moderate-fg:  #01008b;
  --grade-low-bg:       #f8edd7;  --grade-low-fg:       #daa521;
  --grade-vlow-bg:      #e8d0d0;  --grade-vlow-fg:      #8b0000;

  /* RoB palette: re-uses CINeMA tones for Low / Some / High */
  --rob-low-bg:    var(--grade-high-bg);    --rob-low-fg:    var(--grade-high-fg);
  --rob-some-bg:   var(--grade-low-bg);     --rob-some-fg:   var(--grade-low-fg);
  --rob-high-bg:   var(--grade-vlow-bg);    --rob-high-fg:   var(--grade-vlow-fg);

  /* Type scale */
  --font-sans: ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont,
               "Segoe UI", Roboto, "Helvetica Neue", sans-serif;
  --font-mono: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
}

* { box-sizing: border-box; }

body {
  font-family: var(--font-sans);
  background: hsl(var(--background));
  color: hsl(var(--foreground));
}

/* Card */
.pma-card {
  background: hsl(var(--card));
  color: hsl(var(--card-foreground));
  border: 1px solid hsl(var(--border));
  border-radius: var(--radius);
  padding: 1.5rem;
  margin-bottom: 1rem;
}
.pma-card-header { font-size: 1.125rem; font-weight: 600; margin-bottom: 0.75rem; }
.pma-card-subtitle { font-size: 0.875rem; color: hsl(var(--muted-foreground)); margin-bottom: 1rem; }

/* Badge */
.pma-badge {
  display: inline-flex;
  align-items: center;
  padding: 0.125rem 0.625rem;
  border-radius: 9999px;
  font-size: 0.75rem;
  font-weight: 500;
  letter-spacing: 0.025em;
}
.pma-badge.grade-high     { background: var(--grade-high-bg);     color: var(--grade-high-fg); }
.pma-badge.grade-moderate { background: var(--grade-moderate-bg); color: var(--grade-moderate-fg); }
.pma-badge.grade-low      { background: var(--grade-low-bg);      color: var(--grade-low-fg); }
.pma-badge.grade-vlow     { background: var(--grade-vlow-bg);     color: var(--grade-vlow-fg); }

/* Stepper */
.pma-stepper { display: flex; gap: 0.5rem; margin-bottom: 1.5rem; align-items: center; }
.pma-step { display: flex; align-items: center; gap: 0.5rem; font-size: 0.875rem;
            color: hsl(var(--muted-foreground)); }
.pma-step.current { color: hsl(var(--foreground)); font-weight: 600; }
.pma-step.done    { color: hsl(var(--grade-high-fg)); }
.pma-step .num { width: 1.5rem; height: 1.5rem; border-radius: 9999px;
                 display: inline-flex; align-items: center; justify-content: center;
                 background: hsl(var(--muted)); }
.pma-step.current .num { background: hsl(var(--primary));
                          color: hsl(var(--primary-foreground)); }
.pma-step.done .num    { background: var(--grade-high-bg);
                          color: var(--grade-high-fg); }

/* Banner (used for Indirectness review reminder) */
.pma-banner {
  background: var(--grade-low-bg);
  border-left: 4px solid var(--grade-low-fg);
  color: var(--grade-low-fg);
  padding: 0.75rem 1rem;
  border-radius: var(--radius);
  margin-bottom: 1rem;
}

/* Button overrides (work with bslib bs5 .btn) */
.btn-primary {
  background: hsl(var(--primary));
  border-color: hsl(var(--primary));
  color: hsl(var(--primary-foreground));
}
.btn-primary:hover { opacity: 0.9; }

/* Accordion overrides */
.accordion-button:not(.collapsed) {
  background: hsl(var(--accent));
  color: hsl(var(--foreground));
}

/* Inline help icon */
.pma-help { color: hsl(var(--muted-foreground)); cursor: help; margin-left: 0.25rem; }
.pma-help:hover { color: hsl(var(--foreground)); }

/* flextable output: page font on screen, Arial in the .docx (v0.5.1) */
.tabwid table,
.tabwid p,
.tabwid span { font-family: var(--font-sans); }
```

**`--muted-foreground` is darker than shadcn's [v0.5.1].** `215 16% 40%`, not the
`47%` this token carries upstream. Nearly everything it colours is set between
0.75rem and 0.875rem — card subtitles, step labels, table captions, the help
icon — and 47% is 4.72:1 on white, an AA pass with no margin at small sizes.
40% is 6.08:1.

**flextable output takes the page font [v0.5.1].** `pmatools` sets every table it
builds in Arial (`SPEC.md` §4.6) because those tables exist to be dropped into a
.docx, where a word processor resolves a named face and a CSS stack means
nothing. On screen that put an Arial table inside a `var(--font-sans)` page, and
the mismatch was plainest in the Summary of Findings preview. The rule above
restyles the **family only**; the builders' sizes, colours, borders and column
widths are untouched, so the exported document is unaffected.

`flextable::htmltools_value()` wraps its output in `<div class="tabwid">` and
emits a `<style>` block of generated `.cl-xxxxxxxx` selectors, one per distinct
cell style, applied to the `<span>` inside each `<p>`. Those are single-class
selectors, so the descendant selectors above outrank them and no `!important` is
needed — the same reasoning as the `.pma-flowchart` rules in §3.4.12. The
wrapper class name is flextable's, not this app's, and **has changed between
flextable versions**: verify it against the rendered DOM before assuming this
rule still bites.

Note that `www/shadcn.css` is inlined by `htmltools::includeCSS()` when the UI
object is built, so editing it requires restarting the Shiny process. A browser
reload alone re-serves the old stylesheet.

### 4.2 bslib theme

```r
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(
    version    = 5,
    bootswatch = "default",
    primary    = "#0f172a",
    base_font  = bslib::font_google("Inter")
  ),
  htmltools::tags$head(htmltools::includeCSS("www/shadcn.css")),
  ...
)
```

---

## 5. Educational copy storage

All American English copy lives in `R/educational_copy.R` as named lists for easy maintenance and translation later. Dynamic content (e.g., MID label per `sm`) is rendered by helpers that consume `EDU_COPY` plus runtime data.

**MID input is rendered dynamically:**

```r
# In step3_grade.R
mid_suggestion <- pmatools::suggest_mid(state$ma)   # NULL if sm unrecognized
mid_label      <- EDU_COPY$mid_labels[[state$ma$sm]] %||% "Minimally important difference"
mid_help       <- EDU_COPY$mid_help[[state$ma$sm]]   %||% "..."

ui <- numericInput(
  inputId = "mid",
  label   = mid_label,
  value   = mid_suggestion$mid_user %||% NA,
  min     = 0,
  step    = 0.01
)
# state$grade_args$mid_scale <- mid_suggestion$mid_scale  (default "auto")
```

The user's edit replaces the default; `state$grade_args$mid` tracks the live value.



```r
EDU_COPY <- list(
  steps = list(
    step1 = list(
      title    = "Step 1: Data",
      what     = "This step loads your study-level dataset and validates it...",
      why      = "Why this matters. Clean data is the foundation..."
    ),
    step2 = list(...),
    step3 = list(...),
    step4 = list(...)
  ),
  domains = list(
    rob = list(
      header       = "Risk of Bias",
      ref          = "Core GRADE 4. Guyatt G, et al. BMJ. 2025",
      how_judged   = "GRADE rates down for risk of bias when...",
      result_template = "• Per-study RoB labels: ...",
      override_help = "..."
    ),
    inconsistency = list(...),
    indirectness  = list(...),
    imprecision   = list(...),
    pubias        = list(...)
  ),
  glossary = list(
    OIS  = "Optimal Information Size: the sample size a single well-powered RCT...",
    "I²" = "I-squared: percentage of total variability across studies due to heterogeneity...",
    REML = "Restricted Maximum Likelihood: the modern default method for estimating...",
    HK   = "Hartung-Knapp adjustment: improves CI coverage in random-effects meta-analysis with few studies...",
    PI   = "Prediction Interval: the range of effects expected in a future similar study...",
    Egger = "Egger's regression test for funnel plot asymmetry..."
  ),
  mid_labels = list(
    OR  = "MID (as OR ratio, e.g., 1.25 = 25% relative odds change)",
    RR  = "MID (as risk ratio, e.g., 1.20 = 20% relative risk change)",
    HR  = "MID (as hazard ratio, e.g., 1.20)",
    RoM = "MID (as ratio of means, e.g., 1.10)",
    SMD = "MID (in standardized units, e.g., 0.20 = Cohen's small)",
    MD  = "MID (in outcome units; default = 0.20 × pooled SD)",
    ARD = "MID (as absolute risk difference, e.g., 0.05 = 5%)"
  ),
  mid_help = list(
    OR  = "An OR of 1.25 vs 1.0 represents a 25% relative change in odds — a typical small but clinically meaningful effect.",
    RR  = "An RR of 1.20 vs 1.0 represents a 20% relative change in risk.",
    HR  = "An HR of 1.20 represents a 20% relative change in hazard.",
    RoM = "A 10% ratio of means is a typical small clinically meaningful difference for continuous outcomes.",
    SMD = "Cohen's small effect size (0.20) is widely accepted as the smallest clinically meaningful SMD.",
    MD  = "Auto-suggested as 0.20 × pooled SD (Cohen's small in raw units). Replace with a published MID for your outcome whenever possible.",
    ARD = "A 5% absolute risk difference is a typical small clinically meaningful effect."
  )
)
```

The Shiny modules read from `EDU_COPY$...` rather than hardcoding strings.

---

## 6. Reactivity flow (full)

```
                                           ┌──────────────────────────────┐
input$data_file / input$data_paste / etc.  │ Step 1                       │
input$data_format                          │   pmatools::ingest_data()    │
input$mapping_*                            │     ↓                         │
input$data_preview_edit                    │   state$data ← tibble        │
                                           └──────────────────────────────┘
                                                          ↓
                                           ┌──────────────────────────────┐
input$outcome_type / sm / method /         │ Step 2                       │
input$method_tau / random / incr /         │   pmatools::run_ma()          │
input$subgroup / input$run_ma (action)     │     ↓ (on "Run analysis")    │
                                           │   state$ma ← meta object     │
                                           │     ↓                         │
                                           │   pmatools::plot_forest()    │
                                           │   pmatools::plot_funnel()    │
                                           └──────────────────────────────┘
                                                          ↓
                                           ┌──────────────────────────────┐
input$rob_override / rob_dom_threshold /   │ Step 3 (debounced 500ms)     │
input$rob_some_concerns / small_values /  │   pmatools::grade_meta()     │
input$mid /                                 │     ↓                         │
input$inconsistency_override /              │   state$grade ← pmatools obj │
input$indirectness /                        │     ↓                         │
input$imprecision_override /                │   pmatools::sof_table()      │
input$pubias_* / etc.                       │     ↓ (preview)              │
input$per / prediction / convert_smd_to_or  │   htmltools_value(ft)        │
input$baseline_risk / threshold_label       │                              │
                                           └──────────────────────────────┘
                                                          ↓
                                           ┌──────────────────────────────┐
input$bundle_name / include /              │ Step 4                       │
input$download_zip                         │   pmatools::export_bundle()  │
                                           │     ↓                         │
                                           │   downloadHandler returns ZIP│
                                           └──────────────────────────────┘
```

---

## 7. shinyapps.io deployment

### 7.1 Local verification

```r
setwd("~/Developer/pairwise_meta_analysis")
shiny::runApp(launch.browser = TRUE)
# → exercise all 4 steps with sample data; confirm Step 1→4 → ZIP download
```

### 7.2 Deploy

```r
# Ensure pmatools is on GitHub and devtools::install_github("ykfrkw/pmatools") works locally
rsconnect::deployApp(
  appDir   = "~/Developer/pairwise_meta_analysis",
  appName  = "pairwise_meta_analysis",
  account  = "yuki-furukawa",
  forceUpdate = TRUE
)
```

`rsconnect` reads `DESCRIPTION` and resolves `Remotes: github::ykfrkw/pmatools` automatically. If the deploy fails on the build server, the most likely causes are:

1. pmatools is private on GitHub → make it public, OR add a deploy token via `rsconnect::setAccountInfo()` with `gitCredentials`.
2. pmatools system dependencies (e.g., libxml2 for officer) missing on shinyapps.io → declare in pmatools DESCRIPTION's `SystemRequirements`.
3. App size exceeds shinyapps.io free-tier limit → upgrade plan or reduce bundle.

### 7.3 Post-deploy smoke test

Open https://yuki-furukawa.shinyapps.io/pairwise_meta_analysis/ and run through Steps 1–4 with the bundled sample data. Verify ZIP download produces a valid archive containing `analysis.R` that runs with `Rscript`.

---

## 8. Testing

Manual smoke test at minimum:

1. Step 1: load sample → mapping shows green-checked → preview renders → Next
2. Step 2: defaults → Run analysis → forest renders → funnel renders with Egger annotation → result text shows pooled OR
3. Step 3: open each accordion → algorithm explanation reads naturally → auto judgment matches expected for sample data → set MID = 0.20 → see zone counts update → click Indirectness "No" → banner clears
4. Step 4: bundle name → Download ZIP → unzip → 9 files present → open analysis.R in R → `source("analysis.R")` reproduces same TE.random

Optional automated: `shinytest2::record_test()` for Step 1 → Step 4 happy path.

---

## 9. Versioning

This SPEC tracks the pairwise_meta_analysis app version, NOT the pmatools package version. The app declares the minimum compatible pmatools version in its `DESCRIPTION`:

```dcf
Imports: pmatools (>= 0.2.0)
```

When pmatools API changes, bump the `>=` constraint here.

---

## 10. References

- Schwarzer G. *meta: An R package for meta-analysis*. R News 2007.
- Guyatt G, et al. *Core GRADE 1: Overview*. BMJ 2025. PMID: 40262844.
- Guyatt G, et al. *Core GRADE 3: Inconsistency*. BMJ 2025. PMID: 40328467.
- Guyatt G, et al. *Core GRADE 4: Risk of bias, publication bias*. BMJ 2025. PMID: 40360206.
- Guyatt G, et al. *Core GRADE 5: Indirectness*. BMJ 2025. PMID: 40393729.
- Chinn S. *A simple method for converting an odds ratio to effect size*. Stat Med. 2000;19(22):3127-3131.
- Nikolakopoulou A, et al. *CINeMA*. PLoS Med. 2020;17(4):e1003082.
- Viechtbauer W. *Bias and efficiency of meta-analytic variance estimators in the random-effects model*. J Educ Behav Stat. 2005;30:261-293.
