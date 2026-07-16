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
- `state$grade` recomputes whenever `state$ma` is set OR any Step 3 input changes (debounced 500ms; cheap to compute).
- Forest/funnel plots are rendered from `state$ma`.
- SoF preview rendered from `state$grade` and `state$display`.

---

## 3. Wizard structure

### 3.1 Top-level chrome

- **Header**: app title "pmatools — pairwise meta-analysis with GRADE", small nav with link to docs (pmatools README on GitHub) and shinyapps.io status.
- **Stepper**: horizontal 4-step indicator under the header. Steps shown as: `1 Data — 2 Meta-analysis — 3 GRADE — 4 Export`. Current step bold + filled circle; completed steps green check; future steps muted.
- **Footer**: small print "Powered by pmatools v{version}; see github.com/ykfrkw/pmatools"; citation hint "If you use this in published work, please cite pmatools and the BMJ 2025 Core GRADE series."

### 3.2 Step 1 — Data

#### 3.2.1 Layout

Single column (`bslib::page_fluid`), 3 cards stacked:

1. **Load data** — input source selection
2. **Column mapping** — visible after data is loaded
3. **Preview & edit** — `DT::DTOutput` with editable cells

Below: Step header banner with `What this step does`, and **Next →** button (disabled until `state$data` is non-NULL and validates).

#### 3.2.2 Step header copy

> **Step 1: Data.** This step loads your study-level dataset and validates it. The app accepts data in two formats: **long format** (one row per study-arm pair, used internally by `{meta}`) and **wide format** (one row per study with paired columns like `event_e`/`event_c`). The app converts wide → long automatically. You can paste from Excel, upload a `.csv` or `.xlsx`, or load the bundled sample dataset.

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

> **Why this matters.** Clean data is the foundation of every analysis that follows. Meta-analysis tools assume specific column names and types — if your `n` column is read as text or your event counts include decimals, the pooled estimate will be wrong or the analysis will fail. The mapping screen lets you point your column names to the canonical names without renaming columns in your source file.

### 3.3 Step 2 — Meta-analysis

#### 3.3.1 Layout

Two columns: left sidebar with model controls, right pane with tabbed plots and result text.

#### 3.3.2 Step header copy

> **Step 2: Meta-analysis.** This step pools effect estimates across studies using the `{meta}` R package. You choose the **outcome type** (binary or continuous), the **effect measure** (e.g., OR, RR, SMD), the **pooling method**, and the **heterogeneity estimator**. The forest plot visualizes individual study estimates and the pooled effect with its 95% confidence interval; the funnel plot helps detect small-study effects and possible publication bias.

#### 3.3.3 Inputs (sidebar)

- `radioButtons("outcome_type", c("Binary" = "binary", "Continuous" = "continuous"))`
- For binary: `radioButtons("sm", c("OR", "RR"))`
- For continuous: `radioButtons("sm", c("SMD", "MD", "RoM"))`
- `radioButtons("model", c("Random" = "random", "Common (Fixed)" = "common"))`
- `selectInput("method", "Pooling method")` — depends on sm (Inverse / MH / Peto)
- `selectInput("method_tau", "Heterogeneity estimator", c("REML", "DL"), selected = "REML")` — only when random
- `numericInput("incr", "Continuity correction", value = 0.5, min = 0)` — only when binary
- `radioButtons("use_subgroup", "Subgroup analysis", c("No", "Yes"))`
- If yes: `selectInput("subgroup_col", "Subgroup column", choices = ...)`
- `actionButton("run_ma", "Run analysis", class = "btn-primary")`

Inline help (`bslib::tooltip()`) on each input, e.g. for REML: *"Restricted maximum likelihood. Generally preferred over DerSimonian-Laird (DL); produces tau-squared estimates with better small-sample properties (Viechtbauer 2005)."*

#### 3.3.4 Outputs (right pane)

Tabset with 3 tabs:

- **Forest plot** — `plotOutput("forest", height = "auto")` rendered via `pmatools::plot_forest(state$ma, title = input$forest_title, label_e = ..., label_c = ..., xlim = parsed_input)`. Auto-height: `350 + 30 * k` pixels.
- **Funnel plot** — `plotOutput("funnel")` via `pmatools::plot_funnel(state$ma, show_egger = TRUE)`.
- **Text results** — `verbatimTextOutput("ma_summary")` showing `summary(state$ma)`.

Below the tabset: collapsible "Forest plot adjustments" with title, label_e, label_c, xlim min/max overrides.

#### 3.3.5 Why this matters

> **Why this matters.** The pooled estimate and confidence interval you see here drive every GRADE judgment in the next step. The choice between random and fixed-effects models, and between REML and DL, can meaningfully change tau-squared and the prediction interval. Random-effects with REML and Hartung-Knapp adjustment is the modern default for clinical evidence synthesis.

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
- `sliderInput("rob_inf_threshold", "Inflation threshold", min = 0.05, max = 0.5, value = 0.1, step = 0.05)`
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

- `numericInput("per", "Display rates per N patients", value = 1000, min = 1)` (1000 / 100 typical)
- `checkboxInput("prediction", "Show 95% prediction interval in Effect column")`
- `checkboxInput("convert_smd_to_or", "Show as dichotomous outcome (Chinn's formula)")` *— SMD/MD outcomes only*
- If `convert_smd_to_or` checked:
  - `numericInput("baseline_risk", "Control event rate (proportion responding)", value = 0.30, min = 0.01, max = 0.99)`
  - `textInput("threshold_label", "Threshold definition (free text)", placeholder = "e.g., ≥50% reduction in PHQ-9 from baseline")`
  - Inline note: *"This is the proportion of control patients who would meet the threshold of clinical interest. Continuous effect sizes are statistically rigorous but hard for clinicians and patients to interpret. Showing 'X out of 1,000 patients respond' alongside the SMD often communicates the same evidence more accessibly."*

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
  --muted-foreground:      215 16% 47%;
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
```

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
      reference    = "BMJ Core GRADE 4 (Guyatt et al., 2025)",
      pmid         = "40360206",
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
input$rob_inf_threshold / small_values /   │   pmatools::grade_meta()     │
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
