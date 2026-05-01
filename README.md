# pmatools

**End-to-end pairwise meta-analysis with GRADE certainty assessment**

`pmatools` is an R package that runs the full pairwise meta-analysis pipeline — from data ingestion (long or wide format) through pooled effect estimation (binary or continuous) and forest/funnel plots — and rates the resulting evidence with the **GRADE** approach following the **BMJ 2025 Core GRADE series** (Guyatt G et al., BMJ 2025). It produces a Summary of Findings flextable, a multi-outcome GRADE table, and a full Appendix report (docx/html/pdf/md), and bundles every artifact plus a reproducible `analysis.R` script into a single ZIP.

A wizard-style Shiny front-end lives in the companion repository [pairwise_meta_analysis](https://yuki-furukawa.shinyapps.io/pairwise_meta_analysis/) (deployed on shinyapps.io).

---

## Installation

```r
# From GitHub
remotes::install_github("ykfrkw/pmatools")
library(pmatools)

# Local development (open pmatools.Rproj in RStudio)
devtools::load_all(".", reset = TRUE)
```

---

## Quick start

End-to-end: data -> meta-analysis -> GRADE -> SoF -> ZIP.

```r
library(pmatools)

# 1. Ingest study data (long or wide; auto-detected)
data <- ingest_data(system.file("extdata/cbti_depression.csv", package = "pmatools"))

# 2. Run meta-analysis
ma <- run_ma(data,
             outcome_type       = "binary",
             sm                 = "OR",
             experimental_label = "CBT-I",
             control_label      = "Control")

# 3. Plots (auto-laid-out; funnel includes Egger when k >= 10)
plot_forest(ma, title = "CBT-I for depression")
plot_funnel(ma)

# 4. GRADE certainty (per-study RoB; MID drives both Inconsistency Step 2 and OIS)
g <- grade_meta(ma,
                study_design = "RCT",
                rob          = data$rob[data$treat == "CBT-I"],
                small_values = "undesirable",
                mid          = 1.25,    # OR-scale MID (auto-detected)
                ois_p0       = 0.25,
                outcome_name = "Depression response")

print(g)
sof_table(g)                 # pastel palette, rates per 1,000
sof_table(g, per = 100)
sof_table(g, prediction = TRUE)

# 5. Reproducible ZIP bundle
export_bundle(ma, g, output_dir = "outputs", bundle_name = "cbti_depression")
```

`outputs/cbti_depression.zip` contains: `data_long.csv`, `analysis.R` (re-runs the
analysis with `library(pmatools)`), `results.txt`, forest/funnel PDF+PNG, the SoF
docx, and the GRADE Appendix docx.

### Shorter version: GRADE-only on an existing meta object

```r
m <- meta::metabin(event.e = c(10,15,20), n.e = c(50,60,70),
                   event.c = c(15,20,25), n.c = c(50,60,70),
                   studlab = c("A","B","C"), sm = "OR",
                   prediction = TRUE)

g <- grade_meta(m, study_design = "RCT", rob = "some",
                small_values = "undesirable", indirectness = "no",
                outcome_name = "My Outcome")
print(g)
sof_table(g)
```

---

## Background: GRADE and the BMJ 2025 Core GRADE series

GRADE is the international standard for rating certainty of evidence and grading strength of recommendations. The **BMJ 2025 Core GRADE series** (Guyatt G and colleagues) distills the essential judgments needed to summarize comparative evidence:

| Article | Topic | PMID |
|---------|-------|------|
| Introduction | Why Core GRADE is needed | — |
| Core GRADE 1 | Overview and PICO framing | 40262844 |
| Core GRADE 3 | Rating inconsistency | 40328467 |
| Core GRADE 4 | Risk of bias, publication bias, rating up | 40360206 |
| Core GRADE 5 | Assessing indirectness | 40393729 |

GRADE certainty starts at **High** for RCTs (or **Low** for observational studies) and can be downgraded across five domains.

---

## The five GRADE domains

### Overview

| Domain | Auto-computed? | Source |
|--------|---------------|--------|
| Risk of Bias | Partial (auto flowchart when vector supplied) | User argument + meta weights |
| Inconsistency | **Yes** (flowchart auto-detected from I², Q, TE direction) | meta object |
| Indirectness | **No** — manual input | User argument |
| Imprecision | **Yes** | CI width, null crossing, OIS |
| Publication Bias | **Yes** (Egger's test when k ≥ 10) | meta object |

### Downgrade scale

| Judgment | Downgrade |
|----------|-----------|
| `"no"` | 0 |
| `"some"` | −1 |
| `"serious"` | −1 |
| `"very_serious"` | −2 |

### Starting and final certainty

| Study design | Starting score |
|-------------|---------------|
| RCT | 4 (High) |
| Observational | 2 (Low) |

| Final score | Certainty | Symbol |
|-------------|-----------|--------|
| 4 | High | ⊕⊕⊕⊕ |
| 3 | Moderate | ⊕⊕⊕○ |
| 2 | Low | ⊕⊕○○ |
| ≤1 | Very Low | ⊕○○○ |

---

## Domain-by-domain logic

### 1. Risk of Bias (BMJ Core GRADE 4, Fig 2 flowchart)

**Scalar input — flowchart bypassed:**

```r
grade_meta(m, rob = "serious")   # used as-is; no domination check
```

**Per-study vector — BMJ Fig 2 flowchart applied:**

```
Is evidence DOMINATED by high-RoB studies?
(= high/very-high-RoB weight ≥ rob_dominant_threshold of total random-effects weight)

  NO  → judgment = "no" (do not rate down)

  YES → Does including high-RoB studies inflate the apparent effect?
          Automatically checked by comparing:
            TE_all  (pooled estimate including all studies)
            TE_low  (IV-weighted mean of low/some-RoB studies only)

          small_values = "undesirable": inflates = (TE_all > TE_low)
          small_values = "desirable":  inflates = (TE_all < TE_low)
          small_values = NULL:         always rate down (conservative) + warning

          Inflates     → judgment = "serious"  (rate down)
          Conservative → judgment = "no"       (do not rate down)
```

```r
grade_meta(m,
  rob                    = rob_vec,         # character vector, length k
  rob_dominant_threshold = 0.60,            # default: >60% weight = dominated
  small_values           = "undesirable")   # large OR = desirable (response)

grade_meta(m,
  rob          = rob_vec,
  small_values = "desirable")    # small values good (eg, mortality rate)
```

**`small_values` parameter** (consistent with `netmetaviz`):

| Value | Meaning | Example |
|-------|---------|---------|
| `"undesirable"` | Small values are bad; large = good | Response rate, remission |
| `"desirable"` | Small values are good | Mortality, symptom severity |
| `NULL` | Unknown direction | Conservative rate-down + warning |

**Threshold guidance** (BMJ 2025 Core GRADE 4 Fig 2 footnote):
- `rob_dominant_threshold = 0.60` (default): >60% weight = dominated
- `rob_dominant_threshold = 0.55`: more conservative

**Cochrane RoB 2.0 labels accepted directly** — no pre-mapping needed:

| Cochrane RoB 2.0 | Internal GRADE level |
|------------------|---------------------|
| `"No concerns"` | `"no"` |
| `"Some concerns"` | `"some"` |
| `"Serious concerns"` | `"serious"` |
| `"Critical concerns"` | `"very_serious"` |

Plain-English aliases (`"low"`, `"moderate"`, `"high"`) are also accepted.

```r
rob_map <- c(L = "No concerns", S = "Some concerns",
             H = "Serious concerns", `*` = "Some concerns")
rob_vec <- unname(rob_map[df$rob_d])
grade_meta(m, rob = rob_vec, ...)   # works directly
```

**Manual override:**
```r
grade_meta(m, rob = "no")        # bypass flowchart entirely
grade_meta(m, rob = "serious")   # force rate-down regardless of weights
```

---

### 2. Inconsistency (BMJ Core GRADE 3, Fig 2 flowchart)

**Fully auto-detected when all flowchart parameters are `NULL` (default):**

```
AUTO Step 1: Is there important heterogeneity?
  proxy: I² > 25% OR Q p < 0.10

  NO  → judgment = "no" (do not rate down)

  YES → AUTO Step 2: Are most point estimates on one side of null?
          meta$TE is on null = 0 scale for all measures
          (log OR/RR/HR for relative; raw MD/SMD for absolute)
          ≥ 75% on same side → "majority_one_side" → judgment = "some"
          < 75% on same side → "opposite_sides"    → judgment = "serious"
          (subgroup explanation cannot be checked automatically)
```

**Manual flowchart (for full BMJ Core GRADE 3 compliance):**

```r
grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "majority_one_side")
# → judgment = "no"

grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "opposite_sides",
  inconsistency_subgroup_explained = "no")
# → judgment = "serious"
```

**Supplementary statistics** (always computed and noted, never the primary driver):
I², tau², Q p-value.

**Scalar override:**
```r
grade_meta(m, inconsistency = "very_serious")   # overrides flowchart entirely
```

---

### 3. Indirectness (manual — BMJ Core GRADE 5)

Cannot be automated — requires domain expertise.

**Considerations:**
- **Population:** Do trial populations match target patients (age, comorbidities, severity)?
- **Intervention:** Are the interventions the same as those used in practice (dose, delivery)?
- **Outcome:** Are surrogate outcomes used instead of patient-important outcomes?
- **Comparator:** Is the comparator representative of usual care?

```r
grade_meta(m, indirectness = "no")                  # scalar
grade_meta(m, indirectness = indirectness_vec)       # per-study vector
```

---

### 4. Imprecision (auto — BMJ Core GRADE 4)

**Algorithm:**

```
Does the 95% CI cross the null?
  null = 0 on log scale for OR/RR/HR; = 0 for MD/SMD

  NO + OIS met (or not specified)  → judgment = "no"
  NO + OIS NOT met                 → judgment = "serious"
  YES + OIS met (or not specified) → judgment = "serious"
  YES + OIS NOT met                → judgment = "very_serious"
```

**OIS specification options:**

```r
# Option 1: provide target event count directly (binary)
grade_meta(m, ois_events = 400)

# Option 2: provide target total N directly (continuous)
grade_meta(m, ois_n = 300)

# Option 3: auto-calculate from event rates (binary)
# Formula: n_arm = (z_α/2 + z_β)² × [p0(1−p0) + p1(1−p1)] / (p0−p1)²
grade_meta(m, ois_p0 = 0.25, ois_p1 = 0.40, ois_alpha = 0.05, ois_beta = 0.20)

# Option 4: auto-calculate from MID/SD (continuous)
# Formula: n_arm = 2 × (z_α/2 + z_β)² × σ² / δ²
grade_meta(m, ois_delta = 3, ois_sd = 7)
```

---

### 5. Publication Bias (BMJ Core GRADE 4, Fig 5 flowchart)

```
Step 1: Are most or all studies small AND industry-sponsored?
  pubias_small_industry = "yes"        → judgment = "serious" (stop)
  pubias_small_industry = "no" / NULL  → continue

Step 2 (k ≥ 10):
  pubias_funnel_asymmetry = NULL  → run Egger's test automatically
    Egger p < 0.10  → judgment = "serious"
    Egger p ≥ 0.10  → judgment = "no"
  pubias_funnel_asymmetry = "yes" / "no"  → use as-is (override Egger)

Step 2 (k < 10):
  Egger's test underpowered — not run automatically.
  pubias_unpublished = "yes"        → judgment = "serious"
  pubias_unpublished = "no" / NULL  → judgment = "no" (note: k < 10)
```

```r
grade_meta(m,
  pubias_small_industry   = "no",   # explicitly specify
  pubias_funnel_asymmetry = NULL,   # auto Egger (if k ≥ 10)
  pubias_unpublished      = NULL)   # assumed "no" (if k < 10)
```

**Manual override:**
```r
grade_meta(m, pubias_funnel_asymmetry = "yes")   # force rate-down (k ≥ 10)
grade_meta(m, pubias_unpublished      = "yes")   # force rate-down (k < 10)
```

---

## Overriding automatic judgments

Every domain can be overridden with a scalar judgment. Standard workflow: run `grade_meta()` with auto-computed values, inspect the output, then override where your clinical judgment differs.

```r
# Step 1: inspect auto results
g <- grade_meta(m,
  study_design  = "RCT",
  rob           = rob_vec,
  small_values  = "undesirable",
  ois_p0        = 0.25, ois_p1 = 0.40,
  outcome_name  = "Depression response")

print(g)   # inspect domain_assessments

# Step 2: override specific domains
g_override <- grade_meta(m,
  study_design  = "RCT",
  rob           = "some",           # override: single overall judgment
  inconsistency = "serious",        # override: clinical judgment → serious
  indirectness  = "no",
  outcome_name  = "Depression response (manual override)")
```

**Which domains to override and when:**

| Domain | When to override |
|--------|-----------------|
| `rob` scalar | When a single overall judgment is more appropriate than the weight-based flowchart |
| `inconsistency` scalar | When CI differences, clinical context, or subgroup findings warrant a different judgment |
| `indirectness` | Always manual |
| `inconsistency_ci_diff` / `threshold_side` | When you want to apply the full BMJ flowchart manually |
| `ois_events` / `ois_n` | When you have a literature-based OIS |
| `pubias_funnel_asymmetry` | When visual funnel inspection differs from Egger's result |

**The `domain_assessments` tibble tracks judgment source:**

```r
g$domain_assessments
# # A tibble: 5 × 5
#   domain           judgment  auto  downgrade notes
#   Risk of bias     no        FALSE         0 "Not dominated: 38% weight..."
#   Indirectness     no        FALSE         0 NA
#   Inconsistency    some      TRUE         -1 "AUTO Step 1: I²=36% > 25%..."
#   Imprecision      no        TRUE          0 "CI does not cross null; OIS = 311..."
#   Publication bias no        TRUE          0 "Egger p = 0.93"
```

`auto = TRUE` = computed by `pmatools`; `auto = FALSE` = supplied by the user.

---

## Worked example: CBT-I for depression response

Full runnable code in [sample.R](sample.R). Sample data is bundled in `inst/extdata/cbti_depression.csv`.

### Study

**Furukawa Y, Nagaoka D, Sato S, et al.**
*Cognitive behavioral therapy for insomnia to treat major depressive disorder with comorbid insomnia.*
J Affect Disord. 2024;367:359-366. doi:10.1016/j.jad.2024.09.017

- **Question:** Is CBT-I effective for achieving depression response in patients with MDD and comorbid insomnia?
- **Effect measure:** Odds Ratio (OR), random-effects, k = 17 RCTs

> **Note on sample data:** `cbti_depression.csv` is a synthetic dataset that reproduces the structure of the original data. All study names, effect sizes, and sample sizes are fictional.

### GRADE assessment code

```r
m_response <- metabin(
  event.e = event_e, n.e = n_e,
  event.c = event_c, n.c = n_c,
  data = df, studlab = study,
  sm = "OR", method.tau = "REML",
  prediction = TRUE              # compute 95% prediction interval
)

rob_map <- c(L = "No concerns", S = "Some concerns",
             H = "Serious concerns", `*` = "Some concerns")

g_response <- grade_meta(
  meta_obj               = m_response,
  study_design           = "RCT",
  rob                    = unname(rob_map[df$rob_d]),
  rob_dominant_threshold = 0.60,
  small_values           = "undesirable",  # large OR = more response = desirable
  indirectness           = "no",
  ## Inconsistency: auto-detected (or override manually)
  ## inconsistency_ci_diff        = "yes"
  ## inconsistency_threshold_side = "majority_one_side"
  outcome_type           = "relative",
  ois_p0                 = 0.25, ois_p1 = 0.40,
  pubias_small_industry  = "no",
  outcome_name           = "Depression response"
)
```

### Domain-by-domain rationale

| Domain | Judgment | Rationale | Source |
|--------|----------|-----------|--------|
| **Risk of Bias** | no | High-RoB weight ≈ 38% < 60% → NOT dominated | auto flowchart |
| **Indirectness** | no | Directly applicable PICO | manual |
| **Inconsistency** | some | I² ≈ 36% > 25%; majority on same side (OR > 1) | auto |
| **Imprecision** | no | CI [1.64, 3.23] does not cross null; OIS met | auto |
| **Publication Bias** | no | Egger p ≈ 0.93; k = 17 ≥ 10 | auto |

**Final certainty: Moderate ⊕⊕⊕○**
(High − 1 for inconsistency = Moderate)

---

## Output functions

### `sof_table()` — single-outcome Summary of Findings

Columns: **Outcome | k | N | Control rate | Exp. rate | Effect (95% CI) | Certainty**

```r
sof_table(g)                           # pastel palette, per 1,000
sof_table(g, palette = "classic")      # saturated colours
sof_table(g, per = 100)               # per 100 patients
sof_table(g, prediction = TRUE)        # add 95% PI to Effect column
flextable::save_as_docx(sof_table(g), path = "sof.docx")
```

### `grade_table()` — multi-outcome SoF table

```r
grade_table(
  outcomes   = list("Depression response" = g1, "Insomnia remission" = g2),
  primary    = "Depression response",   # → "Primary outcome" (singular)
  palette    = "pastel",
  per        = 1000,
  prediction = FALSE
)
# primary = c("A", "B")   → "Primary outcomes" (plural)
```

### `grade_report()` — Appendix report (docx / html / pdf / md)

```r
## Interactive preview in RStudio Viewer (no file saved)
out <- grade_report(
  outcomes    = list("Depression response" = g1, "Insomnia remission" = g2),
  primary     = "Depression response",
  format      = "html",
  output_dir  = tempdir(),
  output_file = "GRADE_preview",
  per         = 1000,
  prediction  = TRUE
)
rstudioapi::viewer(out)

## Manuscript export
grade_report(
  outcomes    = list("Depression response" = g1, "Insomnia remission" = g2),
  primary     = "Depression response",
  palette     = "pastel",
  format      = c("docx", "md"),
  output_dir  = "outputs/",
  output_file = "GRADE_appendix"
)
```

---

## Event rate columns (Control rate / Experimental rate)

For binary outcomes the SoF table shows:
- **Control rate**: the baseline (control-arm) event rate per `per` patients
- **Experimental (Exp.) rate**: derived from the baseline + pooled relative effect, with 95% CI

### Three ways to specify `baseline_risk`

| Value | Behaviour |
|-------|-----------|
| `0.25` (numeric 0–1) | Used directly |
| `"simple"` | Pooled control-arm proportion: Σ events_c / Σ n_c |
| `"metaprop"` | GLMM-pooled via `meta::metaprop()` (logit back-transform); falls back to `"simple"` if convergence fails |
| `NULL` (default) | Uses `ois_p0` if supplied; otherwise auto-computes via `"simple"` for `metabin` objects |

```r
grade_meta(m, baseline_risk = 0.25, ...)          # explicit
grade_meta(m, baseline_risk = "simple", ...)       # simple pooled
grade_meta(m, baseline_risk = "metaprop", ...)     # GLMM meta-analysis
```

### Experimental rate formula

| Effect measure | Experimental rate (p1) |
|----------------|------------------------|
| RR, HR, IRR | `p0 × RE` |
| OR | `p0 × OR / (1 + p0 × (OR − 1))` |

Display: `X per 1,000 (Y; Z)` where Y; Z are the CI bounds.

### `per` parameter

```r
sof_table(g, per = 1000)   # default: per 1,000 patients
sof_table(g, per = 100)    # per 100 patients
```

---

## Prediction intervals

When the meta object is created with `prediction = TRUE`, the 95% prediction interval can be shown in the Effect column:

```r
m <- metabin(..., prediction = TRUE)   # compute PI
g <- grade_meta(m, ...)
sof_table(g, prediction = TRUE)        # show PI in Effect column
```

Output format in Effect column:
```
OR 2.30 (1.64; 3.23)
PI (0.71; 7.43)
```

---

## Colour palettes

### pastel (default)

| Certainty | Background | Text |
|-----------|-----------|------|
| High | `#d7e8d3` | `#238b21` (green) |
| Moderate | `#cccce9` | `#01008b` (navy) |
| Low | `#f8edd7` | `#daa521` (amber) |
| Very Low | `#e8d0d0` | `#8b0000` (dark red) |

### classic

| Certainty | Background | Text |
|-----------|-----------|------|
| High | `#1e8449` | white |
| Moderate | `#2471a3` | white |
| Low | `#e67e22` | white |
| Very Low | `#c0392b` | white |

---

## API reference

### `grade_meta()`

```r
grade_meta(
  meta_obj,
  study_design  = "RCT",           # "RCT" | "obs"

  ## Risk of Bias
  rob           = NULL,            # scalar | vector length k | column name | NULL
  rob_dominant_threshold = 0.60,   # fraction above which evidence is "dominated"
  small_values  = NULL,            # "undesirable" | "desirable" | NULL (conservative)

  ## Indirectness
  indirectness  = "no",            # scalar | vector | column name

  ## Inconsistency
  inconsistency = NULL,            # scalar override (skips flowchart)
  inconsistency_ci_diff            = NULL,  # "yes" | "no" | NULL (auto-detect)
  inconsistency_threshold_side     = NULL,  # "majority_one_side" | "opposite_sides"
  inconsistency_subgroup_explained = NULL,  # "yes" | "no"

  ## Imprecision
  outcome_type  = "relative",      # "relative" (OR/RR/HR) | "absolute" (MD/SMD)
  ois_events    = NULL,            # binary: target events (direct)
  ois_n         = NULL,            # continuous: target N (direct)
  ois_alpha     = 0.05,            # type I error
  ois_beta      = 0.20,            # type II error (1 − power)
  ois_p0        = NULL,            # control event rate (binary)
  ois_p1        = NULL,            # experimental event rate (binary)
  ois_delta     = NULL,            # MID (continuous)
  ois_sd        = NULL,            # pooled SD (continuous)

  ## Event rate columns
  baseline_risk = NULL,            # numeric | "simple" | "metaprop" | NULL

  ## Publication Bias
  pubias_small_industry   = NULL,  # "yes" | "no"
  pubias_funnel_asymmetry = NULL,  # "yes" | "no" | NULL (auto Egger, k ≥ 10)
  pubias_unpublished      = NULL,  # "yes" | "no" | NULL (k < 10)

  ## Labels
  outcome_name  = NULL             # outcome label for SoF table
)
```

**Returns** an S3 object of class `pmatools`:

| Slot | Content |
|------|---------|
| `$domain_assessments` | tibble: domain, judgment, downgrade, auto, notes |
| `$certainty` | "High" / "Moderate" / "Low" / "Very Low" |
| `$certainty_score` | integer 1–4 |
| `$starting_quality` | starting certainty |
| `$study_design` | "RCT" or "obs" |
| `$outcome_name` | outcome label |
| `$outcome_type` | "relative" or "absolute" |
| `$baseline_risk` | resolved baseline risk (numeric or NULL) |
| `$meta` | original meta object |

### `sof_table(x, palette, per, prediction)`

Generates a single-outcome flextable. `auto = TRUE` rows are computed by `pmatools`.

### `grade_table(outcomes, primary, palette, show_domains, per, prediction)`

Generates a multi-outcome flextable with optional primary/secondary grouping.

### `grade_report(outcomes, primary, palette, format, output_dir, output_file, title, show_domains, per, prediction)`

Exports a full GRADE appendix in docx / html / pdf / md format.

---

## File structure

```
pmatools/
├── pmatools.Rproj             ← open in RStudio (gitignored)
├── .gitignore
├── DESCRIPTION
├── NAMESPACE
├── LICENSE                  ← CC BY 4.0
├── PLAN.md
├── README.md
├── sample.R                 ← worked example; run line-by-line in RStudio
├── R/
│   ├── utils.R              # constants + baseline_risk helpers
│   ├── domain_rob.R         # Risk of Bias flowchart + Cochrane alias normalisation
│   ├── domain_indirectness.R
│   ├── domain_inconsistency.R  # auto-detect + manual flowchart
│   ├── domain_imprecision.R    # OIS auto-calculation
│   ├── domain_pubias.R         # Egger's test + Fig 5 flowchart
│   ├── grade_meta.R            # main function + print/summary
│   ├── sof_table.R             # single-outcome flextable + helpers
│   ├── grade_table.R           # multi-outcome flextable
│   ├── grade_report.R          # Appendix report
│   └── data.R                  # cbti_depression dataset documentation
├── inst/extdata/
│   └── cbti_depression.csv  # bundled sample data (synthetic)
├── data-raw/
│   └── cbti_depression.R    # script to generate data/*.rda
├── tests/testthat/
│   └── test-grade_meta.R
└── outputs/                 # generated output (gitignored)
```

---

## Dependencies

| Package | Role |
|---------|------|
| meta | meta object; `metabias()` for Egger's test |
| flextable | SoF table rendering and Word export |
| officer | Word (docx) report generation |
| tibble, dplyr | Data manipulation |
| rlang | Error/warning handling |
| rmarkdown | html/pdf report generation (Suggests) |

---

## Limitations and future work

1. **GRADE upgrade criteria** (large effect, dose-response, plausible confounding) not yet implemented (v0.2).
2. **GRADEpro GDT integration** (JSON export) planned.

---

## License

[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) — Yuki Furukawa, 2025.

---

## References

- Guyatt G, et al. Why Core GRADE is needed. BMJ 2025;389:bmj-2024-081902.
- Guyatt G, et al. Core GRADE 1: Overview. BMJ 2025. PMID: 40262844.
- Guyatt G, et al. Core GRADE 3: Inconsistency. BMJ 2025. PMID: 40328467.
- Guyatt G, et al. Core GRADE 4: Risk of bias, publication bias. BMJ 2025. PMID: 40360206.
- Guyatt G, et al. Core GRADE 5: Indirectness. BMJ 2025. PMID: 40393729.
- Furukawa Y, et al. CBT-I for MDD with comorbid insomnia. J Affect Disord. 2024;367:359-366.
- Higgins JPT, et al. Measuring inconsistency in meta-analyses. BMJ 2003;327:557.
- Sterne JAC, et al. Recommendations for examining funnel plot asymmetry. BMJ 2011;343:d4002.
