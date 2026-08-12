# pmatools

**End-to-end pairwise meta-analysis with certainty ratings following the Core GRADE series**

`pmatools` is an R package that runs the full pairwise meta-analysis pipeline — from data ingestion (long or wide format) through pooled effect estimation (binary or continuous) and forest/funnel plots — and rates the certainty of the resulting evidence following the **BMJ 2025 Core GRADE series** (Guyatt G et al., BMJ 2025). It produces a Summary of Findings flextable, a multi-outcome evidence-profile table, and a full Appendix report (docx/html/pdf/md), and bundles every artifact plus a reproducible `analysis.R` script into a single ZIP.

> **Disclaimer**: `pmatools` implements the Core GRADE series (Guyatt et al., BMJ 2025), which summarizes GRADE guidance; it is not an official GRADE Working Group tool.

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

End-to-end: data -> meta-analysis -> certainty rating (Core GRADE series) -> SoF -> ZIP.

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

# 4. Certainty rating, Core GRADE series (per-study RoB; Threshold drives
#    RoB / Inconsistency / Imprecision)
g <- grade_meta(ma,
                study_design = "RCT",
                rob          = data$rob[data$treat == "CBT-I"],
                small_values = "undesirable",
                threshold    = 1.25,    # OR-scale Threshold (auto-detected)
                ois_p0       = 0.25,
                outcome_name = "Depression response")

# 4b. Alternative (v0.4): specify the Threshold on the absolute scale.
#     50 per 1,000 ARD, converted to the OR scale at a 25% baseline risk.
g_abs <- grade_meta(ma,
                    study_design       = "RCT",
                    rob                = data$rob[data$treat == "CBT-I"],
                    small_values       = "undesirable",
                    threshold          = 0.05,   # absolute risk difference
                    threshold_scale    = "ard",
                    threshold_baseline = 0.25,   # control-arm risk (default: pooled)
                    ois_p0             = 0.25,
                    outcome_name       = "Depression response")

print(g)
sof_table(g)                 # pastel palette, rates per 1,000
sof_table(g, per = 100)
sof_table(g, prediction = TRUE)

# 5. Reproducible ZIP bundle
export_bundle(ma, g, output_dir = "outputs", bundle_name = "cbti_depression")
```

`outputs/cbti_depression.zip` contains: `data_long.csv`, `analysis.R` (re-runs the
analysis with `library(pmatools)`), `results.txt`, forest/funnel PDF+PNG, the SoF
docx, and the certainty Appendix docx (Core GRADE series).

### Several outcomes in one session (v0.5)

Give the long-format data an `outcome` column and the whole pipeline runs once
per outcome. `run_ma()` still takes a single outcome; `run_ma_multi()` is the
orchestrator above it.

```r
data <- ingest_data("outcomes_long.csv", format = "long")   # has an `outcome` column

ma_list <- run_ma_multi(
  data,
  sm = list("Mortality" = "RR", "Depression severity" = "SMD")  # or one value for all
)

set <- grade_meta_multi(
  ma_list,
  common = list(study_design = "RCT", threshold_type = "mid",
                threshold = 1.25, threshold_scale = "ratio",
                small_values = "undesirable", follow_up = "12 weeks"),
  per_outcome = list(
    "Depression severity" = list(threshold = 0.2, threshold_scale = "auto",
                                 outcome_type = "absolute", unit = "points")
  ),
  primary = "Mortality"
)

print(set)                                   # certainty / rating target / analysis set
set <- reorder_outcomes(set, c("Mortality", "Depression severity"))
grade_table(set, style = "bmj")              # rows follow set$order
export_bundle(set, output_dir = "outputs", bundle_name = "all_outcomes")
```

`outputs/all_outcomes.zip` puts the summary table, the evidence profiles, the
multi-outcome `analysis.R`, the data and a `README.txt` at the top level, and one
`outcomes/NN_name/` directory per outcome (plots, `results.txt`, that outcome's
data and evidence profile). An outcome that fails to fit or rate is skipped with
a warning rather than taking the session down — the one exception being the Core
GRADE 2 entry gate, which still aborts.

Full walkthrough, `pmatools_set` reference and the export layout:
[Multi-outcome workflow](#multi-outcome-workflow-v05).

### Shorter version: certainty rating only, on an existing meta object

```r
m <- meta::metabin(event.e = c(10,15,20), n.e = c(50,60,70),
                   event.c = c(15,20,25), n.c = c(50,60,70),
                   studlab = c("A","B","C"), sm = "OR",
                   prediction = TRUE)

# v0.4 breaking change: every manual domain-judgment override (scalar rob,
# indirectness != "no", inconsistency, imprecision, pubias_funnel_asymmetry)
# requires a matching *_rationale argument.
# v0.5 breaking change: threshold_type defaults to "mid", which requires a
# threshold. Use threshold_type = "null" to rate certainty in a true
# underlying effect, or require_threshold = FALSE to run without a MID.
g <- grade_meta(m, study_design = "RCT", rob = "some_concerns",
                rob_rationale = "RoB2 consensus: some concerns from missing outcome data",
                small_values = "undesirable", indirectness = "no",
                threshold = 1.25, threshold_scale = "ratio",
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
| Core GRADE 2 | Choosing the target of certainty rating and assessing imprecision | doi:10.1136/bmj-2024-081904 |
| Core GRADE 3 | Rating inconsistency | 40328467 |
| Core GRADE 4 | Risk of bias, publication bias, rating up | 40360206 |
| Core GRADE 5 | Assessing indirectness | 40393729 |

GRADE certainty starts at **High** for RCTs (or **Low** for observational studies) and can be downgraded across five domains.

---

## The five Core GRADE domains

### Overview

| Domain | Auto-computed? | Source |
|--------|---------------|--------|
| Risk of Bias | Partial (auto flowchart when vector supplied) | User argument + meta weights |
| Inconsistency | **Yes** (flowchart auto-detected from I², Q, TE direction) | meta object |
| Indirectness | **No** — manual input (optionally as PICO subdomains) | User argument |
| Imprecision | **Yes** | CI width, null crossing, OIS |
| Publication Bias | **Yes** (Egger's test when k ≥ 10) | meta object |

### Downgrade scale

| Judgment | Downgrade |
|----------|-----------|
| `"no"` | 0 |
| `"some_concerns"` | −1 |
| `"serious"` | −2 |

Legacy labels are still accepted and normalized: `"some"` → `"some_concerns"` (−1), `"very_serious"` → `"serious"` (−2).

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

### 0. Target of the certainty rating (BMJ Core GRADE 2, Fig 2)

Before any domain is judged, Core GRADE asks *what* the certainty is about.
`grade_meta()` makes that choice explicit and mandatory.

```r
# "Is there an important effect or not?" -> MID threshold (default).
# A threshold is REQUIRED; the error message quotes suggest_threshold().
grade_meta(m, threshold_type = "mid", threshold = 1.25, threshold_scale = "ratio")

# "Is there a true underlying effect, benefit or harm?" -> null threshold.
grade_meta(m, threshold_type = "null")

# Escape hatch: keep the pre-0.5 MID-free behaviour.
grade_meta(m, require_threshold = FALSE)
```

The target is then derived from the pooled point estimate:

| `threshold_type` | point estimate | target | threshold used by Imprecision |
|---|---|---|---|
| `"mid"`  | \|TE\| > MID   | `important_effect`        | ±MID |
| `"mid"`  | \|TE\| ≤ MID   | `little_to_no_difference` | ±MID |
| `"null"` | very near null | `little_to_no_difference` | ±MID |
| `"null"` | not near null  | `non_null_effect`         | null (0) |

"Very near null" is operationalized as \|TE\| ≤ MID (Core GRADE 2 gives no
numeric definition). Without a MID, nearness cannot be judged and the target
falls back to `non_null_effect`. The result is on the object as
`$rating_target`, `$rating_target_note` and `$rating_target_auto`, is printed
by `print()`, and is appended to the Imprecision domain notes.

Override it — with a mandatory rationale — when the panel disagrees:

```r
grade_meta(m, threshold = 1.25, threshold_scale = "ratio",
           rating_target = "little_to_no_difference",
           rating_target_rationale = "Panel targets an unimportant effect")
```

### 1. Risk of Bias (5-rule MECE zone-based decision; aligned with BMJ Core GRADE 4)

**Scalar input — flowchart bypassed** (v0.4.0+: requires `rob_rationale`):

```r
grade_meta(m, rob = "serious",
           rob_rationale = "RoB2 consensus: high risk of bias in most domains")
```

**Per-study vector — Core GRADE 4 Fig 2 flowchart applied:**

Studies are first folded into a binary low / high classification
(`rob_some_concerns` decides which side "some concerns" lands on), then the
figure's first node asks whether the high-RoB studies **dominate** the
evidence — whether they carry at least `rob_dominant_threshold` (default
`0.55`, compared with `>=`) of the inverse-variance weight. The Fig 2 footnote
names two candidate thresholds — ">65% weight or ≥55% weight = possibly
dominating" — and pmatools defaults to the conservative one; pass `0.65` for
the stricter reading.

```
w_high >= rob_dominant_threshold ?
├── yes → check direction of bias (the 5-rule zone decision below)
│           bias could account for the effect / its absence → rate down
│           bias would under-estimate the effect             → do not rate down
└── no  → appreciable evidence from low-RoB studies?
          substantial difference between the high- and low-RoB estimates?
            ├── yes → do not rate down; use low risk of bias studies only
            └── no  → do not rate down; use all studies
```

The non-dominated branch **never rates the domain down**. When it lands on
"use low risk of bias studies only", `grade_meta()` refits the meta-analysis on
the low-RoB subset (`rob_refit = TRUE`, the default), so every other domain,
the rating target and the SoF table use the restricted estimate. The original
analysis stays available as `$meta_full`; `$rob_analysis_set` and `$rob_refit`
record what happened, and `sof_table()` footnotes the restriction.

**Direction-of-bias check (dominated branch) — 5-rule zone decision:**

`pmatools` classifies the pooled effect from all studies (`TE_all`) and the
IV-weighted pooled effect of low / some-RoB studies (`TE_low`) into one of three
zones defined by the clinical decision Threshold on the analysis scale:

- `above`   : TE > +Threshold
- `trivial` : −Threshold ≤ TE ≤ +Threshold
- `below`   : TE < −Threshold

Then a single MECE 3×3 table determines the downgrade:

| zone(TE_all) → / zone(TE_low) ↓ | **above** | **trivial** | **below** |
|---|---|---|---|
| **above**    | rule 2 / 3 | rule 4    | **rule 5** |
| **trivial**  | rule 4    | rule 1    | rule 4    |
| **below**    | **rule 5** | rule 4    | rule 2 / 3 |

```
Rule 1: same trivial zone                                         → no
Rule 2: same non-trivial zone, inflation ≤ 10%                    → no
Rule 3: same non-trivial zone, bias-favouring inflation > 10%     → some_concerns (−1)
Rule 4: zone differs without sign flip                            → some_concerns (−1)
Rule 5: zone differs with sign flip (above ↔ below)               → serious       (−2)
```

`inflation_ratio = (|TE_all| − |TE_low|) / |TE_low|` is evaluated only when the
sign of the inflation matches the bias-favouring direction (per `small_values`);
deflation in the bias-favouring direction never triggers a downgrade.

```r
grade_meta(m,
  rob                     = rob_vec,        # character vector, length k
  threshold               = 1.20,           # Threshold on natural scale
  threshold_scale         = "ratio",        # OR/RR/HR/RoM: ratio; MD/SMD: te_scale
  small_values            = "undesirable",  # large OR = good (eg, response)
  rob_inflation_threshold = 0.10)           # rule 3 trigger; default 10%
```

**Threshold default: `suggest_threshold()`.** Returns a conventional clinical
decision Threshold for the effect measure — OR 1.25, RR/HR 1.20, RoM 1.10,
SMD 0.20, MD 0.20 × pooled SD, ARD 0.05 — as
`list(threshold_user, threshold_scale)`, suitable for pre-filling an input
field. Override with a published or expert-derived Threshold whenever available.

```r
suggest_threshold(ma)
#> $threshold_user  1.25
#> $threshold_scale "ratio"
```

**`small_values` parameter** (consistent with `netmetaviz`):

| Value | Meaning | Example |
|-------|---------|---------|
| `"undesirable"` | Small values are bad; large = good | Response rate, remission |
| `"desirable"` | Small values are good | Mortality, symptom severity |
| `NULL` | Unknown direction | Bias direction inferred from |TE| comparison |

**Fallback when Threshold is not supplied** (`threshold = NULL`): the trivial
zone collapses to `{0}`, so only sign flips can change zones. The algorithm
reduces to a sign-flip check (rule 5 vs rule 2/3); rule 1 and rule 4 cannot
fire.

**Dominance gate: deprecation retracted.** `rob_dominant_threshold` was
deprecated in v0.3.1 ("accepted but ignored"), on the reasoning that the
zone-and-magnitude comparison subsumed it. That decision is **retracted** in
the current development version: the gate is the first decision node of Core
GRADE 4 Fig 2, and the two branches beneath it are not interchangeable — one
can rate down, the other only chooses the analysis set. The argument is live
again, with a default of `0.55` — one of the two thresholds the Fig 2 footnote
names (the earlier `0.60` matched neither). (The CI-overlap and CI-significance
branches removed in v0.3.1 stay removed; they really are subsumed by the
zone comparison.)

**Substantial difference is judged on magnitude alone.** On the non-dominated
branch the figure asks "whether low and high risk of bias studies suggest
similar or **substantially different magnitudes of effect**" — a symmetric
question. The `small_values` direction gate therefore applies only to the
dominated branch, whose node is explicitly "check direction of bias". A body of
evidence whose *low*-RoB studies show the larger effect is a substantial
difference too.

**No automatic two-level downgrade.** Every leaf of Fig 2 reads "rate down" /
"do not rate down", and Core GRADE 4 describes no two-level risk-of-bias
downgrade. The automated flowchart therefore stops at `some_concerns` (−1),
including the sign-flip rule and the all-studies-high-RoB case. Use the scalar
`rob = "serious"` (with `rob_rationale`) when −2 is genuinely warranted.

**Study-level overrides.** A single study's classification can be corrected
without rebuilding the vector. Both arguments are named character vectors
keyed on `studlab`; a key that matches no study label is an error, and every
override needs a written rationale:

```r
grade_meta(m,
  rob                    = rob_vec,
  rob_overrides          = c("Smith 2020" = "high"),
  rob_override_rationale = c("Smith 2020" = "Unblinded outcome assessment"),
  ...)
```

**Cochrane RoB 2.0 labels accepted directly** — no pre-mapping needed:

| Cochrane RoB 2.0 | Internal GRADE level |
|------------------|---------------------|
| `"No concerns"` | `"no"` |
| `"Some concerns"` | `"some_concerns"` |
| `"Serious concerns"` | `"serious"` |
| `"Critical concerns"` | `"serious"` |

Plain-English aliases (`"low"`, `"moderate"`, `"high"`) are also accepted.

```r
rob_map <- c(L = "No concerns", S = "Some concerns",
             H = "Serious concerns", `*` = "Some concerns")
rob_vec <- unname(rob_map[df$rob_d])
grade_meta(m, rob = rob_vec, ...)   # works directly
```

**Manual override** (v0.4.0+: a scalar override requires `rob_rationale` —
Core GRADE transparency principle):
```r
grade_meta(m, rob = "no",        # bypass flowchart entirely
           rob_rationale = "RoB2 consensus: all domains low risk")
grade_meta(m, rob = "serious",   # force rate-down regardless of weights
           rob_rationale = "RoB2 consensus: high risk of bias in most domains")
```

**Visual companion: `plot_forest_rob()`.** Re-runs the meta-analysis with a
Risk-of-Bias subgroup (low / some / high / unknown) and draws a forest plot
with per-stratum pooled estimates next to the overall diamond — useful to see
whether high-RoB studies inflate the apparent effect. Accepts the same label
aliases as `grade_meta()` (`"L"/"S"/"H"`, `"Some concerns"`, ..., matched
case-insensitively); `NA` / `""` / `"?"` are kept as their own "unknown"
stratum, and any other unrecognized label is bucketed into "unknown" **with a
warning** (so a mistyped vocabulary can no longer collapse every trial into a
single meaningless stratum). `plot_forest_indirectness()` uses the same
vocabulary.

```r
plot_forest_rob(ma, rob = rob_vec)   # rob_vec: character, length k
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
          ≥ 75% on same side → "majority_one_side" → judgment = "some_concerns"
          < 75% on same side → "opposite_sides"    → judgment = "some_concerns"
          (subgroup explanation cannot be checked automatically)
```

**The chosen threshold is shared with Imprecision (v0.5.1).** Core GRADE 3
Fig 2 node 2 reads "Evaluate point estimates of studies **in relation to
chosen threshold**", and its Fig 4 example shows the verdict reversing with
that choice. The zone classification therefore uses the same threshold the
rating target resolved for Imprecision: ±MID for an important-effect or
little-to-no-difference target, the null for a non-null-effect target. Before
v0.5.1 this domain received the raw MID even when Imprecision was rating
against the null.

**No automated two-level downgrade (v0.5.1).** Core GRADE 3: "A final issue is
consideration of rating down twice for inconsistency. Although this is a
theoretical possibility, we have found compelling reason to rate down twice for
inconsistency sufficiently unusual that it need not concern users of Core
GRADE." Every automated and flowchart path now stops at `some_concerns` (−1);
−2 requires the scalar override below.

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
# → judgment = "some_concerns" (−1; capped, see above)
```

**Supplementary statistics** (always computed and noted, never the primary driver):
I², tau², Q p-value.

**Scalar override** (v0.4.0+: requires `inconsistency_rationale`):
```r
grade_meta(m, inconsistency = "serious",   # overrides flowchart entirely (−2)
           inconsistency_rationale = "Clinically divergent effects across settings")
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

**Per-study input is aggregated by weight share, not worst case (v0.5.1).**
Indirectness is a judgment about the *body* of evidence, and Core GRADE 5
frames it that way: "if Core GRADE users are interested in effects in elderly
people but **all or almost all** evidence comes from younger people ... they
lack the data to test whether effects differ across these variables." A
worst-case fold rated the whole body of evidence down when one study out of
eighteen was indirect, which is the opposite of "all or almost all". A vector
or column name is now resolved as:

```
w_serious >= indirectness_dominant_threshold  → "serious"       (-2)
w_any     >= indirectness_dominant_threshold  → "some_concerns" (-1)
otherwise                                     → "no"
```

where `w_serious` / `w_any` are the inverse-variance weight shares carried by
the `"serious"` studies and by the `"some_concerns"` + `"serious"` studies (the
count share is used, and flagged, when weights are unavailable). **The
threshold has no basis in Core GRADE 5**, which gives no numeric
operationalisation of "all or almost all"; the `0.55` default is a pmatools
convention aligned with `rob_dominant_threshold`, and every aggregated domain
note says so. The `indirectness_subdomains` table below keeps its worst-case
fold — subdomains are facets of one judgment, not units of evidence.

#### PICO subdomains (v0.5): `indirectness_subdomains`

Core GRADE 5 asks the indirectness question **separately for each element of the
PICO**, on a 4-point scale, rather than as one global judgment. Pass a data frame
with the columns `subdomain`, `target`, `evidence` and `judgment`, one row per
PICO element:

```r
ind_sub <- data.frame(
  subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
  target    = c("Adults with major depressive disorder and comorbid insomnia",
                "Cognitive behavioural therapy for insomnia (CBT-I)",
                "Treatment as usual or attention control",
                "Depression response (>=50% reduction in severity)"),
  evidence  = c("17 RCTs, mostly recruited in sleep or psychiatry clinics.",
                "Therapist-delivered and digital CBT-I; both used in practice.",
                "Comparators ranged from waitlist to active attention control.",
                "Response derived from continuous scales at 8-12 weeks only."),
  judgment  = c("yes", "probably_yes", "probably_no", "probably_yes"),
  stringsAsFactors = FALSE
)

g_ind <- grade_meta(m_response,
                    study_design            = "RCT",
                    rob                     = rob_vec,
                    small_values            = "undesirable",
                    threshold_type          = "mid",
                    threshold               = 1.25,
                    threshold_scale         = "ratio",
                    indirectness_subdomains = ind_sub,   # `indirectness` left at NULL
                    ois_p0                  = 0.25, ois_p1 = 0.40,
                    outcome_name            = "Depression response")
```

**Judgment mapping** ("Is the evidence sufficiently direct?"):

| Subdomain judgment | Levels down |
|---|---|
| `"yes"` | 0 |
| `"probably_yes"` | 0 |
| `"probably_no"` | −1 (serious indirectness) |
| `"no"` | −2 (very serious indirectness) |

Aliases such as `"Probably No"` are normalized. The **domain judgment defaults to
the worst case across the four subdomains** — in the example above
`probably_no` for Comparison drives the whole domain to `some_concerns` (−1),
even though the other three subdomains raise no concern.

The normalized table comes back on the object as `$indirectness_subdomains`;
`$domain_assessments` keeps its one-row-per-domain schema, so nothing downstream
has to change.

**Overriding the worst case requires a rationale.** Leave `indirectness` at its
`NULL` default whenever the subdomain worst case should stand: once a subdomain
table is supplied, *any* non-`NULL` scalar `indirectness` is read as a manual
override and needs `indirectness_rationale`.

```r
# Aborts: "Overriding the Indirectness judgment requires indirectness_rationale ..."
grade_meta(m_response, indirectness_subdomains = ind_sub, indirectness = "no",
           threshold = 1.25, threshold_scale = "ratio")

# Correct form of the override
grade_meta(m_response, indirectness_subdomains = ind_sub,
           indirectness           = "no",
           indirectness_rationale = "Panel judged the waitlist comparator acceptable",
           threshold = 1.25, threshold_scale = "ratio")
```

#### `indirectness_table()` — BMJ Core GRADE 5 judgment table

```r
indirectness_table(g_ind)                       # flextable, renders in the RStudio Viewer
flextable::save_as_docx(indirectness_table(g_ind), path = "indirectness.docx")
```

Renders `$indirectness_subdomains` in the publication format used by Core GRADE
5: the target question, the evidence found, a colour-graded 4-option judgment row
with the recorded answer ticked, and a merged "Judgment across subdomains" row
carrying the overall judgment. It aborts (with a message telling you how to
record subdomains) when the object has none. `export_bundle()` writes it as
`indirectness_table.docx` for every outcome that recorded subdomains.

---

### 4. Imprecision (auto — BMJ Core GRADE 2, Fig 4)

**Algorithm (Core GRADE 2 Fig 4):**

```
Does the CI cross the chosen threshold?
  (the target of the rating decides which threshold — see §0 above)

Yes -> rate down one level                                        (−1)
       rate down two levels when the CI crosses BOTH ±MID          (−2)
       (important benefit and important harm)
       -> sample size is NOT considered on this path

No  -> effect moderate       -> do not rate down                   (0)
                                (the OIS is not consulted at all)
    -> effect implausibly large -> OIS approach:
         Continuous: N >= OIS (or 800)  -> do not rate down        (0)
                     N <  OIS           -> rate down one level    (−1)
                     N <  30% of OIS    -> rate down two levels   (−2)
         Binary:     RR CI ratio >= 3 or OR CI ratio >= 2.5
                                        -> rate down two levels   (−2)
                     otherwise, calculate OIS:
                       N >= OIS         -> do not rate down        (0)
                       N <  OIS         -> rate down one level    (−1)
```

**The two-level branch applies on the null-threshold path too (v0.5.1).**
Core GRADE 2: "The two considerations also apply to imprecision judgments when
Core GRADE users choose the null as the threshold of interest. For example,
consider a situation in which users rate their certainty in a benefit
(threshold the null) but the CI also includes clearly important harm. The
finding that the CI is consistent with both benefit and important harm
motivates a plain language summary stating that the intervention 'may' result
in a benefit, and rating down two levels for imprecision." So whenever a MID is
available, the ±MID span is evaluated even when the −1/−0 decision is made
against the null. Without a MID the two-level check is undecidable and the
judgment stops at −1.

**OIS is compared in participants (v0.5.1).** Fig 4 caption: "N=number of
participants; OIS=optimal information size". The auto-computed binary OIS is a
target sample size compared against `sum(n.e) + sum(n.c)`, not a target event
count; the implied event count is still reported in the notes. Supplying
`ois_events` explicitly keeps the event-based comparison.

"Implausibly large" follows the paper's binary wording (relative risk
reduction > 40% certainly, > 30% possibly). Core GRADE 2 does not define it
for continuous outcomes; pmatools uses Cohen's convention (standardized
effect >= 0.8) there and says so in the domain notes. The CI ratio is the
upper CI bound divided by the lower bound on the ratio scale (Fig 4 caption).
The domain notes always record which Fig 4 path produced the judgment.

**OIS specification options:**

```r
# Option 1: provide target event count directly (binary; event-based comparison)
grade_meta(m, ois_events = 400)

# Option 2: provide target total N directly
grade_meta(m, ois_n = 300)

# Option 3: auto-calculate from event rates (binary)
# Formula: n_arm = (z_α/2 + z_β)² × [p0(1−p0) + p1(1−p1)] / (p0−p1)²
grade_meta(m, ois_p0 = 0.25, ois_p1 = 0.40, ois_alpha = 0.05, ois_beta = 0.20)

# Option 3b (default, binary): p1 comes from a modest relative risk reduction.
# Core GRADE 2: "the control group event rate (chosen from the context), and a
# modest relative risk reduction, typically 20% or 25%". p0 defaults to the
# pooled control-arm rate; the MID is NOT used for binary OIS.
grade_meta(m, ois_rrr = 0.25)          # ois_p1 = ois_p0 × (1 − 0.25)

# Option 4: auto-calculate from Threshold/SD (continuous — the MID is used here)
# Formula: n_arm = 2 × (z_α/2 + z_β)² × σ² / δ²
grade_meta(m, ois_delta = 3, ois_sd = 7)
```

---

### 5. Publication Bias (BMJ Core GRADE 4, Fig 5 flowchart)

```
Q1: Are most or all studies small AND industry-sponsored?
  pubias_small_industry = "yes"        → judgment = "some_concerns" (-1; stop)
  pubias_small_industry = "no" / NULL  → continue

[After Q1] pmatools convenience input (NOT a node of Fig 5):
  pubias_registry_complete = "yes"
    → judgment = "no" (stop; the user asserts complete pre-registration coverage)

Q2: Is statistical analysis feasible (k ≥ 10)?
  YES → Q3
  NO  → Q4

Q3 (k ≥ 10): Visual asymmetry / Egger's test
  pubias_funnel_asymmetry = NULL  → run Egger's test automatically
    Egger p < 0.05           → judgment = "some_concerns"  (-1)
    Egger p ≥ 0.05           → judgment = "no"
  pubias_funnel_asymmetry = "yes"  → judgment = "some_concerns" (-1; visual override)
  pubias_funnel_asymmetry = "no"   → judgment = "no"             (visual override)

Q4 (k < 10): Documentation of unpublished studies
  pubias_unpublished = "yes"        → judgment = "some_concerns" (-1)
  pubias_unpublished = "no" / NULL  → judgment = "no" (NULL: assumed "no" with warning)
```

**Registry rule-out is evaluated after Q1 (v0.5.1).** Core GRADE 4 Fig 5 has
exactly four decision nodes and no structural rule-out; its only registry node
is Q4. Evaluating `pubias_registry_complete` first, as pmatools did up to
v0.5.0, let a body of small industry-sponsored trials escape the Q1 downgrade.
The domain note now states that the rule-out is the user's assertion rather
than a figure node.

**No two-level publication-bias downgrade, and no p-value in the source
(v0.5.1).** The `p < 0.01 → serious (-2)` tier is gone. Fig 5's asymmetry node
asks qualitatively whether the evidence "strongly suggests publication bias"
and names no threshold; the surviving `p < 0.05` cut-off is a pmatools
operational convention and is labelled as such in the domain notes.

```r
grade_meta(m,
  pubias_registry_complete = "no",   # default; "yes" if all trials pre-registered
  pubias_small_industry    = "no",
  pubias_funnel_asymmetry  = NULL,   # auto Egger (if k ≥ 10)
  pubias_unpublished       = NULL)   # assumed "no" (if k < 10)
```

**Visual inspection.** A contour-enhanced funnel plot is available as
`plot_funnel(g)` (or `plot_funnel(meta_obj)`). Reviewers who want to override
the auto Egger judgment from visual inspection can pass
`pubias_funnel_asymmetry = "yes"` or `"no"`.

**Trim-and-fill diagnostics.** Trim-and-fill no longer drives the certainty
judgment (the Egger asymmetry check supersedes the previous sign-flip escalation),
but the imputed studies and adjusted random-effects summary are still
informative. They are available through `plot_trimfill_forest(g)` for display
in the Reporting bias tab of the companion Shiny app.

**Available vs missing results: `plot_forest_pubias_subgroup()`.** Draws the
RoB-ME-style two-subgroup forest (Page et al., BMJ 2023): "Available results"
with the usual estimates and pooled diamond, stacked over "Missing results" —
registry/protocol entries supplied via `missing_df` (columns `studlab`, `n`,
`results_known`) plus auto-detected studies whose effect estimate is `NA`
(e.g. all-zero events). Missing rows show a status string in place of an
estimate and contribute nothing to pooling. Reference-only diagnostic — it
does not drive the certainty judgment.

```r
miss <- data.frame(
  studlab       = "Trial X",
  n             = 40L,
  results_known = "Measured but not reported (suspect P > 0.05)")
plot_forest_pubias_subgroup(ma, missing_df = miss)
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
  rob           = "some_concerns",  # override: single overall judgment
  rob_rationale = "RoB2 consensus: some concerns from missing outcome data",
  inconsistency = "serious",        # override: clinical judgment → serious
  inconsistency_rationale = "Clinically divergent effects across settings",
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
#   Inconsistency    some_concerns TRUE     -1 "AUTO Step 1: I²=36% > 25%..."
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

### Certainty assessment code (Core GRADE series)

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
  rob_dominant_threshold = 0.55,
  small_values           = "undesirable",  # large OR = more response = desirable
  indirectness           = "no",
  ## Core GRADE 2 entry gate (v0.5): threshold_type defaults to "mid", so a
  ## minimal important difference is mandatory. suggest_threshold(m_response)
  ## returns the conventional OR 1.25 for a binary outcome.
  threshold_type         = "mid",
  threshold              = 1.25,
  threshold_scale        = "ratio",
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
| **Inconsistency** | some_concerns | I² ≈ 36% > 25%; all 17 estimates favour CBT-I, but no single zone around ±MID holds ≥ 80% → magnitude genuinely heterogeneous | auto |
| **Imprecision** | no | OR 2.33, CI [1.66, 3.26] lies entirely beyond the MID; OIS met (1,222 / 97 events) | auto |
| **Publication Bias** | no | Egger p ≈ 0.93; k = 17 ≥ 10 | auto |

Rating target: **important effect** (Core GRADE 2 Fig 2), derived automatically
because \|point estimate\| exceeds the MID.

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

### `evidence_profile()` — single-outcome Evidence Profile (Core GRADE series)

Evidence Profile layout of the BMJ 2025 Core GRADE series for one outcome:
**Outcome | No of studies (N) | Design | Risk of bias | Inconsistency |
Indirectness | Imprecision | Other considerations | Certainty**, with numbered
footnotes for every rated-down domain.

```r
evidence_profile(g)
evidence_profile(g, palette = "classic",
                 other_text = "Strong plausible confounding",
                 other_downgrade = -1L)   # extra downgrade on top of domains
flextable::save_as_docx(evidence_profile(g), path = "evidence_profile.docx")
```

### `style = "bmj"` — BMJ Core GRADE Summary of Findings layout (v0.5)

`sof_table()` and `grade_table()` take a `style` argument. **The GRADEpro layout
is the default and is unchanged** — existing calls keep producing exactly the
table they produced in v0.4. `style = "bmj"` switches to the Summary of Findings
layout used in the BMJ Core GRADE series.

```r
sof_table(g_response, style = "bmj", follow_up = "8-12 weeks")

# continuous outcome: name the unit of the absolute difference
sof_table(g_severity, style = "bmj", follow_up = "12 weeks", unit = "PHQ-9 points")

grade_table(list("Depression response" = g1, "Insomnia remission" = g2),
            primary = "Depression response", style = "bmj")
```

The BMJ layout differs from GRADEpro in what each column carries:

| Column | Example content |
|---|---|
| Outcome and follow-up | `Depression response` / `8-12 weeks` |
| No of participants (No of studies and type) | `4762 (17 randomised controlled trials)` — the design is spelled out |
| Relative effect (95% CI) | `Odds ratio 2.33 (1.66 to 3.26)` — the measure is spelled out, not just "OR" |
| **Absolute effects (95% CI)** | A spanning block over three columns: *With control* `250 per 1000`, *With intervention* `437 per 1000 (356 to 521)`, and a new **Difference** column `187 more per 1000 (106 more to 271 more)`. `unit` labels the difference for continuous outcomes |
| Certainty of evidence (quality of evidence) | `Moderate` / `Due to serious inconsistency` — the domains that pulled it down |
| Plain language summary | `Treatment likely has an important benefit` (see below) |

**Plain language summaries** are the statements from **Core GRADE 2, Table 1**,
carried verbatim. Which one is used follows from the certainty level,
`threshold_type` and the derived `rating_target` — pmatools does not paraphrase
them. Objects created before the Core GRADE 2 entry gate (no `$rating_target`)
simply omit the column rather than guessing.

When the rated analysis is a low-risk-of-bias refit, the table carries a
footnote saying so; `grade_table()` numbers the marker per row, so a table
mixing analysis sets says which rows were restricted.

`grade_report()` accepts the same `style` argument (`"gradepro"` by default).
`export_bundle()` on a `pmatools_set` is the one place where **`"bmj"` is the
default**; the single-outcome bundle has no `style` argument and always writes
the GRADEpro layout.

---

## Multi-outcome workflow (v0.5)

Calling `grade_meta()` by hand once per outcome stops scaling after two or three
outcomes. Give the long-format data an **`outcome` column** and the whole
pipeline runs once per outcome:

```
ingest_data()        long data with an `outcome` column
      |
run_ma_multi()       splits on `outcome`, one run_ma() per outcome
      |              -> named list of meta objects
grade_meta_multi()   one grade_meta() per outcome (common + per-outcome args)
      |              -> pmatools_set
reorder_outcomes()   row order of the summary table = export directory numbering
set_primary()        primary / secondary grouping
      |
grade_table()        one summary table for the whole set
export_bundle()      ZIP with outcomes/NN_name/ sub-directories
```

`run_ma()` itself is unchanged and still **aborts** on data holding more than one
outcome — `run_ma_multi()` is the only supported way to batch.

### Worked example

Runs end to end on the bundled sample data (see also `sample.R`, section 9).
`df`, `rob_vec` and the CBT-I data frame come from the
[worked example](#worked-example-cbt-i-for-depression-response) above.

```r
## One long table holding both outcomes ------------------------------------
long_response <- rbind(
  data.frame(studlab = df$study, outcome = "Depression response",
             treat = "experimental", n = df$n_e, event = df$event_e,
             rob = rob_vec, stringsAsFactors = FALSE),
  data.frame(studlab = df$study, outcome = "Depression response",
             treat = "control", n = df$n_c, event = df$event_c,
             rob = rob_vec, stringsAsFactors = FALSE)
)
long_remission <- long_response
long_remission$outcome <- "Insomnia remission"
long_remission$event   <- pmax(0, round(long_remission$event * 0.7))  # placeholder

data_multi <- ingest_data(rbind(long_response, long_remission), format = "long")

## One meta-analysis per outcome -------------------------------------------
ma_list <- run_ma_multi(data_multi, sm = "OR", method.tau = "REML", incr = 0.1)
names(ma_list)
#> [1] "Depression response" "Insomnia remission"

## One certainty rating per outcome ----------------------------------------
set <- grade_meta_multi(
  ma_list,
  ## arguments shared by every outcome
  common = list(
    study_design          = "RCT",
    threshold_type        = "mid",
    threshold             = 1.25,
    threshold_scale       = "ratio",
    small_values          = "undesirable",
    rob                   = rob_vec,
    pubias_small_industry = "no",
    follow_up             = "8-12 weeks"   # BMJ "Outcome and follow-up" column
  ),
  ## arguments for one outcome only; these override `common`
  per_outcome = list(
    "Depression response" = list(ois_p0 = 0.25, ois_p1 = 0.40),
    "Insomnia remission"  = list(ois_p0 = 0.18, ois_p1 = 0.30)
  ),
  primary = "Depression response"
)

print(set)
#> -- Multi-outcome Certainty Set (Core GRADE series) -------
#>  Outcomes : 2
#>  Primary  : Depression response
#>  Data     : 68 rows of long-format data
#>
#>   #  Outcome / certainty / rating target / analysis set
#>   1  Depression response  [primary]
#>      Moderate   | Important effect         | all studies
#>   2  Insomnia remission
#>      High       | Important effect         | all studies
#> ----------------------------------------------------------

## Order and grouping ------------------------------------------------------
set <- reorder_outcomes(set, c("Insomnia remission", "Depression response"))
set <- set_primary(set, "Depression response")

## Outputs -----------------------------------------------------------------
grade_table(set, style = "bmj")     # rows follow set$order
grade_report(set, format = "docx", output_dir = "outputs")
export_bundle(set, output_dir = "outputs", bundle_name = "cbti_multi")
```

`sm` and `outcome_type` may be a single value (applied to every outcome) or a
list named by outcome, so **binary and continuous outcomes can share one
session**:

```r
run_ma_multi(data_multi,
             sm           = list("Mortality" = "RR", "Depression severity" = "SMD"),
             outcome_type = list("Mortality" = "binary",
                                 "Depression severity" = "continuous"))
```

### The `pmatools_set` class

`grade_meta_multi()` returns a `pmatools_set`:

| Field | Contents |
|---|---|
| `$outcomes` | named list of `pmatools` objects (a failed outcome is `NULL`) |
| `$order` | display order; every outcome exactly once |
| `$primary` | primary outcomes (possibly empty) |
| `$data` | the long-format data the set was built from |

`print()` / `summary()` list each outcome's certainty, rating target and
analysis set — a low-risk-of-bias refit is called out per outcome, and a set
mixing analysis sets says so. `grade_table()`, `grade_report()` and
`export_bundle()` all accept the set directly; the named-list API is unchanged.

`reorder_outcomes(set, order)` requires `order` to list every outcome exactly
once. `set_primary(set, primary)` sets the primary group; `NULL` clears it.
Both drive the summary-table row order **and** the numbering of the export
sub-directories.

**Failure semantics.** An outcome that fails to fit or rate is recorded as
`NULL` with a warning, so the rest of the batch completes. The one exception is
the Core GRADE 2 entry gate (`threshold_type = "mid"` without a MID): that abort
carries condition class `"pmatools_threshold_gate"` and is re-raised unchanged,
so a batch run cannot be used to get around the gate.

### Export layout

`export_bundle()` on a `pmatools_set` writes the hierarchical layout — set-level
files at the top, one directory per outcome below:

```
cbti_multi.zip
├── summary_of_findings.docx      rows in set$order
├── summary_of_findings.csv       the same table as plain text
├── evidence_profile.docx         one profile per outcome
├── analysis.R                    multi-outcome reproducibility script
├── data_long.csv                 every outcome
├── README.txt                    outcome order and per-outcome analysis sets
└── outcomes/
    ├── 01_insomnia_remission/
    │   ├── forest_plot.pdf / .png          the analysis actually rated
    │   ├── forest_plot_full.pdf / .png     only after a low-RoB refit
    │   ├── forest_plot_rob.pdf / .png      only when RoB labels are known
    │   ├── funnel_plot.pdf / .png
    │   ├── results.txt
    │   ├── data_long.csv                   this outcome only
    │   ├── evidence_profile.docx
    │   └── indirectness_table.docx         only when subdomains were recorded
    └── 02_depression_response/
        └── ...
```

Directory names carry the set order as a zero-padded numeric prefix, so
`reorder_outcomes()` renumbers them. A non-ASCII outcome name falls back to
`outcome_NN` so the ZIP stays portable. The bundled `analysis.R` re-issues the
`run_ma_multi()` / `grade_meta_multi()` / `reorder_outcomes()` / `set_primary()`
calls with the arguments actually used, and is syntax-checked before it is
written.

The **single-outcome** bundle (`export_bundle(ma, g, ...)`) still writes the
original flat layout; only a `pmatools_set` triggers the hierarchical one.

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

## Rare-event meta-analysis

For binary outcomes with very low event rates, inverse-variance pooling with
continuity corrections is biased. `rare_event_diagnostics()` flags when the
rare-event flow should apply; `run_rare_ma()` fits a suite of rare-event
methods (per Efthimiou 2018, Evid Based Ment Health; Tsujimoto 2024, Res
Synth Methods) and reports them side by side for sensitivity analysis.

```r
d    <- ingest_data("events_long.csv", format = "long")
diag <- rare_event_diagnostics(d)    # $rare_flow, event rates, zero-cell counts

rare <- run_rare_ma(d, effect_scale = "OR")
rare$primary                         # primary result as a regular meta object
rare$method_table                    # all methods: estimate, CI, zero-cell handling
plot_rare_sensitivity_forest(rare)   # method-comparison forest

g <- grade_meta(rare$primary, ...)                 # certainty rating proceeds as usual
export_bundle(rare$primary, g, rare = rare, ...)   # adds rare-event diagnostics to ZIP
```

The default primary on the OR scale is `BB_CR` (beta-binomial with correlated
responses, via {mmeta}), falling back to `MH_no_cc` (Mantel-Haenszel without
continuity correction) when {mmeta} is unavailable or fails to converge.
Prespecify a different primary with `primary_method = "MH_no_cc"` (or any
other method id in `method_table`).

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

> Abridged — [SPEC.md §4.5](SPEC.md) is authoritative for the full signature and
> `?grade_meta` for every argument.

```r
grade_meta(
  meta_obj,
  study_design  = "RCT",           # "RCT" | "obs"

  ## Entry gate and rating target (Core GRADE 2, v0.5)
  threshold_type    = "mid",       # "mid" (requires a threshold) | "null"
  threshold         = NULL,        # minimal important difference
  threshold_scale   = "auto",      # "auto" | "ratio" | "ard" | "te_scale"
                                   #   "auto" reads the scale off meta_obj$sm:
                                   #   ratio for OR/RR/HR/RoM, raw units for MD,
                                   #   standardized units for SMD
  threshold_baseline = NULL,       # baseline risk when threshold_scale = "ard"
  require_threshold = TRUE,        # FALSE restores the pre-0.5 MID-free behaviour
  rating_target     = NULL,        # manual override; needs rating_target_rationale
  rating_target_rationale = NULL,

  ## Risk of Bias
  rob           = NULL,            # scalar | vector length k | column name | NULL
  rob_rationale          = NULL,   # required when `rob` is a scalar override
  rob_some_concerns      = "low",  # fold "some concerns" into "low" | "high"
  rob_overrides          = NULL,   # named chr, keyed on studlab
  rob_override_rationale = NULL,   # named chr, same keys
  rob_dominant_threshold = 0.55,   # weight share at/above which evidence is "dominated"
  rob_refit              = TRUE,   # refit on low-RoB studies when Fig 2 says so
  rob_inflation_threshold = 0.10,  # relative inflation feeding the direction check
  small_values  = NULL,            # "undesirable" | "desirable" | NULL (conservative)

  ## Indirectness
  indirectness  = NULL,            # scalar | vector | column name; NULL = no concern
  indirectness_dominant_threshold = 0.55,  # weight share for vector/column aggregation
  indirectness_rationale  = NULL,  # required for a scalar override
  indirectness_subdomains = NULL,  # PICO data.frame (Core GRADE 5, v0.5)

  ## Inconsistency
  inconsistency = NULL,            # scalar override (skips flowchart)
  inconsistency_rationale          = NULL,  # required for a scalar override
  inconsistency_ci_diff            = NULL,  # "yes" | "no" | NULL (auto-detect)
  inconsistency_threshold_side     = NULL,  # "majority_one_side" | "opposite_sides"
  inconsistency_subgroup_explained = NULL,  # "yes" | "no"

  ## Imprecision
  imprecision   = NULL,            # scalar override (bypasses the Fig 4 flowchart)
  imprecision_rationale = NULL,    # required for a scalar override
  outcome_type  = "relative",      # "relative" (OR/RR/HR) | "absolute" (MD/SMD)
  ois_events    = NULL,            # binary: target events (direct)
  ois_n         = NULL,            # continuous: target N (direct)
  ois_alpha     = 0.05,            # type I error
  ois_beta      = 0.20,            # type II error (1 − power)
  ois_p0        = NULL,            # control event rate (binary)
  ois_p1        = NULL,            # experimental event rate (binary); wins over ois_rrr
  ois_rrr       = 0.20,            # binary: modest RRR the OIS is powered for (Core GRADE 2)
  ois_delta     = NULL,            # Threshold (continuous)
  ois_sd        = NULL,            # pooled SD (continuous)

  ## Event rate columns
  baseline_risk = NULL,            # numeric | "simple" | "metaprop" | NULL

  ## Publication Bias
  pubias_small_industry   = NULL,  # "yes" | "no"
  pubias_funnel_asymmetry = NULL,  # "yes" | "no" | NULL (auto Egger, k ≥ 10)
  pubias_unpublished      = NULL,  # "yes" | "no" | NULL (k < 10)
  pubias_registry_complete = NULL, # "yes": user-asserted rule-out, applied after Q1
  pubias_rationale         = NULL, # required for a scalar override

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
| `$meta` | the analysis every domain was assessed on — the **refitted** one after a low-RoB refit |
| `$meta_full` | the all-studies analysis (v0.5) |
| `$rob_analysis_set` | `"all"` or `"low_only"` (v0.5) |
| `$rob_refit` | whether the refit actually happened (v0.5) |
| `$rating_target` | `"important_effect"` / `"little_to_no_difference"` / `"non_null_effect"` (v0.5) |
| `$rating_target_note`, `$rating_target_auto`, `$threshold_type` | how the target was arrived at (v0.5) |
| `$indirectness_subdomains` | normalized PICO subdomain table, when supplied (v0.5) |

### `sof_table(x, style, palette, per, prediction, follow_up, unit, ...)`

Generates a single-outcome flextable. `auto = TRUE` rows are computed by `pmatools`.
`style = "bmj"` switches to the BMJ Core GRADE layout; `follow_up` / `unit` feed it.

### `grade_table(outcomes, primary, style, palette, show_domains, per, prediction, follow_up, unit, ...)`

Generates a multi-outcome flextable with optional primary/secondary grouping.
`outcomes` is a named list of `pmatools` objects **or** a `pmatools_set`.

### `grade_report(outcomes, primary, palette, style, format, output_dir, output_file, title, show_domains, per, prediction, ...)`

Exports a full certainty appendix (Core GRADE series) in docx / html / pdf / md
format. `outcomes` accepts a `pmatools_set` too.

### Exported functions at a glance

All 25 exports of `NAMESPACE`, one line each. Details are in `?function_name`
and in [SPEC.md §4](SPEC.md).

| Function | What it does |
|---|---|
| `ingest_data()` | Read long or wide study data (file, data frame or clipboard), auto-detect the format, apply column-name mapping, return the canonical long tibble |
| `run_ma()` | Fit one pairwise meta-analysis via `{meta}` (binary or continuous); aborts on data holding more than one outcome |
| `run_ma_multi()` | Split long data on its `outcome` column and run one `run_ma()` per outcome |
| `run_rare_ma()` | Fit a suite of rare-event methods side by side (beta-binomial is the default primary on the OR scale) |
| `rare_event_diagnostics()` | Report event rates, zero cells and whether the rare-event flow should be used |
| `grade_meta()` | Rate certainty for one outcome across the five Core GRADE domains; returns a `pmatools` object |
| `grade_meta_multi()` | Run `grade_meta()` per outcome from `common` + `per_outcome` arguments; returns a `pmatools_set` |
| `reorder_outcomes()` | Set the display order of a `pmatools_set` (table rows and export directory numbers) |
| `set_primary()` | Set (or clear) the primary outcomes of a `pmatools_set` |
| `sof_table()` | Single-outcome Summary of Findings flextable (GRADEpro or BMJ style) |
| `grade_table()` | Multi-outcome Summary of Findings flextable, with primary/secondary grouping |
| `evidence_profile()` | Single-outcome Evidence Profile flextable with per-domain footnotes |
| `indirectness_table()` | Core GRADE 5 PICO subdomain judgment table as a flextable |
| `grade_report()` | Full certainty appendix in docx / html / pdf / md |
| `export_bundle()` | Reproducible ZIP: data, `analysis.R`, results, plots and tables (S3 generic; flat layout for one outcome, `outcomes/NN_name/` for a `pmatools_set`) |
| `plot_forest()` | Forest plot with automatic layout and log/linear axis selection |
| `plot_forest_rob()` | Forest plot stratified by risk-of-bias level |
| `plot_forest_indirectness()` | Forest plot stratified by indirectness level |
| `plot_forest_pubias_subgroup()` | Two-subgroup forest plot of studies with available vs missing results (reference diagnostic; does not drive the judgment) |
| `plot_trimfill_forest()` | Forest plot of the trim-and-fill imputed analysis |
| `plot_rare_sensitivity_forest()` | Method-comparison forest for a `run_rare_ma()` result |
| `plot_funnel()` | Contour-enhanced funnel plot, annotated with Egger's test when k ≥ 10 |
| `suggest_threshold()` | Suggest a conventional minimal important difference for the effect measure in hand (ratio and absolute scales) |
| `chinn_smd_to_or()` | Chinn's conversion of an SMD (and its CI) to an odds ratio |
| `compute_pooled_sd()` | Sample-size-weighted pooled within-study SD across a `metacont` analysis, for deriving a continuous OIS or converting an MD threshold |

---

## File structure

```
pmatools/
├── pmatools.Rproj             ← open in RStudio (gitignored)
├── .gitignore
├── DESCRIPTION
├── NAMESPACE
├── LICENSE                  ← CC BY 4.0
├── PLAN.md                  ← implementation status + roadmap
├── SPEC.md                  ← authoritative specification
├── NEWS.md                  ← per-release change list
├── README.md
├── sample.R                 ← worked example; run line-by-line in RStudio
├── R/
│   ├── utils.R                 # constants, threshold + baseline_risk helpers
│   ├── data_ingest.R           # long/wide ingestion, aliases, `outcome` column
│   ├── run_ma.R                # {meta} wrapper
│   ├── multi_outcome.R         # run_ma_multi / grade_meta_multi / pmatools_set
│   ├── rare_events.R           # rare-event method suite + diagnostics
│   ├── domain_rob.R            # Risk of Bias Fig 2 flowchart + low-RoB refit
│   ├── domain_indirectness.R   # domain judgment + PICO subdomains
│   ├── domain_inconsistency.R  # auto-detect + manual flowchart
│   ├── domain_imprecision.R    # Fig 4 flowchart + OIS
│   ├── domain_pubias.R         # Egger's test + Fig 5 flowchart
│   ├── rating_target.R         # Core GRADE 2 entry gate + rating target
│   ├── grade_meta.R            # main function + print/summary
│   ├── sof_table.R             # single-outcome flextable (GRADEpro layout)
│   ├── sof_bmj.R               # BMJ Core GRADE SoF layout
│   ├── plain_language.R        # Core GRADE 2 Table 1 statements
│   ├── grade_table.R           # multi-outcome flextable
│   ├── evidence_profile.R      # evidence-profile flextable
│   ├── indirectness_table.R    # Core GRADE 5 subdomain table
│   ├── grade_report.R          # Appendix report
│   ├── plot_*.R                # forest / funnel / stratified / trim-fill plots
│   ├── export_bundle.R         # single-outcome ZIP
│   ├── export_bundle_multi.R   # pmatools_set ZIP (outcomes/NN_name/)
│   └── data.R                  # cbti_depression dataset documentation
├── inst/
│   ├── extdata/cbti_depression.csv       # bundled sample data (synthetic)
│   └── templates/                        # analysis.R templates (single + multi)
├── data-raw/
│   └── cbti_depression.R    # script to generate data/*.rda
├── tests/testthat/          # 23 test files (936 tests as of v0.5.0)
└── outputs/                 # generated output (gitignored)
```

---

## Dependencies

`DESCRIPTION` is authoritative; this table is the short version.

| Package | Role |
|---------|------|
| meta | meta object; `metabias()` for Egger's test |
| flextable | SoF table rendering and Word export |
| officer | Word (docx) report generation |
| tibble, dplyr | Data manipulation |
| rlang | Error/warning handling |
| glue | `analysis.R` template rendering |
| zip | Export bundle archives |
| rmarkdown | html/pdf report generation (Suggests) |
| BiasedUrn, metafor, mmeta | Rare-event methods (Suggests) |
| readxl, DT | Excel / clipboard ingestion and Shiny consumers (Suggests) |

---

## Limitations and future work

1. **GRADE upgrade criteria** (large effect, dose-response, plausible confounding) are not implemented. Certainty can only be rated down; use `evidence_profile(other_text =, other_downgrade =)` to record any other consideration by hand.
2. **GRADEpro GDT integration** (JSON import/export) planned.
3. **No internationalization.** Every table, report and label is English only; there is no language argument anywhere in the API.
4. **Pairwise only.** Network meta-analysis is out of scope.

See [PLAN.md](PLAN.md) for the roadmap and [SPEC.md](SPEC.md) §11 for the full
out-of-scope list.

---

## License

[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) — Yuki Furukawa, 2025.

---

## References

- Guyatt G, et al. Why Core GRADE is needed. BMJ 2025;389:bmj-2024-081902.
- Guyatt G, et al. Core GRADE 1: Overview. BMJ 2025. PMID: 40262844.
- Guyatt G, et al. Core GRADE 2: Choosing the target of certainty rating and assessing imprecision. BMJ 2025;389:e081904. doi:10.1136/bmj-2024-081904.
- Guyatt G, et al. Core GRADE 3: Inconsistency. BMJ 2025. PMID: 40328467.
- Guyatt G, et al. Core GRADE 4: Risk of bias, publication bias. BMJ 2025. PMID: 40360206.
- Guyatt G, et al. Core GRADE 5: Indirectness. BMJ 2025. PMID: 40393729.
- Furukawa Y, et al. CBT-I for MDD with comorbid insomnia. J Affect Disord. 2024;367:359-366.
- Higgins JPT, et al. Measuring inconsistency in meta-analyses. BMJ 2003;327:557.
- Sterne JAC, et al. Recommendations for examining funnel plot asymmetry. BMJ 2011;343:d4002.
