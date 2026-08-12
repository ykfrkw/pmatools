# pmatools — Package Specification

> Authoritative specification for the pmatools R package. Implementation MUST conform to this document. UI-side concerns (Shiny wizard, educational copy, accordion layout) are specified separately in `~/Developer/pairwise_meta_analysis/SPEC.md`.

**Version target:** 0.5.0
**Document history:** this file was written for v0.2.0 and has been updated in place for v0.5.0; section numbering is preserved so the diff stays readable. Sections that still describe v0.2 behaviour verbatim are marked **[v0.2 — superseded]** with a pointer to the section that governs.

**Backward compatibility:** v0.2 introduced no breaking changes. v0.4.0 and v0.5.0 did — see `NEWS.md`, which is authoritative for the change list. The three that most affect callers:

1. `grade_meta()` requires a rationale for every manual domain-judgment override (v0.4.0).
2. `grade_meta()` requires a clinical decision Threshold (MID) unless `threshold_type = "null"` or `require_threshold = FALSE` (v0.5.0, Core GRADE 2 entry gate).
3. `export_bundle()` is an S3 generic whose first argument is `x`, not `ma` (v0.5.0). Legacy `ma =` named calls still work with a deprecation warning; see §4.8.

---

## 1. Scope

pmatools is the **single source of truth** for:

1. Pairwise meta-analysis pipeline (data ingestion → `{meta}` MA → forest/funnel)
2. GRADE certainty assessment (5 downgrade domains, BMJ 2025 Core GRADE series)
3. Summary of Findings (SoF) flextable + Appendix report
4. Reproducible export bundle (CSV + R script + plots + tables → ZIP)

It is **Shiny-agnostic**. All functions return plain R objects (tibbles, base graphics, flextables, file paths). The Shiny wizard at `pairwise_meta_analysis` consumes pmatools functions only.

---

## 2. Installation

```r
# CRAN は対象外。GitHub のみ。
remotes::install_github("ykfrkw/pmatools")
library(pmatools)
```

Dependencies (declared in `DESCRIPTION`):

```
Depends: R (>= 4.1.0)
Imports:
  meta (>= 6.0),
  flextable (>= 0.9.0),
  officer (>= 0.6.0),
  tibble,
  dplyr,
  rlang,
  glue,
  zip
Suggests:
  BiasedUrn,
  metafor,
  mmeta,
  rmarkdown,
  testthat (>= 3.0.0),
  readxl,
  DT,
  here
```

`DT` and `readxl` are Suggests because they are used only by `ingest_data()` for Excel/clipboard paths and by Shiny consumers. `BiasedUrn`, `metafor` and `mmeta` are used only by the rare-event methods (§4.12).

No new hard dependency may be added without updating this section.

---

## 3. Data formats

### 3.1 Long format (canonical)

One row per **study × arm**. Required columns:

| Column | Type | Description |
|---|---|---|
| `studlab` | chr | Study label (must appear exactly twice per study, once per arm) |
| `treat` | chr | Arm label. By convention: experimental rows have `treat == experimental_label`, control rows have `treat == control_label` |
| `n` | int | Sample size in arm |

**Binary outcomes (additional):**

| Column | Type | Description |
|---|---|---|
| `event` | int | Number of events in arm |

**Continuous outcomes (additional):**

| Column | Type | Description |
|---|---|---|
| `mean` | num | Arithmetic mean of outcome in arm |
| `sd` | num | Standard deviation of outcome in arm |

**Optional columns (at study or arm granularity):**

| Column | Type | Description |
|---|---|---|
| `outcome` | chr | Outcome label. Per study × outcome. When present, the unit of a row is **study × outcome × arm** and `studlab` must appear twice *per outcome*. Required by `run_ma_multi()` (§4.10). |
| `rob` | chr | Risk of bias label (Cochrane RoB 2.0 or GRADE level). Per-study. |
| `indirectness` | chr | Indirectness label (No / Some / Serious / Very serious). Per-study. |
| `subgroup` | chr / fct | Subgroup variable. Per-study. |

**`outcome` column semantics.** `ingest_data()` recognises `outcome` and validates arm pairing within each (`studlab`, `outcome`) pair rather than within `studlab` alone; its diagnostics name the unit accordingly. `run_ma()` still **aborts** on data holding more than one outcome — `run_ma_multi()` splits on this column and is the only supported way to batch (§4.10). Data without an `outcome` column behaves exactly as before.

### 3.2 Wide format

One row per **study**. Required columns:

| Column | Type | Description |
|---|---|---|
| `studlab` | chr | Study label (unique per row) |
| `n_e`, `n_c` | int | Sample sizes (experimental, control) |

**Binary outcomes (additional):**

| Column | Type | Description |
|---|---|---|
| `event_e`, `event_c` | int | Events in experimental and control arms |

**Continuous outcomes (additional):**

| Column | Type | Description |
|---|---|---|
| `mean_e`, `mean_c` | num | Means |
| `sd_e`, `sd_c` | num | Standard deviations |

Optional columns (`rob`, `indirectness`, `subgroup`) are per-study (single column). An `outcome` column is accepted here too, but the multi-outcome workflow (§4.10) consumes the canonical **long** tibble that `ingest_data()` returns.

### 3.3 Format auto-detection

`ingest_data(format = "auto")` chooses based on column presence:

```r
if (all(c("event_e", "event_c") %in% names(df)) ||
    all(c("mean_e", "mean_c") %in% names(df))) {
  format <- "wide"
} else if ("studlab" %in% names(df) &&
           any(duplicated(df$studlab))) {
  format <- "long"
} else {
  abort("Could not detect format; specify format = 'long' or 'wide' explicitly.")
}
```

### 3.4 Column-name mapping

Users with non-standard column names supply `mapping`:

```r
ingest_data(df, format = "long", mapping = list(
  studlab = "study_id",
  treat   = "arm",
  n       = "sample_size",
  event   = "responders",
  rob     = "rob_overall"
))
```

After mapping, the data is normalized to canonical column names.

---

## 4. Public API

### 4.1 `ingest_data()` [new]

```r
ingest_data(
  data,
  format  = c("auto", "long", "wide"),
  mapping = NULL,
  experimental_label = NULL,    # if NULL, inferred from first non-control treat
  control_label      = NULL     # if NULL, inferred
) -> tibble
```

**Behavior:**

- Accepts: `data.frame`, `tibble`, character path to `.csv`/`.xlsx`/`.tsv`, multi-line character string (clipboard paste).
- Detects delimiter (tab, comma, semicolon) for character input.
- For `.xlsx` requires `readxl` (Suggests). Errors with informative message if missing.
- Returns canonical long-format tibble.
- Validates: each `studlab` appears exactly twice in long output; required columns present; `n` and `event` are non-negative integers; `sd` non-negative.
- Warnings (not errors): missing `rob` / `indirectness` / `subgroup` columns.

**Errors:**

- Unrecognized file extension
- Required columns missing after mapping
- Non-numeric values where numeric expected
- A `studlab` that appears !=2 times in long output (after wide→long conversion)

### 4.2 `run_ma()` [new]

```r
run_ma(
  data,                                          # output of ingest_data()
  outcome_type = c("binary", "continuous"),
  sm           = NULL,                            # NULL → "OR" if binary, "SMD" if continuous
  method       = NULL,                            # NULL → "Inverse" if binary, irrelevant if continuous
  method.tau   = c("REML", "DL"),
  random       = TRUE,
  common       = FALSE,
  hakn         = NULL,                            # NULL → TRUE if k>=3
  prediction   = NULL,                            # NULL → TRUE if k>=3
  incr         = 0.5,
  subgroup     = NULL,                            # column name in `data` or NULL
  experimental_label = NULL,
  control_label      = NULL
) -> meta object (class "meta", as returned by metabin/metacont)
```

**Allowed `sm` values:**

| outcome_type | sm |
|---|---|
| binary | "OR", "RR" |
| continuous | "SMD", "MD", "RoM" |

**Allowed `method` (binary only):**

| sm | method |
|---|---|
| OR | "Inverse", "MH", "Peto" |
| RR | "Inverse", "MH" |

**Implementation:**

- Pivot canonical long → wide internally for `metabin/metacont` (which take wide input via `event.e/event.c/n.e/n.c` etc.).
- Pass `prediction = TRUE` to `metabin/metacont` when `hakn = TRUE` (required by `{meta}`).
- If `subgroup` not NULL, pass `subgroup = data[[subgroup]]` to `{meta}`.

**Returns:** the raw `meta` object so existing pmatools functions (`grade_meta`, `sof_table`) work unchanged.

### 4.3 `plot_forest()` [new]

```r
plot_forest(
  meta_obj,
  title       = NULL,
  label_e     = NULL,                  # NULL → meta_obj$label.e
  label_c     = NULL,                  # NULL → meta_obj$label.c
  xlim        = NULL,                  # NULL → auto
  prediction  = TRUE,
  auto_layout = TRUE,
  ...                                   # passed to meta::forest()
) -> invisible(NULL)
# Side effect: draws on the active graphics device.
```

**`auto_layout = TRUE` behavior:**

- `par(mar = c(4, 4, 2 + ceiling(k/8), 4))` for top margin growing with k.
- For binary `sm` (OR/RR/RoM): log-scale x-axis; `xlim` from `quantile(c(meta_obj$lower, meta_obj$upper), c(0.01, 0.99))` clamped to `c(0.01, 100)`.
- For continuous (MD/SMD): linear-scale x-axis; `xlim` from `quantile(..., c(0.01, 0.99))` clamped to ±5.
- If `meta::forest()` returns coordinates and the heterogeneity row overlaps a diamond/PI row, reduce `fontsize` by 10% (max 2 reductions).
- Long study labels (>30 chars) get `cex.lab = 0.85`; otherwise default.
- Pass `colgap.left = unit(2, "mm")`, `colgap.right = unit(2, "mm")` for tighter columns.

`auto_layout = FALSE` behaves like a thin wrapper around `meta::forest()` with no overrides.

Since v0.4 the signature also carries display arguments used by the export bundle: `threshold_lines`, `show_n`, `show_events`, `favors_left`, `favors_right`, `addrow_above`, `addrow_below`. `addrow_below = NULL` (the default) derives the bottom spacing from the drawn content so the heterogeneity text cannot overlap the x-axis band.

### 4.4 `plot_funnel()` [new]

```r
plot_funnel(
  meta_obj,
  contour     = c(0.9, 0.95, 0.99),
  show_egger  = TRUE,
  auto_layout = TRUE,
  ...                                   # passed to meta::funnel()
) -> invisible(NULL)
```

**`show_egger = TRUE` behavior:**

```r
k <- meta_obj$k
if (k >= 10) {
  res <- tryCatch(
    meta::metabias(meta_obj, method.bias = "linreg"),
    error = function(e) NULL
  )
  if (!is.null(res)) {
    mtext(sprintf("Egger's test: t = %.2f, df = %d, p = %.3f",
                  res$statistic, res$parameter, res$p.value),
          side = 3, line = 0.3, cex = 0.85)
  } else {
    mtext("Egger's test failed to run", side = 3, line = 0.3,
          cex = 0.85, col = "grey40")
  }
} else {
  mtext("Egger's test not run (k < 10)", side = 3, line = 0.3,
        cex = 0.85, col = "grey40")
}
```

`auto_layout = TRUE` sets `par(mar = c(4, 4, 3, 4))` to make room for the Egger annotation in the top margin.

### 4.5 `grade_meta()`

**Full signature as of v0.5.0** (this is the authoritative list; every argument below exists):

```r
grade_meta(
  meta_obj,
  study_design                     = c("RCT", "obs"),

  # --- Risk of bias (Core GRADE 4 Fig 2; §5.1) ---
  rob                              = NULL,   # per-study vector, or scalar override
  rob_rationale                    = NULL,   # REQUIRED with a scalar `rob`
  rob_some_concerns                = c("low", "high"),  # which side "some concerns" folds into
  rob_overrides                    = NULL,   # named chr vector keyed on studlab
  rob_override_rationale           = NULL,   # named chr vector, one per override (REQUIRED)
  rob_dominant_threshold           = 0.55,   # weight share for the Fig 2 dominance gate (`>=`)
  rob_refit                        = TRUE,   # refit on the low-RoB subset when Fig 2 says so
  rob_inflation_threshold          = 0.10,   # minimum relative inflation to act on

  small_values                     = NULL,   # "desirable" / "undesirable" / NULL

  # --- Indirectness (Core GRADE 5; §4.5.3) ---
  indirectness                     = NULL,   # scalar judgment, or override of the subdomain table
  indirectness_dominant_threshold  = 0.55,   # weight share for per-study aggregation (pmatools convention)
  indirectness_rationale           = NULL,   # REQUIRED with a scalar override other than "no"
  indirectness_subdomains          = NULL,   # PICO data.frame

  # --- Inconsistency (Core GRADE 3; §5.2) ---
  inconsistency                    = NULL,
  inconsistency_rationale          = NULL,   # REQUIRED with a scalar `inconsistency`
  inconsistency_ci_diff            = NULL,
  inconsistency_threshold_side     = NULL,
  inconsistency_subgroup_explained = NULL,

  # --- Imprecision (Core GRADE 2 Fig 4; §5.5) ---
  imprecision                      = NULL,
  imprecision_rationale            = NULL,   # REQUIRED with a scalar `imprecision`

  # --- Threshold / rating target (Core GRADE 2; §4.5.1, §4.5.2) ---
  threshold_type                   = c("mid", "null"),
  threshold                        = NULL,
  threshold_scale                  = "auto",
  threshold_baseline               = NULL,
  rating_target                    = NULL,
  rating_target_rationale          = NULL,   # REQUIRED with a manual `rating_target`
  require_threshold                = TRUE,

  outcome_name                     = NULL,
  outcome_type                     = c("relative", "absolute"),

  # --- Optimal Information Size ---
  ois_events = NULL, ois_n = NULL, ois_alpha = 0.05, ois_beta = 0.20,
  ois_p0 = NULL, ois_p1 = NULL, ois_rrr = 0.20, ois_delta = NULL, ois_sd = NULL,

  baseline_risk                    = NULL,

  # --- Publication bias (Core GRADE 4) ---
  pubias_small_industry            = NULL,
  pubias_funnel_asymmetry          = NULL,
  pubias_rationale                 = NULL,   # REQUIRED with a scalar pubias_funnel_asymmetry
  pubias_unpublished               = NULL,
  pubias_registry_complete         = NULL
) -> S3 "pmatools" object
```

`threshold_scale` values:

| Value | Meaning |
|---|---|
| `"auto"` | infer from `meta_obj$sm` (recommended) |
| `"te_scale"` | already on the `meta_obj$TE` scale (log for OR/RR/HR/RoM, raw for MD/SMD) |
| `"ratio"` | user gave an OR/RR ratio (e.g. 1.25); internally `log(threshold)` |
| `"ard"` | absolute risk difference; converted to the ratio scale at `threshold_baseline` (or the pooled baseline risk) for OR/RR/HR/RoM |

**Mandatory rationales (v0.4.0, breaking).** Supplying a scalar `rob`, an `indirectness` other than `"no"`, an `inconsistency`, an `imprecision`, a `pubias_funnel_asymmetry`, a manual `rating_target`, or any `rob_overrides` **without** the matching `*_rationale` argument is an error. Rationales are stored in the domain notes and surfaced by `sof_table()`, `grade_report()` and `export_bundle()`.

**Return value additions (v0.5.0).** Beyond the v0.2 fields, the `pmatools` object carries:

| Field | Contents |
|---|---|
| `$meta` | the analysis every domain was assessed on — **the low-RoB refit when one happened** |
| `$meta_full` | the all-studies analysis |
| `$rob_analysis_set` | `"all"` or `"low_only"` |
| `$rob_refit` | logical; whether a refit actually took place |
| `$threshold_type` | `"mid"` or `"null"` |
| `$rating_target` | `"important_effect"` / `"little_to_no_difference"` / `"non_null_effect"` |
| `$rating_target_note`, `$rating_target_auto` | derivation note and whether it was derived rather than supplied |
| `$indirectness_subdomains` | the normalised PICO table, or NULL |

Downstream consumers MUST read pooled numbers from `$meta`, not `$meta_full`, so a refit propagates.

#### 4.5.1 Entry gate: `threshold_type` (Core GRADE 2, v0.5.0 — breaking)

`threshold_type` decides what the certainty rating is *about*, and it is checked before any domain is assessed:

- `threshold_type = "mid"` (default) — certainty in whether the effect crosses a **minimal important difference**. A `threshold` is then **mandatory**: a call without one aborts, and the error message quotes the value `suggest_threshold()` recommends for that `sm`. The abort carries condition class `"pmatools_threshold_gate"`.
- `threshold_type = "null"` — certainty in a **non-null effect**. No MID needed.
- `require_threshold = FALSE` — escape hatch restoring the pre-v0.5.0 MID-free behaviour.

`grade_meta_multi()` re-raises the gate abort unchanged rather than recording the outcome as failed, so a batch run cannot be used to get around the gate.

#### 4.5.2 Rating target (Core GRADE 2 Fig 2)

`grade_meta()` derives the target of the rating from the pooled point estimate and `threshold_type`:

| Derived target | Threshold Imprecision evaluates the CI against |
|---|---|
| `"important_effect"` | ±MID |
| `"little_to_no_difference"` | ±MID |
| `"non_null_effect"` | null (0 on the TE scale) |

Supplying `rating_target` manually overrides the derivation and requires `rating_target_rationale`. `print()` shows the target. Objects created before v0.5.0 have no `$rating_target`; consumers must tolerate its absence (the plain language column is simply omitted).

#### 4.5.3 Indirectness subdomains (per-PICO; Core GRADE 5 reasoning, pmatools scale)

`indirectness_subdomains` is a data.frame with columns `subdomain`, `target`, `evidence`, `judgment`, one row per PICO element (Population / Intervention / Comparison / Outcome). Asking the question per element is Core GRADE 5's; **the 4-point scale below and the question wording "Is the evidence sufficiently direct?" are pmatools conventions** and do not appear in the article body, which instead grades the *likelihood* of rating down per element (Table 2: Low / Intermediate / Substantial / High likelihood). `judgment` uses the 4-point scale:

| Judgment | Levels down |
|---|---|
| `"yes"` | 0 |
| `"probably_yes"` | 0 |
| `"probably_no"` | 1 |
| `"no"` | 2 |

Aliases such as `"Probably No"` are normalised. The domain judgment defaults to the **worst case across subdomains**; a scalar `indirectness` still overrides it, and then requires `indirectness_rationale`. The normalised table is returned as `$indirectness_subdomains`; `domain_assessments` keeps its one-row-per-domain schema. `indirectness_table()` (§4.13) renders the table. The worst-case fold is symmetric across the four elements and therefore does not reproduce Core GRADE 5 Table 2's asymmetric likelihood gradient (Population lowest, Outcome highest); the rendered footer says so.

**`threshold` and `threshold_scale` interaction (auto-detection table):**

When `threshold_scale = "auto"`:

| `meta_obj$sm` | User input convention | Internal storage |
|---|---|---|
| `"OR"`, `"RR"`, `"HR"`, `"RoM"` | ratio scale (e.g., 1.25, 1.10) | `log(threshold)` (matches `meta_obj$TE`) |
| `"MD"` | raw outcome units (e.g., 3 PHQ-9 points) | `threshold` as-is |
| `"SMD"` | standardized units (e.g., 0.20) | `threshold` as-is |
| `"ARD"` | proportion (e.g., 0.05 = 5%) | `threshold` as-is |

`threshold_scale = "te_scale"` is the escape hatch for power users who want to specify directly on the log scale.

When `threshold` is supplied but auto-detection fails (unrecognized `sm`), the function aborts with a clear error.

**`rob_inflation_threshold` semantics:** the relative inflation

```r
inflation_ratio <- (abs(TE_all) - abs(TE_low)) / abs(TE_low)
```

is one input to the zone-based direction check, not a judgment on its own. It is evaluated only when the shift runs in the bias-favouring direction, and what it triggers depends on which branch of the Core GRADE 4 Fig 2 flowchart is active. **§5.1 is authoritative for the whole domain.**

**`threshold` semantics — Inconsistency (BMJ Core GRADE 3 flowchart):**

> v0.2 keeps the existing BMJ-faithful flowchart from v0.1.0. The only change is that **Step 2's clinical decision boundary uses Threshold** when it is supplied (instead of always using null = 0). Point estimates are classified into zones around ±Threshold; "majority on one side of the clinical Threshold" is interpreted accordingly. I², τ², and Q-test statistics remain supplementary context — they do not drive the judgment. PI is **not** used in the decision logic.

**The flowchart (BMJ Core GRADE 3, Fig 2):**

```
Step 1: Are there important differences in point estimates AND limited CI overlap?
  NO  → judgment = "no"  (do not rate down)
  YES → continue to Step 2

Step 2: Where do the point estimates fall relative to the clinical decision Threshold?
  Majority on one side of Threshold → judgment = "no"  (do not rate down)
  Substantial proportion on opposite sides → continue to Step 3

Step 3: Is the opposite-sided inconsistency explained by a credible subgroup analysis?
  YES → judgment = "no" + note "present subgroups separately"
  NO  → judgment = "serious"
```

The "clinical decision Threshold" in Step 2 is **null = 0 by default**, but **±Threshold** when `threshold` is supplied. This is the v0.2 enhancement.

**Three input paths (preserved from v0.1.0):**

**Path A — Scalar override:**

```r
grade_meta(m, inconsistency = "serious")
# → judgment = "serious", auto = FALSE
```

**Path B — Manual flowchart (full BMJ-faithful):**

```r
grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "majority_one_side"
)
# → judgment = "no"  (Step 2: majority on one side → do not rate down)

grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "opposite_sides",
  inconsistency_subgroup_explained = "no"
)
# → judgment = "serious"

grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "opposite_sides",
  inconsistency_subgroup_explained = "yes"
)
# → judgment = "no" + note "present subgroups separately"
```

These paths are **unchanged from v0.1.0**.

**Path C — Auto (no flowchart params supplied):**

The algorithm proxies each step from data. **This section was rewritten in v0.5.0 to match `R/domain_inconsistency.R`;** the previous text described a `≥ 0.75` cut-off and a `pct_one_side = (n_above + n_trivial)/k` formula that the code has not used since v0.5.0.

```
Step 1 surrogate:
  ci_diff_yes <- (I² > 30%)          # INCONSISTENCY_I2_CUT
  Core GRADE 3 gives 30% as its only number ("one will seldom see serious
  inconsistency with I2 values <30%") while warning that "the limitations of
  the statistic make such rules problematic". Its actual Step 1 is visual
  ("Core GRADE relies on the visual inspection of forest plots"), so this is
  an automation surrogate; every auto note says so.
  (v0.5.0: raised from 25%, which had no source.)

Step 2 surrogate (3-zone tally, identical shape with and without a threshold):

  M <- threshold_chosen if finite and > 0, else 0
       (threshold_chosen is the SAME value Imprecision rates against:
        ±MID for important-effect / little-to-no-difference targets,
        the null for a non-null-effect target)

  n_above   <- sum(TE > +M)
  n_below   <- sum(TE < -M)
  n_trivial <- k - n_above - n_below          # collapses to 0 when M == 0

  pct_max_zone  <- max(n_above, n_trivial, n_below) / k
  pct_each_side <- min(n_above, n_below) / k

  if (pct_max_zone  >= ZONE_MAJORITY)  → "majority_one_side"    → "no"
  else if (pct_each_side >= OPPOSITE_EACH) → "opposite_substantial" → "some_concerns"
  else                                  → "heterogeneous"       → "some_concerns"

  ZONE_MAJORITY = 0.80   # CINeMA (Nikolakopoulou 2020); Core GRADE 3 Fig 2
                         # says only "Majority are on one side of threshold"
  OPPOSITE_EACH = 0.20   # pmatools convention; Core GRADE 3's phrase is
                         # "substantial proportion", with no number

Step 3:
  Subgroup credibility cannot be auto-checked. Core GRADE 3 keys it to the
  interaction P value, within-study vs between-study comparison, and a small
  number of direction-specifying a priori hypotheses, assessed with ICEMAN
  (www.iceman.help; Schandelmaier 2020, CMAJ 2020;192:E901-6). Supply
  inconsistency_subgroup_explained = "yes" to take the credible-subgroup
  branch. NOTE: Core GRADE 3 says "a conclusion of moderate or high
  credibility warrants the creation of separate PICO questions for each
  subgroup", so the faithful response is to split the analysis, not to keep
  reporting the pooled estimate that this branch lets through.
```

**Auto judgment outputs:**

| Auto path outcome | Auto judgment | Manual flowchart equivalent |
|---|---|---|
| ci_diff_yes = FALSE | `"no"` | `"no"` (same) |
| ci_diff_yes & majority_one_side | `"no"` | `"no"` (same) |
| ci_diff_yes & opposite_substantial | `"some_concerns"` | `"some_concerns"` (same, modulo Step 3) |
| ci_diff_yes & heterogeneous | `"some_concerns"` | — (no manual counterpart) |

Every automated and flowchart path is capped at `"some_concerns"` (−1): Core GRADE 3 declines to endorse a two-level inconsistency downgrade. `"serious"` (−2) is reachable only through the scalar `inconsistency` override, which requires `inconsistency_rationale`.

**Notes content (all signals shown for transparency):**

```
{{#if path_C}}
AUTO Step 1: {{"I2 > 30% → important heterogeneity detected" | "No important heterogeneity (I2 <= 30%) → do not rate down"}}
  + the I² surrogate caveat (visual inspection; "hard and fast rules ... problematic")
AUTO Step 2 ({{threshold_label}}): zone counts (k = {{k}}): above_threshold = {{n_above}},
  trivial = {{n_trivial}}, below_threshold = {{n_below}}. {{decision_note}}
  + the zone-cut-off caveat (80% = CINeMA, 20% = pmatools convention)
{{#if opposite_substantial}}
  + "Supply inconsistency_subgroup_explained = 'yes' to override" + the ICEMAN caveat
  + the −1 cap note
{{/if}}
{{/if}}
| I2 = {{i2_pct}}%, tau2 = {{tau2}}, Q p = {{q_p}} (supplementary; not the primary criterion)
```

`{{threshold_label}}` is `"vs ±Threshold = ±{{threshold_internal}}"` when Threshold is supplied, otherwise `"vs null = 0"`.

**`threshold` semantics — Imprecision:**

> **[v0.2 — superseded]** As of v0.5.0 Imprecision follows the Core GRADE 2 Fig 4 flowchart, in which the Optimal Information Size is consulted **only** when the CI does not cross the chosen threshold *and* the effect is implausibly large. **§5.5 is authoritative.** What follows describes only how `threshold` seeds the OIS inputs when that branch is reached.

In `assess_imprecision()`, when no explicit `ois_*` is provided:

- **Binary (v0.5.0): `ois_p1 = ois_p0 * (1 - ois_rrr)`, default `ois_rrr = 0.20`.** The MID is *not* used. Core GRADE 2 (p6): "For binary outcomes, these involve specifying the acceptable error rates: α (typically 0.05) and β (typically 0.20), the control group event rate (chosen from the context), and **a modest relative risk reduction, typically 20% or 25%**." `ois_p0` still comes from the ARD baseline risk when `threshold_scale = "ard"`, otherwise from the pooled control-arm rate.
- Continuous (MD): `ois_delta = threshold_internal` (raw outcome units) — the same paragraph writes the continuous case out separately and *does* send it to the MID ("by specifying the smallest difference between intervention and control that one would want to avoid missing (ie, the MID)").
- Continuous (SMD): `ois_delta = threshold_internal × pooled_SD` *(see §5.4 for pooled_SD computation)*.

**Comparison unit (v0.5.0): participants, not events.** Core GRADE 2 Fig 4 caption: "N=number of participants; OIS=optimal information size"; body: "If the total sample size of all the studies included in a meta-analysis exceeds the OIS, one does not rate down". The auto-computed binary OIS is therefore a target **N** compared against `sum(n.e) + sum(n.c)`; the implied event count is reported in the notes for information. Supplying `ois_events` explicitly still selects an event-based comparison (backward compatible).

If both `threshold` and `ois_*` supplied, `ois_*` wins (`ois_p1` also wins over `ois_rrr`). Notes string indicates source.

`threshold` and `threshold_scale` are the **single source of truth** — RoB, Inconsistency, and Imprecision derive their boundary from them. Inconsistency and Imprecision additionally share the *chosen* threshold resolved by the rating target (Core GRADE 3 Fig 2: "Evaluate point estimates of studies **in relation to chosen threshold**"), so a `non_null_effect` target puts both domains on the null.

### 4.6 `sof_table()`

**Full signature as of v0.5.0:**

```r
sof_table(
  x,                                     # pmatools object
  style   = c("gradepro", "bmj"),         # v0.5.0
  palette = c("pastel", "classic"),
  per        = 1000,
  prediction = FALSE,
  follow_up  = NULL,                      # v0.5.0: time frame, BMJ style
  unit       = NULL,                      # v0.5.0: unit of a continuous difference
  convert_smd_to_or = FALSE,
  baseline_risk     = NULL,
  threshold_label   = NULL,
  chinn_invert      = FALSE,              # v0.4: flip SMD sign before Chinn
  label_intervention = "intervention",
  label_control      = "control",
  ...
) -> flextable
```

**`style = "gradepro"` (default).** The v0.1–v0.4 layout. Column headers were renamed in v0.4.0 (breaking for string matching): "Risk with &lt;control&gt;" / "Risk with &lt;intervention&gt;" replaced "Control rate" / "Exp. rate", and the certainty header reads "Certainty of the evidence (Core GRADE series)".

**`style = "bmj"` (v0.5.0).** The BMJ Core GRADE Summary of Findings layout: outcome and follow-up; participants with the study design spelled out; the relative effect with its measure spelled out; a spanning "Absolute effects (95% CI)" block holding control arm, intervention arm and a **Difference** column (e.g. "88 fewer per 1000 (129 fewer to 42 fewer)"); certainty annotated with the domains that pulled it down; and a plain language summary.

**Plain language summaries** are the **Core GRADE 6 Box 1** statements ("Writing standardised GRADE plain language summaries in summary of findings tables"), carried verbatim. Box 1 supersedes the earlier Core GRADE 2 Table 1 guidance, which it "summarises ... as well as additional guidance related to the null and MID thresholds that are the focus of Core GRADE"; unlike Table 1 it names the direction of the effect on the outcome instead of fixing the wording to "benefit". The statement is selected from **four** inputs: certainty level, `threshold_type`, `rating_target`, and the **sign of the pooled point estimate** (`increases` / `reduces`). An object without `$rating_target` (created before v0.5.0) omits the column rather than guessing, as does an object with no usable direction — Box 1 has no direction-free wording.

**Analysis-set footnote.** When the rated analysis is a low-RoB refit (§5.1), the table carries a footnote saying so. `grade_table()` numbers the marker per row, so a table mixing analysis sets says which rows were restricted.

When `convert_smd_to_or = TRUE`:

- Requires `x$outcome_type == "absolute"` and `x$meta$sm %in% c("SMD", "MD")` (MD must additionally have SD context, but spec accepts SMD as the canonical case).
- Requires `baseline_risk` numeric in `(0, 1)`.
- Computes:
  ```r
  factor      <- pi / sqrt(3)
  log_or      <- x$meta$TE.random       * factor
  log_or_lo   <- x$meta$lower.random    * factor
  log_or_hi   <- x$meta$upper.random    * factor
  or          <- exp(log_or)
  or_lo       <- exp(log_or_lo)
  or_hi       <- exp(log_or_hi)
  p_e         <- baseline_risk * or / (1 + baseline_risk * (or - 1))
  p_e_lo      <- baseline_risk * or_lo / (1 + baseline_risk * (or_lo - 1))
  p_e_hi      <- baseline_risk * or_hi / (1 + baseline_risk * (or_hi - 1))
  ```
- SoF table shows: Outcome, k, N, Risk with &lt;control&gt; (X per `per`), Risk with &lt;intervention&gt; (Y per `per`, [Y_lo; Y_hi]), Effect (SMD ...), Certainty.
- `chinn_invert = TRUE` flips the SMD sign before applying Chinn's formula, so a negative-is-better SMD yields OR > 1 in the dichotomised rate columns.
- Adds a footer note row: *"Continuous outcome dichotomized via Chinn's formula (log OR = SMD × π/√3). Control event rate user-specified{{; threshold: <threshold_label>}}."*

When `convert_smd_to_or = FALSE` (default), behavior is identical to v0.1.0.

### 4.7 `chinn_smd_to_or()` [new helper, exported]

```r
chinn_smd_to_or(
  smd,
  ci_lower = NULL,
  ci_upper = NULL
) -> list(or, or_lower, or_upper, factor)
```

`factor = pi / sqrt(3)`. NA propagation: any NA input yields NA output for that position.

### 4.7a `suggest_threshold()` [new helper, exported]

```r
suggest_threshold(meta_obj) -> list(threshold_user, threshold_scale, source,
                                    [threshold_absolute], [threshold_ratio]) | NULL
```

Returns a conventional default Threshold for the given `{meta}` object based on `meta_obj$sm`. See §5.4 for the table. Returns `NULL` when `sm` is unrecognized.

For `sm = "MD"`, calls `compute_pooled_sd()` internally and returns `0.20 * sd_pooled`.

### 4.7b `compute_pooled_sd()` [new helper, exported]

```r
compute_pooled_sd(meta_obj) -> numeric
```

Returns the sample-size-weighted pooled standard deviation across studies. Required input: `meta_obj` from `metacont()` with `sd.e` and `sd.c` available. Falls back to `weighted.mean(seTE * sqrt(n_total), n_total)` when arm-level SDs are missing.

### 4.8 `export_bundle()` — S3 generic

As of v0.5.0 `export_bundle()` is an **S3 generic** dispatching on its first argument, which is named `x`:

```r
export_bundle(x, ...)

# methods
export_bundle.meta(x, grade, ...)          # single-outcome flat layout  (§4.8.2)
export_bundle.pmatools(x, ...)             # convenience: x$meta + x     (§4.8.2)
export_bundle.pmatools_set(x, ...)         # multi-outcome layout        (§4.8.3)
export_bundle.default(x, ...)              # abort "must be a meta object"
```

`export_bundle.pmatools(g)` is the unambiguous single-argument form: the `pmatools` object knows which meta object it rated (the low-RoB refit, when one happened), so it passes `x$meta` on.

#### 4.8.1a Legacy `ma =` calls

Before v0.5.0 the first formal was named `ma`. Positional calls are unaffected. A **named** legacy call is intercepted by the generic itself, which reassigns `ma` to `x` and re-dispatches:

```r
export_bundle(ma = m, grade = g, output_dir = d)   # works; warns
```

The deprecation warning is raised with `rlang::warn(.frequency = "once", .frequency_id = "export_bundle_ma_arg")`, so it appears once per session. **Do not rely on this path in new code**; it will be removed. Callers outside this repository (notably the Shiny app) are the reason it exists.

#### 4.8.2 Single-outcome layout (`meta` / `pmatools` methods)

```r
export_bundle(
  x,                                      # meta object (from run_ma)
  grade,                                  # pmatools S3 object (from grade_meta)
  output_dir   = ".",
  bundle_name  = "pmatools_results",
  include      = c("data", "script", "results",
                   "forest", "forest_rob", "funnel", "funnel_trimfill",
                   "pubias_missing_forest", "grade_table"),
  style        = c("bmj", "gradepro"),     # v0.5.1; same default as the set method
  per          = 1000,
  prediction   = FALSE,
  follow_up    = NULL,                     # v0.5.1: time frame, BMJ style
  unit         = NULL,                     # v0.5.1: unit of a continuous difference
  convert_smd_to_or = FALSE,
  baseline_risk     = NULL,
  threshold_label   = NULL,
  chinn_invert      = FALSE,
  other_text        = NULL,               # "Other considerations" for evidence_profile()
  other_downgrade   = 0L,
  data              = NULL,               # canonical long tibble; else reconstructed
  grade_args        = NULL,               # origin-tracked specs (§4.8.1)
  ma_args           = NULL,
  forest_display    = NULL,               # named list passed to plot_forest()
  rob               = NULL,               # per-study labels; required for "forest_rob"
  forest_display_rob = NULL,
  rare               = NULL,              # pma_rare_meta from run_rare_ma()
  rare_forest_display = NULL,
  pubias_missing_df  = NULL,              # studies with unavailable results
  ...
) -> chr (path to .zip)
```

Note the shape change from v0.2: display arguments are passed as the named lists `forest_display` / `forest_display_rob`, not as `forest_args` / `funnel_args`.

**`style` (v0.5.1).** Forwarded to `sof_table()` for `sof_table.docx` and to `grade_report()` for the certainty appendix — one layout per ZIP — and rendered into the generated `analysis.R`, so re-running the script reproduces the layout that was exported rather than the `sof_table()` default. `follow_up` / `unit` are the BMJ layout's presentation arguments (§4.6); each falls back to the field of the same name on the rated object, which is where `grade_meta_multi()` stores it. The default is `"bmj"`, matching §4.8.3: **both bundle methods** default to the Core GRADE layout, while `sof_table()` and `grade_table()` themselves keep `"gradepro"`. Before v0.5.1 this method had no `style` and always wrote GRADEpro.

**ZIP contents — flat, no sub-directories** (only the requested `include` items appear):

```
{bundle_name}.zip
├── data_long.csv
├── analysis.R
├── results.txt
├── forest_plot.pdf / .png              (300 dpi, width = max(7, 3 + 0.3*k))
├── forest_plot_rob.pdf / .png          "forest_rob"; needs `rob`
├── funnel_plot.pdf / .png              (300 dpi)
├── funnel_trimfill.pdf / .png          "funnel_trimfill"
├── pubias_missing_forest.pdf / .png    "pubias_missing_forest"; rendered only when k >= 10
├── grade_table.docx                    SoF table (single outcome → 1 row)
├── indirectness_table.docx             when subdomain judgments were recorded
├── rare_event_diagnostics.csv          when `rare` is supplied
├── rare_event_method_table.csv         when `rare` is supplied
└── rare_event_method_forest.pdf / .png when `rare` is supplied
```

A renderer that fails warns and is skipped rather than aborting the whole bundle.

#### 4.8.3 Multi-outcome layout (`pmatools_set` method)

```r
export_bundle(
  x,                                      # pmatools_set from grade_meta_multi()
  output_dir  = ".",
  bundle_name = "pmatools_results",
  include     = c("data", "script", "results", "forest", "forest_full",
                  "forest_rob", "funnel", "sof", "evidence_profile",
                  "indirectness", "readme"),
  style       = c("bmj", "gradepro"),      # as of v0.5.1 the meta method matches this default
  per         = 1000,
  prediction  = FALSE,
  rob         = NULL,                      # named list by outcome, or one vector for all
  forest_display  = NULL,
  other_text      = NULL,
  other_downgrade = 0L,
  label_intervention = "intervention",
  label_control      = "control",
  ...
) -> chr (path to .zip)
```

**ZIP contents — hierarchical:**

```
{bundle_name}.zip
├── summary_of_findings.docx      rows in set$order
├── summary_of_findings.csv       the same table as plain text
├── evidence_profile.docx         one profile per outcome
├── analysis.R                    multi-outcome reproducibility script
├── data_long.csv                 every outcome
├── README.txt                    outcome order and per-outcome analysis sets
└── outcomes/
    ├── 01_<slug>/
    │   ├── forest_plot.pdf / .png          the analysis actually rated
    │   ├── forest_plot_full.pdf / .png     only when a low-RoB refit happened
    │   ├── forest_plot_rob.pdf / .png      only when RoB labels are known
    │   ├── funnel_plot.pdf / .png
    │   ├── results.txt
    │   ├── data_long.csv                   this outcome only
    │   ├── evidence_profile.docx
    │   └── indirectness_table.docx         only when subdomains were recorded
    └── 02_<slug>/ ...
```

Directory names carry the set order as a zero-padded numeric prefix. A non-ASCII outcome name falls back to `outcome_NN`, so the ZIP stays portable.

**`analysis.R` template** (rendered via `glue` from `inst/templates/analysis_script.R.tpl`; the multi-outcome bundle uses `inst/templates/analysis_script_multi.R.tpl`, which re-issues the `run_ma_multi()` / `grade_meta_multi()` / `reorder_outcomes()` / `set_primary()` calls with the arguments actually used).

Both rendered scripts are **syntax-checked with `parse()` before they are written**. If the check fails the script is omitted with a warning and the rest of the bundle still ships.

> **[v0.2 — superseded]** The skeleton below shows the shape only. The real template renders every v0.4/v0.5 argument as well: the domain rationales, the rating-target override and its rationale, `threshold_type`, `require_threshold`, the risk-of-bias settings (`rob_some_concerns`, `rob_overrides`, `rob_override_rationale`, `rob_dominant_threshold`, `rob_refit`), and the full `indirectness_subdomains` table. The template file is authoritative, not this listing.

```r
# pmatools auto-generated reproducibility script
# Generated: {{timestamp}}
# pmatools version: {{pmatools_version}}

library(pmatools)

data <- ingest_data("data_long.csv", format = "long")

ma <- run_ma(
  data,
  outcome_type = "{{outcome_type}}",
  sm           = "{{sm}}",
  method       = "{{method}}",
  method.tau   = "{{method_tau}}",
  random       = {{random}},
  common       = {{common}},
  hakn         = {{hakn}},
  prediction   = {{prediction}},
  incr         = {{incr}}{{subgroup_arg}}
)

g <- grade_meta(
  ma,
  study_design            = "{{study_design}}",
  rob                     = {{rob_expr}},
  rob_dominant_threshold  = {{rob_dom_threshold}},
  indirectness_dominant_threshold = {{indirectness_dom_threshold}},
  rob_inflation_threshold = {{rob_inf_threshold}},
  small_values            = {{small_values_expr}},
  indirectness            = "{{indirectness}}",
  outcome_type            = "{{ois_outcome_type}}",
  threshold               = {{threshold_expr}},
  ois_p0                  = {{ois_p0_expr}},
  ois_p1                  = {{ois_p1_expr}},
  ois_rrr                 = {{ois_rrr_arg}},
  ois_delta               = {{ois_delta_expr}},
  ois_sd                  = {{ois_sd_expr}},
  pubias_small_industry   = "{{pubias_small_industry}}",
  pubias_funnel_asymmetry = {{pubias_funnel_expr}},
  pubias_unpublished      = {{pubias_unpub_expr}},
  outcome_name            = "{{outcome_name}}"
)

print(g)
plot_forest(ma, title = "{{outcome_name}}")
plot_funnel(ma)

ft <- sof_table(g, per = {{per}}, prediction = {{prediction_sof}}{{convert_args}})
print(ft)

grade_report(
  outcomes    = list("{{outcome_name}}" = g),
  primary     = "{{outcome_name}}",
  format      = "docx",
  output_dir  = ".",
  output_file = "grade_appendix"
)
```

`{{...}}` placeholders are substituted by `glue::glue()` from a list constructed in `export_bundle()`. NULL-valued slots become `NULL` literal in the rendered script. `subgroup_arg` is `""` if no subgroup, otherwise `,\n  subgroup = "<col>"`.

#### 4.8.1 Argument origin tracking (for analysis.R faithfulness)

To produce an `analysis.R` that faithfully reflects what the user did in Shiny, every `grade_meta()` argument must be expressible as one of **four** "origin types":

| Origin | Source | analysis.R rendering |
|---|---|---|
| `"null"` | unset | literal `NULL` |
| `"scalar"` | typed value (string or number) | quoted/unquoted literal (`"some"`, `0.10`) |
| `"column"` | per-study vector from data | column reference (`data$rob`) |
| `"vector"` | literal vector typed or built in the UI | deparsed `c(...)` literal |

**Any other value aborts.** An unrecognised origin used to fall through and render the argument as `NULL`, which silently produced a reproducibility script that did not reproduce the analysis. Since v0.5.0 `export_bundle()` aborts with a message naming the bad origin and listing the accepted ones. Callers that build `grade_args` programmatically must therefore keep the origin vocabulary in sync with this table.

Named vectors are a special case: the `"vector"` rendering drops names, which would silently break `rob_overrides` / `rob_override_rationale` (both keyed on `studlab`). Those arguments are rendered with their names preserved.

The Shiny app stores each argument in its `state` as a list with origin metadata:

```r
state$grade_args <- list(
  rob          = list(value = rob_vec,  origin = "column", col = "rob"),
  indirectness = list(value = "no",      origin = "scalar"),
  threshold       = list(value = 0.20,      origin = "scalar"),
  threshold_scale = list(value = "auto",    origin = "scalar"),
  ois_p0       = list(value = 0.25,      origin = "scalar"),
  ois_events   = list(value = NULL,      origin = "null"),
  small_values = list(value = NULL,      origin = "null"),
  ...
)
```

`export_bundle()` accepts this richer structure (or falls back to inferring origin if a plain meta/grade object is passed) and renders via:

```r
ARG_LIT_ORIGINS <- c("null", "column", "scalar", "vector")

.arg_lit <- function(spec) {
  if (is.list(spec) && !is.null(spec$origin)) {
    origin <- spec$origin
    if (length(origin) != 1L || !is.character(origin) ||
        !origin %in% ARG_LIT_ORIGINS) {
      rlang::abort(...)                      # names the bad origin; see §4.8.1
    }
    if (origin == "null")   return("NULL")
    if (origin == "column") return(paste0("data$", spec$col))
    if (origin == "scalar") { ... }          # quoted/unquoted literal
    if (origin == "vector") { ... }          # deparsed c(...)
  }
  # Plain value (CLI callers who never set grade_args): best-effort literal.
}
```

CLI users (who never set `grade_args` explicitly) get a best-effort fallback: scalars are quoted, vectors are deparsed as literal `c(...)`. The Shiny path always gets the cleanest output via origin tracking.

**`results.txt`** contains:

```
================================================================
pmatools analysis — generated {{timestamp}}
Outcome: {{outcome_name}}
================================================================

{{summary(ma) text}}

================================================================
GRADE assessment
================================================================

{{print(g) text}}

Domain notes:
{{domain notes one per line}}
```

### 4.9 `grade_table()` and `grade_report()`

```r
grade_table(
  outcomes,                               # named list of pmatools objects, OR a pmatools_set
  primary      = NULL,
  style        = c("gradepro", "bmj"),
  palette      = c("pastel", "classic"),
  show_domains = TRUE,
  per          = 1000,
  prediction   = FALSE,
  follow_up    = NULL,
  unit         = NULL,
  label_intervention = "intervention",
  label_control      = "control"
) -> flextable

grade_report(
  outcomes,                               # named list, OR a pmatools_set
  primary      = NULL,
  palette      = c("pastel", "classic"),
  style        = c("gradepro", "bmj"),
  format       = "docx",                  # any of "docx", "html", "pdf", "md"
  output_dir   = getwd(),
  output_file  = "grade_report",
  title        = "Certainty of Evidence Assessment (Core GRADE series)",
  show_domains = TRUE,
  per          = 1000,
  prediction   = FALSE,
  label_intervention = "intervention",
  label_control      = "control"
) -> chr (paths to written files)
```

Passing a `pmatools_set` uses the set's `order` for the row order and its `primary` for grouping; the named-list API is unchanged. In the BMJ style, per-outcome `follow_up` / `unit` recorded by `grade_meta_multi()` are picked up automatically, and a table mixing effect measures keeps a generic Effect header plus a footnote pointing at the per-cell measure names.

`print.pmatools()` / `summary.pmatools()` are unchanged apart from also reporting `$rating_target` and any low-RoB refit.

### 4.10 Multi-outcome workflow [v0.5.0]

```r
run_ma_multi(
  data,                     # canonical long tibble with an `outcome` column
  outcomes     = NULL,      # NULL -> every outcome present, in first-seen order
  sm           = NULL,      # single value, or named by outcome
  outcome_type = NULL,      # single value, or named by outcome
  ...                       # forwarded to run_ma()
) -> named list of meta objects

grade_meta_multi(
  ma_list,                  # named list from run_ma_multi()
  common      = list(),     # grade_meta() arguments shared by every outcome
  per_outcome = list(),     # keyed by outcome name; overrides `common`
  data        = NULL,
  primary     = NULL
) -> pmatools_set

reorder_outcomes(set, order)     -> pmatools_set   # `order` must list every outcome exactly once
set_primary(set, primary)        -> pmatools_set   # NULL clears the primary set
```

Because `sm` and `outcome_type` may be named by outcome, binary and continuous outcomes can share one session.

`run_ma()` itself is unchanged and still **aborts** on data holding more than one outcome; `run_ma_multi()` is the only supported way to batch.

**Failure semantics.** An outcome that fails is recorded as `NULL` with a warning so the rest of the batch completes — **except** the Core GRADE 2 entry gate (§4.5.1), whose abort (condition class `"pmatools_threshold_gate"`) is re-raised unchanged.

### 4.11 The `pmatools_set` class [v0.5.0]

A list with at least:

| Field | Contents |
|---|---|
| `$outcomes` | named list of `pmatools` objects (a failed outcome is `NULL`) |
| `$order` | character vector: display order, every outcome exactly once |
| `$primary` | character vector: primary outcomes (possibly empty) |
| `$data` | the long-format data the set was built from, when available |

`print()` and `summary()` methods list each outcome's certainty, rating target and analysis set; a low-RoB refit is called out per outcome, and a set mixing analysis sets says so. `grade_table()`, `grade_report()` and `export_bundle()` all accept the set directly.

### 4.12 Rare-event methods and additional plots

Exported but not specified in detail here; see the roxygen pages. They predate this section and their signatures are authoritative in the code:

`run_rare_ma()`, `rare_event_diagnostics()`, `plot_rare_sensitivity_forest()`, `plot_trimfill_forest()`, `plot_forest_rob()`, `plot_forest_indirectness()`, `plot_forest_pubias_subgroup()`, `evidence_profile()`.

(`combine_arms()` in `R/combine_arms.R` is **internal**, not in `NAMESPACE`; earlier revisions of this list wrongly named it as exported.)

`evidence_profile(grade, palette, study_design, other_text, other_downgrade)` renders the per-outcome GRADE evidence profile used by both bundle layouts.

### 4.13 `indirectness_table()` [v0.5.0]

```r
indirectness_table(x, summary_text = NULL, ...) -> flextable
```

Renders `x$indirectness_subdomains` (§4.5.3): target question, evidence found, a colour-graded 4-option judgment row with the recorded answer ticked, and a merged "Judgment across subdomains" row carrying the overall judgment. Aborts with a message telling the caller how to record subdomains when `x` has none.

**Attribution.** This is a **pmatools table layout implementing Core GRADE 5's per-PICO reasoning — not a Core GRADE 5 publication table.** The article body carries exactly two tables: Table 1 (an adaptation of a summary of findings table) and Table 2 ("Summary of indirectness issues": PICO element / Reason for rating down / Examples / Likelihood of rating down). Nothing of this shape appears there, and the strings "sufficiently direct", "probably yes" and "probably no" occur nowhere in it. *(The online supplementary appendices have not been checked.)* The footer of the rendered table states this, and also reproduces the Table 2 likelihood gradient (Population "Low" → Intervention "Intermediate" → Comparison "Substantial" → Outcome "High likelihood"), which the symmetric worst-case fold does not reproduce.

---

## 5. Algorithm specifications

### 5.0 Domain judgment vocabulary

Every domain returns one of **three** levels (v0.3+):

```r
GRADE_LEVELS <- c("no", "some_concerns", "serious")   # 0, -1, -2
```

The legacy spellings `"some"` and `"very_serious"` are accepted on input and normalised (`"some"` → `"some_concerns"`, `"very_serious"` → `"serious"`), but **`"some_concerns"` is what the objects, notes and tables contain**. Consumers matching on judgment strings must match the normalised form. Anything outside the accepted set aborts via `validate_grade_level()`.

### 5.1 Risk of bias — Core GRADE 4 Fig 2 flowchart (v0.5.0)

`assess_rob()` follows the BMJ 2025 Core GRADE 4 Fig 2 flowchart literally. `R/domain_rob.R`'s header comment is the maintained long form; this section is the contract.

**Step 0 — binary classification.** Each study is folded into low or high risk of bias:

| `rob_some_concerns` | low | high |
|---|---|---|
| `"low"` (default) | `no`, `some_concerns` | `serious` |
| `"high"` | `no` | `some_concerns`, `serious` |

`rob_overrides` (named by `studlab`) are applied **before** the fold; each override requires a rationale, and a key matching no study label aborts rather than being silently ignored.

**Step 1 — dominance gate (Fig 2's first node).** `w_high` is the inverse-variance weight share carried by high-RoB studies.

```
dominated  <=>  w_high >= rob_dominant_threshold      # default 0.55 (Fig 2 footnote: ">=55% = possibly dominating"), compared with >=
```

If the weight share cannot be computed the count share is used and the notes say so; if neither can be computed, dominance is assumed (conservative).

> `rob_dominant_threshold` was deprecated in v0.3.1 ("accepted but ignored"). **That decision is retracted in v0.5.0**: the gate is the first decision node of Fig 2 and the two branches below it are not interchangeable.

**Step 2a — dominated = Yes: check the direction of bias.** `TE_all` and `TE_low` are each classified into one of three zones defined by ±Threshold (`above` / `trivial` / `below`). With `za = zone(TE_all)`, `zl = zone(TE_low)`:

| Rule | Condition | Judgment |
|---|---|---|
| 1 | `za == zl == "trivial"` | `"no"` |
| 2 | `za == zl`, non-trivial, inflation ≤ `rob_inflation_threshold` | `"no"` |
| 3 | `za == zl`, non-trivial, inflation > `rob_inflation_threshold` | `"some_concerns"` (−1) |
| 4 | `za != zl`, no sign flip across null | `"some_concerns"` (−1) |
| 5 | `za != zl`, sign flip (`above` ↔ `below`) | `"serious"` (−2) |

`inflation_ratio = (|TE_all| - |TE_low|) / |TE_low|` is evaluated **only** when the shift runs in the bias-favouring direction implied by `small_values`; a deflation in that direction never triggers a downgrade. When the direction gate blocks a downgrade that the inflation threshold would otherwise have caused, the notes say so explicitly, including the direction reasoning, so readers do not conclude the threshold was ignored (v0.4.0).

When `threshold_internal` is NULL/NA/≤ 0 the trivial zone collapses to `{0}`, so only rule 5 can fire.

**Step 2b — dominated = No.** **This branch never rates the domain down.** It decides which studies the analysis should use:

| "Substantial difference between high- and low-RoB estimates?" | `analysis_set` |
|---|---|
| Yes (a zone change, i.e. rule 4/5, or a bias-favouring inflation beyond `rob_inflation_threshold`) | `"low_only"` |
| No | `"all"` |

Consequence (breaking in v0.5.0): a body of evidence in which a *minority* of the weight is at high risk of bias can no longer be downgraded for risk of bias.

**Step 3 — refit (`rob_refit`, default `TRUE`).** When `analysis_set == "low_only"`, `grade_meta()` refits the meta-analysis on the low-RoB subset. Every downstream domain, the rating target, the baseline risk and the SoF table then use the restricted estimate, **so pooled numbers can change with no change to the input data**. The refit is announced with a message, recorded in the RoB notes, shown by `print()`, and footnoted in `sof_table()`. `rob_refit = FALSE` keeps the full analysis and returns the recommendation only.

The object then carries `$meta` (rated analysis), `$meta_full` (all studies), `$rob_analysis_set` and `$rob_refit` — see §4.5.

### 5.2 Inconsistency — BMJ Core GRADE 3 flowchart (v0.2)

> **Design rationale.** v0.2 preserves the BMJ Core GRADE 3 flowchart implemented in v0.1.0. The only enhancement is that **Step 2's clinical decision boundary uses ±Threshold** when supplied (instead of always using null = 0). I², τ², and Q-test are supplementary context only — they never drive the judgment in the manual flowchart path. PI is **not** used in the decision logic. Per-study CIs are also not used (CI overlap is judged clinically by the user in manual mode, or proxied by I² in auto mode).

**Algorithm:**

```
INPUT:
  meta_obj                          # {meta} object
  threshold_internal                # Threshold on TE scale, or NULL
  inconsistency                     # scalar override, or NULL
  inconsistency_ci_diff             # "yes"/"no"/NULL  (Step 1 manual)
  inconsistency_threshold_side      # "majority_one_side"/"opposite_sides"/NULL (Step 2 manual)
  inconsistency_subgroup_explained  # "yes"/"no"/NULL  (Step 3 manual)

# ---- Path A: scalar override ----
if (!is.null(inconsistency)) {
  return judgment = inconsistency, auto = FALSE
}

# ---- Path B: manual flowchart (BMJ-faithful) ----
if (!is.null(inconsistency_ci_diff)) {

  # Step 1
  if (inconsistency_ci_diff == "no") {
    return judgment = "no", auto = FALSE,
           notes = "Step 1: no important differences in point estimates / adequate CI overlap."
  }

  # ci_diff = "yes" → Step 2
  if (is.null(inconsistency_threshold_side)) {
    abort "inconsistency_ci_diff = 'yes' requires inconsistency_threshold_side"
  }

  if (inconsistency_threshold_side == "majority_one_side") {
    return judgment = "no", auto = FALSE,
           notes = "Step 2: important differences exist, but majority on one side of clinical Threshold → do not rate down (per BMJ Core GRADE 3 flowchart)."
  }

  # opposite_sides → Step 3
  if (is.null(inconsistency_subgroup_explained)) {
    abort "inconsistency_threshold_side = 'opposite_sides' requires inconsistency_subgroup_explained"
  }

  if (inconsistency_subgroup_explained == "yes") {
    return judgment = "no", auto = FALSE,
           notes = "Step 3: opposite-sided estimates explained by credible subgroup; present subgroups separately."
  }

  return judgment = "some_concerns", auto = FALSE,
         notes = "Step 3: opposite-sided estimates not explained by credible subgroup → rate down one level."
}

# ---- Path C: auto-detect ----

# Step 1 surrogate: I² > 30%   (INCONSISTENCY_I2_CUT; v0.5.0, was 25%)
ci_diff_yes <- (i2_pct > 30)

if (!ci_diff_yes) {
  return judgment = "no", auto = TRUE,
         notes = "AUTO Step 1: No important heterogeneity (I2 <= 30%) → do not rate down." + I2 caveat
}

# Step 2 surrogate: one 3-zone tally, with M = 0 when no threshold applies
M <- if (is finite and > 0) threshold_chosen else 0

n_above   <- sum(TE > +M)
n_below   <- sum(TE < -M)
n_trivial <- k - n_above - n_below      # 0 when M == 0

pct_max_zone  <- max(n_above, n_trivial, n_below) / k
pct_each_side <- min(n_above, n_below)  / k

if (pct_max_zone >= 0.80) {                    # ZONE_MAJORITY (CINeMA)
  threshold_side <- "majority_one_side";    judgment <- "no"
} else if (pct_each_side >= 0.20) {            # OPPOSITE_EACH (pmatools)
  threshold_side <- "opposite_substantial"; judgment <- "some_concerns"
} else {
  threshold_side <- "heterogeneous";        judgment <- "some_concerns"
}
# Notes carry: the I² surrogate caveat, the zone-cut-off provenance caveat,
# and (for opposite_substantial) the ICEMAN subgroup caveat and the −1 cap.
```

**Edge cases:**

- `k < 2`: cannot assess inconsistency. Return judgment = `"no"` with note "k < 2; inconsistency not assessable."
- I² is NA (e.g., k = 1): the Step 1 surrogate returns FALSE → judgment = `"no"` with note "I² unavailable; cannot detect heterogeneity."
- All TE values equal (τ² = 0): I² will be 0 → the Step 1 surrogate returns FALSE → judgment = `"no"`.
- Study-level TEs unavailable: Step 2 is not assessable → `"some_concerns"` (conservative).
- Threshold supplied but `threshold_internal` cannot be derived (unknown sm): function aborts before reaching this domain.

**Judgment interpretation table:**

| Path | Step 1 / I² | Step 2 / Threshold check | Step 3 / subgroup | Judgment |
|---|---|---|---|---|
| Manual | ci_diff = "no" | — | — | **No** |
| Manual | ci_diff = "yes" | majority_one_side | — | **No** |
| Manual | ci_diff = "yes" | opposite_sides | yes | **No** + note |
| Manual | ci_diff = "yes" | opposite_sides | no | **some_concerns** *(capped at −1)* |
| Auto | I² ≤ 30% | — | — | **No** |
| Auto | I² > 30% | majority_one_side (max zone ≥ 80%) | — | **No** |
| Auto | I² > 30% | opposite_substantial (≥ 20% each side) | n/a | **some_concerns** |
| Auto | I² > 30% | heterogeneous (neither) | n/a | **some_concerns** |

`"serious"` (−2) for this domain is reachable only through the scalar `inconsistency` override.

### 5.3 Chinn's formula (SMD ↔ OR)

```
factor = π / √3 ≈ 1.81380
log(OR) = SMD * factor
OR     = exp(SMD * factor)

SMD = log(OR) / factor
```

CI bounds use the same multiplication. Document in `?chinn_smd_to_or` that the conversion assumes the latent-variable/logistic distribution (Cox 1970, Hasselblad & Hedges 1995, Chinn 2000).

### 5.4 Threshold auto-default per `sm` (suggested defaults)

When the Shiny app pre-fills the Threshold input, use these **placeholder defaults** based on `meta_obj$sm`. The user can always override — and, except for SMD, should.

**Every default now carries a `source` field** naming where the number comes from, and **for binary ratio measures the absolute candidate leads** (v0.5.0). Rationale, in the source's own words:

- **No ratio-scale MID exists anywhere in Core GRADE 1, 6 or 7.** Every binary MID discussed there is on the absolute scale (per 1000 or percent) — Core GRADE 7 lists MIDs "associated with mortality of 1%, stroke of 2%, myocardial infarction of 3%, and serious gastrointestinal bleeding of 5%"; Core GRADE 2 discusses "an MID of 5 deaths per [1000]". A ratio-scale default is therefore a pmatools extrapolation.
- **The MID belongs to the outcome, not to the effect measure.** Those same Core GRADE 7 numbers "reflect the gradient of importance across these outcomes"; one default shared by every outcome erases that gradient.
- **The procedure runs the other way round.** Core GRADE 7 has users read the CI first and pin down a MID only where the verdict turns on it ("whether the MID for mortality is 2%, 1%, or less than 1%, the CI does not cross the MID threshold ... one need not specify a single particular value"). Starting from a pre-filled default inverts that order.
- **SMD 0.20 is the one sourced value**, and Core GRADE 6 hedges it: "an SMD of 0.2 is the threshold for a small and important effect", but "clinicians may be appropriately sceptical of this threshold, which is limited by large variability in the methods investigators use to calculate the SMD".

```r
suggest_threshold <- function(meta_obj) {
  sm <- meta_obj$sm
  ard <- list(threshold_user = 0.05, threshold_scale = "ard",
              source = "package_convention")      # 50 per 1000
  switch(sm,
    # Binary ratio measures: ABSOLUTE first, ratio kept as $threshold_ratio.
    "OR"  = c(ard, list(threshold_absolute = ard,
                        threshold_ratio = list(threshold_user = 1.25,
                                               threshold_scale = "ratio",
                                               source = "package_convention"))),
    "RR"  = c(ard, list(threshold_absolute = ard,
                        threshold_ratio = list(threshold_user = 1.20,
                                               threshold_scale = "ratio",
                                               source = "package_convention"))),
    "HR"  = c(ard, list(threshold_absolute = ard,
                        threshold_ratio = list(threshold_user = 1.20,
                                               threshold_scale = "ratio",
                                               source = "package_convention"))),
    "RoM" = list(threshold_user = 1.10, threshold_scale = "ratio",
                 source = "package_convention"),
    "ARD" = ard,
    "SMD" = list(threshold_user = 0.20, threshold_scale = "te_scale",
                 source = "core_grade_6"),        # the only sourced default
    "MD"  = {
      sd_pooled <- compute_pooled_sd(meta_obj)
      list(threshold_user = 0.20 * sd_pooled, threshold_scale = "te_scale",
           source = "package_convention")
    },
    NULL  # unknown sm → no default
  )
}
```

`threshold_scale = "auto"` in `grade_meta()` is **unaffected** by this reordering; only the suggestion helper and the entry-gate error message changed.

**`compute_pooled_sd()` (for MD only):**

```r
compute_pooled_sd <- function(meta_obj) {
  # meta::metacont stores per-study arm SDs
  n_e   <- meta_obj$n.e
  n_c   <- meta_obj$n.c
  sd_e  <- meta_obj$sd.e
  sd_c  <- meta_obj$sd.c

  # Per-study pooled SD (Cohen's pooled formula)
  sd_per_study <- sqrt(
    ((n_e - 1) * sd_e^2 + (n_c - 1) * sd_c^2) /
    (n_e + n_c - 2)
  )

  # Sample-size-weighted average across studies
  weights <- n_e + n_c
  weighted.mean(sd_per_study, weights, na.rm = TRUE)
}
```

If `meta_obj$sd.e/sd.c` is unavailable (some metacont calls), fall back to `weighted.mean(meta_obj$seTE * sqrt(n_total), n_total)` as a rough estimate.

**API surface:**

`suggest_threshold()` and `compute_pooled_sd()` are **exported** (so Shiny can pre-fill the input). The user may also call them directly from R.

**Behavior in `grade_meta()`:**

> **[v0.5.0]** `grade_meta()` still does not auto-fill `threshold`, but it no longer lets the call proceed without one either: with `threshold_type = "mid"` (the default) a missing `threshold` aborts and the error quotes the `suggest_threshold()` value. See §4.5.1.

`grade_meta()` itself does **not** auto-fill `threshold` — passing `threshold = NULL` triggers the I²-fallback path explicitly. The Shiny app calls `suggest_threshold()` to pre-fill the input field, but the *value* the user sees is what gets passed (override-able). This keeps `grade_meta()`'s behavior deterministic and reproducible: no hidden defaults at the R API level.

**Threshold conversion to TE scale** (used internally by `assess_rob()`, `assess_inconsistency()` and `assess_imprecision()`):

```r
threshold_to_te_scale <- function(threshold, threshold_scale, sm) {
  if (is.null(threshold)) return(NULL)

  scale <- if (threshold_scale == "auto") {
    switch(sm,
      "OR" = "ratio", "RR" = "ratio", "HR" = "ratio", "RoM" = "ratio",
      "ARD" = "ard",
      "SMD" = "te_scale", "MD" = "te_scale",
      rlang::abort(sprintf("Cannot auto-detect threshold_scale for sm = %s", sm))
    )
  } else threshold_scale

  switch(scale,
    "te_scale" = threshold,                # already on TE scale
    "ratio"    = log(threshold),            # convert ratio → log
    "ard"      = threshold                  # ARD: keep as-is, but special handling in OIS
                                            # (ois uses p1 = p0 + ARD)
  )
}
```

### 5.5 Imprecision — Core GRADE 2 Fig 4 flowchart (v0.5.0, breaking)

`assess_imprecision()` follows Fig 4 of Core GRADE 2 verbatim. The chosen threshold is the one the **rating target** selects (§4.5.2): ±MID for `important_effect` / `little_to_no_difference`, null (0) for `non_null_effect`.

```
Does the CI cross the chosen threshold?

  YES -> Rate down one level                                        [-1]
         Rate down two levels if either:                            [-2]
           - the CI crosses two thresholds (important benefit AND important harm), or
           - the plain language description implies more uncertainty ("may" not "likely").
         Sample size / OIS is NOT consulted on this path.

  NO  -> Is the effect implausibly large?
           No  (moderate effect) -> Do not rate down                [-0]
           Yes (large effect)    -> OIS approach:
             Continuous outcome:
               N >= OIS (or 800)        -> do not rate down         [-0]
               N <  OIS                 -> rate down one level      [-1]
               N <  30% of OIS          -> consider two levels      [-2]
             Binary outcome:
               relative risk CI ratio >= 3, or odds ratio CI ratio >= 2.5
                                        -> consider two levels      [-2]
               otherwise, calculate OIS:
                 N >= OIS               -> do not rate down         [-0]
                 N <  OIS               -> rate down one level      [-1]
```

**What changed from v0.2/v0.4 (breaking).** Previously the "N ≤ 30% of OIS" rule forced a two-level rate-down regardless of where the CI sat. Under Fig 4 the OIS branch is reached only when the CI is clear of the threshold *and* the effect is implausibly large, so an analysis with a moderate effect, a CI clear of the threshold and a small sample size no longer rates down at all.

**Operationalisation of "implausibly large".** The BMJ text operationalises this for binary outcomes only ("certainly relative risk reduction >40%, possibly >30%"). For continuous outcomes pmatools uses Cohen's convention (standardised effect ≥ 0.8) and **says so in the notes**, flagging it as a pmatools choice rather than a Core GRADE rule.

**CI ratio** (Fig 4 caption) is the upper CI limit divided by the lower limit on the ratio scale.

**Notes** record which Fig 4 path produced the judgment, including which CI-ratio rule fired and the continuous 400-per-group (total N 800) rule of thumb.

**Manual override.** A scalar `imprecision` (with mandatory `imprecision_rationale`) bypasses this assessment entirely (v0.4.0).

---

## 6. Test matrix

`tests/testthat/`:

| File | Coverage |
|---|---|
| test-grade_meta.R *(existing)* | Existing tests preserved; add `rob_inflation_threshold = 0` to those that exercise old "always rate down when small_values = NULL" behavior |
| test-data_ingest.R *(new)* | long & wide CSV/data.frame/clipboard, mapping, format auto-detect, missing optional columns, validation errors |
| test-run_ma.R *(new)* | binary OR/RR with method × method.tau matrix, continuous SMD/MD/RoM, hakn/prediction auto k>=3, subgroup, error on invalid sm |
| test-export_bundle.R *(new)* | ZIP generated; all 9 files present; `analysis.R` is syntactically valid R (`parse()` succeeds); `analysis.R` reproduces same `meta::TE.random` when run via `Rscript` |
| test-domain_rob.R *(new)* | inflation threshold 0/0.05/0.10/0.20 boundary, dominated + below-threshold = "no", dominated + above-threshold = "serious", `small_values = NULL` paths |
| test-inconsistency_threshold.R *(new)* | manual flowchart (3 paths: ci_diff=no / majority_one_side / opposite_sides×subgroup); auto Step 1 = I² > 30% only (Q-test no longer used); auto Step 2 3-zone tally (≥80% max-zone share, ≥20% each-side share) with and without a Threshold; the I² / zone / ICEMAN provenance caveats appear in the notes |
| test-threshold_scale.R *(new)* | `threshold_scale = "auto"` correctly maps OR/RR/HR/RoM → log, MD/SMD → te_scale, ARD → ard; abort on unknown sm |
| test-suggest_threshold.R *(new)* | defaults match table for OR/RR/SMD/MD/ARD/RoM; MD default = 0.2 × pooled SD; unknown sm returns NULL |
| test-chinn.R *(new)* | numerical accuracy of factor π/√3, NA propagation, sof_table integration with convert_smd_to_or = TRUE |

**Added since v0.2** (the files actually on disk are authoritative; `test-threshold_scale.R` and `test-suggest_threshold.R` were folded into other files):

| File | Coverage |
|---|---|
| test-rating_target.R | Core GRADE 2 Fig 2 target derivation, manual override + mandatory rationale, `threshold_type` entry gate |
| test-imprecision.R | Fig 4 paths (§5.5), CI-ratio rules, OIS branch reachability |
| test-rob_flowchart.R | Core GRADE 4 Fig 2 (§5.1): dominance gate, the 5 zone rules, `analysis_set`, refit propagation |
| test-domain_rob.R | inflation threshold boundaries, `rob_some_concerns`, `rob_overrides` |
| test-indirectness_subdomains.R | PICO table normalisation, worst-case rollup, scalar override |
| test-sof_bmj.R | BMJ SoF layout, Difference column, plain language summaries |
| test-multi_outcome.R | `run_ma_multi()`, `grade_meta_multi()`, ordering, the hierarchical bundle layout |
| test-override-rationale.R | every mandatory `*_rationale` (v0.4.0) |
| test-threshold_absolute.R | `threshold_scale = "ard"` + `threshold_baseline` conversion |
| test-domain_pubias.R, test-evidence_profile.R, test-grade_report.R, test-plot_forest_rob.R, test-plot_forest_subgroup.R, test-rare_events.R, test-sof_table.R | as named |

---

## 7. Edge cases

| Case | Expected behavior |
|---|---|
| `ingest_data` with empty data.frame | abort with "no rows" |
| `ingest_data` long with one studlab appearing 3 times | abort with "studlab X has 3 rows; expected 2" |
| `run_ma` with k = 1 | return meta object; `hakn`/`prediction` set FALSE; warn "single study; CI may be unreliable" |
| `run_ma` with k = 2 | hakn FALSE, prediction FALSE; otherwise normal |
| `plot_funnel` with k < 10 | render plot; annotate "Egger's test not run (k < 10)" |
| `grade_meta` with `threshold = -3` (negative) | abort with "threshold must be positive (it is treated as a half-width around null)" |
| `grade_meta` with `threshold` AND `ois_p0/p1` both supplied | use `ois_p0/p1`; note "Threshold provided but explicit ois_p0/p1 takes precedence" in domain notes |
| `sof_table(convert_smd_to_or = TRUE, baseline_risk = NULL)` | abort with informative message |
| `export_bundle` to non-writable directory | abort with file-system error |
| `export_bundle` with `include = c("data")` only | ZIP contains only `data_long.csv`; no analysis.R |
| `export_bundle(ma = m, grade = g, ...)` (legacy named call) | works; deprecation warning once per session (§4.8.1a) |
| `export_bundle` with a `grade_args` entry whose `origin` is not one of the four accepted values | abort naming the bad origin (§4.8.1) |
| `export_bundle` on a `pmatools_set` with zero outcomes | abort "the pmatools_set holds no outcomes" |
| `grade_meta` with `threshold_type = "mid"` and no `threshold` | abort (class `"pmatools_threshold_gate"`), quoting the `suggest_threshold()` value |
| `grade_meta` with a scalar domain override and no matching `*_rationale` | abort |
| `rob_overrides` key matching no `studlab` | abort (never silently ignored) |
| `run_ma` on data with more than one `outcome` value | abort; use `run_ma_multi()` |
| One outcome of a `run_ma_multi()` / `grade_meta_multi()` batch fails | recorded as `NULL` with a warning; the batch continues — except a threshold-gate abort, which is re-raised |
| Non-ASCII outcome name in the multi-outcome bundle | directory falls back to `outcome_NN` |

---

## 8. Performance & resource

> **[v0.2 targets — not re-verified.]** The numbers below were written as v0.2 design targets and have **not** been measured again since. Treat them as intent, not as measured behaviour; in particular the multi-outcome bundle (§4.8.3) writes one plot set per outcome, so its wall time scales with the number of outcomes and was never covered by the single-outcome ZIP target. Re-measure before quoting any of these figures.

- `metaprop` baseline-risk pooling can be slow for large k. **Correction:** the implementation falls back to the simple pooled proportion (with a warning) when `meta::metaprop()` **errors**; there is no time limit. Earlier revisions of this section claimed a 10-second timeout — no such timeout exists in `.compute_control_risk()`.
- ZIP creation should complete in < 5 seconds for k ≤ 100. *(v0.2 target, unverified; single-outcome layout only.)*
- `plot_forest()` should render in < 2 seconds for k ≤ 50. *(v0.2 target, unverified.)*
- Memory: assume the `meta` object fits in memory; no streaming required.

---

## 9. Documentation deliverables

| File | Audience | Content |
|---|---|---|
| `README.md` (updated) | First-time users | install_github, quick start (CLI + Shiny URL), how-to sections and an index of every exported function |
| `sample.R` | R users | End-to-end CLI workflow on bundled `cbti_depression.csv`, run as part of the release gate (§10). **This is what shipped instead of `vignettes/pmatools_cli.Rmd`, which does not exist.** |
| `PLAN.md` (updated) | Maintainer | Implementation status per release + roadmap **only**. It deliberately holds no API signatures and no domain logic — those live here, and duplicating them is what made PLAN.md drift. |
| `NEWS.md` | Users upgrading | Authoritative per-release change list, including breaking changes |
| `SPEC.md` (this file) | Maintainer + AI implementers | Authoritative spec |
| roxygen man pages | Function-level reference | `?grade_meta`, `?sof_table`, etc., kept in sync with SPEC |

---

## 10. Backward compatibility checklist

> **[v0.2 — superseded]** The checklist below was the v0.2 release gate and is kept for history. v0.4.0 and v0.5.0 deliberately broke items 2–5 of it (mandatory rationales, the SoF header rename, the Core GRADE 2 entry gate, the RoB and Imprecision flowcharts). `NEWS.md` is the authoritative change list. The v0.5.0 gate is instead:
>
> - [ ] `Rscript -e 'devtools::test()'` — all tests pass
> - [ ] `Rscript -e 'devtools::load_all("."); source("sample.R")'` — the worked example runs end to end
> - [ ] `Rscript -e 'devtools::check(args = c("--no-manual"))'` — 0 ERROR, 0 WARNING
> - [ ] `export_bundle(ma = m, grade = g, ...)` still works for the out-of-repo Shiny caller (§4.8.1a)

- [ ] All v0.1.0 example code in `sample.R` continues to run unchanged with v0.2.0 installed
- [ ] All existing `tests/testthat/test-grade_meta.R` cases pass without modification (or with clearly-documented adjustments for the two intentional behavior changes below)
- [ ] `grade_meta(..., rob = "some")` returns identical output in v0.1.0 and v0.2.0
- [ ] `sof_table(g)` (default args) returns identical flextable in v0.1.0 and v0.2.0
- [ ] `grade_report(...)` produces identical docx structure (allowing minor stylistic differences from flextable updates) in v0.1.0 and v0.2.0

**Intentional behavior changes (documented in CHANGELOG):**

1. **RoB `small_values = NULL` path (§5.1):** v0.1.0 always rated down conservatively when dominated; v0.2.0 only rates down when relative inflation exceeds `rob_inflation_threshold`. Set `rob_inflation_threshold = 0` to restore v0.1.0 behavior.
2. **Inconsistency auto Step 1 (§5.2 Path C):** v0.1.0 used `I² > 25% OR Q p < 0.10`; v0.2.0 dropped the Q-test; **v0.5.0 raised the cut-off to `I² > 30%`**, the only figure Core GRADE 3 puts on paper ("one will seldom see serious inconsistency with I2 values <30%"). Q-test is supplementary in notes only. Analyses with 25% < I² ≤ 30% that previously reached Step 2 now stop at Step 1 and are not rated down.

---

## 11. Out of scope (v0.6+)

- GRADE upgrade domains (large effect, dose-response, plausible confounding)
- GRADEpro JSON export/import
- shinyapps.io deployment automation in pmatools (kept in pairwise_meta_analysis)
- Internationalization
- CRAN submission

**Delivered since this list was written:** the multi-outcome session (§4.10–§4.11) shipped in v0.5.0. The pmatools side — `run_ma_multi()`, `grade_meta_multi()`, `pmatools_set`, `reorder_outcomes()`, `set_primary()` and the hierarchical export layout — is complete; wiring it into the Shiny wizard is UI work tracked in the app's own SPEC.

---

## 12. References

- Guyatt G, et al. *Core GRADE 1: Overview*. BMJ 2025. PMID: 40262844.
- Guyatt G, et al. *Core GRADE 2: Choosing the target of certainty rating and assessing imprecision*. BMJ 2025;389:e081904. doi:10.1136/bmj-2024-081904.
- Guyatt G, et al. *Core GRADE 3: Inconsistency*. BMJ 2025. PMID: 40328467.
- Guyatt G, et al. *Core GRADE 4: Risk of bias, publication bias*. BMJ 2025. PMID: 40360206.
- Guyatt G, et al. *Core GRADE 5: Indirectness*. BMJ 2025. PMID: 40393729.
- Chinn S. *A simple method for converting an odds ratio to effect size for use in meta-analysis.* Stat Med. 2000;19(22):3127-3131.
- Nikolakopoulou A, et al. *CINeMA: An approach for assessing confidence in the results of a network meta-analysis.* PLoS Med. 2020;17(4):e1003082.
- Hasselblad V, Hedges LV. *Meta-analysis of screening and diagnostic tests.* Psychol Bull. 1995;117(1):167-178.
