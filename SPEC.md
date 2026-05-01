# pmatools v0.2 — Package Specification

> Authoritative specification for the pmatools R package. Implementation MUST conform to this document. UI-side concerns (Shiny wizard, educational copy, accordion layout) are specified separately in `~/Developer/pairwise_meta_analysis/SPEC.md`.

**Version target:** 0.2.0
**Backward compatibility:** All v0.1.0 public APIs continue to work unchanged. New parameters are added with defaults that preserve existing behavior. No breaking changes.

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
  rmarkdown,
  testthat (>= 3.0.0),
  readxl,
  DT
```

`DT` and `readxl` are Suggests because they are used only by `ingest_data()` for Excel/clipboard paths and by Shiny consumers.

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
| `rob` | chr | Risk of bias label (Cochrane RoB 2.0 or GRADE level). Per-study. |
| `indirectness` | chr | Indirectness label (No / Some / Serious / Very serious). Per-study. |
| `subgroup` | chr / fct | Subgroup variable. Per-study. |

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

Optional columns (`rob`, `indirectness`, `subgroup`) are per-study (single column).

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

### 4.5 `grade_meta()` [modified — backward compatible]

All v0.1.0 parameters preserved. **New parameters:**

```r
grade_meta(
  meta_obj,
  ...,                                  # all v0.1.0 params unchanged
  rob_inflation_threshold = 0.10,       # NEW: minimum relative inflation to rate down
  mid                     = NULL,       # NEW: minimally important difference (numeric)
  mid_scale               = "auto"      # NEW: how to interpret `mid`
                                         #   "auto"        → infer from meta_obj$sm (recommended)
                                         #   "te_scale"    → already on meta_obj$TE scale
                                         #                   (log for OR/RR/HR/RoM, raw for MD/SMD)
                                         #   "ratio"       → user gave OR/RR ratio (e.g., 1.25);
                                         #                   internally → log(mid)
                                         #   "ard"         → absolute risk difference (binary only);
                                         #                   internally treated specially
) -> S3 "pmatools" object
```

**`mid` and `mid_scale` interaction (auto-detection table):**

When `mid_scale = "auto"`:

| `meta_obj$sm` | User input convention | Internal storage |
|---|---|---|
| `"OR"`, `"RR"`, `"HR"`, `"RoM"` | ratio scale (e.g., 1.25, 1.10) | `log(mid)` (matches `meta_obj$TE`) |
| `"MD"` | raw outcome units (e.g., 3 PHQ-9 points) | `mid` as-is |
| `"SMD"` | standardized units (e.g., 0.20) | `mid` as-is |
| `"ARD"` | proportion (e.g., 0.05 = 5%) | `mid` as-is |

`mid_scale = "te_scale"` is the escape hatch for power users who want to specify directly on the log scale.

When `mid` is supplied but auto-detection fails (unrecognized `sm`), the function aborts with a clear error.

**`rob_inflation_threshold` semantics:**

In `assess_rob()`, after the dominance check, if dominated:

```r
inflation_ratio <- (abs(TE_all) - abs(TE_low)) / abs(TE_low)
if (inflation_ratio > rob_inflation_threshold) {
  judgment <- "serious"
} else {
  judgment <- "no"
}
notes <- sprintf("Dominated by high-RoB studies (%.1f%% weight); |TE_all|=%.3f, |TE_low|=%.3f, relative inflation=%.1f%% (threshold %.0f%%) → %s",
                 100 * weight_high, abs(TE_all), abs(TE_low),
                 100 * inflation_ratio, 100 * rob_inflation_threshold,
                 if (judgment == "serious") "rate down" else "no rate-down")
```

When `small_values` is `NULL` (unknown direction), the conservative rate-down is **only** applied when the threshold is exceeded. This is a behavioral refinement; v0.1.0 always rated down conservatively. Test coverage MUST verify the previous "always rate down when small_values = NULL" tests now pass with `rob_inflation_threshold = 0` (which forces the old behavior).

**`mid` semantics — Inconsistency (BMJ Core GRADE 3 flowchart):**

> v0.2 keeps the existing BMJ-faithful flowchart from v0.1.0. The only change is that **Step 2's clinical threshold uses MID** when it is supplied (instead of always using null = 0). Point estimates are classified into zones around ±MID; "majority on one side of the clinical threshold" is interpreted accordingly. I², τ², and Q-test statistics remain supplementary context — they do not drive the judgment. PI is **not** used in the decision logic.

**The flowchart (BMJ Core GRADE 3, Fig 2):**

```
Step 1: Are there important differences in point estimates AND limited CI overlap?
  NO  → judgment = "no"  (do not rate down)
  YES → continue to Step 2

Step 2: Where do the point estimates fall relative to the clinical decision threshold?
  Majority on one side of threshold → judgment = "no"  (do not rate down)
  Substantial proportion on opposite sides → continue to Step 3

Step 3: Is the opposite-sided inconsistency explained by a credible subgroup analysis?
  YES → judgment = "no" + note "present subgroups separately"
  NO  → judgment = "serious"
```

The "clinical decision threshold" in Step 2 is **null = 0 by default**, but **±MID** when `mid` is supplied. This is the v0.2 enhancement.

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

The algorithm proxies each step from data:

```
Step 1 proxy:
  ci_diff_yes <- (I² > 25%)
  Notes: I² is a statistical proxy for "important differences AND limited CI overlap";
         clinical visual judgment may differ.

Step 2 proxy (depends on whether mid is provided):

  When mid_internal is NULL (v0.1.0 behavior, fall-back):
    threshold = 0 (null)
    pct_positive <- mean(meta_obj$TE > 0)
    if (pct_positive >= 0.75 OR pct_positive <= 0.25) → "majority_one_side"
    else → "opposite_sides"

  When mid_internal is provided (v0.2 enhancement):
    Classify each study's TE into 3 zones around ±MID:
      above_mid: TE > +mid_internal
      trivial:   -mid_internal ≤ TE ≤ +mid_internal
      below_mid: TE < -mid_internal
    Compute:
      pct_one_side <- max(
        (n_above_mid + n_trivial) / k,
        (n_below_mid + n_trivial) / k
      )
    if (pct_one_side >= 0.75) → "majority_one_side"
    else → "opposite_sides"

Step 3 proxy:
  Cannot auto-check subgroup explanation. If Step 2 is "opposite_sides", auto judgment is "serious".
  User must override with inconsistency_subgroup_explained = "yes" to demote to "no".
```

**Auto judgment outputs (asymmetric vs manual; preserved from v0.1.0):**

| Auto path outcome | Auto judgment | Manual flowchart equivalent |
|---|---|---|
| ci_diff_yes = FALSE | `"no"` | `"no"` (same) |
| ci_diff_yes & majority_one_side | **`"some"`** | `"no"` (manual) |
| ci_diff_yes & opposite_sides | `"serious"` | `"serious"` (same, modulo Step 3) |

Why the auto path returns `"some"` (instead of BMJ's `"no"`) when majority_one_side: the auto Step 1 proxy (I² > 25%) is statistical, not clinical. It cannot rule out that the heterogeneity is clinically meaningful. Returning `"some"` instead of `"no"` is a conservative acknowledgment that "we detected heterogeneity but cannot confirm it is clinically trivial without your judgment." Users who want the BMJ-faithful "no" should supply manual params.

**Notes content (all signals shown for transparency):**

```
Method: {{"scalar (path A)" | "manual flowchart (path B)" | "auto (path C)"}}
{{#if path_C}}
AUTO Step 1: I² = {{i2_pct}}% → {{"important differences detected (I² > 25%)" | "no important heterogeneity (I² ≤ 25%)"}}
AUTO Step 2 ({{threshold_label}}):
  {{#if mid}}
    Zone counts: above_mid = {{n_above}}, trivial = {{n_trivial}}, below_mid = {{n_below}} (k = {{k}})
    Largest one-side proportion = {{pct_one_side}}% → {{"majority_one_side" | "opposite_sides"}}
  {{else}}
    {{pct_positive}}% of TE > 0 → {{"majority_one_side" | "opposite_sides"}}
  {{/if}}
AUTO Step 3: subgroup explanation not auto-detectable; supply inconsistency_subgroup_explained = "yes" to override
{{/if}}
Supportive context: I² = {{i2_pct}}%, τ² = {{tau2}}, Q p = {{q_p}} (supplementary only)
Resulting judgment: {{judgment}}
```

`{{threshold_label}}` is `"vs ±MID = ±{{mid_internal}}"` when MID is supplied, otherwise `"vs null = 0"`.

**`mid` semantics — Imprecision:**

In `assess_imprecision()`, when `mid` is supplied AND no explicit `ois_*` is provided:

- Binary, sm = OR/RR: `ois_p1 = ois_p0 * exp(mid_internal)` (mid_internal is on log scale via `mid_scale = "auto"`).
- Binary, ARD scale: `ois_p1 = ois_p0 + mid_internal` (mid_internal is the absolute risk difference).
- Continuous (MD): `ois_delta = mid_internal` (raw outcome units).
- Continuous (SMD): `ois_delta = mid_internal × pooled_SD` *(see §5.4 for pooled_SD computation)*.

If both `mid` and `ois_*` supplied, `ois_*` wins. Notes string indicates source.

`mid` and `mid_scale` are the **single source of truth** — Inconsistency and Imprecision use the same `mid_internal` derived from them, never two different MIDs.

### 4.6 `sof_table()` [modified — backward compatible]

```r
sof_table(
  x,                                     # pmatools object
  ...,                                    # all v0.1.0 params
  convert_smd_to_or = FALSE,              # NEW
  baseline_risk     = NULL,               # existing; semantics extended
  threshold_label   = NULL                # NEW: optional free-text label
) -> flextable
```

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
- SoF table shows: Outcome, k, N, Control rate (X per `per`), Exp. rate (Y per `per`, [Y_lo; Y_hi]), Effect (SMD ...), Certainty.
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

### 4.7a `suggest_mid()` [new helper, exported]

```r
suggest_mid(meta_obj) -> list(mid_user, mid_scale) | NULL
```

Returns a conventional default MID for the given `{meta}` object based on `meta_obj$sm`. See §5.4 for the table. Returns `NULL` when `sm` is unrecognized.

For `sm = "MD"`, calls `compute_pooled_sd()` internally and returns `0.20 * sd_pooled`.

### 4.7b `compute_pooled_sd()` [new helper, exported]

```r
compute_pooled_sd(meta_obj) -> numeric
```

Returns the sample-size-weighted pooled standard deviation across studies. Required input: `meta_obj` from `metacont()` with `sd.e` and `sd.c` available. Falls back to `weighted.mean(seTE * sqrt(n_total), n_total)` when arm-level SDs are missing.

### 4.8 `export_bundle()` [new]

```r
export_bundle(
  ma,                                     # meta object (from run_ma)
  grade,                                  # pmatools S3 object (from grade_meta)
  output_dir   = ".",
  bundle_name  = "pmatools_results",
  include      = c("data", "script", "results",
                   "forest", "funnel", "grade_table", "grade_appendix"),
  per          = 1000,
  prediction   = FALSE,
  convert_smd_to_or = FALSE,
  baseline_risk     = NULL,
  threshold_label   = NULL,
  forest_args  = list(),                  # passed to plot_forest()
  funnel_args  = list()                   # passed to plot_funnel()
) -> chr (path to .zip)
```

**ZIP contents (when all `include` items requested):**

```
{bundle_name}.zip
├── data_long.csv
├── analysis.R
├── results.txt
├── forest_plot.pdf
├── forest_plot.png         (300 dpi, width = max(7, 3 + 0.3*k))
├── funnel_plot.pdf
├── funnel_plot.png         (300 dpi)
├── grade_table.docx        (multi-outcome SoF; here single outcome → 1-row)
└── grade_appendix.docx     (full grade_report() docx output)
```

**`analysis.R` template** (rendered via `glue` from `inst/templates/analysis_script.R.tpl`):

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
  rob_inflation_threshold = {{rob_inf_threshold}},
  small_values            = {{small_values_expr}},
  indirectness            = "{{indirectness}}",
  outcome_type            = "{{ois_outcome_type}}",
  mid                     = {{mid_expr}},
  ois_p0                  = {{ois_p0_expr}},
  ois_p1                  = {{ois_p1_expr}},
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

To produce an `analysis.R` that faithfully reflects what the user did in Shiny, every `grade_meta()` argument must be expressible as one of three "origin types":

| Origin | Source | analysis.R rendering |
|---|---|---|
| `"null"` | unset | literal `NULL` |
| `"scalar"` | typed value (string or number) | quoted/unquoted literal (`"some"`, `0.10`) |
| `"column"` | per-study vector from data | column reference (`data$rob`) |

The Shiny app stores each argument in its `state` as a list with origin metadata:

```r
state$grade_args <- list(
  rob          = list(value = rob_vec,  origin = "column", col = "rob"),
  indirectness = list(value = "no",      origin = "scalar"),
  mid          = list(value = 0.20,      origin = "scalar"),
  mid_scale    = list(value = "auto",    origin = "scalar"),
  ois_p0       = list(value = 0.25,      origin = "scalar"),
  ois_events   = list(value = NULL,      origin = "null"),
  small_values = list(value = NULL,      origin = "null"),
  ...
)
```

`export_bundle()` accepts this richer structure (or falls back to inferring origin if a plain meta/grade object is passed) and renders via:

```r
.format_arg <- function(spec) {
  if (spec$origin == "null"   || is.null(spec$value)) return("NULL")
  if (spec$origin == "column") return(paste0("data$", spec$col))
  if (spec$origin == "scalar") {
    if (is.character(spec$value)) return(shQuote(spec$value))
    if (is.numeric(spec$value))   return(format(spec$value))
    if (is.logical(spec$value))   return(as.character(spec$value))
  }
  # Fallback for unexpected types
  paste0(deparse(spec$value), collapse = " ")
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

### 4.9 Existing functions (unchanged behavior)

`grade_table()`, `grade_report()`, `print.pmatools()`, `summary.pmatools()` are unchanged in v0.2 except that `grade_table()` and `grade_report()` propagate the `convert_smd_to_or` and `baseline_risk` arguments to `sof_table()` internally.

---

## 5. Algorithm specifications

### 5.1 RoB inflation threshold (refined)

**v0.1.0 behavior:** when dominated, rate down if `(small_values = "undesirable" AND TE_all > TE_low)` OR `(small_values = "desirable" AND TE_all < TE_low)` OR `(small_values = NULL AND always conservative)`.

**v0.2.0 behavior:**

```
inflation_ratio = (|TE_all| - |TE_low|) / |TE_low|

direction_ok = match small_values:
  "undesirable":  TE_all > TE_low
  "desirable":    TE_all < TE_low
  NULL:           |TE_all| > |TE_low|         (use absolute distance from null)

rate_down = direction_ok AND (inflation_ratio > rob_inflation_threshold)
```

This means `small_values = NULL` is no longer "always rate down conservatively when dominated"; it now defers to the absolute distance from null. The behavior change is documented in the v0.2 release notes.

### 5.2 Inconsistency — BMJ Core GRADE 3 flowchart (v0.2)

> **Design rationale.** v0.2 preserves the BMJ Core GRADE 3 flowchart implemented in v0.1.0. The only enhancement is that **Step 2's clinical decision threshold uses ±MID** when supplied (instead of always using null = 0). I², τ², and Q-test are supplementary context only — they never drive the judgment in the manual flowchart path. PI is **not** used in the decision logic. Per-study CIs are also not used (CI overlap is judged clinically by the user in manual mode, or proxied by I² in auto mode).

**Algorithm:**

```
INPUT:
  meta_obj                          # {meta} object
  mid_internal                      # MID on TE scale, or NULL
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
           notes = "Step 2: important differences exist, but majority on one side of clinical threshold → do not rate down (per BMJ Core GRADE 3 flowchart)."
  }

  # opposite_sides → Step 3
  if (is.null(inconsistency_subgroup_explained)) {
    abort "inconsistency_threshold_side = 'opposite_sides' requires inconsistency_subgroup_explained"
  }

  if (inconsistency_subgroup_explained == "yes") {
    return judgment = "no", auto = FALSE,
           notes = "Step 3: opposite-sided estimates explained by credible subgroup; present subgroups separately."
  }

  return judgment = "serious", auto = FALSE,
         notes = "Step 3: opposite-sided estimates not explained by credible subgroup → rate down."
}

# ---- Path C: auto-detect ----

# Step 1 proxy: I² > 25%
ci_diff_yes <- (i2_pct > 25)

if (!ci_diff_yes) {
  return judgment = "no", auto = TRUE,
         notes = "AUTO Step 1: no important heterogeneity (I² ≤ 25%)."
}

# Step 2 proxy: depends on whether mid is provided
if (!is.null(mid_internal)) {
  # MID-based 3-zone classification of point estimates
  te_studies <- meta_obj$TE
  k <- length(te_studies)
  M <- mid_internal

  n_above   <- sum(te_studies > +M)
  n_below   <- sum(te_studies < -M)
  n_trivial <- k - n_above - n_below

  pct_one_side <- max(
    (n_above + n_trivial) / k,
    (n_below + n_trivial) / k
  )

  threshold_side <- if (pct_one_side >= 0.75) "majority_one_side" else "opposite_sides"
  threshold_label <- sprintf("vs ±MID = ±%g", M)
} else {
  # Fall-back: null=0 threshold (v0.1.0 behavior)
  pct_positive <- mean(meta_obj$TE > 0, na.rm = TRUE)
  threshold_side <- if (pct_positive >= 0.75 OR pct_positive <= 0.25) "majority_one_side" else "opposite_sides"
  threshold_label <- "vs null = 0"
}

# Auto judgment: more conservative than manual
# (auto Step 1 is statistical proxy, cannot confirm "important" clinically)
if (threshold_side == "majority_one_side") {
  return judgment = "some", auto = TRUE,
         notes = sprintf("AUTO Step 1: I² > 25% → important heterogeneity detected. AUTO Step 2 (%s): majority on one side → 'some' concern (auto path is conservative; supply manual flowchart params for BMJ-faithful 'no').", threshold_label)
}

# opposite_sides — Step 3 cannot be auto-checked
return judgment = "serious", auto = TRUE,
       notes = sprintf("AUTO Step 1: I² > 25% → important heterogeneity detected. AUTO Step 2 (%s): opposite-sided estimates; subgroup explanation not auto-detectable. Supply inconsistency_subgroup_explained = 'yes' to override.", threshold_label)
```

**Why the auto path differs from the manual path on `majority_one_side`:**

In the BMJ manual flowchart, "majority_one_side" → judgment = `"no"` because the user has clinically confirmed Step 1 ("important differences" is real heterogeneity, not just statistical noise). The auto path uses I² > 25% as a **statistical proxy** for Step 1; this proxy can flag heterogeneity that is not clinically important. Returning `"some"` instead of `"no"` is a conservative acknowledgment that the auto path cannot fully replace clinical judgment. Users who want the BMJ-faithful `"no"` should supply manual flowchart parameters.

**Edge cases:**

- `k < 2`: cannot assess inconsistency. Return judgment = `"no"` with note "k < 2; inconsistency not assessable."
- I² is NA (e.g., k = 1): Step 1 proxy returns FALSE → judgment = `"no"` with note "I² unavailable; cannot detect heterogeneity."
- All TE values equal (τ² = 0): I² will be 0 → Step 1 proxy returns FALSE → judgment = `"no"`.
- MID supplied but `mid_internal` cannot be derived (unknown sm): function aborts before reaching this domain.

**Judgment interpretation table:**

| Path | Step 1 / I² | Step 2 / threshold check | Step 3 / subgroup | Judgment |
|---|---|---|---|---|
| Manual | ci_diff = "no" | — | — | **No** |
| Manual | ci_diff = "yes" | majority_one_side | — | **No** |
| Manual | ci_diff = "yes" | opposite_sides | yes | **No** + note |
| Manual | ci_diff = "yes" | opposite_sides | no | **Serious** |
| Auto | I² ≤ 25% | — | — | **No** |
| Auto | I² > 25% | majority_one_side | — | **Some** *(more conservative than manual)* |
| Auto | I² > 25% | opposite_sides | n/a | **Serious** |

### 5.3 Chinn's formula (SMD ↔ OR)

```
factor = π / √3 ≈ 1.81380
log(OR) = SMD * factor
OR     = exp(SMD * factor)

SMD = log(OR) / factor
```

CI bounds use the same multiplication. Document in `?chinn_smd_to_or` that the conversion assumes the latent-variable/logistic distribution (Cox 1970, Hasselblad & Hedges 1995, Chinn 2000).

### 5.4 MID auto-default per `sm` (suggested defaults)

When the Shiny app pre-fills the MID input, use these **conventional defaults** based on `meta_obj$sm`. The user can always override.

```r
suggest_mid <- function(meta_obj) {
  sm <- meta_obj$sm
  switch(sm,
    "OR"  = list(mid_user = 1.25,  mid_scale = "ratio"),     # log(1.25) ≈ 0.223
    "RR"  = list(mid_user = 1.20,  mid_scale = "ratio"),     # log(1.20) ≈ 0.182
    "HR"  = list(mid_user = 1.20,  mid_scale = "ratio"),
    "RoM" = list(mid_user = 1.10,  mid_scale = "ratio"),     # 10% ratio of means
    "ARD" = list(mid_user = 0.05,  mid_scale = "ard"),        # 5% absolute risk diff
    "SMD" = list(mid_user = 0.20,  mid_scale = "te_scale"),   # Cohen's small
    "MD"  = {
      sd_pooled <- compute_pooled_sd(meta_obj)
      list(mid_user = 0.20 * sd_pooled, mid_scale = "te_scale")  # 0.2 * pooled SD
    },
    NULL  # unknown sm → no default
  )
}
```

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

`suggest_mid()` and `compute_pooled_sd()` are **exported** (so Shiny can pre-fill the input). The user may also call them directly from R.

**Behavior in `grade_meta()`:**

`grade_meta()` itself does **not** auto-fill `mid` — passing `mid = NULL` triggers the I²-fallback path explicitly. The Shiny app calls `suggest_mid()` to pre-fill the input field, but the *value* the user sees is what gets passed (override-able). This keeps `grade_meta()`'s behavior deterministic and reproducible: no hidden defaults at the R API level.

**MID conversion to TE scale** (used internally by `assess_inconsistency()` and `assess_imprecision()`):

```r
mid_to_te_scale <- function(mid, mid_scale, sm) {
  if (is.null(mid)) return(NULL)

  scale <- if (mid_scale == "auto") {
    switch(sm,
      "OR" = "ratio", "RR" = "ratio", "HR" = "ratio", "RoM" = "ratio",
      "ARD" = "ard",
      "SMD" = "te_scale", "MD" = "te_scale",
      rlang::abort(sprintf("Cannot auto-detect mid_scale for sm = %s", sm))
    )
  } else mid_scale

  switch(scale,
    "te_scale" = mid,                # already on TE scale
    "ratio"    = log(mid),            # convert ratio → log
    "ard"      = mid                  # ARD: keep as-is, but special handling in OIS
                                       # (ois uses p1 = p0 + ARD)
  )
}
```

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
| test-inconsistency_mid.R *(new)* | manual flowchart (3 paths: ci_diff=no / majority_one_side / opposite_sides×subgroup); auto Step 1 = I² > 25% only (Q-test no longer used); auto Step 2 with MID (3-zone classification, ≥75% threshold); auto Step 2 without MID (null=0 fallback, v0.1.0 behavior preserved); auto vs manual asymmetry on majority_one_side |
| test-mid_scale.R *(new)* | `mid_scale = "auto"` correctly maps OR/RR/HR/RoM → log, MD/SMD → te_scale, ARD → ard; abort on unknown sm |
| test-suggest_mid.R *(new)* | defaults match table for OR/RR/SMD/MD/ARD/RoM; MD default = 0.2 × pooled SD; unknown sm returns NULL |
| test-chinn.R *(new)* | numerical accuracy of factor π/√3, NA propagation, sof_table integration with convert_smd_to_or = TRUE |

---

## 7. Edge cases

| Case | Expected behavior |
|---|---|
| `ingest_data` with empty data.frame | abort with "no rows" |
| `ingest_data` long with one studlab appearing 3 times | abort with "studlab X has 3 rows; expected 2" |
| `run_ma` with k = 1 | return meta object; `hakn`/`prediction` set FALSE; warn "single study; CI may be unreliable" |
| `run_ma` with k = 2 | hakn FALSE, prediction FALSE; otherwise normal |
| `plot_funnel` with k < 10 | render plot; annotate "Egger's test not run (k < 10)" |
| `grade_meta` with `mid = -3` (negative) | abort with "mid must be positive (it is treated as a half-width around null)" |
| `grade_meta` with `mid` AND `ois_p0/p1` both supplied | use `ois_p0/p1`; note "MID provided but explicit ois_p0/p1 takes precedence" in domain notes |
| `sof_table(convert_smd_to_or = TRUE, baseline_risk = NULL)` | abort with informative message |
| `export_bundle` to non-writable directory | abort with file-system error |
| `export_bundle` with `include = c("data")` only | ZIP contains only `data_long.csv`; no analysis.R |

---

## 8. Performance & resource

- `metaprop` baseline-risk pooling can be slow for large k. The function falls back to `"simple"` after 10 seconds (existing v0.1.0 behavior).
- ZIP creation should complete in < 5 seconds for k ≤ 100.
- `plot_forest()` should render in < 2 seconds for k ≤ 50.
- Memory: assume the `meta` object fits in memory; no streaming required.

---

## 9. Documentation deliverables

| File | Audience | Content |
|---|---|---|
| `README.md` (updated) | First-time users | install_github, 30-line quick start (CLI + Shiny URL), feature highlights |
| `vignettes/pmatools_cli.Rmd` (new) | R users | End-to-end CLI workflow on bundled `cbti_depression.csv`; explain MID, inflation threshold, Chinn |
| `PLAN.md` (updated) | Maintainer | v0.2 scope, what's done / not done, v0.3 roadmap |
| `SPEC.md` (this file) | Maintainer + AI implementers | Authoritative spec |
| roxygen man pages | Function-level reference | `?grade_meta`, `?sof_table`, etc., kept in sync with SPEC |

---

## 10. Backward compatibility checklist

- [ ] All v0.1.0 example code in `sample.R` continues to run unchanged with v0.2.0 installed
- [ ] All existing `tests/testthat/test-grade_meta.R` cases pass without modification (or with clearly-documented adjustments for the two intentional behavior changes below)
- [ ] `grade_meta(..., rob = "some")` returns identical output in v0.1.0 and v0.2.0
- [ ] `sof_table(g)` (default args) returns identical flextable in v0.1.0 and v0.2.0
- [ ] `grade_report(...)` produces identical docx structure (allowing minor stylistic differences from flextable updates) in v0.1.0 and v0.2.0

**Intentional behavior changes (documented in CHANGELOG):**

1. **RoB `small_values = NULL` path (§5.1):** v0.1.0 always rated down conservatively when dominated; v0.2.0 only rates down when relative inflation exceeds `rob_inflation_threshold`. Set `rob_inflation_threshold = 0` to restore v0.1.0 behavior.
2. **Inconsistency auto Step 1 (§5.2 Path C):** v0.1.0 used `I² > 25% OR Q p < 0.10`; v0.2.0 uses `I² > 25%` only. Q-test is supplementary in notes only. This may shift edge-case judgments where I² ≤ 25% but Q p < 0.10 (small k with extreme outliers); typically rare in practice.

---

## 11. Out of scope (v0.3+)

- GRADE upgrade domains (large effect, dose-response, plausible confounding)
- GRADEpro JSON export/import
- Multi-outcome session in Shiny
- shinyapps.io deployment automation in pmatools (kept in pairwise_meta_analysis)
- Internationalization
- CRAN submission

---

## 12. References

- Guyatt G, et al. *Core GRADE 1: Overview*. BMJ 2025. PMID: 40262844.
- Guyatt G, et al. *Core GRADE 3: Inconsistency*. BMJ 2025. PMID: 40328467.
- Guyatt G, et al. *Core GRADE 4: Risk of bias, publication bias*. BMJ 2025. PMID: 40360206.
- Guyatt G, et al. *Core GRADE 5: Indirectness*. BMJ 2025. PMID: 40393729.
- Chinn S. *A simple method for converting an odds ratio to effect size for use in meta-analysis.* Stat Med. 2000;19(22):3127-3131.
- Nikolakopoulou A, et al. *CINeMA: An approach for assessing confidence in the results of a network meta-analysis.* PLoS Med. 2020;17(4):e1003082.
- Hasselblad V, Hedges LV. *Meta-analysis of screening and diagnostic tests.* Psychol Bull. 1995;117(1):167-178.
