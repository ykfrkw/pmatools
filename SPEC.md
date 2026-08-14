# pmatools — Package Specification

> Authoritative specification for the pmatools R package. Implementation MUST conform to this document. UI-side concerns (Shiny wizard, educational copy, accordion layout) are specified separately in [`shiny/SPEC.md`](shiny/SPEC.md), which governs the app in this repository's `shiny/` directory.

**Version target:** 0.5.1
**Document history:** this file was written for v0.2.0 and has been updated in place for v0.5.0 and v0.5.1; section numbering is preserved so the diff stays readable. Sections that still describe v0.2 behaviour verbatim are marked **[v0.2 — superseded]** with a pointer to the section that governs.

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

It is **Shiny-agnostic**. All functions return plain R objects (tibbles, base graphics, flextables, file paths). The Shiny wizard under `shiny/` is a consumer like any other: it calls pmatools functions and nothing here may depend on it.

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
| `rob` | chr | Risk of bias label (Cochrane RoB 2, ROBINS-I or GRADE level; §4.11 `rob_strata()`). Per-study. |
| `indirectness` | chr | Indirectness label (`low` / `some` / `high`; read through the same `rob_strata()` table). Per-study. |
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

#### 4.1a `detect_column_roles()` [v0.5.1]

```r
detect_column_roles(data) -> data.frame   # data: a data.frame, or column names
```

Reports which column filled each of `ingest_data()`'s long-format roles,
without ingesting anything. One row per role, in role order, with columns
`role`, `column` (the source column, `NA` when unfilled), `matched_by`
(`"canonical"` / `"alias"` / `NA`), `found` and `required`.

The roles, and the aliases accepted for each, are `PMA_INGEST_ROLE_ALIASES` —
the **single** definition. `.resolve_role_names()` renames by it during ingest
and `detect_column_roles()` reports by it, so a role reported as filled is a
role `ingest_data()` fills. Resolution order matters and is shared: a canonical
name beats an alias, the first alias listed wins among several present, and a
column claimed by one role is invisible to later ones (`group` is an alias of
both `treat` and `subgroup`; `treat` claims it).

| role | required | aliases |
|---|---|---|
| `studlab` | yes | `study`, `id`, `study_name`, `study_id`, `trial`, `trial_id` |
| `treat` | yes | `treatment`, `arm`, `t`, `intervention`, `group`, `condition` |
| `n` | yes | `n_randomized`, `n_total`, `sample_size`, `N` |
| `event` | no | `events`, `d_r`, `responders`, `n_events` |
| `mean` | no | `means` |
| `sd` | no | `stdev`, `stddev` |
| `outcome` | no | — (recognised, never renamed into) |
| `rob` | no | `risk_of_bias`, `rob_d`, `rob_overall`, `rob_judgment`, `rob_judgement` |
| `indirectness` | no | `indir` |
| `subgroup` | no | `group`, `stratum` |

`required` is what `.validate_long()` aborts without. Long format only: wide
input has fixed canonical pair names and no alias mechanism to report on.

Exists for host applications: the Shiny app's Step 1 states which column was
recognised as what (`shiny/SPEC.md` §3.2.3), which is the check an upload most
often fails.

### 4.2 `run_ma()` [new]

```r
run_ma(
  data,                                          # output of ingest_data()
  outcome_type = c("binary", "continuous"),
  sm           = NULL,                            # NULL → "OR" if binary, "SMD" if continuous
  method       = NULL,                            # NULL → "Inverse" if binary, irrelevant if continuous
  method.tau   = c("REML", "PM", "DL", "SJ", "ML", "EB"),
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

**`hakn` — the random-effects confidence interval:**

`hakn` is translated to `{meta}`'s `method.random.ci`: `TRUE` → `"HK"`, `FALSE` → `"classic"`.

| `hakn` | behaviour |
|---|---|
| `NULL` (default) | `TRUE` when `k >= 3` **and** `random`, else `FALSE` |
| `TRUE` | Hartung-Knapp, whatever `k` is. Below `k = 3` it warns — the interval is very wide there — and applies it anyway |
| `FALSE` | classic (Wald), whatever `k` is |

`prediction` keeps its own independent `k >= 3 && random` rule; the two are not coupled.

**One outcome per study:**

An `outcome` column is descriptive, not an analysis partition key. Several outcome labels across the data set are fine — a continuous review where each study used its own instrument (PHQ-9 / HAMD / BDI) is exactly what `sm = "SMD"` is for, and all of those studies pool together. `run_ma()` aborts only when a single `studlab` carries more than one distinct outcome, which would count that study twice; the message names the offending study labels (first five, then "and N more"). `.long_to_wide()`'s "exactly one intervention and one control arm" check remains the last line of defence.

**Implementation:**

- Pivot canonical long → wide internally for `metabin/metacont` (which take wide input via `event.e/event.c/n.e/n.c` etc.).
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

**`title` placement (v0.5.1).** The title is drawn on its own line(s) above the
column headers, word-wrapped to the device width. It is **not** passed to
`meta::forest()` as `smlab`: `{meta}` draws `smlab` inside the header row,
centred over the forest column, so a title wider than that column overruns the
neighbouring header cells and renders as `EvenDepression response…GR (95% CI)`.
Titles are outcome names chosen by the caller, so no length bound holds;
`{meta}` also refuses an `smlab` of more than two lines, so wrapping it in
place does not generalise either.

`plot_forest()` therefore passes `smlab = ""` and draws the title **after**
`meta::forest()` returns, anchored to the top of the block that call reports as
`figheight$total_height`. `{meta}` sizes its block to the device and centres it
vertically, so reserving a band up front instead would shrink the region it
centres in without shrinking the block, stranding the title above a large gap
on a tall canvas. Failing to measure degrades to the device top rather than
dropping the title.

This covers `plot_forest_rob()` and `plot_forest_indirectness()` too — both
delegate — so the suffix a stratified plot appends needs no length budget.

### 4.3a `plot_forest_rob()` — stratified by risk of bias

```r
plot_forest_rob(meta_obj, rob, some_concerns_as = NULL, ...)
```

Re-runs the analysis with a risk-of-bias subgroup and draws the per-stratum pooled estimates beside the overall one. `rob` accepts the whole `grade_meta()` vocabulary, normalised by the exported `rob_strata()` — the single place that vocabulary is defined. `...` goes to `plot_forest()`.

**`some_concerns_as` (v0.5.1) selects the grouping:**

| value | strata | `subgroup.name` |
|---|---|---|
| `NULL` (default) | `low` / `some` / `high` / `unknown` — the four descriptive strata | `"Risk of bias"` |
| `"low"` / `"high"` | `Low risk of bias` / `High risk of bias` — **two** groups | `"Risk of bias (as analysed)"` |

The two-group fold is not a second implementation: `.rob_analysis_strata()` maps the plot strata back to the internal levels and asks **`.rob_high_levels()`**, the same internal `assess_rob()` consults for `rob_some_concerns`. The argument is named after `grade_meta(rob_some_concerns =)` for that reason, and a study on the high side of the plot is by construction a study on the high side of the rating. Unrated studies follow whichever side "some concerns" takes, matching `grade_meta()`, where an unrated study arrives as `"*"` and normalises to `serious`.

Both levels are always present in the factor, even when one is empty, so the subgroup rows do not reorder as the boundary moves.

**Why this exists.** With the four descriptive strata the plot splits studies four ways beside a judgment made on two, so under the common `rob_some_concerns = "high"` setting the figure and the evaluation printed next to it disagree. Any caller that shows the plot alongside a rating should pass the same value it passed to `grade_meta()`. `NULL` keeps the pre-0.5.1 behaviour exactly, so this is an additive change.

Folding at the call site (handing pre-collapsed labels to `rob`) is **not** supported: `rob_strata()` warns on any label outside its vocabulary and buckets it to `"unknown"`.

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

**Mandatory rationales (v0.4.0, breaking).** Supplying a scalar `rob`, an `indirectness` other than `"not_serious"`, an `inconsistency`, an `imprecision`, a `pubias_funnel_asymmetry`, a manual `rating_target`, or any `rob_overrides` **without** the matching `*_rationale` argument is an error. Rationales are stored in the domain notes and surfaced by `sof_table()`, `grade_report()` and `export_bundle()`.

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
| `$control_risk` (v0.5.1) | how the one control-arm risk was shared across `threshold_baseline` / `ois_p0` / `baseline_risk`: `value`, `donor`, `inherited`, `note`, and `used` (the number each of the three ended up with). See §4.5.4 |

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

#### 4.5.4 One control-arm risk, three arguments [v0.5.1]

`threshold_baseline`, `ois_p0` and `baseline_risk` are three names for the same quantity — the control-arm event rate — consumed by three different calculations:

| Argument | What it does with the number | §|
|---|---|---|
| `threshold_baseline` | converts an absolute (ARD) threshold to the analysis scale | §4.5 |
| `ois_p0` | the control rate the Optimal Information Size is powered from | §5.5 |
| `baseline_risk` | the control rate the Summary of Findings table prints | §4.6 |

**The number is supplied once.** `grade_meta()` resolves the three before any domain runs, in this order:

1. **An argument that was supplied keeps its own value.** Always. An explicitly passed value is never displaced by an inherited one.
2. **An argument left `NULL` takes the first value supplied to any of the others**, in the order `threshold_baseline`, `ois_p0`, `baseline_risk`.
3. **An argument still `NULL` falls back to the pooled control event rate** of the analysis being rated — its own pre-existing default, computed on the low-RoB refit when one happened.

The donation order is not arbitrary. `threshold_baseline` is the risk of the population the decision threshold is about, and the app makes the reviewer confirm it or justify a replacement in writing (`shiny/SPEC.md`, "Control-group risk is a whole number of events"); `ois_p0` is Core GRADE 2's "control group event rate (chosen from the context)"; `baseline_risk` is presentational. So the most deliberate value donates first and the most presentational last.

**The three can legitimately differ, and rule 1 is what protects that.** A Summary of Findings table is routinely drawn against a named risk group — a high-risk stratum, a registry rate for the population the guideline addresses — while the OIS is powered from the trials' own control arms, and an ARD threshold is converted at whichever rate makes the threshold interpretable. Rule 1 means any of the three can be pinned to its own number while the rest share one.

**Only a number in (0, 1) is inherited.** `baseline_risk` accepts the closed interval `[0, 1]`; `threshold_baseline` rejects `0` and `1` outright. Donating an edge value would turn a working call into an abort somewhere else, so it stays where it was put. A **character** `baseline_risk` (`"simple"` / `"metaprop"`) names a computation rather than a value and does not donate at all: each use already performs that computation on the analysis *it* is judging.

**Which one won is recorded, in two places.** `$control_risk` carries `value`, `donor`, `inherited`, `note` and `used` — the number each of the three uses ended up with, after its own pooled default has run, and `NULL` for a use that never needed one (a threshold that was not on the absolute scale, or a continuous outcome); and the sentence in `note` is appended to the **Imprecision** domain notes, so it reaches `summary()`, the Evidence Profile, `grade_report()` and the exported bundle without the reader having to see the call. Nothing is appended when nothing was inherited.

**`export_bundle()` pins all three.** The bundled `analysis.R` emits `threshold_baseline`, `ois_p0` and `baseline_risk` as literals taken from `$control_risk$used`, so the re-run reproduces the rating instead of re-deriving a baseline of its own. Emitting only two of them would let the third inherit on the re-run — visible whenever `baseline_risk` was a pooling method or a named risk group.

**Why three arguments and not one.** Consolidating onto a single `baseline_risk`, with the two calculations that need a *different* number taking an explicit override, remains the destination. It is a breaking rename of three public arguments, and v0.5.1 already carries the breaking rename of the domain judgment vocabulary (`NEWS.md`). Stacking a second migration on one release would cost callers two passes over their scripts for one release's benefit, and the complaint that prompted this — having to pass the same number twice — is a call-site complaint that the mutual fallback answers on its own. The consolidation is deferred, not abandoned; `.resolve_control_risk()` in `R/utils.R` is where it will land.

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
  NO  → judgment = "not_serious"  (do not rate down)
  YES → continue to Step 2

Step 2: Where do the point estimates fall relative to the clinical decision Threshold?
  Majority on one side of Threshold → judgment = "not_serious"  (do not rate down)
  Substantial proportion on opposite sides → continue to Step 3

Step 3: Is the opposite-sided inconsistency explained by a credible subgroup analysis?
  YES → judgment = "not_serious" + note "present subgroups separately"
  NO  → judgment = "very_serious"
```

The "clinical decision Threshold" in Step 2 is **null = 0 by default**, but **±Threshold** when `threshold` is supplied. This is the v0.2 enhancement.

**Three input paths (preserved from v0.1.0):**

**Path A — Scalar override:**

```r
grade_meta(m, inconsistency = "very_serious",
           inconsistency_rationale = "Panel judgment")
# → judgment = "very_serious", auto = FALSE
```

**Path B — Manual flowchart (full BMJ-faithful):**

```r
grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "majority_one_side"
)
# → judgment = "not_serious"  (Step 2: majority on one side → do not rate down)

grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "opposite_sides",
  inconsistency_subgroup_explained = "no"
)
# → judgment = "very_serious"

grade_meta(m,
  inconsistency_ci_diff            = "yes",
  inconsistency_threshold_side     = "opposite_sides",
  inconsistency_subgroup_explained = "yes"
)
# → judgment = "not_serious" + note "present subgroups separately"
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

  if (pct_max_zone  >= ZONE_MAJORITY)  → "majority_one_side"    → "not_serious"
  else if (pct_each_side >= OPPOSITE_EACH) → "opposite_substantial" → "very_serious"
  else                                  → "heterogeneous"       → "serious"

  ZONE_MAJORITY = 0.80   # CINeMA (Nikolakopoulou 2020); Core GRADE 3 Fig 2
                         # says only "Majority are on one side of threshold"
  OPPOSITE_EACH = 0.20   # pmatools convention; Core GRADE 3's phrase is
                         # "substantial proportion", with no number

Step 3:
  Subgroup credibility cannot be auto-checked. Core GRADE 3 keys it to the
  interaction P value, within-study vs between-study comparison, and a small
  number of direction-specifying a priori hypotheses, assessed with ICEMAN
  (Schandelmaier S, et al. CMAJ. 2020). Supply
  inconsistency_subgroup_explained = "yes" to take the credible-subgroup
  branch. NOTE: Core GRADE 3 says "a conclusion of moderate or high
  credibility warrants the creation of separate PICO questions for each
  subgroup", so the faithful response is to split the analysis, not to keep
  reporting the pooled estimate that this branch lets through.
```

**Auto judgment outputs:**

| Auto path outcome | Auto judgment | Manual flowchart equivalent |
|---|---|---|
| ci_diff_yes = FALSE | `"not_serious"` | `"not_serious"` (same) |
| ci_diff_yes & majority_one_side | `"not_serious"` | `"not_serious"` (same) |
| ci_diff_yes & opposite_substantial | `"very_serious"` | `"very_serious"` (same, modulo Step 3) |
| ci_diff_yes & heterogeneous | `"serious"` | — (no manual counterpart) |

**The opposite-sided branch rates down two levels, and this departs from the source.** Core GRADE 3 (p5–6) says a compelling reason to rate down twice for inconsistency is "sufficiently unusual that it need not concern users of Core GRADE", and v0.5.0 read that as a cap. pmatools no longer does. The branch is not "studies disagree by more than the eye likes"; it is the narrow case where a substantial share of estimates sits **above** the chosen threshold and a substantial share sits **below** it, and no credible subgroup explains the split — the reviewer cannot say which direction the intervention works in. One level would leave a body of evidence at Moderate while the sign of the effect is unresolved, which overstates it. Core GRADE 3 calls the case unusual rather than impossible, and pmatools' 20%-each-side gate is exactly what makes it unusual: the ordinary disagreements land on `heterogeneous`, which still rates down one.

`"heterogeneous"` (a scattered tally with no substantial opposite mass) stays at `"serious"` (−1), and so does every risk-of-bias path — the cap that was removed here was never a general rule.

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
  + the two-level departure note (.INCONSISTENCY_TWO_LEVEL_NOTE)
{{/if}}
{{/if}}
| I2 = {{i2_pct}}%, tau2 = {{tau2}}, Q p = {{q_p}} (supplementary; not the primary criterion)
```

`{{threshold_label}}` is `"vs ±Threshold = ±{{threshold_internal}}"` when Threshold is supplied, otherwise `"vs null = 0"`.

**`threshold` semantics — Imprecision:**

> **[v0.2 — superseded]** As of v0.5.0 Imprecision follows the Core GRADE 2 Fig 4 flowchart, in which the Optimal Information Size is consulted **only** when the CI does not cross the chosen threshold *and* the effect is implausibly large. **§5.5 is authoritative.** What follows describes only how `threshold` seeds the OIS inputs when that branch is reached.

In `assess_imprecision()`, when no explicit `ois_*` is provided:

- **Binary (v0.5.0): `ois_p1 = ois_p0 * (1 ∓ ois_rrr)`, default `ois_rrr = 0.20`.** The MID is *not* used. (v0.5.1: the sign follows the outcome direction and the observed effect — §5.5, "OIS inputs".) Core GRADE 2 (p6): "For binary outcomes, these involve specifying the acceptable error rates: α (typically 0.05) and β (typically 0.20), the control group event rate (chosen from the context), and **a modest relative risk reduction, typically 20% or 25%**." `ois_p0` is resolved before any domain runs: an explicit value, else whatever `threshold_baseline` or `baseline_risk` supplied, else the pooled control-arm rate (§4.5.4).
- Continuous (MD): `ois_delta = threshold_internal` (raw outcome units) — the same paragraph writes the continuous case out separately and *does* send it to the MID ("by specifying the smallest difference between intervention and control that one would want to avoid missing (ie, the MID)").
- Continuous (SMD): `ois_delta = threshold_internal` as well — the SMD threshold is *already* in standardized units, so it goes into the formula unchanged and the SD that accompanies it is 1 (below). Multiplying delta by the pooled SD and taking `ois_sd = 1` would give the same `n`; the implemented formulation is delta-unchanged, sigma-one.
- Continuous (v0.5.1): `ois_sd = compute_pooled_sd(meta_obj)` when the caller supplies none — **except for SMD, where `ois_sd = 1`** *(see §5.4 for pooled_SD computation)*. An explicitly supplied `ois_sd` always wins.

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

**Arm-level columns for continuous outcomes [v0.5.1].** Both layouts previously drove their two arm cells off `baseline_risk`, which is meaningful only for a binary outcome with a relative effect measure, so a `metacont` object filled them with `-`. As of v0.5.1 the control cell is the **inverse-variance weighted mean of the control arms** (weights `n / SD²`, the reciprocal of the variance of each control mean) and the intervention cell is that value plus the pooled difference, its interval coming from the pooled difference alone. The control mean is pooled with fixed weights whatever model produced the effect estimate: honouring the parent model's `random` setting would need a τ² for the between-study distribution of arm-level means, a different quantity from the contrast-level τ² that model estimated. An **SMD** is multiplied by the pooled within-arm SD of the control arms (Cochrane Handbook 15.5.3.2) before being added, since SD units cannot be added to a mean on the original scale; the Difference column keeps the SMD in SD units and does **not** borrow the outcome's `unit` there. Both derivations are footnoted. In the GRADEpro layout the arm headers fall back to "With control" / "With intervention" when the cells hold means, because the rate wording and the `per` denominator would misdescribe them; **binary tables are byte-for-byte unchanged**. The Chinn dichotomisation keeps its own cells and its own footnote.

**Per-domain rate-down footnotes [v0.5.1].** `sof_table()`, `grade_table()` (both layouts) and `evidence_profile()` render the structured domain facts of §4.15 as numbered footnotes for the domains that pulled the rating down, with the marker on the certainty cell — after the symbol in the GRADEpro layouts, and beside the domain name inside the BMJ "Due to serious risk of bias [1] …" sentence. In `grade_table()` these continue the same `[n]` register as the per-outcome analysis-set notes and name the outcome they belong to, so one footer never shows two different `[1]`s; the analysis-set and publication-bias sentences keep their existing numbering and wording.

**Not-reported outcomes.** `sof_table()` **aborts** on a `pmatools_not_reported` (§4.14) with a message pointing at `grade_table()`.

**Citation style [v0.5.1].** Every bibliographic reference pmatools renders — flextable footnotes, the `.docx` header paragraphs of `grade_report()` and `export_bundle()`, and the caveat strings that reach `notes` — is written in one house style: **first author, `et al.`, journal abbreviation, year**. No volume, no pages, no DOI, no URL. The six BMJ 2025 Core GRADE papers defeat the bare form (all Guyatt, all BMJ, all 2025), so a specific paper carries its series number as a prefix — `Core GRADE 4. Guyatt G, et al. BMJ. 2025` — and the series as a whole is `Core GRADE series. Guyatt G, et al. BMJ. 2025`. Both shapes come from the internal `.core_grade_ref()` in `R/utils.R`, and the disclaimer that follows the series citation on every table is the single constant `.PMA_CORE_GRADE_FOOTNOTE` (`"Reference: … . Not an official GRADE Working Group assessment."`), which replaced eight literals wording it four ways. Short parentheticals that point at a figure rather than a paper — "(Core GRADE 4 Fig 2)", "(Core GRADE 5 Table 2)" — are pointers, not citations, and are unchanged.

**Table typography [v0.5.1].** Every flextable pmatools builds — `sof_table()` (both layouts), `grade_table()` (both layouts), `evidence_profile()`, `indirectness_table()` and the domain-detail table inside `grade_report()` — is set in one family, the internal constant `.PMA_TABLE_FONT` (`"Arial"`). Arial is chosen for the **document**: these tables are built to be dropped into a .docx, where a named face beats a stack the word processor cannot resolve. The Shiny app restyles the *screen* copy to the page font in CSS instead (`shiny/SPEC.md` §4.1); the exported document is unaffected.

Footer notes are 8pt `#555555`, applied through `.style_table_footer()`. That helper re-applies the family as well as the size and colour, because `add_footer_lines()` creates its rows *after* `font(part = "all")` has run and a new row falls back to flextable's own default (Helvetica) rather than inheriting the table's — so a footer set before v0.5.1 rendered in a different face from the body it annotated. Host applications appending their own notes (`sof_add_notes()`, §4.8) go through the same helper and inherit the same guarantee.

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

**The conversion is a presentation, not a rating input.** `convert_smd_to_or` reaches `sof_table()` and `grade_table()` and nothing else — `grade_meta()` never sees it, and Imprecision is rated on the SMD/MD against `threshold_cont` whichever way this argument is set. The package default is and stays `FALSE`; as of the Shiny app's `input$sof_presentation` radio the app default matches it, where the app previously defaulted the conversion on.

**Per-row, in a combined table [v0.5.1].** `sof_table()` takes the four arguments above, which is right for a table of one row. `grade_table()` — the only Summary of Findings a multi-outcome bundle carries — reads the same four **per row** off each rated object's `"pmatools_display"` attribute (§4.8, `PMATOOLS_RESPONDER_FIELDS`), so one continuous outcome can be shown as responders while another is shown as its effect and a binary one is untouched. They ride on the attribute rather than in `grade_meta_multi()`'s `common` / `per_outcome` because `grade_meta()` takes none of them and its own `baseline_risk` means the control-arm event rate, not the proportion of control patients who respond. Details in §4.9.

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

### 4.7c `format_effect()` [helper, exported]

```r
format_effect(
  meta_obj,                  # meta object (run_ma(), metabin(), metacont(), ...)
  outcome_type,              # "relative" (ratio measures) | "absolute"
  prediction = FALSE
) -> chr(1)
```

The **exact** string `sof_table()`, `grade_table()` and `grade_report()` put in their Effect column: `sprintf("%s %.2f (%.2f; %.2f)", sm, est, lo, hi)`, e.g. `"RR 0.55 (0.38; 0.79)"`. Exported so a caller building its own view of the same analysis renders the effect to the same wording and precision instead of re-deriving it.

- `outcome_type == "relative"` **and** `sm %in% c("RR", "OR", "HR", "IRR")` → estimate and CI are exponentiated. Any other combination is printed as-is, with `"Effect"` substituted for a `NULL` `sm`.
- Model selection follows the object: random-effects pool when `meta_obj$random` is `TRUE`, else the common-effect pool, with a fallback to the other model when the preferred one was not fitted (a finite scalar `est` is the test).
- `prediction = TRUE` appends `"\nPrI (lo; hi)"` when the object carries a prediction interval, exponentiated under the same rule.
- No usable pooled estimate → `"NR"`.

`.format_effect()` remains as an internal alias so existing call sites in `sof_table.R`, `grade_table.R` and `grade_report.R` did not move. `sof_bmj.R` has its own `.format_effect_bmj()`, which spells the measure out ("Odds ratio 2.33 (1.66 to 3.26)") and is **not** this function.

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
  sof_notes    = NULL,                     # v0.5.1: extra footnotes for sof_table.docx
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

**`sof_notes` (v0.5.1).** Extra footnote lines for the bundled Summary of Findings table, appended by the exported `sof_add_notes(x, notes)` after the table's own footnotes and in the same 8pt grey styling, then rendered into `analysis.R` as a `sof_add_notes()` call so the script reproduces the annotated table. `NULL`, `NA` and empty entries are dropped, and a bundle with no usable note renders no call at all. The `pmatools_set` method takes the same argument for `summary_of_findings.docx`. Neither applies it to the certainty appendix (`grade_report()` has no notes hook). Its purpose is annotations pmatools cannot derive — a host application's rare-event alert, a scope caveat, a registration number — which previously forced such callers to write the .docx themselves outside the bundler.

**`style` (v0.5.1).** Forwarded to `sof_table()` for `sof_table.docx` and to `grade_report()` for the certainty appendix — one layout per ZIP — and rendered into the generated `analysis.R`, so re-running the script reproduces the layout that was exported rather than the `sof_table()` default. `follow_up` / `unit` are the BMJ layout's presentation arguments (§4.6); each falls back to the field of the same name on the rated object, which is where `grade_meta_multi()` stores it. The default is `"bmj"`, matching §4.8.3: **both bundle methods** default to the Core GRADE layout, while `sof_table()` and `grade_table()` themselves keep `"gradepro"`. Before v0.5.1 this method had no `style` and always wrote GRADEpro.

**ZIP contents — flat, no sub-directories** (only the requested `include` items appear):

```
{bundle_name}.zip
├── data_long.csv
├── analysis.R
├── results.txt
├── forest_plot.pdf                     (width = max(7, 3 + 0.3*k))
├── forest_plot_rob.pdf                 "forest_rob"; needs `rob`
├── funnel_plot.pdf
├── funnel_trimfill.pdf                 "funnel_trimfill"
├── pubias_missing_forest.pdf           "pubias_missing_forest"; rendered only when k >= 10
├── grade_table.docx                    SoF table (single outcome → 1 row)
├── indirectness_table.docx             when subdomain judgments were recorded
├── rare_event_diagnostics.csv          when `rare` is supplied
├── rare_event_method_table.csv         when `rare` is supplied
└── rare_event_method_forest.pdf        when `rare` is supplied
```

**Plots are PDF only (v0.5.1).** Every plot used to ship twice, as a PDF and as a
raster PNG of the same figure. The PNG was the lower-fidelity copy of the two and
nothing in the bundle referenced it, so it doubled the plot count for no reader who
could not open the PDF. The generated `analysis.R` writes PDF only for the same
reason.

A renderer that fails warns and is skipped rather than aborting the whole bundle.

#### 4.8.3 Multi-outcome layout (`pmatools_set` method)

```r
export_bundle(
  x,                                      # pmatools_set from grade_meta_multi()
  output_dir  = ".",
  bundle_name = "pmatools_results",
  include     = c("data", "script", "results", "forest", "forest_full",
                  "forest_rob", "funnel", "funnel_trimfill",
                  "pubias_missing_forest", "sof", "evidence_profile",
                  "indirectness", "readme"),
  style       = c("bmj", "gradepro"),      # as of v0.5.1 the meta method matches this default
  per         = 1000,
  prediction  = FALSE,
  rob         = NULL,                      # named list by outcome, or one vector for all
  forest_display      = NULL,
  forest_display_rob  = NULL,
  rare                = NULL,              # pma_rare_meta from run_rare_ma()
  rare_forest_display = NULL,
  pubias_missing_df   = NULL,
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
    │   ├── forest_plot.pdf                 the analysis actually rated
    │   ├── forest_plot_full.pdf            only when a low-RoB refit happened
    │   ├── forest_plot_rob.pdf             only when RoB labels are known
    │   ├── funnel_plot.pdf
    │   ├── funnel_trimfill.pdf             "funnel_trimfill"; only when k >= 10
    │   ├── pubias_missing_forest.pdf       "pubias_missing_forest"; only when k >= 10
    │   ├── results.txt
    │   ├── data_long.csv                   this outcome only
    │   ├── evidence_profile.docx
    │   ├── indirectness_table.docx         only when subdomains were recorded
    │   ├── rare_event_diagnostics.csv      when this outcome carries a `rare` fit
    │   ├── rare_event_method_table.csv     "
    │   └── rare_event_method_forest.pdf    "
    └── 02_<slug>/ ...
```

Directory names carry the set order as a zero-padded numeric prefix. A non-ASCII outcome name falls back to `outcome_NN`, so the ZIP stays portable.

**A one-outcome set gets the same tree**, with a single `outcomes/01_<slug>/`. There is no flat fallback: the layout a reader learns from one bundle is the layout of the next.

**Per-outcome display arguments [v0.5.1].** `rob`, `forest_display`, `forest_display_rob`, `rare`, `rare_forest_display` and `pubias_missing_df` describe **one analysis**. A set built by `grade_meta_multi()` in one call can answer for all of them at once; a set assembled outcome by outcome (which is what the Shiny app does) cannot. Such a caller attaches them to each rated object as the `"pmatools_display"` attribute — a named list holding any of `forest_display`, `forest_display_rob`, `rare`, `rare_forest_display`, `pubias_missing_df` — and this method reads them per outcome, falling back to the argument of the same name for an outcome that carries none. The same arrangement already lets `follow_up` / `unit` differ per row (§4.6, `.display_arg_from_outcomes()`).

The same attribute also carries **how a continuous outcome is presented** in `summary_of_findings.docx` / `.csv`: `convert_smd_to_or`, `baseline_risk`, `threshold_label` and `chinn_invert`, each the `sof_table()` argument of the same name (`PMATOOLS_RESPONDER_FIELDS`). `grade_table()` reads them per row (§4.9). They are not `grade_meta_multi()` arguments: `grade_meta()` takes none of them, and its own `baseline_risk` is a different quantity.

An unrecognised name in the attribute **aborts**, and so does an attribute that is not a fully named list. A misspelt field is read by nothing, so the artifact it was meant to shape would be written as if it had never been supplied — the same silent-drop failure `grade_args` name checking exists to prevent (§4.8.1).

Because the presentation rides on an attribute and `grade_meta_multi()` cannot restore it, the generated multi-outcome `analysis.R` carries an explicit re-stamp block — one `attr(set$outcomes[[…]], "pmatools_display") <- list(convert_smd_to_or = TRUE, …)` per converted outcome, emitted after `set_primary()` and before `grade_table()`. Without it the script would reproduce every number of the exported table except how its continuous rows are presented. The block is absent from a set with no converted outcome, so an ordinary bundle's script is byte-for-byte what it was.

`other_text` / `other_downgrade` follow the same rule without an attribute: an outcome carrying its own `$other_text` (a non-blank single string) or `$other_downgrade` uses it for its own evidence profile, in the per-outcome directory and in the combined `evidence_profile.docx`, and the set-wide argument applies only to the outcomes that carry none.

**Arm labels reach the script too.** `label_intervention` / `label_control` name the review's own arms in `summary_of_findings.docx` — the `With <control>` / `With <intervention>` column headers and the plain-language subject — so the generated `analysis.R` renders them onto its `grade_table()` call. Without them the script rebuilt every number of the shipped table and printed the three generic strings instead. A label left at its `grade_table()` default (`"intervention"` / `"control"`) is omitted from the generated call, so an ordinary bundle's script is byte-for-byte what it was; a non-default label is rendered with `deparse()`, which survives the apostrophe in free text such as `"clinicians' usual care"`. The single-outcome method has no such arguments — it calls `sof_table()` with the defaults — so its script and its table cannot disagree, and `inst/templates/analysis_script.R.tpl` therefore carries no label placeholder.

**Rare-event artifacts are gated on the outcome having a `rare` fit, not on `include`** — the same rule as the flat layout. `include` cannot answer "were this outcome's events rare enough to have been re-analysed?".

The generated `analysis.R` gains a `# ----- 2b. Rare-events outcomes -----` block for each such outcome: it re-runs `run_rare_ma()` on that outcome's rows and substitutes the primary fit into `ma_list` before anything is rated. `run_ma_multi()` pools with `run_ma()`, which drops a double-zero study, so without the block the script would silently rate a different analysis than the one in the bundle. The block is absent from a set with no rare outcome, so an ordinary bundle's script is byte-for-byte what it was.

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
pmatools analysis - generated {{timestamp}}
Outcome: {{outcome_name}}
================================================================

[ Meta-analysis summary{{ - <analysis set>}} ]
{{summary(ma) text}}

{{[ Meta-analysis summary - low risk of bias studies only (K of N studies; rated below) ]
{{summary(grade$meta) text}}}}

================================================================
[ Certainty assessment (Core GRADE series) ]
================================================================

{{print(g) text}}

[ Domain notes ]
- [{{domain}}] {{note}}          # one line per domain with a note
```

**Analysis-set heading [v0.5.1].** Without a low-RoB refit (§5.1) the heading is the bare `[ Meta-analysis summary ]`, unchanged. When `grade$rob_refit` is `TRUE` the heading **names the analysis set**, because an unqualified heading let a reader take a pooled estimate from the top of the file that the certainty assessment below it was not computed on:

| `ma` is | Heading suffix |
|---|---|
| the rated (refitted) analysis | `- low risk of bias studies only (K of N studies; rated below)` |
| the all-studies analysis (`grade$meta_full`) | `- all studies (N studies; NOT the analysis rated below)` |
| neither | `- analysis as supplied (NOT the analysis rated below)` |

and when `ma` is **not** the rated analysis, the rated one is printed below it as a **second, separately headed block**, so the file always contains the analysis the certainty rating was made on. Two meta objects count as the same analysis set when their `k` and `studlab` match (`.same_analysis_set()`); the refit rebuilds the object, so `identical()` is too strict.

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

**Presenting a continuous outcome as a proportion of responders, per row [v0.5.1].** `grade_table()` has no `convert_smd_to_or` argument, because the answer is not one per table. It reads `convert_smd_to_or`, `baseline_risk`, `threshold_label` and `chinn_invert` off each rated object's `"pmatools_display"` attribute (§4.6, §4.8) and applies them **row by row**, in both layouts. A converted row's two arm columns hold the dichotomised rates with the same `.format_cer()` / `.format_ier_chinn()` numbers `sof_table()` produces, marked `*`; its Difference column keeps the continuous estimate, and the arm-derivation footnote is not written for it. In the GRADEpro layout a converted row counts as a *rate* row for the arm-header vote, since its cells hold event rates again.

The `*` footnote explaining Chinn's formula is written **once when at least one row used the conversion, and not at all when none did**. It omits the direction and the threshold that `sof_table()`'s single-row version weaves in, because a combined table can hold rows converted in opposite directions against different thresholds; each converted row states its own on a following line keyed by outcome name (`[Depression] Responder presentation: OR direction inverted (OR > 1 = treatment better). Threshold definition: …`), the same shape as the per-outcome publication-bias sentences of the same footer.

**A row that cannot be converted falls back; it does not take the table down.** `sof_table()` **aborts** when `convert_smd_to_or = TRUE` and the summary measure is not SMD/MD or `baseline_risk` is not in (0, 1) — its table *is* that row, so with the conversion refused there is nothing left to render, and that behaviour is unchanged. In `grade_table()` the same conditions (plus a missing pooled estimate) leave the row in its **unconverted** presentation and add the reason to the numbered per-row register, so the marker sits on the outcome name: *"The responder presentation was asked for but could not be applied: …. This row shows the unconverted presentation instead."* One outcome must not cost the reviewer the whole document. A row can therefore carry more than one `[n]` marker — the analysis-set note and this one — and `disp()` renders them as `Mortality [1][2]`.

An unrecognised name in the attribute **aborts**, in `grade_table()` as in `export_bundle()`, so the on-screen preview and the exported table agree about what is legal.

`summary_of_findings.csv` resolves the presentation through the same helper as the `.docx`, so the two cannot disagree, and the generated `analysis.R` re-stamps the attribute onto the set it rebuilds (§4.8).

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

add_not_reported(                                   # v0.5.1; see §4.14
  set, outcome_name,
  follow_up = NULL, reason = NULL,
  label     = "Not reported",
  after     = NULL
) -> pmatools_set
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

As of v0.5.1 an element of `$outcomes` may also be a `pmatools_not_reported` (§4.14). Such an element is printed as `<not reported>` in place of certainty with `-` for the rating target and analysis set, and is **excluded from the "analysis sets differ" test** — "not reported" is not an analysis set, and must not make a homogeneous set look mixed.

### 4.12 Rare-event methods and additional plots

Exported but not specified in detail here; see the roxygen pages. They predate this section and their signatures are authoritative in the code:

`run_rare_ma()`, `rare_event_diagnostics()`, `plot_rare_sensitivity_forest()`, `plot_trimfill_forest()`, `plot_forest_rob()`, `plot_forest_indirectness()`, `plot_forest_pubias_subgroup()`, `evidence_profile()`.

(`combine_arms()` in `R/combine_arms.R` **is** exported — see §4.16. Revisions of this list between v0.3 and v0.5.0 wrongly called it internal.)

`evidence_profile(grade, palette, study_design, other_text, other_downgrade)` renders the per-outcome GRADE evidence profile used by both bundle layouts.

### 4.13 `indirectness_table()` [v0.5.0]

```r
indirectness_table(x, summary_text = NULL, ...) -> flextable
```

Renders `x$indirectness_subdomains` (§4.5.3): target question, evidence found, a colour-graded 4-option judgment row with the recorded answer ticked, and a merged "Judgment across subdomains" row carrying the overall judgment. Aborts with a message telling the caller how to record subdomains when `x` has none.

**Attribution (§4.13).** This is a **pmatools table layout implementing Core GRADE 5's per-PICO reasoning — not a Core GRADE 5 publication table.** The article body carries exactly two tables: Table 1 (an adaptation of a summary of findings table) and Table 2 ("Summary of indirectness issues": PICO element / Reason for rating down / Examples / Likelihood of rating down). Nothing of this shape appears there, and the strings "sufficiently direct", "probably yes" and "probably no" occur nowhere in it. *(The online supplementary appendices have not been checked.)* The footer of the rendered table states this, and also reproduces the Table 2 likelihood gradient (Population "Low" → Intervention "Intermediate" → Comparison "Substantial" → Outcome "High likelihood"), which the symmetric worst-case fold does not reproduce.

### 4.14 `not_reported_outcome()` / `add_not_reported()` [v0.5.1]

```r
not_reported_outcome(
  outcome_name,             # single non-empty string
  follow_up = NULL,         # optional single string
  reason    = NULL,         # optional single string -> numbered footnote
  label     = "Not reported"
) -> pmatools_not_reported

add_not_reported(
  set,                      # pmatools_set
  outcome_name,             # not already in the set
  follow_up = NULL, reason = NULL, label = "Not reported",
  after     = NULL          # NULL append | outcome name | integer 0..length(order)
) -> pmatools_set
```

Core GRADE 6 asks the summary of findings table to cover every patient-important outcome the review addressed, **including the ones no included study reported**. Every other row of `grade_table()` is derived from `x$meta`, so such an outcome could not be expressed at all.

**Object.** A list of `outcome_name`, `follow_up`, `reason`, `label` with class `"pmatools_not_reported"`. `follow_up` and `reason` normalise `NULL` / `NA` / `""` to `NULL`, so downstream code only tests for `NULL`. It carries no meta object, no effect estimate and no certainty rating.

**The class deliberately does NOT inherit `"pmatools"`.** `grade_table.R`, `sof_bmj.R`, `export_bundle_multi.R` and `grade_report.R` between them dereference `g$meta$…` about forty times; a `pmatools` with a `NULL $meta` would flow through all of them and evaluate to `NULL` — blank cells, missing bundle files, no error anywhere. Not inheriting means every existing `inherits(x, "pmatools")` guard fails loudly and each consumer has to opt in on purpose.

**Consumers.**

| Function | Behaviour |
|---|---|
| `grade_table()` | accepted, in a named list or a set, both styles |
| `grade_report()` | accepted; renders a prose line, not a domain table |
| `export_bundle()` on a `pmatools_set` | accepted; the outcome keeps its numbered `outcomes/NN_name/` directory with a `results.txt` recording the status, and `analysis.R` re-issues the `add_not_reported()` call |
| `reorder_outcomes()`, `set_primary()` | treated like a rated outcome (both key off names only) |
| `print()` / `summary()` on the set | `<not reported>` in place of certainty; `-` for rating target and analysis set |
| `sof_table()` | **abort** — one analysis to summarise, and there is none |
| `evidence_profile()` | **abort** — five domain columns judge a body of evidence, and there is none |
| `export_bundle()` on the object alone | **abort** — pointing at `add_not_reported()` + the set method |

**Rendering rules.**

- `label` fills every value cell: participants, effect, both arm-level cells, Difference.
- Certainty cell = `NOT_REPORTED_CERTAINTY` = `"Not rated"`, **never blank**: a blank cell cannot be told apart from a forgotten one, which is the argument for showing the row at all, and there is no body of evidence to rate.
- GRADEpro domain cells (`show_domains = TRUE`) = `NOT_REPORTED_DOMAIN_SYMBOL` = U+2013 en dash, which must stay visually distinct from `.domain_symbol()`'s `"?"` (judgment unknown, go and find it).
- BMJ plain-language cell = "No included study reported this outcome."
- `reason` becomes a numbered footnote on the row, sharing the `[n]` pool with the risk-of-bias analysis-set notes.
- One table-level footnote (`.not_reported_table_note()`) is emitted **once per table**, not once per row, whenever any row is not reported.
- Effect-measure headers and every domain-derived footnote are computed over `.rated_outcomes()` only, so one not-reported outcome cannot degrade a table that is otherwise homogeneous.

**Errors.** Empty / non-scalar / `NA` `outcome_name` or `label` aborts. `add_not_reported()` aborts when the set already holds that name ("an outcome is either rated or not reported, not both") and when `after` names an outcome not in the set or is outside `0..length(order)`.

### 4.15 `domain_facts()` [v0.5.1]

```r
domain_facts(
  x,                # pmatools object
  domain = NULL     # NULL -> every domain; else one of the five names, matched exactly
) -> named list of tibbles | tibble | NULL
```

Accessor over `x$domain_facts`, the structured record of the numbers a domain assessor used. With `domain = NULL` returns the whole named list, keyed by domain name and empty when nothing was recorded. With `domain` supplied returns that domain's tibble — columns `key` (chr), `label` (chr), `value` (chr, pre-formatted for display), `numeric` (dbl, `NA` when the fact is not scalar-numeric) — or `NULL` when the domain recorded nothing.

Valid `domain` values are exactly `x$domain_assessments$domain`: `"Risk of bias"`, `"Indirectness"`, `"Inconsistency"`, `"Imprecision"`, `"Publication bias"`. Anything else aborts, listing the valid names. **`NULL` for a valid name is not an error** — it means that domain records nothing today.

Facts are a machine-readable **companion**, not a replacement: `$domain_assessments$notes` remains the authoritative prose record of *why* a domain was rated the way it was, everything a fact reports is also stated there, and `notes` is unchanged down to the byte. The facts exist so a consumer can branch on the Fig 4 path or compute with I² without regex-parsing a sentence. Keys are in §5.6.

### 4.16 Helpers exported for host applications

Steps the pipeline already takes internally, exported so a caller assembling its own view of the same analysis agrees with pmatools instead of keeping a second copy of the logic.

```r
combine_arms(df) -> data.frame      # R/combine_arms.R
rob_strata(x, arg = "rob") -> chr   # R/domain_rob.R
```

**`combine_arms(df)`.** Collapses every group of rows sharing a study unit — `studlab`, `outcome` when that column is present, and `treat` — into a single row (Cochrane Handbook 6.5.2.10), which is what turns a multi-arm trial into the two-arm shape a pairwise meta-analysis needs. `n` and `event` are summed; `mean` / `sd` are pooled by the Handbook's iterative pairing formula (§below); every other column is a per-study property and takes the first row's value. Returns `df` unchanged when no study unit is duplicated. `ingest_data()` calls it via the internal alias `.combine_arms()`, so the normal pipeline never has to; it is exported for callers that assemble their own long data frame (notably a data editor previewing the merge).

```
N    = n1 + n2
M    = (n1*m1 + n2*m2) / N
SD^2 = ((n1-1)*s1^2 + (n2-1)*s2^2 + (n1*n2/N)*(m1-m2)^2) / (N - 1)
```

applied iteratively over the rows of a group; rows with `NA` in `n` / `mean` / `sd` are dropped from the continuous pooling.

**`rob_strata(x, arg = "rob")`.** Normalises free-text risk-of-bias labels onto the four strata pmatools groups studies by, returning a character vector the same length as `x` with elements in `"low"` / `"some"` / `"high"` / `"unknown"`. It shares the alias vocabulary of the internal `.normalize_rob_level()`, so `grade_meta()`, `plot_forest_rob()` and `plot_forest_indirectness()` (via the internal alias `.rob_plot_strata()`) all agree with it. Matching is case-insensitive after `trimws()`.

**Cochrane RoB 2 defines three judgments, not four** (Sterne JAC, et al. BMJ 2019;366:l4898), and they are accepted verbatim. So are ROBINS-I's four (Sterne JAC, et al. BMJ 2016;355:i4919), whose top two fold onto `high` because Core GRADE describes no three-level risk-of-bias downgrade (§5.1). The tools are kept apart in the table below because they are not interchangeable: RoB 2 is for randomised trials, ROBINS-I for non-randomised studies.

| Tool | Judgment | Stratum |
|---|---|---|
| RoB 2 | `Low risk of bias` | `low` |
| RoB 2 | `Some concerns` | `some` |
| RoB 2 | `High risk of bias` | `high` |
| ROBINS-I | `Low risk of bias` | `low` |
| ROBINS-I | `Moderate risk of bias` | `some` |
| ROBINS-I | `Serious risk of bias` | `high` |
| ROBINS-I | `Critical risk of bias` | `high` |

Every other accepted label:

| Stratum | Accepted labels |
|---|---|
| `low` | `not_serious`, `no`, `low`, `L`, `No concerns` |
| `some` | `some_concerns`, `some`, `S`, `M`, `*`, `moderate`, `unclear` (RoB 1) |
| `high` | `very_serious`, `extremely_serious`, `high`, `very high`, `H`, `C`, `Serious concerns`, `Critical concerns` |
| `unknown` | `NA`, `""`, `?`, `unknown`, `na` |

`No concerns` / `Serious concerns` / `Critical concerns` are **pmatools' own phrasings**. Up to v0.5.1 the package documented all four of them as "Cochrane RoB 2.0", which RoB 2 does not define — only `Some concerns` is RoB 2's. They stay accepted, permanently: they are what stored extraction sheets and scripts written against v0.4–v0.5.1 contain, and removing a working alias would break those for no gain the corrected documentation does not already deliver. New work writes the RoB 2 or ROBINS-I judgments.

A bare `serious` is **rejected** here, not mapped: it named the `high` stratum up to v0.5.0 and names `some` from v0.5.1 (§5.0). Write `some_concerns` or `very_serious`.

Anything else also becomes `"unknown"`, but **with a warning naming the offending labels** — it deliberately does not abort, because it feeds plots and a plot with an "unknown" stratum beats no plot. `arg` prefixes that warning so a caller can name its own argument (`rob_strata(v, arg = "my_app: rob column")`). Callers needing a hard failure check the result for `"unknown"` themselves.

---

## 5. Algorithm specifications

### 5.0 Domain judgment vocabulary

The stored values **are** Core GRADE's words (v0.5.1). Core GRADE 1, verbatim: "We characterise limitations in each of these domains involved in rating down certainty as not serious; serious; very serious; or, rarely, extremely serious."

```r
GRADE_LEVELS <- c("not_serious", "serious", "very_serious", "extremely_serious")
GRADE_DOWNGRADE <- c(not_serious = 0, serious = -1,
                     very_serious = -2, extremely_serious = -3)
```

| value | Core GRADE wording | downgrade |
|---|---|---|
| `"not_serious"` | not serious | 0 |
| `"serious"` | serious | −1 |
| `"very_serious"` | very serious | −2 |
| `"extremely_serious"` | extremely serious | −3 |

**`"extremely_serious"` is manual only.** No assessor in the package emits it, and none may: Core GRADE describes no three-level downgrade in any of its flowcharts, and calls the level rare ("or, *rarely*, extremely serious"). `GRADE_LEVEL_AUTO_MAX` names the deepest level an automated path may produce (`"very_serious"`, −2), and `tests/testthat/test-grade-levels.R` asserts the invariant against every function in the namespace that calls `make_domain_row()` — not against a fixture sweep, which could only cover the branches it happens to reach. The level is reached through the scalar domain arguments (`rob =`, `inconsistency =`, `indirectness =`, `imprecision =`), each of which already requires its written rationale, and through the Shiny override menus (`pma_judgment_choices()`), which offer it on all five domain tabs. It is deliberately **not** offered on the app's "Other considerations" control, which stays 0 / −1 / −2: that control is not a Core GRADE domain and a −3 there would invent a rating the source does not describe.

**Aliases.** `"no"` → `"not_serious"`, and `"some"` / `"some_concerns"` → `"serious"`. These are the spellings whose *meaning* did not move in the v0.5.1 rename, so they are accepted silently and permanently. **`"not_serious"` / `"serious"` / `"very_serious"` / `"extremely_serious"` are what the objects, notes and tables contain**; consumers matching on judgment strings must match the canonical form. Anything outside the accepted set aborts via `validate_grade_level()`.

**A bare `"serious"` aborts, for one release.** Up to v0.5.0 `"serious"` was pmatools' internal name for the source's *very serious* (−2); from v0.5.1 it carries the source's own meaning (−1). Nothing about the spelling changed, so a script written against either release would keep running and report a **different certainty rating**. `.check_grade_level_input()` (`R/utils.R`) therefore refuses the string at every user-input boundary — the four scalar domain arguments, per-study `rob` / `indirectness` vectors, `rob_overrides` values and `rob_strata()` — with a message naming both readings and the spelling for each (`"some_concerns"` for −1, `"very_serious"` for −2, both unchanged in meaning since v0.5.0). The refusal is a temporary migration aid: it is to be deleted one release later, after which `"serious"` is simply the −1 level. `validate_grade_level(check_ambiguous = FALSE)` is the one exemption, used by `assess_rob()` where the values have already been normalised from the Cochrane RoB2 vocabulary.

**Display vocabulary (single source, v0.5.1).** `GRADE_LEVEL_SOURCE_WORDING` holds Core GRADE's wording and **`.grade_level_wording()`** (`R/utils.R`) is the **only** function permitted to turn a judgment into user-facing words. The seam survived the rename that made the values Core GRADE's own words, because it still does three things a renderer must not re-implement: it resolves the aliases a stored object or a user argument may carry, it turns a value into prose (`"very_serious"` is not what a table cell should read), and it is the single place to change if the display vocabulary ever diverges from the stored one again. The table is written out rather than derived with `sub("_", " ", .)` so that a level added later cannot silently acquire invented source wording.

`evidence_profile()` calls it; so do `sof_bmj()`'s certainty sentence, `indirectness_table()`'s notes, and the Shiny app's domain badges, verdict lines and override menus. Note the trap it closes: a second hand-written mapping — which `evidence_profile()`, `sof_bmj()`, `domain_indirectness.R` and the app badge each used to carry — could and did print "Serious" for −1 in one place and for −2 in another. A new renderer must call `.grade_level_wording()` rather than write another `switch()`. Likewise, the level → downgrade lookup is **`.grade_level_downgrade()`**, which reports 0 for an unrecognised level rather than aborting a render; the Shiny app's badge, chip and verdict helpers call it instead of the private copies they used to keep in step by hand.

### 5.1 Risk of bias — Core GRADE 4 Fig 2 flowchart (v0.5.0)

`assess_rob()` follows the BMJ 2025 Core GRADE 4 Fig 2 flowchart literally. `R/domain_rob.R`'s header comment is the maintained long form; this section is the contract.

**Step 0 — binary classification.** Each study is folded into low or high risk of bias:

| `rob_some_concerns` | low | high |
|---|---|---|
| `"low"` (default) | `not_serious`, `serious` | `very_serious`, `extremely_serious` |
| `"high"` | `not_serious` | `serious`, `very_serious`, `extremely_serious` |

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
| 1 | `za == zl == "trivial"` | `"not_serious"` |
| 2 | `za == zl`, non-trivial, inflation ≤ `rob_inflation_threshold` | `"not_serious"` |
| 3 | `za == zl`, non-trivial, inflation > `rob_inflation_threshold` | `"serious"` (−1) |
| 4 | `za != zl`, no sign flip across null | `"serious"` (−1) |
| 5 | `za != zl`, sign flip (`above` ↔ `below`) | `"serious"` (−1) |

Rule 5 rated down **two** levels up to v0.4. Since v0.5.0 every automated risk-of-bias path is capped at one level: Core GRADE 4 describes no two-level risk-of-bias downgrade (every leaf of Fig 2 reads "rate down" / "do not rate down"), and `.ROB_CAP_NOTE` is appended to the judgment note wherever the cap bites. `"very_serious"` stays reachable only through the scalar `rob` override, which requires `rob_rationale`.

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

**Two index spaces, one mapping.** The Fig 2 maths runs in **k-space** (length `meta_obj$k`, the estimable studies), whereas `rob_overrides` keys and `update.meta(subset = )` live in **studlab space** (length `meta_obj$studlab`). The two differ whenever {meta} drops a study from the pool — a trial with missing results, a double-zero trial under `method = "Inverse"`. `R/domain_rob.R` resolves the mapping **once per `assess_rob()` call** in `.rob_alignment()`, and `.rob_expand()` / `.rob_contract()` move vectors between the spaces, so nothing re-derives it. The resolver never guesses: it tries `length(studlab) == k`, then `!is.na(TE)`, then `is.finite(TE)`, and unless one of them reproduces exactly `k` rows it returns `NULL` and the caller keeps its existing abort/skip behaviour. `attr(<rob domain row>, "high_idx")` is studlab-aligned, which is what `update.meta(subset = )` needs.

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
    return judgment = "not_serious", auto = FALSE,
           notes = "Step 1: no important differences in point estimates / adequate CI overlap."
  }

  # ci_diff = "yes" → Step 2
  if (is.null(inconsistency_threshold_side)) {
    abort "inconsistency_ci_diff = 'yes' requires inconsistency_threshold_side"
  }

  if (inconsistency_threshold_side == "majority_one_side") {
    return judgment = "not_serious", auto = FALSE,
           notes = "Step 2: important differences exist, but majority on one side of clinical Threshold → do not rate down (per BMJ Core GRADE 3 flowchart)."
  }

  # opposite_sides → Step 3
  if (is.null(inconsistency_subgroup_explained)) {
    abort "inconsistency_threshold_side = 'opposite_sides' requires inconsistency_subgroup_explained"
  }

  if (inconsistency_subgroup_explained == "yes") {
    return judgment = "not_serious", auto = FALSE,
           notes = "Step 3: opposite-sided estimates explained by credible subgroup; present subgroups separately."
  }

  return judgment = "serious", auto = FALSE,
         notes = "Step 3: opposite-sided estimates not explained by credible subgroup → rate down one level."
}

# ---- Path C: auto-detect ----

# Step 1 surrogate: I² > 30%   (INCONSISTENCY_I2_CUT; v0.5.0, was 25%)
ci_diff_yes <- (i2_pct > 30)

if (!ci_diff_yes) {
  return judgment = "not_serious", auto = TRUE,
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
  threshold_side <- "majority_one_side";    judgment <- "not_serious"
} else if (pct_each_side >= 0.20) {            # OPPOSITE_EACH (pmatools)
  threshold_side <- "opposite_substantial"; judgment <- "very_serious"
} else {
  threshold_side <- "heterogeneous";        judgment <- "serious"
}

# AUTO Step 3 (v0.5.1). Reached ONLY on the opposite_substantial branch.
# inconsistency_subgroup_explained is read here as well as on Path B.
if (threshold_side == "opposite_substantial") {
  if (inconsistency_subgroup_explained == "yes") {
    return judgment = "not_serious", auto = TRUE,
           notes = "AUTO Step 3: opposite-sided estimates explained by a
                    credible subgroup → do not rate down; present subgroup
                    results separately." + ICEMAN caveat
  }
  # "no" or unanswered: judgment stays "very_serious" (−2).
}
# Notes carry: the I² surrogate caveat, the zone-cut-off provenance caveat,
# and (for opposite_substantial) the ICEMAN subgroup caveat and the note
# declaring the two-level departure from Core GRADE 3.
```

**AUTO Step 3 (v0.5.1, behaviour change).** `inconsistency_subgroup_explained` now reaches the automated path. Before v0.5.1 the automated opposite-sides note advised the reviewer to supply it, and doing so switched the domain onto Path B — which then aborted unless `inconsistency_ci_diff` **and** `inconsistency_threshold_side` were supplied too. The advice was therefore a no-op, and this closes that gap rather than adding a new judgment route:

- `"yes"` → `"not_serious"` (do not rate down), `auto = TRUE`, with `.INCONSISTENCY_SUBGROUP_CAVEAT`;
- `"no"` → `"very_serious"`, `auto = TRUE`, note says the subgroup did not explain it;
- unanswered → `"very_serious"`, note points at the argument.

It is read **only** on the `opposite_substantial` branch: on `majority_one_side` and `heterogeneous` Core GRADE 3 never reaches Step 3, so an answer there changes nothing. The value is validated (`"yes"` / `"no"`) on both paths.

**Edge cases:**

- `k < 2`: cannot assess inconsistency. Return judgment = `"not_serious"` with note "k < 2; inconsistency not assessable."
- I² is NA (e.g., k = 1): the Step 1 surrogate returns FALSE → judgment = `"not_serious"` with note "I² unavailable; cannot detect heterogeneity."
- All TE values equal (τ² = 0): I² will be 0 → the Step 1 surrogate returns FALSE → judgment = `"not_serious"`.
- Study-level TEs unavailable: Step 2 is not assessable → `"serious"` (conservative).
- Threshold supplied but `threshold_internal` cannot be derived (unknown sm): function aborts before reaching this domain.

**Judgment interpretation table:**

| Path | Step 1 / I² | Step 2 / Threshold check | Step 3 / subgroup | Judgment |
|---|---|---|---|---|
| Manual | ci_diff = "no" | — | — | **not_serious** |
| Manual | ci_diff = "yes" | majority_one_side | — | **not_serious** |
| Manual | ci_diff = "yes" | opposite_sides | yes | **not_serious** + note |
| Manual | ci_diff = "yes" | opposite_sides | no | **very_serious** (−2) |
| Auto | I² ≤ 30% | — | — | **not_serious** |
| Auto | I² > 30% | majority_one_side (max zone ≥ 80%) | not reached | **not_serious** |
| Auto | I² > 30% | opposite_substantial (≥ 20% each side) | yes | **not_serious** + note *(v0.5.1)* |
| Auto | I² > 30% | opposite_substantial (≥ 20% each side) | no / unanswered | **very_serious** (−2) |
| Auto | I² > 30% | heterogeneous (neither) | not reached | **serious** (−1) |

The two `very_serious` rows are the deliberate departure from Core GRADE 3 described above; they are the only automated route to −2 in this domain. Everything else stops at −1, and the scalar `inconsistency` override (with `inconsistency_rationale`) remains available for judgments the flowchart does not reach.

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

The ratio-scale magnitude is `1 - exp(-|log ratio|)`, which is symmetric: RR 0.60 and RR 1.667 both read 40%. Its **wording** is not symmetric, and up to v0.5.0 the note always said "relative risk reduction", so a pooled OR of 2.33 was reported as a 57% *reduction*. Since v0.5.1 an effect above the null is labelled a relative risk **increase**, stated as the equivalent reduction with the arms exchanged so the printed number keeps its meaning.

#### OIS inputs (v0.5.1)

**Direction of the binary alternative rate.** `assess_imprecision()` takes `small_values` (forwarded by `grade_meta()`, the same value `assess_rob()` receives) and derives `ois_p1` from `ois_p0` accordingly:

| `small_values` | meaning for a binary outcome | `ois_p1` |
|---|---|---|
| `NULL` | direction unknown | `ois_p0 * (1 - ois_rrr)` — the pre-v0.5.1 behaviour, unchanged |
| `"undesirable"` | a smaller value is worse ⇒ **events are desirable** (response, remission), and a benefit is an *increase* | `ois_p0 * (1 + ois_rrr)` |
| `"desirable"` | a smaller value is better ⇒ **events are undesirable** (mortality, relapse), and a benefit is a *reduction* | `ois_p0 * (1 - ois_rrr)` |

**The declared direction decides, not the observed effect.** The OIS is an a-priori power calculation for the smallest effect worth not missing, which is a property of the question — a modest *benefit* — and `small_values` is what states which way that runs. Letting the pooled estimate pick the side would make the target partly data-driven and would collapse `"desirable"` and `"undesirable"` onto the same answer whenever the estimate sits above the null. The pooled effect is nevertheless read and reported: the note says whether it agrees, and when it does not (the evidence describes a harm on this outcome) it says so explicitly rather than silently powering against the other tail.

`ois_p1` is clamped into (0, 1) — `ois_p0 * (1 + ois_rrr)` can exceed 1 on a high control-group risk — and the note says so when it clamps. An explicitly supplied `ois_p1` still takes precedence and no direction is applied to it.

**Auto-derived `ois_sd` (continuous).** `.calc_ois()` needs both `ois_delta` and `ois_sd`. `ois_delta` has always fallen back to the Threshold; `ois_sd` had no fallback, so a continuous outcome with no reviewer-supplied SD reached Fig 4's large-effect path, found no OIS and landed on "do not rate down" with no explanation. `ois_sd` now falls back to `compute_pooled_sd(meta_obj)`, and the notes and the `ois_sd_source` fact record that it was derived rather than supplied.

**`ois_sd = 1` for SMD.** `n_arm = 2(z_α + z_β)² σ² / δ²` requires δ and σ on the same scale, and an SMD is *by construction* expressed in within-study SD units — so σ is 1 and the raw pooled SD must not be applied to a threshold that is already standardized. Deriving it from the data anyway inflated the target N by σ² (a pooled SD of 4 gives 16×, of 8 gives 64×), which reaches the rating: Fig 4's large-effect path consults the "< 30% of OIS" rule, so an inflated OIS can turn `not_serious` into `serious` or `very_serious`. MD and RoM keep the pooled-SD derivation, where the threshold is on the raw scale. `ois_sd_source` reports which of the two applied, in words. An explicitly supplied `ois_sd` still takes precedence for every measure.

**"OIS could not be computed" names the missing input.** When the OIS is still unavailable, the Fig 4 path string names which of `ois_p0` / `ois_p1` / `ois_delta` / `ois_sd` was missing, or says that the analysis carries no complete arm-level sample sizes to compare against.

**CI ratio** (Fig 4 caption) is the upper CI limit divided by the lower limit on the ratio scale.

**Notes** record which Fig 4 path produced the judgment, including which CI-ratio rule fired and the continuous 400-per-group (total N 800) rule of thumb.

**Manual override.** A scalar `imprecision` (with mandatory `imprecision_rationale`) bypasses this assessment entirely (v0.4.0).

### 5.6 Structured domain facts (v0.5.1)

The container is **domain-agnostic**: `.fact(key, label, value, numeric = NA)` builds one row, `.facts(...)` binds the non-`NULL` ones into a tibble, and the assessors attach it to their `make_domain_row(facts = )`. `grade_meta()` collects the non-`NULL` results into `$domain_facts`, keyed by domain name; a domain that records nothing is simply absent from the list. Reached with `domain_facts()` (§4.15).

**Facts are recorded on every branch, including the ones that do not rate down** — the assessors decide what is *true*, the renderers decide what to *show*. The prose in `$domain_assessments$notes` stays authoritative and byte-identical; every fact restates something already in it.

| Domain | `key` | `numeric` |
|---|---|---|
| Risk of bias | `high_rob_studies` | count of high-RoB studies |
| | `high_rob_weight_share` | weight share (or the count-share fallback, which the `value` string names as such) |
| | `estimate_shift` | `inflation_ratio` (`NULL` when the direction check did not run) |
| | `fig2_branch` | the Fig 2 direction-of-bias rule number, when one fired |
| Inconsistency | `i2` | I² in per cent |
| | `tau2` | τ² |
| | `q_pvalue` | Cochran Q p value |
| | `zone_counts` | k (the `value` names the **chosen** threshold, Core GRADE 3 Fig 2 node 2) |
| | `zone_decision` | largest single-zone share |
| Imprecision | `confidence_interval` | — |
| | `crosses_null` | — (`"yes"` / `"no"`) |
| | `threshold_position` | — (omitted when no MID zone applies) |
| | `ois` | observed / target ratio; the `value` says `"not applied on this Fig 4 path"` when Fig 4 did not consult it |
| | `ois_target_rate` | `ois_p1` (v0.5.1; recorded only when `ois_p1` was derived rather than supplied — the `value` names the direction and why it was chosen) |
| | `ois_sd_source` | `ois_sd` (v0.5.1; recorded only when it was derived rather than supplied — the `value` says whether it is the pooled within-study SD or the SMD's σ = 1) |
| | `fig4_path` | — |
| | `ois_used` | — (`"yes"` / `"no"`) |
| Publication bias | `k` | effective study count (`.pubias_effective_k()`) |
| | `egger_p` | Egger p value, recorded only when the test actually ran |
| Indirectness | none recorded | its judgment is a gradient, not a flowchart branch — the structured record is `x$indirectness_subdomains` |
| Risk of bias, Inconsistency, Imprecision, Publication bias | `flow_path` | the flowchart path taken; see §5.7 |

Keys are **stable API**: a consumer branches on `key`, not on the wording of `label` or `value`.

`flow_path` is **machine-only**: `.FACT_KEYS_MACHINE_ONLY` in `R/utils.R` lists it and
`.drop_machine_only_facts()` filters it out before facts are rendered as prose, so it
never reaches a Summary of Findings footnote. Anything else added for a renderer rather
than a reader belongs in that constant too.

### 5.7 Decision flowcharts and the node vocabulary (v0.5.1)

Four domains route their judgment through a flowchart and ship a drawing of it:
Risk of bias (Core GRADE 4 Fig 2), Inconsistency (Core GRADE 3 Fig 2), Imprecision
(Core GRADE 2 Fig 4) and Publication bias (Core GRADE 4 Fig 5). Indirectness does not:
Core GRADE 5 Table 2 grades it on a gradient across the four PICO elements, so there is
no branch to draw or to record.

**These are pmatools' own diagrams, not reproductions of the BMJ figures.** They differ
from the source on purpose — the Risk-of-Bias chart enumerates five direction rules the
source does not, the Fig 5 chart carries a pmatools node that is not one of Figure 5's
four, and the Inconsistency edges are labelled with pmatools' numeric surrogates
(I² > 30%, the 0.80 / 0.20 zone shares) which Core GRADE declines to quantify. Each
figure says so in its `<desc>` and names the source figure in its caption.

**A chart draws decisions, not commentary (v0.5.1).** Three prunings follow from that:

- The Risk-of-Bias chart **starts at the dominance node**. It used to open with "Any
  study at high risk of bias?", whose "no" branch is not a decision — with no high-risk
  study the dominance share is 0, which is below the gate, and there is nothing to
  exclude. That case now routes through the surviving chart
  (`dominance → dominance-no → appreciable → appreciable-no → leaf-all`), which is where
  it always belonged, so `.ROB_FIG2_NODE_IDS` no longer carries
  `pma-rob-node-anyhigh`, `pma-rob-leaf-nohigh` or the two `anyhigh` edges.
- The Publication-bias chart's questions are **unnumbered**. The Q1–Q4 labels came from
  Core GRADE 4 Fig 5, but the chart interleaves a pmatools node between Q1 and Q2, so
  the numbering on screen described neither the source nor the route. The wizard
  headings and `PUBIAS_NODE_TITLES` in the app dropped their prefixes with it. The
  `"Q1:"`–`"Q4:"` prefixes inside the domain **notes** stay: those travel into
  `evidence_profile()` and the `.docx`, where they are the ordered machine-readable
  record of the assessment and no figure is present to disagree with them.
- The Publication-bias chart drops its two "qualitative assessment required" leaves.
  Both were reached with judgment `"not_serious"`, so they are drawn as the `nodown` leaves they
  are; the qualitative caveat is carried by the note and the `rlang::warn()`, which is
  where a reader can act on it.

**Where they live.** `inst/figures/<figkey>.svg` is canonical, with `<figkey>` one of
`rob`, `incon`, `impre`, `pubias`. `man/figures/` carries byte-identical copies so
roxygen's `\figure{}` can resolve them, regenerated by `data-raw/build_figures.R`; a
test asserts the two directories agree, so drift fails the build instead of shipping.
`inst/` is staged wholesale into the app bundle, so the app reads the same file the
package documents.

> **No figure lookup may live in `R/`.** `stage_bundle.R`'s `system.file()` rewrite is
> templates-only (`TPL_LOOKUP_PAT` hard-codes the literal `"templates"`), so a
> `system.file("figures", …, package = "pmatools")` call would resolve to `""` in the
> deployed app *and* be reported as a survivor by the check `deploy.R` fails on. The
> loader is app-side: `pma_flowchart()` in `shiny/R/ui_helpers.R`.

**Node vocabulary.** Every decision box, edge and terminal in an SVG carries
`id="pma-<figkey>-<kind>-<slug>"` with `<kind>` one of `node`, `edge`, `leaf`. Each
assessor declares the ids it can emit as a constant beside itself —
`.ROB_FIG2_NODE_IDS`, `.INCON_FIG2_NODE_IDS`, `.IMPRE_FIG4_NODE_IDS`,
`.PUBIAS_FIG5_NODE_IDS` — and every `return()` records the ids it traversed in the
`flow_path` fact, space-separated, in order.

`tests/testthat/test-flowchart-nodes.R` holds the two halves together: the constant
must be a subset of the ids actually present in the SVG, an emitted `flow_path` must
only name ids in the constant, and the `inst/` and `man/` copies must be byte-identical.
Adding a branch without drawing it, or renaming a node in the SVG, fails there.

**PDF manual.** `\figure{}` is emitted under `\if{html}{}` only. Rendering SVG to PNG
for the LaTeX manual would need a new dependency, which §1 of `CLAUDE.md` makes
expensive for no reader benefit, so the LaTeX branch carries a pointer to the HTML help
instead.

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
| test-plain_language.R | Core GRADE 6 Box 1 statement selection from certainty × `threshold_type` × `rating_target` × sign; the one-adverb rule; the omit-the-column cases |
| test-indirectness_dominance.R | scalar `indirectness` override vs the subdomain worst-case rollup |
| test-control_risk.R | `baseline_risk` resolution, the complete-case `event.c` / `n.c` filter, `metaprop` fallback |
| test-version-stamp.R | `.pmatools_version()` under `source()` (vendored) as well as installed |

**Added in v0.5.1:**

| File | Coverage |
|---|---|
| test-domain-facts.R | §4.15 / §5.6: the keys and `numeric` values each domain records on every branch; `domain_facts()` argument validation; `NULL` for a valid domain that records nothing; the footnote markers the three renderers derive from the facts |
| test-not_reported.R | §4.14: the `pmatools_not_reported` constructor and its input validation; `add_not_reported()` ordering (`after` as name / integer / `NULL`) and the duplicate-name abort; both table layouts, `grade_report()` and the set bundle; the `sof_table()` / `evidence_profile()` / single-object-`export_bundle()` refusals; the once-per-table footnote; rated-subset computation of headers and domain footnotes |
| test-public-helpers.R | §4.16 and §4.7c: `combine_arms()` binary sum and continuous pooling, per-study column carry-over, unchanged-when-unique; `rob_strata()` alias table, quiet-`unknown` inputs, warn-not-abort on unrecognised labels, the `arg` prefix; `format_effect()` exponentiation rule, model fallback, `"NR"`, prediction line |
| test-export_bundle.R (extended) | `style` / `follow_up` / `unit` / `sof_notes` on both methods and their rendering into `analysis.R`; exact `[[` lookup of `grade_args` / `ma_args` and the formals check; the `threshold_baseline` slot; the `results.txt` analysis-set heading and the second rated-analysis block (§4.8.2) |
| test-domain_rob.R (extended) | the k-space / studlab-space alignment (§5.1): refit and `rob_overrides` when {meta} drops a study; unresolvable alignment keeps the skip-with-a-warning behaviour |
| test-sof_bmj.R (extended) | arm-level columns for continuous outcomes: the IV-weighted control mean, the SMD × pooled-SD rescale, the two derivation footnotes, and binary tables left unchanged |

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
| `grade_table()` row asking for the responder presentation that cannot support it (non-SMD/MD `sm`, `baseline_risk` outside (0, 1), no pooled estimate) | the row keeps its unconverted presentation; the reason is a numbered per-row footnote; the table still renders (§4.9) |
| `grade_table()` where every row asked for the conversion and none could take it | no Chinn footnote at all — it would describe a conversion no cell went through |
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
- shinyapps.io deployment automation in the package (it lives with the app, in `shiny/deploy.R`)
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
