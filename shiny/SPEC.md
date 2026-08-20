# pmatools wizard — Shiny App Specification

> Authoritative specification for the pmatools wizard, the Shiny app in this repository's `shiny/` directory. The package-level R logic (data ingestion, MA pipeline, GRADE assessment, SoF table, export) is provided by [pmatools](../SPEC.md), whose sources live one directory up and are **staged into the deploy bundle** rather than installed (§7.1). This app is a **UI layer** on top of pmatools.

**Public URL:** https://yuki-furukawa.shinyapps.io/pmatools/
**Deployment:** shinyapps.io (account: `yuki-furukawa`, appId: `17697029`)
**App version:** 3.2.0 — the `Version:` field of `shiny/DESCRIPTION`, which is the app's own and tracks separately from the package version in `../DESCRIPTION` (§9)

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

pmatools is **not** a dependency in the install sense. Its sources are staged
into the bundle by `stage_bundle.R` and `source()`d by `app.R`; §7.1 describes
the mechanism and why it exists.

| Package | Where it comes from | Role |
|---|---|---|
| pmatools | `../R` + `../inst`, staged into `R/_pmatools/` + `_pmatools_inst/` | All MA + GRADE + SoF + export logic |
| shiny | CRAN | Reactive framework |
| bslib | CRAN | Bootstrap 5 theming, accordion, cards |
| htmltools | CRAN | HTML helpers |
| DT | CRAN | Editable grids: Step 1 preview, Step 3 per-study RoB / Indirectness |
| flextable, officer | CRAN | SoF preview and `.docx` export |
| meta | CRAN | pmatools' pooling backend |
| metafor, mmeta, BiasedUrn, brglm2 | CRAN | rare-event backends, named only as strings |
| tibble, dplyr, rlang, glue, zip, magick, readxl, shinycssloaders | CRAN | staged pmatools code and app chrome |

**`shiny/DESCRIPTION` is a deployment manifest, not a package.** It is
`Type: Project` and is never installed. It exists for exactly one reason:
`rsconnect::deployApp()` reads its `Imports` field and installs those packages
on the shinyapps.io build server. rsconnect ignores its `Version:`, which is
the app's own (§9).

Two consequences, both load-bearing:

- **There is no `Remotes:` field, and there must not be one.** A
  `Remotes: github::ykfrkw/pmatools` entry would put `install_github()` back on
  the build server, which is the HTTP 401 this whole arrangement exists to
  avoid (§7.1).
- **A package added to `../DESCRIPTION` `Imports`/`Suggests` has to be added
  here too.** rsconnect reads only this file, and its static code scan cannot
  see a package named as a string — `rare_events.R` names `metafor`, `mmeta`
  and `BiasedUrn` that way, so this `Imports` field is the only thing that
  installs them. Omitting one deploys cleanly and fails at runtime, in
  production. `Rscript shiny/stage_bundle.R --check-only` audits it, and
  `deploy.R` fails the deploy on it (§7.3).

`../CLAUDE.md` §1 states the four rules of this kind that survived the
repository merge; read it before changing either `DESCRIPTION`.

### 2.2 File layout

```
shiny/
├── app.R                          # entrypoint: ui + server; sources R/_pmatools/
├── DESCRIPTION                    # rsconnect dependency manifest (see §2.1)
├── deploy.R                       # clean-tree gate → stage → .rscignore → deployApp()
├── stage_bundle.R                 # stages ../R and ../inst into the bundle
├── .rscignore                     # committed bundle exclusions, one glob per line
├── R/
│   ├── step1_data.R               # data import module (UI + server)
│   ├── step2_ma.R                 # MA module
│   ├── step3_grade.R              # GRADE module (domain tabs)
│   ├── step3_threshold.R          # decision-threshold + presentation helpers
│   ├── step4_export.R             # export/download module
│   ├── ui_helpers.R               # shadcn-style component helpers (card, badge, stepper)
│   ├── educational_copy.R         # American English copy as named-list constants
│   └── _pmatools/                 # GENERATED: staged copy of ../R, plus VERSION
├── _pmatools_inst/                # GENERATED: staged copy of ../inst
├── www/
│   ├── shadcn.css                 # design tokens + component CSS
│   ├── required-fields.js         # the two-tier required-field marking (§3.3.6)
│   ├── flowchart.js               # lights up the branch each domain took
│   └── embed-height.js            # posts document height to the WordPress embed
├── tests/
│   ├── testthat.R                 # app suite entrypoint (§8)
│   └── testthat/
├── rsconnect/shinyapps.io/yuki-furukawa/
│   ├── pmatools.dcf               # deployment record for the live app
│   └── pairwise_meta_analysis.dcf # record for the app this one replaced
└── SPEC.md                        # this file
```

**`R/_pmatools/` and `_pmatools_inst/` are generated and gitignored.** Never
hand-edit them: the next `stage_bundle.R` run deletes and regenerates both.
Fix `../R` and re-stage.

The two deployment records coexist deliberately. `pairwise_meta_analysis` was
the app this wizard replaced, and its record is kept so the old URL stays
identifiable; `deploy.R` therefore names the full `server` / `account` /
`appName` triple, leaving rsconnect no room to resolve the wrong one. Nothing
matching `app_*.R` is tracked any more, but both `.rscignore` and `deploy.R`
still handle that pattern: the legacy single-page sources carried Latin-1 bytes
in their filenames, which break rsconnect's path scanner, and an untracked copy
left in the directory would still reach the bundle (§7.3).

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
    # The GUARDED responder-presentation boolean, written by step3_server()
    # from input$sof_presentation only after sm and the proportion have been
    # checked -- never a raw mirror of the radio. Named `convert` because it
    # is not the radio; it is what reaches sof_table(convert_smd_to_or =).
    # All five are banked ON each outcome as it is saved (§3.4.10a), because
    # they describe one ROW of the Summary of Findings, not the table.
    convert            = FALSE,
    # The second half of the three-way radio: TRUE only on "both", and only
    # ever written from the same guarded reactive as `convert`, so a row that
    # is not converting can never carry it.
    keep_effect_scale  = FALSE,
    baseline_risk      = NULL,
    chinn_invert       = FALSE,
    threshold_label    = NULL,
    follow_up          = NULL,
    unit               = NULL
  )
)
```

Step transitions (`Next` / `Back` buttons) update `state$step`. Each step's UI is conditionally shown via `conditionalPanel(condition = "input.step == 'N'", ...)`.

**Re-computation rules:**

- `state$data` recomputes when Step 1 inputs change (debounced 300ms).
- `state$ma` recomputes on every change to Step 2's debounced input bundle while **`auto_rerun`** is ON, and only on an unserved press of **"Run analysis"** while it is OFF (§3.3.3). `input$run_ma` is a never-reset actionButton counter, so "unserved" means *greater than the count `ma()` has already run for* — read as a bare `> 0L` the first press latches, and the OFF setting stops doing anything for the rest of the session.
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
analysis: statistical pooling is a small part of a systematic review and
meta-analysis (SR&MA), which also needs a prespecified and pre-registered
protocol, a comprehensive search, dual independent screening and extraction,
and risk-of-bias assessment, all completed before the analysis.

**The first sentence names the whole activity, then abbreviates it.** It reads
"a systematic review and meta-analysis (SR&MA)", not "a systematic review":
the modal is the one place the app says what the reviewer is doing, and the
sentences after it already use `SR&MA`, so the expansion has to come first or
the abbreviation arrives undefined.

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

The card opens with two pieces of feedback, because the step's job is to answer
"is my data right?", not to display it:

1. **A load banner** — `pma_banner(tone = "success")` carrying
   `pma_load_summary()` ("36 rows, 18 studies, long format."). It replaces a
   `verbatimTextOutput` reading `Status: ...`; a read failure renders the same
   banner in its warning tone instead.
2. **A detected-columns strip** — `pma_column_roles_strip()`, one chip per
   `pmatools::detect_column_roles()` role, above the table. The strip is built
   from the column names **as they arrived**, captured into `loaded_raw_names`
   at ingest: `ingest_data()` renames source columns onto their role names, so
   the ingested tibble alone can no longer say that `studlab` came from
   `study`.

Chip states, from `pma_column_role_status()`:

| state | meaning | shown |
|---|---|---|
| `found` | a column filled the role | green, naming the source column when it differs from the role |
| `missing` | nothing filled a role the analysis needs | amber, with a short hint |
| `optional` | absence is ordinary | muted, "not in your data" |

`outcome` and `subgroup` are always `optional` when absent. The measure roles
are an either-or family: with `event` present, `mean`/`sd` are `optional`, and
vice versa; with neither branch satisfied all three are `missing`. `rob` and
`indirectness` report **how much of the review is rated** (from
`state$rob_table`), not whether a column exists, so assigning them here turns
the chip green and a file carrying unreadable labels cannot show green on the
strength of the column existing.

**Column toggle.** `radioButtons("preview_columns", …)` switches the preview
between **Analysis columns** (default) and **All columns**. The analysis set is
`pma_analysis_columns()` — the `detect_column_roles()` roles the data carries.
The bundled sample is 39 columns wide and five of them are the analysis; the
full table stays one click away.

The toggle **hides** columns (`columnDefs`, `visible = FALSE`); it never
subsets the frame handed to `DT::datatable()`. DT reports a cell edit as
`col`, the DataTables column index, which counts hidden columns, and
`input$data_preview_cell_edit` is applied as `res[[info$col + 1]]` against the
full frame. A subset would leave that index pointing at a different column and
write the edit into the wrong one.

**Bulk risk of bias.** `step1_rob_set_low` / `_some` / `_high` / `step1_rob_clear`,
below the table. Assigning risk of bias across every study is data-entry work.
They write `state$rob_table` — the same object Step 3's editor and the rating
read — and the preview's `rob` column, so the table reflects the press. The
Step 3 copies (§3.4) stay: correcting one study while looking at the certainty
verdict is a real workflow, and both sets edit the same state.

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

The sidebar is `flex: 0 1 300px` and the right pane
`flex: 1; min-width: min(480px, 100%)`. Two independent rules meet here:

- **Both columns must be able to shrink.** A fixed `flex: 0 0` basis and a flat
  `min-width: 480px` floor were what made a 375px viewport render a 492px
  document and scroll the whole page sideways; the rule the app has to satisfy
  is `document.scrollWidth <= document.clientWidth` at 375px. The sidebar's
  shrink factor therefore stays `1` at any basis.
- **Only the right pane grows.** With the sidebar on `flex: 1 1 320px` it took
  its share of every spare pixel, so on a wide screen a column of short selects
  grew past 500px while the forest plot beside it stayed small. Grow factor `0`
  sends the spare width to the pane that holds the plots.

`tests/testthat/test-step2-layout.R` pins both — there is no browser driver
here, so what is asserted is the CSS, not the measurement.

#### 3.3.2 Step header

The title, and nothing else. See §3.2.2.

#### 3.3.3 Inputs (sidebar)

One `pma_card("Model configuration")` holding a `bslib::accordion(multiple = FALSE)` of four panels, then a sticky action bar.

| Panel (`value`) | Contents | Open on build |
|---|---|---|
| Outcome (`outcome`) | `outcome_name`, `small_values`, `outcome_type`, `outcome_follow_up`, `outcome_unit` (continuous only) | always |
| Data mapping (`mapping`) | `col_studlab`, `col_treat`, `arm_assignment_ui`, `col_n`, `col_event` (binary) / `col_mean` + `col_sd` (continuous) | never |
| Model details (`model`) | `sm_bin` / `sm_cont_ui`, `model`, `method`, `method_tau`, `random_ci`, `incr` | never |
| Subgroup (`subgroup`) | `subgroup_col`, `subgroup_order_ui` | never |

- **Exactly one panel is open at a time** (`multiple = FALSE`), and on arrival
  it is Outcome — the only panel holding something no default can supply.
  Opening another closes it, so the sidebar is never taller than the question
  being answered and the sticky action bar stays in view. The open state no
  longer depends on `state$ma`: Data mapping used to open alongside Outcome
  until a pooled object existed, which made the two together longer than a
  laptop viewport on the very first visit.
- **`arm_assignment_ui` MUST carry `outputOptions(suspendWhenHidden = FALSE)`.**
  It is the only source of `experimental_label` and `control_label`, it lives in
  a panel that is never open on build, and the `ma` reactive bails on NULL arm
  labels. Under Shiny's default the closed panel suspends the output, the two
  selects are never created, and Run analysis does nothing at all — no result,
  no message, no log. The other two `uiOutput`s in closed panels do not need the
  exemption and do not have it: `sm_cont` falls back to `run_ma()`'s own default
  and `subgroup_order` is guarded on `NULL`, so neither can stop an analysis.
- **No exit from the `ma` reactive is silent once the reviewer has asked for a
  run.** The arm-label guard used to `return(NULL)` saying nothing, which is
  what turned the suspension above into an app with an inert button. It now
  names what is wrong — arms unset, or the same arm value picked twice — under
  the same `!auto || ever_run_requested` condition the required-fields branch
  uses, so a first page load stays quiet and a reviewer who pressed Run
  analysis never gets nothing back.
- **`multiple = FALSE` is load-bearing for `www/required-fields.js`.** It is
  what puts `data-bs-parent` on every `.accordion-collapse`, and that attribute
  is how Bootstrap closes the open sibling. The reveal in §3.3.6 therefore goes
  through `bootstrap.Collapse.getOrCreateInstance(panel).show()`; adding the
  `show` class by hand bypasses the sibling-closing and leaves two panels open.
  The hand toggle survives only as a fallback for a page without Bootstrap's
  JS bundle, because that file must degrade rather than throw.
- **The six column-mapping selects are selectize widgets**, not
  `selectize = FALSE`. A native `<select>` is token-styled while closed, but its
  open list is drawn by the operating system, outside the document, and no rule
  in `www/shadcn.css` can reach it — so those six changed appearance at exactly
  the moment they were being read. `subgroup_col`, `sample_dataset` and
  `rare_primary_method` stay native. The server fills all of them with
  `updateSelectInput()`, which preserves the current selection for either
  flavour.

- **Outcome type is identity, not mapping.** `outcome_type` sits in the
  Outcome panel: it says what kind of thing is being rated, and decides which
  of the panel's own optional fields (`outcome_unit`) applies.
- **There is no outcome row-filter.** Step 2 once rendered a `selected_outcome`
  select whenever the data held more than one `outcome` value, and analysed
  only the rows matching it. On a continuous review where each study measured
  its own instrument — PHQ-9, HAMD, BDI — that sliced the data down to one
  study, silently: the reactive returned `NULL` with no notification and no
  `state$ma_blocked` entry, so Step 3 could not explain the empty screen
  either. `outcome` is a descriptive column now and every study pools. The one
  case that cannot pool — one study under two outcomes — is `run_ma()`'s to
  reject, and its message reaches the reviewer through the existing
  `tryCatch()` around the run.
- **Every abbreviation in this panel is spelled out on sight** — `OR (odds
  ratio)`, `RoM (ratio of means)`, `MH (Mantel-Haenszel)`, `REML (restricted
  maximum likelihood, default)`. Only the **label** carries the expansion; the
  input **value** stays the bare code (`"OR"`, `"REML"`), which is what every
  branch and every saved outcome compares against. Labels are built from
  `PMA_ABBREVIATION_EXPANSIONS` in `R/step2_ma.R` via `pma_spell_out()` /
  `pma_spelled_choices()`, never typed beside the value, so the control and
  `step2_model_summary_line()` (§3.3.4) cannot word the same code differently.
  A code the table does not carry is shown unchanged — `Inverse` and `Peto` are
  names, not abbreviations, and must not gain an empty bracket.
- **`method_tau` offers six estimators** — `REML` (default), `PM`, `DL`, `SJ`,
  `ML`, `EB` — labelled with their names in the select. **`random_ci`** sits
  beside it in the same `input.model == 'random' && input.use_rare_workflow
  != true` panel: `auto` (default), `hk`, `classic`, mapped into `run_ma()`'s
  `hakn` as `NULL` / `TRUE` / `FALSE`. `auto` is `run_ma()`'s own `k >= 3`
  rule, so the default run is byte-for-byte what it was before the control
  existed.
- **A blank mapping select cannot be caught at build time** — the selects are
  populated by the server *after* this UI is built, so at build time they are
  all blank whatever the data holds. One that is still blank when the reviewer
  actually asks for an analysis is handled from the other end, by
  `www/required-fields.js` (§3.3.6), which opens the panel holding it.
- **The continuous summary measure defaults to `SMD`**, in
  `output$sm_cont_ui` and in the observer that hides `RoM` when the mean column
  holds a non-positive value. A review pooling continuous outcomes at all is
  usually pooling several instruments (PHQ-9, HAMD, BDI), and a mean difference
  across two scales is not a quantity; `MD` stays one click away. Both places
  build the radio with `pma_spelled_choices()`, so changing the mean column
  cannot silently strip the spelled-out labels the bullet above requires.
- **Every input id is unchanged by the restructure.** Step 3 reads most of
  these off `input$` directly, so a rename is silent everywhere else;
  `tests/testthat/test-step2-layout.R` asserts each id renders.
- **Sticky action bar** (`.pma-step2-actions`, `position: sticky; bottom: 0`)
  closing the card, holding `actionButton("run_ma")` and
  `checkboxInput("auto_rerun")`. The sidebar is taller than a laptop viewport
  with every panel open, and the primary action used to sit at the bottom of
  it, so changing a model setting meant scrolling back down to act on it.
- **`auto_rerun` defaults ON, and OFF once rare events are detected**
  (`step2_ui()` seeds it from `state$rare_diagnostics`; a one-time
  `updateCheckboxInput()` in `step2_server` makes the live transition, once per
  detection episode). Rare-event data puts `run_rare_ma()`'s multi-method suite
  on every re-run, which is minutes on the shared shinyapps.io tier, so it must
  not ride the debounced input bundle unasked.
- **`run_ma` is a one-shot request, not a latch, and `auto_rerun` OFF therefore
  keeps meaning what it says.** `input$run_ma` is an actionButton counter: it
  only ever increases, and nothing in the app resets it. The `ma` reactive
  therefore compares it against the count it has already served
  (`run_clicks_spent`, via the pure `step2_run_request()`), and only an
  *increase* authorises a run. Reading the counter as `> 0L` instead — which is
  what it did — latched TRUE at the first press, so from then on every change
  to the debounced input bundle re-ran the analysis no matter what the checkbox
  said, silently defeating the OFF default above. The two states, spelled out:
  - **`auto_rerun` ON** — every debounced change re-runs, presses are
    irrelevant to the gate. Unchanged by the one-shot rule; this is the common
    path.
  - **`auto_rerun` OFF** — a run happens only for an unserved press. After the
    run, changing a model setting re-enters the reactive, which returns `NULL`;
    `observeEvent(ma())` ignores `NULL`, so the previous result and its Step 3
    rating stay on screen until the reviewer presses Run analysis again.
- **A pending press survives the cheap guards.** It is spent immediately before
  `run_ma()` is called, not at the gate, so a press blocked by a missing
  required field or by arm labels left over from the previous dataset is served
  the moment that blocker clears — the reviewer does not have to press twice
  while `arm_assignment_ui` re-renders. A press that reaches `run_ma()` is
  spent whether the run returns a fit or the `tryCatch()` turns it into a
  notification.
- **The spent baseline follows a rebuilt counter back down.** app.R's
  `step_body` is a `renderUI`, so a Step 2 → 3 → 2 round trip rebuilds the
  button and its counter starts again at 0. `step2_run_request()` floors the
  baseline at the observed count for that reason; without it the reviewer's
  next press is numbered 1, the stale baseline is 3, and Run analysis is inert
  again.
- **The warning branches keep the latch on purpose.** "Has the reviewer asked
  for an analysis at all?" is a different question from "is a press waiting to
  be served", and the required-fields and arm-assignment toasts want the first
  one (`ever_run_requested`, plain `input$run_ma > 0L`). Before the first
  request a half-filled form is a normal state and stays quiet; afterwards it
  is worth a toast however the reviewer got back to it. `required_touched`
  (§3.3.6) is the same latch for the same reason.

#### 3.3.4 Outputs (right pane)

Above the tabs, `uiOutput("ma_model_summary")`: one line naming the model that
produced the numbers, e.g. `Random effects, REML (restricted maximum
likelihood), Hartung-Knapp CI, k = 12` (`classic (Wald) CI` when Hartung-Knapp
is off, `Common (fixed) effect, k = 12` for a common-effect fit). The estimator
is its own comma-separated part rather than `Random effects (REML)` because it
is spelled out from the same table the control uses (§3.3.3), and a nested
bracket reads worse than a fourth item. Built by the pure
`step2_model_summary_line()`, which
reads `random` / `method.tau` / `method.random.ci` / `k` **off the fitted
object** rather than off the controls, so it cannot drift from what actually
ran, and reports the rare-events primary fit correctly when that is what is on
screen. It sits above the tabset rather than inside "Text results" because the
Hartung-Knapp adjustment was applied silently at `k >= 3` for as long as it
existed: a setting nobody can see is a setting nobody can question.

Tabset with 3 tabs:

- **Forest plot** — `plotOutput("forest", height = "auto")` rendered via `pmatools::plot_forest(state$ma, title = input$forest_title, label_e = ..., label_c = ..., xlim = parsed_input)`. Auto-height: `350 + 30 * k` pixels.
- **Funnel plot** — `plotOutput("funnel")` via `pmatools::plot_funnel(state$ma, show_egger = TRUE)`.
- **Text results** — `verbatimTextOutput("ma_summary")` showing `summary(state$ma)`.

Below the tabset: the collapsible **"Forest plot display"** panel, built by the
single `pma_forest_display_panel(prefix)` in `R/ui_helpers.R` and shared with
each of the four Step 3 domain tabs (`prefix = NULL` gives Step 2's unprefixed
ids). It holds the title, the two arm labels, the two "Favors …" labels, the
x-min / x-max overrides, the two blank-row spinners, the Mean / SD decimal
spinners and the per-arm column checkbox.

- **`digits_mean` / `digits_sd` both default to 1**, matching
  `plot_forest()`'s own defaults (SPEC.md §4.3) rather than `{meta}`'s 2 and 4.
  They are coerced by `pma_forest_digits()` before they leave the app: a blank
  field is `NA`, and `NA` reaching `meta::forest()` costs the Mean and SD
  columns rather than raising. Only a continuous outcome draws those columns,
  but the two fields are shown unconditionally — the panel is shared with the
  Step 3 tabs, and a control that appears and disappears with the outcome type
  is harder to find than one that is inert.

- **The blank-row spinners default to 1 above and 0 below.** Above = 1
  reproduces the blank row `meta::forest()` draws on its own. Below = 0 is
  tighter than `plot_forest()`'s own default, which is `NULL` = derive from the
  drawn content (SPEC.md §4.3): `.auto_addrow_below()` reserves 2–4 rows for the
  axis band, the Favors labels and the xlab, and that clearance is whitespace
  most forests do not need. Clearing the field sends `NULL` and restores the
  derived spacing, which is the fix when the heterogeneity text lands on the
  x-axis — most likely with the per-arm columns hidden. `pma_addrow_below()`
  does the coercion; `0` is a real answer to it and only a blank, a negative or
  a non-number becomes `NULL`.

- **Layout is `.pma-display-grid`**, four columns. A child that needs the whole
  row carries `.pma-span-4` (title, the blank-row hint, the checkbox); a child
  that is one of a **pair** carries `.pma-span-2` (x-min / x-max, and the two
  blank-row spinners). Without the pair class those two rows filled columns 1–2
  and left 3–4 empty, so they read as a misaligned rung between the four-column
  rows around them. The grid folds to two columns under 760px, because
  `minmax(160px, 1fr)` is a hard floor and four such tracks cannot fit a phone.
- **The title is a `textAreaInput`, not a `textInput`.** `plot_forest()` honours
  a newline in the title as an explicit line break (SPEC.md §4.3), and
  `<input type="text">` cannot carry one — the HTML value sanitisation
  algorithm strips CR/LF, so the break would be lost both when the user typed
  it and when the Step 3 autofill pushed a stratified default in.
  `updateTextInput()` and `updateTextAreaInput()` send the same message, so
  `pma_autofill_text()` drives the field unchanged.
- **The Step 3 copies prefill a stratification suffix** onto the outcome name
  (`.forest_title_suffix` in `R/step3_grade.R`, applied by
  `pma_autofill_forest_panel()` and only to a title the user has not edited).
  Risk of Bias and Indirectness append `"\n(stratified by <domain>)"` — on its
  **own line**, because on one line the wrapped title reached down into the
  `Events / N / OR (95% CI) / Weight` headings. Inconsistency and
  **publication bias append nothing**: the
  publication-bias figure's own subgroup heading already says "available" vs
  "missing results", so the suffix repeated it at the cost of a title line.

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
**opens the panel** containing one — through
`bootstrap.Collapse.getOrCreateInstance(panel).show()`, so the panel that is
currently open closes with it (§3.3.3), and only when `armed`, and only once
per panel per DOM build. Before the reviewer has asked for an analysis the panel
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

#### 3.4.3a Per-study Risk of Bias and Indirectness editors (v0.5.1)

Both tabs carry a collapsed `<details>` holding a `DT` grid over
`state$rob_table`, one row per study, plus four bulk buttons. The cell used to
be free text with a caption telling the reviewer to type `low` / `some` /
`high`. **It is a dropdown**, and a reviewer cannot enter anything else.

| Tab | Column | Offered |
|---|---|---|
| Risk of Bias | RoB 2 judgment | (not set) · Low risk of bias · Some concerns · High risk of bias |
| Indirectness | Indirectness | (not set) · Low indirectness · Some indirectness · High indirectness |

**The two label sets are not the same vocabulary and must never be worded
alike.** Cochrane RoB 2 defines exactly three judgments and the Risk of Bias
column shows its words verbatim; the app can offer them unconditionally
because `study_design` is hardcoded to `"RCT"`, so ROBINS-I evidence never
reaches it. Indirectness has no such instrument — those three are pmatools'
own forest-plot strata (`rob_strata()`), worded so that nothing on the tab
reads as a published judgment. Both store `"low"` / `"some"` / `"high"` in
`state$rob_table`, which is what the bulk buttons, Step 1 and `grade_meta()`
already exchange.

**Why a rendered `<select>` and not DT's own editor.** DT 0.34's `editable=`
injects an `<input type="text">` — or `number` / `textarea` / `date` — and has
no dropdown type; its factor/selectize support belongs to column *filters*.
The Publication bias tab reaches its vocabulary with a `<datalist>` bolted to
that injected input (§3.4.8), but a datalist is autocomplete and still accepts
anything typed, which is the failure this control exists to remove. So the
column is built by `pma_study_level_select()` (`R/ui_helpers.R`), rendered with
`escape =` naming every *other* column, and left out of `editable`. One
delegated `change` handler, emitted once per Step 3 body by
`pma_study_level_script()`, calls `Shiny.setInputValue()` with the row index
the cell was **rendered** with — never DT's `col`, which counts hidden columns
and has already cost this app a bug (§3.2.3). The handler binds under the
`.pmaLevel` namespace and `off()`s itself first: `app.R` rebuilds this whole
body on every step change and Shiny re-executes the inline script inside it,
so a plain `on()` would stack a handler per Step 3 → 2 → 3 round trip and
report each change once per rebuild.

The server observers (`input$step3_rob_choice`, `input$step3_indir_choice`)
still validate the value against the offered set before writing. The dropdown
cannot produce anything else; the check is against a hand-crafted message, and
an unrecognised value is dropped rather than written, because a value that
reached `state$rob_table` unrecognised would land the study in the `"unknown"`
stratum where the app shows no warning.

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

- `selectInput("rob_override", "Override RoB judgment", pma_judgment_choices(blank_label = "(use auto)"))` — the four Core GRADE levels, values and labels per §3.4.11
- `sliderInput("rob_dom_threshold", "Dominance threshold", min = 0.5, max = 0.7, value = 0.6, step = 0.05)`
- ~~`sliderInput("rob_inf_threshold", …)`~~ — deleted in 0.5.1; see §3.4.11
- `radioButtons("small_values", "Small values are...", c("Desirable (e.g., mortality, severity)" = "desirable", "Undesirable (e.g., response rate)" = "undesirable", "(use auto)" = ""))`

#### 3.4.5 Educational copy — Inconsistency

> **How this is judged.** GRADE rates down for inconsistency when there are *important differences in effect across studies* AND those differences cannot be explained. The BMJ Core GRADE 3 flowchart asks three questions in sequence. **Step 1**: Are there important differences in point estimates AND limited overlap of confidence intervals? If no, do not rate down. If yes, continue. **Step 2**: Where do the point estimates fall relative to the **clinical decision threshold**? If a clear majority sits on one side of the threshold, the direction of effect is consistent — do not rate down. If a substantial proportion fall on opposite sides, continue. **Step 3**: Can the opposite-sided difference be explained by a credible subgroup analysis (e.g., RCTs vs observational, adults vs children)? If yes, present the subgroups separately and do not rate down; if no, rate down for serious inconsistency. I² is shown as *supportive context only* — the decision is anchored in clinical judgment about whether the differences are important and whether the directions are consistent. (τ² and Q-test results are also displayed for transparency but never drive the judgment.) Reference: BMJ Core GRADE 3 (Guyatt et al., 2025).

**Auto-evaluation result template:**

```
Method: auto (statistical proxies for the BMJ flowchart)

AUTO Step 1: I² = {{i2_pct}}%
  → {{"important differences detected (I² > 25%)" | "no important heterogeneity (I² ≤ 25%)"}}

{{#if has_mid}}
AUTO Step 2 (vs ±Threshold = ±{{threshold}}):
  Zone counts (k = {{k}}): above_mid = {{n_above}}, trivial = {{n_trivial}}, below_mid = {{n_below}}
  Largest one-side proportion = {{pct_one_side}}%
  → {{"majority on one side" | "opposite sides"}}
{{else}}
AUTO Step 2 (vs null = 0; Threshold not specified):
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

**Recommended input — single decision-threshold field (always shown, dynamically labeled):**

The threshold moved to the Configuration tab (§3.4.0) and the input is `threshold_cont` on the continuous branch, `threshold_abs` / `threshold_ratio` on the binary one. The label and default value adapt to the meta-analysis effect measure (`meta_obj$sm`). The Shiny app calls `pmatools::suggest_threshold(state$ma)` to pre-fill, and renders its `$source` as a badge so a pmatools convention is never presented as a Core GRADE number:

| `sm` | Input label | Default | `$source` | Help text |
|---|---|---|---|---|
| OR | "Threshold (as OR ratio, e.g., 1.25 = 25% relative odds change)" | 1.25 | pmatools convention | "An OR of 1.25 vs 1.0 represents a 25% relative change in odds — a typical small but clinically meaningful effect." |
| RR | "Threshold (as risk ratio, e.g., 1.20 = 20% relative risk change)" | 1.20 | pmatools convention | "An RR of 1.20 vs 1.0 represents a 20% relative change in risk." |
| HR | "Threshold (as hazard ratio)" | 1.20 | pmatools convention | "An HR of 1.20 vs 1.0 represents a 20% relative change in hazard." |
| RoM | "Threshold (as ratio of means, e.g., 1.10 = 10% ratio change)" | 1.10 | pmatools convention | "A 10% ratio of means is a typical small clinically meaningful difference for continuous outcomes." |
| SMD | "Threshold (in standardized units, e.g., 0.20 = Cohen's small)" | 0.20 | **Core GRADE 6** | "Core GRADE 6's own threshold for a small and important effect, shown with the paper's own scepticism note about SMD variability." |
| MD | "Threshold (in outcome units, e.g., 3 PHQ-9 points)" | **0.20 × pooled SD = {{computed}}** | pmatools convention | "Auto-suggested as 0.20 × pooled SD (Cohen's small in raw units). Replace with a published threshold for your outcome if available." |
| ARD | "Threshold (as absolute risk difference, e.g., 0.05 = 5%)" | 0.05 | pmatools convention | "A 5% absolute risk difference is a typical small clinically meaningful effect." |

Note below the input: "*This threshold is shared with Risk of Bias, Inconsistency and Imprecision — enter once, used for all three.* The default is a conventional value; please replace with a published or expert-derived threshold for your specific outcome whenever possible."

**Scale (binary branch only):** `radioButtons("threshold_mode", c("absolute", "relative"))` on the Configuration tab, with the absolute scale recommended and converted to the analysis scale at the control-group risk. The continuous branch has one box and no scale choice. `threshold_scale` is what `grade_meta()` receives; the app derives it rather than asking.

**Manual flowchart inputs (BMJ-faithful path; collapsed by default):**

The BMJ flowchart can be driven manually for a fully clinically-informed judgment. Each Step's question is asked in plain language:

- *Step 1: Are there important differences in point estimates AND limited overlap of confidence intervals?*
  - `selectInput("inconsistency_ci_diff", c("(use auto)" = "", "No" = "no", "Yes" = "yes"))`
  - Help: "This is a clinical-visual judgment. Look at the forest plot above: do the point estimates differ by a clinically meaningful amount, and do the CIs fail to overlap substantially?"
- *Step 2: Where do the point estimates fall relative to the clinical decision threshold?*
  - Visible only if Step 1 = "yes"
  - `radioButtons("inconsistency_threshold_side", c("Majority on one side of the threshold" = "majority_one_side", "Substantial proportion on opposite sides" = "opposite_sides"))`
- *Step 3: Is the opposite-sided difference explained by a credible subgroup analysis?*
  - Visible only if Step 2 = "opposite_sides"
  - `radioButtons("inconsistency_subgroup_explained", c("Yes" = "yes", "No" = "no"))`

**Override controls (collapsed by default):**

- `selectInput("inconsistency_override", "Override Inconsistency judgment with a single value", pma_judgment_choices(blank_label = "(use flowchart)"))`

#### 3.4.6 Educational copy — Indirectness

> **How this is judged.** Indirectness *cannot be automated* — it requires expert judgment about whether the trial evidence applies to the question of interest. GRADE asks you to consider four things: **Population** (do trial participants resemble the target patients?), **Intervention** (is the intervention deliverable as studied?), **Comparator** (is it representative of usual care?), and **Outcome** (is it patient-important, or a surrogate?). The app defaults to **"No concerns"**, but this is the only domain whose value comes purely from your judgment, so please review before exporting. Reference: BMJ Core GRADE 5 (Guyatt et al., 2025).

**Banner (shown until user clicks any rating, including re-clicking "No"):**

```
⚠ Auto-defaulted to "No" — please review and confirm or override below.
This is the only domain that cannot be informed by your data.
```

**Required (always visible):**

- `radioButtons("indirectness", "Overall indirectness rating", pma_judgment_choices(include_blank = FALSE))`

**Educational sub-prompts (collapsed by default, do not affect rating):**

- `radioButtons("indir_population", "Is the trial population sufficiently similar to the target patients?", c("Yes", "Some concern", "Serious concern"))`
- `radioButtons("indir_intervention", "Is the intervention deliverable as studied?", same)`
- `radioButtons("indir_comparator", "Is the comparator representative of usual care?", same)`
- `radioButtons("indir_outcome", "Is the outcome patient-important (vs. surrogate)?", same)`

State logic: `indirectness_reviewed <- reactive(input$indirectness_clicked >= 1)`. The banner is hidden once `indirectness_reviewed()` is TRUE.

#### 3.4.7 Educational copy — Imprecision

> **How this is judged.** Imprecision asks whether the **pooled estimate's 95% confidence interval** is narrow enough to support a clinical decision — distinct from Inconsistency, which asks how much true effects vary across studies. The algorithm checks two conditions: **(a)** does the pooled 95% CI cross the null value? and **(b)** is the **Optimal Information Size (OIS)** met? OIS is the sample size a single well-powered RCT would need to detect the **decision threshold** (the same threshold set on the Configuration tab and shared with Risk of Bias and Inconsistency — enter it once). If both conditions are met (CI does not cross null AND OIS is reached), no downgrade. If only one fails, rate down 1 level. If both fail, rate down 2 levels. Reference: BMJ Core GRADE 4 (Guyatt et al., 2025).

**Auto-evaluation result template:**

```
• 95% CI: {{ci}} — {{"crosses null" | "does not cross null"}}
• Threshold: {{threshold_value | "(not specified)"}}
• OIS target: {{ois_target}} {{"events" | "participants"}} (auto-computed); observed: {{ois_observed}} → {{"met" | "not met" | "not assessable"}}
Resulting judgment: {{judgment}}
```

**Override controls (collapsed by default):**

- `numericInput("ois_p0", "Baseline (control) event rate for OIS", value = NA, min = 0, max = 1)` *(binary only)*
- `numericInput("ois_sd", ...)` *(continuous only)*, rendered by `output$ois_sd_ui` and **prefilled from the data**, because `.calc_ois()` needs δ and σ on the same scale:
  - **MD / RoM** — label "Pooled SD for OIS (auto from data)", value `compute_pooled_sd(state$ma)`. The threshold is on the raw outcome scale, so σ has to be too.
  - **SMD** — label "SD for OIS (1: the SMD is already expressed in SD units, so the threshold above is standardized)", value `1`. An SMD threshold is *already* standardized; prefilling the raw pooled SD there inflated the target N by σ² and could flip Fig 4's large-effect path from `not_serious` to `serious`/`very_serious` through the "< 30% of OIS" rule.
  - A value the reviewer types always wins, for every measure.
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

#### 3.4.8a Missing results (RoB-ME) — the status dot's algorithm

Built in 0.5.1 as `.pubias_missing_tipping()` / `.pubias_missing_dot()`
(`R/pubias_missing.R`) — in the package, not the app, so the arithmetic below
is unit-tested against this section rather than only rendered.

The dot on the RoB-ME tab (§3.4.12's tabset) answers one question: **how far
from the observed pooled effect would the missing studies have to lie before the
conclusion changes?** Far means the missing evidence cannot overturn the result;
near means it can. Nothing here rates the domain — RoB-ME is not part of the
Core GRADE algorithm and the tab already says so.

**Inputs.** `state$pubias_missing` (`studlab`, `n`, `results_known`, `source`)
and `state$ma`. The threshold `T` is the Core GRADE threshold already chosen for
the outcome (null or MID, §3.4.10a); no second threshold is introduced.

**Imputing a missing study's standard error.** Borrow from the observed studies
rather than assuming an SD, a control-group risk or an allocation ratio — none
of which exist for every effect measure:

```
c_med = median(seTE_i * sqrt(n_i))     over observed studies
se_j  = c_med / sqrt(n_j)              n_j blank -> median(seTE_i)
```

`se ∝ 1/sqrt(n)` holds for SMD, MD, log OR and log RR alike, so one formula
covers every measure the app pools. This is the reviewer's "assume the same SD"
generalised to measures that have no SD.

**The tipping point is closed-form.** Assume the `m` missing studies share one
effect `δ`, and **hold `tau2` at its observed value** — placing every missing
study at a single `δ` would otherwise shrink it artificially, which flatters the
result:

```
W_obs = 1/seTE_pooled^2
w_j   = 1/(se_j^2 + tau2)          W_miss = sum(w_j)
TE_new(δ) = (W_obs*TE_obs + W_miss*δ) / (W_obs + W_miss)
se_new    = 1/sqrt(W_obs + W_miss)          -- independent of δ
```

`se_new` not depending on `δ` is what makes this cheap enough to redraw on every
edit of the table: the interval's width is constant, `TE_new` is linear in `δ`,
and the crossing solves directly as
`δ* = (W_tot*(T ± 1.96*se_new) − W_obs*TE_obs) / W_miss`. No root-finding.

**Ordered decision, cheapest and most decisive first:**

```
1. m = 0                                            -> 🟢
2. no prediction interval (tau2 = 0, k < 3) or
   se imputation impossible                         -> ⚪
3. δ = TE_obs already changes the conclusion        -> 🔴  (precision alone)
4. no δ changes the conclusion                      -> 🟢  (cannot be overturned)
5. direction gate: δ* outside the suspected region  -> 🟢
6. magnitude: δ* against the intervals below        -> 🔴 / 🟡 / 🟢
```

**Steps 3 and 4 MUST precede step 6.** Adding studies shrinks `se_new`, so a
body of evidence can cross the threshold on precision alone, with the missing
studies reporting exactly what the observed ones did; step 6 asked on its own
would call that case reassuring.

**"The conclusion", precisely, and when step 4 can fire.** The conclusion is
which side of `T` the pooled 95% interval lies on: `above` when
`TE − 1.96*se > +T`, `below` when `TE + 1.96*se < −T`, `spans` otherwise. Both
the observed and the new conclusion are read with `1.96*se`, never off the
reported confidence limits — a Hartung-Knapp interval is wider than `1.96*se`,
and comparing one against the other would report a change that came from the
quantile rather than from the missing studies. The reported limits are used at
step 6 only, where the CI is a *region* `δ*` is judged against rather than a
decision rule.

Step 4 is a guard, and fires only when `W_miss = 0`. `TE_new` is affine and
increasing in `δ` and covers the whole real line, and each conclusion is an
interval in `TE_new`, so a finite `δ*` exists whenever the missing studies
carry any weight at all. Studies too imprecise to carry any leave
`TE_new(δ) = TE_obs` for every `δ`, which is the one case in which the
conclusion genuinely cannot be overturned — 🟢, not ⚪, because the model gave
an answer rather than failing to run. The step keeps its place in the order
regardless: step 6 divides by `W_miss` too, and an infinity compared against
the prediction interval would come out 🟢 by accident rather than by reasoning.

**Step 6 is anchored on the prediction interval, not on a fixed effect size.**

| `δ*` lies | dot | reading |
|---|---|---|
| inside the pooled 95% CI | 🔴 | an ordinary missing result changes the conclusion |
| outside the CI, inside the 95% prediction interval | 🟡 | a plausible missing result changes it |
| outside the prediction interval, or unreachable | 🟢 | only a study unlike any observed changes it |

A fixed cutoff in SMD units was considered and rejected: SMD does not exist for
a binary outcome, so it cannot be the app's one rule. The prediction interval is
already computed, needs no cutoff, and works on whatever scale the model was fit
on. It is conservative in the safe direction — `δ*` is the *mean* of `m` studies
but is judged against the spread of a single one, which errs toward 🔴.

**Step 5 makes direction a gate, not a second scale.** `results_known` records
*why* a result is missing, which is what RoB-ME is actually about, and three of
its five labels also imply *which way* the missing effect lies. With the null at
0 (log scale for ratio measures) and `s = sign(TE_obs)`:

| `results_known` | mechanism | suspected region for `δ` |
|---|---|---|
| Not measured | none | unconstrained, but step 6 **caps at 🟡** |
| Reported but data not extractable | unknown | unconstrained |
| Measured but not reported (suspect P > 0.05) | present | null-ward, `δ*s < TE_obs*s` |
| Measured but not reported (suspect P < 0.05) | present | further out, `δ*s > TE_obs*s` |
| Measured but not reported (opposite direction) | present | `δ*s < 0` |
| free text | unknown | unconstrained |

Take the **union** across rows. One unconstrained row makes the union everything
and the gate never fires, which is the conservative answer and the right one:
an unconstrained row means no direction can be ruled out.

The gate earns its place on cases like this one: the missing results are
suspected null-ward, but `δ*` is further from the null than the observed effect.
Nothing the missing studies could plausibly report moves the conclusion **in the
direction they are suspected of lying**, so the dot is 🟢 whatever its magnitude
would have said. This is the direction-of-bias step of the Risk of bias
flowchart, asked of missing evidence instead of high-RoB evidence.

**`Not measured` is capped at 🟡 and nothing else is.** An outcome that was never
assessed cannot have been suppressed for what it showed, so its absence is
incompleteness rather than bias, and incompleteness does not earn the strongest
warning the tab can give. `Reported but data not extractable` — the label
auto-seeded onto every NA-TE row, and therefore the most common one — is
deliberately **not** capped: "not significant, data not shown" is textbook
selective reporting and the label cannot rule it out. The default behaviour is
therefore magnitude-driven, which is what the dot is for.

The cap applies only when **every** row is `Not measured`. One row with any
other label is a row whose absence could be selective, and capping the whole
dot because that row shares a table with never-assessed outcomes would suppress
exactly the warning the tab exists to give. The cap is also a ceiling and never
a floor: it turns 🔴 into 🟡 and touches nothing else.

#### 3.4.8b The scale the trim-and-fill comparison runs on

The direction-of-bias rules measure magnitude as `|TE|` and zones as `±T`, both
of which mean "distance from the null" **only on a scale whose null is zero**.
Reading the original-versus-adjusted comparison off the raw summary measure
would break that for every ratio: an odds ratio's null is 1, so `|OR| = 2.0` and
`|OR| = 0.5` — equidistant from the null in fact — come out four-fold apart.

**So the comparison runs on whichever scale puts the null at zero, and for a
binary outcome that is the absolute risk difference the app already computes.**

| outcome | scale the check runs on | why |
|---|---|---|
| binary (OR, RR, and any measure on a `metabin`) | absolute risk difference per 1,000 at the outcome's baseline risk `p0` | null is 0, and it is the scale the reviewer's threshold is already stated in |
| continuous difference measures (MD, SMD) | the internal scale unchanged | already a difference with the null at 0 |
| RoM | the internal (log) scale | a ratio on a continuous outcome has no event rate to convert to; log puts its null at 0 |

No new arithmetic is introduced for the binary row. `step3_ard_equivalence()` /
`step3_p1_from_ratio()` already convert an effect to an event rate at `p0`, the
absolute difference is `p1 − p0`, and `step3_threshold_suggestions()` already
carries the threshold as `absolute1000` (`threshold_abs_state()` is where it is
stored). The check therefore compares two absolute risk
differences against a threshold stated in the same units, which is also the form
the Configuration tab shows the reviewer (§3.4.10a) — the dot and the number the
reviewer read are on one scale, not two.

Built in 0.5.1 as `.pubias_trimfill_scale()` (`R/pubias_status.R`). It owns the
**decision** — which scale, and when the answer is ⚪ — and the event-rate map
is **injected**: `step3_p1_from_ratio()` lives in the app, and a second copy in
the package is exactly the pair of implementations that drift. With no map
supplied a binary outcome is ⚪, never converted by a guess. A measure already
stated as an absolute difference (`RD` / `ARD`) needs no map, only the change of
unit to per 1,000.

`PMA_ROB_INFLATION_THRESHOLD` is unchanged and still shared: it is a *ratio* of
magnitudes, so it transfers to any scale whose null is zero without being
restated.

**Where no baseline risk is available** — a `metabin` whose control arm gives no
usable `p0`, or a reviewer who has cleared it — the dot is ⚪ rather than falling
back to the internal scale. A silent scale change is what this section exists to
prevent.

#### 3.4.9 Final certainty summary

Below the accordion, a `pma-card` with:

- Large heading: `Final certainty: {{certainty}} {{symbol}}` (symbol = ⊕⊕⊕⊕ etc.)
- Color-coded by CINeMA pastel palette
- "Why this rating?" expandable showing per-domain judgment + 1-line justification
- Embedded SoF preview rendered from `pmatools::sof_table()` via `htmltools_value(ft)`,
  called with the review's own arm labels, resolved by `pma_arm_labels(state)`
  (`R/ui_helpers.R`) from `state$arm_e` / `state$arm_c`. **One resolver, two
  steps:** it was a closure inside `step4_server()` until 0.5.1, so this preview
  showed `With control` and a Core GRADE 6 Box 1 subject of `Treatment` one screen
  before Step 4's combined table showed the reviewer's own words. The rare-event
  alert and the two footnotes under the table take the same labels, because each
  of them names a column by its header

#### 3.4.10 Display options card

- `checkboxInput("prediction", "Show 95% prediction interval in Effect column")`
- The responder-presentation controls left this card for the Configuration tab; see §3.4.10a. What remains here is a read-only echo (`output$display_options_config_note`) saying where they went.

#### 3.4.10a Presentation of a continuous outcome — Configuration tab

Rendered by `.responder_block()` (`R/step3_threshold.R`), **below** the Decision threshold section on the continuous branch of `output$threshold_panel`. The order is load-bearing: the threshold drives the rating, the presentation does not, and the old order — conversion first — read as though converting were a step on the way to a rating.

- `radioButtons("sof_presentation", "How should the Summary of Findings table present this outcome?", c("The <sm> itself, on its own scale" = "effect", "The proportion of responders, converted with Chinn's formula (Core GRADE 6 option 2)" = "responder", "Both, on two rows of one outcome: the <sm> on its own scale above and the proportion of responders below (what Core GRADE 6 recommends)" = "both"), selected = "both")` *— SMD/MD outcomes only; other measures get a note saying the conversion is undefined for them and no radio at all.* The default is `"both"` because that is the pairing Core GRADE 6 recommends. It commits every continuous outcome to a responder proportion, and therefore to the `responder_p0_confirm` gate below: the Configuration tab's Next stays shut until the reviewer confirms the app-convention proportion or replaces it with a rationale. That is deliberate — the assumption is examined once per outcome rather than defaulted past.
- **`"both"` is the presentation Core GRADE 6 recommends**, and it is what the two-way version of this radio could not offer: the block used to concede that pmatools showed one presentation at a time and that Core GRADE 6's agreement check was therefore out of reach. It maps to `sof_table(convert_smd_to_or = TRUE, keep_effect_scale = TRUE)` — one outcome rendered as two table rows, the effect above and the dichotomised reading below, with the columns that do not split merged over the pair (`SPEC.md` §4.6).
- **`"both"` is the default.** The premise that a continuous outcome must be dichotomised before it can be *rated* is still false — `convert_smd_to_or` reaches `sof_table()` only, `grade_meta()` never sees it, and Imprecision is rated on the SMD/MD against `threshold_cont` whichever of the three is chosen, which the Decision threshold section says on screen. What changed is the answer to the objection that `"both"` demands a responder proportion the reviewer has to justify: the demand is not silent. The proportion seeds to `RESPONDER_P0_DEFAULT` and the Configuration tab's Next stays shut until the reviewer confirms it or replaces it with a rationale, in a box that reads REQUIRED until it is ticked. The assumption is examined once per outcome instead of being defaulted past — which is what the old `"effect"` default achieved only by never raising it.
- If `sof_presentation` is `"responder"` **or** `"both"` — both run the conversion, so both need the same inputs, and a `conditionalPanel` testing only `'responder'` would leave a reviewer on `"both"` with no way to enter the proportion:
  - `numericInput("baseline_risk_chinn", "Proportion of control patients meeting the threshold of clinical interest", value = RESPONDER_P0_DEFAULT (0.20), min = 0.01, max = 0.99)`, gating Next until it is confirmed or replaced-with-a-rationale. The two `conditionalPanel`s compare against the **constant**, not the seed: what obliges a rationale is departing from the app convention.
  - `textAreaInput("responder_p0_rationale", ...)` when the default is replaced; `pma_confirm_checkbox("responder_p0_confirm", ...)` when it is not. It is built with the **shared confirmation box** (§3.4.13), not a bare `checkboxInput()`: this is the Configuration tab's second Next gate and has to look like the first one. It stays where it is — between `EDU_COPY$config_tab$responder_default` and `threshold_label` — because it and the rationale textarea are the two arms of one `conditionalPanel` pair, so the box reads as the alternative to justifying a change, which is what it is.
  - `textInput("threshold_label", "Definition of the threshold of clinical interest (free text)")`
  - `output$chinn_direction_echo` — `chinn_invert` is derived from the Step 2 direction answer, not asked again.
**The threshold-equivalence summary is not a question and must stop looking like one.** The block under the threshold input that reads `Increase: 156 per 1,000 -> 206 per 1,000, equivalent OR 1.404` (and its decrease mirror) is derived entirely from the number typed directly above it: there is nothing in it to answer. It used to carry a left accent on a filled ground — the wizard-question costume — which is what made a reviewer read it as one more question in the same column as Publication bias's. It is body copy under its input, per §3.4.13, and the accent it gave up went to `threshold_confirm`, which is the thing on that tab that genuinely has to be answered.

**And it is two lines, not four (0.5.1).** The block used to end with two
italic paragraphs deriving the conversion: *"What the algorithm uses: a
symmetric +/- log(1.396) band…"* and *"The … side is therefore the approximate
one: the band's mirror is OR 0.717…"*. Both are deleted, from
`output$threshold_equiv` **and** from `threshold_summary()`, and `.equiv_lines()`
no longer builds them. The two conversion lines above are what a reviewer reads
this block for; the derivation behind them is not a decision anyone makes here.

Nothing about the residual asymmetry is lost. `step3_threshold_note()` states
it, is written onto the rated object, and travels into the domain notes and the
Evidence Profile footnote — see the exception block below, whose status this
raises rather than lowers: it is now the only statement of the asymmetry.
`.equiv_lines()` still returns a `caveat`, surfaced under the two lines, but
only when the requested direction could **not** be honoured. That one is not
part of the derivation: an app that quietly converts on the opposite side to
the one the pooled effect lies on is the silent exit §2.3 forbids.


- `output$responder_p0_badge` renders `confirmed` / `unconfirmed assumption` beside the section heading, and **nothing at all** on the `"effect"` route, where there is no assumption to confirm.
- `input$sof_presentation` is registered in `PMA_OUTCOME_INPUT_IDS$configuration` (`R/ui_helpers.R`), so a change of outcome clears it. An id missing from that list is an id whose stale answer survives an outcome change.
- `responder_mode()` in `step3_server()` is the single definition of "the responder route was chosen" and is TRUE for `"responder"` and `"both"` alike; the Next gate, `sof_convert_args()` and the `state$display$convert` mirror all read it rather than the input. `keep_effect_scale_mode()` beside it decodes the one question that separates the two, and is read only by `sof_convert_args()`.

**The choice is banked with the outcome, and it reaches the ZIP.** All five values — `convert_smd_to_or`, `keep_effect_scale`, `baseline_risk`, `threshold_label`, `chinn_invert` — reach `state$display` and are stamped onto the rated object by `pma_bank_export_material()` in `.store_outcome()`, under the `"pmatools_display"` attribute pmatools already reads per outcome. Four of them are written by the `sof_convert_args()` observer in `step3_server()`; `threshold_label` is left to app.R's display observer, which already mirrors the raw input. **One key, one writer**: a second observer writing `state$display$threshold_label` with a different answer invalidates the first forever — the session never goes idle again and no output updates. `threshold_label` needs no guard of its own, because nothing reads it unless `state$display$convert` is `TRUE`, and that is the guarded value. `grade_table()` picks them up **per row**, so the Step 4 preview and the root `summary_of_findings.docx` of the bundle both show the presentation the reviewer chose, and two continuous outcomes in one review can be presented differently. Only the routes that convert stamp anything: an outcome shown as its effect carries no field at all, so nothing reads as a decision that was never made. `keep_effect_scale` is stamped alongside `convert_smd_to_or`, never instead of it, so a banked outcome cannot ask for both scales without asking for the conversion that supplies one of them. A row whose conversion cannot be applied falls back to the unconverted presentation with the reason footnoted rather than failing the export (`SPEC.md` §4.9).

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
that render as one and pin the cap; `test-edu-copy.R` asserts it. Step headers
and the intro modal are deliberately outside the registry, and the comment
there says why. The `multi_outcome` strings joined it in 0.5.1, when the
press-to-save model they described was deleted (§3.4.14) — they were exempted
as "a later phase owns them", and that phase has now happened. There were four;
`save_intro` went with the Step 3 section it introduced (§3.4.14).

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
no pages, **no DOI in the citation text**. `EDU_COPY$pmid_url()` is gone and so
are the per-domain `$ref_text` / `$doi` pairs: each rated domain carries its
reference as the single field `EDU_COPY$domains$*$ref`. The six BMJ 2025 Core
GRADE papers are all Guyatt, all BMJ, all 2025, so the bare form cannot tell
them apart; a specific paper carries its series number as a prefix — `Core
GRADE 4. Guyatt G, et al. BMJ. 2025`. Risk of Bias and Publication bias both
cite Core GRADE 4 and so render identically, which is correct.
`test-edu-copy.R` pins the shape with a regex over every `$ref`, so the format
cannot drift back. The rule reaches the whole app, not just Step 3: the Step 1
sample-dataset line, the Step 2 rare-events references and the RoB-ME notes on
Steps 1 and 3 all follow it. **Step 4's "How to cite" card is the one exception
— it is Vancouver; see §Step 4.**

**Linking a citation.** `pma_reference(..., url = NULL)` renders the citation
text, wrapped in `<a href target="_blank" rel="noopener">` when a `url` is
given and as plain text when it is not. The argument is back after having been
removed: what the removal was right about was the *inconsistency* — the same
paper rendered four different ways across the wizard — not the link itself, and
a reviewer checking a domain against its source wants the paper rather than the
ability to retype a citation into a search box. So the destination comes from
one map, `PMA_CORE_GRADE_DOIS` in the package's `R/house_style.R`, keyed on the Core
GRADE series number and read through `.core_grade_doi_url()`, and every Core
GRADE tab renders alike.

Each rated domain names its paper as a **number**, `EDU_COPY$domains$*$core_grade`,
and `pma_domain_reference(EDU_COPY$domains$<d>)` — the single call at all five
Step 3 tabs — reads the citation and the link from that one entry. The number is
a field rather than something parsed back out of the `"Core GRADE n."` prefix
because the prefix is display text: a regex over it would turn any rewording of
the citation into a silently dead link. `.core_grade_doi_url()` returns `NULL`
for a number the map does not carry (series papers 6 and 7 have no DOI recorded),
so an unmapped domain renders as plain text rather than losing its tab. Step 2's
rare-events references pass no `url` and take that same plain path.

**Judgment wording.** Badges, verdict lines and the four override
`selectInput`s read `.grade_level_wording()` from the package (SPEC.md §5.0),
so they say *Not serious* / *Serious* / *Very serious* / *Extremely serious*.
Since 0.5.1 the override **values** are those same Core GRADE words
(`not_serious` / `serious` / `very_serious` / `extremely_serious`), and their
labels still carry the downgrade — `"Serious (-1)"` — so a reviewer can see
what a level costs before picking it.

**The fourth level, and where it is not offered.** `pma_judgment_choices()`
offers `Extremely serious (-3)` on all five domain tabs. No assessor produces
it (SPEC.md §5.0), so this menu is the only route into the level, and it lands
in the same rationale gate as every other override: selecting it without a
written rationale leaves the automatic judgment standing and raises the
"a written rationale is required" notification. It is deliberately **absent**
from `radioButtons("other_downgrade")`, which stays `0 / -1 / -2`: "Other
considerations" is not a Core GRADE domain, and a −3 there would invent a
rating the source does not describe.

**Level → downgrade is the package's table, not the app's.**
`pma_downgrade_chip()`, `pma_judgment_badge()`, `pma_domain_verdict()` and
`pmatools_GRADE_DOWNGRADE()` call the vendored `.grade_level_downgrade()`.
They used to carry four private copies of `c(no = 0, some = -1, ...)`, each
needing a hand edit whenever the package gained a level; a level the app did
not know about scored 0 and painted green.

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

Two more `.config_note()` paragraphs are **deleted** (0.5.1), on the same
"delete first" test. The control-group risk box's *"Converts the absolute
threshold to the analysis scale, and seeds the Optimal Information Size…"*:
the input above it is already labelled *Control-group risk (events per N
patients)*, and the box it seeds is one tab away. The per-N radio's *"One
setting for the whole app — display only, never what is computed…"*: the radio
above it is labelled *Report event rates per* and the units are the answer. The
conditional rare-event note under the same radio **stays** — it reports
something that happened (a unit was seeded, and why) rather than restating a
control.

| input | where it lives | why |
|---|---|---|
| `per` | Configuration | it relabels the control-group risk, the absolute threshold and the OIS figures, none of which are on Final certainty (Final certainty keeps a read-only echo) |
| `rob_some_concerns` | **Risk of Bias**, under `Inputs for this domain` | it decides which side of the binary split each study falls on, and the stratified forest on that tab draws exactly that split. Its default is `"high"`, which the **package** default matched only from 0.5.1 (`SPEC.md` §5.1); before that the radio said `(default)` beside a value `grade_meta()` did not default to, and a rating reproduced by calling the package directly could differ from the one on screen. Its **scope is unchanged** — still one review-wide setting that persists across outcomes, still absent from `PMA_OUTCOME_INPUT_IDS$rob`. Only the point of edit moved (0.5.1; it was on Configuration for one release, and on a closed `<details>` on Risk of Bias before that). Seeded from `state$rob_some_concerns` — see below |
| `rob_inf_threshold` | **deleted** (0.5.1) | a pmatools convention rather than a Core GRADE 4 rule, and a reviewer had no basis on which to move it. The package default `rob_inflation_threshold = PMA_ROB_INFLATION_THRESHOLD` (`R/domain_rob.R`, `0.20` since 0.5.1) now applies unconditionally; the app no longer passes the argument at all, and `export_bundle()` writes that same value into the bundled `analysis.R`. Deleting the slider also removed the only consumer of the RoB `how` closure's `inflation_threshold` argument — producer and consumer died together |

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

**The per-N display unit.** `radioButtons("per", …)` offers
`step3_per_units_offered(rare, selected)` — **100 and 1,000 for an ordinary
analysis, and 10,000 and 100,000 as well when the analysis on screen is flagged
rare** — with the labels built by `step3_per_choices()`, and is backed by the
`display_per_state()` reactiveVal,
seeded under `isolate()` and synced back with `.sync_widget()` — the same
machinery the threshold values use, because a statically declared radio would
push its default back on every 3 → 2 → 3 round trip. The two large units exist
for rare events (§3.4.14) and are seeded there; the default is 1,000.

> **Two lists, one rule: narrow the offer, never the acceptance.**
> `STEP3_PER_UNITS` keeps all four and remains the set `step3_per_unit()`
> validates against, because a rare analysis seeded to 10,000 (§3.4.14) must
> keep validating — `app.R` routes `state$display$per` through the same
> function, so a validator that had learned about the offered set would print
> per 10,000 on screen and export per 1,000, with nothing on screen to say so.
> `STEP3_PER_UNITS_COMMON` is what the radio shows. `step3_rare_per_seed()`
> still reads all four in ascending order and is unchanged.
>
> **A unit the reviewer is standing on is never taken away.**
> `step3_per_units_offered()` unions the current selection into the offer, so
> an analysis that stops being rare at per 100,000 keeps that choice on the
> radio. Without it `radioButtons()` renders with no selection and pushes its
> first choice — per 100, coarser than the default — back on the next rebuild.
> Same principle as the seeding observer's "only while the unit is still the
> default": the display unit is a property of the review, not of the outcome.

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
pubias_registry_complete == "yes"                   -> "result"   (terminal)
# "no" falls through to the Figure 5 nodes
k >= 10 : !answered(pubias_funnel_asymmetry) ? "q3" : "result"
k <  10 : !answered(pubias_unpublished)      ? "q4" : "result"
```

- **Q2 is not a question.** k decides it (`.pubias_effective_k()`), so it is
  reported as a one-line automatic step under the chart, never a screen.
- **Every node is one card, and the four nodes agree with each other.** Each
  node's body is wrapped in a single `div(class = "pma-wizard-question")` built
  by one local helper inside `output$pubias_wizard`. Before that each returned
  a bare `tagList()`, so the live question — the only thing on the tab that can
  be answered — looked exactly like the reference plots below it and the
  override `<details>` below those. The CSS (`www/shadcn.css`) is a
  `hsl(var(--primary))` left accent on a `hsl(var(--muted))` ground, an accent
  no other block on the tab uses.
  - **The question is the heading; the widget's label is `NULL`.** Two nodes
    used to carry a second, differently worded question string in the widget
    label, and one labelled its select `"Your answer"`. Where there were two
    wordings the better one survives as the heading and the other is deleted,
    not merged.
  - **The three radio groups are `inline = FALSE`.** Two of them have option
    labels that are whole sentences. Q3 stays a `selectInput` — four options,
    one of which is the `"egger"` sentinel.
  - **A progress line sits above the heading**, from
    `step3_pubias_question_line(node, path)` (`R/step3_threshold.R`, pure and
    unit-tested), where `path` is `step3_pubias_reachable()`. `"result"` is not
    counted: it is the verdict, not a question. The **total is printed only
    once `"result"` has joined the reachable path**, i.e. once the answers
    settle the route — before that the reviewer's own next answer decides
    whether the wizard ends here or runs to three questions, so a total taken
    from the path so far would always equal the current index and would tell
    every reviewer they were on the last question. Until then the line reads
    `"Question 2"` with no total. This does not reinstate Fig 5's `Q1`–`Q4`
    numbering (below): it counts the questions on the route the reviewer is
    actually walking, which is the thing the figure's numbers never named.
- **The reviewer sees no *Figure 5* question numbers (0.5.1).**
  `PUBIAS_NODE_TITLES`, the four wizard `h5()` headings and
  `step3_pubias_k_line()` state the question
  and drop the `Q1` / `Q2` / `Q3` / `Q4` prefix, and `inst/figures/pubias.svg`
  drops it too. The numbering is Core GRADE 4 Fig 5's, but the chart puts a
  pmatools node between Q1 and Q2, so on screen it numbered neither the source
  nor the route the reviewer was walking. The **node keys** (`"q1"`, `"q3"`,
  `"q4"`) and the `"Q1:"`–`"Q4:"` prefixes inside the package's domain notes
  are unchanged: the first are internal, the second are the exported record.
- **The overall reporting-bias question has two answers, and only one of them
  decides anything (0.5.1, breaking).** `pubias_registry_complete = "yes"`
  ("reporting bias is unlikely; do not rate down") is the pmatools
  short-circuit and is forwarded to `grade_meta()`. `"no"` ("reporting bias is
  possible; go on to the Figure 5 nodes") is sent as `NULL` and decides
  nothing on its own. Two things went with that:
  - the app-level post-override that rewrote a `"no"` into a forced rate-down 1
    **regardless of the remaining nodes** is deleted. Core GRADE 4 Fig 5 has no
    such rule and the app was the only thing that had one: a reviewer who
    thought reporting bias plausible and then answered the funnel question
    found the funnel answer had counted for nothing. A reviewer who wants the
    rating regardless still has `pubias_override`, which demands a written
    rationale;
  - the third `"defer"` option ("leave it to the Figure 5 nodes") is deleted
    with it, because `"no"` now means exactly that. `STEP3_PUBIAS_DEFER` is
    gone from `R/step3_threshold.R` rather than left unused.
- **The Q3 select still carries an explicit VALUE for "no opinion":**
  `pubias_funnel_asymmetry = "egger"` ("accept the automated Egger test").
  Without it, "the reviewer looked and accepts the test" is indistinguishable
  from "the reviewer has not reached this yet" and the wizard can never advance
  past an optional node. It does not reach `grade_meta()`: it is mapped to
  `NULL`, which is what "let the algorithm decide" means to `assess_pubias()`.
  In particular it must not be routed through `.override_or_ignore()`, which
  would demand a rationale for declining to override.
- **Egger's test is computed once, by `pubias_egger()`, and read three times.**
  The reactive returns `list(feasible =, p =, asymmetric =)`; `feasible` is
  `FALSE` below `k = 10`, where the test is not run at all, which is a
  different state from a test that ran and produced no p value.
  `.pubias_egger_callout()` renders it as the colour-coded callout — a plain
  function, not a `renderUI`, because Shiny binds one output to one place in
  the DOM and the callout appears **twice**: under the Funnel sub-tab it is
  computed from, and inside the **q3 wizard node**, beside the question that
  asks the reviewer to accept or reject that very number. It used to be
  computed inline inside `output$pubias_egger_result`, which is why the
  flowchart could not read it. The single tier is `p < STEP3_EGGER_ALPHA`
  (0.05); the `p < 0.01` → `"very_serious"` tier pmatools 0.5 removed is not
  coming back.
- **Advancing happens on answer.** One `observeEvent` per input clears
  `pubias_reopen`; the derivation moves on by itself. No `updateTabsetPanel`,
  no manual Next.
- **A breadcrumb re-opens any answered node, and does nothing else (0.5.1).**
  `pubias_reopen` is honoured ahead of the derivation, but only for a node the
  current answers put on the path — so re-opening Q1 and answering "yes" cannot
  strand the reviewer on a Q3 that no longer exists. Reset by
  `state$step3_reset()`. `output$pubias_breadcrumb` is now **links only**: it
  used to restate every answer in prose beside its `PUBIAS_NODE_TITLES` heading,
  which the lit chart above says better and in the algorithm's own shape. The
  titles survive as the link text, because two undifferentiated "change" links
  would be a trap of their own.
- **The chart is a progress indicator, drawn before the first answer (0.5.1).**
  `output$pubias_flowchart` renders Figure 5 **above** the wizard, from the
  first node onwards, unlit, and lights up node by node as answers arrive. One
  question at a time answers "what am I being asked" but never answered "how
  much is left", and a reviewer three questions in could not tell whether two
  more were coming. The two surfaces therefore have different jobs: the wizard
  is the only place anything is answered; the chart says where the reviewer is.
  - `on_ids` comes from `step3_pubias_flow_ids()` (`R/step3_threshold.R`, pure
    and unit-tested), **not** from `pma_flow_path_ids()`: the `flow_path` fact
    exists only once `grade_meta()` has rated the domain, which is exactly when
    a progress indicator has stopped being useful. It translates the wizard's
    node keys (`q1` / `extra` / `q3` / `q4`) into the figure's ids, which are
    a different vocabulary — `extra` is the pmatools registry node, and the k
    gate is the figure's `q2`, which the wizard never asks.
  - **Accepting the automated test lights the chart.** `"egger"` is an answer
    ("I looked, and I accept the test"), so the leaf it reaches is decided —
    by a p value the *caller* holds. `step3_pubias_flow_ids()` takes it as
    `egger_asymmetric` (logical or `NULL`), supplied at the call site from
    `pubias_egger()$asymmetric`, and resolves the sentinel to `"yes"` / `"no"`
    before matching. The function stays pure and side-effect free, which is
    what keeps it unit-testable. `NULL` or `NA` — the test was infeasible or
    failed — still stops the trail at `pma-pubias-node-q3`, because then no
    leaf genuinely is decided; a test that could not run is not a symmetric
    funnel. Until 0.5.1 the sentinel always stopped there, so a reviewer who
    accepted Egger saw a chart that looked unfinished for the rest of the
    assessment.
  - **This changes nothing that reaches `grade_meta()`.** The sentinel is still
    mapped to `NULL` (above): `"egger"` means "let `assess_pubias()` decide",
    and lighting the chart is a display concern.
  - **`"no"` on the registry node lights the k gate and the edge out of it.**
    That node is the one the reviewer is never asked about, so lighting the
    node alone would show the chart stopping at an unanswered question; the
    edge is what says which branch the study count chose for them. Up to 0.5.0
    a `"no"` stopped the trail at `pma-pubias-edge-registry-no`, because it
    ended the wizard.
  - `.domain_evaluation("Publication bias", flowchart = FALSE)` suppresses the
    usual under-the-verdict copy, so the tab draws the figure once.
  - The k gate is printed under the chart by `step3_pubias_k_line()`: the chart
    can light that branch but cannot print the number it turned on.
- **Structural constraints.** The three reference plots — funnel,
  trim-and-fill, missing results (RoB-ME) — are a **statically declared
  `tabsetPanel`** below the wizard, one panel at a time at full width, Funnel
  first. Static is load-bearing: `imageOutput` and `DT::DTOutput` do not
  re-bind cleanly inside a container a `renderUI` replaces, and a `tabsetPanel`
  only toggles `display`, so the outputs are built once and survive every
  switch. `pubias_missing_editor` keeps
  `outputOptions(suspendWhenHidden = FALSE)`.
  - **Each tab title carries a status dot, and the dot rates nothing.** A
    reviewer who never opens a tab never learns that its diagnostic disagreed
    with the answer they gave the wizard, and these three are reference material
    precisely because Core GRADE 4 Fig 5 has no node for any of them. The dot is
    a nudge toward looking, not an input: no dot reaches `assess_pubias()` or
    `grade_meta()`, and the wizard's answers stay the only thing that rates the
    domain.
    - **It must not be the domain tabs' mark.** The Step 3 domain tabs already
      carry `pma-tab-mark` — a `●` meaning "opened, not yet confirmed", i.e.
      *the reviewer's progress*. These dots mean *what a diagnostic found*, on a
      tabset nested inside one of those tabs, so one glyph would carry two
      unrelated meanings a few pixels apart. The status dot takes its own class
      and its own shape; `pma-tab-mark` is untouched. Rendered by
      `pma_tab_status_dot()` (`R/ui_helpers.R`, class `pma-tab-status`) into a
      `uiOutput` slot on each tab title, and drawn by `www/shadcn.css` as a
      **rounded square** — a shape, not only a colour, so the two markers never
      read as the same thing whatever the font does. Drawn in CSS rather than
      written as a glyph for the reason `pma_wizard_nav()` gives about HTML
      entities: an empty element with a class cannot arrive mojibaked.
    - **The three algorithms live in the package** (`R/pubias_status.R`,
      `R/pubias_missing.R`), not in the app: they are arithmetic a test should
      hold to a contract, and the app is the wiring. A tag list title leaves
      `tabPanel` with no string to derive a `value` from, so all three reference
      tabs state their own — the same consequence `.tab_title()` met on the
      domain tabs.
    - **Four states, and the fourth is not a colour.** 🟢 / 🟡 / 🔴 say what the
      diagnostic found; ⚪ says it was never computed, with the reason as its
      tooltip. Three colours alone make "not computed" read as "nothing wrong",
      which is backwards for every tab here: each one declines to compute on
      exactly the sparse data where reporting bias is most likely.
    - **Funnel** is Egger's p — 🟢 at `p >= 0.05`, 🟡 at `0.01 <= p < 0.05`, 🔴 at
      `p < 0.01`, ⚪ when Egger did not run. It does not run below the existing
      `k >= 10` gate, **and it does not run on rare-event data**: Egger loses
      validity on sparse binary data, so a `rare_flow` from
      `rare_event_diagnostics()` forces ⚪ rather than letting an invalid
      p-value paint a 🔴.
    - **Missing results (RoB-ME)** is the tipping-point algorithm in §3.4.8a.
    - **Trim-and-fill** is the direction-of-bias verdict read off the pair
      (original pooled effect, trim-and-fill adjusted effect): `not_serious` is
      🟢, one level is 🟡, two levels is 🔴, and the panel's existing `k >= 10`
      gate is ⚪. It is the same check the Risk of bias tab runs on the low-RoB
      subset, so the 20% figure stays `PMA_ROB_INFLATION_THRESHOLD` and is not
      duplicated here. It runs on the absolute risk scale for a binary outcome —
      see §3.4.8b, which is also what makes ⚪ reachable without a baseline risk.
  - The tabset is **not gated on a wizard node**. All three are computable the
    moment `state$ma` exists and all three are reference material rather than
    answers; gating them meant the funnel appeared only at `q3` and the RoB-ME
    editor only at the result, so each was absent exactly when a reviewer might
    want to check it against a different question. `output$pubias_show_funnel`
    is deleted with the gate.
  - `output$pubias_show_result` remains, gating `output$pubias_evaluation`
    alone: the verdict is the wizard's conclusion, and printing it before the
    wizard has run reports a rating of nothing.
  - **`output$pubias_trimfill_summary` states the 20% exaggeration check
    (0.5.1).** The panel used to print the original and adjusted pooled effects
    and leave the reviewer to compare them by eye. It now also prints the
    sentence `.pubias_trimfill_line()` builds from
    `.pubias_trimfill_inflation()` (`R/pubias_trimfill.R`, SPEC.md §5.5a): the
    same "is the favourable direction exaggerated by more than a fifth?"
    question the Risk of bias tab asks of the low-RoB subset, asked here of the
    trim-and-fill adjustment, sharing `PMA_ROB_INFLATION_THRESHOLD`. **It rates
    nothing** — Core GRADE 4 Fig 5 has no trim-and-fill node — and is material
    for the funnel-asymmetry question above it; the sentence says so, and the
    left border is amber only when the check fires. The arithmetic and the
    wording are the package's so a test can hold them to that; the app supplies
    `state$small_values` (normalised to `NULL` when it is not one of the two
    known values, because an aborting `renderUI` would replace the panel with a
    stack trace) and its own `fmt()` as `format_te`. The `k >= 10` gate on the
    whole panel is unchanged.

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
> Inconsistency `very_serious` (−2), not `serious` (−1): with a substantial
> share of estimates on each side of the threshold and no credible subgroup,
> the direction of effect is unresolved. The app renders whatever the package
> returns — no app-side arithmetic changes — but the domain badge, the
> certainty verdict and the flowchart leaf all move with it, and the leaf now
> reads "Rate down 2 levels". See SPEC.md §5.2 for why this departs from Core
> GRADE 3.

**Risk of Bias.** `output$rob_rule_note` (a ~180-word standing statement of the
binary rule) and the "See also RoB 2" paragraph are deleted, not collapsed: the
`rob_some_concerns` radio states the rule beside itself — that Core GRADE 4
rates risk of bias from a **two-way split**, that Fig 2 asks whether the high
risk of bias group dominates, and that RoB 2's three judgments therefore have
to fold into two before the question can be asked at all — the two-group forest
*shows* it, and `pma_reference()` already carries the source. That copy says
why the control exists; up to 0.5.1 it said only that Core GRADE 4 left the
boundary open, which describes the choice without naming the mechanism that
forces it.
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
| Imprecision | `FALSE` | `output$impre_evaluation`, directly below, renders the interval against both thresholds and states the verdict; the box was showing the arithmetic behind an answer the tab was about to give |

Imprecision passed `TRUE` until 0.5.1, on the argument that Core GRADE 2's
two-level rule tests the confidence interval against the important-benefit
**and** important-harm thresholds by eye, so both bounds — and the
residual-asymmetry sentence, since only one conversion is exact on the absolute
scale — were operative. That is true of the rule and was still wrong for this
box, which is not where the reviewer reads the interval. All three tabs now
render the same one-line box. `detail` survives as a parameter because the
argument for the long form is tab-specific and could win again.

The trailing *"This decision threshold is shared by … Change it in the
Configuration tab"* sentence is now the tab's own name as a link, built by
`pma_domain_jump_links()` (§3.4.13). One id prefix per domain
(`threshold_block_jump_<domain>_`): all seven tab panels are in the DOM at once,
so three copies of one `actionLink` id would collide.

**Indirectness: the default is on screen.** All four PICO radios are
**preselected to `"yes"`** (0.5.1), and so is the **overall rating**, to
`STEP3_INDIR_DEFAULT_LEVEL` (`"not_serious"`, `R/step3_threshold.R`). Leaving
them blank used to send `indirectness = "no"` to `grade_meta()` while the
screen showed four unanswered questions — the domain scored no downgrade
silently. Preselection makes that default visible and leaves the reviewer to
downgrade the elements they have concerns about.

The **judgment is unchanged** and this is verified, not assumed:
`indir_subdomains()` now returns four rows instead of `NULL`, so `grade_obj()`
takes the *subdomain* path rather than the scalar one; `indir_worst_case()`
folds four `"yes"` answers to `"not_serious"`; and the override-rationale logic
compares `input$indirectness` against that same fold, which is the same value
it compared against before. Rated on the bundled CBT-I sample, certainty, all
five domain judgments and every downgrade are identical either way.

**The overall rating's blank carried two jobs, and only one of them was
visible.** It was how a reviewer accepted the fold, and — because "nothing
selected" was a reliable proxy for "no override intended" — it was also the
rationale gate (`conditionalPanel("(input.indirectness || '') != ''")`).
Preselecting removes the proxy, so the gate now compares the rating against
the fold itself:

- `step3_indir_worst_case(levels)` folds the answered subdomain levels by
  severity, and `step3_indir_rationale_required(overall, worst)` is TRUE only
  when the two differ (`R/step3_threshold.R`, both pure and unit-tested). An
  unanswered overall demands nothing; a fold of nothing reads as
  `STEP3_INDIR_DEFAULT_LEVEL`. `grade_obj()`, the rationale
  `conditionalPanel` and the note below the radio all read the same function,
  so the three cannot disagree.
- The `conditionalPanel` is gated on `output.indir_override_active`, not on a
  JavaScript expression: the fold is four radios mapped through
  `STEP3_INDIR_ANSWER_TO_LEVEL` and then reduced by severity, which the client
  cannot compute.
- `output$indir_override_note` states **which of the two is in force** — a
  restatement of the fold, an override that is rated because a reason was
  written, or an override that is *not yet* rated because none has been. That
  last case is the one preselection creates: a reviewer who downgrades a PICO
  element leaves the overall radio reading "Not serious" over a fold of
  "Serious", and `grade_obj()` drops a rationale-less override, so the fold is
  what rates the domain until they either move the radio or explain it.
- **`indir_worst_case()` used to fold against a dead vocabulary.** Its severity
  table spelled the levels `"no"` / `"some_concerns"` / `"serious"`, which
  0.5.1 replaced; every level the PICO answers produce except `"serious"`
  missed it, so four answers containing a `"No"` (very serious) folded to
  `NULL` and were reported as `"not_serious"`. It ranks the current levels now.

The **completeness gate is untouched by any of this.** A domain is confirmed by
its checkbox and nothing else (`pma_domain_confirmations()`, §3.4.13), which is
precisely why a preselected widget cannot open the export gate for an outcome
nobody has looked at.

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
shared `<details>` are **deleted**; what survives is three capped subtitles
beside the questions:

- `EDU_COPY$domains$indirectness$population`, under the Population radio — the
  test is whether the treatment effect would differ, not whether the trial
  population resembles the target one, and relative effects are rarely
  different across populations, which is why Core GRADE 5 Table 2 ranks
  Population least likely to warrant rating down. It sits under Population
  because the radio's own wording ("sufficiently similar") invites the
  demographic reading;
- `$surrogate`, under the Outcome radio — a surrogate outcome is grounds to
  consider rating down, and pooling it with the patient-important one is not
  recommended. That last clause read *"Never pool the two"* until 0.5.1; Core
  GRADE 5 states no such prohibition, so the imperative claimed more than the
  source does;
- `$gradient`, below the four questions — the fold is symmetric and ignores
  Table 2's ranking, which is why the override exists.

`$mapping` and `$banner` are gone, and with `$banner` went
`output$indirectness_banner` and `state$indir_reviewed`: the banner said "no
indirectness judgment recorded yet", and with the radios preselected there
always is one.

**Imprecision.** The tab is, in order: the domain header and reference, the
read-only threshold box, `output$impre_evaluation`, the inputs, the override.

**`output$impre_branch` and its "Core GRADE 2 Figure 4 branch taken" heading are
deleted [0.5.1].** They rendered the `fig4_path` / `ois_used` facts as a headline
plus two paragraphs of prose. The Fig 4 flowchart inside `impre_evaluation`
directly below draws the *same* `fig4_path` fact with the route lit up
(`pma_flowchart_details()`, §3.4), so the tab named its branch twice and argued
with itself about which was the answer. What the prose was for — making it
visible that sample size is not consulted unless the OIS branch is reached — is
what an unlit OIS node on the chart says; and the two-level condition the
algorithm cannot judge is stated at the override, where the reviewer can act on
it. Nothing else read `impre_branch`: its fact lookup was a closure inside the
`renderUI`, and it went with it.

The `.override_details` preamble is deleted (it restated the branch text), and
the nested `<details>` inside it is unwrapped: the one sentence a reviewer needs
at the override — *"Rate down two levels when the plain language summary
warrants 'may' rather than 'likely'"* — is now the only thing there, visible.
`.inputs_details(open = TRUE)` stays open.

**Final certainty.** `other_text` / `other_downgrade` are answers and stay
open; the rest of Display options collapses. The Heimke CER/EER recommendation
is now `pma_sof_cer_eer_note(arms)`, written into the SoF footer by
`pma_sof_add_notes()`, so it travels into the exported .docx — which it never
did as page text.

> **CER and EER keep their acronyms (0.5.1).** The arm labels are *not*
> substituted into "control event rate" / "intervention event rate": the two
> acronyms are the cited source's own and stop deriving from the words the
> moment the words change ("the placebo event rate (CER)"). What the reviewer
> needs is to find the columns, so the note names the columns instead — and
> those do follow the labels, because the headers do. Same reasoning inverted
> for `pma_sof_limitations_note(arms)`, which names the arm columns and
> therefore must follow them: it says "the value with &lt;control&gt;", mirroring
> the column head "With &lt;control&gt;", because the older
> "&lt;label&gt;-group value" shape does not survive free text ("CBT-I-group
> value").

#### 3.4.14 Rare events in Step 3

**The gap is not that Step 3 rates the wrong number.** When the reviewer accepts
the rare-event workflow, `state$ma` already *is* `state$rare$primary` — the
sparse-data fit, `BB_CR` by default — so every domain is rated on it. The gap is
that Step 3 does not know it: it never says which method produced the estimate,
never says the rating would survive a different one, and runs three domain
computations whose assumptions the data has already broken.

**The governing rule: rare mode changes what is computed and what is said, and
never changes a rating by itself.** Core GRADE has five domains and sparse data
does not earn a sixth; what it earns is arithmetic that is valid on the data at
hand and a record of how much the answer depended on a method choice. Every
change below is either a correction to a computation or a fact added to the
record — none of them moves a judgment on their own.

**What Step 3 reads.** `state$rare_diagnostics` (the `pma_rare_diagnostics`
object), `state$rare_mode_active`, `state$rare_primary_method`, and
`state$rare` — the whole fitted suite, not just the primary. The suite is the
part that matters most and was used for nothing after Step 2.

Two reactives in `step3_server()` gate everything below: `.rare_active()` is
`state$rare_mode_active && state$rare_diagnostics$rare_flow`, and
`.rare_one_arm_zero()` adds `one_arm_total_zero`. **Both conditions are
required.** A dataset can trip `rare_flow` and still be rated on the regular
analysis, because the Step 2 checkbox lets the reviewer decline the workflow;
nothing here applies to a rating made on the regular fit.

Three arguments carry the facts into the package —
`grade_meta(rare_flow =, rare_one_arm_total_zero =, rare_method =)` — and they
are on `PMA_GRADE_ARGS_EXPORTED` and in `analysis_script.R.tpl`, because a
script that omitted them would re-run the same data and report a different
rating. `grade_meta()` records them back on the object as `$rare`, which is
what `pma_outcome_grade_args()` recovers them from for a set assembled out of
banked outcomes.

**1. The method is named, once, where the rating is set up.**
`.rare_method_block()` (`R/step3_threshold.R`) opens the Configuration tab —
above the control-group risk, because it qualifies every number under it — and
states that the pooled estimate comes from the rare-event workflow, which
method produced it, and the study/event counts behind it. The method id reaches
`grade_meta(rare_method =)`, which stamps `$rare$method` /
`$rare$method_statement` onto the rated object, so it is banked with the
outcome and `.write_results_txt()` prints it in the bundle as
`[ Analysis method - rare events ]`. A reader of the Summary of Findings cannot
otherwise tell a beta-binomial estimate from an inverse-variance one.

**2. Imprecision: the rule is unchanged, the information size and the
sensitivity are not.** Whether the confidence interval crosses the threshold
stays exactly Core GRADE's question.

- **The optimal information size switches to an event basis.** An OIS in
  participants is the wrong denominator when the events are what is scarce; with
  a 0.5% event rate a "sufficiently large" participant count can carry a dozen
  events. Under `rare_flow` `.calc_ois(event_basis = TRUE)` returns the event
  count the same power calculation implies, `.compute_ois_pct()` compares total
  events with it, and the **`ois_basis` fact names the basis on every path** —
  "83% of the OIS" is two different claims on the two bases. An explicit
  `ois_n` override puts the comparison back on participants and the fact says
  so. Rating against a participant-based OIS on sparse data is not a stricter
  reading of Core GRADE, it is a wrong one.
- **The suite becomes a sensitivity analysis for the rating, not just for the
  estimate.** `rare_suite_crossing()` (`R/rare_step3.R`) asks every method in
  `state$rare` the same crossing question the primary was asked — against
  `.rated_threshold_for_imprecision()`, which is the threshold the rating
  actually used and not `threshold_internal`, or the sensitivity would answer a
  different question. `rare_suite_crossing_note()` reports unanimity, or names
  the disagreeing methods with their intervals; `output$impre_rare_sensitivity`
  renders it on the tab and `grade_obj()` appends the same sentence to the
  Imprecision domain note, so the export carries it. Computed app-side, like the
  threshold note, because a fitted suite cannot travel through `analysis.R`.
  This costs no new statistics — every fit already exists.
- **One arm with no events at all** (`one_arm_total_zero`) has no finite odds
  ratio and no interval to compare with a threshold. `assess_imprecision()`
  returns before it reads the CI, with `IMPRECISION NOT ASSESSABLE` in those
  words and no downgrade, and the domain's confirmation is what carries the
  reviewer's decision.

**3. Inconsistency: the I² proxy is withdrawn, not reinterpreted.** The
automated path uses I² as a statistical proxy for Core GRADE 3's first question.
On sparse data τ² is badly estimated and I² inherits that, so under `rare_flow`
`assess_inconsistency()` takes a path before the automated one and reports
`NOT ASSESSABLE BY THE AUTOMATED PATH` — **recording no I², τ² or Q at all**,
because a caveat beside a number is read past and the number is not. No
downgrade follows: an unusable statistic is not grounds for one. The scalar
override and the manual flowchart run first and are unchanged, and they are the
routes that still work; `incon_confirm_na` is the existing gate and needs no new
mechanism.

**4. Publication bias: rare data takes the k < 10 route, whatever k is.** Egger's
test loses validity on sparse binary data, and Core GRADE 4 Fig 5 already has a
branch for "Egger is not available to you" — the one k < 10 takes, which asks
about registries and unpublished studies instead. `step3_pubias_statistical(k,
rare_flow)` returns `FALSE` under `rare_flow` at any k, which routes the wizard,
the breadcrumb, the lit chart and `step3_pubias_k_line()` together;
`assess_pubias(rare_flow =)` gates its own Q2 the same way, so the answer the
reviewer gives at Q4 is the answer that rates the domain. The funnel status dot
is ⚪ (§3.4.12), unchanged. This adds no node to Fig 5; it routes to one the
figure already has, for the reason the figure already has it.

**5. Absolute effects: one denominator per outcome, chosen from the data.** The
per-N unit is already reviewer-selectable and already flows through one
formatter to every string in Step 3 and into `sof_table()`
(`step3_per_label()` / `display_per_state()`), so what was needed is a better
default and two more units. `STEP3_PER_UNITS` gains 10,000 and 100,000 as
*accepted* units — they are offered on the radio only while the analysis is
flagged rare (§3.4.10a), since four choices made the commonest control on the
tab twice as long for a case almost no analysis is in — and
`step3_rare_per_seed(event_rate_c)` picks the **smallest** unit at which the
control-arm event rate still rounds to a whole event — never below the 1,000
default, since 100 is the coarser unit and would make the problem worse.

The seeding observer applies it **once per detection episode, and only while
the unit is still the default**: the display unit is a property of the review
rather than of the outcome (see `display_per_state()`), so a reviewer who has
chosen a unit does not have it taken back on the next recompute. A one-off
notification says what changed and where to change it.

> **Assumption, stated.** The seed reads `event_rate_c`, the **control-arm**
> rate, as this section names — not `min(event_rate_c, event_rate_e)`. It is
> the rate every absolute number on Step 3 is already built from (the baseline
> risk, the threshold's conversion, the "with intervention" row), so seeding
> from anything else would put the unit and the arithmetic on different
> footings. When the intervention arm is the rarer one its risk can still round
> to zero at the seeded unit; the reviewer can raise the unit by hand.

**The threshold moves with it.** The decision threshold is stated in the same
per-N unit, and it must be the same one, for the reason §3.4.8b gives about
trim-and-fill: an effect and the threshold it is judged against on two different
scales is the failure mode this app has already had once. This needs no code of
its own — the threshold is stored per-1,000 and displayed through
`step3_to_per()`, so it follows whatever unit the seed picks. That is what "one
denominator per outcome" buys.

**6. The continuity correction needs no Step 3 change, only a record.** The
`incr` input is Step 2's, and the suite's methods are correction-free by
construction. What Step 3 owes the reader is the statement that no 0.5 was added
— a 0.5 correction biases toward the null and would otherwise be an invisible
assumption behind every downstream number. It is `PMA_RARE_NO_CC_NOTE`
(`R/rare_step3.R`), printed in the Configuration block and on the object as
`$rare$no_cc_note`, from where the bundle's `results.txt` prints it too.

**Not done, deliberately:** no rare-event domain, no automatic downgrade for
sparse data, and no change to any domain's decision rule. Sparse data makes an
estimate harder to trust, and Core GRADE already has the domain for that.
`tests/testthat/test-rare_step3.R` closes with the assertion that says so:
turning `rare_flow` on over ordinary data moves no domain judgment, no
downgrade and no final certainty.

### 3.5 Step 4 — Export

#### 3.5.1 Step header copy

> **Step 4: Export.** This step bundles every artifact you have generated into a single ZIP — including a fully reproducible `analysis.R` script. Anyone (including future-you) can re-run the analysis from the CSV and the script alone, with `library(pmatools)`. This is what makes the work reproducible and citable.

#### 3.5.2 Inputs

- `textInput("bundle_name", "Bundle name (no extension)", value = "pmatools_results")`
- `checkboxGroupInput("include", "Artifacts to include", ...)`, all selected by default
- `downloadButton("download_zip", "Download ZIP", class = "btn-primary")`

The checkbox **values are `export_bundle.pmatools_set()`'s `include` vocabulary,
verbatim** — `data`, `script`, `results`, `forest`, `forest_full`, `forest_rob`,
`funnel`, `funnel_trimfill`, `pubias_missing_forest`, `sof`,
`evidence_profile`, `indirectness`, `readme` (`PMA_EXPORT_INCLUDE_DEFAULT`,
`R/step4_export.R`). Only the labels are the app's. Translating the values in
the download handler would hide from the next reader of either side which
artifact each box controls, and that is how the app's old `grade_table` /
`sof_combined` values drifted away from what the bundler accepted.

#### 3.5.3 Server logic

The bundle is **always** built from a `pmatools_set`, even when there is one
outcome, so the ZIP has one layout: the combined Summary of Findings at the
root and one `outcomes/NN_name/` directory per outcome (SPEC.md §4.8.3).

```r
output$download_zip <- downloadHandler(
  filename = function() paste0(input$bundle_name, ".zip"),
  content  = function(file) {
    out <- export_bundle(
      pma_export_set(.export_outcomes(), primary = state$sof_primary),
      output_dir   = tmp_dir,
      bundle_name  = input$bundle_name,
      include      = input$include,
      style        = PMA_SOF_STYLE,
      sof_notes    = .export_sof_notes(outs),
      per          = state$display$per,
      prediction   = state$display$prediction,
      rob          = .export_rob(outs),
      label_intervention = arms$intervention,
      label_control      = arms$control
    )
    file.copy(out, file)
  }
)
```

`arms` is `pma_arm_labels(state)` (`R/ui_helpers.R`), shared with the Step 3
preview (§3.4.9). It was a closure inside `step4_server()` until 0.5.1, which
is exactly why the two disagreed: Step 3's preview could not call it.

Everything else the bundler needs is **per outcome**, and travels on the rated
object rather than being read from the live state at download time — which
describes whichever outcome is on screen, not the ones banked before it. Two
attributes carry it, stamped together by `pma_bank_export_material()` when
Step 3 banks an outcome:

| attribute | written by | read by |
|---|---|---|
| `pmatools_display` | `pma_outcome_display()` | `export_bundle.pmatools_set()`: `forest_display`, `forest_display_rob`, `rare`, `rare_forest_display`, `pubias_missing_df` (SPEC.md §4.8.3) |
| `pma_outcome_source` | `pma_outcome_source()` | the app itself: the data the outcome was rated on and the arm values it was pooled with |

`pma_export_set()` (`R/ui_helpers.R`) assembles the set from the banked
outcomes:

- **`data`** is every outcome's own data, bound row-wise with an `outcome`
  column naming the outcome (`pma_export_data()`). A review whose outcomes came
  from separate files therefore exports one `data_long.csv` that
  `run_ma_multi()` can split back apart. An `outcome` column already in the
  data is overwritten: there it names the measurement scale within one
  analysis, here it has to name the analysis. The `treat` column is rewritten
  to `experimental` / `control` using the arm values that outcome was pooled
  with, keeping the reviewer's own words in `treat_label`
  (`pma_name_arms()`). Which arm is which is a per-outcome answer and
  `run_ma_multi()` takes one `experimental_label` for the whole set, so two
  outcomes loaded from different files would otherwise fight over it — and
  losing that argument does not merely relabel a column, it inverts the pooled
  effect and every judgment that reads its direction.
- **`ma_args`** carries `sm` and `outcome_type` per outcome and the `run_ma()`
  settings the outcomes agree on (`method.tau`, `random`, `common`, `incr`,
  `hakn`, the arm labels). A setting they disagree about is omitted rather
  than claimed for all of them, because `run_ma_multi()` applies its `...` to
  every outcome.
- **`per_outcome`** is each outcome's `grade_meta()` argument specs
  (`PMA_GRADE_ARGS_ATTR`), plus what the multi-outcome `analysis.R` template
  cannot recover from the rated object the way the single-outcome one does:
  `study_design`, `outcome_type`, `threshold_type`, `follow_up`, `unit`, and —
  where they apply — `require_threshold`, `rob_refit`, `baseline_risk`. Without
  `threshold_type` the regenerated call does not merely reproduce a different
  rating, it aborts on the Core GRADE 2 entry gate.
- **`common`** is empty: two outcomes rated in separate passes share no
  argument by construction.

With no banked **rated** outcome the set holds the rating currently on screen,
with any not-reported rows kept alongside it. Step 3 banks an outcome once
every domain is confirmed *and* it has a name, while the download unlocks on
the domains alone, so an unnamed outcome can reach the button with nothing
banked.

**The click is acknowledged before the bundle is built.** Building it renders
every plot and writes a docx report, and nothing on screen changed between the
press and the first `incProgress()` — which is what "the download takes ages to
start" looks like. Two signals, because neither covers the other's window:

- **client-side, instantly**: a delegated `click` listener on `#download_zip`
  adds `.pma-download-busy`, a CSS ring that spins in the button
  (`www/shadcn.css`; `prefers-reduced-motion` stops the spin, not the ring).
  It is removed by the `download_done` custom message the handler sends on its
  way out. `shinycssloaders` cannot do this — it wraps a Shiny *output*, and a
  `downloadButton` is a link;
- **server-side**: `showNotification(id = PMA_DOWNLOAD_BUSY_ID, duration =
  NULL)` as the **first** statement of `content`, taken down by `on.exit()`.
  `on.exit()`, not a call at the end: three of the paths out of the handler are
  early returns from the Steps 2–3 guards and a fourth is an error inside the
  `tryCatch`, and a `duration = NULL` notification nothing removes stays on
  screen for the rest of the session.

#### 3.5.4 "How to cite" card

A `pma_card` holding one model sentence and an **ordered** reference list of
seven entries: Core GRADE 1–5, `{meta}`, pmatools.

**This card is Vancouver, and it is the app's one exception to the house
citation style [0.5.1].** Everywhere else a reference points a reviewer at a
paper *while they work*, and the short form (`.core_grade_ref()`) is the right
length for that. Here the reference **is** the deliverable — the reviewer copies
these lines into a manuscript — so each entry carries up to six authors then
`et al.`, the volume, the elocation id and the DOI. **Do not fold this list back
onto `.core_grade_ref()`.**

The prose cites **by bracketed number** into that list rather than repeating
short forms inline, so the card is one citation system and not two:

> Pairwise meta-analysis was performed using the {meta} R package [6]. Certainty
> of evidence was rated following the BMJ 2025 Core GRADE series [1-5],
> implemented in pmatools [7].

Numbering runs Core GRADE 1–5, `{meta}`, pmatools rather than by first
appearance: an author pasting this in renumbers against their own bibliography
regardless, and keeping the series contiguous is what makes it readable as a
block.

The pmatools entry is software, not an article, so it takes Vancouver's
`Available from:` form for the URL and carries a **version**, because an
analysis is only reproducible against one.

That version comes from **`pma_pmatools_version_number()`** — never
`utils::packageVersion()`, which errors under the vendored `source()` the
deployed app runs on (CLAUDE.md §1), and **never `pma_pmatools_version()`**,
which appends a ` (vendored)` provenance marker. The marker is right where it is
used — the Step 2 environment block (`step2_ma.R`) and the app footer (`app.R`)
are reporting *how the code was loaded* — and wrong here, because this line is
pasted into someone else's manuscript, where `Version 0.5.1 (vendored).` lands
in their reference list and reads as part of the version number. The two
helpers must not converge; `test-edu-copy.R` asserts both forms.

`pma_pmatools_version_number()` returns **`NULL`** when the version is genuinely
unknown (no installed package and no `pmatools.version_stamp` option), and the
card then **omits the whole `Version X.` clause**. An incomplete citation is
honest; `Version (vendored; version unknown).` in a bibliography is not.

Entries are ASCII apart from `Rücker`, written as the HTML entity `&uuml;` —
the shinyapps.io build has mangled Latin-1 in this app before.

#### 3.5.5 The saved-outcome list lives here (v0.5.1)

The "Summary of Findings (all saved outcomes)" card carries, in order:
`sof_intro_block` (`step4_intro` / `step4_empty`), `sof_stale_warning`,
`combined_sof_block`, and `pma_add_next_outcome_button()`.

`combined_sof_block` renders the combined table **and**
`pma_saved_outcomes_ui()` — the per-row Move up / Move down / Mark primary /
Remove controls. The list used to sit on the Step 3 "Final certainty" tab, one
step away from the table it feeds, and Step 4 rendered a second copy of it
writing to the same input ids. There is one copy now, on the step that owns it.

Two consequences for the block itself:

- **no early return when the list is empty.** It used to return `NULL` for
  `length(outs) == 0`, which took the list with it; the table half is skipped
  instead, and `pma_saved_outcomes_ui()` is passed
  `empty_text = EDU_COPY$multi_outcome$list_empty` so the empty state is a
  sentence rather than nothing;
- **a global signal above the list** once there is at least one row:
  *"N outcomes saved — add another, or download below."* The reviewer's
  question at this point is "did the thing I just confirmed land here?", and
  before this the only answer was to count rows.

#### 3.5.6 "+ Add next outcome" asks which kind (v0.5.1)

Core GRADE 6 asks the table to cover **every** patient-important outcome the
review prespecified, including the ones no included study reported. Those have
no data to map and no analysis to run, so the button no longer walks straight
back to Step 2: `input$add_next_outcome` opens
`pma_add_outcome_choice_modal()` with two routes.

| route | input id | what happens |
|---|---|---|
| *Analyse it from the data* | `add_outcome_analyse` | the previous behaviour: `begin_new_outcome(identity = TRUE)`, then `state$step <- 2L` |
| *Record it as not reported* | `add_outcome_not_reported` | opens `pma_not_reported_modal()` — outcome name, follow-up, reason |

Submitting the second modal (`not_reported_save`) runs
`pma_not_reported_entry()` (`R/ui_helpers.R`, pure and unit tested), which
trims the name, refuses a blank one and refuses one already in
`names(state$outcomes)` — *an outcome is either rated or not reported, not
both*, the same rule `add_not_reported()` enforces, reached before the reviewer
loses the form. On failure the modal **stays open** with what was typed in it
and the reason arrives as an error notification. On success the resulting
`pmatools_not_reported` goes into `state$outcomes` under its name.

`begin_new_outcome()` is deliberately **not** called on this route: a
not-reported row is a finished row, not an outcome the reviewer is about to
work on, so a rating half-answered in Step 3 survives adding one.

All three ids are written from hand-written JavaScript
(`Shiny.setInputValue(..., {priority: "event"})`) rather than being
`actionButton`s, because a modal is rebuilt on every showing and a rebuilt
`actionButton` reports 0 before it reports 1. They are named constants
(`PMA_ADD_OUTCOME_ANALYSE_ID`, `PMA_ADD_OUTCOME_NOT_REPORTED_ID`,
`PMA_NOT_REPORTED_SAVE_ID`) because a typo on either side is silent.

**`state$outcomes` now holds two classes**, and `pmatools_not_reported`
deliberately does not inherit `"pmatools"` (SPEC.md §4.14), so every filter it
meets has to name it:

| helper | behaviour |
|---|---|
| `pma_outcomes_list()` | keeps **both** classes. Before this a declared row vanished the next time anything read the list back |
| `pma_rated_outcomes()` | the rated subset — everything that can be pooled, plotted or re-run |
| `pma_outcome_summary_df()` | a `not_reported` column; `k = "0"`, effect `"Not reported"`, certainty `"Not rated"` in a grey `grade-unrated` badge, which is not a fifth rung under Very low |
| `pma_export_data()`, `pma_set_ma_args()`, `pma_export_set()`'s `grade_args` | **rated only**. These become `run_ma_multi(outcomes = )`, `data_long.csv`'s `outcome` column and `grade_meta_multi(per_outcome = )` in the generated `analysis.R`, and there is nothing there to run or rate |
| `pma_export_set()`'s `outcomes` / `order` | **both**. The row is a row of the table and gets its own numbered `outcomes/NN_name/` directory |
| `pma_export_set()` overall | **aborts** when no *rated* outcome is left: such a bundle has no analysis to build from and no effect measure to head the table's columns |
| `combined_rare_alerts()`, `.export_sof_notes()`, `.export_rob()` | rated only — each reads `g$meta` |

The bundled `analysis.R` needs no extra work: `export_bundle_multi()` already
emits one `add_not_reported()` call per not-reported outcome, after
`grade_meta_multi()` and before `reorder_outcomes()` (SPEC.md §4.14).

`pma_sof_limitations_note()` (a constant until 0.5.1, when it had to start
following the arm labels) lost its first sentence with this change. It said
*"'Not reported' rows: outcomes the evidence base did not measure are absent
from this table"*, which is no longer true.

### 3.4.12 Domain flowcharts (v0.5.1)

Each of the four flowcharted domains — Risk of Bias, Inconsistency, Imprecision,
Publication bias — shows the decision tree it was judged by, **with the path this
analysis actually took highlighted**. Indirectness has no flowchart; Core GRADE 5
Table 2 is a gradient, and its `indir_subdomain_table` stays the visual.

Three renderings of the same file, all through `pma_flowchart()` in
`R/ui_helpers.R`:

| where | `on_ids` | why |
|---|---|---|
| under the verdict, in `<details class="pma-flowchart-details" open>` | the path taken, from the `flow_path` fact | it answers "why this judgment", so it is open by default — but a reviewer who does not want it can shut it |
| inside the collapsed "How is this judged?" accordion | none | the plain diagram, as reference |
| **above the Publication bias wizard**, always visible | the answers so far, from `step3_pubias_flow_ids()` | that tab asks one question at a time, so the chart is what says how much is left; it has to light up before any rating exists. The under-the-verdict copy is suppressed there (`flowchart = FALSE`) so the figure is drawn once |

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
judged against the threshold — so a tick alone will not do. One member of
`config_blockers()` is itself a tick: `responder_p0_confirm`, on the responder
route (§3.4.10a). So the Configuration tab carries **two** confirmations that
gate its Next, and they are built the same way.

**Every confirmation is built by `pma_confirm_checkbox()` (`R/ui_helpers.R`),
and looks unfinished until it is ticked.** The helper is shared rather than
local because `responder_p0_confirm` is rendered from `R/step3_threshold.R`
and the other six from `step3_ui()` in `R/step3_grade.R`; a closure inside
`step3_ui()` — which is what this was until it moved — is unreachable from the
first, and the consequence shipped: one gate boxed, one gate rendered as a
bare `checkboxInput()` in a column of numeric inputs and notes, with nothing
saying a click was required. `PMA_OUTCOME_CONFIRM_IDS` is the canonical list of
all seven, and `test-confirm-checkbox.R` asserts against the **built UI** that
the boxed set and that list are the same set, so a confirmation added as a
bare checkbox fails rather than ships.

**A left accent on a filled ground means "answer this", and nothing else on a
tab may wear it.** The accent is the wizard question's (§3.4.12): a
`hsl(var(--primary))` left border on a `hsl(var(--muted))` ground. A read-only
block that borrows the shape is read as a question, and a required block that
is lighter than the read-only blocks around it is read as optional — the tab
then teaches the reviewer the opposite of what it means, and no amount of
wording inside either block undoes it.

**The rule is about the `--primary` accent, not about every left accent.** The
app has a second, older vocabulary — the alert and status palette, amber and
green and red on their own tinted grounds — worn by `output$config_status`, the
incomplete-certainty banner, the stale-analysis and rare-event banners, the
Egger callout and `.pma-analysis-set`. Those say "notice this state", they are
read-only by design, and they are none of this rule's business: a reviewer never
mistakes an amber notice for a question. Only `--primary` on `--muted` is
reserved, which is what the test enforces — it rejects the primary colour
specifically rather than any 4px border, so a status block cannot fail it and a
question-costumed read-only block cannot pass.

What the Configuration tab shipped before this rule, measured on the deployed
app: the read-only threshold-equivalence summary sat on a solid
`rgb(245,245,245)` ground behind a **4px** left accent, while `.pma-confirm`
around `threshold_confirm` — the gate that actually blocks Next — had a
near-transparent `rgba(15,23,41,0.05)` ground and a **1px** translucent border.
The heaviest block on the tab was the one with nothing to answer and the
lightest the one that had to be answered. That is why the REQUIRED pill added
with the shared helper did not settle the question the reviewer raised: the
pill was never the problem, the ranking was.

The rule is therefore two-directional, and both halves ship together:

- **Every required answer carries the same treatment, and it outranks every
  read-only block on its tab.** `.pma-confirm` gets the accent — the same left
  border weight and ground the wizard question uses — so "must be answered"
  looks identical wherever it appears, on Step 2's fields and Step 3's seven
  confirmations alike.
- **Derived read-only summaries lose the accent and the ground.** They are
  body copy under the input they are derived from, distinguished by position
  rather than by decoration. Two wore the costume and both moved:
  `output$threshold_equiv`, the threshold-equivalence summary §3.4.10a names,
  and `output$ois_rrr_equiv`, which reads the Imprecision tab's relative risk
  reduction back on the absolute scale. Neither takes an answer.

The rule is enforced on the source, not on a screenshot: `.pma-confirm`'s left
border is asserted to be no lighter than `.pma-wizard-question`'s, and no block
in `R/step3_grade.R` may pair a 4px `#0f172a` left border with a filled ground
(`test-confirm-checkbox.R`). The publication-bias callouts are deliberately not
covered by the second half — `.pubias_egger_callout()` accents in the judgment's
own green or amber, which is the status vocabulary, not the question's.

Two visual states, both in `www/shadcn.css`, no JavaScript and no server round
trip:

| state | treatment |
|---|---|
| unticked | an uppercase `REQUIRED` pill above the label, a **4px `--primary` left accent on a `--muted` ground** — the wizard question's own weight and ground — inside a 1px `--primary` outline. An outstanding action, legible before anything has been pressed |
| ticked | the muted dashed border these boxes have always had; the `border` shorthand drops the accent, because an answered box must stop claiming "answer this". The pill greys to `--muted` rather than being removed, so the box does not change height and drag the Next button out from under the cursor |

The pill reuses the vocabulary of Step 2's `.pma-required-unset` mark
(§3.3.6): same radius, same type. It sits at `--primary` rather than Step 2's
armed `--destructive` because there is no "armed" tier here — these boxes are
static markup with nothing to arm on, and a permanently red box would destroy
the never-red-on-a-fresh-page property the two-tier scheme exists for.
**Unticked is the base rule** and `:has(input:checked)` is what quietens it, so
a browser without `:has()` degrades to "always looks required" rather than
"always looks done"; only the second of those can let a reviewer walk past a
gate believing it cleared.

This is a legibility change only. `responder_p0_confirmed()`,
`config_blockers()` and `pma_domain_confirmations()` are untouched: the same
clicks are required, they just look required.

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

### 3.4.14 Banking an outcome — automatic, keyed by uid (v0.5.1)

**There is no Save button.** An outcome is banked into `state$outcomes` the
moment its sixth certainty domain is confirmed, and re-banked whenever the
rating it holds changes. `output$save_outcome_panel`, `input$save_outcome`, the
"already saved — replace?" modal and `input$save_outcome_overwrite` are all
**deleted**. The press was never a decision: §3.4.13 already makes confirming
all six domains the reviewer's statement that the rating is finished, and a
button behind that statement could only be forgotten. It regularly was — a
reviewer confirmed six domains, walked to Step 4 and found an empty table.

The trigger is one `observe()` in `step3_server()` over `grade_obj()` +
`domain_confirmed()`, **debounced 750 ms**, storing when
`.save_blocked_reasons()` is empty. Three guards, each answering a way the
naive version banks the wrong row:

| guard | what it stops |
|---|---|
| `req()` on `state$step == 3` | all four step servers are wired unconditionally in `app.R`, and `grade_obj()` / `domain_confirmed()` read Step 3 `input$` widgets that are destroyed whenever another step's body renders. Off-step the observer must not even take the dependency |
| `req()` on `state$outcome_gen` unchanged since the top of the observer | `begin_new_outcome()` bumps the generation and blanks the outcome name in one tick; a save queued before the bump must not land after it |
| `.save_key()` returns `NULL` for a blank outcome name, and the observer no-ops on `NULL` | the old fallback was the literal string `"Outcome"`, so a save firing during the reset banked a row called *Outcome*. A nameless outcome is not saveable, and the reviewer has to name it in Step 2 anyway |

**Identity is a uid, not the name.** `state$outcome_uid` is minted by
`begin_new_outcome()` (and at session start, for the first outcome), stamped
onto the stored object as `attr(g, "pma_outcome_uid")`, and
`pma_upsert_outcome(outcomes, name, g, uid)` (`R/ui_helpers.R`, pure and unit
tested) drops any existing row carrying that uid before inserting under the
*current* display name. So **renaming an outcome in Step 2 renames its row**.
Before this it added a second one — a pre-existing bug that auto-save would
have made constant, since every keystroke in the name field re-banks.

`names(outcomes)` stays the **display name**. `grade_table()`,
`pma_saved_outcomes_ui()`, `.outcome_set()` and `set$order` all key on it, so
the list is not re-keyed by uid; the uid rides along as an attribute, the same
way the dataset signature does.

`attr(g, "pma_saved_at")` keeps its name — it is on every stored object — but
under auto-save it means *last recomputed at*, so the row label reads
**"last updated"**.

**Nothing about the banking is on Final certainty.** A "Saved for the Summary
of Findings table" section used to end that tab — a heading, the
`multi_outcome$save_intro` copy, `output$autosave_status` ("Saved automatically
as **&lt;name&gt;**", or the outstanding domains as `pma_domain_jump_links()`)
and a second `pma_add_next_outcome_button()`. All four are **deleted**, along
with the `autosave_jump_` id prefix that existed only because that line and
`cert_incomplete_banner` named the same domains in the DOM at once. The section
described the saved rows a step away from the rows themselves; Step 4 shows the
list, the count and the button beside the table they build (§3.5.5). **The
auto-save observer is unchanged** — it never depended on this UI.

**The saved-outcome list moved to Step 4** (§3.5.5). The per-row observers
(`outcome_delete`, `outcome_move`, `outcome_primary`) did **not**: they read
`.outcome_set()` and the `state$sof_primary` helpers, which are step3-local, so
moving them costs a refactor for no user benefit. The comment above them says
so.

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

All American English copy lives in `R/educational_copy.R` as named lists for easy maintenance and translation later. Dynamic content (e.g., the threshold label per `sm`) is rendered by helpers that consume `EDU_COPY` plus runtime data.

**The decision-threshold input is rendered dynamically:**

```r
# In step3_grade.R, output$threshold_panel (Configuration tab)
threshold_suggestion <- pmatools::suggest_threshold(state$ma)  # NULL if sm unrecognized
threshold_label      <- EDU_COPY$threshold_labels[[state$ma$sm]] %||%
                        "Threshold for clinical importance"
threshold_help       <- EDU_COPY$threshold_help[[state$ma$sm]]   %||% "..."

ui <- numericInput(
  inputId = "threshold_cont",                # continuous branch
  label   = threshold_label,
  value   = threshold_suggestion$threshold %||% NA,
  min     = 0,
  step    = 0.01
)
# threshold_suggestion$source drives the badge: "Core GRADE 6" vs
# "pmatools convention, not Core GRADE". threshold_scale is derived from the
# branch and from input$threshold_mode, never asked for directly.
```

The user's edit replaces the default; the value is held in a `reactiveVal` (not read straight off the input) so a panel rebuild cannot discard it, and `state$grade_args$threshold` tracks the live value.



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
  # Named for the decision THRESHOLD, which is what the app asks for and what
  # grade_meta() takes. "MID" survives only where a source quotes it.
  threshold_labels = list(
    OR  = "Threshold (as OR ratio, e.g., 1.25 = 25% relative odds change)",
    RR  = "Threshold (as risk ratio, e.g., 1.20 = 20% relative risk change)",
    HR  = "Threshold (as hazard ratio, e.g., 1.20)",
    RoM = "Threshold (as ratio of means, e.g., 1.10)",
    SMD = "Threshold (in standardized units, e.g., 0.20 = Cohen's small)",
    MD  = "Threshold (in outcome units; default = 0.20 × pooled SD)",
    ARD = "Threshold (as absolute risk difference, e.g., 0.05 = 5%)"
  ),
  threshold_help = list(
    OR  = "An OR of 1.25 vs 1.0 represents a 25% relative change in odds — a typical small but clinically meaningful effect.",
    RR  = "An RR of 1.20 vs 1.0 represents a 20% relative change in risk.",
    HR  = "An HR of 1.20 represents a 20% relative change in hazard.",
    RoM = "A 10% ratio of means is a typical small clinically meaningful difference for continuous outcomes.",
    SMD = "Cohen's small effect size (0.20) is widely accepted as the smallest clinically meaningful SMD.",
    MD  = "Auto-suggested as 0.20 × pooled SD (Cohen's small in raw units). Replace with a published threshold for your outcome whenever possible.",
    ARD = "A 5% absolute risk difference is a typical small clinically meaningful effect."
  ),
  config_tab = list(
    continuous_intro     = "...",   # Core GRADE 6's three presentations
    continuous_departure = "...",   # the two are offered as a choice, not together
    chinn_caveat         = "...",   # Chinn's formula is not Core GRADE 6 option 2
    responder_default    = "..."    # the 20% starting value is an app convention
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
input$rob_some_concerns / small_values /   │   pmatools::grade_meta()     │
input$threshold_cont / threshold_abs /      │     ↓                         │
input$threshold_ratio / threshold_mode /    │   state$grade ← pmatools obj │
input$inconsistency_override /              │     ↓                         │
input$indirectness /                        │   pmatools::sof_table()      │
input$imprecision_override /                │     ↓ (preview)              │
input$pubias_* / etc.                       │   htmltools_value(ft)        │
input$per / prediction / sof_presentation / │                              │
input$baseline_risk_chinn / threshold_label │                              │
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

### 7.1 How the app gets pmatools

**The app sources pmatools; it never installs it.** A stale `GITHUB_PAT` cached
on the shinyapps.io account makes `install_github(ykfrkw/pmatools)` return
HTTP 401 on the build server. Shipping the package sources inside the bundle
removes the install step, and with it the failure.

`Rscript shiny/stage_bundle.R` is what puts them there. It runs from anywhere in
the repo, reads `..` — the repository it lives in, so there is no second
checkout and no version skew — and does five things:

1. Wipes `shiny/R/_pmatools/` and copies every `../R/*.R` into it, **except
   `data.R`**, which is roxygen for the lazy-loaded `data/` that the app must
   never depend on.
2. Writes `R/_pmatools/VERSION`. Line 1 is the `Version:` field of
   `../DESCRIPTION` and nothing else, because `app.R` reads exactly one line.
   Line 2 is provenance: `source: <branch>@<sha>`, suffixed `-dirty` when
   staged from uncommitted work, or `source: unknown` when git cannot answer.
3. Wipes `shiny/_pmatools_inst/` and copies `../inst` into it.
4. Rewrites every `system.file("templates", <name>, package = "pmatools")` in
   the staged tree to
   `file.path(getOption("pmatools.vendored_root", "."), "_pmatools_inst", "templates", <name>)`.
   The rewrite is driven by a scan for that call *shape*, not a filename list,
   so a template added upstream is picked up with no edit here.
5. Runs two checks and **warns** (never stops) on either: the dependency-sync
   check of §2.1, and a grep of the staged tree for any surviving
   `system.file(..., package = "pmatools")`, reported with `file:line`. In the
   app that call can only return `""`, so a survivor is a template that will not
   be found at runtime.

The dependency-sync check ignores `testthat`, `rmarkdown`, `here`, `knitr`,
`covr`, `devtools`, `usethis` and `roxygen2` as dev-only; everything else in
`../DESCRIPTION` `Imports` + `Suggests` is expected in `shiny/DESCRIPTION`
`Imports`.

Both checks run standalone, touching no staged file:

```bash
Rscript shiny/stage_bundle.R --check-only
```

`app.R` then, at startup: `source()`s every `.R` file in `R/_pmatools/` (order
does not matter — they define functions only); reads the first line of
`R/_pmatools/VERSION` into `options(pmatools.version_stamp = )`, leaving the
option unset if the file is missing, unreadable or blank; and pins
`options(pmatools.vendored_root = normalizePath(getwd()))` so a later `setwd()`
cannot move the staged templates and sample data out from under the paths above.

Staging from a dirty tree is normal and deliberate — you stage precisely to try
out the change you just made in `../R`. The clean-tree requirement lives in
`deploy.R` instead (§7.3), where it belongs.

`../CLAUDE.md` §1 is the standing statement of the rules this arrangement
imposes on package-level code. Read it before adding a dependency, a
`system.file()` call site, or anything that reads
`utils::packageVersion("pmatools")`.

### 7.2 Local verification

```bash
Rscript shiny/stage_bundle.R          # needed once per clone, and after any ../R change
Rscript -e 'shiny::runApp("shiny", launch.browser = TRUE)'
```

Exercise all 4 steps with the bundled sample data; confirm Step 1 → 4 → ZIP
download. The footer reports the staged pmatools version, which should match
`../DESCRIPTION`.

### 7.3 Deploy

```bash
Rscript shiny/deploy.R
```

That is the whole procedure. There is no `install_github()` step and no
`Remotes:` field; adding either restores the HTTP 401 of §7.1. `deploy.R` runs
five steps:

1. **Refuse a dirty tree.** `git status --porcelain` over the repository root;
   any output aborts, listing the first ten entries. `deployApp()` ships the
   working tree as it stands, so a dirty deploy puts bytes in production that
   no commit describes. It then prints the short HEAD SHA it is shipping — put
   that SHA in the follow-up commit.
2. **Stage**, by `source()`ing `stage_bundle.R` **with warnings promoted to
   errors**. Both of §7.1's checks warn rather than stop, and both describe
   problems that are invisible locally and fail only in production; the deploy
   gate is where they have to be fatal.
3. **Verify `.rscignore` exists.** It is a permanent, committed file — one glob
   per line, read by rsconnect at deploy time — that keeps `SPEC.md`,
   `deploy.R`, `stage_bundle.R`, `tests/`, `rsconnect/`, the reference PDFs and
   the local `.Rproj`/`.claude` metadata out of the bundle. Missing → abort with
   the `git checkout` that restores it. `deploy.R` does not generate it.
4. **Deploy.** Anything matching `app_*.R` is first renamed into
   `.deploy_excluded/` and restored on exit, because non-UTF-8 bytes in a
   filename break rsconnect's path scanner. Then:

   ```r
   rsconnect::deployApp(
     appDir         = APP_DIR,        # this directory, resolved from --file=
     appName        = "pmatools",
     account        = "yuki-furukawa",
     server         = "shinyapps.io",
     forceUpdate    = TRUE,
     launch.browser = FALSE,
     quarto         = FALSE
   )
   ```

   The full `server` / `account` / `appName` triple is named on purpose (§2.2).
5. **Report**, and exit non-zero on failure.

**`APP_NAME` is not a rename.** Changing it in `deploy.R` creates a *new* app at
a *new* URL; the live app is embedded in WordPress post 1021 at
https://yukifurukawa.jp/pmatools/, and that iframe keeps pointing at the old
one. `../CLAUDE.md` §3 lists what has to move with it, and in what order.

If a deploy fails on the build server, the likely causes are:

1. A package used at runtime but absent from `shiny/DESCRIPTION` `Imports` →
   the build succeeds and the feature fails live. Audit with
   `stage_bundle.R --check-only` (§2.1).
2. A system dependency missing on shinyapps.io (e.g. libxml2 for officer) →
   declare it in `../DESCRIPTION`'s `SystemRequirements`.
3. Bundle size over the plan limit → widen `.rscignore` or upgrade the plan.

### 7.4 Post-deploy smoke test

Open https://yuki-furukawa.shinyapps.io/pmatools/ and run through Steps 1–4
with the bundled sample data. Confirm the footer's pmatools version matches
`../DESCRIPTION` at the SHA that was deployed — the footer carries the version
string only, so the SHA itself is the one `deploy.R` printed in step 1 — and
that the ZIP download produces a valid archive whose `analysis.R` runs under
`Rscript`.

---

## 8. Testing

Manual smoke test at minimum:

1. Step 1: load sample → mapping shows green-checked → preview renders → Next
2. Step 2: defaults → Run analysis → forest renders → funnel renders with Egger annotation → result text shows pooled OR
3. Step 3: open each accordion → algorithm explanation reads naturally → auto judgment matches expected for sample data → set the threshold = 0.20 → see zone counts update → click Indirectness "No" → banner clears
4. Step 4: bundle name → Download ZIP → unzip → 9 files present → open analysis.R in R → `source("analysis.R")` reproduces same TE.random

Automated, and the one to run before every deploy:

```bash
Rscript shiny/tests/testthat.R
```

It is deliberately a separate suite from the package's `devtools::test()`: the
app loads its code with `source()` and the package loads it as a namespace, and
the two cannot share a harness.

---

## 9. Versioning

**Two versions, and they are not the same number.**

| file | field | what it versions |
|---|---|---|
| `../DESCRIPTION` | `Version:` | the pmatools package. The real one. |
| `shiny/DESCRIPTION` | `Version:` | this app's wizard/UI work |

Neither number is restated here. `shiny/DESCRIPTION` is the field of record and
the header at the top of this file mirrors it; a copy in this table would be a
third place to forget, which is exactly how the header itself sat at 3.1.0 for
five days after the field moved to 3.2.0.

The app's version tracks separately because the two artifacts change for
different reasons, and rsconnect ignores the field entirely (§2.1). It is not
read at runtime by anything.

**There is no minimum-pmatools constraint, and there cannot be one.** The app
does not install pmatools, so `Imports: pmatools (>= x.y.z)` would express
nothing: `stage_bundle.R` stages `../R` from the repository it lives in, so the
app always runs the sources of the commit being deployed. What the app reports
at runtime is that exact version, read from `R/_pmatools/VERSION` (§7.1), and
line 2 of that file names the commit it came from.

A pmatools API change is therefore not a constraint to bump but a change to
make in the same commit: the app is one directory away and the package `SPEC.md`
is authoritative for both.

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
