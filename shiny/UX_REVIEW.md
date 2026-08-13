# UI/UX review — pmatools Shiny wizard

Reviewed 2026-08-13 against the app running locally from a freshly staged bundle
(`main@2edfdfc-dirty`, pmatools 0.5.1), walked end to end with the bundled CBT-I
sample dataset at desktop (1440px) and phone (375px) widths.

This is a **review and a plan, not a change**. Nothing in `R/` or `shiny/` was
edited. Items marked *(feedback)* implement a point from the "未着手フィードバック"
section of [PLAN.md](../PLAN.md); the two documents are meant to be worked
together, because several UI fixes are the visible half of a package-side rename.

---

## Summary

| Dimension | Rating | Basis |
|---|---|---|
| Cognitive load | 🔴 | Instructions are 60–100-word gray paragraphs on nearly every control; Step 3 Configuration shows six paragraphs before the first input. No tooltip/disclosure mechanism exists (`pma_help()` is defined and used zero times). |
| Navigation | 🟡 | The 4-step stepper is clear and marks done steps green. Inside Step 3 a second, stateless 7-tab bar carries six required confirmations with no per-tab progress. |
| Error prevention / recovery | 🟡 | Export gating, provenance guards and required-rationale-on-override are genuinely strong. But gate messages are dead text that name a tab instead of linking to it, and a backend failure renders a raw R error string where a badge belongs. |
| Task flow | 🟡 | The wizard order matches the analysis order. Step 2's primary CTA sits below ~20 controls at the bottom of a 1,700px sidebar. |
| Consistency | 🟡 | One card/badge vocabulary throughout, and the app already speaks Core GRADE wording — which the package API does not (see C-2). Three different "next" affordances coexist. |
| Accessibility | 🟡 | Certainty is never color-only (color + word + downgrade chip). Muted text is 4.76:1 — AA-passing but carries required instructions at 0.8rem. **Horizontal overflow at 375px is a hard failure** (below). |

The app's real strength is the part most tools get wrong: the algorithm is not a
black box. `pma_flowchart()` highlights the branch each judgment took, the facts
list shows the numbers behind it, and every manual override demands a written
rationale. **Nothing below should be fixed by removing that.** The problem is
that the explanation is always on, always prose, and always at full length.

---

## Critical

### C-1. The page scrolls sideways on a phone

**Current.** Step 2 lays out a fixed sidebar and a right pane:
[step2_ma.R:68](R/step2_ma.R:68) `flex: 0 0 320px` and
[step2_ma.R:199](R/step2_ma.R:199) `flex: 1; min-width: 480px`. At a 375px
viewport the document measures `scrollWidth 492` against `clientWidth 375`. The
7-tab bar at [step3_grade.R:209](R/step3_grade.R:209) overflows the same way.

**Problem.** The live app is embedded in an iframe in WordPress post 1021, which
is read on phones. A horizontally scrolling page inside an iframe whose height is
driven by `embed-height.js` is the worst case: the reader cannot see the right
edge of the results and cannot easily scroll to it.

**Fix.** `min-width: min(480px, 100%)` on the right pane and `flex: 1 1 320px`
on the sidebar, so the two stack cleanly below ~800px; give the Step 3 tab strip
`overflow-x: auto` with scroll-snap, or swap it for a `selectInput` domain picker
below the `sm` breakpoint. Add a regression check at 375px to the app test suite.

**Reference.** WCAG 1.4.10 Reflow; ベイジ UIラボ「リキッドレイアウト」.

### C-2. Step 3 has six required confirmations and no visible progress

**Current.** `state$domain_confirmed` tracks six keys
([ui_helpers.R:83](R/ui_helpers.R:83)). The only surfaces that report them are a
banner on the 7th tab and a locked-download block on Step 4, both plain prose
listing names ("Unconfirmed: Configuration, Inconsistency, Indirectness,
Imprecision, Publication bias"). The tab bar itself is stateless, and so is the
"3 Certainty" node of the main stepper.

**Problem.** The user is told *what* is missing in a sentence, on a different
screen from where they fix it, and must translate names back into tab clicks.
This is the single largest source of "why is my export still locked?".

**Fix.** Three changes, cheap and independent:
1. A state marker on every domain tab label (`✓` confirmed / `●` visited,
   unconfirmed / nothing). `tabsetPanel` labels accept tag lists.
2. `Certainty 3/6` on the stepper node and in the Step 3 card header.
3. Make each domain name in the incomplete banner and in the Step 4 lock an
   `actionLink` that switches to that tab (`updateTabsetPanel`) — the wiring for
   free tab jumping already exists.

**Reference.** Nielsen #1 (visibility of system status); NN/G progress
indicators for multi-step forms.

### C-3. "Run analysis" is below the fold, under twenty controls

**Current.** The Step 2 sidebar stacks, in one column: outcome name, direction
(2 long radio options), a 40-word note, follow-up, a 45-word note, conditional
unit + note, six column-mapping selects, outcome type, summary measure, model,
pooling method, τ² estimator, continuity correction, subgroup column, subgroup
order, auto-rerun checkbox, and finally `run_ma` at
[step2_ma.R:192](R/step2_ma.R:192) — roughly 1,700px down.

**Problem.** Miller's law says 4±1; this is a group of twenty. The primary
action of the whole step is the last thing on screen, and the right pane sits
empty beside 1,000px of dead space once the plot has rendered.

**Fix (decided 2026-08-13: collapse).** (a) `position: sticky; bottom: 0` action bar inside the sidebar card
holding *Run analysis* + *Auto-rerun*, so the CTA is always reachable. (b) Split
the sidebar into **Outcome** (name, direction, follow-up, unit) → **Data
mapping** (the six selects) → collapsed **Model details** (summary measure,
model, method, τ², continuity correction) → collapsed **Subgroup**. Defaults are
already correct for the common case, so collapsing costs nothing and cuts the
visible group count from ~20 to ~8.

**Reference.** ヒックの法則; NN/G form Structure; Progressive Disclosure.

---

## Important

### I-1. The risk-of-bias decision is spread over three screens *(feedback)*

**Current.** To answer "why is Risk of Bias *Serious*?" a reviewer needs:

| What | Where |
|---|---|
| Where "some concerns" falls (low vs high) | Configuration tab, `rob_some_concerns` |
| Sensitivity-analysis change threshold (10%) | Configuration tab, `rob_inf_threshold` |
| The dominance gate (55% weight) | only inside the flowchart `<details>` |
| The five direction-of-bias rules | only inside the same flowchart |
| Per-study RoB values | collapsed `<details>` on the RoB tab — *and* buried in Step 1's 39-column preview |
| The verdict and its facts | RoB tab, above all of it |

**Problem.** This is exactly the "RoB のルールが分かりづらい" feedback, and the
cause is structural rather than editorial: four knobs, three homes, two of them
never stated outside a collapsed element. The Configuration tab's honesty about
which parts are pmatools' own convention rather than Core GRADE (a genuinely
good touch) is also spent where the reviewer is not yet thinking about RoB.

**Fix (decided 2026-08-13).** This **supersedes the earlier decision** recorded
in the comments at [step3_grade.R:364](R/step3_grade.R:364), which moved
`rob_some_concerns` and `rob_inf_threshold` to Configuration on the grounds that
both are review-wide rather than per-outcome. That reasoning is sound and is not
being overturned as reasoning — the two settings *are* review-wide. What it
underweighted is that a value can be review-wide and still be unreadable when it
is stated three screens away from the verdict it produces. Keep them review-wide
in scope (they persist across outcomes exactly as now); move only where they are
*edited*.

Give Risk of Bias one screen that reads top to bottom:
verdict → the three numbers that produced it (`some concerns` fold, dominance
share, observed change vs threshold), each as an **inline editable value in the
sentence that uses it**, not a control on another tab → per-study grid (open by
default, with the bulk buttons) → stratified forest → override. Keep the
flowchart, collapsed, as the "show the whole map" affordance it is now.
Configuration keeps only what genuinely crosses domains (control-group risk,
threshold, per-unit).

### I-2. Two navigation metaphors, three "next" affordances

**Current.** The 4-step stepper (`actionLink`s), the Step 3 `tabsetPanel`, and
per-tab `Back: X / Next: Y` buttons ([step3_grade.R:121](R/step3_grade.R:121)),
plus the global `pma_wizard_nav()` Back/Next. On the Final certainty tab, "Next:
Export" and the stepper's "4 Export" do different things (one is gated, one is
not).

**Fix.** Keep the tabs as the domain switcher, keep per-tab Next as the
default path, but drop the global wizard nav from Step 3 (the tab-level nav
already covers it) and make the stepper's Export node reflect the same gate as
"Next: Export" so the two cannot disagree.

### I-3. Instructional prose has no compression mechanism *(partly feedback)*

**Current.** `pma_help()` ([ui_helpers.R:26](R/ui_helpers.R:26)) renders a
`(?)` with `data-bs-toggle="tooltip"`. It is called **nowhere**, and no code
ever runs `new bootstrap.Tooltip(...)`, so tooltips would not work if it were.
The compensation is `pma-card-subtitle` paragraphs — muted, 0.875rem — carrying
required instructions under nearly every input.

**Problem.** Everything is equally loud, so nothing is. Required instructions
are rendered in the same gray as optional commentary; the Step 3 Configuration
tab shows six paragraphs before the first control.

**Fix.** Initialise Bootstrap tooltips once in `app.R`'s head, then adopt a
three-tier rule and apply it mechanically:
- **inline, full colour** — text needed to answer the question on screen;
- **`(?)` tooltip** — definitions and units;
- **`<details>`** — provenance, caveats, departures from the source.

The `<details>` tier already exists and is used well; the missing middle tier is
what forces one-liners to become paragraphs. Note that the MIC paragraph at
[step3_threshold.R:410](R/step3_threshold.R:410) is tier 3 content sitting in
tier 1 — and its wording is itself a feedback item (see F-1).

### I-4. Step 1 shows 39 raw columns and validates almost nothing visibly

**Current.** The preview is the ingested table as-is: for the sample dataset,
39 columns wide with `age_n`, `n_hypnotic`, `group1`, `severity_scale`, … ahead
of the ones the analysis uses. The only feedback is a monospace line, `Status:
36 rows, 18 studies (long format).` Per-study RoB is editable here in principle,
but you must scroll horizontally past 30 columns to reach `rob`, and the bulk
Set-all buttons exist only in Step 3.

**Problem.** Step 1's job is "is my data right?", and the screen answers "here
is your data". Nothing states which columns were recognised as `studlab` /
`treat` / `n` / `event` / `rob` / `indirectness`, which is precisely what an
upload gets wrong.

**Fix.** A detected-columns strip above the table — one chip per role, green
when found, amber with a "map it in Step 2" hint when not — and a **Analysis
columns / All columns** toggle defaulting to the six roles. Move the RoB bulk
buttons here too; assigning RoB across 17 studies is Step 1 work.

**Reference.** Empty/validation states, NN/G; ベイジ「入力インターフェースで
エラーを予防」.

### I-5. Backend failure renders a raw R error where a judgment belongs

**Current.** Reviewing against a stale vendored bundle, the Risk of Bias tab
rendered `Error: could not find function ".grade_level_wording"` twice — once in
the badge slot beside the domain title, once in place of the evaluation block.
Re-staging fixed it, so this is not a production defect today; the *handling* is.

**Fix.** Wrap the per-domain evaluation renderers in a failure branch that emits
a neutral card ("This domain could not be evaluated — the analysis or the
bundled pmatools version is out of step. Re-run Step 2, or report this."), and
have Step 4 refuse to export rather than shipping a bundle built around a failed
domain.

### I-6. Duplicated blocks of text

The "Not implemented in this table" note is printed **twice** on the Final
certainty tab: as a footnote under the SoF table and again as a bordered box
below it. The export-lock explanation is likewise near-identical on Step 3 and
Step 4. Deduplicate; the footnote is the one that travels into the `.docx`, so
keep that and delete the box.

### I-7. Forest plot title collides with the column headers

In the RoB-stratified plot, the plot title overlaps the `Events / N` and
`OR (95% CI)` headers ("EverDepresNion response", "BiaGR (95% CI)"). Visible on
screen and in the exported PNG/PDF. It shows up when the title is long — the
stratified plot appends "(stratified by Risk of Bias)". Shorten the appended
suffix, or drop the title into a subtitle line above the header row.

---

## Suggestions

### F-1. Retire "MIC" from the UI *(feedback)*

[step3_threshold.R:410](R/step3_threshold.R:410) is the last place the app says
"Minimal Important Change (MIC)". The API is already `threshold` /
`threshold_type` / `threshold_scale`. Rewrite as a `<details>` titled "Why this
is not a minimally important difference" and use `threshold` throughout.

### F-2. Align the downgrade vocabulary, and allow a manual −3 *(feedback)*

**The app is already right; the package is not.** `pma_judgment_choices()`
([ui_helpers.R:738](R/ui_helpers.R:738)) presents *Not serious (-0) / Serious
(-1) / Very serious (-2)* and maps them onto the internal `no` /
`some_concerns` / `serious`, and the badges render Core GRADE's words through
`.grade_level_wording()`. So the label/value indirection in the app exists only
to paper over the package's off-by-one naming.

When the package renames (PLAN.md item 6), this UI can drop the mapping and pass
values straight through. In the same change:
- add **Extremely serious (-3)** to `pma_judgment_choices()`
  ([ui_helpers.R:738](R/ui_helpers.R:738)) — so it reaches the override select on
  all five domain tabs, which is where Core GRADE 1 puts the word. Manual only,
  never produced by the algorithm, and requiring a rationale as every override
  already does;
- keep the automated path capped at −2 and say so once, on the Configuration
  tab, rather than in each domain.

**Decided 2026-08-13:** −3 goes on the per-domain overrides only.
`other_downgrade` ([step3_grade.R:823](R/step3_grade.R:823)) stays at 0 / −1 /
−2 — "Other considerations" is not a Core GRADE domain, so extending it to −3
would invent a rating the source does not describe.

### F-3. RoB 2 is three levels, and the editor should say so *(feedback)*

The Configuration copy correctly uses RoB 2's vocabulary ("some concerns"), but
the per-study editors accept `low / some / high` as free text typed into a DT
cell, and the package's alias table mixes in ROBINS-I's "serious concerns" /
"critical concerns". Replace the free-text cell with a three-value dropdown
labelled exactly as RoB 2 labels it — **Low risk of bias / Some concerns / High
risk of bias** — and, if non-randomised evidence is ever in scope, offer
ROBINS-I's five levels as a separate, explicitly named vocabulary.

This also removes a whole class of typo: `rob_strata()` currently buckets an
unrecognised label into "unknown" with a warning the app never shows.

### F-4. Say what the app is, in the app and in the README *(feedback)*

The README should describe the Shiny app as the primary way pmatools is used
(PLAN.md item 4). Reciprocally, the app never states its own version outside the
Step 2 "Text results" tab ([step2_ma.R:1253](R/step2_ma.R:1253)). Put
`pmatools 0.5.1` in the footer beside the yukifurukawa.jp link — it costs one
line and makes every support question answerable.

### F-5. Smaller items

- Step 2's right pane is empty until the first run; give it a one-line "Press
  *Run analysis* to pool the studies" placeholder rather than a bare card.
- The disabled *Next: Export* button has no `title`; add one naming the gate.
- `Status: 36 rows, 18 studies` is rendered as code. Make it a success banner
  with the same visual language as the warnings that share the screen.
- Muted text (`--muted-foreground`, [shadcn.css:13](www/shadcn.css:13)) is
  4.76:1 on white — AA for body text, but it is used at 0.8rem for SoF
  footnotes. Either lift the size to 0.875rem or darken the token one step.

---

## Proposed sequence

Ordered so that each phase is shippable on its own and nothing waits on the
package rename.

**Phase 1 — reachability (no copy changes, no package changes).**
C-1 responsive overflow, C-2 progress markers and jump links, C-3 sticky CTA and
collapsed model details. These are the three that stop a first-time user, and
none of them touches wording, so they can land while the copy questions are
still open.

**Phase 2 — the Risk of Bias screen.**
I-1 consolidation, I-5 error handling, I-7 plot title. Do F-3 (RoB 2 three-level
dropdown) in the same PR: it is the same screen, and the fix is meaningless
without it.

**Phase 3 — copy tiers.**
I-3 tooltip tier + the mechanical pass over `pma-card-subtitle` paragraphs,
I-6 deduplication, F-1 MIC removal, F-4 version in footer. Largest diff, lowest
risk, easiest to review in isolation.

**Phase 4 — Step 1 data confidence.**
I-4 detected-columns strip, column toggle, bulk RoB buttons moved forward.

**Phase 5 — with the package rename.**
F-2 downgrade vocabulary and the manual −3, once `DESCRIPTION`/`SPEC.md` land the
Core GRADE naming. App-side this is a small diff; it is last only because it
cannot ship first.

Per the repo rules, each phase updates `shiny/SPEC.md` in the same PR as the
change it describes, and a behaviour change edits `shiny/SPEC.md` first.
