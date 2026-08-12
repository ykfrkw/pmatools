###############################################################################
##  pmatools package — Worked Example
##  CBT-I for Depression Response in MDD with Comorbid Insomnia
##
##  Data source:
##    Furukawa Y, Nagaoka D, Sato S, et al.
##    Cognitive behavioral therapy for insomnia to treat major depressive
##    disorder with comorbid insomnia: A systematic review and meta-analysis.
##    J Affect Disord. 2024;367:359-366.
##    doi:10.1016/j.jad.2024.09.017
##
##  GRADE reference:
##    Guyatt G, et al. (BMJ 2025 Core GRADE series)
##    Core GRADE 1-5, BMJ 389, 2025.
##
##  実行方法:
##    1. pmatools.Rproj を RStudio で開く
##    2. このファイルを開き、Ctrl+Enter で1行ずつ実行
##    3. plot/viewer ペインにすべての出力が表示される（ファイル保存不要）
###############################################################################

## ── Prerequisites ────────────────────────────────────────────────────────────
## Install if needed (one-time):
##   install.packages(c("devtools", "meta", "dplyr", "flextable", "officer",
##                      "rmarkdown", "here"))

library(meta)
library(dplyr)
library(flextable)

## ── Load pmatools ─────────────────────────────────────────────────────────────
## devtools::load_all() はパッケージのインストールなしに R/ 以下のコードを全て読み込む。
## pmatools.Rproj を開いた状態で実行すると here::here() がプロジェクトルートを返す。

devtools::load_all(".", reset = TRUE)   # reset=TRUE: 毎回完全リロード（変更が必ず反映される）

## ↑ devtools 未インストールの場合は以下の代替コードを使用:
## pkg_dir <- here::here()
## r_files <- c("utils.R", "domain_rob.R", "domain_indirectness.R",
##              "domain_inconsistency.R", "domain_imprecision.R",
##              "domain_pubias.R", "grade_meta.R",
##              "sof_table.R", "grade_table.R", "grade_report.R")
## for (f in r_files) source(file.path(pkg_dir, "R", f))


## ── 1. Load and prepare data ─────────────────────────────────────────────────
## パッケージ同梱の合成サンプルデータを使用（外部ファイル不要）
## system.file() は devtools::load_all() 後に inst/extdata/ を正しく参照する
data_raw <- read.csv(
  system.file("extdata", "cbti_depression.csv", package = "pmatools"),
  stringsAsFactors = FALSE
)

## ── 実データへの切り替え（論文再現時）────────────────────────────────────────
## data_raw <- read.csv(
##   "~/Library/CloudStorage/OneDrive-Personal/mMEDICI/supplements/R_pairwise_meta_analysis/data/data.csv"
## )

cbti <- data_raw |>
  filter(treatment == "CBT-I", !is.na(d_r)) |>
  select(study, event_e = d_r, n_e = n_randomized, rob_d)

ctrl <- data_raw |>
  filter(treatment == "Control", !is.na(d_r)) |>
  select(study, event_c = d_r, n_c = n_randomized)

ctrl_sum <- ctrl |> group_by(study) |>
  summarise(event_c = sum(event_c), n_c = sum(n_c), .groups = "drop")
cbti_sum <- cbti |> group_by(study) |>
  summarise(across(c(event_e, n_e), sum), rob_d = first(rob_d), .groups = "drop")

df <- inner_join(cbti_sum, ctrl_sum, by = "study")
cat(sprintf("Studies in depression response analysis: k = %d\n", nrow(df)))


## ── 2. Meta-analysis ─────────────────────────────────────────────────────────
##
## NOTE — Deviation from the original paper:
##   Furukawa et al. J Affect Disord 2024;367:359-366 used DerSimonian-Laird
##   (DL). This example uses REML, which is generally preferred for τ²
##   estimation (Viechtbauer 2005; Langan et al. 2019 Res Synth Methods).
##   Effect estimates will differ slightly from the published results.

m_response <- metabin(
  event.e    = event_e,
  n.e        = n_e,
  event.c    = event_c,
  n.c        = n_c,
  data       = df,
  studlab    = study,
  sm         = "OR",
  method.tau = "REML",   # preferred over DL; deviates from original paper
  common     = FALSE,
  random     = TRUE,
  prediction = TRUE,     # compute 95% prediction interval
  incr       = 0.1,
  label.e    = "CBT-I",
  label.c    = "Control"
)

## → Plot ペインに forest plot が表示される（PI も表示）
meta::forest(m_response, xlim = c(0.1, 50), digits = 2,
             prediction = TRUE,
             col.square = "steelblue", col.diamond.random = "navy")


## ── 3. Map Cochrane RoB 2.0 judgments to GRADE levels ────────────────────────
##
##   L = "No concerns"    → internally normalised to "no"
##   S = "Some concerns"  → internally normalised to "some"
##   H = "Serious concerns" → internally normalised to "serious"
##   * = "Some concerns"  → internally normalised to "some"  (conservative)
##
##   Cochrane RoB2 labels are accepted directly by assess_rob() — no pre-mapping needed.

rob_map <- c(L = "No concerns", S = "Some concerns", H = "Serious concerns", `*` = "Some concerns")
rob_vec <- unname(rob_map[df$rob_d])

cat("\nRoB distribution:\n")
print(table(df$rob_d))


## ── 4. GRADE certainty assessment ────────────────────────────────────────────
##
## Domain-by-domain rationale:
##
## [Entry gate — BMJ Core GRADE 2, Fig 2 step 1]
##
##   Before any domain is judged, Core GRADE 2 asks what the certainty rating
##   is *about*. That is fixed by `threshold_type`:
##     "mid"  = certainty in whether the effect is IMPORTANT → a minimal
##              important difference (MID) is mandatory, because "important"
##              is undefinable without one. grade_meta() aborts if it is
##              missing (pass require_threshold = FALSE to override).
##     "null" = certainty in whether there is ANY true (non-null) effect →
##              no MID needed. See section 4b.
##   Here we rate importance, so a MID is supplied. suggest_threshold(m_response)
##   returns the conventional OR 1.25 for binary outcomes; a review-specific,
##   published or expert-derived MID is always preferable.
##
## [Risk of Bias — BMJ Core GRADE 4, Fig 2]
##
##   small_values = "undesirable": small values (low response rates) are bad,
##   so a large OR = more response = desirable. This defines which direction
##   of shift counts as bias-favouring.
##   Domination check (threshold 60%):
##     6/17 studies are Serious-RoB, but Henry2020 (n=3,352, "Some concerns")
##     dominates the weight. High-RoB weight ≈ 38% < 60% → NOT dominated.
##   → Do not rate down.
##
##   If dominated, the package automatically compares:
##     TE(all studies) vs TE(excl. high-RoB)
##     small_values = "undesirable": TE_all > TE_low → inflates → rate down
##   Which side "Some concerns" studies land on is itself a review decision:
##   see rob_some_concerns in section 6b.
##
## [Indirectness — BMJ Core GRADE 5]
##   Population/intervention/comparator/outcome are directly applicable.
##   → "no" concern.
##   NOTE: `indirectness` defaults to NULL, which is read as "no". Passing
##   "no" explicitly (as below) is equivalent here, but it is NOT equivalent
##   once an `indirectness_subdomains` table is supplied — there any non-NULL
##   scalar is a manual override of the subdomain worst case. See section 4c.
##
## [Inconsistency — BMJ Core GRADE 3, Fig 2]
##   I² ≈ 36%, tau² ≈ 0.17, Q p ≈ 0.07.
##   Left at NULL → the flowchart runs automatically: I² > 25% triggers step 2,
##   which tallies where the per-study estimates sit relative to ±MID. All 17
##   studies are on the benefit side (13 above the MID, 4 in the trivial zone),
##   so there is no opposite-direction disagreement — but no single zone holds
##   ≥ 80% either, so the magnitude of benefit is genuinely heterogeneous.
##   → Rate down 1 level.
##   (I² statistics are supplementary and reported in the domain note.)
##   Override manually if the panel disagrees:
##     inconsistency_ci_diff        = "yes"
##     inconsistency_threshold_side = "majority_one_side"
##
## [Imprecision — BMJ Core GRADE 4, Fig 4]
##   OR 2.30 [1.64, 3.23] lies entirely beyond the MID, so the effect is
##   definitively important and the Fig 4 "implausibly large effect" branch
##   sends the judgment to the OIS check.
##   OIS: using baseline response rate ~25% and expected experimental ~40%,
##        power 80%, α = 0.05 → auto-calculated target events; met.
##
## [Publication Bias — BMJ Core GRADE 4, Fig 5]
##   Studies are not predominantly small and industry-sponsored (k=17, many
##   academic trials). k ≥ 10 → Egger's test auto-computed.

g_response <- grade_meta(
  meta_obj               = m_response,
  study_design           = "RCT",

  ## Entry gate (BMJ Core GRADE 2 Fig 2 step 1)
  threshold_type         = "mid",            # rating certainty in IMPORTANCE
  threshold              = 1.25,             # MID on the OR scale
  threshold_scale        = "ratio",          # 1.25 is an OR, not a log OR

  ## Risk of Bias
  rob                    = rob_vec,          # per-study Cochrane RoB2 labels → flowchart
  rob_dominant_threshold = 0.60,             # default: >60% weight = dominated
  small_values           = "undesirable",    # small values are bad → large OR desirable

  ## Indirectness
  indirectness           = "no",             # NULL (the default) means the same here

  ## Inconsistency (BMJ Core GRADE 3 flowchart — auto-computed when NULL)
  ## Leave NULL → auto-detects from I², tau², Q p, and per-study TE zone tally

  ## Imprecision (OIS auto-calculated)
  outcome_type           = "relative",
  ois_p0                 = 0.25,             # control event rate (approx)
  ois_p1                 = 0.40,             # CBT-I event rate (approx)
  ois_alpha              = 0.05,
  ois_beta               = 0.20,

  ## Publication Bias (BMJ Core GRADE 4 Fig 5)
  pubias_small_industry  = "no",             # academic multi-centre trials
  ## pubias_funnel_asymmetry = NULL → Egger's test run automatically (k ≥ 10)

  outcome_name           = "Depression response"
)

print(g_response)
summary(g_response)
## Expected: Moderate (High − inconsistency 1 level = Moderate; the rating
##   target is auto-derived as "Important effect" because the point estimate
##   sits beyond the MID)


## ── 4b. Entry gate variants — threshold_type and rating_target ───────────────
##
## [BMJ Core GRADE 2, Fig 2 steps 1-3]
##
## Step 1 fixes the threshold; steps 2-3 derive the *rating target* from where
## the point estimate falls relative to it. pmatools does that automatically:
##
##   |point estimate| beyond the MID  → "Important effect"
##   |point estimate| inside the MID  → "Little to no difference"
##   no MID (threshold_type = "null") → "Non-null effect"
##
## The target matters because Imprecision is judged against it: an
## "Important effect" rating asks whether the CI excludes the MID, whereas a
## "Non-null effect" rating only asks whether it excludes the null.

## (i) No MID available → rate certainty in a non-null effect.
g_null <- grade_meta(
  meta_obj       = m_response,
  study_design   = "RCT",
  rob            = rob_vec,
  small_values   = "undesirable",
  threshold_type = "null",                   # no MID needed on this path
  outcome_type   = "relative",
  ois_p0 = 0.25, ois_p1 = 0.40,
  pubias_small_industry = "no",
  outcome_name   = "Depression response (no MID: non-null effect target)"
)
cat("\nAuto-derived rating target without a MID:", g_null$rating_target, "\n")

## (ii) Manual override of the auto-derived target. Because this replaces a
##      Core GRADE 2 Fig 2 derivation, a written rationale is MANDATORY —
##      grade_meta() aborts without one (transparency principle).
g_target <- grade_meta(
  meta_obj                = m_response,
  study_design            = "RCT",
  rob                     = rob_vec,
  small_values            = "undesirable",
  threshold_type          = "mid",
  threshold               = 1.25,
  threshold_scale         = "ratio",
  rating_target           = "non_null_effect",
  rating_target_rationale = paste(
    "The guideline panel rated certainty in whether CBT-I has any effect on",
    "depression response, not in whether the effect exceeds the MID, because",
    "no acceptable alternative treatment exists for this population."
  ),
  outcome_type = "relative",
  ois_p0 = 0.25, ois_p1 = 0.40,
  pubias_small_industry = "no",
  outcome_name = "Depression response (manual rating target)"
)
cat("Manual rating target:", g_target$rating_target,
    "| auto-derived:", g_target$rating_target_auto, "\n")


## ── 4c. Indirectness subdomains (PICO) ───────────────────────────────────────
##
## [Indirectness — BMJ Core GRADE 5]
##
## Core GRADE 5 asks the indirectness question separately for each element of
## the PICO, on a 4-point scale ("Is the evidence sufficiently direct?"):
##
##   yes / probably_yes → no downgrade
##   probably_no        → serious indirectness      (rate down 1)
##   no                 → very serious indirectness (rate down 2)
##
## The domain judgment defaults to the WORST case across subdomains, and
## indirectness_table() renders the reasoning in the BMJ publication format.

ind_sub <- data.frame(
  subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
  target    = c("Adults with major depressive disorder and comorbid insomnia",
                "Cognitive behavioural therapy for insomnia (CBT-I)",
                "Treatment as usual or attention control",
                "Depression response (>=50% reduction in depression severity)"),
  evidence  = c(paste("17 RCTs; most recruited from sleep or psychiatry clinics,",
                      "broadly representative of the target population."),
                paste("Both therapist-delivered and digital CBT-I; both are",
                      "used in practice."),
                paste("Comparators ranged from waitlist to active attention",
                      "control, which inflates the contrast against waitlist."),
                paste("Response was derived from continuous depression scales",
                      "at 8-12 weeks; longer-term response was not measured.")),
  judgment  = c("yes", "probably_yes", "probably_no", "probably_yes"),
  stringsAsFactors = FALSE
)

g_indirect <- grade_meta(
  meta_obj                = m_response,
  study_design            = "RCT",
  rob                     = rob_vec,
  small_values            = "undesirable",
  threshold_type          = "mid",
  threshold               = 1.25,
  threshold_scale         = "ratio",
  indirectness_subdomains = ind_sub,
  ## `indirectness` is deliberately NOT passed. Leave it at its NULL default
  ## whenever the subdomain worst case should stand: any non-NULL scalar is
  ## treated as a manual override and then requires indirectness_rationale.
  outcome_type            = "relative",
  ois_p0 = 0.25, ois_p1 = 0.40,
  pubias_small_industry   = "no",
  outcome_name            = "Depression response (PICO subdomains)"
)
print(g_indirect)

## → Viewer ペインに Core GRADE 5 形式の subdomain 表が表示される
ft_indirect <- indirectness_table(g_indirect)
print(ft_indirect)


## ── 5. Summary of Findings table (single outcome) ────────────────────────────
## → Viewer ペインに flextable が表示される（ファイル保存不要）

ft_pastel <- sof_table(g_response)                           # default: pastel, per 1,000
print(ft_pastel)

ft_classic <- sof_table(g_response, palette = "classic")     # classic palette
print(ft_classic)

ft_per100 <- sof_table(g_response, per = 100)                # per 100 patients
print(ft_per100)

ft_pi <- sof_table(g_response, prediction = TRUE)            # show 95% PI in Effect column
print(ft_pi)


## ── 5b. Absolute effect (ARD per 1,000) — 3 ways to set baseline_risk ────────
##
## All three variants rate the same outcome as section 4, so they keep the same
## entry gate (threshold_type = "mid", MID = OR 1.25) and the same direction
## convention (small_values = "undesirable": a low response rate is bad).
## Only the baseline risk used for the absolute effect differs.

## Method 1: explicit numeric (e.g., from published baseline event rate)
g_br_explicit <- grade_meta(
  meta_obj     = m_response, study_design = "RCT",
  rob          = rob_vec, small_values = "undesirable",
  threshold_type = "mid", threshold = 1.25, threshold_scale = "ratio",
  outcome_type = "relative",
  baseline_risk = 0.25,           # <-- direct specification
  outcome_name = "Depression response (baseline_risk = 0.25)"
)
sof_table(g_br_explicit)

## Method 2: simple pooled control-arm proportion
g_br_simple <- grade_meta(
  meta_obj     = m_response, study_design = "RCT",
  rob          = rob_vec, small_values = "undesirable",
  threshold_type = "mid", threshold = 1.25, threshold_scale = "ratio",
  outcome_type = "relative",
  baseline_risk = "simple",       # <-- sum(events_c) / sum(n_c)
  outcome_name = "Depression response (simple)"
)
sof_table(g_br_simple)
cat("Simple pooled baseline risk:", round(g_br_simple$baseline_risk, 3), "\n")

## Method 3: GLMM-pooled via metaprop (logit back-transform)
g_br_metaprop <- grade_meta(
  meta_obj     = m_response, study_design = "RCT",
  rob          = rob_vec, small_values = "undesirable",
  threshold_type = "mid", threshold = 1.25, threshold_scale = "ratio",
  outcome_type = "relative",
  baseline_risk = "metaprop",     # <-- meta::metaprop() GLMM
  outcome_name = "Depression response (metaprop)"
)
sof_table(g_br_metaprop)
cat("metaprop baseline risk:", round(g_br_metaprop$baseline_risk, 3), "\n")

## An absolute MID is also allowed and is what Core GRADE 2 prefers whenever a
## baseline risk is available: threshold_scale = "ard" expresses the MID as a
## risk difference (here 5 more responders per 100) and pmatools converts it to
## the OR scale using the pooled baseline risk.
g_br_ard <- grade_meta(
  meta_obj     = m_response, study_design = "RCT",
  rob          = rob_vec, small_values = "undesirable",
  threshold_type = "mid", threshold = 0.05, threshold_scale = "ard",
  outcome_type = "relative",
  baseline_risk = "simple",
  outcome_name = "Depression response (absolute MID = 5%)"
)
print(g_br_ard)


## ── 6. Sensitivity: restrict to low/some RoB studies ─────────────────────────
df_sens  <- df |> filter(rob_d != "H")
rob_sens <- unname(rob_map[df_sens$rob_d])

m_sens <- metabin(
  event.e = event_e, n.e = n_e,
  event.c = event_c, n.c = n_c,
  data    = df_sens, studlab = study,
  sm = "OR", method.tau = "REML", common = FALSE, random = TRUE, incr = 0.1
)

g_sens <- grade_meta(
  m_sens,
  study_design           = "RCT",
  rob                    = rob_sens,
  rob_dominant_threshold = 0.60,
  small_values           = "undesirable",    # same outcome direction as section 4
  threshold_type         = "mid",
  threshold              = 1.25,
  threshold_scale        = "ratio",
  indirectness           = "no",
  inconsistency_ci_diff  = "yes",
  inconsistency_threshold_side = "majority_one_side",
  ois_p0 = 0.25, ois_p1 = 0.40,
  outcome_name = "Depression response (sensitivity: low/some RoB only)"
)
print(g_sens)


## ── 6b. rob_some_concerns — where "Some concerns" studies land ───────────────
##
## [Risk of Bias — BMJ Core GRADE 4, Fig 2]
##
## The Core GRADE 4 flowchart is binary: every study is either low or high risk
## of bias. RoB 2.0 has three levels, so the review must decide which side
## "Some concerns" falls on. `rob_some_concerns` makes that decision explicit:
##
##   "low"  (default) — lenient; only "Serious concerns" studies count as high
##   "high"           — conservative; "Some concerns" studies count as high too
##
## In this dataset the choice moves the high-RoB weight share from 38% to 84%,
## which flips the first flowchart node from "not dominated" to "dominated" and
## makes the package compare the all-studies estimate with the low-RoB-only
## estimate. Reporting both is good practice when the classification is
## debatable. (Here the shift is 9.9%, just inside the 10% inflation
## threshold, so the domain is still not rated down.)

g_conservative <- grade_meta(
  meta_obj          = m_response,
  study_design      = "RCT",
  rob               = rob_vec,
  rob_some_concerns = "high",                # <-- conservative classification
  rob_refit         = TRUE,                  # apply the "low RoB only" leaf if reached
  small_values      = "undesirable",
  threshold_type    = "mid",
  threshold         = 1.25,
  threshold_scale   = "ratio",
  outcome_type      = "relative",
  ois_p0 = 0.25, ois_p1 = 0.40,
  pubias_small_industry = "no",
  outcome_name      = "Depression response (rob_some_concerns = 'high')"
)
print(g_conservative)

cat("\nRoB domain note (conservative classification):\n")
d_cons <- g_conservative$domain_assessments
cat(d_cons$notes[d_cons$domain == "Risk of bias"], "\n")
cat("Analysis set actually used:", g_conservative$rob_analysis_set, "\n")


## ── 7. Multi-outcome GRADE table ─────────────────────────────────────────────
##
## Suppose we also have a second outcome (insomnia remission).
## Here we reuse the same data as a placeholder.

g_insomnia <- grade_meta(
  m_response,   # placeholder: in real use, fit a separate m_insomnia
  study_design           = "RCT",
  rob                    = rob_vec,
  rob_dominant_threshold = 0.60,
  ## Remission is a benefit outcome too: a low remission rate is bad, so small
  ## values are undesirable and a large OR is the favourable direction.
  small_values           = "undesirable",
  threshold_type         = "mid",
  threshold              = 1.25,
  threshold_scale        = "ratio",
  indirectness           = "no",
  inconsistency_ci_diff  = "yes",
  inconsistency_threshold_side = "majority_one_side",
  outcome_name           = "Insomnia remission"
)

## → Viewer ペインに多アウトカム GRADE テーブルが表示される
ft_multi <- grade_table(
  outcomes = list(
    "Depression response" = g_response,
    "Insomnia remission"  = g_insomnia
  ),
  primary = "Depression response"   # 1つ → "Primary outcome", 複数 → "Primary outcomes"
)
print(ft_multi)


## ── 8. Appendix GRADE report (Viewer プレビュー) ─────────────────────────────
##
## HTML を一時ファイルに出力し RStudio Viewer ペインで表示する（保存不要）。
## 原稿への組み込み時は末尾の 8b エクスポートコードを使用する。

out_html <- grade_report(
  outcomes = list(
    "Depression response" = g_response,
    "Insomnia remission"  = g_insomnia
  ),
  primary     = "Depression response",
  format      = "html",              # Viewer 表示用
  output_dir  = tempdir(),           # 一時ディレクトリ（起動ごとにクリア）
  output_file = "GRADE_appendix_preview"
)

## RStudio Viewer ペインで開く
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::viewer(out_html)
} else {
  utils::browseURL(out_html)         # RStudio 以外の環境ではブラウザで開く
}


## ── 8b. 原稿用エクスポート（必要時のみ実行） ─────────────────────────────────
## 論文 Appendix として添付する場合はこのブロックを実行:
##
## grade_report(
##   outcomes = list(
##     "Depression response" = g_response,
##     "Insomnia remission"  = g_insomnia
##   ),
##   primary     = "Depression response",
##   format      = c("docx", "md"),
##   output_dir  = file.path(here::here(), "outputs"),
##   output_file = "GRADE_appendix_I4D"
## )
## ## Output files:
## ##   output/GRADE_appendix_I4D.docx  — paste directly into manuscript Appendix
## ##   output/GRADE_appendix_I4D.md    — Markdown version


## ── 9. 複数アウトカム一括ワークフロー ────────────────────────────────────────
##
## セクション 7 は grade_meta() を手で 2 回呼んだ。アウトカムが増えるとこれは
## 破綻するので、`outcome` 列を持つ long データを 1 本用意し、取込 → アウトカム
## ごとの MA → アウトカムごとの GRADE → まとめ表 → Export を一気に通す。
##
##   run_ma_multi()     : outcome 列で分割し run_ma() をアウトカムごとに実行
##                        （run_ma() 自体は今も単一アウトカムしか受け付けない）
##   grade_meta_multi() : grade_meta() をアウトカムごとに実行し pmatools_set を返す
##   reorder_outcomes() : まとめ表の行順と Export のサブディレクトリ番号を決める
##   export_bundle()    : outcomes/NN_name/ 構成の ZIP を書き出す
##
## NOTE: 以下の "Insomnia remission" はセクション 7 と同じくプレースホルダで、
##       depression response のイベント数を流用した合成データ。

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

ma_list <- run_ma_multi(
  data_multi,
  sm         = "OR",            # 単一値 = 全アウトカム共通。名前付きリストで個別指定も可
  method.tau = "REML",
  incr       = 0.1
)

set <- grade_meta_multi(
  ma_list,
  ## 全アウトカム共通の引数
  common = list(
    study_design           = "RCT",
    threshold_type         = "mid",
    threshold              = 1.25,
    threshold_scale        = "ratio",
    small_values           = "undesirable",
    rob                    = rob_vec,
    indirectness           = "no",
    pubias_small_industry  = "no",
    follow_up              = "8-12 weeks"   # BMJ 様式の "Outcome and follow-up" 列
  ),
  ## そのアウトカムだけの引数（common を上書きする）
  per_outcome = list(
    "Depression response" = list(ois_p0 = 0.25, ois_p1 = 0.40),
    "Insomnia remission"  = list(ois_p0 = 0.18, ois_p1 = 0.30)
  ),
  primary = "Depression response"
)

## → アウトカムごとの certainty / rating target / 解析セットを一覧
print(set)

## 並び替え（まとめ表の行順と outcomes/NN_name/ の番号の両方に効く）
set <- reorder_outcomes(set, c("Depression response", "Insomnia remission"))

## → Viewer ペインに BMJ 様式のまとめ表が表示される
ft_set <- grade_table(set, style = "bmj")
print(ft_set)

## Export: 直下にまとめ表、outcomes/NN_name/ にアウトカム別の図表と results.txt
zip_path <- export_bundle(set, output_dir = tempdir(), bundle_name = "pmatools_multi")
cat("\nBundle written to:", zip_path, "\n")
print(zip::zip_list(zip_path)$filename)

## 原稿用に書き出す場合は output_dir を差し替える:
## export_bundle(set, output_dir = file.path(here::here(), "outputs"),
##               bundle_name = "GRADE_multi_I4D")
