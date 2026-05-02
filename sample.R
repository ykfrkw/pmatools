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
## [Risk of Bias — BMJ Core GRADE 4, Fig 2]
##
##   small_values = "undesirable": large OR = more response = desirable.
##   Domination check (threshold 60%):
##     6/17 studies are Serious-RoB, but Henry2020 (n=3,352, "Some concerns")
##     dominates the weight. High-RoB weight ≈ 38% < 60% → NOT dominated.
##   → Do not rate down.
##
##   If dominated, the package automatically compares:
##     TE(all studies) vs TE(excl. high-RoB)
##     small_values = "undesirable": TE_all > TE_low → inflates → rate down
##
## [Indirectness — BMJ Core GRADE 5]
##   Population/intervention/comparator/outcome are directly applicable.
##   → "no" concern.
##
## [Inconsistency — BMJ Core GRADE 3, Fig 2]
##   I² ≈ 36%, tau² ≈ 0.17, Q p ≈ 0.07.
##   Note: point estimates are mostly in the same direction (OR > 1), but
##   CIs vary in width. There are important CI differences → ci_diff = "yes".
##   Most estimates are on one side of the threshold (OR > 1) → majority_one_side.
##   → Do not rate down.
##   (I² stats are supplementary and noted in the output.)
##
## [Imprecision — BMJ Core GRADE 4]
##   OR 2.30 [1.64, 3.23] does not cross null (log OR = 0).
##   OIS: using baseline response rate ~25% and expected experimental ~40%,
##        power 80%, α = 0.05 → auto-calculated target events.
##
## [Publication Bias — BMJ Core GRADE 4, Fig 5]
##   Studies are not predominantly small and industry-sponsored (k=17, many
##   academic trials). k ≥ 10 → Egger's test auto-computed.

g_response <- grade_meta(
  meta_obj               = m_response,
  study_design           = "RCT",

  ## Risk of Bias
  rob                    = rob_vec,          # per-study Cochrane RoB2 labels → flowchart
  rob_dominant_threshold = 0.60,             # default: >60% weight = dominated
  small_values           = "undesirable",    # large OR = desirable (response outcome)

  ## Indirectness
  indirectness           = "no",

  ## Inconsistency (BMJ Core GRADE 3 flowchart — auto-computed when NULL)
  ## Leave NULL → auto-detects from I², tau², Q p, and per-study TE direction
  ## Override manually if needed:
  ## inconsistency_ci_diff        = "yes"
  ## inconsistency_threshold_side = "majority_one_side"

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
## Expected: Moderate (High − inconsistency no − imprecision no = High,
##   unless OIS is not met)


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
## Method 1: explicit numeric (e.g., from published baseline event rate)
g_br_explicit <- grade_meta(
  meta_obj     = m_response, study_design = "RCT",
  rob          = rob_vec, outcome_favors = "high",
  outcome_type = "relative",
  baseline_risk = 0.25,           # <-- direct specification
  outcome_name = "Depression response (baseline_risk = 0.25)"
)
sof_table(g_br_explicit)

## Method 2: simple pooled control-arm proportion
g_br_simple <- grade_meta(
  meta_obj     = m_response, study_design = "RCT",
  rob          = rob_vec, outcome_favors = "high",
  outcome_type = "relative",
  baseline_risk = "simple",       # <-- sum(events_c) / sum(n_c)
  outcome_name = "Depression response (simple)"
)
sof_table(g_br_simple)
cat("Simple pooled baseline risk:", round(g_br_simple$baseline_risk, 3), "\n")

## Method 3: GLMM-pooled via metaprop (logit back-transform)
g_br_metaprop <- grade_meta(
  meta_obj     = m_response, study_design = "RCT",
  rob          = rob_vec, outcome_favors = "high",
  outcome_type = "relative",
  baseline_risk = "metaprop",     # <-- meta::metaprop() GLMM
  outcome_name = "Depression response (metaprop)"
)
sof_table(g_br_metaprop)
cat("metaprop baseline risk:", round(g_br_metaprop$baseline_risk, 3), "\n")


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
  outcome_favors         = "high",
  indirectness           = "no",
  inconsistency_ci_diff  = "yes",
  inconsistency_threshold_side = "majority_one_side",
  ois_p0 = 0.25, ois_p1 = 0.40,
  outcome_name = "Depression response (sensitivity: low/some RoB only)"
)
print(g_sens)


## ── 7. Multi-outcome GRADE table ─────────────────────────────────────────────
##
## Suppose we also have a second outcome (insomnia remission).
## Here we reuse the same data as a placeholder.

g_insomnia <- grade_meta(
  m_response,   # placeholder: in real use, fit a separate m_insomnia
  study_design           = "RCT",
  rob                    = rob_vec,
  rob_dominant_threshold = 0.60,
  outcome_favors         = "high",
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
