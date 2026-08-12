# PLAN: pmatools パッケージ

## 概要

データ取込 → メタアナリシス（{meta}）→ GRADE 確実性評価（BMJ 2025 Core GRADE 準拠）→ SoF/Appendix → 再現性 ZIP までを一気通貫で行う R パッケージ。Shiny UI は別リポジトリ `pairwise_meta_analysis`（shinyapps.io 公開済み）。

**バージョン**: 0.5.0
**仕様書**: [SPEC.md](SPEC.md)
**変更履歴**: [NEWS.md](NEWS.md)
**使い方**: [README.md](README.md)
**作成日**: 2026-03-16（v0.1.0）／2026-05-01（v0.2.0）／2026-08-11（v0.5.0）

このファイルの役割は **実装ステータスとロードマップだけ**。API シグネチャ・ドメイン判定ロジック・Downgrade 表は [SPEC.md](SPEC.md) が唯一の正であり、ここには複製しない（複製がドリフトの原因になったため v0.5.0 で削除した）。

| 知りたいこと | 見るファイル |
|---|---|
| 公開 API のシグネチャ | SPEC.md §4 |
| ドメイン判定アルゴリズム・Downgrade 点数・確実性の算出 | SPEC.md §5（特に §5.0 判定語彙） |
| データ形式（long / wide / `outcome` 列） | SPEC.md §3 |
| 使い方・コード例 | README.md、`sample.R` |
| リリースごとの変更点 | NEWS.md |

---

## 参照文献

- Core GRADE 1 (BMJ 2025): Overview - PMID 40262844
- Core GRADE 2 (BMJ 2025): Choosing the target of certainty rating and assessing imprecision - doi:10.1136/bmj-2024-081904
- Core GRADE 3 (BMJ 2025): Inconsistency - PMID 40328467
- Core GRADE 4 (BMJ 2025): Risk of Bias, Publication Bias - PMID 40360206
- Core GRADE 5 (BMJ 2025): Indirectness - PMID 40393729

---

## 実装ステータス（v0.5.0）

### v0.1.0 既存
- [x] パッケージ骨格（DESCRIPTION, NAMESPACE）
- [x] utils.R（共通ユーティリティ）
- [x] domain_rob.R（Risk of Bias 入力処理）
- [x] domain_indirectness.R（非直接性入力処理）
- [x] domain_inconsistency.R（非一貫性自動計算）
- [x] domain_imprecision.R（不精確性自動計算）
- [x] domain_pubias.R（出版バイアス自動計算）
- [x] grade_meta.R（メイン関数）
- [x] sof_table.R / grade_table.R / grade_report.R
- [x] tests/testthat/test-grade_meta.R

### v0.2.0 追加
- [x] data_ingest.R（long/wide 両対応 + alias mapping）
- [x] run_ma.R（{meta} ラッパー、binary/continuous）
- [x] plot_forest.R（auto_layout, log/linear 自動）
- [x] plot_funnel.R（contour + Egger 注釈）
- [x] export_bundle.R + inst/templates/analysis_script.R.tpl（ZIP 一括出力）
- [x] utils.R 拡張（chinn_smd_to_or, suggest_threshold, compute_pooled_sd, threshold_to_te_scale）
- [x] grade_meta() に rob_inflation_threshold, threshold, threshold_scale 引数
- [x] sof_table() に convert_smd_to_or, baseline_risk, threshold_label
- [x] domain_rob.R: inflation 閾値（既定 10%）+ small_values=NULL の |TE| ロジック
- [x] domain_inconsistency.R: Step 2 で Threshold-3-zone（auto）、Q-test 駆動撤去
- [x] domain_imprecision.R: Threshold から ois_p1/ois_delta 自動派生
- [x] tests: test-data_ingest, test-run_ma, test-domain_rob, test-inconsistency_threshold, test-chinn, test-export_bundle

### v0.3.x 追加
- [x] ドメイン判定語彙を 3 段階（`no` / `some_concerns` / `serious`）に統一（SPEC.md §5.0）
- [x] domain_rob.R / domain_pubias.R を Core GRADE 4 準拠に再構築（Fig 5 出版バイアスフローチャート含む）
- [x] rare_events.R（`run_rare_ma()`, `rare_event_diagnostics()`, `plot_rare_sensitivity_forest()`）
- [x] 追加プロット（`plot_trimfill_forest()`, `plot_forest_rob()`, `plot_forest_indirectness()`, `plot_forest_pubias_subgroup()`）
- [x] evidence_profile.R（単一アウトカムの Evidence Profile 表）
- [x] domain_imprecision.R リファクタ（CI-vs-Threshold 判定の修正、MID → Threshold へ用語統一、観測イベント数の併記）
- [x] data_ingest.R の列名エイリアス拡張、`outcome` 列を持つデータの行保持
- [x] export_bundle.R に rare-event diagnostics と trim-and-fill を同梱

### v0.4.0 追加
- [x] 手動オーバーライドに rationale 必須化（`rob_rationale` ほか、**breaking**）
- [x] SoF 用語を GRADEpro に整合（"Risk with &lt;control&gt;" / "Risk with &lt;intervention&gt;"、**breaking**）
- [x] 絶対スケール（ARD）の意思決定 Threshold：`threshold_scale = "ard"` + `threshold_baseline`
- [x] `imprecision` スカラーオーバーライド（自動判定を完全にバイパス）
- [x] RoB direction-gate の透明化（ドメイン注記に判断理由を明記）
- [x] forest plot の下部余白を動的化（重なり解消）
- [x] Core GRADE series としてのリブランディング（関数名・引数名は不変）

### v0.5.0 追加
- [x] Core GRADE 2 エントリゲート：`threshold_type`（既定 `"mid"`）が MID を必須化（**breaking**、condition class `"pmatools_threshold_gate"`）／`require_threshold = FALSE` が退避路
- [x] Rating target（Core GRADE 2 Fig 2）：`$rating_target` / `$rating_target_note` / `$rating_target_auto` を点推定値から自動導出
- [x] Imprecision を Core GRADE 2 Fig 4 フローチャートに準拠（**breaking**、OIS は CI が Threshold をまたがない場合のみ参照）
- [x] Risk of bias を Core GRADE 4 Fig 2 に準拠（**breaking**）。weight-share dominance gate を復活（`rob_dominant_threshold`、既定 0.60）
- [x] low-RoB サブセットでの再フィット（`rob_refit`、既定 TRUE）＋ `$meta_full` / `$rob_analysis_set`
- [x] `rob_some_concerns` / `rob_overrides` / `rob_override_rationale`
- [x] Indirectness サブドメイン（Core GRADE 5）：`indirectness_subdomains`（PICO × 4 択）＋ `indirectness_table()`
- [x] 複数アウトカム一括ワークフロー：`run_ma_multi()` / `grade_meta_multi()` / `pmatools_set` / `reorder_outcomes()` / `set_primary()`
- [x] `export_bundle()` を S3 generic 化（第 1 引数 `x`、**breaking**）＋ `outcomes/NN_name/` 階層レイアウト
- [x] BMJ 様式 SoF：`sof_table()` / `grade_table()` / `grade_report()` の `style = "bmj"`（GRADEpro 様式が既定のまま）
- [x] Plain language summary（Core GRADE 2 Table 1 の文言を逐語採用）
- [x] `inst/templates/analysis_script_multi.R.tpl`（複数アウトカム版 analysis.R、書き出し前に `parse()` で構文検査）
- [x] tests: 936 pass / 0 fail、`R CMD check` 0 errors / 0 warnings / 0 notes

---

## 今後の拡張（v0.6+）

SPEC.md §11「Out of scope」と同期していること。

- Upgrade ドメイン（large effect, dose-response, plausible confounding）
- GRADEpro GDT 連携（JSON インポート・エクスポート）
- 多言語対応（日本語ラベル）— 現状 `sof_table()` などに言語切替引数は無い
- CRAN 提出
- Shiny 側（`pairwise_meta_analysis`）へ複数アウトカムワークフローを配線する UI 作業 — パッケージ側は完了済み、追従はアプリ側の SPEC で管理
- shinyapps.io 公開ガイド（`pairwise_meta_analysis` 側）
