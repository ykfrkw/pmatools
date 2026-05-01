# PLAN: pmatools パッケージ

## 概要

データ取込 → メタアナリシス（{meta}）→ GRADE 確実性評価（BMJ 2025 Core GRADE 準拠）→ SoF/Appendix → 再現性 ZIP までを一気通貫で行う R パッケージ。Shiny UI は別リポジトリ `pairwise_meta_analysis`（shinyapps.io 公開済み）。

**バージョン**: 0.2.0
**仕様書**: [SPEC.md](SPEC.md)
**作成日**: 2026-03-16（v0.1.0）／2026-05-01（v0.2.0）

---

## 参照文献

- Core GRADE 1 (BMJ 2025): Overview - PMID 40262844
- Core GRADE 3 (BMJ 2025): Inconsistency - PMID 40328467
- Core GRADE 4 (BMJ 2025): Risk of Bias, Publication Bias - PMID 40360206
- Core GRADE 5 (BMJ 2025): Indirectness - PMID 40393729

---

## 実装ステータス（v0.2.0）

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
- [x] utils.R 拡張（chinn_smd_to_or, suggest_mid, compute_pooled_sd, mid_to_te_scale）
- [x] grade_meta() に rob_inflation_threshold, mid, mid_scale 引数
- [x] sof_table() に convert_smd_to_or, baseline_risk, threshold_label
- [x] domain_rob.R: inflation 閾値（既定 10%）+ small_values=NULL の |TE| ロジック
- [x] domain_inconsistency.R: Step 2 で MID-3-zone（auto）、Q-test 駆動撤去
- [x] domain_imprecision.R: MID から ois_p1/ois_delta 自動派生
- [x] tests: test-data_ingest, test-run_ma, test-domain_rob, test-inconsistency_mid, test-chinn, test-export_bundle

---

## API 設計

```r
grade_meta(
  meta_obj,
  study_design = "RCT",       # "RCT" | "obs"
  rob = NULL,                  # "no" | "some" | "serious" | "very_serious"
                               #   or 列名 (chr) or 長さkベクタ
  indirectness = "no",         # 同上
  outcome_name = NULL,         # アウトカム名
  outcome_type = "relative",   # "relative" | "absolute"
  ois_events = NULL,           # バイナリ OIS イベント数
  ois_n = NULL                 # 連続 OIS サンプル数
)

sof_table(pmatools_obj, language = "en")  # "en" | "ja"
```

---

## GRADE 確実性計算ロジック

### 開始確実性
- RCT → High (4)
- 観察研究 → Low (2)

### Downgrade
| 判定 | 点数 |
|------|------|
| no concern | 0 |
| some concern | -1 |
| serious | -1 |
| very serious | -2 |

### 最終確実性
| 点数 | 確実性 |
|------|--------|
| 4 | High |
| 3 | Moderate |
| 2 | Low |
| ≤1 | Very Low |

---

## 今後の拡張（v0.3+）

- Upgrade ドメイン（large effect, dose-response, plausible confounding）
- 複数アウトカムの一括処理（`grade_meta_list()`）
- GRADEpro JSON インポート・エクスポート
- 多言語対応（日本語ラベル）
- shinyapps.io 公開ガイド（`pairwise_meta_analysis` 側）
