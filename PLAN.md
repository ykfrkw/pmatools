# PLAN: pmatools パッケージ

## 概要

{meta} の meta オブジェクトから BMJ 2025 Core GRADE に準拠した確実性評価を行う R パッケージ。

**バージョン**: 0.1.0
**作成日**: 2026-03-16

---

## 参照文献

- Core GRADE 1 (BMJ 2025): Overview - PMID 40262844
- Core GRADE 3 (BMJ 2025): Inconsistency - PMID 40328467
- Core GRADE 4 (BMJ 2025): Risk of Bias, Publication Bias - PMID 40360206
- Core GRADE 5 (BMJ 2025): Indirectness - PMID 40393729

---

## 実装ステータス

- [x] パッケージ骨格（DESCRIPTION, NAMESPACE）
- [x] utils.R（共通ユーティリティ）
- [x] domain_rob.R（Risk of Bias 入力処理）
- [x] domain_indirectness.R（非直接性入力処理）
- [x] domain_inconsistency.R（非一貫性自動計算）
- [x] domain_imprecision.R（不精確性自動計算）
- [x] domain_pubias.R（出版バイアス自動計算）
- [x] grade_meta.R（メイン関数）
- [x] sof_table.R（SoF テーブル flextable 出力）
- [x] tests/testthat/test-grade_meta.R

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

## 今後の拡張（v0.2）

- Upgrade ドメイン（large effect, dose-response, plausible confounding）
- 複数アウトカムの一括処理（`grade_meta_list()`）
- GRADEpro CSV インポート・エクスポート
- R Markdown / Quarto テンプレート
