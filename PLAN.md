# PLAN: pmatools パッケージ

## 概要

データ取込 → メタアナリシス（{meta}）→ GRADE 確実性評価（BMJ 2025 Core GRADE 準拠）→ SoF/Appendix → 再現性 ZIP までを一気通貫で行う R パッケージ。Shiny UI は同一リポジトリの `shiny/` にあり、shinyapps.io に公開済み（https://yuki-furukawa.shinyapps.io/pmatools/）。

**バージョン**: 0.5.1
**仕様書**: [SPEC.md](SPEC.md)
**変更履歴**: [NEWS.md](NEWS.md)
**使い方**: [README.md](README.md)
**作成日**: 2026-03-16（v0.1.0）／2026-05-01（v0.2.0）／2026-08-11（v0.5.0）／2026-08-12（v0.5.1）

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

## 実装ステータス（v0.5.1）

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
- [x] Risk of bias を Core GRADE 4 Fig 2 に準拠（**breaking**）。weight-share dominance gate を復活（`rob_dominant_threshold`、既定 0.55 — Fig 2 脚注が挙げる 2 候補のうち保守的な方。0.60 はどちらとも一致しないため 0.5.0 内で訂正済み）
- [x] low-RoB サブセットでの再フィット（`rob_refit`、既定 TRUE）＋ `$meta_full` / `$rob_analysis_set`
- [x] `rob_some_concerns` / `rob_overrides` / `rob_override_rationale`
- [x] Indirectness サブドメイン（Core GRADE 5）：`indirectness_subdomains`（PICO × 4 択）＋ `indirectness_table()`
- [x] 複数アウトカム一括ワークフロー：`run_ma_multi()` / `grade_meta_multi()` / `pmatools_set` / `reorder_outcomes()` / `set_primary()`
- [x] `export_bundle()` を S3 generic 化（第 1 引数 `x`、**breaking**）＋ `outcomes/NN_name/` 階層レイアウト
- [x] BMJ 様式 SoF：`sof_table()` / `grade_table()` / `grade_report()` の `style = "bmj"`（GRADEpro 様式が既定のまま）
- [x] Plain language summary（**Core GRADE 6 Box 1** の文言を逐語採用。当初は Core GRADE 2 Table 1 を典拠としていたが、Box 1 がそれを包含する正典であり、方向（reduces / increases）を明示する点が異なる）
- [x] `inst/templates/analysis_script_multi.R.tpl`（複数アウトカム版 analysis.R、書き出し前に `parse()` で構文検査）
- [x] tests: 936 pass / 0 fail、`R CMD check` 0 errors / 0 warnings / 0 notes

### v0.5.1 追加

典拠は [NEWS.md](NEWS.md) の `# pmatools 0.5.1 (development version)` 節。

**新機能**

- [x] 構造化ドメインファクト：`domain_facts(x, domain = NULL)` を export、`$domain_facts` に格納（`key` / `label` / `value` / `numeric`）。Risk of bias・Inconsistency・Imprecision が記録し、Indirectness・Publication bias は当面 prose のみ。`domain_assessments$notes` は 1 バイトも変えていない（ファクトは prose の機械可読な**併走物**）（SPEC §4.15 / §5.6）
- [x] ドメイン別 rate-down 脚注：`sof_table()` / `grade_table()`（gradepro・bmj 両方）／`evidence_profile()` が上記ファクトを番号付き脚注として certainty セルに出力。`grade_table()` では analysis-set 脚注と同じ `[n]` 連番を共有し、どのアウトカムの脚注かを明記
- [x] 報告されなかったアウトカム：`not_reported_outcome()` / `add_not_reported()` を export。どの included study も報告しなかった prespecified outcome を SoF 表の 1 行として持てる（Core GRADE 6）。certainty セルは空欄ではなく `"Not rated"`。`grade_table()` / `grade_report()` / `export_bundle()` は受け入れ、`sof_table()` / `evidence_profile()` は理由を述べて拒否。クラス `pmatools_not_reported` は意図的に `"pmatools"` を継承しない（SPEC §4.14）
- [x] 連続アウトカムの arm-level 列：両 SoF レイアウトが対照群セル（対照アームの逆分散加重平均）と介入群セルを埋めるようになった。SMD は対照アームの pooled within-arm SD（Cochrane Handbook 15.5.3.2）を掛けてから加算。導出は脚注化、binary 表は不変（SPEC §4.6）
- [x] `export_bundle()` の両メソッドに `style` 引数（既定は `"bmj"` に変更、**behaviour change**）、`export_bundle.meta()` に `follow_up` / `unit`、両メソッドに `sof_notes`。いずれも `analysis.R` にレンダリングされる
- [x] `sof_add_notes(x, notes)` を export（呼び出し側の脚注行を SoF flextable に追記）

**バグ修正（抜粋）**

- [x] `.total_n()` の名前衝突（`domain_imprecision.R` 側を `.total_n_strict()` にリネーム）
- [x] `export_bundle()` の `grade_args` / `ma_args` 参照を `$` の部分一致から厳密な `[[` へ。`grade_args` の名前は `grade_meta()` の formals と照合
- [x] 単一アウトカム `analysis.R` テンプレートに `threshold_baseline` スロットを追加
- [x] baseline risk：`event.c` / `n.c` を同一の complete-case フィルタに統一
- [x] `suggest_threshold()` が `sm = "RD"` を扱えるように
- [x] Risk of bias の k-space / studlab-space マッピングを明示化。refit と `rob_overrides` が {meta} の drop 発生時にも動く。v0.5.1 中に `.rob_alignment()` / `.rob_expand()` / `.rob_contract()` へ切り出し（純粋なリファクタ、外部挙動は不変）（SPEC §5.1）
- [x] `results.txt` の pooled 推定値の見出しが解析セット名を含むように（low-RoB refit 時）。呼び出し側が all-studies オブジェクトを渡した場合は rated 側を第 2 ブロックとして併記（SPEC §4.8.2）

**ドキュメント / 運用**

- [x] `CLAUDE.md` 新設（ベンダリングのライフライン + docs は変更と同じ PR で更新するルール）
- [x] SPEC.md の `Version target:` を 0.5.1 に同期（CLAUDE.md ルール 2）

---

## 今後の拡張（v0.6+）

SPEC.md §11「Out of scope」と同期していること。

- Upgrade ドメイン（large effect, dose-response, plausible confounding）
- GRADEpro GDT 連携（JSON インポート・エクスポート）
- 多言語対応（日本語ラベル）— 現状 `sof_table()` などに言語切替引数は無い
- CRAN 提出
- Shiny 側（`shiny/`）へ複数アウトカムワークフローを配線する UI 作業 — パッケージ側は完了済み、追従は [shiny/SPEC.md](shiny/SPEC.md) で管理
- shinyapps.io 公開ガイド（`shiny/deploy.R` と CLAUDE.md §3 の運用を文書化する）

---

## 未着手フィードバック（2026-08-13 受領、実装前のメモ）

ユーザーから GitHub の README/ドキュメントに対して受領。**まだ実装していない。**
着手時はこの節を上から潰し、済んだ項目は削除する。

### A. ドキュメント表記（低リスク・すぐ直せる）

1. ~~**companion repository の記述を削除**~~ — 完了。README.md 冒頭の
   companion repository 行を削除し、SPEC.md 冒頭注記・「Shiny-agnostic」段落・
   §11 の out-of-scope 行、PLAN.md:5 と「今後の拡張」2 行を `shiny/` 前提に
   書き換えた。`shiny/SPEC.md` に残る `pairwise_meta_analysis` 参照は別件
   （下記 9 を見よ）。

2. ~~**"MIC" 表記をやめ `threshold` に統一**~~ — 実装済み（下の「関連」節を見よ）。

3. ~~**`Mortality` はサンプルデータに無い**~~ — 完了。README の
   「Several outcomes in one session」を `inst/extdata/cbti_depression.csv` から
   Depression response / remission / severity の 3 アウトカムを組み立てる例に
   差し替え、実行して出力を確認した（binary 2 + continuous 1 の混在例を兼ねる）。
   「Multi-outcome workflow」節の worked example も同じデータに統一し、捏造の
   `* 0.7` プレースホルダを撤去。ついでに存在しないファイルを指していた
   `events_long.csv` を同梱の `rare_events_mock.csv` に、ZIP 内容の記述
   （PNG は生成されない・Appendix docx は入らない）を実測に合わせた。

4. ~~**Shiny アプリ側の説明を README に入れる**~~ — 完了。README の Installation
   直前に「The Shiny app — how most people use pmatools」節を追加（位置づけ・
   公開 URL・埋め込み先・ローカル起動手順・4 ステップ・アプリ版と
   パッケージ版が別管理である理由・`shiny/SPEC.md` へのリンク）。

### B. 設計・仕様に踏み込む項目（要検討）

5. **`threshold_baseline` と `ois_p0` が両方あるのは何故か** — README.md:60-70 の
   `g_abs` 例で同じ 0.25 を 2 回渡していて冗長に見える。現状の役割:
   - `threshold_baseline`: `threshold_scale = "ard"` のとき、絶対リスク差の閾値を
     効果尺度（RR/OR）のスケールへ換算するための対照群リスク。未指定ならプール
     対照群イベント率（[R/utils.R:552](R/utils.R:552) 付近）。
   - `ois_p0`: OIS（optimal information size）のサンプルサイズ計算に使う対照群
     イベント率。
   - `baseline_risk`（SoF 表用）は「未指定なら `ois_p0` を使う」というフォール
     バックを既に持つ（README.md:1682）。つまり 3 つの引数が同じ量を指している。
   - 対応方針の候補: (a) 3 者を単一の `baseline_risk` に集約し、旧引数は
     deprecate、(b) 少なくとも `ois_p0` / `threshold_baseline` の相互フォール
     バックを実装し README でそう書く。どちらも API 変更なので `feat!:` 扱い、
     NEWS.md にエントリが要る。

6. **Downgrade scale を Core GRADE 用語に合わせる** — [README.md:184-209](README.md:184)
   の対応表で pmatools の値が Core GRADE 文言と 1 段ズレている
   （`"some_concerns"` = serious、`"serious"` = very serious）。
   - Core GRADE の語彙（not serious / serious / very serious / extremely
     serious）に合わせて値そのものを改名する。
   - **−3（extremely serious）は自動判定はしないが、手動指定は可能にする。**
     現状「単一ドメインの最大 downgrade は −2」とハードコードされているので、
     手動 `rob = "extremely_serious"` 等の経路を通す必要がある。
   - 破壊的変更。既存値は legacy alias として受け付けて正規化する（既に
     `"some"` → `"some_concerns"` の前例あり）。SPEC.md を先に直す。

7. **RoB のルールが分かりづらい** — README.md:440-530 のフローチャート説明・
   overrides・alias 表・`rob_strata()` が一続きで読みづらい。判定ルールを
   1 枚の表か図に集約して整理し直す。

8. **RoB2 のマッピング表が誤り** — [README.md:463-472](README.md:463) の
   「Cochrane RoB 2.0 → Internal GRADE level」表が 4 段階
   （No concerns / Some concerns / Serious concerns / Critical concerns）に
   なっているが、**RoB 2 の判定は 3 段階**: low risk of bias / some concerns /
   high risk of bias。"Serious concerns" / "Critical concerns" は ROBINS-I の
   語彙。表を RoB 2 の 3 段階に直し、ROBINS-I を併記するなら別表に分ける。
   [README.md:475](README.md:475)・README.md:1161 の `rob_map` 例、および
   `rob_strata()` の alias 表（R 側実装）も同じ誤りを持っていないか確認する。

9. ~~**`shiny/SPEC.md` がリポジトリ統合前のまま**~~ — 完了（2026-08-14）。
   §2.1・§2.2・§7・§9 を `stage_bundle.R` / `deploy.R` の実装に突き合わせて
   書き直した。危険だった `Remotes: github::ykfrkw/pmatools` +
   `install_github()` の記述は削除し、代わりに「その 2 つを足すと 401 が戻る」
   と明記。§2.1 はアプリ `DESCRIPTION` が rsconnect 用マニフェストである
   ことと依存追加ルール、§2.2 は実際のファイルレイアウト（生成物の
   `R/_pmatools/` / `_pmatools_inst/` と 2 つの deployment record を含む）、
   §7 は staging 5 ステップ・`--check-only`・`deploy.R` の 5 ステップ・
   `APP_NAME` を変えても rename にならない件、§9 は 2 つのバージョンと
   最小 pmatools 制約が存在しない理由。冒頭の「ここから下は古い」警告は
   撤去した。細目は CLAUDE.md §1 を再掲せずリンクしている。

10. **README のサンプル出力が実装とドリフトしている**（同じく 2026-08-14 に発見。
    コードは動くが、README に貼られた出力例が古い）。
    - `suggest_threshold(ma)` の出力例が `$threshold_ratio 1.20` になっているが、
      `sm = "OR"` の実測は 1.25（直前の表の記載 OR → 1.25 とも矛盾）。
    - `domain_facts()` の例が `# A tibble: 6 x 4` かつ 3 ドメインだが、実測は
      Imprecision が 8 行（`ois_target_rate` / `flow_path` が増えている）、
      ドメインは 4 つ（Publication bias も記録するようになっている）。
      その直後の「Indirectness, Publication bias | none yet」表も同様に古い。
    ドキュメントを実装に合わせるのか実装側の追加が意図せぬものだったのかの
    判断が要るので、確認せずに書き換えていない。

### 関連: Shiny アプリの UI/UX レビュー

上記フィードバックのうち UI に現れる分は、アプリ実機を通したレビューと
まとめて [shiny/UX_REVIEW.md](shiny/UX_REVIEW.md) に整理してある。

**2026-08-13 時点で、そこに挙がった UI 側の作業は 9 フェーズとして main に
入っている。** この節の項目 2（MIC 表記）と項目 7（RoB のルールが分かりづらい）は
実装済み。残っているのは:

- ~~**項目 1・3・4**~~ — 2026-08-14 に完了（README の companion repository 記述、
  サンプルデータに無い `Mortality` の例、アプリ説明の追加）。テストは
  パッケージ 2099 pass / アプリ 747 pass のまま、どちらも変化なし
- **項目 5** — `threshold_baseline` と `ois_p0` の重複。パッケージと README の
  作業で、UI 側の実装を伴わない
- **項目 6**（downgrade 語彙の Core GRADE 準拠と手動 −3）— UX_REVIEW.md の
  当初見積もりが誤っていた。アプリ側の小さな diff ではなく、パッケージに
  **第 4 の judgment レベルを新設**する必要がある（現状 no / some_concerns /
  serious の 3 値のみ）。この節の改名作業と同時にやるのが自然
- **項目 8**（RoB2 の 3 段階）— 表の誤りは未修正。ただし調査の結果、アプリは
  `study_design = "RCT"` をハードコードしていて観察研究の経路が無いので、
  ROBINS-I との併記は不要で RoB 2 の 3 値だけで足りる

### 実装順の目安

~~A（1〜4）は独立・低リスクなので先に片付く。~~ A は完了。残りの B は
6 → 8 → 7 → 5 の順が安全（6 と 8 は語彙の定義そのもの、7 はその上での再構成、
5 は別軸の API 整理）。9 と 10 は B と独立なのでいつでもよいが、9 は誤った
deploy 手順を書き残している分だけ急ぐ理由がある。
どれも CLAUDE.md §5 に従い SPEC.md / NEWS.md を同じ PR で更新する。
