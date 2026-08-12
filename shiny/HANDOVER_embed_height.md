# 申し送り：埋め込み高さの自動調整（embed-height）

作成 2026-08-12。別セッションで続きを扱うための引き継ぎです。

---

## 1. いま本番はどうなっているか

| 項目 | 状態 |
|---|---|
| shinyapps.io | `feature/embed-height`（当時の `main` dd9ea4f + 2コミット）からデプロイ済み。**bundleId 12397692** |
| ブランチ | `feature/embed-height` = `1f5848d`（実装）→ `3defa88`（bundleId 記録）。**`main` へマージ済み**（`1b1e7f7`）。未 push（このリポジトリに origin は未設定） |
| WordPress | `/pmatools/`（post 1021）は受け口つきマークアップに差し替え済み。作業完了 |

実ページ `https://yukifurukawa.jp/pmatools/` で動作確認済みです。lazyload が `data-src` → `src` を差し替えてアプリが読み込まれ、`embed:height` 914 → 968 が届いて iframe が実高 968px に追従します。

---

## 2. 最優先の注意：feature/rsm-revision からデプロイすると消える

2026-08-12 に `main` へマージしたので（`1b1e7f7`）、`main` は解消済みです。残る穴は 1 本です。

```
main                  www/embed-height.js: あり   ← マージ済み（1b1e7f7）
feature/embed-height  www/embed-height.js: あり   ← 本番に出ているのはこれ
feature/rsm-revision  www/embed-height.js: なし   ← pmatools v0.4.0 vendor 作業中
```

`feature/rsm-revision` から `Rscript deploy.R` を打つと、**バンドルから `www/embed-height.js` が抜け、`app.R` の読み込み行も消えるため、本番の自動調整が黙って壊れます**（WordPress 側の受け口は残るので、iframe はフォールバック高さに戻るだけで、エラーは出ません。気づきにくい壊れ方です）。

v0.4.0 を出すときは、**先に `main` を `feature/rsm-revision` に取り込んでから**デプロイしてください。

`rsconnect/.../pairwise_meta_analysis.dcf` の bundleId も `main` 側は 12397692 に揃いましたが、共有チェックアウト（`feature/rsm-revision`）は 12350785 のままです。これも `main` を取り込めば揃います。

---

## 3. 何を変えたか

- **`www/embed-height.js`（新規）** … iframe で開かれているときだけ、中身の実高を `{ type: 'embed:height', height: <px> }` として親へ `postMessage` します。`window.pmaNotifyScrollTop` も公開します
- **`app.R`（+9行）** … `tags$head` で上記を読み込み、`scroll_top` ハンドラから親にも通知します。iframe が実高まで伸びると内部にスクロール余地が無くなり、既存の `window.scrollTo` が no-op になるためです

親ページ側（WordPress）の受け口は、ブログリポジトリの `apply_tool_embed_resize.py` が生成します。`origin` と `event.source` を検証してから反映し、高さは **cap 2800px** で頭打ちにします。

---

## 4. 踏んだ罠（同じ道を二度通らないために）

いずれもローカル実機で計測して初めて分かったものです。コード側にもコメントを残してあります。

1. **Shiny 1.13 の busy-indicator が `body` 直下に `<svg>` を挿す。** SVGElement は `offsetTop` / `offsetHeight` を持たないので `undefined + undefined` → `NaN` となり、子要素の最大値計算が壊れて `documentElement.scrollHeight` へフォールバックします。iframe 内の `scrollHeight` はビューポート高より小さくならないため、**「いまの iframe の高さ」を送り返す固定点にハマり、高さが frame 高に張り付きます**。数値でない子はスキップしてください
2. **`shiny:*` は jQuery イベントです。** `$(document).trigger()` で発火するため `document.addEventListener` には**一度も届きません**。実測でも native リスナは 0 回、jQuery 経由は 3 回でした。native に書き換えると Shiny 由来の再計測が全部黙って死にます
3. **`shiny:idle` 単独をゲートにすると早すぎます。** Shiny は出力を伴わない初回 flush でも idle を出すので、実測では t=968ms の 1 回目の idle 時点で中身はまだ骨組み 210px でした（実高 968px）。これを送ると親の枠が一度つぶれてから伸びます。`shiny:value` を見たあとの idle まで待ってください。保険として 5 秒タイマーも入れてあります
4. **rAF は使わないでください。** debounce に `requestAnimationFrame` を使うと、iframe がオフスクリーン／タブが非アクティブのときコールバックが走らず、`pending` フラグが立ちっぱなしで通知が止まります。`setTimeout` に統一済みです

---

## 5. 再検証の手順（ローカル）

```bash
cd ~/Developer/pairwise_meta_analysis
Rscript -e 'shiny::runApp(".", port = 7788, host = "127.0.0.1", launch.browser = FALSE)'
```

親ページのハーネスは、ブログリポジトリで生成できます。

```python
# メインブログ/ で実行。build_block の src をローカルに向けるだけ
import importlib.util
spec = importlib.util.spec_from_file_location("m", "apply_tool_embed_resize.py")
m = importlib.util.module_from_spec(spec); spec.loader.exec_module(m)
e = [x for x in m.CONFIG if x["post_id"] == 1021][0]
block = m.build_block(e, "http://127.0.0.1:7788/", [])
```

これを HTML に貼って別ポート（例 7789）から配信し、`window.addEventListener("message", ...)` で通知を記録します。同一オリジンで検証したい場合は、ハーネスを `www/` に置けば iframe の中を直接操作できます（**デプロイ前に必ず削除してください**）。

期待される観測値（内側幅 1232px）:

```
読み込み〜初回描画   通知なし（親はフォールバック 1160px を維持）
初回描画完了         914 → 968
サンプルデータ読込   1137 → 1960
Next（Step 2）       embed:scrolltop → 1623 → 1790
Step 3               2310 → 2347
幅 340px に縮小      2839 →（親が cap 2800 にクランプ）
```

---

## 6. WordPress 側（作業済み・こちらで持つ必要なし）

- post 1021 は `apply_tool_embed_resize.py` の CONFIG に登録済み。フォールバック高さ 1160 / 1500 / 1780 / 2170px、cap 2800px
- 戻す場合: `python3 apply_tool_embed_resize.py --posts 1021 --restore --apply`（変更前の 950px 固定版は `tool_embed_resize_before.json` にあります）
- 既存の他ツール7本は `embed:scrolltop` 受け口が無い旧マークアップのままです。揃えるなら `--force` ですが、**退避 JSON が現在の本文で上書きされる**ので、事前に `tool_embed_resize_before.json` をコピーしてください

---

## 7. 雑務

- このリポジトリに `.claude/worktrees/agent-*` が 3 つ残っています（今回の作業とは無関係）。不要なら `git worktree remove` してください
- デプロイは**共有チェックアウトからではなく、`main` 由来の専用 worktree を切って行ってください**。今回、作業中に別セッションが `feature/rsm-revision`（v0.4.0 vendor、R 18ファイル・+793行）へ切り替えており、気づかず打っていれば範囲外の変更が本番に出ていました
