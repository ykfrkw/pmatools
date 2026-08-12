# CLAUDE.md — pairwise_meta_analysis

このリポジトリで作業する Claude セッション向けの規律。

このアプリは pmatools を**インストールせず vendoring** している（`R/_pmatools/`
+ `_pmatools_inst/`）。ソースが複数のチェックアウトにまたがるぶん、隔離が破れる
と「別セッションの作業中コードが shinyapps.io に出荷される」まで一直線につながる。
以下はその経路を塞ぐためのルール。

## 1. 実装は worktree 隔離したサブエージェントに委譲する

- 実装タスクを Agent ツールに投げるときは **必ず `isolation: "worktree"` を付ける**。
  付けないと共有チェックアウトの index / staged を汚し、deploy に混入する。
- 並行して走らせる独立タスクは、それぞれ別 worktree に隔離する。

## 2. worktree の外に書き込まない

- worktree 内から main チェックアウト
  (`~/Developer/pairwise_meta_analysis`) の**絶対パスに書き込まない**。
- 編集はすべて cwd 相対で行う。main 側のファイルが必要なときは
  **読み取りコピーのみ**（`cp <main>/file ./file`）。
- 作業終了時、main チェックアウトの `git status --porcelain` が着手時と
  同一であることを確認する。

## 3. vendored pmatools の更新

- `~/Developer/pmatools` の**共有チェックアウトを直接編集しない**。pmatools 側にも
  worktree を切り、そこを指して vendor する:

  ```bash
  git -C ~/Developer/pmatools worktree add ../pmatools-wt/<branch> <branch>
  PMATOOLS_SRC=~/Developer/pmatools-wt/<branch> Rscript update_vendor.R
  ```

- `PMATOOLS_SRC` 未設定時は共有チェックアウト
  (`~/Developer/pmatools`) が既定。**共有チェックアウトが dirty なら
  `update_vendor.R` は止まる** — 別セッションの作業中コードを vendor して
  出荷しないため。`PMATOOLS_ALLOW_DIRTY=1` で警告に降格できるが、
  そのときは `R/_pmatools/VERSION` の 2 行目に `-dirty` が刻まれる。
  止められたら、まず「なぜ dirty なのか」を確認する。フラグで踏み潰さない。
- 書き換えずに現状確認だけしたいときは `Rscript update_vendor.R --check-only`
  （読み取り専用。dirty でも止まらず、状態を表示するだけ）。

## 4. 生成物を手編集しない

- `R/_pmatools/` と `_pmatools_inst/` は **`update_vendor.R` の生成物**。
  手で直しても次の vendor で消える。直すなら upstream pmatools 側を直して
  vendor し直す。
- `R/_pmatools/VERSION` も生成物。1 行目 = version 文字列（`app.R` が
  `readLines(n = 1L)` で読む）、2 行目 = `source: <branch>@<sha>`。
  1 行目のセマンティクスを変えない。

## 5. デプロイ

- **deploy は clean な main チェックアウトからのみ。**
- `deploy.R` は cwd のワーキングツリーを**そのまま出荷する**。worktree や
  dirty な状態から実行すると、意図しない中身がそのまま shinyapps.io に乗る。
- デプロイ前に **HEAD SHA とクリーン性を記録する**:

  ```bash
  git rev-parse --short HEAD
  git status --porcelain   # 空であること
  ```

- 記録した SHA は deploy 後のコミットメッセージに残す。
