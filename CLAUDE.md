# pmatools — repo rules

## 0. Layout

One repo, two artifacts:

- **the R package**, at the root (`R/`, `man/`, `tests/`, `inst/`, `DESCRIPTION`);
- **the Shiny app**, under `shiny/`, deployed to shinyapps.io.

`shiny/` is in `.Rbuildignore`, so it never reaches the package tarball. The app is
not part of the package and `shiny` is deliberately absent from `DESCRIPTION`.

Two `DESCRIPTION` files, and only one of them is a package:

| file | what it is |
|---|---|
| `DESCRIPTION` | the package. The real one. `Version:` here is *the* version. |
| `shiny/DESCRIPTION` | an **rsconnect dependency manifest**. `Type: Project`, never installed. Its `Version:` is the app's own and tracks separately; rsconnect ignores the field. |

Two `SPEC.md` files: the root one describes the package, `shiny/SPEC.md` the app. A
change updates whichever describes what it changed — sometimes both.

Two test suites, deliberately not merged (they load code in incompatible ways —
namespace vs `source()`):

```bash
Rscript -e 'devtools::test()'      # package
Rscript shiny/tests/testthat.R     # app
```

## 1. The deploy lifeline (MUST)

The app **sources** pmatools rather than installing it: a stale `GITHUB_PAT` cached on
the shinyapps.io account makes `install_github(ykfrkw/pmatools)` return HTTP 401 on the
build server. So the bundle carries the package sources itself, staged from `../R` and
`../inst` by `shiny/stage_bundle.R`, and `app.R` `source()`s them.

Merging the repos removed the re-vendoring gate — `stage_bundle.R` reads the same
`DESCRIPTION` that defines the package version, so the app can no longer run sources
older than the package. **These four survived it. Do not assume otherwise.**

- **Adding a package to `DESCRIPTION` `Imports`/`Suggests` means adding it to
  `shiny/DESCRIPTION` `Imports` too.** rsconnect reads only the latter; without it
  shinyapps.io does not install the package and the feature fails at runtime, in
  production. This is the rule most likely to look obsolete in a merged repo and it is
  not. `metafor`, `mmeta` and `BiasedUrn` are the standing example: `rare_events.R`
  names them only as *strings*, so rsconnect's static scan cannot see them at all.
  Audit with `Rscript shiny/stage_bundle.R --check-only`; `deploy.R` fails on it.
- **NEVER rely on `utils::packageVersion("pmatools")` at runtime.** It errors under
  `source()`. Call `.pmatools_version()` in `R/house_style.R`, which falls back to
  `.vendored_version_stamp()` / `getOption("pmatools.version_stamp")`.
- **NEVER make runtime code depend on lazy-loaded data** (`data/cbti_depression.rda`).
  `R/data.R` and `data/` are deliberately not staged. Ship sample data through
  `inst/extdata/`, which is.
- **A new `system.file(..., package = "pmatools")` call site needs the staging rewrite
  to cover it.** Unpatched sites resolve to `""` in the app. `stage_bundle.R` scans for
  the call shape rather than naming files, and verifies afterwards that none survived,
  so a new site is usually picked up automatically — but a lookup whose *shape* drifts
  is not. `--check-only` reports survivors with `file:line`.

## 2. Generated files

`shiny/R/_pmatools/`, `shiny/_pmatools_inst/` and `shiny/R/_pmatools/VERSION` are
`stage_bundle.R` output. They are gitignored. **Never hand-edit them** — the next run
deletes and regenerates the lot. Fix `R/` instead, then re-stage.

`VERSION` line 1 is load-bearing: `app.R` reads exactly one line. Line 2 is provenance
(`source: <branch>@<sha>`, `-dirty` when staged from uncommitted work).

`deploy.R` stages before every deploy, so a fresh clone needs no extra step. If you
wiped the tree with `git clean -xdf`, `Rscript shiny/stage_bundle.R` brings it back.

## 3. Deploying

- **Deploy only from a clean tree.** `deploy.R` ships the working tree as it stands, so
  a dirty deploy puts bytes in production that no commit describes. `deploy.R` enforces
  this and prints the HEAD SHA it shipped; put that SHA in the follow-up commit.
- The live app is `https://yuki-furukawa.shinyapps.io/pmatools/`, embedded in WordPress
  post 1021 at `https://yukifurukawa.jp/pmatools/`. **Changing `APP_NAME` in `deploy.R`
  does not rename the app — it creates a new one at a new URL** and the embed keeps
  pointing at the old one. The iframe `src` and the `mark` in the blog repo's
  `apply_tool_embed_resize.py` have to move with it, `src` first.

## 4. Worktree isolation

Many sessions run against this repo at once.

- Delegating implementation to the Agent tool: **always pass `isolation: "worktree"`.**
  Without it the shared checkout's index gets dirtied and that dirt reaches a deploy.
- From inside a worktree, never write to the main checkout by absolute path. Edit
  relative to `cwd`; copy *out of* the main checkout read-only if you need something.
- Before finishing, confirm the main checkout's `git status --porcelain` is what it was
  when you started.

## 5. Docs ship with the change, not after it

`README.md`, `SPEC.md`, `shiny/SPEC.md`, `PLAN.md` and `NEWS.md` are updated in the
**same PR** as the change that makes them stale. NEVER open a standalone "bring docs
back in line" resync commit.

- `SPEC.md` is authoritative: implementation MUST conform to it, so a behaviour change
  edits the relevant `SPEC.md` first or alongside — never afterwards.
- `NEWS.md` is authoritative for the breaking-change list. Every `feat!:` gets an entry
  in the same PR.
- `SPEC.md`'s "Version target:" header must be bumped whenever `DESCRIPTION` `Version:`
  is.

## 6. Language

Docs and code comments are English. Keep it that way.

Commits made under `shiny/` before the repos merged carry Japanese messages. That is
history; do not rewrite it.

## 7. File headers are prose, on purpose

Every `.R` file here opens with a **prose header** — a title line, then a few short
paragraphs — and not with the six-part numbered header (`#1. Copyright` …
`#6. Executed statements`) the house R style guide mandates. Every file in the repo is
written this way. It is a deliberate house rule, not 51 files that drifted, and it is
not changing. Write new files the same way.

What the numbered form encodes is *intent*, and the header must still deliver all of it:

- **what the file is** — the title line says what it owns, in its own words;
- **what it reads and what it produces** — the inputs it takes, and the objects,
  strings or UI it hands back;
- **what it depends on, or what depends on it** — name the neighbouring files;
- **the admission rule** — how a future editor decides whether a new function belongs
  here or next door. This is the part a numbered skeleton has no slot for, and the part
  that keeps a split file from silting up again.

`R/grade_vocabulary.R`, `shiny/R/step3_pubias.R` and `shiny/R/outcome_provenance.R` are
the models. Read one before writing a header.

The layout order the guide asks for still holds: header, then libraries, then constants,
then function definitions, with executed statements last. Never interleave executed
statements between function definitions.

Why prose: these files are read by sessions that begin with no memory of them, and a
`#4. Function definitions` heading sitting above 400 lines of function definitions tells
that reader nothing the prose header has not already said.
