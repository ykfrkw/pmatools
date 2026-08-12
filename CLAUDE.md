# pmatools — repo rules

## 1. The vendoring lifeline (MUST)

The Shiny app at `/Users/furukawayuonore/Developer/pairwise_meta_analysis` **vendors** this
package: `update_vendor.R` copies `R/*.R` into its `R/_pmatools/` and `inst/` into its
`_pmatools_inst/`, and `app.R` `source()`s those files. It never installs pmatools.

Consequences, and they are not optional:

- A release is NOT done until the app has been re-vendored and
  `pairwise_meta_analysis/R/_pmatools/VERSION` matches this repo's `DESCRIPTION` `Version:`.
  This applies doubly to any `feat!:` breaking change — the app runs the old sources until
  someone runs `Rscript update_vendor.R` there.
- NEVER rely on `utils::packageVersion("pmatools")` at runtime. It errors under `source()`.
  Use the existing `.vendored_version_stamp()` / `getOption("pmatools.version_stamp")` path
  in `R/utils.R`.
- NEVER add a new `system.file(..., package = "pmatools")` call site without arranging the
  matching rewrite in the app's `update_vendor.R`. Unpatched call sites resolve to `""`
  in the app and the feature fails only at runtime, only in production.
- NEVER make runtime code depend on lazy-loaded data (`data/cbti_depression.rda`). `R/data.R`
  and `data/` are deliberately not vendored. Ship sample data through `inst/extdata/`, which is.
- Adding a package to `DESCRIPTION` `Imports`/`Suggests` means adding it to the *app's*
  `DESCRIPTION` `Imports` too, or shinyapps.io will not install it. Check with
  `Rscript update_vendor.R --check-only` in the app repo.

Do not edit `update_vendor.R` from this repo. Report what it needs; the app repo owns it.

## 2. Docs ship with the change, not after it

`README.md`, `SPEC.md`, `PLAN.md` and `NEWS.md` are updated in the **same PR** as the change
that makes them stale. NEVER open a standalone "bring docs back in line" resync commit.

- `SPEC.md` is authoritative: implementation MUST conform to it, so a behaviour change edits
  `SPEC.md` first or alongside — never afterwards.
- `NEWS.md` is authoritative for the breaking-change list. Every `feat!:` gets an entry in the
  same PR.
- `SPEC.md`'s "Version target:" header must be bumped whenever `DESCRIPTION` `Version:` is.

## 3. Language

Docs and code comments are English. Keep it that way.
