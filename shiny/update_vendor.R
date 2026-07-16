# update_vendor.R - refresh vendored pmatools sources from the local checkout
#
# Run after pulling new pmatools commits in ~/Developer/pmatools/.
# Idempotent: re-patches the template path so the vendored copy looks in
# _pmatools_inst/ before falling back to system.file().

PMATOOLS_SRC <- "/Users/furukawayuonore/Developer/pmatools"
APP_DIR      <- normalizePath(".")

cat("Refreshing vendored pmatools from ", PMATOOLS_SRC, "\n", sep = "")

# 1. R sources (skip data.R which is just lazy-data roxygen)
target_r <- file.path(APP_DIR, "R", "_pmatools")
unlink(target_r, recursive = TRUE)
dir.create(target_r, recursive = TRUE)
src_r <- list.files(file.path(PMATOOLS_SRC, "R"), pattern = "\\.R$",
                    full.names = TRUE)
src_r <- src_r[basename(src_r) != "data.R"]
file.copy(src_r, target_r)
cat("  R/_pmatools/: ", length(src_r), " files\n", sep = "")

# 2. inst assets
target_inst <- file.path(APP_DIR, "_pmatools_inst")
unlink(target_inst, recursive = TRUE)
file.copy(file.path(PMATOOLS_SRC, "inst"), APP_DIR, recursive = TRUE)
file.rename(file.path(APP_DIR, "inst"), target_inst)
cat("  _pmatools_inst/: copied\n")

# 3. Patch template path in vendored export_bundle.R: replace
#    `system.file("templates", ..., package = "pmatools")` with a path
#    that first looks in _pmatools_inst/ (where the vendored template lives).
ebpath <- file.path(target_r, "export_bundle.R")
if (file.exists(ebpath)) {
  txt <- paste(readLines(ebpath, warn = FALSE), collapse = "\n")
  # Match the multi-line literal regardless of indentation.
  pat <- "tpl_path <- system.file\\(\"templates\", \"analysis_script\\.R\\.tpl\",[^\n]*\n[ \t]+package = \"pmatools\"\\)"
  replacement <- paste0(
    'tpl_path <- file.path(getOption("pmatools.vendored_root", "."),\n',
    '                          "_pmatools_inst", "templates",\n',
    '                          "analysis_script.R.tpl")'
  )
  new_txt <- sub(pat, replacement, txt, perl = TRUE)
  if (!identical(new_txt, txt)) {
    writeLines(new_txt, ebpath)
    cat("  export_bundle.R: template path patched.\n")
  } else {
    warning("export_bundle.R: template-path pattern not matched; ",
            "patch manually if you see 'system.file(..., package=\"pmatools\")'.")
  }
}

cat("\nDone. Restart the Shiny app to pick up changes.\n")
