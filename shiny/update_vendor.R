# update_vendor.R - refresh vendored pmatools sources from the local checkout
#
# =============================================================================
# !! DEPENDENCY LIFELINE - READ BEFORE TOUCHING DESCRIPTION !!
#
# The rare-event backends `metafor`, `mmeta`, and `BiasedUrn` are referenced
# ONLY as name strings in R/_pmatools/rare_events.R (e.g. package = "metafor",
# extra_packages = "BiasedUrn", engine = "mmeta"), so rsconnect's static code
# scan CANNOT detect them. The app DESCRIPTION's Imports field is the ONLY
# mechanism that gets them installed on the shinyapps.io server. They MUST
# stay listed in the app DESCRIPTION Imports, even if grep for `pkg::` finds
# nothing. Removing them deploys fine but breaks the rare-event method suite
# at runtime.
#
# After every vendor refresh, the dependency-sync check below compares the
# source pmatools DESCRIPTION against the app DESCRIPTION and warns loudly
# about anything missing. Run it standalone with:
#   Rscript update_vendor.R --check-only
# =============================================================================
#
# Run after pulling new pmatools commits in ~/Developer/pmatools/.
# Idempotent: re-patches the template path so the vendored copy looks in
# _pmatools_inst/ before falling back to system.file().

PMATOOLS_SRC <- "/Users/furukawayuonore/Developer/pmatools"
APP_DIR      <- normalizePath(".")

# Dev-only packages in pmatools Suggests that the deployed app never needs.
# Everything else in pmatools Imports/Suggests is assumed runtime-relevant.
DEV_ONLY_PKGS <- c("testthat", "rmarkdown", "here", "knitr", "covr",
                   "devtools", "usethis", "roxygen2")

# --- Dependency-sync check ---------------------------------------------------
# Parses the source pmatools DESCRIPTION and compares its Imports + Suggests
# against the app DESCRIPTION's Imports. Prints a loud warning listing any
# package present in pmatools deps but missing from the app DESCRIPTION.
# Non-fatal (warning, not stop): some Suggests are genuinely optional, but
# every miss must be a conscious decision -- string-referenced backends
# (metafor, mmeta, BiasedUrn) are invisible to rsconnect's scanner.
check_app_dependencies <- function(pmatools_src = PMATOOLS_SRC,
                                   app_dir = APP_DIR,
                                   dev_only = DEV_ONLY_PKGS) {
  parse_deps <- function(desc_path, fields) {
    if (!file.exists(desc_path)) {
      warning("DESCRIPTION not found: ", desc_path, call. = FALSE)
      return(character(0))
    }
    dcf <- read.dcf(desc_path, fields = fields)
    raw <- unlist(strsplit(paste(dcf[!is.na(dcf)], collapse = ","), ","))
    # Strip version requirements like "meta (>= 6.0)" and whitespace.
    pkgs <- trimws(sub("\\(.*\\)", "", raw))
    pkgs <- pkgs[nzchar(pkgs) & pkgs != "R"]
    unique(pkgs)
  }

  src_desc <- file.path(pmatools_src, "DESCRIPTION")
  app_desc <- file.path(app_dir, "DESCRIPTION")

  src_deps <- parse_deps(src_desc, c("Imports", "Suggests"))
  app_imports <- parse_deps(app_desc, "Imports")

  missing <- setdiff(setdiff(src_deps, dev_only), app_imports)

  cat("\n--- Dependency-sync check (pmatools -> app DESCRIPTION) ---\n")
  cat("  pmatools deps (Imports+Suggests): ",
      paste(src_deps, collapse = ", "), "\n", sep = "")
  cat("  ignored as dev-only:              ",
      paste(intersect(src_deps, dev_only), collapse = ", "), "\n", sep = "")

  if (length(missing) == 0L) {
    cat("  OK: every runtime-relevant pmatools dependency is in the app",
        "DESCRIPTION Imports.\n")
  } else {
    banner <- paste(rep("!", 72), collapse = "")
    cat("\n", banner, "\n", sep = "")
    cat("!! WARNING: packages in pmatools deps but MISSING from app",
        "DESCRIPTION Imports:\n")
    cat("!!   ", paste(missing, collapse = ", "), "\n", sep = "")
    cat("!! rsconnect cannot see string-referenced packages -- if any of",
        "these is\n!! used at runtime it will be ABSENT on shinyapps.io.\n")
    cat(banner, "\n\n", sep = "")
    warning("App DESCRIPTION Imports is missing pmatools dependencies: ",
            paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(missing)
}

# Standalone mode: `Rscript update_vendor.R --check-only` runs only the
# dependency-sync check without touching any vendored files.
if ("--check-only" %in% commandArgs(trailingOnly = TRUE)) {
  check_app_dependencies()
  quit(save = "no", status = 0)
}

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

# 4. Dependency-sync check: did upstream pmatools grow a dependency the app
#    DESCRIPTION does not install on shinyapps.io?
check_app_dependencies()

cat("\nDone. Restart the Shiny app to pick up changes.\n")
