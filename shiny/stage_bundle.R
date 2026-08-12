# stage_bundle.R - stage the pmatools sources this app deploys with
#
# The app SOURCES pmatools rather than installing it: a stale GITHUB_PAT cached
# on the shinyapps.io account makes install_github(ykfrkw/pmatools) return HTTP
# 401 on the build server. So the deploy bundle has to carry the package
# sources inside appDir, and this script puts them there -- copying ../R and
# ../inst into R/_pmatools/ and _pmatools_inst/.
#
# It reads from the repository it lives in. There is no second checkout to
# point at, no version skew to guard against, and no re-vendoring step anyone
# can forget: the sources one directory up ARE the sources that ship.
#
# Its output is generated and gitignored. Never hand-edit R/_pmatools/ or
# _pmatools_inst/ -- the next run deletes both and regenerates them. Fix the
# package sources in ../R instead.
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
# about anything missing.
#
# !! TEMPLATE PATHS ARE REWRITTEN ON EVERY REFRESH !!
#
# Upstream pmatools locates its script templates with
#   system.file("templates", <name>, package = "pmatools")
# which always returns "" here: the app SOURCES R/_pmatools/*.R instead of
# installing the package, so there is no installed inst/ to look in. Step 3
# scans EVERY vendored R file for that call shape and rewrites each hit to
# read from _pmatools_inst/templates/ instead. It is a scan, not a filename
# list, so a template added upstream later is picked up with no edit here
# (upstream currently has two: export_bundle.R and export_bundle_multi.R).
#
# Step 5 then greps the whole vendored tree for any system.file() call that
# still carries package = "pmatools" -- i.e. a lookup whose shape drifted
# far enough that the patch missed it -- and warns loudly with file:line.
#
# Both checks run standalone, without touching any vendored file, via:
#   Rscript shiny/stage_bundle.R --check-only
#
# !! STAGING FROM A DIRTY TREE IS NORMAL - THE DEPLOY IS WHERE IT IS CHECKED !!
#
# This script used to refuse a dirty source, because it read from a separate
# checkout that a parallel session might have left mid-edit. Reading from its
# own repository, that reversed: you stage precisely to try out the change you
# just made in ../R, so refusing would break the ordinary case.
#
# The clean-tree requirement did not disappear, it moved to where it belongs.
# deploy.R refuses to ship a dirty tree, because a deploy from one puts bytes
# in production that no commit describes. The git state is still read here,
# but only to record provenance on line 2 of VERSION.
#
# !! R/_pmatools/VERSION IS GENERATED - DO NOT HAND-EDIT !!
#
# This script writes the source pmatools DESCRIPTION `Version:` field to
# R/_pmatools/VERSION, and app.R loads it into
# options(pmatools.version_stamp = ...) so the app can report which pmatools
# it actually vendors. Editing that file by hand is pointless: the next
# vendor refresh deletes R/_pmatools/ and regenerates the stamp.
#
# Line 1 is the version string and nothing else -- app.R reads exactly one
# line -- so line 2 carries the provenance a bare version cannot:
#   source: <branch>@<sha>          (clean tree)
#   source: <branch>@<sha>-dirty    (staged from uncommitted work)
#   source: unknown                 (no readable git state)
# Anything added later goes on line 3 or below; line 1 stays load-bearing.
#
# Since the version now comes from the same DESCRIPTION that defines the
# package version, VERSION line 1 and ../DESCRIPTION cannot disagree.
# =============================================================================
#
# Idempotent: the staged tree is regenerated from ../R on every run, and the
# template-path rewrite leaves behind text that no longer matches its own
# pattern, so re-running is a no-op.

# Self-locating, so `Rscript shiny/stage_bundle.R` works from anywhere in the
# repo and deploy.R can source() it.
#
# --file= is whatever Rscript was handed, which is usually RELATIVE, and under
# source() it is deploy.R's rather than ours. Both scripts live in this
# directory so the dirname is right either way -- but deploy.R has already
# setwd()'d here by then, so resolving "shiny" against the new working
# directory looks for shiny/shiny and fails. When the derived path does not
# exist, the working directory is already the app directory: use it.
.script_dir <- function() {
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f) == 0L) return(normalizePath("."))   # interactive fallback
  d <- dirname(sub("^--file=", "", f[1L]))
  if (!dir.exists(d)) return(normalizePath("."))
  normalizePath(d)
}

APP_DIR    <- .script_dir()
PKG_ROOT   <- normalizePath(file.path(APP_DIR, ".."))
VENDORED_R <- file.path(APP_DIR, "R", "_pmatools")

# Every path below this line is app-relative, as are app.R's source() calls and
# the "." default of getOption("pmatools.vendored_root").
setwd(APP_DIR)

# Dev-only packages in pmatools Suggests that the deployed app never needs.
# Everything else in pmatools Imports/Suggests is assumed runtime-relevant.
DEV_ONLY_PKGS <- c("testthat", "rmarkdown", "here", "knitr", "covr",
                   "devtools", "usethis", "roxygen2")

# --- Package-root validation -------------------------------------------------
# DESCRIPTION / R / inst are exactly the three entries the steps below consume.
# Checking them turns "this script was moved, or the repo was restructured"
# into one clear message instead of a read.dcf()/file.copy() failure halfway
# through a wiped R/_pmatools/.
require_valid_src <- function(src = PKG_ROOT) {
  hint <- paste0("stage_bundle.R expects to sit one level below the pmatools ",
                 "package root\n(pmatools/shiny/stage_bundle.R). If the ",
                 "layout moved, update PKG_ROOT.")
  if (!dir.exists(src)) {
    stop("pmatools package root not found: ", src, "\n", hint, call. = FALSE)
  }
  needed <- c("DESCRIPTION", "R", "inst")
  absent <- needed[!file.exists(file.path(src, needed))]
  if (length(absent) > 0L) {
    stop("Not a pmatools package root (missing ",
         paste(absent, collapse = ", "), "): ", src, "\n", hint,
         call. = FALSE)
  }
  invisible(TRUE)
}

# --- Vendor source git state -------------------------------------------------
# One git invocation, captured rather than inherited so a missing git binary
# or a non-repository source degrades to "unknown" instead of spraying stderr
# and aborting. stderr is folded into stdout only so it cannot leak to the
# console; the output is used exclusively when the exit status is 0.
git_field <- function(src, args) {
  out <- tryCatch(
    suppressWarnings(system2("git", c("-C", shQuote(src), args),
                             stdout = TRUE, stderr = TRUE)),
    error = function(e) NULL,
    warning = function(w) NULL)
  if (is.null(out)) return(NULL)
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) return(NULL)
  as.character(out)
}

# Reads branch / short SHA / porcelain status of the repo. Returns
# known = FALSE (never an error) when git cannot answer, because an
# undescribable source is unverifiable rather than known-bad. Used for the
# VERSION provenance line only -- deploy.R owns the clean-tree requirement.
pmatools_src_meta <- function(src = PKG_ROOT) {
  unknown <- list(known = FALSE, branch = NA_character_, sha = NA_character_,
                  dirty = NA, dirty_files = character(0))

  sha    <- git_field(src, c("rev-parse", "--short", "HEAD"))
  branch <- git_field(src, c("rev-parse", "--abbrev-ref", "HEAD"))
  porc   <- git_field(src, c("status", "--porcelain"))

  # porc is legitimately character(0) on a clean tree, so only sha and branch
  # are required to be non-empty.
  if (is.null(sha) || is.null(branch) || is.null(porc) ||
      length(sha) == 0L || length(branch) == 0L) {
    return(unknown)
  }

  dirty_files <- porc[nzchar(trimws(porc))]
  list(known = TRUE,
       branch = trimws(branch[1L]),
       sha = trimws(sha[1L]),
       dirty = length(dirty_files) > 0L,
       dirty_files = dirty_files)
}

# The `source:` line written to R/_pmatools/VERSION, and the same string used
# in the progress output so the stamp and the console agree.
src_label <- function(meta) {
  if (!isTRUE(meta$known)) return("unknown")
  paste0(meta$branch, "@", meta$sha, if (isTRUE(meta$dirty)) "-dirty" else "")
}

# --- Dependency-sync check ---------------------------------------------------
# Parses the source pmatools DESCRIPTION and compares its Imports + Suggests
# against the app DESCRIPTION's Imports. Prints a loud warning listing any
# package present in pmatools deps but missing from the app DESCRIPTION.
# Non-fatal (warning, not stop): some Suggests are genuinely optional, but
# every miss must be a conscious decision -- string-referenced backends
# (metafor, mmeta, BiasedUrn) are invisible to rsconnect's scanner.
check_app_dependencies <- function(pmatools_src = PKG_ROOT,
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

# --- Template-path rewrite ---------------------------------------------------
# Matches the whole upstream lookup, capturing the template basename so each
# call site keeps asking for the file it actually wants (analysis_script.R.tpl
# vs analysis_script_multi.R.tpl vs whatever lands next). Deliberately blind to
# the assignment target: what matters is the system.file() expression, not the
# variable it feeds.
TPL_LOOKUP_PAT <- paste0(
  "system\\.file\\(\\s*\"templates\",\\s*\"([^\"]+)\",\\s*",
  "package\\s*=\\s*\"pmatools\"\\s*\\)"
)

# Rewrites every such lookup in the vendored tree to read from
# _pmatools_inst/templates/. Driven off a directory scan, not a filename list:
# upstream grew a second call site (export_bundle_multi.R) after this script
# was first written, and the single hardcoded filename meant nobody noticed.
# Idempotent by construction -- the replacement text contains no system.file()
# call, so a second pass matches nothing.
patch_template_paths <- function(target_dir = VENDORED_R) {
  files <- list.files(target_dir, pattern = "\\.R$", full.names = TRUE)
  n_files <- 0L
  n_sites <- 0L

  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- gregexpr(TPL_LOOKUP_PAT, txt, perl = TRUE)[[1L]]
    if (m[1L] == -1L) next

    hits <- regmatches(txt, list(m))[[1L]]
    starts <- as.integer(m)
    tpl_names <- sub(TPL_LOOKUP_PAT, "\\1", hits, perl = TRUE)
    reps <- character(length(hits))

    for (i in seq_along(hits)) {
      # Continuation lines are indented to where the arguments of the call we
      # are replacing already sat, so the patched source keeps the upstream
      # visual shape and the diff stays confined to the path itself.
      before <- substr(txt, 1L, starts[i] - 1L)
      nl <- gregexpr("\n", before, fixed = TRUE)[[1L]]
      last_nl <- if (nl[1L] == -1L) 0L else max(nl)
      indent <- strrep(" ", starts[i] - last_nl - 1L + nchar("system.file("))
      reps[i] <- paste0(
        'file.path(getOption("pmatools.vendored_root", "."),\n',
        indent, '"_pmatools_inst", "templates",\n',
        indent, '"', tpl_names[i], '")'
      )
    }

    regmatches(txt, list(m)) <- list(reps)
    writeLines(txt, f)
    n_files <- n_files + 1L
    n_sites <- n_sites + length(hits)
    cat("  ", basename(f), ": ", length(hits), " template path(s) patched (",
        paste(tpl_names, collapse = ", "), ")\n", sep = "")
  }

  if (n_files == 0L) {
    # Not fatal on its own: either upstream stopped using system.file() for
    # templates, or the call shape drifted. The verification below decides.
    cat("  no system.file() template lookup matched -- see verification\n")
  }
  invisible(c(files = n_files, sites = n_sites))
}

# --- Vendored template-path verification -------------------------------------
# Safety net for the rewrite above: greps the whole vendored tree for any
# system.file() call still carrying package = "pmatools". In the app that call
# can only ever return "", so a survivor is a template the vendored code will
# fail to find at runtime. Non-fatal (warning, not stop), matching
# check_app_dependencies(): the refresh itself is still worth completing.
check_vendored_template_paths <- function(target_dir = VENDORED_R) {
  cat("\n--- Vendored template-path check (system.file -> _pmatools_inst) ---\n")

  if (!dir.exists(target_dir)) {
    warning("Vendored directory not found: ", target_dir, call. = FALSE)
    return(invisible(character(0)))
  }

  # `[^()]` spans newlines, so this catches the call whether or not the
  # package argument sits on a continuation line. The inner alternative allows
  # one level of nested parens (e.g. a paste0() argument) before giving up.
  pat <- "system\\.file\\((?:[^()]|\\([^()]*\\))*package\\s*=\\s*\"pmatools\""

  files <- list.files(target_dir, pattern = "\\.R$", full.names = TRUE)
  survivors <- character(0)
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- gregexpr(pat, txt, perl = TRUE)[[1L]]
    if (m[1L] == -1L) next
    for (s in as.integer(m)) {
      before <- substr(txt, 1L, s - 1L)
      nl <- gregexpr("\n", before, fixed = TRUE)[[1L]]
      line_no <- if (nl[1L] == -1L) 1L else length(nl) + 1L
      survivors <- c(survivors,
                     paste0("R/_pmatools/", basename(f), ":", line_no))
    }
  }

  cat("  scanned: ", length(files), " vendored R files\n", sep = "")

  if (length(survivors) == 0L) {
    cat("  OK: no system.file(..., package = \"pmatools\") lookups remain.\n")
  } else {
    banner <- paste(rep("!", 72), collapse = "")
    cat("\n", banner, "\n", sep = "")
    cat("!! WARNING: vendored code still calls system.file(package =",
        "\"pmatools\"):\n")
    for (s in survivors) cat("!!   ", s, "\n", sep = "")
    cat("!! pmatools is SOURCED here, never installed, so that call returns",
        "\"\" and\n!! the template will NOT be found at runtime. Widen",
        "TPL_LOOKUP_PAT in\n!! shiny/stage_bundle.R to cover the new call shape.\n")
    cat(banner, "\n\n", sep = "")
    warning("Vendored pmatools still resolves templates via system.file(): ",
            paste(survivors, collapse = ", "), call. = FALSE)
  }
  invisible(survivors)
}

# Validate the package root before either mode proceeds: --check-only reads
# ../DESCRIPTION too, so a broken layout has to fail there just as loudly.
require_valid_src()
src_meta <- pmatools_src_meta()

# Standalone mode: `Rscript shiny/stage_bundle.R --check-only` runs both
# read-only checks without touching any staged file. One flag rather than a
# sibling --check-templates: both answer the same question ("is the staged
# tree deployable as it stands?"), both are pure reads, and a single entry
# point means a check added later cannot be forgotten by whoever runs it.
if ("--check-only" %in% commandArgs(trailingOnly = TRUE)) {
  cat("Checking the staged pmatools against ", PKG_ROOT, "\n", sep = "")
  cat("  source state: ", src_label(src_meta), "\n", sep = "")
  check_app_dependencies()
  check_vendored_template_paths()
  quit(save = "no", status = 0)
}

cat("Staging pmatools from ", PKG_ROOT, "\n", sep = "")
cat("  source state: ", src_label(src_meta), "\n", sep = "")

# 1. R sources (skip data.R which is just lazy-data roxygen)
target_r <- VENDORED_R
unlink(target_r, recursive = TRUE)
invisible(dir.create(target_r, recursive = TRUE))
src_r <- list.files(file.path(PKG_ROOT, "R"), pattern = "\\.R$",
                    full.names = TRUE)
src_r <- src_r[basename(src_r) != "data.R"]
invisible(file.copy(src_r, target_r))
cat("  R/_pmatools/: ", length(src_r), " files\n", sep = "")

# 1b. Version stamp. The app SOURCES these files instead of installing the
#     package, so utils::packageVersion("pmatools") always errors there and
#     nothing downstream can tell which pmatools it is running. Record the
#     source DESCRIPTION Version in R/_pmatools/VERSION; app.R reads it into
#     options(pmatools.version_stamp = ...). Written AFTER the copy step
#     because that step unlink()s and recreates the whole directory.
#
#     Line 1 stays the bare version string: app.R does readLines(n = 1L) and
#     must keep seeing exactly that. Line 2 records which commit the version
#     came from ("source: <branch>@<sha>", "-dirty" when staged from
#     uncommitted work, "unknown" when git could not say), which is what
#     actually pins the bytes down -- two builds can share a Version field
#     and differ.
stamp_vendored_version <- function(src_desc, target_dir, meta = src_meta) {
  if (!file.exists(src_desc)) {
    warning("Version stamp NOT written: DESCRIPTION not found at ", src_desc,
            call. = FALSE)
    return(NA_character_)
  }
  ver <- tryCatch(read.dcf(src_desc, fields = "Version")[1L, 1L],
                  error = function(e) NA_character_)
  if (is.na(ver) || !nzchar(trimws(ver))) {
    warning("Version stamp NOT written: no usable 'Version:' field in ",
            src_desc, ". The app will report ",
            "'(vendored; version unknown)' until this is fixed.",
            call. = FALSE)
    return(NA_character_)
  }
  ver <- trimws(ver)
  writeLines(c(ver, paste0("source: ", src_label(meta))),
             file.path(target_dir, "VERSION"))
  ver
}

stamped_version <- stamp_vendored_version(
  file.path(PKG_ROOT, "DESCRIPTION"), target_r, src_meta)
cat("  R/_pmatools/VERSION: ",
    if (is.na(stamped_version)) "NOT WRITTEN (see warning)" else
      paste0(stamped_version, " (source: ", src_label(src_meta), ")"),
    "\n", sep = "")

# 2. inst assets
target_inst <- file.path(APP_DIR, "_pmatools_inst")
unlink(target_inst, recursive = TRUE)
invisible(file.copy(file.path(PKG_ROOT, "inst"), APP_DIR, recursive = TRUE))
invisible(file.rename(file.path(APP_DIR, "inst"), target_inst))
cat("  _pmatools_inst/: copied\n")

# 3. Patch template paths across the whole vendored tree: replace every
#    `system.file("templates", <name>, package = "pmatools")` with a path into
#    _pmatools_inst/ (where the vendored templates live).
patch_template_paths(target_r)

# 4. Dependency-sync check: did upstream pmatools grow a dependency the app
#    DESCRIPTION does not install on shinyapps.io?
check_app_dependencies()

# 5. Verification: did any template lookup escape step 3?
check_vendored_template_paths(target_r)

cat("\nDone. Restart the Shiny app to pick up changes.\n")
