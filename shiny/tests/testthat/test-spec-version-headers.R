# test-spec-version-headers.R - each SPEC.md opens by stating a version and
# naming the DESCRIPTION field it mirrors. A header that names its source and
# then disagrees with it is worse than one that says nothing, and CLAUDE.md 5
# requires the bump to land in the same change as the field's.
#
# It did not: `chore: bump the app version for the Step 2 / Step 3 review
# batch` (ccbb119) moved shiny/DESCRIPTION from 3.1.0 to 3.2.0 as its only
# edit, and shiny/SPEC.md went on claiming 3.1.0 for five days. Nothing failed,
# because nothing was watching. This is what watches.
#
# The two pairs are checked SEPARATELY and never against each other. There are
# deliberately two DESCRIPTION files (CLAUDE.md 0): the root one is the package
# and shiny/DESCRIPTION is an rsconnect manifest whose Version: is the app's
# own. 0.5.1 and 3.2.0 are both correct at once, and a test that expected them
# to converge would be demanding a merge the repo has ruled out.
#
# Lives in the app suite rather than the package suite because SPEC.md and
# shiny/ are both .Rbuildignore'd: under R CMD check neither file is in the
# tarball, so the package suite could only skip. This suite always runs from
# the source tree.

library(testthat)

# Far enough into either file to cover the opening block and no further; a
# version number quoted in the body is history, not a claim about the field.
SPEC_HEADER_LINES <- 40L

# The one header line introduced by `**<label>:**`, from a SPEC's opening
# block. Read as text rather than parsed as Markdown because the header is
# prose with a version in it, and its exact spelling is what the
# .Rbuildignore'd files are read for.
.spec_header_line <- function(path, label) {
  header <- readLines(path, n = SPEC_HEADER_LINES, warn = FALSE)
  hit <- grep(paste0("^\\*\\*", label, ":\\*\\*"), header, value = TRUE)
  expect_length(hit, 1L)
  hit[[1L]]
}

# The version the header states: the first dotted number after the label.
.stated_version <- function(line, label) {
  sub(paste0("^\\*\\*", label, ":\\*\\* *([0-9][0-9.]*).*$"), "\\1", line)
}

.description_version <- function(path) {
  unname(read.dcf(path, fields = "Version")[1L, 1L])
}

test_that("shiny/SPEC.md states the version in shiny/DESCRIPTION", {
  line <- .spec_header_line(file.path(PMA_APP_ROOT, "SPEC.md"), "App version")

  # The header earns the assertion by naming its source; if a rewrite points it
  # at some other file, the comparison below stops meaning what it says.
  expect_match(line, "`Version:` field of `shiny/DESCRIPTION`", fixed = TRUE)

  expect_equal(.stated_version(line, "App version"),
               .description_version(file.path(PMA_APP_ROOT, "DESCRIPTION")))
})

test_that("SPEC.md states the version in DESCRIPTION", {
  repo_root <- dirname(PMA_APP_ROOT)
  skip_if_not(file.exists(file.path(repo_root, "DESCRIPTION")),
              "package sources not next to the app")

  line <- .spec_header_line(file.path(repo_root, "SPEC.md"), "Version target")

  expect_equal(.stated_version(line, "Version target"),
               .description_version(file.path(repo_root, "DESCRIPTION")))
})
