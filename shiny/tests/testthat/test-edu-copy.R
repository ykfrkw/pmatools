# EDU_COPY (R/educational_copy.R) is the app's copy deck. Two things are
# asserted here: the shape the UI reads it through, and the one-line cap on
# the strings that render as a `.pma-card-subtitle`.
#
# The five `how` bodies and their accessor `edu_domain_how()` are gone as of
# 0.5.1, together with `pma_how_collapse()` and `EDU_COPY$config_tab$intro`.
# Four of the five domains draw their algorithm as a flowchart with the branch
# taken lit up, and Indirectness has no flowchart because its PICO question
# labels and subdomain table cover the same ground. The assertions below are
# what stops the accordion coming back: it is one line to re-add, and each
# body was 100+ words that sat behind a click nobody made.

test_that("the collapsed 'How is this judged?' copy is gone", {
  expect_false(exists("edu_domain_how", mode = "function"))
  for (d in c("rob", "inconsistency", "indirectness", "imprecision", "pubias")) {
    expect_null(EDU_COPY$domains[[d]]$how, info = d)
  }
  # The Configuration tab's 115-word opener went the same way, and so did the
  # recitation of the three presentations Core GRADE 6 ranks. The DEPARTURE
  # from it stays: that one tells the reviewer their inferences must be weaker,
  # which is not a thing they can read off the screen.
  expect_null(EDU_COPY$config_tab$intro)
  expect_null(EDU_COPY$config_tab$continuous_intro)
  expect_true(nzchar(EDU_COPY$config_tab$continuous_departure))
})

test_that("every EDU_COPY subtitle fits one desktop line", {
  # The operational form of "delete first, shorten second, hide never". A muted
  # line under a control is read while the reviewer decides that control; past
  # EDU_COPY_SUBTITLE_WORD_CAP words it wraps to a second line and stops being
  # read at all, and a sentence that cannot be said in one line was not
  # answering the control it sat under.
  expect_gt(length(EDU_COPY_SUBTITLE_FIELDS), 0L)
  for (path in EDU_COPY_SUBTITLE_FIELDS) {
    text <- edu_copy_field(path)
    expect_true(is.character(text) && length(text) == 1L,
                info = paste(path, "- stale entry in EDU_COPY_SUBTITLE_FIELDS"))
    expect_lte(edu_copy_word_count(text), EDU_COPY_SUBTITLE_WORD_CAP)
  }
})

test_that("the saved-outcome copy no longer describes a Save button", {
  # There is none as of 0.5.1 (shiny/SPEC.md 3.4.14). `save_locked` was the
  # "saving is locked until..." note and was already dead code; the other four
  # strings told the reviewer to press something.
  expect_null(EDU_COPY$multi_outcome$save_locked)
  for (field in c("save_intro", "list_empty", "step4_intro", "step4_empty")) {
    text <- EDU_COPY$multi_outcome[[field]]
    expect_true(nzchar(text), info = field)
    expect_false(grepl("save it|Save a|press|button", text), info = field)
    # ... and each is now capped like every other subtitle.
    expect_true(paste0("multi_outcome$", field) %in% EDU_COPY_SUBTITLE_FIELDS,
                info = field)
  }
})

test_that("edu_copy_word_count() counts what a reader sees", {
  expect_equal(edu_copy_word_count("one two three"), 3L)
  # Copy is assembled with paste0() across source lines, so a joined string can
  # carry runs of whitespace. They are not words.
  expect_equal(edu_copy_word_count("  one   two\nthree  "), 3L)
  expect_equal(edu_copy_word_count(""), 0L)
})

test_that("edu_copy_field() returns NULL for a path that no longer exists", {
  # So a stale registry entry is reported as one, by name, rather than
  # erroring out of the loop above with nothing to identify it.
  expect_null(edu_copy_field("domains$rob$how"))
  expect_null(edu_copy_field("no_such$path"))
})

test_that("a step header carries a title and nothing else", {
  # The `what` paragraph each step used to open with is gone: it was re-read
  # on every visit, pushed the first control below the fold, and said the same
  # thing four times over. pma_step_header() takes the title alone now, so a
  # resurrected `$what` would be silently dropped rather than rendered - hence
  # the assertion on the shape, not just on the header.
  for (step in c("step1", "step2", "step3", "step4")) {
    entry <- EDU_COPY$steps[[step]]
    expect_named(entry, "title", info = step)
    expect_true(nzchar(entry$title), info = step)
  }
  expect_identical(names(formals(pma_step_header)), "title")
})

test_that("the once-per-session intro modal carries the SR&MA caveat", {
  # Formerly EDU_COPY$steps$step1$why, and restated verbatim in the Step 4
  # "How to cite" card. It is the one claim in the app about the work AROUND
  # the analysis, so it is now stated once, from app.R, as a modal.
  intro <- EDU_COPY$intro_modal
  expect_setequal(names(intro), c("title", "body", "dismiss"))
  for (field in c("title", "dismiss")) {
    expect_true(nzchar(intro[[field]]), info = field)
  }
  body <- as.character(intro$body)
  expect_match(body, "pre-registered protocol", fixed = TRUE)
  expect_match(body, "dual independent screening and data extraction",
               fixed = TRUE)
  expect_match(body, "completed BEFORE the analysis", fixed = TRUE)
})

test_that("every rated domain still carries its Core GRADE reference", {
  # The domain tabs lost their `how` bodies; the reference line under each is
  # now the ONLY pointer to the source paper, so it is load-bearing.
  for (d in c("rob", "inconsistency", "indirectness", "imprecision", "pubias")) {
    entry <- EDU_COPY$domains[[d]]
    expect_true(nzchar(entry$header), info = d)
    expect_true(nzchar(entry$ref), info = d)
    # The `$ref_text` / `$doi` pair it replaced, so a half-done revert is a
    # failure here rather than a reference line that renders two ways.
    expect_null(entry$ref_text, info = d)
    expect_null(entry$doi, info = d)
  }
})

test_that("every domain reference is in the app's one citation style", {
  # First author, "et al.", journal abbreviation, year - and nothing else. The
  # six Core GRADE papers are all Guyatt / BMJ / 2025, so they carry the series
  # number as a prefix; without it the five tabs would cite one string. The
  # regex is what stops a volume, a page range or a DOI creeping back in.
  house_style <- "^(Core GRADE \\d\\. )?[A-Z][a-z]+ [A-Z]{1,2}, et al\\. .+\\. \\d{4}$"
  for (d in c("rob", "inconsistency", "indirectness", "imprecision", "pubias")) {
    expect_match(EDU_COPY$domains[[d]]$ref, house_style, info = d)
  }
  # Risk of bias and publication bias are both Core GRADE 4, so they render
  # identically. That is the source paper, not a copy-paste slip.
  expect_identical(EDU_COPY$domains$rob$ref, EDU_COPY$domains$pubias$ref)
})

test_that("every domain names the Core GRADE paper it cites, as a number", {
  # The number is what .core_grade_doi_url() is keyed on. Recovering it from
  # the "Core GRADE n." prefix instead would make any rewording of the citation
  # a dead link, so the field has to exist and has to agree with the prefix.
  for (d in c("rob", "inconsistency", "indirectness", "imprecision", "pubias")) {
    entry <- EDU_COPY$domains[[d]]
    expect_true(is.numeric(entry$core_grade), info = d)
    expect_match(entry$ref, paste0("^Core GRADE ", entry$core_grade, "\\. "),
                 info = d)
    expect_false(is.null(.core_grade_doi_url(entry$core_grade)), info = d)
  }
})

test_that("a domain reference links to the paper's DOI in a new tab", {
  # The `doi` argument came back as `url` in 0.5.1, fed from one map rather
  # than from a per-domain field, so every Core GRADE tab renders alike.
  expect_true("url" %in% names(formals(pma_reference)))
  ref <- as.character(pma_domain_reference(EDU_COPY$domains$rob))
  expect_match(ref, "Core GRADE 4. Guyatt G, et al. BMJ. 2025", fixed = TRUE)
  expect_match(ref, "https://doi.org/10.1136/bmj-2024-083864", fixed = TRUE)
  expect_match(ref, "target=\"_blank\"", fixed = TRUE)
  # target="_blank" without this hands the opened page a live window.opener.
  expect_match(ref, "rel=\"noopener\"", fixed = TRUE)
  # Indirectness cites a different paper; the two must not collapse.
  indir <- as.character(pma_domain_reference(EDU_COPY$domains$indirectness))
  expect_match(indir, "https://doi.org/10.1136/bmj-2024-083865", fixed = TRUE)

  # No url, no link: the reference still renders, as plain text. That is the
  # path Step 2's non-Core-GRADE citations take.
  plain <- as.character(pma_reference("Efthimiou O. Evid Based Ment Health. 2018"))
  expect_false(grepl("<a ", plain, fixed = TRUE))

  # Deleted with the per-domain DOI fields; it had no call sites left.
  expect_null(EDU_COPY$pmid_url)
})

# The two version helpers must not converge: Step 4's "How to cite" entry is
# pasted into someone's manuscript, so "Version 0.5.1 (vendored)." must never
# reach it, while the Step 2 environment block and the app footer do want the
# marker. Both tests below drive the *stamp* path, so they only mean anything
# when pmatools is not installed -- which is the deployed app's situation, and
# the one the citation is written for.
skip_unless_vendored <- function() {
  installed <- tryCatch(utils::packageVersion("pmatools"),
                        error = function(e) NULL)
  skip_if(!is.null(installed),
          "pmatools is installed, so the vendored stamp path is unreachable")
}

# Plain options() rather than withr::with_options(): withr is not in the app's
# DESCRIPTION, and the app test suite is not a package that can declare it.
with_version_stamp <- function(stamp, code) {
  previous <- options(pmatools.version_stamp = stamp)
  on.exit(options(previous), add = TRUE)
  force(code)
}

test_that("the citable version carries no provenance marker", {
  skip_unless_vendored()
  with_version_stamp("9.9.9", {
    expect_identical(pma_pmatools_version(), "9.9.9 (vendored)")
    expect_identical(pma_pmatools_version_number(), "9.9.9")
  })
})

test_that("an unknown version yields NULL rather than a sentinel string", {
  # The caller drops the whole "Version X." clause on NULL. An incomplete
  # citation is honest; "Version (vendored; version unknown)." is not.
  skip_unless_vendored()
  with_version_stamp(NULL, {
    expect_identical(pma_pmatools_version(), "(vendored; version unknown)")
    expect_null(pma_pmatools_version_number())
  })
})
