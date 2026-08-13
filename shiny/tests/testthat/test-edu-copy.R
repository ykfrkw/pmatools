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
    expect_true(nzchar(entry$doi), info = d)
    expect_true(nzchar(entry$ref_text), info = d)
  }
})
