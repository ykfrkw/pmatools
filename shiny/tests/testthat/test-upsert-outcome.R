# pma_upsert_outcome() (R/outcome_bank.R) - which row a banked outcome lands in.
#
# There is no Save button: an outcome is re-banked on every recompute once its
# six domains are confirmed (shiny/SPEC.md 3.4.14). So "which row is this?"
# cannot be the display name - correcting a typo in the outcome name would add
# a second row on the next recompute, and keep adding one per rename. The uid
# answers it instead, and names(outcomes) stays the display name because
# grade_table(), pma_saved_outcomes_ui(), .outcome_set() and set$order all key
# on it.
#
# The rename-in-place case is the one that matters.

.saved <- function(certainty = "High") {
  structure(list(certainty = certainty), class = "pmatools")
}

test_that("a first save inserts a row under its display name", {
  outs <- pma_upsert_outcome(NULL, "Depression response", .saved(), "outcome-1")

  expect_named(outs, "Depression response")
  expect_identical(pma_outcome_uid(outs[[1]]), "outcome-1")
})

test_that("saving the same uid under the same name updates in place", {
  outs <- pma_upsert_outcome(NULL, "Depression response", .saved("High"),
                             "outcome-1")
  outs <- pma_upsert_outcome(outs, "Depression response", .saved("Low"),
                             "outcome-1")

  expect_length(outs, 1L)
  expect_named(outs, "Depression response")
  expect_identical(outs[["Depression response"]]$certainty, "Low")
})

test_that("saving the same uid under a different name renames the row", {
  # The pre-existing bug this exists to fix: this used to add a second row, and
  # auto-save would have added one per keystroke in the Step 2 name field.
  outs <- pma_upsert_outcome(NULL, "Depresion response", .saved(), "outcome-1")
  outs <- pma_upsert_outcome(outs, "Depression response", .saved(), "outcome-1")

  expect_length(outs, 1L)
  expect_named(outs, "Depression response")
})

test_that("a different uid adds a row", {
  outs <- pma_upsert_outcome(NULL, "Depression response", .saved(), "outcome-1")
  outs <- pma_upsert_outcome(outs, "Remission", .saved(), "outcome-2")

  expect_length(outs, 2L)
  expect_named(outs, c("Depression response", "Remission"))
})

test_that("an in-place update keeps the row where the reviewer put it", {
  # Row order is a statement about priority in a Summary of Findings table, so
  # a recompute must not move the row to the end.
  outs <- pma_upsert_outcome(NULL, "First",  .saved(), "outcome-1")
  outs <- pma_upsert_outcome(outs, "Second", .saved(), "outcome-2")
  outs <- pma_upsert_outcome(outs, "Third",  .saved(), "outcome-3")

  outs <- pma_upsert_outcome(outs, "Second", .saved("Very Low"), "outcome-2")
  expect_named(outs, c("First", "Second", "Third"))

  # ... and a rename does not move it either.
  outs <- pma_upsert_outcome(outs, "Renamed", .saved(), "outcome-2")
  expect_named(outs, c("First", "Renamed", "Third"))
})

test_that("renaming onto a name another row holds leaves one row with it", {
  # Two rows cannot share a name: grade_table() and every per-row control key
  # on names(outcomes). The older claim on the name loses it, which is what
  # the plain insert path does anyway.
  outs <- pma_upsert_outcome(NULL, "First",  .saved("High"), "outcome-1")
  outs <- pma_upsert_outcome(outs, "Second", .saved("Low"),  "outcome-2")

  outs <- pma_upsert_outcome(outs, "First", .saved("Moderate"), "outcome-2")

  expect_length(outs, 1L)
  expect_named(outs, "First")
  expect_identical(outs[["First"]]$certainty, "Moderate")
  expect_identical(pma_outcome_uid(outs[[1]]), "outcome-2")
})

test_that("an outcome banked without a uid behaves as it always did", {
  # Anything stored before the uid existed carries none, so it is matched by
  # name alone rather than silently duplicated.
  outs <- pma_upsert_outcome(NULL, "First", .saved("High"))
  expect_true(is.na(pma_outcome_uid(outs[[1]])))

  outs <- pma_upsert_outcome(outs, "First", .saved("Low"))
  expect_length(outs, 1L)
  expect_identical(outs[["First"]]$certainty, "Low")
})

test_that("a nameless outcome is refused rather than banked as a placeholder", {
  # .save_key() returns NULL for a blank Outcome name and the auto-save no-ops
  # on it; this is the backstop. The old fallback was the literal "Outcome",
  # which the outcome reset then banked as a row.
  expect_error(pma_upsert_outcome(NULL, "", .saved(), "outcome-1"), "name")
  expect_error(pma_upsert_outcome(NULL, "   ", .saved(), "outcome-1"), "name")
  expect_error(pma_upsert_outcome(NULL, NULL, .saved(), "outcome-1"), "name")
})

test_that("the saved-outcome row reports when it was last updated", {
  # attr "pma_saved_at" keeps its name - every stored object carries it - but
  # under auto-save it is when the row was last recomputed, so the label says
  # "last updated" rather than "saved at".
  g <- .saved()
  attr(g, PMA_SAVED_AT_ATTR) <- as.POSIXct("2026-08-13 09:41:00", tz = "UTC")
  expect_match(pma_outcome_updated_label(g), "^\\d{2}:\\d{2}$")
  # No stamp is not an error; the row just does not carry the line.
  expect_identical(pma_outcome_updated_label(.saved()), "")

  outs <- pma_upsert_outcome(NULL, "First", g, "outcome-1")
  html <- paste(as.character(pma_saved_outcomes_ui(outs)), collapse = "")
  expect_match(html, "last updated", fixed = TRUE)
  expect_false(grepl("saved at", html, fixed = TRUE))
})
