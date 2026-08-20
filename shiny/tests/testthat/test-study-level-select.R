# The per-study RoB / Indirectness dropdowns on Step 3 (R/judgment_display.R).
#
# The bug these pin down: the cell was free text, with a caption telling the
# reviewer to type low / some / high. A typo landed the study in the "unknown"
# stratum, where rob_strata() warns and the app showed the warning nowhere.
#
# The second bug, one layer up: the RoB labels were pmatools' invention, and
# the package advertised four of them as "Cochrane RoB 2.0". RoB 2 defines
# three. Indirectness reuses the same three-value scale but has no such
# instrument behind it, so its labels must NOT read like a published judgment.

test_that("the Risk of Bias dropdown offers RoB 2's three judgments, verbatim", {
  expect_identical(names(PMA_ROB2_CHOICES),
                   c("Low risk of bias", "Some concerns", "High risk of bias"))
  expect_identical(unname(PMA_ROB2_CHOICES), c("low", "some", "high"))
})

test_that("Indirectness does not borrow a risk-of-bias vocabulary", {
  # Same three strata, deliberately different words: there is no RoB 2 for
  # indirectness, and matching labels would claim one existed.
  expect_identical(unname(PMA_INDIRECTNESS_CHOICES), c("low", "some", "high"))
  expect_length(intersect(names(PMA_INDIRECTNESS_CHOICES),
                          names(PMA_ROB2_CHOICES)), 0L)
  expect_true(all(grepl("indirectness", names(PMA_INDIRECTNESS_CHOICES),
                        ignore.case = TRUE)))
})

test_that("both scales store what the rest of the app already exchanges", {
  # The bulk buttons, Step 1's uploaded-column mapping and grade_meta() all
  # speak "low" / "some" / "high". A dropdown that stored its own labels
  # instead would need a second translation table to keep in sync.
  expect_identical(unname(PMA_ROB2_CHOICES), unname(PMA_INDIRECTNESS_CHOICES))
})

test_that("a cell renders one <select> carrying its own row index", {
  html <- pma_study_level_select(3L, "some", "step3_rob_choice",
                                 PMA_ROB2_CHOICES)
  expect_match(html, "<select", fixed = TRUE)
  expect_match(html, "data-input=\"step3_rob_choice\"", fixed = TRUE)
  # The row travels with the rendered cell rather than being recovered from
  # DT's `col` index, which counts hidden columns.
  expect_match(html, "data-row=\"3\"", fixed = TRUE)

  # Exactly four options: the three judgments plus an explicit unset.
  expect_length(gregexpr("<option ", html, fixed = TRUE)[[1]], 4L)
  expect_match(html, ">(not set)</option>", fixed = TRUE)
  expect_match(html, "value=\"some\" selected", fixed = TRUE)
})

test_that("an unset study selects the blank option, not the first judgment", {
  for (blank in list(NA_character_, "", NULL)) {
    html <- pma_study_level_select(1L, blank, "step3_rob_choice",
                                   PMA_ROB2_CHOICES)
    expect_match(html, "value=\"\" selected", fixed = TRUE)
    expect_false(grepl("value=\"low\" selected", html, fixed = TRUE))
  }
})

test_that("nothing outside the three values can be selected", {
  # The whole point of the control. A value the app does not offer renders as
  # no selection at all rather than as an option the reviewer could re-pick.
  html <- pma_study_level_select(1L, "hihg", "step3_rob_choice",
                                 PMA_ROB2_CHOICES)
  expect_length(gregexpr(" selected", html, fixed = TRUE)[[1]], 1L)
  expect_match(html, "value=\"\" selected", fixed = TRUE)
})

test_that("the column is one select per study, in row order", {
  col <- pma_study_level_column(c("low", NA, "high"), "step3_indir_choice",
                                PMA_INDIRECTNESS_CHOICES)
  expect_length(col, 3L)
  expect_match(col[[1]], "data-row=\"1\"", fixed = TRUE)
  expect_match(col[[2]], "data-row=\"2\"", fixed = TRUE)
  expect_match(col[[3]], "data-row=\"3\"", fixed = TRUE)
  expect_match(col[[1]], "value=\"low\" selected", fixed = TRUE)
  expect_match(col[[2]], "value=\"\" selected", fixed = TRUE)
  expect_match(col[[3]], "value=\"high\" selected", fixed = TRUE)
  expect_match(col[[3]], "High indirectness", fixed = TRUE)

  # An empty table is an empty column, not one stray <select>.
  expect_identical(pma_study_level_column(character(0), "x", PMA_ROB2_CHOICES),
                   character(0))
})

test_that("one delegated handler serves every dropdown on the step", {
  js <- as.character(pma_study_level_script())
  expect_match(js, "select.pma-level-select", fixed = TRUE)
  expect_match(js, "Shiny.setInputValue", fixed = TRUE)
  expect_match(js, "dataset.input", fixed = TRUE)
  expect_match(js, "dataset.row", fixed = TRUE)
  expect_match(js, "priority: 'event'", fixed = TRUE)

  # app.R rebuilds the Step 3 body on every step change and Shiny re-executes
  # the inline script in what it inserts. Without the namespaced off(), a
  # 3 -> 2 -> 3 round trip would stack handlers and report each change twice.
  expect_match(js, "off('change.pmaLevel')", fixed = TRUE)
  expect_match(js, "on('change.pmaLevel'", fixed = TRUE)
})

test_that("step3_ui registers the handler exactly once", {
  src <- readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                   warn = FALSE)
  expect_identical(sum(grepl("pma_study_level_script()", src, fixed = TRUE)),
                   1L)

  # And neither editor is DT-editable any more: a surviving `editable=` would
  # put the free-text cell back beside the dropdown.
  expect_false(any(grepl("step3_rob_editor_cell_edit", src, fixed = TRUE)))
  expect_false(any(grepl("step3_indir_editor_cell_edit", src, fixed = TRUE)))
})
