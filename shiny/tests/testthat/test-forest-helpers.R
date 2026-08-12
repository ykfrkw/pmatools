# Forest-plot display helpers (R/ui_helpers.R).
#
# These decide what the axis says and how much white space surrounds the pooled
# row. A wrong "Favors ..." side is a reversed conclusion on the printed plot,
# so the direction mapping gets an explicit test on both vocabularies.

test_that("pma_favors_labels() puts the intervention on the side that favours it", {
  # "undesirable" = a smaller value is worse (response, remission), so a larger
  # effect favours the intervention: intervention on the right.
  expect_equal(pma_favors_labels("undesirable", "CBT-I", "Control"),
               list(left = "Favors Control", right = "Favors CBT-I"))
  # "desirable" = a smaller value is better (mortality, symptom score): mirrored.
  expect_equal(pma_favors_labels("desirable", "CBT-I", "Control"),
               list(left = "Favors CBT-I", right = "Favors Control"))
})

test_that("pma_favors_labels() prefills nothing when it cannot know the side", {
  none <- list(left = "", right = "")
  expect_equal(pma_favors_labels(NULL, "CBT-I", "Control"), none)
  expect_equal(pma_favors_labels(NA, "CBT-I", "Control"), none)
  expect_equal(pma_favors_labels("", "CBT-I", "Control"), none)
  expect_equal(pma_favors_labels("something else", "CBT-I", "Control"), none)
  # A missing arm name is equally disqualifying: better blank than half a label.
  expect_equal(pma_favors_labels("undesirable", "", "Control"), none)
  expect_equal(pma_favors_labels("undesirable", "CBT-I", NULL), none)
  expect_equal(pma_favors_labels("undesirable", "CBT-I", NA), none)
})

test_that("pma_favors_labels() trims what the user typed", {
  expect_equal(pma_favors_labels(" undesirable ", "  CBT-I ", " Control  "),
               list(left = "Favors Control", right = "Favors CBT-I"))
})

test_that("pma_addrow_above() falls back to the historical one blank row", {
  expect_equal(pma_addrow_above(0), 0)
  expect_equal(pma_addrow_above(3), 3)
  expect_equal(pma_addrow_above("2"), 2)
  # Blank / invalid -> the default, because there is no auto mode above.
  expect_equal(pma_addrow_above(NULL), 1)
  expect_equal(pma_addrow_above(NA), 1)
  expect_equal(pma_addrow_above(-1), 1)
  expect_equal(pma_addrow_above(Inf), 1)
  expect_equal(pma_addrow_above("abc"), 1)
  expect_equal(pma_addrow_above(c(1, 2)), 1)
  expect_equal(pma_addrow_above(NA, default = 0), 0)
})

test_that("pma_addrow_below() expresses 'let plot_forest() decide' as NULL", {
  # 0 is a real answer (switch the heuristic off), not a blank.
  expect_equal(pma_addrow_below(0), 0)
  expect_equal(pma_addrow_below(2), 2)
  expect_equal(pma_addrow_below("2"), 2)
  expect_null(pma_addrow_below(NULL))
  expect_null(pma_addrow_below(NA))
  expect_null(pma_addrow_below(-1))
  expect_null(pma_addrow_below(Inf))
  expect_null(pma_addrow_below("abc"))
  expect_null(pma_addrow_below(c(1, 2)))
})

test_that("the forest id helpers keep Step 2 and Step 3 apart", {
  # Step 2 (no prefix) and Step 3 (prefixed) genuinely disagree on the
  # blank-row suffix; that asymmetry is the reason these helpers exist, and
  # pma_forest_display_panel() must not paper over it by inventing ids.
  expect_equal(unname(pma_forest_label_ids(NULL)["title"]), "forest_title")
  expect_equal(unname(pma_forest_label_ids("rob")["title"]), "rob_title")
  expect_equal(unname(pma_forest_label_ids("")["favors_left"]), "favors_left")
  expect_equal(unname(pma_forest_label_ids("incon")["favors_left"]),
               "incon_favors_left")

  expect_equal(unname(pma_forest_addrow_ids(NULL)["above"]),
               "addrows_above_overall")
  expect_equal(unname(pma_forest_addrow_ids(NULL)["below"]),
               "addrows_below_overall")
  expect_equal(unname(pma_forest_addrow_ids("rob")["above"]),
               "rob_addrows_above")
  expect_equal(unname(pma_forest_addrow_ids("rob")["below"]),
               "rob_addrows_below")
})
