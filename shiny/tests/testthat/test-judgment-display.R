# The app's display vocabulary and the shared domain-evaluation shape
# (R/ui_helpers.R).
#
# The bug these pin down: the badge used to print "No concern" / "Some
# concerns" / "Serious" from a hand-written switch, while the Evidence Profile
# printed Core GRADE's own "not serious" / "serious" / "very serious" from a
# second one. The same judgment therefore read differently in the app and in
# the exported table, and worse, "Serious" meant -2 in the badge and -1 in the
# table. Both now go through .grade_level_wording() in the package's
# R/utils.R, which helper-app.R sources.

.badge_text <- function(x) {
  gsub("<[^>]*>", "", as.character(x))
}

test_that("the app reads its wording from the package constant", {
  expect_identical(unname(GRADE_LEVEL_SOURCE_WORDING[["no"]]), "not serious")
  expect_identical(unname(GRADE_LEVEL_SOURCE_WORDING[["some_concerns"]]),
                   "serious")
  expect_identical(unname(GRADE_LEVEL_SOURCE_WORDING[["serious"]]),
                   "very serious")

  expect_identical(pma_judgment_label("no"), "Not serious")
  expect_identical(pma_judgment_label("some_concerns"), "Serious")
  expect_identical(pma_judgment_label("serious"), "Very serious")
  # Legacy labels normalise rather than falling through as themselves.
  expect_identical(pma_judgment_label("some"), "Serious")
  expect_identical(pma_judgment_label("very_serious"), "Very serious")
  expect_identical(pma_judgment_label(NULL), "Not serious")
})

test_that("the badge keeps its severity classes while changing its words", {
  expect_match(.badge_text(pma_judgment_badge("no")), "Not serious")
  expect_match(as.character(pma_judgment_badge("no")), "grade-high")
  expect_match(.badge_text(pma_judgment_badge("some_concerns")), "Serious")
  expect_match(as.character(pma_judgment_badge("some_concerns")), "grade-low")
  expect_match(.badge_text(pma_judgment_badge("serious")), "Very serious")
  expect_match(as.character(pma_judgment_badge("serious")), "grade-vlow")

  # An unrecognised level must not abort a tab.
  expect_silent(pma_judgment_badge("something_new"))
})

test_that("the override menus relabel without moving any value", {
  ch <- pma_judgment_choices()
  # VALUES are the internal levels and nothing downstream is aware the labels
  # changed. This is the assertion that keeps a relabelling from becoming a
  # behaviour change.
  expect_identical(unname(ch), c("", "no", "some_concerns", "serious"))
  expect_identical(names(ch)[1], "(no override)")
  expect_identical(names(ch)[-1],
                   c("Not serious (-0)", "Serious (-1)", "Very serious (-2)"))

  # The Indirectness radio has no "(no override)": leaving the group
  # unselected is how the reviewer accepts the worst-case fold.
  bare <- pma_judgment_choices(include_blank = FALSE)
  expect_identical(unname(bare), c("no", "some_concerns", "serious"))
})

test_that("the verdict line states the level and the downgrade it carries", {
  expect_match(as.character(pma_domain_verdict("no")), "Not serious")
  expect_match(as.character(pma_domain_verdict("no")), "do not rate down")
  expect_match(as.character(pma_domain_verdict("some_concerns")),
               "rate down 1 level")
  expect_match(as.character(pma_domain_verdict("serious")),
               "rate down 2 levels")

  # An explicit downgrade from the rated object wins over the level's default
  # (the app-level publication-bias override writes both).
  expect_match(as.character(pma_domain_verdict("no", -1L)), "rate down 1 level")
  # ... and an unusable one falls back rather than aborting.
  expect_match(as.character(pma_domain_verdict("no", NA)), "do not rate down")
  expect_match(as.character(pma_domain_verdict("mystery", NULL)),
               "do not rate down")
})

test_that("the facts list brings the numbers forward, in the order asked", {
  facts <- data.frame(
    key     = c("i2", "tau2", "zone_counts"),
    label   = c("I-squared", "Tau-squared", "Zone counts"),
    value   = c("70.0%", "0.0400", "1 above, 1 within, 1 below"),
    numeric = c(70, 0.04, NA_real_),
    stringsAsFactors = FALSE
  )

  html <- as.character(pma_facts_list(facts))
  expect_match(html, "I-squared")
  expect_match(html, "1 above, 1 within, 1 below")

  # `keys` selects AND orders; a key the assessor did not emit is skipped
  # rather than rendered blank.
  picked <- as.character(pma_facts_list(facts,
                                        keys = c("zone_counts", "not_a_key",
                                                 "i2")))
  expect_lt(regexpr("Zone counts", picked, fixed = TRUE),
            regexpr("I-squared", picked, fixed = TRUE))
  expect_false(grepl("Tau-squared", picked, fixed = TRUE))

  expect_lte(length(gregexpr("<dt", as.character(
    pma_facts_list(facts, max_rows = 2L)))[[1]]), 2L)

  # Nothing to show is NULL, so a caller can drop it straight into a tagList.
  expect_null(pma_facts_list(NULL))
  expect_null(pma_facts_list(facts[0, ]))
  expect_null(pma_facts_list(data.frame(a = 1)))
  expect_null(pma_facts_list(facts, keys = "no_such_key"))
})

test_that("the verbatim note is collapsed, never dropped", {
  html <- as.character(pma_notes_collapse("AUTO Step 1: ... | I2 = 70.0%"))
  expect_match(html, "<details")
  expect_match(html, "Full reasoning")
  # The whole string survives: this is the authoritative record of why the
  # domain was rated as it was, and it has to stay reachable.
  expect_match(html, "AUTO Step 1", fixed = TRUE)
  expect_match(html, "I2 = 70.0%", fixed = TRUE)

  expect_null(pma_notes_collapse(NULL))
  expect_null(pma_notes_collapse(""))
  expect_null(pma_notes_collapse("   "))
})

# ----- Step 2 required fields ---------------------------------------------
# The rule the marks are painted from. Which TIER they are painted in (muted
# from the first render, destructive only after a failed Next) is the server's
# `armed` flag and the two-tier CSS; this is only the "what is still blank"
# half, which is what can be tested without a session.

test_that("pma_step2_required_unset names exactly the blank required fields", {
  expect_identical(pma_step2_required_unset(NULL, NULL),
                   c("outcome_name", "small_values"))
  expect_identical(pma_step2_required_unset("", ""),
                   c("outcome_name", "small_values"))
  # Whitespace is not an outcome name.
  expect_identical(pma_step2_required_unset("   ", "undesirable"),
                   "outcome_name")
  expect_identical(pma_step2_required_unset("Depression response", NULL),
                   "small_values")
  expect_identical(
    pma_step2_required_unset("Depression response", "undesirable"),
    character(0))

  # radioButtons(selected = character(0)) reports a zero-length value; NA
  # comes back from an emptied text field on some paths. Neither counts.
  expect_true("small_values" %in%
                pma_step2_required_unset("x", character(0)))
  expect_true("outcome_name" %in%
                pma_step2_required_unset(NA_character_, "undesirable"))

  # The ids reported are the ids the JS is told to manage, so the two cannot
  # drift apart.
  expect_true(all(pma_step2_required_unset(NULL, NULL) %in%
                    PMA_STEP2_REQUIRED))
})
