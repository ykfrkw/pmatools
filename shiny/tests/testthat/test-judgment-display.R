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
  expect_identical(unname(GRADE_LEVEL_SOURCE_WORDING[["not_serious"]]),
                   "not serious")
  expect_identical(unname(GRADE_LEVEL_SOURCE_WORDING[["serious"]]), "serious")
  expect_identical(unname(GRADE_LEVEL_SOURCE_WORDING[["very_serious"]]),
                   "very serious")
  expect_identical(unname(GRADE_LEVEL_SOURCE_WORDING[["extremely_serious"]]),
                   "extremely serious")

  expect_identical(pma_judgment_label("not_serious"), "Not serious")
  expect_identical(pma_judgment_label("serious"), "Serious")
  expect_identical(pma_judgment_label("very_serious"), "Very serious")
  expect_identical(pma_judgment_label("extremely_serious"),
                   "Extremely serious")
  # Legacy labels normalise rather than falling through as themselves.
  expect_identical(pma_judgment_label("no"), "Not serious")
  expect_identical(pma_judgment_label("some"), "Serious")
  expect_identical(pma_judgment_label("some_concerns"), "Serious")
  expect_identical(pma_judgment_label(NULL), "Not serious")
})

test_that("the badge keeps its severity classes while changing its words", {
  expect_match(.badge_text(pma_judgment_badge("not_serious")), "Not serious")
  expect_match(as.character(pma_judgment_badge("not_serious")), "grade-high")
  expect_match(.badge_text(pma_judgment_badge("serious")), "Serious")
  expect_match(as.character(pma_judgment_badge("serious")), "grade-low")
  expect_match(.badge_text(pma_judgment_badge("very_serious")), "Very serious")
  expect_match(as.character(pma_judgment_badge("very_serious")), "grade-vlow")
  expect_match(.badge_text(pma_judgment_badge("extremely_serious")),
               "Extremely serious")
  expect_match(as.character(pma_judgment_badge("extremely_serious")),
               "grade-vlow")

  # An unrecognised level must not abort a tab, and must not be painted as
  # "no concern" either.
  expect_silent(pma_judgment_badge("something_new"))
  expect_match(as.character(pma_judgment_badge("something_new")), "grade-low")
})

test_that("every domain override menu offers all four Core GRADE levels", {
  ch <- pma_judgment_choices()
  # The values are the stored GRADE levels. This is the assertion that keeps a
  # relabelling from becoming a behaviour change -- and that keeps the manual
  # -3 reachable, which is the ONLY way into it.
  expect_identical(unname(ch),
                   c("", "not_serious", "serious", "very_serious",
                     "extremely_serious"))
  expect_identical(names(ch)[1], "(no override)")
  expect_identical(names(ch)[-1],
                   c("Not serious (-0)", "Serious (-1)", "Very serious (-2)",
                     "Extremely serious (-3)"))

  # The Indirectness radio has no "(no override)": leaving the group
  # unselected is how the reviewer accepts the worst-case fold.
  bare <- pma_judgment_choices(include_blank = FALSE)
  expect_identical(unname(bare),
                   c("not_serious", "serious", "very_serious",
                     "extremely_serious"))
})

test_that("the verdict line states the level and the downgrade it carries", {
  expect_match(as.character(pma_domain_verdict("not_serious")), "Not serious")
  expect_match(as.character(pma_domain_verdict("not_serious")),
               "do not rate down")
  expect_match(as.character(pma_domain_verdict("serious")),
               "rate down 1 level")
  expect_match(as.character(pma_domain_verdict("very_serious")),
               "rate down 2 levels")
  expect_match(as.character(pma_domain_verdict("extremely_serious")),
               "Extremely serious")
  expect_match(as.character(pma_domain_verdict("extremely_serious")),
               "rate down 3 levels")

  # An explicit downgrade from the rated object wins over the level's default
  # (the app-level publication-bias override writes both).
  expect_match(as.character(pma_domain_verdict("not_serious", -1L)),
               "rate down 1 level")
  # ... and an unusable one falls back rather than aborting.
  expect_match(as.character(pma_domain_verdict("not_serious", NA)),
               "do not rate down")
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

test_that("the on-screen prose helpers are gone and stay gone", {
  # pma_notes_collapse() parked the machine-generated note under every domain
  # verdict; pma_how_collapse() wrapped the five EDU_COPY `how` bodies; and
  # pma_help() was a Bootstrap tooltip nothing ever initialised. All three are
  # deleted (v0.5.1). The note itself is NOT lost - it still travels into
  # evidence_profile() and the exported .docx - so a re-introduced helper here
  # would be a second, screen-only copy of a record that already ships.
  for (fn in c("pma_notes_collapse", "pma_how_collapse", "pma_help")) {
    expect_false(exists(fn, mode = "function"), info = fn)
  }
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

test_that("the required mapping selects depend on the outcome type", {
  expect_identical(pma_step2_mapping_required("binary"),
                   c("col_studlab", "col_treat", "col_n", "col_event"))
  expect_identical(pma_step2_mapping_required("continuous"),
                   c("col_studlab", "col_treat", "col_n",
                     "col_mean", "col_sd"))
  # The ids the client is told to manage are fixed, and cover both types: an
  # id dropped from the message keeps its cached mark instead of losing it.
  expect_true(all(pma_step2_mapping_required("binary") %in%
                    PMA_STEP2_MAPPING_ALL))
  expect_true(all(pma_step2_mapping_required("continuous") %in%
                    PMA_STEP2_MAPPING_ALL))
})

test_that("pma_step2_mapping_unset names the blank selects for that type", {
  mapped <- list(col_studlab = "study", col_treat = "arm", col_n = "n",
                 col_event = "events", col_mean = "", col_sd = "")
  # A binary outcome does not care that mean and sd are unmapped.
  expect_identical(pma_step2_mapping_unset("binary", mapped), character(0))
  expect_identical(pma_step2_mapping_unset("continuous", mapped),
                   c("col_mean", "col_sd"))

  # "(select)" is submitted as "", which is how an unmapped select reads; NULL
  # is how it reads before the server has populated it at all.
  expect_identical(pma_step2_mapping_unset("binary", list()),
                   c("col_studlab", "col_treat", "col_n", "col_event"))
  expect_identical(
    pma_step2_mapping_unset("binary", utils::modifyList(mapped,
                                                        list(col_n = NA))),
    "col_n")
})
