# Step 3 on a rare-event analysis: the app's half (shiny/SPEC.md 3.4.14).
#
# The domain arithmetic is the package's and is tested there
# (tests/testthat/test-rare_step3.R). What is here is the two things the app
# owns: which Figure 5 node the wizard puts on screen, and which per-N
# denominator the tab opens on.
#
# The property behind both: rare mode changes what is computed and what is
# said, and never changes a rating by itself. Nothing below feeds
# grade_meta(); the wizard's ROUTE decides which question is asked, and the
# per-N unit is display only.

# --------------------------------------------------------------------------
# Publication bias: Figure 5's k < 10 route, whatever k is
# --------------------------------------------------------------------------
# Egger's regression loses validity on sparse binary data, and Fig 5 already
# has the branch for "Egger is not available to you" - the one k < 10 takes.
# The wizard has to take the SAME branch assess_pubias() takes, or the reviewer
# answers a question the rating then ignores.

test_that("the Q2 gate answers 'not feasible' for rare data at any k", {
  # Without the flag, k decides, exactly as before.
  expect_true(step3_pubias_statistical(14))
  expect_true(step3_pubias_statistical(10))
  expect_false(step3_pubias_statistical(9))

  # With it, the study count stops deciding.
  expect_false(step3_pubias_statistical(14, rare_flow = TRUE))
  expect_false(step3_pubias_statistical(10, rare_flow = TRUE))
  expect_false(step3_pubias_statistical(9,  rare_flow = TRUE))

  # A non-TRUE value is not a rare analysis: the flag arrives from
  # state$rare_mode_active, which is NULL before Step 2 has run.
  expect_true(step3_pubias_statistical(14, rare_flow = NULL))
  expect_true(step3_pubias_statistical(14, rare_flow = FALSE))
})

test_that("the wizard opens Q4 rather than Q3 on a rare analysis with k >= 10", {
  answered <- list(small_industry = "no", registry_complete = "no")

  expect_identical(
    do.call(step3_pubias_node, c(answered, list(k = 14))),
    "q3")
  expect_identical(
    do.call(step3_pubias_node, c(answered, list(k = 14, rare_flow = TRUE))),
    "q4")

  # And the funnel answer no longer settles the wizard, because the funnel
  # question is not on the path: only the Q4 answer reaches "result".
  expect_identical(
    do.call(step3_pubias_node,
            c(answered, list(funnel_asymmetry = "egger", k = 14,
                             rare_flow = TRUE))),
    "q4")
  expect_identical(
    do.call(step3_pubias_node,
            c(answered, list(unpublished = "no", k = 14, rare_flow = TRUE))),
    "result")
})

test_that("the reachable path swaps q3 for q4 and stays four nodes long", {
  regular <- step3_pubias_reachable("no", "no", k = 14)
  rare    <- step3_pubias_reachable("no", "no", k = 14, rare_flow = TRUE)

  expect_identical(regular, c("q1", "extra", "q3", "result"))
  expect_identical(rare,    c("q1", "extra", "q4", "result"))
})

test_that("the earlier nodes are untouched: rare data does not skip Q1", {
  # Q1 is Fig 5's entry node and is terminal on "yes" whatever the data looks
  # like. Rare mode routes ONE gate and no others.
  expect_identical(step3_pubias_node(rare_flow = TRUE), "q1")
  expect_identical(
    step3_pubias_node(small_industry = "yes", k = 14, rare_flow = TRUE),
    "result")
  expect_identical(
    step3_pubias_node(small_industry = "no", k = 14, rare_flow = TRUE),
    "extra")
  expect_identical(
    step3_pubias_node(small_industry = "no", registry_complete = "yes",
                      k = 14, rare_flow = TRUE),
    "result")
})

test_that("the lit chart takes an edge Figure 5 already has - no new node", {
  ids_regular <- step3_pubias_flow_ids("no", "no", unpublished = "no", k = 14)
  ids_rare    <- step3_pubias_flow_ids("no", "no", unpublished = "no", k = 14,
                                       rare_flow = TRUE)

  expect_true("pma-pubias-edge-q2-yes" %in% ids_regular)
  expect_true("pma-pubias-edge-q2-no"  %in% ids_rare)
  expect_true("pma-pubias-node-q4"     %in% ids_rare)
  expect_false("pma-pubias-node-q3"    %in% ids_rare)

  # Every id it lights is one the k < 10 route already lights, spelled out
  # rather than checked against the package's node vocabulary: that vector
  # lives in R/domain_pubias.R, which the app's helper does not source, and
  # tests/testthat/test-rare_step3.R holds the package side to it.
  expect_identical(
    ids_rare,
    step3_pubias_flow_ids("no", "no", unpublished = "no", k = 4))
})

test_that("the Q2 line names k as well as the reason", {
  # A reviewer looking at 14 studies and a registry question has to be able to
  # see that the study count was not what sent them there.
  line <- step3_pubias_k_line(14, rare_flow = TRUE)
  expect_match(line, "not feasible", fixed = TRUE)
  expect_match(line, "rare-event analysis", fixed = TRUE)
  expect_match(line, "14", fixed = TRUE)
  expect_false(grepl("< 10", line, fixed = TRUE))

  expect_match(step3_pubias_k_line(14), ">= 10", fixed = TRUE)
  expect_match(step3_pubias_k_line(4), "< 10", fixed = TRUE)
})

# --------------------------------------------------------------------------
# Absolute effects: one denominator per outcome, chosen from the data
# --------------------------------------------------------------------------
# Core GRADE 7 presents absolute effects per 1,000, which is the right unit
# for an outcome that happens. At a control-arm event rate of 0.05% it prints
# "0 per 1,000" against "0 per 1,000" and a difference of "0 per 1,000".

test_that("the per-N unit is seeded from the control-arm event rate", {
  # 12 per 1,000 needs no help.
  expect_identical(step3_rare_per_seed(0.012), 1000L)
  # 5 per 10,000: per 1,000 would round to 0.
  expect_identical(step3_rare_per_seed(0.0005), 10000L)
  # 2 per 100,000.
  expect_identical(step3_rare_per_seed(0.00002), 100000L)
  # Below the largest unit offered, the largest unit is the best available.
  expect_identical(step3_rare_per_seed(0.0000001), 100000L)
})

test_that("the seed is the SMALLEST unit that still shows a whole event", {
  # Exactly 1 per 1,000 is a figure, so the unit does not grow.
  expect_identical(step3_rare_per_seed(0.001), 1000L)
  # Just under it does.
  expect_identical(step3_rare_per_seed(0.0009), 10000L)
})

test_that("the seed never goes BELOW the default", {
  # A rare outcome is not a reason to move from 1,000 to 100: 100 is the
  # coarser unit, and it would make the problem worse.
  expect_identical(step3_rare_per_seed(0.5), 1000L)
  expect_identical(step3_rare_per_seed(1), 1000L)
})

test_that("an unusable event rate leaves the default alone", {
  expect_identical(step3_rare_per_seed(NULL), 1000L)
  expect_identical(step3_rare_per_seed(NA_real_), 1000L)
  expect_identical(step3_rare_per_seed(0), 1000L)
  expect_identical(step3_rare_per_seed(-0.01), 1000L)
  expect_identical(step3_rare_per_seed(c(0.001, 0.002)), 1000L)
})

test_that("the two large units are real units, not just seeds", {
  # Everything that formats or stores a rate has to accept them, or the seed
  # would put the tab into a unit its own arithmetic rejects and silently
  # snap back to 1,000.
  expect_identical(step3_per_unit("10000"), 10000L)
  expect_identical(step3_per_unit(100000), 100000L)
  expect_identical(step3_per_label(0.5, 10000), "5 per 10,000")
  expect_identical(step3_per_label(0.02, 100000), "2 per 100,000")
  expect_identical(step3_per_unit_label(10000), "per 10,000")

  # The round trip that every threshold box depends on: internal storage stays
  # per 1,000 whatever is on screen.
  for (per in STEP3_PER_UNITS) {
    for (v in c(0.02, 0.5, 3.3, 156)) {
      expect_equal(step3_from_per(step3_to_per(v, per), per), v)
    }
  }
})

test_that("quantising at a large unit keeps a whole number of events", {
  # The numericInput offers integers in the DISPLAYED unit, so the stored
  # per-1,000 value has to land on that grid however fine it is.
  for (per in STEP3_PER_UNITS) {
    for (v in c(0.037, 0.52, 3.31, 156.4)) {
      shown <- step3_to_per(step3_quantise_per1000(v, per), per)
      expect_equal(shown, round(shown))
    }
  }
})

test_that("every offered unit has a radio label", {
  choices <- step3_per_choices()
  expect_identical(unname(choices), as.character(STEP3_PER_UNITS))
  expect_identical(names(choices)[[1]], "100 patients")
  expect_identical(names(choices)[[4]], "100,000 patients")
})

test_that("the radio offers two units normally and four on rare-event data", {
  # ACCEPTED is not OFFERED. 10,000 and 100,000 answer a question only a
  # rare-event analysis asks, and a reviewer pooling ordinary event rates
  # should not read past them to reach the two they pick between -- but
  # narrowing what step3_per_unit() ACCEPTS instead would make a seeded 10,000
  # fail validation and snap the exported denominator back to 1,000 behind the
  # reviewer's back.
  expect_identical(step3_per_units_offered(rare = FALSE), c(100L, 1000L))
  expect_identical(step3_per_units_offered(rare = TRUE), STEP3_PER_UNITS)

  # Still accepted either way: the offered list is a display decision only.
  expect_identical(step3_per_unit(10000), 10000L)
})

test_that("the offered units always contain the unit currently in force", {
  # An analysis can stop being rare -- a column remapped, a study dropped --
  # while a seeded 10,000 is still the unit on screen. A radio rendered
  # without its own selected value shows no selection at all and pushes a
  # different unit back to the server on the next rebuild, which would move
  # the exported denominator without anyone touching the control.
  offered <- step3_per_units_offered(rare = FALSE, selected = 10000L)
  expect_true(10000L %in% offered)
  expect_identical(offered, c(100L, 1000L, 10000L))

  # Ascending and unique, so the labels read in order and no unit is listed
  # twice when the selected value is already common.
  common <- step3_per_units_offered(rare = FALSE, selected = 100L)
  expect_identical(common, c(100L, 1000L))
  expect_false(any(duplicated(step3_per_units_offered(rare = TRUE,
                                                      selected = 1000L))))

  # A junk selection falls back through step3_per_unit() rather than widening
  # the list with something the rest of Step 3 would reject.
  expect_identical(step3_per_units_offered(rare = FALSE, selected = 7L),
                   c(100L, 1000L))
})

test_that("every offered unit still has a radio label", {
  # step3_per_choices() is handed the offered set now, not always the accepted
  # one, so the label builder has to survive a short vector.
  choices <- step3_per_choices(step3_per_units_offered(rare = FALSE))
  expect_identical(unname(choices), c("100", "1000"))
  expect_identical(names(choices), c("100 patients", "1,000 patients"))
})

# --------------------------------------------------------------------------
# The method is named where the rating is set up
# --------------------------------------------------------------------------

test_that("the Configuration block names the method and the absent correction", {
  html <- as.character(.rare_method_block(
    "BB_CR", "OR",
    list(k = 12L, both_arms_events_k = 8L, zero_cell_k = 4L,
         total_events = 41, event_rate_overall = 0.0074)))

  expect_match(html, "rare-event workflow", fixed = TRUE)
  expect_match(html, "Beta-binomial with correlated responses", fixed = TRUE)
  # Requirement 6: the 0.5 that was never added leaves no other trace.
  expect_match(html, "No continuity correction", fixed = TRUE)
  # The counts a reviewer weighs the method against.
  expect_match(html, "8 of 12 studies", fixed = TRUE)
  expect_match(html, "0.74%", fixed = TRUE)
})

test_that("the block still renders without diagnostics or a known method", {
  expect_silent(as.character(.rare_method_block("MH_no_cc", "OR", NULL)))
  html <- as.character(.rare_method_block(NULL, "OR", NULL))
  expect_match(html, "rare-event workflow", fixed = TRUE)
})
