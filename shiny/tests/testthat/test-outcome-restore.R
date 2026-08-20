# Putting Step 3's answers back after app.R rebuilds the step
# (R/outcome_provenance.R).
#
# app.R renders output$step_body from step3_ui() on every entry, so leaving
# Step 3 and returning destroys every widget on it. Without this the reviewer
# lost their control-group override, its rationale and every free-text answer,
# silently. The property that must not break while fixing that is the one the
# freshness guard exists for: an answer given for a PREVIOUS outcome must never
# be reinstated as if it belonged to the one now open.

test_that("pma_restorable_input_ids() covers Step 3 and excludes what it must", {
  ids <- pma_restorable_input_ids()

  # The answers that were being lost.
  expect_true(all(c("threshold_baseline_input", "threshold_baseline_rationale",
                    "threshold_abs", "threshold_ratio", "threshold_cont",
                    "baseline_risk_chinn", "responder_p0_rationale",
                    "threshold_label", "rob_override", "rob_override_rationale",
                    "indir_rationale", "pubias_fa_rationale", "other_text")
                  %in% ids))

  # Confirmations are deliberately NOT restored: a confirmation says "I have
  # looked at what is on screen", so it is re-armed and re-ticked.
  expect_length(intersect(ids, PMA_OUTCOME_CONFIRM_IDS), 0L)

  # Step 2's own identity fields are not on screen when Step 3 is rebuilt.
  expect_length(intersect(ids, PMA_OUTCOME_INPUT_IDS$identity), 0L)

  # Every id restored is an id the freshness guard already stamps, or it would
  # be restored with no way to tell which outcome it belonged to.
  expect_true(all(ids %in% pma_outcome_input_ids()))
  expect_false(anyDuplicated(ids) > 0L)
})

test_that("pma_restorable_value() refuses answers from another outcome", {
  # This is the guard. A stale answer is never put back.
  expect_false(pma_restorable_value("some rationale", stamp = 1L, gen = 2L))
  expect_false(pma_restorable_value(210, stamp = 1L, gen = 2L))
  # An answer never stamped at all (NULL) is not this outcome's either.
  expect_false(pma_restorable_value("text", stamp = NULL, gen = 1L))

  expect_true(pma_restorable_value("some rationale", stamp = 1L, gen = 1L))
  expect_true(pma_restorable_value(210, stamp = 3L, gen = 3L))
})

test_that("pma_restorable_value() skips answers with nothing to restore", {
  expect_false(pma_restorable_value(NULL, 1L, 1L))
  expect_false(pma_restorable_value(character(0), 1L, 1L))
  expect_false(pma_restorable_value(NA, 1L, 1L))
  expect_false(pma_restorable_value(NA_character_, 1L, 1L))
  expect_false(pma_restorable_value(NA_real_, 1L, 1L))
  # A blank or whitespace-only rationale is not an answer.
  expect_false(pma_restorable_value("", 1L, 1L))
  expect_false(pma_restorable_value("   ", 1L, 1L))

  # Values that ARE answers, including the falsy-looking ones.
  expect_true(pma_restorable_value(0, 1L, 1L))
  expect_true(pma_restorable_value(FALSE, 1L, 1L))
  expect_true(pma_restorable_value("no", 1L, 1L))
  # Multi-element answers (a multi-select) are restorable as they stand.
  expect_true(pma_restorable_value(c("a", "b"), 1L, 1L))
})
