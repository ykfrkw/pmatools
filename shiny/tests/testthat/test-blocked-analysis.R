# The "why is there no analysis?" sentence (R/step3_threshold.R).
#
# Step 2 records what was missing in state$ma_blocked; four Step 3 outputs read
# it -- final_certainty, sof_preview, cert_incomplete_banner and
# outcome_name_echo. Before 0.5.1 the first two printed "Run analysis and
# configure domains." / "..." at a reviewer who HAD run one and then emptied a
# required field, while outcome_name_echo went on showing the outcome name
# nobody had typed any more. The builders below are pure, so the wording the
# four share can be pinned without a session.

test_that("no record means no message, so the idle placeholder still shows", {
  expect_null(step3_blocked_message(NULL))
  expect_null(step3_blocked_message(character(0)))
  # Blanks are not fields. A vector of empty strings must not produce
  # "Step 2 is missing: ." on screen.
  expect_null(step3_blocked_message(c("", NA_character_)))
  expect_equal(step3_blocked_fields(NULL), character(0))
})

test_that("the message names the missing fields and says where to go", {
  msg <- step3_blocked_message("Outcome name")
  expect_equal(
    msg,
    "No analysis. Step 2 is missing: Outcome name. Go back to Step 2 and complete it."
  )
})

test_that("several missing fields are listed once each, in order", {
  msg <- step3_blocked_message(c("Outcome name", "Events column",
                                 "Outcome name"))
  expect_match(msg, "Step 2 is missing: Outcome name, Events column.",
               fixed = TRUE)
})

test_that("the identity subset is exactly the Step 2 outcome-identity fields", {
  blocked <- c(unname(STEP2_IDENTITY_FIELD_LABELS), "Events column",
               "Sample size column")
  expect_equal(step3_blocked_identity(blocked),
               unname(STEP2_IDENTITY_FIELD_LABELS))
  # A purely column-mapping block leaves the outcome-name echo alone.
  expect_equal(step3_blocked_identity(c("Events column")), character(0))
  expect_equal(step3_blocked_identity(NULL), character(0))
})

test_that("the two identity labels are the ones Step 2 records", {
  # step2_ma.R writes these strings into state$ma_blocked by reading this
  # constant, and step3_blocked_identity() matches on it. If the constant is
  # ever inlined again in one of the two places, the echo silently stops
  # noticing a cleared outcome name.
  expect_equal(sort(names(STEP2_IDENTITY_FIELD_LABELS)),
               c("outcome_name", "small_values"))
  expect_equal(STEP2_IDENTITY_FIELD_LABELS[["outcome_name"]], "Outcome name")
})

test_that("column names are printed as their Step 2 labels", {
  expect_equal(step2_column_labels(c("studlab", "event", "sd")),
               c("Study label column", "Events column", "SD column"))
  # An unmapped column is passed through rather than dropped: a new required
  # column should show up as itself, not vanish from the sentence.
  expect_equal(step2_column_labels("brand_new"), "brand_new")
  expect_equal(step2_column_labels(NULL), character(0))
})

test_that("a column block reads as a full sentence end to end", {
  # The shape step2_ma.R actually builds: mapped column labels first, then the
  # identity fields.
  blocked <- c(step2_column_labels(c("event")),
               STEP2_IDENTITY_FIELD_LABELS[["small_values"]])
  expect_equal(
    step3_blocked_message(blocked),
    paste0("No analysis. Step 2 is missing: Events column, ",
           "Direction (smaller = favorable?). Go back to Step 2 and ",
           "complete it.")
  )
})
