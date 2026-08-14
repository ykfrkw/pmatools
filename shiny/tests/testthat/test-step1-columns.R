# test-step1-columns.R - Step 1's detected-columns strip and the preview's
# analysis-column set.
#
# The strip is the whole answer to "did my data load correctly?", so what it
# colours green has to be what ingest_data() actually recognised. It is built
# on detect_column_roles() (sourced by helper-app.R from the package) and
# pma_column_role_status() adds only the traffic light, which is what this
# file pins.

library(testthat)

status_of <- function(status, role) status$status[match(role, status$role)]
hint_of   <- function(status, role) status$hint[match(role, status$role)]

rated <- function(rob = NULL, indirectness = NULL, studies = 3) {
  data.frame(
    studlab      = paste0("S", seq_len(studies)),
    rob          = if (is.null(rob)) rep(NA_character_, studies) else rob,
    indirectness = if (is.null(indirectness)) rep(NA_character_, studies)
                   else indirectness,
    stringsAsFactors = FALSE
  )
}

test_that("a recognised column turns its role green and is named", {
  status <- pma_column_role_status(
    detect_column_roles(c("study", "arm", "n_randomized", "d_r")))

  expect_equal(status_of(status, "studlab"), "found")
  expect_equal(status_of(status, "n"), "found")
  expect_equal(status$column[status$role == "studlab"], "study")
  expect_equal(status$column[status$role == "n"], "n_randomized")
})

test_that("a required role nothing filled is amber, with a hint", {
  status <- pma_column_role_status(detect_column_roles(c("studlab", "treat")))

  expect_equal(status_of(status, "n"), "missing")
  expect_true(nzchar(hint_of(status, "n")))
})

test_that("the unused half of the measure pair is muted, not amber", {
  binary <- pma_column_role_status(
    detect_column_roles(c("studlab", "treat", "n", "event")))
  expect_equal(status_of(binary, "event"), "found")
  expect_equal(status_of(binary, "mean"), "optional")
  expect_equal(status_of(binary, "sd"), "optional")

  continuous <- pma_column_role_status(
    detect_column_roles(c("studlab", "treat", "n", "mean", "sd")))
  expect_equal(status_of(continuous, "mean"), "found")
  expect_equal(status_of(continuous, "event"), "optional")
})

test_that("no measure column at all leaves all three amber", {
  status <- pma_column_role_status(
    detect_column_roles(c("studlab", "treat", "n")))

  expect_equal(status_of(status, "event"), "missing")
  expect_equal(status_of(status, "mean"), "missing")
  expect_equal(status_of(status, "sd"), "missing")
})

test_that("outcome and subgroup are muted when absent, not amber", {
  status <- pma_column_role_status(
    detect_column_roles(c("studlab", "treat", "n", "event")))

  expect_equal(status_of(status, "outcome"), "optional")
  expect_equal(status_of(status, "subgroup"), "optional")
})

test_that("Risk of Bias reports how much is rated, not whether a column exists", {
  detected <- detect_column_roles(c("studlab", "treat", "n", "event", "rob"))

  none <- pma_column_role_status(detected, rated())
  expect_equal(status_of(none, "rob"), "missing")
  expect_match(hint_of(none, "rob"), "0 of 3")

  some <- pma_column_role_status(detected, rated(c("low", NA, NA)))
  expect_equal(status_of(some, "rob"), "missing")
  expect_match(hint_of(some, "rob"), "1 of 3")

  all_rated <- pma_column_role_status(detected, rated(c("low", "some", "high")))
  expect_equal(status_of(all_rated, "rob"), "found")
  expect_equal(hint_of(all_rated, "rob"), "")
})

test_that("bulk-setting every study is what turns the chip green", {
  # What the Step 1 bulk buttons do: write one level into every row of
  # state$rob_table.
  detected <- detect_column_roles(c("studlab", "treat", "n", "event"))
  before <- pma_column_role_status(detected, rated())
  after  <- pma_column_role_status(detected, rated(rep("low", 3)))

  expect_equal(status_of(before, "rob"), "missing")
  expect_equal(status_of(after, "rob"), "found")
})

test_that("no judgment table at all still reports rather than erroring", {
  status <- pma_column_role_status(
    detect_column_roles(c("studlab", "treat", "n", "event")))

  expect_equal(status_of(status, "rob"), "missing")
  expect_equal(hint_of(status, "rob"), "not rated yet")
})

test_that("every role gets a chip, and the strip renders", {
  detected <- detect_column_roles(c("study", "arm", "n_randomized", "d_r"))
  html <- as.character(pma_column_roles_strip(detected, rated()))

  for (label in PMA_ROLE_LABELS) expect_match(html, label, fixed = TRUE)
  expect_match(html, "pma-role-found", fixed = TRUE)
  expect_match(html, "pma-role-missing", fixed = TRUE)
  expect_match(html, "n_randomized", fixed = TRUE)
})

test_that("the analysis columns are the roles the data carries, in role order", {
  # Shaped like an ingested frame: ingest_data() has already renamed the
  # source columns onto their roles and left every extra column in place.
  ingested <- data.frame(age_n = 1, event = 1, studlab = "A", n = 1,
                         treat = "x", severity_scale = "y",
                         stringsAsFactors = FALSE)

  analysis <- pma_analysis_columns(ingested)

  expect_equal(analysis, c("studlab", "treat", "n", "event"))
  expect_false("age_n" %in% analysis)
  expect_false("severity_scale" %in% analysis)
})

test_that("hiding the extra columns leaves the analysis columns' indices alone", {
  # DT reports a cell edit as the DataTables column index, which counts
  # hidden columns, and step1_data.R applies it against the full frame. The
  # preview therefore hides columns rather than subsetting -- this is the
  # invariant that makes that safe.
  frame_names <- c("age_n", "studlab", "severity_scale", "treat", "n", "event")
  analysis <- pma_analysis_columns(
    stats::setNames(as.data.frame(as.list(rep(1, length(frame_names)))),
                    frame_names))
  hidden <- which(!frame_names %in% analysis) - 1L

  expect_equal(hidden, c(0L, 2L))
  expect_equal(frame_names[hidden + 1L], c("age_n", "severity_scale"))
  # An index into the full frame still names the column it named before.
  expect_equal(frame_names[which(frame_names == "event")], "event")
})

test_that("the load summary states rows and studies without a Status: prefix", {
  data <- data.frame(studlab = rep(c("A", "B"), each = 2),
                     treat = rep(c("x", "y"), 2), n = 1,
                     stringsAsFactors = FALSE)

  expect_equal(pma_load_summary(data), "4 rows, 2 studies, long format.")
  expect_no_match(pma_load_summary(data), "Status")
})

test_that("the load summary counts study-outcomes when an outcome column rides along", {
  data <- data.frame(studlab = rep("A", 4),
                     outcome = rep(c("o1", "o2"), each = 2),
                     treat = rep(c("x", "y"), 2), n = 1,
                     stringsAsFactors = FALSE)

  expect_match(pma_load_summary(data), "2 study-outcomes", fixed = TRUE)
})
