# test-step1-columns.R - the preview's analysis-column set and Step 1's load
# summary.
#
# The preview defaults to the columns the analysis reads, so what
# pma_analysis_columns() returns decides what a reviewer checking a 39-column
# upload actually sees -- and the indices the rest of the frame is hidden by
# have to stay pointing at the same columns, because a cell edit is applied
# against the full frame by DataTables column index.

library(testthat)

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
