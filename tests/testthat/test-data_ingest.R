library(testthat)

# Sample wide-format binary data
make_wide_binary <- function() {
  data.frame(
    studlab = c("A", "B", "C"),
    n_e     = c(50, 60, 70),
    n_c     = c(50, 60, 70),
    event_e = c(10, 15, 20),
    event_c = c(15, 20, 25),
    rob     = c("L", "S", "H"),
    stringsAsFactors = FALSE
  )
}

# Sample long-format binary data
make_long_binary <- function() {
  data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    rob     = rep(c("L", "S", "H"), each = 2),
    stringsAsFactors = FALSE
  )
}

test_that("ingest_data accepts long-format data.frame", {
  df <- make_long_binary()
  out <- ingest_data(df, format = "long")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 6)
  expect_true(all(c("studlab", "treat", "n", "event") %in% names(out)))
})

test_that("ingest_data converts wide -> long", {
  df <- make_wide_binary()
  out <- ingest_data(df, format = "wide")
  expect_equal(nrow(out), 6)
  expect_true(all(c("studlab", "treat", "n", "event") %in% names(out)))
  expect_equal(sum(out$treat == "experimental"), 3)
  expect_equal(sum(out$treat == "control"), 3)
})

test_that("ingest_data auto-detects wide vs long", {
  wide <- make_wide_binary()
  long <- make_long_binary()
  expect_equal(nrow(ingest_data(wide, format = "auto")), 6)
  expect_equal(nrow(ingest_data(long, format = "auto")), 6)
})

test_that("ingest_data applies column-name mapping", {
  df <- data.frame(
    trial_id  = rep(c("A", "B", "C"), each = 2),
    arm       = rep(c("experimental", "control"), 3),
    sample_n  = c(50, 50, 60, 60, 70, 70),
    n_events  = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
  out <- ingest_data(df, format = "long",
                     mapping = list(studlab = "trial_id", treat = "arm",
                                    n = "sample_n", event = "n_events"))
  expect_equal(nrow(out), 6)
  expect_true("studlab" %in% names(out))
  expect_true("event" %in% names(out))
})

test_that("ingest_data rejects empty input", {
  expect_error(ingest_data(data.frame()), regexp = "no rows")
})

test_that("ingest_data validates studlab pair count", {
  df <- data.frame(
    studlab = c("A", "A", "A", "B", "B"),
    treat   = c("e", "c", "c", "e", "c"),
    n       = c(10, 10, 10, 20, 20),
    stringsAsFactors = FALSE
  )
  expect_error(ingest_data(df, format = "long"), regexp = "exactly 2 rows")
})

test_that("ingest_data accepts CSV path", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(make_long_binary(), path, row.names = FALSE)
  out <- ingest_data(path, format = "long")
  expect_equal(nrow(out), 6)
})
