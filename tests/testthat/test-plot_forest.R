library(testthat)

skip_if_not_installed("meta")

make_metabin_pf <- function() {
  meta::metabin(
    event.e = c(20, 18, 15, 12, 10, 8),
    n.e     = rep(100, 6),
    event.c = c(20, 19, 17, 15, 13, 11),
    n.c     = rep(100, 6),
    studlab = paste0("S", 1:6),
    sm      = "OR",
    random  = TRUE,
    method  = "Inverse"
  )
}

make_metacont_pf <- function() {
  meta::metacont(
    n.e    = rep(40, 5),
    mean.e = c(-4.1, -3.5, -5.2, -2.8, -4.6),
    sd.e   = c(3.1, 2.8, 3.6, 2.4, 3.0),
    n.c    = rep(40, 5),
    mean.c = c(-1.2, -0.9, -2.0, -1.4, -1.1),
    sd.c   = c(3.0, 2.9, 3.4, 2.6, 3.2),
    studlab = paste0("C", 1:5),
    sm      = "MD",
    random  = TRUE
  )
}

test_that("plot_forest draws mean and SD columns for a continuous outcome", {
  m <- make_metacont_pf()
  with_null_device(
    expect_silent(plot_forest(m, show_events = TRUE, show_n = TRUE))
  )
})

test_that("plot_forest still draws event columns for a binary outcome", {
  # Regression guard for the intervention-then-control column ordering: the
  # mean/SD branch must not disturb the binary layout.
  m <- make_metabin_pf()
  with_null_device(
    expect_silent(plot_forest(m, show_events = TRUE, show_n = TRUE))
  )
})

test_that("plot_forest falls back cleanly when a continuous object lacks sd.e", {
  m <- make_metacont_pf()
  m$sd.e <- NULL
  with_null_device(
    expect_silent(plot_forest(m, show_events = TRUE, show_n = TRUE))
  )
})

test_that(".resolve_arm_labels replaces the {meta} 'Experimental' default", {
  m <- make_metabin_pf()
  expect_identical(m$label.e, "Experimental")

  expect_equal(
    .resolve_arm_labels(NULL, NULL, m),
    list(e = "Intervention", c = "Control")
  )
  # An explicit caller label passes through untouched.
  expect_equal(
    .resolve_arm_labels("CBT-I", NULL, m),
    list(e = "CBT-I", c = "Control")
  )
})

test_that("plot_forest tolerates NA addrow arguments", {
  m <- make_metabin_pf()
  with_null_device(
    expect_silent(plot_forest(m, addrow_above = NA, addrow_below = NA))
  )
})

test_that(".auto_addrow_below keeps its documented baseline", {
  expect_identical(.auto_addrow_below(FALSE, FALSE), 2L)
  expect_identical(.auto_addrow_below(TRUE, FALSE), 3L)
})

test_that("plot_forest rejects non-meta objects", {
  expect_error(plot_forest(list()),
               regexp = "must be a meta-analysis object")
})
