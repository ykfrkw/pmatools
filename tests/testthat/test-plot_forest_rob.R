library(testthat)

skip_if_not_installed("meta")

make_metabin_pfr <- function() {
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

test_that("plot_forest_rob draws a stratified forest and returns invisible NULL", {
  m   <- make_metabin_pfr()
  rob <- c("L", "L", "S", "S", "H", "H")
  with_null_device({
    res <- withVisible(plot_forest_rob(m, rob = rob))
    expect_null(res$value)
    expect_false(res$visible)
  })
})

test_that("plot_forest_rob accepts word-form and legacy RoB labels plus NA", {
  m   <- make_metabin_pfr()
  rob <- c("low", "Some concerns", "some", "high", "very_serious", NA)
  with_null_device(
    expect_silent(plot_forest_rob(m, rob = rob))
  )
})

test_that("plot_forest_rob accepts explicit addrow_below with favors labels", {
  # Regression guard for the reviewer-reported overlap between the bottom
  # heterogeneity/test text and the axis / favors labels: an explicit
  # addrow_below must be honored alongside the subgroup layout.
  m   <- make_metabin_pfr()
  rob <- c("L", "L", "S", "S", "H", "H")
  with_null_device(
    expect_silent(plot_forest_rob(
      m, rob = rob,
      favors_left  = "Favors treatment",
      favors_right = "Favors control",
      addrow_below = 3
    ))
  )
})

test_that("plot_forest_rob computes bottom spacing when addrow_below is NULL", {
  m   <- make_metabin_pfr()
  rob <- c("L", "L", "S", "S", "H", "H")
  with_null_device(
    expect_silent(plot_forest_rob(
      m, rob = rob,
      favors_left  = "Favors treatment",
      favors_right = "Favors control"
    ))
  )
})

test_that(".auto_addrow_below scales with favors labels and xlab", {
  expect_identical(.auto_addrow_below(), 2L)
  expect_identical(.auto_addrow_below(has_favors = TRUE), 3L)
  expect_identical(.auto_addrow_below(has_favors = TRUE, has_xlab = TRUE), 4L)
})

test_that("plot_forest_rob draws a placeholder on length mismatch", {
  m <- make_metabin_pfr()
  with_null_device({
    expect_silent(plot_forest_rob(m, rob = c("L", "H")))  # wrong length
    expect_silent(plot_forest_rob(m, rob = NULL))
  })
})

test_that(".normalise_rob accepts the Cochrane RoB2 labels documented in README", {
  expect_equal(
    pmatools:::.normalise_rob(c("No concerns", "Some concerns",
                                "Serious concerns", "Critical concerns")),
    c("low", "some", "high", "high")
  )
  # Same vocabulary as grade_meta(): single letters, plain words, internal
  # levels, legacy aliases and free capitalisation all land in a stratum.
  expect_equal(
    pmatools:::.normalise_rob(c("L", "S", "H", "low", "some", "high",
                                "no", "some_concerns", "serious",
                                "moderate", "unclear", "very_serious",
                                "SOME CONCERNS", " Low ")),
    c("low", "some", "high", "low", "some", "high",
      "low", "some", "high",
      "some", "some", "high",
      "some", "low")
  )
  # Missing markers stay "unknown"
  expect_equal(
    pmatools:::.normalise_rob(c(NA, "", "?", "na")),
    rep("unknown", 4)
  )
})

test_that("Cochrane labels stratify the forest plot instead of collapsing to unknown", {
  m   <- make_metabin_pfr()
  rob <- c("No concerns", "No concerns", "Some concerns",
           "Some concerns", "Serious concerns", "Critical concerns")

  with_null_device(expect_silent(plot_forest_rob(m, rob = rob)))

  # The subgroup actually reaching meta::update() must have >1 non-empty level
  strata <- pmatools:::.normalise_rob(rob)
  expect_setequal(unique(strata), c("low", "some", "high"))
  expect_false(any(strata == "unknown"))
})

test_that("plot_forest_rob warns on unrecognized labels rather than silently bucketing", {
  m   <- make_metabin_pfr()
  rob <- c("L", "L", "S", "S", "H", "totally bogus")
  with_null_device(
    expect_warning(plot_forest_rob(m, rob = rob), regexp = "unrecognized label")
  )
  expect_warning(
    expect_equal(pmatools:::.normalise_rob(c("L", "bogus")), c("low", "unknown")),
    regexp = "unrecognized label"
  )
})

test_that("plot_forest_indirectness shares the RoB label vocabulary", {
  expect_equal(
    pmatools:::.normalise_indirectness(c("No concerns", "Some concerns",
                                         "Serious concerns", NA)),
    c("low", "some", "high", "unknown")
  )
})

test_that("plot_forest_rob rejects non-meta objects", {
  expect_error(plot_forest_rob(list(), rob = "L"),
               regexp = "must be a meta-analysis object")
})
