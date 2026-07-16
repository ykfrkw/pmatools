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

test_that("plot_forest_rob draws a placeholder on length mismatch", {
  m <- make_metabin_pfr()
  with_null_device({
    expect_silent(plot_forest_rob(m, rob = c("L", "H")))  # wrong length
    expect_silent(plot_forest_rob(m, rob = NULL))
  })
})

test_that("plot_forest_rob rejects non-meta objects", {
  expect_error(plot_forest_rob(list(), rob = "L"),
               regexp = "must be a meta-analysis object")
})
