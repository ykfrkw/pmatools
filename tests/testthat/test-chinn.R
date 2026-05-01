library(testthat)

test_that("chinn_smd_to_or factor is pi/sqrt(3)", {
  r <- chinn_smd_to_or(0)
  expect_equal(r$factor, pi / sqrt(3))
  expect_equal(r$or, 1)  # exp(0)
})

test_that("chinn_smd_to_or numerical accuracy", {
  r <- chinn_smd_to_or(-0.5)
  expected <- exp(-0.5 * pi / sqrt(3))
  expect_equal(r$or, expected, tolerance = 1e-10)
})

test_that("chinn_smd_to_or propagates CI bounds", {
  r <- chinn_smd_to_or(-0.5, ci_lower = -0.7, ci_upper = -0.3)
  expect_lt(r$or_lower, r$or)
  expect_gt(r$or_upper, r$or)
})

test_that("chinn_smd_to_or handles NULL CI inputs", {
  r <- chinn_smd_to_or(0.2)
  expect_true(is.na(r$or_lower))
  expect_true(is.na(r$or_upper))
})

test_that("suggest_mid returns expected defaults", {
  skip_if_not_installed("meta")
  m <- suppressWarnings(meta::metabin(
    event.e = c(10, 15), n.e = c(50, 50),
    event.c = c(15, 20), n.c = c(50, 50),
    studlab = c("A", "B"), sm = "OR"
  ))
  s <- suggest_mid(m)
  expect_equal(s$mid_user, 1.25)
  expect_equal(s$mid_scale, "ratio")
})

test_that("compute_pooled_sd returns numeric for metacont", {
  skip_if_not_installed("meta")
  m <- suppressWarnings(meta::metacont(
    n.e = c(50, 60), mean.e = c(10, 11), sd.e = c(3, 3.5),
    n.c = c(50, 60), mean.c = c(12, 13), sd.c = c(3, 3.5),
    studlab = c("A", "B")
  ))
  sd_pooled <- compute_pooled_sd(m)
  expect_true(is.numeric(sd_pooled))
  expect_gt(sd_pooled, 0)
})

test_that("mid_to_te_scale handles ratio scale", {
  out <- mid_to_te_scale(1.25, "ratio", "OR")
  expect_equal(out$mid_internal, log(1.25))
  expect_equal(out$mid_kind, "ratio")
})

test_that("mid_to_te_scale auto for OR -> ratio", {
  out <- mid_to_te_scale(1.25, "auto", "OR")
  expect_equal(out$mid_internal, log(1.25))
})

test_that("mid_to_te_scale auto for SMD -> te_scale", {
  out <- mid_to_te_scale(0.20, "auto", "SMD")
  expect_equal(out$mid_internal, 0.20)
  expect_equal(out$mid_kind, "te_scale")
})

test_that("mid_to_te_scale returns NULL for NULL mid", {
  out <- mid_to_te_scale(NULL, "auto", "OR")
  expect_null(out$mid_internal)
})
