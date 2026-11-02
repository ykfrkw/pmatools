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

# Updated (v0.5): binary outcomes lead with the absolute candidate; the OR
# ratio value moved to $threshold_ratio. Every default now carries $source, and
# only SMD 0.20 is sourced to Core GRADE 6.
test_that("suggest_threshold returns expected defaults", {
  skip_if_not_installed("meta")
  m <- suppressWarnings(meta::metabin(
    event.e = c(10, 15), n.e = c(50, 50),
    event.c = c(15, 20), n.c = c(50, 50),
    studlab = c("A", "B"), sm = "OR"
  ))
  s <- suggest_threshold(m)
  expect_equal(s$threshold_user, 0.05)
  expect_equal(s$threshold_scale, "ard")
  expect_equal(s$source, "package_convention")
  expect_equal(s$threshold_ratio$threshold_user, 1.25)
})

test_that("suggest_threshold marks SMD 0.20 as the only Core GRADE sourced default", {
  skip_if_not_installed("meta")
  m <- suppressWarnings(meta::metacont(
    n.e = c(50, 60), mean.e = c(10, 11), sd.e = c(3, 3.5),
    n.c = c(50, 60), mean.c = c(12, 13), sd.c = c(3, 3.5),
    studlab = c("A", "B"), sm = "SMD"
  ))
  s <- suggest_threshold(m)
  expect_equal(s$threshold_user, 0.20)
  expect_equal(s$source, "core_grade_6")
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

test_that("threshold_to_te_scale handles ratio scale", {
  out <- threshold_to_te_scale(1.25, "ratio", "OR")
  expect_equal(out$threshold_internal, log(1.25))
  expect_equal(out$threshold_kind, "ratio")
})

test_that("a ratio threshold below 1 is taken as its reciprocal, with a note", {
  out <- threshold_to_te_scale(0.8, "auto", "RR")
  expect_equal(out$threshold_internal, log(1.25))
  expect_match(out$threshold_note, "below 1; its reciprocal 1.25 was used")
  expect_null(threshold_to_te_scale(1.25, "auto", "RR")$threshold_note)
  expect_error(threshold_to_te_scale(0, "ratio", "RR"), "positive")
  expect_error(threshold_to_te_scale(-1.25, "ratio", "RR"), "positive")
})

test_that("grade_meta() rates RR 0.8 exactly as RR 1.25", {
  dat <- data.frame(
    studlab = paste0("S", 1:3),
    event.e = c(10, 15, 12), n.e = c(100, 120, 110),
    event.c = c(20, 22, 25), n.c = c(100, 118, 112))
  m <- meta::metabin(event.e, n.e, event.c, n.c, studlab = studlab,
                     data = dat, sm = "RR")
  rate <- function(thr) grade_meta(m, rob = rep("not_serious", 3),
                                   small_values = "desirable", threshold = thr,
                                   pubias_unpublished = "no")
  g_low  <- rate(0.8)
  g_high <- rate(1.25)
  expect_equal(g_low$threshold_internal, g_high$threshold_internal)
  expect_identical(g_low$rating_target, g_high$rating_target)
  expect_identical(g_low$domain_assessments$judgment,
                   g_high$domain_assessments$judgment)
  expect_length(g_low$domain_assessments$judgment, 5L)
  expect_match(g_low$threshold_note, "reciprocal")
})

test_that("threshold_to_te_scale auto for OR -> ratio", {
  out <- threshold_to_te_scale(1.25, "auto", "OR")
  expect_equal(out$threshold_internal, log(1.25))
})

test_that("threshold_to_te_scale auto for SMD -> te_scale", {
  out <- threshold_to_te_scale(0.20, "auto", "SMD")
  expect_equal(out$threshold_internal, 0.20)
  expect_equal(out$threshold_kind, "te_scale")
})

test_that("threshold_to_te_scale returns NULL for NULL threshold", {
  out <- threshold_to_te_scale(NULL, "auto", "OR")
  expect_null(out$threshold_internal)
})

test_that("threshold notes speak the ratio the reviewer entered, not its log", {
  dat <- data.frame(
    studlab = paste0("S", 1:3),
    event.e = c(10, 15, 12), n.e = c(100, 120, 110),
    event.c = c(20, 22, 25), n.c = c(100, 118, 112))
  m <- meta::metabin(event.e, n.e, event.c, n.c, studlab = studlab,
                     data = dat, sm = "OR")
  g <- grade_meta(m, rob = c("not_serious", "very_serious", "not_serious"),
                  small_values = "desirable", threshold = 1.25,
                  pubias_unpublished = "no")
  notes <- setNames(g$domain_assessments$notes, g$domain_assessments$domain)
  expect_match(notes[["Publication bias"]],
               "threshold OR 1.25 (or 0.8 on the other side)", fixed = TRUE)
  expect_match(notes[["Risk of bias"]], "Threshold = OR 1.25", fixed = TRUE)
  expect_no_match(notes[["Publication bias"]], "analysis scale")
})
