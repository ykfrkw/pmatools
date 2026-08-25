library(testthat)

skip_if_not_installed("meta")

# A continuous outcome on an instrument whose pooled SD is 10, with study
# effects spread from -40 to +40 -- four standard deviations either side, so
# the quantile candidate range runs well past the +/-3 SD display bound while
# the pooled estimate, sitting at zero with k = 40, stays comfortably inside it.
make_wide_metacont <- function(sm = "MD") {
  k <- 40
  effects <- seq(-40, 40, length.out = k)
  meta::metacont(
    n.e     = rep(30, k), mean.e = effects,   sd.e = rep(10, k),
    n.c     = rep(30, k), mean.c = rep(0, k), sd.c = rep(10, k),
    studlab = paste0("W", seq_len(k)),
    sm      = sm,
    random  = TRUE
  )
}

# The same instrument read by five well-powered trials of a modest effect: no
# study limit comes near 3 SD, so the clamp has nothing to do.
make_narrow_metacont <- function() {
  meta::metacont(
    n.e     = rep(60, 5), mean.e = c(-4.1, -3.5, -5.2, -2.8, -4.6),
    sd.e    = rep(3, 5),
    n.c     = rep(60, 5), mean.c = c(-1.2, -0.9, -2.0, -1.4, -1.1),
    sd.c    = rep(3, 5),
    studlab = paste0("C", seq_len(5)),
    sm      = "MD",
    random  = TRUE
  )
}

# The quantile-plus-padding range .auto_xlim() starts from, recomputed here so
# a test can say what the clamp and the snap did to it.
candidate_xlim <- function(meta_obj) {
  qrange <- stats::quantile(c(meta_obj$lower, meta_obj$upper), c(0.05, 0.95),
                            na.rm = TRUE)
  pad <- 0.1 * abs(diff(qrange))
  unname(c(qrange[1] - pad, qrange[2] + pad))
}

test_that(".auto_xlim stops a mean difference at three pooled SDs", {
  m <- make_wide_metacont()
  expect_equal(compute_pooled_sd(m), 10)

  candidate <- candidate_xlim(m)
  # Without the clamp the axis would follow the widest studies out to 4 SD.
  expect_gt(candidate[2], 30)
  expect_lt(candidate[1], -30)

  # Before snapping: exactly +/-3 pooled SD.
  expect_equal(.clamp_standardised_xlim(candidate, m), c(-30, 30))

  # After snapping: still round numbers (30 is already a multiple of the step).
  xlim <- .auto_xlim(m)
  expect_equal(xlim, c(-30, 30))
  step <- .nice_lin_step(xlim[2] - xlim[1])
  expect_equal(xlim / step, round(xlim / step))
})

test_that(".auto_xlim clamps an SMD at three, with no SD scaling", {
  m <- make_wide_metacont(sm = "SMD")
  # The values are already standardised, so the scale factor is exactly 1 and
  # the bound is the bare 3 -- not 3 x anything.
  expect_identical(.standardised_scale_factor(m), 1)
  expect_equal(.auto_xlim(m), c(-3, 3))
})

test_that(".auto_xlim leaves an unbinding clamp alone and only snaps outward", {
  m <- make_narrow_metacont()
  candidate <- candidate_xlim(m)
  # 3 x pooled SD is 9 here; nothing in the data reaches it.
  expect_identical(.clamp_standardised_xlim(candidate, m), candidate)

  xlim <- .auto_xlim(m)
  expect_lte(xlim[1], candidate[1])
  expect_gte(xlim[2], candidate[2])

  # Snapping rounds outward by less than one step, and lands on multiples of it.
  step <- .nice_lin_step(xlim[2] - xlim[1])
  expect_lt(candidate[1] - xlim[1], step)
  expect_lt(xlim[2] - candidate[2], step)
  expect_equal(xlim / step, round(xlim / step))
})

test_that(".auto_xlim never crops the pooled diamond", {
  # Four studies pulling in opposite directions: tau is large and k is small,
  # so the random-effects interval is wider than the +/-3 SD clamp. A display
  # bound that hid the pooled result would be worse than no bound at all.
  m <- meta::metacont(
    n.e     = rep(30, 4), mean.e = c(-45, -15, 15, 45), sd.e = rep(10, 4),
    n.c     = rep(30, 4), mean.c = rep(0, 4),           sd.c = rep(10, 4),
    studlab = paste0("G", seq_len(4)),
    sm      = "MD",
    random  = TRUE
  )
  expect_gt(m$upper.random, 3 * compute_pooled_sd(m))

  xlim <- .auto_xlim(m)
  expect_lte(xlim[1], m$lower.random)
  expect_gte(xlim[2], m$upper.random)
})

test_that(".auto_xlim skips the clamp when no pooled SD can be derived", {
  m <- make_wide_metacont()
  m$sd.e <- NULL
  m$sd.c <- NULL
  m$seTE <- NULL
  expect_null(compute_pooled_sd(m))
  expect_null(.standardised_scale_factor(m))

  candidate <- candidate_xlim(m)
  xlim <- .auto_xlim(m)
  # No error, and the range is the unclamped candidate, snapped outward.
  expect_lte(xlim[1], candidate[1])
  expect_gte(xlim[2], candidate[2])
  expect_lt(xlim[1], -30)
  expect_gt(xlim[2], 30)
})

test_that(".auto_xlim skips the clamp for a continuous sm that is neither MD nor SMD", {
  m <- make_narrow_metacont()
  m$sm <- "MRAW"
  expect_null(.standardised_scale_factor(m))
  expect_silent(.auto_xlim(m))
})

test_that(".nice_lin_step picks a round step at every magnitude", {
  # width, then the steps that leave 4 to 8 intervals and are acceptable here.
  cases <- list(
    list(width = 0.3,  allowed = c(0.05, 0.1)),
    list(width = 1,    allowed = c(0.2, 0.25)),
    list(width = 6,    allowed = c(1, 1.25)),
    list(width = 12,   allowed = c(2, 2.5)),
    list(width = 400,  allowed = c(50, 100))
  )
  for (case in cases) {
    step <- .nice_lin_step(case$width)
    expect_true(any(abs(step - case$allowed) < 1e-9),
                info = paste("width", case$width, "gave step", step))
    intervals <- case$width / step
    expect_gte(intervals, PMA_FOREST_MIN_INTERVALS)
    expect_lte(intervals, PMA_FOREST_MAX_INTERVALS)
  }

  # A width nothing can be fitted to is reported, not guessed at.
  expect_true(is.na(.nice_lin_step(0)))
  expect_true(is.na(.nice_lin_step(NA_real_)))
  expect_true(is.na(.nice_lin_step(Inf)))
})

test_that(".snap_lin_xlim rounds both ends outward to multiples of the step", {
  snapped <- .snap_lin_xlim(c(-0.37, 0.12))
  expect_equal(snapped, c(-0.4, 0.2))

  snapped_large <- .snap_lin_xlim(c(-183, 217))
  expect_equal(snapped_large, c(-200, 250))

  # Ends already on the step are left where they are.
  expect_equal(.snap_lin_xlim(c(-30, 30)), c(-30, 30))

  # Degenerate input is handed back untouched rather than snapped to nothing.
  expect_identical(.snap_lin_xlim(c(2, 2)), c(2, 2))
  expect_identical(.snap_lin_xlim(c(NA_real_, 1)), c(NA_real_, 1))
})

test_that(".nice_lin_ticks lands on the snapped ends and always marks the null", {
  xlim <- .snap_lin_xlim(c(-0.37, 0.12))
  ticks <- .nice_lin_ticks(xlim)
  expect_equal(ticks[1], xlim[1])
  expect_equal(ticks[length(ticks)], xlim[2])
  expect_true(0 %in% ticks)
  expect_gte(length(ticks), PMA_FOREST_MIN_TICKS)

  # The same holds for the range the wide MD object produces.
  wide_xlim <- .auto_xlim(make_wide_metacont())
  wide_ticks <- .nice_lin_ticks(wide_xlim)
  expect_equal(range(wide_ticks), wide_xlim)
  expect_true(0 %in% wide_ticks)
})

test_that(".nice_lin_ticks still serves limits it did not choose", {
  # A caller-typed range: interior ticks are multiples of the step, the ends
  # need not be, and there are never fewer than three marks.
  ticks <- .nice_lin_ticks(c(-1.7, 2.3))
  expect_gte(length(ticks), PMA_FOREST_MIN_TICKS)
  expect_true(all(ticks >= -1.7 & ticks <= 2.3))
  expect_true(0 %in% ticks)

  # Degenerate limits degrade to the axisTicks() behaviour this used to be,
  # or to no ticks at all -- never to an error.
  expect_silent(.nice_lin_ticks(c(0, 0)))
  expect_null(.nice_lin_ticks(c(NA_real_, NA_real_)))
  expect_null(.nice_lin_ticks(numeric(0)))
})

test_that("plot_forest passes a caller's xlim to meta::forest unchanged", {
  # Only the ticks become nice numbers: the x-min / x-max the app sends are the
  # numbers the user typed, and snapping them would move the axis under them.
  m <- make_narrow_metacont()
  seen <- NULL
  with_null_device(
    testthat::with_mocked_bindings(
      plot_forest(m, xlim = c(-7.3, 1.4)),
      forest = function(...) { seen <<- list(...); invisible(NULL) },
      .package = "meta"
    )
  )
  expect_identical(seen$xlim, c(-7.3, 1.4))
  expect_true(all(seen$at >= -7.3 & seen$at <= 1.4))
})

test_that("plot_forest sends the snapped auto range and its ticks together", {
  m <- make_wide_metacont()
  seen <- NULL
  with_null_device(
    testthat::with_mocked_bindings(
      plot_forest(m),
      forest = function(...) { seen <<- list(...); invisible(NULL) },
      .package = "meta"
    )
  )
  expect_equal(seen$xlim, c(-30, 30))
  expect_equal(range(seen$at), seen$xlim)
})
