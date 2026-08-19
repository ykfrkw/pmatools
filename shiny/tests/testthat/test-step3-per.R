# The per-N display unit (R/step3_threshold.R).
#
# The property that matters, and the reason these are separate functions
# rather than inline arithmetic: INTERNAL STORAGE IS ALWAYS PER 1,000. The
# reactiveVals behind the control-group risk and the absolute threshold, the
# /1000 in .threshold_grade_args() and ois_p0_value(), and the note recorded
# on the rated object all stay in that unit whatever the reviewer picks. Only
# the displayed value, its label and sof_table(per =) move. So every test
# below is really a test that the two directions compose back to identity.

# The two large units (10,000 and 100,000) were added for rare events
# (shiny/SPEC.md 3.4.14) and are exercised in test-step3-rare.R; the tests
# below are the original per-100 / per-1,000 contract, unchanged.
test_that("step3_per_unit accepts only the units the app offers", {
  expect_identical(step3_per_unit(100), 100L)
  expect_identical(step3_per_unit(1000), 1000L)
  # A radioButtons value arrives as a character. This is the whole reason the
  # function exists: sof_table(per = "1000") is not the same call.
  expect_identical(step3_per_unit("100"), 100L)
  expect_identical(step3_per_unit("1000"), 1000L)

  # Anything else falls back to the default rather than propagating.
  expect_identical(step3_per_unit(NULL), 1000L)
  expect_identical(step3_per_unit(NA), 1000L)
  expect_identical(step3_per_unit("many"), 1000L)
  expect_identical(step3_per_unit(500), 1000L)
  expect_identical(step3_per_unit(c(100, 1000)), 1000L)
})

test_that("to_per and from_per are inverses", {
  expect_equal(step3_to_per(156, 100), 15.6)
  expect_equal(step3_to_per(156, 1000), 156)
  expect_equal(step3_from_per(15.6, 100), 156)
  expect_equal(step3_from_per(156, 1000), 156)

  for (per in c(100, 1000)) {
    for (v in c(1, 15.6, 156, 999)) {
      expect_equal(step3_from_per(step3_to_per(v, per), per), v)
    }
  }

  # Unusable input is NA, never a silently wrong number.
  expect_true(is.na(step3_to_per(NULL, 100)))
  expect_true(is.na(step3_to_per(NA_real_, 100)))
  expect_true(is.na(step3_from_per(Inf, 100)))
})

test_that("quantising puts the value on a whole number of events", {
  # per 1,000: the existing grid, one event.
  expect_equal(step3_quantise_per1000(156.4, 1000), 156)
  expect_equal(step3_quantise_per1000(156.6, 1000), 157)

  # per 100: ten times coarser, and DELIBERATELY so - 15.6 events per 100
  # patients is not something a reviewer can read off a trial. shiny/SPEC.md
  # states the cost rather than leaving it to be discovered.
  expect_equal(step3_quantise_per1000(156, 100), 160)
  expect_equal(step3_quantise_per1000(154, 100), 150)

  # Whatever the unit, the result is exactly representable as an integer in
  # that unit - which is what the numericInput(step = 1) offers.
  for (per in c(100, 1000)) {
    for (v in c(3.3, 156, 947.2)) {
      shown <- step3_to_per(step3_quantise_per1000(v, per), per)
      expect_equal(shown, round(shown))
    }
  }

  expect_true(is.na(step3_quantise_per1000(NA_real_, 100)))
  expect_true(is.na(step3_quantise_per1000(NULL, 1000)))
})

test_that("one formatter carries every rate string on the tab", {
  expect_identical(step3_per_label(156, 1000), "156 per 1,000")
  expect_identical(step3_per_label(160, 100), "16 per 100")
  expect_identical(step3_per_label(1560, 1000), "1,560 per 1,000")
  expect_identical(step3_per_label(156, 1000, digits = 1), "156.0 per 1,000")

  # An unset value says so in the right unit instead of printing NA.
  expect_identical(step3_per_label(NA_real_, 100), "not set (per 100)")

  expect_identical(step3_per_unit_label(100), "per 100")
  expect_identical(step3_per_unit_label(1000), "per 1,000")
  expect_identical(step3_per_unit_label("100"), "per 100")
})
