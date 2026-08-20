# Event rates and the Core GRADE 6 rare-event alert (R/sof_display.R).
#
# Core GRADE 6 names two bands - "event rates <2% and most problematic <1%" -
# and the app has to fire on the right one. The boundaries are half-open, so
# they are tested exactly at 1% and at 2%.

.fake_g <- function(event.e, n.e, event.c, n.c, baseline_risk = NA_real_) {
  list(meta = list(event.e = event.e, n.e = n.e,
                   event.c = event.c, n.c = n.c),
       baseline_risk = baseline_risk)
}

test_that("pma_sof_event_rates() reads the crude arm rates off the metabin arms", {
  r <- pma_sof_event_rates(list(event.e = c(10, 20), n.e = c(100, 100),
                                event.c = c(5, 15),  n.c = c(100, 100)))
  expect_equal(r$intervention, 30 / 200)
  expect_equal(r$control, 20 / 200)
  expect_equal(r$overall, 50 / 400)
  expect_equal(r$events, 50)
  expect_equal(r$n, 400)
})

test_that("pma_sof_event_rates() drops unusable studies but keeps the rest", {
  r <- pma_sof_event_rates(list(event.e = c(10, NA), n.e = c(100, 100),
                                event.c = c(5, 15),  n.c = c(100, 0)))
  expect_equal(r$intervention, 10 / 100)   # second study has no event count
  expect_equal(r$control, 5 / 100)         # second study has a zero denominator
})

test_that("pma_sof_event_rates() returns NULL for anything not binary", {
  expect_null(pma_sof_event_rates(NULL))
  expect_null(pma_sof_event_rates(list(mean.e = 1, sd.e = 1)))
  expect_null(pma_sof_event_rates(list(event.e = 1, n.e = c(1, 2),
                                       event.c = 1, n.c = 1)))
  expect_null(pma_sof_event_rates(list(event.e = NA, n.e = 0,
                                       event.c = NA, n.c = 0)))
})

test_that("pma_rare_event_alert() stays silent at and above 2 percent", {
  # Every rate is exactly 2%: 0.02 >= PMA_RARE_BAND_2, so no alert.
  expect_null(pma_rare_event_alert(
    .fake_g(event.e = 20, n.e = 1000, event.c = 20, n.c = 1000)))
  expect_null(pma_rare_event_alert(
    .fake_g(event.e = 100, n.e = 1000, event.c = 100, n.c = 1000)))
})

test_that("pma_rare_event_alert() bands on the LOWEST rate involved", {
  # Exactly 1%: below the 2% band, but 0.01 < 0.01 is false, so not "below 1%".
  a <- pma_rare_event_alert(
    .fake_g(event.e = 10, n.e = 1000, event.c = 10, n.c = 1000))
  expect_equal(a$band, "below 2%")
  expect_equal(a$lowest, 0.01)

  # One arm just under 1% drags the whole alert into the lower band.
  b <- pma_rare_event_alert(
    .fake_g(event.e = 9, n.e = 1000, event.c = 10, n.c = 1000))
  expect_equal(b$band, "below 1%")
  expect_equal(b$lowest, 0.009)
})

test_that("pma_rare_event_alert() counts the baseline risk as a candidate rate", {
  # The observed rates are comfortably above 2%, but the absolute columns are
  # drawn against a baseline the reviewer set by hand, and that is what the
  # Core GRADE 6 warning is actually about.
  a <- pma_rare_event_alert(
    .fake_g(event.e = 100, n.e = 1000, event.c = 100, n.c = 1000,
            baseline_risk = 0.005))
  expect_equal(a$band, "below 1%")
  expect_equal(a$lowest, 0.005)
  expect_equal(a$baseline_risk, 0.005)

  # An explicit baseline_risk argument (the Chinn responder proportion) wins
  # over the object's own.
  b <- pma_rare_event_alert(
    .fake_g(event.e = 100, n.e = 1000, event.c = 100, n.c = 1000,
            baseline_risk = 0.005),
    baseline_risk = 0.2)
  expect_null(b)
})

test_that("pma_rare_event_alert() reports the rates it banded on", {
  a <- pma_rare_event_alert(
    .fake_g(event.e = 9, n.e = 1000, event.c = 10, n.c = 1000),
    label = "Serious adverse events")

  expect_equal(a$rates$events, 19)
  expect_equal(a$rates$n, 2000)
  expect_match(a$headline, "Serious adverse events", fixed = TRUE)
  expect_match(a$headline, "below 1%", fixed = TRUE)
  # The same text goes on screen and into the exported docx.
  expect_match(a$note, "^Rare-event caution \\(Core GRADE 6\\)\\.")
  expect_match(a$detail, "0.90%", fixed = TRUE)   # intervention arm
  expect_null(pma_rare_event_alert(NULL))
})

test_that("pma_fmt_pct() keeps enough resolution to separate the two bands", {
  expect_equal(pma_fmt_pct(0.0095), "0.95%")
  expect_equal(pma_fmt_pct(0.01), "1.00%")
  expect_equal(pma_fmt_pct(NA_real_), "not estimable")
  expect_equal(pma_fmt_pct(NULL), "not estimable")
})

# pma_sof_unit() has exactly one destination as of v0.6: the Difference column
# of a mean difference. It used to return "standard deviation units" for an
# SMD, and the same value reached sof_table()'s `unit`, which labelled the ARM
# columns with it -- so a control mean already re-expressed on the outcome's own
# scale printed as "13.89 standard deviation units". Those columns are gone.
test_that("pma_sof_unit() labels a mean difference and nothing else", {
  md_unit <- function(sm, unit) {
    pma_sof_unit(list(meta = list(sm = sm)), unit)
  }
  expect_identical(md_unit("MD", "days"), "days")
  expect_identical(md_unit("MD", "  days  "), "days")
  expect_null(md_unit("MD", NULL))
  expect_null(md_unit("MD", "   "))

  # An SMD's Difference cell is empty, so there is nothing left to label.
  expect_null(md_unit("SMD", "days"))
  expect_null(md_unit("RR", "days"))
  expect_null(md_unit("RoM", "days"))
})

test_that("the rare-event alert names the arms the table names", {
  # The alert quotes two arm rates and then names a column by its header. With
  # a reviewer's own labels in the headers, an alert saying "the intervention
  # arm" and 'the "With intervention" column' points at a column the table does
  # not have.
  a <- pma_rare_event_alert(
    .fake_g(event.e = 9, n.e = 1000, event.c = 10, n.c = 1000),
    labels = list(intervention = "CBT-I", control = "placebo"))
  expect_false(is.null(a))
  expect_match(a$note, "in the placebo arm", fixed = TRUE)
  expect_match(a$note, "in the CBT-I arm", fixed = TRUE)
  expect_match(a$note, '"With CBT-I" column', fixed = TRUE)

  # Unchanged when the arms were never named.
  b <- pma_rare_event_alert(
    .fake_g(event.e = 9, n.e = 1000, event.c = 10, n.c = 1000))
  expect_match(b$note, "in the control arm", fixed = TRUE)
  expect_match(b$note, '"With intervention" column', fixed = TRUE)
})
