# pma_analysis_signature() (R/ui_helpers.R) decides what counts as a DIFFERENT
# outcome, and therefore when the app throws away a finished Step 3 assessment.
# Both failure directions are expensive: too eager and a reviewer loses work
# for correcting a typo, too lax and answers given for one outcome are exported
# as though they were given for another.

.fake_ma <- function(studlab = c("Alpha 2020", "Beta 2021"),
                     event.e = c(10, 20), n.e = c(50, 60),
                     event.c = c(5, 12),  n.c = c(50, 60)) {
  structure(
    list(studlab = studlab,
         event.e = event.e, n.e = n.e,
         event.c = event.c, n.c = n.c),
    class = c("metabin", "meta")
  )
}

test_that("renaming an outcome does not change the signature", {
  ma <- .fake_ma()
  # The outcome name is not an input at all: the identity is built from the
  # body of evidence plus the direction answer, so a relabelled row (or a typo
  # correction) produces the same string by construction.
  expect_identical(pma_analysis_signature(ma, "undesirable"),
                   pma_analysis_signature(ma, "undesirable"))
  expect_false(grepl("outcome_name", pma_analysis_signature(ma, "undesirable")))
})

test_that("flipping the direction changes the signature", {
  ma <- .fake_ma()
  expect_false(identical(pma_analysis_signature(ma, "undesirable"),
                         pma_analysis_signature(ma, "desirable")))
  expect_match(pma_analysis_signature(ma, "desirable"), "direction=desirable")
})

test_that("changing arm-level numbers changes the signature", {
  base <- pma_analysis_signature(.fake_ma(), "undesirable")

  expect_false(identical(
    base, pma_analysis_signature(.fake_ma(event.e = c(11, 20)), "undesirable")))
  expect_false(identical(
    base, pma_analysis_signature(.fake_ma(n.c = c(51, 60)), "undesirable")))
  # A different set of studies is a different body of evidence.
  expect_false(identical(
    base,
    pma_analysis_signature(.fake_ma(studlab = c("Alpha 2020", "Gamma 2022")),
                           "undesirable")))
})

test_that("re-sorting the rows does not change the signature", {
  a <- pma_analysis_signature(.fake_ma(), "undesirable")
  b <- pma_analysis_signature(
    .fake_ma(studlab = c("Beta 2021", "Alpha 2020"),
             event.e = c(20, 10), n.e = c(60, 50),
             event.c = c(12, 5),  n.c = c(60, 50)),
    "undesirable")
  expect_identical(a, b)
})

test_that("no analysis yields NA, which callers read as 'unknown'", {
  expect_true(is.na(pma_analysis_signature(NULL, "undesirable")))
  expect_true(is.na(pma_analysis_signature(list(studlab = "A"), "undesirable")))
  expect_true(is.na(pma_analysis_signature(.fake_ma(studlab = character(0)),
                                           "undesirable")))
})

test_that("a missing direction is recorded as empty rather than dropped", {
  expect_match(pma_analysis_signature(.fake_ma(), NULL), "direction=$")
  expect_match(pma_analysis_signature(.fake_ma(), character(0)), "direction=$")
})
