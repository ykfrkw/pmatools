# The trim-and-fill exaggeration check (R/pubias_trimfill.R).
#
# Two properties these tests exist for. First, the comparison is oriented the
# same way as the risk-of-bias direction check: the ORIGINAL pooled effect is
# the side that may be exaggerated and the trim-and-fill ADJUSTED effect is the
# reference. Reverse them and the function reports exaggeration exactly when
# there is none, which no arithmetic test on a single case would catch.
# Second, it decides nothing: assess_pubias() must not read it.

library(testthat)

test_that("original further in the favourable direction, past 20%, is flagged", {
  # small_values = "undesirable": larger is better, so an original ABOVE the
  # adjusted estimate is the exaggerating direction.
  res <- .pubias_trimfill_inflation(te_original = 1.30, te_adjusted = 1.00,
                                    small_values = "undesirable")
  expect_true(res$assessable)
  expect_true(res$favourable)
  expect_equal(res$ratio, 0.30)
  expect_true(res$exaggerated)
  expect_equal(res$threshold, PMA_ROB_INFLATION_THRESHOLD)

  # small_values = "desirable": the mirror image, and the same verdict.
  res_mirror <- .pubias_trimfill_inflation(te_original = -1.30,
                                           te_adjusted = -1.00,
                                           small_values = "desirable")
  expect_true(res_mirror$favourable)
  expect_true(res_mirror$exaggerated)
})

test_that("a shift the other way is never flagged, however large", {
  # The adjustment makes the effect LOOK BETTER, so publication bias was not
  # what inflated it. The magnitude is well past the threshold on purpose.
  res <- .pubias_trimfill_inflation(te_original = 1.00, te_adjusted = 1.60,
                                    small_values = "undesirable")
  expect_true(res$assessable)
  expect_false(res$favourable)
  expect_false(res$exaggerated)
})

test_that("the threshold is the risk-of-bias one, and it is a strict '>'", {
  expect_equal(PMA_ROB_INFLATION_THRESHOLD, 0.20)

  at <- .pubias_trimfill_inflation(te_original = 1.20, te_adjusted = 1.00,
                                   small_values = "undesirable")
  expect_equal(at$ratio, 0.20)
  expect_false(at$exaggerated)

  over <- .pubias_trimfill_inflation(te_original = 1.21, te_adjusted = 1.00,
                                     small_values = "undesirable")
  expect_true(over$exaggerated)
})

test_that("without small_values only the magnitude is compared", {
  # Same pair of numbers, opposite verdicts once a direction is supplied: the
  # magnitude grew, but the growth is away from the favourable side.
  blind <- .pubias_trimfill_inflation(te_original = -1.50, te_adjusted = -1.00)
  expect_true(blind$favourable)
  expect_true(blind$exaggerated)

  directed <- .pubias_trimfill_inflation(te_original = -1.50,
                                         te_adjusted = -1.00,
                                         small_values = "undesirable")
  expect_false(directed$favourable)
  expect_false(directed$exaggerated)
})

test_that("an unusable pair is reported as unassessable, not as 'no concern'", {
  for (bad in list(list(o = NA_real_, a = 1),
                   list(o = 1, a = NA_real_),
                   list(o = Inf, a = 1),
                   list(o = 1, a = 0))) {
    res <- .pubias_trimfill_inflation(bad$o, bad$a, small_values = "undesirable")
    expect_false(res$assessable)
    expect_true(is.na(res$ratio))
    expect_false(res$exaggerated)
  }

  expect_false(.pubias_trimfill_inflation(numeric(0), 1)$assessable)
})

test_that("an unknown small_values is an error rather than a silent guess", {
  expect_error(
    .pubias_trimfill_inflation(1.3, 1.0, small_values = "smaller_is_better"),
    "small_values")
})

test_that("the printed line names both estimates and never claims a rating", {
  flagged <- .pubias_trimfill_inflation(1.30, 1.00, small_values = "undesirable")
  line <- .pubias_trimfill_line(flagged, te_original = 1.30, te_adjusted = 1.00)
  expect_match(line, "1.300")
  expect_match(line, "1.000")
  expect_match(line, "30%")
  expect_match(line, "20%")
  expect_match(line, "rates nothing", fixed = TRUE)
  expect_false(grepl("rate down", line, fixed = TRUE))

  within <- .pubias_trimfill_inflation(1.10, 1.00, small_values = "undesirable")
  expect_match(.pubias_trimfill_line(within, 1.10, 1.00), "within the 20% mark")

  wrong_way <- .pubias_trimfill_inflation(1.00, 1.60,
                                          small_values = "undesirable")
  expect_match(.pubias_trimfill_line(wrong_way, 1.00, 1.60),
               "does not move the estimate away from the favourable side")

  unusable <- .pubias_trimfill_inflation(NA_real_, 1.00)
  expect_match(.pubias_trimfill_line(unusable, NA_real_, 1.00),
               "not assessable")
})

test_that("format_te controls how the two estimates are printed", {
  res  <- .pubias_trimfill_inflation(0.30, 0.10, small_values = "undesirable")
  line <- .pubias_trimfill_line(res, 0.30, 0.10,
                                format_te = function(v) sprintf("%.2f", exp(v)))
  expect_match(line, "1.35")   # exp(0.30)
  expect_match(line, "1.11")   # exp(0.10)
})

test_that("the diagnostic does not reach the GRADE judgment", {
  # The guard on the design: Core GRADE 4 Fig 5 has no trim-and-fill node, and
  # the day assess_pubias() starts reading this function it will have invented
  # a rule the source does not contain.
  src <- paste(readLines(test_path("..", "..", "R", "domain_pubias.R"),
                         warn = FALSE),
               collapse = "\n")
  skip_if(!nzchar(src), "R/domain_pubias.R not readable from the test tree")
  code <- sub("^.*?\n\\.PUBIAS_FIG5_NODE_IDS", ".PUBIAS_FIG5_NODE_IDS", src)
  expect_false(grepl("trimfill", code, fixed = TRUE))
})
