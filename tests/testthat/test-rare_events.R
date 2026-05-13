library(testthat)

skip_if_not_installed("meta")

test_that("rare-events mock sample ingests as 20 rows and 10 studies", {
  path <- testthat::test_path("../../inst/extdata/rare_events_mock.csv")
  d <- ingest_data(path, format = "long")

  expect_equal(nrow(d), 20)
  expect_equal(length(unique(d$studlab)), 10)
  expect_true(all(c("studlab", "treat", "n", "event", "rob", "indirectness") %in% names(d)))
})

test_that("rare-events mock sample triggers rare flow without total-events rule", {
  path <- testthat::test_path("../../inst/extdata/rare_events_mock.csv")
  d <- ingest_data(path, format = "long")
  diag <- rare_event_diagnostics(d)

  expect_true(diag$rare_flow)
  expect_true(diag$event_rate_overall < 0.01)
  expect_equal(diag$single_zero_k, 5)
  expect_equal(diag$double_zero_k, 2)
  expect_gt(diag$total_events, 20)
})

test_that("regular CBT-I sample does not trigger rare flow", {
  path <- testthat::test_path("../../inst/extdata/cbti_depression.csv")
  d <- ingest_data(path, format = "long")
  d <- d[!is.na(d$event), , drop = FALSE]
  diag <- rare_event_diagnostics(
    d,
    experimental_label = "CBT-I",
    control_label = "Control"
  )

  expect_false(diag$rare_flow)
  expect_gt(diag$event_rate_overall, 0.01)
})

test_that("rare MA returns method table and sensitivity forest renders", {
  path <- testthat::test_path("../../inst/extdata/rare_events_mock.csv")
  d <- ingest_data(path, format = "long")
  rare <- run_rare_ma(d, effect_scale = "OR")

  expect_s3_class(rare, "pma_rare_meta")
  expect_true(inherits(rare$primary, "meta"))
  expected_primary <- if (requireNamespace("mmeta", quietly = TRUE)) {
    "BB_CR"
  } else {
    "MH_no_cc"
  }
  expect_equal(rare$primary_method, expected_primary)
  expect_equal(rare$method_table$method_id[[1]], expected_primary)
  expect_equal(rare$method_table$role[[1]], "Primary")
  expect_true(all(c("role", "method_id", "estimate", "ci_low", "ci_high") %in%
                    names(rare$method_table)))
  expect_equal(nrow(rare$method_table), 7)
  expect_true(all(c("BB_CR", "MH_no_cc", "GLMM", "Peto", "REIV_CC",
                    "REIV_TACC", "MH_CC") %in% rare$method_table$method_id))
  expect_false("LRP" %in% rare$method_table$method_id)
  expect_false(any(grepl("Rev", rare$method_table$method_id)))
  expect_true(grep("Fixed|Treatment-arm", rare$method_table$zero_cell_handling)[1] >
                grep("Peto", rare$method_table$method_id))

  expect_silent(plot_rare_sensitivity_forest(rare))
})
