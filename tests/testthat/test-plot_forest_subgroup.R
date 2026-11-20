library(testthat)

skip_if_not_installed("meta")

mk_simple_meta <- function(k = 6) {
  ev_e <- c(20, 18, 15, 12, 10, 8)[seq_len(k)]
  ev_c <- c(20, 19, 17, 15, 13, 11)[seq_len(k)]
  n_e  <- rep(100, k)
  n_c  <- rep(100, k)
  meta::metabin(ev_e, n_e, ev_c, n_c, sm = "OR",
                studlab = paste0("S", seq_len(k)),
                random  = TRUE, method = "Inverse")
}

# --------------------------------------------------------------------------
# plot_forest_indirectness
# --------------------------------------------------------------------------
test_that("plot_forest_indirectness draws without error for valid input", {
  m <- mk_simple_meta(6)
  indir <- c("low", "low", "some", "some", "high", "high")
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot_forest_indirectness(m, indirectness = indir))
})

test_that("plot_forest_indirectness tolerates NA / unknown labels", {
  m <- mk_simple_meta(4)
  indir <- c("low", NA, "high", "*")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(plot_forest_indirectness(m, indirectness = indir))
})

test_that("plot_forest_indirectness draws a placeholder when length mismatches", {
  m <- mk_simple_meta(4)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # length 2 -> mismatch; should draw a fallback message rather than abort
  expect_silent(plot_forest_indirectness(m, indirectness = c("low", "high")))
})

test_that("plot_forest_indirectness rejects non-meta objects", {
  expect_error(
    plot_forest_indirectness(list(), indirectness = "low"),
    "must be a meta-analysis object"
  )
})

# --------------------------------------------------------------------------
# plot_forest_pubias_subgroup
# --------------------------------------------------------------------------
test_that("plot_forest_pubias_subgroup falls back to plot_forest when missing_df is empty", {
  m <- mk_simple_meta(5)
  empty_df <- data.frame(studlab = character(0),
                          n = integer(0),
                          results_known = character(0),
                          stringsAsFactors = FALSE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(plot_forest_pubias_subgroup(m, missing_df = empty_df))
})

test_that("plot_forest_pubias_subgroup draws two-subgroup forest", {
  m <- mk_simple_meta(6)
  miss_df <- data.frame(
    studlab = c("Stefanelli 2013", "Tenconi 2017"),
    n = c(40L, 50L),
    results_known = c(
      "Measured but not reported (suspect P > 0.05)",
      "Measured but not reported (suspect P > 0.05)"
    ),
    stringsAsFactors = FALSE
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(plot_forest_pubias_subgroup(m, missing_df = miss_df))
})

test_that("plot_forest_pubias_subgroup rejects malformed missing_df", {
  m <- mk_simple_meta(4)
  bad_df <- data.frame(study = "X", n = 10, status = "Not measured",
                       stringsAsFactors = FALSE)
  expect_error(
    plot_forest_pubias_subgroup(m, missing_df = bad_df),
    "studlab|n|results_known"
  )
})

test_that("plot_forest_pubias_subgroup rejects non-meta objects", {
  miss_df <- data.frame(studlab = "X", n = 10L,
                         results_known = "Not measured",
                         stringsAsFactors = FALSE)
  expect_error(
    plot_forest_pubias_subgroup(list(), missing_df = miss_df),
    "must be a meta-analysis object"
  )
})

test_that("plot_forest_pubias_subgroup auto-detects NA-TE rows as Missing", {
  # A study with all-zero events in both arms produces a non-finite TE in
  # metabin. It should be auto-classified into the Missing subgroup.
  m <- meta::metabin(
    event.e = c(10, 0,  20),
    n.e     = c(50, 50, 70),
    event.c = c(15, 0,  25),
    n.c     = c(50, 50, 70),
    studlab = c("S1", "Lopez2019", "S3"),
    sm      = "OR",
    method  = "Inverse",
    random  = TRUE,
    common  = FALSE,
    incr    = 0  # disable continuity correction so the all-zero row stays NA
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(plot_forest_pubias_subgroup(m))
  # A user-supplied missing row should be combined with the auto-detected one.
  miss_df <- data.frame(studlab = "Extra", n = 100L,
                         results_known = "Not measured",
                         stringsAsFactors = FALSE)
  expect_silent(plot_forest_pubias_subgroup(m, missing_df = miss_df))
})

test_that("plot_forest_pubias_subgroup with no missing_df arg works", {
  # missing_df is now optional; passing nothing should fall back to a
  # standard one-panel forest with N column shown.
  m <- mk_simple_meta(5)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(plot_forest_pubias_subgroup(m))
})

test_that("plot_forest_pubias_subgroup accepts display options", {
  m <- mk_simple_meta(6)
  miss_df <- data.frame(
    studlab = "Stefanelli 2013", n = 40L,
    results_known = "Measured but not reported (suspect P > 0.05)",
    stringsAsFactors = FALSE
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(plot_forest_pubias_subgroup(
    m, missing_df = miss_df,
    title        = "Test title",
    label_e      = "CBT-I",
    label_c      = "Control",
    favors_left  = "Favors Control",
    favors_right = "Favors CBT-I",
    xlim         = c(0.1, 10),
    addrow_above = 1,
    addrow_below = 1
  ))
  # No-missing fallback also accepts display options.
  expect_silent(plot_forest_pubias_subgroup(
    m,
    title        = "Fallback title",
    favors_left  = "L", favors_right = "R",
    xlim         = c(0.2, 5),
    show_n       = FALSE,
    addrow_above = 1, addrow_below = 1
  ))
})

test_that("plot_forest_pubias_subgroup respects auto_detect = FALSE", {
  # When the caller drives the missing list (e.g. a Shiny editor),
  # auto_detect = FALSE skips internal NA-TE detection: NA-TE rows are
  # excluded from Available but NOT auto-added to Missing.
  m <- meta::metabin(
    event.e = c(10, 0,  20),
    n.e     = c(50, 50, 70),
    event.c = c(15, 0,  25),
    n.c     = c(50, 50, 70),
    studlab = c("S1", "Lopez2019", "S3"),
    sm = "OR", method = "Inverse",
    random = TRUE, common = FALSE, incr = 0
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  empty_df <- data.frame(studlab = character(0), n = integer(0),
                          results_known = character(0),
                          stringsAsFactors = FALSE)
  # Empty + auto_detect = FALSE -> no Missing rows at all -> standard forest.
  expect_silent(plot_forest_pubias_subgroup(m, missing_df = empty_df,
                                             auto_detect = FALSE))
  # Caller-supplied row + auto_detect = FALSE -> only the supplied row in Missing.
  miss_df <- data.frame(studlab = "Lopez2019", n = 100L,
                         results_known = "Custom label",
                         stringsAsFactors = FALSE)
  expect_silent(plot_forest_pubias_subgroup(m, missing_df = miss_df,
                                             auto_detect = FALSE))
})

# --------------------------------------------------------------------------
# What reaches meta::forest() - checked without eyes
# --------------------------------------------------------------------------
# The mock records every forest() call's arguments; the first is the one the
# function means to draw (a second only happens on its error fallback).
capture_subgroup_forest <- function(...) {
  forest_calls <- list()
  local_mocked_bindings(
    forest = function(x, ...) {
      forest_calls[[length(forest_calls) + 1L]] <<- list(x = x, ...)
      invisible(TRUE)
    },
    .package = "meta")
  plot_forest_pubias_subgroup(...)
  forest_calls
}

SUBGROUP_MISSING_ROWS <- data.frame(
  studlab = c("Missing A", "Missing B"), n = c(120L, NA),
  results_known = c("Measured but not reported (suspect P > 0.05)",
                    "Not measured"),
  stringsAsFactors = FALSE)

test_that("the subgroup forest labels its sample-size column 'N'", {
  forest_calls <- capture_subgroup_forest(mk_simple_meta(6),
                                          missing_df = SUBGROUP_MISSING_ROWS)
  expect_length(forest_calls, 1L)
  forest_args <- forest_calls[[1]]
  expect_identical(forest_args$leftlabs, c("Study", "N"))
  expect_false("No" %in% forest_args$leftlabs)
  n_column <- forest_args$x[[forest_args$leftcols[[2]]]]
  expect_identical(as.integer(n_column), c(rep(200L, 6), 120L, NA))
})

test_that("show_results_known = FALSE blanks the missing rows' effect cell", {
  shown <- capture_subgroup_forest(mk_simple_meta(6),
                                   missing_df = SUBGROUP_MISSING_ROWS)[[1]]
  expect_identical(tail(shown$x$effect_status, 2),
                   SUBGROUP_MISSING_ROWS$results_known)

  blank <- capture_subgroup_forest(mk_simple_meta(6),
                                   missing_df = SUBGROUP_MISSING_ROWS,
                                   show_results_known = FALSE)[[1]]
  expect_identical(tail(blank$x$effect_status, 2), c("", ""))
  # The available studies keep their effect text either way.
  expect_identical(head(blank$x$effect_status, 6),
                   head(shown$x$effect_status, 6))
  expect_true(all(nzchar(head(blank$x$effect_status, 6))))
})

test_that("one pooled diamond and one heterogeneity line, for the available", {
  forest_args <- capture_subgroup_forest(
    mk_simple_meta(6), missing_df = SUBGROUP_MISSING_ROWS,
    favors_left = "Favours intervention",
    favors_right = "Favours control")[[1]]
  # The Available subgroup's pool and heterogeneity line are drawn ...
  expect_true(forest_args$random)
  expect_true(forest_args$random.subgroup)
  expect_true(forest_args$subgroup.hetstat)
  # ... and the overall pool, which repeats it under the Missing list, is not.
  expect_false(forest_args$overall)
  expect_false(forest_args$overall.hetstat)
  expect_false(forest_args$test.overall)
  expect_false(forest_args$test.subgroup)
  expect_false(forest_args$common)
  # Missing rows carry no estimate, so the Available pool IS the overall
  # pool: dropping the overall diamond loses no number.
  pooled <- forest_args$x
  expect_equal(pooled$TE.random.w[["Available results"]], pooled$TE.random)
  expect_true(all(is.na(pooled$TE[pooled$subgroup == "Missing results"])))
})

test_that("a caller can still ask for the overall summary through ...", {
  forest_args <- capture_subgroup_forest(
    mk_simple_meta(6), missing_df = SUBGROUP_MISSING_ROWS,
    overall = TRUE)[[1]]
  expect_true(forest_args$overall)
})
