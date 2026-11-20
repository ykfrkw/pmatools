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

test_that("the status text has its own 'Results known' column, or none", {
  shown <- capture_subgroup_forest(mk_simple_meta(6),
                                   missing_df = SUBGROUP_MISSING_ROWS)[[1]]
  expect_identical(shown$rightcols, c("effect.ci", "results_known"))
  expect_identical(shown$rightlabs, c("OR (95% CI)", "Results known"))
  expect_identical(shown$x$results_known,
                   c(rep("", 6), SUBGROUP_MISSING_ROWS$results_known))

  hidden <- capture_subgroup_forest(mk_simple_meta(6),
                                    missing_df = SUBGROUP_MISSING_ROWS,
                                    show_results_known = FALSE)[[1]]
  expect_identical(hidden$rightcols, "effect.ci")
  expect_identical(hidden$rightlabs, "OR (95% CI)")
})

test_that("missing rows carry no estimate, so their effect cell is blank", {
  forest_args <- capture_subgroup_forest(mk_simple_meta(6),
                                         missing_df = SUBGROUP_MISSING_ROWS)[[1]]
  is_missing_row <- forest_args$x$subgroup == "Missing results"
  expect_true(all(is.na(forest_args$x$TE[is_missing_row])))
  expect_true(all(is.na(forest_args$x$lower[is_missing_row])))
  expect_true(all(is.finite(forest_args$x$TE[!is_missing_row])))
})

# The plotted Available pool must be the analysis' own pool.
expect_same_pool <- function(plotted, meta_obj) {
  available <- "Available results"
  expect_equal(
    unname(c(plotted$TE.random.w[available], plotted$lower.random.w[available],
             plotted$upper.random.w[available], plotted$lower.predict.w[available],
             plotted$upper.predict.w[available])),
    # [1]: an analysis asking for several CI methods stores one value each.
    unname(c(meta_obj$TE.random[1], meta_obj$lower.random[1],
             meta_obj$upper.random[1], meta_obj$lower.predict[1],
             meta_obj$upper.predict[1])))
}

mk_cont_meta <- function(sm = "MD") {
  meta::metacont(
    n.e = rep(30, 6), mean.e = c(10, 12, 9, 11, 8, 10),
    sd.e = c(8, 9, 10, 7, 9, 8),
    n.c = rep(30, 6), mean.c = c(15, 14, 20, 13, 18, 22),
    sd.c = c(8, 9, 10, 7, 9, 8),
    studlab = paste0("S", 1:6), sm = sm,
    method.tau = "REML", method.random.ci = "HK", prediction = TRUE)
}

test_that("the plotted pool, CI and PI are the analysis' own (REML + HK)", {
  meta_obj <- mk_cont_meta()
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  plotted <- plot_forest_pubias_subgroup(meta_obj,
                                         missing_df = SUBGROUP_MISSING_ROWS)
  expect_same_pool(plotted, meta_obj)
  expect_identical(plotted$method.random.ci, "HK")
  expect_identical(plotted$method.tau, "REML")
  # The defaults (DerSimonian-Laird, classic CI) would give a narrower CI.
  plain_refit <- meta::metagen(meta_obj$TE, meta_obj$seTE, common = FALSE)
  expect_false(isTRUE(all.equal(plain_refit$lower.random,
                                meta_obj$lower.random)))
})

test_that("a ratio measure keeps the analysis' pool on the log scale", {
  rom_meta <- mk_cont_meta(sm = "ROM")
  or_meta  <- meta::metabin(c(20, 18, 15, 12, 10, 8), rep(100, 6),
                            c(20, 19, 17, 15, 13, 11), rep(100, 6),
                            sm = "OR", method = "MH",
                            method.tau = "REML", method.random.ci = "HK")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (meta_obj in list(rom_meta, or_meta)) {
    plotted <- plot_forest_pubias_subgroup(meta_obj,
                                           missing_df = SUBGROUP_MISSING_ROWS)
    expect_same_pool(plotted, meta_obj)
    expect_identical(plotted$sm, meta_obj$sm)
    expect_true(plotted$backtransf)
  }
})

test_that("auto-detected NA-TE rows leave the pool unchanged", {
  meta_obj <- meta::metabin(
    event.e = c(10, 0, 20, 5, 7), n.e = c(50, 50, 70, 40, 40),
    event.c = c(15, 0, 25, 9, 8), n.c = c(50, 50, 70, 40, 40),
    studlab = c("S1", "Lopez2019", "S3", "S4", "S5"),
    sm = "OR", method = "Inverse", incr = 0,
    method.tau = "REML", method.random.ci = "HK")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  plotted <- plot_forest_pubias_subgroup(meta_obj)
  expect_same_pool(plotted, meta_obj)
  expect_identical(plotted$studlab[plotted$subgroup == "Missing results"],
                   "Lopez2019")
})

test_that("excluded studies are drawn in Available but not pooled", {
  meta_obj <- meta::metacont(
    n.e = rep(30, 6), mean.e = c(10, 12, 9, 11, 8, 10),
    sd.e = c(8, 9, 10, 7, 9, 8),
    n.c = rep(30, 6), mean.c = c(15, 14, 20, 13, 18, 22),
    sd.c = c(8, 9, 10, 7, 9, 8),
    exclude = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    method.tau = "REML", method.random.ci = "HK")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  plotted <- plot_forest_pubias_subgroup(meta_obj,
                                         missing_df = SUBGROUP_MISSING_ROWS)
  expect_same_pool(plotted, meta_obj)
  expect_identical(plotted$exclude, c(FALSE, TRUE, rep(FALSE, 6)))
})

test_that("the subgroup PI is drawn on request, the overall one never", {
  shown <- capture_subgroup_forest(mk_cont_meta(),
                                   missing_df = SUBGROUP_MISSING_ROWS)[[1]]
  expect_true(shown$prediction.subgroup)
  expect_false(shown$prediction)

  hidden <- capture_subgroup_forest(mk_cont_meta(),
                                    missing_df = SUBGROUP_MISSING_ROWS,
                                    prediction = FALSE)[[1]]
  expect_false(hidden$prediction.subgroup)
  expect_false(hidden$prediction)

  # Fewer than three pooled studies: {meta} has no PI to draw.
  two_studies <- capture_subgroup_forest(mk_simple_meta(2),
                                         missing_df = SUBGROUP_MISSING_ROWS)[[1]]
  expect_false(two_studies$prediction.subgroup)
})

test_that("prediction and show_results_known switches all draw silently", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (draw_prediction in c(TRUE, FALSE)) {
    for (show_status in c(TRUE, FALSE)) {
      expect_silent(plot_forest_pubias_subgroup(
        mk_cont_meta(), missing_df = SUBGROUP_MISSING_ROWS,
        prediction = draw_prediction, show_results_known = show_status))
    }
  }
})

test_that("a pool the refit cannot reproduce warns and still draws", {
  skip_if_not_installed("lme4")
  glmm_meta <- suppressWarnings(meta::metabin(
    c(20, 18, 15, 12, 10, 8), rep(100, 6),
    c(20, 19, 17, 15, 13, 11), rep(100, 6), sm = "OR", method = "GLMM"))
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(
    plotted <- plot_forest_pubias_subgroup(glmm_meta,
                                           missing_df = SUBGROUP_MISSING_ROWS),
    "differs from the analysis")
  expect_s3_class(plotted, "meta")
})

# Heterogeneous enough that Mantel-Haenszel + DerSimonian-Laird gives a tau
# metagen does not reproduce without the tau.preset retry.
mk_mh_dl_meta <- function(...) {
  meta::metabin(c(5, 30, 12, 40, 8, 25), rep(100, 6),
                c(20, 25, 30, 20, 30, 22), rep(100, 6),
                studlab = paste0("S", 1:6), sm = "RR", method = "MH",
                method.tau = "DL", ...)
}

test_that("Mantel-Haenszel + DL is reproduced through the tau.preset retry", {
  meta_obj <- mk_mh_dl_meta()
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(plotted <- plot_forest_pubias_subgroup(
    meta_obj, missing_df = SUBGROUP_MISSING_ROWS))
  expect_same_pool(plotted, meta_obj)
})

test_that("a failed tau.preset retry warns instead of aborting", {
  meta_obj <- mk_mh_dl_meta(exclude = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(
    plotted <- plot_forest_pubias_subgroup(meta_obj,
                                           missing_df = SUBGROUP_MISSING_ROWS),
    "differs from the analysis")
  expect_s3_class(plotted, "meta")
})

test_that("several requested CI methods pool with the first one", {
  meta_obj <- meta::metabin(c(20, 18, 15, 12, 10, 8), rep(100, 6),
                            c(20, 19, 17, 15, 13, 11), rep(100, 6),
                            sm = "OR", method = "Inverse", method.tau = "REML",
                            method.random.ci = c("classic", "HK"))
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  plotted <- plot_forest_pubias_subgroup(meta_obj,
                                         missing_df = SUBGROUP_MISSING_ROWS)
  expect_same_pool(plotted, meta_obj)
})

test_that("the no-missing fallback returns meta_obj invisibly", {
  meta_obj <- mk_simple_meta(5)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_invisible(returned <- plot_forest_pubias_subgroup(meta_obj))
  expect_identical(returned, meta_obj)
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
