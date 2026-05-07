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
