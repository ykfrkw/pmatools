library(testthat)

skip_if_not_installed("meta")

# A study reporting a denominator but no event count (eg it contributed a
# continuous outcome only) used to be dropped from event.c while staying in
# n.c. That left the two vectors different lengths, metaprop() errored, and the
# tryCatch handed back the crude proportion labelled as a pooled estimate.
gappy_metabin <- function() {
  d <- data.frame(
    studlab = paste0("S", 1:6),
    event_e = c(12,  30,  4, 18,  9, 21),
    n_e     = c(60, 140, 55, 90, 70, 110),
    event_c = c(20,  46, NA,  6, 15, 34),
    n_c     = c(60, 140, 58, 90, 70, 110)
  )
  suppressWarnings(
    meta::metabin(d$event_e, d$n_e, d$event_c, d$n_c, d$studlab,
                  sm = "OR", method = "Inverse", method.tau = "REML")
  )
}

test_that("a study with a missing event.c but a present n.c keeps both vectors aligned", {
  m <- gappy_metabin()

  # Precondition: the object really does carry the ragged pair.
  expect_equal(length(m$event.c), length(m$n.c))
  expect_equal(sum(is.na(m$event.c)), 1L)
  expect_equal(sum(is.na(m$n.c)), 0L)

  keep <- !is.na(m$event.c) & !is.na(m$n.c) & m$n.c > 0
  expect_equal(
    .compute_control_risk(m, method = "simple"),
    sum(m$event.c[keep]) / sum(m$n.c[keep])
  )

  # The old code divided the complete-case numerator by the full denominator,
  # so the incomplete study's 58 controls diluted the crude risk.
  stale <- sum(m$event.c[keep]) / sum(m$n.c)
  expect_false(isTRUE(all.equal(.compute_control_risk(m, method = "simple"), stale)))
})

test_that("metaprop path runs instead of falling back when a study lacks event.c", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("lme4")

  m <- gappy_metabin()

  # The fallback announces itself; reaching it at all is the regression.
  expect_no_warning(res <- .compute_control_risk(m, method = "metaprop"))

  keep <- !is.na(m$event.c) & !is.na(m$n.c) & m$n.c > 0
  fit <- meta::metaprop(event = m$event.c[keep], n = m$n.c[keep],
                        method = "GLMM", sm = "PLOGIT", method.tau = "ML")
  expect_equal(res, stats::plogis(fit$TE.random))

  # ...and it is a genuinely different number from the crude proportion, so a
  # silent fallback would have been visible in a SoF absolute-risk column.
  crude <- sum(m$event.c[keep]) / sum(m$n.c[keep])
  expect_false(isTRUE(all.equal(res, crude, tolerance = 1e-4)))
})

test_that("bundled cbti_depression reaches the metaprop estimate, not the crude one", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("lme4")

  d <- pmatools::cbti_depression
  long <- data.frame(
    studlab = d$study,
    treat   = ifelse(d$treatment == "CBT-I", "experimental", "control"),
    n       = d$n_randomized,
    event   = d$d_r
  )
  m <- run_ma(long, outcome_type = "binary", sm = "OR")

  expect_no_warning(mp <- .compute_control_risk(m, method = "metaprop"))
  simple <- .compute_control_risk(m, method = "simple")

  # ~156 vs ~176 per 1,000: far enough apart to move an absolute-risk column.
  expect_equal(mp * 1000, 155.6, tolerance = 0.5)
  expect_equal(simple * 1000, 175.5, tolerance = 0.5)
})

test_that("control risk is NULL when no study has a usable control arm", {
  d <- data.frame(
    studlab = c("S1", "S2"),
    event_e = c(5, 8), n_e = c(40, 50),
    event_c = c(NA_real_, NA_real_), n_c = c(40, 50)
  )
  m <- suppressWarnings(
    meta::metabin(d$event_e, d$n_e, d$event_c, d$n_c, d$studlab, sm = "OR")
  )

  expect_null(.compute_control_risk(m, method = "simple"))
  expect_null(.compute_control_risk(m, method = "metaprop"))
})

test_that("suggest_threshold covers the risk-difference measure metabin emits", {
  d <- data.frame(
    studlab = paste0("S", 1:3),
    event_e = c(5, 8, 11), n_e = c(40, 50, 60),
    event_c = c(9, 14, 20), n_c = c(40, 50, 60)
  )
  m <- suppressWarnings(
    meta::metabin(d$event_e, d$n_e, d$event_c, d$n_c, d$studlab, sm = "RD")
  )
  expect_identical(m$sm, "RD")

  s <- suggest_threshold(m)
  expect_equal(s$threshold_user, 0.05)
  expect_equal(s$threshold_scale, "ard")
  expect_equal(s$source, "package_convention")

  # threshold_scale = "auto" must resolve the same measure without erroring.
  conv <- threshold_to_te_scale(0.05, "auto", m$sm)
  expect_equal(conv$threshold_kind, "ard")
  expect_equal(conv$threshold_internal, 0.05)
})
