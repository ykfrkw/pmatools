library(testthat)

skip_if_not_installed("meta")

# Fixtures: real metabin objects with Egger p-values pre-calibrated to fall in
# each of the three 2-tier buckets. The p-values are recomputed at fixture
# build time and verified by separate sanity-check tests below, so the buckets
# stay correct if {meta} updates the metabias internals.
mk_metabin <- function(ee, ec, ne, nc) {
  meta::metabin(ee, ne, ec, nc, sm = "OR",
                studlab = paste0("S", seq_along(ee)),
                random = TRUE, method = "Inverse")
}

make_strong_asymmetry <- function() {
  ev_e <- c(20, 18, 15, 12, 10,  8,  5,  3,  2,  1)
  ev_c <- c(20, 19, 17, 15, 13, 11,  8,  6,  4,  2)
  n_e  <- rep(500, 10) - c(0, 0, 100, 200, 300, 350, 400, 430, 450, 470)
  n_c  <- n_e
  mk_metabin(ev_e, ev_c, n_e, n_c)  # Egger p ~ 0.0003 (-2 bucket)
}

make_mild_asymmetry <- function() {
  ev_e <- c(15, 14, 13, 12, 11, 10,  9,  8,  7,  6)
  ev_c <- rep(13, 10)
  n_e  <- c(50, 60, 80, 100, 150, 200, 300, 400, 500, 600)
  mk_metabin(ev_e, ev_c, n_e, n_e) # Egger p ~ 0.014 (-1 bucket)
}

make_symmetric <- function() {
  set.seed(7)
  n    <- rep(c(40, 80, 160, 320, 640), 2)
  ev_e <- pmin(round(n * 0.30 + rnorm(10, 0, 3)), n - 1)
  ev_c <- pmin(round(n * 0.30 + rnorm(10, 0, 3)), n - 1)
  mk_metabin(ev_e, ev_c, n, n)     # Egger p ~ 0.75 (no rate-down)
}

make_small_meta <- function() {
  # k = 3, all moderate-sized; Egger not applicable (k < 10)
  meta::metabin(
    c(20, 25, 30), c(100, 100, 100),
    c(15, 18, 22), c(100, 100, 100),
    studlab = c("A", "B", "C"),
    sm = "OR", random = TRUE, method = "Inverse"
  )
}

# --- Registry rule-out, evaluated after Q1 ----------------------------------
# Updated (v0.5): Core GRADE 4 Fig 5 has no structural rule-out node, so
# pubias_registry_complete is now consumed AFTER Q1 and can no longer suppress
# the small-and-industry-sponsored downgrade.
test_that("pubias_registry_complete = 'yes' short-circuits the Fig 5 nodes after Q1", {
  m <- make_strong_asymmetry()  # would otherwise rate down via Egger
  g <- grade_meta(m, pubias_registry_complete = "yes",
                  pubias_small_industry = "no", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "not_serious")
  expect_match(pb$notes, "pubias_registry_complete = 'yes'", fixed = TRUE)
  expect_match(pb$notes, "not a decision node of Core GRADE 4 Fig 5", fixed = TRUE)
})

test_that("Q1 rate-down survives pubias_registry_complete = 'yes' (k >= 10)", {
  m <- make_strong_asymmetry()
  g <- grade_meta(m, pubias_registry_complete = "yes",
                  pubias_small_industry = "yes", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "serious")
  expect_match(pb$notes, "Q1:")
  expect_match(pb$notes, "evaluated only after Q1", fixed = TRUE)
})

test_that("Q1 rate-down survives pubias_registry_complete = 'yes' when k < 10", {
  # Acceptance case H: small + industry-sponsored, complete registry, k < 10.
  m <- make_symmetric()  # k = 3
  g <- grade_meta(m, pubias_small_industry = "yes",
                  pubias_registry_complete = "yes", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "serious")
  expect_equal(pb$downgrade, -1L)
})

# --- Q1: small + industry-sponsored -----------------------------------------
test_that("Q1: pubias_small_industry = 'yes' -> some_concerns", {
  m <- make_symmetric()
  g <- grade_meta(m, pubias_small_industry = "yes", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "serious")
  expect_match(pb$notes, "Q1:")
})

# --- Q3 (k >= 10): single-tier auto Egger -----------------------------------
# Updated (v0.5): the p < 0.01 -> serious (-2) tier was removed. Core GRADE 4
# never describes a two-level publication-bias downgrade, and Fig 5's asymmetry
# node is qualitative ("strongly suggests") with no p-value cut-off.
test_that("Q3 auto: very small Egger p still rates down only one level", {
  m <- make_strong_asymmetry()
  g <- grade_meta(m, threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "serious")
  expect_equal(pb$downgrade, -1L)
  expect_match(pb$notes, "p < 0.05")
  expect_match(pb$notes, "pmatools operational convention", fixed = TRUE)
})

test_that("Q3 auto: Egger p < 0.05 -> some_concerns (-1)", {
  m <- make_mild_asymmetry()
  g <- grade_meta(m, threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "serious")
  expect_match(pb$notes, "p < 0.05")
})

test_that("Q3 auto: Egger p >= 0.05 -> no", {
  m <- make_symmetric()
  g <- grade_meta(m, threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "not_serious")
  expect_match(pb$notes, "p >= 0.05")
})

# --- Q3 (k >= 10): manual visual override -----------------------------------
test_that("Q3 manual 'yes' -> some_concerns regardless of Egger", {
  m <- make_symmetric()  # Egger says no
  g <- grade_meta(m, pubias_funnel_asymmetry = "yes",
                  pubias_rationale = "Contour-enhanced funnel plot visually asymmetric", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "serious")
  expect_match(pb$notes, "manual")
})

test_that("Q3 manual 'no' -> no regardless of Egger", {
  m <- make_strong_asymmetry()  # Egger says serious
  g <- grade_meta(m, pubias_funnel_asymmetry = "no",
                  pubias_rationale = "Asymmetry driven by heterogeneity, not small-study bias", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "not_serious")
  expect_match(pb$notes, "rules out funnel-plot asymmetry")
})

# --- Q4 (k < 10) ------------------------------------------------------------
test_that("Q4: k < 10, pubias_unpublished = 'yes' -> some_concerns", {
  m <- make_small_meta()
  g <- grade_meta(m, pubias_unpublished = "yes", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "serious")
  expect_match(pb$notes, "Q4:")
})

test_that("Q4: k < 10, pubias_unpublished = 'no' -> no", {
  m <- make_small_meta()
  g <- grade_meta(m, pubias_unpublished = "no", threshold_type = "null")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "not_serious")
})

# --- Not formally assessed -> qualitative-assessment note -------------------
test_that("Egger not computable (k >= 10) -> 'no' with prominent qualitative note", {
  # A plain list makes meta::metabias() error out inside tryCatch,
  # exercising the Egger-failure branch directly.
  row <- NULL
  expect_warning(
    row <- .pubias_statistical(meta_obj = list(), k = 12,
                               pubias_funnel_asymmetry = NULL,
                               q1_note = "Q1: test. "),
    regexp = "qualitative assessment is required"
  )
  expect_equal(row$judgment, "not_serious")
  expect_equal(row$downgrade, 0)
  expect_match(row$notes, "QUALITATIVE ASSESSMENT REQUIRED")
  expect_match(row$notes, "funnel plot")
  expect_match(row$notes, "registry")
})

test_that("k < 10 with no manual input -> note flags qualitative assessment", {
  m <- make_small_meta()
  g <- NULL
  expect_warning(g <- grade_meta(m, threshold_type = "null"), regexp = "pubias_unpublished")
  pb <- g$domain_assessments[g$domain_assessments$domain == "Publication bias", ]
  expect_equal(pb$judgment, "not_serious")
  expect_match(pb$notes, "QUALITATIVE ASSESSMENT REQUIRED")
  expect_false(is.null(.pubias_qualitative_note(g)))
})

test_that("manual pubias input suppresses the qualitative marker", {
  m <- make_small_meta()
  g <- grade_meta(m, pubias_unpublished = "no", threshold_type = "null")
  expect_null(.pubias_qualitative_note(g))

  m10 <- make_symmetric()
  g10 <- grade_meta(m10, threshold_type = "null")  # Egger runs fine
  expect_null(.pubias_qualitative_note(g10))
})

# --- meta object passthrough + plot helpers ---------------------------------
test_that("grade_meta() exposes the meta object via $meta and plot helpers accept pmatools", {
  m <- make_symmetric()
  g <- grade_meta(m, threshold_type = "null")
  expect_true(inherits(g$meta, "meta"))
  # plot_funnel and plot_trimfill_forest must accept a pmatools object.
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot_funnel(g))
  expect_invisible(plot_trimfill_forest(g))
})

test_that("plot_trimfill_forest annotates the adjusted pooled estimate", {
  m <- make_strong_asymmetry()  # trim-and-fill imputes >= 1 study here
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  tf <- plot_trimfill_forest(m)
  expect_true(inherits(tf, "meta"))
  expect_true(!is.null(tf$k0) && tf$k0 >= 1L)
})
