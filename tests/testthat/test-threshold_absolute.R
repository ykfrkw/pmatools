# test-threshold_absolute.R — absolute (ARD) decision thresholds for
# ratio-scale binary outcomes

library(testthat)

skip_if_not_installed("meta")

# Pooled CER = (18 + 27) / (100 + 150) = 45/250 = 0.18
make_metabin_ard <- function(sm = "RR") {
  meta::metabin(
    event.e = c(12, 20),
    n.e     = c(100, 150),
    event.c = c(18, 27),
    n.c     = c(100, 150),
    studlab = c("Study A", "Study B"),
    sm      = sm,
    method  = "MH"
  )
}

# --------------------------------------------------------------------------
# threshold_to_te_scale(): ARD -> ratio conversion
# --------------------------------------------------------------------------

test_that("ARD threshold converts to log RR at explicit baseline risk", {
  out <- threshold_to_te_scale(0.05, "ard", "RR", threshold_baseline = 0.18)
  expect_equal(out$threshold_internal, log(0.23 / 0.18))
  expect_equal(out$threshold_kind, "ard")
  expect_equal(out$threshold_ard, 0.05)
  expect_equal(out$threshold_baseline, 0.18)
  expect_match(out$threshold_note, "50 per 1000", fixed = TRUE)
  expect_match(out$threshold_note, "180 per 1000", fixed = TRUE)
  expect_match(out$threshold_note, "RR 1.28", fixed = TRUE)
})

test_that("ARD threshold converts to log OR via odds at baseline risk", {
  out <- threshold_to_te_scale(0.10, "ard", "OR", threshold_baseline = 0.20)
  expect_equal(out$threshold_internal, log((0.3 / 0.7) / (0.2 / 0.8)))
  expect_equal(out$threshold_ard, 0.10)
  expect_match(out$threshold_note, "OR 1.71", fixed = TRUE)
})

test_that("ARD threshold with HR uses RR approximation with caveat note", {
  out <- threshold_to_te_scale(0.05, "ard", "HR", threshold_baseline = 0.18)
  expect_equal(out$threshold_internal, log(0.23 / 0.18))
  expect_match(out$threshold_note, "HR approximated as RR", fixed = TRUE)
})

test_that("baseline risk falls back to pooled control event rate", {
  m <- make_metabin_ard()
  out <- threshold_to_te_scale(0.05, "ard", "RR", meta_obj = m)
  expect_equal(out$threshold_baseline, 0.18)
  expect_equal(out$threshold_internal, log(0.23 / 0.18))
})

test_that("missing baseline risk raises an actionable error", {
  # metagen: no control-arm event data, no explicit baseline
  mg <- suppressWarnings(
    meta::metagen(TE = log(c(0.8, 0.9)), seTE = c(0.2, 0.25), sm = "RR")
  )
  expect_error(
    threshold_to_te_scale(0.05, "ard", "RR", meta_obj = mg),
    "threshold_baseline"
  )
  expect_error(
    threshold_to_te_scale(0.05, "ard", "RR"),
    "threshold_baseline"
  )
})

test_that("invalid baseline / ARD inputs error clearly", {
  expect_error(
    threshold_to_te_scale(0.05, "ard", "RR", threshold_baseline = 1.5),
    "between 0 and 1"
  )
  expect_error(
    threshold_to_te_scale(-0.05, "ard", "RR", threshold_baseline = 0.18),
    "positive"
  )
  # p0 + ARD >= 1 is impossible as a risk
  expect_error(
    threshold_to_te_scale(0.05, "ard", "RR", threshold_baseline = 0.97),
    ">= 1"
  )
})

test_that("non-ARD scales are unchanged and carry NULL ARD fields", {
  out <- threshold_to_te_scale(1.25, "ratio", "OR")
  expect_equal(out$threshold_internal, log(1.25))
  expect_equal(out$threshold_kind, "ratio")
  expect_null(out$threshold_ard)
  expect_null(out$threshold_note)
  expect_null(out$threshold_baseline)

  out2 <- threshold_to_te_scale(0.20, "auto", "SMD")
  expect_equal(out2$threshold_internal, 0.20)
  expect_null(out2$threshold_ard)

  # sm = "ARD": pass-through preserved (no conversion, no note)
  out3 <- threshold_to_te_scale(0.05, "ard", "ARD")
  expect_equal(out3$threshold_internal, 0.05)
  expect_equal(out3$threshold_kind, "ard")
  expect_null(out3$threshold_ard)
  expect_null(out3$threshold_note)
})

# --------------------------------------------------------------------------
# grade_meta() integration
# --------------------------------------------------------------------------

test_that("grade_meta converts ARD threshold and OIS uses p1 = p0 + ARD", {
  m <- make_metabin_ard()
  g <- suppressWarnings(grade_meta(
    m,
    threshold          = 0.05,
    threshold_scale    = "ard",
    threshold_baseline = 0.18,
    outcome_name       = "ARD outcome"
  ))

  expect_equal(g$threshold_internal, log(0.23 / 0.18))
  expect_equal(g$threshold_ard, 0.05)
  expect_equal(g$threshold_baseline, 0.18)
  expect_match(g$threshold_note, "equivalent RR 1.28", fixed = TRUE)

  d <- g$domain_assessments
  impre_notes <- d$notes[d$domain == "Imprecision"]
  # OIS derivation must use the raw ARD (p1 = 0.18 + 0.05 = 0.23),
  # not the log-converted threshold value.
  expect_match(impre_notes, "ois_p1 = 0.2300", fixed = TRUE)
  expect_match(impre_notes, "p1=0.230", fixed = TRUE)
  expect_match(impre_notes, "p0=0.180", fixed = TRUE)

  # Conversion note flows into all three Threshold-aware domains
  for (dom in c("Risk of bias", "Inconsistency", "Imprecision")) {
    expect_match(
      d$notes[d$domain == dom],
      "Absolute threshold 50 per 1000 at baseline risk 180 per 1000",
      fixed = TRUE
    )
  }
})

test_that("grade_meta without threshold_baseline uses pooled CER", {
  m <- make_metabin_ard()
  g <- suppressWarnings(grade_meta(m, threshold = 0.05,
                                   threshold_scale = "ard"))
  expect_equal(g$threshold_baseline, 0.18)
  expect_equal(g$threshold_internal, log(0.23 / 0.18))
})

test_that("relative-scale grade_meta results carry no ARD fields", {
  m <- make_metabin_ard()
  g <- suppressWarnings(grade_meta(m, threshold = 1.25,
                                   threshold_scale = "ratio"))
  expect_equal(g$threshold_internal, log(1.25))
  expect_null(g$threshold_ard)
  expect_null(g$threshold_note)
  expect_null(g$threshold_baseline)
  expect_false(any(grepl("Absolute threshold",
                         g$domain_assessments$notes), na.rm = TRUE))
})

test_that("explicit ois_p0/ois_p1 take precedence over ARD derivation", {
  m <- make_metabin_ard()
  g <- suppressWarnings(grade_meta(
    m,
    threshold          = 0.05,
    threshold_scale    = "ard",
    threshold_baseline = 0.18,
    ois_p0             = 0.30,
    ois_p1             = 0.40
  ))
  d <- g$domain_assessments
  impre_notes <- d$notes[d$domain == "Imprecision"]
  expect_match(impre_notes, "p0=0.300", fixed = TRUE)
  expect_match(impre_notes, "p1=0.400", fixed = TRUE)
})

# --------------------------------------------------------------------------
# suggest_threshold(): absolute suggestion
# --------------------------------------------------------------------------

test_that("suggest_threshold keeps ratio default and adds absolute suggestion", {
  m <- make_metabin_ard()
  s <- suggest_threshold(m)
  expect_equal(s$threshold_user, 1.20)
  expect_equal(s$threshold_scale, "ratio")
  expect_equal(s$threshold_absolute$threshold_user, 0.05)
  expect_equal(s$threshold_absolute$threshold_scale, "ard")
})

# --------------------------------------------------------------------------
# evidence_profile(): threshold note footnote
# --------------------------------------------------------------------------

test_that("evidence_profile carries the threshold note as a footer line", {
  skip_if_not_installed("flextable")
  m <- make_metabin_ard()
  g <- suppressWarnings(grade_meta(
    m,
    threshold          = 0.05,
    threshold_scale    = "ard",
    threshold_baseline = 0.18,
    outcome_name       = "ARD outcome"
  ))
  ft <- evidence_profile(g)
  expect_s3_class(ft, "flextable")
  footer_txt <- unlist(ft$footer$dataset, use.names = FALSE)
  expect_true(any(grepl("Absolute threshold 50 per 1000", footer_txt,
                        fixed = TRUE)))
})
