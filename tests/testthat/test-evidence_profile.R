library(testthat)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

make_metabin_ep <- function() {
  meta::metabin(
    event.e = c(10, 15, 20),
    n.e     = c(50, 60, 70),
    event.c = c(15, 20, 25),
    n.c     = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm      = "RR",
    method  = "MH"
  )
}

make_grade_ep <- function() {
  m <- make_metabin_ep()
  suppressWarnings(grade_meta(m, study_design = "RCT", rob = "no",
                              indirectness = "no",
                              outcome_name = "EP Outcome"))
}

test_that("evidence_profile returns a flextable", {
  g  <- make_grade_ep()
  ft <- evidence_profile(g)
  expect_s3_class(ft, "flextable")
})

test_that("evidence_profile accepts the classic palette and custom design label", {
  g  <- make_grade_ep()
  ft <- evidence_profile(g, palette = "classic",
                         study_design = "randomised trials")
  expect_s3_class(ft, "flextable")
})

test_that("evidence_profile applies other_text / other_downgrade", {
  g  <- make_grade_ep()
  ft <- evidence_profile(g,
                         other_text      = "Strong plausible confounding",
                         other_downgrade = -1L)
  expect_s3_class(ft, "flextable")
})

test_that("evidence_profile rejects non-pmatools input", {
  expect_error(evidence_profile(list(a = 1)), regexp = "pmatools")
})
