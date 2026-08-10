library(testthat)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

make_common_only_grade <- function() {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = "OR",
               random = FALSE, common = TRUE)
  g  <- suppressWarnings(grade_meta(ma, study_design = "RCT", rob = "no",
                                    rob_rationale = "Consensus RoB2: all domains low risk",
                                    indirectness = "no",
                                    outcome_name = "Common only"))
  g
}

.footer_text <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")

test_that(".pooled_estimate falls back to common when random is absent", {
  g <- make_common_only_grade()
  pooled <- .pooled_estimate(g$meta)
  expect_true(is.finite(pooled$est))
  expect_equal(pooled$est, g$meta$TE.common)
  expect_equal(pooled$lower, g$meta$lower.common)
})

test_that("sof_table shows the effect for a common-effect-only analysis", {
  g  <- make_common_only_grade()
  ft <- sof_table(g)
  eff <- ft$body$dataset[["Effect (95% CI)"]]
  expect_false(identical(eff, "NR"))
  expect_match(eff, "^OR ")
  # Exp. rate column derived from the common-effect pool, not "-"
  ier <- ft$body$dataset[[grep("^Exp. rate", names(ft$body$dataset))]]
  expect_false(identical(ier, "-"))
})

test_that("grade_table shows the effect for a common-effect-only analysis", {
  g  <- make_common_only_grade()
  ft <- grade_table(list("Common only" = g))
  eff <- ft$body$dataset[["Effect (95% CI)"]]
  expect_false(any(eff == "NR"))
})

test_that("random-effects pool still preferred when available", {
  m <- meta::metabin(
    event.e = c(10, 15, 20), n.e = c(50, 60, 70),
    event.c = c(15, 20, 25), n.c = c(50, 60, 70),
    studlab = c("A", "B", "C"), sm = "OR",
    random = TRUE, common = TRUE, method = "Inverse"
  )
  pooled <- .pooled_estimate(m)
  expect_equal(pooled$est, m$TE.random)
})

# --- Publication bias qualitative-assessment note propagation ---------------

test_that("qualitative pubias note propagates to SoF and grade_table footers", {
  g <- make_common_only_grade()  # k = 3 < 10, pubias_unpublished not given
  expect_false(is.null(.pubias_qualitative_note(g)))

  ft_sof <- sof_table(g)
  expect_match(.footer_text(ft_sof), "QUALITATIVE ASSESSMENT REQUIRED")

  ft_gt <- grade_table(list("Common only" = g))
  expect_match(.footer_text(ft_gt), "QUALITATIVE ASSESSMENT REQUIRED")
})

test_that("evidence_profile flags publication bias as not formally assessed", {
  g  <- make_common_only_grade()
  ft <- evidence_profile(g)
  other <- ft$body$dataset[["Other considerations"]]
  expect_match(other, "publication bias not formally assessed")
  expect_match(.footer_text(ft), "QUALITATIVE ASSESSMENT REQUIRED")
})

test_that("no qualitative note when pubias was decided manually", {
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 15, 15, 20, 20, 25),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = "OR")
  g  <- suppressWarnings(grade_meta(ma, pubias_unpublished = "no",
                                    outcome_name = "Manual"))
  expect_null(.pubias_qualitative_note(g))
  ft <- sof_table(g)
  expect_no_match(.footer_text(ft), "QUALITATIVE ASSESSMENT REQUIRED")
})
