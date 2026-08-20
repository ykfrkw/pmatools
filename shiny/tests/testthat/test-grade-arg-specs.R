# pma_grade_arg_specs() (R/outcome_provenance.R) builds the {value, origin} specs that
# export_bundle() renders into the "reproducible" analysis.R.
#
# History worth keeping, because it explains why these tests are pointed at
# absence rather than presence. export_bundle() used to look specs up with
# `grade_args$<name>`, and `$` partial-matches on lists: with `inconsistency`
# absent, `grade_args$inconsistency` returned the `inconsistency_ci_diff` spec
# and wrote one question's answer into another argument of the exported call.
# The app worked around it by emitting every registry name with a null spec.
# pmatools now looks up exactly, so the workaround is gone and only the
# arguments the reviewer actually set are emitted.
#
# The hazard is therefore the package's to prevent, but these tests still pin
# the property that makes an exact lookup correct: an argument the reviewer
# did not set must be ABSENT, not present-and-null. Note that the assertions
# below use [[ ]], never $ -- `$` on the returned list would reintroduce the
# very partial match this file is about.

test_that("only the supplied arguments are emitted, in registry order", {
  expect_identical(pma_grade_arg_specs(list()), list())

  specs <- pma_grade_arg_specs(list(
    small_values = "undesirable",
    threshold    = 1.25
  ))
  # Registry order, not the caller's argument order.
  expect_identical(names(specs),
                   intersect(PMA_GRADE_ARGS_EXPORTED,
                             c("small_values", "threshold")))
})

test_that("an unsupplied prefix name is absent, so an exact lookup finds nothing", {
  # `inconsistency` is a strict prefix of `inconsistency_ci_diff`,
  # `inconsistency_rationale`, `inconsistency_threshold_side` and
  # `inconsistency_subgroup_explained`. Supply only the longer one.
  specs <- pma_grade_arg_specs(list(inconsistency_ci_diff = 0.5))

  expect_false("inconsistency" %in% names(specs))
  expect_null(specs[["inconsistency"]])
  expect_identical(specs[["inconsistency_ci_diff"]]$value, 0.5)

  # The other prefix families behave the same way.
  expect_false("rob" %in% names(pma_grade_arg_specs(list(rob_rationale = "x"))))
  expect_false("threshold" %in%
                 names(pma_grade_arg_specs(list(threshold_scale = "ratio"))))
  expect_false("imprecision" %in%
                 names(pma_grade_arg_specs(list(imprecision_rationale = "x"))))
  expect_false("indirectness" %in%
                 names(pma_grade_arg_specs(
                   list(indirectness_subdomains = data.frame(a = 1)))))
})

test_that("supplied values keep their value and get a valid origin", {
  specs <- pma_grade_arg_specs(list(
    threshold       = 1.25,
    threshold_scale = "ratio",
    rob             = c("low", "high", "*"),
    indirectness_subdomains = data.frame(subdomain = "Population",
                                         judgment  = "no",
                                         stringsAsFactors = FALSE)
  ))

  expect_identical(specs[["threshold"]]$value, 1.25)
  expect_identical(specs[["threshold"]]$origin, "scalar")
  expect_identical(specs[["threshold_scale"]]$origin, "scalar")
  expect_identical(specs[["rob"]]$origin, "vector")
  expect_identical(specs[["rob"]]$value, c("low", "high", "*"))
  expect_identical(specs[["indirectness_subdomains"]]$origin, "scalar")

  # pmatools 0.5.0 aborts on anything outside this set, so nothing may leak
  # another string through.
  origins <- vapply(specs, function(s) s$origin, character(1))
  expect_true(all(origins %in% c("null", "column", "scalar", "vector")))
})

test_that("pma_arg_spec() treats NA as 'not supplied', never as the string NA", {
  # shQuote(NA) would put 'NA' into the exported script and change the call.
  expect_identical(pma_arg_spec(NA), list(value = NULL, origin = "null"))
  expect_identical(pma_arg_spec(NA_character_), list(value = NULL, origin = "null"))
  expect_identical(pma_arg_spec(NULL), list(value = NULL, origin = "null"))
  expect_identical(pma_arg_spec(character(0)), list(value = NULL, origin = "null"))

  expect_identical(pma_arg_spec(0.1), list(value = 0.1, origin = "scalar"))
  expect_identical(pma_arg_spec(c(1, 2)), list(value = c(1, 2), origin = "vector"))
})

test_that("unknown arguments are dropped rather than smuggled in", {
  specs <- pma_grade_arg_specs(list(not_a_grade_meta_arg = 1,
                                    small_values         = "desirable"))
  expect_false("not_a_grade_meta_arg" %in% names(specs))
  expect_identical(names(specs), "small_values")
})
