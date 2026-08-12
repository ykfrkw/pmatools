# pma_grade_arg_specs() (R/ui_helpers.R) builds the {value, origin} specs that
# export_bundle() renders into the "reproducible" analysis.R.
#
# The behaviour under test is the always-emit-every-name rule. export_bundle()
# looks specs up with `grade_args$<name>`, and `$` PARTIAL-MATCHES on lists: if
# `inconsistency` were absent, `grade_args$inconsistency` would silently return
# the `inconsistency_ci_diff` spec and write that answer into the wrong
# argument of the exported call. A complete list makes every lookup exact.
# Do not "simplify" this to only emit supplied names.

test_that("every registry name is emitted, in registry order", {
  specs <- pma_grade_arg_specs(list())
  expect_identical(names(specs), PMA_GRADE_ARGS_EXPORTED)
  expect_true(all(vapply(specs, function(s) identical(s$origin, "null"),
                         logical(1))))
})

test_that("a prefix name cannot partial-match onto a longer sibling", {
  # `inconsistency` is a strict prefix of `inconsistency_ci_diff`,
  # `inconsistency_rationale`, `inconsistency_threshold_side` and
  # `inconsistency_subgroup_explained`. Supply only the longer one.
  specs <- pma_grade_arg_specs(list(inconsistency_ci_diff = 0.5))

  expect_true("inconsistency" %in% names(specs))
  expect_identical(specs$inconsistency$origin, "null")
  expect_null(specs$inconsistency$value)
  expect_identical(specs$inconsistency_ci_diff$value, 0.5)

  # Same hazard on the other prefix families.
  expect_identical(pma_grade_arg_specs(
    list(rob_rationale = "x"))$rob$origin, "null")
  expect_identical(pma_grade_arg_specs(
    list(threshold_scale = "ratio"))$threshold$origin, "null")
  expect_identical(pma_grade_arg_specs(
    list(imprecision_rationale = "x"))$imprecision$origin, "null")
  expect_identical(pma_grade_arg_specs(
    list(indirectness_subdomains = data.frame(a = 1)))$indirectness$origin,
    "null")
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

  expect_identical(specs$threshold$value, 1.25)
  expect_identical(specs$threshold$origin, "scalar")
  expect_identical(specs$threshold_scale$origin, "scalar")
  expect_identical(specs$rob$origin, "vector")
  expect_identical(specs$rob$value, c("low", "high", "*"))
  expect_identical(specs$indirectness_subdomains$origin, "scalar")

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
  specs <- pma_grade_arg_specs(list(not_a_grade_meta_arg = 1))
  expect_false("not_a_grade_meta_arg" %in% names(specs))
  expect_identical(names(specs), PMA_GRADE_ARGS_EXPORTED)
})
