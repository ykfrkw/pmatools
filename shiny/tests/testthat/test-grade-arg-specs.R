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

test_that("the five clinical-question arguments are all exported", {
  # An argument the app supplies but does not declare here disappears from the
  # bundled analysis.R, and the script then replays a DIFFERENT QUESTION from
  # the one the bundle documents - while still printing a rating. Between them
  # these five are the question, so all five have to be on the registry.
  expect_true(all(c("threshold_type", "rating_target",
                    "rating_target_rationale", "threshold_sides",
                    "plain_language_frame") %in% PMA_GRADE_ARGS_EXPORTED))

  # Every one of them is also a name grade_meta() actually has. export_bundle()
  # matches these exactly and aborts on a name it does not know, so a typo here
  # would take the download with it.
  for (q in PMA_CLINICAL_QUESTIONS) {
    args <- pma_question_grade_args(q)
    expect_true(all(names(args) %in% PMA_GRADE_ARGS_EXPORTED), info = q)
  }
})

test_that("each question's arguments survive pma_grade_arg_specs()", {
  for (q in PMA_CLINICAL_QUESTIONS) {
    args  <- pma_question_grade_args(q)
    specs <- pma_grade_arg_specs(args)

    # threshold_type and threshold_sides are set on every question, so they
    # must always arrive with a value rather than a null spec.
    expect_identical(specs[["threshold_type"]]$value, args$threshold_type,
                     info = q)
    expect_identical(specs[["threshold_sides"]]$value, args$threshold_sides,
                     info = q)

    # rating_target and its rationale travel together or neither: a pinned
    # target with nothing written down aborts in .check_override_rationale(),
    # so a script that carried one without the other would not run at all.
    has_target <- !is.null(specs[["rating_target"]]$value)
    has_reason <- !is.null(specs[["rating_target_rationale"]]$value)
    expect_identical(has_target, has_reason, info = q)
    expect_identical(has_target,
                     q %in% c("equivalence", "non_inferiority"), info = q)
    if (has_target) {
      expect_identical(specs[["rating_target"]]$value,
                       "little_to_no_difference", info = q)
      expect_identical(specs[["rating_target_rationale"]]$value,
                       unname(PMA_QUESTION_RATIONALE[[q]]), info = q)
    }
  }
})

test_that("threshold_sides is not answered by threshold_scale", {
  # The regression the comment at pma_outcome_grade_args() documents:
  # `threshold_sides` is a partial-match neighbour of `threshold_scale`, so an
  # inexact lookup on an object carrying only the latter answers "ratio" - a
  # value grade_meta() then rejects as a threshold_sides, in a script the
  # reviewer has already downloaded.
  specs <- pma_grade_arg_specs(list(threshold_scale = "ratio"))
  expect_false("threshold_sides" %in% names(specs))
  expect_null(specs[["threshold_sides"]])

  # ... and the same in the other direction, since either can be supplied
  # alone.
  specs <- pma_grade_arg_specs(list(threshold_sides = "worse_only"))
  expect_false("threshold_scale" %in% names(specs))
  expect_identical(specs[["threshold_sides"]]$value, "worse_only")

  # rating_target is a strict prefix of rating_target_rationale, which is the
  # same hazard one argument over.
  specs <- pma_grade_arg_specs(list(rating_target_rationale = "because"))
  expect_false("rating_target" %in% names(specs))
  expect_null(specs[["rating_target"]])
})

test_that("unknown arguments are dropped rather than smuggled in", {
  specs <- pma_grade_arg_specs(list(not_a_grade_meta_arg = 1,
                                    small_values         = "desirable"))
  expect_false("not_a_grade_meta_arg" %in% names(specs))
  expect_identical(names(specs), "small_values")
})
