# test-rule-set.R - the Configuration tab's Rule set (pmatools 0.8)
#
# The dropdown chooses grade_meta(rule_set =): the Core GRADE Guide (default)
# or the BMJ 2025 Core GRADE series rules of pmatools 0.5.x. What is pinned
# here is the app's side of it: the pure helpers in R/step3_threshold.R, the
# section step3_ui() renders and seeds from state, the publication-bias wizard
# not claiming a Q0 the rating never asked, and the export specs carrying the
# legacy set (and only the legacy set) into the bundled analysis.R.

.html <- function(x) paste(as.character(x), collapse = "")

test_that("the rule-set helpers fall back to the Guide for anything unknown", {
  expect_identical(step3_rule_set_value(NULL), "core_grade_guide")
  expect_identical(step3_rule_set_value(NA), "core_grade_guide")
  expect_identical(step3_rule_set_value("junk"), "core_grade_guide")
  expect_identical(step3_rule_set_value("core_grade_bmj_2025"),
                   "core_grade_bmj_2025")
  expect_true(step3_rule_set_is_legacy("core_grade_bmj_2025"))
  expect_false(step3_rule_set_is_legacy("core_grade_guide"))
  expect_identical(step3_rule_set_some_concerns("core_grade_bmj_2025"), "high")
  expect_identical(step3_rule_set_some_concerns("core_grade_guide"), "low")
})

test_that("the Configuration tab offers both rule sets, seeded from state", {
  html <- .html(step3_ui())
  expect_match(html, "id=\"rule_set\"", fixed = TRUE)
  expect_match(html, "Core GRADE Guide (current)", fixed = TRUE)
  expect_match(html, "BMJ 2025 Core GRADE series (pmatools 0.5.x)",
               fixed = TRUE)
  expect_match(html, "direction-of-bias check", fixed = TRUE)
  expect_match(html, "flowcharts stay the Core GRADE Guide", fixed = TRUE)
  expect_match(html, "<option value=\"core_grade_guide\" selected>",
               fixed = TRUE)

  legacy_state <- shiny::reactiveValues(rule_set = "core_grade_bmj_2025",
                                        rob_some_concerns = "high")
  html_legacy <- .html(step3_ui(legacy_state))
  expect_match(html_legacy, "<option value=\"core_grade_bmj_2025\" selected>",
               fixed = TRUE)
})

test_that("under the 0.5.x set the wizard does not claim Q0 was answered", {
  q0 <- step3_pubias_important_effect(NULL, rule_set = "core_grade_bmj_2025")
  expect_identical(q0$answer, "not_asked")
  expect_match(q0$line, "not asked", fixed = TRUE)
  expect_false(grepl("assumed yes", q0$line, fixed = TRUE))

  # A rating that recorded "not asked" reads back as not asked, never as the
  # "no" its leading letters would otherwise match.
  rated <- list(domain_facts = list("Publication bias" = data.frame(
    key = "important_effect",
    value = "not asked (pmatools 0.5.x rule set; Core GRADE 4 Fig 5 has no such node)",
    stringsAsFactors = FALSE)))
  expect_identical(step3_pubias_important_effect(rated)$answer, "not_asked")

  # The Guide's reading is unchanged.
  expect_identical(step3_pubias_important_effect(NULL)$answer, "yes")
})

test_that("a Q0 that was not asked lights nothing of Q0 and asks Q1 first", {
  expect_identical(step3_pubias_flow_ids(important_effect = "not_asked"),
                   character(0))
  ids <- step3_pubias_flow_ids(small_industry = "yes",
                               important_effect = "not_asked")
  expect_identical(ids, c("pma-pubias-node-q1", "pma-pubias-edge-q1-yes",
                          "pma-pubias-leaf-down1-q1"))
  expect_identical(step3_pubias_node(important_effect = "not_asked"), "q1")
  expect_identical(step3_pubias_reachable(important_effect = "not_asked"),
                   "q1")
})

test_that("only the 0.5.x rule set reaches the export specs", {
  legacy <- pma_grade_arg_specs(list(rule_set = "core_grade_bmj_2025"))
  expect_identical(legacy[["rule_set"]]$value, "core_grade_bmj_2025")
  expect_identical(legacy[["rule_set"]]$origin, "scalar")
  expect_false("rule_set" %in%
                 names(pma_grade_arg_specs(list(rule_set = "core_grade_guide"))))
  expect_true("rule_set" %in% PMA_GRADE_ARGS_EXPORTED)
})

test_that("a banked legacy outcome recovers its rule set for the bundle", {
  pkg <- pma_vendored_pkg()
  skip_if(is.null(pkg), "no staged bundle - run Rscript shiny/stage_bundle.R")
  m <- meta::metabin(c(10, 15, 20), c(50, 60, 70), c(15, 20, 25),
                     c(50, 60, 70), studlab = c("A", "B", "C"), sm = "OR",
                     method = "Inverse")
  g <- suppressWarnings(pkg$grade_meta(m, small_values = "desirable",
                                       threshold_type = "null",
                                       rule_set = "core_grade_bmj_2025"))
  expect_identical(pma_outcome_grade_args(g)[["rule_set"]],
                   "core_grade_bmj_2025")
  expect_true(all(PMA_GRADE_ARGS_EXPORTED %in% pkg$.grade_arg_names()))

  g_default <- suppressWarnings(pkg$grade_meta(m, small_values = "desirable",
                                               threshold_type = "null"))
  expect_null(pma_outcome_grade_args(g_default)[["rule_set"]])
})
