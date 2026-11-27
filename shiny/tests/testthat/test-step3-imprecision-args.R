# test-step3-imprecision-args.R - the Imprecision tab's pmatools 0.11.0 inputs
#
# Four widgets reach grade_meta() here: large_effect and its rationale,
# ois_rrr = "threshold", and the CI-ratio rule's ci_ratio_shortcut and
# ci_ratio_scale. What is pinned: the pure mapping in R/step3_imprecision.R,
# that a judgment with no reason never reaches grade_meta() and the tab says
# so, that an untouched CI-ratio pair is OMITTED so the 0.5.x rule set keeps
# its own defaults, and that every new input is registered for clearing,
# restoring and export. The server-side tests boot step3_server() through
# shiny::testServer(), the same harness test-step3-question-panel.R uses.

library(testthat)
suppressWarnings(library(shiny))

.html <- function(x) paste(as.character(x), collapse = "")

# ---------------------------------------------------------------------------
# The pure mapping
# ---------------------------------------------------------------------------

test_that("ois_rrr passes 'threshold' through and falls back to 0.20", {
  expect_identical(step3_ois_rrr_value("threshold"), "threshold")
  expect_identical(step3_ois_rrr_value("0.25"), 0.25)
  expect_identical(step3_ois_rrr_value(NULL), 0.20)
  expect_identical(step3_ois_rrr_value("junk"), 0.20)
  expect_identical(step3_ois_rrr_for_grade("threshold", has_threshold = TRUE),
                   "threshold")
  # No threshold to power against: grade_meta() would abort.
  expect_identical(step3_ois_rrr_for_grade("threshold", has_threshold = FALSE),
                   0.20)
  expect_identical(step3_ois_rrr_for_grade(0.25, has_threshold = FALSE), 0.25)
})

test_that("a large-effect judgment is sent only with its reason", {
  expect_identical(step3_large_effect_args("auto", "ignored"),
                   list(args = list(), blocked = FALSE))
  expect_identical(step3_large_effect_args(NULL, NULL),
                   list(args = list(), blocked = FALSE))
  blank <- step3_large_effect_args("implausible", "   ")
  expect_true(blank$blocked)
  expect_length(blank$args, 0L)
  sent <- step3_large_effect_args("plausible", "  Mechanism is credible. ")
  expect_false(sent$blocked)
  expect_identical(sent$args, list(large_effect = "plausible",
                                   large_effect_rationale = "Mechanism is credible."))
})

test_that("only explicit CI-ratio settings on a binary fit are forwarded", {
  expect_identical(step3_ci_ratio_args(STEP3_RULE_SET_DEFAULT_CHOICE,
                                       STEP3_RULE_SET_DEFAULT_CHOICE,
                                       is_binary = TRUE), list())
  expect_identical(step3_ci_ratio_args(NULL, NULL, is_binary = TRUE), list())
  expect_identical(step3_ci_ratio_args("always", "fitted", is_binary = TRUE),
                   list(ci_ratio_shortcut = "always", ci_ratio_scale = "fitted"))
  expect_identical(step3_ci_ratio_args("always", "fitted", is_binary = FALSE),
                   list())
  # The widgets offer exactly the package's values plus the sentinel.
  expect_setequal(setdiff(STEP3_CI_RATIO_SHORTCUT_CHOICES,
                          STEP3_RULE_SET_DEFAULT_CHOICE),
                  PMA_CI_RATIO_SHORTCUT_CHOICES)
  expect_setequal(setdiff(STEP3_CI_RATIO_SCALE_CHOICES,
                          STEP3_RULE_SET_DEFAULT_CHOICE),
                  PMA_CI_RATIO_SCALE_CHOICES)
  expect_setequal(unname(STEP3_LARGE_EFFECT_CHOICES), PMA_LARGE_EFFECT_CHOICES)
})

test_that("a pinned fact moves to the front and nothing is lost", {
  facts <- data.frame(key = c("a", "b", "large_effect", "c"),
                      value = c("1", "2", "3", "4"), stringsAsFactors = FALSE)
  pinned <- step3_facts_pinned_first(facts, "large_effect")
  expect_identical(pinned$key, c("large_effect", "a", "b", "c"))
  expect_identical(step3_large_effect_fact(facts), "3")
  expect_null(step3_large_effect_fact(facts[-3, ]))
})

test_that("the tab renders all four inputs", {
  html <- .html(step3_ui())
  for (id in c("large_effect", "large_effect_rationale", "ois_rrr",
               "ci_ratio_shortcut", "ci_ratio_scale")) {
    expect_match(html, sprintf("id=\"%s\"", id), fixed = TRUE, info = id)
  }
  expect_match(html, "Match the threshold", fixed = TRUE)
  expect_match(html, "GRADE guidance 34 (section 4.1)", fixed = TRUE)
  expect_match(html, "CI-ratio rule (advanced)", fixed = TRUE)
})

test_that("every new input is cleared, restored and exported", {
  ids <- PMA_OUTCOME_INPUT_IDS$imprecision
  new_ids <- c("large_effect", "large_effect_rationale", "ci_ratio_shortcut",
               "ci_ratio_scale", "ois_rrr")
  expect_true(all(new_ids %in% ids))
  expect_true(all(new_ids %in% pma_restorable_input_ids()))
  expect_true(all(c("large_effect", "large_effect_rationale",
                    "ci_ratio_shortcut", "ci_ratio_scale") %in%
                    PMA_GRADE_ARGS_EXPORTED))

  specs <- pma_grade_arg_specs(list(
    large_effect = "implausible", large_effect_rationale = "Few events.",
    ci_ratio_shortcut = "always", ci_ratio_scale = "rr", ois_rrr = "threshold"))
  expect_identical(specs[["large_effect"]]$value, "implausible")
  expect_identical(specs[["large_effect_rationale"]]$value, "Few events.")
  expect_identical(specs[["ci_ratio_shortcut"]]$value, "always")
  expect_identical(specs[["ci_ratio_scale"]]$value, "rr")
  expect_identical(specs[["ois_rrr"]]$value, "threshold")
  # The prefix name is exact: a rationale alone does not invent a judgment.
  expect_false("large_effect" %in%
                 names(pma_grade_arg_specs(list(large_effect_rationale = "x"))))
  # An answer given for another outcome is never put back.
  expect_false(pma_restorable_value("plausible", stamp = 1L, gen = 2L))
  expect_true(pma_restorable_value("plausible", stamp = 2L, gen = 2L))
})

# ---------------------------------------------------------------------------
# Through the server: what grade_meta() was actually called with
# ---------------------------------------------------------------------------

# An OR fit with arm-level counts, so the RR refit is possible, and a large
# effect, so Fig 4 reaches the large-effect node.
PMA_IMPRE_FIT <- function() {
  meta::metabin(
    event.e = c(2, 3, 1), n.e = c(100, 110, 90),
    event.c = c(12, 14, 11), n.c = c(100, 110, 90),
    sm = "OR", random = TRUE, common = FALSE)
}

# step3_server() calls grade_meta(), which the helper sources only into an
# environment of its own (pma_vendored_pkg()). A copy of the server is
# enclosed by that environment, whose parent is the global one the app's
# helpers live in: the lookup order app.R gets, minus the app-wins rule that
# test-vendor-collisions.R makes moot.
pma_impre_server <- function() {
  pkg <- pma_vendored_pkg()
  if (is.null(pkg)) skip("no staged bundle; run Rscript shiny/stage_bundle.R")
  server <- step3_server
  environment(server) <- pkg
  server
}

pma_impre_app <- function(fit, rule_set = "core_grade_guide",
                          handle = new.env(parent = emptyenv())) {
  step3_server <- pma_impre_server()
  server <- function(input, output, session) {
    handle$state <- shiny::reactiveValues(
      ma = fit, small_values = "undesirable", outcome_gen = 1L,
      step = 3L, rare = NULL, rare_mode_active = FALSE,
      rare_primary_method = NULL, rare_diagnostics = NULL,
      rule_set = rule_set, outcome_type = "binary")
    step3_server(input, output, session, handle$state)
  }
  list(app = shiny::shinyApp(ui = shiny::fluidPage(), server = server),
       handle = handle)
}

pma_impre_open <- function(session, ...) {
  session$setInputs(outcome_type = "binary", per = "1000",
                    threshold_mode = "absolute",
                    clinical_question = "important_superiority", ...)
  invisible(session$getOutput("threshold_panel"))
}

# The specs grade_obj() stamped on the rating: the arguments it passed.
.rated_specs <- function(g) attr(g, PMA_GRADE_ARGS_ATTR, exact = TRUE)

test_that("untouched inputs send nothing new and rate under the 0.11.0 defaults", {
  skip_if_not_installed("meta")
  built <- pma_impre_app(PMA_IMPRE_FIT())
  shiny::testServer(built$app, {
    pma_impre_open(session)
    g <- built$handle$state$grade
    expect_false(is.null(g))
    specs <- .rated_specs(g)
    for (nm in c("large_effect", "large_effect_rationale",
                 "ci_ratio_shortcut", "ci_ratio_scale")) {
      expect_false(nm %in% names(specs), info = nm)
    }
    expect_identical(g$imprecision_settings$large_effect, "auto")
    expect_identical(g$imprecision_settings$ci_ratio_shortcut,
                     "when_ois_unavailable")
    expect_identical(g$imprecision_settings$ci_ratio_scale, "rr")
    expect_match(.html(session$getOutput("ci_ratio_settings_line")$html),
                 "only when the OIS cannot be calculated", fixed = TRUE)
  })
})

test_that("the 0.5.x rule set keeps its CI-ratio defaults unless the reviewer sets them", {
  skip_if_not_installed("meta")
  built <- pma_impre_app(PMA_IMPRE_FIT(), rule_set = "core_grade_bmj_2025")
  shiny::testServer(built$app, {
    pma_impre_open(session, rule_set = "core_grade_bmj_2025",
                   ci_ratio_shortcut = STEP3_RULE_SET_DEFAULT_CHOICE,
                   ci_ratio_scale = STEP3_RULE_SET_DEFAULT_CHOICE)
    g <- built$handle$state$grade
    expect_identical(g$rule_set, "core_grade_bmj_2025")
    expect_false("ci_ratio_shortcut" %in% names(.rated_specs(g)))
    expect_identical(g$imprecision_settings$ci_ratio_shortcut, "always")
    expect_identical(g$imprecision_settings$ci_ratio_scale, "fitted")

    # An explicit answer wins over the rule set, and reaches the export.
    session$setInputs(ci_ratio_shortcut = "when_ois_unavailable",
                      ci_ratio_scale = "rr")
    g <- built$handle$state$grade
    expect_identical(g$imprecision_settings$ci_ratio_shortcut,
                     "when_ois_unavailable")
    expect_identical(g$imprecision_settings$ci_ratio_scale, "rr")
    expect_identical(.rated_specs(g)[["ci_ratio_shortcut"]]$value,
                     "when_ois_unavailable")
    expect_identical(.rated_specs(g)[["ci_ratio_scale"]]$value, "rr")
  })
})

test_that("a large-effect judgment with its reason reaches grade_meta() and the tab", {
  skip_if_not_installed("meta")
  built <- pma_impre_app(PMA_IMPRE_FIT())
  shiny::testServer(built$app, {
    pma_impre_open(session, large_effect = "plausible",
                   large_effect_rationale = "A dose-response mechanism.")
    g <- built$handle$state$grade
    expect_identical(g$imprecision_settings$large_effect, "plausible")
    expect_identical(g$imprecision_settings$large_effect_rationale,
                     "A dose-response mechanism.")
    expect_identical(.rated_specs(g)[["large_effect"]]$value, "plausible")
    expect_identical(.rated_specs(g)[["large_effect_rationale"]]$value,
                     "A dose-response mechanism.")
    # On screen beside the verdict, not cut off by the six-row fact list.
    evaluation <- .html(session$getOutput("impre_evaluation")$html)
    expect_match(evaluation, "Implausibly large effect", fixed = TRUE)
    expect_match(evaluation, "A dose-response mechanism.", fixed = TRUE)
    expect_null(session$getOutput("large_effect_rationale_msg")$html)
  })
})

test_that("a large-effect judgment without a reason is held back, with a message", {
  skip_if_not_installed("meta")
  built <- pma_impre_app(PMA_IMPRE_FIT())
  shiny::testServer(built$app, {
    pma_impre_open(session, large_effect = "implausible",
                   large_effect_rationale = "  ")
    g <- built$handle$state$grade
    # Still rated - under the magnitude rule - rather than aborted.
    expect_false(is.null(g))
    expect_identical(g$imprecision_settings$large_effect, "auto")
    expect_false("large_effect" %in% names(.rated_specs(g)))
    msg <- .html(session$getOutput("large_effect_rationale_msg")$html)
    expect_match(msg, "A written reason is required for 'implausible'",
                 fixed = TRUE)
  })
})

test_that("ois_rrr = 'threshold' reaches grade_meta() and its echo renders", {
  skip_if_not_installed("meta")
  built <- pma_impre_app(PMA_IMPRE_FIT())
  shiny::testServer(built$app, {
    pma_impre_open(session, ois_rrr = "threshold")
    g <- built$handle$state$grade
    expect_identical(.rated_specs(g)[["ois_rrr"]]$value, "threshold")
    facts <- g$domain_facts[["Imprecision"]]
    expect_match(facts$value[facts$key == "ois_target_rate"],
                 "derived from the imprecision threshold", fixed = TRUE)
    echo <- .html(session$getOutput("ois_rrr_equiv")$html)
    expect_match(echo, "Powered against the Threshold", fixed = TRUE)
    expect_match(echo, "alternative rate", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# "Match the threshold" whose OIS rate leaves (0, 1): re-run at 0.20
# ---------------------------------------------------------------------------

# The classed condition grade_meta() raises (pmatools 0.11.1), built by hand
# so the pure helpers can be pinned without a fit.
.out_of_range_cnd <- function(p0 = 0.156, p1 = 1.056,
                              how = "p0 + the ARD threshold 0.9000") {
  rlang::catch_cnd(rlang::abort(
    "not a probability", class = PMA_OIS_THRESHOLD_OUT_OF_RANGE_CLASS,
    p0 = p0, p1 = p1, how = how))
}

test_that("only the classed abort under 'threshold' is retried, at 0.20", {
  cnd  <- .out_of_range_cnd()
  args <- list(meta_obj = "fit", ois_rrr = "threshold", ois_p0 = 0.156)
  retry <- step3_ois_threshold_retry_args(args, cnd)
  expect_identical(retry$ois_rrr, STEP3_OIS_RRR_DEFAULT)
  expect_identical(retry$ois_p0, 0.156)
  # A number the reviewer chose is never replaced.
  expect_null(step3_ois_threshold_retry_args(
    modifyList(args, list(ois_rrr = 0.25)), cnd))
  # Any other abort stays an error.
  expect_null(step3_ois_threshold_retry_args(
    args, simpleError("grade_meta() needs a threshold")))
  expect_null(step3_ois_threshold_retry_args(args, NULL))
})

test_that("the fallback message names both rates in the display unit", {
  info <- step3_ois_threshold_fallback_info(.out_of_range_cnd())
  expect_identical(info, list(p0 = 0.156, p1 = 1.056,
                              how = "p0 + the ARD threshold 0.9000"))
  msg <- step3_ois_threshold_fallback_message(info, per = 1000)
  expect_match(msg, "alternative rate of 1,056 per 1,000", fixed = TRUE)
  expect_match(msg, "control-group risk of 156 per 1,000", fixed = TRUE)
  expect_match(msg, "20 percent modest relative risk reduction instead",
               fixed = TRUE)
  expect_match(msg, "Choose 20 or 25 percent, or change the Threshold.",
               fixed = TRUE)
  expect_match(step3_ois_threshold_fallback_message(info, per = 100),
               "106 per 100", fixed = TRUE)
  note <- step3_ois_threshold_fallback_note(info)
  expect_match(note, "ois_rrr = 0.20 instead", fixed = TRUE)
  expect_match(note, "1.0560", fixed = TRUE)
  # A condition missing its fields still words a message, not an error.
  bare <- step3_ois_threshold_fallback_info(simpleError("x"))
  expect_true(is.na(bare$p0) && is.na(bare$p1) && is.na(bare$how))
  expect_match(step3_ois_threshold_fallback_message(bare), "not set",
               fixed = TRUE)
})

test_that("a threshold that pushes the OIS rate past 1 still rates the outcome", {
  skip_if_not_installed("meta")
  # A risk ratio fit whose desirable events are more frequent on treatment,
  # so the benefit direction is an increase. An odds ratio fit could not get
  # here: a ratio applied to the odds never leaves (0, 1).
  fit <- meta::metabin(
    event.e = c(20, 25, 18), n.e = c(100, 110, 90),
    event.c = c(10, 12, 9), n.c = c(100, 110, 90),
    sm = "RR", random = TRUE, common = FALSE)
  built <- pma_impre_app(fit)
  # Every grade_meta() call's ois_rrr, and every notification, recorded from
  # inside the server's own lookup path. The vendored environment is
  # memoised, so the real grade_meta() is put back on exit.
  pkg <- pma_vendored_pkg()
  real_grade_meta <- get("grade_meta", envir = pkg)
  sent <- list()
  notified <- character()
  assign("grade_meta", function(...) {
    sent[[length(sent) + 1L]] <<- list(...)$ois_rrr
    real_grade_meta(...)
  }, envir = pkg)
  on.exit(assign("grade_meta", real_grade_meta, envir = pkg), add = TRUE)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      notified <<- c(notified, paste(as.character(ui), collapse = ""))
      invisible(NULL)
    },
    .package = "shiny")

  shiny::testServer(built$app, {
    # Control-group risk about 100 per 1,000; a relative threshold of RR 20
    # asks for an alternative rate of about 2,000 per 1,000.
    pma_impre_open(session, ois_rrr = "threshold")
    session$setInputs(threshold_mode = "relative", threshold_ratio = 20)
    g <- built$handle$state$grade

    expect_false(is.null(g))
    expect_identical(nrow(g$domain_assessments), 5L)
    expect_true(any(vapply(sent, identical, logical(1), "threshold")))
    expect_identical(sent[[length(sent)]], STEP3_OIS_RRR_DEFAULT)
    expect_false(any(grepl("grade_meta error", notified, fixed = TRUE)))

    fallback <- attr(g, STEP3_OIS_FALLBACK_ATTR, exact = TRUE)
    expect_false(is.null(fallback))
    expect_gte(fallback$p1, 1)
    # The export replays the call that was rated, not the one that aborted.
    expect_identical(.rated_specs(g)[["ois_rrr"]]$value, STEP3_OIS_RRR_DEFAULT)
    impre_note <- g$domain_assessments$notes[
      g$domain_assessments$domain == "Imprecision"]
    expect_match(impre_note, "ois_rrr = 'threshold' was not used",
                 fixed = TRUE)

    echo <- .html(session$getOutput("ois_rrr_equiv")$html)
    expect_match(echo, "pma-ois-threshold-fallback", fixed = TRUE)
    expect_match(echo, PMA_ALERT_BG, fixed = TRUE)
    expect_match(echo, "which is not a probability", fixed = TRUE)
    expect_false(grepl("Powered against the Threshold", echo, fixed = TRUE))

    # Back to a threshold that gives a probability: no fallback, no warning.
    session$setInputs(threshold_ratio = 1.5)
    g <- built$handle$state$grade
    expect_null(attr(g, STEP3_OIS_FALLBACK_ATTR, exact = TRUE))
    expect_identical(.rated_specs(g)[["ois_rrr"]]$value, "threshold")
    expect_false(grepl("pma-ois-threshold-fallback",
                       .html(session$getOutput("ois_rrr_equiv")$html),
                       fixed = TRUE))
  })
})
