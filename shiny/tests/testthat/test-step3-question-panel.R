# test-step3-question-panel.R - what output$threshold_panel RENDERS when the
# clinical question changes (R/step3_grade.R, shiny/SPEC.md 3.4.10b).
#
# This is the only file in the suite that runs step3_server(). Everything else
# about the four clinical questions is covered by test-step3-threshold.R, which
# exercises the pure helpers, and those helpers were all correct when the bug
# below shipped to production. What was wrong was the HTML.
#
# The bug: reaching Step 3 on the default question with the threshold
# prefilled, then clicking Non-inferiority, left the numeric input still
# DISPLAYING the suggestion. Every piece of server state was right -
# threshold_state() / threshold_abs_state() really were NA, threshold_confirm
# really was unticked, config_status really did say "No non-inferiority
# threshold is set" and Next really was disabled - so the reviewer saw a filled
# box above a message saying no threshold was set, beside a locked Next, with
# nothing on screen to say what the app wanted. output$threshold_panel and the
# clear/reseed observer both depend on question_state(), Shiny does not order
# them by creation order, and when the render won it seeded the box from the
# PREVIOUS question's reactiveVal under isolate().
#
# So every assertion here is made against what the renderUI PRODUCES, and the
# central one (`the panel that renders before the seeder`) drives the render
# function itself in the losing order. What it reads: nothing but the app's own
# R/ files, through helper-app.R, plus `meta` for a real fit. What it produces:
# no files, no fixtures - the harness below builds one session per test.
#
# THE ADMISSION RULE: a test belongs here when it needs step3_server() running
# and asserts on rendered HTML or on an output's value. A test of a pure helper
# belongs in test-step3-threshold.R even when it is about the same question -
# the split is by what the test has to boot, not by subject.

library(testthat)
# Attached here, and the warning swallowed, because shiny::testServer() calls
# require(shiny) itself: attaching it first turns that into a no-op, and an
# R-version note raised from inside a test would break the suite's WARN 0.
suppressWarnings(library(shiny))

# --------------------------------------------------------------------------
# The harness
# --------------------------------------------------------------------------
# step3_server() is not a Shiny module, so shiny::testServer() is handed a
# one-server app that calls it. `state` is stashed in an environment the test
# can reach, because three of the tests below have to move state$ma or
# state$outcome_gen - the two things app.R moves when the analysis or the
# outcome changes - and neither is an input.
#
# MockShinySession has no sendInputMessage(), so state$step must never change
# during a test: the answer-restore observer in step3_server() calls it. Every
# session below therefore starts and stays on step 3.

PMA_Q_FIT_BINARY <- function(sm = "OR") {
  meta::metabin(
    event.e = c(10, 12, 9), n.e = c(100, 110, 90),
    event.c = c(20, 22, 19), n.c = c(100, 110, 90),
    sm = sm, random = TRUE, common = FALSE)
}

PMA_Q_FIT_CONTINUOUS <- function(sm = "SMD") {
  meta::metacont(
    n.e = c(40, 50, 45), mean.e = c(9, 8.5, 9.2), sd.e = c(4, 4.2, 3.9),
    n.c = c(41, 49, 44), mean.c = c(11, 10.5, 11.2), sd.c = c(4.1, 4.3, 4),
    sm = sm, random = TRUE, common = FALSE)
}

# The app, plus the environment its `state` lands in. `small_values` is
# "undesirable" throughout, matching the sample CBT-I dataset the bug was found
# on: it is what makes the non-inferiority help name the LOWER side as the
# worse one, which one of the tests reads back.
pma_q_app <- function(fit, handle = new.env(parent = emptyenv())) {
  server <- function(input, output, session) {
    handle$state <- shiny::reactiveValues(
      ma = fit, small_values = "undesirable", outcome_gen = 1L,
      step = 3L, rare = NULL, rare_mode_active = FALSE,
      rare_primary_method = NULL, rare_diagnostics = NULL)
    step3_server(input, output, session, handle$state)
  }
  list(app = shiny::shinyApp(ui = shiny::fluidPage(), server = server),
       handle = handle)
}

# The rendered `value` attribute of one numeric input, or NA_character_ when
# the box rendered empty. NA is the assertion the bug turns on: an empty box is
# what "no threshold is set" looks like, and the box the reviewer was shown
# carried value="50".
pma_q_box_value <- function(ui, id) {
  html <- paste(as.character(ui), collapse = "")
  tag <- regmatches(html, regexpr(paste0('id="', id, '"[^>]*'), html))
  if (!length(tag)) return("<absent>")
  value <- regmatches(tag, regexpr('value="[^"]*"', tag))
  if (!length(value)) return(NA_character_)
  sub('^value="', "", sub('"$', "", value))
}

pma_q_html <- function(ui) paste(as.character(ui), collapse = "")

# Open the Configuration tab of a binary outcome on the default question, with
# the threshold prefilled. The `per` unit and the threshold scale are set
# explicitly because the radios that carry them live inside the panel and a
# MockShinySession does not report a rendered widget's own value back.
pma_q_open_binary <- function(session) {
  session$setInputs(outcome_type = "binary", per = "1000",
                    threshold_mode = "absolute",
                    clinical_question = "important_superiority")
  invisible(session$getOutput("threshold_panel"))
}

# --------------------------------------------------------------------------
# Switching into a margin question empties the boxes
# --------------------------------------------------------------------------

test_that("switching into a margin question renders both binary boxes empty", {
  skip_if_not_installed("meta")

  for (question in c("equivalence", "non_inferiority")) {
    built <- pma_q_app(PMA_Q_FIT_BINARY())
    shiny::testServer(built$app, {
      pma_q_open_binary(session)

      # The default question really is prefilled: without this the assertions
      # below would pass against an app that offers nothing to anybody.
      panel <- session$getOutput("threshold_panel")
      expect_equal(pma_q_box_value(panel$html, "threshold_abs"), "50")
      expect_equal(pma_q_box_value(panel$html, "threshold_ratio"), "1.25")

      session$setInputs(clinical_question = question)
      panel <- session$getOutput("threshold_panel")

      # Both boxes, because the reviewer can be standing on either scale when
      # they click: the relative one is behind a conditionalPanel, so it is in
      # the DOM whichever radio is selected.
      expect_identical(pma_q_box_value(panel$html, "threshold_abs"),
                       NA_character_, info = question)
      expect_identical(pma_q_box_value(panel$html, "threshold_ratio"),
                       NA_character_, info = question)

      # And the panel really did re-render for the new question, rather than
      # the boxes having emptied for some unrelated reason.
      html <- pma_q_html(panel$html)
      expect_true(grepl(
        unname(EDU_COPY$config_tab$question_headings[[question]]),
        html, fixed = TRUE), info = question)
      expect_true(grepl(step3_threshold_copy(question, "OR",
                                             small_values = "undesirable")$help,
                        html, fixed = TRUE), info = question)
      # The source badge goes with the prefill: no value, so no source.
      expect_false(grepl("source:", html, fixed = TRUE), info = question)
    })
  }
})

test_that("switching into a margin question renders the continuous box empty", {
  skip_if_not_installed("meta")

  for (question in c("equivalence", "non_inferiority")) {
    built <- pma_q_app(PMA_Q_FIT_CONTINUOUS())
    shiny::testServer(built$app, {
      session$setInputs(outcome_type = "continuous", per = "1000",
                        clinical_question = "important_superiority")
      panel <- session$getOutput("threshold_panel")
      # Core GRADE 6's 0.2 for an SMD, so this branch is prefilled too.
      expect_equal(pma_q_box_value(panel$html, "threshold_cont"), "0.2")

      session$setInputs(clinical_question = question)
      panel <- session$getOutput("threshold_panel")
      expect_identical(pma_q_box_value(panel$html, "threshold_cont"),
                       NA_character_, info = question)
      expect_true(grepl(
        unname(EDU_COPY$config_tab$question_headings[[question]]),
        pma_q_html(panel$html), fixed = TRUE), info = question)
    })
  }
})

test_that("the non-inferiority panel names the worse side of the threshold", {
  skip_if_not_installed("meta")

  built <- pma_q_app(PMA_Q_FIT_BINARY())
  shiny::testServer(built$app, {
    pma_q_open_binary(session)
    session$setInputs(clinical_question = "non_inferiority")
    html <- pma_q_html(session$getOutput("threshold_panel")$html)
    # A one-sided test whose side the reviewer cannot see on screen is a silent
    # exit (shiny/SPEC.md 2.3). state$small_values is "undesirable" here, so
    # the lower values are the worse ones.
    expect_true(grepl(step3_worse_side_sentence("undesirable"), html,
                      fixed = TRUE))
  })
})

# --------------------------------------------------------------------------
# The server state the render has to agree with
# --------------------------------------------------------------------------
# These four were already correct when the bug shipped. They are here because
# the defect was the DISAGREEMENT between them and the box: a fix that emptied
# the box by leaving the threshold set would satisfy the tests above and
# reintroduce the same contradiction from the other side.

test_that("switching into a margin question locks Next and voids the rating", {
  skip_if_not_installed("meta")

  built <- pma_q_app(PMA_Q_FIT_BINARY())
  shiny::testServer(built$app, {
    pma_q_open_binary(session)
    # A complete configuration on the default question: prefilled threshold,
    # confirmation given, Next open.
    session$setInputs(threshold_confirm = TRUE)
    expect_false(grepl("disabled",
                       pma_q_html(session$getOutput("grade_nav_config")$html),
                       fixed = TRUE))

    session$setInputs(clinical_question = "non_inferiority")

    status <- pma_q_html(session$getOutput("config_status")$html)
    gate <- pma_question_gate_copy("non_inferiority")
    expect_true(grepl(gate$status, status, fixed = TRUE))
    expect_true(grepl(gate$blocker, status, fixed = TRUE))
    expect_true(grepl("Still to do:", status, fixed = TRUE))
    # Next is locked again, on the threshold alone.
    expect_true(grepl("disabled",
                      pma_q_html(session$getOutput("grade_nav_config")$html),
                      fixed = TRUE))
  })

  # The untick itself is NOT asserted above, and it is not an omission:
  # step3_server() performs it with updateCheckboxInput(), MockShinySession has
  # no sendInputMessage() to carry that back, so input$threshold_confirm keeps
  # whatever the test last set. What CAN be asserted is that the sentence the
  # reviewer reads while stuck names the box - the other half of "Still to do"
  # in the bug report - so the blocker is checked here with the confirmation
  # left as a fresh panel leaves it.
  built <- pma_q_app(PMA_Q_FIT_BINARY())
  shiny::testServer(built$app, {
    pma_q_open_binary(session)
    session$setInputs(clinical_question = "non_inferiority")
    status <- pma_q_html(session$getOutput("config_status")$html)
    expect_true(grepl("tick the confirmation box below", status, fixed = TRUE))
  })
})

# --------------------------------------------------------------------------
# A margin the reviewer typed for THIS question survives a re-render
# --------------------------------------------------------------------------
# This is the constraint that makes the fix subtle, and the reason "a margin
# question always renders an empty box" is the wrong rule. The panel rebuilds
# on a change of the per-N unit and on an outcome-generation bump, and neither
# of those is a change of question.

test_that("a typed margin survives a per-N switch and an outcome_gen bump", {
  skip_if_not_installed("meta")

  built <- pma_q_app(PMA_Q_FIT_BINARY())
  shiny::testServer(built$app, {
    pma_q_open_binary(session)
    session$setInputs(clinical_question = "non_inferiority")
    session$setInputs(threshold_abs = 300, threshold_ratio = 1.4)

    # The unit switch relabels and RESCALES the absolute box: 300 events per
    # 1,000 is 30 per 100. The margin is the same margin.
    session$setInputs(per = "100")
    panel <- session$getOutput("threshold_panel")
    expect_equal(pma_q_box_value(panel$html, "threshold_abs"), "30")
    expect_equal(pma_q_box_value(panel$html, "threshold_ratio"), "1.4")

    # An outcome-generation bump with no reset behind it (app.R pairs the two,
    # and the reset is what is entitled to discard a margin - not the bump).
    built$handle$state$outcome_gen <- 2L
    session$flushReact()
    panel <- session$getOutput("threshold_panel")
    expect_equal(pma_q_box_value(panel$html, "threshold_abs"), "30")
    expect_equal(pma_q_box_value(panel$html, "threshold_ratio"), "1.4")
  })
})

test_that("switching out of a margin question re-offers the suggestion", {
  skip_if_not_installed("meta")

  built <- pma_q_app(PMA_Q_FIT_BINARY())
  shiny::testServer(built$app, {
    pma_q_open_binary(session)
    session$setInputs(clinical_question = "non_inferiority")
    session$setInputs(threshold_abs = 300, threshold_ratio = 1.4)

    # "Only ever prefill a reactiveVal that is still NA" would decline to
    # re-offer the suggestion over the margin, so the margin has to be cleared
    # first - the box must carry the suggestion again, not the typed margin.
    session$setInputs(clinical_question = "important_superiority")
    panel <- session$getOutput("threshold_panel")
    expect_equal(pma_q_box_value(panel$html, "threshold_abs"), "50")
    expect_equal(pma_q_box_value(panel$html, "threshold_ratio"), "1.25")
    expect_true(grepl("source:", pma_q_html(panel$html), fixed = TRUE))
  })
})

# --------------------------------------------------------------------------
# The panel that renders BEFORE the seeding observer
# --------------------------------------------------------------------------
# The regression test proper. Everything above asserts the panel AFTER the
# flush has settled, and a MockShinySession settles every observer before the
# test can read an output - so none of it can distinguish a render that is
# self-sufficient from one that merely got lucky with the flush order. In
# production it did not: the render won, and drew a box the state contradicted.
#
# So this one calls output$threshold_panel's own render function from an
# observer of higher priority than the seeder, which is what "the render won
# the flush race" is. The render is the real one - taken off the session rather
# than rebuilt here, so it cannot drift from the panel the app ships.
#
# The stale value is carried across a change of ANALYSIS rather than a change
# of question, because a priority can be put in front of the observer on
# state$ma but not in between the two observers a question change chains. It is
# the same defect and the same key: a threshold entered against an odds ratio
# is not a risk-ratio margin, exactly as a threshold of clinical importance
# suggested for the default question is not a non-inferiority margin.

# The render function output$threshold_panel was defined with. Reaching into
# the mock session's private store is the price of asserting on the app's own
# renderUI rather than on a copy of it; if a future Shiny moves it, the
# expect_false() below turns into an error naming this line.
pma_q_render_fn <- function(session, id) {
  outs <- session$.__enclos_env__$private$outs
  expect_false(is.null(outs[[id]]$func),
               info = "MockShinySession no longer stores output render functions")
  outs[[id]]$func
}

test_that("the panel refuses a threshold seeded for another analysis", {
  skip_if_not_installed("meta")

  built <- pma_q_app(PMA_Q_FIT_BINARY("OR"))
  shiny::testServer(built$app, {
    pma_q_open_binary(session)
    session$setInputs(clinical_question = "non_inferiority")
    session$setInputs(threshold_abs = 300, threshold_ratio = 1.4)

    render <- pma_q_render_fn(session, "threshold_panel")
    early <- NULL
    shiny::observe({
      built$handle$state$ma
      early <<- shiny::isolate(render(session, "threshold_panel"))
    }, priority = 100)

    # Step 2 re-run on the risk-ratio scale. The question has not moved, so
    # the panel still says Non-inferiority threshold - and the margin in the
    # reactiveVals is 300 events, entered against the odds-ratio analysis.
    built$handle$state$ma <- PMA_Q_FIT_BINARY("RR")
    session$flushReact()

    html <- pma_q_html(early$html)
    expect_true(grepl(
      unname(EDU_COPY$config_tab$question_headings[["non_inferiority"]]),
      html, fixed = TRUE))
    expect_identical(pma_q_box_value(html, "threshold_abs"), NA_character_)
    expect_identical(pma_q_box_value(html, "threshold_ratio"), NA_character_)

    # And the settled panel agrees, so this is not a render that is merely
    # emptier than the state it describes.
    panel <- session$getOutput("threshold_panel")
    expect_identical(pma_q_box_value(panel$html, "threshold_abs"),
                     NA_character_)
    expect_identical(pma_q_box_value(panel$html, "threshold_ratio"),
                     NA_character_)
  })
})

test_that("the early panel still offers a suggestion on a prefilled question", {
  skip_if_not_installed("meta")

  # The other half of the same rule: refusing the stored value must not turn
  # into refusing the SUGGESTION. On a question that may be prefilled, a
  # render that beats the seeder has to fall back to suggest_threshold() for
  # the new analysis rather than leave the reviewer an empty box.
  built <- pma_q_app(PMA_Q_FIT_BINARY("OR"))
  shiny::testServer(built$app, {
    pma_q_open_binary(session)
    session$setInputs(threshold_abs = 300, threshold_ratio = 1.4)

    render <- pma_q_render_fn(session, "threshold_panel")
    early <- NULL
    shiny::observe({
      built$handle$state$ma
      early <<- shiny::isolate(render(session, "threshold_panel"))
    }, priority = 100)

    built$handle$state$ma <- PMA_Q_FIT_CONTINUOUS("SMD")
    session$setInputs(outcome_type = "continuous")
    session$flushReact()

    # 300 was an events-per-1,000 threshold for an odds ratio; it is not an
    # SMD. Core GRADE 6's 0.2 is.
    expect_equal(pma_q_box_value(early$html, "threshold_cont"), "0.2")
  })
})
