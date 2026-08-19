# The Step 2 sidebar layout, asserted on the HTML step2_ui() renders.
#
# There is no browser driver here, so the overflow itself cannot be measured.
# What CAN be pinned is the pair of CSS declarations that caused it: a sidebar
# that refuses to shrink (`flex: 0 0 <basis>`) and a right pane with a flat
# `min-width: 480px` that refuses to narrow below it, so on a 375px viewport
# the row was 492px wide and the whole document scrolled sideways. Both are
# one-token edits away from coming back, which is exactly what this catches.
#
# The sidebar's grow factor is 0 so that a wide screen spends its spare pixels
# on the forest plot rather than on the controls; the shrink factor is what the
# phone guarantee rests on, and the two are one character apart.

step2_html <- function(state = NULL) as.character(step2_ui(state))

# Each accordion item opens with data-value="<panel>" and carries its state on
# the .accordion-collapse div below it, as "collapse" or "collapse show".
step2_open_panels <- function(html) {
  items   <- strsplit(html, 'data-value="', fixed = TRUE)[[1]][-1]
  values  <- sub('".*$', "", items)
  is_open <- vapply(items, function(item) {
    at <- regexpr('accordion-collapse collapse[^"]*"', item)
    at != -1L && grepl("show", regmatches(item, at), fixed = TRUE)
  }, logical(1))
  unname(values[is_open])
}

test_that("neither Step 2 column can force the document wider than a phone", {
  html <- step2_html()
  expect_true(grepl("flex: 0 1 300px", html, fixed = TRUE))
  expect_true(grepl("min-width: min(480px, 100%)", html, fixed = TRUE))
  # The declarations that produced the 492px document. Any basis with a 0
  # shrink factor brings it back, so the whole shape is rejected, not one size.
  expect_false(grepl("flex: 0 0 ", html, fixed = TRUE))
  expect_false(grepl("flex: 1 0 ", html, fixed = TRUE))
  expect_false(grepl("min-width: 480px", html, fixed = TRUE))
})

test_that("the Step 2 sidebar does not take the width the forest plot wants", {
  # Grow factor 0: on a wide screen the spare pixels belong to the right pane,
  # which is what holds the plots. `flex: 1 1 300px` renders identically on a
  # phone and is the edit this guards against.
  html <- step2_html()
  expect_false(grepl("flex: 1 1 ", html, fixed = TRUE))
})

test_that("every Step 2 input id survives the accordion restructure", {
  # The whole app addresses these by id - Step 3 reads most of them off the
  # Step 2 inputs - so moving a control into an accordion panel must not touch
  # the id it is registered under. A rename is silent everywhere else.
  html <- step2_html()
  ids <- c("outcome_name", "small_values", "outcome_type", "outcome_follow_up",
           "outcome_unit", "col_studlab", "col_treat", "col_n", "col_event",
           "col_mean", "col_sd", "sm_bin", "model", "method", "method_tau",
           "random_ci", "incr", "subgroup_col", "auto_rerun", "run_ma",
           "arm_assignment_ui", "sm_cont_ui", "subgroup_order_ui")
  for (id in ids) {
    expect_true(grepl(sprintf('id="%s"', id), html, fixed = TRUE), info = id)
  }
})

test_that("Step 2 offers no outcome row-filter", {
  # The filter sliced a continuous data set whose studies each named their own
  # measurement scale (PHQ-9 / HAMD / BDI) down to whichever scale came first,
  # leaving one study to pool and saying nothing about the rest. `outcome` is
  # a descriptive column now, so neither the control nor its placeholder may
  # come back.
  html <- step2_html()
  expect_false(grepl("selected_outcome", html, fixed = TRUE))
  expect_false(grepl("outcome_filter_ui", html, fixed = TRUE))
})

test_that("the sidebar is four accordion panels plus a sticky action bar", {
  html <- step2_html()
  for (value in c("outcome", "mapping", "model", "subgroup")) {
    expect_true(grepl(sprintf('data-value="%s"', value), html, fixed = TRUE),
                info = value)
  }
  expect_true(grepl("pma-step2-actions", html, fixed = TRUE))
})

test_that("the sidebar opens on Outcome alone, whatever state it is built from", {
  # One panel at a time: "Outcome" is the only one holding something no default
  # can supply, so it is where the step starts, and it starts there whether or
  # not an analysis already exists. A mapping select left blank is caught from
  # the other end, by www/required-fields.js, which opens the panel holding it.
  expect_setequal(step2_open_panels(step2_html(NULL)), "outcome")

  settled <- shiny::reactiveValues(ma = list(TE = 0))
  expect_setequal(step2_open_panels(step2_html(settled)), "outcome")
})

test_that("the Step 2 accordion opens one panel at a time", {
  # `multiple = FALSE` is what puts data-bs-parent on each .accordion-collapse,
  # and that attribute is the whole mechanism: it is what lets Bootstrap close
  # the open sibling. www/required-fields.js depends on it too - it opens a
  # panel through the Collapse API precisely so the sibling closes with it.
  html <- step2_html()
  expect_true(grepl("data-bs-parent=", html, fixed = TRUE))
})

test_that("arm_assignment_ui is exempt from suspension while its panel is shut", {
  # The panel holding it is never open on build (see above), and Shiny does not
  # render a hidden output. Without the exemption the two arm selects are never
  # created, input$experimental_label stays NULL, and the analysis reactive
  # returns NULL - which is an app whose Run analysis button does nothing at
  # all, with no message anywhere to say why. The exemption is one line and its
  # absence is invisible in every other test, so it is asserted on the source.
  #
  # Only this output needs it. sm_cont falls back to run_ma()'s own default and
  # subgroup_order is guarded on NULL, so a suspended sm_cont_ui or
  # subgroup_order_ui costs a default rather than the analysis.
  src <- gsub("[[:space:]]+", " ", paste(deparse(body(step2_server)),
                                         collapse = " "))
  expect_true(grepl(
    'outputOptions(output, "arm_assignment_ui", suspendWhenHidden = FALSE)',
    src, fixed = TRUE))
})

test_that("the arm-label guard says something instead of returning in silence", {
  # This exit was the only one in the analysis reactive that produced no
  # notification, so a suspended arm_assignment_ui was indistinguishable from a
  # working app that had decided not to run. Losing the message would make the
  # next such fault invisible again.
  src <- gsub("[[:space:]]+", " ", paste(deparse(body(step2_server)),
                                         collapse = " "))
  expect_true(grepl('id = "step2_arm_assignment"', src, fixed = TRUE))
})

test_that("a Run analysis press is served once, not for the rest of the session", {
  # THE bug this file exists to keep out. input$run_ma is an actionButton
  # counter that only ever increases and that nothing resets, so the gate used
  # to read it as `(input$run_ma %||% 0L) > 0L` and latch TRUE at the first
  # press. From then on "auto-rerun off" did nothing at all: every change to
  # the debounced input bundle re-ran the analysis, and on rare-event data that
  # is run_rare_ma()'s whole multi-method suite -- the cost the OFF default
  # exists to avoid.
  fresh <- step2_run_request(0L, 0L)
  expect_false(fresh$pending)

  pressed <- step2_run_request(1L, 0L)
  expect_true(pressed$pending)

  # Served: the reactive records the count it ran for, and the SAME count must
  # not authorise a second run. This is the assertion the latch failed.
  expect_false(step2_run_request(1L, 1L)$pending)
  expect_false(step2_run_request(7L, 7L)$pending)

  # Pressing again does authorise one more run, and exactly one.
  expect_true(step2_run_request(2L, 1L)$pending)
  expect_false(step2_run_request(2L, 2L)$pending)
})

test_that("the spent baseline follows a rebuilt Run analysis button back down", {
  # app.R's step_body is a renderUI, so a Step 2 -> 3 -> 2 round trip rebuilds
  # the button and its counter restarts at 0 (see commit_loaded_data() in
  # R/step1_data.R). A baseline that only ever climbs would then swallow the
  # reviewer's next press -- numbered 1, against a stale baseline of 3 -- which
  # is the inert Run analysis button all over again.
  rebuilt <- step2_run_request(0L, 3L)
  expect_false(rebuilt$pending)
  expect_equal(rebuilt$spent, 0L)
  expect_true(step2_run_request(1L, rebuilt$spent)$pending)

  # A NULL counter is the same case: the widget has not reported yet.
  expect_false(step2_run_request(NULL, 3L)$pending)
  expect_equal(step2_run_request(NULL, 3L)$spent, 0L)
})

test_that("the analysis gate spends the press at the run, not at the gate", {
  # Two properties of the reactive that no pure helper can hold, asserted on
  # the source because their absence is invisible in every other test:
  #
  #  * the gate consults step2_run_request(), not the raw counter. A rewrite
  #    back to `input$run_ma > 0L` here is the whole bug.
  #  * the press is spent immediately before the run, not at the gate. Every
  #    exit in between is a cheap guard on something the reviewer is expected
  #    to go and fix (a blank required field, arm labels left over from the
  #    previous dataset); a press held across those is served the moment the
  #    blocker clears, instead of needing a second click.
  src <- gsub("[[:space:]]+", " ", paste(deparse(body(step2_server)),
                                         collapse = " "))
  expect_true(grepl("if (!auto && !run_pending) return(NULL)", src,
                    fixed = TRUE))
  expect_false(grepl("(input$run_ma %||% 0L) > 0L", src, fixed = TRUE))
  expect_match(
    src,
    "shiny::isolate(run_clicks_spent(run_clicks)) shiny::withProgress(",
    fixed = TRUE
  )
})

test_that("the required-field warnings keep the latch the run gate gave up", {
  # "Has the reviewer asked for an analysis at all?" is a DIFFERENT question
  # from "is a press waiting to be served", and the two warning branches want
  # the first one: before the first request a half-filled form is a normal
  # state and stays quiet, afterwards it is worth a toast however the reviewer
  # got back to it. Fixing the run gate by making these one-shot too would
  # silence the "the analysis has been cleared" toast on the common path, where
  # auto-rerun is left ON and nothing is ever pending.
  src <- gsub("[[:space:]]+", " ", paste(deparse(body(step2_server)),
                                         collapse = " "))
  expect_true(grepl("ever_run_requested <- run_clicks > 0L", src, fixed = TRUE))
  expect_true(grepl("if (!auto || ever_run_requested || had_ma) {", src,
                    fixed = TRUE))
  expect_true(grepl("if (!auto || ever_run_requested) {", src, fixed = TRUE))
})

test_that("the column-mapping dropdowns are selectize widgets", {
  # A native <select> (selectize = FALSE) is token-styled while closed but its
  # open list is OS chrome that no stylesheet can reach, so these six changed
  # appearance at the moment they were being read. selectize renders a
  # <script data-for="<id>"> beside the hidden <select>; a native one does not.
  html <- step2_html()
  for (id in c("col_studlab", "col_treat", "col_n", "col_event",
               "col_mean", "col_sd")) {
    expect_true(grepl(sprintf('data-for="%s"', id), html, fixed = TRUE),
                info = id)
  }
  # subgroup_col is deliberately left native and must stay that way.
  expect_false(grepl('data-for="subgroup_col"', html, fixed = TRUE))
})
