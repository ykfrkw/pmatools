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
