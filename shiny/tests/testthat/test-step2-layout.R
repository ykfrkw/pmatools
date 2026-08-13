# The Step 2 sidebar layout, asserted on the HTML step2_ui() renders.
#
# There is no browser driver here, so the overflow itself cannot be measured.
# What CAN be pinned is the pair of CSS declarations that caused it: a sidebar
# with `flex: 0 0 320px` refuses to shrink and a right pane with a flat
# `min-width: 480px` refuses to narrow below it, so on a 375px viewport the row
# was 492px wide and the whole document scrolled sideways. Both are one-token
# edits away from coming back, which is exactly what this catches.

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
  expect_true(grepl("flex: 1 1 320px", html, fixed = TRUE))
  expect_true(grepl("min-width: min(480px, 100%)", html, fixed = TRUE))
  # The two declarations that produced the 492px document.
  expect_false(grepl("flex: 0 0 320px", html, fixed = TRUE))
  expect_false(grepl("min-width: 480px", html, fixed = TRUE))
})

test_that("every Step 2 input id survives the accordion restructure", {
  # The whole app addresses these by id - Step 3 reads most of them off the
  # Step 2 inputs - so moving a control into an accordion panel must not touch
  # the id it is registered under. A rename is silent everywhere else.
  html <- step2_html()
  ids <- c("outcome_name", "small_values", "outcome_type", "outcome_follow_up",
           "outcome_unit", "col_studlab", "col_treat", "col_n", "col_event",
           "col_mean", "col_sd", "sm_bin", "model", "method", "method_tau",
           "incr", "subgroup_col", "auto_rerun", "run_ma",
           "arm_assignment_ui", "outcome_filter_ui", "sm_cont_ui",
           "subgroup_order_ui")
  for (id in ids) {
    expect_true(grepl(sprintf('id="%s"', id), html, fixed = TRUE), info = id)
  }
})

test_that("the sidebar is four accordion panels plus a sticky action bar", {
  html <- step2_html()
  for (value in c("outcome", "mapping", "model", "subgroup")) {
    expect_true(grepl(sprintf('data-value="%s"', value), html, fixed = TRUE),
                info = value)
  }
  expect_true(grepl("pma-step2-actions", html, fixed = TRUE))
})

test_that("Data mapping is open until an analysis exists, and closed after", {
  # `state$ma` is the proxy for "the mapping resolved": nothing else can have
  # produced a pooled object. Before that the panel is open because the
  # reviewer still has to confirm it; after it, re-opening on every return trip
  # from Step 3 is noise. A mapping select blanked afterwards is caught from
  # the other end, by www/required-fields.js.
  expect_setequal(step2_open_panels(step2_html(NULL)),
                  c("outcome", "mapping"))

  settled <- shiny::reactiveValues(ma = list(TE = 0))
  expect_setequal(step2_open_panels(step2_html(settled)), "outcome")
})
