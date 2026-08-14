# test-confirm-checkbox.R - the Step 3 confirmation boxes
# (pma_confirm_checkbox() in R/ui_helpers.R, painted by www/shadcn.css).
#
# What shipped before this file existed: `responder_p0_confirm` gated the
# Configuration tab's Next button exactly as `threshold_confirm` does, but was
# built as a bare checkboxInput() because the boxing helper was a closure
# inside step3_ui() that R/step3_threshold.R could not reach. Two gates on one
# tab, one of them indistinguishable from the notes around it.
#
# So the assertion below is deliberately made against the BUILT UI rather than
# against the helper alone: a future confirmation added as a bare
# checkboxInput() has to fail here rather than ship unmarked. The set equality
# is two-directional on purpose - a box that is not in PMA_OUTCOME_CONFIRM_IDS
# would not be cleared when the reviewer moves to the next outcome, so a new
# confirmation that skips the registry is as much a bug as one that skips the
# helper.

library(testthat)

# Every input id sitting inside a .pma-confirm container, anywhere in `ui`.
.boxed_confirm_ids <- function(ui) {
  boxes <- htmltools::tagQuery(ui)$find(".pma-confirm")$selectedTags()
  vapply(boxes, function(box) {
    inputs <- htmltools::tagQuery(box)$find("input")$selectedTags()
    if (!length(inputs)) return(NA_character_)
    inputs[[1L]]$attribs$id %||% NA_character_
  }, character(1))
}

test_that("pma_confirm_checkbox() emits the container, the id and the label", {
  html <- as.character(pma_confirm_checkbox("some_confirm", "Tick me"))

  expect_match(html, "class=\"pma-confirm\"", fixed = TRUE)
  expect_match(html, "id=\"some_confirm\"", fixed = TRUE)
  expect_match(html, "Tick me", fixed = TRUE)
  expect_match(html, "type=\"checkbox\"", fixed = TRUE)

  # The eyebrow is what says "this is an outstanding action" before the
  # reviewer has pressed anything, and www/shadcn.css can only grey it out on
  # tick if it is in the DOM to begin with.
  expect_match(html, "class=\"pma-confirm-eyebrow\">Required<", fixed = TRUE)

  # Never pre-ticked: the tick IS the confirmation, so a default of TRUE would
  # open the export gate for an outcome nobody has looked at.
  expect_no_match(html, "checked", fixed = TRUE)

  # The default label is the one STEP3_CONFIRM_GATE_TITLE quotes back at the
  # reviewer on the greyed-out Next button, so the two have to stay identical.
  expect_match(as.character(pma_confirm_checkbox("d")),
               "I have reviewed this domain", fixed = TRUE)
  expect_match(STEP3_CONFIRM_GATE_TITLE, "I have reviewed this domain",
               fixed = TRUE)
})

test_that("every confirmation in the registry is rendered through it", {
  # .responder_block() is not reachable from step3_ui(): output$threshold_panel
  # renders it server-side, and only for a continuous outcome. Building it for
  # an SMD alongside the static tab body is what puts all seven ids in one
  # tree.
  built <- htmltools::tagList(step3_ui(), .responder_block("SMD"))

  expect_setequal(.boxed_confirm_ids(built), PMA_OUTCOME_CONFIRM_IDS)
})

test_that("the boxing helper has exactly one implementation", {
  src <- paste(readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                         warn = FALSE),
               collapse = "\n")

  # The closure this replaced. It could not be reached from
  # R/step3_threshold.R, which is how the two Configuration-tab gates came to
  # look like different kinds of control; a reintroduced local copy would put
  # that back.
  expect_no_match(src, "\\.confirm_checkbox\\s*<-\\s*function")
  expect_no_match(src, "[^_]\\.confirm_checkbox\\(")
})
