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

# ----- The ranking rule (shiny/SPEC.md 3.4.13) -----------------------------
#
# "A left accent on a filled ground means 'answer this', and nothing else on a
# tab may wear it." What shipped before: the read-only threshold-equivalence
# summary sat on a solid ground behind a 4px accent while the confirmation
# gating the same tab's Next carried a 1px translucent outline, so the tab
# taught the reviewer the opposite of what it meant. Asserted on the stylesheet
# and on the source rather than on a rendering, because that inversion is a
# pair of numbers in two files and nothing about it needs a browser to see.

.css_rule <- function(css, selector) {
  # Comments first: the block comment above .pma-confirm quotes the very
  # declarations these assertions read.
  bare  <- gsub("/\\*.*?\\*/", "", css)
  rules <- strsplit(bare, "}", fixed = TRUE)[[1L]]
  hit <- Filter(function(r) grepl(paste0(selector, "\\s*\\{"), r), rules)
  expect_true(length(hit) > 0, info = selector)
  hit[[1L]]
}

.left_border_px <- function(rule) {
  m <- regmatches(rule, regexpr("border-left:\\s*([0-9.]+)px", rule))
  expect_true(nzchar(m), info = rule)
  as.numeric(sub("^border-left:\\s*", "", sub("px$", "", m)))
}

test_that("a required answer is never lighter than a read-only block", {
  css <- paste(readLines(file.path(PMA_APP_ROOT, "www", "shadcn.css"),
                         warn = FALSE), collapse = "\n")

  question <- .css_rule(css, "\\.pma-wizard-question")
  confirm  <- .css_rule(css, "\\.pma-confirm")

  # Same weight and same ground as the one live wizard question, so "must be
  # answered" looks identical wherever it appears.
  expect_gte(.left_border_px(confirm), .left_border_px(question))
  expect_match(confirm, "border-left:\\s*4px solid hsl\\(var\\(--primary\\)\\)")
  expect_match(confirm, "background:\\s*hsl\\(var\\(--muted\\)\\)")

  # The wash it replaced. A translucent primary tint under a solid #f5f5f5
  # block is the inversion this rule exists to forbid.
  expect_no_match(confirm, "background:\\s*hsl\\(var\\(--primary\\) / 0\\.05\\)")
})

test_that("no read-only Step 3 block wears the question's accent", {
  src <- paste(readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                         warn = FALSE),
               collapse = "\n")

  # #0f172a IS --primary (222 47% 11%), so a 4px left border in it is the
  # wizard question's accent spelled as a literal. Both blocks that carried it
  # -- output$threshold_equiv and output$ois_rrr_equiv -- are derivations of
  # the box directly above them and are now body copy under it.
  #
  # Scoped to that colour on purpose: .pubias_egger_callout() accents in the
  # judgment's own green or amber, which is the status vocabulary and not this
  # rule's business.
  expect_no_match(src, "border-left: 4px solid #0f172a", fixed = TRUE)
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
