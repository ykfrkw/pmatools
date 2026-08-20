# test-pubias-dots.R - the status dots on the three publication-bias
# reference tabs (pma_tab_status_dot() in R/ui_helpers.R, painted by
# www/shadcn.css, fed by the package's .pubias_*_dot() functions).
#
# Two things this file guards, both of which are about what the dot is NOT.
#
# First, it is not pma_tab_mark(). That marker is a round glyph meaning "the
# reviewer opened this tab and has not confirmed the domain" - their
# PROGRESS - and it sits on the Step 3 tabs one level up from these. Two
# unrelated meanings a few pixels apart would be unreadable, so the status
# dot takes its own class and its own shape and pma-tab-mark is untouched.
#
# Second, it rates nothing. No dot value may reach grade_meta() or
# assess_pubias(); the wizard's answers stay the only thing that rates the
# domain. The last block is a structural check on that, because a runtime one
# could only prove that today's inputs happen not to carry a dot.

library(testthat)

test_that("each state renders its own class, tooltip and label", {
  for (state in c("green", "amber", "red", "unknown")) {
    html <- as.character(
      pma_tab_status_dot(list(state = state, reason = "because reasons")))
    expect_match(html, paste0("pma-tab-status-", state), fixed = TRUE)
    expect_match(html, "because reasons", fixed = TRUE)
    # The colour is not the only channel: a reader who cannot see it gets
    # the same verdict from the accessible name.
    expect_match(html, unname(PMA_STATUS_DOT_LABELS[[state]]), fixed = TRUE)
  }
})

test_that("the tooltip is the point of the fourth state", {
  html <- as.character(pma_tab_status_dot(list(
    state = "unknown",
    reason = "Not computed: Egger's test is underpowered below 10 studies.")))
  expect_match(html, "underpowered below 10 studies", fixed = TRUE)
  # And it is NOT painted as one of the three colours. "Not computed"
  # rendered green would read as "nothing wrong", which is backwards for
  # every tab here: each declines to compute on exactly the sparse data
  # where reporting bias is most likely.
  for (colour in c("green", "amber", "red")) {
    expect_false(grepl(paste0("pma-tab-status-", colour), html, fixed = TRUE))
  }
})

test_that("nothing is rendered for a missing or unrecognised state", {
  expect_null(pma_tab_status_dot(NULL))
  expect_null(pma_tab_status_dot(list(reason = "x")))
  expect_null(pma_tab_status_dot(list(state = "chartreuse", reason = "x")))
})

test_that("the status dot never borrows the domain tabs' progress mark", {
  dot  <- as.character(pma_tab_status_dot(list(state = "red", reason = "x")))
  mark <- as.character(pma_tab_mark(confirmed = FALSE, visited = TRUE))

  expect_false(grepl("pma-tab-mark", dot, fixed = TRUE))
  expect_false(grepl("pma-tab-status", mark, fixed = TRUE))
  # The progress mark is still the glyph it always was.
  expect_match(mark, "&#9679;", fixed = TRUE)
})

test_that("the CSS gives the dot its own shape, and leaves the mark alone", {
  css <- readLines(file.path(PMA_APP_ROOT, "www", "shadcn.css"), warn = FALSE)
  css <- paste(css, collapse = "\n")

  expect_match(css, ".pma-tab-status {", fixed = TRUE)
  for (state in c("green", "amber", "red", "unknown")) {
    expect_match(css, paste0(".pma-tab-status-", state), fixed = TRUE)
  }
  # A rounded square, not the round glyph one level up. If this ever became
  # `border-radius: 50%` the two markers would be the same shape again.
  expect_false(grepl("\\.pma-tab-status \\{[^}]*border-radius:\\s*50%", css))
  # "not computed" is hollow rather than a colour.
  expect_match(css, ".pma-tab-status-unknown { background: transparent; }",
               fixed = TRUE)
})

test_that("all three reference tabs carry a dot slot", {
  html <- as.character(step3_ui())
  for (id in c("pubias_dot_funnel", "pubias_dot_trimfill",
               "pubias_dot_missing")) {
    expect_match(html, paste0("id=\"", id, "\""), fixed = TRUE)
  }
  # A tagList title leaves tabPanel with no string to derive a value from,
  # so each of the three states its own (see .tab_title() on the domain
  # tabs, which learned this first).
  expect_match(html, "Missing results (RoB-ME)", fixed = TRUE)
})

test_that("no dot value reaches grade_meta() or assess_pubias()", {
  dot_names <- c(".pubias_funnel_dot", ".pubias_trimfill_dot",
                 ".pubias_missing_dot", ".pubias_missing_tipping",
                 "pma_tab_status_dot",
                 "pubias_dot_funnel", "pubias_dot_trimfill",
                 "pubias_dot_missing")
  # grade_obj() is the one reactive that assembles the argument list and
  # hands it to grade_meta() (through do.call, which is why scanning the call
  # site alone would prove nothing - the arguments are built above it). The
  # assertion is therefore made against everything inside that reactive, plus
  # every direct call to a rating entry point anywhere in the app.
  #
  # grade_obj() no longer holds that assembly on its own: each domain's
  # arguments are built by a closure beside it, and the call plus everything
  # done to the rating afterwards by a second set. They are named here for the
  # same reason grade_obj() is - a dot value reaching the rating through any
  # one of them is the thing this test exists to catch. A new block added to
  # grade_obj() belongs in this list.
  rating_calls   <- c("grade_meta", "assess_pubias", "grade_meta_multi")
  rating_holders <- c("grade_obj",
                      ".rob_grade_args", ".incon_grade_args",
                      ".indir_grade_args", ".impre_grade_args",
                      ".ois_grade_args", ".pubias_grade_args",
                      ".rare_grade_args",
                      ".run_grade_with_refit", ".apply_threshold_note",
                      ".append_rare_crossing_note", ".apply_pubias_override",
                      ".apply_other_downgrade", ".attach_grade_call_args")

  .rating_names <- function(expr, acc = character(0)) {
    if (!is.call(expr)) return(acc)
    head <- expr[[1L]]
    is_assign <- is.name(head) && as.character(head) %in% c("<-", "=", "<<-")
    if (is_assign && is.name(expr[[2L]]) &&
        as.character(expr[[2L]]) %in% rating_holders) {
      acc <- c(acc, all.names(expr[[3L]]))
    }
    if (is.name(head) && as.character(head) %in% rating_calls) {
      acc <- c(acc, all.names(expr))
    }
    for (part in as.list(expr)) {
      if (missing(part)) next
      acc <- .rating_names(part, acc)
    }
    acc
  }

  found <- character(0)
  for (f in list.files(file.path(PMA_APP_ROOT, "R"), pattern = "[.]R$",
                       full.names = TRUE)) {
    for (e in parse(f)) {
      found <- c(found, .rating_names(e))
    }
  }
  # The walker found grade_obj and the rating calls at all - otherwise an
  # empty intersection below would mean nothing.
  expect_true("assess_rob" %in% found || "grade_meta" %in% found)
  expect_equal(intersect(unique(found), dot_names), character(0))
})
