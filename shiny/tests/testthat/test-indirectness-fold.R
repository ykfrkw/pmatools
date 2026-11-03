# The Indirectness worst-case fold and its rationale gate
# (R/step3_threshold.R).
#
# The overall rating ships PRESELECTED. Before that it shipped blank, and
# blank was how a reviewer said "accept the fold of the four PICO answers" -
# so the rationale gate could key on "is anything selected at all?" and be
# right. Preselecting turns that blank into a real answer, and a gate that
# still asked "is anything selected?" would demand a written reason for a
# default nobody chose. These tests hold the replacement to that: the demand
# fires on a genuine departure from the fold, and on nothing else.

test_that("the fold takes the most severe answered level", {
  expect_identical(
    step3_indir_worst_case(rep("not_serious", 4L)), "not_serious")
  expect_identical(
    step3_indir_worst_case(c("not_serious", "serious", "not_serious",
                             "not_serious")),
    "serious")

  # The regression this fold was rewritten for. Its predecessor ranked
  # "no" / "some_concerns" / "serious", which is the vocabulary 0.5.1
  # replaced: every level the PICO answers actually produce missed the table,
  # so four answers containing a "No" folded to nothing and the domain was
  # reported as folding to "not serious".
  expect_identical(
    step3_indir_worst_case(c("not_serious", "not_serious", "very_serious",
                             "not_serious")),
    "very_serious")
  expect_identical(
    step3_indir_worst_case(c("serious", "very_serious")), "very_serious")
  expect_identical(
    step3_indir_worst_case(c("very_serious", "extremely_serious")),
    "extremely_serious")
})

test_that("the fold reports nothing rather than guessing", {
  expect_null(step3_indir_worst_case(NULL))
  expect_null(step3_indir_worst_case(character(0)))
  expect_null(step3_indir_worst_case(NA_character_))
  # An unrecognised level is dropped, not ranked: ranking it would put a
  # judgment nobody made at the top of the fold.
  expect_null(step3_indir_worst_case(c("some_concerns", "no")))
  expect_identical(
    step3_indir_worst_case(c("some_concerns", "serious")), "serious")
})

test_that("the preselected default owes no written reason", {
  # What the tab ships with: four "do not rate down" answers folding to
  # not_serious, and an overall rating restating it.
  expect_identical(STEP3_INDIR_DEFAULT_LEVEL, "not_serious")
  worst <- step3_indir_worst_case(rep(STEP3_INDIR_DEFAULT_LEVEL, 4L))
  expect_identical(worst, STEP3_INDIR_DEFAULT_LEVEL)
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL,
                                              worst))

  # No fold at all (nothing answered) reads as the default, which is what
  # grade_obj() sends on the scalar path.
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL, NULL))
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL, ""))
  expect_false(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL,
                                              NA_character_))

  # A restatement of a downgraded fold is still a restatement.
  expect_false(step3_indir_rationale_required("serious", "serious"))
})

test_that("a genuine departure from the fold owes one", {
  # Rating down further than the four answers warrant.
  expect_true(step3_indir_rationale_required("serious", "not_serious"))
  # And rating down LESS than they warrant, which is the direction most in
  # need of a written reason: the preselected default sitting over a PICO
  # element the reviewer has just downgraded.
  expect_true(step3_indir_rationale_required(STEP3_INDIR_DEFAULT_LEVEL,
                                             "very_serious"))
  expect_true(step3_indir_rationale_required("extremely_serious", "serious"))
})

test_that("an unanswered overall rating demands nothing", {
  for (empty in list(NULL, "", character(0), NA_character_)) {
    expect_false(step3_indir_rationale_required(empty, "very_serious"))
  }
})

test_that("the Indirectness tab ships the overall rating preselected", {
  # The UI half of the same contract. step3_ui() needs a Shiny session to
  # render, so the source is read: what matters is that the radio names the
  # constant rather than a second literal that could drift away from it, and
  # that the rationale panel is gated on the fold comparison rather than on
  # "is anything selected".
  lines <- readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                     warn = FALSE)
  src <- paste(lines, collapse = "\n")

  at <- grep('radioButtons("indirectness", NULL,', lines, fixed = TRUE)
  expect_length(at, 1L)
  # The two lines that follow the widget's own: the choices, then what it
  # ships selected.
  expect_true(any(grepl("selected = STEP3_INDIR_DEFAULT_LEVEL",
                        lines[at + (1:2)], fixed = TRUE)))
  expect_false(any(grepl("selected = character(0)",
                         lines[at + (1:2)], fixed = TRUE)))

  expect_true(grepl('"output.indir_override_active === true"', src,
                    fixed = TRUE))
  expect_false(grepl("(input.indirectness || '') != ''", src, fixed = TRUE))
})

# ----- Fig 8-2 answer codes (living Core GRADE Guide) -----------------------

test_that("the radios offer exactly the Fig 8-2 codes of their own branch", {
  expect_setequal(unname(STEP3_INDIR_PIC_ANSWERS),
                  c("no_difference", "similar", "dissimilar",
                    "very_dissimilar"))
  expect_setequal(unname(STEP3_INDIR_OUTCOME_ANSWERS),
                  c("similar_definition", "different_definition",
                    "surrogate_very_close", "surrogate_close",
                    "surrogate_far"))
  # Every offered code has a level, and nothing else does.
  expect_setequal(names(STEP3_INDIR_ANSWER_TO_LEVEL),
                  unname(STEP3_INDIR_ANSWERS))
  # The four preselected answers fold to the default, so the preselected
  # overall rating owes no reason.
  expect_identical(
    step3_indir_worst_case(unname(
      STEP3_INDIR_ANSWER_TO_LEVEL[STEP3_INDIR_DEFAULT_ANSWERS])),
    STEP3_INDIR_DEFAULT_LEVEL)
  expect_true(STEP3_INDIR_DEFAULT_ANSWERS[["indir_outcome"]] %in%
                STEP3_INDIR_OUTCOME_ANSWERS)
  expect_setequal(names(STEP3_INDIR_DEFAULT_ANSWERS),
                  unname(STEP3_INDIR_SUBDOMAINS))
})

test_that("the app's level table agrees with the package's", {
  pkg <- pma_vendored_pkg()
  skip_if(is.null(pkg), "vendored pmatools not staged")
  expect_identical(
    STEP3_INDIR_ANSWER_TO_LEVEL,
    pkg$INDIRECTNESS_ANSWER_TO_GRADE[names(STEP3_INDIR_ANSWER_TO_LEVEL)])
})

test_that("a legacy answer is restored as the Fig 8-2 choice of the same level", {
  legacy_level <- c(yes = "not_serious", probably_yes = "not_serious",
                    probably_no = "serious", no = "very_serious")
  for (id in unname(STEP3_INDIR_SUBDOMAINS)) {
    offered <- if (identical(id, "indir_outcome")) {
      STEP3_INDIR_OUTCOME_ANSWERS
    } else STEP3_INDIR_PIC_ANSWERS
    for (ans in names(legacy_level)) {
      up <- step3_indir_upgrade_answer(id, ans)
      expect_true(up %in% offered, info = paste(id, ans))
      expect_identical(unname(STEP3_INDIR_ANSWER_TO_LEVEL[up]),
                       unname(legacy_level[ans]), info = paste(id, ans))
    }
  }
  # The specified mapping, pinned.
  expect_identical(step3_indir_upgrade_answer("indir_population", "probably_yes"),
                   "similar")
  expect_identical(step3_indir_upgrade_answer("indir_comparator", "no"),
                   "very_dissimilar")
  expect_identical(step3_indir_upgrade_answer("indir_outcome", "yes"),
                   "similar_definition")
  # Not "surrogate_very_close": that answer now needs a written reason, and
  # a restored answer must keep its level without one.
  expect_identical(step3_indir_upgrade_answer("indir_outcome", "probably_yes"),
                   "similar_definition")
  expect_identical(step3_indir_upgrade_answer("indir_outcome", "probably_no"),
                   "surrogate_close")
  expect_identical(step3_indir_upgrade_answer("indir_outcome", "no"),
                   "surrogate_far")
})

test_that("the upgrade leaves everything else alone", {
  # A Fig 8-2 code, another input's "yes", and empty values pass through.
  expect_identical(step3_indir_upgrade_answer("indir_outcome", "surrogate_far"),
                   "surrogate_far")
  expect_identical(step3_indir_upgrade_answer("subgroup_explained", "yes"),
                   "yes")
  expect_identical(step3_indir_upgrade_answer("pubias_unpublished", "no"), "no")
  expect_null(step3_indir_upgrade_answer("indir_outcome", NULL))
  expect_identical(step3_indir_upgrade_answer("indir_outcome", NA_character_),
                   NA_character_)
  expect_identical(step3_indir_upgrade_answer("indir_population", ""), "")
})

test_that("restored answers go through the upgrade before reaching a radio", {
  src <- paste(readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                         warn = FALSE), collapse = "\n")
  expect_match(src, "v <- step3_indir_upgrade_answer(id, v)\n      session$sendInputMessage(id, list(value = v))",
               fixed = TRUE)
})

# ----- A "very close" surrogate needs a written reason (app 3.8.0) ---------

test_that("very close without a reason is sent as close", {
  for (blank in list(NULL, "", "   ", NA_character_)) {
    out <- step3_indir_outcome_effective("surrogate_very_close", blank)
    expect_identical(out$judgment, "surrogate_close")
    expect_identical(out$rationale, NA_character_)
    expect_true(out$fell_back)
  }
  # The fallback is the package's own suggested alternative, one level down.
  expect_identical(
    unname(STEP3_INDIR_ANSWER_TO_LEVEL[STEP3_INDIR_VERY_CLOSE_FALLBACK]),
    "serious")
})

test_that("very close with a reason is sent with that reason", {
  out <- step3_indir_outcome_effective("surrogate_very_close",
                                       "  The surrogate is the event.  ")
  expect_identical(out$judgment, "surrogate_very_close")
  expect_identical(out$rationale, "The surrogate is the event.")
  expect_false(out$fell_back)
})

test_that("every other answer passes through untouched", {
  others <- setdiff(unname(STEP3_INDIR_ANSWERS), "surrogate_very_close")
  for (ans in others) {
    out <- step3_indir_outcome_effective(ans, "a reason typed, then hidden")
    expect_identical(out$judgment, ans, info = ans)
    # A reason left in the hidden box is not sent with another answer.
    expect_identical(out$rationale, NA_character_, info = ans)
    expect_false(out$fell_back, info = ans)
  }
  expect_null(step3_indir_outcome_effective(NULL, NULL)$judgment)
  expect_identical(step3_indir_outcome_effective(NA_character_)$judgment,
                   NA_character_)
})

test_that("the fold and the override gate see the effective answer", {
  sent <- step3_indir_outcome_effective("surrogate_very_close", "")$judgment
  worst <- step3_indir_worst_case(unname(STEP3_INDIR_ANSWER_TO_LEVEL[
    c("no_difference", "no_difference", "no_difference", sent)]))
  expect_identical(worst, "serious")
  # The preselected "not serious" overall now departs from that fold.
  expect_true(step3_indir_rationale_required("not_serious", worst))

  # And the server wires all three to the same reactive.
  src <- paste(readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                         warn = FALSE), collapse = "\n")
  expect_match(src, "outcome   <- indir_outcome_resolved()", fixed = TRUE)
  expect_match(src, "\"input.indir_outcome == 'surrogate_very_close'\"",
               fixed = TRUE)
  expect_match(src, '"indir_outcome_rationale"', fixed = TRUE)
  expect_true("indir_outcome_rationale" %in% pma_outcome_input_ids())
})

test_that("the package accepts what the app sends", {
  pkg <- pma_vendored_pkg()
  skip_if(is.null(pkg), "vendored pmatools not staged")
  row <- function(judgment, rationale) {
    data.frame(subdomain = "Outcome", judgment = judgment,
               rationale = rationale, stringsAsFactors = FALSE)
  }
  for (reason in c("", "The surrogate is the event.")) {
    out <- step3_indir_outcome_effective("surrogate_very_close", reason)
    tbl <- pkg$.normalize_indirectness_subdomains(
      row(out$judgment, out$rationale))
    expect_identical(tbl$judgment, out$judgment)
  }
  # What the app never sends, the package refuses.
  expect_error(pkg$.normalize_indirectness_subdomains(
    row("surrogate_very_close", NA_character_)), "surrogate_very_close")
})

test_that("the very-close option says a reason is required", {
  label <- names(STEP3_INDIR_OUTCOME_ANSWERS)[
    STEP3_INDIR_OUTCOME_ANSWERS == "surrogate_very_close"]
  expect_match(label, "(reason required)", fixed = TRUE)
  expect_match(EDU_COPY$domains$indirectness$surrogate, "reason required",
               fixed = TRUE)
})

# ----- Core GRADE's worked examples, per answer -----------------------------

test_that("every question has a Core GRADE examples block", {
  ex <- EDU_COPY$domains$indirectness$examples
  for (el in c("population", "intervention", "comparison", "outcome")) {
    html <- as.character(step3_indir_examples_details(el))
    expect_match(html, "<details", fixed = TRUE, info = el)
    expect_match(html, ex$title, fixed = TRUE, info = el)
    expect_match(html, ex$source, fixed = TRUE, info = el)
  }
  # One entry per answer the radio offers: three for P/I/C (no differences
  # and similar share one), five for Outcome.
  for (el in c("population", "intervention", "comparison")) {
    expect_length(ex[[el]], 3L)
  }
  expect_length(ex$outcome, length(STEP3_INDIR_OUTCOME_ANSWERS))
})

test_that("an answer with no Core GRADE example says so", {
  ex <- EDU_COPY$domains$indirectness$examples
  html <- as.character(step3_indir_examples_details("intervention"))
  expect_match(html, ex$none, fixed = TRUE)
  out <- as.character(step3_indir_examples_details("outcome"))
  expect_match(out, ex$none, fixed = TRUE)
  expect_match(out, "footnote", fixed = TRUE)
  expect_match(out, "would patients take it", fixed = TRUE)
  # No example the sources do not give.
  all_text <- paste(unlist(ex), collapse = " ")
  expect_false(grepl("all-cause|disease-specific", all_text))
  expect_null(step3_indir_examples_details("setting"))
})

test_that("the four examples blocks are on the tab", {
  src <- paste(readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                         warn = FALSE), collapse = "\n")
  for (el in c("population", "intervention", "comparison", "outcome")) {
    expect_match(src, sprintf('step3_indir_examples_details("%s")', el),
                 fixed = TRUE, info = el)
  }
})
