# What confirms a certainty domain (R/ui_helpers.R).
#
# The rule is the export gate: a domain the app reports as confirmed is a
# domain whose rating can leave the app in a Summary of Findings table. It used
# to fire on substantive input too - a filled risk-of-bias table, an answered
# PICO radio, a valid override - which meant a preselected widget could confirm
# a domain nobody had looked at. It is now the checkbox and nothing else, and
# these tests are what holds it there.

# htmltools renders a tag list element by element; paste it back into one
# string so a match is a match against the whole fragment.
.rendered <- function(tag) paste(as.character(tag), collapse = "")

test_that("nothing ticked leaves every domain unconfirmed", {
  ids <- unname(PMA_DOMAIN_CONFIRM_INPUTS)
  none <- stats::setNames(rep(FALSE, length(ids)), ids)
  all_fresh <- stats::setNames(rep(TRUE, length(ids)), ids)

  conf <- pma_domain_confirmations(none, all_fresh)

  expect_equal(names(conf), names(PMA_DOMAIN_LABELS))
  expect_false(any(conf))
  expect_equal(pma_unconfirmed_domains(conf), unname(PMA_DOMAIN_LABELS))
})

test_that("only the domains whose box is ticked are confirmed", {
  ids <- unname(PMA_DOMAIN_CONFIRM_INPUTS)
  ticked <- stats::setNames(rep(FALSE, length(ids)), ids)
  ticked[[PMA_DOMAIN_CONFIRM_INPUTS[["rob"]]]] <- TRUE
  ticked[[PMA_DOMAIN_CONFIRM_INPUTS[["imprecision"]]]] <- TRUE
  fresh <- stats::setNames(rep(TRUE, length(ids)), ids)

  conf <- pma_domain_confirmations(ticked, fresh)

  expect_true(conf[["rob"]])
  expect_true(conf[["imprecision"]])
  expect_equal(pma_unconfirmed_domain_keys(conf),
               c("threshold", "inconsistency", "indirectness", "pubias"))
})

test_that("a tick left behind by the previous outcome confirms nothing", {
  # .fresh() is FALSE for an id last answered under another outcome
  # generation. Failing closed is the point: the stale tick locks the gate.
  ids <- unname(PMA_DOMAIN_CONFIRM_INPUTS)
  ticked <- stats::setNames(rep(TRUE, length(ids)), ids)
  fresh <- stats::setNames(rep(TRUE, length(ids)), ids)
  fresh[[PMA_DOMAIN_CONFIRM_INPUTS[["pubias"]]]] <- FALSE

  conf <- pma_domain_confirmations(ticked, fresh)

  expect_false(conf[["pubias"]])
  expect_equal(pma_unconfirmed_domains(conf), "Publication bias")

  # Every tick stale: an outcome the reviewer has not opened at all.
  none_fresh <- stats::setNames(rep(FALSE, length(ids)), ids)
  expect_false(any(pma_domain_confirmations(ticked, none_fresh)))
})

test_that("Configuration also needs the values it collects to be set", {
  # The one domain where a tick is not the whole story: three of the five
  # domains are judged against the threshold Configuration sets.
  ids <- unname(PMA_DOMAIN_CONFIRM_INPUTS)
  ticked <- stats::setNames(rep(TRUE, length(ids)), ids)
  fresh <- stats::setNames(rep(TRUE, length(ids)), ids)

  expect_true(pma_domain_confirmations(ticked, fresh,
                                       config_ready = TRUE)[["threshold"]])
  blocked <- pma_domain_confirmations(ticked, fresh, config_ready = FALSE)
  expect_false(blocked[["threshold"]])
  expect_equal(pma_unconfirmed_domains(blocked), "Configuration")
})

test_that("Configuration's threshold blocker follows the clinical question", {
  # config_blockers() is a reactive inside step3_server() and cannot be called
  # here, but its threshold branch is now one line over a pure helper:
  #   if (threshold_required() && threshold_missing()) blocker
  # So what has to hold is the helper's half. Superiority contributes NOTHING -
  # no blocker, and therefore a Configuration domain that confirms on the tick
  # alone with the threshold box empty.
  expect_false(pma_question_gate_copy("superiority")$required)
  expect_null(pma_question_gate_copy("superiority")$blocker)

  # Every other question contributes exactly one line, and it is lower case so
  # it reads inside "Still to do: %s.".
  for (q in c("important_superiority", "equivalence", "non_inferiority")) {
    gate <- pma_question_gate_copy(q)
    expect_true(gate$required, info = q)
    expect_match(gate$blocker, "^enter ", info = q)
  }

  # The word "MID" appears in none of it. The full audit over every string the
  # feature can emit is in test-step3-threshold.R; this is the gate's own copy,
  # which is what a reviewer reads while they are stuck.
  gate_strings <- unlist(lapply(PMA_CLINICAL_QUESTIONS, function(q) {
    gate <- pma_question_gate_copy(q)
    c(gate$blocker, gate$status, gate$no_rating)
  }), use.names = FALSE)
  expect_gt(length(gate_strings), 0L)
  expect_identical(
    gate_strings[grepl("\\bmid\\b", gate_strings, ignore.case = TRUE)],
    character(0))
})

test_that("the clinical question is a per-outcome answer, so it is re-asked", {
  # threshold_confirm is unticked on a question change (an observer in
  # step3_server()): the reviewer confirmed a configuration that no longer
  # exists, and on two of the four questions the threshold has just been
  # emptied. What is checkable without a session is that the two ids the
  # observer joins are both registered per-outcome, or the freshness guard
  # could never tell a question answered for this outcome from one left behind
  # by the last.
  expect_true("clinical_question" %in% PMA_OUTCOME_INPUT_IDS$configuration)
  expect_true("threshold_confirm" %in% PMA_OUTCOME_INPUT_IDS$configuration)
  expect_true(all(c("clinical_question", "threshold_confirm") %in%
                    pma_outcome_input_ids()))

  # threshold_confirm is a confirmation and is cleared on screen when the
  # outcome changes; the question is an ANSWER and is restored with the rest of
  # them, which is the split PMA_OUTCOME_CONFIRM_IDS draws.
  expect_true("threshold_confirm" %in% PMA_OUTCOME_CONFIRM_IDS)
  expect_false("clinical_question" %in% PMA_OUTCOME_CONFIRM_IDS)
})

test_that("an id the caller never reported is not confirmed", {
  # Missing rather than FALSE is what an input whose widget is not on screen
  # looks like; it must read as "not confirmed", not error.
  expect_false(any(pma_domain_confirmations(NULL, NULL)))
  expect_false(any(pma_domain_confirmations(c(rob_confirm_na = TRUE),
                                            c(rob_confirm_na = TRUE))[
                     c("threshold", "inconsistency")]))
})

test_that("the confirmation inputs are the ones a new outcome unticks", {
  # PMA_OUTCOME_CONFIRM_IDS is what app.R clears when the outcome changes. A
  # gate input missing from it would survive that clearing and confirm the
  # next outcome by itself.
  expect_setequal(names(PMA_DOMAIN_CONFIRM_INPUTS),
                  names(PMA_DOMAIN_LABELS))
  expect_true(all(PMA_DOMAIN_CONFIRM_INPUTS %in% PMA_OUTCOME_CONFIRM_IDS))
  # ... and each is registered per-outcome, or .fresh() could never stamp it.
  expect_true(all(PMA_DOMAIN_CONFIRM_INPUTS %in% pma_outcome_input_ids()))
})

test_that("the tab marker distinguishes confirmed, seen and untouched", {
  expect_null(pma_tab_mark(confirmed = FALSE, visited = FALSE))
  seen <- .rendered(pma_tab_mark(confirmed = FALSE, visited = TRUE))
  done <- .rendered(pma_tab_mark(confirmed = TRUE,  visited = TRUE))
  expect_match(seen, "pma-tab-mark-seen")
  expect_match(done, "pma-tab-mark-done")
  # Confirmed outranks visited, and a confirmation without a visit (restored
  # from the previous session's answers) still shows the tick.
  expect_match(.rendered(pma_tab_mark(TRUE, FALSE)), "pma-tab-mark-done")
})

test_that("jump links name the domain and carry a unique id per caller", {
  keys <- c("rob", "pubias")
  step3 <- .rendered(pma_domain_jump_links(keys, "cert_jump_"))
  step4 <- .rendered(pma_domain_jump_links(keys, "dl_jump_"))

  expect_match(step3, "cert_jump_rob")
  expect_match(step3, "cert_jump_pubias")
  expect_match(step3, "Risk of Bias", fixed = TRUE)
  expect_match(step4, "dl_jump_rob")
  # Both messages can be alive in one session, so the ids must not collide.
  expect_false(grepl("cert_jump_", step4, fixed = TRUE))
  expect_null(pma_domain_jump_links(character(0), "cert_jump_"))
})

test_that("the jump-link sentence has no stray spaces around its punctuation", {
  # htmltools joins a tag's children with a newline, which renders as a space,
  # so the words on either side are pasted into the same string.
  html <- .rendered(pma_domain_jump_links(c("rob", "pubias"), "cert_jump_",
                                          before = "Confirm: ", after = "."))
  expect_match(html, "Confirm: <a", fixed = TRUE)
  expect_match(html, "</a>, <a", fixed = TRUE)
  expect_match(html, "</a>.$")
})

test_that("the stepper shows the confirmed count only when it is given", {
  plain <- .rendered(pma_stepper(3))
  expect_match(plain, "Certainty")
  expect_false(grepl("Certainty 0/6", plain, fixed = TRUE))

  counted <- .rendered(pma_stepper(3, certainty_confirmed = 4))
  expect_match(counted, "Certainty 4/6", fixed = TRUE)
})
