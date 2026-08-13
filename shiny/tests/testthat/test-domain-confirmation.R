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
