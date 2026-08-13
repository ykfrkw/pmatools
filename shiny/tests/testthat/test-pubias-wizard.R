# The publication-bias wizard's node derivation (R/step3_threshold.R).
#
# The node is DERIVED from the answers, not stored. That is the property these
# tests exist for: change an earlier answer and everything after it must
# re-derive, so the reviewer can never be parked on a question the algorithm
# no longer reaches. The chain mirrors assess_pubias()'s own short-circuit
# order (R/domain_pubias.R), which is why the app cannot drift away from the
# package without one of these failing.

test_that("Q1 is the entry node and is terminal on 'yes'", {
  expect_identical(step3_pubias_node(), "q1")
  expect_identical(step3_pubias_node(small_industry = ""), "q1")

  # Fig 5 node 1: sufficient on its own. Nothing after it can undo it, so the
  # wizard goes straight to the result even with later answers present.
  expect_identical(
    step3_pubias_node(small_industry = "yes", registry_complete = "defer",
                      funnel_asymmetry = "egger", k = 14),
    "result")
})

test_that("the overall reporting-bias question is terminal both ways", {
  expect_identical(step3_pubias_node(small_industry = "no"), "extra")

  for (ans in c("yes", "no")) {
    expect_identical(
      step3_pubias_node(small_industry = "no", registry_complete = ans,
                        k = 14),
      "result")
  }
})

test_that("only the explicit deferral falls through to the Figure 5 nodes", {
  # This is why the widget carries a "defer" VALUE rather than a blank: a
  # blank cannot be told apart from "the reviewer has not got here yet", and
  # the wizard could then never move past the question.
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = STEP3_PUBIAS_DEFER, k = 14),
    "q3")
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = STEP3_PUBIAS_DEFER, k = 4),
    "q4")
})

test_that("k routes to Q3 or Q4 and Q2 is never a question", {
  base <- list(small_industry = "no", registry_complete = STEP3_PUBIAS_DEFER)

  expect_identical(do.call(step3_pubias_node, c(base, list(k = 10))), "q3")
  expect_identical(do.call(step3_pubias_node, c(base, list(k = 9))),  "q4")
  expect_true(step3_pubias_statistical(10))
  expect_false(step3_pubias_statistical(9))
  expect_false(step3_pubias_statistical(NA))

  # Q2 shows up as a reported step, not a screen.
  expect_match(step3_pubias_k_line(14), "k = 14 >= 10")
  expect_match(step3_pubias_k_line(14), "statistical route")
  expect_match(step3_pubias_k_line(4), "registry route")
  expect_false("q2" %in% STEP3_PUBIAS_NODES)
})

test_that("answering the terminal node reaches the result", {
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = STEP3_PUBIAS_DEFER,
                      funnel_asymmetry = STEP3_PUBIAS_USE_EGGER, k = 14),
    "result")
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = STEP3_PUBIAS_DEFER,
                      funnel_asymmetry = "yes", k = 14),
    "result")
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = STEP3_PUBIAS_DEFER,
                      unpublished = "no", k = 4),
    "result")
  # An answer on the branch that was NOT taken must not advance the wizard.
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = STEP3_PUBIAS_DEFER,
                      unpublished = "no", k = 14),
    "q3")
})

test_that("a breadcrumb re-open wins, but only for a reachable node", {
  answered <- list(small_industry = "no",
                   registry_complete = STEP3_PUBIAS_DEFER,
                   funnel_asymmetry = STEP3_PUBIAS_USE_EGGER, k = 14)

  expect_identical(do.call(step3_pubias_node,
                           c(answered, list(reopen = "q1"))), "q1")
  expect_identical(do.call(step3_pubias_node,
                           c(answered, list(reopen = "extra"))), "extra")

  # q4 is on the k < 10 branch, which these answers do not take: honouring it
  # would strand the reviewer on a question the algorithm never asks.
  expect_identical(do.call(step3_pubias_node,
                           c(answered, list(reopen = "q4"))), "result")

  # Same guard after an answer changes the path: Q1 = "yes" is terminal, so a
  # stale re-open of Q3 is ignored rather than resurrecting it.
  expect_identical(
    step3_pubias_node(small_industry = "yes", registry_complete = "defer",
                      k = 14, reopen = "q3"),
    "result")

  expect_null(NULL)
  expect_identical(do.call(step3_pubias_node,
                           c(answered, list(reopen = NULL))), "result")
})

test_that("the reachable path is what the breadcrumb may link to", {
  expect_identical(step3_pubias_reachable(), "q1")
  expect_identical(step3_pubias_reachable(small_industry = "yes"),
                   c("q1", "result"))
  expect_identical(step3_pubias_reachable(small_industry = "no"),
                   c("q1", "extra"))
  expect_identical(
    step3_pubias_reachable(small_industry = "no", registry_complete = "yes"),
    c("q1", "extra", "result"))
  expect_identical(
    step3_pubias_reachable(small_industry = "no",
                           registry_complete = STEP3_PUBIAS_DEFER, k = 14),
    c("q1", "extra", "q3", "result"))
  expect_identical(
    step3_pubias_reachable(small_industry = "no",
                           registry_complete = STEP3_PUBIAS_DEFER, k = 3),
    c("q1", "extra", "q4", "result"))
})
