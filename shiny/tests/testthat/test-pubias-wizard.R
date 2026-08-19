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
    step3_pubias_node(small_industry = "yes", registry_complete = "no",
                      funnel_asymmetry = "egger", k = 14),
    "result")
})

test_that("the overall reporting-bias question is terminal on 'yes' only", {
  expect_identical(step3_pubias_node(small_industry = "no"), "extra")

  expect_identical(
    step3_pubias_node(small_industry = "no", registry_complete = "yes",
                      k = 14),
    "result")
})

test_that("the registry question has no deferral value left to offer", {
  # Two answers now, and the third is gone rather than hidden: "no" IS the
  # deferral. A constant left behind would be an invitation to reinstate the
  # option that made "no" mean something else.
  expect_false(exists("STEP3_PUBIAS_DEFER"))
  expect_true(exists("STEP3_PUBIAS_USE_EGGER"))
})

test_that("'no' on the registry question falls through to the Figure 5 nodes", {
  # 0.5.1: "reporting bias is plausible" decides nothing on its own. It used
  # to force rate down 1 and end the wizard, next to a third "leave it to the
  # Figure 5 nodes" option that did what "no" does now.
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = "no", k = 14),
    "q3")
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = "no", k = 4),
    "q4")
})

test_that("k routes to Q3 or Q4 and Q2 is never a question", {
  base <- list(small_industry = "no", registry_complete = "no")

  expect_identical(do.call(step3_pubias_node, c(base, list(k = 10))), "q3")
  expect_identical(do.call(step3_pubias_node, c(base, list(k = 9))),  "q4")
  expect_true(step3_pubias_statistical(10))
  expect_false(step3_pubias_statistical(9))
  expect_false(step3_pubias_statistical(NA))

  # Q2 shows up as a reported step, not a screen. It also shows up without its
  # number: the wizard prints no question numbers (shiny/SPEC.md).
  expect_match(step3_pubias_k_line(14), "k = 14 >= 10")
  expect_match(step3_pubias_k_line(14), "Statistical analysis feasible")
  expect_match(step3_pubias_k_line(4), "registry route")
  expect_false(grepl("Q2", step3_pubias_k_line(14), fixed = TRUE))
  expect_false(grepl("Q2", step3_pubias_k_line(4), fixed = TRUE))
  expect_false("q2" %in% STEP3_PUBIAS_NODES)
})

test_that("answering the terminal node reaches the result", {
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = "no",
                      funnel_asymmetry = STEP3_PUBIAS_USE_EGGER, k = 14),
    "result")
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = "no",
                      funnel_asymmetry = "yes", k = 14),
    "result")
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = "no",
                      unpublished = "no", k = 4),
    "result")
  # An answer on the branch that was NOT taken must not advance the wizard.
  expect_identical(
    step3_pubias_node(small_industry = "no",
                      registry_complete = "no",
                      unpublished = "no", k = 14),
    "q3")
})

test_that("a breadcrumb re-open wins, but only for a reachable node", {
  answered <- list(small_industry = "no",
                   registry_complete = "no",
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
    step3_pubias_node(small_industry = "yes", registry_complete = "no",
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
                           registry_complete = "no", k = 14),
    c("q1", "extra", "q3", "result"))
  expect_identical(
    step3_pubias_reachable(small_industry = "no",
                           registry_complete = "no", k = 3),
    c("q1", "extra", "q4", "result"))
})

# --------------------------------------------------------------------------
# step3_pubias_flow_ids(): the chart above the wizard
# --------------------------------------------------------------------------
# The chart is a PROGRESS indicator, so it is lit from the answers rather than
# from the `flow_path` fact - the fact only exists once grade_meta() has rated
# the domain, and by then the reviewer has finished. That makes this the one
# place the app translates wizard node keys into figure ids on its own, and the
# one place it can drift away from inst/figures/pubias.svg without the package
# suite noticing.

.pubias_svg <- local({
  # The staged copy first, the source tree second - the same two places
  # pma_flowchart_path() looks.
  cands <- c(file.path(PMA_APP_ROOT, "_pmatools_inst", "figures",
                       "pubias.svg"),
             file.path(dirname(PMA_APP_ROOT), "inst", "figures", "pubias.svg"))
  hit <- Filter(file.exists, cands)
  if (length(hit)) hit[[1L]] else NA_character_
})

test_that("nothing is lit until something is answered", {
  expect_identical(step3_pubias_flow_ids(), character(0))
  expect_identical(step3_pubias_flow_ids(small_industry = "", k = 14),
                   character(0))
})

test_that("each answer lights its node, its edge and any leaf it reaches", {
  expect_identical(
    step3_pubias_flow_ids(small_industry = "yes"),
    c("pma-pubias-node-q1", "pma-pubias-edge-q1-yes",
      "pma-pubias-leaf-down1-q1"))

  # Answered Q1 = "no": the trail runs on to the node now being asked, so the
  # chart shows where the reviewer is as well as where they have been.
  expect_identical(
    step3_pubias_flow_ids(small_industry = "no"),
    c("pma-pubias-node-q1", "pma-pubias-edge-q1-no",
      "pma-pubias-node-registry"))

  expect_identical(
    step3_pubias_flow_ids(small_industry = "no", registry_complete = "yes"),
    c("pma-pubias-node-q1", "pma-pubias-edge-q1-no",
      "pma-pubias-node-registry", "pma-pubias-edge-registry-yes",
      "pma-pubias-leaf-nodown-registry"))
})

test_that("the k gate lights the branch it computed, on either side", {
  base <- list(small_industry = "no",
               registry_complete = "no")

  expect_identical(
    do.call(step3_pubias_flow_ids, c(base, list(k = 14))),
    c("pma-pubias-node-q1", "pma-pubias-edge-q1-no",
      "pma-pubias-node-registry", "pma-pubias-edge-registry-no",
      "pma-pubias-node-q2", "pma-pubias-edge-q2-yes", "pma-pubias-node-q3"))

  expect_identical(
    do.call(step3_pubias_flow_ids, c(base, list(k = 4))),
    c("pma-pubias-node-q1", "pma-pubias-edge-q1-no",
      "pma-pubias-node-registry", "pma-pubias-edge-registry-no",
      "pma-pubias-node-q2", "pma-pubias-edge-q2-no", "pma-pubias-node-q4"))

  # An answer on the branch that was NOT taken lights nothing on it, exactly
  # as it advances nothing in step3_pubias_node().
  expect_false("pma-pubias-leaf-nodown-q4" %in%
    do.call(step3_pubias_flow_ids,
            c(base, list(unpublished = "no", k = 14))))
})

test_that("the terminal answers light their leaves", {
  base <- list(small_industry = "no",
               registry_complete = "no")

  for (case in list(
    list(args = list(funnel_asymmetry = "yes", k = 14),
         leaf = "pma-pubias-leaf-down1-q3"),
    list(args = list(funnel_asymmetry = "no", k = 14),
         leaf = "pma-pubias-leaf-nodown-q3"),
    list(args = list(unpublished = "yes", k = 4),
         leaf = "pma-pubias-leaf-down1-q4"),
    list(args = list(unpublished = "no", k = 4),
         leaf = "pma-pubias-leaf-nodown-q4"))) {
    ids <- do.call(step3_pubias_flow_ids, c(base, case$args))
    expect_identical(ids[length(ids)], case$leaf)
  }
})

test_that("an answer that decides no leaf stops the trail at a node", {
  # Accepting the automated Egger test hands Q3 to a p value this function is
  # not given here, so the chart lights the node and waits. Supplied, the same
  # answer reaches a leaf - see the egger_asymmetric tests at the foot of this
  # file.
  ids <- step3_pubias_flow_ids(small_industry = "no",
                               registry_complete = "no",
                               funnel_asymmetry = STEP3_PUBIAS_USE_EGGER,
                               k = 14)
  expect_identical(ids[length(ids)], "pma-pubias-node-q3")
})

# --------------------------------------------------------------------------
# "Question 2 of 3": where the reviewer is in the wizard
# --------------------------------------------------------------------------
# One question on screen answers "what am I being asked" and never answered
# "how many of these are there". The count comes from the reachable path, so
# it can only ever name questions the current answers actually reach.

test_that("the total waits until the answers settle the route", {
  # Q1 unanswered: the reviewer's own next answer decides whether the wizard
  # ends here (Q1 = "yes") or runs to three questions. Printing "of 1" would
  # be a claim the answers have not made.
  expect_identical(
    step3_pubias_question_line("q1", step3_pubias_reachable()),
    "Question 1")

  # Same at the second question, for the same reason: "yes" ends it here.
  expect_identical(
    step3_pubias_question_line(
      "extra", step3_pubias_reachable(small_industry = "no")),
    "Question 2")
})

test_that("every reachable path numbers its own questions", {
  # The two terminal-on-"yes" short circuits: one question, then the result.
  expect_identical(
    step3_pubias_question_line(
      "q1", step3_pubias_reachable(small_industry = "yes")),
    "Question 1 of 1")
  expect_identical(
    step3_pubias_question_line(
      "extra", step3_pubias_reachable(small_industry = "no",
                                      registry_complete = "yes")),
    "Question 2 of 2")

  # The two full routes. Q2 is computed rather than asked, so it is not
  # counted: the reviewer answers three questions on either branch.
  stat <- step3_pubias_reachable(small_industry = "no",
                                 registry_complete = "no", k = 14)
  expect_identical(step3_pubias_question_line("q1", stat), "Question 1 of 3")
  expect_identical(step3_pubias_question_line("extra", stat),
                   "Question 2 of 3")
  expect_identical(step3_pubias_question_line("q3", stat), "Question 3 of 3")

  registry <- step3_pubias_reachable(small_industry = "no",
                                     registry_complete = "no", k = 4)
  expect_identical(step3_pubias_question_line("q4", registry),
                   "Question 3 of 3")
})

test_that("anything that is not a question on this path has no line", {
  full <- step3_pubias_reachable(small_industry = "no",
                                 registry_complete = "no", k = 14)
  # The terminal node is never numbered - it is the verdict, not a question.
  expect_null(step3_pubias_question_line("result", full))
  # q4 is on the branch these answers did not take.
  expect_null(step3_pubias_question_line("q4", full))
  expect_null(step3_pubias_question_line(NULL, full))
  expect_null(step3_pubias_question_line(NA_character_, full))
})

# --------------------------------------------------------------------------
# Accepting Egger's test lights the chart
# --------------------------------------------------------------------------
# "egger" is an ANSWER ("I looked, and I accept the automated test"), so the
# leaf it reaches is decided - by a p value the caller holds. It used to stop
# the trail dead at pma-pubias-node-q3, and a reviewer who accepted the test
# saw a chart that looked unfinished for the rest of the assessment.

test_that("the Egger sentinel reaches the leaf its p value chose", {
  base <- list(small_industry = "no", registry_complete = "no",
               funnel_asymmetry = STEP3_PUBIAS_USE_EGGER, k = 14)

  ids <- do.call(step3_pubias_flow_ids,
                 c(base, list(egger_asymmetric = TRUE)))
  expect_identical(ids[(length(ids) - 1L):length(ids)],
                   c("pma-pubias-edge-q3-yes", "pma-pubias-leaf-down1-q3"))

  ids <- do.call(step3_pubias_flow_ids,
                 c(base, list(egger_asymmetric = FALSE)))
  expect_identical(ids[(length(ids) - 1L):length(ids)],
                   c("pma-pubias-edge-q3-no", "pma-pubias-leaf-nodown-q3"))

  # No p value - the test was infeasible or failed - is not the same claim as
  # a symmetric funnel, so the trail stops at the node exactly as before.
  for (unknown in list(NULL, NA)) {
    ids <- do.call(step3_pubias_flow_ids,
                   c(base, list(egger_asymmetric = unknown)))
    expect_identical(ids[length(ids)], "pma-pubias-node-q3")
  }
  # Omitting the argument entirely is the same as not knowing it.
  ids <- do.call(step3_pubias_flow_ids, base)
  expect_identical(ids[length(ids)], "pma-pubias-node-q3")
})

test_that("a visual override ignores Egger's verdict entirely", {
  # The regression guard on the two literal answers: a reviewer who looked at
  # the funnel and overrode the test must not have the test's own p value put
  # back under them.
  base <- list(small_industry = "no", registry_complete = "no", k = 14)

  for (answer in c("yes", "no")) {
    leaf <- if (identical(answer, "yes")) {
      "pma-pubias-leaf-down1-q3"
    } else {
      "pma-pubias-leaf-nodown-q3"
    }
    for (egger in list(NULL, TRUE, FALSE, NA)) {
      ids <- do.call(step3_pubias_flow_ids,
                     c(base, list(funnel_asymmetry = answer,
                                  egger_asymmetric = egger)))
      expect_identical(ids[(length(ids) - 1L):length(ids)],
                       c(paste0("pma-pubias-edge-q3-", answer), leaf))
    }
  }

  # And an unanswered Q3 stays unanswered however the automated test came out:
  # the sentinel is what says the reviewer looked and accepted it.
  ids <- do.call(step3_pubias_flow_ids,
                 c(base, list(egger_asymmetric = TRUE)))
  expect_identical(ids[length(ids)], "pma-pubias-node-q3")
})

test_that("the computed k gate lights its node AND the edge it chose", {
  # The one node the reviewer is never asked about. Lighting the node alone
  # would show the chart stopping at a question nobody answered; the edge is
  # what says which way the study count sent them.
  ids <- step3_pubias_flow_ids(small_industry = "no",
                               registry_complete = "no", k = 14)
  expect_true(all(c("pma-pubias-node-q2", "pma-pubias-edge-q2-yes") %in% ids))
  expect_identical(ids[length(ids)], "pma-pubias-node-q3")

  ids <- step3_pubias_flow_ids(small_industry = "no",
                               registry_complete = "no", k = 4)
  expect_true(all(c("pma-pubias-node-q2", "pma-pubias-edge-q2-no") %in% ids))
  expect_identical(ids[length(ids)], "pma-pubias-node-q4")
})

test_that("every id the chart lights is an id the figure draws", {
  # The drift guard. inst/figures/pubias.svg is redrawn by
  # data-raw/build_figures.R, and 0.5.1 deleted two leaves from it; the
  # package suite checks the ids the ASSESSOR emits (test-flowchart-nodes.R)
  # and would not have seen these.
  skip_if(is.na(.pubias_svg), "no pubias.svg in this checkout")
  svg <- paste(readLines(.pubias_svg, warn = FALSE), collapse = "\n")
  drawn <- unique(gsub('^id="|"$', "",
                       regmatches(svg, gregexpr('id="pma-pubias-[^"]+"',
                                                svg))[[1L]]))
  expect_true(length(drawn) > 0L)

  # Every combination of answers the wizard can be in, not a sample: the whole
  # input space is five small factors.
  grid <- expand.grid(
    small_industry    = c("", "no", "yes"),
    registry_complete = c("", "no", "yes"),
    funnel_asymmetry  = c("", "no", "yes", STEP3_PUBIAS_USE_EGGER),
    unpublished       = c("", "no", "yes"),
    k                 = c(0, 4, 10, 14),
    stringsAsFactors  = FALSE)
  lit <- unique(unlist(lapply(seq_len(nrow(grid)), function(i) {
    do.call(step3_pubias_flow_ids, as.list(grid[i, ]))
  })))

  expect_true(length(lit) > 0L)
  expect_identical(setdiff(lit, drawn), character(0))
})

# --------------------------------------------------------------------------
# The trim-and-fill exaggeration diagnostic under the funnel
# --------------------------------------------------------------------------
# The arithmetic and the wording are the package's (R/pubias_trimfill.R) and
# are tested there. What only the app can go wrong about is the vendoring:
# step3_grade.R calls those two functions by name, and a staged bundle that
# does not carry the file fails at runtime, in production, on a tab nobody
# opens until a reviewer needs it.
test_that("the staged bundle carries the trim-and-fill diagnostic", {
  cands <- c(file.path(PMA_APP_ROOT, "R", "_pmatools", "pubias_trimfill.R"),
             file.path(dirname(PMA_APP_ROOT), "R", "pubias_trimfill.R"))
  hit <- Filter(file.exists, cands)
  skip_if(!length(hit), "no pmatools sources in this checkout")

  defined <- ls(envir = local({
    e <- new.env(parent = globalenv())
    sys.source(hit[[1L]], envir = e)
    e
  }), all.names = TRUE)
  expect_true(all(c(".pubias_trimfill_inflation", ".pubias_trimfill_line")
                  %in% defined))
})
