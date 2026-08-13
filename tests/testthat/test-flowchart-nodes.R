# test-flowchart-nodes.R - the contract between the four decision flowcharts
# and the assessors that claim to walk them.
#
# The figures live in inst/figures/ and are drawn by data-raw/build_figures.R.
# Nothing in R/ reads them, so nothing in R/ would notice if a node were
# renamed, a branch added without being drawn, or the man/ copy left stale.
# These tests are the thing that notices.
#
# Three invariants, one per failure mode:
#   (a) every id in an assessor's node vocabulary is actually drawn;
#   (b) every id an assessor emits in its flow_path fact is in that
#       vocabulary (so adding a branch means extending the vocabulary, which
#       (a) then forces you to draw);
#   (c) inst/figures/ and man/figures/ are byte-identical, because Rd's
#       \figure{} resolves only against man/figures/ while the Shiny bundle
#       stages only inst/.
#
# Deliberately no XML dependency: the ids are matched with a regex, which is
# also what the browser-side highlighter effectively does.

library(testthat)
library(meta)

skip_if_not_installed("meta")

quiet_grade <- function(...) suppressWarnings(grade_meta(...))

# testthat runs with the working directory at tests/testthat, so the package
# root is two levels up in a source checkout. An installed package has the
# figures under the install directory instead; try both and skip if neither
# is present rather than failing a check run that has no inst/ to read.
.fig_roots <- function() {
  c(file.path("..", "..", "inst", "figures"),
    system.file("figures", package = "pmatools"))
}

.fig_dir <- local({
  hit <- Filter(function(d) nzchar(d) && dir.exists(d), .fig_roots())
  if (length(hit)) hit[[1L]] else NA_character_
})

.man_fig_dir <- file.path("..", "..", "man", "figures")

FIGKEYS <- c(rob = "rob", incon = "incon", impre = "impre", pubias = "pubias")

# Every id="pma-..." in the file, in order of appearance.
.svg_ids <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  m <- gregexpr('id="(pma-[^"]+)"', txt, perl = TRUE)[[1L]]
  if (m[1L] == -1L) return(character(0))
  hits <- regmatches(txt, list(m))[[1L]]
  sub('^id="(.*)"$', "\\1", hits)
}

.vocab <- list(
  rob    = pmatools:::.ROB_FIG2_NODE_IDS,
  incon  = pmatools:::.INCON_FIG2_NODE_IDS,
  impre  = pmatools:::.IMPRE_FIG4_NODE_IDS,
  pubias = pmatools:::.PUBIAS_FIG5_NODE_IDS
)

# --------------------------------------------------------------------------
# (a) The vocabulary is drawn
# --------------------------------------------------------------------------

test_that("every figure exists and carries ids", {
  skip_if(is.na(.fig_dir), "inst/figures not available in this layout")
  for (key in FIGKEYS) {
    path <- file.path(.fig_dir, paste0(key, ".svg"))
    expect_true(file.exists(path), info = key)
    expect_gt(length(.svg_ids(path)), 0L)
    # The bundle carries these twice (tarball and shinyapps.io), so an
    # accidental blow-up in size is worth catching here rather than at deploy.
    expect_lt(file.size(path), 40 * 1024)
  }
})

test_that("each assessor's node vocabulary is a subset of the figure's ids", {
  skip_if(is.na(.fig_dir), "inst/figures not available in this layout")
  for (key in names(.vocab)) {
    drawn <- .svg_ids(file.path(.fig_dir, paste0(key, ".svg")))
    missing <- setdiff(.vocab[[key]], drawn)
    expect_identical(
      missing, character(0),
      info = paste0(key, ".svg does not draw: ",
                    paste(missing, collapse = ", "))
    )
  }
})

test_that("node ids are unique within a figure and namespaced to it", {
  skip_if(is.na(.fig_dir), "inst/figures not available in this layout")
  for (key in FIGKEYS) {
    ids <- .svg_ids(file.path(.fig_dir, paste0(key, ".svg")))
    expect_identical(anyDuplicated(ids), 0L, info = key)
    # Duplicated ids across figures would break the browser-side highlighter
    # once two charts share a page.
    expect_true(all(startsWith(ids, paste0("pma-", key, "-"))), info = key)
  }
})

test_that("the vocabulary lists no id twice", {
  for (key in names(.vocab)) {
    expect_identical(anyDuplicated(.vocab[[key]]), 0L, info = key)
  }
})

# --------------------------------------------------------------------------
# (b) Nothing is emitted that is not in the vocabulary
# --------------------------------------------------------------------------

.flow_path <- function(g, domain) {
  f <- domain_facts(g, domain)
  if (is.null(f)) return(NULL)
  v <- f$value[f$key == "flow_path"]
  if (length(v) != 1L) return(NULL)
  strsplit(v, " ", fixed = TRUE)[[1L]]
}

# A spread of inputs wide enough to reach every branch that the automated
# assessors can reach at all. Each entry returns a rated object.
mk_binary <- function() {
  meta::metabin(
    event.e = c(20, 18, 30, 35, 25), n.e = c(100, 100, 200, 200, 150),
    event.c = c(22, 20, 60, 65, 50), n.c = c(100, 100, 200, 200, 150),
    studlab = paste0("S", 1:5), sm = "RR"
  )
}

# Ten studies, so Q2 of the publication-bias flowchart takes the "yes" edge
# and Egger's test actually runs.
mk_binary_k10 <- function() {
  set.seed(11)
  meta::metabin(
    event.e = c(20, 18, 30, 35, 25, 22, 19, 28, 31, 24),
    n.e     = c(100, 100, 200, 200, 150, 110, 105, 190, 205, 160),
    event.c = c(22, 20, 40, 45, 30, 25, 21, 33, 38, 29),
    n.c     = c(100, 100, 200, 200, 150, 110, 105, 190, 205, 160),
    studlab = paste0("S", 1:10), sm = "RR"
  )
}

# Point estimates deliberately straddling the null, so the inconsistency zone
# tally reaches the opposite-sides branch.
mk_opposite <- function() {
  meta::metabin(
    event.e = c(10, 40, 12, 38, 11), n.e = c(100, 100, 100, 100, 100),
    event.c = c(40, 10, 38, 12, 39), n.c = c(100, 100, 100, 100, 100),
    studlab = paste0("O", 1:5), sm = "RR"
  )
}

.cases <- function() {
  list(
    rob_dominated = quiet_grade(
      mk_binary(),
      rob = c("no", "no", "serious", "serious", "serious"),
      small_values = "desirable",
      threshold = 1.10, threshold_scale = "ratio"),
    rob_none_high = quiet_grade(
      mk_binary(), rob = rep("no", 5),
      small_values = "desirable",
      threshold = 1.10, threshold_scale = "ratio"),
    rob_not_dominated = quiet_grade(
      mk_binary(),
      rob = c("serious", "no", "no", "no", "no"),
      small_values = "desirable",
      threshold = 1.10, threshold_scale = "ratio"),
    incon_opposite = quiet_grade(
      mk_opposite(), rob = rep("no", 5),
      small_values = "desirable",
      threshold = 1.10, threshold_scale = "ratio"),
    incon_opposite_explained = quiet_grade(
      mk_opposite(), rob = rep("no", 5),
      small_values = "desirable",
      inconsistency_subgroup_explained = "yes",
      threshold = 1.10, threshold_scale = "ratio"),
    pubias_k10 = quiet_grade(
      mk_binary_k10(), rob = rep("no", 10),
      small_values = "desirable",
      threshold = 1.10, threshold_scale = "ratio"),
    pubias_unpublished = quiet_grade(
      mk_binary(), rob = rep("no", 5),
      small_values = "desirable",
      pubias_unpublished = "yes",
      threshold = 1.10, threshold_scale = "ratio"),
    pubias_registry = quiet_grade(
      mk_binary(), rob = rep("no", 5),
      small_values = "desirable",
      pubias_registry_complete = "yes",
      threshold = 1.10, threshold_scale = "ratio"),
    pubias_small_industry = quiet_grade(
      mk_binary(), rob = rep("no", 5),
      small_values = "desirable",
      pubias_small_industry = "yes",
      threshold = 1.10, threshold_scale = "ratio"),
    null_threshold = quiet_grade(
      mk_binary(), rob = rep("no", 5),
      small_values = "desirable",
      threshold_type = "null")
  )
}

DOMAIN_FIG <- c("Risk of bias"     = "rob",
                "Inconsistency"    = "incon",
                "Imprecision"      = "impre",
                "Publication bias" = "pubias")

test_that("every emitted flow_path stays inside its figure's vocabulary", {
  cases <- .cases()
  seen  <- stats::setNames(vector("list", length(DOMAIN_FIG)),
                           names(DOMAIN_FIG))
  for (nm in names(cases)) {
    g <- cases[[nm]]
    for (domain in names(DOMAIN_FIG)) {
      ids <- .flow_path(g, domain)
      if (is.null(ids)) next
      key   <- DOMAIN_FIG[[domain]]
      stray <- setdiff(ids, .vocab[[key]])
      expect_identical(
        stray, character(0),
        info = paste0(nm, " / ", domain, " emitted unknown ids: ",
                      paste(stray, collapse = ", "))
      )
      seen[[domain]] <- union(seen[[domain]], ids)
    }
  }
  # A flow_path that is never emitted at all would make the assertion above
  # vacuously true, so pin that all four domains reached it.
  for (domain in names(DOMAIN_FIG)) {
    expect_gt(length(seen[[domain]]), 0L)
  }
})

test_that("all four flowcharted domains record a flow_path on a plain rating", {
  g <- .cases()$rob_dominated
  for (domain in names(DOMAIN_FIG)) {
    ids <- .flow_path(g, domain)
    expect_true(!is.null(ids) && length(ids) >= 3L,
                info = paste(domain, "recorded no usable flow_path"))
  }
})

test_that("the flow_path fact is not scalar-numeric", {
  f <- domain_facts(.cases()$rob_dominated, "Risk of bias")
  expect_true(is.na(f$numeric[f$key == "flow_path"]))
})

test_that("Indirectness records no facts, and that is on purpose", {
  # Core GRADE 5 Table 2 is a gradient, not a flowchart; the subdomain table
  # is the structured record instead. See ?grade_flowcharts.
  expect_null(domain_facts(.cases()$rob_dominated, "Indirectness"))
})

test_that("the risk-of-bias path names the rule the direction check applied", {
  g <- .cases()$rob_dominated
  ids <- .flow_path(g, "Risk of bias")
  f   <- domain_facts(g, "Risk of bias")
  rule <- f$numeric[f$key == "fig2_branch"]
  expect_true(any(grepl("^pma-rob-leaf-rule", ids)))
  if (!is.na(rule)) {
    expect_true(paste0("pma-rob-leaf-rule", rule) %in% ids)
  }
})

test_that("publication bias records k, and Egger's p when it ran", {
  f10 <- domain_facts(.cases()$pubias_k10, "Publication bias")
  expect_true("k" %in% f10$key)
  expect_identical(f10$numeric[f10$key == "k"], 10)
  expect_true("egger_p" %in% f10$key)

  # k < 10 takes the registry branch, where there is no test to report.
  f5 <- domain_facts(.cases()$pubias_unpublished, "Publication bias")
  expect_true("k" %in% f5$key)
  expect_false("egger_p" %in% f5$key)
})

test_that("a scalar override records no flow_path: the flowchart did not run", {
  g <- quiet_grade(
    mk_binary(),
    rob = "serious",
    rob_rationale = "RoB2 consensus across all five trials",
    inconsistency = "some_concerns",
    inconsistency_rationale = "Forest plot shows two clusters",
    small_values = "desirable",
    threshold = 1.10, threshold_scale = "ratio")
  expect_null(.flow_path(g, "Risk of bias"))
  expect_null(.flow_path(g, "Inconsistency"))
})

# --------------------------------------------------------------------------
# (b, continued) Every branch, not just the reachable-from-grade_meta() ones
#
# The cases above go in the front door and therefore cannot reach a leaf that
# needs, say, an Egger test that fails to run. Invariant (b) is only worth
# having if it covers the whole vocabulary, so the rest is driven at the
# assessor's own level. What is asserted is the same thing: nothing outside
# the vocabulary is ever emitted, and between the two halves the whole
# vocabulary is emitted at least once.
# --------------------------------------------------------------------------

# Two-arm generic-inverse-variance fixture, so the risk-of-bias zone
# arithmetic can be aimed at a specific rule rather than discovered.
mk_gen <- function(te, se = rep(0.1, length(te))) {
  suppressWarnings(meta::metagen(
    TE = te, seTE = se, studlab = paste0("G", seq_along(te)), sm = "RR"))
}

.rob_flow <- function(rob_vec, te, threshold, ...) {
  row <- pmatools:::.flowchart_rob(
    rob_vec, mk_gen(te),
    small_values = "desirable", threshold_internal = threshold, ...)
  f <- attr(row, "facts")
  strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
}

test_that("the risk-of-bias vocabulary is fully reachable", {
  seen <- character(0)

  # Rule 1: every estimate inside the trivial zone.
  seen <- union(seen, .rob_flow(c("no", "serious", "serious", "serious"),
                                c(0.01, 0.02, 0.01, 0.02), threshold = 0.5))
  # Rule 2 / rule 3: same non-trivial zone, small vs large bias-favouring
  # change. small_values = "desirable" makes a LOWER TE_all the inflated one.
  seen <- union(seen, .rob_flow(c("no", "serious", "serious", "serious"),
                                c(-1.00, -1.02, -1.01, -1.03),
                                threshold = 0.5))
  seen <- union(seen, .rob_flow(c("no", "serious", "serious", "serious"),
                                c(-0.60, -3.00, -3.10, -2.90),
                                threshold = 0.5))
  # Rules 4 and 5: the two estimates land in different zones, once on the
  # same side of the null and once across it.
  seen <- union(seen, .rob_flow(c("no", "serious", "serious", "serious"),
                                c(-0.20, -3.00, -3.10, -2.90),
                                threshold = 0.5))
  seen <- union(seen, .rob_flow(c("no", "serious", "serious", "serious"),
                                c(2.00, -3.00, -3.10, -2.90),
                                threshold = 0.5))
  # Direction not assessable: every study is high risk, so there is no
  # comparator estimate to check the direction against.
  seen <- union(seen, .rob_flow(rep("serious", 4),
                                c(-1.0, -1.1, -0.9, -1.05), threshold = 0.5))
  # No high-risk study at all.
  seen <- union(seen, .rob_flow(rep("no", 4),
                                c(-1.0, -1.1, -0.9, -1.05), threshold = 0.5))
  # Not dominated, similar magnitudes -> analyse all studies.
  seen <- union(seen, .rob_flow(c("serious", "no", "no", "no"),
                                c(-1.00, -1.01, -0.99, -1.02),
                                threshold = 0.5))
  # Not dominated, substantially different -> analyse low risk only.
  seen <- union(seen, .rob_flow(c("serious", "no", "no", "no"),
                                c(-4.00, -0.10, -0.11, -0.09),
                                threshold = 0.5))
  # Not dominated and the comparison is not assessable. This one cannot be
  # built with metagen(): it needs low-risk studies that exist and carry
  # weight but have no usable standard error, which metagen() would drop.
  # .flowchart_rob() reads a handful of slots, so a plain list is enough.
  fake <- list(
    TE = c(-1.00, -0.9, -1.1, -1.0), seTE = c(0.1, NA, NA, NA),
    w.random = c(1, 1, 1, 1), random = TRUE,
    TE.random = -1.0, seTE.random = 0.2, sm = "RR")
  fake_row <- pmatools:::.flowchart_rob(
    c("serious", "no", "no", "no"), fake,
    small_values = "desirable", threshold_internal = 0.5)
  fake_f <- attr(fake_row, "facts")
  seen <- union(seen, strsplit(
    fake_f$value[fake_f$key == "flow_path"], " ", fixed = TRUE)[[1L]])

  expect_identical(setdiff(seen, .vocab$rob), character(0))
  expect_identical(setdiff(.vocab$rob, seen), character(0))
})

test_that("the imprecision vocabulary is fully reachable", {
  defaults <- list(
    crosses_threshold = FALSE, crosses_both_thresholds = FALSE,
    large = list(large = FALSE, note = "moderate effect"),
    is_binary = TRUE, ois_met = NA, ois_pct = NA_real_,
    n_total = NA_real_, ci_ratio = NA_real_, ci_ratio_cut = NA_real_)
  cl <- function(...) {
    do.call(pmatools:::.classify_imprecision,
            utils::modifyList(defaults, list(...)))$flow
  }
  big <- list(large = TRUE, note = "implausibly large")

  seen <- Reduce(union, list(
    # CI crosses one threshold, then both.
    cl(crosses_threshold = TRUE),
    cl(crosses_threshold = TRUE, crosses_both_thresholds = TRUE),
    # Does not cross, moderate effect.
    cl(),
    # Large effect, binary, CI ratio over the cut-off.
    cl(large = big, ci_ratio = 4, ci_ratio_cut = 3),
    # Large effect, continuous, 800 participants.
    cl(large = big, is_binary = FALSE, n_total = 900),
    # Large effect, OIS not computable / met / short / far short.
    cl(large = big, ois_met = NA),
    cl(large = big, ois_met = TRUE),
    cl(large = big, ois_met = FALSE, ois_pct = 0.7),
    cl(large = big, is_binary = FALSE, n_total = 100,
       ois_met = FALSE, ois_pct = 0.1)
  ))

  expect_identical(setdiff(seen, .vocab$impre), character(0))
  expect_identical(setdiff(.vocab$impre, seen), character(0))
})

test_that("the publication-bias vocabulary is fully reachable", {
  path_of <- function(row) {
    f <- attr(row, "facts")
    strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
  }
  q1 <- "Q1: Not dominated by small industry-sponsored studies. "

  # Q4 branch: documented, not documented, and nobody asked.
  seen <- Reduce(union, list(
    path_of(pmatools:::.pubias_registry(5, "yes", q1)),
    path_of(pmatools:::.pubias_registry(5, "no", q1)),
    path_of(suppressWarnings(pmatools:::.pubias_registry(5, NULL, q1)))
  ))

  # Q3 branch, manual: the reviewer's visual judgment either way.
  for (ans in c("yes", "no")) {
    seen <- union(seen, path_of(pmatools:::.pubias_statistical(
      meta_obj = mk_binary_k10(), k = 10, pubias_funnel_asymmetry = ans,
      q1_note = q1, rationale = "Contour-enhanced funnel plot inspected")))
  }

  # Q3 branch, automatic: a real Egger test, then one that cannot run.
  seen <- union(seen, path_of(suppressWarnings(pmatools:::.pubias_statistical(
    meta_obj = mk_binary_k10(), k = 10, pubias_funnel_asymmetry = NULL,
    q1_note = q1))))
  seen <- union(seen, path_of(suppressWarnings(pmatools:::.pubias_statistical(
    meta_obj = list(TE = 1, seTE = 1), k = 10,
    pubias_funnel_asymmetry = NULL, q1_note = q1))))

  # The two entry nodes, from the front door.
  seen <- union(seen, .flow_path(.cases()$pubias_small_industry,
                                 "Publication bias"))
  seen <- union(seen, .flow_path(.cases()$pubias_registry,
                                 "Publication bias"))
  # Whichever side of p = 0.05 the fixture happens to fall on, the other leaf
  # is still drawn; assert the pair is covered between the manual and
  # automatic runs above rather than pinning the fixture's p value.
  expect_identical(setdiff(seen, .vocab$pubias), character(0))
  expect_identical(setdiff(.vocab$pubias, seen), character(0))
})

test_that("the inconsistency vocabulary is fully reachable", {
  # The manual flowchart answers every node directly, which is the only way
  # to reach Step 2's majority leaf without hand-building a zone tally.
  mp <- function(...) {
    row <- assess_inconsistency(mk_binary(), ...)
    f <- attr(row, "facts")
    strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
  }
  seen <- Reduce(union, list(
    mp(inconsistency_ci_diff = "no"),
    mp(inconsistency_ci_diff = "yes",
       inconsistency_threshold_side = "majority_one_side"),
    mp(inconsistency_ci_diff = "yes",
       inconsistency_threshold_side = "opposite_sides",
       inconsistency_subgroup_explained = "yes"),
    mp(inconsistency_ci_diff = "yes",
       inconsistency_threshold_side = "opposite_sides",
       inconsistency_subgroup_explained = "no"),
    # Automated: heterogeneous but with no zone holding a majority and
    # neither side substantial -- the "scattered" leaf.
    {
      row <- assess_inconsistency(
        mk_gen(c(-2.0, -0.9, 0.05, 1.1, 2.2, 0.02),
               se = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)),
        threshold_chosen = 1.0)
      f <- attr(row, "facts")
      strsplit(f$value[f$key == "flow_path"], " ", fixed = TRUE)[[1L]]
    },
    .flow_path(.cases()$incon_opposite, "Inconsistency"),
    .flow_path(.cases()$incon_opposite_explained, "Inconsistency")
  ))

  expect_identical(setdiff(seen, .vocab$incon), character(0))
  expect_identical(setdiff(.vocab$incon, seen), character(0))
})

# --------------------------------------------------------------------------
# (c) The two copies agree
# --------------------------------------------------------------------------

test_that("inst/figures and man/figures are byte-identical", {
  skip_if(is.na(.fig_dir), "inst/figures not available in this layout")
  skip_if_not(dir.exists(.man_fig_dir),
              "man/figures not available in this layout")
  for (key in FIGKEYS) {
    a <- readBin(file.path(.fig_dir, paste0(key, ".svg")), "raw",
                 n = 200000)
    b <- readBin(file.path(.man_fig_dir, paste0(key, ".svg")), "raw",
                 n = 200000)
    expect_identical(
      a, b,
      info = paste0(key, ".svg differs between inst/ and man/; re-run ",
                    "data-raw/build_figures.R")
    )
  }
})
