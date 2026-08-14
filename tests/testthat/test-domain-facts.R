# test-domain-facts.R — structured facts behind the Risk of bias,
# Inconsistency and Imprecision judgments: the container on the rated object,
# the domain_facts() accessor, and the numbered footnotes the SoF tables and
# the Evidence Profile render them as.
#
# The governing invariant is that facts are recorded ALONGSIDE the prose in
# domain_assessments$notes, never instead of it: several assertions below pin
# the note fragments that existed before this feature.

library(testthat)
library(meta)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

quiet_grade <- function(...) suppressWarnings(grade_meta(...))

.footer_text <- function(ft) paste(unlist(ft$footer$dataset), collapse = " ")
.body_col    <- function(ft, i) as.character(ft$body$dataset[[i]])

# Binary fixture that rates down for BOTH Risk of bias and Imprecision:
# the three big studies are at high risk of bias and show a much larger effect
# than the two small low-risk ones (Core GRADE 4 Fig 2, dominated branch,
# direction-of-bias rule 3), and the participant count falls short of the OIS.
# Inconsistency records its statistics but does not rate down, which is what
# makes the "no footnote for a domain that did not rate down" case observable.
mk_binary <- function() {
  meta::metabin(
    event.e = c(20, 18, 30, 35, 25), n.e = c(100, 100, 200, 200, 150),
    event.c = c(22, 20, 60, 65, 50), n.c = c(100, 100, 200, 200, 150),
    studlab = paste0("S", 1:5), sm = "RR"
  )
}

make_facts <- function() {
  quiet_grade(mk_binary(),
              rob             = c("no", "no", "very_serious", "very_serious", "very_serious"),
              small_values    = "desirable",
              threshold       = 1.10, threshold_scale = "ratio",
              outcome_name    = "Mortality",
              indirectness    = "no",
              indirectness_rationale = "PICO matches the review question",
              pubias_unpublished     = "no")
}

# A body of evidence with nothing to rate down: same studies, all at low risk
# of bias, rated against the null, with an OIS the 1500 participants clear so
# Fig 4's large-effect branch does not rate Imprecision down either.
make_clean <- function() {
  quiet_grade(mk_binary(),
              small_values = "desirable",
              rob            = "no",
              rob_rationale  = "Consensus RoB2: all domains low risk",
              threshold_type = "null",
              ois_n          = 1000,
              outcome_name   = "Mortality",
              indirectness   = "no",
              indirectness_rationale = "PICO matches the review question",
              pubias_unpublished     = "no")
}

# The Core GRADE 4 refit leaf (mirrors test-sof_bmj.R): exact inverse-variance
# weights via tau.preset = 0, one dominant high-risk study.
mk_gen <- function(te, w, studlab = paste0("S", seq_along(te)), sm = "RR") {
  meta::metagen(TE = te, seTE = sqrt(1 / w), studlab = studlab, sm = sm,
                tau.preset = 0)
}

make_refit <- function() {
  quiet_grade(mk_gen(te = c(1.2, 0.02, 0.02, 0.02),
                     w  = c(400, 400 / 3, 400 / 3, 400 / 3),
                     studlab = c("High-1", "Low-1", "Low-2", "Low-3")),
              rob             = c("very_serious", "no", "no", "no"),
              small_values    = "undesirable",
              threshold       = 1.05, threshold_scale = "ratio",
              outcome_name    = "Refitted outcome",
              indirectness    = "no",
              indirectness_rationale = "PICO matches the review question",
              pubias_unpublished     = "no")
}

# --- 1. the container ------------------------------------------------------

test_that("grade_meta() records facts for the four flowcharted domains", {
  g <- make_facts()

  expect_type(g$domain_facts, "list")
  expect_setequal(names(g$domain_facts),
                  c("Risk of bias", "Inconsistency", "Imprecision",
                    "Publication bias"))

  for (dm in names(g$domain_facts)) {
    f <- g$domain_facts[[dm]]
    expect_s3_class(f, "tbl_df")
    expect_identical(names(f), c("key", "label", "value", "numeric"))
    expect_type(f$key, "character")
    expect_type(f$label, "character")
    expect_type(f$value, "character")
    expect_type(f$numeric, "double")
    expect_gt(nrow(f), 0L)
  }

  expect_true(all(c("high_rob_studies", "high_rob_weight_share",
                    "estimate_shift", "fig2_branch") %in%
                    g$domain_facts[["Risk of bias"]]$key))
  expect_true(all(c("i2", "tau2", "q_pvalue", "zone_counts", "zone_decision") %in%
                    g$domain_facts[["Inconsistency"]]$key))
  expect_true(all(c("confidence_interval", "crosses_null", "threshold_position",
                    "ois", "fig4_path", "ois_used") %in%
                    g$domain_facts[["Imprecision"]]$key))
  expect_true("k" %in% g$domain_facts[["Publication bias"]]$key)
  # All four also record the route they took through their figure; see
  # test-flowchart-nodes.R for the contract that keeps the ids honest.
  for (dm in names(g$domain_facts)) {
    expect_true("flow_path" %in% g$domain_facts[[dm]]$key, info = dm)
  }
})

test_that("Indirectness records no facts, and that is deliberate", {
  # Core GRADE 5 Table 2 grades the four PICO elements on a gradient rather
  # than routing them through a flowchart, so there is no branch to record.
  # When the reviewer answers the subdomain questions the structured record
  # is x$indirectness_subdomains instead; this fixture supplies a scalar, so
  # there is neither, which is also correct.
  g <- make_facts()
  expect_false("Indirectness" %in% names(g$domain_facts))
})

# --- 2. the numeric column really carries numbers --------------------------

test_that("the numeric column carries the raw statistics, not the display text", {
  g <- make_facts()

  incon <- g$domain_facts[["Inconsistency"]]
  expect_equal(incon$numeric[incon$key == "i2"], g$meta$I2 * 100,
               tolerance = 1e-8)
  expect_equal(incon$numeric[incon$key == "tau2"], g$meta$tau2,
               tolerance = 1e-8)
  expect_equal(incon$numeric[incon$key == "q_pvalue"], g$meta$pval.Q,
               tolerance = 1e-8)
  # k, so a caller can turn the zone counts back into shares.
  expect_equal(incon$numeric[incon$key == "zone_counts"], g$meta$k)

  impre <- g$domain_facts[["Imprecision"]]
  ois_val <- impre$value[impre$key == "ois"]
  nums    <- as.numeric(regmatches(ois_val,
                                   gregexpr("[0-9]+", ois_val))[[1]])
  expect_equal(impre$numeric[impre$key == "ois"], nums[1] / nums[2],
               tolerance = 1e-3)

  rob <- g$domain_facts[["Risk of bias"]]
  expect_equal(rob$numeric[rob$key == "high_rob_studies"], 3)
  expect_gt(rob$numeric[rob$key == "high_rob_weight_share"], 0.55)
  # rule 3 of the direction-of-bias check drove this downgrade.
  expect_equal(rob$numeric[rob$key == "fig2_branch"], 3)
})

# --- 3. the prose notes are untouched --------------------------------------

test_that("recording facts leaves domain_assessments$notes unchanged", {
  g <- make_facts()
  note <- function(dm) g$domain_assessments$notes[g$domain_assessments$domain == dm]

  expect_match(note("Imprecision"), "Fig 4 path: ", fixed = TRUE)
  expect_match(note("Risk of bias"), "High-RoB studies:", fixed = TRUE)
  expect_match(note("Inconsistency"), "I2 = ", fixed = TRUE)
})

# --- 4. the accessor -------------------------------------------------------

test_that("domain_facts() returns the whole list, one domain, or NULL", {
  g <- make_facts()

  expect_identical(domain_facts(g), g$domain_facts)
  expect_identical(domain_facts(g, "Imprecision"), g$domain_facts[["Imprecision"]])
  expect_identical(domain_facts(g, "Publication bias"),
                   g$domain_facts[["Publication bias"]])
  # A valid domain name that recorded nothing. Indirectness is now the only
  # one; see the note on the gradient above.
  expect_null(domain_facts(g, "Indirectness"))
})

test_that("domain_facts() aborts on a bogus domain and on a non-pmatools input", {
  g <- make_facts()
  expect_error(domain_facts(g, "Imprecison"), "not a GRADE domain name")
  expect_error(domain_facts(g, "Imprecison"), "Publication bias")
  expect_error(domain_facts(g, c("Imprecision", "Inconsistency")),
               "single GRADE domain name")
  expect_error(domain_facts(list(certainty = "Low")), "pmatools object")
})

# --- 5. single-outcome rendering, both styles ------------------------------

test_that("sof_table(style = 'gradepro') numbers the domain facts and marks the cell", {
  g  <- make_facts()
  ft <- sof_table(g, style = "gradepro")

  txt <- .footer_text(ft)
  expect_match(txt, "[1] Risk of bias. High risk of bias studies: 3 of 5",
               fixed = TRUE)
  expect_match(txt, "[2] Imprecision. 95% confidence interval:", fixed = TRUE)
  # The Fig 4 path is carried as its own fact, without the notes prefix.
  expect_match(txt, "Core GRADE 2 Fig 4 path: CI does not cross", fixed = TRUE)

  # Markers concatenate onto the certainty cell after the symbol.
  expect_match(.body_col(ft, 6), " [1][2]", fixed = TRUE)
})

test_that("sof_table(style = 'bmj') puts the marker on the domain name", {
  g  <- make_facts()
  ft <- sof_table(g, style = "bmj")

  expect_identical(
    .body_col(ft, 7),
    paste0(g$certainty, "\nDue to serious risk of bias [1] and imprecision [2]")
  )

  txt <- .footer_text(ft)
  expect_match(txt, "[1] Risk of bias. High risk of bias studies:", fixed = TRUE)
  expect_match(txt, "[2] Imprecision. 95% confidence interval:", fixed = TRUE)
})

test_that("the analysis-set and publication-bias footnotes stay unnumbered", {
  ft  <- sof_table(make_refit(), style = "bmj")
  txt <- .footer_text(ft)
  expect_match(txt, "Effect estimate restricted to low risk of bias studies",
               fixed = TRUE)
  expect_no_match(txt, "[1] Effect estimate restricted", fixed = TRUE)
})

# --- 6. multi-outcome numbering --------------------------------------------

test_that("grade_table() continues the analysis-set register into the fact notes", {
  g1 <- make_facts()
  g2 <- make_refit()
  outcomes <- list("Mortality" = g1, "Refitted outcome" = g2)

  for (style in c("gradepro", "bmj")) {
    ft  <- grade_table(outcomes, style = style)
    txt <- .footer_text(ft)

    # [1] is the per-outcome analysis-set note, unchanged.
    expect_match(txt, "[1] Effect estimate restricted to low risk of bias studies",
                 fixed = TRUE)
    # Domain facts continue from [2], and name the outcome they belong to.
    expect_match(txt, "[2] Risk of bias (Mortality). High risk of bias studies:",
                 fixed = TRUE)
    expect_match(txt, "[3] Imprecision (Mortality). 95% confidence interval:",
                 fixed = TRUE)
    expect_match(txt, "[4] Imprecision (Refitted outcome). 95% confidence interval:",
                 fixed = TRUE)
  }

  cells_gp <- .body_col(grade_table(outcomes, style = "gradepro"), 6)
  expect_match(cells_gp[1], " [2][3]", fixed = TRUE)
  expect_match(cells_gp[2], " [4]", fixed = TRUE)

  cells_bmj <- .body_col(grade_table(outcomes, style = "bmj"), 7)
  expect_match(cells_bmj[1], "risk of bias [2] and imprecision [3]", fixed = TRUE)
  expect_match(cells_bmj[2], "imprecision [4]", fixed = TRUE)
})

# --- 7. evidence_profile() -------------------------------------------------

test_that("evidence_profile() footnotes the structured facts, not the first sentence", {
  g  <- make_facts()
  ft <- evidence_profile(g)

  txt <- .footer_text(ft)
  expect_match(txt, "[1] High risk of bias studies: 3 of 5", fixed = TRUE)
  expect_match(txt, "[2] 95% confidence interval: RR [", fixed = TRUE)
  # The old behaviour was the first " | "-separated sentence of the notes.
  expect_no_match(txt, "[1] High-RoB studies:", fixed = TRUE)
  # The domain name is left off: the marker already sits in the domain column.
  expect_no_match(txt, "[1] Risk of bias. High risk of bias studies",
                  fixed = TRUE)

  expect_match(.body_col(ft, 4), "serious [1]", fixed = TRUE)
  expect_match(.body_col(ft, 7), "serious [2]", fixed = TRUE)
})

test_that("evidence_profile() falls back to the notes for a domain without facts", {
  g <- quiet_grade(mk_binary(),
                   small_values = "desirable",
                   rob            = "no",
                   rob_rationale  = "Consensus RoB2: all domains low risk",
                   indirectness   = "very_serious",
                   indirectness_rationale = "Surrogate outcome only",
                   threshold      = 1.10, threshold_scale = "ratio",
                   outcome_name   = "Mortality",
                   pubias_unpublished = "no")
  txt <- .footer_text(evidence_profile(g))
  expect_match(txt, "Manual override (very_serious): Surrogate outcome only",
               fixed = TRUE)
})

# --- 8. a domain that did not rate down is not footnoted -------------------

test_that("a domain with facts that did not rate down gets no footnote or marker", {
  g <- make_facts()
  # Inconsistency recorded its statistics but did not pull the rating down.
  expect_true("Inconsistency" %in% names(g$domain_facts))
  expect_equal(
    g$domain_assessments$downgrade[g$domain_assessments$domain == "Inconsistency"],
    0
  )
  expect_false("Inconsistency" %in% .rated_down_fact_domains(g))

  txt <- .footer_text(sof_table(g, style = "gradepro"))
  expect_no_match(txt, "Inconsistency. I-squared", fixed = TRUE)
})

test_that("nothing rated down means no numbered footnote and an unmarked cell", {
  g <- make_clean()
  expect_true(all(g$domain_assessments$downgrade == 0))
  expect_identical(.rated_down_fact_domains(g), character(0))

  ft <- sof_table(g, style = "gradepro")
  expect_no_match(.body_col(ft, 6), "[", fixed = TRUE)
  expect_no_match(.footer_text(ft), "[1] ", fixed = TRUE)

  ft_bmj <- sof_table(g, style = "bmj")
  expect_identical(.body_col(ft_bmj, 7), g$certainty)
})

# --- the container helpers themselves --------------------------------------

test_that(".fact() and .facts() build and bind the container", {
  f <- .fact("i2", "I-squared", "62.3%", 62.3)
  expect_s3_class(f, "tbl_df")
  expect_identical(names(f), c("key", "label", "value", "numeric"))
  expect_identical(nrow(f), 1L)

  # A non-character value is coerced for display but kept in `numeric`.
  f2 <- .fact("k", "Studies", 11L, 11L)
  expect_identical(f2$value, "11")
  expect_identical(f2$numeric, 11)

  expect_null(.facts())
  expect_null(.facts(NULL, NULL))
  expect_identical(nrow(.facts(f, NULL, f2)), 2L)
  # A list argument is accepted, so a caller can assemble facts conditionally.
  expect_identical(nrow(.facts(list(f, NULL, f2))), 2L)
})
