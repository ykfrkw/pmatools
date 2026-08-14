# test-public-helpers.R — the three helpers promoted to the public API in
# v0.5.0: combine_arms(), format_effect(), rob_strata().
#
# They were internal (.combine_arms / .format_effect / .rob_plot_strata) and
# were being reached into from outside the package, notably by the Shiny front
# end, which needs to merge multi-arm rows, print an effect the same way the
# SoF table does, and agree with pmatools about what a risk-of-bias label
# means. These tests cover the public entry points and assert that the internal
# aliases still resolve to them, so existing call sites cannot silently drift.

library(testthat)
library(meta)

# --- combine_arms() ---------------------------------------------------------

test_that("combine_arms() sums a multi-arm trial's events and sample sizes", {
  df <- data.frame(
    studlab = c("Trial 1", "Trial 1", "Trial 1", "Trial 2", "Trial 2"),
    treat   = c("experimental", "experimental", "control",
                "experimental", "control"),
    n       = c(30, 28, 60, 50, 50),
    event   = c(12, 10, 15, 20, 18),
    stringsAsFactors = FALSE
  )
  out <- combine_arms(df)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4L)

  e1 <- out[out$studlab == "Trial 1" & out$treat == "experimental", ]
  expect_equal(nrow(e1), 1L)
  expect_equal(e1$n, 58)
  expect_equal(e1$event, 22)

  # The untouched study is carried through unchanged.
  e2 <- out[out$studlab == "Trial 2" & out$treat == "experimental", ]
  expect_equal(e2$n, 50)
  expect_equal(e2$event, 20)
})

test_that("combine_arms() pools continuous arms by Cochrane 6.5.2.10", {
  n1 <- 30; m1 <- -5.2; s1 <- 6.0
  n2 <- 28; m2 <- -4.4; s2 <- 6.4
  df <- data.frame(
    studlab = c("Trial 1", "Trial 1", "Trial 1"),
    treat   = c("experimental", "experimental", "control"),
    n       = c(n1, n2, 60),
    mean    = c(m1, m2, -1.1),
    sd      = c(s1, s2, 5.8),
    stringsAsFactors = FALSE
  )
  out <- combine_arms(df)
  arm <- out[out$treat == "experimental", ]

  n_exp <- n1 + n2
  m_exp <- (n1 * m1 + n2 * m2) / n_exp
  s_exp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2 +
                   (n1 * n2 / n_exp) * (m1 - m2)^2) / (n_exp - 1))

  expect_equal(arm$n, n_exp)
  expect_equal(arm$mean, m_exp)
  expect_equal(arm$sd, s_exp)
})

test_that("combine_arms() keys on outcome when the column is present", {
  df <- data.frame(
    studlab = rep("Trial 1", 4),
    outcome = c("Response", "Response", "Remission", "Remission"),
    treat   = rep("experimental", 4),
    n       = c(30, 28, 30, 28),
    event   = c(12, 10, 5, 4),
    stringsAsFactors = FALSE
  )
  out <- combine_arms(df)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$outcome, c("Response", "Remission"))
  expect_equal(out$n, c(58, 58))
})

test_that("combine_arms() returns the input untouched when nothing duplicates", {
  df <- data.frame(
    studlab = c("Trial 1", "Trial 1"),
    treat   = c("experimental", "control"),
    n       = c(50, 50),
    event   = c(20, 18),
    stringsAsFactors = FALSE
  )
  expect_identical(combine_arms(df), df)
})

test_that("combine_arms() carries per-study columns over from the first row", {
  df <- data.frame(
    studlab = c("Trial 1", "Trial 1", "Trial 1"),
    treat   = c("experimental", "experimental", "control"),
    n       = c(30, 28, 60),
    event   = c(12, 10, 15),
    rob     = c("L", "L", "L"),
    subgroup = c("adults", "adults", "adults"),
    stringsAsFactors = FALSE
  )
  out <- combine_arms(df)
  expect_true(all(c("rob", "subgroup") %in% names(out)))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$rob == "L"))
})

test_that("the .combine_arms alias still resolves to the public function", {
  df <- data.frame(
    studlab = c("Trial 1", "Trial 1", "Trial 1"),
    treat   = c("experimental", "experimental", "control"),
    n       = c(30, 28, 60),
    event   = c(12, 10, 15),
    stringsAsFactors = FALSE
  )
  expect_identical(.combine_arms(df), combine_arms(df))
})

# --- format_effect() --------------------------------------------------------

fe_binary <- function(sm = "RR", ...) {
  suppressWarnings(meta::metabin(
    event.e = c(10, 12, 8), n.e = c(50, 60, 40),
    event.c = c(20, 22, 18), n.c = c(50, 60, 40),
    studlab = c("Trial 1", "Trial 2", "Trial 3"),
    sm = sm, random = TRUE, ...
  ))
}

test_that("format_effect() exponentiates ratio measures and names them", {
  m <- fe_binary("RR")
  s <- format_effect(m, outcome_type = "relative")

  expect_type(s, "character")
  expect_length(s, 1L)
  expect_match(s, "^RR ")
  # The printed numbers are the back-transformed random-effects pool.
  expect_identical(
    s,
    sprintf("RR %.2f (%.2f; %.2f)",
            exp(m$TE.random), exp(m$lower.random), exp(m$upper.random))
  )
})

test_that("format_effect() leaves absolute measures on their own scale", {
  m <- suppressWarnings(meta::metacont(
    n.e = c(30, 40, 50), mean.e = c(-5.2, -4.8, -6.1), sd.e = c(6, 6.4, 5.9),
    n.c = c(30, 40, 50), mean.c = c(-1.1, -1.4, -0.9), sd.c = c(5.8, 6, 6.2),
    studlab = c("Trial 1", "Trial 2", "Trial 3"), sm = "SMD", random = TRUE
  ))
  s <- format_effect(m, outcome_type = "absolute")
  expect_match(s, "^SMD ")
  expect_identical(
    s,
    sprintf("SMD %.2f (%.2f; %.2f)",
            m$TE.random, m$lower.random, m$upper.random)
  )
})

test_that("format_effect() appends the prediction interval on request", {
  m <- fe_binary("RR", prediction = TRUE)
  plain <- format_effect(m, outcome_type = "relative")
  with_pi <- format_effect(m, outcome_type = "relative", prediction = TRUE)

  expect_false(grepl("PrI", plain, fixed = TRUE))
  expect_match(with_pi, "\nPrI \\(")
  expect_match(with_pi, "^RR ")
  # prediction = TRUE only adds a second line.
  expect_identical(strsplit(with_pi, "\n", fixed = TRUE)[[1]][1], plain)
})

test_that("format_effect() returns NR when there is no pooled estimate", {
  m <- fe_binary("RR")
  m$TE.random <- NA_real_
  m$TE.common <- NA_real_
  expect_identical(format_effect(m, outcome_type = "relative"), "NR")
})

test_that("format_effect() matches what the SoF table prints", {
  # The reason the function is exported: a caller rendering its own view must
  # get the identical string.
  data <- data.frame(
    studlab = rep(c("A", "B", "C"), each = 2),
    treat   = rep(c("experimental", "control"), 3),
    n       = c(50, 50, 60, 60, 70, 70),
    event   = c(10, 20, 12, 22, 8, 18),
    stringsAsFactors = FALSE
  )
  ma <- run_ma(data, outcome_type = "binary", sm = "RR")
  g  <- suppressWarnings(grade_meta(
    ma,
    small_values = "desirable", study_design = "RCT", rob = "no",
    rob_rationale = "Consensus RoB2: all domains low risk",
    indirectness = "no", outcome_name = "Depression response",
    threshold_type = "null"))

  ft <- sof_table(g)
  cells <- unlist(lapply(ft$body$dataset, as.character), use.names = FALSE)
  expect_true(format_effect(g$meta, g$outcome_type) %in% cells)
})

test_that("the .format_effect alias still resolves to the public function", {
  m <- fe_binary("RR", prediction = TRUE)
  expect_identical(.format_effect(m, "relative"),
                   format_effect(m, "relative"))
  expect_identical(.format_effect(m, "relative", prediction = TRUE),
                   format_effect(m, "relative", prediction = TRUE))
})

# --- rob_strata() -----------------------------------------------------------

test_that("rob_strata() maps every accepted vocabulary onto the same strata", {
  expect_identical(rob_strata(c("L", "S", "H")), c("low", "some", "high"))
  expect_identical(rob_strata(c("not_serious", "some_concerns", "very_serious")),
                   c("low", "some", "high"))
  expect_identical(rob_strata(c("low", "some", "high")),
                   c("low", "some", "high"))
  # pmatools' own older "... concerns" phrasings. Kept working on purpose:
  # they are what extraction sheets written against v0.4-v0.5.1 contain. They
  # are NOT RoB 2's words, which is what this release stopped claiming.
  expect_identical(
    rob_strata(c("No concerns", "Some concerns", "Serious concerns",
                 "Critical concerns")),
    c("low", "some", "high", "high"))
  # RoB 1 wording and the legacy aliases.
  expect_identical(rob_strata(c("unclear", "moderate", "very high")),
                   c("some", "some", "high"))
  expect_identical(rob_strata("very_serious"), "high")
})

test_that("Cochrane RoB 2's three judgments land where a reviewer expects", {
  # RoB 2 (Sterne JAC, et al. BMJ 2019;366:l4898) defines THREE levels and no
  # more. Up to 0.5.1 pmatools advertised four under RoB 2's name and accepted
  # only one of RoB 2's actual labels ("Some concerns"); the other two went to
  # the "unknown" stratum with a warning, which is the opposite of what the
  # documentation promised.
  expect_identical(
    rob_strata(c("Low risk of bias", "Some concerns", "High risk of bias")),
    c("low", "some", "high"))

  # And nothing about them is a warning path.
  expect_silent(
    rob_strata(c("Low risk of bias", "Some concerns", "High risk of bias")))

  # Same three through the rating, where they decide which studies count as
  # high risk rather than merely which stratum a plot draws them in.
  expect_identical(
    vapply(c("Low risk of bias", "Some concerns", "High risk of bias"),
           pmatools:::.normalize_rob_level, character(1), USE.NAMES = FALSE),
    c("not_serious", "serious", "very_serious"))
})

test_that("ROBINS-I's four judgments are accepted, and are not RoB 2's", {
  # Kept, not removed: grade_meta() is public and a script may rate
  # non-randomised evidence with it. The Shiny app cannot reach this -- it
  # hardcodes study_design = "RCT" -- which is a reason to document the two
  # vocabularies apart, not a reason to drop a working one.
  expect_identical(
    rob_strata(c("Low risk of bias", "Moderate risk of bias",
                 "Serious risk of bias", "Critical risk of bias")),
    c("low", "some", "high", "high"))

  # Serious and Critical share the "high" stratum because Core GRADE
  # describes no three-level risk-of-bias downgrade for either to reach.
  expect_identical(
    vapply(c("Serious risk of bias", "Critical risk of bias"),
           pmatools:::.normalize_rob_level, character(1), USE.NAMES = FALSE),
    c("very_serious", "very_serious"))

  # The two tools disagree about the middle level, which is why the strings
  # are stored separately rather than folded into one "four-level" table.
  expect_identical(rob_strata("Moderate risk of bias"), "some")
  expect_identical(rob_strata("High risk of bias"), "high")
})

test_that("an unrecognised label's warning names the right vocabularies", {
  w <- tryCatch(rob_strata("Very high risk of bias, probably"),
                warning = function(e) conditionMessage(e))
  expect_match(w, "Cochrane RoB 2", fixed = TRUE)
  expect_match(w, "ROBINS-I", fixed = TRUE)
  # The four-level list that RoB 2 never defined is gone from the message.
  expect_false(grepl("RoB2 labels", w, fixed = TRUE))
})

test_that("rob_strata() is case- and whitespace-insensitive", {
  expect_identical(rob_strata(c(" low ", "SOME CONCERNS", "High")),
                   c("low", "some", "high"))
})

test_that("rob_strata() treats blanks and explicit unknowns as unknown", {
  expect_silent(out <- rob_strata(c("low", NA, "", "?", "unknown", "NA")))
  expect_identical(out, c("low", rep("unknown", 5L)))
  # An all-blank vector short-circuits and is still the right length.
  expect_identical(rob_strata(c(NA, NA)), c("unknown", "unknown"))
  expect_identical(rob_strata(character(0)), character(0))
})

test_that("rob_strata() warns rather than aborting on unrecognised labels", {
  # Deliberate: this feeds plots, and a plot with an "unknown" stratum beats
  # no plot at all.
  expect_warning(out <- rob_strata(c("low", "not sure yet")),
                 "unrecognized label")
  expect_identical(out, c("low", "unknown"))

  w <- tryCatch(rob_strata("not sure yet", arg = "my_app: rob column"),
                warning = function(e) conditionMessage(e))
  expect_match(w, "my_app: rob column", fixed = TRUE)
  expect_match(w, "not sure yet", fixed = TRUE)
})

test_that("rob_strata() only ever returns the four documented strata", {
  labels <- c("L", "S", "H", "C", "M", "*", "no", "some", "very_serious",
              "low", "moderate", "unclear", "high", "very high",
              "Low risk of bias", "Some concerns", "High risk of bias",
              "Moderate risk of bias", "Serious risk of bias",
              "Critical risk of bias",
              "No concerns", "Serious concerns",
              "Critical concerns", NA, "", "?", "banana")
  out <- suppressWarnings(rob_strata(labels))
  expect_length(out, length(labels))
  expect_true(all(out %in% c("low", "some", "high", "unknown")))
})

test_that("the .rob_plot_strata alias still resolves to the public function", {
  labels <- c("L", "S", "H", NA)
  expect_identical(.rob_plot_strata(labels), rob_strata(labels))
  w1 <- tryCatch(.rob_plot_strata("banana", arg = "caller"),
                 warning = function(e) conditionMessage(e))
  w2 <- tryCatch(rob_strata("banana", arg = "caller"),
                 warning = function(e) conditionMessage(e))
  expect_identical(w1, w2)
})

test_that("rob_strata() agrees with the strata plot_forest_rob draws", {
  # The point of exporting it: an outside editor using rob_strata() and
  # pmatools' own plot land on the same grouping.
  rob <- c("L", "Some concerns", "H")
  expect_identical(rob_strata(rob), c("low", "some", "high"))
  expect_identical(rob_strata(rob), .normalise_rob(rob))
})
