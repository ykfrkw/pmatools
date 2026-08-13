# test-export-set.R - assembling the pmatools_set the ZIP is built from.
#
# The app banks each outcome with everything export_bundle() needs to know
# about it, because at download time the live state describes only the outcome
# on screen. These tests are about what gets banked and what is rebuilt from
# it; the resulting ZIP layout is the package's own test.

library(testthat)

fake_meta <- function(sm = "OR", binary = TRUE, k = 3L,
                      method.tau = "REML", random = TRUE, common = FALSE,
                      hk = FALSE) {
  m <- list(sm = sm, k = k, studlab = paste("Study", seq_len(k)),
            method.tau = method.tau, random = random, common = common,
            method.random.ci = if (hk) "HK" else "classic")
  if (binary) m$event.e <- rep(1, k)
  structure(m, class = c("metabin", "meta"))
}

fake_outcome <- function(name, meta_obj = fake_meta(), data = NULL,
                         specs = list(), ...) {
  g <- structure(
    utils::modifyList(
      list(meta = meta_obj, outcome_name = name, certainty = "High",
           study_design = "RCT", outcome_type = "relative",
           threshold_type = "null", threshold = 1.25),
      list(...)),
    class = "pmatools")
  attr(g, PMA_GRADE_ARGS_ATTR) <- specs
  pma_bank_export_material(g, data = data,
                           experimental_label = "drug",
                           control_label      = "placebo")
}

long_rows <- function(studies, extra = NULL) {
  d <- data.frame(studlab = rep(studies, 2),
                  treat = rep(c("drug", "placebo"), each = length(studies)),
                  n = 100, event = 10, stringsAsFactors = FALSE)
  if (!is.null(extra)) for (nm in names(extra)) d[[nm]] <- extra[[nm]]
  d
}

test_that("banking stamps both attributes, and drops what was not set", {
  g <- fake_outcome("Mortality", data = long_rows(c("A", "B")))

  src <- attr(g, PMA_OUTCOME_SOURCE_ATTR, exact = TRUE)
  expect_equal(src$experimental_label, "drug")
  expect_equal(nrow(src$data), 4L)

  # Nothing was displayed, so nothing is claimed: no field at all, rather than
  # a NULL under every name that would override the set-wide argument with
  # nothing.
  expect_length(attr(g, PMATOOLS_DISPLAY_ATTR, exact = TRUE), 0L)

  g2 <- pma_bank_export_material(
    g, display = list(forest_step2 = list(label_e = "Drug"),
                      forest_rob   = list(label_e = "Drug (RoB)")),
    pubias_missing = data.frame(studlab = "C", n = 50,
                                results_known = "no",
                                stringsAsFactors = FALSE))
  d <- attr(g2, PMATOOLS_DISPLAY_ATTR, exact = TRUE)
  expect_equal(d$forest_display, list(label_e = "Drug"))
  expect_equal(d$forest_display_rob, list(label_e = "Drug (RoB)"))
  expect_equal(d$pubias_missing_df$studlab, "C")
  expect_false("rare" %in% names(d))

  # Every name it does carry is one the bundler or grade_table() reads.
  expect_true(all(names(d) %in% PMATOOLS_DISPLAY_ATTR_FIELDS))
})

test_that("the responder presentation is banked with the outcome it describes", {
  g <- fake_outcome("Depression")

  # The effect route claims nothing: an outcome shown as its own effect must
  # not carry a convert_smd_to_or = FALSE that reads as a decision.
  effect_route <- pma_bank_export_material(
    g, display = list(convert = FALSE, baseline_risk = 0.2))
  expect_false("convert_smd_to_or" %in%
                 names(attr(effect_route, PMATOOLS_DISPLAY_ATTR, exact = TRUE)))

  # state$display$convert is the guarded boolean, so all four travel together.
  responder <- pma_bank_export_material(
    g, display = list(convert = TRUE, baseline_risk = 0.2,
                      threshold_label = ">=50% drop in PHQ-9",
                      chinn_invert = TRUE))
  d <- attr(responder, PMATOOLS_DISPLAY_ATTR, exact = TRUE)
  expect_true(d$convert_smd_to_or)
  expect_equal(d$baseline_risk, 0.2)
  expect_equal(d$threshold_label, ">=50% drop in PHQ-9")
  expect_true(d$chinn_invert)
  expect_true(all(names(d) %in% PMATOOLS_DISPLAY_ATTR_FIELDS))
})

test_that("export data binds each outcome's own rows under its own name", {
  outs <- list(
    "Mortality" = fake_outcome("Mortality",
                               data = long_rows(c("A", "B"),
                                                extra = list(rob = "no"))),
    "Relapse"   = fake_outcome("Relapse", data = long_rows(c("C", "D"))))

  d <- pma_export_data(outs)
  expect_equal(nrow(d), 8L)
  expect_equal(sort(unique(d$outcome)), c("Mortality", "Relapse"))
  # The union of columns, not the intersection: dropping to the intersection
  # would throw away the rob column the first outcome was rated from.
  expect_true("rob" %in% names(d))
  expect_equal(unique(d$rob[d$outcome == "Mortality"]), "no")
  expect_true(all(is.na(d$rob[d$outcome == "Relapse"])))
})

test_that("the exported data names the arms, and keeps what they were called", {
  outs <- list("Mortality" = fake_outcome("Mortality",
                                          data = long_rows(c("A", "B"))))
  d <- pma_export_data(outs)
  # "drug" / "placebo" were this outcome's arm values; two outcomes from
  # different files can disagree about them, and run_ma_multi() takes one
  # answer for the whole set - so the column carries it instead. Getting it
  # wrong inverts the pooled effect.
  expect_equal(sort(unique(d$treat)), c("control", "experimental"))
  expect_equal(sort(unique(d$treat_label)), c("drug", "placebo"))
})

test_that("arms are left alone when the outcome names no labels", {
  g <- fake_outcome("Mortality", data = long_rows("A"))
  attr(g, PMA_OUTCOME_SOURCE_ATTR) <- pma_outcome_source(
    data = long_rows("A"))
  d <- pma_export_data(list("Mortality" = g))
  # run_ma() resolved these arms itself when the rating was made, from these
  # same rows, so it resolves them the same way again.
  expect_equal(sort(unique(d$treat)), c("drug", "placebo"))
  expect_false("treat_label" %in% names(d))
})

test_that("an outcome column in the source data is replaced by the outcome name", {
  outs <- list("Depression" = fake_outcome(
    "Depression",
    data = long_rows(c("A", "B"), extra = list(outcome = "PHQ-9"))))
  d <- pma_export_data(outs)
  expect_equal(unique(d$outcome), "Depression")
})

test_that("export data is NULL when no outcome carries any", {
  expect_null(pma_export_data(list("Mortality" = fake_outcome("Mortality"))))
  expect_null(pma_export_data(list()))
})

test_that("run_ma settings are per outcome where they can differ", {
  outs <- list(
    "Mortality" = fake_outcome("Mortality", fake_meta(sm = "OR")),
    "Change"    = fake_outcome("Change",
                               fake_meta(sm = "SMD", binary = FALSE)))
  args <- pma_set_ma_args(outs)
  expect_equal(args$outcomes, c("Mortality", "Change"))
  expect_equal(args$sm, list(Mortality = "OR", Change = "SMD"))
  expect_equal(args$outcome_type,
               list(Mortality = "binary", Change = "continuous"))
})

test_that("a run_ma setting the outcomes disagree about is not claimed for all", {
  agree <- list(
    "A" = fake_outcome("A", fake_meta(method.tau = "REML")),
    "B" = fake_outcome("B", fake_meta(method.tau = "REML")))
  expect_equal(pma_uniform_ma_dots(agree)$method.tau, "REML")
  expect_false(pma_uniform_ma_dots(agree)$hakn)
  # Arm labels are never a set-wide dot; the data names the arms instead.
  expect_false("experimental_label" %in% names(pma_uniform_ma_dots(agree)))

  differ <- list(
    "A" = fake_outcome("A", fake_meta(method.tau = "REML", hk = TRUE)),
    "B" = fake_outcome("B", fake_meta(method.tau = "DL", hk = FALSE)))
  dots <- pma_uniform_ma_dots(differ)
  expect_false("method.tau" %in% names(dots))
  expect_false("hakn" %in% names(dots))
})

test_that("grade args recover what the multi-outcome script cannot read back", {
  g <- fake_outcome("Mortality", specs = list(
    small_values = list(value = "undesirable", origin = "scalar")),
    threshold_type = "null", follow_up = "12 months", unit = "days")
  args <- pma_outcome_grade_args(g)

  expect_equal(args$study_design, "RCT")
  # Without threshold_type the regenerated call does not merely rate
  # differently, it aborts on the Core GRADE 2 entry gate.
  expect_equal(args$threshold_type, "null")
  expect_equal(args$follow_up, "12 months")
  expect_equal(args$unit, "days")
  expect_equal(args$small_values, list(value = "undesirable",
                                       origin = "scalar"))
  expect_false("require_threshold" %in% names(args))
})

test_that("a rating made without a MID is reproduced without the gate", {
  g <- fake_outcome("Mortality", threshold_type = "mid", threshold = NULL)
  expect_false(pma_outcome_grade_args(g)$require_threshold)
})

test_that("a declined low-RoB refit is reproduced as declined", {
  g <- fake_outcome("Mortality", rob_analysis_set = "low_only",
                    rob_refit = FALSE)
  expect_false(pma_outcome_grade_args(g)$rob_refit)
  expect_false("rob_refit" %in%
                 names(pma_outcome_grade_args(fake_outcome("Mortality"))))
})

test_that("the specs the app actually passed win over the recovered values", {
  g <- fake_outcome("Mortality", specs = list(
    threshold_type = list(value = "mid", origin = "scalar")),
    threshold_type = "null")
  expect_equal(pma_outcome_grade_args(g)$threshold_type,
               list(value = "mid", origin = "scalar"))
})

test_that("pma_export_set builds a set of every banked outcome, in order", {
  skip_if_not(exists(".new_pmatools_set"), "package sources not available")
  outs <- list(
    "Mortality" = fake_outcome("Mortality", data = long_rows(c("A", "B"))),
    "Relapse"   = fake_outcome("Relapse",   data = long_rows(c("A", "B"))))

  set <- pma_export_set(outs, primary = c("Relapse", "Not an outcome"))
  expect_s3_class(set, "pmatools_set")
  expect_equal(set$order, c("Mortality", "Relapse"))
  # A stale primary name is dropped rather than aborting the download.
  expect_equal(set$primary, "Relapse")
  expect_equal(nrow(set$data), 8L)
  expect_equal(names(set$per_outcome), c("Mortality", "Relapse"))
  # grade_args is what makes export_bundle() render analysis.R at all.
  expect_false(is.null(set$grade_args))
  expect_equal(set$common, list())
})

test_that("a one-element set is built exactly like an N-element one", {
  skip_if_not(exists(".new_pmatools_set"), "package sources not available")
  set <- pma_export_set(
    list("Mortality" = fake_outcome("Mortality", data = long_rows("A"))))
  expect_equal(set$order, "Mortality")
  expect_equal(set$primary, character(0))
  expect_equal(unique(set$data$outcome), "Mortality")

  expect_error(pma_export_set(list()), "at least one rated outcome")
})
