# test-not-reported.R - outcomes the review prespecified and nobody reported.
#
# The pmatools API for these (not_reported_outcome(), add_not_reported()) and
# the table rows it produces are the package's own tests. These are about the
# app's side of the wiring: which saved rows survive normalisation, what the
# saved-outcome list says about one, what the two modals ask for, and the
# guards that keep a row with no analysis out of the arguments that describe
# analyses.

library(testthat)

fake_meta <- function(sm = "OR", k = 3L) {
  structure(list(sm = sm, k = k, studlab = paste("Study", seq_len(k)),
                 event.e = rep(1, k), method.tau = "REML", random = TRUE,
                 common = FALSE, method.random.ci = "classic"),
            class = c("metabin", "meta"))
}

rated <- function(name, data = NULL) {
  g <- structure(
    list(meta = fake_meta(), outcome_name = name, certainty = "High",
         study_design = "RCT", outcome_type = "relative",
         threshold_type = "null", threshold = 1.25),
    class = "pmatools")
  pma_bank_export_material(g, data = data, experimental_label = "drug",
                           control_label = "placebo")
}

long_rows <- function(studies) {
  data.frame(studlab = rep(studies, 2),
             treat = rep(c("drug", "placebo"), each = length(studies)),
             n = 100, event = 10, stringsAsFactors = FALSE)
}

mixed_outcomes <- function() {
  list(Mortality       = rated("Mortality", data = long_rows(c("A", "B"))),
       `Quality of life` = not_reported_outcome(
         "Quality of life", follow_up = "12 months",
         reason = "Prespecified; no included trial measured it."),
       Relapse         = rated("Relapse", data = long_rows(c("A", "B"))))
}

# --- what survives normalisation -------------------------------------------

test_that("pma_outcomes_list() keeps not-reported rows and junk stays out", {
  # This is the filter that decided the feature: `pmatools_not_reported` does
  # not inherit "pmatools" on purpose, so while the check named one class a
  # declared row vanished the next time anything read state$outcomes back.
  outs <- pma_outcomes_list(c(mixed_outcomes(), list(Nonsense = 42)))
  expect_identical(names(outs), c("Mortality", "Quality of life", "Relapse"))
  expect_s3_class(outs[["Quality of life"]], "pmatools_not_reported")
  expect_false(inherits(outs[["Quality of life"]], "pmatools"))
})

test_that("pma_rated_outcomes() is the subset with an analysis behind it", {
  expect_identical(names(pma_rated_outcomes(mixed_outcomes())),
                   c("Mortality", "Relapse"))
  expect_length(pma_rated_outcomes(list(A = not_reported_outcome("A"))), 0L)
  expect_length(pma_rated_outcomes(NULL), 0L)
})

test_that("re-banking a rated outcome does not drop a not-reported row", {
  # Auto-save round-trips the whole list through pma_upsert_outcome() on every
  # recompute, so a row it silently discarded would disappear on the next
  # keystroke in a rationale field.
  outs <- pma_upsert_outcome(mixed_outcomes(), "Mortality",
                             rated("Mortality"), uid = "outcome-1")
  expect_identical(names(outs), c("Mortality", "Quality of life", "Relapse"))
  expect_s3_class(outs[["Quality of life"]], "pmatools_not_reported")
})

# --- the saved-outcome list ------------------------------------------------

test_that("the saved-outcome row says what it is instead of reading '-'", {
  df <- pma_outcome_summary_df(mixed_outcomes())
  i  <- match("Quality of life", df$name)
  expect_true(df$not_reported[i])
  expect_identical(df$k[i], "0")
  expect_identical(df$effect[i], "Not reported")
  expect_identical(df$certainty[i], NOT_REPORTED_CERTAINTY)
  # The rated rows are untouched.
  expect_false(any(df$not_reported[-i]))
  expect_identical(unique(df$certainty[-i]), "High")
})

test_that("the empty summary frame carries the not_reported column too", {
  # Callers index df$not_reported unconditionally; a frame missing the column
  # in the zero-row case would only fail once the list was empty.
  expect_identical(names(pma_outcome_summary_df(list())),
                   names(pma_outcome_summary_df(mixed_outcomes())))
})

test_that("a not-reported row gets a grey badge, not a GRADE colour", {
  # "Not rated" is the absence of a rating, not a fifth rung under Very low.
  badge <- as.character(pma_certainty_badge(NOT_REPORTED_CERTAINTY))
  expect_match(badge, "grade-unrated", fixed = TRUE)
  expect_no_match(badge, "grade-low", fixed = TRUE)
  expect_match(as.character(pma_certainty_badge("Low")), "grade-low",
               fixed = TRUE)
})

test_that("the saved-outcome list renders a mixed set", {
  html <- as.character(pma_saved_outcomes_ui(mixed_outcomes()))
  expect_match(html, "Quality of life", fixed = TRUE)
  expect_match(html, "Not reported", fixed = TRUE)
  expect_match(html, NOT_REPORTED_CERTAINTY, fixed = TRUE)
})

# --- the two modals --------------------------------------------------------

test_that("the choice modal offers both routes and names their input ids", {
  html <- as.character(pma_add_outcome_choice_modal())
  expect_match(html, PMA_ADD_OUTCOME_ANALYSE_ID, fixed = TRUE)
  expect_match(html, PMA_ADD_OUTCOME_NOT_REPORTED_ID, fixed = TRUE)
  expect_match(html, "Analyse it from the data", fixed = TRUE)
  expect_match(html, "Record it as not reported", fixed = TRUE)
  # Shiny.setInputValue with priority "event", so a second click on the same
  # button fires the observer again (an actionButton rebuilt with the modal
  # would report 0 before it reported 1). The quotes are HTML-escaped in the
  # onclick attribute, so match the part that is not.
  expect_match(html, "Shiny.setInputValue(", fixed = TRUE)
  expect_match(html, "priority:", fixed = TRUE)
})

test_that("the not-reported modal collects name, follow-up and reason", {
  html <- as.character(pma_not_reported_modal())
  for (id in c("not_reported_name", "not_reported_follow_up",
               "not_reported_reason", PMA_NOT_REPORTED_SAVE_ID)) {
    expect_match(html, id, fixed = TRUE)
  }
})

# --- the form's rules ------------------------------------------------------

test_that("pma_not_reported_entry() builds the row the form describes", {
  entry <- pma_not_reported_entry(
    "  Quality of life  ", follow_up = "12 months",
    reason = "Prespecified; no included trial measured it.",
    existing = c("Mortality"))
  expect_true(entry$ok)
  expect_identical(entry$name, "Quality of life")
  expect_s3_class(entry$outcome, "pmatools_not_reported")
  expect_identical(entry$outcome$follow_up, "12 months")
  expect_identical(entry$outcome$outcome_name, "Quality of life")
})

test_that("pma_not_reported_entry() normalises the two optional fields away", {
  # not_reported_outcome() treats "", NA and NULL alike, so the form's empty
  # boxes must not arrive as empty strings that print as blank footnotes.
  entry <- pma_not_reported_entry("Quality of life", follow_up = "",
                                  reason = "   ")
  expect_true(entry$ok)
  expect_null(entry$outcome$follow_up)
  expect_null(entry$outcome$reason)
})

test_that("pma_not_reported_entry() refuses a blank name", {
  for (bad in list(NULL, "", "   ", NA_character_)) {
    entry <- pma_not_reported_entry(bad)
    expect_false(entry$ok)
    expect_match(entry$message, "Name the outcome")
  }
})

test_that("pma_not_reported_entry() refuses a name already saved", {
  # An outcome is either rated or not reported, not both - the same rule
  # add_not_reported() enforces, reached before the reviewer loses the form.
  entry <- pma_not_reported_entry("Mortality",
                                  existing = names(mixed_outcomes()))
  expect_false(entry$ok)
  expect_match(entry$message, "either rated or not reported")
  # The comparison is on the trimmed name, as the successful path stores it.
  expect_false(pma_not_reported_entry("  Mortality  ",
                                      existing = "Mortality")$ok)
})

# --- keeping a row with no analysis out of the analysis arguments ----------

test_that("the export set carries the row but not its name as an analysis", {
  set <- pma_export_set(mixed_outcomes(), primary = "Mortality")

  # A row of the table, and a numbered directory in the ZIP.
  expect_identical(set$order,
                   c("Mortality", "Quality of life", "Relapse"))
  expect_s3_class(set$outcomes[["Quality of life"]], "pmatools_not_reported")

  # But never an argument that describes an analysis: these become
  # run_ma_multi(outcomes = ) and grade_meta_multi(per_outcome = ) in the
  # generated analysis.R, and there is nothing to run or rate.
  expect_identical(set$ma_args$outcomes, c("Mortality", "Relapse"))
  expect_identical(names(set$per_outcome), c("Mortality", "Relapse"))
  expect_identical(names(set$grade_args), c("Mortality", "Relapse"))
  expect_false("Quality of life" %in% unique(set$data$outcome))
})

test_that("a bundle of nothing but not-reported rows is refused", {
  # It would have no analysis to build from and no effect measure to head the
  # table's columns. The Download button is gated on Steps 2-3 as well, so
  # this is the second line of defence rather than the first.
  expect_error(
    pma_export_set(list(A = not_reported_outcome("A"))),
    "at least one rated outcome")
})

test_that("pma_export_data() leaves not-reported outcomes out of the CSV", {
  d <- pma_export_data(mixed_outcomes())
  expect_setequal(unique(d$outcome), c("Mortality", "Relapse"))
})

# --- the footnote that used to say this was impossible ---------------------

test_that("the limitations note no longer claims the rows are absent", {
  expect_no_match(pma_sof_limitations_note(), "Not reported", fixed = TRUE)
  # The other two caveats it carries are untouched.
  expect_match(pma_sof_limitations_note(), "Arm-level values", fixed = TRUE)
  expect_match(pma_sof_limitations_note(), "Per-domain footnotes", fixed = TRUE)
})
