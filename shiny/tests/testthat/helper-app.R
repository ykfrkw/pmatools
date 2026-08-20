# helper-app.R - make the app's pure helpers available to the tests.
#
# There is no package here, so nothing can be library()'d. The files below are
# sourced straight out of R/, in the order app.R sources them, and every one of
# them only DEFINES things at source time - no Shiny session is created, no
# reactive is touched.
#
# The six files after R/ui_helpers.R are what came out of it when it was split
# by role: the multi-outcome bank, the provenance signatures and input
# registry, the judgment badges and flowcharts, the forest / funnel display
# panels, the column-role and analysis-set helpers, and the Summary of Findings
# presentation. They are all pure and the suite calls into all six directly, so
# they are on the list for the same reason R/ui_helpers.R always was. They keep
# app.R's relative order among themselves and sit immediately after the file
# they were carved out of, so that a helper moving between the seven never
# changes what the suite has loaded.
#
# R/step3_pubias.R is on the list for neither half: it defines
# step3_pubias_server() and .effective_pubias_k() and nothing else, and no test
# calls either. It is sourced anyway so that the helper keeps loading the same
# set of files app.R does - a file left off the list is a file whose parse
# errors and top-level name collisions the suite would stop noticing.
#
# R/step2_ma.R and R/step3_grade.R are on the list for their UI halves only:
# step2_ui() and step3_ui() are pure functions of `state` and can be rendered
# to HTML and inspected (see test-step2-layout.R and
# test-confirm-checkbox.R). Their other halves, step2_server() and
# step3_server(), are merely defined here and never called - like
# R/step1_data.R and R/step4_export.R, whose server functions need `input` /
# `output` / `session` / `state` and a Shiny test harness this app does not
# have.
#
# Sourcing a file for one half of it is only safe because nothing in it runs
# at source time and no top-level name collides with one already sourced;
# test-vendor-collisions.R is the standing guard on the second half of that.

# testthat::test_dir() sets the working directory to tests/testthat before
# sourcing helpers, so walking up from getwd() lands on the app root -- which
# is shiny/ now that the app is a subdirectory of the package repo, not the
# repo root. tests/testthat.R starts its own walk from the script path
# instead, because it runs before test_dir() has set anything.
PMA_APP_ROOT <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "app.R"))) {
    up <- dirname(d)
    if (identical(up, d)) stop("could not find app.R above ", getwd())
    d <- up
  }
  d
})

# The package's own R/utils.R, which the running app gets from the staged
# copy under R/_pmatools/. judgment_display.R reads GRADE_LEVEL_SOURCE_WORDING /
# .grade_level_wording() from it so the app's badges and the Evidence Profile
# cannot word the same judgment differently, and without it every badge test
# would fail on a missing object rather than on a wrong label. Both locations
# are tried and neither is required: a checkout with no staged bundle still
# has ../../R relative to shiny/, and the guard keeps the helper usable in a
# tree where the sources are laid out some third way.
#
# R/multi_outcome.R rides along for PMATOOLS_DISPLAY_ATTR and the set
# constructor: the app's export helpers write that attribute and build a
# pmatools_set out of the banked outcomes, so without it their tests would
# fail on a missing object rather than on a wrong bundle.
#
# R/data_ingest.R rides along for detect_column_roles(): Step 1's
# detected-columns strip is a presentation of that function's output, and a
# test that hand-built the frame instead would keep passing after the two
# drifted apart.
#
# R/not_reported.R rides along because pma_outcomes_list() and the modal
# helpers are built ON it: `.is_not_reported()`, `.rated_outcomes()` and
# `not_reported_outcome()` decide which saved rows survive normalisation, and
# an app-side re-implementation of that class check is exactly the drift the
# other three entries are here to prevent.
# R/rare_step3.R and R/rare_events.R ride along for the same reason as the
# four above: the Configuration tab's rare-event block (.rare_method_block()
# in R/step3_threshold.R) prints rare_method_statement() and
# PMA_RARE_NO_CC_NOTE verbatim, and .rare_method_label() reads the method
# labels off .rare_method_specs() so the app and the fitted suite cannot name
# the same method two ways. Without them a test of that block would fail on a
# missing object rather than on wrong copy.
for (.stem in c("utils.R", "multi_outcome.R", "data_ingest.R",
                "not_reported.R", "rare_events.R", "rare_step3.R")) {
  for (.f in c(file.path(PMA_APP_ROOT, "R", "_pmatools", .stem),
               file.path(dirname(PMA_APP_ROOT), "R", .stem))) {
    if (file.exists(.f)) {
      source(.f)
      break
    }
  }
}
rm(.stem)

for (.f in c("R/ui_helpers.R", "R/outcome_bank.R", "R/outcome_provenance.R",
             "R/judgment_display.R", "R/plot_panels.R", "R/column_roles.R",
             "R/sof_display.R", "R/educational_copy.R", "R/step3_threshold.R",
             "R/step3_pubias.R", "R/step2_ma.R", "R/step3_grade.R")) {
  source(file.path(PMA_APP_ROOT, .f))
}
rm(.f)
