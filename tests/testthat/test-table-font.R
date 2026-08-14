# test-table-font.R — SPEC.md §4.6 "Table typography".
#
# The regression this file exists for: add_footer_lines() creates its rows
# after font(part = "all") has already run, so a footer row does NOT inherit
# the table's family -- it falls back to flextable's own default. Every builder
# shipped a footer in Helvetica under an Arial body until .style_table_footer()
# started re-applying the family at the end. Nothing about the builders stops
# that from happening again the next time a footnote is appended, so the
# footer is asserted here rather than the body.

library(testthat)

skip_if_not_installed("meta")
skip_if_not_installed("flextable")

part_fonts <- function(ft, part) {
  unique(as.vector(ft[[part]]$styles$text$font.family$data))
}

metabin_fixture <- function() {
  meta::metabin(
    event.e = c(10, 15, 20), n.e = c(50, 60, 70),
    event.c = c(15, 20, 25), n.c = c(50, 60, 70),
    studlab = c("Study A", "Study B", "Study C"),
    sm = "RR", method = "MH"
  )
}

# k = 3 leaves publication bias qualitatively assessed and imprecision rated
# down, so every builder below emits several footer lines rather than one.
graded_fixture <- function(...) {
  suppressWarnings(grade_meta(metabin_fixture(), study_design = "RCT",
                              rob = "no",
                              rob_rationale = "Consensus RoB2: all domains low",
                              indirectness = "no",
                              outcome_name = "Table font",
                              small_values = "desirable",
                              threshold_type = "null", ...))
}

subdomain_fixture <- function() {
  data.frame(
    subdomain = c("Population", "Intervention", "Comparison", "Outcome"),
    target    = c("Adults", "Any heparin", "Placebo", "Symptomatic VTE"),
    evidence  = c("Representative", "Both types pooled", "Placebo injections",
                  "Screening-detected cases included"),
    judgment  = c("yes", "yes", "yes", "probably_no"),
    stringsAsFactors = FALSE
  )
}

test_that(".style_table_footer sets the shared family on footer rows", {
  ft <- flextable::flextable(data.frame(a = 1, b = 2))
  ft <- flextable::font(ft, fontname = .PMA_TABLE_FONT, part = "all")
  ft <- flextable::add_footer_lines(ft, values = "a note")

  # Guard on the mechanism itself: if flextable ever starts propagating the
  # family into rows added later, this expectation flips and the helper below
  # becomes belt-and-braces rather than the fix.
  expect_false(identical(part_fonts(ft, "footer"), .PMA_TABLE_FONT))

  expect_identical(part_fonts(.style_table_footer(ft), "footer"),
                   .PMA_TABLE_FONT)
})

test_that("every table builder sets one family across body and footer", {
  graded <- graded_fixture()

  builders <- list(
    "sof_table(gradepro)"   = sof_table(graded),
    "sof_table(bmj)"        = sof_table(graded, style = "bmj"),
    "grade_table(gradepro)" = grade_table(list("Table font" = graded)),
    "grade_table(bmj)"      = grade_table(list("Table font" = graded),
                                          style = "bmj"),
    "evidence_profile"      = evidence_profile(graded),
    "indirectness_table"    = indirectness_table(
      suppressWarnings(grade_meta(metabin_fixture(), threshold_type = "null",
                                  small_values = "desirable",
                                  indirectness_subdomains = subdomain_fixture()))
    )
  )

  for (label in names(builders)) {
    ft <- builders[[label]]
    expect_gt(nrow(ft$footer$dataset), 0)
    expect_identical(part_fonts(ft, "body"),   .PMA_TABLE_FONT, info = label)
    expect_identical(part_fonts(ft, "header"), .PMA_TABLE_FONT, info = label)
    expect_identical(part_fonts(ft, "footer"), .PMA_TABLE_FONT, info = label)
  }
})

test_that("host-appended notes keep the shared family", {
  ft <- sof_add_notes(sof_table(graded_fixture()),
                      c("A registration number.", "A scope caveat."))
  expect_identical(part_fonts(ft, "footer"), .PMA_TABLE_FONT)
})
