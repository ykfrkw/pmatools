# test-detect_column_roles.R - the reporting half of ingest_data()'s alias
# resolution. Step 1 of the Shiny app tells a reviewer which column filled
# which role from this, so a claim it makes here has to be a claim
# ingest_data() then honours; the last test in this file is what pins that.

library(testthat)

ROLE_ORDER <- c("studlab", "treat", "n", "event", "mean", "sd",
                "outcome", "rob", "indirectness", "subgroup")

# The column that filled `role`, or NA.
filled_by <- function(detected, role) detected$column[match(role, detected$role)]

test_that("every role is reported, in ingest order", {
  detected <- detect_column_roles(c("studlab", "treat", "n"))

  expect_equal(detected$role, ROLE_ORDER)
  expect_equal(nrow(detected), length(ROLE_ORDER))
})

test_that("canonical names are reported as canonical", {
  detected <- detect_column_roles(
    c("studlab", "treat", "n", "event", "rob", "indirectness"))

  expect_true(all(detected$found[detected$role %in%
                                   c("studlab", "treat", "n", "event",
                                     "rob", "indirectness")]))
  expect_equal(unique(detected$matched_by[detected$found]), "canonical")
  expect_equal(filled_by(detected, "studlab"), "studlab")
})

test_that("aliases are reported with the source column that filled the role", {
  detected <- detect_column_roles(
    c("study", "arm", "n_randomized", "d_r", "rob_d"))

  expect_equal(filled_by(detected, "studlab"), "study")
  expect_equal(filled_by(detected, "treat"), "arm")
  expect_equal(filled_by(detected, "n"), "n_randomized")
  expect_equal(filled_by(detected, "event"), "d_r")
  expect_equal(filled_by(detected, "rob"), "rob_d")
  expect_equal(unique(detected$matched_by[detected$found]), "alias")
})

test_that("an unfilled role reports NA rather than dropping out", {
  detected <- detect_column_roles(c("studlab", "treat", "n"))

  expect_false(detected$found[detected$role == "indirectness"])
  expect_true(is.na(filled_by(detected, "indirectness")))
  expect_true(is.na(detected$matched_by[detected$role == "indirectness"]))
})

test_that("required roles are exactly the ones ingest_data aborts without", {
  detected <- detect_column_roles(c("studlab", "treat", "n"))

  expect_equal(detected$role[detected$required], c("studlab", "treat", "n"))
})

test_that("a canonical column beats an alias for the same role", {
  detected <- detect_column_roles(c("studlab", "study", "treat", "n"))

  expect_equal(filled_by(detected, "studlab"), "studlab")
})

test_that("the first alias listed wins when several are present", {
  # `treatment` precedes `arm` in the alias list, and the sample dataset
  # carries both.
  detected <- detect_column_roles(c("study", "arm", "treatment", "n"))

  expect_equal(filled_by(detected, "treat"), "treatment")
})

test_that("a column claimed by one role is not reported for another", {
  # `group` is an alias of both `treat` and `subgroup`; treat is resolved
  # first, so subgroup must come back empty rather than naming the same
  # column twice.
  detected <- detect_column_roles(c("studlab", "group", "n"))

  expect_equal(filled_by(detected, "treat"), "group")
  expect_false(detected$found[detected$role == "subgroup"])
})

test_that("outcome is recognised as a role but has no aliases", {
  expect_true(detect_column_roles(c("studlab", "treat", "n", "outcome"))$found[
    detect_column_roles(c("studlab", "treat", "n", "outcome"))$role == "outcome"])

  detected <- detect_column_roles(c("studlab", "treat", "n", "endpoint"))
  expect_false(detected$found[detected$role == "outcome"])
})

test_that("a data.frame and its names give the same answer", {
  df <- data.frame(study = "A", arm = "x", n_randomized = 1,
                   stringsAsFactors = FALSE)

  expect_equal(detect_column_roles(df), detect_column_roles(names(df)))
})

test_that("the bundled sample reports the roles it actually has", {
  path <- system.file("extdata", "cbti_depression.csv", package = "pmatools")
  skip_if(!nzchar(path), "sample dataset not installed")
  detected <- detect_column_roles(utils::read.csv(path))

  expect_equal(filled_by(detected, "studlab"), "study")
  expect_equal(filled_by(detected, "n"), "n_randomized")
  expect_equal(filled_by(detected, "event"), "d_r")
  expect_equal(filled_by(detected, "rob"), "rob_d")
  # A binary dataset carries no mean/SD, and this one rates no indirectness.
  expect_false(detected$found[detected$role == "mean"])
  expect_false(detected$found[detected$role == "indirectness"])
})

test_that("what is reported as filled is what ingest_data fills", {
  # The claim the strip makes. Everything above tests the report in
  # isolation; this pins it to the renaming it describes.
  df <- data.frame(
    study        = rep(c("A", "B"), each = 2),
    arm          = rep(c("experimental", "control"), 2),
    n_randomized = c(50, 50, 60, 60),
    d_r          = c(10, 15, 15, 20),
    rob_d        = rep(c("L", "S"), each = 2),
    stringsAsFactors = FALSE
  )
  detected <- detect_column_roles(df)
  ingested <- ingest_data(df, format = "long")

  expect_setequal(detected$role[detected$found],
                  intersect(ROLE_ORDER, names(ingested)))
  expect_equal(ingested$studlab, df$study)
  expect_equal(ingested$event, df$d_r)
})
