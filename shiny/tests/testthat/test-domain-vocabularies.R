# test-domain-vocabularies.R - this app spells the GRADE domain names TWICE,
# on purpose, and the two spellings are one keystroke apart.
#
# `.GRADE_DOMAIN_NAMES` (the package, R/domain_facts.R) is the DOMAIN KEY
# vocabulary, and it says "Risk of bias" with a lower-case b. It is the string
# domain_facts() validates its `domain` argument against, and the string
# make_domain_row() writes into `$domain_assessments$domain` - so it is also
# the only spelling any app-side lookup may use. domain_judgment(),
# domain_notes(), domain_downgrade(), domain_fact_table(),
# step3_append_domain_note() and every `$domain_assessments$domain == ...`
# filter are all matching on it. Getting it wrong does not raise: the row is
# simply not found, the `%||% "not_serious"` fallback beside every one of
# those calls takes over, and the tab reports a rating nobody made.
#
# `PMA_DOMAIN_LABELS` (the app, R/ui_helpers.R) is the TAB TITLE vocabulary,
# and it says "Risk of Bias" with a capital B. Its values are shiny::tabPanel()
# `value`s, the <h4> that .domain_header() prints, the words inside
# pma_domain_jump_links(), and the name .override_or_ignore() sprintf()s into a
# showNotification(). Every one of those is read by a person or matched against
# another tab title. None of them reaches a package lookup - which is the fact
# the first test below pins down, by reading the call sites rather than
# trusting this paragraph.
#
# So this is not a typo anyone should fix, and two things make that plain.
#
# First, the two vectors do not even describe the same set. PMA_DOMAIN_LABELS
# carries `threshold = "Configuration"`, and Configuration is not a GRADE
# domain at all - it is the app's settings tab, which is on that list because
# it is gated by the same confirmation checkbox as the five real domains, not
# because GRADE has anything to say about it. A vector that is partly domains
# and partly furniture cannot be the domain-key vector no matter how it is
# spelled.
#
# Second, the capital B is load-bearing exactly where it stands. The Risk of
# Bias tab is declared `value = "Risk of Bias"`, and that literal is what
# `grade_tab_sequence` walks and what updateTabsetPanel() is handed - both
# directly, by the Back/Next observers, and indirectly, as
# `selected = PMA_DOMAIN_LABELS[[key]]` from the jump links in Step 3 and
# Step 4. Lower-case the tabPanel and tab navigation breaks; lower-case
# PMA_DOMAIN_LABELS alone and the jump links start selecting a tab that does
# not exist, silently, because updateTabsetPanel() does not complain about an
# unmatched `selected`. Either half of the "fix" is worse than the mismatch.
#
# Neither vector can be edited into the other, then, and nothing in the code
# says which is which - they are one keystroke apart and they live in different
# files. That is what the tests below are for. If you are reading this because
# one of them failed right after you made the two spellings agree, the failure
# message names which vocabulary you moved; move it back, and change neither.

library(testthat)

# ----- Reaching the package's vector --------------------------------------
# Same route test-vendor-collisions.R takes to the package sources: the app
# lives in shiny/ inside the package repo, so the package's R/ is one level up
# from PMA_APP_ROOT. Skipped rather than failed when it is absent, because the
# app is deployed without it - shinyapps.io gets the staged copy under
# R/_pmatools/ instead, and a bundle-shaped checkout must not fail the suite.
#
# domain_facts.R is sourced into its own environment rather than added to
# helper-app.R's list. Nothing in the suite calls domain_facts(), and dropping
# a second definition of it into the global environment is precisely the
# collision test-vendor-collisions.R exists to catch.
.pkg_dir <- function() file.path(dirname(PMA_APP_ROOT), "R")

.grade_domain_names <- function() {
  env <- new.env(parent = baseenv())
  source(file.path(.pkg_dir(), "domain_facts.R"), local = env)
  get(".GRADE_DOMAIN_NAMES", envir = env)
}

# ----- Reading the app's call sites ---------------------------------------
# The app's own sources: R/*.R is not recursive, so the staged R/_pmatools/
# copy of the package is excluded for free, and app.R is added by name.
.app_sources <- function() {
  c(list.files(file.path(PMA_APP_ROOT, "R"), pattern = "[.][Rr]$",
               full.names = TRUE),
    file.path(PMA_APP_ROOT, "app.R"))
}

# Depth-first walk of one parsed expression, calling `visit` on every node.
#
# The children are taken through as.list() and indexed in place rather than
# bound to a loop variable first. Any `function(a, b = 1)` in the sources puts
# an EMPTY SYMBOL in its formals pairlist - the placeholder for "no default" -
# and binding that to a name makes every later mention of the name raise
# "argument is missing, with no default". Reading it out of a list and
# comparing it to `quote(expr = )` never binds it, so the walk can step over
# it. It holds no string literals, so stepping over it costs nothing.
.walk_ast <- function(node, visit) {
  visit(node)
  if (!(is.call(node) || is.expression(node) || is.pairlist(node))) {
    return(invisible(NULL))
  }
  parts <- as.list(node)
  for (i in seq_along(parts)) {
    if (identical(parts[[i]], quote(expr = ))) next
    .walk_ast(parts[[i]], visit)
  }
  invisible(NULL)
}

# The bare name of a called function, seeing through pkg::fn(). NA for a call
# whose head is computed, which none of the ones below is.
.called_name <- function(node) {
  head <- node[[1]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && as.character(head[[1]])[1] %in% c("::", ":::")) {
    return(as.character(head[[3]]))
  }
  NA_character_
}

# The n-th argument passed WITHOUT a name. The domain always travels
# positionally at these call sites, while `keys` and `flowchart` follow it
# named, so counting unnamed arguments is what keeps the two apart.
.unnamed_arg <- function(node, position) {
  args <- as.list(node)[-1]
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  unnamed <- args[!nzchar(arg_names)]
  if (length(unnamed) < position) return(NULL)
  unnamed[[position]]
}

# Every helper that takes a domain KEY, and where in its signature it takes
# it. Adding a helper here is how a new domain-keyed lookup joins the guard;
# the count test below is what stops one from quietly leaving it.
DOMAIN_KEY_ARG_POSITIONS <- c(
  "domain_facts"             = 2L,
  "domain_judgment"          = 1L,
  "domain_notes"             = 1L,
  "domain_downgrade"         = 1L,
  "domain_fact_table"        = 1L,
  ".domain_evaluation"       = 1L,
  "pma_flowchart_details"    = 1L,
  "step3_append_domain_note" = 2L
)

# Is this node a `<something>$domain` field read? That is the other shape a
# domain key is spelled in: `g$domain_assessments$domain == "Imprecision"`.
.is_domain_field <- function(node) {
  is.call(node) && identical(as.character(node[[1]])[1], "$") &&
    identical(as.character(node[[3]])[1], "domain")
}

# Every domain key the app spells as a literal, each tagged with where it was
# found so a failure names the file and the call rather than just the string.
#
# Keys that arrive through a variable are not collected, because a static scan
# cannot follow them. Three shapes do that today: `domain_fact_table(domain)`
# and `pma_flowchart_details(domain, facts)` inside .domain_evaluation(), and
# `d$domain == domain` inside step3_append_domain_note() - all three are
# parameters, and the literals their callers pass in ARE collected, so the
# guard still covers them. The one genuine gap is
# `for (dom in c("Risk of bias", "Inconsistency", "Imprecision"))` in
# grade_obj(), whose three keys reach step3_append_domain_note() through the
# loop variable. Widening the scan to chase a loop over a literal vector would
# buy those three at the cost of a parser that has to be right about
# dataflow; the nineteen literals it does collect include all five domain
# names several times over, so a case slip anywhere in the app still lands on
# one of them.
.spelled_domain_keys <- function() {
  found <- character(0)
  for (path in .app_sources()) {
    file_label <- basename(path)
    record <- function(literal, context) {
      found <<- c(found, stats::setNames(literal,
                                         paste0(file_label, ": ", context)))
    }
    .walk_ast(parse(path), function(node) {
      if (!is.call(node)) return(invisible(NULL))
      name <- .called_name(node)
      if (is.na(name)) return(invisible(NULL))

      if (name %in% names(DOMAIN_KEY_ARG_POSITIONS)) {
        arg <- .unnamed_arg(node, DOMAIN_KEY_ARG_POSITIONS[[name]])
        if (is.character(arg) && length(arg) == 1L) {
          record(arg, paste0(name, "()"))
        }
        return(invisible(NULL))
      }

      # `x$domain == "literal"`, either way round.
      if (identical(name, "==")) {
        sides <- as.list(node)[-1]
        if (length(sides) != 2L) return(invisible(NULL))
        field <- vapply(sides, .is_domain_field, logical(1))
        if (!any(field)) return(invisible(NULL))
        for (other in sides[!field]) {
          if (is.character(other) && length(other) == 1L) {
            record(other, "$domain == ...")
          }
        }
      }
      invisible(NULL)
    })
  }
  found
}

# A `name <- c("a", "b", ...)` assignment of string literals, read out of the
# app sources without running the server body it sits in.
.assigned_string_vector <- function(target) {
  values <- character(0)
  for (path in .app_sources()) {
    .walk_ast(parse(path), function(node) {
      if (!is.call(node)) return(invisible(NULL))
      if (!as.character(node[[1]])[1] %in% c("<-", "=")) return(invisible(NULL))
      if (!is.symbol(node[[2]]) || !identical(as.character(node[[2]]), target)) {
        return(invisible(NULL))
      }
      rhs <- node[[3]]
      if (!is.call(rhs) || !identical(as.character(rhs[[1]])[1], "c")) {
        return(invisible(NULL))
      }
      parts <- as.list(rhs)[-1]
      if (!all(vapply(parts, is.character, logical(1)))) return(invisible(NULL))
      values <<- c(values, unlist(parts))
      invisible(NULL)
    })
  }
  values
}


test_that("every domain key the app spells is one the package answers to", {
  skip_if_not(dir.exists(.pkg_dir()), "package sources not next to the app")

  package_names <- .grade_domain_names()
  spelled <- .spelled_domain_keys()

  # Not an assertion about the app so much as about this test: if the helpers
  # in DOMAIN_KEY_ARG_POSITIONS get renamed and nobody updates the list, the
  # scan finds nothing and the subset check below passes vacuously. Nineteen
  # literal domain keys are found today; the floor is set well under that so
  # ordinary edits do not trip it, and well over zero so a silently disarmed
  # guard does.
  expect_gt(length(spelled), 5L)

  for (i in seq_along(spelled)) {
    expect_true(spelled[[i]] %in% package_names,
                info = paste0(names(spelled)[i], ' spells "', spelled[[i]],
                              '", which is not a domain key. The package will ',
                              "not match it and the lookup will silently miss. ",
                              "Valid keys: ",
                              paste(package_names, collapse = ", ")))
  }
})


test_that("PMA_DOMAIN_LABELS is a tab-title vocabulary, not a domain-key one", {
  # The positive half of the claim: these strings are tab titles, and the
  # proof is that every one of them names a tab in grade_tab_sequence. That
  # is what makes `selected = PMA_DOMAIN_LABELS[[key]]` land somewhere.
  tab_sequence <- .assigned_string_vector("grade_tab_sequence")
  expect_true(length(tab_sequence) > 0L,
              info = "grade_tab_sequence not found in the app sources")

  for (key in names(PMA_DOMAIN_LABELS)) {
    expect_true(PMA_DOMAIN_LABELS[[key]] %in% tab_sequence,
                info = paste0('PMA_DOMAIN_LABELS[["', key, '"]] is "',
                              PMA_DOMAIN_LABELS[[key]],
                              '", which is not a tab in grade_tab_sequence. ',
                              "These labels are tabPanel values; the jump ",
                              "links select tabs by them, so one that names ",
                              "no tab selects nothing, silently."))
  }

  # And the entry that could never be a domain key whatever the case: the
  # settings tab. It is on this list because the same confirmation checkbox
  # gates it, not because GRADE has a Configuration domain. This test needs no
  # skip guard - every assertion in it reads the app alone, so it still earns
  # its keep in a bundle-shaped checkout with no package sources beside it. The
  # half of the claim that needs the package is in the next test, which skips.
  expect_true("Configuration" %in% unname(PMA_DOMAIN_LABELS))
  expect_true("Configuration" %in% tab_sequence)

  # Six labels for five GRADE domains. The count is the tidiest statement of
  # the mismatch: this vector cannot be the domain-key vector because it has
  # one entry too many, before anyone even looks at the spelling.
  expect_length(PMA_DOMAIN_LABELS, 6L)
})


test_that("the two vocabularies stay deliberately unequal", {
  skip_if_not(dir.exists(.pkg_dir()), "package sources not next to the app")

  package_names <- .grade_domain_names()
  labels <- unname(PMA_DOMAIN_LABELS)

  # Five GRADE domains against the app's six tab titles, and Configuration is
  # the extra one - the half of that claim that needs the package to check.
  expect_length(package_names, 5L)
  expect_false("Configuration" %in% package_names)

  # The exact, intended difference, pinned in both directions. Read the two
  # setdiffs as sentences: the app titles that are not domain keys are the
  # settings tab and the capitalised Risk of Bias; the domain key that is not
  # an app title is the lower-case one. Anything else means someone moved a
  # vocabulary, and which setdiff changed says which one they moved.
  expect_equal(sort(setdiff(labels, package_names)),
               c("Configuration", "Risk of Bias"),
               info = paste("The app's tab titles changed relative to the",
                            "package's domain keys. If you lower-cased",
                            'PMA_DOMAIN_LABELS["rob"], you broke the tab',
                            "titles: that string is a tabPanel value and the",
                            "Step 3 / Step 4 jump links select the tab by it."))

  expect_equal(sort(setdiff(package_names, labels)), "Risk of bias",
               info = paste("The package's domain keys changed relative to the",
                            "app's tab titles. If you capitalised",
                            ".GRADE_DOMAIN_NAMES, you broke the domain keys:",
                            "that string is what domain_facts() validates",
                            "against and what make_domain_row() stores, so",
                            "every app-side lookup now misses its row and",
                            'falls back to "not_serious".'))

  # Said once more without the setdiffs, because this is the line a future
  # editor is most likely to arrive at directly: the two vectors are NOT the
  # same set of strings, and making them the same is the change these tests
  # exist to refuse.
  expect_false(setequal(labels, package_names))
})
