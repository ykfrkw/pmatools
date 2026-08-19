# The app side of the Core GRADE decision flowcharts (R/ui_helpers.R,
# www/flowchart.js, www/shadcn.css).
#
# The package half of the contract - that the ids an assessor emits are ids
# the figure draws - is tested in the package suite
# (tests/testthat/test-flowchart-nodes.R). What is tested here is the other
# half: that the app finds the file, wires the ids onto the wrapper the
# JavaScript reads, and degrades to a sentence rather than an error when the
# figure is missing.

.fig_dir <- local({
  # PMA_APP_ROOT is shiny/; the staged copy and the source copy are the two
  # places a checkout can have them.
  cands <- c(file.path(PMA_APP_ROOT, "_pmatools_inst", "figures"),
             file.path(dirname(PMA_APP_ROOT), "inst", "figures"))
  hit <- Filter(dir.exists, cands)
  if (length(hit)) hit[[1L]] else NA_character_
})

.html <- function(x) paste(as.character(x), collapse = "")

test_that("every flowcharted domain names a figure and an implementation", {
  expect_setequal(names(PMA_FLOWCHART_FIGS),
                  c("Risk of bias", "Inconsistency", "Imprecision",
                    "Publication bias"))
  # Indirectness is deliberately absent: Core GRADE 5 Table 2 is a gradient,
  # not a flowchart, and the subdomain table on that tab stands in for one.
  expect_null(PMA_FLOWCHART_FIGS[["Indirectness"]])

  for (domain in names(PMA_FLOWCHART_FIGS)) {
    src <- pma_algorithm_source(domain)
    expect_true(nzchar(src), info = domain)
    # "アルゴリズムの関数の位置も明示": the caption has to name the function
    # AND the file, or it is not telling the reviewer where to look.
    expect_match(src, "\\(\\)", info = domain)
    expect_match(src, "^Core GRADE .*R/.*\\.R", info = domain)
  }
  expect_null(pma_algorithm_source("Indirectness"))
  expect_null(pma_algorithm_source("Nonsense"))
})

test_that("pma_flow_path_ids() reads the fact and survives everything else", {
  facts <- data.frame(
    key   = c("i2", "flow_path"),
    label = c("I-squared", "Flowchart path"),
    value = c("42.0%", "pma-incon-node-step1 pma-incon-edge-step1-yes"),
    numeric = c(42, NA_real_),
    stringsAsFactors = FALSE)
  expect_identical(pma_flow_path_ids(facts),
                   c("pma-incon-node-step1", "pma-incon-edge-step1-yes"))

  expect_identical(pma_flow_path_ids(NULL), character(0))
  expect_identical(pma_flow_path_ids("not a tibble"), character(0))
  expect_identical(pma_flow_path_ids(facts[facts$key == "i2", ]),
                   character(0))
  blank <- facts
  blank$value[blank$key == "flow_path"] <- "   "
  expect_identical(pma_flow_path_ids(blank), character(0))
})

test_that("pma_facts_list() never shows the machine-only flow_path", {
  facts <- data.frame(
    key   = c("i2", "flow_path"),
    label = c("I-squared", "Flowchart path"),
    value = c("42.0%", "pma-incon-node-step1"),
    numeric = c(42, NA_real_),
    stringsAsFactors = FALSE)
  html <- .html(pma_facts_list(facts))
  expect_match(html, "I-squared")
  expect_false(grepl("pma-incon-node-step1", html, fixed = TRUE))

  # A domain whose only fact is the path has nothing to list.
  expect_null(pma_facts_list(facts[facts$key == "flow_path", ]))
})

test_that("a missing figure yields a sentence, not an error", {
  out <- pma_flowchart("rob", dir = file.path(tempdir(), "no-such-dir"))
  html <- .html(out)
  expect_match(html, "pma-flowchart-missing")
  expect_false(grepl("<svg", html, fixed = TRUE))

  # And so does a figure key nobody drew.
  expect_match(.html(pma_flowchart("not-a-figure", dir = .fig_dir)),
               "pma-flowchart-missing")
})

test_that("the figure is inlined and the path lands on the wrapper", {
  skip_if(is.na(.fig_dir), "no figures directory in this checkout")
  ids  <- c("pma-rob-node-dominance", "pma-rob-edge-dominance-yes")
  html <- .html(pma_flowchart("rob", on_ids = ids,
                              caption = "Implemented by assess_rob().",
                              dir = .fig_dir))

  expect_match(html, "class=\"pma-flowchart\"")
  # Inlined, not <img src>: the highlighter has to reach inside it.
  expect_match(html, "<svg", fixed = TRUE)
  expect_false(grepl("<img", html, fixed = TRUE))
  # The contract with www/flowchart.js, in one attribute.
  expect_match(html,
               'data-pma-path="pma-rob-node-dominance pma-rob-edge-dominance-yes"',
               fixed = TRUE)
  expect_match(html, "Implemented by assess_rob\\(\\)")
})

test_that("an unhighlighted chart is a valid state, not a missing one", {
  skip_if(is.na(.fig_dir), "no figures directory in this checkout")
  html <- .html(pma_flowchart("incon", dir = .fig_dir))
  expect_match(html, 'data-pma-path=""', fixed = TRUE)
  expect_match(html, "<svg", fixed = TRUE)
})

test_that("the SVGs carry no inline colour, so the app's tokens apply", {
  skip_if(is.na(.fig_dir), "no figures directory in this checkout")
  for (key in c("rob", "incon", "impre", "pubias")) {
    txt <- paste(readLines(file.path(.fig_dir, paste0(key, ".svg")),
                           warn = FALSE), collapse = "\n")
    # A style= / fill= / stroke= on a shape would beat the CSS tokens and
    # freeze the chart to one theme.
    expect_false(grepl("<(rect|path|text)[^>]*\\sstyle=", txt, perl = TRUE),
                 info = key)
    expect_false(grepl("<(rect|path|text)[^>]*\\s(fill|stroke)=", txt,
                       perl = TRUE), info = key)
    # No width/height on the root, or it cannot scale to the card.
    expect_false(grepl("<svg[^>]*\\swidth=", txt, perl = TRUE), info = key)
    expect_match(txt, "aria-labelledby=")
  }
})

test_that("pma_flowchart_details() renders open, with the chart inside", {
  skip_if(is.na(.fig_dir), "no figures directory in this checkout")
  facts <- data.frame(
    key = "flow_path", label = "Flowchart path",
    value = "pma-pubias-node-q1 pma-pubias-edge-q1-yes",
    numeric = NA_real_, stringsAsFactors = FALSE)

  html <- .html(pma_flowchart_details("Publication bias", facts,
                                      dir = .fig_dir))
  expect_match(html, "<details", fixed = TRUE)
  expect_match(html, "pma-flowchart-details")
  # Open by default: it answers the question the verdict above it raises.
  expect_match(html, "open", fixed = TRUE)
  expect_match(html, "Which path did this assessment take\\?")
  expect_match(html, "assess_pubias\\(\\)")
  expect_match(html,
               'data-pma-path="pma-pubias-node-q1 pma-pubias-edge-q1-yes"',
               fixed = TRUE)

  # Indirectness has no chart, so the whole block is absent rather than empty.
  expect_null(pma_flowchart_details("Indirectness", facts, dir = .fig_dir))
})

test_that("www/flowchart.js is loaded by Step 3 and is idempotent by design", {
  js <- readLines(file.path(PMA_APP_ROOT, "www", "flowchart.js"), warn = FALSE)
  txt <- paste(js, collapse = "\n")
  # Bound once per page, exactly as required-fields.js is: Step 3's body is
  # rebuilt by renderUI, so this file re-executes and must not stack
  # listeners.
  expect_match(txt, "pmaFlowchartBound", fixed = TRUE)
  expect_match(txt, "shiny:value", fixed = TRUE)
  # Scoped lookup: ids inside an inlined SVG are document-global, so the
  # search has to start at the wrapper, not at `document`.
  expect_match(txt, "wrap.querySelector(", fixed = TRUE)
  expect_false(any(grepl("^[^/*]*getElementById", js, perl = TRUE)))
  # Clears before painting, so a re-render cannot leave a stale highlight.
  expect_match(txt, "classList.remove", fixed = TRUE)

  step3 <- paste(readLines(file.path(PMA_APP_ROOT, "R", "step3_grade.R"),
                           warn = FALSE), collapse = "\n")
  expect_match(step3, 'src = "flowchart.js"', fixed = TRUE)
})

test_that("the flowchart CSS outranks the style block inside the SVG", {
  css <- paste(readLines(file.path(PMA_APP_ROOT, "www", "shadcn.css"),
                         warn = FALSE), collapse = "\n")
  # The inlined <style> comes later in document order, so an app rule at
  # equal specificity would LOSE. Every rule that has to win therefore
  # carries the .pma-flowchart wrapper class as well.
  for (sel in c(".pma-flowchart .pma-fc-node rect",
                ".pma-flowchart .pma-fc-on.pma-fc-node rect",
                ".pma-flowchart .pma-fc-on.pma-fc-edge path")) {
    expect_match(css, sel, fixed = TRUE)
  }
  # ...and does it without reaching for !important. Checked rule by rule
  # rather than over a fixed window of characters: the flowchart block is not
  # the last thing in the file, and a character count runs straight into the
  # pre-existing .btn-primary rules further down, which do use !important and
  # are none of this test's business.
  # Comments go first, or the block comment above these rules matches on both
  # ".pma-flowchart" and the "!important" it is promising not to use.
  bare  <- gsub("/\\*.*?\\*/", "", css)
  rules <- strsplit(bare, "}", fixed = TRUE)[[1L]]
  flow_rules <- rules[grepl(".pma-flowchart", rules, fixed = TRUE)]
  expect_true(length(flow_rules) > 0)
  expect_false(any(grepl("!important", flow_rules, fixed = TRUE)))
  # The highlight is not carried by colour alone; some reviewers print these.
  expect_match(css, "stroke-width: 2.5", fixed = TRUE)
})

test_that("the Risk of bias caption says what in the chart is not the source's", {
  # The Fig 2 drawing gave up its footnote when it was redrawn to the source's
  # shape (SPEC.md 5.1a). That footnote was the only place in the picture that
  # said the five direction rules and the two-level rule 5 are pmatools' own,
  # and a chart that looks more like the paper needs that said MORE, not less.
  # The caption beside it is where it moved, so a caption that loses it again
  # leaves a reviewer reading a departure as the source.
  caption <- pma_algorithm_source("Risk of bias")
  expect_match(caption, "Core GRADE 4 Fig 2", fixed = TRUE)
  expect_match(caption, "assess_rob()", fixed = TRUE)
  expect_match(caption, "not the source's", fixed = TRUE)
  expect_match(caption, "rule 5 rates down two levels", fixed = TRUE)

  # A chart that departs from its source in nothing says nothing extra, so the
  # sentence stays a signal rather than boilerplate every caption carries.
  plain <- pma_algorithm_source("Imprecision")
  expect_match(plain, "Core GRADE 2 Fig 4", fixed = TRUE)
  expect_false(grepl("not the source's", plain, fixed = TRUE))

  # An unknown domain still has no caption at all.
  expect_null(pma_algorithm_source("Indirectness"))
})
