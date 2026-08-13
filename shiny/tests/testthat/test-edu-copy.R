# edu_domain_how() (R/educational_copy.R) is the single accessor for the
# "How is this judged?" copy. It exists because `how` has two shapes - a
# finished string for four domains, a function for Risk of Bias - and callers
# used to have to know which. The test locks the contract: a string comes back
# either way.

test_that("edu_domain_how() returns a string for every rated domain", {
  domains <- c("rob", "inconsistency", "indirectness", "imprecision", "pubias")
  for (d in domains) {
    how <- edu_domain_how(d)
    expect_type(how, "character")
    expect_length(how, 1L)
    expect_true(nzchar(how), info = d)
  }
})

test_that("edu_domain_how() interpolates the Risk of Bias arguments", {
  expect_match(edu_domain_how("rob", 0.10, "high"), "10 percent")
  expect_match(edu_domain_how("rob", 0.25, "high"), "25 percent")
  # The low/high boundary switches which sentence is used.
  expect_match(edu_domain_how("rob", 0.10, "high"),
               "only studies explicitly", fixed = TRUE)
  expect_match(edu_domain_how("rob", 0.10, "low"),
               "count as low, together with studies rated low", fixed = TRUE)
})

test_that("edu_domain_how() ignores arguments a string domain does not take", {
  # This is the whole point: a call site does not have to change when a
  # domain's copy gains or loses an interpolated slot.
  expect_identical(edu_domain_how("imprecision"),
                   edu_domain_how("imprecision", 0.10, "high"))
})

test_that("the Risk of Bias copy states the one-level cap on rule 5", {
  # The copy said "(5) zones differ across the null (above <-> below) -> rate
  # down 2" for two releases after .assess_bias_direction() stopped doing it.
  # The enumeration itself now lives in the flowchart above the copy
  # (inst/figures/rob.svg, whose rule 5 row reads "rate down 1", pinned by
  # the package's test-flowchart-nodes.R), so what is guarded here is the
  # sentence that explains WHY there is no second level - the part a diagram
  # cannot carry.
  how <- edu_domain_how("rob", 0.10, "high")
  expect_match(how, "Rule 5 rated down 2 up to pmatools 0.4 and no longer does",
               fixed = TRUE)
  expect_false(grepl("-> rate down 2", how, fixed = TRUE))
  # And it explains the cap the way .ROB_CAP_NOTE does, rather than leaving
  # the reader to wonder where the second level went.
  expect_match(how, "no automatic two-level downgrade for risk of bias",
               fixed = TRUE)
})

test_that("every rated domain still carries its Core GRADE reference", {
  for (d in c("rob", "inconsistency", "indirectness", "imprecision", "pubias")) {
    entry <- EDU_COPY$domains[[d]]
    expect_true(nzchar(entry$header), info = d)
    expect_true(nzchar(entry$doi), info = d)
    expect_true(nzchar(entry$ref_text), info = d)
  }
})

test_that("the domain-confirmation labels match the state keys they name", {
  # PMA_DOMAIN_LABELS is keyed by the names of state$domain_confirmed; Step 3
  # writes it and Step 4's export gate reads it, so a drifting key silently
  # opens the gate.
  expect_setequal(names(PMA_DOMAIN_LABELS),
                  c("threshold", "rob", "inconsistency", "indirectness",
                    "imprecision", "pubias"))
  expect_equal(pma_unconfirmed_domains(NULL), unname(PMA_DOMAIN_LABELS))
  expect_equal(pma_unconfirmed_domains(c(threshold = TRUE, rob = TRUE,
                                         inconsistency = TRUE,
                                         indirectness = TRUE,
                                         imprecision = TRUE, pubias = TRUE)),
               character(0))
  expect_equal(pma_unconfirmed_domains(c(threshold = TRUE)),
               unname(PMA_DOMAIN_LABELS[c("rob", "inconsistency",
                                          "indirectness", "imprecision",
                                          "pubias")]))
})
