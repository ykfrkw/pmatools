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
