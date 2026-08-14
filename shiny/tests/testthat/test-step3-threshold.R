# The absolute-scale threshold conversion (R/step3_threshold.R).
#
# This is the arithmetic the whole certainty rating hangs on: Risk of Bias,
# Inconsistency and Imprecision all judge against the band derived here, so a
# silent change of sign or of side would move judgments without moving any
# number the reviewer typed.

test_that("step3_ard_equivalence() converts both sides at a fixed p0", {
  eq <- step3_ard_equivalence("RR", abs1000 = 50, base1000 = 180)

  expect_false(is.null(eq))
  expect_equal(eq$p0, 0.18)
  expect_equal(eq$ard, 0.05)
  expect_equal(eq$p1_up, 0.23)
  expect_equal(eq$p1_dn, 0.13)
  expect_true(eq$down_ok)

  # The worked example in the code comment: RR at p0 = 0.18 with ard = 0.05
  # gives 0.23 / 0.18 on the increase side.
  expect_equal(round(eq$ratio_up, 3), 1.278)
  expect_equal(eq$ratio_up, 0.23 / 0.18)
  expect_equal(eq$ratio_dn, 0.13 / 0.18)

  # The mirror of the increase side is NOT the decrease side.
  expect_equal(eq$mirror_ratio, 1 / eq$ratio_up)
  expect_equal(round(1000 * eq$mirror_ard, 0), -39)
  expect_false(isTRUE(all.equal(eq$mirror_ard, -eq$ard)))
})

test_that("step3_ard_equivalence() works on the odds for OR", {
  eq <- step3_ard_equivalence("OR", abs1000 = 50, base1000 = 180)
  expect_equal(eq$ratio_up, (0.23 / 0.77) / (0.18 / 0.82))
  # rr_up / or_up are shown side by side, so they must not be the same number.
  expect_equal(eq$rr_up, 0.23 / 0.18)
  expect_true(eq$or_up > eq$rr_up)
})

test_that("step3_ard_equivalence() rejects unusable inputs", {
  expect_null(step3_ard_equivalence("RR", NULL, 180))
  expect_null(step3_ard_equivalence("RR", 50, NULL))
  expect_null(step3_ard_equivalence("RR", NA, 180))
  expect_null(step3_ard_equivalence("RR", 50, NA))
  expect_null(step3_ard_equivalence("RR", Inf, 180))
  expect_null(step3_ard_equivalence("RR", c(50, 60), 180))
  expect_null(step3_ard_equivalence("RR", 0, 180))     # ard must be > 0
  expect_null(step3_ard_equivalence("RR", 50, 0))      # p0 must be in (0, 1)
  expect_null(step3_ard_equivalence("RR", 50, 1000))
  expect_null(step3_ard_equivalence("RR", 900, 180))   # p0 + ard >= 1
})

test_that("step3_ard_equivalence() marks the decrease side undefined when ard >= p0", {
  eq <- step3_ard_equivalence("RR", abs1000 = 50, base1000 = 30)
  expect_false(eq$down_ok)
  expect_true(is.na(eq$ratio_dn))
  # The increase side is still meaningful.
  expect_equal(eq$ratio_up, 0.08 / 0.03)
})

test_that("step3_directed_threshold() makes the requested side exact", {
  eq <- step3_ard_equivalence("RR", abs1000 = 50, base1000 = 180)

  up <- step3_directed_threshold(eq, "increase")
  expect_equal(up$exact_side, "increase")
  expect_equal(up$approx_side, "decrease")
  expect_equal(1000 * up$exact_ard, 50)
  expect_equal(round(1000 * up$approx_ard, 0), -39)
  expect_true(is.na(up$caveat))

  dn <- step3_directed_threshold(eq, "decrease")
  expect_equal(dn$exact_side, "decrease")
  expect_equal(dn$approx_side, "increase")
  # The point of the whole exercise: the decrease-side conversion is exactly
  # -50 per 1,000, where the mirrored value would have been about -39.
  expect_equal(1000 * dn$exact_ard, -50)
  expect_true(is.na(dn$caveat))

  # pmatools needs threshold_internal = log(ratio) > 0, so the argument passed
  # is always above 1 even when the exact side sits below it.
  expect_true(dn$exact_ratio < 1)
  expect_true(dn$ratio > 1)
  expect_equal(dn$ratio, 1 / dn$exact_ratio)
  expect_true(up$ratio > 1)
  expect_equal(up$ratio, up$exact_ratio)
})

test_that("step3_directed_threshold() edge case: threshold not smaller than p0", {
  # p0 - ard <= 0, so no event rate lies `ard` below the control-group risk.
  eq <- step3_ard_equivalence("RR", abs1000 = 50, base1000 = 30)
  d  <- step3_directed_threshold(eq, "decrease")

  expect_equal(d$exact_side, "increase")   # falls back
  expect_equal(1000 * d$exact_ard, 50)
  expect_false(is.na(d$caveat))
  expect_match(d$caveat, "decrease-side conversion is\\s+undefined")
  expect_match(d$caveat, "The increase side is used instead")
})

test_that("step3_directed_threshold() edge case: pooled effect on the null", {
  eq <- step3_ard_equivalence("RR", abs1000 = 50, base1000 = 180)
  d  <- step3_directed_threshold(eq, "indeterminate")

  expect_equal(d$exact_side, "increase")   # by convention
  expect_equal(1000 * d$exact_ard, 50)
  expect_match(d$caveat, "indistinguishable from")
  expect_match(d$caveat, "made exact by convention")
})

test_that("step3_directed_threshold() edge case: pooled effect unavailable", {
  eq <- step3_ard_equivalence("RR", abs1000 = 50, base1000 = 180)
  d  <- step3_directed_threshold(eq, "unavailable")

  expect_equal(d$exact_side, "increase")
  expect_match(d$caveat, "not available")
  expect_match(d$caveat, "made exact by convention")
})

test_that("step3_directed_threshold() edge case: no baseline at all", {
  # step3_ard_equivalence() returns NULL without a usable p0, and the directed
  # conversion has to pass that NULL through rather than build a bogus band.
  expect_null(step3_directed_threshold(step3_ard_equivalence("RR", 50, NA)))
  expect_null(step3_directed_threshold(NULL))
  expect_null(step3_directed_threshold(NULL, "decrease"))
})

test_that("step3_threshold_direction() reads the side off the TE scale", {
  expect_equal(step3_threshold_direction(0.5), "increase")
  expect_equal(step3_threshold_direction(-0.5), "decrease")
  expect_equal(step3_threshold_direction(0), "indeterminate")
  expect_equal(step3_threshold_direction(STEP3_TE_NULL_TOL), "indeterminate")
  expect_equal(step3_threshold_direction(2 * STEP3_TE_NULL_TOL), "increase")
  expect_equal(step3_threshold_direction(NULL), "unavailable")
  expect_equal(step3_threshold_direction(NA_real_), "unavailable")
  expect_equal(step3_threshold_direction(Inf), "unavailable")
  expect_equal(step3_threshold_direction(c(0.5, 0.6)), "unavailable")
  expect_equal(step3_threshold_direction("0.5"), "unavailable")
})

test_that("step3_pooled_te() prefers the fitted model and falls back", {
  expect_equal(step3_pooled_te(list(random = TRUE,
                                    TE.random = 0.8, TE.common = 0.3)), 0.8)
  expect_equal(step3_pooled_te(list(random = FALSE,
                                    TE.random = 0.8, TE.common = 0.3)), 0.3)
  # Non-finite preferred value -> use the other one.
  expect_equal(step3_pooled_te(list(random = TRUE,
                                    TE.random = NA_real_, TE.common = 0.3)), 0.3)
  expect_true(is.na(step3_pooled_te(NULL)))
  expect_true(is.na(step3_pooled_te(list(random = TRUE))))
})

test_that("step3_threshold_note() states the absolute threshold and the mirror", {
  eq   <- step3_ard_equivalence("RR", abs1000 = 50, base1000 = 180)
  note <- step3_threshold_note(step3_directed_threshold(eq, "decrease"))

  expect_match(note, "^Absolute threshold 50 per 1,000 at a baseline risk 180 per 1,000")
  expect_match(note, "converted on the decrease side")
  expect_match(note, "-50 per 1,000")
  expect_null(step3_threshold_note(NULL))
})

test_that("step3_is_binary_outcome() decides on the object, not the measure", {
  expect_true(step3_is_binary_outcome(structure(list(), class = "metabin")))
  expect_false(step3_is_binary_outcome(structure(list(), class = "metacont")))
  # A rare-events engine returns something that is not metabin but carries the
  # arm-level counts.
  expect_true(step3_is_binary_outcome(list(event.c = c(1, 2), n.c = c(10, 10))))
  # Pre-analysis: fall back to the Step 2 radio.
  expect_true(step3_is_binary_outcome(NULL, "binary"))
  expect_false(step3_is_binary_outcome(NULL, "continuous"))
  expect_false(step3_is_binary_outcome(NULL, NULL))
})

test_that("step3_threshold_suggestions() splits the two scales", {
  # pmatools >= 0.5 shape for a binary ratio measure: absolute at the top
  # level, the ratio candidate nested.
  s <- list(threshold_user = 0.05, threshold_scale = "ard",
            threshold_ratio = list(threshold_user = 1.25,
                                   threshold_scale = "ratio"))
  out <- step3_threshold_suggestions(s)
  expect_equal(out$absolute1000, 50)
  expect_equal(out$relative, 1.25)

  # Flat te-scale shape (SMD / MD).
  out2 <- step3_threshold_suggestions(
    list(threshold_user = 0.2, threshold_scale = "te_scale"))
  expect_equal(out2$relative, 0.2)
  expect_true(is.na(out2$absolute1000))

  # Unsupported measures return NULL, and non-positive / non-finite candidates
  # must not be taken.
  expect_true(all(is.na(unlist(step3_threshold_suggestions(NULL)))))
  expect_true(all(is.na(unlist(step3_threshold_suggestions(
    list(threshold_user = 0, threshold_scale = "ard"))))))
})

test_that("step3_append_domain_note() appends in the ' | ' house style", {
  d <- data.frame(domain = c("Risk of bias", "Imprecision"),
                  notes  = c(NA_character_, "existing"),
                  stringsAsFactors = FALSE)

  expect_equal(step3_append_domain_note(d, "Risk of bias", "new")$notes[1], "new")
  expect_equal(step3_append_domain_note(d, "Imprecision", "new")$notes[2],
               "existing | new")
  # Only the named domain is touched.
  expect_true(is.na(step3_append_domain_note(d, "Imprecision", "new")$notes[1]))
  # No-ops.
  expect_identical(step3_append_domain_note(d, "Nonexistent", "new"), d)
  expect_identical(step3_append_domain_note(d, "Imprecision", NULL), d)
  expect_identical(step3_append_domain_note(d, "Imprecision", ""), d)
  expect_null(step3_append_domain_note(NULL, "Imprecision", "new"))
})

# ---------------------------------------------------------------------------
# Keeping the Configuration widgets in step with the reactiveVals that back
# them. The panel seeds each box from its reactiveVal under isolate(), and
# app.R's provenance guard resets those reactiveVals after the panel has
# already rendered - so without a push the box can show the previous outcome's
# number while the rating uses the current one.
# ---------------------------------------------------------------------------

test_that("step3_widget_sync_value() pushes when the widget has gone stale", {
  # The reported case: the box still holds the previous outcome's pooled
  # control-group risk, the state holds this outcome's.
  expect_equal(step3_widget_sync_value(127, 74.3), 127)
  # Same for a value the reviewer typed for the previous outcome.
  expect_equal(step3_widget_sync_value(1.25, 2.5), 1.25)
  # A box that is empty when the state changes gets the new value.
  expect_equal(step3_widget_sync_value(127, NA_real_), 127)
  expect_equal(step3_widget_sync_value(127, NULL), 127)
})

test_that("step3_widget_sync_value() leaves an agreeing widget alone", {
  # Re-pushing a value the box already shows would move the caret to the end
  # while the reviewer is still typing, so agreement means no message.
  expect_null(step3_widget_sync_value(127, 127))
  expect_null(step3_widget_sync_value(0.2, 0.2))
  expect_null(step3_widget_sync_value(127, 127 + 1e-12))
  # Differences the reviewer could actually have typed are NOT rounding.
  expect_equal(step3_widget_sync_value(127, 127.1), 127)
})

test_that("step3_widget_sync_value() never pushes an unseeded state", {
  # An NA state means "not seeded yet", not "blank the box": the panel falls
  # back to the pooled value / the suggestion on purpose while the seeding
  # observers catch up, and blanking would replace a correct number with none.
  expect_null(step3_widget_sync_value(NA_real_, 127))
  expect_null(step3_widget_sync_value(NULL, 127))
  expect_null(step3_widget_sync_value(numeric(0), 127))
  expect_null(step3_widget_sync_value(c(1, 2), 127))
  expect_null(step3_widget_sync_value(Inf, 127))
  expect_null(step3_widget_sync_value("127", 127))
})

test_that(".responder_block() seeds the proportion box from its argument", {
  # The reviewer's replaced proportion must survive a rebuild of the panel:
  # the block renders whatever the reactiveVal holds, not the constant.
  html <- as.character(.responder_block("SMD", 0.35))
  expect_match(html, 'id="baseline_risk_chinn"[^>]*value="0.35"')

  # Absent / unusable seeds fall back to the app convention rather than
  # rendering an empty or malformed box.
  for (bad in list(NULL, NA_real_, numeric(0), c(0.2, 0.3), Inf, "0.3")) {
    expect_match(as.character(.responder_block("SMD", bad)),
                 sprintf('id="baseline_risk_chinn"[^>]*value="%s"',
                         RESPONDER_P0_DEFAULT))
  }
  expect_match(as.character(.responder_block("SMD")),
               sprintf('id="baseline_risk_chinn"[^>]*value="%s"',
                       RESPONDER_P0_DEFAULT))

  # The rationale / confirm panels still key on the CONSTANT: what obliges a
  # written justification is departing from the app convention, not from
  # whatever was seeded.
  seeded <- as.character(.responder_block("SMD", 0.35))
  expect_match(seeded, sprintf("baseline_risk_chinn != %s",
                               RESPONDER_P0_DEFAULT), fixed = FALSE)
  expect_match(seeded, sprintf("baseline_risk_chinn == %s",
                               RESPONDER_P0_DEFAULT), fixed = FALSE)

  # Measures with no responder conversion have no box to seed.
  expect_no_match(as.character(.responder_block("RoM", 0.35)),
                  "baseline_risk_chinn")
})

test_that(".responder_block() offers a three-way choice defaulting to effect", {
  # The presentation is a choice, not a tick-box that is on to start with:
  # the rating never sees the conversion, so the plain SMD/MD is the default.
  html <- as.character(.responder_block("SMD"))
  expect_match(html, 'name="sof_presentation"')
  expect_no_match(html, "convert_smd_to_or")
  expect_match(html, 'value="effect"[^>]*checked="checked"')
  expect_no_match(html, 'value="responder"[^>]*checked="checked"')
  expect_no_match(html, 'value="both"[^>]*checked="checked"')
  # All three options are named, and the two that name the measure do.
  expect_match(html, "The SMD itself", fixed = TRUE)
  expect_match(html, "Chinn", fixed = TRUE)
  expect_match(html, 'value="both"')
  expect_match(html, "Both, in one row: the SMD on its own scale", fixed = TRUE)
  expect_match(html, "what Core GRADE 6 recommends", fixed = TRUE)
  expect_match(as.character(.responder_block("MD")), "The MD itself",
               fixed = TRUE)

  # Everything the responder route needs hangs off the radio, not the box, and
  # fires for BOTH converting choices: a panel testing only 'responder' would
  # leave a reviewer on 'both' with no way to enter the proportion.
  # htmltools escapes the quotes in the conditionalPanel expression.
  expect_match(html, "input.sof_presentation == &#39;responder&#39;",
               fixed = TRUE)
  expect_match(html, "input.sof_presentation == &#39;both&#39;", fixed = TRUE)
  expect_no_match(html, "input.convert_smd_to_or")

  # A measure with no conversion offers no choice at all.
  expect_no_match(as.character(.responder_block("RoM")), "sof_presentation")
})
