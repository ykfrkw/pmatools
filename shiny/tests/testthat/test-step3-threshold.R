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

test_that(".responder_block() offers a three-way choice defaulting to both", {
  # The presentation is a choice, not a tick-box that is on to start with, and
  # the rating never sees the conversion whichever way it goes. The default is
  # the pairing Core GRADE 6 recommends; what keeps its responder proportion
  # from being defaulted past is the responder_p0_confirm gate, not a quieter
  # default here.
  html <- as.character(.responder_block("SMD"))
  expect_match(html, 'name="sof_presentation"')
  expect_no_match(html, "convert_smd_to_or")
  expect_match(html, 'value="both"[^>]*checked="checked"')
  expect_no_match(html, 'value="effect"[^>]*checked="checked"')
  expect_no_match(html, 'value="responder"[^>]*checked="checked"')
  # All three options are named, and the two that name the measure do.
  expect_match(html, "The SMD itself", fixed = TRUE)
  expect_match(html, "Chinn", fixed = TRUE)
  expect_match(html, 'value="both"')
  expect_match(html, "Both, on two rows of one outcome: the SMD on its own scale",
               fixed = TRUE)
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

# --------------------------------------------------------------------------
# The four clinical questions
# --------------------------------------------------------------------------
# Every summary measure the Configuration tab offers a threshold box for, so
# the copy audits below cover the whole (question x measure) grid rather than
# one cell of it.
PMA_TEST_SM <- c("OR", "RR", "HR", "RoM", "SMD", "MD", "ARD")

# Every string the four helpers and the copy deck can put in front of a
# reviewer, over that whole grid. One function, because the "MID" audit and the
# "is anything empty" audit have to see the same set: a string that escapes one
# escapes both.
pma_all_question_strings <- function() {
  out <- c(
    EDU_COPY$config_tab$question_section,
    EDU_COPY$config_tab$question_label,
    EDU_COPY$config_tab$question_intro,
    EDU_COPY$config_tab$question_labels,
    EDU_COPY$config_tab$question_headings,
    EDU_COPY$config_tab$question_threshold_units,
    unlist(EDU_COPY$question_help, use.names = FALSE),
    PMA_QUESTION_RATIONALE[!is.na(PMA_QUESTION_RATIONALE)],
    PMA_QUESTION_NOTE_QUESTIONS,
    step3_worse_side_sentence("desirable"),
    step3_worse_side_sentence("undesirable"),
    step3_worse_side_sentence(NULL)
  )
  for (q in PMA_CLINICAL_QUESTIONS) {
    for (sm in PMA_TEST_SM) {
      for (sv in list(NULL, "desirable", "undesirable")) {
        cp <- step3_threshold_copy(q, sm, small_values = sv)
        out <- c(out, cp$heading, cp$label, cp$help)
      }
    }
  }
  unname(out)
}

test_that("pma_question_grade_args() is the mapping table and nothing else", {
  # Five names, always, so grade_obj() can c() the result into its argument
  # list without a four-way branch over something this table already answers.
  for (q in PMA_CLINICAL_QUESTIONS) {
    expect_named(pma_question_grade_args(q),
                 c("threshold_type", "rating_target",
                   "rating_target_rationale", "threshold_sides",
                   "plain_language_frame"),
                 info = q)
  }

  # Superiority rates against the null and needs no threshold.
  sup <- pma_question_grade_args("superiority")
  expect_identical(sup$threshold_type, "null")
  expect_null(sup$rating_target)
  expect_null(sup$rating_target_rationale)
  expect_identical(sup$threshold_sides, "both")
  expect_null(sup$plain_language_frame)

  # The default is today's app: threshold_type "mid", Fig 2 derives the target,
  # both sides tested, Box 1's existing wording. Every value that is not NULL
  # here is a value the pre-0.5.1 app also produced.
  imp <- pma_question_grade_args("important_superiority")
  expect_identical(imp$threshold_type, "mid")
  expect_null(imp$rating_target)
  expect_null(imp$rating_target_rationale)
  expect_identical(imp$threshold_sides, "both")
  expect_null(imp$plain_language_frame)

  eq <- pma_question_grade_args("equivalence")
  expect_identical(eq$threshold_type, "mid")
  expect_identical(eq$rating_target, "little_to_no_difference")
  expect_identical(eq$threshold_sides, "both")
  expect_identical(eq$plain_language_frame, "equivalence")

  ni <- pma_question_grade_args("non_inferiority")
  expect_identical(ni$threshold_type, "mid")
  expect_identical(ni$rating_target, "little_to_no_difference")
  expect_identical(ni$threshold_sides, "worse_only")
  expect_identical(ni$plain_language_frame, "non_inferiority")

  # A pinned target is a manual override, and the package makes the rationale
  # mandatory. rating_target and its rationale must travel together or neither:
  # one without the other aborts inside grade_meta().
  for (q in PMA_CLINICAL_QUESTIONS) {
    a <- pma_question_grade_args(q)
    expect_identical(is.null(a$rating_target),
                     is.null(a$rating_target_rationale), info = q)
    if (!is.null(a$rating_target_rationale)) {
      expect_true(nzchar(trimws(a$rating_target_rationale)), info = q)
    }
  }
})

test_that("an unrecognised question resolves to the pre-0.5.1 default", {
  # NULL before the radio has rendered, and a stale value restored from an
  # outcome banked by an older build, both land on the default rather than
  # aborting - the default being the only answer that cannot silently change a
  # rating.
  expect_identical(PMA_CLINICAL_QUESTION_DEFAULT, "important_superiority")
  expect_true(PMA_CLINICAL_QUESTION_DEFAULT %in% PMA_CLINICAL_QUESTIONS)
  for (bad in list(NULL, NA, NA_character_, "", "noninferiority",
                   c("superiority", "equivalence"), 3)) {
    expect_identical(pma_clinical_question(bad), PMA_CLINICAL_QUESTION_DEFAULT)
  }
  for (q in PMA_CLINICAL_QUESTIONS) {
    expect_identical(pma_clinical_question(q), q)
  }
  expect_identical(pma_question_grade_args(NULL),
                   pma_question_grade_args(PMA_CLINICAL_QUESTION_DEFAULT))
})

test_that("every question survives a real grade_meta() round trip", {
  # The point of doing this against a REAL fit rather than a stub: the branch
  # table in pma_question_of() reads fields grade_meta() has to have STORED,
  # and a stub built from the same table proves only that the table agrees with
  # itself.
  skip_if_not_installed("meta")
  pkg <- pma_vendored_pkg()
  skip_if(is.null(pkg), "no staged bundle - run Rscript shiny/stage_bundle.R")

  fit <- meta::metabin(
    event.e = c(10, 12, 9), n.e = c(100, 110, 90),
    event.c = c(20, 22, 19), n.c = c(100, 110, 90),
    sm = "RR", random = TRUE, common = FALSE)

  for (q in PMA_CLINICAL_QUESTIONS) {
    g <- suppressWarnings(do.call(pkg$grade_meta, c(
      list(fit, small_values = "desirable", threshold = 1.20,
           threshold_scale = "ratio", pubias_unpublished = "no"),
      pma_question_grade_args(q))))
    expect_identical(pma_question_of(g), q, info = q)
    # And the object really did carry the arguments, rather than the recovery
    # having guessed right off a default.
    expect_identical(g$threshold_sides,
                     pma_question_grade_args(q)$threshold_sides, info = q)
    expect_identical(g$plain_language_frame,
                     pma_question_grade_args(q)$plain_language_frame, info = q)
  }
})

test_that("pma_question_of() reads a pre-0.5.1 object as the default", {
  # An object rated before the feature carries none of the three new fields, so
  # the recovery has to land on the question the app was asking then.
  expect_identical(pma_question_of(list(threshold_type = "mid")),
                   "important_superiority")
  expect_identical(pma_question_of(list(threshold_type = "null")),
                   "superiority")
  expect_identical(pma_question_of(NULL), PMA_CLINICAL_QUESTION_DEFAULT)
  expect_identical(pma_question_of("not an object"),
                   PMA_CLINICAL_QUESTION_DEFAULT)

  # THE ONE JUDGMENT in that function. A pinned little_to_no_difference with
  # threshold_type "mid" is what an equivalence question looks like AND what a
  # legitimate pre-0.5.1 manual override looks like. Reading it as equivalence
  # would reinterpret somebody's override as a question they never asked and
  # reword their Summary of Findings sentence to match, so it must not.
  legacy_override <- list(threshold_type = "mid",
                          rating_target = "little_to_no_difference",
                          rating_target_auto = FALSE)
  expect_identical(pma_question_of(legacy_override), "important_superiority")

  # worse_only identifies itself without help from any other field.
  expect_identical(pma_question_of(list(threshold_sides = "worse_only")),
                   "non_inferiority")
  # An object with no threshold_type at all, rated against the null: the same
  # fact recovered from the other side.
  expect_identical(pma_question_of(list(rating_target = "non_null_effect",
                                        rating_target_auto = TRUE)),
                   "superiority")
})

test_that("step3_threshold_copy() keeps the default question byte-identical", {
  # A reviewer who never touches the radio must see exactly the tab that was
  # there before, so the label and the note are the existing copy-deck strings
  # and not a rewording of them.
  for (sm in PMA_TEST_SM) {
    cp <- step3_threshold_copy("important_superiority", sm)
    expect_identical(cp$heading, "Decision threshold", info = sm)
    expect_identical(cp$label, EDU_COPY$threshold_labels[[sm]], info = sm)
    expect_identical(cp$help, EDU_COPY$threshold_help[[sm]], info = sm)
    expect_true(cp$prefill, info = sm)
  }
  # An unmapped measure falls back to the same strings output$threshold_panel
  # already used for one.
  odd <- step3_threshold_copy("important_superiority", "IRR")
  expect_identical(odd$label, "Threshold for clinical importance")
  expect_identical(odd$help, "")
})

test_that("step3_threshold_copy() prefills only the two superiority questions", {
  # suggest_threshold() offers a placeholder for a threshold of clinical
  # importance, which belongs to the outcome. A margin belongs to the review's
  # own question, and a reviewer who starts on the default and switches to
  # non-inferiority must not inherit that placeholder as their margin.
  for (sm in PMA_TEST_SM) {
    expect_true(step3_threshold_copy("superiority", sm)$prefill, info = sm)
    expect_true(step3_threshold_copy("important_superiority", sm)$prefill,
                info = sm)
    expect_false(step3_threshold_copy("equivalence", sm)$prefill, info = sm)
    expect_false(step3_threshold_copy("non_inferiority", sm)$prefill, info = sm)
  }
})

test_that("step3_threshold_copy() names the question in its heading and label", {
  expect_identical(step3_threshold_copy("superiority", "RR")$heading,
                   "Decision threshold (optional)")
  expect_identical(step3_threshold_copy("equivalence", "RR")$heading,
                   "Equivalence threshold")
  expect_identical(step3_threshold_copy("non_inferiority", "RR")$heading,
                   "Non-inferiority threshold")

  # A margin input label carries the SCALE and no example value: an example in
  # the label of a box that has no default is a number a reviewer can read as a
  # suggestion.
  for (sm in PMA_TEST_SM) {
    for (q in c("equivalence", "non_inferiority")) {
      lab <- step3_threshold_copy(q, sm)$label
      expect_match(lab, "^(Equivalence|Non-inferiority) threshold \\(",
                   info = paste(q, sm))
      expect_false(grepl("e.g.", lab, fixed = TRUE), info = paste(q, sm))
    }
  }
  expect_identical(step3_threshold_copy("equivalence", "RR")$label,
                   "Equivalence threshold (as a risk ratio above 1)")
  expect_identical(step3_threshold_copy("non_inferiority", "MD")$label,
                   "Non-inferiority threshold (in outcome units)")
})

test_that("the two margin questions send the reviewer to their protocol", {
  # One sentence, not PMA_NO_MARGIN_PLACEHOLDER's three. The rationale for
  # offering no default is provenance -- "where this number comes from" --
  # which shiny/SPEC.md 3.4.11 deletes from muted copy outright; it lives in
  # the package constant and in SPEC 4.7a. What survives on screen is the part
  # a reviewer cannot answer the box without: enter what the protocol says.
  for (q in c("equivalence", "non_inferiority")) {
    for (sm in PMA_TEST_SM) {
      help <- step3_threshold_copy(q, sm)$help
      expect_match(help, "margin your protocol specifies", fixed = TRUE,
                   info = paste(q, sm))
      expect_match(help, "no default", fixed = TRUE, info = paste(q, sm))
      # The rationale itself must NOT be inlined here, or the box grows the
      # wall of text that rule exists to prevent.
      expect_false(grepl(PMA_NO_MARGIN_PLACEHOLDER, help, fixed = TRUE),
                   info = paste(q, sm))
    }
  }
  # And the two superiority questions say neither: there IS a placeholder for
  # a threshold of clinical importance, so both sentences would be false.
  for (q in c("superiority", "important_superiority")) {
    help <- step3_threshold_copy(q, "RR")$help
    expect_false(grepl("no default", help, fixed = TRUE), info = q)
    expect_false(grepl(PMA_NO_MARGIN_PLACEHOLDER, help, fixed = TRUE),
                 info = q)
  }
})

test_that("no margin help text grows back into a wall of prose", {
  # A standing cap, so the next copy edit fails here rather than on screen.
  # shiny/SPEC.md 3.4.11: delete first, shorten second, hide never.
  for (q in PMA_CLINICAL_QUESTIONS) {
    for (sm in PMA_TEST_SM) {
      words <- length(strsplit(trimws(
        step3_threshold_copy(q, sm, small_values = "desirable")$help),
        "\\s+")[[1]])
      # 95, not 70: non-inferiority carries one sentence the others do not
      # (which side is the worse one), and that echo is mandatory -- a
      # one-sided test whose side the reviewer cannot see is a silent exit.
      # The cap is here to catch regrowth, not to squeeze earned content.
      expect_lt(words, 95L, label = paste(q, sm, "help word count"))
    }
  }
})

test_that("the superiority question never mentions a threshold in its label", {
  # On that question a threshold is optional, and a radio label implying
  # otherwise sends a reviewer looking for a protocol value they do not need.
  lab <- EDU_COPY$config_tab$question_labels[["superiority"]]
  expect_identical(lab, "Superiority - is there any effect at all?")
  expect_false(grepl("threshold|margin", lab, ignore.case = TRUE))

  # The other three do name one, because on those three it is the question.
  for (q in c("important_superiority", "equivalence", "non_inferiority")) {
    expect_true(nzchar(EDU_COPY$config_tab$question_labels[[q]]), info = q)
  }

  # All four are prose questions rather than bare names: the reviewer is
  # choosing between questions, and "non-inferiority" alone assumes they
  # already know which one that is.
  for (q in PMA_CLINICAL_QUESTIONS) {
    expect_match(EDU_COPY$config_tab$question_labels[[q]], "\\?$", info = q)
  }
})

test_that("non-inferiority copy states the one-sidedness and names the side", {
  # A one-sided test whose side the reviewer cannot see on screen is a silent
  # exit. The side comes from .threshold_worse_sign(), so the side echoed is
  # the side Imprecision tested.
  hi <- step3_threshold_copy("non_inferiority", "RR",
                             small_values = "desirable")$help
  lo <- step3_threshold_copy("non_inferiority", "RR",
                             small_values = "undesirable")$help
  none <- step3_threshold_copy("non_inferiority", "RR")$help

  for (txt in c(hi, lo, none)) {
    expect_match(txt, "Only the worse side is tested", fixed = TRUE)
  }
  expect_match(hi, "HIGHER values are the worse ones", fixed = TRUE)
  expect_match(lo, "LOWER values are the worse ones", fixed = TRUE)
  # Unanswered is said to be unanswered. The package's fallback is the +1
  # reading, but printing a side off a question nobody has answered is the
  # silent exit this sentence exists to close.
  expect_match(none, "that answer is still missing", fixed = TRUE)
  expect_false(grepl("HIGHER values", none, fixed = TRUE))

  # Equivalence says the opposite, because it tests both.
  eqv <- step3_threshold_copy("equivalence", "RR")$help
  expect_match(eqv, "Both sides are tested", fixed = TRUE)
  expect_false(grepl("worse side", eqv, fixed = TRUE))

  # step3_worse_side_sentence() agrees with the package helper on both signs,
  # rather than re-reading small_values with a rule of its own.
  expect_equal(.threshold_worse_sign("desirable"), 1)
  expect_equal(.threshold_worse_sign("undesirable"), -1)
})

test_that("dropping the worse-side sentence leaves no double space behind", {
  # step3_worse_side_sentence() is empty until Step 2 records a direction, and
  # the parts are joined by a filter for exactly this reason.
  for (q in PMA_CLINICAL_QUESTIONS) {
    for (sm in c("SMD", "MD", "RR")) {
      txt <- step3_threshold_copy(q, sm, small_values = NULL)$help
      expect_false(grepl("  ", txt, fixed = TRUE), info = paste(q, sm))
      expect_identical(txt, trimws(txt), info = paste(q, sm))
    }
  }
})

test_that("PMA_QUESTION_RATIONALE is a constant, and survives analysis.R", {
  expect_setequal(names(PMA_QUESTION_RATIONALE), PMA_CLINICAL_QUESTIONS)
  # NA for the two questions that pin nothing, so a lookup by question can
  # never yield a rationale for a rating that was not overridden.
  expect_true(is.na(PMA_QUESTION_RATIONALE[["superiority"]]))
  expect_true(is.na(PMA_QUESTION_RATIONALE[["important_superiority"]]))

  for (q in c("equivalence", "non_inferiority")) {
    txt <- PMA_QUESTION_RATIONALE[[q]]
    expect_true(nzchar(trimws(txt)), info = q)
    # These become string literals in the exported bundle's analysis.R, where
    # an apostrophe inside a single-quoted argument is a syntax error rather
    # than a typo.
    expect_false(grepl("'", txt, fixed = TRUE), info = q)
    expect_false(grepl('"', txt, fixed = TRUE), info = q)
    # It has to say why the override was made, since that is what
    # .check_override_rationale() exists to capture.
    expect_match(txt, "Fig 2", fixed = TRUE, info = q)
  }
})

test_that("no string a reviewer can read says MID", {
  # A standing guard, and an explicit user instruction rather than a style
  # preference: the internals keep .has_mid() and mid_zone, and the screen says
  # Threshold (shiny/SPEC.md 4.5.1). The audit runs over the whole
  # (question x measure x unit x direction) grid, because a per-measure switch
  # is exactly where one would survive.
  strings <- pma_all_question_strings()
  expect_gt(length(strings), 100L)
  offenders <- strings[grepl("\\bmid\\b", strings, ignore.case = TRUE)]
  expect_identical(offenders, character(0))
  # ... and nothing in the grid is empty or NA, which would hide a string from
  # the audit above rather than pass it.
  expect_false(any(is.na(strings)))
  expect_true(all(nzchar(trimws(strings))))
})

test_that("pma_question_note() is NULL-safe", {
  # It goes into a table footer, so a caller must be able to drop the result
  # straight in. NULL for a NULL object, and for one that was never rated:
  # "Question rated: clinically important superiority" against an unrated stub
  # would be a fabrication, not a default.
  expect_null(pma_question_note(NULL))
  expect_null(pma_question_note(list()))
  expect_null(pma_question_note("not an object"))
  expect_null(pma_question_note(structure(list(certainty = "High"),
                                          class = "pmatools")))
  expect_null(pma_question_note(list(threshold_type = NA_character_)))
  expect_null(pma_question_note(list(threshold_type = "")))
})
