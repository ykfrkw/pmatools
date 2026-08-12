# step3_grade.R - Step 3: GRADE 5-domain assessment + Final certainty (sub-tabs)

# Map a suggest_threshold() return onto the two threshold reactiveVals used by
# the Configuration tab.
#
# pmatools >= 0.5 leads with the ABSOLUTE candidate for binary ratio measures
# (OR/RR/HR): the top level is threshold_user 0.05 / threshold_scale "ard", and
# the ratio-scale value (e.g. 1.25 for OR) sits in $threshold_ratio. Other
# measures return a flat list whose $threshold_scale is "ratio" (RoM), "ard"
# (ARD) or "te_scale" (SMD, MD); unsupported measures return NULL.
#
# Returns list(relative =, absolute1000 =): `relative` feeds threshold_state()
# (the ratio / te-scale input) and `absolute1000` feeds threshold_abs_state()
# (events per 1,000). Either element is NA when the object offers no candidate
# on that scale.
step3_threshold_suggestions <- function(s) {
  out <- list(relative = NA_real_, absolute1000 = NA_real_)
  if (!is.list(s)) return(out)

  .candidate <- function(cand) {
    if (!is.list(cand)) return(NULL)
    v <- cand$threshold_user
    if (is.null(v) || length(v) != 1L || !is.numeric(v) ||
        !is.finite(v) || v <= 0) {
      return(NULL)
    }
    list(value = v, scale = cand$threshold_scale %||% "")
  }

  cands <- list(.candidate(s),
                .candidate(s$threshold_absolute),
                .candidate(s$threshold_ratio))
  for (cand in cands) {
    if (is.null(cand)) next
    if (identical(cand$scale, "ard")) {
      if (is.na(out$absolute1000)) out$absolute1000 <- 1000 * cand$value
    } else if (is.na(out$relative)) {
      out$relative <- cand$value
    }
  }
  out
}

# Does this analysis have an absolute (event-rate) scale at all?
#
# BUG FIX: the Configuration panel used to branch on `sm %in% c("OR", "RR")`,
# so a binary outcome analysed as ARD, RD or HR fell through to the
# continuous branch and lost the absolute-scale interface entirely - even
# though its control-arm event data is exactly what that interface needs.
# The question is about the outcome, not about the summary measure, so the
# class of the meta object decides: metabin always carries event.c / n.c,
# metacont never does. `outcome_type` (the Step 2 radio, mirrored into
# input$outcome_type) is only the fallback for the pre-analysis state.
step3_is_binary_outcome <- function(obj, outcome_type = NULL) {
  if (!is.null(obj)) {
    if (inherits(obj, "metabin")) return(TRUE)
    if (inherits(obj, "metacont")) return(FALSE)
    # Rare-event engines return objects that are not metabin but still carry
    # arm-level counts; those are binary too.
    if (!is.null(obj$event.c) && !is.null(obj$n.c) &&
        length(obj$event.c) > 0 && length(obj$n.c) > 0) {
      return(TRUE)
    }
  }
  identical(outcome_type, "binary")
}

# Pooled control-group risk, with the fallback made visible.
#
# The number the reviewer sees is a meta::metaprop random-effects pooled
# proportion (GLMM, logit link, back-transformed) via the vendored
# .compute_control_risk(). That function falls back to the crude
# sum(event.c) / sum(n.c) with a warning when the GLMM fails; the warning is
# captured here rather than swallowed, so the UI can say which of the two it
# is actually showing. A crude ratio must never be presented as pooled.
#
# Up to pmatools 0.5.0 the input had to be sanitised to complete
# (event.c, n.c) pairs before calling in, because .compute_control_risk()
# dropped NA events but not the matching denominators and metaprop() then
# errored on every such dataset. pmatools 0.5.1 filters both vectors on one
# complete-case predicate, so the meta object is now passed through untouched.
# `keep` survives only to report how many studies the estimate rests on.
#
# Known limitation, stated not fixed: .compute_control_risk() returns a bare
# scalar and discards metaprop's confidence interval, so the uncertainty in
# the pooled control risk cannot be shown alongside it.
step3_control_risk <- function(meta_obj) {
  out <- list(value = NA_real_, method = "none", crude = NA_real_,
              k_used = 0L, k_dropped = 0L)
  if (is.null(meta_obj)) return(out)
  ec <- meta_obj$event.c
  nc <- meta_obj$n.c
  if (is.null(ec) || is.null(nc) || length(ec) != length(nc) ||
      length(nc) == 0) {
    return(out)
  }
  keep <- !is.na(ec) & !is.na(nc) & nc > 0
  if (!any(keep)) return(out)
  out$k_used    <- sum(keep)
  out$k_dropped <- sum(!keep)
  crude <- .compute_control_risk(meta_obj, method = "simple")
  out$crude <- if (is.null(crude)) NA_real_ else crude

  fell_back <- FALSE
  val <- withCallingHandlers(
    tryCatch(.compute_control_risk(meta_obj, method = "metaprop"),
             error = function(e) NULL),
    warning = function(w) {
      if (grepl("metaprop", conditionMessage(w), fixed = TRUE)) {
        fell_back <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )
  if (is.null(val) || length(val) != 1L || !is.finite(val) ||
      val <= 0 || val >= 1) {
    return(out)
  }
  out$value  <- val
  out$method <- if (fell_back) "simple_fallback" else "metaprop"
  out
}

# Event rate <-> ratio at a fixed control-group risk. Kept at file scope
# because the equivalence table, the directed conversion and the notes all
# need the same two maps; OR works on the odds, everything else on the risk.
step3_ratio_from_p1 <- function(sm, p0, p1) {
  if (identical(sm, "OR")) (p1 / (1 - p1)) / (p0 / (1 - p0)) else p1 / p0
}
step3_p1_from_ratio <- function(sm, p0, ratio) {
  if (identical(sm, "OR")) {
    odds <- (p0 / (1 - p0)) * ratio
    odds / (1 + odds)
  } else {
    p0 * ratio
  }
}

# Both directions of the absolute threshold, plus the mirror of the increase
# side.
#
# The rating algorithm works on the log scale with a symmetric
# +/- threshold_internal, so whichever ratio it is given, the opposite side
# it applies is that ratio inverted. Inverting the increase-side ratio T does
# NOT give the ratio implied by p0 - ard: e.g. RR with p0 = 0.18 and
# ard = 0.05 gives T = 0.23 / 0.18 = 1.278 on the increase side, whose mirror
# 1 / 1.278 = 0.782 implies p1 = 0.141, an absolute difference of -0.039
# rather than -0.050. The gap is larger for OR. `mirror_*` below is that
# mirror of the increase side; step3_directed_threshold() decides which of
# the two sides the app makes exact.
#
# Returns NULL unless p0 and the threshold are usable. `down_ok` is FALSE
# when p0 - ard would leave the (0, 1) interval, in which case only the
# increase side and the mirror are meaningful.
step3_ard_equivalence <- function(sm, abs1000, base1000) {
  if (is.null(abs1000) || is.null(base1000)) return(NULL)
  if (length(abs1000) != 1L || length(base1000) != 1L) return(NULL)
  if (is.na(abs1000) || is.na(base1000)) return(NULL)
  if (!is.finite(abs1000) || !is.finite(base1000)) return(NULL)
  p0  <- base1000 / 1000
  ard <- abs1000 / 1000
  if (p0 <= 0 || p0 >= 1 || ard <= 0) return(NULL)
  p1_up <- p0 + ard
  if (p1_up >= 1) return(NULL)
  p1_dn <- p0 - ard

  .ratio  <- function(p1)    step3_ratio_from_p1(sm, p0, p1)
  # Invert a ratio back to an event rate at the same p0 (the side the
  # algorithm mirrors).
  .invert <- function(ratio) step3_p1_from_ratio(sm, p0, ratio)

  ratio_up     <- .ratio(p1_up)
  mirror_ratio <- 1 / ratio_up
  mirror_p1    <- .invert(mirror_ratio)

  list(
    sm            = sm,
    p0            = p0,
    ard           = ard,
    p1_up         = p1_up,
    ratio_up      = ratio_up,
    down_ok       = p1_dn > 0,
    p1_dn         = p1_dn,
    ratio_dn      = if (p1_dn > 0) .ratio(p1_dn) else NA_real_,
    mirror_ratio  = mirror_ratio,
    mirror_p1     = mirror_p1,
    mirror_ard    = mirror_p1 - p0,
    # RR / OR equivalents of the increase side, shown side by side so the
    # reader can see how much the choice of summary measure matters.
    rr_up         = p1_up / p0,
    or_up         = (p1_up / (1 - p1_up)) / (p0 / (1 - p0))
  )
}

# --------------------------------------------------------------------------
# Directed conversion of the absolute threshold
# --------------------------------------------------------------------------
# grade_meta() takes one scalar threshold and every domain judges against the
# symmetric band +/- threshold_internal, so exactly one of the two sides can
# be exact on the absolute scale. Which one matters is settled by where the
# pooled effect lies: that is the crossing Risk of Bias, Inconsistency and
# Imprecision actually turn on. So the app converts the absolute threshold on
# that side and passes the result as threshold_scale = "ratio", instead of
# handing pmatools the ARD (which always converts on the increase side).
#
# Effects closer to the null than this tolerance on the TE (log) scale have no
# meaningful direction; the increase side is then made exact by convention and
# the UI says so.
STEP3_TE_NULL_TOL <- 1e-6

step3_threshold_direction <- function(te_point) {
  if (is.null(te_point) || length(te_point) != 1L || !is.numeric(te_point) ||
      is.na(te_point) || !is.finite(te_point)) {
    return("unavailable")
  }
  if (abs(te_point) <= STEP3_TE_NULL_TOL) return("indeterminate")
  if (te_point < 0) "decrease" else "increase"
}

# Pooled TE of a meta object, on the TE (log) scale. Mirrors the vendored
# .pooled_te() but is defined here so the app does not depend on an internal.
step3_pooled_te <- function(obj) {
  if (is.null(obj)) return(NA_real_)
  te <- if (isTRUE(obj$random)) obj$TE.random else obj$TE.common
  if (is.null(te) || length(te) == 0L || !all(is.finite(te))) {
    te <- if (isTRUE(obj$random)) obj$TE.common else obj$TE.random
  }
  if (is.null(te) || length(te) == 0L) return(NA_real_)
  as.numeric(te)[1]
}

# The ratio to pass to grade_meta(threshold =, threshold_scale = "ratio").
#
# `eq` is step3_ard_equivalence(); `direction` is step3_threshold_direction().
# Returns NULL when `eq` is unusable. Fields:
#   ratio        value to pass; always > 1, so threshold_internal =
#                log(ratio) > 0 as pmatools requires. On the decrease side
#                that is 1 / T_down, i.e. threshold_internal = |log(T_down)|.
#   exact_side   "increase" or "decrease" - the side that is exact per 1,000
#   exact_ratio  the equivalent effect measure on the exact side (< 1 when
#                the exact side is the decrease side)
#   exact_p1 / exact_ard     event rate and absolute difference it implies
#   approx_ratio / approx_p1 / approx_ard   the mirrored, opposite side
#   caveat       why the requested direction was not honoured, or NA
step3_directed_threshold <- function(eq, direction = "increase") {
  if (is.null(eq)) return(NULL)
  sm <- eq$sm
  p0 <- eq$p0
  caveat <- NA_character_
  want_down <- identical(direction, "decrease")

  # Edge case 1: p0 - ard <= 0. The decrease-side conversion is undefined
  # (no event rate is `ard` per 1,000 below the control-group risk), so the
  # increase side is used and the decrease side stays the mirrored value.
  if (want_down && !isTRUE(eq$down_ok)) {
    caveat <- sprintf(paste0(
      "The pooled effect is below the null, but the threshold (%g per 1,000) ",
      "is not smaller than the control-group risk (%g per 1,000), so no event ",
      "rate lies that far below it and the decrease-side conversion is ",
      "undefined. The increase side is used instead; the decrease side ",
      "remains the mirrored approximation."),
      1000 * eq$ard, 1000 * p0)
    want_down <- FALSE
  }
  # Edge case 2: the pooled effect sits on the null (or is unavailable), so
  # neither side is the one that decides the judgments.
  if (identical(direction, "indeterminate")) {
    caveat <- paste0(
      "The pooled effect is at (or indistinguishable from) the null, so ",
      "neither direction is the one the judgments turn on. The increase side ",
      "is made exact by convention.")
  } else if (identical(direction, "unavailable")) {
    caveat <- paste0(
      "The pooled effect is not available, so the direction could not be ",
      "read from it. The increase side is made exact by convention.")
  }

  exact_ratio <- if (want_down) eq$ratio_dn else eq$ratio_up
  if (is.null(exact_ratio) || !is.finite(exact_ratio) || exact_ratio <= 0) {
    return(NULL)
  }
  ratio_arg    <- if (exact_ratio < 1) 1 / exact_ratio else exact_ratio
  approx_ratio <- 1 / exact_ratio
  approx_p1    <- step3_p1_from_ratio(sm, p0, approx_ratio)
  exact_p1     <- step3_p1_from_ratio(sm, p0, exact_ratio)

  list(
    sm           = sm,
    p0           = p0,
    ard          = eq$ard,
    direction    = direction,
    exact_side   = if (want_down) "decrease" else "increase",
    approx_side  = if (want_down) "increase" else "decrease",
    ratio        = ratio_arg,
    exact_ratio  = exact_ratio,
    exact_p1     = exact_p1,
    exact_ard    = exact_p1 - p0,
    approx_ratio = approx_ratio,
    approx_p1    = approx_p1,
    approx_ard   = approx_p1 - p0,
    caveat       = caveat
  )
}

# One sentence stating, in absolute terms, the threshold that was used and
# which side of it is exact. This replaces the pmatools $threshold_note that
# threshold_scale = "ard" used to produce: with threshold_scale = "ratio" the
# package no longer knows the absolute value, so the app has to say it, or the
# Evidence Profile footnote and the domain notes would lose the provenance.
step3_threshold_note <- function(dir) {
  if (is.null(dir)) return(NULL)
  sm <- dir$sm
  note <- sprintf(paste0(
    "Absolute threshold %g per 1,000 at a baseline risk %g per 1,000, ",
    "converted on the %s side, where it is exact (equivalent %s %.3f: ",
    "%.0f -> %.0f per 1,000, %+.0f per 1,000). Domains judge against the ",
    "symmetric band +/- log(%.3f), so the %s side is the mirrored value ",
    "%s %.3f, implying %+.0f per 1,000 rather than %+.0f"),
    1000 * dir$ard, 1000 * dir$p0, dir$exact_side, sm, dir$exact_ratio,
    1000 * dir$p0, 1000 * dir$exact_p1, 1000 * dir$exact_ard,
    dir$ratio, dir$approx_side, sm, dir$approx_ratio,
    1000 * dir$approx_ard, -1000 * dir$exact_ard)
  if (!is.na(dir$caveat)) note <- paste0(note, ". ", sub("[.]$", "", dir$caveat))
  note
}

# Append a sentence to ONE domain's notes, in the " | " style the vendored
# .append_domain_note() uses across the whole table. Needed because the app
# now writes the threshold note itself (see grade_obj()).
step3_append_domain_note <- function(d, domain, note) {
  if (is.null(d) || is.null(note) || !length(note) || !nzchar(note)) return(d)
  idx <- which(d$domain == domain)
  if (!length(idx)) return(d)
  d$notes[idx] <- ifelse(is.na(d$notes[idx]), note,
                         paste0(d$notes[idx], " | ", note))
  d
}

# --------------------------------------------------------------------------
# Indirectness subdomains (Core GRADE 5 / pmatools indirectness_subdomains)
# --------------------------------------------------------------------------
# The four PICO questions are inputs to grade_meta(indirectness_subdomains =),
# so the UI and the server have to agree on the input ids, the subdomain
# labels pmatools expects, and the 4-point answer scale. File scope, because
# both step3_ui() and step3_server() read them.
#
# Asking the indirectness question per PICO element is Core GRADE 5's; the
# 4-point scale below and the worst-case fold pmatools applies to it are
# pmatools conventions (see EDU_COPY$domains$indirectness$gradient).
STEP3_INDIR_SUBDOMAINS <- c(
  Population   = "indir_population",
  Intervention = "indir_intervention",
  Comparison   = "indir_comparator",
  Outcome      = "indir_outcome"
)

STEP3_INDIR_ANSWERS <- c(
  "Yes"          = "yes",
  "Probably yes" = "probably_yes",
  "Probably no"  = "probably_no",
  "No"           = "no"
)

# pmatools maps the 4-point answer onto a GRADE level; mirrored here so the
# app can tell whether an overall rating restates the worst case (no rationale
# needed) or overrides it (rationale required) without a second grade_meta()
# call.
STEP3_INDIR_ANSWER_TO_LEVEL <- c(
  yes          = "no",
  probably_yes = "no",
  probably_no  = "some_concerns",
  no           = "serious"
)

# Sub-tab navigation inside Step 3. File scope rather than local to
# step3_ui(), because step3_server() re-renders the Final certainty copy of
# it whenever the confirmation state changes (see output$grade_nav_final).
.grade_nav <- function(back_id, back_label, next_id, next_label = "Next",
                       next_disabled = FALSE) {
  htmltools::div(
    style = paste(
      "display: flex;",
      "justify-content: space-between;",
      "margin-top: 1.5rem;"),
    shiny::actionButton(back_id, back_label,
      class = "btn btn-secondary"),
    # See pma_wizard_nav(): TRUE / NULL, never a string, never FALSE.
    shiny::actionButton(next_id, next_label,
      class = "btn btn-primary",
      disabled = if (isTRUE(next_disabled)) TRUE else NULL)
  )
}

step3_ui <- function() {
  s <- EDU_COPY$steps$step3

  .domain_header <- function(name, badge_id, chip_id) {
    htmltools::div(
      class = "pma-accordion-title",
      style = "margin-bottom: 0.5rem;",
      htmltools::h4(class = "pma-domain-name",
                    style = "margin: 0; font-size: 1.1rem;", name),
      shiny::uiOutput(badge_id, inline = TRUE),
      shiny::uiOutput(chip_id,  inline = TRUE)
    )
  }

  .inputs_details <- function(..., title = "Inputs for this domain", open = TRUE) {
    htmltools::tags$details(
      open = if (isTRUE(open)) NA else NULL,
      htmltools::tags$summary(title),
      htmltools::div(...)
    )
  }
  .override_details <- function(..., title = "Override final judgment") {
    htmltools::tags$details(
      htmltools::tags$summary(title),
      htmltools::div(...)
    )
  }
  # Rationale textarea shown only while the paired override select has a
  # non-empty value. A written rationale is mandatory for manual overrides
  # (pmatools v0.4.0 breaking change; Core GRADE transparency principle).
  .override_rationale <- function(select_id, rationale_id) {
    shiny::conditionalPanel(
      sprintf("(input['%s'] || '') != ''", select_id),
      shiny::textAreaInput(
        rationale_id,
        "Rationale (required for override)",
        rows = 2, width = "100%",
        placeholder = "State why the automated assessment was replaced."
      )
    )
  }
  # Explicit per-domain confirmation checkbox (output gate W4-A). Checking
  # it marks the domain as reviewed even if no substantive input was given.
  .confirm_checkbox <- function(id,
                                label = "I have reviewed this domain") {
    htmltools::div(
      style = paste(
        "margin-top: 1rem; padding: 0.5rem 0.75rem;",
        "border: 1px dashed hsl(var(--border)); border-radius: 6px;"),
      shiny::checkboxInput(id, label, value = FALSE, width = "100%")
    )
  }

  # Forest plot display panel (used in RoB and Inconsistency tabs)
  .forest_display_panel <- function(prefix) {
    htmltools::tags$details(
      style = "margin-top: 0.5rem;",
      htmltools::tags$summary("Forest plot display"),
      htmltools::div(
        class = "pma-display-grid",
        htmltools::div(class = "pma-span-4",
          shiny::textInput(paste0(prefix, "_title"), "Title", value = "", width = "100%")),
        shiny::textInput(paste0(prefix, "_label_e"),  "Intervention label", value = "", width = "100%"),
        shiny::textInput(paste0(prefix, "_label_c"),  "Control label",      value = "", width = "100%"),
        shiny::textInput(paste0(prefix, "_favors_left"),  "Favors (left)",  placeholder = "e.g., Favors Control", width = "100%"),
        shiny::textInput(paste0(prefix, "_favors_right"), "Favors (right)", placeholder = "e.g., Favors CBT-I",   width = "100%"),
        shiny::numericInput(paste0(prefix, "_xlim_lo"), "x-min", value = NA, width = "100%"),
        shiny::numericInput(paste0(prefix, "_xlim_hi"), "x-max", value = NA, width = "100%"),
        # Blank rows around the pooled result. Always visible (they used to be
        # wrapped in conditionalPanels that only revealed them when both column
        # checkboxes were off, which conditionalPanel implemented as a display
        # toggle anyway). Defaults mirror the Step 2 panel and are asymmetric
        # on purpose: above = 1 is the blank row meta::forest() draws by
        # default, while a blank "below" keeps plot_forest()'s automatic
        # derivation that holds the heterogeneity line clear of the x-axis.
        htmltools::p(class = "pma-card-subtitle pma-span-4",
          paste0("Blank rows around the pooled result. If the heterogeneity ",
                 "text overlaps the x-axis - most likely once the per-arm ",
                 "columns are hidden - use these to move it up or down. ",
                 "Above: 0 removes the blank row before the pooled result. ",
                 "Below: blank = automatic.")),
        shiny::numericInput(paste0(prefix, "_addrows_above"),
                            "Blank rows above pooled result",
                            value = 1, min = 0, step = 1, width = "100%"),
        shiny::numericInput(paste0(prefix, "_addrows_below"),
                            "Blank rows below pooled result",
                            value = NA, min = 0, step = 1, width = "100%"),
        # One checkbox for both per-arm column groups: plot_forest() keeps
        # show_n and show_events separate (correct for a library), but no user
        # wants the N columns without the event / mean-and-SD columns.
        htmltools::div(class = "pma-span-4",
          shiny::checkboxInput(paste0(prefix, "_show_arm_columns"),
                               paste0("Show per-arm data columns ",
                                      "(events or mean & SD, and N)"),
                               TRUE))
      )
    )
  }

  htmltools::tagList(
    pma_step_header(s$title, s$what),

    # Which studies the numbers on this step came from. Renders nothing when
    # the analysis rests on all studies, and the bare uiOutput wrapper is
    # unstyled, so the "all studies" case adds no box and no whitespace.
    # (A sticky "FINAL CERTAINTY" bar used to sit above this one; it was
    # removed because the Final certainty tab states the same thing properly.)
    shiny::uiOutput("analysis_set_indicator"),

    pma_card(
      title = "Certainty assessment (Core GRADE series)",
      shiny::tabsetPanel(
        id = "grade_tabs",

        # --- Configuration (cross-cutting; everything the five domains
        #     depend on is established and confirmed here, in the order a
        #     reviewer needs to decide it: control-group risk, then the
        #     threshold, then how the effect is presented) ---
        shiny::tabPanel("Configuration",
          htmltools::h4("Configuration",
                        style = "margin: 0 0 0.5rem; font-size: 1.1rem;"),
          htmltools::p(class = "pma-card-subtitle",
            EDU_COPY$config_tab$intro),
          shiny::uiOutput("threshold_panel"),
          shiny::uiOutput("config_status"),
          .confirm_checkbox("threshold_confirm",
            paste0("I have reviewed and confirm this configuration ",
                   "(required before export; the default values are fine ",
                   "if you agree with them)")),
          # Unlike every other sub-tab, this Next IS gated. The threshold
          # drives Risk of Bias, Inconsistency and Imprecision, so it has to
          # be settled before the reviewer works through them; letting them
          # walk the domains against a provisional threshold would mean
          # re-doing the work. Rendered as its own output so the gate can
          # flip without rebuilding the tab. See also output$grade_nav_final.
          shiny::uiOutput("grade_nav_config")
        ),

        # --- Risk of Bias ---
        shiny::tabPanel("Risk of Bias",
          .domain_header("Risk of Bias", "rob_badge", "rob_chip"),
          shiny::uiOutput("analysis_set_banner_rob"),
          # The body is a live output, not a fixed string: EDU_COPY$domains$
          # rob$how() interpolates the sensitivity-analysis change threshold
          # so the explanation always quotes the value the algorithm used.
          # inline = TRUE keeps it a <span> inside the <p> pma_how_collapse()
          # builds, and re-rendering the span leaves the <details> open.
          pma_how_collapse(shiny::uiOutput("rob_how_body", inline = TRUE)),
          pma_reference(EDU_COPY$domains$rob$ref_text, EDU_COPY$domains$rob$doi),
          # The binary classification rule, stated where it takes effect.
          # Live, because the boundary is a reviewer choice: a fixed
          # assertion here would contradict the control under "Inputs for
          # this domain" as soon as it is moved.
          shiny::uiOutput("rob_rule_note"),
          htmltools::p(
            style = "font-size: 0.85rem; color: hsl(var(--muted-foreground)); font-style: italic;",
            "See also: ",
            htmltools::tags$a(href = "https://www.bmj.com/content/366/bmj.l4898",
                              target = "_blank", "Sterne et al. RoB 2 (BMJ 2019)"),
            " for study-level risk of bias assessment."
          ),
          shiny::uiOutput("threshold_block_rob"),
          htmltools::h5("Evaluation"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("rob_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),
          htmltools::tags$details(
            class = "pma-edit-details",
            htmltools::tags$summary(
              class = "pma-edit-summary",
              htmltools::HTML(
                "&#9998;&nbsp; <strong>Edit per-study Risk of Bias</strong> ",
                "<span class='pma-edit-hint'>(click to expand &middot; ",
                "manually override each study or apply bulk presets)</span>"
              )
            ),
            htmltools::div(
              class = "pma-edit-body",
              htmltools::p(class = "pma-card-subtitle",
                           "Click a cell in the table to type a value (low / some / high). ",
                           "Use the bulk buttons to set all studies at once. ",
                           "Changes here are synced with Step 1."),
              htmltools::div(
                style = "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 0.5rem;",
                shiny::actionButton("step3_rob_set_low",  "Set all to Low",  class = "btn-sm"),
                shiny::actionButton("step3_rob_set_some", "Set all to Some", class = "btn-sm"),
                shiny::actionButton("step3_rob_set_high", "Set all to High", class = "btn-sm"),
                shiny::actionButton("step3_rob_clear",    "Clear all",       class = "btn-sm")
              ),
              DT::DTOutput("step3_rob_editor")
            )
          ),
          htmltools::h5("Forest plot stratified by RoB", style = "margin-top: 1rem;"),
          htmltools::div(class = "pma-forest-image",
            shinycssloaders::withSpinner(
              shiny::imageOutput("rob_forest", height = "auto"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "320px")),
          .forest_display_panel("rob"),
          .inputs_details(open = FALSE, title = "Inputs for this domain",
            # Where the low/high boundary falls. Core GRADE 4 endorses the
            # binary split but declines to fix the cut-off, so this is a
            # review decision rather than a rule taken from the source.
            shiny::radioButtons("rob_some_concerns",
              paste0("Review decision: where do studies rated 'some ",
                     "concerns' belong? (Core GRADE 4 leaves this boundary ",
                     "open; it is not a Core GRADE rule)"),
              choices = c(
                "Some concerns count as high risk of bias (default)" = "high",
                "Some concerns count as low risk of bias"            = "low"),
              selected = "high"),
            htmltools::p(class = "pma-card-subtitle",
              "Studies left unrated follow whichever side 'some concerns' ",
              "takes. The choice feeds the dominance gate, the ",
              "low-risk-only comparison estimate, and any refit on the ",
              "low-risk set, so it can change the certainty rating."),
            shiny::sliderInput("rob_inf_threshold",
              "Sensitivity-analysis change threshold (RoB-specific)",
              min = 0.05, max = 0.5, value = 0.10, step = 0.05),
            htmltools::div(
              class = "pma-card-subtitle",
              htmltools::p(
                "The analysis is pooled twice: once over all studies (TE_all) ",
                "and once over the low risk-of-bias studies only (TE_low). ",
                "This slider is how far the estimate must move between the ",
                "two before the domain is rated down. The comparison is ",
                "strict: a relative change of exactly the threshold does not ",
                "rate down, only a change greater than it."),
              htmltools::p(
                "The shift must also run in the direction that means the ",
                "high risk-of-bias studies were inflating the effect. Which ",
                "direction that is follows the outcome direction set in Step ",
                "2: when small values are undesirable, inflation means ",
                "TE_all above TE_low; when small values are desirable, it ",
                "means TE_all below TE_low. A shift of any size in the ",
                "opposite direction does not rate down."),
              htmltools::p(
                "Only one of the five decision rules consults this value ",
                "(rule 3, a bias-favouring change within the same non-trivial ",
                "zone). The trivial-zone rule, the zone-change rule and the ",
                "sign-flip rule ignore it entirely, so moving the slider ",
                "changes nothing on those paths. It has a second effect ",
                "outside the five rules: when high risk-of-bias studies do ",
                "not dominate the evidence, the same threshold decides ",
                "whether the two estimates count as substantially different ",
                "and the analysis is therefore restricted to the low ",
                "risk-of-bias studies. The slider governs both whether the ",
                "domain is rated down and whether the analysis is ",
                "restricted."),
              htmltools::p(
                htmltools::em("Caveat: "),
                "TE_low is always a fixed-effect inverse-variance mean, even ",
                "when the parent model is random-effects. Part of any ",
                "observed shift is therefore an estimator difference rather ",
                "than bias, and the gap widens with heterogeneity and with ",
                "unequal study sizes.")
            )
          ),
          .override_details(
            shiny::selectInput("rob_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious")),
            .override_rationale("rob_override", "rob_override_rationale")
          ),
          .confirm_checkbox("rob_confirm_na"),
          .grade_nav("grade_back_rob", "Back: Configuration",
                     "grade_next_rob", "Next: Inconsistency")
        ),

        # --- Inconsistency ---
        shiny::tabPanel("Inconsistency",
          .domain_header("Inconsistency", "incon_badge", "incon_chip"),
          pma_how_collapse(EDU_COPY$domains$inconsistency$how),
          pma_reference(EDU_COPY$domains$inconsistency$ref_text,
                        EDU_COPY$domains$inconsistency$doi),
          shiny::uiOutput("threshold_block_inco"),
          htmltools::h5("Evaluation"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("incon_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),
          htmltools::h5("Forest plot"),
          htmltools::div(class = "pma-forest-image",
            shinycssloaders::withSpinner(
              shiny::imageOutput("incon_forest", height = "auto"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "320px")),
          .forest_display_panel("incon"),
          .inputs_details(open = FALSE, title = "Inputs for this domain",
            shiny::selectInput("ci_diff",
              "Step 1: Important differences in point estimates AND limited CI overlap?",
              choices = c("(no override)" = "", "No" = "no", "Yes" = "yes")),
            shiny::conditionalPanel(
              "input.ci_diff == 'yes'",
              shiny::radioButtons("threshold_side",
                "Step 2: Where do point estimates fall vs the threshold?",
                choices = c("Majority on one side"  = "majority_one_side",
                            "Opposite sides"        = "opposite_sides"),
                selected = character(0))
            ),
            shiny::conditionalPanel(
              "input.threshold_side == 'opposite_sides'",
              shiny::radioButtons("subgroup_explained",
                "Step 3: Explained by credible subgroup analysis?",
                choices = c("Yes" = "yes", "No" = "no"),
                selected = character(0))
            )
          ),
          .override_details(
            shiny::selectInput("incon_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious")),
            .override_rationale("incon_override", "incon_override_rationale")
          ),
          .confirm_checkbox("incon_confirm_na"),
          .grade_nav("grade_back_incon", "Back: Risk of Bias",
                     "grade_next_incon", "Next: Indirectness")
        ),

        # --- Indirectness ---
        shiny::tabPanel("Indirectness",
          .domain_header("Indirectness", "indir_badge", "indir_chip"),
          shiny::uiOutput("indirectness_banner"),
          pma_how_collapse(EDU_COPY$domains$indirectness$how),
          pma_reference(EDU_COPY$domains$indirectness$ref_text,
                        EDU_COPY$domains$indirectness$doi),

          # ----- The four Core GRADE 5 PICO questions ----------------------
          # These used to sit collapsed under a "Considerations" summary and
          # fed nothing. They are now the primary input of the domain: the
          # answers become grade_meta(indirectness_subdomains = ), which
          # folds them worst-case into the domain judgment.
          htmltools::h5("Subdomain judgments (Core GRADE 5)",
                        style = "margin-top: 1rem;"),
          htmltools::p(class = "pma-card-subtitle",
            paste0("Core GRADE 5 asks the indirectness question separately ",
                   "for each PICO element. Answer each on the four-point ",
                   "scale - 'Is the evidence sufficiently direct?' - and the ",
                   "answers drive the domain judgment. Leave an element ",
                   "blank to omit it.")),
          htmltools::p(class = "pma-card-subtitle",
            EDU_COPY$domains$indirectness$mapping),
          htmltools::div(
            style = paste0(
              "padding: 0.5rem 0.75rem; background: #f5f5f5; ",
              "border-left: 4px solid #c07020; margin: 0.5rem 0 1rem; ",
              "font-size: 0.85rem;"),
            htmltools::p(style = "margin: 0;",
              htmltools::strong("Departure from the source, stated rather than implied: "),
              EDU_COPY$domains$indirectness$gradient)
          ),
          shiny::radioButtons("indir_population",
            "Population - trial population sufficiently similar to target patients?",
            choices = STEP3_INDIR_ANSWERS, inline = TRUE,
            selected = character(0)),
          htmltools::p(
            style = paste0("font-size: 0.8rem; color: hsl(var(--muted-foreground)); ",
                           "margin-top: -0.4rem; margin-bottom: 0.8rem;"),
            "Core GRADE 5 Table 2 rates Population the LEAST likely element to ",
            "justify rating down; differences in trial population rarely ",
            "affect relative effects in most clinical contexts (",
            htmltools::tags$a(href = "https://doi.org/10.1503/cmaj.200077",
                              target = "_blank", "ICEMAN; Schandelmaier et al., CMAJ 2020"),
            ")."
          ),
          shiny::radioButtons("indir_intervention",
            "Intervention - deliverable as studied?",
            choices = STEP3_INDIR_ANSWERS, inline = TRUE,
            selected = character(0)),
          htmltools::p(
            style = paste0("font-size: 0.8rem; color: hsl(var(--muted-foreground)); ",
                           "margin-top: -0.4rem; margin-bottom: 0.8rem;"),
            "Core GRADE 5 lists non-adherence to interventions as one of three ",
            "situations that warrant considering a downgrade even on an ",
            "ordinary search for direct evidence."
          ),
          shiny::radioButtons("indir_comparator",
            "Comparison - representative of usual care?",
            choices = STEP3_INDIR_ANSWERS, inline = TRUE,
            selected = character(0)),
          htmltools::p(
            style = paste0("font-size: 0.8rem; color: hsl(var(--muted-foreground)); ",
                           "margin-top: -0.4rem; margin-bottom: 0.8rem;"),
            "Problematic comparators are the second of those three situations."
          ),
          shiny::radioButtons("indir_outcome",
            "Outcome - patient-important, rather than a surrogate?",
            choices = STEP3_INDIR_ANSWERS, inline = TRUE,
            selected = character(0)),
          htmltools::div(
            style = paste0(
              "padding: 0.5rem 0.75rem; background: #f9f9f9; ",
              "border-left: 4px solid #0f172a; margin: 0.25rem 0 1rem; ",
              "font-size: 0.85rem;"),
            htmltools::p(style = "margin: 0;",
              EDU_COPY$domains$indirectness$surrogate)
          ),

          # The subdomain table pmatools built from those answers. Surfaced
          # because it is the only rendering of exactly what was sent to
          # grade_meta(): it shows which element drove the worst-case fold,
          # and its footer repeats the Core GRADE 5 Table 2 gradient caveat
          # next to the judgment rather than only in the collapsed copy.
          shiny::uiOutput("indir_subdomain_table"),

          # No preselected value (W4-A). With subdomain answers present this
          # radio OVERRIDES their worst-case fold; with none it is the whole
          # judgment. Either way a rating that differs from the automatic
          # value requires a written rationale.
          htmltools::h5("Overall indirectness rating",
                        style = "margin-top: 1.25rem;"),
          htmltools::p(class = "pma-card-subtitle",
            paste0("Leave this blank to accept the worst case across the ",
                   "subdomains above. Selecting a rating overrides it - use ",
                   "this when the symmetric fold misplaces the judgment, for ",
                   "example when the only concern sits on Population (low ",
                   "likelihood in Core GRADE 5 Table 2) or on Outcome ",
                   "(high).")),
          shiny::radioButtons("indirectness", NULL,
            choices = c("No" = "no",
                        "Some concerns" = "some_concerns",
                        "Serious" = "serious"),
            selected = character(0), inline = TRUE),
          shiny::conditionalPanel(
            "(input.indirectness || '') != ''",
            shiny::textAreaInput(
              "indir_rationale",
              paste0("Rationale (required whenever this rating differs from ",
                     "the automatic judgment)"),
              rows = 2, width = "100%",
              placeholder = paste0(
                "State which element (population / intervention / comparison ",
                "/ outcome) drives the rating and why it outweighs the ",
                "worst-case fold.")
            )
          ),
          htmltools::tags$details(
            class = "pma-edit-details",
            htmltools::tags$summary(
              class = "pma-edit-summary",
              htmltools::HTML(
                "&#9998;&nbsp; <strong>Edit per-study Indirectness</strong> ",
                "<span class='pma-edit-hint'>(click to expand &middot; ",
                "optional per-study notes; they label the stratified forest ",
                "plot only. The subdomain answers above, or an overall ",
                "rating, are what feed Core GRADE)</span>"
              )
            ),
            htmltools::div(
              class = "pma-edit-body",
              htmltools::p(class = "pma-card-subtitle",
                           "Click a cell in the table to type a value (low / some / high). ",
                           "Use the bulk buttons to set all studies at once. ",
                           "Changes here are synced with Step 1."),
              htmltools::div(
                style = "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 0.5rem;",
                shiny::actionButton("step3_indir_set_low",  "Set all to Low",  class = "btn-sm"),
                shiny::actionButton("step3_indir_set_some", "Set all to Some", class = "btn-sm"),
                shiny::actionButton("step3_indir_set_high", "Set all to High", class = "btn-sm"),
                shiny::actionButton("step3_indir_clear",    "Clear all",       class = "btn-sm")
              ),
              DT::DTOutput("step3_indir_editor")
            )
          ),
          shiny::uiOutput("indir_forest_image_block"),
          .forest_display_panel("indir"),
          .confirm_checkbox("indir_confirm_na"),
          .grade_nav("grade_back_indir", "Back: Inconsistency",
                     "grade_next_indir", "Next: Imprecision")
        ),

        # --- Imprecision ---
        shiny::tabPanel("Imprecision",
          .domain_header("Imprecision", "impre_badge", "impre_chip"),
          pma_how_collapse(EDU_COPY$domains$imprecision$how),
          pma_reference(EDU_COPY$domains$imprecision$ref_text,
                        EDU_COPY$domains$imprecision$doi),
          shiny::uiOutput("threshold_block_impre"),
          # Which Fig 4 branch this analysis took. Stated on the tab because
          # the branch decides whether sample size is consulted at all: on
          # the CI-crosses-threshold path the OIS is never reached, and a
          # reviewer reading OIS figures further down must be able to see
          # that they did not drive the judgment.
          htmltools::h5("Core GRADE 2 Figure 4 branch taken",
                        style = "margin-top: 1rem;"),
          shiny::uiOutput("impre_branch"),
          htmltools::h5("Evaluation"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("impre_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),
          .inputs_details(open = TRUE, title = "Inputs for this domain",
            shiny::conditionalPanel(
              "input.outcome_type == 'binary'",
              shiny::uiOutput("ois_p0_ui"),
              # Core GRADE 2 parameterises the BINARY OIS by a modest
              # relative risk reduction, not by the threshold, and names two
              # values. Both are offered; the absolute-scale equivalent is
              # shown alongside so the target can be read in the same units
              # as the threshold.
              shiny::radioButtons("ois_rrr",
                paste0("Modest relative risk reduction the OIS is powered to ",
                       "detect (Core GRADE 2 names 20% and 25%)"),
                choices = c("20 percent (default)" = "0.20",
                            "25 percent"           = "0.25"),
                selected = "0.20", inline = TRUE),
              shiny::uiOutput("ois_rrr_equiv")
            ),
            shiny::conditionalPanel(
              "input.outcome_type == 'continuous'",
              shiny::uiOutput("ois_sd_ui"),
              htmltools::p(class = "pma-card-subtitle",
                paste0("For continuous outcomes Core GRADE 2 directs the OIS ",
                       "to the threshold rather than to a relative risk ",
                       "reduction, so the Configuration threshold is used as ",
                       "the target difference and no RRR is asked for."))
            ),
            shiny::numericInput("ois_events_override",
              "Override OIS - target events (binary)",
              value = NA, min = 0, step = 1),
            shiny::numericInput("ois_n_override",
              "Override OIS - target N (continuous)",
              value = NA, min = 0, step = 1),
            htmltools::p(class = "pma-card-subtitle",
              paste0("Either override replaces the calculated OIS. Figure 4 ",
                     "compares the OIS against participants, not events: ",
                     "'If the total sample size of all the studies included ",
                     "in a meta-analysis exceeds the OIS, one does not rate ",
                     "down.' The events override is kept for backward ",
                     "compatibility and switches the comparison to total ",
                     "events."))
          ),
          .override_details(
            htmltools::p(class = "pma-card-subtitle",
              paste0("Figure 4's second two-level condition is a reviewer ",
                     "judgment and is not assessed automatically: consider ",
                     "rating down two levels when the most appropriate plain ",
                     "language summary of the result warrants 'may' rather ",
                     "than 'likely'. Read the Summary of Findings wording ",
                     "against the message you intend to convey, and record ",
                     "the conclusion here.")),
            shiny::selectInput("impre_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious")),
            .override_rationale("impre_override", "impre_override_rationale")
          ),
          .confirm_checkbox("impre_confirm_na"),
          .grade_nav("grade_back_impre", "Back: Indirectness",
                     "grade_next_impre", "Next: Publication bias")
        ),

        # --- Publication bias ---
        shiny::tabPanel("Publication bias",
          .domain_header("Publication bias", "pubias_badge", "pubias_chip"),
          pma_how_collapse(EDU_COPY$domains$pubias$how),
          pma_reference(EDU_COPY$domains$pubias$ref_text,
                        EDU_COPY$domains$pubias$doi),
          htmltools::p(
            style = "font-size: 0.85rem; color: hsl(var(--muted-foreground)); font-style: italic;",
            "You may also consider applying ",
            htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2023-076754",
                              target = "_blank", "RoB-ME"),
            " to assess risk of bias due to missing evidence (Page et al., BMJ 2023)."
          ),

          htmltools::p(class = "pma-card-subtitle",
            paste0("Core GRADE 4 Figure 5 has exactly four decision nodes and ",
                   "no entry-level rule-out. The headings below are numbered ",
                   "in the source's order, and the collapsible explanation ",
                   "above uses the same numbers.")),

          # ----- Q1 (Fig 5 node 1): small + industry-sponsored -------------
          htmltools::h5("Q1. Most or all studies small AND industry-sponsored?",
                        style = "margin-top: 1rem;"),
          htmltools::p(class = "pma-card-subtitle",
            "A 'yes' answer is sufficient evidence on its own (rate down 1; some concerns)."
          ),
          shiny::radioButtons("pubias_small_industry", NULL,
            choices = c("(use default: no)" = "", "No" = "no", "Yes" = "yes"),
            inline = TRUE),
          htmltools::hr(),

          # ----- Not a Fig 5 node: the overall reporting-bias judgment -----
          # pmatools 0.5 moved this convenience input from an entry rule-out
          # to a check applied AFTER Q1, so a body of small industry-sponsored
          # trials still rates down even when registry coverage is asserted
          # complete. The UI position mirrors the evaluation order.
          htmltools::h5("Additional input (not a node of Figure 5): overall reporting-bias judgment"),
          htmltools::p(class = "pma-card-subtitle",
            paste0("This question is a pmatools convenience input, not one of ",
                   "Figure 5's four nodes. pmatools 0.5 evaluates it after ",
                   "Q1, which is why it sits here rather than at the top: ",
                   "'Yes' then short-circuits the domain to no rate down, ",
                   "while a 'yes' answer to Q1 still rates down. Answering ",
                   "'No' is an app-level rule that forces rate down 1 ",
                   "regardless of Q2-Q4. Leave it blank to let the four ",
                   "Figure 5 nodes decide.")),
          htmltools::div(class = "pma-card-subtitle",
            htmltools::p(htmltools::strong("Suspect reporting bias when:")),
            htmltools::tags$ul(
              htmltools::tags$li("Unpublished data and grey literature were not searched."),
              htmltools::tags$li("The synthesis rests on a small number of positive early findings (eg, a newly marketed drug, where early evidence tends to overestimate efficacy and safety)."),
              htmltools::tags$li("Prior empirical evidence documents reporting bias for this comparison (eg, Turner et al. 2008 for placebo-controlled antidepressant trials).")
            ),
            htmltools::p(htmltools::strong("Reporting bias is unlikely when:")),
            htmltools::tags$ul(
              htmltools::tags$li("Unpublished studies have been identified and their findings agree with the published evidence."),
              htmltools::tags$li("Prospective trial registration is the field standard, and registered protocols / registries do not show important discrepancies with published reports.")
            )
          ),
          shiny::radioButtons("pubias_registry_complete",
            "Overall, does the situation argue against reporting bias?",
            choices = c(
              "(no overall judgment yet)"               = "",
              "No - reporting bias is plausible (rate down 1)"      = "no",
              "Yes - reporting bias is unlikely (no rate down)"     = "yes"
            ),
            inline = FALSE),
          htmltools::hr(),

          # ----- Q2 + Q3 (or Q4) + reference materials (server-rendered) ---
          shiny::uiOutput("pubias_main_block"),

          # Reference: Subgroup analysis (Available vs Missing results) - RoB-ME.
          # DT::DTOutput must be statically placed; placing it inside the
          # uiOutput("pubias_main_block") above prevents DT/htmlwidgets from
          # binding cleanly. This block is shown unconditionally and the
          # server-side render returns NULL when k < 10.
          htmltools::hr(),
          htmltools::h5("Reference: Subgroup analysis (available vs missing results)"),
          htmltools::p(class = "pma-card-subtitle",
            "Studies with no extractable effect estimate are automatically ",
            "moved into the Missing-results subgroup. You can also add ",
            "trials that exist (registry / protocol / conference abstract) ",
            "but were not even loaded into this meta-analysis. The forest ",
            "plot renders Available and Missing results as two subgroups, ",
            "mirroring ",
            htmltools::tags$a(href = "https://doi.org/10.1136/bmj-2023-076754",
                              target = "_blank", "Page et al., BMJ 2023 (RoB-ME)"),
            htmltools::HTML(paste0(
              ". This information is <strong>not part of the automated ",
              "Core GRADE algorithm</strong>, but you are encouraged to consider ",
              "it when finalising the publication-bias judgment manually ",
              "(use the override below)."))),
          htmltools::div(
            style = "display: flex; gap: 0.5rem; margin-bottom: 0.5rem;",
            shiny::actionButton("pubias_missing_add", "+ Add missing trial", class = "btn-sm")
          ),
          htmltools::p(class = "pma-card-subtitle",
            "Click any cell to edit. The Results known cell offers an ",
            "autocomplete list of recommended RoB-ME labels but also accepts ",
            "free text. Auto-classified rows from the dataset cannot be ",
            "removed (they are part of the meta-analysis); user-added rows ",
            "remain fully editable."),
          # Datalist powering the in-cell autocomplete for results_known.
          htmltools::tags$datalist(id = "pubias_rk_datalist",
            htmltools::tags$option(value = "Reported but data not extractable"),
            htmltools::tags$option(value = "Not measured"),
            htmltools::tags$option(value = "Measured but not reported (suspect P > 0.05)"),
            htmltools::tags$option(value = "Measured but not reported (suspect P < 0.05)"),
            htmltools::tags$option(value = "Measured but not reported (in the opposite direction)")
          ),
          # When DT injects an <input type="text"> on cell edit for the
          # results_known column (index 2 of the visible columns), give it a
          # `list` attribute so the browser shows the datalist suggestions
          # while still allowing free-text typing.
          htmltools::tags$script(htmltools::HTML(
            "$(document).on('focusin', '#pubias_missing_editor input[type=text]', function(){",
            "  var $td = $(this).closest('td');",
            "  var col = $td.parent().children().index($td);",
            "  if (col === 2) { $(this).attr('list', 'pubias_rk_datalist'); }",
            "});"
          )),
          DT::DTOutput("pubias_missing_editor"),
          shinycssloaders::withSpinner(
            shiny::imageOutput("pubias_missing_forest", height = "auto"),
            type = 4, color = "#0f172a", size = 0.6,
            proxy.height = "320px"),
          .forest_display_panel("pubias"),

          htmltools::hr(),
          htmltools::h5("Evaluation result"),
          shinycssloaders::withSpinner(
            shiny::verbatimTextOutput("pubias_notes"),
            type = 4, color = "#0f172a", size = 0.5,
            proxy.height = "80px"),

          .override_details(
            shiny::selectInput("pubias_override", NULL,
              choices = c("(no override)" = "", "No" = "no",
                          "Some concerns" = "some_concerns",
                          "Serious" = "serious")),
            .override_rationale("pubias_override", "pubias_override_rationale")
          ),
          .confirm_checkbox("pubias_confirm_na"),
          .grade_nav("grade_back_pubias", "Back: Imprecision",
                     "grade_next_pubias", "Next: Final certainty")
        ),

        # --- Final certainty (7th tab) ---
        shiny::tabPanel("Final certainty",
          shiny::uiOutput("cert_incomplete_banner"),
          shiny::uiOutput("analysis_set_banner_cert"),
          htmltools::h5("Core GRADE Evidence Profile"),
          htmltools::div(
            style = "margin-top: 0.5rem; margin-bottom: 1rem;",
            shinycssloaders::withSpinner(
              shiny::uiOutput("final_certainty"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "200px")
          ),
          htmltools::hr(),
          htmltools::h5("Summary of Findings"),
          htmltools::div(
            style = "margin-top: 0.5rem; margin-bottom: 1rem;",
            shinycssloaders::withSpinner(
              shiny::uiOutput("sof_preview"),
              type = 4, color = "#0f172a", size = 0.6,
              proxy.height = "200px")
          ),
          htmltools::p(
            style = "font-size: 0.85rem; color: hsl(var(--muted-foreground)); font-style: italic;",
            "Recommended: report both control event rate (CER) and intervention event rate (EER) ",
            "alongside the relative effect to aid clinical interpretation (",
            htmltools::tags$a(href = "https://doi.org/10.1136/bmjment-2023-300978",
                              target = "_blank", "Heimke et al., BMJ Ment Health 2024"),
            ")."
          ),
          htmltools::hr(),
          htmltools::h5("Display options"),
          htmltools::div(
            style = paste(
              "display: grid;",
              "grid-template-columns: repeat(2, minmax(220px, 1fr));",
              "gap: 0.75rem 1rem;"),
            htmltools::div(style = "grid-column: span 2;",
              shiny::uiOutput("outcome_name_echo")),
            shiny::numericInput("per", "Display rates per N patients",
              value = 1000, min = 1, step = 100),
            htmltools::div(style = "grid-column: span 2;",
              shiny::checkboxInput("prediction",
                "Show 95 percent prediction interval in Effect column", FALSE)),
            htmltools::div(style = "grid-column: span 2;",
              shiny::textInput("other_text",
                "Other considerations (free text shown in Evidence Profile)",
                placeholder = "e.g., All trials conducted in a single country; reporting bias")),
            htmltools::div(style = "grid-column: span 2;",
              shiny::radioButtons("other_downgrade",
                "Apply additional downgrade for the above?",
                choices = c("No (-0)" = "0",
                            "Yes, by 1 level (-1)" = "-1",
                            "Yes, by 2 levels (-2)" = "-2"),
                selected = "0", inline = TRUE)),
            # The responder-conversion controls (convert_smd_to_or,
            # baseline_risk_chinn, threshold_label) used to live here. They
            # moved to the Configuration tab: the control-group responder
            # proportion and the definition of the threshold of clinical
            # interest are not display preferences, they are inputs the
            # rating is read against, and they belong with the threshold
            # they mirror. The input IDs are unchanged, so app.R's display
            # observer and Step 4's export still read them. chinn_invert
            # lost its checkbox entirely - it is now derived from the Step 2
            # direction answer (see chinn_invert_derived()).
            htmltools::div(style = "grid-column: span 2;",
              shiny::uiOutput("display_options_config_note"))
          ),
          htmltools::hr(),

          # ----- Save this outcome for the multi-outcome SoF table -----
          # Sits at the end of the Final certainty tab: this is the point in
          # the wizard where the rating for one outcome is complete, and the
          # natural place to bank it before going back to Step 2 for the
          # next outcome.
          htmltools::h5("Saved outcomes for the Summary of Findings table"),
          htmltools::p(class = "pma-card-subtitle",
                       EDU_COPY$multi_outcome$save_intro),
          shiny::uiOutput("save_outcome_panel"),
          shiny::uiOutput("saved_outcomes_list"),

          # The only Next in Step 3 that leaves the step (every other one
          # just moves to the following sub-tab, which stays free), so it is
          # the only one gated on the domain confirmations. Rendered as its
          # own output so that gate can flip without rebuilding this tab.
          shiny::uiOutput("grade_nav_final")
        )
      )
    )
  )
}

step3_server <- function(input, output, session, state) {

  grade_tab_sequence <- c(
    "Configuration",
    "Risk of Bias",
    "Inconsistency",
    "Indirectness",
    "Imprecision",
    "Publication bias",
    "Final certainty"
  )

  advance_grade_tab <- function(current = NULL) {
    current <- current %||% shiny::isolate(input$grade_tabs) %||%
      grade_tab_sequence[1]
    idx <- match(current, grade_tab_sequence)
    if (is.na(idx)) idx <- 1L
    if (idx < length(grade_tab_sequence)) {
      shiny::updateTabsetPanel(
        session,
        "grade_tabs",
        selected = grade_tab_sequence[[idx + 1L]]
      )
      session$sendCustomMessage("scroll_top", list())
      return(invisible(TRUE))
    }
    commit <- state$step3_commit %||% function() TRUE
    if (isTRUE(commit())) {
      state$step <- 4L
      session$sendCustomMessage("scroll_top", list())
    }
    invisible(TRUE)
  }

  retreat_grade_tab <- function(current = NULL) {
    current <- current %||% shiny::isolate(input$grade_tabs) %||%
      grade_tab_sequence[1]
    idx <- match(current, grade_tab_sequence)
    if (is.na(idx)) idx <- 1L
    if (idx > 1L) {
      shiny::updateTabsetPanel(
        session,
        "grade_tabs",
        selected = grade_tab_sequence[[idx - 1L]]
      )
      session$sendCustomMessage("scroll_top", list())
      return(invisible(TRUE))
    }
    state$step <- 2L
    session$sendCustomMessage("scroll_top", list())
    invisible(TRUE)
  }

  shiny::observeEvent(input$grade_back_thresh, {
    retreat_grade_tab("Configuration")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_rob, {
    retreat_grade_tab("Risk of Bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_incon, {
    retreat_grade_tab("Inconsistency")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_indir, {
    retreat_grade_tab("Indirectness")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_impre, {
    retreat_grade_tab("Imprecision")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_pubias, {
    retreat_grade_tab("Publication bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_back_final, {
    retreat_grade_tab("Final certainty")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_thresh, {
    advance_grade_tab("Configuration")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_rob, {
    advance_grade_tab("Risk of Bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_incon, {
    advance_grade_tab("Inconsistency")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_indir, {
    advance_grade_tab("Indirectness")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_impre, {
    advance_grade_tab("Imprecision")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_pubias, {
    advance_grade_tab("Publication bias")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$grade_next_final, {
    advance_grade_tab("Final certainty")
  }, ignoreInit = TRUE)
  # ----- Threshold state (single source of truth: Configuration tab;
  # independent of UI render so Final certainty doesn't flip as the user
  # clicks tabs) -----------------------------------------------------------
  # threshold_state          : relative (ratio) or te-scale value
  # threshold_mode_state     : "absolute" / "relative" (binary ratio SMs only)
  # threshold_abs_state      : absolute threshold, events per 1,000
  # threshold_baseline_state : baseline (control-group) risk, per 1,000
  THRESHOLD_MODE_DEFAULT   <- "absolute"
  threshold_state          <- shiny::reactiveVal(NA_real_)
  threshold_mode_state     <- shiny::reactiveVal(THRESHOLD_MODE_DEFAULT)
  threshold_abs_state      <- shiny::reactiveVal(NA_real_)
  threshold_baseline_state <- shiny::reactiveVal(NA_real_)

  # Initialise defaults from suggest_threshold() / pooled CER as soon as
  # state$ma is available. These observers live outside renderUI on purpose:
  # renderUI is suspended while the tab is hidden, so grade_obj() would
  # otherwise see NA thresholds until the user first opened the tab.
  # observeEvent on state$ma, with the threshold reactiveVals read under
  # isolate(): a plain observe() also depended on them, so clearing the
  # threshold field immediately re-seeded it from suggest_threshold() and the
  # "no threshold" state was unreachable. Seed once per analysis instead.
  # Which analysis the current thresholds were seeded for. A threshold is
  # only meaningful on one scale, so when the summary measure changes (OR ->
  # SMD, say) the old value is discarded and the new suggestion applied;
  # otherwise an OR of 1.25 would silently become an SMD threshold of 1.25.
  threshold_seed_key <- shiny::reactiveVal(NA_character_)

  # Prefill the threshold reactiveVals from suggest_threshold(). Extracted
  # from the observer below so that voiding an outcome can re-run it: the
  # observer only fires on a change of state$ma, and it may already have fired
  # for this analysis by the time the reset lands.
  .seed_thresholds <- function() {
    obj <- shiny::isolate(state$ma)
    if (is.null(obj)) return(invisible(NULL))
    key <- paste(class(obj)[1], obj$sm %||% "", sep = "/")
    fresh <- !identical(key, shiny::isolate(threshold_seed_key()))
    if (fresh) {
      threshold_seed_key(key)
      threshold_state(NA_real_)
      threshold_abs_state(NA_real_)
      threshold_baseline_state(NA_real_)
    }
    s <- tryCatch(suggest_threshold(obj), error = function(e) NULL)
    sug <- step3_threshold_suggestions(s)
    # Only ever prefill a reactiveVal that is still NA: a value the user typed
    # must never be overwritten.
    if (is.na(shiny::isolate(threshold_state())) && !is.na(sug$relative)) {
      threshold_state(round(sug$relative, 4))
    }
    if (is.na(shiny::isolate(threshold_abs_state())) &&
        !is.na(sug$absolute1000)) {
      threshold_abs_state(round(sug$absolute1000, 1))
    }
    invisible(NULL)
  }

  # ----- Voiding this outcome's Step 3 answers ------------------------------
  # Called by app.R's begin_new_outcome(). The threshold values live in
  # reactiveVals, not in input$, so rebuilding the Step 3 body does not touch
  # them: without this, a threshold entered for one outcome would be re-seeded
  # into the next outcome's field and silently rated against.
  #
  # Both the threshold suggestion and the pooled control-group risk are asked
  # for again here rather than left to the observers that normally seed them.
  # Those observers fire on a change of state$ma, which may already have
  # happened by the time the reset lands - and the reset can also be triggered
  # by a change of direction alone, which does not touch state$ma at all.
  state$step3_reset <- function() {
    threshold_seed_key(NA_character_)
    threshold_state(NA_real_)
    threshold_abs_state(NA_real_)
    threshold_baseline_state(NA_real_)
    threshold_mode_state(THRESHOLD_MODE_DEFAULT)
    .seed_thresholds()
    cr <- shiny::isolate(control_risk())
    if (is.finite(cr$value)) threshold_baseline_state(round(1000 * cr$value, 1))
  }

  # Which outcome was each per-outcome input last answered for? Shiny keeps
  # the last value of an input whose widget has been torn down, so between
  # "the outcome changed" (app.R bumps state$outcome_gen) and "Step 3 was
  # rendered again" every input below still reports the PREVIOUS outcome's
  # answer. That is the whole bug: Step 4's export gate reads
  # state$domain_confirmed, which is computed from these inputs, so an outcome
  # nobody had looked at exported as reviewed.
  #
  # state$outcome_gen is read under isolate() on purpose. A reactive read would
  # make every one of these observers re-fire when the generation is bumped,
  # re-stamping the stale answers as current and defeating the guard.
  .answer_gen <- shiny::reactiveValues()
  for (.outcome_input_id in pma_outcome_input_ids()) {
    local({
      id <- .outcome_input_id
      shiny::observeEvent(input[[id]], {
        .answer_gen[[id]] <- shiny::isolate(state$outcome_gen)
      }, ignoreInit = FALSE, ignoreNULL = FALSE)
    })
  }
  # An answer counts only if it was given for the outcome now open. Failing
  # closed is the safe direction: a wrongly-stale answer locks the gate, it
  # never opens it.
  .fresh <- function(id) identical(.answer_gen[[id]], state$outcome_gen)

  shiny::observeEvent(state$ma, { .seed_thresholds() }, ignoreNULL = TRUE)
  # Pooled control-group risk. One computation, cached in a reactive, feeding
  # BOTH the Configuration threshold baseline and the Imprecision OIS p0, so
  # the two can no longer disagree. Previously each site recomputed a crude
  # sum(event.c) / sum(n.c) of its own while the on-screen copy claimed the
  # number was pooled.
  control_risk <- shiny::reactive({
    step3_control_risk(state$ma)
  })

  shiny::observe({
    cr <- control_risk()
    if (is.na(threshold_baseline_state()) && is.finite(cr$value)) {
      threshold_baseline_state(round(1000 * cr$value, 1))
    }
  })

  # The auto (pooled) value in events per 1,000, or NA. Used to decide
  # whether the reviewer has overridden it - and therefore owes a rationale.
  control_risk_auto1000 <- shiny::reactive({
    cr <- control_risk()
    if (is.finite(cr$value)) round(1000 * cr$value, 1) else NA_real_
  })

  baseline_overridden <- shiny::reactive({
    auto <- control_risk_auto1000()
    cur  <- threshold_baseline_state()
    if (!is.finite(auto)) return(FALSE)
    if (!is.finite(cur)) return(TRUE)
    !isTRUE(all.equal(auto, cur, tolerance = 1e-8))
  })

  baseline_rationale_ok <- shiny::reactive({
    if (!baseline_overridden()) return(TRUE)
    nzchar(trimws(input$threshold_baseline_rationale %||% ""))
  })

  # Mirror Configuration-tab inputs into the reactiveVals.
  shiny::observeEvent(input$threshold_mode, {
    if (nzchar(input$threshold_mode %||% "")) {
      threshold_mode_state(input$threshold_mode)
    }
  }, ignoreInit = TRUE)
  # An emptied threshold field now clears the state rather than silently
  # leaving the previous value in force: the reviewer sees an empty box, so
  # the app must not keep rating against a number they removed. NULL (the
  # widget does not exist, e.g. on the continuous branch) is left alone.
  shiny::observeEvent(input$threshold_abs, {
    v <- input$threshold_abs
    if (is.null(v) || length(v) != 1) return()
    threshold_abs_state(if (is.na(v)) NA_real_ else v)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(input$threshold_baseline_input, {
    v <- input$threshold_baseline_input
    if (!is.null(v) && length(v) == 1 && !is.na(v)) threshold_baseline_state(v)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$threshold_ratio, {
    v <- input$threshold_ratio
    if (is.null(v) || length(v) != 1) return()
    threshold_state(if (is.na(v)) NA_real_ else v)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  shiny::observeEvent(input$threshold_cont, {
    v <- input$threshold_cont
    if (is.null(v) || length(v) != 1) return()
    threshold_state(if (is.na(v)) NA_real_ else v)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Responder-conversion state (continuous outcomes). The control-group
  # proportion has no auto value: Core GRADE 6 says only that the rate is
  # "chosen from the context", and nothing in pmatools proposes a default.
  # 20 percent is therefore an app convention and is labelled as one until
  # the reviewer confirms or replaces it.
  RESPONDER_P0_DEFAULT <- 0.20

  responder_p0 <- shiny::reactive({
    v <- input$baseline_risk_chinn
    if (is.null(v) || length(v) != 1L || is.na(v)) return(NA_real_)
    v
  })
  responder_p0_valid <- shiny::reactive({
    v <- responder_p0()
    is.finite(v) && v > 0 && v < 1
  })
  responder_p0_overridden <- shiny::reactive({
    v <- responder_p0()
    if (!is.finite(v)) return(TRUE)
    !isTRUE(all.equal(v, RESPONDER_P0_DEFAULT, tolerance = 1e-8))
  })
  # Confirmed = the reviewer either ticked the confirmation box, or replaced
  # the app-convention default and said why. An unconfirmed default is an
  # assumption nobody has looked at, so it gates this tab's Next.
  responder_p0_confirmed <- shiny::reactive({
    if (!responder_p0_valid()) return(FALSE)
    if (responder_p0_overridden()) {
      return(nzchar(trimws(input$responder_p0_rationale %||% "")))
    }
    isTRUE(input$responder_p0_confirm)
  })

  # chinn_invert is DERIVED from the Step 2 direction answer rather than
  # asked again. Chinn's formula gives OR = exp(SMD * pi / sqrt(3)), so a
  # symptom scale where smaller is better produces a negative SMD for an
  # effective treatment and an OR below 1 - the wrong side. Flipping the sign
  # puts the intervention above 1. Verified empirically on the bundled
  # CBT-I data (pooled SMD -0.49, control 200 per 1,000): invert = TRUE
  # gives 377 per 1,000 for the intervention, invert = FALSE gives 94.
  chinn_invert_derived <- shiny::reactive({
    identical(state$small_values, "desirable")
  })

  .mic_note <- function() {
    htmltools::p(
      class = "pma-card-subtitle",
      style = "font-style: italic;",
      paste0(
        "Note: do not equate this threshold with a Minimally Important ",
        "Change (MIC). MIC is a within-individual change over time, while ",
        "a meta-analysis pools between-group differences across studies. ",
        "These are distinct quantities and should not be substituted for ",
        "each other when judging clinical importance."
      )
    )
  }

  # Section heading + note helpers, so the Configuration blocks look alike.
  .config_section <- function(title, ...) {
    htmltools::div(
      style = paste(
        "border: 1px solid hsl(var(--border)); border-radius: 8px;",
        "padding: 0.75rem 1rem; margin-bottom: 1rem;"),
      htmltools::h5(title, style = "margin: 0 0 0.5rem; font-size: 1rem;"),
      ...
    )
  }
  .config_note <- function(...) {
    htmltools::p(class = "pma-card-subtitle", style = "margin-bottom: 0.5rem;",
                 ...)
  }
  .warn_badge <- function(text) {
    htmltools::span(
      class = "pma-badge",
      style = sprintf(paste0("background: %s; color: %s; border: 1px solid %s;",
                             " white-space: nowrap; margin-left: 0.4rem;"),
                      PMA_ALERT_BG, PMA_ALERT_FG, PMA_ALERT_FG),
      text)
  }
  .ok_badge <- function(text) {
    htmltools::span(
      class = "pma-badge",
      style = paste0("background: hsl(var(--muted)); ",
                     "color: hsl(var(--muted-foreground)); ",
                     "white-space: nowrap; margin-left: 0.4rem;"),
      text)
  }

  # suggest_threshold() carries a $source telling the reader whether the
  # prefilled number comes from Core GRADE 6 itself or is only a pmatools
  # convention. Never present a package convention as a Core GRADE number.
  threshold_suggestion <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    tryCatch(suggest_threshold(obj), error = function(e) NULL)
  })
  .source_badge <- function(src) {
    if (identical(src, "core_grade_6")) {
      return(.ok_badge("source: Core GRADE 6"))
    }
    .warn_badge("source: pmatools convention, not Core GRADE")
  }

  # ----- Configuration tab: control-group risk block (binary) ------------
  # First on the tab. The absolute threshold is only interpretable against a
  # control-group risk, and the same number is the Optimal Information Size
  # baseline in Imprecision, so it is settled before anything else.
  .control_risk_block <- function() {
    cr   <- control_risk()
    auto <- control_risk_auto1000()
    cond <- if (is.finite(auto)) {
      sprintf("input.threshold_baseline_input != %s",
              format(auto, scientific = FALSE))
    } else {
      "true"
    }
    provenance <- if (identical(cr$method, "metaprop")) {
      htmltools::tagList(
        .ok_badge("pooled (random-effects metaprop)"),
        .config_note(sprintf(paste0(
          "Prefilled from a random-effects pooled proportion of the ",
          "control arms (meta::metaprop, GLMM with a logit link, ",
          "back-transformed), over %d stud%s%s. The crude ratio of total ",
          "events to total control participants is %.1f per 1,000; the ",
          "pooled value shown is %.1f per 1,000."),
          cr$k_used, if (cr$k_used == 1L) "y" else "ies",
          if (cr$k_dropped > 0L) {
            sprintf(" (%d study with no control-arm count excluded)",
                    cr$k_dropped)
          } else "",
          1000 * cr$crude, auto))
      )
    } else if (identical(cr$method, "simple_fallback")) {
      htmltools::tagList(
        .warn_badge("not pooled: crude event rate"),
        .config_note(sprintf(paste0(
          "The random-effects pooled proportion (metaprop, GLMM) did not ",
          "converge on these data, so this is the crude ratio of total ",
          "control events to total control participants (%.1f per 1,000) ",
          "over %d stud%s. It is not a pooled estimate; treat it as a rough ",
          "summary and replace it if you have a better one."),
          1000 * cr$crude, cr$k_used,
          if (cr$k_used == 1L) "y" else "ies"))
      )
    } else {
      .warn_badge("no control-arm data")
    }

    .config_section(
      htmltools::tagList("Control-group risk", provenance),
      shiny::numericInput("threshold_baseline_input",
        "Control-group risk (events per 1,000 patients)",
        # Fall back to the pooled value directly: this render and the
        # observer that seeds threshold_baseline_state() both hang off
        # control_risk(), and their order is not guaranteed.
        value = {
          v <- shiny::isolate(threshold_baseline_state())
          if (is.finite(v)) v else auto
        },
        min = 0, max = 1000, step = 5),
      .config_note(
        "Used to convert an absolute threshold to the analysis scale, and ",
        "as the Optimal Information Size baseline in Imprecision. Replace ",
        "it with a better estimate for your target population if you have ",
        "one."),
      # Same pattern as the domain-tab overrides: replacing an automated
      # value requires a written justification (Core GRADE transparency).
      shiny::conditionalPanel(
        cond,
        shiny::textAreaInput("threshold_baseline_rationale",
          "Rationale (required when the pooled value is replaced)",
          rows = 2, width = "100%",
          placeholder = paste0(
            "e.g., rounded to 175 per 1,000; taken from the untreated arm ",
            "of the Smith 2021 cohort; adjusted downwards for a primary ",
            "care population with milder disease."))
      )
    )
  }

  # ----- Configuration tab: responder conversion block (continuous) ------
  # Core GRADE 6 ranks three presentations of a continuous outcome and
  # recommends the mean difference and the responder proportion together.
  # This app implements the responder proportion only; the departure is
  # stated on screen rather than left implicit, and so is the fact that the
  # conversion used is Chinn's formula, not Core GRADE 6's own procedure.
  .responder_block <- function(sm) {
    convertible <- sm %in% c("SMD", "MD")
    if (!convertible) {
      return(.config_section(
        "Presentation of this outcome",
        .config_note(EDU_COPY$config_tab$continuous_intro),
        .config_note(EDU_COPY$config_tab$continuous_departure),
        htmltools::p(
          class = "pma-card-subtitle", style = "font-style: italic;",
          sprintf(paste0(
            "The responder conversion is unavailable for %s: it is defined ",
            "on the standardized mean difference (and on the mean ",
            "difference via the pooled SD) only. The Summary of Findings ",
            "table will report the %s itself."), sm, sm))
      ))
    }
    # The badge is its own output on purpose: if this renderUI depended on
    # the confirmation state, ticking the box (or typing a rationale) would
    # rebuild the panel and reset the very widget being used.
    .config_section(
      htmltools::tagList(
        "Proportion of control patients meeting the threshold",
        shiny::uiOutput("responder_p0_badge", inline = TRUE)),
      .config_note(EDU_COPY$config_tab$continuous_intro),
      .config_note(EDU_COPY$config_tab$continuous_departure),
      .config_note(EDU_COPY$config_tab$chinn_caveat),
      shiny::checkboxInput("convert_smd_to_or",
        paste0("Present this outcome as a proportion of responders ",
               "(recommended; Core GRADE 6 option 2)"),
        value = TRUE),
      shiny::conditionalPanel(
        "input.convert_smd_to_or",
        shiny::numericInput("baseline_risk_chinn",
          paste0("Proportion of control patients meeting the threshold of ",
                 "clinical interest"),
          value = RESPONDER_P0_DEFAULT, min = 0.01, max = 0.99, step = 0.01),
        .config_note(EDU_COPY$config_tab$responder_default),
        # This is not a risk and Core GRADE has no notion of baseline risk
        # for a continuous outcome, so it does not reuse the binary label.
        shiny::conditionalPanel(
          sprintf("input.baseline_risk_chinn != %s", RESPONDER_P0_DEFAULT),
          shiny::textAreaInput("responder_p0_rationale",
            "Rationale (required when the default is replaced)",
            rows = 2, width = "100%",
            placeholder = paste0(
              "e.g., 31 percent of control participants met the 50 percent ",
              "reduction criterion in the three trials that reported it; ",
              "taken from the placebo arm of Jones 2019."))
        ),
        shiny::conditionalPanel(
          sprintf("input.baseline_risk_chinn == %s", RESPONDER_P0_DEFAULT),
          shiny::checkboxInput("responder_p0_confirm",
            paste0("I have considered this rate and accept 20 percent ",
                   "(200 per 1,000) for this outcome"),
            value = FALSE)
        ),
        shiny::textInput("threshold_label",
          "Definition of the threshold of clinical interest (free text)",
          placeholder = "e.g., >=50 percent reduction in PHQ-9 from baseline"),
        shiny::uiOutput("chinn_direction_echo")
      )
    )
  }

  output$responder_p0_badge <- shiny::renderUI({
    if (responder_p0_confirmed()) {
      .ok_badge("confirmed")
    } else {
      .warn_badge("unconfirmed assumption")
    }
  })
  shiny::outputOptions(output, "responder_p0_badge",
                       suspendWhenHidden = FALSE)

  # ----- Configuration tab: centralized input panel -----------------
  output$threshold_panel <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) {
      return(htmltools::p("Run the analysis in Step 2 first."))
    }
    sm <- obj$sm %||% "OR"
    # Gate on the OUTCOME, not on the summary measure: a binary outcome
    # analysed as ARD, RD or HR used to fall through to the continuous
    # branch and lose the absolute-scale interface entirely.
    is_binary <- step3_is_binary_outcome(obj, input$outcome_type)
    sug_obj <- threshold_suggestion()
    src <- sug_obj$source
    # Same ordering caveat as the control-group risk above: fall back to the
    # suggestion when the seeding observer has not run yet.
    sug <- step3_threshold_suggestions(sug_obj)
    .abs_value <- function() {
      v <- shiny::isolate(threshold_abs_state())
      if (is.finite(v)) v else round(sug$absolute1000, 1)
    }
    .rel_value <- function() {
      v <- shiny::isolate(threshold_state())
      if (is.finite(v)) v else round(sug$relative, 4)
    }

    if (is_binary) {
      htmltools::tagList(
        .control_risk_block(),
        shiny::uiOutput("direction_echo"),
        .config_section(
          htmltools::tagList("Decision threshold", .source_badge(src)),
          shiny::radioButtons("threshold_mode", "Threshold scale",
            choices = c(
              "Absolute (per 1,000 patients) - recommended" = "absolute",
              "Relative (ratio)"                            = "relative"),
            selected = shiny::isolate(threshold_mode_state())),
          shiny::conditionalPanel(
            "input.threshold_mode == 'absolute'",
            .config_note(
              "Core GRADE recommends expressing the threshold on the ",
              "absolute scale: the smallest difference in events per 1,000 ",
              "patients that would matter for a decision. It is converted ",
              "to the ", sm, " scale at the control-group risk above."),
            shiny::numericInput("threshold_abs",
              "Threshold (events per 1,000 patients)",
              value = .abs_value(), min = 0, step = 5),
            shiny::uiOutput("threshold_equiv")
          ),
          shiny::conditionalPanel(
            "input.threshold_mode == 'relative'",
            shiny::numericInput("threshold_ratio",
              EDU_COPY$threshold_labels[[sm]] %||%
                "Threshold for clinical importance",
              value = .rel_value(), min = 0, step = 0.01),
            .config_note(EDU_COPY$threshold_help[[sm]] %||% "")
          ),
          .mic_note()
        )
      )
    } else {
      htmltools::tagList(
        .responder_block(sm),
        shiny::uiOutput("direction_echo"),
        .config_section(
          htmltools::tagList("Decision threshold", .source_badge(src)),
          shiny::numericInput("threshold_cont",
            EDU_COPY$threshold_labels[[sm]] %||%
              "Threshold for clinical importance",
            value = .rel_value(), min = 0, step = 0.01),
          .config_note(EDU_COPY$threshold_help[[sm]] %||% ""),
          if (identical(sm, "SMD")) {
            htmltools::p(
              class = "pma-card-subtitle", style = "font-style: italic;",
              paste0(
                "Core GRADE 6 describes a standardized mean difference of ",
                "0.2 as the threshold for a small and important effect, and ",
                "immediately qualifies it: clinicians may be appropriately ",
                "sceptical of this threshold, which is limited by large ",
                "variability in the methods investigators use to calculate ",
                "the standardized mean difference."))
          } else {
            .config_note(
              "This prefill is a pmatools convention (",
              if (identical(sm, "MD")) "0.20 x the pooled SD" else "1.10",
              "), not a value taken from Core GRADE. Replace it with a ",
              "published threshold for this instrument whenever one exists.")
          },
          .mic_note()
        )
      )
    }
  })
  shiny::outputOptions(output, "threshold_panel", suspendWhenHidden = FALSE)

  # The direction the pooled effect lies in, and the conversion the app
  # therefore uses. Read from state$ma, the Step 2 all-studies analysis: it
  # is the only analysis available before grade_meta() runs, and it is the
  # one Risk of Bias is assessed on. When Core GRADE 4 sends grade_meta()
  # off to refit on the low-risk subset and that refit flips the sign,
  # grade_obj() re-runs with the corrected direction (see there).
  .threshold_direction <- shiny::reactive(
    step3_threshold_direction(step3_pooled_te(state$ma)))

  # Live equivalent-effect display for the absolute mode. Shows BOTH
  # directions and names the one that is exact.
  #
  # The algorithm is symmetric on the log scale: whichever ratio it is given,
  # the opposite side it applies is that ratio inverted. So the app converts
  # the absolute threshold on the side the pooled effect lies (that is the
  # crossing the judgments turn on) and hands grade_meta() the ratio rather
  # than the ARD. The residual asymmetry cannot be removed - it is moved to
  # the other side, and named here. Same wording as threshold_summary_text()
  # below; these two are the app's copies.
  .equiv_lines <- function(eq, dir = NULL) {
    sm <- eq$sm
    up <- sprintf("Increase: %.0f -> %.0f per 1,000, equivalent %s %.3f",
                  1000 * eq$p0, 1000 * eq$p1_up, sm, eq$ratio_up)
    dn <- if (isTRUE(eq$down_ok)) {
      sprintf("Decrease: %.0f -> %.0f per 1,000, equivalent %s %.3f",
              1000 * eq$p0, 1000 * eq$p1_dn, sm, eq$ratio_dn)
    } else {
      "Decrease: not shown - the threshold exceeds the control-group risk."
    }
    if (is.null(dir)) {
      return(list(up = up, dn = dn, alg = character(), approx = character()))
    }
    alg <- sprintf(
      paste0("What the algorithm uses: a symmetric +/- log(%.3f) band, ",
             "converted on the %s side because the pooled effect lies %s ",
             "the null. That side is exact - %s %.3f is %+.0f per 1,000 at ",
             "this control-group risk."),
      dir$ratio, dir$exact_side,
      if (identical(dir$exact_side, "decrease")) "below" else "above",
      sm, dir$exact_ratio, 1000 * dir$exact_ard)
    approx <- sprintf(
      paste0("The %s side is therefore the approximate one: the band's ",
             "mirror is %s %.3f, which implies %+.0f per 1,000 rather than ",
             "%+.0f. Imprecision's two-level rule asks whether the ",
             "confidence interval crosses both thresholds, so that one ",
             "crossing is judged against the mirrored value."),
      dir$approx_side, sm, dir$approx_ratio,
      1000 * dir$approx_ard, -1000 * dir$exact_ard)
    if (!is.na(dir$caveat)) approx <- paste(approx, dir$caveat)
    list(up = up, dn = dn, alg = alg, approx = approx)
  }

  output$threshold_equiv <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    sm <- obj$sm %||% "OR"
    ta <- input$threshold_abs            %||% threshold_abs_state()
    tb <- input$threshold_baseline_input %||% threshold_baseline_state()
    eq <- step3_ard_equivalence(sm, ta, tb)
    if (is.null(eq)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "Enter a positive threshold and a control-group risk between 0 and ",
        "1,000 (threshold + control-group risk must stay below 1,000) to ",
        "see the equivalent relative effect."))
    }
    dir <- step3_directed_threshold(eq, .threshold_direction())
    ln  <- .equiv_lines(eq, dir)
    .exact_first <- identical(dir$exact_side %||% "increase", "decrease")
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f5f5f5; ",
        "border-left: 4px solid #0f172a; margin: 0.5rem 0; ",
        "font-size: 0.85rem;"),
      # The exact side is emboldened, so the reader can see at a glance which
      # of the two the judgments are anchored to.
      htmltools::div(if (.exact_first) ln$up else htmltools::strong(ln$up)),
      htmltools::div(if (.exact_first) htmltools::strong(ln$dn) else ln$dn),
      htmltools::div(
        style = "margin-top: 0.35rem;",
        sprintf("On other scales, the increase side is RR %.3f / OR %.3f.",
                eq$rr_up, eq$or_up)),
      htmltools::div(
        style = paste0("margin-top: 0.35rem; font-style: italic; ",
                       "color: hsl(var(--muted-foreground));"),
        ln$alg),
      if (length(ln$approx)) {
        htmltools::div(
          style = paste0("margin-top: 0.35rem; font-style: italic; ",
                         "color: hsl(var(--muted-foreground));"),
          ln$approx)
      }
    )
  })
  shiny::outputOptions(output, "threshold_equiv", suspendWhenHidden = FALSE)

  # Human-readable summary of the active threshold (for read-only blocks).
  # Returns a list so the read-only block can show the same two-direction
  # wording as output$threshold_equiv rather than a third variant.
  threshold_summary <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(head = "No threshold set - run the analysis first.",
                  lines = character()))
    }
    sm <- obj$sm %||% "OR"
    if (step3_is_binary_outcome(obj, input$outcome_type) &&
        identical(threshold_mode_state(), "absolute")) {
      ta <- threshold_abs_state()
      tb <- threshold_baseline_state()
      if (!is.finite(ta) || ta <= 0) {
        return(list(head = "Absolute threshold not set yet.",
                    lines = character()))
      }
      eq <- step3_ard_equivalence(sm, ta, tb)
      if (is.null(eq)) {
        return(list(head = sprintf(paste0(
          "Absolute threshold: %g per 1,000 (control-group risk missing ",
          "or invalid)"), ta), lines = character()))
      }
      dir <- step3_directed_threshold(eq, .threshold_direction())
      ln  <- .equiv_lines(eq, dir)
      return(list(
        head = sprintf(
          "Absolute threshold: %g per 1,000 at a control-group risk of %g per 1,000",
          ta, tb),
        lines = c(ln$up, ln$dn, ln$alg),
        approx = ln$approx))
    }
    th <- threshold_state()
    if (!is.finite(th)) {
      return(list(head = "Threshold not set yet.", lines = character()))
    }
    list(head = sprintf("Threshold: %s = %g", sm, th), lines = character())
  })
  threshold_summary_text <- shiny::reactive(threshold_summary()$head)

  # Read-only threshold display inside RoB / Inconsistency / Imprecision.
  #
  # `domain = "impre"` adds the residual-asymmetry sentence, because
  # Imprecision is the domain it bites in: its two-level rule tests the
  # confidence interval against both the important-benefit and the
  # important-harm threshold, and by construction only one of those two is
  # exact on the absolute scale.
  .render_threshold_readonly <- function(domain = NULL) {
    ts <- threshold_summary()
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f9f9f9; ",
        "border: 1px solid #e5e5e5; border-radius: 6px; margin: 0.5rem 0;"),
      htmltools::p(style = "margin: 0; font-size: 0.9rem;",
        htmltools::strong(ts$head)),
      if (length(ts$lines)) {
        htmltools::div(
          style = paste0("margin: 0.25rem 0 0; font-size: 0.85rem; ",
                         "color: hsl(var(--muted-foreground));"),
          lapply(ts$lines, htmltools::div))
      },
      if (identical(domain, "impre") && length(ts$approx %||% character())) {
        htmltools::div(
          style = paste0("margin: 0.35rem 0 0; font-size: 0.85rem; ",
                         "font-style: italic; ",
                         "color: hsl(var(--muted-foreground));"),
          ts$approx)
      },
      htmltools::p(
        class = "pma-card-subtitle",
        style = "margin: 0.25rem 0 0;",
        "This decision threshold is shared by Risk of Bias, Inconsistency, ",
        "and Imprecision. Change it in the Configuration tab.")
    )
  }
  output$threshold_block_rob   <- shiny::renderUI(.render_threshold_readonly("rob"))
  output$threshold_block_inco  <- shiny::renderUI(.render_threshold_readonly("inco"))
  output$threshold_block_impre <- shiny::renderUI(.render_threshold_readonly("impre"))
  shiny::outputOptions(output, "threshold_block_rob",   suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_inco",  suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "threshold_block_impre", suspendWhenHidden = FALSE)

  # grade_meta() threshold arguments derived from the active mode.
  # Gated on the outcome type, matching output$threshold_panel: a binary
  # outcome analysed as something other than OR / RR still has an absolute
  # threshold to convert.
  #
  # For a binary outcome with an absolute threshold the app converts to the
  # ratio scale ITSELF and passes threshold_scale = "ratio", rather than
  # handing pmatools the ARD. threshold_scale = "ard" always converts on the
  # increase side (T = ratio implied by p0 + ard) and every domain then judges
  # against the symmetric band +/- log(T), whose decrease side 1 / T is not
  # the ratio implied by p0 - ard. Converting in the direction the pooled
  # effect actually lies makes the comparison that decides each judgment exact
  # on the absolute scale, which is the scale Core GRADE 7 puts the threshold
  # on. `direction`, `dir` and `note` are carried along so grade_obj() can
  # detect a refit-induced flip and rebuild the provenance note that
  # threshold_scale = "ratio" no longer gets from pmatools.
  #
  # `te_point` overrides the direction source; it defaults to the Step 2
  # all-studies analysis.
  #
  # Falls back to the previous threshold_scale = "ard" call whenever the app
  # cannot do the conversion (control-group risk missing or out of range), so
  # pmatools' own validation and error messages still apply there.
  .threshold_grade_args <- function(obj, te_point = NULL) {
    sm <- obj$sm %||% "OR"
    if (step3_is_binary_outcome(obj, shiny::isolate(input$outcome_type)) &&
        identical(threshold_mode_state(), "absolute")) {
      ta <- threshold_abs_state()
      tb <- threshold_baseline_state()
      if (is.finite(ta) && ta > 0) {
        base <- if (is.finite(tb) && tb > 0 && tb < 1000 &&
                    (tb + ta) < 1000) tb / 1000 else NULL
        direction <- step3_threshold_direction(
          if (is.null(te_point)) step3_pooled_te(obj) else te_point)
        dir <- step3_directed_threshold(step3_ard_equivalence(sm, ta, tb),
                                        direction)
        if (!is.null(dir) && is.finite(dir$ratio) && dir$ratio > 1) {
          return(list(threshold          = dir$ratio,
                      threshold_scale    = "ratio",
                      threshold_baseline = NULL,
                      direction          = direction,
                      dir                = dir,
                      note               = step3_threshold_note(dir)))
        }
        return(list(threshold          = ta / 1000,
                    threshold_scale    = "ard",
                    threshold_baseline = base,
                    direction          = NULL, dir = NULL, note = NULL))
      }
      return(list(threshold = NULL, threshold_scale = "auto",
                  threshold_baseline = NULL,
                  direction = NULL, dir = NULL, note = NULL))
    }
    th <- threshold_state()
    list(
      threshold = if (is.numeric(th) && !is.na(th) && th > 0) th else NULL,
      threshold_scale    = "auto",
      threshold_baseline = NULL,
      direction = NULL, dir = NULL, note = NULL
    )
  }

  # ----- OIS default values -----
  # The OIS baseline is no longer a second, independently computed crude
  # ratio: it IS the Configuration control-group risk, shown read-only, so
  # the two cannot disagree. grade_obj() passes the same number as ois_p0.
  output$ois_p0_ui <- shiny::renderUI({
    tb <- threshold_baseline_state()
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f9f9f9; ",
        "border: 1px solid #e5e5e5; border-radius: 6px; margin: 0.5rem 0;"),
      htmltools::p(style = "margin: 0; font-size: 0.9rem;",
        htmltools::strong(
          if (is.finite(tb)) {
            sprintf("Control-group event rate for the OIS: %.4f (%g per 1,000)",
                    tb / 1000, tb)
          } else {
            "Control-group event rate for the OIS: not set"
          })),
      htmltools::p(class = "pma-card-subtitle", style = "margin: 0.25rem 0 0;",
        "Taken from the Configuration tab, where it is set once. Change it ",
        "there.")
    )
  })
  shiny::outputOptions(output, "ois_p0_ui", suspendWhenHidden = FALSE)

  # Control-group risk as a proportion, for grade_meta(ois_p0 = ).
  ois_p0_value <- shiny::reactive({
    tb <- threshold_baseline_state()
    if (is.finite(tb) && tb > 0 && tb < 1000) tb / 1000 else NULL
  })

  # The modest relative risk reduction the binary OIS is powered to detect.
  # Core GRADE 2 names 20 percent and 25 percent; the radio offers both and
  # 0.20 is pmatools' default.
  ois_rrr_value <- shiny::reactive({
    v <- suppressWarnings(as.numeric(input$ois_rrr %||% "0.20"))
    if (!is.finite(v) || v <= 0 || v >= 1) 0.20 else v
  })

  # The RRR read on the absolute scale, so the OIS target can be compared
  # with the Configuration threshold in the same units. The control-group
  # risk is the one the Configuration tab established; it is not recomputed
  # here.
  output$ois_rrr_equiv <- shiny::renderUI({
    rrr <- ois_rrr_value()
    tb  <- threshold_baseline_state()
    if (!is.finite(tb) || tb <= 0 || tb >= 1000) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        paste0("Set a control-group risk on the Configuration tab to see the ",
               "absolute-scale equivalent of this relative risk reduction.")))
    }
    # Round the control-group risk before applying the RRR so the three
    # displayed numbers add up; the calculation itself uses the unrounded
    # rate (ois_p0 = threshold baseline / 1,000).
    p0d <- round(tb)
    p1d <- round(p0d * (1 - rrr))
    dif <- p0d - p1d
    htmltools::div(
      style = paste0(
        "padding: 0.5rem 0.75rem; background: #f5f5f5; ",
        "border-left: 4px solid #0f172a; margin: 0.5rem 0; ",
        "font-size: 0.85rem;"),
      htmltools::p(style = "margin: 0;",
        htmltools::strong(sprintf(
          "RRR %.0f%% = %.0f -> %.0f per 1,000 (%.0f fewer per 1,000)",
          100 * rrr, p0d, p1d, dif))),
      htmltools::p(style = "margin: 0.25rem 0 0;",
        sprintf(paste0("The OIS is powered to detect this difference at ",
                       "alpha = 0.05 and beta = 0.20, using the ",
                       "control-group risk of %g per 1,000 set on the ",
                       "Configuration tab. It is a separate quantity from ",
                       "the decision threshold and is not derived from it."),
                tb)),
      htmltools::p(style = paste0("margin: 0.35rem 0 0; font-style: italic; ",
                                  "color: hsl(var(--muted-foreground));"),
        paste0("Core GRADE 2, verbatim: 'For binary outcomes, these involve ",
               "specifying the acceptable error rates: alpha (typically ",
               "0.05) and beta (typically 0.20), the control group event ",
               "rate (chosen from the context), and a modest relative risk ",
               "reduction, typically 20% or 25%.' Core GRADE's separate ",
               "advice that binary thresholds belong on the absolute scale ",
               "concerns thresholds, not the OIS."))
    )
  })
  shiny::outputOptions(output, "ois_rrr_equiv", suspendWhenHidden = FALSE)

  output$ois_sd_ui <- shiny::renderUI({
    obj <- state$ma
    val <- if (!is.null(obj)) {
      sd_pooled <- tryCatch(compute_pooled_sd(obj),
                            error = function(e) NULL)
      if (!is.null(sd_pooled) && is.finite(sd_pooled) && sd_pooled > 0) {
        round(sd_pooled, 4)
      } else NA
    } else NA
    shiny::numericInput("ois_sd",
      "Pooled SD for OIS (auto from data)",
      value = val, min = 0, step = 0.1)
  })

  .na_null <- function(x) {
    if (is.null(x)) return(NULL)
    if (length(x) == 0) return(NULL)
    # An empty numericInput arrives as logical NA, not NA_real_, so test for
    # NA before looking at the type: is.numeric(NA) is FALSE and a type-first
    # check let empty OIS fields count as user input (W4-A gate) and reach
    # grade_meta() as NA instead of NULL.
    if (all(is.na(x))) return(NULL)
    if (is.character(x) && !any(nzchar(x))) return(NULL)
    x
  }

  # Which side of the binary low/high split "some concerns" (and, through the
  # "*" default of the rob vector, an unrated study) falls on. Reviewer
  # choice; "high" until the radio group reports in, so the first render
  # matches the documented default rather than the vendored one.
  .rob_some_concerns_setting <- function() {
    v <- input$rob_some_concerns
    if (is.null(v) || length(v) != 1L || !v %in% c("low", "high")) "high" else v
  }

  .study_labels_for_grade <- function(obj) {
    studs <- as.character(obj$studlab)
    if (!length(studs)) return(character())
    if (identical(attr(obj, "pma_rare_engine"), "mmeta")) return(studs)
    if (!is.null(obj$k) && length(studs) == obj$k) return(studs)
    keep <- !is.na(obj$TE)
    if (length(keep) == length(studs)) return(studs[keep])
    studs
  }

  .study_covariate <- function(labels, col, default = NA_character_) {
    labels <- as.character(labels)
    source <- NULL
    rt <- state$rob_table
    if (!is.null(rt) && col %in% names(rt)) {
      source <- rt[, c("studlab", col), drop = FALSE]
    } else {
      d <- state$data
      if (!is.null(d) && col %in% names(d)) {
        first_per_study <- !duplicated(d$studlab)
        source <- d[first_per_study, c("studlab", col), drop = FALSE]
      }
    }
    if (is.null(source)) return(NULL)
    lookup <- as.character(source[[col]])
    names(lookup) <- as.character(source$studlab)
    out <- unname(lookup[labels])
    if (!is.null(default)) out[is.na(out) | !nzchar(trimws(out))] <- default
    out
  }

  .effective_pubias_k <- function(obj) {
    te <- obj$TE
    se <- obj$seTE
    if (!is.null(te) && !is.null(se) &&
        length(te) == length(se) && length(te) > 0L) {
      return(sum(is.finite(te) & is.finite(se) & se > 0))
    }
    obj$k %||% 0L
  }

  # Non-empty scalar select value or NULL.
  .sel_val <- function(id) {
    v <- input[[id]]
    if (is.null(v) || length(v) == 0 || !nzchar(v)) NULL else v
  }
  # Trimmed non-empty rationale text or NULL.
  .rat_val <- function(id) {
    v <- input[[id]]
    if (is.null(v) || length(v) == 0) return(NULL)
    v <- trimws(as.character(v)[1])
    if (nzchar(v)) v else NULL
  }
  # Resolve an override select + its mandatory rationale. Returns
  # list(value=, rationale=); value is NULL (override ignored, with a
  # notification) while the rationale is missing, so grade_meta() can
  # never see a rationale-less override (v0.4.0 would abort).
  .override_or_ignore <- function(sel_id, rat_id, domain_label) {
    sel <- .sel_val(sel_id)
    if (is.null(sel)) return(list(value = NULL, rationale = NULL))
    rat <- .rat_val(rat_id)
    if (is.null(rat)) {
      shiny::showNotification(
        sprintf(paste0("%s: override ignored - a written rationale is ",
                       "required for manual overrides."), domain_label),
        id = paste0(sel_id, "_rationale_missing"),
        type = "warning", duration = 6)
      return(list(value = NULL, rationale = NULL))
    }
    list(value = sel, rationale = rat)
  }

  # ----- Indirectness subdomains (Core GRADE 5 / pmatools 0.5) ------------
  # The four PICO answers, shaped as the data frame grade_meta() documents
  # for indirectness_subdomains: one row per subdomain, `subdomain` and
  # `judgment` required, `target` / `evidence` optional and display-only.
  # NULL while nothing has been answered, so an unanswered domain falls back
  # to the scalar path rather than asserting four "yes" answers.
  indir_subdomains <- shiny::reactive({
    ans <- vapply(STEP3_INDIR_SUBDOMAINS, function(id) {
      v <- input[[id]]
      if (is.null(v) || length(v) != 1L || !nzchar(v)) NA_character_
      else as.character(v)
    }, character(1))
    keep <- !is.na(ans) & ans %in% STEP3_INDIR_ANSWERS
    if (!any(keep)) return(NULL)
    data.frame(
      subdomain = names(STEP3_INDIR_SUBDOMAINS)[keep],
      judgment  = unname(ans[keep]),
      stringsAsFactors = FALSE
    )
  })

  # Worst case across the answered subdomains. Mirrors the symmetric fold
  # pmatools applies (.indirectness_worst_case), so the app can tell a
  # restatement of the automatic judgment (no rationale needed) from a real
  # override (rationale required) without a second grade_meta() call.
  indir_worst_case <- shiny::reactive({
    sd <- indir_subdomains()
    if (is.null(sd)) return(NULL)
    lv  <- unname(STEP3_INDIR_ANSWER_TO_LEVEL[sd$judgment])
    ord <- c(no = 1L, some_concerns = 2L, serious = 3L)
    names(which.max(ord[lv]))
  })

  grade_obj <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(NULL)

    # The `require_threshold = FALSE` bridge added for pmatools 0.5.1 is
    # gone. It was a temporary opt-out that let grade_meta() rate an outcome
    # with no threshold at all, which is exactly the silent behaviour Core
    # GRADE warns against - three of the five domains would have been judged
    # against nothing. Deleting it alone would crash the tab, because
    # grade_meta() aborts on a NULL threshold under the default
    # threshold_type = "mid", and a reviewer can still clear the field. So
    # the app never makes that call: with no threshold, grade_obj() returns
    # NULL and threshold_missing() drives an explicit on-screen state
    # (output$config_status, the read-only domain blocks and
    # output$final_certainty). No error toast, and no rating computed
    # without a threshold.
    if (is.null(.threshold_grade_args(obj)$threshold)) return(NULL)

    # --- Risk of bias: per-study vector, or scalar override + rationale ---
    rob_arg <- .study_covariate(.study_labels_for_grade(obj), "rob", default = "*")
    rob_rationale <- NULL
    rob_ov <- .override_or_ignore("rob_override", "rob_override_rationale",
                                  "Risk of Bias")
    if (!is.null(rob_ov$value)) {
      rob_arg       <- rob_ov$value
      rob_rationale <- rob_ov$rationale
    }

    # Outcome direction is a required Step 2 answer, mirrored into state.
    sv <- state$small_values

    # --- Inconsistency: scalar override + rationale, or manual flowchart ---
    incon_ov <- .override_or_ignore("incon_override",
                                    "incon_override_rationale",
                                    "Inconsistency")
    ci_diff <- if (nzchar(input$ci_diff %||% "")) input$ci_diff else NULL
    threshold_side <- if (!is.null(input$threshold_side) &&
                          length(input$threshold_side) > 0 &&
                          nzchar(input$threshold_side)) input$threshold_side else NULL
    subgroup_expl <- if (!is.null(input$subgroup_explained) &&
                         length(input$subgroup_explained) > 0 &&
                         nzchar(input$subgroup_explained)) input$subgroup_explained else NULL

    # --- Indirectness: the four Core GRADE 5 subdomain answers, plus an
    # optional scalar override of their worst-case fold.
    #
    # `indirectness` MUST be NULL - not "no" - whenever no override is
    # intended: grade_meta() reads any non-NULL scalar alongside a subdomain
    # table as a manual override and demands indirectness_rationale for it.
    # With no subdomain answers the scalar path is unchanged and "no" is the
    # safe default; the confirmation gate (not an error) is what tells the
    # user the domain is still unassessed.
    indir_sub       <- indir_subdomains()
    indir_worst     <- indir_worst_case()
    indir_arg       <- if (is.null(indir_sub)) "no" else NULL
    indir_rationale <- NULL
    indir_sel <- input$indirectness
    if (!is.null(indir_sel) && length(indir_sel) == 1 && nzchar(indir_sel)) {
      auto_level <- indir_worst %||% "no"
      if (identical(indir_sel, auto_level)) {
        # A restatement of the automatic judgment: accepted without a
        # rationale, and it changes nothing.
        indir_arg <- indir_sel
      } else {
        r <- .rat_val("indir_rationale")
        if (is.null(r)) {
          shiny::showNotification(
            paste0("Indirectness: overall rating ignored - a written ",
                   "rationale is required whenever it differs from the ",
                   "automatic judgment."),
            id = "indir_rationale_missing", type = "warning", duration = 6)
        } else {
          indir_arg       <- indir_sel
          indir_rationale <- r
        }
      }
    }

    # --- Imprecision: scalar override + rationale (vendored v0.4.0 API) ---
    impre_ov <- .override_or_ignore("impre_override",
                                    "impre_override_rationale",
                                    "Imprecision")

    # --- Publication bias ---
    pubias_si <- if (nzchar(input$pubias_small_industry %||% "")) input$pubias_small_industry else NULL
    pubias_un <- if (nzchar(input$pubias_unpublished %||% "")) input$pubias_unpublished else NULL
    pubias_rc <- if (nzchar(input$pubias_registry_complete %||% "")) input$pubias_registry_complete else NULL
    # Visual override of Egger's test: v0.4.0 requires pubias_rationale
    # whenever pubias_funnel_asymmetry is supplied.
    pubias_fa       <- NULL
    pubias_rationale <- NULL
    fa_ov <- .override_or_ignore("pubias_funnel_asymmetry",
                                 "pubias_fa_rationale",
                                 "Publication bias (visual override of Egger)")
    if (!is.null(fa_ov$value)) {
      pubias_fa        <- fa_ov$value
      pubias_rationale <- fa_ov$rationale
    }
    # Final scalar override (app-level; grade_meta has no scalar
    # publication-bias override parameter).
    pubias_ov_res <- .override_or_ignore("pubias_override",
                                         "pubias_override_rationale",
                                         "Publication bias")
    pubias_ov <- pubias_ov_res$value

    th_args <- .threshold_grade_args(obj)

    args <- list(
      meta_obj                 = obj,
      study_design             = "RCT",
      rob                      = rob_arg,
      rob_rationale            = rob_rationale,
      # Where the low/high boundary falls. The app defaults to "high" (only
      # studies explicitly rated low are low), not the vendored default
      # "low"; unrated studies normalise to 'some concerns' (the "*" default
      # of rob_arg above) and so follow the same side. Core GRADE 4 endorses
      # the binary split but leaves the boundary open, so it is a reviewer
      # choice, exposed on the tab.
      rob_some_concerns        = .rob_some_concerns_setting(),
      rob_inflation_threshold  = input$rob_inf_threshold %||% 0.10,
      small_values             = sv,
      indirectness             = indir_arg,
      indirectness_rationale   = indir_rationale,
      # Core GRADE 5 asks the indirectness question per PICO element; the
      # four answers on the tab are that table. pmatools folds them
      # worst-case, which does NOT reproduce the Table 2 gradient - the tab
      # says so next to the questions.
      indirectness_subdomains  = indir_sub,
      inconsistency            = incon_ov$value,
      inconsistency_rationale  = incon_ov$rationale,
      inconsistency_ci_diff            = ci_diff,
      inconsistency_threshold_side     = threshold_side,
      inconsistency_subgroup_explained = subgroup_expl,
      imprecision              = impre_ov$value,
      imprecision_rationale    = impre_ov$rationale,
      threshold          = th_args$threshold,
      threshold_scale    = th_args$threshold_scale,
      threshold_baseline = th_args$threshold_baseline,
      outcome_type = if (identical(input$outcome_type, "binary")) "relative" else "absolute",
      # Same control-group risk the Configuration tab shows, not a second
      # crude computation of its own.
      ois_p0       = ois_p0_value(),
      # Core GRADE 2 parameterises the binary OIS by a modest relative risk
      # reduction ("typically 20% or 25%"), not by the threshold. Reviewer
      # choice between the two values the paper names.
      ois_rrr      = ois_rrr_value(),
      ois_sd       = .na_null(input$ois_sd),
      ois_events   = .na_null(input$ois_events_override),
      ois_n        = .na_null(input$ois_n_override),
      pubias_small_industry    = pubias_si,
      pubias_funnel_asymmetry  = pubias_fa,
      pubias_rationale         = pubias_rationale,
      pubias_unpublished       = pubias_un,
      # Q1: only "yes" (denied) is forwarded to the package short-circuit;
      # "no" (suspected) is handled by a post-override below so it forces
      # rate-down regardless of Q2-Q5.
      pubias_registry_complete = if (identical(pubias_rc, "yes")) "yes" else NULL,
      outcome_name = state$outcome_name %||% "Outcome"
    )

    .run_grade <- function(th) {
      args$threshold          <- th$threshold
      args$threshold_scale    <- th$threshold_scale
      args$threshold_baseline <- th$threshold_baseline
      tryCatch(
        suppressWarnings(do.call(grade_meta, args)),
        error = function(e) {
          shiny::showNotification(
            paste("grade_meta error:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    }
    g <- .run_grade(th_args)

    # Core GRADE 4 can send grade_meta() off to refit on the low risk-of-bias
    # subset, and the rating target plus the other four domains are then read
    # off THAT analysis. The directed conversion above used the all-studies
    # pooled effect, because it is the only one available before the call. If
    # the refit put the pooled effect on the other side of the null, the
    # threshold was converted in the wrong direction, so re-run once against
    # the refitted analysis. One correction, not a fixed point: the threshold
    # feeds back into the Risk of Bias analysis-set decision, so iterating
    # could oscillate.
    if (!is.null(g) && !is.null(th_args$direction) && isTRUE(g$rob_refit)) {
      th2 <- .threshold_grade_args(obj, te_point = step3_pooled_te(g$meta))
      if (!identical(th2$direction, th_args$direction) &&
          !is.null(th2$threshold)) {
        g2 <- .run_grade(th2)
        if (!is.null(g2)) {
          th_args <- th2
          g       <- g2
          g$domain_assessments <- step3_append_domain_note(
            g$domain_assessments, "Risk of bias",
            paste0("The absolute threshold was re-converted on the ",
                   th2$dir$exact_side, " side after the refit on the low ",
                   "risk of bias studies moved the pooled effect to the ",
                   "other side of the null."))
        }
      }
    }

    # threshold_scale = "ratio" means pmatools no longer returns
    # $threshold_ard or the $threshold_note it used to append to the three
    # threshold-aware domains and to the Evidence Profile footnote. Rebuild
    # both here, in absolute terms, so nothing loses that provenance.
    if (!is.null(g) && !is.null(th_args$note)) {
      g$threshold_note     <- th_args$note
      g$threshold_ard      <- th_args$dir$ard
      g$threshold_baseline <- th_args$dir$p0
      for (dom in c("Risk of bias", "Inconsistency", "Imprecision")) {
        g$domain_assessments <- step3_append_domain_note(
          g$domain_assessments, dom, th_args$note)
      }
      # .derive_rating_target() writes a scale note off threshold_kind, which
      # is now "ratio", so it says the target was derived on the relative
      # scale and recommends threshold_scale = "ard". Both are wrong here -
      # the threshold IS absolute, converted app-side - so swap the sentence
      # for one that describes what happened. A no-op if the vendored wording
      # ever changes.
      sm_g   <- obj$sm %||% "OR"
      stale  <- paste0(
        " Target derived on the relative-effect scale (", sm_g,
        "); Core GRADE 2 recommends an absolute-effect threshold ",
        "(threshold_scale = 'ard') where a baseline risk is available.")
      fresh <- sprintf(paste0(
        " Target derived from the absolute-effect threshold (%g per 1,000 at ",
        "a baseline risk %g per 1,000), converted to the %s scale on the %s ",
        "side."), 1000 * th_args$dir$ard, 1000 * th_args$dir$p0, sm_g,
        th_args$dir$exact_side)
      if (!is.null(g$rating_target_note)) {
        g$rating_target_note <- sub(stale, fresh, g$rating_target_note,
                                    fixed = TRUE)
      }
      idx <- which(g$domain_assessments$domain == "Imprecision")
      if (length(idx)) {
        g$domain_assessments$notes[idx] <-
          sub(stale, fresh, g$domain_assessments$notes[idx], fixed = TRUE)
      }
    }

    if (!is.null(g)) {
      # Q1 = "no" (reporting bias suspected) forces rate-down 1 regardless
      # of Q2-Q5. Run BEFORE pubias_ov so the manual override below can still
      # win if the user explicitly sets it.
      if (identical(pubias_rc, "no")) {
        idx <- which(g$domain_assessments$domain == "Publication bias")
        if (length(idx)) {
          g$domain_assessments$judgment[idx]  <- "some_concerns"
          g$domain_assessments$auto[idx]      <- FALSE
          g$domain_assessments$downgrade[idx] <- -1L
          g$domain_assessments$notes[idx] <- paste0(
            "Q1: reporting bias suspected based on the overall judgment ",
            "of the listed conditions; rate down 1 (some concerns) regardless ",
            "of Q2-Q5. | ", g$domain_assessments$notes[idx])
        }
      }
      # Final scalar publication-bias override (app-level; recorded in the
      # notes in the same "Manual override (<judgment>): <rationale>"
      # format the vendored make_domain_row() uses).
      if (!is.null(pubias_ov)) {
        idx <- which(g$domain_assessments$domain == "Publication bias")
        if (length(idx)) {
          g$domain_assessments$judgment[idx] <- pubias_ov
          g$domain_assessments$auto[idx]     <- FALSE
          g$domain_assessments$downgrade[idx] <- pmatools_GRADE_DOWNGRADE(pubias_ov)
          g$domain_assessments$notes[idx] <- paste0(
            sprintf("Manual override (%s): %s", pubias_ov,
                    pubias_ov_res$rationale),
            " | ", g$domain_assessments$notes[idx])
        }
      }
      # Additional user-specified downgrade from "Other considerations"
      other_dg <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
      if (is.na(other_dg)) other_dg <- 0L
      other_dg <- min(0L, other_dg)  # cannot rate UP via this control

      total_dg <- sum(g$domain_assessments$downgrade) + other_dg
      score    <- max(1L, 4L + total_dg)
      g$certainty_score    <- score
      g$certainty          <- c("Very Low","Low","Moderate","High")[score]
      g$other_text         <- input$other_text
      g$other_downgrade    <- other_dg

      # The exact grade_meta() call that produced this rating, shaped as the
      # {value, origin, col} specs export_bundle() renders analysis.R from
      # (see pma_grade_arg_specs() in ui_helpers.R). Carried on the object
      # rather than in a separate reactive value so it cannot drift away from
      # the rating it describes; Step 4 reads it back off state$grade.
      #
      # th_args, not args$threshold: the low-risk-of-bias refit above can
      # re-convert the threshold, and the script must reproduce the conversion
      # that was actually rated against.
      args$threshold          <- th_args$threshold
      args$threshold_scale    <- th_args$threshold_scale
      args$threshold_baseline <- th_args$threshold_baseline
      attr(g, PMA_GRADE_ARGS_ATTR) <- pma_grade_arg_specs(args)
    }

    g
  })

  shiny::observe({
    g <- grade_obj()
    if (!is.null(g)) state$grade <- g
  })

  # ----- W4-A: per-domain confirmation state (output gate) ----------------
  # A domain counts as confirmed when it has substantive user input, or
  # when its explicit "I have reviewed this domain" checkbox is ticked.
  # Progression through tabs stays free; only outputs are gated.
  #
  # Every read below goes through .fresh(): an answer given for a PREVIOUS
  # outcome must not confirm this one, however the reviewer got here (see the
  # .answer_gen note above and begin_new_outcome() in app.R).
  .valid_override <- function(sel_id, rat_id) {
    sel <- input[[sel_id]]
    rat <- input[[rat_id]]
    .fresh(sel_id) &&
      !is.null(sel) && length(sel) == 1 && nzchar(sel) &&
      !is.null(rat) && nzchar(trimws(rat))
  }
  .answered <- function(id) {
    v <- input[[id]]
    .fresh(id) && !is.null(v) && length(v) > 0 && nzchar(v[1])
  }
  .confirmed_na <- function(id) .fresh(id) && isTRUE(input[[id]])

  # ----- Configuration gate -----------------------------------------------
  # Everything that must be settled before the reviewer starts on the five
  # domains, as a list of human-readable blockers. Empty means ready.
  #
  # Configuration is the ONE Step-3 sub-tab whose Next is gated. The recently
  # established convention is that only the last sub-tab (Final certainty) is
  # gated, so reviewers can move freely between domains; Configuration is a
  # deliberate exception, because the threshold it sets drives Risk of Bias,
  # Inconsistency and Imprecision, and walking those three against an
  # unsettled threshold means doing them twice. No other sub-tab is gated.
  threshold_missing <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return(TRUE)
    is.null(.threshold_grade_args(obj)$threshold)
  })

  config_blockers <- shiny::reactive({
    obj <- state$ma
    if (is.null(obj)) return("run the meta-analysis in Step 2")
    out <- character()
    if (threshold_missing()) {
      out <- c(out, "enter a decision threshold above zero")
    }
    if (step3_is_binary_outcome(obj, input$outcome_type)) {
      if (!baseline_rationale_ok()) {
        out <- c(out, paste0("give a rationale for replacing the pooled ",
                             "control-group risk"))
      }
    } else if (isTRUE(input$convert_smd_to_or)) {
      if (!responder_p0_valid()) {
        out <- c(out, paste0("enter a control-group responder proportion ",
                             "between 0 and 1"))
      } else if (!responder_p0_confirmed()) {
        out <- c(out, paste0("confirm the 20 percent control-group responder ",
                             "proportion, or replace it and say why"))
      }
    }
    if (!isTRUE(input$threshold_confirm)) {
      out <- c(out, "tick the confirmation box below")
    }
    out
  })

  output$grade_nav_config <- shiny::renderUI({
    .grade_nav("grade_back_thresh", "Back: Meta-analysis",
               "grade_next_thresh", "Next: Risk of Bias",
               next_disabled = length(config_blockers()) > 0)
  })
  shiny::outputOptions(output, "grade_nav_config", suspendWhenHidden = FALSE)

  # Explicit on-screen state for the Configuration tab, including the
  # "no threshold" case that used to be papered over by require_threshold.
  output$config_status <- shiny::renderUI({
    blockers <- config_blockers()
    if (!length(blockers)) {
      return(htmltools::div(
        style = paste0(
          "padding: 0.5rem 0.75rem; margin: 0.5rem 0; ",
          "background: #ecfdf5; border-left: 4px solid #047857; ",
          "border-radius: 4px; font-size: 0.9rem;"),
        htmltools::strong("Configuration complete. "),
        "The five certainty domains are rated against these values."))
    }
    htmltools::div(
      style = paste0(
        "padding: 0.75rem 1rem; margin: 0.5rem 0; ",
        "background: ", PMA_ALERT_BG, "; border-left: 4px solid ",
        PMA_ALERT_FG, "; border-radius: 4px; font-size: 0.9rem;"),
      htmltools::strong("Configuration incomplete. "),
      if (threshold_missing()) {
        paste0("No decision threshold is set, so no certainty rating is ",
               "computed: three of the five domains are judged against it. ")
      } else "",
      sprintf("Still to do: %s.", paste(blockers, collapse = "; "))
    )
  })
  shiny::outputOptions(output, "config_status", suspendWhenHidden = FALSE)

  domain_confirmed <- shiny::reactive({
    rt <- state$rob_table
    rob_data <- !is.null(rt) && "rob" %in% names(rt) &&
      any(!is.na(rt$rob) & nzchar(trimws(as.character(rt$rob))))

    # Answering any of the four Core GRADE 5 subdomain questions counts as
    # substantive input on its own: those answers now reach grade_meta() and
    # decide the domain judgment.
    indir_sel <- input$indirectness
    indir_pico <- c("indir_population", "indir_intervention",
                    "indir_comparator", "indir_outcome")
    indir_active <-
      (!is.null(indir_subdomains()) &&
         any(vapply(indir_pico, .fresh, logical(1)))) ||
      (.fresh("indirectness") &&
         !is.null(indir_sel) && length(indir_sel) == 1 &&
         nzchar(indir_sel) &&
         (identical(indir_sel, "no") ||
            nzchar(trimws(input$indir_rationale %||% ""))))

    c(
      # config_blockers() already requires input$threshold_confirm, so adding
      # the freshness test to that one id is enough to say "confirmed FOR THIS
      # outcome" without restating the whole gate.
      threshold = length(config_blockers()) == 0L &&
        .fresh("threshold_confirm"),
      # rob_data is the per-study risk-of-bias table, which describes the
      # studies rather than the outcome and is deliberately kept across a
      # change of outcome - so it legitimately re-confirms this domain.
      rob = rob_data ||
        .valid_override("rob_override", "rob_override_rationale") ||
        .confirmed_na("rob_confirm_na"),
      inconsistency = .answered("ci_diff") ||
        .valid_override("incon_override", "incon_override_rationale") ||
        .confirmed_na("incon_confirm_na"),
      indirectness = indir_active || .confirmed_na("indir_confirm_na"),
      imprecision = (.fresh("ois_events_override") &&
                       !is.null(.na_null(input$ois_events_override))) ||
        (.fresh("ois_n_override") &&
           !is.null(.na_null(input$ois_n_override))) ||
        .valid_override("impre_override", "impre_override_rationale") ||
        .confirmed_na("impre_confirm_na"),
      pubias = .answered("pubias_registry_complete") ||
        .answered("pubias_small_industry") ||
        .answered("pubias_unpublished") ||
        .valid_override("pubias_funnel_asymmetry", "pubias_fa_rationale") ||
        .valid_override("pubias_override", "pubias_override_rationale") ||
        .confirmed_na("pubias_confirm_na")
    )
  })

  # Mirror into state so Step 4 (export gate) can read it.
  shiny::observe({
    state$domain_confirmed <- domain_confirmed()
  })

  # Nav on the Final certainty tab. Its Next is the one that leaves Step 3,
  # so it carries the same signal as the Step 4 download gate: enabled only
  # once every domain is confirmed. The other six sub-tabs keep a plain,
  # always-enabled Next - moving between sub-tabs is never gated.
  output$grade_nav_final <- shiny::renderUI({
    .grade_nav("grade_back_final", "Back: Publication bias",
               "grade_next_final", "Next: Export",
               next_disabled = length(
                 pma_unconfirmed_domains(domain_confirmed())) > 0)
  })

  # Banner on the Final certainty tab while domains remain unconfirmed.
  output$cert_incomplete_banner <- shiny::renderUI({
    unconf <- pma_unconfirmed_domains(domain_confirmed())
    if (!length(unconf)) return(NULL)
    htmltools::div(
      style = paste0(
        "padding: 0.75rem 1rem; margin-bottom: 1rem; ",
        "background: #fef3c7; border-left: 4px solid #b45309; ",
        "border-radius: 4px; font-size: 0.9rem;"),
      htmltools::strong("Assessment incomplete. "),
      sprintf(paste0(
        "The certainty shown below is provisional until every domain has ",
        "been reviewed. Unconfirmed: %s. "), paste(unconf, collapse = ", ")),
      "Provide inputs in each tab, or tick 'I have reviewed this domain' ",
      "to confirm it as-is. Export (Step 4) stays locked until then."
    )
  })
  shiny::outputOptions(output, "cert_incomplete_banner",
                       suspendWhenHidden = FALSE)

  pmatools_GRADE_DOWNGRADE <- function(j) {
    # 3-level system (v0.3+): -1 = some_concerns, -2 = serious. Legacy
    # labels are still mapped so old user input doesn't break.
    c(no = 0, some = -1, some_concerns = -1,
      serious = -2, very_serious = -2)[[j]]
  }

  domain_judgment <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    row <- g$domain_assessments[g$domain_assessments$domain == domain, ]
    if (nrow(row) == 0) return(NULL)
    row$judgment[1]
  }

  domain_notes <- function(domain) {
    g <- grade_obj()
    if (is.null(g)) return("")
    row <- g$domain_assessments[g$domain_assessments$domain == domain, ]
    if (nrow(row) == 0) return("")
    row$notes[1]
  }

  output$rob_badge    <- shiny::renderUI(pma_judgment_badge(domain_judgment("Risk of bias")    %||% "no"))
  output$incon_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Inconsistency")   %||% "no"))
  output$indir_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Indirectness")    %||% "no"))
  output$impre_badge  <- shiny::renderUI(pma_judgment_badge(domain_judgment("Imprecision")     %||% "no"))
  output$pubias_badge <- shiny::renderUI(pma_judgment_badge(domain_judgment("Publication bias")%||% "no"))

  output$rob_chip    <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Risk of bias")    %||% "no"))
  output$incon_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Inconsistency")   %||% "no"))
  output$indir_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Indirectness")    %||% "no"))
  output$impre_chip  <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Imprecision")     %||% "no"))
  output$pubias_chip <- shiny::renderUI(pma_downgrade_chip(domain_judgment("Publication bias")%||% "no"))

  # ----- Risk-of-bias analysis set (Core GRADE 4 Fig 2) -------------------
  # Display only: these three outputs read the rated object and change no
  # judgment, no number and no rating. They exist because grade_meta() can
  # refit the analysis on the low risk-of-bias subset and only says so via
  # an R-level message() that never reaches the browser.
  output$analysis_set_indicator <- shiny::renderUI(
    pma_analysis_set_indicator(grade_obj()))
  shiny::outputOptions(output, "analysis_set_indicator",
                       suspendWhenHidden = FALSE)

  output$analysis_set_banner_rob <- shiny::renderUI(
    pma_analysis_set_banner(grade_obj()))
  output$analysis_set_banner_cert <- shiny::renderUI(
    pma_analysis_set_banner(grade_obj()))

  # Live "How is this judged?" body for Risk of Bias. Rendered rather than
  # baked into the UI so the sensitivity-analysis change threshold quoted in
  # the copy is the one the algorithm actually used.
  output$rob_how_body <- shiny::renderUI(
    htmltools::HTML(htmltools::htmlEscape(
      EDU_COPY$domains$rob$how(input$rob_inf_threshold %||% 0.10,
                               .rob_some_concerns_setting()))))
  # It lives inside a collapsed <details>, so without this the copy is only
  # filled in after the user opens the block.
  shiny::outputOptions(output, "rob_how_body", suspendWhenHidden = FALSE)

  # Standing statement of the binary rule currently in force. Tracks the
  # reviewer's choice, so it never contradicts the control that set it.
  output$rob_rule_note <- shiny::renderUI({
    high_side <- identical(.rob_some_concerns_setting(), "high")
    htmltools::div(
      class = "pma-rob-rule",
      htmltools::strong(if (high_side) {
        "Currently: only studies rated low count as low risk of bias. "
      } else {
        "Currently: studies rated low or 'some concerns' count as low risk of bias. "
      }),
      "Core GRADE 4 permits each study to be classified as low or high risk ",
      "of bias overall, but defines that boundary by counting high-risk ",
      "items, uses three different counts in its three worked examples, and ",
      "leaves the choice open. Where 'some concerns' falls is therefore a ",
      "review decision, not a Core GRADE rule; set it under 'Inputs for this ",
      "domain' below. Under the current setting, studies rated ",
      if (high_side) {
        "'some concerns', studies rated high and studies left unrated all count as high risk of bias. "
      } else {
        "'some concerns' and studies left unrated count as low risk of bias, and only studies rated high count as high. "
      },
      "The classification feeds the dominance gate, the low-risk-only ",
      "comparison estimate and any refit on the low-risk set. The ",
      "three-level input is retained because reviewers assess studies with ",
      "RoB 2, whose vocabulary Core GRADE 4 does not use; the 'Risk group' ",
      "column shows which of the two groups each study reaches the analysis ",
      "in."
    )
  })

  output$rob_notes    <- shiny::renderText(domain_notes("Risk of bias"))
  output$incon_notes  <- shiny::renderText(domain_notes("Inconsistency"))
  output$impre_notes  <- shiny::renderText(domain_notes("Imprecision"))
  output$pubias_notes <- shiny::renderText(domain_notes("Publication bias"))

  # ----- Which Core GRADE 2 Fig 4 branch the analysis took ----------------
  # Parsed from the "Fig 4 path: ..." fragment pmatools writes into the
  # domain note, so the headline here can never disagree with the note below
  # it. The point of stating it is that the two branches treat sample size
  # differently: on the CI-crosses-threshold path the OIS is never consulted,
  # and the OIS figures printed further down are informational only.
  output$impre_branch <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "Run the analysis and set a threshold to see which branch applies."))
    }
    notes <- domain_notes("Imprecision")
    path  <- if (grepl("Fig 4 path: ", notes, fixed = TRUE)) {
      p <- sub("^.*Fig 4 path: ", "", notes)
      p <- sub("\\s*\\[Second Fig 4.*$", "", p)
      trimws(strsplit(p, " | ", fixed = TRUE)[[1]][1])
    } else ""
    if (!nzchar(path)) {
      return(htmltools::div(
        style = paste0(
          "padding: 0.6rem 0.85rem; background: #f5f5f5; ",
          "border-left: 4px solid #6b7280; margin: 0.5rem 0; ",
          "font-size: 0.85rem;"),
        htmltools::p(style = "margin: 0;",
          htmltools::strong("Figure 4 was not applied."),
          " The imprecision judgment was supplied manually through the ",
          "override below, which bypasses the automated assessment.")))
    }
    crosses  <- grepl("^CI crosses", path)
    ois_used <- grepl("OIS approach", path, fixed = TRUE)
    head <- if (crosses) {
      "Yes branch - the CI crosses the chosen threshold."
    } else if (ois_used) {
      "No branch, implausibly large effect - the OIS approach was applied."
    } else {
      "No branch, moderate effect - do not rate down."
    }
    detail <- if (crosses) {
      paste0("Sample size is NOT considered on this path: the Optimal ",
             "Information Size is not consulted, and any OIS figures in the ",
             "evaluation below are reported for information only. Rate down ",
             "one level; two only if the CI crosses two thresholds ",
             "(important benefit and important harm), or if the most ",
             "appropriate plain language summary warrants 'may' rather than ",
             "'likely'. The second condition is a reviewer judgment and is ",
             "not assessed automatically - record it through the override ",
             "below.")
    } else if (ois_used) {
      paste0("This is the only route to the OIS: the CI stays clear of the ",
             "threshold AND the effect is implausibly large. The participant ",
             "count therefore did drive the judgment. Figure 4 compares the ",
             "OIS against participants, not events.")
    } else {
      paste0("Figure 4 stops here. A moderate effect whose CI stays clear of ",
             "the threshold does not rate down, and sample size never enters ",
             "the decision: the OIS is reached only when the effect is ",
             "implausibly large.")
    }
    color <- if (crosses) "#c07020" else if (ois_used) "#0f172a" else "#208050"
    htmltools::div(
      style = sprintf(paste0(
        "padding: 0.6rem 0.85rem; background: #f5f5f5; ",
        "border-left: 4px solid %s; margin: 0.5rem 0; font-size: 0.85rem;"),
        color),
      htmltools::p(style = "margin: 0;", htmltools::strong(head)),
      htmltools::p(style = "margin: 0.25rem 0 0;", detail),
      htmltools::p(
        style = paste0("margin: 0.35rem 0 0; font-family: monospace; ",
                       "font-size: 0.78rem; color: #444;"),
        path)
    )
  })
  shiny::outputOptions(output, "impre_branch", suspendWhenHidden = FALSE)

  # ----- Indirectness subdomain table (pmatools indirectness_table) -------
  # Surfaced deliberately: it is the only rendering of exactly what the app
  # sent to grade_meta(), it shows which element drove the worst-case fold,
  # and its footer repeats the Core GRADE 5 Table 2 gradient caveat next to
  # the judgment rather than only inside the collapsed explanation.
  output$indir_subdomain_table <- shiny::renderUI({
    if (is.null(indir_subdomains())) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        paste0("Answer at least one subdomain question above to record a ",
               "subdomain table. Until then the domain rests on the overall ",
               "rating alone.")))
    }
    g <- grade_obj()
    if (is.null(g) || is.null(g$indirectness_subdomains)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "(Subdomain table not yet available - set a threshold first.)"))
    }
    ft <- tryCatch(indirectness_table(g), error = function(e) NULL)
    if (is.null(ft)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "(Subdomain table could not be rendered.)"))
    }
    htmltools::div(
      style = "margin-top: 0.75rem;",
      tryCatch(flextable::htmltools_value(ft),
               error = function(e) htmltools::p(paste("Render error:",
                                                      conditionMessage(e))))
    )
  })

  # Helper to collect display-panel inputs for a given prefix.
  .display_args <- function(prefix) {
    pick_text <- function(id) {
      v <- input[[id]]
      if (is.null(v) || !nzchar(v)) NULL else v
    }
    lo <- input[[paste0(prefix, "_xlim_lo")]]
    hi <- input[[paste0(prefix, "_xlim_hi")]]
    xlim <- if (!is.null(lo) && !is.null(hi) && !is.na(lo) && !is.na(hi) && lo < hi) {
      c(lo, hi)
    } else NULL
    addrow_ids <- pma_forest_addrow_ids(prefix)
    list(
      title        = pick_text(paste0(prefix, "_title")),
      label_e      = pick_text(paste0(prefix, "_label_e")),
      label_c      = pick_text(paste0(prefix, "_label_c")),
      favors_left  = pick_text(paste0(prefix, "_favors_left")),
      favors_right = pick_text(paste0(prefix, "_favors_right")),
      xlim         = xlim,
      # An absent checkbox means the tab has not been rendered yet; fall back
      # to the UI default (TRUE) rather than to isTRUE(NULL). One checkbox
      # feeds both keys - plot_forest()'s two arguments are kept as they are.
      show_n       = isTRUE(input[[paste0(prefix, "_show_arm_columns")]] %||% TRUE),
      show_events  = isTRUE(input[[paste0(prefix, "_show_arm_columns")]] %||% TRUE),
      addrow_above = pma_addrow_above(input[[addrow_ids[["above"]]]]),
      addrow_below = pma_addrow_below(input[[addrow_ids[["below"]]]])
    )
  }

  # Mirror per-plot display options into state for downstream (export) use
  shiny::observe({
    state$display$forest_rob    <- .display_args("rob")
    state$display$forest_incon  <- .display_args("incon")
    state$display$forest_indir  <- .display_args("indir")
    state$display$forest_pubias <- .display_args("pubias")
    state$display$funnel_pub    <- pma_funnel_display_args(input, "funnel_pub")
    state$display$funnel_trim   <- pma_funnel_display_args(input, "funnel_trim",
                                                           include_egger = FALSE)
  })

  # ----- RoB stratified forest plot (render generous, trim, display) -----
  output$rob_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    rob_vec <- if (!is.null(state$data) && "rob" %in% names(state$data)) {
      .study_covariate(as.character(obj$studlab), "rob", default = "*")
    } else .study_covariate(as.character(obj$studlab), "rob", default = "*")
    da <- .display_args("rob")
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (obj$k %||% 0L) + 600,
      plot_fn = function() {
        do.call(plot_forest_rob,
                c(list(meta_obj = obj, rob = rob_vec), da))
      }
    )
  }, deleteFile = TRUE)

  .has_indir_forest_input <- function() {
    vals <- character()
    rt <- state$rob_table
    if (!is.null(rt) && "indirectness" %in% names(rt)) {
      vals <- c(vals, as.character(rt$indirectness))
    }
    d <- state$data
    if (!is.null(d) && "indirectness" %in% names(d)) {
      vals <- c(vals, as.character(d$indirectness))
    }
    vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
    length(vals) > 0
  }

  output$indir_forest_image_block <- shiny::renderUI({
    if (!.has_indir_forest_input()) return(NULL)
    htmltools::tagList(
      htmltools::h5("Forest plot stratified by Indirectness",
                    style = "margin-top: 1rem;"),
      htmltools::div(class = "pma-forest-image",
        shinycssloaders::withSpinner(
          shiny::imageOutput("indir_forest", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px"))
    )
  })

  # ----- Indirectness stratified forest plot (mirror of RoB) -----
  output$indir_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj) || !.has_indir_forest_input()) return(NULL)
    indir_vec <- if (!is.null(state$data) && "indirectness" %in% names(state$data)) {
      .study_covariate(as.character(obj$studlab), "indirectness", default = "low")
    } else .study_covariate(as.character(obj$studlab), "indirectness", default = "low")
    da <- .display_args("indir")
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (obj$k %||% 0L) + 600,
      plot_fn = function() {
        do.call(plot_forest_indirectness,
                c(list(meta_obj = obj, indirectness = indir_vec), da))
      }
    )
  }, deleteFile = TRUE)

  # ----- Inconsistency forest plot (render generous, trim, display) -----
  output$incon_forest <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    da <- .display_args("incon")
    pma_render_trimmed(
      width  = 1400,
      height = 200 + 80 * (obj$k %||% 0L) + 400,
      plot_fn = function() {
        do.call(plot_forest,
                c(list(meta_obj = obj, auto_layout = TRUE), da))
      }
    )
  }, deleteFile = TRUE)

  # ----- Publication bias: Q2 + Q3 (or Q4) flowchart-ordered block -----
  # Numbered to match Core GRADE 4 Fig 5's four nodes and the headings in
  # step3_ui(): Q1 small-and-industry-sponsored (static, above), Q2
  # statistical feasibility, Q3 asymmetry (k >= 10), Q4 documented
  # unpublished studies (k < 10).
  output$pubias_main_block <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) {
      return(htmltools::p("Run analysis first."))
    }
    k <- .effective_pubias_k(obj)

    if (k >= 10) {
      htmltools::tagList(
        # Q2
        htmltools::h5(sprintf(
          "Q2. Statistical analysis feasible - k = %d >= 10", k)),
        htmltools::p(class = "pma-card-subtitle",
          sprintf(paste0("k counts the studies contributing a usable ",
                         "estimate (finite effect and positive standard ",
                         "error); studies with missing results are excluded ",
                         "from it. The same k = %d gates every block on this ",
                         "tab. Egger's linear regression test is run ",
                         "automatically and shown below the funnel plot."),
                  k)),

        # Q3 funnel + Egger auto + visual override
        htmltools::h5("Q3. Does funnel plot asymmetry strongly suggest publication bias?",
                      style = "margin-top: 1rem;"),
        htmltools::p(class = "pma-card-subtitle",
          "Egger's p < 0.05 -> rate down 1 (some concerns); ",
          "p >= 0.05 -> do not rate down. There is no second tier: Core ",
          "GRADE 4 never rates down two levels for publication bias."),
        htmltools::p(class = "pma-card-subtitle",
          htmltools::strong("Provenance: "),
          paste0("the p < 0.05 cut-off is a pmatools operational convention, ",
                 "not a Core GRADE criterion. Figure 5 asks this node ",
                 "qualitatively - whether asymmetry 'strongly suggests ",
                 "publication bias' - and names no threshold. Override it ",
                 "with a visual judgment whenever the plot warrants one.")),
        shinycssloaders::withSpinner(
          shiny::imageOutput("pubias_funnel", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px"),
        pma_funnel_display_panel("funnel_pub"),
        shiny::uiOutput("pubias_egger_result"),
        shiny::selectInput("pubias_funnel_asymmetry",
          "Visual override of Egger",
          choices = c("(use Egger)" = "",
                      "Funnel symmetric"  = "no",
                      "Funnel asymmetric" = "yes")),
        shiny::conditionalPanel(
          "(input.pubias_funnel_asymmetry || '') != ''",
          shiny::textAreaInput(
            "pubias_fa_rationale",
            "Rationale (required for the visual override)",
            rows = 2, width = "100%",
            placeholder = paste0(
              "State why your visual judgment replaces the automated ",
              "Egger's test."))
        ),

        # Reference materials: trim-and-fill funnel + summary text
        htmltools::hr(),
        htmltools::h5("Reference: trim-and-fill"),
        htmltools::p(class = "pma-card-subtitle",
          "Filled (imputed) studies appear alongside observed studies on the ",
          "funnel plot. The numerical summary below shows how the pooled ",
          "estimate would shift if these imputed studies actually existed. ",
          htmltools::HTML(paste0(
            "This information is <strong>not part of the automated Core GRADE ",
            "algorithm</strong>, but you are encouraged to consider it ",
            "when finalising the publication-bias judgment manually."))),
        shinycssloaders::withSpinner(
          shiny::imageOutput("pubias_trimfill_funnel", height = "auto"),
          type = 4, color = "#0f172a", size = 0.6,
          proxy.height = "320px"),
        pma_funnel_display_panel("funnel_trim", include_egger = FALSE),
        shiny::uiOutput("pubias_trimfill_summary")
      )
    } else {
      htmltools::tagList(
        htmltools::h5(sprintf(
          "Q2. Statistical analysis NOT feasible - k = %d < 10", k)),
        htmltools::p(class = "pma-card-subtitle",
          sprintf(paste0("k counts the studies contributing a usable ",
                         "estimate (finite effect and positive standard ",
                         "error); studies with missing results are excluded ",
                         "from it. The same k = %d gates every block on this ",
                         "tab. Egger's test would be unreliable below 10 ",
                         "studies, so Figure 5 routes to the registry ",
                         "question instead."), k)),

        htmltools::h5("Q4. Documentation of unpublished studies",
                      style = "margin-top: 1rem;"),
        htmltools::p(class = "pma-card-subtitle",
          "If unpublished trials are documented in a registry ",
          "(eg, ClinicalTrials.gov, FDA), rate down 1."),
        shiny::radioButtons("pubias_unpublished",
          "Q4. Unpublished studies documented?",
          choices = c("(use default: no)" = "", "No" = "no", "Yes" = "yes"),
          inline = TRUE)
      )
    }
  })
  shiny::outputOptions(output, "pubias_main_block", suspendWhenHidden = FALSE)

  # Contour-enhanced funnel plot (Q4 visual)
  output$pubias_funnel <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj)) {
      return(list(src = "", contentType = "image/png",
                  alt = "Run analysis first.", width = "100%"))
    }
    da <- pma_funnel_display_args(input, "funnel_pub")
    show_egger <- if (is.na(da$show_egger)) TRUE else da$show_egger
    pma_render_trimmed(
      width  = da$width,
      height = da$height,
      plot_fn = function() {
        if (!is.null(da$xlim))
          plot_funnel(obj, show_egger = show_egger, xlim = da$xlim)
        else
          plot_funnel(obj, show_egger = show_egger)
      }
    )
  }, deleteFile = TRUE)

  # Egger's auto judgment displayed as colour-coded callout
  output$pubias_egger_result <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj) || .effective_pubias_k(obj) < 10) return(NULL)
    res <- tryCatch(
      suppressWarnings(meta::metabias(obj, method.bias = "linreg")),
      error = function(e) NULL
    )
    if (is.null(res) || is.null(res$p.value) || is.na(res$p.value)) {
      return(htmltools::p(
        class = "pma-card-subtitle", style = "font-style: italic;",
        "Egger's test could not be computed."))
    }
    pval <- res$p.value
    # Single tier. pmatools 0.5 removed the p < 0.01 -> "serious" (-2) rule
    # because Core GRADE 4 never rates down two levels for publication bias.
    judgment <- if (pval < 0.05) {
      list(text = sprintf(paste0("p = %.3f < 0.05 - evidence of asymmetry. ",
                                 "Auto judgment: some concerns (rate down 1)."),
                          pval),
           color = "#c07020")
    } else {
      list(text = sprintf(paste0("p = %.3f >= 0.05 - no strong evidence of ",
                                 "asymmetry. Auto judgment: no rate down."),
                          pval),
           color = "#208050")
    }
    htmltools::div(
      style = sprintf(
        "padding: 0.6rem 0.85rem; background: #f5f5f5; border-left: 4px solid %s; margin: 0.5rem 0;",
        judgment$color),
      htmltools::p(style = "margin: 0;",
        htmltools::strong("Egger's regression: "),
        judgment$text),
      htmltools::p(style = paste0("margin: 0.35rem 0 0; font-size: 0.8rem; ",
                                  "font-style: italic; ",
                                  "color: hsl(var(--muted-foreground));"),
        paste0("The p < 0.05 cut-off is a pmatools convention. Core GRADE 4 ",
               "Figure 5 asks only whether asymmetry strongly suggests ",
               "publication bias and names no p-value; the judgment can be ",
               "no more than one level down either way."))
    )
  })

  # Trim-and-fill funnel plot (reference only, NOT forest)
  # Imputed (filled) studies are drawn as solid red filled circles so they
  # stand out from observed studies (default dark-gray fill, black border).
  # We pass per-point vectors of pch / col / bg directly to meta::funnel(),
  # which forwards them to its single internal points() call (line 400 of
  # meta::funnel.meta). This is more reliable than drawing an overlay on
  # top, since the y-axis transformation in meta::funnel is not necessarily
  # raw seTE.
  output$pubias_trimfill_funnel <- shiny::renderImage({
    obj <- state$ma
    if (is.null(obj) || .effective_pubias_k(obj) < 10) {
      return(list(src = "", contentType = "image/png",
                  alt = "Trim-and-fill requires k >= 10.", width = "100%"))
    }
    tf <- tryCatch(suppressWarnings(meta::trimfill(obj)),
                   error = function(e) NULL)
    da <- pma_funnel_display_args(input, "funnel_trim", include_egger = FALSE)

    pma_render_trimmed(
      width  = da$width,
      height = da$height,
      plot_fn = function() {
        if (is.null(tf)) {
          graphics::plot.new()
          graphics::title(main = "Trim-and-fill could not be computed")
          return(invisible(NULL))
        }

        par_old <- graphics::par(mar = c(4, 4, 1, 2))
        on.exit(graphics::par(par_old), add = TRUE)

        n_total <- length(tf$TE)
        is_imp  <- if (!is.null(tf$trimfill)) {
          as.logical(tf$trimfill)
        } else {
          k0 <- if (!is.null(tf$k0)) as.integer(tf$k0) else
                (n_total - (obj$k %||% 0L))
          c(rep(FALSE, n_total - k0), rep(TRUE, k0))
        }

        # Per-point styling. pch = 21 is a filled circle that respects both
        # `col` (border) and `bg` (fill).
        pch_vec <- rep(21L, n_total)
        col_vec <- ifelse(is_imp, "red", "black")
        bg_vec  <- ifelse(is_imp, "red", "darkgray")
        cex_vec <- ifelse(is_imp, 1.6, 1.0)

        funnel_args <- list(tf,
                            contour = c(0.9, 0.95, 0.99),
                            pch = pch_vec,
                            col = col_vec,
                            bg  = bg_vec,
                            cex = cex_vec)
        if (!is.null(da$xlim)) funnel_args$xlim <- da$xlim
        do.call(meta::funnel, funnel_args)

        graphics::legend(
          "topright",
          legend = c("Observed studies", "Imputed (filled) studies"),
          pch    = c(21, 21),
          col    = c("black", "red"),
          pt.bg  = c("darkgray", "red"),
          pt.cex = c(1.0, 1.4),
          bty    = "o", bg = "#ffffff", cex = 0.8
        )
      }
    )
  }, deleteFile = TRUE)

  # Trim-and-fill numerical summary
  output$pubias_trimfill_summary <- shiny::renderUI({
    obj <- state$ma
    # Same k as Q2, the funnel block and the missing-results forest. This
    # used to gate on the raw obj$k, which counts studies with missing
    # results too, so a dataset with missing-results studies could show the
    # trim-and-fill summary while Q2 said statistical analysis was not
    # feasible (and vice versa).
    if (is.null(obj) || .effective_pubias_k(obj) < 10) return(NULL)
    tf <- tryCatch(suppressWarnings(meta::trimfill(obj)),
                   error = function(e) NULL)
    if (is.null(tf)) return(NULL)

    k_imputed <- length(tf$TE) - length(obj$TE)
    te_orig <- obj$TE.random
    te_adj  <- tf$TE.random
    is_log  <- !is.null(obj$sm) && obj$sm %in% c("OR", "RR", "HR", "RoM", "IRR")

    fmt <- function(x) {
      if (!is.finite(x)) return("NA")
      if (is_log) sprintf("%.3f (log %s = %.3f)", exp(x), obj$sm, x)
      else sprintf("%.3f", x)
    }

    sign_flips <- is.finite(te_orig) && is.finite(te_adj) &&
                  (sign(te_orig) != sign(te_adj)) &&
                  (abs(te_orig) > 1e-6) && (abs(te_adj) > 1e-6)

    htmltools::div(
      style = paste0(
        "padding: 0.6rem 0.85rem; background: #f9f9f9; ",
        "border: 1px solid #ddd; margin: 0.5rem 0; ",
        "font-family: monospace; font-size: 0.85rem;"
      ),
      htmltools::p(style = "margin: 0 0 0.25rem;",
        htmltools::strong("Trim-and-fill summary (reference only)")),
      htmltools::p(style = "margin: 0;",
        sprintf("Imputed studies: %d", k_imputed)),
      htmltools::p(style = "margin: 0;",
        sprintf("Original pooled TE.random  = %s", fmt(te_orig))),
      htmltools::p(style = "margin: 0;",
        sprintf("Adjusted pooled TE.random = %s%s", fmt(te_adj),
                if (sign_flips) "  [direction flips]" else ""))
    )
  })

  # ----- Reference: Subgroup analysis (Available vs Missing results) -----
  # Schema: studlab (chr), n (int), results_known (chr), source (chr).
  # source = "auto" for dataset-derived rows (NA TE in meta_obj); "user"
  # for rows added via "+ Add missing trial".
  .pubias_missing_empty <- function() {
    data.frame(studlab = character(0), n = integer(0),
               results_known = character(0),
               source = character(0),
               stringsAsFactors = FALSE)
  }

  # Auto-seed: when state$ma changes, refresh the auto rows from NA-TE
  # studies. Preserve any user edits to existing auto rows (matched by
  # studlab) and keep all "+ Add" rows untouched.
  shiny::observe({
    obj <- state$ma
    if (is.null(obj)) return()
    k_te <- length(obj$TE)
    if (k_te == 0L) return()

    studlab_obj <- as.character(obj$studlab)
    if (length(studlab_obj) > k_te) studlab_obj <- studlab_obj[seq_len(k_te)]
    n_obj <- if (!is.null(obj$n.e) && !is.null(obj$n.c) &&
                 length(obj$n.e) >= k_te && length(obj$n.c) >= k_te) {
      obj$n.e[seq_len(k_te)] + obj$n.c[seq_len(k_te)]
    } else {
      rep(NA_integer_, k_te)
    }
    auto_idx <- which(!(is.finite(obj$TE) & is.finite(obj$seTE)))

    auto_df <- if (length(auto_idx)) {
      data.frame(
        studlab = studlab_obj[auto_idx],
        n = suppressWarnings(as.integer(n_obj[auto_idx])),
        results_known = "Reported but data not extractable",
        source = "auto",
        stringsAsFactors = FALSE
      )
    } else .pubias_missing_empty()

    cur <- state$pubias_missing
    new_state <- if (is.null(cur) || nrow(cur) == 0L) {
      auto_df
    } else {
      src_col <- if ("source" %in% names(cur)) cur$source else rep("user", nrow(cur))
      user_rows <- cur[src_col == "user", , drop = FALSE]
      prev_auto <- cur[src_col == "auto", , drop = FALSE]
      if (nrow(auto_df) && nrow(prev_auto)) {
        m <- match(auto_df$studlab, prev_auto$studlab)
        have <- !is.na(m)
        auto_df$results_known[have] <- prev_auto$results_known[m[have]]
        auto_df$n[have]              <- prev_auto$n[m[have]]
      }
      rbind(auto_df, user_rows)
    }
    if (!identical(new_state, cur)) state$pubias_missing <- new_state
  })

  shiny::observeEvent(input$pubias_missing_add, {
    cur <- state$pubias_missing %||% .pubias_missing_empty()
    cur <- rbind(cur, data.frame(
      studlab = "(new trial)",
      n = NA_integer_,
      results_known = "Measured but not reported (suspect P > 0.05)",
      source = "user",
      stringsAsFactors = FALSE))
    state$pubias_missing <- cur
  })

  output$pubias_missing_editor <- DT::renderDT({
    d <- state$pubias_missing %||% .pubias_missing_empty()
    display <- d[, c("studlab", "n", "results_known"), drop = FALSE]
    DT::datatable(
      display,
      editable = list(target = "cell", disable = list(columns = 0)),
      options  = list(dom = "tp", pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  shiny::outputOptions(output, "pubias_missing_editor", suspendWhenHidden = FALSE)

  # Cell edits accept free text. studlab is read-only (auto rows must
  # match meta_obj; user-added rows can change studlab via a future
  # iteration if needed). n and results_known are freely editable.
  shiny::observeEvent(input$pubias_missing_editor_cell_edit, {
    info <- input$pubias_missing_editor_cell_edit
    if (is.null(info)) return()
    d <- state$pubias_missing %||% .pubias_missing_empty()
    if (nrow(d) == 0) return()
    col_name <- c("studlab", "n", "results_known")[info$col + 1]
    new_val <- info$value
    if (col_name == "n") {
      d$n[info$row] <- suppressWarnings(as.integer(new_val))
    } else {
      d[[col_name]][info$row] <- as.character(new_val)
    }
    state$pubias_missing <- d
  })

  output$pubias_missing_forest <- shiny::renderImage({
    obj <- state$ma
    # Effective k, not obj$k: see output$pubias_trimfill_summary.
    if (is.null(obj) || .effective_pubias_k(obj) < 10) {
      return(list(src = "", contentType = "image/png",
                  alt = "Missing-results forest requires k >= 10.",
                  width = "100%"))
    }
    m_df <- state$pubias_missing %||% .pubias_missing_empty()
    da <- .display_args("pubias")
    # Adaptive canvas: 1 row per available study + 1 row per missing study
    # plus margin for two subgroup labels and the overall pooled diamond.
    k_avail <- length(obj$TE)
    k_miss  <- nrow(m_df)
    pma_render_trimmed(
      width  = 1400,
      height = 400 + 80 * (k_avail + k_miss) + 600,
      plot_fn = function() {
        do.call(plot_forest_pubias_subgroup,
                c(list(meta_obj = obj, missing_df = m_df,
                       auto_detect = FALSE), da))
      }
    )
  }, deleteFile = TRUE)

  shiny::observeEvent(input$indirectness, {
    state$indir_reviewed <- TRUE
  })
  # The four subdomain answers are the primary input of the domain, so any of
  # them clears the "no judgment recorded yet" banner too.
  for (.indir_id in unname(STEP3_INDIR_SUBDOMAINS)) {
    local({
      id <- .indir_id
      shiny::observeEvent(input[[id]], {
        state$indir_reviewed <- TRUE
      })
    })
  }

  output$indirectness_banner <- shiny::renderUI({
    if (isTRUE(state$indir_reviewed)) return(NULL)
    pma_banner(EDU_COPY$domains$indirectness$banner)
  })

  output$final_certainty <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) {
      # Distinguish "no analysis yet" from "no threshold": the latter is a
      # state the reviewer can create at any time by clearing the field, and
      # it must say why nothing is being rated rather than fail silently.
      if (!is.null(state$ma) && threshold_missing()) {
        return(htmltools::div(
          style = paste0(
            "padding: 0.75rem 1rem; background: ", PMA_ALERT_BG,
            "; border-left: 4px solid ", PMA_ALERT_FG,
            "; border-radius: 4px; font-size: 0.9rem;"),
          htmltools::strong("No certainty rating. "),
          "The decision threshold is empty. Risk of Bias, Inconsistency and ",
          "Imprecision are all judged against it, so no rating is computed ",
          "until it is set on the Configuration tab."))
      }
      return(htmltools::p("Run analysis and configure domains."))
    }
    other_dg <- suppressWarnings(as.integer(input$other_downgrade %||% "0"))
    if (is.na(other_dg)) other_dg <- 0L
    ft <- tryCatch(
      evidence_profile(g,
                       other_text      = input$other_text,
                       other_downgrade = other_dg),
      error = function(e) NULL
    )
    if (is.null(ft)) {
      return(htmltools::p("(Evidence Profile not yet available)"))
    }
    htmltools::tags$div(
      style = "margin-top: 1rem;",
      tryCatch(flextable::htmltools_value(ft),
               error = function(e) htmltools::p(paste("Render error:",
                                                      conditionMessage(e))))
    )
  })

  # Whether the SoF can safely be rendered through the responder conversion.
  # sof_table() HARD-ABORTS when convert_smd_to_or = TRUE and baseline_risk
  # is absent or outside (0, 1), and when meta_obj$sm is not SMD / MD
  # (R/_pmatools/sof_table.R). All three preconditions are checked here so
  # Step 3 never renders into that abort.
  sof_convert_args <- shiny::reactive({
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    sm <- g$meta$sm %||% ""
    if (!isTRUE(input$convert_smd_to_or)) return(NULL)
    if (!sm %in% c("SMD", "MD")) return(NULL)
    p0 <- responder_p0()
    if (!responder_p0_valid()) return(NULL)
    list(
      convert_smd_to_or = TRUE,
      baseline_risk     = p0,
      threshold_label   = input$threshold_label,
      chinn_invert      = chinn_invert_derived()
    )
  })

  # Presentation fields collected in Step 2 (see step2_ui()), resolved here
  # into the exact values sof_table() wants.
  sof_follow_up <- shiny::reactive(pma_sof_follow_up(state$outcome_follow_up))
  sof_unit <- shiny::reactive({
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    pma_sof_unit(g, state$outcome_unit)
  })

  # Core GRADE 6's rare-event trap: the Difference and "With intervention"
  # columns are both derived by applying the pooled relative effect to a
  # baseline risk, which misleads when events are rare. Computed from the
  # analysis itself, against the risk the table is actually drawn on (the
  # responder proportion when the Chinn conversion is active).
  sof_rare_alert <- shiny::reactive({
    g <- grade_obj()
    if (is.null(g)) return(NULL)
    pma_rare_event_alert(g, baseline_risk = sof_convert_args()$baseline_risk)
  })

  output$sof_preview <- shiny::renderUI({
    g <- grade_obj()
    if (is.null(g)) return(htmltools::p("..."))
    args <- c(
      list(x          = g,
           # Core GRADE 6 layout for every SoF the app renders or exports;
           # see PMA_SOF_STYLE in ui_helpers.R for why it is not an option.
           style      = PMA_SOF_STYLE,
           palette    = PMA_SOF_PALETTE,
           per        = input$per %||% 1000,
           prediction = isTRUE(input$prediction),
           follow_up  = sof_follow_up(),
           unit       = sof_unit()),
      sof_convert_args() %||% list()
    )
    ft <- tryCatch(do.call(sof_table, args),
                   error = function(e) NULL)
    if (is.null(ft)) return(htmltools::p("(SoF not yet available)"))
    alert <- sof_rare_alert()
    # Both notes go into the flextable footer as well as onto the page, so
    # they travel into the exported .docx.
    ft <- pma_sof_add_notes(ft, c(alert$note, PMA_SOF_LIMITATIONS_NOTE))
    htmltools::tagList(
      pma_rare_event_banner(alert),
      pma_sof_scroller(
        tryCatch(flextable::htmltools_value(ft),
                 error = function(e)
                   htmltools::p(paste("SoF render error:",
                                      conditionMessage(e))))),
      pma_sof_limitations_ui()
    )
  })

  # The responder-conversion settings are owned by Step 3 (Configuration
  # tab), not by app.R's display observer. Step 4's export_bundle() gets the
  # same guarded values the Step 3 preview uses, so it cannot walk into
  # sof_table()'s abort - which is reachable otherwise, because an
  # input$convert_smd_to_or left TRUE from an earlier SMD run survives a
  # switch to RoM (hiding a checkbox does not reset it).
  shiny::observe({
    ca <- sof_convert_args()
    state$display$convert       <- !is.null(ca)
    state$display$baseline_risk <- ca$baseline_risk
    state$display$chinn_invert  <- isTRUE(ca$chinn_invert)
    # Resolved presentation fields, so Step 4 builds the exported SoF from
    # exactly the values this preview used.
    state$display$follow_up     <- sof_follow_up()
    state$display$unit          <- sof_unit()
  })

  # Read-only echo of the outcome name: it is owned by Step 2, so Step 3 shows
  # it (the SoF row label depends on it) without offering a second, divergent
  # field to edit.
  output$outcome_name_echo <- shiny::renderUI({
    nm <- state$outcome_name %||% "(not set)"
    # Raw value, not sof_follow_up(): that one already carries the
    # "Follow-up: " prefix the table cell needs, and the label supplies it
    # here.
    fu <- state$outcome_follow_up
    if (!is.null(fu) && !nzchar(trimws(fu))) fu <- NULL
    un <- sof_unit()
    htmltools::tagList(
      htmltools::p(class = "pma-card-subtitle",
        "Outcome name: ", htmltools::tags$strong(nm),
        " - set in Step 2 (Model configuration)."),
      htmltools::p(class = "pma-card-subtitle",
        "Follow-up: ",
        htmltools::tags$strong(fu %||% "(not set)"),
        " - shown under the outcome name in the Summary of Findings table ",
        "and saved with this outcome. Set in Step 2."),
      if (!is.null(un)) htmltools::p(class = "pma-card-subtitle",
        "Unit of the Difference column: ", htmltools::tags$strong(un),
        ".") else NULL
    )
  })

  # Read-only echo of the outcome direction, in the same style. small_values
  # is set in Step 2 and was invisible in Step 3 even though it flips the
  # direction gate in Risk of Bias (and, on the continuous path, the sign of
  # the responder odds ratio). Shown here; editing stays in Step 2.
  output$direction_echo <- shiny::renderUI({
    sv <- state$small_values
    label <- if (identical(sv, "desirable")) {
      "Favorable - a smaller value of this outcome is better"
    } else if (identical(sv, "undesirable")) {
      "Unfavorable - a smaller value of this outcome is worse"
    } else {
      "(not set)"
    }
    htmltools::div(
      style = "margin: 0 0 1rem;",
      htmltools::p(class = "pma-card-subtitle",
        "Outcome direction: ", htmltools::tags$strong(label),
        " - set in Step 2 (Model configuration). It sets the bias direction ",
        "used by the Risk-of-Bias check.")
    )
  })
  shiny::outputOptions(output, "direction_echo", suspendWhenHidden = FALSE)

  # How the derived direction lands in the responder conversion.
  output$chinn_direction_echo <- shiny::renderUI({
    inv <- chinn_invert_derived()
    htmltools::p(class = "pma-card-subtitle",
      "Direction of the responder odds ratio: ",
      htmltools::tags$strong(
        if (inv) "sign flipped" else "as analysed"),
      if (inv) {
        paste0(" - a smaller value of this outcome is better, so the sign of ",
               "the standardized mean difference is flipped to put the ",
               "intervention above 1. Derived from the Step 2 direction ",
               "answer; not asked again here.")
      } else {
        paste0(" - a larger value of this outcome is better, so the ",
               "standardized mean difference is used as analysed. Derived ",
               "from the Step 2 direction answer; not asked again here.")
      })
  })
  shiny::outputOptions(output, "chinn_direction_echo",
                       suspendWhenHidden = FALSE)

  # Pointer left where the responder-conversion controls used to be.
  output$display_options_config_note <- shiny::renderUI({
    obj <- state$ma
    if (is.null(obj)) return(NULL)
    if (step3_is_binary_outcome(obj, input$outcome_type)) return(NULL)
    htmltools::p(class = "pma-card-subtitle",
      "How this outcome is presented - as a proportion of responders or as ",
      "the summary measure itself - is set on the Configuration tab, ",
      "together with the control-group responder proportion and the ",
      "definition of the threshold of clinical interest.")
  })

  # Smart defaults for the four Step 3 forest display panels. Only state$ is
  # read: while the user is on Step 3, the Step 2 body (and therefore
  # input$experimental_label / input$control_label) does not exist.
  #
  # The title suffix mirrors what export_bundle.R writes for each stratified
  # plot, so the on-screen title and the exported one agree.
  .forest_label_defaults <- shiny::reactive({
    iv  <- state$arm_e %||% ""
    ct  <- state$arm_c %||% ""
    fav <- pma_favors_labels(state$small_values, iv, ct)
    list(title = state$outcome_name %||% "", label_e = iv, label_c = ct,
         favors_left = fav$left, favors_right = fav$right)
  })
  .forest_title_suffix <- c(
    rob    = " (stratified by Risk of Bias)",
    incon  = "",
    indir  = " (stratified by Indirectness)",
    pubias = " (available vs missing results)"
  )
  for (.pfx in names(.forest_title_suffix)) {
    # local() binds the prefix per iteration; without it all four panels would
    # be wired to the last prefix.
    local({
      p <- .pfx
      pma_autofill_forest_panel(input, session, prefix = p,
                                values_fn = .forest_label_defaults,
                                title_suffix = .forest_title_suffix[[p]])
    })
  }

  # ----- Saving the current outcome into state$outcomes -------------------
  # Key for the saved outcome: the Outcome name entered in Step 2. The label
  # is what grade_table() prints in the Outcome column, so keeping the two
  # identical avoids a second, divergent name field.
  .save_key <- shiny::reactive({
    nm <- trimws(state$outcome_name %||% "")
    if (nzchar(nm)) nm else "Outcome"
  })

  .save_blocked_reasons <- shiny::reactive({
    reasons <- character()
    if (is.null(state$ma)) {
      reasons <- c(reasons, "run the meta-analysis in Step 2")
    }
    if (is.null(grade_obj())) {
      reasons <- c(reasons, "produce a certainty rating")
    }
    unconf <- pma_unconfirmed_domains(domain_confirmed())
    if (length(unconf)) {
      reasons <- c(reasons, paste0("review and confirm: ",
                                   paste(unconf, collapse = ", ")))
    }
    reasons
  })

  output$save_outcome_panel <- shiny::renderUI({
    reasons <- .save_blocked_reasons()
    if (length(reasons)) {
      # Same locked-note treatment as the Step 4 download gate: an
      # unconfirmed assessment must not be banked into the SoF table.
      return(htmltools::div(
        class = "pma-card-subtitle",
        style = paste(
          "border: 1px dashed hsl(var(--border)); border-radius: 6px;",
          "padding: 0.75rem; margin-top: 0.5rem;"),
        htmltools::p(style = "margin: 0;",
          htmltools::strong("Saving locked - certainty assessment incomplete.")),
        htmltools::p(style = "margin: 0.25rem 0 0;",
          paste0("To save this outcome, ", paste(reasons, collapse = "; "), "."))
      ))
    }
    key <- .save_key()
    htmltools::div(
      style = "margin-top: 0.5rem;",
      shiny::actionButton(
        "save_outcome",
        sprintf("Save this outcome's assessment as \"%s\"", key),
        class = "btn btn-primary", style = "width: 100%;"),
      # Sits beside Save so the reviewer can carry straight on to the next
      # outcome. It returns to Step 2 and clears everything that belongs to
      # this outcome (app.R's begin_new_outcome()).
      pma_add_next_outcome_button(style = "width: 100%;"),
      htmltools::p(
        class = "pma-card-subtitle",
        style = "margin-top: 0.4rem;",
        "Saved under the Outcome name set in Step 2 - change it there to ",
        "relabel the Summary of Findings row. \"+ Add next outcome\" returns ",
        "to Step 2 with this outcome's name, direction, follow-up and every ",
        "certainty answer cleared; the saved outcomes, the loaded data and ",
        "the per-study risk-of-bias and indirectness ratings are kept.")
    )
  })
  shiny::outputOptions(output, "save_outcome_panel", suspendWhenHidden = FALSE)

  # Signature of the dataset currently loaded in Step 1. Used both to stamp
  # newly saved outcomes and to flag already-saved ones that came from a
  # different dataset (see pma_dataset_signature()).
  .current_signature <- shiny::reactive(pma_dataset_signature(state$data))

  .store_outcome <- function(key, g) {
    outs <- pma_outcomes_list(state$outcomes)
    # Follow-up and unit are per-outcome, so they are banked ON the saved
    # object rather than read from the live Step 2 fields at render time -
    # otherwise the combined Step 4 table would print the current outcome's
    # follow-up against every earlier row. grade_table(style = "bmj") picks
    # these up per row via .display_arg_from_outcomes() (grade_table.R), which
    # is why Step 4 passes no follow_up / unit argument of its own.
    g$follow_up <- sof_follow_up()
    g$unit      <- pma_sof_unit(g, state$outcome_unit)
    attr(g, "pma_saved_at") <- Sys.time()
    # Provenance stamp: which dataset this rating was made on.
    attr(g, PMA_DATASET_SIGNATURE_ATTR) <- pma_dataset_signature(state$data)
    outs[[key]] <- g
    state$outcomes <- outs
    shiny::showNotification(
      sprintf("Saved \"%s\" (%s certainty). %d outcome(s) ready for the combined Summary of Findings table.",
              key, g$certainty %||% "-", length(outs)),
      type = "message", duration = 5)
  }

  shiny::observeEvent(input$save_outcome, {
    if (length(.save_blocked_reasons())) {
      shiny::showNotification(
        "Cannot save: review and confirm every certainty domain first.",
        type = "error", duration = 6)
      return()
    }
    g <- grade_obj()
    if (is.null(g)) return()
    key <- .save_key()
    # grade_table() labels rows by list name, so the pmatools object's own
    # outcome_name is aligned with the key for any downstream single-outcome
    # use of the saved object.
    g$outcome_name <- key
    if (key %in% names(pma_outcomes_list(state$outcomes))) {
      shiny::showModal(shiny::modalDialog(
        title = "Outcome already saved",
        htmltools::p(sprintf(
          "\"%s\" is already in the saved list. Replace it with the current assessment?",
          key)),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("save_outcome_overwrite", "Replace",
                              class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
      return()
    }
    .store_outcome(key, g)
  })

  shiny::observeEvent(input$save_outcome_overwrite, {
    shiny::removeModal()
    if (length(.save_blocked_reasons())) return()
    g <- grade_obj()
    if (is.null(g)) return()
    key <- .save_key()
    g$outcome_name <- key
    .store_outcome(key, g)
  })

  output$saved_outcomes_list <- shiny::renderUI({
    outs <- pma_outcomes_list(state$outcomes)
    sig  <- .current_signature()
    n_stale <- sum(pma_outcomes_stale(outs, sig))
    htmltools::tagList(
      pma_stale_warning_banner(n_stale),
      pma_saved_outcomes_ui(outs,
                            delete_input_id = "outcome_delete",
                            empty_text = EDU_COPY$multi_outcome$list_empty,
                            signature = sig,
                            primary = state$sof_primary)
    )
  })
  shiny::outputOptions(output, "saved_outcomes_list", suspendWhenHidden = FALSE)

  # ----- Row order and primary outcomes -----------------------------------
  # state$outcomes stays a plain named list (see the note on
  # pma_outcomes_list()), so the reordering rules are borrowed rather than
  # reimplemented: a pmatools_set is built for the call, reorder_outcomes() /
  # set_primary() validate and apply it, and only the result is kept. Moving
  # the app's storage to the class would change the ZIP's directory layout,
  # which is a separate step.
  #
  # Both observers live here, beside Remove, and serve BOTH saved-outcome
  # lists: the Step 3 one and the Step 4 one write to the same input ids, and
  # only one step body is mounted at a time.
  .outcome_set <- function(outs, primary = character(0)) {
    .new_pmatools_set(outcomes = outs, order = names(outs),
                      primary = intersect(as.character(primary), names(outs)))
  }

  shiny::observeEvent(input$outcome_delete, {
    key  <- as.character(input$outcome_delete)[1]
    outs <- pma_outcomes_list(state$outcomes)
    if (!key %in% names(outs)) return()
    outs[[key]] <- NULL
    state$outcomes <- outs
    state$sof_primary <- intersect(state$sof_primary %||% character(0),
                                   names(outs))
    shiny::showNotification(sprintf("Removed \"%s\" from the saved outcomes.", key),
                            type = "message", duration = 4)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$outcome_move, {
    info <- input$outcome_move
    key  <- as.character(info$name %||% "")[1]
    dir  <- as.character(info$dir  %||% "")[1]
    outs <- pma_outcomes_list(state$outcomes)
    nms  <- names(outs)
    i <- match(key, nms)
    if (is.na(i)) return()
    j <- if (identical(dir, "up")) i - 1L else i + 1L
    if (j < 1L || j > length(nms)) return()
    new_order <- nms
    new_order[c(i, j)] <- nms[c(j, i)]
    set <- tryCatch(
      reorder_outcomes(.outcome_set(outs, state$sof_primary), new_order),
      error = function(e) {
        shiny::showNotification(paste("Could not reorder:", conditionMessage(e)),
                                type = "error", duration = 6)
        NULL
      })
    if (is.null(set)) return()
    state$outcomes <- set$outcomes[set$order]
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$outcome_primary, {
    key  <- as.character(input$outcome_primary$name %||% "")[1]
    outs <- pma_outcomes_list(state$outcomes)
    if (!key %in% names(outs)) return()
    cur <- intersect(state$sof_primary %||% character(0), names(outs))
    new <- if (key %in% cur) setdiff(cur, key) else c(cur, key)
    set <- tryCatch(
      set_primary(.outcome_set(outs), if (length(new)) new else NULL),
      error = function(e) {
        shiny::showNotification(
          paste("Could not set the primary outcomes:", conditionMessage(e)),
          type = "error", duration = 6)
        NULL
      })
    if (is.null(set)) return()
    state$sof_primary <- set$primary
  }, ignoreInit = TRUE)

  # ----- Per-study RoB / Indirectness editors (synced with Step 1) -----
  .step3_bulk_set <- function(col, value) {
    d <- state$rob_table
    if (is.null(d)) return()
    d[[col]] <- value
    state$rob_table <- d
  }

  shiny::observeEvent(input$step3_rob_set_low,    { .step3_bulk_set("rob", "low")  })
  shiny::observeEvent(input$step3_rob_set_some,   { .step3_bulk_set("rob", "some") })
  shiny::observeEvent(input$step3_rob_set_high,   { .step3_bulk_set("rob", "high") })
  shiny::observeEvent(input$step3_rob_clear,      { .step3_bulk_set("rob", NA_character_) })
  shiny::observeEvent(input$step3_indir_set_low,  { .step3_bulk_set("indirectness", "low")  })
  shiny::observeEvent(input$step3_indir_set_some, { .step3_bulk_set("indirectness", "some") })
  shiny::observeEvent(input$step3_indir_set_high, { .step3_bulk_set("indirectness", "high") })
  shiny::observeEvent(input$step3_indir_clear,    { .step3_bulk_set("indirectness", NA_character_) })

  # Binary consequence of a three-level entry, under the reviewer's chosen
  # boundary. "some" and an unrated study follow `some_as`; an explicit "low"
  # is always low and an explicit "high" always high. Purely a display of
  # what grade_meta() will do; it changes nothing.
  .rob_risk_group <- function(v, some_as = "high") {
    side <- if (identical(some_as, "high")) "High" else "Low"
    v <- tolower(trimws(as.character(v)))
    out <- rep(paste0(side, " (unrated)"), length(v))
    out[!is.na(v) & v == "low"]  <- "Low"
    out[!is.na(v) & v == "some"] <- paste0(side, " (some concerns)")
    out[!is.na(v) & v == "high"] <- "High"
    out
  }

  output$step3_rob_editor <- DT::renderDT({
    d <- state$rob_table
    if (is.null(d)) {
      return(DT::datatable(data.frame(message = "Load data in Step 1 first."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    some_as <- .rob_some_concerns_setting()
    tbl <- d[, c("studlab", "rob"), drop = FALSE]
    tbl[["Risk group"]] <- .rob_risk_group(d$rob, some_as)
    dt <- DT::datatable(
      tbl,
      # Only the rating is editable; "Risk group" is derived from it.
      editable = list(target = "cell", disable = list(columns = c(0, 2))),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
    DT::formatStyle(
      dt, "Risk group",
      color = DT::styleEqual(
        c("Low", "Low (some concerns)", "Low (unrated)"),
        rep("#166534", 3), default = "#b45309"),
      fontWeight = "600")
  })
  # The editor sits in a collapsed <details>. Suspended, it does not pick up a
  # change to the low/high boundary made while it is closed, so the derived
  # "Risk group" column would be stale the next time it is opened.
  shiny::outputOptions(output, "step3_rob_editor", suspendWhenHidden = FALSE)

  output$step3_indir_editor <- DT::renderDT({
    d <- state$rob_table
    if (is.null(d)) {
      return(DT::datatable(data.frame(message = "Load data in Step 1 first."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(
      d[, c("studlab", "indirectness"), drop = FALSE],
      editable = list(target = "cell", disable = list(columns = 0)),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
  })

  .step3_validate_value <- function(val) {
    if (is.na(val) || !nzchar(val)) return(NA_character_)
    val <- tolower(trimws(val))
    if (!val %in% c("low", "some", "high")) {
      shiny::showNotification(
        "Value must be 'low', 'some', or 'high' (case-insensitive).",
        type = "warning"
      )
      return(NULL)
    }
    val
  }

  shiny::observeEvent(input$step3_rob_editor_cell_edit, {
    info <- input$step3_rob_editor_cell_edit
    if (is.null(info)) return()
    d <- state$rob_table
    if (is.null(d)) return()
    val <- .step3_validate_value(as.character(info$value))
    if (is.null(val)) return()
    d$rob[info$row] <- val
    state$rob_table <- d
  })

  shiny::observeEvent(input$step3_indir_editor_cell_edit, {
    info <- input$step3_indir_editor_cell_edit
    if (is.null(info)) return()
    d <- state$rob_table
    if (is.null(d)) return()
    val <- .step3_validate_value(as.character(info$value))
    if (is.null(val)) return()
    d$indirectness[info$row] <- val
    state$rob_table <- d
  })

  # Sync state$rob_table back into state$data so grade_meta picks up edits
  # made on Step 3 immediately (without re-running Step 1 commit).
  shiny::observe({
    rt <- state$rob_table
    d  <- state$data
    if (is.null(rt) || is.null(d)) return()
    shiny::isolate({
      idx <- match(as.character(d$studlab), as.character(rt$studlab))
      changed <- FALSE
      if (any(!is.na(rt$rob))) {
        new_rob <- rt$rob[idx]
        cur_rob <- d$rob %||% rep(NA_character_, nrow(d))
        if (!identical(as.character(cur_rob), new_rob)) {
          d$rob <- new_rob
          changed <- TRUE
        }
      }
      if (any(!is.na(rt$indirectness))) {
        new_indir <- rt$indirectness[idx]
        cur_indir <- d$indirectness %||% rep(NA_character_, nrow(d))
        if (!identical(as.character(cur_indir), new_indir)) {
          d$indirectness <- new_indir
          changed <- TRUE
        }
      }
      if (changed) state$data <- d
    })
  })

  state$step3_commit <- function() TRUE
}
