# ui_helpers.R - shadcn-style component helpers (HTML wrappers)
#
# What is left of the app's grab-bag file after it was split six ways: the
# chrome. A helper belongs here when it is a WRAPPER - it takes tags or plain
# strings, hangs a class on them and returns HTML - and when it knows nothing
# about what it is wrapping. The card, the step header, the stepper, the tab
# marks and status dots, the alert banner, the reference line and the Back /
# Next pair pass that test. So does pma_confirm_checkbox(): one <div> and one
# checkbox, built identically from step3_ui() in R/step3_grade.R and from
# .responder_block() in R/step3_threshold.R, which is exactly why it cannot
# live inside either of them.
#
# What moved out is everything that KNOWS something:
#
#   R/outcome_bank.R        the multi-outcome bank and its two modals
#   R/outcome_provenance.R  signatures, staleness, and the per-outcome input
#                           registry
#   R/judgment_display.R    certainty badges, verdicts and the Core GRADE
#                           decision flowcharts
#   R/plot_panels.R         the forest / funnel display panels and autofill
#   R/column_roles.R        Step 1's detected-columns strip, Step 2's required
#                           fields, and the risk-of-bias analysis set
#   R/sof_display.R         the Summary of Findings presentation
#
# THE RULE FOR A NEW HELPER: if it reads a GRADE object, a meta object, a
# saved-outcome list or a detect_column_roles() frame, it belongs in one of
# those six, not here. A helper here may read its arguments and the constants
# below, and nothing else.
#
# The domain-confirmation block is the one thing here that is a RULE rather
# than a wrapper: pma_domain_confirmations() and the two pma_unconfirmed_*
# helpers decide which of the six gated Step 3 tabs count as confirmed, off
# PMA_DOMAIN_LABELS and PMA_DOMAIN_CONFIRM_INPUTS. They stay because what they
# gate is chrome - the tab marks, the jump links and the Next button, all
# built three lines further down - and because splitting the labels from the
# marks they paint would let a domain be named two ways on one screen.
#
# `%||%` is defined here because every one of the six uses it. The package
# defines it too, identically; test-vendor-collisions.R allows that one shared
# name and no other.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Card with optional title and subtitle
pma_card <- function(..., title = NULL, subtitle = NULL, id = NULL) {
  htmltools::div(
    class = "pma-card", id = id,
    if (!is.null(title)) htmltools::div(class = "pma-card-header", title) else NULL,
    if (!is.null(subtitle)) htmltools::div(class = "pma-card-subtitle", subtitle) else NULL,
    ...
  )
}

# Step header: the title, and nothing else. The descriptive paragraph and the
# Step 1 "why this matters" note that used to render here are gone -- the
# once-per-session claim is EDU_COPY$intro_modal (shown from app.R), and what
# described a single control now sits beside that control.
pma_step_header <- function(title) {
  htmltools::div(
    class = "pma-step-header",
    htmltools::h2(title, style = "margin-top: 0;")
  )
}

# pma_help() was deleted here: a "(?)" span with a Bootstrap tooltip. It had
# no call site and nothing in the app ever initialised Bootstrap tooltips, so
# it had never rendered anything. It is not coming back - a tooltip is still a
# sentence somebody has to write, review and keep true, and the rule this
# release applies is that such a sentence either earns a visible line or goes.

# Stepper (4 steps) - each step is a clickable actionLink.
#
# `certainty_confirmed` is how many of the six gated Step 3 tabs the reviewer
# has confirmed; when given, the Certainty node reads "Certainty 4/6". It is an
# ARGUMENT rather than a read of state$domain_confirmed, because this helper is
# pure UI and is called from app.R, where the state it would have to reach into
# lives.
pma_stepper <- function(current_step, certainty_confirmed = NULL) {
  steps <- c("Data", "Meta-analysis", "Certainty", "Export")
  certainty_step <- match("Certainty", steps)
  step_label <- function(i) {
    if (i != certainty_step || is.null(certainty_confirmed)) return(steps[i])
    sprintf("%s %d/%d", steps[i], as.integer(certainty_confirmed),
            length(PMA_DOMAIN_LABELS))
  }
  htmltools::div(
    class = "pma-stepper",
    lapply(seq_along(steps), function(i) {
      cls <- if (i == current_step) "pma-step current"
             else if (i < current_step) "pma-step done"
             else "pma-step"
      htmltools::tagList(
        shiny::actionLink(
          inputId = paste0("step_jump_", i),
          label   = htmltools::tagList(
            htmltools::span(class = "num", i),
            htmltools::span(step_label(i))
          ),
          class = cls,
          style = "text-decoration: none; cursor: pointer;"
        ),
        if (i < length(steps)) htmltools::span(class = "sep", " > ") else NULL
      )
    })
  )
}

# ----- Vendored pmatools version ------------------------------------------
# The app sources pmatools out of R/_pmatools/ instead of installing it, so
# utils::packageVersion("pmatools") errors in the deployed app. app.R reads
# R/_pmatools/VERSION (generated by update_vendor.R) into
# options(pmatools.version_stamp = ...); this is the single place that turns
# the installed-or-vendored question into a display string. Never returns a
# hardcoded number: an unset option means the stamp is genuinely unknown.
pma_pmatools_version <- function() {
  installed <- tryCatch(as.character(utils::packageVersion("pmatools")),
                        error = function(e) NULL)
  if (!is.null(installed)) return(installed)
  stamp <- getOption("pmatools.version_stamp")
  if (is.character(stamp) && length(stamp) == 1L &&
      !is.na(stamp) && nzchar(trimws(stamp))) {
    return(sprintf("%s (vendored)", trimws(stamp)))
  }
  "(vendored; version unknown)"
}

# The same version as a bare number, for a citation.
#
# pma_pmatools_version() returns a PROVENANCE string: its " (vendored)" marker
# says the run came from staged sources rather than an installed package, which
# is exactly what the Step 2 environment block and the app footer need. A
# citation is not provenance. Step 4's "How to cite" card is copied into someone
# else's manuscript, where "Version 0.5.1 (vendored)." lands in their reference
# list and reads as part of the version number. So the marker is stripped here
# and pma_pmatools_version() is left alone -- its two callers still need it.
#
# NULL when the version is genuinely unknown, so the caller drops the version
# clause rather than printing "(vendored; version unknown)" into a bibliography.
# A citation without a version is incomplete; that string is misinformation.
# The guard is "starts with a digit" because it catches the unknown sentinel
# (which starts with "(") without having to spell the sentinel out twice.
pma_pmatools_version_number <- function() {
  version <- sub(" \\(vendored\\)$", "", pma_pmatools_version())
  if (!grepl("^[0-9]", version)) return(NULL)
  version
}

# ----- W4-A output gate: shared confirmation-domain labels -----
# Named after the keys of the state$domain_confirmed logical vector set in
# step3_server(); used by both Step 3 (banner/badge) and Step 4 (export gate).
PMA_DOMAIN_LABELS <- c(
  threshold     = "Configuration",
  rob           = "Risk of Bias",
  inconsistency = "Inconsistency",
  indirectness  = "Indirectness",
  imprecision   = "Imprecision",
  pubias        = "Publication bias"
)

# Internal keys of the domains not yet confirmed, in tab order. The labels are
# also the tabPanel values of the six gated Step 3 tabs, so a key is all a
# caller needs to jump to the tab that clears it - see pma_domain_jump_links().
pma_unconfirmed_domain_keys <- function(conf) {
  keys <- names(PMA_DOMAIN_LABELS)
  if (is.null(conf)) return(keys)
  ok <- vapply(keys, function(k) {
    k %in% names(conf) && isTRUE(conf[[k]])
  }, logical(1))
  keys[!ok]
}

# Human-readable labels of the domains not yet confirmed. `conf` is the
# named logical vector from state$domain_confirmed (NULL = nothing
# confirmed yet, e.g. before Step 3 was opened).
pma_unconfirmed_domains <- function(conf) {
  unname(PMA_DOMAIN_LABELS[pma_unconfirmed_domain_keys(conf)])
}

# ----- What confirms a domain --------------------------------------------
# One confirmation checkbox per gated tab, keyed by the domain key above.
PMA_DOMAIN_CONFIRM_INPUTS <- c(
  threshold     = "threshold_confirm",
  rob           = "rob_confirm_na",
  inconsistency = "incon_confirm_na",
  indirectness  = "indir_confirm_na",
  imprecision   = "impre_confirm_na",
  pubias        = "pubias_confirm_na"
)

# The confirmation rule of state$domain_confirmed, as a pure function of the
# checkbox values (`ticked`) and the freshness stamps (`fresh`), both named
# logical vectors keyed by INPUT id. `config_ready` is the Configuration tab's
# extra condition (config_blockers() empty): that tab gates on values actually
# being set, not only on a tick.
#
# A domain is confirmed if and only if its box is ticked FOR THE OUTCOME NOW
# OPEN. Two reasons this is deliberately narrower than what it replaced, which
# also counted substantive input and valid overrides:
#
#   - the tick the reviewer can see must be the verdict. Under the old rule a
#     domain could be reported as unconfirmed with its box ticked (a stale
#     tick), or confirmed with the box empty, and the screen said nothing;
#   - any widget that arrives PRESELECTED - the four Indirectness PICO radios
#     are about to - would otherwise satisfy "substantive input" the moment it
#     mounts, and open the export gate for an outcome nobody has looked at.
#
# Failing closed is the safe direction: an answer whose stamp is out of date
# locks the gate, it never opens it.
pma_domain_confirmations <- function(ticked, fresh, config_ready = TRUE) {
  flag <- function(v, id) {
    if (is.null(v) || !id %in% names(v)) return(FALSE)
    isTRUE(unname(v[[id]]))
  }
  out <- vapply(names(PMA_DOMAIN_CONFIRM_INPUTS), function(key) {
    id <- PMA_DOMAIN_CONFIRM_INPUTS[[key]]
    flag(ticked, id) && flag(fresh, id)
  }, logical(1))
  out[["threshold"]] <- out[["threshold"]] && isTRUE(config_ready)
  out
}

# The progress marker on a gated Step 3 tab: a tick once the domain is
# confirmed, a dot once the reviewer has opened the tab, nothing before that.
# HTML entities rather than literal glyphs, for the reason given at
# pma_wizard_nav(): the deploy bundle's encoding is not guaranteed.
pma_tab_mark <- function(confirmed, visited) {
  if (isTRUE(confirmed)) {
    return(htmltools::span(class = "pma-tab-mark pma-tab-mark-done",
                           title = "Confirmed",
                           htmltools::HTML("&#10003;")))
  }
  if (isTRUE(visited)) {
    return(htmltools::span(class = "pma-tab-mark pma-tab-mark-seen",
                           title = "Opened, not yet confirmed",
                           htmltools::HTML("&#9679;")))
  }
  NULL
}

# The status marker on a publication-bias REFERENCE tab (funnel,
# trim-and-fill, missing results). `dot` is the list(state =, reason =) the
# package's .pubias_funnel_dot() / .pubias_trimfill_dot() /
# .pubias_missing_dot() return.
#
# It is deliberately NOT pma_tab_mark(). That mark is a filled dot glyph
# meaning "the reviewer has opened this tab, and has not yet confirmed the
# domain" - their PROGRESS. These mean "the diagnostic on this tab found X",
# and they sit on a tabset nested inside one of the tabs the progress mark is
# on, so one glyph a few pixels from the other would carry two unrelated
# meanings. This marker therefore takes its own class and its own shape: a
# CSS-drawn rounded square, not a round glyph, so the two never read as the
# same thing whatever the font does.
#
# Drawn in CSS rather than written as a character for the reason
# pma_wizard_nav() gives about HTML entities: the deploy bundle's encoding is
# not guaranteed, and an empty element with a class cannot arrive mojibaked.
#
# NOTHING here rates anything. The dot's value never reaches assess_pubias()
# or grade_meta(); it is a nudge toward opening a tab the reviewer would
# otherwise skip.
PMA_STATUS_DOT_LABELS <- c(
  green   = "No concern found",
  amber   = "Worth a look",
  red     = "Concern found",
  unknown = "Not computed"
)

pma_tab_status_dot <- function(dot) {
  if (is.null(dot)) return(NULL)
  state <- dot$state
  if (is.null(state) || length(state) != 1L ||
      !state %in% names(PMA_STATUS_DOT_LABELS)) {
    return(NULL)
  }
  reason <- dot$reason %||% ""
  label  <- unname(PMA_STATUS_DOT_LABELS[[state]])
  htmltools::span(
    class = paste0("pma-tab-status pma-tab-status-", state),
    # The tooltip is the whole point of the "unknown" state: a marker that
    # says "not computed" without saying why is worse than no marker.
    title = reason,
    role  = "img",
    `aria-label` = if (nzchar(reason)) paste0(label, ": ", reason) else label
  )
}

# Comma-separated actionLinks, one per domain key, each jumping to the Step 3
# tab that clears it. `id_prefix` separates the Step 3 and Step 4 copies: both
# can exist in one session and Shiny input ids have to be unique. The observers
# that act on them live beside the message they belong to.
#
# One HTML string rather than a tag list, and `before` / `after` for the words
# on either side: htmltools joins a tag's children with a newline, which the
# browser renders as a space, so a list built out of siblings reads
# "Configuration , Risk of Bias ." Same fix as the blocked-analysis sentence.
pma_domain_jump_links <- function(keys, id_prefix, before = "", after = "") {
  if (!length(keys)) return(NULL)
  links <- vapply(keys, function(key) {
    as.character(shiny::actionLink(paste0(id_prefix, key),
                                   PMA_DOMAIN_LABELS[[key]],
                                   class = "pma-domain-jump"))
  }, character(1))
  htmltools::HTML(paste0(
    htmltools::htmlEscape(before),
    paste(links, collapse = ", "),
    htmltools::htmlEscape(after)))
}

# The widget every id in PMA_OUTCOME_CONFIRM_IDS is built with (output gate
# W4-A). Ticking one is the ONE thing that confirms its domain
# (pma_domain_confirmations()), so it is also what un-greys the Next button
# below it.
#
# This lives here, and not as a closure inside step3_ui() where it started,
# because the Configuration tab renders its confirmations from TWO files:
# `threshold_confirm` and the five `*_confirm_na` boxes come from step3_ui()
# in R/step3_grade.R, but `responder_p0_confirm` comes from .responder_block()
# in R/step3_threshold.R, which a closure in step3_ui() cannot reach. The
# consequence of that unreachability shipped: the responder confirmation was a
# bare checkboxInput() sitting in a column of numeric inputs and notes, and
# reviewers could not tell it was a click they had to make - while the boxed
# `threshold_confirm` two screens down, gating the same Next button, read
# plainly as a gate. Two gates on one tab must not look like one gate and one
# note, so there is one implementation and every call site uses it.
#
# The eyebrow is a real element rather than a CSS `content:` string so that
# the class alone is enough for a test to prove a confirmation was rendered
# through this helper. www/shadcn.css paints the UNTICKED state as the base
# rule and quietens it under :has(input:checked); see the long comment there
# for why that direction and not the other.
pma_confirm_checkbox <- function(id, label = "I have reviewed this domain") {
  htmltools::div(
    class = "pma-confirm",
    htmltools::span(class = "pma-confirm-eyebrow", "Required"),
    shiny::checkboxInput(id, label, value = FALSE, width = "100%")
  )
}

# Alert colours follow the existing warning treatment in Step 3
# (output$cert_incomplete_banner): amber #fef3c7 / #b45309, reserved for
# genuine alerts.
PMA_ALERT_BG <- "#fef3c7"
PMA_ALERT_FG <- "#b45309"

# Banner (used for the Indirectness review reminder, the risk-of-bias
# analysis-set notice and the Step 1 load confirmation). Takes any number of
# children so callers can pass a single string (the original signature) or
# structured tags.
#
# `tone = "success"` recolours it green while keeping the shape: Step 1's
# "your data loaded, here is what that means" message has to read as the same
# kind of object as the warnings sharing that screen, not as a console line.
pma_banner <- function(..., tone = c("warning", "success")) {
  tone <- match.arg(tone)
  htmltools::div(
    class = paste0("pma-banner", if (tone == "success") " pma-banner-success"),
    ...)
}

# The one reference line in the app. `...` takes the citation strings in house
# style (first author, "et al.", journal abbreviation, year), joined by the
# caller when there is more than one.
#
# `url` links the whole citation, opening in a new tab so the reviewer does not
# lose a half-filled wizard to a navigation. The argument came back in 0.5.1:
# it had been removed on the grounds that the citation carried everything a
# reviewer needed, but a reviewer who wants to check a domain against its
# source paper wants the paper, not the ability to retype the citation into a
# search box. What that removal was actually right about was the inconsistency
# -- the same paper used to render four different ways -- so the link now hangs
# off ONE map (.core_grade_doi_url() in R/utils.R) reached through ONE helper
# (pma_domain_reference() below), and every Core GRADE tab renders alike.
# Citation TEXT is unchanged and still carries no DOI.
pma_reference <- function(..., url = NULL) {
  citation <- if (is.null(url)) {
    list(...)
  } else {
    # rel = "noopener" because target = "_blank" otherwise hands the opened
    # page a live window.opener back into the app.
    list(htmltools::a(href = url, target = "_blank", rel = "noopener", ...))
  }
  htmltools::p(class = "pma-reference",
    style = "font-style: italic; color: hsl(var(--muted-foreground)); font-size: 0.85rem;",
    "Reference: ", citation
  )
}

# The reference line for one rated domain. Takes the whole EDU_COPY entry, not
# its `$ref` string, so the citation and the link are read from the same place
# and a domain cannot end up pointing at another domain's paper.
pma_domain_reference <- function(domain_copy) {
  pma_reference(domain_copy$ref,
                url = .core_grade_doi_url(domain_copy$core_grade))
}

# pma_how_collapse() was deleted here, with the five EDU_COPY `how` bodies it
# wrapped (600 words between them). Four of the five domains draw their
# decision as a flowchart with the branch taken lit up, and a picture of the
# algorithm beats a paragraph describing it; Indirectness has no flowchart and
# its PICO question labels and subdomain table carry the same ground.
# pma_reference() still names the source paper on every tab.

# Wizard navigation buttons (Back / Next).
# Use HTML entities to dodge Latin-1 encoding issues on shinyapps.io build.
pma_wizard_nav <- function(current_step, max_step = 4,
                           back_id = "btn_back", next_id = "btn_next",
                           next_label = NULL,
                           next_disabled = FALSE) {
  arrow_left  <- htmltools::HTML("&#8592;")  # left arrow
  arrow_right <- htmltools::HTML("&#8594;")  # right arrow
  next_label  <- next_label %||% htmltools::tagList("Next ", arrow_right)
  back_label  <- htmltools::tagList(arrow_left, " Back")
  htmltools::div(
    style = "display: flex; justify-content: space-between; margin-top: 1.5rem;",
    if (current_step > 1) {
      shiny::actionButton(back_id, back_label, class = "btn btn-secondary")
    } else htmltools::div(),
    if (current_step < max_step) {
      # TRUE, not the string "disabled": shiny >= 1.9 gives actionButton() a
      # real `disabled` formal it tests with isTRUE(), so a string silently
      # produced an enabled button. NULL (not FALSE) when enabled, so the
      # attribute is dropped rather than rendered as disabled="FALSE" should
      # the argument ever land in `...` on an older shiny.
      shiny::actionButton(next_id, next_label,
                          class = "btn btn-primary",
                          disabled = if (isTRUE(next_disabled)) TRUE else NULL)
    } else htmltools::div()
  )
}
