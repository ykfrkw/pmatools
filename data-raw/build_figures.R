# data-raw/build_figures.R - generate the Core GRADE decision flowcharts.
#
# Run with:  Rscript data-raw/build_figures.R
#
# Writes one SVG per flowcharted domain into inst/figures/, then copies each
# file byte-for-byte into man/figures/. Two copies are needed because the two
# consumers resolve paths differently and neither can read the other's
# directory:
#
#   inst/figures/  ships in the package tarball AND is staged into the Shiny
#                  bundle wholesale (shiny/stage_bundle.R copies inst/ into
#                  shiny/_pmatools_inst/), so this is the copy the app reads.
#   man/figures/   is the only directory Rd's \figure{} macro resolves
#                  against, so this is the copy ?grade_flowcharts renders.
#
# tests/testthat/test-flowchart-nodes.R asserts the two copies are identical,
# so an edit here that is not re-run fails the build rather than shipping a
# help page and an app that disagree.
#
# WHAT THESE FIGURES ARE. They are pmatools' operationalisation of the Core
# GRADE decision described in each cited figure, not a reproduction of the
# published artwork. They deliberately differ from the source where pmatools
# had to make the decision executable: the risk-of-bias chart (Core GRADE
# Guide Fig 7-2) is answered with pmatools' numbers at its
# substantial-differences node, the publication-bias chart (Guide Fig 9-1)
# carries a registry node that is not in the source and computes its
# important-effect and study-count nodes, and the inconsistency chart names
# the numeric surrogates (I-squared > 30%, the 80% / 20% zone shares) that
# Core GRADE 3 declines to quantify and rates the opposite-sided branch down
# TWO levels, which Core GRADE 3 declines to describe at all. Every <desc> and
# every caption says so.
#
# The risk-of-bias chart draws Guide Fig 7-2's shape and wording exactly and
# nothing else. In particular it does NOT draw the legacy 0.5.x
# direction-of-bias check (rob_direction_check = TRUE): that check is beyond
# Core GRADE (Guide Appendix 11), and a box for it would present it as part of
# the Core GRADE figure.
#
# The charts also draw fewer boxes than the algorithm has branches, wherever
# the extra box was not a decision: the risk-of-bias chart has no "any study
# at high risk of bias?" entry node (with none, the dominance share is 0 and
# the undominated route is the answer), and the publication-bias chart has no
# "qualitative assessment required" leaves (both were judged "no", and the
# caveat belongs in the note that a reader can act on).
#
# STYLING CONTRACT (see shiny/www/shadcn.css and shiny/www/flowchart.js):
#   - no width/height on <svg>, so CSS can scale it;
#   - no inline style= and no fill=/stroke= on shapes: every colour comes
#     from a class, so the app's tokens apply and the highlight rule wins
#     without !important;
#   - the <style> block inside each file carries standalone defaults for
#     man/, where no app CSS exists. The app's rules are written with one
#     more class (".pma-flowchart .pma-fc-node rect") so they outrank these
#     despite the inlined <style> coming later in document order;
#   - every node, edge and terminal is a <g id="pma-<figkey>-<kind>-<slug>"
#     class="pma-fc-<kind>">, and those ids are the vocabulary the assessors'
#     `flow_path` fact draws on.

FIG_DIR <- file.path("inst", "figures")
MAN_DIR <- file.path("man", "figures")

FONT_SIZE <- 13
LINE_H    <- 17
PAD_Y     <- 11
PAD_X     <- 12

# --- primitives --------------------------------------------------------------

# One box. `lines` is a character vector, one <tspan> each; `align` is
# "middle" (default) or "start". Returns a list carrying both the markup and
# the geometry, so edges can be expressed against real coordinates rather
# than repeated magic numbers.
fc_box <- function(id, kind, x, y, w, lines, align = "middle") {
  h  <- 2 * PAD_Y + length(lines) * LINE_H
  tx <- if (identical(align, "start")) x + PAD_X else x + w / 2
  y0 <- y + PAD_Y + 12
  tspans <- vapply(seq_along(lines), function(i) {
    sprintf('      <tspan x="%g" dy="%s">%s</tspan>',
            tx, if (i == 1L) "0" else as.character(LINE_H), lines[i])
  }, character(1))
  cls <- paste0("pma-fc-", kind)
  markup <- c(
    sprintf('  <g id="%s" class="%s">', id, cls),
    sprintf('    <rect x="%g" y="%g" width="%g" height="%g" rx="6" ry="6"/>',
            x, y, w, h),
    sprintf('    <text x="%g" y="%g" text-anchor="%s">', tx, y0, align),
    tspans,
    '    </text>',
    '  </g>'
  )
  list(markup = markup, x = x, y = y, w = w, h = h,
       cx = x + w / 2, cy = y + h / 2, right = x + w, bottom = y + h)
}

# One edge: an orthogonal polyline through `pts` (a list of c(x, y)), a
# stroked chevron at the far end, and an optional label. The chevron is
# stroked rather than filled so it inherits the same stroke colour and
# thickness as the line, including when the highlight class is applied.
fc_edge <- function(id, pts, label = NULL, lx = NULL, ly = NULL,
                    anchor = "middle") {
  d <- paste0("M ", paste(vapply(pts, function(p) sprintf("%g %g", p[1], p[2]),
                                 character(1)),
                          collapse = " L "))
  n  <- length(pts)
  p2 <- pts[[n]]
  p1 <- pts[[n - 1L]]
  dx <- p2[1] - p1[1]
  dy <- p2[2] - p1[2]
  len <- sqrt(dx^2 + dy^2)
  ux <- dx / len
  uy <- dy / len
  # Perpendicular, for the two chevron arms.
  px <- -uy
  py <- ux
  a  <- 7   # arm length back along the line
  b  <- 4.5 # arm half-width across it
  head_d <- sprintf("M %g %g L %g %g L %g %g",
                    p2[1] - a * ux + b * px, p2[2] - a * uy + b * py,
                    p2[1], p2[2],
                    p2[1] - a * ux - b * px, p2[2] - a * uy - b * py)
  lab <- if (is.null(label)) character(0) else {
    sprintf('    <text class="pma-fc-edge-label" x="%g" y="%g" text-anchor="%s">%s</text>',
            lx, ly, anchor, label)
  }
  markup <- c(
    sprintf('  <g id="%s" class="pma-fc-edge">', id),
    sprintf('    <path d="%s"/>', d),
    sprintf('    <path d="%s"/>', head_d),
    lab,
    '  </g>'
  )
  list(markup = markup)
}

# The standalone defaults. Kept at one class of specificity so the app's
# ".pma-flowchart ..." rules outrank them; see the styling contract above.
FC_STYLE <- c(
  '  <style>',
  '    .pma-fc-node rect, .pma-fc-leaf rect {',
  '      fill: #ffffff; stroke: #cbd5e1; stroke-width: 1.2;',
  '    }',
  '    .pma-fc-edge path { stroke: #cbd5e1; stroke-width: 1.5; fill: none; }',
  '    .pma-fc-node text, .pma-fc-leaf text, .pma-fc-edge text {',
  '      fill: #475569; font-size: 13px; font-weight: 400;',
  '      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,',
  '        "Helvetica Neue", Arial, sans-serif;',
  '    }',
  '    .pma-fc-edge-label { font-size: 12px; }',
  '    .pma-fc-caption { fill: #64748b; font-size: 12px; font-style: italic;',
  '      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,',
  '        "Helvetica Neue", Arial, sans-serif; }',
  '    .pma-fc-heading { fill: #0f172a; font-size: 14px; font-weight: 600;',
  '      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,',
  '        "Helvetica Neue", Arial, sans-serif; }',
  '    .pma-fc-on.pma-fc-node rect, .pma-fc-on.pma-fc-leaf rect {',
  '      stroke: #0f172a; stroke-width: 2.5;',
  '    }',
  '    .pma-fc-on.pma-fc-edge path { stroke: #0f172a; stroke-width: 3; }',
  '    .pma-fc-on text { fill: #0f172a; font-weight: 600; }',
  '  </style>'
)

# The provenance line under the figure. Multi-line, because a single 960-unit
# line does not hold a sentence that has to name the source figure AND say
# what pmatools changed; an overflowing <text> is simply clipped.
fc_caption <- function(y, lines) {
  c(
    sprintf('  <text class="pma-fc-caption" x="20" y="%g">', y),
    vapply(seq_along(lines), function(i) {
      sprintf('    <tspan x="20" dy="%s">%s</tspan>',
              if (i == 1L) "0" else "16", lines[i])
    }, character(1)),
    '  </text>'
  )
}

fc_svg <- function(figkey, height, title, desc, heading, body) {
  c(
    sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" ',
                   'viewBox="0 0 960 %g" preserveAspectRatio="xMidYMid meet" ',
                   'role="img" aria-labelledby="%s-title %s-desc">'),
            height, figkey, figkey),
    sprintf('  <title id="%s-title">%s</title>', figkey, title),
    sprintf('  <desc id="%s-desc">%s</desc>', figkey, desc),
    FC_STYLE,
    sprintf('  <text class="pma-fc-heading" x="20" y="26">%s</text>', heading),
    body,
    '</svg>',
    ''
  )
}

write_fig <- function(figkey, lines) {
  path <- file.path(FIG_DIR, paste0(figkey, ".svg"))
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(lines, con = con, sep = "\n")
  invisible(path)
}

# ============================================================================
# Risk of bias - Core GRADE Guide Fig 7-2 (formerly Core GRADE 4 Fig 2)
# ============================================================================

build_rob <- function() {
  # Guide Fig 7-2's three boxes and three leaves, with the source's wording on
  # every one of them. The figure has no "check direction of bias" node: the
  # Guide moved that consideration beyond Core GRADE (Appendix 11), and the
  # legacy 0.5.x check pmatools keeps behind rob_direction_check = TRUE is
  # deliberately not drawn -- a box for it would present a Beyond-Core-GRADE
  # step as part of the Core GRADE figure. In legacy mode flow_path lights
  # the dominance node and its Yes edge only.
  #
  # No entry node. With no high risk of bias study the dominance share is 0,
  # below the gate, and that case walks the undominated route like any other.
  #
  # The source draws the dominated leaf in red. pmatools does not: every
  # colour here comes from a class (see the styling contract), and the app's
  # highlight is the only colour signal the charts carry.
  dom  <- fc_box("pma-rob-node-dominance", "node", 20, 48, 430,
                 c("Is evidence dominated by high",
                   "risk of bias studies?*"))
  down <- fc_box("pma-rob-leaf-ratedown", "leaf", 20, 165, 430,
                 c("Use results from all studies and rate down"))
  # A statement, not a question, as in the source: below the dominance gate
  # the low risk of bias studies always carry the footnote's 35-45%, so the
  # box has one outgoing edge and no labels on it.
  appr <- fc_box("pma-rob-node-appreciable", "node", 505, 48, 435,
                 c("Appreciable evidence exists from",
                   "low risk of bias studies&#8224;"))
  magn <- fc_box("pma-rob-node-magnitude", "node", 505, 150, 435,
                 c("Are there substantial differences in estimate of",
                   "effect between high and low risk of bias studies?"))
  llow <- fc_box("pma-rob-leaf-lowonly", "leaf", 505, 275, 210,
                 c("Use only low risk of bias",
                   "studies and do not",
                   "rate down"))
  lall <- fc_box("pma-rob-leaf-all", "leaf", 730, 275, 210,
                 c("Use results from all",
                   "studies and do not",
                   "rate down"))

  e <- list(
    fc_edge("pma-rob-edge-dominance-yes",
            list(c(235, dom$bottom), c(235, down$y)),
            "Yes", 243, 140, "start"),
    fc_edge("pma-rob-edge-dominance-no",
            list(c(dom$right, 76), c(appr$x, 76)),
            "No", 477, 69),
    fc_edge("pma-rob-edge-appreciable-magnitude",
            list(c(722, appr$bottom), c(722, magn$y))),
    # "Not assessable" shares the No edge: both keep every study and rate
    # down neither way. Which of the two happened is in the fig2_branch fact.
    fc_edge("pma-rob-edge-magnitude-different",
            list(c(610, magn$bottom), c(610, llow$y)),
            "Yes", 618, 254, "start"),
    fc_edge("pma-rob-edge-magnitude-similar",
            list(c(835, magn$bottom), c(835, lall$y)),
            "No", 843, 254, "start")
  )

  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(list(dom, down, appr, magn, llow, lall), `[[`, "markup"),
           use.names = FALSE),
    # The two footnotes are the source's, verbatim; the parenthesis after the
    # first is pmatools' default, and the last line is the provenance every
    # other chart carries.
    fc_caption(370, c(
      paste0("* Possible thresholds for high risk of bias &#8216;dominating&#8217;: ",
             "&gt;65% weight or &#8805;55% weight=possibly dominating ",
             "(pmatools default: 55%)."),
      paste0("&#8224; Possible thresholds for appreciable low risk of bias ",
             "evidence, corresponding to thresholds for high risk of bias ",
             "dominating, &#8805;35% or &#8805;45% weight."),
      paste0("After the Core GRADE Guide (Guyatt et al.) Figure 7-2, formerly ",
             "Core GRADE 4 Figure 2. pmatools&#8217; operationalisation, not a ",
             "reproduction:"),
      paste0("the numbers that answer the substantial-differences question ",
             "are pmatools&#8217;; see ?grade_flowcharts.")))
  )

  fc_svg(
    "rob", 440,
    "Risk of bias: the Core GRADE Guide Figure 7-2 decision as pmatools implements it",
    paste0("Flowchart. Is evidence dominated by high risk of bias studies? ",
           "Possible thresholds for dominating are more than 65% of the ",
           "weight, or 55% or more as possibly dominating; pmatools uses 55% ",
           "by default. Yes: use results from all studies and rate down. No: ",
           "appreciable evidence exists from low risk of bias studies, with ",
           "corresponding thresholds of 35% or 45% of the weight, and the ",
           "chart asks whether there are substantial differences in estimate ",
           "of effect between high and low risk of bias studies. Yes: use only ",
           "low risk of bias studies and do not rate down. No: use results ",
           "from all studies and do not rate down. A comparison that cannot ",
           "be made takes the No edge, and a body of evidence with no high ",
           "risk of bias study takes that route too. This is pmatools&#8217; ",
           "operationalisation of the Core GRADE Guide Figure 7-2 decision ",
           "(formerly Core GRADE 4 Figure 2), not a reproduction of the ",
           "published figure: the Guide words the substantial-differences ",
           "question qualitatively, and pmatools answers it with a change of ",
           "zone relative to the chosen threshold or a relative change in ",
           "magnitude of more than 20% in either direction. The legacy ",
           "pmatools 0.5.x direction-of-bias check, which is beyond Core GRADE ",
           "and available only on request, is not drawn."),
    "Risk of bias &#8212; Core GRADE Guide Fig 7-2, as pmatools implements it",
    body
  )
}

# ============================================================================
# Inconsistency - Core GRADE 3 Fig 2, three sequential steps
# ============================================================================

build_incon <- function() {
  step1 <- fc_box("pma-incon-node-step1", "node", 20, 48, 470,
                  c("Step 1  Are there important differences in point",
                    "estimates and limited overlap of confidence intervals?",
                    "pmatools surrogate for the visual step: I&#178; above 30%"))
  nd1   <- fc_box("pma-incon-leaf-nodown1", "leaf", 560, 48, 380,
                  c("Do not rate down"))
  step2 <- fc_box("pma-incon-node-step2", "node", 20, 180, 470,
                  c("Step 2  Where do the point estimates fall",
                    "relative to the chosen threshold?",
                    "pmatools surrogates: 80% in one zone, 20% on each side"))
  nd2   <- fc_box("pma-incon-leaf-nodown2", "leaf", 560, 150, 380,
                  c("Do not rate down",
                    "A clear majority sits in one zone"))
  sc    <- fc_box("pma-incon-leaf-down1-scattered", "leaf", 560, 222, 380,
                  c("Rate down 1 level",
                    "No zone holds a majority and neither side is substantial"))
  step3 <- fc_box("pma-incon-node-step3", "node", 20, 305, 470,
                  c("Step 3  Is the opposite-sided difference",
                    "explained by a credible subgroup?"))
  nd3   <- fc_box("pma-incon-leaf-nodown3", "leaf", 560, 296, 380,
                  c("Do not rate down",
                    "Report the subgroups as separate questions"))
  # Two levels, and pmatools knows Core GRADE 3 says otherwise; the caption
  # and R/domain_inconsistency.R carry the reasoning. The box carried a second
  # line reading "(serious)", which was simply the wrong word for this leaf -
  # the branch that reaches it assigns judgment = "very_serious" - so the line
  # is gone rather than corrected. `y` moves down with the box's own height, to
  # keep the pma-incon-edge-step3-no arrowhead landing on the text rather than
  # on the bottom edge.
  d2    <- fc_box("pma-incon-leaf-down2", "leaf", 560, 377, 380,
                  c("Rate down 2 levels"))

  e <- list(
    fc_edge("pma-incon-edge-step1-no", list(c(490, 84), c(560, 84)),
            "No", 525, 77),
    fc_edge("pma-incon-edge-step1-yes", list(c(255, 121), c(255, 180)),
            "Yes", 263, 154, "start"),
    fc_edge("pma-incon-edge-step2-majority", list(c(490, 195), c(560, 195)),
            "Majority", 525, 188),
    fc_edge("pma-incon-edge-step2-scattered", list(c(490, 240), c(560, 240)),
            "Scattered", 525, 233),
    fc_edge("pma-incon-edge-step2-opposite", list(c(255, 253), c(255, 305)),
            "Opposite sides", 263, 282, "start"),
    fc_edge("pma-incon-edge-step3-yes", list(c(490, 320), c(560, 320)),
            "Yes", 525, 313),
    fc_edge("pma-incon-edge-step3-no",
            list(c(255, 361), c(255, 396), c(560, 396)),
            "No", 263, 388, "start")
  )

  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(list(step1, nd1, step2, nd2, sc, step3, nd3, d2),
                  `[[`, "markup"), use.names = FALSE),
    fc_caption(446, c(
      paste0("After BMJ Core GRADE 3 (Guyatt et al., 2025) Figure 2. ",
             "pmatools&#8217; operationalisation, not a reproduction: the ",
             "numeric surrogates are pmatools&#8217; own, and the"),
      paste0("two-level leaf departs from Core GRADE 3, which calls a ",
             "two-level inconsistency downgrade unusual enough to leave out.")))
  )

  fc_svg(
    "incon", 476,
    "Inconsistency: the Core GRADE 3 Figure 2 decision as pmatools implements it",
    paste0("Flowchart with three sequential steps. Step 1 asks whether there ",
           "are important differences in point estimates and limited overlap ",
           "of confidence intervals; pmatools automates this with an ",
           "I-squared above 30% surrogate. No means do not rate down. Yes ",
           "leads to Step 2, which asks where the point estimates fall ",
           "relative to the chosen threshold; pmatools automates this with a ",
           "zone tally, 80% in one zone counting as a majority and 20% on ",
           "each side as substantial. A majority means do not rate down; a ",
           "scattered tally means rate down one level; estimates on opposite ",
           "sides lead to Step 3, which asks whether a credible subgroup ",
           "explains the difference. Yes means do not rate down and report ",
           "the subgroups separately; no means rate down two levels, because ",
           "the direction of the effect is then unresolved. This is ",
           "pmatools&#8217; operationalisation of the Core GRADE 3 Figure 2 ",
           "decision, not a reproduction of the published figure: Core GRADE ",
           "3 words these nodes qualitatively and quantifies none of them, ",
           "and it declines to describe a two-level inconsistency downgrade ",
           "at all, calling the case unusual enough that Core GRADE users ",
           "need not consider it."),
    "Inconsistency &#8212; Core GRADE 3 Fig 2, as pmatools implements it",
    body
  )
}

# ============================================================================
# Imprecision - Core GRADE 2 Fig 4
# ============================================================================

build_impre <- function() {
  cross <- fc_box("pma-impre-node-crosses", "node", 20, 48, 470,
                  c("Does the pooled 95% confidence interval",
                    "cross the chosen threshold?"))
  both  <- fc_box("pma-impre-node-both", "node", 560, 48, 380,
                  c("Does it also cross the second threshold",
                    "(important benefit and important harm)?"))
  d1    <- fc_box("pma-impre-leaf-down1", "leaf", 560, 140, 180,
                  c("Rate down 1 level"))
  d2b   <- fc_box("pma-impre-leaf-down2-both", "leaf", 760, 140, 180,
                  c("Rate down 2 levels"))
  large <- fc_box("pma-impre-node-large", "node", 20, 200, 470,
                  c("Is the effect implausibly large?",
                    "binary: relative risk reduction above 30 to 40% (source)",
                    "continuous: standardised effect 0.8 or more (pmatools)"))
  ndm   <- fc_box("pma-impre-leaf-nodown-moderate", "leaf", 560, 198, 380,
                  c("Do not rate down"))
  ois   <- fc_box("pma-impre-node-ois", "node", 20, 325, 470,
                  c("Optimal information size (OIS)",
                    "Compare the total number of participants",
                    "with the OIS for this outcome"))
  ndo   <- fc_box("pma-impre-leaf-nodown-ois", "leaf", 560, 330, 380,
                  c("Do not rate down",
                    "Total N of 800 or more (continuous rule of thumb),",
                    "N at or above the OIS, or the OIS is not computable"))
  d1o   <- fc_box("pma-impre-leaf-down1-ois", "leaf", 20, 440, 225,
                  c("Rate down 1 level", "N below the OIS"))
  d2o   <- fc_box("pma-impre-leaf-down2-ois", "leaf", 265, 440, 225,
                  c("Rate down 2 levels",
                    "CI ratio of 3 (RR) or 2.5 (OR)",
                    "or N below 30% of the OIS"))

  e <- list(
    fc_edge("pma-impre-edge-crosses-yes", list(c(490, 76), c(560, 76)),
            "Yes", 525, 69),
    fc_edge("pma-impre-edge-both-no", list(c(655, 104), c(655, 140)),
            "No", 663, 126, "start"),
    fc_edge("pma-impre-edge-both-yes", list(c(855, 104), c(855, 140)),
            "Yes", 863, 126, "start"),
    fc_edge("pma-impre-edge-crosses-no", list(c(255, 104), c(255, 200)),
            "No", 263, 156, "start"),
    fc_edge("pma-impre-edge-large-no", list(c(490, 218), c(560, 218)),
            "No", 525, 211),
    fc_edge("pma-impre-edge-large-yes", list(c(255, 273), c(255, 325)),
            "Yes", 263, 302, "start"),
    fc_edge("pma-impre-edge-ois-nodown", list(c(490, 360), c(560, 360)),
            "Reached", 525, 353),
    fc_edge("pma-impre-edge-ois-down1", list(c(160, 398), c(160, 440)),
            "N below", 168, 424, "start"),
    fc_edge("pma-impre-edge-ois-down2", list(c(350, 398), c(350, 440)),
            "N far below", 358, 424, "start")
  )

  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(list(cross, both, d1, d2b, large, ndm, ois, ndo, d1o, d2o),
                  `[[`, "markup"), use.names = FALSE),
    fc_caption(537, paste0(
      "After BMJ Core GRADE 2 (Guyatt et al., 2025) Figure 4. ",
      "pmatools&#8217; operationalisation, not a reproduction."))
  )

  fc_svg(
    "impre", 550,
    "Imprecision: the Core GRADE 2 Figure 4 decision as pmatools implements it",
    paste0("Flowchart. Does the pooled 95% confidence interval cross the ",
           "chosen threshold? If yes, does it also cross the second ",
           "threshold, important benefit and important harm? No means rate ",
           "down one level; yes means rate down two. If the interval does ",
           "not cross the threshold, is the effect implausibly large? No ",
           "means do not rate down. Yes leads to the optimal ",
           "information size comparison: a total of 800 or more participants ",
           "on the continuous rule of thumb, a total at or above the OIS, or ",
           "an OIS that cannot be computed, all mean do not rate down; a ",
           "total below the OIS means rate down one level; a confidence ",
           "interval ratio of 3 for a relative risk or 2.5 for an odds ",
           "ratio, or a total below 30% of the OIS, means rate down two. ",
           "This is pmatools&#8217; operationalisation of the Core GRADE 2 ",
           "Figure 4 decision, not a reproduction of the published figure."),
    "Imprecision &#8212; Core GRADE 2 Fig 4, as pmatools implements it",
    body
  )
}

# ============================================================================
# Publication bias - Core GRADE Guide Fig 9-1, plus the pmatools registry node
# ============================================================================

build_pubias <- function() {
  # Unnumbered on purpose. The source's nodes are referred to as Q0 to Q4 in
  # the domain notes, but this chart puts a pmatools node between Q1 and Q2,
  # so numbers on screen would describe neither the source nor the route.
  #
  # Three nodes are not ordinary questions, and each says so on its own last
  # line rather than through a visual convention: the important-effect node
  # and the study-count node are computed from the analysis, and the registry
  # node is pmatools' own and has no counterpart in the source.
  #
  # Wording follows Guide Fig 9-1 node by node. `oy` is the height the new
  # first node added; everything below it moved down by that much.
  oy  <- 102
  q0  <- fc_box("pma-pubias-node-q0", "node", 20, 48, 470,
                c("Did the studies show an important effect?",
                  "Computed from the pooled estimate and the threshold."))
  l0  <- fc_box("pma-pubias-leaf-nodown-q0", "leaf", 560, 48, 380,
                c("Do not rate down"))
  q1  <- fc_box("pma-pubias-node-q1", "node", 20, 48 + oy, 470,
                c("Are most or all of the studies small",
                  "and industry sponsored?"))
  l1  <- fc_box("pma-pubias-leaf-down1-q1", "leaf", 560, 48 + oy, 380,
                c("Rate down 1 level"))
  reg <- fc_box("pma-pubias-node-registry", "node", 20, 150 + oy, 470,
                c("Is registry coverage complete, so that every",
                  "registered trial can be accounted for?",
                  "A pmatools input; the source has no such node."))
  lreg <- fc_box("pma-pubias-leaf-nodown-registry", "leaf", 560, 167 + oy, 380,
                 c("Do not rate down"))
  q2  <- fc_box("pma-pubias-node-q2", "node", 20, 275 + oy, 470,
                c("Are there enough studies (10 or more)",
                  "to examine funnel plot asymmetry?",
                  "Computed from the analysis, never asked."))
  q3  <- fc_box("pma-pubias-node-q3", "node", 20, 400 + oy, 440,
                c("Does visual inspection of the funnel plot",
                  "demonstrate substantial asymmetry, and/or",
                  "is funnel plot asymmetry statistically",
                  "significant (Egger&#8217;s test)?"))
  q4  <- fc_box("pma-pubias-node-q4", "node", 500, 400 + oy, 440,
                c("Did the review team discover and document",
                  "unpublished studies of sufficient size to",
                  "appreciably influence effect estimates",
                  "(e.g., registry of FDA)?"))
  ly  <- 520 + oy
  l3y <- fc_box("pma-pubias-leaf-down1-q3", "leaf", 94, ly, 140,
                c("Rate down", "1 level"))
  l3n <- fc_box("pma-pubias-leaf-nodown-q3", "leaf", 246, ly, 140,
                c("Do not", "rate down"))
  l4y <- fc_box("pma-pubias-leaf-down1-q4", "leaf", 574, ly, 140,
                c("Rate down", "1 level"))
  l4n <- fc_box("pma-pubias-leaf-nodown-q4", "leaf", 726, ly, 140,
                c("Do not", "rate down"))

  e <- list(
    fc_edge("pma-pubias-edge-q0-no", list(c(490, 76), c(560, 76)),
            "No", 525, 69),
    fc_edge("pma-pubias-edge-q0-yes", list(c(255, q0$bottom), c(255, q1$y)),
            "Yes", 263, 130, "start"),
    fc_edge("pma-pubias-edge-q1-yes", list(c(490, 76 + oy), c(560, 76 + oy)),
            "Yes", 525, 69 + oy),
    fc_edge("pma-pubias-edge-q1-no", list(c(255, q1$bottom), c(255, reg$y)),
            "No", 263, 130 + oy, "start"),
    fc_edge("pma-pubias-edge-registry-yes",
            list(c(490, 186 + oy), c(560, 186 + oy)),
            "Yes", 525, 179 + oy),
    fc_edge("pma-pubias-edge-registry-no",
            list(c(255, reg$bottom), c(255, q2$y)),
            "No or not answered", 263, 252 + oy, "start"),
    fc_edge("pma-pubias-edge-q2-yes", list(c(200, q2$bottom), c(200, q3$y)),
            "Yes", 208, 377 + oy, "start"),
    fc_edge("pma-pubias-edge-q2-no",
            list(c(490, 311 + oy), c(720, 311 + oy), c(720, q4$y)),
            "No", 500, 303 + oy, "start"),
    fc_edge("pma-pubias-edge-q3-yes", list(c(164, q3$bottom), c(164, ly)),
            "Yes", 172, ly - 9, "start"),
    fc_edge("pma-pubias-edge-q3-no", list(c(316, q3$bottom), c(316, ly)),
            "No", 324, ly - 9, "start"),
    fc_edge("pma-pubias-edge-q4-yes", list(c(644, q4$bottom), c(644, ly)),
            "Yes", 652, ly - 9, "start"),
    fc_edge("pma-pubias-edge-q4-no", list(c(796, q4$bottom), c(796, ly)),
            "No", 804, ly - 9, "start")
  )

  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(list(q0, l0, q1, l1, reg, lreg, q2, q3, q4,
                       l3y, l3n, l4y, l4n),
                  `[[`, "markup"), use.names = FALSE),
    fc_caption(l3y$bottom + 30, c(
      paste0("After the Core GRADE Guide (Guyatt et al.) Figure 9-1, formerly ",
             "Core GRADE 4 Figure 5. pmatools&#8217; operationalisation, not a ",
             "reproduction:"),
      paste0("the registry node is a pmatools input and is not in the source; ",
             "the important-effect and study-count nodes are computed, never ",
             "asked;"),
      "the p &lt; 0.05 cut-off for Egger&#8217;s test is pmatools&#8217;."))
  )

  fc_svg(
    "pubias", l3y$bottom + 30 + 2 * 16 + 20,
    "Publication bias: the Core GRADE Guide Figure 9-1 decision as pmatools implements it",
    paste0("Flowchart. The first question asks whether the studies showed an ",
           "important effect; pmatools answers it from the pooled point ",
           "estimate and the threshold the rating target uses, and no means ",
           "do not rate down. Otherwise the next question asks whether most ",
           "or all of the studies are small and industry sponsored; yes means ",
           "rate down one level. Otherwise a pmatools input, labelled on the ",
           "box itself because it is not a node of the source, asks whether ",
           "registry coverage is complete; yes means do not rate down. ",
           "Otherwise the chart asks whether there are enough studies, ten or ",
           "more, to examine funnel plot asymmetry; pmatools counts the ",
           "studies and answers that node itself. If there are, the next ",
           "question asks whether visual inspection of the funnel plot ",
           "demonstrates substantial asymmetry and/or the asymmetry is ",
           "statistically significant by Egger&#8217;s test at p below 0.05: ",
           "yes rates down one level, no does not, and a test that could not ",
           "be run takes the same no as a default while the domain note asks ",
           "for a qualitative assessment. If there are not, the chart asks ",
           "instead whether the review team discovered and documented ",
           "unpublished studies of sufficient size to appreciably influence ",
           "effect estimates: yes rates down one level, and no, or no answer ",
           "at all, does not. This is pmatools&#8217; operationalisation of ",
           "the Core GRADE Guide Figure 9-1 decision (formerly Core GRADE 4 ",
           "Figure 5), not a reproduction of the published figure."),
    "Publication bias &#8212; Core GRADE Guide Fig 9-1, as pmatools implements it",
    body
  )
}

# ============================================================================

main <- function() {
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(MAN_DIR, showWarnings = FALSE, recursive = TRUE)

  figs <- list(rob    = build_rob(),
               incon  = build_incon(),
               impre  = build_impre(),
               pubias = build_pubias())

  for (key in names(figs)) {
    src <- write_fig(key, figs[[key]])
    dst <- file.path(MAN_DIR, basename(src))
    file.copy(src, dst, overwrite = TRUE)
    cat(sprintf("  %-12s %6.1f KB  ->  %s\n", basename(src),
                file.size(src) / 1024, dst))
  }
  cat("Done.\n")
}

main()
