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
# published artwork. They deliberately differ from the source: the risk-of-
# bias chart enumerates five direction-of-bias rules the source does not and
# rates the fifth down TWO levels, which Core GRADE 4 declines to describe at
# all, the publication-bias chart carries a registry node that is not in Core GRADE 4
# Fig 5, and the inconsistency chart names the numeric surrogates (I-squared
# > 30%, the 80% / 20% zone shares) that Core GRADE 3 declines to quantify
# and rates the opposite-sided branch down TWO levels, which Core GRADE 3
# declines to describe at all. Every <desc> says so.
#
# Three of the four say it in a caption as well. The risk-of-bias chart does
# not, and that is the one deliberate exception: its shape now follows Fig 2
# closely enough that a footnote inside the drawing reads as part of the
# source, so the claim is made in the prose beside the figure instead
# (?grade_flowcharts in R/flowcharts.R). Nothing about the claim weakened;
# only where it is written moved, and the <desc> still carries it for a reader
# who meets the file alone.
#
# They also draw fewer boxes than the algorithm has branches, wherever the
# extra box was not a decision: the risk-of-bias chart has no "any study at
# high risk of bias?" entry node (with none, the dominance share is 0 and the
# undominated route is the answer), and the publication-bias chart has no
# "qualitative assessment required" leaves (both were judged "no", and the
# caveat belongs in the note that a reader can act on).
#
# And the risk-of-bias chart draws one box FEWER than it used to on the
# undominated side: "is there appreciable evidence from the low risk of bias
# studies?" has no "no" edge, because that answer is unreachable. Not
# dominating means the low risk of bias studies carry more than 45% of the
# weight at the default gate and more than 35% at the strictest one Core
# GRADE 4 discusses, and the same paragraph puts appreciable at 35 to 45%.
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
# Risk of bias - Core GRADE 4 Fig 2, plus the five-rule direction check
# ============================================================================

build_rob <- function() {
  b <- list()
  e <- list()

  # No entry node. "Any study at high risk of bias?" used to open the chart,
  # but its "no" branch is not a decision: with no high risk of bias study the
  # dominance share is 0, which is below the gate, and there is nothing to
  # exclude. That case walks the dominance and appreciable nodes to
  # pma-rob-leaf-all like any other undominated body of evidence.
  dom     <- fc_box("pma-rob-node-dominance", "node", 20, 48, 430,
                    c("Do the high risk of bias studies",
                      "dominate the evidence?",
                      "weight share 55% or more, by default"))
  dirn    <- fc_box("pma-rob-node-direction", "node", 20, 173, 430,
                    c("Check the direction of bias",
                      "Compare the pooled estimate with and without",
                      "the high risk of bias studies"))
  # THE RULES ARE NOT DRAWN. Core GRADE 4 Fig 2 has exactly two boxes and two
  # leaves below "check direction of bias", and the five direction-of-bias
  # rules are the mechanism that decides which box is reached, not an
  # alternative to it. They were drawn as an intermediate layer of six numbered
  # boxes; the column was the tallest thing in the figure and the numbers meant
  # nothing without R/domain_rob.R open beside it. The rule that fired is
  # reported where a reader can act on it -- the fig2_branch fact, the domain
  # notes, and from there the Summary of Findings footnote and the Evidence
  # Profile -- rather than drawn as a shape the source does not have.
  #
  # Consequence, deliberate: flow_path no longer names which rule fired, only
  # which of Fig 2's two conclusions it reached. fig2_branch carries the
  # number, and carries it as a number, so nothing that reported the rule has
  # stopped being able to.
  #
  # The 55% the dominance box quotes is PMA_ROB_DOMINANT_THRESHOLD's default
  # and the 20% the <desc> still quotes is PMA_ROB_INFLATION_THRESHOLD, both in
  # R/domain_rob.R, and nothing links them to this file: the figure is
  # generated ahead of time, so it cannot read either constant, and it went on
  # saying 10% for a whole release after that one moved to 0.20. Move the
  # literals here and in the <desc> whenever a constant moves.
  # The source's two boxes, quoted as closely as a box will hold, and its two
  # leaves under them. They start at y = 470 rather than directly under the
  # direction node because `cons` and `lstay` sit in the right half of the
  # canvas, under the undominated branch, whose lowest leaf ends at 439.
  resp <- fc_box("pma-rob-node-bias-responsible", "node", 20, 470, 450,
                 c("Risk of bias may be responsible for the",
                   "apparent effect, or for the apparent lack of one",
                   "Also reached when the direction cannot be assessed"))
  cons <- fc_box("pma-rob-node-bias-conservative", "node", 490, 470, 450,
                 c("There is an apparent effect and bias would have",
                   "decreased it, or there is no apparent effect and",
                   "bias would have increased it"))
  # Two leaves, each saying one thing. The red one used to carry rule 5's
  # second level as two further lines of annotation; that made the only
  # coloured shape in the figure the wordiest one, and it named a rule the
  # figure no longer draws. .ROB_TWO_LEVEL_NOTE is the full statement, in the
  # notes a reader acts on, and the app repeats it in the caption beside the
  # figure (PMA_FLOWCHART_FIGS$departure) -- so the departure is still
  # disclosed wherever the judgment is.
  ldown <- fc_box("pma-rob-leaf-ratedown", "leaf", 20, 580, 450,
                  c("Rate down"))
  lstay <- fc_box("pma-rob-leaf-noratedown", "leaf", 490, 580, 450,
                  c("Do not rate down"))
  # The third line is not decoration: this node lost its "no" edge, and a
  # question with one answer looks like a drawing error unless it says why.
  # Reaching this side of the chart at all means the high risk of bias studies
  # carry less than the dominance threshold, so the low risk of bias share is
  # above 45% at the default gate and above 35% at the strictest one Core
  # GRADE 4 discusses -- at or above the 35-45% the same paragraph calls
  # appreciable, across the whole range. The "no" answer is unreachable.
  appr <- fc_box("pma-rob-node-appreciable", "node", 505, 173, 435,
                 c("Is there appreciable evidence from",
                   "the low risk of bias studies?",
                   "Always yes here: not dominating leaves 35% or more"))
  magn <- fc_box("pma-rob-node-magnitude", "node", 620, 275, 320,
                 c("Similar or substantially different",
                   "magnitudes of effect?"))
  lall <- fc_box("pma-rob-leaf-all", "leaf", 505, 383, 200,
                 c("Do not rate down", "Analyse all studies"))
  llow <- fc_box("pma-rob-leaf-lowonly", "leaf", 725, 383, 215,
                 c("Do not rate down", "Analyse low risk studies"))

  e$dom_yes <- fc_edge("pma-rob-edge-dominance-yes",
                       list(c(235, 121), c(235, 173)),
                       "Yes", 243, 150, "start")
  e$dom_no <- fc_edge("pma-rob-edge-dominance-no",
                      list(c(450, 84), c(722, 84), c(722, 173)),
                      "No", 460, 76, "start")
  # Straight from the direction node to the two conclusions. These two kept
  # their ids through the loss of the rule column -- they are still "which way
  # the rule check came out" -- but not their labels: the labels named the rule
  # numbers ("Rules 1 and 2", "Rules 3, 4, 5 and not assessable"), which is
  # exactly what the figure no longer shows, and each destination box is a
  # full sentence that says what the branch means without them.
  e$rules_resp <- fc_edge("pma-rob-edge-rules-responsible",
                          list(c(150, 246), c(150, 470)))
  e$rules_cons <- fc_edge("pma-rob-edge-rules-conservative",
                          list(c(360, 246), c(360, 452), c(715, 452),
                               c(715, 470)))
  e$resp_down <- fc_edge("pma-rob-edge-responsible-ratedown",
                         list(c(245, 543), c(245, 580)))
  e$cons_stay <- fc_edge("pma-rob-edge-conservative-noratedown",
                         list(c(715, 543), c(715, 580)))
  e$appr_yes <- fc_edge("pma-rob-edge-appreciable-yes",
                        list(c(800, 246), c(800, 275)),
                        "Yes", 808, 266, "start")
  # One answer, two findings. "The magnitudes are similar" and "the comparison
  # could not be made" reach the same leaf and rate down neither way, so they
  # share one arrow: a second edge drawn to the same box asked the reader to
  # hold a distinction the drawing then made nothing of. Which of the two
  # happened is still recorded, in the fig2_branch fact and the domain notes.
  e$mag_sim <- fc_edge("pma-rob-edge-magnitude-similar",
                       list(c(660, 331), c(660, 383)),
                       "Similar/Not assessable", 668, 361, "start")
  e$mag_dif <- fc_edge("pma-rob-edge-magnitude-different",
                       list(c(860, 331), c(860, 383)),
                       "Different", 868, 361, "start")

  b <- list(dom, dirn, resp, cons, ldown, lstay, appr, magn, lall, llow)

  # No caption. The "pmatools' operationalisation, not a reproduction" line was
  # the only place in the drawing that said the five rules and the -2 are
  # pmatools' own, and the closer the figure gets to Fig 2's shape the more
  # that claim is needed -- so it moves to the prose beside the figure
  # (?grade_flowcharts, and the app's own caption) rather than disappearing.
  # The <desc> below still carries it for a reader who meets the file alone.
  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(b, `[[`, "markup"), use.names = FALSE)
  )

  fc_svg(
    "rob", 650,
    "Risk of bias: the Core GRADE 4 Figure 2 decision as pmatools implements it",
    paste0("Flowchart. Do the high risk of bias studies dominate the ",
           "evidence (55% or more of the pooled weight by default)? If they ",
           "do, pmatools checks the direction of bias by comparing the ",
           "pooled estimate with and without those studies, and that check ",
           "decides which of the two conclusions Core GRADE 4 draws is ",
           "reached. One is: risk of bias may be responsible for the ",
           "apparent effect, or for the apparent lack of one, so rate down. ",
           "The other is: there is an apparent effect and bias would have ",
           "decreased it, or there is no apparent effect and bias would have ",
           "increased it, so do not rate down. The check itself is five ",
           "mutually exclusive rules -- both estimates trivial; the same zone ",
           "with a change within 20%; the same zone with a bias-favouring ",
           "change over 20%; zones differing on the same side of the null; ",
           "and zones differing across the null -- plus a direction that ",
           "cannot be assessed at all. The rules are not drawn: the one that ",
           "fired is reported in the judgment notes and in the structured ",
           "fact recorded beside them. If the high risk of bias studies do ",
           "not dominate, pmatools asks whether there is appreciable ",
           "evidence from the low risk of bias studies. Below the dominance ",
           "gate that answer is always yes, so the node has one outgoing ",
           "edge, and the chart then asks whether the two magnitudes of ",
           "effect are similar or substantially different. That question is ",
           "decided by the same rules: one that would rate down means ",
           "substantially different, one that would not means similar, and a ",
           "comparison that cannot be made shares the similar edge, since ",
           "both reach the same leaf. Neither answer rates down, but a ",
           "substantial difference restricts the analysis to the low risk of ",
           "bias studies. A body of evidence with no high risk of bias study ",
           "at all takes that same undominated route and does not rate down. ",
           "This is pmatools&#8217; operationalisation of the Core GRADE 4 ",
           "Figure 2 decision, not a reproduction of the published figure: ",
           "the five rules are pmatools&#8217; and are not enumerated in the ",
           "source; Core GRADE 4 words the magnitude question symmetrically ",
           "where the rules are directional; and it describes no two-level ",
           "risk-of-bias downgrade at all, so the two-level fifth rule is a ",
           "departure from it, which also requires a threshold to have been ",
           "supplied &#8212; without one that rule rates down one level."),
    "Risk of bias &#8212; Core GRADE 4 Fig 2, as pmatools implements it",
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
# Publication bias - Core GRADE 4 Fig 5, plus the pmatools registry node
# ============================================================================

build_pubias <- function() {
  # Unnumbered on purpose. Core GRADE 4 Fig 5 numbers these Q1 to Q4, but this
  # chart puts a pmatools node between Q1 and Q2, so numbers on screen would
  # describe neither the source nor the route. The domain notes keep the
  # "Q1:" - "Q4:" prefixes: those are the exported record, and no figure
  # travels with them.
  #
  # Two nodes here are not ordinary questions, and each says so on its own
  # third line rather than through a visual convention. The registry node is
  # pmatools' own and has no counterpart in Fig 5; the study-count node is
  # computed from the analysis and is never put to the reviewer. Both used to
  # be signalled some other way -- the registry box was drawn with a dashed
  # outline, the study count was explained in a line of app copy under the
  # chart -- and neither reached a reader who had only the figure. A dashed
  # outline in particular reads as "provisional" or "not reached yet" on a
  # chart whose whole job is to show which boxes an analysis went through.
  q1  <- fc_box("pma-pubias-node-q1", "node", 20, 48, 470,
                c("Are most or all studies small",
                  "and industry sponsored?"))
  l1  <- fc_box("pma-pubias-leaf-down1-q1", "leaf", 560, 48, 380,
                c("Rate down 1 level"))
  reg <- fc_box("pma-pubias-node-registry", "node", 20, 150, 470,
                c("Is registry coverage complete, so that every",
                  "registered trial can be accounted for?",
                  "A pmatools input; Figure 5 has no such node."))
  lreg <- fc_box("pma-pubias-leaf-nodown-registry", "leaf", 560, 167, 380,
                 c("Do not rate down"))
  q2  <- fc_box("pma-pubias-node-q2", "node", 20, 275, 470,
                c("Is statistical analysis feasible?",
                  "(a meta-analysis of 10 or more studies)",
                  "Computed from the analysis, never asked."))
  q3  <- fc_box("pma-pubias-node-q3", "node", 20, 400, 440,
                c("Does funnel plot asymmetry, visually",
                  "or by Egger&#8217;s test, strongly suggest",
                  "publication bias?"))
  q4  <- fc_box("pma-pubias-node-q4", "node", 500, 400, 440,
                c("Is there documentation of unpublished",
                  "studies (a trial registry, FDA records)?"))
  l3y <- fc_box("pma-pubias-leaf-down1-q3", "leaf", 94, 517, 140,
                c("Rate down", "1 level"))
  l3n <- fc_box("pma-pubias-leaf-nodown-q3", "leaf", 246, 517, 140,
                c("Do not", "rate down"))
  l4y <- fc_box("pma-pubias-leaf-down1-q4", "leaf", 574, 517, 140,
                c("Rate down", "1 level"))
  l4n <- fc_box("pma-pubias-leaf-nodown-q4", "leaf", 726, 517, 140,
                c("Do not", "rate down"))

  e <- list(
    fc_edge("pma-pubias-edge-q1-yes", list(c(490, 76), c(560, 76)),
            "Yes", 525, 69),
    fc_edge("pma-pubias-edge-q1-no", list(c(255, 104), c(255, 150)),
            "No", 263, 130, "start"),
    fc_edge("pma-pubias-edge-registry-yes", list(c(490, 186), c(560, 186)),
            "Yes", 525, 179),
    fc_edge("pma-pubias-edge-registry-no", list(c(255, 223), c(255, 275)),
            "No or not answered", 263, 252, "start"),
    fc_edge("pma-pubias-edge-q2-yes", list(c(200, 348), c(200, 400)),
            "Yes", 208, 377, "start"),
    fc_edge("pma-pubias-edge-q2-no",
            list(c(490, 311), c(720, 311), c(720, 400)),
            "No", 500, 303, "start"),
    fc_edge("pma-pubias-edge-q3-yes", list(c(164, 473), c(164, 517)),
            "Yes", 164, 509),
    fc_edge("pma-pubias-edge-q3-no", list(c(316, 473), c(316, 517)),
            "No", 316, 509),
    fc_edge("pma-pubias-edge-q4-yes", list(c(644, 456), c(644, 517)),
            "Yes", 644, 509),
    fc_edge("pma-pubias-edge-q4-no", list(c(796, 456), c(796, 517)),
            "No", 796, 509)
  )

  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(list(q1, l1, reg, lreg, q2, q3, q4,
                       l3y, l3n, l4y, l4n),
                  `[[`, "markup"), use.names = FALSE),
    fc_caption(597, c(
      paste0("After BMJ Core GRADE 4 (Guyatt et al., 2025) Figure 5. ",
             "pmatools&#8217; operationalisation, not a reproduction:"),
      paste0("the registry node is a pmatools input and is not in Figure 5; ",
             "the study-count node is computed, never asked.")))
  )

  fc_svg(
    "pubias", 626,
    "Publication bias: the Core GRADE 4 Figure 5 decision as pmatools implements it",
    paste0("Flowchart. The first question asks whether most or all studies ",
           "are small and industry sponsored; yes means rate down one level. ",
           "Otherwise a pmatools input, labelled on the box itself because it ",
           "is not a node of Figure 5, asks whether registry coverage is ",
           "complete; yes ",
           "means do not rate down. Otherwise the chart asks whether ",
           "statistical analysis is feasible, that is whether a ",
           "meta-analysis of ten or more studies was performed. pmatools ",
           "counts the studies and answers that node itself. If it is, ",
           "the next question asks whether funnel plot asymmetry, visually ",
           "or by Egger&#8217;s test, strongly suggests publication bias: ",
           "yes rates down one level, no does not, and a test that could not ",
           "be run takes the same no as a default while the domain note asks ",
           "for a qualitative assessment. If statistical analysis is not ",
           "feasible, the chart asks instead whether unpublished studies are ",
           "documented in a trial registry or FDA records: yes rates down ",
           "one level, and no, or no answer at all, does not. This is ",
           "pmatools&#8217; operationalisation of the Core GRADE 4 Figure 5 ",
           "decision, not a reproduction of the published figure."),
    "Publication bias &#8212; Core GRADE 4 Fig 5, as pmatools implements it",
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
