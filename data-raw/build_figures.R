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
# bias chart enumerates five direction-of-bias rules the source does not, the
# publication-bias chart carries a registry node that is not in Core GRADE 4
# Fig 5, and the inconsistency chart names the numeric surrogates (I-squared
# > 30%, the 80% / 20% zone shares) that Core GRADE 3 declines to quantify.
# Every <desc> says so.
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
fc_box <- function(id, kind, x, y, w, lines, align = "middle",
                   extra_class = NULL) {
  h  <- 2 * PAD_Y + length(lines) * LINE_H
  tx <- if (identical(align, "start")) x + PAD_X else x + w / 2
  y0 <- y + PAD_Y + 12
  tspans <- vapply(seq_along(lines), function(i) {
    sprintf('      <tspan x="%g" dy="%s">%s</tspan>',
            tx, if (i == 1L) "0" else as.character(LINE_H), lines[i])
  }, character(1))
  cls <- paste(c(paste0("pma-fc-", kind), extra_class), collapse = " ")
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
  '    .pma-fc-pmatools rect { stroke-dasharray: 5 3; }',
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

  anyhigh <- fc_box("pma-rob-node-anyhigh", "node", 20, 48, 430,
                    c("Any study at high risk of bias?"))
  nohigh  <- fc_box("pma-rob-leaf-nohigh", "leaf", 505, 48, 435,
                    c("Do not rate down",
                      "There is no high risk of bias study to check"))
  dom     <- fc_box("pma-rob-node-dominance", "node", 20, 139, 430,
                    c("Do the high risk of bias studies",
                      "dominate the evidence?",
                      "weight share 55% or more, by default"))
  dirn    <- fc_box("pma-rob-node-direction", "node", 20, 264, 430,
                    c("Check the direction of bias",
                      "Compare the pooled estimate with and without",
                      "the high risk of bias studies"))
  rules <- list(
    fc_box("pma-rob-leaf-rule1", "leaf", 35, 369, 400,
           c("1  both estimates trivial  &#8594;  do not rate down"),
           align = "start"),
    fc_box("pma-rob-leaf-rule2", "leaf", 35, 414, 400,
           c("2  same zone, change within 10%  &#8594;  do not rate down"),
           align = "start"),
    fc_box("pma-rob-leaf-rule3", "leaf", 35, 459, 400,
           c("3  same zone, bias-favouring change over 10%  &#8594;  rate down 1"),
           align = "start"),
    fc_box("pma-rob-leaf-rule4", "leaf", 35, 504, 400,
           c("4  zones differ, same side of the null  &#8594;  rate down 1"),
           align = "start"),
    fc_box("pma-rob-leaf-rule5", "leaf", 35, 549, 400,
           c("5  zones differ across the null  &#8594;  rate down 1"),
           align = "start"),
    fc_box("pma-rob-leaf-rulena", "leaf", 35, 594, 400,
           c("&#8211;  direction not assessable  &#8594;  rate down 1"),
           align = "start")
  )
  appr <- fc_box("pma-rob-node-appreciable", "node", 505, 264, 435,
                 c("Is there appreciable evidence from",
                   "the low risk of bias studies?"))
  magn <- fc_box("pma-rob-node-magnitude", "node", 620, 366, 320,
                 c("Similar or substantially different",
                   "magnitudes of effect?"))
  lall <- fc_box("pma-rob-leaf-all", "leaf", 505, 474, 200,
                 c("Do not rate down", "Analyse all studies"))
  llow <- fc_box("pma-rob-leaf-lowonly", "leaf", 725, 474, 215,
                 c("Do not rate down", "Analyse low risk studies"))

  e$anyhigh_no <- fc_edge("pma-rob-edge-anyhigh-no",
                          list(c(450, 67), c(505, 67)),
                          "No", 477, 60)
  e$anyhigh_yes <- fc_edge("pma-rob-edge-anyhigh-yes",
                           list(c(235, 87), c(235, 139)),
                           "Yes", 243, 116, "start")
  e$dom_yes <- fc_edge("pma-rob-edge-dominance-yes",
                       list(c(235, 212), c(235, 264)),
                       "Yes", 243, 241, "start")
  e$dom_no <- fc_edge("pma-rob-edge-dominance-no",
                      list(c(450, 175), c(722, 175), c(722, 264)),
                      "No", 460, 167, "start")
  e$dir_rules <- fc_edge("pma-rob-edge-direction-rules",
                         list(c(235, 337), c(235, 369)))
  e$appr_yes <- fc_edge("pma-rob-edge-appreciable-yes",
                        list(c(800, 320), c(800, 366)),
                        "Yes", 808, 346, "start")
  e$appr_no <- fc_edge("pma-rob-edge-appreciable-no",
                       list(c(560, 320), c(560, 474)),
                       "No", 568, 400, "start")
  e$mag_sim <- fc_edge("pma-rob-edge-magnitude-similar",
                       list(c(660, 422), c(660, 474)),
                       "Similar", 668, 452, "start")
  e$mag_dif <- fc_edge("pma-rob-edge-magnitude-different",
                       list(c(860, 422), c(860, 474)),
                       "Different", 868, 452, "start")

  b <- c(list(anyhigh, nohigh, dom, dirn), rules,
         list(appr, magn, lall, llow))

  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(b, `[[`, "markup"), use.names = FALSE),
    fc_caption(654, c(
      paste0("After BMJ Core GRADE 4 (Guyatt et al., 2025) Figure 2. ",
             "pmatools&#8217; operationalisation, not a reproduction:"),
      "the five direction-of-bias rules are pmatools&#8217; own."))
  )

  fc_svg(
    "rob", 684,
    "Risk of bias: the Core GRADE 4 Figure 2 decision as pmatools implements it",
    paste0("Flowchart. Any study at high risk of bias? If no, do not rate ",
           "down. If yes, do the high risk of bias studies dominate the ",
           "evidence (55% or more of the pooled weight by default)? If they ",
           "do, pmatools checks the direction of bias by comparing the ",
           "pooled estimate with and without those studies and applying five ",
           "mutually exclusive rules: both estimates trivial, or the same ",
           "zone with a change within 10%, do not rate down; the same zone ",
           "with a bias-favouring change over 10%, zones differing on the ",
           "same side of the null, zones differing across the null, or a ",
           "direction that cannot be assessed, rate down one level. If the ",
           "high risk of bias studies do not dominate, pmatools asks whether ",
           "there is appreciable evidence from the low risk of bias studies ",
           "and, if so, whether the two magnitudes of effect are similar or ",
           "substantially different; neither answer rates down, but a ",
           "substantial difference restricts the analysis to the low risk of ",
           "bias studies. This is pmatools&#8217; operationalisation of the ",
           "Core GRADE 4 Figure 2 decision, not a reproduction of the ",
           "published figure: the five rules are pmatools&#8217; and are not ",
           "enumerated in the source."),
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
  d1    <- fc_box("pma-incon-leaf-down1", "leaf", 560, 368, 380,
                  c("Rate down 1 level", "(some concerns)"))

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
    unlist(lapply(list(step1, nd1, step2, nd2, sc, step3, nd3, d1),
                  `[[`, "markup"), use.names = FALSE),
    fc_caption(446, c(
      paste0("After BMJ Core GRADE 3 (Guyatt et al., 2025) Figure 2. ",
             "pmatools&#8217; operationalisation, not a reproduction:"),
      paste0("Core GRADE 3 words these nodes qualitatively and quantifies ",
             "none of them; the numeric surrogates are pmatools&#8217; own.")))
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
           "the subgroups separately; no means rate down one level. This is ",
           "pmatools&#8217; operationalisation of the Core GRADE 3 Figure 2 ",
           "decision, not a reproduction of the published figure: Core GRADE ",
           "3 words these nodes qualitatively and quantifies none of them."),
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
                  c("Do not rate down",
                    "The optimal information size is never consulted here"))
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
    fc_edge("pma-impre-edge-large-no", list(c(490, 226), c(560, 226)),
            "No", 525, 219),
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
           "means do not rate down, and the optimal information size is ",
           "never consulted on that path. Yes leads to the optimal ",
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
  q1  <- fc_box("pma-pubias-node-q1", "node", 20, 48, 470,
                c("Q1  Are most or all studies small",
                  "and industry sponsored?"))
  l1  <- fc_box("pma-pubias-leaf-down1-q1", "leaf", 560, 48, 380,
                c("Rate down 1 level"))
  reg <- fc_box("pma-pubias-node-registry", "node", 20, 150, 470,
                c("pmatools input, not a node of Figure 5",
                  "Is registry coverage complete, so that every",
                  "registered trial can be accounted for?"),
                extra_class = "pma-fc-pmatools")
  lreg <- fc_box("pma-pubias-leaf-nodown-registry", "leaf", 560, 148, 380,
                 c("Do not rate down",
                   "On the reviewer&#8217;s assertion alone"))
  q2  <- fc_box("pma-pubias-node-q2", "node", 20, 275, 470,
                c("Q2  Is statistical analysis feasible?",
                  "(a meta-analysis of 10 or more studies)"))
  q3  <- fc_box("pma-pubias-node-q3", "node", 20, 383, 440,
                c("Q3  Does funnel plot asymmetry, visually",
                  "or by Egger&#8217;s test, strongly suggest",
                  "publication bias?"))
  q4  <- fc_box("pma-pubias-node-q4", "node", 500, 383, 440,
                c("Q4  Is there documentation of unpublished",
                  "studies (a trial registry, FDA records)?"))
  l3y <- fc_box("pma-pubias-leaf-down1-q3", "leaf", 20, 500, 140,
                c("Rate down", "1 level"))
  l3n <- fc_box("pma-pubias-leaf-nodown-q3", "leaf", 172, 500, 140,
                c("Do not", "rate down"))
  l3q <- fc_box("pma-pubias-leaf-qual-q3", "leaf", 324, 500, 136,
                c("Qualitative", "assessment", "required"))
  l4y <- fc_box("pma-pubias-leaf-down1-q4", "leaf", 500, 500, 140,
                c("Rate down", "1 level"))
  l4n <- fc_box("pma-pubias-leaf-nodown-q4", "leaf", 652, 500, 140,
                c("Do not", "rate down"))
  l4q <- fc_box("pma-pubias-leaf-qual-q4", "leaf", 804, 500, 136,
                c("Qualitative", "assessment", "required"))

  e <- list(
    fc_edge("pma-pubias-edge-q1-yes", list(c(490, 76), c(560, 76)),
            "Yes", 525, 69),
    fc_edge("pma-pubias-edge-q1-no", list(c(255, 104), c(255, 150)),
            "No", 263, 130, "start"),
    fc_edge("pma-pubias-edge-registry-yes", list(c(490, 176), c(560, 176)),
            "Yes", 525, 169),
    fc_edge("pma-pubias-edge-registry-no", list(c(255, 223), c(255, 275)),
            "No or not answered", 263, 252, "start"),
    fc_edge("pma-pubias-edge-q2-yes", list(c(200, 331), c(200, 383)),
            "Yes", 208, 360, "start"),
    fc_edge("pma-pubias-edge-q2-no",
            list(c(490, 303), c(720, 303), c(720, 383)),
            "No", 500, 295, "start"),
    fc_edge("pma-pubias-edge-q3-yes", list(c(90, 456), c(90, 500)),
            "Yes", 90, 492),
    fc_edge("pma-pubias-edge-q3-no", list(c(242, 456), c(242, 500)),
            "No", 242, 492),
    fc_edge("pma-pubias-edge-q3-na", list(c(392, 456), c(392, 500)),
            "Test not run", 392, 492),
    fc_edge("pma-pubias-edge-q4-yes", list(c(570, 439), c(570, 500)),
            "Yes", 570, 492),
    fc_edge("pma-pubias-edge-q4-no", list(c(722, 439), c(722, 500)),
            "No", 722, 492),
    fc_edge("pma-pubias-edge-q4-na", list(c(872, 439), c(872, 500)),
            "Not answered", 872, 492)
  )

  body <- c(
    unlist(lapply(e, `[[`, "markup"), use.names = FALSE),
    unlist(lapply(list(q1, l1, reg, lreg, q2, q3, q4,
                       l3y, l3n, l3q, l4y, l4n, l4q),
                  `[[`, "markup"), use.names = FALSE),
    fc_caption(597, c(
      paste0("After BMJ Core GRADE 4 (Guyatt et al., 2025) Figure 5. ",
             "pmatools&#8217; operationalisation, not a reproduction:"),
      "the dashed registry node is a pmatools input and is not in Figure 5."))
  )

  fc_svg(
    "pubias", 626,
    "Publication bias: the Core GRADE 4 Figure 5 decision as pmatools implements it",
    paste0("Flowchart. Q1 asks whether most or all studies are small and ",
           "industry sponsored; yes means rate down one level. Otherwise a ",
           "pmatools input, drawn dashed because it is not a node of Figure ",
           "5, asks whether registry coverage is complete; yes means do not ",
           "rate down, on the reviewer&#8217;s assertion alone. Otherwise Q2 ",
           "asks whether statistical analysis is feasible, that is whether a ",
           "meta-analysis of ten or more studies was performed. If it is, Q3 ",
           "asks whether funnel plot asymmetry, visually or by Egger&#8217;s ",
           "test, strongly suggests publication bias: yes rates down one ",
           "level, no does not, and a test that could not be run leaves a ",
           "qualitative assessment required. If statistical analysis is not ",
           "feasible, Q4 asks whether unpublished studies are documented in ",
           "a trial registry or FDA records: yes rates down one level, no ",
           "does not, and no answer leaves a qualitative assessment ",
           "required. This is pmatools&#8217; operationalisation of the Core ",
           "GRADE 4 Figure 5 decision, not a reproduction of the published ",
           "figure."),
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
