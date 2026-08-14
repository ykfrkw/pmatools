# flowcharts.R - documentation-only topic for the decision flowcharts.
#
# No code lives here. The figures are in inst/figures/ (canonical, and staged
# into the Shiny bundle) with byte-identical copies in man/figures/, which is
# the only directory Rd's \figure{} macro resolves against. Both are generated
# by data-raw/build_figures.R; tests/testthat/test-flowchart-nodes.R asserts
# they match.
#
# The \if{html}{} wrapper is not decoration. The PDF manual cannot render SVG,
# and converting to PNG at build time would mean a new rendering dependency,
# which the deploy lifeline in CLAUDE.md makes expensive; the \if{latex}{}
# branch therefore points the reader at the HTML help instead.

#' Core GRADE decision flowcharts implemented by pmatools
#'
#' @name grade_flowcharts
#'
#' @description
#' Four of the five Core GRADE domains reach their judgment by walking a
#' decision flowchart. This topic draws each of those four and names the
#' function that implements it, so a judgment can be traced from the picture
#' to the code without reading the code first.
#'
#' These are pmatools' \emph{operationalisation} of the decision each cited
#' figure describes, not reproductions of the published artwork, and they
#' deliberately differ from the source where pmatools had to make the decision
#' executable. Every difference is marked in the figure's caption and stated
#' again below. Where a figure and the source disagree, the source is right
#' about GRADE and the figure is right about pmatools.
#'
#' Each assessment also records the route it took through its figure, as a
#' fact keyed \code{"flow_path"} in \code{\link{domain_facts}}: a
#' space-separated list of the element ids the judgment traversed. The
#' companion Shiny app reads it and highlights the path.
#'
#' @section Risk of bias - Core GRADE 4 Fig 2:
#' Implemented by \code{assess_rob()} in \code{R/domain_rob.R}, which delegates
#' the flowchart itself to \code{.flowchart_rob()} in the same file; the
#' five-rule direction-of-bias check is \code{.assess_bias_direction()}, also
#' in \code{R/domain_rob.R}, and both branches of the figure read its verdict.
#'
#' Departs from the source three times.
#'
#' First, the five rules are pmatools' own. Core GRADE 4 Fig 2 has a single
#' node reading "check direction of bias" and does not enumerate how. The
#' dominance threshold shown (55% of the pooled weight) is the conservative one
#' of the two the figure's footnote offers.
#'
#' Second, and more consequentially, the fifth rule rates down \strong{two}
#' levels. Core GRADE 4 describes no two-level risk-of-bias downgrade at all:
#' every leaf of its Fig 2 reads "rate down" or "do not rate down", and the
#' only two-level move in the paper is rating \emph{up} observational evidence.
#' pmatools rates down two there because the rule is reached only when the
#' pooled estimate sits beyond the chosen threshold on one side of the null and
#' the estimate restricted to the low risk of bias studies sits beyond it on
#' the other: the direction of the effect is what the high risk of bias studies
#' produced, and moderate certainty would overstate the evidence. The
#' neighbouring fourth rule, which is what an ordinary shift in the estimate
#' reaches, still rates down one level, as do the third rule and the case where
#' every study is at high risk of bias and no restricted estimate exists to
#' compare. Every judgment on the two-level branch carries the departure in its
#' \code{notes}. The two levels also require a threshold to have been supplied;
#' without one the rule rates down one level, because the zones are then
#' separated by the null alone and a sign flip no longer implies either
#' estimate is appreciably away from it.
#'
#' Third, the undominated branch's node - "whether low and high risk of bias
#' studies suggest similar or substantially different magnitudes of effect" -
#' is worded symmetrically in the source and names no direction, whereas
#' pmatools answers it with those same directional rules: a rule that rates
#' down means substantially different, a rule that does not means similar. A
#' shift past the inflation threshold that does not run in the bias-favouring
#' direction implied by \code{small_values} is therefore read as "similar", and
#' the analysis is not restricted to the low risk of bias studies. The
#' symmetric reading was in force up to and including 0.5.0 and was worse
#' rather than merely different: under it one and the same pair of estimates is
#' "substantially different" on this branch and "not substantially different"
#' one node away on the dominated branch. Rule 5's depth does not cross with
#' its verdict - this branch rates down nothing, so every rating rule reaches
#' the same leaf. Every judgment that answers the node carries the departure in
#' its \code{notes}.
#'
#' The chart opens at the dominance question. A body of evidence with no
#' high-risk study is not drawn a node of its own, because it does not reach a
#' different decision: the dominance share is then 0, below the gate, and
#' "analyse all studies" is the answer when there is nothing to exclude. That
#' case therefore lights the undominated route like any other.
#'
#' \if{html}{\figure{rob.svg}{options: width="100\%" alt="pmatools risk-of-bias decision flowchart: whether the high risk of bias studies dominate the evidence, then either the five-rule direction-of-bias check or the appreciable-evidence and magnitude questions."}}
#' \if{latex}{Figure omitted from the PDF manual (SVG); see the HTML help.}
#'
#' @section Inconsistency - Core GRADE 3 Fig 2:
#' Implemented by \code{assess_inconsistency()} in
#' \code{R/domain_inconsistency.R}; the automated route through the same three
#' steps is \code{.auto_inconsistency()} in that file.
#'
#' Departs from the source twice.
#'
#' First, the numbers on the edges are pmatools'. Core GRADE 3 words Step 1 as
#' a visual inspection of the forest plot and Step 2 as "majority on one side"
#' versus "a substantial proportion on opposite sides", quantifying neither.
#' pmatools automates them as I-squared above 30% (the only figure Core GRADE 3
#' names, and it names it grudgingly), a largest-zone share of 80% (following
#' CINeMA), and 20% on each side (a pmatools convention). Supplying the manual
#' flowchart inputs after looking at \code{\link{plot_forest}} follows the
#' source instead.
#'
#' Second, and more consequentially, the opposite-sided leaf rates down
#' \strong{two} levels. Core GRADE 3 declines to describe a two-level
#' inconsistency downgrade at all, holding that a compelling reason to rate
#' down twice for inconsistency is "sufficiently unusual that it need not
#' concern users of Core GRADE". pmatools rates down two here because the
#' branch is reached only when a substantial share of point estimates sits
#' above the chosen threshold, a substantial share sits below it, and no
#' credible subgroup explains the split: the direction of the effect is
#' unresolved, and moderate certainty would overstate the evidence. The
#' neighbouring scattered leaf, which is what ordinary disagreement between
#' studies reaches, still rates down one level. Every judgment on the
#' two-level branch carries the departure in its \code{notes}.
#'
#' \if{html}{\figure{incon.svg}{options: width="100\%" alt="pmatools inconsistency decision flowchart: three sequential steps asking about differences in point estimates and confidence-interval overlap, then the position of the estimates relative to the chosen threshold, then whether a credible subgroup explains an opposite-sided difference."}}
#' \if{latex}{Figure omitted from the PDF manual (SVG); see the HTML help.}
#'
#' @section Imprecision - Core GRADE 2 Fig 4:
#' Implemented by \code{assess_imprecision()} in
#' \code{R/domain_imprecision.R}; the branch itself is
#' \code{.classify_imprecision()} in that file, and the optimal information
#' size it consults is computed by \code{.compute_ois_pct()}.
#'
#' Follows the source most closely of the four. The one pmatools number on the
#' figure is the continuous definition of an implausibly large effect (a
#' standardised effect of 0.8 or more, Cohen's convention): Core GRADE 2 gives
#' a definition for binary outcomes only.
#'
#' \if{html}{\figure{impre.svg}{options: width="100\%" alt="pmatools imprecision decision flowchart: whether the pooled confidence interval crosses the chosen threshold and then the second threshold, or, when it does not, whether the effect is implausibly large and how the total sample compares with the optimal information size."}}
#' \if{latex}{Figure omitted from the PDF manual (SVG); see the HTML help.}
#'
#' @section Publication bias - Core GRADE 4 Fig 5:
#' Implemented by \code{assess_pubias()} in \code{R/domain_pubias.R}; the two
#' branches out of Q2 are \code{.pubias_statistical()} (k at least 10) and
#' \code{.pubias_registry()} (k below 10) in the same file.
#'
#' Departs from the source by one whole node: the registry-coverage box is a
#' pmatools input and appears nowhere in Fig 5, whose only registry node is Q4.
#' The box says so on its own third line, having been drawn with a dashed
#' outline up to 0.5.0 -- a dash reads as "provisional" or "not reached yet" on
#' a chart whose job is to show which boxes an analysis went through. It is
#' evaluated after Q1, so a body of small industry-sponsored trials still rates
#' down even when the reviewer asserts complete coverage.
#' The p < 0.05 cut-off on Q3 is also pmatools': Fig 5 asks qualitatively
#' whether asymmetry "strongly suggests publication bias" and names no
#' threshold.
#'
#' The study-count node is computed rather than asked, and says that on its own
#' third line too. \code{flow_path} lights the node AND the edge out of it, so
#' a reader can see which branch the count chose.
#'
#' The chart prints no question numbers, although Fig 5 numbers its four nodes
#' Q1 to Q4: the registry box sits between Q1 and Q2, so numbering on the
#' drawing would describe neither the source nor the route. The \code{"Q1:"}
#' to \code{"Q4:"} prefixes in the domain \code{notes} are unchanged -- they
#' are the exported record, and no figure travels with them.
#'
#' \if{html}{\figure{pubias.svg}{options: width="100\%" alt="pmatools publication-bias decision flowchart: whether most studies are small and industry sponsored, a pmatools registry-coverage input, whether statistical analysis is feasible, and then either funnel-plot asymmetry or documentation of unpublished studies."}}
#' \if{latex}{Figure omitted from the PDF manual (SVG); see the HTML help.}
#'
#' @section Indirectness - deliberately not a flowchart:
#' Assessed by \code{assess_indirectness()} in
#' \code{R/domain_indirectness.R}, and there is no figure here because Core
#' GRADE 5 offers no flowchart to draw. Its Table 2 grades how likely each
#' PICO element is to justify rating down - Population low, Intervention
#' intermediate, Comparison substantial, Outcome high - which is a gradient,
#' not a branch. pmatools folds the four subdomain answers worst-case, which
#' does not reproduce that gradient; \code{\link{indirectness_table}} renders
#' the subdomain table that stands in for a figure here, and the disclosure
#' travels with it.
#'
#' @references
#' Guyatt G, Brignardello-Petersen R, Hultcrantz M, et al. Core GRADE 2:
#' choosing the target of certainty rating and assessing imprecision. BMJ.
#' 2025;389:e081904. \doi{10.1136/bmj-2024-081904}
#'
#' Guyatt G, Brignardello-Petersen R, Hultcrantz M, et al. Core GRADE 3:
#' rating certainty of evidence - assessing inconsistency. BMJ.
#' 2025;389:e081905. \doi{10.1136/bmj-2024-081905}
#'
#' Guyatt G, Brignardello-Petersen R, Hultcrantz M, et al. Core GRADE 4:
#' rating certainty of evidence - risk of bias and publication bias. BMJ.
#' 2025;389:e083864. \doi{10.1136/bmj-2024-083864}
#'
#' Guyatt G, Brignardello-Petersen R, Hultcrantz M, et al. Core GRADE 5:
#' rating certainty of evidence - indirectness. BMJ. 2025;389:e083865.
#' \doi{10.1136/bmj-2024-083865}
#'
#' @seealso \code{\link{grade_meta}}, \code{\link{domain_facts}},
#'   \code{\link{indirectness_table}}.
#'
#' @keywords internal
NULL
