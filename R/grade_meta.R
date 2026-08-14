# grade_meta.R — メイン GRADE 評価関数
#
# BMJ 2025 Core GRADE シリーズに準拠した確実性評価を {meta} オブジェクトから実施する。

#' Assess certainty of evidence (Core GRADE series) from a meta-analysis object
#'
#' @description
#' Run the BMJ 2025 Core GRADE assessment on a meta-analysis object and
#' return per-domain judgments and final certainty.
#'
#' @param meta_obj A meta-analysis object from the \{meta\} package
#'   (\code{metabin}, \code{metacont}, \code{metagen}, etc.).
#' @param study_design Starting certainty. \code{"RCT"} (default, starts at High)
#'   or \code{"obs"} (observational, starts at Low).
#' @param rob Risk of bias judgment. One of:
#'   \itemize{
#'     \item A scalar GRADE level (see \code{grade_meta}'s "Domain judgment
#'       levels" section) — used as-is (flowchart not applied).
#'     \item A character vector of length k (one per study) — BMJ 2025 Core GRADE 4
#'       Fig 2 flowchart applied: domination check then direction-of-bias check.
#'     \item A column name in \code{meta_obj$data} containing per-study RoB judgments
#'       — same flowchart logic applied.
#'     \item \code{NULL} (default): treated as \code{"not_serious"}.
#'   }
#'   \strong{Breaking change (v0.4.0)}: passing a scalar GRADE level bypasses
#'   the automated flowchart and therefore requires \code{rob_rationale}.
#' @param rob_rationale Free-text justification, required whenever \code{rob}
#'   is supplied as a scalar GRADE level (manual override of the automated
#'   Risk of Bias flowchart). Recorded in the domain notes as
#'   \code{"Manual override (<judgment>): <rationale>"} and propagated to
#'   \code{\link{evidence_profile}}, \code{\link{grade_report}} and
#'   \code{\link{export_bundle}} outputs (Core GRADE transparency principle).
#'   Not used for per-study vectors or column-name input (automated
#'   assessment). Default \code{NULL}.
#' @param rob_some_concerns (v0.5) How studies rated \code{"some concerns"}
#'   are folded into the binary low/high classification that Core GRADE 4
#'   Fig 2 operates on. \code{"low"} (default, previous behaviour) or
#'   \code{"high"}. Changing it changes which studies count toward the
#'   high-RoB weight share and therefore the dominance gate. Only used when
#'   \code{rob} is a vector or column name.
#'
#'   \strong{Core GRADE 4 does not define this fold.} The phrase "some
#'   concerns" never appears in the article; it comes from three-level tools
#'   such as RoB 2. Core GRADE 4 asks only for a binary verdict per study —
#'   verbatim, "For simplicity, however, Core GRADE users can assess the
#'   overall risk of bias in individual studies as low or high" — and the
#'   examples it gives set the low/high boundary by \emph{counting high-risk
#'   items}, not by mapping an intermediate category: one review took a trial
#'   as high risk "if at least one item was rated as high risk of bias",
#'   another "required two or more of the seven items ... rated as high risk of
#'   bias", a third counted a study high "only if three or more of the eight
#'   items were assessed as high risk of bias". Core GRADE 4 then declines to
#'   settle the question: "The choice of threshold--high risk of bias in only
#'   one or more than one item or category--may be an issue that will be
#'   impossible to resolve". Whichever value you pass here is your own
#'   operational choice, and it is worth recording it alongside the item-count
#'   rule your review actually used.
#' @param rob_overrides (v0.5) Optional named character vector of study-level
#'   Risk-of-Bias overrides keyed on \code{meta_obj$studlab}, e.g.
#'   \code{c("Smith 2020" = "high")}. Values accept the same vocabulary as
#'   \code{rob}. Keys that match no study label abort (a typo must never be
#'   silently ignored). Every override is recorded in the domain notes as
#'   \code{"Study-level override: <studlab> <from> -> <to> (<rationale>)"}.
#' @param rob_override_rationale (v0.5) Named character vector of
#'   justifications, one per \code{rob_overrides} key. A missing rationale
#'   aborts (Core GRADE transparency principle). Default \code{NULL}.
#' @param rob_dominant_threshold (v0.5, reinstated) Weight share at or above
#'   which the body of evidence counts as \emph{dominated} by high-RoB studies
#'   in BMJ Core GRADE 4 Fig 2 (first decision node). The footnote to that
#'   figure offers two candidate values — "\code{>65\%} weight or
#'   \code{>=55\%} weight = possibly dominating" — and pmatools defaults to
#'   the conservative one, \code{0.55}, with a \code{>=} comparison so that
#'   exactly 55 percent counts as dominated. Pass \code{0.65} for the stricter
#'   reading.
#'   The share is computed from the inverse-variance study weights; when those
#'   are unavailable the count share is used instead and the domain note says
#'   so. When neither is computable, dominance is assumed (conservative).
#'   \strong{This argument was deprecated and ignored in v0.3.1-v0.4.0}; the
#'   retirement is retracted because the gate is the entry node of Fig 2.
#'   Only used when \code{rob} is a vector or column name.
#' @param rob_refit (v0.5) Logical, default \code{TRUE}. When the flowchart
#'   reaches the "use low risk of bias studies only" leaf (not dominated, but
#'   a substantial difference between the high- and low-RoB estimates), refit
#'   the meta-analysis on the low-RoB subset so that every downstream domain,
#'   the rating target, the baseline risk and the SoF table use the restricted
#'   estimate. Set to \code{FALSE} to keep the full analysis and receive the
#'   recommendation only (\code{$rob_analysis_set} is still
#'   \code{"low_only"}). A refit is skipped, with a warning, when fewer than
#'   two low-RoB studies remain or when \code{update()} fails.
#' @param rob_inflation_threshold (v0.2) Threshold for the relative change of
#'   the pooled estimate when high-RoB studies are excluded, computed on the
#'   absolute analysis scale: \eqn{(|TE_{all}| - |TE_{low}|) / |TE_{low}|},
#'   where \eqn{TE_{low}} is the inverse-variance weighted mean of the
#'   low/some-concerns RoB studies. Default
#'   \code{PMA_ROB_INFLATION_THRESHOLD} (\code{0.20}, 20 percent).
#'   \strong{\eqn{TE_{low}} is always a fixed-effect estimate}
#'   (\eqn{\sum w \cdot TE / \sum w} with \eqn{w = 1/se^2}), even when the
#'   parent meta-analysis is a random-effects model. Part of any observed shift
#'   can therefore come from the estimator difference rather than from risk of
#'   bias, and the gap widens with heterogeneity. Core GRADE 4 does not specify
#'   how to recompute the restricted estimate; \code{rob_refit = TRUE} refits
#'   the model on the low-RoB subset and keeps both estimates on the same
#'   footing.
#'   A downgrade under this criterion requires BOTH (a) the relative change to
#'   be *strictly* greater than the threshold (\code{>}; a change exactly at
#'   the threshold does not rate down) AND (b) the shift to be in the
#'   bias-favouring direction per \code{small_values}: only shifts that would
#'   make the apparent effect look more favourable (over-estimation) count.
#'   Shifts toward a smaller or less favourable effect never rate down under
#'   this criterion, even when their magnitude exceeds the threshold; in that
#'   case the domain note states explicitly why no downgrade was applied.
#'   When every study is high-RoB, no low/some-RoB comparator pool exists and
#'   the domain is rated \code{"very_serious"} (rate down 2 levels) unconditionally.
#'   Set to \code{0} to restore v0.1.0 behavior (any bias-favouring change
#'   rates down). Only used when \code{rob} is a vector or column name.
#' @param small_values \strong{Required} (v0.5.1, breaking). Are small outcome
#'   values desirable? \code{"desirable"} if small values are good (eg,
#'   mortality, symptom severity) or \code{"undesirable"} if small values are
#'   bad (eg, response rate, remission OR > 1). Anything else, including the
#'   \code{NULL} that used to be the default, aborts before any domain is
#'   assessed, with condition class \code{"pmatools_direction_gate"}.
#'   (Consistent with \code{netmetaviz} \code{small_values} parameter.)
#'
#'   Two domains consume it. Risk of bias uses it to decide which shift of the
#'   pooled estimate would flatter the intervention (Core GRADE 4 Fig 2, "check
#'   direction of bias"); Imprecision uses it to decide which side of the
#'   control-group risk the Optimal Information Size is powered against (Core
#'   GRADE 2's "modest relative risk reduction" is written for an undesirable
#'   event and does not generalise on its own).
#'
#'   \strong{There is no escape hatch}, unlike \code{require_threshold}. Rating
#'   without a MID is a legitimate methodological choice; rating without a
#'   direction is not, because every outcome has one. Up to v0.5.0 the argument
#'   was optional and both domains guessed — risk of bias fell back to
#'   \eqn{|TE_{all}| > |TE_{low}|} and then warned that the assumption had
#'   determined the downgrade, and the OIS used the paper's \emph{reduction} as
#'   written even for outcomes whose events are the desirable thing.
#' @param indirectness Indirectness judgment. Same format as \code{rob} (scalar/vector/column).
#'   Default \code{NULL}, which is treated as \code{"not_serious"} (no
#'   downgrade). Pass \code{NULL} — rather than \code{"not_serious"} — whenever no manual judgment is
#'   intended, so that programmatic callers (\code{do.call()}, Shiny UIs) that
#'   always supply every argument are not mistaken for manual overrides of an
#'   \code{indirectness_subdomains} table.
#'   \strong{Breaking change (v0.4.0)}: a scalar value other than
#'   \code{"not_serious"} is a manual override and requires
#'   \code{indirectness_rationale}. \code{"not_serious"} (no downgrade) never requires
#'   a rationale, so default calls are unaffected.
#' @param indirectness_dominant_threshold (v0.5) Weight share at or above
#'   which per-study indirectness dominates the body of evidence. Only used
#'   when \code{indirectness} is a per-study vector or a column name (the
#'   \code{indirectness_subdomains} table keeps its worst-case fold). Studies
#'   rated \code{"very_serious"} are pooled first: if their share reaches the
#'   threshold the domain is \code{"very_serious"}; otherwise, if the combined
#'   share of \code{"serious"} and \code{"very_serious"} studies reaches it,
#'   the domain is \code{"serious"}; otherwise \code{"not_serious"}. Default
#'   \code{0.55}.
#'   \strong{The threshold has no basis in Core GRADE 5}, which operationalizes
#'   indirectness of the body of evidence only qualitatively ("all or almost
#'   all evidence comes from ..."); \code{0.55} is a pmatools convention
#'   aligned with \code{rob_dominant_threshold}, and every aggregated domain
#'   note says so. Shares come from the inverse-variance study weights; when
#'   those are unavailable the count share is used and the note says so.
#'   \strong{Behaviour change (v0.5)}: per-study vectors were previously
#'   folded worst-case, so a single indirect study out of many rated the whole
#'   body of evidence down.
#' @param indirectness_rationale Free-text justification, required whenever
#'   \code{indirectness} is supplied as a scalar GRADE level other than
#'   \code{"not_serious"}. See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
#' @param indirectness_subdomains (v0.5) Optional per-PICO subdomain judgment
#'   table.
#'   \strong{Attribution}: asking the indirectness question separately for each
#'   PICO element is Core GRADE 5's; the table, the 4-point answer scale
#'   (\code{"yes"} / \code{"probably_yes"} / \code{"probably_no"} /
#'   \code{"no"}) and the wording "Is the evidence sufficiently direct?" are
#'   \strong{pmatools conventions} and do not appear in the Core GRADE 5
#'   article body. Core GRADE 5 instead asks how likely it is that the effect
#'   differs substantially from the target PICO, and grades that likelihood per
#'   element in its Table 2 ("Likelihood of rating down"): Population = "Low
#'   likelihood", Intervention = "Intermediate", Comparison = "Substantial",
#'   Outcome = "High likelihood". The worst-case fold below is symmetric and
#'   does not reproduce that gradient.
#'   A data.frame / tibble (or an equivalent list) with:
#'   \describe{
#'     \item{subdomain}{Required. Subdomain label, typically
#'       \code{"Population"}, \code{"Intervention"}, \code{"Comparison"},
#'       \code{"Outcome"}. Any order, subset, or extra subdomain is accepted;
#'       duplicate labels abort.}
#'     \item{target}{Optional. The target question for that subdomain (display
#'       only).}
#'     \item{evidence}{Optional. The evidence found (display only).}
#'     \item{judgment}{Required. One of \code{"yes"}, \code{"probably_yes"},
#'       \code{"probably_no"}, \code{"no"} — the answer to "Is the evidence
#'       sufficiently direct?". Case and separator variants
#'       (\code{"Probably No"}, \code{"probably-no"}) are accepted.}
#'   }
#'   \code{yes} / \code{probably_yes} contribute \code{"not_serious"},
#'   \code{probably_no} contributes \code{"serious"} and \code{no}
#'   contributes \code{"very_serious"}; the domain judgment defaults to the worst
#'   case across subdomains. Supplying \code{indirectness} as a non-\code{NULL}
#'   scalar alongside overrides that default and then requires
#'   \code{indirectness_rationale} (a restatement of the default value needs
#'   none); leave \code{indirectness} at its \code{NULL} default to accept the
#'   worst case. Cannot be combined with per-study vector or column-name
#'   \code{indirectness} input. The normalised table is returned as
#'   \code{indirectness_subdomains} on the result object and rendered by
#'   \code{\link{indirectness_table}}. Default \code{NULL}.
#' @param inconsistency Overall inconsistency scalar judgment: a GRADE level
#'   (see the \strong{Domain judgment levels} section).
#'   If provided, flowchart parameters are ignored.
#'   \strong{Breaking change (v0.4.0)}: this scalar override requires
#'   \code{inconsistency_rationale}. The manual flowchart inputs
#'   (\code{inconsistency_ci_diff} etc.) do not.
#' @param inconsistency_rationale Free-text justification, required whenever
#'   \code{inconsistency} is supplied (manual override of the flowchart /
#'   automated assessment). See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
#' @param inconsistency_ci_diff \code{"yes"} / \code{"no"}: Are there important
#'   differences in point estimates AND limited CI overlap? (BMJ Core GRADE 3 Fig 2
#'   Step 1). Required for flowchart; if NULL, falls back to I^2-based assessment.
#' @param inconsistency_threshold_side Required when \code{inconsistency_ci_diff = "yes"}.
#'   \code{"majority_one_side"} or \code{"opposite_sides"}: are most estimates on one
#'   side of the clinical threshold, or spread across both sides?
#' @param inconsistency_subgroup_explained Required when
#'   \code{inconsistency_threshold_side = "opposite_sides"}.
#'   \code{"yes"} / \code{"no"}: is the inconsistency explained by a credible
#'   subgroup?
#'
#'   Since 0.5.1 this argument is also read on the \emph{automated} path, on
#'   its own: when \code{inconsistency_ci_diff} is \code{NULL} and the
#'   automated zone tally lands on opposite sides of the threshold,
#'   \code{"yes"} rates the domain \code{"not_serious"} (present the subgroups
#'   separately) and \code{"no"} keeps \code{"very_serious"}. Leaving it
#'   unanswered also keeps \code{"serious"}, which is the conservative
#'   default. Before 0.5.1 the automated note advised supplying it while the
#'   automated path ignored it.
#'
#'   \strong{How to decide.} pmatools cannot assess credibility; Core GRADE 3
#'   points at a formal instrument and three key criteria. Verbatim (Core
#'   GRADE 3 summary points): "Key criteria for determining the credibility of
#'   a subgroup analysis include the P value associated with a test of
#'   interaction, consistency with a priori hypotheses that include direction
#'   of effect, and whether the subgroup effect is based on within study
#'   comparisons". The instrument is ICEMAN
#'   (\url{https://www.iceman.help}; Schandelmaier S, Briel M, Varadhan R,
#'   et al. CMAJ 2020;192:E901-6. doi:10.1503/cmaj.200077).
#'
#'   \strong{This implementation is more permissive than Core GRADE 3.}
#'   Passing \code{"yes"} lets the pooled estimate through without rating down.
#'   Core GRADE 3 instead says (verbatim) that "a conclusion of moderate or
#'   high credibility warrants the creation of separate PICO questions for each
#'   subgroup, separate presentation of results for each subgroup, separate
#'   ratings of certainty considering all five domains of rating down, and
#'   separate conclusions in keeping with each estimate of effect". So
#'   \code{"yes"} should be reserved for credibility that is at least moderate,
#'   and the faithful next step is to split the analysis and rate each subgroup
#'   separately (see \code{\link{grade_meta_multi}}), not to keep reporting the
#'   pooled estimate.
#' @param threshold_type (v0.5) Which threshold the certainty rating is
#'   anchored to (Core GRADE 2 Fig 2, step 1). One of:
#'   \itemize{
#'     \item \code{"mid"} (default): "Are you interested in whether there is
#'       an important effect or not?" — requires a minimal important
#'       difference (\code{threshold}).
#'     \item \code{"null"}: "Are you interested in whether there is a true
#'       underlying effect, benefit or harm?" — the null is the threshold and
#'       a MID is optional.
#'   }
#'   With \code{threshold_type = "mid"} and no \code{threshold}, the call
#'   aborts unless \code{require_threshold = FALSE}.
#'
#'   \strong{Guideline use forces \code{"mid"}.} Core GRADE 7, verbatim: "To
#'   inform recommendations, that threshold must be the MID (the smallest
#'   difference in effect that patients would consider important) rather than
#'   the null. Thus, decisions on the MID must precede certainty ratings in the
#'   evidence synthesis." A rating produced with \code{threshold_type = "null"}
#'   is therefore not a suitable input to a recommendation, however defensible
#'   it is as a systematic review result.
#' @param require_threshold (v0.5) Gate for the above. \code{TRUE} (default)
#'   makes a MID mandatory when \code{threshold_type = "mid"}. Set to
#'   \code{FALSE} to run without a MID (backward-compatible escape hatch); the
#'   rating target then falls back to a non-null effect and imprecision is
#'   judged against the null.
#' @param rating_target (v0.5) Target of the certainty rating (Core GRADE 2
#'   Fig 2). \code{NULL} (default) derives it automatically from the pooled
#'   point estimate:
#'   \tabular{llll}{
#'     \strong{threshold_type} \tab \strong{point estimate} \tab
#'       \strong{target} \tab \strong{imprecision threshold} \cr
#'     \code{"mid"}  \tab \eqn{|TE| >} MID    \tab \code{"important_effect"}        \tab \eqn{\pm}MID \cr
#'     \code{"mid"}  \tab \eqn{|TE| \le} MID  \tab \code{"little_to_no_difference"} \tab \eqn{\pm}MID \cr
#'     \code{"null"} \tab very near null      \tab \code{"little_to_no_difference"} \tab \eqn{\pm}MID \cr
#'     \code{"null"} \tab not near null       \tab \code{"non_null_effect"}         \tab null (0) \cr
#'   }
#'   "Very near null" is operationalized as \eqn{|TE| \le} MID; with no MID
#'   supplied, nearness cannot be judged and the target falls back to
#'   \code{"non_null_effect"} (recorded in \code{rating_target_note}).
#'   Supplying a value overrides the automatic derivation and requires
#'   \code{rating_target_rationale}.
#' @param rating_target_rationale Free-text justification, required whenever
#'   \code{rating_target} is supplied (manual override of the Core GRADE 2
#'   Fig 2 derivation). See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
#' @param threshold (v0.2) Numeric clinical decision Threshold (a minimally
#'   important effect). This is a cross-cutting parameter shared by the three
#'   Threshold-aware domains — it is not a Risk-of-Bias-specific setting:
#'   \itemize{
#'     \item Inconsistency: Step 2 3-zone classification of study point
#'       estimates around \eqn{\pm}Threshold.
#'     \item Imprecision: CI-vs-Threshold check (Zeng et al. BMJ 2025) and
#'       the OIS calculation when explicit OIS values are not supplied.
#'     \item Risk of bias: sensitivity comparison of the pooled estimate with
#'       vs without high-RoB studies, zone-classified around
#'       \eqn{\pm}Threshold.
#'   }
#'   The same Threshold is used across all three domains.
#' @param threshold_scale (v0.2) How to interpret \code{threshold}. One of:
#'   \itemize{
#'     \item \code{"auto"} (default): infer from \code{meta_obj$sm}
#'       (OR/RR/HR/RoM \eqn{\to} ratio scale; SMD/MD \eqn{\to} TE scale;
#'       ARD \eqn{\to} ARD scale).
#'     \item \code{"te_scale"}: \code{threshold} is already on the same
#'       scale as \code{meta_obj$TE} (log scale for ratios, raw for MD/SMD).
#'     \item \code{"ratio"}: \code{threshold} is supplied as a ratio (e.g.,
#'       \code{1.25} for a 25 percent relative effect); internally
#'       converted to \code{log(threshold)}.
#'     \item \code{"ard"}: \code{threshold} is an absolute risk difference
#'       (binary outcomes only; a proportion, e.g., \code{0.05} for 50 per
#'       1,000). When the effect measure is a ratio (OR/RR/HR; RoM
#'       approximated as RR), the ARD Threshold is converted to an
#'       equivalent ratio at the baseline risk (see
#'       \code{threshold_baseline}): RR uses
#'       \eqn{T = (p_0 + ARD) / p_0}, OR uses
#'       \eqn{T = odds(p_0 + ARD) / odds(p_0)}, and HR is approximated by
#'       the RR formula (accurate only for low event rates; interpret with
#'       care).
#'   }
#' @param threshold_baseline (v0.4) Baseline (control-arm) risk as a
#'   proportion in (0, 1) (e.g., \code{0.18} for 180 per 1,000), used to
#'   convert an absolute Threshold (\code{threshold_scale = "ard"}) to the
#'   ratio scale when the effect measure is OR/RR/HR/RoM. If \code{NULL}
#'   (default), it is inherited from \code{ois_p0} or \code{baseline_risk}
#'   when either was supplied (v0.5.1; see \strong{One control-arm risk});
#'   failing that, the pooled control event rate
#'   (\eqn{\sum event_c / \sum n_c}) of \code{meta_obj} is used, and if that is
#'   unavailable too, an informative error is raised. Ignored unless an ARD
#'   Threshold requires conversion.
#' @param imprecision Optional overall imprecision scalar judgment: a GRADE
#'   level (see the \strong{Domain judgment levels} section). If provided, the
#'   automated imprecision assessment (CI-vs-null/Threshold and OIS checks)
#'   is bypassed entirely and \code{imprecision_rationale} is required.
#'   Default \code{NULL} (automated assessment).
#' @param imprecision_rationale Free-text justification, required whenever
#'   \code{imprecision} is supplied (manual override of the automated
#'   assessment). See \code{rob_rationale} for how it is recorded.
#'   Default \code{NULL}.
#' @param outcome_name Optional label for the outcome (used in SoF table).
#' @param outcome_type \code{"relative"} (RR/OR/HR, null = 1) or
#'   \code{"absolute"} (MD/SMD, null = 0). Default \code{"relative"}.
#' @param ois_events For binary outcomes: target number of events for the
#'   Optimal Information Size (OIS). Takes precedence over auto-calculated OIS.
#' @param ois_n For continuous outcomes: target total sample size for OIS.
#'   Takes precedence over auto-calculated OIS.
#' @param ois_alpha Type I error rate for OIS calculation (default 0.05, two-sided).
#' @param ois_beta Type II error rate for OIS calculation (default 0.20, ie 80 percent power).
#' @param ois_p0 For binary outcomes: baseline (control) event rate for OIS
#'   calculation. Used with \code{ois_p1} to auto-compute target events. If
#'   \code{NULL} (default), it is inherited from \code{threshold_baseline} or
#'   \code{baseline_risk} when either was supplied (v0.5.1; see \strong{One
#'   control-arm risk}), and otherwise falls back to the pooled control event
#'   rate of the analysis being rated.
#' @param ois_p1 For binary outcomes: experimental arm event rate for OIS
#'   calculation. When supplied it takes precedence over \code{ois_rrr}.
#' @param ois_rrr (v0.5) For binary outcomes: the "modest relative risk
#'   reduction" the OIS is powered to detect, used to derive \code{ois_p1}
#'   from \code{ois_p0} as \eqn{p_1 = p_0 (1 \mp ois\_rrr)}. Default \code{0.20}.
#'   \strong{Which sign} depends on \code{small_values} and on the pooled
#'   effect (v0.5.1): the paper writes "reduction" because its worked example
#'   has an undesirable event, but when \code{small_values = "undesirable"} the
#'   events themselves are the desirable outcome and the intervention raises
#'   them, so the alternative rate is \eqn{p_0 (1 + ois\_rrr)}.
#'   Core GRADE 2 specifies exactly this input for binary outcomes: "For binary
#'   outcomes, these involve specifying the acceptable error rates: alpha
#'   (typically 0.05) and beta (typically 0.20), the control group event rate
#'   (chosen from the context), and a modest relative risk reduction, typically
#'   20 percent or 25 percent." Pass \code{0.25} for the other value the paper
#'   names. Ignored when \code{ois_p1}, \code{ois_events} or \code{ois_n} is
#'   supplied, and ignored for continuous outcomes, where the same paragraph
#'   directs users to the MID instead (\code{ois_delta}).
#'   \strong{Behaviour change (v0.5)}: \code{ois_p1} was previously derived
#'   from the MID for binary outcomes too.
#' @param ois_delta For continuous outcomes: minimally important difference for OIS
#'   calculation. Used with \code{ois_sd}.
#' @param ois_sd For continuous outcomes: pooled SD for OIS calculation. When
#'   left \code{NULL} it is derived from the contributing studies with
#'   \code{\link{compute_pooled_sd}()} (v0.5.1); before that the continuous OIS
#'   was simply unavailable unless the caller supplied one, and Fig 4's
#'   large-effect path fell through to "do not rate down" without saying why.
#'   The domain notes state when the value was derived rather than supplied.
#' @param baseline_risk Baseline (control-arm) event rate used for computing
#'   absolute risk differences (ARD per 1,000) in the SoF table. Accepts:
#'   \itemize{
#'     \item A numeric scalar in \code{[0, 1]}: used directly.
#'     \item \code{"simple"}: pooled control-arm proportion
#'       (\eqn{\sum events_c / \sum n_c}).
#'     \item \code{"metaprop"}: GLMM-pooled proportion via
#'       \code{meta::metaprop()} (logit back-transform); falls back to simple
#'       if convergence fails.
#'     \item \code{NULL} (default): inherits \code{threshold_baseline} or
#'       \code{ois_p0} when either was supplied (v0.5.1; see \strong{One
#'       control-arm risk}), otherwise auto-computes via \code{"simple"} for
#'       binary outcomes.
#'   }
#'   Only meaningful for binary outcomes with a relative effect measure
#'   (RR, OR, HR, IRR). Set to \code{NULL} explicitly to suppress ARD display.
#'
#' @param pubias_small_industry \code{"yes"} / \code{"no"}: Are most or all studies
#'   small AND industry-sponsored? (BMJ Core GRADE Fig 5 Step 1). Default \code{NULL}
#'   (treated as \code{"no"}).
#' @param pubias_funnel_asymmetry \code{"yes"} / \code{"no"}: Does visual funnel plot
#'   asymmetry and/or statistical test strongly suggest publication bias?
#'   Only used when k \eqn{\geq} 10. If \code{NULL} (default), Egger's test is
#'   run automatically.
#'   \strong{Breaking change (v0.4.0)}: supplying this argument replaces the
#'   automated Egger's test with a manual visual judgment and therefore
#'   requires \code{pubias_rationale}.
#' @param pubias_rationale Free-text justification, required whenever
#'   \code{pubias_funnel_asymmetry} is supplied (manual override of the
#'   automated Egger's test). The informational inputs
#'   \code{pubias_small_industry}, \code{pubias_unpublished} and
#'   \code{pubias_registry_complete} do not require a rationale. See
#'   \code{rob_rationale} for how it is recorded. Default \code{NULL}.
#' @param pubias_unpublished \code{"yes"} / \code{"no"}: Is there documentation of
#'   unpublished studies (eg, in trial registry or FDA)? Only used when k < 10.
#'   If \code{NULL} (default), assumed \code{"no"} with a warning.
#' @param pubias_registry_complete \code{"yes"} / \code{"no"}: Top-level structural
#'   rule-out for fields where pre-registration is universal and all registered
#'   trials can be accounted for. \code{"yes"} short-circuits the publication
#'   bias domain to "no" regardless of Egger's test or k. Default \code{NULL}.
#'
#' @section One control-arm risk:
#' \code{threshold_baseline}, \code{ois_p0} and \code{baseline_risk} are three
#' names for the control-arm event rate, consumed by three different
#' calculations: converting an absolute Threshold to the analysis scale,
#' powering the Optimal Information Size, and printing the absolute-risk
#' columns of the Summary of Findings table. Since v0.5.1 you supply the number
#' \strong{once}, to whichever of the three you think of first, and the other
#' two inherit it:
#'
#' \enumerate{
#'   \item An argument you supplied keeps its own value. Always. The three can
#'     legitimately differ -- a Summary of Findings table may be drawn against
#'     a named risk group while the OIS is powered from the trials' own control
#'     arms -- and an explicit value is never displaced by an inherited one.
#'   \item An argument left \code{NULL} takes the first value supplied to any
#'     of the others, in the order \code{threshold_baseline}, \code{ois_p0},
#'     \code{baseline_risk}.
#'   \item An argument still \code{NULL} after that falls back to the pooled
#'     control event rate of the analysis being rated, exactly as before.
#' }
#'
#' Only a number in (0, 1) is inherited. A character \code{baseline_risk}
#' (\code{"simple"} / \code{"metaprop"}) names a computation rather than a
#' value and does not donate; each use then computes its own pooled default.
#'
#' Which argument the value came from and which ones took it is recorded in
#' \code{$control_risk} and stated in the Imprecision domain notes, so a reader
#' of the Evidence Profile or the exported bundle can see it without reading
#' the call. Consolidating the three onto a single \code{baseline_risk} remains
#' the eventual destination; see SPEC.md section 4.5.4 for why it is not this
#' release.
#'
#' @section Domain judgment levels:
#' The vocabulary used by every domain argument and by
#' \code{$domain_assessments$judgment} is Core GRADE's own. Core GRADE 1,
#' verbatim: "We characterise limitations in each of these domains involved in
#' rating down certainty as not serious; serious; very serious; or, rarely,
#' extremely serious."
#'
#' \tabular{lll}{
#'   \strong{pmatools value} \tab \strong{Core GRADE wording} \tab \strong{levels down} \cr
#'   \code{"not_serious"}       \tab not serious       \tab  0 \cr
#'   \code{"serious"}           \tab serious           \tab -1 \cr
#'   \code{"very_serious"}      \tab very serious      \tab -2 \cr
#'   \code{"extremely_serious"} \tab extremely serious \tab -3 \cr
#' }
#'
#' \code{"extremely_serious"} is \strong{manual only}: no assessor in this
#' package produces it, because no Core GRADE flowchart describes a three-level
#' downgrade. Supply it through a scalar domain argument with the rationale
#' that argument already requires.
#'
#' \code{"no"} is a permanent alias for \code{"not_serious"}, and
#' \code{"some"} / \code{"some_concerns"} for \code{"serious"}; all three have
#' always meant what they mean now.
#'
#' \strong{\code{"serious"} on its own is refused in this release.} Up to 0.5.0
#' it was this package's internal name for the source's \emph{very serious}
#' (\eqn{-2}); from 0.5.1 it carries the source's meaning (\eqn{-1}). A script
#' passing it would keep running and report a different certainty, so pmatools
#' aborts and asks which was meant: write \code{"some_concerns"} for \eqn{-1}
#' or \code{"very_serious"} for \eqn{-2}. The refusal is temporary and will be
#' lifted once one release has passed. \code{$downgrade} always carries the
#' signed number, so read that when in doubt.
#'
#' @section Parts of Core GRADE not implemented:
#' \itemize{
#'   \item \strong{Rating up} non-randomised evidence. Core GRADE 1, verbatim:
#'     Core GRADE users "can rate up certainty in non-randomised studies (but
#'     not randomised controlled trials) for large effects and for evidence of
#'     a dose-response gradient". Certainty can only be rated down here; record
#'     an upgrade by hand with
#'     \code{evidence_profile(other_text =, other_downgrade =)}. Rating up for
#'     plausible confounding is correctly absent: Core GRADE 1 drops it
#'     explicitly, saying it "has proved too difficult to apply and too rarely
#'     applicable to be part of Core GRADE".
#'   \item \strong{Automatic "extremely serious" (\eqn{-3})}. The level exists
#'     and can be recorded by hand (see the vocabulary table above), but no
#'     assessor reaches it: Core GRADE's flowcharts describe no three-level
#'     downgrade, so the deepest any automated path goes is \eqn{-2}.
#'   \item \strong{The cross-domain gestalt step.} Core GRADE 1 asks for
#'     "stepping back and taking an overall view of the threats to certainty of
#'     evidence" after the individual domains, precisely so that several
#'     borderline domains do not add up to an unduly low rating. pmatools sums
#'     the per-domain downgrades arithmetically, so a result with two or three
#'     near-threshold domains can land lower than a Core GRADE panel would put
#'     it. Inspect \code{$domain_assessments$notes} and override the domains
#'     you judge borderline.
#' }
#'
#' @return An S3 object of class \code{pmatools} containing:
#'   \describe{
#'     \item{domain_assessments}{A tibble with one row per GRADE domain.}
#'     \item{domain_facts}{A named list of tibbles (\code{key}, \code{label},
#'       \code{value}, \code{numeric}), one per domain that recorded the
#'       numbers behind its judgment: currently \code{"Risk of bias"},
#'       \code{"Inconsistency"}, \code{"Imprecision"} and
#'       \code{"Publication bias"}. Each of those four also records a
#'       \code{flow_path} fact naming the decision nodes its judgment
#'       traversed in the corresponding figure of
#'       \code{\link{grade_flowcharts}}. A domain with nothing recorded is
#'       absent, and the list is empty when no domain recorded anything. The
#'       prose in \code{domain_assessments$notes} remains authoritative; this
#'       is its machine-readable companion. Read it with
#'       \code{\link{domain_facts}}.}
#'     \item{certainty}{Final certainty label: "High", "Moderate", "Low", or "Very Low".}
#'     \item{certainty_score}{Numeric score (1–4).}
#'     \item{starting_quality}{Starting certainty label.}
#'     \item{outcome_name}{Outcome label.}
#'     \item{threshold_type}{\code{"mid"} or \code{"null"} (Core GRADE 2 Fig 2).}
#'     \item{rating_target}{Target of the certainty rating.}
#'     \item{rating_target_note}{How the target was derived (or overridden).}
#'     \item{rating_target_auto}{\code{TRUE} when the target was derived
#'       automatically rather than supplied by the user.}
#'     \item{indirectness_subdomains}{The normalised Core GRADE 5 subdomain
#'       tibble (\code{subdomain}, \code{target}, \code{evidence},
#'       \code{judgment}, \code{grade_level}), or \code{NULL} when none was
#'       supplied. Render it with \code{\link{indirectness_table}}.}
#'     \item{meta}{The meta object every domain was assessed on: the refitted
#'       low-RoB analysis when one was performed, otherwise the original.}
#'     \item{meta_full}{The original (all-studies) meta object.}
#'     \item{rob_analysis_set}{\code{"all"} or \code{"low_only"} — the analysis
#'       set BMJ Core GRADE 4 Fig 2 recommends.}
#'     \item{rob_refit}{\code{TRUE} when the low-RoB refit was actually
#'       performed.}
#'     \item{control_risk}{How the one control-arm risk was shared across
#'       \code{threshold_baseline}, \code{ois_p0} and \code{baseline_risk}
#'       (see \strong{One control-arm risk}): \code{value} and \code{donor}
#'       name the number and the argument it came from, \code{inherited} the
#'       arguments that took it, \code{note} the sentence appended to the
#'       Imprecision domain notes, and \code{used} the number each of the
#'       three uses ended up with once its own pooled default had run
#'       (\code{NULL} for a use that did not need one -- a threshold that was
#'       never on the absolute scale, or a continuous outcome).}
#'   }
#'
#' @examples
#' \dontrun{
#' library(meta)
#' m <- metabin(Ee, Ne, Ec, Nc, studlab = study, data = Olkin1995, sm = "RR")
#' # threshold_type defaults to "mid", which requires a threshold (MID).
#' g <- grade_meta(m, study_design = "RCT", rob = "some",
#'                 rob_rationale = "RoB2 consensus: some concerns from missing outcome data",
#'                 threshold = 1.2, threshold_scale = "ratio",
#'                 outcome_name = "Mortality")
#' print(g)
#' print(g$rating_target)
#' sof_table(g)
#'
#' # Rating certainty in a true underlying effect instead (null threshold).
#' g_null <- grade_meta(m, threshold_type = "null", outcome_name = "Mortality")
#' }
#'
#' @seealso \code{\link{domain_facts}} for the structured numbers behind each
#'   domain judgment, and \code{\link{grade_flowcharts}} for the decision
#'   flowcharts those judgments follow, each naming the function that
#'   implements it.
#'
#' @export
grade_meta <- function(meta_obj,
                       study_design                     = c("RCT", "obs"),
                       rob                              = NULL,
                       rob_rationale                    = NULL,
                       rob_some_concerns                = c("low", "high"),
                       rob_overrides                    = NULL,
                       rob_override_rationale           = NULL,
                       rob_dominant_threshold           = 0.55,
                       rob_refit                        = TRUE,
                       rob_inflation_threshold          =
                         PMA_ROB_INFLATION_THRESHOLD,
                       # Required. The NULL default is kept only so that the
                       # omission is answered by .check_small_values()'s message
                       # rather than by R's "argument is missing" -- the same
                       # shape as `threshold`, whose gate works this way too.
                       small_values                     = NULL,
                       indirectness                     = NULL,
                       indirectness_dominant_threshold  = 0.55,
                       indirectness_rationale           = NULL,
                       indirectness_subdomains          = NULL,
                       inconsistency                    = NULL,
                       inconsistency_rationale          = NULL,
                       inconsistency_ci_diff            = NULL,
                       inconsistency_threshold_side     = NULL,
                       inconsistency_subgroup_explained = NULL,
                       imprecision                      = NULL,
                       imprecision_rationale            = NULL,
                       threshold_type                   = c("mid", "null"),
                       threshold                        = NULL,
                       threshold_scale                  = "auto",
                       threshold_baseline               = NULL,
                       rating_target                    = NULL,
                       rating_target_rationale          = NULL,
                       require_threshold                = TRUE,
                       outcome_name                     = NULL,
                       outcome_type                     = c("relative", "absolute"),
                       ois_events                       = NULL,
                       ois_n                            = NULL,
                       ois_alpha                        = 0.05,
                       ois_beta                         = 0.20,
                       ois_p0                           = NULL,
                       ois_p1                           = NULL,
                       ois_rrr                          = 0.20,
                       ois_delta                        = NULL,
                       ois_sd                           = NULL,
                       baseline_risk                    = NULL,
                       pubias_small_industry            = NULL,
                       pubias_funnel_asymmetry          = NULL,
                       pubias_unpublished               = NULL,
                       pubias_registry_complete         = NULL,
                       pubias_rationale                 = NULL) {

  # --- input check ---
  if (!inherits(meta_obj, "meta")) {
    rlang::abort("meta_obj must be an object of class 'meta' (from the {meta} package).")
  }
  study_design      <- match.arg(study_design)
  outcome_type      <- match.arg(outcome_type)
  threshold_type    <- match.arg(threshold_type)
  # Validated by .check_rob_some_concerns() rather than match.arg() so that a
  # bad value gets the message explaining what the setting does.
  rob_some_concerns <- .check_rob_some_concerns(
    if (length(rob_some_concerns) > 1L) rob_some_concerns[1] else rob_some_concerns
  )

  # --- Core GRADE 5 Indirectness subdomains (PICO) ---
  # With a subdomain table the domain judgment defaults to the worst case, and
  # `indirectness` becomes an optional manual override. "No judgment supplied"
  # is therefore encoded as NULL rather than detected with missing(): callers
  # that always pass every argument (do.call(), the Shiny UI) would otherwise
  # have their pass-through of the old "no" default read as an override.
  # assess_indirectness() maps NULL back to "no" when no subdomains are given,
  # so behaviour without a subdomain table is unchanged.
  indirectness_sub_tbl <-
    .normalize_indirectness_subdomains(indirectness_subdomains)

  # --- Core GRADE 2 Fig 2 step 1: the chosen threshold must be explicit ---
  # "mid" means importance is being judged, which is impossible without a MID.
  .check_threshold_type_gate(meta_obj, threshold_type, threshold,
                             require_threshold)

  # --- the outcome direction must be explicit (v0.5.1) ---
  # Risk of bias and Imprecision both consume it, and both used to guess in its
  # absence. Checked here, before any domain runs, so the call stops rather than
  # returning a rating that a guess helped produce.
  .check_small_values(small_values)

  # --- starting certainty ---
  start_score     <- if (study_design == "RCT") 4L else 2L
  starting_quality <- score_to_certainty(start_score)

  # --- one control-arm risk for the three arguments that name it ---
  # threshold_baseline, ois_p0 and baseline_risk are three names for the
  # control-arm event rate. Passing it once is enough; see
  # .resolve_control_risk() for the order and for why the three arguments still
  # exist separately.
  control_risk       <- .resolve_control_risk(threshold_baseline, ois_p0,
                                              baseline_risk)
  threshold_baseline <- control_risk$threshold_baseline
  ois_p0             <- control_risk$ois_p0
  baseline_risk      <- control_risk$baseline_risk

  # --- resolve Threshold to TE scale (used by RoB, Inconsistency, Imprecision) ---
  # `meta_full` is the all-studies analysis; `meta_obj` is rebound below to
  # the refitted low-RoB analysis when Core GRADE 4 Fig 2 calls for one.
  meta_full          <- meta_obj
  threshold_resolved <- threshold_to_te_scale(
    threshold, threshold_scale, meta_obj$sm,
    threshold_baseline = threshold_baseline,
    meta_obj           = meta_obj
  )
  threshold_internal <- threshold_resolved$threshold_internal
  threshold_kind     <- threshold_resolved$threshold_kind
  threshold_ard      <- threshold_resolved$threshold_ard
  threshold_note     <- threshold_resolved$threshold_note
  threshold_p0       <- threshold_resolved$threshold_baseline

  # --- Risk of bias first (Core GRADE 4 Fig 2) ---
  # RoB is assessed on the full analysis and may hand back the recommendation
  # to restrict the evidence to low-RoB studies. Everything downstream (rating
  # target, the other domains, baseline risk, SoF) then works from the
  # restricted analysis, so RoB has to run before them.
  d_rob   <- assess_rob(rob, meta_obj,
                        rob_some_concerns       = rob_some_concerns,
                        rob_overrides           = rob_overrides,
                        rob_override_rationale  = rob_override_rationale,
                        rob_dominant_threshold  = rob_dominant_threshold,
                        rob_inflation_threshold = rob_inflation_threshold,
                        small_values            = small_values,
                        threshold_internal      = threshold_internal,
                        rationale               = rob_rationale)
  rob_analysis_set <- attr(d_rob, "analysis_set") %||% "all"
  rob_high_idx     <- attr(d_rob, "high_idx")
  # Read immediately, for the same reason as the two attributes above: neither
  # .append_domain_note() nor dplyr::bind_rows() promises to carry attributes
  # of the row through.
  rob_facts        <- attr(d_rob, "facts")

  # The RoB domain note is written against the full analysis, so it keeps the
  # threshold note that was resolved above even if an ARD threshold is
  # re-resolved after the refit.
  if (!is.null(threshold_note)) {
    d_rob <- .append_domain_note(d_rob, threshold_note)
  }

  # --- act on the "use low risk of bias studies only" leaf ---
  refit_done <- FALSE
  if (identical(rob_analysis_set, "low_only") && isTRUE(rob_refit)) {
    refit_res  <- .refit_low_rob(meta_obj, rob_high_idx)
    meta_obj   <- refit_res$meta
    refit_done <- isTRUE(refit_res$refit)
    d_rob      <- .append_domain_note(d_rob, refit_res$note)

    # An absolute (ARD) threshold is anchored to the pooled control-arm risk,
    # which the restricted analysis changes; re-resolve it so the downstream
    # domains judge against the right equivalent ratio.
    if (refit_done && identical(threshold_kind, "ard")) {
      threshold_resolved <- threshold_to_te_scale(
        threshold, threshold_scale, meta_obj$sm,
        threshold_baseline = threshold_baseline,
        meta_obj           = meta_obj
      )
      threshold_internal <- threshold_resolved$threshold_internal
      threshold_kind     <- threshold_resolved$threshold_kind
      threshold_ard      <- threshold_resolved$threshold_ard
      threshold_note     <- threshold_resolved$threshold_note
      threshold_p0       <- threshold_resolved$threshold_baseline
      d_rob <- .append_domain_note(d_rob, paste0(
        "Absolute (ARD) threshold re-resolved on the restricted analysis ",
        "because the pooled baseline risk changed."
      ))
    }
  } else if (identical(rob_analysis_set, "low_only")) {
    d_rob <- .append_domain_note(d_rob, paste0(
      "rob_refit = FALSE: the recommendation to use low risk of bias studies ",
      "only is reported but not applied; the pooled estimate still includes ",
      "all studies."
    ))
  }

  # --- Core GRADE 2 Fig 2 steps 2-3: target of the certainty rating ---
  # Derived from the point estimate of the analysis actually being rated, so
  # this must sit after the refit and before the Imprecision domain (which
  # consumes target_info$threshold_for_imprecision).
  auto_target <- .derive_rating_target(
    te_point           = .pooled_te(meta_obj),
    threshold_internal = threshold_internal,
    threshold_type     = threshold_type,
    sm                 = meta_obj$sm,
    threshold_kind     = threshold_kind
  )
  target_info <- .resolve_rating_target(rating_target, rating_target_rationale,
                                        auto_target, threshold_internal)

  # --- remaining domain assessments (on the possibly refitted analysis) ---
  d_indir <- assess_indirectness(
    indirectness,
    meta_obj,
    rationale          = indirectness_rationale,
    subdomains         = indirectness_sub_tbl,
    dominant_threshold = indirectness_dominant_threshold
  )

  # Inconsistency evaluates point estimates "in relation to chosen threshold"
  # (Core GRADE 3 Fig 2), which is the same threshold the rating target
  # resolved for Imprecision -- +/-MID, or the null when the target is a
  # non-null effect. Passing the raw MID here instead (pre-v0.5) let the two
  # domains judge against different boundaries.
  d_incon <- assess_inconsistency(
    meta_obj,
    inconsistency                    = inconsistency,
    inconsistency_ci_diff            = inconsistency_ci_diff,
    inconsistency_threshold_side     = inconsistency_threshold_side,
    inconsistency_subgroup_explained = inconsistency_subgroup_explained,
    threshold_chosen                 = target_info$threshold_for_imprecision,
    rationale                        = inconsistency_rationale
  )
  incon_facts <- attr(d_incon, "facts")

  # Imprecision: scalar override bypasses the automated assessment entirely
  # (v0.4.0). Requires imprecision_rationale (Core GRADE transparency).
  d_impre <- if (!is.null(imprecision)) {
    if (!is.character(imprecision) || length(imprecision) != 1L) {
      rlang::abort(paste0(
        "imprecision must be a single GRADE level (",
        paste0("'", GRADE_LEVELS, "'", collapse = ", "), ") or NULL."
      ))
    }
    validate_grade_level(imprecision, "imprecision")
    .check_override_rationale(imprecision_rationale, "imprecision_rationale",
                              "Imprecision")
    make_domain_row(
      domain    = "Imprecision",
      judgment  = imprecision,
      auto      = FALSE,
      notes     = paste0("Overall judgment provided by user (scalar; ",
                         "automated assessment not applied)."),
      rationale = imprecision_rationale
    )
  } else {
    assess_imprecision(
      meta_obj,
      outcome_type       = outcome_type,
      ois_events         = ois_events,
      ois_n              = ois_n,
      ois_alpha          = ois_alpha,
      ois_beta           = ois_beta,
      ois_p0             = ois_p0,
      ois_p1             = ois_p1,
      ois_rrr            = ois_rrr,
      ois_delta          = ois_delta,
      ois_sd             = ois_sd,
      # Same outcome direction assess_rob() receives above. Imprecision needs
      # it because Core GRADE 2's "modest relative risk REDUCTION" assumes an
      # undesirable event; when the events are the desirable thing the OIS has
      # to be powered against p0 * (1 + rrr) instead.
      small_values       = small_values,
      threshold_internal = threshold_internal,
      threshold_kind     = threshold_kind,
      threshold_ard      = threshold_ard,
      threshold_p0       = threshold_p0,
      rating_target      = target_info$target,
      threshold_type     = threshold_type,
      threshold_for_imprecision = target_info$threshold_for_imprecision
    )
  }
  # The scalar-override branch above records no facts; the domain is then
  # simply absent from `domain_facts`.
  impre_facts <- attr(d_impre, "facts")
  # The control-arm rate the OIS was actually powered from, which is only known
  # after assess_imprecision() has applied its own pooled default. Read here
  # for the same reason as the RoB attributes above -- bind_rows() drops it --
  # and recorded so export_bundle() can pin it into the bundled analysis.R.
  ois_p0_used <- attr(d_impre, "ois_p0") %||% ois_p0

  # Record how the rating target was chosen in the Imprecision notes: the
  # target decides which threshold Fig 4 evaluates the CI against, so the two
  # must be auditable together (and the note then propagates to
  # evidence_profile / grade_report / export_bundle).
  d_impre$notes <- ifelse(is.na(d_impre$notes), target_info$note,
                          paste0(d_impre$notes, " | ", target_info$note))

  # Absolute-threshold conversion note: surface it in every Threshold-aware
  # domain so the baseline-risk assumption is auditable per domain. (Risk of
  # bias already carries the pre-refit note, appended above.)
  if (!is.null(threshold_note)) {
    d_incon <- .append_domain_note(d_incon, threshold_note)
    d_impre <- .append_domain_note(d_impre, threshold_note)
  }

  # A shared control-arm risk is only safe if the reader can see which of the
  # three arguments it came from. Imprecision carries the note because that is
  # where the number does the most work (it powers the OIS) and where the
  # neighbouring provenance already lives; from there it reaches summary(),
  # evidence_profile(), grade_report() and the bundle like any domain note.
  if (!is.null(control_risk$note)) {
    d_impre <- .append_domain_note(d_impre, control_risk$note)
  }

  d_pubias <- assess_pubias(
    meta_obj,
    pubias_small_industry    = pubias_small_industry,
    pubias_funnel_asymmetry  = pubias_funnel_asymmetry,
    pubias_unpublished       = pubias_unpublished,
    pubias_registry_complete = pubias_registry_complete,
    rationale                = pubias_rationale
  )
  # bind_rows() below drops attributes, so every domain's facts have to be
  # lifted off its row BEFORE the bind. Publication bias was the one assessor
  # that recorded nothing at all until v0.5.1; forgetting this line is what
  # would make it silently record nothing again.
  pubias_facts <- attr(d_pubias, "facts")

  domains <- dplyr::bind_rows(d_rob, d_indir, d_incon, d_impre, d_pubias)

  # Named by GRADE domain; a domain that recorded nothing is simply absent.
  domain_facts <- list()
  if (!is.null(rob_facts))    domain_facts[["Risk of bias"]]     <- rob_facts
  if (!is.null(incon_facts))  domain_facts[["Inconsistency"]]    <- incon_facts
  if (!is.null(impre_facts))  domain_facts[["Imprecision"]]      <- impre_facts
  if (!is.null(pubias_facts)) domain_facts[["Publication bias"]] <- pubias_facts

  # --- 確実性スコア計算 ---
  total_downgrade <- sum(domains$downgrade)
  final_score     <- max(1L, start_score + total_downgrade)
  certainty       <- score_to_certainty(final_score)

  # Resolved once: "metaprop" fits a GLMM, and the object records this number
  # in two places.
  baseline_risk_used <- .resolve_baseline_risk(baseline_risk, meta_obj, ois_p0)

  # --- output object ---
  structure(
    list(
      domain_assessments = domains,
      certainty          = certainty,
      certainty_score    = final_score,
      starting_quality   = starting_quality,
      study_design       = study_design,
      outcome_name       = if (is.null(outcome_name)) "Outcome" else outcome_name,
      outcome_type       = outcome_type,
      # The direction the rating was made under. Required since v0.5.1, so it
      # is always here -- which is what lets export_bundle() write it into the
      # bundled analysis.R by reading the object instead of falling back to a
      # NULL that reproduced a different analysis.
      small_values       = small_values,
      baseline_risk      = baseline_risk_used,
      # Provenance for the one number the three arguments share: which argument
      # supplied it, which ones inherited it, and the value each of the three
      # uses ended up with once its own pooled default had run. `used` is what
      # makes the bundled analysis.R reproduce this rating instead of
      # re-deriving a baseline of its own.
      control_risk       = list(
        value     = control_risk$value,
        donor     = control_risk$donor,
        inherited = control_risk$inherited,
        note      = control_risk$note,
        used      = list(
          threshold_baseline = threshold_p0,
          ois_p0             = ois_p0_used,
          baseline_risk      = baseline_risk_used
        )
      ),
      threshold_type     = threshold_type,
      rating_target      = target_info$target,
      rating_target_note = target_info$note,
      rating_target_auto = is.null(rating_target),
      threshold          = threshold,
      threshold_scale    = threshold_scale,
      threshold_internal = threshold_internal,
      threshold_ard      = threshold_ard,
      threshold_note     = threshold_note,
      threshold_baseline = threshold_p0,
      # Kept at the top level (not as a list-column of domain_assessments,
      # which must stay one row per domain with atomic columns).
      indirectness_subdomains = indirectness_sub_tbl,
      # Same reason: the structured facts behind the Risk of bias,
      # Inconsistency and Imprecision judgments are one tibble per domain, so
      # they cannot live in a tibble that must stay one row per domain with
      # atomic columns. Read them with domain_facts().
      domain_facts            = domain_facts,
      meta               = meta_obj,
      meta_full          = meta_full,
      rob_analysis_set   = rob_analysis_set,
      rob_refit          = refit_done
    ),
    class = "pmatools"
  )
}

# Append a sentence to a domain row's notes, preserving the " | " separator
# style (and the row's attributes, which assess_rob() uses to carry the
# analysis-set recommendation).
.append_domain_note <- function(d, note) {
  if (is.null(note) || !nzchar(note)) return(d)
  d$notes <- ifelse(is.na(d$notes), note, paste0(d$notes, " | ", note))
  d
}

#' @export
print.pmatools <- function(x, ...) {
  cat("\n-- Certainty Assessment (Core GRADE series) -------------\n")
  cat(sprintf(" Outcome      : %s\n", x$outcome_name))
  cat(sprintf(" Study design : %s  (starting quality: %s)\n",
              x$study_design, x$starting_quality))
  if (!is.null(x$rating_target)) {
    target_label <- unname(RATING_TARGET_LABELS[x$rating_target])
    if (is.na(target_label)) target_label <- x$rating_target
    cat(sprintf(" Rating target: %s  (threshold: %s%s)\n",
                target_label,
                x$threshold_type %||% "?",
                if (isTRUE(x$rating_target_auto)) ", auto" else ", manual"))
  }
  if (isTRUE(x$rob_refit)) {
    cat(sprintf(" Analysis set : low risk of bias studies only (%d of %d studies)\n",
                x$meta$k, x$meta_full$k))
  } else if (identical(x$rob_analysis_set, "low_only")) {
    cat(" Analysis set : all studies (Core GRADE 4 Fig 2 recommends low risk of bias studies only)\n")
  }
  cat("\n Domain assessments:\n")

  d <- x$domain_assessments
  for (i in seq_len(nrow(d))) {
    row <- d[i, ]
    dg  <- if (row$downgrade < 0) sprintf(" [%d]", row$downgrade) else "    "
    cat(sprintf("  %-20s %-14s %s\n",
                row$domain, row$judgment, dg))
  }

  cat(sprintf("\n Final certainty : %s  %s\n",
              x$certainty, CERTAINTY_SYMBOLS[[x$certainty]]))
  cat("----------------------------------------------------------\n\n")
  invisible(x)
}

#' @export
summary.pmatools <- function(object, ...) {
  print(object, ...)
  cat("Domain details:\n")
  d <- object$domain_assessments
  for (i in seq_len(nrow(d))) {
    row <- d[i, ]
    if (!is.na(row$notes)) {
      cat(sprintf("  [%s] %s\n", row$domain, row$notes))
    }
  }
  invisible(object)
}
