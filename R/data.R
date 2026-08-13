#' CBT-I for Depression Response: 18-trial synthetic dataset
#'
#' Per-arm aggregated dataset for 18 randomized trials of cognitive
#' behavioral therapy for insomnia (CBT-I) as adjunct treatment for major
#' depressive disorder (MDD) with comorbid insomnia, with depression
#' response (>= 50 percent reduction on a depression severity scale) as
#' the binary outcome.
#'
#' All study names, effect sizes, and sample sizes are synthetic: the
#' dataset reproduces the structure (not the values) of the source review.
#' Seventeen trials report the outcome and enter the pooled estimate; the
#' remaining trial has \code{d_r = NA} and serves as a missing-results
#' example for the publication-bias / ROB-ME workflow.
#'
#' Source: Furukawa Y, et al. J Affect Disord. 2024.
#'
#' Multi-arm CBT-I trials are aggregated to a single CBT-I row per study
#' by summing events and sample sizes across CBT-I arms.
#'
#' @format A data frame with 36 rows (18 studies x 2 arms) and 7 columns
#'   (the lean analysis subset; the full synthetic extraction with all
#'   variables is bundled as \code{inst/extdata/cbti_depression.csv}):
#' \describe{
#'   \item{study}{Study identifier (first author + year).}
#'   \item{arm}{Arm label within the study (mirrors \code{treatment}).}
#'   \item{year}{Publication year.}
#'   \item{treatment}{\code{"CBT-I"} or \code{"Control"}.}
#'   \item{n_randomized}{Number of participants randomized to this arm.}
#'   \item{d_r}{Number of depression responders (binary outcome).}
#'   \item{rob_d}{Cochrane RoB 2.0 judgment for the depression outcome:
#'     \code{"L"} low, \code{"S"} some concerns, \code{"H"} serious concerns.}
#' }
#'
#' @examples
#' head(cbti_depression)
#' table(cbti_depression$rob_d)
#'
#' @references
#' Furukawa Y, et al. J Affect Disord. 2024.
#'
#' @source Furukawa Y, et al. J Affect Disord. 2024.
"cbti_depression"
