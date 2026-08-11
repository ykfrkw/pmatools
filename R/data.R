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
#'
#' Source: Furukawa Y, Nagaoka D, Sato S, et al. Cognitive behavioral
#' therapy for insomnia to treat major depressive disorder with comorbid
#' insomnia: A systematic review and meta-analysis. J Affect Disord.
#' 2024;367:359-366. doi:10.1016/j.jad.2024.09.017.
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
#' Furukawa Y, Nagaoka D, Sato S, et al. Cognitive behavioral therapy for
#' insomnia to treat major depressive disorder with comorbid insomnia: A
#' systematic review and meta-analysis. J Affect Disord. 2024;367:359-366.
#' \doi{10.1016/j.jad.2024.09.017}
#'
#' @source Furukawa Y et al. (2024) J Affect Disord. doi:10.1016/j.jad.2024.09.017
"cbti_depression"
