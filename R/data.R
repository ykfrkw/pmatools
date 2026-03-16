#' Synthetic CBT-I for Depression Response Dataset
#'
#' A synthetic dataset mimicking the structure of a systematic review
#' on cognitive behavioral therapy for insomnia (CBT-I) as adjunct treatment
#' for major depressive disorder (MDD) with comorbid insomnia.
#'
#' The data are entirely fictional (study names, effect sizes, sample sizes)
#' and are provided solely as a self-contained example for \code{pmatools}.
#' They are loosely patterned on the structure of:
#' Furukawa Y et al. J Affect Disord. 2024;367:359-366.
#'
#' @format A data frame with 34 rows and 7 columns:
#' \describe{
#'   \item{study}{Study identifier (first author + year).}
#'   \item{arm}{Arm label within the study.}
#'   \item{year}{Publication year.}
#'   \item{treatment}{\code{"CBT-I"} or \code{"Control"}.}
#'   \item{n_randomized}{Number of participants randomized to this arm.}
#'   \item{d_r}{Number of depression responders (binary outcome).}
#'   \item{rob_d}{Cochrane RoB 2.0 judgment for the depression outcome:
#'     \code{"L"} (low), \code{"S"} (some concerns), \code{"H"} (high risk).}
#' }
#'
#' @examples
#' head(cbti_depression)
#' table(cbti_depression$rob_d)
#'
#' @source Synthetic data generated for package examples.
"cbti_depression"
