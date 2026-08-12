# domain_facts.R - accessor for the structured facts behind a domain judgment

# The five Core GRADE domains, in the order grade_meta() assembles them. Used
# to reject a mistyped domain name with a message that lists the valid ones
# (three of which record facts today; the other two are valid names that
# simply have none).
.GRADE_DOMAIN_NAMES <- c("Risk of bias", "Indirectness", "Inconsistency",
                         "Imprecision", "Publication bias")

#' Structured facts behind the GRADE domain judgments
#'
#' @description
#' Returns the numbers a domain assessor recorded when it made its judgment:
#' how many studies were at high risk of bias and how much weight they carried,
#' the heterogeneity statistics and the zone tally behind the Inconsistency
#' verdict, the confidence interval and optimal information size behind the
#' Imprecision one. Each fact carries a stable machine key, a human label, a
#' pre-formatted display string, and the raw number when the fact is
#' scalar-numeric.
#'
#' Facts are a machine-readable \emph{companion}, not a replacement:
#' \code{$domain_assessments$notes} remains the authoritative prose record of
#' why a domain was rated the way it was, and everything a fact reports is also
#' stated there. Read the facts when you need to compute with the numbers or
#' branch on them; read \code{notes} when you need the reasoning.
#'
#' Risk of bias, Inconsistency and Imprecision record facts. Indirectness and
#' Publication bias do not yet, so they return \code{NULL} - a valid domain
#' name with nothing recorded, not an error.
#'
#' @param x A \code{pmatools} object from \code{\link{grade_meta}}.
#' @param domain Optional single GRADE domain name, exactly as it appears in
#'   \code{x$domain_assessments$domain}: \code{"Risk of bias"},
#'   \code{"Indirectness"}, \code{"Inconsistency"}, \code{"Imprecision"} or
#'   \code{"Publication bias"}. \code{NULL} (default) returns every domain.
#'
#' @return With \code{domain = NULL}, a named list of tibbles keyed by domain
#'   name, empty when no domain recorded anything. With \code{domain} supplied,
#'   that domain's tibble with columns \code{key} (character),
#'   \code{label} (character), \code{value} (character) and \code{numeric}
#'   (double, \code{NA} when the fact is not scalar-numeric), or \code{NULL}
#'   when the domain recorded nothing.
#'
#' @seealso \code{\link{grade_meta}}, \code{\link{sof_table}},
#'   \code{\link{evidence_profile}}.
#'
#' @examples
#' \dontrun{
#' g <- grade_meta(m, rob = rob_vector, threshold = 1.2,
#'                 threshold_scale = "ratio")
#' domain_facts(g)
#' domain_facts(g, "Imprecision")
#'
#' # The raw numbers are there to compute with.
#' f <- domain_facts(g, "Inconsistency")
#' f$numeric[f$key == "i2"]
#' }
#'
#' @export
domain_facts <- function(x, domain = NULL) {
  if (!inherits(x, "pmatools")) {
    rlang::abort("domain_facts: 'x' must be a pmatools object from grade_meta().")
  }
  all_facts <- x$domain_facts
  if (is.null(all_facts)) all_facts <- stats::setNames(list(), character(0))

  if (is.null(domain)) return(all_facts)

  if (!is.character(domain) || length(domain) != 1L || is.na(domain)) {
    rlang::abort(paste0(
      "domain_facts: 'domain' must be a single GRADE domain name, one of ",
      paste(shQuote(.GRADE_DOMAIN_NAMES), collapse = ", "), ", or NULL."
    ))
  }
  if (!domain %in% .GRADE_DOMAIN_NAMES) {
    rlang::abort(paste0(
      "domain_facts: ", shQuote(domain), " is not a GRADE domain name. ",
      "Valid names (matched exactly): ",
      paste(shQuote(.GRADE_DOMAIN_NAMES), collapse = ", "), "."
    ))
  }

  all_facts[[domain]]
}
