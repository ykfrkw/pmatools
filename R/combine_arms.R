# combine_arms.R - Combine multiple rows with same study unit + treat
#
# Cochrane Handbook 6.5.2.10: when several arms in a single study should be
# treated as a single group (e.g. multiple dose groups merged), pool them
# before pairwise meta-analysis.
#
# Binary:    events / n summed.
# Continuous: weighted mean and pooled SD via iterative pairing.

#' Combine multiple arms of the same study into one row
#'
#' Collapses every group of rows that share a study unit -- \code{studlab},
#' \code{outcome} when that column is present, and \code{treat} -- into a single
#' row, following Cochrane Handbook 6.5.2.10. This is the step that turns a
#' multi-arm trial (two active dose groups against one control, say) into the
#' two-arm shape a pairwise meta-analysis needs, without counting the shared
#' arm twice or treating the dose groups as independent studies.
#'
#' \code{\link{ingest_data}} calls this on the way to canonical long format, so
#' a normal pmatools pipeline never has to. It is exported for callers that
#' assemble their own long data frame -- notably an interactive data editor that
#' wants to show the user what the merged rows will look like before the
#' analysis runs.
#'
#' Sample sizes and events are summed. Means and standard deviations are pooled
#' by the Handbook's iterative pairing formula, which preserves the
#' between-subgroup variance:
#' \deqn{N = n_1 + n_2}
#' \deqn{M = (n_1 m_1 + n_2 m_2) / N}
#' \deqn{SD^2 = \frac{(n_1-1)s_1^2 + (n_2-1)s_2^2 +
#'   \frac{n_1 n_2}{N}(m_1-m_2)^2}{N-1}}
#' Any other column (\code{rob}, \code{indirectness}, \code{subgroup},
#' user-supplied extras) is a per-study property, so the value of the first row
#' in the group is carried over.
#'
#' @param df A data frame in canonical long format: one row per study x arm,
#'   with columns \code{studlab}, \code{treat}, \code{n}, plus \code{event}
#'   (binary) or \code{mean} and \code{sd} (continuous), and optionally
#'   \code{outcome}. Rows sharing a study unit are the ones that get merged.
#'
#' @return A data frame with the same columns and one row per study unit. When
#'   no study unit is duplicated, \code{df} is returned unchanged.
#'
#' @references
#' Higgins JPT, Thomas J, Chandler J, et al (editors). Cochrane Handbook for
#' Systematic Reviews of Interventions, section 6.5.2.10.
#'
#' @seealso \code{\link{ingest_data}}, which applies this automatically.
#'
#' @examples
#' # A three-arm trial: two CBT-I doses against one shared control.
#' df <- data.frame(
#'   studlab = c("Trial 1", "Trial 1", "Trial 1", "Trial 2", "Trial 2"),
#'   treat   = c("experimental", "experimental", "control",
#'               "experimental", "control"),
#'   n       = c(30, 28, 60, 50, 50),
#'   event   = c(12, 10, 15, 20, 18),
#'   stringsAsFactors = FALSE
#' )
#' combine_arms(df)
#'
#' # Continuous outcomes pool the mean and SD instead of summing.
#' cont <- data.frame(
#'   studlab = c("Trial 1", "Trial 1", "Trial 1"),
#'   treat   = c("experimental", "experimental", "control"),
#'   n       = c(30, 28, 60),
#'   mean    = c(-5.2, -4.4, -1.1),
#'   sd      = c(6.0, 6.4, 5.8),
#'   stringsAsFactors = FALSE
#' )
#' combine_arms(cont)
#'
#' @export
combine_arms <- function(df) {
  key_cols <- c("studlab", if ("outcome" %in% names(df)) "outcome", "treat")
  combo_key <- do.call(paste, c(df[key_cols], sep = "::"))
  if (!any(duplicated(combo_key))) return(df)

  out_rows <- list()
  for (key in unique(combo_key)) {
    rows <- df[combo_key == key, , drop = FALSE]
    if (nrow(rows) == 1) {
      out_rows[[key]] <- rows
      next
    }

    combined <- data.frame(
      studlab = rows$studlab[1],
      treat   = rows$treat[1],
      n       = sum(rows$n, na.rm = FALSE),
      stringsAsFactors = FALSE
    )

    if ("event" %in% names(rows)) {
      combined$event <- sum(rows$event, na.rm = FALSE)
    }
    if (all(c("mean", "sd") %in% names(rows))) {
      cs <- .pool_continuous(rows$n, rows$mean, rows$sd)
      combined$mean <- cs$mean
      combined$sd   <- cs$sd
    }

    # Per-study cols (rob, indirectness, subgroup, plus any other extras)
    other_cols <- setdiff(names(rows), c("studlab", "treat", "n", "event", "mean", "sd"))
    for (col in other_cols) {
      combined[[col]] <- rows[[col]][1]
    }

    out_rows[[key]] <- combined
  }

  out <- do.call(rbind, out_rows)
  rownames(out) <- NULL
  out
}

# Internal alias kept so existing call sites (data_ingest.R) do not move.
.combine_arms <- function(df) combine_arms(df)

# Cochrane Handbook 6.5.2.10 - iteratively pool pairs:
#   N    = n1 + n2
#   M    = (n1*m1 + n2*m2) / N
#   SD^2 = ((n1-1)*s1^2 + (n2-1)*s2^2 + (n1*n2/N) * (m1-m2)^2) / (N - 1)
.pool_continuous <- function(n_vec, mean_vec, sd_vec) {
  ok <- !is.na(n_vec) & !is.na(mean_vec) & !is.na(sd_vec)
  n_vec    <- n_vec[ok]
  mean_vec <- mean_vec[ok]
  sd_vec   <- sd_vec[ok]
  if (length(n_vec) == 0) return(list(mean = NA_real_, sd = NA_real_))
  if (length(n_vec) == 1) return(list(mean = mean_vec[1], sd = sd_vec[1]))

  n1 <- n_vec[1]; m1 <- mean_vec[1]; s1 <- sd_vec[1]
  for (i in 2:length(n_vec)) {
    n2 <- n_vec[i]; m2 <- mean_vec[i]; s2 <- sd_vec[i]
    n_new   <- n1 + n2
    m_new   <- (n1 * m1 + n2 * m2) / n_new
    s_new_sq <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2 +
                 (n1 * n2 / n_new) * (m1 - m2)^2) / (n_new - 1)
    n1 <- n_new
    m1 <- m_new
    s1 <- sqrt(s_new_sq)
  }
  list(mean = m1, sd = s1)
}
