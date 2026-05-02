# combine_arms.R - Combine multiple rows with same (studlab, treat)
#
# Cochrane Handbook 6.5.2.10: when several arms in a single study should be
# treated as a single group (e.g. multiple dose groups merged), pool them
# before pairwise meta-analysis.
#
# Binary:    events / n summed.
# Continuous: weighted mean and pooled SD via iterative pairing.

.combine_arms <- function(df) {
  combo_key <- paste(df$studlab, df$treat, sep = "::")
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
