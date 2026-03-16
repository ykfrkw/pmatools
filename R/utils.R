# utils.R — 共通ユーティリティ

GRADE_LEVELS <- c("no", "some", "serious", "very_serious")
GRADE_DOWNGRADE <- c(no = 0, some = -1, serious = -1, very_serious = -2)
CERTAINTY_LABELS <- c("Very Low", "Low", "Moderate", "High")
CERTAINTY_SYMBOLS <- c(
  "High"       = "\u2295\u2295\u2295\u2295",
  "Moderate"   = "\u2295\u2295\u2295\u25cb",
  "Low"        = "\u2295\u2295\u25cb\u25cb",
  "Very Low"   = "\u2295\u25cb\u25cb\u25cb"
)

# Certainty colour palettes (bg + text colour pairs)
# pastel: soft backgrounds, coloured text — readable on screen and in print
# classic: saturated backgrounds, white text — matches netmetaviz classic palette
CERTAINTY_PALETTES <- list(
  pastel = list(
    "High"     = list(bg = "#d7e8d3", text = "#238b21"),
    "Moderate" = list(bg = "#cccce9", text = "#01008b"),
    "Low"      = list(bg = "#f8edd7", text = "#daa521"),
    "Very Low" = list(bg = "#e8d0d0", text = "#8b0000")
  ),
  classic = list(
    "High"     = list(bg = "#1e8449", text = "#ffffff"),
    "Moderate" = list(bg = "#2471a3", text = "#ffffff"),
    "Low"      = list(bg = "#e67e22", text = "#ffffff"),
    "Very Low" = list(bg = "#c0392b", text = "#ffffff")
  )
)

# スコアを確実性ラベルに変換
score_to_certainty <- function(score) {
  score <- max(1L, min(4L, as.integer(round(score))))
  c(1L, 2L, 3L, 4L) |>
    (\(.) CERTAINTY_LABELS[. == score])()
}

# GRADE 判定の検証
validate_grade_level <- function(x, arg = "argument") {
  valid <- c("no", "some", "serious", "very_serious")
  bad <- setdiff(x, valid)
  if (length(bad) > 0) {
    rlang::abort(
      paste0(arg, " contains invalid GRADE level(s): ", paste(bad, collapse = ", "),
             ". Use one of: ", paste(valid, collapse = ", "))
    )
  }
  invisible(x)
}

# --------------------------------------------------------------------------
# Baseline risk helpers
# --------------------------------------------------------------------------

#' Resolve baseline risk to a single numeric probability (0–1)
#'
#' @param baseline_risk NULL, a numeric scalar, "simple", or "metaprop"
#' @param meta_obj meta object (used for auto-computation)
#' @param ois_p0 Fallback when baseline_risk is NULL
#' @return Numeric scalar [0,1] or NULL
.resolve_baseline_risk <- function(baseline_risk, meta_obj, ois_p0 = NULL) {
  # 1. Explicit numeric
  if (is.numeric(baseline_risk)) {
    if (baseline_risk < 0 || baseline_risk > 1)
      rlang::abort("baseline_risk must be between 0 and 1.")
    return(baseline_risk)
  }
  # 2. "simple" or "metaprop"
  if (is.character(baseline_risk) && baseline_risk %in% c("simple", "metaprop")) {
    return(.compute_control_risk(meta_obj, method = baseline_risk))
  }
  # 3. NULL → fallback to ois_p0, then simple auto-compute
  if (is.null(baseline_risk)) {
    if (!is.null(ois_p0) && is.numeric(ois_p0)) return(ois_p0)
    return(.compute_control_risk(meta_obj, method = "simple"))
  }
  NULL
}

#' Compute control-arm event rate from a metabin object
.compute_control_risk <- function(meta_obj, method = "simple") {
  ec <- meta_obj$event.c
  nc <- meta_obj$n.c
  if (is.null(ec) || is.null(nc) || length(nc) == 0 || sum(nc, na.rm = TRUE) == 0) {
    return(NULL)
  }
  ec <- ec[!is.na(ec) & !is.na(nc)]
  nc <- nc[!is.na(nc)]

  if (method == "simple") {
    return(sum(ec) / sum(nc))
  }

  if (method == "metaprop") {
    mp <- tryCatch(
      meta::metaprop(event = ec, n = nc,
                     method = "GLMM", sm = "PLOGIT",
                     method.tau = "ML"),
      error = function(e) NULL
    )
    if (!is.null(mp) && !is.na(mp$TE.random)) {
      return(stats::plogis(mp$TE.random))
    }
    warning("metaprop() failed; falling back to simple pooled proportion.")
    return(sum(ec) / sum(nc))
  }
  NULL
}

# 確実性ドメイン判定をサマリ tibble にまとめる
make_domain_row <- function(domain, judgment, auto, notes = NA_character_) {
  tibble::tibble(
    domain    = domain,
    judgment  = judgment,
    downgrade = GRADE_DOWNGRADE[[judgment]],
    auto      = auto,
    notes     = notes
  )
}
