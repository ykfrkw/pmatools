# domain_indirectness.R — 非直接性ドメイン処理
#
# BMJ 2025 Core GRADE 5: 非直接性は臨床的判断が必要なため手動入力。
# domain_rob.R と同じ入力パターンをサポート。

assess_indirectness <- function(indirectness, meta_obj) {
  k <- meta_obj$k

  # デフォルト: スカラ "no"
  if (is.null(indirectness)) indirectness <- "no"

  # 列名参照
  if (length(indirectness) == 1 && is.character(indirectness) &&
      !indirectness %in% GRADE_LEVELS) {
    col <- indirectness
    data <- meta_obj$data
    if (is.null(data) || !col %in% names(data)) {
      rlang::abort(paste0(
        "indirectness = '", col, "' is not a valid GRADE level and was not found ",
        "as a column in the meta object's data."
      ))
    }
    ind_vec <- as.character(data[[col]])
    return(.aggregate_indirectness(ind_vec))
  }

  # スカラ
  if (length(indirectness) == 1) {
    validate_grade_level(indirectness, "indirectness")
    return(make_domain_row(
      domain   = "Indirectness",
      judgment = indirectness,
      auto     = FALSE,
      notes    = "Overall judgment provided by user."
    ))
  }

  # ベクタ
  if (length(indirectness) == k) {
    validate_grade_level(indirectness, "indirectness")
    return(.aggregate_indirectness(indirectness))
  }

  rlang::abort(paste0(
    "indirectness must be a scalar GRADE level, a column name, ",
    "or a vector of length k (", k, "). Got length ", length(indirectness), "."
  ))
}

.aggregate_indirectness <- function(ind_vec) {
  validate_grade_level(ind_vec, "indirectness")
  ind_vec <- .normalize_grade_level(ind_vec)
  order_map <- c(no = 1, some_concerns = 2, serious = 3)
  worst <- names(which.max(order_map[ind_vec]))
  n <- length(ind_vec)
  tbl <- table(ind_vec)
  notes <- paste(
    paste0(names(tbl), ": n=", as.integer(tbl)), collapse = "; "
  )
  make_domain_row(
    domain   = "Indirectness",
    judgment = worst,
    auto     = FALSE,
    notes    = paste0("Aggregated from ", n, " studies (worst case). ", notes)
  )
}
