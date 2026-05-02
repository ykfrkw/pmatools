# data_ingest.R - Unified data ingestion for pmatools
#
# Accepts long or wide format from data.frame, CSV/Excel/TSV path, or pasted
# text. Normalises to canonical long format (one row per study x arm).
#
# Canonical long columns:
#   studlab, treat, n, [event], [mean], [sd], [rob], [indirectness], [subgroup]

#' Ingest study-level data into canonical long format
#'
#' @param data A data.frame, tibble, character path to a `.csv`/`.tsv`/`.xlsx`
#'   file, or a multi-line character string (clipboard paste).
#' @param format One of `"auto"` (default), `"long"`, or `"wide"`.
#' @param mapping Optional named list mapping canonical column names to user
#'   column names. Keys: `studlab`, `treat`, `n`, `event`, `mean`, `sd`, `rob`,
#'   `indirectness`, `subgroup`, plus wide-format pairs `n_e`/`n_c`,
#'   `event_e`/`event_c`, `mean_e`/`mean_c`, `sd_e`/`sd_c`.
#' @param experimental_label,control_label Optional. Values of `treat` to
#'   identify experimental and control arms in long format. If NULL, inferred
#'   from data.
#'
#' @return A tibble in canonical long format (one row per study x arm).
#'
#' @export
ingest_data <- function(data,
                        format             = c("auto", "long", "wide"),
                        mapping            = NULL,
                        experimental_label = NULL,
                        control_label      = NULL) {
  format <- match.arg(format)

  # 1. Resolve input source -> data.frame
  df <- .resolve_input(data)
  if (is.null(df) || nrow(df) == 0) {
    rlang::abort("ingest_data: input has no rows.")
  }

  # 2. Apply column-name mapping (if provided)
  if (!is.null(mapping)) {
    df <- .apply_mapping(df, mapping)
  }

  # 3. Detect format if "auto"
  if (format == "auto") {
    format <- .detect_format(df)
  }

  # 4. Normalise to canonical long format
  if (format == "wide") {
    long_df <- .wide_to_long(df)
  } else {
    long_df <- .normalise_long(df, experimental_label, control_label)
  }

  # 5. Combine duplicate (studlab, treat) rows (Cochrane Handbook 6.5.2.10)
  n_before <- nrow(long_df)
  long_df  <- .combine_arms(long_df)
  if (nrow(long_df) < n_before) {
    message(sprintf(
      "Combined %d duplicate (studlab, treat) rows using Cochrane Handbook 6.5.2.10.",
      n_before - nrow(long_df)
    ))
  }

  # 6. Validate
  .validate_long(long_df)

  tibble::as_tibble(long_df)
}

# --------------------------------------------------------------------------
# Input source resolver
# --------------------------------------------------------------------------
.resolve_input <- function(data) {
  if (is.data.frame(data)) {
    return(as.data.frame(data, stringsAsFactors = FALSE))
  }

  if (is.character(data) && length(data) == 1) {
    if (file.exists(data)) {
      ext <- tolower(tools::file_ext(data))
      if (ext == "csv") {
        return(utils::read.csv(data, stringsAsFactors = FALSE))
      }
      if (ext == "tsv") {
        return(utils::read.delim(data, stringsAsFactors = FALSE))
      }
      if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          rlang::abort(paste0(
            "Reading Excel files requires the 'readxl' package. ",
            "Install with install.packages('readxl')."
          ))
        }
        return(as.data.frame(readxl::read_excel(data), stringsAsFactors = FALSE))
      }
      rlang::abort(paste0("Unrecognised file extension: '", ext, "'."))
    }
    # Multi-line text (clipboard paste)
    return(.parse_pasted_text(data))
  }

  if (is.character(data) && length(data) > 1) {
    return(.parse_pasted_text(paste(data, collapse = "\n")))
  }

  rlang::abort(paste0(
    "ingest_data: 'data' must be a data.frame, file path, or pasted text. ",
    "Got class: ", paste(class(data), collapse = "/")
  ))
}

.parse_pasted_text <- function(txt) {
  if (!nzchar(trimws(txt))) {
    rlang::abort("ingest_data: pasted text is empty.")
  }
  # Detect delimiter (tab, comma, semicolon)
  first_line <- strsplit(txt, "\n", fixed = TRUE)[[1]][1]
  sep <- if (grepl("\t", first_line)) {
    "\t"
  } else if (grepl(";", first_line) && !grepl(",", first_line)) {
    ";"
  } else {
    ","
  }
  utils::read.table(text = txt, sep = sep, header = TRUE,
                    stringsAsFactors = FALSE, na.strings = c("", "NA", "."))
}

# --------------------------------------------------------------------------
# Column-name mapping
# --------------------------------------------------------------------------
.apply_mapping <- function(df, mapping) {
  if (!is.list(mapping) || is.null(names(mapping))) {
    rlang::abort("mapping must be a named list.")
  }
  for (canonical in names(mapping)) {
    user_col <- mapping[[canonical]]
    if (!is.character(user_col) || length(user_col) != 1) next
    if (user_col %in% names(df)) {
      names(df)[names(df) == user_col] <- canonical
    }
  }
  df
}

# --------------------------------------------------------------------------
# Format detection
# --------------------------------------------------------------------------
.detect_format <- function(df) {
  cols <- names(df)
  wide_pairs <- c(
    all(c("event_e", "event_c") %in% cols),
    all(c("mean_e", "mean_c") %in% cols),
    all(c("n_e", "n_c") %in% cols)
  )
  if (any(wide_pairs)) return("wide")

  if ("studlab" %in% cols && any(duplicated(df$studlab))) {
    return("long")
  }
  if ("study" %in% cols && any(duplicated(df$study))) {
    return("long")
  }

  rlang::abort(paste0(
    "Could not auto-detect format. Specify format = 'long' or 'wide'. ",
    "For long format, 'studlab' must appear twice per study (one row per arm). ",
    "For wide format, expected paired columns like 'event_e'/'event_c' or 'mean_e'/'mean_c'."
  ))
}

# --------------------------------------------------------------------------
# Wide -> long pivot
# --------------------------------------------------------------------------
.wide_to_long <- function(df) {
  cols <- names(df)
  if (!"studlab" %in% cols && "study" %in% cols) {
    names(df)[names(df) == "study"] <- "studlab"
  }
  if (!"studlab" %in% names(df)) {
    rlang::abort("Wide format requires a 'studlab' column.")
  }

  is_binary  <- all(c("event_e", "event_c", "n_e", "n_c") %in% names(df))
  is_contin  <- all(c("mean_e", "mean_c", "sd_e", "sd_c", "n_e", "n_c") %in% names(df))

  if (!is_binary && !is_contin) {
    rlang::abort(paste0(
      "Wide format must include either ('event_e','event_c','n_e','n_c') ",
      "for binary outcomes or ('mean_e','mean_c','sd_e','sd_c','n_e','n_c') ",
      "for continuous outcomes."
    ))
  }

  # Canonical wide-only columns get pivoted; everything else is treated as
  # per-study extra and copied into both arm rows so Step 2 column mapping can
  # see them.
  canonical_wide <- c("studlab",
                      "n_e", "n_c",
                      "event_e", "event_c",
                      "mean_e", "mean_c",
                      "sd_e", "sd_c")
  extra_cols <- setdiff(names(df), canonical_wide)

  rows_e <- data.frame(
    studlab = df$studlab,
    treat   = "experimental",
    n       = df$n_e,
    stringsAsFactors = FALSE
  )
  rows_c <- data.frame(
    studlab = df$studlab,
    treat   = "control",
    n       = df$n_c,
    stringsAsFactors = FALSE
  )
  if (is_binary) {
    rows_e$event <- df$event_e
    rows_c$event <- df$event_c
  }
  if (is_contin) {
    rows_e$mean <- df$mean_e
    rows_e$sd   <- df$sd_e
    rows_c$mean <- df$mean_c
    rows_c$sd   <- df$sd_c
  }
  for (col in extra_cols) {
    rows_e[[col]] <- df[[col]]
    rows_c[[col]] <- df[[col]]
  }

  rbind(rows_e, rows_c)
}

# --------------------------------------------------------------------------
# Long-format normalisation
# --------------------------------------------------------------------------
.normalise_long <- function(df, experimental_label = NULL, control_label = NULL) {
  cols <- names(df)

  # Auto-rename common aliases to canonical names
  aliases <- list(
    studlab = c("study", "trial", "study_id", "trial_id"),
    treat   = c("treatment", "arm"),
    n       = c("n_randomized", "n_total", "sample_size", "N"),
    event   = c("events", "d_r", "responders", "n_events"),
    mean    = c("means"),
    sd      = c("stdev", "stddev"),
    rob     = c("risk_of_bias", "rob_d", "rob_overall", "rob_judgement"),
    indirectness = c("indir"),
    subgroup = c("group", "stratum")
  )
  for (canonical in names(aliases)) {
    if (!canonical %in% names(df)) {
      hit <- intersect(aliases[[canonical]], names(df))
      if (length(hit) >= 1) {
        names(df)[names(df) == hit[1]] <- canonical
      }
    }
  }

  if (!"studlab" %in% names(df) || !"treat" %in% names(df)) {
    rlang::abort(
      "Long format requires 'studlab' and 'treat' columns (or aliases 'study'/'arm'/'treatment')."
    )
  }

  # Tag experimental vs control if requested
  if (!is.null(experimental_label) && !is.null(control_label)) {
    df$treat <- ifelse(df$treat == experimental_label, "experimental",
                       ifelse(df$treat == control_label, "control", df$treat))
  }
  df
}

# --------------------------------------------------------------------------
# Validation
# --------------------------------------------------------------------------
.validate_long <- function(df) {
  required <- c("studlab", "treat", "n")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    rlang::abort(sprintf(
      "Long-format data is missing required columns: %s",
      paste(missing, collapse = ", ")
    ))
  }

  if (!is.numeric(df$n)) {
    df$n <- suppressWarnings(as.numeric(df$n))
  }
  if (any(is.na(df$n)) || any(df$n < 0)) {
    rlang::abort("'n' must be non-negative integers.")
  }

  if ("event" %in% names(df)) {
    if (!is.numeric(df$event)) df$event <- suppressWarnings(as.numeric(df$event))
    if (any(!is.na(df$event) & df$event < 0)) {
      rlang::abort("'event' must be non-negative integers (NA permitted).")
    }
  }
  if ("sd" %in% names(df)) {
    if (!is.numeric(df$sd)) df$sd <- suppressWarnings(as.numeric(df$sd))
    if (any(!is.na(df$sd) & df$sd < 0)) {
      rlang::abort("'sd' must be non-negative (NA permitted).")
    }
  }

  # Each studlab must appear exactly twice (one row per arm)
  counts <- table(df$studlab)
  bad    <- names(counts)[counts != 2]
  if (length(bad) > 0) {
    rlang::abort(sprintf(
      "Each study must have exactly 2 rows (one per arm). Offending: %s (counts: %s)",
      paste(bad, collapse = ", "),
      paste(counts[bad], collapse = ", ")
    ))
  }

  invisible(df)
}
