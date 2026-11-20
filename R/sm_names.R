# sm_names.R - the one place an effect-measure name is put into pmatools' spelling
#
# {meta} stores the summary measure of a ratio-of-means analysis as "ROM", even
# when the caller asked for "RoM": metacont() normalises the name. Every
# comparison in pmatools is written against its own spelling ("RoM"), so a
# raw `meta_obj$sm` read let a ratio of means fall through as a non-ratio
# measure -- log-scale numbers printed as if natural, `threshold_scale =
# "auto"` aborting, forest-plot limits going negative.
#
# Reads: an `sm` string, or any object carrying `$sm` (a {meta} object, a
# grade_meta's `$meta`). Produces: the same name in pmatools' canonical
# spelling, matched case-insensitively. It never writes back to the object:
# {meta} needs its own "ROM" for back-transformation and forest().
#
# Used by every file that reads `$sm` (effect_scales.R, sof_bmj.R,
# sof_table.R, domain_*.R, plot_forest*.R, pubias_status.R, rating_target.R,
# grade_meta.R, grade_table.R, export_bundle*.R).
#
# A helper belongs here when it decides how an effect-measure name is spelled;
# what a measure means for a threshold or a display belongs to the file that
# draws or converts it.

# pmatools' spelling of every measure it names. Lookup is by toupper().
.SM_CANONICAL <- c("OR", "RR", "HR", "IRR", "RoM", "RD", "ARD", "SMD", "MD")

# Canonicalise an sm string; NULL, NA and unknown names pass through unchanged.
.canon_sm <- function(sm) {
  if (is.null(sm) || length(sm) != 1 || is.na(sm)) return(sm)
  sm_text <- as.character(sm)
  match_index <- match(toupper(sm_text), toupper(.SM_CANONICAL))
  if (is.na(match_index)) sm_text else .SM_CANONICAL[[match_index]]
}

# The canonical sm of an object carrying `$sm`, or NULL when it has none.
.sm_of <- function(x) {
  if (is.null(x)) return(NULL)
  .canon_sm(x$sm)
}
