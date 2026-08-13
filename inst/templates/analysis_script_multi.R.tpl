# pmatools auto-generated reproducibility script (multi-outcome)
# Generated: {{timestamp}}
# pmatools version: {{pmatools_version}}
#
# To reproduce: place this file alongside data_long.csv and run
#   Rscript analysis.R
# or open in RStudio and source line by line.
#
# Every argument below is the one the original session used, including the
# per-outcome overrides. run_ma() is still called once per outcome: the
# multi-outcome data is split on its `outcome` column by run_ma_multi().

library(pmatools)

# ----- 1. Load data -----
data <- ingest_data("data_long.csv", format = "long")

# ----- 2. Meta-analysis, one per outcome -----
ma_list <- run_ma_multi(
  data,
  outcomes     = {{outcomes_arg}},
  sm           = {{sm_arg}},
  outcome_type = {{outcome_type_arg}}{{ma_extra_args}}
)
{{rare_block}}
# ----- 3. GRADE certainty assessment, one per outcome -----
set <- grade_meta_multi(
  ma_list,
  common = {{common_arg}},
  per_outcome = {{per_outcome_arg}}
)
{{not_reported_block}}
set <- reorder_outcomes(set, {{order_arg}})
{{primary_line}}
print(set)
summary(set)

# ----- 4. Summary of findings (all outcomes) -----
sof <- grade_table(set, style = "{{style}}", per = {{per}},
                   prediction = {{sof_prediction}})
{{sof_notes_block}}print(sof)

# ----- 5. Per-outcome plots and data -----
outcome_dirs <- {{dir_names_arg}}

for (i in seq_along(set$order)) {
  nm  <- set$order[i]
  g   <- set$outcomes[[nm]]
  # No study reported this outcome, so there is nothing to plot or write out.
  if (inherits(g, "pmatools_not_reported")) next
  dir_i <- file.path("outcomes", outcome_dirs[i])
  dir.create(dir_i, recursive = TRUE, showWarnings = FALSE)

  grDevices::pdf(file.path(dir_i, "forest_plot.pdf"), width = 8, height = 6)
  plot_forest(g$meta, title = nm)
  grDevices::dev.off()

  grDevices::pdf(file.path(dir_i, "funnel_plot.pdf"), width = 7, height = 6)
  plot_funnel(g$meta)
  grDevices::dev.off()

  # Only meaningful when Core GRADE 4 Fig 2 restricted the analysis: the
  # full-analysis forest then documents what was left out.
  if (isTRUE(g$rob_refit)) {
    grDevices::pdf(file.path(dir_i, "forest_plot_full.pdf"),
                   width = 8, height = 6)
    plot_forest(g$meta_full, title = paste0(nm, " (all studies)"))
    grDevices::dev.off()
  }

  if (!is.null(g$indirectness_subdomains)) {
    print(indirectness_table(g))
  }

  write.csv(data[!is.na(data$outcome) & data$outcome == nm, , drop = FALSE],
            file.path(dir_i, "data_long.csv"), row.names = FALSE)
}

# ----- 6. Appendix report (docx) -----
grade_report(
  set,
  style       = "{{style}}",
  format      = "docx",
  output_dir  = ".",
  output_file = "evidence_profile"
)
