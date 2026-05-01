# pmatools auto-generated reproducibility script
# Generated: {{timestamp}}
# pmatools version: {{pmatools_version}}
#
# To reproduce: place this file alongside data_long.csv and run
#   Rscript analysis.R
# or open in RStudio and source line by line.

library(pmatools)

# ----- 1. Load data -----
data <- ingest_data("data_long.csv", format = "long")

# ----- 2. Meta-analysis -----
ma <- run_ma(
  data,
  outcome_type = "{{outcome_type}}",
  sm           = "{{sm}}",
  method       = {{method_arg}},
  method.tau   = "{{method_tau}}",
  random       = {{random}},
  common       = {{common}},
  hakn         = {{hakn}},
  prediction   = {{prediction}},
  incr         = {{incr}}{{subgroup_arg}}
)

# ----- 3. GRADE certainty assessment -----
g <- grade_meta(
  ma,
  study_design            = "{{study_design}}",
  rob                     = {{rob_arg}},
  rob_dominant_threshold  = {{rob_dom_threshold}},
  rob_inflation_threshold = {{rob_inf_threshold}},
  small_values            = {{small_values_arg}},
  indirectness            = {{indirectness_arg}},
  inconsistency           = {{inconsistency_arg}},
  mid                     = {{mid_arg}},
  mid_scale               = "{{mid_scale}}",
  outcome_type            = "{{ois_outcome_type}}",
  ois_p0                  = {{ois_p0_arg}},
  ois_p1                  = {{ois_p1_arg}},
  ois_delta               = {{ois_delta_arg}},
  ois_sd                  = {{ois_sd_arg}},
  pubias_small_industry   = {{pubias_small_industry_arg}},
  pubias_funnel_asymmetry = {{pubias_funnel_arg}},
  pubias_unpublished      = {{pubias_unpub_arg}},
  outcome_name            = "{{outcome_name}}"
)

print(g)
summary(g)

# ----- 4. Plots -----
plot_forest(ma, title = "{{outcome_name}}")
plot_funnel(ma)

# ----- 5. SoF table -----
sof <- sof_table(g, per = {{per}}, prediction = {{sof_prediction}}{{convert_args}})
print(sof)

# ----- 6. Appendix report (docx) -----
grade_report(
  outcomes    = list("{{outcome_name}}" = g),
  primary     = "{{outcome_name}}",
  format      = "docx",
  output_dir  = ".",
  output_file = "grade_appendix"
)
