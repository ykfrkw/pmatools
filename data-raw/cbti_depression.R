## data-raw/cbti_depression.R
##
## CBT-I for Depression Response — synthetic sample dataset
##
## This script reads the CSV from inst/extdata/ and saves it as an R data object.
## Run once (from the package root) to regenerate data/cbti_depression.rda:
##
##   source("data-raw/cbti_depression.R")
##
## Requires: usethis

cbti_depression <- read.csv(
  file.path("inst", "extdata", "cbti_depression.csv"),
  stringsAsFactors = FALSE
)

# Keep only the columns documented in R/data.R (the lean teaching subset);
# the full extraction stays available in inst/extdata/cbti_depression.csv.
cbti_depression <- cbti_depression[
  , c("study", "arm", "year", "treatment", "n_randomized", "d_r", "rob_d")
]

usethis::use_data(cbti_depression, overwrite = TRUE)
