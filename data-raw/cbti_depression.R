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

usethis::use_data(cbti_depression, overwrite = TRUE)
