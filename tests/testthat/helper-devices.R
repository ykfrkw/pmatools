# helper-devices.R — shared test helpers (auto-sourced by testthat)

# Evaluate plotting code against a null PDF device so tests never open the
# default device (which would leave tests/testthat/Rplots.pdf behind).
with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}
