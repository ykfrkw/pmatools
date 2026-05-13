\name{rare_event_diagnostics}
\alias{rare_event_diagnostics}
\alias{run_rare_ma}
\alias{plot_rare_sensitivity_forest}
\title{Rare-event pairwise meta-analysis helpers}
\usage{
rare_event_diagnostics(
  x,
  experimental_label = NULL,
  control_label = NULL,
  event_rate_threshold = 0.01,
  zero_study_fraction_threshold = 0.5
)

run_rare_ma(
  data,
  effect_scale = c("OR", "RR"),
  primary_method = NULL,
  random = TRUE,
  common = FALSE,
  method.tau = c("REML", "DL"),
  experimental_label = NULL,
  control_label = NULL
)

plot_rare_sensitivity_forest(
  x,
  title = NULL,
  xlim = NULL,
  threshold_lines = NULL,
  favors_left = NULL,
  favors_right = NULL,
  ...
)
}
\arguments{
\item{x}{For \code{rare_event_diagnostics()}, canonical long-format binary data
or a \code{metabin} object. For \code{plot_rare_sensitivity_forest()}, a
\code{pma_rare_meta} object from \code{run_rare_ma()}.}

\item{experimental_label, control_label}{Optional arm labels for long-format
data.}

\item{event_rate_threshold}{Event-rate threshold used as the primary rare-event
signal. Default is 0.01 (1 percent).}

\item{zero_study_fraction_threshold}{Fraction of studies with at least one
zero-event arm used as a sparse-data warning.}

\item{data}{Canonical long-format binary data.}

\item{effect_scale}{\code{"OR"} (recommended) or \code{"RR"}.}

\item{primary_method}{Optional rare-method id to force as primary.}

\item{random, common}{Logical model flags.}

\item{method.tau}{Between-study variance estimator for methods that use it.}

\item{title}{Optional plot title.}

\item{xlim}{Optional x-axis limits on the back-transformed scale.}

\item{threshold_lines}{Optional positive log-scale threshold.}

\item{favors_left, favors_right}{Optional labels shown on the left and right
of the x-axis.}

\item{...}{Additional arguments passed to \code{meta::forest()}.}
}
\value{
\code{rare_event_diagnostics()} returns a list with event rates, zero-cell
counts, and \code{rare_flow}. \code{run_rare_ma()} returns a
\code{pma_rare_meta} object with the primary \code{meta} object, method table,
fits, and diagnostics. \code{plot_rare_sensitivity_forest()} draws a plot and
returns \code{NULL} invisibly.
}
\description{
Helpers for detecting rare-event binary meta-analysis patterns, running a
small OR-first sensitivity method suite, and plotting pooled method estimates
in one forest plot.
}
\details{
The rare-events workflow is intended as a diagnostic and sensitivity-analysis
aid. For odds ratios, the default primary method is the beta-binomial model
with correlated responses from \pkg{mmeta} when available; Mantel-Haenszel
without continuity correction is used as fallback. Continuity-correction
methods are retained as sensitivity analyses.
}
\references{
Efthimiou O. Practical guide to the meta-analysis of rare events.
\emph{Evidence-Based Mental Health}. 2018;21(2):72-76.
doi:10.1136/eb-2018-102911.

Tsujimoto Y, Tsutsumi Y, Kataoka Y, Shiroshita A, Efthimiou O, Furukawa TA.
The impact of continuity correction methods in Cochrane reviews with
single-zero trials with rare events: A meta-epidemiological study.
\emph{Research Synthesis Methods}. 2024;15(5):769-779.
doi:10.1002/jrsm.1720.
}
