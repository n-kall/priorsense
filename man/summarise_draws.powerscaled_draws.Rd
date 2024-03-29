% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_draws.R
\name{summarise_draws.powerscaled_draws}
\alias{summarise_draws.powerscaled_draws}
\title{Summarise draws}
\usage{
\method{summarise_draws}{powerscaled_draws}(
  .x,
  ...,
  .num_args = getOption("posterior.num_args", list()),
  .args = list(),
  base_draws = NULL,
  diagnostics = FALSE,
  div_measures = "cjs_dist",
  measure_args = list(),
  resample = FALSE
)
}
\arguments{
\item{.x}{An object of class powerscaled_draws}

\item{...}{summary functions}

\item{.num_args}{(named list) Optional arguments passed to
\link[tibble:num]{num()} for pretty printing of summaries. Can be
controlled globally via the \code{posterior.num_args}
\link[base:options]{option}.}

\item{.args}{arguments for summary functions}

\item{base_draws}{base draws}

\item{diagnostics}{boolean, if TRUE include diagnostics for mean
and variance}

\item{div_measures}{divergence measures}

\item{measure_args}{arguments for divergence measures}

\item{resample}{resample draws}
}
\description{
Summarise power-scaled draws
}
