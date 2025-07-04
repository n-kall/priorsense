% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{powerscale_plots}
\alias{powerscale_plots}
\alias{powerscale_plot_dens}
\alias{powerscale_plot_ecdf}
\alias{powerscale_plot_ecdf.powerscaled_sequence}
\alias{powerscale_plot_quantities}
\alias{powerscale_plot_quantities.powerscaled_sequence}
\title{Diagnostic plots for power-scaling sensitivity}
\usage{
powerscale_plot_dens(x, ...)

powerscale_plot_ecdf(x, ...)

\method{powerscale_plot_ecdf}{powerscaled_sequence}(
  x,
  variable = NULL,
  resample = FALSE,
  length = 3,
  facet_rows = "component",
  help_text = getOption("priorsense.plot_help_text", TRUE),
  colors = NULL,
  variables_per_page = getOption("priorsense.plot_variables_per_page", 6),
  ...
)

powerscale_plot_quantities(x, ...)

\method{powerscale_plot_quantities}{powerscaled_sequence}(
  x,
  variable = NULL,
  quantity = c("mean", "sd"),
  div_measure = "cjs_dist",
  resample = FALSE,
  measure_args = NULL,
  mcse = TRUE,
  quantity_args = NULL,
  help_text = getOption("priorsense.plot_help_text", TRUE),
  colors = NULL,
  variables_per_page = getOption("priorsense.plot_variables_per_page", 6),
  ...
)
}
\arguments{
\item{x}{An object of class \code{powerscaled_sequence} or an object
for which \code{powerscale_sequence} will first be run on.}

\item{...}{Arguments passed to \code{powerscale_sequence} if \code{x} is not
of class \code{powerscaled_sequence}.}

\item{variable}{A character vector of variable names. If \code{NULL}
(the default) all variables will be plotted.}

\item{resample}{Logical; Indicate whether or not draws should be
resampled based on calculated importance weights.}

\item{length}{Numeric specifying how many alpha values should be
used. Ignored of the object is of class \code{powerscaled_sequence}.}

\item{facet_rows}{Character defining the rows of the plot facets,
either "variable" or "component". Default is "variable".}

\item{help_text}{Logical indicating whether title and subtitle
with explanatory description should be included in the
plot. Default is TRUE. Can be set via option
"priorsense.show_help_text".}

\item{colors}{Character vector of colors to be used for
plots. Either length 3 for \code{powerscale_plot_ecdf} and
\code{powerscale_plot_dens} with order lowest, base, highest; or
length 2 for \code{powerscale_plot_quantities} with order low Pareto
k, high Pareto k. If \code{NULL} the defaults will be used.}

\item{variables_per_page}{Number specifying the maximum number of
variables to show on each page of the plot. Default is 6. If
\code{NULL} or \code{Inf}, all variables will be plotted on the same page.}

\item{quantity}{A character vector specifying one or several
quantities to plot. Options are "mean", "median", "sd", "mad",
"quantile".}

\item{div_measure}{Character (case sensitive) specifying the
divergence measure to use. The following methods are
implemented:
\itemize{
\item \code{"cjs_dist"}: Cumulative Jensen-Shannon distance. Default
method. See function \code{cjs_dist} for more details.
\item \code{"js_dist"}: Jensen-Shannon distance.
\item \code{"js_div"}: Jensen-Shannon divergence.
\item \code{"hellinger_dist"}: Hellinger distance.
\item \code{"kl_dist"}: Kullback-Leibler distance.
\item \code{"kl_div"}: Kullback-Leibler divergence.
\item \code{"ks_dist"}: Kolmogorov-Smirnov distance.
\item \code{"hellinger_dist"}: Hellinger distance.
\item \code{"ws_dist"}: Wassterstein distance (pass \code{measure_args = list(p = N)})
for a different order, where N is the order.
}}

\item{measure_args}{Named list of further arguments passed to divergence measure functions.}

\item{mcse}{Boolean; If TRUE will plot +/- 2 * Monte Carlo
standard error of the base quantity on the quantities plot.}

\item{quantity_args}{Named list of further arguments passed to
quantity functions. Passed as \code{.args} to
\verb{[posterior::summarise_draws]}.}
}
\value{
A ggplot object (or a \code{priorsense_plot} object which is a
list of ggplot objects if there is more than one page) that can
be further customized using the \strong{ggplot2} package.
}
\description{
Various diagnostic plots for power-scaling sensitivity. See \strong{Plot
Descriptions} below for details.
}
\section{Plot Descriptions}{
 \describe{
\item{\code{powerscale_plot_dens()}}{ Kernel density plot of
power-scaled posterior draws with respect to power-scaling.  }
\item{\code{powerscale_plot_ecdf()}}{ Empirical cumulative
distribution function plot of power-scaled posterior draws with
respect to power-scaling.  }
\item{\code{powerscale_plot_quantities()}}{ Plot of posterior
quantities with respect to power-scaling.} }
}

\examples{
ex <- example_powerscale_model()

powerscale_plot_dens(ex$draws)
}
