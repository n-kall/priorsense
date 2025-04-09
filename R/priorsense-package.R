#' priorsense: Prior (and likelihood) diagnostics and sensitivity
#' analysis
#'
#' @name priorsense-package
#' @aliases priorsense
#' @importFrom posterior summarise_draws
#'
#' @description The \pkg{priorsense} package provides functions for
#'   prior and likelihood sensitivity analysis of Bayesian
#'   models. Currently it implements methods to determine the
#'   sensitivity of the posterior to power-scaling perturbations of
#'   the prior and likelihood and is the first implementation of the
#'   method described in Kallioinen et al. (2023).
#'
#' @srrstats {G1.1} *Statistical Software should document whether the algorithm(s) it implements are:* - *The first implementation of a novel algorithm*; or - *The first implementation within **R** of an algorithm which has previously been implemented in other languages or contexts*; or - *An improvement on other implementations of similar algorithms in **R***. 
#'
#' @details The main diagnostic function provided by \pkg{priorsense}
#'   is \code{\link{powerscale_sensitivity}}. Given a fitted model
#'   or draws object, it computes the powerscaling sensitivity
#'   diagnostic described in Kallioinen et al. (2023). It does so by
#'   perturbing the prior and likelihood and computing the effect on
#'   the posterior, without needing to refit the model (using Pareto
#'   smoothed importance sampling and importance weighted moment
#'   matching; Vehtari et al. 2022, Paananen et al. 2021).
#' @srrstats {G1.0} *Statistical Software should list at least one primary reference from published academic literature.* 
#'
#' In addition, visual diagnostics are available by first using
#' \code{\link{powerscale_sequence}} to create a sequence of perturbed
#' posteriors, and then a plot function such as
#' \code{\link{powerscale_plot_ecdf}} to visualise the change.
#'
#' The following global options are available:
#'    * `priorsense.plot_help_text`: If `TRUE` (the default), priorsense plots will include a title and explanatory text. If `FALSE` they will not.
#'    * `priorsense.plot_variables_per_page`: Number specifying the maximum number of variables to be plotted on one page of a plot.
#'    * `priorsense.plot_ask`: If `TRUE` (the default), when multiple pages are plotted input is required before each subsequent page is rendered.
#'       If `FALSE` no input is required.
#' @srrstats {G1.3} vignettes, documentation and linked papers explain statistical terminology
#' @srrstats {G1.4} All functions are documented with roxygen2
#' @srrstats {G1.2} Lifecycle statement is in the file CONTRIBUTING.md
#' @srrstats {G1.4a} All internal (non-exported) functions are documneted with roxyget2 along with a final `@noRd` tag
#' @srrstats {G2.10} Tabular inputs are converted to `posterior::draws` objects and subsetting is handled through `posterior` functions.
#' @srrstats {BS2.12, BS2.13} `priorsense.plot_help_text` controls the inclusion of explanatory text in plots
#' @srrstats {BS2.14} warnings are provided through built in R function `warning()`
#' @srrstats {BS2.15} errors are provided through checkmate or built in R functions

#' @seealso
#' \code{\link{powerscale_sensitivity}}
#' \code{\link{powerscale_sequence}}
#' \code{\link{powerscale}}
#' \code{\link{powerscale_plot_ecdf}}
#' \code{\link{powerscale_plot_dens}}
#' \code{\link{powerscale_plot_quantities}}
#' @template powerscale_references
"_PACKAGE"


## usethis namespace: start
##' @importFrom lifecycle deprecated
##' ## usethis namespace: end
#' NULL
