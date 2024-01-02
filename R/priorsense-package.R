#' priorsense: Prior (and likelihood) diagnostics and sensitivity
#' analysis
#'
#' @docType package
#' @name priorsense-package
#' @aliases priorsense
#' @import posterior
#'
#' @description The \pkg{priorsense} package provides functions for
#'   prior and likelihood sensitivity analysis of Bayesian
#'   models. Currently it implements methods to determine the
#'   sensitivity of the posterior to power-scaling perturbations of
#'   the prior and likelihood.
#'
#' @details The main diagnostic function provided by \pkg{priorsense}
#'   is \code{\link{powerscale_sensitivity}}. Given a fitted model
#'   or draws object, it computes the powerscaling sensitivity
#'   diagnostic described in Kallioinen et al. (2023). It does so by
#'   perturbing the prior and likelihood and computing the effect on
#'   the posterior, without needing to refit the model (using Pareto
#'   smoothed importance sampling and importance weighted moment
#'   matching; Vehtari et al. 2022, Paananen et al. 2021).
#'
#' In addition, visual diagnostics are available by first using
#' \code{\link{powerscale_sequence}} to create a sequence of perturbed
#' posteriors, and then a plot function such as
#' \code{\link{powerscale_plot_ecdf}} to visualise the change.
#'
#' @seealso
#' \code{\link{powerscale_sensitivity}}
#' \code{\link{powerscale_sequence}}
#' \code{\link{powerscale}}
#' \code{\link{powerscale_plot_ecdf}}
#' \code{\link{powerscale_plot_dens}}
#' \code{\link{powerscale_plot_quantities}}
#' @template powerscale_references
NULL
