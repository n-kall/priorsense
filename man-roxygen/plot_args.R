##' @param x An object of class `powerscaled_sequence` or an object
##'   for which `powerscale_sequence` will first be run on.
##' @param variables A character vector of variable names.
##' @param quantities A character vector specifying quantities to
##'   plot. Options are "mean", "median", "sd", "mad", "quantile".
##' @param quantity_args Named list of further arguments passed to
##'   quantity functions. Passed as `.args` to
##'   `[posterior::summarise_draws]`.
##' @param mcse Boolean; If TRUE will plot +/- 2 * Monte Carlo
##'   standard error of the base quantity on the quantities plot.
##' @param ... Arguments passed to `powerscale_sequence` if `x` is not
##'   of class `powerscaled_sequence`.
