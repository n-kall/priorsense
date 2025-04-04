##' @param x An object of class `powerscaled_sequence` or an object
##'   for which `powerscale_sequence` will first be run on.
##' @param variable A character vector of variable names. If `NULL`
##'   (the default) all variables will be plotted.
##' @param quantity A character vector specifying one or several
##'   quantities to plot. Options are "mean", "median", "sd", "mad",
##'   "quantile".
##' @param quantity_args Named list of further arguments passed to
##'   quantity functions. Passed as `.args` to
##'   `[posterior::summarise_draws]`.
##' @param length Numeric specifying how many alpha values should be
##'   used. Ignored of the object is of class `powerscaled_sequence`.
##' @param mcse Boolean; If TRUE will plot +/- 2 * Monte Carlo
##'   standard error of the base quantity on the quantities plot.
##' @param help_text Logical indicating whether title and subtitle
##'   with explanatory description should be included in the
##'   plot. Default is TRUE. Can be set via option
##'   "priorsense.show_help_text".
##' @param colors Character vector of colors to be used for
##'   plots. Either length 3 for `powerscale_plot_ecdf` and
##'   `powerscale_plot_dens` with order lowest, base, highest; or
##'   length 2 for `powerscale_plot_quantities` with order low Pareto
##'   k, high Pareto k. If `NULL` the defaults will be used.
##' @param facet_rows Character defining the rows of the plot facets,
##'   either "variable" or "component". Default is "variable".
##' @param variables_per_page Number specifying the maximum number of
##'   variables to show on each page of the plot. Default is 6. If
##'   `NULL` or `Inf`, all variables will be plotted on the same page.
##' @param ... Arguments passed to `powerscale_sequence` if `x` is not
##'   of class `powerscaled_sequence`.
