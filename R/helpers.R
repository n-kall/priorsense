##' stop without call
##' @keywords internal
##' @noRd
##' @param ... 
stop2 <- function(...) {
  stop(..., call. = FALSE)
}

##' row sums for draws objects
##' @param x draws object
##' @return draws object with rows summed
##' @keywords internal
##' @noRd
rowsums_draws <- function(x) {
  posterior::draws_array(
    sum = rowSums(
      posterior::as_draws_array(x),
      dims = 2
    ),
    .nchains = posterior::nchains(x)
  )
}

##' remove unwanted variables
##' @param x draws object
##' @param excluded_variables character vector specifying variables to remove
##' @param regex flag indicating whether to match with regex
##' @param ... unused
##' @return draws object without excluded variables
##' @keywords internal
##' @noRd
remove_unwanted_vars <- function(x,
                                 excluded_variables = c(
                                   "lprior",
                                   "log_lik",
                                   "lp__"
                                 ),
                                 regex = TRUE, ...) {

  draws <- posterior::as_draws_df(x)

  draws <- posterior::subset_draws(draws,
                                   variable = excluded_variables,
                                   exclude = TRUE,
                                   regex = regex)

  return(draws)
}
##' require package
##' 
##' @param package character specifying which package is required
##' @param version character specifying which version is required,
##'   default is NULL, implying any version is acceptable
##' @param message message to display if package is not installed
##' @return invisibly returns `TRUE`
##' @keywords internal
##' @noRd
require_package <- function(package, version = NULL, message = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop2("Please install the '", package, "' package", message)
  }
  if (!is.null(version)) {
    version <- as.package_version(version)
    if (utils::packageVersion(package) < version) {
      stop2("Please install package '", package,
            "' version ", version, " or higher.")
    }
  }
  invisible(TRUE)
}

##' get power-scaling details
##' @param x object with powerscaling attribute
##' @return powerscaling attribute
##' @keywords internal
##' @noRd
get_powerscaling_details <- function(x) {
  attr(x, "powerscaling")
}
