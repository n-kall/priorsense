SW <- function(...) {
  suppressWarnings(...)
}

rowsums_draws <- function(x) {
  posterior::draws_array(
        sum = rowSums(
          posterior::as_draws_array(x),
          dims = 2
        ),
        .nchains = posterior::nchains(x)
      )
}

remove_unwanted_vars <- function(x, excluded_variables = c("lprior", "log_lik", "lp__"), regex, ...) {

  draws <- posterior::as_draws_df(x, variable = variable, regex = regex)

    # remove unnecessary variables
    variable <- posterior::variables(draws)

    has_prefix <- sapply(
      excluded_variables,
      function(p) startsWith(variable, p)
    )
    variable <- variable[!(apply(has_prefix, 1, any))]
    draws <- posterior::subset_draws(draws, variable = variable)

  return(draws)
}

require_package <- function(package, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop2("Please install the '", package, "' package.")
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

stop2 <- function(...) {
  stop(..., call. = FALSE)
}


`%||%` <- function(x, y) {
  if (is.null(x)) x <- y
  x
}

seq_cols <- function(x) {
  seq_len(NCOL(x))
}
