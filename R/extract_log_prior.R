##' Extract log prior from fitted model
##'
##' Extract log prior from variable in fitted Stan model.
##'
##' @name extract_log_prior
##' 
##' @param x Model fit.
##' @param parameter_name Name of parameter in Stan model
##'   corresponding to log prior, default is "log_prior".
##' @param ... Arguments passed to individual methods.
##' @return A draws_array object containing log_prior values.
NULL

##' @rdname extract_log_prior
##' @export
extract_log_prior <- function(x, ...) {
  UseMethod("extract_log_prior")
}

##' @rdname extract_log_prior
##' @export
extract_log_prior.stanfit <- function(x,
                                      parameter_name = "log_prior", ...) {

  if (!inherits(x, "stanfit"))
    stop("Not a stanfit object.", call. = FALSE)
  if (x@mode != 0)
    stop("Stan model does not contain posterior draws.",
         call. = FALSE)
  if (!requireNamespace("rstan", quietly = TRUE))
    stop("Please load the 'rstan' package.", call. = FALSE)

  log_prior <- posterior::subset_draws(
    posterior::merge_chains(
      posterior::as_draws_array(x, pars = parameter_name)),
    variable = parameter_name
  )

  return(log_prior)
}

##' @rdname extract_log_prior
##' @export
extract_log_prior.brmsfit <- function(x,
                                      parameter_name = "log_prior", ...) {

  log_prior <- as.array(x, pars = parameter_name)

  return(log_prior)
}

##' @rdname extract_log_prior
##' @export
extract_log_prior.CmdStanFit <- function(x,
                                         parameter_name = "log_prior", ...) {

  log_prior <- x$draws(variables = parameter_name)

  return(log_prior)
}
