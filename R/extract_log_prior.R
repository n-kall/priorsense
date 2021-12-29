##' Extract log prior from fitted model
##'
##' Extract log prior from variable in fitted Stan model.
##'
##' @name log_prior
##' 
##' @param x Model fit.
##' @param parameter_name Name of parameter in Stan model
##'   corresponding to log prior, default is "log_prior".
##' @param ... Arguments passed to individual methods.
##' @return A draws_array object containing log_prior values.
NULL

##' @rdname log_prior
##' @export
log_prior_stanfit <- function(x,
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
      posterior::as_draws_array(x, variable = parameter_name)),
    variable = parameter_name
  )

  return(log_prior)
}

##' @rdname log_prior
##' @export
log_prior_brmsfit <- function(x, parameter_name = "lprior", ...) {

  log_prior <- as.array(x, variable = parameter_name)

  return(log_prior)
}

##' @rdname log_prior
##' @export
log_prior_CmdStanFit <- function(x, parameter_name = "log_prior", ...) {

  log_prior <- x$draws(variables = parameter_name)

  return(log_prior)
}
