##' Extract log prior from fitted model
##'
##' Extract log prior from variable in fitted Stan model.
##'
##' @name log_prior
##'
##' @param x Model fit.
##' @param log_prior_name Name of parameter in Stan model
##'   corresponding to log prior, default is "lprior".
##' @param ... Arguments passed to individual methods.
##' @return A draws_array object containing log_prior values.
NULL

##' @rdname log_prior
##' @export
log_prior_stanfit <- function(x, log_prior_name = "lprior", ...) {

  if (!inherits(x, "stanfit"))
    stop("Not a stanfit object.", call. = FALSE)
  if (x@mode != 0)
    stop("Stan model does not contain posterior draws.",
         call. = FALSE)
  if (!requireNamespace("rstan", quietly = TRUE))
    stop("Please load the 'rstan' package.", call. = FALSE)

  log_prior <- posterior::subset_draws(
    posterior::as_draws_array(x, variable = log_prior_name),
    variable = log_prior_name
  )

  return(log_prior)
}

##' @rdname log_prior
##' @export
log_prior_CmdStanFit <- function(x, log_prior_name = "lprior", ...) {

  log_prior <- x$draws(variables = log_prior_name)

  return(log_prior)
}

##' @rdname log_prior
##' @export
log_prior_draws <- function(x, log_prior_name = "lprior", ...) {

  log_prior <- posterior::subset_draws(x, variable = log_prior_name)

  return(log_prior)
}
