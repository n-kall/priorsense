##' Extract log likelihood from fitted model
##'
##' Extract log likelihood from fitted model
##'
##' @name log_lik_draws
##'
##' @param x Model fit.
##' @param log_lik_name Name of parameter in Stan model
##'   corresponding to log likelihood, default is "log_lik".
##' @param ... Arguments passed to individual methods.
##' @return A draws_array object containing log_lik values.
log_lik_draws <- function(x, ...) {
  UseMethod("log_lik_draws")
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.stanfit <- function(x, log_lik_name = "log_lik", ...) {
  log_lik <- as.array(x, pars = log_lik_name)

  log_lik <- posterior::as_draws_array(log_lik)

  return(log_lik)
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.CmdStanFit <- function(x, log_lik_name = "log_lik", ...) {

  log_lik <- x$draws(variables = log_lik_name)

  return(log_lik)
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.draws <- function(x, log_lik_name = "log_lik", ...) {

  log_lik <- posterior::subset_draws(x, variable = log_lik_name)

  return(log_lik)
}
