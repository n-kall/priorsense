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
##' @return A draws_array object containing log_prior values.
log_lik_draws <- function(x, ...) {
  UseMethod("log_lik_draws")
}

##' @rdname joint_log_lik
##' @export
log_lik_draws.stanfit <- function(x, log_lik_name = "log_lik", ...) {
  log_lik <- loo::extract_log_lik(
      stanfit = x,
      parameter_name = log_lik_name,
      merge_chains = FALSE
    )

  log_lik <- posterior::as_draws_array(log_lik)

  return(log_lik)
}

##' @rdname joint_log_lik
##' @export
log_lik_draws.CmdStanFit <- function(x, log_lik_name = "log_lik", ...) {

  log_lik <- x$draws(variables = log_lik_name)

  return(log_lik)
}

##' @rdname joint_log_lik
##' @export
log_lik_draws.draws <- function(x, log_lik_name = "log_lik", ...) {

  log_lik <- posterior::subset_draws(x, variable = log_lik_name)

  return(log_lik)
}
