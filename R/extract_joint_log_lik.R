##' Extract joint log likelihood from fitted model
##'
##' Extract joint log likelihood from fitted model
##'
##' @name joint_log_lik
##'
##' @param x Model fit.
##' @param log_lik_name Name of parameter in Stan model
##'   corresponding to log likelihood, default is "log_lik".
##' @param ... Arguments passed to individual methods.
##' @return A draws_array object containing log_prior values.
NULL

##' @rdname joint_log_lik
##' @export
log_lik_stanfit <- function(x, log_lik_name = "log_lik", ...) {
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
log_lik_CmdStanFit <- function(x, log_lik_name = "log_lik", ...) {

  # sum over correct dimension
  log_lik <- x$draws(variables = log_lik_name)

  return(log_lik)
}
