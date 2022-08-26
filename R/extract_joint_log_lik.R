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
joint_log_lik_stanfit <- function(x, log_lik_name = "log_lik", ...) {
  log_lik <- rowSums(
    x = loo::extract_log_lik(
      stanfit = x,
      parameter_name = log_lik_name,
      merge_chains = FALSE
    ),
    dims = 2
  )

  dim(log_lik) <- c(dim(log_lik), 1)
  log_lik <- posterior::as_draws_array(log_lik)
  posterior::variables(log_lik) <- log_lik_name

  return(log_lik)
}

##' @rdname joint_log_lik
##' @export
joint_log_lik_CmdStanFit <- function(x, log_lik_name = "log_lik", ...) {

  # sum over correct dimension
  log_lik <- rowSums(x = x$draws(variables = log_lik_name), dims = 2)

  # retain dimensions
  dim(log_lik) <- c(dim(log_lik), 1)

  # back to draws_array
  log_lik <- posterior::as_draws_array(log_lik)
  posterior::variables(log_lik) <- log_lik_name

  return(log_lik)
}
