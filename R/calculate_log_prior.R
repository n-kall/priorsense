##' Log prior calculation
##'
##' Log prior calculation by subtracting log likelihood from log joint
##' density.
##'
##' @name calculate_log_prior
##' @param x Fitted model object.
##' @param ... Currently unused.
##' @return A draws_array object containing calculated log_prior values.
NULL

##' @rdname calculate_log_prior
##' @export
calculate_log_prior <- function(x, ...) {
  UseMethod("calculate_log_prior")
}

##' @rdname calculate_log_prior
##' @export
calculate_log_prior.stanfit <- function(x, ...) {
  post_draws <- posterior::merge_chains(
    posterior::as_draws_array(x, ...)
  )

  upars <- unconstrain_pars(x, post_draws)
  lp <- log_prob_pars(x, upars)

  log_prior <- lp - extract_joint_log_lik(x)

  posterior::variables(log_prior) <- "log_prior"

  return(posterior::merge_chains(log_prior))
}

##' @rdname calculate_log_prior
##' @export
calculate_log_prior.brmsfit <- function(x, ...) {
  post_draws <- posterior::merge_chains(
    posterior::as_draws_array(x$fit, ...)
  )

  upars <- unconstrain_pars(x$fit, post_draws)
  lp <- log_prob_pars(x$fit, upars)

  log_prior <- lp - extract_joint_log_lik(x)

  posterior::variables(log_prior) <- "log_prior"

  return(posterior::merge_chains(log_prior))
}

# compute log_prob for each posterior draw on the constrained space
log_prob_pars <- function(x, ...) {
  UseMethod("log_prob_pars")
}

log_prob_pars.stanfit <- function(x, upars, ...) {

  if (!requireNamespace("rstan", quietly = TRUE))
    stop("Please load the 'rstan' package.", call. = FALSE)
  
  apply(upars, 1, rstan::log_prob,
    object = x,
    adjust_transform = FALSE, gradient = FALSE
  )
}

log_prob_pars.brmsfit <- function(x, upars, ...) {
  log_prob_pars.stanfit(x$fit, upars = upars, ...)
}
