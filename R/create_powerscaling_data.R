##' Create power-scaling data structure
##'
##' Create a data structure that contains all required data and
##' functions for powerscaling
##' @name create-powerscaling-data
##' @param x a fit object
##' @param draws posterior draws
##' @param log_prior draws from log prior
##' @param log_lik draws from log likelihood
##' @param constrain_pars function that transforms unconstrained
##'   parameters to constrained space
##' @param unconstrain_pars function that transforms constrained
##'   parameters to unconstrained space
##' @param log_prob_upars function that returns log density given
##'   unconstrained parameter values
##' @param log_ratio_upars function that returns log density ratio for
##'   importance sampling given unconstrained parameter values
##' @param prediction function that returns predictions from x to be
##'   added to the draws
##' @param ... arguments passed to methods
##' @return A `powerscaling_data` object, which contains the data and
##'   functions to run power-scaling sensitivity analyses.
create_powerscaling_data <- function(x, ...) {
  UseMethod("create_powerscaling_data")
}

##' @rdname create-powerscaling-data
##' @export
create_powerscaling_data.default <- function(x, draws, log_prior,
                                             log_lik,
                                             constrain_pars,
                                             unconstrain_pars,
                                             log_prob_upars,
                                             log_ratio_upars,
                                             prediction = NULL, ...) {


  # add predictions to draws if specified
  if (!is.null(prediction)) {
    draws <- posterior::bind_draws(draws(x), prediction(x), along = "variable")
  }
  psd <- list(
    fit = x,
    draws = draws(x),
    log_prior = log_prior(x),
    log_lik = log_lik(x),
    get_draws = draws,
    log_prior_fn = log_prior,
    log_lik_fn = log_lik,
    constrain_pars = constrain_pars,
    unconstrain_pars = unconstrain_pars,
    log_prob_upars = log_prob_upars,
    log_ratio_upars = log_ratio_upars
  )

  class(psd) <- c("powerscaling_data", class(psd))

  return(psd)
}

##' @rdname create-powerscaling-data
##' @export
create_powerscaling_data.stanfit <- function(x, ...) {

  create_powerscaling_data.default(
    x = x,
    draws = get_draws_stanfit,
    log_prior = log_prior_stanfit,
    log_lik = joint_log_lik_stanfit,
    constrain_pars = rstan::constrain_pars,
    unconstrain_pars = unconstrain_pars,
    log_prob_upars = log_prob_upars,
    log_ratio_upars = log_ratio_upars,
    ...
  )
}

##' @rdname create-powerscaling-data
##' @export
create_powerscaling_data.CmdStanFit <- function(x, ...) {
  
  create_powerscaling_data.default(
    x = x,
    draws = get_draws_CmdStanFit,
    log_prior = log_prior_CmdStanFit,
    log_lik = joint_log_lik_CmdStanFit,
    unconstrain_pars = NULL,
    constrain_pars = NULL,
    log_prob_upars = NULL,
    log_ratio_upars = NULL,
    ...
  )
}

##' @rdname create-powerscaling-data
#' @export
create_powerscaling_data.brmsfit <- function(x, ...) {
  
  create_powerscaling_data.default(
    x = x,
    log_prior = log_prior_brmsfit,
    log_lik = joint_log_lik_brmsfit,
    draws = get_draws_brmsfit,
    unconstrain_pars = unconstrain_pars,
    log_prob_upars = log_prob_upars,
    log_ratio_upars = log_ratio_upars,
    ...
  )
}
