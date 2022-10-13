##' Create power-scaling data structure
##'
##' Create a data structure that contains all required data and
##' functions for powerscaling
##' @name create-powerscaling-data
##' @param x a fit object
##' @param get_draws function to extract posterior draws from fit
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
##' @export
create_powerscaling_data <- function(x, ...) {
  UseMethod("create_powerscaling_data")
}

##' @rdname create-powerscaling-data
##' @export
create_powerscaling_data.default <- function(x, get_draws, log_prior,
                                             log_lik,
  #                                           constrain_pars,
 #                                            unconstrain_pars,
#                                             log_prob_upars,
 #                                            log_ratio_upars,
                                             prediction = NULL, ...) {


  # add predictions to draws if specified
  if (!is.null(prediction)) {
    draws <- posterior::bind_draws(
      get_draws(x, ...),
      prediction(x, ...),
      along = "variable"
    )
  } else {
    draws <- get_draws(x)
  }
  psd <- list(
    fit = x,
    get_draws = get_draws,
    draws = draws,
    log_prior = log_prior(x, ...),
    log_lik = log_lik(x, ...),
    log_prior_fn = log_prior,
    log_lik_fn = log_lik#,
 #   constrain_pars = constrain_pars,
#    unconstrain_pars = unconstrain_pars,
#    log_prob_upars = log_prob_upars,
#    log_ratio_upars = log_ratio_upars
  )

  class(psd) <- c("powerscaling_data", class(psd))

  return(psd)
}

##' @rdname create-powerscaling-data
##' @export
create_powerscaling_data.stanfit <- function(x, ...) {

  create_powerscaling_data.default(
    x = x,
    get_draws = get_draws_stanfit,
    log_prior = log_prior_stanfit,
    log_lik = log_lik_stanfit,
    constrain_pars = iwmm::constrain_pars,
#    unconstrain_pars = iwmm::unconstrain_pars,
#    log_prob_upars = log_prob_upars,
#    log_ratio_upars = log_ratio_upars,
    ...
  )
}

##' @rdname create-powerscaling-data
##' @export
create_powerscaling_data.CmdStanFit <- function(x, ...) {

  create_powerscaling_data.default(
    x = x,
    get_draws = get_draws_CmdStanFit,
    log_prior = log_prior_CmdStanFit,
    log_lik = log_lik_CmdStanFit,
    unconstrain_pars = NA,
    constrain_pars = x$constrain_pars,
#    log_prob_upars = NA,
#    log_ratio_upars = NA,
    ...
  )
}
