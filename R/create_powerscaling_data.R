create_powerscaling_data <- function(x, ...) {
  UseMethod("create_powerscaling_data")
}

create_powerscaling_data.default <- function(x, draws, log_prior,
                                             log_lik,
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
    unconstrain_pars = unconstrain_pars,
    log_prob_upars = log_prob_upars,
    log_ratio_upars = log_ratio_upars
  )

  class(psd) <- c("powerscaling_data", class(psd))

  return(psd)
}

create_powerscaling_data.stanfit <- function(x, ...) {

  create_powerscaling_data.default(
    fit = x,
    draws = get_draws_stanfit,
    log_prior = log_prior_stanfit,
    log_lik = joint_log_lik_stanfit,
    unconstrain_pars = unconstrain_pars,
    log_prob_upars = log_prob_upars,
    log_ratio_upars = log_ratio_upars,
    ...
  )
}

create_powerscaling_data.CmdStanFit <- function(x, ...) {
  
  create_powerscaling_data.default(
    x = x,
    draws = get_draws_CmdStanFit,
    log_prior = log_prior_CmdStanFit,
    log_lik = joint_log_lik_CmdStanFit,
    unconstrain_pars = NULL,
    log_prob_upars = NULL,
    log_ratio_upars = NULL,
    ...
  )
}

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
