create_powerscaling_data <- function(x, ...) {
  UseMethod("create_powerscaling_data")

}

create_powerscaling_data.default <- function(x, log_prior_fn,
                                             joint_log_lik_fn,
                                             get_draws,
                                             unconstrain_pars,
                                             log_prob_upars,
                                             log_ratio_upars) {

  psd <- list(
    fit = x,
    log_prior_fn = log_prior_fn,
    joint_log_lik_fn = joint_log_lik_fn,
    get_draws = get_draws,
    unconstrain_pars = unconstrain_pars,
    log_prob_upars = log_prob_upars,
    log_ratio_upars = log_ratio_upars
  )

  class(psd) <- c("powerscaling_data", class(psd))

  return(psd)
}

create_powerscaling_data.stanfit <- function(x, ...) {

  create_powerscaling_data.default(
    x = x,
    log_prior_fn = log_prior_stanfit,
    joint_log_lik_fn = joint_log_lik_stanfit,
    get_draws = get_draws_stanfit,
    unconstrain_pars = unconstrain_pars,
    log_prob_upars = log_prob_upars,
    log_ratio_upars = log_ratio_upars
  )
}

create_powerscaling_data.CmdStanFit <- function(x, ...) {

  create_powerscaling_data.default(
    x = x,
    log_prior_fn = log_prior_CmdStanFit,
    joint_log_lik_fn = joint_log_lik_CmdStanFit,
    get_draws = get_draws_CmdStanFit,
    unconstrain_pars = NULL,
    log_prob_upars = NULL,
    log_ratio_upars = NULL
  )
}
