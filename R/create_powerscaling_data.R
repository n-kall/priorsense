##' @rdname powerscale-overview
##' @export
create_powerscaling_data <- function(x, log_prior_fn,
                                     joint_log_lik_fn, get_draws,
                                     unconstrain_pars, log_prob_upars,
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
