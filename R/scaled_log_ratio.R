##' Calculate importance ratios based on scaling of component
##'
##' @param component_draws draws from component to powerscale
##' @param alpha scaling factor
##' @param ... unused
##' @return log ratio
scaled_log_ratio <- function(component_draws, alpha,
                             ...) {

  # calculate log ratios for power-scaling
  scaled <- component_draws * (alpha - 1)
  
  return(scaled)
}

# density ratio function for moment matching
powerscale_log_ratio_fun <- function(draws, fit, alpha, component_fn, ...) {

  constr_draws <- iwmm::constrain_draws(fit, draws)

  component_draws <- rowsums_draws(component_fn(constr_draws))
   
  component_draws * (alpha - 1)
  
}


powerscale_log_ratio_fun_brmsfit <- function(draws, fit, alpha, component_fn, ...) {

  fit <- brms:::.update_pars(x = fit, upars = draws)
  component_draws <- component_fn(fit)

  component_draws <- rowsums_draws(component_draws)
   
  component_draws * (alpha - 1)

}
