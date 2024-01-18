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

powerscale_log_ratio_fun <- function(draws, fit, alpha, component, ...) {

  if (component == "prior") {
    component_name = "lprior"
  } else if (component == "likelihood") {
    component_name = "log_lik"
  }

  constr_draws <- iwmm::constrain_draws(fit, draws)

  component_draws <- rowsums_draws(
    posterior::subset_draws(
      constr_draws, variable = component_name
    )
  )

  component_draws * (alpha - 1)
  
}
