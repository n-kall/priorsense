##' Calculate importance ratios based on scaling of component
##'
##' @param x a model fit
##' @param component component to scale
##' @param alpha scaling factor
##' @param log_prior_fn function to extract log_prior
##' @param joint_log_lik_fn function to extract joint log lik
##' @param ... other arguments
##' @return log ratio
scaled_log_ratio <- function(component_draws, alpha,
                             ...) {

  # calculate log ratios for power-scaling
  return ((alpha - 1) * posterior::as_draws_array(component_draws))

}
