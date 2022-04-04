##' Calculate importance ratios based on scaling of component
##'
##' @param component_draws draws from component to powerscale
##' @param alpha scaling factor
##' @param ... unused
##' @return log ratio
scaled_log_ratio <- function(component_draws, alpha,
                             ...) {

  # calculate log ratios for power-scaling
  return ((alpha - 1) * posterior::as_draws_array(component_draws))

}
