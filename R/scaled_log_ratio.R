##' Calculate importance ratios based on scaling of component
##'
##' @param component_draws draws from component to powerscale
##' @param alpha scaling factor
##' @param ... unused
##' @return log ratio
scaled_log_ratio <- function(component_draws, alpha,
                             ...) {

  # calculate log ratios for power-scaling
  scaled <- (alpha - 1) * component_draws
  
  return(scaled)
}
