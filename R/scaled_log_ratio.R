##' Calculate importance ratios based on scaling of component
##'
##' @param x a model fit
##' @param component component to scale
##' @param alpha scaling factor
##' @param log_prior_fn function to extract log_prior
##' @param joint_log_lik_fn function to extract joint log lik
##' @param ... other arguments
##' @return log ratio
scaled_log_ratio <- function(x, component, alpha,
                             log_prior_fn,
                             joint_log_lik_fn,
                             ...) {

  if (component == "prior") {
    # calculate log ratios for prior scaling
    log_prior <- log_prior_fn(x, ...)
    log_ratio <- (alpha - 1) * log_prior

  } else if (component == "likelihood") {
    # calculate log ratios for likelihood scaling
    log_lik <- joint_log_lik_fn(x, ...)
    log_ratio <- (alpha - 1) * log_lik
  }
  return(log_ratio)
}
