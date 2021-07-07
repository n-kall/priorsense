##' Derivative of posterior quantity with respect to power-scaling
##'
##' Calculate the derivative of a posterior quantity with respect to
##' power-scaling the prior or likelihood.
##'
##' @param x Vector of draws.
##' @param log_component Vector of log likelihood or log prior values.
##' @param quantity Character specifying quantity of interest. "mean",
##'   "sd" and "var" are currently implemented.
##' @return Derivative of the quantity with respect to the
##'   power-scaling factor.
##' @template powerscale_references
##' @export
powerscale_derivative <- function(x,
                                  log_component,
                                  quantity = "mean") {

  if (quantity %in% c("median","mad","q5","q95")) {
    out <- NA
    warning("Power-scaling derivative for medians or quantiles is zero. Consider using powerscale_gradients instead.")

  }

  # adapted from method by Topi Paananen

  else if (quantity == "mean") {

    deriv_first_moment <- mean(x * log_component) -
      mean(x) * mean(log_component)

    #    deriv_first_moment <- sum(x * log_component)/length(x) -
    #      sum(x)/length(x) * sum(log_component)/length(x)

    # wrt log_2(alpha)
    out <- log(2) * deriv_first_moment
    
  } else if (quantity == "sd") {

    first_moment <- mean(x)
    second_moment <- mean(x^2)

    deriv_first_moment <- mean(x * log_component) -
      mean(x) * mean(log_component)

    deriv_second_moment <- mean(x^2 * log_component) -
      mean(x^2) * mean(log_component)

    out <- log(2) * ((deriv_second_moment - 2 * deriv_first_moment * first_moment) *
                       0.5 / sqrt(second_moment - first_moment^2))

  } else if (quantity == "var") {

    first_moment <- mean(x)

    deriv_first_moment <- mean(x * log_component) -
      mean(x) * mean(log_component)

    deriv_second_moment <- mean(x^2 * log_component) -
      mean(x^2) * mean(log_component)
    
    out <- log(2) * (deriv_second_moment - 2 * deriv_first_moment * first_moment)
  }

  names(out) <- paste0("ps_", quantity, "_deriv")
  
  return(out)
}
