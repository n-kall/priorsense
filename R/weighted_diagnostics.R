##' Importance sampling effective sample size diagnostic for
##' computing the mean of a parameter
##' @param x vector of values
##' @param log_ratios vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
##' @noRd
ess_mean <- function(x, log_ratios, ...) {
  lwf_mean <- c(log(log_ratios) + log(abs(x)))
  lwf_mean <- lwf_mean - matrixStats::logSumExp(lwf_mean)
  ess <- 1.0 / sum(exp(2 * lwf_mean))

  return(c(ess_mean = ess))
}

##' Importance sampling effective sample size diagnostic for
##' computing the variance of a parameter
##' @param x vector of values
##' @param log_ratios vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
##' @noRd
ess_var <- function(x, log_ratios, ...) {
  lwf_var <- c(log(log_ratios) + log(abs(x^2)))
  lwf_var <- lwf_var - matrixStats::logSumExp(lwf_var)
  ess <- 1.0 / sum(exp(2 * lwf_var))

  return(c(ess_var = ess))
}

##' Pareto-k diagnostic for computing the mean of a parameter
##' @param x vector of values
##' @param log_ratios vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
##' @noRd
pareto_k_mean <- function(x, log_ratios, ...) {
  pareto_k_mean <- posterior::pareto_khat(
    x = c(log(log_ratios) + log(abs(x))),
    are_log_weights = TRUE
    )
  return(c(pareto_k_mean = pareto_k_mean))
}

##' Pareto-k diagnostic for computing the variance of a parameter
##' @param x vector of values
##' @param log_ratios vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
##' @noRd
pareto_k_var <- function(x, log_ratios, ...) {
  pareto_k_var <- posterior::pareto_khat(
    x = c(log(log_ratios) + log(abs(x^2))),
    are_log_weights = TRUE
    )
  return(c(pareto_k_var = pareto_k_var))
}
