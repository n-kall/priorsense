##' Importance sampling effective sample size diagnostic for
##' computing the mean of a parameter
##' @param x vector of values
##' @param log_ratios vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
n_eff_mean <- function(x, log_ratios, ...) {
  lwf_mean <- c(log(weights) + log(abs(x)))
  lwf_mean <- lwf_mean - matrixStats::logSumExp(lwf_mean)
  n_eff <- 1.0 / sum(exp(2 * lwf_mean))

  return(c(n_eff_mean = n_eff))
}

##' Importance sampling effective sample size diagnostic for
##' computing the variance of a parameter
##' @param x vector of values
##' @param weights vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
n_eff_var <- function(x, log_ratios, ...) {
  lwf_var <- c(log(log_ratios) + log(abs(x^2)))
  lwf_var <- lwf_var - matrixStats::logSumExp(lwf_var)
  n_eff <- 1.0 / sum(exp(2 * lwf_var))

  return(c(n_eff_var = n_eff))
}

##' Pareto-k diagnostic for computing the mean of a parameter
##' @param x vector of values
##' @param log_ratios vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
pareto_k_mean <- function(x, log_ratios, ...) {
  psis_f <- SW(loo::psis(
    log_ratios = c(log(log_ratios) + log(abs(x))),
    r_eff = 1)
    )
  return(c(pareto_k_mean = psis_f$diagnostics$pareto_k))
}

##' Pareto-k diagnostic for computing the variance of a parameter
##' @param x vector of values
##' @param log_ratios vector of standard importance sampling weights
##' @param ... unused
##' @return numeric
pareto_k_var <- function(x, log_ratios, ...) {
  psis_f <- SW(loo::psis(
    log_ratios = c(log(log_ratios) + log(abs(x^2))),
    r_eff = 1)
    )
  return(c(pareto_k_var = psis_f$diagnostics$pareto_k))
}
