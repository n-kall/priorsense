##' Find the value of alpha furthest from 1 which gives the specified
##' pareto-k value
##'
##' @param x a fit object
##' @param component string specifying which component to investigate,
##'   `likelihood` or `prior`.
##' @param range vector of length 2 specifying the range of alphas to
##'   consider, both values must be less than 1 or greater than 1
##' @param k_threshold highest acceptable pareto-k value
##' @param epsilon value to define differences
##' @param moment_match moment match
##' @param ... other arguments passed to powerscale
##' @return numeric alpha value furthest from 1 that leads to pareto-k
##'   below threshold
##' @export
##' 
find_alpha_threshold <- function(x, ...) {
  UseMethod("find_alpha_threshold")

}

##' @export
find_alpha_threshold.CmdStanFit <- function(x, ...) {

  psd <- create_powerscaling_data(x)

  find_alpha_threshold(psd, ...)

}

##' @export
find_alpha_threshold.stanfit <- function(x, ...) {

  psd <- create_powerscaling_data(x)

  find_alpha_threshold(psd, ...)

}

##' @export
find_alpha_threshold.powerscaling_data <- function(x, ...) {

  find_alpha_threshold.default(
    x = x$fit,
    log_prior_fn = x$log_prior_fn,
    joint_log_lik_fn = x$joint_log_lik_fn,
    get_draws = x$get_draws,
    unconstrain_pars = x$unconstrain_pars,
    log_prob_upars = x$log_prob_upars,
    log_ratio_upars = x$log_ratio_upars,
    ...
  )

}

##' @export
find_alpha_threshold.default <- function(x,
                                 component,
                                 range,
                                 log_prior_fn,
                                 joint_log_lik_fn,
                                 get_draws,
                                 unconstrain_pars,
                                 log_prob_upars,
                                 log_ratio_upars,
                                 variables = NA,
                                 k_threshold = 0.5,
                                 epsilon = 0.001,
                                 moment_match = FALSE, ...) {
  checkmate::assert_number(epsilon)
  checkmate::assert_number(k_threshold)
  checkmate::assert(epsilon > 0)
  checkmate::assert_vector(range)
  checkmate::assert_choice(component, c("prior", "likelihood"))

  lower <- range[1]
  upper <- range[2]

  if (lower < 1 & upper < 1) {
    comparison <- below_one_comparison
  } else if (lower > 1 & upper > 1) {
    comparison <- above_one_comparison
  }

  pareto_k <- -Inf # set inital low pareto_k
  alpha <- (lower + upper) / 2
  continue <- TRUE

  while (continue) {
    
    # calculate criterion
    new_pareto_k <- suppressWarnings(
      powerscale(
        x = x,
        alpha = alpha,
        component = component,
        moment_match = moment_match
      )
    )$powerscaling$importance_sampling$diagnostics$pareto_k

    compare <- comparison(new_pareto_k, pareto_k, k_threshold, epsilon)

    # check criterion
    if (compare == "left") {
      upper <- alpha
      alpha <- (lower + alpha) / 2
    } else if (compare == "right") {
      lower <- alpha
      alpha <- (alpha + upper) / 2
    } else if (compare == "stop") {
      continue <- FALSE
    }
    pareto_k <- new_pareto_k
  }

  return(alpha)
}

above_one_comparison <- function(new_pareto_k, pareto_k, k_threshold, epsilon) {
  if (abs(new_pareto_k - pareto_k) < epsilon) {
    return("stop")
  } else if (new_pareto_k >= k_threshold) {
    return("left")
  } else if (new_pareto_k < k_threshold) {
    return("right")
  }
}

below_one_comparison <- function(new_pareto_k, pareto_k, k_threshold, epsilon) {
  if (abs(new_pareto_k - pareto_k) < epsilon) {
    return("stop")
  } else if (new_pareto_k < k_threshold) {
    return("left")
  } else if (new_pareto_k >= k_threshold) {
    return("right")
  }
}
