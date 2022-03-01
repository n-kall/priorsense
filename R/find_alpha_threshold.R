##' Find the value of alpha furthest from 1 which gives the specified
##' pareto-k value
##'
##' @param fit a model fit of class `brmsfit`
##' @param variables variables to report
##' @param component string specifying which component to investigate,
##'   `likelihood` or `prior`.
##' @param range vector of length 2 specifying the range of alphas to
##'   consider, both values must be less than 1 or greater than 1
##' @param k_threshold highest acceptable pareto-k value
##' @param epsilon value to define differences
##' @param moment_match moment match
##' @return numeric
##' @export
find_alpha_threshold <- function(fit,
                                 component,
                                 range,
                                 variables = NA,
                                 k_threshold = 0.5,
                                 epsilon = 0.001,
                                 moment_match = FALSE) {
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
        fit = fit,
        variables = variables,
        alpha = alpha,
        component = component,
        is_method = "psis",
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
