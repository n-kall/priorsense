##' find alpha value with pareto-k lower than threshold
##' @param x object
##' @param ... additional arguments passed to methods
##' @return numeric value of alpha for which threshold is not reached
##' @srrstats {EA4.1} epsilon controls numeric precision
##' @noRd
##' @srrstats {G3.0} Numeric equality comparisons use appropriate
##'   tolerances for approximate equality.*
find_alpha_threshold <- function(x, ...) {
  UseMethod("find_alpha_threshold")

}

##' @export
find_alpha_threshold.default <- function(x, ...) {

  psd <- create_priorsense_data(x, ...)

  find_alpha_threshold(psd, ...)

}

##' @export
find_alpha_threshold.priorsense_data <- function(x,
                                                 component,
                                                 alpha_bound,
                                                 epsilon = 0.00001,
                                                 moment_match = FALSE,
                                                 selection = NULL,
                                                 ...) {
  checkmate::assert_number(alpha_bound, lower = 0)
  checkmate::assert_number(epsilon, lower = 0)
  checkmate::assert_choice(component, c("prior", "likelihood"))

  if (alpha_bound < 1) {
    lower <- alpha_bound
    upper <- 1 - epsilon
  } else if (alpha_bound > 1) {
    lower <- 1 + epsilon
    upper <- alpha_bound
  }

  if (lower < 1 && upper < 1) {
    comparison <- below_one_comparison
  } else if (lower > 1 && upper > 1) {
    comparison <- above_one_comparison
  }

  pareto_k <- -Inf # set inital low pareto_k
  alpha <- (lower + upper) / 2
  continue <- TRUE

  while (continue) {

    # calculate criterion
    new_pareto_k_diags <- get_powerscaling_details(
      suppressWarnings(
      powerscale(
        x = x,
        alpha = alpha,
        component = component,
        moment_match = moment_match,
        selection = selection,
        ...
      )
      ))$diagnostics

    new_pareto_k <- new_pareto_k_diags$khat
    new_khat_threshold <- new_pareto_k_diags$khat_threshold

    compare <- comparison(new_pareto_k, pareto_k,
                          new_khat_threshold, epsilon)

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

  # be conservative to ensure pareto_k lower
  alpha <- ifelse(alpha > 1, alpha - epsilon, alpha + epsilon)

  return(alpha)
}

##' above one comparison
##' @param new_pareto_k numeric new pareto-k
##' @param pareto_k numeric current pareto-k
##' @param k_threshold numeric k threshold
##' @param epsilon numeric tolerance
##' @return character specifying direction of next value
##' @noRd
above_one_comparison <- function(new_pareto_k,
                                 pareto_k,
                                 k_threshold,
                                 epsilon) {
  if (abs(new_pareto_k - pareto_k) < epsilon) {
    return("stop")
  } else if (new_pareto_k >= k_threshold) {
    return("left")
  } else if (new_pareto_k < k_threshold) {
    return("right")
  }
}

##' below one comparison
##' @param new_pareto_k numeric new pareto-k
##' @param pareto_k numeric current pareto-k
##' @param k_threshold numeric k threshold
##' @param epsilon numeric tolerance
##' @return character specifying direction of next value
##' @noRd
below_one_comparison <- function(new_pareto_k,
                                 pareto_k,
                                 k_threshold,
                                 epsilon) {
  if (abs(new_pareto_k - pareto_k) < epsilon) {
    return("stop")
  } else if (new_pareto_k < k_threshold) {
    return("left")
  } else if (new_pareto_k >= k_threshold) {
    return("right")
  }
}
