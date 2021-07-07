all_divergence_measures <- function() {
  c(
    "cjs_dist",
    "js_dist",
    "hellinger_dist",
    "kl_dist",
    "js_div",
    "kl_div",
    "ws_dist",
    "ks_dist"
  ) 
}


##' Calculate specified divergence measures for each posterior
##'
##' @param draws1 draws of first distribution
##' @param draws2 draws of second distribution
##' @param measure divergence measure
##' @param measure_args arguments for divergence measure
##' @param ... unused
##' @return a tibble
divergence_measures <- function(draws1, draws2,
                                measure,
                                measure_args = list(),
                                ...) {

  weights1 <- stats::weights(draws1, log = FALSE)
  weights2 <- stats::weights(draws2, log = FALSE)


  variables <- posterior::variables(draws1)

  out <- tibble::as_tibble_col(variables, "variable")
  
  for (m in measure) {
    divs <- numeric(length(variables))
    names(divs) <- variables
    for (v in variables) {

      args <- c(
        list(
          x = draws1[[v]],
          y = draws2[[v]],
          x_weights = weights1,
          y_weights = weights2
        ),
        measure_args
      )
        
      divs[v] <- do.call(
        what = m,
        args = args
      )
    }
    divs <- tibble::as_tibble_col(divs, column_name = m)
    out <- cbind(out, divs)
  }

  return(out)
}

##' Jensen-Shannon divergence
##'
##' @param x Samples from first distribution
##' @param y Samples from second distribution
##' @param x_weights Weights of first distribution
##' @param y_weights Weights of second distribution
##' @param ... unused
##' @return numeric
js_div <- function(x, y, x_weights, y_weights, ...) {

  y_density <- stats::density(y, from = min(c(x, y)), to = max(c(x, y)), weights = y_weights)$y
  y_density <- y_density / sum(y_density)

  x_density <- stats::density(x, from = min(c(x, y)), to = max(c(x, y)), weights = x_weights)$y
  x_density <- x_density / sum(x_density)

  div <- philentropy::jensen_shannon(x_density, y_density, testNA = FALSE, unit = "log2")

  # TODO: consider removing the names of the output
  return(c(js_div = div))
}

##' Jensen-Shannon distance
##'
##' @param x Samples from first distribution
##' @param y Samples from second distribution
##' @param x_weights Weights of first distribution
##' @param y_weights Weights of second distribution
##' @param ... unused
##' @return numeric
js_dist <- function(x, y, x_weights, y_weights, ...) {

  dist <- sqrt(
    js_div(
      x = x,
      y = y,
      x_weights = x_weights,
      y_weights = y_weights
    )[[1]])

  return(c(js_dist = dist))

}

##' Hellinger distance
##'
##' @param x Samples from first distribution
##' @param y Samples from second distribution
##' @param x_weights Weights of first distribution
##' @param y_weights Weights of second distribution
##' @param ... unused
##' @return numeric
hellinger_dist <- function(x, y, x_weights, y_weights, ...) {

  y_density <- stats::density(y, from = min(c(x, y)), to = max(c(x, y)), weights = y_weights)$y
  y_density <- y_density / sum(y_density)

  x_density <- stats::density(x, from = min(c(x, y)), to = max(c(x, y)), weights = x_weights)$y
  x_density <- x_density / sum(x_density)

  div <- philentropy::hellinger(x_density, y_density, testNA = FALSE)

  # TODO: consider removing the names of the output
  return(c(hellinger_dist = div))
}



##' Kullback-Leibler divergence
##'
##' @param x Samples from first distribution
##' @param y Samples from second distribution
##' @param x_weights Weights of first distribution
##' @param y_weights Weights of second distribution
##' @param ... unused
##' @return numeric value of approximate KL(p_x||p_y) based on
##'   estimated densitites
kl_div <- function(x, y, x_weights, y_weights, ...) {

  y_density <- stats::density(y, from = min(c(x, y)), to = max(c(x, y)), weights = y_weights)$y
  y_density <- y_density / sum(y_density)

  x_density <- stats::density(x, from = min(c(x, y)), to = max(c(x, y)), weights = x_weights)$y
  x_density <- x_density / sum(x_density)

  div <- philentropy::kullback_leibler_distance(x_density, y_density, testNA = FALSE, unit = "log")

  return(c(kl_div = div))
}

##' Kullback-Leibler distance
##'
##' @param x Samples from first distribution
##' @param y Samples from second distribution
##' @param x_weights Weights of first distribution
##' @param y_weights Weights of second distribution
##' @param ... unused
##' @return numeric value of approximate KL(p_x||p_y) based on
##'   estimated densitites
kl_dist <- function(x, y, x_weights, y_weights, ...) {

  dist <- sqrt(
    kl_div(
      x = x,
      y = y,
      x_weights = x_weights,
      y_weights = y_weights,
      ...
    )[[1]])
  
  return(c(kl_dist = dist))
}



##' Kolmogorov-Smirnov distance
##' @param x vector
##' @param y vector
##' @param x_weights vector of weights
##' @param y_weights vector of weights
##' @param ... ununsed
##' @return numeric value of Kolmogorov Smirnov distance
ks_dist <- function(x, y, x_weights, y_weights, ...) {

  if (is.null(x_weights)) {
    x_weights <- rep(1, length(x))
  }
  if (is.null(y_weights)) {
    y_weights <- rep(1, length(x))
  }

  ks <- stats::ks.test(
    y = y * y_weights,
    x = x * x_weights
  )$statistic

  return(c(ks_dist = ks))
}
##' Wasserstein distance
##'
##' @param x vector
##' @param y vector
##' @param x_weights vector of weights
##' @param y_weights vector of weights
##' @param p degree
##' @param ... unused
##' @return numeric value of Wassterstein distance
ws_dist <- function(x, y, x_weights, y_weights, p = 1, ...) {

  wa <- transport::wasserstein1d(
    a = x,
    b = y,
    p = p,
    wa = x_weights,
    wb = y_weights
  )

  names(wa) <- paste0("wasserstein", p)
  
  return(c(wasserstein = wa))
}
