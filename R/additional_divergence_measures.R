##' Multivariate Kullback-Leibler divergence
##'
##' @param weights importance weights (unnormalized)
##' @param ... unused
mv_kl_div <- function(weights, ...) {
  
  return(-mean(log(weights, base = 2)))
  
}

###' Multivariate Wasserstein distance
###'
##' @param draws1 draws from first distribution
##' @param draws2 draws from second distribution
##' @param weights1 weights for first distribution
##' @param weights2 weights for second distribution
##' @param subsample_size size of subsamples
##' @param ... unused
mv_wasserstein_dist <- function(draws1,
                                   draws2,
                                   weights1 = NULL,
                                   weights2 = NULL,
                                   subsample_size = 100,
                                   ...
                                   ) {

  if (is.null(weights1)) {
    weights1 <- rep(
      1/posterior::ndraws(draws1),
      times = posterior::ndraws(draws1)
    )
    draws1 <- posterior::weight_draws(x = draws1, weights = weights1)
  }

  if (is.null(weights2)) {
    weights2 <- rep(
      1/posterior::ndraws(draws2),
      times = posterior::ndraws(draws2)
    )
    draws2 <- posterior::weight_draws(x = draws2, weights = weights2)
  }

  d1 <- transport::wpp(
    posterior::as_draws_matrix(draws1, merge_chains = TRUE)[, 1:posterior::nvariables(draws1)],
    mass = weights1
  )
  d2 <- transport::wpp(
    posterior::as_draws_matrix(draws2, merge_chains = TRUE)[, 1:posterior::nvariables(draws2)],
    mass = weights2
  )

  dist <- transport::subwasserstein(d1, d2, S = subsample_size)

  return(dist)

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

  div <- philentropy::kullback_leibler_distance(x_density, y_density, testNA = FALSE, unit = "log", epsilon = 1e-05)

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
