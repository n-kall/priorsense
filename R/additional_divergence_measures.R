##' Multivariate Kullback-Leibler divergence
##'
##' @param weights importance weights (unnormalized)
##' @param ... unused
##' @noRd
##' @srrstats {G2.13} checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms
##' @srrstats {G2.2} Input is checked that it is numeric vector and excludes matrix
mv_kl_div <- function(weights, ...) {

  checkmate::assert_numeric(weights, any.missing = FALSE)
  
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
##' @srrstats {G2.2} Input is checked that it is numeric vector and excludes matrix
##' @noRd
mv_wasserstein_dist <- function(draws1,
                                draws2,
                                weights1 = NULL,
                                weights2 = NULL,
                                subsample_size = 100,
                                ...
                                ) {

  checkmate::assert_class(draws1, "draws")
  checkmate::assert_class(draws2, "draws")
  checkmate::assert_numeric(weights1, len = posterior::ndraws(draws1), null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(weights2, len = posterior::ndraws(draws2), null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_vector(weights1, null.ok = TRUE)
  checkmate::assert_vector(weights2, null.ok = TRUE)
  checkmate::assert_number(subsample_size, lower = 0)

  require_package("transport")
  
  if (is.null(weights1)) {
    weights1 <- rep(
      1 / posterior::ndraws(draws1),
      times = posterior::ndraws(draws1)
    )
    draws1 <- posterior::weight_draws(x = draws1, weights = weights1)
  }

  if (is.null(weights2)) {
    weights2 <- rep(
      1 / posterior::ndraws(draws2),
      times = posterior::ndraws(draws2)
    )
    draws2 <- posterior::weight_draws(x = draws2, weights = weights2)
  }

  d1 <- transport::wpp(
    posterior::as_draws_matrix(
      x = draws1,
      merge_chains = TRUE
    )[, 1:posterior::nvariables(draws1)],
    mass = weights1
  )
  d2 <- transport::wpp(
    posterior::as_draws_matrix(
      x = draws2,
      merge_chains = TRUE
    )[, 1:posterior::nvariables(draws2)],
    mass = weights2
  )

  dist <- transport::subwasserstein(d1, d2, S = subsample_size)

  return(dist)
}

##' Jensen-Shannon divergence
##'
##' @template draws_and_weights_arg
##' @param ... unused
##' @return numeric
##' @srrstats {G2.2} Input is checked that it is numeric vector and excludes matrix
##' @noRd
js_div <- function(x, y, x_weights = NULL, y_weights = NULL, ...) {
  
  require_package("philentropy")

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(x, strict = TRUE)
  
  checkmate::assert_numeric(y, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(y, strict = TRUE)

  checkmate::assert_numeric(x_weights, len = length(x), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(y_weights, len = length(y), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  
  y_density <- stats::density(
    x = y,
    from = min(c(x, y)),
    to = max(c(x, y)),
    weights = y_weights
  )$y
  y_density <- y_density / sum(y_density)

  x_density <- stats::density(
    x = x,
    from = min(c(x, y)),
    to = max(c(x, y)),
    weights = x_weights
  )$y

  x_density <- x_density / sum(x_density)

  div <- philentropy::jensen_shannon(
    P = x_density,
    Q = y_density,
    testNA = FALSE,
    unit = "log2"
  )

  return(c(js_div = div))
}

##' Jensen-Shannon distance
##'
##' @template draws_and_weights_arg
##' @param ... unused
##' @return numeric
##' @noRd
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
##' @template draws_and_weights_arg
##' @param ... unused
##' @return numeric
##' @srrstats {G2.2} Input is checked that it is numeric vector and excludes matrix
##' @noRd
hellinger_dist <- function(x, y, x_weights = NULL, y_weights = NULL, ...) {
  
  require_package("philentropy")

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(x, strict = TRUE)
  
  checkmate::assert_numeric(y, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(y, strict = TRUE)

  checkmate::assert_numeric(x_weights, len = length(x), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(y_weights, len = length(y), null.ok = TRUE, any.missing = FALSE, finite = TRUE)

  
  y_density <- stats::density(
    x = y,
    from = min(c(x, y)),
    to = max(c(x, y)), weights = y_weights
  )$y
  y_density <- y_density / sum(y_density)

  x_density <- stats::density(
    x = x,
    from = min(c(x, y)),
    to = max(c(x, y)),
    weights = x_weights
  )$y
  x_density <- x_density / sum(x_density)

  div <- philentropy::hellinger(
    P = x_density,
    Q = y_density,
    testNA = FALSE
  )

  return(c(hellinger_dist = div))
}

##' Kullback-Leibler divergence
##'
##' @template draws_and_weights_arg
##' @param ... unused
##' @return numeric value of approximate KL(p_x||p_y) based on
##'   estimated densitites
##' @srrstats {G2.2} Input is checked that it is numeric vector and excludes matrix
##' @noRd
kl_div <- function(x, y, x_weights = NULL, y_weights = NULL, ...) {

  require_package("philentropy")
  
  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(x, strict = TRUE)
  
  checkmate::assert_numeric(y, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(y, strict = TRUE)

  checkmate::assert_numeric(x_weights, len = length(x), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(y_weights, len = length(y), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  
  y_density <- stats::density(
    x = y,
    from = min(c(x, y)),
    to = max(c(x, y)),
    weights = y_weights
  )$y
  y_density <- y_density / sum(y_density)

  x_density <- stats::density(
    x = x,
    from = min(c(x, y)),
    to = max(c(x, y)),
    weights = x_weights
  )$y
  x_density <- x_density / sum(x_density)

  div <- philentropy::kullback_leibler_distance(
    P = x_density,
    Q = y_density,
    testNA = FALSE,
    unit = "log",
    epsilon = 1e-05
  )

  return(c(kl_div = div))
}

##' Kullback-Leibler distance
##'
##' @template draws_and_weights_arg
##' @param ... unused
##' @return numeric value of approximate KL(p_x||p_y) based on
##'   estimated densitites
##' @noRd
kl_dist <- function(x, y, x_weights = NULL, y_weights = NULL, ...) {
  
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
##' @template draws_and_weights_arg
##' @param ... ununsed
##' @return numeric value of Kolmogorov Smirnov distance
##' @srrstats {G2.2} Input is checked that it is numeric vector and excludes matrix
##' @noRd
ks_dist <- function(x, y, x_weights = NULL, y_weights = NULL, ...) {

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(x, strict = TRUE)
  
  checkmate::assert_numeric(y, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(y, strict = TRUE)

  checkmate::assert_numeric(x_weights, len = length(x), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(y_weights, len = length(y), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  
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
##' @template draws_and_weights_arg
##' @param p degree
##' @param ... unused
##' @return numeric value of Wassterstein distance
##' @srrstats {G2.2} Input is checked that it is numeric vector and excludes matrix
##' @noRd
ws_dist <- function(x, y, x_weights = NULL, y_weights = NULL, p = 1, ...) {

  require_package("transport")
  
  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(x, strict = TRUE)
  
  checkmate::assert_numeric(y, min.len = 1, any.missing = FALSE, finite = TRUE)
  checkmate::assert_vector(y, strict = TRUE)

  checkmate::assert_numeric(x_weights, len = length(x), null.ok = TRUE, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(y_weights, len = length(y), null.ok = TRUE, any.missing = FALSE, finite = TRUE)

  checkmate::assert_number(p, lower = 0)
  
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
