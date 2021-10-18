##' Transform draws to be spherical
##' @param draws draws to be transformed
##' @param method transformation method
##' @param ... unused
##' @return transformed draws
spherize_draws <- function(draws, method = "ZCA-cor", ...) {

  base_draws <- posterior::as_draws_matrix(
    posterior::merge_chains(draws)
  )

  # keep track of weights
  wei <- stats::weights(base_draws)

  # remove weights
  if (!(is.null(wei))) {
    base_draws <- base_draws[, -ncol(base_draws)]
  }

  # center draws
  base_draws_c <- scale(base_draws)

  # calculate whitening matrix
  s <- stats::cov(base_draws_c)
  w <- whitening::whiteningMatrix(Sigma = s, method = method)

  # transform draws
  d1w <- base_draws_c %*% w

  # cleanup transformed draws
  d1w <- posterior::as_draws_df(d1w)
  posterior::variables(d1w) <- posterior::variables(base_draws)

  # add weights column back
  if (!(is.null(wei))) {
    d1w <- posterior::weight_draws(d1w, wei)
  }

  return(d1w)
}

##' Transform matrix to be spherical
##' @param x matrix to be transformed
##' @param method transformation method
##' @param ... unused
##' @return transformed matrix
spherize_matrix <- function(x, method = "ZCA-cor", ...) {

  # center matrix
  x_c <- scale(x)
  
  # calculate whitening matrix
  s <- stats::cov(x_c)
  w <- whitening::whiteningMatrix(Sigma = s, method = method)

  # transform matrix
  d1w <- x_c %*% w

  return(d1w)
}
