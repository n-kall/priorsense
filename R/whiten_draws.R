##' Transform draws to be spherical
##' @param draws draws to be transformed
##' @param ... unused
##' @return transformed draws
whiten_draws <- function(draws, ...) {

  base_draws <- posterior::as_draws_matrix(
    posterior::merge_chains(draws)
  )

  # keep track of weights
  wei <- stats::weights(base_draws)

  # remove weights
  if (!(is.null(wei))) {
    base_draws <- posterior::mutate_variables(
      base_draws,
      .log_weight = NULL)
  }

  # code from whitening package (c) Korbinian Strimmer and Takoua
  # Jendoubi and Agnan Kessy and Alex Lewin
  Sigma <- stats::cov(base_draws)
  v <- diag(Sigma)
  R <- stats::cov2cor(Sigma)
  eR <- eigen(R, symmetric = TRUE)
  G <- eR$vectors
  theta <- eR$values
  G <- sweep(G, 2, sign(diag(G)), "*")
  W <- diag(1 / sqrt(theta)) %*% t(G) %*% diag(1 / sqrt(v))
  draws_tr <- tcrossprod(base_draws, W)
  draws_tr <- sweep(draws_tr, 2, colMeans(draws_tr))

  loadings <- G

  # cleanup transformed draws
  draws_tr <- posterior::as_draws_df(draws_tr)
  posterior::variables(draws_tr) <- paste0(
    "C",
    1:posterior::nvariables(draws_tr)
  )

  # add weights column back
  if (!(is.null(wei))) {
    draws_tr <- posterior::weight_draws(draws_tr, wei)
  }

  colnames(loadings) <- posterior::variables(draws_tr)
  rownames(loadings) <- posterior::variables(base_draws)

  attr(draws_tr, "loadings") <- t(loadings)

  class(draws_tr) <- c("whitened_draws", class(draws_tr))
  
  return(draws_tr)  
}
