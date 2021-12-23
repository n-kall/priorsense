##' Transform draws to be spherical
##' @param draws draws to be transformed
##' @param whitening_method transformation method
##' @param ... unused
##' @return transformed draws
whiten_draws <- function(draws, whitening_method = "PCA-cor", ...) {

  base_draws <- posterior::as_draws_matrix(
    posterior::merge_chains(draws)
  )

  # keep track of weights
  wei <- stats::weights(base_draws)

  # remove weights
  if (!(is.null(wei))) {
    base_draws <- base_draws[, -ncol(base_draws)]
  }

  Sigma <- cov(scale(base_draws))
  v <- diag(Sigma)
  R <- cov2cor(Sigma)
  eR <- eigen(R, symmetric = TRUE)
  G <- eR$vectors
  loadings <- G
  
  draws_tr <- whitening::whiten(
    base_draws,
    center = TRUE,
    method = whitening_method
    )
  
  # cleanup transformed draws
  draws_tr <- posterior::as_draws_df(draws_tr)
  posterior::variables(draws_tr) <- paste0("C", 1:posterior::nvariables(draws_tr))

  # add weights column back
  if (!(is.null(wei))) {
    draws_tr <- posterior::weight_draws(draws_tr, wei)
  }

  colnames(loadings) <- variables(draws_tr)
  rownames(loadings) <- variables(base_draws)
  
  return(list(draws = draws_tr, loadings = t(loadings)))
}

##' Transform matrix to be spherical
##' @param x matrix to be transformed
##' @param whitening_method transformation method
##' @param ... unused
##' @return transformed matrix
whiten_matrix <- function(x, whitening_method = "PCA-cor", ...) {

  # transform matrix
  m_tr <- whitening::whiten(
    x,
    center = TRUE,
    method = whitening_method
  )

  return(m_tr)
}
