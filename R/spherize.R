##' Transform draws to be spherical
##' @param draws draws to be transformed
##' @param method transformation method
##' @param ... unused
##' @return transformed draws
spherize_draws <- function(draws, spherize_method = "PCA-cor", ...) {

  base_draws <- posterior::as_draws_matrix(
    posterior::merge_chains(draws)
  )

  # keep track of weights
  wei <- stats::weights(base_draws)

  # remove weights
  if (!(is.null(wei))) {
    base_draws <- base_draws[, -ncol(base_draws)]
  }

  draws_tr <- whitening::whiten(
    base_draws,
    center = TRUE,
    method = spherize_method
  )
  
  # cleanup transformed draws
  draws_tr <- posterior::as_draws_df(draws_tr)
  posterior::variables(draws_tr) <- paste0("C", 1:posterior::nvariables(draws_tr))

  # add weights column back
  if (!(is.null(wei))) {
    draws_tr <- posterior::weight_draws(draws_tr, wei)
  }

  return(draws_tr)
}

##' Transform matrix to be spherical
##' @param x matrix to be transformed
##' @param method transformation method
##' @param ... unused
##' @return transformed matrix
spherize_matrix <- function(x, spherize_method = "PCA", ...) {

  # transform matrix
  m_tr <- whitening::whiten(
    x,
    center = TRUE,
    method = spherize_method
  )

  return(m_tr)
}
