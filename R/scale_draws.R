##' scale draws
##' @param draws draws object
##' @param center boolean flag indicating whether or not to center draws
##' @param scale boolean flag indicating whether or not to scale draws by standard deviation
##' @param ... unused
##' @return draws object with variables centered and/or scaled
scale_draws <- function(draws, center = TRUE, scale = TRUE, ...) {

  draws <- posterior::as_draws_matrix(draws)

  # keep track of weights
  wei <- stats::weights(draws)

  # remove weights
  if (!(is.null(wei))) {
    base_draws <- posterior::mutate_variables(
      base_draws,
      .log_weight = NULL)
  }

  # center draws
  if (center) {
    center <- matrixStats::colMedians(draws)
  }
  if (scale) {
   scale <- matrixStats::colMads(draws)
  }
  draws_c <- base::scale(draws, center = center, scale = scale)

  # add weights column back
  if (!(is.null(wei))) {
    draws_c <- posterior::weight_draws(draws_c, wei)
  }

  draws_c <- posterior::as_draws_df(draws_c)

  return(draws_c)
}
