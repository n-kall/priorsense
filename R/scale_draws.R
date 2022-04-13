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
