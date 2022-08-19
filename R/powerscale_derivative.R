##' Derivative with respect to power-scaling
##'
##' Calculate the analytical derivative of a quantity with respect to
##' power-scaling prior or likelihood.
##'
##' @param x Posterior draws.
##' @param log_component Log likelihood or log prior values.
##' @param quantity Character specifying quantity of interest (default
##'   is "mean"). Options are "mean", "sd", "var".
##' @param ... unused
##' @return Derivative of the quantity with respect to log2 of the
##'   power-scaling factor (alpha).
##'
##' @examples
##' \dontrun{
##' example_model <- example_powerscale_model()
##' fit <- rstan::stan(
##'     model_code = example_model$model_code,
##'     data = example_model$data
##' )
##' draws <- posterior::subset_draws(
##'     posterior::as_draws_df(fit),
##'     variable = c("mu", "sigma")
##' )
##' log_prior <- log_prior_stanfit(fit)
##' posterior::summarise_draws(
##'     draws,
##'     mean,
##'     ~powerscale_derivative(.x, log_prior, quantity = "mean")
##' )
##' }
##' @export
powerscale_derivative <- function(x,
                                  log_component,
                                  quantity = "mean",
                                  ...) {

  log_component <- as.numeric(log_component)

  if (quantity %in% c("median", "mad", "q5", "q95")) {
    out <- NA
    warning("Power-scaling derivative for medians or quantiles is zero. Consider using powerscale_gradients instead.")

  } else if (quantity == "mean") {
  # adapted from method by Topi Paananen
    deriv_first_moment <- mean(x * log_component) -
      mean(x) * mean(log_component)

    # wrt log_2(alpha)
    out <- log(2) * deriv_first_moment

  } else if (quantity == "sd") {

    first_moment <- mean(x)
    second_moment <- mean(x^2)

    deriv_first_moment <- mean(x * log_component) -
      mean(x) * mean(log_component)

    deriv_second_moment <- mean(x^2 * log_component) -
      mean(x^2) * mean(log_component)

    out <- log(2) * ((deriv_second_moment -
                        2 * deriv_first_moment * first_moment) *
                       0.5 / sqrt(second_moment - first_moment^2))

  } else if (quantity == "var") {

    first_moment <- mean(x)

    deriv_first_moment <- mean(x * log_component) -
      mean(x) * mean(log_component)

    deriv_second_moment <- mean(x^2 * log_component) -
      mean(x^2) * mean(log_component)

    out <- log(2) *
      (deriv_second_moment -
         2 * deriv_first_moment * first_moment)
  }

  names(out) <- paste0("psens_", quantity)

  return(out)
}
