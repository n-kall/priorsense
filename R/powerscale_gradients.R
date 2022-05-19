##' Power-scale gradients
##'
##' Calculate the numerical derivative of posterior
##' quantities/divergence with respect to power-scaling the specified
##' component (prior or likelihood). This is done using importance
##' sampling (and optionally moment matching).
##' @name powerscale-gradients
##' @param x Model fit object or a powerscaling_data object.
##' @param variable Variables to compute sensitivity of. If NA
##'   (default) sensitivity is computed for all variables.
##' @param component Component to power-scale (prior or likelihood).
##' @param type type of sensitivity to measure ("distance",
##'   "quantity").  Multiple options can be specified at the same
##'   time.
##' @param lower_alpha lower power to scale component by, should be <
##'   1 (default is 0.9).
##' @param upper_alpha upper power to scale component by, should be >
##'   1 (default is 1.1).
##' @template div_measure_arg
##' @param scale logical scale quantity gradients by base posterior
##'   standard deviation.
##' @template powerscale_args
##' @template prediction_arg
##' @param ... Further arguments passed to functions.
##' @return Maximum of the absolute derivatives above and below alpha
##'   = 1.
##' @export
powerscale_gradients <- function(x, ...) {

  UseMethod("powerscale_gradients")
}

##' @rdname powerscale-gradients
##' @export
powerscale_gradients.CmdStanFit <- function(x, ...) {

  psd <- create_powerscaling_data(x)

  powerscale_gradients(psd, ...)

  }

##' @rdname powerscale-gradients
##' @export
powerscale_gradients.stanfit <- function(x, ...) {

  psd <- create_powerscaling_data(x)

  powerscale_gradients(psd, ...)


}


##' @rdname powerscale-gradients
##' @export
powerscale_gradients.powerscaling_data <- function(x,
                                         variable = NULL,
                                         component = c("prior", "likelihood"),
                                         type = c("quantities", "divergence"),
                                         lower_alpha = 0.99,
                                         upper_alpha = 1.01,
                                         div_measure = "cjs_dist",
                                         measure_args = list(),
                                         is_method = "psis",
                                         moment_match = FALSE,
                                         k_threshold = 0.5,
                                         resample = FALSE,
                                         transform = FALSE,
                                         prediction = NULL,
                                         scale = FALSE,
                                         ...) {

  checkmate::assert_number(lower_alpha, lower = 0, upper = 1)
  checkmate::assert_number(upper_alpha, lower = 1, upper = Inf)
  checkmate::assert_character(div_measure)
  checkmate::assert_list(measure_args)
  checkmate::assert_number(k_threshold)
  checkmate::assert_character(component)
  checkmate::assert_logical(moment_match)
  checkmate::assert_logical(resample)

  # extract the draws
  base_draws <- x$draws

  base_draws <- posterior::subset_draws(base_draws, variable = variable, ...)

  # transform if needed
  loadings <- NULL
  if (transform == "whiten") {
    whitened_draws <- whiten_draws(base_draws, ...)
    base_draws_t <- whitened_draws$draws
    loadings <- whitened_draws$loadings
  } else if (transform == "scale") {
    base_draws_t <- scale_draws(base_draws, ...)
  } else {
    base_draws_t <- base_draws
  }

  perturbed_draws_lower <- list(
    prior = NULL,
    likelihood = NULL
  )

  perturbed_draws_upper <- list(
    prior = NULL,
    likelihood = NULL
  )

  out <- list(
    multivariate_divergence = list(
      prior = NULL,
      likelihood = NULL
    ),
    divergence = list(
      prior = NULL,
      likelihood = NULL
    ),
    quantities = list(
      prior = NULL,
      likelihood = NULL
    ),
    loadings = loadings
  )

  for (comp in component) {

    # calculate the lower scaled draws
    perturbed_draws_lower[[comp]] <- powerscale(
      x = x,
      variable = variable,
      component = comp,
      alpha = lower_alpha,
      is_method = is_method,
      moment_match = moment_match,
      k_threshold = k_threshold,
      resample = resample,
      transform = transform,
      prediction = prediction,
      ...
    )

    # calculate the upper scaled draws
    perturbed_draws_upper[[comp]] <- powerscale(
      x = x,
      variable = variable,
      component = comp,
      alpha = upper_alpha,
      is_method = is_method,
      moment_match = moment_match,
      k_threshold = k_threshold,
      resample = resample,
      transform = transform,
      prediction = prediction,
      ...
    )

    if ("divergence" %in% type) {

      # compute the divergence for lower draws
      lower_dist <- measure_divergence(
        draws1 = base_draws_t,
        draws2 = perturbed_draws_lower[[comp]]$draws,
        measure = div_measure,
        measure_args = measure_args,
        ...
      )

      # compute the divergence for upper draws
      upper_dist <- measure_divergence(
        draws1 = base_draws_t,
        draws2 = perturbed_draws_upper[[comp]]$draws,
        measure = div_measure,
        measure_args = measure_args,
        ...
      )

      # calculate the gradients
      out$divergence[[comp]] <- powerscale_divergence_gradients(
        lower_divergences = lower_dist,
        upper_divergences = upper_dist,
        lower_alpha = lower_alpha,
        upper_alpha = upper_alpha,
        ...
      )
    }

    if ("quantities" %in% type) {

      # summarise base posterior
      base_quantities <- summarise_draws(
        base_draws_t,
        posterior::default_summary_measures()
      )

      # calculate lower quantities
      lower_quantities <- summarise_draws(
        perturbed_draws_lower[[comp]]
      )$draws_summary

      # calculate upper quantities
      upper_quantities <- summarise_draws(
        perturbed_draws_upper[[comp]]
      )$draws_summary

      # calculate gradients of quantities
      out$quantities[[comp]] <- powerscale_quantities_gradients(
        base_quantities = base_quantities,
        lower_quantities = lower_quantities,
        upper_quantities = upper_quantities,
        lower_alpha = lower_alpha,
        upper_alpha = upper_alpha,
        scale = scale,
        ...
      )

    }
  }

  if ("multi_div" %in% type) {

    upper_multi_kl <- c()
    upper_multi_wasserstein <- c()

    for (comp in component) {

      upper_multi_kl[[comp]] <- sqrt(mv_kl_div(
        weights = stats::weights(perturbed_draws_upper[[comp]]$draws)
      )) / log(upper_alpha, base = 2)

      upper_multi_wasserstein[[comp]] <- mv_wasserstein_dist(
        draws1 = base_draws_t,
        draws2 = perturbed_draws_upper[[comp]]$draws,
        weights2 = stats::weights(perturbed_draws_upper[[comp]]$draws)
      )

      upper_mw_dist[[comp]] <- mv_wasserstein_dist(
        posterior::weight_draws(base_draws_t, rep(1/posterior::ndraws(base_draws_t), times = posterior::ndraws(base_draws_t)), perturbed_draws_upper[[comp]]$draws)
      )

      out$multivariate_divergence[[comp]] <- c(
        KL = upper_multi_kl[[comp]],
        wasserstein = upper_multi_wasserstein[[comp]],
        mw_dist = upper_mw_dist
      )
    }
  }
  return(out)
}


##' Calculate gradient for quantities
##'
##' @param base_quantities quantities for unscaled
##' @param lower_quantities quantities for lower scaled
##' @param upper_quantities quantities for upper scaled
##' @param lower_alpha lower alpha
##' @param upper_alpha upper alpha
##' @param scale scale by base posterior sd
##' @noRd
##' @return a tibble
powerscale_quantities_gradients <- function(base_quantities,
                                            lower_quantities,
                                            upper_quantities,
                                            lower_alpha,
                                            upper_alpha,
                                            scale = FALSE,
                                            ...) {

  variable <- base_quantities$variable

  gradients_lower <- (base_quantities[-1] - lower_quantities[-1]) /
    (0 - log(lower_alpha, base = 2))

  gradients_upper <- (upper_quantities[-1] - base_quantities[-1]) /
    (log(upper_alpha, base = 2))

  gradients <- ((gradients_upper + gradients_lower) / 2)

  if (scale) {
    gradients <- gradients / base_quantities$sd
  }

  return(tibble::as_tibble(cbind(variable, gradients)))
}


##' Gradients for divergence
##'
##' @param lower_divergences divergences to lower scaled
##' @param upper_divergences divergences to upper scaled
##' @param lower_alpha lower alpha
##' @param upper_alpha upper alpha
##' @noRd
##' @return gradients
powerscale_divergence_gradients <- function(lower_divergences, upper_divergences,
                                            lower_alpha, upper_alpha, ...) {

  variable <- lower_divergences$variable

  ## second-order centered difference approximation
  ## f''(x) = (f(x + dx) - 2f(x) + f(x - dx)) / dx^2
  ## here it is wrt log_2 alpha
  upper_diff <- subset(upper_divergences, select = -c(variable))
  lower_diff <- subset(lower_divergences, select = -c(variable))
  logdiffsquare <- 2 * log(upper_alpha, base = 2)
  grad <- (upper_diff + lower_diff) / logdiffsquare

  return(tibble::as_tibble(cbind(variable, grad)))

}
