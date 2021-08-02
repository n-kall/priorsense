##' Power-scale gradients
##'
##' Calculate the numerical derivative of the posterior to the
##' specified component (prior or likelihood) of the model fit. This
##' is done using importance sampling (and optionally moment matching)
##' to approximate the posteriors that result from power-scaling the
##' specified component distribution (prior or likelihood).
##' @param fit Model fit of class brmsfit, stanfit or CmdStanFit
##' @param variables Variables to compute sensitivity of. If NA
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
##' @param ... Further arguments passed to functions.
##' @return Maximum of the absolute derivatives above and below alpha
##'   = 1.
##' @export
powerscale_gradients <- function(fit, variables = NA, component = c("prior", "likelihood"),
                                 type = c("quantities", "divergence"),
                                 lower_alpha = 0.9,
                                 upper_alpha = 1.1,
                                 div_measure = "cjs_dist",
                                 measure_args = list(),
                                 scale = FALSE,
                                 is_method = "psis",
                                 moment_match = FALSE,
                                 k_threshold = 0.5,
                                 resample = FALSE,
                                 log_prior_fn = calculate_log_prior,
                                 joint_log_lik_fn = extract_joint_log_lik,
                                 ...
                                 ) {

  checkmate::assert_number(lower_alpha, lower = 0, upper = 1)
  checkmate::assert_number(upper_alpha, lower = 1, upper = Inf)
  checkmate::assert_character(variables)
  checkmate::assert_character(div_measure)
  checkmate::assert_list(measure_args)
  checkmate::assert_number(k_threshold)
  checkmate::assert_character(component)
  checkmate::assert_logical(moment_match)
  checkmate::assert_logical(resample)
  checkmate::assert_function(log_prior_fn)
  checkmate::assert_function(joint_log_lik_fn)

  
  # extract the draws
  base_draws <- get_draws(fit, variables = variables)

  base_quantities <- summarise_draws(base_draws, posterior::default_summary_measures())


  perturbed_draws_lower <- list(
    prior = NULL,
    likelihood = NULL
  )

  perturbed_draws_upper <- list(
    prior = NULL,
    likelihood = NULL
  )

  out <- list(
    divergence = list(
      prior = NULL,
      likelihood = NULL
    ),
    quantities = list(
      prior = NULL,
      likelihood = NULL
    )
  )

  for (comp in component) {

    # calculate the lower scaled draws
    perturbed_draws_lower[[comp]] <- powerscale(
      fit = fit,
      variables = variables,
      component = comp,
      alpha = lower_alpha,
      is_method = is_method,
      moment_match = moment_match,
      k_threshold = k_threshold,
      resample = resample,
      joint_log_lik_fn = joint_log_lik_fn,
      log_prior_fn = log_prior_fn
    )

    # calculate the upper scaled draws
    perturbed_draws_upper[[comp]] <- powerscale(
      fit = fit,
      variables = variables,
      component = comp,
      alpha = upper_alpha,
      is_method = is_method,
      moment_match = moment_match,
      k_threshold = k_threshold,
      joint_log_lik_fn = joint_log_lik_fn,
      log_prior_fn = log_prior_fn
    )

    if ("divergence" %in% type) {

      # compute the divergence for lower draws
      lower_dist <- divergence_measures(
        draws1 = base_draws,
        draws2 = perturbed_draws_lower[[comp]]$draws,
        measure = div_measure,
        measure_args = measure_args,
        ...
      )

      # compute the divergence for upper draws
      upper_dist <- divergence_measures(
        draws1 = base_draws,
        draws2 = perturbed_draws_upper[[comp]]$draws,
        measure = div_measure,
        measure_args = measure_args,
        ...
      )

      out$divergence[[comp]] <- powerscale_divergence_gradients(
        lower_divergences = lower_dist,
        upper_divergences = upper_dist,
        lower_alpha = lower_alpha,
        upper_alpha = upper_alpha,
        ...
      )
    }

    if ("quantities" %in% type) {

      # calculate lower quantities
      lower_quantities <- posterior::summarise_draws(
        .x = perturbed_draws_lower[[comp]]$draws,
        mean_weighted,
        median_weighted,
        sd_weighted,
        mad_weighted,
        quantile_weighted,
        .args = list(weights = stats::weights(perturbed_draws_lower[[comp]]$draws))
      )

      # calculate upper quantities
      upper_quantities <- posterior::summarise_draws(
        .x = perturbed_draws_upper[[comp]]$draws,
        mean_weighted,
        median_weighted,
        sd_weighted,
        mad_weighted,
        quantile_weighted,
        .args = list(weights = stats::weights(perturbed_draws_upper[[comp]]$draws))
      )

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

  gradients_lower <- (subset(base_quantities, select = -c(variable)) -
                        subset(lower_quantities, select = -c(variable))) /
    (0 - log(lower_alpha, base = 2))

  gradients_upper <- (subset(upper_quantities, select = -c(variable)) -
                        subset(base_quantities, select = -c(variable))) /
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
##' @return
powerscale_divergence_gradients <- function(lower_divergences, upper_divergences,
                                            lower_alpha, upper_alpha, ...) {

  variable <- lower_divergences$variable

  lower_grad <- -1 * (subset(lower_divergences, select = -c(variable))) /
    (0 - log(lower_alpha, base = 2))

  upper_grad <- (subset(upper_divergences, select = -c(variable))) /
    (log(upper_alpha, base = 2))

  # take max of gradients for each variable
  grad <- pmax(abs(upper_grad), abs(lower_grad))

  return(tibble::as_tibble(cbind(variable, grad)))

}
