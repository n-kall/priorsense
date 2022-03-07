##' Prior/likelihood power-scaling perturbation
##'
##' Estimate posterior draws based on power-scaling perturbations of
##' prior or likelihood using importance sampling (and optionally moment matching).
##'
##' @name powerscale-overview
##'
##' @template fit_arg
##' @param alpha Value by which to power-scale specified component. (likelihood/prior).
##' @param lower_alpha Lower power-scaling alpha value in sequence.
##' @param upper_alpha Upper power-scaling alpha value in sequence.
##' @param alpha_step Step size of power-scaling alphas in sequence.
##' @param variable Vector of variable names to return estimated
##'   posterior draws for.
##' @param component Component to be power-scaled (either "prior" or
##'   "likelihood"). For powerscale_sequence, this can be both "prior"
##'   and "likelihood".
##' @template powerscale_args
##' @param log_prior_fn A function that takes as input the model fit
##'   and returns the log prior values.
##' @param joint_log_lik_fn A function that takes as input the model
##'   fit and returns the joint log likelihood values.
##' @param ... Further arguments passed to the custom functions
##'   documented above.
##' @return A `powerscaled_draws` or `powerscaled_sequence` object,
##'   which contains the estimated posterior draws resulting from the
##'   power-scaling perturbations and details of the perturbation and
##'   estimation methods.
##' @template powerscale_references
##' @export
powerscale <- function(x, ...) {
  UseMethod("powerscale")
}


  
##' @rdname powerscale-overview
##' @export
powerscale.powerscaling_data <- function(x,
                                         alpha,
                                         ...
                                         ) {
  powerscale.default(
    x = x$fit,
    alpha = alpha,
    log_prior_fn = x$log_prior_fn,
    joint_log_lik_fn = x$joint_log_lik_fn,
    get_draws = x$get_draws,
    unconstrain_pars = x$unconstrain_pars,
    log_prob_upars = x$log_prob_upars,
    log_ratio_upars = x$log_ratio_upars,
    ...
  )
}


##' @rdname powerscale-overview
##' @export
powerscale.default <- function(x,
                               alpha,
                               log_prior_fn,
                               joint_log_lik_fn,
                               get_draws,
                               unconstrain_pars,
                               log_prob_upars,
                               log_ratio_upars,
                               variable = NULL,
                               component = "prior",
                               is_method = "psis",
                               moment_match = FALSE,
                               k_threshold = 0.5,
                               resample = FALSE,
                               transform = FALSE,
                               ...
                               ) {


  # extract draws from fit
  draws <- get_draws(x, variable = variable, ...)
  
  # get the correct importance sampling function
  if (is.character(is_method)) {
    is_method <- get(is_method, asNamespace("loo"))
  }

  # calculate the log density ratios
  log_ratios <- scaled_log_ratio(
    x = x,
    alpha = alpha,
    component = component,
    log_prior = log_prior_fn,
    joint_log_lik = joint_log_lik_fn
  )
  
  if (!moment_match) {
    # calculate the importance weights
    importance_sampling <- is_method(
      log_ratios = log_ratios,
      # TODO: check if r_eff specification is correct
      r_eff = loo::relative_eff(
        x = exp(-log_ratios)
      )
    )
  } else {
    # perform moment matching if specified
    # calculate the importance weights
    importance_sampling <- SW(
      is_method(
        log_ratios = log_ratios,
        # TODO: check if r_eff specification is correct
        r_eff = loo::relative_eff(
          x = exp(-log_ratios)
        )
      )
    )

    # TODO: give a warning if trying to use moment_matching with:
    # stanfit from other session

    mm <- moment_match(
      x = fit,
      psis = importance_sampling,
      alpha = alpha,
      component = component,
      k_threshold = k_threshold,
      joint_log_lik_fn = joint_log_lik_fn,
      log_prior_fn = log_prior_fn
    )

    # TODO: use iwmm package for moment_match
    ## mm <- iwmm::moment_match(
    ##   x = fit,
    ##   psis = importance_sampling,
    ##   log_ratio_fun = log_ratios
    ## )

    importance_sampling <- mm$importance_sampling
    draws <- get_draws(mm$x, variable = variable, ...)

 }
  
  # transform the draws if specified
  if (transform == "whiten") {
    whitened_draws <- whiten_draws(draws, ...)
    draws_tr <- whitened_draws$draws
    loadings <- whitened_draws$loadings
    transform_details = list(
      transform = transform,
      loadings = loadings
    )
    draws <- draws_tr
  } else if (transform == "scale") {
    draws <- scale_draws(draws, ...)
    transform_details = list(transform = transform)
  } else {
    transform_details = list(transform = transform)
  }

  # reweight the draws with the calculated importance weights
  new_draws <- posterior::weight_draws(
    x = draws,
    weights = stats::weights(importance_sampling, normalize = FALSE),
    log = TRUE
  )

  # resample the draws if specified
  if (resample) {
    new_draws <- posterior::resample_draws(
      x = posterior::merge_chains(new_draws)
    )
  }

  # create object with details of power-scaling
  powerscaling_details <- list(
    alpha = alpha,
    component = component,
    importance_sampling = importance_sampling,
    moment_match = moment_match,
    resampled = resample,
    transform_details = transform_details
  )
  class(powerscaling_details) <- "powerscaling_details"

  # return draws and details
  powerscaled_draws <- list(
    draws = new_draws,
    powerscaling = powerscaling_details
  )
  class(powerscaled_draws) <- c("powerscaled_draws", class(powerscaled_draws))

  return(powerscaled_draws)
}


##' @rdname powerscale-overview
##' @export
powerscale.CmdStanFit <- function(x,
                                  ...
                                  ) {
  psd <- create_powerscaling_data(x, ...)

  powerscale.powerscaling_data(psd, ...)

}

##' @rdname powerscale-overview
##' @export
powerscale.stanfit <- function(x,
                               ...
                               ) {
  
  psd <- create_powerscaling_data.stanfit(x, ...)
  
  powerscale.powerscaling_data(psd, ...)

}
