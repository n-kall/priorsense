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
##' @param variables Vector of variable names to return estimated
##'   posterior draws for.
##' @param component Component to be power-scaled (either "prior" or
##'   "likelihood"). For powerscale_sequence, this can be both "prior"
##'   and "likelihood".
##' @template powerscale_args
##' @param log_prior_fn A function that takes as input the model fit
##'   and returns the log prior values. Provided functions are
##'   `calculate_log_prior` (requires model to be fit with rstan and
##'   in the same R session) and `extract_log_prior` (requires a
##'   variable in the Stan model code to correspond to the joint log
##'   prior, by default named "log_prior").
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
powerscale <- function(fit, ...) {
  UseMethod("powerscale")
}

##' @rdname powerscale-overview
##' @export
powerscale.brmsfit <- function(fit,
                               alpha,
                               variables = NULL,
                               component = "prior",
                               is_method = "psis",
                               moment_match = FALSE,
                               k_threshold = 0.5,
                               resample = FALSE,
                               transform = FALSE,
                               log_prior_fn = log_prior_brmsfit,
                               joint_log_lik_fn = joint_log_lik_brmsfit,
                               ...) {
  
  # input checks
  checkmate::assert_number(alpha)
  checkmate::assert_number(k_threshold)
  checkmate::assert_choice(component, c("prior", "likelihood"))
  checkmate::assert_logical(moment_match)
  checkmate::assert_logical(resample)
  checkmate::assert_function(log_prior_fn)
  checkmate::assert_function(joint_log_lik_fn)

  # moment matching only works with PSIS
  if (is_method != "psis" & moment_match) {
    # TODO: also allow moment_match if loo::psis function is given as
    # argument
    moment_match <- FALSE
    warning("Moment-matching only works with PSIS. Falling back to moment_match = FALSE")
  }

  # extract draws from fit
  draws <- get_draws(fit, variables = variables)

  # get the correct importance sampling function
  if (is.character(is_method)) {
    is_method <- get(is_method, asNamespace("loo"))
  }

  # calculate the log density ratios
  log_ratios <- scaled_log_ratio(
    x = fit,
    alpha = alpha,
    component = component,
    log_prior_fn = log_prior_fn,
    joint_log_lik_fn = joint_log_lik_fn
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
  } else if (moment_match) {
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
      k_threshold = k_threshold
    )

    # TODO: use iwmm package for moment_match
    ## mm <- iwmm::moment_match(
    ##   x = fit,
    ##   psis = importance_sampling,
    ##   log_ratio_fun = log_ratios
    ## )

    importance_sampling <- mm$importance_sampling
    draws <- get_draws(mm$x, variables = variables)

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
  class(powerscaled_draws) <- "powerscaled_draws"

  return(powerscaled_draws)
}

##' @rdname powerscale-overview
##' @export
powerscale.CmdStanFit <- function(fit,
                                  alpha,
                                  variables = NULL,
                                  component = "prior",
                                  is_method = "psis",
                                  moment_match = FALSE,
                                  k_threshold = 0.5,
                                  resample = FALSE,
                                  transform = FALSE,
                                  log_prior_fn = log_prior_CmdStanFit,
                                  joint_log_lik_fn = joint_log_lik_CmdStanFit,
                                  ...) {

  if (moment_match) {
    moment_match <- FALSE
    warning("Moment-matching does not yet work with fits created with cmdstanr. Falling back to moment_match = FALSE")
  }

  # extract draws from fit
  draws <- get_draws(fit, variables = variables)

  # get the correct importance sampling function
  if (is.character(is_method)) {
    is_method <- get(is_method, asNamespace("loo"))
  }

  # calculate the log density ratios
  log_ratios <- scaled_log_ratio(
    x = fit,
    alpha = alpha,
    component = component,
    log_prior_fn = log_prior_fn,
    joint_log_lik_fn = joint_log_lik_fn
  )

  # calculate the importance weights
  importance_sampling <- is_method(
    log_ratios = log_ratios,
    # TODO: check if r_eff specification is correct
    r_eff = loo::relative_eff(
      x = exp(-log_ratios)
    )
  )

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
  class(powerscaled_draws) <- "powerscaled_draws"

  return(powerscaled_draws)
  
}

##' @rdname powerscale-overview
##' @export
powerscale.stanfit <- function(fit,
                               alpha,
                               variables = NULL,
                               component = "prior",
                               is_method = "psis",
                               moment_match = FALSE,
                               k_threshold = 0.5,
                               resample = FALSE,
                               transform = FALSE,
                               log_prior_fn = log_prior_stanfit,
                               joint_log_lik_fn = joint_log_lik_stanfit,
                               ...) {

  # input checks
  checkmate::assert_number(alpha)
  checkmate::assert_number(k_threshold)
  checkmate::assert_choice(component, c("prior", "likelihood"))
  checkmate::assert_logical(moment_match)
  checkmate::assert_logical(resample)
  checkmate::assert_function(log_prior_fn)
  checkmate::assert_function(joint_log_lik_fn)

  # moment matching only works with PSIS
  if (is_method != "psis" & moment_match) {
    # TODO: also allow moment_match if loo::psis function is given as
    # argument
    moment_match <- FALSE
    warning("Moment-matching only works with PSIS. Falling back to moment_match = FALSE")
  }

  # extract draws from fit
  draws <- get_draws(fit, variables = variables)

  # get the correct importance sampling function
  if (is.character(is_method)) {
    is_method <- get(is_method, asNamespace("loo"))
  }

  # calculate the log density ratios
  log_ratios <- scaled_log_ratio(
    x = fit,
    alpha = alpha,
    component = component,
    log_prior_fn = log_prior_fn,
    joint_log_lik_fn = joint_log_lik_fn
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
  } else if (moment_match) {
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
      k_threshold = k_threshold
    )

    # TODO: use iwmm package for moment_match
    ## mm <- iwmm::moment_match(
    ##   x = fit,
    ##   psis = importance_sampling,
    ##   log_ratio_fun = log_ratios
    ## )

    importance_sampling <- mm$importance_sampling
    draws <- get_draws(mm$x, variables = variables)

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
  class(powerscaled_draws) <- "powerscaled_draws"

  return(powerscaled_draws)
}

##' Extract weights from power-scaled draws
##'
##' Extract weights from an object of class powerscaled_draws
##' @param object powerscaled_draws
##' @param ... unused
##' @importFrom stats weights
##' @export
weights.powerscaled_draws <- function(object, ...) {

  # get the weights if they exist
  if (!object$powerscaling$resampled) {
    stats::weights(object$powerscaling$importance_sampling, ...)
  } else
    NULL
}


##' Resample power-scaled draws based on weights
##'
##' Resample object of class powerscaled_draws if not already
##' resampled
##' @param x powerscaled_draws
##' @param ... unused
##' @importFrom posterior resample_draws
##' @export
resample_draws.powerscaled_draws <- function(x, ...) {

  # resample if not already resampled
  if(x$powerscaling$resampled) {
    message("Power-scaled draws already resampled.")
  } else {
    x$draws <- posterior::resample_draws(posterior::merge_chains(x$draws), ...)
    x$powerscaling$resampled <- TRUE
  }
  return(x)
}

##' Convert power-scaled draws to data.frame
##' @export
##' @param x powerscaled_draws
##' @param ... unused
as.data.frame.powerscaled_draws_summary <- function(x, ...) {

  # get the draws dataframe
  draws <- x$draws

  # add details as columns to the draws dataframe
  draws$component <- x$powerscaling$component
  draws$alpha <- x$powerscaling$alpha
  draws$pareto_k <- x$powerscaling$importance_sampling$diagnostics$pareto_k
  draws$n_eff <- x$powerscaling$importance_sampling$diagnostics$n_eff

  return(draws)
}
