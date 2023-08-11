##' Prior/likelihood power-scaling perturbation
##'
##' Estimate posterior draws based on power-scaling perturbations of
##' prior or likelihood using importance sampling (and optionally
##' moment matching).
##'
##' @name powerscale-overview
##'
##' @template fit_arg
##' @template alpha_args
##' @param variable Vector of variable names to return estimated
##'   posterior draws for.
##' @param component Component to be power-scaled (either "prior" or
##'   "likelihood"). For powerscale_sequence, this can be both "prior"
##'   and "likelihood".
##' @param selection Vector specifying parts of component to be considered.
##' @template selection_arg
##' @template powerscale_args
##' @template prediction_arg
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
powerscale.priorsense_data <- function(x,
                                       component,
                                       alpha,
                                       is_method = "psis",
                                       moment_match = FALSE,
                                       k_threshold = 0.5,
                                       resample = FALSE,
                                       transform = FALSE,
                                       prediction = NULL,
                                       variable = NULL,
                                       selection = NULL,
                                       ...) {

  # input checks
  checkmate::assertClass(x, classes = "priorsense_data")
  checkmate::assertNumeric(alpha, lower = 0)
  checkmate::assertSubset(component, c("prior", "likelihood"))
  checkmate::assertCharacter(is_method)
  checkmate::assertLogical(moment_match)
  checkmate::assertNumber(k_threshold)
  checkmate::assertLogical(resample)
  checkmate::assertFunction(prediction, null.ok = TRUE)
  checkmate::assertCharacter(variable, null.ok = TRUE)
  checkmate::assertNumeric(selection, null.ok = TRUE)

  draws <- x$draws

  # get predictions if specified
  if (!(is.null(prediction))) {
    pred_draws <- prediction(x$fit, ...)

    # bind predictions and posterior draws
    draws <- posterior::bind_draws(draws, pred_draws)
  }
  
  # subset the draws
  draws <- posterior::subset_draws(draws, variable = variable)
  
  # get the correct importance sampling function
  is_method <- get(is_method, asNamespace("loo"))

  # select the appropriate component draws
  if (component == "prior") {
    log_comp_draws <- x[["log_prior"]]
  } else if (component == "likelihood") {
    log_comp_draws <- x[["log_lik"]]
  }

  # subset component draws if specified
  if (!(is.null(selection))) {
    log_comp_draws <- log_comp_draws[, , selection]
  }

  # sum component draws
  log_comp_draws <- rowsums_draws(log_comp_draws)

  # calculate the log weights
  log_ratios <- scaled_log_ratio(
    component_draws = log_comp_draws,
    alpha = alpha
  )

  if (!moment_match) {
    # calculate the importance weights
    importance_sampling <- is_method(
      log_ratios = log_ratios,
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
        r_eff = loo::relative_eff(
          x = exp(-log_ratios)
        )
      )
    )

    if (component == "prior") {
      component_fn <- x$log_prior_fn
    } else if (component == "likelihood") {
      component_fn <- x$log_lik_fn
    }

    mm <- iwmm::moment_match(
      x = x$fit,
      log_ratio_fun = powerscale_log_ratio_fun,
      alpha = alpha,
      component = component,
      ...
    )

    importance_sampling <- list(
      diagnostics = list(pareto_k = mm$pareto_k, n_eff = NA),
      log_weights = mm$log_weights
    )
    class(importance_sampling) <- c(
      "psis",
      "importance_sampling",
      class(importance_sampling)
    )

    draws <- remove_unwanted_vars(posterior::as_draws_df(mm$draws))

  }

  # keep track of base log-ratios for diagnostics
  importance_sampling$orig_log_ratios <- log_ratios

  # transform the draws if specified
  if (transform == "whiten") {
    whitened_draws <- whiten_draws(draws, ...)
    draws_tr <- whitened_draws$draws
    loadings <- whitened_draws$loadings
    transform_details <- list(
      transform = transform,
      loadings = loadings
    )
    draws <- draws_tr
  } else if (transform == "scale") {
    draws <- scale_draws(draws, ...)
    transform_details <- list(transform = transform)
  } else {
    transform_details <- list(transform = transform)
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
                                  component,
                                  alpha,
                                  ...
                                  ) {
  psd <- create_priorsense_data.CmdStanFit(x, ...)

  powerscale.priorsense_data(
    psd,
    component = component,
    alpha = alpha,
    ...
  )

}

##' @rdname powerscale-overview
##' @export
powerscale.stanfit <- function(x,
                               component,
                               alpha,
                               ...
                               ) {

  psd <- create_priorsense_data.stanfit(x, ...)

  powerscale.priorsense_data(
    psd,
    component = component,
    alpha = alpha,
    ...
  )

}
