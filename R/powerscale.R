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
##'   posterior draws for. If `NULL` all variables will be included.
##' @param component Component to be power-scaled (either "prior" or
##'   "likelihood"). For powerscale_sequence, this can be both "prior"
##'   and "likelihood".
##' @param selection Numeric vector specifying partitions of component
##'   to be included in power-scaling. Default is NULL, which takes
##'   all partitions.
##' @template selection_arg
##' @template powerscale_args
##' @template prediction_arg
##' @param ... Further arguments passed to internal functions.
##' @return A `powerscaled_draws` or `powerscaled_sequence` object,
##'   which contains the estimated posterior draws resulting from the
##'   power-scaling perturbations and details of the perturbation and
##'   estimation methods.
##' @template powerscale_references
##' @examples
##' ex <- example_powerscale_model()
##'
##' powerscale(ex$draws, component = "prior", alpha = 0.5)
##'
##' powerscale_sequence(ex$draws)
##' @export
powerscale <- function(x, ...) {
  UseMethod("powerscale")
}


##' @rdname powerscale-overview
##' @export
powerscale.default <- function(x, component, alpha,
                               moment_match = FALSE,
                               k_threshold = NULL,
                               resample = FALSE,
                               transform = NULL,
                               prediction = NULL,
                               variable = NULL,
                               selection = NULL,
                               ...) {
  psd <- create_priorsense_data(x, ...)
  powerscale(
    psd,
    component = component,
    alpha = alpha,
    moment_match = moment_match,
    k_threshold = k_threshold,
    resample = resample,
    transform = transform,
    prediction = prediction,
    variable = variable,
    selection = selection
  )
}


##' @rdname powerscale-overview
##' @export
powerscale.priorsense_data <- function(x,
                                       component,
                                       alpha,
                                       moment_match = FALSE,
                                       k_threshold = NULL,
                                       resample = FALSE,
                                       transform = NULL,
                                       prediction = NULL,
                                       variable = NULL,
                                       selection = NULL,
                                       ...) {

  # input checks
  checkmate::assertNumber(alpha, lower = 0)
  checkmate::assertChoice(component, c("prior", "likelihood"))
  checkmate::assertLogical(moment_match, len = 1)
  checkmate::assertNumber(k_threshold, null.ok = TRUE)
  checkmate::assertChoice(transform, c("whiten", "scale", "identity"), null.ok = TRUE)
  checkmate::assertLogical(resample, len = 1)
  checkmate::assertCharacter(transform, null.ok = TRUE, len = 1)
  checkmate::assertFunction(prediction, null.ok = TRUE)
  checkmate::assertCharacter(variable, null.ok = TRUE)
  checkmate::assertNumeric(selection, null.ok = TRUE)

  draws <- x$draws

  if (is.null(k_threshold)) {
    k_threshold <- min(1 - 1 / log10(ndraws(draws)), 0.7)
  }

    # transform the draws if specified
    if (is.null(transform)) {
      transform <- "identity"
    }
    if (transform == "whiten") {
      whitened_draws <- whiten_draws(draws, ...)
      draws_tr <- whitened_draws
      loadings <- attr(whitened_draws, "loadings")
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


  # if alpha is 1, just return the draws with the powerscaling details
  if (alpha == 1) {

    new_draws <- draws

    # create object with details of power-scaling
    powerscaling_details <- list(
      alpha = alpha,
      component = component,
      moment_match = moment_match,
      diagnostics = list(
        khat = -Inf,
        min_ss = NA,
        khat_threshold = 0.7,
        convergence_rate = NA
      ),
      resampled = resample,
      transform_details = transform_details
    )
    class(powerscaling_details) <- "powerscaling_details"


  } else {

    # duplicate here
    # get predictions if specified
    if (!(is.null(prediction))) {
      pred_draws <- prediction(x$fit, ...)

      # bind predictions and posterior draws
      draws <- posterior::bind_draws(draws, pred_draws)
    }

    # subset the draws
    draws <- posterior::subset_draws(draws, variable = variable)

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

    if (moment_match) {

      require_package(
        "iwmm",
        message = " to use moment matching, available from https://github.com/topipa/iwmm"
      )

      # perform moment matching if specified
      # calculate the importance weights
      if (component == "prior") {
        component_fn <- x$log_prior_fn
      } else if (component == "likelihood") {
        component_fn <- x$log_lik_fn
      }

      mm <- iwmm::moment_match(
        x = x$fit,
        log_ratio_fun = x$log_ratio_fn,
        alpha = alpha,
        component_fn = component_fn,
        k_threshold = k_threshold,
        ...
      )

      smoothed_log_ratios <- list(
        diagnostics = list(
          khat = mm$diagnostics$pareto_k,
          khat_threshold = k_threshold
        ),
        x = mm$log_weights
      )
      draws <- remove_unwanted_vars(posterior::as_draws_df(mm$draws))

      # get moment-matched predictions
      if (!(is.null(prediction))) {
        pred_draws <- prediction(mm$fit, ...)

        # bind predictions and posterior draws
        draws <- posterior::bind_draws(draws, pred_draws)
      }

    } else {

      # no moment matching
      smoothed_log_ratios <- posterior::pareto_smooth(
        log_ratios,
        r_eff = NULL,
        return_k = TRUE,
        extra_diags = TRUE,
        are_log_weights = TRUE,
        verbose = FALSE
      )

      smoothed_log_ratios$x <- as.numeric(smoothed_log_ratios$x)
    }

    # keep track of base log-ratios for diagnostics
    smoothed_log_ratios$orig_log_ratios <- log_ratios

    # reweight the draws with the calculated importance weights
    new_draws <- posterior::weight_draws(
      x = draws,
      weights = smoothed_log_ratios$x,
      log = TRUE
    )

    # resample the draws if specified
    if (resample) {
      new_draws <- posterior::resample_draws(
        x = posterior::merge_chains(new_draws)
      )
    }

    # subset the draws
    new_draws <- posterior::subset_draws(new_draws, variable = variable)

    # create object with details of power-scaling
    powerscaling_details <- list(
      alpha = alpha,
      component = component,
      moment_match = moment_match,
      diagnostics = smoothed_log_ratios$diagnostics,
      resampled = resample,
      transform_details = transform_details
    )
    class(powerscaling_details) <- "powerscaling_details"

  }

  # return draws and details
  powerscaled_draws <- list(
    draws = new_draws,
    powerscaling = powerscaling_details
  )


  attr(new_draws, "powerscaling") <- powerscaling_details

  class(new_draws) <- c("powerscaled_draws", class(new_draws))


  return(new_draws)

}
