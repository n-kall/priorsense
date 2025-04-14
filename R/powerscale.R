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
##' @param selection Vector specifying partitions of component to be
##'   included in power-scaling. Default is NULL, which takes all
##'   partitions. If this is a character, then it is appended to the
##'   variable name (`log_prior_name` or `log_lik_name`) with an `_`
##'   between them.
##' @template selection_arg
##' @template powerscale_args
##' @template log_comp_name
##' @template prediction_arg
##' @param ... Further arguments passed to internal functions.
##' @return A `powerscaled_draws` or `powerscaled_sequence` object,
##'   which contains the estimated posterior draws resulting from the
##'   power-scaling perturbations and details of the perturbation and
##'   estimation methods.
##' @template powerscale_references
##' @srrstats {G2.0} Assertions are made on the lengths of inputs via the checkmate package
##' @srrstats {G2.1} Assertions on types of inputs are made via the checkmate package
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
                               log_prior_name = "lprior",
                               log_lik_name = "log_lik",
                               ...) {

  psd <- create_priorsense_data(
    x,
    log_prior_name = log_prior_name,
    log_lik_name = log_lik_name,
    ...
  )
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
##' @srrstats {G2.3}
##' @srrstats {G2.3a} checkmate functions used to allow only specific
##'   argument values
##' @srrstats {G2.3b} tolower() used for component
##' @srrstats {G2.4} Input coercion
##' @srrstats {G2.4b} Input coercion with `as.numeric()`
##' @srrstats {G2.4c} Input coercion with `as.character()`
##' @srrstats {G2.4e} Input coercion
##' @srrstats {EA2.6} vector inputs are coerced to numeric
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
                                       log_prior_name = "lprior",
                                       log_lik_name = "log_lik",
                                       ...) {

  # input coercion
  component <- tolower(as.character(component))
  alpha <- as.numeric(alpha)
  moment_match <- as.logical(moment_match)
  if (!is.null(k_threshold)) {
    k_threshold <- as.numeric(k_threshold)
  }
  resample <- as.logical(resample)
  if (!is.null(transform)) {
    transform <- as.character(transform)
  }
  if (!is.null(prediction)) {
    prediction <- as.function(prediction)
  }
  if (!is.null(variable)) {
    variable <- as.character(variable)
  }

  log_prior_name <- as.character(log_prior_name)
  log_lik_name <- as.character(log_lik_name)


  # input checks
  checkmate::assertNumber(alpha, lower = 0)
  checkmate::assertChoice(component, c("prior", "likelihood"))
  checkmate::assertFlag(moment_match)
  checkmate::assertNumber(k_threshold, null.ok = TRUE)
  checkmate::assertChoice(
    transform,
    c("whiten", "scale", "identity"),
    null.ok = TRUE)
  checkmate::assertFlag(resample)
  checkmate::assertCharacter(transform, null.ok = TRUE, len = 1)
  checkmate::assertFunction(prediction, null.ok = TRUE)
  checkmate::assertCharacter(variable, null.ok = TRUE)


  log_component_name <- ifelse(
    component == "prior",
    log_prior_name,
    log_lik_name
  )


  # handle selection as either numeric or character
  orig_selection <- selection

  if (is.numeric(selection)) {
      selection <- paste0(log_component_name, "[", selection, "]")
  } else if (is.character(selection)) {
    selection <- paste0(log_component_name, "_", selection)
  } else if (is.null(selection)) {
    selection <- log_component_name
  }

  draws <- x$draws

  if (is.null(k_threshold)) {
    k_threshold <- min(1 - 1 / log10(posterior::ndraws(draws)), 0.7)
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
    log_comp_draws <- posterior::subset_draws(
      log_comp_draws, variable = selection
    )

    # sum component draws
    log_comp_draws <- rowsums_draws(log_comp_draws)

    # calculate the log weights
    log_ratios <- scaled_log_ratio(
      component_draws = log_comp_draws,
      alpha = alpha,
      )

    if (is_constant(log_ratios)) {
      stop2(
        paste0("Log ", component,
               " is constant. Power-scaling will not work in this case.")
      )
    }

    if (moment_match) {

      require_package(
        "iwmm",
        message = paste0(
          " to use moment matching,",
          "available from https://github.com/topipa/iwmm"
        )
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
        log_prior_name = log_prior_name,
        log_lik_name = log_lik_name,
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
      smoothed_log_ratios <- suppressWarnings(posterior::pareto_smooth(
        log_ratios,
        r_eff = NULL,
        return_k = TRUE,
        extra_diags = TRUE,
        are_log_weights = TRUE,
        verbose = FALSE
      ))

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
      selection = orig_selection,
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
