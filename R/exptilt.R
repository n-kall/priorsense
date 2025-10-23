##' Exponential tilting perturbation
##'
##' Estimate posterior draws based on exponential perturbations of
##' prior using importance sampling (and optionally moment matching).
##'
##' @name exptilt-overview
##'
##' @template fit_arg
##' @param variable Vector of variable names to return estimated
##'   posterior draws for. If `NULL` all variables will be included.
##' @param variables Alias of `variable`.
##' @template selection_arg
##' @template prediction_arg
##' @param ... Further arguments passed to internal functions.
##' @return An `exptilted_draws` or `exptilted_sequence` object,
##'   which contains the estimated posterior draws resulting from the
##'   power-scaling perturbations and details of the perturbation and
##'   estimation methods.
##' @template powerscale_references
##' @srrstats {G2.0} Assertions are made on the lengths of inputs via the checkmate package
##' @srrstats {G2.1} Assertions on types of inputs are made via the checkmate package
##' @examples
##' ex <- example_powerscale_model()
##'
##' exptilt(ex$draws, tilted_variable = "sigma", eta = 0.5)
##'
##' @export
exptilt <- function(x, ...) {
  UseMethod("exptilt")
}


##' @rdname exptilt-overview
##' @export
exptilt.default <- function(x, tilted_variable, eta, tilt_fun = I,
                            moment_match = FALSE,
                            k_threshold = NULL,
                            resample = FALSE,
                            transform = NULL,
                            prediction = NULL,
                            variable = NULL,
                            variables = NULL,
                            tilt_fun_args = NULL,
                            ...) {

  psd <- create_priorsense_data(
    x,
    ...
  )
  exptilt(
    psd,
    tilted_variable = tilted_variable,
    eta = eta,
    tilt_fun = tilt_fun,
    moment_match = moment_match,
    k_threshold = k_threshold,
    resample = resample,
    transform = transform,
    prediction = prediction,
    variable = variable,
    variables = variables,
    tilt_fun_args = tilt_fun_args
  )
}


##' @rdname exptilt-overview
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
exptilt.priorsense_data <- function(x,
                                    tilted_variable,
                                    eta,
                                    tilt_fun = I,
                                    moment_match = FALSE,
                                    k_threshold = NULL,
                                    resample = FALSE,
                                    transform = NULL,
                                    prediction = NULL,
                                    variable = NULL,
                                    variables = NULL,
                                    tilt_fun_args = NULL,
                                    ...) {

  # input coercion
  eta <- as.numeric(eta)
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

  # input checks
  checkmate::assertNumber(eta)
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


  draws <- x$draws

  tilted_variable_draws <- posterior::extract_variable(draws, variable = tilted_variable)
  
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


  # if eta is 0, just return the draws with the exptilting details
  if (eta == 0) {

    new_draws <- draws

    # create object with details of power-scaling
    exptilting_details <- list(
      eta = eta,
      tilted_variable = tilted_variable,
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
    class(exptilting_details) <- "exptilting_details"


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

    # calculate the log weights
    log_ratios <- exptilt_log_ratio(
      tilted_variable_draws,
      eta = eta,
      tilt_fun = tilt_fun,
      tilt_fun_args = tilt_fun_args
    )

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

      mm <- iwmm::moment_match(
        x = x$fit,
        log_ratio_fun = x$log_ratio_fn,
        eta = eta,
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
    exptilting_details <- list(
      eta = eta,
      tilted_variable = tilted_variable,
      moment_match = moment_match,
      diagnostics = smoothed_log_ratios$diagnostics,
      resampled = resample,
      transform_details = transform_details
    )
    class(exptilting_details) <- "exptilting_details"

  }

  # return draws and details
  exptilted_draws <- list(
    draws = new_draws,
    exptilting = exptilting_details
  )


  attr(new_draws, "exptilting") <- exptilting_details

  class(new_draws) <- c("exptilted_draws", class(new_draws))


  return(new_draws)

}
