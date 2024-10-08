##' brms predictions as draws
##'
##' Create predictions using brms functions and convert them into
##' draws format
##'
##' @param x brmsfit object
##' @param predict_fn function for predictions
##' @param prediction_names optional names of the predictions
##' @param warn_dims throw a warning when coercing predict_fn's output from 3
##'   margins to 2 margins?
##' @param ... further arguments passed to predict_fn
##' @return draws array of predictions
##' @examples
##' \dontrun{
##' library(brms)
##'
##' if ("log_prior_draws.brmsfit" %in% methods(log_prior_draws) &&
##'     ("log_lik_draws.brmsfit" %in% methods(log_lik_draws))) {
##'   fit <- brm(
##'     yield ~ N * P * K,
##'     data = npk,
##'     prior = prior(normal(0, 1), class = "b"),
##'     refresh = 0
##'   )
##'
##'   powerscale_sensitivity(
##'       fit,
##'       variable = "_pred",
##'       prediction = function(x) predictions_as_draws(
##'                                  x, brms::posterior_epred
##'                                )
##'   )
##' }
##' }
##' @export
predictions_as_draws <- function(x, predict_fn, prediction_names = NULL,
                                 warn_dims = getOption("priorsense.warn", TRUE),
                                 ...) {
  require_package("brms")
  terms <- brms::brmsterms(x$formula)
  if(inherits(terms, "mvbrmsterms")) {
    responses <- brms::brmsterms(x$formula)$responses
    mv <- TRUE
  } else {
    responses <- ""
    mv <- FALSE
  }
  pred_draws <- list()
  predictions <- predict_fn(x, ...)
  if (!(mv)) {
    dim_pred <- dim(predictions)
    if (length(dim_pred) == 3) {
      if (warn_dims) {
        warning("coercing predict_fn()'s output from 3 margins to 2 margins ",
                "(by making the former margin 2 nested within blocks which ",
                "correspond to former margin 3)")
      }
      predictions <- array(predictions,
                           dim = c(dim_pred[1], dim_pred[2] * dim_pred[3]))
    } else if (length(dim_pred) > 3) {
      stop("predict_fn() returned an unexpected number of margins (> 3) for ",
           "this univariate model")
    }
    # add additional dimension in univariate case
    dim(predictions) <- c(dim(predictions), 1)
  } else {
    if (length(dim_pred) != 3) {
      stop("predict_fn() returned an unexpected number of margins (!= 3) for ",
           "this multivariate model")
    }
  }
  for (resp in seq_along(responses)) {
    # create draws array of predictions for each response variable
    predicted_draws <- posterior::as_draws_array(
      array(
        predictions[, , resp],
        dim = c(
          posterior::ndraws(x) / posterior::nchains(x),
          posterior::nchains(x), dim(predictions)[2]
        )
      )
    )
    # name predicted variables
    posterior::variables(predicted_draws) <-  c(
      paste0(
        responses[[resp]],
        "_pred[",
        seq_along(posterior::variables(predicted_draws)),
        "]")
    )
    pred_draws[[resp]] <- predicted_draws
  }
  # bind draws from different responses
  out <- posterior::bind_draws(pred_draws)
  if (!(is.null(prediction_names))) {
    posterior::variables(out) <- prediction_names
  }
  out
}
