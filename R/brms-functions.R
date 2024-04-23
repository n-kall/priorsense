##' @rdname create-priorsense-data
##' @export
create_priorsense_data.brmsfit <- function(x, ...) {

  create_priorsense_data.default(
    x = get_draws_brmsfit(x, ...),
    fit = x,
    log_prior = log_prior_draws.brmsfit(x, ...),
    log_lik = log_lik_draws.brmsfit(x, ...),
    log_prior_fn = log_prior_draws,
    log_lik_fn = log_lik_draws,
    log_ratio_fn = powerscale_log_ratio_fun_brmsfit,
    ...
  )
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.brmsfit <- function(x, ...) {
  require_package("brms")

  log_lik <- brms::log_lik(x, ...)

  log_lik <- posterior::as_draws_array(log_lik)

  posterior::variables(log_lik) <- paste0("log_lik[", 1:nvariables(log_lik), "]")

  return(log_lik)
}


##' @rdname log_prior_draws
##' @export
log_prior_draws.brmsfit <- function(x, log_prior_name = "lprior", ...) {

  log_prior <- posterior::subset_draws(
    posterior::as_draws_array(x),
    variable = log_prior_name
  )

  return(log_prior)
}

get_draws_brmsfit <- function(x, variable = NULL, regex = FALSE, log_prior_name = "lprior", ...) {

  excluded_variables <- c(log_prior_name, "lp__")
  draws <- posterior::as_draws_df(x, regex = regex)

  if (is.null(variable)) {
    # remove unnecessary variables
    variable <- posterior::variables(x)
    variable <- variable[!(variable %in% excluded_variables)]

    draws <- posterior::subset_draws(draws, variable = variable)
  }

  return(draws)
}

##' Predictions as draws
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
##' @export
predictions_as_draws <- function(x, predict_fn, prediction_names = NULL,
                                 warn_dims = getOption("priorsense.warn", TRUE),
                                 ...) {
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


powerscale_log_ratio_fun_brmsfit <- function(draws, fit, alpha, component_fn, ...) {

  component_draws <- component_fn(fit)

  component_draws <- rowsums_draws(component_draws)

  component_draws * (alpha - 1)

}
