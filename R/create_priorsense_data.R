##' Create data structure for priorsense
##'
##' Create a data structure that contains all required data and
##' functions for priorsense
##' @name create-priorsense-data
##' @param x an object for which the method is defined
##' @param fit a model fit object (only used if x is not a fit object)
##' @param log_prior_fn function to derive log prior from object
##' @param log_lik_fn function to derive log likelihood from object
##' @param log_prior draws from log prior
##' @param log_lik draws from log likelihood
##' @param log_ratio_fn function for moment matching
##' @template log_comp_name
##' @param ... arguments passed to methods
##' @return A `priorsense_data` object, which contains the data and
##'   functions to run sensitivity analyses.
##' @examples
##' x <- example_powerscale_model()
##' drw <- x$draws
##'
##' psd <- create_priorsense_data(drw)
##' @export
create_priorsense_data <- function(x, ...) {
  UseMethod("create_priorsense_data")
}

##' @rdname create-priorsense-data
##' @export
create_priorsense_data.default <- function(x,
                                           fit = NULL,
                                           log_prior_fn = log_prior_draws,
                                           log_lik_fn = log_lik_draws,
                                           log_prior = NULL,
                                           log_lik = NULL,
                                           log_ratio_fn = NULL,
                                           log_prior_name = "lprior",
                                           log_lik_name = "log_lik",
                                           ...) {

  if (is.null(log_prior)) {
    if (is.null(fit)) {
      log_prior <- log_prior_fn(x, log_prior_name = log_prior_name, ...)
    } else {
      log_prior <- log_prior_fn(fit, log_prior_name = log_prior_name, ...)
    }
  }

  if (is.null(log_lik)) {
    if (is.null(fit)) {
      log_lik <- log_lik_fn(x, ...)
    } else {
      log_lik <- log_lik_fn(fit, ...)
    }
  }

  psd <- list(
    draws = remove_unwanted_vars(x),
    fit = fit,
    log_prior_fn = log_prior_fn,
    log_lik_fn = log_lik_fn,
    log_prior = log_prior,
    log_lik = log_lik,
    log_ratio_fn = log_ratio_fn
  )
  
  class(psd) <- c("priorsense_data", class(psd))

  return(psd)
}

##' @rdname create-priorsense-data
##' @export
create_priorsense_data.stanfit <- function(x, ...) {

  create_priorsense_data.default(
    x = get_draws_stanfit(x),
    fit = x,
    log_prior_fn = log_prior_draws,
    log_lik_fn = log_lik_draws,
    log_prior = log_prior_draws(x, ...),
    log_lik = log_lik_draws(x, ...),
    log_ratio_fn = powerscale_log_ratio_fun,
    ...
  )
}

##' @rdname create-priorsense-data
##' @export
create_priorsense_data.CmdStanFit <- function(x, ...) {

  create_priorsense_data.default(
    x = get_draws_CmdStanFit(x, ...),
    fit = x,
    log_prior_fn = log_prior_draws,
    log_lik_fn = log_lik_draws,
    log_prior = log_prior_draws(x, ...),
    log_lik = log_lik_draws(x, ...),
    log_ratio_fn = powerscale_log_ratio_fun,
    ...
  )
}

##' @rdname create-priorsense-data
##' @export
create_priorsense_data.draws <- function(x, ...) {

  create_priorsense_data.default(
    x = x,
    ...
  )
}



##' @rdname create-priorsense-data
##' @export
create_priorsense_data.rjags <- function(x, ...) {

  create_priorsense_data(
    x = posterior::as_draws(x$BUGSoutput$sims.array),
    ...
  )
}
