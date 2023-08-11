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
##' @param ... arguments passed to methods
##' @return A `priorsense_data` object, which contains the data and
##'   functions to run sensitivity analyses.
##' @export
create_priorsense_data <- function(x, ...) {
  UseMethod("create_priorsense_data")
}

##' @rdname create-priorsense-data
##' @export
create_priorsense_data.default <- function(x,
                                           fit = NULL,
                                           log_prior_fn = NULL,
                                           log_lik_fn = NULL,
                                           log_prior = NULL,
                                           log_lik = NULL,
                                           ...) {
  psd <- list(
    draws = x,
    fit = fit,
    log_prior_fn = log_prior_fn,
    log_lik_fn = log_lik_fn,
    log_prior = log_prior,
    log_lik = log_lik
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
    log_prior_fn = log_prior_stanfit,
    log_lik_fn = log_lik_stanfit,
    log_prior = log_prior_stanfit(x, ...),
    log_lik = log_lik_stanfit(x, ...),
    ...
  )
}

##' @rdname create-priorsense-data
##' @export
create_priorsense_data.CmdStanFit <- function(x, ...) {

  create_priorsense_data.default(
    x = get_draws_CmdStanFit(x, ...),
    fit = x,
    log_prior_fn = log_prior_CmdStanFit,
    log_lik_fn = log_lik_CmdStanFit,
    log_prior = log_prior_CmdStanFit(x, ...),
    log_lik = log_lik_CmdStanFit(x, ...),
    ...
  )
}

##' @rdname create-priorsense-data
##' @export
create_priorsense_data.draws <- function(x, ...) {

  create_priorsense_data.default(
    x = remove_unwanted_vars(x, ...),
#    log_prior_fn = log_prior_draws,
#    log_lik_fn = log_lik_draws,
    log_prior = log_prior_draws(x, ...),
    log_lik = log_lik_draws(x, ...),    
    ...
  )
}
