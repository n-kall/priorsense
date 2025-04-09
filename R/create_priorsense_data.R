##' Create data structure for priorsense
##'
##' Create a data structure that contains all required data and
##' functions for priorsense
##' @name create-priorsense-data
##' @param x an object for which the method is defined or an object coercible to a `posterior::draws` object
##' @param fit a model fit object (only used if x is not a fit object)
##' @param log_prior_fn function to derive log prior from x or fit (if not NULL)
##' @param log_lik_fn function to derive log likelihood from x or fit (if not NULL)
##' @param log_prior draws object from log prior, must be numeric and not include NA, NaN, Inf, -Inf or be constant
##' @param log_lik draws from log likelihood, must be numeric and not include NA, NaN, Inf, -Inf or be constant
##' @param log_ratio_fn function for moment matching
##' @template log_comp_name
##' @param ... arguments passed to methods
##' @return A `priorsense_data` object, which contains the data and
##'   functions to run sensitivity analyses.
##' @srrstats {G2.1, G2.1a} Assertions on inputs and types document
##' @srrstats {G2.4} Input coercion
##' @srrstats {G2.4b} Input coercion with `as.numeric()`
##' @srrstats {G2.4c} Input coercion with `as.character()`
##' @srrstats {G2.4e} Input coercion
##' @srrstats {G2.7} x can be an object coercible to `posterior::draws` object which includes many tabular formats
##' @srrstats {G2.8} the `priorsense_data` class is constructed to
##'   contain all the required data for the primary functions in the package
##' @srrstats {BS3.0} log_prior and log_lik restrictions specified

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

  # input coercion
  x <- posterior::as_draws(x)
  if (!is.null(log_prior)) {
    log_prior <- posterior::as_draws(log_prior)
  }
  if (!is.null(log_lik)) {
    log_lik <- posterior::as_draws(log_lik)
  }
  log_prior_name <- as.character(log_prior_name)
  log_lik_name <- as.character(log_lik_name)

  # input checks
  checkmate::assert_true(posterior::ndraws(x) > 0)
  
  checkmate::assertClass(log_prior, "draws", null.ok = TRUE)
  checkmate::assertClass(log_lik, "draws", null.ok = TRUE)

  checkmate::assertCharacter(log_lik_name, any.missing = FALSE)
  checkmate::assertCharacter(log_prior_name, any.missing = FALSE)

  checkmate::assertFunction(log_prior_fn, null.ok = TRUE)
  checkmate::assertFunction(log_lik_fn, null.ok = TRUE)
  checkmate::assertFunction(log_ratio_fn, null.ok = TRUE)
  
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

  checkmate::assert_false(checkmate::anyMissing(log_prior))
  checkmate::assert_false(checkmate::anyMissing(log_lik))

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
