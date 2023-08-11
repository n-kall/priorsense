##' @rdname create-powerscaling-data
##' @export
create_priorsense_data.brmsfit <- function(x, ...) {

  create_priorsense_data.default(
    x = get_draws_brmsfit(x, ...),
    fit = x,
    log_prior = log_prior_brmsfit(x, ...),
    log_lik = log_lik_brmsfit(x, ...),
    log_prior_fn = log_prior_brmsfit,
    log_lik_fn = log_lik_brmsfit,
    ...
  )
}

##' @rdname powerscale-overview
##' @export
powerscale.brmsfit <- function(x,
                               component,
                               alpha,
                               ...
                               ) {
  psd <- create_priorsense_data.brmsfit(x, ...)

  powerscale.priorsense_data(
    psd,
    component = component,
    alpha = alpha,
    ...
  )

}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.brmsfit <- function(x,
                                        ...
                                        ) {

  psd <- create_priorsense_data.brmsfit(x, ...)

  powerscale_sequence.priorsense_data(psd, ...)

}

##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.brmsfit <- function(x,
                                           ...
                                           ) {

  psd <- create_priorsense_data.brmsfit(x, ...)

  powerscale_sensitivity.priorsense_data(
    psd,
    ...
  )
}


##' @rdname joint_log_lik
##' @export
log_lik_brmsfit <- function(x, ...) {
  require_package("brms")

  nc <- posterior::nchains(x)
  ndrw <- posterior::ndraws(x)/nc

  log_lik <- brms::log_lik(x, ...)

  nobs <- ncol(log_lik)

  log_lik <- array(log_lik, dim = c(ndrw, nc, nobs))

  log_lik <- posterior::as_draws_array(
    log_lik,
  )

  posterior::variables(log_lik) <- paste0("log_lik[", 1:nobs, "]")

  return(log_lik)
}


##' @rdname log_prior
##' @export
log_prior_brmsfit <- function(x, log_prior_name = "lprior", ...) {

  log_prior <- posterior::subset_draws(posterior::as_draws_array(x), variable = log_prior_name)

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

moment_match.brmsfit <- function(x, ...) {

  tryCatch(
    iwmm::moment_match(x = x$fit, ...),
    error = stop("'moment_match = TRUE' is currently unsupported for brms models")
  )

  return(TRUE)
}
  
## moment_match.brmsfit <- function(x, psis, ...) {
##   # ensure compatibility with objects not created in the current R session
##   x$fit@.MISC <- suppressMessages(brm(fit = x, chains = 0))$fit@.MISC
##   mm <- try(iwmm::moment_match.default(
##     x,
##     psis = psis, post_draws = as.matrix,
##     unconstrain_pars = unconstrain_pars.brmsfit,
##     log_prob_upars = log_prob_upars.brmsfit,
##     log_ratio_upars = log_ratio_upars.brmsfit,
##     nchains = posterior::nchains(x),
##     ...
##   ))
##   if (methods::is(mm, "try-error")) {
##     stop(
##       "'moment_match' failed. Did you set 'save_all_pars' ",
##       "to TRUE when fitting your brms model?"
##     )
##   }
##   return(mm)
## }

## unconstrain_pars.brmsfit <- function(x, pars, ...) {
##   unconstrain_pars.stanfit(x$fit, pars = pars, ...)
## }

## log_prob_upars.brmsfit <- function(x, upars, ...) {
##   log_prob_upars.stanfit(x$fit, upars = upars, ...)
## }

## update_pars.brmsfit <- function(x, upars, ...) {
##   x$fit <- update_pars(x$fit, upars = upars, save_old_pars = FALSE, ...)
##   brms::rename_pars(x)
## }

## log_ratio_upars.brmsfit <- function(x, upars, component_fn, samples = NULL,
##                                     subset = NULL, ...) {
##   # do not pass subset or nsamples further to avoid subsetting twice
##   x <- update_pars(x, upars = upars, ...)
##   component_draws <- component_fn(x)
##   scaled_log_ratio(component_draws, ...)
## }
