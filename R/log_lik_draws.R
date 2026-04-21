##' Extract log likelihood draws
##'
##' Extract log likelihood from fitted model and return as a draws
##' object.
##'
##' @name log_lik_draws
##'
##' @param x Model fit or draws object.
##' @param joint Logical indicating whether to return the joint log
##'   likelihood or array. Default is FALSE.
##' @param log_lik_name Name of parameter in Stan model corresponding
##'   to log likelihood, default is "log_lik".
##' @param ... Arguments passed to individual methods.
##' @return A draws_array object containing log_lik values.
##' @examples
##' ex <- example_powerscale_model()
##' drw <- ex$draws
##'
##' log_lik_draws(drw)
##'
##' @export
log_lik_draws <- function(x, ...) {
  UseMethod("log_lik_draws")
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.stanfit <- function(x, joint = FALSE,
                                  log_lik_name = "log_lik", ...) {
  
  stan_vars <- names(x)
  log_lik_name <- stan_vars[grepl(pattern = paste0("^", log_lik_name), stan_vars)]
  log_lik <- as.array(x, pars = log_lik_name)

  log_lik <- posterior::as_draws_array(log_lik)

  if (joint) {
    log_lik <- rowsums_draws(log_lik)
    posterior::variables(log_lik) <- log_lik_name
  }
  
  return(log_lik)
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.CmdStanFit <- function(x, joint = FALSE,
                                     log_lik_name = "log_lik", ...) {

  stan_vars <- x$variables()
  stan_vars <- c(stan_vars$parameters, stan_vars$generated_quantities)
  log_lik_name <- stan_vars[grepl(pattern = paste0("^", log_lik_name), stan_vars)]
  log_lik <- x$draws(variables = log_lik_name)

  if (joint) {
    log_lik <- rowsums_draws(log_lik)
    posterior::variables(log_lik) <- log_lik_name
  }
  
  return(log_lik)
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.draws <- function(x, joint = FALSE,
                                log_lik_name = "log_lik", ...) {

  log_lik <- posterior::subset_draws(x, variable = paste0("^", log_lik_name), regex = TRUE)

  if (joint) {
    log_lik <- rowsums_draws(log_lik)
    posterior::variables(log_lik) <- log_lik_name
  }
  
  return(log_lik)
}
