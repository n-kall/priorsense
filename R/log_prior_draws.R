##' Extract log prior draws
##'
##' Extract log likelihood from fitted model and return as a draws object.
##'
##' @name log_prior_draws
##'
##' @param x Model fit or draws object.
##' @param joint Logical indicating whether to return the joint log prior
##'   or array. Default is FALSE.
##' @param log_prior_name Name of parameter in Stan model
##'   corresponding to log prior, default is "lprior".
##' @param ... Arguments passed to individual methods.
##' @return A draws_array object containing log_prior values.
##' @examples
##' ex <- example_powerscale_model()
##' drw <- ex$draws
##'
##' log_prior_draws(drw)
##'
##' @export
log_prior_draws <- function(x, ...) {
  UseMethod("log_prior_draws")
}


##' @rdname log_prior_draws
##' @export
log_prior_draws.stanfit <- function(x, joint = FALSE, log_prior_name = "lprior", ...) {

  if (!inherits(x, "stanfit"))
    stop("Not a stanfit object.", call. = FALSE)
  if (x@mode != 0)
    stop("Stan model does not contain posterior draws.",
         call. = FALSE)
  if (!requireNamespace("rstan", quietly = TRUE))
    stop("Please load the 'rstan' package.", call. = FALSE)

  checkmate::assert_logical(joint, len = 1)
  checkmate::assert_character(log_prior_name, len = 1)
  
  log_prior <- posterior::subset_draws(
    posterior::as_draws_array(x, variable = log_prior_name),
    variable = log_prior_name
  )

  if (joint) {
    log_prior <- rowsums_draws(log_prior)
    posterior::variables(log_prior) <- log_prior_name
  }

  return(log_prior)
}

##' @rdname log_prior_draws
##' @export
log_prior_draws.CmdStanFit <- function(x, joint = FALSE, log_prior_name = "lprior", ...) {

  checkmate::assert_logical(joint, len = 1)
  checkmate::assert_character(log_prior_name, len = 1)
  
  log_prior <- x$draws(variables = log_prior_name)

  if (joint) {
    log_prior <- rowsums_draws(log_prior)
    posterior::variables(log_prior) <- log_prior_name
  }

  
  return(log_prior)
}

##' @rdname log_prior_draws
##' @export
log_prior_draws.draws <- function(x, joint = FALSE, log_prior_name = "lprior", ...) {

  checkmate::assert_logical(joint, len = 1)
  checkmate::assert_character(log_prior_name, len = 1)
  
  log_prior <- posterior::subset_draws(x, variable = log_prior_name)

  if (joint) {
    log_prior <- rowsums_draws(log_prior)
    posterior::variables(log_prior) <- log_prior_name
  }
  
  return(log_prior)
}
