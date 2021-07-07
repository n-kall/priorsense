##' Extract draws
##' Extract the draws in the form required for power-scaling
##' @param x model fit
##' @param variables which variables to extract draws from
##' @param ... unused
##' @noRd
##' @return draws_df
get_draws <- function(x, variables, ...) {
  UseMethod("get_draws")
}

get_draws.brmsfit <- function(x, variables, ...) {
  return(posterior::as_draws_df(as.array(x, pars = variables)))
}

get_draws.stanfit <- function(x, variables, ...) {
  if (anyNA(variables)) {
    out <- posterior::as_draws_df(as.array(x))
  } else {
    out <- posterior::as_draws_df(as.array(x, pars = variables))
  }

  return(out)
}

get_draws.CmdStanFit <- function(x, variables, ...) {

  if (anyNA(variables)) {
    out <- posterior::as_draws_df(x$draws())
  } else {
    out <- posterior::as_draws_df(x$draws(variables))
  }
  return(out)
}
