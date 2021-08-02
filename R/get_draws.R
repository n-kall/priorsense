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

  draws <- posterior::as_draws_df(as.array(x, pars = variables))


  if (anyNA(variables)) {
    # remove unnecessary variables
    variables <- posterior::variables(draws)
    variables <- variables[!(variables %in% c("log_prior", "lp__")) &
                             !(startsWith(variables, "log_lik"))]

    draws <- posterior::subset_draws(draws, variable = variables)
  }

  return(draws)
}

get_draws.stanfit <- function(x, variables, ...) {
  if (anyNA(variables)) {

    draws <- posterior::as_draws_df(as.array(x))

    # remove unnecessary variables
    variables <- posterior::variables(draws)
    variables <- variables[!(variables %in% c("log_prior", "lp__")) &
                             !(startsWith(variables, "log_lik"))]
    draws <- posterior::subset_draws(draws, variable = variables)
  } else {
    draws <- posterior::as_draws_df(as.array(x, pars = variables))
  }

  return(draws)
}

get_draws.CmdStanFit <- function(x, variables, ...) {

  if (anyNA(variables)) {
    draws <- posterior::as_draws_df(x$draws())

    # remove unnecessary variables
    variables <- posterior::variables(draws)
    variables <- variables[!(variables %in% c("log_prior", "lp__")) &
                             !(startsWith(variables, "log_lik"))]

    draws <- posterior::subset_draws(draws, variable = variables)
  } else {
    draws <- posterior::as_draws_df(x$draws(variables))
  }
  return(draws)
}
