##' Extract draws
##' Extract the draws in the form required for power-scaling
##' @param x model fit
##' @param variable which variable(s) to extract draws from
##' @param ... unused
##' @noRd
##' @return draws_df
get_draws <- function(x, variable, ...) {
  UseMethod("get_draws")
}

get_draws.brmsfit <- function(x, variable, regex = FALSE, excluded_variables = c("lprior", "lp__"), ...) {

  draws <- posterior::as_draws_df(x, variable = variable, regex = regex)
  
  if (is.null(variable)) {
    # remove unnecessary variables
    variable <- posterior::variables(x)
    variable <- variable[!(variable %in% excluded_variables) &
                             !(startsWith(variable, "log_lik"))]
    
    draws <- posterior::subset_draws(draws, variable = variable)
  }
    
  return(draws)
}

get_draws.stanfit <- function(x, variable, excluded_variables = c("log_prior", "lp__"), ...) {
  if (is.null(variable)) {

    draws <- posterior::as_draws_df(as.array(x))

    # remove unnecessary variables
    variable <- posterior::variables(draws)
    variable <- variables[!(variable %in% excluded_variables) &
                             !(startsWith(variable, "log_lik"))]
    draws <- posterior::subset_draws(draws, variable = variable)
  } else {
    draws <- posterior::as_draws_df(as.array(x, pars = variable))
  }

  return(draws)
}

get_draws.CmdStanFit <- function(x, variable, regex, excluded_variables = c("log_prior", "lp__"), ...) {

  if (is.null(variables)) {
    draws <- posterior::as_draws_df(x$draws(), variable = variable, regex = regex)

    # remove unnecessary variables
    variable <- posterior::variables(draws)
    variable <- variables[!(variable %in% excluded_variables) &
                             !(startsWith(variable, "log_lik"))]

    draws <- posterior::subset_draws(draws, variable = variable)
  } else {
    draws <- posterior::as_draws_df(x$draws(variable))
  }
  return(draws)
}
