##' @export
get_draws_brmsfit <- function(x, variable, regex = FALSE, excluded_variables = c("lprior", "lp__"), ...) {

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

##' @export
get_draws_stanfit <- function(x, variable, excluded_variables = c("log_prior", "lp__"), ...) {
  if (is.null(variable)) {

    draws <- posterior::as_draws_df(as.array(x))

    # remove unnecessary variables
    variable <- posterior::variables(draws)
    variable <- variable[!(variable %in% excluded_variables) &
                             !(startsWith(variable, "log_lik"))]
    draws <- posterior::subset_draws(draws, variable = variable)
  } else {
    draws <- posterior::as_draws_df(as.array(x, pars = variable))
  }

  return(draws)
}

##' @export
get_draws_CmdStanFit <- function(x, variable, regex, excluded_variables = c("log_prior", "lp__"), ...) {

  if (is.null(variable)) {
    draws <- posterior::as_draws_df(x$draws(), variable = variable, regex = regex)

    # remove unnecessary variables
    variable <- posterior::variables(draws)
    variable <- variable[!(variable %in% excluded_variables) &
                             !(startsWith(variable, "log_lik"))]

    draws <- posterior::subset_draws(draws, variable = variable)
  } else {
    draws <- posterior::as_draws_df(x$draws(variable))
  }
  return(draws)
}
