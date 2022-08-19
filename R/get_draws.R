get_draws_stanfit <- function(x, variable = NULL,
                              excluded_variables = c("log_prior", "lp__"),
                              ...) {
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

get_draws_CmdStanFit <- function(x, variable = NULL, regex,
                                 excluded_variables = c("log_prior", "lp__"),
                                 ...) {

  if (is.null(variable)) {
    draws <- posterior::as_draws_df(
      x$draws(),
      variable = variable,
      regex = regex
    )

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
