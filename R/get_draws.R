get_draws_stanfit <- function(x, variable = NULL,
                              excluded_variables = c(
                                "lprior", "lp__", "log_lik"
                              ),
                              ...) {
  if (is.null(variable)) {

    draws <- posterior::as_draws_df(as.array(x))

    # remove unnecessary variables
    variable <- posterior::variables(draws)

    has_prefix <- sapply(
      excluded_variables,
      function(p) startsWith(variable, p)
    )
    variable <- variable[!(apply(has_prefix, 1, any))]
    draws <- posterior::subset_draws(draws, variable = variable)
  } else {
    draws <- posterior::as_draws_df(as.array(x, pars = variable))
  }

  return(draws)
}

get_draws_CmdStanFit <- function(x, variable = NULL, regex,
                                 excluded_variables = c(
                                   "lprior", "lp__", "log_lik"
                                 ),
                                 ...) {

  if (is.null(variable)) {
    draws <- posterior::as_draws_df(
      x$draws(),
      variable = variable,
      regex = regex
    )

    # remove unnecessary variables
    variable <- posterior::variables(draws)

    has_prefix <- sapply(
      excluded_variables,
      function(p) startsWith(variable, p)
    )
    variable <- variable[!(apply(has_prefix, 1, any))]
    draws <- posterior::subset_draws(draws, variable = variable)
  } else {
    draws <- posterior::as_draws_df(x$draws(variable))
  }
  return(draws)
}
