get_draws_stanfit <- function(x, variable = NULL,
                              excluded_variables = c(
                                "lprior", "lp__", "log_lik"
                              ),
                              ...) {
  if (is.null(variable)) {
    draws <- remove_unwanted_vars(x)
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
    draws <- remove_unwanted_vars(x)
  } else {
    draws <- posterior::as_draws_df(x$draws(variable))
  }
  return(draws)
}
