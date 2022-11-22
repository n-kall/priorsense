SW <- function(...) {
  suppressWarnings(...)
}

rowsums_draws <- function(x) {
  posterior::draws_array(
        sum = rowSums(
          x,
          dims = 2
        ),
        .nchains = posterior::nchains(x)
      )
}

remove_unwanted_vars <- function(x, excluded_variables = c("lprior", "log_lik", "lp__"), regex) {

  draws <- posterior::as_draws_df(x, variable = variable, regex = regex)

    # remove unnecessary variables
    variable <- posterior::variables(draws)

    has_prefix <- sapply(
      excluded_variables,
      function(p) startsWith(variable, p)
    )
    variable <- variable[!(apply(has_prefix, 1, any))]
    draws <- posterior::subset_draws(draws, variable = variable)
  
}
