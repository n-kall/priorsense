##' @rdname create-priorsense-data
##' @export
create_priorsense_data.stanreg <- function(x, ...) {

    create_priorsense_data.default(
    x = posterior::as_draws(x, ...),
    fit = x,
    log_prior_fn = log_prior_draws,
    log_lik_fn = log_lik_draws,
    log_prior = log_prior_draws(x, ...),
    log_lik = log_lik_draws(x, ...),
    log_ratio_fn = NULL,
    ...
  )

}

extract_stanreg_prior <- function(x) {

  dist_to_density <- list(
    "normal" = "dnorm",
    "exponential" = "dexp"
  )

  fit_summary <- summary(x)
  priors <- attr(fit_summary, "priors")

  draws <- as_draws(x)

  vars <- variables(draws)

  prior_eq <- list()

  # handle intercept prior
  if ("(Intercept)" %in% vars) {

    prior_eq["(Intercept)"] <- paste0(
      dist_to_density[[priors$prior_intercept$dist]],
      "(",
      "`(Intercept)`",
      ",",
      priors$prior_intercept$location,
      ",",
      priors$prior_intercept$adjusted_scale,
      ", log = TRUE)"
    )

    # remove intercept from vars
    vars <- vars[c(which(vars != "(Intercept)"))]  
  }

  # handle coefficient prior

  for (b in 1:length(priors$prior$location)) {
    
    prior_eq[[vars[[b]]]] <- paste0(
      dist_to_density[[priors$prior$dist]],
      "(",
      "`", vars[[b]], "`",
      ",",
      priors$prior$location[[b]],
      ",",
      priors$prior$adjusted_scale[[b]],
      ", log = TRUE)"
    )
  }

  # handle aux parameter

  prior_eq[[priors$prior_aux$aux_name]] <- paste0(
    dist_to_density[[priors$prior_aux$dist]],
    "(",
    priors$prior_aux$aux_name,
    ",",
    "1/", priors$prior_aux$adjusted_scale,
    ", log = TRUE)"
  )

  return(paste0(prior_eq, collapse = "+"))
}

##' @rdname log_prior_draws
##' @export
log_prior_draws.stanreg <- function(x, joint = FALSE, ...) {
  
  prior_fun <- extract_stanreg_prior(x)

  lprior <- apply(
    posterior::as_draws_df(x),
    1,
    function(row) rlang::eval_tidy(parse(text = prior_fun)[[1]], data = as.list(row))
  )

  return(draws_matrix("lprior" = lprior))
}


##' @rdname log_lik_draws
##' @export
log_lik_draws.stanreg <- function(x, joint = FALSE, ...) {

  ll <- rstanarm::log_lik(x)

  ll <- as_draws(ll)
  variables(ll) <- paste0("log_lik[", variables(ll), "]")

  return(ll)
}
