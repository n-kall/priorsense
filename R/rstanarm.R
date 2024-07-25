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

extract_and_create_function <- function(x) {
  # Extract the priors
  prior_string <- extract_stanreg_prior(x)

  # Extract variable names from the model
  fit_summary <- summary(x)
  priors <- attr(fit_summary, "priors")
  vars <- names(priors$prior$location)  # Assuming location is always present
  
  # Handle Intercept
  if ("(Intercept)" %in% vars) {
    intercept_index <- which(vars == "(Intercept)")
    vars[intercept_index] <- "theta[1]"  # Assuming theta[1] is always the intercept if present
    # Adjust other indices accordingly
    for (i in (intercept_index + 1):length(vars)) {
      vars[i] <- paste0("theta[", i, "]")
    }
  } else {
    # Map vars to theta indices directly if no intercept or already handled
    vars <- sapply(seq_along(vars), function(i) paste0("theta[", i, "]"))
  }

  # Replace variable names in prior string with theta indices
  modified_prior_string <- prior_string
  for (i in seq_along(vars)) {
    modified_prior_string <- gsub(sprintf("`%s`", names(priors$prior$location)[i]), vars[i], modified_prior_string)
  }

  # Create the function expression
  func_expression <- paste("function(theta) {", modified_prior_string, "}")
  prior_function <- eval(parse(text = func_expression))

  return(prior_function)
}

log_prior_pdf <- function(x, theta){
    library(VGAM)
    library(LaplacesDemon)
    prior_function <- extract_and_create_function(x)
    create_prior_function <- function(prior_string) {
    func_expression <- paste("function(theta) {", prior_string, "}")
    prior_function <- eval(parse(text = func_expression))
    return(prior_function)
  }

  prior_function <- create_prior_function(prior_string)
  log_probability <- prior_function(theta)
  return(log_probability)
}

extract_stanreg_prior <- function(x) {
  # Mapping distributions to their corresponding density functions
  dist_to_density <- list(
    "normal" = "dnorm",
    "student_t" = "dt",
    "cauchy" = "dcauchy",
    "exponential" = "dexp",
    "laplace" = "dlaplace",  ## From the VGAM package
    "lasso" = "dlasso",      ## From the LaplacesDemon package
    "hs" = "dhs",            ## From the LaplacesDemon package
    "dirichlet" = "ddirichlet" ## From the LaplacesDemon package
  )

  fit_summary <- summary(x)
  priors <- attr(fit_summary, "priors")

  draws <- as_draws(x)
  vars <- posterior::variables(draws)

  prior_eq <- list()

  # Handle intercept prior
  if ("(Intercept)" %in% vars) {
    prior_eq["(Intercept)"] <- paste0(
      dist_to_density[[priors$prior_intercept$dist]],
      "(",
      "`(Intercept)`",
      ", ",
      priors$prior_intercept$location,
      ", ",
      priors$prior_intercept$adjusted_scale,
      ", log = TRUE)"
    )
    vars <- vars[which(vars != "(Intercept)")]  # Remove intercept from vars
  }

  # Handle coefficient prior
  for (b in seq_along(vars)) {
    print(vars[b])
    dist_name <- priors$prior$dist[[b]]
    dist_func <- dist_to_density[[dist_name]]
    args_list <- list()

    switch(dist_name,
      normal = {
        args_list <- c("`", vars[[b]], "`", ", ", priors$prior$location[[b]], ", ", priors$prior$scale[[b]])
      },
      student_t = {
        args_list <- c("`", vars[[b]], "`", ", ", priors$prior$df[[b]], ", ", priors$prior$location[[b]], ", ", priors$prior$scale[[b]])
      },
      cauchy = {
        args_list <- c("`", vars[[b]], "`", ", ", priors$prior$location[[b]], ", ", priors$prior$scale[[b]])
      },
      exponential = {
        args_list <- c("`", vars[[b]], "`", ", ", "1/", priors$prior$rate[[b]])
      },
      laplace = {
        args_list <- c("`", vars[[b]], "`", ", ", priors$prior$location[[b]], ", ", priors$prior$scale[[b]])
      },
      lasso = {
        args_list <- c("`", vars[[b]], "`", ", ", priors$prior$sigma[[b]], ", ", priors$prior$tau[[b]], ", ", priors$prior$lambda[[b]], ", a=1, b=1")
      },
      hs = {
        args_list <- c("`", vars[[b]], "`", ", ", priors$prior$lambda[[b]], ", ", priors$prior$tau[[b]])
      },
      dirichlet = {
        args_list <- c("`", vars[[b]], "`", ", ", priors$prior$alpha[[b]])
      }
    )
    
    # Construct the prior equation
    args_list <- c(args_list, ", log = TRUE)")
    prior_eq[[vars[[b]]]] <- do.call("paste0", c(list(dist_func, "("), args_list))
  }

  # Handle auxiliary parameters, if they exist
  if (!is.null(priors$prior_aux)) {
    aux_vars <- names(priors$prior_aux)
    for (aux in aux_vars) {
      dist_name <- priors$prior_aux[[aux]]$dist
      if (dist_name %in% names(dist_to_density)) {
        dist_func <- dist_to_density[[dist_name]]
        args_list <- list(
          aux, 
          priors$prior_aux[[aux]]$location, 
          priors$prior_aux[[aux]]$scale
        )
        args_list <- c(args_list, ", log = TRUE)")
        prior_eq[[aux]] <- do.call("paste0", c(list(dist_func, "("), args_list))
      }
    }
  }

  # Return the combined prior equations
  return(paste0(prior_eq, collapse = " + "))
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
