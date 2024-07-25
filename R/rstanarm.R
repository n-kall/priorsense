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
  if (is.null(priors$prior$location)) {
    stop("Priors or location attribute is missing.")
  }
  vars <- names(priors$prior$location)

  # Handle Intercept
  if ("(Intercept)" %in% vars) {
    intercept_index <- which(vars == "(Intercept)")
    vars[intercept_index] <- "theta[1]"
    for (i in (intercept_index + 1):length(vars)) {
      vars[i] <- paste0("theta[", i, "]")
    }
  } else {
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
  print("Loading additional libraries")
  library(extraDistr)
  
  # Directly use the function returned by extract_and_create_function
  prior_function <- extract_and_create_function(x)
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
    "laplace" = "dlaplace",
    "lasso" = "dlasso",
    "dirichlet" = "ddirichlet"
  )

  fit_summary <- summary(x)
  priors <- attr(fit_summary, "priors")
  
  if (is.null(priors)) {
    stop("No priors found in model summary.")
  }
  
  draws <- as_draws(x)
  vars <- posterior::variables(draws)

  # Construct the prior equations
  prior_eq <- list()

  # Check if a common prior is used across all coefficients
  common_dist <- priors$prior$dist
  if (length(unique(common_dist)) == 1 && !is.null(dist_to_density[[common_dist[1]]])) {
    # Apply the same distribution to all non-intercept coefficients
    dist_func <- dist_to_density[[common_dist[1]]]
    for (var in vars) {
      if (var != "(Intercept)") {
        prior_eq[[var]] <- paste0(
          dist_func, "(`", var, "`, ", 
          priors$prior$location, ", ", 
          priors$prior$scale, ", log = TRUE)"
        )
      }
    }
  }

  # Handle intercept separately if present
  if ("(Intercept)" %in% vars) {
    intercept_prior <- priors$prior_intercept
    dist_func <- dist_to_density[[intercept_prior$dist]]
    prior_eq["(Intercept)"] <- paste0(
      dist_func, "(`(Intercept)`, ", 
      intercept_prior$location, ", ", 
      intercept_prior$adjusted_scale, ", log = TRUE)"
    )
  }

  # Combine all priors into a single expression
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
