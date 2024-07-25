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
eval_expr_with_vars <- function(expr_string, vars) {
  # Parse the expression string
  parsed_expr <- parse(text = expr_string)[[1]]
  
  # Create an environment with the variables
  env <- new.env()
  for (var_name in names(vars)) {
    assign(var_name, vars[var_name], envir = env)
  }
  
  # Evaluate the expression in the created environment
  result <- eval(parsed_expr, envir = env)
  result <- as.numeric(result)
  return(result)
}

log_prior_pdf <- function(x, theta){
    prior_string <- extract_stanreg_prior(x)
    val <- eval_expr_with_vars(prior_string, theta)
    return(val)
}
extract_stanreg_prior <- function(x) {
  # Mapping distributions to their corresponding density functions with expected parameters
  dist_to_density <- list(
    "normal" = list(func="dnorm", params=c("mean"="location", "sd"="scale")),
    "student_t" = list(func="dt", params=c("df"="df", "x"="location", "ncp"="scale")),
    "cauchy" = list(func="dcauchy", params=c("location"="location", "scale"="scale")),
    "exponential" = list(func="dexp", params=c("rate"="rate")),
    "laplace" = list(func="dlaplace", params=c("location"="location", "scale"="scale")),
    "lasso" = list(func="dlasso", params=c("location"="location", "scale"="scale")),
    "dirichlet" = list(func="ddirichlet", params=c("alpha"="scale"))
    ##HS, HS PLUS, Product normal and the prior_smooth family is not implemented right now
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

  # Iterate through each variable and apply the appropriate prior
  for (var in vars) {
    if (var == "sigma") {
      # Special handling for sigma variable
      prior_name <- "prior_aux"
    } else {
      prior_name <- if (var == "(Intercept)") "prior_intercept" else "prior"
    }

    if (!is.null(priors[[prior_name]])) {
      dist_name <- priors[[prior_name]]$dist
      dist_info <- dist_to_density[[dist_name]]
      if (!is.null(dist_info)) {
        dist_func <- dist_info$func
        param_mapping <- dist_info$params
        param_values <- sapply(param_mapping, function(p) priors[[prior_name]][[p]])
        if (is.matrix(param_values)) {
            param_values <- param_values[1,]
        }
        # Constructing the function call with parameters
        func_call <- paste0(dist_func, "(`", var, "`, ", paste(param_values, collapse=", "), ", log = TRUE)")
        prior_eq[[var]] <- func_call
      }
    }
  }

  # Combine all priors into a single expression
  return(paste0(prior_eq, collapse = " + "))
}


##' @rdname log_prior_draws
##' @export
log_prior_draws.stanreg <- function(x, joint = FALSE, ...) {  
  # Get the draws as a dataframe
  draws_matrix <- posterior::as_draws_matrix(x)
  # Initialize vector to store log prior values
  lprior <- numeric(nrow(draws_matrix))
  
  # Calculate log prior for each row of the draws object
  for (i in 1:nrow(draws_matrix)) {
    row <- as.vector(draws_matrix[i, ])
    names(row) <- colnames(draws_matrix)
    lprior[i] <- log_prior_pdf(x, row)
  }
  return(posterior::draws_matrix("lprior" = lprior))
}

##' @rdname log_lik_draws
##' @export
log_lik_draws.stanreg <- function(x, joint = FALSE, ...) {

  ll <- rstanarm::log_lik(x)

  ll <- as_draws(ll)
  variables(ll) <- paste0("log_lik[", variables(ll), "]")

  return(ll)
}
