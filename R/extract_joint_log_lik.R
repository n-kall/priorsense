extract_joint_log_lik <- function(x, ...) {
  UseMethod("extract_joint_log_lik")
}

extract_joint_log_lik.stanfit <- function(x, parameter_name = "log_lik", ...) {
  log_lik <- rowSums(
    x = loo::extract_log_lik(
      stanfit = x,
      parameter_name = parameter_name,
      merge_chains = FALSE
    ),
    dims = 2
  )

  dim(log_lik) <- c(dim(log_lik), 1)
  log_lik <- posterior::as_draws_array(log_lik)
  posterior::variables(log_lik) <- "log_lik"

  return(log_lik)
}

extract_joint_log_lik.brmsfit <- function(x, parameter_name = "log_lik", merge_chains = TRUE, ...) {
  if (!requireNamespace("brms", quietly = TRUE))
    stop("Please load the 'brms' package.", call. = FALSE)

  log_lik <- rowSums(brms::log_lik(x))
  names(log_lik) <- parameter_name
  chains <- x$fit@sim$chains
  
  log_lik <- posterior::draws_array(
    log_lik = log_lik,
    .nchains = chains
  )
    
  return(log_lik)
}

extract_joint_log_lik.CmdStanFit <- function(x, parameter_name = "log_lik", ...) {

  # sum over correct dimension
  log_lik <- rowSums(x = x$draws(variables = parameter_name), dims = 2)

  # retain dimensions
  dim(log_lik) <- c(dim(log_lik), 1)

  # back to draws_array
  log_lik <- posterior::as_draws_array(log_lik)
  posterior::variables(log_lik) <- "log_lik"
  
  return(log_lik)
}


# Notes:
# check loo moment match how functions are handled rather than methods
