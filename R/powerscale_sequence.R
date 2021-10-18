##' @rdname powerscale-overview
##' @export
powerscale_sequence <- function(fit, lower_alpha = 0.5,
                                upper_alpha = 1/lower_alpha,
                                alpha_step = 0.1, variables = NA,
                                component = c("prior", "likelihood"),
                                is_method = "psis",
                                moment_match = FALSE,
                                k_threshold = 0.5,                              
                                resample = FALSE,
                                log_prior_fn = calculate_log_prior,
                                joint_log_lik_fn = extract_joint_log_lik,
                                transform = FALSE,
                                ...
                                ) {

  # input checks
  checkmate::assert_number(lower_alpha, lower = 0, upper = 1)
  checkmate::assert_number(upper_alpha, lower = 1)
  checkmate::assert_number(alpha_step, lower = 0.0001)
  checkmate::assert_character(variables)
  checkmate::assert_number(k_threshold)
  checkmate::assert_character(component)
  checkmate::assert_logical(moment_match)
  checkmate::assert_logical(resample)
  checkmate::assert_function(log_prior_fn)
  checkmate::assert_function(joint_log_lik_fn)
  
  if (moment_match & !(is_method == "psis")) {
    moment_match <- FALSE
    warning("Moment-matching only works with PSIS. Falling back to moment_match = FALSE")
  }

  if (inherits(fit, "CmdStanFit") & moment_match) {
    moment_match <- FALSE
    warning("Moment-matching does not yet work with fits created with cmdstanr. Falling back to moment_match = FALSE")
  }
  
  alpha_seq <- seq(lower_alpha, 1 - alpha_step, alpha_step)
  alpha_seq <- c(alpha_seq, rev(1 / alpha_seq))

  # extract the base draws
  base_draws <- get_draws(fit, variables = variables)

  if (transform == "spherize") {
    base_draws <- spherize_draws(base_draws)
  } else if (transform == "scale") {
    base_draws <- scale_draws(base_draws)
  }

  scaled_draws_list <- vector("list", length(alpha_seq))

  likelihood_scaled <- NULL
  prior_scaled <- NULL

  if ("prior" %in% component) {

    scaled_component <- "prior"

    for (i in seq_along(alpha_seq)) {

      # calculate the scaled draws
      scaled_draws_list[[i]] <- powerscale(
        fit = fit,
        variables = variables,
        component = scaled_component,
        alpha = alpha_seq[i],
        is_method = is_method,
        moment_match = moment_match,
        resample = resample,
        log_prior_fn = log_prior_fn,
        joint_log_lik_fn = joint_log_lik_fn,
        transform = transform
      )

      prior_scaled <- list(
        draws_sequence = scaled_draws_list,
        component = scaled_component
      )

    }
  }
  if ("likelihood" %in% component) {

    scaled_component <- "likelihood"

    for (i in seq_along(alpha_seq)) {

      # calculate the scaled draws
      scaled_draws_list[[i]] <- powerscale(
        fit = fit,
        variables = variables,
        component = scaled_component,
        alpha = alpha_seq[i],
        is_method = is_method,
        moment_match = moment_match,
        k_treshold = k_threshold,
        resample = resample,
        log_prior_fn = log_prior_fn,
        joint_log_lik_fn = joint_log_lik_fn,
        transform = transform
      )

      likelihood_scaled <- list(
        draws_sequence = scaled_draws_list,
        component = scaled_component
      )

    }
  }
  out <- list(
    base_draws = base_draws,
    prior_scaled = prior_scaled,
    likelihood_scaled = likelihood_scaled,
    is_method = is_method,
    moment_match = moment_match,
    resampled = resample
  )

  class(out) <- c("powerscaled_sequence", class(out))

  return(out)

}
