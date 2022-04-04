##' @rdname powerscale-overview
##' @export
powerscale_sequence <- function(x, ...) {
  UseMethod("powerscale_sequence")

}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.CmdStanFit <- function(x, lower_alpha = 0.5,
                                           upper_alpha = 1/lower_alpha,
                                           alpha_step = 0.1, variable = NULL,
                                           component = c("prior", "likeliood"),
                                           ...
                                           ) {

  psd <- create_powerscaling_data.CmdStanFit(x, ...)

  powerscale_sequence.powerscaling_data(
    psd,
    ...
  )

}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.stanfit <- function(x, lower_alpha = 0.5,
                                        upper_alpha = 1/lower_alpha,
                                        alpha_step = 0.1, variable = NULL,
                                        component = c("prior", "likelihood"),
                                        ...
                                        ) {

  psd <- create_powerscaling_data.stanfit(x, ...)

  powerscale_sequence.powerscaling_data(
    psd,
    ...
  )

}


##' @rdname powerscale-overview
##' @export
powerscale_sequence.brmsfit <- function(x,
                                        ...
                                        ) {

  psd <- create_powerscaling_data.brmsfit(x, ...)

  powerscale_sequence.powerscaling_data(psd, ...)

}



##' @rdname powerscale-overview
##' @export
powerscale_sequence.powerscaling_data <- function(x, lower_alpha = 0.5,
                                                  upper_alpha = 1/lower_alpha,
                                                  alpha_step = 0.1, variable = NULL,
                                                  component = c("prior", "likelihood"),
                                                  is_method = "psis",
                                                  moment_match = FALSE,
                                                  k_threshold = 0.5,
                                                  resample = FALSE,
                                                  transform = FALSE,
                                                  ...
                                                  ) {

  alpha_seq <- seq(lower_alpha, 1 - alpha_step, alpha_step)
  alpha_seq <- c(alpha_seq, rev(1 / alpha_seq))

  # extract the base draws
  base_draws <- posterior::subset_draws(x$draws, variable = variable, ...)

  if (transform == "whiten") {
    base_draws_tr <- whiten_draws(base_draws, ...)
    transform_details = list(
      transform = transform,
      loadings = stats::cor(
        base_draws_tr[,1:posterior::nvariables(base_draws_tr)],
        base_draws[,1:posterior::nvariables(base_draws)]
      )
    )

    base_draws <- base_draws_tr
  } else if (transform == "scale") {
    base_draws <- scale_draws(base_draws, ...)
    transform_details = list(transform = transform)
  } else {
    transform_details = list(transform = transform)
  }



  scaled_draws_list <- vector("list", length(alpha_seq))

  likelihood_scaled <- NULL
  prior_scaled <- NULL

  if ("prior" %in% component) {

    scaled_component <- "prior"

    for (i in seq_along(alpha_seq)) {

      # calculate the scaled draws
      scaled_draws_list[[i]] <- powerscale(
        x = x,
        variable = variable,
        component = scaled_component,
        alpha = alpha_seq[i],
        is_method = is_method,
        moment_match = moment_match,
        resample = resample,
        transform = transform,
        ...
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
        x = x,
        variable = variable,
        component = scaled_component,
        alpha = alpha_seq[i],
        is_method = is_method,
        moment_match = moment_match,
        k_treshold = k_threshold,
        resample = resample,
        transform = transform,
        ...
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
    resampled = resample,
    transform = transform_details
  )

  class(out) <- c("powerscaled_sequence", class(out))

  return(out)

}
