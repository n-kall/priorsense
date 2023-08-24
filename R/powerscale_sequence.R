##' @rdname powerscale-overview
##' @export
powerscale_sequence <- function(x, ...) {
  UseMethod("powerscale_sequence")

}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.CmdStanFit <- function(x,
                                           ...
                                           ) {

  psd <- create_priorsense_data.CmdStanFit(x, ...)

  powerscale_sequence.priorsense_data(
    psd,
    ...
  )

}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.stanfit <- function(x,
                                        ...
                                        ) {

  psd <- create_priorsense_data.stanfit(x, ...)

  powerscale_sequence.priorsense_data(
    psd,
    ...
  )

}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.priorsense_data <- function(x, lower_alpha = 0.8,
                                                upper_alpha = 1 / lower_alpha,
                                                length = 9, variable = NULL,
                                                component = c("prior", "likelihood"),
                                                is_method = "psis",
                                                moment_match = FALSE,
                                                k_threshold = 0.5,
                                                resample = FALSE,
                                                transform = FALSE,
                                                prediction = NULL,
                                                auto_alpha_range = FALSE,
                                                symmetric = TRUE,
                                                prior_selection = NULL,
                                                likelihood_selection = NULL,
                                                ...
                                                ) {

  # input checks
  checkmate::assertFunction(prediction, null.ok = TRUE)
  # TODO: check other inputs, too

  # adapt alpha range to ensure pareto-k < theshold
  if (auto_alpha_range) {
    alpha_range <- list(prior = NULL, likelihood = NULL)
    for (comp in component) {
      lower_alpha <- find_alpha_threshold(
        x,
        component = comp,
        alpha_bound = lower_alpha,
        k_threshold = k_threshold,
        moment_match = moment_match
      )
      upper_alpha <- find_alpha_threshold(
        x,
        component = comp,
        alpha_bound = upper_alpha,
        k_threshold = k_threshold,
        moment_match = moment_match
      )
      alpha_range[[comp]] <- list(lower_alpha, upper_alpha)
    }
    lower_alpha <- max(
      alpha_range[["prior"]][[1]],
      alpha_range[["likelihood"]][[1]],
      na.rm = TRUE
    )
    upper_alpha <- min(
      alpha_range[["prior"]][[2]],
      alpha_range[["likelihood"]][[2]],
      na.rm = TRUE
    )
  }

  if (!symmetric) {
    alpha_seq <- seq(
      lower_alpha,
      upper_alpha,
      length.out = length
    )
  } else {
    if (abs(log(lower_alpha, 2)) < abs(log(upper_alpha, 2))) {
      alpha_seq_l <- seq(lower_alpha, 1, length.out = length / 2)
      alpha_seq_l <- alpha_seq_l[-length(alpha_seq_l)]
      alpha_seq_u <- rev(1 / alpha_seq_l)
    } else {
      alpha_seq_u <- seq(1, upper_alpha, length.out = length / 2)
      alpha_seq_u <- alpha_seq_u[-1]
      alpha_seq_l <- rev(1 / alpha_seq_u)
    }
    alpha_seq <- c(alpha_seq_l, alpha_seq_u)
  }

  variable_base <- variable
  # compute predictions (necessary at this place to infer the variable names
  # from the predictions in the next step)
  if (!is.null(prediction)) {
    pred_draws <- prediction(x$fit, ...)
  }
  # for retrieving the base draws, we need to exclude the variable names from
  # the predictions
  # TODO: this step might be necessary at other places in the package as well
  # (probably whenever both arguments `prediction` and `variable` (or
  # `variables`) exist)
  if (!is.null(prediction) && !is.null(variable_base)) {
    variable_base <- setdiff(variable_base, posterior::variables(pred_draws))
  }
  # extract the base draws
  base_draws <- posterior::subset_draws(
    x$draws,
    variable = variable_base,
    ...)
  # append predictions
  if (!is.null(prediction)) {
    # bind predictions and posterior draws
    base_draws <- posterior::bind_draws(base_draws, pred_draws)
  }

  if (transform == "whiten") {
    base_draws_tr <- whiten_draws(base_draws, ...)
    transform_details <- list(
      transform = transform,
      loadings = stats::cor(
        base_draws_tr[, 1:posterior::nvariables(base_draws_tr)],
        base_draws[, 1:posterior::nvariables(base_draws)]
      )
    )

    base_draws <- base_draws_tr
  } else if (transform == "scale") {
    base_draws <- scale_draws(base_draws, ...)
    transform_details <- list(transform = transform)
  } else {
    transform_details <- list(transform = transform)
  }



  scaled_draws_list <- vector("list", length(alpha_seq))

  likelihood_scaled <- NULL
  prior_scaled <- NULL

  if ("prior" %in% component) {

    scaled_component <- "prior"

    for (i in seq_along(alpha_seq)) {

      # skip alpha = 1
      if (alpha_seq[i] == 1) {
        next
      }

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
        prediction = prediction,
        selection = prior_selection,
        ...
      )

    }

    prior_scaled <- list(
      draws_sequence = scaled_draws_list,
      component = scaled_component
    )

  }
  if ("likelihood" %in% component) {

    scaled_component <- "likelihood"

    for (i in seq_along(alpha_seq)) {

      # skip alpha = 1
      if (alpha_seq[i] == 1) {
        next
      }

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
        prediction = prediction,
        selection = likelihood_selection,
        ...
      )

    }

    likelihood_scaled <- list(
      draws_sequence = scaled_draws_list,
      component = scaled_component
    )

  }

  out <- list(
    base_draws = base_draws,
    prior_scaled = prior_scaled,
    likelihood_scaled = likelihood_scaled,
    alphas = alpha_seq,
    is_method = is_method,
    moment_match = moment_match,
    resampled = resample,
    transform = transform_details
  )

  class(out) <- c("powerscaled_sequence", class(out))

  return(out)

}
