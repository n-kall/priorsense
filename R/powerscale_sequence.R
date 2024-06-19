##' @rdname powerscale-overview
##' @export
powerscale_sequence <- function(x, ...) {
  UseMethod("powerscale_sequence")

}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.default <- function(x,
                                        lower_alpha = 0.8,
                                        upper_alpha = 1 / lower_alpha,
                                        length = 3, variable = NULL,
                                        component = c("prior", "likelihood"),
                                        moment_match = FALSE,
                                        k_threshold = 0.5,
                                        resample = FALSE,
                                        transform = NULL,
                                        prediction = NULL,
                                        auto_alpha_range = FALSE,
                                        symmetric = TRUE,
                                        prior_selection = NULL,
                                        likelihood_selection = NULL,
                                        ...) {
  psd <- create_priorsense_data(x, ...)
  powerscale_sequence(
    psd,
    lower_alpha = lower_alpha,
    upper_alpha = upper_alpha,
    length = length,
    variable = variable,
    component = component,
    moment_match = moment_match,
    k_threshold = k_threshold,
    resample = resample,
    transform = transform,
    prediction = prediction,
    auto_alpha_range = auto_alpha_range,
    symmetric = symmetric,
    prior_selection = prior_selection,
    likelihood_selection = likelihood_selection,
    ...
  )
}

##' @rdname powerscale-overview
##' @export
powerscale_sequence.priorsense_data <- function(x, lower_alpha = 0.8,
                                                upper_alpha = 1 / lower_alpha,
                                                length = 3, variable = NULL,
                                                component = c("prior", "likelihood"),
                                                moment_match = FALSE,
                                                k_threshold = 0.5,
                                                resample = FALSE,
                                                transform = NULL,
                                                prediction = NULL,
                                                auto_alpha_range = FALSE,
                                                symmetric = TRUE,
                                                prior_selection = NULL,
                                                likelihood_selection = NULL,
                                                ...
                                                ) {

  # input checks
  checkmate::assertFunction(prediction, null.ok = TRUE)
  checkmate::assertSubset(component, c("prior", "likelihood"))
  checkmate::assertNumber(lower_alpha)
  checkmate::assertNumber(upper_alpha)
  checkmate::assertNumber(length)
  checkmate::assertLogical(moment_match, len = 1)
  checkmate::assertLogical(symmetric, len = 1)
  checkmate::assertNumber(k_threshold, null.ok = TRUE)
  checkmate::assertLogical(resample, len = 1)
  checkmate::assertChoice(transform, c("whiten", "scale", "identity"), null.ok = TRUE)
  checkmate::assertFunction(prediction, null.ok = TRUE)
  checkmate::assertCharacter(variable, null.ok = TRUE)
  checkmate::assertNumeric(prior_selection, null.ok = TRUE)
  checkmate::assertNumeric(likelihood_selection, null.ok = TRUE)

  

  # adapt alpha range to ensure pareto-k < theshold
  if (auto_alpha_range) {
    alpha_range <- list(prior = NULL, likelihood = NULL)
    for (comp in component) {
      lower_alpha <- find_alpha_threshold(
        x,
        component = comp,
        alpha_bound = lower_alpha,
        moment_match = moment_match
      )
      upper_alpha <- find_alpha_threshold(
        x,
        component = comp,
        alpha_bound = upper_alpha,
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
    base_draws <- posterior::bind_draws(base_draws, pred_draws)
  }

  if (is.null(transform)) {
    transform <- "identity"
  }
  if (transform == "whiten") {
    base_draws_tr <- whiten_draws(base_draws, ...)
    transform_details <- list(
      transform = transform,
      loadings = attr(base_draws_tr, "loadings")
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
    moment_match = moment_match,
    resampled = resample,
    transform = transform_details
  )

  class(out) <- c("powerscaled_sequence", class(out))

  return(out)

}
