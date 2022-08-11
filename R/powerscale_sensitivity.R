##' Power-scaling sensitivity analysis
##'
##' Calculates the prior/likelihood sensitivity based on power-scaling
##' perturbations. This is done using importance sampling (and
##' optionally moment matching).
##' @template fit_arg
##' @name powerscale-sensitivity
##' @param x Model fit object or powerscaling_data object.
##' @param ... Further arguments passed to functions.
##' @param variable Character vector of variables to check.
##' @param lower_alpha Lower alpha value for gradient calculation.
##' @param upper_alpha Upper alpha value for gradient calculation.
##' @param component Character vector specifying component(s) to scale
##'   (default is both "prior" and "likelihood").
##' @param sensitivity_threshold Threshold for flagging variable as
##'   sensitive to power-scaling.
##' @template div_measure_arg
##' @template powerscale_args
##' @template prediction_arg
##' @template resample_arg
##' @return Table of sensitivity values for each specified variable.
##' @template powerscale_references
##' @export
##'
powerscale_sensitivity <- function(x, ...) {
  UseMethod("powerscale_sensitivity")
}

##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.default <- function(x,
                                           ...
                                           ) {

  psd <- create_powerscaling_data(
    x = x,
    ...
  )

  powerscale_sensitivity.powerscaling_data(
    psd,
    ...)

}

##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.powerscaling_data <- function(x,
                                                     variable = NULL,
                                                     lower_alpha = 0.99,
                                                     upper_alpha = 1.01,
                                                     div_measure = "cjs_dist",
                                                     measure_args = list(),
                                                     component = c("prior", "likelihood"),
                                                     sensitivity_threshold = 0.05,
                                                     is_method = "psis",
                                                     moment_match = FALSE,
                                                     k_threshold = 0.5,
                                                     resample = FALSE,
                                                     transform = FALSE,
                                                     prediction = NULL,
                                                     ...
                                                     ) {
  # input checks
  checkmate::assertClass(x, classes = "powerscaling_data")
  checkmate::assertCharacter(variable, null.ok = TRUE)
  checkmate::assertNumeric(lower_alpha, lower = 0, upper = 1)
  checkmate::assertNumeric(upper_alpha, lower = 1)
  checkmate::assertCharacter(div_measure)
  checkmate::assertList(measure_args)
  checkmate::assertLogical(moment_match)
  checkmate::assertSubset(component, c("prior", "likelihood"))
  checkmate::assertNumber(sensitivity_threshold, lower = 0)
  checkmate::assertCharacter(is_method)
  checkmate::assertNumber(k_threshold)
  checkmate::assertLogical(resample)
  checkmate::assertLogical(transform)
  checkmate::assertFunction(prediction, null.ok = TRUE)



  if (is_method != "psis" & moment_match) {
    # TODO: also allow moment_match if loo::psis function is given as
    # argument
    moment_match <- FALSE
    warning("Moment-matching only works with PSIS. Falling back to moment_match = FALSE")
  }

  gradients <- powerscale_gradients(
    x = x,
    variable = variable,
    component = component,
    type = "divergence",
    lower_alpha = lower_alpha,
    upper_alpha = upper_alpha,
    is_method = is_method,
    moment_match = moment_match,
    div_measure = div_measure,
    measure_args = measure_args,
    transform = transform,
    resample = resample,
    prediction = prediction,
    ...
  )

  prior_sense <- gradients$divergence$prior[[2]]
  lik_sense <- gradients$divergence$likelihood[[2]]

  if (is.null(lik_sense)) {
    lik_sense <- NA
  }

  if (is.null(prior_sense)) {
    prior_sense <- NA
  }

  mean_prior_sign <- gradients$quantities$prior$mean
  mean_lik_sign <- gradients$quantities$likelihood$mean

  varnames <- unique(c(as.character(gradients$divergence$prior$variable),
                as.character(gradients$divergence$likelihood$variable)))

  sense <- tibble::tibble(
    variable = varnames,
    prior = prior_sense,
    likelihood = lik_sense
  )

  # categorise variables has prior-data conflict or uninformative
  # likelihood

  sense$diagnosis <- ifelse(sense$prior >= sensitivity_threshold & sense$likelihood >= sensitivity_threshold, "prior-data conflict",
                            ifelse(sense$prior > sensitivity_threshold & sense$likelihood < sensitivity_threshold,
                                   "weak likelihood",
                                   "-"
                                   )
                            )

  out <- list(
    # order by largest value first
    sensitivity = sense,
    div_measure = div_measure,
    loadings = gradients$loadings
  )

  class(out) <- "powerscaled_sensitivity_summary"

  return(out)
}


##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.CmdStanFit <- function(x,
                                              ...
                                              ) {

  psd <- create_powerscaling_data.CmdStanFit(x)

  powerscale_sensitivity.powerscaling_data(
    psd,
    ...
  )
}

##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.stanfit <- function(x,
                                           ...
                                           ) {

  psd <- create_powerscaling_data.stanfit(x, ...)

  powerscale_sensitivity.powerscaling_data(
    psd,
    ...
  )
}
