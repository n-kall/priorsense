##' Power-scaling sensitivity analysis
##'
##' Calculates the prior/likelihood sensitivity based on power-scaling
##' perturbations. This is done using importance sampling (and
##' optionally moment matching).
##' @template fit_arg
##' @name powerscale-sensitivity
##' @param x Model fit object or priorsense_data object.
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
##' @template selection_arg
##' @param num_args (named list) Optional arguments passed to
##'   [num()][tibble::num] for pretty printing of summaries. Can be
##'   controlled globally via the `posterior.num_args`
##'   [option][base::options].
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
                                           variable = NULL,
                                           lower_alpha = 0.99,
                                           upper_alpha = 1.01,
                                           div_measure = "cjs_dist",
                                           measure_args = list(),
                                           component = c(
                                             "prior",
                                             "likelihood"
                                           ),
                                           sensitivity_threshold = 0.05,
                                           moment_match = FALSE,
                                           k_threshold = 0.5,
                                           resample = FALSE,
                                           transform = NULL,
                                           prediction = NULL,
                                           prior_selection = NULL,
                                           likelihood_selection = NULL,
                                           num_args = NULL,
                                           ...
                                           ) {

  psd <- create_priorsense_data(
    x = x,
    ...
  )

  powerscale_sensitivity.priorsense_data(
    psd,
    variable = variable,
    lower_alpha = lower_alpha,
    upper_alpha = upper_alpha,
    div_measure = div_measure,
    measure_args = measure_args,
    sensitivity_threshold = sensitivity_threshold,
    moment_match = moment_match,
    k_threshold = k_threshold,
    resample = resample,
    transform = transform,
    prediction = prediction,
    prior_selection = prior_selection,
    likelihood_selection = likelihood_selection,
    num_args = num_args,
    ...
  )

}

##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.priorsense_data <- function(x,
                                                   variable = NULL,
                                                   lower_alpha = 0.99,
                                                   upper_alpha = 1.01,
                                                   div_measure = "cjs_dist",
                                                   measure_args = list(),
                                                   component = c(
                                                     "prior",
                                                     "likelihood"
                                                   ),
                                                   sensitivity_threshold = 0.05,
                                                   moment_match = FALSE,
                                                   k_threshold = 0.5,
                                                   resample = FALSE,
                                                   transform = NULL,
                                                   prediction = NULL,
                                                   prior_selection = NULL,
                                                   likelihood_selection = NULL,
                                                   num_args = NULL,
                                                   ...) {
  # input checks
  checkmate::assertCharacter(variable, null.ok = TRUE)
  checkmate::assertNumber(lower_alpha, lower = 0, upper = 1)
  checkmate::assertNumber(upper_alpha, lower = 1)
  checkmate::assertCharacter(div_measure, len = 1)
  checkmate::assertList(measure_args)
  checkmate::assertLogical(moment_match, len = 1)
  checkmate::assertSubset(component, c("prior", "likelihood"))
  checkmate::assertNumber(sensitivity_threshold, lower = 0)
  checkmate::assertNumber(k_threshold, null.ok = TRUE)
  checkmate::assertLogical(resample, len = 1)
  checkmate::assertCharacter(transform, null.ok = TRUE, len = 1)
  checkmate::assertFunction(prediction, null.ok = TRUE)
  checkmate::assertNumeric(prior_selection, null.ok = TRUE)
  checkmate::assertNumeric(likelihood_selection, null.ok = TRUE)

  gradients <- powerscale_gradients(
    x = x,
    variable = variable,
    component = component,
    type = "divergence",
    lower_alpha = lower_alpha,
    upper_alpha = upper_alpha,
    moment_match = moment_match,
    div_measure = div_measure,
    measure_args = measure_args,
    transform = transform,
    resample = resample,
    prediction = prediction,
    prior_selection = prior_selection,
    likelihood_selection = likelihood_selection,
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

  varnames <- unique(c(as.character(gradients$divergence$prior$variable),
                       as.character(gradients$divergence$likelihood$variable)))

  sense <- tibble::tibble(
    variable = varnames,
    prior = prior_sense,
    likelihood = lik_sense
  )

  # categorise variables has prior-data conflict or uninformative
  # likelihood

  sense$diagnosis <- ifelse(
    sense$prior >= sensitivity_threshold & sense$likelihood >= sensitivity_threshold, "prior-data conflict",
    ifelse(sense$prior > sensitivity_threshold & sense$likelihood < sensitivity_threshold,
           "prior domination / weak likelihood",
           "-"
           )
  )

  out <- sense

  class(out) <- c("powerscaled_sensitivity_summary", class(out))

  attr(out, "num_args") <- num_args
  attr(out, "div_measure") <- div_measure
  attr(out, "loadings") <- as.data.frame(gradients$loadings)

  return(out)
}


##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.CmdStanFit <- function(x,
                                              ...
                                              ) {

  psd <- create_priorsense_data.CmdStanFit(x)

  powerscale_sensitivity.priorsense_data(
    psd,
    ...
  )
}

##' @rdname powerscale-sensitivity
##' @export
powerscale_sensitivity.stanfit <- function(x,
                                           ...
                                           ) {

  psd <- create_priorsense_data.stanfit(x, ...)

  powerscale_sensitivity.priorsense_data(
    psd,
    ...
  )
}
