##' Power-scaling sensitivity analysis
##'
##' Prior/likelihood sensitivity analysis based on power-scaling perturbations.
##'
##' @template fit_arg
##' @param variables Character vector of variables to check.
##' @param lower_alpha Lower alpha value for gradient calculation.
##' @param upper_alpha Upper alpha value for gradient calculation.
##' @param component Character vector specifying component(s) to scale (default is both "prior" and
##'   "likelihood").
##' @template div_measure_arg
##' @param sensitivity_threshold Threshold for flagging variable as
##'   sensitive to power-scaling.
##' @template powerscale_args
##' @param ... Further arguments passed to functions.
##' @return Table of sensitivity values for each specified variable.
##' @template powerscale_references
##' @export
powerscale_sensitivity <- function(fit, variables = NA,
                                   lower_alpha = 0.9,
                                   upper_alpha = 1.1,
                                   div_measure = "cjs_dist",
                                   measure_args = list(),
                                   component = c("prior", "likelihood"),
                                   sensitivity_threshold = 0.05,
                                   is_method = "psis",
                                   moment_match = FALSE,
                                   k_threshold = 0.5,
                                   transform = "spherize",
                                   resample = FALSE,
                                   log_prior_fn = calculate_log_prior,
                                   joint_log_lik_fn = extract_joint_log_lik,
                                   ...
                                   ) {

  checkmate::assert_number(lower_alpha, lower = 0, upper = 1)
  checkmate::assert_number(upper_alpha, lower = 1)
  checkmate::assert_character(variables)
  checkmate::assert_choice(div_measure, all_divergence_measures())
  checkmate::assert_list(measure_args)
  checkmate::assert_number(sensitivity_threshold, lower = 0)
  checkmate::assert_number(k_threshold)
  checkmate::assert_character(component)
  checkmate::assert_logical(moment_match)
  checkmate::assert_logical(resample)
  checkmate::assert_function(log_prior_fn)
  checkmate::assert_function(joint_log_lik_fn)

  
  if (is_method != "psis" & moment_match) {
    # TODO: also allow moment_match if loo::psis function is given as
    # argument
    moment_match <- FALSE
    warning("Moment-matching only works with PSIS. Falling back to moment_match = FALSE")
  }

  if (inherits(fit, "CmdStanFit") & moment_match) {
    moment_match <- FALSE
    warning("Moment-matching does not yet work with fits created with cmdstanr. Falling back to moment_match = FALSE")
  }
  
  gradients <- powerscale_gradients(
    fit = fit,
    variables = variables,
    component = component,
    type = c("quantities", "divergence"),
    lower_alpha = lower_alpha,
    upper_alpha = upper_alpha,
    is_method = is_method,
    moment_match = moment_match,
    div_measure = div_measure,
    measure_args = measure_args,
    transform = transform,
    resample = resample,
    joint_log_lik_fn = joint_log_lik_fn,
    log_prior_fn = log_prior_fn,
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

  
  sense <- tibble::tibble(
    # TODO: more elegant way to extract variables
    variable = unique(
      c(
        gradients$divergence$prior$variable,
        gradients$divergence$likelihood$variable
      )
    ),
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
    div_measure = div_measure
  )

  class(out) <- "powerscaled_sensitivity_summary"

  return(out)
}

