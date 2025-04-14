##' Summarise draws
##'
##' Summarise power-scaled draws
##' @param .x An object of class powerscaled_draws
##' @param ... summary functions
##' @param .args arguments for summary functions
##' @param .num_args (named list) Optional arguments passed to
##'   [num()][tibble::num] for pretty printing of summaries. Can be
##'   controlled globally via the `posterior.num_args`
##'   [option][base::options].
##' @param base_draws base draws
##' @param diagnostics boolean, if TRUE include diagnostics for mean
##'   and variance
##' @param div_measures divergence measures
##' @param measure_args arguments for divergence measures
##' @param resample resample draws
##' @noRd
##' @exportS3Method posterior::summarise_draws
summarise_draws.powerscaled_draws <- function(.x,
                                              ...,
                                              .num_args = NULL,
                                              .args = list(),
                                              base_draws = NULL,
                                              diagnostics = FALSE,
                                              div_measures = "cjs_dist",
                                              measure_args = list(),
                                              resample = FALSE) {

  funs <- c(...)
  if (length(funs) == 0) {
    funs <- posterior::default_summary_measures()
  }

  ps_details <- get_powerscaling_details(.x)

  .args <- as.list(.args)

  if (resample && !ps_details$resampled) {
    # only resample if specified and draws are not already resampled
    target_draws <- posterior::resample_draws(
      posterior::merge_chains(.x)
    )
  } else {
    target_draws <- .x
  }

  if (!resample && !ps_details$resampled) {
    # without resampling, only weighted quantities used
    funs <- as.list(
      paste0(funs, "_weighted")
    )

    # add the weights to args
    .args <- c(
      list(weights = stats::weights(.x)),
      .args
    )
  }

  class(target_draws) <- class(target_draws)[-1]

  summ <- posterior::summarise_draws(
    .x = target_draws,
    ... = funs,
    .args = .args,
    .num_args = .num_args
  )


  if (!is.null(base_draws)) {
    # calculate the divergences between the base and target draws
    divergences <- measure_divergence(
      draws1 = posterior::merge_chains(base_draws),
      draws2 = posterior::merge_chains(target_draws),
      measure = div_measures,
      measure_args = measure_args
    )

    summ <- merge(summ, divergences, by = "variable")
  }

  out <- summ

  if (resample) {
    ps_details$resampled <- TRUE
  }

  attr(out, "powerscaling") <- ps_details


  class(out) <- c("powerscaled_draws_summary", class(out))
  return(out)
}

##' Summarise draws
##'
##' @param .x powerscaled_sequence
##' @param ... summary functions
##' @param .args additional arguments to summary functions
##' @param .num_args (named list) Optional arguments passed to
##'   [num()][tibble::num] for pretty printing of summaries. Can be
##'   controlled globally via the `posterior.num_args`
##'   [option][base::options].
##' @param div_measures divergence measures
##' @param measure_args arguments for divergence measures
##' @param resample resample
##' @return powerscaled_sequence_summary
##' @srrstats {BS6.4} equivalent to summary method
##' @noRd
##' @exportS3Method posterior::summarise_draws
summarise_draws.powerscaled_sequence <- function(.x,
                                                 ...,
                                                 .args = list(),
                                                 .num_args = NULL,
                                                 div_measures = "cjs_dist",
                                                 measure_args = list(),
                                                 resample = FALSE) {
  # handle quantity functions
  funs <- unname(c(...))
  # use default functions if unspecified
  if (length(funs) == 0) {
    funs <- posterior::default_summary_measures()
  }

  # extract base draws
  base_draws <- .x$base_draws

  # create summaries for all power-scaled posteriors
  summaries <- data.frame()

  # for base posterior
  base_quantities <- posterior::summarise_draws(
    .x = posterior::merge_chains(base_draws),
    funs,
    .args = .args,
    .num_args = .num_args
  )

  base_quantities[[".powerscale_alpha"]] <- 1
  base_quantities$pareto_k_threshold <- Inf
  base_quantities$pareto_k <- -Inf

  base_distance <- measure_divergence(
    draws1 = posterior::merge_chains(base_draws),
    draws2 = posterior::merge_chains(base_draws),
    measure = div_measures,
    measure_args = measure_args
  )

  base_summary <- merge(
    x = base_quantities,
    y = base_distance,
    by = "variable"
  )

  base_summary_prior <- c()
  base_summary_likelihood <- c()

  # for prior-scaled
  if (!is.null(.x$prior_scaled)) {

    base_summary_prior <- base_summary
    base_summary_prior$component <- "prior"

    # loop over and summarise set of power-scaled posteriors
    for (scaled in .x$prior_scaled$draws_sequence) {

      quantities <- summarise_draws(
        .x = scaled,
        ... = funs,
        .args = .args,
        base_draws = base_draws,
        div_measures = div_measures,
        resample = resample
      )

      ps_details <- get_powerscaling_details(quantities)

      quantities[[".powerscale_alpha"]] <- ps_details$alpha
      quantities$component <- ps_details$component
      quantities$pareto_k <- ps_details$diagnostics$khat
      quantities$pareto_k_threshold <- ps_details$diagnostics$khat_threshold

      summaries <- rbind(summaries, quantities)
    }
  }

  # for likelihood-scaled
  if (!is.null(.x$likelihood_scaled)) {

    base_summary_likelihood <- base_summary
    base_summary_likelihood$component <- "likelihood"

    # loop over and summarise set of power-scaled posteriors
    for (scaled in .x$likelihood_scaled$draws_sequence) {

      quantities <- summarise_draws(
        .x = scaled,
        funs,
        .args = .args,
        base_draws = base_draws,
        div_measures = div_measures,
        resample = resample
      )

      ps_details <- get_powerscaling_details(quantities)

      quantities[[".powerscale_alpha"]] <- ps_details$alpha
      quantities$component <- ps_details$component
      quantities$pareto_k <- ps_details$diagnostics$khat
      quantities$pareto_k_threshold <- ps_details$diagnostics$khat_threshold

      summaries <- rbind(summaries, quantities)
    }
  }

  # join base and perturbed summaries
  summaries <- list(
    rbind(
      base_summary_prior,
      base_summary_likelihood,
      summaries)
  )

  # correctly specify types of variables
  summaries[[1]][["component"]] <- factor(
    summaries[[1]][["component"]],
    levels = c("prior", "likelihood")
  )

  class(summaries) <- c("powerscaled_sequence_summary", class(summaries))

  return(summaries)
}

##' @export
summarise_draws.whitened_draws <- function(.x, ...) {
  class(.x) <- class(.x)[-1]
  summary <- posterior::summarise_draws(.x, ...)
  attr(summary, "loadings") <- attr(.x, "loadings")
  class(summary) <- c("whitened_draws_summary", class(summary))
  return(summary)
}
