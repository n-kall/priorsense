##' @export
summary.powerscaled_draws <- function(object, ...) {

  summarise_draws.powerscaled_draws(object, ...)

}


##' Summarise draws
##'
##' Summarise power-scaled draws
##' @export
##' @importFrom posterior summarise_draws
##' @param .x An object of class powerscaled_draws
##' @param ... summary functions
##' @param .args arguments for summary functions
##' @param base_draws base draws
##' @param div_measures divergence measures
##' @param measure_args arguments for divergence measures
##' @param resample resample draws
summarise_draws.powerscaled_draws <- function(.x,
                                              ...,
                                              .args = list(),
                                              base_draws = NULL,
                                              div_measures = "cjs_dist",
                                              measure_args = list(),
                                              resample = FALSE) {

  funs <- c(...)
  if (length(funs) == 0) {
    funs <- posterior::default_summary_measures()
  }

  .args <- as.list(.args)

  if (resample & !(.x$powerscaling$resampled)) {
    # only resample if specified and draws are not already resampled
    target_draws <- posterior::resample_draws(
      posterior::merge_chains(.x$draws)
    )
  } else {
    target_draws <- .x$draws
  }

  if (!resample & !.x$powerscaling$resampled) {
    # without resampling, only weighted quantities used

    funs <- as.list(
      paste0(funs, "_weighted")
    )

    # add the weights to args
    .args <- c(
      list(weights = stats::weights(.x$draws)),
      .args
    )

  }

  summ <- posterior::summarise_draws(
    target_draws,
    funs,
    .args = .args
  )

  if (!is.null(base_draws)) {
    # calculate the divergences between the base and target draws
    divergences <- divergence_measures(
      draws1 = posterior::merge_chains(base_draws),
      draws2 = posterior::merge_chains(target_draws),
      measure = div_measures,
      measure_args = measure_args
    )

    summ <- merge(summ, divergences, by = "variable")
  }

  out <- list(
    draws_summary = summ,
    powerscaling = .x$powerscaling
  )

  if (resample) {
    out$powerscaling$resampled <- TRUE
  }

  class(out) <- "powerscaled_draws_summary"
  return(out)
}

##' Summarise draws
##'
##' @param .x powerscaled_sequence
##' @param ... summary functions
##' @param .args additional arguments to summary functions
##' @param div_measures divergence measures
##' @param measure_args arguments for divergence measures
##' @param resample resample
##' @return powerscaled_sequence_summary
##' @export
summarise_draws.powerscaled_sequence <- function(.x,
                                                 ...,
                                                 .args = list(),
                                                 div_measures = "cjs_dist",
                                                 measure_args = list(),
                                                 resample = FALSE) {

  funs <- unname(c(...))
  if (length(funs) == 0) {
    funs <- posterior::default_summary_measures()
  }

  base_draws <- .x$base_draws

  summaries <- data.frame()

  if (!is.null(.x$prior_scaled)) {
    for (scaled in .x$prior_scaled$draws_sequence) {

      quantities <- summarise_draws(
        .x = scaled,
        funs,
        base_draws = base_draws,
        div_measures = div_measures,
        resample = resample
      )

      quant_df <- as.data.frame(quantities[[1]])

      quant_df$alpha <- quantities$powerscaling$alpha
      quant_df$component <- quantities$powerscaling$component
      quant_df$pareto_k <- quantities$powerscaling$importance_sampling$diagnostics$pareto_k
      quant_df$n_eff <- quantities$powerscaling$importance_sampling$diagnostics$n_eff

      summaries <- rbind(summaries, quant_df)

    }

  }

  if (!is.null(.x$likelihood_scaled)) {
    for (scaled in .x$likelihood_scaled$draws_sequence) {

      quantities <- summarise_draws(
        .x = scaled,
        funs,
        base_draws = base_draws,
        div_measures = div_measures,
        resample = resample
      )

      quant_df <- as.data.frame(quantities[[1]])

      quant_df$alpha <- quantities$powerscaling$alpha
      quant_df$component <- quantities$powerscaling$component
      quant_df$pareto_k <- quantities$powerscaling$importance_sampling$diagnostics$pareto_k
      quant_df$n_eff <- quantities$powerscaling$importance_sampling$diagnostics$n_eff

      summaries <- rbind(summaries, quant_df)

    }
  }

  base_quantities <- posterior::summarise_draws(
    posterior::merge_chains(base_draws),
    funs
  )
  base_quantities$alpha <- 1
  base_quantities$n_eff <- NA
  if (.x$is_method == "psis") {
    base_quantities$pareto_k <- -Inf
  } else {
    base_quantities$pareto_k <- NA
  }

  base_distance <- divergence_measures(
    draws1 = posterior::merge_chains(base_draws),
    draws2 = posterior::merge_chains(base_draws),
    measure = div_measures,
    measure_args = measure_args
  )

  base_summary <- merge(base_quantities, base_distance, by = "variable")

  base_summary_prior <- c()
  base_summary_likelihood <- c()

  if (!is.null(.x$prior_scaled)) {
    base_summary_prior <- base_summary
    base_summary_prior$component <- "prior"
  }

  if (!is.null(.x$likelihood_scaled)) {
    base_summary_likelihood <- base_summary
    base_summary_likelihood$component <- "likelihood"
  }

  summaries <- list(rbind(base_summary_prior, base_summary_likelihood, summaries))

  summaries[[1]][["component"]] <- factor(summaries[[1]][["component"]], levels = c("prior", "likelihood"))

  class(summaries) <- c("powerscaled_sequence_summary", class(summaries))

  return(summaries)
}
