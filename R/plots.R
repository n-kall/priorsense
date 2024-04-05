##' Diagnostic plots for power-scaling sensitivity
##'
##' Various diagnostic plots for power-scaling sensitivity. See **Plot Descriptions** below for details.
##'
##' @name powerscale_plots
##'
##' @template plot_args
##' @template div_measure_arg
##' @template resample_arg
##' @template ggplot_return
##' @section Plot Descriptions:
##' \describe{
##'   \item{`powerscale_plot_dens()`}{
##'    Kernel density plot of power-scaled posterior draws with respect to power-scaling.
##'   }
##'   \item{`powerscale_plot_ecdf()`}{
##'    Empirical cumulative distribution function plot
##'    of power-scaled posterior draws with respect to power-scaling.
##'   }
##'   \item{`powerscale_plot_quantities()`}{
##'    Plot of posterior quantities with respect to power-scaling.
##'   }
##' }
##'
##' @importFrom rlang .data
NULL

prepare_plot_data <- function(x, variables, resample, ...) {

  base_draws <- posterior::merge_chains(x$base_draws)

  if (!resample && !(x$resampled)) {
    base_draws <- posterior::weight_draws(
      x = base_draws,
      weights = rep(
        1 / posterior::ndraws(base_draws),
        posterior::ndraws(base_draws)
      )
    )
  }

  likelihood_draws <- list()
  prior_draws <- list()
  base_draws_prior <- data.frame()
  base_draws_lik <- data.frame()

  if (!(is.null(x$prior_scaled))) {
    prior_scaled <- x$prior_scaled$draws_sequence

    for (i in seq_along(prior_scaled)) {
      prior_draws[[i]] <- posterior::merge_chains(prior_scaled[[i]]$draws)

      if (resample && !x$resampled) {
        prior_draws[[i]]  <- posterior::resample_draws(prior_draws[[i]])
      }

      prior_draws[[i]]$alpha <- prior_scaled[[i]]$powerscaling$alpha
      prior_draws[[i]]$component <- "prior"
      prior_draws[[i]]$pareto_k <- prior_scaled[[i]]$powerscaling$importance_sampling$diagnostics$pareto_k
    }

    base_draws_prior <- base_draws
    base_draws_prior$alpha <- 1
    base_draws_prior$component <- "prior"
    base_draws_prior$pareto_k <- 0
  }

  if (!(is.null(x$likelihood_scaled))) {

    likelihood_scaled <- x$likelihood_scaled$draws_sequence

    for (i in seq_along(likelihood_scaled)) {
      likelihood_draws[[i]] <- posterior::merge_chains(
        likelihood_scaled[[i]]$draws
      )

      if (resample && !x$resampled) {
        likelihood_draws[[i]]  <- posterior::resample_draws(
          likelihood_draws[[i]]
        )
      }

      likelihood_draws[[i]]$alpha <- likelihood_scaled[[i]]$powerscaling$alpha
      likelihood_draws[[i]]$component <- "likelihood"
      likelihood_draws[[i]]$pareto_k <- likelihood_scaled[[i]]$powerscaling$importance_sampling$diagnostics$pareto_k

    }

    base_draws_lik <- base_draws
    base_draws_lik$alpha <- 1
    base_draws_lik$component <- "likelihood"
    base_draws_lik$pareto_k <- 0
  }

  d <- rbind(
    do.call("rbind", prior_draws),
    do.call("rbind", likelihood_draws),
    base_draws_lik,
    base_draws_prior
  )

  d$pareto_k_value <- ifelse(d$pareto_k > 0.5, "> 0.5",
                             "< 0.5")

  d$pareto_k_value <- factor(
    d$pareto_k_value,
    levels = c("< 0.5", "> 0.5")
  )


  d$component <- factor(d$component, levels = c("prior", "likelihood"))

  # prepare for plotting
  d <- stats::reshape(
    data = as.data.frame(d),
    varying = variables,
    direction = "long",
    times = variables,
    v.names = "value",
    timevar = "variable"
  )

  return(d)
}

prepare_plot <- function(d, resample, variable, ...) {
  if (resample) {
    p <- ggplot2::ggplot(
      data = d,
      ggplot2::aes(
        x = .data$value,
        group = .data$alpha,
        linetype = .data$pareto_k_value
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = d,
      ggplot2::aes(
        x = .data$value,
        weight = exp(.data$.log_weight),
        group = .data$alpha,
        linetype = .data$pareto_k_value
      )
    )
  }

  p <- p +
    ggplot2::scale_linetype_manual(
      values = c("solid", "dashed", "dotted"),
      drop = FALSE
    ) +
    ggplot2::scale_color_gradientn(
      colours = cetcolor::cet_pal(3, "d8"),
      trans = "log",
      limits = c(min(d$alpha) - 0.01, max(d$alpha) + 0.01),
      breaks = c(min(d$alpha), 1, max(d$alpha)),
      labels = c(
        round(min(d$alpha), digits = 3),
        "1",
        round(max(d$alpha), digits = 3)
      )
    ) +
    ggplot2::scale_fill_gradientn(
      colours = cetcolor::cet_pal(3, "d8"),
      trans = "log",
      limits = c(min(d$alpha) - 0.01, max(d$alpha) + 0.01),
      breaks = c(min(d$alpha), 1, max(d$alpha)),
      labels = c(
        round(min(d$alpha), digits = 3),
        "1",
        round(max(d$alpha), digits = 3)
      )
    ) +
    ggplot2::theme(aspect.ratio = 1)

  return(p)

}

##' @rdname powerscale_plots
##' @export
powerscale_plot_dens <- function(x, variables, resample = FALSE,
                                 ...) {
  # input checks
  checkmate::assert_class(x, c("powerscaled_sequence"))
  checkmate::assert_character(variables)
  checkmate::assert_logical(resample)

  d <- prepare_plot_data(x, variables, resample, ...)

  if (resample || x$resample) {
    resample <- TRUE
  }

  out <- prepare_plot(d, resample, ...) +
      ggplot2::ylab("Density") +
      ggplot2::guides(
        linetype = ggplot2::guide_legend(
          title = "pareto-k"
        )
      ) +
    ggdist::stat_slab(
        ggplot2::aes(color = .data$alpha),
        fill = NA,
        linewidth = 0.5,
        ...,
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$component),
        cols = ggplot2::vars(.data$variable),
        labeller = ggplot2::labeller(
          component = c(
            likelihood = "Likelihood power-scaling",
            prior = "Prior power-scaling"
        )
        ),
        scales = "free",
        switch = "y"
      ) +
      ggplot2::xlab("")

  out <- out +
      ggplot2::ggtitle(
        label = "Power-scaling sensitivity",
        subtitle = "Posterior density estimates depending on amount of power-scaling (alpha).\nOverlapping lines indicate low sensitivity.\nWider gaps between lines indicate greater sensitivity.\nEstimates with Pareto-k values > 0.5 may be inaccurate."
    )
  
  return(out)
}

##' @rdname powerscale_plots
##' @export
powerscale_plot_ecdf <- function(x, variables, resample = FALSE, ...) {

  # input checks
  checkmate::assert_class(x, "powerscaled_sequence")
  checkmate::assert_character(variables)
  checkmate::assert_logical(resample)

  d <- prepare_plot_data(x, variables, resample, ...)

  if (resample || x$resample) {
    resample <- TRUE
  }
  p <- prepare_plot(d, resample, ...) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        title = "pareto-k"
      )
    ) +
    ggplot2::ylab("ECDF")

  if (resample || x$resampled) {
    p <- p +
      ggplot2::stat_ecdf(ggplot2::aes(color = .data$alpha))
  } else {
    p <- p +
      stat_ewcdf(ggplot2::aes(color = .data$alpha))
  }

  p <- p + ggplot2::facet_grid(
    rows = ggplot2::vars(.data$component),
    cols = ggplot2::vars(.data$variable),
    labeller = ggplot2::labeller(
      component = c(
        likelihood = "Likelihood power-scaling",
        prior = "Prior power-scaling"
      )
    ),
    scales = "free_x", switch = "y"
  ) +
    ggplot2::xlab("") +
  ggplot2::ggtitle(
    label = "Power-scaling sensitivity",
    subtitle = "Posterior ECDF depending on amount of power-scaling (alpha).\nOverlapping lines indicate low sensitivity.\nWider gaps between lines indicate greater sensitivity.\nEstimates with Pareto-k values > 0.5 may be inaccurate."
  )

  return(p)

}

##' @rdname powerscale_plots
##' @export
powerscale_plot_quantities <- function(x, variables,
                                       quantities = c("mean", "sd"),
                                       div_measure = "cjs_dist",
                                       resample = FALSE,
                                       measure_args = NULL,
                                       mcse = TRUE,
                                       quantity_args = NULL,
                                       ...) {

  summ <- summarise_draws(
    x,
    ... = quantities,
    .args = quantity_args,
    resample = resample,
    div_measures = div_measure,
    measure_args = measure_args
  )

  # TODO: better way to handle e.g. "a" -> "a[1]", "a[2]"
  variables <- posterior::variables(
    posterior::subset_draws(x[[1]], variable = variables)
  )

  if (mcse) {
    quants <- setdiff(
      colnames(summ[[1]]),
      c("variable", "alpha", "component",
        "pareto_k", "pareto_kf", "n_eff")
    )
    base_quantities <- summ[[1]][which(summ[[1]]$alpha == 1), ]
    base_quantities <- unique(base_quantities[c("variable", quants)])
    base_mcse <- posterior::summarise_draws(
      x$base_draws,
      posterior::default_mcse_measures()
    )

    base_mcse <- base_mcse[which(base_mcse$variable %in% variables), ]

    base_mcse <- as.data.frame(base_mcse)
    base_mcse <- stats::reshape(
      data = base_mcse,
      varying = c("mcse_mean", "mcse_median", "mcse_sd", "mcse_q5", "mcse_q95"),
      direction = "long",
      times = c("mean", "median", "sd", "q5", "q95"),
      v.names = "mcse",
      timevar = "quantity",
      idvar = "variable"
    )

    base_q <- as.data.frame(base_quantities)
    base_q <- stats::reshape(
      data = base_q,
      varying = quants,
      direction = "long",
      times = quants,
      v.names = "value",
      timevar = "quantity",
      idvar = "variable"
    )

    base_mcse <- merge(base_q, base_mcse)
    base_mcse$mcse_min <- base_mcse$value - 2 * base_mcse$mcse
    base_mcse$mcse_max <- base_mcse$value + 2 * base_mcse$mcse

  } else {
    base_mcse <- NULL
  }

  return(
    powerscale_summary_plot(
      summ, variables = variables, base_mcse = base_mcse, ...)
  )

}

powerscale_summary_plot <- function(x, variables, quantities = NULL,
                                    scale = FALSE, base_mcse = NULL,
                                    ...) {

  if (is.null(quantities)) {

    # get default quantities
    quantities <- setdiff(
      colnames(x[[1]]),
      c("variable", "alpha", "component",
        "pareto_k", "pareto_kf", "n_eff")
    )
  }

  # select only specified variables
  x[[1]] <- x[[1]][x[[1]][["variable"]] %in% variables, ]

  # reshape quantities for plotting
  summaries <- stats::reshape(
    data = x[[1]],
    varying = quantities,
    direction = "long",
    times = quantities,
    v.names = "value",
    timevar = "quantity"
  )

  summaries$quantity <- factor(summaries$quantity, levels = quantities)
  summaries$pareto_k_value <- ifelse(summaries$pareto_k > 0.5, "> 0.5",
                                     "< 0.5")

  summaries$pareto_k_value <- factor(
    summaries$pareto_k_value,
    levels = c("< 0.5", "> 0.5")
  )

  # subset for plotting points at ends of lines
  points <- summaries[summaries$alpha == min(summaries$alpha) |
                        summaries$alpha == max(summaries$alpha), ]

  p <- ggplot2::ggplot(
    data = summaries,
    mapping = ggplot2::aes(x = .data$alpha, y = .data$value)
  ) +
    ggplot2::geom_line(ggplot2::aes(
      color = .data$pareto_k_value, group = .data$component)) +
  ggplot2::facet_wrap(
    facets = ggplot2::vars(.data$variable, .data$quantity),
    scales = "free",
    ncol = length(quantities)
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = .data$alpha,
      y = .data$value,
      shape = .data$component,
      color = .data$pareto_k_value
    ),
    fill = "white",
    size = 3,
    data = points
  ) +
    ggplot2::scale_shape_manual(
      values = c("likelihood" = 22, "prior" = 15)) +
  ggplot2::scale_color_viridis_d(drop = FALSE) +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      title = "pareto-k",
      override.aes = list(shape = 15)
    )
  ) +
  ggplot2::ylab("") +
  ggplot2::scale_x_continuous(
    trans = "log2",
    limits = c(min(summaries$alpha) - 0.01, max(summaries$alpha) + 0.01),
    breaks = c(min(summaries$alpha), 1, max(summaries$alpha)),
    labels = round(c(min(summaries$alpha), 1, max(summaries$alpha)), digits = 3)
  ) +
  ggplot2::ggtitle(
    label = "Power-scaling sensitivity",
    subtitle = "Posterior quantities depending on amount of power-scaling (alpha).\nHorizontal lines indicate low sensitivity.\nSteeper lines indicate greater sensitivity.\nEstimates with Pareto-k values > 0.5 may be inaccurate."
  ) +
  ggplot2::theme(aspect.ratio = 1)

  if (!is.null(base_mcse)) {

    p <- p +
      ggplot2::scale_linetype_manual(values = "dashed", name = NULL) +
      ggplot2::geom_hline(
        ggplot2::aes(
          yintercept = .data$mcse_min,
          linetype = "+-2MCSE"
        ),
        data = base_mcse,
        color = "black"
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(
          yintercept = .data$mcse_max,
          linetype = "+-2MCSE"
        ),
        data = base_mcse,
        color = "black"
      )
  }

  return(p)
}
