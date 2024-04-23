##' Diagnostic plots for power-scaling sensitivity
##'
##' Various diagnostic plots for power-scaling sensitivity. See **Plot Descriptions** below for details.
##'
##' @name powerscale_plots
##'
##' @template plot_args
##' @template div_measure_arg
##' @template resample_arg
##' @param help_text Logical indicating whether title and subtitle
##'   with explanatory description should be included in the
##'   plot. Default is TRUE. Can be set via option
##'   "priorsense.show_help_text".
##' @template ggplot_return
##' @section Plot Descriptions: \describe{
##'   \item{`powerscale_plot_dens()`}{ Kernel density plot of
##'   power-scaled posterior draws with respect to power-scaling.  }
##'   \item{`powerscale_plot_ecdf()`}{ Empirical cumulative
##'   distribution function plot of power-scaled posterior draws with
##'   respect to power-scaling.  }
##'   \item{`powerscale_plot_quantities()`}{ Plot of posterior
##'   quantities with respect to power-scaling.} }
##'
##' @importFrom rlang .data
##' @importFrom lifecycle deprecated
NULL

prepare_plot_data <- function(x, variable, resample, ...) {

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
      prior_draws[[i]] <- posterior::merge_chains(prior_scaled[[i]])

      if (resample && !x$resampled) {
        prior_draws[[i]]  <- posterior::resample_draws(prior_draws[[i]])
      }

      prior_ps_details <- get_powerscaling_details(prior_scaled[[i]])

      prior_draws[[i]]$alpha <- prior_ps_details$alpha
      prior_draws[[i]]$component <- "prior"
      prior_draws[[i]]$pareto_k <- prior_ps_details$diagnostics$khat
      prior_draws[[i]]$pareto_k_threshold <- prior_ps_details$diagnostics$khat_threshold

    }

    base_draws_prior <- base_draws
    base_draws_prior$alpha <- 1
    base_draws_prior$component <- "prior"
    base_draws_prior$pareto_k <- -Inf
    base_draws_prior$pareto_k_threshold <- Inf
  }

  if (!(is.null(x$likelihood_scaled))) {

    likelihood_scaled <- x$likelihood_scaled$draws_sequence

    for (i in seq_along(likelihood_scaled)) {
      likelihood_draws[[i]] <- posterior::merge_chains(
        likelihood_scaled[[i]]
      )

      if (resample && !x$resampled) {
        likelihood_draws[[i]]  <- posterior::resample_draws(
          likelihood_draws[[i]]
        )
      }

      likelihood_ps_details <- get_powerscaling_details(likelihood_scaled[[i]])

      likelihood_draws[[i]]$alpha <- likelihood_ps_details$alpha
      likelihood_draws[[i]]$component <- "likelihood"
      likelihood_draws[[i]]$pareto_k <- likelihood_ps_details$diagnostics$khat
      likelihood_draws[[i]]$pareto_k_threshold <- likelihood_ps_details$diagnostics$khat_threshold


    }

    base_draws_lik <- base_draws
    base_draws_lik$alpha <- 1
    base_draws_lik$component <- "likelihood"
    base_draws_lik$pareto_k <- -Inf
    base_draws_lik$pareto_k_threshold <- Inf
  }

  d <- rbind(
    do.call("rbind", prior_draws),
    do.call("rbind", likelihood_draws),
    base_draws_lik,
    base_draws_prior
  )

  d$pareto_k_value <- ifelse(d$pareto_k > d$pareto_k_threshold, "High",
                             "OK")

  d$pareto_k_value <- factor(
    d$pareto_k_value,
    levels = c("OK", "High")
  )


  d$component <- factor(d$component, levels = c("prior", "likelihood"))

  # prepare for plotting
  d <- stats::reshape(
    data = as.data.frame(d),
    varying = variable,
    direction = "long",
    times = variable,
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
      values = c("solid", "dashed"),
      drop = TRUE
    ) +
    ggplot2::scale_color_gradientn(
      name = "Power-scaling alpha",
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
    )


  return(p)

}

##' @rdname powerscale_plots
##' @export
powerscale_plot_dens <- function(x, ...) {
  UseMethod("powerscale_plot_dens")
}

##' @export
powerscale_plot_dens.default <- function(x, variable = NULL, resample = FALSE, help_text = getOption("priorsense.plot_help_text", TRUE), variables = deprecated(), ...) {
  ps <- powerscale_sequence(x, ...)
  powerscale_plot_dens(ps, variable = variable, resample = resample, help_text = help_text)
}


##' @export
powerscale_plot_dens.powerscaled_sequence <- function(x, variable = NULL, resample = FALSE, help_text = getOption("priorsense.plot_help_text", TRUE),
                                                      variables = deprecated(),
                                                      ...) {

    if (lifecycle::is_present(variables)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_dens(variables)", "powerscale_plot_dens(variable)")
    variable <- variables
  }

  # input checks
  checkmate::assert_character(variable, null.ok = TRUE)
  checkmate::assert_logical(resample, len = 1)
  checkmate::assert_logical(help_text, len = 1)

  if (is.null(variable)) {
    variable <- posterior::variables(x$base_draws)
  } else {
    variable <- posterior::variables(
      posterior::subset_draws(x$base_draws, variable = variable)
    )
  }

  d <- prepare_plot_data(x, variable = variable, resample = resample, ...)

  if (resample || x$resample) {
    resample <- TRUE
  }

  n_components <- length(unique(d$component))

  out <- prepare_plot(d, resample, ...) +
      ggplot2::ylab("Density") +
      ggplot2::guides(
        linetype = ggplot2::guide_legend(
          title = "Pareto k"
        )
      ) +
    ggdist::stat_slab(
      ggplot2::aes(color = .data$alpha),
      fill = NA,
      linewidth = 0.5,
      trim = FALSE,
      normalize = "xy",
      ...,
      ) +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$variable),
      cols = ggplot2::vars(.data$component),
      labeller = ggplot2::labeller(
        component = c(
          likelihood = "Likelihood power-scaling",
          prior = "Prior power-scaling"
        )
      ),
      independent = "all",
      scales = "free",
      switch = "y"
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  if (help_text) {
    out <- out +
      ggplot2::ggtitle(
        label = "Power-scaling sensitivity",
        subtitle = "Posterior density estimates depending on amount of power-scaling (alpha).\nOverlapping lines indicate low sensitivity.\nWider gaps between lines indicate greater sensitivity.\nEstimates with high Pareto-k values may be inaccurate."
      )
  }

  if (getOption("priorsense.use_plot_theme", TRUE)) {
    out <- out +
      theme_priorsense()
  }

  return(out)
}

##' @rdname powerscale_plots
##' @export
powerscale_plot_ecdf <- function(x, ...) {
  UseMethod("powerscale_plot_ecdf")
}


##' @export
powerscale_plot_ecdf.default <- function(x, variable = NULL, resample = FALSE, help_text = getOption("priorsense.plot_help_text", TRUE), variables = lifecycle::deprecated(), ...) {
  ps <- powerscale_sequence(x, ...)
  powerscale_plot_ecdf(
    ps,
    variable = variable,
    resample = resample,
    help_text = help_text
  )
}

##' @rdname powerscale_plots
##' @export
powerscale_plot_ecdf.powerscaled_sequence <- function(x, variable = NULL, resample = FALSE, help_text = getOption("priorsense.plot_help_text", TRUE), variables = lifecycle::deprecated(), ...) {

  if (lifecycle::is_present(variables)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_ecdf(variables)", "powerscale_plot_ecdf(variable)")
    variable <- variables
  }

  # input checks
  checkmate::assert_character(variable, null.ok = TRUE)
  checkmate::assert_logical(resample, len = 1)
  checkmate::assert_logical(help_text, len = 1)

  if (is.null(variable)) {
    variable <- posterior::variables(x$base_draws)
  } else {
    variable <- posterior::variables(
      posterior::subset_draws(x$base_draws, variable = variable)
    )
  }

  d <- prepare_plot_data(x, variable = variable, resample = resample, ...)

  n_components <- length(unique(d$component))

  if (resample || x$resample) {
    resample <- TRUE
  }
  p <- prepare_plot(d, resample, ...) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        title = "Pareto k"
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

  p <- p +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$variable),
      cols = ggplot2::vars(.data$component),
    labeller = ggplot2::labeller(
      component = c(
        likelihood = "Likelihood power-scaling",
        prior = "Prior power-scaling"
      )
    ),
    scales = "free",
    independent = "all",
    switch = "y"
  ) +
    ggplot2::xlab("")

  if (help_text) {
    p <- p +
  ggplot2::ggtitle(
    label = "Power-scaling sensitivity",
    subtitle = "Posterior ECDF depending on amount of power-scaling (alpha).\nOverlapping lines indicate low sensitivity.\nWider gaps between lines indicate greater sensitivity.\nEstimates with high Pareto-k values may be inaccurate."
  )
  }

  if (getOption("priorsense.use_plot_theme", TRUE)) {
    p <- p +
      theme_priorsense()
  }

  return(p)

}


##' @rdname powerscale_plots
##' @export
powerscale_plot_quantities <- function(x, ...) {
  lifecycle::deprecate_warn("0.9", "powerscale_plot_quantities()", "powerscale_plot_quantity()")
  UseMethod("powerscale_plot_quantity")
}



##' @rdname powerscale_plots
##' @export
powerscale_plot_quantity <- function(x, ...) {
  UseMethod("powerscale_plot_quantity")
}

##' @export
powerscale_plot_quantity.default <- function(x, variable = NULL,
                                       quantity = c("mean", "sd"),
                                       div_measure = "cjs_dist",
                                       resample = FALSE,
                                       measure_args = NULL,
                                       mcse = TRUE,
                                       quantity_args = NULL,
                                       help_text = getOption("priorsense.plot_help_text", TRUE),
                                       variables = lifecycle::deprecated(),
                                       quantities = lifecycle::deprecated(),

                                       ...) {
  ps <- powerscale_sequence(x, ...)

  powerscale_plot_quantity(
    ps,
    variable = variable,
    quantity = quantity,
    div_measure = div_measure,
    resample = resample,
    measure_args = measure_args,
    mcse = mcse,
    quantity_args = quantity_args,
    help_text = help_text,
    quantities = quantities,
    variables = variables
  )
}

##' @rdname powerscale_plots
##' @export
powerscale_plot_quantity.powerscaled_sequence <- function(x, variable = NULL,
                                       quantity = c("mean", "sd"),
                                       div_measure = "cjs_dist",
                                       resample = FALSE,
                                       measure_args = NULL,
                                       mcse = TRUE,
                                       quantity_args = NULL,
                                       help_text = getOption("priorsense.plot_help_text", TRUE),
                                       quantities = deprecated(),
                                       variables = deprecated(),
                                       ...) {

  if (lifecycle::is_present(variables)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_quantity(variables)", "powerscale_plot_quantity(variable)")
    variable <- variables
  }

  if (lifecycle::is_present(quantities)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_quantity(quantities)", "powerscale_plot_quantity(quantity)")
    quantity <- quantities
  }


  checkmate::assertCharacter(variable, null.ok = TRUE)
  checkmate::assertCharacter(quantity)
  checkmate::assertCharacter(div_measure)
  checkmate::assertLogical(resample, len = 1)
  checkmate::assertList(measure_args, null.ok = TRUE)
  checkmate::assertLogical(mcse, len = 1)
  checkmate::assertList(quantity_args, null.ok = TRUE)
  checkmate::assertLogical(help_text, len = 1)

  summ <- summarise_draws(
    x,
    ... = quantity,
    .args = quantity_args,
    resample = resample,
    div_measures = div_measure,
    measure_args = measure_args
  )

  if (is.null(variable)) {
    variable <- posterior::variables(x$base_draws)
  } else {
    # TODO: better way to handle e.g. "a" -> "a[1]", "a[2]"
    variable <- posterior::variables(
      posterior::subset_draws(x$base_draws, variable = variable)
    )
  }

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

    base_mcse <- base_mcse[which(base_mcse$variable %in% variable), ]

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
      summ, variable = variable, base_mcse = base_mcse, help_text = help_text, ...)
  )

}

powerscale_summary_plot <- function(x,
                                    variable,
                                    base_mcse = NULL,
                                    help_text,
                                    ...) {


  # get default quantities
  quantities <- setdiff(
    colnames(x[[1]]),
    c("variable", "alpha", "component",
      "pareto_k", "pareto_kf", "n_eff", "pareto_k_threshold")
  )

  # select only specified variables
    x[[1]] <- x[[1]][x[[1]][["variable"]] %in% variable, ]

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
  summaries$pareto_k_value <- ifelse(summaries$pareto_k > summaries$pareto_k_threshold, "High",
                                     "OK")

  summaries$pareto_k_value <- factor(
    summaries$pareto_k_value,
    levels = c("OK", "High")
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
    ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$variable),
      cols = ggplot2::vars(.data$quantity),
      scales = "free",
      switch = "y",
    independent = "all"
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
  ggplot2::scale_color_viridis_d() +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      title = "Pareto k",
      override.aes = list(shape = 15)
    )
  ) +
  ggplot2::ylab("") +
  ggplot2::scale_x_continuous(
    trans = "log2",
    limits = c(min(summaries$alpha) - 0.01, max(summaries$alpha) + 0.01),
    breaks = c(min(summaries$alpha), 1, max(summaries$alpha)),
    labels = round(c(min(summaries$alpha), 1, max(summaries$alpha)), digits = 3)
  )

  if (help_text) {
    p <- p +
  ggplot2::ggtitle(
    label = "Power-scaling sensitivity",
    subtitle = "Posterior quantities depending on amount of power-scaling (alpha).\nHorizontal lines indicate low sensitivity.\nSteeper lines indicate greater sensitivity.\nEstimates with high Pareto-k values may be inaccurate."
  )
  }

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

  if (getOption("priorsense.use_plot_theme", TRUE)) {
    p <- p +
      theme_priorsense()
  }

  return(p)
}
